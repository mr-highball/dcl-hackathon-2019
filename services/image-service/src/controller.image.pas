unit controller.image;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb,
  controller.base, controller.dto,
  controller.image.dto;

type

  { TImageController }

  TImageController = class(TBaseController)
  private
    procedure InitImageTable;

    //action for fetching image commands
    procedure FetchAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);
  protected
    procedure DoInitialize(); override;
    function DoInitializeActions(): TActions; override;
  public
    (*
      fetches the image response provided an image token. the image
      requested must be completed, otherwise this request will fail
    *)
    function Fetch(Const AImageToken : String; Out Response : TImageResponse;
      Out Error : String) : Boolean;

    function UpdateImage(constref AToken : String;
      constref AImage : TImageResponse; Out Error : String) : Boolean;
  end;

var
  ImageController: TImageController;

implementation
uses
  controller.registration,
  controller.status,
  fpjson,
  jsonparser;
{$R *.lfm}

{ TImageController }

procedure TImageController.InitImageTable;
var
  LError: String;
begin
  //initialize our table if it doesn't exist
  if not ExecuteSQL(
    'CREATE TABLE IF NOT EXISTS image(' +
    ' id Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
    ' registration_id integer,' +
    ' commands text NOT NULL,' +
    ' create_date datetime NOT NULL DEFAULT (datetime(''now'')),' +
    ' FOREIGN KEY(registration_id) REFERENCES registration(id));',
    LError
  ) then
    LogError(LError);
end;

procedure TImageController.FetchAction(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  LRequest : TImageRequest;
  LResponse : TImageResponse;
  LError: String;
begin
  try
    AResponse.ContentType := 'application/json';
    AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
    Handled := True;
    LogRequester('FetchAction::', ARequest);

    //try and parse the request
    LRequest.FromJSON(ARequest.Content);

    //make a call to auth service to validate the auth token
    //todo...

    //try to fetch the status
    if not Fetch(LRequest.Image.Token, LResponse, LError) then
    begin
      AResponse.Content := GetErrorJSON('unable to fetch image');
      LogError(LError);
    end
    else
      AResponse.Content := LResponse.ToJSON;
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON('unable to fetch image - server error');
  end
  end;
end;

procedure TImageController.DoInitialize();
begin
  inherited DoInitialize();
  InitImageTable;
end;

function TImageController.DoInitializeActions(): TActions;
var
  I : Integer;
  LAction : TAction;
begin
  Result := inherited DoInitializeActions();

  //set the starting index to the last index of result
  I := High(Result);

  //if base added some default actions, then increment the index
  if I >= 0 then
    Inc(I);

  //initialize the image actions
  SetLength(Result, Succ(Length(Result)));

  LAction.Name := 'fetch';
  LAction.Action := FetchAction;
  Result[I] := LAction;
end;

function TImageController.Fetch(const AImageToken: String; out
  Response: TImageResponse; out Error: String): Boolean;
var
  LStatusController : TStatusController;
  LStatus: TImageStatus;
  LData: TDatasetResponse;
  LJSON: TJSONData;
begin
  Result := False;

  LStatusController := TStatusController.Create(nil);
  try
    try
      //check status of requested image to make sure it's completed
      if not LStatusController.Fetch(AImageToken, LStatus, Error) then
      begin
        Error := 'Fetch::' + Error;
        Exit;
      end;

      //if we don't have a completed status then bail
      if LStatus <> isCompleted then
        raise Exception.Create('status for token [' + AImageToken + '] is not ' + ImageStatusToString(isCompleted));

      //try to fetch the image response
      if not GetSQLResultsJSON(
        'SELECT commands FROM image i' +
        ' JOIN registration r ON i.registration_id = r.id' +
        ' WHERE r.token = ' + QuotedStr(AImageToken) +
        ' LIMIT 1;',
        LData,
        Error
      ) then
        raise Exception.Create(Error);

      if LData.Count < 1 then
        raise Exception.Create('no commands for token [' + AImageToken + ']');

      //if we have the commands record, then deserialize to a response
      //by first casting to the sql results objects and passing down the inner
      //commands object (envelope of a json object)
      LJSON := GetJSON(LData[0]);
      try
        if LJSON.JSONType <> jtObject then
          raise Exception.Create('unable to cast database commands to object');

        //object is quoted as a standard sql string when saved to db, so dequote
        Response.FromJSON(TJSONObject(LJSON).Get('commands'));
      finally
        LJSON.Free;
      end;

      //success
      Result := True;
    except on E : Exception do
      Error := 'Fetch::' + E.Message;
    end;
  finally
    LStatusController.Free;
  end;
end;

function TImageController.UpdateImage(constref AToken: String; constref
  AImage: TImageResponse; out Error: String): Boolean;
var
  LRegistration: TRegistrationController;
  LID: Integer;
begin
  Result := False;

  LRegistration := TRegistrationController.Create(nil);
  try
    //get the registration id
    if not LRegistration.LookupImageID(AToken, LID) then
    begin
      Error := 'Fetch::[' + AToken + '] not found';
      Exit;
    end;

    //upsert the image commands by deleting first then inserting
    if not ExecuteSQL(
      'DELETE FROM image WHERE registration_id = ' + IntToStr(LID) + ';',
      Error
    ) then
      Exit;

    if not ExecuteSQL(
      'INSERT INTO image(registration_id, commands)' +
      ' SELECT ' + IntToStr(LID) + ',' + QuotedStr(AImage.ToJSON) + ';',
      Error
    ) then
      Exit;

    Result := True;
  finally
    LRegistration.Free;
  end;
end;

initialization
  RegisterHTTPModule(GetControllerRoute('image'), TImageController);
end.

