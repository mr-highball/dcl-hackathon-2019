unit controller.status;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb,
  controller.base, controller.dto;

type

  (*
    enum representing an image's lifecycle
  *)
  TImageStatus = (
    isInProgress,
    isCompleted,
    isFailed
  );

  { TStatusController }

  TStatusController = class(TBaseController)
  strict private
    procedure InitStatusTable;

    //action for fetching a status
    procedure FetchAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);
  strict protected
    procedure DoInitialize(); override;
    function DoInitializeActions(): TActions; override;
  public
    (*
      inserts a new status record for the provided image token,
      and can be used if there are no previous status records
    *)
    function UpdateStatus(Const AImageToken : String;
      Const ANewStatus : TImageStatus; Out Error : String) : Boolean;

    (*
      provided an image token, will lookup the status of a given
      image, and return it to the caller. a failure indicates the
      token is invalid or an error occurred
    *)
    function Fetch(Const AImageToken : String; Out Status : TImageStatus;
      Out Error : String) : Boolean;
  end;

(*
  helper method for converting a status enum to a string representation
*)
function ImageStatusToString(Const AStatus : TImageStatus) : String;

(*
  helper method for converting a status string back to an enum value
*)
function StringToImageStatus(Const AStatus : String) : TImageStatus;

var
  StatusController: TStatusController;

const

  IMAGE_STATUS_LOOKUP : array[0 .. 2] of String = (
    'In-Progress',
    'Completed',
    'Failed'
  );

implementation
uses
  controller.registration,
  controller.status.dto,
  fpjson,
  jsonparser;

function ImageStatusToString(const AStatus: TImageStatus): String;
begin
  Result := IMAGE_STATUS_LOOKUP[Ord(AStatus)];
end;

function StringToImageStatus(const AStatus: String): TImageStatus;
var
  I: Integer;
begin
  Result := isInProgress;

  for I := 0 to High(IMAGE_STATUS_LOOKUP) do
    if IMAGE_STATUS_LOOKUP[I].ToLower = AStatus.ToLower then
    begin
      Result := TImageStatus(I);
      Exit;
    end;

  //if we didn't find it above it means we have an invalid string
  Raise Exception.Create('ImageStatusToString::[' + AStatus + '] is not a valid status');
end;

{$R *.lfm}

{ TStatusController }

procedure TStatusController.InitStatusTable;
var
  LError: String;
begin
  //initialize our table if it doesn't exist
  if not ExecuteSQL(
    'CREATE TABLE IF NOT EXISTS status(' +
    ' id Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
    ' registration_id integer,' +
    ' status varchar(128) NOT NULL,' +
    ' create_date datetime NOT NULL DEFAULT (datetime(''now'')),' +
    ' FOREIGN KEY(registration_id) REFERENCES registration(id));',
    LError
  ) then
    LogError(LError);
end;

procedure TStatusController.FetchAction(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  LRequest : TStatusRequest;
  LResponse : TStatusResponse;
  LStatus : TImageStatus;
  LError: String;
begin
  try
    LResponse.Status := isInProgress;

    AResponse.ContentType := 'application/json';
    Handled := True;
    LogRequester('FetchAction::', ARequest);

    //try and parse the token
    LRequest.FromJSON(ARequest.Content);

    //try to fetch the status
    if not Fetch(LRequest.Token, LStatus, LError) then
    begin
      AResponse.Content := GetErrorJSON('unable to fetch status');
      LogError(LError);
    end
    else
    begin
      LResponse.Status := LStatus;
      AResponse.Content := LResponse.ToJSON;
    end;
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON('unable to fetch status - server error');
  end
  end;
end;

procedure TStatusController.DoInitialize();
begin
  inherited DoInitialize();
  InitStatusTable;
end;

function TStatusController.DoInitializeActions(): TActions;
var
  I : Integer;
  LAction : TAction;
begin
  Result:=inherited DoInitializeActions();

  //set the starting index to the last index of result
  I := High(Result);

  //if base added some default actions, then increment the index
  if I >= 0 then
    Inc(I);

  //initialize the status route actions
  SetLength(Result, Succ(Length(Result)));

  //only have a single fetching action for web callers
  LAction.Name := 'fetch';
  LAction.Action := FetchAction;
  Result[I] := LAction;
end;

function TStatusController.UpdateStatus(const AImageToken: String;
  const ANewStatus: TImageStatus; out Error: String): Boolean;
var
  LRegistration : TRegistrationController;
  LID: Integer;
  LData: TDatasetResponse;
begin
  Result := False;

  LRegistration := TRegistrationController.Create(nil);
  try
    try
      LogInfo('UpdateStatus::requesting new status [' + ImageStatusToString(ANewStatus) + '] for token [' + AImageToken + ']');

      //check if we can find the requested image token and get the internal
      //id so we can properly update
      if not LRegistration.LookupImageID(AImageToken, LID) then
      begin
        Error := 'UpdateStatus::[' + AImageToken + '] not found';
        Exit;
      end;

      //attempt to update the status
      if not GetSQLResultsJSON(
        'INSERT INTO status (registration_id, status) SELECT ' + IntToStr(LID) + ',' +
        ' ' + QuotedStr(ImageStatusToString(ANewStatus)) + ';' +
        'SELECT last_insert_rowid() AS id;',
        LData,
        Error
      ) then
        Exit;

      //check the count returned in data to see if we were successful
      if LData.Count < 1 then
      begin
        Error := 'UpdateStatus::unable to update status for image [' + AImageToken + '];';
        Exit;
      end;

      Result := True;
    except on E : Exception do
      Error := 'UpdateStatus::' + E.Message;
    end;
  finally
    LRegistration.Free;
  end;
end;

function TStatusController.Fetch(const AImageToken: String; out
  Status: TImageStatus; out Error: String): Boolean;
var
  LRegistration : TRegistrationController;
  LID: Integer;
  LData: TDatasetResponse;
  LStatus: TJSONData;
begin
  Result := False;

  LRegistration := TRegistrationController.Create(nil);
  try
    if not LRegistration.LookupImageID(AImageToken, LID) then
    begin
      Error := 'Fetch::[' + AImageToken + '] not found';
      Exit;
    end;

    //attempt to fetch the status
    if not GetSQLResultsJSON(
      'SELECT status FROM status WHERE registration_id = ' + IntToStr(LID) + ' LIMT 1;',
      LData,
      Error
    ) then
      Exit;

    if LData.Count < 1 then
    begin
      Error := 'Fetch::unable to fetch status for image [' + AImageToken + '];';
      Exit;
    end;

    LStatus := GetJSON(LData[0]);
    try
      //try to parse the response
      if not Assigned(LStatus) or (LStatus.JSONType <> jtObject) then
      begin
        Error := 'Fetch::malformed json';
        Exit;
      end;

      //convert to status
      Status := StringToImageStatus(TJSONObject(LStatus).Get('status'));

      //success
      Result := True;
    finally
      if Assigned(LStatus) then
        LStatus.Free;
    end;
  finally
    LRegistration.Free;
  end;
end;

initialization
  RegisterHTTPModule(GetControllerRoute('status'), TStatusController);
end.

