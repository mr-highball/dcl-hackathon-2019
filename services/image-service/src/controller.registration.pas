unit controller.registration;

{$mode delphi}{$H+}
{$ModeSwitch nestedprocvars}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb, FPImage,
  controller.base,
  controller.registration.dto;

type

  TRegistrationValidateCallback = function(Const AURL : String) : Boolean;

var
  //to add validations rules for url's, add them to this array
  VALIDATE_CALLBACKS : TArray<TRegistrationValidateCallback>;
  MAX_IMAGE_WIDTH : Integer;
  MAX_IMAGE_HEIGHT : Integer;

type

  { TRegistrationController }
  (*
    controller that handles the starting step of processing
    image requests. endpoints will allow image client to validate
    whether or not a URL is acceptable, begin process, as well as cancelling a
    a request
  *)
  TRegistrationController = class(TBaseController)
  private
    //action for handling validating urls
    procedure ValidateAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);

    //action for handling registering an image to be processed
    procedure RegisterAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);

    //action to attempt to cancel an in progress image request
    procedure CancelAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);

    procedure InitRegistrationTable;
  strict protected
    procedure DoInitialize(); override;
    function DoInitializeActions(): TActions; override;

    //async processing for a url
    class procedure StartProcessing(Constref ARequest : TRegisterURLRequest;
      Constref AResponse : TRegisterURLResponse); static;
  public
    (*
      uses validation rules to determine whether or not a particular
      url is valid for processing. this could be as simple as domain
      checking, or could do text analysis on the resource, etc...

      note:
        by default all images that *can* be processed will be accepted.
        it's up to individual land owners who use this service to
        implement the custom validation rules
    *)
    function ValidateURL(Constref AValidation : TValidateURLRequest) : Boolean;

    //begins the processing step as long as the URL is valid
    function RegisterImage(Constref ARequest : TRegisterURLRequest;
      out Response : TRegisterURLResponse) : Boolean;

    //will cancel the request as long as the process is in-progress
    function CancelRequest() : Boolean;
  end;

var
  RegistrationController: TRegistrationController;

implementation
uses
  Graphics,
  BGRABitmap,
  BGRABitmapTypes,
  bgradithering,
  BGRAPalette,
  BGRAColorQuantization,
  fphttpclient,
  controller.status,
  controller.dto,
  ezthreads,
  ezthreads.collection,
  opensslsockets;
var
  Collection : IEZCollection;
  Palette : TBGRAApproxPalette;

{$R *.lfm}

procedure InitializePalette;
begin
  //for now we initialize this in here rather than exposing the
  //palette to a config, since we don't want to overload dcl
  Palette := TBGRAApproxPalette.Create(
    [
      TBGRAPixel.New(255, 0, 0), //red
      TBGRAPixel.New(0, 255, 0), //green
      TBGRAPixel.New(0, 0, 255), //blue
      TBGRAPixel.New(0, 0, 0), //black
      TBGRAPixel.New(128, 128, 128), //grey
      TBGRAPixel.New(255, 255, 255), //white
      TBGRAPixel.New(255, 255, 0), //yellow
      TBGRAPixel.New(255, 128, 0), //orange
      TBGRAPixel.New(128, 0, 255) //purple
    ]
  );
end;

function DitherImageStream(Const AImage : TStream; Const AURL : String;
  Const ABitmap : TBGRABitmap; Out Error : String) : Boolean;
var
  LTask: TDitheringTask;
  LReader: TFPCustomImageReader;
  LBitmap : TBGRACustomBitmap;
  LBuffer : TMemoryStream;
begin
  Result := False;

  try
    AImage.Position := 0;

    //initialize a bitmap
    if (AURL.Contains('.jpg') or AURL.Contains('.jpeg')) then
    begin
      //jpegs can sometimes be strange... use finer control here
      LReader := DefaultBGRAImageReader[ifJpeg].Create;
      LBitmap := TBGRABitmap.Create;
      LBitmap.LoadFromStream(AImage, LReader, [loBmpAutoOpaque, loJpegQuick]);
      LReader.Free;
    end
    else
      LBitmap := TBGRABitmap.Create(AImage);

    LBuffer := TMemoryStream.Create;
    try
      //resize the image to the configured dimensions
      with LBitmap.Resample(MAX_IMAGE_WIDTH, MAX_IMAGE_HEIGHT) do
      begin
        LBuffer.Clear;
        LBuffer.Position := 0;
        SaveToStreamAs(LBuffer, ifPng);
        Free;
      end;

      //initialize the dither task
      LTask := CreateDitheringTask(
        daNearestNeighbor, //algorithm for dithering
        LBitmap, //source image
        Palette, //use the reduced color palette
        True, //ignore alpha for now
        ABitmap.GetImageBounds()
      );

      try
        //configure to dither to the current bitmap
        LTask.Destination := ABitmap;

        //begin dithering
        with TBGRABitmap(LTask.Execute) do
        begin
          LBuffer.Clear;
          LBuffer.Position := 0;
          SaveToStreamAs(LBuffer, ifPng);
          Free;
        end;

        //load the dithered image to result
        ABitmap.Assign(LBitmap);

        //success
        Result := True;
      finally
        LBitmap.Free;
        LTask.Free;
      end;
    finally
      LBuffer.Free;
    end;

  except on E : Exception do
    Error := E.Message;
  end;
end;

function ConstructDCLImageJSON(Const AImage : TBGRABitmap;
  Out JSON, Error : String) : Boolean;
begin
  Result := False;

  try
    //todo
  except on E : Exception do
    Error := E.Message;
  end;
end;

{ TRegistrationController }

procedure TRegistrationController.ValidateAction(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
  LValidate : TValidateURLRequest;
  LResult : TBoolResponse;
begin
  try
    LResult.Success := False;

    AResponse.ContentType := 'application/json';
    Handled := True;
    LogRequester('ValidateAction::', ARequest);

    //try and parse the token
    LValidate.FromJSON(ARequest.Content);

    //try to validate
    if not ValidateURL(LValidate) then
      AResponse.Content := LResult.ToJSON
    else
    begin
      LResult.Success := True;
      AResponse.Content := LResult.ToJSON;
    end;
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON('unable to validate - server error');
  end
  end;
end;

procedure TRegistrationController.RegisterAction(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
  LRequest : TRegisterURLRequest;
  LResult : TRegisterURLResponse;
  LGuid, LError: String;
begin
  try
    AResponse.ContentType := 'application/json';
    Handled := True;
    LogRequester('ValidateAction::', ARequest);

    //try and parse the request
    LRequest.FromJSON(ARequest.Content);

    //perform registrations
    if not RegisterImage(LRequest, LResult) then
    begin
      AResponse.Content := GetErrorJSON('unable to register image');
      Exit;
    end;

    //success
    AResponse.Content := LResult.ToJSON;
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON('unable to register image - server error');
  end
  end;
end;

procedure TRegistrationController.CancelAction(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin

end;

procedure TRegistrationController.InitRegistrationTable;
var
  LError: String;
begin
  //initialize our table if it doesn't exist
  if not ExecuteSQL(
    'CREATE TABLE IF NOT EXISTS registration(' +
    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
    ' "token" varchar(128) NOT NULL,' +
    ' "user_key" varchar(50) NOT NULL,' +
    ' "parcel_identity" varchar(25) NOT NULL,' +
    ' "url" varchar(2000) NOT NULL,' +
    ' "create_date" datetime NOT NULL DEFAULT (datetime(''now'')));',
    LError
  ) then
    LogError(LError);
end;

procedure TRegistrationController.DoInitialize();
begin
  inherited DoInitialize();
  InitRegistrationTable;
end;

function TRegistrationController.DoInitializeActions(): TActions;
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

  //initialize the registration route actions
  SetLength(Result, Length(Result) + 3);

  LAction.Name := 'validate';
  LAction.Action := ValidateAction;
  Result[I] := LAction;

  LAction.Name := 'register';
  LAction.Action := RegisterAction;
  Result[I + 1] := LAction;

  LAction.Name := 'cancel';
  LAction.Action := CancelAction;
  Result[I + 2] := LAction;
end;

function TRegistrationController.ValidateURL(
  constref AValidation: TValidateURLRequest): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 0 to High(VALIDATE_CALLBACKS) do
    if not VALIDATE_CALLBACKS[I](AValidation.URL) then
      Exit(False);
end;

function TRegistrationController.RegisterImage(constref ARequest : TRegisterURLRequest;
  out Response : TRegisterURLResponse): Boolean;
var
  LGuid,
  LError: String;
begin
  Result := False;

  try
    //make a call to make sure the requested token matches the details
    //...

    //try to validate the url
    if not ValidateURL(ARequest.Validation) then
    begin
      LogError('RegisterImage::validation failed for ' + ARequest.ToJSON);
      Exit;
    end;

    //save the job to db with the generated token
    LGuid := TGuid.NewGuid.ToString();

    if not ExecuteSQL(
      'INSERT INTO registration(token, user_key, parcel_identity, url)' +
      ' SELECT ' + QuotedStr(LGuid) + ',' +
      ' ' + QuotedStr(ARequest.Authentication.UserKey) + ',' +
      ' ' + QuotedStr(ARequest.Authentication.ParcelID) + ',' +
      ' ' + QuotedStr(ARequest.Validation.URL) + ';',
      LError
    ) then
    begin
      LogError('RegisterImage::' + LError);
      Exit;
    end;

    Response.Token := LGuid;

    //kick off the background process to perform the actual image deconstruction
    StartProcessing(ARequest, Response);

    Result := True;
  except on E : Exception do
    LogError('RegisterImage::' + E.Message);
  end;
end;

function TRegistrationController.CancelRequest(): Boolean;
begin
  //not implemented, but need to look in collection and
  //then add some kind of flag to the thread (if found) to stop processing
  Result := False;
end;

class procedure TRegistrationController.StartProcessing(
  constref ARequest: TRegisterURLRequest; constref AResponse: TRegisterURLResponse);
var
  LThread : IEZThread;

  procedure Start(Const AThread : IEZThread);
  var
    LStatus : TStatusController;
    LData : TMemoryStream;
    LClient : TFPHTTPClient;
    LError,
    LJSON : String;
    LBitmap : TBGRABitmap;
  begin
    LStatus := TStatusController.Create(nil);
    LClient := TFPHTTPClient.Create(nil);
    LData := TMemoryStream.Create;
    LBitmap := TBGRABitmap.Create;
    try
      try
        //update status to in-work
        //todo... (use 'token')

        //attempt to download the image
        LClient.Get(AThread['url'], LData);

        //we need to dither the image to a reduced color space so our
        //dcl client can draw it in some reasonable amount of time
        //(currently uses color components to draw, this may change later...)
        if not DitherImageStream(LData, AThread['url'], LBitmap, LError) then
          raise Exception.Create(LError);

        //debug... remove me
        LBitmap.SaveToFile('debugRemoveMeLater.bmp');

        //after dithering has been completed, we can construct to a
        //dcl readable format
        if not ConstructDCLImageJSON(LBitmap, LJSON, LError) then
          raise Exception.Create(LError);
      finally
        LBitmap.Free;
        LData.Free;
        LClient.Free;
        LStatus.Free;
      end;
    except on E : Exception do
    begin
      AThread.AddArg('error', E.Message);
      raise E; //re-raise to trigger failure method
    end;
    end;
  end;

  procedure Finish(Const AThread : IEZThread);
  var
    LStatus : TStatusController;
  begin
    LStatus := TStatusController.Create(nil);

    //mark the status as complete
    //...

    //remove from collection
    Collection.Remove(AThread);
  end;

  procedure Failure(Const AThread : IEZThread);
  var
    LStatus: TStatusController;
    LRegister : TRegistrationController;
  begin
    LStatus := TStatusController.Create(nil);
    LRegister := TRegistrationController.Create(nil);

    //mark the status as failed
    //...

    //log the error with the controller since we wont have scope
    //to this thread's calling controller
    LRegister.LogError(
      IfThen<String>(
        AThread.Exists['error'],
        'StartProcessing::' + AThread['error'],
        'StartProcessing::no error message for thread'
      )
    );

    //remove from collection
    Collection.Remove(AThread);
  end;

begin
  //setup and start a processing thread
  LThread := TEZThreadImpl.Create;
  LThread
    .Setup(
      Start, //handles initialize of status and processing
      Finish, //updates the status to finished and produces saves the result
      Failure //updates the status to failed
    )
    .AddArg('url', ARequest.Validation.URL)
    .AddArg('token', AResponse.Token)
    .Settings
      .Await
        (*
          update the group id to be the thread id since we won't have
          access to the thread from the controller (gets freed after the request)
          and we may need to index back into the collection. alternatively,
          could use a different collection type (map<token,thread> for example)
        *)
        .UpdateGroupID(LThread.Settings.Await.ThreadID);

  //before starting the thread, add it to the monitor collection
  Collection.Add(LThread);

  //good to start now
  LThread.Start;
end;

initialization
  MAX_IMAGE_HEIGHT := 64;
  MAX_IMAGE_WIDTH := 64;
  Collection := TEZCollectionImpl.Create;
  InitializePalette;
  RegisterHTTPModule(GetControllerRoute('registration'), TRegistrationController);
finalization
  Collection := nil;
  Palette.Free;
end.

