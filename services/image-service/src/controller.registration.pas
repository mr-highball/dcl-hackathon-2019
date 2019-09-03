unit controller.registration;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb,
  controller.base;

type

  TRegistrationValidateCallback = function(Const AURL : String) : Boolean;

var
  //to add validations rules for url's, add them to this array
  VALIDATE_CALLBACKS : TArray<TRegistrationValidateCallback>;

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

    (*
      uses validation rules to determine whether or not a particular
      url is valid for processing. this could be as simple as domain
      checking, or could do text analysis on the resource, etc...

      note:
        by default all images that *can* be processed will be accepted.
        it's up to individual land owners who use this service to
        implement the custom validation rules
    *)
    function ValidateURL(Const AURL : String) : Boolean;

    //begins the processing step as long as the URL is valid
    function RegisterImage(Const AURL : String) : Boolean;

    //will cancel the request as long as the process is in-progress
    function CancelRequest() : Boolean;
  public

  end;

var
  RegistrationController: TRegistrationController;

implementation
uses
  controller.dto,
  controller.registration.dto;
{$R *.lfm}

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
    if not ValidateURL(LValidate.AuthValidation.Token) then
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
begin

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

function TRegistrationController.ValidateURL(const AURL: String): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := 0 to Length(VALIDATE_CALLBACKS) do
    if not VALIDATE_CALLBACKS[I](AURL) then
      Exit(False);
end;

function TRegistrationController.RegisterImage(const AURL: String): Boolean;
begin

end;

function TRegistrationController.CancelRequest(): Boolean;
begin

end;

initialization
  RegisterHTTPModule(GetControllerRoute('registration'), TRegistrationController);
end.

