unit controller.auth;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  controller.base,
  controller.auth.dto;

type

  { TAuthController }
  (*
    auth controller handles generating / validating of tokens,
    as well as providing a means to "reverse lookup" for initial request
    user and parcel info. other services requiring validation for actions
    can easily integrate that functionality by using the auth.dto's and
    required methods
  *)
  TAuthController = class(TBaseController)
  strict private
    procedure InitAuthTable;

    //action for authenticating a user and generating a token
    procedure AuthAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);

    //action for looking up details of an auth token
    procedure LookupAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);

    //action to validate whether a token is valid
    procedure ValidateAction(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; var Handled : Boolean);
  strict protected
    procedure DoInitialize(); override;
    function DoInitializeActions(): TActions; override;

    //performs authentication, and returns token
    function Authenticate(Const AUserKey : String;
      Const AParcelIdentity : String; Out Response : TAuthResponse) : Boolean;

    //checks to make sure a token exists and hasn't expired
    function Validate(Const AToken : String) : Boolean;

    //provided a token, will lookup the details associated with the
    //original request
    function Lookup(Const AToken : String; Out Details : TAuthRequest) : Boolean;
  public

  end;

var
  AuthController: TAuthController;
  EXPIRATION_TIME : Integer;

implementation
uses
  fpjson,
  jsonparser,
  DateUtils,
  FPIndexer,
  LazSysUtils,
  controller.dto;

{$R *.lfm}

{ TAuthController }

procedure TAuthController.InitAuthTable;
var
  LError: String;
begin
  //initialize our table if it doesn't exist
  ExecuteSQL(
    'CREATE TABLE IF NOT EXISTS ''auth''(' +
    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
    ' "token" varchar(128) NOT NULL,' +
    ' "user_key" varchar(50) NOT NULL,' +
    ' "parcel_identity" varchar(25) NOT NULL,' +
    ' "expire_date" datetime NOT NULL DEFAULT (datetime(''now'')),' +
    ' "create_date" datetime NOT NULL DEFAULT (datetime(''now'')));',
    LError
  );
end;

procedure TAuthController.AuthAction(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  LReq : TAuthRequest;
  LResp: TAuthResponse;
begin
  try
    AResponse.ContentType := 'application/json';
    Handled := True;
    LogRequester('AuthAction::', ARequest);

    //try to deserialize the request
    LReq.FromJSON(ARequest.Content);

    //don't expose any implementation details to caller in failure case
    //since auth method will do any logging it needs
    if not Authenticate(LReq.UserKey, LReq.ParcelID, LResp) then
      AResponse.Content := GetErrorJSON('unable to authenticate')
    else
      AResponse.Content := LResp.ToJSON; //success

  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON('unable to authenticate - server failure');
  end
  end;
end;

procedure TAuthController.LookupAction(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  LLookup : TLookupRequest;
  LDetails: TAuthRequest;
begin
  try
    AResponse.ContentType := 'application/json';
    Handled := True;
    LogRequester('LookupAction::', ARequest);

    //try to parse the request
    LLookup.FromJSON(ARequest.Content);

    //lookup will handle logging errors, return invalid if failure
    if not Lookup(LLookup.Token, LDetails) then
      AResponse.Content := GetErrorJSON('token is invalid')
    else
      AResponse.Content := LDetails.ToJSON;
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON('unable to lookup - server failure');
  end
  end;
end;

procedure TAuthController.ValidateAction(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  LValidate : TValidateRequest;
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
    if not Validate(LValidate.Token) then
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

procedure TAuthController.DoInitialize();
begin
  inherited DoInitialize();
  InitAuthTable;
end;

function TAuthController.DoInitializeActions(): TActions;
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

  //initialize the auth route actions
  SetLength(Result, Length(Result) + 3);

  LAction.Name := 'authenticate';
  LAction.Action := AuthAction;
  Result[I] := LAction;

  LAction.Name := 'lookup';
  LAction.Action := LookupAction;
  Result[I + 1] := LAction;

  LAction.Name := 'validate';
  LAction.Action := ValidateAction;
  Result[I + 2] := LAction;
end;

function TAuthController.Authenticate(const AUserKey: String;
  const AParcelIdentity: String; out Response: TAuthResponse): Boolean;
var
  LError: String;
  LExp: TDateTime;
  LData : TDatasetResponse;
  LObj: TJSONData;
begin
  Result := False;

  try
    //first look to see if we have a valid Response still. we add 30 seconds
    //to the filter so that the caller doesn't get jipped from the web
    //call delay, returning an expired token
    if GetSQLResultsJSON(
      'SELECT token, expire_date FROM auth WHERE user_key = ' + QuotedStr(AUserKey) +
      ' AND expire_date > DATETIME("now", "+30 seconds")' +
      ' ORDER BY id DESC LIMIT 1;',
      LData,
      LError
    ) then
    begin
      if LData.Count > 0 then
      begin
        //get the sql row to a json object
        LObj := GetJSON(LData[0]);

        if Assigned(LObj) then
        begin
          try
            //as long as we have a valid object we can get the properties
            if LObj.JSONType = jtObject then
            begin
              Response.Token := TJSONObject(LObj).Get('token');
              Response.Expires := TJSONObject(LObj).Get('expire_date');

              //found non-expired token, success
              Result := True;
              Exit;
            end
            else
              LogWarn('Authenticate::auth result not an object');
          finally
            LObj.Free;
          end;
        end;
      end;
    end;

    //otherwise we need to create a Response and store it in the db so
    //start with a time, and make sure it's in utc
    LExp := IncMilliSecond(NowUTC, EXPIRATION_TIME);
    Response.Expires := DateToISO8601(LExp);
    Response.Token := TGuid.NewGuid.ToString();

    //attempt to record the Response
    if not ExecuteSQL(
      'INSERT INTO auth(token, user_key, parcel_identity, expire_date)' +
      ' SELECT "' + Response.Token + '",' +
      ' ' + QuotedStr(AUserKey) + ',' +
      ' ' + QuotedStr(AParcelIdentity) + ',' +
      ' ' + QuotedStr(Response.Expires) + ';',
      LError
    ) then
    begin
      LogError('Authenticate::' + 'failed to insert auth token [' + LError + ']');
      Exit;
    end;

    Result := True;
  except on E : Exception do
    LogError('Authenticate::' + E.Message)
  end;
end;

function TAuthController.Validate(const AToken: String): Boolean;
var
  LData : TDatasetResponse;
  LError : String;
begin
  Result := False;

  //see if we have a record that matches withing the grace window
  if GetSQLResultsJSON(
    'SELECT 1 FROM auth WHERE token = ' + QuotedStr(AToken) +
    ' AND expire_date > DATETIME("now", "+5 seconds")' + //5 second grace time
    ' ORDER BY id DESC LIMIT 1;',
    LData,
    LError
  ) then
  begin
    if LData.Count > 0 then
      Result := True;
  end;
end;

function TAuthController.Lookup(const AToken: String;
  out Details: TAuthRequest): Boolean;
var
  LData: TDatasetResponse;
  LError: String;
begin
  Result := False;

  //no need to perform a query if we don't have a valid token to begin with
  if not Validate(AToken) then
    Exit;

  if GetSQLResultsJSON(
    'SELECT user_key, parcel_identity FROM auth WHERE token = ' + QuotedStr(AToken),
    LData,
    LError
  ) then
  begin
    //assign the details
    if LData.Count > 0 then
    begin
      Details.FromJSON(LData[0]);
      Result := True;
    end;
  end
  else
    LogError('Lookup::' + LError);
end;

initialization
  RegisterHTTPModule(GetControllerRoute('auth'), TAuthController);
  EXPIRATION_TIME := 10 * 60 * 1000; //default 10 min expiration
end.

