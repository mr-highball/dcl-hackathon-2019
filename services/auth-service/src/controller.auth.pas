unit controller.auth;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  controller.base;

type

  { TToken }

  TToken = record
  public
    const
      PROP_TOKEN = 'token';
      PROP_EXPIRES = 'expires';
  strict private
    FExp: String;
    FToken: String;
  public
    //guid identifier
    property Token : String read FToken write FToken;

    //expiration time in milliseconds
    property Expires : String read FExp write FExp;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

  { TAuthController }

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
      Const AParcelIdentity : String; Out Token : TToken) : Boolean;
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
  FPIndexer;

{$R *.lfm}

{ TToken }

function TToken.ToJSON: String;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    LObj.Add(PROP_TOKEN, TJSONString.Create(FToken));
    LObj.Add(PROP_EXPIRES, TJSONString.Create(FExp));

    Result := LObj.AsJSON;
  finally
    LObj.Free;
  end;
end;

procedure TToken.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    Raise Exception.Create('invalid json form TToken');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    Raise Exception.Create('json is not valid object');
  end;

  try
    FExp := TJSONObject(LObj).Get(PROP_EXPIRES);
    FToken := TJSONObject(LObj).Get(PROP_TOKEN);
  finally
    LObj.Free;
  end;
end;

{ TAuthController }

procedure TAuthController.InitAuthTable;
var
  LError: String;
begin
  //initialize our table if it doesn't exist
  ExecuteSQL(
    'CREATE TABLE IF NOT EXISTS "auth"(' +
    ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
    ' "token" varchar(128) NOT NULL,' +
    ' "user_key" varchar(50) NOT NULL,' +
    ' "parcel_identity" varchar(25) NOT NULL,' +
    ' "expire_date" datetime NOT NULL DEFAULT (datetime(''now'',''localtime'')),' +
    ' "create_date" datetime NOT NULL DEFAULT (datetime(''now'',''localtime'')));',
    LError
  );
end;

procedure TAuthController.AuthAction(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  LToken: TToken;
begin
  try
    AResponse.ContentType := 'application/json';
    Handled := True;
    LogRequester('AuthAction::', ARequest);
    Authenticate('testKey', '0,0', LToken);
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON(E.Message);
  end
  end;
end;

procedure TAuthController.LookupAction(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  try
    AResponse.ContentType := 'application/json';
    Handled := True;
    LogRequester('LookupAction::', ARequest);
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON(E.Message);
  end
  end;
end;

procedure TAuthController.ValidateAction(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  try
    AResponse.ContentType := 'application/json';
    Handled := True;
    LogRequester('ValidateAction::', ARequest);
  except on E : Exception do
  begin
    LogError(E.Message);
    AResponse.Content := GetErrorJSON(E.Message);
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
  const AParcelIdentity: String; out Token: TToken): Boolean;
var
  LError: String;
  LExp: TDateTime;
begin
  Result := False;

  try
    //first look to see if we have a valid token still
    //...

    //otherwise we need to create a token and store it in the db so
    //start with a time, and make sure it's in utc
    LExp := IncMilliSecond(Now, EXPIRATION_TIME);
    Token.Expires := DateToISO8601(LExp);
    Token.Token := TGuid.NewGuid.ToString();

    //attempt to record the token
    if not ExecuteSQL(
      'INSERT INTO auth(token, user_key, parcel_identity, expire_date)' +
      ' select "' + Token.Token + '",' +
      ' ' + QuotedStr(AUserKey) + ',' +
      ' ' + QuotedStr(AParcelIdentity) + ',' +
      ' ' + QuotedStr(Token.Expires) + ';',
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

initialization
  RegisterHTTPModule(GetControllerRoute('auth'), TAuthController);
  EXPIRATION_TIME := 10 * 60 * 1000; //default 10 min expiration
end.

