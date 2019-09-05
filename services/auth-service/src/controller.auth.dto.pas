unit controller.auth.dto;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  { TAuthResponse }
  (*
    data transfer object sent back from the authentication endpoint
  *)
  TAuthResponse = record
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

    //expiration time in ISO8601 (utc)
    property Expires : String read FExp write FExp;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

  { TAuthRequest }
  (*
    data transfer object to request authentication
  *)
  TAuthRequest = record
  public
    const
      PROP_USER = 'userKey';
      PROP_PARCEL = 'parcelIdentity';
  strict private
    FKey,
    FParcel : String;
  public
    property UserKey : String read FKey write FKey;
    property ParcelID : String read FParcel write FParcel;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

  { TLookupRequest }
  (*
    data transfer object to request details for lookup using a token
  *)
  TLookupRequest = record
  public
    const
      PROP_TOKEN = 'token';
  strict private
    FToken : String;
  public
    property Token : String read FToken write FToken;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

  //for now validate request only requires a token
  TValidateRequest = TLookupRequest;

implementation
uses
  fpjson,
  jsonparser;

{ TLookupRequest }

function TLookupRequest.ToJSON: String;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    LObj.Add(PROP_TOKEN, TJSONString.Create(FToken));

    Result := LObj.AsJSON;
  finally
    LObj.Free;
  end;
end;

procedure TLookupRequest.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TLookupRequest::FromJSON::invalid json [' + AJSON + ']');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TLookupRequest::FromJSON::json is not valid object [' + AJSON + ']');
  end;

  try
    FToken := TJSONObject(LObj).Get(PROP_TOKEN);
  finally
    LObj.Free;
  end;
end;

{ TAuthRequest }

function TAuthRequest.ToJSON: String;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    LObj.Add(PROP_USER, TJSONString.Create(FKey));
    LObj.Add(PROP_PARCEL, TJSONString.Create(FParcel));

    Result := LObj.AsJSON;
  finally
    LObj.Free;
  end;
end;

procedure TAuthRequest.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TAuthRequest::FromJSON::invalid json [' + AJSON + ']');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TAuthRequest::FromJSON::json is not valid object [' + AJSON + ']');
  end;

  try
    FKey := TJSONObject(LObj).Get(PROP_USER);
    FParcel := TJSONObject(LObj).Get(PROP_PARCEL);
  finally
    LObj.Free;
  end;
end;

{ TAuthResponse }

function TAuthResponse.ToJSON: String;
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

procedure TAuthResponse.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TToken::FromJSON::invalid json [' + AJSON + ']');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TToken::FromJSON::json is not valid object [' + AJSON + ']');
  end;

  try
    FExp := TJSONObject(LObj).Get(PROP_EXPIRES);
    FToken := TJSONObject(LObj).Get(PROP_TOKEN);
  finally
    LObj.Free;
  end;
end;

end.

