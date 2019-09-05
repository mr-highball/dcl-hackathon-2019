unit controller.registration.dto;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  controller.auth.dto;

type

  { TValidateURLRequest }

  TValidateURLRequest = record
  public
    const
      PROP_URL = 'url';
      PROP_AUTH = 'validation';
  strict private
    FURL : String;
    FValidate : TValidateRequest;
  public
    property URL : String read FURL write FURL;
    property AuthValidation : TValidateRequest read FValidate write FValidate;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

  { TRegisterURLRequest }

  TRegisterURLRequest = record
  public
    const
      PROP_VALIDATE = 'validation';
      PROP_AUTH = 'authentication';
  strict private
    FValidate : TValidateURLRequest;
    FAuth : TAuthRequest;
  public
    property Validation : TValidateURLRequest read FValidate write FValidate;
    property  Authentication : TAuthRequest read FAuth write FAuth;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

  //returns a token used to check status and pull image info
  TRegisterURLResponse = TLookupRequest;

implementation
uses
  fpjson,
  jsonparser;

{ TRegisterURLRequest }

function TRegisterURLRequest.ToJSON: String;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    try
      LObj.Add(PROP_VALIDATE, GetJSON(FValidate.ToJSON));
      LObj.Add(PROP_AUTH, GetJSON(FAuth.ToJSON));

      Result := LObj.AsJSON;
    finally
      LObj.Free;
    end;
  except
      raise;
  end;
end;

procedure TRegisterURLRequest.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TRegisterURLRequest::FromJSON::invalid json [' + AJSON + ']');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TRegisterURLRequest::FromJSON::json is not valid object [' + AJSON + ']');
  end;

  try
    try
      FValidate.FromJSON(TJSONObject(LObj).Objects[PROP_VALIDATE].AsJSON);
      FAuth.FromJSON(TJSONObject(LObj).Objects[PROP_AUTH].AsJSON);
    finally
      LObj.Free;
    end;
  except
    raise;
  end;
end;

{ TValidateURLRequest }

function TValidateURLRequest.ToJSON: String;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    try
      LObj.Add(PROP_URL, TJSONString.Create(FURL));
      LObj.Add(PROP_AUTH, GetJSON(FValidate.ToJSON));

      Result := LObj.AsJSON;
    finally
      LObj.Free;
    end;
  except
      raise;
  end;
end;

procedure TValidateURLRequest.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TValidateURLRequest::FromJSON::invalid json [' + AJSON + ']');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TValidateURLRequest::FromJSON::json is not valid object [' + AJSON + ']');
  end;

  try
    try
      FURL := TJSONObject(LObj).Get(PROP_URL);
      FValidate.FromJSON(TJSONObject(LObj).Objects[PROP_AUTH].AsJSON);
    finally
      LObj.Free;
    end;
  except
    raise;
  end;
end;

end.

