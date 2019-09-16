unit controller.status.dto;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  controller.auth.dto,
  controller.status;

type

  { TStatusRequest }
  (*
    simple request with the image token, doesn't require authentication
  *)
  TStatusRequest = TLookupRequest;

  { TStatusResponse }

  TStatusResponse = record
  public
    const
      PROP_STATUS = 'status';
  strict private
    FStatus: TImageStatus;
  public
    property Status : TImageStatus read FStatus write FStatus;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

implementation
uses
  fpjson,
  jsonparser;

{ TStatusResponse }

function TStatusResponse.ToJSON: String;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    try
      LObj.Add(PROP_STATUS, ImageStatusToString(FStatus));

      Result := LObj.AsJSON;
    finally
      LObj.Free;
    end;
  except
      raise;
  end;
end;

procedure TStatusResponse.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TStatusResponse::FromJSON::invalid json [' + AJSON + ']');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TStatusResponse::FromJSON::json is not valid object [' + AJSON + ']');
  end;

  try
    try
      FStatus := StringToImageStatus(TJSONObject(LObj).Get(PROP_STATUS));
    finally
      LObj.Free;
    end;
  except
    raise;
  end;
end;

end.

