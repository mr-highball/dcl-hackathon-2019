unit controller.image.dto;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  controller.auth.dto;

type

  { TDrawCommand }

  TDrawCommand = record
  public
    const
      PROP_TOP_LEFT_X = 'topLX';
      PROP_TOP_RIGHT_X = 'topRX';
      PROP_BOTTOM_LEFT_X = 'botLX';
      PROP_BOTTOM_RIGHT_X = 'botRX';
      PROP_TOP_LEFT_Y = 'topLY';
      PROP_TOP_RIGHT_Y = 'topRY';
      PROP_BOTTOM_LEFT_Y = 'botLY';
      PROP_BOTTOM_RIGHT_Y = 'botRY';
      PROP_RED = 'r';
      PROP_GREEN = 'g';
      PROP_BLUE = 'b';
      PROP_ALPHA = 'a';
  strict private
    FAlpha: Byte;
    FBL: Integer;
    FBlue: Byte;
    FBLY: Integer;
    FBR: Integer;
    FBRY: Integer;
    FGreen: Byte;
    FRed: Byte;
    FTL: Integer;
    FTLY: Integer;
    FTR: Integer;
    FTRY: Integer;
  public
    { positional }

    property TopLeftX : Integer read FTL write FTL;
    property TopRightX : Integer read FTR write FTR;
    property BottomLeftX : Integer read FBL write FBL;
    property BottomRightX : Integer read FBR write FBR;

    property TopLeftY : Integer read FTLY write FTLY;
    property TopRightY : Integer read FTRY write FTRY;
    property BottomLeftY : Integer read FBLY write FBLY;
    property BottomRightY : Integer read FBRY write FBRY;

    { color }

    property Red : Byte read FRed write FRed;
    property Green : Byte read FGreen write FGreen;
    property Blue : Byte read FBlue write FBlue;
    property Alpha : Byte read FAlpha write FAlpha;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

  TDrawCommands = TArray<TDrawCommand>;

  { TImageRequest }

  TImageRequest = record
  public
    const
      PROP_IMAGE = 'image';
      PROP_AUTH = 'authentication';
  strict private
    FAuth: TLookupRequest;
    FImage: TLookupRequest;
  public
    property Image : TLookupRequest read FImage write FImage;
    property Authentication : TLookupRequest read FAuth write FAuth;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
    constructor Create(Const AImageToken, AAuthToken : String);
  end;

  { TImageResponse }

  TImageResponse = record
  public
    const
      PROP_COMMANDS = 'commands';
  strict private
    FCommands: TDrawCommands;
    FCount: Integer;
    function GetCommand(const AIndex : Integer): TDrawCommand;
    function GetCount: Integer;
  public
    property Commands : TDrawCommands read FCommands write FCommands;
    property Count : Integer read GetCount;
    property ByIndex[Const AIndex : Integer] : TDrawCommand read GetCommand; default;

    function Add(constref ACommand : TDrawCommand) : Integer;
    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

implementation
uses
  fpjson,
  jsonparser;

{ TImageRequest }

function TImageRequest.ToJSON: String;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    try
      LObj.Add(PROP_IMAGE, GetJSON(FImage.ToJSON));
      LObj.Add(PROP_AUTH, GetJSON(FAuth.ToJSON));

      Result := LObj.AsJSON;
    finally
      LObj.Free;
    end;
  except
      raise;
  end;
end;

procedure TImageRequest.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TImageRequest::FromJSON::invalid json [' + AJSON + ']');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TImageRequest::FromJSON::json is not valid object [' + AJSON + ']');
  end;

  try
    try
      FImage.FromJSON(TJSONObject(LObj).Objects[PROP_IMAGE].AsJSON);
      FAuth.FromJSON(TJSONObject(LObj).Objects[PROP_AUTH].AsJSON);
    finally
      LObj.Free;
    end;
  except
    raise;
  end;
end;

constructor TImageRequest.Create(const AImageToken, AAuthToken: String);
begin
  FImage.Token := AImageToken;
  FAuth.Token := AAuthToken;
end;

{ TDrawCommand }

function TDrawCommand.ToJSON: String;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    try
      LObj.Add(PROP_TOP_LEFT_X, FTL);
      LObj.Add(PROP_TOP_RIGHT_X, FTR);
      LObj.Add(PROP_BOTTOM_LEFT_X, FBL);
      LObj.Add(PROP_BOTTOM_RIGHT_X, FBR);

      LObj.Add(PROP_TOP_LEFT_Y, FTLY);
      LObj.Add(PROP_TOP_RIGHT_Y, FTRY);
      LObj.Add(PROP_BOTTOM_LEFT_Y, FBLY);
      LObj.Add(PROP_BOTTOM_RIGHT_Y, FBRY);

      LObj.Add(PROP_RED, FRed);
      LObj.Add(PROP_GREEN, FGreen);
      LObj.Add(PROP_BLUE, FBlue);
      LObj.Add(PROP_ALPHA, FAlpha);

      Result := LObj.AsJSON;
    finally
      LObj.Free;
    end;
  except
      raise;
  end;
end;

procedure TDrawCommand.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TDrawCommand::FromJSON::invalid json [' + AJSON + ']');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TDrawCommand::FromJSON::json is not valid object [' + AJSON + ']');
  end;

  try
    try
      FTL := TJSONObject(LObj).Get(PROP_TOP_LEFT_X);
      FTR := TJSONObject(LObj).Get(PROP_TOP_RIGHT_X);
      FBL := TJSONObject(LObj).Get(PROP_BOTTOM_LEFT_X);
      FBR := TJSONObject(LObj).Get(PROP_BOTTOM_RIGHT_X);

      FTLY := TJSONObject(LObj).Get(PROP_TOP_LEFT_Y);
      FTRY := TJSONObject(LObj).Get(PROP_TOP_RIGHT_Y);
      FBLY := TJSONObject(LObj).Get(PROP_BOTTOM_LEFT_Y);
      FBRY := TJSONObject(LObj).Get(PROP_BOTTOM_RIGHT_Y);

      FRed := TJSONObject(LObj).Get(PROP_RED);
      FGreen := TJSONObject(LObj).Get(PROP_GREEN);
      FBlue := TJSONObject(LObj).Get(PROP_BLUE);
      FAlpha := TJSONObject(LObj).Get(PROP_ALPHA);
    finally
      LObj.Free;
    end;
  except
    raise;
  end;
end;

{ TImageResponse }

function TImageResponse.GetCommand(const AIndex : Integer): TDrawCommand;
begin
  Result := FCommands[AIndex];
end;

function TImageResponse.GetCount: Integer;
begin
  Result := Length(FCommands);
end;

function TImageResponse.Add(constref ACommand: TDrawCommand) : Integer;
begin
  SetLength(FCommands, Succ(Length(FCommands)));
  FCommands[High(FCommands)] := ACommand;
  Result := High(FCommands);
end;

function TImageResponse.ToJSON: String;
var
  LObj: TJSONObject;
  LArr : TJSONArray;
  I: Integer;
begin
  LObj := TJSONObject.Create;
  LArr := TJSONArray.Create;
  try
    try
      //serialize all the commands and add them to the array
      for I := 0 to High(FCommands) do
        LArr.Add(GetJSON(FCommands[I].ToJSON));

      LObj.Add(PROP_COMMANDS, LArr);
      Result := LObj.AsJSON;
    finally
      LObj.Free;
    end;
  except
      raise;
  end;
end;

procedure TImageResponse.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
  I: Integer;
  LArr: TJSONArray;
  LCommand : TDrawCommand;
begin
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TImageResponse::FromJSON::invalid json [' + AJSON + ']');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TImageResponse::FromJSON::json is not valid object [' + AJSON + ']');
  end;

  try
    try
      LArr := TJSONObject(LObj).Arrays[PROP_COMMANDS];
      SetLength(FCommands, LArr.Count);

      for I := 0 to Pred(LArr.Count) do
      begin
        LCommand.FromJSON(LArr.Objects[I].AsJSON);
        FCommands[I] := LCommand;
      end;
    finally
      LObj.Free;
    end;
  except
    raise;
  end;
end;

end.

