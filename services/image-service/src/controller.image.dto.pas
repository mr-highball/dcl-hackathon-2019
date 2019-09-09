unit controller.image.dto;

{$mode delphi}

interface

uses
  Classes,
  SysUtils;

type

  { TDrawCommand }

  TDrawCommand = record
  public
    const
      PROP_TOP_LEFT = 'topLeft';
      PROP_TOP_RIGHT = 'topRight';
      PROP_BOTTOM_LEFT = 'bottomLeft';
      PROP_BOTTOM_RIGHT = 'bottomRight';
      PROP_RED = 'red';
      PROP_GREEN = 'green';
      PROP_BLUE = 'blue';
      PROP_ALPHA = 'alpha';
  strict private
    FAlpha: Byte;
    FBL: Integer;
    FBlue: Byte;
    FBR: Integer;
    FGreen: Byte;
    FRed: Byte;
    FTL: Integer;
    FTR: Integer;
  public
    { positional }

    property TopLeft : Integer read FTL write FTL;
    property TopRight : Integer read FTR write FTR;
    property BottomLeft : Integer read FBL write FBL;
    property BottomRight : Integer read FBR write FBR;

    { color }

    property Red : Byte read FRed write FRed;
    property Green : Byte read FGreen write FGreen;
    property Blue : Byte read FBlue write FBlue;
    property Alpha : Byte read FAlpha write FAlpha;

    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

  TDrawCommands = TArray<TDrawCommand>;

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

    function Add(Const ACommand : TDrawCommand);
    function ToJSON : String;
    procedure FromJSON(Const AJSON : String);
  end;

implementation
uses
  fpjson,
  jsonparser;

{ TDrawCommand }

function TDrawCommand.ToJSON: String;
var
  LObj: TJSONObject;
begin
  LObj := TJSONObject.Create;
  try
    try
      LObj.Add(PROP_TOP_LEFT, FTL);
      LObj.Add(PROP_TOP_RIGHT, FTR);
      LObj.Add(PROP_BOTTOM_LEFT, FBL);
      LObj.Add(PROP_BOTTOM_RIGHT, FBR);

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
      FTL := TJSONObject(LObj).Get(PROP_TOP_LEFT);
      FTR := TJSONObject(LObj).Get(PROP_TOP_RIGHT);
      FBL := TJSONObject(LObj).Get(PROP_BOTTOM_LEFT);
      FBR := TJSONObject(LObj).Get(PROP_BOTTOM_RIGHT);

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
  Result := FCommands[I];
end;

function TImageResponse.GetCount: Integer;
begin
  Result := Length(FCommands);
end;

function TImageResponse.Add(const ACommand: TDrawCommand);
begin
  SetLength(FCommands, Succ(Length(FCommands));
  FCommands[High(FCommands)] := ACommand;
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

