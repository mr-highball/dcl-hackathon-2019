unit controller.image;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb,
  controller.base,
  controller.image.dto;

type

  { TImageController }

  TImageController = class(TBaseController)
  private
    procedure InitImageTable;
  protected
    procedure DoInitialize(); override;
  public
    function UpdateImage(constref AToken : String;
      constref AImage : TImageResponse; Out Error : String) : Boolean;
  end;

var
  ImageController: TImageController;

implementation
uses
  controller.registration;
{$R *.lfm}

{ TImageController }

procedure TImageController.InitImageTable;
var
  LError: String;
begin
  //initialize our table if it doesn't exist
  if not ExecuteSQL(
    'CREATE TABLE IF NOT EXISTS image(' +
    ' id Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
    ' registration_id integer,' +
    ' commands varchar(4000) NOT NULL,' +
    ' create_date datetime NOT NULL DEFAULT (datetime(''now'')),' +
    ' FOREIGN KEY(registration_id) REFERENCES registration(id));',
    LError
  ) then
    LogError(LError);
end;

procedure TImageController.DoInitialize();
begin
  inherited DoInitialize();
  InitImageTable;
end;

function TImageController.UpdateImage(constref AToken: String; constref
  AImage: TImageResponse; out Error: String): Boolean;
var
  LRegistration: TRegistrationController;
  LID: Integer;
begin
  Result := False;

  LRegistration := TRegistrationController.Create(nil);
  try
    //get the registration id
    if not LRegistration.LookupImageID(AToken, LID) then
    begin
      Error := 'Fetch::[' + AToken + '] not found';
      Exit;
    end;

    //upsert the image commands
    if not ExecuteSQL(
      'DELETE image WHERE registration_id = ' + IntToStr(LID) + ';' +
      'INSERT image(registration_id, commands)' +
      ' SELECT ' + IntToStr(LID) + ',' + QuotedStr(AImage.ToJSON) + ';',
      Error
    ) then
      Exit;

    Result := True;
  finally
    LRegistration.Free;
  end;
end;

initialization
  RegisterHTTPModule(GetControllerRoute('image'), TImageController);
end.

