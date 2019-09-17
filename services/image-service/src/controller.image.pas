unit controller.image;

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

  { TImageController }

  TImageController = class(TBaseController)
  private
    procedure InitImageTable;
  protected
    procedure DoInitialize(); override;
  public

  end;

var
  ImageController: TImageController;

implementation

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

initialization
  RegisterHTTPModule(GetControllerRoute('image'), TImageController);
end.

