unit controller.status;

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

  { TStatusController }

  TStatusController = class(TBaseController)
  strict private
    procedure InitStatusTable;
  strict protected
    procedure DoInitialize(); override;
  public

  end;

var
  StatusController: TStatusController;

implementation

{$R *.lfm}

{ TStatusController }

procedure TStatusController.InitStatusTable;
var
  LError: String;
begin
  //initialize our table if it doesn't exist
  if not ExecuteSQL(
    'CREATE TABLE IF NOT EXISTS status(' +
    ' id Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
    ' registration_id integer,' +
    ' status varchar(128) NOT NULL,' +
    ' create_date datetime NOT NULL DEFAULT (datetime(''now'')),' +
    ' FOREIGN KEY(registration_id) REFERENCES registration(id));',
    LError
  ) then
    LogError(LError);
end;

procedure TStatusController.DoInitialize();
begin
  inherited DoInitialize();
  InitStatusTable;
end;

initialization
  RegisterHTTPModule(GetControllerRoute('status'), TStatusController);
end.

