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
  private

  public

  end;

var
  StatusController: TStatusController;

implementation

{$R *.lfm}

initialization
  RegisterHTTPModule(GetControllerRoute('status'), TStatusController);
end.

