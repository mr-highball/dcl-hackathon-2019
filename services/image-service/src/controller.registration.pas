unit controller.registration;

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

  { TRegistrationController }

  TRegistrationController = class(TBaseController)
  private

  public

  end;

var
  RegistrationController: TRegistrationController;

implementation

{$R *.lfm}

initialization
  RegisterHTTPModule(GetControllerRoute('registration'), TRegistrationController);
end.

