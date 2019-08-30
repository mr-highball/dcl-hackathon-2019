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

  public

  end;

var
  ImageController: TImageController;

implementation

{$R *.lfm}

initialization
  RegisterHTTPModule(GetControllerRoute('image'), TImageController);
end.

