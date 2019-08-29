unit controller.registration;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, SQLite3Conn, SQLDB;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    connection: TSQLite3Connection;
    query: TSQLQuery;
    transaction: TSQLTransaction;
  private

  public

  end;

var
  FPWebModule1: TFPWebModule1;

implementation

{$R *.lfm}

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.

