program image_service;

{$mode objfpc}{$H+}

uses
  fphttpapp, unit1;

begin
  Application.Title:='httpproject1';
  Application.Port:=8080;
  Application.Threaded:=True;
  Application.Initialize;
  Application.Run;
end.

