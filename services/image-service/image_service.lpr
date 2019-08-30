program image_service;

{$mode delphi}{$H+}

uses
  fphttpapp,
  controller.base,
  controller.registration,
  controller.status,
  controller.image;

begin
  Application.Title:='image_service';
  DEFAULT_DB_NAME := 'image_service.sqlite3';
  //todo - read in configuration here with config.utils
  Application.Port:=8080;
  Application.Threaded:=True;
  Application.Initialize;
  Application.Run;
end.

