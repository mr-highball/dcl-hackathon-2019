program auth_service;

{$mode delphi}{$H+}

uses
  SysUtils,
  fphttpapp,
  config.types,
  controller.base,
  controller.auth;

const
  CONFIG_NAME = 'auth_service_config.json';
var
  LConfig : IJSONConfig;
  LError: String;
begin
  WriteLn('authentication service started...');

  //create and load config if one exists
  LConfig := CreateJSONConfig;

  if not FileExists(CONFIG_NAME) then
  begin
    LConfig.UpsertValue('port', '8081'); //set default port
    LConfig.UpsertValue('databaseName', 'auth_service.sqlite3'); //default db name
    LConfig.UpsertValue('tokenExpirationTime', IntToStr(5 * 60 * 1000)); //default expiration

    if not LConfig.SaveToFile(CONFIG_NAME, LError) then
      WriteLn(LError);
  end
  else
    if not LConfig.LoadFromFile(CONFIG_NAME, LError) then
      WriteLn(LError);

  //set the database name for the image service
  DEFAULT_DB_NAME := LConfig['databaseName'];

  //set the expiration time of the token
  EXPIRATION_TIME := StrToIntDef(LConfig['tokenExpirationTime'], 5 * 60 * 1000);

  //init web app
  Application.Title:='auth_service';
  Application.Port:=StrToIntDef(LConfig['port'], 8081);
  WriteLn('listening on port ', Application.Port);
  WriteLn('database is ', DEFAULT_DB_NAME);
  Application.Threaded:=True;
  Application.Initialize;
  Application.Run;
end.

