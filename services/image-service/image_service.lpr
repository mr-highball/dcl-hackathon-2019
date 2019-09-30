program image_service;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,
  fphttpapp,
  config.types,
  controller.base,
  controller.registration,
  controller.status,
  controller.image,
  controller.settings,
  controller.registration.dto;

const
  CONFIG_NAME = 'image_service_config.json';
var
  LConfig : IJSONConfig;
  LError: String;
begin
  CONTROLLER_LOG_TYPES := [];

  WriteLn('image service starting...');
  //create and load config if one exists
  LConfig := CreateJSONConfig;

  if not FileExists(CONFIG_NAME) then
  begin
    LConfig.UpsertValue('port', '8083'); //set default port
    LConfig.UpsertValue('databaseName', 'image_service.sqlite3'); //default db name
    LConfig.UpsertValue('authServiceAddress', '127.0.0.1:8081'); //default db name
    LConfig.UpsertValue('maxImageWidth', '48');
    LConfig.UpsertValue('maxImageHeight', '48');
    LConfig.UpsertValue('useSSL', 'true');
    LConfig.UpsertValue('sslPublicKeyFile', CERT_PUBLIC_FILE);
    LConfig.UpsertValue('sslPrivateKeyFile', CERT_PRIVATE_FILE);
    LConfig.UpsertValue('sslPrivateKeyPassphrase', CERT_PASSPHRASE);

    if not LConfig.SaveToFile(CONFIG_NAME, LError) then
      WriteLn(LError);
  end
  else
    if not LConfig.LoadFromFile(CONFIG_NAME, LError) then
      WriteLn(LError);

  //set the database name for the image service
  DEFAULT_DB_NAME := LConfig['databaseName'];

  //update the auth service address
  AUTH_ADDRESS := LConfig['authServiceAddress'];

  //update the max dimensions
  MAX_IMAGE_WIDTH := StrToIntDef(LConfig['maxImageWidth'], 48);
  MAX_IMAGE_HEIGHT := StrToIntDef(LConfig['maxImageHeight'], 48);

  //update ssl info
  CERT_PUBLIC_FILE := LConfig['sslPublicKeyFile'];
  CERT_PRIVATE_FILE := LConfig['sslPrivateKeyFile'];
  CERT_PASSPHRASE := LConfig['sslPrivateKeyPassphrase'];

  //init web app
  Application.Title:='image_service';
  Application.UseSSL := StrToBoolDef(LConfig['useSSL'], True);
  Application.Port:=StrToIntDef(LConfig['port'], 8083); //shouldn't fail, but default here
  Application.Threaded:=True; //thread for every request
  Application.Initialize;
  Application.Run;
end.

