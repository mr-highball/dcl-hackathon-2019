program image_service;

{$mode delphi}{$H+}

uses
  SysUtils,
  fphttpapp,
  config.types,
  controller.base,
  controller.registration,
  controller.status,
  controller.image,
  controller.settings,
  BGRABitmap,
  BGRABitmapTypes;


procedure test;
var
  LBits : TBGRABitmap;
  LJSON, LError: String;
begin
  LBits := TBGRABitmap.Create('debugRemoveMeLater.bmp');
  ConstructDCLImageJSON(LBits, LJSON, LError);
  LBits.Free;
end;

const
  CONFIG_NAME = 'image_service_config.json';
var
  LConfig : IJSONConfig;
  LError: String;
begin
  test;
  //create and load config if one exists
  LConfig := CreateJSONConfig;

  if not FileExists(CONFIG_NAME) then
  begin
    LConfig.UpsertValue('port', '8080'); //set default port
    LConfig.UpsertValue('databaseName', 'image_service.sqlite3'); //default db name
    LConfig.UpsertValue('authServiceAddress', '127.0.0.1:8081'); //default db name
    LConfig.UpsertValue('maxImageWidth', '64');
    LConfig.UpsertValue('maxImageHeight', '64');

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
  MAX_IMAGE_WIDTH := StrToIntDef(LConfig['maxImageWidth'], 64);
  MAX_IMAGE_HEIGHT := StrToIntDef(LConfig['maxImageHeight'], 64);

  //init web app
  Application.Title:='image_service';
  Application.Port:=StrToIntDef(LConfig['port'], 8080); //shouldn't fail, but default here
  Application.Threaded:=True; //thread for every request
  Application.Initialize;
  Application.Run;
end.

