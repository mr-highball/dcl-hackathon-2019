unit controller.status;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb,
  controller.base, controller.dto;

type

  (*
    enum representing an image's lifecycle
  *)
  TImageStatus = (
    isInProgress,
    isCompleted,
    isFailed
  );

  { TStatusController }

  TStatusController = class(TBaseController)
  strict private
    procedure InitStatusTable;
  strict protected
    procedure DoInitialize(); override;
  public
    (*
      inserts a new status record for the provided image token,
      and can be used if there are no previous status records
    *)
    function UpdateStatus(Const AImageToken : String;
      Const ANewStatus : TImageStatus; Out Error : String) : Boolean;
  end;

(*
  helper method for converting a status enum to a string representation
*)
function ImageStatusToString(Const AStatus : TImageStatus) : String;

(*
  helper method for converting a status string back to an enum value
*)
function StringToImageStatus(Const AStatus : String) : TImageStatus;

var
  StatusController: TStatusController;

const

  IMAGE_STATUS_LOOKUP : array[0 .. 2] of String = (
    'In-Progress',
    'Completed',
    'Failed'
  );

implementation
uses
  controller.registration;

function ImageStatusToString(const AStatus: TImageStatus): String;
begin
  Result := IMAGE_STATUS_LOOKUP[Ord(AStatus)];
end;

function StringToImageStatus(const AStatus: String): TImageStatus;
var
  I: Integer;
begin
  Result := isInProgress;

  for I := 0 to High(IMAGE_STATUS_LOOKUP) do
    if IMAGE_STATUS_LOOKUP[I].ToLower = AStatus.ToLower then
    begin
      Result := TImageStatus(I);
      Exit;
    end;

  //if we didn't find it above it means we have an invalid string
  Raise Exception.Create('ImageStatusToString::[' + AStatus + '] is not a valid status');
end;

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

function TStatusController.UpdateStatus(const AImageToken: String;
  const ANewStatus: TImageStatus; out Error: String): Boolean;
var
  LRegistration : TRegistrationController;
  LID: Integer;
  LData: TDatasetResponse;
begin
  Result := False;

  LRegistration := TRegistrationController.Create(nil);
  try
    try
      LogInfo('UpdateStatus::requesting new status [' + ImageStatusToString(ANewStatus) + '] for token [' + AImageToken + ']');

      //check if we can find the requested image token and get the internal
      //id so we can properly update
      if not LRegistration.LookupImageID(AImageToken, LID) then
      begin
        Error := 'UpdateStatus::[' + AImageToken + '] not found';
        Exit;
      end;

      //attempt to update the status
      if not GetSQLResultsJSON(
        'INSERT status (registration_id, status) SELECT ' + IntToStr(LID) + ',' +
        ' ' + ImageStatusToString(ANewStatus) + ';' +
        'SELECT last_insert_rowid() AS id;',
        LData,
        Error
      ) then
        Exit;

      //check the count returned in data to see if we were successful
      if LData.Count < 1 then
      begin
        Error := 'UpdateStatus::unable to update status for image [' + AImageToken + '];';
        Exit;
      end;

      Result := True;
    except on E : Exception do
      Error := 'UpdateStatus::' + E.Message;
    end;
  finally
    LRegistration.Free;
  end;
end;

initialization
  RegisterHTTPModule(GetControllerRoute('status'), TStatusController);
end.

