unit controller.base;

{$mode delphi}{$H+}

interface

uses
  SysUtils,
  Classes,
  httpdefs,
  fpHTTP,
  fpWeb, SQLite3Conn,
  SQLDB,
  eventlog;

type

  TControllerLogType = (clInfo, clWarn, clError);
  TControllerLogTypes = set of TControllerLogType;

  { TAction }
  (*
    singular action for a controller
  *)
  TAction = record
  private
    FAction: TWebActionEvent;
    FName: String;
  public
    property Name : String read FName write FName;
    property Action : TWebActionEvent read FAction write FAction;
  end;

  TActions = TArray<TAction>;

  { TBaseController }
  (*
    base class for controllers that offers virtual methods
    to override for consistency across dcl micro services
  *)
  TBaseController = class(TFPWebModule)
    connection: TSQLite3Connection;
    transaction: TSQLTransaction;
    //default health check event
    procedure HealthCheck(Sender : TObject; ARequest : TRequest;
      AResponse : TResponse; Var Handled : Boolean);
  strict private
    FRoot : String;
    FName : String;
    FLogTypes : TControllerLogTypes;
    function GetDBName: String;
    function GetFullPath: String;
    function GetLogTypes: TControllerLogTypes;
    function GetRoot: String;
    procedure SetDBName(Const AValue: String);
    procedure SetLogTypes(Const AValue: TControllerLogTypes);
    procedure SetRoot(Const AValue: String);

    //check if the db exists on the file system
    function DBExists : Boolean;

    //create the db if the file doesn't exist
    procedure CreateDB;


  strict protected

    (*
      logging methods accessible to children
      --------------------------------------------------------------------------
    *)

    //log info messages
    procedure LogInfo(Const AMsg : String);

    //log warning messages
    procedure LogWarn(Const AMsg : String);

    //log error messages
    procedure LogError(Const AMsg : String);

    (*
      raw sql commands
      --------------------------------------------------------------------------
    *)

    //executes blocking sql that will return an exception if failed
    function ExecuteSQL(Const ASQL : String; Out Error : String) : Boolean;

    //runs non-blocking sql that provides only evidence in the log a failure occurred
    procedure FireAndForget(Const ASQL : String);

    (*
      children override these methods
      --------------------------------------------------------------------------
    *)

    //called when Initialize() is called to ensure db is setup
    procedure DoInitialize(); virtual;

    //called when a controller is initialized to define the actions available
    function DoInitializeActions() : TActions; virtual;
  public
    (*
      properties
      --------------------------------------------------------------------------
    *)

    //name of the sqlite db file
    property DataBaseName : String read GetDBName write SetDBName;

    //root directory of the db file
    property RootPath : String read GetRoot write SetRoot;

    //full path to the db file (root + delim + dbname)
    property FullPath : String read GetFullPath;

    property LogTypes : TControllerLogTypes read GetLogTypes write SetLogTypes;

    (*
      methods
      --------------------------------------------------------------------------
    *)

    (*
      performs any initialization step required to the database file
      ensuring future queries have everything they need to operate
    *)
    procedure Setup;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  DEFAULT_DB_NAME : String;

function GetControllerRoute(Const AControllerName : String) : String;

implementation
var
  FLog : TEventLog; //log singleton

function GetControllerRoute(const AControllerName: String): String;
begin
  Result := 'controller/' + AControllerName;
end;

{$R *.lfm}

{ TBaseController }

function TBaseController.GetDBName: String;
begin
  Result := FName;
end;

function TBaseController.GetFullPath: String;
begin
  if Length(FRoot) < 1 then
    Result := DataBaseName
  else
  begin
    //check first to see if the last index is a path delimiter
    if IsPathDelimiter(FRoot, High(FRoot)) then
      Result := FRoot + DataBaseName
    //otherwise we need to append the delimiter to the end of root
    else
      Result := FRoot + PathDelim + DataBaseName;
  end;
end;

function TBaseController.GetLogTypes: TControllerLogTypes;
begin
  Result := FLogTypes;
end;

function TBaseController.GetRoot: String;
begin
  Result := FRoot;
end;

procedure TBaseController.SetDBName(const AValue: String);
begin
  FName := AValue;
end;

procedure TBaseController.SetLogTypes(const AValue: TControllerLogTypes);
begin
  FLogTypes := AValue;
end;

procedure TBaseController.SetRoot(const AValue: String);
begin
  FRoot := AValue;
end;

function TBaseController.DBExists: Boolean;
begin
  Result := FileExists(FullPath);
end;

procedure TBaseController.CreateDB;
begin
  //set to default if caller hasn't specified
  if FName.IsEmpty then
    FName := DEFAULT_DB_NAME;

  connection.Close(true);
  connection.DatabaseName := FullPath;
  connection.Open;
  connection.Close;
end;

procedure TBaseController.HealthCheck(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Handled := True;
  AResponse.ContentType := 'application/json';
  AResponse.Content := '{"status" : "OK"}';
end;

procedure TBaseController.LogInfo(const AMsg: String);
begin
  while FLog.Active do
    Sleep(5);

  //we disable the log each time to prevent file locking as well as
  //to keep up a "rolling" log file
  try
    FLog.AppendContent:=True;
    FLog.LogType:=ltFile;
    FLog.FileName:=ExtractFileDir(ParamStr(0)) + PathDelim + DataBaseName + '_' + FormatDateTime('mm_dd_yyyy',Now) + '.log';
    FLog.Active:=True;
    FLog.Info(AMsg);
  finally
    FLog.Active := False;
  end;
end;

procedure TBaseController.LogWarn(const AMsg: String);
begin
  while FLog.Active do
    Sleep(5);

  //log warnings
  try
    FLog.AppendContent:=True;
    FLog.LogType:=ltFile;
    FLog.FileName:=ExtractFileDir(ParamStr(0)) + PathDelim + DataBaseName + '_' + FormatDateTime('mm_dd_yyyy',Now) + '.log';
    FLog.Active:=True;
    FLog.Warning(AMsg);
  finally
    FLog.Active := False;
  end;
end;

procedure TBaseController.LogError(const AMsg: String);
begin
  while FLog.Active do
    Sleep(5);

  //log errors
  try
    FLog.AppendContent:=True;
    FLog.LogType:=ltFile;
    FLog.FileName:=ExtractFileDir(ParamStr(0)) + PathDelim + DataBaseName + '_' + FormatDateTime('mm_dd_yyyy',Now) + '.log';
    FLog.Active:=True;
    FLog.Error(AMsg);
  finally
    FLog.Active := False;
  end;
end;

function TBaseController.ExecuteSQL(const ASQL: String; out Error: String): Boolean;
begin
  Result := False;

  try
    connection.Close;
    connection.Open;

    try
      //attempt to execute sql
      connection.ExecuteDirect(ASQL);

      //commit transaction the disk
      transaction.Commit;
    finally
      connection.Close;
    end;
  except on E : Exception do
  begin
    Error := E.Message;
    LogError(Error);
  end;
  end;
end;

procedure TBaseController.FireAndForget(const ASQL: String);
var
  LError: String;
begin
  //need to create an ezthread and fire
  ExecuteSQL(ASQL, LError); //todo - make this async
end;

procedure TBaseController.DoInitialize();
begin
  //nothing
end;

function TBaseController.DoInitializeActions(): TActions;
var
  LHealth : TAction;
begin
  SetLength(Result, 1);

  //define the health action which outputs all available actions
  LHealth.Name := 'health';
  LHealth.Action := HealthCheck;

  //add the health action
  Result[0] := LHealth;
end;

procedure TBaseController.Setup;
var
  LActions : TActions;
  I: Integer;
  LAction: TFPWebAction;
begin
  try
    //before children Setup is called, we need to ensure the
    //database exists, and if not, create it
    if not DBExists then
      CreateDB;

    //attempt to call child method
    DoInitialize;

    //Setup the actions for this controller
    Actions.Clear; //clear existing if any
    LActions := DoInitializeActions; //get child actions

    //for each of the actions returned to us by our children, add
    //an actual action item
    for I := 0 to High(LActions) do
    begin
      LAction := Actions.Add;

      //first action will be the default
      if I = 0 then
        LAction.Default := True;

      //configure the action
      LAction.Name := LActions[I].Name;
      LAction.OnRequest := LActions[I].Action;
    end;
  except on E : Exception do
  begin
    LogError(E.Message); //log
    raise E; //re-throw so caller knows and can handle
  end;
  end;
end;

constructor TBaseController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //we need at least one default action defined in base before calling this
  //ie. the health check will be defined at the design level
  Setup;
end;

destructor TBaseController.Destroy;
begin

  inherited Destroy;
end;

initialization
  RegisterHTTPModule('controller', TBaseController);
  DEFAULT_DB_NAME := 'database.sqlite3';
  FLog := TEventLog.Create(nil);
finalization
  FLog.Active := False;
  FLog.Free;
end.

