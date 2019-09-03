unit controller.settings;

{$mode delphi}

interface

uses
  Classes,
  SysUtils;

type
  TAuthSvcActions = (aaAuth, aaValidate, aaLookup);

const
  AUTH_ROUTE = 'controller/auth';
  AUTH_ACTION_AUTH = '?a=authenticate';
  AUTH_ACTION_VALIDATE = '?a=validate';
  AUTH_ACTION_LOOKUP = '?a=lookup';
var
  //root IP address set by config
  AUTH_ADDRESS  : String;

function GetAuthSvcRoute(const AType :  TAuthSvcActions) : String;

implementation

function GetAuthSvcRoute(const AType: TAuthSvcActions): String;
begin
  case AType of
    aaAuth : Result := AUTH_ADDRESS + AUTH_ROUTE + AUTH_ACTION_AUTH;
    aaValidate : Result := AUTH_ADDRESS + AUTH_ROUTE + AUTH_ACTION_VALIDATE;
    aaLookup : Result := AUTH_ADDRESS + AUTH_ROUTE + AUTH_ACTION_LOOKUP;
  end;
end;

end.

