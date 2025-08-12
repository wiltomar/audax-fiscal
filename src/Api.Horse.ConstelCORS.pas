unit Api.Horse.ConstelCORS;
{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}
interface

uses
  {$IF DEFINED(FPC)}
    SysUtils,
  {$ELSE}
    System.SysUtils,
  {$ENDIF}
  Horse;

type
  ApiConstelCORSConfig = record
  public
    function AllowedOrigin(AAllowedOrigin: string): ApiConstelCORSConfig;
    function AllowedCredentials(AAllowedCredentials: Boolean): ApiConstelCORSConfig;
    function AllowedHeaders(AAllowedHeaders: string): ApiConstelCORSConfig;
    function AllowedMethods(AAllowedMethods: string): ApiConstelCORSConfig;
    function ExposedHeaders(AExposedHeaders: string): ApiConstelCORSConfig;
  end;

function ApiConstelCORS(): ApiConstelCORSConfig; overload;
procedure ConstelCORS(Req: THorseRequest; Res: THorseResponse; Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}  TProc {$ENDIF}); overload;

implementation

uses
  {$IF DEFINED(FPC)}
    httpdefs, StrUtils,
  {$ELSE}
    Web.HTTPApp, System.StrUtils,
  {$ENDIF}
  Horse.Commons;

var
  LAllowedOrigin: string;
  LAllowedCredentials: string;
  LAllowedHeaders: string;
  LAllowedMethods: string;
  LExposedHeaders: string;

procedure ConstelCORS(Req: THorseRequest; Res: THorseResponse; Next: {$IF DEFINED(FPC)}TNextProc{$ELSE}  TProc {$ENDIF});
begin
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Origin', LAllowedOrigin);
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Credentials', LAllowedCredentials);
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Headers', LAllowedHeaders);
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Methods', LAllowedMethods);
  Res.RawWebResponse.SetCustomHeader('Access-Control-Expose-Headers', LExposedHeaders);
  if Req.RawWebRequest.Method = 'OPTIONS' then
  begin
    Res.Send('').Status(THTTPStatus.NoContent);
    Req.RawWebRequest.Connection := 'close';
    Exit;
  end
  else
    Next();
end;

{ HorseCORS }

function ApiConstelCORSConfig.AllowedCredentials(AAllowedCredentials: Boolean): ApiConstelCORSConfig;
begin
  LAllowedCredentials := ifthen(AAllowedCredentials, 'true', 'false');
end;

function ApiConstelCORSConfig.AllowedHeaders(AAllowedHeaders: string): ApiConstelCORSConfig;
begin
  LAllowedHeaders := AAllowedHeaders;
end;

function ApiConstelCORSConfig.AllowedMethods(AAllowedMethods: string): ApiConstelCORSConfig;
begin
  LAllowedMethods := AAllowedMethods;
end;

function ApiConstelCORSConfig.AllowedOrigin(AAllowedOrigin: string): ApiConstelCORSConfig;
begin
  LAllowedOrigin := AAllowedOrigin;
end;

function ApiConstelCORSConfig.ExposedHeaders(AExposedHeaders: string): ApiConstelCORSConfig;
begin
  LExposedHeaders := AExposedHeaders;
end;

function ApiConstelCORS(): ApiConstelCORSConfig;
begin
  //
end;

initialization
  LAllowedOrigin := '*';
  LAllowedCredentials := 'true';
  LAllowedHeaders := '*';
  LAllowedMethods := '*';
  LExposedHeaders := '*';

end.
