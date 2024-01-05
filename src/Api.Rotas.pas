unit Api.Rotas;

interface

uses Classes, SysUtils, Horse, Horse.CORS, System.JSON, Horse.Jhonson, REST.Json,
  Model.DocumentoFiscal, Api.Componentes, System.Net.HttpClient, System.NetEncoding;

const
  apiVersion = '/api/v1';

type
  TRotas = class
    class procedure Registra;
  end;

implementation

{ TRotas }

function retorno(const documentoFiscal: TJSONObject = nil; Error: String = ''; Msg: String = ''): TJSONObject;
var
  lJson: TJSONObject;
begin
  lJson := TJSONObject.Create;
  try
    try
      if Error = '' then
      begin
        with lJson do
        begin
          AddPair('message', Msg);
          AddPair('error', '');
          AddPair('response', documentoFiscal);
        end;
      end
      else
      begin
        with lJson do
        begin
          AddPair('message', '');
          AddPair('error', Error);
          AddPair('response', documentoFiscal);
        end;

      end;
    except
      on E: Exception do
      begin
        lJson.AddPair('message', 'Erro interno.');
        lJson.AddPair('error', E.message);
        lJson.AddPair('response', '{}');
      end;
    end;

  finally
    Result := lJson;
  end;
end;

procedure index(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res
    .AddHeader('Designed by', 'Constel Cloud')
    .ContentType('application/json')
    .Send<TJSONObject>(retorno);
end;

procedure conectaMFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin

end;

procedure emiteDFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscal: TDocumentoFiscal;
  Resposta: TJSONObject;
  Components: Tcomponents;
  Error, Msg: String;
  StatusCode: THttpStatus;
begin
  DocumentoFiscal := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);

  Components := Tcomponents.Create(nil);
  try
    Resposta := TJson.ObjectToJsonObject(Components.EmiteDFe(DocumentoFiscal, Error, Msg), [joIgnoreEmptyStrings, joIgnoreEmptyArrays, joDateIsUTC, joDateFormatISO8601]);
    try
      Resposta := retorno(Resposta, Error, Msg);

      if Error > '' then
        StatusCode := THTTPStatus.BadRequest
      else
        StatusCode := THTTPStatus.OK;
      Res
        .Status(StatusCode)
        .Send<TJSONObject>(Resposta);
    except
      Res
        .Status(THTTPStatus.BadRequest)
        .Send<TJSONObject>(Resposta);
    end;
  finally
    Components.Free;
  end;
end;

procedure estornaCFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscalCFe: TDocumentoFiscal;
  Resposta: TJSONObject;
  Components: Tcomponents;
begin
  DocumentoFiscalCFe := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);

  Components := Tcomponents.Create(nil);
  try
    Resposta := TJson.ObjectToJsonObject(Components.CancelarDFe(DocumentoFiscalCFe));

    Res
      .Send<TJSONObject>(Resposta);
  finally
    Components.Free;
  end;
end;

procedure gerarDANFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscal: TDocumentoFiscal;
  Resposta: TJSONObject;
  Components: TComponents;
  Error, Msg: String;
  StatusCode: THTTPStatus;
begin
  DocumentoFiscal := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);

  Components := Tcomponents.Create(nil);
  try
    Resposta := TJson.ObjectToJsonObject(Components.ImprimirDFe(DocumentoFiscal, Error, Msg), [joIgnoreEmptyStrings, joIgnoreEmptyArrays, joDateIsUTC, joDateFormatISO8601]);
    try
      Resposta := retorno(Resposta, Error, Msg);

      if Error > '' then
        StatusCode := THTTPStatus.BadRequest
      else
        StatusCode := THTTPStatus.OK;
      Res
        .Status(StatusCode)
        .Send<TJSONObject>(Resposta);
    except
      Res
        .Status(THTTPStatus.BadRequest)
        .Send<TJSONObject>(Resposta);
    end;
  finally
    Components.Free;
  end;
end;

class procedure TRotas.Registra;
begin
  HorseCORS
    .AllowedOrigin('*')
    .AllowedCredentials(true)
    .AllowedHeaders('*')
    .AllowedMethods('*')
    .ExposedHeaders('*');

  THorse
    .Use(Jhonson)
    .Use(CORS)
    .Get(apiVersion + '/', index)
    .Get(apiVersion + '/fiscal/documentofiscal/mfe/conecta', conectaMFe)
    .Post(apiVersion + '/fiscal/documentofiscal/emite', emiteDFe)
    .Get(apiVersion + '/fiscal/documentofiscal/imprime', gerarDANFe)
    .Post(apiVersion + '/fiscal/documentofiscal/estorna', estornaCFe)
end;

end.
