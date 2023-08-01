unit Api.Rotas;

interface

uses Classes, SysUtils, Horse, System.JSON, Horse.Jhonson, REST.Json,
  Model.DocumentoFiscal, Api.Componentes, System.Net.HttpClient;

const
  apiVersion = '/api/v1';

type
  TRotas = class
    class procedure Registra;
  end;

implementation

{ TRotas }

function retorno(const documentoFiscal: TJSONObject = nil): TJSONObject;
var
  lJson: TJSONObject;
begin
  lJson := TJSONObject.Create;
  try
    try
      with lJson do
      begin
        AddPair('success', true);
        AddPair('message', 'API Fiscal executando normalmente.');
        if not(documentoFiscal = nil) then
          AddPair('documentoFiscal', documentoFiscal);
      end;
    except
      on E: Exception do
      begin
        lJson.AddPair('success', false);
        lJson.AddPair('errors', E.message);
      end;
    end;

  finally
    Result := lJson;
    FreeAndNil(lJson);
  end;
end;

procedure index(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res
    .AddHeader('Designed by', 'Solução Sistemas Ltda.')
    .ContentType('application/json')
    .Send(retorno);
end;

procedure conectaMFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin

end;

procedure emiteDFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscal: TDocumentoFiscal;
  Resposta: TJSONObject;
  Components: Tcomponents;
begin
  DocumentoFiscal := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);

  Components := Tcomponents.Create(nil);
  try
    try
      Resposta := TJSon.ObjectToJsonObject(Components.EmiteDFe(DocumentoFiscal));
      Resposta := retorno(Resposta);

      Res
        .Status(THTTPStatus.OK)
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

class procedure TRotas.Registra;
begin
  THorse
    .Use(Jhonson)
    .Get(apiVersion + '/', index)
    .Get(apiVersion + '/fiscal/documentofiscal/mfe/conecta', conectaMFe)
    .Post(apiVersion + '/fiscal/documentofiscal/emite', emiteDFe)
    .Post(apiVersion + '/fiscal/documentofiscal/estorna', estornaCFe)
end;

end.
