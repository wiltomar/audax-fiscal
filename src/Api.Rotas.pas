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

procedure index(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
  function getIndex: string;
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
        end;
      except
        on E: Exception do
        begin
          lJson.AddPair('success', false);
          lJson.AddPair('errors', E.message);
        end;
      end;

    finally
      Result := lJson.Format;
      FreeAndNil(lJson);
    end;
  end;
begin
  Res
    .AddHeader('Designed by', 'Solução Sistemas Ltda.')
    .ContentType('application/json')
    .Send(getIndex);
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
    Resposta := TJSon.ObjectToJsonObject(Components.EmiteDFe(DocumentoFiscal));

    Res
      .Send<TJSONObject>(Resposta);
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
