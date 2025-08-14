unit Api.Rotas;

interface

uses Classes, SysUtils, Horse, Horse.CORS, System.JSON, Horse.Jhonson, REST.Json,
  Model.DocumentoFiscal, Api.Componentes, System.Net.HttpClient, System.NetEncoding,
  Model.Inutilizacao, APIService, Api.Funcoes, Model.DocumentoFiscalManifesto,
  Model.DocumentoFiscalCartaCorrecao;

const
  apiVersion = '/api/v1/';

var
  cToken,
  cErrors,
  cMsg: string;
  cStatusCode: THTTPStatus;
  DocumentoFiscal: TDocumentoFiscal;
  Resposta: TJSONObject;

type
  TRotas = class
    class procedure Registra;
  end;

implementation

{ TRotas }

function seEntao(AValue: Boolean; const ATrue: THTTPStatus;
  AFalse: THTTPStatus = THTTPStatus.BadRequest): THTTPStatus;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

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
          AddPair('Constel Fiscal versão', build);
          AddPair('message', Msg);
          AddPair('response', documentoFiscal);
        end;
        Log(Format('Constel Fiscal versão: %s. %s', [build, Msg]));
      end
      else
      begin
        with lJson do
        begin
          AddPair('Constel Fiscal versão', build);
          AddPair('error', Error);
          AddPair('response', documentoFiscal);
        end;
        Log(Format('Constel Fiscal versão: %s. %s', [build, Error]));
      end;
    except
      on E: Exception do
      begin
        with lJson do
        begin
          AddPair('Constel Fiscal versão', build);
          AddPair('error', E.message);
          AddPair('response', documentoFiscal);
        end;
        Error := Format('Constel Fiscal versão: %s. Houve o segunite erro na tentativa de uso da API Fiscal: %s.', [build, E.Message]);
        Log(Error);
      end;
    end;

  finally
    Result := lJson;
  end;
end;

procedure index(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res
    .AddHeader('Designed-by', 'Constel Cloud')
    .ContentType('application/json')
    .Send<TJSONObject>(retorno(nil, '', 'Serviço em execução'))
    .Status(THTTPStatus.OK);
end;

procedure geraSPED(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  stringStream: TStringStream;
begin
  cToken := Req.Headers.Field('Authorization').AsString;
  InfoAPI().Autentica(cToken);

  try
    cErrors := '';
    cMsg := '';

    stringStream := Componentes.gerarSPED(Req, cErrors, cMsg);
    stringStream.Encoding.UTF8;

    try
      if Length(cErrors) = 0 then
      begin
        Res
          .Send(stringStream.DataString)
          .ContentType('text/plain;charset=utf8')
          .Status(THTTPStatus.Created);

        Log('Requisição realizada com sucesso.');
      end
      else
      begin
        Res
          .Send(Format('A solicitação não foi bem sucedida, o erro %s, foi retornado.',
            [cErrors]))
          .ContentType('text/plain')
          .Status(THTTPStatus.BadRequest);

        Log(Format('Ocorreu o seguinte erro na solicitação: %s', [cErrors]));
      end;
    finally
      stringStream.Free;
    end;

  except
    on E:Exception do
    begin
      Res
        .Send(Format('Não foi possível gerar o arquivo, com o erro %s.',
          [E.Message]))
        .Status(THTTPStatus.InternalServerError);
      Log(Format('Houve um erro no processamento da requisição. Mensagem: %s',
                 [E.Message]));

    end;
  end;
end;

procedure enviaArquivo(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  cToken := Req.Headers.Field('Authorization').AsString;
  InfoAPI().Autentica(cToken);

  cErrors := '';
  cMsg    := '';

  try
    Resposta := TJSON.ObjectToJsonObject(Componentes.enviaArquivo(Req.RawWebRequest.Files[0], cErrors, cMsg));
    try
      Resposta := retorno(Resposta, cErrors, cMsg);
      cStatusCode := seEntao(Length(cErrors) = 0, THTTPStatus.Created);

      Res
        .Status(cStatusCode)
        .Send<TJSONObject>(Resposta);
    except
      Res
        .Status(THTTPStatus.InternalServerError)
        .Send<TJSONObject>(Resposta);
    end;
  finally
  end;
end;

procedure cartaDeCorrecao(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscalCartaCorrecao: TDocumentoFiscalCartaCorrecao;
  Resposta: TJSONObject;
begin
  cToken := Req.Headers.Field('Authorization').AsString;
  InfoAPI().Autentica(cToken);

  cErrors := '';
  cMsg := '';

  DocumentoFiscalCartaCorrecao := TJson.JsonToObject<TDocumentoFiscalCartaCorrecao>(Req.Body);

  try
    Resposta := TJson.ObjectToJsonObject(Componentes.CartaDeCorrecao(DocumentoFiscalCartaCorrecao, cErrors, cMsg), [joIgnoreEmptyStrings, joIgnoreEmptyArrays, joDateIsUTC, joDateFormatISO8601]);
    try
      Resposta := retorno(Resposta, cErrors, cMsg);
      cStatusCode := seEntao(Length(cErrors) = 0, THTTPStatus.Created);

      Res
        .Status(cStatusCode)
        .Send<TJSONObject>(Resposta);
    except
      Res
        .Status(THTTPStatus.BadRequest)
        .Send<TJSONObject>(Resposta);
    end;
  finally
  end;
end;

procedure manifestaDocumento(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscalManifesto: TDocumentoFiscalManifesto;
begin
  cToken := Req.Headers.Field('Authorization').AsString;
  InfoAPI().Autentica(cToken);

  cErrors := '';
  cMsg    := '';

  DocumentoFiscalManifesto := TJson.JsonToObject<TDocumentoFiscalManifesto>(Req.Body);
  Resposta := TJson.ObjectToJsonObject(Componentes.manifestarDocumento(DocumentoFiscalManifesto, cErrors, cMsg),
    [joIgnoreEmptyStrings, joIgnoreEmptyArrays, joDateIsUTC, joDateFormatISO8601]);
  try
    Resposta := retorno(Resposta, cErrors, cMsg);
    cStatusCode := seEntao(Length(cErrors) = 0, THTTPStatus.OK);

    Res
      .Status(cStatusCode)
      .Send<TJSONObject>(Resposta);
  except
    Res
      .Status(THTTPStatus.InternalServerError)
      .Send<TJSONObject>(Resposta);
  end;
end;


procedure emiteDFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  cToken := Req.Headers.Field('Authorization').AsString;
  InfoAPI().Autentica(cToken);

  cErrors := '';
  cMsg    := '';

  DocumentoFiscal := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);
  Resposta := TJson.ObjectToJsonObject(Componentes.EmiteDFe(DocumentoFiscal, cErrors, cMsg),
    [joIgnoreEmptyStrings, joIgnoreEmptyArrays, joDateIsUTC, joDateFormatISO8601]);
  try
    Resposta := retorno(Resposta, cErrors, cMsg);
    cStatusCode := seEntao(Length(cErrors) = 0, THTTPStatus.Created);

    Res
      .Status(cStatusCode)
      .Send<TJSONObject>(Resposta);
  except
    Res
      .Status(THTTPStatus.InternalServerError)
      .Send<TJSONObject>(Resposta);
  end;
end;

procedure estornaDFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscalDFe: TDocumentoFiscal;
begin
  cToken := Req.Headers.Field('Authorization').AsString;
  InfoAPI().Autentica(cToken);

  cErrors := '';
  cMsg    := '';

  DocumentoFiscalDFe := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);

  try
    Resposta := TJson.ObjectToJsonObject(Componentes.CancelarDFe(DocumentoFiscalDFe, cErrors, cMsg));
    Resposta := retorno(Resposta, cErrors, cMsg);
    cStatusCode := seEntao(Length(cErrors) = 0, THTTPStatus.OK);

    Res
      .Status(cStatusCode)
      .Send<TJSONObject>(Resposta);
  except
    Res
      .Status(cStatusCode)
      .Send<TJSONObject>(Resposta);
  end;
end;

procedure imrimeDFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  cToken := Req.Headers.Field('Authorization').AsString;
  InfoAPI().Autentica(cToken);

  cErrors := '';
  cMsg    := '';

  DocumentoFiscal := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);

  try
    Resposta := TJson.ObjectToJsonObject(Componentes.ImprimirDFe(DocumentoFiscal, cErrors, cMsg), [joIgnoreEmptyStrings, joIgnoreEmptyArrays, joDateIsUTC, joDateFormatISO8601]);
    try
      Resposta := retorno(Resposta, cErrors, cMsg);
      cStatusCode := seEntao(Length(cErrors) = 0, THTTPStatus.OK);

      Res
        .Status(cStatusCode)
        .Send<TJSONObject>(Resposta);
    except
      Res
        .Status(THTTPStatus.InternalServerError)
        .Send<TJSONObject>(Resposta);
    end;
  finally
  end;
end;

procedure HandleOptions(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  if SameText(Req.RawWebRequest.Method, 'OPTIONS') then
  begin
    Res
      .RawWebResponse
      .SetCustomHeader('Access-Control-Allow-Origin', '*');
    Res
      .RawWebResponse
      .SetCustomHeader('Access-Control-Allow-Credentials', 'true');
    Res
      .RawWebResponse
      .SetCustomHeader('Access-Control-Allow-Headers', 'Content-Type,Authorization');
    Res
      .RawWebResponse
      .SetCustomHeader('Access-Control-Allow-Methods', 'GET,POST,PUT,DELETE,OPTIONS');
    Res
      .RawWebResponse
      .SetCustomHeader('Access-Control-Expose-Headers', 'Content-Length,Content-Type');

    Res.Status(204).Send('');
    Exit;
  end
  else
    Next;
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
    .Use(HandleOptions)
    .Use(Jhonson)
    .Use(CORS)
    .Get(apiVersion, index)
    .Get(apiVersion + 'fiscal/arquivo/sped', geraSPED)
    .Post(apiVersion + 'fiscal/documento/emite', emiteDFe)
    .Post(apiVersion  + 'fiscal/arquivo/manifesto', manifestaDocumento)
    .Post(apiVersion  + 'fiscal/arquivo/cartacorrecao', cartaDeCorrecao)
    .Post(apiVersion  + 'fiscal/documento/imprime', imrimeDFe)
    .Post(apiVersion + 'fiscal/documento/estorna', estornaDFe)
    .Post(apiVersion + 'fiscal/arquivo/envia', enviaArquivo);

end;

end.
