unit Api.Rotas;

interface

uses Classes, SysUtils, Horse, Horse.CORS, System.JSON, Horse.Jhonson, REST.Json,
  Model.DocumentoFiscal, Api.Componentes, System.Net.HttpClient, System.NetEncoding,
  Model.Inutilizacao, APIService, Api.Funcoes;

const
  apiVersion = '/api/v1/';

var
  cToken: string;

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
          Log('sucesso. API fiscal em execução.');
        end;
      end
      else
      begin
        with lJson do
        begin
          var internalError: string;
          internalError := Error;
          AddPair('message', '');
          AddPair('error', internalError);
          AddPair('response', documentoFiscal);
          Log('erro. A API Fiscal retornou um erro.');
        end;

      end;
    except
      on E: Exception do
      begin
        lJson.AddPair('message', 'Erro interno.');
        lJson.AddPair('error', E.message);
        lJson.AddPair('response', '{}');
        Log(Format('falha geral. Houve o segunite erro na tentativa de uso da API Fiscal: %s.', [E.Message]));
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
    .Send<TJSONObject>(retorno(nil, '', 'API Fiscal em execução'))
    .Status(200);
end;

procedure geraSPED(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  stringStream: TStringStream;
  erros: string;
begin
  cToken := Req.Headers.Field('Authorization').AsString;
  InfoAPI().Autentica(cToken);

  try
    erros := '';

    stringStream := Components.gerarSPED(Req, erros);
    stringStream.Encoding.UTF8;

    try
      if Length(erros) = 0 then
      begin
        Res
          .Send(stringStream.DataString)
          .ContentType('text/plain;charset=utf8')
          .Status(201);

        Log('Requisição realizada com sucesso.');
      end
      else
      begin
        Res
          .Send(Format('A solicitação não foi bem sucedida, o erro %s, foi retornado.',
            [erros]))
          .ContentType('text/plain')
          .Status(406);

        Log(Format('Ocorreu o seguinte erro na solicitação: %s', [erros]));
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
        .Status(501);
      Log(Format('Houve um erro no processamento da requisição. Mensagem: %s',
                 [E.Message]));

    end;
  end;
end;

procedure emiteDFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscal: TDocumentoFiscal;
  Resposta: TJSONObject;
  Components: Tcomponents;
  Error, Msg: String;
  StatusCode: THttpStatus;
begin
//  cToken := Req.Headers.Field('Authorization').AsString;
//  InfoAPI().Autentica(cToken);

  DocumentoFiscal := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);

  if Assigned(Components) then
    Components := nil;

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

procedure estornaDFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscalDFe: TDocumentoFiscal;
  Resposta: TJSONObject;
  Components: Tcomponents;
begin
//  cToken := Req.Headers.Field('Authorization').AsString;
//  InfoAPI().Autentica(cToken);

  DocumentoFiscalDFe := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);

  if Assigned(Components) then
    Components := nil;

  Components := Tcomponents.Create(nil);
  try
    Resposta := TJson.ObjectToJsonObject(Components.CancelarDFe(DocumentoFiscalDFe));

    Res
      .Send<TJSONObject>(Resposta);
  finally
    Components.Free;
  end;
end;

procedure imrimeDFe(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
var
  DocumentoFiscal: TDocumentoFiscal;
  Resposta: TJSONObject;
  Components: TComponents;
  Error, Msg: String;
  StatusCode: THTTPStatus;
begin
//  cToken := Req.Headers.Field('Authorization').AsString;
//  InfoAPI().Autentica(cToken);

  DocumentoFiscal := TJson.JsonToObject<TDocumentoFiscal>(Req.Body);

  if Assigned(Components) then
    Components := nil;

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
    .Get(apiVersion, index)
    .Get(apiVersion + 'fiscal/arquivo/sped', geraSPED)
    .Post(apiVersion + 'fiscal/documento/emite', emiteDFe)
    .Post(apiVersion  + 'fiscal/documento/imprime', imrimeDFe)
    .Post(apiVersion + 'fiscal/documento/estorna', estornaDFe)

    // Manter retrocompatibilidade.
    .Post(apiVersion + 'fiscal/documentofiscal/emite', emiteDFe)
    .Post(apiVersion  + 'fiscal/documentofiscal/imprime', imrimeDFe)
    .Post(apiVersion + 'fiscal/documentofiscal/estorna', estornaDFe)
end;

end.
