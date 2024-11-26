unit Lib.Excecoes;

interface

uses
  System.SysUtils;
 
 type
  EHttpResponse = class(Exception)
  private
    FStatusCodde: Integer;
  public
    constructor Create(AStatusCode: Integer; AMessage: string; AError: string = '');
    class procedure Throw(AStatusCode: Integer; AMessage: string; AError: string = '');
    class procedure ParamMissing(const ParamName: string);
    class procedure BadRequest(const &Message: string);
    property StatusCodde: Integer read FstatusCodde;
  end;

procedure Erro(S: string); overload;
procedure Erro(S: string; const Arguments: array of const); overload;
procedure Perdao(S: string); overload;
procedure Perdao(S: string; const Arguments: array of const); overload;
procedure EmDesenvolvimento();
procedure ErroHttp(statusCode: Integer; &message: string; error: string);
procedure ObjetoNaoIniciado(S: string = '');
procedure ParametroInvalido(S: string = '');
procedure RetornoInvalido(S: string = '');
procedure Proibido(S: string = '');

implementation

uses Lib.Funcoes;

constructor EHttpResponse.Create(AStatusCode: Integer; AMessage: string; AError: string = '');
begin
  if AError > '' then
    AMessage := AMessage + ': ' + AError;
  inherited Create(AMessage);
  FStatusCodde := AStatusCode;
end;

class procedure EHttpResponse.Throw(AStatusCode: Integer; AMessage: string; AError: string = '');
begin
  raise EHttpResponse.Create(AStatusCode, AMessage, AError);
end;

class procedure EHttpResponse.ParamMissing(const ParamName: string);
begin
  EHttpResponse.Throw(400, 'parâmetro não fornecido "' + ParamName + '"');
end;

class procedure EHttpResponse.BadRequest(const &Message: string);
begin
  EHttpResponse.Throw(400, 'incoerência: ' + &Message);
end;

procedure Erro(S: string); overload;
begin
  raise Exception.Create(S);
end;

procedure Erro(S: string; const Arguments: array of const); overload;
begin
  raise Exception.CreateFmt(S, Arguments);
end;

procedure Perdao(S: string);
begin
  raise Exception.Create('Perdão, ' + S);
end;

procedure Perdao(S: string; const Arguments: array of const); overload;
begin
  raise Exception.CreateFmt('Perdão, ' + S, Arguments);
end;

procedure EmDesenvolvimento();
begin
  raise Exception.Create('Em desenvolvimento');
end;

procedure ErroHttp(statusCode: Integer; &message: string; error: string);
begin
//  case StatusCode of
//    401: raise Exception.Create('Erro HTTP: Não autorizado' + CHAR_LF2 + &message);
//  end;
//  raise Exception.CreateFmt('Erro HTTP - status: %d, mensagem: %s, erro: %s', [statusCode, &message, error]);
  raise Exception.CreateFmt('%s' + CHAR_LF2 + 'Exceção HTTP - status: %d %s', [Capitaliza(&message), statusCode, error]);
end;

procedure ObjetoNaoIniciado(S: string = '');
begin
  raise Exception.CreateFmt('Objeto %s não iniciado', [S]);
end;

procedure ParametroInvalido(S: string = '');
begin
  raise Exception.CreateFmt('Parâmetro inválido: %s', [S]);
end;

procedure RetornoInvalido(S: string = '');
begin
  raise Exception.CreateFmt('Retorno inválido: %s', [S]);
end;

procedure Proibido(S: string = '');
begin
  raise Exception.CreateFmt('Não permitido: %s', [S]);
end;

end.

