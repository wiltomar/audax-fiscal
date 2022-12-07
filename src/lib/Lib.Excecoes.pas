unit Lib.Excecoes;

interface

uses
  System.SysUtils;

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
  raise Exception.Create('Perd�o, ' + S);
end;

procedure Perdao(S: string; const Arguments: array of const); overload;
begin
  raise Exception.CreateFmt('Perd�o, ' + S, Arguments);
end;

procedure EmDesenvolvimento();
begin
  raise Exception.Create('Em desenvolvimento');
end;

procedure ErroHttp(statusCode: Integer; &message: string; error: string);
begin
//  case StatusCode of
//    401: raise Exception.Create('Erro HTTP: N�o autorizado' + CHAR_LF2 + &message);
//  end;
//  raise Exception.CreateFmt('Erro HTTP - status: %d, mensagem: %s, erro: %s', [statusCode, &message, error]);
  raise Exception.CreateFmt('%s' + CHAR_LF2 + 'Exce��o HTTP - status: %d %s', [Capitaliza(&message), statusCode, error]);
end;

procedure ObjetoNaoIniciado(S: string = '');
begin
  raise Exception.CreateFmt('Objeto %s n�o iniciado', [S]);
end;

procedure ParametroInvalido(S: string = '');
begin
  raise Exception.CreateFmt('Par�metro inv�lido: %s', [S]);
end;

procedure RetornoInvalido(S: string = '');
begin
  raise Exception.CreateFmt('Retorno inv�lido: %s', [S]);
end;

procedure Proibido(S: string = '');
begin
  raise Exception.CreateFmt('N�o permitido: %s', [S]);
end;

end.

