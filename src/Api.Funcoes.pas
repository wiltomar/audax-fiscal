unit Api.Funcoes;

interface

uses
  Classes, SysUtils, IniFiles, Horse;

procedure onListen(Horse: THorse);
procedure startApi;
procedure stopApi;
procedure statusApi;

implementation

procedure onListen(Horse: THorse);
begin
  if THorse.IsRunning then
    Writeln(Format('Api fiscal em execu��o e escutando na porta %d', [Horse.Port]))
  else
    Writeln('A api fiscal n�o est� sendo executada no momento.');
  exit;
end;

procedure startApi;
var
  ini: TIniFile;
  arq: String;
  porta: Integer;
begin
  arq := ChangeFileExt(ParamStr(0), '.ini');
  try
    ini := TIniFile.Create(arq);
    try
      Porta := ini.ReadInteger('Config', 'Porta', 9000);

      THorse.Listen(Porta, Api.Funcoes.onListen);
    finally
      ini.Free;
    end;
  except
    Writeln(Format('N�o foi poss�vel carregar o arquivo de configura��o em %s. \nA aplica��o ser� encerrada!', [ChangeFileExt(ParamStr(0), '.ini')]));
  end;
end;

procedure stopApi;
begin
  try
    if THorse.IsRunning then
      THorse.StopListen;
  except
    on E: Exception do
      WriteLn(Format('Erro na tentativa de encerrar a api, com a seguinte mensagem: %s.', [E.Message]));
  end;
end;

procedure statusApi;
begin
  if THorse.isRunning then
    WriteLn('A api est� em execu��o')
  else
    WriteLn('A api n�o est� sendo executada no momento');
end;

end.
