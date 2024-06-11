unit Api.Funcoes;

interface

uses
  Classes, SysUtils, Horse, IdSSLOpenSSL, Vcl.Forms, Model.Config, System.JSON, Rest.Json,
  WinApi.Windows;

type
  TGetPasswordSSL = class
    procedure OnGetPassword(var Password: String);
  end;
const
  cKey = 31987;
  C1 = 77543;
  C2 = 59381;

procedure startApi;
procedure stopApi;
procedure statusApi;
procedure InfoConfig(var FConfig: TConfig);
procedure Log(const Mensagem: String);

function encrypt(const S: string): string;
function decrypt(const S: string): string;


implementation

procedure InfoConfig(var FConfig: TConfig);
begin
  if Assigned(FConfig) then
    Exit;
  const FileName = ExtractFilePath(Application.ExeName) + 'config.json';
  if not FileExists(FileName) then
  begin
    Log(Format('Desculpe, o arquivo de configuração "%s" não foi encontrado.', [FileName]));
    Application.Terminate;
  end;
  var jo: TJSONObject;
  var sl := TStringList.Create();
  try
    sl.LoadFromFile(FileName);
    jo := (TJSONObject.ParseJSONValue(sl.Text) as TJSONObject);
  finally
    sl.Free();
  end;
  try
    FConfig := TJSON.JsonToObject<TConfig>(jo);
  except
    Log('Desculpe, arquivo de configuração danificado');
    Application.Terminate;
  end;
end;

procedure Log(const Mensagem: String);
begin
  try
    if Mensagem = EmptyStr then
    begin
      WriteLn(Format('%s - Não há uma mensagem para exibição.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now)]));
      Exit;
    end;

    WriteLn(Format('%s - %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now), Mensagem]));
  except
    on E: Exception do
      WriteLn(Format('%s - Houve um erro no processamento da mensagem, conforme a seguir. %s',
                    [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now), E.Message]));
  end;
end;

procedure onStop(Horse: THorse);
begin
  if THorse.IsRunning then
    Log(Format('Api fiscal será encerrada na porta %d', [Horse.Port]))
  else
    Log('A api fiscal foi encerrada corretamente.');
end;

procedure startApi;
var
  Porta: Integer;
  FConfig: TConfig;
  LGetSSLPassword: TGetPasswordSSL;
begin
  InfoConfig(FConfig);
  if FConfig.ambienteseguro then
  begin
    THorse.IOHandleSSL
    .CertFile(FConfig.emitente.certificado.caminhoraiz + '\constel.crt')
    .KeyFile(FConfig.emitente.certificado.caminhoraiz + '\constel.key')
    .OnGetPassword(LGetSSLPassword.OnGetPassword)
    .SSLVersions([sslvTLSv1_2])
    .Method(sslvTLSv1_2)
    .Active(True);
  end;

  Porta := FConfig.porta;

  THorse.Listen(Porta,
    procedure
    begin
      if THorse.IsRunning then
        Log(Format('Api fiscal em execução e escutando na porta %d', [Porta]))
      else
        Log('A api fiscal não está sendo executada no momento.');
    end
  );
end;

procedure stopApi;
begin
  try
    if THorse.IsRunning then
      THorse.StopListen;
    Log('Encerrando a API.');
    Application.Terminate;
  except
    on E: Exception do
      Log(Format('Erro na tentativa de encerrar a api, com a seguinte mensagem: %s.', [E.Message]));
  end;
end;

procedure statusApi;
begin
  if THorse.isRunning then
    Log('A api está em execução')
  else
    Log('A api não está sendo executada no momento');
end;

function decode(const S: AnsiString): AnsiString;
const
  Map: array[AnsiChar] of Byte = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 62, 0, 0, 0, 63, 52, 53,
    54, 55, 56, 57, 58, 59, 60, 61, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2,
    3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 0, 0, 0, 0, 0, 0, 26, 27, 28, 29, 30,
    31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
    46, 47, 48, 49, 50, 51, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0);
var
  I: LongInt;
begin
  case Length(S) of
    2:
      begin
        I := Map[S[1]] + (Map[S[2]] shl 6);
        SetLength(Result, 1);
        Move(I, Result[1], Length(Result))
      end;
    3:
      begin
        I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12);
        SetLength(Result, 2);
        Move(I, Result[1], Length(Result))
      end;
    4:
      begin
        I := Map[S[1]] + (Map[S[2]] shl 6) + (Map[S[3]] shl 12) +
          (Map[S[4]] shl 18);
        SetLength(Result, 3);
        Move(I, Result[1], Length(Result))
      end
  end
end;

function PreProcess(const S: string): string;
var
  SS: string;
begin
  SS := S;
  Result := '';
  while SS <> '' do
  begin
    Result := Result + Decode(Copy(SS, 1, 4));
    Delete(SS, 1, 4)
  end
end;

function InternalDecrypt(const S: string; Key: Word): string;
var
  I: Word;
  Seed: Word;
begin
  Result := S;
  Seed := Key;
  for I := 1 to Length(Result) do
  begin
    Result[I] := Char(Byte(Result[I]) xor (Seed shr 8));
    Seed := (Byte(S[I]) + Seed) * Word(C1) + Word(C2)
  end
end;

function decrypt(const S: string): string;
begin
  Result := InternalDecrypt(PreProcess(S), cKey)
end;

function Encode(const S: string): string;
const
  Map: array[0..63] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    'abcdefghijklmnopqrstuvwxyz0123456789+/';
var
  I: LongInt;
begin
  I := 0;
  Move(S[1], I, Length(S));
  case Length(S) of
    1:
      Result := Map[I mod 64] + Map[(I shr 6) mod 64];
    2:
      Result := Map[I mod 64] + Map[(I shr 6) mod 64] +
        Map[(I shr 12) mod 64];
    3:
      Result := Map[I mod 64] + Map[(I shr 6) mod 64] +
        Map[(I shr 12) mod 64] + Map[(I shr 18) mod 64]
  end
end;

function PostProcess(const S: string): string;
var
  SS: string;
begin
  SS := S;
  Result := '';
  while SS <> '' do
  begin
    Result := Result + Encode(Copy(SS, 1, 3));
    Delete(SS, 1, 3)
  end
end;

function InternalEncrypt(const S: string; Key: Word): string;
var
  I: Word;
  Seed: Word;
begin
  Result := S;
  Seed := Key;
  for I := 1 to Length(Result) do
  begin
    Result[I] := Char(Byte(Result[I]) xor (Seed shr 8));
    Seed := (Byte(Result[I]) + Seed) * Word(C1) + Word(C2)
  end
end;

function encrypt(const S: string): string;
begin
  Result := PostProcess(InternalEncrypt(S, cKey))
end;

{ TGetPasswordSSL }

procedure TGetPasswordSSL.OnGetPassword(var Password: String);
begin
  Password := '';
end;

end.
