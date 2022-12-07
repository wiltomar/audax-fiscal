unit Lib.Funcoes;

interface

uses
  System.SysUtils, System.Character, Sistema.Tipos;

type
  AString = Sistema.Tipos.AString;

const
  APP_NAME = 'ConstelPDV';

const
  CHAR_LF = #10;
  CHAR_CR = #13;
  CHAR_LF2 = #10#10;
  CHAR_MARCA = Char($25AA);

const
  LINE_FEED = CHAR_CR + CHAR_LF;
  LINE_FEED2 = LINE_FEED + LINE_FEED;

type
  TAHelper<T> = class
    class procedure Add(var A: TArray<T>; Value: T);
    class procedure Delete(var A: TArray<T>; const Index: Integer; Count: Integer = 1);
    class procedure Clear(var A: TArray<T>);
  end;

  function InDebug(): Boolean;
  function DebugKey(): Boolean;
  function Capitaliza(Texto: string): string;
  function QuebraTexto(Texto: string; Tamanho: Integer; Prefixos: TArray<string> = []): string;
  function NewGUID(): string;
  function IsGUID(S: string): Boolean;
  function GUIDToValidName(GUID: string): string;
  function GUIDToAlphaNumeric(GUID: string): string;
  function NewTempFile(Ext: string): string;
  procedure AppendLine(var Text: string; Line: string);
  procedure TextEdit(const S: string);
  procedure ShellOpenSite(Site: string);
  procedure ShellOpenFile(const FileName: string);
  procedure AppendLog(S: string);
  procedure AppendLogError(S: string);
  procedure AStringAdd(var Target: AString; Element: string);
  procedure AddInteger(var Total: Integer; const Valor: Integer = 1);
  procedure AddExtended(var Total: Extended; const Valor: Extended);
  procedure AddCurrency(var Total: Currency; const Valor: Currency);
  function Trunca(Valor: Currency; Digitos: Word = 2): Currency;
  function FormataDataHora(Momento: TDateTime): string;
  function FusoHorario(): TDateTime;
  function FormataDataHoraZ(Momento: TDateTime): string;
  function FormataQuantidade(Quantidade: Extended; Unidade: string = ''): string;
  function FormataMoeda(Moeda: Currency): string;
  function FormataValor(Valor: Currency): string;
  function FormataPercentual(Percentual: Extended; Decimais: Byte; Simbolo: Boolean = True): string;
  function FormataInteiro(Valor: Extended): string;
  function VarAsString(V: Variant): string;
  function VarToInteger(V: Variant): Integer;
  function Concorda(Texto: string; Quantidade: Integer): string;
  function PlainText(S: string): string;
  function OnlyDigits(S: string): string;
  function CEPValido(Texto: string; var Resultado: string): Boolean;
  function TelefoneValido(Texto: string; var Resultado: string): Boolean;
  function TelefoneFixo(Texto: string): Boolean;
  function EmailValido(Texto: string; var Resultado: string): Boolean;
  function PlacaValida(Texto: string; var Resultado: string): Boolean;
  function eNumerico(Texto: string): Boolean;
  function DataCompacta(Momento: TDateTime): string;
  function MomentoCompacto(Momento: TDateTime): string;
  function AIntegerJoin(Lista: TArray<Integer>; Separador: string): string;
  procedure ExcecaoErro(S: string); overload;
  procedure ExcecaoErro(S: string; const Arguments: array of const); overload;
  procedure ExcecaoPerdao(S: string); overload;
  procedure ExcecaoPerdao(S: string; const Arguments: array of const); overload;
  function EncodeURLString(S: string): string;
  function URLParams(Params: TArray<string>): string;
  procedure Obliterate(var Obj);
  function IfNull(const Value, Default: variant): variant;

implementation

uses System.Classes, System.IOUtils, System.Variants, System.RegularExpressions,
  System.Net.URLClient, Winapi.Windows, Winapi.ShellApi, Math, Excecoes;

class procedure TAHelper<T>.Add(var A: TArray<T>; Value: T);
begin
  SetLength(A, Length(A) + 1);
  A[High(A)] := Value;
end;

class procedure TAHelper<T>.Delete(var A: TArray<T>; const Index: Integer; Count: Integer = 1);
var
  ALength: Cardinal;
  I: Integer;
begin
  ALength := Length(A);
  Assert(ALength > 0);
  Assert(Count > 0);
  Assert(Count <= ALength - Index);
  Assert(Index < ALength);
  for I := Index + Count to ALength - 1 do
    A[I - Count] := A[I];
  SetLength(A, ALength - Count);
end;

class procedure TAHelper<T>.Clear(var A: TArray<T>);
begin
  // Apagar itens
  A := [];
end;

function DebugKey(): Boolean;
begin
  if GetKeyState(VK_LWIN) < 0 then
    Exit(True);
  Result := False;
end;

procedure Inicializa();
var
  I: Integer;
  S: string;
begin
  S := 'Parâmetros:';
  for I := 0 to ParamCount - 1 do
    AppendLine(S, paramstr(I));
  AppendLog(S);
end;

function InDebug(): Boolean;
begin
  Result := True;
end;

function NewTempFile(Ext: string): string;
begin
  if not Ext.StartsWith('.') then
    Ext := '.' + Ext;
  Result := TPath.GetTempPath() + TPath.GetGUIDFileName() + Ext;
end;

procedure AppendLine(var Text: string; Line: string);
begin
  if Text > '' then
  begin
    Text := Text + LINE_FEED + Line;
    Exit;
  end;
  Text := Line;
end;

procedure TextEdit(const S: string);
var
  FileName: string;
  sl: TStringList;
begin
  FileName := NewTempFile('.txt');
  sl := TStringList.Create();
  try
    sl.Text := S;
    sl.SaveToFile(FileName);
    ShellOpenFile(FileName);
  finally
    sl.Free();
  end;
end;

procedure ShellOpenSite(Site: string);
begin
  ShellExecute(0, 'open', PChar(Site), nil, nil, SW_SHOWMAXIMIZED);
end;

procedure ShellOpenFile(const FileName: string);
var
  Ext: string;
  SEI: TShellExecuteInfo;
begin
  Ext := TPath.GetExtension(FileName);
  ZeroMemory(@sei, SizeOf(sei));
  SEI.cbSize := SizeOf(sei);
  SEI.fMask := SEE_MASK_CLASSNAME;
  SEI.lpFile := PChar(FileName);
  SEI.lpClass := PChar(Ext); //'.txt';
  SEI.nShow := SW_SHOWNORMAL;
  ShellExecuteEx(@SEI);
end;

procedure AddString(var S: string; const Subs: string); overload;
begin
  S := S + Subs;
end;

procedure AddString(var S: string; const Subs, Separator: string); overload;
begin
  if S = '' then
    S := Subs
  else if Subs > '' then
    S := S + Separator + Subs;
end;

function Capitaliza(Texto: string): string;
var
  I: Integer;
  Segmentos: AString;
begin
  if Texto.IsEmpty then
    Exit('');
  if not Texto.Contains('-') then
    Exit(Texto.Substring(0, 1).ToUpper() + Texto.Substring(1).ToLower());
  Segmentos := Texto.Split(['-']);
  for I := 0 to Length(Segmentos) do
    Segmentos[I] := Capitaliza(Segmentos[I]);
  Result := String.Join('-', Segmentos);
end;

function QuebraTexto(Texto: string; Tamanho: Integer; Prefixos: TArray<string> = []): string;
var
  I, L: Integer;
  Conectivo: string;
  Palavra, S: string;
  ATexto: TArray<string>;
begin
  I := 0;
  L := 0;
  Result := '';
  for S in Prefixos do
    if Texto.StartsWith(S.Trim()) then
      Texto := Texto.Substring(S.Trim().Length + 1);
  ATexto := Texto.Trim().Split([' ']);
  for Palavra in ATexto do
  begin
    if (I > 0) and ('a_e_o_da_de_do_com_sem_na_quer_uma_um'.Contains(Palavra.ToLower())) then
      S := Palavra.ToLower()
    else
      S := Capitaliza(Palavra);
    if S.Length > Tamanho then
    begin
      S := WrapText(S, Tamanho);
      L := 0;
      Conectivo := '';
    end else
    if
      (S > '')
      and ((L + S.Length) > Tamanho)
    then
    begin
      AddString(Result, CHAR_LF);
      L := 0;
      Conectivo := '';
    end else
    begin
      Inc(L, S.Length + 1); // Tamanho da palavra + espaço
      Conectivo := ' ';
    end;
    AddString(Result, S, Conectivo);
    Inc(I);
  end;
  Result := Trim(Result);
end;

function NewGUID(): string;
var
  ID: TGUID;
begin
  Result := '';
  if CreateGuid(ID) <> S_OK then
    Excecoes.ObjetoNaoIniciado('GUID');
  Result := GUIDToString(ID);
  Result := Copy(Result, 2, Length(Result) - 2); // without { and }
end;

function IsGUID(S: string): Boolean;
begin
  if S.IsEmpty then
    Exit(False);
  if S.CountChar('-') <> 4 then
    Exit(False);
  if
    not S.IsEmpty
    and not (S.StartsWith('{') and S.EndsWith('}'))
  then
    S := '{' + S + '}';
  try
    StringToGUID(S);
    Exit(True);
  except
  end;
  Result := False;
end;

function GUIDToValidName(GUID: string): string;
var
  I: Integer;
begin
  I := 0;
  while I < GUID.Length do
  begin
    if not (GUID.Substring(I, 1).Chars[0] in ['0'..'9', 'A'..'Z', 'a'..'z']) then
    begin
      Delete(GUID, I + 1, 1);
      Continue;
    end;
    Inc(I);
  end;
  Result := 'OBJ_' + GUID;
end;

function GUIDToAlphaNumeric(GUID: string): string;
var
  I: Integer;
begin
  I := 0;
  while I < GUID.Length do
  begin
    if not (GUID.Substring(I, 1).Chars[0] in ['0'..'9', 'A'..'Z', 'a'..'z']) then
    begin
      Delete(GUID, I + 1, 1);
      Continue;
    end;
    Inc(I);
  end;
  Result := GUID;
end;

procedure AppendLog(S: string);
var
  FileName: string;
  Novo: Boolean;
  T: TextFile;
begin
  FileName := ExtractFilePath(ParamStr(0)) + '\' + APP_NAME + '.log';
  Novo := not FileExists(FileName);
  AssignFile(T, FileName);
  try
    if Novo then
      ReWrite(T)
    else
    begin
      Append(T);
      if FileSize(T) > 10000 then // Reseta o arquivo
      begin
        CloseFile(T);
        AssignFile(T, FileName);
        ReWrite(T);
        Append(T);
      end;
    end;
    S := S.Replace(LINE_FEED, ';').Replace(CHAR_LF, ' ');
    WriteLn(T, DateTimeToStr(Now()) + ': ' + S);
  finally
    CloseFile(T);
  end;
end;

procedure AppendLogError(S: string);
begin
  AppendLog('** Erro: ' + S);
end;

procedure AStringAdd(var Target: AString; Element: string);
begin
  Target := Target + [Element];
end;

procedure AddInteger(var Total: Integer; const Valor: Integer = 1);
begin
  Total := Total + Valor;
end;

procedure AddExtended(var Total: Extended; const Valor: Extended);
begin
  Total := Total + Valor;
end;

procedure AddCurrency(var Total: Currency; const Valor: Currency);
begin
  Total := Total + Valor;
end;

function Trunca(Valor: Currency; Digitos: Word = 2): Currency;
var
  Fator: Currency;
begin
  Fator := Power(10, Digitos);
  Result := Trunc(Valor * Fator) / Fator;
end;

function FormataDataHora(Momento: TDateTime): string;
begin
  Result := FormatDateTime('dd/mm/yyyy hh:nn:ss', Momento);
end;

function FusoHorario(): TDateTime;
begin
  Result := EncodeTime(3, 0, 0, 0);
end;

function FormataDataHoraZ(Momento: TDateTime): string;
begin
  Result := FormatDateTime('dd/mm/yyyy hh:nn:ss', Momento - FusoHorario());
end;

function FormataQuantidade(Quantidade: Extended; Unidade: string = ''): string;
begin
  if Unidade = '' then
    Exit(FloatToStr(Quantidade));
  Result := FloatToStr(Quantidade) + Unidade;
end;

function FormataMoeda(Moeda: Currency): string;
begin
  Result := FormatFloat(',0.00', Moeda);
end;

function FormataValor(Valor: Currency): string;
begin
  Result := FormatFloat(',0.00', Valor);
end;

function FormataPercentual(Percentual: Extended; Decimais: Byte; Simbolo: Boolean = True): string;
var
  Formato: string;
begin
  if Decimais <= 0 then
    Formato := ',0'
  else
    Formato := ',0.' + string.Create('0', Decimais);
  Result := FormatFloat(Formato, Percentual);
  if Simbolo then
    AddString(Result, '%');
end;

function FormataInteiro(Valor: Extended): string;
begin
  Result := FormatFloat(',0', Trunc(Valor));
end;

function VarAsString(V: Variant): string;
begin
  Result := VarToStr(V);
end;

function VarToInteger(V: Variant): Integer;
begin
  if VarIsNull(V) then
    Exit(0);
  if VarIsEmpty(V) then
    Exit(0);
  Result := V;
end;

function Concorda(Texto: string; Quantidade: Integer): string;
var
  I: Integer;
  Segmentos: AString;
begin
  //ite+m*ns
  Segmentos := Texto.Split(['*']);
  if Length(Segmentos) = 0 then
    Exit('?');
  Result := Segmentos[0];
  if Quantidade = 1 then
    Result := Result.Replace('+', '')
  else if Length(Segmentos) > 1 then
  begin
    I := Result.IndexOf('+');
    if I > 0 then
      Result := Result.Substring(0, I);
    Result := Result + Segmentos[1];
  end;
  if Result.Contains('%c') then
    Result := Result.Replace('%c', FormataInteiro(Quantidade));
end;

function PlainText(S: string): string;
type
  USASCIIString = type AnsiString(20127);//20127 = us ascii
begin
  Result := string(USASCIIString(S));
end;

function OnlyDigits(S: string): string;
begin
  SetLength(Result, S.Length);
  var I := 0;
  for var C in S do
    if C.IsDigit then
    begin
      Inc(I);
      Result[I] := C;
    end;
  SetLength(Result, I);
end;

function CEPValido(Texto: string; var Resultado: string): Boolean;
const
  EXEMPLOS = CHAR_LF2 + ' Exemplos de CEP''s válidos: 60125-101, 87654321, 87654.';
var
  C: Char;
begin
  Texto := Texto.Trim();
  for C in Texto do
    if not (C.IsDigit or (C in ['.', '-'])) then
    begin
      Resultado := 'CEP inválido.  Apenas dígitos e os caracteres .- são permitidos.' + EXEMPLOS;
      Exit(False);
    end;
  Texto := OnlyDigits(Texto);
  if Texto.Length = 0 then
    Exit(False);
  if Texto.Length = 5 then
    Texto := Texto + '000';
  case Texto.Length of
    8: Resultado := Texto.Substring(0, 2) + '.' + Texto.Substring(2, 3) + '-' + Texto.Substring(5, 3);
  else
    Resultado := 'CEP inválido.' + EXEMPLOS;
    Exit(False);
  end;
  Result := True;
end;

function TelefoneValido(Texto: string; var Resultado: string): Boolean;
const
  EXEMPLOS = CHAR_LF2 + ' Exemplos de números válidos: (85) 3307-6262, (11) 98765-4321, (53) 2345-6789, 21987654321, 6123456789.';
var
  C: Char;
begin
  for C in Texto do
    if not (C.IsDigit or (C in ['(', ')', ' ', '-'])) then
    begin
      Resultado := 'Número inválido.  Apenas dígitos, espaços e os caracteres ()- são permitidos.' + EXEMPLOS;
      Exit(False);
    end;
  Texto := OnlyDigits(Texto.Trim());
  if Texto.StartsWith('0') then // 085
    Delete(Texto, 1, 1);
  if Texto.Length = 0 then
    Exit(False);
  if Texto.Length in [8, 9] then
    Texto := '85' + Texto;
  if not (Texto.Substring(0, 1).ToCharArray()[0] in ['1'..'9']) then
  begin
    Resultado := 'DDD inválido.' + EXEMPLOS;
    Exit(False);
  end;
  case Texto.Length of
    10: Resultado := '(' + Texto.Substring(0, 2) + ') ' + Texto.Substring(2, 4) + '-' + Texto.Substring(6, 4);
    11: Resultado := '(' + Texto.Substring(0, 2) + ') ' + Texto.Substring(2, 5) + '-' + Texto.Substring(7, 4);
  else
    Resultado := 'Número inválido.' + EXEMPLOS;
    Exit(False);
  end;
  Result := True;
end;

function TelefoneFixo(Texto: string): Boolean;
begin
  Result := OnlyDigits(Texto).Length = 10;
end;

function EmailValido(Texto: string; var Resultado: string): Boolean;
begin
  Texto := Texto.Trim().ToLower();
  if Texto.Length = 0 then
    Exit(False);
  if not TRegEx.IsMatch(Texto, '[a-zA-Z0-9._-]{3,}@[a-zA-Z0-9.-]{3,}\.[a-zA-Z]{2,4}') then
  begin
    Resultado := 'e-mail inválido';
    Exit(False);
  end;
  Resultado := Texto;
  Result := True;
end;

function PlacaValida(Texto: string; var Resultado: string): Boolean;
var
  Regex: string;
begin
  Texto := Texto.Trim().ToUpper();
  if Texto.Length = 0 then
    Exit(False);
  if Texto.Length > 8 then
  begin
    Resultado := 'placa inválida';
    Exit(False);
  end;
  if Texto.Substring(3, 1) <> '-' then // Placa nova
    Regex := '[A-Z]{3}[0-9][0-9A-Z][0-9]{2}'
  else
    Regex := '[A-Z]{3}\-[0-9]{4}';
  if not TRegEx.IsMatch(Texto, Regex) then
  begin
    Resultado := 'placa inválida';
    Exit(False);
  end;
  Resultado := Texto;
  Result := True;
end;

function eNumerico(Texto: string): Boolean;
begin
  Result := TRegEx.IsMatch(Texto, '^\d+$');
end;

function DataCompacta(Momento: TDateTime): string;
begin
  if Abs(Momento - Now()) < 366 then
    Exit(FormatDateTime('dd/mmm', Momento));
  Result := FormatDateTime('dd/mmm/yyyy hh:nn', Momento);
end;

function MomentoCompacto(Momento: TDateTime): string;
begin
  if Abs(Momento - Now()) < 366 then
    Exit(FormatDateTime('dd/mm hh:nn', Momento));
  Result := FormatDateTime('dd/mm/yyyy hh:nn', Momento);
end;

function AIntegerJoin(Lista: TArray<Integer>; Separador: string): string;
var
  Conectivo: string;
  Elemento: Integer;
begin
  if Length(Lista) = 0 then
    Exit('');
  Conectivo := '';
  for Elemento in Lista do
  begin
    Result := Result + Conectivo + Elemento.ToString();
    Conectivo := Separador;
  end;
end;

//function TelefoneFormata(S: string; Valida: Boolean = False): string;
//var
//  Resultado: string;
//begin
//  if not TelefoneValida(S, Resultado) then
//  begin
//    if Valida then
//      raise Exception.Create(S);
//    Exit(S);
//  end;
//end;

procedure Obliterate(var Obj);
{$IF not Defined(AUTOREFCOUNT)}
var
  Temp: TObject;
begin
  if TObject(Obj) = nil then
    Exit;
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free();
end;
{$ELSE}
begin
  if TObject(Obj) = nil then
    Exit;
  TObject(Obj) := nil;
end;
{$ENDIF}

function GUIDToName(GUID: string): string;
begin
  Result := 'NM' + GUID.Replace('-', '_');
end;

function GUIDFromName(Name: string): string;
begin
  Result := Name.Substring(2).Replace('_', '-');
end;

procedure ExcecaoErro(S: string); overload;
begin
  raise Exception.Create(S);
end;

procedure ExcecaoErro(S: string; const Arguments: array of const); overload;
begin
  raise Exception.CreateFmt(S, Arguments);
end;

procedure ExcecaoPerdao(S: string);
begin
  raise Exception.Create('Perdão, ' + S);
end;

procedure ExcecaoPerdao(S: string; const Arguments: array of const); overload;
begin
  raise Exception.CreateFmt('Perdão, ' + S, Arguments);
end;

function EncodeURLString(S: string): string;
begin
  Result := System.Net.URLClient.TURI.URLEncode(S);
end;

function URLParams(Params: TArray<string>): string;
begin
  if Length(Params) = 0 then
    Exit('');
  Result := '?' + string.Join('&', Params);
end;

function IfNull(const Value, Default: variant): variant;
begin
  if Value = Null then
    result := Default
  else
    result := Value;
end;

initialization
  Inicializa();

end.
ChangeFileExt( Application.ExeName, '.INI' )
program Project1;
{$APPTYPE CONSOLE}

uses
  System.Collections;

var
  myArray    : System.Array;
  enumerator : System.Collections.IEnumerator;

begin
  // Create a 4 element single dimension array of strings
  myArray := System.Array.CreateInstance(TypeOf(String), 4);

  // Fill the array with values
  myArray.SetValue('Hello',  0);
  myArray.SetValue('from',   1);
  myArray.SetValue('Delphi', 2);
  myArray.SetValue('Basics', 3);

  // Display the array contents using an enumerator
  enumerator := myArray.GetEnumerator;

  // We must move to the start before displaying the first value
  while Enumerator.MoveNext do
    Console.WriteLine(Enumerator.Current.ToString);

  Console.ReadLine;
end.
