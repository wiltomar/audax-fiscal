unit Model.Base;

interface
uses Classes;
type
  TBase = class
  private
    fid: string;
    finclusao: TDateTime;
    fedicao: TDateTime;
    fexclusao: TDateTime;
    Fversao: SmallInt;
  public
    constructor Create(aid: string = '');
    function ToJsonString(Format: Boolean = False): string;
    property id: string read fid write fid;
    property inclusao: TDateTime read finclusao write finclusao;
    property edicao: TDateTime read fedicao write fedicao;
    property exclusao: TDateTime read fexclusao write fexclusao;
    property versao: SmallInt read fversao write fversao;
  end;
type
  TBaseR = class(TPersistent)
  public
    fid: string;
  public
    constructor Create(); overload;
    constructor Create(aid: string); overload;
    function Clone<T: class>(Source: T): T;
    function ToJsonString(): string;
    property id: string read fid write fid;
  end;
type
  TBaseX = class(TPersistent)
  public
    function Clone<T: class>(Source: T): T;
    function ToJsonString(): string;
  end;
type
  TBaseTR = class(TPersistent)
  private
    fid: string;
    fcodigo: string;
    fnome: string;
  public
    constructor Create(); overload;
    constructor Create(Referencia: TBaseTR); overload;
    constructor Create(aid: string; anome: string); overload;
    function Clone<T: class>(Source: T): T;
//    class function FromJsonString<T: TBaseR>(AJsonString: string): T;
    function ToJsonString(): string;
    property id: string read fid write fid;
    property codigo: string read fcodigo write fcodigo;
    property nome: string read fnome write fnome;
  end;

implementation
uses System.SysUtils, System.RTTI, System.TypInfo, System.JSON, Rest.JSON,
  lib.excecoes;
constructor TBase.Create(aid: string = '');
begin
  inherited Create();
  id := aid;
end;
function TBase.ToJsonString(Format: Boolean = False): string;
begin
  if Format then
    Exit(TJSON.ObjectToJsonObject(Self).Format());
  Result := TJSON.ObjectToJsonString(Self);
end;
constructor TBaseR.Create();
begin
  inherited Create();
end;
constructor TBaseR.Create(aid: string);
begin
  inherited Create();
  id := aid;
end;
function TBaseR.Clone<T>(Source: T): T;
begin
  Result := T(GetTypeData(PTypeInfo(TypeInfo(T)))^.ClassType.Create());
  if Result is TPersistent then
    TPersistent(Result).Assign(Self);
end;
//class function TBaseR.FromJsonString<T>(AJsonString: string): T;
//begin
//  Result := TJSON.JsonToObject<T>(AJsonString);
////  Result := T(GetTypeData(PTypeInfo(TypeInfo(T)))^.ClassType.Create());
////  JsonValue := TJSonObject.ParseJSONValue(AJsonString);
////  if not (JsonValue is TJsonObject) then
////    Excecoes.Erro('o JSON fornecido não é um objeto válido');
////  TJSON.JsonToObject(Result, TJSONObject(JsonValue));
////  id := JsonValue.GetValue<string>('id');
////  Result.id := id;//TJSONObject(JsonValue).GetValue('id').ToString();
//end;
function TBaseR.ToJsonString(): string;
begin
  Result := TJSON.ObjectToJsonString(Self);
end;
{ TBaseX }
function TBaseX.Clone<T>(Source: T): T;
begin
  Result := T(GetTypeData(PTypeInfo(TypeInfo(T)))^.ClassType.Create());
  if Result is TPersistent then
    TPersistent(Result).Assign(Self);
end;
function TBaseX.ToJsonString(): string;
begin
  Result := TJSON.ObjectToJsonString(Self);
end;
{ TBaseTR }
function TBaseTR.Clone<T>(Source: T): T;
begin
//
end;
constructor TBaseTR.Create();
begin
  fid := '';
  fnome := '';
end;
constructor TBaseTR.Create(Referencia: TBaseTR);
begin
  if not Assigned(Referencia) then
    lib.excecoes.ParametroInvalido(Self.ClassName + ': referência');
  fid := Referencia.id;
  fcodigo := Referencia.codigo;
  fnome := Referencia.nome;
end;
constructor TBaseTR.Create(aid: string; anome: string);
begin
  fid := aid;
  fnome := anome;
end;
function TBaseTR.ToJsonString(): string;
begin
  Result := TJSON.ObjectToJsonString(Self);
end;
end.
