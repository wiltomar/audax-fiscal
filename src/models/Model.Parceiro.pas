unit Model.Parceiro;

interface

uses
  Lib.Sistema.Tipos, Model.Base, Model.UF, Model.Municipio;

type
  TParceiro = class(TBaseTR)
  private
    fimagem: string;
    fsituacao: SmallInt;
    festrela: Boolean;
    fpadrao: Boolean;
    fclasse: SmallInt;
    function getIniciais(): string;
  public
    constructor Create(Referencia: TParceiro = nil); overload;
    class function FromJsonString(AJsonString: string): TParceiro;
    class procedure Clear(var Parceiros: TArray<TParceiro>);
    procedure Atribui(Parceiro: TParceiro);
    property imagem: string read fimagem write fimagem;
    property iniciais: string read getIniciais;
    property situacao: SmallInt read fsituacao write fsituacao;
    property estrela: Boolean read festrela write festrela;
    property padrao: Boolean read fpadrao write fpadrao;
    property classe: SmallInt read fclasse write fclasse;
  end;
  TParceiros = TArray<TParceiro>;
type
  TCliente = TParceiro;
  TVendedor = TParceiro;
  TVendedores = TArray<TVendedor>;
  TEntregador = TParceiro;
  TEntregadores = TArray<TEntregador>;
type
  TParceiroCTipo =
  (
    Cliente = 1,
    Fornecedor = 2,
    Colaborador = 4,
    Vendedor = 8
  );
  TParceiroCTipos = set of TParceiroCTipo;
type
  TParceiroCSituacao =
  (
    Ativo = 1,
    Suspenso = 410,
    Desativado = 510
  );
type
  TParceiroCTelefonesMobilidade =
  (
    Fixo = 1,
    Movel = 3
  );
type
  TParceiroCDocumentoTipo =
  (
    CNPJ = 1,
    CPF = 2,
    ProdutorRural = 3,
    SeguradoEspecial = 4,
    DocumentoDeEstrangeiro = 9
  );
const
  TParceiroCTelefonesMobilidadeNomes: array[0..1] of TComboItem =
  (
    (id: Ord(TParceiroCTelefonesMobilidade.Fixo); nome: 'Fixo'),
    (id: Ord(TParceiroCTelefonesMobilidade.Movel); nome: 'Móvel')
  );
type
  TParceiroCDocumento = class(TBaseR)
  private
    fdocumentotipo: SmallInt;
    fdocumentoNumero: AnsiString;
    finscricaoEstadual: AnsiString;
    finscricaoMunicipal: AnsiString;
    fregimeTributarioICMS: SmallInt;
    fregimeTributarioISSQN: SmallInt;
    finicio: TDateTime;
    fconclusao: TDateTime;
  public
    property documentoTipo: SmallInt read fdocumentoTipo write fdocumentoTipo;
    property documentoNumero: AnsiString read fdocumentoNumero write fdocumentoNumero;
    property inscricaoEstadual: AnsiString read finscricaoEstadual write finscricaoEstadual;
    property inscricaoMunicipal: AnsiString read finscricaoMunicipal write finscricaoMunicipal;
    property regimeTributarioICMS: SmallInt read fregimeTributarioICMS write fregimeTributarioICMS;
    property regimeTributarioISSQN: SmallInt read fregimeTributarioISSQN write fregimeTributarioISSQN;
    property inicio: TDateTime read finicio write finicio;
    property conclusao: TDateTime read fconclusao write fconclusao;
  end;
  TParceiroCDocumentos = TArray<TParceiroCDocumento>;
type
  TParceiroCEndereco = class(TBaseR)
  private
    fnome: string;
    fpessoal: Boolean;
    fempresarial: Boolean;
    fentrega: Boolean;
    fcorrespondencia: Boolean;
    fcep: string;
    flogradouro: string;
    fnumero: Integer;
    fcomplemento: string;
    fbairro: string;
    fuf: TUF;
    fmunicipio: TMunicipio;
    freferencia: string;
  public
    constructor Create();
    destructor Destroy(); override;
    property nome: string read fnome write fnome;
    property pessoal: Boolean read fpessoal write fpessoal;
    property empresarial: Boolean read fempresarial write fempresarial;
    property entrega: Boolean read fentrega write fentrega;
    property correspondencia: Boolean read fcorrespondencia write fcorrespondencia;
    property cep: string read fcep write fcep;
    property logradouro: string read flogradouro write flogradouro;
    property numero: Integer read fnumero write fnumero;
    property complemento: string read fcomplemento write fcomplemento;
    property bairro: string read fbairro write fbairro;
    property uf: TUF read fuf write fuf;
    property municipio: TMunicipio read fmunicipio write fmunicipio;
    property referencia: string read freferencia write freferencia;
  end;
  TParceiroCEnderecos = TArray<TParceiroCEndereco>;
type
  TParceiroCTelefone = class(TBaseR)
  private
    fnome: string;
    fpessoal: Boolean;
    fempresarial: Boolean;
    fmobilidade: SmallInt;
    fnumero: string;
    framal: string;
  public
    property nome: string read fnome write fnome;
    property pessoal: Boolean read fpessoal write fpessoal;
    property empresarial: Boolean read fempresarial write fempresarial;
    property mobilidade: SmallInt read fmobilidade write fmobilidade;
    property numero: string read fnumero write fnumero;
    property ramal: string read framal write framal;
  end;
  TParceiroCTelefones = TArray<TParceiroCTelefone>;
type
  TParceiroCEmail = class(TBaseR)
  private
    fnome: string;
    fpessoal: Boolean;
    fempresarial: Boolean;
    fendereco: string;
  public
    property nome: string read fnome write fnome;
    property pessoal: Boolean read fpessoal write fpessoal;
    property empresarial: Boolean read fempresarial write fempresarial;
    property endereco: string read fendereco write fendereco;
  end;
  TParceiroCEmails = TArray<TParceiroCEmail>;
type
  TParceiroC = class(TBaseR)
  private
    fcodigo: string;
    fnome: string;
    fimagem: string;
    fsituacao: SmallInt;
    fatributos: TArray<string>;
    festrela: Boolean;
    fpadrao: Boolean;
    fcliente: Boolean;
    ffornecedor: Boolean;
    fcolaborador: Boolean;
    fvendedor: Boolean;
    fcredito: Currency;
    fsenha: string;
    fparceiroDocumentos: TParceiroCDocumentos;
    fparceiroEnderecos: TParceiroCEnderecos;
    fparceiroTelefones: TParceiroCTelefones;
    fparceiroEmails: TParceiroCEmails;
  public
    constructor Create(aid: string = ''; anome: string = '');
    destructor Destroy(); override;
    procedure Atribui(ParceiroC: TParceiroC);
    class function classeTexto(classe: SmallInt): string;
    property codigo: string read fcodigo write fcodigo;
    property nome: string read fnome write fnome;
    property imagem: string read fimagem write fimagem;
    property situacao: SmallInt read fsituacao write fsituacao;
    property atributos: TArray<string> read fatributos write fatributos;
    property estrela: Boolean read festrela write festrela;
    property padrao: Boolean read fpadrao write fpadrao;
    property cliente: Boolean read fcliente write fcliente;
    property fornecedor: Boolean read ffornecedor write ffornecedor;
    property colaborador: Boolean read fcolaborador write fcolaborador;
    property vendedor: Boolean read fvendedor write fvendedor;
    property credito: Currency read fcredito write fcredito;
    property senha: string read fsenha write fsenha;
    property parceiroDocumentos: TParceiroCDocumentos read fparceiroDocumentos write fparceiroDocumentos;
    procedure parceiroCDocumentosAdd(parceiroCDocumento: TParceiroCDocumento);
    property parceiroEnderecos: TParceiroCEnderecos read fparceiroEnderecos write fparceiroEnderecos;
    procedure parceiroCEnderecosAdd(parceiroCEndereco: TParceiroCEndereco);
    property parceiroTelefones: TParceiroCTelefones read fparceiroTelefones write fparceiroTelefones;
    procedure parceiroCTelefonesAdd(parceiroCTelefone: TParceiroCTelefone);
    property parceiroEmails: TParceiroCEmails read fparceiroEmails write fparceiroEmails;
    procedure parceiroCEmailsAdd(parceiroCEmail: TParceiroCEmail);
  end;

implementation

uses System.SysUtils, Rest.JSON, Lib.Funcoes;

constructor TParceiro.Create(Referencia: TParceiro = nil);
begin
  if not Assigned(Referencia) then
  begin
    inherited Create();
    Exit;
  end;
  inherited Create(Referencia);
  fpadrao := Referencia.padrao;
end;

class procedure TParceiro.Clear(var Parceiros: TArray<TParceiro>);
var
  Parceiro: TParceiro;
begin
  for Parceiro in Parceiros do
    Parceiro.Free();
  Parceiros := [];
end;

class function TParceiro.FromJsonString(AJsonString: string): TParceiro;
begin
  Result := TJson.JsonToObject<TParceiro>(AJsonString);
end;

procedure TParceiro.Atribui(Parceiro: TParceiro);
begin
  id := Parceiro.id;
  codigo := Parceiro.codigo;
  nome := Parceiro.nome;
  padrao := Parceiro.padrao;
  estrela := Parceiro.estrela;
end;

function TParceiro.getIniciais(): string;
var
  I: Integer;
  S: string;
  palavras: AString;
begin
  if nome.IsEmpty then
    Exit('');
  Result := '';
  S := Lib.Funcoes.PlainText(nome).ToUpper();
  for I := 0 to S.Length - 1 do
    if S.Substring(I, 1).ToCharArray[0] in ['A'..'Z', '0'..'9', ' '] then
      Result := Result + S.Substring(I, 1);
  if Result.IsEmpty then
    Result := nome;
  palavras := Result.Split([' ']);
  if Length(palavras) = 1 then
    Result := Result.Substring(0, 1) + Result.Substring(Result.length - 1, 1)
  else if Length(palavras) > 0 then
    Result := palavras[0].Substring(0, 1) + palavras[High(palavras)].Substring(0, 1);
  if Result.ToUpper().Equals('CU') then // Verificar as palavaras obscenas
    Result := nome.substring(0, 2);
end;

constructor TParceiroC.Create(aid: string = ''; anome: string = '');
begin
  inherited Create(aid);
  fnome := anome;
  fsituacao := Ord(TParceiroCSituacao.Ativo);
  fatributos := [];
  festrela := False;
  fpadrao := False;
  fcliente := True;
  ffornecedor := False;
  fcolaborador := False;
  fvendedor := False;
  fcredito := 0.00;
  fsenha := '';
  fparceiroDocumentos := [];
  fparceiroEnderecos := [];
  fparceiroTelefones := [];
  fparceiroEmails := [];
end;

destructor TParceiroC.Destroy();
var
  parceiroDocumento: TParceiroCDocumento;
  parceiroEndereco: TParceiroCEndereco;
  parceiroTelefone: TParceiroCTelefone;
  parceiroEmail: TParceiroCEmail;
begin
  for parceiroDocumento in fparceiroDocumentos do
    parceiroDocumento.Free();
  fparceiroDocumentos := [];
  for parceiroEndereco in fparceiroEnderecos do
    parceiroEndereco.Free();
  fparceiroEnderecos := [];
  for parceiroTelefone in fparceiroTelefones do
    parceiroTelefone.Free();
  fparceiroTelefones := [];
  for parceiroEmail in fparceiroEmails do
    parceiroEmail.Free();
  fparceiroEmails := [];
end;

procedure TParceiroC.Atribui(ParceiroC: TParceiroC);
begin
  id := ParceiroC.id;
  codigo := ParceiroC.codigo;
  nome := ParceiroC.nome;
  padrao := ParceiroC.padrao;
  estrela := ParceiroC.estrela;
end;

class function TParceiroC.classeTexto(classe: SmallInt): string;
begin
  Result := Char(Ord('A') + (Classe - 1));
end;

procedure TParceiroC.parceiroCEnderecosAdd(parceiroCEndereco: TParceiroCEndereco);
begin
  fparceiroEnderecos := fparceiroEnderecos + [parceiroCEndereco];
end;

procedure TParceiroC.parceiroCTelefonesAdd(parceiroCTelefone: TParceiroCTelefone);
begin
  fparceiroTelefones := fparceiroTelefones + [parceiroCTelefone];
end;

procedure TParceiroC.parceiroCDocumentosAdd(
  parceiroCDocumento: TParceiroCDocumento);
begin
  fparceiroDocumentos := fparceiroDocumentos + [parceiroCDocumento];
end;

procedure TParceiroC.parceiroCEmailsAdd(parceiroCEmail: TParceiroCEmail);
begin
  fparceiroEmails := fparceiroEmails + [parceiroCEmail];
end;

constructor TParceiroCEndereco.Create();
begin
  fuf := nil;
  fmunicipio := nil;
end;

destructor TParceiroCEndereco.Destroy();
begin
  Obliterate(fuf);
  Obliterate(fmunicipio);
end;

end.
