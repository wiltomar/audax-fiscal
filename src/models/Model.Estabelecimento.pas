unit Model.Estabelecimento;

interface

uses
  Model.Base, Model.UF, Model.Municipio, Model.CNAE;

const
  ESTABELECIMENTO_SITUACAO_ATIVO = 1;
  ESTABELECIMENTO_SITUACAO_SUSPENSO = 410;
  ESTABELECIMENTO_SITUACAO_DESATIVADO = 510;

type
  TEstabelecimento = class(TBaseTR)
  private
    fsituacao: SmallInt;
  public
    property situacao: SmallInt read fsituacao write fsituacao;
  end;
  TEstabelecimentos = TArray<TEstabelecimento>;

type
  TEstabelecimentoCDocumento = class(TBaseR)
  private
    fdocumentotipo: SmallInt;
    fdocumentoNumero: String;
    finscricaoEstadual: String;
    finscricaoMunicipal: String;
    finscricaoEstadualSubstitutoTributario: String;
    finscricaoSuframa: String;
    fregimeTributarioICMS: SmallInt;
    fregimeTributarioISSQN: SmallInt;
    findicadorDeRateioISSQN: SmallInt;
    fcnae: TCnae;
    fspedperfil: String;
    fambienteEmissaoNFe: SmallInt;
    fambienteEmissaoNFCe: SmallInt;
    fambienteEmissaoCFe: SmallInt;
    fambienteEmissaoNFSe: SmallInt;
    fambienteEmissaoCTe: SmallInt;
    fambienteEmissaoMDFe: SmallInt;
    fatributos: TArray<string>;
    finicio: TDateTime;
    fconclusao: TDateTime;
  public
    property documentoTipo: SmallInt read fdocumentoTipo write fdocumentoTipo;
    property documentoNumero: String read fdocumentoNumero write fdocumentoNumero;
    property inscricaoEstadual: String read finscricaoEstadual write finscricaoEstadual;
    property inscricaoMunicipal: String read finscricaoMunicipal write finscricaoMunicipal;
    property inscricaoEstadualSubstitutoTributario: String read finscricaoEstadualSubstitutoTributario write finscricaoEstadualSubstitutoTributario;
    property inscricaoSuframa: String read finscricaoSuframa write finscricaoSuframa;
    property regimeTributarioICMS: SmallInt read fregimeTributarioICMS write fregimeTributarioICMS;
    property regimeTributarioISSQN: SmallInt read fregimeTributarioISSQN write fregimeTributarioISSQN;
    property indicadorDeRateioISSQN: SmallInt read findicadorDeRateioISSQN write findicadorDeRateioISSQN;
    property cnae: TCnae read fcnae write fcnae;
    property spedperfil: String read fspedperfil write fspedperfil;
    property ambienteEmissaoNFe: SmallInt read fambienteEmissaoNFe write fambienteEmissaoNFe;
    property ambienteEmissaoNFCe: SmallInt read fambienteEmissaoNFCe write fambienteEmissaoNFCe;
    property ambienteEmissaoCFe: SmallInt read fambienteEmissaoCFe write fambienteEmissaoCFe;
    property ambienteEmissaoNFSe: SmallInt read fambienteEmissaoNFSe write fambienteEmissaoNFSe;
    property ambienteEmissaoCTe: SmallInt read fambienteEmissaoCTe write fambienteEmissaoCTe;
    property ambienteEmissaoMDFe: SmallInt read fambienteEmissaoMDFe write fambienteEmissaoMDFe;
  end;
  TEstabelecimentoCDocumentos = TArray<TEstabelecimentoCDocumento>;

  TEstabelecimentoCEndereco = class(TBaseR)
  private
    fnome: string;
    ffavorito: boolean;
    fentrega: boolean;
    fcorrespondencia: boolean;
    ffaturamento: boolean;
    fretirada: boolean;
    fcep: string;
    flogradouro: string;
    fnumero: integer;
    fcomplemento: string;
    fbairro: string;
    fuf: TUf;
    fmunicipio: TMunicipio;
    freferencia: string;
  public
    property nome: string read fnome write fnome;
    property favorito: boolean read ffavorito write ffavorito;
    property entrega: boolean read fentrega write fentrega;
    property correspondencia: boolean read fcorrespondencia write fcorrespondencia;
    property faturamento: boolean read ffaturamento write ffaturamento;
    property retirada: boolean read fretirada write fretirada;
    property cep: string read fcep write fcep;
    property logradouro: string read flogradouro write flogradouro;
    property numero: integer read fnumero write fnumero;
    property complemento: string read fcomplemento write fcomplemento;
    property bairro: string read fbairro write fbairro;
    property uf: TUF read fuf write fuf;
    property municipio: TMunicipio read fmunicipio write fmunicipio;
    property referencia: string read freferencia write freferencia;
  end;
  TEstabelecimentoCEnderecos = TArray<TEstabelecimentoCEndereco>;

type
  TEstabelecimentoCAmbiente = class(TBaseR)
  private
    fsituacao: SmallInt;
    fpadrao: Boolean;
  public
    property situacao: SmallInt read fsituacao write fsituacao;
    property padrao: Boolean read fpadrao write fpadrao;
  end;
  TEstabelecimentoCAmbientes = TArray<TEstabelecimentoCAmbiente>;

type
  TEstabelecimentoCDepartamento = class(TBaseTR)
  private
    fsituacao: SmallInt;
    fpadrao: Boolean;
    festoque: Boolean;
  public
    property situacao: SmallInt read fsituacao write fsituacao;
    property padrao: Boolean read fpadrao write fpadrao;
    property estoque: Boolean read festoque write festoque;
  end;
  TEstabelecimentoCDepartamentos = TArray<TEstabelecimentoCDepartamento>;

type
  TEstabelecimentoC = class(TBaseR)
  private
    fcodigo: string;
    fnome: string;
    fsituacao: SmallInt;
    festabelecimentoAmbientes: TEstabelecimentoCAmbientes;
    festabelecimentoDepartamentos: TEstabelecimentoCDepartamentos;
    festabelecimentoDocumentos: TEstabelecimentoCDocumentos;
    festabelecimentoEnderecos: TEstabelecimentoCEnderecos;
  public
    constructor Create(aid: string; anome: string);
    destructor Destroy(); override;
    property codigo: string read fcodigo write fcodigo;
    property nome: string read fnome write fnome;
    property situacao: SmallInt read fsituacao write fsituacao;
    property estabelecimentoAmbientes: TEstabelecimentoCAmbientes read festabelecimentoAmbientes write festabelecimentoAmbientes;
    property estabelecimentoDepartamentos: TEstabelecimentoCDepartamentos read festabelecimentoDepartamentos write festabelecimentoDepartamentos;
    property estabelecimentoDocumentos: TEstabelecimentoCDocumentos read festabelecimentoDocumentos write festabelecimentoDocumentos;
    property estabelecimentoEnderecos: TEstabelecimentoCEnderecos read festabelecimentoEnderecos write festabelecimentoEnderecos;
  end;

implementation

uses
  System.SysUtils, Lib.Funcoes;

constructor TEstabelecimentoC.Create(aid: string; anome: string);
begin
  inherited Create(aid);
  nome := anome;
  festabelecimentoAmbientes := [];
  festabelecimentoDepartamentos := [];
  festabelecimentoDocumentos := [];
end;

destructor TEstabelecimentoC.Destroy();
var
  estabelecimentoAmbiente: TEstabelecimentoCAmbiente;
  estabelecimentoDepartamento: TEstabelecimentoCDepartamento;
  estabelecimentoDocumento: TEstabelecimentoCDocumento;
begin
  for estabelecimentoAmbiente in festabelecimentoAmbientes do
    estabelecimentoAmbiente.Free();
  estabelecimentoAmbientes := [];
  for estabelecimentoDepartamento in festabelecimentoDepartamentos do
    estabelecimentoDepartamento.Free();
  estabelecimentoDepartamentos := [];
  for estabelecimentoDocumento in festabelecimentoDocumentos do
    estabelecimentoDocumento.Free();
  estabelecimentoDocumentos := [];
end;

end.

