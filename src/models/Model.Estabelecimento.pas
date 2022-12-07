unit Model.Estabelecimento;

interface

uses
  Model.Base;

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
    fdocumentoNumero: AnsiString;
    finscricaoEstadual: AnsiString;
    finscricaoMunicipal: AnsiString;
    finscricaoEstadualSubstitutoTributario: AnsiString;
    fregimeTributarioICMS: SmallInt;
    fregimeTributarioISSQN: SmallInt;
    findicadorDeRateioISSQN: SmallInt;
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
    property documentoNumero: AnsiString read fdocumentoNumero write fdocumentoNumero;
    property inscricaoEstadual: AnsiString read finscricaoEstadual write finscricaoEstadual;
    property inscricaoMunicipal: AnsiString read finscricaoMunicipal write finscricaoMunicipal;
    property inscricaoMunicipalSubstitutoTributario: AnsiString read finscricaoEstadualSubstitutoTributario write finscricaoEstadualSubstitutoTributario;
    property regimeTributarioICMS: SmallInt read fregimeTributarioICMS write fregimeTributarioICMS;
    property regimeTributarioISSQN: SmallInt read fregimeTributarioISSQN write fregimeTributarioISSQN;
    property indicadorDeRateioISSQN: SmallInt read findicadorDeRateioISSQN write findicadorDeRateioISSQN;
    property ambienteEmissaoNFe: SmallInt read fambienteEmissaoNFe write fambienteEmissaoNFe;
    property ambienteEmissaoNFCe: SmallInt read fambienteEmissaoNFCe write fambienteEmissaoNFCe;
    property ambienteEmissaoCFe: SmallInt read fambienteEmissaoCFe write fambienteEmissaoCFe;
    property ambienteEmissaoNFSe: SmallInt read fambienteEmissaoNFSe write fambienteEmissaoNFSe;
    property ambienteEmissaoCTe: SmallInt read fambienteEmissaoCTe write fambienteEmissaoCTe;
    property ambienteEmissaoMDFe: SmallInt read fambienteEmissaoMDFe write fambienteEmissaoMDFe;
  end;
  TEstabelecimentoCDocumentos = TArray<TEstabelecimentoCDocumento>;

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
  public
    constructor Create(aid: string; anome: string);
    destructor Destroy();
    property codigo: string read fcodigo write fcodigo;
    property nome: string read fnome write fnome;
    property situacao: SmallInt read fsituacao write fsituacao;
    property estabelecimentoAmbientes: TEstabelecimentoCAmbientes read festabelecimentoAmbientes write festabelecimentoAmbientes;
    property estabelecimentoDepartamentos: TEstabelecimentoCDepartamentos read festabelecimentoDepartamentos write festabelecimentoDepartamentos;
    property estabelecimentoDocumentos: TEstabelecimentoCDocumentos read festabelecimentoDocumentos write festabelecimentoDocumentos;
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

