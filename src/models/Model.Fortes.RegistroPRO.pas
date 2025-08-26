unit Model.Fortes.RegistroPRO;

interface

uses
  System.Classes, Fortes.IRegistro, Model.Fortes.RegistroOUM, Generics.Collections, System.StrUtils;

type

  TTipoUnidadeMedidaDIEF = (
    tsLitro = 1, tsMetro = 2, tsMetroCubico = 3, tsMetroQuadrado = 4, tsPar = 5,
    tsQuilograma = 6, tsQuilowattHora = 7, tsUnidade = 8
  );

  TUnidadeMedidaCENFOP = (
    tsArroba = 1, tsBarril = 2, tsBalde = 3, tsCento = 4, tsCartela = 5, tsCaixa = 6,
    tsCaixote = 7, tsDuzia = 8, tsEmbalagem = 9, tsEnvelope = 10, tsFardo = 11,
    tsGarrafa = 12, tsGalao = 13, tsQuilogramaCENFOP = 14, tsQuloLitro = 15,
    tsQuiloWattHoraCENFOP = 16
  );

  TRegistroPRO = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigo: integer;
    FDescricao: string;
    FCodigoUtilizadoEstab: string;
    FCodigoNCM: string;
    FUnidadeMedida: string;
    FOutraUnidade: TList<TRegistroOUM>;
    FUnidadeMedidaDIEF: TtipoUnidadeMedidaDIEF;
    FUnidadeMedidaCENFOP: TUnidadeMedidaCENFOP;
    FClassificacao: integer;
    FGrupo: integer;
    FGenero: smallInt;
    FCodigoDeBarras: string;
    FReducao: double;
    FCodigoGAM57: integer;
    FCSTICMS: string;
    FCSTIPI: string;
    FCSTCOFINS: string;
    FCSTPIS: string;
    FCodigoANP: integer;
    FCSTICMSSimplesNacional: SmallInt;
    FCSOSN: Integer;
    FProdutoEspecifico: SmallInt;
    FTipoDeMedicamento: SmallInt;
    FDesativado: boolean;
    FCodigoIndicadorContribuicaoPrevidenciaria: string;
    FTipoDeTributacaoDIA: string;
    FCodificacaoNVE: string;
    FIndicadorEspecial: string;
    FCodigoDaApuracao: string;
    FCodigoTIPI: string;
    FCodigoCombustívelDIEFPA : SmallInt;
    FPercentualDeIncentivo: double;
    FPrazoDeFruicao: integer;
    FIndicadorEspecialDeIncentivo: SmallInt;
    FPercentualDaCSL: SmallInt;
    FPercentualDoIRPJ: SmallInt;
    FAliqICMSInterna: double;
    FCodigosDaReceitaProdutoEspecifico: boolean;
    FCodReceitaRetidoCOFINS: string;
    FCodReceitaRetidoPIS: string;
    FCodReceitaRetidoCSL: string;
    FCodReceitaRetidoIRPJ: string;
    FCodReceitaRetidoCOSIRF: string;
    FDecretoAM: boolean;
    FCodReceitaPIS: integer;
    FCodCEST: string;
    FCustoDeAquisicao: double;
    FSubstituicaoDeICMS: boolean;
    FSubstituicaoDeIPI: boolean;
    FSubstituicaoDeCOFINS: boolean;
    FSubstituicaoDePISPASEP: boolean;
    FTributacaoMonofasicaDeCOFINS: boolean;
    FTributacaoMonofasicaDePIS: boolean;
    FApuracaoDoPISCOFINS: SmallInt;
  public
    constructor Create;
    destructor Destroy;
    function GerarLinha: string;
    procedure AdicionarOutraUnidade(OutraUnidade: TRegistroOUM);
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Codigo: integer read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property CodigoUtilizadoEstab: string read FCodigoUtilizadoEstab write FCodigoUtilizadoEstab;
    property CodigoNCM: string read FCodigoNCM write FCodigoNCM;
    property UnidadeMedida: string read FUnidadeMedida write FUnidadeMedida;
    property OutraUnidade: TList<TRegistroOUM> read FOutraUnidade write FOutraUnidade; //VER
    property UnidadeMedidaDIEF: TTipoUnidadeMedidaDIEF read FUnidadeMedidaDIEF write FUnidadeMedidaDIEF;
    property UnidadeMedidaCENFOP: TUnidadeMedidaCENFOP read FUnidadeMedidaCENFOP write FUnidadeMedidaCENFOP;
    property Classificacao: integer read FClassificacao write FClassificacao;
    property Grupo: Integer read FGrupo write FGrupo;
    property Genero: SmallInt read FGenero write FGenero;
    property CodigoDeBarras: string read FCodigoDeBarras write FCodigoDeBarras;
    property Reducao: double read FReducao write FReducao;  // TIPO V grava com .   "12450.20"
    property CodigoGAM57: integer read FCodigoGAM57 write FCodigoGAM57;
    property CSTICMS: string read FCSTICMS write FCSTICMS;
    property CSTIPI: string read FCSTIPI write FCSTIPI;
    property CSTCOFINS: string read FCSTCOFINS write FCSTCOFINS;
    property CSTPIS: string read FCSTPIS write FCSTPIS;
    property CodigoANP: integer read FCodigoANP write FCodigoANP;
    property CSTICMSSimplesNacional: SmallInt read FCSTICMSSimplesNacional write FCSTICMSSimplesNacional;
    property CSOSN: Integer read FCSOSN write FCSOSN;
    property ProdutoEspecifico: smallInt read FProdutoEspecifico write FProdutoEspecifico;
    property TipoDeMedicamento: smallInt read FTipoDeMedicamento write FTipoDeMedicamento;
    property Desativado: boolean read FDesativado write FDesativado;
    property CodigoIndicadorContribuicaoPrevidenciaria: string read FCodigoIndicadorContribuicaoPrevidenciaria write FCodigoIndicadorContribuicaoPrevidenciaria;
    property TipoDeTributacaoDIA: string read FTipoDeTributacaoDIA write FTipoDeTributacaoDIA;
    property CodificacaoNVE: string read FCodificacaoNVE write FCodificacaoNVE;
    property IndicadorEspecial: string read FIndicadorEspecial write FIndicadorEspecial;
    property CodigoDaApuracao: string read FCodigoDaApuracao write FCodigoDaApuracao;
    property CodigoTIPI: string read FCodigoTIPI write FCodigoTIPI;
    property CodigoCombustívelDIEFPA: SmallInt read FCodigoCombustívelDIEFPA write FCodigoCombustívelDIEFPA;
    property PercentualDeIncentivo: double read FPercentualDeIncentivo write FPercentualDeIncentivo;
    property PrazoDeFruicao: integer read FPrazoDeFruicao write FPrazoDeFruicao;
    property IndicadorEspecialDeIncentivo: SmallInt read FIndicadorEspecialDeIncentivo write FIndicadorEspecialDeIncentivo;
    property PercentualDaCSL: SmallInt read FPercentualDaCSL write FPercentualDaCSL;
    property PercentualDoIRPJ: SmallInt read FPercentualDoIRPJ write FPercentualDoIRPJ;
    property AliqICMSInterna: Double read FAliqICMSInterna write FAliqICMSInterna;
    property CodigosDaReceitaProdutoEspecifico: boolean read FCodigosDaReceitaProdutoEspecifico write FCodigosDaReceitaProdutoEspecifico;
    property CodReceitaPIS: integer read FCodReceitaPIS write FCodReceitaPIS;
    property CodCEST: string read FCodCEST write FCodCEST;
    property CustoDeAquisicao: double read FCustoDeAquisicao write FCustoDeAquisicao;
    property SubstituicaoDeICMS: boolean read FSubstituicaoDeICMS write FSubstituicaoDeICMS;
    property SubstituicaoDeIPI: boolean read FSubstituicaoDeIPI write FSubstituicaoDeIPI;
    property SubstituicaoDeCOFINS: boolean read FSubstituicaoDeCOFINS write FSubstituicaoDeCOFINS;
    property SubstituicaoDePISPASEP: boolean read FSubstituicaoDePISPASEP write FSubstituicaoDePISPASEP;
    property TributacaoMonofasicaDeCOFINS: boolean read FTributacaoMonofasicaDeCOFINS write FTributacaoMonofasicaDeCOFINS;
    property TributacaoMonofasicaDePIS: boolean read FTributacaoMonofasicaDePIS write FTributacaoMonofasicaDePIS;
    property ApuracaoDoPISCOFINS: SmallInt read FApuracaoDoPISCOFINS write FApuracaoDoPISCOFINS;
    property CodReceitaRetidoCOFINS: string read FCodReceitaRetidoCOFINS write FCodReceitaRetidoCOFINS;
    property CodReceitaRetidoPIS: string read FCodReceitaRetidoPIS write FCodReceitaRetidoPIS;
    property CodReceitaRetidoCSL: string read FCodReceitaRetidoCSL write FCodReceitaRetidoCSL;
    property CodReceitaRetidoIRPJ: string read FCodReceitaRetidoIRPJ write FCodReceitaRetidoIRPJ;
    property CodReceitaRetidoCOSIRF: string read FCodReceitaRetidoCOSIRF write FCodReceitaRetidoCOSIRF;
    property DecretoAM: boolean read FDecretoAM write FDecretoAM;
  end;

implementation

uses
  SysUtils;

procedure TRegistroPRO.AdicionarOutraUnidade(OutraUnidade: TRegistroOUM);
begin
  FOutraUnidade.Add(OutraUnidade);
end;

constructor TRegistroPRO.Create;
begin
  FTipoRegistro := 'PRO';
//  FOutraUnidade := FOutraUnidade.Create;
end;

destructor TRegistroPRO.Destroy;
begin
 // OutraUnidade.Destroy;
//  inherited Destroy;
end;

function TRegistroPRO.GerarLinha: string;
var
  Linha: TStringList;
begin
  var Produto := Format(
    '%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s', [
    FTipoRegistro,
    FCodigo.ToString,
    FDescricao,
    FCodigoUtilizadoEstab,
    FCodigoNCM,
    FUnidadeMedida,
    IntTostr(Integer(FUnidadeMedidaDIEF)),
    IntTostr(Integer(FUnidadeMedidaCENFOP)),
    FClassificacao.ToString,
    FGrupo.ToString,
    FGenero.ToString,
    FCodigoDeBarras,
    FormatFloat('#0.00',FReducao),
    FCodigoGAM57.ToString,
    FCSTICMS,
    FCSTIPI,
    FCSTCOFINS,
    FCSTPIS,
    FCodigoANP.ToString,
    FCSTICMSSimplesNacional.ToString,
    FCSOSN.ToString,
    FProdutoEspecifico.ToString,
    FTipoDeMedicamento.ToString,
    IfThen(FDesativado, 'S', 'N'),
    FCodigoIndicadorContribuicaoPrevidenciaria,
    FTipoDeTributacaoDIA,
    FCodificacaoNVE,
    FIndicadorEspecial,
    FCodigoDaApuracao,
    FCodigoTIPI,
    FCodigoCombustívelDIEFPA.ToString,
    formatFloat('#0.00',FPercentualDeIncentivo),
    FPrazoDeFruicao.ToString,
    FIndicadorEspecialDeIncentivo.ToString,
    FPercentualDaCSL.ToString,
    FPercentualDoIRPJ.ToString,
    formatFloat('#0.00', FAliqICMSInterna),
    ifThen(FCodigosDaReceitaProdutoEspecifico, 'S', 'N'),
    FCodReceitaPIS.ToString,
    FCodCEST,
    FormatFloat('#0.00',FCustoDeAquisicao),
    IfThen(FSubstituicaoDeICMS, 'S', 'N'),
    IfThen(FSubstituicaoDeIPI, 'S', 'N'),
    IfThen(FSubstituicaoDeCOFINS, 'S', 'N'),
    IfThen(FSubstituicaoDePISPASEP, 'S', 'N'),
    IfThen(FTributacaoMonofasicaDeCOFINS, 'S', 'N'),
    IfThen(FTributacaoMonofasicaDePIS, 'S', 'N'),
    FApuracaoDoPISCOFINS.ToString,
    FCodReceitaRetidoCOFINS,
    FCodReceitaRetidoPIS,
    FCodReceitaRetidoCSL,
    FCodReceitaRetidoIRPJ,
    FCodReceitaRetidoCOSIRF,
    IfThen(FDecretoAM, 'S', 'N')
  ]);

  Linha := TStringList.Create;

  var  RegistroOUM := TRegistroOUM.Create;
  var ListaDeRegistros := TList<IRegistro>.Create;
  ListaDeRegistros.Add(RegistroOUM);

  try
    for var Unidade in ListaDeRegistros do
    begin
      Linha.Add(Unidade.GerarLinha)
    end;
    Result := Produto;
  finally
    Linha.Free;
    ListaDeRegistros.Free;
  end;
end;

end.

