unit Model.Fortes.RegistroPNM;

interface

uses Fortes.IRegistro, System.StrUtils;

type
  TRegistroPNM = class(TInterfacedObject, IRegistro)
  private
    FAliquotaDeDestinoDiferencialDeAliquota: double;
    FValorFrete: double;
    FBaseDeCalcSubstTrib: double;
    FTributacaoICMS: integer;
    FProduto: integer;
    FAliquotaDeAjusteICMS: double;
    FValorUnitarioBCICMSRemetenteRessarcimentoST: double;
    FValorFCPSubTribOuRetido: double;
    FAliquotaFECOPDiferencialDeAliquotas: double;
    FAliquotaDoIPI: double;
    FCustoAquisicaoSubstTrib: double;
    FValorBruto: double;
    FBaseCalculoMonoRetido: double;
    FValorUnitarioBCICMSPagoRessarcimentoST: double;
    FPercAgregacaoAntecipado: double;
    FCodigoItemDocRessarcimentoST: integer;
    FDataDocRessarcimentoST: TDateTime;
    FAliquotaDeOrigemDiferencialDeAliquota: double;
    FItemDerivadoDoPetroleo: boolean;
    FCustoDeAquisicaoAntecipado: double;
    FCSTB: integer;
    FAliquotaMono: double;
    FValorUnitarioItemDocRessarcimentoST: double;
    FCodigoParticipanteDocRessarcimentoST: integer;
    FSubstJaRecolhido: double;
    FAliquotaMonoRetidoAnterior: double;
    FExclusaoBCPISCOFINS: double;
    FBaseDeCalculoDoIPI: double;
    FValorICMSDesonerado: double;
    FCSTA: integer;
    FBaseDeCalculoDoFCPNormal: double;
    FBaseDeCalculoDiferencialDeAliquotas: double;
    FNaturezaDaContribuicaoDoEstornoPIS: smallint;
    FAliquotaPISPercentual: double;
    FValorDesconto: double;
    FAliquotaInterna: double;
    FTipoDeRecolhimento: smallint;
    FChaveEletronicaSubstitutoRessarcimentoST: string;
    FAliquotaICMSRessarcimentoST: double;
    FNaturezaDaContribuicaoDoEstornoCOFINS: smallint;
    FAliquotaCOFINSPercentual: double;
    FBaseCalculoDeAjusteICMS: double;
    FCalculaFECOP: boolean;
    FIndicadorEspecialPRODEPE: smallint;
    FUnidadeDeMedida: string;
    FEspecieDocRessarcimentoST: string;
    FIndicadorEspecialDeIncentivo: smallint;
    FPercentualIPIDevolvido: double;
    FBaseDeCalculoPIS: double;
    FAliquotaSubstTributaria: double;
    FRECOPI: integer;
    FOutrasDespesas: double;
    FCodigoDeAjusteFiscal: integer;
    FAliquotaPISValor: double;
    FBaseDeCalculoCOFINS: double;
    FICMSEfetivo: double;
    FCodItemRessRetICMSSTRessarcimentoST: integer;
    FAliquotaCOFINSValor: double;
    FAliquotaDoICMS: double;
    FBaseCalculoMono: double;
    FPedidoDeCompra: string;
    FBaseCalculoMonoRetidoAnterior: double;
    FCodigoRetencaoDoICMSSTRessarcimentoST: smallint;
    FQuantidadeItemDocRessarcimentoST: double;
    FMotivoDaDevolucaoIPI: string;
    FCodigoContabil: integer;
    FICMSSubstituicao: double;
    FValorICMSMonoDiferido: double;
    FDecreto20686AM: boolean;
    FValorSeguro: double;
    FCSTPIS: smallint;
    FCFOP: integer;
    FAliquotaICMSDiferenciado: double;
    FDecreto46303PE: boolean;
    FCodigoMotivoDoRessarcimentoST: smallint;
    FAliquotaFCPSubTribOuRetido: double;
    FAliquotaCredito: double;
    FCSTCOFINS: smallint;
    FTipoTributacaoIPI: smallint;
    FBaseDeCalculoDoICMS: double;
    FValorDeAjusteICMS: double;
    FSerieDocRetICMSSTRessarcimentoST: string;
    FChaveEletronicaDocRessarcimentoST: string;
    FRessarcimentoSubTrib: boolean;
    FItemDoPedidoDeCompra: integer;
    FValorTotal: double;
    FSerieDocRessarcimentoST: string;
    FAliquotaFCPNormal: double;
    FItemNaoCompoeValorTotal: smallint;
    FValorDoIPI: double;
    FBaseFCPSubTribOuRetido: double;
    FPercAgregSubstituicao: double;
    FBaseCalculoSubstTributaria: double;
    FAliquotaICMSDiferimento: double;
    FCodDocumentoDeArrecadacaoRessarcimentoST: smallint;
    FNumeroDocRetICMSSTRessarcimentoST: integer;
    FParticipaneteRetencaoICMSSTRessarcimentoST: integer;
    FFCI: integer;
    FTipoCalculoPIS: smallint;
    FCreditoDeOrigem: double;
    FNumeroDocDeArrecadacaoRessarcimentoST: string;
    FNumeroDocRessarcimentoST: integer;
    FTipoCalculoCOFINS: smallint;
    FQuantidade: double;
    FMotivoDaDesoneracaoDoICMS: smallint;
    FValorPIS: double;
    FValorUnitarioBCICMSRetencaoRessarcimentoST: double;
    FValorCOFINS: double;
    FCodigoApuracaoPRODEPE: smallint;
    FAliquotaMonoRetido: double;
    FCSTIPI: smallint;
    FAliqSubstFECOP: double;
    FValorICMSSubstituto: double;
    FNaturezaDoCreditoDoEstornoPIS: smallint;
    FNaturezaReceitaPIS: smallint;
    FValorICMSMono: double;
    FValorUnitBCICMSSTRessarcimentoST: double;
    FAliquotaICMSSTRessarcimentoST: double;
    FValorUnitarioCreditoICMSRessarcimentoST: double;
    FNaturezaDoCreditoDoEstornoCOFINS: smallint;
    FNaturezaReceitaCOFINS: smallint;
    FTipoDeSubstituicao: smallint;
    FCFOPTransferencia: integer;
    FValorICMSMonoRetidoAnterior: double;
    FCSOSN: smallint;
    FTipoRegistro: string;

  public
    constructor Create;
    function GerarLinha: string;

    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Produto: integer read FProduto write FProduto;
    property CFOP: integer read FCFOP write FCFOP;
    property CFOPTransferencia: integer read FCFOPTransferencia write FCFOPTransferencia;
    property CSTA: integer read FCSTA write FCSTA;
    property CSTB: integer read FCSTB write FCSTB;
    property UnidadeDeMedida: string read FUnidadeDeMedida write FUnidadeDeMedida;
    property Quantidade: double read FQuantidade write FQuantidade;
    property ValorBruto: double read FValorBruto write FValorBruto;
    property ValorIPI: double read FValorDoIPI write FValorDoIPI;
    property TributacaoICMS: integer read FTributacaoICMS write FTributacaoICMS;
    property BaseDeCalculoDoICMS: double read FBaseDeCalculoDoICMS write FBaseDeCalculoDoICMS;
    property AliquotaDoICMS: double read FAliquotaDoICMS write FAliquotaDoICMS;
    property BaseCalculoSubstTributaria: double read FBaseCalculoSubstTributaria write FBaseCalculoSubstTributaria;
    property ICMSSubstituicao: double read FICMSSubstituicao write FICMSSubstituicao;
    property TipoDeRecolhimento: smallint read FTipoDeRecolhimento write FTipoDeRecolhimento;
    property TipoDeSubstituicao: smallint read FTipoDeSubstituicao write FTipoDeSubstituicao;
    property CustoAquisicaoSubstTrib: double read FCustoAquisicaoSubstTrib write FCustoAquisicaoSubstTrib;
    property PercAgregSubstituicao: double read FPercAgregSubstituicao write FPercAgregSubstituicao;
    property BaseDeCalcSubstTrib: double read FBaseDeCalcSubstTrib write FBaseDeCalcSubstTrib;
    property AliquotaSubstTributaria: double read FAliquotaSubstTributaria write FAliquotaSubstTributaria;
    property CreditoDeOrigem: double read FCreditoDeOrigem write FCreditoDeOrigem;
    property SubstJaRecolhido: double read FSubstJaRecolhido write FSubstJaRecolhido;
    property CustoDeAquisicaoAntecipado: double read FCustoDeAquisicaoAntecipado write FCustoDeAquisicaoAntecipado;
    property PercAgregacaoAntecipado: double read FPercAgregacaoAntecipado write FPercAgregacaoAntecipado;
    property AliquotaInterna: double read FAliquotaInterna write FAliquotaInterna;
    property TipoTributacaoIPI: smallint read FTipoTributacaoIPI write FTipoTributacaoIPI;
    property BaseDeCalculoDoIPI: double read FBaseDeCalculoDoIPI write FBaseDeCalculoDoIPI;
    property AliquotaDoIPI: double read FAliquotaDoIPI write FAliquotaDoIPI;
    property ValorDoIPI: double read FValorDoIPI write FValorDoIPI;
    property CSTIPI: smallint read FCSTIPI write FCSTIPI;
    property CSTCOFINS: smallint read FCSTCOFINS write FCSTCOFINS;
    property CSTPIS: smallint read FCSTPIS write FCSTPIS;
    property BaseDeCalculoCOFINS: double read FBaseDeCalculoCOFINS write FBaseDeCalculoCOFINS;
    property BaseDeCalculoPIS: double read FBaseDeCalculoPIS write FBaseDeCalculoPIS;
    property ValorFrete: double read FValorFrete write FValorFrete;
    property ValorSeguro: double read FValorSeguro write FValorSeguro;
    property ValorDesconto: double read FValorDesconto write FValorDesconto;
    property ValorTotal: double read FValorTotal write FValorTotal;
    property NaturezaReceitaCOFINS: smallint read FNaturezaReceitaCOFINS write FNaturezaReceitaCOFINS;
    property NaturezaReceitaPIS: smallint read FNaturezaReceitaPIS write FNaturezaReceitaPIS;
    property IndicadorEspecialPRODEPE:smallint read FIndicadorEspecialPRODEPE write FIndicadorEspecialPRODEPE;
    property CodigoApuracaoPRODEPE: smallint read FCodigoApuracaoPRODEPE write FCodigoApuracaoPRODEPE;
    property CSOSN: smallint read FCSOSN write FCSOSN;
    property TipoCalculoCOFINS: smallint read FTipoCalculoCOFINS write FTipoCalculoCOFINS;
    property AliquotaCOFINSPercentual: double read FAliquotaCOFINSPercentual write FAliquotaCOFINSPercentual;
    property AliquotaCOFINSValor: double read FAliquotaCOFINSValor write FAliquotaCOFINSValor;
    property ValorCOFINS: double read FValorCOFINS write FValorCOFINS;
    property TipoCalculoPIS: smallint read FTipoCalculoPIS write FTipoCalculoPIS;
    property AliquotaPISPercentual: double read FAliquotaPISPercentual write FAliquotaPISPercentual;
    property AliquotaPISValor: double read FAliquotaPISValor write FAliquotaPISValor;
    property ValorPIS: double read FValorPIS write FValorPIS;
    property CodigoDeAjusteFiscal: integer read FCodigoDeAjusteFiscal write FCodigoDeAjusteFiscal;
    property PedidoDeCompra: string read FPedidoDeCompra write FPedidoDeCompra;
    property ItemDoPedidoDeCompra: integer read FItemDoPedidoDeCompra write FItemDoPedidoDeCompra;
    property OutrasDespesas: double read FOutrasDespesas write FOutrasDespesas;
    property CodigoContabil: integer read FCodigoContabil write FCodigoContabil;
    property ItemNaoCompoeValorTotal: smallint read FItemNaoCompoeValorTotal;
    property NaturezaDaContribuicaoDoEstornoCOFINS: smallint read FNaturezaDaContribuicaoDoEstornoCOFINS write FNaturezaDaContribuicaoDoEstornoCOFINS;
    property NaturezaDaContribuicaoDoEstornoPIS: smallint read FNaturezaDaContribuicaoDoEstornoPIS write FNaturezaDaContribuicaoDoEstornoPIS;
    property NaturezaDoCreditoDoEstornoCOFINS: smallint read FNaturezaDoCreditoDoEstornoCOFINS write FNaturezaDoCreditoDoEstornoCOFINS;
    property NaturezaDoCreditoDoEstornoPIS: smallint read FNaturezaDoCreditoDoEstornoPIS write FNaturezaDoCreditoDoEstornoPIS;
    property FCI: integer read FFCI write FFCI;
    property ItemDerivadoDoPetroleo: boolean read FItemDerivadoDoPetroleo write FItemDerivadoDoPetroleo;
    property RECOPI: integer read FRECOPI write FRECOPI;
    property PercentualIPIDevolvido: double read FPercentualIPIDevolvido write FPercentualIPIDevolvido;
    property MotivoDaDevolucaoIPI: string read FMotivoDaDevolucaoIPI write FMotivoDaDevolucaoIPI;
    property IndicadorEspecialDeIncentivo: smallint read FIndicadorEspecialDeIncentivo write FIndicadorEspecialDeIncentivo;
    property ValorICMSDesonerado: double read FValorICMSDesonerado write FValorICMSDesonerado;
    property MotivoDaDesoneracaoDoICMS: smallint read FMotivoDaDesoneracaoDoICMS write FMotivoDaDesoneracaoDoICMS;
    property BaseDeCalculoDiferencialDeAliquotas: double read FBaseDeCalculoDiferencialDeAliquotas write FBaseDeCalculoDiferencialDeAliquotas;
    property AliquotaFECOPDiferencialDeAliquotas: double read FAliquotaFECOPDiferencialDeAliquotas write FAliquotaFECOPDiferencialDeAliquotas;
    property AliquotaDeOrigemDiferencialDeAliquota: double read FAliquotaDeOrigemDiferencialDeAliquota write FAliquotaDeOrigemDiferencialDeAliquota;
    property AliquotaDeDestinoDiferencialDeAliquota: double read FAliquotaDeDestinoDiferencialDeAliquota write FAliquotaDeDestinoDiferencialDeAliquota;
    property CalculaFECOP: boolean read FCalculaFECOP write FCalculaFECOP;
    property AliqSubstFECOP: double read FAliqSubstFECOP write FAliqSubstFECOP;
    property ExclusaoBCPISCOFINS: double read FExclusaoBCPISCOFINS write FExclusaoBCPISCOFINS;
    property AliquotaCredito: double read FAliquotaCredito write FAliquotaCredito;
    property BaseDeCalculoDoFCPNormal: double read FBaseDeCalculoDoFCPNormal write FBaseDeCalculoDoFCPNormal;
    property AliquotaFCPNormal: double read FAliquotaFCPNormal write FAliquotaFCPNormal;
    property BaseFCPSubTribOuRetido: double read FBaseFCPSubTribOuRetido write FBaseFCPSubTribOuRetido;
    property AliquotaFCPSubTribOuRetido: double read FAliquotaFCPSubTribOuRetido write FAliquotaFCPSubTribOuRetido;
    property ValorFCPSubTribOuRetido: double read FValorFCPSubTribOuRetido write FValorFCPSubTribOuRetido;
    property RessarcimentoSubTrib: boolean read FRessarcimentoSubTrib write FRessarcimentoSubTrib;
    property EspecieDocRessarcimentoST: string read FEspecieDocRessarcimentoST write FEspecieDocRessarcimentoST;
    property DataDocRessarcimentoST: TDateTime read FDataDocRessarcimentoST write FDataDocRessarcimentoST;
    property NumeroDocRessarcimentoST: integer read FNumeroDocRessarcimentoST write FNumeroDocRessarcimentoST;
    property SerieDocRessarcimentoST: string read FSerieDocRessarcimentoST write FSerieDocRessarcimentoST;
    property ChaveEletronicaDocRessarcimentoST: string read FChaveEletronicaDocRessarcimentoST write FChaveEletronicaDocRessarcimentoST;
    property CodigoParticipanteDocRessarcimentoST: integer read FCodigoParticipanteDocRessarcimentoST write FCodigoParticipanteDocRessarcimentoST;
    property CodigoItemDocRessarcimentoST: integer read FCodigoItemDocRessarcimentoST write FCodigoItemDocRessarcimentoST;
    property QuantidadeItemDocRessarcimentoST: double read FQuantidadeItemDocRessarcimentoST write FQuantidadeItemDocRessarcimentoST;
    property ValorUnitarioItemDocRessarcimentoST: double read FValorUnitarioItemDocRessarcimentoST write FValorUnitarioItemDocRessarcimentoST;
    property ValorUnitarioBCICMSPagoRessarcimentoST: double read FValorUnitarioBCICMSPagoRessarcimentoST write FValorUnitarioBCICMSPagoRessarcimentoST;
    property ValorUnitarioBCICMSRemetenteRessarcimentoST: double read FValorUnitarioBCICMSRemetenteRessarcimentoST write FValorUnitarioBCICMSRemetenteRessarcimentoST;
    property AliquotaICMSRessarcimentoST: double read FAliquotaICMSRessarcimentoST write FAliquotaICMSRessarcimentoST;
    property ValorUnitarioBCICMSRetencaoRessarcimentoST: double read FValorUnitarioBCICMSRetencaoRessarcimentoST write FValorUnitarioBCICMSRetencaoRessarcimentoST;
    property ValorUnitarioCreditoICMSRessarcimentoST: double read FValorUnitarioCreditoICMSRessarcimentoST write FValorUnitarioCreditoICMSRessarcimentoST;
    property AliquotaICMSSTRessarcimentoST: double read FAliquotaICMSSTRessarcimentoST write FAliquotaICMSSTRessarcimentoST;
    property ValorUnitBCICMSSTRessarcimentoST: double read FValorUnitBCICMSSTRessarcimentoST write FValorUnitBCICMSSTRessarcimentoST;
    property CodigoRetencaoDoICMSSTRessarcimentoST: smallint read FCodigoRetencaoDoICMSSTRessarcimentoST write FCodigoRetencaoDoICMSSTRessarcimentoST;
    property CodigoMotivoDoRessarcimentoST: smallint read FCodigoMotivoDoRessarcimentoST write FCodigoMotivoDoRessarcimentoST;
    property ChaveEletronicaSubstitutoRessarcimentoST: string read FChaveEletronicaSubstitutoRessarcimentoST write FChaveEletronicaSubstitutoRessarcimentoST;
    property ParticipaneteRetencaoICMSSTRessarcimentoST: integer read FParticipaneteRetencaoICMSSTRessarcimentoST write FParticipaneteRetencaoICMSSTRessarcimentoST;
    property NumeroDocRetICMSSTRessarcimentoST: integer read FNumeroDocRetICMSSTRessarcimentoST write FNumeroDocRetICMSSTRessarcimentoST;
    property SerieDocRetICMSSTRessarcimentoST: string read FSerieDocRetICMSSTRessarcimentoST write FSerieDocRetICMSSTRessarcimentoST;
    property CodItemRessRetICMSSTRessarcimentoST: integer read FCodItemRessRetICMSSTRessarcimentoST write FCodItemRessRetICMSSTRessarcimentoST;
    property CodDocumentoDeArrecadacaoRessarcimentoST: smallint read FCodDocumentoDeArrecadacaoRessarcimentoST write FCodDocumentoDeArrecadacaoRessarcimentoST;
    property NumeroDocDeArrecadacaoRessarcimentoST: string read FNumeroDocDeArrecadacaoRessarcimentoST write FNumeroDocDeArrecadacaoRessarcimentoST;
    property BaseCalculoDeAjusteICMS: double read FBaseCalculoDeAjusteICMS write FBaseCalculoDeAjusteICMS;
    property AliquotaDeAjusteICMS: double read FAliquotaDeAjusteICMS write FAliquotaDeAjusteICMS;
    property ValorDeAjusteICMS: double read FValorDeAjusteICMS write FValorDeAjusteICMS;
    property AliquotaICMSDiferimento: double read FAliquotaICMSDiferimento write FAliquotaICMSDiferimento;
    property Decreto46303PE: boolean read FDecreto46303PE write FDecreto46303PE;
    property ValorICMSSubstituto: double read FValorICMSSubstituto write FValorICMSSubstituto;
    property ICMSEfetivo: double read FICMSEfetivo write FICMSEfetivo;
    property Decreto20686AM: boolean read FDecreto20686AM write FDecreto20686AM;
    property BaseCalculoMono: double read FBaseCalculoMono write FBaseCalculoMono;
    property AliquotaMono: double read FAliquotaMono write FAliquotaMono;
    property ValorICMSMono: double read FValorICMSMono write FValorICMSMono;
    property BaseCalculoMonoRetido: double read FBaseCalculoMonoRetido write FBaseCalculoMonoRetido;
    property AliquotaMonoRetido: double read FAliquotaMonoRetido write FAliquotaMonoRetido;
    property AliquotaICMSDiferenciado: double read FAliquotaICMSDiferenciado write FAliquotaICMSDiferenciado;
    property ValorICMSMonoDiferido: double read FValorICMSMonoDiferido write FValorICMSMonoDiferido;
    property BaseCalculoMonoRetidoAnterior: double read FBaseCalculoMonoRetidoAnterior write FBaseCalculoMonoRetidoAnterior;
    property AliquotaMonoRetidoAnterior: double read FAliquotaMonoRetidoAnterior write FAliquotaMonoRetidoAnterior;
    property ValorICMSMonoRetidoAnterior: double read FValorICMSMonoRetidoAnterior write FValorICMSMonoRetidoAnterior;
  end;

implementation

uses
  SysUtils;

constructor TRegistroPNM.Create;
begin
  FTipoRegistro := 'PNM';
end;

function TRegistroPNM.GerarLinha: string;
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Builder
      .Append(FTipoRegistro).Append('|')
      .Append(FProduto).Append('|')
      .Append(IntToStr(FCFOP)).Append('|')
      .Append(IntToStr(FCFOPTransferencia)).Append('|')
      .Append(IntToStr(FCSTA)).Append('|')
      .Append(IntToStr(FCSTB)).Append('|')
      .Append(FUnidadeDeMedida).Append('|')
      .Append(FloatToStr(FQuantidade)).Append('|')
      .Append(FloatToStr(FValorBruto)).Append('|')
      .Append(FloatToStr(FValorDoIPI)).Append('|')
      .Append(IntToStr(FTributacaoICMS)).Append('|')
      .Append(FloatToStr(FBaseDeCalculoDoICMS)).Append('|')
      .Append(FloatToStr(FAliquotaDoICMS)).Append('|')
      .Append(FloatToStr(FBaseCalculoSubstTributaria)).Append('|')
      .Append(FloatToStr(FICMSSubstituicao)).Append('|')
      .Append(IntToStr(FTipoDeRecolhimento)).Append('|')
      .Append(IntToStr(FTipoDeSubstituicao)).Append('|')
      .Append(FloatToStr(FCustoAquisicaoSubstTrib)).Append('|')
      .Append(FloatToStr(FPercAgregSubstituicao)).Append('|')
      .Append(FloatToStr(FBaseDeCalcSubstTrib)).Append('|')
      .Append(FloatToStr(FAliquotaSubstTributaria)).Append('|')
      .Append(FloatToStr(FCreditoDeOrigem)).Append('|')
      .Append(FloatToStr(FSubstJaRecolhido)).Append('|')
      .Append(FloatToStr(FCustoDeAquisicaoAntecipado)).Append('|')
      .Append(FloatToStr(FPercAgregacaoAntecipado)).Append('|')
      .Append(FloatToStr(FAliquotaInterna)).Append('|')
      .Append(IntToStr(FTipoTributacaoIPI)).Append('|')
      .Append(FloatToStr(FBaseDeCalculoDoIPI)).Append('|')
      .Append(FloatToStr(FAliquotaDoIPI)).Append('|')
      .Append(FloatToStr(FValorDoIPI)).Append('|')
      .Append(IntToStr(FCSTIPI)).Append('|')
      .Append(IntToStr(FCSTCOFINS)).Append('|')
      .Append(IntToStr(FCSTPIS)).Append('|')
      .Append(FloatToStr(FBaseDeCalculoCOFINS)).Append('|')
      .Append(FloatToStr(FBaseDeCalculoPIS)).Append('|')
      .Append(FloatToStr(FValorFrete)).Append('|')
      .Append(FloatToStr(FValorSeguro)).Append('|')
      .Append(FloatToStr(FValorDesconto)).Append('|')
      .Append(FloatToStr(FValorTotal)).Append('|')
      .Append(IntToStr(FNaturezaReceitaCOFINS)).Append('|')
      .Append(IntToStr(FNaturezaReceitaPIS)).Append('|')
      .Append(IntToStr(FIndicadorEspecialPRODEPE)).Append('|')
      .Append(IntToStr(FCodigoApuracaoPRODEPE)).Append('|')
      .Append(IntToStr(FCSOSN)).Append('|')
      .Append(IntToStr(FTipoCalculoCOFINS)).Append('|')
      .Append(FloatToStr(FAliquotaCOFINSPercentual)).Append('|')
      .Append(FloatToStr(FAliquotaCOFINSValor)).Append('|')
      .Append(FloatToStr(FValorCOFINS)).Append('|')
      .Append(IntToStr(FTipoCalculoPIS)).Append('|')
      .Append(FloatToStr(FAliquotaPISPercentual)).Append('|')
      .Append(FloatToStr(FAliquotaPISValor)).Append('|')
      .Append(FloatToStr(FValorPIS)).Append('|')
      .Append(IntToStr(FCodigoDeAjusteFiscal)).Append('|')
      .Append(FPedidoDeCompra).Append('|')
      .Append(IntToStr(FItemDoPedidoDeCompra)).Append('|')
      .Append(FloatToStr(FOutrasDespesas)).Append('|')
      .Append(IntToStr(FCodigoContabil)).Append('|')
      .Append(IntToStr(FItemNaoCompoeValorTotal)).Append('|')
      .Append(IntToStr(FNaturezaDaContribuicaoDoEstornoCOFINS)).Append('|')
      .Append(IntToStr(FNaturezaDaContribuicaoDoEstornoPIS)).Append('|')
      .Append(IntToStr(FNaturezaDoCreditoDoEstornoCOFINS)).Append('|')
      .Append(IntToStr(FNaturezaDoCreditoDoEstornoPIS)).Append('|')
      .Append(IntToStr(FFCI)).Append('|')
      .Append(IfThen(FItemDerivadoDoPetroleo, 'S', 'N')).Append('|')
      .Append(IntToStr(FRECOPI)).Append('|')
      .Append(FloatToStr(FPercentualIPIDevolvido)).Append('|')
      .Append(FMotivoDaDevolucaoIPI).Append('|')
      .Append(IntToStr(FIndicadorEspecialDeIncentivo)).Append('|')
      .Append(FloatToStr(FValorICMSDesonerado)).Append('|')
      .Append(IntToStr(FMotivoDaDesoneracaoDoICMS)).Append('|')
      .Append(FloatToStr(FBaseDeCalculoDiferencialDeAliquotas)).Append('|')
      .Append(FloatToStr(FAliquotaFECOPDiferencialDeAliquotas)).Append('|')
      .Append(FloatToStr(FAliquotaDeOrigemDiferencialDeAliquota)).Append('|')
      .Append(FloatToStr(FAliquotaDeDestinoDiferencialDeAliquota)).Append('|')
      .Append(IfThen(FCalculaFECOP, 'S', 'N')).Append('|')
      .Append(FloatToStr(FAliqSubstFECOP)).Append('|')
      .Append(FloatToStr(FExclusaoBCPISCOFINS)).Append('|')
      .Append(FloatToStr(FAliquotaCredito)).Append('|')
      .Append(FloatToStr(FBaseDeCalculoDoFCPNormal)).Append('|')
      .Append(FloatToStr(FAliquotaFCPNormal)).Append('|')
      .Append(FloatToStr(FBaseFCPSubTribOuRetido)).Append('|')
      .Append(FloatToStr(FAliquotaFCPSubTribOuRetido)).Append('|')
      .Append(FloatToStr(FValorFCPSubTribOuRetido)).Append('|')
      .Append(IfThen(FRessarcimentoSubTrib, 'S', 'N')).Append('|')
      .Append(FEspecieDocRessarcimentoST).Append('|')
      .Append(FormatDateTime('yyyymmdd', FDataDocRessarcimentoST)).Append('|')
      .Append(IntToStr(FNumeroDocRessarcimentoST)).Append('|')
      .Append(FSerieDocRessarcimentoST).Append('|')
      .Append(FChaveEletronicaDocRessarcimentoST).Append('|')
      .Append(IntToStr(FCodigoParticipanteDocRessarcimentoST)).Append('|')
      .Append(IntToStr(FCodigoItemDocRessarcimentoST)).Append('|')
      .Append(FloatToStr(FQuantidadeItemDocRessarcimentoST)).Append('|')
      .Append(FloatToStr(FValorUnitarioItemDocRessarcimentoST)).Append('|')
      .Append(FloatToStr(FValorUnitarioBCICMSPagoRessarcimentoST)).Append('|')
      .Append(FloatToStr(FValorUnitarioBCICMSRemetenteRessarcimentoST)).Append('|')
      .Append(FloatToStr(FAliquotaICMSRessarcimentoST)).Append('|')
      .Append(FloatToStr(FValorUnitarioBCICMSRetencaoRessarcimentoST)).Append('|')
      .Append(FloatToStr(FValorUnitarioCreditoICMSRessarcimentoST)).Append('|')
      .Append(FloatToStr(FAliquotaICMSSTRessarcimentoST)).Append('|')
      .Append(FloatToStr(FValorUnitBCICMSSTRessarcimentoST)).Append('|')
      .Append(IntToStr(FCodigoRetencaoDoICMSSTRessarcimentoST)).Append('|')
      .Append(IntToStr(FCodigoMotivoDoRessarcimentoST)).Append('|')
      .Append(FChaveEletronicaSubstitutoRessarcimentoST).Append('|')
      .Append(IntToStr(FParticipaneteRetencaoICMSSTRessarcimentoST)).Append('|')
      .Append(IntToStr(FNumeroDocRetICMSSTRessarcimentoST)).Append('|')
      .Append(FSerieDocRetICMSSTRessarcimentoST).Append('|')
      .Append(IntToStr(FCodItemRessRetICMSSTRessarcimentoST)).Append('|')
      .Append(IntToStr(FCodDocumentoDeArrecadacaoRessarcimentoST)).Append('|')
      .Append(FNumeroDocDeArrecadacaoRessarcimentoST).Append('|')
      .Append(FloatToStr(FBaseCalculoDeAjusteICMS)).Append('|')
      .Append(FloatToStr(FAliquotaDeAjusteICMS)).Append('|')
      .Append(FloatToStr(FValorDeAjusteICMS)).Append('|')
      .Append(FloatToStr(FAliquotaICMSDiferimento)).Append('|')
      .Append(IfThen(FDecreto46303PE, 'S', 'N')).Append('|')
      .Append(FloatToStr(FValorICMSSubstituto)).Append('|')
      .Append(FloatToStr(FICMSEfetivo)).Append('|')
      .Append(IfThen(FDecreto20686AM, 'S', 'N')).Append('|')
      .Append(FloatToStr(FBaseCalculoMono)).Append('|')
      .Append(FloatToStr(FAliquotaMono)).Append('|')
      .Append(FloatToStr(FValorICMSMono)).Append('|')
      .Append(FloatToStr(FBaseCalculoMonoRetido)).Append('|')
      .Append(FloatToStr(FAliquotaMonoRetido)).Append('|')
      .Append(FloatToStr(FAliquotaICMSDiferenciado)).Append('|')
      .Append(FloatToStr(FValorICMSMonoDiferido)).Append('|')
      .Append(FloatToStr(FBaseCalculoMonoRetidoAnterior)).Append('|')
      .Append(FloatToStr(FAliquotaMonoRetidoAnterior)).Append('|')
      .Append(FloatToStr(FValorICMSMonoRetidoAnterior)).Append('|');

    Result := sLineBreak + Builder.ToString;
  finally
    Builder.Free;
  end;
end;

end.


