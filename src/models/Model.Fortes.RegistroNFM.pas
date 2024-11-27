unit Model.Fortes.RegistroNFM;

interface

uses
  Fortes.IRegistro, Model.Fortes.RegistroPNM, Generics.Collections, System.StrUtils;

type
  TTipoSituacao = (
    tsNormal = 0, tsCancelado = 1, tsNormalExtemporaneo = 2, tsCanceladoExtemporaneo = 3,
    tsNFeDenegada = 4, tsNFeInutilizada = 5, tsDFComplementar = 6, tsDFRegimeEspecial = 8);

  TTipoGNRE = (tgNenhum = 0, tgSubstituicao = 1, tgImportacao = 2, tgDifal = 3);

  TRegistroNFM = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FEstabelecimento: smallInt;
    FOperacao: string;
    FEspecie: string;
    FDocumentoProprio: Boolean;
    FAIDF: integer;
    FSerie: string;
    FSubserie: string;
    FNumero: integer;
    FFormularioInicial: integer;
    FFormularioFinal: integer;
    FDataEmissao: TDateTime;
    FSituacaoDocumento: TTipoSituacao;
    FDataEntradaSaida: TDateTime;
    FRemetenteDestinatario: integer;
    FGNREVinculada: Boolean;
    FGNREICMS: TTipoGNRE;
    FGNREMesAno: integer;
    FGNREConvenio: string;
    FGNREDataVencimento: TDateTime;
    FGNREDataRecebimento: TDateTime;
    FGNREBanco: string;
    FGNREAgencia: string;
    FGNREAgenciaDV: string;
    FGNREAutenticado: string;
    FProdutos: double;
    FFrete: double;
    FSeguro: double;
    FOutrasDespesas: double;
    FICMSImportacao: double;
    FICMSImportacaoDiferido: double;
    FIPI: double;
    FSubstituicaoRetido: double;
    FServicoISS: double;
    FDescontoTotal: double;
    FValorTotal: double;
    FQuantProdutos: integer;
    FSubstituicaoRecolher: boolean;
    FAntecipadoRecolher: boolean;
    FDiferencialDeAliquota: boolean;
    FValorSusbstituicao: double;
    FBaseSubstituicao: double;
    FValorAntecipado: double;
    FISSRetido: boolean;
    FDataRetencaoISS: TDateTime;
    FServico: smallint;
    FDataEntradaEstado: TDateTime;
    FFretePorConta: string; //N - Não inform., R - Remetente, D - Destinatário
    FFatura: string;  //N - Não inform., V - A vista, P - A prazo
    FNumeroEEC: smallint;
    FNumeroCupom: integer;
    FReceitaTributavelCOFINS: double;
    FReceitaTributavelPIS: double;
    FReceitaTributavelCSL1: double;
    FReceitaTributavelCSL2: double;
    FReceitaTributavelIRPJ1: double;
    FRejeitaTributavelIRPJ2: double;
    FRejeitaTributavelIRPJ3: double;
    FReceitaTributavelIRPJ4: double;
    FCOFINSRetidoNaFonte: double;
    FPISRetidoNaFonte: double;
    FCSLRetidoNaFonte: double;
    FIRPJRetidoNaFonte: double;
    FGeraTransferencia: boolean;
    FObservacao: string;
    FAliquotaSubstituicaoTributaria: double;
    FChaveEletronica: string;
    FINSSRetidoNaFonte: double;
    FBaseCOFINSPISNaoCumulativos: double;
    FMotivoDoCancelamento: string;
    FNaturezaDaOperacao: integer;
    FCodigoDaInformacaoComplementar: integer;
    FComplementoDaInformacaoComplementar: string;
    FHoraDaSaida: TTime;
    FUFDeEmbarque: string;
    FLocalDeEmbarque: string;
    FCodigoContabil: integer;
    FChaveNFeDeReferencia: string;
    FInformacaoAdicionalFisco: string;
    FIndicadorDeOperacaoConsumidorFinal: boolean;
    FIndicadorDePresencaDoComprador: smallint;
    FDataDaContingencia: TDateTime;
    FHoraDaContingencia: TTime;
    FReconheceNFe: string;
    FNFeInformadaPeloContribuinte: boolean;
    FTotalDoICMSDesoneracao: double;
    FPrestadorDeServicosEmObraDeConstrucaoCivil: smallint;
    FCNO: integer;
    FDataDaEscrituracao: TDateTime;
    FTotalDoFCPSubstituicao: double;
    FNFeSaidaDeTerceiros: boolean;
    FICMSMonoRetido: double;
    FDecreto35395: boolean;
    FListaProdutos: TList<TRegistroPNM>;
  public
    constructor Create;
    destructor Destroy;
    function GerarLinha: string;
    procedure AdicionarProduto(ProdutoPNM: TRegistroPNM);

    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Estabelecimento: smallInt read FEstabelecimento write FEstabelecimento;
    property Operacao: string read FOperacao write FOperacao;
    property Especie: string read FEspecie write FEspecie;
    property DocumentoProprio: Boolean read FDocumentoProprio write FDocumentoProprio;
    property AIDF: integer read FAIDF write FAIDF;
    property Serie: string read FSerie write FSerie;
    property Subserie: string read FSubserie write FSubserie;
    property Numero: integer read FNumero write FNumero;
    property FormularioInicial: integer read FFormularioInicial write FFormularioInicial;
    property FormularioFinal: integer read FFormularioFinal write FFormularioFinal;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property SituacaoDocumento: TTipoSituacao read FSituacaoDocumento write FSituacaoDocumento;
    property DataEntradaSaida: TDateTime read FDataEntradaSaida write FDataEntradaSaida;
    property RemetenteDestinatario: integer read FRemetenteDestinatario write FRemetenteDestinatario;
    property GNREVinculada: Boolean read FGNREVinculada write FGNREVinculada;
    property GNREICMS: TTipoGNRE read FGNREICMS write FGNREICMS;
    property GNREMesAno: integer read FGNREMesAno write FGNREMesAno;
    property GNREConvenio: string read FGNREConvenio write FGNREConvenio;
    property GNREDataVencimento: TDateTime read FGNREDataVencimento write FGNREDataVencimento;
    property GNREDataRecebimento: TDateTime read FGNREDataRecebimento write FGNREDataRecebimento;
    property GNREBanco: string read FGNREBanco write FGNREBanco;
    property GNREAgencia: string read FGNREAgencia write FGNREAgencia;
    property GNREAgenciaDV: string read FGNREAgenciaDV write FGNREAgenciaDV;
    property GNREAutenticado: string read FGNREAutenticado write FGNREAutenticado;
    property Produtos: double read FProdutos write FProdutos;
    property Frete: double read FFrete write FFrete;
    property Seguro: double read FSeguro write FSeguro;
    property OutrasDespesas: double read FOutrasDespesas write FOutrasDespesas;
    property ICMSImportacao: double read FICMSImportacao write FICMSImportacao;
    property ICMSImportacaoDiferido: double read FICMSImportacaoDiferido write FICMSImportacaoDiferido;
    property IPI: double read FIPI write FIPI;
    property SubstituicaoRetido: double read FSubstituicaoRetido write FSubstituicaoRetido;
    property ServicoISS: double read FServicoISS write FServicoISS;
    property DescontoTotal: double read FDescontoTotal write FDescontoTotal;
    property ValorTotal: double read FValorTotal write FValorTotal;
    property QuantProdutos: integer read FQuantProdutos write FQuantProdutos;
    property SubstituicaoRecolher: boolean read FSubstituicaoRecolher write FSubstituicaoRecolher;
    property AntecipadoRecolher: boolean read FAntecipadoRecolher write FAntecipadoRecolher;
    property DiferencialDeAliquota: boolean read FDiferencialDeAliquota write FDiferencialDeAliquota;
    property ValorSusbstituicao: double read FValorSusbstituicao write FValorSusbstituicao;
    property BaseSubstituicao: double read FBaseSubstituicao write FBaseSubstituicao;
    property ValorAntecipado: double read FValorAntecipado write FValorAntecipado;
    property ISSRetido: boolean read FISSRetido write FISSRetido;
    property DataRetencaoISS: TDateTime read FDataRetencaoISS write FDataRetencaoISS;
    property Servico: smallint read FServico write FServico;
    property DataEntradaEstado: TDateTime read FDataEntradaEstado write FDataEntradaEstado;
    property FretePorConta: string read FFretePorConta write FFretePorConta; //N - Não inform., R - Remetente, D - Destinatário
    property Fatura: string read FFatura write FFatura;  //N - Não inform., V - A vista, P - A prazo
    property NumeroEEC: smallint read FNumeroEEC write FNumeroEEC;
    property NumeroCupom: integer read FNumeroCupom write FNumeroCupom;
    property ReceitaTributavelCOFINS: double read FReceitaTributavelCOFINS write FReceitaTributavelCOFINS;
    property ReceitaTributavelPIS: double read FReceitaTributavelPIS write FReceitaTributavelPIS;
    property ReceitaTributavelCSL1: double read FReceitaTributavelCSL1 write FReceitaTributavelCSL1;
    property ReceitaTributavelCSL2: double read FReceitaTributavelCSL2 write FReceitaTributavelCSL2;
    property ReceitaTributavelIRPJ1: double read FReceitaTributavelIRPJ1 write FReceitaTributavelIRPJ1;
    property RejeitaTributavelIRPJ2: double read FRejeitaTributavelIRPJ2 write FRejeitaTributavelIRPJ2;
    property RejeitaTributavelIRPJ3: double read FRejeitaTributavelIRPJ3 write FRejeitaTributavelIRPJ3;
    property ReceitaTributavelIRPJ4: double read FReceitaTributavelIRPJ4 write FReceitaTributavelIRPJ4;
    property COFINSRetidoNaFonte: double read FCOFINSRetidoNaFonte write FCOFINSRetidoNaFonte;
    property PISRetidoNaFonte: double read FPISRetidoNaFonte write FPISRetidoNaFonte;
    property CSLRetidoNaFonte: double read FCSLRetidoNaFonte write FCSLRetidoNaFonte;
    property IRPJRetidoNaFonte: double read FIRPJRetidoNaFonte write FIRPJRetidoNaFonte;
    property GeraTransferencia: boolean read FGeraTransferencia write FGeraTransferencia;
    property Observacao: string read FObservacao write FObservacao;
    property AliquotaSubstituicaoTributaria: double read FAliquotaSubstituicaoTributaria write FAliquotaSubstituicaoTributaria;
    property ChaveEletronica: string read FChaveEletronica write FChaveEletronica;
    property INSSRetidoNaFonte: double read FINSSRetidoNaFonte write FINSSRetidoNaFonte;
    property BaseCOFINSPISNaoCumulativos: double read FBaseCOFINSPISNaoCumulativos write FBaseCOFINSPISNaoCumulativos;
    property MotivoDoCancelamento: string read FMotivoDoCancelamento write FMotivoDoCancelamento;
    property NaturezaDaOperacao: integer read FNaturezaDaOperacao write FNaturezaDaOperacao;
    property CodigoDaInformacaoComplementar: integer read FCodigoDaInformacaoComplementar write FCodigoDaInformacaoComplementar;
    property ComplementoDaInformacaoComplementar: string read FComplementoDaInformacaoComplementar write FComplementoDaInformacaoComplementar;
    property HoraDaSaida: TTime read FHoraDaSaida write FHoraDaSaida;
    property UFDeEmbarque: string read FUFDeEmbarque write FUFDeEmbarque;
    property LocalDeEmbarque: string read FLocalDeEmbarque write FLocalDeEmbarque;
    property CodigoContabil: integer read FCodigoContabil write FCodigoContabil;
    property ChaveNFeDeReferencia: string read FChaveNFeDeReferencia write FChaveNFeDeReferencia;
    property InformacaoAdicionalFisco: string read FInformacaoAdicionalFisco write FInformacaoAdicionalFisco;
    property IndicadorDeOperacaoConsumidorFinal: boolean read FIndicadorDeOperacaoConsumidorFinal write FIndicadorDeOperacaoConsumidorFinal;
    property IndicadorDePresencaDoComprador: smallint read FIndicadorDePresencaDoComprador write FIndicadorDePresencaDoComprador;
    property DataDaContingencia: TDateTime read FDataDaContingencia write FDataDaContingencia;
    property HoraDaContingencia: TTime read FHoraDaContingencia write FHoraDaContingencia;
    property ReconheceNFe: string read FReconheceNFe write FReconheceNFe;
    property NFeInformadaPeloContribuinte: boolean read FNFeInformadaPeloContribuinte write FNFeInformadaPeloContribuinte;
    property TotalDoICMSDesoneracao: double read FTotalDoICMSDesoneracao write FTotalDoICMSDesoneracao;
    property PrestadorDeServicosEmObraDeConstrucaoCivil: smallint read FPrestadorDeServicosEmObraDeConstrucaoCivil write FPrestadorDeServicosEmObraDeConstrucaoCivil;
    property CNO: integer read FCNO write FCNO;
    property DataDaEscrituracao: TDateTime read FDataDaEscrituracao write FDataDaEscrituracao;
    property TotalDoFCPSubstituicao: double read FTotalDoFCPSubstituicao write FTotalDoFCPSubstituicao;
    property NFeSaidaDeTerceiros: boolean read FNFeSaidaDeTerceiros write FNFeSaidaDeTerceiros;
    property ICMSMonoRetido: double read FICMSMonoRetido write FICMSMonoRetido;
    property Decreto35395: boolean read FDecreto35395 write FDecreto35395;
    property ListaProdutos: TList<TRegistroPNM> read FListaProdutos write FListaProdutos;
  end;

implementation

uses
  SysUtils;

procedure TRegistroNFM.AdicionarProduto(ProdutoPNM: TRegistroPNM);
begin
  FListaProdutos.Add(ProdutoPNM);
end;

constructor TRegistroNFM.Create;
begin
  FTipoRegistro := 'NFM';
  FListaProdutos := TList<TRegistroPNM>.Create;
end;

destructor TRegistroNFM.Destroy;
begin
  FListaProdutos.Free;
  inherited;
end;

function TRegistroNFM.GerarLinha: string;
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Builder
      .Append(FTipoRegistro).Append('|')
      .Append(FNumero).Append('|')
      .Append(IntToStr(FEstabelecimento)).Append('|')
      .Append(FOperacao).Append('|')
      .Append(FEspecie).Append('|')
      .Append(IfThen(FDocumentoProprio, 'S', 'N')).Append('|')
      .Append(IntToStr(FAIDF)).Append('|')
      .Append(FSerie).Append('|')
      .Append(FSubserie).Append('|')
      .Append(IntToStr(FNumero)).Append('|')
      .Append(IntToStr(FFormularioInicial)).Append('|')
      .Append(IntToStr(FFormularioFinal)).Append('|')
      .Append(FormatDateTime('yyyymmdd', FDataEmissao)).Append('|')
      .Append('0').Append('|')   //FSituacaoDocumento
      .Append(FormatDateTime('yyyymmdd', FDataEntradaSaida)).Append('|')
      .Append(FRemetenteDestinatario).Append('|')
      .Append(IfThen(FGNREVinculada, 'S', 'N')).Append('|')
      .Append('').Append('|') //FGNREICMS
      .Append(IntToStr(FGNREMesAno)).Append('|')
      .Append(FGNREConvenio).Append('|')
      .Append(FormatDateTime('yyyymmdd', FGNREDataVencimento)).Append('|')
      .Append(FormatDateTime('yyyymmdd', FGNREDataRecebimento)).Append('|')
      .Append(FGNREBanco).Append('|')
      .Append(FGNREAgencia).Append('|')
      .Append(FGNREAgenciaDV).Append('|')
      .Append(FGNREAutenticado).Append('|')
      .Append(FormatFloat('#0.00', FProdutos)).Append('|')
      .Append(FormatFloat('#0.00', FFrete)).Append('|')
      .Append(FormatFloat('#0.00', FSeguro)).Append('|')
      .Append(FormatFloat('#0.00', FOutrasDespesas)).Append('|')
      .Append(FormatFloat('#0.00', FICMSImportacao)).Append('|')
      .Append(FormatFloat('#0.00', FICMSImportacaoDiferido)).Append('|')
      .Append(FormatFloat('#0.00', FIPI)).Append('|')
      .Append(FormatFloat('#0.00', FSubstituicaoRetido)).Append('|')
      .Append(FormatFloat('#0.00', FServicoISS)).Append('|')
      .Append(FormatFloat('#0.00', FDescontoTotal)).Append('|')
      .Append(FormatFloat('#0.00', FValorTotal)).Append('|')
      .Append(IntToStr(FQuantProdutos)).Append('|')
      .Append(IfThen(FSubstituicaoRecolher, 'S', 'N')).Append('|')
      .Append(IfThen(FAntecipadoRecolher, 'S', 'N')).Append('|')
      .Append(IfThen(FDiferencialDeAliquota, 'S', 'N')).Append('|')
      .Append(FormatFloat('#0.00', FValorSusbstituicao)).Append('|')
      .Append(FormatFloat('#0.00', FBaseSubstituicao)).Append('|')
      .Append(FormatFloat('#0.00', FValorAntecipado)).Append('|')
      .Append(IfThen(FISSRetido, 'S', 'N')).Append('|')
      .Append(FormatDateTime('yyyymmdd', FDataRetencaoISS)).Append('|')
      .Append(IntToStr(FServico)).Append('|')
      .Append(FormatDateTime('yyyymmdd', FDataEntradaEstado)).Append('|')
      .Append(FFretePorConta).Append('|')
      .Append(FFatura).Append('|')
      .Append(IntToStr(FNumeroEEC)).Append('|')
      .Append(IntToStr(FNumeroCupom)).Append('|')
      .Append(FormatFloat('#0.00', FReceitaTributavelCOFINS)).Append('|')
      .Append(FormatFloat('#0.00', FReceitaTributavelPIS)).Append('|')
      .Append(FormatFloat('#0.00', FReceitaTributavelCSL1)).Append('|')
      .Append(FormatFloat('#0.00', FReceitaTributavelCSL2)).Append('|')
      .Append(FormatFloat('#0.00', FReceitaTributavelIRPJ1)).Append('|')
      .Append(FormatFloat('#0.00', FRejeitaTributavelIRPJ2)).Append('|')
      .Append(FormatFloat('#0.00', FRejeitaTributavelIRPJ3)).Append('|')
      .Append(FormatFloat('#0.00', FReceitaTributavelIRPJ4)).Append('|')
      .Append(FormatFloat('#0.00', FCOFINSRetidoNaFonte)).Append('|')
      .Append(FormatFloat('#0.00', FPISRetidoNaFonte)).Append('|')
      .Append(FormatFloat('#0.00', FCSLRetidoNaFonte)).Append('|')
      .Append(FormatFloat('#0.00', FIRPJRetidoNaFonte)).Append('|')
      .Append(IfThen(FGeraTransferencia, 'S', 'N')).Append('|')
      .Append(FObservacao).Append('|')
      .Append(FAliquotaSubstituicaoTributaria).Append('|')
      .Append(FChaveEletronica).Append('|')
      .Append(FormatFloat('#0.00', FINSSRetidoNaFonte)).Append('|')
      .Append(FormatFloat('#0.00', FBaseCOFINSPISNaoCumulativos)).Append('|')
      .Append(FMotivoDoCancelamento).Append('|')
      .Append(IntToStr(FNaturezaDaOperacao)).Append('|')
      .Append(IntToStr(FCodigoDaInformacaoComplementar)).Append('|')
      .Append(FComplementoDaInformacaoComplementar).Append('|')
      .Append(FormatDateTime('hhmm', FHoraDaSaida)).Append('|')
      .Append(FUFDeEmbarque).Append('|')
      .Append(FLocalDeEmbarque).Append('|')
      .Append(IntToStr(FCodigoContabil)).Append('|')
      .Append(FChaveNFeDeReferencia).Append('|')
      .Append(FInformacaoAdicionalFisco).Append('|')
      .Append(IfThen(FIndicadorDeOperacaoConsumidorFinal, 'S', 'N')).Append('|')
      .Append(IntToStr(FIndicadorDePresencaDoComprador)).Append('|')
      .Append(FormatDateTime('yyyymmdd', FDataDaContingencia)).Append('|')
      .Append(FormatDateTime('hhmm', FHoraDaContingencia)).Append('|')
      .Append(FReconheceNFe).Append('|')
      .Append(IfThen(FNFeInformadaPeloContribuinte, 'S', 'N')).Append('|')
      .Append(FormatFloat('#0.00', FTotalDoICMSDesoneracao)).Append('|')
      .Append(IntToStr(FPrestadorDeServicosEmObraDeConstrucaoCivil)).Append('|')
      .Append(IntToStr(FCNO)).Append('|')
      .Append(FormatDateTime('yyymmdd', FDataDaEscrituracao)).Append('|')
      .Append(FormatFloat('#0.00', FTotalDoFCPSubstituicao)).Append('|')
      .Append(IfThen(FNFeSaidaDeTerceiros, 'S', 'N')).Append('|')
      .Append(FormatFloat('#0.00', FICMSMonoRetido)).Append('|')
      .Append(IfThen(FDecreto35395, 'S', 'N')).Append('|');

    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

end.

