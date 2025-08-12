unit Model.DocumentoFiscal;

interface

uses Model.Base, Model.Estabelecimento, Model.Parceiro, Model.Historico,
  Model.Operacao, Model.Item, Model.Unidade, Model.Municipio, Model.Conta,
  Model.OrigemDaMercadoria, Model.NCM, Model.CEST, Model.CFOP, Model.CSTICMS,
  Model.CSTIPI, Model.CSTPISCOFINS, Model.Moeda, Lib.Sistema.Tipos, Lib.Funcoes,
  Model.Forma, Model.Empresa;

const
  DOCUMENTOFISCAL_MODELO_NFe = '55';
  DOCUMENTOFISCAL_MODELO_NFSe = '56';
  DOCUMENTOFISCAL_MODELO_CTe = '57';
  DOCUMENTOFISCAL_MODELO_MDFe = '58';
  DOCUMENTOFISCAL_MODELO_CFE = '59';
  DOCUMENTOFISCAL_MODELO_NFCE = '65';

type
  TDocumentoFiscalNFeReferencia = class
  private
    festabelecimento: TEstabelecimento;
    fdocumentoFiscal: string;
    fdocumentoFiscalChave: string;
  public
    property estabelecimento: TEstabelecimento read festabelecimento write festabelecimento;
    property documentoFiscal: string read fdocumentoFiscal write fdocumentoFiscal;
    property documentoFiscalChave: string read fdocumentoFiscalChave write fdocumentoFiscalChave;
  end;

  TDocumentoFiscalNFe = class
  private
    festabelecimento: TEstabelecimento;
    fnumero: Integer;
    fserie: Integer;
    fchave: string;
    fstatus: integer;
    fxml: string;
    fimagem: string;
    fprotocolo: string;
    fformaDeEmissao: smallint;
    findicadorIntermediador: smallint;
    ftipoImpressao: char;
    ffinalidadeEmissao: smallint;
    fconsumidorFinal: smallint;
    ftipoDeAtendimento: smallint;
    finformacoesAdicionaisContribuinte: string;
    finformacoesAdicionaisFisco: string;
    fmsgRetorno: string;
    fcancelamentoProtocolo: string;
    fcancelamentoData: TDateTime;
    fcancelamentoJustificativa: string;
    fdocumentoFiscalNFeReferencia: TArray<TDocumentoFiscalNFeReferencia>;
  public
    property estabelecimento: TEstabelecimento read festabelecimento write festabelecimento;
    property numero: Integer read fnumero write fnumero;
    property serie: Integer read fserie write fserie;
    property chave: String read fchave write fchave;
    property status: Integer read fstatus write fstatus;
    property xml: String read fxml write fxml;
    property imagem: String read fimagem write fimagem;
    property protocolo: String read fprotocolo write fprotocolo;
    property formaDeEmissao: smallint read fformaDeEmissao write fformaDeEmissao;
    property indicadorIntermediador: smallint read findicadorIntermediador write findicadorIntermediador;
    property tipoImpressao: char read ftipoImpressao write ftipoImpressao;
    property finalidadeEmissao: smallint read ffinalidadeEmissao write ffinalidadeEmissao;
    property consumidorFinal: smallint read fconsumidorFinal write fconsumidorFinal;
    property tipoDeAtendimento: smallint read ftipoDeAtendimento write ftipoDeAtendimento;
    property informacoesAdicionaisContribuinte: string read finformacoesAdicionaisContribuinte write finformacoesAdicionaisContribuinte;
    property informacoesAdicionaisFisco: string read finformacoesAdicionaisFisco write finformacoesAdicionaisFisco;
    property msgRetorno: string read fmsgRetorno write fmsgRetorno;
    property cancelamentoProtocolo: string read fcancelamentoProtocolo write fcancelamentoProtocolo;
    property cancelamentoData: TDateTime read fcancelamentoData write fcancelamentoData;
    property cancelamentoJustificativa: string read fcancelamentoJustificativa write fcancelamentoJustificativa;
    property documentoFiscalNFeReferencia: TArray<TDocumentoFiscalNFeReferencia> read fdocumentoFiscalNFeReferencia write fdocumentoFiscalNFeReferencia;
  end;

  TDocumentoFiscalCFe = class
  private
    festabelecimento: TEstabelecimento;
    fnumero: Integer;
    fserie: int64;
    fchave: string;
    fchaveCancelamento: string;
    fstatus: integer;
    fxml: string;
    fxmlCancelamento: string;
    fformaDeEmissao: smallint;
    fsessao: integer;
    fmsgRetorno: string;
    fimagem: string;
  public
    property estabelecimento: TEstabelecimento read festabelecimento write festabelecimento;
    property numero: Integer read fnumero write fnumero;
    property serie: int64 read fserie write fserie;
    property chave: string read fchave write fchave;
    property chaveCancelamento: string read fchaveCancelamento write fchaveCancelamento;
    property status: integer read fstatus write fstatus;
    property xml: string read fxml write fxml;
    property xmlCancelamento: string read fxmlCancelamento write fxmlCancelamento;
    property formaDeEmissao: smallint read fformaDeEmissao write fformaDeEmissao;
    property sessao: integer read fsessao write fsessao;
    property msgRetorno: string read fmsgRetorno write fmsgRetorno;
    property imagem: string read fimagem write fimagem;
  end;

  TDocumentoFiscalPagamento = class
  private
    festabelecimento: TEstabelecimento;
    fforma: TForma;
    fformaIndicador: string;
    fvalor: currency;
    ftroco: currency;
    fcartaoIntegracao: SmallInt;
    fcartaoCredenciadora: string;
    fcartaoBandeira: string;
    fcartaoAutorizacao: string;
    fcartaoCNPJ: string;
    fcartaoParceiro: TParceiro;
  public
    property estabelecimento: TEstabelecimento read festabelecimento write festabelecimento;
    property forma: TForma read fforma write fforma;
    property formaIndicador: string read fformaIndicador write fformaIndicador;
    property valor: currency read fvalor write fvalor;
    property troco: currency read ftroco write ftroco;
    property cartaoIntegracao: SmallInt read fcartaoIntegracao write fcartaoIntegracao;
    property cartaoCredenciadora: string read fcartaoCredenciadora write fcartaoCredenciadora;
    property cartaoBandeira: string read fcartaoBandeira write fcartaoBandeira;
    property cartaoAutorizacao: string read fcartaoAutorizacao write fcartaoAutorizacao;
    property cartaoCNPJ: string read fcartaoCNPJ write fcartaoCNPJ;
    property cartaoParceiro: TParceiro read fcartaoParceiro write fcartaoParceiro;
  end;

  TDocumentoFiscalCobranca = class
  private
    festabelecimento: TEstabelecimento;
    fduplicata: Smallint;
    fvencimento: TDate;
    fvalor: Currency;
  public
    property estabelecimento: TEstabelecimento read festabelecimento write festabelecimento;
    property duplicata: Smallint read fduplicata write fduplicata;
    property vencimento: TDate read fvencimento write fvencimento;
    property valor: Currency read fvalor write fvalor;
  end;

  TDocumentoFiscalItem = class
  private
    festabelecimento: TEstabelecimento;
    foperacao: TOperacao;
    fsequencial: Integer;
    fitem: TItem;
    funidade: TUnidade;
    fconta: TConta;
    fvalor: Currency;
    fquantidade: Currency;
    fsubtotal: Currency;
    fdesconto: Currency;
    ffrete: Currency;
    ffreteICMS: Currency;
    fseguro: Currency;
    foutrasDespesas: Currency;
    ftotal: Currency;
    ftotalImpostoAproximado: Currency;
    forigemDaMercadoria: TOrigemDaMercadoria;
    fncm: TNCM;
    fcodigoBeneficioFiscal: string;
    fcest: TCEST;
    fcfop: TCFOP;
    fpesoBruto: Currency;
    fpesoLiquido: Currency;
    fcstICMS: TCSTICMS;
    ficmsReducaoBase: Currency;
    ficmsBC: Currency;
    ficmsAliquota: Currency;
    ficmsValor: Currency;
    ficmsSTReducaoBase: Currency;
    ficmsModalidadeDeCalculoMVA: String;
    ficmsAliquotaMVA: Currency;
    ficmsSTBC: Currency;
    ficmsSTAliquota: Currency;
    ficmsSTValor: Currency;
    ffcpSTBC: Currency;
    ffcpSTAliquota: Currency;
    ffcpSTValor: Currency;
    fsimplesAliquotaDeCredito: Currency;
    fsimplesICMSAproveitado: Currency;
    fcstIPI: TCSTIPI;
    fipiCNPJProdutor: string;
    fipiCodigoEnquadramento: string;
    fipiSeloDeControle: string;
    fipiQuantidadeDoSelo: smallint;
    fipiDevolucaoPercentualMercadoria: Currency;
    fipiDevolucaoValorDevolvido: Currency;
    fipiBC: Currency;
    fipiAliquota: Currency;
    fipiValor: Currency;
    fcstPIS: TCSTPISCOFINS;
    fpisBC: Currency;
    fpisAliquota: Currency;
    fpisValor: Currency;
    fpisSTBC: Currency;
    fpisSTAliquota: Currency;
    fpisSTValor: Currency;
    fpisSTCompoeValorDeProduto: boolean;
    fcstCOFINS: TCSTPISCOFINS;
    fcofinsBC: Currency;
    fcofinsAliquota: Currency;
    fcofinsValor: Currency;
    fcofinsSTBC: Currency;
    fcofinsSTAliquota: Currency;
    fcofinsSTValor: Currency;
    fcofinsSTCompoeValorDeProduto: boolean;
    fiiBC: Currency;
    fiiAliquota: Currency;
    fiiValor: Currency;
    fiiDespesasAduaneiras: Currency;
    fiiIOF: Currency;
    ffcpPercentualUFDestino: Currency;
    ffcpBCUFDestinatario: Currency;
    ffcpAliquotaInternaUFDestinatario: Currency;
    ffcpValorICMSUFDestinatario: Currency;
    ffcpValorICMSUFRemetente: Currency;
    ffcpValorICMSRelativoFCPUDDestino: Currency;
    fpedidoNumero: Integer;
    fpedidoItem: Integer;
    finformacoesAdicionais: string;
  public
    property estabelecimento: TEstabelecimento read festabelecimento write festabelecimento;
    property operacao: TOperacao read foperacao write foperacao;
    property sequencial: Integer read fsequencial write fsequencial;
    property item: TItem read fitem write fitem;
    property unidade: TUnidade read funidade write funidade;
    property conta: TConta read fconta write fconta;
    property valor: Currency read fvalor write fvalor;
    property quantidade: Currency read fquantidade write fquantidade;
    property subtotal: Currency read fsubtotal write fsubtotal;
    property desconto: Currency read fdesconto write fdesconto;
    property frete: Currency read ffrete write ffrete;
    property freteICMS: Currency read ffreteICMS write ffreteICMS;
    property seguro: Currency read fseguro write fseguro;
    property outrasDespesas: Currency read foutrasDespesas write foutrasDespesas;
    property total: Currency read ftotal write ftotal;
    property totalImpostoAproximado: Currency read ftotalImpostoAproximado write ftotalImpostoAproximado;
    property origemDaMercadoria: TOrigemDaMercadoria read forigemDaMercadoria write forigemDaMercadoria;
    property ncm: TNCM read fncm write fncm;
    property codigoBeneficioFiscal: string read fcodigoBeneficioFiscal write fcodigoBeneficioFiscal;
    property cest: TCEST read fcest write fcest;
    property cfop: TCFOP read fcfop write fcfop;
    property pesoBruto: Currency read fpesoBruto write fpesoBruto;
    property pesoLiquido: Currency read fpesoLiquido write fpesoLiquido;
    property cstICMS: TCSTICMS read fcstICMS write fcstICMS;
    property icmsReducaoBase: Currency read ficmsReducaoBase write ficmsReducaoBase;
    property icmsBC: Currency read ficmsBC write ficmsBC;
    property icmsAliquota: Currency read ficmsAliquota write ficmsAliquota;
    property icmsValor: Currency read ficmsValor write ficmsValor;
    property icmsSTReducaoBase: Currency read ficmsSTReducaoBase write ficmsSTReducaoBase;
    property icmsModalidadeDeCalculoMVA: string read ficmsModalidadeDeCalculoMVA write ficmsModalidadeDeCalculoMVA;
    property icmsAliquotaMVA: Currency read ficmsAliquotaMVA write ficmsAliquotaMVA;
    property icmsSTBC: Currency read ficmsSTBC write ficmsSTBC;
    property icmsSTAliquota: Currency read ficmsSTAliquota write ficmsSTAliquota;
    property icmsSTValor: Currency read ficmsSTValor write ficmsSTValor;
    property fcpSTBC: Currency read ffcpSTBC write ffcpSTBC;
    property fcpSTAliquota: Currency read ffcpSTAliquota write ffcpSTAliquota;
    property fcpSTValor: Currency read ffcpSTValor write ffcpSTValor;
    property simplesAliquotaDeCredito: Currency read fsimplesAliquotaDeCredito write fsimplesAliquotaDeCredito;
    property simplesICMSAproveitado: Currency read fsimplesICMSAproveitado write fsimplesICMSAproveitado;
    property cstIPI: TCSTIPI read fcstIPI write fcstIPI;
    property ipiCNPJProdutor: string read fipiCNPJProdutor write fipiCNPJProdutor;
    property ipiCodigoEnquadramento: string read fipiCodigoEnquadramento write fipiCodigoEnquadramento;
    property ipiSeloDeControle: string read fipiSeloDeControle write fipiSeloDeControle;
    property ipiQuantidadeDoSelo: smallint read fipiQuantidadeDoSelo write fipiQuantidadeDoSelo;
    property ipiDevolucaoPercentualMercadoria: Currency read fipiDevolucaoPercentualMercadoria write fipiDevolucaoPercentualMercadoria;
    property ipiDevolucaoValorDevolvido: Currency read fipiDevolucaoValorDevolvido write fipiDevolucaoValorDevolvido;
    property ipiBC: Currency read fipiBC write fipiBC;
    property ipiAliquota: Currency read fipiAliquota write fipiAliquota;
    property ipiValor: Currency read fipiValor write fipiValor;
    property cstPIS: TCSTPISCOFINS read fcstPIS write fcstPIS;
    property pisBC: Currency read fpisBC write fpisBC;
    property pisAliquota: Currency read fpisAliquota write fpisAliquota;
    property pisValor: Currency read fpisValor write fpisValor;
    property pisSTBC: Currency read fpisSTBC write fpisSTBC;
    property pisSTAliquota: Currency read fpisSTAliquota write fpisSTAliquota;
    property pisSTValor: Currency read fpisSTValor write fpisSTValor;
    property pisSTCompoeValorDeProduto: boolean read fpisSTCompoeValorDeProduto write fpisSTCompoeValorDeProduto;
    property cstCOFINS: TCSTPISCOFINS read fcstCOFINS write fcstCOFINS;
    property cofinsBC: Currency read fcofinsBC write fcofinsBC;
    property cofinsAliquota: Currency read fcofinsAliquota write fcofinsAliquota;
    property cofinsValor: Currency read fcofinsValor write fcofinsValor;
    property cofinsSTBC: Currency read fcofinsSTBC write fcofinsSTBC;
    property cofinsSTAliquota: Currency read fcofinsSTAliquota write fcofinsSTAliquota;
    property cofinsSTValor: Currency read fcofinsSTValor write fcofinsSTValor;
    property cofinsSTCompoeValorDeProduto: boolean read fcofinsSTCompoeValorDeProduto write fcofinsSTCompoeValorDeProduto;
    property iiBC: Currency read fiiBC write fiiBC;
    property iiAliquota: Currency read fiiAliquota write fiiAliquota;
    property iiValor: Currency read fiiValor write fiiValor;
    property iiDespesasAduaneiras: Currency read fiiDespesasAduaneiras write fiiDespesasAduaneiras;
    property iiIOF: Currency read fiiIOF write fiiIOF;
    property fcpPercentualUFDestino: Currency read ffcpPercentualUFDestino write ffcpPercentualUFDestino;
    property fcpBCUFDestinatario: Currency read ffcpBCUFDestinatario write ffcpBCUFDestinatario;
    property fcpAliquotaInternaUFDestinatario: Currency read ffcpAliquotaInternaUFDestinatario write ffcpAliquotaInternaUFDestinatario;
    property fcpValorICMSUFDestinatario: Currency read ffcpValorICMSUFDestinatario write ffcpValorICMSUFDestinatario;
    property fcpValorICMSUFRemetente: Currency read ffcpValorICMSUFRemetente write ffcpValorICMSUFRemetente;
    property fcpValorICMSRelativoFCPUDDestino: Currency read ffcpValorICMSRelativoFCPUDDestino write ffcpValorICMSRelativoFCPUDDestino;
    property pedidoNumero: Integer read fpedidoNumero write fpedidoNumero;
    property pedidoItem: Integer read fpedidoItem write fpedidoItem;
    property informacoesAdicionais: string read finformacoesAdicionais write finformacoesAdicionais;
  end;

type
  TDocumentoFiscal = class
  private
    fempresa: TEmpresa;
    festabelecimento: TEstabelecimentoC;
    fsituacao: SmallInt;
    femissao: TDateTime;
    fsaida: TDateTime;
    fambiente: Integer;
    fmodelo: string;
    fparceiro: TParceiroC;
    fhistorico: THistorico;
    fmoeda: TMoeda;
    fcnpjInformado: string;
    fcpfInformado: string;
    fsubtotal: Currency;
    fdesconto: Currency;
    ffrete: Currency;
    foutrasDespesas: Currency;
    ftotal: Currency;
    ficmsBC: Currency;
    ficmsValor: Currency;
    ficmsSTBC: Currency;
    ficmsSTValor: Currency;
    ffcpSTBC: Currency;
    ffcpSTValor: Currency;
    fipiBC: Currency;
    fipiValor: Currency;
    fpisBC: Currency;
    fpisValor: Currency;
    fpisSTBC: Currency;
    fpisSTValor: Currency;
    fcofinsBC: Currency;
    fcofinsValor: Currency;
    fcofinsSTBC: Currency;
    fcofinsSTValor: Currency;
    fiiBC: Currency;
    fiiValor: Currency;
    fissBC: Currency;
    fissValor: Currency;
    finssValor: Currency;
    fObjetivo: Integer;
    femail: String;
    fimagem: AnsiString;
    fdocumentoFiscalCFe: TDocumentoFiscalCFe;
    fdocumentoFiscalNFe: TDocumentoFiscalNFe;
    fdocumentoFiscalItens: TArray<TDocumentoFiscalItem>;
    fdocumentoFiscalPagamentos: TArray<TDocumentoFiscalPagamento>;
    fdocumentoFiscalCobrancas: TArray<TDocumentoFiscalCobranca>;
    freferencia: string;
    freferenciaid: string;
    freferenciaClasse: Smallint;
    fdocumentoFiscalSerie: TDocumentoFiscalSerie;
  public
    constructor Create;
    destructor Destroy;
    property empresa: TEmpresa read fempresa write fempresa;
    property estabelecimento: TEstabelecimentoC read festabelecimento write festabelecimento;
    property situacao: SmallInt read fsituacao write fsituacao;
    property emissao: TDateTime read femissao write femissao;
    property saida: TDateTime read fsaida write fsaida;
    property ambiente: Integer read fambiente write fambiente;
    property modelo: string read fmodelo write fmodelo;
    property moeda: TMoeda read fmoeda write fmoeda;
    property parceiro: TParceiroC read fparceiro write fparceiro;
    property historico: THistorico read fhistorico write fhistorico;
    property cpfInformado: string read fcpfInformado write fcpfInformado;
    property cnpjInformado: string read fcnpjInformado write fcnpjInformado;
    property subtotal: Currency read fsubtotal write fsubtotal;
    property desconto: Currency read fdesconto write fdesconto;
    property frete: Currency read ffrete write ffrete;
    property outrasDespesas: Currency read foutrasDespesas write foutrasDespesas;
    property total: Currency read ftotal write ftotal;
    property icmsBC: Currency read ficmsBC write ficmsBC;
    property icmsValor: Currency read ficmsValor write ficmsValor;
    property icmsSTBC: Currency read ficmsSTBC write ficmsSTBC;
    property icmsSTValor: Currency read ficmsSTValor write ficmsSTValor;
    property fcpSTBC: Currency read ffcpSTBC write ffcpSTBC;
    property fcpSTValor: Currency read ffcpSTValor write ffcpSTValor;
    property ipiBC: Currency read fipiBC write fipiBC;
    property ipiValor: Currency read fipiValor write fipiValor;
    property pisBC: Currency read fpisBC write fpisBC;
    property pisValor: Currency read fpisValor write fpisValor;
    property pisSTBC: Currency read fpisSTBC write fpisSTBC;
    property pisSTValor: Currency read fpisSTValor write fpisSTValor;
    property cofinsBC: Currency read fcofinsBC write fcofinsBC;
    property cofinsValor: Currency read fcofinsValor write fcofinsValor;
    property cofinsSTBC: Currency read fcofinsSTBC write fcofinsSTBC;
    property cofinsSTValor: Currency read fcofinsSTValor write fcofinsSTValor;
    property iiBC: Currency read fiiBC write fiiBC;
    property iiValor: Currency read fiiValor write fiiValor;
    property issBC: Currency read fissBC write fissBC;
    property issValor: Currency read fissValor write fissValor;
    property objetivo: Integer read fobjetivo write fobjetivo default 0;
    property email: string read femail write femail;
    property imagem: AnsiString read fimagem write fimagem;
    property inssValor: Currency read finssValor write finssValor;
    property documentoFiscalNFe: TDocumentoFiscalNFe read fdocumentoFiscalNFe write fdocumentoFiscalNFe;
    property documentoFiscalCFe: TDocumentoFiscalCFe read fdocumentoFiscalCFe write fdocumentoFiscalCFe;
    property documentoFiscalItens: TArray<TDocumentoFiscalItem> read fdocumentoFiscalItens write fdocumentoFiscalItens;
    property documentoFiscalPagamentos: TArray<TDocumentoFiscalPagamento> read fdocumentoFiscalPagamentos write fdocumentoFiscalPagamentos;
    property documentoFiscalCobrancas: TArray<TDocumentoFiscalCobranca> read fdocumentoFiscalCobrancas write fdocumentoFiscalCobrancas;
    property referencia: string read freferencia write freferencia;
    property referenciaId: string read freferenciaId write freferenciaId;
    property referenciaClasse: SmallInt read freferenciaClasse write freferenciaClasse;
    property documentoFiscalSerie: TDocumentoFiscalSerie read fdocumentoFiscalSerie write fdocumentoFiscalSerie;
  end;
  TDocumentosFiscais = TArray<TDocumentoFiscal>;

implementation

constructor TDocumentoFiscal.Create;
begin
  inherited Create();
  fdocumentoFiscalCFe := TDocumentoFiscalCFe.Create;
  fdocumentoFiscalNFe := TDocumentoFiscalNFe.Create;
end;

destructor TDocumentoFiscal.Destroy;
begin
  fdocumentoFiscalCFe.Free;
  fdocumentoFiscalNFe.Free;
end;

end.
