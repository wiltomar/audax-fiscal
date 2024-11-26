unit Model.DocumentoFiscalItemSPED;

interface

uses
  Model.Base, Model.Parceiro;

type

  TUnidade = class(TBaseR)
  private
    fcodigo: String;
    fnome: String;
    fsimbolo: String;
  public
    property codigo: String read fcodigo write fcodigo;
    property nome: String read fnome write fnome;
    property simbolo: String read fsimbolo write fsimbolo;
  end;

  TItem = class(TBaseR)
  private
    fcodigo: String;
    fnome: String;
    funidade: TUnidade;
  public
    property codigo: String read fcodigo write fcodigo;
    property nome: String read fnome write fnome;
    property unidade: TUnidade read funidade write funidade;
  end;

  TcstICMS = class(TBaseR)
  private
    fcodigo: String;
    fnome: String;
  public
    property codigo: String read fcodigo write fcodigo;
    property nome: String read fnome write fnome;
  end;

  TcFOP = class(TBaseR)
  private
    fcodigo: String;
    fnome: String;
  public
    property codigo: String read fcodigo write fcodigo;
    property nome: String read fnome write fnome;
  end;

  TcstPIS = class(TBaseR)
  private
    fcodigo: String;
    fnome: String;
  public
    property codigo: String read fcodigo write fcodigo;
    property nome: String read fnome write fnome;
  end;

  TcstCofins = class(TBaseR)
  private
    fcodigo: String;
    fnome: String;
  public
    property codigo: String read fcodigo write fcodigo;
    property nome: String read fnome write fnome;
  end;


  TcstIPI = class(TBaseR)
  private
    fcodigo: String;
    fnome: String;
  public
    property codigo: String read fcodigo write fcodigo;
    property nome: String read fnome write fnome;
  end;

  TDocumentoFiscalSPEDItem = class(TBaseR)
  private
    fsequencial: Integer;
    fitem: TItem;
    fvalor: currency;
    fquantidade: Currency;
    fsubtotal: Currency;
    fdesconto: Currency;
//    ffrete: currency;
//    ffreteICMS: Currency;
//    fseguro: Currency;
//    foutrasDespesas: Currency;
    ftotal: Currency;
//    ftotalImpostoAproximado: Currency;
//    fpesoBruto: Double;
//    fpesoLiquido: Double;
    ficmsBC: Currency;
    ficmsAliquota: Currency;
    ficmsValor: Currency;
    ficmsSTBC: Currency;
    ficmsSTValor: Currency;
//    fsimplesAliquotaDeCredito: Currency;
    ficmsSTAliquota: Currency;
//    fipiCodigoEnquadramento: String;
//    fsimplesICMSAproveitado: Currency;
//    fipiCNPJProdutor: String;
//    fipiDevolucaoValorDevolvido: Currency;
//    fipiDevolucaoPercentualMercadoria: Currency;
    fipiBC: Currency;
//    fipiQuantidadeDoSelo: Integer;
    fpisBC: Currency;
    fipiValor: Currency;
    fipiAliquota: Currency;
    fpisAliquota: Currency;
    fpisValor: Currency;
//    fcofinsBC: Currency;
//    fpisSTCompoeValorDeProduto: Currency;
//    fcofinsAliquota: Currency;
    fpisSTValor: Currency;
    fpisSTBC: Currency;
    fcofinsValor: Currency;
    fcstICMS: TcstICMS;
    fcfop: TcFOP;
    fcstpis: TcstPIS;
    fcstcofins: TcstCofins;
    fcstipi: TcstIPI;

//    fpisSTAliquota: Currency;
//    fcofinsSTCompoeValorDeProduto: Currency;
//    fcofinsSTValor: Currency;
//    fiiBC: Currency;
//    fiiAliquota: Currency;
//    fcofinsSTBC: Currency;
//    fcofinsSTAliquota: Currency;
//    ffcpAliquotaInternaUFDestinatario: Currency;
//    fiiDespesasAduaneiras: Currency;
//    ffcpValorICMSUFDestinatario: Currency;
//    ffcpBCUFDestinatario: Currency;
//    fiiValor: Currency;
//    ffcpPercentualUFDestino: Currency;
//    ffcpValorICMSUFRemetente: Currency;
//    fiiIOF: Currency;
//    ffcpValorICMSRelativoFCPUDDestino: Currency;
//    fpedidoNumero: Integer;
//    fpedidoItem: Integer;
//    finformacoesAdicionais: String;
//    fitem: TItem;
  public
    property sequencial: Integer read fsequencial write fsequencial;
    property valor: currency read fvalor write fvalor;
    property quantidade: Currency read fquantidade write fquantidade;
    property subtotal: Currency read fsubtotal write fsubtotal;
    property desconto: Currency read fdesconto write fdesconto;
//    property frete: currency read ffrete write ffrete;
//    property freteICMS: Currency read ffreteICMS write ffreteICMS;
//    property seguro: Currency read fseguro write fseguro;
//    property outrasDespesas: Currency read foutrasDespesas write foutrasDespesas;
    property total: Currency read ftotal write ftotal;
//    property totalImpostoAproximado: Currency read ftotalImpostoAproximado write ftotalImpostoAproximado;
//    property pesoBruto: Double read fpesoBruto write fpesoBruto; //ver double
//    property pesoLiquido: Double read fpesoLiquido write fpesoLiquido;
    property icmsBC: Currency read ficmsBC write ficmsBC;
    property icmsAliquota: Currency read ficmsAliquota write ficmsAliquota;
    property icmsValor: Currency read ficmsValor write ficmsValor;
    property icmsSTBC: Currency read ficmsSTBC write ficmsSTBC;
    property icmsSTAliquota: Currency read ficmsSTAliquota write ficmsSTAliquota;
    property icmsSTValor: Currency read ficmsSTValor write ficmsSTValor;
//    property simplesAliquotaDeCredito: Currency read fsimplesAliquotaDeCredito write fsimplesAliquotaDeCredito;
//    property simplesICMSAproveitado: Currency read fsimplesICMSAproveitado write fsimplesICMSAproveitado;
//    property ipiCodigoEnquadramento: String read fipiCodigoEnquadramento write fipiCodigoEnquadramento;
//    property ipiCNPJProdutor: String read fipiCNPJProdutor write fipiCNPJProdutor;
//    property ipiQuantidadeDoSelo: Integer read fipiQuantidadeDoSelo write fipiQuantidadeDoSelo;
//    property ipiDevolucaoPercentualMercadoria: Currency read fipiDevolucaoPercentualMercadoria write fipiDevolucaoPercentualMercadoria;
//    property ipiDevolucaoValorDevolvido: Currency read fipiDevolucaoValorDevolvido write fipiDevolucaoValorDevolvido;
    property ipiBC: Currency read fipiBC write fipiBC;
    property ipiAliquota: Currency read fipiAliquota write fipiAliquota;
    property ipiValor: Currency read fipiValor write fipiValor;
    property pisBC: Currency read fpisBC write fpisBC;
    property pisAliquota: Currency read fpisAliquota write fpisAliquota;
    property pisValor: Currency read fpisValor write fpisValor;
    property pisSTBC: Currency read fpisSTBC write fpisSTBC;
//    property pisSTAliquota: Currency read fpisSTAliquota write fpisSTAliquota;
    property pisSTValor: Currency read fpisSTValor write fpisSTValor;
//    property pisSTCompoeValorDeProduto: Currency read fpisSTCompoeValorDeProduto write fpisSTCompoeValorDeProduto;
//    property cofinsBC: Currency read fcofinsBC write fcofinsBC;
//    property cofinsAliquota: Currency read fcofinsAliquota write fcofinsAliquota;
    property cofinsValor: Currency read fcofinsValor write fcofinsValor;
//    property cofinsSTBC: Currency read fcofinsSTBC write fcofinsSTBC;
//    property cofinsSTAliquota: Currency read fcofinsSTAliquota write fcofinsSTAliquota;
//    property cofinsSTValor: Currency read fcofinsSTValor write fcofinsSTValor;
//    property cofinsSTCompoeValorDeProduto: Currency read fcofinsSTCompoeValorDeProduto write fcofinsSTCompoeValorDeProduto;
//    property iiBC: Currency read fiiBC write fiiBC;
//    property iiAliquota: Currency read fiiAliquota write fiiAliquota;
//    property iiValor: Currency read fiiValor write fiiValor;
//    property iiDespesasAduaneiras: Currency read fiiDespesasAduaneiras write fiiDespesasAduaneiras;
//    property iiIOF: Currency read fiiIOF write fiiIOF;
//    property fcpPercentualUFDestino: Currency read ffcpPercentualUFDestino write ffcpPercentualUFDestino;
//    property fcpBCUFDestinatario: Currency read ffcpBCUFDestinatario write ffcpBCUFDestinatario;
//    property fcpAliquotaInternaUFDestinatario: Currency read ffcpAliquotaInternaUFDestinatario write ffcpAliquotaInternaUFDestinatario;
//    property fcpValorICMSUFDestinatario: Currency read ffcpValorICMSUFDestinatario write ffcpValorICMSUFDestinatario;
//    property fcpValorICMSUFRemetente: Currency read ffcpValorICMSUFRemetente write ffcpValorICMSUFRemetente;
//    property fcpValorICMSRelativoFCPUDDestino: Currency read ffcpValorICMSRelativoFCPUDDestino write ffcpValorICMSRelativoFCPUDDestino;
//    property pedidoNumero: Integer read fpedidoNumero write fpedidoNumero;
//    property pedidoItem: Integer read fpedidoItem write fpedidoItem;
//    property informacoesAdicionais: String read finformacoesAdicionais write finformacoesAdicionais;
     property item: TItem read fitem write fitem;
     property csicms : Tcsticms read fcsticms write fcsticms;
     property cfop : TcFOP read fcfop write fcfop;
     property cstpis : TcstPIS read fcstpis write fcstpis;
     property cstcofins: TcstCofins read fcstcofins write fcstcofins;
     property cstipi: TcstIPI read fcstipi write fcstipi;

  end;
  TDocumentoFiscalSPEDItens = Array of TDocumentoFiscalSPEDItem;

implementation

end.

