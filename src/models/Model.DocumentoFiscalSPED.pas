unit Model.DocumentoFiscalSPED;

interface

uses
  Model.Base, Model.Parceiro, Model.DocumentoFiscalSPEDItem;

type

  TDocumentofiscalSPEDcfe = class(TBaseR)
  private
    fsessao: integer;
    fserie: integer;
    fxmlCancelamento: string;
    fchaveCancelamento: string;
    fnumero: integer;
    fstatus: integer;
    fdanfe: string;
    fxml: string;
    fchave: string;
  public
    property numero: integer read fnumero write fnumero;
    property serie: integer read fserie write fserie;
    property chave: string read fchave write fchave;
    property chaveCancelamento: string read fchaveCancelamento write fchaveCancelamento;
    property status: integer read fstatus write fstatus;
    property sessao: integer read fsessao write fsessao;
    property xml: string read fxml write fxml;
    property danfe: string read fdanfe write fdanfe;
    property xmlCancelamento: string read fxmlCancelamento write fxmlCancelamento;
  end;

  TDocumentofiscalSPEDnfe = class(TBaseR)
  private
    fprotocolo: string;
    fserie: integer;
    fnumero: integer;
    fstatus: integer;
    fchave: string;

  public
    property numero: integer read fnumero write fnumero;
    property serie: integer read fserie write fserie;
    property chave: string read fchave write fchave;
    property status: integer read fstatus write fstatus;
    property protocolo: string read fprotocolo write fprotocolo;
  end;

  TDocumentoFiscal860CFe = class(TBaseR)
  private
    fserie: integer;
    fnumero: integer;
  public
    property serie: integer read fserie write fserie;
    property numero: integer read fnumero write fnumero;
  end;

  TDocumentofiscalSPED860 = class(TBaseR)
  private
    fmodelo: string;
    femissao: TDate;
    fdocumentofiscalcfe: TDocumentoFiscal860CFe;
  public
    property modelo: string read fmodelo write fmodelo;
    property emissao: TDate read femissao write femissao;
    property documentofiscalcfe: TDocumentoFiscal860CFe read fdocumentofiscalcfe write fdocumentofiscalcfe;
  end;

  TDocumentoFiscalSPED = class(TBaseR)
  private
    ffrete: Currency;
    fsaida: TDateTime;
    fipiValor: Currency;
    ficmsSTValor: Currency;
    fsituacao: smallint;
    ficmsBC: Currency;
    fdesconto: Currency;
    findicadorfrete: integer;
    fvalorpisst: Currency;
    fserie: string;
    fparceiro: TParceiro;
    fvalorcofinsst: Currency;
    fvalorseguro: Currency;
    fnumero: integer;
    ficmsSTBC: Currency;
    foutrasdespesas: Currency;
    ftotal: Currency;
    femissao: TDateTime;
    fvalormercadoria: Currency;
    fabatimentonaotributado: Currency;
    fvalorpis: Currency;
    fvalorcofins: Currency;
    fnatureza: integer;
    fchave: string;
    fmodelo: string;
    findicadorpagamento: integer;
    fdocumentofiscalitens: TDocumentoFiscalSPEDItens;
    fdocumentofiscalcfe: TDocumentofiscalSPEDcfe;
    fdocumentofiscalnfe: TDocumentofiscalSPEDnfe;
    fvaloricms: Currency;
    ficmsValor: Currency;
  public
    property natureza: integer read fnatureza write fnatureza;
    property parceiro: TParceiro read fparceiro write fparceiro;
    property modelo: string read fmodelo write fmodelo;
    property situacao: smallint read fsituacao write fsituacao;
    property serie: string read fserie write fserie;
    property numero: integer read fnumero write fnumero;
    property chave: string read fchave write fchave;
    property emissao: TDateTime read femissao write femissao;
    property saida: TDateTime read fsaida write fsaida;
    property total: Currency read ftotal write ftotal;
    property indicadorpagamento: integer read findicadorpagamento write findicadorpagamento;
    property desconto: Currency read fdesconto write fdesconto;
    property abatimentonaotributado: Currency read fabatimentonaotributado write fabatimentonaotributado;
    property valormercadoria: Currency read fvalormercadoria write fvalormercadoria;
    property indicadorfrete: integer read findicadorfrete write findicadorfrete;
    property frete: Currency read ffrete write ffrete;
    property valorseguro: Currency read fvalorseguro write fvalorseguro;
    property outrasdespesas: Currency read foutrasdespesas write foutrasdespesas;
    property icmsValor: Currency read ficmsValor write ficmsValor;
    property valoricms: Currency read fvaloricms write fvaloricms;
    property icmsSTBC: Currency read ficmsSTBC write ficmsSTBC;
    property icmsSTValor: Currency read ficmsSTValor write ficmsSTValor;
    property ipiValor: Currency read fipiValor write fipiValor;
    property valorpis: Currency read fvalorpis write fvalorpis;
    property valorcofins: Currency read fvalorcofins write fvalorcofins;
    property valorpisst: Currency read fvalorpisst write fvalorpisst;
    property valorcofinsst: Currency read fvalorcofinsst write fvalorcofinsst;
    property documentofiscalitens: TDocumentoFiscalSPEDItens read fdocumentofiscalitens write fdocumentofiscalitens;
    property documentofiscalcfe: TDocumentofiscalSPEDcfe read fdocumentofiscalcfe write fdocumentofiscalcfe;
    property documentofiscalnfe: TDocumentofiscalSPEDnfe read fdocumentofiscalnfe write fdocumentofiscalnfe;
  end;

implementation

end.

