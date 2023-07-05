unit Model.Config;

interface

uses
  Classes;

type
  TEmitente = class;
  TCertificadoNFe = class;
  TEmail = class;
  TSwHouseCFe = class;
  TArquivosCFe = class;
  TCFe = class;
  TSSLNFe = class;
  TGeralNFe = class;
  TWebServiceNFe = class;
  TArquivosNFe = class;
  TNFe = class;
  TNFSe = class;

  TConfig = class
  private
    fporta: smallint;
    fdaemon: smallint;
    fambienteseguro: boolean;
    femitente: TEmitente;
    fcfe: TCFe;
    fnfe: TNFe;
    fnfse: TNFSe;
  public
    constructor Create(Owner: TObject);
    destructor Destroy;
    property porta: smallint read fporta write fporta;
    property daemon: smallint read fdaemon write fdaemon;
    property ambienteseguro: boolean read fambienteseguro write fambienteseguro;
    property emitente: TEmitente read femitente write femitente;
    property cfe: TCFe read fcfe write fcfe;
    property nfe: TNFe read fnfe write fnfe;
    property nfse: TNFSe read fnfse write fnfse;
  end;

  TEmitente = class
  private
    fcnpj: string;
    fie: string;
    fim: string;
    fregimeicms: smallint;
    fregimeiss: smallint;
    findicadorderateio: smallint;
    fcertificado: TCertificadoNFe;
    femail: TEmail;
  public
    property cnpj: string read fcnpj write fcnpj;
    property ie: string read fie write fie;
    property im: string read fim write fim;
    property regimeicms: smallint read fregimeicms write fregimeicms;
    property regimeiss: smallint read fregimeiss write fregimeiss;
    property indicadorderateio: smallint read findicadorderateio write findicadorderateio;
    property certificado: TCertificadoNFe read fcertificado write fcertificado;
    property email: TEmail read femail write femail;
  end;

  TCertificadoNFe = class
  private
    fsenhadocertificado: string;
    fcaminhoraiz: string;
    fcaminhopfx: string;
    fnumerodeserie: string;
    furl: string;
  public
    property url: string read furl write furl;
    property caminhoraiz: string read fcaminhoraiz write fcaminhoraiz;
    property caminhopfx: string read fcaminhopfx write fcaminhopfx;
    property senhadocertificado: string read fsenhadocertificado write fsenhadocertificado;
    property numerodeserie: string read fnumerodeserie write fnumerodeserie;
  end;

  TEmail = class
  private
    fservidor: string;
    fsenha: string;
    fusuario: string;
    fporta: string;
    forigem: string;
    fusassl: boolean;
    fusatls: boolean;
    fconfirmaleitura: boolean;
    fusathread: boolean;
    fremetente: string;
  public
    property servidor: string read fservidor write fservidor;
    property porta: string read fporta write fporta;
    property usuario: string read fusuario write fusuario;
    property senha: string read fsenha write fsenha;
    property origem: string read forigem write forigem;
    property usassl: boolean read fusassl write fusassl;
    property usatls: boolean read fusatls write fusatls;
    property confirmaleitura: boolean read fconfirmaleitura write fconfirmaleitura;
    property usathread: boolean read fusathread write fusathread;
    property remetente: string read fremetente write fremetente;
  end;

  TSwHouseCFe = class
  private
    fcnpj: string;
    fassinatura: string;
  public
    property cnpj: string read fcnpj write fcnpj;
    property assinatura: string read fassinatura write fassinatura;
  end;

  TArquivosCFe = class
  private
    fpathvenda: string;
    fpathenvio: string;
    fpathcancelamento: string;
    fsepararpormes: boolean;
    fsepararpormodelo: boolean;
    fsalvarcfe: boolean;
    fsepararporano: boolean;
    fsepararpordia: boolean;
    fsalvarenvio: boolean;
    fsalvarcancelamento: boolean;
    fsepararporcnpj: boolean;
  public
    property pathvenda: string read fpathvenda write fpathvenda;
    property pathenvio: string read fpathenvio write fpathenvio;
    property pathcancelamento: string read fpathcancelamento write fpathcancelamento;
    property salvarcfe: boolean read fsalvarcfe write fsalvarcfe default true;
    property salvarcancelamento: boolean read fsalvarcancelamento write fsalvarcancelamento default true;
    property salvarenvio: boolean read fsalvarenvio write fsalvarenvio default true;
    property separarporcnpj: boolean read fsepararporcnpj write fsepararporcnpj default true;
    property separarpormodelo: boolean read fsepararpormodelo write fsepararpormodelo default true;
    property separarpordia: boolean read fsepararpordia write fsepararpordia default false;
    property separarpormes: boolean read fsepararpormes write fsepararpormes default true;
    property separarporano: boolean read fsepararporano write fsepararporano default true;
  end;

  TCFe = class
  private
    fpaginadecodigo: smallint;
    fversaolayout: real;
    futf: boolean;
    fcodigodeativacao: string;
    fmodelo: smallint;
    fcaixa: smallint;
    farquivolog: string;
    fambiente: smallint;
    fschemas: string;
    fcaminhodll: string;
    fswhouse: TSwHouseCFe;
    farquivos: TArquivosCFe;
  public
    property schemas: string read fschemas write fschemas;
    property paginadecodigo: smallint read fpaginadecodigo write fpaginadecodigo default 850;
    property utf: boolean read futf write futf default true;
    property versaolayout: real read fversaolayout write fversaolayout;
    property modelo: smallint read fmodelo write fmodelo;
    property codigodeativacao: string read fcodigodeativacao write fcodigodeativacao;
    property arquivolog: string read farquivolog write farquivolog;
    property caminhodll: string read fcaminhodll write fcaminhodll;
    property caixa: smallint read fcaixa write fcaixa default 1;
    property ambiente: smallint read fambiente write fambiente default 2;
    property swhouse: TSwHouseCFe read fswhouse write fswhouse;
    property arquivos: TArquivosCFe read farquivos write farquivos;
  end;

  TSSLNFe = class
  private
    fhttplib: smallint;
    ftypelib: smallint;
    flib: smallint;
    fxmlsignlib: smallint;
    fcryptlib: smallint;
  public
    property typelib: smallint read ftypelib write ftypelib default 5;
    property lib: smallint read flib write flib default 4;
    property cryptlib: smallint read fcryptlib write fcryptlib default 3;
    property httplib: smallint read fhttplib write fhttplib default 2;
    property xmlsignlib: smallint read fxmlsignlib write fxmlsignlib default 4;
  end;

  TGeralNFe = class
  private
    fexibirerrosschema: boolean;
    fatualizarxml: boolean;
    fsalvar: boolean;
    fformaemissao: smallint;
    fretiraracentos: boolean;
    fversaodf: string;
  public
    property salvar: boolean read fsalvar write fsalvar default true;
    property retiraracentos: boolean read fretiraracentos write fretiraracentos default true;
    property atualizarxml: boolean read fatualizarxml write fatualizarxml default true;
    property exibirerroschema: boolean read fexibirerrosschema write fexibirerrosschema default true;
    property formaemissao: smallint read fformaemissao write fformaemissao default 0;
    property versaodf: string read fversaodf write fversaodf;
  end;

  TWebServiceNFe = class
  private
    fsalvar: boolean;
    fajustaaguardaconsultaret: boolean;
    fintervalotentativas: integer;
    fproxyport: string;
    fproxypass: string;
    fproxyhost: string;
    ftentativas: smallint;
    fproxyuser: string;
    ftimeout: smallint;
    fvisualizar: boolean;
    faguardarconsultaret: integer;
  public
    property visualizar: boolean read fvisualizar write fvisualizar default false;
    property salvar: boolean read fsalvar write fsalvar default true;
    property ajustaaguardaconsultaret: boolean read fajustaaguardaconsultaret write fajustaaguardaconsultaret default true;
    property aguardarconsultaret: integer read faguardarconsultaret write faguardarconsultaret default 30000;
    property tentativas: smallint read ftentativas write ftentativas default 3;
    property intervalotentativas: integer read fintervalotentativas write fintervalotentativas default 10000;
    property timeout: smallint read ftimeout write ftimeout default 60;
    property proxyhost: string read fproxyhost write fproxyhost;
    property proxyport: string read fproxyport write fproxyport;
    property proxyuser: string read fproxyuser write fproxyuser;
    property proxypass: string read fproxypass write fproxypass;
  end;

  TArquivosNFe = class
  private
    fsepararpormes: boolean;
    fsepararpormodelo: boolean;
    fadicionarliteral: boolean;
    fsalvar: boolean;
    fpathsalvar: string;
    fpathinu: string;
    femissaopathnfe: boolean;
    fpathschemas: string;
    fsepararporcnpj: boolean;
    fpathevento: string;
    fpathnfe: string;
    fsalvarevento: boolean;
  public
    property salvar: boolean read fsalvar write fsalvar default true;
    property separarpormes: boolean read fsepararpormes write fsepararpormes default true;
    property adicionarliteral: boolean read fadicionarliteral write fadicionarliteral default false;
    property emissaopathnfe: boolean read femissaopathnfe write femissaopathnfe default true;
    property salvarevento: boolean read fsalvarevento write fsalvarevento default true;
    property separarporcnpj: boolean read fsepararporcnpj write fsepararporcnpj default true;
    property separarpormodelo: boolean read fsepararpormodelo write fsepararpormodelo default true;
    property pathschemas: string read fpathschemas write fpathschemas;
    property pathnfe: string read fpathnfe write fpathnfe;
    property pathinu: string read fpathinu write fpathinu;
    property pathevento: string read fpathevento write fpathevento;
    property pathsalvar: string read fpathsalvar write fpathsalvar;
  end;

  TNFe = class
  private
    fssl: TSSLNFe;
    fgeral: TGeralNFe;
    fwebservice: TWebServiceNFe;
    farquivos: TArquivosNFe;
  public
    property ssl: TSSLNFe read fssl write fssl;
    property geral: TGeralNFe read fgeral write fgeral;
    property webservice: TWebServiceNFe read fwebservice write fwebservice;
    property arquivos: TArquivosNFe read farquivos write farquivos;
  end;

  TNFSe = class
  private
    fidcsc: string;
    fcsc: string;
  public
    property idcsc: string read fidcsc write fidcsc;
    property csc: string read fcsc write fcsc;
  end;

implementation

{ TConfig }

constructor TConfig.Create(Owner: TObject);
begin
  //
end;

destructor TConfig.Destroy;
begin
  //
end;

end.
