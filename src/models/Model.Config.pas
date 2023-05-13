unit Model.Config;

interface

uses
  Classes, Rest.JSON;

type
  TConfig = class
  private
    fPorta: SmallInt;
    fDaemon: SmallInt;
    fAmbienteSeguro: Boolean;
  public
    property Porta: SmallInt read fPorta write fPorta;
    property Daemon: SmallInt read fDaemon write fDaemon;
    property AmbienteSeguro: Boolean read fAmbienteSeguro write FAmbienteSeguro;
  end;

  TEmitente = class
  private
    fCNPJ: String;
    fIE: String;
  public
    property CNPJ: String read fCNPJ write fCNPJ;
    property IE: String read fIE write fIE;
  end;

  TEmail = class
  private
    fServidor: string;
    fSenha: string;
    fUsuario: string;
    fPorta: SmallInt;
    fOrigem: string;
  public
    property Servidor: string read fServidor write fServidor;
    property Porta: SmallInt read fPorta write fPorta default 587;
    property Usuario: string read fUsuario write fUsuario;
    property Senha: string read fSenha write fSenha;
    property Origem: string read fOrigem write fOrigem;
  end;

  TConfigCFe = class
  private
    fPaginaDeCodigo: SmallInt;
    fVersaoLayout: Real;
    fUTF: Boolean;
    fCodigoDeAtivacao: Integer;
    fModelo: Smallint;
    fCaixa: SmallInt;
    fArquivoLog: string;
    fAmbiente: SmallInt;
    fSchemas: string;
    fCaminhoDLL: string;
  public
    property Schemas: string read fSchemas write fSchemas;
    property PaginaDeCodigo: SmallInt read fPaginaDeCodigo write fPaginaDeCodigo default 850;
    property UTF: Boolean read fUTF write fUTF default True;
    property VersaoLayout: Real read fVersaoLayout write fVersaoLayout;
    property Modelo: Smallint read fModelo write fModelo;
    property CodigoDeAtivacao: Integer read fCodigoDeAtivacao write fCodigoDeAtivacao;
    property ArquivoLog: string read fArquivoLog write fArquivoLog;
    property CaminhoDLL: string read fCaminhoDLL write fCaminhoDLL;
    property Caixa: SmallInt read fCaixa write fCaixa default 1;
    property Ambiente: SmallInt read fAmbiente write fAmbiente default 2;
  end;

  TSwHouseCFe = class
  private
    fCNPJ: string;
    fAssinatura: string;
  public
    property CNPJ: string read fCNPJ write fCNPJ;
    property Assinatura: string read fAssinatura write fAssinatura;
  end;

  TArquivosCFe = class
  private
    fSepararPorMes: Boolean;
    fSepararPorModelo: Boolean;
    fSalvarCFe: Boolean;
    fSepararPorAno: Boolean;
    fSepararPorDia: Boolean;
    fSalvarEnvio: Boolean;
    fSalvarCancelamento: Boolean;
    fSepararPorCNPJ: Boolean;
  public
    property SalvarCFe: Boolean read fSalvarCFe write fSalvarCFe default True;
    property SalvarCancelamento: Boolean read fSalvarCancelamento write fSalvarCancelamento default True;
    property SalvarEnvio: Boolean read fSalvarEnvio write fSalvarEnvio default True;
    property SepararPorCNPJ: Boolean read fSepararPorCNPJ write fSepararPorCNPJ default True;
    property SepararPorModelo: Boolean read fSepararPorModelo write fSepararPorModelo default True;
    property SepararPorDia: Boolean read fSepararPorDia write fSepararPorDia default False;
    property SepararPorMes: Boolean read fSepararPorMes write fSepararPorMes default True;
    property SepararPorAno: Boolean read fSepararPorAno write fSepararPorAno default True;
  end;

  TSSLNFe = class
  private
    fHttpLib: SmallInt;
    fTypeLib: SmallInt;
    fLib: SmallInt;
    fXmlSignLib: SmallInt;
    fCryptLib: SmallInt;
  public
    property TypeLib: SmallInt read fTypeLib write fTypeLib default 5;
    property Lib: SmallInt read fLib write fLib default 4;
    property CryptLib: SmallInt read fCryptLib write fCryptLib default 3;
    property HttpLib: SmallInt read fHttpLib write fHttpLib default 2;
    property XmlSignLib: SmallInt read fXmlSignLib write fXmlSignLib default 4;
  end;

  TGeralNFe = class
  private
    fExibirErrosSchema: Boolean;
    fAtualizarXML: Boolean;
    fSalvar: Boolean;
    fFormaEmissao: SmallInt;
    fRetirarAcentos: Boolean;
    fVersaoDF: Real;
  public
    property Salvar: Boolean read fSalvar write fSalvar default True;
    property RetirarAcentos: Boolean read fRetirarAcentos write fRetirarAcentos default True;
    property AtualizarXML: Boolean read fAtualizarXML write fAtualizarXML default True;
    property ExibirErroSchema: Boolean read fExibirErrosSchema write fExibirErrosSchema default True;
    property FormaEmissao: SmallInt read fFormaEmissao write fFormaEmissao default 0;
    property VersaoDF: Real read fVersaoDF write fVersaoDF;
  end;

  TWebServiceNFe = class
  private
    fSalvar: Boolean;
    fAjustaAguardaConsultaRet: Boolean;
    fIntervaloTentativas: Integer;
    fProxyPort: SmallInt;
    fProxyPass: String;
    fProxyHost: string;
    fTentativas: SmallInt;
    fProxyUser: String;
    fTimeOut: SmallInt;
    fVisualizar: Boolean;
    fAguardarConsultaRet: Integer;
  public
    property Visualizar: Boolean read fVisualizar write fVisualizar default False;
    property Salvar: Boolean read fSalvar write fSalvar default True;
    property AjustaAguardaConsultaRet: Boolean read fAjustaAguardaConsultaRet write fAjustaAguardaConsultaRet default True;
    property AguardarConsultaRet: Integer read fAguardarConsultaRet write fAguardarConsultaRet default 30000;
    property Tentativas: SmallInt read fTentativas write fTentativas default 3;
    property IntervaloTentativas: Integer read fIntervaloTentativas write fIntervaloTentativas default 10000;
    property TimeOut: SmallInt read fTimeOut write fTimeOut default 60;
    property ProxyHost: string read fProxyHost write fProxyHost;
    property ProxyPort: SmallInt read fProxyPort write fProxyPort;
    property ProxyUser: String read fProxyUser write fProxyUser;
    property ProxyPass: String read fProxyPass write fProxyPass;
  end;

  TArquivosNFe = class
  private
    fSepararPorMes: Boolean;
    fSepararPorModelo: Boolean;
    fAdicionarLiteral: Boolean;
    fSalvar: Boolean;
    fPathSalvar: string;
    fPathInu: string;
    fEmissaoPathNFe: Boolean;
    fPathSchemas: string;
    fSepararPorCNPJ: Boolean;
    fPathEvento: string;
    fPathNFe: string;
    fSalvarEvento: Boolean;
  public
    property Salvar: Boolean read fSalvar write fSalvar default True;
    property SepararPorMes: Boolean read fSepararPorMes write fSepararPorMes default True;
    property AdicionarLiteral: Boolean read fAdicionarLiteral write fAdicionarLiteral default False;
    property EmissaoPathNFe: Boolean read fEmissaoPathNFe write fEmissaoPathNFe default True;
    property SalvarEvento: Boolean read fSalvarEvento write fSalvarEvento default True;
    property SepararPorCNPJ: Boolean read fSepararPorCNPJ write fSepararPorCNPJ default True;
    property SepararPorModelo: Boolean read fSepararPorModelo write fSepararPorModelo default True;
    property PathSchemas: string read fPathSchemas write fPathSchemas;
    property PathNFe: string read fPathNFe write fPathNFe;
    property PathInu: string read fPathInu write fPathInu;
    property PathEvento: string read fPathEvento write fPathEvento;
    property PathSalvar: string read fPathSalvar write fPathSalvar;
  end;

  TIdentificacaoNFSe = class
  private
    fIdToken: SmallInt;
    fCSC: String;
  public
    property IdToken: SmallInt read fIdToken write fIdToken;
    property CSC: String read fCSC write fCSC;
  end;

  TCertificadoNFe = class
  private
    fSenhaDoCertificado: string;
    fCaminhoPFX: string;
    fNumeroDeSerie: string;
    fURL: string;
  public
    property URL: string read fURL write fURL;
    property CaminhoPFX: string read fCaminhoPFX write fCaminhoPFX;
    property SenhaDoCertificado: string read fSenhaDoCertificado write fSenhaDoCertificado;
    property NumeroDeSerie: string read fNumeroDeSerie write fNumeroDeSerie;
  end;

implementation

end.
