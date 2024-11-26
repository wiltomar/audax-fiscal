unit Model.Fortes.RegistroPAR;

interface

uses Fortes.IRegistro;

type
  TRegistroPAR = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigo: Integer;
    FNome: string;
    FUF: string;
    FCNPJCPF: string;
    FInscricaoEstadual: boolean;
    FInscricaoMunicipal: boolean;
    FInformaISSDigital: boolean;
    FInformaDIEF: boolean;
    FInformaDIC: boolean;
    FInformaDEMMS: boolean;
    FOrgaoPublico: boolean;
    FInformaLivroEletronico: boolean;
    FFornecedorDeProdPrimario: boolean;
    FSociedadeSimples: boolean;
    FTipoDeLogradouro: SmallInt;
    FLogradouro: string;
    FNumero: integer;
    FComplemento: string;
    FTipodeBairro: SmallInt;
    FBairro: string;
    FCEP: integer;
    FMunicipio: integer;
    FDDD: integer;
    FTelefone: integer;
    FSuframa: string;
    FSubstitutoISS: boolean;
    FContaRemetentePrestador: string;
    FContaDestinatárioTomador: string;
    FPais: integer;
    FExterior: boolean;
    FIndicadorDoICMS: SmallInt;
    FEmail: string;
    FHospitais: boolean;
    FAdministradora: boolean;
    FCNAE21: string;
    FCPRB: boolean;
    FSituaçaoTributária: boolean;
    FProdutorRural: boolean;
    FIndicativoDaAquisicao: SmallInt;
    FIndicativoDeNIF: SmallInt;
    FNdeIdentificaçãoFiscal: double;
    FFormaDeTributacao: SmallInt;
  public
    constructor Create;
    function GerarLinha: string;

    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Codigo: Integer read FCodigo write FCodigo;
    property Nome: string read FNome write FNome;
    property UF: string read FUF write FUF;
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property InscricaoEstadual: boolean read FInscricaoEstadual write FInscricaoEstadual;
    property InscricaoMunicipal: boolean read FInscricaoMunicipal write FInscricaoMunicipal;
    property InformaISSDigital: boolean read FInformaISSDigital write FInformaISSDigital;
    property InformaDIEF : boolean read FInformaDIEF write FInformaDIEF;
    property InformaDIC: boolean read FInformaDIC write FInformaDIC;
    property InformaDEMMS: boolean read FInformaDEMMS write FInformaDEMMS;
    property OrgaoPublico: boolean read FOrgaoPublico write FOrgaoPublico;
    property InformaLivroEletronico: boolean read FInformaLivroEletronico write FInformaLivroEletronico;
    property FornecedorDeProdPrimario: boolean read FFornecedorDeProdPrimario write FFornecedorDeProdPrimario;
    property SociedadeSimples: boolean read FSociedadeSimples write FSociedadeSimples;
    property TipoDeLogradouro: SmallInt read FTipoDeLogradouro write FTipoDeLogradouro;
    property Logradouro: string read FLogradouro write FLogradouro;
    property Numero: integer read FNumero write FNumero;
    property Complemento: string read FComplemento write FComplemento;
    property TipodeBairro: SmallInt read FTipodeBairro write FTipodeBairro;
    property Bairro: string read FBairro write FBairro;
    property CEP: integer read FCEP write FCEP;
    property Municipio: integer read FMunicipio write FMunicipio;
    property DDD: integer read FDDD write FDDD;
    property Telefone: integer read FTelefone write FTelefone;
    property Suframa: string read FSuframa write FSuframa;
    property SubstitutoISS: boolean read FSubstitutoISS write FSubstitutoISS;
    property ContaRemetentePrestador: string read FContaRemetentePrestador write FContaRemetentePrestador;
    property ContaDestinatárioTomador: string read FContaDestinatárioTomador write FContaDestinatárioTomador;
    property Pais: integer read FPais write FPais;
    property Exterior: boolean read FExterior write FExterior;
    property IndicadorDoICMS: SmallInt read FIndicadorDoICMS write FIndicadorDoICMS;
    property Email: string read FEmail write FEmail;
    property Hospitais: boolean read FHospitais write FHospitais;
    property Administradora: boolean read FAdministradora write FAdministradora;
    property CNAE21: string read FCNAE21 write FCNAE21;
    property CPRB: boolean read FCPRB write FCPRB;
    property SituaçaoTributária: boolean read FSituaçaoTributária write FSituaçaoTributária;
    property ProdutorRural: boolean read FProdutorRural write FProdutorRural;
    property IndicativoDaAquisicao: SmallInt read FIndicativoDaAquisicao write FIndicativoDaAquisicao;
    property IndicativoDeNIF: SmallInt read FIndicativoDeNIF write FIndicativoDeNIF;
    property NdeIdentificaçãoFiscal: double read FNdeIdentificaçãoFiscal write FNdeIdentificaçãoFiscal;
    property FormaDeTributacao: SmallInt read FFormaDeTributacao write FFormaDeTributacao;


  end;

implementation

uses
  SysUtils;

constructor TRegistroPAR.Create;
begin
  FTipoRegistro := 'PAR';
end;

function TRegistroPAR.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s', [
    FTipoRegistro,
    FCodigo,
    FNome,
    FUF,
    FCNPJCPF,
    FInscricaoEstadual,
    FInscricaoMunicipal,
    FInformaISSDigital,
    FInformaDIEF,
    FInformaDIC,
    FInformaDEMMS,
    FOrgaoPublico,
    FInformaLivroEletronico,
    FFornecedorDeProdPrimario,
    FSociedadeSimples,
    FTipoDeLogradouro,
    FLogradouro,
    FNumero,
    FComplemento,
    FTipodeBairro,
    FBairro,
    FCEP,
    FMunicipio,
    FDDD,
    FTelefone,
    FSuframa,
    FSubstitutoISS,
    FContaRemetentePrestador,
    FContaDestinatárioTomador,
    FPais,
    FExterior,
    FIndicadorDoICMS,
    FEmail,
    FHospitais,
    FAdministradora,
    FCNAE21,
    FCPRB,
    FSituaçaoTributária,
    FProdutorRural,
    FIndicativoDaAquisicao,
    FIndicativoDeNIF,
    FNdeIdentificaçãoFiscal,
    FFormaDeTributacao
  ]);
end;

end.

