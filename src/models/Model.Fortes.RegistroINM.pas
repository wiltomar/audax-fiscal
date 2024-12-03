unit Model.Fortes.RegistroINM;

interface

uses Fortes.IRegistro, System.StrUtils;

type
  TRegistroINM = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FValorDaOperacao: double;
    FUF: string;
    FCFOP: integer;
    FCFOPTransferencia: integer;
    FBaseCalculoICMS: double;
    FAliquotaDoICMS: double;
    FValorICMS: double;
    FIsentasDoICMS: double;
    FOutrasDoICMS: double;
    FBaseCalculoIPI: double;
    FValorIPI: double;
    FIsentasDoIPI: double;
    FOutrasDoIPI: double;
    FSubstituicaoICMS: boolean;
    FSubstituicaoIPI: boolean;
    FSubstituicaoCOFINS: boolean;
    FSubstituicaoPISPASEP: boolean;
    FCSTA: double;
    FCSTB: boolean;
    FCodigoDaSituacaoTributariaDoCSOSN: SmallInt;
    FCSOSN: integer;
    FCSTIPI: SmallInt;
    FCOFINSMonofasico: boolean;
    FPISMonofasico: boolean;
    FCalculaFecopDecreto: boolean;
    FAliqSubstDecreto: SmallInt;
    FBaseDeCalculoDoFCPNormal: double;
    FAliquotaDoFCPNormal: double;
    FValorDoFCPNormal: double;
    FBaseDeCalculoDoFCPSubstTribOuRetidoAntPorST: double;
    FAliquotaDoFCPSubstTribOuRetidoAntPorST: double;
    FValorDoFCPSubstTribOuRetidoAntPorST: double;
    FAliquotaDoICMSDiferimento: double;

  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property ValorDaOperacao: double read FValorDaOperacao write FValorDaOperacao;
    property UF: string read FUF write FUF;
    property CFOP: integer read FCFOP write FCFOP;
    property CFOPTransferencia: integer read FCFOPTransferencia write FCFOPTransferencia;
    property BaseCalculoICMS: double read FBaseCalculoICMS write FBaseCalculoICMS;
    property AliquotaDoICMS: double read FAliquotaDoICMS write FAliquotaDoICMS;
    property ValorICMS: double read FValorICMS write FValorICMS;
    property IsentasDoICMS: double read FIsentasDoICMS write FIsentasDoICMS;
    property OutrasDoICMS: double read FOutrasDoICMS write FOutrasDoICMS;
    property BaseCalculoIPI: double read FBaseCalculoIPI write FBaseCalculoIPI;
    property ValorIPI: double read FValorIPI write FValorIPI;
    property IsentasDoIPI: double read FIsentasDoIPI write FIsentasDoIPI;
    property OutrasDoIPI: double read FOutrasDoIPI write FOutrasDoIPI;
    property SubstituicaoICMS: boolean read FSubstituicaoICMS write FSubstituicaoICMS;
    property SubstituicaoIPI: boolean read FSubstituicaoIPI write FSubstituicaoIPI;
    property SubstituicaoCOFINS: boolean read FSubstituicaoCOFINS write FSubstituicaoCOFINS;
    property SubstituicaoPISPASEP: boolean read FSubstituicaoPISPASEP write FSubstituicaoPISPASEP;
    property CSTA: double read FCSTA write FCSTA;
    property CSTB: boolean read FCSTB write FCSTB;
    property CodigoDaSituacaoTributariaDoCSOSN: SmallInt read FCodigoDaSituacaoTributariaDoCSOSN write FCodigoDaSituacaoTributariaDoCSOSN;
    property CSOSN: integer read FCSOSN write FCSOSN;
    property CSTIPI: SmallInt read FCSTIPI write FCSTIPI;
    property COFINSMonofasico:boolean read FCOFINSMonofasico write FCOFINSMonofasico;
    property PISMonofasico: boolean read FPISMonofasico write FPISMonofasico;
    property CalculaFecopDecreto: boolean read FCalculaFecopDecreto write FCalculaFecopDecreto;
    property AliqSubstDecreto: SmallInt read FAliqSubstDecreto write FAliqSubstDecreto;
    property BaseDeCalculoDoFCPNormal: double read FBaseDeCalculoDoFCPNormal write FBaseDeCalculoDoFCPNormal;
    property AliquotaDoFCPNormal: double read FAliquotaDoFCPNormal write FAliquotaDoFCPNormal;
    property ValorDoFCPNormal: double read FValorDoFCPNormal write FValorDoFCPNormal;
    property BaseDeCalculoDoFCPSubstTribOuRetidoAntPorST: double read FBaseDeCalculoDoFCPSubstTribOuRetidoAntPorST write FBaseDeCalculoDoFCPSubstTribOuRetidoAntPorST;
    property AliquotaDoFCPSubstTribOuRetidoAntPorST: double read FAliquotaDoFCPSubstTribOuRetidoAntPorST write FAliquotaDoFCPSubstTribOuRetidoAntPorST;
    property ValorDoFCPSubstTribOuRetidoAntPorST: double read FValorDoFCPSubstTribOuRetidoAntPorST write FValorDoFCPSubstTribOuRetidoAntPorST;
    property AliquotaDoICMSDiferimento: double read FAliquotaDoICMSDiferimento write FAliquotaDoICMSDiferimento;
  end;

implementation
uses
  SysUtils;

constructor TRegistroINM.Create;
begin
  FTipoRegistro := 'INM';
end;

function TRegistroINM.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FormatFloat('#0.00', FValorDaOperacao),
    FUF,
    FCFOP,
    FCFOPTransferencia,
    FormatFloat('#0.00', FBaseCalculoICMS),
    FormatFloat('#0.00', FAliquotaDoICMS),
    FormatFloat('#0.00', FValorICMS),
    FormatFloat('#0.00', FIsentasDoICMS),
    FormatFloat('#0.00', FOutrasDoICMS),
    FormatFloat('#0.00', FBaseCalculoIPI),
    FormatFloat('#0.00', FValorIPI),
    FormatFloat('#0.00', FIsentasDoIPI),
    FormatFloat('#0.00', FOutrasDoIPI),
    IfThen(FSubstituicaoICMS, 'S', 'N'),
    IfThen(FSubstituicaoIPI, 'S', 'N'),
    IfThen(FSubstituicaoCOFINS, 'S', 'N'),
    IfThen(FSubstituicaoPISPASEP, 'S', 'N'),
    FormatFloat('#0.00', FCSTA),
    IfThen(FCSTB, 'S', 'N'),
    FCodigoDaSituacaoTributariaDoCSOSN,
    FCSOSN,
    FCSTIPI,
    IfThen(FCOFINSMonofasico, 'S', 'N'),
    IfThen(FPISMonofasico, 'S', 'N'),
    IfThen(FCalculaFecopDecreto, 'S', 'N'),
    FAliqSubstDecreto,
    FormatFloat('#0.00', FBaseDeCalculoDoFCPNormal),
    FormatFloat('#0.00', FAliquotaDoFCPNormal),
    FormatFloat('#0.00', FValorDoFCPNormal),
    FormatFloat('#0.00', FBaseDeCalculoDoFCPSubstTribOuRetidoAntPorST),
    FormatFloat('#0.00', FAliquotaDoFCPSubstTribOuRetidoAntPorST),
    FormatFloat('#0.00', FValorDoFCPSubstTribOuRetidoAntPorST),
    FormatFloat('#0.00', FAliquotaDoICMSDiferimento)
  ]);
end;

end.

