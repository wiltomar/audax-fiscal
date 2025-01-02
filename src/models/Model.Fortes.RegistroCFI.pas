unit Model.Fortes.RegistroCFI;

interface

uses Fortes.IRegistro, System.StrUtils;

type
  TRegistroCFI = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FValor: Double;
    FUF: string;
    FCFOP: integer;
    FBaseCalculo: Double;
    FAliquota: Double;
    FICMSDebitado: Double;
    FIsentas: Double;
    FOutras: Double;
    FSubstituicaoICMS: boolean;
    FSubstituicaoCOFINS: boolean;
    FSubstituicaoPIS: boolean;
    FCSTA: integer;
    FCSTB: integer;
    FCodigoDaSituacaoTributariaDoCSOSN: SmallInt;
    FCSOSN: integer;
    FCOFINSMonofasico: boolean;
    FPISMonofasico: boolean;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Valor: Double read FValor write FValor;
    property UF: string read FUF write FUF;
    property CFOP: integer read FCFOP write FCFOP;
    property BaseCalculo: Double read FBaseCalculo write FBaseCalculo;
    property Aliquota: Double read FAliquota write FAliquota;
    property ICMSDebitado: Double read FICMSDebitado write FICMSDebitado;
    property Isentas: Double read FIsentas write FIsentas;
    property Outras: Double read FOutras write FOutras;
    property SubstituicaoICMS: boolean read FSubstituicaoICMS write FSubstituicaoICMS;
    property SubstituicaoCOFINS: boolean read FSubstituicaoCOFINS write FSubstituicaoCOFINS;
    property SubstituicaoPIS: boolean read FSubstituicaoPIS write FSubstituicaoPIS;
    property CSTA: integer read FCSTA write FCSTA;
    property CSTB: integer read FCSTB write FCSTB;
    property CodigoDaSituacaoTributariaDoCSOSN: SmallInt read FCodigoDaSituacaoTributariaDoCSOSN write FCodigoDaSituacaoTributariaDoCSOSN;
    property CSOSN: integer read FCSOSN write FCSOSN;
    property COFINSMonofasico: boolean read FCOFINSMonofasico write FCOFINSMonofasico;
    property PISMonofasico: boolean read FPISMonofasico write FPISMonofasico;
  end;

implementation
uses
  SysUtils;

constructor TRegistroCFI.Create;
begin
  FTipoRegistro := 'CFI';
end;

function TRegistroCFI.GerarLinha: string;
begin
  //Result := Format('%s|%.2f|%s|%s|%.2f|%.2f|%.2f|%.2f|%.2f|%s|%s|', [
  Result := Format('%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    formatFloat('#0.00', FValor),
    FUF,
    FCFOP,
    formatFloat('#0.00', FBaseCalculo),
    formatFloat('#0.00', FAliquota),
    formatFloat('#0.00', FICMSDebitado),
    formatFloat('#0.00', FIsentas),
    formatFloat('#0.00', FOutras),
    IfThen(FSubstituicaoICMS, 'S', 'N'),
    IfThen(FSubstituicaoCOFINS, 'S', 'N'),
    IfThen(FSubstituicaoPIS, 'S', 'N'),
    FCSTA,
    FCSTB,
    FCodigoDaSituacaoTributariaDoCSOSN,
    FCSOSN,
    IfThen(COFINSMonofasico, 'S', 'N'),
    IfThen(PISMonofasico, 'S', 'N')
  ]);
end;


end.

