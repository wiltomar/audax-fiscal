unit Model.Fortes.RegistroCFI;

interface

uses Fortes.IRegistro;

type
  TRegistroCFI = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FValor: Double;
    FUF: string;
    FCFOP: string;
    FBaseCalculo: Double;
    FAliquota: Double;
    FICMSDebitado: Double;
    FIsentas: Double;
    FOutras: Double;
    FSubstituicaoICMS: string;
    FSubstituicaoCOFINS: string;
    FSubstituicaoPIS: string;
    FCSTA: string;
    FCSTB: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Valor: Double read FValor write FValor;
    property UF: string read FUF write FUF;
    property CFOP: string read FCFOP write FCFOP;
    property BaseCalculo: Double read FBaseCalculo write FBaseCalculo;
    property Aliquota: Double read FAliquota write FAliquota;
    property ICMSDebitado: Double read FICMSDebitado write FICMSDebitado;
    property Isentas: Double read FIsentas write FIsentas;
    property Outras: Double read FOutras write FOutras;
    property SubstituicaoICMS: string read FSubstituicaoICMS write FSubstituicaoICMS;
    property SubstituicaoCOFINS: string read FSubstituicaoCOFINS write FSubstituicaoCOFINS;
    property SubstituicaoPIS: string read FSubstituicaoPIS write FSubstituicaoPIS;
    property CSTA: string read FCSTA write FCSTA;
    property CSTB: string read FCSTB write FCSTB;
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
  Result := Format('%s|%.2f|%s|%s|%.2f|%.2f|%.2f|%.2f|%.2f|%s|%s|', [
    FTipoRegistro,
    FValor,
    FUF,
    FCFOP,
    FBaseCalculo,
    FAliquota,
    FICMSDebitado,
    FIsentas,
    FOutras,
    FSubstituicaoICMS,
    FSubstituicaoCOFINS,
    FSubstituicaoPIS
  ]);
end;

end.

