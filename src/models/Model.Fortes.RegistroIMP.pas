unit Model.Fortes.RegistroIMP;

interface

uses Fortes.IRegistro;

type
  TRegistroIMP = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroNota: string;
    FCodigoImposto: string;
    FBaseCalculo: Double;
    FValorImposto: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroNota: string read FNumeroNota write FNumeroNota;
    property CodigoImposto: string read FCodigoImposto write FCodigoImposto;
    property BaseCalculo: Double read FBaseCalculo write FBaseCalculo;
    property ValorImposto: Double read FValorImposto write FValorImposto;
  end;

implementation

uses
  SysUtils;

constructor TRegistroIMP.Create;
begin
  FTipoRegistro := 'IMP';
end;

function TRegistroIMP.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|%.2f|', [
    FTipoRegistro,
    FNumeroNota,
    FCodigoImposto,
    FBaseCalculo,
    FValorImposto
  ]);
end;

end.

