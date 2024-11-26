unit Model.Fortes.RegistroCEE;

interface

uses Fortes.IRegistro;

type
  TRegistroCEE = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroNota: string;
    FValorTotal: Double;
    FICMS: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroNota: string read FNumeroNota write FNumeroNota;
    property ValorTotal: Double read FValorTotal write FValorTotal;
    property ICMS: Double read FICMS write FICMS;
  end;

implementation

uses
  SysUtils;

constructor TRegistroCEE.Create;
begin
  FTipoRegistro := 'CEE';
end;

function TRegistroCEE.GerarLinha: string;
begin
  Result := Format('%s|%s|%.2f|%.2f|', [
    FTipoRegistro,
    FNumeroNota,
    FValorTotal,
    FICMS
  ]);
end;

end.

