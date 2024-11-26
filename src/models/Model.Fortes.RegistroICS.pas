unit Model.Fortes.RegistroICS;

interface

uses Fortes.IRegistro;

type
  TRegistroICS = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FValorImpostos: Double;
    FValorTotal: Double;
    FDescricaoImposto: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property ValorImpostos: Double read FValorImpostos write FValorImpostos;
    property ValorTotal: Double read FValorTotal write FValorTotal;
    property DescricaoImposto: string read FDescricaoImposto write FDescricaoImposto;
  end;

implementation

uses
  SysUtils;

constructor TRegistroICS.Create;
begin
  FTipoRegistro := 'ICS';
end;

function TRegistroICS.GerarLinha: string;
begin
  Result := Format('%s|%.2f|%.2f|%s|', [
    FTipoRegistro,
    FValorImpostos,
    FValorTotal,
    FDescricaoImposto
  ]);
end;

end.

