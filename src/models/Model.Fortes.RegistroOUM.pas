unit Model.Fortes.RegistroOUM;

interface

uses Fortes.IRegistro;

type
  TRegistroOUM = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigoProduto: integer;
    FUnidadeDeMedida: string;
    FUnidadeEquivalentePadrao: double;
    FCodigoDeBarras: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property CodigoProduto: integer read FCodigoProduto write FCodigoProduto;
    property UnidadeDeMedida: string read FUnidadeDeMedida write FUnidadeDeMedida;
    property UnidadeEquivalentePadrao: double read FUnidadeEquivalentePadrao write FUnidadeEquivalentePadrao;
    property CodigoDeBarras: string read FCodigoDeBarras write FCodigoDeBarras;
  end;

implementation

uses
  SysUtils;

constructor TRegistroOUM.Create;
begin
  FTipoRegistro := 'OUM';
end;

function TRegistroOUM.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    CodigoProduto,
    UnidadeDeMedida,
    formatFloat('#0.00',UnidadeEquivalentePadrao),
    CodigoDeBarras
  ]);
end;

end.

