unit Model.Fortes.RegistroCBL;

interface

uses Fortes.IRegistro;

type
  TRegistroCBL = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FDescricaoBaseLegal: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property DescricaoBaseLegal: string read FDescricaoBaseLegal write FDescricaoBaseLegal;
  end;

implementation

uses
  SysUtils;

constructor TRegistroCBL.Create;
begin
  FTipoRegistro := 'CBL';
end;

function TRegistroCBL.GerarLinha: string;
begin
  Result := Format('%s|%s|', [
    FTipoRegistro,
    FDescricaoBaseLegal
  ]);
end;

end.

