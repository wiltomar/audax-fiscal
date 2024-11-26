unit Model.Fortes.RegistroTRA;

interface

uses Fortes.IRegistro;

type
  TRegistroTRA = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FQuantidadeRegistros: Integer;
    FSomaValores: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property QuantidadeRegistros: Integer read FQuantidadeRegistros write FQuantidadeRegistros;
    property SomaValores: Double read FSomaValores write FSomaValores;
  end;

implementation

uses
  SysUtils;

constructor TRegistroTRA.Create;
begin
  FTipoRegistro := 'TRA';
end;

function TRegistroTRA.GerarLinha: string;
begin
  Result := Format('%s|%d|%.2f|', [
    FTipoRegistro,
    FQuantidadeRegistros,
    FSomaValores
  ]);
end;

end.

