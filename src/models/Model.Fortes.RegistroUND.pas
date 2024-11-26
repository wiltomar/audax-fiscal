unit Model.Fortes.RegistroUND;

interface

uses Fortes.IRegistro;

type
  TRegistroUND = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FUnidadeMedida: string;
    FDescricao: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property UnidadeMedida: string read FUnidadeMedida write FUnidadeMedida;
    property Descricao: string read FDescricao write FDescricao;
  end;

implementation

uses
  SysUtils;

constructor TRegistroUND.Create;
begin
  FTipoRegistro := 'UND';
end;

function TRegistroUND.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|', [FTipoRegistro, FUnidadeMedida, FDescricao]);
end;

end.

