unit Model.Fortes.RegistroOUM;

interface

uses Fortes.IRegistro;

type
  TRegistroOUM = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FDescricao: string;
    FConversao: Double; // Conversão para unidade padrão
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Descricao: string read FDescricao write FDescricao;
    property Conversao: Double read FConversao write FConversao;
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
  Result := Format('%s|%s|%.2f|', [
    FTipoRegistro,
    FDescricao,
    FConversao
  ]);
end;

end.

