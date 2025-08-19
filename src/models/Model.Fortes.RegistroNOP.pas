unit Model.Fortes.RegistroNOP;

interface

uses Fortes.IRegistro, System.StrUtils;

type
  TRegistroNOP = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigo: integer;
    FDescricao: string;
    FEstorno: boolean;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Codigo: integer read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property Estorno: boolean read FEstorno write FEstorno;
  end;

implementation

uses
  SysUtils;

constructor TRegistroNOP.Create;
begin
  FTipoRegistro := 'NOP';
end;

function TRegistroNOP.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|', [
  FTipoRegistro,
  FCodigo.ToString,
  FDescricao,
  IfThen(FEstorno, 'S', 'N')
  ]);
end;

end.

