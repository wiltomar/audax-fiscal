unit Model.Fortes.RegistroNOP;

interface

uses Fortes.IRegistro;

type
  TRegistroNOP = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigo: string;
    FDescricao: string;
    FEstorno: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Codigo: string read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property Estorno: string read FEstorno write FEstorno;
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
  Result := Format('%s|%s|%s|%s|', [FTipoRegistro, FCodigo, FDescricao, FEstorno]);
end;

end.

