unit Model.Fortes.RegistroLAN;

interface

uses Fortes.IRegistro;

type
  TRegistroLAN = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FDataLancamento: TDateTime;
    FCodigoConta: string;
    FDescricao: string;
    FValor: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property DataLancamento: TDateTime read FDataLancamento write FDataLancamento;
    property CodigoConta: string read FCodigoConta write FCodigoConta;
    property Descricao: string read FDescricao write FDescricao;
    property Valor: Double read FValor write FValor;
  end;

implementation

uses
  SysUtils;

constructor TRegistroLAN.Create;
begin
  FTipoRegistro := 'LAN';
end;

function TRegistroLAN.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%.2f|', [
    FTipoRegistro,
    FormatDateTime('yyyymmdd', FDataLancamento),
    FCodigoConta,
    FDescricao,
    FValor
  ]);
end;

end.

