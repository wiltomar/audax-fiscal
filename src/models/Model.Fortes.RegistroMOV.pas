unit Model.Fortes.RegistroMOV;

interface

uses Fortes.IRegistro;

type
  TRegistroMOV = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FDataMovimento: TDateTime;
    FCodigoProduto: string;
    FQuantidade: Double;
    FValorTotal: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property DataMovimento: TDateTime read FDataMovimento write FDataMovimento;
    property CodigoProduto: string read FCodigoProduto write FCodigoProduto;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property ValorTotal: Double read FValorTotal write FValorTotal;
  end;

implementation

uses
  SysUtils;

constructor TRegistroMOV.Create;
begin
  FTipoRegistro := 'MOV';
end;

function TRegistroMOV.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|%.2f|', [
    FTipoRegistro,
    FormatDateTime('yyyymmdd', FDataMovimento),
    FCodigoProduto,
    FQuantidade,
    FValorTotal
  ]);
end;

end.

