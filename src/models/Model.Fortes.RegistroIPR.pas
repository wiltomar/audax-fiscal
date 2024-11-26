unit Model.Fortes.RegistroIPR;

interface

uses Fortes.IRegistro;

type
  TRegistroIPR = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroImportacao: string;
    FDataImportacao: TDateTime;
    FValorProduto: Double;
    FValorPagamento: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroImportacao: string read FNumeroImportacao write FNumeroImportacao;
    property DataImportacao: TDateTime read FDataImportacao write FDataImportacao;
    property ValorProduto: Double read FValorProduto write FValorProduto;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
  end;

implementation

uses
  SysUtils;

constructor TRegistroIPR.Create;
begin
  FTipoRegistro := 'IPR';
end;

function TRegistroIPR.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|%.2f|', [
    FTipoRegistro,
    FNumeroImportacao,
    FormatDateTime('yyyymmdd', FDataImportacao),
    FValorProduto,
    FValorPagamento
  ]);
end;

end.

