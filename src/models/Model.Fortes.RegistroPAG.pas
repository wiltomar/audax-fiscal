unit Model.Fortes.RegistroPAG;

interface

uses Fortes.IRegistro;

type
  TRegistroPAG = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroPagamento: string;
    FDataPagamento: TDateTime;
    FValorPagamento: Double;
    FFormaPagamento: string; // "DINHEIRO", "CARTÃO", etc.
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroPagamento: string read FNumeroPagamento write FNumeroPagamento;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property FormaPagamento: string read FFormaPagamento write FFormaPagamento;
  end;

implementation

uses
  SysUtils;

constructor TRegistroPAG.Create;
begin
  FTipoRegistro := 'PAG';
end;

function TRegistroPAG.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|%s|', [
    FTipoRegistro,
    FNumeroPagamento,
    FormatDateTime('yyyymmdd', FDataPagamento),
    FValorPagamento,
    FFormaPagamento
  ]);
end;

end.

