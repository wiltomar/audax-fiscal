unit Model.Fortes.RegistroCVN;

interface

uses Fortes.IRegistro;

type
  TRegistroCVN = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroNota: string;
    FDataEmissao: TDateTime;
    FValorCompra: Double;
    FCodigoFornecedor: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroNota: string read FNumeroNota write FNumeroNota;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property ValorCompra: Double read FValorCompra write FValorCompra;
    property CodigoFornecedor: string read FCodigoFornecedor write FCodigoFornecedor;
  end;

implementation

uses
  SysUtils;

constructor TRegistroCVN.Create;
begin
  FTipoRegistro := 'CVN';
end;

function TRegistroCVN.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|%s|', [
    FTipoRegistro,
    FNumeroNota,
    FormatDateTime('yyyymmdd', FDataEmissao),
    FValorCompra,
    FCodigoFornecedor
  ]);
end;

end.

