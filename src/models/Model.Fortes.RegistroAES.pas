unit Model.Fortes.RegistroAES;

interface

uses Fortes.IRegistro;

type
  TRegistroAES = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroNota: string;
    FDataPagamento: TDateTime;
    FValorPago: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroNota: string read FNumeroNota write FNumeroNota;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property ValorPago: Double read FValorPago write FValorPago;
  end;

implementation

uses
  SysUtils;

constructor TRegistroAES.Create;
begin
  FTipoRegistro := 'AES';
end;

function TRegistroAES.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|', [
    FTipoRegistro,
    FNumeroNota,
    FormatDateTime('yyyymmdd', FDataPagamento),
    FValorPago
  ]);
end;

end.

