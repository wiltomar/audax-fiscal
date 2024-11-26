unit Model.Fortes.RegistroDCT;

interface

uses Fortes.IRegistro;

type
  TRegistroDCT = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroDocumento: string;
    FDataEmissao: TDateTime;
    FValorDocumento: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroDocumento: string read FNumeroDocumento write FNumeroDocumento;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property ValorDocumento: Double read FValorDocumento write FValorDocumento;
  end;

implementation

uses
  SysUtils;

constructor TRegistroDCT.Create;
begin
  FTipoRegistro := 'DCT';
end;

function TRegistroDCT.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|', [
    FTipoRegistro,
    FNumeroDocumento,
    FormatDateTime('yyyymmdd', FDataEmissao),
    FValorDocumento
  ]);
end;

end.

