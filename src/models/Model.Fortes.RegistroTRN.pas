unit Model.Fortes.RegistroTRN;

interface

uses Fortes.IRegistro;

type
  TRegistroTRN = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroTransacao: string;
    FDataTransacao: TDateTime;
    FValorTransacao: Double;
    FCodigoBanco: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroTransacao: string read FNumeroTransacao write FNumeroTransacao;
    property DataTransacao: TDateTime read FDataTransacao write FDataTransacao;
    property ValorTransacao: Double read FValorTransacao write FValorTransacao;
    property CodigoBanco: string read FCodigoBanco write FCodigoBanco;
  end;

implementation

uses
  SysUtils;

constructor TRegistroTRN.Create;
begin
  FTipoRegistro := 'TRN';
end;

function TRegistroTRN.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|%s|', [
    FTipoRegistro,
    FNumeroTransacao,
    FormatDateTime('yyyymmdd', FDataTransacao),
    FValorTransacao,
    FCodigoBanco
  ]);
end;

end.

