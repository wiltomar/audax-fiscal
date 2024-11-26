unit Model.Fortes.RegistroFIN;

interface

uses Fortes.IRegistro;

type
  TRegistroFIN = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FDataFechamento: TDateTime;
    FValorFinal: Double;
    FStatus: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property DataFechamento: TDateTime read FDataFechamento write FDataFechamento;
    property ValorFinal: Double read FValorFinal write FValorFinal;
    property Status: string read FStatus write FStatus;
  end;

implementation

uses
  SysUtils;

constructor TRegistroFIN.Create;
begin
  FTipoRegistro := 'FIN';
end;

function TRegistroFIN.GerarLinha: string;
begin
  Result := Format('%s|%s|%.2f|%s|', [
    FTipoRegistro,
    FormatDateTime('yyyymmdd', FDataFechamento),
    FValorFinal,
    FStatus
  ]);
end;

end.

