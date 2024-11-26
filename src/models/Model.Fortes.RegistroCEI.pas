unit Model.Fortes.RegistroCEI;

interface

uses Fortes.IRegistro;

type
  TRegistroCEI = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroDeclaracao: string;
    FDataDeclaracao: TDateTime;
    FValorTotal: Double;
    FNumeroContrato: string;
    FCodigoExportador: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroDeclaracao: string read FNumeroDeclaracao write FNumeroDeclaracao;
    property DataDeclaracao: TDateTime read FDataDeclaracao write FDataDeclaracao;
    property ValorTotal: Double read FValorTotal write FValorTotal;
    property NumeroContrato: string read FNumeroContrato write FNumeroContrato;
    property CodigoExportador: string read FCodigoExportador write FCodigoExportador;
  end;

implementation

uses
  SysUtils;

constructor TRegistroCEI.Create;
begin
  FTipoRegistro := 'CEI';
end;

function TRegistroCEI.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|%s|%s|', [
    FTipoRegistro,
    FNumeroDeclaracao,
    FormatDateTime('yyyymmdd', FDataDeclaracao),
    FValorTotal,
    FNumeroContrato,
    FCodigoExportador
  ]);
end;

end.

