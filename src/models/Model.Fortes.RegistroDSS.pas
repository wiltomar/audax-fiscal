unit Model.Fortes.RegistroDSS;

interface

uses Fortes.IRegistro;

type
  TRegistroDSS = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroNota: string;
    FDataEmissao: TDateTime;
    FValorTotal: Double;
    FISS: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroNota: string read FNumeroNota write FNumeroNota;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property ValorTotal: Double read FValorTotal write FValorTotal;
    property ISS: Double read FISS write FISS;
  end;

implementation

uses
  SysUtils;

constructor TRegistroDSS.Create;
begin
  FTipoRegistro := 'DSS';
end;

function TRegistroDSS.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|%.2f|', [
    FTipoRegistro,
    FNumeroNota,
    FormatDateTime('yyyymmdd', FDataEmissao),
    FValorTotal,
    FISS
  ]);
end;

end.

