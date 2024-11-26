unit Model.Fortes.RegistroSPR;

interface

uses Fortes.IRegistro;

type
  TRegistroSPR = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroProcesso: string;
    FDescricaoHistorico: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroProcesso: string read FNumeroProcesso write FNumeroProcesso;
    property DescricaoHistorico: string read FDescricaoHistorico write FDescricaoHistorico;
  end;

implementation

uses
  SysUtils;

constructor TRegistroSPR.Create;
begin
  FTipoRegistro := 'SPR';
end;

function TRegistroSPR.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|', [
    FTipoRegistro,
    FNumeroProcesso,
    FDescricaoHistorico
  ]);
end;

end.

