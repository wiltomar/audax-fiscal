unit Model.Fortes.RegistroSSP;

interface

uses Fortes.IRegistro;

type
  TRegistroSSP = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroProcesso: string;
    FTributoSuspenso: string;
    FValorSuspenso: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroProcesso: string read FNumeroProcesso write FNumeroProcesso;
    property TributoSuspenso: string read FTributoSuspenso write FTributoSuspenso;
    property ValorSuspenso: Double read FValorSuspenso write FValorSuspenso;
  end;

implementation

uses
  SysUtils;

constructor TRegistroSSP.Create;
begin
  FTipoRegistro := 'SSP';
end;

function TRegistroSSP.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|', [
    FTipoRegistro,
    FNumeroProcesso,
    FTributoSuspenso,
    FValorSuspenso
  ]);
end;

end.

