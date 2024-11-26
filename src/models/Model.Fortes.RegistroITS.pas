unit Model.Fortes.RegistroITS;

interface

uses Fortes.IRegistro;

type
  TRegistroITS = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroNota: string;
    FValorISS: Double;
    FAliquotaISS: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroNota: string read FNumeroNota write FNumeroNota;
    property ValorISS: Double read FValorISS write FValorISS;
    property AliquotaISS: Double read FAliquotaISS write FAliquotaISS;
  end;

implementation

uses
  SysUtils;

constructor TRegistroITS.Create;
begin
  FTipoRegistro := 'ITS';
end;

function TRegistroITS.GerarLinha: string;
begin
  Result := Format('%s|%s|%.2f|%.2f|', [
    FTipoRegistro,
    FNumeroNota,
    FValorISS,
    FAliquotaISS
  ]);
end;

end.
