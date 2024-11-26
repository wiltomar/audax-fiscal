unit Model.Fortes.RegistroCRT;

interface

uses Fortes.IRegistro;

type
  TRegistroCRT = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigoCentro: string;
    FDescricao: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property CodigoCentro: string read FCodigoCentro write FCodigoCentro;
    property Descricao: string read FDescricao write FDescricao;
  end;

implementation

uses
  SysUtils;

constructor TRegistroCRT.Create;
begin
  FTipoRegistro := 'CRT';
end;

function TRegistroCRT.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|', [FTipoRegistro, FCodigoCentro, FDescricao]);
end;

end.

