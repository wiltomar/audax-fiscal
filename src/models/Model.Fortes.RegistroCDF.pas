unit Model.Fortes.RegistroCDF;

interface

uses Fortes.IRegistro;

type
  TRegistroCDF = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FChaveNotaFiscal: string;
    FDescricaoComplementar: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property ChaveNotaFiscal: string read FChaveNotaFiscal write FChaveNotaFiscal;
    property DescricaoComplementar: string read FDescricaoComplementar write FDescricaoComplementar;
  end;

implementation

uses
  SysUtils;

constructor TRegistroCDF.Create;
begin
  FTipoRegistro := 'CDF';
end;

function TRegistroCDF.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|', [
    FTipoRegistro,
    FChaveNotaFiscal,
    FDescricaoComplementar
  ]);
end;

end.

