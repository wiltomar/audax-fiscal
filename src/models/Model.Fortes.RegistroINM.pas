unit Model.Fortes.RegistroINM;

interface

uses Fortes.IRegistro;

type
  TRegistroINM = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FBaseCalculoICMS: string;
    FValorICMS: string;
    FBaseCalculoIPI: string;
    FValorIPI: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property BaseCalculoICMS: string read FBaseCalculoICMS write FBaseCalculoICMS;
    property ValorICMS: string read FValorICMS write FValorICMS;
    property BaseCalculoIPI: string read FBaseCalculoIPI write FBaseCalculoIPI;
    property ValorIPI: string read FValorIPI write FValorIPI;
  end;

implementation

uses
  SysUtils;

constructor TRegistroINM.Create;
begin
  FTipoRegistro := 'INM';
end;

function TRegistroINM.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|', [FTipoRegistro, FBaseCalculoICMS, FValorICMS, FBaseCalculoIPI, FValorIPI]);
end;

end.

