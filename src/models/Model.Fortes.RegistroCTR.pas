unit Model.Fortes.RegistroCTR;

interface

uses Fortes.IRegistro;

type
  TRegistroCTR = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigoImposto: string;
    FDescricao: string;
    FAliquota: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property CodigoImposto: string read FCodigoImposto write FCodigoImposto;
    property Descricao: string read FDescricao write FDescricao;
    property Aliquota: Double read FAliquota write FAliquota;
  end;

implementation

uses
  SysUtils;

constructor TRegistroCTR.Create;
begin
  FTipoRegistro := 'CTR';
end;

function TRegistroCTR.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|', [FTipoRegistro, FCodigoImposto, FDescricao, FAliquota]);
end;

end.

