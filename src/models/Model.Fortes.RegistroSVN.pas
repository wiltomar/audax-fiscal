unit Model.Fortes.RegistroSVN;

interface

uses Fortes.IRegistro;

type
  TRegistroSVN = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FDescricaoServico: string;
    FValorServico: Double;
    FISSRetido: Double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property DescricaoServico: string read FDescricaoServico write FDescricaoServico;
    property ValorServico: Double read FValorServico write FValorServico;
    property ISSRetido: Double read FISSRetido write FISSRetido;
  end;

implementation

uses
  SysUtils;

constructor TRegistroSVN.Create;
begin
  FTipoRegistro := 'SVN';
end;

function TRegistroSVN.GerarLinha: string;
begin
  Result := Format('%s|%s|%.2f|%.2f|', [
    FTipoRegistro,
    FDescricaoServico,
    FValorServico,
    FISSRetido
  ]);
end;

end.

