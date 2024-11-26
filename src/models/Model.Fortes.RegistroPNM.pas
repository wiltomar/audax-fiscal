unit Model.Fortes.RegistroPNM;

interface

uses Fortes.IRegistro;

type
  TRegistroPNM = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigoProduto: string;
    FQuantidade: string;
    FValor: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property CodigoProduto: string read FCodigoProduto write FCodigoProduto;
    property Quantidade: string read FQuantidade write FQuantidade;
    property Valor: string read FValor write FValor;
  end;

implementation

uses
  SysUtils;

constructor TRegistroPNM.Create;
begin
  FTipoRegistro := 'PNM';
end;

function TRegistroPNM.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|', [FTipoRegistro, FCodigoProduto, FQuantidade, FValor]);
end;

end.

