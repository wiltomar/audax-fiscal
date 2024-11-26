unit Model.Fortes.RegistroAIP;

interface

uses Fortes.IRegistro;

type
  TRegistroAIP = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroAdicao: string;
    FCodigoFabricante: string;
    FValorDesconto: Double;
    FNumeroPedido: string;
    FItemPedido: string;
    FNumeroDrawback: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroAdicao: string read FNumeroAdicao write FNumeroAdicao;
    property CodigoFabricante: string read FCodigoFabricante write FCodigoFabricante;
    property ValorDesconto: Double read FValorDesconto write FValorDesconto;
    property NumeroPedido: string read FNumeroPedido write FNumeroPedido;
    property ItemPedido: string read FItemPedido write FItemPedido;
    property NumeroDrawback: string read FNumeroDrawback write FNumeroDrawback;
  end;

implementation

uses
  SysUtils;

constructor TRegistroAIP.Create;
begin
  FTipoRegistro := 'AIP';
end;

function TRegistroAIP.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%.2f|%s|%s|%s|', [
    FTipoRegistro,
    FNumeroAdicao,
    FCodigoFabricante,
    FValorDesconto,
    FNumeroPedido,
    FItemPedido,
    FNumeroDrawback
  ]);
end;

end.

