unit Model.Fortes.RegistroTPF;

interface

uses Fortes.IRegistro;

type
  TRegistroTPF = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigoPagamento: string;
    FDescricao: string;
    FForma: string; // Ex.: 'DINHEIRO', 'CARTÃO'
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property CodigoPagamento: string read FCodigoPagamento write FCodigoPagamento;
    property Descricao: string read FDescricao write FDescricao;
    property Forma: string read FForma write FForma;
  end;

implementation

uses
  SysUtils;

constructor TRegistroTPF.Create;
begin
  FTipoRegistro := 'TPF';
end;

function TRegistroTPF.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|', [FTipoRegistro, FCodigoPagamento, FDescricao, FForma]);
end;

end.

