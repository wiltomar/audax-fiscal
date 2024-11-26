unit Model.Fortes.RegistroGRP;

interface

uses Fortes.IRegistro;

type
  TRegistroGRP = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigo: string;
    FDescricao: string;
    FSituacaoDIEF: string;
    FSituacaoProdutoSPED: string;
    FGrupoProdutosDIC: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Codigo: string read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property SituacaoDIEF: string read FSituacaoDIEF write FSituacaoDIEF;
    property SituacaoProdutoSPED: string read FSituacaoProdutoSPED write FSituacaoProdutoSPED;
    property GrupoProdutosDIC: string read FGrupoProdutosDIC write FGrupoProdutosDIC;
  end;

implementation

uses
  SysUtils;

constructor TRegistroGRP.Create;
begin
  FTipoRegistro := 'GRP';
end;

function TRegistroGRP.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FCodigo,
    FDescricao,
    FSituacaoDIEF,
    FSituacaoProdutoSPED,
    FGrupoProdutosDIC
  ]);
end;

end.
