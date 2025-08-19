unit Model.Fortes.RegistroGRP;

interface

uses Fortes.IRegistro;

type
  TRegistroGRP = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigo: string;
    FDescricao: string;
    FSituacaoDIEF: integer;
    FSituacaoProdutoSPED: integer;
    FGrupoProdutosDIC: integer;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Codigo: string read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property SituacaoDIEF: integer read FSituacaoDIEF write FSituacaoDIEF;
    property SituacaoProdutoSPED: integer read FSituacaoProdutoSPED write FSituacaoProdutoSPED;
    property GrupoProdutosDIC: integer read FGrupoProdutosDIC write FGrupoProdutosDIC;
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
  Result := Format('%s|%s|%s|%s|%s|%s', [
    FTipoRegistro,
    FCodigo,
    FDescricao,
    FSituacaoDIEF.ToString,
    FSituacaoProdutoSPED.ToString,
    FGrupoProdutosDIC.ToString
  ]);
end;

end.
