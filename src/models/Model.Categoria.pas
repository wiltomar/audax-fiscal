unit Model.Categoria;

interface

uses Model.Base;

type
  TCategoria = class(TBaseR)
  private
    fcodigo: string;
    fnome: string;
    fimagem: string;
    fnivel: integer;
    fanalitico: boolean;
  public
    constructor Create(aid: string; anome: string);
    property codigo: string read fcodigo write fcodigo;
    property nome: string read fnome write fnome;
    property imagem: string read fimagem write fimagem;
    property nivel: integer read fnivel write fnivel;
    property analitico: boolean read fanalitico write fanalitico;
  end;
  TCategorias = TArray<TCategoria>;

implementation

constructor TCategoria.Create(aid: string; anome: string);
begin
  inherited Create(aid);
  nome := anome;
end;

end.

