unit Model.Item;

interface

uses Model.Base, Model.Categoria, Model.Preco;

type
  TItemTipo =
  (
    Item = 10,
    Servico = 110,
    TaxaDeEntrega = 210
  );

type
  TItemPreco = class(TBaseR)
  private
    fpreco: TPreco;
    fvalor: Currency;
  public
    property preco: TPreco read fpreco write fpreco;
    property valor: Currency read fvalor write fvalor;
  end;
  TItemPrecos = TArray<TItemPreco>;

type
  TItem = class(TBaseTR)
  private
    fimagem: string;
    fvalor: Currency;
    fcategoria: TCategoria;
    fitemPrecos: TArray<TItemPreco>;
  public
    constructor Create(); overload;
    constructor Create(aid, anome: string); overload;
    destructor Destroy(); override;
    class procedure Clear(var Itens: TArray<TItem>);
    class function FromJsonString(AJsonString: string): TItem;
    function ToJsonString(): string;
    procedure Atribui(Item: TItem);
    property imagem: string read fimagem write fimagem;
    property valor: Currency read fvalor write fvalor;
    property categoria: TCategoria read fcategoria write fcategoria;
    property itemPrecos: TArray<TItemPreco> read fitemPrecos write fitemPrecos;
    function getValor(preco: TPreco): Currency;
  end;
  TItens = TArray<TItem>;

type
  TItemC = class(TBaseR)
  private
    fcodigo: string;
    fnome: string;
    fimagem: string;
    fsituacao: SmallInt;
    fatributos: TArray<string>;
    festrela: Boolean;
    fcategoria: TCategoria;
    //funidade: TUnidade;
    fitemPrecos: TItemPrecos;
  public
    constructor Create(aid: string = ''; anome: string = '');
    destructor Destroy(); override;
    property codigo: string read fcodigo write fcodigo;
    property nome: string read fnome write fnome;
    property imagem: string read fimagem write fimagem;
    property situacao: SmallInt read fsituacao write fsituacao;
    property atributos: TArray<string> read fatributos write fatributos;
    property estrela: Boolean read festrela write festrela;
    property categoria: TCategoria read fcategoria write fcategoria;
    //property unidade: TUnidade read funidade write funidade;
    property itemPrecos: TItemPrecos read fitemPrecos write fitemPrecos;
  end;

implementation

uses System.SysUtils, Rest.JSON, Lib.Funcoes;

class procedure TItem.Clear(var Itens: TArray<TItem>);
var
  Item: TItem;
begin
  for Item in Itens do
    Item.Free();
  Itens := [];
end;

class function TItem.FromJsonString(AJsonString: string): TItem;
begin
  Result := TJson.JsonToObject<TItem>(AJsonString);
end;

constructor TItem.Create();
begin
  inherited Create();
//  fitemPrecos := [];
end;

constructor TItem.Create(aid, anome: string);
begin
  inherited Create(aid, anome);
//  fitemPrecos := [];
end;

destructor TItem.Destroy();
begin
//  for var itemPreco in fitemPrecos do
//    itemPreco.Free();
//  fitemPrecos := [];
  inherited;
end;

function TItem.ToJsonString(): string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

procedure TItem.Atribui(Item: TItem);
begin
  id := Item.id;
  codigo := Item.codigo;
  nome := Item.nome;
  imagem := Item.imagem;
  itemPrecos := [];
  for var itemPreco in Item.itemPrecos do
  begin
    var zitemPreco := TItemPreco.Create();
    zitemPreco.preco := TPreco.Create(itemPreco.preco);
    zitemPreco.valor := itemPreco.valor;
    itemPrecos := item.itemPrecos + [zitemPreco];
  end;
end;

function TItem.getValor(preco: TPreco): Currency;
begin
  if not Assigned(preco) then
    ExcecaoErro('parâmetro "preco" não fornecido');
  for var itemPreco in fitemPrecos do
    if itemPreco.preco.id = preco.id then
      Exit(itemPreco.valor);
  Result := 0.00;
end;

constructor TItemC.Create(aid: string = ''; anome: string = '');
begin
  inherited Create(aid);
  fnome := anome;
  fcategoria := nil;
  //funidade := nil;
  fitemPrecos := [];
end;

destructor TItemC.Destroy();
begin
  Obliterate(fcategoria);
  //Obliterate(funidade);
  for var itemPreco in fitemPrecos do
    itemPreco.Free();
  fitemPrecos := [];
end;

end.
