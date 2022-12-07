unit Lib.Sistema.Tipos;

interface

uses System.SysUtils, System.Generics.Collections;

type
  LargeInt = Int64;

type
  AString = TArray<string>;
  AInteger = TArray<Integer>;

type
  TComboItem = record
    id: Integer;
    nome: string;
    tag: LargeInt;
  end;
  TComboItens = TArray<TComboItem>;

function ComboItem(id: Integer; nome: string): TComboItem;

type
  TProcedimento =
  (
    prNenhum,
    prAdicionar,
    prEditar,
    prExcluir,
    prVisualizar,
    prTransferir
  );

const
  SITUACAO_ATIVO = 1;

const
  BS_ASSIGNED_CHANGE_EVENTS = 128;

const
  CLASSE_LOCALIZADOR = 1200;
  CLASSE_OCUPACAO = 1600;
  CLASSE_FATURA = 2100;

const
  IMAGEM_LISTA = 1;
  IMAGEM_BUSCAR = 2;
  IMAGEM_SENHAEXIBE = 5;
  IMAGEM_SENHAESCONDE = 6;
  IMAGEM_APAGAR = 9;
  IMAGEM_ADICIONAR = 110;
  IMAGEM_EDITAR = 120;
  IMAGEM_EXCLUIR = 130;
  IMAGEM_VISUAL = 140;
  IMAGEM_PADRAO = 210;

const
  // Agente
  TP_CLIENTE = 110;
  TP_FORNECEDOR = 120;
  TP_COLABORADOR = 130;
  TP_VENDEDOR = 140;
  TP_ENTREGADOR = 150;
  // Venda
  TP_VENDA = 1100;
  TP_VENDAITEM = 1200;
  // Ocupação
  TP_OCUPACAO = 5100;
  TP_OCUPACAOITEM = 5200;
  // Comanda
  TP_COMANDA = 6100;
  // Fatura
  TP_FATURA = 7100;
  TP_FATURAITEM = 7200;
  TP_FATURAPAGAMENTO = 7300;

type
  TTopico = class
  private
    FReferencia: TObject;
    FImagem: SmallInt;
    FTipo: SmallInt;
    FId: string;
    FCodigo: string;
    FNome: string;
    FValor: Currency;
    FQuantidade: Currency;
    FQuantidadeUnidade: string;
    FSubTotal: Currency;
    FAcrescimo: Currency;
    FFrete: Currency;
    FServico: Currency;
    FDesconto: Currency;
    FTotal: Currency;
    FObservacoes: string;
    FComplemento: string;
    procedure AtualizaSubTotal();
    procedure AtualizaTotal();
    procedure SetValor(Value: Currency);
    procedure SetQuantidade(Value: Currency);
    procedure SetAcrescimo(Value: Currency);
    procedure SetFrete(Value: Currency);
    procedure SetServico(Value: Currency);
    procedure SetDesconto(Value: Currency);
    function GeteVendaItem(): Boolean;
    function GeteOcupacao(): Boolean;
  public
    class function FromData(P: Pointer): TTopico;
    class function FromTag(Tag: NativeInt): TTopico;
//    class procedure Free(P: Pointer);
    class procedure FreeTag(Tag: NativeInt);
    constructor Create
    (
      AReferencia: TObject;
      AImagem: SmallInt;
      ATipo: SmallInt;
      AId: string;
      ANome: string;
      AValor: Currency;
      AQuantidade: Currency = 1;
      AQuantidadeUnidade: string = ''
    );
    function Clone(): TTopico;
    function ToTag(): NativeInt;
    property Referencia: TObject read FReferencia write FReferencia;
    property Imagem: SmallInt read FImagem write FImagem;
    property Tipo: SmallInt read FTipo write FTipo;
    property Id: string read FId write FId;
    property Codigo: string read FCodigo write FCodigo;
    property Nome: string read FNome write FNome;
    property Valor: Currency read FValor write SetValor;
    property Quantidade: Currency read FQuantidade write SetQuantidade;
    property QuantidadeUnidade: string read FQuantidadeUnidade write FQuantidadeUnidade;
    property SubTotal: Currency read FSubTotal;
    property Acrescimo: Currency read FAcrescimo write SetAcrescimo;
    property Frete: Currency read FFrete write SetFrete;
    property Servico: Currency read FServico write SetServico;
    property Desconto: Currency read FDesconto write SetDesconto;
    property Total: Currency read FTotal;
    property Observacoes: string read FObservacoes write FObservacoes;
    property Complemento: string read FComplemento write FComplemento;
    property eVendaItem: Boolean read GeteVendaItem;
    property eOcupacao: Boolean read GeteOcupacao;
  end;

type
  TRegistro = class
  private
    FTopicos: TObjectList<TTopico>;
  public
    constructor Create();
    destructor Destroy();
    property Topicos: TObjectList<TTopico> read FTopicos;
  end;

function GlobalFormatSettings(): TFormatSettings;

implementation

uses Lib.Funcoes, Lib.Excecoes;

var
  FGlobalFormatSettings: TFormatSettings;

function GlobalFormatSettings(): TFormatSettings;
var
  I: Integer;
begin
  if FGlobalFormatSettings.CurrencyString > '' then
    Result := FGlobalFormatSettings;
  FGlobalFormatSettings.CurrencyString := '$';
  FGlobalFormatSettings.CurrencyFormat := 0;
  FGlobalFormatSettings.CurrencyDecimals := 2;
  FGlobalFormatSettings.DateSeparator := '/';
  FGlobalFormatSettings.TimeSeparator := ':';
  FGlobalFormatSettings.ListSeparator := ',';
  FGlobalFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  FGlobalFormatSettings.LongDateFormat := 'dddd, dd MMMMM yyyy HH:mm:ss';
  FGlobalFormatSettings.TimeAMString := 'AM';
  FGlobalFormatSettings.TimePMString := 'PM';
  FGlobalFormatSettings.ShortTimeFormat := 'HH:mm';
  FGlobalFormatSettings.LongTimeFormat := 'HH:mm:ss';
  FGlobalFormatSettings.ThousandSeparator := ',';
  FGlobalFormatSettings.DecimalSeparator := '.';
  FGlobalFormatSettings.TwoDigitYearCenturyWindow := 50;
  FGlobalFormatSettings.NegCurrFormat := 0;
  Result := FGlobalFormatSettings;
end;


constructor TRegistro.Create();
begin
  FTopicos := TObjectList<TTopico>.Create();
end;

destructor TRegistro.Destroy();
begin
  FTopicos.Free();
end;

class function TTopico.FromData(P: Pointer): TTopico;
begin
  Result := TTopico(P);
end;

class function TTopico.FromTag(Tag: NativeInt): TTopico;
begin
  if Tag <= 0 then
    Lib.Excecoes.Erro('tag de ponteiro inválido');
  Result := TTopico(Pointer(Tag));
end;

//class procedure TTopico.Free(P: Pointer);
//begin
//  TObject(P^).Free();
//end;

class procedure TTopico.FreeTag(Tag: NativeInt);
var
  P: Pointer;
begin
  if Tag = 0 then
    Exit;
  //TTopico.Free(Pointer(Tag));
  P := Pointer(Tag);
  TObject(P^).Free();
end;

constructor TTopico.Create
(
  AReferencia: TObject;
  AImagem: SmallInt;
  ATipo: SmallInt;
  AId: string;
  ANome: string;
  AValor: Currency;
  AQuantidade: Currency = 1;
  AQuantidadeUnidade: string = ''
);
begin
  inherited Create();
  FReferencia := AReferencia;
  FImagem := AImagem;
  FTipo := ATipo;
  FId := AId;
  FNome := ANome;
  FValor := AValor;
  FQuantidade := AQuantidade;
  FQuantidadeUnidade := AQuantidadeUnidade;
  FSubTotal := 0.00;
  FAcrescimo := 0.00;
  FDesconto := 0.00;
  FTotal := 0.00;
  FObservacoes := '';
  AtualizaSubTotal();
  AtualizaTotal();
end;

function TTopico.Clone(): TTopico;
begin
  Result := TTopico.Create
    (
      FReferencia,
      FImagem,
      FTipo,
      FId,
      FNome,
      FValor,
      FQuantidade,
      FQuantidadeUnidade
    );
  Result.Acrescimo := FAcrescimo;
  Result.Desconto := FDesconto;
  Result.Observacoes := FObservacoes;
end;

function TTopico.ToTag(): NativeInt;
begin
  Result := NativeInt(Pointer(Self));
end;

function TTopico.GeteVendaItem(): Boolean;
begin
  Result := (FTipo = TP_VENDAITEM);
end;

function TTopico.GeteOcupacao(): Boolean;
begin
  Result := (FTipo = TP_OCUPACAO);
end;

procedure TTopico.AtualizaSubTotal();
begin
  FSubTotal := Lib.Funcoes.Trunca(FValor * FQuantidade, 2);
  AtualizaTotal();
end;

procedure TTopico.AtualizaTotal();
begin
  FTotal := FSubTotal + FAcrescimo + FFrete + FServico - FDesconto;
end;

procedure TTopico.SetValor(Value: Currency);
begin
  FValor := Value;
  AtualizaSubTotal();
end;

procedure TTopico.SetQuantidade(Value: Currency);
begin
  FQuantidade := Value;
  AtualizaSubTotal();
end;

procedure TTopico.SetAcrescimo(Value: Currency);
begin
  FAcrescimo := Value;
  AtualizaTotal();
end;

procedure TTopico.SetFrete(Value: Currency);
begin
  FFrete := Value;
  AtualizaTotal();
end;

procedure TTopico.SetServico(Value: Currency);
begin
  FServico := Value;
  AtualizaTotal();
end;

procedure TTopico.SetDesconto(Value: Currency);
begin
  FDesconto := Value;
  AtualizaTotal();
end;

function ComboItem(id: Integer; nome: string): TComboItem;
begin
  Result.id := id;
  Result.nome := nome;
end;

end.

