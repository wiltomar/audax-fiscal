unit Model.Forma;

interface

uses
  Lib.Sistema.Tipos, Model.Base;

const
  FORMA_SITUACAO_ATIVA = 1;
  FORMA_SITUACAO_SUSPENSA = 410;
  FORMA_SITUACAO_DESATIVADA = 510;

const
  FORMA_NATUREZA_ENTRADA = +1;
  FORMA_NATUREZA_SAIDA = -1;

const
  FORMA_ESPECIE_DINHEIRO = 1;
  FORMA_ESPECIE_PROMISSORIA = 20;
  FORMA_ESPECIE_CHEQUE = 30;
  FORMA_ESPECIE_BOLETO = 40;
  FORMA_ESPECIE_CARTAOCREDITO = 110;
  FORMA_ESPECIE_CARTAODEBITO = 120;
  FORMA_ESPECIE_TIQUETE = 130;
  FORMA_ESPECIE_GIFT = 140;
  FORMA_ESPECIE_CARTEIRADIGITAL = 150;
  FORMA_ESPECIE_PAGAMENTOINTEGRACAO = 160;
  FORMA_ESPECIE_TRANSFERENCIABANCARIA = 210;
  FORMA_ESPECIE_PIX = 230;
  FORMA_ESPECIE_VALEFUNCIONARIO = 310;
  FORMA_ESPECIE_DEVOLUCAO = 410;
  FORMA_ESPECIE_VOUCHER = 420;
  FORMA_ESPECIE_DEDUCAOX = 430;

const
  FORMA_ESPECIE_LISTA: array[0..13] of TComboItem =
  (
    (id: FORMA_ESPECIE_DINHEIRO; nome: 'Dinheiro'),
    (id: FORMA_ESPECIE_PROMISSORIA; nome: 'Crediário'),
    (id: FORMA_ESPECIE_CHEQUE; nome: 'Cheque'),
    (id: FORMA_ESPECIE_BOLETO; nome: 'Boleto'),
    (id: FORMA_ESPECIE_CARTAOCREDITO; nome: 'Cartão de Crédito'),
    (id: FORMA_ESPECIE_CARTAODEBITO; nome: 'Cartão de Débito'),
    (id: FORMA_ESPECIE_TIQUETE; nome: 'Tíquete'),
    (id: FORMA_ESPECIE_GIFT; nome: 'Gift'),
    (id: FORMA_ESPECIE_CARTEIRADIGITAL; nome: 'Carteira Digital'),
    (id: FORMA_ESPECIE_PAGAMENTOINTEGRACAO; nome: 'Pagamento via Integração'),
    (id: FORMA_ESPECIE_TRANSFERENCIABANCARIA; nome: 'Transferência Bancária'),
    (id: FORMA_ESPECIE_PIX; nome: 'PIX'),
    (id: FORMA_ESPECIE_VALEFUNCIONARIO; nome: 'Vale-Funcionário'),
    (id: FORMA_ESPECIE_DEVOLUCAO; nome: 'Devolução')
  );

const
  AFORMA_ESPECIE_PAGAMENTOELETRONICO = [FORMA_ESPECIE_CARTAOCREDITO, FORMA_ESPECIE_CARTAODEBITO, FORMA_ESPECIE_PIX, FORMA_ESPECIE_CARTEIRADIGITAL];
  AFORMA_ESPECIE_CARTAO = [FORMA_ESPECIE_CARTAOCREDITO, FORMA_ESPECIE_CARTAODEBITO];
  AFORMA_ESPECIE_CARTEIRADIGITAL = [FORMA_ESPECIE_CARTEIRADIGITAL, FORMA_ESPECIE_PIX];

const
  FORMA_CDM_NENHUMA = 0;
  FORMA_CDM_PIX = 10;
  FORMA_CDM_PICPAY = 110;
  FORMA_CDM_AME = 120;
  FORMA_CDM_MERCADOPAGO = 130;
  FORMA_CDM_ITI = 140;
  FORMA_CDM_PAGBANK = 150;
  FORMA_CDM_GOOGLEPAY = 210;
  FORMA_CDM_APPLEPAY = 220;
  FORMA_CDM_SAMSUNGPAY = 230;

const
  FORMA_CDM_LISTA: array[0..9] of TComboItem =
  (
    (id: FORMA_CDM_NENHUMA; nome: '(Não informada)'),
    (id: FORMA_CDM_PIX; nome: 'PIX'),
    (id: FORMA_CDM_PICPAY; nome: 'PicPay'),
    (id: FORMA_CDM_AME; nome: 'Ame'),
    (id: FORMA_CDM_MERCADOPAGO; nome: 'Mercado Pago'),
    (id: FORMA_CDM_ITI; nome: 'Iti'),
    (id: FORMA_CDM_PAGBANK; nome: 'PagBank'),
    (id: FORMA_CDM_GOOGLEPAY; nome: 'Google Pay'),
    (id: FORMA_CDM_APPLEPAY; nome: 'Apple Pay'),
    (id: FORMA_CDM_SAMSUNGPAY; nome: 'Sansumg Pay')
  );

type
  TFormaPlano = class;

  TForma = class(TBaseTR)
  private
    fsituacao: SmallInt;
    fespecie: SmallInt;
    fcdm: SmallInt;
    fbaixa: Boolean;
    ftef: Boolean;
    ftroco: Boolean;
    fparceiroRequerido: boolean;
    fformaPlanos: TArray<TFormaPlano>;
    function getNomeAbreviado(): string;
    function getFormaPlanoPadrao(): TFormaPlano;
    function getDeducao(): Boolean;
  public
    constructor Create(aid: string; anome: string); overload;
    destructor Destroy();
    property situacao: SmallInt read fsituacao write fsituacao;
    property especie: SmallInt read fespecie write fespecie;
    property cdm: SmallInt read fcdm write fcdm;
    property baixa: Boolean read fbaixa write fbaixa;
    property tef: Boolean read ftef write ftef;
    property troco: Boolean read ftroco write ftroco;
    property parceiroRequerido: boolean read fparceiroRequerido write fparceiroRequerido;
    property formaPlanos: TArray<TFormaPlano> read fformaPlanos write fformaPlanos;
    property formaPlanoPadrao: TFormaPlano read getFormaPlanoPadrao;
    property deducao: Boolean read getDeducao;
    property nomeAbreviado: string read getNomeAbreviado;
  end;
  TFormas = TArray<TForma>;

  TFormaPlano = class(TBaseR)
  private
    fsituacao: SmallInt;
    fpadrao: Boolean;
    fedita: Boolean;
  public
    property situacao: SmallInt read fsituacao write fsituacao;
    property padrao: Boolean read fpadrao write fpadrao;
    property edita: Boolean read fedita write fedita;
  end;
  TFormaPlanos = TArray<TFormaPlano>;

implementation

uses System.SysUtils, Lib.Excecoes, Lib.Funcoes;

constructor TForma.Create(aid: string; anome: string);
begin
  inherited Create(aid, anome);
  fformaPlanos := [];
end;

destructor TForma.Destroy();
begin
  for var formaPlano in fformaPlanos do
    formaPlano.Free();
  fformaPlanos := [];
  inherited;
end;

function TForma.getDeducao(): Boolean;
begin
  Result := Includes(FEspecie, [FORMA_ESPECIE_DEVOLUCAO, FORMA_ESPECIE_VOUCHER]);
end;

function TForma.getNomeAbreviado(): string;
const
  PREFIXOS: array[0..1] of string = ('recebimento em', 'carteira digital');
begin
  if FEspecie = FORMA_ESPECIE_DINHEIRO then
    Exit('Dinheiro');
  Result := Nome;
  for var Prefixo in PREFiXOS do
    if Result.ToLower().StartsWith(Prefixo) then
      Result := Result.Substring(Prefixo.Length).Trim();
end;

function TForma.GetFormaPlanoPadrao(): TFormaPlano;
begin
  if Length(fformaPlanos) = 0 then
    Lib.Excecoes.Erro('não há planos de pagamento vinculados para a forma de pagamento "' + nome + '"');
  for Result in fformaPlanos do
    if Result.padrao then
      Exit;
  Result := fformaPlanos[0];
end;

end.
