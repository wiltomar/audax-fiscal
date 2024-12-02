unit Model.Fortes.RegistroCPE;

{
 CPE (pai)
    PCE (filho)
    CFI (fihlo)
}

interface

uses
  System.Classes, System.SysUtils, Fortes.IRegistro, Generics.Collections, System.StrUtils, Model.Fortes.RegistroPCE;

type
  TRegistroCPE = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FEstabelecimento: integer;
    FData: TdateTime;
    FChaveEletronica: string;
    FNumeroDeSerieDoECF: integer;
    FNumero: integer;
    FSituacao: integer;
    FDestinatįrio: integer;
    FValorTotal: double;
    FDescontos: double;
    FOutrasDespesas: double;
    FSubstituicao: double;
    FAcrescimo: double;
    FTroco: double;
    FReceitaTributavelCOFINS: double;
    FReceitaTributavelPIS: double;
    FReceitaTributįvelCSL1: double;
    FReceitaTributavelCSL2: double;
    FReceitaTributįvelIRPJ1: double;
    FReceitaTributįvelIRPJ2: double;
    FReceitaTributįvelIRPJ3: double;
    FReceitaTributįvelIRPJ4: double;
    FFatura: string;
    FObservacao: string;
    FCodigoContabil: string;
    FInformacoesAdicionais: string;
    FTotalServicoISS: double;
    FCOFINSRetidoNaFonte: double;
    FPISRetidoNaFonte: double;
    FCSLRetidoNaFonte: double;
    FIRPJRetidoNaFonte: double;
    FINSSRetidoNaFonte: double;
    FISSRetido: boolean;
    FPrestadorDeServicosEmObradeConstrucaoCivil: SmallInt;
    FCNO: string;
    FRegistroPCE: TList<TRegistroPCE>;
    property RegistroPCE: TList<TRegistroPCE> read FRegistroPCE write FRegistroPCE; //VER
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Estabelecimento: integer read FEstabelecimento write FEstabelecimento;
    property Data: TdateTime read FData write FData;
    property ChaveEletronica: string read FChaveEletronica write FChaveEletronica;
    property NumeroDeSerieDoECF: integer read FNumeroDeSerieDoECF write FNumeroDeSerieDoECF;
    property Numero: integer read FNumero write FNumero;
    property Situacao: integer read FSituacao write FSituacao;
    property Destinatario: integer read FDestinatįrio write FDestinatįrio;
    property ValorTotal: double read FValorTotal write FValorTotal;
    property Descontos: double read FDescontos write FDescontos;
    property OutrasDespesas: double read FOutrasDespesas write FOutrasDespesas;
    property Substituicao: double read FSubstituicao write FSubstituicao;
    property Acrescimo: double read FAcrescimo write FAcrescimo;
    property Troco: double read FTroco write FTroco;
    property ReceitaTributavelCOFINS: double read FReceitaTributavelCOFINS write FReceitaTributavelCOFINS;
    property ReceitaTributavelPIS: double read FReceitaTributavelPIS write FReceitaTributavelPIS;
    property ReceitaTributįvelCSL1: double read FReceitaTributįvelCSL1 write FReceitaTributįvelCSL1;
    property ReceitaTributavelCSL2: double read FReceitaTributavelCSL2 write FReceitaTributavelCSL2;
    property ReceitaTributįvelIRPJ1: double read FReceitaTributįvelIRPJ1 write FReceitaTributįvelIRPJ1;
    property ReceitaTributįvelIRPJ2: double read FReceitaTributįvelIRPJ2 write FReceitaTributįvelIRPJ2;
    property ReceitaTributįvelIRPJ3: double read FReceitaTributįvelIRPJ3 write FReceitaTributįvelIRPJ3;
    property ReceitaTributįvelIRPJ4: double read FReceitaTributįvelIRPJ4 write FReceitaTributįvelIRPJ4;
    property Fatura: string read FFatura write FFatura;
    property Observacao: string read FObservacao write FObservacao;
    property CodigoContabil: string read FCodigoContabil write FCodigoContabil;
    property InformacoesAdicionais: string read FInformacoesAdicionais write FInformacoesAdicionais;
    property TotalServicoISS: double read FTotalServicoISS write FTotalServicoISS;
    property COFINSRetidoNaFonte: double read FCOFINSRetidoNaFonte write FCOFINSRetidoNaFonte;
    property PISRetidoNaFonte: double read FPISRetidoNaFonte write FPISRetidoNaFonte;
    property CSLRetidoNaFonte: double read FCSLRetidoNaFonte write FCSLRetidoNaFonte;
    property IRPJRetidoNaFonte: double read FIRPJRetidoNaFonte write FIRPJRetidoNaFonte;
    property INSSRetidoNaFonte: double read FINSSRetidoNaFonte write FINSSRetidoNaFonte;
    property ISSRetido: boolean read FISSRetido write FISSRetido;
    property PrestadorDeServicosEmObradeConstrucaoCivil: SmallInt read FPrestadorDeServicosEmObradeConstrucaoCivil write FPrestadorDeServicosEmObradeConstrucaoCivil;
    property CNO: string read FCNO write FCNO;
  public
    constructor Create;
    destructor Destroy;
    function GerarLinha: string;
    procedure AdicionarPCE(pce: TRegistroPCE);
  end;

implementation

{ TRegistroCPE }
procedure TRegistroCPE.AdicionarPCE(pce: TRegistroPCE);
begin
  FRegistroPCE.Add(pce);
end;

constructor TRegistroCPE.Create;
begin
  FTipoRegistro := 'CPE';
  FRegistroPCE.Create;
end;


destructor TRegistroCPE.Destroy;
begin
  FRegistroPCE.Destroy;
  inherited Destroy;
end;

function TRegistroCPE.GerarLinha: string;
var
  Linha: TStringList;
begin
  var Cpe := Format(
    '%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%S|%S|%s|%s|%s|%s|' +
    '%s|%s|%s|%s|%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FEstabelecimento,
    FormatDateTime('yyymmdd', FData),
    FChaveEletronica,
    FNumeroDeSerieDoECF,
    FNumero,
    FSituacao,
    FDestinatįrio,
    FormatFloat('#0.00', FValorTotal),
    FormatFloat('#0.00', FDescontos),
    FormatFloat('#0.00', FOutrasDespesas),
    FormatFloat('#0.00', FSubstituicao),
    FormatFloat('#0.00', FAcrescimo),
    FormatFloat('#0.00', FTroco),
    FormatFloat('#0.00', FReceitaTributavelCOFINS),
    FormatFloat('#0.00', FReceitaTributavelPIS),
    FormatFloat('#0.00', FReceitaTributįvelCSL1),
    FormatFloat('#0.00', FReceitaTributavelCSL2),
    FormatFloat('#0.00', FReceitaTributįvelIRPJ1),
    FormatFloat('#0.00', FReceitaTributįvelIRPJ2),
    FormatFloat('#0.00', FReceitaTributįvelIRPJ3),
    FormatFloat('#0.00', FReceitaTributįvelIRPJ4),
    FFatura,
    FObservacao,
    FCodigoContabil,
    FInformacoesAdicionais,
    FormatFloat('#0.00', FTotalServicoISS),
    FormatFloat('#0.00', FCOFINSRetidoNaFonte),
    FormatFloat('#0.00', FPISRetidoNaFonte),
    FormatFloat('#0.00', FCSLRetidoNaFonte),
    FormatFloat('#0.00', FIRPJRetidoNaFonte),
    FormatFloat('#0.00', FINSSRetidoNaFonte),
    IfThen(FISSRetido, 'S', 'N' ),
    FPrestadorDeServicosEmObradeConstrucaoCivil,
    FCNO
  ]);

  Linha := TStringList.Create;
  try
    for var pce in FRegistroPCE do
    begin
      Linha.Add(pce.GerarLinha)
    end;
    Result := Cpe + sLineBreak + Linha.Text;
  finally
    Linha.Free;
  end;
end;


end.
