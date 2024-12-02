unit Model.Fortes.RegistroCPE;

{
 CPE (pai)
    PCE (filho)
    CFI (fihlo)
}

interface

uses
  System.Classes, System.SysUtils, Fortes.IRegistro, Generics.Collections, System.StrUtils, Model.Fortes.RegistroPCE,
  Model.Fortes.RegistroCFI;

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
    FDestinat�rio: integer;
    FValorTotal: double;
    FDescontos: double;
    FOutrasDespesas: double;
    FSubstituicao: double;
    FAcrescimo: double;
    FTroco: double;
    FReceitaTributavelCOFINS: double;
    FReceitaTributavelPIS: double;
    FReceitaTribut�velCSL1: double;
    FReceitaTributavelCSL2: double;
    FReceitaTribut�velIRPJ1: double;
    FReceitaTribut�velIRPJ2: double;
    FReceitaTribut�velIRPJ3: double;
    FReceitaTribut�velIRPJ4: double;
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
    FRegistroCFI: TList<TRegistroCFI>;
    property RegistroPCE: TList<TRegistroPCE> read FRegistroPCE write FRegistroPCE;
    property RegistroCFI: TList<TRegistroCFI> read FRegistroCFI write FRegistroCFI;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Estabelecimento: integer read FEstabelecimento write FEstabelecimento;
    property Data: TdateTime read FData write FData;
    property ChaveEletronica: string read FChaveEletronica write FChaveEletronica;
    property NumeroDeSerieDoECF: integer read FNumeroDeSerieDoECF write FNumeroDeSerieDoECF;
    property Numero: integer read FNumero write FNumero;
    property Situacao: integer read FSituacao write FSituacao;
    property Destinatario: integer read FDestinat�rio write FDestinat�rio;
    property ValorTotal: double read FValorTotal write FValorTotal;
    property Descontos: double read FDescontos write FDescontos;
    property OutrasDespesas: double read FOutrasDespesas write FOutrasDespesas;
    property Substituicao: double read FSubstituicao write FSubstituicao;
    property Acrescimo: double read FAcrescimo write FAcrescimo;
    property Troco: double read FTroco write FTroco;
    property ReceitaTributavelCOFINS: double read FReceitaTributavelCOFINS write FReceitaTributavelCOFINS;
    property ReceitaTributavelPIS: double read FReceitaTributavelPIS write FReceitaTributavelPIS;
    property ReceitaTribut�velCSL1: double read FReceitaTribut�velCSL1 write FReceitaTribut�velCSL1;
    property ReceitaTributavelCSL2: double read FReceitaTributavelCSL2 write FReceitaTributavelCSL2;
    property ReceitaTribut�velIRPJ1: double read FReceitaTribut�velIRPJ1 write FReceitaTribut�velIRPJ1;
    property ReceitaTribut�velIRPJ2: double read FReceitaTribut�velIRPJ2 write FReceitaTribut�velIRPJ2;
    property ReceitaTribut�velIRPJ3: double read FReceitaTribut�velIRPJ3 write FReceitaTribut�velIRPJ3;
    property ReceitaTribut�velIRPJ4: double read FReceitaTribut�velIRPJ4 write FReceitaTribut�velIRPJ4;
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
    procedure AdicionarCFI(cfi: TRegistroCFI);
  end;

implementation

{ TRegistroCPE }
procedure TRegistroCPE.AdicionarCFI(cfi: TRegistroCFI);
begin
  FRegistroCFI.Add(cfi);
end;

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
  LinhaPCE, LinhaCFI: TStringList;
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
    FDestinat�rio,
    FormatFloat('#0.00', FValorTotal),
    FormatFloat('#0.00', FDescontos),
    FormatFloat('#0.00', FOutrasDespesas),
    FormatFloat('#0.00', FSubstituicao),
    FormatFloat('#0.00', FAcrescimo),
    FormatFloat('#0.00', FTroco),
    FormatFloat('#0.00', FReceitaTributavelCOFINS),
    FormatFloat('#0.00', FReceitaTributavelPIS),
    FormatFloat('#0.00', FReceitaTribut�velCSL1),
    FormatFloat('#0.00', FReceitaTributavelCSL2),
    FormatFloat('#0.00', FReceitaTribut�velIRPJ1),
    FormatFloat('#0.00', FReceitaTribut�velIRPJ2),
    FormatFloat('#0.00', FReceitaTribut�velIRPJ3),
    FormatFloat('#0.00', FReceitaTribut�velIRPJ4),
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

  LinhaPCE := TStringList.Create;
  try
    // OBS: Dentro dos la��es ver as condi��es para trazer apenas os dos registro CPE que faz rela��o com PCE e CFI
    for var pce in FRegistroPCE do
    begin
      LinhaPCE.Add(pce.GerarLinha)
    end;
    for var cfi in FRegistroCFI do
    begin
      LinhaCFI.Add(cfi.GerarLinha)
    end;
    //Result := Cpe + sLineBreak + Linha.Text;
    Result := Cpe + sLineBreak +  LinhaPCE.Text + sLineBreak + LinhaCFI.Text;
  finally
    LinhaPCE.Free;
    LinhaCFI.Free;
  end;
end;


end.
