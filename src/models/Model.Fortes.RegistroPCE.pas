unit Model.Fortes.RegistroPCE;

interface

uses
  System.Classes, System.SysUtils, Fortes.IRegistro, Generics.Collections, System.StrUtils;

type
  TRegistroPCE = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FProduto: integer;
    FQuantidade: double;
    FUnidadeDeMedida: string;
    FCFOP: integer;
    FValorBruto: double;
    FValorDoDesconto: double;
    FOutrasDespesas: double;
    FAcrescimo: double;
    FCSOSNA: SmallInt;
    FCSOSNB: SmallInt;
    FCSTA: SmallInt;
    FCSTB: SmallInt;
    FBaseDeCalculo: double;
    FAlíquota: double;
    FCSTCOFINS: SmallInt;
    FBaseDeCalculoCOFINS: double;
    FTipoDeCalculoCOFINS: SmallInt;
    FAliquotaCOFINSPercentual: double;
    FAliquotaCOFINSValor: double;
    FValorCOFINS: double;
    FNaturezaDaReceitaCOFINS: integer;
    FCSTPIS: SmallInt;
    FBaseDeCalculoPIS: double;
    FTipoDeCalculoPIS: SmallInt;
    FAliquotaPISPercentual: double;
    FAlíquotaPIS: double;
    FValorPIS: double;
    FNaturezaDaReceitaPIS: integer;
    FCodigoContabil: string;
    FExclusaoDaBCPISCOFINS: double;
    FCodigoDeAjusteFiscal: integer;
    FBaseDeCalculoDeAjusteDeICMS: double;
    FAliquotaDeAjusteDeICMS: double;
    FOutrosValoresDeAjusteDeICMS: double;
    FValorDeAjusteDeICMS: double;
    FDecretoAM: boolean;
    function read: boolean;
     property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
     property Produto: integer read FProduto write FProduto;
     property Quantidade: double read FQuantidade write FQuantidade;
     property UnidadeDeMedida: string read FUnidadeDeMedida write FUnidadeDeMedida;
     property CFOP: integer read FCFOP write FCFOP;
     property ValorBruto: double read FValorBruto write FValorBruto;
     property ValorDoDesconto: double read FValorDoDesconto write FValorDoDesconto;
     property OutrasDespesas: double read FOutrasDespesas write FOutrasDespesas;
     property Acrescimo: double read FAcrescimo write FAcrescimo;
     property CSOSNA: SmallInt read FCSOSNA write FCSOSNA;
     property CSOSNB: SmallInt read FCSOSNB write FCSOSNB;
     property CSTA: SmallInt read FCSTA write FCSTA;
     property CSTB: SmallInt read FCSTB write FCSTB;
     property BaseDeCalculo: double read FBaseDeCalculo write FBaseDeCalculo;
     property Alíquota: double read FAlíquota write FAlíquota;
     property CSTCOFINS: SmallInt read FCSTCOFINS write FCSTCOFINS;
     property BaseDeCalculoCOFINS: double read FBaseDeCalculoCOFINS write FBaseDeCalculoCOFINS;
     property TipoDeCalculoCOFINS: SmallInt read FTipoDeCalculoCOFINS write FTipoDeCalculoCOFINS;
     property AliquotaCOFINSPercentual: double read FAliquotaCOFINSPercentual write FAliquotaCOFINSPercentual;
     property AliquotaCOFINSValor: double read FAliquotaCOFINSValor write FAliquotaCOFINSValor;
     property ValorCOFINS: double read FValorCOFINS write FValorCOFINS;
     property NaturezaDaReceitaCOFINS: integer read FNaturezaDaReceitaCOFINS write FNaturezaDaReceitaCOFINS;
     property CSTPIS: SmallInt read FCSTPIS write FCSTPIS;
     property BaseDeCalculoPIS: double read FBaseDeCalculoPIS write FBaseDeCalculoPIS;
     property TipoDeCalculoPIS: SmallInt read FTipoDeCalculoPIS write FTipoDeCalculoPIS;
     property AliquotaPISPercentual: double read FAliquotaPISPercentual write FAliquotaPISPercentual;
     property AlíquotaPISValor: double read FAlíquotaPIS write FAlíquotaPIS;
     property ValorPIS: double read FValorPIS write FValorPIS;
     property NaturezaDaReceitaPIS: integer read FNaturezaDaReceitaPIS write FNaturezaDaReceitaPIS;
     property CodigoContabil: string read FCodigoContabil write FCodigoContabil;
     property ExclusaoDaBCPISCOFINS: double read FExclusaoDaBCPISCOFINS write FExclusaoDaBCPISCOFINS;
     property CodigoDeAjusteFiscal: integer read FCodigoDeAjusteFiscal write FCodigoDeAjusteFiscal;
     property BaseDeCalculoDeAjusteDeICMS: double read FBaseDeCalculoDeAjusteDeICMS write FBaseDeCalculoDeAjusteDeICMS;
     property AliquotaDeAjusteDeICMS: double read FAliquotaDeAjusteDeICMS write FAliquotaDeAjusteDeICMS;
     property ValorDeAjusteDeICMS: double read FValorDeAjusteDeICMS write FValorDeAjusteDeICMS;
     property OutrosValoresDeAjusteDeICMS: double read FOutrosValoresDeAjusteDeICMS write FOutrosValoresDeAjusteDeICMS;
     property DecretoAM: boolean read FDecretoAM write FDecretoAM;
  public
    constructor Create;
    destructor Destroy;
    function GerarLinha: string;
  end;


implementation

{ TRegistroPCE }

constructor TRegistroPCE.Create;
begin
  FTipoRegistro := 'PCE';
end;

destructor TRegistroPCE.Destroy;
begin
  inherited Destroy;
end;

function TRegistroPCE.GerarLinha: string;
begin
   var Pce := Format(
    '%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|%S|%S|%s|%s|%s|%s|' +
    '%s|%s|%s|%s|%s|%s|%s|%s|%s|%|%s|', [
     FTipoRegistro,
     FProduto,
     FormatFloat('#0.00', FQuantidade),
     FUnidadeDeMedida,
     FCFOP,
     FormatFloat('#0.00', FValorBruto),
     FormatFloat('#0.00', FValorDoDesconto),
     FormatFloat('#0.00', FOutrasDespesas),
     FormatFloat('#0.00', FAcrescimo),
     FCSOSNA,
     FCSOSNB,
     FCSTA,
     FCSTB,
     FormatFloat('#0.00', FBaseDeCalculo),
     FormatFloat('#0.00', FAlíquota),
     FCSTCOFINS,
     FormatFloat('#0.00', FBaseDeCalculoCOFINS),
     FTipoDeCalculoCOFINS,
     FormatFloat('#0.00', FAliquotaCOFINSPercentual),
     FormatFloat('#0.00', FAliquotaCOFINSValor),
     FormatFloat('#0.00', FValorCOFINS),
     FNaturezaDaReceitaCOFINS,
     FCSTPIS,
     FormatFloat('#0.00', FBaseDeCalculoPIS),
     FTipoDeCalculoPIS,
     FormatFloat('#0.00', FAliquotaPISPercentual),
     FormatFloat('#0.00', FAlíquotaPIS),
     FormatFloat('#0.00', FValorPIS),
     FNaturezaDaReceitaPIS,
     FCodigoContabil,
     FormatFloat('#0.00', FExclusaoDaBCPISCOFINS),
     FCodigoDeAjusteFiscal,
     FormatFloat('#0.00', FBaseDeCalculoDeAjusteDeICMS),
     FormatFloat('#0.00', FAliquotaDeAjusteDeICMS),
     FormatFloat('#0.00', FOutrosValoresDeAjusteDeICMS),
     FormatFloat('#0.00', FValorDeAjusteDeICMS),
     IfThen(FDecretoAM, 'S', 'N')
    ]);
end;


function TRegistroPCE.read: boolean;
begin
  Result := FDecretoAM;
end;

end.
