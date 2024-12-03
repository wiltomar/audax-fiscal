unit Model.Fortes.RegistroDNM;

interface

uses Fortes.IRegistro, System.StrUtils;

type
  TRegistroDNM = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FBaseDeCalculo: double;
    FAliquotaDeOrigem: double;
    FAliquotaInterna: double;
    FValorTotalBahiaMaranhao: double;
    FAliquotaDeCredito: double;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property BaseDeCalculo: double read FBaseDeCalculo write FBaseDeCalculo;
    property AliquotaDeOrigem: double read FAliquotaDeOrigem write FAliquotaDeOrigem;
    property AliquotaInterna: double read FAliquotaInterna write FAliquotaInterna;
    property ValorTotalBahiaMaranhao: double read FValorTotalBahiaMaranhao write FValorTotalBahiaMaranhao;
    property AliquotaDeCredito: double read FAliquotaDeCredito write FAliquotaDeCredito;
  end;

implementation
uses
  SysUtils;

constructor TRegistroDNM.Create;
begin
  FTipoRegistro := 'DNM';
end;

function TRegistroDNM.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FormatFloat('#0.00', FBaseDeCalculo),
    FormatFloat('#0.00', FAliquotaDeOrigem),
    FormatFloat('#0.00', FAliquotaInterna),
    FormatFloat('#0.00', FValorTotalBahiaMaranhao),
    FormatFloat('#0.00', FAliquotaDeCredito)
  ]);
end;

end.
