unit Model.Fortes.RegistroPRO;

interface

uses
  System.Classes, Fortes.IRegistro, Model.Fortes.RegistroOUM, Generics.Collections;

type
  TRegistroPRO = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigo: LongInt;
    FDescricao: string;
    FCodigoUtilizadoEstab: string;
    FCodigoNCM: string;
    FUnidadeMedida: string;
    FOutraUnidade: TList<TRegistroOUM>;
  public
    constructor Create;
    destructor Destroy;
    function GerarLinha: string;
    procedure AdicionarOutraUnidade(OutraUnidade: TRegistroOUM);
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Codigo: LongInt read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property CodigoUtilizadoEstab: string read FCodigoUtilizadoEstab write FCodigoUtilizadoEstab;
    property CodigoNCM: string read FCodigoNCM write FCodigoNCM;
    property UnidadeMedida: string read FUnidadeMedida write FUnidadeMedida;
  //  property OutraUnidade: TList<TRegistroOUM> read FOutraUnidade write FOutraUnidade;
  end;

implementation

uses
  SysUtils;

procedure TRegistroPRO.AdicionarOutraUnidade(OutraUnidade: TRegistroOUM);
begin
  FOutraUnidade.Add(OutraUnidade);
end;

constructor TRegistroPRO.Create;
begin
  FTipoRegistro := 'PRO';
  FOutraUnidade.Create;
end;

destructor TRegistroPRO.Destroy;
begin
  OutraUnidade.Destroy;
  inherited Destroy;
end;

function TRegistroPRO.GerarLinha: string;
var
  Linha: TStringList;
begin
  var Produto := Format('%s|%s|%s|%s|%s|%s', [
    FTipoRegistro,
    FCodigo,
    FDescricao,
    FCodigoUtilizadoEstab,
    FCodigoNCM,
    FUnidadeMedida
  ]);

  Linha := TStringList.Create;
  try
    for var Unidade in FOutraUnidade do
    begin
      Linha.Add(Unidade.GerarLinha)
    end;
    Result := Produto + sLineBreak + Linha.Text;
  finally
    Linha.Free;
  end;
end;

end.

