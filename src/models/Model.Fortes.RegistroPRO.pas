unit Model.Fortes.RegistroPRO;

interface

uses
  System.Classes, Fortes.IRegistro, Model.Fortes.RegistroOUM, Generics.Collections;

type
  TTipoUnidadeMedidaDIEF = (
    tsLitro = 1, tsMetro = 2, tsMetroCubico = 3, tsMetroQuadrado = 4, tsPar = 5,
    tsQuilograma = 6, tsQuilowattHora = 7, tsUnidade = 8
  );

type
  TUnidadeMedidaCENFOP = (
    tsArroba = 1, tsBarril = 2, tsBalde = 3, tsCento = 4, tsCartela = 5, tsCaixa = 6,
    tsCaixote = 7, tsDuzia = 8, tsEmbalagem = 9, tsEnvelope = 10, tsFardo = 11,
    tsGarrafa = 12, tsGalao = 13, tsQuilogramaCENFOP = 14, tsQuloLitro = 15, tsQuiloWattHoraCENFOP = 16
    );


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
    FUnidadeMedidaDIEF: TtipoUnidadeMedidaDIEF;
    FUnidadeMedidaCENFOP: TUnidadeMedidaCENFOP;
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
    property OutraUnidade: TList<TRegistroOUM> read FOutraUnidade write FOutraUnidade; //VER
    property UnidadeMedidaDIEF: TTipoUnidadeMedidaDIEF read FUnidadeMedidaDIEF write FUnidadeMedidaDIEF;
    property UnidadeMedidaCENFOP: TUnidadeMedidaCENFOP read FUnidadeMedidaCENFOP write FUnidadeMedidaCENFOP;

  end;



//  TTipoSituacao = (
//    tsNormal = 0, tsCancelado = 1, tsNormalExtemporaneo = 2, tsCanceladoExtemporaneo = 3,
//     = 4, tsNFeInutilizada = 5, tsDFComplementar = 6, tsDFRegimeEspecial = 8);
//
//  TTipoGNRE = (tgSubstituicao = 1, tgImportacao = 2, tgDifal = 3);

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
  var Produto := Format('%s|%s|%s|%s|%s|%s|%s|%s|%s', [
    FTipoRegistro,
    FCodigo,
    FDescricao,
    FCodigoUtilizadoEstab,
    FCodigoNCM,
    FUnidadeMedida,
    FOutraUnidade,
    IntTostr(Integer(FUnidadeMedidaDIEF)),
    IntTostr(Integer(FUnidadeMedidaCENFOP))
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

