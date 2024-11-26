unit Model.Fortes.RegistroCAB;

interface

uses Fortes.IRegistro;

type
  TRegistroCAB = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FVersaoLayout: string;
    FSistemaOrigem: string;
    FDataGeracao: TDateTime;
    FEmpresa: string;
    FDataInicial: TDateTime;
    FDataFinal: TDateTime;
    FComentario: string;
    FAliquotasEspecificas: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property VersaoLayout: string read FVersaoLayout write FVersaoLayout;
    property SistemaOrigem: string read FSistemaOrigem write FSistemaOrigem;
    property DataGeracao: TDateTime read FDataGeracao write FDataGeracao;
    property Empresa: string read FEmpresa write FEmpresa;
    property DataInicial: TDateTime read FDataInicial write FDataInicial;
    property DataFinal: TDateTime read FDataFinal write FDataFinal;
    property Comentario: string read FComentario write FComentario;
    property AliquotasEspecificas: string read FAliquotasEspecificas write FAliquotasEspecificas;
  end;

implementation

uses
  System.SysUtils;

constructor TRegistroCAB.Create;
begin
  FTipoRegistro  := 'CAB';
  FVersaoLayout  := '178';
  FSistemaOrigem := 'Constel';
end;

function TRegistroCAB.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FVersaoLayout,
    FSistemaOrigem,
    FormatDateTime('yyyymmdd', FDataGeracao),
    FEmpresa,
    FormatDateTime('yyyymmdd', FDataInicial),
    FormatDateTime('yyyymmdd', FDataFinal),
    FComentario,
    FAliquotasEspecificas
  ]);
end;

end.
