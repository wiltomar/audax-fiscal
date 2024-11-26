unit Model.Fortes.RegistroPAJ;

interface

uses Fortes.IRegistro;

type
  TRegistroPAJ = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroProcesso: string;
    FDataInicio: TDateTime;
    FDataFim: TDateTime;
    FDescricao: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroProcesso: string read FNumeroProcesso write FNumeroProcesso;
    property DataInicio: TDateTime read FDataInicio write FDataInicio;
    property DataFim: TDateTime read FDataFim write FDataFim;
    property Descricao: string read FDescricao write FDescricao;
  end;

implementation

uses
  SysUtils;

constructor TRegistroPAJ.Create;
begin
  FTipoRegistro := 'PAJ';
end;

function TRegistroPAJ.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FNumeroProcesso,
    FormatDateTime('yyyymmdd', FDataInicio),
    FormatDateTime('yyyymmdd', FDataFim),
    FDescricao
  ]);
end;

end.

