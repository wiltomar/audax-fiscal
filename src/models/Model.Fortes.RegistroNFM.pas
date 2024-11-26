unit Model.Fortes.RegistroNFM;

interface

uses Fortes.IRegistro;

type
  TRegistroNFM = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroNota: string;
    FDataEmissao: TDateTime;
    FValorTotal: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroNota: string read FNumeroNota write FNumeroNota;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property ValorTotal: string read FValorTotal write FValorTotal;
  end;

implementation

uses
  SysUtils;

constructor TRegistroNFM.Create;
begin
  FTipoRegistro := 'NFM';
end;

function TRegistroNFM.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|', [
    FTipoRegistro,
    FNumeroNota,
    FormatDateTime('yyyymmdd', FDataEmissao),
    FValorTotal
  ]);
end;

end.

