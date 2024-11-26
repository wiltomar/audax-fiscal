unit Model.Fortes.RegistroDIP;

interface

uses Fortes.IRegistro;

type
  TRegistroDIP = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroDocumento: string;
    FDataRegistro: TDateTime;
    FLocalDesembaraço: string;
    FUFDesembaraço: string;
    FDataDesembaraço: TDateTime;
    FCodigoExportador: string;
    FViaTransporte: string;
    FValorAFRMM: Double;
    FFormaIntermediacao: string;
    FCNPJAdquirente: string;
    FUFAdquirente: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property NumeroDocumento: string read FNumeroDocumento write FNumeroDocumento;
    property DataRegistro: TDateTime read FDataRegistro write FDataRegistro;
    property LocalDesembaraço: string read FLocalDesembaraço write FLocalDesembaraço;
    property UFDesembaraço: string read FUFDesembaraço write FUFDesembaraço;
    property DataDesembaraço: TDateTime read FDataDesembaraço write FDataDesembaraço;
    property CodigoExportador: string read FCodigoExportador write FCodigoExportador;
    property ViaTransporte: string read FViaTransporte write FViaTransporte;
    property ValorAFRMM: Double read FValorAFRMM write FValorAFRMM;
    property FormaIntermediacao: string read FFormaIntermediacao write FFormaIntermediacao;
    property CNPJAdquirente: string read FCNPJAdquirente write FCNPJAdquirente;
    property UFAdquirente: string read FUFAdquirente write FUFAdquirente;
  end;

implementation

uses
  SysUtils;

constructor TRegistroDIP.Create;
begin
  FTipoRegistro := 'DIP';
end;

function TRegistroDIP.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|%s|%s|%s|%.2f|%s|%s|', [
    FTipoRegistro,
    FNumeroDocumento,
    FormatDateTime('yyyymmdd', FDataRegistro),
    FLocalDesembaraço,
    FUFDesembaraço,
    FormatDateTime('yyyymmdd', FDataDesembaraço),
    FCodigoExportador,
    FViaTransporte,
    FValorAFRMM,
    FFormaIntermediacao,
    FCNPJAdquirente,
    FUFAdquirente
  ]);
end;

end.

