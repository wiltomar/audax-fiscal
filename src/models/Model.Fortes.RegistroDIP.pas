unit Model.Fortes.RegistroDIP;

interface

uses Fortes.IRegistro;

type
  TRegistroDIP = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FNumeroDocumento: string;
    FDataRegistro: TDateTime;
    FLocalDesembara�o: string;
    FUFDesembara�o: string;
    FDataDesembara�o: TDateTime;
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
    property LocalDesembara�o: string read FLocalDesembara�o write FLocalDesembara�o;
    property UFDesembara�o: string read FUFDesembara�o write FUFDesembara�o;
    property DataDesembara�o: TDateTime read FDataDesembara�o write FDataDesembara�o;
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
    FLocalDesembara�o,
    FUFDesembara�o,
    FormatDateTime('yyyymmdd', FDataDesembara�o),
    FCodigoExportador,
    FViaTransporte,
    FValorAFRMM,
    FFormaIntermediacao,
    FCNPJAdquirente,
    FUFAdquirente
  ]);
end;

end.

