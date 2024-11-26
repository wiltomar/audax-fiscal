unit Model.Fortes.RegistroBSS;

interface

uses Fortes.IRegistro;

type
  TRegistroBSS = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FIdentificacaoBeneficiario: string;
    FNomeBeneficiario: string;
    FDataNascimento: TDateTime;
    FCodigoBeneficio: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property IdentificacaoBeneficiario: string read FIdentificacaoBeneficiario write FIdentificacaoBeneficiario;
    property NomeBeneficiario: string read FNomeBeneficiario write FNomeBeneficiario;
    property DataNascimento: TDateTime read FDataNascimento write FDataNascimento;
    property CodigoBeneficio: string read FCodigoBeneficio write FCodigoBeneficio;
  end;

implementation

uses
  SysUtils;

constructor TRegistroBSS.Create;
begin
  FTipoRegistro := 'BSS';
end;

function TRegistroBSS.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FIdentificacaoBeneficiario,
    FNomeBeneficiario,
    FormatDateTime('yyyymmdd', FDataNascimento),
    FCodigoBeneficio
  ]);
end;

end.

