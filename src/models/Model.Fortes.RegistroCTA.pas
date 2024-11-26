unit Model.Fortes.RegistroCTA;

interface

uses Fortes.IRegistro;

type
  TRegistroCTA = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigoConta: string;
    FDescricao: string;
    FNatureza: string; // "D" ou "C" para Débito ou Crédito
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property CodigoConta: string read FCodigoConta write FCodigoConta;
    property Descricao: string read FDescricao write FDescricao;
    property Natureza: string read FNatureza write FNatureza;
  end;

implementation

uses
  SysUtils;

constructor TRegistroCTA.Create;
begin
  FTipoRegistro := 'CTA';
end;

function TRegistroCTA.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|', [FTipoRegistro, FCodigoConta, FDescricao, FNatureza]);
end;

end.

