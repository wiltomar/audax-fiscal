unit Model.Fortes.RegistroCLI;

interface

uses Fortes.IRegistro;

type
  TRegistroCLI = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigoCliente: string;
    FNome: string;
    FCNPJ_CPF: string;
    FEndereco: string;
    FUF: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property CodigoCliente: string read FCodigoCliente write FCodigoCliente;
    property Nome: string read FNome write FNome;
    property CNPJ_CPF: string read FCNPJ_CPF write FCNPJ_CPF;
    property Endereco: string read FEndereco write FEndereco;
    property UF: string read FUF write FUF;
  end;

implementation

uses
  SysUtils;

constructor TRegistroCLI.Create;
begin
  FTipoRegistro := 'CLI';
end;

function TRegistroCLI.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FCodigoCliente,
    FNome,
    FCNPJ_CPF,
    FEndereco,
    FUF
  ]);
end;

end.

