unit Model.Fortes.RegistroFOR;

interface

uses Fortes.IRegistro;

type
  TRegistroFOR = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigoFornecedor: string;
    FNome: string;
    FCNPJ_CPF: string;
    FEndereco: string;
    FUF: string;
  public
    constructor Create;
    function GerarLinha: string;
    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property CodigoFornecedor: string read FCodigoFornecedor write FCodigoFornecedor;
    property Nome: string read FNome write FNome;
    property CNPJ_CPF: string read FCNPJ_CPF write FCNPJ_CPF;
    property Endereco: string read FEndereco write FEndereco;
    property UF: string read FUF write FUF;
  end;

implementation

uses
  SysUtils;

constructor TRegistroFOR.Create;
begin
  FTipoRegistro := 'FOR';
end;

function TRegistroFOR.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FCodigoFornecedor,
    FNome,
    FCNPJ_CPF,
    FEndereco,
    FUF
  ]);
end;

end.

