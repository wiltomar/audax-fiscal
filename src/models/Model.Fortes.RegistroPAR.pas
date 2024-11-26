unit Model.Fortes.RegistroPAR;

interface

uses Fortes.IRegistro;

type
  TRegistroPAR = class(TInterfacedObject, IRegistro)
  private
    FTipoRegistro: string;
    FCodigo: string;
    FNome: string;
    FUF: string;
    FCNPJCPF: string;
    FInscricaoEstadual: string;
    FInscricaoMunicipal: string;
    FEndereco: string;
    FCEP: string;
    FMunicipio: string;
  public
    constructor Create;
    function GerarLinha: string;

    property TipoRegistro: string read FTipoRegistro write FTipoRegistro;
    property Codigo: string read FCodigo write FCodigo;
    property Nome: string read FNome write FNome;
    property UF: string read FUF write FUF;
    property CNPJCPF: string read FCNPJCPF write FCNPJCPF;
    property InscricaoEstadual: string read FInscricaoEstadual write FInscricaoEstadual;
    property InscricaoMunicipal: string read FInscricaoMunicipal write FInscricaoMunicipal;
    property Endereco: string read FEndereco write FEndereco;
    property CEP: string read FCEP write FCEP;
    property Municipio: string read FMunicipio write FMunicipio;
  end;

implementation

uses
  SysUtils;

constructor TRegistroPAR.Create;
begin
  FTipoRegistro := 'PAR';
end;

function TRegistroPAR.GerarLinha: string;
begin
  Result := Format('%s|%s|%s|%s|%s|%s|%s|%s|%s|%s|', [
    FTipoRegistro,
    FCodigo,
    FNome,
    FUF,
    FCNPJCPF,
    FInscricaoEstadual,
    FInscricaoMunicipal,
    FEndereco,
    FCEP,
    FMunicipio
  ]);
end;

end.

