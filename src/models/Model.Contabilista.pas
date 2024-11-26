unit Model.Contabilista;

interface

uses Model.Base, Model.Municipio;

type
  TContabilista = class(TBaseR)
  private
    ffone: string;
    fcnpj: string;
    femail: string;
    fbairro: string;
    ffax: string;
    fnum: string;
    fcrc: string;
    fcpf: string;
    fcep: string;
    fcompl: string;
    fmunicipio: TMunicipio;
    fnome: string;
    fendereco: string;
  public
    property nome: string read fnome write fnome;
    property cpf: string read fcpf write fcpf;
    property crc: string read fcrc write fcrc;
    property cnpj: string read fcnpj write fcnpj;
    property cep: string read fcep write fcep;
    property endereco: string read fendereco write fendereco;
    property num: string read fnum write fnum;
    property compl: string read fcompl write fcompl;
    property bairro: string read fbairro write fbairro;
    property fone: string read ffone write ffone;
    property fax: string read ffax write ffax;
    property email: string read femail write femail;
    property municipio: TMunicipio read fmunicipio write fmunicipio;
  end;

implementation

end.
