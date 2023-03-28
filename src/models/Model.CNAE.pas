unit Model.CNAE;

interface

uses
  Model.Base;

type
  TCnae = class(TBaseR)
  private
    fcodigo: string;
    fnome: string;
  public
    property codigo: string read fcodigo write fcodigo;
    property nome: string read fnome write fnome;
  end;

implementation

end.
