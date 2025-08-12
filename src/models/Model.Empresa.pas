unit Model.Empresa;

interface

uses
  Model.Base;

type
  TEmpresa = class(TBaseTR)
  private
    fimagem: string;
  public
    property imagem: string read fimagem write fimagem;
  end;

implementation

end.
