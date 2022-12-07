unit Model.Municipio;

interface

uses Model.Base, Model.UF;

type
  TMunicipio = class(TBaseTR)
  private
    fuf: TUF;
  public
    constructor Create(aid: string; anome: string); overload;
    destructor Destroy(); override;
    property uf: TUF read fuf write fuf;
  end;
  TMunicipios = TArray<TMunicipio>;

implementation

uses
  System.SysUtils, Lib.Funcoes;

constructor TMunicipio.Create(aid: string; anome: string);
begin
  inherited Create(aid, anome);
  fuf := nil;
end;

destructor TMunicipio.Destroy();
begin
  Obliterate(fuf);
  inherited;
end;

end.

