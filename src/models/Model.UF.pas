unit Model.UF;

interface

uses Model.Base;
type
  TUF = class(TBaseTR)
  private
    fsigla: string;
  public
    property sigla: string read fsigla write fsigla;
  end;
  TUFs = TArray<TUF>;
implementation
end.
