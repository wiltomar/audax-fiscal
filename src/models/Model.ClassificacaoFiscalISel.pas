unit Model.ClassificacaoFiscalISel;

interface

uses
  Model.Base, Model.CSTISEL;

type
  TClassificacaoFiscalISel = class(TBaseTR)
  private
    fcstiselid: String;
  public
    property cstiselid: string read fcstiselid write fcstiselid;
  end;
  TClassificacaoFiscalISels = TArray<TClassificacaoFiscalISel>;

implementation

end.
