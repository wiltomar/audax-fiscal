unit Model.ClassificacaoFiscalIBSCBS;

interface

uses
  Model.Base, Model.CSTIBSCBS;

type
  TClassificacaoFiscalIBSCBS = class(TBaseTR)
  private
    fcstibscbsid: String;
  public
    property cstibscbsid: string read fcstibscbsid write fcstibscbsid;
  end;
  TClassificacaoFiscalIBSCBSs = TArray<TClassificacaoFiscalIBSCBS>;

implementation

end.
