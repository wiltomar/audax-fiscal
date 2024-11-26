unit Model.Operacao;

interface

uses Model.Base;

type
  TOperacao = class(TBaseTR);
  TOperacoes = TArray<TOperacao>;
  
  TOperacaoIcms = class(TBaseR)
  private
    fid: string;
    ficmsAliquota: Currency;
  public
    property id: string read fid write fid;
    property icmsAliquota: Currency read ficmsAliquota write ficmsAliquota;
  end;

  TVendaOperacao = class(TBaseR)
  private
    fcodigo: string;
    fnome: string;
    foperacaoicms: TOperacaoIcms;
  public
    property codigo: string read fcodigo write fcodigo;
    property nome: string read fnome write fnome;
    property operacaoicms: TOperacaoIcms read foperacaoicms write foperacaoicms;
  end;
  TVendaOperacoes = Array of TVendaOperacao;

implementation

end.