unit Model.InventarioFiscal;

interface

uses
  Model.Base, Model.Parceiro;

type
  TItem = class(TBaseR)
  private
    fcodigo: String;
    fnome: String;
  public
    property codigo: String read fcodigo write fcodigo;
    property nome: String read fnome write fnome;
  end;

type
  TcstICMS = class(TBaseR)
  private
    fcodigo: String;
    fnome: String;
  public
    property codigo: String read fcodigo write fcodigo;
    property nome: String read fnome write fnome;
  end;

type
  TInventarioFiscalItem = class(TBaseR)
  private
    FId: string;
    FInclusao: TDateTime;
    FEdicao: TDateTime;
    FExclusao: TDateTime;
    FVersao: Integer;
    FCorporacaoId: string;
    FEmpresaId: string;
    FEstabelecimentoId: string;
    FInventarioFiscalId: string;
    FItemId: string;
    FQuantidade: Double;
    FValor: Double;
    FMedida: string;
    FTotal: Double;
    FIndicadorDePropriedade: SmallInt;
    FParceiroId: string;
    FDescricaoComplementar: string;
    FContaId: string;
    FValorIR: Double;
    FCstIcmsId: string;
    FValorBcIcms: Double;
    FValorIcms: Double;
    FValorIcmsOperacao: Double;
    FValorBcIcmsSt: Double;
    FValorIcmsSt: Double;
    FValorFcp: Double;
    fitem: TItem;
    fcstIcms: TcstIcms;
  public
    property Id: string read FId write FId;
    property Inclusao: TDateTime read FInclusao write FInclusao;
    property Edicao: TDateTime read FEdicao write FEdicao;
    property Exclusao: TDateTime read FExclusao write FExclusao;
    property Versao: Integer read FVersao write FVersao;
    property CorporacaoId: string read FCorporacaoId write FCorporacaoId;
    property EmpresaId: string read FEmpresaId write FEmpresaId;
    property EstabelecimentoId: string read FEstabelecimentoId write FEstabelecimentoId;
    property InventarioFiscalId: string read FInventarioFiscalId write FInventarioFiscalId;
    property ItemId: string read FItemId write FItemId;
    property Quantidade: Double read FQuantidade write FQuantidade;
    property Valor: Double read FValor write FValor;
    property Medida: string read FMedida write FMedida;
    property Total: Double read FTotal write FTotal;
    property IndicadorDePropriedade: SmallInt read FIndicadorDePropriedade write FIndicadorDePropriedade;
    property ParceiroId: string read FParceiroId write FParceiroId;
    property DescricaoComplementar: string read FDescricaoComplementar write FDescricaoComplementar;
    property ContaId: string read FContaId write FContaId;
    property ValorIR: Double read FValorIR write FValorIR;
    property CstIcmsId: string read FCstIcmsId write FCstIcmsId;
    property ValorBcIcms: Double read FValorBcIcms write FValorBcIcms;
    property ValorIcms: Double read FValorIcms write FValorIcms;
    property ValorIcmsOperacao: Double read FValorIcmsOperacao write FValorIcmsOperacao;
    property ValorBcIcmsSt: Double read FValorBcIcmsSt write FValorBcIcmsSt;
    property ValorIcmsSt: Double read FValorIcmsSt write FValorIcmsSt;
    property ValorFcp: Double read FValorFcp write FValorFcp;
    property item: TItem read fitem write fitem;
    property cstIcms : TcstIcms read fcstIcms write fcstIcms;
  end;
  TInventarioFiscalItens = TArray<TInventarioFiscalItem>;

type
  TInventarioFiscal = class(TBaseR)
  private
    FId: string;
    FInclusao: TDateTime;
    FEdicao: TDateTime;
    FExclusao: TDateTime;
    FVersao: Integer;
    FCorporacaoId: string;
    FEmpresaId: string;
    FEstabelecimentoId: string;
    FCodigo: string;
    FSituacao: SmallInt;
    FNome: string;
    FExercicio: SmallInt;
    FReferencia: TDateTime;
    FMotivo: SmallInt;
    FInventarioFiscalItens: TInventarioFiscalItens;
  public
    property Id: string read FId write FId;
    property Inclusao: TDateTime read FInclusao write FInclusao;
    property Edicao: TDateTime read FEdicao write FEdicao;
    property Exclusao: TDateTime read FExclusao write FExclusao;
    property Versao: Integer read FVersao write FVersao;
    property CorporacaoId: string read FCorporacaoId write FCorporacaoId;
    property EmpresaId: string read FEmpresaId write FEmpresaId;
    property EstabelecimentoId: string read FEstabelecimentoId write FEstabelecimentoId;
    property Codigo: string read FCodigo write FCodigo;
    property Situacao: SmallInt read FSituacao write FSituacao;
    property Nome: string read FNome write FNome;
    property Exercicio: SmallInt read FExercicio write FExercicio;
    property Referencia: TDateTime read FReferencia write FReferencia;
    property Motivo: SmallInt read FMotivo write FMotivo;
    property InventarioFiscalItens: TInventarioFiscalItens read FinventarioFiscalItens write FinventarioFiscalItens;
  end;

implementation


end.
