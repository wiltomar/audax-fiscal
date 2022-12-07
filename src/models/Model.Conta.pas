unit Model.Conta;

interface

uses Model.Base;

const
  CONTA_FILTRO_RECEITA = '$receita';
  CONTA_FILTRO_DESPESA = '$despesa';
  CONTA_FILTRO_CIRCULANTE = '$circulante';

type
  TConta = class(TBaseTR)
  private
    fanalitica: Boolean;
  public
    property analitica: Boolean read fanalitica write fanalitica;
  end;
  TContas = TArray<TConta>;

implementation

end.
