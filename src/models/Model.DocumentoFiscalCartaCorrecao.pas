unit Model.DocumentoFiscalCartaCorrecao;

interface

uses
  System.SysUtils, System.Classes, Model.Base, Model.Estabelecimento;

type
  TDocumentoFiscalCartaCorrecao = class
  private
    festabelecimento: TEstabelecimentoC;
    fchave: String;
    fnumero: int64;
    fsequencia: Integer;
    fcorrecao: String;
    fprotocolo: String;
    frecebimento: TDateTime;
    fstatus: SmallInt;
  public
    property estabelecimento: TEstabelecimentoC read festabelecimento write festabelecimento;
    property chave: String read fchave write fchave;
    property numero: int64 read fnumero write fnumero;
    property correcao: String read fcorrecao write fcorrecao;
    property sequencia: Integer read fsequencia write fsequencia;
    property protocolo: String read fprotocolo write fprotocolo;
    property recebimento: TDateTime read frecebimento write frecebimento;
    property status: SmallInt read fstatus write fstatus;
  end;

implementation


end.
