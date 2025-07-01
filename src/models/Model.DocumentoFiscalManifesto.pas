unit Model.DocumentoFiscalManifesto;

interface

uses
  System.SysUtils, System.Classes, Model.Base, Model.Estabelecimento;

type

  TDocumentoFiscalManifesto = class
  private
    festabelecimento: TEstabelecimentoC;
    fchave: String;
    fcodevento: SmallInt;
    fdocumentoNumero: Int64;
    fmotivo: String;
    femitente: String;
    fambiente: SmallInt;
  public
    property estabelecimento: TEstabelecimentoC read festabelecimento write festabelecimento;
    property chave: String read fchave write fchave;
    property codevento: SmallInt read fcodevento write fcodevento;
    property documentoNumero: Int64 read fdocumentoNumero write fdocumentoNumero;
    property motivo: String read fmotivo write fmotivo;
    property emitente: String read femitente write femitente;
    property ambiente: SmallInt read fambiente write fambiente;
  end;

implementation


end.
