unit Model.Inutilizacao;

interface

uses Model.Base;

type
  TInutilizacao = class(TBase)
  private
    fambiente: String;
    fstatus: smallint;
    fmotivo: string;
    fuf: Smallint;
    fano: smallint;
    fcnpj: string;
    fmodelo: smallint;
    fserie: smallint;
    fnumeroinicial: smallint;
    fnumerofinal: smallint;
    frecebimento: TDateTime;
    fprotocolo: string;
    fxml: string;
  public
    property ambiente: String read fambiente write fambiente;
    property status: smallint read fstatus write fstatus;
    property motivo: string read fmotivo write fmotivo;
    property uf: Smallint read fuf write fuf;
    property ano: smallint read fano write fano;
    property cnpj: string read fcnpj write fcnpj;
    property modelo: smallint read fmodelo write fmodelo;
    property serie: smallint read fserie write fserie;
    property numeroinicial: smallint read fnumeroinicial write fnumeroinicial;
    property numerofinal: smallint read fnumerofinal write fnumerofinal;
    property recebimento: TDateTime read frecebimento write frecebimento;
    property protocolo: string read fprotocolo write fprotocolo;
    property xml: string read fxml write fxml;
  end;

implementation

end.
