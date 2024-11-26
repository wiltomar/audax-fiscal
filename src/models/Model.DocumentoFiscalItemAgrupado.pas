unit Model.DocumentoFiscalItemAgrupado;

interface

uses
  Model.Base, Model.Parceiro;

type
  TDocumentoFiscalItemAgrupado = class(TBaseR)
  private
    fvl_icms_st: Currency;
    faliq_icms: Currency;
    fvl_bc_icms_st: Currency;
    fvl_opr: Currency;
    fvl_red_bc: Currency;
    fvl_desconto: Currency;
    fcod_obs: String;
    fcfop: String;
    fvl_ipi: Currency;
    fcst_icms: String;
    fvl_icms: Currency;
    fvl_bc_icms: Currency;
  public
    property cst_icms : String read fcst_icms write fcst_icms;
    property cfop : String read fcfop write fcfop;
    property vl_opr: Currency read fvl_opr write fvl_opr;
    property vl_bc_icms: Currency read fvl_bc_icms write fvl_bc_icms;
    property vl_icms: Currency read fvl_icms write fvl_icms;
    property vl_bc_icms_st: Currency read fvl_bc_icms_st write fvl_bc_icms_st;
    property vl_icms_st: Currency read fvl_icms_st write fvl_icms_st;
    property vl_red_bc: Currency read fvl_red_bc write fvl_red_bc;
    property vl_ipi: Currency read fvl_ipi write fvl_ipi;
    property aliq_icms: Currency read faliq_icms write faliq_icms;
    property vl_desconto: Currency read fvl_desconto write fvl_desconto;
    property cod_obs: String read fcod_obs write fcod_obs;
  end;

implementation

end.
