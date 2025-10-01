unit Model.Sped;

interface

uses
  Classes, DateUtils, SysUtils, ACBrTXTClass, Variants, ACBrEFDBlocos, StrUtils,
  ACBrSpedFiscal, ACBrBase, ACBrEFDBloco_K_Class, ACBrEFDBloco_K, Midas,
  ACBrEFDBloco_0, ACBrEFDBloco_0_Class, Model.Estabelecimento, APIService,
  DBClient, DB, Model.Operacao, Api.Funcoes, model.Base, Model.InventarioFiscal,
  System.Generics.Collections;

type
  TSped = class
  private
    dataReferencia: TDate;
    function anoVersao: TACBrVersaoLeiauteSPEDFiscal;
  public
    SpedFiscal: TACBrSPEDFiscal;
    constructor Create;
    function gerar(Estabelecimento: TEstabelecimentoC;
      dataInicial, dataFinal: TDate; out nomeArquivo: String;
      out cErros: String; finalidade: SmallInt = 0; indMovimento: SmallInt = 0; inventario: string = ''): boolean;
  end;

implementation

{ TSped }

uses Model.Unidade, Model.Item, Model.Contabilista, Model.Parceiro,
  Model.DocumentoFiscalSPED, Model.DocumentoFiscalItemAgrupado, Model.CFOP;

function TSped.anoVersao: TACBrVersaoLeiauteSPEDFiscal;
var
  xVer: string;
begin
  FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
  if (dataReferencia >= StrToDate('01/01/2009')) and
    (dataReferencia <= StrToDate('31/12/2009')) then
    xVer := '002'
  else if (dataReferencia >= StrToDate('01/01/2010')) and
    (dataReferencia <= StrToDate('31/12/2010')) then
    xVer := '003'
  else if (dataReferencia >= StrToDate('01/01/2011')) and
    (dataReferencia <= StrToDate('31/12/2011')) then
    xVer := '004'
  else if (dataReferencia >= StrToDate('01/01/2012')) and
    (dataReferencia <= StrToDate('30/06/2012')) then
    xVer := '005'
  else if (dataReferencia >= StrToDate('01/07/2012')) and
    (dataReferencia <= StrToDate('31/12/2012')) then
    xVer := '006'
  else if (dataReferencia >= StrToDate('01/01/2013')) and
    (dataReferencia <= StrToDate('31/12/2013')) then
    xVer := '007'
  else if (dataReferencia >= StrToDate('01/01/2014')) and
    (dataReferencia <= StrToDate('31/12/2014')) then
    xVer := '008'
  else if (dataReferencia >= StrToDate('01/01/2015')) and
    (dataReferencia <= StrToDate('31/12/2015')) then
    xVer := '009'
  else if (dataReferencia >= StrToDate('01/01/2016')) and
    (dataReferencia <= StrToDate('31/12/2016')) then
    xVer := '010'
  else if (dataReferencia >= StrToDate('01/01/2017')) and
    (dataReferencia <= StrToDate('31/12/2017')) then
    xVer := '011'
  else if (dataReferencia >= StrToDate('01/01/2018')) and
    (dataReferencia <= StrToDate('31/12/2018')) then
    xVer := '012'
  else if (dataReferencia >= StrToDate('01/01/2019')) and
    (dataReferencia <= StrToDate('31/12/2019')) then
    xVer := '013'
  else if (dataReferencia >= StrToDate('01/01/2020')) and
    (dataReferencia <= StrToDate('31/12/2020')) then
    xVer := '014'
  else if (dataReferencia >= StrToDate('01/01/2021')) and
    (dataReferencia <= StrToDate('31/12/2021')) then
    xVer := '015'
  else if (dataReferencia >= StrToDate('01/01/2022')) and
    (dataReferencia <= StrToDate('31/12/2022')) then
    xVer := '016'
  else if (dataReferencia >= StrToDate('01/01/2023')) and
    (dataReferencia <= StrToDate('31/12/2023')) then
    xVer := '017'
  else if (dataReferencia >= StrToDate('01/01/2024')) and
    (dataReferencia <= StrToDate('31/12/2024')) then
    xVer := '018'
  else if (dataReferencia >= StrToDate('01/01/2025')) and
    (dataReferencia <= StrToDate('31/12/2025')) then
    xVer := '019'
  else
    xVer := '019';
  Result := StrToCodVer(xVer);
end;

constructor TSped.Create;
begin
  inherited Create;
end;

function TSped.gerar(Estabelecimento: TEstabelecimentoC;
  dataInicial, dataFinal: TDate; out nomeArquivo: String; out cErros: String; finalidade: SmallInt; indMovimento: SmallInt; inventario: string): boolean;
var
  I: Integer;
  dataTexto: String;
  ItensDoCupom: Boolean;
  dataIniSped, dataFinSped: String;
  contadorLocal: Integer;
  subtotalItem: Currency;
  dataDoc: TdateTime;
  totalValorIcms: Currency;
  valorTtotalGeralIcms: Currency;
  valorTotDebitosBlocoE: Currency;
  valorOutrosCredICMSST: Currency;
  DocumentosFiscaisC190Aux : TObjectList<TDocumentoFiscalItemAgrupado>;
begin
  valorTtotalGeralIcms  := 0;
  valorOutrosCredICMSST := 0;
  SpedFiscal := TACBrSPEDFiscal.Create(nil);
  try
    SpedFiscal.DT_INI := dataInicial;
    SpedFiscal.DT_FIN := dataFinal;
    dataReferencia := dataInicial;
    dataTexto := ReplaceStr(DateToStr(dataInicial), '/', '') + '-' +
      ReplaceStr(DateToStr(dataFinal), '/', '');
    dataIniSped := FormatDateTime('yyyy-mm-dd', dataInicial);
    dataFinSped := FormatDateTime('yyyy-mm-dd', dataFinal);
    SpedFiscal.Path := ExtractFilePath(ParamStr(0)) + '/arquivos/texto/sped/';
    SpedFiscal.Arquivo :=
      'sped' + Estabelecimento.estabelecimentoDocumentos[0].
        documentoNumero + '_' + dataTexto + '.txt';
    nomeArquivo := SpedFiscal.Path + SpedFiscal.Arquivo;

    try
      SpedFiscal.IniciaGeracao;

      var
        tbOperacao: TClientDataSet;
      tbOperacao := TClientDataSet.Create(nil);
      tbOperacao.FieldDefs.Add('id', ftString, 50);
      tbOperacao.FieldDefs.Add('aliqicms', ftCurrency);
      tbOperacao.StoreDefs := False;
      tbOperacao.CreateDataSet;
      tbOperacao.Open;

      var
      operacoes := InfoAPI().GetPagedArray<TVendaOperacao>
        ('movimento/operacao/sped?estabelecimentoid=' + Estabelecimento.id);
      for var Operacao in operacoes do
      begin
        tbOperacao.Append;
        tbOperacao.fieldByname('id').Asstring := Operacao.id;
        tbOperacao.fieldByname('aliqicms').AsCurrency :=
          Operacao.operacaoicms.icmsAliquota;
        tbOperacao.Post;
      end;
      tbOperacao.First;

      with SpedFiscal.Bloco_0 do
      begin
        with Registro0000New do
        begin
          COD_VER := anoVersao;
          COD_FIN := TACBrCodFin(finalidade);
          NOME := Estabelecimento.NOME;
          CNPJ := Estabelecimento.estabelecimentoDocumentos[0].documentoNumero;
          UF := Estabelecimento.estabelecimentoenderecos[0].UF.sigla;
          IE := Estabelecimento.estabelecimentoDocumentos[0].inscricaoEstadual;
          COD_MUN := StrToIntDef(Estabelecimento.estabelecimentoenderecos[0]
            .municipio.codigo, 2304400);
          IM := Estabelecimento.estabelecimentoDocumentos[0].inscricaoMunicipal;
          SUFRAMA := Estabelecimento.estabelecimentoDocumentos[0]
            .inscricaoSuframa;
          IND_PERFIL := StrToIndPerfil(Estabelecimento.estabelecimentoDocumentos
            [0].spedperfil);
          IND_ATIV := TACBrIndAtiv(1);
        end;

        with Registro0001New do
        begin
          if not(IndMovimento = 0) then
            IND_MOV := imSemDados
          else
          begin
            IND_MOV := imComDados;

            with Registro0005New do
            begin
              FANTASIA := Estabelecimento.NOME;
              CEP := SomenteDigitos
                (AnsiString(Estabelecimento.estabelecimentoenderecos[0].CEP));
              ENDERECO := Estabelecimento.estabelecimentoenderecos[0].logradouro;
              NUM := IntToStr(Estabelecimento.estabelecimentoenderecos[0].numero);
              COMPL := Estabelecimento.estabelecimentoenderecos[0].complemento;
              BAIRRO := Estabelecimento.estabelecimentoenderecos[0].BAIRRO;
              FONE := '';
              FAX := '';
              EMAIL := '';
            end;

            if Estabelecimento.estabelecimentoDocumentos[0]
              .inscricaoEstadualSubstitutoTributario > '' then
            begin
              with Registro0015New do
              begin
                UF_ST := Estabelecimento.estabelecimentoenderecos[0].UF.sigla;
                IE_ST := Estabelecimento.estabelecimentoDocumentos[0]
                  .inscricaoEstadualSubstitutoTributario;
              end;
            end;

            if Estabelecimento.estabelecimentoDocumentos[0].spedperfil <> 'C' then
            begin
              var
              contabilistas := InfoAPI().GetPagedArray<TParceiroC>
                ('agente/parceiro/sped?estabelecimentoid=' + Estabelecimento.id +
                '&contador=1');
              for var Contabilista in contabilistas do
              begin
                with Registro0100New do
                begin
                  NOME := Contabilista.NOME;
                  EMAIL := 'contador@contador.com';
                  for var documento in Contabilista.parceirodocumentos do
                  begin
                    case documento.documentotipo of
                      1:
                        CNPJ := String(documento.documentoNumero);
                      2:
                        CPF := String(documento.documentoNumero);
                      5:
                        CRC := String(documento.documentoNumero);
                    end;
                  end;

                  for var enderecoC in Contabilista.parceiroenderecos do
                  begin
                    CEP := SomenteDigitos(enderecoC.CEP);
                    ENDERECO := enderecoC.logradouro;
                    NUM := IntToStr(enderecoC.numero);
                    COMPL := enderecoC.complemento;
                    BAIRRO := enderecoC.BAIRRO;
                    FONE := '';
                    FAX := '';
                    if enderecoC.municipio.codigo > '' then
                      COD_MUN := StrToIntDef(enderecoC.municipio.codigo, 2304400);
                  end;
                end;
              end;
            end;

            var
            Parceiros := InfoAPI().GetPagedArray<TParceiroC>
              ('agente/parceiro/sped?estabelecimentoid=' + Estabelecimento.id +
              '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' +
              dataFinSped);
            for var Parceiro in Parceiros do
            begin
              with Registro0150New do
              begin
                COD_PART := Parceiro.codigo;
                NOME := Parceiro.NOME;
                COD_PAIS := '1058';
                if Assigned(Parceiro.parceirodocumentos) then
                begin
                  for var documento in Parceiro.parceirodocumentos do
                  begin
                    case documento.documentotipo of
                      1:
                        CNPJ := String(documento.documentoNumero);
                      2:
                        CPF := String(documento.documentoNumero);
                    end;
                    if documento.inscricaoEstadual > '' then
                      IE := SomenteDigitos(documento.inscricaoEstadual)
                    else
                      IE := '';
                  end;

                  if Assigned(Parceiro.parceiroenderecos) then
                  begin
                    ENDERECO := Parceiro.parceiroenderecos[0].logradouro;
                    NUM := IntToStr(Parceiro.parceiroenderecos[0].numero);
                    COMPL := Parceiro.parceiroenderecos[0].complemento;
                    BAIRRO := Parceiro.parceiroenderecos[0].BAIRRO;
                    COD_MUN :=
                      StrToInt(Parceiro.parceiroenderecos[0].municipio.codigo);
                  end;
                end;
              end;
            end;

            var
            Unidades := InfoAPI().GetPagedArray<TUnidade>
              ('recurso/unidade/sped?estabelecimentoid=' + Estabelecimento.id +
              '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' +
              dataFinSped + '&natureza=0');
            for var Unidade in Unidades do
            begin
              if not Registro0190.LocalizaRegistro(IntToStr(I)) then
              begin
                with Registro0190New do
                begin
                  UNID := Unidade.codigo;
                  DESCR := Unidade.NOME;
                end;
              end;
            end;

            var Produtos: TArray<TItemC>;

            Produtos := InfoAPI().GetPagedArray<TItemC>
              ('recurso/item/sped?estabelecimentoid=' + Estabelecimento.id +
              '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' +
              dataFinSped + '&natureza=0');

            for var produto in Produtos do
            begin
              with Registro0200New do
              begin
                COD_ITEM := produto.codigo;
                DESCR_ITEM := produto.NOME;
                COD_BARRA := '';
                UNID_INV := produto.Unidade.codigo;
                COD_NCM := SomenteDigitos(AnsiString(produto.ncm.codigo));
                COD_GEN := '';
                tbOperacao.First;
                if Assigned(produto.vendaoperacao) then
                begin
                  if tbOperacao.Locate('id', produto.vendaoperacao.id, []) then
                    ALIQ_ICMS := tbOperacao.fieldByname('aliqicms').AsCurrency
                  else
                    ALIQ_ICMS := 0.0;
                end
                else
                  ALIQ_ICMS := 0.0;
              end;
            end;
          end;
        end;

        SpedFiscal.WriteBloco_0;

        valorTotDebitosBlocoE := 0.0;
        with SpedFiscal.Bloco_C do
        begin
          with RegistroC001New do
            if not(indMovimento = 0) then
              IND_MOV := imSemDados
            else
            begin
              IND_MOV := imComDados;

              var tbDocumentoAgrupado: TClientDataSet;

              tbDocumentoAgrupado := TClientDataSet.Create(nil);
              tbDocumentoAgrupado.FieldDefs.Add('cfop', ftString, 10);
              tbDocumentoAgrupado.FieldDefs.Add('csticms', ftString, 10);
              tbDocumentoAgrupado.FieldDefs.Add('aliqicms', ftFloat);
              tbDocumentoAgrupado.CreateDataSet;
              tbDocumentoAgrupado.Open;

              var
              DocumentosFiscais := InfoAPI().GetPagedArray<TDocumentoFiscalSPED>
                ('fiscal/documentofiscal/sped?estabelecimentoid=' + Estabelecimento.id
                + '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' +
                dataFinSped);

              var itensDocumentoFiscal: String;
              var inicio: boolean := true;
              var qtdeIntens : Integer;
              DocumentosFiscaisC190Aux := TObjectList<TDocumentoFiscalItemAgrupado>.Create(True);

              for var documentoFiscal in DocumentosFiscais do
              begin
                qtdeIntens := qtdeIntens+1;
                if inicio then
                begin
                  itensDocumentoFiscal := documentoFiscal.id;
                  inicio := false;
                end
                else
                  itensDocumentoFiscal := itensDocumentoFiscal + ',' + documentoFiscal.id;

                if (qtdeIntens = 150) then
                begin
                  inicio := true;
                  qtdeIntens := 0;
                  var DocumentosFiscaisC1902 := InfoAPI().GetArray<TDocumentoFiscalItemAgrupado>('fiscal/documentofiscal/sped?estabelecimentoid=' +
                      Estabelecimento.id + '&documentofiscalid in (' + itensDocumentoFiscal + ')'+ '&agrupado=1');

                  for var itemAgrupado in DocumentosFiscaisC1902 do
                  begin
                    DocumentosFiscaisC190Aux.Add(itemAgrupado);
                  end;
                  itensDocumentoFiscal := '';
                end;
              end;

              if (qtdeIntens < 150) then
              begin
                var DocumentosFiscaisC1902 := InfoAPI().GetArray<TDocumentoFiscalItemAgrupado>('fiscal/documentofiscal/sped?estabelecimentoid=' +
                    Estabelecimento.id + '&documentofiscalid in (' + itensDocumentoFiscal + ')'+ '&agrupado=1');

                for var itemAgrupado in DocumentosFiscaisC1902 do
                begin
                  DocumentosFiscaisC190Aux.Add(itemAgrupado);
                end;
              end;

              var
              valorTotalICMS := 0.0;
              for var documentoFiscal in DocumentosFiscais do
              begin
                tbDocumentoAgrupado.EmptyDataSet;
                with RegistroC100New do
                begin
                  if documentoFiscal.natureza = 1 then
                  begin
                    IND_OPER := tpSaidaPrestacao;
                    IND_EMIT := edEmissaoPropria;
                  end
                  else
                  begin
                    IND_OPER := tpEntradaAquisicao;
                    IND_EMIT := edTerceiros;
                  end;
                  if Assigned(documentoFiscal.Parceiro) and (documentoFiscal.modelo <> '65') then
                    COD_PART := documentoFiscal.Parceiro.codigo
                  else
                    COD_PART := '';
                  COD_MOD := documentoFiscal.modelo;

                  case documentoFiscal.situacao of
                    20: COD_SIT := sdRegular;
                    90: COD_SIT := sdCancelado;
                  end;

                  if Assigned(documentoFiscal.documentofiscalnfe) then
                  begin
                    NUM_DOC := IntToStr(documentoFiscal.documentofiscalnfe.numero);
                    CHV_NFE := documentoFiscal.documentofiscalnfe.chave;
                    if (documentoFiscal.modelo = '55') or
                      (documentoFiscal.modelo = '65') then
                      SER := IntToStr(documentoFiscal.documentofiscalnfe.serie);
                  end
                  else if Assigned(documentoFiscal.documentofiscalcfe) then
                  begin
                    NUM_DOC := IntToStr(documentoFiscal.documentofiscalcfe.numero);
                    CHV_NFE := documentoFiscal.documentofiscalcfe.chave;
                    if (documentoFiscal.modelo = '55') or
                      (documentoFiscal.modelo = '65') then
                      SER := IntToStr(documentoFiscal.documentofiscalcfe.serie);
                  end;

                  dataDoc := documentoFiscal.emissao;
                  DT_DOC := documentoFiscal.emissao;
                  DT_E_S := documentoFiscal.emissao;
                  VL_DOC := documentoFiscal.total + documentoFiscal.ipiValor - documentoFiscal.desconto;
                  IND_PGTO := TACBrIndPgto(documentoFiscal.indicadorpagamento);
                  VL_DESC := documentoFiscal.desconto;
                  VL_ABAT_NT := documentoFiscal.desconto;

                  subtotalItem := 0.0;
                  var
                  totalBCicms := 0.0;
                  var
                  totalBCipi := 0.0;
                  var
                  totalValorIpi := 0.0;
                  totalValorIcms := 0.0;
                  for var itemdoc in documentoFiscal.documentoFiscalItens do
                  begin
                    subtotalItem := subtotalItem + itemdoc.subtotal;
                    totalBCicms := totalBCicms + itemdoc.icmsBC;
                    totalValorIcms := totalValorIcms + itemdoc.icmsValor;
                    totalBCipi := totalBCipi + itemdoc.ipiBC;
                    totalValorIpi := totalValorIpi + itemdoc.ipiValor;
                    valorTtotalGeralIcms := valorTtotalGeralIcms + itemdoc.icmsValor;
                    valorTotalICMS := valorTotalICMS + documentoFiscal.icmsValor;
                  end;
                  VL_MERC := subtotalItem;
                  IND_FRT := TACBrIndFrt(documentoFiscal.indicadorfrete);
                  VL_FRT := documentoFiscal.frete;
                  VL_SEG := documentoFiscal.valorseguro;
                  VL_OUT_DA := documentoFiscal.outrasdespesas;

                  VL_ICMS := totalValorIcms;
                  VL_IPI := totalValorIpi;
                  VL_PIS := documentoFiscal.valorpis;
                  VL_COFINS := documentoFiscal.valorcofins;
                  VL_PIS_ST := documentoFiscal.valorpisst;
                  VL_COFINS_ST := documentoFiscal.valorcofinsst;
                  VL_BC_ICMS_ST := documentoFiscal.icmsSTBC;
                  VL_ICMS_ST := documentoFiscal.icmsSTValor;
                  VL_BC_ICMS := totalBCicms;
                end;

                if (Estabelecimento.estabelecimentoDocumentos[0].spedperfil <> 'C') and (documentoFiscal.natureza = 0) then
                begin
                  contadorLocal := 1;
                  for var itemdoc in documentoFiscal.documentoFiscalItens do
                  begin
                    with RegistroC170New do
                    begin
                      NUM_ITEM := FormatFloat('000', contadorLocal);
                      COD_ITEM := itemdoc.Item.codigo;
                      DESCR_COMPL := itemdoc.Item.NOME;
                      QTD := itemdoc.quantidade;
                      UNID := itemdoc.Item.Unidade.codigo;
                      VL_ITEM := itemdoc.total;
                      VL_DESC := itemdoc.desconto;
                      CST_ICMS := itemdoc.csticms.codigo;
                      CFOP := itemdoc.CFOP.codigo;
                      COD_NAT := '';
                      VL_BC_ICMS := itemdoc.icmsBC;
                      ALIQ_ICMS := itemdoc.icmsAliquota;
                      VL_ICMS := itemdoc.icmsValor;

                      VL_BC_ICMS_ST := 0;
                      ALIQ_ST := 0;
                      VL_ICMS_ST := 0;

                      if itemdoc.icmsSTBC > 0 then
                      begin
                        //VL_ITEM := VL_ITEM + itemdoc.icmsSTValor;
                        VL_BC_ICMS_ST := itemdoc.icmsSTBC;
                        ALIQ_ST := itemdoc.icmsSTAliquota;
                        VL_ICMS_ST := itemdoc.icmsSTValor;
                      end;

                      IND_APUR := iamensal;
                      CST_IPI := itemdoc.cstipi.codigo;
                      COD_ENQ := '';

                      VL_BC_IPI := 0;
                      ALIQ_IPI := 0;
                      VL_IPI := 0;

                      if (itemdoc.ipiValor > 0) then
                      begin
                        //VL_ITEM := VL_ITEM + itemdoc.ipiValor;
                        VL_BC_IPI := itemdoc.ipiBC;
                        ALIQ_IPI := itemdoc.ipiAliquota;
                        VL_IPI := itemdoc.ipiValor;
                      end;


                      if itemdoc.cstpis.codigo > '' then
                      begin
                        CST_PIS := itemdoc.cstpis.codigo;
                        VL_BC_PIS := itemdoc.pisBC;
                        ALIQ_PIS_PERC := itemdoc.pisAliquota;
                        QUANT_BC_PIS := 0;
                        ALIQ_PIS_R := 0;
                        VL_PIS := itemdoc.pisValor;
                      end;

                      if itemdoc.cstcofins.codigo > '' then
                      begin
                        CST_COFINS := itemdoc.cstcofins.codigo;
                        VL_BC_COFINS := VL_ITEM;
                        ALIQ_COFINS_PERC := 0;
                        QUANT_BC_COFINS := 0;
                        ALIQ_COFINS_R := 0;
                        VL_COFINS := itemdoc.cofinsValor;
                      end;
                      COD_CTA := '000';
                      VL_ABAT_NT := itemdoc.desconto;

                      inc(contadorLocal);
                    end;
                  end;
                end;

//                var
//                DocumentosFiscaisC190 := InfoAPI()
//                  .GetArray<TDocumentoFiscalItemAgrupado>
//                  ('fiscal/documentofiscal/sped?estabelecimentoid=' +
//                  Estabelecimento.id + '&documentofiscalid=' + documentoFiscal.id +
//                  '&agrupado=1');

                valorTotDebitosBlocoE := 0.0;
                for var documento190 in DocumentosFiscaisC190Aux do
                begin
                  if documento190.documentofiscalid = documentoFiscal.id then
                  begin
                    tbDocumentoAgrupado.First;
                    if not tbDocumentoAgrupado.Locate('cfop;csticms;aliqicms',
                      VarArrayOf([documento190.CFOP, documento190.CST_ICMS,
                      documento190.ALIQ_ICMS]), []) then
                    begin
                      tbDocumentoAgrupado.Append;
                      tbDocumentoAgrupado.fieldByname('cfop').Asstring :=
                        documento190.CFOP;
                      tbDocumentoAgrupado.fieldByname('csticms').Asstring :=
                        documento190.CST_ICMS;
                      tbDocumentoAgrupado.fieldByname('aliqicms').AsCurrency :=
                        documento190.ALIQ_ICMS;
                      tbDocumentoAgrupado.Post;

                      with RegistroC190New do
                      begin
                        var
                        csticms := StrToInt(documento190.CST_ICMS);
                        ALIQ_ICMS := documento190.ALIQ_ICMS;
                        VL_BC_ICMS := documento190.VL_BC_ICMS;
                        VL_ICMS := documento190.VL_ICMS;
                        VL_BC_ICMS_ST := documento190.VL_BC_ICMS_ST;
                        VL_ICMS_ST := documento190.VL_ICMS_ST;
                        VL_RED_BC := documento190.VL_RED_BC;
                        VL_IPI := documento190.VL_IPI;
                        CST_ICMS := documento190.CST_ICMS;
                        CFOP := documento190.CFOP;
                        VL_OPR := documento190.VL_OPR + documento190.vl_ipi - documento190.vl_desconto;
                        COD_OBS := documento190.COD_OBS;

                        valorOutrosCredICMSST := valorOutrosCredICMSST + documento190.VL_ICMS_ST;

                        if (copy(documento190.CFOP, 1, 1) = '5') or
                          (copy(documento190.CFOP, 1, 1) = '6') or
                          (copy(documento190.CFOP, 1, 1) = '7') or
                          (documento190.CFOP = '1605') then
                        begin
                          valorTotDebitosBlocoE := valorTotDebitosBlocoE + VL_ICMS;
                        end;
                      end;
                    end;
                  end;
                end;
              end;

              if (Estabelecimento.estabelecimentoDocumentos[0].spedperfil = 'A') then
              begin
                var
                DocumentosFiscais800 := InfoAPI().GetPagedArray<TDocumentoFiscalSPED>
                  ('fiscal/documentofiscal/sped?estabelecimentoid=' +
                  Estabelecimento.id + '&periodo=intervalo&inicio=' + dataIniSped +
                  '&conclusao=' + dataFinSped + '&modelo=59');

                for var documentoFiscal800 in DocumentosFiscais800 do
                begin
                  tbDocumentoAgrupado.EmptyDataSet;
                  with RegistroC800New do
                  begin
                    COD_MOD := documentoFiscal800.modelo;
                    if documentoFiscal800.documentofiscalcfe.chaveCancelamento > ''
                    then
                      COD_SIT := TACBrCodSit.sdCancelado
                    else
                    begin
                      COD_SIT := TACBrCodSit.sdRegular;
                      DT_DOC := documentoFiscal800.emissao;
                      VL_CFE := documentoFiscal800.total;
                    end;

                    NUM_CFE := IntToStr(documentoFiscal800.documentofiscalcfe.numero);
                    VL_PIS := documentoFiscal800.valorpis;
                    VL_COFINS := documentoFiscal800.valorcofins;
                    CNPJ_CPF := '';
                    NR_SAT := IntToStr(documentoFiscal800.documentofiscalcfe.serie);
                    CHV_CFE := documentoFiscal800.documentofiscalcfe.chave;
                    VL_DESC := documentoFiscal800.desconto;
                    VL_MERC := documentoFiscal800.valormercadoria;
                    VL_OUT_DA := documentoFiscal800.outrasdespesas;
                    VL_PIS_ST := documentoFiscal800.valorpisst;
                    VL_COFINS_ST := documentoFiscal800.valorcofinsst;

                    var
                    valorICMSaux := 0.0;
                    if documentoFiscal800.documentofiscalcfe.chaveCancelamento = ''
                    then
                    begin
                      var
                      DocumentosFiscaisC850 := InfoAPI()
                        .GetArray<TDocumentoFiscalItemAgrupado>
                        ('fiscal/documentofiscal/sped?estabelecimentoid=' +
                        Estabelecimento.id + '&documentofiscalid=' +
                        documentoFiscal800.id + '&agrupado=1&modelo=59');

                      for var documentoC850 in DocumentosFiscaisC850 do
                      begin
                        tbDocumentoAgrupado.First;
                        if not tbDocumentoAgrupado.Locate('cfop;csticms;aliqicms',
                          VarArrayOf([documentoC850.CFOP, documentoC850.CST_ICMS,
                          documentoC850.ALIQ_ICMS]), []) then
                        begin
                          tbDocumentoAgrupado.Append;
                          tbDocumentoAgrupado.fieldByname('cfop').Asstring :=
                            documentoC850.CFOP;
                          tbDocumentoAgrupado.fieldByname('csticms').Asstring :=
                            documentoC850.CST_ICMS;
                          tbDocumentoAgrupado.fieldByname('aliqicms').AsCurrency :=
                            documentoC850.ALIQ_ICMS;
                          tbDocumentoAgrupado.Post;
                          with RegistroC850New do
                          begin
                            CST_ICMS :=
                              FormatFloat('000', strTOfloat(documentoC850.CST_ICMS));
                            CFOP := documentoC850.CFOP;
                            ALIQ_ICMS := documentoC850.ALIQ_ICMS;
                            VL_OPR := documentoC850.VL_OPR;
                            VL_BC_ICMS := documentoC850.VL_BC_ICMS;
                            VL_ICMS := documentoC850.VL_ICMS;

                            if (copy(documentoC850.CFOP, 1, 2) = '14') or
                              (copy(documentoC850.CFOP, 1, 2) = '54') then
                              VL_ICMS := 0;

                            valorICMSaux := valorICMSaux + VL_ICMS;

                            COD_OBS := documentoC850.COD_OBS;
                            var
                            csticms := StrToInt(documentoC850.CST_ICMS);
                          end;
                        end;
                      end;
                    end;
                    VL_ICMS := valorICMSaux;
                  end;

                end;
              end;

              if Estabelecimento.estabelecimentoDocumentos[0].spedperfil = 'B' then
              begin
                var
                DocumentosFiscais860 := InfoAPI().GetPagedArray<TDocumentofiscalSPED860>
                  ('fiscal/documentofiscal/sped?estabelecimentoid=' +
                  Estabelecimento.id + '&periodo=intervalo&inicio=' + dataIniSped +
                  '&conclusao=' + dataFinSped + '&cupons=1&modelo=59');

                var
                  tb860: TClientDataSet;
                tb860 := TClientDataSet.Create(nil);
                tb860.Fields.Clear;
                tb860.FieldDefs.Add('serie', ftInteger);
                tb860.FieldDefs.Add('emissao', ftDateTime);
                tb860.FieldDefs.Add('numeroinical', ftInteger);
                tb860.FieldDefs.Add('numerofinal', ftInteger);
                tb860.FieldDefs.Add('modelo', ftString, 5);
                tb860.CreateDataSet;
                tb860.Open;

                for var documentoFiscal860 in DocumentosFiscais860 do
                begin
                  if not tb860.Locate('serie;emissao',
                    VarArrayOf([documentoFiscal860.documentofiscalcfe.serie, documentoFiscal860.emissao]
                    ), []) then
                  begin
                    tb860.Append;
                    tb860.fieldByname('numeroinical').AsInteger :=
                      documentoFiscal860.documentofiscalcfe.numero;
                    tb860.fieldByname('serie').AsInteger := documentoFiscal860.documentofiscalcfe.serie;
                    tb860.fieldByname('emissao').AsDateTime :=
                      documentoFiscal860.emissao;
                    tb860.fieldByname('modelo').Asstring := documentoFiscal860.modelo;
                    tb860.Post;
                  end
                  else
                  begin
                    tb860.Edit;
                    tb860.fieldByname('numerofinal').AsInteger :=
                      documentoFiscal860.documentofiscalcfe.numero;
                    tb860.Post;
                  end;
                end;

                tb860.First;
                while not tb860.eof do
                begin
                  with RegistroC860New do
                  begin
                    COD_MOD := tb860.fieldByname('modelo').Asstring;
                    NR_SAT := tb860.fieldByname('serie').Asstring;
                    DT_DOC := tb860.fieldByname('emissao').AsDateTime;
                    DOC_INI := tb860.fieldByname('numeroinical').Asstring;
                    DOC_FIN := tb860.fieldByname('numerofinal').Asstring;
                  end;

                  var
                  dataEmissao890 := FormatDateTime('yyyy-mm-dd',
                    tb860.fieldByname('emissao').AsDateTime);

                  var
                  DocumentosFiscais890 := InfoAPI()
                    .GetArray<TDocumentoFiscalItemAgrupado>
                    ('fiscal/documentofiscal/sped?estabelecimentoid=' +
                    Estabelecimento.id + '&agrupado=1&modelo=59&equipamento=' +
                    tb860.fieldByname('serie').Asstring + '&periodo=intervalo&inicio='
                    + dataEmissao890 + '&conclusao=' + dataEmissao890);

                  tbDocumentoAgrupado.EmptyDataSet;
                  valorTotDebitosBlocoE := 0.0;

                  for var documentoFiscal890 in DocumentosFiscais890 do
                  begin
                    if not tbDocumentoAgrupado.Locate('cfop;csticms;aliqicms',
                      VarArrayOf([documentoFiscal890.CFOP,
                      documentoFiscal890.CST_ICMS, documentoFiscal890.ALIQ_ICMS]), [])
                    then
                    begin
                      tbDocumentoAgrupado.Append;
                      tbDocumentoAgrupado.fieldByname('cfop').Asstring :=
                        documentoFiscal890.CFOP;
                      tbDocumentoAgrupado.fieldByname('csticms').Asstring :=
                        documentoFiscal890.CST_ICMS;
                      tbDocumentoAgrupado.fieldByname('aliqicms').AsCurrency :=
                        documentoFiscal890.ALIQ_ICMS;
                      tbDocumentoAgrupado.Post;

                      with RegistroC890New do
                      begin
                        CST_ICMS := documentoFiscal890.CST_ICMS;
                        CFOP := documentoFiscal890.CFOP;
                        VL_OPR := documentoFiscal890.VL_OPR;
                        if (copy(documentoFiscal890.cst_icms, 1, 2) = '30') or
                          (copy(documentoFiscal890.cst_icms, 1, 2) = '40') or
                          (copy(documentoFiscal890.cst_icms, 1, 2) = '41') or
                          (copy(documentoFiscal890.cst_icms, 1, 2) = '50') or
                          (copy(documentoFiscal890.cst_icms, 1, 2) = '60') then
                        begin
                          ALIQ_ICMS := 0.0;
                          VL_ICMS := 0.0;
                          VL_BC_ICMS := 0.0;
                        end
                        else
                        begin
                          ALIQ_ICMS := documentoFiscal890.ALIQ_ICMS;
                          VL_ICMS := documentoFiscal890.VL_ICMS;
                          VL_BC_ICMS := documentoFiscal890.VL_BC_ICMS;
                        end;
                        COD_OBS := '';

                        if (copy(documentoFiscal890.CFOP, 1, 1) = '5') or
                          (copy(documentoFiscal890.CFOP, 1, 1) = '6') or
                          (copy(documentoFiscal890.CFOP, 1, 1) = '7') or
                          (documentoFiscal890.CFOP = '1605') then
                        begin
                          valorTotDebitosBlocoE := valorTotDebitosBlocoE + VL_ICMS;
                        end;
                      end;
                    end;
                  end;
                  tb860.Next;
                end;
                tb860.Free;
                tbDocumentoAgrupado.EmptyDataSet;
              end;

              tbDocumentoAgrupado.Close;
              tbDocumentoAgrupado.Free;
            end;
        end;

        with SpedFiscal.Bloco_E do
        begin
          with RegistroE001New do
          begin
            if not(indMovimento = 0) then
              IND_MOV := imSemDados
            else
            begin
              IND_MOV := imComDados;
              with RegistroE100New do
              begin
                DT_INI := dataInicial;
                DT_FIN := dataFinal;
                with RegistroE110 do
                begin
                  VL_TOT_CREDITOS := valorTtotalGeralIcms;
                  VL_TOT_DEBITOS  := valorTotDebitosBlocoE;
                  VL_SLD_CREDOR_TRANSPORTAR := valorTtotalGeralIcms - valorTotDebitosBlocoE;
                end;
              end;
            end;
          end;
          if valorOutrosCredICMSST > 0 then
          begin
            with RegistroE200New do
            begin
              UF      := estabelecimento.estabelecimentoEnderecos[0].uf.sigla;
              DT_INI  := dataInicial;
              DT_FIN  := dataFinal;

              with RegistroE210New do
              begin
                IND_MOV_ST  := TACBrMovimentoST(0);
                VL_OUT_CRED_ST := valorOutrosCredICMSST;
                VL_SLD_CRED_ST_TRANSPORTAR := valorOutrosCredICMSST;
              end;
            end;
          end;
        end;

        with SpedFiscal.Bloco_G do
        begin
          with RegistroG001New do
            IND_MOV := imSemDados;
        end;
        SpedFiscal.WriteBloco_G;

        if inventario <> '' then
        begin
          var ValorInventario : Currency := 0.00;
          var
            InventariosFiscais := InfoAPI().GetPagedArray<TInventarioFiscal>
              ('fiscal/inventariofiscal/sped?estabelecimentoid=' + Estabelecimento.id
              + '&inventariofiscal=' + inventario);

          for var inventarioFiscal in InventariosFiscais do
          begin
            for var itemInventario in inventarioFiscal.InventarioFiscalItens do
            begin
              ValorInventario := ValorInventario + itemInventario.Total;
            end;
          end;

          if ValorInventario > 0 then
          begin
            with SpedFiscal.Bloco_H do
            begin
              with RegistroH001New do
               IND_MOV := imComDados;

              for var inventarioFiscal in InventariosFiscais do
              begin
                with RegistroH005New do
                begin
                  DT_INV  := inventarioFiscal.Referencia;
                  VL_INV  := ValorInventario;
                  MOT_INV := StrToMotInv(FormatFloat('00', inventarioFiscal.Motivo));

                  for var itemInventario in inventarioFiscal.InventarioFiscalItens do
                  begin
                    with RegistroH010New do
                    begin
                      COD_ITEM := itemInventario.item.codigo;
                      UNID     := itemInventario.Medida;
                      QTD      := itemInventario.Quantidade;
                      VL_UNIT  := itemInventario.Valor;
                      VL_ITEM  := itemInventario.Total;
                      case itemInventario.IndicadorDePropriedade of
                        0: IND_PROP := piInformante;
                        1: IND_PROP := piInformanteNoTerceiro;
                        2: IND_PROP := piTerceiroNoInformante;
                      else
                        IND_PROP := piInformante;
                      end;
                      COD_PART  := '';
                      TXT_COMPL := '';
                      COD_CTA   := '1';
                      if (inventarioFiscal.Motivo in [2,3,4,5]) and (itemInventario.ValorIcms > 0) then
                      begin
                        if Assigned(itemInventario.cstIcms) then
                        begin
                          with RegistroH020New do
                          begin
                            if Assigned(itemInventario.cstIcms) then
                            begin
                              CST_ICMS := itemInventario.cstIcms.codigo.PadLeft(3, '0');
                              BC_ICMS  := itemInventario.valorbcicmsst;
                              VL_ICMS  := itemInventario.ValorIcms;
                            end;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
            SpedFiscal.WriteBloco_H;
          end;
        end
        else
        begin
          with SpedFiscal.Bloco_H do
          begin
            with RegistroH001New do
              IND_MOV := imSemDados;
          end;
          SpedFiscal.WriteBloco_H;
        end;

        with SpedFiscal.Bloco_K do
        begin
          with RegistroK001New do
            IND_MOV := imSemDados;
        end;
        SpedFiscal.WriteBloco_K;

        with SpedFiscal.Bloco_1 do
        begin
          with Registro1001New do
          begin
            if not(indMovimento = 0) then
              IND_MOV := imSemDados
            else
            begin
              IND_MOV := imComDados;
              with Registro1010New do
              begin
                IND_EXP := 'N';
                IND_CCRF := 'N';
                IND_COMB := 'N';
                IND_USINA := 'N';
                IND_VA := 'N';
                IND_EE := 'N';
                IND_CART := 'N';
                IND_FORM := 'N';
                IND_AER := 'N';
                IND_GIAF1 := 'N';
                IND_GIAF3 := 'N';
                IND_GIAF4 := 'N';
                IND_REST_RESSARC_COMPL_ICMS := 'N';
              end;
            end;
          end;
        end;
        SpedFiscal.WriteBloco_1;

        with SpedFiscal.Bloco_9 do
        begin
          with Registro9001 do
            if not(indMovimento = 0) then
              IND_MOV := imSemDados
            else
              IND_MOV := imComDados;
        end;
        SpedFiscal.WriteBloco_9;

        SpedFiscal.WriteBloco_C(False);

        SpedFiscal.WriteBloco_E;
        SpedFiscal.Conteudo.UseLocale := True;
      end;
      Result := True;
    except
      On E: Exception do
      begin
        cErros := E.Message;
        Result := False;
      end;
    end;
  finally
    if Assigned(SpedFiscal) then
      FreeAndNil(SpedFiscal);
  end;
end;

end.
