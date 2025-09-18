unit Model.Fortes;

interface

uses
  Classes, DateUtils, SysUtils, ACBrTXTClass, Variants, ACBrEFDBlocos, StrUtils,
  Model.Estabelecimento, APIService, DBClient, DB, Model.Operacao, Api.Funcoes,
  model.Base, Contnrs,  Model.Categoria,
  Model.Fortes.RegistroCAB, Model.Fortes.RegistroPAR, Model.Fortes.RegistroGRP,
  Model.Fortes.RegistroUND, Model.Fortes.RegistroNOP, Model.Fortes.RegistroPRO,
  Model.Fortes.RegistroOUM, Model.Fortes.RegistroNFM, Model.Fortes.RegistroPNM,
  Model.Fortes.RegistroDNM, Model.DocumentoFiscalSPED, Model.DocumentoFiscalSPEDItem;

type
  TFortes = class
  private
    idEstabelecimento: String;
    dataReferencia: TDate;
    procedure RegistroCAB;
    procedure RegistroPAR;
    procedure RegistroGRP;
    procedure RegistroUND;
    procedure RegistroNOP;
    procedure RegistroPRO;
    procedure RegistroNFM;
    procedure RegistroPNM(DocumentoFiscal: TDocumentoFiscalSPED);
    procedure RegistroDNM;
    procedure GeraArquivofs(Arquivo: String);
  public
    constructor Create;
    function gerar(Estabelecimento: TEstabelecimentoC;
      dataInicial, dataFinal: TDate; out nomeArquivo: String;
      out cErros: String; finalidade: TACBrCodFin = raOriginal): boolean;
  end;

implementation
uses Model.Parceiro, Model.Unidade, Model.Item;
    // Model.Contabilista, Model.DocumentoFiscalSPED, Model.DocumentoFiscalItemAgrupado

var
  FortesLista: TStringList; dataIniSped, dataFinSped: String;
  EstabelecimentoAux: TEstabelecimentoC;
  dataGeracao: TdateTime;
  dataTexto: String;

constructor TFortes.Create;
begin
  inherited Create;
end;

function TFortes.gerar(Estabelecimento: TEstabelecimentoC;
  dataInicial, dataFinal: TDate; out nomeArquivo: String; out cErros: String; finalidade: TACBrCodFin): boolean;
var
  path: String;
begin
  try
    {$IFDEF MSWINDOWS}
       Path := ExtractFileDir(ParamStr(0)) + '\fortes';
    {$ELSE}
       Path := './documentos/sped/';
    {$ENDIF}
    dataIniSped := FormatDateTime('yyyy-mm-dd', dataInicial);
    dataFinSped := FormatDateTime('yyyy-mm-dd', dataFinal);
    dataGeracao := dataInicial;
    dataTexto   := ReplaceStr(DateToStr(dataInicial), '/', '') + '-' +  ReplaceStr(DateToStr(dataFinal), '/', '');
    nomeArquivo := Path + 'fortes' + Estabelecimento.estabelecimentoDocumentos[0].documentoNumero + '_' + dataTexto + '.txt';
    FortesLista := TStringList.Create;
    EstabelecimentoAux := Estabelecimento;

    try
      RegistroCAB;
      RegistroPAR;
      // RegistroGRP;
      RegistroUND;
      RegistroNOP;
      RegistroPRO;
      // RegistroNFM;

      GeraArquivofs(nomeArquivo);

      Result := True;
    except
      On E: Exception do
      begin
        cErros := E.Message;
        Result := False;
      end;
    end;
  finally
    FortesLista.Free;
  end;
end;

procedure TFortes.RegistroCAB;
var
  RegCabFortes: TRegistroCAB;
begin
  var MesGeracao := FormatDateTime('mmm', dataGeracao);

  RegCabFortes := TRegistroCAB.Create;
  try
    // RegCabFortes.DataInicial := dataIniSped;
    // RegCabFortes.DataFinal   := dataFinSped;
    RegCabFortes.Empresa     := EstabelecimentoAux.nome;
    RegCabFortes.Comentario  := 'ACFSICAL ' + UpperCase(MesGeracao) + ' ' + IntToStr(YearOf(dataGeracao));
    FortesLista.Add(RegCabFortes.GerarLinha);
  finally
    RegCabFortes.Free;
  end;
end;

procedure TFortes.RegistroDNM;
var
   RegDnmForte : TRegistroDNM;
begin
   RegDnmForte := TRegistroDNM.Create;
   try
     FortesLista.Add(RegDnmForte.GerarLinha)
   finally
     RegDnmForte.Free;
   end;
end;

procedure TFortes.RegistroGRP;
var
  RegGrpFortes: TRegistroGRP;
begin

  var categorias := InfoAPI().GetPagedArray<TRegistroGRP>
           ('recurso/categoria/sped?estabelecimentoid=' + EstabelecimentoAux.id +
            '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' +  dataFinSped);

  RegGrpFortes := TRegistroGRP.Create;
  try
    for var categoria in Categorias do
    begin
      RegGrpFortes.Codigo := categoria.Codigo;
      RegGrpFortes.Descricao := upperCase(categoria.Descricao);
      FortesLista.Add(RegGrpFortes.GerarLinha);
    end;
  finally
    RegGrpFortes.Free;
  end;
end;

procedure TFortes.RegistroPNM(DocumentoFiscal: TDocumentoFiscalSPED);
var
  RegPnmFortes: TRegistroPNM;
begin
   RegPnmFortes := TRegistroPNM.Create;
   try
     for var itemdoc in DocumentoFiscal.documentofiscalitens do
     begin
        RegPnmFortes.Produto := strToint(itemdoc.item.codigo);
        if Assigned(itemdoc.cfop) then
          RegPnmFortes.CFOP := strTOint(itemdoc.cfop.codigo);

        RegPnmFortes.CFOPTransferencia := -1;

        if Assigned(itemdoc.csticms) then
          RegPnmFortes.CSTA := strToint(itemdoc.csticms.codigo);

        RegPnmFortes.UnidadeDeMedida := itemdoc.item.unidade.codigo;
        RegPnmFortes.Quantidade      := itemdoc.quantidade;
        RegPnmFortes.ValorBruto      := itemdoc.subtotal;
        RegPnmFortes.ValorDoIPI      := itemdoc.ipiValor;

        // helder

//        if (DocumentoFiscal.parceiro.parceiroDocumentos[0].documentoTipo in [1, 2]) and
//           (UpperCase(documentoFiscal.parceiro.parceiroDocumentos[0].inscricaoEstadual) = 'ISENTO') then
//          RegPnmFortes.TributacaoICMS := 2
//        else if ((documentoFiscal.parceiro.parceiroDocumentos[0].documentoTipo in [1, 2])) and (documentoFiscal.parceiro.parceiroDocumentos[0].inscricaoEstadual > '') then
//          RegPnmFortes.TributacaoICMS := 1
//        else
//          RegPnmFortes.TributacaoICMS := 3;

        RegPnmFortes.BaseDeCalculoDoICMS := itemdoc.icmsBC;
        RegPnmFortes.AliquotaDoICMS      := itemdoc.icmsAliquota;    //13

        // helder
//        if itemdoc.csticms.codigo = '10' then
//        begin
//          RegPnmFortes.Icmsstbc    := itemdoc.icmsSTBC;
//          RegPnmFortes.Icmsstvalor := itemdoc.icmsValor;
//        end
//        else
//        begin
//          RegPnmFortes.Icmsstbc    := 0.00;
//          RegPnmFortes.Icmsstvalor := 0.00;
//        end;

        RegPnmFortes.TipoDeSubstituicao      := 1;
        RegPnmFortes.CustoAquisicaoSubstTrib := 0.00;
        RegPnmFortes.PercAgregSubstituicao   := 0.00;
        RegPnmFortes.AliquotaSubstTributaria := itemdoc.icmsSTAliquota;
        RegPnmFortes.ValorIPI := itemdoc.ipiValor;
        RegPnmFortes.BaseDeCalculoDoIPI := itemdoc.ipiBC;
        RegPnmFortes.AliquotaDoIPI := itemdoc.ipiAliquota;
        RegPnmFortes.CSTIPI := itemdoc.cstipi.codigo.ToInteger;    ////36
        RegPnmFortes.CSTCOFINS := itemdoc.cstcofins.codigo.ToInteger; ////37
        RegPnmFortes.CSTPIS := itemdoc.cstpis.codigo.ToInteger;
        RegPnmFortes.BaseDeCalculoCOFINS := itemdoc.cofinsValor;   ///39
        RegPnmFortes.BaseDeCalculoPIS := itemdoc.ipiBC; //40
      //  RegPnmFortes.ValorFrete := itemdoc.frete;
      // helder  RegPnmFortes.ValorSeguro := itemdoc.seguro;
        RegPnmFortes.ValorDesconto := itemdoc.desconto;
        RegPnmFortes.ValorTotal := itemdoc.total;  /// 44   ver se já está deduzindo o desconto

       // itemdoc.operacao

       // RegPnmFortes.NaturezaReceitaCOFINS

        FortesLista.Add(RegPnmFortes.GerarLinha);
     end;
   finally
     RegPnmFortes.Free;
   end;
end;

procedure TFortes.RegistroNFM;
var
  RegNfmForte : TRegistroNFM;
begin
  var DocumentosFiscais := InfoAPI().GetPagedArray<TDocumentoFiscalSPED>
         ('fiscal/documentofiscal/sped?estabelecimentoid=' + EstabelecimentoAux.id +
          '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' + dataFinSped + '&modelo=55');

  RegNfmForte := TRegistroNFM.Create;
  try
    for var documentoFiscal in DocumentosFiscais do
    begin
      RegNfmForte.Estabelecimento := strToint(EstabelecimentoAux.codigo);
      RegNfmForte.Numero := documentoFiscal.numero;
      RegNfmForte.Operacao := 'E';  ///////########################## ver aki depois

      // helder
//      if documentoFiscal.modelo = '55' then
//        RegNfmForte.Modelo := 'NFE'
//      else if documentoFiscal.modelo = '65' then
//        RegNfmForte.Modelo := 'NFCE'
//      else
//        RegNfmForte.Modelo := 'N';

      RegNfmForte.Serie            := documentoFiscal.documentofiscalnfe.serie.ToString;
      RegNfmForte.Subserie         := '';
      RegNfmForte.Numero           := documentoFiscal.numero;
      RegNfmForte.DataEmissao      := documentoFiscal.emissao;
      RegNfmForte.DataEntradaSaida := documentoFiscal.saida;

    //helder  RegNfmForte.FormularioInicial := '|'; //Copy(dataIniSped, 1, 4) + Copy(dataIniSped, 6, 2) + Copy(dataIniSped, 9, 2);
    //helder  RegNfmForte.FormularioFinal   := '|'; //Copy(dataFinSped, 1, 4) + Copy(dataFinSped, 6, 2) + Copy(dataFinSped, 9, 2);

      if documentoFiscal.parceiro.codigo > '' then
        RegNfmForte.RemetenteDestinatario := strTOint(documentoFiscal.parceiro.codigo)
      else
        RegNfmForte.RemetenteDestinatario := 0;

      RegNfmForte.Produtos               := documentoFiscal.total;
      RegNfmForte.Frete                  := documentoFiscal.frete;
      RegNfmForte.Seguro                 := documentoFiscal.valorseguro;
      RegNfmForte.ICMSImportacao         := documentoFiscal.icmsValor;
      RegNfmForte.ICMSImportacaoDiferido := 0.00;
      RegNfmForte.IPI                    := documentoFiscal.ipiValor;
      RegNfmForte.SubstituicaoRetido     := 0.00;
      RegNfmForte.ServicoISS             := 0.00;
      RegNfmForte.DescontoTotal          := documentoFiscal.desconto;
      RegNfmForte.ValorTotal             := documentoFiscal.total;
      RegNfmForte.QuantProdutos          := 0;

      if RegNfmForte.Operacao = 'S' then
      begin
        RegNfmForte.SubstituicaoRecolher  := false;
        RegNfmForte.AntecipadoRecolher    := false;
        RegNfmForte.DiferencialDeAliquota := false;
        RegNfmForte.IndicadorDePresencaDoComprador := 1;
      end
      else if RegNfmForte.Operacao = 'E' then
      begin
        RegNfmForte.ISSRetido := false;
        RegNfmForte.IndicadorDeOperacaoConsumidorFinal := False;
      end;

      RegNfmForte.ValorSusbstituicao := documentoFiscal.icmsSTValor;
      RegNfmForte.BaseSubstituicao   := documentoFiscal.icmsSTBC;
      RegNfmForte.ValorAntecipado    := 0.00;

      if documentoFiscal.natureza = 0 then
        RegNfmForte.Fatura := 'N'
      else
        RegNfmForte.Fatura := 'V';

      RegNfmForte.ReceitaTributavelCOFINS := 0.00;
      RegNfmForte.ReceitaTributavelPIS    := 0.00;
      RegNfmForte.ReceitaTributavelCSL1   := 0.00;
      RegNfmForte.ReceitaTributavelCSL2   := 0.00;
      RegNfmForte.ReceitaTributavelIRPJ1  := 0.00;
      RegNfmForte.RejeitaTributavelIRPJ2  := 0.00;
      RegNfmForte.RejeitaTributavelIRPJ3  := 0.00;
      RegNfmForte.ReceitaTributavelIRPJ4  := 0.00;
      RegNfmForte.COFINSRetidoNaFonte     := 0.00;
      RegNfmForte.PISRetidoNaFonte        := 0.00;
      RegNfmForte.CSLRetidoNaFonte        := 0.00;
      RegNfmForte.IRPJRetidoNaFonte       := 0.00;
      RegNfmForte.GeraTransferencia       := false;
      RegNfmForte.Observacao              := '';
      RegNfmForte.AliquotaSubstituicaoTributaria             := 0; //ver quando saida
      RegNfmForte.ChaveEletronica                            := documentoFiscal.chave;
      RegNfmForte.INSSRetidoNaFonte                          := 0.00;
      RegNfmForte.BaseCOFINSPISNaoCumulativos                := 0.00;
      RegNfmForte.MotivoDoCancelamento                       := '';
      RegNfmForte.CodigoDaInformacaoComplementar             := -1;
      RegNfmForte.ComplementoDaInformacaoComplementar        := '';
      RegNfmForte.UFDeEmbarque                               := '';
      RegNfmForte.LocalDeEmbarque                            := '';
      RegNfmForte.CodigoContabil                             := -1;
      RegNfmForte.ChaveNFeDeReferencia                       := '';
      RegNfmForte.InformacaoAdicionalFisco                   := '';
      RegNfmForte.NFeInformadaPeloContribuinte               := False;
      RegNfmForte.TotalDoICMSDesoneracao                     := 0.00;
      RegNfmForte.PrestadorDeServicosEmObraDeConstrucaoCivil := 0;

      FortesLista.Add(RegNfmForte.GerarLinha);

      RegistroPNM(documentoFiscal);

     // RegistroDNM;
    end;

  finally
    RegNfmForte.Free;
  end;
end;

procedure TFortes.RegistroNOP;
var
  RegNopForte: TRegistroNOP;
begin
  var Operacaoes := InfoAPI().GetPagedArray<TOperacao>
         ('fiscal/cfop/sped?estabelecimentoid=' + EstabelecimentoAux.id +
          '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' + dataFinSped);
  RegNopForte := TRegistroNOP.Create;
  try
    for var Operacao in Operacaoes do
    begin
      RegNopForte.Codigo := strToint(Operacao.codigo);
      if Length(Operacao.nome) >= 50 then
        RegNopForte.Descricao := upperCase(copy(Operacao.nome, 1, 50))
      else
        RegNopForte.Descricao := upperCase(Operacao.nome);
      FortesLista.add(RegNopForte.GerarLinha);
    end;
  finally
    RegNopForte.Free;
  end;
end;

procedure TFortes.RegistroPAR;
var
  RegParForte: TRegistroPAR;
begin
  var Parceiros := InfoAPI().GetPagedArray<TParceiro>
            ('agente/parceiro/sped?estabelecimentoid=' + EstabelecimentoAux.id +
            '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' +
            dataFinSped);
  RegParForte := TRegistroPAR.Create;
  try
    for var Parceiro in Parceiros do
    begin
      RegParForte.Codigo := strToInt(Parceiro.codigo);
      RegParForte.Nome := Parceiro.NOME;
      RegParForte.Pais := 1058;
      if Assigned(Parceiro.parceirodocumentos) then
      begin
        for var documento in Parceiro.parceirodocumentos do
        begin
          RegParForte.CNPJCPF := String(documento.documentoNumero);
          if documento.inscricaoEstadual > '' then
            RegParForte.InscricaoEstadual := SomenteDigitos(documento.inscricaoEstadual)
          else
            RegParForte.InscricaoMunicipal := SomenteDigitos(documento.inscricaoMunicipal)
          end;

          if Assigned(Parceiro.parceiroenderecos) then
          begin
            RegParForte.Logradouro := Parceiro.parceiroenderecos[0].logradouro;
            RegParForte.Numero := Parceiro.parceiroenderecos[0].numero;
            RegParForte.Complemento := Parceiro.parceiroenderecos[0].complemento;
            RegParForte.bairro := Parceiro.parceiroenderecos[0].BAIRRO;
            RegParForte.Municipio :=  StrToInt(Parceiro.parceiroenderecos[0].municipio.codigo);
          end;
      end;
      FortesLista.Add(RegParForte.GerarLinha);
    end;
  finally
    RegParForte.Free;
  end;
end;

procedure TFortes.RegistroPRO;
var
  RegProFortes: TRegistroPRO;
  RegOumFortes : TRegistroOUM;
begin
  var Produtos := InfoAPI().GetPagedArray<TItemC>
            ('recurso/item/sped?estabelecimentoid=' + EstabelecimentoAux.id +
            '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' +  dataFinSped + '&natureza=0');

  RegProFortes := TRegistroPRO.Create;
  RegOumFortes := TRegistroOUM.Create;

  try
    for var produto in Produtos do
    begin
      RegProFortes.Codigo := strTOint(produto.codigo);
      RegProFortes.Descricao := produto.NOME;
      RegProFortes.UnidadeMedida := produto.Unidade.codigo;
      RegProFortes.CodigoNCM := SomenteDigitos(AnsiString(produto.ncm.codigo));
      RegProFortes.UnidadeMedida := produto.unidade.codigo;
      if Assigned(produto.vendaoperacao) and Assigned(produto.vendaoperacao.operacaoicms) then
        RegProFortes.AliqICMSInterna := produto.vendaoperacao.operacaoicms.icmsAliquota;

      FortesLista.Add(RegProFortes.GerarLinha);

      RegOumFortes.CodigoProduto := strTOint(produto.codigo);
      RegOumFortes.UnidadeDeMedida := produto.Unidade.codigo;
      RegOumFortes.UnidadeEquivalentePadrao := 1.00;
      FortesLista.Add(RegOumFortes.GerarLinha);
    end;
  finally
    RegProFortes.Free;
    RegOumFortes.Free;
  end;
end;

procedure TFortes.RegistroUND;
var
  RegUndForte: TRegistroUND;
begin
  var Unidades := InfoAPI().GetPagedArray<TUnidade>
       ('recurso/unidade/sped?estabelecimentoid=' + EstabelecimentoAux.id +
        '&periodo=intervalo&inicio=' + dataIniSped + '&conclusao=' +  dataFinSped + '&natureza=0');

  RegUndForte := TRegistroUND.Create;

  try
    for var Unidade in Unidades do
    begin
      RegUndForte.UnidadeMedida := Unidade.codigo;
      RegUndForte.Descricao := upperCase(Unidade.nome);
      FortesLista.Add(RegUndForte.GerarLinha);
    end;
  finally
    RegUndForte.Free;
  end;
end;

procedure TFortes.GeraArquivofs(Arquivo: String);
begin
  FortesLista.SaveToFile(Arquivo);
end;


end.

