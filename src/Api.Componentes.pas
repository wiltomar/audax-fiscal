unit Api.Componentes;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, IniFiles, ACBrBase, ACBrSAT,
  ACBrDFeSSL, ACBrSATClass, pcnConversao, pcnConversaoNFe, Model.DocumentoFiscal,
  pcnCFe, ACBrDFe, ACBrNFe, ACBrMail, ACBrUtil, ACBrDFeUtil, Api.Funcoes, System.Math;

const
  docModelos: TArray<String> = ['55', '59', '65'];

type
  Tcomponents = class(TDataModule)
    nfe: TACBrNFe;
    sat: TACBrSAT;
    mail: TACBrMail;
    procedure satGetcodigoDeAtivacao(var Chave: AnsiString);
    procedure satGetsignAC(var Chave: AnsiString);
    procedure DataModuleCreate(Sender: TObject);
  private
    FsatCodigoDeAtivacao: AnsiString;
    FsatAssinaturaAC: AnsiString;

    property satCodigoDeAtivacao: AnsiString read FsatCodigoDeAtivacao write FsatCodigoDeAtivacao;
    property satAssinaturaAC: AnsiString read FsatAssinaturaAC write FsatAssinaturaAC;

    function inicializaSAT: boolean;

    procedure carregaSAT;
    procedure carregaNFe(const modelo: string = '55');

    function GerarNFe(const DocumentoFiscal: TDocumentoFiscal): string;
    function GerarCFe(const DocumentoFiscal: TDocumentoFiscal): string;
    procedure GerarNFCe(const DocumentoFiscal: TDocumentoFiscal);

    function CancelarCFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
  public
    function EmiteDFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
    function CancelarDFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;

  end;

var
  components: Tcomponents;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ Tcomponents }

procedure Tcomponents.carregaSAT;
var
  IniFile: String;
  ini: TIniFile;
begin
  try
    IniFile := ChangeFileExt(ParamStr(0), '.ini');

    ini := TIniFile.Create(IniFile);

    try
      with ini do
      begin
        with sat do
        begin
          SSL.SSLCryptLib                 := cryOpenSSL;
          SSL.SSLXmlSignLib               := xsLibXml2;

          Config.ArqSchema                := ReadString('Config - CFe', 'Schemas', 'Schemas');
          Config.PaginaDeCodigo           := ReadInteger('Config - CFe', 'PaginaDeCodigo', 1252);
          Config.EhUTF8                   := ReadBool('Config - CFe', 'UTF', True);
          Config.infCFe_versaoDadosEnt    := ReadFloat('Config - CFe', 'VersaoLayout', 0.07);

          Modelo                          := TACBrSATModelo(ReadInteger('Config - CFe', 'Modelo', 1)) ;
          ArqLOG                          := ReadString('Config - CFe', 'ArqLOG', 'audax-cfe.log');
          NomeDLL                         := ReadString('Config - CFe', 'NomeDLL', '/opt/sefaz/drs/libmfe.so');

          Config.ide_numeroCaixa          := ReadInteger('Config - CFe', 'Caixa', 1);
          Config.ide_tpAmb                := TpcnTipoAmbiente(ReadInteger('Config - CFe', 'Ambiente', 2));
          Config.ide_CNPJ                 := ReadString('SwHouse - CFe', 'CNPJ', '');

          Config.emit_CNPJ                := ReadString('Emitente', 'CNPJ', '');
          Config.emit_IE                  := ReadString('Emitente', 'IE', '');
          Config.emit_IM                  := ReadString('Emitente', 'IM', '');
          Config.emit_cRegTrib            := TpcnRegTrib(ReadInteger('Emitente', 'RegimeICMS', 0));
          Config.emit_cRegTribISSQN       := TpcnRegTribISSQN(ReadInteger('Emitente', 'RegimeISSQN', 0));
          Config.emit_indRatISSQN         := TpcnindRatISSQN(ReadInteger('Emitente', 'IndicadorDeRateio', 1));

          ConfigArquivos.SalvarCFe        := ReadBool('Arquivos - CFe', 'SalvarCFe', True);
          ConfigArquivos.SalvarCFeCanc    := ReadBool('Arquivos - CFe', 'SalvarCancelamento', True);
          ConfigArquivos.SalvarEnvio      := ReadBool('Arquivos - CFe', 'SalvarEnvio', True);
          ConfigArquivos.SepararPorCNPJ   := ReadBool('Arquivos - CFe', 'SalvarPorCNPJ', True);
          ConfigArquivos.SepararPorModelo := ReadBool('Arquivos - CFe', 'SepararPorModelo', False);
          ConfigArquivos.SepararPorDia    := ReadBool('Arquivos - CFe', 'SepararPorDia', False);
          ConfigArquivos.SepararPorMes    := ReadBool('Arquivos - CFe', 'SepararPorMes', True);
          ConfigArquivos.SepararPorAno    := ReadBool('Arquivos - CFe', 'SepararPorAno', True);

          FsatCodigoDeAtivacao            := AnsiString(ReadString('Config - CFe', 'CodigoDeAtivacao', '12345678'));
          FsatAssinaturaAC                := AnsiString(ReadString('SwHouse - CFe', 'Assinatura', ''));

          CFe.IdentarXML                  := True;
          CFe.TamanhoIdentacao            := 3;
          CFe.RetirarAcentos              := True;
        end;
      end;
    finally
      if Assigned(ini) then
        FreeAndNil(ini);
    end;

  except
    on E:Exception do
    begin
      Write(Format('Imposs�vel carregar o SAT. Verificar com suporte, o erro %s.', [E.Message]));
    end;
  end;
end;

procedure Tcomponents.DataModuleCreate(Sender: TObject);
begin
  RemoveDataModule(self);
end;

function Tcomponents.EmiteDFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
var
  xmlDocumento: string;
begin
  try
    case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
      0:
        begin
          xmlDocumento := GerarNFe(DocumentoFiscal);

          // TODO: Implementar gera��o de NFe.

          Result := DocumentoFiscal;
        end;
      1:
        begin
          xmlDocumento := GerarCFe(DocumentoFiscal);

          sat.EnviarDadosVenda(xmlDocumento);

          if sat.Resposta.codigoDeRetorno = 6000 then
          begin
            DocumentoFiscal.documentoFiscalCFe.chave  := sat.CFe.infCFe.ID;
            DocumentoFiscal.documentoFiscalCFe.serie  := sat.CFe.ide.nserieSAT;
            DocumentoFiscal.documentoFiscalCFe.xml    := sat.CFe.AsXMLString;
            DocumentoFiscal.documentoFiscalCFe.numero := sat.CFe.ide.nCFe;
            DocumentoFiscal.documentoFiscalCFe.sessao := sat.Resposta.numeroSessao;
            DocumentoFiscal.documentoFiscalCFe.status := sat.Resposta.codigoDeRetorno;
            DocumentoFiscal.documentoFiscalCFe.formaDeEmissao := StrToIntDef(TpAmbToStr(sat.CFe.ide.tpAmb), 1);

            WriteLn(Format('Emitido o cupom fiscal: %d com chave: %s.', [sat.CFe.ide.nCFe, sat.CFe.infCFe.ID]));

            Result := DocumentoFiscal;
          end
          else
            WriteLn(Format('N�o foi possivel emitir o cupom fiscal, o seguinte erro ocorreu: %s.', [sat.Resposta.mensagemRetorno]));
        end;
      2:
        begin
          GerarNFCe(DocumentoFiscal);

          if nfe.NotasFiscais.Count > 0 then
            nfe.Enviar(DocumentoFiscal.documentoFiscalNFe.numero, False, True);

          DocumentoFiscal.documentoFiscalNFe.status := nfe.WebServices.Enviar.cStat;
          DocumentoFiscal.documentoFiscalNFe.msgRetorno := nfe.WebServices.Enviar.Msg;

          if (nfe.WebServices.Enviar.cStat = 100) and not(nfe.WebServices.Enviar.Protocolo = EmptyStr) then
          begin
            DocumentoFiscal.documentoFiscalNFe.chave := nfe.NotasFiscais.Items[0].NumID;
            DocumentoFiscal.documentoFiscalNFe.xml := nfe.NotasFiscais.Items[0].XMLAssinado;
            DocumentoFiscal.documentoFiscalNFe.protocolo := nfe.WebServices.Enviar.Protocolo;

            WriteLn(Format('%s - Emitida a NFCe de n�mero: %d com chave: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                nfe.NotasFiscais.Items[0].NFe.Ide.nNF, nfe.NotasFiscais.Items[0].NumID]));
          end
          else
            WriteLn(Format('N�o foi poss�vel emitir a NFCe, o seguinte erro ocorreu: %s.', [nfe.WebServices.Retorno.Msg]));

          Result := DocumentoFiscal;
        end;
    end;

  except
    On E: Exception do
    begin
      WriteLn(Format('Houve um erro na tentativa de enviar o documento. Verifique a mensagem a seguir: %s.', [E.Message]));
      Result := nil;
    end;
  end;
end;

function Tcomponents.CancelarDFe(
  DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
begin
  case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
    0:;
    1: Result := CancelarCFe(DocumentoFiscal);
    2:;
  end;
end;

function Tcomponents.CancelarCFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
var
  xmlOriginal, xmlCancelamento: String;
begin
  xmlOriginal := DocumentoFiscal.documentoFiscalCFe.xml;

  inicializaSAT;
  sat.InicializaCFe;

  try
    sat.CFe.SetXMLString(xmlOriginal);
    sat.CFe2CFeCanc;

    xmlCancelamento := sat.CFeCanc.GerarXML(True);

    sat.CFeCanc.AsXMLString := xmlCancelamento;
    sat.CancelarUltimaVenda(sat.CFeCanc.infCFe.chCanc, xmlCancelamento);

    if sat.Resposta.codigoDeRetorno = 7000 then
    begin
      DocumentoFiscal.documentoFiscalCFe.chaveCancelamento := sat.CFeCanc.infCFe.ID;
      DocumentoFiscal.documentoFiscalCFe.xmlCancelamento := sat.CFeCanc.AsXMLString;
      DocumentoFiscal.documentoFiscalCFe.status := sat.Resposta.codigoDeRetorno;
      DocumentoFiscal.documentoFiscalCFe.sessao := sat.Resposta.numeroSessao;

      WriteLn(Format('Cupom fiscal: %d com chave: %s, cancelado com sucesso.', [sat.CFe.ide.nCFe, sat.CFe.infCFe.ID]));

      Result := DocumentoFiscal;

    end
    else
      WriteLn(Format('N�o foi possivel cancelar o cupom fiscal, o seguinte erro ocorreu: %s.', [sat.Resposta.mensagemRetorno]));

  except
    On E: Exception do
    begin
      WriteLn(Format('Houve um erro na tentativa de inicializar o equipamento MFe. Verifique a mensagem a seguir: %s.', [E.Message]));
      Result := nil;
    end;
  end;
end;

function Tcomponents.inicializaSAT: boolean;
begin
  carregaSAT;
  try
    sat.Inicializar;

    if sat.Inicializado then
      Result := True
    else
      Result := False;

  except
    On E: Exception do
    begin
       Write(Format('Erro ao carregar equipamento MFe/SAT. Mensagem de erro: %s.', [E.Message]));
       Result := False;
    end;
  end;

end;

procedure Tcomponents.carregaNFe(const modelo: string = '55');
var
  IniFile: String;
  Ini: TIniFile;
  lOk: Boolean;
begin
  IniFile := ChangeFileExt(ParamStr(0), '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    nfe.SSL.DescarregarCertificado;

    with nfe.Configuracoes.Geral do
    begin
      SSLLib                  := TSSLLib(Ini.ReadInteger('SSL - NFe', 'Lib', 0));
      SSLCryptLib             := TSSLCryptLib(Ini.ReadInteger('SSL - NFe', 'CryptLib', 0));
      SSLHttpLib              := TSSLHttpLib(Ini.ReadInteger('SSL - NFe', 'HttpLib', 0));
      SSLXmlSignLib           := TSSLXmlSignLib(Ini.ReadInteger('SSL - NFe', 'XmlSignLib', 0));

      Salvar                  := Ini.ReadBool('Geral - NFe', 'Salvar', True);
      RetirarAcentos          := Ini.ReadBool('Geral - NFe', 'RetirarAcentos', True);
      AtualizarXMLCancelado   := Ini.ReadBool('Geral - NFe', 'AtualizarXML', True);
      ExibirErroSchema        := Ini.ReadBool('Geral - NFe', 'ExibirErroSchema', True);
      FormaEmissao            := TpcnTipoEmissao(Ini.ReadInteger('Geral - NFe', 'FormaEmissao', 0));
      VersaoDF                := StrToVersaoDF(lOk, Ini.ReadString('Geral - NFe', 'VersaoDF', '4.00'));

      ModeloDF                := StrToModeloDF(lOk, modelo);
      FormatoAlerta           := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.';

      if modelo = docModelos[2] then
      begin
        IdCSC                 := Ini.ReadString('Identificacao - NFSe', 'IdToken', '');
        CSC                   := Ini.ReadString('Identificacao - NFSe', 'CSC', '');
        VersaoQRCode          := veqr200;
      end;
    end;

    with nfe.Configuracoes.Arquivos do
    begin
      Salvar                  := Ini.ReadBool('Arquivos - NFe', 'Salvar', False);
      SepararPorMes           := Ini.ReadBool('Arquivos - NFe', 'SepararPorMes', True);
      AdicionarLiteral        := Ini.ReadBool('Arquivos - NFe', 'AdicionarLiteral', True);
      EmissaoPathNFe          := Ini.ReadBool('Arquivos - NFe', 'EmissaoPathNFe', True);
      SalvarEvento            := Ini.ReadBool('Arquivos - NFe', 'SalvarEvento', True);
      SepararPorCNPJ          := Ini.ReadBool('Arquivos - NFe', 'SepararPorCNPJ', True);
      SepararPorModelo        := Ini.ReadBool('Arquivos - NFe', 'SepararPorModelo', True);
      PathSchemas             := Ini.ReadString('Arquivos - NFe', 'PathSchemas', '');
      PathNFe                 := Ini.ReadString('Arquivos - NFe', 'PathNFe', '');
      PathInu                 := Ini.ReadString('Arquivos - NFe', 'PathInu', '');
      PathEvento              := Ini.ReadString('Arquivos - NFe', 'PathEvento', '');
      PathSalvar              := Ini.ReadString('Arquivos - NFe', 'PathSalvar', '');
    end;

    with nfe.Configuracoes.Certificados do
    begin
      URLPFX                  := Ini.ReadString('Certificado - NFe', 'URL', '');
      ArquivoPFX              := Ini.ReadString('Certificado - NFe', 'CaminhoPFX', '');
      Senha                   := Ini.ReadString('Certificado - NFe', 'SenhaCertificado', '');
      NumeroSerie             := Ini.ReadString('Certificado - NFe', 'NumeroSerie', '');
    end;

    with nfe.Configuracoes.WebServices do
    begin
      Visualizar              := Ini.ReadBool('WebServices - NFe', 'Visualizar', True);
      Salvar                  := Ini.ReadBool('WebServices - NFe', 'Salvar', True);
      AjustaAguardaConsultaRet:= Ini.ReadBool('WebServices - NFe', 'AjustaAguardaConsultaRet', True);
      AguardarConsultaRet     := Ini.ReadInteger('WebServices - NFe', 'AguardarConsultaRet', 30000);
      Tentativas              := Ini.ReadInteger('WebServices - NFe', 'Tentativas', 3);
      IntervaloTentativas     := Ini.ReadInteger('WebServices - NFe', 'IntervaloTentativas', 1000);
      TimeOut                 := Ini.ReadInteger('WebServices - NFe', 'TimeOut', 30);
      ProxyHost               := Ini.ReadString('WebServices - NFe', 'ProxyHost', '');
      ProxyPort               := Ini.ReadString('WebServices - NFe', 'ProxyPort', '8080');
      ProxyUser               := Ini.ReadString('WebServices - NFe', 'ProxyUser', '');
      ProxyPass               := Ini.ReadString('WebServices - NFe', 'ProxyPass', '');
    end;

    with mail do
    begin
      Host                    := Ini.ReadString('Email', 'Servidor', '');
      Port                    := InttoStr(Ini.ReadInteger('Email', 'Porta', 587));
      Username                := Ini.ReadString('Email', 'Usuario', '');
      Password                := decrypt(Ini.ReadString('Email', 'Senha', ''));
      From                    := Ini.ReadString('Email', 'Origem', '');
      SetSSL                  := True;                                                    // SSL - Conexao Segura
      SetTLS                  := True;                                                    // Auto TLS
      ReadingConfirmation     := False;                                                   // Pede confirmacao de leitura do email
      UseThread               := False;                                                   // Aguarda Envio do Email(nao usa thread)
      FromName                := 'Constel Cloud DFe';
    end;

  finally
    Ini.Free;
  end;

end;

procedure Tcomponents.satGetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := FsatCodigoDeAtivacao;
end;

procedure Tcomponents.satGetsignAC(var Chave: AnsiString);
begin
  Chave := FsatAssinaturaAC;
end;

function Tcomponents.GerarNFe(const DocumentoFiscal: TDocumentoFiscal): string;
begin

end;

function Tcomponents.GerarCFe(const DocumentoFiscal: TDocumentoFiscal): string;
var
  TotalItem, TotalImposto, TotalGeral: Double;
  A: Integer;
  lOk: Boolean;
  Counter: Integer;
begin
  TotalItem  := 0;
  TotalGeral := 0;
  TotalImposto := 0;

  inicializaSAT;
  sat.InicializaCFe;

  with sat.CFe, DocumentoFiscal do
  begin
    ide.cNF := Random(999999);

    if Length(cpfInformado) > 0 then
      Dest.CNPJCPF            := cpfInformado;

    For Counter := 0 to Length(DocumentoFiscalItens) - 1 do
    begin
      with Det.New, DocumentoFiscalItens[Counter] do
      begin
        nItem := Counter + 1;
        Prod.cProd := item.codigo;
        Prod.cEAN := '';
        Prod.xProd := item.nome;
        prod.NCM := StringReplace(ncm.codigo, '.', '', [rfReplaceAll]);
        Prod.CFOP := cfop.codigo;
        Prod.uCom := unidade.codigo;
        Prod.qCom := quantidade;
        Prod.vUnCom := valor;
        Prod.indRegra := irTruncamento;
        Prod.vDesc := DocumentoFiscalItens[Counter].desconto;

        with Prod.obsFiscoDet.New do
        begin
          xCampoDet := 'campo';
          xTextoDet := 'texto';
        end;

        TotalItem := RoundABNT((Prod.qCom * Prod.vUnCom) + Prod.vOutro - Prod.vDesc, -2);
        TotalGeral := TotalGeral + TotalItem;
        Imposto.vItem12741 := TotalItem * (icmsAliquota/100);
        TotalImposto := TotalImposto + Imposto.vItem12741;

        Imposto.ICMS.orig := StrToOrig(lOk, origemDamercadoria.codigo);
        if Emit.cRegTrib = RTSimplesNacional then
          Imposto.ICMS.CSOSN := StrToCSOSNIcms(lOk, cstICMS.codigo)
        else
          Imposto.ICMS.CST := StrToCSTICMS(lOk, cstICMS.codigo);

        Imposto.ICMS.pICMS := icmsAliquota;

        Imposto.PIS.CST := StrToCSTPIS(lOk, cstPIS.codigo);
        Imposto.PIS.vBC := TotalItem;
        Imposto.PIS.pPIS := pisAliquota;

        Imposto.COFINS.CST := StrToCSTCOFINS(lOk, CSTCOFINS.codigo);
        Imposto.COFINS.vBC := TotalItem;
        Imposto.COFINS.pCOFINS := cofinsAliquota;

        infAdProd := '';
      end;
    end;

    sat.CFe.Total.DescAcrEntr.vDescSubtot := DocumentoFiscal.desconto;
    sat.CFe.Total.vCFeLei12741 := TotalImposto;

    for Counter := 0 to Length(DocumentoFiscalPagamentos) - 1 do
    begin
      with Pagto.New do
      begin
        cMP := StrToCodigoMP(lOk, DocumentoFiscalPagamentos[Counter].formaIndicador);
        vMP := DocumentoFiscalPagamentos[Counter].valor;
        if StrToCodigoMP(lOk, DocumentoFiscalPagamentos[Counter].formaIndicador) in [MPCartaodeCredito, mpCartaodeDebito] then
          cAdmC := StrToIntDef(DocumentoFiscalPagamentos[Counter].cartaoCredenciadora, 999);
      end;
    end;

    InfAdic.infCpl := 'Acesse constel.cloud para obter maiores;informa��es sobre o sistema Constel';

  end;

  result := sat.CFe.GerarXML(True);

end;

procedure Tcomponents.GerarNFCe(const DocumentoFiscal: TDocumentoFiscal);
var
  lOk: Boolean;
  vBaseDeCalculo,
  vTotalICMS,
  vTotalItens,
  vTotalDescontos: Double;
  nCounter: Integer;
begin
  vBaseDeCalculo := 0;
  vTotalICMS := 0;
  vTotalItens := 0;
  vTotalDescontos := 0;

  carregaNFe('65');

  with nfe.Configuracoes.WebServices do
  begin
    UF            := DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla;
    Ambiente      := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.ambiente));
  end;

  with nfe.NotasFiscais.Add.NFe do
  begin
    Ide.natOp     := DocumentoFiscal.historico.nome;
    Ide.modelo    := StrToIntDef(DocumentoFiscal.modelo, 65);
    Ide.serie     := DocumentoFiscal.documentoFiscalNFe.serie;
    Ide.nNF       := DocumentoFiscal.documentoFiscalNFe.numero;
    Ide.cNF       := GerarCodigoDFe(Ide.nNF);
    Ide.dEmi      := Now(); //DocumentoFiscal.emissao;
    Ide.dSaiEnt   := DocumentoFiscal.saida;
    Ide.hSaiEnt   := Now(); //DocumentoFiscal.saida;
    Ide.tpNF      := tnSaida;
    Ide.tpEmis    := StrToTpEmis(lOk, IntToStr(DocumentoFiscal.documentoFiscalNFe.formaDeEmissao));
    Ide.tpAmb     := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.ambiente));
    Ide.cUF       := UFtoCUF(DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla);
    Ide.cMunFG    := StrToInt(DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].municipio.codigo);
    Ide.finNFe    := fnNormal;
    Ide.tpImp     := tiNFCe;
    Ide.indFinal  := cfConsumidorFinal;
    Ide.indPres   := pcPresencial;
    Ide.indIntermed := iiSemOperacao;

    if nfe.Configuracoes.Geral.FormaEmissao <> teNormal then
    begin
      Ide.dhCont := date;
      Ide.xJust  := 'Problemas com a internet';
    end;

    with DocumentoFiscal.estabelecimento do
    begin
      Emit.CNPJCPF            := estabelecimentoDocumentos[0].documentoNumero;
      Emit.IE                 := RemoveStrings(estabelecimentoDocumentos[0].inscricaoEstadual, ['.', '-']);
      Emit.xNome              := nome;
      Emit.xFant              := nome;

      Emit.EnderEmit.fone     := '';
      Emit.EnderEmit.CEP      := StrToInt(RemoveStrings(estabelecimentoEnderecos[0].cep, ['.', '-']));
      Emit.EnderEmit.xLgr     := estabelecimentoEnderecos[0].logradouro;
      Emit.EnderEmit.nro      := IntToStr(estabelecimentoEnderecos[0].numero);
      Emit.EnderEmit.xCpl     := estabelecimentoEnderecos[0].complemento;
      Emit.EnderEmit.xBairro  := estabelecimentoEnderecos[0].bairro;
      Emit.EnderEmit.cMun     := StrToInt(estabelecimentoEnderecos[0].municipio.codigo);
      Emit.EnderEmit.xMun     := estabelecimentoEnderecos[0].municipio.nome;
      Emit.EnderEmit.UF       := estabelecimentoEnderecos[0].uf.sigla;
      Emit.enderEmit.cPais    := 1058;
      Emit.enderEmit.xPais    := 'BRASIL';

      Emit.IEST               := estabelecimentoDocumentos[0].inscricaoEstadualSubstitutoTributario;
      Emit.CRT                := TpcnCRT(estabelecimentoDocumentos[0].regimeTributarioICMS);
    end;

    if Length(DocumentoFiscal.cpfInformado) > 0 then
      Dest.CNPJCPF            := DocumentoFiscal.cpfInformado;

    For nCounter := 0 to Length(DocumentoFiscal.documentoFiscalItens) - 1 do
    begin
      with Det.New, DocumentoFiscal.DocumentoFiscalItens[nCounter] do
      begin
        Prod.nItem := nCounter + 1;
        Prod.cProd := item.codigo;
        Prod.cEAN := '';
        Prod.xProd := item.nome;
        prod.NCM := StringReplace(ncm.codigo, '.', '', [rfReplaceAll]);
        prod.EXTIPI := '';
        Prod.CFOP := cfop.codigo;
        Prod.CEST := StringReplace(cest.codigo, '.', '', [rfReplaceAll]);
        Prod.uCom := unidade.codigo;
        Prod.qCom := quantidade;
        Prod.vUnCom := valor;
        Prod.cEANTrib := '';
        Prod.uTrib := unidade.codigo;
        Prod.qTrib := quantidade;
        Prod.vUnTrib := valor;
        Prod.vOutro := 0;
        Prod.vFrete := 0;
        Prod.vSeg := 0;
        Prod.vDesc := DocumentoFiscal.DocumentoFiscalItens[nCounter].desconto;

        vTotalItens := vTotalItens + valor;
        vTotalDescontos := vTotalDescontos + DocumentoFiscal.DocumentoFiscalItens[nCounter].desconto;

        with Imposto do
        begin
          vTotTrib := 0;

          with ICMS do
          begin
            orig := StrToOrig(lOk, origemDamercadoria.codigo);
            modBC := dbiValorOperacao;

            if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
            begin
              CST := StrToCSTICMS(lOk, cstICMS.codigo);
              vBC := valor;
              pICMS := IfThen((icmsAliquota >= 1), (icmsAliquota / 100), icmsAliquota);
            end
            else
            begin
              CSOSN := StrToCSOSNIcms(lOk, cstICMS.codigo);
              vBC := 0;
              pICMS := 0;
            end;

            vBaseDeCalculo := vBaseDeCalculo + vBC;
            vICMS := vBC * pICMS;

            vTotalICMS := vTotalICMS + vICMS;

            modBCST := dbisMargemValorAgregado;
            pMVAST  := 0;
            pRedBCST:= 0;
            vBCST   := 0;
            pICMSST := 0;
            vICMSST := 0;
            pRedBC  := 0;

            pCredSN := 0;
            vCredICMSSN := 0;
            vBCFCPST := 0;
            pFCPST := 0;
            vFCPST := 0;
            vBCSTRet := 0;
            pST := 0;
            vICMSSubstituto := 0;
            vICMSSTRet := 0;

            vBCFCPSTRet := 0;
            pFCPSTRet := 0;
            vFCPSTRet := 0;
            pRedBCEfet := 0;
            vBCEfet := 0;
            pICMSEfet := 0;
            vICMSEfet := 0;

            vICMSSTDeson := 0;
            motDesICMSST := mdiOutros;

            pFCPDif := 0;
            vFCPDif := 0;
            vFCPEfet := 0;

            with ICMSUFDest do
            begin
              vBCUFDest      := 0.00;
              pFCPUFDest     := 0.00;
              pICMSUFDest    := 0.00;
              pICMSInter     := 0.00;
              pICMSInterPart := 0.00;
              vFCPUFDest     := 0.00;
              vICMSUFDest    := 0.00;
              vICMSUFRemet   := 0.00;
            end;
          end;

          with PIS do
          begin
            CST := StrToCSTPIS(lOk, cstPIS.codigo);
            vBC := valor;
            pPIS := pisAliquota;
            vPIS := vBC * pPIS;

            qBCProd := 0;
            vAliqProd := 0;
          end;

          with PISST do
          begin
            vBc       := 0;
            pPis      := 0;
            qBCProd   := 0;
            vAliqProd := 0;
            vPIS      := 0;
            IndSomaPISST :=  ispNenhum;
          end;

          with COFINS do
          begin
            CST := StrToCSTCOFINS(lOk, CSTCOFINS.codigo);
            vBC := valor;
            pCOFINS := cofinsAliquota;
            vCOFINS := vBC * pCOFINS;

            qBCProd := 0;
            vAliqProd := 0;
          end;

          with COFINSST do
          begin
            vBC       := 0;
            pCOFINS   := 0;
            qBCProd   := 0;
            vAliqProd := 0;
            vCOFINS   := 0;
            indSomaCOFINSST :=  iscNenhum;
          end;
        end;

        infAdProd := '';
      end;
    end;

    Total.ICMSTot.vBC     := vBaseDeCalculo;
    Total.ICMSTot.vICMS   := vTotalICMS;
    Total.ICMSTot.vBCST   := 0;
    Total.ICMSTot.vST     := 0;
    Total.ICMSTot.vProd   := vTotalItens;
    Total.ICMSTot.vFrete  := 0;
    Total.ICMSTot.vSeg    := 0;
    Total.ICMSTot.vDesc   := vTotalDescontos;
    Total.ICMSTot.vII     := 0;
    Total.ICMSTot.vIPI    := 0;
    Total.ICMSTot.vPIS    := 0;
    Total.ICMSTot.vCOFINS := 0;
    Total.ICMSTot.vOutro  := 0;
    Total.ICMSTot.vNF     := vTotalItens - vTotalDescontos;

    Total.ICMSTot.vFCPUFDest   := 0.00;
    Total.ICMSTot.vICMSUFDest  := 0.00;
    Total.ICMSTot.vICMSUFRemet := 0.00;

    Total.ISSQNtot.vServ   := 0;
    Total.ISSQNTot.vBC     := 0;
    Total.ISSQNTot.vISS    := 0;
    Total.ISSQNTot.vPIS    := 0;
    Total.ISSQNTot.vCOFINS := 0;

    Total.retTrib.vRetPIS    := 0;
    Total.retTrib.vRetCOFINS := 0;
    Total.retTrib.vRetCSLL   := 0;
    Total.retTrib.vBCIRRF    := 0;
    Total.retTrib.vIRRF      := 0;
    Total.retTrib.vBCRetPrev := 0;
    Total.retTrib.vRetPrev   := 0;

    Transp.modFrete := mfSemFrete;

    for nCounter := 0 to Length(DocumentoFiscal.DocumentoFiscalPagamentos) - 1 do
    begin
      with pag.New do
      begin
        tPag := StrToFormaPagamento(lOk, DocumentoFiscal.documentoFiscalPagamentos[nCounter].formaIndicador);
        vPag := DocumentoFiscal.DocumentoFiscalPagamentos[nCounter].valor;
      end;
    end;

    InfAdic.infCpl     :=  '';
    InfAdic.infAdFisco :=  '';

    with InfAdic.obsCont.New do
    begin
      xCampo := 'ObsCont';
      xTexto := 'Texto';
    end;

    with InfAdic.obsFisco.New do
    begin
      xCampo := 'ObsFisco';
      xTexto := 'Texto';
    end;
  end;

  nfe.NotasFiscais.GerarNFe;
end;

end.
