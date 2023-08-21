unit Api.Componentes;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, IniFiles, ACBrBase, ACBrSAT,
  ACBrDFeSSL, ACBrSATClass, pcnConversao, pcnConversaoNFe, Model.DocumentoFiscal,
  pcnCFe, ACBrDFe, ACBrNFe, ACBrMail, ACBrUtil, ACBrDFeUtil, ACBrNFeNotasFiscais,
  pcnNFe, Api.Funcoes, System.Math, Model.Config;

const
  docModelos: TArray<String> = ['55', '56', '57', '58', '59', '65'];

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
    Fconfig: TConfig;

    property satCodigoDeAtivacao: AnsiString read FsatCodigoDeAtivacao write FsatCodigoDeAtivacao;
    property satAssinaturaAC: AnsiString read FsatAssinaturaAC write FsatAssinaturaAC;

    function inicializaSAT: boolean;

    procedure carregaSAT;
    procedure carregaNFe(const modelo: string = '55');

    function GerarCFe(const DocumentoFiscal: TDocumentoFiscal): string;
    procedure GerarNFe(const DocumentoFiscal: TDocumentoFiscal);
    procedure GerarNFCe(const DocumentoFiscal: TDocumentoFiscal);

    function CancelarNFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
    function CancelarCFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
    function CancelarNFCe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
  public
    function EmiteDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
    function CancelarDFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;

  end;

var
  components: Tcomponents;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ Tcomponents }

procedure Tcomponents.carregaSAT;
begin
  try
    InfoConfig(FConfig);

    with sat do
    begin
      SSL.SSLCryptLib                     := cryOpenSSL;
      SSL.SSLXmlSignLib                   := xsLibXml2;

      CFe.IdentarXML                      := True;
      CFe.TamanhoIdentacao                := 3;
      CFe.RetirarAcentos                  := True;

      Config.ArqSchema                    := FConfig.cfe.schemas;
      Config.PaginaDeCodigo               := FConfig.cfe.paginadecodigo;
      Config.EhUTF8                       := FConfig.cfe.utf;
      Config.infCFe_versaoDadosEnt        := FConfig.cfe.versaolayout;

      Modelo                              := TACBrSATModelo(FConfig.cfe.modelo) ;
      ArqLOG                              := FConfig.cfe.arquivolog;
      NomeDLL                             := FConfig.cfe.caminhodll;

      Config.ide_numeroCaixa              := FConfig.cfe.caixa;
      Config.ide_tpAmb                    := TpcnTipoAmbiente(FConfig.cfe.ambiente);
      Config.ide_CNPJ                     := FConfig.cfe.swhouse.cnpj;

      Config.emit_CNPJ                    := FConfig.emitente.cnpj;
      Config.emit_IE                      := FConfig.emitente.ie;
      Config.emit_IM                      := FConfig.emitente.im;
      Config.emit_cRegTrib                := TpcnRegTrib(FConfig.emitente.regimeicms);
      Config.emit_cRegTribISSQN           := TpcnRegTribISSQN(FConfig.emitente.regimeiss);
      Config.emit_indRatISSQN             := TpcnindRatISSQN(FConfig.emitente.indicadorderateio);

      ConfigArquivos.PastaCFeVenda        := FConfig.cfe.arquivos.pathvenda;
      ConfigArquivos.PastaEnvio           := FConfig.cfe.arquivos.pathenvio;
      ConfigArquivos.PastaCFeCancelamento := FConfig.cfe.arquivos.pathcancelamento;
      ConfigArquivos.SalvarCFe            := FConfig.cfe.arquivos.salvarcfe;
      ConfigArquivos.SalvarCFeCanc        := FConfig.cfe.arquivos.salvarcancelamento;
      ConfigArquivos.SalvarEnvio          := FConfig.cfe.arquivos.salvarenvio;
      ConfigArquivos.SepararPorCNPJ       := FConfig.cfe.arquivos.separarporcnpj;
      ConfigArquivos.SepararPorModelo     := FConfig.cfe.arquivos.separarpormodelo;
      ConfigArquivos.SepararPorDia        := FConfig.cfe.arquivos.separarpordia;
      ConfigArquivos.SepararPorMes        := FConfig.cfe.arquivos.separarpormes;
      ConfigArquivos.SepararPorAno        := FConfig.cfe.arquivos.separarporano;

      FsatCodigoDeAtivacao                := FConfig.cfe.codigodeativacao;
      FsatAssinaturaAC                    := FConfig.cfe.swhouse.assinatura;

      CFe.IdentarXML                      := True;
      CFe.TamanhoIdentacao                := 3;
      CFe.RetirarAcentos                  := True;
    end;

  except
    on E:Exception do
    begin
      Write(Format('%s - Impossível carregar o SAT. Verificar com suporte, o erro %s.',
                                                                                      [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                      E.Message]));
    end;
  end;
end;

procedure Tcomponents.DataModuleCreate(Sender: TObject);
begin
  RemoveDataModule(self);
end;

function Tcomponents.EmiteDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
var
  xmlDocumento: string;
begin
  try
    case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
      0:
        begin
          GerarNFe(DocumentoFiscal);

          if nfe.NotasFiscais.Count > 0 then
            nfe.Enviar(DocumentoFiscal.documentoFiscalNFe.numero, False, True);

          DocumentoFiscal.documentoFiscalNFe.status := nfe.WebServices.Enviar.cStat;
          DocumentoFiscal.documentoFiscalNFe.msgRetorno := nfe.WebServices.Enviar.Msg;

          if (nfe.WebServices.Enviar.cStat = 100) and not(nfe.WebServices.Enviar.Protocolo = EmptyStr) then
          begin
            DocumentoFiscal.documentoFiscalNFe.chave := nfe.NotasFiscais.Items[0].NumID;
            DocumentoFiscal.documentoFiscalNFe.xml := nfe.NotasFiscais.Items[0].XMLAssinado;
            DocumentoFiscal.documentoFiscalNFe.protocolo := nfe.WebServices.Enviar.Protocolo;

            WriteLn(Format('%s - Emitida a NFe de número: %d com chave: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                nfe.NotasFiscais.Items[0].NFe.Ide.nNF,
                                                                                nfe.NotasFiscais.Items[0].NumID]));
            Msg := nfe.NotasFiscais.Items[0].Msg;
          end
          else
          begin
            WriteLn(Format('%s - Não foi possível emitir a NFe, o seguinte erro ocorreu: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                nfe.WebServices.Retorno.Msg]));
            Error := nfe.WebServices.Retorno.Msg;
          end;

          Result := DocumentoFiscal;
        end;
      4:
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

            WriteLn(Format('%s - Emitido o cupom fiscal: %d com chave: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                              sat.CFe.ide.nCFe, sat.CFe.infCFe.ID]));
            Msg := sat.Resposta.mensagemRetorno;


          end
          else
          begin
            WriteLn(Format('%s - Não foi possivel emitir o cupom fiscal, o seguinte erro ocorreu: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                         sat.Resposta.mensagemRetorno]));
            Error := sat.Resposta.mensagemRetorno;
          end;

          Result := DocumentoFiscal;
        end;
      5:
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

            WriteLn(Format('%s - Emitida a NFCe de número: %d com chave: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                nfe.NotasFiscais.Items[0].NFe.Ide.nNF,
                                                                                nfe.NotasFiscais.Items[0].NumID]));
            Msg := nfe.WebServices.Retorno.Msg;
          end
          else
          begin
            WriteLn(Format('%s - Não foi possível emitir a NFCe, o seguinte erro ocorreu: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                 nfe.WebServices.Retorno.Msg]));
            Error := nfe.WebServices.Retorno.Msg;
          end;

          Result := DocumentoFiscal;
        end;
    end;

  except
    on E: Exception do
    begin
      WriteLn(Format('%s - Houve um erro na tentativa de enviar o documento. Verifique a mensagem a seguir: %s.',
                                                                                                                [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                                E.Message]));
      Error := E.Message;
      Result := nil;
    end;
  end;
end;

function Tcomponents.CancelarDFe(
  DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
begin
  case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
    0: Result := CancelarNFe(DocumentoFiscal);
    4: Result := CancelarCFe(DocumentoFiscal);
    5: Result := CancelarNFCe(DocumentoFiscal);
  end;
end;

function Tcomponents.CancelarNFCe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
var
  xmlOriginal: String;
begin
  xmlOriginal := DocumentoFiscal.documentoFiscalNFe.xml;

  carregaNFe('65');
  try
    nfe.Consultar(DocumentoFiscal.documentoFiscalNFe.chave, True);

    if not(nfe.WebServices.Consulta.cStat = 101) then
    begin
      nfe.NotasFiscais.Clear;
      nfe.NotasFiscais.LoadFromString(xmlOriginal);

      nfe.EventoNFe.Evento.Clear;
      nfe.EventoNFe.idLote := DocumentoFiscal.documentoFiscalNFe.numero;

      with nfe.EventoNFe.Evento.New do
      begin
        infEvento.dhEvento := now;
        infEvento.tpEvento := teCancelamento;
        infEvento.detEvento.xJust := DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa;
      end;

      nfe.EnviarEvento(DocumentoFiscal.documentoFiscalNFe.numero);

      if nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat = 135 then
      begin
        DocumentoFiscal.documentoFiscalNFe.status := nfe.WebServices.EnvEvento.cStat;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoProtocolo := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoData := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;

        WriteLn(Format('%s - Cupom fiscal: %d com chave: %s, cancelado com sucesso.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                       DocumentoFiscal.documentoFiscalNFe.numero,
                                                                                       DocumentoFiscal.documentoFiscalNFe.chave]));
       end
      else
      begin
        DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
        WriteLn(Format('%s - Não foi possivel cancelar o cupom fiscal, o seguinte erro ocorreu: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                       nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo]));
      end;
    end
    else
    begin
      if nfe.WebServices.Consulta.retCancNFe.cStat = 101 then
      begin
        DocumentoFiscal.documentoFiscalNFe.status := nfe.WebServices.Consulta.retCancNFe.cStat;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoProtocolo := nfe.WebServices.Consulta.retCancNFe.nProt;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoData := nfe.WebServices.Consulta.retCancNFe.dhRecbto;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa := nfe.WebServices.Consulta.retCancNFe.xMotivo;

        WriteLn(Format('%s - Cupom fiscal: %d com chave: %s, já cancelado anteriormente.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                            DocumentoFiscal.documentoFiscalNFe.numero,
                                                                                            DocumentoFiscal.documentoFiscalNFe.chave]));
      end;
    end;

    Result := DocumentoFiscal;
  except
    on E: Exception do
    begin
      WriteLn(Format('%s - Houve um erro na tentativa de inicializar o equipamento MFe. Verifique a mensagem a seguir: %s.',
                                                                                                                            [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                                            E.Message]));
      Result := nil;
    end;
  end;
end;

function Tcomponents.CancelarNFe(
  DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
var
  xmlOriginal: String;
begin
  xmlOriginal := DocumentoFiscal.documentoFiscalNFe.xml;

  carregaNFe;
  try
    nfe.Consultar(DocumentoFiscal.documentoFiscalNFe.chave, True);

    if not(nfe.WebServices.Consulta.cStat = 101) then
    begin
      nfe.NotasFiscais.Clear;
      nfe.NotasFiscais.LoadFromString(xmlOriginal);

      nfe.EventoNFe.Evento.Clear;
      nfe.EventoNFe.idLote := DocumentoFiscal.documentoFiscalNFe.numero;

      with nfe.EventoNFe.Evento.New do
      begin
        infEvento.dhEvento := now;
        infEvento.tpEvento := teCancelamento;
        infEvento.detEvento.xJust := DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa;
      end;

      nfe.EnviarEvento(DocumentoFiscal.documentoFiscalNFe.numero);

      if nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat = 135 then
      begin
        DocumentoFiscal.documentoFiscalNFe.status := nfe.WebServices.EnvEvento.cStat;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoProtocolo := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoData := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;

        WriteLn(Format('%s - Nota fiscal: %d com chave: %s, cancelada com sucesso.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                      DocumentoFiscal.documentoFiscalNFe.numero,
                                                                                      DocumentoFiscal.documentoFiscalNFe.chave]));
       end
      else
      begin
        DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
        WriteLn(Format('%s - Não foi possivel cancelar a nota fiscal, o seguinte erro ocorreu: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                      nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo]));
      end;
    end
    else
    begin
      if nfe.WebServices.Consulta.retCancNFe.cStat = 101 then
      begin
        DocumentoFiscal.documentoFiscalNFe.status := nfe.WebServices.Consulta.retCancNFe.cStat;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoProtocolo := nfe.WebServices.Consulta.retCancNFe.nProt;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoData := nfe.WebServices.Consulta.retCancNFe.dhRecbto;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa := nfe.WebServices.Consulta.retCancNFe.xMotivo;

        WriteLn(Format('%s - Nota fiscal: %d com chave: %s, já cancelada anteriormente.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                           DocumentoFiscal.documentoFiscalNFe.numero,
                                                                                           DocumentoFiscal.documentoFiscalNFe.chave]));
      end;
    end;

    Result := DocumentoFiscal;
  except
    on E: Exception do
    begin
      WriteLn(Format('%s - Houve um erro na tentativa de cancelar o documento. Verifique a mensagem a seguir: %s.',
                                                                                                                  [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                                  E.Message]));
      Result := nil;
    end;
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

      WriteLn(Format('%s - Cupom fiscal: %d com chave: %s, cancelado com sucesso.',
                                                                                  [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                  sat.CFe.ide.nCFe,
                                                                                  sat.CFe.infCFe.ID]));

      Result := DocumentoFiscal;

    end
    else
      WriteLn(Format('%s - Não foi possivel cancelar o cupom fiscal, o seguinte erro ocorreu: %s.',
                                                                                                  [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                  sat.Resposta.mensagemRetorno]));

  except
    on E: Exception do
    begin
      WriteLn(Format('%s - Houve um erro na tentativa de inicializar o equipamento MFe. Verifique a mensagem a seguir: %s.',
                                                                                                                            [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now),
                                                                                                                            E.Message]));
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
    on E: Exception do
    begin
       Write(Format('%s - Erro ao carregar equipamento MFe/SAT. Mensagem de erro: %s.', [FormatDateTime('DD/MM/YYYY hh:mm:ss', Now), E.Message]));
       Result := False;
    end;
  end;

end;

procedure Tcomponents.carregaNFe(const modelo: string = '55');
var
  lOk: Boolean;
begin
  InfoConfig(FConfig);

  nfe.SSL.DescarregarCertificado;
  with nfe.Configuracoes.Geral do
  begin
    SSLLib                  := TSSLLib(FConfig.nfe.ssl.lib);
    SSLCryptLib             := TSSLCryptLib(FConfig.nfe.ssl.cryptlib);
    SSLHttpLib              := TSSLHttpLib(FConfig.nfe.ssl.httplib);
    SSLXmlSignLib           := TSSLXmlSignLib(FConfig.nfe.ssl.xmlsignlib);

    Salvar                  := FConfig.nfe.geral.salvar;
    RetirarAcentos          := FConfig.nfe.geral.retiraracentos;
    AtualizarXMLCancelado   := FConfig.nfe.geral.atualizarxml;
    ExibirErroSchema        := FConfig.nfe.geral.exibirerroschema;
    FormaEmissao            := TpcnTipoEmissao(FConfig.nfe.geral.formaemissao);
    VersaoDF                := StrToVersaoDF(lOk, FConfig.nfe.geral.versaodf);

    ModeloDF        := StrToModeloDF(lOk, modelo);
    FormatoAlerta   := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.';

    IdCSC                 := FConfig.nfse.IdCSC;
    CSC                   := FConfig.nfse.CSC;
    VersaoQRCode          := veqr200;

  end;

  with nfe.Configuracoes.Arquivos do
  begin
    Salvar                  := FConfig.nfe.arquivos.salvar;
    SepararPorMes           := FConfig.nfe.arquivos.separarpormes;
    AdicionarLiteral        := FConfig.nfe.arquivos.adicionarliteral;
    EmissaoPathNFe          := FConfig.nfe.arquivos.emissaopathnfe;
    SalvarEvento            := FConfig.nfe.arquivos.salvarevento;
    SepararPorCNPJ          := FConfig.nfe.arquivos.separarporcnpj;
    SepararPorModelo        := FConfig.nfe.arquivos.separarpormodelo;
    PathSchemas             := FConfig.nfe.arquivos.pathschemas;
    PathNFe                 := FConfig.nfe.arquivos.pathnfe;
    PathInu                 := FConfig.nfe.arquivos.pathinu;
    PathEvento              := FConfig.nfe.arquivos.pathevento;
    PathSalvar              := FConfig.nfe.arquivos.pathsalvar;
  end;

  with nfe.Configuracoes.Certificados do
  begin
    URLPFX                  := FConfig.emitente.certificado.url;
    ArquivoPFX              := FConfig.emitente.certificado.caminhopfx;
    Senha                   := FConfig.emitente.certificado.senhadocertificado;
    NumeroSerie             := FConfig.emitente.certificado.numerodeserie;
  end;

  with nfe.Configuracoes.WebServices do
  begin
    Visualizar              := FConfig.nfe.webservice.visualizar;
    Salvar                  := FConfig.nfe.webservice.salvar;
    AjustaAguardaConsultaRet:= FConfig.nfe.webservice.ajustaaguardaconsultaret;
    AguardarConsultaRet     := FConfig.nfe.webservice.aguardarconsultaret;
    Tentativas              := FConfig.nfe.webservice.tentativas;
    IntervaloTentativas     := FConfig.nfe.webservice.intervalotentativas;
    TimeOut                 := FConfig.nfe.webservice.timeout;
    ProxyHost               := FConfig.nfe.webservice.proxyhost;
    ProxyPort               := FConfig.nfe.webservice.proxyport;
    ProxyUser               := FConfig.nfe.webservice.proxyuser;
    ProxyPass               := FConfig.nfe.webservice.proxypass;
  end;

  with mail do
  begin
    Host                    := FConfig.emitente.email.servidor;
    Port                    := FConfig.emitente.email.porta;
    Username                := FConfig.emitente.email.usuario;
    Password                := decrypt(FConfig.emitente.email.senha);
    From                    := FConfig.emitente.email.origem;
    SetSSL                  := FConfig.emitente.email.usassl;
    SetTLS                  := FConfig.emitente.email.usatls;
    ReadingConfirmation     := FConfig.emitente.email.confirmaleitura;
    UseThread               := FConfig.emitente.email.usathread;
    FromName                := FConfig.emitente.email.remetente;
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

procedure Tcomponents.GerarNFe(const DocumentoFiscal: TDocumentoFiscal);
var
  nCont: Integer;
  lOk: Boolean;
  NotaF: NotaFiscal;
  vBaseDeCalculo,
  vTotalICMS,
  vTotalItens,
  vTotalDescontos: Double;
  Count: TNFe;
begin
  vBaseDeCalculo := 0;
  vTotalICMS := 0;
  vTotalItens := 0;
  vTotalDescontos := 0;

  carregaNFe;

  nfe.NotasFiscais.Clear;

  nfe.Configuracoes.WebServices.Ambiente := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.ambiente));

  NotaF := nfe.NotasFiscais.Add;
  with NotaF.NFe, DocumentoFiscal do
  begin
    Ide.natOp   := historico.nome;
    Ide.modelo  := 55;
    Ide.serie   := documentoFiscalNFe.serie;
    Ide.nNF     := documentoFiscalNFe.numero;
    Ide.cNF     := GerarCodigoDFe(Ide.nNF);
    Ide.dEmi    := emissao;
    Ide.dSaiEnt := saida;
    Ide.hSaiEnt := saida;
    Ide.tpNF    := tnSaida;  // Ver depois de onde pegar essa informação.
    Ide.tpEmis    := StrToTpEmis(lOk, IntToStr(DocumentoFiscal.documentoFiscalNFe.formaDeEmissao));
    Ide.tpAmb     := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.ambiente));
    Ide.verProc := '2023.05'; // Ver depois de onde pegar essa informação.
    Ide.cUF     := UFtoCUF(estabelecimento.estabelecimentoEnderecos[0].uf.sigla);
    Ide.cMunFG  := StrToInt(estabelecimento.estabelecimentoEnderecos[0].municipio.codigo);
    Ide.finNFe  := StrToFinNFe(lOk, IntToStr(documentoFiscalNFe.finalidadeEmissao));
    Ide.indIntermed := TindIntermed(documentoFiscalNFe.indicadorIntermediador);
    Ide.indFinal    := cfNao;
    if DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla =
       DocumentoFiscal.parceiro.parceiroEnderecos[0].uf.sigla then
      Ide.idDest  := doInterna
    else if DocumentoFiscal.parceiro.parceiroEnderecos[0].uf.sigla = 'EX' then
      Ide.idDest  := doExterior
    else
      Ide.idDest  := doInterestadual;


    if documentoFiscalNFe.tipoImpressao = 'R' then
      Ide.tpImp := tiRetrato
    else
      Ide.tpImp := tiPaisagem;

    if not(TpcnTipoEmissao(documentoFiscalNFe.formaDeEmissao) = teNormal) then
    begin
      Ide.dhCont:= date;
      Ide.xJust := 'Falha no webservice normal'; // Ver depois de onde pegar essa informação.
    end;

    Emit.CNPJCPF            := estabelecimento.estabelecimentoDocumentos[0].documentoNumero;
    Emit.IE                 := estabelecimento.estabelecimentoDocumentos[0].inscricaoEstadual;
    Emit.xNome              := estabelecimento.nome;
    Emit.xFant              := estabelecimento.nome;

    Emit.EnderEmit.fone     := '';
    Emit.EnderEmit.CEP      := StrToInt(RemoveStrings(estabelecimento.estabelecimentoEnderecos[0].cep, ['.', '-']));
    Emit.EnderEmit.xLgr     := estabelecimento.estabelecimentoEnderecos[0].logradouro;
    Emit.EnderEmit.nro      := IntToStr(estabelecimento.estabelecimentoEnderecos[0].numero);
    Emit.EnderEmit.xCpl     := estabelecimento.estabelecimentoEnderecos[0].complemento;
    Emit.EnderEmit.xBairro  := estabelecimento.estabelecimentoEnderecos[0].bairro;
    Emit.EnderEmit.cMun     := StrToInt(estabelecimento.estabelecimentoEnderecos[0].municipio.codigo);
    Emit.EnderEmit.xMun     := estabelecimento.estabelecimentoEnderecos[0].municipio.nome;
    Emit.EnderEmit.UF       := estabelecimento.estabelecimentoEnderecos[0].uf.sigla;
    Emit.enderEmit.cPais    := 1058;
    Emit.enderEmit.xPais    := 'BRASIL';

    Emit.IEST               := estabelecimento.estabelecimentoDocumentos[0].inscricaoEstadualSubstitutoTributario;
    Emit.IM                 := estabelecimento.estabelecimentoDocumentos[0].inscricaoMunicipal;
    Emit.CRT                := TpcnCRT(estabelecimento.estabelecimentoDocumentos[0].regimeTributarioICMS);

    infRespTec.CNPJ	        := '04528001000164';
    infRespTec.xContato	    := 'Myron Yerich P. Sales';
    infRespTec.email        := 'myron@solucaosistemas.net';
    infRespTec.fone	        := '8533076262';

    if length(parceiro.parceiroDocumentos) > 0 then
    begin
      Dest.CNPJCPF            := parceiro.parceiroDocumentos[0].documentoNumero;

      if (parceiro.parceiroDocumentos[0].documentoTipo in [1, 2]) and (UpperCase(parceiro.parceiroDocumentos[0].inscricaoEstadual) = 'ISENTO') then
        Dest.indIEDest        := inIsento
      else if ((parceiro.parceiroDocumentos[0].documentoTipo in [1, 2])) and (parceiro.parceiroDocumentos[0].inscricaoEstadual > '') then
        Dest.indIEDest        := inContribuinte
      else
      begin
        Ide.indFinal          := cfConsumidorFinal;
        Dest.indIEDest        := inNaoContribuinte;
      end;
    end;

    Dest.IE	                := parceiro.parceiroDocumentos[0].inscricaoEstadual;
    Dest.ISUF               := '';
    Dest.xNome              := parceiro.nome;

    Dest.EnderDest.Fone     := '';
    Dest.EnderDest.CEP      := StrToInt(RemoveStrings(parceiro.parceiroEnderecos[0].cep, ['.', '-']));
    Dest.EnderDest.xLgr     := parceiro.parceiroEnderecos[0].logradouro;
    Dest.EnderDest.nro      := IntToStr(parceiro.parceiroEnderecos[0].numero);
    Dest.EnderDest.xCpl     := parceiro.parceiroEnderecos[0].complemento;
    Dest.EnderDest.xBairro  := parceiro.parceiroEnderecos[0].bairro;
    Dest.EnderDest.cMun     := StrToInt(parceiro.parceiroEnderecos[0].municipio.codigo);
    Dest.EnderDest.xMun     := parceiro.parceiroEnderecos[0].municipio.nome;
    Dest.EnderDest.UF       := parceiro.parceiroEnderecos[0].uf.sigla;
    Dest.EnderDest.cPais    := 1058;
    Dest.EnderDest.xPais    := 'BRASIL';

    nCont := 0;
    for nCont := 0 to Length(DocumentoFiscalItens) - 1 do
    begin
      with Det.New, DocumentoFiscalItens[nCont] do
      begin
        Prod.nItem := nCont + 1;
        Prod.cProd := item.codigo;
        Prod.cEAN := '';
        Prod.xProd := item.nome;
        Prod.NCM := StringReplace(ncm.codigo, '.', '', [rfReplaceAll]);
        Prod.EXTIPI := '';
        if DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla =
           DocumentoFiscal.parceiro.parceiroEnderecos[0].uf.sigla then
          Prod.CFOP := cfop.codigo
        else
          Prod.CFOP := IntToStr(1000 + StrToInt(cfop.codigo));
        Prod.uCom := unidade.codigo;
        Prod.qCom := quantidade;
        Prod.vUnCom := valor;
        Prod.vDesc := DocumentoFiscalItens[nCont].desconto;

        Prod.cEANTrib  := '';
        Prod.uTrib     := unidade.codigo;
        Prod.qTrib     := quantidade;
        Prod.vUnTrib   := valor;

        Prod.vOutro    := DocumentoFiscalItens[nCont].outrasDespesas;
        Prod.vFrete    := DocumentoFiscalItens[nCont].frete;
        Prod.vSeg      := 0;
        infAdProd := 'Informacao Adicional do Produto';

        Prod.vProd := RoundABNT((Prod.qCom * Prod.vUnCom) + Prod.vOutro, -2);

        vTotalItens := vTotalItens + subtotal;
        vTotalDescontos := vTotalDescontos + DocumentoFiscal.DocumentoFiscalItens[nCont].desconto;

        with Imposto do
        begin
          vTotTrib := 0;

          with ICMS do
          begin
            orig := StrToOrig(lOk, origemDamercadoria.codigo);
            if Emit.CRT = crtSimplesNacional then
            begin
              CSOSN := StrToCSOSNIcms(lOk, cstICMS.codigo);
              pCredSN := simplesAliquotaDeCredito;
              vCredICMSSN := subtotal * (pCredSN / 100);
              modBC := dbiPrecoTabelado;
              modBCST := dbisListaNeutra;
            end
            else
            begin
              CST := StrToCSTICMS(lOk, cstICMS.codigo);
              modBC := dbiValorOperacao;
              modBCST := dbisMargemValorAgregado;
              pRedBC := 0;
              pMVAST := 0;
              pRedBCST := 0;
              vBCST := icmsSTBC;
              pICMSST := icmsstAliquota;
              vICMSST := icmsSTValor;
            end;

            vBC := icmsBC;
            pICMS := IfThen((icmsAliquota >= 1), (icmsAliquota / 100), icmsAliquota);
            vICMS := vBC * pICMS;

            vBCFCPST := icmsSTBC;
            pFCPST := 2;
            vFCPST := 2;
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

            vBaseDeCalculo := vBaseDeCalculo + icmsBC;
            vTotalICMS := vTotalICMS + vICMS;
          end;

          with ICMSUFDest do
          begin
            vBCUFDest      := fcpBCUFDestinatario;
            pFCPUFDest     := fcpPercentualUFDestino;
            pICMSUFDest    := 0.00;
            pICMSInter     := 0.00;
            pICMSInterPart := 0.00;
            vFCPUFDest     := 0.00;
            vICMSUFDest    := 0.00;
            vICMSUFRemet   := 0.00;
          end;

          with IPI do
          begin
            CST := StrToCSTIPI(lOk, cstIPI.codigo);
            clEnq := ipiCodigoEnquadramento;
            CNPJProd := ipiCNPJProdutor;
            cSelo    := ipiSeloDeControle;
            qSelo    := ipiQuantidadeDoSelo;
            cEnq     := '';

            vBC    := subtotal;
            qUnid  := 0;
            vUnid  := 0;
            pIPI   := ipiAliquota;
            vIPI   := subtotal * (ipiAliquota / 100);
          end;

          with PIS do
          begin
            CST  := pis99;
            vBC  := 0;
            pPIS := 0;
            vPIS := 0;

            qBCProd   := 0;
            vAliqProd := 0;
            vPIS      := 0;
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
            CST     := cof99;
            vBC     := 0;
            pCOFINS := 0;
            vCOFINS := 0;
            qBCProd   := 0;
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
      end;
    end;

    if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
    begin
      NotaF.NFe.Total.ICMSTot.vBC := vBaseDeCalculo;
      NotaF.NFe.Total.ICMSTot.vICMS := vTotalICMS;
    end
    else
    begin
      NotaF.NFe.Total.ICMSTot.vBC := 0;
      NotaF.NFe.Total.ICMSTot.vICMS := 0;
    end;

    NotaF.NFe.Total.ICMSTot.vBCST   := 0;
    NotaF.NFe.Total.ICMSTot.vST     := 0;
    NotaF.NFe.Total.ICMSTot.vProd   := vTotalItens;
    NotaF.NFe.Total.ICMSTot.vFrete  := 0;
    NotaF.NFe.Total.ICMSTot.vSeg    := 0;
    NotaF.NFe.Total.ICMSTot.vDesc   := vTotalDescontos + DocumentoFiscal.desconto;
    NotaF.NFe.Total.ICMSTot.vII     := 0;
    NotaF.NFe.Total.ICMSTot.vIPI    := 0;
    NotaF.NFe.Total.ICMSTot.vPIS    := 0;
    NotaF.NFe.Total.ICMSTot.vCOFINS := 0;
    NotaF.NFe.Total.ICMSTot.vOutro  := 0;
    NotaF.NFe.Total.ICMSTot.vNF     := vTotalItens - DocumentoFiscal.desconto - vTotalDescontos;

    // lei da transparencia de impostos
    NotaF.NFe.Total.ICMSTot.vTotTrib := 0;

    // partilha do icms e fundo de probreza
    NotaF.NFe.Total.ICMSTot.vFCPUFDest   := 0.00;
    NotaF.NFe.Total.ICMSTot.vICMSUFDest  := 0.00;
    NotaF.NFe.Total.ICMSTot.vICMSUFRemet := 0.00;

    NotaF.NFe.Total.ICMSTot.vFCPST     := 0;
    NotaF.NFe.Total.ICMSTot.vFCPSTRet  := 0;

    NotaF.NFe.Total.retTrib.vRetPIS    := 0;
    NotaF.NFe.Total.retTrib.vRetCOFINS := 0;
    NotaF.NFe.Total.retTrib.vRetCSLL   := 0;
    NotaF.NFe.Total.retTrib.vBCIRRF    := 0;
    NotaF.NFe.Total.retTrib.vIRRF      := 0;
    NotaF.NFe.Total.retTrib.vBCRetPrev := 0;
    NotaF.NFe.Total.retTrib.vRetPrev   := 0;

    NotaF.NFe.Transp.modFrete := mfContaEmitente;
    NotaF.NFe.Transp.Transporta.CNPJCPF  := '';
    NotaF.NFe.Transp.Transporta.xNome    := '';
    NotaF.NFe.Transp.Transporta.IE       := '';
    NotaF.NFe.Transp.Transporta.xEnder   := '';
    NotaF.NFe.Transp.Transporta.xMun     := '';
    NotaF.NFe.Transp.Transporta.UF       := '';

    NotaF.NFe.Transp.retTransp.vServ    := 0;
    NotaF.NFe.Transp.retTransp.vBCRet   := 0;
    NotaF.NFe.Transp.retTransp.pICMSRet := 0;
    NotaF.NFe.Transp.retTransp.vICMSRet := 0;
    NotaF.NFe.Transp.retTransp.CFOP     := '';
    NotaF.NFe.Transp.retTransp.cMunFG   := 0;
{
    with NotaF.NFe.Transp.Vol.New do
    begin
      qVol  := 1;
      esp   := 'Especie';
      marca := 'Marca';
      nVol  := 'Numero';
      pesoL := 100;
      pesoB := 110;
    end;
}
    NotaF.NFe.Cobr.Fat.nFat  := IntToStr(documentoFiscal.DocumentoFiscalNFe.numero);
    NotaF.NFe.Cobr.Fat.vOrig := documentoFiscal.subtotal;
    NotaF.NFe.Cobr.Fat.vDesc := documentoFiscal.Desconto;
    NotaF.NFe.Cobr.Fat.vLiq  := documentoFiscal.subtotal - documentoFiscal.Desconto;


    for nCont := 0 to Length(DocumentoFiscal.DocumentoFiscalCobrancas) - 1 do
    begin
      with NotaF.NFe.Cobr.Dup.New do
      begin
        nDup := PadLeft(IntToStr(DocumentoFiscal.documentoFiscalCobrancas[nCont].duplicata), 3, '0');
        dVenc := DocumentoFiscal.documentoFiscalCobrancas[nCont].vencimento;
        vDup := DocumentoFiscal.documentoFiscalCobrancas[nCont].valor;
      end;
    end;

    NotaF.NFe.InfAdic.infCpl     :=  '';
    NotaF.NFe.InfAdic.infAdFisco :=  '';

    NotaF.NFe.exporta.UFembarq   := '';;
    NotaF.NFe.exporta.xLocEmbarq := '';

    NotaF.NFe.compra.xNEmp := '';
    NotaF.NFe.compra.xPed  := '';
    NotaF.NFe.compra.xCont := '';

    const indicador = ['01', '02', '03', '04', '05', '10', '11', '12', '13', '15', '16', '17', '18', '19', '90', '99'];

    for nCont := 0 to Length(documentoFiscalPagamentos) - 1 do
    begin
      with NotaF.NFe.pag.New do
      begin
        case AnsiIndexStr(documentoFiscalPagamentos[nCont].formaIndicador, indicador) of
          0, 3, 10, 11, 12:
            begin
              Ide.indPag := ipVista;
              indPag := ipVista;
            end;
          1, 2, 4, 5, 6, 7, 8, 9, 13:
            begin
              Ide.indPag := ipPrazo;
              indPag := ipPrazo;
            end;
          14:
            begin
              Ide.indPag := ipNenhum;
              indPag := ipNenhum;
            end;
          15:
            begin
              Ide.indPag := ipOutras;
              indPag := ipOutras;
            end;
        end;
        tPag   := StrToFormaPagamento(lOk, documentoFiscalPagamentos[nCont].formaIndicador);
        vPag   := documentoFiscalPagamentos[nCont].valor;
        pag.vTroco := pag.vTroco + DocumentoFiscal.documentoFiscalPagamentos[nCont].troco;
       end;
    end;

    NotaF.NFe.infIntermed.CNPJ := '';
    NotaF.NFe.infIntermed.idCadIntTran := '';
  end;

  nfe.NotasFiscais.GerarNFe();

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
    if Length(cnpjInformado) > 0 then
      Dest.CNPJCPF            := cnpjInformado;

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
        Imposto.ICMS.vICMS := icmsValor;

        Imposto.PIS.CST := StrToCSTPIS(lOk, cstPIS.codigo);
        Imposto.PIS.vBC := TotalItem;
        Imposto.PIS.pPIS := pisAliquota;

        Imposto.COFINS.CST := StrToCSTCOFINS(lOk, CSTCOFINS.codigo);
        Imposto.COFINS.vBC := TotalItem;
        Imposto.COFINS.pCOFINS := cofinsAliquota;

        infAdProd := '';
      end;
    end;
    sat.CFe.Total.vCFe := TotalGeral - DocumentoFiscal.desconto;
    sat.CFe.Total.vCFeLei12741 := TotalImposto;

    for Counter := 0 to Length(DocumentoFiscalPagamentos) - 1 do
    begin
      with Pagto.New do
      begin
        cMP := StrToCodigoMP(lOk, DocumentoFiscalPagamentos[Counter].formaIndicador);
        vMP := DocumentoFiscalPagamentos[Counter].valor;
        if StrToCodigoMP(lOk, DocumentoFiscalPagamentos[Counter].formaIndicador) in [mpCartaodeCredito, mpCartaodeDebito] then
          cAdmC := StrToIntDef(DocumentoFiscalPagamentos[Counter].cartaoCredenciadora, 999);
      end;
    end;

    InfAdic.infCpl := 'Acesse constel.cloud para obter maiores;informações sobre o sistema Constel';

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

  nfe.NotasFiscais.Clear;

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
      pag.vTroco := DocumentoFiscal.documentoFiscalPagamentos[nCounter].troco;
    end;

    InfAdic.infCpl     :=  '';
    InfAdic.infAdFisco :=  '';

  end;

  nfe.NotasFiscais.GerarNFe;
end;

end.
