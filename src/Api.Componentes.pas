unit Api.Componentes;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, IniFiles, ACBrBase, ACBrSAT, Lib.Sistema.Tipos,
  ACBrDFeSSL, ACBrSATClass, pcnConversao, pcnConversaoNFe, Model.DocumentoFiscal, Model.Estabelecimento,
  pcnCFe, ACBrDFe, ACBrNFe, ACBrMail, ACBrUtil.Strings, ACBrUtil.Math, ACBrDFeUtil, ACBrNFeNotasFiscais,
  pcnNFe, Api.Funcoes, System.Math, System.NetEncoding, ACBrNFeDANFeFPDF, ACBrSATExtratoClass,
  System.IOUtils, Model.Config, Soap.EncdDecd, System.Generics.Collections, Lib.Funcoes,
  Model.Inutilizacao, ACBrSATExtratoFPDF, Horse, Model.Sped, APIService, Fortes.IRegistro,
  {$IFDEF MSWINDOWS}
    WinApi.ActiveX, ACBrSATExtratoESCPOS, ACBrPosPrinter, ACBrSATExtratoFortesFr;
  {$ENDIF}

const
  docModelos: TArray<String> = ['55', '56', '57', '58', '59', '65'];

type
  Tcomponentes = class(TDataModule)
    nfe: TACBrNFe;
    sat: TACBrSAT;
    mail: TACBrMail;
    procedure satGetcodigoDeAtivacao(var Chave: AnsiString);
    procedure satGetsignAC(var Chave: AnsiString);
    procedure DataModuleCreate(Sender: TObject);
  private
    FsatCodigoDeAtivacao: String;
    FsatAssinaturaAC: String;
    FRespTec: Boolean;
    Fconfig: TConfig;

    {$IFDEF MSWINDOWS}
      Fimpressora: TACBrPosPrinter;
      function PreparaImpressao(const impressora: TACBrPosPrinter): Boolean;
    {$ENDIF}

    function inicializaSAT: Boolean;

    procedure carregaSAT;
    procedure carregaNFe(estabelecimento: TEstabelecimentoC; const modelo: string = '55');
    procedure carregaCertificado(estabelecimento: TEstabelecimentoC);
    procedure carregaEmail;

    function  GerarCFe(const DocumentoFiscal: TDocumentoFiscal): string;
    procedure GerarNFe(const DocumentoFiscal: TDocumentoFiscal);
    procedure GerarNFCe(const DocumentoFiscal: TDocumentoFiscal);

    function CancelarDoc(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
    function CancelarCFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;

  public
    function EmiteDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
    function CancelarDFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
    function ImprimirDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;

    property satCodigoDeAtivacao: String read FsatCodigoDeAtivacao write FsatCodigoDeAtivacao;
    property satAssinaturaAC: String read FsatAssinaturaAC write FsatAssinaturaAC;

    function gerarSPED(Req: THorseRequest; var erros: string): TStringStream;
    procedure gerarArquivoFortesFiscal(const FileName: String; Registros: TList<IRegistro>);

  end;

var
  componentes: Tcomponentes;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ Tcomponents }

procedure Tcomponentes.carregaSAT;
begin
  try
    InfoConfig(FConfig);

    with sat do
    begin
      SSL.SSLCryptLib                     := cryOpenSSL;
      SSL.SSLXmlSignLib                   := xsLibXml2;

      Config.XmlSignLib                   := SSL.SSLXmlSignLib;
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

      satCodigoDeAtivacao                 := FConfig.cfe.codigodeativacao;
      satAssinaturaAC                     := FConfig.cfe.swhouse.assinatura;
    end;

  except
    on E:Exception do
    begin
      Log(Format('Impossível carregar o SAT. Verificar com suporte, o erro %s.', [E.Message]));
    end;
  end;
end;

procedure Tcomponentes.DataModuleCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    RemoveDataModule(self);
  {$ENDIF}
end;

function Tcomponentes.ImprimirDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
  procedure EmailDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String);
  var
    CC: Tstrings;
    xmlDocumento: String;
    mmEmailMsg: TStringList;
  begin
    CC := TStringList.Create;

    mmEmailMsg := TStringList.Create;
    mmEmailMsg.Add('Segue documento fiscal eletrônico referente a ');
    mmEmailMsg.Add('sua compra realizada conosco no dia ' + FormatDateTime('dd/mm/yyyy', DocumentoFiscal.emissao));

    try
      case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
        0, 5:
        begin
          var danfe: TACBrNFeDANFeFPDF;
          danfe := TACBrNFeDANFeFPDF.Create(nil);

          with danfe do
          begin
            Sistema := 'Audax Constel';
            Site    := 'https://constel.cloud';
          end;

          nfe.DANFE := danfe;
          nfe.NotasFiscais.Clear;

          {$IFDEF MSWINDOWS}
            CoInitialize(nil);
          {$ENDIF}

          xmlDocumento := DocumentoFiscal.documentoFiscalNFe.xml;
          if nfe.NotasFiscais.LoadFromString(xmlDocumento) then
          begin
            mmEmailMsg.Add('de número ' + IntToStr(DocumentoFiscal.documentoFiscalNFe.numero));

            carregaEmail;
            nfe.NotasFiscais.Items[0].EnviarEmail(DocumentoFiscal.email
              , 'Constel Docs [Documento Fiscal nº ' + IntToStr(DocumentoFiscal.documentoFiscalNFe.numero) + ']'
              , TStrings(mmEmailMsg)
              , True
              , CC
              , nil
              );
          end;
        end;
        4:
        begin
          xmlDocumento := DocumentoFiscal.documentoFiscalCFe.xml;
          sat.CFe.SetXMLString(AnsiString(xmlDocumento));

          mmEmailMsg.Add('de número ' + IntToStr(DocumentoFiscal.documentoFiscalCFe.numero));

          carregaEmail;
          sat.EnviarEmail(DocumentoFiscal.email
            , 'Constel Docs [Cupom Fiscal nº ' + IntToStr(DocumentoFiscal.documentoFiscalCFe.numero) + ']'
            , TStrings(mmEmailMsg)
            , CC
            , nil
          );
        end;
      end;
    finally
      CC.Free;
      mmEmailMsg.Free;
    end;

  end;

var
  xmlDocumento, documento: String;
  stream: TMemoryStream;
begin
  {$IFDEF MSWINDOWS}
    CoInitialize(nil);
  {$ENDIF}
  if Assigned(FConfig) then
    FConfig := nil;
  InfoConfig(FConfig);

  try
    case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
      0, 5:
        begin
          if (DocumentoFiscal.documentoFiscalNFe.chave > '') and (DocumentoFiscal.documentoFiscalNFe.status = 100) and (DocumentoFiscal.documentoFiscalNFe.protocolo > '') then
          begin
            xmlDocumento := DocumentoFiscal.documentoFiscalNFe.xml;
            case IntToTObjetivo(DocumentoFiscal.objetivo) of
              toNenhum: ;
              toImpressao:
                begin
                  var nfe: TACBrNFe;
                  var danfe: TACBrNFeDANFeFPDF;

                  nfe := TACBrNFe.Create(nil);
                  danfe := TACBrNFeDANFeFPDF.Create(Self);

                  try
                    with danfe do
                    begin
                      Sistema := 'Audax Constel';
                      Site    := 'https://constel.cloud';
                    end;

                    nfe.DANFE := danfe;
                    nfe.NotasFiscais.Clear;

                    if nfe.NotasFiscais.LoadFromString(xmlDocumento) then
                    begin
                      nfe.DANFE.PathPDF := ExtractFilePath(GetCurrentDir) + 'DocumentosFiscais\NFe\PDF';
                      nfe.DANFE.ImprimirDANFE;
                    end;

                  finally
                    danfe.Free;
                    nfe.Free;
                  end;
                end;
              toEmail:
                EmailDFe(DocumentoFiscal, Error, Msg);
              toBase64:
                begin
                  var nfe: TACBrNFe;
                  var danfe: TACBrNFeDANFeFPDF;

                  nfe := TACBrNFe.Create(nil);
                  danfe := TACBrNFeDANFeFPDF.Create(Self);

                  try
                    with danfe do
                    begin
                      Sistema := 'Audax Constel';
                      Site    := 'https://constel.cloud';
                    end;

                    nfe.DANFE := danfe;
                    nfe.NotasFiscais.Clear;

                    if nfe.NotasFiscais.LoadFromString(xmlDocumento) then
                    begin
                      var pastaPDF: string;
                      {$IFDEF MSWINDOWS}
                        pastaPDF := FConfig.nfe.arquivos.pathnfe + '\PDF';
                      {$ELSE}
                        pastaPDF := FConfig.nfe.arquivos.pathnfe + '/pdf';
                      {$ENDIF}

                      nfe.DANFE.PathPDF := pastaPDF;

                      stream := TMemoryStream.Create;
                      try
                        nfe.DANFE.ImprimirDANFEPDF;
                        stream.LoadFromFile(nfe.DANFE.ArquivoPDF);
                        documento := StringReplace(String(EncodeBase64(stream.Memory, stream.Size)), #13#10, '', [rfReplaceAll]);
                        documentoFiscal.imagem := AnsiString(documento);
                      finally
                        if FileExists(nfe.DANFE.ArquivoPDF) then
                          DeleteFile(PWideChar(nfe.DANFE.ArquivoPDF));
                        stream.Free;
                      end;
                    end;

                  finally
                    danfe.Free;
                    nfe.Free;
                  end;
                end;
            end;
          end;
        end;
      4:
        begin
          sat.CFe.SetXMLString(AnsiString(DocumentoFiscal.documentoFiscalCFe.xml));
          case IntToTObjetivo(DocumentoFiscal.objetivo) of
          toNenhum: ;
          toImpressao:
            begin
            {$IFDEF MSWINDOWS}
              Fimpressora := TACBrPosPrinter.Create(Self);
              var Extrato := TACBrSATExtratoESCPOS.Create(Fimpressora);
              Extrato.PosPrinter := Fimpressora;
              sat.Extrato := Extrato;
              PreparaImpressao(Fimpressora);

              with Extrato do
              begin
                Sistema := 'Audax Constel';
                Site    := 'https://constel.cloud';
              end;

              sat.ImprimirExtrato;
            {$ELSE}
              Log('Impressão exclusiva em ambiente local.');
            {$ENDIF}
            end;
          toEmail:
            EmailDFe(DocumentoFiscal, Error, Msg);
          toBase64:
            begin
              {$IFDEF MSWINDOWS}
                var Extrato := TACBrSATExtratoFortes.Create(Self);
                Extrato.PathPDF := GetCurrentDir + '\DocumentosFiscais\CFe\PDF\';
              {$ELSE}
                var Extrato := TACBrSATExtratoFPDF.Create(Self);
                Extrato.PathPDF := GetCurrentDir + 'DocumentosFiscais/CFe/PDF/';
              {$ENDIF}

              with Extrato do
              begin
                Sistema := 'Audax Constel';
                Site    := 'https://constel.cloud';
                NomeDocumento := IntToStr(sat.CFe.ide.nCFe) + '.pdf';
                Filtro := fiPDF;
              end;

              sat.Extrato := Extrato;
              sat.ImprimirExtrato;

              stream := TMemoryStream.Create;
              try
                stream.LoadFromFile(Extrato.PathPDF + Extrato.NomeDocumento);
                documento := StringReplace(String(EncodeBase64(stream.Memory, stream.Size)), #13#10, '', [rfReplaceAll]);
                documentoFiscal.imagem := AnsiString(documento);
                documentoFiscal.documentoFiscalCFe.imagem := documento;
              finally
                if FileExists(Extrato.PathPDF + Extrato.NomeDocumento) then
                  DeleteFile(PWideChar(Extrato.PathPDF + Extrato.NomeDocumento));
                stream.Free;
              end;
            end
          end;
        end;
    end;
  finally
    Result := documentoFiscal;
  end;

end;

function Tcomponentes.EmiteDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
var
  xmlDocumento: string;
  documentoFiscalImpresso: TDocumentoFiscal;
begin
  try
    documentoFiscalImpresso := nil;
    case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
      0: {$REGION 'NFe'}
        begin
          GerarNFe(DocumentoFiscal);

          if nfe.NotasFiscais.Count > 0 then
          begin

            nfe.Enviar(DocumentoFiscal.documentoFiscalNFe.numero, False, DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono);

            DocumentoFiscal.documentoFiscalNFe.status := IfThen(DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono, nfe.WebServices.Enviar.cStat, nfe.WebServices.Retorno.cStat);
            DocumentoFiscal.documentoFiscalNFe.msgRetorno := IfThen(DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono, nfe.WebServices.Enviar.Msg, nfe.WebServices.Retorno.Msg);


            if (DocumentoFiscal.documentoFiscalNFe.status = 100) and not(IfThen(DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono, nfe.WebServices.Enviar.Protocolo, nfe.WebServices.Retorno.Protocolo) = EmptyStr) then
            begin
              DocumentoFiscal.documentoFiscalNFe.chave := nfe.NotasFiscais.Items[0].NumID;
              DocumentoFiscal.documentoFiscalNFe.xml := nfe.NotasFiscais.Items[0].XMLAssinado;
              DocumentoFiscal.documentoFiscalNFe.protocolo := IfThen(DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono, nfe.WebServices.Enviar.Protocolo, nfe.WebServices.Retorno.Protocolo);

              Log(Format('Emitida a NFe de número: %d com chave: %s.', [nfe.NotasFiscais.Items[0].NFe.Ide.nNF,
                                                                 nfe.NotasFiscais.Items[0].NumID]));
              Msg := nfe.NotasFiscais.Items[0].Msg;

              documentoFiscalImpresso := ImprimirDFe(DocumentoFiscal, Error, Msg);
              Log(Format('Nota fiscal %d com chave: %s, impressa com sucesso..', [nfe.NotasFiscais.Items[0].NFe.Ide.nNF,
                                                                 nfe.NotasFiscais.Items[0].NumID]));
            end
            else
            begin
              Log(Format('Não foi possível emitir a NFe, o seguinte erro ocorreu: %s.', [nfe.WebServices.Retorno.Msg]));
              Error := nfe.WebServices.Retorno.Msg;
              documentoFiscalImpresso := DocumentoFiscal;
            end;
          end
          else
            Log('Não há documento fiscal para enviar.');
        end;
        {$ENDREGION 'NFe'}
      4: {$REGION 'CFe'}
        begin
          xmlDocumento := GerarCFe(DocumentoFiscal);

          var
            erro: string;

          sat.ValidarDadosVenda(AnsiString(xmlDocumento), erro);
          sat.EnviarDadosVenda(AnsiString(xmlDocumento));

          if sat.Resposta.codigoDeRetorno = 6000 then
          begin
            DocumentoFiscal.documentoFiscalCFe.chave  := sat.CFe.infCFe.ID;
            DocumentoFiscal.documentoFiscalCFe.serie  := sat.CFe.ide.nserieSAT;
            DocumentoFiscal.documentoFiscalCFe.xml    := String(sat.CFe.AsXMLString);
            DocumentoFiscal.documentoFiscalCFe.numero := sat.CFe.ide.nCFe;
            DocumentoFiscal.documentoFiscalCFe.sessao := sat.Resposta.numeroSessao;
            DocumentoFiscal.documentoFiscalCFe.status := sat.Resposta.codigoDeRetorno;
            DocumentoFiscal.documentoFiscalCFe.formaDeEmissao := StrToIntDef(TpAmbToStr(sat.CFe.ide.tpAmb), 1);
            Log(Format('Emitido o cupom fiscal: %d com chave: %s.', [sat.CFe.ide.nCFe, sat.CFe.infCFe.ID]));

            Msg := sat.Resposta.mensagemRetorno;

            documentoFiscalImpresso := ImprimirDFe(DocumentoFiscal, erro, Msg);
            Log(Format('Cumpom fiscal %d com chave: %s, impresso com sucesso..', [sat.CFe.ide.nCFe, sat.CFe.infCFe.ID]));

          end
          else
          begin
            Log(Format('Não foi possivel emitir o cupom fiscal, o seguinte erro ocorreu: %s.', [sat.Resposta.mensagemRetorno]));
            Error := erro;
            documentoFiscalImpresso := DocumentoFiscal;
          end;
        end;
        {$ENDREGION 'CFe'}
      5: {$REGION 'NCFe'}
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

            Log(Format('Emitida a NFCe de número: %d com chave: %s.', [nfe.NotasFiscais.Items[0].NFe.Ide.nNF,
                                                                       nfe.NotasFiscais.Items[0].NumID]));
            Msg := nfe.WebServices.Retorno.Msg;

            documentoFiscalImpresso := ImprimirDFe(DocumentoFiscal, Error, Msg);
            Log(Format('Nota fiscal %d com chave: %s, impressa com sucesso..', [nfe.NotasFiscais.Items[0].NFe.Ide.nNF,
                                                               nfe.NotasFiscais.Items[0].NumID]));
          end
          else
          begin
            Log(Format('Não foi possível emitir a NFCe, o seguinte erro ocorreu: %s.', [nfe.WebServices.Retorno.Msg]));
            Error := nfe.WebServices.Retorno.Msg;
            documentoFiscalImpresso := DocumentoFiscal;
          end;
        end;
        {$ENDREGION 'NFCe'}
    end;

    Result := documentoFiscalImpresso;

  except
    on E: Exception do
    begin
      Log(Format('Houve um erro na tentativa de enviar o documento. Verifique a mensagem a seguir: %s.',[E.Message]));
      Error := E.Message;
      Result := nil;
    end;
  end;
end;

procedure Tcomponentes.gerarArquivoFortesFiscal(const FileName: String; Registros: TList<IRegistro>);
var
  Lista: TStringList;
  Registro: IRegistro;
begin
  Lista := TStringList.Create;
  try
    for Registro in Registros do
      Lista.Add(Registro.GerarLinha);

    Lista.SaveToFile(FileName);
  finally
    Lista.Free;
  end;
end;

function Tcomponentes.CancelarDFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
begin
  var DocumentoFiscalCancelado: TDocumentoFiscal := nil;
  case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
    0, 5: DocumentoFiscalCancelado := CancelarDoc(DocumentoFiscal);
    4: DocumentoFiscalCancelado := CancelarCFe(DocumentoFiscal);
  end;
  Result := DocumentofiscalCancelado;
end;

function Tcomponentes.CancelarDoc(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
var
  lOk: Boolean;
begin
  try
    if DocumentoFiscal.modelo = '55' then
    begin
      carregaNFe(DocumentoFiscal.estabelecimento);

      nfe.Consultar(DocumentoFiscal.documentoFiscalNFe.chave, True);

      nfe.Configuracoes.WebServices.UF            := DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla;
      nfe.Configuracoes.WebServices.Ambiente      := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.ambiente));

      if not(nfe.WebServices.Consulta.cStat = 101) then
      begin
        nfe.NotasFiscais.Clear;
        nfe.NotasFiscais.LoadFromString(DocumentoFiscal.documentoFiscalNFe.xml);

        nfe.EventoNFe.Evento.Clear;
        nfe.EventoNFe.idLote := DocumentoFiscal.documentoFiscalNFe.numero;

        with nfe.EventoNFe.Evento.New do
        begin
          infEvento.dhEvento        := now;
          infEvento.tpEvento        := teCancelamento;
          infEvento.detEvento.xJust := DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa;
        end;

        nfe.EnviarEvento(DocumentoFiscal.documentoFiscalNFe.numero);

        if nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat = 135 then
        begin
          DocumentoFiscal.documentoFiscalNFe.status                     := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat;
          DocumentoFiscal.documentoFiscalNFe.cancelamentoProtocolo      := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt;
          DocumentoFiscal.documentoFiscalNFe.cancelamentoData           := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento;
          DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa  := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;

          Log(Format('Nota fiscal: %d com chave: %s, cancelada com sucesso.', [DocumentoFiscal.documentoFiscalNFe.numero,
                                                                               DocumentoFiscal.documentoFiscalNFe.chave]));
        end
        else
        begin
          DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
          Log(Format('Não foi possivel cancelar a nota fiscal, o seguinte erro ocorreu: %s.', [nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo]));
        end;
      end
      else
      begin
        if nfe.WebServices.Consulta.retCancNFe.cStat = 101 then
        begin
          DocumentoFiscal.documentoFiscalNFe.status                     := nfe.WebServices.Consulta.retCancNFe.cStat;
          DocumentoFiscal.documentoFiscalNFe.cancelamentoProtocolo      := nfe.WebServices.Consulta.retCancNFe.nProt;
          DocumentoFiscal.documentoFiscalNFe.cancelamentoData           := nfe.WebServices.Consulta.retCancNFe.dhRecbto;
          DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa  := nfe.WebServices.Consulta.retCancNFe.xMotivo;

          Log(Format('Nota fiscal: %d com chave: %s, já cancelada anteriormente.', [DocumentoFiscal.documentoFiscalNFe.numero,
                                                                                    DocumentoFiscal.documentoFiscalNFe.chave]));
        end;
      end;
    end
    else if DocumentoFiscal.modelo = '65' then
    begin
      carregaNFe(DocumentoFiscal.estabelecimento, '65');
      nfe.Consultar(DocumentoFiscal.documentoFiscalNFe.chave, True);

      nfe.Configuracoes.WebServices.UF       := DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla;
      nfe.Configuracoes.WebServices.Ambiente := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.ambiente));

      if not(nfe.WebServices.Consulta.cStat = 101) then
      begin
        nfe.NotasFiscais.Clear;
        nfe.NotasFiscais.LoadFromString(DocumentoFiscal.documentoFiscalNFe.xml);

        nfe.EventoNFe.Evento.Clear;
        nfe.EventoNFe.idLote := DocumentoFiscal.documentoFiscalNFe.numero;

        with nfe.EventoNFe.Evento.New do
        begin
          infEvento.dhEvento        := now;
          infEvento.tpEvento        := teCancelamento;
          infEvento.detEvento.xJust := DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa;
        end;

        nfe.EnviarEvento(DocumentoFiscal.documentoFiscalNFe.numero);

        if nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat = 135 then
        begin
          DocumentoFiscal.documentoFiscalNFe.status                     := nfe.WebServices.EnvEvento.cStat;
          DocumentoFiscal.documentoFiscalNFe.cancelamentoProtocolo      := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt;
          DocumentoFiscal.documentoFiscalNFe.cancelamentoData           := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento;
          DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa  := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;

          Log(Format('Cupom fiscal: %d com chave: %s, cancelado com sucesso.', [DocumentoFiscal.documentoFiscalNFe.numero,
                                                                                DocumentoFiscal.documentoFiscalNFe.chave]));
         end
        else
        begin
          DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
          Log(Format('Não foi possivel cancelar o cupom fiscal, o seguinte erro ocorreu: %s.', [nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo]));
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

          Log(Format('Cupom fiscal: %d com chave: %s, já cancelado anteriormente.', [DocumentoFiscal.documentoFiscalNFe.numero,
                                                                                     DocumentoFiscal.documentoFiscalNFe.chave]));
        end;
      end;
    end;

    Result := DocumentoFiscal;
  except
    on E: Exception do
    begin
      Log(Format('Houve um erro na tentativa de cancelar o documento. Verifique a mensagem a seguir: %s.',[E.Message]));
      Result := nil;
    end;
  end;
end;

function Tcomponentes.CancelarCFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
begin
  inicializaSAT;
  sat.InicializaCFe;
  var DocumentoFiscalCancelado: TDocumentoFiscal := nil;

  try
    sat.CFe.SetXMLString(AnsiString(DocumentoFiscal.documentoFiscalCFe.xml));
    sat.CFe2CFeCanc;

    var xmlCancelamento := sat.CFeCanc.GerarXML(True);

    sat.CFeCanc.AsXMLString := xmlCancelamento;
    sat.CancelarUltimaVenda(AnsiString(sat.CFeCanc.infCFe.chCanc), xmlCancelamento);

    if sat.Resposta.codigoDeRetorno = 7000 then
    begin
      DocumentoFiscal.documentoFiscalCFe.chaveCancelamento := sat.CFeCanc.infCFe.ID;
      DocumentoFiscal.documentoFiscalCFe.xmlCancelamento := String(sat.CFeCanc.AsXMLString);
      DocumentoFiscal.documentoFiscalCFe.status := sat.Resposta.codigoDeRetorno;
      DocumentoFiscal.documentoFiscalCFe.sessao := sat.Resposta.numeroSessao;

      Log(Format('Cupom fiscal: %d com chave: %s, cancelado com sucesso.',[sat.CFe.ide.nCFe, sat.CFe.infCFe.ID]));

      DocumentoFiscalCancelado := DocumentoFiscal;

    end
    else
      Log(Format('Não foi possivel cancelar o cupom fiscal, o seguinte erro ocorreu: %s.', [sat.Resposta.mensagemRetorno]));

  except
    on E: Exception do
    begin
      Log(Format('Houve um erro na tentativa de inicializar o equipamento MFe. Verifique a mensagem a seguir: %s.', [E.Message]));
    end;
  end;
  Result := DocumentoFiscalCancelado;
end;

function Tcomponentes.inicializaSAT: boolean;
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
       Log(Format('Erro ao carregar equipamento MFe/SAT. Mensagem de erro: %s.', [E.Message]));
       Result := False;
    end;
  end;

end;

{$IFDEF MSWINDOWS}
  function Tcomponentes.PreparaImpressao(const impressora: TACBrPosPrinter): Boolean;
  begin
    try
      InfoConfig(FConfig);
      with impressora do
      begin
        Desativar;
        Modelo := TACBrPosPrinterModelo(FConfig.impressora.modelo);
        PaginaDeCodigo := pcUTF8;
        Porta := FConfig.impressora.porta;
        ColunasFonteNormal := FConfig.impressora.colunas;
        LinhasEntreCupons := FConfig.impressora.linhas;
        EspacoEntreLinhas := 3;
      end;
    except
    end;
    Result := True;
  end;
{$ENDIF}

procedure Tcomponentes.carregaCertificado(estabelecimento: TEstabelecimentoC);
begin
  nfe.SSL.DescarregarCertificado;

  if (estabelecimento.estabelecimentoFiscalSerie.certificadopfx = '') and (estabelecimento.estabelecimentoFiscalSerie.certificadonumerodeserie = '') and (estabelecimento.estabelecimentoFiscalSerie.certificadourl = '') then
  begin
    Log('Erro ao tentar carregar o certificado digital. Não foi informado o caminho, número de série ou url válida.');
    Exception.Create('Erro ao tentar carregar o certificado digital. Não foi informado o caminho, número de série ou url.');
  end;

  with nfe.SSL do
  begin
    ArquivoPFX              := estabelecimento.estabelecimentoFiscalSerie.certificadopfx;
    URLPFX                  := estabelecimento.estabelecimentoFiscalSerie.certificadourl;
    Senha                   := AnsiString(estabelecimento.estabelecimentoFiscalSerie.certificadosenha);
    NumeroSerie             := estabelecimento.estabelecimentoFiscalSerie.certificadonumerodeserie;
  end;

  with nfe.Configuracoes.Certificados do
  begin
    ArquivoPFX              := estabelecimento.estabelecimentoFiscalSerie.certificadopfx;
    URLPFX                  := estabelecimento.estabelecimentoFiscalSerie.certificadourl;
    Senha                   := AnsiString(estabelecimento.estabelecimentoFiscalSerie.certificadosenha);
    NumeroSerie             := estabelecimento.estabelecimentoFiscalSerie.certificadonumerodeserie;
  end;

  if not(nfe.Configuracoes.Certificados.VerificarValidade) then
  begin
    Log(Format('Erro de certificado. Certificado vencido em %s.', [FormatDateTime('dd/mm/yyyy', nfe.ssl.CertDataVenc)]));
    Exit;
  end;
end;

procedure Tcomponentes.carregaEmail;
begin
  if Assigned(FConfig) then
    FConfig := nil;
  InfoConfig(FConfig);
  with mail do
  begin
    Host                := FConfig.emitente.email.servidor;
    Port                := FConfig.emitente.email.porta;
    Username            := FConfig.emitente.email.usuario;
    Password            := FConfig.emitente.email.senha;
    From                := FConfig.emitente.email.origem;
    SetSSL              := FConfig.emitente.email.usassl;
    SetTLS              := FConfig.emitente.email.usatls;
    ReadingConfirmation := False;
    UseThread           := FConfig.emitente.email.usathread;
    FromName            := FConfig.emitente.email.remetente;
  end;
end;

procedure Tcomponentes.carregaNFe(estabelecimento: TEstabelecimentoC; const modelo: string = '55');
var
  lOk: Boolean;
begin

  FRespTec := estabelecimento.estabelecimentoFiscal.responsaveltecnico;

  try
    with nfe.Configuracoes.Geral, estabelecimento do
    begin
      SSLLib                  := TSSLLib(estabelecimentoFiscal.ssllib);
      SSLCryptLib             := TSSLCryptLib(estabelecimentoFiscal.cryptlib);
      SSLHttpLib              := TSSLHttpLib(estabelecimentoFiscal.httplib);
      SSLXmlSignLib           := TSSLXmlSignLib(estabelecimentoFiscal.xmlsignlib);

      Salvar                  := True;
      RetirarAcentos          := estabelecimentoFiscalSerie.retiraracentos;
      AtualizarXMLCancelado   := estabelecimentoFiscalSerie.atualizarxml;
      ExibirErroSchema        := estabelecimentoFiscalSerie.exibirerroschema;
      FormaEmissao            := TpcnTipoEmissao(estabelecimentoFiscalSerie.formadeemissao);
      VersaoDF                := StrToVersaoDF(lOk, estabelecimentoFiscalSerie.versaodf);

      ModeloDF        := StrToModeloDF(lOk, modelo);
      FormatoAlerta   := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.';

      IdCSC                 := estabelecimentoFiscalSerie.nfceidcsc.ToString;
      CSC                   := estabelecimentoFiscalSerie.nfcecsc;
      VersaoQRCode          := veqr200;

    end;

    with nfe.Configuracoes.Arquivos do
    begin
      Salvar                  := True;
      SepararPorMes           := True;
      AdicionarLiteral        := True;
      EmissaoPathNFe          := True;
      SalvarEvento            := False;
      SepararPorCNPJ          := True;
      SepararPorModelo        := True;
      PathSchemas             := '.\documentos\schemas\nfe';
      PathNFe                 := '.\documentos\arquivos\envio';
      PathInu                 := '.\documentos\arquivos\inu';
      PathEvento              := '.\documentos\arquivos\eventos';
      PathSalvar              := '.\documentos\arquivos\nfe';
    end;

    carregaCertificado(estabelecimento);

    with nfe.Configuracoes.WebServices do
    begin
      Visualizar              := False;
      Salvar                  := True;
      AjustaAguardaConsultaRet:= True;
      AguardarConsultaRet     := 10000;
      Tentativas              := 3;
      IntervaloTentativas     := 10000;
      TimeOut                 := 30000;
      ProxyHost               := '';
      ProxyPort               := '';
      ProxyUser               := '';
      ProxyPass               := '';
    end;

  except
    on E: Exception do
       Log(Format('Erro ao carregar dados do arquivo de configuração. Mensagem de erro: %s.', [E.Message]));
  end;
end;

procedure Tcomponentes.satGetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := AnsiString(FsatCodigoDeAtivacao);
end;

procedure Tcomponentes.satGetsignAC(var Chave: AnsiString);
begin
  Chave := AnsiString(FsatAssinaturaAC);
end;

procedure Tcomponentes.GerarNFe(const DocumentoFiscal: TDocumentoFiscal);
var
  lOk: Boolean;
  NotaF: NotaFiscal;
  vBaseDeCalculo,
  vTotalICMS,
  vTotalPIS,
  vTotalCOFINS,
  vTotalItens,
  vTotalFCPST,
  vTotalIPI,
  vTotalII,
  vTotalIPIDevol,
  vTotalDescontos: Double;
  Count: TNFe;
begin
  vBaseDeCalculo := 0;
  vTotalICMS := 0;
  vTotalPIS := 0;
  vTotalCOFINS := 0;
  vTotalIPI := 0;
  vTotalIPIDevol := 0;
  vTotalII := 0;
  vTotalItens := 0;
  vTotalFCPST := 0;
  vTotalDescontos := 0;

  nfe.NotasFiscais.Clear;

  carregaNFe(DocumentoFiscal.estabelecimento);

  nfe.Configuracoes.WebServices.UF       := DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla;
  nfe.Configuracoes.WebServices.Ambiente := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.estabelecimento.estabelecimentoFiscalSerie.ambiente));

  NotaF := nfe.NotasFiscais.Add;
  with NotaF.NFe, DocumentoFiscal do
  begin
    Ide.natOp       := historico.nome;
    Ide.modelo      := 55;
    Ide.serie       := documentoFiscalNFe.serie;
    Ide.nNF         := documentoFiscalNFe.numero;
    Ide.cNF         := GerarCodigoDFe(Ide.nNF);
    Ide.dEmi        := emissao;
    Ide.dSaiEnt     := saida;
    Ide.hSaiEnt     := saida;
    Ide.tpNF        := tnSaida;
    Ide.tpEmis      := StrToTpEmis(lOk, IntToStr(DocumentoFiscal.estabelecimento.estabelecimentoFiscalSerie.formadeemissao));
    Ide.tpAmb       := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.estabelecimento.estabelecimentoFiscalSerie.ambiente));
    Ide.verProc     := '2024.06';
    Ide.cUF         := UFtoCUF(estabelecimento.estabelecimentoEnderecos[0].uf.sigla);
    Ide.cMunFG      := StrToInt(estabelecimento.estabelecimentoEnderecos[0].municipio.codigo);
    Ide.finNFe      := StrToFinNFe(lOk, IntToStr(documentoFiscalNFe.finalidadeEmissao));
    Ide.indIntermed := TindIntermed(documentoFiscalNFe.indicadorIntermediador);
    Ide.indFinal    := cfNao;

    if DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla =
       DocumentoFiscal.parceiro.parceiroEnderecos[0].uf.sigla then
      Ide.idDest    := doInterna
    else if DocumentoFiscal.parceiro.parceiroEnderecos[0].uf.sigla = 'EX' then
      Ide.idDest    := doExterior
    else
      Ide.idDest    := doInterestadual;


    if documentoFiscalNFe.tipoImpressao = 'R' then
      Ide.tpImp     := tiRetrato
    else
      Ide.tpImp     := tiPaisagem;

    if not(TpcnTipoEmissao(documentoFiscalNFe.formaDeEmissao) = teNormal) then
    begin
      Ide.dhCont    := date;
      Ide.xJust     := 'Falha no webservice normal';
    end;

    Emit.CNPJCPF            := estabelecimento.estabelecimentoDocumentos[0].documentoNumero;
    Emit.IE                 := estabelecimento.estabelecimentoDocumentos[0].inscricaoEstadual;
    Emit.xNome              := estabelecimento.nome;
    Emit.xFant              := estabelecimento.nome;

    Emit.EnderEmit.fone     := '';
    Emit.EnderEmit.CEP      := StrToInt(String(RemoveStrings(AnsiString(estabelecimento.estabelecimentoEnderecos[0].cep), ['.', '-'])));
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

    if FRespTec then
    begin
      infRespTec.CNPJ	        := '04528001000164';
      infRespTec.xContato	    := 'Myron Yerich P. Sales';
      infRespTec.email        := 'myron@solucaosistemas.net';
      infRespTec.fone	        := '8533076262';
    end;

    if not(estabelecimento.estabelecimentoFiscal.cnpjaut = EmptyStr)  then
    begin
      with autXML.New do
      begin
        CNPJCPF := estabelecimento.estabelecimentoFiscal.cnpjaut;
      end;
    end;

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
    Dest.EnderDest.CEP      := StrToInt(String(RemoveStrings(AnsiString(parceiro.parceiroEnderecos[0].cep), ['.', '-'])));
    Dest.EnderDest.xLgr     := parceiro.parceiroEnderecos[0].logradouro;
    Dest.EnderDest.nro      := IntToStr(parceiro.parceiroEnderecos[0].numero);
    Dest.EnderDest.xCpl     := parceiro.parceiroEnderecos[0].complemento;
    Dest.EnderDest.xBairro  := parceiro.parceiroEnderecos[0].bairro;
    Dest.EnderDest.cMun     := StrToInt(parceiro.parceiroEnderecos[0].municipio.codigo);
    Dest.EnderDest.xMun     := parceiro.parceiroEnderecos[0].municipio.nome;
    Dest.EnderDest.UF       := parceiro.parceiroEnderecos[0].uf.sigla;
    Dest.EnderDest.cPais    := 1058;
    Dest.EnderDest.xPais    := 'BRASIL';

    for var nCont := 0 to Length(DocumentoFiscalItens) - 1 do
    begin
      with Det.New, DocumentoFiscalItens[nCont] do
      begin
        Prod.nItem := nCont + 1;
        Prod.cProd := item.codigo;
        Prod.cEAN := '';
        Prod.xProd := item.nome;
        Prod.NCM := StringReplace(ncm.codigo, '.', '', [rfReplaceAll]);
        Prod.EXTIPI := '';
        if (DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla =
           DocumentoFiscal.parceiro.parceiroEnderecos[0].uf.sigla) or
           (DocumentoFiscal.parceiro.parceiroEnderecos[0].uf.sigla = 'EX') then
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
        infAdProd := '';

        Prod.vProd := RoundABNT(subtotal, -2);

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
              pRedBC := icmsReducaoBase;
              pMVAST := 0;
              pRedBCST := icmsSTReducaoBase;
              vBCST := icmsSTBC;
              pICMSST := icmsstAliquota;
              vICMSST := icmsSTValor;
            end;

            vBC := icmsBC;
            pICMS := icmsAliquota;
            vICMS := vBC * (pICMS/100);

            vBCFCPST := icmsSTBC;
            pFCPST := IfThen(icmsSTBC > 0, 2, 0);
            vFCPST := IfThen(icmsSTBC > 0, (icmsSTBC * (2/100)), 0);
            vTotalFCPST := vTotalFCPST + vFCPST;
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

            if not(Ide.finNFe = fnDevolucao) then
            begin
              pIPI   := ipiAliquota;
              vIPI   := vBC * (ipiAliquota / 100);

              vTotalIPI := vTotalIPI + vIPI;
            end
            else begin
              pDevol    := ipiAliquota;
              vIPIDevol := vBC * (ipiAliquota / 100);
            end;
          end;

          with PIS do
          begin
            CST       := pis99;
            vBC       := 0;
            pPIS      := 0;
            vPIS      := 0;

            qBCProd   := 0;
            vAliqProd := 0;
            vPIS      := 0;

            vTotalPIS := vTotalPIS + vPIS;
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

    NotaF.NFe.Total.ICMSTot.vBCST     := 0;
    NotaF.NFe.Total.ICMSTot.vST       := 0;
    NotaF.NFe.Total.ICMSTot.vProd     := vTotalItens;
    NotaF.NFe.Total.ICMSTot.vFrete    := DocumentoFiscal.frete;
    NotaF.NFe.Total.ICMSTot.vSeg      := 0;
    NotaF.NFe.Total.ICMSTot.vDesc     := vTotalDescontos +
                                         DocumentoFiscal.desconto;
    NotaF.NFe.Total.ICMSTot.vII       := vTotalII;
    NotaF.NFe.Total.ICMSTot.vIPI      := vTotalIPI;
    NotaF.NFe.Total.ICMSTot.vIPIDevol := vTotalIPIDevol;
    NotaF.NFe.Total.ICMSTot.vPIS      := vTotalPIS;
    NotaF.NFe.Total.ICMSTot.vCOFINS   := vTotalCOFINS;
    NotaF.NFe.Total.ICMSTot.vOutro    := DocumentoFiscal.outrasDespesas;
    NotaF.NFe.Total.ICMSTot.vNF       := vTotalItens +
                                         vTotalIPI +
                                         vTotalIPIDevol +
                                         DocumentoFiscal.frete +
                                         DocumentoFiscal.outrasDespesas -
                                         DocumentoFiscal.desconto -
                                         vTotalDescontos;
    NotaF.NFe.Total.ICMSTot.vTotTrib  := 0;

    NotaF.NFe.Total.ICMSTot.vFCPUFDest   := 0.00;
    NotaF.NFe.Total.ICMSTot.vICMSUFDest  := 0.00;
    NotaF.NFe.Total.ICMSTot.vICMSUFRemet := 0.00;

    NotaF.NFe.Total.ICMSTot.vFCPST     := vTotalFCPST;
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

    NotaF.NFe.Cobr.Fat.nFat  := IntToStr(documentoFiscal.DocumentoFiscalNFe.numero);
    NotaF.NFe.Cobr.Fat.vOrig := documentoFiscal.subtotal;
    NotaF.NFe.Cobr.Fat.vDesc := documentoFiscal.Desconto;
    NotaF.NFe.Cobr.Fat.vLiq  := documentoFiscal.subtotal - documentoFiscal.Desconto;


    for var nCont := 0 to Length(DocumentoFiscal.DocumentoFiscalCobrancas) - 1 do
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

    for var nCont := 0 to Length(documentoFiscalPagamentos) - 1 do
    begin
      with NotaF.NFe.pag.New do
      begin
        case AnsiIndexStr(documentoFiscalPagamentos[nCont].formaIndicador, indicador) of
          0, 3, 5, 6, 7, 8, 10, 11, 12:
            begin
              Ide.indPag := ipVista;
              indPag := ipVista;
            end;
          1, 2, 4, 9, 13:
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
              xPag := 'Outras formas de pagamento';
            end;
        end;
        tPag   := StrToFormaPagamento(lOk, documentoFiscalPagamentos[nCont].formaIndicador);
        vPag   := documentoFiscalPagamentos[nCont].valor;
        pag.vTroco := pag.vTroco + DocumentoFiscal.documentoFiscalPagamentos[nCont].troco;
       end;
    end;

    NotaF.NFe.infIntermed.CNPJ := '';
    NotaF.NFe.infIntermed.idCadIntTran := '';

    with NotaF.Nfe.InfAdic.obsCont.New do
    begin
      xCampo := 'Info';
      xTexto := Format('%s. Total %m.', [DocumentoFiscal.referencia, DocumentoFiscal.total]);
    end;
  end;

  nfe.NotasFiscais.GerarNFe();

end;

function Tcomponentes.GerarCFe(const DocumentoFiscal: TDocumentoFiscal): string;
var
  TotalItem, TotalImposto, TotalGeral: Double;
  lOk: Boolean;
  Counter: Integer;
  regimeTrib: TpcnRegTrib;
begin
  TotalImposto := 0;
  TotalGeral := 0;

  inicializaSAT;
  sat.InicializaCFe;

  with sat do
  begin
    if DocumentoFiscal.estabelecimento.estabelecimentoDocumentos[0].regimeTributarioICMS > 1 then
      regimeTrib := RTRegimeNormal
    else
      regimeTrib := TpcnRegTrib(DocumentoFiscal.estabelecimento.estabelecimentoDocumentos[0].regimeTributarioICMS);

    Config.emit_CNPJ                    := DocumentoFiscal.estabelecimento.estabelecimentoDocumentos[0].documentoNumero;
    Config.emit_IE                      := DocumentoFiscal.estabelecimento.estabelecimentoDocumentos[0].inscricaoEstadual;
    Config.emit_IM                      := DocumentoFiscal.estabelecimento.estabelecimentoDocumentos[0].inscricaoMunicipal;
    Config.emit_cRegTrib                := regimeTrib;
  end;

  with sat.CFe, DocumentoFiscal do
  begin
    IdentarXML        := False;
    TamanhoIdentacao  := 0;
    RetirarAcentos    := True;
    RetirarEspacos    := True;
    infCFe.versao     := FConfig.cfe.versao;
    infCFe.versaoSB   := FConfig.cfe.versaosb;
    ide.cNF           := Random(999999);

    Emit.CNPJ               := estabelecimento.estabelecimentoDocumentos[0].documentoNumero;
    Emit.IE                 := estabelecimento.estabelecimentoDocumentos[0].inscricaoEstadual;
    Emit.xNome              := estabelecimento.nome;
    Emit.xFant              := estabelecimento.nome;

    Emit.EnderEmit.CEP      := StrToInt(String(RemoveStrings(AnsiString(estabelecimento.estabelecimentoEnderecos[0].cep), ['.', '-'])));
    Emit.EnderEmit.xLgr     := estabelecimento.estabelecimentoEnderecos[0].logradouro;
    Emit.EnderEmit.nro      := IntToStr(estabelecimento.estabelecimentoEnderecos[0].numero);
    Emit.EnderEmit.xCpl     := estabelecimento.estabelecimentoEnderecos[0].complemento;
    Emit.EnderEmit.xBairro  := estabelecimento.estabelecimentoEnderecos[0].bairro;
    Emit.EnderEmit.xMun     := estabelecimento.estabelecimentoEnderecos[0].municipio.nome;

    Emit.IM                 := estabelecimento.estabelecimentoDocumentos[0].inscricaoMunicipal;
    Emit.cRegTrib           := regimeTrib;

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
        Prod.vOutro := DocumentoFiscalItens[Counter].outrasDespesas;
        Prod.vDesc := DocumentoFiscalItens[Counter].desconto;

        TotalItem := RoundABNT((Prod.qCom * Prod.vUnCom), -2);
        TotalGeral := TotalGeral + TotalItem;
        Imposto.vItem12741 := TotalItem * (icmsAliquota/100);
        TotalImposto := TotalImposto + Imposto.vItem12741;

        with Imposto.ICMS do
        begin
          orig := StrToOrig(lOk, origemDamercadoria.codigo);
          if Emit.cRegTrib = RTSimplesNacional then
            CSOSN := StrToCSOSNIcms(lOk, cstICMS.codigo)
          else
            CST := StrToCSTICMS(lOk, cstICMS.codigo);

          pICMS := icmsAliquota;
          vICMS := icmsValor;
        end;

        with Imposto.PIS do
        begin
          CST       := StrToCSTPIS(lOk, cstPIS.codigo);
          vBC       := TotalItem;
          pPIS      := (pisAliquota / 100);
          vPIS      := pisValor;
        end;

        with Imposto.COFINS do
        begin
          CST      := StrToCSTCOFINS(lOk, CSTCOFINS.codigo);
          vBC      := TotalItem;
          pCOFINS  := (cofinsAliquota / 100);
          vCOFINS  := cofinsValor;
        end;

        infAdProd := '';
      end;
    end;
    TotalGeral := TotalGeral + DocumentoFiscal.outrasDespesas + DocumentoFiscal.frete;
    with sat.CFe.Total do
    begin
      DescAcrEntr.vAcresSubtot := DocumentoFiscal.frete;
      vCFe := TotalGeral - DocumentoFiscal.desconto;
      vCFeLei12741 := TotalImposto;
    end;

    for Counter := 0 to Length(DocumentoFiscalPagamentos) - 1 do
    begin
      with Pagto.New do
      begin
        cMP := StrToCodigoMP(lOk, DocumentoFiscalPagamentos[Counter].formaIndicador);
        vMP := DocumentoFiscalPagamentos[Counter].valor;
        if StrToCodigoMP(lOk, DocumentoFiscalPagamentos[Counter]   .formaIndicador) in [mpCartaodeCredito, mpCartaodeDebito] then
          cAdmC := StrToIntDef(DocumentoFiscalPagamentos[Counter].cartaoCredenciadora, 999);
      end;
      Pagto.vTroco := DocumentoFiscalPagamentos[Counter].troco;
    end;
    if DocumentoFiscal.frete > 0 then
      InfAdic.infCpl := 'Acréscimo sobre o subtotal referente a taxa de entrega;';
    InfAdic.infCpl := InfAdic.infCpl + 'Acesse constel.cloud para obter maiores;informações sobre o sistema Constel;';
    infAdic.infCpl := InfAdic.infCpl + Format('%s. Total %m.', [Trim(DocumentoFiscal.referencia), DocumentoFiscal.total]);

  end;

  result := String(sat.CFe.GerarXML(True));

end;

procedure Tcomponentes.GerarNFCe(const DocumentoFiscal: TDocumentoFiscal);
var
  lOk: Boolean;
  nCounter: Integer;
  vBaseDeCalculo,
  vTotalICMS,
  vTotalPIS,
  vTotalCOFINS,
  vTotalItens,
  vTotalDescontos: Double;
begin
  vBaseDeCalculo := 0;
  vTotalICMS := 0;
  vTotalPIS := 0;
  vTotalCOFINS := 0;
  vTotalItens := 0;
  vTotalDescontos := 0;

  nfe.NotasFiscais.Clear;

  carregaNFe(DocumentoFiscal.estabelecimento, '65');

  nfe.Configuracoes.WebServices.Ambiente := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.ambiente));

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
    Ide.dEmi      := Now;
    Ide.dSaiEnt   := DocumentoFiscal.saida;
    Ide.hSaiEnt   := Now;
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
      Emit.IE                 := String(RemoveStrings(AnsiString(estabelecimentoDocumentos[0].inscricaoEstadual), ['.', '-']));
      Emit.xNome              := nome;
      Emit.xFant              := nome;

      Emit.EnderEmit.fone     := '';
      Emit.EnderEmit.CEP      := StrToInt(String(RemoveStrings(AnsiString(estabelecimentoEnderecos[0].cep), ['.', '-'])));
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

      if FRespTec then
      begin
        infRespTec.CNPJ	        := '04528001000164';
        infRespTec.xContato	    := 'Myron Yerich P. Sales';
        infRespTec.email        := 'myron@solucaosistemas.net';
        infRespTec.fone	        := '8533076262';
      end;

    end;

    if Length(DocumentoFiscal.cpfInformado) > 0 then
    begin
      Dest.CNPJCPF            := DocumentoFiscal.cpfInformado;
      Ide.indFinal            := cfConsumidorFinal;
      Dest.indIEDest          := inNaoContribuinte;
    end;

    if Length(DocumentoFiscal.cnpjInformado) > 0 then
    begin
      Dest.CNPJCPF            := DocumentoFiscal.cnpjInformado;
      Ide.indFinal            := cfConsumidorFinal;
      Dest.indIEDest          := inNaoContribuinte;
    end;

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
        if Assigned(cest) then
          Prod.CEST := StringReplace(cest.codigo, '.', '', [rfReplaceAll]);
        Prod.uCom := unidade.codigo;
        Prod.qCom := quantidade;
        Prod.vUnCom := valor;
        Prod.cEANTrib := '';
        Prod.uTrib := unidade.codigo;
        Prod.qTrib := quantidade;
        Prod.vUnTrib := valor;
        Prod.vOutro := DocumentoFiscal.documentoFiscalItens[nCounter].outrasDespesas;
        Prod.vFrete := DocumentoFiscal.documentoFiscalItens[nCounter].frete;
        Prod.vSeg := 0;
        Prod.vDesc := DocumentoFiscal.DocumentoFiscalItens[nCounter].desconto;

        Prod.vProd := RoundABNT((Prod.qCom * Prod.vUnCom), -2);

        vTotalItens := vTotalItens + Prod.vProd;
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
              vBC := icmsBC;
              pICMS := IfThen((icmsAliquota >= 1), (icmsAliquota / 100), icmsAliquota);
            end
            else
            begin
              CSOSN := StrToCSOSNIcms(lOk, cstICMS.codigo);
              vBC := 0;
              pICMS := 0;
            end;

            vBC := icmsBC;
            pICMS := icmsAliquota;
            vICMS := vBC * (pICMS/100);


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

            vBaseDeCalculo := vBaseDeCalculo + icmsBC;


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
            vBC := pisBC;
            pPIS := pisAliquota;
            vPIS := vBC * (pPIS/100);

            qBCProd := 0;
            vAliqProd := 0;

            vTotalPIS := vTotalPIS + vPIS;
          end;

          with PISST do
          begin
            vBc       := pisSTBC;
            pPis      := pisSTAliquota;
            qBCProd   := 0;
            vAliqProd := 0;
            vPIS      := vBC * (pPis/100);
            IndSomaPISST :=  ispNenhum;
          end;

          with COFINS do
          begin
            CST := StrToCSTCOFINS(lOk, CSTCOFINS.codigo);
            vBC := cofinsBC;
            pCOFINS := cofinsAliquota;
            vCOFINS := vBC * (pCOFINS/100);

            qBCProd := 0;
            vAliqProd := 0;

            vTotalCOFINS := vTotalCOFINS + vCOFINS;
          end;

          with COFINSST do
          begin
            vBC             := cofinsSTBC;
            pCOFINS         := cofinsSTAliquota;
            qBCProd         := 0;
            vAliqProd       := 0;
            vCOFINS         := vBC * (pCOFINS/100);
            indSomaCOFINSST :=  iscNenhum;
          end;
        end;

        infAdProd := '';
      end;
    end;

    with Total.ICMSTot do
    begin
      vBC     := 0;
      vICMS   := 0;
      if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
      begin
        vBC   := vBaseDeCalculo;
        vICMS := vTotalICMS;
      end;

      vBCST   := 0;
      vST     := 0;
      vProd   := vTotalItens;
      vFrete  := DocumentoFiscal.frete;
      vSeg    := 0;
      vDesc   := vTotalDescontos;
      vII     := 0;
      vIPI    := 0;
      vPIS    := vTotalPIS;
      vCOFINS := vTotalCOFINS;
      vOutro  := DocumentoFiscal.outrasDespesas;
      vNF     := vTotalItens + + vOutro + vFrete - vTotalDescontos;

      vFCPUFDest   := 0.00;
      vICMSUFDest  := 0.00;
      vICMSUFRemet := 0.00;
    end;

    with Total.ISSQNtot do
    begin
      vServ   := 0;
      vBC     := 0;
      vISS    := 0;
      vPIS    := 0;
      vCOFINS := 0;
    end;

    with Total.retTrib do
    begin
      vRetPIS    := 0;
      vRetCOFINS := 0;
      vRetCSLL   := 0;
      vBCIRRF    := 0;
      vIRRF      := 0;
      vBCRetPrev := 0;
      vRetPrev   := 0;
    end;

    Transp.modFrete := mfSemFrete;

    for nCounter := 0 to Length(DocumentoFiscal.DocumentoFiscalPagamentos) - 1 do
    begin
      with pag.New do
      begin
        tPag            := StrToFormaPagamento(lOk, DocumentoFiscal.documentoFiscalPagamentos[nCounter].formaIndicador);
        if DocumentoFiscal.documentoFiscalPagamentos[nCounter].formaIndicador = '99' then
          xPag          := FormaPagamentoToDescricao(StrToFormaPagamento(lOk, DocumentoFiscal.documentoFiscalPagamentos[nCounter].formaIndicador), '');
        vPag            := DocumentoFiscal.DocumentoFiscalPagamentos[nCounter].valor;

        if StrToCodigoMP(lOk, DocumentoFiscal.documentoFiscalPagamentos[nCounter].formaIndicador) in [mpCartaodeCredito, mpCartaodeDebito, mpPagamentoInstantaneo] then
        begin
          if  (Length(Trim(DocumentoFiscal.documentoFiscalPagamentos[nCounter].cartaoAutorizacao)) > 0) then
          begin
            tpIntegra   := tiPagIntegrado;
            if not (StrToCodigoMP(lOk, DocumentoFiscal.documentoFiscalPagamentos[nCounter].formaIndicador) = mpPagamentoInstantaneo) then
            begin
              CNPJ      := ACBrUtil.Strings.OnlyNumber(DocumentoFiscal.documentoFiscalPagamentos[nCounter].cartaoCNPJ);
              tBand     := TpcnBandeiraCartao(StrToIntDef(DocumentoFiscal.documentoFiscalPagamentos[nCounter].cartaoCredenciadora, 27));
            end;
            cAut        := DocumentoFiscal.documentoFiscalPagamentos[nCounter].cartaoAutorizacao;
          end
          else
            tpIntegra   := tiPagNaoIntegrado;
        end;
      end;
      pag.vTroco        := DocumentoFiscal.documentoFiscalPagamentos[nCounter].troco;
    end;

    InfAdic.infCpl      :=  Format('%s. Total %m.', [DocumentoFiscal.referencia, DocumentoFiscal.total]);
    InfAdic.infAdFisco  :=  '';

  end;

  nfe.NotasFiscais.GerarNFe;
end;

function Tcomponentes.gerarSPED(Req: THorseRequest; var erros: string): TStringStream;
var
  fileStream: TFileStream;
  sped: TSped;
begin
  sped := TSped.Create;
  try
    var Estabelecimentos := InfoAPI().GetPagedArray<TEstabelecimentoC>(
      'agente/estabelecimento/sped?estabelecimentoid=' +
      Req.Query.Field('estabelecimentoid').AsString);

    var dataInicial  := Req.Query.Field('inicio').AsISO8601DateTime;
    var dataFinal    := Req.Query.Field('conclusao').AsISO8601DateTime;
    var finalidade   := Req.Query.Field('tipodearquivo').AsInteger;
    var semMovimento := Req.Query.Field('semmovimento').AsInteger;
    var inventario   := Req.Query.Field('inventariofiscal').AsString;
    var nomeArq      :  string;

    for var Estabelecimento in Estabelecimentos do
    begin
      sped.gerar(Estabelecimento, dataInicial, dataFinal, nomeArq, erros, finalidade, semMovimento, inventario);
      try
        fileStream := TFileStream.Create(nomeArq, fmOpenRead or fmShareDenyWrite);
      except
        On E: Exception do
          erros := E.Message;
      end;
    end;

    Result := TStringStream.Create;
    Result.LoadFromStream(fileStream);

  finally
    if Assigned(fileStream) then FreeAndNil(fileStream);
    if Assigned(sped) then FreeAndNil(sped);
  end;

end;

end.
