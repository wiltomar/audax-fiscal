unit Api.Componentes;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes, IniFiles, ACBrBase, ACBrSAT, Lib.Sistema.Tipos,
  ACBrDFeSSL, ACBrSATClass, pcnConversao, pcnConversaoNFe, Model.DocumentoFiscal, Model.Estabelecimento,
  pcnCFe, ACBrDFe, ACBrNFe, ACBrMail, ACBrUtil.Strings, ACBrUtil.Math, ACBrDFeUtil, ACBrNFeNotasFiscais,
  Api.Funcoes, System.Math, System.NetEncoding, ACBrNFeDANFeFPDF, ACBrSATExtratoClass, ACBrNFCeDANFeFPDF,
  System.IOUtils, Model.Config, Soap.EncdDecd, System.Generics.Collections, Lib.Funcoes, Web.HTTPApp,
  Model.Inutilizacao, ACBrSATExtratoFPDF, Horse, Model.Sped, APIService, Fortes.IRegistro, ACBr_fpdf_report,
  Xml.XMLDoc, Xml.XMLIntf, Xml.XMLDom, Model.DocumentoFiscalManifesto, Model.DocumentoFiscalCartaCorrecao,
  {$IFDEF MSWINDOWS}
    WinApi.ActiveX, ACBrSATExtratoESCPOS, ACBrPosPrinter, ACBrSATExtratoFortesFr,
  {$ENDIF}
  Lib.Sistema.DAO;

const
  docModelos: TArray<String> = ['55', '56', '57', '58', '59', '65'];
  build = '2025.8.24-2355.1';
  xPathComum = 'C:\Constel\Constel Fiscal';

type
  TArquivo = class
  private
    FGuid: string;
    FCaminho: string;
  public
    property Guid: string read fGuid write fGuid;
    property Caminho: string read fCaminho write fCaminho;
  end;

  TComponentes = class(TDataModule)
    Nfe: TACBrNFe;
    Sat: TACBrSAT;
    Mail: TACBrMail;
    procedure SatGetcodigoDeAtivacao(var Chave: AnsiString);
    procedure SatGetsignAC(var Chave: AnsiString);
    procedure DataModuleCreate(Sender: TObject);
  private
    FSatCodigoDeAtivacao: String;
    FSatAssinaturaAC: String;
    FRespTec: Boolean;
    FConfig: TConfig;

    {$IFDEF MSWINDOWS}
      FImpressora: TACBrPosPrinter;
    {$ENDIF}

    function InicializaSAT: Boolean;

    procedure CarregaNFe(estabelecimento: TEstabelecimentoC; formaDeEmissao: SmallInt; var Error, Msg: String; const modelo: string = '55'); overload;
    procedure CarregaCertificado(documentoFiscalSerie: TDocumentoFiscalSerie; var Error, Msg: String);
    procedure CarregaEmail;

    function CalculaTributos(const vProd: Double; const NCM: string): Double;
    function  GerarCFe(const DocumentoFiscal: TDocumentoFiscal): string;
    procedure GerarNFe(const DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String);
    procedure GerarNFCe(const DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String);

    function ConsultarNFe(var DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): Boolean;

    function CancelarDoc(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
    function CancelarCFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
  public
    function EmiteDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
    function CancelarDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
    function ImprimirDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;

    property SatCodigoDeAtivacao: String read FsatCodigoDeAtivacao write FsatCodigoDeAtivacao;
    property SatAssinaturaAC: String read FsatAssinaturaAC write FsatAssinaturaAC;

    function GerarSPED(Req: THorseRequest; var Error: string; var Msg: string): TStringStream;
    procedure GerarArquivoFortesFiscal(const FileName: String; Registros: TList<IRegistro>);
    function EnviaArquivo(const Arquivo: TAbstractWebRequestFile; var erros: string; var msg: string): TArquivo;
    function CartaDeCorrecao(DocumentoFiscalCartaCorrecao: TDocumentoFiscalCartaCorrecao; var Error, Msg: String): TDocumentoFiscalCartaCorrecao;
    function RecebeArquivo(Caminho: String; var FileName: string; var erros: string; var msg: string): TArquivo;
    function ManifestarDocumento(DocumentoFiscalManifesto: TDocumentoFiscalManifesto; var Error, Msg: String): TDocumentoFiscalManifesto;
  end;

var
  Componentes: TComponentes;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ Tcomponents }

function Tcomponentes.CartaDeCorrecao(DocumentoFiscalCartaCorrecao: TDocumentoFiscalCartaCorrecao; var Error, Msg: String): TDocumentoFiscalCartaCorrecao;
var
  lOk: Boolean;
  CodigoRetorno: Integer;
  Sequencia: Integer;
begin
  Msg := '';
  Error := '';

  carregaNFe(
    DocumentoFiscalCartaCorrecao.estabelecimento,
    1,
    Error,
    Msg,
    '55');

  try
    nfe.Configuracoes.WebServices.Ambiente := StrToTpAmb(lOk, IntToStr(DocumentoFiscalCartaCorrecao.estabelecimento.estabelecimentoFiscalSerie.ambiente));
    nfe.Configuracoes.WebServices.UF  := DocumentoFiscalCartaCorrecao.estabelecimento.estabelecimentoEnderecos[0].uf.sigla;

    nfe.EventoNFe.Evento.Clear;
    Sequencia := DocumentoFiscalCartaCorrecao.sequencia + 1;
    with nfe.EventoNFe.Evento.Add do
    begin
      InfEvento.chNFe               := DocumentoFiscalCartaCorrecao.chave;
      InfEvento.CNPJ                := DocumentoFiscalCartaCorrecao.estabelecimento.estabelecimentoDocumentos[0].documentoNumero;
      InfEvento.nSeqEvento          := Sequencia;
      InfEvento.detEvento.xCorrecao := Trim(UpperCase(DocumentoFiscalCartaCorrecao.correcao));
      InfEvento.dhEvento            := Now;
      InfEvento.tpEvento            := teCCe;
    end;

    var retorno := nfe.EnviarEvento(Sequencia);

    if nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat = 573 then
      Error := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo
    else
    if nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat = 135 then
    begin
      DocumentoFiscalCartaCorrecao.status := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat;
      DocumentoFiscalCartaCorrecao.protocolo := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt;
      DocumentoFiscalCartaCorrecao.recebimento := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento;
      DocumentoFiscalCartaCorrecao.sequencia := DocumentoFiscalCartaCorrecao.sequencia;

      Msg := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
    end;
    Result := DocumentoFiscalCartaCorrecao;
  except
    on E: Exception do
    begin
      CodigoRetorno := 0;
      Error := 'Erro ao emitir carta de correção: ' + E.Message;
    end;
  end;
end;

function Tcomponentes.ConsultarNFe(var DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): Boolean;
var
  lOk: Boolean;
begin
  try
    nfe.WebServices.Consulta.NFeChave := DocumentoFiscal.documentoFiscalNFe.chave;
    lOk := nfe.WebServices.Consulta.Executar;
    Msg := nfe.WebServices.Consulta.XMotivo;

    Result := lOk;
  except
    on E:Exception do
    begin
      Error := Format('Houve um erro na tentativa de consulta: %s.', [E.Message]);
      Result := False;
    end;
  end;
end;

procedure TComponentes.DataModuleCreate(Sender: TObject);
  procedure ChecarPastas;
  var
    Lista: TStringList;
  begin
    Lista := TStringList.Create;
    try
      Lista.Add(xPathComum + '\arquivos\documentos\nfe\eventos');
      Lista.Add(xPathComum + '\arquivos\documentos\nfe\envio');
      Lista.Add(xPathComum + '\arquivos\documentos\nfe\inutilizacoes');
      Lista.Add(xPathComum + '\arquivos\documentos\nfe\notas');
      Lista.Add(xPathComum + '\arquivos\documentos\nfe\pdf');

      Lista.Add(xPathComum + '\arquivos\documentos\cfe\pdf');

      Lista.Add(xPathComum + '\arquivos\estabelecimentos');
      Lista.Add(xPathComum + '\arquivos\schemas\nfe');
      Lista.Add(xPathComum + '\arquivos\schemas\cfe');

      Lista.Add(xPathComum + '\arquivos\texto\sped');

      for var Dir in Lista do
      begin
        try
          if not TDirectory.Exists(Dir) then
          begin
            TDirectory.CreateDirectory(Dir);
          end;
        except
          on E: Exception do
        end;
      end;
    finally
      Lista.Free;
    end;
  end;
begin
  {$IFDEF MSWINDOWS}
    RemoveDataModule(self);
  {$ENDIF}
  ChecarPastas;
end;

function TComponentes.ImprimirDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
  procedure EmailDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String);
  var
    CC: Tstrings;
    XmlDocumento: String;
    MmEmailMsg: TStringList;
  begin
    CC := TStringList.Create;

    MmEmailMsg := TStringList.Create;
    MmEmailMsg.Add('Segue documento fiscal eletr�nico referente a ');
    MmEmailMsg.Add('sua compra realizada conosco no dia ' + FormatDateTime('dd/mm/yyyy', DocumentoFiscal.emissao));

    try
      case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
        0, 5:
        begin
          var Danfe: TACBrNFeDANFeFPDF;
          Danfe := TACBrNFeDANFeFPDF.Create(nil);

          with Danfe do
          begin
            Sistema := 'Audax Constel';
            Site    := 'https://constel.cloud';
          end;

          Nfe.DANFE := Danfe;
          Nfe.NotasFiscais.Clear;

          {$IFDEF MSWINDOWS}
            CoInitialize(nil);
          {$ENDIF}

          XmlDocumento := DocumentoFiscal.documentoFiscalNFe.xml;
          if Nfe.NotasFiscais.LoadFromString(xmlDocumento) then
          begin
            MmEmailMsg.Add('de n�mero ' + IntToStr(DocumentoFiscal.documentoFiscalNFe.numero));

            CarregaEmail;
            Nfe.NotasFiscais.Items[0].EnviarEmail(
              DocumentoFiscal.email,
              'Constel Docs [Documento Fiscal n� ' + IntToStr(DocumentoFiscal.documentoFiscalNFe.numero) + ']',
              TStrings(mmEmailMsg),
              True,
              CC,
              nil
            );
          end;
        end;
        4:
        begin
          XmlDocumento := DocumentoFiscal.documentoFiscalCFe.xml;
          Sat.CFe.SetXMLString(AnsiString(xmlDocumento));

          MmEmailMsg.Add('de n�mero ' + IntToStr(DocumentoFiscal.documentoFiscalCFe.numero));

          CarregaEmail;
          Sat.EnviarEmail(
            DocumentoFiscal.email,
            'Constel Docs [Cupom Fiscal n� ' + IntToStr(DocumentoFiscal.documentoFiscalCFe.numero) + ']',
            TStrings(mmEmailMsg),
            CC,
            nil
          );
        end;
      end;
    finally
      CC.Free;
      MmEmailMsg.Free;
    end;

  end;

var
  xmlDocumento, documento: String;
  stream: TMemoryStream;
begin
  {$IFDEF MSWINDOWS}
    CoInitialize(nil);
  {$ENDIF}

  try
    case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
      0, 5:
        begin
          if (DocumentoFiscal.documentoFiscalNFe.chave > '') and ((DocumentoFiscal.documentoFiscalNFe.status = 100) and (DocumentoFiscal.documentoFiscalNFe.protocolo > '') or
             (DocumentoFiscal.documentoFiscalNFe.status = 1000)) then
          begin
            xmlDocumento := DocumentoFiscal.documentoFiscalNFe.xml;
            case IntToTObjetivo(DocumentoFiscal.objetivo) of
              toNenhum: ;
              toImpressao:
                begin
                  var nfe: TACBrNFe;
                  var danfe: TACBrNFeDANFeFPDF;
                  var danfce: TACBrNFCeDANFeFPDF;
                  var empresaLogo: string := '';

                  if Assigned(documentoFiscal.empresa) and (documentoFiscal.empresa.imagem <> '') then
                    if not(TDAO.GetArquivo(documentoFiscal.Empresa.nome, documentoFiscal.Empresa.imagem, empresaLogo)) then
                      empresaLogo := '';

                  nfe := TACBrNFe.Create(nil);
                  nfe.NotasFiscais.Clear;

                  if DocumentoFiscal.modelo = '65' then
                  begin
                    danfce := TACBrNFCeDANFeFPDF.Create(Self);
                    danfce.TipoDANFE := tiNFCe;

                    nfe.DANFE := danfce;
                  end
                  else
                  begin
                    danfe := TACBrNFeDANFeFPDF.Create(Self);
                    danfe.TipoDANFE := tiRetrato;
                    nfe.DANFE := danfe;
                  end;

                  try
                    with nfe.DANFE do
                    begin
                      Sistema := 'Audax Constel';
                      Site    := 'https://constel.cloud';
                      Logo    := empresaLogo;
                    end;

                    if nfe.NotasFiscais.LoadFromString(xmlDocumento) then
                    begin
                      nfe.DANFE.PathPDF := xPathComum + '\arquivos\documentos\nfe\pdf';
                      nfe.DANFE.ImprimirDANFE;
                    end;

                  finally
                    if Assigned(danfe) then danfe.Free;
                    if Assigned(danfce) then danfce.Free;
                    nfe.Free;
                  end;
                end;
              toEmail:
                EmailDFe(DocumentoFiscal, Error, Msg);
              toBase64:
                begin
                  var nfe: TACBrNFe;
                  nfe := TACBrNFe.Create(nil);
                  var empresaLogo: string := '';

                  if Assigned(documentoFiscal.empresa) and (documentoFiscal.empresa.imagem <> '') then
                    if not(TDAO.GetArquivo(documentoFiscal.Empresa.nome, documentoFiscal.Empresa.imagem, empresaLogo)) then
                      empresaLogo := '';
                  try
                    nfe.NotasFiscais.Clear;

                    if DocumentoFiscal.modelo = '65' then
                    begin
                      var danfe: TACBrNFCeDANFeFPDF;
                      danfe := TACBrNFCeDANFeFPDF.Create(Self);

                      danfe.TipoDANFE := tiNFCe;

                      nfe.DANFE := danfe;
                    end
                    else
                    begin
                      var danfe: TACBrNFeDANFeFPDF;
                      danfe := TACBrNFeDANFeFPDF.Create(Self);
                      nfe.DANFE := danfe;
                    end;

                    if nfe.NotasFiscais.LoadFromString(xmlDocumento) then
                    begin
                      var pastaPDF: string;
                      pastaPDF := xPathComum + '\arquivos\documentos\nfe\pdf';

                      with nfe.DANFE do
                      begin
                        Sistema := 'Audax Constel';
                        Site    := 'https://constel.cloud';
                        NomeDocumento := nfe.NotasFiscais.Items[0].NumID + '.pdf';
                        ExpandeLogoMarca := False;
                        Logo    := empresaLogo;

                        MostraPreview  := False;
                        MostraSetup    := False;
                        MostraStatus   := False;
                      end;

                      nfe.DANFE.PathPDF := pastaPDF;

                      stream := TMemoryStream.Create;
                      try
                        nfe.DANFE.ImprimirDANFEPDF;
                        stream.LoadFromFile(nfe.DANFE.ArquivoPDF);
                        documento := StringReplace(String(EncodeBase64(stream.Memory, stream.Size)), #13#10, '', [rfReplaceAll]);
                        documentoFiscal.imagem := AnsiString(documento);
                        documentoFiscal.documentoFiscalNFe.imagem := documento;
                      finally
                        if FileExists(nfe.DANFE.ArquivoPDF) then
                          DeleteFile(PWideChar(nfe.DANFE.ArquivoPDF));
                        stream.Free;
                      end;
                    end;

                  finally
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

              with Extrato do
              begin
                Sistema := 'Audax Constel';
                Site    := 'https://constel.cloud';
              end;

              sat.ImprimirExtrato;
            {$ELSE}
              //
            {$ENDIF}
            end;
          toEmail:
            EmailDFe(DocumentoFiscal, Error, Msg);
          toBase64:
            begin
              {$IFDEF MSWINDOWS}
                var Extrato := TACBrSATExtratoFortes.Create(Self);
              {$ELSE}
                var Extrato := TACBrSATExtratoFPDF.Create(Self);
              {$ENDIF}
              Extrato.PathPDF := xPathComum + '/arquivos/documentos/cfe/pdf';

              with Extrato do
              begin
                Sistema := 'Audax Constel';
                Site    := 'https://constel.cloud';
                NomeDocumento := IntToStr(sat.CFe.ide.nCFe) + '.pdf';
                Filtro := TACBrSATExtratoFiltro.fiPDF;
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

    if Msg = EmptyStr then
      Msg := 'Documento fiscal impresso com sucesso.';
    Result := documentoFiscal;

  except
    on E: Exception do
      Error := E.Message;
  end;


end;

function TComponentes.EmiteDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
var
  xmlDocumento: string;
  documentoFiscalImpresso: TDocumentoFiscal;

  procedure ProcessaNFe;
  begin
    if nfe.NotasFiscais.Count > 0 then
    begin
      if not(nfe.Configuracoes.Geral.FormaEmissao = teOffLine) or (documentoFiscal.documentoFiscalNFe.status = 1001) then
      begin
        nfe.Enviar(DocumentoFiscal.documentoFiscalNFe.numero, False, True);

        DocumentoFiscal.documentoFiscalNFe.status     := nfe.WebServices.Enviar.cStat;
        DocumentoFiscal.documentoFiscalNFe.msgRetorno := nfe.WebServices.Enviar.Msg;
        DocumentoFiscal.documentoFiscalNFe.chave      := nfe.NotasFiscais.Items[0].NumID;

        if (nfe.WebServices.Enviar.cStat = 100) and not(nfe.WebServices.Enviar.Protocolo = EmptyStr) then
        begin
          DocumentoFiscal.documentoFiscalNFe.xml := nfe.NotasFiscais.Items[0].XMLAssinado;
          DocumentoFiscal.documentoFiscalNFe.protocolo := nfe.WebServices.Enviar.Protocolo;

          Msg := Format('Documento fiscal de numero: %d com chave: %s, %s.', [
                        nfe.NotasFiscais.Items[0].NFe.Ide.nNF,
                        nfe.NotasFiscais.Items[0].NumID,
                        nfe.WebServices.Enviar.Msg]);

          documentoFiscalImpresso := ImprimirDFe(DocumentoFiscal, Error, Msg);
        end
        else
        begin
          Error := Format('Nao foi possivel emitir o documento fiscal, o seguinte erro ocorreu: %s.', [nfe.WebServices.Retorno.Msg]);
          documentoFiscalImpresso := DocumentoFiscal;
        end;
      end
      else
      begin
        Msg := 'Documento fiscal impresso em contingência (OffLine), favor enviar para a SEFAZ';
        DocumentoFiscal.documentoFiscalNFe.status     := 1000;
        DocumentoFiscal.documentoFiscalNFe.chave      := nfe.NotasFiscais.Items[0].NumID;
        DocumentoFiscal.documentoFiscalNFe.msgRetorno := Msg;
        DocumentoFiscal.documentoFiscalNFe.xml := nfe.NotasFiscais.Items[0].XMLAssinado;

        documentoFiscalImpresso := ImprimirDFe(DocumentoFiscal, Error, Msg);
      end;
    end;
  end;
begin
  try
    documentoFiscalImpresso := nil;
    if not Assigned(DocumentoFiscal.estabelecimento.estabelecimentoFiscal) then
    begin
      Error := 'Estabelecimento fiscal não configurado.';
      Result := DocumentoFiscal;
      Exit;
    end;
    case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
      0: {$REGION 'NFe'}
        begin
          GerarNFe(DocumentoFiscal, Error, Msg);
          if not(DocumentoFiscal.documentoFiscalNFe.chave = '') then
          begin
            var XMLAssinado := nfe.NotasFiscais.Items[0].XMLAssinado;
            ConsultarNFe(DocumentoFiscal, Error, Msg);

            if nfe.WebServices.Consulta.cStat = 613 then
            begin
              Error := 'Documento fiscal emitido por outro aplicativo: ' + nfe.WebServices.Consulta.Msg;
              Result := documentoFiscal;
              Exit;
            end;

            if nfe.WebServices.Consulta.cStat = 539 then
            begin
              DocumentoFiscal.documentoFiscalNFe.chave := ACBrDFeUtil.ExtrairChaveMsg(nfe.WebServices.Consulta.Msg);
              ConsultarNFe(DocumentoFiscal, Error, Msg);
            end;

            if (nfe.WebServices.Consulta.cStat = 100) or (nfe.WebServices.Consulta.cStat = 150) then
            begin
              DocumentoFiscal.documentoFiscalNFe.status := IfThen(DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono, nfe.WebServices.Consulta.cStat, nfe.WebServices.Retorno.cStat);
              DocumentoFiscal.documentoFiscalNFe.msgRetorno := IfThen(DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono, nfe.WebServices.Consulta.Msg, nfe.WebServices.Retorno.Msg);

              if not(IfThen(DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono, nfe.WebServices.Consulta.Protocolo, nfe.WebServices.Retorno.Protocolo) = EmptyStr) then
              begin
                DocumentoFiscal.documentoFiscalNFe.chave := nfe.WebServices.Consulta.NFeChave;
                DocumentoFiscal.documentoFiscalNFe.xml := XMLAssinado;
                DocumentoFiscal.documentoFiscalNFe.protocolo := nfe.WebServices.Consulta.protNFe.nProt;

                Msg := Format('Documento fiscal de numero: %d com chave: %s, %s.', [
                              DocumentoFiscal.documentoFiscalNFe.numero,
                              DocumentoFiscal.documentoFiscalNFe.chave,
                              IfThen(DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono, nfe.WebServices.Consulta.Msg, nfe.WebServices.Retorno.Msg)]);

                documentoFiscalImpresso := ImprimirDFe(DocumentoFiscal, Error, Msg);
              end
              else
              begin
                Error := Format('Não foi possível emitir o documento fiscal, o seguinte erro ocorreu: %s.', [nfe.WebServices.Consulta.Msg]);
                documentoFiscalImpresso := DocumentoFiscal;
              end;
            end
            else
              ProcessaNFe;
          end
          else
            ProcessaNFe;
        end;
        {$ENDREGION 'NFe'}
      4: {$REGION 'CFe'}
        begin
          xmlDocumento := GerarCFe(DocumentoFiscal);

          var erro: string;

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

            Msg := Format('Documento fiscal de numero: %d com chave: %s, %s.', [
                          sat.CFe.ide.nCFe,
                          sat.CFe.infCFe.ID,
                          sat.Resposta.mensagemRetorno]);

            documentoFiscalImpresso := ImprimirDFe(DocumentoFiscal, erro, Msg);
          end
          else
          begin
            Error := Format('Não foi possivel emitir o cupom fiscal, o seguinte erro ocorreu: %s.', [sat.Resposta.mensagemRetorno]);
            documentoFiscalImpresso := DocumentoFiscal;
          end;
        end;
        {$ENDREGION 'CFe'}
      5: {$REGION 'NCFe'}
        begin
          GerarNFCe(DocumentoFiscal, Error, Msg);
          if not(DocumentoFiscal.documentoFiscalNFe.formaDeEmissao = 9) then
            ConsultarNFe(DocumentoFiscal, Error, Msg);

          if (nfe.WebServices.Consulta.cStat = 539) or (nfe.WebServices.Consulta.cStat = 613) then
          begin
            DocumentoFiscal.documentoFiscalNFe.chave := ACBrDFeUtil.ExtrairChaveMsg(nfe.WebServices.Consulta.Msg);
            if not(DocumentoFiscal.documentoFiscalNFe.formaDeEmissao = 9) then
              ConsultarNFe(DocumentoFiscal, Error, Msg);
          end;

          if (nfe.WebServices.Consulta.cStat = 100) or (nfe.WebServices.Consulta.cStat = 150) then
          begin
            DocumentoFiscal.documentoFiscalNFe.status := nfe.WebServices.Consulta.cStat;
            DocumentoFiscal.documentoFiscalNFe.msgRetorno := nfe.WebServices.Consulta.Msg;

            if not(nfe.WebServices.Consulta.Protocolo = EmptyStr) then
            begin
              DocumentoFiscal.documentoFiscalNFe.chave := nfe.WebServices.Consulta.NFeChave;
              DocumentoFiscal.documentoFiscalNFe.xml := nfe.NotasFiscais.Items[0].XML;
              DocumentoFiscal.documentoFiscalNFe.protocolo := nfe.WebServices.Consulta.protNFe.nProt;

              Msg := Format('Documento fiscal de numero: %d com chave: %s, %s.', [
                            DocumentoFiscal.documentoFiscalNFe.numero,
                            DocumentoFiscal.documentoFiscalNFe.chave,
                            IfThen(DocumentoFiscal.estabelecimento.estabelecimentoFiscal.sincrono, nfe.WebServices.Consulta.Msg, nfe.WebServices.Retorno.Msg)]);

              documentoFiscalImpresso := ImprimirDFe(DocumentoFiscal, Error, Msg);
            end
            else
            begin
              Error := Format('Não foi possível emitir o documento fiscal, o seguinte erro ocorreu: %s.', [nfe.WebServices.Consulta.Msg]);
              documentoFiscalImpresso := DocumentoFiscal;
            end;
          end
          else
          begin
            ProcessaNFe;
          end;
        end
        else
          ProcessaNFe;
        {$ENDREGION 'NFCe'}
    end;

    Result := documentoFiscalImpresso;

  except
    on E: Exception do
    begin
      if Assigned(documentoFiscalImpresso) then
      begin
        Error  := Format('Houve a seguinte falha na tentativa de emissão do documento fiscal: %s.', [nfe.WebServices.Retorno.Msg]);
        Result := documentoFiscalImpresso;
      end
      else
      begin
        Error  := Format('Houve um erro interno na API: %s.', [E.Message]);
        Result := documentoFiscal;
      end;
    end;
  end;
end;

function TComponentes.enviaArquivo(const Arquivo: TAbstractWebRequestFile; var erros: string; var msg: string): TArquivo;
var
  lStream: TMemoryStream;
  Caminho, ArquivoGUID: string;
begin
  erros := '';
  msg := '';
  Caminho := TPath.GetAppPath + '/arquivos/estabelecimentos';
  ForceDirectories(Caminho);

  lStream := TMemoryStream.Create;
  var fArquivo: TArquivo;
  fArquivo := TArquivo.Create;
  try
    lStream.LoadFromStream(Arquivo.Stream);
    lStream.Position := 0;

    ArquivoGUID := NewGUID.ToLower;
    var nomeArquivo := Caminho + '/' + ArquivoGUID + ExtractFileExt(Arquivo.FileName);
    lStream.SaveToFile(nomeArquivo);

    fArquivo.GUID := ArquivoGUID;
    fArquivo.Caminho := nomeArquivo;
    msg := 'Arquivo ' + Arquivo.FileName + ' enviado com sucesso!';
  except
    On E:Exception do
    begin
      erros := 'Ocorreu o seguinte erro ao processar o arquivo: ' + E.Message;
    end;
  end;
  Result := fArquivo;
end;

function TComponentes.recebeArquivo(Caminho: String; var FileName: string; var erros: string; var msg: string): TArquivo;
var
  lStream: TMemoryStream;
  ArquivoNome, ArquivoURL: string;
begin
  var fArquivo: TArquivo;
  fArquivo := TArquivo.Create;
  try
    InfoAPI().GetArquivo('', Caminho, FileName);
    fArquivo.Guid    := FileName;
    fArquivo.Caminho := Caminho;
  except

  end;
  Result := fArquivo;
end;

procedure TComponentes.gerarArquivoFortesFiscal(const FileName: String; Registros: TList<IRegistro>);
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

function TComponentes.CancelarDFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
begin
  var DocumentoFiscalCancelado: TDocumentoFiscal := nil;
  case AnsiIndexStr(DocumentoFiscal.modelo, docModelos) of
    0, 5: DocumentoFiscalCancelado := CancelarDoc(DocumentoFiscal, Error, Msg);
    4: DocumentoFiscalCancelado := CancelarCFe(DocumentoFiscal, Error, Msg);
  end;
  Result := DocumentofiscalCancelado;
end;

function TComponentes.CancelarDoc(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
var
  lOk: Boolean;
begin
  Error := '';
  Msg := '';
  try
    nfe.NotasFiscais.Clear;
    CarregaNFe(
      DocumentoFiscal.estabelecimento,
      DocumentoFiscal.documentoFiscalNFe.formaDeEmissao,
      Error,
      Msg,
      DocumentoFiscal.modelo);

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
        if Length(DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa) > 15 then
          infEvento.detEvento.xJust := DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa
        else
          infEvento.detEvento.xJust := 'Operação de ' + DocumentoFiscal.referencia + ' foi extornada.';
      end;

      nfe.EnviarEvento(DocumentoFiscal.documentoFiscalNFe.numero);

      if (nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat = 135) or
         (nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat = 573) then
      begin
        DocumentoFiscal.documentoFiscalNFe.status                     := 135;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoProtocolo      := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.nProt;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoData           := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa  := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;

        if nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat = 573 then
          Msg := Format('Documento fiscal: %d com chave: %s, já cancelado anteriormente.', [
                        DocumentoFiscal.documentoFiscalNFe.numero,
                        DocumentoFiscal.documentoFiscalNFe.chave])
        else
          Msg := Format('Documento fiscal: %d com chave: %s, cancelado com sucesso.', [
                        DocumentoFiscal.documentoFiscalNFe.numero,
                        DocumentoFiscal.documentoFiscalNFe.chave]);
      end
      else
      begin
        DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
        Error := Format('Não foi possivel cancelar o documento fiscal, o seguinte erro ocorreu: %s.', [nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo]);
      end;
    end
    else
    begin
      if nfe.WebServices.Consulta.cStat = 101 then
      begin
        DocumentoFiscal.documentoFiscalNFe.status                     := nfe.WebServices.Consulta.cStat;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoProtocolo      := nfe.WebServices.Consulta.retCancNFe.nProt;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoData           := nfe.WebServices.Consulta.retCancNFe.dhRecbto;
        DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa  := DocumentoFiscal.documentoFiscalNFe.cancelamentoJustificativa;

        Msg := Format('Documento fiscal: %d com chave: %s, já cancelado anteriormente.', [
                      DocumentoFiscal.documentoFiscalNFe.numero,
                      DocumentoFiscal.documentoFiscalNFe.chave]);

      end;
    end;

    Result := DocumentoFiscal;

  except
    on E: Exception do
    begin
      Error := Format('Houve um erro na tentativa de cancelar o documento. Verifique a mensagem a seguir: %s.', [E.Message]);
      Result := DocumentoFiscal;
    end;
  end;
end;

function TComponentes.CalculaTributos(const vProd: Double;
  const NCM: string): Double;
begin
  // Implementar rotina de calculo, depois.
  Result := 0.00;
end;

function TComponentes.CancelarCFe(DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String): TDocumentoFiscal;
begin
  inicializaSAT;
  sat.InicializaCFe;
  var DocumentoFiscalCancelado: TDocumentoFiscal := nil;

  Error := '';
  Msg := '';

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

      Msg := Format('Cupom fiscal: %d com chave: %s, cancelado com sucesso.', [
                    sat.CFe.ide.nCFe,
                    sat.CFe.infCFe.ID]);
      DocumentoFiscalCancelado := DocumentoFiscal;

    end
    else
      Error := Format('Não foi possivel cancelar o cupom fiscal, o seguinte erro ocorreu: %s.', [
                      sat.Resposta.mensagemRetorno]);

  except
    on E: Exception do
      Error := Format('Houve um erro na tentativa de inicializar o equipamento MFe. Verifique a mensagem a seguir: %s.', [E.Message]);
  end;
  Result := DocumentoFiscalCancelado;
end;

function TComponentes.inicializaSAT: boolean;
  procedure carregaSAT;
  begin
    try
{      with sat do
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
      end; }

    except
      on E:Exception do
      begin
        Log(Format('Impossivel carregar o SAT. Verificar com suporte, o erro %s.', [E.Message]));
      end;
    end;
  end;
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

procedure TComponentes.carregaCertificado(documentoFiscalSerie: TDocumentoFiscalSerie; var Error, Msg: String);
begin
  nfe.SSL.DescarregarCertificado;
  if (documentoFiscalSerie.certificadopfx = '') and (documentoFiscalSerie.certificadonumerodeserie = '') and (documentoFiscalSerie.certificadourl = '') then
  begin
    Error := 'Erro ao tentar carregar o certificado digital. Não foi informado o caminho, número de série ou url válida.';
    Exit;
  end;

  with nfe.SSL do
  begin
    ArquivoPFX              := documentoFiscalSerie.certificadopfx;
    URLPFX                  := documentoFiscalSerie.certificadourl;
    Senha                   := AnsiString(documentoFiscalSerie.certificadosenha);
    NumeroSerie             := documentoFiscalSerie.certificadonumerodeserie;
  end;

  with nfe.Configuracoes.Certificados do
  begin
    ArquivoPFX              := documentoFiscalSerie.certificadopfx;
    URLPFX                  := documentoFiscalSerie.certificadourl;
    Senha                   := AnsiString(documentoFiscalSerie.certificadosenha);
    NumeroSerie             := documentoFiscalSerie.certificadonumerodeserie;
  end;

  if not(nfe.Configuracoes.Certificados.VerificarValidade) then
  begin
    Error := Format('Erro de certificado. Certificado vencido em %s.', [FormatDateTime('dd/mm/yyyy', nfe.ssl.CertDataVenc)]);
    Exit;
  end;

end;

procedure TComponentes.carregaEmail;
begin
{  if Assigned(FConfig) then FConfig := nil;

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
  end;}
end;

procedure TComponentes.CarregaNFe(estabelecimento: TEstabelecimentoC;
  formaDeEmissao: SmallInt; var Error, Msg: String; const modelo: string);
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
      FormaEmissao            := StrToTpEmis(lOk, IntToStr(formaDeEmissao));
      VersaoDF                := StrToVersaoDF(lOk, estabelecimentoFiscalSerie.versaodf);
      ModeloDF                := StrToModeloDF(lOk, modelo);
      FormatoAlerta           := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.';

      if modelo = '65' then
      begin
        IdCSC                 := IntToStr(estabelecimentoFiscalSerie.nfceidcsc);
        CSC                   := estabelecimentoFiscalSerie.nfcecsc;
        VersaoQRCode          := veqr200;
      end;
    end;

    with nfe.Configuracoes.Arquivos do
    begin
      Salvar                  := True;
      SepararPorMes           := True;
      AdicionarLiteral        := True;
      EmissaoPathNFe          := True;
      SalvarEvento            := True;
      SepararPorCNPJ          := True;
      SepararPorModelo        := True;
      PathSchemas             := xPathComum + '\arquivos\schemas\nfe';
      PathNFe                 := xPathComum + '\arquivos\documentos\nfe\notas';
      PathInu                 := xPathComum + '\arquivos\documentos\nfe\inutilizacoes';
      PathEvento              := xPathComum + '\arquivos\documentos\nfe\eventos';
      PathSalvar              := xPathComum + '\arquivos\documentos\nfe\envio';
    end;

    carregaCertificado(estabelecimento.estabelecimentoFiscalSerie, Error, Msg);

    with nfe.Configuracoes.WebServices do
    begin
      Visualizar              := False;
      Salvar                  := True;
      AjustaAguardaConsultaRet:= True;
      AguardarConsultaRet     := 10000;
      Tentativas              := 3;
      IntervaloTentativas     := 10000;
      TimeOut                 := 120000;
      ProxyHost               := '';
      ProxyPort               := '';
      ProxyUser               := '';
      ProxyPass               := '';
    end;

  except
    on E: Exception do
      Error := Format('Ocorreu o seguinte erro ao carregar a configuração: %s.', [E.Message]);
  end;
end;

procedure TComponentes.satGetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := AnsiString(FsatCodigoDeAtivacao);
end;

procedure TComponentes.satGetsignAC(var Chave: AnsiString);
begin
  Chave := AnsiString(FsatAssinaturaAC);
end;

procedure TComponentes.GerarNFe(const DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String);
var
  lOk: Boolean;
  NotaF: NotaFiscal;
  vBaseDeCalculo,
  vTotalICMS,
  vBaseDeCalculoICMSST,
  vTotalICMSST,
  vTotalPIS,
  vTotalCOFINS,
  vTotalItens,
  vTotalFCPST,
  vTotalIPI,
  vTotalII,
  vTotalIPIDevol,
  vTotalOutrasDespesas,
  vTotalDescontos,
  vTotalTributos: Double;
  Count: TNFe;
begin
  vBaseDeCalculo := 0;
  vTotalICMS := 0;
  vBaseDeCalculoICMSST  := 0;
  vTotalICMSST := 0;
  vTotalPIS := 0;
  vTotalCOFINS := 0;
  vTotalIPI := 0;
  vTotalIPIDevol := 0;
  vTotalII := 0;
  vTotalItens := 0;
  vTotalFCPST := 0;
  vTotalOutrasDespesas := 0;
  vTotalDescontos := 0;
  vTotalTributos := 0;

  nfe.NotasFiscais.Clear;

  CarregaNFe(
    DocumentoFiscal.estabelecimento,
    DocumentoFiscal.documentoFiscalNFe.formaDeEmissao,
    Error,
    Msg,
    DocumentoFiscal.modelo);

  nfe.Configuracoes.WebServices.UF       := DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla;
  nfe.Configuracoes.WebServices.Ambiente := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.estabelecimento.estabelecimentoFiscalSerie.ambiente));

  NotaF := nfe.NotasFiscais.Add;
  with NotaF.NFe, DocumentoFiscal do
  begin
    Ide.natOp       := historico.nome;
    Ide.modelo      := StrToIntDef(DocumentoFiscal.modelo, 55);
    Ide.serie       := documentoFiscalNFe.serie;
    Ide.nNF         := documentoFiscalNFe.numero;
    if not(DocumentoFiscal.documentoFiscalNFe.chave = '') then
    begin
      infNFe.ID       := 'NFe' + documentoFiscalNFe.chave;
      Ide.cNF         := ExtrairCodigoChaveAcesso(infNFe.ID);
      Ide.cDV         := ExtrairDigitoChaveAcesso(infNFe.ID);
    end;
    Ide.dEmi        := emissao;
    Ide.dSaiEnt     := saida;
    Ide.hSaiEnt     := saida;
    Ide.tpNF        := tnSaida;
    Ide.tpEmis      := StrToTpEmis(lOk, IntToStr(DocumentoFiscal.documentoFiscalNFe.formadeemissao));
    Ide.tpAmb       := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.estabelecimento.estabelecimentoFiscalSerie.ambiente));
    Ide.verProc     := build;
    Ide.cUF         := UFtoCUF(estabelecimento.estabelecimentoEnderecos[0].uf.sigla);
    Ide.cMunFG      := StrToInt(estabelecimento.estabelecimentoEnderecos[0].municipio.codigo);
    Ide.finNFe      := StrToFinNFe(lOk, IntToStr(documentoFiscalNFe.finalidadeEmissao));
    Ide.indIntermed := TindIntermed(documentoFiscalNFe.indicadorIntermediador);

    if Length(DocumentoFiscal.parceiro.parceiroDocumentos[0].documentoNumero) > 11 then
      Ide.indFinal  := cfNao
    else
      Ide.IndFinal  := cfConsumidorFinal;

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

    case nfe.Configuracoes.Geral.FormaEmissao of
      teOffLine: begin
        Ide.dhCont := date;
        Ide.xJust  := 'Problemas com a internet';
      end;
      teContingencia: begin
        Ide.dhCont := date;
        Ide.xJust  := 'Modo de contingência ativado, problema com a SEFAZ';
      end;
      teSVCRS: begin
        Ide.dhCont := date;
        Ide.xJust  := 'Contingência SVCRS habilitado por problemas com a SEFAZ';
      end;
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

    if not(estabelecimento.estabelecimentoFiscal.cnpjaut = EmptyStr) then
      autXML.New.CNPJCPF := estabelecimento.estabelecimentoFiscal.cnpjaut;

    if Assigned(estabelecimento.estabelecimentoFiscal.cnpjauts)  then
      for var I := 0 to High(estabelecimento.estabelecimentofiscal.cnpjauts) do
        with autXML.New do
          CNPJCPF := estabelecimento.estabelecimentoFiscal.cnpjauts[I];

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

    for var refCont := 0 to Length(documentoFiscalNFe.documentoFiscalNFeReferencia) - 1 do
    begin
      with NotaF.NFe.Ide.NFref.Add, documentoFiscalNFe.documentoFiscalNFeReferencia[refCont] do
      begin
        refNFe := documentoFiscalChave;
      end;
    end;

    for var nCont := 0 to Length(DocumentoFiscalItens) - 1 do
    begin
      with Det.New, DocumentoFiscalItens[nCont] do
      begin
        Prod.nItem := nCont + 1;
        Prod.cProd := item.codigo;
        Prod.cEAN := '';
        Prod.xProd := item.nome;
        Prod.NCM := StringReplace(ncm.codigo, '.', '', [rfReplaceAll]);
        Prod.cBenef := codigoBeneficioFiscal;
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
        vTotalOutrasDespesas := vTotalOutrasDespesas + documentoFiscalItens[nCont].outrasDespesas;
        vTotalDescontos := vTotalDescontos + DocumentoFiscal.DocumentoFiscalItens[nCont].desconto;

        with Imposto do
        begin
          vTotTrib := CalculaTributos(Prod.vProd, Prod.NCM);

          with ICMS do
          begin
            orig := StrToOrig(lOk, origemDamercadoria.codigo);
            if Emit.CRT = crtSimplesNacional then
            begin
              CSOSN := StrToCSOSNIcms(lOk, cstICMS.codigo);
              pCredSN := simplesAliquotaDeCredito;
              vCredICMSSN := subtotal * (pCredSN / 100);
            end
            else
              CST := StrToCSTICMS(lOk, cstICMS.codigo);

            vBC := icmsBC;
            pICMS := icmsAliquota;
            vICMS := vBC * (pICMS/100);
            pRedBC := icmsReducaoBase;
            pRedBCST := icmsSTReducaoBase;

            modBC := dbiValorOperacao;
            if  Length(icmsModalidadeDeCalculoMVA) > 0 then
              modBCST := dbisMargemValorAgregado
            else
              modBCST := dbisValorDaOperacao;
            pMVAST := icmsAliquotaMVA;
            vBCST := icmsSTBC;
            pICMSST := icmsstAliquota;
            vICMSST := icmsSTValor;

            vBCFCPST := 0;
            pFCPST := 0;
            vFCPST := 0;

            if (Ide.indFinal = cfConsumidorFinal) then
            begin
              vBCFCPST := fcpSTBC;
              pFCPST := IfThen(fcpSTBC > 0, fcpSTAliquota, 0);
              vFCPST := IfThen(fcpSTBC > 0, fcpSTValor, 0);
            end;

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

            vBaseDeCalculoICMSST := vBaseDeCalculoICMSST + icmsSTBC;
            vTotalICMSST := vTotalICMSST + vICMSST;
            vTotalTributos := vTotalTributos + vTotTrib;
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
            CST       := pis99; // StrToCSTPIS(lOk, cstPIS.codigo);
            vBC       := 0.00; //pisBC;
            pPIS      := 0.00; //pisAliquota;
            vPIS      := 0.00; //pisValor;
            qBCProd   := 0;
            vAliqProd := 0;
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
            CST     := cof99; //StrToCSTCOFINS(lOk, cstCOFINS.codigo);
            vBC     := 0.00; //cofinsBC;
            pCOFINS := 0.00; //cofinsAliquota;
            vCOFINS := 0.00; //cofinsValor;
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

    NotaF.NFe.Total.ICMSTot.vBCST         := vBaseDeCalculoICMSST;
    NotaF.NFe.Total.ICMSTot.vST           := vTotalICMSST;
    NotaF.NFe.Total.ICMSTot.vProd         := vTotalItens;
    NotaF.NFe.Total.ICMSTot.vFrete        := DocumentoFiscal.frete;
    NotaF.NFe.Total.ICMSTot.vSeg          := 0;
    NotaF.NFe.Total.ICMSTot.vDesc         := vTotalDescontos;
    NotaF.NFe.Total.ICMSTot.vII           := vTotalII;
    NotaF.NFe.Total.ICMSTot.vIPI          := vTotalIPI;
    NotaF.NFe.Total.ICMSTot.vIPIDevol     := vTotalIPIDevol;
    NotaF.NFe.Total.ICMSTot.vPIS          := vTotalPIS;
    NotaF.NFe.Total.ICMSTot.vCOFINS       := vTotalCOFINS;
    NotaF.NFe.Total.ICMSTot.vOutro        := vTotalOutrasDespesas;
    NotaF.NFe.Total.ICMSTot.vNF           := vTotalItens +
                                             vTotalICMSST +
                                             vTotalFCPST +
                                             vTotalIPI +
                                             vTotalIPIDevol +
                                             DocumentoFiscal.frete +
                                             vTotalOutrasDespesas -
                                             vTotalDescontos;
    NotaF.NFe.Total.ICMSTot.vTotTrib      := vTotalTributos;

    NotaF.NFe.Total.ICMSTot.vFCPUFDest    := 0.00;
    NotaF.NFe.Total.ICMSTot.vICMSUFDest   := 0.00;
    NotaF.NFe.Total.ICMSTot.vICMSUFRemet  := 0.00;

    NotaF.NFe.Total.ICMSTot.vFCPST        := vTotalFCPST;
    NotaF.NFe.Total.ICMSTot.vFCPSTRet     := 0;

    NotaF.NFe.Total.retTrib.vRetPIS       := 0;
    NotaF.NFe.Total.retTrib.vRetCOFINS    := 0;
    NotaF.NFe.Total.retTrib.vRetCSLL      := 0;
    NotaF.NFe.Total.retTrib.vBCIRRF       := 0;
    NotaF.NFe.Total.retTrib.vIRRF         := 0;
    NotaF.NFe.Total.retTrib.vBCRetPrev    := 0;
    NotaF.NFe.Total.retTrib.vRetPrev      := 0;

    NotaF.NFe.Transp.modFrete             := mfContaEmitente;
    NotaF.NFe.Transp.Transporta.CNPJCPF   := '';
    NotaF.NFe.Transp.Transporta.xNome     := '';
    NotaF.NFe.Transp.Transporta.IE        := '';
    NotaF.NFe.Transp.Transporta.xEnder    := '';
    NotaF.NFe.Transp.Transporta.xMun      := '';
    NotaF.NFe.Transp.Transporta.UF        := '';

    NotaF.NFe.Transp.retTransp.vServ      := 0;
    NotaF.NFe.Transp.retTransp.vBCRet     := 0;
    NotaF.NFe.Transp.retTransp.pICMSRet   := 0;
    NotaF.NFe.Transp.retTransp.vICMSRet   := 0;
    NotaF.NFe.Transp.retTransp.CFOP       := '';
    NotaF.NFe.Transp.retTransp.cMunFG     := 0;

    NotaF.NFe.Cobr.Fat.nFat               := IntToStr(documentoFiscal.DocumentoFiscalNFe.numero);
    NotaF.NFe.Cobr.Fat.vOrig              := DocumentoFiscal.total + vTotalFCPST + vTotalDescontos;
    NotaF.NFe.Cobr.Fat.vDesc              := vTotalDescontos;
    NotaF.NFe.Cobr.Fat.vLiq               := DocumentoFiscal.total + vTotalFCPST;

    var numDup := 0;
    for var nCont := 0 to Length(DocumentoFiscal.DocumentoFiscalCobrancas) - 1 do
    begin
      with NotaF.NFe.Cobr.Dup.New do
      begin
        numDup := numDup + 1;
        nDup := PadLeft(IntToStr(numDup), 3, '0');
        dVenc := DocumentoFiscal.documentoFiscalCobrancas[nCont].vencimento;
        vDup := DocumentoFiscal.documentoFiscalCobrancas[nCont].valor;
      end;
    end;

    NotaF.NFe.exporta.UFembarq   := '';
    NotaF.NFe.exporta.xLocEmbarq := '';

    NotaF.NFe.compra.xNEmp := '';
    NotaF.NFe.compra.xPed  := '';
    NotaF.NFe.compra.xCont := '';

    const indicador = ['01', '02', '03', '04', '05', '10', '11', '12', '13', '15', '16', '17', '18', '19', '90', '99'];

    if not(Length(documentoFiscalPagamentos) > 0) then
    begin
      with NotaF.NFe.pag.New do
      begin
        Ide.indPag := ipNenhum;
        IndPag := ipNenhum;
      end;
    end
    else
    begin
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
    end;

    NotaF.NFe.infIntermed.CNPJ := '';
    NotaF.NFe.infIntermed.idCadIntTran := '';

    with NotaF.Nfe.InfAdic.obsCont.New do
    begin
      xCampo := 'Info';
      xTexto := Format('%s. Total %m.', [DocumentoFiscal.referencia, DocumentoFiscal.total]);
    end;
    if (DocumentoFiscal.documentoFiscalNFe.informacoesAdicionaisContribuinte > '') then
      with NotaF.Nfe.InfAdic.obsCont.New do
      begin
        xCampo := 'Obs:';
        xTexto := DocumentoFiscal.documentoFiscalNFe.informacoesAdicionaisContribuinte;
      end;
    NotaF.NFe.InfAdic.infAdFisco :=  DocumentoFiscal.documentoFiscalNFe.informacoesAdicionaisFisco;
  end;

  nfe.NotasFiscais.GerarNFe();

end;

function TComponentes.GerarCFe(const DocumentoFiscal: TDocumentoFiscal): string;
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
    ide.dEmi          := emissao;

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
        if StrToCodigoMP(lOk, DocumentoFiscalPagamentos[Counter].formaIndicador) in [mpCartaodeCredito, mpCartaodeDebito] then
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

procedure TComponentes.GerarNFCe(const DocumentoFiscal: TDocumentoFiscal; var Error, Msg: String);
var
  lOk: Boolean;
  nCounter: Integer;
  NotaF: NotaFiscal;
  vBaseDeCalculo,
  vTotalICMS,
  vBaseeDeCalculoICMSST,
  vTotalICMSST,
  vTotalPIS,
  vTotalCOFINS,
  vTotalItens,
  vTotalFCPST,
  vTotalIPI,
  vTotalII,
  vTotalIPIDevol,
  vTotalOutrasDespesas,
  vTotalDescontos,
  vTotalTributos: Double;
  Count: TNFe;
begin
  vBaseDeCalculo := 0;
  vBaseeDeCalculoICMSST := 0;
  vTotalICMS := 0;
  vTotalICMSST := 0;
  vTotalPIS := 0;
  vTotalCOFINS := 0;
  vTotalIPI := 0;
  vTotalIPIDevol := 0;
  vTotalII := 0;
  vTotalItens := 0;
  vTotalFCPST := 0;
  vTotalOutrasDespesas := 0;
  vTotalDescontos := 0;
  vTotalTributos := 0;

  nfe.NotasFiscais.Clear;

  CarregaNFe(
    DocumentoFiscal.estabelecimento,
    DocumentoFiscal.documentoFiscalNFe.formaDeEmissao,
    Error,
    Msg,
    DocumentoFiscal.modelo);

  nfe.Configuracoes.WebServices.UF       := DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla;
  nfe.Configuracoes.WebServices.Ambiente := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.estabelecimento.estabelecimentoFiscalSerie.ambiente));

  with nfe.NotasFiscais.Add.NFe do
  begin
    Ide.natOp     := DocumentoFiscal.historico.nome;
    Ide.modelo    := StrToIntDef(DocumentoFiscal.modelo, 65);
    Ide.serie     := DocumentoFiscal.documentoFiscalNFe.serie;
    Ide.nNF       := DocumentoFiscal.documentoFiscalNFe.numero;
    if not(DocumentoFiscal.documentoFiscalNFe.chave = '') then
    begin
      infNFe.ID   := 'NFe' + DocumentoFiscal.documentoFiscalNFe.chave;
      Ide.cNF     := ExtrairCodigoChaveAcesso(infNFe.ID);
      Ide.cDV     := ExtrairDigitoChaveAcesso(infNFe.ID);
    end;
    Ide.dEmi      := Now;
    Ide.dSaiEnt   := Now;
    Ide.hSaiEnt   := Now;
    Ide.tpNF      := tnSaida;
    Ide.verProc   := build;
    Ide.tpEmis    := StrToTpEmis(lOk, IntToStr(ExtrairTipoEmissaoChaveAcesso(infNFe.ID)));
    Ide.tpAmb     := StrToTpAmb(lOk, IntToStr(DocumentoFiscal.ambiente));
    Ide.cUF       := UFtoCUF(DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].uf.sigla);
    Ide.cMunFG    := StrToInt(DocumentoFiscal.estabelecimento.estabelecimentoEnderecos[0].municipio.codigo);
    Ide.finNFe    := fnNormal;
    Ide.tpImp     := tiNFCe;
    Ide.indPres   := pcPresencial;
    Ide.indIntermed := iiSemOperacao;

    if not(Assigned(DocumentoFiscal.parceiro.parceiroDocumentos)) then
      Ide.indFinal := cfConsumidorFinal
    else
    if DocumentoFiscal.parceiro.parceiroDocumentos[0].inscricaoEstadual = '' then
      Ide.indFinal  := cfConsumidorFinal
    else
      Ide.indFinal  := cfNao;

    case Ide.tpEmis of
      teOffLine: begin
        Ide.dhCont := date;
        Ide.xJust  := 'Problemas de comunicação com a SEFAZ devido a falha na internet.';
      end;
      teContingencia: begin
        Ide.dhCont := date;
        Ide.xJust  := 'Modo de contingência ativado, por problema com a SEFAZ';
      end;
      teSVCRS: begin
        Ide.dhCont := date;
        Ide.xJust  := 'Contingência SVCRS habilitado por problemas com a SEFAZ';
      end;
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

      if not(estabelecimentoFiscal.cnpjaut = EmptyStr) then
        autXML.New.CNPJCPF := estabelecimentoFiscal.cnpjaut;


      if Assigned(estabelecimentoFiscal.cnpjauts)  then
        for var I := 0 to High(estabelecimentofiscal.cnpjauts) do
          with autXML.New do
            CNPJCPF := estabelecimentoFiscal.cnpjauts[I];

    end;

    if length(DocumentoFiscal.parceiro.parceiroDocumentos) > 0 then
    begin
      Dest.CNPJCPF            := DocumentoFiscal.parceiro.parceiroDocumentos[0].documentoNumero;

      if (DocumentoFiscal.parceiro.parceiroDocumentos[0].documentoTipo in [1, 2]) and (UpperCase(DocumentoFiscal.parceiro.parceiroDocumentos[0].inscricaoEstadual) = 'ISENTO') then
        Dest.indIEDest        := inIsento
      else if ((DocumentoFiscal.parceiro.parceiroDocumentos[0].documentoTipo in [1, 2])) and (DocumentoFiscal.parceiro.parceiroDocumentos[0].inscricaoEstadual > '') then
        Dest.indIEDest        := inContribuinte
      else
      begin
        Ide.indFinal          := cfConsumidorFinal;
        Dest.indIEDest        := inNaoContribuinte;
      end;

      Dest.IE	                := DocumentoFiscal.parceiro.parceiroDocumentos[0].inscricaoEstadual;
      Dest.ISUF               := '';
      Dest.xNome              := DocumentoFiscal.parceiro.nome;

      Dest.EnderDest.Fone     := '';
      Dest.EnderDest.CEP      := StrToInt(String(RemoveStrings(AnsiString(DocumentoFiscal.parceiro.parceiroEnderecos[0].cep), ['.', '-'])));
      Dest.EnderDest.xLgr     := DocumentoFiscal.parceiro.parceiroEnderecos[0].logradouro;
      Dest.EnderDest.nro      := IntToStr(DocumentoFiscal.parceiro.parceiroEnderecos[0].numero);
      Dest.EnderDest.xCpl     := DocumentoFiscal.parceiro.parceiroEnderecos[0].complemento;
      Dest.EnderDest.xBairro  := DocumentoFiscal.parceiro.parceiroEnderecos[0].bairro;
      Dest.EnderDest.cMun     := StrToInt(DocumentoFiscal.parceiro.parceiroEnderecos[0].municipio.codigo);
      Dest.EnderDest.xMun     := DocumentoFiscal.parceiro.parceiroEnderecos[0].municipio.nome;
      Dest.EnderDest.UF       := DocumentoFiscal.parceiro.parceiroEnderecos[0].uf.sigla;
      Dest.EnderDest.cPais    := 1058;
      Dest.EnderDest.xPais    := 'BRASIL';
    end

    else if Length(DocumentoFiscal.cpfInformado) > 0 then
    begin
      Dest.CNPJCPF            := DocumentoFiscal.cpfInformado;
      Ide.indFinal            := cfConsumidorFinal;
      Dest.indIEDest          := inNaoContribuinte;
    end

    else if Length(DocumentoFiscal.cnpjInformado) > 0 then
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
        prod.cBenef := codigoBeneficioFiscal;
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
        vTotalOutrasDespesas := vTotalOutrasDespesas + DocumentoFiscal.documentoFiscalItens[nCounter].outrasDespesas;
        vTotalDescontos := vTotalDescontos + DocumentoFiscal.DocumentoFiscalItens[nCounter].desconto;

        with Imposto do
        begin
          vTotTrib := CalculaTributos(Prod.vProd, Prod.NCM);

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
            vICMS := RoundABNT(vBC * (pICMS/100), 2);

            modBCST := dbisMargemValorAgregado;
            pMVAST  := icmsAliquotaMVA;
            pRedBC := icmsReducaoBase;
            pRedBCST := icmsSTReducaoBase;
            vBCST := icmsSTBC;
            pICMSST := icmsstAliquota;
            vICMSST := icmsSTValor;

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
            vTotalICMS := vTotalICMS + vICMS;
            vBaseeDeCalculoICMSST := vBaseeDeCalculoICMSST + icmsSTBC;
            vTotalICMSST := vTotalICMSST + vICMSST;
            vTotalTributos := vTotalTributos + vTotTrib;

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
            vPIS := RoundABNT(vBC * (pPIS/100), 2);

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
            vPIS      := RoundABNT(vBC * (pPis/100), 2);
            IndSomaPISST :=  ispNenhum;
          end;

          with COFINS do
          begin
            CST := StrToCSTCOFINS(lOk, CSTCOFINS.codigo);
            vBC := cofinsBC;
            pCOFINS := cofinsAliquota;
            vCOFINS := RoundABNT(vBC * (pCOFINS/100), 2);

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
            vCOFINS         := RoundABNT(vBC * (pCOFINS/100), 2);
            indSomaCOFINSST :=  iscNenhum;
          end;
        end;

        infAdProd := '';
      end;
    end;

    with Total.ICMSTot do
    begin
      vBC           := vBaseDeCalculo;
      vICMS         := vTotalICMS;
      vBCST         := vBaseeDeCalculoICMSST;
      vST           := vTotalICMSST;
      vProd         := vTotalItens;
      vFrete        := DocumentoFiscal.frete;
      vSeg          := 0;
      vDesc         := vTotalDescontos;
      vII           := 0;
      vIPI          := 0;
      vPIS          := vTotalPIS;
      vCOFINS       := vTotalCOFINS;
      vOutro        := vTotalOutrasDespesas;
      vTotTrib      := vTotalTributos;
      vNF           := vTotalItens + vTotalICMSST + vOutro + vFrete - vTotalDescontos;

      vFCPUFDest    := 0.00;
      vICMSUFDest   := 0.00;
      vICMSUFRemet  := 0.00;
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
            if not (StrToFormaPagamento(lOk, DocumentoFiscal.documentoFiscalPagamentos[nCounter].formaIndicador) = fpPagamentoInstantaneo) then
            begin
              CNPJ      := ACBrUtil.Strings.OnlyNumber(DocumentoFiscal.documentoFiscalPagamentos[nCounter].cartaoCNPJ);
              tBand     := TpcnBandeiraCartao(StrToIntDef(DocumentoFiscal.documentoFiscalPagamentos[nCounter].cartaoCredenciadora, 27));
            end
            else
              tBand     := bcOutros;
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

function TComponentes.ManifestarDocumento(DocumentoFiscalManifesto: TDocumentoFiscalManifesto; var Error, Msg: String): TDocumentoFiscalManifesto;
var
  xmlDocumento: String;
  estabelecimentoNumero, chave, documentoNumero: String;
  eventoParametro, ultimoNSU: String;
begin
  Error := '';
  Msg := '';

//  if not Assigned(DocumentoFiscalManifesto.estabelecimento) then
//    raise Exception.Create('Estabelecimento não informado');
//
//  if Length(DocumentoFiscalManifesto.estabelecimento.estabelecimentoDocumentos) <= 0 then
//    raise Exception.Create('Estabelecimento sem documentação informado');
//
//  if DocumentoFiscalManifesto.chave = '' then
//    raise Exception.Create('Chave não informada');
//
//  if DocumentoFiscalManifesto.estabelecimento.estabelecimentoDocumentos[0].documentoNumero = '' then
//    raise Exception.Create('CNPJ não informado');

  carregaNFe(
    DocumentoFiscalManifesto.estabelecimento,
    1,
    Error,
    Msg,
    '55');

  var eventoSelecionado := 0;

  case DocumentoFiscalManifesto.codevento of
     200: eventoSelecionado := 3;
     210: eventoSelecionado := 4;
     220: eventoSelecionado := 5;
     240: eventoSelecionado := 6;
  end;

  try
    if not (eventoSelecionado in [3, 4, 5, 6]) then
      Error := 'Evento inválido. Permitidos: 3, 4, 5, 6.'
    else
    begin
      if DocumentoFiscalManifesto.ambiente = 0 then
        nfe.Configuracoes.WebServices.Ambiente := taProducao
      else if DocumentoFiscalManifesto.ambiente = 1 then
        nfe.Configuracoes.WebServices.Ambiente := taHomologacao
      else
        nfe.Configuracoes.WebServices.Ambiente := taHomologacao;

      nfe.EventoNFe.Evento.Clear;

      with nfe.EventoNFe.Evento.Add do
      begin
        InfEvento.cOrgao          := 91; // NF-e, CT-e, MDF-e
        InfEvento.chNFe           := DocumentoFiscalManifesto.chave;
        InfEvento.CNPJ            := DocumentoFiscalManifesto.estabelecimento.estabelecimentoDocumentos[0].documentoNumero;
        InfEvento.dhEvento        := Now;
        InfEvento.detEvento.xJust := Trim(UpperCase(DocumentoFiscalManifesto.motivo));

        case eventoSelecionado of
          3: InfEvento.tpEvento := teManifDestConfirmacao;
          4: InfEvento.tpEvento := teManifDestCiencia;
          5: InfEvento.tpEvento := teManifDestDesconhecimento;
          6: InfEvento.tpEvento := teManifDestOperNaoRealizada;
        end;
      end;

      if DocumentoFiscalManifesto.documentoNumero > 0 then
      begin
        nfe.EnviarEvento(DocumentoFiscalManifesto.documentoNumero);

        if (nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Count > 0) and
           (nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.cStat <> 135) then
          Error := 'O seguinte erro ocorreu na tentativa de manifestar o documento: ' + nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo
        else
          Msg := nfe.WebServices.EnvEvento.EventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
          Result := DocumentoFiscalManifesto;
      end;
    end;
  except
    on E: Exception do
    begin
      Error := E.Message;
      Result := DocumentoFiscalManifesto;
    end;
  end;
end;


function TComponentes.gerarSPED(Req: THorseRequest; var Error: string; var Msg: string): TStringStream;
var
  fileStream: TFileStream;
  sped: TSped;
begin
  with FormatSettings do
  begin
    ShortDateFormat := 'dd/mm/yyyy';
    DecimalSeparator := ',';
  end;

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
      sped.gerar(Estabelecimento, dataInicial, dataFinal, nomeArq, error, finalidade, semMovimento, inventario);
      try
        fileStream := TFileStream.Create(nomeArq, fmOpenRead or fmShareDenyWrite);
      except
        On E: Exception do
          error := E.Message;
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
