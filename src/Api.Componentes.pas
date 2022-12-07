unit Api.Componentes;

interface

uses
  System.SysUtils, System.Classes, IniFiles, ACBrBase, ACBrSAT, ACBrDFeSSL,
  ACBrSATClass, pcnConversao, Model.DocumentoFiscal, ACBrDFeReport,
  ACBrSATExtratoClass, ACBrSATExtratoESCPOS, pcnCFe;

type
  Tcomponents = class(TDataModule)
    sat: TACBrSAT;
    satExtrato: TACBrSATExtratoESCPOS;
    procedure satGetcodigoDeAtivacao(var Chave: AnsiString);
    procedure satGetsignAC(var Chave: AnsiString);
    procedure DataModuleCreate(Sender: TObject);
  private
    FCodigoDeAtivacao: AnsiString;
    FAssinaturaAC: AnsiString;

    procedure carregaSAT;
  public
    property CodigoDeAtivacao: AnsiString read FCodigoDeAtivacao write FCodigoDeAtivacao;
    property AssinaturaAC: AnsiString read FAssinaturaAC write FAssinaturaAC;

    function inicializaSAT: boolean;
    procedure GerarVendaCFe(out xmlVenda: string; const DocumentoFiscal: TDocumentoFiscal);
    function EnviarVendaCFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
    function CancelarVendaCFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;

  end;

var
  components: Tcomponents;

implementation

uses
  ACBrUtil;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ Tcomponents }

procedure Tcomponents.carregaSAT;
var
  ini: TIniFile;
begin
  try
    ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));

    try
      with ini do
      begin
        with sat do
        begin
          SSL.SSLCryptLib := cryOpenSSL;
          SSL.SSLXmlSignLib := xsLibXml2;

          CFe.IdentarXML := True;
          CFe.TamanhoIdentacao := 2;
          CFe.RetirarAcentos := True;

          Config.ArqSchema := ReadString('CFe', 'Schemas', 'Schemas');
          Config.PaginaDeCodigo     := ReadInteger('CFe', 'PaginaDeCodigo', 1252);
          Config.EhUTF8             := ReadBool('CFe', 'UTF', True);
          Config.infCFe_versaoDadosEnt := ReadFloat('CFe', 'VersaoLayout', 0.07);

          Modelo  := TACBrSATModelo(ReadInteger('CFe', 'Modelo', 1)) ;
          ArqLOG  := ReadString('CFe', 'ArqLOG', 'audax-cfe.log');
          NomeDLL := ReadString('CFe', 'NomeDLL', '/opt/sefaz/drs/libmfe.so');

          Config.ide_numeroCaixa := ReadInteger('CFe', 'Caixa', 1);
          Config.ide_tpAmb       := TpcnTipoAmbiente(ReadInteger('CFe', 'Ambiente', 2));
          Config.ide_CNPJ        := ReadString('SwHouse', 'CNPJ', '');

          Config.emit_CNPJ       := ReadString('Emitente', 'CNPJ', '');
          Config.emit_IE         := ReadString('Emitente', 'IE', '');
          Config.emit_IM         := ReadString('Emitente', 'IM', '');
          Config.emit_cRegTrib      := TpcnRegTrib(ReadInteger('Emitente', 'RegimeICMS', 0));
          Config.emit_cRegTribISSQN := TpcnRegTribISSQN(ReadInteger('Emitente', 'RegimeISSQN', 0));
          Config.emit_indRatISSQN   := TpcnindRatISSQN(ReadInteger('Emitente', 'IndicadorDeRateio', 1));

          ConfigArquivos.SalvarCFe := ReadBool('Arquivos', 'SalvarCFe', True);
          ConfigArquivos.SalvarCFeCanc := ReadBool('Arquivos', 'SalvarCancelamento', True);
          ConfigArquivos.SalvarEnvio := ReadBool('Arquivos', 'SalvarEnvio', True);
          ConfigArquivos.SepararPorCNPJ := ReadBool('Arquivos', 'SalvarPorCNPJ', True);
          ConfigArquivos.SepararPorModelo := ReadBool('Arquivos', 'SepararPorModelo', False);
          ConfigArquivos.SepararPorDia := ReadBool('Arquivos', 'SepararPorDia', False);
          ConfigArquivos.SepararPorMes := ReadBool('Arquivos', 'SepararPorMes', True);
          ConfigArquivos.SepararPorAno := ReadBool('Arquivos', 'SepararPorAno', True);

          FCodigoDeAtivacao := AnsiString(ReadString('CFe', 'CodigoDeAtivacao', '12345678'));
          FAssinaturaAC := AnsiString(ReadString('SwHouse', 'Assinatura', ''));

          CFe.IdentarXML := True;
          CFe.TamanhoIdentacao := 3;
          CFe.RetirarAcentos := True;
        end;
      end;
    finally
      if Assigned(ini) then
        FreeAndNil(ini);
    end;

  except
    on E:Exception do
    begin
      Write(Format('Impossível carregar o SAT. Verificar com suporte, o erro %s.', [E.Message]));
    end;
  end;
end;

procedure Tcomponents.DataModuleCreate(Sender: TObject);
begin
  RemoveDataModule(self);
end;

function Tcomponents.EnviarVendaCFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
var
  xmlVenda: string;
begin
  try
    GerarVendaCFe(xmlVenda, DocumentoFiscal);

    sat.EnviarDadosVenda(xmlVenda);

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
      WriteLn(Format('Não foi possivel emitir o cupom fiscal, o seguinte erro ocorreu: %s.', [sat.Resposta.mensagemRetorno]));

  except
    On E: Exception do
    begin
      Result := nil;
    end;
  end;
end;

function Tcomponents.CancelarVendaCFe(DocumentoFiscal: TDocumentoFiscal): TDocumentoFiscal;
var
  xmlOriginal: String;
begin
  xmlOriginal := DocumentoFiscal.documentoFiscalCFe.xml;

  inicializaSAT;
  sat.InicializaCFe;

  try
    sat.CFe.SetXMLString(xmlOriginal);
    sat.CFe2CFeCanc;

    sat.CFeCanc.GerarXML(True);
    sat.CancelarUltimaVenda;

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
      WriteLn(Format('Não foi possivel cancelar o cupom fiscal, o seguinte erro ocorreu: %s.', [sat.Resposta.mensagemRetorno]));

  except
    On E: Exception do
    begin
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

procedure Tcomponents.satGetcodigoDeAtivacao(var Chave: AnsiString);
begin
  Chave := FCodigoDeAtivacao;
end;

procedure Tcomponents.satGetsignAC(var Chave: AnsiString);
begin
  Chave := FAssinaturaAC;
end;

procedure Tcomponents.GerarVendaCFe(out xmlVenda: string; const DocumentoFiscal: TDocumentoFiscal);
var
  TotalItem, TotalImposto, TotalGeral: Double;
  A: Integer;
  lOk: Boolean;
  Counter: Integer;
begin
  xmlVenda := '';

  TotalItem  := 0;
  TotalGeral := 0;
  TotalImposto := 0;

  inicializaSAT;
  sat.InicializaCFe;

  with sat.CFe, DocumentoFiscal do
  begin
    ide.cNF := Random(999999);

    //Dest.CNPJCPF := ''; //parceiro.parceiroDocumentos[0].documentoNumero;
    //Dest.xNome := ''; //parceiro.nome;

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
      end;
    end;

    InfAdic.infCpl := 'Acesse www.solucaosistemas.net para obter maiores;informações sobre o sistema Constel';

  end;

  xmlVenda := sat.CFe.GerarXML(True);
end;

end.
