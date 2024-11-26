program ConstelFiscal;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.AnsiStrings,
  Horse,
  Api.Rotas in 'Api.Rotas.pas',
  Api.Funcoes in 'Api.Funcoes.pas',
  Api.Componentes in 'Api.Componentes.pas' {components: TDataModule},
  Lib.Excecoes in 'lib\Lib.Excecoes.pas',
  Lib.Funcoes in 'lib\Lib.Funcoes.pas',
  Lib.Sistema.Tipos in 'lib\Lib.Sistema.Tipos.pas',
  Model.Base in 'models\Model.Base.pas',
  Model.Categoria in 'models\Model.Categoria.pas',
  Model.Conta in 'models\Model.Conta.pas',
  Model.DocumentoFiscal in 'models\Model.DocumentoFiscal.pas',
  Model.Estabelecimento in 'models\Model.Estabelecimento.pas',
  Model.Historico in 'models\Model.Historico.pas',
  Model.Item in 'models\Model.Item.pas',
  Model.Municipio in 'models\Model.Municipio.pas',
  Model.Operacao in 'models\Model.Operacao.pas',
  Model.OrigemDaMercadoria in 'models\Model.OrigemDaMercadoria.pas',
  Model.Parceiro in 'models\Model.Parceiro.pas',
  Model.Preco in 'models\Model.Preco.pas',
  Model.UF in 'models\Model.UF.pas',
  Model.Unidade in 'models\Model.Unidade.pas',
  Model.NCM in 'models\Model.NCM.pas',
  Model.CEST in 'models\Model.CEST.pas',
  Model.CFOP in 'models\Model.CFOP.pas',
  Model.CSTICMS in 'models\Model.CSTICMS.pas',
  Model.CSTIPI in 'models\Model.CSTIPI.pas',
  Model.CSTPISCOFINS in 'models\Model.CSTPISCOFINS.pas',
  Model.Moeda in 'models\Model.Moeda.pas',
  Model.CNAE in 'models\Model.CNAE.pas',
  Model.Config in 'models\Model.Config.pas',
  Model.Inutilizacao in 'models\Model.Inutilizacao.pas',
  Model.DocumentoFiscalItemAgrupado in 'models\Model.DocumentoFiscalItemAgrupado.pas',
  Model.DocumentoFiscalItemSPED in 'models\Model.DocumentoFiscalItemSPED.pas',
  Model.DocumentoFiscalSPED in 'models\Model.DocumentoFiscalSPED.pas',
  Model.DocumentoFiscalSPEDItem in 'models\Model.DocumentoFiscalSPEDItem.pas',
  Model.Sped in 'models\Model.Sped.pas',
  Model.Contabilista in 'models\Model.Contabilista.pas',
  APIService in 'APIService.pas',
  Model.Fortes.RegistroCAB in 'models\Model.Fortes.RegistroCAB.pas',
  Model.Fortes.RegistroPAR in 'models\Model.Fortes.RegistroPAR.pas',
  Model.Fortes.RegistroGRP in 'models\Model.Fortes.RegistroGRP.pas',
  Model.Fortes.RegistroPRO in 'models\Model.Fortes.RegistroPRO.pas',
  Model.Fortes.RegistroUND in 'models\Model.Fortes.RegistroUND.pas',
  Model.Fortes.RegistroNOP in 'models\Model.Fortes.RegistroNOP.pas',
  Model.Fortes.RegistroPNM in 'models\Model.Fortes.RegistroPNM.pas',
  Model.Fortes.RegistroINM in 'models\Model.Fortes.RegistroINM.pas',
  Model.Fortes.RegistroNFM in 'models\Model.Fortes.RegistroNFM.pas',
  Model.Fortes.RegistroCTA in 'models\Model.Fortes.RegistroCTA.pas',
  Model.Fortes.RegistroLAN in 'models\Model.Fortes.RegistroLAN.pas',
  Model.Fortes.RegistroCTR in 'models\Model.Fortes.RegistroCTR.pas',
  Model.Fortes.RegistroMOV in 'models\Model.Fortes.RegistroMOV.pas',
  Model.Fortes.RegistroIMP in 'models\Model.Fortes.RegistroIMP.pas',
  Model.Fortes.RegistroCLI in 'models\Model.Fortes.RegistroCLI.pas',
  Model.Fortes.RegistroFOR in 'models\Model.Fortes.RegistroFOR.pas',
  Model.Fortes.RegistroTPF in 'models\Model.Fortes.RegistroTPF.pas',
  Model.Fortes.RegistroCRT in 'models\Model.Fortes.RegistroCRT.pas',
  Model.Fortes.RegistroDCT in 'models\Model.Fortes.RegistroDCT.pas',
  Model.Fortes.RegistroPAG in 'models\Model.Fortes.RegistroPAG.pas',
  Model.Fortes.RegistroTRN in 'models\Model.Fortes.RegistroTRN.pas',
  Model.Fortes.RegistroCFI in 'models\Model.Fortes.RegistroCFI.pas',
  Model.Fortes.RegistroDIP in 'models\Model.Fortes.RegistroDIP.pas',
  Model.Fortes.RegistroAIP in 'models\Model.Fortes.RegistroAIP.pas',
  Model.Fortes.RegistroCEI in 'models\Model.Fortes.RegistroCEI.pas',
  Model.Fortes.RegistroCVN in 'models\Model.Fortes.RegistroCVN.pas',
  Model.Fortes.RegistroIPR in 'models\Model.Fortes.RegistroIPR.pas',
  Model.Fortes.RegistroICS in 'models\Model.Fortes.RegistroICS.pas',
  Model.Fortes.RegistroFIN in 'models\Model.Fortes.RegistroFIN.pas',
  Model.Fortes.RegistroBSS in 'models\Model.Fortes.RegistroBSS.pas',
  Model.Fortes.RegistroCDF in 'models\Model.Fortes.RegistroCDF.pas',
  Model.Fortes.RegistroCBL in 'models\Model.Fortes.RegistroCBL.pas',
  Model.Fortes.RegistroOUM in 'models\Model.Fortes.RegistroOUM.pas',
  Model.Fortes.RegistroPAJ in 'models\Model.Fortes.RegistroPAJ.pas',
  Model.Fortes.RegistroSPR in 'models\Model.Fortes.RegistroSPR.pas',
  Model.Fortes.RegistroSSP in 'models\Model.Fortes.RegistroSSP.pas',
  Model.Fortes.RegistroITS in 'models\Model.Fortes.RegistroITS.pas',
  Model.Fortes.RegistroAES in 'models\Model.Fortes.RegistroAES.pas',
  Model.Fortes.RegistroDSS in 'models\Model.Fortes.RegistroDSS.pas',
  Model.Fortes.RegistroSVN in 'models\Model.Fortes.RegistroSVN.pas',
  Model.Fortes.RegistroCEE in 'models\Model.Fortes.RegistroCEE.pas',
  Model.Fortes.RegistroTRA in 'models\Model.Fortes.RegistroTRA.pas',
  Fortes.IRegistro in 'Fortes.IRegistro.pas';

function startApi: boolean;
begin
  {$IFDEF MSWINDOWS}
    IsConsole := True;
    ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  try
    TRotas.Registra;
    Result := True;
  except
    On E: Exception do
    begin
      Log(Format('O seguinte erro ocorreu na tentativa de executar a API, %s.', [E.Message]));
      Result := False;
    end;
  end;
end;

begin
  try
    if startApi then
    begin
      case AnsiIndexStr(AnsiString(LowerCase(ParamStr(1))), ['start', 'stop', 'status']) of
        0: Api.Funcoes.startApi;
        1: Api.Funcoes.stopApi;
        2: Api.Funcoes.statusApi;
        else Log(Format('O parâmentro %s não é válido. \nFavor informar como parâmetro "start", "stop" ou "status".', [ParamStr(1)]));
      end;
    end;
  except
    on E: Exception do
      Log(Format('Erro na classe: %s, com a mensagem: %s', [E.ClassName, E.Message]));
  end;
end.
