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
  Model.CNAE in 'models\Model.CNAE.pas';

function startApi: boolean;
begin
  try
    TRotas.Registra;
    Result := True;
  except
    On E: Exception do
    begin
      Writeln(Format('O seguinte erro ocorreu na tentativa de executar a API, %s.', [E.Message]));
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
        else WriteLn(Format('O parâmentro %s não é válido. \nFavor informar como parâmetro "start", "stop" ou "status".', [ParamStr(1)]));
      end;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
