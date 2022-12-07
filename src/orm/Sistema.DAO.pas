unit Sistema.DAO;

interface

uses System.SysUtils, System.Classes, System.JSON, REST.JSON,
  FireDAC.Phys.MongoDBWrapper, Model.Base, Vcl.StdCtrls;

type
  TSincronizacaoBloco =
  (
    sbBloco1,
    sbBloco2
  );

type
  TListProgressEvent = procedure(Nome: string; Etapa, Etapas: Integer) of object;
  TClassProgressEvent = procedure(Registro, Registros: Integer) of object;

type
  TSincronizacaoSituacao = record
    CargaItens: Boolean;
    CartaFinanceira: Boolean;
    CargaApontamento: Boolean;
    CargaAtendimento: Boolean;
    CargaAuxiliares: Boolean;
  end;

type
  TDAO = class
  private
    class function DBName(): string;
    class function Collection(CollectionName: string): TMongoCollection;
    class function GetConfig(Key: string): string;
    class function SetConfig(Key, Value: string): string;
    class procedure Armazena(ProgressEvent: TClassProgressEvent; Recurso: string);
    class procedure ArmazenaDocumento(Collection: TMongoCollection; JSONValue: TJSONValue; var UltimaAtualizacao: TDateTime);
  public
    class function Get<T: class, constructor>(Colecao: string): T; overload;
    class function Get<T: class, constructor>(Colecao, Id: string): T; overload;
    class function GetArray<T: class, constructor>(Colecao: string; Criterio: string = ''; Ordem: string = ''): TArray<T>; overload;
    class function GetArray(Colecao: string; Criterio: string = ''; Ordem: string = ''): TJSONArray; overload;
    class function Post<T: class>(Colecao: string; Instancia: TObject): T;
    class function Replace<T: class>(Colecao: string; Instancia: TObject): T;
    class function GetArquivo(Nome, Arquivo: string; var FileName: string): Boolean;
    class function GetArquivoDisponivel(Nome, Arquivo: string; var FileName: string): Boolean;
//    class function Iniciado(): Boolean;
    class procedure Sincroniza(SincronizacaoBloco: TSincronizacaoBloco; ListProgressEvent: TListProgressEvent; ClassProgressEvent: TClassProgressEvent);
  end;

type
  TSincroniaThread = class(TThread)
  private
    FMensagem: string;
    FMessageLabel: TLabel;
    FMessageBack: string;
  public
    constructor Create(MessageLabel: TLabel);
    procedure Execute(); override;
    procedure Contador();
  end;

implementation

uses System.DateUtils, Lib.Funcoes;

class function TDAO.Get<T>(Colecao: string): T;
begin
  AppendLog(Colecao);
  var Collection := TDAO.Collection(Colecao);
  var Find := Collection.Find();
  Find.Project('{_id: false}');
  var oCrs := Find.Open();
  if oCrs.Next then
  begin
    Result := TJSON.JsonToObject<T>(oCrs.Doc.AsJSON);
    Exit;
  end;
  Result := TJSON.JsonToObject<T>('{}');
end;

class function TDAO.Get<T>(Colecao, Id: string): T;
begin
  AppendLog('GET: ' + Colecao + '/' + Id);
  var Collection := TDAO.Collection(Colecao);
  var Find := Collection.Find();
  Find.Project('{_id: false}');
  Find.Match('{id: "' + Id + '"}');
  var oCrs := Find.Open();
  if oCrs.Next then
  begin
    Result := TJSON.JsonToObject<T>(oCrs.Doc.AsJSON);
    Exit;
  end;
  raise Exception.CreateFmt('Id "%s" não encontrado na coleção "%s"', [Id, Colecao]);
end;

class function TDAO.GetArray<T>(Colecao: string; Criterio: string = ''; Ordem: string = ''): TArray<T>;
begin
  while Criterio.StartsWith('?') do
    Delete(Criterio, 1, 1);
  AppendLog('GET: ' + Colecao + '?' + Criterio);
  Result := [];
  var Limite := 0;
  var Collection := TDAO.Collection(Colecao);
  var Find := Collection.Find();
  // Critério
  var ACriterio: TArray<string> := [];
  if Criterio > '' then
    for var Elemento in Criterio.Split([',', ';', '&']) do
    begin
      var Par := Elemento.Trim().Split(['=']);
      if Length(Par) < 2 then
        Continue;
      var Campo := Par[0];
      var Valor := Par[1];
      if Valor.IsEmpty then // Sem valor
        Continue;
      if Campo.Equals('limite') then
      begin
        Limite := StrToIntDef(Valor, 0);
        Continue;
      end;
      if Campo.Equals('id') then // Id
      begin
        ACriterio := ACriterio + ['"id": "' + Valor + '"'];
      end else
      if Campo.Equals('ids') then // Array de Ids
      begin
        ACriterio := ACriterio + ['"id": { $in: ["' + String.Join('","', Valor.Split([','])) + '"] }'];
      end else
      if Campo.EndsWith('.id') then // Id
      begin
        ACriterio := ACriterio + ['"' + Campo + '": "' + Valor + '"'];
      end else
      if Campo.Equals('codigos') then // Array de Códigos
      begin
        ACriterio := ACriterio + ['"codigo": { $in: ["' + String.Join('","', Valor.Split([','])) + '"] }'];
      end else
      if not Valor.ToLower().Equals('null') then // Não nulo
      begin
        if Campo.Equals('texto') then
        begin
          Campo := 'nome';
          Valor := '{"$regex": "' + Valor.Replace('*', '') + '", "$options" : "i"}';
          //{ $regex : /^http/ } starts with
          //db.collection.find({name: {$regex : /^string/i}}) começa com
          //db.collection.find({name: {$regex : /string/i}})
        end else
        if not Valor.StartsWith('$') then // Número
        begin
          Valor := Valor.Replace('"', '''').Replace('%', '*');
          if Valor.StartsWith('*') then
            Valor := '{"$regex": "' + Valor.Replace('*', '') + '$", "$options" : "i"}'
          else if Valor.EndsWith('*') then
            Valor := '{"$regex": "^' + Valor.Replace('*', '') + '", "$options" : "i"}'
          else if eNumerico(Valor) then
            Valor := '"' + Valor + '"'
          else
            Valor := '{"$regex": "' + Valor + '", "$options" : "i"}';
        end else
          Valor := Valor.Substring(1);
        ACriterio := ACriterio + ['"' + Campo + '": ' + Valor];
      end else
        ACriterio := ACriterio + ['"' + Campo + '": ' + Valor];
    end;
  ACriterio := ACriterio + ['"exclusao": null'];
  var S := '{' + String.Join(', ', ACriterio) + '}';
  Find.Match(S);
  if Limite > 0 then
    Find.Limit(Limite);
  // Ordem
  if Ordem > '' then
  begin
    var ASort: TArray<string> := [];
    for var Elemento in Ordem.Split([',', ';']) do
      ASort := ASort + ['"' + Elemento.Trim() + '": 1'];
    Find.Sort('{' + String.Join(', ', ASort) + '}');
  end;
  // Resultado
  var oCrs := Find.Open();
  while oCrs.Next do
    Result := Result + [TJSON.JsonToObject<T>(oCrs.Doc.AsJSON)];
end;

class function TDAO.GetArray(Colecao: string; Criterio: string = ''; Ordem: string = ''): TJSONArray;
begin
  while Criterio.StartsWith('?') do
    Delete(Criterio, 1, 1);
  AppendLog('GET: ' + Colecao + '?' + Criterio);
  Result := TJSONArray.Create();
  var Collection := TDAO.Collection(Colecao);
  var Find := Collection.Find();
  // Critério
  var ACriterio: TArray<string> := [];
  if Criterio > '' then
    for var Elemento in Criterio.Split([',', ';', '&']) do
    begin
      var Par := Elemento.Trim().Split(['=']);
      if Length(Par) < 2 then
        Continue;
      var Campo := Par[0];
      var Valor := Par[1];
      if Valor.IsEmpty then // Sem valor
        Continue;
      if Campo.Equals('id') then // Id
      begin
        ACriterio := ACriterio + ['"' + Campo + '": "' + Valor + '"'];
      end else
      if not Valor.ToLower().Equals('null') then // Não nulo
      begin
        if Campo.Equals('texto') then
        begin
          Campo := 'nome';
          Valor := '{"$regex": "' + Valor.Replace('*', '') + '", "$options" : "i"}';
        end else
        if not Valor.StartsWith('$') then // Número
        begin
          Valor := Valor.Replace('"', '''').Replace('%', '*');
          if Valor.StartsWith('*') then
            Valor := '{"$regex": "' + Valor.Replace('*', '') + '$", "$options" : "i"}'
          else if Valor.EndsWith('*') then
            Valor := '{"$regex": "^' + Valor.Replace('*', '') + '", "$options" : "i"}'
          else
            Valor := '{"$regex": "' + Valor + '", "$options" : "i"}';
        end else
          Valor := Valor.Substring(1);
        ACriterio := ACriterio + ['"' + Campo + '": ' + Valor];
      end else
        ACriterio := ACriterio + ['"' + Campo + '": ' + Valor];
    end;
  ACriterio := ACriterio + ['"exclusao": null'];
  Find.Match('{' + String.Join(', ', ACriterio) + '}');
  // Ordem
  if Ordem > '' then
  begin
    var ASort: TArray<string> := [];
    for var Elemento in Ordem.Split([',', ';']) do
      ASort := ASort + ['"' + Elemento.Trim() + '": 1'];
    Find.Sort('{' + String.Join(', ', ASort) + '}');
  end;
  // Resultado
  var oCrs := Find.Open();
  while oCrs.Next do
    Result.Add(TJSONObject(TJSONObject.ParseJSONValue(oCrs.Doc.AsJSON)));
end;

class function TDAO.Post<T>(Colecao: string; Instancia: TObject): T;
begin
  //
end;

class function TDAO.Replace<T>(Colecao: string; Instancia: TObject): T;
begin
  //
end;

class function TDAO.GetArquivo(Nome, Arquivo: string; var FileName: string): Boolean;
begin
  Result := InfoAPI().GetArquivo(Nome, Arquivo, FileName);
end;

class function TDAO.GetArquivoDisponivel(Nome, Arquivo: string; var FileName: string): Boolean;
begin
  Result := InfoAPI().GetArquivoDisponivel(Nome, Arquivo, FileName);
end;

var
  fdbname: string = '';

class function TDAO.DBName(): string;
begin
  if fdbname > '' then
    Exit(fdbname);
  var id := InfoDispositivo().empresa.id;
  id := id.Substring(0, id.IndexOf('-') - 1);
  var nome := PlainText(InfoDispositivo().empresa.nome.ToLower());
  if nome.Contains(' ') then
    nome := nome.Substring(0, nome.IndexOf(' ')).Substring(0, 20);
  var i := 0;
  while i < nome.Length do
  begin
    if not (nome.Substring(i, 1).ToCharArray()[0] in ['a'..'z']) then
    begin
      nome := nome.Remove(i, 1);
      nome := nome.Insert(i, '_');
    end;
    Inc(i);
  end;
  fdbname := 'constel-' + id + '-' + nome;
  Result := fdbname;
end;

class function TDAO.Collection(CollectionName: string): TMongoCollection;
begin
  Result := _AppModule.GetCollection(TDAO.DBName(), CollectionName);
end;

class function TDAO.GetConfig(Key: string): string;
begin
  var Collection := Collection('sistema.configuracao');
  var Find := Collection.Find();
  Find.Project('{"_id": 0, "chave": 1, "valor": 1}');
  Find.Match('{"chave": "' + key + '"}');
  var oCrs := Find.Open();
  if oCrs.Next then
  begin
    var JSONGravado := TJSONObject.ParseJSONValue(oCrs.Doc.AsJSON);
    try
      Exit(JSONGravado.GetValue<string>('valor', ''));
    finally
      JSONGravado.Free();
    end;
  end;
  Result := '';
end;

class function TDAO.SetConfig(Key, Value: string): string;
begin
  var Collection := Collection('sistema.configuracao');
  var Find := Collection.Find();
  Find.Project('{"_id": 0, "chave": 1, "valor": 1}');
  Find.Match('{"chave": "' + key + '"}');
  var oCrs := Find.Open();
  while oCrs.Next do
  begin
    Collection.Remove()
      .Match('{"chave": "' + Key + '"}')
      .&End
      .Exec();
  end;
  var oDoc := _AppModule.Connection.Env.NewDoc();
  try
    oDoc.Add('id', Funcoes.NewGUID());
    oDoc.Add('edicao', DateToISO8601(Now()));
    oDoc.Add('chave', Key);
    oDoc.Add('valor', Value);
    Collection.Insert(oDoc);
  finally
    oDoc.Free();
  end;
end;

class procedure TDAO.Armazena(ProgressEvent: TClassProgressEvent; Recurso: string);
var
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
begin
  // Referências
  const InicioConfig = TDAO.GetConfig('sincronizacao:' + Recurso);
  var Inicio: TDateTime;
  if not TryISO8601ToDate(InicioConfig, Inicio) then
    Inicio := EncodeDate(2001, 01, 01);
  var UltimaAtualizacao: TDateTime := Inicio;
  var CampoNome := False;
  var CampoSituacao := False;
  var Collection := TDAO.Collection(Recurso.Replace('/', '.'));
  try
    // Dados
    var LastNotification := 0;
    if Assigned(ProgressEvent) then
      ProgressEvent(0, 0);
    var Registro := 1;
    var Registros := 1;
    var Pagina := 1;
    var Paginas := 1;
    while Pagina <= Paginas do
    begin
      JSONValue := InfoAPI().Carga(Recurso + '/carga?pagina=' + Pagina.ToString() + '&linhas=300&inicio=' + DateToISO8601(Inicio + (OneMillisecond * 2)));
      Registros := JSONValue.GetValue<Integer>('registros');
      JSONArray := JSONValue.GetValue<TJSONArray>('lista');
      for var JSONElement in JSONArray do
      begin
        if (Abs(Now() - LastNotification) > (OneSecond / 3)) and Assigned(ProgressEvent) then
          ProgressEvent(Registro, Registros);
        TDAO.ArmazenaDocumento(Collection, JSONElement, UltimaAtualizacao);
        if not CampoNome then
          CampoNome := Assigned(JSONElement.FindValue('nome'));
        if not CampoSituacao then
          CampoSituacao := Assigned(JSONElement.FindValue('situacao'));
        Inc(Registro);
      end;
      Paginas := JSONValue.GetValue<Integer>('paginas', 0);
      Inc(Pagina);
    end;
    // Atualiza a data de referência de sincronização
    TDAO.SetConfig('sincronizacao:' + Recurso, DateToISO8601(UltimaAtualizacao));
    if Registro <= 0 then
      Exit;
    // Índices
    var sl := TStringList.Create();
    sl.Clear();
    var Cursor := Collection.ListIndexes;
    while Cursor.Next do
    begin
      var JSONIndexValue := TJSONObject.ParseJSONValue(Cursor.Doc.AsJSON);
      if not (JSONIndexValue is TJSONObject) then
        Continue;
      var JSONIndexObject := TJSONObject(JSONIndexValue);
      // as chaves ficam na propriedade key
      sl.Add(JSONIndexObject.GetValue<string>('name'));
    end;
    var IndexName := Collection.Name + '_id';
    if sl.IndexOf(IndexName) < 0 then
    begin
      var MongoIndex := TMongoIndex.Create(_AppModule.Connection.Env);
      try
        MongoIndex.Keys('{"id": 1}');
        MongoIndex.Options.Name := Collection.Name + '_id';
        Collection.CreateIndex(MongoIndex);
      finally
        MongoIndex.Free();
      end;
    end;
    IndexName := Collection.Name + '_nome';
    if sl.IndexOf(IndexName) < 0 then
    begin
      var MongoIndex := TMongoIndex.Create(_AppModule.Connection.Env);
      try
        MongoIndex.Keys('{"nome": 1}');
        MongoIndex.Options.Name := Collection.Name + '_nome';
        Collection.CreateIndex(MongoIndex);
      finally
        MongoIndex.Free();
      end;
    end;
    sl.Free();
  except
    on E: Exception do
    begin
      E.Message := 'Erro ao importar a coleção ' + Collection.Name + #10 + E.Message;
//      Collection.CancelBulk();
      raise;
    end;
  end;
end;

class procedure TDAO.ArmazenaDocumento(Collection: TMongoCollection; JSONValue: TJSONValue; var UltimaAtualizacao: TDateTime);
begin
  if not (JSONValue is TJSONObject) then
    raise Exception.CreateFmt('Objeto inválido na coleção %s', [Collection.Name]);
  var JSONObject := TJSONObject(JSONValue);
  var Id := JSONObject.GetValue<string>('id', '');
  if Id.IsEmpty then
    raise Exception.CreateFmt('Objeto inválido na coleção %s, campo "id" não informado', [Collection.Name]);
  var Edicao := JSONObject.GetValue<TDateTime>('edicao', 0);
  if Edicao <= 0 then
  begin
    Edicao := JSONObject.GetValue<TDateTime>('inclusao', 0);
    if Edicao <= 0 then
      raise Exception.CreateFmt('Objeto inválido na coleção %s, campo "edicao" não informado', [Collection.Name]);
  end;
  if UltimaAtualizacao < Edicao then
    UltimaAtualizacao := Edicao;
  var Find := Collection.Find();
  Find.Project('{"_id": 0, "id": 1, "edicao": 1}');
  Find.Match('{"id": "' + id + '"}');
  var oCrs := Find.Open();
  if oCrs.Next then
  begin
    var JSONGravado := TJSONObject.ParseJSONValue(oCrs.Doc.AsJSON);
    if not (JSONGravado.GetValue<TDateTime>('edicao') < Edicao) then // Não foi editado
      Exit;
    Collection.Remove()
      .Match('{"id": "' + id + '"}')
      .&End
      .Exec();
  end;
  var oDoc := _AppModule.Connection.Env.NewDoc();
  try
    oDoc.AsJSON := JSONObject.ToJSON();
    Collection.Insert(oDoc);
  finally
    oDoc.Free();
  end;
end;

//class function TDAO.Iniciado(): Boolean;
//begin
//  var Sintonia := TDAO.Get<TSintonia>('sistema.sintonia');
//  var Inicio := Sintonia.momento;
//  if Inicio <= 0 then
//    Exit(False);
//  Result := True;
//end;

class procedure TDAO.Sincroniza(SincronizacaoBloco: TSincronizacaoBloco; ListProgressEvent: TListProgressEvent; ClassProgressEvent: TClassProgressEvent);
begin
  var sl := TStringList.Create();
  try
    case SincronizacaoBloco of
      sbBloco1:
        begin
          sl.AddPair('agente/estabelecimento', 'Estabelecimento');
          sl.AddPair('agente/parceiro', 'Clientes e Colaboradores');
          sl.AddPair('recurso/categoria', 'Categorias');
          sl.AddPair('recurso/item', 'Produtos/Serviços');
          sl.AddPair('movimento/modalidade', 'Modalidades');
        end;
      sbBloco2:
        begin
          sl.AddPair('financeiro/forma', 'Formas de Pagamento');
          sl.AddPair('estrutura/dispositivo', 'Perfis');
          sl.AddPair('movimento/modalidade', 'Modalidades');
          sl.AddPair('financeiro/moeda', 'Unidades Monetárias');
          sl.AddPair('contabil/conta', 'Contas');
          sl.AddPair('contabil/historico', 'Históricos');
          sl.AddPair('uniao/uf', 'Estados');
          sl.AddPair('uniao/municipio', 'Municípios');
          sl.AddPair('venda/preco', 'Tabelas de Preço');
          sl.AddPair('venda/localizador', 'Localizadores de Atendimentos');
          sl.AddPair('seguranca/usuario', 'Usuários');
        end;
    end;
    var I := 0;
    for var Recurso in sl do
    begin
      var Segmentos := Recurso.Split(['=']);
      if Length(Segmentos) < 2 then
        Continue;
      ListProgressEvent(Segmentos[1], I + 1, sl.Count);
      TDAO.Armazena(ClassProgressEvent, Segmentos[0]);
      Inc(I);
    end;
  finally
    sl.Free();
  end;
end;

constructor TSincroniaThread.Create(MessageLabel: TLabel);
begin
  inherited Create();
  FMessageLabel := MessageLabel;
  FMessageBack := MessageLabel.Caption;
  Self.FreeOnTerminate := True;
end;

procedure TSincroniaThread.Execute();
begin
  NameThreadForDebugging('Sincronizando');
  var sl := TStringList.Create();
  try
    sl.AddPair('financeiro/forma', 'Formas de Pagamento');
    sl.AddPair('estrutura/dispositivo', 'Perfis');
    sl.AddPair('movimento/modalidade', 'Modalidades');
    sl.AddPair('contabil/conta', 'Contas');
    sl.AddPair('contabil/historico', 'Históricos');
    sl.AddPair('uniao/uf', 'Estados');
    sl.AddPair('uniao/municipio', 'Municípios');
    sl.AddPair('uniao/moeda', 'Unidades Monetárias');
    sl.AddPair('venda/preco', 'Tabelas de Preço');
    sl.AddPair('venda/localizador', 'Localizadores de Atendimentos');
    sl.AddPair('seguranca/usuario', 'Usuários');
    var I := 0;
    for var Recurso in sl do
    begin
      var Segmentos := Recurso.Split(['=']);
      if Length(Segmentos) < 2 then
        Continue;
      FMensagem := 'Sincronizando ' + Segmentos[1] + ' ' + Trunc(((I + 1) / sl.Count) * 100).ToString() + '%';
      Synchronize(Contador);
      TDAO.Armazena(nil, Segmentos[0]);
      Inc(I);
      if Terminated then
        Exit;
    end;
  finally
    sl.Free();
  end;
  FMensagem := FMessageBack;
  Synchronize(Contador);
end;

procedure TSincroniaThread.Contador();
begin
  FMessageLabel.Caption := FMensagem;
end;

end.
