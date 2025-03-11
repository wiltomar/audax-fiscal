unit APIService;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  REST.Types,
  REST.Client,
  REST.JSON;

type
  TAPIService = class
  private
    FServidor: string;
    FToken: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    procedure Request(Resource: string; Method: TRestRequestMethod; ResponseType: TTypeKind; Instance: TObject = nil; Options: TJsonOptions = []);
    procedure CheckResponse(InvalidResponse: Boolean = False);
  public
    constructor Create(const serverName: string = '');
    procedure Autentica(Token: string);
    function BodyToObject<T: class, constructor>(JSON: string): T;
    function Get<T: class, constructor>(Resource, Id: string): T;
    function GetArray<T: class, constructor>(Resource: string): TArray<T>;
    function GetPagedArray<T: class, constructor>(Resource: string; Linhas: Integer = 144): TArray<T>;
    function Post<T: class, constructor>(Resource: string; Instance: TObject): T;
  end;

function InfoAPI(const serverName: string = ''): TAPIService;

implementation

uses System.DateUtils, Lib.Funcoes, Lib.Excecoes;

var
  FAPIService: TAPIService;

function InfoAPI(const serverName: string = ''): TAPIService;
begin
  if Assigned(FAPIService) then
    Exit(FAPIService);
//  if (DebugHook <> 0) or (serverName <> '') then
//      FAPIService := TAPIService.Create('https://192.168.1.8:3000/api/')
//  else
    FAPIService := TAPIService.Create();
  Result := FAPIService;
end;

constructor TAPIService.Create(const serverName: string = '');
begin
  inherited Create();
  if serverName = '' then
    FServidor := 'https://sirius.constel.builders/api/'
  else
    FServidor := serverName;

  FRESTClient := TRESTClient.Create(nil);
  with FRESTClient do
  begin
    Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
    AcceptCharset := 'utf-8, *;q=0.8';
    SecureProtocols := [];
    ContentType := 'application/json';
    Params.Clear();
    RaiseExceptionOn500 := False;
  end;
  FRESTResponse := TRESTResponse.Create(nil);
  with FRESTResponse do
  begin
    ContentType := 'application/json';
  end;
  FRESTRequest := TRESTRequest.Create(nil);
  with FRESTRequest do
  begin
    Client := FRESTClient;
    Method := rmPOST;
    Response := FRESTResponse;
    SynchronizedEvents := False;
  end;
end;

procedure TAPIService.Autentica(Token: string);
begin
  if Token.Trim().IsEmpty then
    EHttpResponse.BadRequest('credencial não fornecida');
  var Segmentos := Token.Split([' ']);
  if Length(Segmentos) <> 2 then
    EHttpResponse.BadRequest('credencial inválida');
  FToken := Segmentos[1];
end;

procedure TAPIService.Request(Resource: string; Method: TRestRequestMethod; ResponseType: TTypeKind; Instance: TObject = nil; Options: TJsonOptions = []);
  procedure SubConfiavel(JSONObject: TJSONObject);
  begin
    for var I := 0 to JSONObject.Count - 1 do
    begin
      if JSONObject.Pairs[I].JSONValue is TJSONObject then
      begin
        SubConfiavel(TJSONObject(JSONObject.Pairs[I].JSONValue));
        Continue;
      end;
      if JSONObject.Pairs[I].JSONValue is TJSONArray then
      begin
        for var J := 0 to TJSONArray(JSONObject.Pairs[I].JSONValue).Count - 1 do
          if TJSONArray(JSONObject.Pairs[I].JSONValue).Items[J] is TJSONObject then
            SubConfiavel(TJSONObject(TJSONArray(JSONObject.Pairs[I].JSONValue).Items[J]));
        Continue;
      end;
      var JSONName := JSONObject.Pairs[I].JSONString.ToString().ToLower().Replace('"', '');
      if JSONName.Contains('senha') or JSONName.Contains('password') then
      begin
        var Nome := JSONObject.Pairs[I].JSONString.ToString().Replace('"', '');
        JSONObject.RemovePair(Nome);
        JSONObject.AddPair(Nome, '*****');
      end;
    end;
  end;
const
  ERRO_CONEXAO = 'Error sending data: (12029)';
begin
  FRESTClient.ResetToDefaults();
  FRESTRequest.ResetToDefaults();
  FRESTResponse.ResetToDefaults();
  FRESTClient.BaseURL := FServidor + Resource;
  FRESTRequest.Method := Method;
  FRESTRequest
    .Params
    .AddHeader('Authorization', 'Bearer ' + FToken)
    .Options := [poDoNotEncode];
  while True do
  begin
    try
      var JSONBody: TJSONObject := nil;
      if Assigned(Instance) then
      begin
        if Options = [] then
          Options := [joIgnoreEmptyStrings, joIgnoreEmptyArrays, joDateIsUTC, joDateFormatISO8601];
        JSONBody := TJson.ObjectToJsonObject(Instance, Options);
        FRESTRequest.AddBody(JSONBody);
      end;
      var Start := Now();
      try
        FRESTRequest.Timeout := 12000000;
        FRESTRequest.ConnectTimeout := 12000000;
        FRESTRequest.ReadTimeout := 12000000;
        FRESTRequest.Execute();
      finally
        {$IFDEF MSWINDOWS}
          Lib.Funcoes.AppendLog('Requisição - ' + System.DateUtils.MilliSecondsBetween(Now(), Start).ToString().PadLeft(4) + ' ms ' + RESTRequestMethodToString(Method) + ' ' + FRESTClient.BaseURL);
        {$ENDIF}
        if Assigned(JSONBody) then
        begin
          SubConfiavel(JSONBody);
          {$IFDEF MSWINDOWS}
            Lib.Funcoes.AppendLogJSON(JSONBody.Format());
          {$ENDIF}
        end;
      end;
      Break;
    except
      on E: Exception do
      begin
        {$IFDEF MSWINDOWS}
          Lib.Funcoes.AppendLogError(E.ClassName + ': ' + FRESTClient.BaseURL + ' ' + E.Message + ' ' + TJSON.ObjectToJsonObject(E).ToJSON());
        {$ENDIF}
        if E is ERESTException then
        begin
          if
            (E is ERESTException)
            and (ERESTException(E).Message.Contains('500 Internal Server Error'))
          then
            CheckResponse();
        end;
        raise;
      end;
    end;
  end;
  CheckResponse();
  case ResponseType of
    tkClass:
      if not (Assigned(FRESTResponse.JSONValue) and (FRESTResponse.JSONValue is TJSONObject)) then
        CheckResponse(True);
    tkArray:
      if not (Assigned(FRESTResponse.JSONValue) and (FRESTResponse.JSONValue is TJSONArray)) then
        CheckResponse(True);
  end;
end;

procedure TAPIService.CheckResponse(InvalidResponse: Boolean = False);
begin
  if FRESTResponse.StatusCode in [200, 201] then
    Exit;
  var Metodo := RESTRequestMethodToString(FRESTRequest.Method);
  var JSONResponse := TJSONObject(FRESTResponse.JSONValue);
  if InvalidResponse then
  begin
    Lib.Funcoes.AppendLogError(Format('Exceção HTTP - status: %d %s %s', [FRESTResponse.StatusCode, 'Invalid format', JSONResponse.ToJSON()]));
    Exit;
  end;
  if Assigned(JSONResponse.GetValue('message')) then
  begin
    Lib.Funcoes.AppendLogError(Format('Exceção HTTP - status: %d %s %s', [FRESTResponse.StatusCode, JSONResponse.GetValue('message').ToString(), JSONResponse.GetValue('error').ToString()]));
    Lib.Funcoes.AppendLogJSON(JSONResponse.Format());
    EHttpResponse.Throw(FRESTResponse.StatusCode, JSONResponse.GetValue('message').ToString(), JSONResponse.GetValue('error').ToString());
  end else
  begin
    Lib.Funcoes.AppendLogError(Format('Exceção HTTP - status: %d %s', [FRESTResponse.StatusCode, FRESTResponse.StatusText]));
    Lib.Funcoes.AppendLogJSON(JSONResponse.Format());
    EHttpResponse.Throw(FRESTResponse.StatusCode, FRESTResponse.StatusText, '');
  end;
end;

function TAPIService.BodyToObject<T>(JSON: string): T;
begin
  JSON := JSON.Trim();
  if JSON.IsEmpty then
    EHttpResponse.ParamMissing('body');
  var JSONValue := TJSONObject.ParseJSONValue(JSON);
  if not (JSONValue is TJSONObject) then
    EHttpResponse.BadRequest('body não é um objeto JSON válido');
  Result := TJSON.JsonToObject<T>(TJSONObject(JSONValue));
end;

function TAPIService.Get<T>(Resource, Id: string): T;
begin
  Request(Resource + '/' + Id, rmGET, tkClass);
  Result := TJSON.JsonToObject<T>(TJSONObject(FRESTResponse.JSONValue));
end;

function TAPIService.GetArray<T>(Resource: string): TArray<T>;
begin
  Request(Resource, rmGET, tkArray);
  var I := 0;
  var LArray := TJSONArray(FRESTResponse.JSONValue);
  SetLength(Result, LArray.Count);
  for var LElement in LArray do
    if LElement is TJSONObject then
    begin
      Result[I] := TJSON.JsonToObject<T>(TJSONObject(LElement));
      Inc(I);
    end;

end;

function TAPIService.GetPagedArray<T>(Resource: string; Linhas: Integer = 144): TArray<T>;
begin
  if not Resource.Contains('?') then
    Resource := Resource + '?';
  var Segmentos := Resource.Split(['?']);
  var Base := Segmentos[0];
  var Parametros := Segmentos[1];
  if not Parametros.IsEmpty then
    Parametros := '?' + Parametros + '&'
  else
    Parametros := '?';
  Result := [];
  var Pagina := 1;
  var Paginas := 1;
  while Pagina <= Paginas do
  begin
    Request(Base + Parametros + 'pagina=' + Pagina.ToString() + '&linhas=' + Linhas.ToString(), rmGET, tkClass);
    var JSONArray := FRESTResponse.JSONValue.GetValue<TJSONArray>('lista');
    for var JSONElement in JSONArray do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := TJSON.JsonToObject<T>(TJSONObject(JSONElement), [joDateIsUTC, joDateFormatISO8601]);
    end;
    Paginas := FRESTResponse.JSONValue.GetValue<Integer>('paginas', 0);
    Inc(Pagina);
  end;
end;

function TAPIService.Post<T>(Resource: string; Instance: TObject): T;
begin
  if not Assigned(Instance) then
    EHttpResponse.ParamMissing('instance');
  Request(Resource, rmPOST, tkClass, Instance, []);
  Result := TJSON.JsonToObject<T>(TJSONObject(FRESTResponse.JSONValue));
end;

end.
