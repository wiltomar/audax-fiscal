unit Horse.ErrorInterceptor;

interface

uses
  Horse,
  Horse.Commons;

procedure HandleError(Req: THorseRequest; Res: THorseResponse; Next: TProc);

implementation

uses
  System.SysUtils, System.TypInfo, System.DateUtils, System.JSON, Web.HTTPApp,
  Lib.Excecoes;

procedure HandleError(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  try
    Next();
  except
    on E: Exception do
    begin
      var StatusCode := 500;
      if E is EHttpResponse then
        StatusCode := EHttpResponse(E).StatusCodde;
      var JSONObject := TJSONObject.Create();
      JSONObject.AddPair('statusCode', statusCode);
      JSONObject.AddPair('message', E.Message);
      JSONObject.AddPair('timestamp', System.DateUtils.DateToISO8601(Now()));
      JSONObject.AddPair('path', Req.PathInfo);
      JSONObject.AddPair('method', GetEnumName(TypeInfo(TMethodType), Ord(Req.MethodType)).Substring(2).ToUpper());
      Res.Status(StatusCode).Send(JSONObject.ToJSON());
    end;
  end;
end;

end.
