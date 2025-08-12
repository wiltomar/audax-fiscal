unit Lib.Sistema.DAO;

interface

uses System.SysUtils, System.Classes, System.JSON, REST.JSON,
  FireDAC.Phys.MongoDBWrapper, Vcl.StdCtrls, APIService;

type
  TDAO = class
  public
    class function GetArquivo(Nome, Arquivo: string; var FileName: string): Boolean;
  end;

implementation

{ TDAO }

class function TDAO.GetArquivo(Nome, Arquivo: string;
  var FileName: string): Boolean;
begin
  Result := InfoAPI().GetArquivo(Nome, Arquivo, FileName);
end;

end.
