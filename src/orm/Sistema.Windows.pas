unit Sistema.Windows;

interface

uses
  System.SysUtils;

  function MachineId(): string;

implementation

uses ActiveX, ComObj, Variants;

var
  FMachineID: string = '';

function GetBIOSInfo(): string;
const
  WbemUser            = '';
  WbemPassword        = '';
  WbemComputer        = 'localhost';
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject   : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;
begin;
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer(WbemComputer, 'root\CIMV2', WbemUser, WbemPassword);
  FWbemObjectSet:= FWMIService.ExecQuery('SELECT SerialNumber FROM Win32_BIOS','WQL', wbemFlagForwardOnly);
  oEnum         := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  if oEnum.Next(1, FWbemObject, iValue) = 0 then
    Exit(string(FWbemObject.SerialNumber));
  raise Exception.Create('Não é possível detectar o ID da BIOS deste equipamento');
end;


function MachineId(): string;
begin
  if FMachineID > '' then
    Exit(FMachineID);
  try
    CoInitialize(nil);
    try
      FMachineID := GetBIOSInfo();
      Exit(FMachineID);
    finally
      CoUninitialize();
    end;
  except
    on E: EOleException do
    begin
      E.Message := Format('EOleException %s %x', [E.Message,E.ErrorCode]);
      raise;
    end;
    on E: Exception do
    begin
      E.Message := E.Classname + ':' + E.Message;
      raise;
    end;
  end;
end;

end.
