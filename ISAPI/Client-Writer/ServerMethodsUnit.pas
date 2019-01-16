// 
// Created by the DataSnap proxy generator.
// 26.08.2012 1:14:12
// 

unit ServerMethodsUnit;

interface

uses Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXJSONReflect;

type
  TServerMethods1Client = class(TDSAdminClient)
  private
    FEchoStringCommand: TDBXCommand;
    FReverseStringCommand: TDBXCommand;
    FGetDataSetCommand: TDBXCommand;
    FAnyFuncCommand: TDBXCommand;
  public
    constructor Create(ADBXConnection: TDBXConnection); overload;
    constructor Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    function GetDataSet: TDataSet;
    function AnyFunc(Value: string): string;
  end;

implementation

function TServerMethods1Client.EchoString(Value: string): string;
begin
  if FEchoStringCommand = nil then
  begin
    FEchoStringCommand := FDBXConnection.CreateCommand;
    FEchoStringCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FEchoStringCommand.Text := 'TServerMethods1.EchoString';
    FEchoStringCommand.Prepare;
  end;
  FEchoStringCommand.Parameters[0].Value.SetWideString(Value);
  FEchoStringCommand.ExecuteUpdate;
  Result := FEchoStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.ReverseString(Value: string): string;
begin
  if FReverseStringCommand = nil then
  begin
    FReverseStringCommand := FDBXConnection.CreateCommand;
    FReverseStringCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FReverseStringCommand.Text := 'TServerMethods1.ReverseString';
    FReverseStringCommand.Prepare;
  end;
  FReverseStringCommand.Parameters[0].Value.SetWideString(Value);
  FReverseStringCommand.ExecuteUpdate;
  Result := FReverseStringCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethods1Client.GetDataSet: TDataSet;
begin
  if FGetDataSetCommand = nil then
  begin
    FGetDataSetCommand := FDBXConnection.CreateCommand;
    FGetDataSetCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FGetDataSetCommand.Text := 'TServerMethods1.GetDataSet';
    FGetDataSetCommand.Prepare;
  end;
  FGetDataSetCommand.ExecuteUpdate;
  Result := TCustomSQLDataSet.Create(nil, FGetDataSetCommand.Parameters[0].Value.GetDBXReader(False), True);
  Result.Open;
  if FInstanceOwner then
    FGetDataSetCommand.FreeOnExecute(Result);
end;

function TServerMethods1Client.AnyFunc(Value: string): string;
begin
  if FAnyFuncCommand = nil then
  begin
    FAnyFuncCommand := FDBXConnection.CreateCommand;
    FAnyFuncCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FAnyFuncCommand.Text := 'TServerMethods1.AnyFunc';
    FAnyFuncCommand.Prepare;
  end;
  FAnyFuncCommand.Parameters[0].Value.SetWideString(Value);
  FAnyFuncCommand.ExecuteUpdate;
  Result := FAnyFuncCommand.Parameters[1].Value.GetWideString;
end;


constructor TServerMethods1Client.Create(ADBXConnection: TDBXConnection);
begin
  inherited Create(ADBXConnection);
end;


constructor TServerMethods1Client.Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ADBXConnection, AInstanceOwner);
end;


destructor TServerMethods1Client.Destroy;
begin
  FreeAndNil(FEchoStringCommand);
  FreeAndNil(FReverseStringCommand);
  FreeAndNil(FGetDataSetCommand);
  FreeAndNil(FAnyFuncCommand);
  inherited;
end;

end.
