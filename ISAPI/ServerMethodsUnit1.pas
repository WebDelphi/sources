unit ServerMethodsUnit1;

interface

uses System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth,
  Data.DB, Data.SqlExpr, Data.DBXMySQL, Data.FMTBcd, Datasnap.Provider;

type
  TServerMethods1 = class(TDSServerModule)
    SQLConnection1: TSQLConnection;
    SQLDataSet1: TSQLDataSet;
    DataSetProvider1: TDataSetProvider;
  private
    { Private declarations }
  public
    { Public declarations }
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    function GetDataSet: TDataSet;
    function AnyFunc(Value: string):string;
  end;

implementation

{$R *.dfm}

uses System.StrUtils;

function TServerMethods1.AnyFunc(Value: string): string;
begin
  Result:=Value
end;

function TServerMethods1.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TServerMethods1.GetDataSet: TDataSet;
begin
  SQLDataSet1.Open;
  Result:=SQLDataSet1;
end;

function TServerMethods1.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

end.

