unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.FMTBcd, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids, Data.DB, Datasnap.Provider,
  Datasnap.DBClient, Data.SqlExpr, Data.DBXDataSnap, IndyPeerImpl,
  Data.DBXCommon, Datasnap.DSHTTPLayer;

type
  TForm19 = class(TForm)
    SQLConnection1: TSQLConnection;
    SqlServerMethod1: TSqlServerMethod;
    ClientDataSet1: TClientDataSet;
    DataSetProvider1: TDataSetProvider;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    CheckBox1: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form19: TForm19;

implementation

{$R *.dfm}

procedure TForm19.CheckBox1Click(Sender: TObject);
begin
ClientDataSet1.Active:=CheckBox1.Checked;
end;

end.
