unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DBXDataSnap, IndyPeerImpl,
  Data.DBXCommon, Datasnap.DSHTTPLayer, Data.SqlExpr, Data.DB,
  Datasnap.DBClient, Datasnap.DSConnect,ServerMethodsUnit, Vcl.ExtCtrls,
  Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls;

type
  TForm18 = class(TForm)
    DSProviderConnection1: TDSProviderConnection;
    SQLConnection1: TSQLConnection;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form18: TForm18;

implementation

{$R *.dfm}

procedure TForm18.Button1Click(Sender: TObject);
begin
ClientDataSet1.ApplyUpdates(0);
end;

procedure TForm18.CheckBox1Click(Sender: TObject);
begin
  ClientDataSet1.Active:=CheckBox1.Checked;
end;

end.
