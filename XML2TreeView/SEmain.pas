unit SEmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls,XMLTree;

type
  TForm5 = class(TForm)
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  Form5: TForm5;
  XMLTree: TXMLTree;

implementation

uses Skins;

{$R *.dfm}

procedure TForm5.FormCreate(Sender: TObject);
begin
XMLTree:=TXMLTree.Create('Skin.xml',@TreeView1);
TreeView1.AutoExpand:=true;
end;

procedure TForm5.ToolButton1Click(Sender: TObject);
begin
XMLTree.SaveTreeToXML;
end;

end.
