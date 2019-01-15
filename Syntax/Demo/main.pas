unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, uSyntax;

type
  TForm4 = class(TForm)
    RichEdit1: TRichEdit;
    edFilePath: TEdit;
    btnLoadFile: TButton;
    OpenDialog1: TOpenDialog;
    btnHighLight: TButton;
    procedure btnLoadFileClick(Sender: TObject);
    procedure btnHighLightClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.btnHighLightClick(Sender: TObject);
begin
  HighLight(RichEdit1);
end;

procedure TForm4.btnLoadFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      LoadBuildStops(OpenDialog1.FileName);
      edFilePath.Text:=OpenDialog1.FileName;
    end;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  Colorer.FontSize:=10;
  Colorer.CurrSize:=10;
  Colorer.FontColor:=clRed;
  Colorer.CurrColor:=clBlue;
end;

end.
