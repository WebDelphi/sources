unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, InvokeRegistry, Rio, SOAPHTTPClient, com_yaspell, StdCtrls;

type
  TForm1 = class(TForm)
    YandexSpeller1: TYandexSpeller;
    Button1: TButton;
    ListBox1: TListBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var Res: TCheckTextResponse;
    Err: TSpellError;
    S: string;
begin
  Res:=YandexSpeller1.CheckText('синхрофазатрон');
  for Err in Res.SpellResult do
    begin
      ListBox1.Items.Add('Ошибка в слове '+Err.word_);
      for s in Err.s do
        ListBox1.Items.Add(s)
    end;
end;

end.
