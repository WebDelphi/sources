unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    ListBox1: TListBox;
    Button1: TButton;
    Edit1: TEdit;
    Panel2: TPanel;
    Label2: TLabel;
    RichEdit1: TRichEdit;
    Label3: TLabel;
    Edit2: TEdit;
    UpDown1: TUpDown;
    ListBox2: TListBox;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Add2Cloud(Tag: string); //добавление тэга в облако
    procedure SortCloud;              //сортировка облака по количеству вхождений тегов (incoming)
    procedure GetSortCloud;           //вывод сортированного облака
    procedure GetCloudFonts;          //определение размера шрифта для каждого тэга
    procedure PaintCloud;             //вывод облака тэгов
  end;

type
  TCloud = record
    Tag      : string;
    Incoming : integer;
    FontSize : integer;
end;

var
  Form1: TForm1;
  Cloud   : array of TCloud;
  min_incom: integer; //минимальное число вхождений
  min_font: integer;

implementation

{$R *.dfm}

procedure TForm1.Add2Cloud(Tag: string);
var i:integer;
    incloud: boolean;
begin
  for I:=0 to length(Cloud)-1 do
    begin
      if Cloud[i].Tag=Tag then
        begin
          inc(Cloud[i].Incoming);
          incloud:=true;
          break;
        end;
    end;
if not incloud then
  begin
    setlength(Cloud, length(Cloud)+1);
    Cloud[length(Cloud)-1].Tag:=Tag;
    Cloud[length(Cloud)-1].Incoming:=1;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var i:integer;
begin
ListBox1.Items.Add(Edit1.Text);
Cloud:=nil;
min_font:=StrToInt(Edit2.Text);
for i:=0 to ListBox1.Count-1 do
  Add2Cloud(ListBox1.Items.Strings[i]);
GetSortCloud;
GetCloudFonts;
PaintCloud;
end;

procedure TForm1.GetCloudFonts;
var i:integer;
begin
//определяем минимальное и максимальное число вхождений тегов в облако
min_incom:=Cloud[0].Incoming;
for i:=0 to Length(Cloud)-1 do
  if Cloud[i].Incoming<min_incom then
    min_incom:=Cloud[i].Incoming;
//назначаем размер шрифта для каждой грурры
for i:=0 to Length(Cloud)-1 do
  begin
    if Cloud[i].Incoming=min_incom then
      Cloud[i].FontSize:=min_font
    else
      Cloud[i].FontSize:=min_font+Cloud[i].incoming
  end;
end;

procedure TForm1.GetSortCloud;
var i:integer;
begin
SortCloud; //на всякий случай сортируем облако
ListBox2.Clear;
for i:=0 to Length(Cloud)-1 do
   ListBox2.Items.Add(Cloud[i].Tag+' ('+IntToStr(Cloud[i].Incoming)+')');
end;

procedure TForm1.PaintCloud;
var i:integer;
begin
  RichEdit1.Clear;
  for i:=0 to Length(Cloud)-1 do
    begin
      RichEdit1.SelAttributes.Size:=Cloud[i].FontSize;
      Randomize;
      RichEdit1.SelAttributes.Color:=RGB(random(255),random(255),random(255));
      RichEdit1.SetSelText(Cloud[i].Tag+' ('+IntToStr(Cloud[i].Incoming)+')'+', ');
    end;
end;

procedure TForm1.SortCloud;
var i,j: integer;
    Temp: TCloud;
begin
{нулевой проход}
//проходим от последнего элемента к первому
for i:=length(Cloud)-1 downto 1 do
  begin
    {нижний элемент меньше ("легче") верхнего - поднимаем его выше}
    if Cloud[i].Incoming<Cloud[i-1].Incoming then
      begin
        Temp:=Cloud[i];
        Cloud[i]:=Cloud[i-1];
        Cloud[i-1]:=Temp;
      end;
  end;
//сортируем оставшуюся часть массива
i:=length(Cloud)-1;
repeat
//проход от последнего до j-го элемента сверху
  for j:=length(Cloud)-1 downto length(Cloud)-i do
    begin
      if Cloud[j].Incoming<Cloud[j-1].Incoming then
        begin
          Temp:=Cloud[j];
          Cloud[j]:=Cloud[j-1];
          Cloud[j-1]:=Temp;
        end;
    end;
  i:=i-1;
until i<=0;

end;

end.
