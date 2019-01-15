unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,ComObj, ActiveX, StdCtrls;

const ExcelApp = 'Excel.Application';

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListBox1: TListBox;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  MyExcel:OLEVariant;
  RibbonControlBar: OleVariant;

implementation

{$R *.dfm}
function CheckExcelInstall:boolean;
var
  ClassID: TCLSID;
  Rez : HRESULT;
begin
// Ищем CLSID OLE-объекта
  Rez := CLSIDFromProgID(PWideChar(WideString(ExcelApp)), ClassID);
  if Rez = S_OK then  // Объект найден
    Result := true
  else
    Result := false;
end;

function CheckExcelRun: boolean;
begin
  try
    MyExcel:=GetActiveOleObject(ExcelApp);
    Result:=True;
  except
    Result:=false;
  end;
end;

function RunExcel(DisableAlerts:boolean=true; Visible: boolean=false): boolean;
begin
  try
{проверяем установлен ли Excel}
    if CheckExcelInstall then
      begin
        MyExcel:=CreateOleObject(ExcelApp);
//показывать/не показывать системные сообщения Excel (лучше не показывать)
        MyExcel.Application.EnableEvents:=DisableAlerts;
        MyExcel.Visible:=Visible;
        Result:=true;
      end
    else
      begin
        MessageBox(0,'Приложение MS Excel не установлено на этом компьютере','Ошибка',MB_OK+MB_ICONERROR);
        Result:=false;
      end;
  except
    Result:=false;
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
RunExcel(true, true);
ListBox1.Items.Clear;
end;

procedure TForm1.Button2Click(Sender: TObject);
var CommandBar: variant;
   i:integer;
begin
 for i:=1 to MyExcel.CommandBars.Count do
   begin
    CommandBar:=MyExcel.CommandBars.item[i];
    ListBox1.Items.Add(CommandBar.Name);
    if CommandBar.Name='Standard' then
      RibbonControlBar:=CommandBar;
   end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var i:integer;
    NewControls,Bar:Variant;
begin
ListBox1.Items.Clear;
RibbonControlBar.Reset;
NewControls:=RibbonControlBar.Controls.Add(Type:=1,id:=1);
NewControls.Caption:='New Caption';
NewControls.TooltipText:='c:\Windows\notepad.exe';
NewControls.HyperlinkType:=1;
NewControls.FaceId:=10;
NewControls.Style:=5;
RibbonControlBar.Parent.LargeButtons:=true;
if NewControls.BuiltIn then
  ListBox1.Items.Add('BuiltIn')
else
  ListBox1.Items.Add('not BuiltIn')
end;

end.
