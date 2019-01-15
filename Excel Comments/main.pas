unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComObj, ActiveX, StdCtrls;

const
  ExcelApp = 'Excel.Application';

type
  TForm8 = class(TForm)
    CommentBtn: TButton;
    procedure CommentBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form8: TForm8;
  MyExcel: OleVariant;

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

function AddWorkBook(AutoRun:boolean=true):boolean;
begin
  if CheckExcelRun then
    begin
      MyExcel.WorkBooks.Add;
      Result:=true;
    end
  else
   if AutoRun then
     begin
       RunExcel;
       MyExcel.WorkBooks.Add;
       Result:=true;
     end
   else
     Result:=false;
end;

function SaveWorkBook(FileName:TFileName; WBIndex:integer):boolean;
begin
  try
    MyExcel.WorkBooks.Item[WBIndex].SaveAs(FileName);
    if MyExcel.WorkBooks.Item[WBIndex].Saved then
      Result:=true
    else
      Result:=false;
  except
    Result:=false;
  end;
end;

function StopExcel:boolean;
begin
  try
    if MyExcel.Visible then MyExcel.Visible:=false;
    MyExcel.Quit;
    MyExcel:=Unassigned;
    Result:=True;
  except
    Result:=false;
  end;
end;

procedure TForm8.CommentBtnClick(Sender: TObject);
var MyRange: OleVariant;
begin
  if RunExcel then //Excel запущен
    begin
      if AddWorkBook then //добавили рабочую книгу
        begin
          MyRange:=MyExcel.ActiveSheet.Range['A1'];//получили ссылку на ячейку
          MyRange.ClearComments;//почистили на всякий случай комментарии
          MyRange.AddComment('Comment Text');
          {задаем новый цвет фона}
          MyRange.Comment.Shape.Fill.ForeColor.RGB:=RGB(255, 0, 0);
          MyRange.Comment.Shape.Fill.UserPicture(ExtractFilePath(Application.ExeName)+'2.gif');

          MyExcel.Visible:=true;
        end;
    end;
end;

end.
