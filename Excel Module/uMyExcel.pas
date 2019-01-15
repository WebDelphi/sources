{uMyExcel - модуль для выполнения основных операций с MS Excel

 Разработчик Vlad
 Сайт http://webdelphi.ru
 E-mail: vlad383@mail.ru

 Описание модуля:
 Вся работа ведется через переменную MyExcel
   [+] CheckExcelInstall - проверяет установлен ли Excel на компьютре
   [+] CheckExcelRun - проверяет есть ил запущенный экземпляр Excel и, если таковой
                   обнаруживается - получает ссылку на него для дальнейшей работы
   [+] RunExcel() - запускает Excel
       DisableAlerts - включает/отключает сообщения Excel (рекомендуется
                       использовать значение по-умолчанию);
       Visible - определяет будет ли показано окно Excel после открытия
   [+] StopExcel - останавливает Excel без каких-либо сохранений документа
   [+] AddWorkBook - добавляет рабочую книгу в запущенном Excel, если параметр
                 AutoRun=true, то сначала запускает Excel (если он не запущен)
                 и затем добавляет пустую книгу
   [+] GetAllWorkBooks - перечисляет все рабочие книги, которые открыты в данный
                     момент в Excel в список заносятся полные названия книг
   [+] SaveWorkBook - сохраняет рабочую книгу с индексом WBIndex в файл с названием
                  FileName (индекс первой открытой рабочей книги всегда 1)
   [+] OpenWorkBook - открывает файл рабочей книги FileName; Visible - определяет
                  будет ли окно Excel показано пользователю

 }
unit uMyExcel;

interface

uses ComObj, ActiveX, Variants, Windows, Messages, SysUtils, Classes;

const ExcelApp = 'Excel.Application';

function CheckExcelInstall:boolean;
function CheckExcelRun: boolean;
function RunExcel(DisableAlerts:boolean=true; Visible: boolean=false): boolean;
function StopExcel:boolean;
function AddWorkBook(AutoRun:boolean=true):boolean;
function GetAllWorkBooks:TStringList;
function SaveWorkBook(FileName:TFileName; WBIndex:integer):boolean;
function OpenWorkBook(FileName:TFileName; Visible:boolean=true):boolean;

var MyExcel: OleVariant;

implementation

function OpenWorkBook(FileName:TFileName; Visible:boolean=true):boolean;
begin
  try
    MyExcel.WorkBooks.Open(FileName);
    MyExcel.Visible:=Visible;
    Result:=true;
  except
    result:=false;
  end;
end;

function GetAllWorkBooks:TStringList;
var i:integer;
begin
try
  Result:=TStringList.Create;
  for i:=1 to MyExcel.WorkBooks.Count do
    Result.Add(MyExcel.WorkBooks.Item[i].FullName)
except
  MessageBox(0,'Ошибка перечисления открытых книг','Ошибка',MB_OK+MB_ICONERROR);
end;
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


end.
