{uMyExcel - ������ ��� ���������� �������� �������� � MS Excel

 ����������� Vlad
 ���� http://webdelphi.ru
 E-mail: vlad383@mail.ru

 �������� ������:
 ��� ������ ������� ����� ���������� MyExcel
   [+] CheckExcelInstall - ��������� ���������� �� Excel �� ���������
   [+] CheckExcelRun - ��������� ���� �� ���������� ��������� Excel �, ���� �������
                   �������������� - �������� ������ �� ���� ��� ���������� ������
   [+] RunExcel() - ��������� Excel
       DisableAlerts - ��������/��������� ��������� Excel (�������������
                       ������������ �������� ��-���������);
       Visible - ���������� ����� �� �������� ���� Excel ����� ��������
   [+] StopExcel - ������������� Excel ��� �����-���� ���������� ���������
   [+] AddWorkBook - ��������� ������� ����� � ���������� Excel, ���� ��������
                 AutoRun=true, �� ������� ��������� Excel (���� �� �� �������)
                 � ����� ��������� ������ �����
   [+] GetAllWorkBooks - ����������� ��� ������� �����, ������� ������� � ������
                     ������ � Excel � ������ ��������� ������ �������� ����
   [+] SaveWorkBook - ��������� ������� ����� � �������� WBIndex � ���� � ���������
                  FileName (������ ������ �������� ������� ����� ������ 1)
   [+] OpenWorkBook - ��������� ���� ������� ����� FileName; Visible - ����������
                  ����� �� ���� Excel �������� ������������

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
  MessageBox(0,'������ ������������ �������� ����','������',MB_OK+MB_ICONERROR);
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
  // ���� CLSID OLE-�������
  Rez := CLSIDFromProgID(PWideChar(WideString(ExcelApp)), ClassID);
  if Rez = S_OK then  // ������ ������
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
  {��������� ���������� �� Excel}
 if CheckExcelInstall then
   begin
     MyExcel:=CreateOleObject(ExcelApp);
     //����������/�� ���������� ��������� ��������� Excel (����� �� ����������)
     MyExcel.Application.EnableEvents:=DisableAlerts;
     MyExcel.Visible:=Visible;
     Result:=true;
   end
 else
   begin
     MessageBox(0,'���������� MS Excel �� ����������� �� ���� ����������','������',MB_OK+MB_ICONERROR);
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
