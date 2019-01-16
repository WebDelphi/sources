unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, ComObj,
  ActiveX;

const
  cExcelApp = 'Excel.Application';

type
  TForm16 = class(TForm)
    StringGrid1: TStringGrid;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    edFile: TEdit;
    btnFile: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    Label2: TLabel;
    Button1: TButton;
    Button3: TButton;
    procedure btnFileClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    ExcelApp: OLEVariant;
    function CheckExcelRun: boolean;
    function RunExcel(DisableAlerts: boolean = true;
      Visible: boolean = false): boolean;
    function StopExcel: boolean;

    procedure SlowVariant;
    procedure RangeRead;

    procedure WriteData;
//    procedure RapidWrite;
  public
    { Public declarations }
  end;

var
  Form16: TForm16;

implementation

{$R *.dfm}

procedure TForm16.btnFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    edFile.Text := OpenDialog1.FileName;
end;

procedure TForm16.Button1Click(Sender: TObject);
begin
 if CheckExcelRun then
    begin
    if not StopExcel then
      raise Exception.Create('Ошибка остановки Excel. Завершите процесс Excel вручную и повторите попытку');
    end;
  if RunExcel then
    RangeRead
  else
    raise Exception.Create('Ошибка запуска Excel');
end;

procedure TForm16.Button2Click(Sender: TObject);
begin
  if CheckExcelRun then
    begin
    if not StopExcel then
      raise Exception.Create('Ошибка остановки Excel. Завершите процесс Excel вручную и повторите попытку');
    end;
  if RunExcel then
    SlowVariant
  else
    raise Exception.Create('Ошибка запуска Excel');
end;

procedure TForm16.Button3Click(Sender: TObject);
begin
 if CheckExcelRun then
    begin
    if not StopExcel then
      raise Exception.Create('Ошибка остановки Excel. Завершите процесс Excel вручную и повторите попытку');
    end;
  if RunExcel then
    WriteData
  else
    raise Exception.Create('Ошибка запуска Excel');
end;

function TForm16.CheckExcelRun: boolean;
begin
  try
    ExcelApp := GetActiveOleObject(cExcelApp);
    Result := true;
  except
    Result := false;
  end;
end;

procedure TForm16.FormDestroy(Sender: TObject);
begin
 if CheckExcelRun then
   if not StopExcel then
      raise Exception.Create('Ошибка остановки Excel. Завершите процесс Excel вручную и повторите попытку');
end;

procedure TForm16.RangeRead;
var Rows, Cols, i,j: integer;
    WorkSheet: OLEVariant;
    FData: Variant;
    d: TDateTime;
begin
  //открываем книгу
  ExcelApp.Workbooks.Open(edFile.Text);
  //получаем активный лист
  WorkSheet:=ExcelApp.ActiveWorkbook.ActiveSheet;
  //определяем количество строк и столбцов таблицы
  Rows:=WorkSheet.UsedRange.Rows.Count;
  Cols:=WorkSheet.UsedRange.Columns.Count;

  //считываем данные всего диапазона
  FData:=WorkSheet.UsedRange.Value;

  StringGrid1.RowCount:=Rows;
  StringGrid1.ColCount:=Cols;

//засекаем время начала чтения
  d:=Now;

//выводим данные в таблицу
  for I := 0 to Rows-1 do
    for j := 0 to Cols-1 do
        StringGrid1.Cells[J,I]:=FData[I+1,J+1];

    Label2.Caption:='Время чтения всего листа: '+FormatDateTime('hh:mm:ss:zzz',
    Now()-d);
end;

function TForm16.RunExcel(DisableAlerts, Visible: boolean): boolean;
begin
  try
    ExcelApp := CreateOleObject(cExcelApp);
    ExcelApp.Application.EnableEvents := DisableAlerts;
    ExcelApp.Visible := Visible;
    Result := true;
  except
    Result := false;
  end;
end;

procedure TForm16.SlowVariant;
var Rows, Cols, i,j: integer;
    WorkSheet: OLEVariant;
    d: TDateTime;
begin
  //открываем книгу
  ExcelApp.Workbooks.Open(edFile.Text);
  //получаем активный лист
  WorkSheet:=ExcelApp.ActiveWorkbook.ActiveSheet;
  //определяем количество строк и столбцов таблицы
  Rows:=WorkSheet.UsedRange.Rows.Count;
  Cols:=WorkSheet.UsedRange.Columns.Count;

  StringGrid1.RowCount:=Rows;
  StringGrid1.ColCount:=Cols;

  //засекаем время начала чтения
  d:=Now;

  //выводим данные в таблицу
  for I := 0 to Rows-1 do
    for j := 0 to Cols-1 do
        StringGrid1.Cells[J,I]:=WorkSheet.UsedRange.Cells[I+1,J+1].Value;

 Label2.Caption:='Время чтения всего листа: '+FormatDateTime('hh:mm:ss:zzz',
    Now()-d);
end;

function TForm16.StopExcel: boolean;
begin
  try
    if ExcelApp.Visible then
      ExcelApp.Visible := false;
    ExcelApp.Quit;
    ExcelApp := Unassigned;
    Result := true;
  except
    Result := false;
  end;
end;

procedure TForm16.WriteData;
var i,j: integer;
    FData: Variant;
    Sheet,Range: Variant;
    d: TDateTime;
begin
 d:=Now;
//создаем вариантный массив
  FData:=VarArrayCreate([1,StringGrid1.RowCount,1,StringGrid1.ColCount],varVariant);
//заполняем массив данными из StringGrid
  for i:=1 to VarArrayHighBound(FData,1) do
    for j:=1 to VarArrayHighBound(FData,2) do
      FData[i,j]:=StringGrid1.Cells[J-1,I-1];
{активируем второй лист книги}
//открываем книгу
ExcelApp.Workbooks.Open(edFile.Text);
//активируем
Sheet:=ExcelApp.ActiveWorkBook.Sheets[2];
Sheet.Activate;
//выделяем диапазон для вставки данных
Range:=Sheet.Range[Sheet.Cells[1,1],Sheet.Cells[VarArrayHighBound(FData,1),VarArrayHighBound(FData,2)]];
//вставляем данные
Range.Value:=FData;
//показываем окно Excel
ExcelApp.Visible:=True;

Label2.Caption:='Время записи листа: '+FormatDateTime('hh:mm:ss:zzz',
    Now()-d);
end;

initialization
  ReportMemoryLeaksOnShutdown:=true;

end.
