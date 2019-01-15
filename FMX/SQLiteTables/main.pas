unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Grid, FMX.Layouts,
  SQLite3, SQLiteTable3, FMX.Memo, FMX.ListBox;

type
  TForm13 = class(TForm)
    StringGrid1: TStringGrid;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Button2: TButton;
    dlgOpenBase: TOpenDialog;
    ComboBox1: TComboBox;
    Label2: TLabel;
    StyleBook1: TStyleBook;
    StringColumn5: TStringColumn;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FBase: TSQLiteDatabase;
    FStmt: TSQLitePreparedStatement;
    FTable: TSQLiteUniTable;
  public
    procedure DrawTable(ATable:TSQLiteUniTable);
  end;

var
  Form13: TForm13;

implementation

{$R *.fmx}

procedure TForm13.Button1Click(Sender: TObject);
begin
  if dlgOpenBase.Execute then
    begin
      try
        FBase:=TSQLiteDatabase.Create(dlgOpenBase.FileName);
        FBase.GetTableStrings('SELECT name FROM sqlite_master WHERE type="table" ORDER BY name',ComboBox1.Items);
        ShowMessage('Подключились. Названия таблиц прочитали');
      except
        ShowMessage('Не смогли подключиться к базе данных');
      end;
    end;

end;

procedure TForm13.Button2Click(Sender: TObject);
begin
if not Assigned(FStmt) then
  FStmt:=TSQLitePreparedStatement.Create(FBase);
FStmt.SQL:=Memo1.Text;
FTable:=FStmt.ExecQuery();
DrawTable(FTable)
end;

procedure TForm13.ComboBox1Change(Sender: TObject);
var TableName: string;
begin
  TableName:=ComboBox1.Items[ComboBox1.ItemIndex];
  FStmt:=TSQLitePreparedStatement.Create(FBase,'SELECT * FROM '+TableName);
  FTable:=FStmt.ExecQuery;
  Memo1.Lines.Clear;
  Memo1.Lines.Add(FStmt.SQL);
  DrawTable(FTable)
end;

procedure TForm13.DrawTable(ATable: TSQLiteUniTable);
var i:integer;
    delta: integer;
begin
  //смотрим сколько надо удалить/добавить столбуов
  delta:=StringGrid1.ColumnCount-ATable.FieldCount;
  if delta>0 then //в таблице столбцов больше чем надо - удаляем
    begin
      for I := 1 to delta do
        StringGrid1.Columns[StringGrid1.ColumnCount-1].Release;
    end
  else //не хватает столбуов - добавляем
    begin
      for I := 1 to abs(delta) do
        StringGrid1.AddObject(TStringColumn.Create(nil));
    end;
  StringGrid1.RowCount:=0;//обнуляем количество строк
try
  StringGrid1.BeginUpdate;
  //заполняем табличку данными
  while not ATable.EOF do
    begin
      {увеличиваем количество строк на 1}
      StringGrid1.RowCount:=StringGrid1.RowCount+1;
      for I := 0 to ATable.FieldCount-1 do
        begin
        {задаем заголовок столбца}
        StringGrid1.Columns[i].Header:=ATable.Fields[i].Name;
        {выписываем в ячейку последнее значение}
        StringGrid1.Cells[i,ATable.Row-1]:=ATable.FieldAsString(i);
        end;
      ATable.Next
    end;
finally
  StringGrid1.EndUpdate;
end;
end;

procedure TForm13.FormCreate(Sender: TObject);
var i,j: integer;
begin
  for I := 0 to StringGrid1.ColumnCount-1 do
    for j := 0 to StringGrid1.RowCount-1 do
       StringGrid1.Cells[i,j]:=Format('(%d, %d)',[i,j]);
end;

end.
