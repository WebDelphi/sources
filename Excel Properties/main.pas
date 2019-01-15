unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IOUtils, ComObj;

const
  // свойства документа
  msoPropertyTypeBoolean = 2; // Boolean value.
  msoPropertyTypeDate = 3; // Date value.
  msoPropertyTypeFloat = 5; // Floating point value.
  msoPropertyTypeNumber = 1; // Integer value.
  msoPropertyTypeString = 4; // String value.

  StampProp = 'Проверено';

type
  TFileProperty = class
  private
    FItemIndex: integer;
    FCreator: integer;
    FName: string;
    FPropertyType: integer;
    FValue: variant;
    procedure SetCreator(const Value: integer);
    procedure SetItemIndex(const Value: integer);
    procedure SetName(const Value: string);
    procedure SetPropertyType(const Value: integer);
    procedure SetValue(const Value: variant);
  public
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Creator: integer read FCreator write SetCreator;
    property Name: string read FName write SetName;
    property PropertyType: integer read FPropertyType write SetPropertyType;
    property Value: variant read FValue write SetValue;
  end;

type
  TExcelFile = class
  private
    FFileName: string; // имя файла с инвентаризацией
    FExcelObject: OleVariant; // объект Excel
    FListsNames: TStringList;
    FCustomProperties: array of TFileProperty;
    FPropertyList: TStringList;
    procedure SetExcelObject(const Value: OleVariant);
    procedure SetFileName(const Value: string);
    function GetListCount: integer;
    procedure SetPropertyList(const Value: TStringList);
    function GetPropertyList: TStringList;
  public
    constructor Create(const aFileName: string);
    destructor Destroy;
    procedure GetFileInfo;
    procedure ReadProperties;
    function LastCheck(): TDate;
    procedure AddProperty(const aName: string; aType: byte; aValue: variant;
      Rewrite: boolean = true);
    function PropertyExist(const aName: string): integer;
    property FileName: string read FFileName write SetFileName;
    // имя файла с инвентаризацией
    property ExcelObject: OleVariant read FExcelObject write SetExcelObject;
    // объект Excel
    property ListsNames: TStringList read FListsNames;
    property ListCount: integer read GetListCount;
    property PropertyList: TStringList read GetPropertyList;
  end;

type
  TForm11 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    ListBox1: TListBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit2: TEdit;
    ComboBox1: TComboBox;
    Edit3: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form11: TForm11;
  ExcelFile: TExcelFile;

implementation

{$R *.dfm}
{ TExcelFile }

procedure TExcelFile.AddProperty(const aName: string; aType: byte;
  aValue: variant; Rewrite: boolean = true);
var
  index: integer;
begin
  try
    index := PropertyExist(aName);
    if (index > -1) and Rewrite then
      FExcelObject.ActiveWorkBook.CustomDocumentProperties.Item[aName].Delete
    else if (index > -1) and (not Rewrite) then
      Exit;
    case aType of
      msoPropertyTypeBoolean, msoPropertyTypeFloat:
        FExcelObject.ActiveWorkBook.CustomDocumentProperties.Add(Name := aName,
          LinkToContent := false, Type := aType, Value := aValue);
      msoPropertyTypeDate:
        FExcelObject.ActiveWorkBook.CustomDocumentProperties.Add(Name := aName,
          LinkToContent := false, Type := aType,
          Value := VarToDateTime(aValue));
      msoPropertyTypeNumber:
        begin
          index := aValue;
          FExcelObject.ActiveWorkBook.CustomDocumentProperties.Add
            (Name := aName, LinkToContent := false, Type := aType,
            Value := index);
        end;
      msoPropertyTypeString:
        FExcelObject.ActiveWorkBook.CustomDocumentProperties.Add(Name := aName,
          LinkToContent := false, Type := aType, Value := VarToStr(aValue));
    end;
    ReadProperties;
  except
    MessageBox(0, PChar('Свойство ' + aName + ' уже записано'), 'Ошибка',
      MB_OK + MB_ICONERROR)
  end;
end;

constructor TExcelFile.Create(const aFileName: string);
begin
  inherited Create;
  FListsNames := TStringList.Create;
  if FileExists(aFileName) then
  begin
    FFileName := aFileName;
    GetFileInfo;
  end
  else
  begin
    MessageBox(0, 'Необходимо указать файл с Инвентаризацией', 'Ошибка',
      MB_OK + MB_ICONERROR);
    Destroy;
  end;
end;

destructor TExcelFile.Destroy;
begin
  FreeAndNil(FListsNames);
  if not VarIsNull(FExcelObject) then
  begin
    FExcelObject.Save;
    FExcelObject.Quit;
    FExcelObject := Unassigned;
  end;
end;

procedure TExcelFile.GetFileInfo;
var
  i: integer;
begin
  try
    FExcelObject := GetActiveOleObject('Excel.Application');
  except
    FExcelObject := CreateOleObject('Excel.Application');
  end;
  FExcelObject.DisplayAlerts := false;
  if not VarIsNull(FExcelObject) then
  begin
    FExcelObject.WorkBooks.Open(FFileName);
    for i := 1 to FExcelObject.ActiveWorkBook.Sheets.Count do
      FListsNames.Add(FExcelObject.ActiveWorkBook.Sheets[i].Name);
    ReadProperties;
  end
  else
  begin
    MessageBox(0, 'Ошибка запуска Excel', 'Ошибка', MB_OK + MB_ICONERROR);
    Destroy;
  end;
end;

function TExcelFile.GetListCount: integer;
begin
  Result := FListsNames.Count;
end;

function TExcelFile.GetPropertyList: TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to Length(FCustomProperties) - 1 do
    Result.Add(FCustomProperties[i].FName + ' = ' +
      VarToStr(FCustomProperties[i].FValue));
end;

function TExcelFile.LastCheck: TDate;
var
  i: integer;
begin
  Result := 0;
  if PropertyExist(StampProp) > -1 then
    Result := FCustomProperties[PropertyExist(StampProp)].FValue
end;

function TExcelFile.PropertyExist(const aName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Length(FCustomProperties) - 1 do
  begin
    if LowerCase(aName) = LowerCase(FCustomProperties[i].FName) then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TExcelFile.ReadProperties;
var
  PropVar: OleVariant;
  i: integer;
begin
  FCustomProperties := nil;
  for i := 1 to FExcelObject.ActiveWorkBook.CustomDocumentProperties.Count do
  begin
    SetLength(FCustomProperties, Length(FCustomProperties) + 1);
    FCustomProperties[Length(FCustomProperties) - 1] := TFileProperty.Create;
    PropVar := FExcelObject.ActiveWorkBook.CustomDocumentProperties.Item[i];
    FCustomProperties[Length(FCustomProperties) - 1].FItemIndex := i;
    FCustomProperties[Length(FCustomProperties) - 1].FCreator :=
      PropVar.Creator;
    FCustomProperties[Length(FCustomProperties) - 1].FName := PropVar.Name;
    FCustomProperties[Length(FCustomProperties) - 1].FPropertyType :=
      PropVar.Type;
    FCustomProperties[Length(FCustomProperties) - 1].FValue := PropVar.Value;
  end;
end;

procedure TExcelFile.SetExcelObject(const Value: OleVariant);
begin
  FExcelObject := Value;
end;

procedure TExcelFile.SetFileName(const Value: string);
begin
  FFileName := Value;
  GetFileInfo;
end;

procedure TExcelFile.SetPropertyList(const Value: TStringList);
begin

end;

{ TFileProperty }

procedure TFileProperty.SetCreator(const Value: integer);
begin
  FCreator := Value;
end;

procedure TFileProperty.SetItemIndex(const Value: integer);
begin
  FItemIndex := Value;
end;

procedure TFileProperty.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TFileProperty.SetPropertyType(const Value: integer);
begin
  FPropertyType := Value;
end;

procedure TFileProperty.SetValue(const Value: variant);
begin
  FValue := Value;
end;

procedure TForm11.Button1Click(Sender: TObject);
var
  i: integer;
begin
  if OpenDialog1.Execute then
  begin
    Edit1.Text := OpenDialog1.FileName;
    ExcelFile := TExcelFile.Create(OpenDialog1.FileName);
    ExcelFile.ReadProperties;
    ListBox1.Items.Clear;
    ListBox1.Items.Assign(ExcelFile.PropertyList);
  end;
end;

procedure TForm11.Button2Click(Sender: TObject);
begin
  ExcelFile.AddProperty(Edit2.Text, ComboBox1.ItemIndex + 1, Edit3.Text);
  ListBox1.Items.Assign(ExcelFile.PropertyList);
end;

procedure TForm11.FormDestroy(Sender: TObject);
begin
  ExcelFile.Destroy;
end;

end.
