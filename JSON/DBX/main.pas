unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Data.DBXJSON;

type
TTaskResource = class
  private
    FId: string;
    FTitle: string;
    FUpdated: TDateTime;
    FSelfLink: string;
    FParent: string;
    FPosition: string;
    FNotes: string;
    FStatus: string;
    FDue: TDateTime;
    FCompleted: TDateTime;
    FDeleted: string;
    FHidden: string;
    procedure SetCompleted(const Value: TDateTime);
    procedure SetDue(const Value: TDateTime);
    procedure SetId(const Value: string);
    procedure SetNotes(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetTitle(const Value: string);
    procedure ParseJSON(Value: TJSONValue);
  public
    constructor Create;
    property Id: string read FId;
    property Title: string read FTitle write SetTitle;
    property Updated: TDateTime read FUpdated;
    property SelfLink: string read FSelfLink;
    property Parent: string read FParent;
    property Position: string read FPosition;
    property Notes: string read FNotes write SetNotes;
    property Status: string read FStatus write SetStatus;
    property Due: TDateTime read FDue write SetDue;
    property Completed: TDateTime read FCompleted write SetCompleted;
    property Deleted: string read FDeleted;
    property Hidden: string read FHidden;
  end;


type
  Tfmain = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    lbPathToFile: TLabel;
    Label1: TLabel;
    cbPairs: TComboBox;
    Label2: TLabel;
    mmPairValue: TMemo;
    Label3: TLabel;
    lbClass: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbPairsChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FJSONObject: TJSONObject;
    procedure EnumPairs();
    function GetBestClassName(AJSONValue: TJSONAncestor):string;
    {примеры перечисления элементов массива}
    procedure EnumArray();
    procedure EnumArray2();
  public

  end;

var
  fmain: Tfmain;

implementation

{$R *.dfm}

procedure Tfmain.Button1Click(Sender: TObject);
var S: TStringList;
begin
  if OpenDialog1.Execute then
    begin
      lbPathToFile.Caption:=OpenDialog1.FileName;
      S:=TStringList.Create;
      try
        S.LoadFromFile(OpenDialog1.FileName);
        FJSONObject:=TJSONObject.ParseJSONValue(S.Text) as TJSONObject;
        if Assigned(FJSONObject) then
          begin
            EnumPairs;
          end
        else
          raise Exception.Create('Файл не содержит JSON-данные');
      finally
        S.Free;
      end;
    end;
end;

procedure Tfmain.Button2Click(Sender: TObject);
var JSONObject, InnerObject : TJSONObject;
    Pair : TJSONPair;
    JsonArray: TJSONArray;
    S:TStringList;
begin
  S:=TStringList.Create;
  {создали объект верхнего уровня}
try
  JSONObject:=TJSONObject.Create;
  {создали пару}
  Pair:=TJSONPair.Create('stringPair','TestString');
  {записали пару в объект}
  JSONObject.AddPair(Pair);
  {можно записать и так}
  JSONObject.AddPair(TJSONPair.Create('stringPair2','TestString2'));
  {и даже так}
  JSONObject.AddPair(TJSONPair.Create('NumberPair2',TJSONNumber.Create(128)));

  {добавляем в объект ещё один объект}
  InnerObject:=TJSONObject.Create;
  InnerObject.AddPair('Inner_StringPair','Hello World!');
  JSONObject.AddPair(TJSONPair.Create('InnerObject',InnerObject));

  {записываем в объект массив}
  //создали пустой массив
  JsonArray:=TJSONArray.Create();
  JsonArray.Add('StringElement');//первый элемент - строка
  JsonArray.Add(True); //второй элемент - True
  JsonArray.Add(100500); //третий элемент - число
  JsonArray.AddElement(InnerObject);//четвертый элемент - объект
  //записали массив в объект
  JSONObject.AddPair('Array',JsonArray);


  S.Add(JSONObject.ToString);
  S.SaveToFile('json.txt');
finally
  S.Free;
 // JsonArray.Free;
  JSONObject.Destroy;
end;

end;

procedure Tfmain.cbPairsChange(Sender: TObject);
begin
  mmPairValue.Clear;
  mmPairValue.Text:=FJSONObject.Get(cbPairs.ItemIndex).JsonValue.ToString;
  lbClass.Caption:=GetBestClassName(FJSONObject.Get(cbPairs.ItemIndex).JsonValue);
end;

procedure Tfmain.EnumArray;
var JsonArray: TJSONArray;
    i: integer;
begin
  {получаем масиив из значения пары с названием "items"}
  JsonArray:=FJSONObject.Get('items').JsonValue as TJSONArray;
  for I := 0 to JsonArray.Size-1 do
     ShowMessage((JsonArray.Get(i) as TJSONObject).Get('title').JsonValue.Value)
end;

procedure Tfmain.EnumArray2;
var JsonArray: TJSONArray;
    Enum: TJSONArrayEnumerator;
    Task : TTaskResource;
begin
  {получаем масиив из значения пары с названием "items"}
  JsonArray:=FJSONObject.Get('items').JsonValue as TJSONArray;
  {создаем перечислитель}
  Enum:=TJSONArrayEnumerator.Create(JsonArray);
  {перебираем все элементы массива}
  while Enum.MoveNext do
    begin
      Task:=TTaskResource.Create;
      {передали значение в метод для парсинга}
      Task.ParseJSON(Enum.GetCurrent);
      Task.Free;
    end;
end;

procedure Tfmain.EnumPairs;
var i:integer;
begin
cbPairs.Clear;
  for I := 0 to FJSONObject.Size-1 do
    cbPairs.Items.Add(FJSONObject.Get(i).JsonString.Value);
end;

procedure Tfmain.FormDestroy(Sender: TObject);
begin
 if Assigned(FJSONObject) then
   FJSONObject.Destroy;
end;

function Tfmain.GetBestClassName(AJSONValue: TJSONAncestor): string;
begin
  Result:=AJSONValue.ClassName;
end;

{ TTaskResource }

constructor TTaskResource.Create;
begin

end;

procedure TTaskResource.ParseJSON(Value: TJSONValue);
begin

end;

procedure TTaskResource.SetCompleted(const Value: TDateTime);
begin

end;

procedure TTaskResource.SetDue(const Value: TDateTime);
begin

end;

procedure TTaskResource.SetId(const Value: string);
begin

end;

procedure TTaskResource.SetNotes(const Value: string);
begin

end;

procedure TTaskResource.SetStatus(const Value: string);
begin

end;

procedure TTaskResource.SetTitle(const Value: string);
begin

end;

initialization
  ReportMemoryLeaksOnShutdown:=true;

end.
