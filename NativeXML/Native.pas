unit Native;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NativeXML, httpsend {httpsend - для Synapse};

type
  TForm3 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Memo1: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox1: TComboBox;
    Label6: TLabel;
    Edit2: TEdit;
    Label7: TLabel;
    Edit3: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ParseXML(const XML: TStream);
  end;

var
  Form3: TForm3;
  XMLDoc: TNativeXml; // объект XML-документа
  NodeList: TXmlNodeList; // список узлов

implementation

resourcestring
  rsEEmptyXML = 'Пустой XML! Работа прервана!';

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
begin
  with THTTPSend.Create do
  begin
    if HTTPMethod('GET', Edit1.Text) then
    begin
      Memo1.Clear;
      Memo1.Lines.LoadFromStream(Document, TEncoding.UTF8);
      ParseXML(Document);
    end;
  end;
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  XMLDoc: TNativeXml;
  Node: TXmlNode;
  WithCdata, NodeCDATA: TXmlNode;
begin
  XMLDoc := TNativeXml.Create; // создали документ
  XMLDoc.CreateName('RootNode'); // создали корневой узел
  // задаем параметры документа
  XMLDoc.WriteOnDefault := false;
  XMLDoc.CommentString := 'Комментарий к файлу';
  XMLDoc.EncodingString := 'UTF-8';
  XMLDoc.VersionString := '1.0';

  // создаем дочерний узел
  Node := TXmlNode.Create(nil);
  Node.Name := 'FirstNode';
  Node.ValueAsString := 'Значение узла';
  Node.AttributeAdd('attr_integer', 12); // добавляем атрибут integer
  Node.WriteAttributeDateTime('attr_date', Now);
  // другой способ. записываем дату
  XMLDoc.Root.NodeAdd(Node);
  { второй узел: другой вариант записи }
  XMLDoc.Root.NodeNew('SecondNode').ValueAsBool := false;

  WithCdata := TXmlNode.Create(nil);
  NodeCDATA := TXmlNode.CreateType(XMLDoc, xeCData);
  NodeCDATA.ValueAsString := 'Это строка в CDATA';
  WithCdata.Name := 'WithCdata';
  WithCdata.NodeAdd(NodeCDATA);
  XMLDoc.Root.NodeAdd(WithCdata);

  Memo1.Lines.Add(XMLDoc.WriteToString);
  XMLDoc.SaveToFile('MyXML.xml');
end;

procedure TForm3.ComboBox1Change(Sender: TObject);
var
  Category: TXmlNodeList;
  i: integer;
begin
  Edit2.Text := '';
  Edit3.Text := '';
  with NodeList.Items[ComboBox1.ItemIndex] do
  begin
    Category := TXmlNodeList.Create;
    Edit3.Text := NodeByName('pubDate').ValueAsString;
    NodesByName('category', Category);
    for i := 0 to Category.Count - 1 do
      Edit2.Text := Edit2.Text + Category.Items[i].NodeByElementType(xeCData)
        .ValueAsUnicodeString + ', ';
  end;
end;

procedure TForm3.ParseXML(const XML: TStream);
var
  i: integer;
begin
  XMLDoc := TNativeXml.Create; // создаем экземпляр класса
  XMLDoc.LoadFromStream(XML); // загружаем данные из потока
  if XMLDoc.IsEmpty then
    raise Exception.Create(rsEEmptyXML);
  NodeList := TXmlNodeList.Create;
  XMLDoc.Root.FindNodes('item', NodeList); // получаем список узлов Item
  Label4.Caption := IntToStr(NodeList.Count);
  { парсим каждый узел Item }
  ComboBox1.Items.Clear;
  for i := 0 to NodeList.Count - 1 do
    ComboBox1.Items.Add(NodeList.Items[i].NodeByName('title')
      .ValueAsUnicodeString);
end;

end.
