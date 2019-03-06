unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, httpsend, StdCtrls, NativeXML, ExtCtrls;

type
  TForm2 = class(TForm)
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Panel2: TPanel;
    GroupBox2: TGroupBox;
    Memo2: TMemo;
    GroupBox3: TGroupBox;
    Memo3: TMemo;
    GroupBox4: TGroupBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure XmlNodeEvent(Sender: TObject; Node: TXmlNode);
    procedure XmlMethodDesc(Sender: TObject; Node: TXmlNode);
    procedure XmlMethodSignature(Sender: TObject; Node: TXmlNode);
    procedure XmlCapabilityParse(Sender: TObject; Node: TXmlNode);
    procedure XmlProgressEvent(Sender: TObject; Size: integer);
    procedure GetListMethods(const URL:string);
    procedure GetMethodHelp(const URL,Method:string);
    procedure GetMethodSignature(const URL, Method: string);
    procedure GetCapibilites(URL: string);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}


procedure TForm2.Button1Click(Sender: TObject);
begin
 Memo1.Lines.Clear;
 Memo2.Lines.Clear;
 Memo3.Lines.Clear;
 ListBox1.Items.Clear;
 GetCapibilites(Edit1.Text);
 if Memo1.Lines.Text<>'XML-RPC Server not enabled.' then
   GetListMethods(Edit1.Text);
end;

procedure TForm2.GetCapibilites(URL: string);
var
  HTTPSender: THTTPSend;
  XMLDoc: TNativeXML;
begin
  HTTPSender := THTTPSend.Create;
  XMLDoc := TNativeXML.CreateName('methodCall');
  try
    XMLDoc.Root.NodeNew('methodName').ValueAsString := 'system.getCapabilities';
    XMLDoc.SaveToStream(HTTPSender.Document);
    if HTTPSender.HTTPMethod('POST', URL) then
      begin
        Memo1.Lines.Clear;
        HTTPSender.Document.Position := 0;
        XMLDoc.OnNodeLoaded := XmlCapabilityParse;
        XMLDoc.LoadFromStream(HTTPSender.Document);
      end;
  finally
    HTTPSender.Free;
    XMLDoc.Free;
  end;
end;

procedure TForm2.GetListMethods(const URL: string);
var
  HTTPSender: THTTPSend;
  XMLDoc: TNativeXML;
begin
  HTTPSender := THTTPSend.Create;
  XMLDoc := TNativeXML.CreateName('methodCall');
  try
    XMLDoc.Root.NodeNew('methodName').ValueAsString := 'system.listMethods';
    XMLDoc.SaveToStream(HTTPSender.Document);
    if HTTPSender.HTTPMethod('POST', URL) then
      begin
        ListBox1.Items.Clear;
        HTTPSender.Document.Position := 0;
        XMLDoc.OnNodeLoaded := XmlNodeEvent;
        XMLDoc.LoadFromStream(HTTPSender.Document);
        if XMLDoc.Root.Name<>'methodResponse' then
          Memo1.Lines.Add('XML-RPC Server not enabled.');

      end;
  finally
    HTTPSender.Free;
    XMLDoc.Free;
  end;
end;

procedure TForm2.GetMethodHelp(const URL, Method: string);
var
  HTTPSender: THTTPSend;
  XMLDoc: TNativeXML;
begin
  HTTPSender := THTTPSend.Create;
  XMLDoc := TNativeXML.CreateName('methodCall');
  try
    XMLDoc.Root.NodeNew('methodName').ValueAsString := 'system.methodHelp';
    XMLDoc.Root.NodeNew('params').NodeNew('param').NodeNew('value').NodeNew('string').ValueAsString:=Method;
    XMLDoc.SaveToStream(HTTPSender.Document);
    if HTTPSender.HTTPMethod('POST', URL) then
      begin
        Memo2.Lines.Clear;
        HTTPSender.Document.Position := 0;
        XMLDoc.OnNodeLoaded := XmlMethodDesc;
        XMLDoc.LoadFromStream(HTTPSender.Document);
      end;
  finally
    HTTPSender.Free;
    XMLDoc.Free;
  end;
end;

procedure TForm2.GetMethodSignature(const URL, Method: string);
var
  HTTPSender: THTTPSend;
  XMLDoc: TNativeXML;
begin
  HTTPSender := THTTPSend.Create;
  XMLDoc := TNativeXML.CreateName('methodCall');
  try
    XMLDoc.Root.NodeNew('methodName').ValueAsString := 'system.methodSignature';
    XMLDoc.Root.NodeNew('params').NodeNew('param').NodeNew('value').NodeNew('string').ValueAsString:=Method;
    XMLDoc.SaveToStream(HTTPSender.Document);
    if HTTPSender.HTTPMethod('POST', URL) then
      begin
        Memo3.Lines.Clear;
        HTTPSender.Document.Position := 0;
        XMLDoc.OnNodeLoaded := XmlMethodSignature;
        XMLDoc.LoadFromStream(HTTPSender.Document);
      end;
  finally
    HTTPSender.Free;
    XMLDoc.Free;
  end;
end;

procedure TForm2.ListBox1Click(Sender: TObject);
begin
GetMethodHelp(Edit1.Text, ListBox1.Items[ListBox1.ItemIndex]);
GetMethodSignature(Edit1.Text, ListBox1.Items[ListBox1.ItemIndex]);
end;

procedure TForm2.XmlCapabilityParse(Sender: TObject; Node: TXmlNode);
var List: TXmlNodeList;
begin
  if (Node.Name='member')and(Node.TotalNodeCount=11) then
    begin
      List:=TXmlNodeList.Create;
      try
      Memo1.Lines.Add('Характеристика '+Node.NodeByName('name').ValueAsString);
      Node.NodeByName('value').NodeByName('struct').NodesByName('member',List);
      Memo1.Lines.Add('  URL '+List[0].NodeByName('value').NodeByName('string').ValueAsString);
      Memo1.Lines.Add('  Версия '+List[1].NodeByName('value').NodeByName('int').ValueAsString);
      finally
        List.Free;
      end;
    end;
end;

procedure TForm2.XmlMethodDesc(Sender: TObject; Node: TXmlNode);
begin
 if Node.Name = 'string' then
   Memo2.Lines.Add(Node.ValueAsString)
end;

procedure TForm2.XmlMethodSignature(Sender: TObject; Node: TXmlNode);
begin
 if Node.Name = 'string' then
   begin
     if Memo3.Lines.Count=0 then
       Memo3.Lines.Add('[out] '+Node.ValueAsString)
     else
       Memo3.Lines.Add('[in] '+Node.ValueAsString)
   end;
end;

procedure TForm2.XmlNodeEvent(Sender: TObject; Node: TXmlNode);
begin
  if Node.Name = 'string' then
    ListBox1.Items.Add(Node.ValueAsString)
end;

procedure TForm2.XmlProgressEvent(Sender: TObject; Size: integer);
begin

end;

end.
