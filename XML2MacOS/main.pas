unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.Edit, FMX.TreeView, FMX.Grid,
  Macapi.Foundation, Macapi.AppKit, System.IOUtils;

type
  TMainForm = class(TForm)
    Layout1: TLayout;
    Image1: TImage;
    Label1: TLabel;
    xmlPath: TEdit;
    btnOpen: TButton;
    OpenXMLDialog: TOpenDialog;
    GroupBox1: TGroupBox;
    Splitter1: TSplitter;
    GroupBox2: TGroupBox;
    XMLTreeView: TTreeView;
    Expander1: TExpander;
    Expander2: TExpander;
    CommonPropsGrid: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    AttributesGrid: TStringGrid;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    Layout2: TLayout;
    Label2: TLabel;
    NodeStrValue: TEdit;
    procedure btnOpenClick(Sender: TObject);
    procedure XMLTreeViewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    XmlDoc: NSXMLDocument;
    function LoadXmlFromFile(const Filename: string):NSXMLDocument;
    procedure GenerateTree;
    procedure ReadNodePropertys(XPath:string);
    procedure ReadAttributes(XPath:string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

{ TForm9 }

procedure TMainForm.btnOpenClick(Sender: TObject);
begin
  if OpenXMLDialog.Execute then
    begin
      xmlPath.Text:=OpenXMLDialog.FileName;
      XmlDoc:=LoadXmlFromFile(OpenXMLDialog.FileName);
      GenerateTree;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CommonPropsGrid.RowCount:=6;
  CommonPropsGrid.Cells[0,0]:='XPath';
  CommonPropsGrid.Cells[0,1]:='Name';
  CommonPropsGrid.Cells[0,2]:='LocalName';
  CommonPropsGrid.Cells[0,3]:='Index';
  CommonPropsGrid.Cells[0,4]:='Child Count';
  CommonPropsGrid.Cells[0,5]:='Level';
end;

procedure TMainForm.GenerateTree;

function AddNode(RTreeNode: TTreeViewItem; Element: NSXMLElement):TTreeViewItem;
begin
 Result:=TTreeViewItem.Create(RTreeNode);
 Result.Parent:=RTreeNode;
 Result.Text:=Element.name.UTF8String;
 Result.TagString:=Element.XPath.UTF8String;
 RTreeNode.AddObject(Result);
end;

procedure ProcessNode(Node: NSXMLElement; TreeNode: TTreeViewItem);
var childArr: NSArray;
    I: Integer;
    ChildNode: NSXMLElement;
    childTreeNode: TTreeViewItem;
//    count: Integer;
begin
  if Node.childCount=0 then Exit;
  childArr:=Node.children;
  for I := 0 to childArr.count-1 do
    begin
      ChildNode:=TNSXMLElement.Wrap(childArr.objectAtIndex(i));
      if (ChildNode.kind=NSXMLDocumentKind)or(ChildNode.kind=NSXMLElementKind) then
        begin
          childTreeNode:=AddNode(TreeNode,ChildNode);
//          count:=ChildNode.childCount;
          if ChildNode.childCount>0 then
            ProcessNode(ChildNode,childTreeNode);
        end;
    end;
end;


var RootNode: NSXMLElement;
    TreeNode: TTreeViewItem;
    Arr: NSArray;
    I: Integer;
begin
  XMLTreeView.Clear;
  RootNode:=XmlDoc.rootElement;
  TreeNode:=TTreeViewItem.Create(XMLTreeView);
  TreeNode.Parent:=XMLTreeView;
  TreeNode.Text:=RootNode.name.UTF8String;
  TreeNode.TagString:=RootNode.XPath.UTF8String;
  XMLTreeView.AddObject(TreeNode);
  Arr:=RootNode.children;
  for I := 0 to Arr.count-1 do
    begin
      RootNode:=TNSXMLElement.Wrap(Arr.objectAtIndex(i));
      ProcessNode(RootNode,AddNode(TreeNode,RootNode));
    end;
end;

function TMainForm.LoadXmlFromFile(const Filename: string): NSXMLDocument;
var
  Data: NSData;
begin
  Data := TNSData.Wrap(TNSData.OCClass.dataWithContentsOfFile(NSSTR(FileName)));
  Result := TNSXMLDocument.Create;
  Result.initWithData(Data, NSXMLDocumentTidyXML, nil);
end;

procedure TMainForm.ReadAttributes(XPath: string);
var Arr: NSArray;
    Element:NSXMLElement;
    Attributes: NSArray;
    I: Integer;
    AttrNode: NSXMLNode;
begin
  AttributesGrid.RowCount:=0;
  Arr:=XmlDoc.nodesForXPath(NSSTR(XPath),nil);
  if Assigned(Arr) then
    begin
      Element:=TNSXMLElement.Wrap(Arr.objectAtIndex(0));
      Attributes:=Element.attributes;
      if Assigned(Attributes) then
        begin
          AttributesGrid.RowCount:=Attributes.count;
          for I := 0 to AttributesGrid.RowCount-1 do
            begin
              AttrNode:=TNSXMLNode.Wrap(Attributes.objectAtIndex(i));
              AttributesGrid.Cells[0,i]:=AttrNode.name.UTF8String;
              AttributesGrid.Cells[1,i]:=AttrNode.stringValue.UTF8String;
            end;
        end;
    end;
end;

procedure TMainForm.ReadNodePropertys(XPath: string);
var Arr: NSArray;
    Element:NSXMLElement;
begin
  Arr:=XmlDoc.nodesForXPath(NSSTR(XPath),nil);
  if Assigned(Arr) then
    begin
      Element:=TNSXMLElement.Wrap(Arr.objectAtIndex(0));
      NodeStrValue.Text:=Element.stringValue.UTF8String;
      CommonPropsGrid.Cells[1,0]:=Element.XPath.UTF8String;
      CommonPropsGrid.Cells[1,1]:=Element.name.UTF8String;
      CommonPropsGrid.Cells[1,2]:=Element.localName.UTF8String;
      CommonPropsGrid.Cells[1,3]:=IntToStr(Element.index);
      CommonPropsGrid.Cells[1,4]:=IntToStr(Element.childCount);
      CommonPropsGrid.Cells[1,5]:=IntToStr(Element.level);
    end;
end;

procedure TMainForm.XMLTreeViewClick(Sender: TObject);
begin
  ReadNodePropertys(XMLTreeView.Selected.TagString);
  ReadAttributes(XMLTreeView.Selected.TagString);
end;

initialization
  ReportMemoryLeaksOnShutdown:=True;

end.
