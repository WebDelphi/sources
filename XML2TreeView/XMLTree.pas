unit XMLTree;

interface

uses xmldoc, XMLIntf, ComCtrls, Classes, SysUtils, Dialogs, LAseConst;

type
  TNodeRecord = record
    XMLNode: IXMLNode;
    TrNode: TTreeNode;
  end;

type
  PTreeView = ^TTreeView;
  PNodeRecord = ^TNodeRecord;

type
  TNodeList = class(TList)
  private
    procedure SetRecord(index: Integer; Ptr: PNodeRecord);
    function GetRecord(index: Integer): PNodeRecord;
  public
    constructor Create;
    procedure Clear;
    destructor Destroy; override;
    property NodeItem[i: Integer]: PNodeRecord read GetRecord write SetRecord;
  end;

type
  TXMLTree = class
  private
    FTreeView: PTreeView; // указатель на дерево
    FXMLDoc: IXMLDocument; // XML-документ
    FFileName: string;
    FNodeList: TNodeList;
    procedure LoadTopElements;
    procedure Recurse;
  public
    function XMLNodeFromTreeText(const cText: string): IXMLNode;
    procedure SaveTreeToXML;
    property NodeList: TNodeList read FNodeList;
    constructor Create(const aFileName: string; aTree: PTreeView);
  end;

implementation

{ TXMLTree }

constructor TXMLTree.Create(const aFileName: string; aTree: PTreeView);
begin
  if aTree = nil then
    Exit;
  inherited Create;
  FNodeList := TNodeList.Create;
  FFileName := aFileName;
  FTreeView := aTree;
  if Length(Trim(FFileName)) > 0 then
  begin
    FXMLDoc := NewXMLDocument();
    FXMLDoc.LoadFromFile(FFileName);
    FXMLDoc.Active := true;
    Recurse;
    FTreeView.SortType := stText;
  end;
end;

procedure TXMLTree.LoadTopElements;
var
  i: Integer;
  Root: IXMLNode;
begin
  Root := FXMLDoc.DocumentElement;
  for i := 0 to Root.ChildNodes.Count - 1 do
  begin
    ShowMessage(Root.ChildNodes[i].NodeName);
    FTreeView.Items.AddObject(FTreeView.TopItem, Root.ChildNodes[i].NodeName,
      pointer(i));
    FTreeView.TopItem.HasChildren := Root.ChildNodes[i].HasChildNodes;
  end;
end;

procedure TXMLTree.Recurse;
var
  iNode: IXMLNode;

  procedure ProcessNode(Node: IXMLNode; TreeNode: TTreeNode);
  var
    cNode: IXMLNode;
    s: string;
    NodeRec: PNodeRecord;
  begin
    if Node = nil then
      Exit;
    if (Node.NodeName = 'Button') or (Node.NodeName = 'CheckedButton') then
      s := Node.Attributes['BtnName']
    else
      s := Node.NodeName;
    if s <> '#comment' then
    begin
      TreeNode := FTreeView.Items.AddChild(TreeNode, SysBtnNameToHuman(s));
      New(NodeRec);
      with NodeRec^ do
      begin
        XMLNode := Node;
        TrNode := TreeNode;
      end;
      FNodeList.Add(NodeRec);
    end;
    cNode := Node.ChildNodes.First;
    while cNode <> nil do
    begin
      ProcessNode(cNode, TreeNode);
      cNode := cNode.NextSibling;
    end;
  end;

begin
  iNode := FXMLDoc.DocumentElement.ChildNodes.First;
  // стартуем с первого элемента
  while iNode <> nil do
  begin
    ProcessNode(iNode, nil); // Рекурсия
    iNode := iNode.NextSibling;
  end;
end;

procedure TXMLTree.SaveTreeToXML;
var
  tn: TTreeNode;
  xmldoc: TXMLDocument;
  iNode: IXMLNode;

  procedure ProcessTreeItem(tn: TTreeNode; iNode: IXMLNode);
  var
    cNode: IXMLNode;
    sNode: IXMLNode;
  begin
    if (tn = nil) then
      Exit;
    sNode := XMLNodeFromTreeText(tn.Text);
    cNode := iNode.AddChild(sNode.NodeName); // имя узла
    { атрибуы узла }
    cNode.Attributes['Text'] := tn.Text;
    cNode.Attributes['Level'] := IntToStr(tn.Level);

    // дочерние узлы
    tn := tn.getFirstChild;
    while tn <> nil do
    begin
      ProcessTreeItem(tn, cNode);
      tn := tn.getNextSibling;
    end;
  end;

begin
  xmldoc := TXMLDocument.Create(nil);
  xmldoc.Active := true;
  iNode := xmldoc.AddChild('Skin'); // создаем корневой элемент
  // атрибуты корневого узла
  iNode.Attributes['name'] := 'MySkin';
  iNode.Attributes['author'] := 'vlad';

  tn := FTreeView^.TopItem;
  while tn <> nil do
  begin
    ProcessTreeItem(tn, iNode);
    tn := tn.getNextSibling;
  end;
  // сохраняем файл
  xmldoc.SaveToFile(ChangeFileExt(ParamStr(0), '.XML'));
  // уничтожаем объект
  xmldoc := nil
end;

function TXMLTree.XMLNodeFromTreeText(const cText: string): IXMLNode;
var
  LCount: Integer;
  LList: PPointerList;
  i: Integer;
begin
  LCount := FNodeList.Count;
  LList := Pointer(FNodeList.List);
  for i := 0 to LCount - 1 do
  begin
    if PNodeRecord(LList)^.TrNode.Text = cText then
    begin
      Result := PNodeRecord(LList)^.XMLNode;
      break;
    end;
  end;
end;

{ TNodeList }

procedure TNodeList.Clear;
var
  i: Integer;
  p: PNodeRecord;
begin
  for i := 0 to Pred(Count) do
  begin
    p := NodeItem[i];
    if p <> nil then
      Dispose(p);
  end;
  inherited Clear;
end;

constructor TNodeList.Create;
begin
  inherited Create;
end;

destructor TNodeList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TNodeList.GetRecord(index: Integer): PNodeRecord;
begin
  Result := PNodeRecord(Items[index]);

end;

procedure TNodeList.SetRecord(index: Integer; Ptr: PNodeRecord);
var
  p: PNodeRecord;
begin
  p := NodeItem[index];
  if p <> Ptr then
  begin
    if p <> nil then
      Dispose(p);
    Items[index] := Ptr;
  end;

end;

end.
