unit GoogleTree;

interface

uses SysUtils, Windows, Controls, Classes, ComCtrls, GoogleDocument;

resourcestring
  rsRootFolder = 'Мои документы';

const
  cRootFolder = 'rootfolder';

type
  TTreeMode = (mFiles, mFolders, mBoth);

  TGoogleVirtualTree = class(TCustomTreeView)
  private
    FRootName: string;
    FDocuments: TGoogleDocList;
    FAssigned: boolean;
    FMode: TTreeMode;
    procedure SetRootName(Value: string);
    procedure SetMode(Value: TTreeMode);
    procedure GenerateTree(Path: string);
    function FindFolder(const aParentID: string; var aDocument: TGoogleDoc;
      Offset: Integer = 0): Integer;
    procedure Setup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AssignDocuments(ADocuments: TGoogleDocList);
    property Items;
  published
    property Align;
    property Anchors;
    property AutoExpand;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DoubleBuffered;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    property MultiSelect;
    property MultiSelectStyle;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default true;
    property ToolTips;
    property Touch;
    property Visible;
    property OnAddition;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnCancelEdit;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGesture;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Mode: TTreeMode read FMode write SetMode;
    property RootName: string read FRootName write SetRootName;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BuBa Group', [TGoogleVirtualTree]);
end;

{ TFolderTree }

procedure TGoogleVirtualTree.AssignDocuments(ADocuments: TGoogleDocList);
begin
  FDocuments.Clear;
  if Assigned(ADocuments) then
  begin
    FDocuments.AddRange(ADocuments);
    FAssigned := FDocuments.Count > 0;
    Setup;
  end;
end;

procedure TGoogleVirtualTree.Setup;
begin
  if FAssigned then
  begin
    Items.Clear;
    SetRootName(FRootName);
    GenerateTree('');
  end;
end;

constructor TGoogleVirtualTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocuments := TGoogleDocList.Create;
  SetRootName(FRootName);
end;

destructor TGoogleVirtualTree.Destroy;
begin
  FDocuments.Destroy;
  inherited;
end;

function TGoogleVirtualTree.FindFolder(const aParentID: string;
  var aDocument: TGoogleDoc; Offset: Integer): Integer;
var
  i: Integer;
begin
  for i := Offset to FDocuments.Count - 1 do
    if FDocuments[i].ParentID = aParentID then
    begin
      aDocument := FDocuments[i];
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TGoogleVirtualTree.GenerateTree(Path: string);

  procedure AddElement(Parent: TTreeNode; Document: TGoogleDoc);
  var
    Add: boolean;
  begin
    case Mode of
      mFolders:
        Add := (Document.DocType = dtFolder);
      mBoth:
        Add := true;
    end;
    if Add then
      Self.Items.AddChild(Parent, Document.Title).Data := pointer(Document)
  end;

var
  found, i: Integer;
  Folder: TGoogleDoc;
begin
  if (Mode = mFolders) or (Mode = mBoth) then
  begin
    found := FindFolder(Path, Folder);
    while (found > -1) do
    begin
      if (Folder.ParentID = '') then
        AddElement(TopItem, Folder)
      else
        for i := Items[Items.Count - 1].Parent.Index to Items.Count - 1 do
          if Items[i].Data <> nil then
            if TGoogleDoc(Items[i].Data).ID = Folder.ParentID then
            begin
              AddElement(Items[i], Folder);
              break;
            end;
      if (Folder.DocType = dtFolder) and (Mode <> mFiles) then
        GenerateTree(Folder.ID);
      found := FindFolder(Path, Folder, found + 1);
    end;
  end
  else
  begin
    for i := 0 to FDocuments.Count - 1 do
      if FDocuments[i].DocType <> dtFolder then
        Items.AddChild(TopItem, FDocuments[i].Title).Data := pointer(FDocuments[i])
  end;
end;

procedure TGoogleVirtualTree.SetMode(Value: TTreeMode);
begin
  FMode := Value;
  Setup;
end;

procedure TGoogleVirtualTree.SetRootName(Value: string);
begin
  if Length(Trim(Value)) = 0 then
    FRootName := rsRootFolder
  else
    FRootName := Value;
  if Assigned(TopItem) then
    TopItem.Text := FRootName
  else
  begin
    try
      Self.Items.Add(TTreeNode.Create(Self.Items), RootName);
      with Self.TopItem do
      begin
        Data := TGoogleDoc.Create(nil);
        TGoogleDoc(Data).Title := RootName;
      end;
    except
    end;
  end;
end;

end.
