unit MenuItemHelper;

interface

uses System.SysUtils, Classes, FMX.Types, FMX.Menus, FMX.Platform;

type
  TMenuItemHelper = class helper for TMenuItem
    function Content: TContent;
    procedure UpdateItem;
    procedure DeleteChildrenItems;
    procedure ChildNames(ANames: TStrings);
    procedure DeleteChildByName(const AChildName: string;
      DeleteAll: boolean = True);
    procedure DeleteChildByIndex(AChildIndex: integer);
    function AddChildrenItem(AChild: TMenuItem):integer; overload;
    function AddChildrenItem(const AChildName: string):integer; overload;
    function IndexOf(const AChildName: string): integer;
    function GetChildItem(AIndex:Integer):TMenuItem;
    function ChidItemsCount: integer;
  end;

implementation

uses System.RTLConsts;

{ TMenuItemHelper }

function TMenuItemHelper.AddChildrenItem(AChild: TMenuItem):integer;
begin
  Result:=-1;
  if not Assigned(AChild) then
    Exit;
  AChild.PopupMenu:=Self.PopupMenu;
  AChild.Parent := Self;
  AChild.Locked := True;
  AddObject(AChild);
  UpdateItem;
  Result:=AChild.Index;
end;

function TMenuItemHelper.AddChildrenItem(const AChildName: string):integer;
var
  NewItem: TMenuItem;
begin
  NewItem := TMenuItem.Create(Self.Owner);
  NewItem.Text := AChildName;
  Result:=AddChildrenItem(NewItem)
end;

function TMenuItemHelper.ChidItemsCount: integer;
begin
  Result:=GetItemsCount;
end;

procedure TMenuItemHelper.ChildNames(ANames: TStrings);
var
  I: integer;
begin
  if not Assigned(ANames) then
    Exit;
  ANames.Clear;
  for I := 0 to GetItemsCount - 1 do
    ANames.Add((GetItem(I) as TMenuItem).Text);
end;

function TMenuItemHelper.Content: TContent;
var i:integer;
begin
  for I := 0 to ChildrenCount-1 do
    if Children[i].ClassNameIs('TContent') then
      begin
        Result:=Children[i] as TContent;
        Exit;
      end;
  Result:=nil;
end;

procedure TMenuItemHelper.DeleteChildByIndex(AChildIndex: integer);
begin
  if AChildIndex > GetItemsCount-1 then
    raise Exception.CreateFmt(SListIndexError, [AChildIndex]);
  GetItem(AChildIndex).Free;
  UpdateItem;
end;

procedure TMenuItemHelper.DeleteChildByName(const AChildName: string;
  DeleteAll: boolean);
var
  I: integer;
  Item: TMenuItem;
  b: boolean;
begin
  BeginUpdate;
  b := False;
  for I := GetItemsCount - 1 downto 0 do
  begin
    Item := GetItem(I) as TMenuItem;
    if AnsiSameText(Item.Text, AChildName) then
    begin
      Item.Free;
      b := True;
      if not DeleteAll then
        break;
    end;
  end;
  if b then
    UpdateItem;
  EndUpdate;
end;

procedure TMenuItemHelper.DeleteChildrenItems;
var
  I: integer;
begin
  BeginUpdate;
  for I := GetItemsCount - 1 downto 0 do
    GetItem(I).Free;
  UpdateItem;
  EndUpdate;
end;

function TMenuItemHelper.GetChildItem(AIndex: Integer): TMenuItem;
begin
  if AIndex > GetItemsCount-1 then
    raise Exception.CreateFmt(SListIndexError, [AIndex]);
  Result:=GetItem(AIndex) as TMenuItem;
end;

function TMenuItemHelper.IndexOf(const AChildName: string): integer;
var
  I: integer;
  Item: TMenuItem;
begin
  Result := -1;
  for I := 0 to GetItemsCount - 1 do
  begin
    Item := GetItem(I) as TMenuItem;
    if AnsiSameText(Item.Text, AChildName) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TMenuItemHelper.UpdateItem;
begin
  if IsHandleValid(Handle) then
    Platform.UpdateMenuItem(Self);
end;

end.
