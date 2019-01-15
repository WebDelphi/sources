unit GoogleImageList;

interface

uses Controls, Classes, ShellAPI, ImgList, Graphics, SysUtils, Dialogs;

type
  TFileIcon = record
    Ext: string;
    IconIndex: Integer;
  end;

  TFileIconList = class
  private
    FFileIcons: array of TFileIcon;
    FCount: Integer;
    function Get(Index: Integer): TFileIcon;
    function GetCount: Integer;
  public
    function IconIndexOf(const Ext: string): Integer;
    function ExtOf(const IconIndex: Integer): string;
    function Add(const Ext: string; IconIndex: Integer): Integer;
    procedure Clear;
    property Item[Index: Integer]: TFileIcon read Get; default;
    property Count: Integer read GetCount;
  end;

  TGoogleImageList = class(TImageList)
  private
    FIcon: TIcon;
    fList: TFileIconList;
    fExistedFiles: Boolean;
    FDefaultImageIndex: TImageIndex;
    function Get(FileExt: string): Integer;
    procedure SetDefaultImageIndex(const Value: TImageIndex);
  public
    property List: TFileIconList read fList;
    function CountFileIcon: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property ExtIcon[FileExt: string]: Integer read Get; default;
  published
    property ColorDepth default cd32Bit;
    property ExistedFiles: Boolean read fExistedFiles write fExistedFiles
      default false;
    property DefaultImageIndex : TImageIndex read FDefaultImageIndex write SetDefaultImageIndex;
  end;

procedure Register;
function ExtractHandleIcon(FileName: string; ExistedFiles: Boolean): Cardinal;

implementation

procedure Register;
begin
  RegisterComponents('WebDelphi.ru', [TGoogleImageList]);
end;

function ExtractHandleIcon(FileName: string; ExistedFiles: Boolean): Cardinal;
var
  FileInfo: SHFILEINFO;
  Flags: Cardinal;
begin
  Flags := SHGFI_ICON or SHGFI_SMALLICON;
  if not(ExistedFiles) then
    Flags := Flags or SHGFI_USEFILEATTRIBUTES;
  if SHGetFileInfo(PWideChar(FileName), 0, FileInfo, SizeOf(FileInfo), Flags)>0 then
    Result := FileInfo.hIcon
  else
    Result:=0;
end;

{ TFileIconImageList }

procedure TGoogleImageList.Clear;
begin
  inherited Clear;
  List.Clear;
end;

function TGoogleImageList.CountFileIcon: Integer;
begin
  Result := List.Count;
end;

constructor TGoogleImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ColorDepth := cd32Bit;
  fList := TFileIconList.Create;
  FIcon := TIcon.Create;
  fExistedFiles := False;
end;

destructor TGoogleImageList.Destroy;
begin
  FIcon.Free;
  List.Free;
  inherited Destroy;
end;

function TGoogleImageList.Get(FileExt: string): Integer;
begin
  Result := List.IconIndexOf(FileExt);
  if Result = -1 then
  begin
    FIcon.Handle := ExtractHandleIcon(FileExt, ExistedFiles);
    if FIcon.Handle<=0 then
      Result:=DefaultImageIndex
    else
      Result := List[(List.Add(FileExt, AddIcon(FIcon)))].IconIndex
  end;
end;

procedure TGoogleImageList.SetDefaultImageIndex(const Value: TImageIndex);
begin
  if FDefaultImageIndex <> Value then
  begin
    FDefaultImageIndex := Value;
  end;
end;

{ TFileIconList }

function TFileIconList.Add(const Ext: string; IconIndex: Integer): Integer;
begin
  Result := Count;
  SetLength(fFileIcons, Count + 1);
  fFileIcons[Count].Ext := Ext;
  fFileIcons[Count].IconIndex := IconIndex;
  inc(fCount);
end;

procedure TFileIconList.Clear;
begin
  SetLength(fFileIcons, 0);
  fCount := 0;
end;

function TFileIconList.ExtOf(const IconIndex: Integer): string;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Get(i).IconIndex = IconIndex then
    begin
      Result := Get(i).Ext;
      Exit;
    end;
  Result := '';
end;

function TFileIconList.Get(Index: Integer): TFileIcon;
begin
  Result := fFileIcons[Index];
end;

function TFileIconList.GetCount: Integer;
begin
  Result := fCount;
end;

function TFileIconList.IconIndexOf(const Ext: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Get(i).Ext = Ext then
    begin
      Result := Get(i).IconIndex;
      Exit;
    end;
  Result := -1;
end;

end.
