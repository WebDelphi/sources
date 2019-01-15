unit GoogleDocument;

interface

uses Classes, SysUtils, xmlintf, xmldoc, TypInfo, httpsend, ssl_openssl,
  Generics.Collections, ComCtrls, StrUtils, SynaCode, Controls;

resourcestring
  rsParseError = 'Ошибка разбора XML: %s';
  rsGETError = 'Ошибка отправки GET-запроса: %s';
  rsUnknownError = 'Неизвестная ошибка';
  rsRootFolder = 'Мои папки';

const
  cEntryTag = 'entry';
  cTitleTag = 'title';
  cResourceIDTag = 'gd:resourceId';
  cGDataVersion = '3.0';
  cRootFolder = 'rootfolder';

type
  TDocType = (dtUnknown, dtDocument, dtPdf, dtFolder, dtPresentation,
    dtSpreadsheet, dtForm, dtFile, dtDrawing);

  TTreeMode = (mFiles, mFolders, mBoth);

  TGoogleDoc = class
  private
    FTitle: string;
    FID: string;
    FParentID: string;
    FDocType: TDocType;
  public
    constructor Create(const AEntry: IXMLNode);
    destructor Destroy; override;
    procedure ParseXML(const AEntry: IXMLNode);
    property Title: string read FTitle write FTitle;
    property ID: string read FID;
    property ParentID: string read FParentID;
    property DocType: TDocType read FDocType;
  end;

  TGoogleRequest = class
  private
    FAuthKey: string;
    FHTTP: THTTPSend;
    procedure SetAuthKey(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure GET(const URL: string; var Stream: TStream);
    property AuthKey: string read FAuthKey write SetAuthKey;
  end;

  TGoogleDocList = class(TList<TGoogleDoc>)
  public
    constructor Create;
    destructor Destroy; override;
    procedure FillFromXML(XMLStream: TStream);
  end;

implementation

{ TGoogleDoc }

constructor TGoogleDoc.Create(const AEntry: IXMLNode);
begin
  inherited Create;
  ParseXML(AEntry);
end;

destructor TGoogleDoc.Destroy;
begin
  inherited Destroy;
end;

procedure TGoogleDoc.ParseXML(const AEntry: IXMLNode);

  function GetDocType(const ResourceID: string): TDocType;
  var
    ParamStr: string;
    ID: Integer;
  begin
    ParamStr := copy(ResourceID, 1, pos(':', ResourceID) - 1);
    ID := GetEnumValue(TypeInfo(TDocType), 'dt' + ParamStr);
    if ID > -1 then
      Result := TDocType(ID)
    else
      Result := dtUnknown;
  end;

var
  Node: IXMLNode;

begin
  if (AEntry = nil) or (AEntry.NodeName <> cEntryTag) then
    Exit;
  try
    Node := AEntry.ChildNodes.First;
    while Assigned(Node) do
    begin
      if Node.NodeName = cTitleTag then
        FTitle := Node.Text
      else if Node.NodeName = cResourceIDTag then
      begin
        FID := Node.Text;
        FDocType := GetDocType(FID);
      end
      else
        if Node.NodeName='link' then
          begin
            if Node.Attributes['rel']='http://schemas.google.com/docs/2007#parent' then
               begin
                 FParentID := ReverseString(Node.Attributes['href']);
                 FParentID := DecodeURL(ReverseString(copy(FParentID, 1, pos('/', FParentID) - 1)));
               end;
          end;
      Node := Node.NextSibling;
    end;
  except
    on E: EXMLDocError do
      raise E.CreateFmt(rsParseError, [E.Message])
    else
      raise Exception.Create(rsUnknownError);
  end;
end;

{ TGoogleRequest }

constructor TGoogleRequest.Create;
begin
  inherited Create;
  FHTTP := THTTPSend.Create;
end;

destructor TGoogleRequest.Destroy;
begin
  FHTTP.Free;
  inherited Destroy;
end;

procedure TGoogleRequest.GET(const URL: string; var Stream: TStream);
begin
  FHTTP.Clear;
  FHTTP.Headers.Add('GData-Version: ' + cGDataVersion);
  FHTTP.Headers.Add('Authorization: GoogleLogin auth=' + FAuthKey);
  if FHTTP.HTTPMethod('GET', URL) and (FHTTP.ResultCode = 200) then
    Stream.CopyFrom(FHTTP.Document, FHTTP.Document.Size)
  else
    raise Exception.CreateFmt(rsGETError,
      [IntToStr(FHTTP.ResultCode) + ' ' + FHTTP.ResultString]);
end;

procedure TGoogleRequest.SetAuthKey(const Value: string);
begin
  FAuthKey := Value;
end;

{ TGoogleDocList }

constructor TGoogleDocList.Create;
begin
  inherited Create;
end;

destructor TGoogleDocList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TGoogleDocList.FillFromXML(XMLStream: TStream);
var
  xmldoc: IXMLDocument;
  Node: IXMLNode;
  i: Integer;
begin
  if not Assigned(XMLStream) or (XMLStream.Size = 0) then
    Exit;
  xmldoc := TXMLDocument.Create(nil);
  try
    xmldoc.LoadFromStream(XMLStream, xetUTF_8);
    if xmldoc.IsEmptyDoc then
      Exit;
    for i := 0 to xmldoc.DocumentElement.ChildNodes.Count - 1 do
    begin
      Node := xmldoc.DocumentElement.ChildNodes.GET(i);
      if Node.NodeName = cEntryTag then
        self.Add(TGoogleDoc.Create(Node));
    end;
  finally
    xmldoc := nil
  end;
end;

end.
