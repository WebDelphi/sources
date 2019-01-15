unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, NativeXML, httpsend;

type
  TSitemapElement = record
    loc: string; // URL страницы
    lastmod: string; // дата последнего изменения
    changefreq: string; // частота обновления
    priority: currency; // приоритет внутри сайта
  end;

  TOnGetElement = procedure (Element:TSitemapElement) of object;

  TSitemapLoader = class(TThread)
  private
    FHeaderXML: TNativeXML; // индексный файл XML
    FSitemapURL: string; // URL базового сайтмапа
    FURLList: TStrings; // список всех url где есть сайтмапы
    FChankedXML: array of TNativeXML; // массив всех сайтмапов
    FOnGetElement: TOnGetElement;
    FOnLoadDone : TNotifyEvent;
    function TryLoadHeadSitemap: boolean; // пробуем закачать головной сайтмап
    function IsChanked: boolean;
    procedure LoadFile(URL: string);
    procedure ParseXML(XML: TNativeXML);
  public
    constructor Create(Syspended: boolean; AAddress: string);
    property OnGetElement : TOnGetElement read FOnGetElement write FOnGetElement;
    property OnLoadDone   : TNotifyEvent read FOnLoadDone write FOnLoadDone;
  protected
    procedure Execute; override;
  end;

type
  TFMain = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    GroupBox1: TGroupBox;
    ListView1: TListView;
    procedure Button1Click(Sender: TObject);
  private
    procedure GetElement(Element:TSitemapElement);
    procedure LoadDone(Sender:TObject);
  public
    { Public declarations }
  end;

var
  FMain: TFMain;

implementation

{$R *.dfm}

{ TSitemapLoader }

constructor TSitemapLoader.Create(Syspended: boolean; AAddress: string);
begin
  inherited Create(Syspended);
  FHeaderXML := TNativeXML.Create;
  FSitemapURL := AAddress;
  FURLList := TStringList.Create;
end;

procedure TSitemapLoader.Execute;
var i:integer;
begin
try
  if TryLoadHeadSitemap then
    begin
      if IsChanked then
        begin
          for i:=0 to FURLList.Count-1 do
            LoadFile(FURLList[i]);
          for i:=0 to Length(FChankedXML)-1 do
            ParseXML(FChankedXML[i]);
        end
      else
         ParseXML(FHeaderXML);
    end
finally
  FURLList.Free;
  for i:=Length(FChankedXML)-1 downto 0 do
     FChankedXML[i].Free;
  FChankedXML:=nil;
  if Assigned(FOnLoadDone) then
    FOnLoadDone(self)
end;
end;

function TSitemapLoader.IsChanked: boolean;
var
  NodeList: TXmlNodeList;
  i: integer;
begin
  if FHeaderXML.IsEmpty then Exit;
  Result := FHeaderXML.Root.NodeByName('sitemap') <> nil;
  FURLList.Clear;
  if Result then
  begin
    NodeList := TXmlNodeList.Create;
    FHeaderXML.Root.NodesByName('sitemap', NodeList);
    for i := 0 to NodeList.Count - 1 do
      FURLList.Add(NodeList[i].NodeByName('loc').ValueAsString);
  end
end;

procedure TSitemapLoader.LoadFile(URL: string);
var
  XML: TNativeXML;
begin
    with THTTPSend.Create do
    begin
      if HTTPMethod('GET', URL) then
      begin
        XML := TNativeXML.Create;
        try
          XML.LoadFromStream(Document);
          SetLength(FChankedXML, length(FChankedXML) + 1);
          FChankedXML[length(FChankedXML) - 1]:=TNativeXML.Create;
           FChankedXML[length(FChankedXML) - 1].Assign(XML);
        finally
          XML.Free
        end;
      end
    end;
end;

procedure TSitemapLoader.ParseXML(XML: TNativeXML);
var
  NL: TXmlNodeList;
  i: integer;
  Element: TSitemapElement;
begin
  if (Assigned(XML)) then
    if not XML.IsEmpty then
    begin
      NL := TXmlNodeList.Create;
      XML.Root.NodesByName('url', NL);
      OutputDebugString(PChar('Найдено URL = '+IntToStr(NL.Count)));
      try
        for i := 0 to NL.Count - 1 do
        begin
          Element.loc := LowerCase(NL[i].NodeByName('loc').ValueAsString);
          if NL[i].NodeByName('lastmod') <> nil then
            Element.lastmod := NL[i].NodeByName('lastmod').ValueAsString;
          if NL[i].NodeByName('changefreq') <> nil then
            Element.changefreq := NL[i].NodeByName('changefreq').ValueAsString;
          if NL[i].NodeByName('priority') <> nil then
            Element.priority := NL[i].NodeByName('priority').ValueAsFloat;
          if Assigned(FOnGetElement) then
            FOnGetElement(Element);
        end;
      finally
        NL.Free;
      end;
    end;
end;

function TSitemapLoader.TryLoadHeadSitemap: boolean;
begin
    with THTTPSend.Create do
    begin
      if HTTPMethod('GET', FSitemapURL) then
      begin
        try
          FHeaderXML.LoadFromStream(Document);
          Result := true;
        except
          Result := false;
        end;
      end
      else
        Result := false;
    end;
end;

{ TFMain }

procedure TFMain.Button1Click(Sender: TObject);
var Loader: TSitemapLoader;
begin
  ListView1.Items.Clear;
  Loader:= TSitemapLoader.Create(true,Edit1.Text);
  Loader.OnGetElement:=GetElement;
  Loader.OnLoadDone:=LoadDone;
  Loader.Start;
end;

procedure TFMain.GetElement(Element: TSitemapElement);
begin
ListView1.Items.BeginUpdate;
try
  with ListView1.Items.Add do
    begin
      Caption:=Element.loc;
      SubItems.Add(Element.lastmod);
      SubItems.Add(CurrToStr(Element.priority));
      SubItems.Add(Element.changefreq)
    end;
finally
  ListView1.Items.EndUpdate;
end;
end;

procedure TFMain.LoadDone(Sender: TObject);
begin
  ShowMessage('Загрузка завершена')
end;

end.
