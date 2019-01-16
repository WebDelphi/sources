unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, httpsend, synacode, ssl_openssl, synautil,
  xmldoc, xmlintf, Generics.Collections, Vcl.ComCtrls, Vcl.ExtCtrls;

const
  cWebDAVServer = 'https://webdav.yandex.ru/';

type
  TWDResource = class
  private
    FHref         : string;
    FStatusCode   : integer;
    FContentLength: int64;
    FCreationDate : TDateTime;
    FLastmodified : TDateTime;
    FDisplayName  : string;
    FContentType  : string;
    FCollection   : Boolean;
  public
    property StatusCode   : integer read FStatusCode;
    property ContentLength: int64 read FContentLength;
    property CreationDate : TDateTime read FCreationDate;
    property Lastmodified : TDateTime read FLastmodified;
    property DisplayName  : string read FDisplayName;
    property ContentType  : string read FContentType;
    property Href: string read FHref;
    property Collection   : Boolean read FCollection;
end;

TWDResourceList = class(TList<TWDResource>)
public
  constructor Create;
  destructor Destroy;override;
  procedure Clear;
end;

type
  TWebDAVSend = class
  private
    FHTTP: THTTPSend;
    FToken: AnsiString;
    FPassword: string;
    FLogin: string;
    procedure SetLogin(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetToken;
    function EncodeUTF8URI(const URI: string): string;
    function GetRequestURL(const Element: string; EncodePath:boolean=True):string;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    /// Получение свойств каталога или файла.
    /// </summary>
    /// <param name="Depth">
    /// 0 — запрашиваются свойства файла или каталога, непосредственно
    /// указанного в запросе. 1 — запрашиваются свойства каталога, а также
    /// всех элементов, находящихся на первом уровне каталога.
    /// </param>
    /// <param name="Element">
    /// Файл или каталог для которого необходимо получить свойства.
    /// </param>
    /// <returns>
    /// XML-документ, содержащий запрошенные свойства
    /// </returns>
    /// <remarks>
    /// Если Element не определен, то возвращаются свойства корневого каталога
    /// </remarks>
    function PROPFIND(Depth: integer; const Element: String): string;

    ///	<summary>
    ///	  Создание нового каталога на сервере
    ///	</summary>
    ///	<param name="ElementPath">
    ///	  путь к новому каталогу, включая его имя. Согласно протоколу, в
    ///	  результате одного запроса может быть создан только один каталог. Если
    ///	  приложение отправляет запрос о создании каталога a/b/c/, а в каталоге
    ///	  a/ нет каталога b/, то сервис не создает каталог b/, а отвечает c
    ///	  кодом 409 Conflict.
    ///	</param>
    ///	<returns>
    ///	  True - если каталог создан успешно
    ///	</returns>
    function MKCOL(const ElementPath: string):boolean;
    /// <summary>
    ///   Скачивание документа с сервера
    /// </summary>
    function Get(const ElementHref:string; var Response:TStream):boolean;

    property Login: string read FLogin write SetLogin;
    property Password: string read FPassword write SetPassword;
  end;

type
  TForm5 = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;
  WebDAV: TWebDAVSend;
  Resources: TWDResourceList;

implementation

uses DateUtils;

resourcestring
  rsPropfindError = 'Ошибка при выполнении запроса PROPFIND';

function TzSpecificLocalTimeToSystemTime(lpTimeZoneInformation: PTimeZoneInformation; var lpLocalTime, lpUniversalTime: TSystemTime): BOOL; stdcall;
  external kernel32 name 'TzSpecificLocalTimeToSystemTime';
{$EXTERNALSYM TzSpecificLocalTimeToSystemTime}

{$R *.dfm}
function SystemTimeToUTC(Sys: TDateTime): TDateTime;
var
  TimeZoneInf: _TIME_ZONE_INFORMATION;
  SysTime, LocalTime: TSystemTime;
begin
  if GetTimeZoneInformation(TimeZoneInf) < $FFFFFFFF then
  begin
    DatetimetoSystemTime(Sys, SysTime);
    if TzSpecificLocalTimeToSystemTime(@TimeZoneInf, SysTime, LocalTime) then
      Result := SystemTimeToDateTime(LocalTime)
    else
      Result := Sys;
  end
  else
    Result := Sys;
end;

function UTCToSystemTime(UTC: TDateTime): TDateTime;
var
  TimeZoneInf: _TIME_ZONE_INFORMATION;
  UTCTime, LocalTime: TSystemTime;
begin
  if GetTimeZoneInformation(TimeZoneInf) < $FFFFFFFF then
  begin
    DatetimetoSystemTime(UTC, UTCTime);
    if SystemTimeToTzSpecificLocalTime(@TimeZoneInf, UTCTime, LocalTime) then
    begin
      Result := SystemTimeToDateTime(LocalTime);
    end
    else
      Result := UTC;
  end
  else
    Result := UTC;
end;


function ISODateTime2UTC(const AValue: string; ADateOnly: Boolean = False): TDateTime;
// 2012-03-13
// 2012-03-13T15:58Z
// 20120313T1558Z
// 2012-03-13T00:00:00.000+07:00
// 20120417T100000
var
  I, Len: Integer;
  DD, MM, YY: Word;
  HH, MN, SS, ZZ: Word;
  HH1, MN1: Integer;
  TimeOffsetSign: Char;
begin
  Len := Length(AValue);
  YY := StrToIntDef(copy(AValue, 1, 4), 0);
  I := 5;
  if (I <= Len) and (AValue[I] = '-') then
    inc(I);
  MM := StrToIntDef(copy(AValue, I, 2), 0);
  inc(I, 2);
  if (I <= Len) and (AValue[I] = '-') then
    inc(I);
  DD := StrToIntDef(copy(AValue, I, 2), 0);
  inc(I, 2);
  HH := 0;
  MN := 0;
  SS := 0;
  ZZ := 0;
  if not ADateOnly and (I <= Len) and (AValue[I] = 'T') then
  begin
    inc(I);
    HH := StrToIntDef(copy(AValue, I, 2), 0);
    inc(I, 2);
    if (I <= Len) and CharInSet(AValue[I], [':', '0' .. '5']) then
    begin
      if AValue[I] = ':' then
        inc(I);
      MN := StrToIntDef(copy(AValue, I, 2), 0);
      inc(I, 2);
      if (I <= Len) and CharInSet(AValue[I], [':', '0' .. '5']) then
      begin
        if AValue[I] = ':' then
          inc(I);
        SS := StrToIntDef(copy(AValue, I, 2), 0);
        inc(I, 2);
        if (I <= Len) and (AValue[I] = '.') then
        begin
          inc(I);
          ZZ := StrToIntDef(copy(AValue, I, 3), 0);
          inc(I, 3);
        end;
      end;
    end;
  end;
  Result := EncodeDateTime(YY, MM, DD, HH, MN, SS, ZZ);
  if ADateOnly then
    Exit;
  if (I <= Len) and CharInSet(AValue[I], ['Z', '+', '-']) then
  begin
    if AValue[I] <> 'Z' then
    begin
      TimeOffsetSign := AValue[I];
      inc(I);
      HH1 := StrToIntDef(copy(AValue, I, 2), 0);
      inc(I, 2);
      if (I <= Len) and CharInSet(AValue[I], [':', '0' .. '5']) then
      begin
        if AValue[I] = ':' then
          inc(I);
        MN1 := StrToIntDef(copy(AValue, I, 2), 0);
      end
      else
        MN1 := 0;
      if TimeOffsetSign = '+' then
      begin
        HH1 := -HH1;
        MN1 := -MN1;
      end;
      Result := IncHour(Result, HH1);
      Result := IncMinute(Result, MN1);
    end;
  end
  else Result := SystemTimeToUTC(Result);
end;


procedure TForm5.Button1Click(Sender: TObject);

procedure ParseResources(const AXMLStr: string);
var XMLDoc: IXMLDocument;
    ResponseNode,ChildNode,PropNodeChild, PropertyNode: IXMLNode;
     s, su,Value: string;
begin
  XMLDoc:=TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromXML(AXMLStr);
    if not XMLDoc.IsEmptyDoc then
      begin
        //выбираем первый узел d:response
        ResponseNode:=XMLDoc.DocumentElement.ChildNodes.First;
        while Assigned(ResponseNode) do
          begin
            //создаем запись нового ресурса в списке
            Resources.Add(TWDResource.Create);
            //проходим по дочерним узлам d:response
            ChildNode:=ResponseNode.ChildNodes.First;
            while Assigned(ChildNode) do
              begin
                if ChildNode.NodeName='d:href' then
                   Resources.Last.FHref:=ChildNode.Text
                else
                  //нашли узел со свойствами ресурса
                  if ChildNode.NodeName='d:propstat' then
                    begin
                      //выбираем первый дочерний узел, обычно - это d:status
                      PropNodeChild:=ChildNode.ChildNodes.First;
                      while Assigned(PropNodeChild) do
                        begin
                          //считываем код статуса
                          if PropNodeChild.NodeName='d:status' then
                            begin
                              Value:=PropNodeChild.Text;
                              s := Trim(SeparateRight(Value, ' '));
                              su := Trim(SeparateLeft(s, ' '));
                              Resources.Last.FStatusCode:=StrToIntDef(su, 0);
                            end
                          else
                            //нашли узел d:prop - проходимся по его дочерним узлам
                            if PropNodeChild.NodeName='d:prop' then
                              begin
                                PropertyNode:=PropNodeChild.ChildNodes.First;
                                while Assigned(PropertyNode) do
                                  begin
                                    if PropertyNode.NodeName='d:creationdate' then
                                      Resources.Last.FCreationDate:=UTCToSystemTime(ISODateTime2UTC(PropertyNode.Text))
                                    else
                                      if PropertyNode.NodeName='d:displayname' then
                                        Resources.Last.FDisplayName:=Utf8ToAnsi(PropertyNode.Text)
                                      else
                                        if PropertyNode.NodeName='d:getcontentlength' then
                                          Resources.Last.FContentLength:=PropertyNode.NodeValue
                                        else
                                          if PropertyNode.NodeName='d:getlastmodified' then
                                            Resources.Last.FLastmodified:=DecodeRfcDateTime(PropertyNode.Text)
                                          else
                                            if PropertyNode.NodeName='d:resourcetype' then
                                              Resources.Last.FCollection:=PropertyNode.ChildNodes.Count>0;
                                    //выбираем следующий дочерний узел у d:prop
                                    PropertyNode:=PropertyNode.NextSibling;
                                  end;
                              end;
                          //выбираем следующий дочерний узел у d:propstat
                          PropNodeChild:=PropNodeChild.NextSibling;
                        end;
                    end;
                //выбираем следующий дочерний узел у d:response
                ChildNode:=ChildNode.NextSibling;
              end;
            //выбираем следующий узел d:response
            ResponseNode:=ResponseNode.NextSibling;
          end;
      end;
  finally
    XMLDoc:=nil;
  end;
end;

var Str: string;
  I: Integer;
begin
  WebDAV.Login := Edit1.Text;
  WebDAV.Password := Edit2.Text;
  Resources.Clear;
  Str:=WebDAV.PROPFIND(1, InputBox('Ресурс', 'Ресурс', ''));
  if Length(Trim(Str))>0 then
    begin
      ParseResources(Str);
      for I := 0 to Resources.Count-1 do
        begin
          with ListView1.Items.Add do
            begin
              Caption:=Resources[i].DisplayName;
              SubItems.Add(Resources[i].Href);
              SubItems.Add(DateTimeToStr(Resources[i].CreationDate));
              SubItems.Add(DateTimeToStr(Resources[i].Lastmodified));
              SubItems.Add(IntToStr(Resources[i].ContentLength));
              if Resources[i].Collection then
                SubItems.Add('yes')
              else
                SubItems.Add('no');
              SubItems.Add(IntToStr(Resources[i].StatusCode))
            end;
        end;

    end;
end;

{ TWebDAVSend }

constructor TWebDAVSend.Create;
begin
  inherited;
  FHTTP := THTTPSend.Create;
end;

destructor TWebDAVSend.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TWebDAVSend.EncodeUTF8URI(const URI: string): string;
var
  i: integer;
  Char: AnsiChar;
begin
  result := '';
  for i := 1 to length(URI) do
  begin
    if not(URI[i] in URLFullSpecialChar) then
      begin
      for Char in UTF8String(URI[i]) do
        Result:=Result+'%'+IntToHex(Ord(Char), 2)
      end
    else
      Result:=Result+URI[i];
  end;
end;

function TWebDAVSend.Get(const ElementHref: string; var Response:TStream): boolean;
var URL: string;
begin
  if not Assigned(Response) then Exit;
  URL:=GetRequestURL(ElementHref,false);
  with FHTTP do
  begin
    Headers.Clear;
    Document.Clear;
    Headers.Add('Authorization: Basic ' + FToken);
    Headers.Add('Accept: */*');
    if HTTPMethod('GET', URL) then
      begin
        Result:=ResultCode=200;
        if not Result then
          raise Exception.Create(IntToStr(ResultCode)+' '+ResultString)
        else
          Document.SaveToStream(Response);
      end
    else
      raise Exception.Create(rsPropfindError+' '+ResultString);
  end;
end;

function TWebDAVSend.GetRequestURL(const Element: string; EncodePath:boolean): string;
var URI: string;
begin
  if Length(Element)>0 then
    begin
      URI:=Element;
      if URI[1]='/' then
        Delete(URI,1,1);
      if EncodePath then
        Result:=cWebDAVServer+EncodeUTF8URI(URI)
      else
        Result:=cWebDAVServer+URI
    end
  else
   Result:=cWebDAVServer;
end;

function TWebDAVSend.MKCOL(const ElementPath: string): boolean;
begin
  Result:=False;
  with FHTTP do
  begin
    Headers.Clear;
    Document.Clear;
    Headers.Add('Authorization: Basic ' + FToken);
    Headers.Add('Accept: */*');
    if HTTPMethod('MKCOL', GetRequestURL(ElementPath)) then
      begin
        Result:=ResultCode=201;
        if not Result then
          raise Exception.Create(IntToStr(ResultCode)+' '+ResultString);
      end
    else
      raise Exception.Create(rsPropfindError+' '+ResultString);
  end;
end;

function TWebDAVSend.PROPFIND(Depth: integer; const Element: String): string;
begin
  with FHTTP do
  begin
    Headers.Clear;
    Document.Clear;
    Headers.Add('Authorization: Basic ' + FToken);
    Headers.Add('Depth: ' + IntToStr(Depth));
    Headers.Add('Accept: */*');
    if HTTPMethod('PROPFIND', GetRequestURL(Element)) then
      result := ReadStrFromStream(Document, Document.Size)
    else
      raise Exception.Create(rsPropfindError+' '+ResultString);
  end;
end;

procedure TWebDAVSend.SetToken;
begin
  FToken := EncodeBase64(FLogin + ':' + FPassword);
end;

procedure TWebDAVSend.SetLogin(const Value: string);
begin
  FLogin := Value;
  SetToken;
end;

procedure TWebDAVSend.SetPassword(const Value: string);
begin
  FPassword := Value;
  SetToken;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  WebDAV := TWebDAVSend.Create;
  Resources:=TWDResourceList.Create;
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
  Resources.Free;
  WebDAV.Free;
end;

procedure TForm5.ListView1DblClick(Sender: TObject);
var Res:TWDResource;
    Stream:TStream;
begin
  if ListView1.ItemIndex>-1 then
    Res:=Resources[ListView1.ItemIndex];
  Stream:=TMemoryStream.Create;
  try
    if WebDAV.Get(Res.Href,Stream) then
      TMemoryStream(Stream).SaveToFile(ExtractFilePath(Application.ExeName)+Res.DisplayName);
  finally
    Stream.Free;
  end;
end;

{ TWDResourceList }

procedure TWDResourceList.Clear;
var i:integer;
begin
  for I := Count-1 downto 0 do
    Extract(Items[0]).Free;
  inherited Clear;
end;

constructor TWDResourceList.Create;
begin
  inherited Create;
end;

destructor TWDResourceList.Destroy;
begin
  Clear;
  inherited;
end;

end.
