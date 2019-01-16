unit myrss;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, httpsend,dialogs;

type
  TRSSItem = record
    Title: string;
    Link: string;
    Description: string;
    Tags: TStringList;
end;

type
  TRSSElements = array of TRSSItem;
  TRSSItems = TRSSElements;

type
  { TFeedChanel }
  TFeedChanel = class
  private
    FDocument: TXMLDocument;
    FFeedAdress: string;
    procedure GetDocument(cURL:string);
    function GetItemsCount:integer;
    procedure SetFeedAdress(const cURL: string);
    function GetTitle(const i:integer):string;
    function GetDescr(const i:integer):string;
    function GetTags(const i:integer):TstringList;
    function GetDirectLink(const i:integer):string;
    function GetBlogTitle: string;
    function GetBlogURL: string;
  public
    constructor Create(cURL:string);
    function GetAllItems: TRSSItems;
    property ItemsCount: integer read GetItemsCount;
    property FeedAddress: string read FFeedAdress write SetFeedAdress;
    property Title[i:integer]:string read GetTitle;
    property Description[i:integer]:string read GetDescr;
    property Tags[i:integer]:TstringList read GetTags;
    property DirectLink[i:integer]: string read GetDirectLink;
    property BlogTitle: string read GetBlogTitle;
    property BlogURL: string read GetBlogURL;
end;

implementation

{ TFeedChanel }

procedure TFeedChanel.GetDocument(cURL: string);
var HTTPSend: THTTPSend;
begin
  if length(cURL)=0 then Exit;
  try
    if pos('?format=xml',cURL)<=0 then
      FFeedAdress:=cURL+'?format=xml'
    else
      FFeedAdress:=cURL;
    HTTPSend:=THTTPSend.Create;
    HTTPSend.HTTPMethod('get',FFeedAdress);
    ReadXMLFile(FDocument,HTTPSend.Document);
  finally
    FreeAndNil(HTTPSend);
  end;
end;

function TFeedChanel.GetItemsCount: integer;
begin
  Result:=FDocument.DocumentElement.GetElementsByTagName('item').Count;
end;

procedure TFeedChanel.SetFeedAdress(const cURL: string);
begin
  GetDocument(cURL);
end;

function TFeedChanel.GetTitle(const i: integer): string;
var List: TDOMNodeList;
begin
  if i>ItemsCount then Exit;
  List:=FDocument.DocumentElement.GetElementsByTagName('item');
  Result:=TDOMElement(List.Item[i]).GetElementsByTagName('title').Item[0].TextContent;
  {$IFDEF win32}
    Result:=UTF8Encode(Result);
  {$ENDIF}
end;

function TFeedChanel.GetDescr(const i: integer): string;
var List: TDOMNodeList;
begin
  if i>ItemsCount then Exit;
  List:=FDocument.DocumentElement.GetElementsByTagName('item');
  Result:=TDOMElement(List.Item[i]).GetElementsByTagName('description').Item[0].TextContent;
  {$IFDEF win32}
    Result:=UTF8Encode(Result);
  {$ENDIF}
end;

function TFeedChanel.GetTags(const i: integer): TstringList;
var j:integer;
    List: TDOMNodeList;
begin
  Result:=TStringList.Create;
  List:=TDOMElement(FDocument.DocumentElement.GetElementsByTagName('item').Item[i]).GetElementsByTagName('category');
  for j:=0 to List.Count-1 do
    begin
      Result.Add(List.Item[j].TextContent);
      {$IFDEF win32}
         Result.Strings[j]:=UTF8Encode(Result.Strings[j]);
      {$ENDIF}
    end;
end;

function TFeedChanel.GetDirectLink(const i: integer): string;
var List: TDOMNodeList;
    j:integer;
begin
  if i>ItemsCount then Exit;
  List:=FDocument.DocumentElement.GetElementsByTagName('item');
  Result:=TDOMElement(List.Item[i]).GetElementsByTagName('guid').Item[0].TextContent;
  //проверяем "правильность" УРЛа
  with (THTTPSend.Create) do
    begin
      HTTPMethod('get',result);
      case ResultCode of
        301,302:begin
                  for j:=0 to Headers.Count-1 do
                    begin
                      if pos('Location: ',Headers.Strings[j])>0 then
                        begin
                          Result:=copy(Headers.Strings[j],11, length(Headers.Strings[j])-10);
                          break;
                        end;
                    end;
                end;
        400:Result:='';
      end;
    end;
end;

function TFeedChanel.GetBlogTitle: string;
var List: TDOMNodeList;
begin
 List:=FDocument.DocumentElement.GetElementsByTagName('title');
 Result:=List.Item[0].TextContent;
 {$IFDEF win32}
    Result:=UTF8Encode(Result);
  {$ENDIF}
end;

function TFeedChanel.GetBlogURL: string;
var List: TDOMNodeList;
begin
 List:=FDocument.DocumentElement.GetElementsByTagName('link');
 Result:=List.Item[0].TextContent;
 {$IFDEF win32}
    Result:=UTF8Encode(Result);
  {$ENDIF}
end;


constructor TFeedChanel.Create(cURL: string);
var HTTPSend: THTTPSend;
begin
  inherited Create;
  GetDocument(cURL);
end;

function TFeedChanel.GetAllItems: TRSSItems;
var i:integer;
begin
SetLength(Result,ItemsCount);
  for i:=0 to ItemsCount-1 do
    begin
      Result[i].Title:=Title[i];
      Result[i].Description:=Description[i];
      Result[i].Link:=DirectLink[i];
      Result[i].Tags:=TStringList.Create;
      Result[i].Tags.Assign(Tags[i]);
    end;
end;

end.

