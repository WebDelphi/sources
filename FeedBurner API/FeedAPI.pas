unit FeedAPI;

interface

uses SysUtils, Classes, Dialogs, XMLDoc, xmldom, XMLIntf;

type
  TRSPError = record //ошибка
    code: integer;
    msg: string;
  end;

type
  TRSPBasicEntry = record
    date: TDate;
    circulation: integer;
    hits: integer;
    reach: integer;
    downloads: integer;
  end;

type
  TBasicResponce = record
    rsp_stat: string;
    error: TRSPError;
    entry: array of TRSPBasicEntry;
  end;

type
  TRSPItem = record
    Title: string;
    URL: string;
    itemviews: integer;
    clickthroughs: integer;
  end;

type
  TRSPItemEntry = record
    Basic: TRSPBasicEntry;
    Items: array of TRSPItem;
  end;

type
  TItemResponce = record
    rsp_stat: string;
    error: TRSPError;
    entry: array of TRSPItemEntry;
  end;

type
  TReferrer = record
    URL: string;
    itemviews: integer;
    clickthroughs: integer;
  end;

type
  TRSPResyncItem = record
    Basic: TRSPItem;
    Referrer: array of TReferrer;
  end;

type
  TRSPResyncItemEntry = record
    Basic: TRSPBasicEntry;
    Items: array of TRSPResyncItem;
  end;

type
  TResyncItemResponce = record
    rsp_stat: string;
    error: TRSPError;
    entry: array of TRSPResyncItemEntry;
  end;

type
  TDiscreteDates = array of TDate;

function SetAPIDate(const cDate: TDate): string; // перевод даты в формат API
function GetAPIDate(cDate: string): TDate; // перевод даты из формата API в TDate
function SetAPIDiscreteDate(Dates: TDiscreteDates): string;

{ обработка запросов с GetFeedData }
function ParseFeedData(URL: string): TBasicResponce; // разбор XML-документа
function GetCurFeedData(const FeedURI: string; cDate: TDate): TBasicResponce;
// текущее состояние фида
{ информация по фиду за период времени }
function GetFeedDataBetween(const FeedURI: string; StartDate, EndDate: TDate)
  : TBasicResponce;
{ информация по фиду за несколько дней (дискретно) }
function GetFeedDataDiscrete(const FeedURI: string; Dates: TDiscreteDates)
  : TBasicResponce;

{ обработка запросов GetItemData }
function ParseItemData(URL: string): TItemResponce;
// текущая информация по элементам фида
function GetCurItemData(FeedURI: string; cDate: TDate): TItemResponce;
// информация по элементам фида за период
function GetItemDataBetween(FeedURI: string; StartDate, EndDate: TDate)
  : TItemResponce;
function GetItemDataDiscrete(FeedURI: string; Dates: TDiscreteDates)
  : TItemResponce;
// информация по одному из элементов фида
function GetCurSingleItemData(FeedURI, ElementURL: string; cDate: TDate)
  : TItemResponce;
function GetSingleItemDataBetween(FeedURI, ElementURL: string;
  StartDate, EndDate: TDate): TItemResponce;
function GetSingleItemDataDiscrete(FeedURI, ElementURL: string;
  Dates: TDiscreteDates): TItemResponce;

{ обработка запросов GetResyndicationData }
function ParseResyncData(URL: string): TResyncItemResponce;
function GetCurResyncData(FeedURI: string; cDate: TDate): TResyncItemResponce;
function GetResyncDataBetween(FeedURI: string; StartDate, EndDate: TDate)
  : TResyncItemResponce;
function GetResyncDataDiscrete(FeedURI: string; Dates: TDiscreteDates)
  : TResyncItemResponce;
function GetResyncSingleData(FeedURI, ElementURL: string; cDate: TDate)
  : TResyncItemResponce;
function GetResyncSingleDataBetween(FeedURI, ElementURL: string;
  StartDate, EndDate: TDate): TResyncItemResponce;
function GetResyncSingleDataDiscrete(FeedURI, ElementURL: string;
  Dates: TDiscreteDates): TResyncItemResponce;

implementation

function SetAPIDiscreteDate(Dates: TDiscreteDates): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to length(Dates) - 1 do
    Result := Result + SetAPIDate(Dates[i]) + '/';
  Delete(Result, length(Result), 1);
end;

function GetResyncSingleDataDiscrete(FeedURI, ElementURL: string;
  Dates: TDiscreteDates): TResyncItemResponce;
var
  i: integer;
  URL: string;
begin
  Result := ParseResyncData(
    'http://feedburner.google.com/api/awareness/1.0/GetResyndicationData?uri='
      + FeedURI + '&&itemurl=' + ElementURL + '&&dates=' + SetAPIDiscreteDate
      (Dates));
end;

function GetResyncSingleDataBetween(FeedURI, ElementURL: string;
  StartDate, EndDate: TDate): TResyncItemResponce;
begin
  Result := ParseResyncData(
    'http://feedburner.google.com/api/awareness/1.0/GetResyndicationData?uri='
      + FeedURI + '&&itemurl=' + ElementURL + '&&dates=' + SetAPIDate
      (StartDate) + ',' + SetAPIDate(EndDate));
end;

function GetResyncSingleData(FeedURI, ElementURL: string; cDate: TDate)
  : TResyncItemResponce;
begin
  Result := ParseResyncData(
    'http://feedburner.google.com/api/awareness/1.0/GetResyndicationData?uri='
      + FeedURI + '&&itemurl=' + ElementURL + '&&dates=' + SetAPIDate(cDate));
end;

function GetResyncDataDiscrete(FeedURI: string; Dates: TDiscreteDates)
  : TResyncItemResponce;
begin
  Result := ParseResyncData(
    'http://feedburner.google.com/api/awareness/1.0/GetResyndicationData?uri='
      + FeedURI + '&&dates=' + SetAPIDiscreteDate(Dates));
end;

function GetResyncDataBetween(FeedURI: string; StartDate, EndDate: TDate)
  : TResyncItemResponce;
begin
  Result := ParseResyncData(
    'http://feedburner.google.com/api/awareness/1.0/GetResyndicationData?uri='
      + FeedURI + '&&dates=' + SetAPIDate(StartDate) + ',' + SetAPIDate(EndDate)
    );
end;

function GetCurResyncData(FeedURI: string; cDate: TDate): TResyncItemResponce;
begin
  // http://feedburner.google.com/api/awareness/1.0/GetResyndicationData?uri=myDelphi&dates=2009-11-08
  Result := ParseResyncData(
    'http://feedburner.google.com/api/awareness/1.0/GetResyndicationData?uri='
      + FeedURI + '&&dates=' + SetAPIDate(cDate));
end;

function ParseResyncData(URL: string): TResyncItemResponce;
var
  i, j, k: integer;
  Doc: IXMLDocument;
  EntryNodes: IDOMNodeList;
  ItemNodes: IDOMNodeList;
  ReferNodes: IDOMNodeList;
  SingleNode: IDOMNodeEx;
begin
  Doc := NewXMLDocument(); // создаем пустой документ
  { загружаем XML-документ API }
  Doc.LoadFromFile(URL);
  if Doc.IsEmptyDoc then
  begin
    ShowMessage('Пустой документ');
    Exit
  end;
  ItemNodes := Doc.DOMDocument.getElementsByTagName('rsp');
  SingleNode := ItemNodes.item[0] as IDOMNodeEx; // элемент rsp
  Result.rsp_stat := SingleNode.attributes.getNamedItem('stat').nodeValue;
  if Result.rsp_stat = 'ok' then // получен ответ с результатами
  begin
    EntryNodes := Doc.DOMDocument.getElementsByTagName('entry');
    SetLength(Result.entry, EntryNodes.length);
    for i := 0 to EntryNodes.length - 1 do
    begin
      SingleNode := EntryNodes.item[i] as IDOMNodeEx;
      // заполняем данные по entry
      with Result.entry[i].Basic do
      begin
        date := GetAPIDate(SingleNode.attributes.getNamedItem('date')
            .nodeValue);
        circulation := StrToInt(SingleNode.attributes.getNamedItem
            ('circulation').nodeValue);
        hits := StrToInt(SingleNode.attributes.getNamedItem('hits').nodeValue);
        downloads := StrToInt(SingleNode.attributes.getNamedItem('downloads')
            .nodeValue);
        reach := StrToInt(SingleNode.attributes.getNamedItem('reach')
            .nodeValue);
      end;
      // выбираеем все элементы Item
      ItemNodes := SingleNode.childNodes;
      SetLength(Result.entry[i].Items, ItemNodes.length);
      for j := 0 to ItemNodes.length - 1 do
      begin
        SingleNode := ItemNodes.item[j] as IDOMNodeEx;
        with Result.entry[i].Items[j].Basic do
        begin
          Title := SingleNode.attributes.getNamedItem('title').nodeValue;
          URL := SingleNode.attributes.getNamedItem('url').nodeValue;
          itemviews := StrToInt(SingleNode.attributes.getNamedItem('itemviews')
              .nodeValue);
          clickthroughs := StrToInt(SingleNode.attributes.getNamedItem
              ('clickthroughs').nodeValue);
        end;
        ReferNodes := SingleNode.childNodes; // выбираем реферреров
        SetLength(Result.entry[i].Items[j].Referrer, ReferNodes.length);
        for k := 0 to ReferNodes.length - 1 do
        begin
          SingleNode := ReferNodes.item[k] as IDOMNodeEx;
          with Result.entry[i].Items[j].Referrer[k] do
          begin
            URL := SingleNode.attributes.getNamedItem('url').nodeValue;
            itemviews := StrToInt(SingleNode.attributes.getNamedItem
                ('itemviews').nodeValue);
            clickthroughs := StrToInt(SingleNode.attributes.getNamedItem
                ('clickthroughs').nodeValue);
          end;
        end;
      end;
    end;
  end
  else // получен ответ об ошибке
  begin
    ItemNodes := Doc.DOMDocument.getElementsByTagName('err');
    SingleNode := ItemNodes.item[0] as IDOMNodeEx;
    Result.error.code := StrToInt(SingleNode.attributes.getNamedItem('code')
        .nodeValue);
    Result.error.msg := SingleNode.attributes.getNamedItem('msg').nodeValue;
  end;
end;

function GetSingleItemDataDiscrete(FeedURI, ElementURL: string;
  Dates: TDiscreteDates): TItemResponce;
begin
  Result := ParseItemData(
    'http://feedburner.google.com/api/awareness/1.0/GetItemData?uri=' +
      FeedURI + '&&itemurl=' + ElementURL + '&&dates=' + SetAPIDiscreteDate
      (Dates));
end;

function GetSingleItemDataBetween(FeedURI, ElementURL: string;
  StartDate, EndDate: TDate): TItemResponce;
begin
  Result := ParseItemData(
    'http://feedburner.google.com/api/awareness/1.0/GetItemData?uri=' +
      FeedURI + '&&itemurl=' + ElementURL + '&&dates=' + SetAPIDate(StartDate)
      + ',' + SetAPIDate(EndDate));
end;

function GetCurSingleItemData(FeedURI, ElementURL: string; cDate: TDate)
  : TItemResponce;
begin
  Result := ParseItemData(
    'http://feedburner.google.com/api/awareness/1.0/GetItemData?uri=' +
      FeedURI + '&&itemurl=' + ElementURL + '&&dates=' + SetAPIDate(cDate));
end;

function GetItemDataDiscrete(FeedURI: string; Dates: TDiscreteDates)
  : TItemResponce;
begin
  Result := ParseItemData(
    'http://feedburner.google.com/api/awareness/1.0/GetItemData?uri=' +
      FeedURI + '&&dates=' + SetAPIDiscreteDate(Dates));
end;

function GetItemDataBetween(FeedURI: string; StartDate, EndDate: TDate)
  : TItemResponce;
begin
  Result := ParseItemData(
    'http://feedburner.google.com/api/awareness/1.0/GetItemData?uri=' +
      FeedURI + '&&dates=' + SetAPIDate(StartDate) + ',' + SetAPIDate(EndDate));
end;

function GetCurItemData(FeedURI: string; cDate: TDate): TItemResponce;
begin
  Result := ParseItemData(
    'http://feedburner.google.com/api/awareness/1.0/GetItemData?uri=' +
      FeedURI + '&&dates=' + SetAPIDate(cDate));
end;

function ParseItemData(URL: string): TItemResponce;
var
  i, j: integer;
  Doc: IXMLDocument;
  ItemNodes: IDOMNodeList;
  childNodes: IDOMNodeList;
  ChildNode: IDOMNodeEx;
begin
  Doc := NewXMLDocument(); // создаем пустой документ
  { загружаем XML-документ API }
  Doc.LoadFromFile(URL);
  if Doc.IsEmptyDoc then
  begin
    ShowMessage('Пустой документ');
    Exit
  end;
  ItemNodes := Doc.DOMDocument.getElementsByTagName('rsp');
  ChildNode := ItemNodes.item[0] as IDOMNodeEx; // элемент rsp
  Result.rsp_stat := ChildNode.attributes.getNamedItem('stat').nodeValue;
  if Result.rsp_stat = 'ok' then // получен ответ с результатами
  begin
    ItemNodes := Doc.DOMDocument.getElementsByTagName('entry');
    // выбираем все элементы entry
    SetLength(Result.entry, ItemNodes.length);
    for i := 0 to ItemNodes.length - 1 do // проходим по всем entry
    begin
      ChildNode := ItemNodes.item[i] as IDOMNodeEx;
      with Result.entry[i].Basic do // сведения по фиду
      begin
        date := GetAPIDate(ChildNode.attributes.getNamedItem('date').nodeValue);
        circulation := StrToInt(ChildNode.attributes.getNamedItem('circulation')
            .nodeValue);
        hits := StrToInt(ChildNode.attributes.getNamedItem('hits').nodeValue);
        downloads := StrToInt(ChildNode.attributes.getNamedItem('downloads')
            .nodeValue);
        reach := StrToInt(ChildNode.attributes.getNamedItem('reach').nodeValue);
      end;
      childNodes := ItemNodes.item[i].childNodes;
      // выбираем все элементы Items для Entry
      SetLength(Result.entry[i].Items, childNodes.length);
      for j := 0 to childNodes.length - 1 do
      begin
        ChildNode := childNodes.item[j] as IDOMNodeEx;
        with Result.entry[i].Items[j] do
        begin
          Title := ChildNode.attributes.getNamedItem('title').nodeValue;
          URL := ChildNode.attributes.getNamedItem('url').nodeValue;
          itemviews := StrToInt(ChildNode.attributes.getNamedItem('itemviews')
              .nodeValue);
          clickthroughs := StrToInt(ChildNode.attributes.getNamedItem
              ('clickthroughs').nodeValue);
        end;
      end;
    end;
  end
  else // получен ответ об ошибке
  begin
    ItemNodes := Doc.DOMDocument.getElementsByTagName('err');
    ChildNode := ItemNodes.item[0] as IDOMNodeEx;
    Result.error.code := StrToInt(ChildNode.attributes.getNamedItem('code')
        .nodeValue);
    Result.error.msg := ChildNode.attributes.getNamedItem('msg').nodeValue;
  end;
end;

function ParseFeedData(URL: string): TBasicResponce; // разбор XML-документа
var
  i: integer;
  Doc: IXMLDocument;
  ItemNodes: IDOMNodeList;
  ChildNode: IDOMNodeEx;
begin
  Doc := NewXMLDocument(); // создаем пустой документ
  { загружаем XML-документ API }
  Doc.LoadFromFile(URL);
  if Doc.IsEmptyDoc then
  begin
    ShowMessage('Пустой документ');
    Exit
  end;
  ItemNodes := Doc.DOMDocument.getElementsByTagName('rsp');
  ChildNode := ItemNodes.item[0] as IDOMNodeEx; // элемент rsp
  Result.rsp_stat := ChildNode.attributes.getNamedItem('stat').nodeValue;
  if Result.rsp_stat = 'ok' then // получен ответ с результатами
  begin
    ItemNodes := Doc.DOMDocument.getElementsByTagName('entry');
    SetLength(Result.entry, ItemNodes.length);
    // читаем данные о фиде
    for i := 0 to ItemNodes.length - 1 do
    begin
      ChildNode := ItemNodes.item[i] as IDOMNodeEx;
      with Result.entry[i] do
      begin
        date := GetAPIDate(ChildNode.attributes.getNamedItem('date').nodeValue);
        circulation := StrToInt(ChildNode.attributes.getNamedItem('circulation')
            .nodeValue);
        hits := StrToInt(ChildNode.attributes.getNamedItem('hits').nodeValue);
        downloads := StrToInt(ChildNode.attributes.getNamedItem('downloads')
            .nodeValue);
        reach := StrToInt(ChildNode.attributes.getNamedItem('reach').nodeValue);
      end;
    end;
  end
  else // получен ответ об ошибке
  begin
    ItemNodes := Doc.DOMDocument.getElementsByTagName('err');
    ChildNode := ItemNodes.item[0] as IDOMNodeEx;
    Result.error.code := StrToInt(ChildNode.attributes.getNamedItem('code')
        .nodeValue);
    Result.error.msg := ChildNode.attributes.getNamedItem('msg').nodeValue;
  end;
end;

function GetFeedDataDiscrete(const FeedURI: string; Dates: TDiscreteDates)
  : TBasicResponce;
begin
  Result := ParseFeedData(
    'http://feedburner.google.com/api/awareness/1.0/GetFeedData?uri=' +
      FeedURI + '&&dates=' + SetAPIDiscreteDate(Dates));
end;

function GetFeedDataBetween(const FeedURI: string; StartDate, EndDate: TDate)
  : TBasicResponce;
begin
  { http://feedburner.google.com/api/awareness/1.0/GetFeedData?uri=<feeduri>&dates=<StartDate>,<EndDate> }
  Result := ParseFeedData(
    'http://feedburner.google.com/api/awareness/1.0/GetFeedData?uri=' +
      FeedURI + '&&dates=' + SetAPIDate(StartDate) + ',' + SetAPIDate(EndDate));
end;

function GetCurFeedData(const FeedURI: string; cDate: TDate): TBasicResponce;
begin
  // feedburner.google.com/api/awareness/1.0/GetFeedData?uri=<feeduri>
  Result := ParseFeedData(
    'http://feedburner.google.com/api/awareness/1.0/GetFeedData?uri=' +
      FeedURI + '&&dates=' + SetAPIDate(cDate))
end;

function SetAPIDate(const cDate: TDate): string;
var
  Day, Month, Year: word;
begin
  DecodeDate(cDate, Year, Month, Day);
  Result := IntToStr(Year) + '-';
  if Month < 10 then
    Result := Result + '0' + IntToStr(Month) + '-'
  else
    Result := Result + IntToStr(Month) + '-';
  if Day < 10 then
    Result := Result + '0' + IntToStr(Day)
  else
    Result := Result + IntToStr(Day);
end;

function GetAPIDate(cDate: string): TDate;
var
  Day, Month, Year: integer;
begin
  Year := StrToInt(copy(cDate, 1, 4));
  Delete(cDate, 1, 5);
  Month := StrToInt(copy(cDate, 1, 2));
  Delete(cDate, 1, 3);
  Day := StrToInt(copy(cDate, 1, 2));
  Result := EncodeDate(Year, Month, Day);
end;

end.
