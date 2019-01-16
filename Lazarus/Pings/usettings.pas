unit uSettings;

{$mode objfpc}{$H+}

//делать отдельный поток на пингование

interface

uses
  Classes, SysUtils,DOM,XMLRead,XMLWrite, strutils,httpsend,Dialogs;

type
  TLanguage = (tlRus,tlEng,tlOther);

type
  TCleanFilter = record
    ByCount, ByCode: boolean;
    Count, Code: integer;
end;

type
  { TPingService }
  TPingService = class
    private
      FDocument: TXMLDocument; //XML содержащий все сервисы для пинга
      FWorkList: TStringList;  //отфильтрованый лист сервисов для пинга
      FPingInfo: TXMLDocument; //XML сдержищий информацию для пинга
      FLog     : TStringList;  //лог выполнения операции
      FCleanFilter:TCleanFilter; //фильтр для чистки списка каталогов
      procedure AddRootNode;
      function  GetList: TStringList;
      function  ParseResponce(const Responce: TMemoryStream; var ResponceStr:string):boolean;//Result - удачно или неудачно прошел пинг
      procedure MarkService(const aURL:string; ErrCode:integer);//отмечаем сервис с ошибкой
      function  IsBrokenService(const Node: TDOMNode):boolean;
    public
      constructor Create(AFile: string);
      function  AppendServices(AFile: string):integer;
      function  CleanServices:integer;
      procedure DeleteAllServices;
      procedure ReloadServices;
      procedure SetPingInfo(const BlogName,BlogURL,PageURL,BlogRSS,Tags:string);
      function  AddService(aURL: string; Language: TLanguage):boolean;
      function  LoadServicesFromTxt(AFile:string):boolean;
      function  LoadServicesFromXML(AFile:string):boolean;
      procedure SaveListToXMLFile(AFile:string);
      procedure SaveListToTXTFile(AFile:string);
      function  Ping(const PingURL:string):boolean;
      property  Services: TStringList read GetList; //все сервисы
      property  WorkList: TStringList read FWorkList write FWorkList;
      property  PingInfo: TXMLDocument read FPingInfo;
      property  Log: TStringList read FLog write Flog;
      property  CleanFilter: TCleanFilter read FCleanFilter write FCleanFilter;

end;



implementation

{ TPingService }

procedure TPingService.AddRootNode;
var Root: TDOMNode;
begin
  if FDocument=nil then
    FDocument:=TXMLDocument.Create;
  Root:=FDocument.CreateElement('services');
  FDocument.AppendChild(Root);
end;

function TPingService.GetList: TStringList;
var i:integer;
    List: TDOMNodeList;
begin
  if FDocument=nil then Exit;
  try
    List:=FDocument.DocumentElement.GetElementsByTagName('ping');
    Result:=TStringList.Create;
    if List.Count>0 then
      for i:=0 to List.Count-1 do
        Result.Add(List.Item[i].TextContent);
  except
    Result.Free;
  end;
end;

function TPingService.ParseResponce(const Responce: TMemoryStream;
  var ResponceStr: string): boolean;
var Doc: TXMLDocument;
    Root,Node: TDOMNode;
    List,Childs: TDOMNodeList;
    i: integer;
    str: TStringStream;
begin
  try
    Doc:=TXMLDocument.Create;
    ReadXMLFile(Doc,Responce);
    List:=Doc.DocumentElement.GetElementsByTagName('member');
    for i:=0 to List.Count-1 do
      begin
        Childs:=List.Item[i].ChildNodes;
        if (LowerCase(Childs.Item[0].NodeName)='name')and(LowerCase(Childs.Item[0].TextContent)='flerror')then
           Result:=(StrToInt(Childs.Item[1].FirstChild.TextContent)=0)
        else
          if (LowerCase(Childs.Item[0].NodeName)='name')and(LowerCase(Childs.Item[0].TextContent)='message') then
            begin
              ResponceStr:=Childs.Item[1].FirstChild.TextContent;
            end;
      end;
  except
    FreeAndNil(Doc);
    Result:=false;
    str:=TStringStream.Create('');
    str.CopyFrom(Responce,Responce.Size);
    ResponceStr:='ответ не XML: '+str.DataString;
  end;
end;

procedure TPingService.MarkService(const aURL: string; ErrCode: integer);
var i:integer;
    Node:TDOMNode;
begin
  i:=Services.IndexOf(aURL);
  if i>-1 then
  Node:=FDocument.DocumentElement.GetElementsByTagName('ping').Item[i];
  TDOMElement(Node).SetAttribute('lasterror',inttostr(ErrCode));
  TDOMElement(Node).SetAttribute('error',IntToStr(StrToInt(TDOMElement(Node).AttribStrings['error'])+1));
end;

constructor TPingService.Create(AFile: string);
begin
  inherited Create;
  if FileExists(AFile) then
    ReadXMLFile(FDocument,AFile)
  else
    begin
      FDocument:=nil;
      AddRootNode;
    end;
  FWorkList:=TStringList.Create;
  FLog:=TStringList.Create;
end;

function TPingService.AppendServices(AFile: string): integer;
var i:integer;
    F:TextFile;
    aUrl: string;
    doc: TXMLDocument;
    NodeList: TDOMNodeList;
begin
  if not FileExists(AFile) then Exit;
Result:=0;
if LowerCase(ExtractFileExt(AFile))='.txt' then
  begin
    AssignFile(F, AFile);
    Reset(F);
    Result:=0;
    while not EOF(F) do
      begin
        readln(F,aUrl);
        if Services.IndexOf(aUrl)<0 then
          begin
            AddService(aUrl,tlOther);
            Inc(Result);
          end;
      end;
    CloseFile(F);
  end
else
  if LowerCase(ExtractFileExt(AFile))='.xml' then
    begin
      doc:=TXMLDocument.Create;
      ReadXMLFile(doc, AFile);
      NodeList:=doc.DocumentElement.GetElementsByTagName('ping');
      for i:=0 to NodeList.Count-1 do
        begin
          if Services.IndexOf(NodeList.Item[i].TextContent)<0 then
            begin
              AddService(NodeList.Item[i].TextContent,tlOther);
              Inc(Result);
            end;
        end;
    end
  else
    Exit;
end;

function TPingService.CleanServices:integer;
var i:integer;
    NodeList: TDOMNodeList;
begin
  if (not FCleanFilter.ByCode) and (not FCleanFilter.ByCount) then Exit;
  if FDocument=nil then Exit;
  Result:=0;
  i:=0;
  repeat
    NodeList:=FDocument.DocumentElement.GetElementsByTagName('ping');
    if IsBrokenService(NodeList.Item[i]) then
      begin
        FDocument.DocumentElement.RemoveChild(NodeList.Item[i]);
        inc(Result)
      end
    else
      inc(i);
  until i>NodeList.Count-1;

end;

procedure TPingService.DeleteAllServices;
var NodeList: TDOMNodeList;
begin
  repeat
    NodeList:=FDocument.DocumentElement.GetElementsByTagName('ping');
    FDocument.DocumentElement.RemoveChild(NodeList.Item[0]);
  until NodeList.Count=0;
end;

procedure TPingService.ReloadServices;
begin
 //
end;

function TPingService.IsBrokenService(const Node: TDOMNode): boolean;
var cod, count: boolean;
begin
 if StrToIntDef(TDOMElement(Node).AttribStrings['error'],0)>0 then //были ошибки
    begin
       if FCleanFilter.ByCode then //стоит фильтр по коду
          cod:=StrToIntDef(TDOMElement(Node).AttribStrings['lasterror'],0)=FCleanFilter.Code;
       if FCleanFilter.ByCount then //стоит фильтр по количеству
          count:=StrToIntDef(TDOMElement(Node).AttribStrings['error'],0)>FCleanFilter.Count;
       if FCleanFilter.ByCode and FCleanFilter.ByCount then
         begin
           if cod and count then Result:=true
                            else Result:=false;
         end
       else
         if FCleanFilter.ByCode then
           begin
             if cod then Result:=true
                    else Result:=false;
           end
         else
           if FCleanFilter.ByCount then
             begin
               if count then Result:=true
                        else Result:=false;
             end
           else
             Result:=false;
    end
 else
   Result:=false;
end;

procedure TPingService.SetPingInfo(const BlogName, BlogURL, PageURL, BlogRSS,
  Tags: string);
var Root,Node,Text:TDOMNode;
begin
  FreeAndNil(FPingInfo);
  FPingInfo:=TXMLDocument.Create;
  Root:=FPingInfo.CreateElement('methodCall');
  FPingInfo.AppendChild(Root);
  Root:=FPingInfo.DocumentElement;
  Node:=FPingInfo.CreateElement('methodName');
  Text:=FPingInfo.CreateTextNode('weblogUpdates.extendedPing');
  Node.AppendChild(Text);
  Root.AppendChild(Node);

  Node:=FPingInfo.CreateElement('params');
  Root:=Root.AppendChild(Node);
  Node:=FPingInfo.CreateElement('param');
  Root:=Root.AppendChild(Node);
  Node:=FPingInfo.CreateElement('value');
  Text:=FPingInfo.CreateTextNode(BlogName);
  Node.AppendChild(Text);
  Root.AppendChild(Node);
  Root:=Root.ParentNode;

  Node:=FPingInfo.CreateElement('param');
  Root:=Root.AppendChild(Node);
  Node:=FPingInfo.CreateElement('value');
  Text:=FPingInfo.CreateTextNode(BlogURL);
  Node.AppendChild(Text);
  Root.AppendChild(Node);
  Root:=Root.ParentNode;

  Node:=FPingInfo.CreateElement('param');
  Root:=Root.AppendChild(Node);
  Node:=FPingInfo.CreateElement('value');
  Text:=FPingInfo.CreateTextNode(PageURL);
  Node.AppendChild(Text);
  Root.AppendChild(Node);
  Root:=Root.ParentNode;

  Node:=FPingInfo.CreateElement('param');
  Root:=Root.AppendChild(Node);
  Node:=FPingInfo.CreateElement('value');
  Text:=FPingInfo.CreateTextNode(BlogRSS);
  Node.AppendChild(Text);
  Root.AppendChild(Node);
  Root:=Root.ParentNode;

  Node:=FPingInfo.CreateElement('param');
  Root:=Root.AppendChild(Node);
  Node:=FPingInfo.CreateElement('value');
  Text:=FPingInfo.CreateTextNode(StringsReplace(Tags,[','],['|'],[rfReplaceAll]));
  Node.AppendChild(Text);
  Root.AppendChild(Node);
end;

function TPingService.AddService(aURL: string; Language: TLanguage): boolean;
var Root: TDOMNode;
    Node,Text: TDOMNode;
begin
try
  if FDocument=nil then AddRootNode;
  Root:=FDocument.DocumentElement;
  Node:=FDocument.CreateElement('ping');
  case Language of
    tlRus:TDOMElement(Node).SetAttribute('lang','ru');
    tlEng:TDOMElement(Node).SetAttribute('lang','eng');
    tlOther:TDOMElement(Node).SetAttribute('lang','other');
  end;
  TDOMElement(Node).SetAttribute('error','0');
  TDOMElement(Node).SetAttribute('lasterror','0');
  if pos('http://',aURL)<0 then aURL:='http://'+aURL;
  Text:=FDocument.CreateTextNode(aURL);
  Node.AppendChild(Text);
  Root.AppendChild(Node);
  Result:=true;
except
  Result:=false;
end;
end;

function TPingService.LoadServicesFromTxt(AFile: string): boolean;
var i:integer;
    F:TextFile;
    url: string;
begin
try
  if FileExists(AFile) then
    begin
      FreeAndNil(FDocument);
      AssignFile(F,AFile);
      Reset(F);
      while not EOF(F) do
        begin
         Readln(F,url);
         AddService(url,tlOther);
        end;
      Result:=true;
      CloseFile(F)
    end
  else
    begin
      Result:=false;
      Exit;
    end;
except
  Result:=false;
  FreeAndNil(FDocument);
end;
end;

function TPingService.LoadServicesFromXML(AFile: string): boolean;
begin
try
  if not FileExists(AFile) then Exit;
  FreeAndNil(FDocument);
  FDocument:=TXMLDocument.Create;
  ReadXMLFile(FDocument, AFile);
except
  Result:=false;
  FreeAndNil(FDocument);
end;
end;

procedure TPingService.SaveListToXMLFile(AFile: string);
begin
  WriteXMLFile(FDocument,AFile);
end;

procedure TPingService.SaveListToTXTFile(AFile: string);
var i:integer;
    NodeList: TDOMNodeList;
    F: TextFile;
begin
 NodeList:=FDocument.DocumentElement.GetElementsByTagName('ping');
 AssignFile(F, AFile);
 Rewrite(F);
 for i:=0 to NodeList.Count-1 do
   writeln(F,NodeList.Item[i].TextContent);
 CloseFile(F);
end;

function TPingService.Ping(const PingURL: string): boolean;
var str: string;
begin
  with THTTPSend.Create do
    begin
      MimeType:='text/xml';
      WriteXMLFile(FPingInfo,Document);
      Result:=true;
      if HTTPMethod('post',PingURL) then
        begin
          if ResultCode=200 then//отправка прошла успешно - вернулся ответ
             Result:=ParseResponce(Document,str)
          else
            begin
              Result:=false;
              MarkService(PingURL, ResultCode);
            end;
        end
      else
        begin
          Result:=false;
          MarkService(PingURL, ResultCode);
        end;
      Log.Add(PingURL+' '+IntToStr(ResultCode)+' '+str);
    end;
end;

end.

