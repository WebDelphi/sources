unit ISPDB;

interface

uses Classes, SysUtils, Generics.Collections, httpsend, dnssend, ssl_openssl,
  Xml.XMLDoc, Xml.XMLIntf;

type
  TServerType = (stIMAP, stPOP, stSMTP);
  TSocketType = (sSSL, sSTARTTLS, sPlain);
  TUserName = (unAddress, unLocalPart, unDomain);
  TPassType = (ptClearText, ptCRAMMD5, ptOther);

  TServerOptions = class
  private
    FServerType: TServerType;
    FSocketType: TSocketType;
    FPort: cardinal;
    FHostName: string;
    FUserName: TUserName;
    FAuthentication: TPassType;
  public
    constructor Create(AServerType: TServerType; ASocketType: TSocketType;
      APort: cardinal; AHostName: string; AUserName: TUserName;
      AAuthentication: TPassType);
    destructor Destroy; override;
    property ServerType: TServerType read FServerType;
    property SocketType: TSocketType read FSocketType;
    property Port: cardinal read FPort;
    property HostName: string read FHostName;
    property UserName: TUserName read FUserName;
    property Authentication: TPassType read FAuthentication;
  end;

  TISPDB = class
  private
    FServers: TObjectList<TServerOptions>;
    function GET(const URL: string; AResponse: TStringStream): integer;
    function MX(const AEmail: string; AResponse: TStringList):boolean;
    procedure Parse(AXMLResponse: TStringStream);
  public
    constructor Create;
    destructor Destroy; override;
    function GetDomain(const AEmail: string): string;
    function FindOptions(const AEmail: string): boolean;
    function FindServer(AServerType: TServerType; ASocketType: TSocketType):TServerOptions;
    property Servers: TObjectList<TServerOptions> read FServers;
  end;

implementation

uses StrUtils, System.Types;

const
  cBaseURL = 'https://autoconfig.thunderbird.net/v1.1/';
  cDefaultDNS = '8.8.8.8';

resourcestring
  rsWrongEmailAddress = 'Неверно задан адрес почты. Домен не определен';
  rsEmptyXML = 'В ответе ISPDB получен пустой XML';

  { TServerOptions }

constructor TServerOptions.Create(AServerType: TServerType;
  ASocketType: TSocketType; APort: cardinal; AHostName: string;
  AUserName: TUserName; AAuthentication: TPassType);
begin
  inherited Create;
  FServerType := AServerType;
  FSocketType := ASocketType;
  FPort := APort;
  FHostName := AHostName;
  FUserName := AUserName;
  FAuthentication := AAuthentication;
end;

destructor TServerOptions.Destroy;
begin
  inherited;
end;

{ TISPDB }

constructor TISPDB.Create;
begin
  inherited;
  FServers:=TObjectList<TServerOptions>.Create();
end;

destructor TISPDB.Destroy;
begin
  FServers.Free;
  inherited;
end;

function TISPDB.FindOptions(const AEmail: string): boolean;
var AStream: TStringStream;
    AMX: TStringList;
    HTTPResult: integer;
    Str: TStringDynArray;
    Domain: string;
begin
  FServers.Clear;

  Domain:=GetDomain(AEmail);
  if Domain.IsEmpty then
    raise Exception.Create(rsWrongEmailAddress);

  AStream:=TStringStream.Create;
  try
    HTTPResult:=GET(cBaseURL+Domain,AStream);
    Result:=HTTPResult=200;
    if Result then
      Parse(AStream)
    else
      begin
        AMX:=TStringList.Create;
        try
          if MX(AEmail, AMX) then
            begin
              Str:=SplitString(AMX[0],'.');
              AStream.Clear;
              HTTPResult:=GET(cBaseURL+LowerCase(Str[High(Str)-1]+'.'+Str[High(Str)]),AStream);
              Result:=HTTPResult=200;
              if Result then
                Parse(AStream)
            end;
        finally
          AMX.Free;
        end;
       end;
  finally
    AStream.Free
  end;
end;

function TISPDB.FindServer(AServerType: TServerType;
  ASocketType: TSocketType): TServerOptions;
var
  I: Integer;
begin
  for I := 0 to Pred(FServers.Count) do
    if (FServers[i].ServerType=AServerType) and (FServers[i].SocketType=ASocketType) then
      Exit(FServers[i]);
  Result:=nil;
end;

function TISPDB.GET(const URL: string; AResponse: TStringStream): integer;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    if HTTP.HTTPMethod('GET', URL) then
      AResponse.LoadFromStream(HTTP.Document);
    Result := HTTP.ResultCode;
  finally
    HTTP.Free;
  end;
end;

function TISPDB.GetDomain(const AEmail: string): string;
var
  i: integer;
begin
  Result := EmptyStr;
  i := pos('@', AEmail);
  if i > 0 then
    Result := Trim(copy(AEmail, i + 1, length(AEmail) - i));
end;

function TISPDB.MX(const AEmail: string; AResponse: TStringList): boolean;
begin
  Result:=GetMailServers(cDefaultDNS,GetDomain(AEmail),AResponse);
end;

procedure TISPDB.Parse(AXMLResponse: TStringStream);
const
  cServerNodes: array [0 .. 4] of string = ('hostname', 'port', 'socketType',
    'username', 'authentication');
var
  XMLDoc: IXMLDocument;
  Node, ServerNode: IXMLNode;
  ServerStr: string;
  ServerType: TServerType;
  SocketType: TSocketType;
  Port: cardinal;
  HostName: string;
  UserName: TUserName;
  Authentication: TPassType;
begin
  if not Assigned(AXMLResponse) then
    raise Exception.Create(rsEmptyXML);

  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.LoadFromStream(AXMLResponse);
    Node := XMLDoc.DocumentElement.ChildNodes.FindNode('emailProvider');
    if Assigned(Node) then
      Node := Node.ChildNodes.First;
    while Assigned(Node) do
    begin
      if SameText(Node.NodeName, 'incomingServer') or
        SameText(Node.NodeName, 'outgoingServer') then
      begin
        ServerStr := Node.Attributes['type'];
        // определяем тип сервера
        if SameText(ServerStr, 'imap') then
          ServerType := TServerType.stIMAP
        else if SameText(ServerStr, 'pop3') then
          ServerType := TServerType.stPOP
        else
          ServerType := TServerType.stSMTP;
        // читаем настройки сервера
        ServerNode := Node.ChildNodes.First;
        while Assigned(ServerNode) do
        begin
          case AnsiIndexStr(ServerNode.NodeName, cServerNodes) of
            0: HostName := ServerNode.Text; // hostname
            1: Port := StrToInt(ServerNode.Text); // port
            2: begin // socketType
                if SameText(ServerNode.Text, 'SSL') then
                  SocketType := TSocketType.sSSL
                else if SameText(ServerNode.Text, 'STARTTLS') then
                  SocketType := TSocketType.sSTARTTLS
                else
                  SocketType := TSocketType.sPlain;
              end;
            3:begin // username
                if SameText(ServerNode.Text, '%EMAILADDRESS%') then
                  UserName := unAddress
                else
                  UserName := unLocalPart;
              end;
            4:begin // authentication
                if SameText(ServerNode.Text, 'password-cleartext') then
                  Authentication:=ptClearText
                else
                  if SameText(ServerNode.Text, 'password-encrypted') then
                    Authentication:=ptCRAMMD5
                  else
                    Authentication:=ptOther;
              end;
          end;
          ServerNode := ServerNode.NextSibling;
        end;
        FServers.Add(TServerOptions.Create(ServerType,SocketType,Port,HostName,UserName,Authentication));
      end;
      Node := Node.NextSibling;
    end;
  finally
    XMLDoc := nil;
  end;
end;

end.
