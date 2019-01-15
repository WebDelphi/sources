unit GoogleLogin;

interface

uses WinInet, strutils, SysUtils, Classes;

resourcestring
 rcNone = '�������������� �� ������������� ��� ��������';
 rcOk = '�������������� ������ �������';
 rcBadAuthentication ='�� ������� ���������� ��� ������������ ��� ������, �������������� � ������� �� ����';
 rcNotVerified ='����� ����������� �����, ��������� � ���������, �� ��� �����������';
 rcTermsNotAgreed ='������������ �� ������ ������� ������������� ������';
 rcCaptchaRequired ='��������� ����� �� ���� CAPTCHA';
 rcUnknown ='����������� ������';
 rcAccountDeleted ='������� ����� ������������ ������';
 rcAccountDisabled ='������� ����� ������������ ��������';
 rcServiceDisabled ='������ ������������ � ��������� ������ ��������';
 rcServiceUnavailable ='������ ����������, ��������� ������� �����';

const
  DefoultAppName = 'Noname-MyCompany-1.0';

  Flags_Connection = INTERNET_DEFAULT_HTTPS_PORT;

  Flags_Request = INTERNET_FLAG_RELOAD or
                  INTERNET_FLAG_IGNORE_CERT_CN_INVALID or
                  INTERNET_FLAG_NO_CACHE_WRITE or
                  INTERNET_FLAG_SECURE or
                  INTERNET_FLAG_PRAGMA_NOCACHE or
                  INTERNET_FLAG_KEEP_CONNECTION;

 Errors : array [0..8] of string = ('BadAuthentication','NotVerified',
  'TermsNotAgreed','CaptchaRequired','Unknown','AccountDeleted','AccountDisabled',
  'ServiceDisabled','ServiceUnavailable');

type
  TServices = (tsNone,tsAnalytics,tsApps,tsGBase,tsSites,tsBlogger,tsBookSearch,
               tsCelendar,tcCodeSearch,tsContacts,tsDocLists,tsFinance,
               tsGMailFeed,tsHealth,tsMaps,tsPicasa,tsSidewiki,tsSpreadsheets,
               tsWebmaster,tsYouTube);

const
  ServiceIDs: array[0..19]of string=('xapi','analytics','apps','gbase',
  'jotspot','blogger','print','cl','codesearch','cp','writely','finance',
  'mail','health','local','lh2','annotateweb','wise','sitemaps','youtube');

type
  TAccountType = (atNone ,atGOOGLE, atHOSTED, atHOSTED_OR_GOOGLE);

type
  TLoginResult = (lrNone,lrOk, lrBadAuthentication, lrNotVerified,
                  lrTermsNotAgreed, lrCaptchaRequired, lrUnknown,
                  lrAccountDeleted, lrAccountDisabled, lrServiceDisabled,
                  lrServiceUnavailable);

type
  TGoogleLogin = class
  private
    //��������������� ������
    FAccountType  : TAccountType;
    FLastResult   : TLoginResult;
    FEmail        : string;
    FPassword     : string;
    //������ ������/�������
    FSID          : string;//� ��������� ����� �� ������������
    FLSID         : string;//� ��������� ����� �� ������������
    FAuth         : string;
    FService      : TServices;//������ � �������� ���������� �������� ������
    FSource       : string;//��� ����������� ����������
    FLogintoken   : string;
    FLogincaptcha : string;
    //��������� Captcha
    FCaptchaURL    : string;
    function SendRequest(const ParamStr: string):AnsiString;
    function ExpertLoginResult(const LoginResult:string):TLoginResult;
    function GetLoginError(const str: string):TLoginResult;
    function GetCaptchaToken(const cList:TStringList):String;
    function GetCaptchaURL(const cList:TStringList):string;
    function GetResultText:string;
    procedure SetEmail(cEmail:string);
    procedure SetPassword(cPassword:string);
    procedure SetService(cService:TServices);
    procedure SetSource(cSource: string);
    procedure SetCaptcha(cCaptcha:string);
  public
    constructor Create(const aEmail, aPassword: string);
    function Login(aLoginToken:string='';aLoginCaptcha:string=''):TLoginResult;
    procedure Disconnect;//������� ��� ������ �� �����������
    property AccountType: TAccountType read FAccountType write FAccountType;
    property LastResult: TLoginResult read FLastResult;
    property LastResultText:string read GetResultText;
    property Email: string read FEmail write SetEmail;
    property Password:string read FPassword write SetPassword;
    property Service: TServices read FService write SetService;
    property Source: string read FSource write FSource;
    property Auth: string read FAuth;
    property SID: string read FSID;
    property LSID: string read FLSID;
    property CaptchaURL: string read FCaptchaURL;
    property LoginToken: string read FLogintoken;
    property LoginCaptcha: string read FLogincaptcha write FLogincaptcha;
end;


implementation

{ TGoogleLogin }

procedure TGoogleLogin.Disconnect;
begin
 FAccountType:=atNone;
 FLastResult:=lrNone;
 FSID:='';
 FLSID:='';
 FAuth:='';
 FLogintoken:='';
 FLogincaptcha:='';
 FCaptchaURL:='';
 FLogintoken:='';
end;

constructor TGoogleLogin.Create(const aEmail, aPassword: string);
begin
inherited Create;
  if (Length(Trim(aEmail))>0)and(Length(Trim(aPassword))>0) then
    begin
      FEmail:=aEmail;
      FPassword:=aPassword;
    end
  else
    Destroy;
end;

function TGoogleLogin.ExpertLoginResult(const LoginResult: string): TLoginResult;
var List: TStringList;
    i:integer;
begin
//������ ����� ������� � ������
  List:=TStringList.Create;
  List.Text:=LoginResult;
//����������� ���������
if pos('error',LowerCase(LoginResult))>0 then //���� ��������� �� ������
  begin
    for i:=0 to List.Count-1 do
      begin
        if pos('error',LowerCase(List[i]))>0 then //������ � �������
          begin
            Result:=GetLoginError(List[i]);//�������� ��� ������
            break;
          end;
      end;
      if Result=lrCaptchaRequired then //��������� ���� ������
        begin
          FCaptchaURL:=GetCaptchaURL(List);
          FLogintoken:=GetCaptchaToken(List);
        end;
  end
else
  begin
    Result:=lrOk;
    for i:=0 to List.Count-1 do
      begin
        if pos('SID',UpperCase(List[i]))>0 then
          FSID:=Trim(copy(List[i],pos('=',List[i])+1,Length(List[i])-pos('=',List[i])))
        else
          if pos('LSID',UpperCase(List[i]))>0 then
            FLSID:=Trim(copy(List[i],pos('=',List[i])+1,Length(List[i])-pos('=',List[i])))
          else
            if pos('AUTH',UpperCase(List[i]))>0 then
              FAuth:=Trim(copy(List[i],pos('=',List[i])+1,Length(List[i])-pos('=',List[i])));
      end;
  end;
FreeAndNil(List);
end;

function TGoogleLogin.GetCaptchaToken(const cList: TStringList): String;
var i:integer;
begin
  for I := 0 to cList.Count - 1 do
    begin
      if pos('captchatoken',lowerCase(cList[i]))>0 then
        begin
          Result:=Trim(copy(cList[i],pos('=',cList[i])+1,Length(cList[i])-pos('=',cList[i])));
          break;
        end;
    end;
end;

function TGoogleLogin.GetCaptchaURL(const cList: TStringList): string;
var i:integer;
begin
  for I := 0 to cList.Count - 1 do
    begin
      if pos('captchaurl',lowerCase(cList[i]))>0 then
        begin
          Result:=Trim(copy(cList[i],pos('=',cList[i])+1,Length(cList[i])-pos('=',cList[i])));
          break;
        end;
    end;
end;

function TGoogleLogin.GetLoginError(const str: string): TLoginResult;
var ErrorText:string;
begin
//�������� ����� ������
  ErrorText:=Trim(copy(str,pos('=',str)+1,Length(str)-pos('=',str)));
 Result:=TLoginResult(AnsiIndexStr(ErrorText,Errors)+2);
end;

function TGoogleLogin.GetResultText: string;
begin
 case FLastResult of
   lrNone: Result:=rcNone;
   lrOk: Result:=rcOk;
   lrBadAuthentication: Result:=rcBadAuthentication;
   lrNotVerified: Result:=rcNotVerified;
   lrTermsNotAgreed: Result:=rcTermsNotAgreed;
   lrCaptchaRequired: Result:=rcCaptchaRequired;
   lrUnknown: Result:=rcUnknown;
   lrAccountDeleted: Result:=rcAccountDeleted;
   lrAccountDisabled: Result:=rcAccountDisabled;
   lrServiceDisabled: Result:=rcServiceDisabled;
   lrServiceUnavailable: Result:=rcServiceUnavailable;
 end;
end;

function TGoogleLogin.Login(aLoginToken, aLoginCaptcha: string): TLoginResult;
var cBody: TStringStream;
    ResponseText: string;
begin
 //��������� ������
 cBody:=TStringStream.Create('');
 case FAccountType of
   atNone,atHOSTED_OR_GOOGLE:cBody.WriteString('accountType=HOSTED_OR_GOOGLE&');
   atGOOGLE:cBody.WriteString('accountType=GOOGLE&');
   atHOSTED:cBody.WriteString('accountType=HOSTED&');
 end;
 cBody.WriteString('Email='+FEmail+'&');
 cBody.WriteString('Passwd='+FPassword+'&');
 cBody.WriteString('service='+ServiceIDs[ord(FService)]+'&');
 if Length(Trim(FSource))>0 then
   cBody.WriteString('source='+FSource)
 else
   cBody.WriteString('source='+DefoultAppName);
 if Length(Trim(aLoginToken))>0 then
   begin
     cBody.WriteString('&logintoken='+aLoginToken);
     cBody.WriteString('&logincaptcha='+aLoginCaptcha);
   end;
//���������� ������ �� ������
ResponseText:=SendRequest(cBody.DataString);
//���������������� ��������� � ��������� ����������� ����
Result:=ExpertLoginResult(ResponseText);
FLastResult:=Result;
end;

function TGoogleLogin.SendRequest(const ParamStr: string): AnsiString;
  function DataAvailable(hRequest: pointer; out Size : cardinal): boolean;
  begin
    result := wininet.InternetQueryDataAvailable(hRequest, Size, 0, 0);
  end;
var hInternet,hConnect,hRequest : Pointer;
    dwBytesRead,I,L : Cardinal;
begin
try
hInternet := InternetOpen(PChar('GoogleLogin'),INTERNET_OPEN_TYPE_PRECONFIG,Nil,Nil,0);
 if Assigned(hInternet) then
    begin
      //��������� ������
      hConnect := InternetConnect(hInternet,PChar('www.google.com'),Flags_connection,nil,nil,INTERNET_SERVICE_HTTP,0,1);
      if Assigned(hConnect) then
        begin
          //��������� ������
          hRequest := HttpOpenRequest(hConnect,PChar(uppercase('post')),PChar('accounts/ClientLogin?'+ParamStr),HTTP_VERSION,nil,Nil,Flags_Request,1);
          if Assigned(hRequest) then
            begin
              //���������� ������
              I := 1;
              if HttpSendRequest(hRequest,nil,0,nil,0) then
                begin
                  repeat
                  DataAvailable(hRequest, L);//�������� ���-�� ����������� ������
                  if L = 0 then break;
                  SetLength(Result,L + I);
                  if InternetReadFile(hRequest,@Result[I],sizeof(L),dwBytesRead) then//�������� ������ � �������
                  else break;
                  inc(I,dwBytesRead);
                  until dwBytesRead = 0;
                  Result[I] := #0;
                end;
            end;
            InternetCloseHandle(hRequest);
        end;
        InternetCloseHandle(hConnect);
    end;
    InternetCloseHandle(hInternet);
except
  InternetCloseHandle(hRequest);
  InternetCloseHandle(hConnect);
  InternetCloseHandle(hInternet);
end;
end;

procedure TGoogleLogin.SetCaptcha(cCaptcha: string);
begin
  FLogincaptcha:=cCaptcha;
  Login(FLogintoken,FLogincaptcha);//���������������� � �������
end;

procedure TGoogleLogin.SetEmail(cEmail: string);
begin
  FEmail:=cEmail;
  Disconnect;//�������� ����������
end;

procedure TGoogleLogin.SetPassword(cPassword: string);
begin
  FPassword:=cPassword;
  Disconnect;//�������� ����������
end;

procedure TGoogleLogin.SetService(cService: TServices);
begin
  FService:=cService;
  Disconnect;//�������� ����������
  Login();    //����������������
end;

procedure TGoogleLogin.SetSource(cSource: string);
begin
FSource:=cSource;
Disconnect;//�������� ����������
end;

end.
