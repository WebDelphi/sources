unit BitlyAPI;

interface

uses NativeXML, SysUtils, Classes, httpsend, TypInfo ,dialogs;

resourcestring
  rcValidateError = 'BIT.LY: Недействительное значение API-key или Login';
  rcQueryError = 'Ошибка выполнения запроса';
  rcOutputFormatError = 'Формат %s не поддерживается в запросе %s';

const
  APIURL = 'http://api.bit.ly/v3/%s?login=%s&apiKey=%s&%s';
  http = 'http://';

type
  TBitlyDomains = (tdBit, tdJmp);
  TBitlyFormat = (xml,json,txt);

type
  TBitly = class
  private
    FLogin: string;
    FApiKey: string;
    function NormalizeURL(const aURL: string):string;
    function GetParamString(const Params:TStringList):string;
    function GetFormat(aFormat:TBitlyFormat):string;
    function GETQuery(const URL:string):string;
  public
    constructor Create(const Login, APIKey: string);
    function Shorten(const LongURL:string; Domain:TBitlyDomains;OutputFormat:TBitlyFormat=xml):string;
    function Validate(const Login, APIKey: string; OutputFormat:TBitlyFormat=xml):string;
    function Expand(const URLs,Hashes:TStringList;OutputFormat:TBitlyFormat=xml):string;
    function Clicks(const URLs,Hashes:TStringList;OutputFormat:TBitlyFormat=xml):string;
    function BitlyProDomain(const domain:string;OutputFormat:TBitlyFormat=xml):string;
    property Login: string read FLogin write FLogin;
    property ApiKey: string read FApiKey write FApiKey;
end;

implementation

{ TBitly }

function URLEncode(const WStr:String):string;
var UStr:UTF8String;
    i:integer;
begin
UStr:=UTF8Encode(WStr);
result:='';
if length(UStr)>0 then
for i:=1 to length(UStr) do
result:=result+'%'+IntToHex(ord(UStr[i]),2);
end;

function TBitly.BitlyProDomain(const domain: string;
  OutputFormat: TBitlyFormat): string;
var params:TStringList;
begin
  if OutputFormat=txt then
    raise Exception.Create(Format(rcOutputFormatError,['txt','bitly_pro_domain']));
try
  params:=TStringList.Create;
  params.Add('domain='+StringReplace(domain, http,'',[rfReplaceAll]));
  params.Add(GetFormat(OutputFormat));
  Result:=GETQuery(Format(APIURL,['bitly_pro_domain',FLogin,FAPIKey,GetParamString(Params)]));
finally
  FreeAndNil(params);
end;
end;

function TBitly.Clicks(const URLs, Hashes: TStringList;
  OutputFormat: TBitlyFormat): string;
var i:integer;
    params: TStringList;
begin
  if OutputFormat=txt then
    raise Exception.Create(Format(rcOutputFormatError,['txt','clicks']));
try
  params:=TStringList.Create;
  if URLs.Count>0 then
    for I:=0 to URLs.Count - 1 do
       params.Add('shortUrl='+NormalizeURL(URLs[i]));
  if Hashes.Count>0 then
    for I:=0 to Hashes.Count - 1 do
       params.Add('hash='+Hashes[i]);
  params.Add(GetFormat(OutputFormat));
  Result:=GETQuery(Format(APIURL,['clicks',FLogin,FAPIKey,GetParamString(Params)]));
finally
  FreeAndNil(params);
end;
end;

constructor TBitly.Create(const Login, APIKey: string);
var params: TStringList;
begin
  inherited Create;
try
  params:=TStringList.Create;
  params.Add(Validate(Login,APIKey,txt));
  if (params.Count>0)and(params[0]='1') then
    begin
      FLogin:=Login;
      FApiKey:=APIKey;
    end
  else
    raise Exception.Create(rcValidateError);
finally
  FreeAndNil(params)
end;
end;

function TBitly.Expand(const URLs, Hashes: TStringList;
  OutputFormat: TBitlyFormat): string;
var i:integer;
    params: TStringList;
begin
try
  params:=TStringList.Create;
  if URLs.Count>0 then
    for I:=0 to URLs.Count - 1 do
      params.Add('shortUrl='+NormalizeURL(URLs[i]));
  if Hashes.Count>0 then
    for I:=0 to Hashes.Count - 1 do
       params.Add('hash='+Hashes[i]);
  params.Add(GetFormat(OutputFormat));
  Result:=GETQuery(Format(APIURL,['expand',FLogin,FAPIKey,GetParamString(Params)]));
finally
  FreeAndNil(params);
end;
end;

function TBitly.GetFormat(aFormat: TBitlyFormat): string;
begin
  Result :='format='+GetEnumName(TypeInfo(TBitlyFormat),  Ord(aFormat));
end;

function TBitly.GetParamString(const Params: TStringList): string;
begin
  Params.Delimiter:='&';
  Result:=Params.DelimitedText;
end;

function TBitly.GETQuery(const URL: string): string;
var Res:TStringList;
begin
try
  Res:=TStringList.Create;
  with THTTPSend.Create do
    begin
      if HTTPMethod('GET',URL) then
        begin
          Res.LoadFromStream(Document);
          Result:=Trim(Res.Text);
        end
      else
        MessageDlg(rcQueryError,mtError,[mbOK],0);
    end;
finally
  FreeAndNil(Res);
end;
end;

function TBitly.NormalizeURL(const aURL: string): string;
begin
  if pos(http,aURL)<=0 then
    Result:=http+aURL
  else
    Result:=aURL;
  Result:=StringReplace(Result,'?','/?',[rfReplaceAll,rfIgnoreCase]);
  Result:=URLEncode(Result);
end;

function TBitly.Shorten(const LongURL: string; Domain: TBitlyDomains;OutputFormat:TBitlyFormat): string;
var Params:TStringList;
begin
try
  Params:=TStringList.Create;
  Params.Add('uri='+NormalizeURL(LongURL));
  Params.Add(GetFormat(OutputFormat));
  case Domain of
    tdBit:Params.Add('domain=bit.ly');
    tdJmp:Params.Add('domain=j.mp');
  end;
  Result:=GETQuery(Format(APIURL,['shorten',FLogin,FAPIKey,GetParamString(Params)]));
finally
  FreeAndNil(Params);
end;
end;

function TBitly.Validate(const Login, APIKey: string;
  OutputFormat: TBitlyFormat): string;
var params:TStringList;
begin
try
  params:=TStringList.Create;
  params.Add('x_login='+Login);
  params.Add('x_apiKey='+APIKey);
  params.Add(GetFormat(OutputFormat));
  Result:=GETQuery(Format(APIURL,['validate',Login,APIKey,GetParamString(Params)]));
finally
  FreeAndNil(params);
end;
end;

end.
