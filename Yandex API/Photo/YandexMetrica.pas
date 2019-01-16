unit YandexMetrica;

interface

uses Classes, SysUtils, httpsend, ssl_openssl, RegularExpressions, Dialogs,
  TypInfo, ShellAPI;

resourcestring
  rsErrClientID = 'Для работы с API необходимо указать ID клиента!';

const
  { id клиента получается при регистрации на https://oauth.yandex.ru/client/new }
  cClientId = '';
  { строка для запроса авторизации }
  cParams = 'grant_type=password&client_id=%s&username=%s&password=%s';
  { адрес для получения токена }
  cAuthURL = 'https://oauth.yandex.ru/token';
  { URL для доступа к методам АПИ }
  cApiURL = 'http://api-metrika.yandex.ru/%s.%s?oauth_token=%s';
  cAuthURL2 = 'https://oauth.yandex.ru/authorize?response_type=token&client_id=%s';

type
  TAuthorizer = class
  private
    FAuthorized: boolean;
    FLogin: string;
    FPassword: string;
    FClientId: string;
    FOAuthToken: string;
    FServerResponse: string;
    FClientSecret: string;
    procedure SetLogin(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetClientSecret(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Authorize;
    property Authorized: boolean read FAuthorized;
    property OAuthToken: string read FOAuthToken;
    property ClientID: string read FClientId write FClientId;
    property ClientSecret: string read FClientSecret write SetClientSecret;
    property Login: string read FLogin write SetLogin;
    property Password: string read FPassword write SetPassword;
    property ServerResponse: string read FServerResponse;
  end;

type
  TOutputFormat = (tfXML, tfJSON);
  TCounterPermission = (no_premission, own, view, edit);
  TCounterType = (no_type, simple, partner);

type
  TYandexMetrica = class
  private
    FAuthorizer: TAuthorizer;
    FOutputFormat: TOutputFormat;
    function GETCommand(Method: string; Params: TStringList): string;
    procedure SetOutputFormat(const Value: TOutputFormat);
  public
    constructor Create;
    destructor Destroy; override;
    {Возвращает список существующих счетчиков, доступных пользователю.}
    function GetCounters(Params: TStringList): string; overload;
    function GetCounters(CounterType: TCounterType;
      CounterPremission: TCounterPermission; ulogin:string=''; field: string='')
      : string; overload;
    {Возвращает информацию об указанном счетчике}
    function GetCounter(id: integer; field:string=''): string;
    property Authorizer: TAuthorizer read FAuthorizer write FAuthorizer;
    property OutputFormat: TOutputFormat read FOutputFormat
      write SetOutputFormat;

  end;

implementation

{ TAuthorizer }

procedure TAuthorizer.Authorize;
//var
//  Params: TStringStream;
begin

//https://oauth.yandex.ru/authorize?response_type=token&client_id=<client_id>
//  if Length(FClientId)=0 then
//    raise Exception.Create(rsErrClientID);
//  Params := TStringStream.Create(Format(cParams, [FClientId, FLogin,
//    FPassword]));
//  try
//    //HTTP.ResponseCode
//    with THTTPSend.Create do
//    begin
//      Document.LoadFromStream(Params);
//      if HTTPMethod('POST', cAuthURL) then
//      begin
//        FAuthorized := (ResultCode = 200);
//        if FAuthorized then
//        begin
//          Params.LoadFromStream(Document);
//          FOAuthToken := TRegEx.Match(Params.DataString,
//            '"access_token": "(.*)"').Groups.Item[1].Value;
//        end
//      end
//      else
//        FAuthorized := false;
//    end
//  finally
//    Params.Free;
//  end;
end;

constructor TAuthorizer.Create;
begin
  inherited Create;
  FClientId := cClientId;
end;

destructor TAuthorizer.Destroy;
begin

  inherited Destroy;
end;

procedure TAuthorizer.SetClientSecret(const Value: string);
begin
  FClientSecret := Value;
end;

procedure TAuthorizer.SetLogin(const Value: string);
begin
  FLogin := Value;
end;

procedure TAuthorizer.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

{ TYandexMetrica }

constructor TYandexMetrica.Create;
begin
  inherited Create;
  FAuthorizer := TAuthorizer.Create;
end;

destructor TYandexMetrica.Destroy;
begin
  FAuthorizer.Free;
  inherited Destroy;
end;

function TYandexMetrica.GETCommand(Method: string; Params: TStringList): string;
var
  URL, ParamsStr, outFormat: string;
  i: integer;
  Data: TStringStream;
begin
  if not FAuthorizer.Authorized then
    Exit;

  if Params <> nil then
  begin
    Params.Delimiter := '&';
    ParamsStr := Params.DelimitedText;
  end;
  outFormat := GetEnumName(TypeInfo(TOutputFormat), ord(OutputFormat));
  outFormat := Copy(outFormat, 3, Length(outFormat) - 2);
  URL := Format(cApiURL, [Method, LowerCase(outFormat),
    FAuthorizer.OAuthToken]);
  if Length(ParamsStr) > 0 then
    URL := URL + '&' + ParamsStr;

  Data := TStringStream.Create;
  try
    with THTTPSend.Create do
    begin
      if HTTPMethod('GET', URL) then
      begin
        Data.LoadFromStream(Document);
        Result := Data.DataString;
      end
      else
        raise Exception.Create('Ошибка отправки запроса');
    end;
  finally
    Data.Free;
  end;
end;

function TYandexMetrica.GetCounter(id:integer; field: string): string;
var Method: string;
    Params: TStringList;
begin
  Method:=Format('counter/%d',[id]);
  Params:=TStringList.Create;
  try
    if Length(field)>0 then
      Params.Add('field='+field);
    Result:=GETCommand(Method,Params);
  finally
    Params.Free;
  end;
end;

function TYandexMetrica.GetCounters(CounterType: TCounterType;
  CounterPremission: TCounterPermission; ulogin, field: string): string;
var
  Params: TStringList;
begin
  Params := TStringList.Create;
  try
    if CounterPremission <> no_premission then
      Params.Add('permission=' + GetEnumName(TypeInfo(TCounterPermission),
        ord(CounterPremission)));
    if CounterType<>no_type then
      Params.Add('type=' + GetEnumName(TypeInfo(TCounterType),
        ord(CounterType)));
    if Length(ulogin)>0 then
      Params.Add('ulogin='+ulogin);
    if Length(field)>0 then
      Params.Add('field='+field);
     Result:=GetCounters(Params);
  finally
    Params.Free;
  end;
end;

function TYandexMetrica.GetCounters(Params: TStringList): string;
begin
  Result := GETCommand('counters', Params);
end;

procedure TYandexMetrica.SetOutputFormat(const Value: TOutputFormat);
begin
  FOutputFormat := Value;
end;

end.
