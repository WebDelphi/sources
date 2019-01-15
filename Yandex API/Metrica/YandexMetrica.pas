unit YandexMetrica;

interface

uses Classes, SysUtils, httpsend, ssl_openssl, RegularExpressions, Dialogs,
  TypInfo;

resourcestring
  rsErrClientID = 'Для работы с API необходимо указать ID клиента!';

const
  { id клиента получается при регистрации на https://oauth.yandex.ru/client/new }
  cClientId = 'bc22b46b274246e6980c2e7a12119283';
  { строка для запроса авторизации }
  cParams = 'grant_type=password&client_id=%s&username=%s&password=%s';
  { адрес для получения токена }
  cAuthURL = 'https://oauth.yandex.ru/token';
  { URL для доступа к методам АПИ }
  cApiURL = 'http://api-metrika.yandex.ru/%s.%s?oauth_token=%s';

type
  TAuthorizer = class
  private
    FAuthorized: boolean;
    FLogin: string;
    FPassword: string;
    FClientId: string;
    FOAuthToken: string;
    FServerResponse: string;
    procedure SetLogin(const Value: string);
    procedure SetPassword(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Authorize;
    property Authorized: boolean read FAuthorized;
    property OAuthToken: string read FOAuthToken;
    property ClientID: string read FClientId write FClientId;
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
    function POSTCommand(Method:string; Params: TStringList):string;overload;
    procedure SetOutputFormat(const Value: TOutputFormat);
    function GetMimeType: string;
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

    {Создает счетчик с заданными параметрами}
    function CreateCounter(SiteName: string):string;

    {Возвращает информацию о целях счетчика.}
    function GetGoals(id: integer):string;
    {возвращает информацию по цели}
    function GetGoalInfo(id,goal_id:integer):string;
    {Возвращает информацию о фильтрах счетчика.}
    function GetFilters(id: integer):string;
    {Возвращает информацию об указанном фильтре счетчика.}
    function GetFilterInfo(id, filter_id:integer):string;
    {Возвращает информацию об операциях счетчика.}
    function GetOperations(id: integer):string;
    {Возвращает информацию об указанной операции счетчика.}
    function GetOperationInfo(id, operation_id:integer):string;
    {Возвращает информацию о разрешениях на управление счетчиком и просмотр статистики.}
    function GetGrants(id: integer):string;
    {Возвращает информацию об указанном разрешении на управление счетчиком и просмотр статистики.}
    function GetGrantInfo(id:integer; user_login:string):string;
    {Возвращает список представителей, которым предоставлен полный доступ к аккаунту текущего пользователя}
    function GetDelegates():string;
    {Возвращает список аккаунтов, представителем которых является текущий пользователь.}
    function GetAccounts():string;
    property Authorizer: TAuthorizer read FAuthorizer write FAuthorizer;
    property OutputFormat: TOutputFormat read FOutputFormat write SetOutputFormat;

  end;

implementation

{ TAuthorizer }

procedure TAuthorizer.Authorize;
var
  Params: TStringStream;
begin
  if Length(FClientId)=0 then
    raise Exception.Create(rsErrClientID);
  Params := TStringStream.Create(Format(cParams, [FClientId, FLogin,
    FPassword]));
  try
    //HTTP.ResponseCode
    with THTTPSend.Create do
    begin
      Document.LoadFromStream(Params);
      if HTTPMethod('POST', cAuthURL) then
      begin
        FAuthorized := (ResultCode = 200);
        if FAuthorized then
        begin
          Params.LoadFromStream(Document);
          FOAuthToken := TRegEx.Match(Params.DataString,
            '"access_token": "(.*)"').Groups.Item[1].Value;
        end
      end
      else
        FAuthorized := false;
    end
  finally
    Params.Free;
  end;
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

function TYandexMetrica.CreateCounter(SiteName: string): string;
var Params: TStringList;
begin
  Params:=TStringList.Create;
  try
    Params.Add(SiteName);
    Result:=POSTCommand('counters',Params);
  finally
    Params.Free;
  end;
end;

destructor TYandexMetrica.Destroy;
begin
  FAuthorizer.Free;
  inherited Destroy;
end;

function TYandexMetrica.GetAccounts: string;
begin
  Result:=GETCommand('accounts',nil)
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

function TYandexMetrica.GetDelegates: string;
begin
  Result:=GETCommand('delegates',nil)
end;

function TYandexMetrica.GetFilterInfo(id, filter_id: integer): string;
begin
  Result:=GETCommand(Format('counter/%d/filter/%d',[id, filter_id]),nil)
end;

function TYandexMetrica.GetFilters(id: integer): string;
begin
  Result:=GETCommand(Format('counter/%d/filters',[id]),nil)
end;

function TYandexMetrica.GetGoalInfo(id, goal_id: integer): string;
begin
  Result:=GETCommand(Format('counter/%d/goal/%d',[id, goal_id]),nil)
end;

function TYandexMetrica.GetGoals(id: integer): string;
begin
  Result:=GETCommand(Format('counter/%d/goals',[id]),nil)
end;

function TYandexMetrica.GetGrantInfo(id: integer; user_login: string): string;
begin
  Result:=GETCommand(Format('counter/%d/operation/%s',[id, user_login]),nil)
end;

function TYandexMetrica.GetGrants(id: integer): string;
begin
  Result:=GETCommand(Format('counter/%d/grants',[id]),nil)
end;

function TYandexMetrica.GetMimeType: string;
begin
  case OutputFormat of
    tfXML:Result:='application/x-yametrika+xml';
    tfJSON:Result:='application/x-yametrika+json';
  end;
end;

function TYandexMetrica.GetOperationInfo(id, operation_id: integer): string;
begin
   Result:=GETCommand(Format('counter/%d/operation/%d',[id, operation_id]),nil)
end;

function TYandexMetrica.GetOperations(id: integer): string;
begin
  Result:=GETCommand(Format('counter/%d/operations',[id]),nil)
end;

function TYandexMetrica.POSTCommand(Method: string;
  Params: TStringList): string;
var
  URL, ParamsStr, outFormat: string;
  i: integer;
  Data: TStringStream;
begin
  if not FAuthorizer.Authorized then
    Exit;
  outFormat :=GetEnumName(TypeInfo(TOutputFormat), ord(OutputFormat));
  outFormat := Copy(outFormat, 3, Length(outFormat) - 2);
  URL := Format(cApiURL, [Method, LowerCase(outFormat),FAuthorizer.OAuthToken]);
  Data := TStringStream.Create;
  try
    with THTTPSend.Create do
    begin
      Params.SaveToStream(Document);
      MimeType:=GetMimeType;
      Headers.Add('Accept : '+GetMimeType);
      if HTTPMethod('POST', URL) then
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

function TYandexMetrica.GetCounters(Params: TStringList): string;
begin
  Result := GETCommand('counters', Params);
end;

procedure TYandexMetrica.SetOutputFormat(const Value: TOutputFormat);
begin
  FOutputFormat := Value;
end;

end.
