unit GoogleOAuth;

interface

uses SysUtils, Classes, Character, idHTTP, IdSSLOpenSSL, IdException, idUri;

resourcestring

  ///	<summary>
  ///	  Новое описание
  ///	</summary>
  rsRequestError = 'Ошибка выполнения запроса: %d - %s';

  ///	<summary>
  ///	  ещё одно описание
  ///	</summary>
  rsUnknownError = 'Неизвестная ошибка';

const
  /// <summary>
  ///   MIME-тип тела запроса, используемый по умолчанию для работы с мета-данными
  /// </summary>
  DefaultMime = 'application/json; charset=UTF-8';

type
  /// <summary>
  ///   Компонент для авторизации в сервисах Google по протоколу OAuth и
  ///  выполнения основных HTTP-запросов: GET, POST, PUT и DELETE к ресурсам API
  /// </summary>
  TOAuth = class(TComponent)
  private
    type
      TMethodType = (tmGET, tmPOST, tmPUT, tmDELETE);
  private
    FHTTP: TIdHTTP;
    FSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    FClientID: string;
    FClientSecret: string;
    FScope: string;
    FResponseCode: string;
    FAccess_token: string;
    FExpires_in: string;
    FRefresh_token: string;
    function  HTTPMethod(AURL: string; AMethod: TMethodType; AParams: TStrings; ABody: TStream;
      AMime: string = DefaultMime):string;
    procedure SetClientID(const Value: string);
    procedure SetResponseCode(const Value: string);
    procedure SetScope(const Value: string);
    function ParamValue(ParamName, JSONString: string): string;
    procedure SetClientSecret(Value: string);
    function PrepareParams(Params: TStrings): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    /// <summary>
    ///   Формирует URL для получения ResponseCode. Полученный URL необходимо
    ///  предоставить пользователю либо использовать в TWebBrowser для перехода
    ///  на URL с кодом.
    /// </summary>
    /// <returns>
    ///  <param name="String">
    ///     Строка URL для получения кода доступа.
    ///  </param>
    /// </returns>
    function AccessURL: string;
    /// <summary>
    ///   Выполнение запроса на получения ключа доступа к ресурсам API.
    /// </summary>
    /// <returns>
    ///  <param name="String">
    ///     Строка, содержащая ключ доступа к ресурсам API.
    ///Это значение необходимо использовать в заголовке <para>Authorization: OAuth</para>
    /// всех HTTP-запросов
    ///  </param>
    /// </returns>
    function GetAccessToken: string;
    /// <summary>
    ///   Выполнение запроса на обновление ключа доступа к ресурсам API.
    /// </summary>
    /// <returns>
    ///   <param name="String">
    ///     Строка, содержащая ключ доступа к ресурсам API.
    ///Это значение необходимо использовать в заголовке <para>Authorization: OAuth</para>
    /// всех HTTP-запросов
    ///  </param>
    ///</returns>
    function RefreshToken: string;
    /// <summary>
    ///   Выполнение GET-запроса к ресурсу API.
    /// </summary>
    /// <returns>
    ///   <param name="RawBytestring">
    ///     Строка, результат выполнения запроса.
    ///   </param>
    /// </returns>    /// <param name="URL">
    ///  String. URL ресурса
    /// </param>
    /// <param name="Params">
    ///  TStrings. Параметры запроса.
    /// </param>
    function GETCommand(URL: string; Params: TStrings): RawBytestring;
    /// <summary>
    ///   Выполнение POST-запроса к ресурсу API.
    /// </summary>
    /// <returns>
    ///   <param name="RawBytestring">
    ///     Строка, результат выполнения запроса.
    ///   </param>
    /// </returns>
    /// <param name="URL">
    ///  String. URL ресурса
    /// </param>
    /// <param name="Params">
    ///  TStrings. Параметры запроса.
    /// </param>
    /// <param name="Body">
    ///  TStream. Поток, содержащий тело запроса.
    /// </param>
    /// <param name="Mime">
    ///  String. MIME-тип тела запроса. Если отправляется строка в формате JSON,
    ///  то этот параметр можно не указывать
    /// </param>
    function POSTCommand(URL: string; Params: TStrings; Body: TStream;
      Mime: string = DefaultMime): RawBytestring;
    /// <summary>
    ///   Выполнение PUT-запроса к ресурсу API.
    /// </summary>
    /// <returns>
    ///   <param name="RawBytestring">
    ///     Строка, результат выполнения запроса.
    ///   </param>
    /// </returns>
    /// <param name="URL">
    ///  String. URL ресурса
    /// </param>
    /// <param name="Body">
    ///  TStream. Поток, содержащий тело запроса.
    /// </param>
    /// <param name="Mime">
    ///  String. MIME-тип тела запроса. Если отправляется строка в формате JSON,
    ///  то этот параметр можно не указывать
    /// </param>
    function PUTCommand(URL: string; Body: TStream; Mime: string = DefaultMime)
      : RawBytestring;
    /// <summary>
    ///   Выполнение DELETE-запроса к ресурсу API.
    /// </summary>
    /// <param name="URL">
    ///  String. URL ресурса
    /// </param>
    procedure DELETECommand(URL: string);
    /// <summary>
    ///   Ключ доступа к ресусам API (токен). Свойство только для чтения.
    /// </summary>
    property Access_token: string read FAccess_token;
    /// <summary>
    ///   Время в секундах после которого необходимо провести обновление токена.
    /// </summary>
    property Expires_in: string read FExpires_in;
    /// <summary>
    ///   Ключ для обновления основного токена.
    /// </summary>
    property Refresh_token: string read FRefresh_token;
    /// <summary>
    ///   Код доступа, полученный пользователем при переходе по URL, полученному
    /// с помощью метода AccessURL.
    /// </summary>
    property ResponseCode: string read FResponseCode write SetResponseCode;
  published
    /// <summary>
    ///   Идентификатор клиента API. Это значение необходимо заранее сгенерировать,
    /// используя консоль Google API (https://code.google.com/apis/console)
    /// </summary>
    property ClientID: string read FClientID write SetClientID;
    /// <summary>
    ///   Точка доступа к API. Значение этого параметра зависит от того API, к которому
    ///  необходимо получить доступ. Точку доступа можно узнать из официальной документации к API
    /// </summary>
    property Scope: string read FScope write SetScope;
    /// <summary>
    ///   Секретный ключ клиента. Это значение необходимо заранее сгенерировать,
    /// используя консоль Google API (https://code.google.com/apis/console)
    /// </summary>
    property ClientSecret: string read FClientSecret write SetClientSecret;
  end;

procedure register;

implementation

procedure register;
begin
  RegisterComponents('WebDelphi.ru',[TOAuth]);
end;

const
  cConnectionTimeout = 15000;
  cReadTimeout = 20000;
  redirect_uri = 'urn:ietf:wg:oauth:2.0:oob';
  oauth_url =
    'https://accounts.google.com/o/oauth2/auth?client_id=%s&redirect_uri=%s&scope=%s&response_type=code';
  tokenurl = 'https://accounts.google.com/o/oauth2/token';
  tokenparams =
    'client_id=%s&client_secret=%s&code=%s&redirect_uri=%s&grant_type=authorization_code';
  crefreshtoken =
    'client_id=%s&client_secret=%s&refresh_token=%s&grant_type=refresh_token';
  AuthHeader = 'Authorization: Bearer %s';
  StripChars: set of char = ['"', ':', ','];

{ TOAuth }

function TOAuth.AccessURL: string;
begin
  Result := Format(oauth_url, [ClientID, redirect_uri, Scope]);
end;

constructor TOAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHTTP := TIdHTTP.Create(nil);
  FSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHTTP.IOHandler := FSSLIOHandler;
  FHTTP.ConnectTimeout := cConnectionTimeout;
  FHTTP.ReadTimeout := 0;
end;

procedure TOAuth.DELETECommand(URL: string);
begin
  HTTPMethod(URL,tmDELETE,nil,nil);
end;

destructor TOAuth.destroy;
begin
  FSSLIOHandler.Destroy;
  FHTTP.Destroy;
  inherited;
end;

function TOAuth.GetAccessToken: string;
var
  Params: TStringStream;
  Response: string;
begin
  Params := TStringStream.Create(Format(tokenparams, [ClientID, ClientSecret,
    ResponseCode, redirect_uri]));
  try
    Response := POSTCommand(tokenurl, nil, Params,
      'application/x-www-form-urlencoded');
    FAccess_token := ParamValue('access_token', Response);
    FExpires_in := ParamValue('expires_in', Response);
    FRefresh_token := ParamValue('refresh_token', Response);
    Result := Access_token;
  finally
    Params.Free;
  end;
end;

function TOAuth.GETCommand(URL: string; Params: TStrings): RawBytestring;
begin
  Result:=HTTPMethod(URL,tmGET,Params,nil);
end;

function TOAuth.HTTPMethod(AURL: string; AMethod: TMethodType;
  AParams: TStrings; ABody: TStream; AMime: string): string;
var Response: TStringStream;
    ParamString: string;
begin
  if Assigned(AParams) then
    ParamString:=PrepareParams(AParams);
  try
    Response:=TStringStream.Create;
    try
      case AMethod of
        tmGET: begin
                 FHTTP.Get(AURL + ParamString, Response);
               end;
        tmPOST:begin
                 FHTTP.Request.ContentType:=AMime;
                 FHTTP.Post(AURL+ParamString,ABody,Response);
               end;
        tmPUT: begin
                 FHTTP.Request.ContentType:=AMime;
                 FHTTP.Put(AURL,ABody,Response);
               end;
        tmDELETE: begin
                   FHTTP.Delete(AURL);
                 end;
      end;
      if AMethod<>tmDELETE then
        Result:=Response.DataString;
    except
      on E: EIdHTTPProtocolException do
        raise E.CreateFmt(rsRequestError, [E.ErrorCode, E.ErrorMessage])
      else
        raise Exception.Create(rsUnknownError);
    end;
  finally
    Response.Free
  end;
end;

function TOAuth.ParamValue(ParamName, JSONString: string): string;
var
  i, j: integer;
begin
  i := pos(ParamName, JSONString);
  if i > 0 then
  begin
    for j := i + Length(ParamName) to Length(JSONString) - 1 do
      if not(JSONString[j] in StripChars) then
        Result := Result + JSONString[j]
      else if JSONString[j] = ',' then
        break;
  end
  else
    Result := '';
end;

function TOAuth.POSTCommand(URL: string; Params: TStrings; Body: TStream;
  Mime: string): RawBytestring;
begin
   Result:=HTTPMethod(URL, tmPOST,Params,Body,Mime);
end;

function TOAuth.PrepareParams(Params: TStrings): string;
var
  S: string;
begin
  if Assigned(Params) then
    begin
      Params.Add('access_token='+FAccess_token);
      for S in Params do
        Result := Result + TIdURI.ParamsEncode(S) + '&';
      Delete(Result, Length(Result), 1);
      Result := '?' + Result;
      Exit;
    end;
  Result := '';
end;

function TOAuth.PUTCommand(URL: string; Body: TStream; Mime: string)
  : RawBytestring;
begin
  Result:=HTTPMethod(URL, tmPUT,nil,Body,Mime)
end;

function TOAuth.RefreshToken: string;
var
  Params: TStringStream;
  Response: string;
begin
  Params := TStringStream.Create(Format(crefreshtoken, [ClientID, ClientSecret,
    Refresh_token]));
  try
    Response := POSTCommand(tokenurl, nil, Params,
      'application/x-www-form-urlencoded');
    FAccess_token := ParamValue('access_token', Response);
    FExpires_in := ParamValue('expires_in', Response);
    Result := Access_token;
  finally
    Params.Free;
  end;
end;

procedure TOAuth.SetClientID(const Value: string);
begin
  FClientID := Value;
end;

procedure TOAuth.SetClientSecret(Value: string);
begin
  FClientSecret :=TidURI.PathEncode(Value)
end;

procedure TOAuth.SetResponseCode(const Value: string);
begin
  FResponseCode := Value;
end;

procedure TOAuth.SetScope(const Value: string);
begin
  FScope := Value;
end;

end.
