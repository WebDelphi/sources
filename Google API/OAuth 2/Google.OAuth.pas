unit Google.OAuth;

interface

uses System.Classes, System.JSON, Data.DbxPlatform, idHTTP, idURI, IdSSLOpenSSL, SysUtils,
{$IFDEF MSWINDOWS}
  WinApi.ShellAPI,
  WinApi.Windows;
{$ELSE}
{$IFDEF MACOS}
  FMX.Platform,
  Macapi.Foundation,
  Macapi.AppKit;
{$ENDIF}
{$ENDIF}

type
  {$REGION 'Класс исключений для компонента. OAuthException, в отличие от своего предка...'}
  ///	<summary>
  ///	  Класс исключений для компонента.
  ///	  <see cref="EOAuthException">OAuthException</see>, в отличие от своего
  ///	  предка EIdHTTPProtocolException содержит полезную информацию об
  ///	  исключении, которую может предоставлять Google
  ///	</summary>
  {$ENDREGION}
  EOAuthException = class(EIdHTTPProtocolException)
  private
    FServerMessage: string;
    FDomain: string;
    FReason: string;
    FLocationType: string;
    FLocation: string;
    procedure Parse;
  public
    constructor CreateError(const anErrCode: Integer;
      const asReplyMessage: string; const asErrorMessage: string); override;

    {$REGION 'Сообщение об ошибке, отправленное сервером Google'}
    ///	<summary>
    ///	  Сообщение об ошибке, отправленное сервером Google
    ///	</summary>
    ///	<value>
    ///	  ServerMessage: string
    ///	</value>
    ///	<example>
    ///	  "Invalid Credentials", "Invalid token" и т.д. 
    ///	</example>
    {$ENDREGION}
    property ServerMessage: string read FServerMessage;
    property Domain: string read FDomain;
    property Reason: string read FReason;

    {$REGION 'Указывает на местоположение проблемы (заголовок запроса, параметр и т.д.) '}
    ///	<summary>
    ///	  Указывает на местоположение проблемы (заголовок запроса, параметр и
    ///	  т.д.) 
    ///	</summary>
    ///	<value>
    ///	  LocationType: string
    ///	</value>
    ///	<example>
    ///	  "header", "parameter"
    ///	</example>
    {$ENDREGION}
    property LocationType: string read FLocationType;

    {$REGION 'Содержит название заголовка или параметра, который стал причиной исключения'}
    ///	<summary>
    ///	  Содержит название заголовка или параметра, который стал причиной
    ///	  исключения
    ///	</summary>
    ///	<value>
    ///	  Location: string
    ///	</value>
    ///	<example>
    ///	  "Authorization"
    ///	</example>
    {$ENDREGION}
    property Location: string read FLocation;
  end;

  {$REGION 'Класс управления точками доступа к Google API'}
  ///	<summary>
  ///	  Класс управления точками доступа к Google API
  ///	</summary>
  {$ENDREGION}
  TScopes = class(TPersistent)
  private
    FFullAccess: boolean;
    FReadonly: boolean;
  protected
    FScopes: TStrings;
    function GetScopes: TStrings; virtual;
    function GetAuthPrefix: string; virtual;
    procedure SetupScope(const AScopeValue: string);virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    {$REGION 'Список, содержащий точки доступа к API, которые будут использованы при ав...'}
    ///	<summary>
    ///	  Список, содержащий точки доступа к API, которые будут использованы
    ///	  при авторизации пользователя. Список формируется в зависимости от
    ///	  значений свойств объекта (<see cref="FullAccess" />,
    ///	  <see cref="Readonly" /> и т.д.)
    ///	</summary>
    ///	<value>
    ///	  Scopes: <span class="valueType">TStrings</span>
    ///	</value>
    ///	<example>
    ///	  "https://www.googleapis.com/auth/urlshortener"
    ///	</example>
    {$ENDREGION}
    property Scopes: TStrings read GetScopes;
  published
    {$REGION 'Указывает на то, что при работе с API будет использоваться полный доступ ...'}
    ///	<summary>
    ///	  Указывает на то, что при работе с API будет использоваться полный
    ///	  доступ к ресурсам в аккаунте пользователя. Это значение может
    ///	  использоваться совместно с <see cref="Readonly" />
    ///	</summary>
    ///	<value>
    ///	  FullAccess: <span class="valueType">boolean</span>
    ///	</value>
    {$ENDREGION}
    property FullAccess: boolean read FFullAccess write FFullAccess;

    {$REGION 'Указывает на то, что при работе с API прилоению будет открыт доступ тольк...'}
    ///	<summary>
    ///	  Указывает на то, что при работе с API прилоению будет открыт доступ
    ///	  только на чтение данных из аккаунта пользователя
    ///	</summary>
    ///	<value>
    ///	  Readonly: <span class="valueType">boolean</span>
    ///	</value>
    {$ENDREGION}
    property Readonly: boolean read FReadonly write FReadonly;
  end;

  {$REGION 'Класс для управления точками доступа к Google.Drive'}
  ///	<summary>
  ///	  Класс для управления точками доступа к Google.Drive
  ///	</summary>
  {$ENDREGION}
  TDriveScopes = class(TScopes)
  private
    const
      cScopeValues: array [0..4] of string = ('file','apps.readonly','readonly.metadata','install','appdata');
  private
    FFileAccess: boolean;
    FAppsReadonly: boolean;
    FReadonlyMetadata: boolean;
    FInstall: boolean;
    FAppdata: boolean;
  protected
    function GetScopes: TStrings; override;
    function GetAuthPrefix: string; override;
    procedure SetupScope(const AScopeValue: string);override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    {$REGION 'См. TScopes.FullAccess'}
    ///	<summary>
    ///	  См. <see cref="TScopes.FullAccess" />
    ///	</summary>
    ///	<value>
    ///	  FullAccess: boolean
    ///	</value>
    {$ENDREGION}
    property FullAccess;

    {$REGION 'См. TScopes.Readonly'}
    ///	<summary>
    ///	  См. <see cref="TScopes.Readonly" />
    ///	</summary>
    ///	<value>
    ///	  Readonly: boolean
    ///	</value>
    {$ENDREGION}
    property Readonly;
    property FileAccess: boolean read FFileAccess write FFileAccess;
    property AppsReadonly: boolean read FAppsReadonly write FAppsReadonly;
    property ReadonlyMetadata: boolean read FReadonlyMetadata
      write FReadonlyMetadata;
    property Install: boolean read FInstall write FInstall;
    property Appdata: boolean read FAppdata write FAppdata;
  end;

  TCalendarScopes = class(TScopes)
  protected
    function GetAuthPrefix: string; override;
  end;

  TTasksScopes = class(TScopes)
  protected
    function GetAuthPrefix: string; override;
  end;

  TCustomTokenInfo = class(TPersistent)
  private
    FAudience: string;
    FExpiresTime: TDateTime;
    FAccessToken: string;
    FRefreshToken: string;
    FTokenType: string;
    FScopes: TStrings;
    FDriveScopes: TDriveScopes;
    FCalendarScopes: TCalendarScopes;
    FTasksScopes: TTasksScopes;
    function GetScopeStr: string;
    procedure SetScopes(AScopes: TStrings);
    procedure SetDriveScopes(ADriveScopes: TDriveScopes);
    procedure SetCalendarScopes(ACalendarScopes: TCalendarScopes);
    procedure SetTasksScopes(ATaskScopes: TTasksScopes);
    function GetJSON: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Revoke;
    procedure Assign(Source: TPersistent); override;
    procedure Parse(const AJSONString: string);
    property AccessToken: string read FAccessToken;
    property RefreshToken: string read FRefreshToken;
    property TokenType: string read FTokenType;
    property ExpiresTime: TDateTime read FExpiresTime;
    property ScopeParam: string read GetScopeStr;
    property Audience: string read FAudience write FAudience;
    property JSON: string read GetJSON;
  published
    property DriveScopes: TDriveScopes read FDriveScopes write SetDriveScopes;
    property CalendarScopes: TCalendarScopes read FCalendarScopes
      write SetCalendarScopes;
    property TasksScopes: TTasksScopes read FTasksScopes write SetTasksScopes;
    property CustomScopes: TStrings read FScopes write SetScopes;
  end;

  TTokenInfo = class(TCustomTokenInfo)
  public
    property AccessToken;
    property RefreshToken;
    property TokenType;
    property ExpiresTime;
    property ScopeParam;
    property Audience;
    property JSON;
  published
    property DriveScopes;
    property CalendarScopes;
    property TasksScopes;
    property CustomScopes;
  end;

  TOnGetToken = procedure(const ATokenInfo: TCustomTokenInfo) of object;

  TSaveFields = (sfClientSecret, sfRedirectURI, sfState, sfLoginHint);
  TSaveFieldsSet = set of TSaveFields;
  TDefaultContentType = (ctJSON, ctXML);

  TOAuthClient = class(TComponent)
  private
    FHTTPClient: TidHTTP;
    FSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    FRedirectURI: string;
    FClientSecret: string;
    FTokenInfo: TCustomTokenInfo;
    FState: string;
    FLoginHint: string;
    FOpenStartURL: boolean;
    FValidateOnLoad: boolean;
    FSaveFields: TSaveFieldsSet;
    FOnGetToken: TOnGetToken;
    FDefaultContentType: TDefaultContentType;
    FOnDisconnect: TNotifyEvent;
    FOnSave: TNotifyEvent;
    FOnLoad: TNotifyEvent;
    FOnValidateError: TNotifyEvent;
    FOnValidateComplete: TNotifyEvent;
    procedure OpenURL(const AURL: string);
    procedure RaiseError(E: Exception; const ACustomMessage: string='');
    function GetStartURL: string;
    procedure SetTokenInfo(ATokenInfo: TCustomTokenInfo);
    procedure DoOnGetToken;
    procedure DoOnLoad;
    function GetClientID: string;
    procedure SetClientID(const AAudience: string);
    procedure SetHeaders(const AMimeType: string = '';
      NeedContentType: boolean = True);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function StartConnect: string;
    procedure EndConnect(const ACode: string);
    procedure RefreshToken;
    procedure Disconnect;
    function OnlineValidate:boolean;
    procedure SaveToFile(const AFileName: string);
    procedure LoadFromFile(const AFileName: string);

    procedure Get(const AURL: String; AResponseStream: TStream);
    procedure Post(const AURL: String; ASourceStream: TStream;
      AResponseStream: TStream); overload;
    procedure Post(const AURL, AContentType: String; ASourceStream: TStream;
      AResponseStream: TStream); overload;
    procedure Delete(const AURL: string); overload;
    procedure Put(const AURL: string; ASourceStream: TStream;
      AResponseStream: TStream); overload;
    procedure Put(const AURL, AContentType: string; ASourceStream: TStream;
      AResponseStream: TStream); overload;

  published
    property RedirectURI: string read FRedirectURI write FRedirectURI;
    property ClientID: string read GetClientID write SetClientID;
    property ClientSecret: string read FClientSecret write FClientSecret;
    property State: string read FState write FState;
    property LoginHint: string read FLoginHint write FLoginHint;

    property TokenInfo: TCustomTokenInfo read FTokenInfo write SetTokenInfo;
    property SaveFields: TSaveFieldsSet read FSaveFields write FSaveFields;
    property DefaultContentType: TDefaultContentType read FDefaultContentType
      write FDefaultContentType;

    property ValidateOnLoad: boolean read FValidateOnLoad write FValidateOnLoad;
    property OpenStartURL: boolean read FOpenStartURL write FOpenStartURL;

    property OnGetToken: TOnGetToken read FOnGetToken write FOnGetToken;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
    property OnValidateError: TNotifyEvent read FOnValidateError write FOnValidateError;
    property OnValidateComplete: TNotifyEvent read FOnValidateComplete write FOnValidateComplete;
  end;

procedure Register;

implementation

uses DateUtils, StrUtils;

const
  cAuthScope = 'https://www.googleapis.com/auth/';
  cOAuthURL = 'https://accounts.google.com/o/oauth2/';
  cValidateURL =
    'https://www.googleapis.com/oauth2/v1/tokeninfo?access_token=%s';
  cRevokeURL = cOAuthURL + 'revoke?token=%s';
  cOAuthAccountURL = cOAuthURL + 'auth';
  cTokenURL = cOAuthURL + 'token';
  cTokenInfoPairs: array [0 .. 5] of string = ('access_token', 'refresh_token',
    'expires_in', 'token_type', 'audience', 'scope');
  cFilePairs: array [0 .. 4] of string = ('clientSecret', 'redirectUri',
    'state', 'loginHint', 'tokenInfo');
  cAPIPrifixes: array [0..2]of string = ('drive','calendar','tasks');

  cDefaultSave = [sfClientSecret, sfRedirectURI, sfState, sfLoginHint];

procedure Register;
begin
  RegisterComponents('Google Components', [TOAuthClient]);
end;

{ TOAuthClient }

constructor TOAuthClient.Create(AOwner: TComponent);
begin
  inherited;
  FTokenInfo := TCustomTokenInfo.Create;
  FSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(self);
  FHTTPClient := TidHTTP.Create(self);
  FHTTPClient.IOHandler := FSSLIOHandler;
  FHTTPClient.HandleRedirects := True;
  FSaveFields := cDefaultSave;
end;

procedure TOAuthClient.Delete(const AURL: string);
begin
  SetHeaders(EmptyStr, False);
  try
    FHTTPClient.Delete(AURL);
  except
    on E:EIdHTTPProtocolException do
      RaiseError(E)
    else
      raise;
  end;
end;

destructor TOAuthClient.Destroy;
begin
  FTokenInfo.Free;
  inherited;
end;

procedure TOAuthClient.Disconnect;
begin
  try
    FHTTPClient.Get(Format(cRevokeURL, [FTokenInfo.AccessToken]));
    FTokenInfo.Revoke;
    if Assigned(FOnDisconnect) then
      FOnDisconnect(self);
  except
    on E:EIdHTTPProtocolException do
      RaiseError(E)
    else
      raise;
  end;
end;

procedure TOAuthClient.DoOnGetToken;
begin
  if Assigned(FOnGetToken) then
    FOnGetToken(FTokenInfo)
end;

procedure TOAuthClient.DoOnLoad;
begin
  if Assigned(FOnLoad) then
    FOnLoad(self);
  if FValidateOnLoad then
    OnlineValidate
end;

procedure TOAuthClient.EndConnect(const ACode: string);
var
  Params: TStringList;
  Response: TStringStream;
begin
  Params := TStringList.Create;
  try
    Params.Values['client_id'] := FTokenInfo.Audience;
    Params.Values['client_secret'] := FClientSecret;
    if Length(ACode) > 0 then
    begin
      Params.Values['redirect_uri'] := FRedirectURI;
      Params.Values['grant_type'] := 'authorization_code';
      Params.Values['code'] := ACode;
    end
    else
    begin
      Params.Values['refresh_token'] := FTokenInfo.RefreshToken;
      Params.Values['grant_type'] := 'refresh_token';
    end;
    Response := TStringStream.Create;
    try
      try
        FHTTPClient.Post(cTokenURL, Params, Response);
        FTokenInfo.Parse(Response.DataString);
        DoOnGetToken;
      except
        on E:EIdHTTPProtocolException do
          RaiseError(E)
        else
          raise
      end;
    finally
      Response.Free;
    end;
  finally
    Params.Free;
  end;
end;

procedure TOAuthClient.LoadFromFile(const AFileName: string);
var
  Stream: TStringStream;
  AJSONValue: TJSONValue;
  Enum: TJSONPairEnumerator;
begin
  if not FileExists(AFileName) then
    Exit;
  Stream := TStringStream.Create;
  try
    Stream.LoadFromFile(AFileName);
    AJSONValue := TJSONObject.ParseJSONValue(Stream.DataString);
    if not Assigned(AJSONValue) then
      raise EOAuthException.CreateFmt('Can''''t parse file %s', [AFileName]);
    try
      Enum := TJSONObject(AJSONValue).GetEnumerator;
      try
        while Enum.MoveNext do
          with Enum.Current do
            case AnsiIndexStr(JsonString.Value, cFilePairs) of
              0:ClientSecret := JsonValue.Value; // clientSecret
              1:FRedirectURI := JsonValue.Value; // redirectUri
              2:FState := JsonValue.Value; // state
              3:FLoginHint := JsonValue.Value; // loginHint
              4:FTokenInfo.Parse(TJSONObject(JsonValue).ToString); // tokenInfo
            end;
        DoOnLoad;
      finally
        Enum.Free
      end;
    finally
      AJSONValue.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TOAuthClient.OnlineValidate:boolean;
var
  Response: TStringStream;
begin
  Response := TStringStream.Create;
  Result:=False;
  try
    try
      FHTTPClient.Get(Format(cValidateURL, [FTokenInfo.AccessToken]), Response);
      FTokenInfo.Parse(Response.DataString);
      Result:=True;
      if Assigned(FOnValidateComplete) then
        FOnValidateComplete(self);
      DoOnGetToken;
    except
      on E:EIdHTTPProtocolException do
        begin
          if Assigned(FOnValidateError) then
            FOnValidateError(self)
        end
      else
        raise;
    end;
  finally
    Response.Free;
  end;
end;

procedure TOAuthClient.OpenURL(const AURL: string);
{$IFDEF MACOS}
var
  Workspace: NSWorkspace;
  Url: NSURL;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
{$ELSE}
{$IFDEF MACOS}
    Workspace := TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace);
    Url := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(NSStr(AURL)));
    Workspace.openURL(Url);
{$ENDIF}
{$ENDIF}
end;

procedure TOAuthClient.Post(const AURL, AContentType: String;
  ASourceStream, AResponseStream: TStream);
begin
  SetHeaders(AContentType);
  try
    FHTTPClient.Post(AURL, ASourceStream, AResponseStream);
  except
    on E:EIdHTTPProtocolException do
      RaiseError(E)
    else
      raise;
  end;
end;

procedure TOAuthClient.Post(const AURL: String;
  ASourceStream, AResponseStream: TStream);
begin
  Post(AURL, EmptyStr, ASourceStream, AResponseStream);
end;

procedure TOAuthClient.Put(const AURL, AContentType: string;
  ASourceStream, AResponseStream: TStream);
begin
  SetHeaders(AContentType);
  try
    FHTTPClient.Put(AURL, ASourceStream, AResponseStream)
  except
    on E:EIdHTTPProtocolException do
      RaiseError(E)
    else
      raise;
  end;
end;

procedure TOAuthClient.Put(const AURL: string;
  ASourceStream, AResponseStream: TStream);
begin
  Put(AURL, EmptyStr, ASourceStream, AResponseStream)
end;

procedure TOAuthClient.RaiseError(E: Exception; const ACustomMessage: string);
begin
  if E is EIdHTTPProtocolException then
    begin
      if Length(ACustomMessage)=0 then
        raise EOAuthException.CreateError(EIdHTTPProtocolException(E).ErrorCode,
                                          E.Message,
                                          EIdHTTPProtocolException(E).ErrorMessage)
      else
        raise EOAuthException.CreateError(EIdHTTPProtocolException(E).ErrorCode,
                                          ACustomMessage,
                                          EIdHTTPProtocolException(E).ErrorMessage)
    end
end;

procedure TOAuthClient.RefreshToken;
begin
  EndConnect(EmptyStr);
end;

procedure TOAuthClient.SaveToFile(const AFileName: string);
var
  JSONObject: TJSONObject;
  Stream: TStringStream;
begin
  if AFileName = EmptyStr then
    Exit;
  JSONObject := TJSONObject.Create;
  try
    if sfClientSecret in FSaveFields then
      JSONObject.AddPair(cFilePairs[0], FClientSecret);
    if sfRedirectURI in FSaveFields then
      JSONObject.AddPair(cFilePairs[1], FRedirectURI);
    if sfState in FSaveFields then
      JSONObject.AddPair(cFilePairs[2], FState);
    if sfLoginHint in FSaveFields then
      JSONObject.AddPair(cFilePairs[3], FLoginHint);
    JSONObject.AddPair(cFilePairs[4],
      TJSONObject.ParseJSONValue(TokenInfo.JSON));
    Stream := TStringStream.Create;
    try
      Stream.WriteString(JSONObject.ToString);
      Stream.SaveToFile(AFileName);
      if Assigned(FOnSave) then
        FOnSave(self)
    finally
      Stream.Free;
    end;
  finally
    JSONObject.Free;
  end;
end;

procedure TOAuthClient.SetClientID(const AAudience: string);
begin
  if FTokenInfo.Audience <> AAudience then
    FTokenInfo.Audience := AAudience;
end;

procedure TOAuthClient.SetHeaders(const AMimeType: string;
  NeedContentType: boolean);
begin
  FHTTPClient.Request.CustomHeaders.Clear;
  FHTTPClient.Request.CustomHeaders.Add('Authorization: ' + FTokenInfo.TokenType
    + ' ' + FTokenInfo.AccessToken);
  if NeedContentType then
  begin
    if AMimeType <> EmptyStr then
      FHTTPClient.Request.ContentType := AMimeType
    else
      case FDefaultContentType of
        ctJSON:FHTTPClient.Request.ContentType := 'application/json';
        ctXML: FHTTPClient.Request.ContentType := 'application/xml';
      end;
  end;
end;

procedure TOAuthClient.SetTokenInfo(ATokenInfo: TCustomTokenInfo);
begin
  FTokenInfo.Assign(ATokenInfo);
end;

function TOAuthClient.StartConnect: string;
begin
  Result := GetStartURL;
  if FOpenStartURL then
    OpenURL(Result);
end;

procedure TOAuthClient.Get(const AURL: String; AResponseStream: TStream);
begin
  SetHeaders(EmptyStr, False);
  try
    FHTTPClient.Get(AURL, AResponseStream);
  except
    on E: EIdHTTPProtocolException do
      RaiseError(E)
    else
      raise;
  end;
end;

function TOAuthClient.GetClientID: string;
begin
  Result := FTokenInfo.Audience;
end;

function TOAuthClient.GetStartURL: string;
var
  Params: TStringList;
begin
  Params := TStringList.Create;
  try
    Params.Delimiter := '&';
    Params.Values['response_type'] := 'code';
    Params.Values['client_id'] := FTokenInfo.Audience;
    Params.Values['redirect_uri'] := FRedirectURI;
    Params.Values['scope'] := TIdURI.ParamsEncode(FTokenInfo.ScopeParam);
    if FState <> EmptyStr then
      Params.Values['state'] := TIdURI.ParamsEncode(FState);
    if FLoginHint <> EmptyStr then
      Params.Values['login_hint'] := FLoginHint;
    Result := cOAuthAccountURL + '?' + Params.DelimitedText;
  finally
    Params.Free;
  end;
end;

{ TTokenInfo }

procedure TCustomTokenInfo.Assign(Source: TPersistent);
begin
  if Source is TCustomTokenInfo then
  begin
    FExpiresTime := TCustomTokenInfo(Source).ExpiresTime;
    FAccessToken := TCustomTokenInfo(Source).AccessToken;
    FRefreshToken := TCustomTokenInfo(Source).RefreshToken;
    FTokenType := TCustomTokenInfo(Source).TokenType;
    FScopes.Assign(TCustomTokenInfo(Source).CustomScopes);
    FDriveScopes.Assign(TCustomTokenInfo(Source).DriveScopes);
    FCalendarScopes.Assign(TCustomTokenInfo(Source).CalendarScopes);
    FTasksScopes.Assign(TCustomTokenInfo(Source).TasksScopes);
  end;
end;

constructor TCustomTokenInfo.Create;
begin
  inherited;
  FDriveScopes := TDriveScopes.Create;
  FCalendarScopes := TCalendarScopes.Create;
  FTasksScopes := TTasksScopes.Create;
  FScopes := TStringList.Create;
  FScopes.Delimiter := ' ';
end;

destructor TCustomTokenInfo.Destroy;
begin
  FScopes.Free;
  FTasksScopes.Free;
  FCalendarScopes.Free;
  FDriveScopes.Free;
  inherited;
end;

function TCustomTokenInfo.GetJSON: string;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    with JSONObject do
      begin
         AddPair(cTokenInfoPairs[0], FAccessToken);
         AddPair(cTokenInfoPairs[1], FRefreshToken);
         AddPair(cTokenInfoPairs[2], TJSONNumber.Create(SecondsBetween(FExpiresTime,Now)));
         AddPair(cTokenInfoPairs[3], FTokenType);
         AddPair(cTokenInfoPairs[4], FAudience);
         AddPair(cTokenInfoPairs[5], GetScopeStr);
      end;
    Result := JSONObject.ToString;
  finally
    JSONObject.Free;
  end;
end;

function TCustomTokenInfo.GetScopeStr: string;
var
  Params: TStringList;
begin
  Params := TStringList.Create;
  try
    Params.Delimiter := ' ';
    Params.AddStrings(FDriveScopes.Scopes);
    Params.AddStrings(FCalendarScopes.Scopes);
    Params.AddStrings(FTasksScopes.Scopes);
    Params.AddStrings(FScopes);
    Result := Params.DelimitedText;
  finally
    Params.Free;
  end;
end;

procedure TCustomTokenInfo.Parse(const AJSONString: string);

procedure ParseScope(const AScope: string; var Prefix, Value: string);
var ScopeText: string;
begin
  Prefix:=EmptyStr;
  Value:=EmptyStr;
  ScopeText:=AScope;
  if pos(cAuthScope,ScopeText)>0 then
    begin
      Delete(ScopeText,1,length(cAuthScope));
      if pos('.',ScopeText)>0 then
        begin
          Prefix:=copy(ScopeText,1,pos('.',ScopeText)-1);
          Delete(ScopeText,1,pos('.',ScopeText));
          Value:=ScopeText;
        end
      else
        Prefix:=ScopeText;
    end;
end;

var
  JsonValue: TJSONValue;
  Enum: TJSONPairEnumerator;
  TmpScopes: TStringList;
  I, idx: integer;
  sPrefix, sValue: string;
begin
  JsonValue := TJSONObject.ParseJSONValue(AJSONString);
  if Assigned(JsonValue) then
  begin
    Enum := TJSONObject(JsonValue).GetEnumerator;
    try
      while Enum.MoveNext do
        with Enum.Current do
          case AnsiIndexStr(JsonString.Value, cTokenInfoPairs) of
            0:FAccessToken := JsonValue.Value; // access_token
            1:FRefreshToken := JsonValue.Value; // refresh_token
            2:FExpiresTime := IncSecond(Now, TJSONNumber(JsonValue).AsInt64);// expires_in
            3:FTokenType := JsonValue.Value; // token_type
            4:FAudience := JsonValue.Value; // audience
            5:begin
                TmpScopes:=TStringList.Create;
                try
                  TmpScopes.Delimiter:=' ';
                  TmpScopes.DelimitedText:=JsonValue.Value;
                  for I:= TmpScopes.Count-1 downto 0 do
                    begin
                      if pos(cAuthScope,TmpScopes[i])>0 then
                        begin
                          ParseScope(TmpScopes[i],sPrefix,sValue);
                          idx:=AnsiIndexStr(sPrefix,cAPIPrifixes);
                          //cAPIPrifixes: array [0..2]of string = ('drive','calendar','tasks');
                          case idx of
                            0:FDriveScopes.SetupScope(sValue);
                            1:FCalendarScopes.SetupScope(sValue);
                            2:FTasksScopes.SetupScope(sValue);
                          end;
                          if idx>-1 then
                            TmpScopes.Delete(i);
                        end
                    end;
                    FScopes.Assign(TmpScopes);
                finally
                  TmpScopes.Free;
                end;
                 // scope
              end;
          end;
    finally
      Enum.Free;
    end;
    JsonValue.Free;
  end;
end;

procedure TCustomTokenInfo.Revoke;
begin
  FExpiresTime := 0;
  FAccessToken := EmptyStr;
  FRefreshToken := EmptyStr;
  FTokenType := EmptyStr;
end;

procedure TCustomTokenInfo.SetCalendarScopes(ACalendarScopes: TCalendarScopes);
begin
  FCalendarScopes.Assign(ACalendarScopes);
end;

procedure TCustomTokenInfo.SetDriveScopes(ADriveScopes: TDriveScopes);
begin
  FDriveScopes.Assign(ADriveScopes);
end;

procedure TCustomTokenInfo.SetScopes(AScopes: TStrings);
begin
  FScopes.Assign(AScopes);
end;

procedure TCustomTokenInfo.SetTasksScopes(ATaskScopes: TTasksScopes);
begin
  FTasksScopes.Assign(ATaskScopes);
end;

{ TDriveScopes }

procedure TDriveScopes.Assign(Source: TPersistent);
begin
  Inherited Assign(Source);
  if Source is TDriveScopes then
  begin
    FFileAccess := TDriveScopes(Source).FileAccess;
    FAppsReadonly := TDriveScopes(Source).AppsReadonly;
    FReadonlyMetadata := TDriveScopes(Source).ReadonlyMetadata;
    FInstall := TDriveScopes(Source).Install;
    FAppdata := TDriveScopes(Source).Appdata;
  end;
end;

function TDriveScopes.GetAuthPrefix: string;
begin
  Result := 'drive';
end;

function TDriveScopes.GetScopes: TStrings;
begin
  inherited;
  if FFileAccess then
    FScopes.Add(cAuthScope + 'drive.file');
  if FAppsReadonly then
    FScopes.Add(cAuthScope + 'drive.apps.readonly');
  if FReadonlyMetadata then
    FScopes.Add(cAuthScope + 'drive.readonly.metadata');
  if FInstall then
    FScopes.Add(cAuthScope + 'drive.install');
  if FAppdata then
    FScopes.Add(cAuthScope + 'drive.appdata');
  Result := FScopes;
end;

procedure TDriveScopes.SetupScope(const AScopeValue: string);
begin
  case AnsiIndexStr(AScopeValue,cScopeValues) of
    0:FFileAccess:=True;
    1:FAppsReadonly:=True;
    2:FReadonlyMetadata:=True;
    3:FInstall:=True;
    4:FAppdata:=True;
  else
    inherited SetupScope(AScopeValue);
  end;
end;

{ TScopes }

procedure TScopes.Assign(Source: TPersistent);
begin
  if Source is TScopes then
  begin
    FFullAccess := TScopes(Source).FullAccess;
    FReadonly := TScopes(Source).Readonly;
  end;
end;

constructor TScopes.Create;
begin
  inherited;
  FScopes := TStringList.Create;
end;

destructor TScopes.Destroy;
begin
  FScopes.Free;
  inherited;
end;

function TScopes.GetAuthPrefix: string;
begin
  Result := EmptyStr;
end;

function TScopes.GetScopes: TStrings;
begin
  FScopes.Clear;
  FScopes.Delimiter := ' ';
  if FFullAccess then
    FScopes.Add(cAuthScope + GetAuthPrefix);
  if FReadonly then
    FScopes.Add(cAuthScope + GetAuthPrefix + '.readonly');
  Result := FScopes;
end;


procedure TScopes.SetupScope(const AScopeValue: string);
begin
  if AScopeValue=EmptyStr then
    FFullAccess:=True
  else
    FReadonly:=SameText(AScopeValue,'readonly');
end;

{ TCalendarScopes }

function TCalendarScopes.GetAuthPrefix: string;
begin
  Result := 'calendar'
end;

{ TTasksScopes }

function TTasksScopes.GetAuthPrefix: string;
begin
  Result := 'tasks'
end;

{ EOAuthException }

constructor EOAuthException.CreateError(const anErrCode: Integer;
  const asReplyMessage, asErrorMessage: string);
begin
  inherited;
  Parse;
end;

procedure EOAuthException.Parse;
const
  cErrorPairs: array [0 .. 4] of string = ('domain', 'reason', 'message',
    'locationType', 'location');
var
  AJSONValue: TJSONValue;
  Errors: TJSONArray;
  Pair: TJSONPair;
  ErrorObject: TJSONObject;
  Enum: TJSONPairEnumerator;
begin
  if Length(ErrorMessage) > 0 then
  begin
    AJSONValue := TJSONObject.ParseJSONValue(ErrorMessage);
    try
      Pair := TJSONObject(AJSONValue).Get('error');
      if Assigned(Pair) then
      begin
        if Pair.JsonValue is TJSONObject then
        begin
          Errors := TJSONObject(Pair.JsonValue).Get('errors')
            .JsonValue as TJSONArray;
          if Errors.Size > 0 then
          begin
            ErrorObject := Errors.Get(0) as TJSONObject;
            if Assigned(ErrorObject) then
            begin
              Enum := ErrorObject.GetEnumerator;
              try
                while Enum.MoveNext do
                  with Enum.Current do
                    case AnsiIndexStr(JsonString.Value, cErrorPairs) of
                      0:FDomain := JsonValue.Value;
                      1:FReason := JsonValue.Value;
                      2:FServerMessage := JsonValue.Value;
                      3:FLocationType := JsonValue.Value;
                      4:FLocation := JsonValue.Value;
                    end;
              finally
                Enum.Free;
              end;
            end;
          end;
        end
       else
         FServerMessage:=Pair.JsonValue.Value;
      end
    finally
      AJSONValue.Free;
    end;
  end;
end;

end.
