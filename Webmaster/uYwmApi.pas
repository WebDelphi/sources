unit uYwmApi;

interface

uses
  System.SysUtils, System.Classes, REST.Types, REST.Client,
  REST.Authenticator.OAuth, Data.Bind.Components, Data.Bind.ObjectScope,
  System.JSON, Generics.Collections;

type
  TYwmHostInfo = class
  private
    Fhost_id: string;
    Fascii_host_url: string;
    Funicode_host_url: string;
    Fverified: boolean;
  public
    property host_id: string read Fhost_id write Fhost_id;
    property ascii_host_url: string read Fascii_host_url write Fascii_host_url;
    property unicode_host_url: string read Funicode_host_url write Funicode_host_url;
    property verified: boolean read Fverified write Fverified;
  end;

  TApiHostDataStatus = (NOT_INDEXED, NOT_LOADED, OK);

  TYwmHost = class
  private
    FMainInfo: TYwmHostInfo;
    FMainMirror: boolean;
    FMirror: TYwmHostInfo;

    Fhost_data_status: TApiHostDataStatus;
    Fhost_display_name: string;

    Fsqi: integer;
    Fexcluded_pages_count: int64;
    Fsearchable_pages_count: int64;
    Fsite_problems: integer;

    procedure Parse(const AJsonValue: TJSONValue);
    procedure Setexcluded_pages_count(const Value: int64);
    procedure Setsearchable_pages_count(const Value: int64);
    procedure Setsite_problems(const Value: integer);
    procedure Setsqi(const Value: integer);
  public
    constructor Create(const AJsonValue: TJSONValue);
    destructor Destroy;override;
    property MainInfo: TYwmHostInfo read FMainInfo;
    property Mirror: TYwmHostInfo read FMirror;
    property MainMirror: boolean read FMainMirror;
    property host_data_status: TApiHostDataStatus read Fhost_data_status write Fhost_data_status;
    property host_display_name: string read Fhost_display_name write Fhost_display_name;
    property sqi: integer read Fsqi write Setsqi;
    property excluded_pages_count: int64 read Fexcluded_pages_count write Setexcluded_pages_count;
    property searchable_pages_count: int64 read Fsearchable_pages_count write Setsearchable_pages_count;
    property site_problems: integer read Fsite_problems write Setsite_problems;
end;

  TYwmHostList = class(TObjectList<TYwmHost>)
  private
    FList:TObjectList<TYwmHost>;
    function GetList:TObjectList<TYwmHost>;
  public
    constructor Create;
    destructor Destroy;override;

    property List:TObjectList<TYwmHost> read GetList;
  end;


type
  TYWM = class(TDataModule)
    YwmClient: TRESTClient;
    YwmRequest: TRESTRequest;
    YwmResponse: TRESTResponse;
    YwmAuth: TOAuth2Authenticator;
    procedure YwmRequestHTTPProtocolError(Sender: TCustomRESTRequest);
    procedure YwmAuthAuthenticate(ARequest: TCustomRESTRequest;
      var ADone: Boolean);
    procedure DataModuleCreate(Sender: TObject);

  private
    ActiveHost:TYwmHost;
    procedure WebFormRedirect(const AURL: string; var DoCloseWebView : boolean);
    procedure DoAfterUserID(Sender: TCustomRESTRequest);
    procedure DoAfterGetHosts(Sender: TCustomRESTRequest);
    procedure DoAfterGetHostInfo(Sender: TCustomRESTRequest);
    procedure DoAfterGetHostSummary(Sender: TCustomRESTRequest);
    procedure GetUserID;
  public
    Authorized: boolean;
    UserID: string;
    YwmHostList:TYwmHostList;
    procedure YandexAuth;
    procedure GetHosts;
    procedure GetHostInfo(const AHost: TYwmHost);
    procedure GetHostSymmary(const AHost: TYwmHost);
  end;

var
  YWM: TYWM;

implementation

uses REST.Authenticator.OAuth.WebForm.Win, REST.Utils,Vcl.Forms;

const
  cApiHostDataStatus: array [0..2] of string = ('NOT_INDEXED', 'NOT_LOADED', 'OK');

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

function StrIndex(const AStr: string; Arr: array of string):integer;
var i:integer;
begin
  Result:=-1;
  for I := Low(Arr) to High(Arr) do
    if SameText(Arr[i],AStr) then
       Exit(I)
end;

{ TYWM }

procedure TYWM.DataModuleCreate(Sender: TObject);
begin
  YwmHostList:=TYwmHostList.Create;
end;

procedure TYWM.DoAfterGetHostInfo(Sender: TCustomRESTRequest);
begin
  ActiveHost.host_display_name:=YwmResponse.JSONValue.P['host_display_name'].AsType<string>;
  ActiveHost.Fhost_data_status:=TApiHostDataStatus(StrIndex(YwmResponse.JSONValue.P['host_data_status'].AsType<string>,cApiHostDataStatus));
end;

procedure TYWM.DoAfterGetHosts(Sender: TCustomRESTRequest);
var AHosts: TJSONArray;
    I: Integer;
begin
  if YwmResponse.StatusCode<>200 then
    Exit;
  YwmHostList.Clear;
  AHosts:=YwmResponse.JSONValue.P['hosts'].AsType<TJSONArray>;
  for I := 0 to Pred(AHosts.Count) do
    YwmHostList.Add(TYwmHost.Create(AHosts.Items[i]));
end;

procedure TYWM.DoAfterGetHostSummary(Sender: TCustomRESTRequest);
begin
  ActiveHost.sqi:=YwmResponse.JSONValue.P['sqi'].AsType<integer>;
  ActiveHost.excluded_pages_count:=YwmResponse.JSONValue.P['excluded_pages_count'].AsType<int64>;
  ActiveHost.searchable_pages_count:=YwmResponse.JSONValue.P['searchable_pages_count'].AsType<int64>;
//  ActiveHost.site_problems:=YwmResponse.JSONValue.P['site_problems'].AsType<TJsonValue>;

end;

procedure TYWM.DoAfterUserID(Sender: TCustomRESTRequest);
begin
  UserID:=YwmResponse.JSONValue.P['user_id'].Value;
end;

procedure TYWM.GetHostInfo(const AHost: TYwmHost);
begin
  with YwmRequest do
    begin
      ActiveHost:=AHost;
      if not AHost.MainMirror then
        Resource:=Format('/user/%s/hosts/%s',[UserId, AHost.MainInfo.host_id])
      else
        Resource:=Format('/user/%s/hosts/%s',[UserId, AHost.Mirror.host_id]);
      Method:=TRESTRequestMethod.rmGET;
      OnAfterExecute:=DoAfterGetHostInfo;
      Execute;
    end;
end;

procedure TYWM.GetHosts;
begin
  with YwmRequest do
    begin
      Resource:=Format('/user/%s/hosts/',[UserId]);
      Method:=TRESTRequestMethod.rmGET;
      OnAfterExecute:=DoAfterGetHosts;
      Execute;
    end;
end;

procedure TYWM.GetHostSymmary(const AHost: TYwmHost);
begin
  ActiveHost:=AHost;
  with YwmRequest do
    begin
      if not AHost.MainMirror then
        Resource:=Format('/user/%s/hosts/%s/summary/',[UserId, AHost.MainInfo.host_id])
      else
        Resource:=Format('/user/%s/hosts/%s/summary/',[UserId, AHost.Mirror.host_id]);
      Method:=TRESTRequestMethod.rmGET;
      OnAfterExecute:=DoAfterGetHostSummary;
      Execute;
    end;
end;

procedure TYWM.GetUserID;
begin
  with YwmRequest do
    begin
      Resource:='user';
      Method:=TRESTRequestMethod.rmGET;
      OnAfterExecute:=DoAfterUserID;
      Execute;
    end;
end;

procedure TYWM.WebFormRedirect(const AURL: string; var DoCloseWebView: boolean);
var Params: TStringList;
begin
  if pos('code=',AURL)>0 then
    begin
      Params:=TStringList.Create;
      try
        ExtractGetParams(AURL, TStrings(Params));
        YWM.YwmAuth.AuthCode:=Params.Values['code'];
        YWM.YwmAuth.ChangeAuthCodeToAccesToken;
        DoCloseWebView:=True;
      finally
        FreeAndNil(Params)
      end;
    end;
end;

procedure TYWM.YandexAuth;
var frmAuth: Tfrm_OAuthWebForm;
begin
  frmAuth:=Tfrm_OAuthWebForm.Create(self);
  try
    frmAuth.OnAfterRedirect:=WebFormRedirect;
    frmAuth.Position:=poMainFormCenter;
    frmAuth.ShowModalWithURL(YWM.YwmAuth.AuthorizationRequestURI);
  finally
    Authorized:=not YwmAuth.AccessToken.IsEmpty;
    frmAuth.Release;
    if Authorized then
      GetUserID;
  end;
end;


procedure TYWM.YwmAuthAuthenticate(ARequest: TCustomRESTRequest;
  var ADone: Boolean);
begin
  YwmClient.AddAuthParameter('Authorization',
                             ' OAuth '+YwmAuth.AccessToken,
                             TRESTRequestParameterKind.pkHTTPHEADER,
                             [TRESTRequestParameterOption.poDoNotEncode]);
  ADone:=not YwmAuth.AccessToken.IsEmpty;
end;

procedure TYWM.YwmRequestHTTPProtocolError(Sender: TCustomRESTRequest);
begin

  raise Exception.Create(YwmResponse.Content);
end;

{ TYwmHost }

constructor TYwmHost.Create(const AJsonValue: TJSONValue);
begin
  inherited Create;
  FMainInfo:=TYwmHostInfo.Create;
  FMirror:=TYwmHostInfo.Create;
  Parse(AJsonValue)
end;

destructor TYwmHost.Destroy;
begin
  FreeAndNil(FMirror);
  FreeAndNil(FMainInfo);
  inherited;
end;

procedure TYwmHost.Parse(const AJsonValue: TJSONValue);
var AMirror: TJSONValue;
begin
  FMainInfo.host_id:=AJsonValue.P['host_id'].AsType<string>;
  FMainInfo.ascii_host_url:=AJsonValue.P['ascii_host_url'].AsType<string>;
  FMainInfo.unicode_host_url:=AJsonValue.P['unicode_host_url'].AsType<string>;
  FMainInfo.verified:=AJsonValue.P['verified'].AsType<boolean>;

  AMirror:=AJsonValue.P['main_mirror'].AsType<TJSONValue>;
  FMainMirror:=not AMirror.Null;
  if FMainMirror then
    begin
      Mirror.host_id:=AMirror.P['host_id'].AsType<string>;
      Mirror.ascii_host_url:=AMirror.P['ascii_host_url'].AsType<string>;
      Mirror.unicode_host_url:=AMirror.P['unicode_host_url'].AsType<string>;
      Mirror.verified:=AMirror.P['verified'].AsType<boolean>;
    end


end;

procedure TYwmHost.Setexcluded_pages_count(const Value: int64);
begin
  Fexcluded_pages_count := Value;
end;

procedure TYwmHost.Setsearchable_pages_count(const Value: int64);
begin
  Fsearchable_pages_count := Value;
end;

procedure TYwmHost.Setsite_problems(const Value: integer);
begin
  Fsite_problems := Value;
end;

procedure TYwmHost.Setsqi(const Value: integer);
begin
  Fsqi := Value;
end;

{ TYwmHostList }

constructor TYwmHostList.Create;
begin
  inherited Create(True);
  FList:=TObjectList<TYwmHost>.Create(False);
end;

destructor TYwmHostList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TYwmHostList.GetList: TObjectList<TYwmHost>;

function NeedShow(const HostID: string):boolean;
  var
    I: Integer;
begin
  Result:=True;
  for I := 0 to Pred(Count) do
    if (Items[i].MainMirror) and (Items[i].Mirror.host_id=HostID) then
      Exit(False);
end;

var
  I: Integer;
begin
  FList.Clear;
  for I := 0 to Pred(Count) do
    if NeedShow(Items[i].MainInfo.host_id) then
      FList.Add(Items[i]);
  Result:=FList;
end;

end.
