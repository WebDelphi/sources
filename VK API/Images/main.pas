unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  REST.Client, REST.Authenticator.OAuth, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts,
  REST.Authenticator.OAuth.WebForm.FMX, REST.Types, FMX.Controls.Presentation;

НЕ ЗАБУДЬ ВСТАВИТЬ СВОИ ЗНАЧЕНИЯ!!!
const
  cAppID = 'сюда_вставить_App_id';
  cAppKey = 'сюда_вставить_AppKey';
  cEndPoint = 'https://oauth.vk.com/authorize';
  cScope = 'wall';
  cAPIVersion = '5.24';

type
  TForm1 = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    OAuth2Authenticator1: TOAuth2Authenticator;
    ListBox1: TListBox;
    ListBoxHeader1: TListBoxHeader;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure RESTRequest1AfterExecute(Sender: TCustomRESTRequest);
  private
    UserId: string;
    WebForm: Tfrm_OAuthWebForm;
    procedure AfterRedirect(const AURL: string; var DoCloseWebView : boolean);
    procedure Auth;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses DateUtils,REST.Types;

{$R *.fmx}

procedure TForm1.AfterRedirect(const AURL: string; var DoCloseWebView: boolean);
var i:integer;
    Str: string;
    Params: TStringList;
begin
  i:=pos('#access_token=',AURL);
  if (i>0) and (OAuth2Authenticator1.AccessToken=EmptyStr) then
    begin
      Str:=AURL;
      Delete(Str,1,i);
      Params:=TStringList.Create;
      try
        Params.Delimiter:='&';
        Params.DelimitedText:=Str;
        OAuth2Authenticator1.AccessToken:=Params.Values['access_token'];
        OAuth2Authenticator1.AccessTokenExpiry:=IncSecond(Now,StrToInt(Params.Values['expires_in'])-10);
        UserId:=Params.Values['user_id'];
        ListBox1.Items.Add(OAuth2Authenticator1.AccessToken);
      finally
        Params.Free;
      end;
      WebForm.Close;
    end;
end;

procedure TForm1.Auth;
begin
  OAuth2Authenticator1.AccessToken:=EmptyStr;
  OAuth2Authenticator1.ClientID:=cAppID;
  OAuth2Authenticator1.ClientSecret:=cAppKey;
  OAuth2Authenticator1.ResponseType:=TOAuth2ResponseType.rtTOKEN;
  OAuth2Authenticator1.AuthorizationEndpoint:=cEndPoint;
  OAuth2Authenticator1.Scope:=cScope;
  WebForm.ShowWithURL(OAuth2Authenticator1.AuthorizationRequestURI);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Auth;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  RESTRequest1.Resource:='account.setOffline';
  RESTRequest1.Method:=TRESTRequestMethod.rmGET;
  RESTRequest1.Execute;
end;

procedure TForm1.Button3Click(Sender: TObject);
var S: string;
begin
  S:=InputBox('Новое сообщение','Текст сообщения','Текст для проверки');
  RESTRequest1.Resource:='wall.post';
  RESTRequest1.Method:=TRESTRequestMethod.rmPOST;
  RESTRequest1.Params.Clear;
  with RESTRequest1.Params.AddItem do
    begin
      name:='v';
      Value:=cAPIVersion;
      Kind:=TRESTRequestParameterKind.pkGETorPOST;
      Options:=[poDoNotEncode];
    end;
  with RESTRequest1.Params.AddItem do
    begin
      name:='owner_id';
      Value:=UserId;
      Kind:=TRESTRequestParameterKind.pkGETorPOST;
      Options:=[poDoNotEncode];
    end;
  with RESTRequest1.Params.AddItem do
    begin
      name:='message';
      Value:=S;
      Kind:=TRESTRequestParameterKind.pkGETorPOST;
      Options:=[poDoNotEncode];
    end;
  RESTRequest1.Execute;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WebForm:=Tfrm_OAuthWebForm.Create(nil);
  WebForm.OnAfterRedirect:=AfterRedirect;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  WebForm.Free;
end;

procedure TForm1.RESTRequest1AfterExecute(Sender: TCustomRESTRequest);
begin
  ListBox1.Items.Add(RESTResponse1.Content);
end;

end.
