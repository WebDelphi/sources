unit uAuth;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw, Vcl.StdCtrls;

type
  TfAuth = class(TForm)
    WebBrowser1: TWebBrowser;
    procedure FormShow(Sender: TObject);
    procedure WebBrowser1NavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fAuth: TfAuth;

implementation

uses main;

{$R *.dfm}

procedure TfAuth.FormShow(Sender: TObject);
begin
  WebBrowser1.Navigate2(Format('https://oauth.yandex.ru/authorize?response_type=token&client_id=%s&display=popup',[вот_сюда_вставить_client_id]));
end;

procedure TfAuth.WebBrowser1NavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
var Params: TStringList;
begin
  if pos('access_token',URL)>0 then
    begin
      Params:=TStringList.Create;
      try
        Params.Delimiter:='&';
        Params.DelimitedText:=copy(URL,pos('#',URL)+1,Length(URL)-pos('#',URL));
        fMain.Token:=Params.Values['access_token'];
        ModalResult:=mrOk;
      finally
        Params.Free;
      end;
    end;
end;

end.
