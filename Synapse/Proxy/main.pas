unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, httpsend, blcksock, ssl_openssl;

type
  TForm8 = class(TForm)
    Label1: TLabel;
    edURL: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    cbTypeProxy: TComboBox;
    Label4: TLabel;
    edProxyHost: TEdit;
    Label5: TLabel;
    edProxyPort: TEdit;
    btnGet: TButton;
    memText: TMemo;
    procedure btnGetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

procedure TForm8.btnGetClick(Sender: TObject);
var HTTP: THTTPSend;
begin
 HTTP:=THTTPSend.Create;
 try
   //настраиваем параметры прокси
   case cbTypeProxy.ItemIndex of
     0:begin
         HTTP.ProxyHost:=edProxyHost.Text;
         HTTP.ProxyPort:=edProxyPort.Text;
       end;
     1:HTTP.Sock.SocksType:=ST_Socks4;
     2:HTTP.Sock.SocksType:=ST_Socks5;
   end;
   if cbTypeProxy.ItemIndex>0 then
     begin
       HTTP.Sock.SocksIP:=edProxyHost.Text;
       HTTP.Sock.SocksPort:=edProxyPort.Text;
       HTTP.Sock.SocksResolver:=false;
     end;
   //отправляем запрос
   if HTTP.HTTPMethod('GET',edURL.Text) then
     begin
       memText.Clear;
       memText.Lines.LoadFromStream(HTTP.Document);
     end
   else
     ShowMessage('Ошибка выполнения запроса: '+IntToStr(HTTP.Sock.SocksLastError));
 finally
   HTTP.Free
 end;
end;

end.
