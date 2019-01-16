unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ispdb, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    edMail: TEdit;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    cbInProtocol: TComboBox;
    Label3: TLabel;
    edInAddress: TEdit;
    Label4: TLabel;
    edInPrort: TEdit;
    cbInConnect: TComboBox;
    Label5: TLabel;
    edInUser: TEdit;
    cbOutConnect: TComboBox;
    edOutPort: TEdit;
    Label6: TLabel;
    edOutAddress: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    edOutUser: TEdit;
    Label9: TLabel;
    edOutAuth: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure cbInProtocolChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbOutConnectChange(Sender: TObject);
  private
    ISPDB: TISPDB;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure TForm1.Button1Click(Sender: TObject);
begin
  if ISPDB.FindOptions(edMail.Text) then
    begin
      StatusBar1.Panels[1].Text:=ISPDB.Servers.Count.ToString;
      ShowMessage('Найдены настройки почтового аккаунта'#13#10'Выберите желаемый протокол и тип соединения в списках ниже');
    end
  else
    begin
      StatusBar1.Panels[1].Text:='0';
      ShowMessage('Настроек почтового аккаунта не найдено в ISPDB');
    end;
end;

procedure TForm1.cbInProtocolChange(Sender: TObject);
var AServerType: TServerType;
    ASocket: TSocketType;
    AOptions: TServerOptions;
begin
  edInAddress.Text:=EmptyStr;
  edInPrort.Text:=EmptyStr;
  edInUser.Text:=EmptyStr;
  if cbInProtocol.ItemIndex=0 then
    AServerType:=TServerType.stPOP
  else
    AServerType:=TServerType.stIMAP;
  case cbInConnect.ItemIndex of
    0:ASocket:=sSSL;
    1:ASocket:=sSTARTTLS;
    2:ASocket:=sPlain;
  end;
  AOptions:=ISPDB.FindServer(AServerType,ASocket);
  if Assigned(AOptions) then
    begin
      edInAddress.Text:=AOptions.HostName;
      edInPrort.Text:=AOptions.Port.ToString;
      case AOptions.UserName of
        unAddress: edInUser.Text:=edMail.Text;
        unLocalPart: edInUser.Text:=copy(edMail.Text,0,pos('@',edMail.Text)-1);
        unDomain: edInUser.Text:=ISPDB.GetDomain(edMail.Text);
      end;
    end
 else
   raise Exception.Create('Настроек сервера входящей почты с заданными параметрами не найдено');
end;

procedure TForm1.cbOutConnectChange(Sender: TObject);
var ASocket: TSocketType;
    AOptions: TServerOptions;
begin
  edOutPort.Text:=EmptyStr;
  edOutAddress.Text:=EmptyStr;
  edOutUser.Text:=EmptyStr;
  edOutAuth.Text:=EmptyStr;

  case cbOutConnect.ItemIndex of
    0:ASocket:=sSSL;
    1:ASocket:=sSTARTTLS;
    2:ASocket:=sPlain;
  end;
  AOptions:=ISPDB.FindServer(TServerType.stSMTP,ASocket);
  if Assigned(AOptions) then
    begin
      edOutAddress.Text:=AOptions.HostName;
      edOutPort.Text:=AOptions.Port.ToString;
      case AOptions.UserName of
        unAddress: edOutUser.Text:=edMail.Text;
        unLocalPart: edOutUser.Text:=copy(edMail.Text,0,pos('@',edMail.Text)-1);
        unDomain: edOutUser.Text:=ISPDB.GetDomain(edMail.Text);
      end;
      case AOptions.Authentication of
        ptClearText: edOutAuth.Text:='password -cleartext';
        ptCRAMMD5: edOutAuth.Text:='CRAMMD5';
        ptOther: edOutAuth.Text:='other';
      end;
    end
 else
   raise Exception.Create('Настроек сервера исходящей почты с заданными параметрами не найдено');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 ISPDB:=TISPDB.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ISPDB.Free;
end;

initialization
  ReportMemoryLeaksOnShutdown:=True;

end.
