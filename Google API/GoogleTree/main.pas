unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uGoogleLogin, StdCtrls, GoogleDocument, ComCtrls, GoogleTree,
  ExtCtrls;

type
  TfMain = class(TForm)
    GoogleLogin1: TGoogleLogin;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edEmail: TEdit;
    Label2: TLabel;
    edPass: TEdit;
    btnLogin: TButton;
    GroupBox2: TGroupBox;
    lbDocCount: TLabel;
    Label3: TLabel;
    btnGetList: TButton;
    btnTree: TButton;
    rgTreeMode: TRadioGroup;
    Panel1: TPanel;
    GoogleVirtualTree1: TGoogleVirtualTree;
    Splitter1: TSplitter;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lbDocType: TLabel;
    lbTitle: TLabel;
    lbID: TLabel;
    lbIDParent: TLabel;
    procedure btnLoginClick(Sender: TObject);
    procedure GoogleLogin1Autorization(const LoginResult: TLoginResult;
      Result: TResultRec);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetListClick(Sender: TObject);
    procedure btnTreeClick(Sender: TObject);
    procedure rgTreeModeClick(Sender: TObject);
    procedure GoogleVirtualTree1Click(Sender: TObject);

  private
    FGoogleDocs: TGoogleDocList;
    FRequest: TGoogleRequest;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

resourcestring
  rsOk = 'Авторизация прошла успешно';
  rsFail = 'Авторизация не удалась. Проверьте логин и пароль';

{$R *.dfm}

procedure TfMain.btnLoginClick(Sender: TObject);
begin
  GoogleLogin1.Email := edEmail.Text;
  GoogleLogin1.Password := edPass.Text;
  GoogleLogin1.Login();
end;

procedure TfMain.btnGetListClick(Sender: TObject);
var
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    FRequest.GET
      ('https://docs.google.com/feeds/default/private/full?showfolders=true',
      Stream);
    FGoogleDocs.FillFromXML(Stream);
    lbDocCount.Caption := IntToStr(FGoogleDocs.Count);
  finally
    Stream.Free;
  end;
end;

procedure TfMain.btnTreeClick(Sender: TObject);
begin
  GoogleVirtualTree1.AssignDocuments(FGoogleDocs);
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  FGoogleDocs := TGoogleDocList.Create;
  FRequest := TGoogleRequest.Create;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FGoogleDocs.Destroy;
  FRequest.Destroy;
end;

procedure TfMain.GoogleLogin1Autorization(const LoginResult: TLoginResult;
  Result: TResultRec);
begin
  if LoginResult = lrOk then
  begin
    FRequest.AuthKey := Result.Auth;
    ShowMessage(rsOk);
  end
  else
    raise Exception.Create(rsFail);
end;

procedure TfMain.GoogleVirtualTree1Click(Sender: TObject);
begin
  if GoogleVirtualTree1.Selected <> nil then
  begin
    with TGoogleDoc(GoogleVirtualTree1.Selected.Data) do
    begin
      case DocType of
        dtUnknown:
          lbDocType.Caption := 'Неизвестен';
        dtDocument:
          lbDocType.Caption := 'Документ';
        dtPdf:
          lbDocType.Caption := 'PDF';
        dtFolder:
          lbDocType.Caption := 'Коллекция';
        dtPresentation:
          lbDocType.Caption := 'Презентация';
        dtSpreadsheet:
          lbDocType.Caption := 'Таблица';
        dtForm:
          lbDocType.Caption := 'Форма';
        dtFile:
          lbDocType.Caption := 'Файл';
        dtDrawing:
          lbDocType.Caption := 'Картинка';
      end;
      lbTitle.Caption:=Title;
      lbID.Caption:=ID;
      lbIDParent.Caption:=ParentID;
    end;
  end;
end;

procedure TfMain.rgTreeModeClick(Sender: TObject);
begin
  case rgTreeMode.ItemIndex of
    0:
      GoogleVirtualTree1.Mode := mFiles;
    1:
      GoogleVirtualTree1.Mode := mFolders;
    2:
      GoogleVirtualTree1.Mode := mBoth;
  end;
end;

end.
