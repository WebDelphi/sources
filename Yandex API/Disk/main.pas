unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, httpsend, synacode, ssl_openssl, synautil, httpapp;

const
  cWebDAVServer = 'https://webdav.yandex.ru/';

type
  TWebDAVSend = class
  private
    FHTTP: THTTPSend;
    FToken: AnsiString;
    FPassword: string;
    FLogin: string;
    procedure SetLogin(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetToken;
    function EncodeUTF8URI(const URI: string): string;
    function GetRequestURL(const Element: string):string;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Получение свойств каталога или файла.
    /// </summary>
    /// <param name="Depth">
    /// 0 — запрашиваются свойства файла или каталога, непосредственно
    /// указанного в запросе. 1 — запрашиваются свойства каталога, а также
    /// всех элементов, находящихся на первом уровне каталога.
    /// </param>
    /// <param name="Element">
    /// Файл или каталог для которого необходимо получить свойства.
    /// </param>
    /// <returns>
    /// XML-документ, содержащий запрошенные свойства
    /// </returns>
    /// <remarks>
    /// Если Element не определен, то возвращаются свойства корневого каталога
    /// </remarks>
    function PROPFIND(Depth: integer; const Element: String): string;

    ///	<summary>
    ///	  Создание нового каталога на сервере
    ///	</summary>
    ///	<param name="ElementPath">
    ///	  путь к новому каталогу, включая его имя. Согласно протоколу, в
    ///	  результате одного запроса может быть создан только один каталог. Если
    ///	  приложение отправляет запрос о создании каталога a/b/c/, а в каталоге
    ///	  a/ нет каталога b/, то сервис не создает каталог b/, а отвечает c
    ///	  кодом 409 Conflict.
    ///	</param>
    ///	<returns>
    ///	  True - если каталог создан успешно
    ///	</returns>
    function MKCOL(const ElementPath: string):boolean;

    property Login: string read FLogin write SetLogin;
    property Password: string read FPassword write SetPassword;
  end;

type
  TForm5 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Memo1: TMemo;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Button1: TButton;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;
  WebDAV: TWebDAVSend;

implementation

resourcestring
  rsPropfindError = 'Ошибка при выполнении запроса PROPFIND';

{$R *.dfm}

procedure TForm5.Button1Click(Sender: TObject);
begin
  WebDAV.Login := Edit1.Text;
  WebDAV.Password := Edit2.Text;
  Memo1.Lines.Clear;
  case ComboBox1.ItemIndex of
    0:Memo1.Lines.Add(WebDAV.PROPFIND(1, InputBox('Ресурс', 'Ресурс', '')));
    2:WebDAV.MKCOL(InputBox('Новый Каталог', 'Новый Каталог', ''));
  end;
end;

{ TWebDAVSend }

constructor TWebDAVSend.Create;
begin
  inherited;
  FHTTP := THTTPSend.Create;
end;

destructor TWebDAVSend.Destroy;
begin
  FHTTP.Free;
  inherited;
end;

function TWebDAVSend.EncodeUTF8URI(const URI: string): string;
var
  i: integer;
  Char: AnsiChar;
  Test: string;
begin
  Test:=EncodeURL(URI);
  OutputDebugString(PChar(Test));

  result := '';
  for i := 1 to length(URI) do
  begin
    if not(URI[i] in URLFullSpecialChar) then
      begin
      for Char in UTF8String(URI[i]) do
        Result:=Result+'%'+IntToHex(Ord(Char), 2)
      end
    else
      Result:=Result+URI[i];
  end;
OutputDebugString(PChar(Result));
end;

function TWebDAVSend.GetRequestURL(const Element: string): string;
var URI: string;
begin
  if Length(Element)>0 then
    begin
      URI:=Element;
      if URI[1]='/' then
        Delete(URI,1,1);
      Result:=cWebDAVServer+EncodeUTF8URI(URI);
    end
  else
   Result:=cWebDAVServer;
end;

function TWebDAVSend.MKCOL(const ElementPath: string): boolean;
begin
  Result:=False;
  with FHTTP do
  begin
    Headers.Clear;
    Document.Clear;
    Headers.Add('Authorization: Basic ' + FToken);
    Headers.Add('Accept: */*');
    if HTTPMethod('MKCOL', GetRequestURL(ElementPath)) then
      begin
        Result:=ResultCode=201;
        if not Result then
          raise Exception.Create(IntToStr(ResultCode)+' '+ResultString);
      end
    else
      raise Exception.Create(rsPropfindError+' '+ResultString);
  end;

end;

function TWebDAVSend.PROPFIND(Depth: integer; const Element: String): string;
begin
  with FHTTP do
  begin
    Headers.Clear;
    Document.Clear;
    Headers.Add('Authorization: Basic ' + FToken);
    Headers.Add('Depth: ' + IntToStr(Depth));
    Headers.Add('Accept: */*');
    if HTTPMethod('PROPFIND', GetRequestURL(Element)) then
      result := ReadStrFromStream(Document, Document.Size)
    else
      raise Exception.Create(rsPropfindError+' '+ResultString);
  end;
end;

procedure TWebDAVSend.SetToken;
begin
  FToken := EncodeBase64(FLogin + ':' + FPassword);
end;

procedure TWebDAVSend.SetLogin(const Value: string);
begin
  FLogin := Value;
  SetToken;
end;

procedure TWebDAVSend.SetPassword(const Value: string);
begin
  FPassword := Value;
  SetToken;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  WebDAV := TWebDAVSend.Create;
end;

procedure TForm5.FormDestroy(Sender: TObject);
begin
  WebDAV.Free;
end;

end.
