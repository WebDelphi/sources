unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs;

type
  Tfmain = class(TForm)
    Button1: TButton;
    lbAuth: TLabel;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edLoadURL: TEdit;
    Label6: TLabel;
    btnPhoto1: TButton;
    lbPhoto1: TLabel;
    OpenDialog1: TOpenDialog;
    Label7: TLabel;
    edTitle: TEdit;
    edDescription: TEdit;
    edTags: TEdit;
    Button3: TButton;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lbLoadResult: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnPhoto1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private

  public
    Token: string;
  end;

var
  fmain: Tfmain;

implementation

uses ShellAPI, uAuth, httpsend, synautil;

{$R *.dfm}

procedure Tfmain.Button1Click(Sender: TObject);
begin
  fAuth.ShowModal;
  if fAuth.ModalResult=mrOk then
    lbAuth.Caption:=Token
end;

procedure Tfmain.Button2Click(Sender: TObject);
var Redirect: string;
begin
  with THTTPSend.Create do
    begin
      Headers.Add('Authorization: OAuth '+Token);
      if HTTPMethod('GET','http://api-fotki.yandex.ru/api/me/') then
        begin
          Headers.NameValueSeparator:=':';
          Redirect:=Headers.Values['Location'];
          if Length(Redirect)>0 then
            begin
              Clear;
              if HTTPMethod('GET',Redirect) then
                begin
                  Document.SaveToFile('ServiceDocument.xml');
                  label4.Caption:='Получен'
                end
              else
                raise Exception.Create('Не удалось получить сервисный документ');
            end
          else
            raise Exception.Create('Не удалось получить адрес сервисного документа');
        end
      else
        raise Exception.Create('Не удалось получить адрес сервисного документа');
    end;
end;

procedure Tfmain.Button3Click(Sender: TObject);
const CR = #$0d;
      LF = #$0a;
      CRLF = CR + LF;
var Bound,s: string;
    HTTP: THTTPSend;
    PicStream: TMemoryStream;
begin
 Bound := IntToHex(Random(MaxInt), 8) + '_PhotoManager_boundary';
 HTTP:=THTTPSend.Create;
 try
   HTTP.Headers.Add('Authorization: OAuth '+Token);
   HTTP.MimeType:='multipart/form-data;  boundary='+Bound;
   S:='--' + Bound + CRLF;
   s:=s+'Content-Disposition: form-data;  name="image"; filename="picture"' + CRLF;
   s:=s+'Content-Type: image/jpeg'+CRLF+CRLF;//тут надо бы определять Mime-тип картинки
   PicStream:=TMemoryStream.Create;
   try
     WriteStrToStream(HTTP.Document,AnsiToUtf8(s));
     PicStream.LoadFromFile(lbPhoto1.Caption);
     PicStream.Position:=0;
     HTTP.Document.CopyFrom(PicStream,0);

     S:=CRLF+CRLF+'--' + Bound + CRLF;
     S:=S+'Content-Disposition: form-data; name="title"'+CRLF+CRLF;
     S:=s+edTitle.Text+CRLF+'--'+Bound+CRLF;
     s:=s+'Content-Disposition: form-data; name="description"'+CRLF+CRLF;
     s:=s+edDescription.Text+CRLF+'--'+Bound+CRLF;
     s:=s+'Content-Disposition: form-data; name="tags"'+CRLF+CRLF;
     s:=s+edTags.Text;
     WriteStrToStream(HTTP.Document,AnsiToUtf8(s));

     HTTP.Document.SaveToFile('Request.txt');
     if HTTP.HTTPMethod('POST',edLoadURL.Text) then
       begin
         HTTP.Document.SaveToFile('Response.xml');
         lbLoadResult.Caption:='Загружено!';
       end
     else
       raise Exception.Create('Во время отправки фотки произошла ошибка');

   finally
     PicStream.Free;
   end;
 finally
   HTTP.Free;
 end;
end;

procedure Tfmain.btnPhoto1Click(Sender: TObject);
begin
if OpenDialog1.Execute then
  lbPhoto1.Caption:=OpenDialog1.FileName
end;

end.
