unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, httpsend, ExtDlgs, MimeFinder;

type
  TForm8 = class(TForm)
    Label1: TLabel;
    edFile: TEdit;
    btnFile: TButton;
    Label2: TLabel;
    edMime: TEdit;
    edBoundary: TEdit;
    Label3: TLabel;
    btnBoundary: TButton;
    btnSend: TButton;
    memText: TMemo;
    dlgOpen: TOpenPictureDialog;
    procedure btnBoundaryClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
  private
    function GetBoundary: string;
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

procedure TForm8.btnBoundaryClick(Sender: TObject);
begin
  edBoundary.Text := GetBoundary
end;

procedure TForm8.btnFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    edFile.Text := dlgOpen.FileName;
    edMime.Text := MimeTypeByExt(edFile.Text)
  end;
end;

procedure TForm8.btnSendClick(Sender: TObject);
Const
  CRLF = #$0a + #$0d;
var
  HTTP: THTTPSend;
  s: AnsiString;
  FS: TFileStream;
begin
  try
    HTTP := THTTPSend.Create;
    FS := TFileStream.Create(edFile.Text, fmOpenRead);
    {Определяем Contetn-Type запроса}
    HTTP.MimeType := 'multipart/form-data; boundary='+edBoundary.Text;
    { Записываем Mime-тип и данные по файлу }
    s := '--'+edBoundary.Text + CRLF +
      'Content-Disposition: form-data; name="userfile[]"; filename="'+ExtractFileName(FS.FileName)+'"'
      + CRLF + 'Content-Type: '+edMime.Text + CRLF + CRLF;
    HTTP.Document.Write(PAnsiChar(s)^, Length(s));
    FS.Position := 0;
    // записываем файл в тело документа
    HTTP.Document.CopyFrom(FS, FS.Size);
    { завершаем тело запроса }
    s := CRLF + '--'+edBoundary.Text+'--' + CRLF;
    HTTP.Document.Write(PAnsiChar(s)^, Length(s)); // завершили тело документа
    // Отправляем запрос
    if HTTP.HTTPMethod('POST', 'http://imagevenue.com/upload.php') then
      memText.Lines.LoadFromStream(HTTP.Document);
  finally
    FS.Free;
    HTTP.Free;
  end;
end;

function TForm8.GetBoundary: string;
begin
  Result := IntToHex(Random(MaxInt), 8) + '_webdelphi';
end;

end.
