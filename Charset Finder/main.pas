unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HTTPSend, StdCtrls, RegularExpressions;

const
  DefCharset =
    'не определена - надо использовать какую-либо дефолтную кодировку';

type
  TForm5 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Button1: TButton;
    Label3: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    function Download(URL: string): THTTPSend;
    function GetCharset(Headers: TStringList): string;
    function CharsetByMeta(Body: string): string;
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

procedure TForm5.Button1Click(Sender: TObject);
var
  HTTP: THTTPSend;
  Body: TstringStream;
begin
  Memo1.Lines.Clear;
  HTTP := Download(Edit1.Text);
  try
    if HTTP <> nil then
    begin
      Memo1.Lines.Add('Проверка кодировки для ' + Edit1.Text);
      if CheckBox1.Checked then
        Memo1.Lines.Add('Определение по заголовкам: ' +
          GetCharset(HTTP.Headers));
      if CheckBox2.Checked then
      begin
        HTTP.Document.Position := 0;
        Body := TstringStream.Create;
        try
          Body.LoadFromStream(HTTP.Document);
          Memo1.Lines.Add('Определение по мета-тегам: ' +
            CharsetByMeta(Body.DataString))
        finally
          Body.Free;
        end;
      end;
    end
    else
      Memo1.Lines.Add
        ('При скачивании страницы произошла ошибка - проверьте URL')
  finally
    HTTP.Free;
  end
end;

function TForm5.CharsetByMeta(Body: string): string;
const
  Pattern = '<meta.*content-type.*charset=(.*)".*/>';
var
  RegEx: TRegEx;
  M: TMatchCollection;
begin
  // <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
  RegEx := TRegEx.Create(Pattern, [roIgnoreCase, roMultiLine]);
  if RegEx.IsMatch(Body) then
  begin
    M:= RegEx.Matches(Body);
    Result := Trim(M[0].Groups[1].Value);
  end
  else
    Result := DefCharset;
end;

function TForm5.Download(URL: string): THTTPSend;
begin
  Result := THTTPSend.Create;
  if not Result.HTTPMethod('GET', URL) then
    Result.Free;


end;

function TForm5.GetCharset(Headers: TStringList): string;
var
  i: integer;
begin
  if Headers.Count = 0 then
    Exit;
  for i := 0 to Headers.Count - 1 do
  begin
    if Pos('content-type', LowerCase(Headers[i])) > 0 then
      if Pos('=', Headers[i]) > 0 then
        Result := LowerCase(Copy(Headers[i], Pos('=', Headers[i]) + 1,
          Length(Headers[i]) - Pos('=', Headers[i])))
      else
        Result := DefCharset;
  end;
end;

end.
