unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MSHTML, ActiveX, hTTPSend;

type
  Tfmain = class(TForm)
    Label1: TLabel;
    edURL: TEdit;
    btnGet: TButton;
    Label2: TLabel;
    ListTags: TListBox;
    procedure btnGetClick(Sender: TObject);
  private
    function GetDOM(URL: string): IHTMLDocument2;
  public
    { Public declarations }
  end;

var
  fmain: Tfmain;

implementation

{$R *.dfm}

procedure Tfmain.btnGetClick(Sender: TObject);
var
  DOC: IHTMLDocument2;
  TagCollection: IHTMLElementCollection;
  Tag: IHTMLMetaElement;
  i:integer;
begin
  DOC := GetDOM(edURL.Text);
  if DOC=nil then Exit;
  ListTags.Clear;
  TagCollection:=DOC.all.tags('meta')as IHTMLElementCollection;
  for I := 0 to TagCollection.length-1 do
    begin
      Tag:=TagCollection.item(i,0) as IHTMLMetaElement;
      ListTags.Items.Add(Tag.content);
      {проверяем атрибут http-equiv}
      if LowerCase(Tag.httpEquiv)='content-type' then
        ListTags.Items.Add('Тег с кодировкой найден. Значение content = '+Tag.content)
    end;
end;

function Tfmain.GetDOM(URL: string): IHTMLDocument2;
var
  V: OleVariant;
  Content: TStringStream;
begin
  with THTTPSend.Create do
  begin
    if HTTPMethod('GET', URL) then
    begin
      Content := TStringStream.Create;
      Result:=CoHTMLDocument.Create as IHTMLDocument2;
      try
        Content.LoadFromStream(Document);
        V := VarArrayCreate([0, 0], varVariant);
        V[0] := Content.DataString;
        Result.write(PSafeArray(TVarData(V).VArray))
      finally
        Content.Free
      end;
    end;
  end;
end;

end.
