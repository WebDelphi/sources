unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TForm6 = class(TForm)
    Label1: TLabel;
    edURL: TEdit;
    chGzip: TCheckBox;
    btnGet: TButton;
    Label2: TLabel;
    chDeflate: TCheckBox;
    chBrotli: TCheckBox;
    chAny: TCheckBox;
    Label3: TLabel;
    edAcceptEncoding: TEdit;
    memContent: TMemo;
    NetHTTPClient1: TNetHTTPClient;
    memHeaders: TMemo;
    procedure btnGetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.btnGetClick(Sender: TObject);
var AResponse: IHTTPResponse;
    ADecompMethods: THTTPCompressionMethods;
    AHeader: TNetHeader;
begin
  ADecompMethods:=[];
  memContent.Lines.Clear;
  memHeaders.Lines.Clear;
  NetHTTPClient1.AcceptEncoding:=edAcceptEncoding.Text;
  NetHTTPClient1.AutomaticDecompression:=[THTTPCompressionMethod.Brotli];

  if chGzip.Checked then
    ADecompMethods:=ADecompMethods+[THTTPCompressionMethod.GZip];
  if chDeflate.Checked then
    ADecompMethods:=ADecompMethods+[THTTPCompressionMethod.Deflate];
  if chBrotli.Checked then
    ADecompMethods:=ADecompMethods+[THTTPCompressionMethod.Brotli];
  if chAny.Checked then
    ADecompMethods:=ADecompMethods+[THTTPCompressionMethod.Any];

  NetHTTPClient1.AutomaticDecompression:=ADecompMethods;

  AResponse:=NetHTTPClient1.Get(edURL.Text);
  memContent.Lines.LoadFromStream(AResponse.ContentStream);
  for AHeader in AResponse.Headers do
    memHeaders.Lines.Add(AHeader.Name+': '+AHeader.Value)

end;

end.
