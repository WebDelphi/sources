unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, httpsend, ssl_openssl, superobject, AdvSmoothGauge;

const
  cURL = 'https://www.googleapis.com/pagespeedonline/v1/runPagespeed?url=%s&key=%s&callback=response';

type
  TfMain = class(TForm)
    Label1: TLabel;
    edURL: TEdit;
    btnGet: TButton;
    Label2: TLabel;
    edTestUrl: TEdit;
    Label3: TLabel;
    memolog: TMemo;
    Label4: TLabel;
    lbScore: TLabel;
    procedure btnGetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure response(const This, Params: ISuperObject; var Result: ISuperObject);
begin
  case Params.I['responseCode'] of
    200:
      begin
        with fMain do
        begin
          memolog.Lines.Add('Количество ресурсов ' + Params.S
            ['pageStats.numberResources']);
          memolog.Lines.Add('Количество хостов ' + Params.S
            ['pageStats.numberHosts']);
          lbScore.Caption := Params.S['score'];
        end;
      end
  else
    ShowMessage('Ошибка');
  end;
end;

procedure TfMain.btnGetClick(Sender: TObject);
var
  Data: TStringStream;
  o: ISuperObject;
begin
  with THTTPSend.Create do
  begin
    if HTTPMethod('GET', Format(cURL, [edTestUrl.Text, edURL.Text])) then
    begin
      Data := TStringStream.Create;
      try
        Data.LoadFromStream(Document);
        o := so;
        o.M['response'] := response;
        o[Data.DataString];
      finally
        Data.Free
      end;
    end;
  end;
end;

end.
