unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, httpsend;

type
  TfMain = class(TForm)
    Label1: TLabel;
    edURL: TEdit;
    Label2: TLabel;
    edStart: TEdit;
    Label3: TLabel;
    edEnd: TEdit;
    Label4: TLabel;
    btnGet: TButton;
    Label5: TLabel;
    lbSize: TLabel;
    Label7: TLabel;
    memHeaders: TMemo;
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

procedure TfMain.btnGetClick(Sender: TObject);
var HTTP: THTTPSend;
begin
  HTTP:=THTTPSend.Create;
  try
    HTTP.RangeStart:=StrToIntDef(edStart.Text,0);
    HTTP.RangeEnd:=StrToIntDef(edEnd.Text,0);
    if HTTP.HTTPMethod('GET',edURL.Text) then
      begin
        lbSize.Caption:=IntToStr(HTTP.Document.Size);
        memHeaders.Clear;
        memHeaders.Lines.Assign(HTTP.Headers);
      end
    else
      ShowMessage(HTTP.ResultString);
  finally
    HTTP.Free;
  end;
end;

end.
