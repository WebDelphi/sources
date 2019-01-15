unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, httpsend, blcksock, synautil;

type
  TMemory = pointer;

type
  TfMain = class(TForm)
    Label1: TLabel;
    urlEdit: TEdit;
    btnGet: TButton;
    pbFile: TProgressBar;
    lbProgress: TLabel;
    Label2: TLabel;
    lbElapsed: TLabel;
    Label3: TLabel;
    lbTime: TLabel;
    procedure btnGetClick(Sender: TObject);
  private
    Download: int64;
    DocSize: int64;
    StartTick: int64;
    ElapsedTime: integer;
    procedure SynaProgress(Sender: TObject; Reason: THookSocketReason;
    const Value: String);
    procedure MyMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
    function GetSize(URL: string): int64;
    function GetSize2(URL: string): int64;

  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

function TfMain.GetSize(URL: string): int64;
var i:integer;
    size:string;
    ch:char;
begin
  Result:=-1;
  with THTTPSend.Create do
  if HTTPMethod('HEAD',URL) then
    begin
      for I := 0 to Headers.Count - 1 do
        begin
          if pos('content-length',lowercase(Headers[i]))>0 then
            begin
              size:='';
              for ch in Headers[i]do
                if ch in ['0'..'9'] then
                   size:=size+ch;
              Result:=StrToInt(size)+Length(Headers.Text);
              break;
            end;
        end;
    end
end;

function TfMain.GetSize2(URL: string): int64;
var size:string;
begin
  Result:=-1;
  with THTTPSend.Create do
  if HTTPMethod('HEAD',URL) then
    begin
      HeadersToList(Headers);//приводим список заголовков к виду Название=Значение
      size:=Headers.Values['Content-Length'];
      Result:=StrToIntDef(size,-1);
      if Result>-1 then
         Result:=Result+Length(Headers.Text)
    end
end;

procedure TfMain.MyMonitor(Sender: TObject; Writing: Boolean;
  const Buffer: TMemory; Len: Integer);
begin
  if not Writing then
     begin
       ElapsedTime := TickDelta(StartTick, GetTick) div 1000;
       lbTime.Caption := IntToStr(Trunc(ElapsedTime * (DocSize - Download) / Download))+' c.';
       lbElapsed.Caption := IntToStr(ElapsedTime)+ 'c.';
     end;
end;

procedure TfMain.btnGetClick(Sender: TObject);
var HTTP: THTTPSend;
begin
  Download:=0;
  DocSize:=GetSize2(urlEdit.Text);

  if DocSize>0 then
    pbFile.Max:=DocSize
  else
    pbFile.Max:=0;
  HTTP:=THTTPSend.Create;
  HTTP.Sock.OnStatus:=SynaProgress;
  HTTP.Sock.OnMonitor:=MyMonitor;
  try
    StartTick:=GetTick;
    if HTTP.HTTPMethod('GET',urlEdit.Text) then
       HTTP.Document.SaveToFile(ExtractFilePath(Application.ExeName)+'file.dat');
  finally
    HTTP.Free;
  end;
end;

procedure TfMain.SynaProgress(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
begin
  if Reason=HR_ReadCount then
    begin
      Download:=Download+StrToInt(Value);
      if pbFile.Max>0 then
        begin
          pbFile.Position:=Download;
          lbProgress.Caption:=IntToStr(Trunc((Download/pbFile.Max)*100))+'%';
        end
      else
        lbProgress.Caption:=IntToStr(Download)+' b';
      Application.ProcessMessages;
    end
end;

end.
