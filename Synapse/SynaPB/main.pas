unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, httpsend, blcksock, synautil;

type
  TfMain = class(TForm)
    Label1: TLabel;
    urlEdit: TEdit;
    btnGet: TButton;
    pbFile: TProgressBar;
    lbProgress: TLabel;
    procedure btnGetClick(Sender: TObject);
  private
    Download: int64;
    procedure SynaProgress(Sender: TObject; Reason: THookSocketReason;
    const Value: String);
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

procedure TfMain.btnGetClick(Sender: TObject);
var HTTP: THTTPSend;
    MaxSize: int64;
begin
  Download:=0;
  MaxSize:=GetSize2(urlEdit.Text);
  if MaxSize>0 then
    pbFile.Max:=MaxSize
  else
    pbFile.Max:=0;
  HTTP:=THTTPSend.Create;
  HTTP.Sock.OnStatus:=SynaProgress;
  try
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
