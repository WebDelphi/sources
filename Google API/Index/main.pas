unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SuperObject, HTTPSend;

const
  cBaseURL = 'http://www.google.com/uds/GwebSearch?v=1.0&q=info:%s';

type
  TOnCheck = procedure (InIndex:boolean;URL: string)of object;

type
  TChecker = class(TThread)
  private
    FURls: TStringList;
    FOnCheck:TOnCheck;
    function Indexed(URL:string):boolean;
    procedure ClearLine;
  protected
     procedure Execute; override;
  public
    constructor Create(Suspended: boolean; aURLs: TStrings);
    property OnCheck: TOnCheck read FOnCheck write FOnCheck;
  end;

type
  TForm3 = class(TForm)
    GroupBox1: TGroupBox;
    Splitter1: TSplitter;
    GroupBox2: TGroupBox;
    Splitter2: TSplitter;
    GroupBox3: TGroupBox;
    Memo2: TMemo;
    Memo3: TMemo;
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Checked(InIndex:boolean;URL: string);
  end;

var
  Form3: TForm3;

implementation

uses Clipbrd;

{$R *.dfm}

{ TChecker }

procedure TChecker.ClearLine;
begin
  Form3.ListBox1.Items.Delete(Form3.ListBox1.Items.Count-1);
end;

constructor TChecker.Create(Suspended: boolean; aURLs: TStrings);
begin
  inherited Create(Suspended);
  FURls:=TStringList.Create;
  if aURLs<>nil then
    FURls.Assign(aURLs);
end;

procedure TChecker.Execute;
var i:integer;
begin
  for i:=FURls.Count-1 downto 0 do
    begin
      if Assigned(FOnCheck) then
        OnCheck(Indexed(FURls[i]),FURls[i]);
      FURls.Delete(i);
      Synchronize(ClearLine);
    end;
end;

function TChecker.Indexed(URL: string): boolean;
var Ob: ISuperObject;
    Stream: TStringStream;
begin
  Stream:=TStringStream.Create;
try
  with THTTPSend.Create do
    begin
      if HTTPMethod('GET',Format(cBaseURL,[URL])) then
        begin
          Stream.LoadFromStream(Document);
          Ob:=SO(Stream.DataString);
          if Ob.I['responseStatus']=200 then
             Result:=Ob.A['responseData.results'].Length>0
          else
            Result:=false;
        end
      else
        Result:=false;
    end;
finally
  Stream.Free;
end;
end;

procedure TForm3.Button1Click(Sender: TObject);
var Thread: TChecker;
begin
  Memo2.Clear;
  Memo3.Clear;
  Thread:=TChecker.Create(true,ListBox1.Items);
  Thread.FreeOnTerminate:=true;
  Thread.OnCheck:=Checked;
  Thread.Start;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    ListBox1.Items.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm3.Button3Click(Sender: TObject);
var S: string;
begin
  if InputQuery('Введите URL','URL',S) then
    begin
      if Length(Trim(S))>0 then
        ListBox1.Items.Add(S)
      else
        Button3Click(self);
    end;
end;

procedure TForm3.Checked(InIndex: boolean; URL: string);
begin
  if InIndex then
    Memo2.Lines.Add(URL)
  else
    Memo3.Lines.Add(URL)
end;

end.
