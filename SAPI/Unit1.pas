unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActiveX,ComObj,SpeechLib_TLB, SAPIEngineLib_TLB, ComCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    VolumeLabel: TLabel;
    TrackBar2: TTrackBar;
    RateLabel: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
     gpIVTxt: TSpVoice;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var Txt:PChar;
    Pool: LongWord;
    ppToken:  ISpeechObjectToken;
    ppTokenCategory:  ISpObjectTokenCategory;
    Tokens: ISpeechObjectTokens;
    i:integer;
    s:cardinal;
    t:PWideChar;

voce: OLEVariant;
zzz:ISpeechObjectTokens;
begin
CoInitialize(0);
//try
//   voce := CreateOLEObject('SAPI.SpVoice');
//   OleError(voce.Speak('Hello', SVSFlagsAsync));
//except
//
//end;


//try
//Tokens:=CreateComObject(IID_IEnumSpObjectTokens) as IEnumSpObjectTokens;

//gpIVTxt := CreateComObject(CLASS_SpVoice) as ISpeechVoice;
//gpIVTxt.Voice:=ppToken as ISpeechObjectToken;
//OleError(gpIVTxt.Speak('Hello my friend', SVSFDefault));
//except
//
//end;

//CoInitialize(0);
//CoCreateInstance(CLASS_SpVoice, nil, CLSCTX_ALL, IID_ISpVoice, gpIVTxt);

zzz:=gpIVTxt.GetVoices('','');
//ShowMessage(IntToStr(zzz.Count));
gpIVTxt.Voice:=zzz.Item(0);
//ShowMessage(zzz.Item(1).GetDescription(0));
////T:=PWideChar(UTF8ToWideString(Edit1.Text));
//////try
////CoInitialize(0);
////Pool:=1;

//gpIVTxt.Rate:=StrToInt(Edit2.Text);
gpIVTxt.Speak(Edit1.Text,SVSFlagsAsync);
////except
////  ShowMessage('')
////end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 gpIVTxt:=TSpVoice.Create(nil);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  VolumeLabel.Caption:=IntToStr(TrackBar1.Position);
  gpIVTxt.Volume:=TrackBar1.Position;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  RateLabel.Caption:=IntToStr(TrackBar2.Position);
  gpIVTxt.Rate:=TrackBar2.Position;
end;

end.
