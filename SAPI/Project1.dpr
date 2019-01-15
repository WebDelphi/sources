program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  SpeechLib_TLB in 'C:\Users\Vlad\Documents\RAD Studio\7.0\Imports\SpeechLib_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
