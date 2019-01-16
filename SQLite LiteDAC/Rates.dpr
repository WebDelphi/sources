program Rates;

uses
  Vcl.Forms,
  main in 'main.pas' {Form3},
  cbrf in 'cbrf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
