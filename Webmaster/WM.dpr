program WM;

uses
  Vcl.Forms,
  main in 'main.pas' {Form4},
  Vcl.Themes,
  Vcl.Styles,
  uYwmApi in 'uYwmApi.pas' {YWM: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Light');
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TYWM, YWM);
  Application.Run;
end.
