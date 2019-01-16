program ISPDB_App;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  ISPDB in 'ISPDB.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Light');
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
