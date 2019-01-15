program syna_multipart;

uses
  Forms,
  main in 'main.pas' {Form8},
  MimeFinder in 'MimeFinder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
