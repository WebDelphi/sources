program SyntaxHighlight;

uses
  Vcl.Forms,
  main in 'main.pas' {Form4},
  uSyntax in '..\uSyntax.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
