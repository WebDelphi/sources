program Writer;

uses
  Vcl.Forms,
  main in 'main.pas' {Form18},
  ServerMethodsUnit in 'ServerMethodsUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm18, Form18);
  Application.Run;
end.
