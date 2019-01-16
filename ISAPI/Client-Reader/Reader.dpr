program Reader;

uses
  Vcl.Forms,
  main in 'main.pas' {Form19};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm19, Form19);
  Application.Run;
end.
