program FMXSQlite;

uses
  FMX.Forms,
  main in 'main.pas' {Form13};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm13, Form13);
  Application.Run;
end.
