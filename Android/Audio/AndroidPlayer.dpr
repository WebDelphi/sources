program AndroidPlayer;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {Form6};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
