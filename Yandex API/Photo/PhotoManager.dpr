program PhotoManager;

uses
  Vcl.Forms,
  main in 'main.pas' {fmain},
  YandexMetrica in 'YandexMetrica.pas',
  uAuth in 'uAuth.pas' {fAuth};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tfmain, fmain);
  Application.CreateForm(TfAuth, fAuth);
  Application.Run;
end.
