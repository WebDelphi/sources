program metatags;

uses
  Forms,
  main in 'main.pas' {fmain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tfmain, fmain);
  Application.Run;
end.
