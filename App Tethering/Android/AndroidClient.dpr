program AndroidClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
