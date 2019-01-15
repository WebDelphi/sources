program DownLoaderTest;

uses
  Forms,
  DownLoaderTestUnit in 'DownLoaderTestUnit.pas' {Form11},
  DownLoader_unit in '..\DownLoader_unit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm11, Form11);
  Application.Run;
end.
