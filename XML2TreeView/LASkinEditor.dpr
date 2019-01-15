program LASkinEditor;

uses
  Forms,
  SEmain in 'SEmain.pas' {Form5},
  XMLTree in 'XMLTree.pas',
  LAseConst in 'LAseConst.pas',
  Skins in 'Skins.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
