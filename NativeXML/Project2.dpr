program Project2;

uses
  Forms,
  Native in 'Native.pas' {Form3},
  NativeXml in 'NativeXml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
