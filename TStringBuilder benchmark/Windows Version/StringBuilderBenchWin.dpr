program StringBuilderBenchWin;

uses
  Vcl.Forms,
  main in 'L:\GitCopy\HelloWorld\main.pas' {Form6};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
