program pings;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, LResources, uSettings, uclean, uabout
  { you can add units after this };

{$IFDEF WINDOWS}{$R pings.rc}{$ENDIF}

begin
  {$I pings.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfClean, fClean);
  Application.CreateForm(Tfabout, fabout);
  Application.Run;
end.

