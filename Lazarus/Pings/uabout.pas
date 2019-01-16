unit uabout; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type

  { Tfabout }

  Tfabout = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure BitBtn1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fabout: Tfabout;

implementation

{ Tfabout }

procedure Tfabout.BitBtn1Click(Sender: TObject);
begin
  ModalResult:=mrOK;
  Hide;
end;

initialization
  {$I uabout.lrs}

end.

