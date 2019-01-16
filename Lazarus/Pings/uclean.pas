unit uclean;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, Buttons, uSettings,main;

type

  { TfClean }

  TfClean = class(TForm)
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  public

  end;


var
  fClean: TfClean;

implementation

{ TfClean }

procedure TfClean.BitBtn1Click(Sender: TObject);
var All, Deleted:integer;
begin
 PingService.CleanFilter.ByCount:=CheckBox1.Checked;
 PingService.CleanFilter.ByCode:=CheckBox2.Checked;
 PingService.CleanFilter.Count:=SpinEdit1.Value;
 PingService.CleanFilter.Code:=StrToInt(Edit1.Text);
 label2.Caption:=IntToStr(PingService.Services.Count);
 label4.Caption:=IntToStr(PingService.CleanServices);
 label6.Caption:=IntToStr(strToInt(label2.Caption)-StrToInt(label4.Caption));
 Form1.CheckListBox1.Items.Assign(PingService.Services);
 Form1.Label18.Caption:=IntToStr(Form1.CheckListBox1.Items.Count);
end;

procedure TfClean.CheckBox1Change(Sender: TObject);
begin
  SpinEdit1.Enabled:=CheckBox1.Checked;
end;

procedure TfClean.CheckBox2Change(Sender: TObject);
begin
  Edit1.Enabled:=CheckBox1.Checked;
end;

procedure TfClean.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ModalResult:=mrOK;
end;


initialization
  {$I uclean.lrs}

end.

