unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus, CheckLst, myrss,uSettings, XMLWrite;

type
  { TForm1 }
  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
    { private declarations }
  public
    RSSLink: string;
    RSSFeed: TFeedChanel;

    procedure RefreshFeed;
    procedure GetWorkList;
  end; 

var
  Form1: TForm1; 
  PingService:TPingService;
implementation

uses uclean, uabout;

{ TForm1 }

procedure TForm1.MenuItem7Click(Sender: TObject);
var F:TextFile;
begin
  if FileExists(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'settings.cfg')then
    begin
      AssignFile(F,IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'settings.cfg');
      Reset(F);
      Read(F,RSSLink);
      Rewrite(F);
      if InputQuery('Введите адрес RSS-канала', 'RSS-канал', RSSLink) then
        begin
          Write(F,RSSLink);
          RefreshFeed;
        end;
      CloseFile(F);
    end
  else
    begin
      if not DirectoryExists(IncludeTrailingPathDelimiter(GetAppConfigDir(false)))then
         CreateDir(IncludeTrailingPathDelimiter(GetAppConfigDir(false)));
      AssignFile(F,IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'settings.cfg');
      Rewrite(F);
      if InputQuery('Введите адрес RSS-канала', 'RSS-канал', RSSLink) then
        begin
          Write(F,RSSLink);
          RefreshFeed;
        end;
      CloseFile(F)
    end;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  PingService.DeleteAllServices;
  CheckListBox1.Items.Assign(PingService.Services);
  label18.Caption:=IntToStr(CheckListBox1.Items.Count)
end;

procedure TForm1.RefreshFeed;
var i:integer;
begin
ComboBox1.Items.Clear;
  if Assigned(RSSFeed) then
    begin
      if Length(RSSLink)>0 then
        begin
      RSSFeed.FeedAddress:=RSSLink;
      for i:=0 to RSSFeed.ItemsCount-1 do
         ComboBox1.Items.Add(RSSFeed.Title[i]);
      if ComboBox1.Items.Count>0 then
        begin
          ComboBox1.ItemIndex:=0;
          ComboBox1Change(self);
        end;
        end
      else
        MenuItem7Click(self)
    end
  else
    if Length(RSSLink)>0 then
      begin
        RSSFeed:=TFeedChanel.Create(RSSLink);
        RefreshFeed;
      end
    else
      Exit;
end;

procedure TForm1.GetWorkList;
var i:integer;
begin
PingService.WorkList.Clear;
  for i:=0 to CheckListBox1.Items.Count-1 do
    begin
      if CheckListBox1.Checked[i] then
        PingService.WorkList.Add(CheckListBox1.Items.Strings[i]);
    end;
end;

procedure TForm1.FormShow(Sender: TObject);
var F:TextFile;
begin
if FileExists(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'settings.cfg')then
    begin
      AssignFile(F,IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'settings.cfg');
      Reset(F);
      Read(F,RSSLink);
      CloseFile(F);
      RSSFeed:=TFeedChanel.Create(RSSLink);
    end
else
  RSSLink:='';
BitBtn2.Height:=ComboBox1.Height;
BitBtn2.Width:=BitBtn2.Height;

if FileExists(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'current.xml') then
  begin
    PingService:=TPingService.Create(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'current.xml');
    CheckListBox1.Items.Assign(PingService.Services);
    label18.Caption:=IntToStr(CheckListBox1.Items.Count);
  end
else
  PingService:=TPingService.Create('');
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  fabout.ShowModal;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  fClean.ShowModal;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      ShowMessage('Добавлено '+IntToStr(PingService.AppendServices(OpenDialog1.FileName))+' сервисов');
      CheckListBox1.Items.Assign(PingService.Services);
      label18.Caption:=IntToStr(CheckListBox1.items.count)
    end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    begin
      case SaveDialog1.FilterIndex of
        1:PingService.SaveListToTXTFile(ExtractFileNameWithoutExt(SaveDialog1.FileName)+'.txt');
        2:PingService.SaveListToXMLFile(ExtractFileNameWithoutExt(SaveDialog1.FileName)+'.xml');
      end;
    end;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
BitBtn3Click(Self);
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var i:integer;
    Tags: TStringList;
begin
  Edit3.Text:=RSSFeed.DirectLink[ComboBox1.ItemIndex];
  Edit1.Text:=RSSFeed.BlogTitle;
  Edit2.Text:=RSSFeed.BlogURL;
  Edit4.Text:=RSSLink;
  Tags:=TStringList.Create;
  Tags.Assign(RSSFeed.Tags[ComboBox1.ItemIndex]);
  Edit5.Text:='';
  for i:=0 to Tags.Count-2 do
     Edit5.Text:=Edit5.Text+Tags.Strings[i]+',';
  Edit5.Text:=Edit5.Text+Tags.Strings[Tags.Count-1];
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  PingService.SaveListToXMLFile(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'current.xml');
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  RefreshFeed;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
var i:integer;
begin
  GetWorkList;
  label11.Caption:=IntToStr(PingService.WorkList.Count);
  PingService.SetPingInfo(Edit1.Text, Edit2.Text, Edit3.Text, Edit4.Text,Edit5.Text);
  label12.Caption:='0';
  label13.Caption:='0';
  label14.Caption:='0';
  label15.Caption:='0';
  for i:=0 to PingService.WorkList.Count-1 do
    begin
      if PingService.Ping(PingService.WorkList.Strings[i]) then
         label13.Caption:=IntToStr(StrToInt(label13.Caption)+1)
      else
        label14.Caption:=IntToStr(StrToInt(label14.Caption)+1);
      label12.Caption:=IntToStr(StrToInt(label13.Caption)+StrToInt(label14.Caption));
      label15.Caption:=IntToStr(StrToInt(label11.Caption)-StrToInt(label12.Caption));
      Application.ProcessMessages;
    end;
  PingService.Log.SaveToFile(ExtractFilePath(Application.ExeName)+'Log.txt');
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      case OpenDialog1.FilterIndex of
        1:PingService.LoadServicesFromTxt(OpenDialog1.FileName);
        2:PingService.LoadServicesFromXML(OpenDialog1.FileName);
      end;
      CheckListBox1.Items.Assign(PingService.Services);
      label18.Caption:=IntToStr(CheckListBox1.Items.Count)
    end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
var i:integer;
begin
  for i:=0 to CheckListBox1.Items.Count-1 do
     CheckListBox1.Checked[i]:=CheckBox1.Checked
end;

initialization
  {$I main.lrs}

end.

