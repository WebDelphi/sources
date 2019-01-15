unit DownLoaderTestUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DownLoader_unit, StdCtrls, ToolWin, ComCtrls, Menus, WinInet;

type
  TForm11 = class(TForm)
    Downloader1: TDownloader;
    Edit1: TEdit;
    ToolBar1: TToolBar;
    StatusBar1: TStatusBar;
    RichEdit1: TRichEdit;
    SaveDialog1: TSaveDialog;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    ProgressBar1: TProgressBar;
    procedure Downloader1Error(Sender: TObject; E: TDownLoadError);
    procedure Downloader1Accepted(Sender: TObject);
    procedure Downloader1StartDownload(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure N2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure Downloader1Break(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure Downloader1Downloading(Sender: TObject; AcceptedSize,
      MaxSize: Cardinal);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form11: TForm11;

implementation

{$R *.dfm}

procedure TForm11.Downloader1Accepted(Sender: TObject);
begin
  ProgressBar1.Visible := False;
  ToolButton2.Visible := false;
  ToolButton1.Visible := true;
  ToolButton3.Visible := true;
  Edit1.ReadOnly := false;
  StatusBar1.SimpleText :=
    'Скачивание успешно завершено. Всего получено данных в байтах: ' + IntToStr
    (Downloader1.AcceptedSize);
end;

procedure TForm11.Downloader1Break(Sender: TObject);
begin
  ProgressBar1.Visible := False;
  ToolButton2.Visible := false;
  ToolButton2.Enabled := true;
  ToolButton1.Visible := true;
  ToolButton3.Visible := Downloader1.AcceptedSize > 0;
  Edit1.ReadOnly := false;
  StatusBar1.SimpleText :=
    'Скачивание остановлено. Всего получено данных в байтах: ' + IntToStr
    (Downloader1.AcceptedSize);
end;

procedure TForm11.Downloader1Downloading(Sender: TObject; AcceptedSize,
  MaxSize: Cardinal);
begin
  StatusBar1.SimpleText := 'Получено байт: ' + IntToStr(AcceptedSize);
  ProgressBar1.Visible := MaxSize > AcceptedSize;
  ProgressBar1.Max := MaxSize;
  ProgressBar1.Position := AcceptedSize;
end;

procedure TForm11.Downloader1Error(Sender: TObject; E: TDownLoadError);
var
  s: string;
begin
  ProgressBar1.Visible := False;
  ToolButton2.Visible := false;
  ToolButton1.Visible := true;
  ToolButton3.Visible := Downloader1.AcceptedSize > 0;
  Edit1.ReadOnly := false;
  case E of
    deInternetOpen: s := 'Ошибка при открытии сессии. ';
    deInternetOpenUrl: s := 'Ошибка при запрашивании файла. ';
    deDownloadingFile: s := 'Ошибка при чтении файла. ';
  end;
  StatusBar1.SimpleText :=
    s + 'Всего получено данных в байтах: ' + IntToStr
    (Downloader1.AcceptedSize);
end;

procedure TForm11.Downloader1StartDownload(Sender: TObject);
begin
  ToolButton1.Visible := false;
  ToolButton2.Visible := true;
  ToolButton3.Visible := false;
  Edit1.ReadOnly := true;
  StatusBar1.SimpleText := 'Инициализация скачивания';
end;

procedure TForm11.N1Click(Sender: TObject);
begin
  RichEdit1.Lines.LoadFromStream(Downloader1.OutStream);
end;

procedure TForm11.N2Click(Sender: TObject);
begin
if Edit1.Text = 'http://www.webdelphi.ru/wp-content/plugins/download-monitor/download.php?id=33' then
  SaveDialog1.FileName := 'Chronometr_2.1.Installer.exe';
  if SaveDialog1.Execute then
    Downloader1.OutStream.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm11.ToolButton1Click(Sender: TObject);
begin
  Downloader1.URL := Edit1.Text;
  Downloader1.DownLoad;
end;

procedure TForm11.ToolButton2Click(Sender: TObject);
begin
  StatusBar1.SimpleText := 'Останавливаем скачку';
  ToolButton2.Enabled := false;
  Downloader1.BreakDownload;
end;

procedure TForm11.ToolButton3Click(Sender: TObject);
begin
  ToolButton3.DropdownMenu.Popup(ToolButton3.ClientOrigin.X,
    ToolButton3.ClientOrigin.Y + ToolButton3.Height);
end;

end.
