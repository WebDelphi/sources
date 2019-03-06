unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.WinXCtrls,
  System.ImageList, Vcl.ImgList, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.CategoryButtons, Vcl.WinXPanels, System.Actions, Vcl.ActnList,
  Vcl.ComCtrls, System.Diagnostics, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TForm4 = class(TForm)
    svMainMenu: TSplitView;
    Panel1: TPanel;
    ImageList1: TImageList;
    imgMenu: TImage;
    CategoryButtons1: TCategoryButtons;
    CardPanel1: TCardPanel;
    imgAuth: TImage;
    ActionList1: TActionList;
    actHosts: TAction;
    cardSites: TCard;
    lvSites: TListView;
    NetHTTPClient1: TNetHTTPClient;
    NetHTTPRequest1: TNetHTTPRequest;
    procedure imgMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgAuthClick(Sender: TObject);
    procedure actHostsExecute(Sender: TObject);
  private

  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses uYwmApi;

{$R *.dfm}

procedure TForm4.actHostsExecute(Sender: TObject);
var
  I: Integer;
begin
  YWM.GetHosts;
  lvSites.Items.Clear;
  lvSites.Items.BeginUpdate;
  try
  for I := 0 to Pred(YWM.YwmHostList.List.Count) do
    with lvSites.Items.Add do
      begin
        GroupID:=1;
        YWM.GetHostInfo(YWM.YwmHostList.List[i]);
        YWM.GetHostSymmary(YWM.YwmHostList.List[i]);
        Caption:=YWM.YwmHostList.List[i].host_display_name;
        if YWM.YwmHostList.List[i].MainMirror then
          SubItems.Add(YWM.YwmHostList.List[i].Mirror.ascii_host_url)
        else
          SubItems.Add(YWM.YwmHostList.List[i].MainInfo.ascii_host_url);
        SubItems.Add(IntToStr(YWM.YwmHostList.List[i].sqi));
        SubItems.Add(IntToStr(YWM.YwmHostList.List[i].searchable_pages_count));
        SubItems.Add(IntToStr(YWM.YwmHostList.List[i].excluded_pages_count));
      end;
  finally
    lvSites.Items.EndUpdate;
  end;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  svMainMenu.Close;
end;


procedure TForm4.imgAuthClick(Sender: TObject);
begin
  YWM.YandexAuth;
  if YWM.Authorized then
    imgAuth.Visible:=False;
end;

procedure TForm4.imgMenuClick(Sender: TObject);
begin
  if svMainMenu.Opened then
    svMainMenu.Close
  else
    svMainMenu.Open;
end;

end.
