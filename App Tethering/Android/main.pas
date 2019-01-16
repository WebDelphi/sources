unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  IPPeerServer, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.Tether.Manager, System.Tether.AppProfile, System.Actions, FMX.ActnList,
  FMX.StdActns, FMX.StdCtrls;

type
  TForm4 = class(TForm)
    AndroidProfile: TTetheringAppProfile;
    AndroidManager: TTetheringManager;
    Memo1: TMemo;
    AndroidActionList: TActionList;
    Action1: TAction;
    Panel1: TPanel;
    Button1: TButton;
    procedure AndroidManagerEndAutoConnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AndroidManagerRequestManagerPassword(const Sender: TObject;
      const ARemoteIdentifier: string; var Password: string);
    procedure Action1Execute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure AndroidProfileAfterReceiveStream(const Sender: TObject;
      const AInputStream, AOutputStream: TStream);
    procedure AndroidProfileResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
  private
    ResStream: TStringStream;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses IOUtils;

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TForm4.Action1Execute(Sender: TObject);
begin
  Memo1.Lines.Add('Action1 Executed');
end;

procedure TForm4.AndroidManagerEndAutoConnect(Sender: TObject);
var
  I: Integer;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('EndAutoConnect'#13#10'Remote Managers:');
  for I := 0 to pred(AndroidManager.RemoteManagers.Count) do
    begin
      Memo1.Lines.Add('  MANAGER '+i.ToString);
      Memo1.Lines.Add('  Name '+AndroidManager.RemoteManagers[i].ManagerName);
      Memo1.Lines.Add('  Description '+AndroidManager.RemoteManagers[i].ManagerText);
      Memo1.Lines.Add('  Identifier '+AndroidManager.RemoteManagers[i].ManagerIdentifier);
      Memo1.Lines.Add('  Connection string '+AndroidManager.RemoteManagers[i].ConnectionString);
      Memo1.Lines.Add('-------------');
    end;
  Memo1.Lines.Add('Remote Profiles');
  for I := 0 to Pred(AndroidManager.RemoteProfiles.Count) do
    begin
      Memo1.Lines.Add('Profile '+i.ToString);
      Memo1.Lines.Add('  Description '+AndroidManager.RemoteProfiles[i].ProfileText);
      Memo1.Lines.Add('  Manager Identifier '+AndroidManager.RemoteProfiles[i].ManagerIdentifier);
      Memo1.Lines.Add('  Identifier '+AndroidManager.RemoteProfiles[i].ProfileIdentifier);
      Memo1.Lines.Add('  Group '+AndroidManager.RemoteProfiles[i].ProfileGroup);
      Memo1.Lines.Add('  Type '+AndroidManager.RemoteProfiles[i].ProfileType);
      Memo1.Lines.Add('  Version '+AndroidManager.RemoteProfiles[i].ProfileVersion.ToString);
      Memo1.Lines.Add('-------------');
    end;
end;

procedure TForm4.AndroidManagerRequestManagerPassword(const Sender: TObject;
  const ARemoteIdentifier: string; var Password: string);
begin
  Password:='123456'
end;


procedure TForm4.AndroidProfileAfterReceiveStream(const Sender: TObject;
  const AInputStream, AOutputStream: TStream);
begin
  Memo1.Lines.Add('Stream recieved');
  TMemoryStream(AInputStream).SaveToFile(IncludeTrailingPathDelimiter(TPath.GetDocumentsPath)+'file.txt');
end;

procedure TForm4.AndroidProfileResourceReceived(const Sender: TObject;
  const AResource: TRemoteResource);
var FS: TFileStream;
begin
  if AResource.ResType = TRemoteResourceType.Stream then
    begin
      Memo1.Lines.Add('Resource '+AResource.Hint);
      FS:=TFileStream.Create(TPath.Combine(TPath.GetSharedDocumentsPath, AResource.Hint), fmCreate or fmOpenWrite);
      try
        AResource.Value.AsStream.Position:=soFromBeginning;
        FS.CopyFrom(AResource.Value.AsStream, AResource.Value.AsStream.Size);
        Memo1.Lines.Add('Resource save to '+FS.FileName);
      finally
        FS.Free
      end;
    end;
end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  ResStream.Clear;
  ResStream.WriteString(Memo1.Lines.Text);
  AndroidProfile.Resources.FindByName('rsInteger').Value:=ResStream.Size;
  AndroidProfile.Resources.FindByName('rsStream').Broadcast;
  Memo1.Lines.Add('Resource Updated')
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  ResStream:=TStringStream.Create;
  AndroidManager.AutoConnect();
  AndroidProfile.Resources.FindByName('rsString').Value:='Hello from Android!';
  AndroidProfile.Resources.FindByName('rsInteger').Value:=12345;
  AndroidProfile.Resources.FindByName('rsStream').Value:=ResStream;
//  AndroidProfile.Resources.FindByName('rsStream').NotifyUpdates:=True;
//  Memo1.Lines.Add(AndroidProfile.Resources.FindByName('rsString').Value.AsString)
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ResStream);
end;

end.
