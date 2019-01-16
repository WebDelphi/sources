unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, IPPeerServer,
  System.Tether.Manager, System.Tether.AppProfile, Vcl.StdCtrls, Vcl.ExtCtrls, Generics.Collections,
  System.Actions, Vcl.ActnList;

type
  TForm3 = class(TForm)
    Memo1: TMemo;
    VCLManager: TTetheringManager;
    VCLProfile: TTetheringAppProfile;
    Panel1: TPanel;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    cbManagers: TComboBox;
    Label2: TLabel;
    cbProfiles: TComboBox;
    Button4: TButton;
    Button5: TButton;
    ActionList1: TActionList;
    Action1: TAction;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    OpenDialog1: TOpenDialog;
    Button9: TButton;
    procedure VCLManagerEndAutoConnect(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure VCLManagerEndManagersDiscovery(const Sender: TObject;
      const ARemoteManagers: TTetheringManagerInfoList);
    procedure VCLManagerEndProfilesDiscovery(const Sender: TObject;
      const ARemoteProfiles: TTetheringProfileInfoList);
    procedure cbManagersChange(Sender: TObject);
    procedure VCLManagerRequestManagerPassword(const Sender: TObject;
      const ARemoteIdentifier: string; var Password: string);
    procedure Button4Click(Sender: TObject);
    procedure VCLProfileBeforeConnectProfile(const Sender: TObject;
      const AProfileInfo: TTetheringProfileInfo; var AllowConnect: Boolean);
    procedure VCLProfileAfterConnectProfile(const Sender: TObject;
      const AProfileInfo: TTetheringProfileInfo);
    procedure VCLProfileRemoteProfileUpdate(const Sender: TObject;
      const AProfileId: string);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure VCLProfileAcceptResource(const Sender: TObject;
      const AProfileId: string; const AResource: TCustomRemoteItem;
      var AcceptResource: Boolean);
    procedure VCLProfileResources0ResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure VCLProfileResourceUpdated(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure VCLProfileDisconnect(const Sender: TObject;
      const AProfileInfo: TTetheringProfileInfo);
    procedure VCLManagerRemoteManagerShutdown(const Sender: TObject;
      const AManagerIdentifier: string);
  private
    function FindManagerInfo(const AManagerIdentifier: string): TTetheringManagerInfo;
    function FindProfile(const AProfileText: string): TTetheringProfileInfo;
    procedure SetRemoteProfiles(AProfiles: TTetheringProfileInfoList);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}


procedure TForm3.Button1Click(Sender: TObject);
begin
  VCLManagerEndAutoConnect(self)
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  VCLManager.DiscoverManagers();
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  Memo1.Lines.Add('Start AutoConnect');
  VCLManager.AutoConnect();
end;

procedure TForm3.Button4Click(Sender: TObject);
var
  Profile: TTetheringProfileInfo;
begin
  Profile := FindProfile(cbProfiles.Items[cbProfiles.ItemIndex]);
  if Profile.ProfileIdentifier <> EmptyStr then
    VCLProfile.Connect(Profile)
  else
    raise Exception.Create('RemoteProfile not found');
end;

procedure TForm3.Button5Click(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Pred(VCLProfile.ConnectedProfiles.Count) do
  begin
    if VCLProfile.RunRemoteAction(VCLProfile.ConnectedProfiles[I], 'raMemo') then
      Memo1.Lines.Add('Удаленное действие для профиля ' + VCLProfile.ConnectedProfiles[I].ProfileText + ' успешно выполнено')
    else
      Memo1.Lines.Add('Удаленное действие для профиля ' + VCLProfile.ConnectedProfiles[I].ProfileText + ' не выполнено')
  end;
end;

procedure TForm3.Button7Click(Sender: TObject);
const
  cResDescr = 'Name: %s  Value: %s';
var
  RemoteResources: TList<TRemoteResource>;
  I, J: Integer;
  Resource: TRemoteResource;
  ResVal: string;
begin
  for I := 0 to Pred(VCLProfile.ConnectedProfiles.Count) do
  begin
    RemoteResources := VCLProfile.GetRemoteProfileResources(VCLProfile.ConnectedProfiles[I]);
    for J := 0 to Pred(RemoteResources.Count) do
    begin
      Resource := VCLProfile.GetRemoteResourceValue(RemoteResources[J]);
      case Resource.ResType of
        TRemoteResourceType.Data:
          begin
            case Resource.Value.DataType of
              TResourceType.Integer:
                ResVal := Resource.Value.AsInteger.ToString;
              TResourceType.Single:
                ResVal := Resource.Value.AsSingle.ToString;
              TResourceType.Double:
                ResVal := Resource.Value.AsDouble.ToString;
              TResourceType.Int64:
                ResVal := Resource.Value.AsInt64.ToString;
              TResourceType.Boolean:
                ResVal := Resource.Value.AsBoolean.ToString;
              TResourceType.String:
                ResVal := Resource.Value.AsString;
            end;
          end;
        TRemoteResourceType.Stream:
          ResVal := Resource.ToJsonString;
      end;
      Memo1.Lines.Add(Format(cResDescr, [Resource.Name, ResVal]));
      // VCLProfile.UnSubscribeFromRemoteItem()
    end;
  end;
end;

procedure TForm3.Button8Click(Sender: TObject);
var
  I: Integer;
  FS: TFileStream;
begin
  if OpenDialog1.Execute then
  begin
    FS := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      for I := 0 to Pred(VCLProfile.ConnectedProfiles.Count) do
        if VCLProfile.SendStream(VCLProfile.ConnectedProfiles[i],ExtractFileName(FS.FileName),FS) then
          Memo1.Lines.Add('Файл '+ExtractFileName(FS.FileName)+' отправлен '+VCLProfile.ConnectedProfiles[i].ProfileText)
        else
          Memo1.Lines.Add('Ошибка отправки файла '+ExtractFileName(FS.FileName)+' профилю '+VCLProfile.ConnectedProfiles[i].ProfileText)
    finally
      FreeAndNil(FS)
    end;
  end;
end;

procedure TForm3.Button9Click(Sender: TObject);
var i: integer;
begin
  for I := 0 to Pred(VCLProfile.ConnectedProfiles.Count) do
    VCLProfile.Disconnect(VCLProfile.ConnectedProfiles[i]);
end;

procedure TForm3.cbManagersChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Pred(VCLManager.PairedManagers.Count) do
    if SameText(cbManagers.Items[cbManagers.ItemIndex], VCLManager.PairedManagers[I].ManagerText) then
    begin
      SetRemoteProfiles(VCLManager.RemoteProfiles);
      exit;
    end;

  for I := 0 to Pred(VCLManager.RemoteManagers.Count) do
    if SameText(cbManagers.Items[cbManagers.ItemIndex], VCLManager.RemoteManagers[I].ManagerText) then
    begin
      VCLManager.PairManager(VCLManager.RemoteManagers[I]);
      break;
    end;
end;

function TForm3.FindManagerInfo(const AManagerIdentifier: string): TTetheringManagerInfo;
var
  I: Integer;
begin
  Result.ManagerIdentifier := EmptyStr;
  for I := 0 to Pred(VCLManager.RemoteManagers.Count) do
    if SameText(VCLManager.RemoteManagers[I].ManagerIdentifier, AManagerIdentifier) then
      exit(VCLManager.RemoteManagers[I])
end;

function TForm3.FindProfile(const AProfileText: string): TTetheringProfileInfo;
var
  I: Integer;
begin
  Result.ProfileIdentifier := EmptyStr;
  for I := 0 to Pred(VCLManager.RegisteredProfiles.Count) do
    if SameText(AProfileText, VCLManager.RemoteProfiles[I].ProfileText) then
      exit(VCLManager.RemoteProfiles[I])
end;

procedure TForm3.SetRemoteProfiles(AProfiles: TTetheringProfileInfoList);
var
  I: Integer;
  Manager: TTetheringManagerInfo;
begin
  cbProfiles.Items.Clear;
  for I := 0 to Pred(AProfiles.Count) do
  begin
    Manager := FindManagerInfo(AProfiles[I].ManagerIdentifier);
    if (Manager.ManagerIdentifier <> EmptyStr) and SameText(cbManagers.Items[cbManagers.ItemIndex], Manager.ManagerText) then
      cbProfiles.Items.Add(AProfiles[I].ProfileText);
  end;
end;

procedure TForm3.VCLManagerEndAutoConnect(Sender: TObject);
var
  I: Integer;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('EndAutoConnect'#13#10'Remote Managers:');
  for I := 0 to Pred(VCLManager.RemoteManagers.Count) do
  begin
    Memo1.Lines.Add('  Name ' + VCLManager.RemoteManagers[I].ManagerName);
    Memo1.Lines.Add('  Description ' + VCLManager.RemoteManagers[I].ManagerText);
    Memo1.Lines.Add('  Identifier ' + VCLManager.RemoteManagers[I].ManagerIdentifier);
    Memo1.Lines.Add('  Connection string ' + VCLManager.RemoteManagers[I].ConnectionString);
  end;
  Memo1.Lines.Add('Remote Profiles');
  for I := 0 to Pred(VCLManager.RemoteProfiles.Count) do
  begin
    Memo1.Lines.Add('  Description ' + VCLManager.RemoteProfiles[I].ProfileText);
    Memo1.Lines.Add('  Manager Identifier ' + VCLManager.RemoteProfiles[I].ManagerIdentifier);
    Memo1.Lines.Add('  Identifier ' + VCLManager.RemoteProfiles[I].ProfileIdentifier);
    Memo1.Lines.Add('  Group ' + VCLManager.RemoteProfiles[I].ProfileGroup);
    Memo1.Lines.Add('  Type ' + VCLManager.RemoteProfiles[I].ProfileType);
    Memo1.Lines.Add('  Version ' + VCLManager.RemoteProfiles[I].ProfileVersion.ToString);
  end;
end;

procedure TForm3.VCLManagerEndManagersDiscovery(const Sender: TObject;
  const ARemoteManagers: TTetheringManagerInfoList);
var
  I: Integer;
begin
  cbManagers.Items.Clear;
  Memo1.Lines.Add('EndManagersDiscovery');
  for I := 0 to Pred(ARemoteManagers.Count) do
  begin
    cbManagers.Items.Add(ARemoteManagers[I].ManagerText);
    Memo1.Lines.Add('  Name ' + ARemoteManagers[I].ManagerName);
    Memo1.Lines.Add('  Description ' + ARemoteManagers[I].ManagerText);
    Memo1.Lines.Add('  Identifier ' + ARemoteManagers[I].ManagerIdentifier);
    Memo1.Lines.Add('  Connection string ' + ARemoteManagers[I].ConnectionString);
  end;
end;

procedure TForm3.VCLManagerEndProfilesDiscovery(const Sender: TObject;
  const ARemoteProfiles: TTetheringProfileInfoList);
var
  I: Integer;
begin
  cbProfiles.Items.Clear;
  Memo1.Lines.Add('EndManagersDiscovery');
  SetRemoteProfiles(ARemoteProfiles);
  for I := 0 to Pred(ARemoteProfiles.Count) do
  begin
    Memo1.Lines.Add('  Description ' + ARemoteProfiles[I].ProfileText);
    Memo1.Lines.Add('  Manager Identifier ' + ARemoteProfiles[I].ManagerIdentifier);
    Memo1.Lines.Add('  Identifier ' + ARemoteProfiles[I].ProfileIdentifier);
    Memo1.Lines.Add('  Group ' + ARemoteProfiles[I].ProfileGroup);
    Memo1.Lines.Add('  Type ' + ARemoteProfiles[I].ProfileType);
    Memo1.Lines.Add('  Version ' + ARemoteProfiles[I].ProfileVersion.ToString);
  end;
end;

procedure TForm3.VCLManagerRemoteManagerShutdown(const Sender: TObject;
  const AManagerIdentifier: string);
begin
  Memo1.Lines.Add('Manager '+AManagerIdentifier+' Shutdown');
end;

procedure TForm3.VCLManagerRequestManagerPassword(const Sender: TObject;
  const ARemoteIdentifier: string; var Password: string);
begin
  Memo1.Lines.Add('RequestManagerPassword. Manager Identifier: ' + ARemoteIdentifier);
  Password := '123456';
end;

procedure TForm3.VCLProfileAcceptResource(const Sender: TObject;
  const AProfileId: string; const AResource: TCustomRemoteItem;
  var AcceptResource: Boolean);
begin
  Memo1.Lines.Add('AcceptResource');
  AcceptResource := True;
end;

procedure TForm3.VCLProfileAfterConnectProfile(const Sender: TObject;
  const AProfileInfo: TTetheringProfileInfo);
var
  I: Integer;
begin
  Memo1.Lines.Add('AfterConnectProfile ' + AProfileInfo.ProfileText);
  Memo1.Lines.Add('---Connected Profiles---');
  for I := 0 to Pred(VCLProfile.ConnectedProfiles.Count) do
  begin
    Memo1.Lines.Add(VCLManager.ProfileInfoToString(VCLProfile.ConnectedProfiles[I]))
  end;
  // if VCLProfile.Connect(AProfileInfo) then
  // begin
  // Memo1.Lines.Add('---Connected Profiles---');
  // for I := 0 to Pred(VCLProfile.ConnectedProfiles.Count) do
  // begin
  // Memo1.Lines.Add(VCLManager.ProfileInfoToString(VCLProfile.ConnectedProfiles[i]))
  // end;
  // end
  // else
  // raise Exception.Create('Error Message');
end;

procedure TForm3.VCLProfileBeforeConnectProfile(const Sender: TObject;
  const AProfileInfo: TTetheringProfileInfo; var AllowConnect: Boolean);
begin
  Memo1.Lines.Add('BeforeConnectProfile ' + AProfileInfo.ProfileText);
  AllowConnect := True;
end;

procedure TForm3.VCLProfileDisconnect(const Sender: TObject;
  const AProfileInfo: TTetheringProfileInfo);
begin
  Memo1.Lines.Add('Disconnect '+AProfileInfo.ProfileText);
end;

procedure TForm3.VCLProfileRemoteProfileUpdate(const Sender: TObject;
  const AProfileId: string);
begin
  Memo1.Lines.Add('RemoteProfileUpdate')
end;

procedure TForm3.VCLProfileResources0ResourceReceived(const Sender: TObject;
  const AResource: TRemoteResource);
begin
  Memo1.Lines.Add('Recived Resource0:');
  Memo1.Lines.Add('Name: ' + AResource.Name);
  TStringStream(AResource.Value.AsStream).SaveToFile('test.txt');
end;

procedure TForm3.VCLProfileResourceUpdated(const Sender: TObject;
  const AResource: TRemoteResource);
begin

  Memo1.Lines.Add('Resource Updated:');
  Memo1.Lines.Add('Name: ' + AResource.Name);
  case AResource.ResType of
    TRemoteResourceType.Data:
      Memo1.Lines.Add('Value: ' + AResource.Value.AsInteger.ToString);
    TRemoteResourceType.Stream:
      TStringStream(AResource.Value.AsStream).SaveToFile('updated_test.txt');
  end;

end;

end.
