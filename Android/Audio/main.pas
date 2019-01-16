unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Androidapi.JNI.Media, Androidapi.Helpers, FMX.Media,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls;

type
  TTagType = (ALBUM, ARTIST, BITRATE, CD_TRACK_NUMBER, GENRE, TITLE, YEAR);

  TAndroidPlayer = class
  private
    FNativePlayer: JMediaPlayer;
    FURL: string;
    FMediaMetadataRetriever: JMediaMetadataRetriever;
    procedure SetURL(const AURL: string);
  public
    constructor Create;
    destructor Destroy;override;
    procedure Play;
    procedure Stop;
    function GetID3Tag(TagType: TTagType): string;
    property URL:string read FURL write SetURL;
end;

type
  TForm6 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button3: TButton;
    lbAlbum: TLabel;
    lbArtist: TLabel;
    lbTitle: TLabel;
    lbGenre: TLabel;
    lbCDTrackNo: TLabel;
    lbYear: TLabel;
    lbBitrate: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    AndroidPlayer:TAndroidPlayer;
  end;

var
  Form6: TForm6;

implementation

uses Androidapi.JNI.Net, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes,Androidapi.JNIBridge;

{$R *.fmx}
{$R *.LgXhdpiTb.fmx ANDROID}

{ TAndroidPlayer }

constructor TAndroidPlayer.Create;
begin
  inherited;
  FNativePlayer:=TJMediaPlayer.Create;
  FMediaMetadataRetriever:=TJMediaMetadataRetriever.Create;
end;

destructor TAndroidPlayer.Destroy;
begin
  FMediaMetadataRetriever.release;
  FMediaMetadataRetriever:=nil;
  FNativePlayer.release;
  FNativePlayer:=nil;
  inherited;
end;

function TAndroidPlayer.GetID3Tag(TagType: TTagType): string;
begin
  case TagType of
    ALBUM: Result:=JStringToString(FMediaMetadataRetriever.extractMetadata(TJMediaMetadataRetriever.JavaClass.METADATA_KEY_ALBUM));
    ARTIST: Result:=JStringToString(FMediaMetadataRetriever.extractMetadata(TJMediaMetadataRetriever.JavaClass.METADATA_KEY_ARTIST));
    BITRATE: Result:=JStringToString(FMediaMetadataRetriever.extractMetadata(TJMediaMetadataRetriever.JavaClass.METADATA_KEY_BITRATE));
    CD_TRACK_NUMBER: Result:=JStringToString(FMediaMetadataRetriever.extractMetadata(TJMediaMetadataRetriever.JavaClass.METADATA_KEY_CD_TRACK_NUMBER));
    GENRE: Result:=JStringToString(FMediaMetadataRetriever.extractMetadata(TJMediaMetadataRetriever.JavaClass.METADATA_KEY_GENRE));
    TITLE: Result:=JStringToString(FMediaMetadataRetriever.extractMetadata(TJMediaMetadataRetriever.JavaClass.METADATA_KEY_TITLE));
    YEAR: Result:=JStringToString(FMediaMetadataRetriever.extractMetadata(TJMediaMetadataRetriever.JavaClass.METADATA_KEY_YEAR));
  end;
end;

procedure TAndroidPlayer.Play();
begin
  if FURL.IsEmpty then Exit;
  FNativePlayer.Start();
end;


procedure TAndroidPlayer.SetURL(const AURL: string);
var O: JHashMap;
    M: JMap;
begin
  FURL:=AURL;
  FNativePlayer.reset;
  if TJBuild_VERSION.JavaClass.SDK_INT>=14 then
    begin
      O:=TJHashMap.Create;
      M:=TJMap.Wrap((O as ILocalObject).GetObjectID);
      FMediaMetadataRetriever.setDataSource(StringToJString(FURL),M);
    end
  else
    FMediaMetadataRetriever.setDataSource(SharedActivityContext, StrToJURI(FURL));
   FNativePlayer.setDataSource(SharedActivityContext, StrToJURI(FURL));
   FNativePlayer.prepare;
end;

procedure TAndroidPlayer.Stop;
begin
  FNativePlayer.stop;
end;

procedure TForm6.Button1Click(Sender: TObject);
begin
  AndroidPlayer.URL:=Edit1.Text;
  AndroidPlayer.Play;
  lbAlbum.Text:=AndroidPlayer.GetID3Tag(ALBUM);
  lbArtist.Text:=AndroidPlayer.GetID3Tag(ARTIST);
  lbTitle.Text:=AndroidPlayer.GetID3Tag(TITLE);
  lbGenre.Text:=AndroidPlayer.GetID3Tag(GENRE);
  lbCDTrackNo.Text:=AndroidPlayer.GetID3Tag(CD_TRACK_NUMBER);
  lbYear.Text:=AndroidPlayer.GetID3Tag(YEAR);
  lbBitrate.Text:=AndroidPlayer.GetID3Tag(BITRATE);
end;

procedure TForm6.Button3Click(Sender: TObject);
begin
AndroidPlayer.Stop;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  AndroidPlayer:=TAndroidPlayer.Create;
end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
  AndroidPlayer.Free;
end;

end.
