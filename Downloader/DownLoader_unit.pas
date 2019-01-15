unit DownLoader_unit;

interface

uses Classes, WinInet, SysUtils, Dialogs, Windows, Forms;

type
  PMemoryStream = ^TMemoryStream;
  TDownloadError = (deInternetOpen, deInternetOpenUrl, deDownloadingFile,
    deConnect, deRequest);
  TErrorEvent = procedure(Sender: TObject; E: TDownloadError) of object;
  TDownloadingEvent = procedure(Sender: TObject;
    AcceptedSize, MaxSize: Cardinal) of object;

  TDownloadThread = class(TThread)
  private
    fURL: String;
    MemoryStream: TMemoryStream;
    err: TDownloadError;
    fError: TErrorEvent;
    fAccepted: TNotifyEvent;
    fBreak: TNotifyEvent;
    fDownloading: TDownloadingEvent;
    AcceptedSize: Cardinal;
    AllSize: Cardinal;
    procedure toError;
    procedure toDownloading;
    procedure toAccepted;
    procedure toBreak;
    procedure Complete;
    function ErrorResult(E: Boolean; eType: TDownloadError): Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspennded: Boolean; const URL: String;
      Stream: PMemoryStream);
    property URL: string read fURL;
    property OnError: TErrorEvent read fError write fError;
    property OnAccepted: TNotifyEvent read fAccepted write fAccepted;
    property OnBreak: TNotifyEvent read fBreak write fBreak;
    property OnDownloading
      : TDownloadingEvent read fDownloading write fDownloading;
  end;

  TDownloader = class(TComponent)
  private
    fOutStream: TMemoryStream;
    fURL: string;
    Downloader: TDownloadThread;
    fOnError: TErrorEvent;
    fOnAccepted: TNotifyEvent;
    fOnBreak: TNotifyEvent;
    fOnStartDownload: TNotifyEvent;
    fInDowloading: Boolean;
    fAcceptedSize: Cardinal;
    fDownloading: TDownloadingEvent;
    procedure AcceptDownload(Sender: TObject);
    procedure Break_Download(Sender: TObject);
    procedure Downloading(Sender: TObject; AcceptedSize, MaxSize: Cardinal);
    procedure ErrorDownload(Sender: TObject; Error: TDownloadError);
  public
    procedure DownLoad;
    procedure BreakDownload;
    property OutStream: TMemoryStream read fOutStream;
    property InDowloading: Boolean read fInDowloading;
    property AcceptedSize: Cardinal read fAcceptedSize;
  published
    property URL: string read fURL write fURL;
    property OnError: TErrorEvent read fOnError write fOnError;
    property OnAccepted: TNotifyEvent read fOnAccepted write fOnAccepted;
    property OnDownloading
      : TDownloadingEvent read fDownloading write fDownloading;
    property OnStartDownload
      : TNotifyEvent read fOnStartDownload write fOnStartDownload;
    property OnBreak: TNotifyEvent read fOnBreak write fOnBreak;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('BuBa Group', [TDownloader]);
end;

procedure TDownloadThread.toDownloading;
begin
  if Assigned(fDownloading) then
    fDownloading(Self, AcceptedSize, AllSize);
end;

procedure TDownloadThread.toAccepted;
begin
  if Assigned(fAccepted) then
    fAccepted(Self);
end;

procedure TDownloadThread.toBreak;
begin
  if Assigned(fBreak) then
    fBreak(Self);
end;

procedure TDownloadThread.Complete;
begin
  if Terminated then
    Synchronize(toBreak)
  else
    Synchronize(toAccepted);
end;

procedure TDownloadThread.toError;
begin
  if Assigned(fError) then
    OnError(Self, err);
end;

function TDownloadThread.ErrorResult(E: Boolean; eType: TDownloadError)
  : Boolean;
begin
  Result := E;
  if E then
  begin
    err := eType;
    toError;
  end;
end;

function GetUrlInfo(const dwInfoLevel: DWORD; const FileURL: string): string;
var
  hSession, hFile: hInternet;
  dwBuffer: array [0 .. 1024] of Byte;
  dwBufferLen, dwIndex: DWORD;
begin
  Result := '';
  hSession := InternetOpen('Dowload Master', INTERNET_OPEN_TYPE_PRECONFIG,
    nil, nil, 0);
  if Assigned(hSession) then
  begin
    hFile := InternetOpenURL(hSession, PChar(FileURL), nil, 0,
      INTERNET_FLAG_RELOAD, 0);
    dwIndex := 0;
    dwBufferLen := Length(dwBuffer);
    if HttpQueryInfo(hFile, dwInfoLevel, @dwBuffer, dwBufferLen, dwIndex) then
      Result := PChar(@dwBuffer);
    if Assigned(hFile) then
      InternetCloseHandle(hFile);
    InternetCloseHandle(hSession);
  end;
end;

procedure TDownloadThread.Execute;
var
  pInet, pUrl: Pointer;
  Buffer: array [0 .. 1024] of Byte;
  BytesRead: Cardinal;
begin
  pInet := InternetOpen('Dowload Master', INTERNET_OPEN_TYPE_PRECONFIG, nil,
    nil, 0);
  if ErrorResult(pInet = nil, deInternetOpen) then
    Exit;
  pUrl := InternetOpenURL(pInet, PChar(URL), nil, 0,
    INTERNET_FLAG_PRAGMA_NOCACHE or INTERNET_FLAG_RELOAD, 0);
  if ErrorResult(pUrl = nil, deInternetOpenUrl) then
    Exit;
  try
    AllSize := StrToInt('0' + GetUrlInfo(HTTP_QUERY_CONTENT_LENGTH, URL));
    repeat
      if Terminated then
        Break;
      FillChar(Buffer, SizeOf(Buffer), 0);
      if ErrorResult(not InternetReadFile(pUrl, @Buffer, Length(Buffer),
          BytesRead), deDownloadingFile) then
        Exit
      else
        MemoryStream.Write(Buffer, BytesRead);
      AcceptedSize := MemoryStream.Size;
      Synchronize(toDownloading);
    until (BytesRead = 0);
    MemoryStream.Position := 0;
  finally
    if pUrl <> nil then
      InternetCloseHandle(pUrl);
    if pInet <> nil then
      InternetCloseHandle(pInet);
  end;
  Pointer(MemoryStream) := nil;
  Complete;
end;

constructor TDownloadThread.Create(CreateSuspennded: Boolean;
  const URL: String; Stream: PMemoryStream);
begin
  inherited Create(CreateSuspennded);
  FreeOnTerminate := True;
  Pointer(MemoryStream) := Stream;
  AcceptedSize := 0;
  fURL := URL;
end;

procedure TDownloader.DownLoad;
begin
  fInDowloading := True;
  if Assigned(Downloader) then
    Downloader.Terminate;
  if Assigned(fOutStream) then
    FreeAndNil(fOutStream);
  fAcceptedSize := 0;
  fOutStream := TMemoryStream.Create;
  Downloader := TDownloadThread.Create(True, URL, Pointer(fOutStream));
  Downloader.OnAccepted := AcceptDownload;
  Downloader.OnError := ErrorDownload;
  Downloader.OnDownloading := Downloading;
  Downloader.OnBreak := Break_Download;
  Downloader.Resume;
  if Assigned(fOnStartDownload) then
    fOnStartDownload(Self);
end;

procedure TDownloader.BreakDownload;
begin
  if not InDowloading then
    Exit;
  if Assigned(Downloader) then
    Downloader.Terminate;
end;

procedure TDownloader.Break_Download(Sender: TObject);
begin
  fInDowloading := False;
  Downloader := nil;
  if Assigned(fOnBreak) then
    fOnBreak(Self);
end;

procedure TDownloader.AcceptDownload(Sender: TObject);
begin
  fInDowloading := False;
  Downloader := nil;
  if Assigned(fOnAccepted) then
    fOnAccepted(Self);
end;

procedure TDownloader.Downloading(Sender: TObject;
  AcceptedSize, MaxSize: Cardinal);
begin
  fAcceptedSize := AcceptedSize;
  if Assigned(fDownloading) then
    fDownloading(Self, AcceptedSize, MaxSize);
end;

procedure TDownloader.ErrorDownload(Sender: TObject; Error: TDownloadError);
begin
  fInDowloading := False;
  Downloader := nil;
  fOutStream := nil;
  if Assigned(fOnError) then
    fOnError(Self, Error);
end;

end.
