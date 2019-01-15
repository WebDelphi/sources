unit GResumableUpload;

interface

uses Classes, SysUtils, Windows, httpsend, synautil, ssl_openssl, synacode, Character;

type
  TUploadParams = record
    FileName  : string;//полное наименование файла, включая путь к нему
    FileURL   : string;//если закачка прерывалась, то поле должно содержать URL для закачки
    XMLContent: string;//содержит XML-данные, если загрузка прошла успешно
  end;

  TStatus = (teUnknown, teWork, teAbortByUser, teAbortByServer, teDone);

  TOnProgress = procedure(TotalBytes, SendBytes: int64) of object;
  TOnUploadStatus = procedure(Status: TStatus; ResponseParams:TUploadParams)
    of object;

type
  TResumableClass = class
  private
    FHTTP: THTTPSend;
    FAuthKey: string;
    FFileStream: TFileStream;
    FInterrupted: boolean;
    FTerminate: boolean;
    FTransmitted: integer;
    FMimeType: string;
    FParams: TUploadParams;
    FOnProgress: TOnProgress;
    FOnThreadStatus: TOnUploadStatus;

    function OpenSession: string;
    procedure PrepareHeaders(const OpenSession: boolean);
    procedure SetNewChunk;
    function GetSentCount(const RangeHeader: string):int64;
    function PrepareData: boolean;
    function QueryStatus: boolean;
    procedure SendFile;
    procedure DoProgress;
    procedure DoStatus;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UploadFile(FileName, MimeType: string);overload;
    procedure UploadFile(UploadParams: TUploadParams);overload;
    property AuthKey: string read FAuthKey write FAuthKey;
    procedure Abort;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property OnThreadStatus: TOnUploadStatus read FOnThreadStatus
      write FOnThreadStatus;
end;

implementation

const
  cResumableURL =
    'https://docs.google.com/feeds/upload/create-session/default/private/full';
  cChunkSize = 262144;
  cRangeHeader = ' bytes %d-%d/%d';
  cQueryRange = 'bytes */100';
  cGDataVersion = '3.0';

{ TResumableClass }

procedure TResumableClass.Abort;
begin
 FTerminate:=true;
end;

constructor TResumableClass.Create;
begin
  inherited Create;
  FHTTP:=THTTPSend.Create;
end;

destructor TResumableClass.Destroy;
begin
  FHTTP.Free;
  if Assigned(FFileStream) then
    FFileStream.Free;
  inherited Destroy;
end;

procedure TResumableClass.DoProgress;
begin
  HeadersToList(FHTTP.Headers);
  if (FHTTP.ResultCode=308) then
    FTransmitted := GetSentCount(FHTTP.Headers.Values['Range'])
  else
    if (FHTTP.ResultCode=201)then
      FTransmitted:=FFileStream.Size;
  if Assigned(FOnProgress) then
     FOnProgress(FFileStream.Size, FTransmitted);
end;

procedure TResumableClass.DoStatus;
var
  Status: TStatus;
  Response: string;
begin
  if Assigned(FOnThreadStatus) then
  begin
    if FTerminate then
      Status := teAbortByUser
    else
      case FHTTP.ResultCode of
        200, 201:
          begin
            Status := teDone;
            FParams.XMLContent:=ReadStrFromStream(FHTTP.Document,FHTTP.Document.Size);
          end;
        308:
          Status := teWork;
        404, 410, 503:
          Status := teAbortByServer;
      else
        Status := teUnknown;
      end;

    case Status of
      teUnknown, teWork:
        Response := IntToStr(FTransmitted);
      teAbortByUser, teAbortByServer:
        Response := FParams.FileURL;
      teDone:
        Response := FParams.XMLContent;
    end;
    FOnThreadStatus(Status, FParams);
  end;

end;

function TResumableClass.GetSentCount(const RangeHeader: string): int64;
var idx: integer;
begin
  idx:=pos('-',RangeHeader);
  if Idx>0 then
    Result:=StrToInt(copy(RangeHeader,idx+1, Length(RangeHeader)-idx))+1
  else
    Result:=0;
end;

function TResumableClass.OpenSession: string;
begin
  Result := '';
  try
    PrepareHeaders(True);
    FHTTP.Headers.Add('Content-Length: 0');
    if FHTTP.HTTPMethod('POST',cResumableURL) then
      begin
        if FHTTP.ResultCode=200 then
          begin
            HeadersToList(FHTTP.Headers);
            Result:=FHTTP.Headers.Values['Location'];
          end
        else
          raise Exception.CreateFmt('Ошибка открытия сессии: %d - %s',[FHTTP.ResultCode, FHTTP.ResultString]);
      end
    else
      raise Exception.Create('Ошибка отправки запроса');
  finally
    FHTTP.Clear;
  end;
end;

function TResumableClass.PrepareData: boolean;
begin
  Result:=(Assigned(FFileStream))and(FMimeType<>'')and(FAuthKey<>'');
end;

procedure TResumableClass.PrepareHeaders(const OpenSession: boolean);
begin
  FHTTP.Headers.Add('Authorization: GoogleLogin auth=' + FAuthKey);
  FHTTP.Headers.Add('GData-Version: ' + cGDataVersion);
  FHTTP.MimeType := FMimeType;
  if OpenSession then
  begin
    FHTTP.Headers.Add('Slug: ' +EncodeURL(UTF8Encode(ExtractFileName(FParams.FileName))));
    FHTTP.Headers.Add('X-Upload-Content-Type: ' + FMimeType);
    FHTTP.Headers.Add('X-Upload-Content-Length: ' + IntToStr(FFileStream.Size));
  end
end;

function TResumableClass.QueryStatus: boolean;
begin
  PrepareHeaders(false);
  FHTTP.Headers.Add('Content-Range: '+cQueryRange);
  Result:=FHTTP.HTTPMethod('PUT',FParams.FileURL) and (FHTTP.ResultCode=308);
end;

procedure TResumableClass.SendFile;
begin
if not FileExists(FParams.FileName) then
    raise Exception.Create('Файл не найден!');
  if Assigned(FFileStream) then
    FFileStream.Free;
  FFileStream:=TFileStream.Create(FParams.FileName, fmShareDenyNone);


if not PrepareData then
    raise Exception.Create('Переданы не все данные для загрузки файла');

  if length(FParams.FileURL)=0 then
    begin
      FParams.FileURL := OpenSession;
      FTransmitted := 0;
    end
  else
    begin
      if not QueryStatus then
        raise Exception.Create('Невозможно восстановить сессию!');
    end;

  if length(FParams.FileURL) > 0 then
  begin
    FHTTP.Clear;
    FHTTP.MimeType := FMimeType;
    FFileStream.Position := soFromBeginning;
    try
      repeat
        FHTTP.Clear;
        SetNewChunk;
        PrepareHeaders(false);
        FHTTP.Headers.Add('Content-Range: '+Format(cRangeHeader, [FTransmitted, (FHTTP.Document.Size + FTransmitted - 1), FFileStream.Size]));
        FHTTP.HTTPMethod('PUT',FParams.FileURL);
        DoProgress;
        DoStatus;
      until (FTransmitted = FFileStream.Size) or (FHTTP.ResultCode <> 308)or FTerminate;
    except
      raise Exception.Create('Внутренняя ошибка загрузки файла');
    end;
  end
  else
    raise Exception.Create('Внутренняя ошибка загрузки файла');
end;

procedure TResumableClass.SetNewChunk;
begin
  FHTTP.Document.Clear;
  FFileStream.Position:=FTransmitted;
  if (FFileStream.Size - FFileStream.Position) < cChunkSize then
    FHTTP.Document.CopyFrom(FFileStream, FFileStream.Size - FFileStream.Position)
  else
    FHTTP.Document.CopyFrom(FFileStream, cChunkSize);
end;

procedure TResumableClass.UploadFile(UploadParams: TUploadParams);
begin
  FParams:=UploadParams;

end;

procedure TResumableClass.UploadFile(FileName, MimeType: string);
begin
  FMimeType:=MimeType;
  FParams.FileName:=FileName;
  SendFile;
end;

end.
