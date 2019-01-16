unit Google.Maps;

interface

uses System.SysUtils, System.Classes, System.UITypes, Generics.Collections;

const
  MAP_BASE_URL = 'http://maps.googleapis.com/maps/api/staticmap';
  MAP_ATTR_DELIMITER = '|';
  MAP_POINT_DEFAULT_PRECISION = 10;
  MAP_POINT_DEFAULT_DIGITS = 6;
  MAP_DEFAULT_HEIGHT = 400;
  MAP_DEFAULT_WIDTH = 400;
  MAP_DEFAULT_PATH_WIDTH = 5;

type
  EMapException = class(Exception);

  TMapPoint = class
  private
    FLatitude: double;
    FLongitude: double;
  public
    constructor Create(ALatitude, ALongitude: double);
    destructor Destroy;override;
    function ToString(Precision, Digits: Integer):string;overload;
    function ToString():string;overload;
    function Validate:boolean;
    property Latitude: double read FLatitude write FLatitude;
    property Longitude: double read FLongitude write FLongitude;
  end;

  TMapFormat = (mfPNG, mfPNG8, mfPNG32, mfGIF, mfJPG, mfBaseline);
  TMapType = (mtRoadmap, mtSatellite, mtTerrain, mtHybrid);
  TMapMarkerSize = (msMid, msTiny, msSmall);

  TMapColor = class
  private
    FColor: TColor;
  public
    constructor Create(AColor: TColor);
    function ToString:string;overload;
    class function ToString(AColor: TColor):string;overload;
    class function ToString(AColor: TAlphaColor):string;overload;
  end;

  TMapMarker = class
  private
    FPoint: TMapPoint;
    FColor: TColor;
    FSize: TMapMarkerSize;
    FText: string;
  public
    constructor Create(ALatitude, ALongitude: double);
    destructor Destroy;override;
    function ToString: string;
    property Point: TMapPoint read FPoint;
    property Color: TColor read FColor write FColor;
    property Size: TMapMarkerSize read FSize write FSize;
    property Text: string read FText write FText;
  end;

  TMapPath = class
  private
    FPathData: TObjectList<TMapPoint>;
    FFillColor: TAlphaColor;
    FColor: TAlphaColor;
    FWeight: byte;
    FFillAlpha: byte;
    FColorAlpha: byte;
  public
    constructor Create;
    destructor Destroy;override;
    function ToString:string;
    property PathData: TObjectList<TMapPoint> read FPathData;
    property FillColor: TAlphaColor read FFillColor write FFillColor;
    property Color: TAlphaColor read FColor write FColor;
    property Weight: byte read FWeight write FWeight;
    property FillAlpha: byte read FFillAlpha write FFillAlpha;
    property ColorAlpha: byte read FColorAlpha write FColorAlpha;
  end;

  TGoogleMap = class
  private
    FSensor: boolean;
    FKey: string;
    FZoom: byte;
    FScale: byte;
    FCenter: TMapPoint;
    FWidth: integer;
    FHeight: integer;
    FMapFormat:TMapFormat;
    FMapType: TMapType;
    FMarkers: TObjectList<TMapMarker>;
    FPaths: TObjectList<TMapPath>;
    procedure SetZoom(const Value: byte);
    procedure SetScale(const Value: byte);
    function GetURL: string;
  public
    constructor Create;
    destructor Destroy;override;
    property Sensor: boolean read FSensor write FSensor;
    property Key: string read FKey write FKey;
    property Zoom: byte read FZoom write SetZoom;
    property Scale: byte read FScale write SetScale;
    property Center: TMapPoint read FCenter;
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property MapFormat:TMapFormat read FMapFormat write FMapFormat;
    property MapType: TMapType read FMapType write FMapType;
    property Markers: TObjectList<TMapMarker> read FMarkers;
    property Paths: TObjectList<TMapPath> read FPaths;
    property URL: string read GetURL;
  end;

implementation

{ TMapPoint }

constructor TMapPoint.Create(ALatitude, ALongitude: double);
begin
  inherited Create;
  FLatitude:=ALatitude;
  FLongitude:=ALongitude;
end;

destructor TMapPoint.Destroy;
begin
  inherited;
end;

function TMapPoint.ToString: string;
begin
  Result:=ToString(MAP_POINT_DEFAULT_PRECISION,MAP_POINT_DEFAULT_DIGITS)
end;

function TMapPoint.ToString(Precision, Digits: Integer): string;
var AFormatSettings: TFormatSettings;
begin
  AFormatSettings.DecimalSeparator:='.';
  Result:=FloatToStrF(FLatitude,fffixed, Precision, Digits, AFormatSettings)+','+FloatToStrF(FLongitude,fffixed, Precision, Digits, AFormatSettings);
end;

function TMapPoint.Validate: boolean;
begin
  //Ўирота может принимать значение от -90 до 90, а долгота Ц от -180 до 180.
  Result:=((FLatitude>=-90)and(FLatitude<=90))and
          ((FLongitude>=-180)and(FLongitude<=180));
end;

{ TGoogleMap }

constructor TGoogleMap.Create;
begin
  inherited;
  FCenter:=TMapPoint.Create(Low(integer),low(integer));
  FMarkers:=TObjectList<TMapMarker>.Create;
  FPaths:=TObjectList<TMapPath>.Create;
  FHeight:=MAP_DEFAULT_HEIGHT;
  FWidth:=MAP_DEFAULT_WIDTH;
  FScale:=0;
  FZoom:=0;
end;

destructor TGoogleMap.Destroy;
begin
  FPaths.Free;
  FMarkers.Free;
  FCenter.Free;
  inherited;
end;

function TGoogleMap.GetURL: string;
var AAttributes: TStringList;
  I: Integer;
begin
  Result:=MAP_BASE_URL;
  AAttributes:=TStringList.Create;
  try
    if FCenter.Validate then
      AAttributes.Values['center']:=FCenter.ToString();
    AAttributes.Values['size']:=IntToStr(FWidth)+'x'+IntToStr(FHeight);
    if FScale>0 then
      AAttributes.Values['scale']:=IntToStr(FScale);
    AAttributes.Values['sensor']:=LowerCase(BoolToStr(FSensor,True));
    case FMapFormat of
      mfPNG8: AAttributes.Values['format']:='png8';
      mfPNG32: AAttributes.Values['format']:='png32';
      mfGIF: AAttributes.Values['format']:='gif';
      mfJPG: AAttributes.Values['format']:='jpg';
      mfBaseline: AAttributes.Values['format']:='jpg-baseline';
    end;
    case FMapType of
      mtSatellite: AAttributes.Values['maptype']:='satellite';
      mtTerrain: AAttributes.Values['maptype']:='terrain';
      mtHybrid: AAttributes.Values['maptype']:='hybrid';
    end;
    if FZoom>0 then
      AAttributes.Values['zoom']:=IntToStr(FZoom);
    for I := 0 to FMarkers.Count-1 do
      AAttributes.Add('markers='+FMarkers[i].ToString);

    for I := 0 to FPaths.Count-1 do
      AAttributes.Add('path='+FPaths[i].ToString);


    AAttributes.Delimiter:='&';
    Result:=Result+'?'+AAttributes.DelimitedText;
  finally
    AAttributes.Free;
  end;
end;

procedure TGoogleMap.SetScale(const Value: byte);
begin
  if (Value>0) and (Value<=4) then
    FScale := Value
  else
    raise EMapException.Create('Error Message');
end;

procedure TGoogleMap.SetZoom(const Value: byte);
begin
  if Value<=21 then
    FZoom := Value
  else
    raise EMapException.Create('Error Message');
end;

{ TMapMarker }

constructor TMapMarker.Create(ALatitude, ALongitude: double);
begin
  inherited Create;
  FColor:=TColorRec.SystemColor;
  FPoint:=TMapPoint.Create(ALatitude,ALongitude);
end;

destructor TMapMarker.Destroy;
begin
  FPoint.Free;
  inherited;
end;

function TMapMarker.ToString: string;
var AColorAttr: string;
    ASizeAttr: string;
begin
  Result:=EmptyStr;
  if FColor=TColorRec.SystemColor then
    AColorAttr:=EmptyStr
  else
    AColorAttr:='color:'+TMapColor.ToString(FColor)+MAP_ATTR_DELIMITER;

  case FSize of
    msMid: ASizeAttr:=EmptyStr;
    msTiny: ASizeAttr:='size:tiny'+MAP_ATTR_DELIMITER;
    msSmall: ASizeAttr:='size:small'+MAP_ATTR_DELIMITER;
  end;
  if AColorAttr<>EmptyStr then
    Result:=Result+AColorAttr;
  if ASizeAttr<>EmptyStr then
    Result:=Result+ASizeAttr;
  if FText<>EmptyStr then
    Result:=Result+'label:'+FText+MAP_ATTR_DELIMITER;
  Result:=Result+FPoint.ToString()
end;

{ TMapColor }

constructor TMapColor.Create(AColor: TColor);
begin
  inherited Create;
  FColor:=AColor;
end;

function TMapColor.ToString: string;
begin
  Result:=ToString(FColor)
end;

class function TMapColor.ToString(AColor: TColor): string;
begin
  Result:='0x'+IntToHex(TColorRec(AColor).R, 2)+
       IntToHex(TColorRec(AColor).G, 2)+
       IntToHex(TColorRec(AColor).B, 2);
end;

class function TMapColor.ToString(AColor: TAlphaColor): string;
begin
 Result:='0x'+IntToHex(TColorRec(AColor).R, 2)+
       IntToHex(TColorRec(AColor).G, 2)+
       IntToHex(TColorRec(AColor).B, 2)+
       IntToHex(TColorRec(AColor).A, 2);
end;

{ TMapPath }

constructor TMapPath.Create;
begin
  inherited;
  FPathData:=TObjectList<TMapPoint>.Create;
  FFillColor:=TAlphaColorRec.Null;
  FColor:=TAlphaColorRec.Red;
  FWeight:=5;
end;

destructor TMapPath.Destroy;
begin
  FPathData.Free;
  inherited;
end;

function TMapPath.ToString: string;
var AAtributes: TStringList;
  I: Integer;
begin
  Result:=EmptyStr;
  AAtributes:=TStringList.Create;
  try
    if FColor<>TAlphaColorRec.Red then
      begin
        TAlphaColorRec(FColor).A :=FColorAlpha;
        AAtributes.Add('color:'+TMapColor.ToString(FColor));
      end;
    if FWeight<>5 then
      AAtributes.Add('weight:'+IntToStr(FWeight));
    if FFillColor<>TAlphaColorRec.Null then
      begin
        TAlphaColorRec(FFillColor).A :=FFillAlpha;
        AAtributes.Add('fillcolor:'+TMapColor.ToString(FFillColor));
      end;
    for I := 0 to FPathData.Count-1 do
      AAtributes.Add(FPathData[i].ToString());
    AAtributes.Delimiter:=MAP_ATTR_DELIMITER;
    Result:=AAtributes.DelimitedText;
  finally
    AAtributes.Free;
  end;
end;

end.
