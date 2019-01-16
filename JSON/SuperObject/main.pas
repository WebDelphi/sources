unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, superobject;

const
  cRegionNameID = 1;
  cCityURLID = 1;
  cCityNameID = 0;
  cCitiesArrID = 5;

type
  TRegions = class
  private
    FJSONObject: ISuperObject;
    FAvlEnum:  TSuperAvlIterator;
    function GetRegionObject(const ARegion: string; out RegionID:integer): ISuperObject;
  public
    constructor Create(const AJsonString: string);
    destructor Destroy; override;
    /// <summary>
    ///   Заполняет список List названиями регионов
    /// </summary>
    function GetRegions(List: TStrings):integer;
    /// <summary>
    ///   Заполняет список List названиями городов, относящихся к области/краю ARegion
    /// </summary>
    /// <returns>
    ///   Возвращает ID региона в основном объекта JSON
    /// </returns>
    function GetCities(const ARegion: string; List:TStrings):integer;
    /// <summary>
    ///   Возвращает название города на латинице (URL города)
    /// </summary>
    function GetCityURL(const ARegionID:integer; ACity: string):string;
end;


type
  TForm8 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Label3: TLabel;
    ComboBox2: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private
    Regions: TRegions;
    CurrentRegion: integer;
  public
    { Public declarations }
  end;

var
  Form8: TForm8;


implementation

{$R *.dfm}

procedure TForm8.Button1Click(Sender: TObject);
var Stream: TStringStream;
begin
  if OpenDialog1.Execute then
    begin
      Edit1.Text:=OpenDialog1.FileName;
      Stream:=TStringStream.Create;
      try
      Stream.LoadFromFile(Edit1.Text);
      Regions:=TRegions.Create(Utf8ToAnsi(Stream.DataString));
      Regions.GetRegions(ComboBox1.Items);
      finally
        Stream.Free;
      end;
    end;
end;

{ TRegions }

constructor TRegions.Create(const AJsonString: string);
begin
  inherited Create;
  FJSONObject:=TSuperObject.ParseString(PChar(AJsonString),false);
  if not Assigned(FJSONObject) then
    raise Exception.Create('Невозможно распарсить JSON')
  else
    FAvlEnum:=FJSONObject.AsObject.GetEnumerator;
end;

destructor TRegions.Destroy;
begin
  if Assigned(FAvlEnum) then
    FAvlEnum.Free;
  inherited;
end;

function TRegions.GetCities(const ARegion: string; List:TStrings): integer;
var RegionObject, CityObject: ISuperObject;
    ID: integer;
    CityEnum: TSuperEnumerator;
begin
   if (not Assigned(FAvlEnum))or(not Assigned(List)) then Exit;
   List.Clear;
   RegionObject:=GetRegionObject(ARegion,ID);
   if Assigned(RegionObject) then
     begin
        CityObject:=RegionObject.AsArray.O[cCitiesArrID];
        if Assigned(CityObject) then
          begin
            CityEnum:=CityObject.GetEnumerator;
            try
              while CityEnum.MoveNext do
                List.Add(CityEnum.Current.AsArray.S[cCityNameID])
            finally
              CityEnum.Free;
            end;
            Exit(ID);
          end;
     end;
end;

function TRegions.GetCityURL(const ARegionID: integer; ACity: string): string;
var Region: TSuperArray;
    CityEnum: TSuperEnumerator;
begin
  if not Assigned(FJSONObject) then Exit;
  Region:=FJSONObject.A[IntToStr(ARegionID)];
  if Assigned(Region) then
    begin
      CityEnum:=Region.O[5].GetEnumerator;
      try
        while CityEnum.MoveNext do
          begin
            if SameText(CityEnum.Current.AsArray.S[cCityNameID],ACity) then
              begin
                Result:=CityEnum.Current.AsArray.S[cCityURLID];
                break;
              end;
          end;
      finally
         CityEnum.Free;
      end;
    end;
end;

function TRegions.GetRegionObject(const ARegion: string; out RegionID:integer): ISuperObject;
begin
  if not Assigned(FAvlEnum) then Exit;
  FAvlEnum.First;
  repeat
     if SameText(FAvlEnum.Current.Value.AsArray.S[1], ARegion)then
       begin
         RegionID:=StrToInt(FAvlEnum.Current.Name);
         Exit(FAvlEnum.Current.Value);
       end;
  until not FAvlEnum.MoveNext;
end;

function TRegions.GetRegions(List: TStrings): integer;
begin
  Result:=0;
  if (not Assigned(FAvlEnum))or(not Assigned(List)) then Exit;
  List.Clear;
  FAvlEnum.First;
  repeat
    List.Add(FAvlEnum.Current.Value.AsArray.S[cRegionNameID]);
  until not FAvlEnum.MoveNext;
  Result:=List.Count;
end;

procedure TForm8.ComboBox1Change(Sender: TObject);
begin
  CurrentRegion:=Regions.GetCities(ComboBox1.Items.Strings[ComboBox1.ItemIndex],ComboBox2.Items);
end;

procedure TForm8.ComboBox2Change(Sender: TObject);
begin
  Label5.Caption:=Regions.GetCityURL(CurrentRegion,ComboBox2.Items.Strings[ComboBox2.ItemIndex])
end;

procedure TForm8.FormDestroy(Sender: TObject);
begin
  if Assigned(Regions) then
    Regions.Free;
end;

initialization
  ReportMemoryLeaksOnShutdown:=true;

end.
