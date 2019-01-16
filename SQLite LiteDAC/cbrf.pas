unit cbrf;

interface

uses System.Classes, System.SysUtils, Generics.Collections, Xml.xmldom, Xml.XMLIntf,
     Xml.omnixmldom, Xml.XMLDoc, idHTTP;

type
  TValute = class
  private
    FCode: string;
    FNominal: cardinal;
    FRuName: string;
    FEngName: string;
  public
    constructor Create(const ACode, ARuName, AEngName: string; ANominal: cardinal);
    destructor Destroy;override;
    property Code: string read FCode write FCode;
    property Nominal: cardinal read FNominal write FNominal;
    property RuName: string read FRuName write FRuName;
    property EngName: string read FEngName write FEngName;
  end;

  TRate = class
  private
    FDate: TDate;
    FNomanal: cardinal;
    FCode: string;
    FValue: currency;
  public
    property Date: TDate read FDate write FDate;
    property Nomanal: cardinal read FNomanal write FNomanal;
    property Code: string read FCode write FCode;
    property Value: currency read FValue write FValue;
  end;

  TCBRF = class
  private
    FHTTP: TIdHTTP;
    FValutes: TObjectList<TValute>;
    FRate: TObjectList<TRate>;
  public
    constructor Create;
    destructor Destroy;override;
    //загрузка справочника валют
    procedure LoadValutes;
    //загрузка курса валюты
    procedure LoadRate(ACode: string; AStart, AEnd: TDate);

    property Valutes: TObjectList<TValute> read FValutes;
    property Rate: TObjectList<TRate> read FRate;
end;

implementation

uses Forms;

const VALUTES_DICT_URL = 'http://www.cbr.ru/scripts/XML_valFull.asp';
      RATES_URL = 'http://www.cbr.ru/scripts/XML_dynamic.asp?date_req1=%s&date_req2=%s&VAL_NM_RQ=%s';

{ TValute }

constructor TValute.Create(const ACode, ARuName, AEngName: string;
  ANominal: cardinal);
begin
  inherited Create;
  FCode:=ACode;
  FNominal:=ANominal;
  FRuName:=ARuName;
  FEngName:=AEngName;
end;

destructor TValute.Destroy;
begin
  inherited;
end;

{ TCBRF }

constructor TCBRF.Create;
begin
  inherited;
  FValutes:=TObjectList<TValute>.Create;
  FRate:=TObjectList<TRate>.Create;
  FHTTP:=TIdHTTP.Create(nil);
end;

destructor TCBRF.Destroy;
begin
  FreeAndNil(FHTTP);
  FreeAndNil(FRate);
  FreeAndNil(FValutes);
  inherited;
end;

procedure TCBRF.LoadRate(ACode: string; AStart, AEnd: TDate);
var URL: string;
    AFormatSettings: TFormatSettings;
    AXMLData: TXMLDocument;
    AXML: TStringStream;
    Root, ValNode: IXMLNode;
    I: Integer;
begin
  //  RATES_URL = 'http://www.cbr.ru/scripts/XML_dynamic.asp?date_req1=%s&date_req2=%s&VAL_NM_RQ=%s';
  AFormatSettings.DateSeparator:='/';
  URL:=Format(RATES_URL,[FormatDateTime('dd/mm/yyyy', AStart, AFormatSettings),FormatDateTime('dd/mm/yyyy', AEnd, AFormatSettings), ACode]);
  AXML:=TStringStream.Create('', TEncoding.ANSI);
  try
    //http://www.cbr.ru/scripts/XML_dynamic.asp?date_req1=12/08/2016&date_req2=12/08/2016&VAL_NM_RQ=R01235
    FHTTP.Get(URL, AXML);
    AXML.SaveToFile('curse.txt');
    AXMLData:=TXMLDocument.Create(Application);
    try
      AXMLData.LoadFromStream(AXML);
      Root:=nil;
      Root:=AXMLData.ChildNodes.FindNode('ValCurs');
      if Assigned(Root) then
        begin
          for I := 0 to Pred(Root.ChildNodes.Count) do
            begin
              ValNode:=Root.ChildNodes.Get(i);
              FRate.Add(TRate.Create);
              FRate.Last.Date:=StrToDate(ValNode.AttributeNodes.FindNode('Date').Text);
              FRate.Last.Nomanal:=ValNode.ChildValues['Nominal'];
              FRate.Last.Code:=ValNode.Attributes['Id'];
              FRate.Last.Value:=ValNode.ChildValues['Value'];
            end;
        end;
    finally
      FreeAndNil(AXMLData)
    end;
  finally
    FreeAndNil(AXML)
  end;
end;

procedure TCBRF.LoadValutes;
var AXMLData: TXMLDocument;
    AXML: TStringStream;
    Root, ValNode: IXMLNode;
    I: Integer;
    ACode: string;
    ARuName, AEngName: string;
    ANominal: cardinal;
begin
  AXML:=TStringStream.Create('', TEncoding.ANSI);
  try
    FHTTP.Get(VALUTES_DICT_URL, AXML);
    AXMLData:=TXMLDocument.Create(Application);
    try
      AXMLData.LoadFromStream(AXML);
      Root:=nil;
      Root:=AXMLData.ChildNodes.FindNode('Valuta');
      if Assigned(Root) then
        begin
          for I := 0 to Pred(Root.ChildNodes.Count) do
            begin
              ValNode:=Root.ChildNodes.Get(i);
              ACode:=ValNode.ChildValues['ParentCode'];
              ARuName:=ValNode.ChildValues['Name'];
              AEngName:=ValNode.ChildValues['EngName'];
              ANominal:=ValNode.ChildValues['Nominal'];
              FValutes.Add(TValute.Create(ACode,ARuName,AEngName,ANominal));
            end;
        end;
    finally
      FreeAndNil(AXMLData)
    end;
  finally
    FreeAndNil(AXML)
  end;
//  AXMLData.Refresh;
end;

end.
