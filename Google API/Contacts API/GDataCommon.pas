unit GDataCommon;

interface

uses NativeXML, Classes, XMLIntf, StrUtils, SysUtils, GHelper, Variants;

const
  cGDTagNames: array [0 .. 49] of string = ('gd:country', 'gd:additionalName',
    'gd:name', 'gd:email', 'gd:extendedProperty', 'gd:geoPt', 'gd:im',
    'gd:orgName', 'gd:orgTitle', 'gd:organization', 'gd:originalEvent',
    'gd:phoneNumber', 'gd:postalAddress', 'gd:rating', 'gd:recurrence',
    'gd:reminder', 'gd:resourceId', 'gd:when', 'gd:agent', 'gd:housename',
    'gd:street', 'gd:pobox', 'gd:neighborhood', 'gd:city', 'gd:subregion',
    'gd:region', 'gd:postcode', 'gd:formattedAddress',
    'gd:structuredPostalAddress', 'gd:entryLink', 'gd:where', 'gd:familyName',
    'gd:givenName', 'gd:namePrefix', 'gd:nameSuffix', 'gd:fullName',
    'gd:orgDepartment', 'gd:orgJobDescription', 'gd:orgSymbol',
    'gd:famileName', 'gd:eventStatus', 'gd:visibility', 'gd:transparency',
    'gd:attendeeType', 'gd:attendeeStatus', 'gd:comments', 'gd:deleted',
    'gd:feedLink', 'gd:who', 'gd:recurrenceException');

type
  TgdEnum = (egdCountry, egdAdditionalName, egdName, egdEmail,
    egdExtendedProperty, egdGeoPt, egdIm, egdOrgName, egdOrgTitle,
    egdOrganization, egdOriginalEvent, egdPhoneNumber, egdPostalAddress,
    egdRating, egdRecurrence, egdReminder, egdResourceId, egdWhen, egdAgent,
    egdHousename, egdStreet, egdPobox, egdNeighborhood, egdCity, egdSubregion,
    egdRegion, egdPostcode, egdFormattedAddress, egdStructuredPostalAddress,
    egdEntryLink, egdWhere, egdFamilyName, egdGivenName, egdNamePrefix,
    egdNameSuffix, egdFullName, egdOregdepartment, egdOrgJobDescription,
    egdOrgSymbol, egdFamileName, egdEventStatus, egdVisibility,
    egdTransparency, egdAttendeeType, egdAttendeeStatus, egdComments,
    egdDeleted, egdFeedLink, egdWho, egdRecurrenceException);

type
  TGDataTags = set of TgdEnum;

type
  TEventStatus = (esCanceled, esConfirmed, esTentative);

const
  cEventStatuses: array [0 .. 2] of string = (
    'http://schemas.google.com/g/2005#event.canceled',
    'http://schemas.google.com/g/2005#event.confirmed',
    'http://schemas.google.com/g/2005#event.tentative');

type
  TgdEventStatus = class(TPersistent)
    private
      FValue: string;
      FStatus: TEventStatus;
      procedure SetStatus(aStatus:TEventStatus);
      procedure SetValue(aValue:string);
    public
      Constructor Create(const ByNode:IXMLNode=nil);
      procedure ParseXML(Node: IXMLNode);
      function AddToXML(Root:IXMLNode):IXMLNode;
      property Value:string read FValue write SetValue;
      property Ststus:TEventStatus read FStatus write SetStatus;
  end;

type
  TVisibility = (vConfidential, vDefault, vPrivate, vPublic);

const
  cVisibility: array [0 .. 3] of string = (
    'http://schemas.google.com/g/2005#event.confidential',
    'http://schemas.google.com/g/2005#event.default',
    'http://schemas.google.com/g/2005#event.private',
    'http://schemas.google.com/g/2005#event.public');

type
  TgdVisibility = class
    private
      FValue: string;
      FVisible: TVisibility;
    procedure SetValue(aValue:string);
      procedure SetVisible(aVisible:TVisibility);
    public
      Constructor Create(const ByNode:IXMLNode=nil);
      procedure ParseXML(Node: IXMLNode);
      function AddToXML(Root:IXMLNode):IXMLNode;
      property Value: string read FValue write SetValue;
      property Visibility: TVisibility read FVisible write SetVisible;
  end;

type
  TTransparency = (tOpaque, tTransparent);

const
  cTransparency: array [0 .. 1] of string = (
    'http://schemas.google.com/g/2005#event.opaque',
    'http://schemas.google.com/g/2005#event.transparent');

type
  TgdTransparency = class(TPersistent)
    private
      FValue: string;
      FTransparency: TTransparency;
      procedure SetValue(aValue:string);
      procedure SetTransp(aTransp:TTransparency);
    public
      Constructor Create(const ByNode:IXMLNode=nil);
      procedure ParseXML(Node: IXMLNode);
      function AddToXML(Root:IXMLNode):IXMLNode;
      property Value: string read FValue write SetValue;
      property Transparency: TTransparency read FTransparency write SetTransp;
  end;

type
  TAttendeeType = (aOptional, aRequired);

const
  cAttendeeType: array [0 .. 1] of string = (
    'http://schemas.google.com/g/2005#event.optional',
    'http://schemas.google.com/g/2005#event.required');

type
  TgdAttendeeType = class
    private
      FValue: string;
      FAttType: TAttendeeType;
      procedure SetValue(aValue:string);
      procedure SetType(aStatus:TAttendeeType);
    public
      Constructor Create(const ByNode:IXMLNode);
      procedure ParseXML(Node: IXMLNode);
      function AddToXML(Root:IXMLNode):IXMLNode;
      property Value: string read FValue write SetValue;
      property AttendeeType: TAttendeeType read FAttType write SetType;
  end;

type
  TAttendeeStatus = (asAccepted, asDeclined, asInvited, asTentative);

const
  cAttendeeStatus: array [0 .. 3] of string = (
    'http://schemas.google.com/g/2005#event.accepted',
    'http://schemas.google.com/g/2005#event.declined',
    'http://schemas.google.com/g/2005#event.invited',
    'http://schemas.google.com/g/2005#event.tentative');

type
  TgdAttendeeStatus = class(TPersistent)
    private
      FValue: string;
      FAttendeeStatus: TAttendeeStatus;
      procedure SetValue(aValue:string);
      procedure SetStatus(aStatus:TAttendeeStatus);
    public
      Constructor Create(const ByNode:IXMLNode);
      procedure ParseXML(Node: IXMLNode);
      function AddToXML(Root:IXMLNode):IXMLNode;
      property Value: string read FValue write SetValue;
      property Status: TAttendeeStatus read FAttendeeStatus write SetStatus;
  end;

type
  TEntryTerms = (ttAny, ttContact, ttEvent, ttMessage, ttType);

type
  TgdCountry = class(TPersistent)
    private
     FCode: string;
     FValue: string;
    public
     Constructor Create(const ByNode:TXMLNode);
     procedure   ParseXML(Node: TXMLNode);
     function    AddToXML(Root:TXMLNode):TXMLNode;
     property Code: string read FCode write FCode;
     property Value: string read FValue write FValue;
  end;

type
  TgdAdditionalNameStruct = TTextTag;

type
  TgdFamilyName = TTextTag;
  TgdGivenName = TTextTag;
//  TgdFamileNameStruct = string;
  TgdNamePrefix = TTextTag;
  TgdNameSuffix = TTextTag;
  TgdFullName = TTextTag;
  TgdOrgDepartment = TTextTag;
  TgdOrgJobDescription = TTextTag;
  TgdOrgSymbol = TTextTag;

type
  TgdName = class(TPersistent)
  private
    FGivenName: TTextTag;
    FAdditionalName: TTextTag;
    FFamilyName: TTextTag;
    FNamePrefix: TTextTag;
    FNameSuffix: TTextTag;
    FFullName: TTextTag;
    function GetFullName:string;
  public
    constructor Create(ByNode: TXMLNode=nil);
    procedure   ParseXML(const Node: TXmlNode);
    function AddToXML(Root:TXMLNode):TXmlNode;
    property GivenName: TTextTag read FGivenName write FGivenName;
    property AdditionalName: TTextTag read FAdditionalName write FAdditionalName;
    property FamilyName: TTextTag read FFamilyName write FFamilyName;
    property NamePrefix: TTextTag read FNamePrefix write FNamePrefix;
    property NameSuffix: TTextTag read FNameSuffix write FNameSuffix;
    property FullName: TTextTag read FFullName write FFullName;
    property FullNameString: string read GetFullName;
end;

type
  TTypeElement = (ttHome,ttOther, ttWork);

type
  TgdEmail = class(TPersistent)
    private
      FAddress: string;
      FEmailType: TTypeElement;
      FLabel: string;
      FRel: string;
      FPrimary: boolean;
      FDisplayName:string;
      const RelValues: array[0..2]of string=('home','other','work');
      procedure SetRel(const aRel:string);
      procedure SetEmailType(aType:TTypeElement);
    public
      constructor Create(ByNode: TXMLNode=nil);
      procedure   ParseXML(const Node: TXmlNode);
      function AddToXML(Root:TXMLNode):TXmlNode;
      property Address : string read FAddress write FAddress;
      property Labl:string read FLabel write FLabel;
      property Rel: string read FRel write SetRel;
      property DisplayName: string read FDisplayName write FDisplayName;
      property Primary: boolean read FPrimary write FPrimary;
      property EmailType:TTypeElement read FEmailType write SetEmailType;
  end;

type
  TgdExtendedPropertyStruct = record
    Name: string;
    Value: string;
  end;

type
  TgdGeoPtStruct = record
    Elav: extended;
    Labels: string;
    Lat: extended;
    Lon: extended;
    Time: TDateTime;
  end;

type
  TgdIm = class(TPersistent)
  private
    FAddress: string;
    FLabel: string;
    FRel: string;
    FProtocol: string;
    FPrimary: boolean;
  public
    constructor Create(ByNode: TXMLNode=nil);
    procedure   ParseXML(const Node: TXmlNode);
    function AddToXML(Root:TXMLNode):TXmlNode;
    property Address: string read FAddress write FAddress;
    property iLabel: string read FLabel write FLabel;
    property Rel: string read FRel write FRel;
    property Protocol: string read FProtocol write FProtocol;
    property Primary: boolean read FPrimary write FPrimary;
end;

  TgdOrgName = TTextTag;
  TgdOrgTitle = TTextTag;

type
  TgdOrganization = class(TPersistent)
  private
     FLabel: string;
     Frel: string;
     Fprimary: boolean;
     ForgName: TgdOrgName;
     ForgTitle: TgdOrgTitle;
  public
    constructor Create(ByNode: TXMLNode=nil);
    procedure   ParseXML(const Node: TXmlNode);
    function AddToXML(Root:TXMLNode):TXmlNode;
    property Labl: string read FLabel write FLabel;
    property Rel: string Read FRel write FRel;
    property Primary: boolean read Fprimary write Fprimary;
    property OrgName: TgdOrgName read ForgName write ForgName;
    property OrgTitle: TgdOrgTitle read ForgTitle write ForgTitle;
end;

type
  TgdOriginalEventStruct = record
    id: string;
    href: string;
  end;

type
  TgdPhoneNumber = class(TPersistent)
    private
      FPrimary: boolean;
      FLabel: string;
      Frel: string;
      FUri: string;
      FValue: string;
      const RelValues: array[0..19]of string=('assistant','callback','car','company_main','fax',
      'home','home_fax','isdn','main','mobile','other','other_fax','pager',
      'radio','telex','tty_tdd','work','work_fax','work_mobile','work_pager');
    public
      constructor Create(ByNode: TXMLNode=nil);
      procedure   ParseXML(const Node: TXmlNode);
      function AddToXML(Root:TXMLNode):TXmlNode;
      property Primary: boolean read FPrimary write FPrimary;
      property Labl: string read FLabel write FLabel;
      property Rel: string read Frel write Frel;
      property Uri: string read FUri write FUri;
      property Text: string read FValue write FValue;
  end;

type
  TgdPostalAddressStruct = record
    Labels: string;
    rel: string;
    primary: boolean;
    Text: string;
  end;

type
  TgdRatingStruct = record
    Average: extended;
    Max: integer;
    Min: integer;
    numRaters: integer;
    rel: string;
    Value: integer;
  end;

type
  TgdRecurrence = class(TPersistent)
  private
    FText: TStringList;
  public
    Constructor Create(const ByNode:IXMLNode=nil);
    procedure ParseXML(Node: IXMLNode);
    function AddToXML(Root:IXMLNode):IXMLNode;
    property Text: TStringList read FText write FText;
end;

const
  cMethods : array [0..2]of string =('alert','email','sms');
type
  TMethod = (tmAlert, tmEmail, tmSMS);
  TRemindPeriod = (tpDays, tpHours, tpMinutes);

type
  TgdReminder = class (TPersistent)
    private
      FabsoluteTime: TDateTime;
      Fmethod:       TMethod;
      FPeriod:       TRemindPeriod;
      FPeriodValue:  integer;
    public
      Constructor Create(const ByNode:IXMLNode);
      procedure ParseXML(Node: IXMLNode);
      function  AddToXML(Root:IXMLNode):IXMLNode;
      property  AbsTime: TDateTime read FabsoluteTime write FabsoluteTime;
      property  Method: TMethod read Fmethod write Fmethod;
      property  Period: TRemindPeriod read FPeriod write FPeriod;
      property  PeriodValue:integer read FPeriodValue write FPeriodValue;
  end;

type
  TgdResourceIdStruct = string;

type
  TgdWhen = class(TPersistent)
    private
      FendTime: TDateTime;
      FstartTime: TDateTime;
      FvalueString: string;
    public
      Constructor Create(const ByNode:TXMLNode=nil);
      procedure ParseXML(Node: TXMLNode);
      function AddToXML(Root:TXMLNode):TXMLNode;
      property endTime: TDateTime read FendTime write FendTime;
      property startTime: TDateTime read FstartTime write FstartTime;
      property valueString: string read FvalueString write FvalueString;
  end;

type
  TgdAgent = TTextTag;
  TgdHousename = TTextTag;
  TgdStreet = TTextTag;
  TgdPobox = TTextTag;
  TgdNeighborhood = TTextTag;
  TgdCity = TTextTag;
  TgdSubregion = TTextTag;
  TgdRegion = TTextTag;
  TgdPostcode = TTextTag;
  TgdFormattedAddress = TTextTag;

type
  TgdStructuredPostalAddress = class(TPersistent)
    private
     FRel: string;
     FMailClass: string;
     FUsage: string;
     Flabel: string;
     Fprimary: boolean;
     FAgent: TgdAgent;
     FHouseName: TgdHousename;
     FStreet: TgdStreet;
     FPobox: TgdPobox;
     FNeighborhood: TgdNeighborhood;
     FCity: TgdCity;
     FSubregion: TgdSubregion;
     FRegion: TgdRegion;
     FPostcode: TgdPostcode;
     FCoutry: TgdCountry;
     FFormattedAddress: TgdFormattedAddress;
    public
      Constructor Create(const ByNode:TXMLNode=nil);
      procedure ParseXML(Node: TXMLNode);
      function AddToXML(Root:TXMLNode):TXMLNode;
      property Rel: string read FRel write FRel;
      property MailClass: string read FMailClass write FMailClass;
      property Usage: string read FUsage write FUsage;
      property Labl: string read Flabel write Flabel;
      property Primary: boolean read FPrimary write FPrimary;
      property Agent: TgdAgent read FAgent write FAgent;
      property HouseName: TgdHousename read FHouseName write FHouseName;
      property Street: TgdStreet read FStreet write FStreet;
      property Pobox: TgdPobox read FPobox write FPobox;
      property Neighborhood: TgdNeighborhood read FNeighborhood write FNeighborhood;
      property City: TgdCity read FCity write FCity;
      property Subregion: TgdSubregion read FSubregion write FSubregion;
      property Region: TgdRegion read FRegion write FRegion;
      property Postcode: TgdPostcode read FPostcode write FPostcode;
      property Coutry: TgdCountry read FCoutry write FCoutry;
      property FormattedAddress: TgdFormattedAddress read FFormattedAddress write FFormattedAddress;
  end;

type
  TgdEntryLink = class(TPersistent)
    private
      Fhref: string;
      FReadOnly: boolean;
      Frel: string;
      FAtomEntry: IXMLNode;
    public
      Constructor Create(const ByNode:IXMLNode);
      procedure ParseXML(Node: IXMLNode);
      function AddToXML(Root:IXMLNode):IXMLNode;
      property Href: string read Fhref write Fhref;
      property OnlyRead: boolean read FReadOnly write FReadOnly;
      property Rel: string read Frel write Frel;
  end;

type
  TgdWhere = class(TPersistent)
    private
     Flabel: string;
     Frel: string;
     FvalueString: string;
     FEntryLink: TgdEntryLink;
    public
      Constructor Create(const ByNode:IXMLNode=nil);
      procedure ParseXML(Node: IXMLNode);
      function AddToXML(Root:IXMLNode):IXMLNode;
      property Labl:string read Flabel write Flabel;
      property Rel:string read FRel write FRel;
      property valueString: string read FvalueString write FvalueString;
      property EntryLink: TgdEntryLink read FEntryLink write FEntryLink;
  end;

const
  cWhoRel: array [1 .. 9] of string = (
    'http://schemas.google.com/g/2005#event.attendee',
    'http://schemas.google.com/g/2005#event.organizer',
    'http://schemas.google.com/g/2005#event.performer',
    'http://schemas.google.com/g/2005#event.speaker',
    'http://schemas.google.com/g/2005#message.bcc',
    'http://schemas.google.com/g/2005#message.cc',
    'http://schemas.google.com/g/2005#message.from',
    'http://schemas.google.com/g/2005#message.reply-to',
    'http://schemas.google.com/g/2005#message.to');

type
  TWhoRel = (twAttendee,twOrganizer,twPerformer,twSpeaker,twBcc,twCc,twFrom,twReply,twTo);

type
  TgdWho = class(TPersistent)
  private
    FEmail: string;
    Frel: string;
    FRelValue: TWhoRel;
    FvalueString: string;
    FAttendeeStatus: TgdAttendeeStatus;
    FAttendeeType: TgdAttendeeType;
    FEntryLink: TgdEntryLink;
    procedure SetRel(aRel:string);
    procedure SetRelValue(aRelValue:TWhoRel);
  public
    Constructor Create(const ByNode:IXMLNode=nil);
    procedure ParseXML(Node: IXMLNode);
    function AddToXML(Root:IXMLNode):IXMLNode;
    property Email: string read FEmail write FEmail;
    property Rel: string read Frel write SetRel;
    property RelValue: TWhoRel read FRelValue write SetRelValue;
    property valueString: string read FvalueString write FvalueString;
    property AttendeeStatus: TgdAttendeeStatus read FAttendeeStatus write FAttendeeStatus;
    property AttendeeType: TgdAttendeeType read FAttendeeType write FAttendeeType;
    property EntryLink: TgdEntryLink read FEntryLink write FEntryLink;
end;

function GetGDNodeType(cName: string): integer;

function gdAttendeeStatus(aXMLNode: IXMLNode): TgdAttendeeStatus;
function gdAttendeeType(aXMLNode: IXMLNode): TgdAttendeeType;
function gdTransparency(aXMLNode: IXMLNode): TgdTransparency;
function gdVisibility(aXMLNode: IXMLNode): TgdVisibility;
function gdEventStatus(aXMLNode: IXMLNode): TgdEventStatus;
function gdWhere(aXMLNode: IXMLNode):TgdWhere;
function gdWhen(aXMLNode: TXMLNode):TgdWhen;
function gdWho(aXMLNode: IXMLNode):TgdWho;
function gdRecurrence(aXMLNode: IXMLNode):TgdRecurrence;
function gdReminder(aXMLNode: IXMLNode):TgdReminder;

implementation

function gdReminder(aXMLNode: IXMLNode):TgdReminder;
begin
  Result:=TgdReminder.Create(aXMLNode);
end;

function gdRecurrence(aXMLNode: IXMLNode):TgdRecurrence;
begin
  Result:=TgdRecurrence.Create(aXMLNode);
end;

function gdWho(aXMLNode: IXMLNode):TgdWho;
begin
  Result:=TgdWho.Create(aXMLNode);
end;

function gdWhen(aXMLNode: TXMLNode):TgdWhen;
begin
  Result:=TgdWhen.Create(aXMLNode);
end;

function gdWhere(aXMLNode: IXMLNode):TgdWhere;
begin
  Result:=TgdWhere.Create(aXMLNode);
end;

function gdEventStatus(aXMLNode: IXMLNode): TgdEventStatus;
begin
  Result:=TgdEventStatus.Create(aXMLNode);
end;

function gdVisibility(aXMLNode: IXMLNode): TgdVisibility;
begin
  Result:=TgdVisibility.Create(aXMLNode);
end;

function gdTransparency(aXMLNode: IXMLNode): TgdTransparency;
begin
  Result:=TgdTransparency.Create(aXMLNode);
end;

function GetGDNodeType(cName: string): integer;
begin
  Result := AnsiIndexStr(cName, cGDTagNames);
end;

function gdAttendeeType(aXMLNode: IXMLNode): TgdAttendeeType;
begin
  Result:=TgdAttendeeType.Create(aXMLNode);
end;

function gdAttendeeStatus(aXMLNode: IXMLNode): TgdAttendeeStatus;
begin
  Result:=TgdAttendeeStatus.Create(aXMLNode);
end;

{ TgdWhere }

function TgdWhere.AddToXML(Root: IXMLNode): IXMLNode;
begin
  //��������� ����
  if Root=nil then Exit;
  Result:=Root.AddChild(cgdTagNames[ord(egdWhere)]);
  if Length(Flabel)>0 then
    Result.Attributes['label']:=Flabel;
  if Length(Frel)>0 then
    Result.Attributes['rel']:=Frel;
  if Length(FvalueString)>0 then
    Result.Attributes['valueString']:=FvalueString;
  if FEntryLink<>nil then
    if (FEntryLink.FAtomEntry<>nil)or(Length(FEntryLink.Fhref)>0) then
      FEntryLink.AddToXML(Result);
end;

constructor TgdWhere.Create(const ByNode: IXMLNode);
begin
inherited Create;
if ByNode=nil then Exit;
FEntryLink:=TgdEntryLink.Create(nil);
ParseXML(ByNode);
end;

procedure TgdWhere.ParseXML(Node: IXMLNode);
begin
if GetGDNodeType(Node.NodeName) <> ord(egdWhere) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdWhere)]]));
  try
    if Node.Attributes['label']<>null then
      Flabel:=Node.Attributes['label'];
    if Node.Attributes['rel']<>null then
      Flabel:=Node.Attributes['rel'];
    if Node.Attributes['valueString']<>null then
      FvalueString:=Node.Attributes['valueString'];
    if Node.ChildNodes.Count>0 then //���� �������� ���� � EntryLink
      begin
        FEntryLink.ParseXML(Node.ChildNodes.FindNode('gd:entry'));
      end;
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

{ TgdEntryLinkStruct }

function TgdEntryLink.AddToXML(Root: IXMLNode): IXMLNode;
begin
if Root=nil then Exit;
 Result:=Root.AddChild(cgdTagNames[ord(egdEntryLink)]);
 if Length(Trim(Fhref))>0 then
   Result.Attributes['href']:=Fhref;
 if Length(Trim(Frel))>0 then
   Result.Attributes['rel']:=Frel;
 Result.Attributes['readOnly']:=FReadOnly;
 if FAtomEntry<>nil then
   Result.ChildNodes.Add(FAtomEntry);
end;

constructor TgdEntryLink.Create(const ByNode: IXMLNode);
begin
  inherited Create;
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdEntryLink.ParseXML(Node: IXMLNode);
begin
if GetGDNodeType(Node.NodeName) <> ord(egdEntryLink) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdEntryLink)]]));
  try
    if Node.Attributes['href']<>null then
      Fhref:=Node.Attributes['href'];
    if Node.Attributes['rel']<>null then
      Frel:=Node.Attributes['rel'];
    if Node.Attributes['readOnly']<>null then
      FReadOnly:=Node.Attributes['readOnly'];
    if Node.ChildNodes.Count>0 then //���� �������� ���� � EntryLink
       FAtomEntry:=Node.ChildNodes.FindNode('entry');
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

{ TgdEventStatus }

function TgdEventStatus.AddToXML(Root: IXMLNode): IXMLNode;
begin
if Root=nil then Exit;
  Result:=Root.AddChild(cgdTagNames[ord(egdEventStatus)]);
  Result.Attributes['value']:=FValue;
end;

constructor TgdEventStatus.Create(const ByNode: IXMLNode);
begin
  inherited Create;
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdEventStatus.ParseXML(Node: IXMLNode);
begin
  if Node=nil then Exit;
  if GetGDNodeType(Node.NodeName) <> ord(egdEventStatus) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdEventStatus)]]));
  try
  //  ShowMessage(Node.Attributes['value']);
    FValue:=VarToStr(Node.Attributes['value']);
    FStatus:=TEventStatus(AnsiIndexStr(FValue, cEventStatuses));
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

procedure TgdEventStatus.SetStatus(aStatus: TEventStatus);
begin
  FStatus:=aStatus;
  FValue:=cEventStatuses[ord(aStatus)]
end;

procedure TgdEventStatus.SetValue(aValue: string);
begin
  if AnsiIndexStr(aValue, cEventStatuses)<=0 then
    raise Exception.Create(Format(rcErrMissValue, [cGDTagNames[ord(egdEventStatus)]]));
  FStatus:=TEventStatus(AnsiIndexStr(aValue, cEventStatuses));
  FValue:=aValue;
end;

{ TgdWhen }

function TgdWhen.AddToXML(Root: TXMLNode): TXMLNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(cgdTagNames[ord(egdWhen)]);
  Result.WriteAttributeString('startTime',DateTimeToServerDate(FstartTime));
  if FendTime>0 then
    Result.WriteAttributeString('endTime',DateTimeToServerDate(FendTime));
  if length(Trim(FvalueString))>0 then
    Result.WriteAttributeString('valueString',FvalueString);
end;

constructor TgdWhen.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdWhen.ParseXML(Node: TXMLNode);
begin
if Node=nil then Exit;
  if GetGDNodeType(Node.Name) <> ord(egdWhen) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdWhen)]]));
  try
    FendTime:=0;
    FstartTime:=0;
    FvalueString:='';
    if Node.HasAttribute('endTime') then
      FendTime:=ServerDateToDateTime(Node.ReadAttributeString('endTime'));
    FstartTime:=ServerDateToDateTime(Node.ReadAttributeString('startTime'));
    if Node.HasAttribute('valueString') then
      FvalueString:=Node.ReadAttributeString('valueString');
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TgdAttendeeStatus }

function TgdAttendeeStatus.AddToXML(Root: IXMLNode): IXMLNode;
begin
  if Root=nil then Exit;
  Result:=Root.AddChild(cgdTagNames[ord(egdAttendeeStatus)]);
  Result.Attributes['value']:=FValue;
end;

constructor TgdAttendeeStatus.Create(const ByNode: IXMLNode);
begin
  inherited Create;
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdAttendeeStatus.ParseXML(Node: IXMLNode);
begin
if Node=nil then Exit;
  if GetGDNodeType(Node.NodeName) <> ord(egdAttendeeStatus) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdAttendeeStatus)]]));
  try
    FValue := Node.Attributes['value'];
    FAttendeeStatus := TAttendeeStatus(AnsiIndexStr(FValue, cAttendeeStatus));
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

procedure TgdAttendeeStatus.SetStatus(aStatus: TAttendeeStatus);
begin
  FAttendeeStatus:=aStatus;
  FValue:=cAttendeeStatus[ord(aStatus)]
end;

procedure TgdAttendeeStatus.SetValue(aValue: string);
begin
  if AnsiIndexStr(aValue, cAttendeeStatus)<=0 then
    raise Exception.Create(Format(rcErrMissValue, [cGDTagNames[ord(egdAttendeeStatus)]]));
  FAttendeeStatus:=TAttendeeStatus(AnsiIndexStr(aValue, cAttendeeStatus));
  FValue:=aValue;
end;

{ TgdAttendeeType }

function TgdAttendeeType.AddToXML(Root: IXMLNode): IXMLNode;
begin
 if Root=nil then Exit;
  Result:=Root.AddChild(cgdTagNames[ord(egdAttendeeType)]);
  Result.Attributes['value']:=FValue;
end;

constructor TgdAttendeeType.Create(const ByNode: IXMLNode);
begin
  inherited Create;
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdAttendeeType.ParseXML(Node: IXMLNode);
begin
  if Node=nil then Exit;
  if GetGDNodeType(Node.NodeName) <> ord(egdAttendeeType) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdAttendeeType)]]));
  try
    FValue := Node.Attributes['value'];
    FAttType := TAttendeeType(AnsiIndexStr(FValue, cAttendeeType));
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

procedure TgdAttendeeType.SetType(aStatus: TAttendeeType);
begin
  FAttType:=aStatus;
  FValue:=cAttendeeType[ord(aStatus)]
end;

procedure TgdAttendeeType.SetValue(aValue: string);
begin
  if AnsiIndexStr(aValue, cAttendeeType)<=0 then
    raise Exception.Create(Format(rcErrMissValue, [cGDTagNames[ord(egdAttendeeType)]]));
  FAttType:=TAttendeeType(AnsiIndexStr(aValue, cAttendeeType));
  FValue:=aValue;
end;

{ TgdWho }

function TgdWho.AddToXML(Root: IXMLNode): IXMLNode;
begin
  if Root=nil then Exit;
  Result:=Root.AddChild(cgdTagNames[ord(egdWho)]);
  if Length(Trim(FEmail))>0 then
    Result.Attributes['email']:=FEmail;
  if Length(Trim(Frel))>0 then
    Result.Attributes['rel']:=Frel;
  if Length(Trim(FvalueString))>0 then
    Result.Attributes['valueString']:=FvalueString;
  if FAttendeeStatus<>nil then
    FAttendeeStatus.AddToXML(Result);
  if FAttendeeType<>nil then
    FAttendeeType.AddToXML(Result);
  if FEntryLink<>nil then
    FEntryLink.AddToXML(Result);
end;

constructor TgdWho.Create(const ByNode: IXMLNode);
begin
  inherited Create;
  FEmail:='';
  Frel:='';
  FvalueString:='';
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdWho.ParseXML(Node: IXMLNode);
var i:integer;
begin
  if Node=nil then Exit;
  if GetGDNodeType(Node.NodeName) <> ord(egdWho) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdWho)]]));
  try
    if Node.Attributes['email']<>null then
      FEmail:=Node.Attributes['email'];
    if Node.Attributes['rel']<>null then
      begin
        Frel:=Node.Attributes['rel'];
        FRelValue:=TWhoRel(AnsiIndexStr(Frel, cWhoRel));
      end;
    if Node.Attributes['valueString']<>null then
      FvalueString:=Node.Attributes['valueString'];
    if Node.ChildNodes.Count>0 then
      begin
        for I := 0 to Node.ChildNodes.Count-1 do
          case GetGDNodeType(Node.ChildNodes[i].NodeName) of
            ord(egdAttendeeStatus):
              FAttendeeStatus:=TgdAttendeeStatus.Create(Node.ChildNodes[i]);
            ord(egdAttendeeType):
              FAttendeeType:=TgdAttendeeType.Create(Node.ChildNodes[i]);
            ord(egdEntryLink):
              FEntryLink:=TgdEntryLink.Create(Node.ChildNodes[i]);
          end;
      end;
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

procedure TgdWho.SetRel(aRel: string);
begin
if AnsiIndexStr(aRel, cWhoRel)<=0 then
    raise Exception.Create(Format(rcErrMissValue, [cGDTagNames[ord(egdWho)]]));
  FRelValue:=TWhoRel(AnsiIndexStr(aRel, cWhoRel));
  Frel:=aRel;
end;

procedure TgdWho.SetRelValue(aRelValue: TWhoRel);
begin
  FRelValue:=aRelValue;
  Frel:=cWhoRel[ord(aRelValue)]
end;

{ TgdRecurrence }

function TgdRecurrence.AddToXML(Root: IXMLNode): IXMLNode;
begin
if Root=nil then Exit;
  Result:=Root.AddChild(cgdTagNames[ord(egdRecurrence)]);
  Result.Text:=FText.Text;
end;

constructor TgdRecurrence.Create(const ByNode: IXMLNode);
begin
  inherited Create;
  FText:=TStringList.Create;
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdRecurrence.ParseXML(Node: IXMLNode);
begin
if Node=nil then Exit;
  if GetGDNodeType(Node.NodeName) <> ord(egdRecurrence) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdRecurrence)]]));
  try
    FText.Text:=Node.Text;
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

{ TgdReminder }

function TgdReminder.AddToXML(Root: IXMLNode): IXMLNode;
begin
  if Root=nil then Exit;
  Result:=Root.AddChild(cgdTagNames[ord(egdReminder)]);
  Result.Attributes['method']:=cMethods[ord(Fmethod)];
  case FPeriod of
    tpDays: Result.Attributes['days']:=FPeriodValue;
    tpHours: Result.Attributes['hours']:=FPeriodValue;
    tpMinutes: Result.Attributes['minutes']:=FPeriodValue;
  end;
  if FabsoluteTime>0 then
    Result.Attributes['absoluteTime']:=DateTimeToServerDate(FabsoluteTime)
end;

constructor TgdReminder.Create(const ByNode: IXMLNode);
begin
  inherited Create;
  FabsoluteTime:=0;
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdReminder.ParseXML(Node: IXMLNode);
begin
if Node=nil then Exit;
  if GetGDNodeType(Node.NodeName) <> ord(egdReminder) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdReminder)]]));
  try
    if (Node.Attributes['absoluteTime']<>null)and(Length(Trim(Node.Attributes['absoluteTime']))>0) then
      FabsoluteTime:=ServerDateToDateTime(Node.Attributes['absoluteTime']);
    if Node.Attributes['method']<>null then
       Fmethod:=TMethod(AnsiIndexStr(Node.Attributes['method'], cMethods));
    if Node.Attributes['days']<>null then
       FPeriod:=tpDays;
    if Node.Attributes['hours']<>null then
       FPeriod:=tpHours;
    if Node.Attributes['minutes']<>null then
       FPeriod:=tpMinutes;
    case FPeriod of
      tpDays: FPeriodValue:=Node.Attributes['days'];
      tpHours: FPeriodValue:=Node.Attributes['hours'];
      tpMinutes: FPeriodValue:=Node.Attributes['minutes'];
    end;

  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

{ TgdTransparency }

function TgdTransparency.AddToXML(Root: IXMLNode): IXMLNode;
begin
if Root=nil then Exit;
Result:=Root.AddChild(cgdTagNames[ord(egdTransparency)]);
Result.Attributes['value']:=FValue;
end;

constructor TgdTransparency.Create(const ByNode: IXMLNode);
begin
  inherited Create;
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdTransparency.ParseXML(Node: IXMLNode);
begin
 if Node=nil then Exit;
  if GetGDNodeType(Node.NodeName) <> ord(egdTransparency) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdTransparency)]]));
  try
    FValue := Node.Attributes['value'];
    FTransparency := TTransparency(AnsiIndexStr(FValue, cTransparency));
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

procedure TgdTransparency.SetTransp(aTransp: TTransparency);
begin
  FTransparency:=aTransp;
  FValue:=cTransparency[ord(aTransp)]
end;

procedure TgdTransparency.SetValue(aValue: string);
begin
if AnsiIndexStr(aValue, cTransparency)<=0 then
    raise Exception.Create(Format(rcErrMissValue, [cGDTagNames[ord(egdTransparency)]]));
  FTransparency:=TTransparency(AnsiIndexStr(aValue, cTransparency));
  FValue:=aValue;
end;

{ TgdVisibility }

function TgdVisibility.AddToXML(Root: IXMLNode): IXMLNode;
begin
if Root=nil then Exit;
Result:=Root.AddChild(cgdTagNames[ord(egdVisibility)]);
Result.Attributes['value']:=FValue;
end;

constructor TgdVisibility.Create(const ByNode: IXMLNode);
begin
  inherited Create;
  if ByNode=nil then Exit;
  ParseXML(ByNode);
end;

procedure TgdVisibility.ParseXML(Node: IXMLNode);
begin
 if Node=nil then Exit;
  if GetGDNodeType(Node.NodeName) <> ord(egdVisibility) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdVisibility)]]));
  try
    FValue := Node.Attributes['value'];
    FVisible := TVisibility(AnsiIndexStr(FValue, cVisibility));
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;

procedure TgdVisibility.SetValue(aValue: string);
begin
if AnsiIndexStr(aValue, cVisibility)<=0 then
    raise Exception.Create(Format(rcErrMissValue, [cGDTagNames[ord(egdVisibility)]]));
  FVisible:=TVisibility(AnsiIndexStr(aValue, cVisibility));
  FValue:=aValue;
end;

procedure TgdVisibility.SetVisible(aVisible: TVisibility);
begin
  FVisible:=aVisible;
  FValue:=cVisibility[ord(aVisible)]
end;

{ TgdOrganization }

function TgdOrganization.AddToXML(Root: TXMLNode): TXmlNode;
begin
if Root=nil then Exit;
  Result:=Root.NodeNew(cGDTagNames[ord(egdOrganization)]);
if Trim(FRel)<>'' then
  Result.WriteAttributeString('rel',FRel);
if Trim(FLabel)<>'' then
  Result.WriteAttributeString('label',FLabel);
if FPrimary then
  Result.WriteAttributeBool('primary',Fprimary);
if Trim(ForgName.Value)<>'' then
  ForgName.AddToXML(Result);
if Trim(ForgTitle.Value)<>'' then
  ForgTitle.AddToXML(Result);
end;

constructor TgdOrganization.Create(ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);

end;

procedure TgdOrganization.ParseXML(const Node: TXmlNode);
var i:integer;
begin
if Node=nil then Exit;
  if GetGDNodeType(Node.Name) <> ord(egdOrganization) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdOrganization)]]));
  try
    Frel:=Node.ReadAttributeString('rel');
    if Node.HasAttribute('primary') then
      Fprimary:=Node.ReadAttributeBool('primary');
    if Node.HasAttribute('label') then
      FLabel:=Node.ReadAttributeString('label');
    for i:=0 to Node.NodeCount-1 do
      begin
        if LowerCase(Node.Nodes[i].Name)=LowerCase(cGDTagNames[ord(egdOrgName)]) then
          ForgName:=TgdOrgName.Create(Node.Nodes[i])
        else
          if LowerCase(Node.Nodes[i].Name)=LowerCase(cGDTagNames[ord(egdOrgTitle)]) then
            ForgTitle:=TgdOrgTitle.Create(Node.Nodes[i]);
      end;
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TgdEmailStruct }

function TgdEmail.AddToXML(Root: TXMLNode): TXmlNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(cGDTagNames[ord(egdEmail)]);
  if Trim(FRel)<>'' then
    Result.WriteAttributeString('rel',FRel);
  if Trim(FLabel)<>'' then
    Result.WriteAttributeString('label',FLabel);
  if Trim(FLabel)<>'' then
    Result.WriteAttributeString('displayName',FDisplayName);
  if FPrimary then
    Result.WriteAttributeBool('primary',FPrimary);
  Result.WriteAttributeString('address',FAddress);
end;

constructor TgdEmail.Create(ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TgdEmail.ParseXML(const Node: TXmlNode);
begin
  if Node=nil then Exit;
  if GetGDNodeType(Node.Name) <> ord(egdEmail) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdEmail)]]));
  try
    Frel:=Node.ReadAttributeString('rel');
    if Node.HasAttribute('primary') then
      Fprimary:=Node.ReadAttributeBool('primary');
    if Node.HasAttribute('label') then
      FLabel:=Node.ReadAttributeString('label');
    if Node.HasAttribute('displayName') then
      FDisplayName:=Node.ReadAttributeString('displayName');
    FAddress:=Node.ReadAttributeString('address');
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

procedure TgdEmail.SetEmailType(aType: TTypeElement);
begin
SetRel(RelValues[ord(aType)]);
FEmailType:=aType;
end;

procedure TgdEmail.SetRel(const aRel: string);
begin
  if AnsiIndexStr(aRel,RelValues)<0 then
   raise Exception.Create
      (Format(rcErrWriteNode, [cGDTagNames[ord(egdEmail)]])+' '+Format(rcWrongAttr,['rel']));
  Rel:=SchemaHref+aRel;
end;

{ TgdNameStruct }

function TgdName.AddToXML(Root: TXMLNode): TXmlNode;
begin
  if Root=nil then Exit;

  Result:=Root.NodeNew(cGDTagNames[ord(egdName)]);
  if AdditionalName<>nil then
    AdditionalName.AddToXML(Result);
  if GivenName<>nil then
    GivenName.AddToXML(Result);
  if AdditionalName<>nil then
    AdditionalName.AddToXML(Result);
  if FamilyName<>nil then
    FamilyName.AddToXML(Result);
  if NamePrefix<>nil then
    NamePrefix.AddToXML(Result);
  if NameSuffix<>nil then
    NameSuffix.AddToXML(Result);
  if FullName<>nil then
    FullName.AddToXML(Result);
end;

constructor TgdName.Create(ByNode: TXMLNode);
begin
  inherited Create;
  FGivenName:=TgdGivenName.Create();
  FAdditionalName:=TgdAdditionalNameStruct.Create();
  FFamilyName:=TgdFamilyName.Create();
  FNamePrefix:=TgdNamePrefix.Create();
  FNameSuffix:=TgdNameSuffix.Create();
  FFullName:=TgdFullName.Create();
  if ByNode<>nil then
    ParseXML(ByNode);
end;

function TgdName.GetFullName: string;
begin
  if FFullName<>nil then
    Result:=FFullName.Value;
end;

procedure TgdName.ParseXML(const Node: TXmlNode);
var i:integer;
begin
  if Node=nil then Exit;
  if GetGDNodeType(Node.Name) <> ord(egdName) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdName)]]));
  try
    for i:=0 to Node.NodeCount-1 do
      begin
        case GetGDNodeType(Node.Nodes[i].Name) of
          ord(egdGivenName):FGivenName.ParseXML(Node.Nodes[i]);
          ord(egdAdditionalName):FAdditionalName.ParseXML(Node.Nodes[i]);
          ord(egdFamilyName):FFamilyName.ParseXML(Node.Nodes[i]);
          ord(egdNamePrefix):FNamePrefix.ParseXML(Node.Nodes[i]);
          ord(egdNameSuffix):FNameSuffix.ParseXML(Node.Nodes[i]);
          ord(egdFullName):FFullName.ParseXML(Node.Nodes[i]);
        end;
      end;
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TgdPhoneNumber }

function TgdPhoneNumber.AddToXML(Root: TXMLNode): TXmlNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(cGDTagNames[ord(egdPhoneNumber)]);
  Result.WriteAttributeString('rel',Frel);
  Result.ValueAsString:=FValue;
  if Trim(FLabel)<>'' then
    Result.WriteAttributeString('label',FLabel);
  if Trim(FUri)<>'' then
    Result.WriteAttributeString('uri',FUri);
  if FPrimary then
    Result.WriteAttributeBool('primary',FPrimary);
end;

constructor TgdPhoneNumber.Create(ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TgdPhoneNumber.ParseXML(const Node: TXmlNode);
begin
  if Node=nil then Exit;
  if GetGDNodeType(Node.Name) <> ord(egdPhoneNumber) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdPhoneNumber)]]));
  try
    Frel:=Node.ReadAttributeString('rel');
    if Node.HasAttribute('primary') then
      Fprimary:=Node.ReadAttributeBool('primary');
    if Node.HasAttribute('label') then
      FLabel:=Node.ReadAttributeString('label');
    if Node.HasAttribute('uri') then
      FUri:=Node.ReadAttributeString('uri');
    FValue:=Node.ValueAsString;
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TgdCountry }

function TgdCountry.AddToXML(Root: TXMLNode): TXMLNode;
begin
 if Root=nil then Exit;
 Result:=Root.NodeNew(cGDTagNames[ord(egdCountry)]);
 if Trim(FCode)<>'' then
   Result.WriteAttributeString('code',FCode);
 Result.ValueAsString:=FValue;
end;

constructor TgdCountry.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TgdCountry.ParseXML(Node: TXMLNode);
begin
  if Node=nil then Exit;
  if GetGDNodeType(Node.Name) <> ord(egdCountry) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdCountry)]]));
  try
    FCode:=Node.ReadAttributeString('rel');
    FValue:=Node.ValueAsString;
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TgdStructuredPostalAddressStruct }

function TgdStructuredPostalAddress.AddToXML(Root: TXMLNode): TXMLNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(cGDTagNames[ord(egdStructuredPostalAddress)]);
  if Trim(FRel)<>'' then
     Result.WriteAttributeString('rel',FRel);
  if Trim(FMailClass)<>'' then
     Result.WriteAttributeString('mailClass',FMailClass);
  if Trim(Flabel)<>'' then
     Result.WriteAttributeString('label',Flabel);
  if Trim(FUsage)<>'' then
     Result.WriteAttributeString('Usage',FUsage);
  if Fprimary then
     Result.WriteAttributeBool('primary',Fprimary);
  if FAgent<>nil then
     FAgent.AddToXML(Result);
  if FHousename<>nil then
     FHousename.AddToXML(Result);
  if FStreet<>nil then
     FStreet.AddToXML(Result);
  if FPobox<>nil then
     FPobox.AddToXML(Result);
  if FNeighborhood<>nil then
     FNeighborhood.AddToXML(Result);
  if FCity<>nil then
     FCity.AddToXML(Result);
  if FSubregion<>nil then
     FSubregion.AddToXML(Result);
  if FRegion<>nil then
     FRegion.AddToXML(Result);
  if FPostcode<>nil then
     FPostcode.AddToXML(Result);
  if FCoutry<>nil then
     FCoutry.AddToXML(Result);
  if FFormattedAddress<>nil then
     FFormattedAddress.AddToXML(Result);
end;

constructor TgdStructuredPostalAddress.Create(const ByNode: TXMLNode);
begin
 inherited Create;
 if ByNode<>nil then
   ParseXML(ByNode);
end;

procedure TgdStructuredPostalAddress.ParseXML(Node: TXMLNode);
var i:integer;
begin
if Node=nil then Exit;
  if GetGDNodeType(Node.Name) <> ord(egdStructuredPostalAddress) then
    raise Exception.Create(Format(rcErrCompNodes,
        [cGDTagNames[ord(egdStructuredPostalAddress)]]));
  try
    FRel:=Node.ReadAttributeString('rel');
    FMailClass:=Node.ReadAttributeString('mailClass');
    Flabel:=Node.ReadAttributeString('label');
    if Node.HasAttribute('primaty') then
      Fprimary:=Node.ReadAttributeBool('primary');
    FUsage:=Node.ReadAttributeString('Usage');
  except
    raise Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
  for I := 0 to Node.NodeCount - 1 do
    begin
      case GetGDNodeType(Node.Nodes[i].Name) of
        ord(egdAgent):FAgent:=TgdAgent.Create(Node.Nodes[i]);
        ord(egdHousename):FHousename:=TgdHousename.Create(Node.Nodes[i]);
        ord(egdStreet):FStreet:=TgdStreet.Create(Node.Nodes[i]);
        ord(egdPobox):FPobox:=TgdPobox.Create(Node.Nodes[i]);
        ord(egdNeighborhood):FNeighborhood:=TgdNeighborhood.Create(Node.Nodes[i]);
        ord(egdCity):FCity:=TgdCity.Create(Node.Nodes[i]);
        ord(egdSubregion):FSubregion:=TgdSubregion.Create(Node.Nodes[i]);
        ord(egdRegion):FRegion:=TgdRegion.Create(Node.Nodes[i]);
        ord(egdPostcode):FPostcode:=TgdPostcode.Create(Node.Nodes[i]);
        ord(egdCountry):FCoutry:=TgdCountry.Create(Node.Nodes[i]);
        ord(egdFormattedAddress):FFormattedAddress:=TgdFormattedAddress.Create(Node.Nodes[i]);
      end;
    end;
end;

{ TgdIm }

function TgdIm.AddToXML(Root: TXMLNode): TXmlNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(cGDTagNames[ord(egdIm)]);
  Result.WriteAttributeString('rel',FRel);
  Result.WriteAttributeString('address',FAddress);
  Result.WriteAttributeString('label',FLabel);
  Result.WriteAttributeString('protocol',FProtocol);
  if FPrimary then
    Result.WriteAttributeBool('primary',FPrimary);
end;

constructor TgdIm.Create(ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TgdIm.ParseXML(const Node: TXmlNode);
begin
if Node=nil then Exit;
  if GetGDNodeType(Node.Name) <> ord(egdIm) then
    raise Exception.Create(Format(rcErrCompNodes,[cGDTagNames[ord(egdIm)]]));
  try
    FRel:=Node.ReadAttributeString('rel');
    FLabel:=Node.ReadAttributeString('label');
    FAddress:=Node.ReadAttributeString('address');
    FProtocol:=Node.ReadAttributeString('protocol');
    if Node.HasAttribute('primary') then
      FPrimary:=Node.ReadAttributeBool('primary');
  except
     raise Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

end.
