unit GContacts;

interface

uses NativeXML, strUtils, httpsend, GHelper,Classes,SysUtils,
GDataCommon,Generics.Collections,Dialogs,jpeg;

const
  {$REGION 'Константы'}
   CpProtocolVer = '3.0';
   CpNodeAlias = 'gContact:';
   CpGroupLink='http://www.google.com/m8/feeds/groups/%s/full';
   CPContactsLink='http://www.google.com/m8/feeds/contacts/default/full';
   cpContactTagNames: array [0..23]of string = ('billingInformation',
   'birthday','calendarLink','directoryServer', 'event','externalId',
   'gender','groupMembershipInfo','hobby','initials','jot','language',
   'maidenName','mileage','nickname','occupation','priority',
   'relation','sensitivity','shortName','subject','userDefinedField',
   'website','systemGroup');
  {$ENDREGION}        

type
  TcpTagEnum = (cpBillingInformation,cpBirthday,cpCalendarLink,cpDirectoryServer,
  cpEvent,cpExternalId,cpGender,cpGroupMembershipInfo,cpHobby,
  cpInitials,cpJot,cpLanguage,cpMaidenName,cpMileage,cpNickname,
  cpOccupation,cpPriority,cpRelation,cpSensitivity,cpShortName,
  cpSubject,cpUserDefinedField,cpWebsite,cpSystemGroup,cpNone);


type
  TcpBillingInformation = TTextTag;

type
  TcpBirthday =class(TPersistent)
  private
    FYear  : word;
    FMonth : word;
    FDay   : word;
    FShortFormat: boolean;//укороченый формат дня рождения --MM-DD
  public
    constructor Create(const byNode: TXmlNode=nil);
    //ToDate - перевод в формат TDate; Если задан укороченый формат, то
    //годом будет текущий
    function ToDate:TDate;
    procedure ParseXML(const Node: TXmlNode);
    function AddToXML(Root: TXMLNode):TXMLNode;
    property ShotrFormat: boolean read FShortFormat;
    property Year: word read FYear write FYear;
    property Month: word read FMonth write FMonth;
    property Day: word read FDay write FDay;
end;

type
  TcpCalendarLink = class(TPersistent)
  private
    FDescr: string;//содержит либо значение атрибута rel либо label
    FPrimary: boolean;
    FHref: string;
    const RelValues: array [0..2] of string = ('work','home','free-busy');
  public
    constructor Create(const byNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function  AddToXML(Root:TXmlNode):TXmlNode;
    property Description: string read FDescr write FDescr;
    property Primary: boolean read FPrimary write FPrimary;
    property Href: string read FHref write FHref;
end;

type
  TcpDirectoryServer = TTextTag;

type
  TcpEvent = class(TPersistent)
  private
    Frel: string;
    Flabel: string;
    FWhen: TgdWhen;
    const RelValues:array[0..1]of string=('anniversary','other');
    procedure SetRel(const aRel: string);
  public
    constructor Create(byNode: TXmlNode=nil);
    procedure ParseXML(const Node: TXMLNode);
    function AddToXML(Root: TXmlNode):TXmlNode;
    property Rel: string read Frel write SetRel;
    property Labl:string read Flabel write Flabel;
 end;

type
  TcpExternalId = class(TPersistent)
  private
    Frel: string;
    FLabel: string;
    FValue: string;
    const RelValues: array [0..3]of string = ('account','customer','network','organization');
    procedure SetRel(const aRel:string);
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function AddToXML(Root: TXmlNode):TXmlNode;
    property Rel: string read Frel write SetRel;
    property Labl: string read FLabel write FLabel;
    property Value: string read FValue write FValue;
end;

type
  TcpGender = class(TPersistent)
  private
    FValue: string;
    const Values:array[0..1]of string=('male','female');
    procedure SetValue(aValue:string);
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function AddToXML(Root: TXmlNode):TXmlNode;
    property Value: string read FValue write SetValue;
end;

type
  TcpGroupMembershipInfo = class(TPersistent)
  private
    FDeleted: boolean;
    FHref: string;
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function AddToXML(Root: TXmlNode):TXmlNode;
    property Href: string read FHref write FHref;
    property Deleted:boolean read FDeleted write FDeleted;
end;

type
  TcpHobby = TTextTag;
  TcpInitials = TTextTag;

type
  TcpJot = class(TPersistent)
  private
    FRel: string;
    FText: string;
    const RelValues:array[0..4]of string=('home', 'work', 'other', 'keywords', 'user');
    procedure SetRel(aRel: string);
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function AddToXML(Root: TXmlNode):TXmlNode;
    property Rel: string read FRel write SetRel;
    property Text:string read FText write FText;
end;

type
  TcpLanguage = class(TPersistent)
  private
    Fcode: string;
    Flabel: string;
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function AddToXML(Root: TXmlNode):TXmlNode;
    property Code: string read Fcode write Fcode;
    property Labl:string read Flabel write Flabel;
end;

type
  TcpMaidenName = TTextTag;
  TcpMileage = TTextTag;
  TcpNickname = TTextTag;
  TcpOccupation = TTextTag;

type
  TcpPriority = class(TPersistent)
  private
    FRel: string;
    const RelValues:array [0..2]of string=('low','normal','high');
    procedure SetRel(const aRel:string);
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function AddToXML(Root: TXmlNode):TXmlNode;
    property Rel: string read FRel write SetRel;
end;

type
  TcpRelation = class(TPersistent)
  private
    FDescr: string;//атрибут rel или label
    FValue: string;
    const
      RelValues: array [0..13]of string =('assistant','brother',
      'child','domestic-partner','father','friend','manager',
      'mother','mother','mother','referred-by','relative','sister','spouse');
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function AddToXML(Root: TXmlNode):TXmlNode;
    property Description: string read FDescr write FDescr;
    property Value: string read FValue write FValue;
end;

type
  TcpSensitivity = class(TPersistent)
  private
    FRel: string;
    const RelValues: array[0..3]of string = ('confidential','normal','personal','private');
    procedure SetRel(aRel:string);
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function AddToXML(Root: TXmlNode):TXmlNode;
    property Rel: string read FRel write SetRel;
end;

type
  TcpShortName = TTextTag;
  TcpSubject = TTextTag;

type
  TcpSystemGroup = class(TPersistent)
  private
    Fid: string;
    const IDValues: array [0..3]of string=('Contacts','Friends','Family','Coworkers');
    procedure SetId(aId:string);
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function  AddToXML(Root: TXmlNode):TXmlNode;
    property  ID: string read Fid write SetId;
end;

type
  TcpUserDefinedField = class(TPersistent)
  private
    FKey: string;
    FValue: string;
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function  AddToXML(Root: TXmlNode):TXmlNode;
    property  Key: string read FKey write FKey;
    property  Value: string read FValue write FValue;
end;

type
  TcpWebsite = class(TPersistent)
  private
    FHref: string;
    FPrimary:boolean;
    Flabel: string;
    FRel: string;
    const RelValues: array[0..6]of string=('home-page','blog','profile',
    'home','work','other','ftp');
    procedure SetRel(aRel:string);
  public
    constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(const Node: TXmlNode);
    function  AddToXML(Root: TXmlNode):TXmlNode;
    property  Href: string read FHref write FHref;
    property  Primary: boolean read FPrimary write FPrimary;
    property  Labl: string read Flabel write Flabel;
    property  Rel: string read FRel write SetRel;
end;

type
  TGoogleContact = class;
  TContactGroup = class;

  TContact = class
  private
    FEtag: string;
    FId: string;
    FUpdated: TDateTime;
    FTitle: TTextTag;
    FContent:TTextTag;
    FLinks:TList<TEntryLink>;
    FName: TgdName;
    FNickName: TcpNickname;
    FBirthDay: TcpBirthday;
    FOrganization: TgdOrganization;
    FEmails: TList<TgdEmail>;
    FPhones: TList<TgdPhoneNumber>;
    FPostalAddreses: TList<TgdStructuredPostalAddress>;
    FEvents : TList<TcpEvent>;
    FRelations: TList<TcpRelation>;
    FUserFields: TList<TcpUserDefinedField>;
    FWebSites: TList<TcpWebsite>;
    FGroupMemberships: TList<TcpGroupMembershipInfo>;
    FIMs:TList<TgdIm>;
    FOwner: TGoogleContact;
    function GetPrimaryEmail: string;
    procedure SetPrimaryEmail(aEmail:string);
    function GetName : TgdName;
    function GetOrganization:TgdOrganization;
  public
    constructor Create(byNode: TXMLNode; aOwner: TGoogleContact=nil);
    procedure ParseXML(Node: TXMLNode);
    function RetriveImage:TJPEGImage;
    function FindEmail(const aEmail:string; out Index:integer):TgdEmail;
    property Owner: TGoogleContact read FOwner write FOwner;
    property Etag: string read FEtag write FEtag;
    property Id: string read FId write FId;
    property Updated: TDateTime read FUpdated write FUpdated;
    property Title: TTextTag read FTitle write FTitle;
    property Content:TTextTag read FContent write FContent;
    property Links:TList<TEntryLink> read FLinks write FLinks;
    property Name: TgdName read GetName write FName;
    property NickName: TcpNickname read FNickName write FNickName;
    property BirthDay: TcpBirthday read FBirthDay write FBirthDay;
    property Organization: TgdOrganization read GetOrganization write FOrganization;
    property Emails: TList<TgdEmail> read FEmails write FEmails;
    property Phones: TList<TgdPhoneNumber> read FPhones write FPhones;
    property PostalAddreses: TList<TgdStructuredPostalAddress> read FPostalAddreses write FPostalAddreses;
    property Events : TList<TcpEvent> read FEvents write FEvents;
    property Relations: TList<TcpRelation> read FRelations write FRelations;
    property UserFields: TList<TcpUserDefinedField> read FUserFields write FUserFields;
    property WebSites: TList<TcpWebsite> read FWebSites write FWebSites;
    property GroupMemberships: TList<TcpGroupMembershipInfo> read FGroupMemberships write FGroupMemberships;
    property IMs:TList<TgdIm> read FIMs write FIMs;
    property PrimaryEmail:string read GetPrimaryEmail write SetPrimaryEmail;
end;

//type
  TContactGroup = class
  private
    FEtag: string;
    Fid: string;
    FLinks: TList<TEntryLink>;
    FUpdate: TDateTime;
    FTitle: TTextTag;
    FContent: TTextTag;
    FSystemGroup: TcpSystemGroup;
  public
    constructor Create(const ByNode: TXMLNode);
    procedure ParseXML(Node: TXmlNode);
    property Etag: string read FEtag write FEtag;
    property id: string read Fid write Fid;
    property Links: TList<TEntryLink> read FLinks write FLinks;
    property Update: TDateTime read FUpdate write FUpdate;
    property Title: TTextTag read FTitle write FTitle;
    property Content: TTextTag read FContent write FContent;
    property SystemGroup: TcpSystemGroup read FSystemGroup write FSystemGroup;
end;

//type
  TGoogleContact = class
  private
    FAuth: string; //AUTH для доступа к API
    FEmail:string; //обязательно GMAIL!
    FGroups: TList<TContactGroup>;
    FContacts: TList<TContact>;
    function GetNextLink(aXMLDoc:TNativeXml):string;
    function GetContactsByGroup(GroupName:string):TList<TContact>;
    function GroupLink(const aGroupName:string):string;
  public
    constructor Create(const aAuth,aEmail: string);
    function RetriveGroups:integer;
    function RetriveContacts: integer;
    property Groups: TList<TContactGroup> read FGroups write FGroups;
    property Contacts:TList<TContact> read FContacts write FContacts;
    property ContactsByGroup[GroupName:string]:TList<TContact> read GetContactsByGroup;
 end;


function GetContactNodeType(const NodeName: string):TcpTagEnum;

implementation

function GetContactNodeType(const NodeName: string):TcpTagEnum;
var
  i: integer;
  withoutAlias: string;
begin
  if pos(CpNodeAlias,NodeName)>0 then
    begin
      withoutAlias:=Trim(ReplaceStr(NodeName,CpNodeAlias,''));
      i:= AnsiIndexStr(withoutAlias, cpContactTagNames);
      if i>-1 then
        Result := TcpTagEnum(i)
      else
        Result:=cpNone;
    end
  else
    Result:=cpNone;
end;

{ TcpBirthday }

function TcpBirthday.AddToXML(Root: TXMLNode):TXmlNode;
var When: string;
begin
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpBirthday)]);
  if FYear=-1 then //укороченный формат даты
    When:='--'+IntToStr(FMonth)+'-'+IntToStr(FDay)
  else
    When:=IntToStr(FYear)+'-'+IntToStr(FMonth)+'-'+IntToStr(FDay);
  Result.AttributeAdd('when',When);
end;

constructor TcpBirthday.Create(const byNode: TXmlNode);
begin
  inherited Create;
  if byNode<>nil then
    ParseXML(byNode);
end;

procedure TcpBirthday.ParseXML(const Node: TXmlNode);
var DateStr: string;
begin
  if GetContactNodeType(Node.Name) <> cpBirthday then
      raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpBirthday)]]));
  try
    DateStr:= Node.ReadAttributeString('when');
    if (Length(Trim(DateStr))>0)then
      begin
        if (pos('--',DateStr)>0) then//сокращенный формат - только месяц и число рождения
          begin
            FYear:=0;
            FMonth:=StrToInt(copy(DateStr,3,2));
            FDay:=StrToInt(copy(DateStr,5,2));
            FShortFormat:=true;
          end
        else
          begin
            FYear:=StrToInt(copy(DateStr,1,4));;
            FMonth:=StrToInt(copy(DateStr,6,2));
            FDay:=StrToInt(copy(DateStr,8,2));
            FShortFormat:=false
          end;
      end;
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

function TcpBirthday.ToDate: TDate;
var aYear, aMonth, aDay: word;
begin
  if FShortFormat then //укороченный формат
    begin
      DecodeDate(Now,aYear,aMonth,aDay);
      Result:=EncodeDate(aYear,FMonth,FDay);
    end
  else
    Result:=EncodeDate(FYear,FMonth,FDay)
end;

{ TcpCalendarLink }

function TcpCalendarLink.AddToXML(Root: TXmlNode): TXmlNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpCalendarLink)]);
  if AnsiIndexStr(FDescr,RelValues)>=0 then
    Result.AttributeAdd('rel',FDescr)
  else
    Result.AttributeAdd('label',FDescr);
  Result.AttributeAdd('href',FHref);
  if FPrimary then
    Result.WriteAttributeBool('primary',FPrimary);
end;

constructor TcpCalendarLink.Create(const byNode: TXMLNode);
begin
  inherited Create;
  if byNode<>nil then
    ParseXML(byNode);
end;

procedure TcpCalendarLink.ParseXML(const Node: TXmlNode);
begin
  if GetContactNodeType(Node.Name) <> cpCalendarLink then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpCalendarLink)]]));
  try
    FPrimary:=false;
    //rel и label - взаимоисключающие атрибуты, но один обязан быть
    if Length(Trim(Node.AttributeByName['rel']))>0 then
      FDescr:=Trim(Node.AttributeByName['rel'])
    else
      FDescr:=Trim(Node.AttributeByName['label']);
    if Node.HasAttribute('primary') then
      FPrimary:=Node.ReadAttributeBool('primary');
    FHref:=Node.ReadAttributeString('href');
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TcpEvent }

function TcpEvent.AddToXML(Root: TXmlNode): TXmlNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpEvent)]);
  if AnsiIndexStr(Frel,RelValues)<0 then
   begin
    Result.Delete;
    raise Exception.Create
      (Format(rcErrWriteNode, [cpContactTagNames[ord(cpEvent)]])+' '+Format(rcWrongAttr,['rel']));
   end;
  Result.WriteAttributeString('rel',Frel);
  if length(Flabel)>0 then
    Result.WriteAttributeString('label',Flabel);
  FWhen.AddToXML(Result);
end;

constructor TcpEvent.Create(byNode: TXmlNode);
begin
  inherited Create;
  if byNode<>nil then
    ParseXML(byNode);
end;

procedure TcpEvent.ParseXML(const Node: TXMLNode);
var WhenNode: TXmlNode;
begin
  if GetContactNodeType(Node.Name) <> cpEvent then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpEvent)]]));
  try
    if Node.HasAttribute('label') then
      Flabel:=Trim(Node.ReadAttributeString('label'));
    if Node.HasAttribute('rel') then
      Frel:=Trim(Node.ReadAttributeString('rel'));
    WhenNode:=Node.FindNode(cGDTagNames[ord(egdWhen)]);
    if WhenNode<>nil then
       FWhen:=TgdWhen.Create(WhenNode)
    else
      Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

procedure TcpEvent.SetRel(const aRel: string);
begin
if AnsiIndexStr(Frel,RelValues)<0 then
  raise Exception.Create
      (Format(rcErrWriteNode, [cpContactTagNames[ord(cpEvent)]])+' '+Format(rcWrongAttr,['rel']));
Frel:=aRel;
end;

{ TcpExternalId }

function TcpExternalId.AddToXML(Root: TXmlNode): TXmlNode;
begin
  if Root=nil then Exit;
  if AnsiIndexStr(Frel,RelValues)<0 then
    raise Exception.Create
      (Format(rcErrWriteNode, [cpContactTagNames[ord(cpEvent)]])+' '+Format(rcWrongAttr,['rel']));
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpExternalId)]);
  if Trim(Flabel)<>'' then
    Result.WriteAttributeString('label',FLabel);
  Result.WriteAttributeString('rel',Frel);
  Result.WriteAttributeString('value',FValue);
end;

constructor TcpExternalId.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpExternalId.ParseXML(const Node: TXmlNode);
begin
  if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpExternalId then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpExternalId)]]));
  try
    if Node.HasAttribute('label') then
      Frel:=Node.ReadAttributeString('label');
    Frel:=Node.ReadAttributeString('rel');
    FValue:=Node.ReadAttributeString('value');
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

procedure TcpExternalId.SetRel(const aRel: string);
begin
if AnsiIndexStr(Frel,RelValues)<0 then
  raise Exception.Create
      (Format(rcErrWriteNode, [cpContactTagNames[ord(cpEvent)]])+' '+Format(rcWrongAttr,['rel']));
Frel:=aRel;
end;

{ TcpGender }

function TcpGender.AddToXML(Root: TXmlNode): TXmlNode;
begin
if Root=nil then Exit;
  if AnsiIndexStr(FValue,Values)<0 then
    raise Exception.Create
      (Format(rcErrWriteNode, [cpContactTagNames[ord(cpGender)]])+' '+Format(rcWrongAttr,['value']));
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpGender)]);
  Result.WriteAttributeString('value',FValue);
end;

constructor TcpGender.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpGender.ParseXML(const Node: TXmlNode);
begin
  if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpGender then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpGender)]]));
  try
    FValue:=Node.ReadAttributeString('value');
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

procedure TcpGender.SetValue(aValue: string);
begin
  if AnsiIndexStr(FValue,Values)<0 then
    raise Exception.Create
      (Format(rcErrWriteNode, [cpContactTagNames[ord(cpGender)]])+' '+Format(rcWrongAttr,['value']));
  FValue:=aValue;
end;

{ TcpGroupMembershipInfo }

function TcpGroupMembershipInfo.AddToXML(Root: TXmlNode): TXmlNode;
begin
 if Root=nil then Exit;
 Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpGroupMembershipInfo)]);
 Result.WriteAttributeString('href',FHref);
 Result.WriteAttributeBool('deleted',FDeleted);
end;

constructor TcpGroupMembershipInfo.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpGroupMembershipInfo.ParseXML(const Node: TXmlNode);
begin
 if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpGroupMembershipInfo then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpGroupMembershipInfo)]]));
  try
    FHref:=Node.ReadAttributeString('href');
    FDeleted:=Node.ReadAttributeBool('deleted')
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TcpJot }

function TcpJot.AddToXML(Root: TXmlNode): TXmlNode;
begin
 if Root=nil then Exit;
 Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpJot)]);
 Result.WriteAttributeString('rel',FRel);
 Result.ValueAsString:=FText;
end;

constructor TcpJot.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpJot.ParseXML(const Node: TXmlNode);
begin
  if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpJot then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpJot)]]));
  try
    FRel:=Node.ReadAttributeString('rel');
    FText:=Node.ValueAsString;
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

procedure TcpJot.SetRel(aRel: string);
begin
if AnsiIndexStr(Frel,RelValues)<0 then
  raise Exception.Create
   (Format(rcErrWriteNode, [cpContactTagNames[ord(cpJot)]])+' '+Format(rcWrongAttr,['rel']));
Frel:=aRel;
end;

{ TcpLanguage }

function TcpLanguage.AddToXML(Root: TXmlNode): TXmlNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpLanguage)]);
  Result.WriteAttributeString('code',Fcode);
  Result.WriteAttributeString('label',Flabel);
end;

constructor TcpLanguage.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpLanguage.ParseXML(const Node: TXmlNode);
begin
 if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpLanguage then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpLanguage)]]));
  try
    Fcode:=Node.ReadAttributeString('code');
    Flabel:=Node.ReadAttributeString('label');
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TcpPriority }

function TcpPriority.AddToXML(Root: TXmlNode): TXmlNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpPriority)]);
  Result.WriteAttributeString('rel',FRel);
end;

constructor TcpPriority.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpPriority.ParseXML(const Node: TXmlNode);
begin
  if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpPriority then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpPriority)]]));
  try
    FRel:=Node.ReadAttributeString('rel');
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

procedure TcpPriority.SetRel(const aRel: string);
begin
if AnsiIndexStr(Frel,RelValues)<0 then
  raise Exception.Create
   (Format(rcErrWriteNode, [cpContactTagNames[ord(cpPriority)]])+' '+Format(rcWrongAttr,['rel']));
Frel:=aRel;
end;

{ TcpRelation }

function TcpRelation.AddToXML(Root: TXmlNode): TXmlNode;
begin
if Root=nil then Exit;
Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpRelation)]);
if AnsiIndexStr(FDescr,RelValues)<0 then
  Result.WriteAttributeString('label',FDescr)
else
  Result.WriteAttributeString('rel',FDescr);
Result.ValueAsString:=FValue;
end;

constructor TcpRelation.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpRelation.ParseXML(const Node: TXmlNode);
begin
 if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpRelation then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpRelation)]]));
  try
    if Node.HasAttribute('rel') then
      FDescr:=Node.ReadAttributeString('rel')
    else
      FDescr:=Node.ReadAttributeString('label');
    FValue:=Node.ValueAsString;
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TcpSensitivity }

function TcpSensitivity.AddToXML(Root: TXmlNode): TXmlNode;
begin
 if Root=nil then Exit;
  if AnsiIndexStr(Frel,RelValues)<0 then
    raise Exception.Create
      (Format(rcErrWriteNode, [cpContactTagNames[ord(cpSensitivity)]])+' '+Format(rcWrongAttr,['rel']));
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpSensitivity)]);
  Result.WriteAttributeString('rel',Frel);
end;

constructor TcpSensitivity.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpSensitivity.ParseXML(const Node: TXmlNode);
begin
if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpSensitivity then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpSensitivity)]]));
  try
      FRel:=Node.ReadAttributeString('rel')
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

procedure TcpSensitivity.SetRel(aRel: string);
begin
if AnsiIndexStr(Frel,RelValues)<0 then
  raise Exception.Create
   (Format(rcErrWriteNode, [cpContactTagNames[ord(cpPriority)]])+' '+Format(rcWrongAttr,['rel']));
Frel:=aRel;
end;

{ TsystemGroup }

function TcpsystemGroup.AddToXML(Root: TXmlNode): TXmlNode;
begin
 if Root=nil then Exit;
  if AnsiIndexStr(Fid,IDValues)<0 then
    raise Exception.Create
      (Format(rcErrWriteNode, [cpContactTagNames[ord(cpSystemGroup)]])+' '+Format(rcWrongAttr,['id']));
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpSystemGroup)]);
  Result.WriteAttributeString('id',Fid);
end;

constructor TcpsystemGroup.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpsystemGroup.ParseXML(const Node: TXmlNode);
begin
  if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpSystemGroup then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpSystemGroup)]]));
  try
      Fid:=Node.ReadAttributeString('id')
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

procedure TcpsystemGroup.SetId(aId: string);
begin
if AnsiIndexStr(Fid,IDValues)<0 then
  raise Exception.Create
   (Format(rcErrWriteNode, [cpContactTagNames[ord(cpSystemGroup)]])+' '+Format(rcWrongAttr,['id']));
Fid:=aId;
end;

{ TcpUserDefinedField }

function TcpUserDefinedField.AddToXML(Root: TXmlNode): TXmlNode;
begin
  if Root=nil then Exit;
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpUserDefinedField)]);
  Result.WriteAttributeString('key',FKey);
  Result.WriteAttributeString('value',FKey);
end;

constructor TcpUserDefinedField.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpUserDefinedField.ParseXML(const Node: TXmlNode);
begin
  if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpUserDefinedField then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpUserDefinedField)]]));
  try
    FKey:=Node.ReadAttributeString('key');
    FValue:=Node.ReadAttributeString('value');
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TcpWebsite }

function TcpWebsite.AddToXML(Root: TXmlNode): TXmlNode;
begin
  if Root=nil then Exit;
  if AnsiIndexStr(FRel,RelValues)<0 then
    raise Exception.Create
      (Format(rcErrWriteNode, [cpContactTagNames[ord(cpWebsite)]])+' '+Format(rcWrongAttr,['rel']));
  Result:=Root.NodeNew(CpNodeAlias+cpContactTagNames[ord(cpWebsite)]);
  Result.WriteAttributeString('href',FHref);
  Result.WriteAttributeString('rel',FRel);
  if FPrimary then
    Result.WriteAttributeBool('primary',FPrimary);
  if Trim(Flabel)<>'' then
    Result.WriteAttributeString('label',Flabel);
end;

constructor TcpWebsite.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TcpWebsite.ParseXML(const Node: TXmlNode);
begin
  if Node=nil then Exit;
  if GetContactNodeType(Node.Name) <> cpWebsite then
     raise Exception.Create
        (Format(rcErrCompNodes, [cpContactTagNames[ord(cpWebsite)]]));
  try
    FHref:=Node.ReadAttributeString('href');
    FRel:=Node.ReadAttributeString('rel');
    if Node.HasAttribute('label') then
      Flabel:=Node.ReadAttributeString('label');
    if Node.HasAttribute('primary') then
      FPrimary:=Node.ReadAttributeBool('primary');
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

procedure TcpWebsite.SetRel(aRel: string);
begin
if AnsiIndexStr(FRel,RelValues)<0 then
  raise Exception.Create
   (Format(rcErrWriteNode, [cpContactTagNames[ord(cpWebsite)]])+' '+Format(rcWrongAttr,['rel']));
FRel:=aRel;
end;

{ TContact }

constructor TContact.Create(byNode: TXMLNode; aOwner: TGoogleContact);
begin
  inherited Create;
  if aOwner<>nil  then
    FOwner:=aOwner;
  FLinks:=TList<TEntryLink>.Create;
  FEmails:= TList<TgdEmail>.Create;
  FPhones:= TList<TgdPhoneNumber>.Create;
  FPostalAddreses:= TList<TgdStructuredPostalAddress>.Create;
  FEvents := TList<TcpEvent>.Create;
  FRelations:= TList<TcpRelation>.Create;
  FUserFields:= TList<TcpUserDefinedField>.Create;
  FWebSites:= TList<TcpWebsite>.Create;
  FIms:=TList<TgdIm>.Create;
  FGroupMemberships:= TList<TcpGroupMembershipInfo>.Create;
  if byNode<>nil then
    ParseXML(byNode);
end;

function TContact.FindEmail(const aEmail:string; out Index:integer):TgdEmail;
var i:integer;
begin
  Result:=nil;
  for i:=0 to FEmails.Count - 1 do
    begin
      if UpperCase(aEmail)=UpperCase(FEmails[i].Address) then
        begin
          Result:=FEmails[i];
          Index:=i;
          break;
        end;
    end;
end;

function TContact.GetName: TgdName;
begin
Result:=TgdName.Create();
if FName<>nil then
  Result:=FName
end;

function TContact.GetOrganization: TgdOrganization;
begin
  Result:=TgdOrganization.Create();
  if FOrganization<>nil then
    Result:=FOrganization
  else
    begin
      Result.OrgName:=TTextTag.Create();
      Result.OrgTitle:=TTextTag.Create();
    end;
end;

function TContact.GetPrimaryEmail: string;
var i:integer;
begin
Result:='';
if FEmails=nil then Exit;
if FEmails.Count=0 then Exit;
for i:=0 to FEmails.Count - 1 do
  begin
    if FEmails[i].Primary then
      begin
        Result:=FEmails[i].Address;
        break;
      end;
  end;
end;

procedure TContact.ParseXML(Node: TXMLNode);
var i:integer;
    List: TXmlNodeList;
begin
 if Node=nil then Exit;
 FEtag:=Node.ReadAttributeString('gd:etag');
 List:=TXmlNodeList.Create;
 //вначале заполняем все списки
 Node.NodesByName(cGDTagNames[ord(egdEmail)],List);
 for i:=0 to List.Count-1 do
   begin
     FEmails.Add(TgdEmail.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 Node.NodesByName(cGDTagNames[ord(egdPhoneNumber)],List);
 for i:=0 to List.Count-1 do
   begin
     FPhones.Add(TgdPhoneNumber.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 Node.NodesByName(cGDTagNames[ord(egdIm)],List);
 for i:=0 to List.Count-1 do
   begin
     FIMs.Add(TgdIM.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 Node.NodesByName(cGDTagNames[ord(egdStructuredPostalAddress)],List);
 for i:=0 to List.Count-1 do
   begin
     FPostalAddreses.Add(TgdStructuredPostalAddress.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 Node.NodesByName(CpNodeAlias+cpContactTagNames[ord(cpEvent)],List);
 for i:=0 to List.Count-1 do
   begin
     FEvents.Add(TcpEvent.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 Node.NodesByName(CpNodeAlias+cpContactTagNames[ord(cpRelation)],List);
 for i:=0 to List.Count-1 do
   begin
     FRelations.Add(TcpRelation.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 Node.NodesByName(CpNodeAlias+cpContactTagNames[ord(cpUserDefinedField)],List);
 for i:=0 to List.Count-1 do
   begin
     FUserFields.Add(TcpUserDefinedField.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 Node.NodesByName(CpNodeAlias+cpContactTagNames[ord(cpWebsite)],List);
 for i:=0 to List.Count-1 do
   begin
     FWebSites.Add(TcpWebsite.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 Node.NodesByName(CpNodeAlias+cpContactTagNames[ord(cpGroupMembershipInfo)],List);
 for i:=0 to List.Count-1 do
   begin
     FGroupMemberships.Add(TcpGroupMembershipInfo.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 Node.NodesByName('link',List);
 for i:=0 to List.Count-1 do
   begin
     FLinks.Add(TEntryLink.Create(List.Items[i]));
     List.Items[i].Delete;
   end;
 for i:=0 to Node.NodeCount - 1 do
   begin
     if Node.Nodes[i].Name='updated' then
       FUpdated:=ServerDateToDateTime(Node.Nodes[i].ValueAsString)
     else
       if Node.Nodes[i].Name='title' then
         FTitle:=TTextTag.Create(Node.Nodes[i])
       else
         if Node.Nodes[i].Name='content' then
           FContent:=TTextTag.Create(Node.Nodes[i])
         else
           if Node.Nodes[i].Name=cGDTagNames[ord(egdName)] then
             FName:=TgdName.Create(Node.Nodes[i])
           else
             if Node.Nodes[i].Name=cGDTagNames[ord(egdOrganization)] then
               FOrganization:=TgdOrganization.Create(Node.Nodes[i])
             else
               if Node.Nodes[i].Name=CpNodeAlias+cpContactTagNames[ord(cpBirthday)] then
                 FBirthDay:=TcpBirthday.Create(Node.Nodes[i])
               else
                 if Node.Nodes[i].Name=CpNodeAlias+cpContactTagNames[ord(cpNickname)] then
                   FNickName:=TTextTag.Create(Node.Nodes[i]);
   end;
end;

function TContact.RetriveImage:TJPEGImage;
var i:integer;
begin
Result:=nil;
  for i:=0 to FLinks.Count - 1 do
    begin
      if FLinks[i].Rel='http://schemas.google.com/contacts/2008/rel#photo' then
        begin
          if Length(FLinks[i].Etag)>0 then
            begin
              Result:=TJPEGImage.Create;
              Result.LoadFromStream(SendRequest('GET',FLinks[i].Href,Owner.FAuth,CpProtocolVer,nil,nil));
              break;
            end;
        end;
    end;
end;

procedure TContact.SetPrimaryEmail(aEmail: string);
var index,i:integer;
    NewEmail: TgdEmail;
begin
  if FindEmail(aEmail,index)=nil then
    begin
      NewEmail:=TgdEmail.Create();
      NewEmail.Address:=aEmail;
      NewEmail.Primary:=true;
      NewEmail.EmailType:=ttOther;
      FEmails.Add(NewEmail);
    end;
 for i:=0 to FEmails.Count - 1 do
   FEmails[i].Primary:=(i=index);
end;

{ TContactGroup }

constructor TContactGroup.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  FLinks:=TList<TEntryLink>.Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TContactGroup.ParseXML(Node: TXmlNode);
var i:integer;
begin
  if Node=nil then Exit;
  FEtag:=Node.ReadAttributeString('gd:etag');
  for i:=0 to Node.NodeCount-1 do
    begin
      if Node.Nodes[i].Name='id' then
        Fid:=Node.Nodes[i].ValueAsString
      else
        if Node.Nodes[i].Name='updated' then
          FUpdate:=ServerDateToDateTime(Node.Nodes[i].ValueAsString)
        else
          if Node.Nodes[i].Name='title' then
            FTitle:=TTextTag.Create(Node.Nodes[i])
          else
            if Node.Nodes[i].Name='content' then
              FContent:=TTextTag.Create(Node.Nodes[i])
            else
              if Node.Nodes[i].Name=CpNodeAlias+cpContactTagNames[ord(cpSystemGroup)] then
                FSystemGroup:=TcpSystemGroup.Create(Node.Nodes[i])
              else
                if Node.Nodes[i].Name='link' then
                  FLinks.Add(TEntryLink.Create(Node.Nodes[i]));
    end;
end;

{ TGoogleContact }

constructor TGoogleContact.Create(const aAuth,aEmail: string);
begin
  if Trim(aAuth)='' then
    raise Exception.Create(rcErrNullAuth);
  inherited Create;
  FEmail:=aEmail;
  FAuth:=aAuth;
  FGroups:=TList<TContactGroup>.Create;
  FContacts:=TList<TContact>.Create;
end;

function TGoogleContact.GetContactsByGroup(GroupName: string): TList<TContact>;
var i,j:integer;
    GrupLink:string;
begin
  Result:=TList<TContact>.Create;
  GrupLink:=GroupLink(GroupName);
  if GrupLink<>'' then
    begin
      for i:=0 to FContacts.Count - 1 do
        for j:=0 to FContacts[i].FGroupMemberships.Count-1 do
          begin
            if FContacts[i].FGroupMemberships[j].FHref=GrupLink then
              Result.Add(FContacts[i])
          end;
    end;
end;

function TGoogleContact.GetNextLink(aXMLDoc: TNativeXml): string;
var i:integer;
    List: TXmlNodeList;
begin
 Result:='';
 List:=TXmlNodeList.Create;
 aXMLDoc.Root.NodesByName('link',List);
 for i:=0 to List.Count-1 do
   begin
     if List.Items[i].ReadAttributeString('rel')='next' then
       begin
         Result:=List.Items[i].ReadAttributeString('href');
         break;
       end;
   end;

end;

function TGoogleContact.GroupLink(const aGroupName: string): string;
var i:integer;
begin
  Result:='';
  for i:=0 to FGroups.Count - 1 do
    begin
      if UpperCase(aGroupName)=UpperCase(FGroups[i].FTitle.Value) then
        begin
          Result:=FGroups[i].Fid;
          break
        end;
    end;
end;

function TGoogleContact.RetriveContacts: integer;
var XMLDoc: TNativeXML;
    i:integer;
    NextLink: string;
begin
 NextLink:=CPContactsLink;
 XMLDoc:=TNativeXml.Create;
 repeat
   XMLDoc.LoadFromStream(SendRequest('GET',NextLink,FAuth,CpProtocolVer, nil, nil));
   for i:=0 to XMLDoc.Root.NodeCount - 1 do
     if XMLDoc.Root.Nodes[i].Name=EntryNodeName then
       FContacts.Add(TContact.Create(XMLDoc.Root.Nodes[i],Self));
    NextLink:=GetNextLink(XMLDoc);
  until NextLink='';
Result:=FContacts.Count;
FreeAndNil(XMLDoc);
end;

function TGoogleContact.RetriveGroups: integer;
var XMLDoc: TNativeXML;
    i:integer;
    NextLink: string;
begin
 NextLink:=Format(CpGroupLink,[FEmail]);
 XMLDoc:=TNativeXml.Create;
 repeat
   XMLDoc.LoadFromStream(SendRequest('GET',NextLink,FAuth,CpProtocolVer, nil, nil));
   for i:=0 to XMLDoc.Root.NodeCount - 1 do
     if XMLDoc.Root.Nodes[i].Name=EntryNodeName then
       FGroups.Add(TContactGroup.Create(XMLDoc.Root.Nodes[i]));
    NextLink:=GetNextLink(XMLDoc);
  until NextLink='';
Result:=FGroups.Count;
FreeAndNil(XMLDoc);
end;

end.
