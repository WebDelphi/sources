// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://speller.yandex.net/services/spellservice?WSDL
//  >Import : http://speller.yandex.net/services/spellservice?WSDL>0
// Encoding : utf-8
// Version  : 1.0
// (02.03.2011 16:12:00 - - $Rev: 28374 $)
// ************************************************************************ //

unit spellservice;

interface

uses InvokeRegistry, SOAPHTTPClient, Types, XSBuiltIns;

const
  IS_OPTN = $0001;
  IS_UNBD = $0002;
  IS_ATTR = $0010;
  IS_REF  = $0080;


type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Embarcadero types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:int             - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:string          - "http://www.w3.org/2001/XMLSchema"[Gbl]

  CheckTextRequest     = class;                 { "http://speller.yandex.net/services/spellservice"[Lit][GblElm] }
  CheckTextResponse    = class;                 { "http://speller.yandex.net/services/spellservice"[Lit][GblElm] }
  CheckTextsResponse   = class;                 { "http://speller.yandex.net/services/spellservice"[Lit][GblElm] }
  CheckTextsRequest    = class;                 { "http://speller.yandex.net/services/spellservice"[Lit][GblElm] }
  SpellError           = class;                 { "http://speller.yandex.net/services/spellservice"[GblCplx] }
  error                = class;                 { "http://speller.yandex.net/services/spellservice"[Alias] }

  SpellResult = array of error;                 { "http://speller.yandex.net/services/spellservice"[GblCplx] }
  ArrayOfSpellResult = array of SpellResult;    { "http://speller.yandex.net/services/spellservice"[GblCplx] }


  // ************************************************************************ //
  // XML       : CheckTextRequest, global, <element>
  // Namespace : http://speller.yandex.net/services/spellservice
  // Serializtn: [xoLiteralParam]
  // Info      : Wrapper
  // ************************************************************************ //
  CheckTextRequest = class(TRemotable)
  private
    Flang: string;
    Flang_Specified: boolean;
    Foptions: Integer;
    Foptions_Specified: boolean;
    Fformat: string;
    Fformat_Specified: boolean;
    Ftext: string;
    procedure Setlang(Index: Integer; const Astring: string);
    function  lang_Specified(Index: Integer): boolean;
    procedure Setoptions(Index: Integer; const AInteger: Integer);
    function  options_Specified(Index: Integer): boolean;
    procedure Setformat(Index: Integer; const Astring: string);
    function  format_Specified(Index: Integer): boolean;
  public
    constructor Create; override;
  published
    property lang:    string   Index (IS_ATTR or IS_OPTN) read Flang write Setlang stored lang_Specified;
    property options: Integer  Index (IS_ATTR or IS_OPTN) read Foptions write Setoptions stored options_Specified;
    property format:  string   Index (IS_ATTR or IS_OPTN) read Fformat write Setformat stored format_Specified;
    property text:    string   read Ftext write Ftext;
  end;



  // ************************************************************************ //
  // XML       : CheckTextResponse, global, <element>
  // Namespace : http://speller.yandex.net/services/spellservice
  // Serializtn: [xoLiteralParam]
  // Info      : Wrapper
  // ************************************************************************ //
  CheckTextResponse = class(TRemotable)
  private
    FSpellResult: SpellResult;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property SpellResult: SpellResult  read FSpellResult write FSpellResult;
  end;



  // ************************************************************************ //
  // XML       : CheckTextsResponse, global, <element>
  // Namespace : http://speller.yandex.net/services/spellservice
  // Serializtn: [xoLiteralParam]
  // Info      : Wrapper
  // ************************************************************************ //
  CheckTextsResponse = class(TRemotable)
  private
    FArrayOfSpellResult: ArrayOfSpellResult;
  public
    constructor Create; override;
  published
    property ArrayOfSpellResult: ArrayOfSpellResult  read FArrayOfSpellResult write FArrayOfSpellResult;
  end;

  Array_Of_string = array of string;            { "http://www.w3.org/2001/XMLSchema"[GblUbnd] }


  // ************************************************************************ //
  // XML       : CheckTextsRequest, global, <element>
  // Namespace : http://speller.yandex.net/services/spellservice
  // Serializtn: [xoLiteralParam]
  // Info      : Wrapper
  // ************************************************************************ //
  CheckTextsRequest = class(TRemotable)
  private
    Flang: string;
    Flang_Specified: boolean;
    Foptions: Integer;
    Foptions_Specified: boolean;
    Fformat: string;
    Fformat_Specified: boolean;
    Ftext: Array_Of_string;
    Ftext_Specified: boolean;
    procedure Setlang(Index: Integer; const Astring: string);
    function  lang_Specified(Index: Integer): boolean;
    procedure Setoptions(Index: Integer; const AInteger: Integer);
    function  options_Specified(Index: Integer): boolean;
    procedure Setformat(Index: Integer; const Astring: string);
    function  format_Specified(Index: Integer): boolean;
    procedure Settext(Index: Integer; const AArray_Of_string: Array_Of_string);
    function  text_Specified(Index: Integer): boolean;
  public
    constructor Create; override;
  published
    property lang:    string           Index (IS_ATTR or IS_OPTN) read Flang write Setlang stored lang_Specified;
    property options: Integer          Index (IS_ATTR or IS_OPTN) read Foptions write Setoptions stored options_Specified;
    property format:  string           Index (IS_ATTR or IS_OPTN) read Fformat write Setformat stored format_Specified;
    property text:    Array_Of_string  Index (IS_OPTN or IS_UNBD) read Ftext write Settext stored text_Specified;
  end;



  // ************************************************************************ //
  // XML       : SpellError, global, <complexType>
  // Namespace : http://speller.yandex.net/services/spellservice
  // ************************************************************************ //
  SpellError = class(TRemotable)
  private
    Fcode: Integer;
    Fpos: Integer;
    Frow: Integer;
    Fcol: Integer;
    Flen: Integer;
    Fword_: string;
    Fs: Array_Of_string;
    Fs_Specified: boolean;
    procedure Sets(Index: Integer; const AArray_Of_string: Array_Of_string);
    function  s_Specified(Index: Integer): boolean;
  published
    property code:  Integer          Index (IS_ATTR) read Fcode write Fcode;
    property pos:   Integer          Index (IS_ATTR) read Fpos write Fpos;
    property row:   Integer          Index (IS_ATTR) read Frow write Frow;
    property col:   Integer          Index (IS_ATTR) read Fcol write Fcol;
    property len:   Integer          Index (IS_ATTR) read Flen write Flen;
    property word_: string           read Fword_ write Fword_;
    property s:     Array_Of_string  Index (IS_OPTN or IS_UNBD) read Fs write Sets stored s_Specified;
  end;



  // ************************************************************************ //
  // XML       : error, alias
  // Namespace : http://speller.yandex.net/services/spellservice
  // ************************************************************************ //
  error = class(SpellError)
  private
  published
  end;


  // ************************************************************************ //
  // Namespace : http://speller.yandex.net/services/spellservice
  // soapAction: http://speller.yandex.net/services/spellservice/%operationName%
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : document
  // binding   : SpellServiceSoap12
  // service   : SpellService
  // port      : SpellServiceSoap12
  // URL       : http://speller.yandex.net/services/spellservice
  // ************************************************************************ //
  SpellServiceSoap = interface(IInvokable)
  ['{FF325358-F5C9-C344-39A8-CBF8F209D3BD}']

    // Cannot unwrap: 
    //     - Input element wrapper name does not match operation's name
    function  checkText(const parameters: CheckTextRequest): CheckTextResponse; stdcall;

    // Cannot unwrap: 
    //     - Input element wrapper name does not match operation's name
    function  checkTexts(const parameters: CheckTextsRequest): CheckTextsResponse; stdcall;
  end;

function GetSpellServiceSoap(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): SpellServiceSoap;


implementation
  uses SysUtils;

function GetSpellServiceSoap(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): SpellServiceSoap;
const
  defWSDL = 'http://speller.yandex.net/services/spellservice?WSDL';
  defURL  = 'http://speller.yandex.net/services/spellservice';
  defSvc  = 'SpellService';
  defPrt  = 'SpellServiceSoap12';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as SpellServiceSoap);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


constructor CheckTextRequest.Create;
begin
  inherited Create;
  FSerializationOptions := [xoLiteralParam];
end;

procedure CheckTextRequest.Setlang(Index: Integer; const Astring: string);
begin
  Flang := Astring;
  Flang_Specified := True;
end;

function CheckTextRequest.lang_Specified(Index: Integer): boolean;
begin
  Result := Flang_Specified;
end;

procedure CheckTextRequest.Setoptions(Index: Integer; const AInteger: Integer);
begin
  Foptions := AInteger;
  Foptions_Specified := True;
end;

function CheckTextRequest.options_Specified(Index: Integer): boolean;
begin
  Result := Foptions_Specified;
end;

procedure CheckTextRequest.Setformat(Index: Integer; const Astring: string);
begin
  Fformat := Astring;
  Fformat_Specified := True;
end;

function CheckTextRequest.format_Specified(Index: Integer): boolean;
begin
  Result := Fformat_Specified;
end;

constructor CheckTextResponse.Create;
begin
  inherited Create;
  FSerializationOptions := [xoLiteralParam];
end;

destructor CheckTextResponse.Destroy;
var
  I: Integer;
begin
  for I := 0 to System.Length(FSpellResult)-1 do
    SysUtils.FreeAndNil(FSpellResult[I]);
  System.SetLength(FSpellResult, 0);
  inherited Destroy;
end;

constructor CheckTextsResponse.Create;
begin
  inherited Create;
  FSerializationOptions := [xoLiteralParam];
end;

constructor CheckTextsRequest.Create;
begin
  inherited Create;
  FSerializationOptions := [xoLiteralParam];
end;

procedure CheckTextsRequest.Setlang(Index: Integer; const Astring: string);
begin
  Flang := Astring;
  Flang_Specified := True;
end;

function CheckTextsRequest.lang_Specified(Index: Integer): boolean;
begin
  Result := Flang_Specified;
end;

procedure CheckTextsRequest.Setoptions(Index: Integer; const AInteger: Integer);
begin
  Foptions := AInteger;
  Foptions_Specified := True;
end;

function CheckTextsRequest.options_Specified(Index: Integer): boolean;
begin
  Result := Foptions_Specified;
end;

procedure CheckTextsRequest.Setformat(Index: Integer; const Astring: string);
begin
  Fformat := Astring;
  Fformat_Specified := True;
end;

function CheckTextsRequest.format_Specified(Index: Integer): boolean;
begin
  Result := Fformat_Specified;
end;

procedure CheckTextsRequest.Settext(Index: Integer; const AArray_Of_string: Array_Of_string);
begin
  Ftext := AArray_Of_string;
  Ftext_Specified := True;
end;

function CheckTextsRequest.text_Specified(Index: Integer): boolean;
begin
  Result := Ftext_Specified;
end;

procedure SpellError.Sets(Index: Integer; const AArray_Of_string: Array_Of_string);
begin
  Fs := AArray_Of_string;
  Fs_Specified := True;
end;

function SpellError.s_Specified(Index: Integer): boolean;
begin
  Result := Fs_Specified;
end;

initialization
  InvRegistry.RegisterInterface(TypeInfo(SpellServiceSoap), 'http://speller.yandex.net/services/spellservice', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(SpellServiceSoap), 'http://speller.yandex.net/services/spellservice/%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(SpellServiceSoap), ioDocument);
  InvRegistry.RegisterInvokeOptions(TypeInfo(SpellServiceSoap), ioLiteral);
  InvRegistry.RegisterInvokeOptions(TypeInfo(SpellServiceSoap), ioSOAP12);
  InvRegistry.RegisterMethodInfo(TypeInfo(SpellServiceSoap), 'checkText', '', 'CheckTextResponse');
  InvRegistry.RegisterParamInfo(TypeInfo(SpellServiceSoap), 'checkText', 'parameters1', 'parameters', '');
  InvRegistry.RegisterMethodInfo(TypeInfo(SpellServiceSoap), 'checkTexts', '', 'CheckTextsResponse');
  InvRegistry.RegisterParamInfo(TypeInfo(SpellServiceSoap), 'checkTexts', 'parameters1', 'parameters', '');
  RemClassRegistry.RegisterXSInfo(TypeInfo(SpellResult), 'http://speller.yandex.net/services/spellservice', 'SpellResult');
  RemClassRegistry.RegisterXSInfo(TypeInfo(ArrayOfSpellResult), 'http://speller.yandex.net/services/spellservice', 'ArrayOfSpellResult');
  RemClassRegistry.RegisterXSClass(CheckTextRequest, 'http://speller.yandex.net/services/spellservice', 'CheckTextRequest');
  RemClassRegistry.RegisterSerializeOptions(CheckTextRequest, [xoLiteralParam]);
  RemClassRegistry.RegisterXSClass(CheckTextResponse, 'http://speller.yandex.net/services/spellservice', 'CheckTextResponse');
  RemClassRegistry.RegisterSerializeOptions(CheckTextResponse, [xoLiteralParam]);
  RemClassRegistry.RegisterXSClass(CheckTextsResponse, 'http://speller.yandex.net/services/spellservice', 'CheckTextsResponse');
  RemClassRegistry.RegisterSerializeOptions(CheckTextsResponse, [xoLiteralParam]);
  RemClassRegistry.RegisterXSInfo(TypeInfo(Array_Of_string), 'http://www.w3.org/2001/XMLSchema', 'Array_Of_string');
  RemClassRegistry.RegisterXSClass(CheckTextsRequest, 'http://speller.yandex.net/services/spellservice', 'CheckTextsRequest');
  RemClassRegistry.RegisterSerializeOptions(CheckTextsRequest, [xoLiteralParam]);
  RemClassRegistry.RegisterXSClass(SpellError, 'http://speller.yandex.net/services/spellservice', 'SpellError');
  RemClassRegistry.RegisterExternalPropName(TypeInfo(SpellError), 'word_', 'word');
  RemClassRegistry.RegisterXSClass(error, 'http://speller.yandex.net/services/spellservice', 'error');

end.