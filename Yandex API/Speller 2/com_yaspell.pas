unit com_yaspell;

interface

uses SOAPHTTPClient, sysutils, classes, spellservice, TypInfo;

resourcestring
  rsErrLongText = 'Размер передаваемого текста максимум 10000 символов. Сейчас %d символов';

const
  // Пропускать слова, написанные заглавными буквами, например, "ВПК".
  cIGNORE_UPPERCASE = 1;
  // Пропускать слова с цифрами, например, "авп17х4534".
  cIGNORE_DIGITS = 2;
  // Пропускать интернет-адреса, почтовые адреса и имена файлов.
  cIGNORE_URLS = 4;
  // Подсвечивать повторы слов, идущие подряд. Например, "я полетел на на Кипр".
  cFIND_REPEAT_WORDS = 8;
  // Пропускать слова, написанные латиницей, например, "madrid".
  cIGNORE_LATIN = 16;
  // Только проверять текст, не выдавая вариантов для замены.
  cNO_SUGGEST = 32;
  // Отмечать слова, написанные латиницей, как ошибочные.
  cFLAG_LATIN = 128;
  // Не использовать словарное окружение (контекст) при проверке. Опция полезна в случаях, когда на вход сервиса передается список отдельных слов.
  cBY_WORDS = 256;
  // Игнорировать неверное употребление ПРОПИСНЫХ/строчных букв, например, в слове "москва".
  cIGNORE_CAPITALIZATION = 512;


  ERROR_UNKNOWN_WORD = 1; // Слова нет в словаре.
  ERROR_REPEAT_WORD = 2; // Повтор слова.
  ERROR_CAPITALIZATION = 3; // Неверное употребление прописных и строчных букв.
  ERROR_TOO_MANY_ERRORS = 4;
  // Текст содержит слишком много ошибок. При этом приложение может отправить Яндекс.Спеллеру оставшийся непроверенным текст в следующем запросе.

  cWSDLLocation = 'http://speller.yandex.net/services/spellservice?WSDL';
  cPort = 'SpellServiceSoap12';
  cService = 'SpellService';

  cMaxTextLen = 10000;

type
  TCheckTextResponse = CheckTextResponse;
  TSpellError = SpellError;
  TCheckTextsResponse = CheckTextsResponse;

type
  TFormat = (plain, html);

type
  TSpellOptions = class(TPersistent)
  private
    FIGNORE_UPPERCASE: boolean;
    FIGNORE_DIGITS: boolean;
    FIGNORE_URLS: boolean;
    FFIND_REPEAT_WORDS: boolean;
    FIGNORE_LATIN: boolean;
    FNO_SUGGEST: boolean;
    FFLAG_LATIN: boolean;
    FBY_WORDS: boolean;
    FIGNORE_CAPITALIZATION: boolean;
    procedure SetBY_WORDS(const Value: boolean);
    procedure SetFIND_REPEAT_WORDS(const Value: boolean);
    procedure SetFLAG_LATIN(const Value: boolean);
    procedure SetIGNORE_CAPITALIZATION(const Value: boolean);
    procedure SetIGNORE_DIGITS(const Value: boolean);
    procedure SetIGNORE_LATIN(const Value: boolean);
    procedure SetIGNORE_UPPERCASE(const Value: boolean);
    procedure SetIGNORE_URLS(const Value: boolean);
    procedure SetNO_SUGGEST(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property IGNORE_UPPERCASE: boolean read FIGNORE_UPPERCASE
      write SetIGNORE_UPPERCASE default false;
    property IGNORE_DIGITS: boolean read FIGNORE_DIGITS write SetIGNORE_DIGITS
      default false;
    property IGNORE_URLS: boolean read FIGNORE_URLS write SetIGNORE_URLS
      default false;
    property FIND_REPEAT_WORDS: boolean read FFIND_REPEAT_WORDS
      write SetFIND_REPEAT_WORDS default false;
    property IGNORE_LATIN: boolean read FIGNORE_LATIN write SetIGNORE_LATIN
      default false;
    property NO_SUGGEST: boolean read FNO_SUGGEST write SetNO_SUGGEST
      default false;
    property FLAG_LATIN: boolean read FFLAG_LATIN write SetFLAG_LATIN
      default false;
    property BY_WORDS: boolean read FBY_WORDS write SetBY_WORDS default false;
    property IGNORE_CAPITALIZATION: boolean read FIGNORE_CAPITALIZATION
      write SetIGNORE_CAPITALIZATION default false;
  end;

type
  TLangOptions = class(TPersistent)
  private
    FRussian: boolean;
    FUkrainian: boolean;
    FEnglish: boolean;
    procedure SetEnglish(const Value: boolean);
    procedure SetRussian(const Value: boolean);
    procedure SetUkrainian(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Russian: boolean read FRussian write SetRussian default True;
    property Ukrainian: boolean read FUkrainian write SetUkrainian
      default false;
    property English: boolean read FEnglish write SetEnglish default True;
  end;

type
  TYandexSpeller = class(THTTPRIO)
  private
    FSpellOptions: TSpellOptions;
    FLangOptions: TLangOptions;
    FFormat: TFormat;
    function GetLangs: string;
    function GetOptions: integer;
    function GetFormat: string;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    function CheckText(Text: string): TCheckTextResponse;
    function CheckTexts(Texts: TStrings): TCheckTextsResponse;
  published
    property SpellOptions: TSpellOptions read FSpellOptions write FSpellOptions;
    property LangOptions: TLangOptions read FLangOptions write FLangOptions;
    property Format: TFormat read FFormat write FFormat;
  end;

procedure Register;

implementation

{ TYandexSpeller }

procedure Register;
begin
  RegisterComponents('BuBa Group', [TYandexSpeller]);
end;

function TYandexSpeller.CheckText(Text: string): TCheckTextResponse;
var
  Params: CheckTextRequest;
begin
  if Length(Text)>cMaxTextLen then
    begin
      Exception.CreateFmt(rsErrLongText,[Length(Text)]);
      Exit;
    end;
  Params := CheckTextRequest.Create;
  try
    Params.lang := GetLangs;
    Params.options := GetOptions;
    Params.Format := GetFormat;
    Params.Text := Text;
    Result := (self as SpellServiceSoap).CheckText(Params);
  finally
    Params.Free;
  end;
end;

function TYandexSpeller.CheckTexts(Texts: TStrings): TCheckTextsResponse;
var Params: CheckTextsRequest;
    i:integer;
    TextArr: Array_Of_string;
    lengthTxt: integer;
begin
  if Texts=nil then Exit;
  lengthTxt:=0;
   for i:=0 to Texts.Count-1 do
    begin
      SetLength(TextArr,length(TextArr)+1);
      TextArr[length(TextArr)-1]:=Texts[i];
      lengthTxt:=lengthTxt+Length(TextArr[length(TextArr)-1]);
    end;
  if lengthTxt>cMaxTextLen then
    begin
      Finalize(TextArr);
      Exception.CreateFmt(rsErrLongText,[lengthTxt]);
      Exit;
    end;
  Params:=CheckTextsRequest.Create;
  try
  Params.lang:=GetLangs;
  Params.options:=GetOptions;
  Params.format:=GetFormat;
  Params.text:=TextArr;
  Result:=(self as SpellServiceSoap).checkTexts(Params)
  finally
    Params.Free;
    Finalize(TextArr);
  end;
end;

constructor TYandexSpeller.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FSpellOptions := TSpellOptions.Create;
  FLangOptions := TLangOptions.Create;
  FLangOptions.Russian := True;
  FLangOptions.English := True;
  WSDLLocation := cWSDLLocation;
  Port := cPort;
  Service := cService;
end;

destructor TYandexSpeller.Destroy;
begin
  FSpellOptions.Destroy;
  inherited Destroy;
end;

function TYandexSpeller.GetFormat: string;
begin
  Result := GetEnumName(TypeInfo(TFormat), ord(FFormat))
end;

function TYandexSpeller.GetLangs: string;
const
  DefString = 'ru,en,uk';
var
  Strs: TStringList;
begin
  Strs := TStringList.Create;
  try
    Strs.Delimiter := ',';
    Strs.DelimitedText := DefString;
    if not LangOptions.Russian then
      Strs.Delete(0);
    if not LangOptions.English then
      Strs.Delete(1);
    if not LangOptions.Ukrainian then
      Strs.Delete(2);
    Result := Strs.DelimitedText;
  finally
    Strs.Free;
  end;
end;

function TYandexSpeller.GetOptions: integer;
begin
  Result := 0;
  if SpellOptions.IGNORE_UPPERCASE then
    Result := Result + cIGNORE_UPPERCASE;
  if SpellOptions.IGNORE_DIGITS then
    Result := Result + cIGNORE_DIGITS;
  if SpellOptions.IGNORE_URLS then
    Result := Result + cIGNORE_URLS;
  if SpellOptions.FIND_REPEAT_WORDS then
    Result := Result + cFIND_REPEAT_WORDS;
  if SpellOptions.IGNORE_LATIN then
    Result := Result + cIGNORE_LATIN;
  if SpellOptions.NO_SUGGEST then
    Result := Result + cNO_SUGGEST;
  if SpellOptions.FLAG_LATIN then
    Result := Result + cFLAG_LATIN;
  if SpellOptions.BY_WORDS then
    Result := Result + cBY_WORDS;
  if SpellOptions.IGNORE_CAPITALIZATION then
    Result := Result + cIGNORE_CAPITALIZATION;
end;

{ TSpellOptions }

constructor TSpellOptions.Create;
begin
  inherited Create;
end;

destructor TSpellOptions.Destroy;
begin

  inherited Destroy;
end;

procedure TSpellOptions.SetBY_WORDS(const Value: boolean);
begin
  FBY_WORDS := Value;
end;

procedure TSpellOptions.SetFIND_REPEAT_WORDS(const Value: boolean);
begin
  FFIND_REPEAT_WORDS := Value;
end;

procedure TSpellOptions.SetFLAG_LATIN(const Value: boolean);
begin
  FFLAG_LATIN := Value;
end;

procedure TSpellOptions.SetIGNORE_CAPITALIZATION(const Value: boolean);
begin
  FIGNORE_CAPITALIZATION := Value;
end;

procedure TSpellOptions.SetIGNORE_DIGITS(const Value: boolean);
begin
  FIGNORE_DIGITS := Value;
end;

procedure TSpellOptions.SetIGNORE_LATIN(const Value: boolean);
begin
  FIGNORE_LATIN := Value;
end;

procedure TSpellOptions.SetIGNORE_UPPERCASE(const Value: boolean);
begin
  FIGNORE_UPPERCASE := Value;
end;

procedure TSpellOptions.SetIGNORE_URLS(const Value: boolean);
begin
  FIGNORE_URLS := Value;
end;

procedure TSpellOptions.SetNO_SUGGEST(const Value: boolean);
begin
  FNO_SUGGEST := Value;
end;

{ TLangOptions }

constructor TLangOptions.Create;
begin
  inherited Create;
end;

destructor TLangOptions.Destroy;
begin

  inherited Destroy;
end;

procedure TLangOptions.SetEnglish(const Value: boolean);
begin
  FEnglish := Value;
end;

procedure TLangOptions.SetRussian(const Value: boolean);
begin
  FRussian := Value;
end;

procedure TLangOptions.SetUkrainian(const Value: boolean);
begin
  FUkrainian := Value;
end;

end.
