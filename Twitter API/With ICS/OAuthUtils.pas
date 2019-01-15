unit OAuthUtils;

interface

uses SysUtils,Classes,Windows,
     OverbyteIcsMimeUtils,
     OverbyteIcsSha1,
     OverbyteIcsMD5;

type
  TSpecials = set of AnsiChar;

const
  URLSpecialChar: TSpecials =
  [#$00..#$21,':',';','/',',','?', '<', '>', '"', '%', '{', '}', '|', '\', '^', '~', '[', ']',
    '`', #$7F..#$FF];
  UnixStartDate : TDateTime = 25569;

//работа с URL'ом
function HTTPEncode(const AStr: AnsiString): AnsiString;
function UrlDecode(const Url : String) : String;
function UrlEncode(const S : String) : String;
//шифрование данных
function EncodeTriplet(const Value: AnsiString; Delimiter: AnsiChar;
  Specials: TSpecials): AnsiString;
function Base64Encode_(const Input: TBytes): string;
function EncryptHMACSha1(Input, AKey: string): TBytes;
function MD5(const Value:string):string;
//вспомогательные функции
function DateTimeToUnix(ConvDate: TDateTime): Longint;
function XDigit(Ch : Char) : Integer;
function IsXDigit(Ch : Char) : Boolean;
function htoin(Value : PChar; Len : Integer) : Integer;
function htoi2(Value : PChar) : Integer;


implementation

function MD5(const Value:string):string;
begin
  Result:=StrMD5(Value);
end;

function HTTPEncode(const AStr: AnsiString): AnsiString;
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-',
                  '0'..'9','$','!','''','(',')'];
var
  Sp, Rp: PAnsiChar;
begin
  SetLength(Result, Length(AStr) * 3);
  Sp := PAnsiChar(AStr);
  Rp := PAnsiChar(Result);
  while Sp^ <> #0 do
  begin
    if Sp^ in NoConversion then
      Rp^ := Sp^
    else
      if Sp^ = ' ' then
        Rp^ := '+'
      else
      begin
        FormatBuf(Rp^, 3, AnsiString('%%%.2x'), 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PAnsiChar(Result));
end;

function EncodeTriplet(const Value: AnsiString; Delimiter: AnsiChar;
  Specials: TSpecials): AnsiString;
var
  n, l: Integer;
  s: AnsiString;
  c: AnsiChar;
begin
  SetLength(Result, Length(Value) * 3);
  l := 1;
  for n := 1 to Length(Value) do
  begin
    c := Value[n];
    if c in Specials then
    begin
      Result[l] := Delimiter;
      Inc(l);
      s := IntToHex(Ord(c), 2);
      Result[l] := s[1];
      Inc(l);
      Result[l] := s[2];
      Inc(l);
    end
    else
    begin
      Result[l] := c;
      Inc(l);
    end;
  end;
  Dec(l);
  SetLength(Result, l);
end;

function Base64Encode_(const Input: TBytes): string;
begin
  Result:=Base64Encode(StringOf(Input));
end;

function DateTimeToUnix(ConvDate: TDateTime): Longint;
var
  x: double;
  lTimeZone: TTimeZoneInformation;
begin
  GetTimeZoneInformation(lTimeZone);
  ConvDate := ConvDate + (lTimeZone.Bias / 1440);
  x := (ConvDate - UnixStartDate) * 86400;
  Result := Trunc(x);
end;

function XDigit(Ch : Char) : Integer;
begin
    if (Ch >= '0') and (Ch <= '9') then
        Result := Ord(Ch) - Ord('0')
    else
        Result := (Ord(Ch) and 15) + 9;
end;


function IsXDigit(Ch : Char) : Boolean;
begin
    Result := ((Ch >= '0') and (Ch <= '9')) or
              ((Ch >= 'a') and (Ch <= 'f')) or
              ((Ch >= 'A') and (Ch <= 'F'));
end;

function htoin(Value : PChar; Len : Integer) : Integer;
var
    I : Integer;
begin
    Result := 0;
    I      := 0;
    while (I < Len) and (Value[I] = ' ') do
        I := I + 1;
    while (I < len) and (IsXDigit(Value[I])) do begin
        Result := Result * 16 + XDigit(Value[I]);
        I := I + 1;
    end;
end;

function EncryptHMACSha1(Input, AKey: string): TBytes;
begin
  Result:=BytesOf(HMAC_SHA1_EX(Input,AKey));
end;


function htoi2(Value : PChar) : Integer;
begin
    Result := htoin(Value, 2);
end;

function UrlEncode(const S : String) : String;
var
    I : Integer;
    Ch : Char;
begin
    Result := '';
    for I := 1 to Length(S) do begin
        Ch := S[I];
        if ((Ch >= '0') and (Ch <= '9')) or
           ((Ch >= 'a') and (Ch <= 'z')) or
           ((Ch >= 'A') and (Ch <= 'Z')) or
           (Ch = '.') or (Ch = '-') or (Ch = '_') or (Ch = '~')then
            Result := Result + Ch
        else
            Result := Result + '%' + IntToHex(Ord(Ch), 2);
    end;
end;

function UrlDecode(const Url : String) : String;
var
    I, J, K, L : Integer;
begin
    Result := Url;
    L      := Length(Result);
    I      := 1;
    K      := 1;
    while TRUE do begin
        J := I;
        while (J <= Length(Result)) and (Result[J] <> '%') do begin
            if J <> K then
                Result[K] := Result[J];
            Inc(J);
            Inc(K);
        end;
        if J > Length(Result) then
            break;                   { End of string }
        if J > (Length(Result) - 2) then begin
            while J <= Length(Result) do begin
                Result[K] := Result[J];
                Inc(J);
                Inc(K);
            end;
            break;
        end;
        Result[K] := Char(htoi2(@Result[J + 1]));
        Inc(K);
        I := J + 3;
        Dec(L, 2);
    end;
    SetLength(Result, L);
end;

end.
