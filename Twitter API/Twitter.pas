{
      Mozilla Public License.

      ``The contents of this file are subject to the Mozilla Public License
      Version 1.1 (the "License"); you may not use this file except in compliance
      with the License. You may obtain a copy of the License at

      http://www.mozilla.org/MPL/

      Software distributed under the License is distributed on an "AS IS"
      basis, WITHOUT WARRANTY OF

      ANY KIND, either express or implied. See the License for the specific language governing rights and

      limitations under the License.

      The Original Code is Twitter - Delphi implementation.

      The Initial Developer of the Original Code is CB2 Enterprises, Inc.
      Portions created by

      CB2 Enterprises, Inc. are Copyright (C) 2009.
      All Rights Reserved.

      Contributor(s): ______________________________________.

      Alternatively, the contents of this file may be used under the terms
      of the _____ license (the  [___] License), in which case the provisions
      of [______] License are applicable  instead of those above.
      If you wish to allow use of your version of this file only under the terms
      of the [____] License and not to allow others to use your version of this
      file under the MPL, indicate your decision by deleting  the provisions
      above and replace  them with the notice and other provisions required
      by the [___] License.  If you do not delete the provisions above,
      a recipient may use your version of this file under either the MPL or the
      [___] License."
}

unit Twitter;

interface

uses
  Classes, IDHttp, TypInfo, SysUtils, OAuth,Dialogs,httpsend, Generics.Collections;

type
  TTwitterDirectMessage = (ddirect_messages, dsent, dnew, ddestroy);

  TTwitterFriendship = (fcreate, fdestroy, fexists);

  TTwitterOutputFormatType = (json, xml, rss, atom);

  TTwitterObjectType = (statuses, account, users, friends, friendships,
                        direct_messages, followers, favorites, notifications,
                        blocks, help);

  TTwitterStatus = (spublic_timeline, sfriends_timeline, suser_timeline,
                    sshow, supdate, sreplies, sdestroy);

  TTwitterUser = (ufriends, ufollowers, ushow);

  TTwitter = class(TObject)
    private
      FHTTP: TIdCustomHTTP;
      FSource: string;
      FTwitterClient: string;
      FTwitterClientVersion: string;
      FTwitterClientURL: string;
      FUserName: string;
      FPassword: string;
      FConsumer: TOAuthConsumer;
      FToken: TOAuthToken;
      FRequest: TOAuthRequest;
      FHMAC: TOAuthSignatureMethod_HMAC_SHA1;
      FKey: string;
      FSecret: string;
      FOAuth_token: string;
      FOAuth_token_secret: string;
      procedure SetSource(const Value: string);
      procedure SetTwitterClient(const Value: string);
      procedure SetTwitterClientVersion(const Value: string);
      procedure SetTwitterCLientURL(const Value: string);
      function UrlEncode(const S : String) : String;
      function _IntToHex(Value: Integer; Digits: Integer): string;
      function DeleteFirstChar(input: string): string;
    protected
      function GetTwitterOutputFormatType(TwitterOutputFormatType: TTwitterOutputFormatType): string;
      function GetTwitterObjectType(TwitterObjectType: TTwitterObjectType): string;
      function GetTwitterStatus(TwitterStatus: TTwitterStatus): string;
      function GetTwitterDirectMessage(TwitterDirectMessage: TTwitterDirectMessage): string;
      function GetTwitterFriendship(TwitterFriendship: TTwitterFriendship): string;
      function GetTwitterUser(TwitterUser: TTwitterUser): string;

      function GETCommand(URL: string): string;
      function POSTCommand(URL: string; Params: TStringList): string;
    public
      constructor Create;
      function GetPublicTimeLine(OutputFormatType: TTwitterOutputFormatType): string;
      function GetUserTimeLine(ScreenName: string; OutputFormatType: TTwitterOutputFormatType): string;
      function GetFriendsTimeLine(OutputFormatType: TTwitterOutputFormatType; since_id: string = '';
                                  max_id: string = ''; count: string = '' ; page: string = ''): string;
      function GetFriends(OutputFormatType: TTwitterOutputFormatType): string;
      function GetUserFollowers(OutputFormatType: TTwitterOutputFormatType): string; overload;
      function GetUserFollowers(parm: string; OutputFormatType: TTwitterOutputFormatType): string; overload;

      function Update(Status: string; OutputFormatType: TTwitterOutputFormatType): string;
      function Show(ScreenName: string; OutputFormatType: TTwitterOutputFormatType): string;
      function GetReplies(OutputFormatType: TTwitterOutputFormatType): string;
      function GetDirectMessages(OutputFormatType: TTwitterOutputFormatType): string;
      function GetDirectMessagesSent(OutputFormatType: TTwitterOutputFormatType): string;
      function PostDirectMessagesNew(OutputFormatType: TTwitterOutputFormatType; user, text: string): string;
      function PostDirectMessagesDestroy(id: string; OutputFormatType: TTwitterOutputFormatType): string;
      property Source: string read FSource write SetSource;
      property TwitterClient: string read FTwitterClient write SetTwitterClient;
      property TwitterClientVersion: string read FTwitterClientVersion write SetTwitterClientVersion;
      property TwitterClientURL: string read FTwitterClientURL write SetTwitterClientURL;
      property Consumer: TOAuthConsumer read FConsumer write FConsumer;
      property Token: TOAuthToken read FToken write FToken;
      property Request: TOAuthRequest read FRequest write FRequest;
      property HMAC: TOAuthSignatureMethod_HMAC_SHA1 read FHMAC write FHMAC;
      property Key: string read FKey write FKey;
      property Secret: string read FSecret write FSecret;
      property OAuth_token: string read FOAuth_token write FOAuth_token;
      property OAuth_token_secret: string read FOAuth_token_secret write FOAuth_token_secret;
  end;

const
  TwitterBaseURLFormat = 'http://twitter.com/%s/%s.%s';
  TwitterBaseURLFormat2 = 'http://twitter.com/%s.%s';

implementation

{ TTwitter }

constructor TTwitter.Create;
begin
  FHTTP := TIdCustomHTTP.Create(nil);
  FHTTP.Request.UserAgent := 'Mozilla/3.0 (compatible; Indy Library)';
  FHTTP.Request.Accept := 'text/html, */*';
  FHTTP.HTTPOptions := [hoForceEncodeParams, hoInProcessAuth];
  FHTTP.MaxAuthRetries := 3;
  FHTTP.Request.ContentType := 'application/x-www-form-urlencoded';
end;

function TTwitter.DeleteFirstChar(input: string): string;
begin
  Delete(input, 1, 1);
  Result := input;
end;

function TTwitter.GETCommand(URL: string): string;
var
  pos: integer;
begin
  FConsumer := nil;
  FConsumer := TOAuthConsumer.Create(FKey, FSecret, 'http://www.webdelphi.ru');
  FRequest := TOAuthRequest.Create(URL);
  FRequest := Request.FromConsumerAndToken(FConsumer, nil, URL);
  FRequest.HTTPURL := URL;
  FToken := TOAuthToken.Create(FOAuth_token, FOAuth_token_secret);
  FRequest := Request.FromConsumerAndToken(FConsumer, FToken, URL);
  FRequest.Sign_Request(HMAC, Consumer, Token);
  pos := AnsiPos('?', URL);
//  ShowMessage(Request.Parameters.Text);
  if pos = 0 then
    URL := URL + '?' + Request.GetString
  else
    URL := URL + '&' + Request.GetString;
  Result := FHTTP.Get(URL);
end;

function TTwitter.GetUserFollowers(OutputFormatType: TTwitterOutputFormatType): string;
var
  URL: string;
begin
  if ((OutputFormatType <> JSON) and (OutputFormatType <> XML)) then
    raise Exception.Create('GetUserFollowers supports only XML and JSON output format.');
  URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(Statuses),
                GetTwitterUser(ufollowers),
                GetTwitterOutputFormatType(OutputFormatType)]);
  Result := GETCommand(URL);
end;

function TTwitter.GetUserFollowers(parm: string;
  OutputFormatType: TTwitterOutputFormatType): string;
var
  URL: string;
begin
  if ((OutputFormatType <> JSON) and (OutputFormatType <> XML)) then
    raise Exception.Create('GetUserFollowers supports only XML and JSON output format.');

  URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(Statuses),
                GetTwitterUser(ufollowers),
                GetTwitterOutputFormatType(OutputFormatType) + '?' + parm]);
  Result := GETCommand(URL);
end;

function TTwitter.GetDirectMessages(
  OutputFormatType: TTwitterOutputFormatType): string;
var
  URL: string;
begin
  URL := Format(TwitterBaseURLFormat2, [GetTwitterDirectMessage(ddirect_messages),
                GetTwitterOutputFormatType(OutputFormatType)]);
  Result := GETCommand(URL);
end;

function TTwitter.GetDirectMessagesSent(
  OutputFormatType: TTwitterOutputFormatType): string;
var
  URL: string;
begin
  URL := Format(TwitterBaseURLFormat, [GetTwitterDirectMessage(ddirect_messages),
                GetTwitterDirectMessage(dsent),
                GetTwitterOutputFormatType(OutputFormatType)]);
  Result := GETCommand(URL);
end;

function TTwitter.GetFriends(OutputFormatType: TTwitterOutputFormatType): string;
var
  URL: string;
begin
  if ((OutputFormatType <> JSON) and (OutputFormatType <> XML)) then
    raise Exception.Create('GetFriends supports only XML and JSON output format.');
  URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(Statuses),
                GetTwitterUser(ufriends),
                GetTwitterOutputFormatType(OutputFormatType)]);
  Result := GETCommand(URL);
end;

function TTwitter.GetFriendsTimeLine(OutputFormatType: TTwitterOutputFormatType;
                                     since_id: string = ''; max_id: string = '';
                                     count: string = '' ; page: string = ''): string;
var
  URL: string;
  parm_cnt: integer;
begin
  URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(Statuses),
                GetTwitterStatus(sfriends_timeline),
                GetTwitterOutputFormatType(OutputFormatType)]);
  parm_cnt := 0;
  if ((since_id <> '') or (max_id <> '') or (count <>  '')  or (page <> '')) then
  begin
    URL := URL + '?';
    Inc(parm_cnt);

    if (since_id <> '') then
    begin
      if (parm_cnt > 1) then
      begin
        URL := URL + '&';
        Inc(parm_cnt);
      end;
      URL := URL + 'since_id=' + since_id;
    end;

    if (count <> '') then
    begin
      if (parm_cnt > 1) then
      begin
        URL := URL + '&';
        Inc(parm_cnt);
      end;
      URL := URL + 'count=' + count;
    end;

    if (max_id <> '') then
    begin
      if (parm_cnt > 1) then
      begin
        URL := URL + '&';
        Inc(parm_cnt);
      end;
      URL := URL + 'max_id=' + max_id;
    end;

    if (page <> '') then
    begin
      if (parm_cnt > 1) then
      begin
        URL := URL + '&';
        Inc(parm_cnt);
      end;
      URL := URL + 'page=' + page;
    end;
 end;
  Result := GETCommand(URL);
end;

function TTwitter.GetPublicTimeLine(OutputFormatType: TTwitterOutputFormatType): string;
var
  URL: string;
begin
  URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(Statuses),
                GetTwitterStatus(spublic_timeline),
                GetTwitterOutputFormatType(OutputFormatType)]);
  Result := GETCommand(URL);
end;

function TTwitter.GetReplies(OutputFormatType: TTwitterOutputFormatType): string;
var
  URL: string;
begin
  URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(Statuses),
                GetTwitterStatus(sreplies),
                GetTwitterOutputFormatType(OutputFormatType)]);
  Result := GETCommand(URL);
end;

function TTwitter.GetTwitterDirectMessage(
  TwitterDirectMessage: TTwitterDirectMessage): string;
var
  x: string;
begin
  x := GetEnumName(TypeInfo(TTwitterDirectMessage),  Ord(TwitterDirectMessage));
  Result := DeleteFirstChar(x);
end;

function TTwitter.GetTwitterFriendship(
  TwitterFriendship: TTwitterFriendship): string;
var
  x: string;
begin
  x := GetEnumName(TypeInfo(TTwitterFriendship),  Ord(TwitterFriendship));
  Result := DeleteFirstChar(x);
end;

function TTwitter.GetTwitterObjectType(
  TwitterObjectType: TTwitterObjectType): string;
begin
  Result := GetEnumName(TypeInfo(TTwitterObjectType),  Ord(TwitterObjectType));
end;

function TTwitter.GetTwitterOutputFormatType(
  TwitterOutputFormatType: TTwitterOutputFormatType): string;
begin
  Result := GetEnumName(TypeInfo(TTwitterOutputFormatType),  Ord(TwitterOutputFormatType));
end;

function TTwitter.GetTwitterStatus(TwitterStatus: TTwitterStatus): string;
var
  x: string;
begin
  x:= GetEnumName(TypeInfo(TTwitterStatus),  Ord(TwitterStatus));
  Result := DeleteFirstChar(x);
end;

function TTwitter.GetTwitterUser(TwitterUser: TTwitterUser): string;
var
  x: string;
begin
  x:= GetEnumName(TypeInfo(TTwitterUser),  Ord(TwitterUser));
  Result := DeleteFirstChar(x);
end;

function TTwitter.GetUserTimeLine(ScreenName: string; OutputFormatType: TTwitterOutputFormatType): string;
var
  URL: string;
begin
  if (ScreenName = '') then
    URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(Statuses),
                  GetTwitterStatus(suser_timeline),
                  GetTwitterOutputFormatType(OutputFormatType)])
  else
    URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(Statuses),
                  GetTwitterStatus(suser_timeline) + '/' + ScreenName,
                  GetTwitterOutputFormatType(OutputFormatType)]);
  Result := GETCommand(URL);
end;

function TTwitter.POSTCommand(URL: string; Params: TStringList): string;
var
  ah,key : string;
  aa : TStringList;
begin
  FRequest := TOAuthRequest.Create(URL);
  FToken := TOAuthToken.Create(FOAuth_token, FOAuth_token_secret);
  FRequest := Request.FromConsumerAndToken(FConsumer, FToken, URL);
  FRequest.HTTPURL := URL;
  ah := FRequest.genAuthHeader(Consumer,Token,Params,URL);

  aa := TStringList.Create;
  aa.Clear;

  FHTTP.HTTPOptions := [];
  FHTTP.Request.CustomHeaders.Add('Authorization: '+ ah);
  aa.Add(FRequest.encodeParams(Params,'&',false,true));
  ShowMessage(Params.Text);
  Result := FHTTP.Post(URL, aa);
  aa.Free;
end;

function TTwitter.PostDirectMessagesDestroy(id: string;
  OutputFormatType: TTwitterOutputFormatType): string;
var
  Data: TStringList;
  URL: string;
begin
  if ((OutputFormatType <> JSON) and (OutputFormatType <> XML)) then
    raise Exception.Create('Direct_messages/destroy supports only XML and JSON output format.');

  Data := TStringlist.Create;
  URL := Format(TwitterBaseURLFormat, [GetTwitterDirectMessage(ddirect_messages),
                GetTwitterDirectMessage(ddestroy) + '/' + id,
                GetTwitterOutputFormatType(OutputFormatType)]);
  Data := TStringlist.Create;
  Result := POSTCommand(URL, Data);
  Data.Free;
end;

function TTwitter.PostDirectMessagesNew(
  OutputFormatType: TTwitterOutputFormatType; user, text: string): string;
var
  Data: TStringList;
  URL: string;
begin
  if ((OutputFormatType <> JSON) and (OutputFormatType <> XML)) then
    raise Exception.Create('Direct_messages/new supports only XML and JSON output format.');


   URL:='http://api.twitter.com/1/direct_messages/new.xml';
//  URL := Format(TwitterBaseURLFormat, [GetTwitterDirectMessage(ddirect_messages),
//                GetTwitterDirectMessage(dnew),
//                GetTwitterOutputFormatType(OutputFormatType)]);
  Data := TStringlist.Create;
//  Data.Add(user);
//  Data.Add(text);
//  ShowMessage(URL);
  Result := POSTCommand(URL, Data);
  Data.Free;
end;

procedure TTwitter.SetSource(const Value: string);
begin
  FSource := Value;
end;

procedure TTwitter.SetTwitterClient(const Value: string);
begin
  FTwitterCLient := Value;
end;

procedure TTwitter.SetTwitterCLientURL(const Value: string);
begin
  FTwitterCLientURL := Value;
end;

procedure TTwitter.SetTwitterClientVersion(const Value: string);
begin
  FTwitterCLientVersion := Value;
end;

function TTwitter.Show(ScreenName: string; OutputFormatType: TTwitterOutputFormatType): string;
var
  URL: string;
begin
  if ((OutputFormatType <> JSON) and (OutputFormatType <> XML)) then
    raise Exception.Create('Show supports only XML and JSON output format.');
    URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(users),
                  GetTwitterUser(ushow) + '/' + ScreenName,
                  GetTwitterOutputFormatType(OutputFormatType)]);
  Result := GETCommand(URL);
end;

function TTwitter.Update(Status: string; OutputFormatType: TTwitterOutputFormatType): string;
var
  Text, URL: string;
  Data: TStringList;
begin
  if ((OutputFormatType <> JSON) and (OutputFormatType <> XML)) then
    raise Exception.Create('Update supports only XML and JSON output format.');
  URL := Format(TwitterBaseURLFormat, [GetTwitterObjectType(Statuses),
                GetTwitterStatus(supdate),
                GetTwitterOutputFormatType(OutputFormatType)]);
  Data := TStringlist.Create;
  Text := Format('status=%s', [status]);
  Data.Add(Text);
  Result := POSTCommand(URL, Data);
  Data.Free;
end;

function TTwitter.UrlEncode(const S: String): String;
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
          Result := Result + '%' + _IntToHex(Ord(Ch), 2);
  end;
end;

function TTwitter._IntToHex(Value, Digits: Integer): String;
begin
  Result := SysUtils.IntToHex(Value, Digits);
end;

end.
