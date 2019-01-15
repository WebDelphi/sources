unit uSyntax;

interface

uses Windows, SysUtils, Classes, Graphics, ComCtrls, OleCtrls;

type
  TColorer = record
    FontSize: integer;
    CurrSize: integer;
    FontColor: TColor;
    CurrColor: TColor;
  end;

function CheckList(InString: string): boolean;
procedure RichEditKeyUp(REdit: TRichEdit; var Key: Word; Shift: TShiftState);
procedure HighLight(REdit: TRichEdit);
procedure RemoveHightLight(REdit: TRichEdit);
procedure LoadBuildStops(AFileName: string);

var
  Colorer: TColorer;
  BuildStops: TStringList;

implementation

procedure LoadBuildStops(AFileName: string);
begin
  BuildStops.LoadFromFile(AFileName);
end;

function SearchFor(WorkSpace, Search: string; Start: integer): integer;
var
  Temp: string;
begin
  Temp := copy(WorkSpace, Start, length(WorkSpace));
  Result := pos(Search, Temp);
end;

procedure RemoveHightLight(REdit: TRichEdit);
var
  WEnd: integer;
begin
  WEnd := REdit.SelStart;
  REdit.SelectAll;
  REdit.SelAttributes.Color := Colorer.CurrColor;
  REdit.SelAttributes.Size := Colorer.CurrSize;
  REdit.SelStart := WEnd;
end;

procedure HighLight(REdit: TRichEdit);
var
  WStart, WEnd, WEnd2: integer;
  WorkSpace, SWord: string;
begin
  RemoveHightLight(REdit);
  WStart := 1;
  WEnd := 1;
  with REdit do
  begin
    WorkSpace := Text + ' ' + #$D#$A;
    while WEnd > 0 do
    begin
      WEnd := SearchFor(WorkSpace, ' ', WStart);
      WEnd2 := SearchFor(WorkSpace, #$A, WStart);
      if WEnd2 < WEnd then
        WEnd := WEnd2;
      SWord := copy(WorkSpace, WStart, WEnd - 1);
      if (SWord <> ' ') and (SWord <> '') then
        if CheckList(SWord) then
        begin
          SelStart := WStart - 1;
          SelLength := length(SWord);
          REdit.SelAttributes.Size := Colorer.FontSize;
          REdit.SelAttributes.Color := Colorer.FontColor;
          SelStart := WStart + length(SWord) + 1;
          REdit.SelAttributes.Size := Colorer.CurrSize;
          REdit.SelAttributes.Color := Colorer.CurrColor;
        end;
      WStart := WStart + WEnd;
    end;
    SelStart := length(Text);
    SetFocus;
  end;
end;

procedure RichEditKeyUp(REdit: TRichEdit; var Key: Word; Shift: TShiftState);
var
  WEnd, WStart, BCount: integer;
  Mark: string;
begin
  if (Key = VK_Return) or (Key = VK_Back) or (Key = VK_Space) then
  begin
    if REdit.SelStart > 1 then
    begin
      WStart := 0;
      WEnd := REdit.SelStart;
      BCount := WEnd - 1;
      while BCount <> 0 do
      begin
        Mark := copy(REdit.Text, BCount, 1);
        if (Mark = ' ') or (Mark = #$A) then
        begin
          WStart := BCount;
          BCount := 1;
        end;
        dec(BCount);
      end;
      REdit.SelStart := WEnd - (WEnd - WStart);
      REdit.SelLength := WEnd - WStart;
      if CheckList(REdit.SelText) then
      begin
        REdit.SelAttributes.Size := Colorer.FontSize;
        REdit.SelAttributes.Color := Colorer.FontColor;
      end
      else
      begin
        REdit.SelAttributes.Size := Colorer.CurrSize;
        REdit.SelAttributes.Color := Colorer.CurrColor;
      end;
      REdit.SelStart := WEnd;
      REdit.SelAttributes.Size := Colorer.CurrSize;
      REdit.SelAttributes.Color := Colorer.CurrColor;
    end;
  end;
end;

function CheckList(InString: string): boolean;
var
  X: integer;
begin
  Result := false;
  X := 0;
  InString := StringReplace(InString, ' ', '', [rfReplaceAll]);
  InString := StringReplace(InString, #$A, '', [rfReplaceAll]);
  InString := StringReplace(InString, #$D, '', [rfReplaceAll]);
  while X < BuildStops.Count do
    if AnsiLowerCase(BuildStops.Strings[X]) = AnsiLowerCase(InString) then
    begin
      Result := true;
      X := BuildStops.Count;
    end
    else
      inc(X);
end;

initialization
  BuildStops:=TStringList.Create;

finalization
  FreeAndNil(BuildStops)

end.
