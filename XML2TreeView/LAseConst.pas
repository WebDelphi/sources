unit LAseConst;

interface

uses Classes,SysUtils,Dialogs;

const BtnNames :array [1..35,1..2] of string =(
('LAInfo','� ���������'),
('AlwaysOnTop','������ ���� ����'),
('Close','�������'),
('Maximize','���������� �� ���� �����'),
('Minimize','��������������'),
('Play','���������������'),
('FrameForw','���� ������'),
('PlaybackSpeed','��������� �����'),
('Open','�������'),
('NextTrack','��������� ����'),
('PlayList','������ ���������������'),
('PreviousTrack','���������� ����'),
('Mute','��������� ����'),
('ShowEQ','�������� ����������'),
('SoundOpt','��������� �����'),
('VideoOpt','��������� �����'),
('ScreenShot','��������� ����'),
('SubtitlesOpt','��������� ���������'),
('ShowGraph','�����������'),
('FileInfo','���������� � �����'),
('Preferences','���������'),
('DefaultSize','�������������� ������'),
('PlaySelected','������������� ��������� ����'),
('AddFiles','�������� �����'),
('DeleteFiles','������� �����'),
('MoveUp','������� �����'),
('Shuffle','��������� ������� ���������������'),
('Bookmarks','��������'),
('Repeat','������'),
('AddFolder','�������� �����'),
('ClearList','�������� ������'),
('MoveDown','�������� ����'),
('Report','������� �� ������'),
('SaveList','��������� ������'),
('FullScreen','�� ���� �����'));

RootElements : array [1..7,1..2] of string = (('VideoMode','����� �����'),
('AudioMode','����� �����'),
('Skin','����'),
('Menu','����'),
('MainWindow','������� ����'),
('ExtPlayList','��������� ������ ���������������'),
('Equalizer','����������'));

OtrherElements: array [1..13,1..2]of string=(('Border','������ ����'),
('CaptionPanel','������ ���������'),
('CaptionText','����� ���������'),
('ControlPanel','������ ����������'),
('ChildPanel','�������� ������'),
('Track','�������'),
('TimeLine','����� ���������������'),
('Clock','����'),
('PlayTime','����� ���������������'),
('PlayList','������ ���������������'),
('Panel','������'),
('ControlsPanel','������ ����������'),
('PlayListPanel','������ ������ ���������������'));

function SysBtnNameToHuman(const SysName: string):string;

implementation

function SysBtnNameToHuman(const SysName: string):string;
var i:integer;
begin
Result:='';
  for I := 1 to High(BtnNames) do
    begin
      if LowerCase(BtnNames[i,1])=LowerCase(SysName) then
        begin
          Result:='������ "'+BtnNames[i,2]+'"';
          break;
        end;
    end;
if Length(Result)=0 then
  begin
   for I := 1 to High(RootElements) do
    begin
      if LowerCase(RootElements[i,1])=LowerCase(SysName) then
        begin
          Result:=RootElements[i,2];
          break;
        end;
    end;
  end;

if Length(Result)=0 then
  begin
   for I := 1 to High(OtrherElements) do
    begin
      if LowerCase(OtrherElements[i,1])=LowerCase(SysName) then
        begin
          Result:=OtrherElements[i,2];
          break;
        end;
    end;
  end;
if Result='' then
end;

end.
