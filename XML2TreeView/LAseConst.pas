unit LAseConst;

interface

uses Classes,SysUtils,Dialogs;

const BtnNames :array [1..35,1..2] of string =(
('LAInfo','О программе'),
('AlwaysOnTop','Поверх всех окон'),
('Close','Закрыть'),
('Maximize','Развернуть во весь экран'),
('Minimize','Минимизаровать'),
('Play','Воспроизведение'),
('FrameForw','Кадр вперед'),
('PlaybackSpeed','Промотать назад'),
('Open','Открыть'),
('NextTrack','Следующий трек'),
('PlayList','Список воспроизведения'),
('PreviousTrack','Предыдущий трек'),
('Mute','Отключить звук'),
('ShowEQ','Показать эквалайзер'),
('SoundOpt','Настройки звука'),
('VideoOpt','Настройки видео'),
('ScreenShot','Сохранить кадр'),
('SubtitlesOpt','Настройки субтитров'),
('ShowGraph','Зависимости'),
('FileInfo','Информация о файле'),
('Preferences','Настройки'),
('DefaultSize','Первоначальный размер'),
('PlaySelected','Воспроизвести выбранный файл'),
('AddFiles','Добавить файлы'),
('DeleteFiles','Удалить файлы'),
('MoveUp','Поднять вверх'),
('Shuffle','Случайный порядок воспроизведения'),
('Bookmarks','Закладки'),
('Repeat','Повтор'),
('AddFolder','Добавить папку'),
('ClearList','Очистить список'),
('MoveDown','Опустить вниз'),
('Report','Таблица по списку'),
('SaveList','Сохранить список'),
('FullScreen','Во весь экран'));

RootElements : array [1..7,1..2] of string = (('VideoMode','Режим видео'),
('AudioMode','Режим аудио'),
('Skin','Скин'),
('Menu','Меню'),
('MainWindow','Главное окно'),
('ExtPlayList','Раскрытый список воспроизведения'),
('Equalizer','Эквалайзер'));

OtrherElements: array [1..13,1..2]of string=(('Border','Бордюр окна'),
('CaptionPanel','Панель заголовка'),
('CaptionText','Текст заголовка'),
('ControlPanel','Панель управления'),
('ChildPanel','Дочерняя панель'),
('Track','Бегунок'),
('TimeLine','Линия воспроизведения'),
('Clock','Часы'),
('PlayTime','Время воспроизведения'),
('PlayList','Спасик воспроизведения'),
('Panel','Панель'),
('ControlsPanel','Панель управления'),
('PlayListPanel','Панель списка воспроизведения'));

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
          Result:='Кнопка "'+BtnNames[i,2]+'"';
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
