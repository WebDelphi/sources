unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DwmApi, StdCtrls;

type
  TForm4 = class(TForm)
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses Unit5;

{$R *.dfm}

procedure TForm4.Button3Click(Sender: TObject);
var PH: PHTHUMBNAIL;
    Props: DWM_THUMBNAIL_PROPERTIES;
    ProgmanHandle: THandle;
begin
  ProgmanHandle:=0;
  ProgmanHandle:=FindWindow('Progman',nil);
  if ProgmanHandle>0 then
    begin
      if Succeeded(DwmRegisterThumbnail(Handle,ProgmanHandle,PH))then
         begin
           Props.dwFlags:=DWM_TNP_SOURCECLIENTAREAONLY or DWM_TNP_VISIBLE or
                          DWM_TNP_OPACITY or DWM_TNP_RECTDESTINATION;
           Props.fSourceClientAreaOnly:=false;
           Props.fVisible:=true;
           Props.opacity:=100;
           Props.rcDestination:=Form4.ClientRect;
           if Succeeded(DwmUpdateThumbnailProperties(PH^,Props))then
              ShowMessage('Thumbnail готов')
           else
             ShowMessage('DwmUpdateThumbnailProperties false');
         end
      else
        begin
          ShowMessage('DwmRegisterThumbnail False ');
        end;
    end;
end;

end.
