unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

const
  HookMsg = WM_USER + $125;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure WndProc(var Msg: TMessage); override;
    procedure SetupHook;
    procedure DeleteHook;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
  hDLL: THandle;
  WM_KEYHOOK: Cardinal;
  Hook: procedure(switch: Boolean; HandleProg: HWND)stdcall;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  SetupHook
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  // Удаляем хук
  DeleteHook;
end;

procedure TForm2.DeleteHook;
begin
  @Hook := nil;
  @Hook := GetProcAddress(hDLL, 'hook');
  Hook(false, Form2.Handle);
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DeleteHook;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm2.SetupHook;
begin
  @Hook := nil;
  hDLL := LoadLibrary(PChar('hook.dll'));
  @Hook := GetProcAddress(hDLL, 'hook');
  Hook(true, Form2.Handle);

end;

procedure TForm2.WndProc(var Msg: TMessage);
var
  VK: integer;
  SC: integer;
  buf: Char;
  KS: TKeyboardState;
  MyHKL: HKL;
begin
  inherited;

  if Msg.Msg = HookMsg then
  begin
    VK := Msg.WParam;
    MyHKL := GetKeyboardLayout(Msg.LParam);
    SC := MapVirtualKeyEx(VK, MAPVK_VK_TO_VSC, MyHKL);
    GetKeyboardState(KS);
    ToUnicodeEx(VK, SC, KS, @buf, sizeof(buf), 0, MyHKL);
    Memo1.Text := Memo1.Text + buf;
    MyHKL := 0;
  End;
end;

end.
