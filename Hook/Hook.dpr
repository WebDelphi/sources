library HookLib;

uses
  SysUtils,Windows,Messages, Ariphm;

const
  HookMsg = WM_USER+$125;

var
  CurHook:HWND;

function KeyboardProc(code: integer; wParam: word; lParam: longint) : longint; stdcall;
var AppWnd:HWND;
Begin
  if code < 0 then
    Result:= CallNextHookEx(CurHook, Code, wParam, lParam)
  else
    begin
      if Byte(LParam shr 24)<$80 then
        begin
          AppWnd:= FindWindow(nil, PChar('�����������'));
          SendMessage(AppWnd,HookMsg,wParam, GetCurrentThreadId {lParam});
          Result:=CallNextHookEx(CurHook, Code, wParam, lParam);
        end;
     end;
end;

procedure hook(Switch: Boolean; HandleProg: HWND) export; stdcall;
begin
  if switch=true then
    begin
      CurHook:= SetWindowsHookEx(WH_KEYBOARD, @KeyboardProc, HInstance, 0);
      if CurHook <> 0 then
          MessageBox(0, '������� ����������� !', '�����������', MB_OK+MB_ICONINFORMATION)
      else
         MessageBox(0, '��������� ������� �� �������!', '������', MB_OK+MB_ICONERROR);
    end
  else
  begin
    if UnhookWindowsHookEx(CurHook) then
      MessageBox(0, '������� �����!', '�����������', MB_OK+MB_ICONINFORMATION)
    else
      MessageBox(0, '�������� ������� �� ������ �� �������!', '', MB_OK+MB_ICONERROR);
  end;
end;

exports hook;

begin

end.
