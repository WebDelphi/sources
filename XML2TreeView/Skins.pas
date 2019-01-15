unit Skins;

interface

uses SysUtils, Classes, xmldoc, XMLIntf,Graphics,controls, Forms,
  Dialogs, ExtCtrls,Windows;

type
  TButtonState = (bsNormal, bsActive, bsPressed, bsDisable);

type
  TSkinButton = class
  private
    FXMLNode     : IXMLNode;
    FNormalRect  : TRect;
    FLightRect   : TRect;
    FPressedRect : TRect;
    FDisabledRect: TRect;
    FPosRect     : TRect;
    function GetRect(const rName: string):TRect;
    function GetHeight:integer;
    function GetWidth: integer;
    procedure SetHeight(aHeight:integer);
    procedure SetWidth(aWidth: integer);
  public
    constructor Create(aXMLNode: IXMLNode);
    destructor Destroy;
    procedure ToCanvas(Canvas:TCanvas; const ClrCanvas: boolean=true;State:TButtonState=bsNormal);
    function FromCanvas(Canvas:TCanvas; State: TButtonState=bsNormal):TBitmap;
    property XNLNode: IXMLNode read FXMLNode write FXMLNode;
    property NormalRect: TRect read FNormalRect write FNormalRect;
    property LightRect: TRect read FLightRect write FLightRect;
    property PressedRect: TRect read FPressedRect write FPressedRect;
    property DisabledRect: TRect read FDisabledRect write FDisabledRect;
    property PosRect: TRect read FPosRect write FPosRect;
    property Height: integer read GetHeight write SetHeight;
    property Width : integer read GetWidth write SetHeight;
end;



implementation

{ TSkinButton }

constructor TSkinButton.Create(aXMLNode: IXMLNode);
begin
inherited Create;
  if aXMLNode=nil then Exit;
  FXMLNode:=aXMLNode;
  try
    FNormalRect:=GetRect('NormalRect');
    FLightRect:=GetRect('LightRect');
    FPressedRect:=GetRect('PressedRect');
    FDisabledRect:=GetRect('DisabledRect');
    FPosRect:=GetRect('PosRect');
  except
    ShowMessage('Ошибка создания кнопки');
    Destroy;
  end;
end;

destructor TSkinButton.Destroy;
begin
 FXMLNode:=nil;
 inherited Destroy;
end;

function TSkinButton.FromCanvas(Canvas: TCanvas; State: TButtonState): TBitmap;
var Dest:TRect;
b: TBitmap;
begin
  case State of
    bsNormal:Canvas.CopyRect(Dest,Canvas,FNormalRect);
    bsActive:Canvas.CopyRect(Dest,Canvas ,FNormalRect);
    bsPressed:Canvas.CopyRect(Dest,Canvas, FNormalRect);
    bsDisable:Canvas.CopyRect(Dest,Canvas ,FNormalRect) ;
  end;
 // Result.;
end;

function TSkinButton.GetHeight: integer;
begin
  Result:=abs(FNormalRect.Top-FNormalRect.Bottom)
end;

function TSkinButton.GetRect(const rName: string): TRect;
var StrLst: TStringList;
    i:integer;
    s:string;
begin
   StrLst:=TStringList.Create;
   s:=FXMLNode.Attributes[rName];
   i:= ExtractStrings([','], [#08], PChar(s), StrLst);
   if i = 4 then
      Result:=Rect(StrToInt(StrLst[0]),StrToInt(StrLst[1]),StrToInt(StrLst[2]),StrToInt(StrLst[3]))
   else
      Result:=Rect(0,0,0,0)
end;

function TSkinButton.GetWidth: integer;
begin
  Result:=abs(FNormalRect.Right-FNormalRect.Left);
end;

procedure TSkinButton.SetHeight(aHeight: integer);
begin
  FNormalRect.Bottom:=FNormalRect.Top-aHeight;
  FLightRect.Bottom:=FLightRect.Top-aHeight;
  FPressedRect.Bottom:=FPressedRect.Top-aHeight;
  FDisabledRect.Bottom:=FDisabledRect.Top-aHeight;
end;

procedure TSkinButton.SetWidth(aWidth: integer);
begin
  FNormalRect.Right:=FNormalRect.Left-aWidth;
  FLightRect.Right:=FLightRect.Left-aWidth;
  FPressedRect.Right:=FPressedRect.Left-aWidth;
  FDisabledRect.Right:=FDisabledRect.Left-aWidth;
end;

procedure TSkinButton.ToCanvas(Canvas:TCanvas;const ClrCanvas: boolean;
  State: TButtonState);
var X1,Y1,X2,Y2: integer;
begin

  case State of
    bsNormal: begin
                X1:=FNormalRect.Left;
                Y1:=FNormalRect.Top;
                X2:=FNormalRect.Right;
                Y2:=FNormalRect.Bottom;
              end;
    bsActive: begin
                X1:=FLightRect.Left;
                Y1:=FLightRect.Top;
                X2:=FLightRect.Right;
                Y2:=FLightRect.Bottom;
              end;
    bsPressed:begin
                X1:=FPressedRect.Left;
                Y1:=FPressedRect.Top;
                X2:=FPressedRect.Right;
                Y2:=FPressedRect.Bottom;
              end;
    bsDisable:begin
                X1:=FDisabledRect.Left;
                Y1:=FDisabledRect.Top;
                X2:=FDisabledRect.Right;
                Y2:=FDisabledRect.Bottom;
              end;
  end;

with Canvas do
  begin
    if ClrCanvas then FillRect(ClipRect);
    Pen.Color:=clRed;
    MoveTo(X1,Y1);
    LineTo(X1,Y2);
    MoveTo(X1,Y2);
    LineTo(X2,Y2);
    MoveTo(X2,Y2);
    LineTo(X2,Y1);
    MoveTo(X2,Y1);
    LineTo(X1,Y1);
  end;

end;

end.
