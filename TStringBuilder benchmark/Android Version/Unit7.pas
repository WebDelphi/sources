unit Unit7;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.EditBox, FMX.NumberBox, FMX.Edit,
  FMX.Controls.Presentation, System.Diagnostics;

type
  TForm7 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edStr1: TEdit;
    edStr2: TEdit;
    Label3: TLabel;
    udLimit: TNumberBox;
    CornerButton1: TCornerButton;
    Memo1: TMemo;
    procedure CornerButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TForm7.CornerButton1Click(Sender: TObject);
var
  InitialString: string;
  FinalString: String;
  ConcatStr: string;
  Index: integer;
  StringBuilder: TStringBuilder;
  Limit: integer;
  I: integer;
  t1,t2,t3: TStopwatch;
begin
  Limit := Trunc(udLimit.Value);
  InitialString := edStr1.Text;
  ConcatStr:=edStr2.Text;
  //повторяем тест 10 раз
  for I := 0 to 9 do
  begin
    Memo1.Lines.Add('Проход № '+(i+1).ToString);
    //проверка TStringBuiler #1
    t1:=TStopwatch.StartNew;//засекаем время
    //используем TStringBuilder для сложения двух строк
    StringBuilder := TStringBuilder.Create(InitialString, InitialString.Length + Limit*ConcatStr.Length);
    try
      for Index := 0 to Limit - 1 do
        StringBuilder.Append(ConcatStr);
      //используем старый метод ToString
      FinalString := StringBuilder.ToString();
      t1.Stop;
      //Вывдим время выполнения операций, мс
      Memo1.Lines.Add('TStringBuiler #1 '+t1.ElapsedMilliseconds.ToString);
    finally
      StringBuilder.Free;
    end;

    FinalString:=EmptyStr;

    //проверка TStringBuiler #2
    t2:=TStopwatch.StartNew; //засекаем время
    //используем TStringBuilder для сложения двух строк
    StringBuilder := TStringBuilder.Create(InitialString, InitialString.Length + Limit*ConcatStr.Length);
    try
      for Index := 0 to Limit - 1 do
        StringBuilder.Append(ConcatStr);
      FinalString := StringBuilder.ToString(True);
      t2.Stop;
      //Вывдим время выполнения операций, мс
      Memo1.Lines.Add('TStringBuiler #2 '+t2.ElapsedMilliseconds.ToString);
//      StringGrid1.Cells[3,i+1]:=t2.ElapsedMilliseconds.ToString;
    finally
      StringBuilder.Free;
    end;

    //проверка обычного сложения строк
    FinalString := InitialString;
    t3:=TStopwatch.StartNew; //засекаем время
    for Index := 0 to Limit - 1 do
    begin
      FinalString := FinalString + ConcatStr;
    end;
    t3.Stop;
    //Вывдим время выполнения операций, мс
    Memo1.Lines.Add('Классика '+t3.ElapsedMilliseconds.ToString);
//    StringGrid1.Cells[2,i+1]:=t3.ElapsedMilliseconds.ToString;

    Application.ProcessMessages;
  end;
end;

end.
