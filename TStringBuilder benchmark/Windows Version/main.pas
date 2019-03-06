unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ComCtrls, System.Diagnostics;

type
  TForm6 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    StringGrid1: TStringGrid;
    edStr1: TEdit;
    edStr2: TEdit;
    Label3: TLabel;
    edLimit: TEdit;
    udLimit: TUpDown;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}


procedure TForm6.Button1Click(Sender: TObject);
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
  Limit := udLimit.Position;
  InitialString := edStr1.Text;
  ConcatStr:=edStr2.Text;
  //повторяем тест 10 раз
  for I := 0 to 9 do
  begin
    StringGrid1.Cells[0,i+1]:=(i+1).ToString;
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
      StringGrid1.Cells[1,i+1]:=t1.ElapsedMilliseconds.ToString;
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
      StringGrid1.Cells[3,i+1]:=t2.ElapsedMilliseconds.ToString;
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
    StringGrid1.Cells[2,i+1]:=t3.ElapsedMilliseconds.ToString;

    Application.ProcessMessages;
  end;
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  StringGrid1.Cells[0, 0] := '#';
  StringGrid1.Cells[1, 0] := 'TStringBuilder';
  StringGrid1.Cells[2, 0] := 'Классика';
  StringGrid1.Cells[3, 0] := 'TStringBuilder.ToString(True)';
end;

initialization
  ReportMemoryLeaksOnShutdown:=True;


end.
