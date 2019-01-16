unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, LiteCall, LiteConsts, Data.DB, DBAccess,
  LiteAccess, Vcl.StdCtrls, cbrf, MemDS, Vcl.Grids, Vcl.DBGrids, CRGrid,
  Vcl.DBCtrls, Vcl.ComCtrls, DADump, LiteDump, DASQLMonitor, LiteSQLMonitor;

type
  TForm3 = class(TForm)
    LiteConnection1: TLiteConnection;
    Button1: TButton;
    LiteSQL1: TLiteSQL;
    Button2: TButton;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    DateTimePicker1: TDateTimePicker;
    Label3: TLabel;
    DateTimePicker2: TDateTimePicker;
    LiteSQL2: TLiteSQL;
    ListView1: TListView;
    LiteDump1: TLiteDump;
    Button3: TButton;
    LiteSQLMonitor1: TLiteSQLMonitor;
    procedure LiteConnection1AfterConnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LiteConnection1BeforeConnect(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    CBR: TCBRF;
    procedure CreateTables;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses System.Diagnostics;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  I: Integer;
  Timer: TStopwatch;
begin
  CBR.LoadValutes;
  Timer := TStopwatch.StartNew;
  ComboBox1.Items.Clear;
  LiteSQL1.Params.ValueCount := CBR.Valutes.Count;
  for I := 0 to Pred(CBR.Valutes.Count) do
  begin
    LiteSQL1.Params[0][I].AsString := CBR.Valutes[I].Code;
    LiteSQL1.Params[1][I].AsInteger := CBR.Valutes[I].Nominal;
    LiteSQL1.Params[2][I].AsString := CBR.Valutes[I].EngName;
    LiteSQL1.Params[3][I].AsString := CBR.Valutes[I].RuName;
    ComboBox1.Items.AddObject(CBR.Valutes[I].RuName, CBR.Valutes[I]);
  end;
  LiteSQL1.Execute(CBR.Valutes.Count);
  Timer.Stop;
  ShowMessage(Timer.Elapsed.TotalSeconds.ToString);
end;

procedure TForm3.Button2Click(Sender: TObject);
var
  I: Integer;
  LI: TListItem;
begin
  CBR.LoadRate(Trim(TValute(ComboBox1.Items.Objects[ComboBox1.ItemIndex]).Code), DateTimePicker1.Date, DateTimePicker2.Date);
  LiteSQL2.Params.ValueCount := CBR.Rate.Count;
  ListView1.Items.BeginUpdate;
  try
    ListView1.Items.Clear;
    for I := 0 to Pred(CBR.Rate.Count) do
    begin
      LI := ListView1.Items.Add;
      LI.Caption := DateToStr(CBR.Rate[I].Date);
      LI.SubItems.Add(CurrToStr(CBR.Rate[I].Value));
      // (:code, :date, :value);
      LiteSQL2.Params[0][I].AsString := CBR.Rate[I].Code;
      LiteSQL2.Params[1][I].AsDate := CBR.Rate[I].Date;
      LiteSQL2.Params[2][I].AsCurrency := CBR.Rate[I].Value;
    end;
    LiteSQL2.Execute(CBR.Rate.Count);
  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TForm3.Button3Click(Sender: TObject);
begin
  LiteDump1.BackupToFile('backup.sql'); // создаем дамп и сохраняем его в поток
  LiteDump1.RestoreFromFile('backup.sql');
end;

procedure TForm3.CreateTables;
begin
  LiteConnection1.ExecSQL('CREATE TABLE IF NOT EXISTS valute (v_id INTEGER PRIMARY KEY AUTOINCREMENT, ' + 'v_code CHAR(6), v_nominal INTEGER, v_eng_name TEXT, v_ru_name TEXT)');
  LiteConnection1.ExecSQL('CREATE TABLE IF NOT EXISTS rates (r_id INTEGER PRIMARY KEY AUTOINCREMENT, ' + 'r_code CHAR(6), r_date DATE, r_value CURRENCY)');
  LiteConnection1.ExecSQL('CREATE UNIQUE INDEX IF NOT EXISTS val_idx ON valute (v_code, v_eng_name)');
  LiteConnection1.ExecSQL('CREATE UNIQUE INDEX IF NOT EXISTS rate_idx ON rates (r_code, r_date)');
  LiteConnection1.ExecSQL('CREATE VIEW IF NOT EXISTS [val_rates] AS select valute.[v_ru_name], rates.[r_date], rates.[r_value] from valute, rates where valute.[v_code]=rates.[r_code]');
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  CBR := TCBRF.Create;
  LiteConnection1.Open; // поключаемся к БД
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CBR);
end;

procedure TForm3.LiteConnection1AfterConnect(Sender: TObject);
begin
  LiteConnection1.ExecSQL('PRAGMA synchronous = NORMAL');
  LiteConnection1.ExecSQL('PRAGMA locking_mode = NORMAL');
  CreateTables;
end;

procedure TForm3.LiteConnection1BeforeConnect(Sender: TObject);
begin
  LiteConnection1.Database := ExtractFilePath(ParamStr(0)) + 'base.db';
  LiteConnection1.Options.Direct := True; // только в Direct Mode можно использовать шифрование
  LiteConnection1.Options.ForceCreateDatabase := True; // проверяем есть ли файл БД на диске
end;

end.
