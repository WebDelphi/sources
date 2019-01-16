unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DCPcrypt2,
  DCPblockciphers, DCPrijndael, DCPsha1;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    edFile: TEdit;
    Button1: TButton;
    Label2: TLabel;
    edPassword: TEdit;
    btnEncrypt: TButton;
    btnDecrypt: TButton;
    Label3: TLabel;
    lbTimer: TLabel;
    DCP_rijndael1: TDCP_rijndael;
    dlgOpenFile: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure btnEncryptClick(Sender: TObject);
    procedure btnDecryptClick(Sender: TObject);
  private
    { Private declarations }
  public
    function EncryptFile(Source, Dest, Password: string): Boolean;
    function DecryptFile(Source, Dest, Password: string): Boolean;
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  if dlgOpenFile.Execute then
    edFile.Text := dlgOpenFile.FileName;
end;

procedure TForm2.btnDecryptClick(Sender: TObject);
var
  iCounterPerSec: TLargeInteger;
  T1, T2: TLargeInteger; // значение счётчика ДО и ПОСЛЕ операции
  SourceStream, DestStream: TFileStream;
begin
  QueryPerformanceFrequency(iCounterPerSec);
  QueryPerformanceCounter(T1);

  if DecryptFile(edFile.Text, ExtractFilePath(ParamStr(0)) + 'decrypted' + ExtractFileExt(edFile.Text), edPassword.Text) then
  begin
    QueryPerformanceCounter(T2);
    lbTimer.Caption := FormatFloat('0.0000', (T2 - T1) / iCounterPerSec) + ' сек.';
  end
  else
    lbTimer.Caption := 'Ошибка!'
end;

procedure TForm2.btnEncryptClick(Sender: TObject);
var
  iCounterPerSec: TLargeInteger;
  T1, T2: TLargeInteger; // значение счётчика ДО и ПОСЛЕ операции
  SourceStream, DestStream: TFileStream;
begin
  QueryPerformanceFrequency(iCounterPerSec);
  QueryPerformanceCounter(T1);

  if EncryptFile(edFile.Text, ExtractFilePath(ParamStr(0)) + 'encrypted' + ExtractFileExt(edFile.Text), edPassword.Text) then
  begin
    QueryPerformanceCounter(T2);
    lbTimer.Caption := FormatFloat('0.0000', (T2 - T1) / iCounterPerSec) + ' сек.';
  end
  else
    lbTimer.Caption := 'Ошибка!'
end;

function TForm2.DecryptFile(Source, Dest, Password: string): Boolean;
var
  SourceStream, DestStream: TFileStream;
begin
  Result := True;
  try
    SourceStream := TFileStream.Create(Source, fmOpenRead);
    try
      DestStream := TFileStream.Create(Dest, fmCreate);
      try
        DCP_rijndael1.InitStr(Password, TDCP_sha1);
        DCP_rijndael1.DecryptStream(SourceStream, DestStream, SourceStream.Size);
        DCP_rijndael1.Burn;
      finally
        FreeAndNil(DestStream);
      end;
    finally
      FreeAndNil(SourceStream)
    end;
  except
    Result := False;
  end;
end;

function TForm2.EncryptFile(Source, Dest, Password: string): Boolean;
var
  SourceStream, DestStream: TFileStream;
begin
  Result := True;
  try
    SourceStream := TFileStream.Create(Source, fmOpenRead); // поток для файла, который будем шифровать
    try
      DestStream := TFileStream.Create(Dest, fmCreate); // поток файла для зашифрованых данных
      try
        DCP_rijndael1.InitStr(Password, TDCP_sha1); // инициализируем ключ (считаем SHA1 для ключа)
        DCP_rijndael1.EncryptStream(SourceStream, DestStream, SourceStream.Size); // шифруем
        DCP_rijndael1.Burn; // "сжигаем" данные о ключе
      finally
        FreeAndNil(DestStream);
      end;
    finally
      FreeAndNil(SourceStream)
    end;
  except
    Result := False;
  end;
end;

initialization

ReportMemoryLeaksOnShutdown := True;

end.
