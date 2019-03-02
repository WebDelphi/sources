object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'HTTP Client API'
  ClientHeight = 410
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    635
    410)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 8
    Top = 36
    Width = 127
    Height = 13
    Caption = 'Automatic Decompression:'
  end
  object Label3: TLabel
    Left = 8
    Top = 59
    Width = 79
    Height = 13
    Caption = 'Accept Encoding'
  end
  object edURL: TEdit
    Left = 33
    Top = 5
    Width = 547
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    TextHint = 'https://www.webdelphi.ru'
  end
  object chGzip: TCheckBox
    Left = 141
    Top = 34
    Width = 40
    Height = 17
    Caption = 'GZip'
    TabOrder = 1
  end
  object btnGet: TButton
    Left = 581
    Top = 5
    Width = 49
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'GET'
    TabOrder = 2
    OnClick = btnGetClick
  end
  object chDeflate: TCheckBox
    Left = 187
    Top = 34
    Width = 54
    Height = 17
    Caption = 'Deflate'
    TabOrder = 3
  end
  object chBrotli: TCheckBox
    Left = 247
    Top = 34
    Width = 40
    Height = 17
    Caption = 'Brotli'
    TabOrder = 4
  end
  object chAny: TCheckBox
    Left = 293
    Top = 34
    Width = 40
    Height = 17
    Caption = 'Any'
    TabOrder = 5
  end
  object edAcceptEncoding: TEdit
    Left = 141
    Top = 55
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'gzip, deflate, br'
  end
  object memContent: TMemo
    Left = 5
    Top = 279
    Width = 622
    Height = 123
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 7
  end
  object memHeaders: TMemo
    Left = 5
    Top = 82
    Width = 622
    Height = 191
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 8
  end
  object NetHTTPClient1: TNetHTTPClient
    Asynchronous = False
    ConnectionTimeout = 60000
    ResponseTimeout = 60000
    HandleRedirects = True
    AllowCookies = True
    UserAgent = 'Embarcadero URI Client/1.0'
    SecureProtocols = [SSL2, SSL3, TLS1, TLS11, TLS12]
    Left = 276
    Top = 171
  end
end
