object Form8: TForm8
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #1056#1072#1073#1086#1090#1072' '#1089' '#1087#1088#1086#1082#1089#1080
  ClientHeight = 324
  ClientWidth = 567
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 16
    Top = 40
    Width = 96
    Height = 13
    Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1088#1086#1082#1089#1080':'
  end
  object Label3: TLabel
    Left = 32
    Top = 63
    Width = 18
    Height = 13
    Caption = #1058#1080#1087
  end
  object Label4: TLabel
    Left = 192
    Top = 63
    Width = 23
    Height = 13
    Caption = #1061#1086#1089#1090
  end
  object Label5: TLabel
    Left = 348
    Top = 63
    Width = 25
    Height = 13
    Caption = #1055#1086#1088#1090
  end
  object edURL: TEdit
    Left = 41
    Top = 13
    Width = 520
    Height = 21
    TabOrder = 0
    Text = 'http://www.webdelphi.ru/check.php'
  end
  object cbTypeProxy: TComboBox
    Left = 72
    Top = 59
    Width = 105
    Height = 21
    TabOrder = 1
    Text = 'cbTypeProxy'
    Items.Strings = (
      'HTTP'
      'Socks4'
      'Socks5')
  end
  object edProxyHost: TEdit
    Left = 221
    Top = 59
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'edProxyHost'
  end
  object edProxyPort: TEdit
    Left = 379
    Top = 59
    Width = 46
    Height = 21
    TabOrder = 3
    Text = 'edProxyPort'
  end
  object btnGet: TButton
    Left = 200
    Top = 86
    Width = 121
    Height = 25
    Caption = #1055#1086#1083#1091#1095#1080#1090#1100' '#1076#1072#1085#1085#1099#1077
    TabOrder = 4
    OnClick = btnGetClick
  end
  object memText: TMemo
    Left = 8
    Top = 117
    Width = 553
    Height = 200
    Lines.Strings = (
      'memText')
    TabOrder = 5
  end
end
