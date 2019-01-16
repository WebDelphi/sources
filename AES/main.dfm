object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'AES Demo'
  ClientHeight = 125
  ClientWidth = 433
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
    Left = 8
    Top = 12
    Width = 26
    Height = 13
    Caption = #1060#1072#1081#1083
  end
  object Label2: TLabel
    Left = 8
    Top = 39
    Width = 28
    Height = 13
    Caption = #1050#1083#1102#1095
  end
  object Label3: TLabel
    Left = 8
    Top = 102
    Width = 106
    Height = 13
    Caption = #1042#1088#1077#1084#1103' '#1085#1072' '#1086#1073#1088#1072#1073#1086#1090#1082#1091':'
  end
  object lbTimer: TLabel
    Left = 124
    Top = 102
    Width = 30
    Height = 13
    Caption = '0 '#1089#1077#1082'.'
  end
  object edFile: TEdit
    Left = 40
    Top = 8
    Width = 307
    Height = 21
    TabOrder = 0
    TextHint = #1059#1082#1072#1078#1080#1090#1077' '#1088#1072#1089#1087#1086#1083#1086#1078#1077#1085#1080#1077' '#1092#1072#1081#1083#1072
  end
  object Button1: TButton
    Left = 350
    Top = 8
    Width = 75
    Height = 21
    Caption = #1054#1090#1082#1088#1099#1090#1100
    TabOrder = 1
    OnClick = Button1Click
  end
  object edPassword: TEdit
    Left = 40
    Top = 35
    Width = 307
    Height = 21
    TabOrder = 2
    TextHint = #1059#1082#1072#1078#1080#1090#1077' '#1082#1083#1102#1095' '#1096#1080#1092#1088#1086#1074#1072#1085#1080#1103
  end
  object btnEncrypt: TButton
    Left = 8
    Top = 66
    Width = 75
    Height = 25
    Caption = #1064#1080#1092#1088#1086#1074#1072#1090#1100
    TabOrder = 3
    OnClick = btnEncryptClick
  end
  object btnDecrypt: TButton
    Left = 89
    Top = 66
    Width = 96
    Height = 25
    Caption = #1056#1072#1089#1096#1080#1092#1088#1086#1074#1072#1090#1100
    TabOrder = 4
    OnClick = btnDecryptClick
  end
  object DCP_rijndael1: TDCP_rijndael
    Id = 9
    Algorithm = 'Rijndael'
    MaxKeySize = 256
    BlockSize = 128
    Left = 264
    Top = 72
  end
  object dlgOpenFile: TOpenDialog
    Left = 348
    Top = 72
  end
end
