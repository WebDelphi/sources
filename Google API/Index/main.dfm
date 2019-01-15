object Form3: TForm3
  Left = 0
  Top = 0
  Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1080#1085#1076#1077#1082#1089#1072#1094#1080#1080' '#1074' Google'
  ClientHeight = 416
  ClientWidth = 741
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 243
    Top = 0
    Height = 416
    ExplicitLeft = 288
    ExplicitTop = 56
    ExplicitHeight = 100
  end
  object Splitter2: TSplitter
    Left = 489
    Top = 0
    Height = 416
    ExplicitLeft = 456
    ExplicitTop = 72
    ExplicitHeight = 100
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 243
    Height = 416
    Align = alLeft
    Caption = #1057#1087#1080#1089#1086#1082' URL '#1076#1083#1103' '#1087#1088#1086#1074#1077#1088#1082#1080
    TabOrder = 0
    object Button1: TButton
      Left = 2
      Top = 389
      Width = 239
      Height = 25
      Align = alBottom
      Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100
      TabOrder = 0
      OnClick = Button1Click
    end
    object ListBox1: TListBox
      Left = 2
      Top = 15
      Width = 239
      Height = 324
      Align = alClient
      ItemHeight = 13
      TabOrder = 1
    end
    object Button2: TButton
      Left = 2
      Top = 364
      Width = 239
      Height = 25
      Align = alBottom
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1080#1079' '#1092#1072#1081#1083#1072
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 2
      Top = 339
      Width = 239
      Height = 25
      Align = alBottom
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' URL'
      TabOrder = 3
      OnClick = Button3Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 246
    Top = 0
    Width = 243
    Height = 416
    Align = alLeft
    Caption = #1042' '#1080#1085#1076#1077#1082#1089#1077
    TabOrder = 1
    object Memo2: TMemo
      Left = 2
      Top = 15
      Width = 239
      Height = 399
      Align = alClient
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 492
    Top = 0
    Width = 249
    Height = 416
    Align = alClient
    Caption = #1053#1045' '#1074' '#1080#1085#1076#1077#1082#1089#1077
    TabOrder = 2
    object Memo3: TMemo
      Left = 2
      Top = 15
      Width = 245
      Height = 399
      Align = alClient
      TabOrder = 0
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 32
    Top = 280
  end
end
