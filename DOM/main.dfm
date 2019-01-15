object fmain: Tfmain
  Left = 0
  Top = 0
  Caption = #1056#1072#1073#1086#1090#1072' '#1089' '#1084#1077#1090#1072'-'#1090#1077#1075#1072#1084#1080' '#1089#1072#1081#1090#1072
  ClientHeight = 283
  ClientWidth = 491
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
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 8
    Top = 35
    Width = 111
    Height = 13
    Caption = #1052#1077#1090#1072'-'#1090#1077#1075#1080' '#1076#1086#1082#1091#1084#1077#1085#1090#1072
  end
  object edURL: TEdit
    Left = 33
    Top = 8
    Width = 376
    Height = 21
    TabOrder = 0
    Text = 'edURL'
  end
  object btnGet: TButton
    Left = 415
    Top = 6
    Width = 75
    Height = 25
    Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100
    TabOrder = 1
    OnClick = btnGetClick
  end
  object ListTags: TListBox
    Left = 8
    Top = 54
    Width = 475
    Height = 221
    ItemHeight = 13
    TabOrder = 2
  end
end
