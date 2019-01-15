object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 383
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RichEdit1: TRichEdit
    Left = 8
    Top = 35
    Width = 593
    Height = 287
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'RichEdit1')
    ParentFont = False
    TabOrder = 0
    Zoom = 100
  end
  object edFilePath: TEdit
    Left = 8
    Top = 8
    Width = 560
    Height = 21
    TabOrder = 1
    TextHint = #1055#1091#1090#1100' '#1082' '#1092#1072#1081#1083#1091' '#1089' '#1079#1072#1088#1077#1079#1077#1088#1074#1080#1088#1086#1074#1072#1085#1085#1099#1084#1080' '#1089#1083#1086#1074#1072#1084#1080
  end
  object btnLoadFile: TButton
    Left = 570
    Top = 8
    Width = 31
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = btnLoadFileClick
  end
  object btnHighLight: TButton
    Left = 231
    Top = 328
    Width = 75
    Height = 25
    Caption = #1055#1086#1076#1089#1074#1077#1090#1080#1090#1100
    TabOrder = 3
    OnClick = btnHighLightClick
  end
  object OpenDialog1: TOpenDialog
    Left = 183
    Top = 333
  end
end
