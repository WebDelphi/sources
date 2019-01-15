object fMain: TfMain
  Left = 0
  Top = 0
  Caption = #1063#1072#1089#1090#1080#1095#1085#1086#1077' '#1089#1082#1072#1095#1080#1074#1072#1085#1080#1077' '#1092#1072#1081#1083#1072
  ClientHeight = 296
  ClientWidth = 472
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
    Top = 12
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 16
    Top = 38
    Width = 94
    Height = 13
    Caption = #1053#1072#1095#1072#1083#1086' '#1076#1080#1072#1087#1072#1079#1086#1085#1072
  end
  object Label3: TLabel
    Left = 183
    Top = 38
    Width = 54
    Height = 13
    Caption = #1086#1082#1086#1085#1095#1072#1085#1080#1077
  end
  object Label4: TLabel
    Left = 310
    Top = 38
    Width = 24
    Height = 13
    Caption = #1073#1072#1081#1090
  end
  object Label5: TLabel
    Left = 16
    Top = 96
    Width = 164
    Height = 13
    Caption = #1056#1072#1079#1084#1077#1088' '#1087#1086#1083#1091#1095#1077#1085#1085#1086#1075#1086' '#1092#1088#1072#1075#1084#1077#1085#1090#1072' '
  end
  object lbSize: TLabel
    Left = 192
    Top = 96
    Width = 27
    Height = 13
    Caption = 'lbSize'
  end
  object Label7: TLabel
    Left = 16
    Top = 115
    Width = 101
    Height = 13
    Caption = #1047#1072#1075#1086#1083#1086#1074#1082#1080' '#1089#1077#1088#1074#1077#1088#1072':'
  end
  object edURL: TEdit
    Left = 48
    Top = 8
    Width = 416
    Height = 21
    TabOrder = 0
    Text = 'edURL'
  end
  object edStart: TEdit
    Left = 116
    Top = 35
    Width = 61
    Height = 21
    TabOrder = 1
    Text = 'edStart'
  end
  object edEnd: TEdit
    Left = 243
    Top = 35
    Width = 61
    Height = 21
    TabOrder = 2
    Text = 'edEnd'
  end
  object btnGet: TButton
    Left = 192
    Top = 62
    Width = 75
    Height = 25
    Caption = #1057#1082#1072#1095#1072#1090#1100
    TabOrder = 3
    OnClick = btnGetClick
  end
  object memHeaders: TMemo
    Left = 8
    Top = 134
    Width = 456
    Height = 154
    Lines.Strings = (
      'memHeaders')
    TabOrder = 4
  end
end
