object fmain: Tfmain
  Left = 0
  Top = 0
  Caption = #1056#1072#1073#1086#1090#1072' '#1089' JSON '#1074' Delphi'
  ClientHeight = 286
  ClientWidth = 593
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbPathToFile: TLabel
    Left = 165
    Top = 14
    Width = 420
    Height = 13
    AutoSize = False
    Caption = 'lbPathToFile'
    EllipsisPosition = epPathEllipsis
  end
  object Label1: TLabel
    Left = 12
    Top = 43
    Width = 137
    Height = 13
    Caption = #1055#1072#1088#1099' '#1074' '#1082#1086#1088#1085#1077#1074#1086#1084' '#1086#1073#1098#1077#1082#1090#1077':'
  end
  object Label2: TLabel
    Left = 12
    Top = 68
    Width = 141
    Height = 13
    Caption = #1047#1085#1072#1095#1077#1085#1080#1077' '#1074#1099#1076#1073#1088#1072#1085#1086#1081' '#1087#1072#1088#1099':'
  end
  object Label3: TLabel
    Left = 12
    Top = 186
    Width = 309
    Height = 13
    Caption = #1044#1083#1103' '#1087#1086#1083#1091#1095#1077#1085#1080#1103' '#1087#1072#1088#1089#1080#1085#1075#1072' '#1079#1085#1072#1095#1077#1085#1080' '#1084#1086#1078#1085#1086' '#1080#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1082#1083#1072#1089#1089
  end
  object lbClass: TLabel
    Left = 334
    Top = 186
    Width = 39
    Height = 13
    Caption = 'lbClass'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 151
    Height = 25
    Caption = #1060#1072#1081#1083' '#1089' JSON-'#1076#1072#1085#1085#1099#1084#1080
    TabOrder = 0
    OnClick = Button1Click
  end
  object cbPairs: TComboBox
    Left = 165
    Top = 39
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 1
    OnChange = cbPairsChange
  end
  object mmPairValue: TMemo
    Left = 12
    Top = 87
    Width = 573
    Height = 89
    Lines.Strings = (
      'mmPairValue')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Button2: TButton
    Left = 12
    Top = 214
    Width = 121
    Height = 25
    Caption = #1047#1072#1087#1080#1089#1072#1090#1100' JSON'
    TabOrder = 3
    OnClick = Button2Click
  end
  object OpenDialog1: TOpenDialog
    Left = 514
    Top = 36
  end
end
