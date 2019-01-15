object Form13: TForm13
  Left = 0
  Top = 0
  Caption = 'Form13'
  ClientHeight = 136
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 48
    Height = 13
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077
  end
  object Label2: TLabel
    Left = 12
    Top = 39
    Width = 31
    Height = 13
    Caption = #1040#1076#1088#1077#1089
  end
  object Label3: TLabel
    Left = 12
    Top = 66
    Width = 129
    Height = 13
    Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1087#1086#1076#1087#1080#1089#1095#1080#1082#1086#1074
  end
  object edName: TEdit
    Left = 66
    Top = 8
    Width = 233
    Height = 21
    TabOrder = 0
    Text = 'edName'
  end
  object edAddress: TEdit
    Left = 66
    Top = 35
    Width = 233
    Height = 21
    TabOrder = 1
    Text = 'edAddress'
  end
  object edSubscribers: TEdit
    Left = 152
    Top = 62
    Width = 51
    Height = 21
    TabOrder = 2
    Text = 'edSubscribers'
  end
  object Button1: TButton
    Left = 76
    Top = 92
    Width = 75
    Height = 25
    Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 157
    Top = 92
    Width = 75
    Height = 25
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    TabOrder = 4
    OnClick = Button2Click
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    UseAppManager = True
    Left = 262
    Top = 66
  end
  object BindScope1: TBindScope
    Left = 26
    Top = 64
  end
end
