object Form5: TForm5
  Left = 0
  Top = 0
  Caption = #1071#1085#1076#1077#1082#1089'.'#1044#1080#1089#1082
  ClientHeight = 354
  ClientWidth = 463
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    463
    354)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 30
    Height = 13
    Caption = #1051#1086#1075#1080#1085
  end
  object Label2: TLabel
    Left = 136
    Top = 8
    Width = 37
    Height = 13
    Caption = #1055#1072#1088#1086#1083#1100
  end
  object Label3: TLabel
    Left = 8
    Top = 42
    Width = 60
    Height = 13
    Caption = #1052#1077#1090#1086#1076' API: '
  end
  object Label4: TLabel
    Left = 8
    Top = 66
    Width = 80
    Height = 13
    Caption = #1054#1090#1074#1077#1090' '#1089#1077#1088#1074#1077#1088#1072':'
  end
  object Edit1: TEdit
    Left = 47
    Top = 4
    Width = 80
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 185
    Top = 4
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 1
    Text = 'Edit2'
  end
  object Memo1: TMemo
    Left = 8
    Top = 80
    Width = 448
    Height = 269
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 74
    Top = 36
    Width = 301
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 3
    Text = #1048#1079#1084#1077#1085#1077#1085#1080#1077' '#1089#1074#1086#1081#1089#1090#1074' '#1092#1072#1081#1083#1072' '#1080#1083#1080' '#1082#1072#1090#1072#1083#1086#1075#1072' (PROPPATCH)'
    Items.Strings = (
      #1057#1074#1086#1081#1089#1090#1074#1072' '#1092#1072#1081#1083#1086#1074' '#1080' '#1082#1072#1090#1072#1083#1086#1075#1086#1074' (PROPFIND)'
      #1048#1079#1084#1077#1085#1077#1085#1080#1077' '#1089#1074#1086#1081#1089#1090#1074' '#1092#1072#1081#1083#1072' '#1080#1083#1080' '#1082#1072#1090#1072#1083#1086#1075#1072' (PROPPATCH)'
      #1057#1086#1079#1076#1072#1085#1080#1077' '#1082#1072#1090#1072#1083#1086#1075#1072' (MKCOL)'
      #1050#1086#1087#1080#1088#1086#1074#1072#1085#1080#1077' '#1092#1072#1081#1083#1072' (COPY)'
      #1055#1077#1088#1077#1084#1077#1097#1077#1085#1080#1077' '#1092#1072#1081#1083#1072' (MOVE)'
      #1059#1076#1072#1083#1077#1085#1080#1077' '#1092#1072#1081#1083#1072' (DELETE)'
      #1047#1072#1075#1088#1091#1079#1082#1072' '#1092#1072#1081#1083#1072' (PUT)'
      #1057#1082#1072#1095#1080#1074#1072#1085#1080#1077' '#1092#1072#1081#1083#1072' (GET)')
  end
  object Button1: TButton
    Left = 381
    Top = 36
    Width = 75
    Height = 21
    Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100
    TabOrder = 4
    OnClick = Button1Click
  end
end
