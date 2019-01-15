object Form11: TForm11
  Left = 0
  Top = 0
  Caption = #1055#1088#1086#1074#1077#1088#1082#1072' '#1089#1074#1086#1081#1089#1090#1074' '#1076#1086#1082#1091#1084#1077#1085#1090#1072
  ClientHeight = 292
  ClientWidth = 295
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
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 26
    Height = 13
    Caption = #1060#1072#1081#1083
  end
  object Label2: TLabel
    Left = 8
    Top = 35
    Width = 199
    Height = 13
    Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1086#1081#1089#1090#1074#1072' '#1076#1086#1082#1091#1084#1077#1085#1090#1072':'
  end
  object Label3: TLabel
    Left = 8
    Top = 163
    Width = 136
    Height = 13
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1086#1077' '#1089#1074#1086#1081#1089#1090#1074#1086':'
  end
  object Label4: TLabel
    Left = 20
    Top = 186
    Width = 19
    Height = 13
    Caption = #1048#1084#1103
  end
  object Label5: TLabel
    Left = 20
    Top = 213
    Width = 18
    Height = 13
    Caption = #1058#1080#1087
  end
  object Label6: TLabel
    Left = 20
    Top = 240
    Width = 48
    Height = 13
    Caption = #1047#1085#1072#1095#1077#1085#1080#1077
  end
  object Edit1: TEdit
    Left = 36
    Top = 8
    Width = 181
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 223
    Top = 8
    Width = 59
    Height = 21
    Caption = #1054#1090#1082#1088#1099#1090#1100
    TabOrder = 1
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 54
    Width = 279
    Height = 103
    ItemHeight = 13
    TabOrder = 2
  end
  object Edit2: TEdit
    Left = 74
    Top = 182
    Width = 213
    Height = 21
    TabOrder = 3
    Text = 'Edit2'
  end
  object ComboBox1: TComboBox
    Left = 74
    Top = 209
    Width = 213
    Height = 21
    Style = csDropDownList
    TabOrder = 4
    Items.Strings = (
      'msoPropertyNumber'
      'msoPropertyBoolean'
      'msoPropertyDate'
      'msoPropertyString'
      'msoPropertyFloat')
  end
  object Edit3: TEdit
    Left = 74
    Top = 236
    Width = 213
    Height = 21
    TabOrder = 5
    Text = 'Edit3'
  end
  object Button2: TButton
    Left = 104
    Top = 263
    Width = 75
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 6
    OnClick = Button2Click
  end
  object OpenDialog1: TOpenDialog
    Left = 124
    Top = 84
  end
end
