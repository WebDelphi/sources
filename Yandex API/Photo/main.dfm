object fmain: Tfmain
  Left = 0
  Top = 0
  Caption = 'PhotoManager'
  ClientHeight = 373
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbAuth: TLabel
    Left = 152
    Top = 10
    Width = 89
    Height = 13
    Caption = #1053#1077' '#1072#1074#1090#1086#1088#1080#1079#1086#1074#1072#1085#1099
  end
  object Label2: TLabel
    Left = 8
    Top = 10
    Width = 48
    Height = 13
    Caption = #1064#1072#1075' '#8470'1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 39
    Width = 48
    Height = 13
    Caption = #1064#1072#1075' '#8470'2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 246
    Top = 39
    Width = 58
    Height = 13
    Caption = #1053#1077' '#1087#1086#1083#1091#1095#1077#1085
  end
  object Label5: TLabel
    Left = 8
    Top = 66
    Width = 414
    Height = 13
    Caption = 
      #1064#1072#1075' '#8470'3 '#1042#1074#1086#1076#1080#1084' '#1074' '#1087#1086#1083#1077' '#1072#1076#1088#1077#1089' '#1086#1073#1097#1077#1081' '#1082#1086#1083#1083#1077#1082#1094#1080#1080' '#1092#1086#1090#1086#1082' (id="photo-lis"' +
      ')'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 8
    Top = 116
    Width = 457
    Height = 13
    Caption = 
      #1064#1072#1075' '#8470'4 '#1042#1099#1073#1080#1088#1072#1077#1084' '#1092#1086#1090#1082#1091' '#1076#1083#1103' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1080' '#1086#1087#1088#1077#1076#1077#1083#1103#1077#1084' '#1085#1077#1086#1073#1093#1086#1076#1080#1084#1099#1077' '#1089#1074#1086#1081 +
      #1089#1090#1074#1072
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lbPhoto1: TLabel
    Left = 117
    Top = 141
    Width = 247
    Height = 13
    AutoSize = False
    Caption = #1085#1077' '#1086#1087#1088#1077#1076#1077#1083#1077#1085#1072
    EllipsisPosition = epPathEllipsis
  end
  object Label7: TLabel
    Left = 8
    Top = 250
    Width = 191
    Height = 13
    Caption = #1064#1072#1075' '#8470'5 '#1043#1088#1091#1079#1080#1084' '#1092#1086#1090#1082#1080' '#1085#1072' '#1089#1077#1088#1074#1077#1088
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 40
    Top = 170
    Width = 48
    Height = 13
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077
  end
  object Label9: TLabel
    Left = 40
    Top = 197
    Width = 49
    Height = 13
    Caption = #1054#1087#1080#1089#1072#1085#1080#1077
  end
  object Label10: TLabel
    Left = 40
    Top = 222
    Width = 23
    Height = 13
    Caption = #1058#1077#1075#1080
  end
  object lbLoadResult: TLabel
    Left = 117
    Top = 279
    Width = 70
    Height = 13
    Caption = #1053#1077' '#1079#1072#1075#1088#1091#1078#1077#1085#1072
  end
  object Button1: TButton
    Left = 57
    Top = 4
    Width = 89
    Height = 25
    Caption = #1040#1074#1090#1086#1088#1080#1079#1072#1094#1080#1103
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 57
    Top = 33
    Width = 180
    Height = 25
    Caption = #1055#1086#1083#1091#1095#1080#1090#1100' '#1089#1077#1088#1074#1080#1089#1085#1099#1081' '#1076#1086#1082#1091#1084#1077#1085#1090
    TabOrder = 1
    OnClick = Button2Click
  end
  object edLoadURL: TEdit
    Left = 57
    Top = 85
    Width = 307
    Height = 21
    TabOrder = 2
  end
  object btnPhoto1: TButton
    Left = 36
    Top = 135
    Width = 75
    Height = 25
    Caption = #1060#1086#1090#1082#1072
    TabOrder = 3
    OnClick = btnPhoto1Click
  end
  object edTitle: TEdit
    Left = 117
    Top = 166
    Width = 247
    Height = 21
    TabOrder = 4
    Text = 'edTitle'
  end
  object edDescription: TEdit
    Left = 117
    Top = 193
    Width = 247
    Height = 21
    TabOrder = 5
    Text = 'edDescription'
  end
  object edTags: TEdit
    Left = 117
    Top = 218
    Width = 247
    Height = 21
    TabOrder = 6
    Text = 'edTags'
  end
  object Button3: TButton
    Left = 36
    Top = 273
    Width = 75
    Height = 25
    Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
    TabOrder = 7
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'png'
    Left = 424
    Top = 50
  end
end
