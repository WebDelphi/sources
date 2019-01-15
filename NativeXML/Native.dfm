object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'NativeXML Test'
  ClientHeight = 249
  ClientWidth = 586
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
    Left = 12
    Top = 12
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 12
    Top = 40
    Width = 49
    Height = 13
    Caption = #1048#1089#1093#1086#1076#1085#1080#1082
  end
  object Label3: TLabel
    Left = 12
    Top = 150
    Width = 68
    Height = 13
    Caption = #1057#1090#1072#1090#1077#1081' '#1074' RSS'
  end
  object Label4: TLabel
    Left = 92
    Top = 150
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 12
    Top = 172
    Width = 53
    Height = 13
    Caption = #1047#1072#1075#1086#1083#1086#1074#1082#1080
  end
  object Label6: TLabel
    Left = 12
    Top = 200
    Width = 86
    Height = 13
    Caption = #1050#1083#1102#1095#1077#1074#1099#1077' '#1089#1083#1086#1074#1072
  end
  object Label7: TLabel
    Left = 12
    Top = 227
    Width = 89
    Height = 13
    Caption = #1044#1072#1090#1072' '#1087#1091#1073#1083#1080#1082#1072#1094#1080#1080
  end
  object Edit1: TEdit
    Left = 37
    Top = 8
    Width = 437
    Height = 21
    TabOrder = 0
    Text = 'http://feeds.feedburner.com/myDelphi?format=xml'
  end
  object Button1: TButton
    Left = 480
    Top = 8
    Width = 98
    Height = 21
    Caption = #1055#1086#1083#1091#1095#1080#1090#1100' XML'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 12
    Top = 55
    Width = 566
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 104
    Top = 169
    Width = 474
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ComboBox1Change
  end
  object Edit2: TEdit
    Left = 104
    Top = 196
    Width = 474
    Height = 21
    TabOrder = 4
  end
  object Edit3: TEdit
    Left = 104
    Top = 223
    Width = 157
    Height = 21
    TabOrder = 5
  end
  object Button2: TButton
    Left = 424
    Top = 223
    Width = 154
    Height = 25
    Caption = #1047#1072#1087#1080#1089#1072#1090#1100' XML'
    TabOrder = 6
    OnClick = Button2Click
  end
end
