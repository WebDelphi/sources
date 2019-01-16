object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'SuperObject Example'
  ClientHeight = 113
  ClientWidth = 357
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
    Caption = 'JSON'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 43
    Height = 13
    Caption = #1054#1073#1083#1072#1089#1090#1080
  end
  object Label3: TLabel
    Left = 8
    Top = 63
    Width = 37
    Height = 13
    Caption = #1043#1086#1088#1086#1076#1072
  end
  object Label4: TLabel
    Left = 8
    Top = 92
    Width = 79
    Height = 13
    Caption = #1055#1088#1077#1076#1089#1090#1072#1074#1083#1077#1085#1080#1077
  end
  object Label5: TLabel
    Left = 98
    Top = 92
    Width = 12
    Height = 13
    Caption = '---'
  end
  object Edit1: TEdit
    Left = 38
    Top = 8
    Width = 275
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 314
    Top = 8
    Width = 37
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object ComboBox1: TComboBox
    Left = 62
    Top = 35
    Width = 289
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = ComboBox1Change
  end
  object ComboBox2: TComboBox
    Left = 62
    Top = 60
    Width = 289
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ComboBox2Change
  end
  object OpenDialog1: TOpenDialog
    Left = 190
    Top = 44
  end
end
