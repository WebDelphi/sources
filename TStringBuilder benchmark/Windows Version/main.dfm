object Form6: TForm6
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1058#1077#1089#1090' TStringBuilder '
  ClientHeight = 401
  ClientWidth = 494
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
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 46
    Height = 13
    Caption = #1057#1090#1088#1086#1082#1072' 1'
  end
  object Label2: TLabel
    Left = 12
    Top = 42
    Width = 46
    Height = 13
    Caption = #1057#1090#1088#1086#1082#1072' 2'
  end
  object Label3: TLabel
    Left = 234
    Top = 11
    Width = 111
    Height = 13
    Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1086#1087#1077#1088#1072#1094#1080#1081
  end
  object Button1: TButton
    Left = 189
    Top = 69
    Width = 115
    Height = 25
    Caption = #1050#1086#1085#1082#1072#1090#1077#1085#1080#1088#1091#1081' '#1101#1090#1086'!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 105
    Width = 475
    Height = 283
    ColCount = 4
    RowCount = 11
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 1
    ColWidths = (
      64
      109
      119
      168)
  end
  object edStr1: TEdit
    Left = 99
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'edStr1'
  end
  object edStr2: TEdit
    Left = 99
    Top = 35
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'edStr2'
  end
  object edLimit: TEdit
    Left = 351
    Top = 8
    Width = 67
    Height = 21
    TabOrder = 4
    Text = '30'#160'000'#160'000'
  end
  object udLimit: TUpDown
    Left = 418
    Top = 8
    Width = 16
    Height = 21
    Associate = edLimit
    Min = 1
    Max = 30000000
    Position = 30000000
    TabOrder = 5
  end
end
