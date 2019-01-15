object fMain: TfMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Synapse Progress'
  ClientHeight = 103
  ClientWidth = 503
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
    Left = 8
    Top = 12
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object lbProgress: TLabel
    Left = 432
    Top = 37
    Width = 50
    Height = 13
    Caption = 'lbProgress'
  end
  object Label2: TLabel
    Left = 8
    Top = 58
    Width = 91
    Height = 13
    Caption = #1055#1088#1086#1096#1083#1086' '#1074#1088#1077#1084#1077#1085#1080': '
  end
  object lbElapsed: TLabel
    Left = 105
    Top = 58
    Width = 45
    Height = 13
    Caption = 'lbElapsed'
  end
  object Label3: TLabel
    Left = 8
    Top = 77
    Width = 55
    Height = 13
    Caption = #1054#1089#1090#1072#1083#1086#1089#1100': '
  end
  object lbTime: TLabel
    Left = 105
    Top = 77
    Width = 30
    Height = 13
    Caption = 'lbTime'
  end
  object urlEdit: TEdit
    Left = 33
    Top = 8
    Width = 384
    Height = 21
    TabOrder = 0
    Text = 'http://www.yandex.ru'
  end
  object btnGet: TButton
    Left = 423
    Top = 8
    Width = 75
    Height = 21
    Caption = #1057#1082#1072#1095#1072#1090#1100
    TabOrder = 1
    OnClick = btnGetClick
  end
  object pbFile: TProgressBar
    Left = 8
    Top = 35
    Width = 417
    Height = 17
    TabOrder = 2
  end
end
