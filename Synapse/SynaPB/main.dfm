object fMain: TfMain
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Synapse Progress'
  ClientHeight = 63
  ClientWidth = 512
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
