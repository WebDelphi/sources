object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'Page Speed Online'
  ClientHeight = 292
  ClientWidth = 409
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
    Left = 16
    Top = 12
    Width = 51
    Height = 13
    Caption = #1050#1083#1102#1095' API '
  end
  object Label2: TLabel
    Left = 16
    Top = 37
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object Label3: TLabel
    Left = 16
    Top = 63
    Width = 48
    Height = 13
    Caption = #1057#1082#1086#1088#1086#1089#1090#1100
  end
  object Label4: TLabel
    Left = 16
    Top = 89
    Width = 96
    Height = 13
    Caption = #1044#1088#1091#1075#1080#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1099
  end
  object lbScore: TLabel
    Left = 89
    Top = 63
    Width = 35
    Height = 13
    Caption = 'lbScore'
  end
  object edURL: TEdit
    Left = 73
    Top = 8
    Width = 248
    Height = 21
    TabOrder = 0
    Text = 'AIzaSyBU0T1n1n4UjBFAwMRXIYSfHoT71evM5rs'
  end
  object btnGet: TButton
    Left = 327
    Top = 33
    Width = 75
    Height = 25
    Caption = #1040#1085#1072#1083#1080#1079
    TabOrder = 1
    OnClick = btnGetClick
  end
  object edTestUrl: TEdit
    Left = 73
    Top = 35
    Width = 248
    Height = 21
    TabOrder = 2
    Text = 'http://www.webdelphi.ru'
  end
  object memolog: TMemo
    Left = 8
    Top = 108
    Width = 394
    Height = 180
    Lines.Strings = (
      'memolog')
    TabOrder = 3
  end
end
