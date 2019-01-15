object FMain: TFMain
  Left = 0
  Top = 0
  Caption = 'sitemap loader'
  ClientHeight = 356
  ClientWidth = 558
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 558
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 14
      Width = 19
      Height = 13
      Caption = 'URL'
    end
    object Edit1: TEdit
      Left = 33
      Top = 11
      Width = 440
      Height = 21
      TabOrder = 0
    end
    object Button1: TButton
      Left = 479
      Top = 9
      Width = 75
      Height = 25
      Caption = #1057#1082#1072#1095#1072#1090#1100
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 41
    Width = 558
    Height = 315
    Align = alClient
    Caption = 'Sitemap'
    TabOrder = 1
    object ListView1: TListView
      Left = 2
      Top = 15
      Width = 554
      Height = 298
      Align = alClient
      Columns = <
        item
          AutoSize = True
          Caption = 'URL'
        end
        item
          Caption = #1044#1072#1090#1072' '#1087#1086#1089#1083#1077#1076#1085#1077#1075#1086' '#1080#1079#1084#1077#1085#1077#1085#1080#1103
        end
        item
          Caption = #1055#1088#1080#1086#1088#1080#1090#1077#1090
        end
        item
          Caption = #1063#1072#1089#1090#1086#1090#1072' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103
        end>
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
end
