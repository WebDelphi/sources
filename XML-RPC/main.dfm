object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Joomla! XML-RPC Explorer'
  ClientHeight = 463
  ClientWidth = 715
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 29
    Width = 281
    Height = 434
    Align = alLeft
    Caption = #1052#1077#1090#1086#1076#1099
    TabOrder = 0
    object ListBox1: TListBox
      Left = 2
      Top = 15
      Width = 277
      Height = 417
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 715
    Height = 29
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 77
      Height = 13
      Caption = #1040#1076#1088#1077#1089' XML-RPC'
    end
    object Button1: TButton
      Left = 630
      Top = 4
      Width = 75
      Height = 21
      Caption = #1054#1087#1088#1086#1089#1080#1090#1100
      TabOrder = 0
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 93
      Top = 4
      Width = 531
      Height = 21
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 281
    Top = 29
    Width = 434
    Height = 434
    Align = alClient
    TabOrder = 2
    object GroupBox2: TGroupBox
      Left = 1
      Top = 1
      Width = 432
      Height = 100
      Align = alTop
      Caption = #1054#1087#1080#1089#1072#1085#1080#1077' '#1084#1077#1090#1086#1076#1072
      TabOrder = 0
      object Memo2: TMemo
        Left = 2
        Top = 15
        Width = 428
        Height = 83
        Align = alClient
        TabOrder = 0
      end
    end
    object GroupBox3: TGroupBox
      Left = 1
      Top = 101
      Width = 432
      Height = 128
      Align = alTop
      Caption = #1057#1087#1080#1089#1086#1082' '#1087#1072#1088#1072#1084#1077#1090#1088#1086#1074
      TabOrder = 1
      object Memo3: TMemo
        Left = 2
        Top = 15
        Width = 428
        Height = 111
        Align = alClient
        TabOrder = 0
      end
    end
    object GroupBox4: TGroupBox
      Left = 1
      Top = 229
      Width = 432
      Height = 204
      Align = alBottom
      Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1087#1086' '#1073#1080#1073#1083#1080#1086#1090#1077#1082#1077
      TabOrder = 2
      object Memo1: TMemo
        Left = 2
        Top = 15
        Width = 428
        Height = 187
        Align = alClient
        TabOrder = 0
      end
    end
  end
end
