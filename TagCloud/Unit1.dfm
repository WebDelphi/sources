object Form1: TForm1
  Left = 0
  Top = 0
  Caption = #1054#1073#1083#1072#1082#1086' '#1090#1077#1075#1086#1074
  ClientHeight = 398
  ClientWidth = 629
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
    Width = 233
    Height = 398
    Align = alLeft
    TabOrder = 0
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 231
      Height = 13
      Align = alTop
      Caption = #1048#1089#1093#1086#1076#1085#1099#1081' '#1089#1087#1080#1089#1086#1082' '#1090#1077#1075#1086#1074':'
      ExplicitWidth = 124
    end
    object ListBox1: TListBox
      Left = 1
      Top = 14
      Width = 231
      Height = 353
      Align = alTop
      ItemHeight = 13
      Items.Strings = (
        'delphi'
        'delphi'
        'delphi'
        'delphi'
        'delphi'
        'delphi'
        'delphi'
        'delphi'
        'delphi'
        'delphi'
        'delphi'
        'delphi'
        #1072#1090#1090#1088#1080#1073#1091#1090
        #1086#1073#1083#1072#1082#1086
        #1086#1073#1083#1072#1082#1086
        #1087#1086#1089#1090#1088#1086#1077#1085#1080#1077
        #1087#1088#1086#1075#1088#1072#1084#1084#1072
        #1087#1088#1086#1075#1088#1072#1084#1084#1072
        #1087#1088#1086#1075#1088#1072#1084#1084#1080#1088#1086#1074#1072#1085#1080#1077
        #1087#1091#1079#1099#1088#1100#1082#1086#1084
        #1089#1086#1088#1090#1080#1088#1086#1074#1082#1072
        #1090#1101#1075
        #1090#1101#1075
        #1090#1101#1075)
      Sorted = True
      TabOrder = 0
    end
    object Button1: TButton
      Left = 157
      Top = 371
      Width = 75
      Height = 21
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      TabOrder = 1
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 1
      Top = 371
      Width = 152
      Height = 21
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 233
    Top = 0
    Width = 396
    Height = 398
    Align = alClient
    TabOrder = 1
    object Label2: TLabel
      Left = 1
      Top = 1
      Width = 394
      Height = 13
      Align = alTop
      Caption = #1054#1073#1083#1072#1082#1086' '#1090#1077#1075#1086#1074':'
      ExplicitWidth = 74
    end
    object Label3: TLabel
      Left = 6
      Top = 24
      Width = 155
      Height = 13
      Caption = #1052#1080#1085#1080#1084#1072#1083#1100#1085#1099#1081' '#1088#1072#1079#1084#1077#1088' '#1096#1088#1080#1092#1090#1072':'
    end
    object Label4: TLabel
      Left = 6
      Top = 197
      Width = 179
      Height = 13
      Caption = #1057#1087#1080#1089#1086#1082' '#1074#1093#1086#1078#1076#1077#1085#1080#1081' '#1074' '#1086#1073#1083#1072#1082#1086' '#1090#1101#1075#1086#1074':'
    end
    object RichEdit1: TRichEdit
      Left = 1
      Top = 47
      Width = 394
      Height = 142
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 167
      Top = 20
      Width = 38
      Height = 21
      TabOrder = 1
      Text = '0'
    end
    object UpDown1: TUpDown
      Left = 205
      Top = 20
      Width = 16
      Height = 21
      Associate = Edit2
      TabOrder = 2
    end
    object ListBox2: TListBox
      Left = 5
      Top = 216
      Width = 384
      Height = 176
      ItemHeight = 13
      TabOrder = 3
    end
  end
end
