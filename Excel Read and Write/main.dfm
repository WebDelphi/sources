object Form16: TForm16
  Left = 0
  Top = 0
  Caption = 'Excel Reader'
  ClientHeight = 417
  ClientWidth = 692
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
  object Label2: TLabel
    Left = 0
    Top = 103
    Width = 692
    Height = 23
    Align = alTop
    Caption = #1042#1088#1077#1084#1103' '#1095#1090#1077#1085#1080#1103' '#1074#1089#1077#1075#1086' '#1083#1080#1089#1090#1072': 00.00.00.000'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 351
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 126
    Width = 692
    Height = 291
    Align = alClient
    FixedCols = 0
    FixedRows = 0
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 53
    Width = 692
    Height = 50
    Align = alTop
    Caption = #1042#1072#1088#1080#1072#1085#1090#1099' '#1095#1090#1077#1085#1080#1103' '#1076#1072#1085#1085#1099#1093
    TabOrder = 1
    object Button2: TButton
      Left = 10
      Top = 18
      Width = 111
      Height = 25
      Caption = #1063#1090#1077#1085#1080#1077' '#1087#1086' '#1103#1095#1077#1081#1082#1072#1084
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button1: TButton
      Left = 124
      Top = 18
      Width = 129
      Height = 25
      Caption = #1063#1090#1077#1085#1080#1077' '#1076#1080#1072#1087#1072#1079#1086#1085#1086#1084
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button3: TButton
      Left = 256
      Top = 18
      Width = 177
      Height = 25
      Caption = #1047#1072#1087#1080#1089#1100' '#1076#1072#1085#1085#1099#1093' '#1076#1080#1072#1087#1072#1079#1086#1085#1086#1084
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 0
    Width = 692
    Height = 53
    Align = alTop
    Caption = #1060#1072#1081#1083' Excel'
    TabOrder = 2
    DesignSize = (
      692
      53)
    object Label1: TLabel
      Left = 20
      Top = 22
      Width = 26
      Height = 13
      Caption = #1060#1072#1081#1083
    end
    object edFile: TEdit
      Left = 52
      Top = 19
      Width = 601
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'edFile'
    end
    object btnFile: TButton
      Left = 656
      Top = 19
      Width = 30
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnFileClick
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'xls'
    Filter = #1060#1072#1081#1083#1099' Excel|*.xls'
    Left = 640
    Top = 150
  end
end
