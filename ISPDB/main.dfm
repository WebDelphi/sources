object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'ISPDB'
  ClientHeight = 320
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 370
    Height = 47
    Align = alTop
    Caption = #1055#1086#1095#1090#1072
    TabOrder = 0
    object Label1: TLabel
      Left = 14
      Top = 22
      Width = 28
      Height = 13
      Caption = 'E-mail'
    end
    object Button1: TButton
      Left = 237
      Top = 16
      Width = 102
      Height = 25
      Caption = #1053#1072#1081#1090#1080' '#1085#1072#1089#1090#1088#1086#1081#1082#1080
      TabOrder = 0
      OnClick = Button1Click
    end
    object edMail: TEdit
      Left = 48
      Top = 18
      Width = 183
      Height = 21
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 47
    Width = 370
    Height = 114
    Align = alTop
    Caption = #1057#1077#1088#1077#1074#1077#1088' '#1074#1093#1086#1076#1103#1097#1077#1081' '#1087#1086#1095#1090#1099
    TabOrder = 1
    object Label2: TLabel
      Left = 14
      Top = 26
      Width = 49
      Height = 13
      Caption = #1055#1088#1086#1090#1086#1082#1086#1083
    end
    object Label3: TLabel
      Left = 14
      Top = 57
      Width = 31
      Height = 13
      Caption = #1040#1076#1088#1077#1089
    end
    object Label4: TLabel
      Left = 287
      Top = 57
      Width = 25
      Height = 13
      Caption = #1055#1086#1088#1090
    end
    object Label5: TLabel
      Left = 14
      Top = 88
      Width = 93
      Height = 13
      Caption = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
    end
    object Label10: TLabel
      Left = 138
      Top = 26
      Width = 81
      Height = 13
      Caption = #1058#1080#1087' '#1089#1086#1077#1076#1080#1085#1077#1085#1080#1103
    end
    object cbInProtocol: TComboBox
      Left = 76
      Top = 23
      Width = 51
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'POP3'
      OnChange = cbInProtocolChange
      Items.Strings = (
        'POP3'
        'IMAP')
    end
    object edInAddress: TEdit
      Left = 116
      Top = 54
      Width = 159
      Height = 21
      TabOrder = 1
    end
    object edInPrort: TEdit
      Left = 318
      Top = 54
      Width = 41
      Height = 21
      TabOrder = 2
    end
    object cbInConnect: TComboBox
      Left = 228
      Top = 23
      Width = 47
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 3
      Text = 'Plain'
      OnChange = cbInProtocolChange
      Items.Strings = (
        'SSL'
        'TLS'
        'Plain')
    end
    object edInUser: TEdit
      Left = 116
      Top = 81
      Width = 159
      Height = 21
      TabOrder = 4
    end
  end
  object GroupBox3: TGroupBox
    Left = 0
    Top = 161
    Width = 370
    Height = 140
    Align = alClient
    Caption = #1057#1077#1088#1074#1077#1088' '#1080#1089#1093#1086#1076#1103#1097#1077#1081' '#1087#1086#1095#1090#1099
    TabOrder = 2
    object Label6: TLabel
      Left = 287
      Top = 54
      Width = 25
      Height = 13
      Caption = #1055#1086#1088#1090
    end
    object Label7: TLabel
      Left = 14
      Top = 56
      Width = 31
      Height = 13
      Caption = #1040#1076#1088#1077#1089
    end
    object Label8: TLabel
      Left = 14
      Top = 82
      Width = 93
      Height = 13
      Caption = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
    end
    object Label9: TLabel
      Left = 14
      Top = 109
      Width = 66
      Height = 13
      Caption = #1040#1074#1090#1086#1088#1080#1079#1072#1094#1080#1103
    end
    object Label11: TLabel
      Left = 14
      Top = 24
      Width = 81
      Height = 13
      Caption = #1058#1080#1087' '#1089#1086#1077#1076#1080#1085#1077#1085#1080#1103
    end
    object cbOutConnect: TComboBox
      Left = 116
      Top = 21
      Width = 47
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 0
      Text = 'Plain'
      OnChange = cbOutConnectChange
      Items.Strings = (
        'SSL'
        'TLS'
        'Plain')
    end
    object edOutPort: TEdit
      Left = 318
      Top = 51
      Width = 41
      Height = 21
      TabOrder = 1
    end
    object edOutAddress: TEdit
      Left = 116
      Top = 51
      Width = 159
      Height = 21
      TabOrder = 2
    end
    object edOutUser: TEdit
      Left = 116
      Top = 78
      Width = 159
      Height = 21
      TabOrder = 3
    end
    object edOutAuth: TEdit
      Left = 116
      Top = 105
      Width = 159
      Height = 21
      TabOrder = 4
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 301
    Width = 370
    Height = 19
    Panels = <
      item
        Text = #1053#1072#1081#1076#1077#1085#1086' '#1085#1072#1089#1090#1088#1086#1077#1082
        Width = 110
      end
      item
        Width = 50
      end>
  end
end
