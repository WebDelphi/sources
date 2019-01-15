object fMain: TfMain
  Left = 0
  Top = 0
  Caption = #1044#1077#1088#1077#1074#1086' '#1076#1086#1082#1091#1084#1077#1085#1090#1086#1074' Google Docs'
  ClientHeight = 380
  ClientWidth = 590
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
    Width = 590
    Height = 57
    Align = alTop
    Caption = #1040#1074#1090#1086#1088#1080#1079#1072#1094#1080#1103
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 24
      Height = 13
      Caption = 'Email'
    end
    object Label2: TLabel
      Left = 173
      Top = 24
      Width = 37
      Height = 13
      Caption = #1055#1072#1088#1086#1083#1100
    end
    object edEmail: TEdit
      Left = 46
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object edPass: TEdit
      Left = 216
      Top = 21
      Width = 121
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object btnLogin: TButton
      Left = 343
      Top = 19
      Width = 75
      Height = 25
      Caption = 'Login'
      TabOrder = 2
      OnClick = btnLoginClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 57
    Width = 590
    Height = 80
    Align = alTop
    Caption = #1056#1072#1073#1086#1090#1072' '#1089#1086' '#1089#1087#1080#1089#1082#1086#1084' '#1076#1086#1082#1091#1084#1077#1085#1090#1086#1074
    TabOrder = 1
    object lbDocCount: TLabel
      Left = 136
      Top = 56
      Width = 6
      Height = 13
      Caption = '0'
    end
    object Label3: TLabel
      Left = 16
      Top = 55
      Width = 108
      Height = 13
      Caption = #1044#1086#1082#1091#1084#1077#1085#1090#1086#1074' '#1074' '#1089#1087#1080#1089#1082#1077
    end
    object btnGetList: TButton
      Left = 182
      Top = 24
      Width = 108
      Height = 25
      BiDiMode = bdRightToLeftNoAlign
      Caption = #1055#1086#1083#1091#1095#1080#1090#1100' '#1089#1087#1080#1089#1086#1082
      ParentBiDiMode = False
      TabOrder = 0
      OnClick = btnGetListClick
    end
    object btnTree: TButton
      Left = 296
      Top = 24
      Width = 119
      Height = 25
      BiDiMode = bdRightToLeftNoAlign
      Caption = #1055#1086#1089#1090#1088#1086#1080#1090#1100' '#1076#1077#1088#1077#1074#1086
      ParentBiDiMode = False
      TabOrder = 1
      OnClick = btnTreeClick
    end
  end
  object rgTreeMode: TRadioGroup
    Left = 0
    Top = 137
    Width = 590
    Height = 48
    Align = alTop
    Caption = #1056#1077#1078#1080#1084' '#1087#1086#1089#1090#1088#1086#1077#1085#1080#1103
    Columns = 3
    ItemIndex = 2
    Items.Strings = (
      #1060#1072#1081#1083#1099
      #1055#1072#1087#1082#1080
      #1042#1089#1105' '#1074#1084#1077#1089#1090#1077)
    TabOrder = 2
    OnClick = rgTreeModeClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 185
    Width = 590
    Height = 195
    Align = alClient
    TabOrder = 3
    object Splitter1: TSplitter
      Left = 277
      Top = 1
      Height = 193
      ExplicitLeft = 240
      ExplicitTop = 32
      ExplicitHeight = 100
    end
    object GoogleVirtualTree1: TGoogleVirtualTree
      Left = 1
      Top = 1
      Width = 276
      Height = 193
      Align = alLeft
      Indent = 19
      TabOrder = 0
      OnClick = GoogleVirtualTree1Click
      Mode = mBoth
      RootName = #1052#1086#1080' '#1076#1086#1082#1091#1084#1077#1085#1090#1099
    end
    object GroupBox3: TGroupBox
      Left = 280
      Top = 1
      Width = 309
      Height = 193
      Align = alClient
      Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086' '#1076#1086#1082#1091#1084#1077#1085#1090#1077
      TabOrder = 1
      DesignSize = (
        309
        193)
      object Label5: TLabel
        Left = 16
        Top = 24
        Width = 18
        Height = 13
        Caption = #1058#1080#1087
      end
      object Label6: TLabel
        Left = 16
        Top = 43
        Width = 48
        Height = 13
        Caption = #1053#1072#1079#1074#1072#1085#1080#1077
      end
      object Label7: TLabel
        Left = 16
        Top = 62
        Width = 11
        Height = 13
        Caption = 'ID'
      end
      object Label8: TLabel
        Left = 16
        Top = 81
        Width = 63
        Height = 13
        Caption = 'ID '#1088#1086#1076#1080#1090#1077#1083#1103
      end
      object lbDocType: TLabel
        Left = 88
        Top = 24
        Width = 209
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'lbDocType'
        EllipsisPosition = epWordEllipsis
      end
      object lbTitle: TLabel
        Left = 88
        Top = 43
        Width = 209
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'lbTitle'
        EllipsisPosition = epWordEllipsis
      end
      object lbID: TLabel
        Left = 88
        Top = 62
        Width = 209
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'lbID'
        EllipsisPosition = epWordEllipsis
      end
      object lbIDParent: TLabel
        Left = 88
        Top = 81
        Width = 209
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'lbIDParent'
        EllipsisPosition = epWordEllipsis
      end
    end
  end
  object GoogleLogin1: TGoogleLogin
    AppName = 'My-Application'
    AccountType = HOSTED_OR_GOOGLE
    Service = writely
    OnAutorization = GoogleLogin1Autorization
    Left = 520
    Top = 320
  end
end
