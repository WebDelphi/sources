object Form8: TForm8
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #1054#1090#1087#1088#1072#1074#1082#1072' '#1092#1072#1081#1083#1086#1074' '#1085#1072' '#1089#1077#1088#1074#1077#1088
  ClientHeight = 359
  ClientWidth = 592
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
    Width = 26
    Height = 13
    Caption = #1060#1072#1081#1083
  end
  object Label2: TLabel
    Left = 16
    Top = 39
    Width = 48
    Height = 13
    Caption = 'MIME-'#1090#1080#1087
  end
  object Label3: TLabel
    Left = 232
    Top = 39
    Width = 66
    Height = 13
    Caption = #1056#1072#1079#1076#1077#1083#1080#1090#1077#1083#1100
  end
  object edFile: TEdit
    Left = 48
    Top = 8
    Width = 506
    Height = 21
    TabOrder = 0
    Text = 'edFile'
  end
  object btnFile: TButton
    Left = 560
    Top = 8
    Width = 26
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = btnFileClick
  end
  object edMime: TEdit
    Left = 70
    Top = 35
    Width = 145
    Height = 21
    TabOrder = 2
    Text = 'edMime'
  end
  object edBoundary: TEdit
    Left = 304
    Top = 35
    Width = 250
    Height = 21
    TabOrder = 3
    Text = 'edBoundary'
  end
  object btnBoundary: TButton
    Left = 559
    Top = 35
    Width = 27
    Height = 21
    Caption = 'get'
    TabOrder = 4
    OnClick = btnBoundaryClick
  end
  object btnSend: TButton
    Left = 199
    Top = 62
    Width = 154
    Height = 25
    Caption = #1054#1090#1087#1088#1072#1074#1080#1090#1100' '#1085#1072' '#1089#1077#1088#1074#1077#1088
    TabOrder = 5
    OnClick = btnSendClick
  end
  object memText: TMemo
    Left = 8
    Top = 93
    Width = 578
    Height = 259
    Lines.Strings = (
      'memText')
    TabOrder = 6
  end
  object dlgOpen: TOpenPictureDialog
    Filter = 
      'All (*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.w' +
      'mf)|*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wm' +
      'f|GIF Image (*.gif)|*.gif|Portable Network Graphics (*.png)|*.pn' +
      'g|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|' +
      'Bitmaps (*.bmp)|*.bmp|TIFF Images (*.tif)|*.tif|TIFF Images (*.t' +
      'iff)|*.tiff|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf' +
      '|Metafiles (*.wmf)|*.wmf|All Files (*.*)|*.*'
    FilterIndex = 0
    Left = 280
    Top = 288
  end
end
