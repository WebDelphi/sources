object Form11: TForm11
  Left = 0
  Top = 0
  Caption = 'TDownloadThread_tester'
  ClientHeight = 252
  ClientWidth = 608
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 608
    Height = 25
    ButtonHeight = 25
    ButtonWidth = 78
    Caption = 'ToolBar1'
    List = True
    ShowCaptions = True
    TabOrder = 0
    object Edit1: TEdit
      Left = 0
      Top = 0
      Width = 385
      Height = 25
      TabOrder = 0
      Text = 
        'http://www.webdelphi.ru/wp-content/plugins/download-monitor/down' +
        'load.php?id=33'
    end
    object ToolButton1: TToolButton
      Left = 385
      Top = 0
      AutoSize = True
      Caption = #1057#1082#1072#1095#1072#1090#1100
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
    object ToolButton2: TToolButton
      Left = 444
      Top = 0
      AutoSize = True
      Caption = #1054#1089#1090#1072#1085#1086#1074#1080#1090#1100
      ImageIndex = 1
      Visible = False
      OnClick = ToolButton2Click
    end
    object ToolButton3: TToolButton
      Left = 521
      Top = 0
      AutoSize = True
      Caption = #1042#1099#1074#1077#1089#1090#1080
      DropdownMenu = PopupMenu1
      ImageIndex = 2
      Style = tbsDropDown
      Visible = False
      OnClick = ToolButton3Click
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 233
    Width = 608
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitTop = 292
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 25
    Width = 608
    Height = 191
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    ExplicitHeight = 250
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 216
    Width = 608
    Height = 17
    Align = alBottom
    TabOrder = 3
    ExplicitTop = 275
  end
  object Downloader1: TDownloader
    OnError = Downloader1Error
    OnAccepted = Downloader1Accepted
    OnDownloading = Downloader1Downloading
    OnStartDownload = Downloader1StartDownload
    OnBreak = Downloader1Break
    Left = 24
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    Left = 24
    Top = 96
  end
  object PopupMenu1: TPopupMenu
    Left = 24
    Top = 152
    object N1: TMenuItem
      Caption = #1054#1090#1086#1073#1088#1072#1079#1080#1090#1100' '#1082#1072#1082' '#1090#1077#1082#1089#1090
      Default = True
      OnClick = N1Click
    end
    object N2: TMenuItem
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1092#1072#1081#1083
      OnClick = N2Click
    end
  end
end
