object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Speech API'
  ClientHeight = 159
  ClientWidth = 389
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 35
    Width = 42
    Height = 13
    Caption = 'Volume'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object VolumeLabel: TLabel
    Left = 362
    Top = 64
    Width = 6
    Height = 13
    Caption = '0'
  end
  object RateLabel: TLabel
    Left = 362
    Top = 126
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label2: TLabel
    Left = 8
    Top = 95
    Width = 27
    Height = 13
    Caption = 'Rate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 292
    Height = 21
    TabOrder = 0
    Text = 'Hello my Friend. Its me - you voice'
  end
  object Button1: TButton
    Left = 306
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Speech'
    TabOrder = 1
    OnClick = Button1Click
  end
  object TrackBar1: TTrackBar
    Left = 8
    Top = 50
    Width = 348
    Height = 39
    Max = 100
    ParentShowHint = False
    Frequency = 10
    ShowHint = False
    ShowSelRange = False
    TabOrder = 2
    TickMarks = tmBoth
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 8
    Top = 109
    Width = 348
    Height = 45
    Min = -10
    ShowSelRange = False
    TabOrder = 3
    TickMarks = tmBoth
    OnChange = TrackBar2Change
  end
end
