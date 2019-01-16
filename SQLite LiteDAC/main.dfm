object Form3: TForm3
  Left = 0
  Top = 0
  Caption = #1050#1091#1088#1089' '#1074#1072#1083#1102#1090
  ClientHeight = 516
  ClientWidth = 797
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
  object Label1: TLabel
    Left = 8
    Top = 43
    Width = 76
    Height = 13
    Caption = #1054#1073#1085#1086#1074#1080#1090#1100' '#1082#1091#1088#1089
  end
  object Label2: TLabel
    Left = 303
    Top = 43
    Width = 59
    Height = 13
    Caption = #1079#1072' '#1087#1077#1088#1080#1086#1076' '#1089
  end
  object Label3: TLabel
    Left = 471
    Top = 42
    Width = 12
    Height = 13
    Caption = #1087#1086
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 169
    Height = 25
    Caption = #1054#1073#1085#1086#1074#1080#1090#1100' '#1089#1087#1088#1072#1074#1086#1095#1085#1080#1082' '#1074#1072#1083#1102#1090
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 592
    Top = 37
    Width = 75
    Height = 25
    Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100
    TabOrder = 1
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 96
    Top = 39
    Width = 201
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object DateTimePicker1: TDateTimePicker
    Left = 368
    Top = 39
    Width = 97
    Height = 21
    Date = 42594.278320902780000000
    Time = 42594.278320902780000000
    TabOrder = 3
  end
  object DateTimePicker2: TDateTimePicker
    Left = 489
    Top = 39
    Width = 97
    Height = 21
    Date = 42594.278320902780000000
    Time = 42594.278320902780000000
    TabOrder = 4
  end
  object ListView1: TListView
    Left = 8
    Top = 66
    Width = 781
    Height = 442
    Columns = <
      item
        Caption = #1044#1072#1090#1072
      end
      item
        Caption = #1050#1091#1088#1089
      end>
    TabOrder = 5
    ViewStyle = vsReport
  end
  object Button3: TButton
    Left = 183
    Top = 8
    Width = 122
    Height = 25
    Caption = #1057#1086#1079#1076#1072#1090#1100' '#1076#1072#1084#1087' '#1073#1072#1079#1099
    TabOrder = 6
    OnClick = Button3Click
  end
  object LiteConnection1: TLiteConnection
    Database = 'C:\Valuta\Win32\Debug\base.db'
    Options.Direct = True
    Options.ForceCreateDatabase = True
    Connected = True
    Debug = True
    AfterConnect = LiteConnection1AfterConnect
    BeforeConnect = LiteConnection1BeforeConnect
    Left = 112
    Top = 368
  end
  object LiteSQL1: TLiteSQL
    Connection = LiteConnection1
    SQL.Strings = (
      
        'INSERT OR IGNORE INTO valute (v_code, v_nominal, v_eng_name, v_r' +
        'u_name) '
      'VALUES (:code, :nominal, :eng_name, :ru_name)')
    Debug = True
    Left = 40
    Top = 368
    ParamData = <
      item
        DataType = ftString
        Name = 'code'
        Value = nil
      end
      item
        DataType = ftInteger
        Name = 'nominal'
        Value = nil
      end
      item
        DataType = ftString
        Name = 'eng_name'
        Value = nil
      end
      item
        DataType = ftString
        Name = 'ru_name'
        Value = nil
      end>
  end
  object LiteSQL2: TLiteSQL
    Connection = LiteConnection1
    SQL.Strings = (
      'INSERT OR IGNORE INTO rates'
      '(r_code, r_date, r_value) VALUES '
      '(:code, :date, :value);')
    Debug = True
    Left = 40
    Top = 288
    ParamData = <
      item
        DataType = ftString
        Name = 'code'
        Value = nil
      end
      item
        DataType = ftDate
        Name = 'date'
        Value = nil
      end
      item
        DataType = ftUnknown
        Name = 'value'
        Value = nil
      end>
  end
  object LiteDump1: TLiteDump
    Debug = True
    Connection = LiteConnection1
    Left = 48
    Top = 208
  end
  object LiteSQLMonitor1: TLiteSQLMonitor
    Left = 264
    Top = 232
  end
end
