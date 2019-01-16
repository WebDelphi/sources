object Form18: TForm18
  Left = 0
  Top = 0
  Caption = 'Form18'
  ClientHeight = 303
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 67
    Width = 472
    Height = 236
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 0
    Width = 472
    Height = 25
    DataSource = DataSource1
    Align = alTop
    Kind = dbnHorizontal
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 0
    Top = 25
    Width = 472
    Height = 17
    Align = alTop
    Caption = 'Connected'
    TabOrder = 2
    OnClick = CheckBox1Click
  end
  object Button1: TButton
    Left = 0
    Top = 42
    Width = 472
    Height = 25
    Align = alTop
    Caption = 'Apply Updates'
    TabOrder = 3
    OnClick = Button1Click
  end
  object DSProviderConnection1: TDSProviderConnection
    ServerClassName = 'TServerMethods1'
    Connected = True
    SQLConnection = SQLConnection1
    Left = 192
    Top = 126
  end
  object SQLConnection1: TSQLConnection
    ConnectionName = 'FIRSTDATASNAPCONNECTION'
    DriverName = 'DATASNAP'
    Params.Strings = (
      'DriverUnit=Data.DBXDataSnap'
      'DatasnapContext=datasnap/'
      
        'DriverAssemblyLoader=Borland.Data.TDBXClientDriverLoader,Borland' +
        '.Data.DbxClientDriver,Version=16.0.0.0,Culture=neutral,PublicKey' +
        'Token=91d62ebb5b0d1b1b'
      'drivername=DATASNAP'
      'Port=8080'
      'communicationprotocol=http'
      'HostName=192.168.1.7'
      'URLPath=ds/server.dll'
      'Filters={}')
    Connected = True
    Left = 46
    Top = 126
    UniqueId = '{DCD2A5C7-47EC-4F02-8634-511F9A6E1B38}'
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'DataSetProvider1'
    RemoteServer = DSProviderConnection1
    Left = 304
    Top = 206
  end
  object DataSource1: TDataSource
    AutoEdit = False
    DataSet = ClientDataSet1
    Left = 408
    Top = 206
  end
end
