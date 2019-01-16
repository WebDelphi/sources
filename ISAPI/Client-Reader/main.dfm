object Form19: TForm19
  Left = 0
  Top = 0
  Caption = 'Client-Reader'
  ClientHeight = 442
  ClientWidth = 785
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
    Top = 42
    Width = 785
    Height = 400
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
    Width = 785
    Height = 25
    DataSource = DataSource1
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbRefresh]
    Align = alTop
    Kind = dbnHorizontal
    TabOrder = 1
    ExplicitWidth = 472
  end
  object CheckBox1: TCheckBox
    Left = 0
    Top = 25
    Width = 785
    Height = 17
    Align = alTop
    Caption = 'Connected'
    TabOrder = 2
    OnClick = CheckBox1Click
    ExplicitWidth = 472
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
    Left = 42
    Top = 184
  end
  object SqlServerMethod1: TSqlServerMethod
    GetMetadata = False
    Active = True
    Params = <
      item
        DataType = ftDataSet
        Name = 'ReturnParameter'
        ParamType = ptResult
        Value = 'TDataSet'
      end>
    SQLConnection = SQLConnection1
    ServerMethodName = 'TServerMethods1.GetDataSet'
    Left = 180
    Top = 184
  end
  object ClientDataSet1: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    ProviderName = 'DataSetProvider1'
    Left = 488
    Top = 182
  end
  object DataSetProvider1: TDataSetProvider
    DataSet = SqlServerMethod1
    Left = 330
    Top = 186
  end
  object DataSource1: TDataSource
    AutoEdit = False
    DataSet = ClientDataSet1
    Left = 660
    Top = 184
  end
end
