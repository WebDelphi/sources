object ServerMethods1: TServerMethods1
  OldCreateOrder = False
  Height = 103
  Width = 350
  object SQLConnection1: TSQLConnection
    ConnectionName = 'dsMySQL'
    DriverName = 'MYSQL'
    GetDriverFunc = 'getSQLDriverMYSQL'
    LibraryName = 'dbxmys.dll'
    Params.Strings = (
      'drivername=MYSQL'
      'librarynameosx=libsqlmys.dylib'
      'vendorlibwin64=libmysql.dll'
      'vendorlibosx=libmysqlclient.dylib'
      'blobsize=-1'
      'Database=dsbase'
      'hostname=localhost'
      'localecode=0000'
      'port=3306'
      'user_name=root'
      'compressed=False'
      'encrypted=False'
      'password=masterkey'
      'servercharset=utf8')
    TableScope = [tsSynonym, tsSysTable, tsTable, tsView]
    VendorLib = 'LIBMYSQL.dll'
    Connected = True
    Left = 46
    Top = 26
  end
  object SQLDataSet1: TSQLDataSet
    SchemaName = 'root'
    CommandText = 'select * from dstable'
    DbxCommandType = 'Dbx.SQL'
    MaxBlobSize = -1
    Params = <>
    SQLConnection = SQLConnection1
    Left = 160
    Top = 26
  end
  object DataSetProvider1: TDataSetProvider
    DataSet = SQLDataSet1
    Left = 268
    Top = 26
  end
end
