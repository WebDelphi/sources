object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 354
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 56
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 137
    Top = 8
    Width = 394
    Height = 169
    ItemHeight = 13
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 137
    Top = 183
    Width = 394
    Height = 163
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object YandexSpeller1: TYandexSpeller
    WSDLLocation = 'http://speller.yandex.net/services/spellservice?WSDL'
    Service = 'SpellService'
    Port = 'SpellServiceSoap12'
    HTTPWebNode.UseUTF8InHeader = True
    HTTPWebNode.InvokeOptions = [soIgnoreInvalidCerts, soAutoCheckAccessPointViaUDDI]
    HTTPWebNode.WebNodeOptions = []
    Converter.Options = [soSendMultiRefObj, soTryAllSchema, soRootRefNodesToBody, soCacheMimeResponse, soUTF8EncodeXML]
    Format = plain
    Left = 72
    Top = 48
  end
end
