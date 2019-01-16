object Form5: TForm5
  Left = 0
  Top = 0
  Caption = #1071#1085#1076#1077#1082#1089'.'#1044#1080#1089#1082
  ClientHeight = 354
  ClientWidth = 463
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
  object ListView1: TListView
    Left = 0
    Top = 31
    Width = 463
    Height = 323
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'DisplayName'
      end
      item
        AutoSize = True
        Caption = 'Href'
      end
      item
        AutoSize = True
        Caption = 'CreationDate'
      end
      item
        AutoSize = True
        Caption = 'LastModified'
      end
      item
        AutoSize = True
        Caption = 'ContentLength'
      end
      item
        AutoSize = True
        Caption = 'Collection'
      end
      item
        AutoSize = True
        Caption = 'Status Code'
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListView1DblClick
    ExplicitTop = 66
    ExplicitHeight = 288
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 463
    Height = 31
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 30
      Height = 13
      Caption = #1051#1086#1075#1080#1085
    end
    object Label2: TLabel
      Left = 136
      Top = 8
      Width = 37
      Height = 13
      Caption = #1055#1072#1088#1086#1083#1100
    end
    object Edit1: TEdit
      Left = 47
      Top = 4
      Width = 80
      Height = 21
      TabOrder = 0
      Text = 'Edit1'
    end
    object Edit2: TEdit
      Left = 185
      Top = 4
      Width = 121
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
      Text = 'Edit2'
    end
    object Button1: TButton
      Left = 312
      Top = 4
      Width = 75
      Height = 21
      Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100
      TabOrder = 2
      OnClick = Button1Click
    end
  end
end
