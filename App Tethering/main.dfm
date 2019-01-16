object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'VCL Client'
  ClientHeight = 363
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 35
    Width = 698
    Height = 287
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 322
    Width = 698
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object Button2: TButton
      Left = 200
      Top = 8
      Width = 106
      Height = 25
      Caption = 'Discover Managers'
      TabOrder = 0
      OnClick = Button2Click
    end
    object Button1: TButton
      Left = 119
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button3: TButton
      Left = 7
      Top = 8
      Width = 106
      Height = 25
      Caption = 'Auto connect'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button5: TButton
      Left = 312
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Run Action'
      TabOrder = 3
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 393
      Top = 8
      Width = 75
      Height = 25
      Action = Action1
      Caption = 'Run Mirror'
      TabOrder = 4
    end
    object Button7: TButton
      Left = 474
      Top = 8
      Width = 95
      Height = 25
      Caption = 'Get Resources'
      TabOrder = 5
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 578
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Send File'
      TabOrder = 6
      OnClick = Button8Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 698
    Height = 35
    Align = alTop
    Caption = 'Panel2'
    ShowCaption = False
    TabOrder = 2
    object Label1: TLabel
      Left = 7
      Top = 10
      Width = 42
      Height = 13
      Caption = 'Manager'
    end
    object Label2: TLabel
      Left = 258
      Top = 10
      Width = 30
      Height = 13
      Caption = 'Profile'
    end
    object cbManagers: TComboBox
      Left = 55
      Top = 6
      Width = 197
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbManagersChange
    end
    object cbProfiles: TComboBox
      Left = 298
      Top = 6
      Width = 145
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object Button4: TButton
      Left = 449
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 2
      OnClick = Button4Click
    end
    object Button9: TButton
      Left = 534
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 3
      OnClick = Button9Click
    end
  end
  object VCLManager: TTetheringManager
    OnEndManagersDiscovery = VCLManagerEndManagersDiscovery
    OnEndProfilesDiscovery = VCLManagerEndProfilesDiscovery
    OnRequestManagerPassword = VCLManagerRequestManagerPassword
    OnRemoteManagerShutdown = VCLManagerRemoteManagerShutdown
    OnEndAutoConnect = VCLManagerEndAutoConnect
    Password = '123456'
    Text = 'VCLManagerDesc'
    AllowedAdapters = 'Network'
    Left = 62
    Top = 54
  end
  object VCLProfile: TTetheringAppProfile
    Manager = VCLManager
    Text = 'VCLProfileDesc'
    Group = 'AppTesting'
    OnDisconnect = VCLProfileDisconnect
    OnBeforeConnectProfile = VCLProfileBeforeConnectProfile
    OnAfterConnectProfile = VCLProfileAfterConnectProfile
    Actions = <
      item
        Name = 'raMemo'
        IsPublic = True
        Kind = Mirror
        Action = Action1
        NotifyUpdates = False
      end>
    Resources = <
      item
        Name = 'rsStream'
        IsPublic = True
        Kind = Mirror
        ResType = Stream
        OnResourceReceived = VCLProfileResources0ResourceReceived
      end
      item
        Name = 'rsInteger'
        IsPublic = True
        Kind = Mirror
      end>
    OnAcceptResource = VCLProfileAcceptResource
    OnResourceUpdated = VCLProfileResourceUpdated
    OnRemoteProfileUpdate = VCLProfileRemoteProfileUpdate
    Left = 174
    Top = 56
  end
  object ActionList1: TActionList
    Left = 278
    Top = 56
    object Action1: TAction
      Caption = 'Action1'
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 594
    Top = 254
  end
end
