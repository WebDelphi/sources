object YWM: TYWM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 276
  Width = 387
  object YwmClient: TRESTClient
    Authenticator = YwmAuth
    BaseURL = 'https://api.webmaster.yandex.net/v4/'
    Params = <>
    Left = 198
    Top = 36
  end
  object YwmRequest: TRESTRequest
    Client = YwmClient
    Params = <>
    Response = YwmResponse
    SynchronizedEvents = False
    OnHTTPProtocolError = YwmRequestHTTPProtocolError
    Left = 153
    Top = 108
  end
  object YwmResponse: TRESTResponse
    Left = 243
    Top = 105
  end
  object YwmAuth: TOAuth2Authenticator
    OnAuthenticate = YwmAuthAuthenticate
    AccessTokenEndpoint = 'https://oauth.yandex.ru/token'
    AuthorizationEndpoint = 'https://oauth.yandex.ru/authorize'
    ClientID = 'b61a204d224d4e66ab43d04251a7b1f4'
    ClientSecret = '5f340020ecc74d6cb4b81b88b1523edb'
    RedirectionEndpoint = 'https://oauth.yandex.ru/verification_code'
    Left = 270
    Top = 36
  end
end
