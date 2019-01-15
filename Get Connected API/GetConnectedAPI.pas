unit GetConnectedAPI;

interface

const
  connect = 'connect.dll';

function GetVPNConnected(hwndParent, dwWizardType, dwContextFlags, dwUserFlags,
  hUserContext: cardinal; pszCommandLine: PWideChar): HRESULT; stdcall;
  external connect name 'GetVPNConnected';

function CreateVPNConnection(hwndParent, dwWizardType, dwContextFlags,
  dwUserFlags, hUserContext: cardinal; pszCommandLine: PWideChar): HRESULT;
  stdcall; external connect name 'CreateVPNConnection';

function GetNetworkConnected(hwndParent, dwWizardType, dwContextFlags,
  dwUserFlags, hUserContext: cardinal; pszCommandLine: PWideChar): HRESULT;
  stdcall; external connect name 'GetNetworkConnected';

function GetInternetConnected(hwndParent, dwWizardType, dwContextFlags,
  dwUserFlags, hUserContext: cardinal; pszCommandLine: PWideChar): HRESULT;
  stdcall; external connect name 'GetInternetConnected';

function IsInternetConnected: HRESULT; stdcall;
  external connect name 'IsInternetConnected';

implementation

end.
