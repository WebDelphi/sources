unit GHelper;

interface

uses Graphics,strutils,Windows,DateUtils,SysUtils, Variants,
Classes,StdCtrls,httpsend,Generics.Collections,xmlintf,xmldom,NativeXML;

resourcestring
  rcErrPrepareNode = '������ ��������� ���� %s';
  rcErrCompNodes = '���� �� �������� ����� %s';
  rcErrWriteNode = '������ ������ ������ ��� ���� %s';
  rcErrReadNode = '������ ������ ������ �� ���� %s';
  rcErrMissValue = '������������ �������� �������� ��� ���� %s';
  rcErrMissAgrument = '������������ �������� � ������ �������';
  rcUnUsedTag = '���������� ��� ';
  rcDuplicateLink = '����� ������ ��� ���� � ������';
  rcWrongAttr = '�������� �������� �������� %s';
  rcRightAttrValues = '���������� �������� ��������: %s';
  rcErrCGroupCreate ='������ XML-��������. ������ ����� ��������� ��������';
  rcErrNullAuth = '�������� Auth �� ����� ���� ������';

const
  GoogleColors: array [1..21]of string = ('A32929','B1365F','7A367A','5229A3',
                                          '29527A','2952A3','1B887A','28754E',
                                          '0D7813','528800','88880E','AB8B00',
                                          'BE6D00','B1440E','865A5A','705770',
                                          '4E5D6C','5A6986','4A716C','6E6E41',
                                          '8D6F47');

  NodeValueAttr = 'value';
  EntryNodeName = 'entry';
  SchemaHref ='http://schemas.google.com/g/2005#';

//��������� ��� TimeZone
  GoogleTimeZones: array [0..308,0..3]of string =
  (('Pacific/Apia','(GMT-11:00) ����','-11,00',''),
   ('Pacific/Midway','(GMT-11:00) ������','-11,00',''),
   ('Pacific/Niue','(GMT-11:00) ����','-11,00',''),
   ('Pacific/Pago_Pago','(GMT-11:00) ����-����','-11,00',''),
   ('Pacific/Fakaofo','(GMT-10:00) �������','-10,00',''),
   ('Pacific/Honolulu','(GMT-10:00) ��������� �����','-10,00',''),
   ('Pacific/Johnston','(GMT-10:00) ����� ��������','-10,00',''),
   ('Pacific/Rarotonga','(GMT-10:00) ���������','-10,00',''),
   ('Pacific/Tahiti','(GMT-10:00) �����','-10,00',''),
   ('Pacific/Marquesas','(GMT-09:30) ���������� �������','-09,30',''),
   ('America/Anchorage','(GMT-09:00) ����� ������','-09,00',''),
   ('Pacific/Gambier','(GMT-09:00) ������','-09,00',''),
   ('America/Los_Angeles','(GMT-08:00) ������������� �����','-08,00',''),
   ('America/Tijuana','(GMT-08:00) ������������� ����� � �������','-08,00',''),
   ('America/Vancouver','(GMT-08:00) ������������� ����� � ��������','-08,00',''),
   ('America/Whitehorse','(GMT-08:00) ������������� ����� � ��������','-08,00',''),
   ('Pacific/Pitcairn','(GMT-08:00) �������','-08,00',''),
   ('America/Dawson_Creek','(GMT-07:00) ������ ����� � ������ ����','-07,00',''),
   ('America/Denver','(GMT-07:00) ������ �����    (America/Denver)','-07,00',''),
   ('America/Edmonton','(GMT-07:00) ������ ����� � ��������','-07,00',''),
   ('America/Hermosillo','(GMT-07:00) ������ ����� � ���������','-07,00',''),
   ('America/Mazatlan','(GMT-07:00) ������ ����� � ������, ��������','-07,00',''),
   ('America/Phoenix','(GMT-07:00) ������ ����� � �������','-07,00',''),
   ('America/Yellowknife','(GMT-07:00) ������ ����� � ����������','-07,00',''),
   ('America/Belize','(GMT-06:00) �����','-06,00',''),
   ('America/Chicago','(GMT-06:00) ����������� �����','-06,00',''),
   ('America/Costa_Rica','(GMT-06:00) �����-����','-06,00',''),
   ('America/El_Salvador','(GMT-06:00) ���������','-06,00',''),
   ('America/Guatemala','(GMT-06:00) ���������','-06,00',''),
   ('America/Managua','(GMT-06:00) �������','-06,00',''),
   ('America/Mexico_City','(GMT-06:00) ����������� ����� � ������','-06,00',''),
   ('America/Regina','(GMT-06:00) ����������� ����� � ��������','-06,00',''),
   ('America/Tegucigalpa','(GMT-06:00) ����������� �����    (America/Tegucigalpa)','-06,00',''),
   ('America/Winnipeg','(GMT-06:00) ����������� ����� � ��������','-06,00',''),
   ('Pacific/Easter','(GMT-06:00) ������ �����','-06,00',''),
   ('Pacific/Galapagos','(GMT-06:00) ���������','-06,00',''),
   ('America/Bogota','(GMT-05:00) ������','-05,00',''),
   ('America/Cayman','(GMT-05:00) ��������� �������','-05,00',''),
   ('America/Grand_Turk','(GMT-05:00) ����� ����','-05,00',''),
   ('America/Guayaquil','(GMT-05:00) ��������','-05,00',''),
   ('America/Havana','(GMT-05:00) ������','-05,00',''),
   ('America/Iqaluit','(GMT-05:00) ��������� ����� � �������','-05,00',''),
   ('America/Jamaica','(GMT-05:00) ������','-05,00',''),
   ('America/Lima','(GMT-05:00) ����','-05,00',''),
   ('America/Montreal','(GMT-05:00) ��������� ����� � ��������','-05,00',''),
   ('America/Nassau','(GMT-05:00) ������','-05,00',''),
   ('America/New_York','(GMT-05:00) ��������� �����','-05,00',''),
   ('America/Panama','(GMT-05:00) ������','-05,00',''),
   ('America/Port-au-Prince','(GMT-05:00) ����-�-�����','-05,00',''),
   ('America/Toronto','(GMT-05:00) ��������� ����� � �������','-05,00',''),
   ('America/Caracas','(GMT-04:30) �������','-04,30',''),
   ('America/Anguilla','(GMT-04:00) �������','-04,00',''),
   ('America/Antigua','(GMT-04:00) �������','-04,00',''),
   ('America/Aruba','(GMT-04:00) �����','-04,00',''),
   ('America/Asuncion','(GMT-04:00) ��������','-04,00',''),
   ('America/Barbados','(GMT-04:00) ��������','-04,00',''),
   ('America/Boa_Vista','(GMT-04:00) ���-�����','-04,00',''),
   ('America/Campo_Grande','(GMT-04:00) �����-������','-04,00',''),
   ('America/Cuiaba','(GMT-04:00) �����','-04,00',''),
   ('America/Curacao','(GMT-04:00) �������','-04,00',''),
   ('America/Dominica','(GMT-04:00) ��������','-04,00',''),
   ('America/Grenada','(GMT-04:00) �������','-04,00',''),
   ('America/Guadeloupe','(GMT-04:00) ���������','-04,00',''),
   ('America/Guyana','(GMT-04:00) ������','-04,00',''),
   ('America/Halifax','(GMT-04:00) ������������� ����� � ��������','-04,00',''),
   ('America/La_Paz','(GMT-04:00) ��-���','-04,00',''),
   ('America/Manaus','(GMT-04:00) ������','-04,00',''),
   ('America/Martinique','(GMT-04:00) ���������','-04,00',''),
   ('America/Montserrat','(GMT-04:00) ���������','-04,00',''),
   ('America/Port_of_Spain','(GMT-04:00) ����-��-�����','-04,00',''),
   ('America/Porto_Velho','(GMT-04:00) �����-�����','-04,00',''),
   ('America/Puerto_Rico','(GMT-04:00) ������-����','-04,00',''),
   ('America/Rio_Branco','(GMT-04:00) ���-������','-04,00',''),
   ('America/Santiago','(GMT-04:00) ��������','-04,00',''),
   ('America/Santo_Domingo','(GMT-04:00) �����-�������','-04,00',''),
   ('America/St_Kitts','(GMT-04:00) ����-����','-04,00',''),
   ('America/St_Lucia','(GMT-04:00) ����-�����','-04,00',''),
   ('America/St_Thomas','(GMT-04:00) ����-�����','-04,00',''),
   ('America/St_Vincent','(GMT-04:00) ����-�������','-04,00',''),
   ('America/Thule','(GMT-04:00) ����','-04,00',''),
   ('America/Tortola','(GMT-04:00) �������','-04,00',''),
   ('Antarctica/Palmer','(GMT-04:00) ������','-04,00',''),
   ('Atlantic/Bermuda','(GMT-04:00) �������','-04,00',''),
   ('Atlantic/Stanley','(GMT-04:00) ������','-04,00',''),
   ('America/St_Johns','(GMT-03:30) ���������������� ����� � ����-�����','-03,30',''),
   ('America/Araguaina','(GMT-03:00) ���������','-03,00',''),
   ('America/Argentina/Buenos_Aires','(GMT-03:00) ������-�����','-03,00',''),
   ('America/Bahia','(GMT-03:00) ���������','-03,00',''),
   ('America/Belem','(GMT-03:00) �����','-03,00',''),
   ('America/Cayenne','(GMT-03:00) �������','-03,00',''),
   ('America/Fortaleza','(GMT-03:00) ���������','-03,00',''),
   ('America/Godthab','(GMT-03:00) ������','-03,00',''),
   ('America/Maceio','(GMT-03:00) ������','-03,00',''),
   ('America/Miquelon','(GMT-03:00) �������','-03,00',''),
   ('America/Montevideo','(GMT-03:00) ����������','-03,00',''),
   ('America/Paramaribo','(GMT-03:00) ����������','-03,00',''),
   ('America/Recife','(GMT-03:00) ������','-03,00',''),
   ('America/Sao_Paulo','(GMT-03:00) ���-�����','-03,00',''),
   ('Antarctica/Rothera','(GMT-03:00) ������','-03,00',''),
   ('America/Noronha','(GMT-02:00) �������','-02,00',''),
   ('Atlantic/South_Georgia','(GMT-02:00) ����� �������','-02,00',''),
   ('America/Scoresbysund','(GMT-01:00) �������','-01,00',''),
   ('Atlantic/Azores','(GMT-01:00) �������� �������','-01,00',''),
   ('Atlantic/Cape_Verde','(GMT-01:00) ������� �������� ����','-01,00',''),
   ('Africa/Abidjan','(GMT+00:00) �������','+00,00',''),
   ('Africa/Accra','(GMT+00:00) �����','+00,00',''),
   ('Africa/Bamako','(GMT+00:00) ������    (Africa/Bamako)','+00,00',''),
   ('Africa/Banjul','(GMT+00:00) ������','+00,00',''),
   ('Africa/Bissau','(GMT+00:00) �����','+00,00',''),
   ('Africa/Casablanca','(GMT+00:00) ����������','+00,00',''),
   ('Africa/Conakry','(GMT+00:00) �������','+00,00',''),
   ('Africa/Dakar','(GMT+00:00) �����','+00,00',''),
   ('Africa/El_Aaiun','(GMT+00:00) ���-���','+00,00',''),
   ('Africa/Freetown','(GMT+00:00) �������','+00,00',''),
   ('Africa/Lome','(GMT+00:00) ����','+00,00',''),
   ('Africa/Monrovia','(GMT+00:00) ��������','+00,00',''),
   ('Africa/Nouakchott','(GMT+00:00) �������','+00,00',''),
   ('Africa/Ouagadougou','(GMT+00:00) ��������','+00,00',''),
   ('Africa/Sao_Tome','(GMT+00:00) ���-����','+00,00',''),
   ('America/Danmarkshavn','(GMT+00:00) �����������','+00,00',''),
   ('Atlantic/Canary','(GMT+00:00) ��������� �������','+00,00',''),
   ('Atlantic/Faroe','(GMT+00:00) ��������� �������','+00,00',''),
   ('Atlantic/Reykjavik','(GMT+00:00) ���������','+00,00',''),
   ('Atlantic/St_Helena','(GMT+00:00) ������ ������ �����','+00,00',''),
   ('Etc/GMT','(GMT+00:00) ����� �� ��������    (��� �������� �� ������ �����)','+00,00',''),
   ('Europe/Dublin','(GMT+00:00) ������','+00,00',''),
   ('Europe/Lisbon','(GMT+00:00) ��������','+00,00',''),
   ('Europe/London','(GMT+00:00) ������    (Europe/London)','+00,00',''),
   ('Africa/Algiers','(GMT+01:00) �����','+01,00',''),
   ('Africa/Bangui','(GMT+01:00) �����','+01,00',''),
   ('Africa/Brazzaville','(GMT+01:00) ����������','+01,00',''),
   ('Africa/Ceuta','(GMT+01:00) �����','+01,00',''),
   ('Africa/Douala','(GMT+01:00) �����','+01,00',''),
   ('Africa/Kinshasa','(GMT+01:00) �������','+01,00',''),
   ('Africa/Lagos','(GMT+01:00) �����','+01,00',''),
   ('Africa/Libreville','(GMT+01:00) ���������','+01,00',''),
   ('Africa/Luanda','(GMT+01:00) ������','+01,00',''),
   ('Africa/Malabo','(GMT+01:00) ������','+01,00',''),
   ('Africa/Ndjamena','(GMT+01:00) ��������','+01,00',''),
   ('Africa/Niamey','(GMT+01:00) ������','+01,00',''),
   ('Africa/Porto-Novo','(GMT+01:00) �����-����','+01,00',''),
   ('Africa/Tunis','(GMT+01:00) �����','+01,00',''),
   ('Africa/Windhoek','(GMT+01:00) �������','+01,00',''),
   ('Europe/Amsterdam','(GMT+01:00) ���������','+01,00',''),
   ('Europe/Andorra','(GMT+01:00) �������','+01,00',''),
   ('Europe/Belgrade','(GMT+01:00) ��������������������� �����    (Europe/Belgrade)','+01,00',''),
   ('Europe/Berlin','(GMT+01:00) ������','+01,00',''),
   ('Europe/Brussels','(GMT+01:00) ��������','+01,00',''),
   ('Europe/Budapest','(GMT+01:00) ��������','+01,00',''),
   ('Europe/Copenhagen','(GMT+01:00) ����������','+01,00',''),
   ('Europe/Gibraltar','(GMT+01:00) ���������','+01,00',''),
   ('Europe/Luxembourg','(GMT+01:00) ����������','+01,00',''),
   ('Europe/Madrid','(GMT+01:00) ������','+01,00',''),
   ('Europe/Malta','(GMT+01:00) ������','+01,00',''),
   ('Europe/Monaco','(GMT+01:00) ������','+01,00',''),
   ('Europe/Oslo','(GMT+01:00) ����    (Europe/Oslo)','+01,00',''),
   ('Europe/Paris','(GMT+01:00) �����','+01,00',''),
   ('Europe/Prague','(GMT+01:00) ��������������������� ����� (Europe/Prague)','+01,00',''),
   ('Europe/Rome','(GMT+01:00) ��� (Europe/Rome)','+01,00',''),
   ('Europe/Stockholm','(GMT+01:00) ���������','+01,00',''),
   ('Europe/Tirane','(GMT+01:00) ������','+01,00',''),
   ('Europe/Vaduz','(GMT+01:00) �����','+01,00',''),
   ('Europe/Vienna','(GMT+01:00) ����','+01,00',''),
   ('Europe/Warsaw','(GMT+01:00) �������','+01,00',''),
   ('Europe/Zurich','(GMT+01:00) �����','+01,00',''),
   ('Africa/Blantyre','(GMT+02:00) ��������','+02,00',''),
   ('Africa/Bujumbura','(GMT+02:00) ���������','+02,00',''),
   ('Africa/Cairo','(GMT+02:00) ����','+02,00',''),
   ('Africa/Gaborone','(GMT+02:00) ��������','+02,00',''),
   ('Africa/Harare','(GMT+02:00) ������','+02,00',''),
   ('Africa/Johannesburg','(GMT+02:00) ������������','+02,00',''),
   ('Africa/Kigali','(GMT+02:00) ������','+02,00',''),
   ('Africa/Lubumbashi','(GMT+02:00) ���������','+02,00',''),
   ('Africa/Lusaka','(GMT+02:00) ������','+02,00',''),
   ('Africa/Maputo','(GMT+02:00) ������','+02,00',''),
   ('Africa/Maseru','(GMT+02:00) ������','+02,00',''),
   ('Africa/Mbabane','(GMT+02:00) �������','+02,00',''),
   ('Africa/Tripoli','(GMT+02:00) �������','+02,00',''),
   ('Asia/Amman','(GMT+02:00) �����','+02,00',''),
   ('Asia/Beirut','(GMT+02:00) ������','+02,00',''),
   ('Asia/Damascus','(GMT+02:00) ������','+02,00',''),
   ('Asia/Gaza','(GMT+02:00) ����','+02,00',''),
   ('Asia/Jerusalem','(GMT+02:00) Jerusalem','+02,00',''),
   ('Asia/Nicosia','(GMT+02:00) �������    (Asia/Nicosia)','+02,00',''),
   ('Europe/Athens','(GMT+02:00) �����','+02,00',''),
   ('Europe/Bucharest','(GMT+02:00) ��������','+02,00',''),
   ('Europe/Chisinau','(GMT+02:00) �������','+02,00',''),
   ('Europe/Helsinki','(GMT+02:00) ��������� (Europe/Helsinki)','+02,00',''),
   ('Europe/Istanbul','(GMT+02:00) ������� (Europe/Istanbul)','+02,00',''),
   ('Europe/Kaliningrad','(GMT+02:00) ������-01 � �����������','+02,00','rus'),
   ('Europe/Kiev','(GMT+02:00) ����','+02,00',''),
   ('Europe/Minsk','(GMT+02:00) �����','+02,00',''),
   ('Europe/Riga','(GMT+02:00) ����','+02,00',''),
   ('Europe/Sofia','(GMT+02:00) �����','+02,00',''),
   ('Europe/Tallinn','(GMT+02:00) �������','+02,00',''),
   ('Europe/Vilnius','(GMT+02:00) �������','+02,00',''),
   ('Africa/Addis_Ababa','(GMT+03:00) �����-�����','+03,00',''),
   ('Africa/Asmara','(GMT+03:00) ������','+03,00',''),
   ('Africa/Dar_es_Salaam','(GMT+03:00) ���-��-�����','+03,00',''),
   ('Africa/Djibouti','(GMT+03:00) �������','+03,00',''),
   ('Africa/Kampala','(GMT+03:00) �������','+03,00',''),
   ('Africa/Khartoum','(GMT+03:00) ������','+03,00',''),
   ('Africa/Mogadishu','(GMT+03:00) ��������','+03,00',''),
   ('Africa/Nairobi','(GMT+03:00) �������','+03,00',''),
   ('Antarctica/Syowa','(GMT+03:00) �����','+03,00',''),
   ('Asia/Aden','(GMT+03:00) ����','+03,00',''),
   ('Asia/Baghdad','(GMT+03:00) ������','+03,00',''),
   ('Asia/Bahrain','(GMT+03:00) �������','+03,00',''),
   ('Asia/Kuwait','(GMT+03:00) ������','+03,00',''),
   ('Asia/Qatar','(GMT+03:00) �����','+03,00',''),
   ('Asia/Riyadh','(GMT+03:00) ��-����','+03,00',''),
   ('Europe/Moscow','(GMT+03:00) ������ +00','+03,00','rus'),
   ('Indian/Antananarivo','(GMT+03:00) ������������','+03,00',''),
   ('Indian/Comoro','(GMT+03:00) ��������� �������','+03,00',''),
   ('Indian/Mayotte','(GMT+03:00) �������','+03,00',''),
   ('Asia/Tehran','(GMT+03:30) �������','+03,30',''),
   ('Asia/Baku','(GMT+04:00) ����','+04,00',''),
   ('Asia/Dubai','(GMT+04:00) �����','+04,00',''),
   ('Asia/Muscat','(GMT+04:00) ������','+04,00',''),
   ('Asia/Tbilisi','(GMT+04:00) �������','+04,00',''),
   ('Asia/Yerevan','(GMT+04:00) ������','+04,00',''),
   ('Europe/Samara','(GMT+04:00) ������ +01 � ������','+04,00','rus'),
   ('Indian/Mahe','(GMT+04:00) ���','+04,00',''),
   ('Indian/Mauritius','(GMT+04:00) ��������','+04,00',''),
   ('Indian/Reunion','(GMT+04:00) �������','+04,00',''),
   ('Asia/Kabul','(GMT+04:30) �����','+04,30',''),
   ('Asia/Aqtau','(GMT+05:00) �����','+05,00',''),
   ('Asia/Aqtobe','(GMT+05:00) ������','+05,00',''),
   ('Asia/Ashgabat','(GMT+05:00) �������','+05,00',''),
   ('Asia/Dushanbe','(GMT+05:00) �������','+05,00',''),
   ('Asia/Karachi','(GMT+05:00) ������','+05,00',''),
   ('Asia/Tashkent','(GMT+05:00) �������','+05,00',''),
   ('Asia/Yekaterinburg','(GMT+05:00) ������ +02 � ������������','+05,00','rus'),
   ('Indian/Kerguelen','(GMT+05:00) ��������','+05,00',''),
   ('Indian/Maldives','(GMT+05:00) ��������','+05,00',''),
   ('Asia/Calcutta','(GMT+05:30) ��������� �����','+05,30',''),
   ('Asia/Colombo','(GMT+05:30) �������','+05,30',''),
   ('Asia/Katmandu','(GMT+05:45) ��������','+05,45',''),
   ('Antarctica/Mawson','(GMT+06:00) ������','+06,00',''),
   ('Antarctica/Vostok','(GMT+06:00) ������','+06,00',''),
   ('Asia/Almaty','(GMT+06:00) ������','+06,00',''),
   ('Asia/Bishkek','(GMT+06:00) ������','+06,00',''),
   ('Asia/Dhaka','(GMT+06:00) �����','+06,00',''),
   ('Asia/Omsk','(GMT+06:00) ������ +03 � ����, �����������','+06,00','rus'),
   ('Asia/Thimphu','(GMT+06:00) �������','+06,00',''),
   ('Indian/Chagos','(GMT+06:00) �����','+06,00',''),
   ('Asia/Rangoon','(GMT+06:30) ������','+06,30',''),
   ('Indian/Cocos','(GMT+06:30) ��������� �������','+06,30',''),
   ('Antarctica/Davis','(GMT+07:00) Davis','+07,00',''),
   ('Asia/Bangkok','(GMT+07:00) �������','+07,00',''),
   ('Asia/Hovd','(GMT+07:00) ����','+07,00',''),
   ('Asia/Jakarta','(GMT+07:00) ��������','+07,00',''),
   ('Asia/Krasnoyarsk','(GMT+07:00) ������ +04 � ����������','+07,00','rus'),
   ('Asia/Phnom_Penh','(GMT+07:00) ��������','+07,00',''),
   ('Asia/Saigon','(GMT+07:00) �����','+07,00',''),
   ('Asia/Vientiane','(GMT+07:00) ��������','+07,00',''),
   ('Indian/Christmas','(GMT+07:00) �������������� �������','+07,00',''),
   ('Antarctica/Casey','(GMT+08:00) �����','+08,00',''),
   ('Asia/Brunei','(GMT+08:00) ������','+08,00',''),
   ('Asia/Choibalsan','(GMT+08:00) ���������','+08,00',''),
   ('Asia/Hong_Kong','(GMT+08:00) �������','+08,00',''),
   ('Asia/Irkutsk','(GMT+08:00) ������ +05 � �������','+08,00','rus'),
   ('Asia/Kuala_Lumpur','(GMT+08:00) �����-������','+08,00',''),
   ('Asia/Macau','(GMT+08:00) �����','+08,00',''),
   ('Asia/Makassar','(GMT+08:00) �������','+08,00',''),
   ('Asia/Manila','(GMT+08:00) ������','+08,00',''),
   ('Asia/Shanghai','(GMT+08:00) ��������� ����� � �����','+08,00',''),
   ('Asia/Singapore','(GMT+08:00) ��������','+08,00',''),
   ('Asia/Taipei','(GMT+08:00) ������','+08,00',''),
   ('Asia/Ulaanbaatar','(GMT+08:00) ����-�����','+08,00',''),
   ('Australia/Perth','(GMT+08:00) �������� ����� � ����','+08,00',''),
   ('Asia/Dili','(GMT+09:00) ����','+09,00',''),
   ('Asia/Jayapura','(GMT+09:00) �������','+09,00',''),
   ('Asia/Pyongyang','(GMT+09:00) �������','+09,00',''),
   ('Asia/Seoul','(GMT+09:00) ����','+09,00',''),
   ('Asia/Tokyo','(GMT+09:00) �����','+09,00',''),
   ('Asia/Yakutsk','(GMT+09:00) ������ +06 � ������','+09,00','rus'),
   ('Pacific/Palau','(GMT+09:00) �����','+09,00',''),
   ('Australia/Adelaide','(GMT+09:30) ����������� ����� � ��������','+09,30',''),
   ('Australia/Darwin','(GMT+09:30) ����������� ����� � ������','+09,30',''),
   ('Antarctica/DumontDUrville','(GMT+10:00) �����-�������','+10,00',''),
   ('Asia/Vladivostok','(GMT+10:00) ������ +07 � ����-���������','+10,00','rus'),
   ('Australia/Brisbane','(GMT+10:00) ��������� ����� � �������','+10,00',''),
   ('Australia/Hobart','(GMT+10:00) ��������� ����� � ������','+10,00',''),
   ('Australia/Sydney','(GMT+10:00) ��������� ����� � ��������, ������','+10,00',''),
   ('Pacific/Guam','(GMT+10:00) ����','+10,00',''),
   ('Pacific/Port_Moresby','(GMT+10:00) ����-������','+10,00',''),
   ('Pacific/Saipan','(GMT+10:00) ������','+10,00',''),
   ('Pacific/Truk','(GMT+10:00) ����    (Pacific/Truk)','+10,00',''),
   ('Asia/Magadan','(GMT+11:00) ������ +08 � �������','+11,00','rus'),
   ('Pacific/Efate','(GMT+11:00) �����','+11,00',''),
   ('Pacific/Guadalcanal','(GMT+11:00) �����������','+11,00',''),
   ('Pacific/Kosrae','(GMT+11:00) Kosrae','+11,00',''),
   ('Pacific/Noumea','(GMT+11:00) �����','+11,00',''),
   ('Pacific/Ponape','(GMT+11:00) ������','+11,00',''),
   ('Pacific/Norfolk','(GMT+11:30) �������','+11,30',''),
   ('Asia/Kamchatka','(GMT+12:00) ������ +09 � �������������-����������','+12,00','rus'),
   ('Pacific/Auckland','(GMT+12:00) ������','+12,00',''),
   ('Pacific/Fiji','(GMT+12:00) �����','+12,00',''),
   ('Pacific/Funafuti','(GMT+12:00) ��������','+12,00',''),
   ('Pacific/Kwajalein','(GMT+12:00) ����������','+12,00',''),
   ('Pacific/Majuro','(GMT+12:00) �������','+12,00',''),
   ('Pacific/Nauru','(GMT+12:00) �����','+12,00',''),
   ('Pacific/Tarawa','(GMT+12:00) ������','+12,00',''),
   ('Pacific/Wake','(GMT+12:00) ������ ����','+12,00',''),
   ('Pacific/Wallis','(GMT+12:00) ������','+12,00',''),
   ('Pacific/Enderbury','(GMT+13:00) ������� ���������','+13,00',''),
   ('Pacific/Tongatapu','(GMT+13:00) ���������','+13,00',''),
   ('Pacific/Kiritimati','(GMT+14:00) ����������','+14,00',''));

  gdRelValues: array [1..25,1..2] of string = (
  ('http://schemas.google.com/g/2005#event',''),
  ('http://schemas.google.com/g/2005#event.alternate',''),
  ('http://schemas.google.com/g/2005#event.parking',''),
  ('http://schemas.google.com/g/2005#message.bcc',''),
  ('http://schemas.google.com/g/2005#message.cc',''),
  ('http://schemas.google.com/g/2005#message.from',''),
  ('http://schemas.google.com/g/2005#message.reply-to',''),
  ('http://schemas.google.com/g/2005#message.to',''),
  ('http://schemas.google.com/g/2005#regular',''),
  ('http://schemas.google.com/g/2005#reviews',''),
  ('http://schemas.google.com/g/2005#home',''),
  ('http://schemas.google.com/g/2005#other',''),
  ('http://schemas.google.com/g/2005#work',''),
  ('http://schemas.google.com/g/2005#fax',''),
  ('http://schemas.google.com/g/2005#home_fax',''),
  ('http://schemas.google.com/g/2005#mobile',''),
  ('http://schemas.google.com/g/2005#pager',''),
  ('http://schemas.google.com/g/2005#work_fax',''),
  ('http://schemas.google.com/g/2005#overall',''),
  ('http://schemas.google.com/g/2005#price',''),
  ('http://schemas.google.com/g/2005#quality',''),
  ('http://schemas.google.com/g/2005#event.attendee',''),
  ('http://schemas.google.com/g/2005#event.organizer',''),
  ('http://schemas.google.com/g/2005#event.performer',''),
  ('http://schemas.google.com/g/2005#event.speaker',''));

//����������� ���� ��� ����������
clNameSpaces: array [0 .. 2, 0 .. 1] of string =
    (('', 'http://www.w3.org/2005/Atom'), ('gd',
      'http://schemas.google.com/g/2005'), ('gCal',
      'http://schemas.google.com/gCal/2005'));
//�������� rel ��� ����� category ����������
clCategories: array [0 .. 1, 0 .. 1] of string = (('scheme',
      'http://schemas.google.com/g/2005#kind'), ('term',
      'http://schemas.google.com/g/2005#event'));

type
 TTimeZone = packed record
   gConst: string;
   Desc : string;
   GMT: extended;
   rus: boolean;
end;

type
  PTimeZone = ^TTimeZone;

type
  TTimeZoneList = class(TList)
  private
    procedure SetRecord(index: Integer; Ptr: PTimeZone);
    function GetRecord(index: Integer): PTimeZone;
  public
    constructor Create;
    procedure Clear;
    destructor Destroy; override;
    property TimeZone[i: Integer]: PTimeZone read GetRecord write SetRecord;
  end;


type
  TAttribute = packed record
    Name: string;
    Value: string;
  end;

type
  TTextTag = class(TPersistent)
  private
    FName: string;
    FValue: string;
    FAtributes: TList<TAttribute>;
  public
    Constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(Node: TXMLNode);
    function AddToXML(Root: TXMLNode): TXMLNode;
    property Value: string read FValue write FValue;
    property Name: string read FName write FName;
    property Attributes: TList<TAttribute>read FAtributes write FAtributes;
  end;

type
  TEntryLink = class(TPersistent)
  private
    Frel: string;
    Ftype: string;
    Fhref: string;
    FEtag: string;
  public
    Constructor Create(const ByNode: TXMLNode=nil);
    procedure ParseXML(Node: TXMLNode);
    function AddToXML(Root: TXMLNode): TXMLNode;
    property Rel:   string read Frel write Frel;
    property Ltype: string read Ftype write Ftype;
    property Href:  string read Fhref write Fhref;
    property Etag:  string read FEtag write FEtag;
  end;

type
  TAuthorTag = Class(TPersistent)
  private
    FAuthor: string;
    FEmail : string;
    FUID   : string;
  public
    constructor Create(ByNode: IXMLNode=nil);
    procedure ParseXML(Node: IXMLNode);
    property Author: string read FAuthor write FAuthor;
    property Email: string read FEmail write FEmail;
  end;

function HexToColor(Color: string): TColor;
function ColorToHex(Color: TColor): string;
//�������������� ������ 2007-07-11T21:50:15.000Z � TDateTime
function ServerDateToDateTime(cServerDate:string):TDateTime;
//�������������� TDateTime � ������ 2007-07-11T21:50:15.000Z
function DateTimeToServerDate(DateTime:TDateTime):string;
//�������������� �����
function ArrayToStr(Values:array of string; Delimiter:char):string;
//������ � HTTP
function GetNewLocationURL(Headers: TStringList):string;
function SendRequest(const aMethod, aURL, aAuth, ApiVersion: string; aDocument:TStream=nil; aExtendedHeaders:TStringList=nil):TStream;


implementation

function ArrayToStr(Values:array of string; Delimiter:char):string;
var i:integer;
begin
  if length(Values)=0 then Exit;
  Result:=Values[0];
  for i:= 1 to Length(Values)-1 do
    Result:=Result+Delimiter+Values[i]
end;

function SendRequest(const aMethod, aURL, aAuth, ApiVersion: string; aDocument:TStream; aExtendedHeaders:TStringList):TStream;
var tmpURL:string;
    i:integer;
begin
  with THTTPSend.Create do
    begin
      Headers.Add('GData-Version: '+ApiVersion);
      Headers.Add('Authorization: GoogleLogin auth='+aAuth);
      MimeType := 'application/atom+xml';
      if aExtendedHeaders<>nil then
        begin
          for I:=0 to aExtendedHeaders.Count - 1 do
            Headers.Add(aExtendedHeaders[i])
        end;
      if aDocument<>nil then
         Document.LoadFromStream(aDocument);

      HTTPMethod(aMethod,aURL);
      if (ResultCode>200)and(ResultCode<400) then
        begin
          tmpURL:=GetNewLocationURL(Headers);
          Document.Clear;
          Headers.Clear;
          Headers.Add('GData-Version: 2');
          Headers.Add('Authorization: GoogleLogin auth='+aAuth);
          MimeType := 'application/atom+xml';
          if aExtendedHeaders<>nil then
            begin
              for I:=0 to aExtendedHeaders.Count - 1 do
                Headers.Add(aExtendedHeaders[i])
            end;
          if aDocument<>nil then
            Document.LoadFromStream(aDocument);
          HTTPMethod(aMethod,tmpURL);
        end;
        Result:=TStringStream.Create('');
        Headers.SaveToFile('headers.txt');
        Document.SaveToStream(Result);
        Result.Seek(0,soFromBeginning);
     end;
end;

function GetNewLocationURL(Headers: TStringList):string;
var i:integer;
begin
  if not Assigned(Headers) then Exit;
  for i:=0 to Headers.Count - 1 do
    begin
      if pos('location:',lowercase(Headers[i]))>0 then
        begin
          Result:=Trim(copy(Headers[i],10,length(Headers[i])-9));
          Exit;
        end;
    end;
end;

function DateTimeToServerDate(DateTime:TDateTime):string;
var Year, Mounth, Day, hours, Mins, Seconds,MSec: Word;
    aYear, aMounth, aDay, ahours, aMins, aSeconds,aMSec: string;
begin
  DecodeDateTime(DateTime,Year, Mounth, Day, hours, Mins, Seconds,MSec);
  aYear:=IntToStr(Year);
  if Mounth<10 then aMounth:='0'+IntToStr(Mounth)
  else aMounth:=IntToStr(Mounth);
  if Day<10 then aDay:='0'+IntToStr(Day)
  else aDay:=IntToStr(Day);
  if hours<10 then ahours:='0'+IntToStr(hours)
  else ahours:=IntToStr(hours);
  if Mins<10 then aMins:='0'+IntToStr(Mins)
  else aMins:=IntToStr(Mins);
  if Seconds<10 then aSeconds:='0'+IntToStr(Seconds)
  else aSeconds:=IntToStr(Seconds);

  case MSec of
    0..9:aMSec:='00'+IntToStr(MSec);
    10..99:aMSec:='0'+IntToStr(MSec);
    else
      aMSec:=IntToStr(MSec);
  end;
  Result:=aYear+'-'+aMounth+'-'+aDay+'T'+ahours+':'+aMins+':'+aSeconds+'.'+aMSec+'Z';
end;

function ServerDateToDateTime(cServerDate:string):TDateTime;
var Year, Mounth, Day, hours, Mins, Seconds,MSec: Word;
begin
  Year:=StrToInt(copy(cServerDate,1,4));
  Mounth:=StrToInt(copy(cServerDate,6,2));
  Day:=StrToInt(copy(cServerDate,9,2));
  if Length(cServerDate)>10 then
     begin
       hours:=StrToInt(copy(cServerDate,12,2));
       Mins:=StrToInt(copy(cServerDate,15,2));
       Seconds:=StrToInt(copy(cServerDate,18,2));
     end
  else
    begin
      hours:=0;
      Mins:=0;
      Seconds:=0;
    end;
  Result:=EncodeDateTime(Year, Mounth, Day, hours, Mins, Seconds,0)
end;

function ColorToHex(Color: TColor): string;
begin
  Result :=
  IntToHex(GetRValue(Color), 2 ) +
  IntToHex(GetGValue(Color), 2 ) +
  IntToHex(GetBValue(Color), 2 );
end;

function HexToColor(Color: string): TColor;
begin
if pos('#',Color)>0 then
  Delete(Color,1,1);
  Result :=
    RGB(
    StrToInt('$' + Copy(Color, 1, 2)),
    StrToInt('$' + Copy(Color, 3, 2)),
    StrToInt('$' + Copy(Color, 5, 2))
    );
end;

{ TTimeZoneList }

procedure TTimeZoneList.Clear;
var
  i: Integer;
  p: PTimeZone;
begin
  for i := 0 to Pred(Count) do
  begin
    p := TimeZone[i];
    if p <> nil then
      Dispose(p);
  end;
  inherited Clear;
end;


constructor TTimeZoneList.Create;
var i:integer;
    Zone:PTimeZone;
begin
  inherited Create;
  for i:=0 to High(GoogleTimeZones) do
    begin
      New(Zone);
      with Zone^ do
       begin
         gConst:=GoogleTimeZones[i,0];
         Desc:=GoogleTimeZones[i,1];
         GMT:=StrToFloat(GoogleTimeZones[i,2]);
         rus:=GoogleTimeZones[i,2]='rus';
       end;
       Add(Zone);
    end;
end;

destructor TTimeZoneList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

function TTimeZoneList.GetRecord(index: Integer): PTimeZone;
begin
  Result:= PTimeZone(Items[index]);
end;

procedure TTimeZoneList.SetRecord(index: Integer; Ptr: PTimeZone);
var
  p: PTimeZone;
begin
  p := TimeZone[index];
  if p <> Ptr then
  begin
    if p <> nil then
      Dispose(p);
    Items[index] := Ptr;
  end;
end;


{ TTextTag }

function TTextTag.AddToXML(Root: TXMLNode): TXMLNode;
var
  i: integer;
begin
  Result:= Root.NodeNew(FName);
  Result.ValueAsString:=AnsiToUtf8(FValue);
  for i := 0 to FAtributes.Count - 1 do
    Result.AttributeAdd(FAtributes[i].Name,FAtributes[i].Value);
  //Root.ChildNodes.Add(Result);
end;

constructor TTextTag.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  FAtributes:=TList<TAttribute>.Create;
  if ByNode = nil then
    Exit;
  ParseXML(ByNode);
end;

procedure TTextTag.ParseXML(Node: TXMLNode);
var
  i: integer;
  Attr: TAttribute;
begin
  try
    FValue := Node.ValueAsString;
    FName := Node.Name;
    for i := 0 to Node.AttributeCount - 1 do
    begin
      Attr.Name := Node.AttributeName[i];
      Attr.Value := Node.AttributeValue[i];
      FAtributes.Add(Attr)
    end;
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.Name]));
  end;
end;

{ TAuthorTag }

{ TAuthorTag }

constructor TAuthorTag.Create(ByNode: IXMLNode);
begin
  inherited Create;
  if ByNode = nil then
    Exit;
  ParseXML(ByNode);
end;

procedure TAuthorTag.ParseXML(Node: IXMLNode);
var
  i: integer;
begin
  try
    for i := 0 to Node.ChildNodes.Count - 1 do
    begin
      if Node.ChildNodes[i].NodeName = 'name' then
        FAuthor := Node.ChildNodes[i].Text
      else
        if Node.ChildNodes[i].NodeName = 'email' then
          FEmail := Node.ChildNodes[i].Text
        else
          if Node.ChildNodes[i].NodeName = 'uid' then
            FUID:=Node.ChildNodes[i].Text;
    end;
  except
    Exception.Create(Format(rcErrPrepareNode, [Node.NodeName]));
  end;
end;


{ TEntryLink }

function TEntryLink.AddToXML(Root: TXMLNode): TXMLNode;
begin

end;

constructor TEntryLink.Create(const ByNode: TXMLNode);
begin
  inherited Create;
  if ByNode<>nil then
    ParseXML(ByNode);
end;

procedure TEntryLink.ParseXML(Node: TXMLNode);
begin
  if Node=nil then Exit;
  try
    Frel:=Node.ReadAttributeString('rel');
    Ftype:=Node.ReadAttributeString('type');
    Fhref:=Node.ReadAttributeString('href');
    FEtag:=Node.ReadAttributeString('gd:etag')
  except
    Exception.Create(Format(rcErrPrepareNode, ['link']));
  end;
end;

end.
