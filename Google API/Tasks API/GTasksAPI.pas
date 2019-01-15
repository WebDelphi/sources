unit GTasksAPI;

interface

uses Classes, SysUtils, GoogleOAuth;

const
  /// <summary>Версия API</summary>
  APIVersion = '1';
  /// <summary>Точка доступа к API для чтения и записи данных</summary>
  APIScope = 'https://www.googleapis.com/auth/tasks';
  /// <summary>Точка доступа к API только для чтения данных</summary>
  APIScopeReadOnly = 'https://www.googleapis.com/auth/tasks.readonly';
  /// <summary>шаблон составления URL для обращения к ресурсам API</summary>
  URI = 'https://www.googleapis.com/tasks/v%s/%s/%s/%s%s';
  /// <summary>Шаблон авторизации по протоколу OAuth 2.0</summary>
 // AuthHeader = 'Authorization: OAuth %s';
  /// <summary>Список с заданиями по умолчанию</summary>
  DefaultList = '@default';
  /// <summary>Пользователь по умолчанию</summary>
  DefaultUser = '@me';

type
  /// <summary>
  /// Базовый класс для отправки запросов к API и получения ответов сервера.
  ///  Все результаты выполнения функций передаются в виде строки, содержащей
  ///  JSON-объекты, определенные в официальной документации:
  ///  <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html">
  ///  http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html
  /// </summary>
  TGTaskAPI = class(TComponent)
  private
    FOAuthClient: TOAuth;
    function GetVersion: string;
    procedure SetOAuthClient(const Value: TOAuth);
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    /// <summary>Возвращает все списки заданий для пользователя.
    /// Набор свойств каждого спска заданий описан в официальной документации,
    /// находящейся по адресу
    /// <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasklists">
    /// http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasklists
    ///</summary>
    /// <param name="maxResults">
    ///  Максимальное количество элементов, возвращаемых в результате
    /// </param>
    /// <param name="pageToken">
    ///  Токен страницы, которую необходимо вернуть в результате
    /// </param>
    /// <returns>
    ///  <code>string</code><para>
    ///   Возвращает JSON-объект, содержащий коллекцию списков заданий пользователя.
    ///   Пример: <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/using.html#retrieving_task_lists">
    /// в официальной документации
    /// </returns>
    function ListsList(maxResults: string = '';
      pageToken: string = ''): string;
    /// <summary>Возвращает данные по одному списку заданий пользователя
    /// Набор свойств каждого спска заданий описан в официальной документации,
    /// находящейся по адресу
    /// <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasklists"/>
    /// http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasklists
    ///</summary>
    /// <param name="ListID">
    /// Идентификатор списка
    /// </param>
    /// <returns>
    /// <code>string</code><para>Возвращает JSON-объект, содержащий свойства списка<para>
    ///  Пример: <exref target= "<exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasklists"/>
    ///  в официальной документации
    /// </returns>
    function ListsGet(const ListID: string): string;
    /// <summary>Добавляет новый список заданий к аккаунту пользователя
    ///  Список должен формироваться в JSON-формате и содержать одно или несколько свойств,
    ///  определенных в официальной документации, расположенной по адресу:
    ///  <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasklists"/>
    ///  http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasklists
    ///</summary>
    /// <param name="JSONStream">
    ///  Поток, содержащий JSON-объект списка заданий
    /// </param>
    /// <returns>
    /// <code>string</code><para>Возвращает JSON-объект, содержащий свойства созданного списка<para>
    ///  Пример: <exref target= "<exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/using.html#creating_task_list"/>
    ///  в официальной документации
    /// </returns>
    function ListsInsert(JSONStream: TStringStream):string;
    /// <summary>Возвращает набор всех заданий из определенного списка.
    ///  Набор свойств для каждого задания определен в официальной документации,
    ///  расположенной по адресу:
    ///  <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasks"/>
    ///  http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasks
    ///</summary>
    /// <param name="ListID">
    /// Идентификатор списка
    /// </param>
    /// <returns>
    ///   <code>string</code><para>
    ///  Возвращает JSON-объект, содержащий коллекцию заданий из списка пользователя
    ///  Пример: <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/using.html#retrieving_tasks"/>
    ///  в официальной документации
    /// </returns>
    function TasksList(const ListID: string):string;overload;
    /// <summary>Возвращает набор всех заданий из определенного списка.
    ///  Набор свойств для каждого задания определен в официальной документации,
    ///  расположенной по адресу:
    ///  <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasks"/>
    ///  http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasks
    ///</summary>
    function TasksList(const ListID: string; Params:TStrings):string;overload;
    /// <summary>Возвращает набор свойств определенного задания из списка пользователя
    ///  Набор свойств для каждого задания определен в официальной документации,
    ///  расположенной по адресу:
    ///  <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasks"/>
    ///  http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasks
    ///</summary>
    /// <param name="ListID">
    /// Идентификатор списка
    /// </param>
    function TasksGet(const ListID: string; TaskID:string):string;overload;
    function TasksGet(const TaskID: string):string;overload;
    /// <summary>Добавляет новое задание к списку пользователя
    /// Задание должно формироваться в JSON-формате и содержать одно или несколько свойств,
    /// определенных в официальной документации, расположенной по адресу:
    /// <exref target= "http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasks"/>
    /// http://code.google.com/intl/ru-RU/apis/tasks/v1/reference.html#resource_tasks
    ///</summary>
    /// <param name="ListID">
    /// Идентификатор списка
    /// </param>
    /// <param name="Parent">
    ///
    /// </param>
    /// <param name="Previous">
    ///
    /// </param>
    /// <param name="JSONStream">
    ///
    /// </param>
    function TasksInsert(const ListID, Parent, Previous: string; JSONStream: TStringStream):string; overload;
    function TasksInsert(const ListID: string; JSONStream: TStringStream):string; overload;
    function TasksInsert(const JSONStream: TStringStream):string; overload;
    /// <summary>
    ///
    ///</summary>
    /// <param name="ListID">
    /// Идентификатор списка
    /// </param>
    /// <param name="Parent">
    ///
    /// </param>
    /// <param name="Previous">
    ///
    /// </param>
    /// <param name="JSONStream">
    ///
    /// </param>
    function TasksMove(const ListID, TaskID, parentTaskID, previousTaskID:string):string;overload;
    function TasksMove(const TaskID, parentTaskID, previousTaskID:string):string;overload;

    function TasksUpdate(const ListID,TaskID:string; JSONStream: TStringStream):string;overload;
    function TasksUpdate(const TaskID:string; JSONStream: TStringStream):string;overload;

    function TasksDelete(const ListID,TaskID:string):boolean;overload;
    function TasksDelete(const TaskID:string):boolean;overload;

    property Version: string read GetVersion;
    published
    property OAuthClient: TOAuth read FOAuthClient write SetOAuthClient;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WebDelphi.ru',[TGTaskAPI]);
end;

{ TGTaskAPI }

constructor TGTaskAPI.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGTaskAPI.Destroy;
begin
  FOAuthClient.Free;
  inherited Destroy;
end;

function TGTaskAPI.GetVersion: string;
begin
  Result := APIVersion;
end;

function TGTaskAPI.ListsGet(const ListID: string): string;
begin
  Result := UTF8ToString(OAuthClient.GETCommand(Format(URI, [Version, 'users', DefaultUser,
    'lists', '/' + ListID]), nil));
end;

function TGTaskAPI.ListsInsert(JSONStream: TStringStream): string;
begin
  Result:=UTF8ToString(OAuthClient.POSTCommand(Format(URI,[Version,'users',DefaultUser,'lists','']),nil,JSONStream))
end;

function TGTaskAPI.ListsList(maxResults, pageToken: string): string;
var
  Params: TStrings;
  URL: string;
begin
  URL := Format(URI, [Version, 'users', DefaultUser, 'lists', '']);
  Params := TStringList.Create;
  try
    if Length(Trim(maxResults)) > 0 then
      Params.Add('maxResults=' + maxResults);
    if Length(Trim(pageToken)) > 0 then
      Params.Add('pageToken=' + pageToken);
    Result := UTF8ToString(OAuthClient.GETCommand(URL, Params));
  finally
    Params.Free;
  end;
end;

procedure TGTaskAPI.SetOAuthClient(const Value: TOAuth);
begin
  FOAuthClient := Value;
end;

function TGTaskAPI.TasksList(const ListID: string): string;
begin
  Result:=TasksList(ListID,nil)
end;

function TGTaskAPI.TasksGet(const ListID: string; TaskID: string): string;
begin
Result := UTF8ToString(OAuthClient.GETCommand(Format(URI, [Version, 'lists', ListID,
    'tasks', '/'+TaskID]), nil));
end;

function TGTaskAPI.TasksDelete(const ListID, TaskID: string): boolean;
begin
  try
  OAuthClient.DELETECommand(Format(URI, [Version, 'lists', ListID,
    'tasks', '/'+TaskID]));
    Result:=true;
  except
    Result:=false;
  end;
end;

function TGTaskAPI.TasksDelete(const TaskID: string): boolean;
begin
  Result:=TasksDelete(DefaultList,TaskID);
end;

function TGTaskAPI.TasksGet(const TaskID: string): string;
begin
  Result:=TasksGet(DefaultList,TaskID);
end;

function TGTaskAPI.TasksInsert(const JSONStream: TStringStream): string;
begin
  Result:=TasksInsert(DefaultList,JSONStream)
end;

function TGTaskAPI.TasksInsert(const ListID, Parent, Previous: string;
  JSONStream: TStringStream): string;
var Params:TStrings;
begin
  Params:=TStringList.Create;
  try
    if Length(Trim(Parent))>0 then
      Params.Values['parent']:=Parent;
    if Length(Trim(Previous))>0 then
      Params.Values['previous']:=Previous;
    Result:=UTF8ToString(OAuthClient.POSTCommand(Format(URI,[Version,'lists',ListId,'tasks','']),Params,JSONStream));
  finally
    Params.Free;
  end;
end;

function TGTaskAPI.TasksInsert(const ListID: string;
  JSONStream: TStringStream): string;
begin
  Result:=TasksInsert(ListID,'','',JSONStream);
end;

function TGTaskAPI.TasksList(const ListID: string; Params: TStrings): string;
begin
  Result := UTF8ToString(OAuthClient.GETCommand(Format(URI, [Version, 'lists', ListID,
    'tasks', '']), Params));
end;

function TGTaskAPI.TasksMove(const TaskID, parentTaskID,
  previousTaskID: string): string;
begin
  Result:=TasksMove(DefaultList,TaskID,parentTaskID,previousTaskID)
end;

function TGTaskAPI.TasksUpdate(const TaskID: string;
  JSONStream: TStringStream): string;
begin
  Result:=TasksUpdate(DefaultList,TaskID,JSONStream);
end;

function TGTaskAPI.TasksUpdate(const ListID, TaskID: string;JSONStream: TStringStream): string;
begin
  Result := UTF8ToString(OAuthClient.PUTCommand(Format(URI, [Version, 'lists', ListID,
    'tasks', '/'+TaskID]),JSONStream));
end;

function TGTaskAPI.TasksMove(const ListID, TaskID, parentTaskID,
  previousTaskID: string): string;
var Params: TStrings;
begin
Params:=TStringList.Create;
try
  if Length(Trim(parentTaskID))>0 then
    Params.Values['parent']:=parentTaskID;
  if Length(Trim(previousTaskID))>0 then
    Params.Values['previous']:=previousTaskID;
  Result:=UTF8ToString(OAuthClient.POSTCommand(Format(URI,[Version,'lists',ListID,'tasks',TaskID,'/move']),Params,nil));
finally
  Params.Free;
end;

end;

end.
