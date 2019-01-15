unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,INIFiles,

  Data.Bind.EngExt,
  Vcl.Bind.DBEngExt,
  Data.Bind.Components, Vcl.StdCtrls;


type
  TBlogInfo = class
  private
    fName: string;
    fURL: string;
    fSubscribers: integer;
    procedure SetName(const Value: string);
    procedure SetSubscribers(const Value: integer);
    procedure SetURL(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName write SetName;
    property URL: string read FURL write SetURL;
    property Subscribers: integer read FSubscribers write SetSubscribers;
end;

type
  TForm13 = class(TForm)
    BindingsList1: TBindingsList;
    BindScope1: TBindScope;
    Label1: TLabel;
    edName: TEdit;
    Label2: TLabel;
    edAddress: TEdit;
    Label3: TLabel;
    edSubscribers: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    BlogInfo: TBlogInfo;
    procedure InitializeBinding(ABlogInfo: TBlogInfo);
    procedure CreateExpression(AControlComponent: TComponent;
                               AControlProperty, ASourceProperty: string;
                               ABindScope: TBindScope;
                               ABindingsList: TBindingsList);
  public
    
  end;

var
  Form13: TForm13;

implementation

{$R *.dfm}

{ TBlogInfo }

constructor TBlogInfo.Create;
begin
  inherited Create;
  fName:='WebDelphi';
  fURL:='http://www.webdelphi.ru';
  fSubscribers:=355;
end;

destructor TBlogInfo.Destroy;
begin
  inherited;
end;

procedure TBlogInfo.SetName(const Value: string);
begin
  FName := Value;
  ShowMessage('Новое название: '+Value);
end;

procedure TBlogInfo.SetSubscribers(const Value: integer);
begin
  FSubscribers := Value;
  ShowMessage('Новое количество подписчиков: '+IntToStr(Value));
end;

procedure TBlogInfo.SetURL(const Value: string);
begin
  FURL := Value;
  ShowMessage('Новый URL: '+Value);
end;

{ TForm13 }

procedure TForm13.Button1Click(Sender: TObject);
begin
  BindScope1.DataObject := BlogInfo;
end;

procedure TForm13.Button2Click(Sender: TObject);
var F: TIniFile;
begin
  BindingsList1.Notify(edName,'');
  BindingsList1.Notify(edAddress,'');
  BindingsList1.Notify(edSubscribers,'');
  F:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'Object.ini');
  try
    F.WriteString('Object','Name',BlogInfo.Name);
    F.WriteString('Object','URL',BlogInfo.URL);
    F.WriteInteger('Object','Subscribers',BlogInfo.Subscribers);
  finally
    F.Free
  end;
end;

procedure TForm13.CreateExpression(AControlComponent: TComponent;
  AControlProperty, ASourceProperty: string; ABindScope: TBindScope;
  ABindingsList: TBindingsList);
var Expression: TBindExpression;
begin
  {создаем выражение}
  Expression := TBindExpression.Create(self);
  {назначаем компонент для отображения информации}
  Expression.ControlComponent := AControlComponent;
  {Указываем в какое свойство выводить данные}
  Expression.ControlExpression := AControlProperty;
  {указываем точку доступа - тут должен содержаться наш класс}
  Expression.SourceComponent := ABindScope;
  {Тут указываем свойство КЛАССА, которое будет выводится в AControlProperty}
  Expression.SourceExpression := ASourceProperty;
  {Обеспечиваем двухстороннюю связь - будем и читать и писать свойство}
  Expression.Direction := TExpressionDirection.dirBidirectional;
  {Назначаем список}
  expression.BindingsList := ABindingsList;
end;

procedure TForm13.FormCreate(Sender: TObject);
begin
 BlogInfo:=TBlogInfo.Create;
 InitializeBinding(BlogInfo);
end;

procedure TForm13.FormDestroy(Sender: TObject);
begin
 BlogInfo.Free;
end;

procedure TForm13.InitializeBinding(ABlogInfo: TBlogInfo);
begin
  CreateExpression(edName,'Text','Name',BindScope1,BindingsList1);
  CreateExpression(edAddress,'Text','URL',BindScope1,BindingsList1);
  CreateExpression(edSubscribers,'Text','Subscribers',BindScope1,BindingsList1);
  BindScope1.DataObject:=ABlogInfo;
end;

initialization
  ReportMemoryLeaksOnShutdown:=true;


end.
