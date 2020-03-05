{*******************************************************************************
*                                                                              *
*  TksPushNotification - Push Notification Component                           *
*                                                                              *
*  https://github.com/gmurt/KernowSoftwareFMX                                  *
*                                                                              *
*  Copyright 2015 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksPushNotification;

interface

{$I ksComponents.inc}

uses Classes, FMX.MediaLibrary, FMX.Media, FMX.Platform, System.Messaging, FMX.Graphics,
  ksTypes, System.PushNotification, Json, FMX.Types,
  {$IFDEF VER290}
  FMX.Notification
  {$ELSE}
  System.Notification
  {$ENDIF}
  ;

type
  TksReceivePushTokenEvent = procedure(Sender: TObject; AToken: string) of object;
  TksReceivePushMessageEvent = procedure(Sender: TObject; AData: TJsonObject) of object;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64
    {$ELSE} pidiOSDevice {$ENDIF} or pidiOSSimulator or pidAndroid)]
  TksPushNotification = class(TComponent)
  private
    FAppEvent: IFMXApplicationEventService;
    FFirebaseSenderID: string;
  //  FTimer: TFmxHandle;
    FServiceConnection: TPushServiceConnection;
    FPushService: TPushService;
    FDeviceToken: string;
    FNotificationCenter: TNotificationCenter;
  //  FTimerService: IFMXTimerService;
    // events...
    FOnReceiveTokenEvent: TksReceivePushTokenEvent;
    FOnReceivePushMessageEvent: TksReceivePushMessageEvent;
    //function CreateTimer(AInterval: integer; AProc: TTimerProc): TFmxHandle;
   // procedure DoCheckStartupNotifications;
    function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure OnNotificationChange(Sender: TObject; AChange: TPushService.TChanges);
    procedure OnReceiveNotificationEvent(Sender: TObject; const ANotification: TPushServiceNotification);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate;
    procedure ClearNotifications;
  published
    property FirebaseSenderID: string read FFirebaseSenderID write FFirebaseSenderID stored True;
    property OnReceiveToken: TksReceivePushTokenEvent read FOnReceiveTokenEvent write FOnReceiveTokenEvent;
    property OnReceivePushMessageEvent: TksReceivePushMessageEvent read FOnReceivePushMessageEvent write FOnReceivePushMessageEvent;
  end;

  procedure Register;

implementation

uses Types, SysUtils, System.Threading, ksCommon, {$IFDEF VER290} FMX.Dialogs {$ELSE} FMX.DialogService {$ENDIF}

  {$IFDEF IOS}
  , FMX.PushNotification.IOS
  {$ENDIF}

  {$IFDEF ANDROID}
  , FMX.PushNotification.Android,
  FMX.Platform.Android,
  Androidapi.Helpers,
  FMX.Helpers.Android,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Telephony
  {$ENDIF}
;



procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksPushNotification]);
end;

{ TksCamara }

procedure TksPushNotification.Activate;
{$IFDEF ANDROID}
var
  ATask: ITask;
{$ENDIF}
begin
  {$IFDEF IOS}
  FServiceConnection.Active := True;
  {$ENDIF}
  {$IFDEF ANDROID}
  ATask := TTask.Create (procedure ()
     begin
        try
          FServiceConnection.Active := True;
        except
          on E:Exception do
          begin
            TThread.Synchronize(nil,
            procedure
            begin
              TDialogService.ShowMessage(e.Message);
            end);
          end;
        end;
     end);
   ATask.Start;
  {$ENDIF}
end;

function TksPushNotification.AppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
var
  ICount: integer;
  ANotification: TPushServiceNotification;
begin
  Result := True;
  if (AAppEvent = TApplicationEvent.BecameActive) or
     (AAppEvent = TApplicationEvent.FinishedLaunching) then
  begin
    for ICount := Low(FPushService.StartupNotifications) to High(FPushService.StartupNotifications) do
    begin
      ANotification := FPushService.StartupNotifications[ICount];
      OnReceiveNotificationEvent(Self, ANotification);
    end;
    FNotificationCenter.CancelAll;
  end;
end;

procedure TksPushNotification.ClearNotifications;
begin
  FNotificationCenter.CancelAll;
end;

constructor TksPushNotification.Create(AOwner: TComponent);
begin
  inherited;
  TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(FAppEvent));
  if FAppEvent <> nil then
    FAppEvent.SetApplicationEventHandler(AppEvent);

  //TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService);
  //CreateTimer(500, DoCheckStartupNotifications);

  FNotificationCenter := TNotificationCenter.Create(nil);
end;
              {
function TksPushNotification.CreateTimer(AInterval: integer; AProc: TTimerProc): TFmxHandle;
begin
  Result := 0;
  if FTimerService <> nil then
    Result := FTimerService.CreateTimer(AInterval, AProc);
end;       }

destructor TksPushNotification.Destroy;
begin
  FreeAndNil(FNotificationCenter);
  inherited;
end;
            {
procedure TksPushNotification.DoCheckStartupNotifications;
var
  ICount: integer;
  ANotification: TPushServiceNotification;
begin
  for ICount := Low(FPushService.StartupNotifications) to High(FPushService.StartupNotifications) do
  begin
    ANotification := FPushService.StartupNotifications[ICount];
    OnReceiveNotificationEvent(Self, ANotification);
  end;
  FNotificationCenter.CancelAll;
end;
             }
procedure TksPushNotification.Loaded;
var
  AServiceName: string;
begin
  inherited;
  {$IFDEF IOS}
  AServiceName := TPushService.TServiceNames.APS;
  {$ENDIF}
  {$IFDEF ANDROID}
  AServiceName := TPushService.TServiceNames.GCM;
  {$ENDIF}
  FPushService := TPushServiceManager.Instance.GetServiceByName(AServiceName);
  {$IFDEF ANDROID}
  FPushService.AppProps[TPushService.TAppPropNames.GCMAppID] := FFirebaseSenderID;
  {$ENDIF}
  FServiceConnection := TPushServiceConnection.Create(FPushService);
  FServiceConnection.OnChange := OnNotificationChange;
  FServiceConnection.OnReceiveNotification := OnReceiveNotificationEvent;
end;

procedure TksPushNotification.OnNotificationChange(Sender: TObject; AChange: TPushService.TChanges);
var
  AToken: string;
begin
  if (TPushService.TChange.Status in AChange) then
  begin


    if (FPushService.Status = TPushService.TStatus.StartupError) then
    begin
      FServiceConnection.Active := False;
      TThread.Synchronize(nil,
        procedure
        begin
          if Pos('java.lang.securityexception', LowerCase(FPushService.StartupError)) > 0 then
            ksCommon.ShowMessage('Unable to activate push notifications...'+#13+#13+
                                       'Check that you have enabled "Receive Push Notifications" in Options->Entitlement List')
          else
          if Pos('invalid_sender', LowerCase(FPushService.StartupError)) > 0 then
            ShowMessage('Unable to activate push notifications...'+#13+#13+
                                       'The FirebaseSenderID value is invalid.')
          else
            ShowMessage(FPushService.StartupError);
        end);
      Exit;
    end;
  end;

  AToken := Trim(FPushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken]);
  if AToken <> '' then
  begin
    FDeviceToken := AToken;
    TThread.Synchronize(nil,
      procedure
      begin
        if Assigned(FOnReceiveTokenEvent) then
          FOnReceiveTokenEvent(Self, FDeviceToken);
      end);
  end;
end;


procedure TksPushNotification.OnReceiveNotificationEvent(Sender: TObject;
  const ANotification: TPushServiceNotification);
var
  AJson: TJsonObject;
begin
  if Assigned(FOnReceivePushMessageEvent) then
  begin
    AJson := ANotification.Json;
    FOnReceivePushMessageEvent(Self,
                               AJson);
  end;
end;

end.

