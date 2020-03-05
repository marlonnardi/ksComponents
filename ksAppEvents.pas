{*******************************************************************************
*                                                                              *
*  TksAppEvents - Application Events Component                                 *
*                                                                              *
*  https://bitbucket.org/gmurt/kscomponents                                    *
*                                                                              *
*  Copyright 2017 Graham Murt                                                  *
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

unit ksAppEvents;

interface

{$I ksComponents.inc}

uses
  FMX.Types, FMX.Platform, Classes;

type
  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64 {$ELSE} pidiOSDevice {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidiOSSimulator32 or pidiOSSimulator64 {$ELSE} pidiOSSimulator {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidAndroid32Arm or pidAndroid64Arm {$ELSE} pidAndroid {$ENDIF}
    )]

  TksAppEvents = class(TComponent)
  private
    FFMXApplicationEventService: IFMXApplicationEventService;
    FFinishedLaunching: TNotifyEvent;
    FBecameActive: TNotifyEvent;
    FEnteredBackground: TNotifyEvent;
    FWillBecomeForeground: TNotifyEvent;
    FWillTerminate: TNotifyEvent;
    FLowMemory: TNotifyEvent;
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  public
    constructor Create(AOwner: TComponent); override;

  published
    property OnFinishedLaunching: TNotifyEvent read FFinishedLaunching write FFinishedLaunching;
    property OnBecameActive: TNotifyEvent read FBecameActive write FBecameActive;
    property OnEnteredBackground: TNotifyEvent read FEnteredBackground write FEnteredBackground;
    property WillBecomeForeground: TNotifyEvent read FWillBecomeForeground write FWillBecomeForeground;
    property WillTerminate: TNotifyEvent read FWillTerminate write FWillTerminate;
    property OnLowMemory: TNotifyEvent read FLowMemory write FLowMemory;
  end;

  //{$R *.dcr}

  procedure Register;

implementation

uses SysUtils, ksCommon, System.TypInfo;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksAppEvents]);
end;

{ TksSlideMenuAppearence }

constructor TksAppEvents.Create(AOwner: TComponent);
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(FFMXApplicationEventService)) then
    FFMXApplicationEventService.SetApplicationEventHandler(HandleAppEvent);
end;

function TksAppEvents.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.FinishedLaunching:    if Assigned(FFinishedLaunching) then FFinishedLaunching(Self);
    TApplicationEvent.BecameActive:         if Assigned(FBecameActive) then FBecameActive(Self);
    TApplicationEvent.EnteredBackground:    if Assigned(FEnteredBackground) then FEnteredBackground(Self);
    TApplicationEvent.WillBecomeForeground: if Assigned(FWillBecomeForeground) then FWillBecomeForeground(Self);
    TApplicationEvent.WillBecomeInactive:   if Assigned(FFinishedLaunching) then FFinishedLaunching(Self);
    TApplicationEvent.WillTerminate:        if Assigned(FWillTerminate) then FWillTerminate(Self);
    TApplicationEvent.LowMemory:            if Assigned(FLowMemory) then FLowMemory(Self);
  end;
  Result := True;
end;

end.

