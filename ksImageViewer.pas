{*******************************************************************************
*                                                                              *
*  TksImageViewer - Enhanced image viewer component                            *
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

unit ksImageViewer;

interface

{$I ksComponents.inc}

uses Classes, Types, ksTypes, FMX.InertialMovement, System.UITypes, FMX.Graphics,
  FMX.Layouts, FMX.Types;

//const
//  C_BORDER = 20;

type
  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64 {$ELSE} pidiOSDevice {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidiOSSimulator32 or pidiOSSimulator64 {$ELSE} pidiOSSimulator {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidAndroid32Arm or pidAndroid64Arm {$ELSE} pidAndroid {$ENDIF}
    )]

  TksImageViewer = class(TksControl)
  private
    FAniCalc: TksAniCalc;
    FBitmap: TBitmap;
    FZoom: single;

    FStartZoom: single;
    FStartDistance: single;
    FOnZoom: TNotifyEvent;
    FMaxXPos: single;
    FMaxYPos: single;
    FZooming: Boolean;
    procedure AniCalcStart(Sender: TObject);
    procedure AniCalcStop(Sender: TObject);
    procedure AniCalcChange(Sender: TObject);

    procedure SetBitmap(const Value: TBitmap);
    procedure UpdateScrollLimits;
    procedure SetZoom(const Value: single);
  protected
    procedure DblClick; override;
    procedure DoMouseLeave; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoDoubleTap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DownloadFile(AUrl: string);
    function GetFitHeightWidthZoom: integer;
    procedure FitHeight;
    procedure FitWidth;
    procedure FitHeightAndWidth;
  published
    property Align;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Zoom: single read FZoom write SetZoom;
    property Position;
    property Padding;
    property Margins;
    property Size;
    property Width;
    property Height;
    property Visible;
    property Touch;
    property OnGesture;
    property OnDblClick;
    property OnZoom: TNotifyEvent read FOnZoom write FOnZoom;
  end;

  procedure Register;

implementation

uses System.UIConsts, SysUtils, Math, FMX.Controls, System.Net.HttpClientComponent,
  ksCommon;


procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksImageViewer]);
end;


{ TksImageViewer }

procedure TksImageViewer.AniCalcStart(Sender: TObject);
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(Self, True);
end;

procedure TksImageViewer.AniCalcStop(Sender: TObject);
begin
  if Scene <> nil then
    Scene.ChangeScrollingState(nil, False);
end;


constructor TksImageViewer.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TBitmap.Create;
  Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan];
  FAniCalc := TksAniCalc.Create(nil);
  FAniCalc.OnChanged := AniCalcChange;
  FAniCalc.ViewportPositionF := PointF(0, 0);
  FAniCalc.Animation := True;
  FAniCalc.Averaging := True;
  FAniCalc.Interval := 8;
  FAniCalc.BoundsAnimation := True;
  FAniCalc.TouchTracking := [ttHorizontal, ttVertical];
  FAniCalc.OnChanged := AniCalcChange;
  FAniCalc.OnStart := AniCalcStart;
  FAniCalc.OnStop := AniCalcStop;
  Width := 100;
  Height := 100;
  FZoom := 100;
  FMaxXPos := 0;
  FMaxYPos := 0;
  FZooming := False;
  Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan];
  //FTouchCount := 0;
end;

procedure TksImageViewer.DblClick;
begin
  inherited;

end;

destructor TksImageViewer.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FAniCalc);
  inherited;
end;

procedure TksImageViewer.DoDoubleTap;
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TksImageViewer.DoMouseLeave;
begin
  inherited;
  if (FAniCalc <> nil) then
    FAniCalc.MouseLeave;
  //FTouchCount := 0;
  FStartDistance := 0;
  FStartZoom := 0;
end;

procedure TksImageViewer.DownloadFile(AUrl: string);
var
  AHttp: TNetHTTPClient;
  AStream: TStream;
  ABmp: TBitmap;
begin
  AHttp := TNetHTTPClient.Create(nil);
  ABmp := TBitmap.Create;
  try
    AStream := AHttp.Get(AUrl).ContentStream;
    AStream.Position := 0;
    ABmp.LoadFromStream(AStream);
    Bitmap := ABmp;
    FitHeightAndWidth;
  finally
    AHttp.DisposeOf;
    FreeAndNil(ABmp);
  end;
  InvalidateRect(ClipRect);
end;


procedure TksImageViewer.FitHeight;
begin
  Zoom := (Height / FBitmap.Height) * 100;
end;

procedure TksImageViewer.FitHeightAndWidth;
begin
  Zoom := GetFitHeightWidthZoom;
end;

procedure TksImageViewer.FitWidth;
begin
  Zoom := (Width / FBitmap.Width) * 100;
end;

function TksImageViewer.GetFitHeightWidthZoom: integer;
var
  z1, z2: single;
begin
  z1 := (Height / FBitmap.Height) * 100;
  z2 := (Width / FBitmap.Width) * 100;
  Result := Trunc(Min(z1, z2));
end;

procedure TksImageViewer.CMGesture(var EventInfo: TGestureEventInfo);
{$IFDEF IOS}
var
  ADistance: integer;
  ANewZoom: single;
{$ENDIF}
begin
  inherited;
  {$IFDEF IOS}
  if EventInfo.GestureID = igiDoubleTap then
    DoDoubleTap;

  if EventInfo.GestureID = igiZoom then
  begin
    if TInteractiveGestureFlag.gfEnd in EventInfo.Flags then
    begin
      FZooming := False;
      FStartDistance := 0;
      Exit;
    end;
    //if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
    if FStartDistance = 0 then
    begin
      FStartDistance := Round(EventInfo.Distance);//EventInfo.Distance;
      FStartZoom := FZoom;
      FZooming := True;
      Exit;
    end;
    ADistance := Round(EventInfo.Distance-FStartDistance) * Round(GetScreenScale(True));
    //if FStartZoom + Round(EventInfo.Distance-FStartDistance) > 10 then

    ANewZoom := FStartZoom + (ADistance / 10);
    ANewZoom := Min(ANewZoom, 200);
    ANewZoom := Max(ANewZoom, 10);
    Zoom := ANewZoom;
  end;
  {$ENDIF}
end;



procedure TksImageViewer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  FStartDistance := 0;
  FAniCalc.MouseDown(x, y);
end;

procedure TksImageViewer.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FAniCalc.MouseMove(x, y);
end;

procedure TksImageViewer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  FZooming := False;
  FAniCalc.MouseUp(x, y);
  FStartDistance := 0;
end;

procedure TksImageViewer.Paint;
var
  ADestRect: TRectF;
  ASaveState: TCanvasSaveState;
  AScale: single;
begin
  inherited;
  if Locked then
    Exit;
  ASaveState := Canvas.SaveState;
  try
    Canvas.IntersectClipRect(RectF(0, 0, Width, Height));
    Canvas.Clear(claLightgray);

    AScale := FZoom / 100;
    ADestRect := RectF(0,0, (FBitmap.Width * AScale), (FBitmap.Height * AScale));

    OffsetRect(ADestRect, 0-(FAniCalc.ViewportPosition.X), 0-(FAniCalc.ViewportPosition.Y));

    if ADestRect.Width < Width then
      OffsetRect(ADestRect, (Width-ADestRect.Width) / 2, 0);

    if ADestRect.Height < Height then
      OffsetRect(ADestRect, 0, (Height-ADestRect.Height) / 2);

    InflateRect(ADestRect, -10, -10);
    Canvas.DrawBitmap(FBitmap,
                      RectF(0, 0, FBitmap.Width, FBitmap.Height),
                      ADestRect,
                      1,
                      True);
  finally
    Canvas.RestoreState(ASaveState);
  end;
end;

procedure TksImageViewer.Resize;
begin
  inherited;
  UpdateScrollLimits;
end;

procedure TksImageViewer.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  UpdateScrollLimits;
  Repaint;
end;

procedure TksImageViewer.SetZoom(const Value: single);
var
  xpercent, ypercent: single;
  ANewX, ANewY: single;
begin

  if FZoom <> Value then
  begin
    if Value < GetFitHeightWidthZoom then
      Exit;
    xPercent := 0;
    yPercent := 0;
    if (FAniCalc.ViewportPosition.X > 0) and (FMaxXPos > 0) then xPercent := (FAniCalc.ViewportPosition.X / FMaxXPos) * 100;
    if (FAniCalc.ViewportPosition.Y > 0) and (FMaxYPos > 0) then yPercent := (FAniCalc.ViewportPosition.Y / FMaxYPos) * 100;
    FZoom := Value;
    UpdateScrollLimits;

    ANewX := 0;
    ANewY := 0;
    if (FMaxXPos > 0) and (xpercent > 0) then ANewX := (FMaxXPos/100) * xpercent;
    if (FMaxYPos > 0) and (ypercent > 0) then ANewY := (FMaxYPos/100) * ypercent;

    FAniCalc.ViewportPositionF := PointF(ANewX, ANewY);

    InvalidateRect(ClipRect);
    if Assigned(FOnZoom) then
      FOnZoom(Self);
  end;
end;

procedure TksImageViewer.AniCalcChange(Sender: TObject);
begin
  InvalidateRect(ClipRect);
end;

procedure TksImageViewer.UpdateScrollLimits;
var
  Targets: array of TAniCalculations.TTarget;
  AScale: single;
begin
  if FAniCalc <> nil then
  begin
    AScale := FZoom / 100;

    SetLength(Targets, 2);
    Targets[0].TargetType := TAniCalculations.TTargetType.Min;
    Targets[0].Point := TPointD.Create(0, 0);

    Targets[1].TargetType := TAniCalculations.TTargetType.Max;
    Targets[1].Point := TPointD.Create(Max(0,((FBitmap.Width*AScale))-Width),
                                       Max(0, ((FBitmap.Height*AScale))-Height));
    FAniCalc.SetTargets(Targets);

    FMaxXPos := Targets[1].Point.X;
    FMaxYPos := Targets[1].Point.Y;


  end;
end;

end.
