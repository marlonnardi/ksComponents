{*******************************************************************************
*                                                                              *
*  TksProgressBar - Progress Bar Component                                     *
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

unit ksProgressBar;

interface

{$I ksComponents.inc}

uses Classes, ksTypes, FMX.Graphics, System.UITypes, System.UIConsts;

type
  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64 {$ELSE} pidiOSDevice {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidiOSSimulator32 or pidiOSSimulator64 {$ELSE} pidiOSSimulator {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidAndroid32Arm or pidAndroid64Arm {$ELSE} pidAndroid {$ENDIF}
    )]

  TksProgressBar = class(TksControl)
  private
    FBackgroundColor: TAlphaColor;
    FBorderColor: TAlphaColor;

    FBarColor: TAlphaColor;

    FValue: integer;

    FMaxValue: integer;

    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetBarColor(const Value: TAlphaColor);
    procedure SetBorderColor(const Value: TAlphaColor);
    procedure SetMaxValue(const Value: integer);
    procedure SetValue(const Value: integer);
    function GetPercent: integer;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValues(APosition, AMax: integer);
  published
    property Align;
    property Height;
    property Width;
    property Size;
    property Margins;
    property Padding;
    property Position;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default claGainsboro;
    property BarColor: TAlphaColor read FBarColor write SetBarColor default claDodgerblue;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor default claBlack;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property Value: integer read FValue write SetValue default 50;
    property Percent: integer read GetPercent;
    property Visible;

  end;

  procedure Register;

implementation

uses FMX.Controls, Math, SysUtils, Types, FMX.Types, FMX.Ani, ksCommon, FMX.Forms;

const
  C_SCALE = 3;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksProgressBar]);
end;

{ TksProgressBar }

constructor TksProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FBorderColor := claBlack;;
  FBarColor := claDodgerblue;
  FValue := 50;
  FMaxValue := 100;
  Width := 100;
  Height := 20;
end;

destructor TksProgressBar.Destroy;
begin
  inherited;
end;

function TksProgressBar.GetPercent: integer;
begin
  Result := 0;
  if (FValue > 0) and (FMaxValue > 0) then
  begin
    Result := Round((FValue / FMaxValue) * 100);
  end;
end;

procedure TksProgressBar.Paint;
var
  AState: TCanvasSaveState;
  ABar: TRectF;
  APercent: single;
  v,m: integer;
begin
  inherited;
  if Locked then
    Exit;
  AState := Canvas.SaveState;
  try
    Canvas.BeginScene;
    Canvas.IntersectClipRect(ClipRect);

    // background...
    Canvas.Clear(FBackgroundColor);

    // bar...
    Canvas.Fill.Color := FBarColor;

    APercent := 0;
    v := FValue;
    m := FMaxValue;

    v := Min(v, m);
    if (v > 0) and (m > 0) then
    begin
      APercent := (v / m);
    end;

    ABar := RectF(0, 0, Width * APercent, Height);
    Canvas.FillRect(ABar, 0, 0, AllCorners, 1);


    // border...
    Canvas.Stroke.Thickness := 1;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := FBorderColor;
    Canvas.DrawRect(ClipRect, 0, 0, AllCorners, 1);
    Canvas.EndScene;
  finally
    canvas.RestoreState(AState);
  end;
end;

procedure TksProgressBar.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
  Repaint;
end;

procedure TksProgressBar.SetBarColor(const Value: TAlphaColor);
begin
  FBarColor := Value;
  Repaint;
end;

procedure TksProgressBar.SetBorderColor(const Value: TAlphaColor);
begin
  FBorderColor := Value;
  Repaint;
end;

procedure TksProgressBar.SetMaxValue(const Value: integer);
begin
  FMaxValue := Value;
  Repaint;
end;

procedure TksProgressBar.SetValue(const Value: integer);
begin
  FValue := Value;
  Repaint;
end;

procedure TksProgressBar.SetValues(APosition, AMax: integer);
begin
  FMaxValue := AMax;
  FValue := APosition;
  Repaint;
end;

end.
