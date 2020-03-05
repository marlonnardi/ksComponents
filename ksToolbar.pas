{*******************************************************************************
*                                                                              *
*  TkToolbar - Toolbar with form stack/transitions awareness                   *
*                                                                              *
*  https://github.com/gmurt/KernowSoftwareFMX                                  *
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

unit ksToolBar;

interface

{$I ksComponents.inc}


uses Classes, FMX.StdCtrls, FMX.Graphics, ksTypes, FMX.Objects, FMX.Types,
  System.UITypes, System.UIConsts, ksSpeedButton, ksFormTransition,
  FMX.Controls.Presentation, FMX.Controls, System.Types;

type
 // TksToolbar = class;

  {IksToolbar = interface
  ['{42609FB8-4DE0-472F-B49C-A6CD636A530D}//]
    //procedure SetTransition(ATransition: TksTransitionType);
  //end;

  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64 {$ELSE} pidiOSDevice {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidiOSSimulator32 or pidiOSSimulator64 {$ELSE} pidiOSSimulator {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidAndroid32Arm or pidAndroid64Arm {$ELSE} pidAndroid {$ENDIF}
    )]
  TksToolbar = class(TPresentedControl)
  private
    FButton: TSpeedButton;
    //FButtonInitialized: Boolean;
    FTintColor: TAlphaColor;
    FFont: TFont;
    FTextColor: TAlphaColor;
    FButtonColor: TAlphaColor;
    FText: string;
    FBackButtonText: string;
    FFormTransition: TksFormTransition;
    FOnMenuButtonClick: TNotifyEvent;
    FShowMenuButton: Boolean;
    FOnBackButtonClick: TNotifyEvent;
    FBackButtonEnabled: Boolean;
    FShowBackButton: Boolean;
    FFirstPaint: Boolean;
    procedure Changed(Sender: TObject);
    procedure ButtonClicked(Sender: TObject);

    procedure SetShowMenuButton(const Value: Boolean);
    procedure SetTintColor(const Value: TAlphaColor);
    procedure SetTextColor(const Value: TAlphaColor);
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetShowBackButton(const Value: Boolean);
    procedure SetButtonColor(const Value: TAlphaColor);
    procedure UpdateButton;
    procedure SetBackButtonText(const Value: string);
  protected
    procedure Paint; override;
    function GetDefaultSize: TSizeF; override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisableBackButton;
    procedure EnableBackButton;
  published
    property Font: TFont read FFont write SetFont;
    property Text: string read FText write SetText;
    property Size;
    property TabOrder;

    property BackButtonText: string read FBackButtonText write SetBackButtonText;
    property TintColor: TAlphaColor read FTintColor write SetTintColor default claWhitesmoke;
    property TextColor: TAlphaColor read FTextColor write SetTextColor default claBlack;
    property ButtonColor: TAlphaColor read FButtonColor write SetButtonColor default claDodgerblue;
    property ShowMenuButton: Boolean read FShowMenuButton write SetShowMenuButton default True;
    property ShowBackButton: Boolean read FShowBackButton write SetShowBackButton default True;


    property OnClick;
    property OnMenuButtonClick: TNotifyEvent read FOnMenuButtonClick write FOnMenuButtonClick;
    property OnBackButtonClick: TNotifyEvent read FOnBackButtonClick write FOnBackButtonClick;
  end;


  procedure Register;

implementation

uses Math, System.TypInfo, ksCommon, SysUtils, ksPickers,
  Fmx.Forms, FMX.Platform;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksToolbar]);
end;


{ TksToolbar }

procedure TksToolbar.ButtonClicked(Sender: TObject);
begin
  PickerService.HidePickers;
  HideKeyboard;
  Application.ProcessMessages;


  if FFormTransition.GetFormDepth(Root as TCommonCustomForm) = 0 then
  begin
    if (Assigned(FOnMenuButtonClick)) and (FShowMenuButton) then
      FOnMenuButtonClick(Self);
  end
  else
  begin
    if (Assigned(FOnBackButtonClick))and (FShowBackButton)  then
    begin
      FOnBackButtonClick(Self);
    	FFormTransition.Pop;
    end
    else
    if (FShowBackButton)  then
    begin
    	FFormTransition.Pop;
    end;
  end;
end;


procedure TksToolbar.Changed(Sender: TObject);
begin
  InvalidateRect(ClipRect);
end;

constructor TksToolbar.Create(AOwner: TComponent);
begin
  inherited;
  FFirstPaint := True;
  FButton := TSpeedButton.Create(Self);
  FButton.Align := TAlignLayout.Left;
  FButton.StyleLookup := 'detailstoolbutton';
  FButton.Width := 44;
  FButton.TouchTargetExpansion.Rect := Rect(4, 4, 4, 4);
  FButtonColor := claDodgerblue;
  FButton.IconTintColor := claDodgerblue;
  FButton.TintColor := FButtonColor;
  FButton.CanFocus := True;
  FButton.Stored := False;
  FButton.StyledSettings := [TStyledSetting.Family,TStyledSetting.Style,TStyledSetting.FontColor];
  //FButton.Visible := False;
  FButton.OnClick := ButtonClicked;
  AddObject(FButton);

  FFont := TFont.Create;
  FFont.Size := 14;
  Align := TAlignLayout.MostTop;
  FFormTransition := TksFormTransition.Create(nil);

  FTintColor := claWhitesmoke;
  FTextColor := claBlack;

  FFont.OnChanged := Changed;

  FShowMenuButton := True;
  FShowBackButton := True;
  FBackButtonEnabled := True;

end;

destructor TksToolbar.Destroy;
begin
  FreeAndNil(FFormTransition);
  FreeAndNil(FFont);
  inherited;
end;

procedure TksToolbar.DisableBackButton;
begin
  FBackButtonEnabled := False;
  UpdateButton;
end;

procedure TksToolbar.DoMouseLeave;
begin
  inherited;
end;

procedure TksToolbar.EnableBackButton;
begin
  FBackButtonEnabled := True;
  UpdateButton;
end;

function TksToolbar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(120, 44);
end;

procedure TksToolbar.Paint;
var
  AState: TCanvasSaveState;
begin
  inherited;
  if FFirstPaint then
  begin
    FFirstPaint := False;
    UpdateButton;
  end;

  AState := Canvas.SaveState;
  try
    Canvas.IntersectClipRect(ClipRect);
    Canvas.Fill.Color := FTintColor;
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.FillRect(ClipRect, 0, 0, AllCorners, 1);


    Canvas.Font.Assign(FFont);
    Canvas.Fill.Color := FTextColor;
    Canvas.FillText(ClipRect, FText, False, 1, [], TTextAlign.Center);

    Canvas.Stroke.Thickness := 1;
    Canvas.Stroke.Color := claDimgray;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.DrawLine(PointF(0, Height), PointF(Width, Height), 1);
    FButton.Repaint;
  finally
    Canvas.RestoreState(AState);
  end;
end;

procedure TksToolbar.SetBackButtonText(const Value: string);
begin
  if FBackButtonText <> Value then
  begin
    FBackButtonText := Value;
    UpdateButton;
  end;
end;

procedure TksToolbar.SetButtonColor(const Value: TAlphaColor);
begin
  FButtonColor := Value;
  UpdateButton;
end;

procedure TksToolbar.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TksToolbar.SetShowBackButton(const Value: Boolean);
begin
  FShowBackButton := Value;
  UpdateButton
end;

procedure TksToolbar.SetShowMenuButton(const Value: Boolean);
begin
  FShowMenuButton := Value;
  UpdateButton;
end;

procedure TksToolbar.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    InvalidateRect(ClipRect);
  end;
end;

procedure TksToolbar.SetTextColor(const Value: TAlphaColor);
begin
  FTextColor := Value;
  UpdateButton;
end;

procedure TksToolbar.SetTintColor(const Value: TAlphaColor);
begin
  if FTintColor <> Value then
  begin
    FTintColor := Value;
    UpdateButton;
  end;
end;

procedure TksToolbar.UpdateButton;
begin
  if Root is TCommonCustomForm then
  begin
    if (FFormTransition.GetFormDepth(Root as TCommonCustomForm) = 0) then
    begin
      FButton.Visible := FShowMenuButton;
      if FButton.StyleLookup <> 'detailstoolbutton' then
        FButton.StyleLookup := 'detailstoolbutton';
    end
    else
    begin
      FButton.Visible := FShowBackButton;
      if FButton.StyleLookup <> 'arrowlefttoolbutton' then
        FButton.StyleLookup := 'arrowlefttoolbutton';
      if FBackButtonText <> '' then
      begin
        FButton.StyleLookup := '';
        FButton.Text := FBackButtonText;
        FButton.Width := 60;
        FButton.Font.Size := 14;
      end;
    end;
  end;
end;

initialization

  Classes.RegisterClass(TksToolbar);

end.
