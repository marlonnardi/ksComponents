unit ksLoadingIndicator;

interface

{$I ksComponents.inc}


uses FMX.Forms, Classes, FMX.Controls, FMX.Objects, ksTypes, FMX.Graphics,
  FMX.StdCtrls, FMX.Layouts
  {$IFDEF IOS}
  , iOSapi.UIKit, iOSapi.Foundation
  {$ENDIF}
  ;

type
  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64 {$ELSE} pidiOSDevice {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidiOSSimulator32 or pidiOSSimulator64 {$ELSE} pidiOSSimulator {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidAndroid32Arm or pidAndroid64Arm {$ELSE} pidAndroid {$ENDIF}
    )]
  TksLoadingIndicator = class(TLayout)
  private
    FRectangle: TRectangle;
    FBackground: TRectangle;
    FLoadingText: string;
    FFadeBackground: Boolean;
    FIsModal: Boolean;
    //FLabel: TLabel;
    FOpacity: single;
    FAnimated: Boolean;
    FAnimator: TAniIndicator;
    procedure SetIsModal(const Value: Boolean);
    procedure SetFadeBackground(const Value: Boolean);
    procedure SetOpacity(const Value: single);
    procedure SetAnimated(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowLoading;
    procedure HideLoading;
  published
    property Animated: Boolean read FAnimated write SetAnimated;
    property IsModal: Boolean read FIsModal write SetIsModal default False;
    property LoadingText: string read FLoadingText write FLoadingText;
    property FadeBackground: Boolean read FFadeBackground write SetFadeBackground default False;
    property Opacity: single read FOpacity write SetOpacity;
  end;


  procedure ShowLoadingIndicator(AForm: TCommonCustomForm;
                                 AOpacity: single); overload;
  procedure ShowLoadingIndicator(AForm: TCommonCustomForm;
                                 const AFade: Boolean = False;
                                 const AModal: Boolean = False;
                                 const AOpacity: single = 1); overload;
  procedure HideLoadingIndicator(AForm: TCommonCustomForm);
  function IsLoadingIndicatorVisible(AForm: TCommonCustomForm): Boolean;
  function FindLoadingIndicator(AForm: TCommonCustomForm): TksLoadingIndicator;



implementation

uses
  System.UIConsts, FMX.Types, SysUtils, Types, FMX.Ani
  {$IFDEF IOS}
  ,iOSapi.CoreGraphics, FMX.Helpers.iOS
  {$ENDIF}
  ;



function FindLoadingIndicator(AForm: TCommonCustomForm): TksLoadingIndicator;
var
  ICount: integer;
begin
  Result := nil;
  if AForm = nil then
    Exit;
  for ICount := AForm.ComponentCount-1 downto 0 do
  begin
    if AForm.Components[ICount] is TksLoadingIndicator then
    begin
      Result := (AForm.Components[ICount] as TksLoadingIndicator);
      Exit;
    end;
  end;
end;

function IsLoadingIndicatorVisible(AForm: TCommonCustomForm): Boolean;
var
  ALoading: TksLoadingIndicator;
begin
  Result := False;
  ALoading := FindLoadingIndicator(AForm);
  if ALoading <> nil then
  begin
    if ALoading.Parent = AForm then
      Result := True;
  end;
end;

procedure ShowLoadingIndicator(AForm: TCommonCustomForm;
                               AOpacity: single); overload;
begin
  ShowLoadingIndicator(AForm, False, False, AOpacity);
end;

procedure ShowLoadingIndicator(AForm: TCommonCustomForm;
                               const AFade: Boolean = False;
                               const AModal: Boolean = False;
                               const AOpacity: single = 1);
var
  ALoadingIndicator: TksLoadingIndicator;
begin
  Application.ProcessMessages;
  try
    ALoadingIndicator := FindLoadingIndicator(AForm);
    if ALoadingIndicator = nil then
      ALoadingIndicator := TksLoadingIndicator.Create(AForm);

    ALoadingIndicator.FadeBackground := AFade;
    ALoadingIndicator.IsModal := AModal;
    ALoadingIndicator.Opacity := AOpacity;
    ALoadingIndicator.Animated := True;
    AForm.AddObject(ALoadingIndicator);

    ALoadingIndicator.BringToFront;
  except
    //
  end;
end;

procedure HideLoadingIndicator(AForm: TCommonCustomForm);
var
  ALoadingIndicator: TksLoadingIndicator;
begin
  if AForm = nil then
    Exit;

  try
    ALoadingIndicator := FindLoadingIndicator(AForm);
    if ALoadingIndicator <> nil then
      AForm.RemoveObject(ALoadingIndicator);
  except
    //
  end;

end;

{ TksLoadingIndicator }

constructor TksLoadingIndicator.Create(AOwner: TComponent);
begin
  inherited;

  Align := TAlignLayout.Contents;

  HitTest := False;
  FLoadingText := 'LOADING';
  FFadeBackground := False;
  FIsModal := False;

  FBackground := TRectangle.Create(Self);
  FBackground.Align := TAlignLayout.Client;
  FBackground.Stroke.Kind := TBrushKind.None;
  FBackground.Fill.Kind := TBrushKind.Solid;
  FBackground.Fill.Color := claBlack;
  FBackground.HitTest := False;
  FBackground.Opacity := 0.3;
  AddObject(FBackground);


 { FRectangle := TRectangle.Create(Self);
  FRectangle.Align := TAlignLayout.Center;
  FRectangle.Stroke.Kind := TBrushKind.Solid;
  FRectangle.Stroke.Color := claBlack;
  FRectangle.Fill.Color := claWhite;
  FRectangle.Width := 50;
  FRectangle.Height := 50;

  FRectangle.XRadius := 5;
  FRectangle.YRadius := 5;

  FRectangle.Opacity := FOpacity;   }

 { FLabel := TLabel.Create(Self);
  FLabel.Align := TAlignLayout.Client;
  FLabel.TextSettings.FontColor := claWhite;
  FLabel.Text := FLoadingText;
  FLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FLabel.TextSettings.VertAlign := TTextAlign.Center;
  FLabel.StyledSettings := [];      }

  FAnimator := TAniIndicator.Create(Self);
  FAnimator.Align := TAlignLayout.Client;
  FAnimator.

  AddObject(FRectangle);
  //AddObject(FLabel);
  AddObject(FAnimator);


end;


destructor TksLoadingIndicator.Destroy;
begin
  inherited;
end;

procedure TksLoadingIndicator.HideLoading;
begin
  HideLoadingIndicator(Owner as TForm);
end;


procedure TksLoadingIndicator.SetAnimated(const Value: Boolean);
begin
  FAnimated := Value;
  FAnimator.Enabled := Value;
end;

procedure TksLoadingIndicator.SetFadeBackground(const Value: Boolean);
begin
  FFadeBackground := Value;
  case Value of
    True: FBackground.Opacity := 0.3;
    False: FBackground.Opacity := 0;
  end;
end;

procedure TksLoadingIndicator.SetIsModal(const Value: Boolean);
begin
  FIsModal := Value;
  FBackground.HitTest := Value;
end;

procedure TksLoadingIndicator.SetOpacity(const Value: single);
begin
  FOpacity := Value;
  //FRectangle.Opacity := Value;
end;

procedure TksLoadingIndicator.ShowLoading;
begin
  ShowLoadingIndicator(Owner as TForm);

end;


end.







