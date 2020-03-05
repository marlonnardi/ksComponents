unit ksFormTransitionUI;

interface

{$I ksComponents.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  ksFormTransition;

type
  TfrmFormTransitionUI = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Fade: TRectangle;
  private

    //function GenerateFormImageExt(AForm: TCommonCustomForm): TBitmap;
    procedure Delay;
    procedure AnimateImage(AImg: TImage; XY: string; ANewValue: single);
    { Private declarations }
  public
    procedure Initialise(AFrom, ATo: TCommonCustomForm);
    procedure Animate(ATransition: TksTransitionType; APop: Boolean);
    { Public declarations }
  end;

implementation

uses ksCommon, System.UIConsts, FMX.Ani, DateUtils, FMX.Platform,
  ksLoadingIndicator;

{$R *.fmx}




procedure TfrmFormTransitionUI.Delay;
var
  ANow: TDatetime;
begin
  ANow := Now;
  while MilliSecondsBetween(ANow, Now) < 100 do
    Application.ProcessMessages;
end;


procedure TfrmFormTransitionUI.Initialise(AFrom, ATo: TCommonCustomForm);
var
  ABmp: TBitmap;
begin

  ATo.SetBounds(0, 0, AFrom.Width, AFrom.Height);
  //Image1.WrapMode := TImageWrapMode.Original;
  //Image2.WrapMode := TImageWrapMode.Original;
  Fade.Fill.Kind := TBrushKind.Solid;
  Fade.Fill.Color := claBlack;
  Fade.Align := TAlignLayout.Client;
  Fade.Opacity := 0;
  ATo.SetBounds(0, 0, AFrom.Width, AFrom.Height);
  ABmp := TBitmap.Create;
  try
    GenerateFormImageExt(AFrom, ABmp);
    Image1.Bitmap := ABmp;

    GenerateFormImageExt(ATo, ABmp);
    Image2.Bitmap := ABmp;
  finally
    FreeAndNil(ABmp);
  end;
  Fade.Opacity := 0;
  {$IFDEF MSWINDOWS}
  SetBounds(AFrom.Left, AFrom.Top, AFrom.Width, AFrom.Height);
  {$ENDIF}
end;

procedure TfrmFormTransitionUI.AnimateImage(AImg: TImage; XY: string; ANewValue: single);
begin
  {$IFNDEF ANDROID}
    {$IFDEF XE10_2_OR_NEWER}
      {$IFDEF FMX_TOKYO_FIX}
      TAnimator.AnimateFloat(AImg, 'Position.'+XY, ANewValue, C_TRANSITION_DURATION);
      {$ELSE}
      TAnimator.AnimateFloatWait(AImg, 'Position.'+XY, ANewValue, C_TRANSITION_DURATION);
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
  TAnimator.AnimateFloat(AImg, 'Position.'+XY, ANewValue, C_TRANSITION_DURATION);
  {$ENDIF}
end;

procedure TfrmFormTransitionUI.Animate(ATransition: TksTransitionType; APop: Boolean);
var
  w,h: single;
begin

  w := Image1.Bitmap.Width / GetScreenScale;
  h := Image1.Bitmap.Height / GetScreenScale;

  if ATransition = ksFtSlideInFromRight then
  begin
    if APop then
    begin
      Fade.Opacity := C_FADE_OPACITY;
      Image2.AddObject(Fade);
      Image2.SetBounds(0-(w/3), 0, w, h);
      Image1.SetBounds(0, 0, w, h);
      Image1.BringToFront;
      Delay;
      TAnimator.AnimateFloat(Image1, 'Position.X', w, C_TRANSITION_DURATION);
      TAnimator.AnimateFloat(Fade, 'Opacity', 0, C_TRANSITION_DURATION);
      AnimateImage(Image2, 'X', 0);
    end
    else
    begin
      Fade.Opacity := 0;
      Image1.AddObject(Fade);
      Image1.SetBounds(0, 0, w, h);
      Image2.SetBounds(w, 0, w, h);
      Image2.BringToFront;
      Delay;
      TAnimator.AnimateFloat(Image1, 'Position.X', 0-(w/3), C_TRANSITION_DURATION);
      TAnimator.AnimateFloat(Fade, 'Opacity', C_FADE_OPACITY, C_TRANSITION_DURATION);
      AnimateImage(Image2, 'X', 0);
    end;
  end;

  if ATransition = ksFtSlideInFromBottom then
  begin
    if APop then
    begin
      Fade.Opacity := C_FADE_OPACITY;
      Image2.AddObject(Fade);
      Image2.SetBounds(0, 0, w, h);
      Image1.SetBounds(0, 0, w, h);
      Image1.BringToFront;
      Delay;
      TAnimator.AnimateFloat(Fade, 'Opacity', 0, C_TRANSITION_DURATION);
      AnimateImage(Image1, 'Y', h);
    end
    else
    begin
      Fade.Opacity := 0;
      Image1.AddObject(Fade);
      Image1.SetBounds(0, 0, w, h);
      Image2.SetBounds(0, h, w, h);
      Image2.BringToFront;
      Delay;
      TAnimator.AnimateFloat(Fade, 'Opacity', C_FADE_OPACITY, C_TRANSITION_DURATION);
      AnimateImage(Image2, 'Y', 0);
    end;
  end;

  if ATransition = ksFtSlideInFromLeft then
  begin
    if APop then
    begin
      Fade.Opacity := C_FADE_OPACITY;
      Image2.AddObject(Fade);
      Image2.SetBounds((w/3), 0, w, h);
      Image1.SetBounds(0, 0, w, h);
      Image1.BringToFront;
      Delay;
      TAnimator.AnimateFloat(Image1, 'Position.X', 0-w, C_TRANSITION_DURATION);
      TAnimator.AnimateFloat(Fade, 'Opacity', 0, C_TRANSITION_DURATION);
      AnimateImage(Image2, 'X', 0);
    end
    else
    begin
      Fade.Opacity := 0;
      Image1.AddObject(Fade);
      Image1.SetBounds(0, 0, w, h);
      Image2.SetBounds(0-w, 0, w, h);
      Image2.BringToFront;
      Delay;
      TAnimator.AnimateFloat(Image1, 'Position.X', (w/3), C_TRANSITION_DURATION);
      TAnimator.AnimateFloat(Fade, 'Opacity', C_FADE_OPACITY, C_TRANSITION_DURATION);
      AnimateImage(Image2, 'X', 0);
    end;
  end;

  if ATransition = ksFtSlideInFromTop then
  begin
    if APop then
    begin
      Fade.Opacity := C_FADE_OPACITY;
      Image2.AddObject(Fade);
      Image2.SetBounds(0, 0, w, h);
      Image1.SetBounds(0, 0, w, h);
      Image1.BringToFront;
      Delay;
      TAnimator.AnimateFloat(Fade, 'Opacity', 0, C_TRANSITION_DURATION);
      AnimateImage(Image1, 'Y', 0-h);
    end
    else
    begin
      Fade.Opacity := 0;
      Image1.AddObject(Fade);
      Image1.SetBounds(0, 0, w, h);
      Image2.SetBounds(0, 0-h, w, h);
      Image2.BringToFront;
      Delay;
      TAnimator.AnimateFloat(Fade, 'Opacity', C_FADE_OPACITY, C_TRANSITION_DURATION);
      AnimateImage(Image2, 'Y', 0);
    end;
  end;


end;

end.
