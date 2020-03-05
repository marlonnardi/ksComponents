{*******************************************************************************
*                                                                              *
*  TksSpeedButton - TSpeedButton with iOS style badge                          *
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

unit ksSpeedButton;

interface

{$I ksComponents.inc}


uses Classes, FMX.StdCtrls, FMX.Graphics, ksControlBadge, ksTypes, FMX.Objects,
  System.UITypes, System.UIConsts;

type
  {TksSpeedButtonIcon = (Custom, AlarmClock, BarChart, Barcode, Bell, BookCover, BookCoverMinus, BookCoverPlus, BookMark, BookOpen,
                        Calendar, Camera, Car, Clock, CloudDownload, CloudUpload, Cross, Document, Download, Earth, Email,
                        Fax, FileList, FileMinus, FilePlus, Files, FileStar, FileTick, Flag, Folder, FolderMinus,
                        FolderPlus, FolderStar, Home, Inbox, Incoming, ListBullets, ListCheckBoxes, ListImages, ListNumbered, ListTicked,
                        Location, More, Note, Outgoing,
                        PaperClip, Photo, PieChart, Pin, Presentation, Search, Settings, Share, ShoppingCart, Spanner, Speaker,
                        Star, Tablet, Tag, Telephone, Telephone2, TelephoneBook, Tick, Timer, Trash, Upload,
                        User, UserEdit, UserGroup, Users, UserSearch,
                        VideoCamera, VideoPlayer, Viewer,
                        Wifi, Window, Write);  }


  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64 {$ELSE} pidiOSDevice {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidiOSSimulator32 or pidiOSSimulator64 {$ELSE} pidiOSSimulator {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidAndroid32Arm or pidAndroid64Arm {$ELSE} pidAndroid {$ENDIF}
    )]

  TksSpeedButton = class(TksBaseSpeedButton)
  private
    FBadge: TksControlBadge;
    FIcon: TksStandardIcon;
    FBitmap: TBitmap;
    FMouseDown: Boolean;
    FIconColor: TAlphaColor;
    procedure SetBadge(Value: TksBadgeProperties);
    function GetBadge: TksBadgeProperties;
    procedure SetIcon(const Value: TksStandardIcon);
    procedure Invalidate;
    procedure SetIconColor(const Value: TAlphaColor);
  protected
    procedure Resize; override;
    procedure Paint; override;
    function GetDefaultStyleLookupName: string; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Icon: TksStandardIcon read FIcon write SetIcon;
    property Badge: TksBadgeProperties read GetBadge write SetBadge;
    property IconColor: TAlphaColor read FIconColor write SetIconColor default claNull;
  end;


  procedure Register;

implementation

uses Math, FMX.types, System.TypInfo, System.Types, ksCommon, SysUtils,
  Fmx.Forms, FMX.Controls;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksSpeedButton]);
end;

{ TksSpeedButton }

constructor TksSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBadge := TksControlBadge.Create(Self);
  FIcon := Custom;
  FBitmap := TBitmap.Create;
  FIconColor := claNull;
  RepeatClick := False;
  ///FImage := TImage.Create(Self);
  //FImage.HitTest := False;
  //FImage.Locked := True;
  //FImage.Stored := False;
  //StyledSettings := [];
  //FImage.Align := TAlignLayout.Center;
  //AddObjecT(FImage);
  AddObject(FBadge);
end;

destructor TksSpeedButton.Destroy;
begin
  FreeAndNil(FBitmap);
  {$IFDEF NEXTGEN}
  FBadge.DisposeOf;
  //FImage.DisposeOf;
  {$ELSE}
  FBadge.Free;
  //FImage.Free;
  {$ENDIF}
  inherited;
end;


function TksSpeedButton.GetBadge: TksBadgeProperties;
begin
  Result := FBadge.Properties;
end;

procedure TksSpeedButton.Resize;
begin
  inherited;
  FBadge.Position.X := (Width - FBadge.Width) - (Width * 0.1);
end;

function TksSpeedButton.GetDefaultStyleLookupName: string;
begin
  Result := GenerateStyleName(TSpeedButton.ClassName);
end;

procedure TksSpeedButton.Invalidate;
begin
  InvalidateRect(ClipRect);
end;

procedure TksSpeedButton.Paint;
var
  AImageRect: TRectF;
  ABmp: TBitmap;
  ASaveState: TCanvasSaveState;
begin
  inherited;
  if Locked then
    Exit;
  ASaveState := Canvas.SaveState;
  try
    canvas.IntersectClipRect(ClipRect);
    AImageRect := RectF(0, 0, 24, 24);;
    OffsetRect(AImageRect,
               (Width - AImageRect.Width) / 2,
               (Height - AImageRect.Height) / 2);


    ABmp := TBitmap.Create;
    try
      ABmp.Assign(FBitmap);

      if (FMouseDown) then
      begin
        {$IFDEF IOS}
        ReplaceOpaqueColor(ABmp, GetColorOrDefault(FIconColor, claLightskyblue));
        {$ENDIF}
        {$IFDEF ANDROID}
        ReplaceOpaqueColor(ABmp, GetColorOrDefault(FIconColor, claDimgray));
        {$ENDIF}
      end
      else
      begin
        {$IFDEF IOS}
        ReplaceOpaqueColor(ABmp, GetColorOrDefault(FIconColor, claDodgerblue));
        {$ENDIF}
        {$IFDEF ANDROID}
        ReplaceOpaqueColor(ABmp, GetColorOrDefault(FIconColor, claDimgray));
        {$ENDIF}
        {$IFDEF MSWINDOWS}
        ReplaceOpaqueColor(ABmp, FIconColor);
        {$ENDIF}
      end;

      Canvas.DrawBitmap(ABmp,
                        RectF(0, 0, ABmp.Width, ABmp.Height),
                        AImageRect,
                        1,
                        False);
    finally
      ABmp.Free;
    end;
  finally
    Canvas.RestoreState(ASaveState);
  end;
end;

procedure TksSpeedButton.SetIcon(const Value: TksStandardIcon);
var
  AStream: TResourceStream;
  AEnumName: String;
begin
  if Value <> TksStandardIcon.Custom  then
  begin
    AEnumName := GetENumName(TypeInfo(TksStandardIcon), Ord(Value));
    AStream := TResourceStream.Create(HInstance, AEnumName, RT_RCDATA);
    try
      FBitmap.Clear(claNull);
      FBitmap.LoadFromStream(AStream);

      if (FMouseDown) then
      begin
        ReplaceOpaqueColor(FBitmap, GetColorOrDefault(FIconColor, claLightskyblue));
      end
      else
      begin
        ReplaceOpaqueColor(FBitmap, GetColorOrDefault(FIconColor, claDodgerblue));
      end;
    finally
      AStream.Free;
    end;
  end;
  FIcon := Value;
  Invalidate;
end;

procedure TksSpeedButton.SetIconColor(const Value: TAlphaColor);
begin
  FIconColor := Value;
  Repaint;
end;

procedure TksSpeedButton.SetBadge(Value: TksBadgeProperties);
begin
  FBadge.Properties.Assign(Value);
end;

procedure TksSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FMouseDown := True;
  Invalidate;
  //Application.ProcessMessages;
end;


procedure TksSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  FMouseDown := False;
  Invalidate;
  //Application.ProcessMessages;
  inherited;
end;

initialization

  Classes.RegisterClass(TksSpeedButton);

end.
