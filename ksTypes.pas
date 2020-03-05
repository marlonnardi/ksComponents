{*******************************************************************************
*                                                                              *
*  ksTypes - ksComponents Base classes and types                               *
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
*  See the License forthe specific language governing permissions and          *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksTypes;

interface

{$I ksComponents.inc}
{$R glyphs.res}

uses Classes, FMX.Controls, FMX.Objects, FMX.Types, FMX.StdCtrls, FMX.Graphics, System.UITypes, Types,
  System.UIConsts, System.Generics.Collections, FMX.InertialMovement;

type
  TksSound = (ksMailNew, ksMailSent, ksVoiceMail, ksBeep, ksMessageReceived, ksMessageSent, ksCameraShutter);


  IksBaseComponent = interface
  ['{23FAF7AC-205E-4F03-924D-DA5C7D663777}']
  end;

  IksSystemSound = interface(IKsBaseComponent)
  ['{CF3A9726-6F3B-4029-B5CD-EB763DB0D2C5}']
    procedure Play(ASound: TksSound);
  end;

  TksControl = class(TControl, IksBaseComponent);


  TksComponent = class(TFmxObject);
  TksRoundRect = class(TRoundRect);
  TksBaseSpeedButton = class(TSpeedButton);

  TksBaseTabControl = class(TksComponent)
  protected
    procedure AddTab; virtual; abstract;
  end;

  TksAccessoryType = (atNone, atMore, atCheckmark, atDetail, atEllipses, atFlag, atBack, atRefresh,
    atAction, atPlay, atRewind, atForward, atPause, atStop, atAdd, atPrior,
    atNext, atArrowUp, atArrowDown, atArrowLeft, atArrowRight, atReply,
    atSearch, atBookmarks, atTrash, atOrganize, atCamera, atCompose, atInfo,
    atPagecurl, atDetails, atRadioButton, atRadioButtonChecked, atCheckBox,
    atCheckBoxChecked, atUserDefined1, atUserDefined2, atUserDefined3);

  TksStandardIcon = (Custom, AlarmClock, BarChart, Barcode, Bell, BookCover, BookCoverMinus, BookCoverPlus, BookMark, BookOpen,
                    Calendar, Camera, Car, Clock, CloudDownload, CloudUpload, Cross, Document, Download, Earth, Email,
                    Fax, FileList, FileMinus, FilePlus, Files, FileStar, FileTick, Flag, Folder, FolderMinus,
                    FolderPlus, FolderStar, Home, Inbox, Incoming, ListBullets, ListCheckBoxes, ListImages, ListNumbered, ListTicked,
                    Location, More, Note, Outgoing,
                    PaperClip, Photo, PieChart, Pin, Presentation, Search, Settings, Share, ShoppingCart, Spanner, Speaker,
                    Star, Tablet, Tag, Telephone, Telephone2, TelephoneBook, Tick, Timer, Trash, Upload,
                    User, UserEdit, UserGroup, Users, UserSearch,
                    VideoCamera, VideoPlayer, Viewer,
                    Wifi, Window, Write);


  //---------------------------------------------------------------------------------------

  TksTableViewAccessoryImage = class(TBitmap)
  private
    FColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
  public
    procedure SetBitmap(ASource: TBitmap);
    procedure DrawToCanvas(ACanvas: TCanvas; ADestRect: TRectF; const AStretch: Boolean = True);
    property Color: TAlphaColor read FColor write SetColor default claNull;
  end;

  //---------------------------------------------------------------------------------------

  TksTableViewAccessoryImageList = class(TObjectList<TksTableViewAccessoryImage>)
  private
    FImageScale: single;
    FImageMap: TBitmap;
    FActiveStyle: TFmxObject;
    procedure AddEllipsesAccessory;
    procedure AddFlagAccessory;
    function GetAccessoryFromResource(AStyleName: array of string; const AState: string = ''): TksTableViewAccessoryImage;
    procedure Initialize;
  public
    constructor Create;
    destructor Destroy; override;
    function GetAccessoryImage(AAccessory: TksAccessoryType): TksTableViewAccessoryImage;
    procedure DrawAccessory(ACanvas: TCanvas; ARect: TRectF; AAccessory: TksAccessoryType; AStroke, AFill: TAlphaColor);
    property Images[AAccessory: TksAccessoryType]: TksTableViewAccessoryImage read GetAccessoryImage; default;
    property ImageMap: TBitmap read FImageMap;
    property ImageScale: single read FImageScale;
  end;

  TksAniCalc = class(TAniCalculations)
  public
    procedure UpdatePosImmediately;
  end;

var
  AUnitTesting: Boolean;
  AAccessories: TksTableViewAccessoryImageList;

  function SwitchWidth: single;
  function SwitchHeight: single;

  procedure SwitchImage(ACanvas: TCanvas; ARect: TRectF; AChecked: Boolean);

implementation

uses FMX.Forms, ksCommon, SysUtils,FMX.Styles, FMX.Styles.Objects, Math, ksSystemSound;

var
  ASwitchSize: TSizeF;
  ASwitchBmp: array[False..True] of TBitmap;

// ------------------------------------------------------------------------------


procedure InitializeSwitch(const AForce: Boolean = false); var ASwitch: TSwitch; i: Boolean;
begin
  if not assigned(application.MainForm) then exit;
  if (not AForce) and (not ASwitchSize.IsZero) then exit;

  ASwitch := TSwitch.Create(nil);
  try
    ASwitch.Parent := application.MainForm;
    ASwitch.Visible := False;
    ASwitch.ApplyStyleLookup;
    for i := Low(ASwitchBmp) to High(ASwitchBmp) do
    begin
      ASwitch.IsChecked := i;

      ASwitchSize.Width := ASwitch.Width;
      ASwitchSize.Height := ASwitch.Height;

      ASwitchBmp[i].SetSize(Round(ASwitch.Width), Round(ASwitch.Height));
      ASwitchBmp[i].Clear(0);
      if ASwitchBmp[i].Canvas.BeginScene(nil) then
      begin
        ASwitch.PaintTo(ASwitchBmp[i].Canvas, RectF(0, 0, ASwitch.Width,
          ASwitch.Height), nil);
        ASwitchBmp[i].Canvas.EndScene;
      end;
    end;
  finally
    ASwitch.DisposeOf;
  end;
end;

(*
procedure InitialiseSwitchSize;
var
  ASwitch: TSwitch;
begin
  ASwitch := TSwitch.Create(nil);
  try
    ASwitchSize.Width := ASwitch.Width;
    ASwitchSize.Height := ASwitch.Height;
  finally
    ASwitch.DisposeOf;
  end;
end;  *)

function SwitchWidth: single;
begin
  InitializeSwitch;
  Result := ASwitchSize.Width;
end;

function SwitchHeight: single;
begin
  InitializeSwitch;
  Result := ASwitchSize.Height;
end;

procedure SwitchImage(ACanvas: TCanvas; ARect: TRectF; AChecked: Boolean);
var
  ASaveState: TCanvasSaveState;
begin
  InitializeSwitch;
  ASaveState := ACanvas.SaveState;
  try
    ACanvas.DrawBitmap(ASwitchBmp[AChecked], RectF(0, 0, ASwitchBmp[AChecked].Width, ASwitchBmp[AChecked].Height), ARect, 1);
  finally
    ACanvas.RestoreState(ASaveState);
  end;
end;

procedure PlaySystemSound(ASound: TksSound);
var
  AObj: TksSystemSound;
begin
  AObj := TksSystemSound.Create;
  AObj.Play(ASound);
end;


{ TksTableViewAccessoryImageList }

function TksTableViewAccessoryImageList.GetAccessoryImage(AAccessory: TksAccessoryType): TksTableViewAccessoryImage;
begin
  Result := nil;
  if Count = 0 then
  begin
    Initialize;
  end;
  if Ord(AAccessory) < Count then
    Result := Items[Ord(AAccessory)];
end;

procedure TksTableViewAccessoryImageList.AddEllipsesAccessory;
var
  AAcc: TksTableViewAccessoryImage;
  ARect: TRectF;
  ASpacing: single;
  ASize: single;
begin
  AAcc := TksTableViewAccessoryImage.Create;
  AAcc.SetSize(Round(32 * GetScreenScale), Round(32 * GetScreenScale));
  ASize := 7 * GetScreenScale;
  ASpacing := (AAcc.Width - (3 * ASize)) / 2;

  AAcc.Clear(claNull);
  AAcc.Canvas.BeginScene;
  try
    AAcc.Canvas.Fill.Color := claSilver;
    ARect := RectF(0, 0, ASize, ASize);
    OffsetRect(ARect, 0, (AAcc.Height - ARect.Height) / 2);
    AAcc.Canvas.FillEllipse(ARect, 1);
    OffsetRect(ARect, ASize+ASpacing, 0);
    AAcc.Canvas.FillEllipse(ARect, 1);
    OffsetRect(ARect, ASize+ASpacing, 0);
    AAcc.Canvas.FillEllipse(ARect, 1);
  finally
    AAcc.Canvas.EndScene;
  end;
  Add(AAcc);
end;

procedure TksTableViewAccessoryImageList.AddFlagAccessory;
var
  AAcc: TksTableViewAccessoryImage;
  ARect: TRectF;
  s: single;
  r1, r2: TRectF;
begin
  s := GetScreenScale;
  AAcc := TksTableViewAccessoryImage.Create;
  AAcc.SetSize(Round(32 * s), Round(32 * s));
  AAcc.Clear(claNull);
  ARect := RectF(0, 0, AAcc.Width, AAcc.Height);
  ARect.Inflate(0-(AAcc.Width / 4), 0-(AAcc.Height / 7));


  AAcc.Canvas.BeginScene;
  try
    r1 := ARect;
    r2 := ARect;

    r2.Top := ARect.Top+(ARect.Height/12);


    r2.Left := r2.Left;
    r2.Height := ARect.Height / 2;
    AAcc.Canvas.stroke.Color := claSilver;
    AAcc.Canvas.Stroke.Thickness := s*2;
    AAcc.Canvas.Fill.Color := claSilver;
    AAcc.Canvas.FillRect(r2, 0, 0, AllCorners, 1);
    AAcc.Canvas.DrawLine(r1.TopLeft, PointF(r1.Left, r1.Bottom), 1);
  finally
    AAcc.Canvas.EndScene;
  end;
  Add(AAcc);
end;

constructor TksTableViewAccessoryImageList.Create;
begin
  inherited Create(True);
  FImageScale := 0;
  FImageMap := TBitmap.Create;
end;

destructor TksTableViewAccessoryImageList.Destroy;
begin
  FreeAndNil(FImageMap);
  if FActiveStyle <> nil then
    FreeAndNil(FActiveStyle);
  inherited;
end;

procedure TksTableViewAccessoryImageList.DrawAccessory(ACanvas: TCanvas; ARect: TRectF; AAccessory: TksAccessoryType; AStroke, AFill: TAlphaColor);
var
  AAcc: TksTableViewAccessoryImage;
begin
  if AFill <> claNull then
  begin
    ACanvas.Fill.Color := AFill;
    ACanvas.Fill.Kind := TBrushKind.Solid;
    ACanvas.FillRect(ARect, 0, 0, AllCorners, 1);
  end;
  AAcc := GetAccessoryImage(AAccessory);
  if AAcc <> nil then
    AAcc.DrawToCanvas(ACanvas, ARect, False);
end;

function TksTableViewAccessoryImageList.GetAccessoryFromResource
  (AStyleName: array of string; const AState: string = ''): TksTableViewAccessoryImage;
var
  AStyleObj: TStyleObject;
  AImgRect: TBounds;
  r: TRectF;
  ABitmapLink: TBitmapLinks;
  AImageMap: TBitmap;
  I: integer;
  AScale: single;
  ICount: integer;
begin
  Result := TksTableViewAccessoryImage.Create;

  if AUnitTesting then
  begin
    if FActiveStyle = nil then
      FActiveStyle := TStyleManager.ActiveStyle(Nil);
    AStyleObj := TStyleObject(FActiveStyle)
  end
  else
    AStyleObj := TStyleObject(TStyleManager.ActiveStyle(nil));


  for ICount := Low(AStyleName) to High(AStyleName) do
    AStyleObj := TStyleObject(AStyleObj.FindStyleResource(AStyleName[ICount]));

  if AStyleObj <> nil then
  begin
    if FImageMap.IsEmpty then
    begin
      FImageScale := GetScreenScale(False);

      for i := 0 to (AStyleObj as TStyleObject).Source.MultiResBitmap.Count-1 do
      begin
        AScale := (AStyleObj as TStyleObject).Source.MultiResBitmap[i].Scale;
        if Round(AScale) <= FImageScale then
        begin
          FImageScale := Round(AScale);
          Break;
        end;
      end;
      AImageMap := ((AStyleObj as TStyleObject).Source.MultiResBitmap.Bitmaps[FImageScale]);
      FImageMap.SetSize(Round(AImageMap.Width), Round(AImageMap.Height));
      FImageMap.Clear(claNull);

      FImageMap.Canvas.BeginScene;
      try
        FImageMap.Canvas.DrawBitmap(AImageMap,
                                    RectF(0, 0, AImageMap.Width, AImageMap.Height),
                                    RectF(0, 0, FImageMap.Width, FImageMap.Height),
                                    1,
                                    True);
      finally
        FImageMap.Canvas.EndScene;
      end;
    end;

    ABitmapLink := nil;
    if AStyleObj = nil then
      Exit;
    if (AStyleObj.ClassType = TCheckStyleObject) then
    begin
      if AState = 'checked' then
        ABitmapLink := TCheckStyleObject(AStyleObj).ActiveLink
      else
        ABitmapLink := TCheckStyleObject(AStyleObj).SourceLink
    end;

    if ABitmapLink = nil then
      ABitmapLink := AStyleObj.SourceLink;

{$IFDEF XE8_OR_NEWER}
    AImgRect := ABitmapLink.LinkByScale(FImageScale, True).SourceRect;
{$ELSE}
    AImgRect := ABitmapLink.LinkByScale(FImageScale, False).SourceRect;
{$ENDIF}
    Result.SetSize(Round(AImgRect.Width), Round(AImgRect.Height));
    Result.Clear(claNull);
    Result.Canvas.BeginScene;

    r := AImgRect.Rect;

    Result.Canvas.DrawBitmap(FImageMap, r, RectF(0, 0, Result.Width,
      Result.Height), 1, True);
    Result.Canvas.EndScene;
  end;
end;

procedure TksTableViewAccessoryImageList.Initialize;
var
  ICount: TksAccessoryType;
begin
  for ICount := Low(TksAccessoryType) to High(TksAccessoryType) do
  begin
    case ICount of
      atNone: Add(GetAccessoryFromResource(['none']));
      atMore: Add(GetAccessoryFromResource(['listviewstyle','accessorymore']));
      atCheckmark: Add(GetAccessoryFromResource(['listviewstyle','accessorycheckmark']));
      atDetail: Add(GetAccessoryFromResource(['listviewstyle','accessorydetail']));
      atEllipses: AddEllipsesAccessory;
      atFlag: AddFlagAccessory;
      atBack: Add(GetAccessoryFromResource(['backtoolbutton','icon']));
      atRefresh: Add(GetAccessoryFromResource(['refreshtoolbutton','icon']));
      atAction: Add(GetAccessoryFromResource(['actiontoolbutton','icon']));
      atPlay: Add(GetAccessoryFromResource(['playtoolbutton','icon']));
      atRewind: Add(GetAccessoryFromResource(['rewindtoolbutton','icon']));
      atForward: Add(GetAccessoryFromResource(['forwardtoolbutton','icon']));
      atPause: Add(GetAccessoryFromResource(['pausetoolbutton','icon']));
      atStop: Add(GetAccessoryFromResource(['stoptoolbutton','icon']));
      atAdd: Add(GetAccessoryFromResource(['addtoolbutton','icon']));
      atPrior: Add(GetAccessoryFromResource(['priortoolbutton','icon']));
      atNext: Add(GetAccessoryFromResource(['nexttoolbutton','icon']));
      atArrowUp: Add(GetAccessoryFromResource(['arrowuptoolbutton','icon']));
      atArrowDown: Add(GetAccessoryFromResource(['arrowdowntoolbutton','icon']));
      atArrowLeft: Add(GetAccessoryFromResource(['arrowlefttoolbutton','icon']));
      atArrowRight: Add(GetAccessoryFromResource(['arrowrighttoolbutton','icon']));
      atReply: Add(GetAccessoryFromResource(['replytoolbutton','icon']));
      atSearch: Add(GetAccessoryFromResource(['searchtoolbutton','icon']));
      atBookmarks: Add(GetAccessoryFromResource(['bookmarkstoolbutton','icon']));
      atTrash: Add(GetAccessoryFromResource(['trashtoolbutton','icon']));
      atOrganize: Add(GetAccessoryFromResource(['organizetoolbutton','icon']));
      atCamera: Add(GetAccessoryFromResource(['cameratoolbutton','icon']));
      atCompose: Add(GetAccessoryFromResource(['composetoolbutton','icon']));
      atInfo: Add(GetAccessoryFromResource(['infotoolbutton','icon']));
      atPagecurl: Add(GetAccessoryFromResource(['pagecurltoolbutton','icon']));
      atDetails: Add(GetAccessoryFromResource(['detailstoolbutton','icon']));
      atRadioButton: Add(GetAccessoryFromResource(['radiobuttonstyle','background']));
      atRadioButtonChecked: Add(GetAccessoryFromResource(['radiobuttonstyle','background'], 'checked'));
      atCheckBox: Add(GetAccessoryFromResource(['checkboxstyle','background']));
      atCheckBoxChecked: Add(GetAccessoryFromResource(['checkboxstyle','background'], 'checked'));
      atUserDefined1: Add(GetAccessoryFromResource(['userdefined1']));
      atUserDefined2: Add(GetAccessoryFromResource(['userdefined2']));
      atUserDefined3: Add(GetAccessoryFromResource(['userdefined3']));
    end;
  end;
  // generate our own ellipses accessory...

end;

// ------------------------------------------------------------------------------

{ TksAccessoryImage }

procedure TksTableViewAccessoryImage.DrawToCanvas(ACanvas: TCanvas;
  ADestRect: TRectF; const AStretch: Boolean = True);
var
  r: TRectF;
begin
  r := ADestRect;
  if AStretch = False then
  begin
    r := RectF(0, 0, Width / GetScreenScale, Height / GetScreenScale);
    OffsetRect(r, ADestRect.Left, ADestRect.Top);
    OffsetRect(r, (ADestRect.Width-r.Width)/2, (ADestRect.Height-r.Height)/2);
  end;

  ACanvas.DrawBitmap(Self, RectF(0, 0, Width, Height), r, 1, True);
end;

procedure TksTableViewAccessoryImage.SetBitmap(ASource: TBitmap);
var
  AScale: single;
begin
  AScale := Min(Trunc(GetScreenScale), 3);
  Assign(ASource);
  Resize(Round(32 * AScale), Round(32 * AScale));
end;

procedure TksTableViewAccessoryImage.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    ReplaceOpaqueColor(Value);
  end;
end;

{ TksAniCalc }

procedure TksAniCalc.UpdatePosImmediately;
begin
  inherited UpdatePosImmediately(True);
end;

initialization
  AUnitTesting := False;
  AAccessories := TksTableViewAccessoryImageList.Create;

  ASwitchBmp[True] := TBitmap.Create;
  ASwitchBmp[False] := TBitmap.Create;


  finalization

  FreeAndNil(AAccessories);

  FreeAndNil(ASwitchBmp[True]);
  FreeAndNil(ASwitchBmp[False]);

end.

