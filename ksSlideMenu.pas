{*******************************************************************************
*                                                                              *
*  TksSlideMenu - Slide Menu Component                                         *
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

unit ksSlideMenu;

interface

{$I ksComponents.inc}

//{$DEFINE ADD_SAMPLE_MENU_ITEMS}


uses System.UITypes, FMX.Controls, FMX.Layouts, FMX.Objects, System.Classes,
  FMX.Types, Generics.Collections, FMX.Graphics, System.UIConsts, FMX.Effects,
  FMX.StdCtrls, System.Types, FMX.Forms, {ksTableView,} ksVirtualListView, ksTypes,
  ksSlideMenuUI
  {$IFDEF XE8_OR_NEWER}
  ,FMX.ImgList
  {$ENDIF}
  ;

const
  C_DEFAULT_MENU_WIDTH = 220;
  C_DEFAULT_MENU_TOOLBAR_HEIGHT = 44;

  C_DEFAULT_MENU_HEADER_HEIGHT = 30;
  C_DEFAULT_MENU_HEADER_FONT_SIZE = 16;
  C_DEFAULT_MENU_HEADER_TEXT_COLOR = claWhite;
  C_DEFAULT_MENU_HEADER_COLOR = $FF323232;

  C_DEFAULT_MENU_ITEM_HEIGHT = 50;
  C_DEFAULT_MENU_FONT_SIZE = 14;
  C_DEFAULT_MENU_TOOLBAR_FONT_SIZE = 14;
  C_DEFAULT_MENU_SLIDE_SPEED = 0.15;

  //C_DEFAULT_MENU_SELECTED_COLOR = claWhite;
  C_DEFAULT_MENU_SELECTED_FONT_COLOR = claWhite;
  C_DEFAULT_MENU_FONT_COLOR = claBlack;
  C_DEFAULT_MENU_BACKGROUND_COLOR = claWhite;
  C_DEFAULT_MENU_TOOLBAR_COLOR = claWhite;

type
  TksSlideMenu = class;
  TksSlideMenuItemList = class;
  TksMenuPosition = (mpLeft, mpRight);
  TKsMenuStyle = (msOverlap, msReveal);
  TKsMenuTheme = (mtCustom, mtDarkGray, mtDarkBlue, mtDarkOrange, mtDarkGreen, mtLightGray, mtLightBlue, mtLightOrange, mtLightGreen);

  TBuildMenuEvent = procedure(Sender: TObject; AItems: TksSlideMenuItemList) of object;
  TSelectMenuItemEvent = procedure(Sender: TObject; AId: string) of object;

  TksSlideMenuAppearence = class(TPersistent)
  private
    [weak]FSlideMenu: TksSlideMenu;
    FBackgroundColor: TAlphaColor;
    FHeaderColor: TAlphaColor;
    FHeaderFontColor: TAlphaColor;
    FItemColor: TAlphaColor;
    FFontColor: TAlphaColor;
    FSelectedColor: TAlphaColor;
    FSelectedFontColor: TAlphaColor;
    FTheme: TKsMenuTheme;
    FToolBarColor: TAlphaColor;
    FAccessoryColor: TAlphaColor;
    procedure SetHeaderColor(const Value: TAlphaColor);
    procedure SetHeaderFontColor(const Value: TAlphaColor);
    procedure SetItemColor(const Value: TAlphaColor);
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);
    procedure SetSelectedFontColor(const Value: TAlphaColor);
    procedure SetTheme(const Value: TKsMenuTheme);
    procedure SetToolBarColor(const Value: TAlphaColor);
    procedure SetAccessoryColor(const Value: TAlphaColor);
    procedure SetBackgroundColor(const Value: TAlphaColor);
  public
    constructor Create(ASlideMenu: TksSlideMenu); virtual;
  published
    property AccessoryColor: TAlphaColor read FAccessoryColor write SetAccessoryColor default claWhite;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default $FF222222;
    property HeaderColor: TAlphaColor read FHeaderColor write SetHeaderColor default C_DEFAULT_MENU_HEADER_COLOR;
    property HeaderFontColor: TAlphaColor read FHeaderFontColor write SetHeaderFontColor default C_DEFAULT_MENU_HEADER_TEXT_COLOR;
    property ItemColor: TAlphaColor read FItemColor write SetItemColor default $FF222222;
    property FontColor: TAlphaColor read FFontColor write SetFontColor default claWhite;
    property SelectedItemColor: TAlphaColor read FSelectedColor write SetSelectedColor default claRed;
    property SelectedFontColor: TAlphaColor read FSelectedFontColor write SetSelectedFontColor default claWhite;
    property ToolBarColor: TAlphaColor read FToolBarColor write SetToolBarColor default $FF323232;
    property Theme: TKsMenuTheme read FTheme write SetTheme default mtDarkGray;
  end;

  TksSlideMenuItem = class

    FID: string;
    FText: string;
    FBitmap: TBitmap;
    FForm: TCommonCustomForm;
    FIcon: TksStandardIcon;
    FIsHeader: Boolean;

  end;

  TksSlideMenuItemList = class(TObjectList<TksSlideMenuItem>)
  public
    procedure AddItem(AID, AText: string; AForm: TCommonCustomForm; const AIcon: TksStandardIcon = Custom; const ABmp: TBitmap = nil);
    procedure AddHeader(AText: string);
  end;

  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64 {$ELSE} pidiOSDevice {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidiOSSimulator32 or pidiOSSimulator64 {$ELSE} pidiOSSimulator {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidAndroid32Arm or pidAndroid64Arm {$ELSE} pidAndroid {$ENDIF}
    )]

  TksSlideMenu = class(TComponent)
  private
    FInitalizedForms: TList<TCommonCustomForm>;
    FItems: TksSlideMenuItemList;
    FCallingForm: TCommonCustomForm;
    FMenuForm: TfrmSlideMenuUI;
    FAppearence: TksSlideMenuAppearence;
    FOnSelectMenuItemEvent: TSelectMenuItemEvent;
    FAfterSelectMenuItemEvent: TSelectMenuItemEvent;
    FOnBuildMenu: TBuildMenuEvent;
    procedure RebuildMenu;
    procedure SelectItem(Sender: TObject; AItem: TksVListItem);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddMenuItem(AID, AText: string; const AForm: TCommonCustomForm = nil; const AIcon: TksStandardIcon = Custom); deprecated 'Use OnBuildMenu event instead';
    procedure OpenMenu(ACallingForm: TCommonCustomForm; const APosition: TksMenuPosition = mpLeft);
    procedure ShowForm(AID : string);
    procedure SelectMenuItem(AID : string);
    //procedure CloseMenu;
  published
    property Appearence: TksSlideMenuAppearence read FAppearence write FAppearence;
    property OnSelectMenuItemEvent: TSelectMenuItemEvent read FOnSelectMenuItemEvent write FOnSelectMenuItemEvent;
    property AfterSelectItemEvent: TSelectMenuItemEvent read FAfterSelectMenuItemEvent write FAfterSelectMenuItemEvent;
    property OnBuildMenu: TBuildMenuEvent read FOnBuildMenu write FOnBuildMenu;
  end;

  //{$R *.dcr}

  procedure Register;



implementation

uses SysUtils, ksCommon, System.TypInfo, ksLoadingIndicator;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksSlideMenu]);
end;


{ TksSlideMenuAppearence }

constructor TksSlideMenuAppearence.Create(ASlideMenu: TksSlideMenu);
begin
  inherited Create;
  FSlideMenu := ASlideMenu;
  FHeaderColor := $FF323232;
  FItemColor := $FF222222;
  FBackgroundColor := $FF222222;;
  FToolBarColor := $FF323232;
  FFontColor := claWhite;
  FSelectedFontColor := claWhite;
  FSelectedColor := claRed;
  FAccessoryColor := claWhite;
  FTheme := mtDarkGray;
end;

procedure TksSlideMenuAppearence.SetAccessoryColor(const Value: TAlphaColor);
begin
  FAccessoryColor := Value;
end;

procedure TksSlideMenuAppearence.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
end;

procedure TksSlideMenuAppearence.SetFontColor(const Value: TAlphaColor);
begin
  FFontColor := Value;
  FTheme := mtCustom;

end;

procedure TksSlideMenuAppearence.SetHeaderColor(const Value: TAlphaColor);
begin
  FHeaderColor := Value;
  FTheme := mtCustom;
end;



procedure TksSlideMenuAppearence.SetHeaderFontColor(const Value: TAlphaColor);
begin
  FHeaderFontColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetItemColor(const Value: TAlphaColor);
begin
  FItemColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetSelectedColor(const Value: TAlphaColor);
begin
  FSelectedColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetSelectedFontColor(const Value: TAlphaColor);
begin
  FSelectedFontColor := Value;
  FTheme := mtCustom;
end;

procedure TksSlideMenuAppearence.SetTheme(const Value: TKsMenuTheme);
begin
   if Value = mtDarkGray then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FF545454;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claWhite;
      FSelectedColor := claRed;
    end;
  if Value = mtDarkBlue then
    begin
      FHeaderColor := $FF2A7A9D;
      FToolBarColor := $FF323232;
      FItemColor := $FF2A7A9D;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := $FFC7FFFB;
      FSelectedFontColor := claWhite;
      FSelectedColor := $FF1A5670;
    end;
  if Value = mtDarkOrange then
    begin
      FHeaderColor := $FFFF9900;
      FToolBarColor := $FF323232;
      FItemColor := $FFBC7202;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := claBlack;
      FSelectedFontColor := claWhite;
      FSelectedColor := $FFBC7202;
    end;
  if Value = mtDarkGreen then
    begin
      FHeaderColor := $FF76D015;
      FToolBarColor := $FF323232;
      FItemColor := $FF424242;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := claBlack;
      FSelectedFontColor := claBlack;
      FSelectedColor := $FFDCFF00;
    end;
  if Value = mtLightGray then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FF828282;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claWhite;
      FSelectedColor := claRed;
    end;
  if Value = mtLightBlue then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FF2A7A9D;
      FBackgroundColor := FItemColor;
      FFontColor := claWhite;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claBlack;
      FSelectedColor := $FFC7FFFB;
    end;
  if Value = mtLightOrange then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FFFF9900;
      FBackgroundColor := FItemColor;
      FFontColor := claBlack;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claBlack;
      FSelectedColor := $FFFFCC00;
    end;
  if Value = mtLightGreen then
    begin
      FHeaderColor := $FF424242;
      FToolBarColor := $FF323232;
      FItemColor := $FF76D015;
      FBackgroundColor := FItemColor;
      FFontColor := claBlack;
      FHeaderFontColor := $FFDADADA;
      FSelectedFontColor := claBlack;
      FSelectedColor := $FFDCFF00;
    end;
  FTheme := Value;
end;

procedure TksSlideMenuAppearence.SetToolBarColor(const Value: TAlphaColor);
begin
  FToolBarColor := Value;
  FTheme := mtCustom;
end;

{ TksSlideMenuExt }

procedure TksSlideMenu.AddMenuItem(AID, AText: string; const AForm: TCommonCustomForm = nil; const AIcon: TksStandardIcon = Custom);
begin
  FItems.AddItem(AID, AText, AForm, AIcon);
end;
      {
procedure TksSlideMenu.CloseMenu;
begin
  FMenuForm.CloseMenu;
  FCallingForm.Visible := True;
end;  }

constructor TksSlideMenu.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TksSlideMenuItemList.Create;
  FAppearence := TksSlideMenuAppearence.Create(Self);
  FInitalizedForms := TList<TCommonCustomForm>.Create;
end;

destructor TksSlideMenu.Destroy;
begin
  FMenuForm.DisposeOf;
  FreeAndNil(FItems);
  FreeAndNil(FAppearence);
  FreeAndNil(FInitalizedForms);
  inherited;
end;

procedure TksSlideMenu.OpenMenu(ACallingForm: TCommonCustomForm; const APosition: TksMenuPosition = mpLeft);
begin
  if FItems.Count = 0 then
  begin
    if Assigned(FOnBuildMenu) then
      FOnBuildMenu(Self, FItems);
  end;

  FCallingForm := ACallingForm;
  HideLoadingIndicator(FCallingForm);

  if FMenuForm = nil then
  begin
    FMenuForm :=  TfrmSlideMenuUI.Create(nil);

    {$IFDEF XE10_2_OR_NEWER}
   	if ACallingForm.SystemStatusBar <> nil then
      FMenuForm.SystemStatusBar.Assign(ACallingForm.SystemStatusBar);
  	{$ENDIF}

    FMenuForm.Caption := ACallingForm.Caption;
    FMenuForm.OnSelectItem := SelectItem;
    RebuildMenu;
  end;

  ACallingForm.Visible := False;
  FMenuForm.OpenMenu(ACallingForm, APosition = mpLeft);
end;

procedure TksSlideMenu.RebuildMenu;
var
  AStream: TResourceStream;
  AEnumName: String;
  ICount: integer;
  lv: TksVirtualListView;
  AItem: TksVListItem;
  ABmp: TBitmap;
begin
  lv := FMenuForm.lvMenu;
  lv.Appearence.SelectedColor := FAppearence.SelectedItemColor;
  lv.Appearence.SelectedFontColor := FAppearence.SelectedFontColor;
  lv.Appearence.ItemBackground := FAppearence.ItemColor;
  lv.Appearence.Background := FAppearence.BackgroundColor;
  lv.ClearItems;
  for ICount := 0 to FItems.Count-1 do
  begin
    if FItems[ICount].FIsHeader then
    begin
      AItem := lv.Items.AddHeader(FItems[ICount].FText);
      AItem.Background := $FF484848;//claDimgray;// lv.Appearence.HeaderColor;
      AItem.Title.TextSettings.FontColor := claWhite;// lv.Appearence.HeaderFontColor;
    end
    else
    begin
      AItem := lv.Items.Add(FItems[ICount].FText, '', '', atMore);
      AItem.TagInt := ICount;
      AItem.TagStr := FItems[ICount].FID;
      AItem.Title.TextSettings.FontColor := FAppearence.FontColor;
      AItem.Accessory.Color := FAppearence.AccessoryColor;
      if lv.ItemIndex = -1 then
        AItem.Selected := True;

      aBmp := TBitmap.Create;
      try
        if FItems[ICount].FBitmap <> nil then
        begin
          ABmp := FItems[ICount].FBitmap;
          ReplaceOpaqueColor(ABmp, claWhite);

          AItem.Image.Bitmap := ABmp;
          AItem.Image.Width := 20;
          AItem.Image.Height := 20;
        end
        else
        begin

          AEnumName := GetENumName(TypeInfo(TksStandardIcon), Ord(FItems[ICount].FIcon));


          if FItems[ICount].FIcon <> Custom then
          begin

            AStream := TResourceStream.Create(HInstance, AEnumName, RT_RCDATA);
            aBmp.LoadFromStream(AStream);
            ReplaceOpaqueColor(ABmp, claWhite);

            AItem.Image.Bitmap := ABmp;
            AItem.Image.Width := 20;
            AItem.Image.Height := 20;

            AStream.Free;
          end;
        end;
      finally
        ABmp.Free;
      end;
    end;
  end;

end;

procedure TksSlideMenu.SelectItem(Sender: TObject; AItem: TksVListItem);
var
  mi: TksSlideMenuItem;
  AForm: TCommonCustomForm;
  ABmp: TBitmap;
begin
  AForm := nil;
  mi := nil;
  if AItem <> nil then
  begin
    mi := FItems[AItem.TagInt];
    if Assigned(FOnSelectMenuItemEvent) then
      FOnSelectMenuItemEvent(Self, mi.FID);
    AForm := mi.FForm;
  end;



  if (AForm = nil) or (AForm = FCallingForm) then
  begin
    AForm := FCallingForm;
    FMenuForm.CloseMenu;
    AForm.Visible := True;
    Exit;
  end;

  if FCallingForm <> nil then
  begin
    if FInitalizedForms.IndexOf(FCallingForm) = -1 then
      FInitalizedForms.Add(FCallingForm);
  end;

  {$IFDEF ANDROID}
  // fix for Android initial form size
  if FInitalizedForms.IndexOf(AForm) = -1 then
  begin
    //AForm.Visible := True;
    //AForm.Visible := False;
    FInitalizedForms.Add(AForm);
  end;
  {$ENDIF}

  //AForm.SetBounds(0, 0, FCallingForm.Width, FCallingForm.Height);
  {$IFDEF XE10_OR_NEWER}
  AForm.SetBounds(FMenuForm.Bounds);
  {$ELSE}
  AForm.SetBounds(FMenuForm.Left, FMenuForm.Top, FMenuForm.Width, FMenuForm.Height);
  {$ENDIF}

  ABmp := TBitmap.Create;
  try
    GenerateFormImageExt(AForm, ABmp);
    FMenuForm.Bitmap.Assign(ABmp);
  finally
    FreeAndNil(ABmp);
  end;
    //FMenuForm.Image1.Bitmap := GenerateFormImageExt(AForm);
  FMenuForm.CloseMenu;

  AForm.Visible := True;
  AForm.BringToFront;

  //Screen.ActiveForm := AForm;

  if FCallingForm <> AForm then
    FCallingForm.Visible := False;


  {TThread.Synchronize (TThread.CurrentThread,
    procedure ()
    begin
      AForm.Visible := True;
      AForm.BringToFront;
      Screen.ActiveForm := AForm;
      if FCallingForm <> AForm then
        FCallingForm.Visible := False;
    end);}

  if mi <> nil then
  begin
    if Assigned(FAfterSelectMenuItemEvent) then
      FAfterSelectMenuItemEvent(Self, mi.FID);
  end;
end;

procedure TksSlideMenu.ShowForm(AID : string);
var
  listItem: TksVListItem;
begin
  if AID = '' then
  	Exit;

  for listItem in FMenuForm.lvMenu.Items do
  begin
    if (listItem.TagStr = AID) then
    begin
    	FMenuForm.lvMenu.DeselectAll();
      listItem.Selected := True;
      SelectItem(nil, listItem);
      Exit;
    end;
  end;

end;

              
procedure TksSlideMenu.SelectMenuItem(AID : string);
var
  listItem: TksVListItem;
begin
  if AID = '' then
  	Exit;

  for listItem in FMenuForm.lvMenu.Items do
  begin
    if (listItem.TagStr = AID) then
    begin
    	FMenuForm.lvMenu.DeselectAll();
      listItem.Selected := True;
      Exit;
    end;
  end;

end;



{TksSlideMenuItemExtList }

procedure TksSlideMenuItemList.AddHeader(AText: string);
var
  AItem: TksSlideMenuItem;
begin
  AItem := TksSlideMenuItem.Create;
  AItem.FText := AText;
  AItem.FIsHeader := True;
  Add(AItem);

end;

procedure TksSlideMenuItemList.AddItem(AID, AText: string;
  AForm: TCommonCustomForm; const AIcon: TksStandardIcon = Custom; const ABmp: TBitmap = nil);
var
  AItem: TksSlideMenuItem;
begin
  AItem := TksSlideMenuItem.Create;
  AItem.FID := AID;
  AItem.FText := AText;
  AItem.FForm := AForm;
  AItem.FIcon := AIcon;
  AItem.FBitmap := ABmp;
  AItem.FIsHeader := False;
  Add(AItem);
end;

initialization

//  frmSlideMenuUI := TfrmSlideMenuUI.Create(nil);

finalization

//  frmSlideMenuUI.DisposeOf;

end.



