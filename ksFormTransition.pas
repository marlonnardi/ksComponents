{*******************************************************************************
*                                                                              *
*  TksFormTransition - Push/Pop Form Queue Component                           *
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

unit ksFormTransition;

interface

{$I ksComponents.inc}

uses System.UITypes, FMX.Controls, FMX.Layouts, FMX.Objects, System.Classes,
  FMX.Types, FMX.Graphics, System.UIConsts, FMX.Effects,
  FMX.StdCtrls, System.Types, FMX.Forms, ksTypes, System.Generics.Collections;

const
  {$IFDEF ANDROID}
  C_TRANSITION_DURATION = 0.2;
  {$ELSE}
  C_TRANSITION_DURATION = 0.2;
  {$ENDIF}
  C_FADE_OPACITY = 0.5;

type
  TksTransitionMethod = (ksTmPush, ksTmPop);

  IksFormTransition = interface
  ['{34A12E50-B52C-4A49-B081-9CB67CA5FD6E}']
    procedure BeforeTransition(AType: TksTransitionMethod);
  end;

  IksPostFormTransition = interface
  ['{CD11BABA-8659-4F42-A6BC-5E03B74690EE}']
    procedure AfterTransition(AType: TksTransitionMethod);
  end;

  TksTransitionType = (ksFtSlideInFromLeft,
                       ksFtSlideInFromTop,
                       ksFtSlideInFromRight,
                       ksFtSlideInFromBottom,
                       ksFtSlideOutToLeft,
                       ksFtSlideOutToTop,
                       ksFtSlideOutToRight,
                       ksFtSlideOutToBottom,
                       ksFtNoTransition);

  TksFormTransitionItem = class
    [weak]FFromForm: TCommonCustomForm;
    [weak]FToForm: TCommonCustomForm;
    FTransition: TksTransitionType;
  public
    property FromForm: TCommonCustomForm read FFromForm;
    property ToForm: TCommonCustomForm read FToForm;
    property Transition: TksTransitionType read FTransition write FTransition;
  end;

  TksFormTransitionList = class(TObjectList<TksFormTransitionItem>)
  public

  end;

  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64 {$ELSE} pidiOSDevice {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidiOSSimulator32 or pidiOSSimulator64 {$ELSE} pidiOSSimulator {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidAndroid32Arm or pidAndroid64Arm {$ELSE} pidAndroid {$ENDIF}
    )]

  TksFormTransition = class(TksComponent)
  private
    FInitalizedForms: TList<TCommonCustomForm>;

    function GetTransitionList: TksFormTransitionList;
    function TransitionExists(AFrom, ATo: TCommonCustomForm): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFormDepth(AForm: TCommonCustomForm): integer;
    procedure Push(AForm: TCommonCustomForm;
                   const ATransition: TksTransitionType = ksFtSlideInFromRight;
                   const ARecordPush: Boolean = True);
    procedure Pop;
    procedure PopTo(AFormClass: string);
    procedure PopAllForms;
    property TransitionList: TksFormTransitionList read GetTransitionList;
  end;

  //{$R *.dcr}


  procedure Push(AForm: TCommonCustomForm; const ATransition: TksTransitionType = ksFtSlideInFromRight; const ARecordPush: Boolean = True);
  procedure Pop;
  procedure PopTo(AFormClass: string);
  procedure PopAllForms;
  procedure ClearFormTransitionStack;
  procedure ClearLastFormTransition;

  procedure Register;

var
  ShowLoadingIndicatorOnTransition: Boolean;
  DisableTransitions: Boolean;

implementation

uses FMX.Ani, SysUtils, ksCommon, DateUtils, ksFormTransitionUI, ksToolbar,
  ksPickers, ksLoadingIndicator
  {$IFDEF IOS}
  , IOSApi.UIKit

  {$ENDIF}
  ;

var
  _InternalTransitionList: TksFormTransitionList;
  _InTransition: Boolean;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksFormTransition]);
end;

function IsIPad: Boolean;
begin
  {$IFDEF IOS}
  Result := TUIDevice.Wrap( TUIDevice.OCClass.currentDevice ).userInterfaceIdiom = UIUserInterfaceIdiomPad;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;



procedure Push(AForm: TCommonCustomForm; const ATransition: TksTransitionType = ksFtSlideInFromRight; const ARecordPush: Boolean = True);
var
  ATran: TksFormTransition;
begin
  ATran := TksFormTransition.Create(nil);
  try
    // prevent animation on ipad...
    if IsIPad then
      ATran.Push(AForm, ksFtNoTransition, ARecordPush)
    else
      {$IFDEF ANDROID}
      ATran.Push(AForm, ksFtNoTransition, ARecordPush)
      {$ELSE}
      ATran.Push(AForm, ATransition, ARecordPush);
      {$ENDIF}
  finally
    FreeAndNil(ATran);
  end;
  HideLoadingIndicator(AForm);
end;

procedure Pop;
var
  ATran: TksFormTransition;
begin
  ATran := TksFormTransition.Create(nil);
  try
    ATran.Pop;
  finally
    FreeAndNil(ATran);
  end;
end;

procedure PopTo(AFormClass: string);
var
  ATran: TksFormTransition;
begin
  ATran := TksFormTransition.Create(nil);
  try
    ATran.PopTo(AFormClass);
  finally
    FreeAndNil(ATran);
  end;
end;

procedure PopAllForms;
var
  ATran: TksFormTransition;
begin
  ATran := TksFormTransition.Create(nil);
  try
    ATran.PopAllForms;
  finally
    FreeAndNil(ATran);
  end;
end;

procedure ClearFormTransitionStack;
begin
  _InternalTransitionList.Clear;
end;

procedure ClearLastFormTransition;
begin
  _InternalTransitionList.Delete(_InternalTransitionList.Count-1);
end;

{ TksFormTransition }

constructor TksFormTransition.Create(AOwner: TComponent);
begin
  inherited;
  FInitalizedForms := TList<TCommonCustomForm>.Create;

end;

destructor TksFormTransition.Destroy;
begin
  FreeAndNil(FInitalizedForms);
  inherited;
end;

function TksFormTransition.GetFormDepth(AForm: TCommonCustomForm): integer;
var
  ICount: integer;
begin
  Result := 0;
  for ICount := 0 to TransitionList.Count-1 do
  begin
    if TransitionList[ICount].ToForm = AForm then
    begin
      Result := ICount+1;
    end;
  end;
end;

function TksFormTransition.GetTransitionList: TksFormTransitionList;
begin
  Result := _InternalTransitionList;
end;

procedure TksFormTransition.Pop;
begin
  PopTo('');
end;

procedure TksFormTransition.PopAllForms;
var
  AInfo: TksFormTransitionItem;
  AFrom, ATo: TCommonCustomForm;
  AAnimateForm: TfrmFormTransitionUI;
  AFormIntf: IksFormTransition;
begin
  if _InternalTransitionList.Count = 0 then
    Exit;

  if _InTransition then
    Exit;
  _InTransition := True;
  try
    AAnimateForm := TfrmFormTransitionUI.Create(nil);
    try

      AInfo := _InternalTransitionList.Last;

      AFrom := AInfo.FToForm;
      ATo := _InternalTransitionList.First.FFromForm;


      {$IFDEF XE10_2_OR_NEWER}
      AAnimateForm.SystemStatusBar.Assign(AFrom.SystemStatusBar);
      {$ENDIF}
      {if Supports(ATo, IksFormTransition, AFormIntf) then
        AFormIntf.BeforeTransition(ksTmPop);  }

      // moved to here...
      if Supports(ATo, IksFormTransition, AFormIntf) then
        AFormIntf.BeforeTransition(ksTmPop);

      AAnimateForm.Initialise(AFrom, ATo);
      AAnimateForm.Visible := True;

      AAnimateForm.Animate(AInfo.FTransition, True);
      ATo.Visible := True;
      AAnimateForm.Visible := False;

      // moved to here...
      //if Supports(ATo, IksFormTransition, AFormIntf) then
      //  AFormIntf.BeforeTransition(ksTmPop);

      AFrom.Visible := False;
      ATo.Activate;

      {$IFDEF MSWINDOWS}
      ATo.SetBounds(AFrom.Left, AFrom.Top, AFrom.Width, AFrom.Height);
      {$ENDIF}

      _InternalTransitionList.Clear;//(_InternalTransitionList.Count-1);
    finally
      AAnimateForm.DisposeOf;
    end;
  finally
    _InTransition := False;
  end;
end;

procedure TksFormTransition.PopTo(AFormClass: string);
var
  AInfo: TksFormTransitionItem;
  AFrom, ATo: TCommonCustomForm;
  AAnimateForm: TfrmFormTransitionUI;
  AFormIntf: IksFormTransition;
  ALevels: Integer;
  ICount: integer;
  ALastTransition: TksTransitionType;
begin

  Screen.ActiveForm.Focused := nil;
  ALevels := 1;

  if _InternalTransitionList.Count < 1 then
    Exit;

  if _InTransition then
    Exit;
  _InTransition := True;

  AInfo := _InternalTransitionList.Last;
  AFrom := AInfo.FToForm;
  ATo := AInfo.FFromForm;

  ALastTransition := _InternalTransitionList.Last.FTransition;

  if AFormClass <> '' then
  begin
    while ATo.ClassName <> AFormClass do
    begin
      AInfo := _InternalTransitionList.Items[_InternalTransitionList.IndexOf(AInfo)-1];
      ATo := AInfo.FFromForm;
      Inc(ALevels);
    end;
  end;

  AInfo.Transition := ALastTransition;

  if ShowLoadingIndicatorOnTransition then
    ShowLoadingIndicator(AFrom);
  try


    if Supports(ATo, IksFormTransition, AFormIntf) then
      AFormIntf.BeforeTransition(ksTmPop);

    {$IFNDEF ANDROID}
    AAnimateForm := TfrmFormTransitionUI.Create(nil);
    try
      {$IFDEF XE10_2_OR_NEWER}
      AAnimateForm.SystemStatusBar.Assign(AFrom.SystemStatusBar);
      {$ENDIF}
       // moved to here...


      AAnimateForm.Initialise(AFrom, ATo);
      AAnimateForm.Visible := True;


      AAnimateForm.Animate(AInfo.FTransition, True);
      ATo.Visible := True;
      AAnimateForm.Visible := False;

      AFrom.Visible := False;
      ATo.Activate;

      {$IFDEF MSWINDOWS}
      ATo.SetBounds(AFrom.Left, AFrom.Top, AFrom.Width, AFrom.Height);
      {$ENDIF}

      for ICount := ALevels downto 1 do
        _InternalTransitionList.Delete(_InternalTransitionList.Count-1);
    finally
      AAnimateForm.DisposeOf;
    end;
    {$ELSE}
    ATo.Visible := True;
    AFrom.Visible := False;
    ATo.Activate;
    for ICount := ALevels downto 1 do
      _InternalTransitionList.Delete(_InternalTransitionList.Count-1);
    {$ENDIF}
  finally
    if ShowLoadingIndicatorOnTransition then
      HideLoadingIndicator(AFrom);

    _InTransition := False;
  end;




end;

procedure TksFormTransition.Push(AForm: TCommonCustomForm;
  const ATransition: TksTransitionType = ksFtSlideInFromRight;
  const ARecordPush: Boolean = True);
var
  AInfo: TksFormTransitionItem;
  AFrom, ATo: TCommonCustomForm;
  AAnimateForm: TfrmFormTransitionUI;
  AFormIntf: IksFormTransition;
  APostFormIntf: IksPostFormTransition;
  ATran: TksTransitionType;
begin

  if _InTransition then
    Exit;

  ATran := ATransition;
  if DisableTransitions then
    ATran := ksFtNoTransition;

  if (Screen.ActiveForm = nil) then // can happen when main form calls push in onShow in Android
  	Exit;

  AFrom := Screen.ActiveForm;

  //(AFrom);

  ATo := AForm;
  {$IFDEF MSWINDOWS}
    {$IFDEF XE10_OR_NEWER}

    if AFrom <> nil then
      ATo.Bounds := AFrom.Bounds;
    {$ENDIF}
  {$ENDIF}
  _InTransition := True;
  if (ShowLoadingIndicatorOnTransition) and (AFrom <> nil) then
    ShowLoadingIndicator(AFrom);
  try
    PickerService.HidePickers;



    {$IFDEF ANDROID}
    // fix for Android initial form size
    if FInitalizedForms.IndexOf(AFrom) = -1 then
      FInitalizedForms.Add(AFrom);

    if FInitalizedForms.IndexOf(ATo) = -1 then
    begin
      FInitalizedForms.Add(ATo);
    end;
    {$ENDIF}


    if (AFrom = ATo) then
      Exit;

    if TransitionExists(AFrom, ATo) then
      Exit;

    AInfo := TksFormTransitionItem.Create;
    AInfo.FFromForm := AFrom;
    AInfo.FToForm := ATo;
    AInfo.FTransition := ATran;
    if ARecordPush then
      _InternalTransitionList.Add(AInfo);

    if (ATran <> TksTransitionType.ksFtNoTransition) and (DisableTransitions = False) then
    begin
      AAnimateForm := TfrmFormTransitionUI.Create(nil);
      try
        {$IFDEF XE10_2_OR_NEWER}
        if AFrom.SystemStatusBar <> nil then
        	AAnimateForm.SystemStatusBar.Assign(AFrom.SystemStatusBar);
        {$ENDIF}

        AAnimateForm.Initialise(AFrom, ATo);
        AAnimateForm.Visible := True;
        AAnimateForm.Animate(AInfo.FTransition, False);

        // moved to here...
        if Supports(ATo, IksFormTransition, AFormIntf) then
          AFormIntf.BeforeTransition(ksTmPush);

        AForm.Visible := True;
        AAnimateForm.Visible := False;
        AFrom.Visible := False;
        AForm.Activate;
        {$IFDEF MSWINDOWS}
        AForm.SetBounds(AFrom.Left, AFrom.Top, AFrom.Width, AFrom.Height);
        {$ENDIF}
      finally
        AAnimateForm.DisposeOf;
      end;
    end
    else
    begin
      // no animation...
      if Supports(ATo, IksFormTransition, AFormIntf) then
        AFormIntf.BeforeTransition(ksTmPush);
      AForm.Visible := True;
      if AFrom <> nil then
        AFrom.Visible := False;
    end;
    if Supports(ATo, IksPostFormTransition, APostFormIntf) then
      APostFormIntf.AfterTransition(ksTmPush);

  finally
    if not ARecordPush then
      FreeAndNil(AInfo);
    _InTransition := False;
    if (ShowLoadingIndicatorOnTransition) and (AFrom <> nil) then
      HideLoadingIndicator(AFrom);
  end;
end;

function TksFormTransition.TransitionExists(AFrom,
  ATo: TCommonCustomForm): Boolean;
var
  ICount: integer;
  AItem: TksFormTransitionItem;
begin
  Result := False;
  for ICount := 0 to GetTransitionList.Count-1 do
  begin
    AItem := GetTransitionList.Items[ICount];
    if (AItem.FromForm = AFrom) and (AItem.ToForm = ATo) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

initialization

  {$IFDEF IOS}
  ShowLoadingIndicatorOnTransition := True;
  {$ELSE}
  ShowLoadingIndicatorOnTransition := True;
  {$ENDIF}

  _InternalTransitionList := TksFormTransitionList.Create(True);
  _InTransition := False;
  DisableTransitions := False;

finalization

  FreeAndNil(_InternalTransitionList);

end.



