{*******************************************************************************
*                                                                              *
*  TksMapView - extended map view component                                    *
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

unit ksMapView;

interface

{$I ksComponents.inc}

uses
  Classes, FMX.Types, FMX.Controls, FMX.Graphics, System.UITypes,
  FMX.StdCtrls, System.Generics.Collections, FMX.Objects, FMX.Effects,
  System.UIConsts, ksSpeedButton, ksTypes, FMX.Maps, System.Types;

type
  TksMapMarker = class;

  TksMarkerEvent = procedure(Marker: TksMapMarker) of object;

  TksMapMarker = class
  private
    FMarker: TMapMarker;
    FID: string;
    FTagStr: string;
    function GetSnippet: string;
    function GetTitle: string;
  public
    procedure Remove;
    property ID: string read FID;
    property Title: string read GetTitle;
    //property Marker: TMapMarker read FMarker;
    property Snipped: string read GetSnippet;
    property TagStr: string read FTagStr write FTagStr;
  end;

  TksMapMarkers = class(TList<TksMapMarker>)
  private
    function GetMarkerByID(AID: string): TksMapMarker;  public
    procedure DeleteMarker(AMarker: TksMapMarker); overload;
    procedure DeleteMarker(AMarkerID: string); overload;
    property MarkerByID[AID: string]: TksMapMarker read GetMarkerByID;
  public

    function GetKsMarker(AMarker: TMapMarker): TksMapMarker;
    procedure Clear; virtual;
  end;

  IksMapView = interface
  ['{AABA39E3-52EB-4344-BF48-5F3DAE1CB6AA}']
    procedure SetTilt(const Degrees: Single);
  end;

  [ComponentPlatformsAttribute(
    pidWin32 or
    pidWin64 or
    {$IFDEF XE8_OR_NEWER} pidiOSDevice32 or pidiOSDevice64 {$ELSE} pidiOSDevice {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidiOSSimulator32 or pidiOSSimulator64 {$ELSE} pidiOSSimulator {$ENDIF} or
    {$IFDEF XE10_3_OR_NEWER} pidAndroid32Arm or pidAndroid64Arm {$ELSE} pidAndroid {$ENDIF}
    )]

  TksMapView = class(TMapView, IksMapView)
  private
    FMarkers: TksMapMarkers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddMarker(AID: string; ALocation: TMapCoordinate; ATitle, ASnippet: string; AImage: TBitmap): TksMapMarker; overload;
    function AddMarker(AID: string; ALat, ALon: Extended; ATitle, ASnippet: string; AImage: TBitmap): TksMapMarker; overload;
    property Markers: TksMapMarkers read FMarkers;
  published
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  procedure Register;

implementation

uses SysUtils,  Math, ksCommon;

procedure Register;
begin
  RegisterComponents('Kernow Software FMX', [TksMapView]);
end;

{ TksMapView }

function TksMapView.AddMarker(AID: string; ALocation: TMapCoordinate; ATitle, ASnippet: string; AImage: TBitmap): TksMapMarker;
begin
  Result := AddMarker(AID, ALocation.Latitude, ALocation.Longitude, ATitle, ASnippet, AImage);
end;

function TksMapView.AddMarker(AID: string; ALat, ALon: Extended; ATitle, ASnippet: string; AImage: TBitmap): TksMapMarker;
var
  ADesc : TMapMarkerDescriptor;
  ALastMarker: TksMapMarker;
begin
  ALastMarker := FMarkers.MarkerByID[AID];

  Result := TksMapMarker.Create;

  ADesc := TMapMarkerDescriptor.Create(TMapCoordinate.Create(ALat, ALon), ATitle);
  ADesc.Snippet := ASnippet;
  ADesc.Icon := AImage;

  Result.FMarker := AddMarker(ADesc);
  Result.FID := AID;

  FMarkers.Add(Result);

  if ALastMarker <> nil then
    FMarkers.DeleteMarker(ALastMarker);
end;


constructor TksMapView.Create(AOwner: TComponent);
begin
  inherited;
  FMarkers := TksMapMarkers.Create;

end;

destructor TksMapView.Destroy;
begin
  FMarkers.Clear;
  FreeAndNil(FMarkers);
  inherited;
end;



{ TksMapMarkers }

procedure TksMapMarkers.Clear;
var
  Marker: TksMapMarker;
begin
  for Marker in Self do
    Marker.FMarker.Remove;
  inherited Clear;
end;

procedure TksMapMarkers.DeleteMarker(AMarker: TksMapMarker);
begin
  AMarker.FMarker.Remove;
  Delete(IndexOf(AMarker));
end;

procedure TksMapMarkers.DeleteMarker(AMarkerID: string);
var
  Marker: TksMapMarker;
begin
  Marker := MarkerByID[AMarkerID];
  if Marker <> nil then
    DeleteMarker(Marker);
end;

function TksMapMarkers.GetKsMarker(AMarker: TMapMarker): TksMapMarker;
var
  Marker: TksMapMarker;
begin
  Result := nil;
  for Marker in Self do
  begin
    if Marker.FMarker = AMarker then
    begin
      Result := Marker;
      Exit;
    end;
  end;
end;

function TksMapMarkers.GetMarkerByID(AID: string): TksMapMarker;
var
  Marker: TksMapMarker;
begin
  Result := nil;
  for Marker in Self do
  begin
    if Marker.FID = AID then
    begin
      Result := Marker;
      Exit;
    end;
  end;
end;

{ TksMapMarker }

function TksMapMarker.GetSnippet: string;
begin
  Result := FMarker.Descriptor.Snippet;
end;

function TksMapMarker.GetTitle: string;
begin
  Result := FMarker.Descriptor.Title;
end;

procedure TksMapMarker.Remove;
begin
  FMarker.Remove;
end;

end.

