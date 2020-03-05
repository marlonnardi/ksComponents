unit ksVirtualListViewTest;

interface


uses
  DUnitX.TestFramework,
  ksTypes,
  ksCommon,
  ksVirtualListView;

type

  [TestFixture]
  TksVirtualListViewTest = class(TObject)
  private
    FListView: TksVirtualListView;
    FEventFired: Boolean;
    procedure Add100Items;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // tests...
    [Test] procedure TestGetCheckedCount;
    [Test] procedure Test1000Items;
    [Test] procedure TestClearItems;
    [Test] procedure TestScrollToLastItem;
    [Test] procedure TestAccessories;
    [Test] procedure TestGetItemFromPos;
    [Test] procedure TestTotalItemHeight;
    [Test] procedure TestTopItem;
    [Test] procedure TestItemHorzAlignments;
    [Test] procedure TestItemVertAlignments;
    [Test] procedure TestTextWidth;
    [Test] procedure TestTextHeight;
    [Test] procedure TestTextHeightMultiLine;
  end;

implementation

uses SysUtils, System.UITypes, System.UIConsts, System.Classes;

//----------------------------------------------------------------------------------------------------

procedure TksVirtualListViewTest.Setup;
begin
  FListView := TksVirtualListView.Create(nil);
  FEventFired := False;
end;

procedure TksVirtualListViewTest.TearDown;
begin
  FListView.Free;
end;

//----------------------------------------------------------------------------------------------------

procedure TksVirtualListViewTest.Add100Items;
var
  ICount: integer;
begin
  // arrange
  // act
  FListView.BeginUpdate;
  try
    for ICount := 1 to 100 do
      FListView.Items.Add('Item '+IntToStr(ICount), 'a sub title', 'the detail', atNone);
  finally
    FListView.EndUpdate;
  end;
end;

procedure TksVirtualListViewTest.TestGetCheckedCount;
var
  ICount: integer;
begin
  FListView.CheckBoxes.Visible := True;
  FListView.CheckBoxes.Mode := TksSelectionType.ksMultiSelect;
  Add100Items;
  for ICount := 1 to 100 do
  begin
    if ICount mod 2 = 0 then
      FListView.Items[ICount-1].Checked := True;
  end;
  Assert.AreEqual(50, FListView.Items.CheckedCount);
end;


procedure TksVirtualListViewTest.TestGetItemFromPos;
var
  AItem: TksVListItem;
begin
  Add100Items;
  AItem := FListView.Items.ItemAtPos(10, 600);
  Assert.AreEqual(13, AItem.Index);
end;

procedure TksVirtualListViewTest.TestItemHorzAlignments;
var
  ICount: TAlignment;
begin
  for ICount := Low(TAlignment) to High(TAlignment) do
  begin
    with FListView.Items.Add('', '', '') do
      AddText(30, 0, 'TEST').HorzAlign := ICount;
  end;
  Assert.AreEqual(3, FListView.Items.Count);
end;

procedure TksVirtualListViewTest.TestItemVertAlignments;
var
  ICount: TVerticalAlignment;
begin
  for ICount := Low(TVerticalAlignment) to High(TVerticalAlignment) do
  begin
    with FListView.Items.Add('', '', '') do
      AddText(30, 0, 'TEST').VertAlign := ICount;
  end;
  Assert.AreEqual(3, FListView.Items.Count);
end;

procedure TksVirtualListViewTest.Test1000Items;
var
  ICount: integer;
begin
  // arrange
  // act
  FListView.BeginUpdate;
  try
    for ICount := 1 to 1000 do
      FListView.Items.Add('Item '+IntToStr(ICount), 'a sub title', 'the detail', atNone);
  finally
    FListView.EndUpdate;
  end;
  // assert
  Assert.AreEqual(1000, FListView.Items.Count);
end;

procedure TksVirtualListViewTest.TestAccessories;
var
  ICount: TksAccessoryType;
begin
  FListView.BeginUpdate;
  for ICount := Low(TksAccessoryType) to High(TksAccessoryType) do
    FListView.Items.Add('Item', '', '', ICount);
  FListView.EndUpdate;
end;

procedure TksVirtualListViewTest.TestClearItems;
begin
  // arrange
  Add100Items;
  // act
  FListView.ClearItems;
  // assert
  Assert.AreEqual(True, FListView.IsEmpty);
end;

procedure TksVirtualListViewTest.TestScrollToLastItem;
var
  AResult: Extended;
begin
  // arrange
  Add100Items;
  // act
  FListView.ScrollToBottom(False);
  // assert
  AResult := FListView.ScrollPos;
  Assert.AreEqual(4350.0, AResult);
end;

procedure TksVirtualListViewTest.TestTextHeight;
var
  AHeight: string;
begin
  FListView.Items.Add('This is a test', '', '');
  AHeight := FormatFloat('0.00', FListView.Items[0].Title.Height);
  Assert.AreEqual('18.29', AHeight);
end;

procedure TksVirtualListViewTest.TestTextHeightMultiLine;
var
  AHeight: string;
begin
  FListView.Items.Add('This is a test'+#13+'This is the second line...'+#13+'And the third :-)', '', '');
  AHeight := FormatFloat('0.00', FListView.Items[0].Title.Height);
  Assert.AreEqual('52.87', AHeight);
end;

procedure TksVirtualListViewTest.TestTextWidth;
var
  AWidth: string;
begin
  FListView.Items.Add('This is a test', '', '');
  AWidth := FormatFloat('0.00', FListView.Items[0].Title.Width);
  Assert.AreEqual('70.92', AWidth);
end;

procedure TksVirtualListViewTest.TestTopItem;
begin
  Add100Items;
  Assert.AreEqual(0, FListView.TopItem.Index);
end;

procedure TksVirtualListViewTest.TestTotalItemHeight;
begin
  Add100Items;
  Assert.AreEqual(4400, Integer(Round(FListView.TotalItemHeight)));
end;

initialization
  TDUnitX.RegisterTestFixture(TksVirtualListViewTest);
end.
