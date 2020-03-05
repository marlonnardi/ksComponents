unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox, ksTypes, ksTableView, ksSegmentButtons,
  ksVirtualListView;

type
  TForm24 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    ToolBar2: TToolBar;
    ksVirtualListView1: TksVirtualListView;
    procedure FormCreate(Sender: TObject);
    procedure ksVirtualListView1GetPickerItems(Sender: TObject;
      ARow: TksVListItem; var ASelected: string; AItems: TStrings);
  private

    procedure PopulateList;

    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form24: TForm24;

implementation

uses System.UIConsts, TypInfo;

{$R *.fmx}

procedure TForm24.FormCreate(Sender: TObject);
begin
  PopulateList;
end;

procedure TForm24.ksVirtualListView1GetPickerItems(Sender: TObject;
  ARow: TksVListItem; var ASelected: string; AItems: TStrings);
begin
  if ARow.TagStr = 'CAR' then AItems.CommaText := 'BMW,Mercedes,Ferrari,Bugatti';
  if ARow.TagStr = 'COLOUR' then AItems.CommaText := 'Red,Green,Yellow,Blue';
end;

procedure TForm24.PopulateList;
var
  AItem: TksVListItem;
begin
  ksVirtualListView1.BeginUpdate;
  try
    AItem := ksVirtualListView1.Items.Add('Car', '', '', atMore); //[]
    AItem.SelectorType := ksSelectorPicker;
    AItem.TagStr := 'CAR';

    AItem := ksVirtualListView1.Items.Add('Colour', '', '', atMore); // ['Red', 'Green', 'Yellow', 'Blue']);
    AItem.SelectorType := ksSelectorPicker;
    AItem.TagStr := 'COLOUR';

    AItem := ksVirtualListView1.Items.Add('Date', '', '', atMore); // ['Red', 'Green', 'Yellow', 'Blue']);
    AItem.SelectorType := ksSelectorDate;
  finally
    ksVirtualListView1.EndUpdate;
  end;
end;

end.
