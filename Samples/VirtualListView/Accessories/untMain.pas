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
    ksVirtualListView1: TksVirtualListView;
    ToolBar2: TToolBar;
    ksSegmentButtons1: TksSegmentButtons;
    procedure FormCreate(Sender: TObject);
    procedure ksSegmentButtons1Change(Sender: TObject);
    procedure ksSegmentButtons1SelectSegment(Sender: TObject; AIndex: Integer;
      AButton: TksSegmentButton);
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


procedure TForm24.ksSegmentButtons1Change(Sender: TObject);
begin
  PopulateList;
end;

procedure TForm24.ksSegmentButtons1SelectSegment(Sender: TObject;
  AIndex: Integer; AButton: TksSegmentButton);
begin
  PopulateList;
end;

procedure TForm24.PopulateList;
var
  ICount: TksAccessoryType;
  AEnumName: string;
  AItem: TksVListItem;
  AColor: TAlphaColor;
begin
  ksVirtualListView1.BeginUpdate;
  try
    ksVirtualListView1.ClearItems;
    for ICount := Low(TksAccessoryType) to High(TksAccessoryType) do
    begin
      AEnumName := GetENumName(TypeInfo(TksAccessoryType), Ord(ICount));
      AItem := ksVirtualListView1.Items.Add(AEnumName, '', '', ICount);
      AColor := claNull;
      case ksSegmentButtons1.ItemIndex of
        1: AColor := claRed;
        2: AColor := claGreen;
        3: AColor := claBlue;
      end;
      if AColor <> claNull then
        AItem.Accessory.Color := AColor;
    end;
  finally
    ksVirtualListView1.EndUpdate;
  end;
end;

end.
