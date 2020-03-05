unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, ksToolBar, ksTypes, ksVirtualListView;

type
  TForm106 = class(TForm)
    ksVirtualListView1: TksVirtualListView;
    ksToolbar1: TksToolbar;
    ToolBar1: TToolBar;
    lblClickInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ksVirtualListView1ItemSegmentButtonClick(Sender: TObject;
      AItem: TksVListItem; ASegID: string; AItemIndex: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form106: TForm106;

implementation

{$R *.fmx}

procedure TForm106.FormCreate(Sender: TObject);
var
  AItem: TksVListItem;
begin
  AItem := ksVirtualListView1.Items.Add('Item 1', '', '');
  AItem.AddSegmentButtons(['ONE', 'TWO', 'THREE'], 150);
  AItem.CanSelect := False;

  AItem := ksVirtualListView1.Items.Add('Item 2', '', '');
  AItem.AddSegmentButtons(['RED', 'GREEN', 'BLUE', 'YELLOW'], 200);
  AItem.CanSelect := False;

  AItem := ksVirtualListView1.Items.Add('Item 3', '', '');
  AItem.AddSegmentButtons(['YES', 'NO', 'MAYBE'], 180);
  AItem.CanSelect := False;
end;

procedure TForm106.ksVirtualListView1ItemSegmentButtonClick(Sender: TObject;
  AItem: TksVListItem; ASegID: string; AItemIndex: Integer);
begin
  lblClickInfo.Text :=('item: '+IntToStr(AItem.Index)+' - Segment index: '+IntToStr(AItemIndex));
end;

end.
