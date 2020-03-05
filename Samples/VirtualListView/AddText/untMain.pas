unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ksTypes,
  ksVirtualListView, FMX.StdCtrls, FMX.Controls.Presentation, ksToolBar;

type
  TfrmMain = class(TForm)
    ksToolbar1: TksToolbar;
    ToolBar1: TToolBar;
    ksVirtualListView1: TksVirtualListView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  AItem: TksVListItem;
begin
  ksVirtualListView1.Items.AddHeader('AddText - VertAlign = Top');
  AItem := ksVirtualListView1.Items.Add;
  AItem.AddText(0, 0, 'AddText 0, 0').VertAlign := TVerticalAlignment.taAlignTop;
  AItem.AddText(0, 16, 'AddText 0, 16').VertAlign := TVerticalAlignment.taAlignTop;
  AItem.AddText(0, 32, 'AddText 0, 32').VertAlign := TVerticalAlignment.taAlignTop;

  ksVirtualListView1.Items.AddHeader('AddText - VertAlign = Middle');
  AItem := ksVirtualListView1.Items.Add;
  AItem.AddText(0, 0, 'AddText 0, 0').VertAlign := TVerticalAlignment.taVerticalCenter;
  AItem.AddText(0, 16, 'AddText 0, 16').VertAlign := TVerticalAlignment.taVerticalCenter;
  AItem.AddText(0, -16, 'AddText 0, -16').VertAlign := TVerticalAlignment.taVerticalCenter;
end;

end.
