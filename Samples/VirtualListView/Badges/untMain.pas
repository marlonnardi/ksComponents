unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ksTypes,
  ksVirtualListView, FMX.StdCtrls, FMX.Controls.Presentation, ksToolBar,
  FMX.Objects;

type
  TfrmMain = class(TForm)
    ksToolbar1: TksToolbar;
    ToolBar1: TToolBar;
    ksVirtualListView1: TksVirtualListView;
    imgUser: TImage;
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
  AItem := ksVirtualListView1.Items.Add('Item 1', '', '', imgUser.Bitmap);
  AItem.Image.Badge := 1;

  AItem := ksVirtualListView1.Items.Add('Item 2', '', '', imgUser.Bitmap);
  AItem.Image.Badge := 2;

  AItem := ksVirtualListView1.Items.Add('Item 3', '', '', imgUser.Bitmap);
  AItem.Image.Badge := 3;
end;

end.
