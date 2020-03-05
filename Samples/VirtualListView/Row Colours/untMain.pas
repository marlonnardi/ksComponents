unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  ksTypes, ksVirtualListView, FMX.Controls.Presentation, ksToolBar;

type
  TForm5 = class(TForm)
    ksToolbar1: TksToolbar;
    ksVirtualListView1: TksVirtualListView;
    ToolBar1: TToolBar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

uses System.UIConsts;

{$R *.fmx}

procedure TForm5.FormCreate(Sender: TObject);
var
  AItem: TksVListItem;
begin
  AItem := ksVirtualListView1.Items.Add('RED', '', '', atMore);
  AItem.Background := claRed;
  AItem.Title.TextSettings.FontColor := claWhite;

  AItem := ksVirtualListView1.Items.Add('YELLOW', '', '', atMore);
  AItem.Background := claYellow;
  AItem.Title.TextSettings.FontColor := claBlack;

  AItem := ksVirtualListView1.Items.Add('GREEN', '', '', atMore);
  AItem.Background := claGreen;
  AItem.Title.TextSettings.FontColor := claWhite;

  AItem := ksVirtualListView1.Items.Add('BLUE', '', '', atMore);
  AItem.Background := claBlue;
  AItem.Title.TextSettings.FontColor := claWhite;


end;

end.
