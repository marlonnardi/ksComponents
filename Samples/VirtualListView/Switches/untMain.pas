unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ksTypes, ksVirtualListView,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, ksToolBar, ksTableView;

type
  TForm99 = class(TForm)
    ksVirtualListView1: TksVirtualListView;
    ksToolbar1: TksToolbar;
    procedure FormCreate(Sender: TObject);
    procedure ksVirtualListView1ItemSwitchClick(Sender: TObject;
      AItem: TksVListItem; ASwitchID: string; AChecked: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form99: TForm99;

implementation

{$R *.fmx}

procedure TForm99.FormCreate(Sender: TObject);
var
  AItem: TksVListItem;
  ICount: integer;
begin
  ksVirtualListView1.BeginUpdate;
  for ICount := 1 to 100 do
  begin
    AItem := ksVirtualListView1.Items.Add('SWITCH '+IntToStr(ICount), '', '');
    AItem.AddSwitch(0, 0, True);
  end;
  ksVirtualListView1.EndUpdate;
end;

procedure TForm99.ksVirtualListView1ItemSwitchClick(Sender: TObject;
  AItem: TksVListItem; ASwitchID: string; AChecked: Boolean);
begin
  ksToolbar1.Text := 'SWITCH CLICKED - Index: '+IntToStr(AItem.Index);
end;

end.
