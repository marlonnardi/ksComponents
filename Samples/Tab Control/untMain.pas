unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Objects, ksTypes, ksTabControl, ksSegmentButtons, FMX.Layouts,
  ksVirtualListView;

type
  TForm24 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    Image1: TImage;
    ksTabControl1: TksTabControl;
    ksTabItem0: TksTabItem;
    ksVirtualListView1: TksVirtualListView;
    ksTabItem1: TksTabItem;
    Layout1: TLayout;
    ksSegmentButtons1: TksSegmentButtons;
    ksSegmentButtons2: TksSegmentButtons;
    ksSegmentButtons3: TksSegmentButtons;
    Button3: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ksSegmentButtons1Change(Sender: TObject);
    procedure ksSegmentButtons2Change(Sender: TObject);
    procedure ksSegmentButtons3SelectSegment(Sender: TObject; AIndex: Integer; AButton: TksSegmentButton);
    procedure Label1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form24: TForm24;

implementation

uses System.UIConsts;

{$R *.fmx}


procedure TForm24.Button2Click(Sender: TObject);
begin
  while ksTabControl1.Tabs.Count > 2 do
    ksTabControl1.Tabs.Delete(ksTabControl1.Tabs.Count-1);

end;

procedure TForm24.Button3Click(Sender: TObject);
var
  AIcon: TksTabItemIcon;
begin
  AIcon := TksTabItemIcon(Random(Ord(High(TksTabItemIcon))));
  ksTabControl1.AddTab.StandardIcon := AIcon;
end;

procedure TForm24.FormCreate(Sender: TObject);
var
  ICount: integer;
  AItem: TksVListItem;
begin
  Image1.Visible := False;
  ksVirtualListView1.BeginUpdate;
  try

    for ICount := 1 to 50 do
    begin
      AItem := ksVirtualListView1.Items.Add;
      AItem.Title.Text := 'Item: '+IntToStr(ICount);
      AItem.SubTitle.Text := 'some subtitle text';
      AItem.Detail.Text := 'some detail';

      //('Item: '+IntToStr(ICount), 'some subtitle text', 'some detail', atMore );
      AItem.Image.Bitmap := Image1.Bitmap;
      //AItem.Image.BadgeValue := ICount;
    end;
  finally
    ksVirtualListView1.EndUpdate;
  end;

end;

procedure TForm24.ksSegmentButtons1Change(Sender: TObject);
begin
  case ksSegmentButtons1.ItemIndex of
    0: ksTabControl1.Appearence.Theme := TksTabBarTheme.ksTbLightTabs;
    1: ksTabControl1.Appearence.Theme := TksTabBarTheme.ksTbDarkTabs;
  end;
end;

procedure TForm24.ksSegmentButtons2Change(Sender: TObject);
begin
  if ksSegmentButtons2.ItemIndex = 0 then
  begin
    ksTabControl1.Tabs[0].BadgeValue := 0;
    ksTabControl1.Tabs[1].BadgeValue := 0;
  end
  else
  begin
    ksTabControl1.Tabs[0].BadgeValue := 3;
    ksTabControl1.Tabs[1].BadgeValue := 8;
  end;
end;

procedure TForm24.ksSegmentButtons3SelectSegment(Sender: TObject; AIndex: Integer; AButton: TksSegmentButton);
begin
  case AIndex of
    0: ksTabControl1.TabPosition := ksTbpBottom;
    1: ksTabControl1.TabPosition := ksTbpTop;
    2: ksTabControl1.TabPosition := ksTbpNone;
  end;
end;

procedure TForm24.Label1Click(Sender: TObject);
begin
  Button3.StaysPressed := True;
  Button3.IsPressed := True;
  Button3.StaysPressed := False;

end;

end.


