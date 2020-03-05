unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ksTypes,
  ksVirtualListView, FMX.StdCtrls, ksToolBar, FMX.Controls.Presentation,
  FMX.Edit, ksListViewFilter, FMX.Objects, FMX.ScrollBox, FMX.Memo;

type
  TForm5 = class(TForm)
    ksListViewFilter1: TksListViewFilter;
    ksToolbar1: TksToolbar;
    ToolBar1: TToolBar;
    ksVirtualListView1: TksVirtualListView;
    Memo1: TMemo;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.FormCreate(Sender: TObject);
var
  ICount: integer;
  AType: string;
begin
  // list of names are stored in a hidden TMemo component

  ksVirtualListView1.BeginUpdate;
  try
    for ICount := 0 to Memo1.Lines.Count-1 do
    begin
      if ICount mod 2 = 1 then
        AType := 'Customer'
      else
        AType := 'Prospect';

      ksVirtualListView1.Items.Add(Memo1.Lines[ICount], '', AType, Image1.Bitmap, atMore);
    end;
  finally
    ksVirtualListView1.EndUpdate;
  end;
end;

end.
