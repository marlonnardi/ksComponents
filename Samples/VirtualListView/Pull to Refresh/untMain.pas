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
    btnClear: TButton;
    procedure btnClearClick(Sender: TObject);
    procedure ksVirtualListView1PullRefresh(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.btnClearClick(Sender: TObject);
begin
  ksVirtualListView1.ClearItems;
end;

procedure TForm5.ksVirtualListView1PullRefresh(Sender: TObject);
begin
  ksVirtualListView1.Items.Add('Item '+IntToStr(ksVirtualListView1.Items.Count+1), '', '', atMore);
end;

end.
