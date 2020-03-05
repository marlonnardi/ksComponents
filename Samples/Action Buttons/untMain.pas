unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Objects, ksTypes, ksVirtualListView;

type
  TForm24 = class(TForm)
    ToolBar2: TToolBar;
    ToolBar1: TToolBar;
    Label1: TLabel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    ksVirtualListView1: TksVirtualListView;
    procedure FormCreate(Sender: TObject);
    procedure ksVirtualListView1ActionButtonClick(Sender: TObject;
      ARow: TksVListItem; AButton: TksVListActionButton);
    procedure ksVirtualListView1ItemSwipe(Sender: TObject; ARow: TksVListItem;
      ASwipeDirection: TksVListSwipeDirection; AButtons: TksVListActionButtons);
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

procedure TForm24.FormCreate(Sender: TObject);
var
  ICount: integer;
  AItem: TksVListItem;
begin
  Image2.Visible := False;
  Image3.Visible := False;

  AAccessories.Images[atUserDefined1].SetBitmap(Image2.Bitmap);
  AAccessories.Images[atUserDefined2].SetBitmap(Image3.Bitmap);

  Image1.Visible := False;

    for ICount := 1 to 6 do
    begin
      AItem := ksVirtualListView1.Items.Add('Item: '+IntToStr(ICount), 'some subtitle text', 'some detail', atMore );
      AItem.Image.Bitmap := Image1.Bitmap;
      //AItem.AddSwitch(0, True);
    end;
    AItem := ksVirtualListView1.Items.Add('User defined', 'with custom images', 'some detail', atMore );
    AItem.Image.Bitmap := Image1.Bitmap;
end;

procedure TForm24.ksVirtualListView1ActionButtonClick(Sender: TObject;
  ARow: TksVListItem; AButton: TksVListActionButton);
begin
  // show a message for the button clicked...
  ShowMessage('You clicked on: '+AButton.Text);

end;

procedure TForm24.ksVirtualListView1ItemSwipe(Sender: TObject;
  ARow: TksVListItem; ASwipeDirection: TksVListSwipeDirection;
  AButtons: TksVListActionButtons);
begin
  if ASwipeDirection = ksSwipeFromRight then
  begin
    if ARow.Index = 3 then
    begin
      // user defined action button image for the last row
      AButtons.AddButton('Like', claSilver, claWhite, atUserDefined1);
      AButtons.AddButton('Lock', claOrange, claWhite, atUserDefined2);
      //AButtons.AddButton('Flag', claOrange, claWhite, atUserDefined2);
    end
    else
    begin
      AButtons.AddButton('More', claSilver, claWhite, atEllipses);
      AButtons.AddButton('Flag', claOrange, claWhite, atFlag);
    end;
  end
  else
  begin
    AButtons.AddButton('Reply', claDodgerblue, claWhite, atCompose)
  end;
end;

end.
