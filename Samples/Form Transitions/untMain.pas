unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  ksFormTransition, FMX.StdCtrls, FMX.Objects,
  FMX.Controls.Presentation, FMX.Edit, ksTypes, ksVirtualListView, ksToolBar;

type
  TForm3 = class(TForm)
    ToolBar2: TToolBar;
    Image1: TImage;
    ksVirtualListView1: TksVirtualListView;
    ksFormTransition1: TksFormTransition;
    ksToolbar1: TksToolbar;
    procedure FormCreate(Sender: TObject);
    procedure ksVirtualListView1ItemClick(Sender: TObject; AItem: TksVListItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses untOtherForm, ksChatView;

{$R *.fmx}

procedure TForm3.FormCreate(Sender: TObject);
var
  AIcon: TBitmap;
begin
  {$IFDEF MSWINDOWS}
  Width := FormFactor.Width;
  Height := FormFactor.Height;
  Left := 360;
  Top := 150;
  {$ENDIF}
  AIcon := Image1.Bitmap;
  Image1.Visible := False;
  ksVirtualListView1.Items.Add('Slide in right', 'Form Transition', 'Click to view', atMore).Image.Bitmap := AIcon;
  ksVirtualListView1.Items.Add('Slide in bottom', 'Form Transition', 'Click to view', atMore).Image.Bitmap := AIcon;
  ksVirtualListView1.Items.Add('Slide in top', 'Form Transition', 'Click to view', atMore).Image.Bitmap := AIcon;
  ksVirtualListView1.Items.Add('Slide in left', 'Form Transition', 'Click to view', atMore).Image.Bitmap := AIcon;
  //SetPassCodeRequireType(TksPassCodeRequireType.ksRequireOnActivate);
end;

procedure TForm3.ksVirtualListView1ItemClick(Sender: TObject;
  AItem: TksVListItem);
begin
  case AItem.Index of
    0: Push(Form5, ksFtSlideInFromRight);
    1: Push(Form5, ksFtSlideInFromBottom);
    2: Push(Form5, ksFtSlideInFromTop);
    3: Push(Form5, ksFtSlideInFromLeft);
  end;
end;

end.
