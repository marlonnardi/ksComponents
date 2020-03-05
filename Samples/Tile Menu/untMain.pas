unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ksTypes,
  ksTileMenu, FMX.StdCtrls, FMX.Controls.Presentation, ksToolBar, FMX.Objects,
  ksSegmentButtons;

type
  TForm6 = class(TForm)
    ksTileMenu1: TksTileMenu;
    ksToolbar1: TksToolbar;
    ToolBar1: TToolBar;
    img1: TImage;
    img2: TImage;
    img3: TImage;
    img4: TImage;
    ksSegmentButtons1: TksSegmentButtons;
    procedure FormCreate(Sender: TObject);
    procedure ksSegmentButtons1SelectSegment(Sender: TObject; AIndex: Integer;
      AButton: TksSegmentButton);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses System.UIConsts;

{$R *.fmx}

procedure TForm6.FormCreate(Sender: TObject);
begin
  ksTileMenu1.Items.AddOption('OPTION 1', 'OPT_1', claRed, claWhite, img1.Bitmap);
  ksTileMenu1.Items.AddOption('OPTION 2', 'OPT_2', claGreen, claWhite, img2.Bitmap);
  ksTileMenu1.Items.AddOption('OPTION 3', 'OPT_3', claYellow, claBlack, img3.Bitmap);
  ksTileMenu1.Items.AddOption('OPTION 4', 'OPT_3', claBlue, claWhite, img4.Bitmap);
end;

procedure TForm6.ksSegmentButtons1SelectSegment(Sender: TObject;
  AIndex: Integer; AButton: TksSegmentButton);
begin
  ksTileMenu1.TileOptions.TileColumns := AIndex+2;
end;

end.
