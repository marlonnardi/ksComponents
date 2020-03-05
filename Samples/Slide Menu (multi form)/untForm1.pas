unit untForm1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Objects, ksTypes, FMX.TabControl, ksSlideMenu, ksToolBar;

type
  TForm1 = class(TForm)
    ToolBar2: TToolBar;
    Image1: TImage;
    ksToolbar1: TksToolbar;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure ksToolbar1MenuButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses System.UIConsts, untMenu;

{$R *.fmx}

procedure TForm1.ksToolbar1MenuButtonClick(Sender: TObject);
begin
  dmMenu.menuLeft.OpenMenu(Self);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  dmMenu.menuRight.OpenMenu(Self, mpRight);
end;

end.
