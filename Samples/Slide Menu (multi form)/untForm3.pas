unit untForm3;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Objects, ksTypes, ksToolBar;

type
  TForm3 = class(TForm)
    ToolBar2: TToolBar;
    Image3: TImage;
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
  Form3: TForm3;

implementation

uses System.UIConsts, untMenu, ksSlideMenu;

{$R *.fmx}

procedure TForm3.ksToolbar1MenuButtonClick(Sender: TObject);
begin
  dmMenu.menuLeft.OpenMenu(Self, mpLeft);
end;

procedure TForm3.SpeedButton1Click(Sender: TObject);
begin
  dmMenu.menuRight.OpenMenu(Self, mpRight);
end;

end.
