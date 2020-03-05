unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Objects, ksTypes, FMX.TabControl, ksSlideMenu, ksToolBar;

type
  TForm24 = class(TForm)
    TabControl1: TTabControl;
    ksSlideMenu1: TksSlideMenu;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Image1: TImage;
    Label2: TLabel;
    Image2: TImage;
    Label3: TLabel;
    Image3: TImage;
    Label4: TLabel;
    ToolBar2: TToolBar;
    ksToolbar1: TksToolbar;
    procedure FormCreate(Sender: TObject);
    procedure ksSlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
    procedure ksSlideMenu1BuildMenu(Sender: TObject;
      AItems: TksSlideMenuItemList);
    procedure ksToolbar1MenuButtonClick(Sender: TObject);
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
begin
  TabControl1.TabPosition := TTabPosition.None;

end;

procedure TForm24.ksSlideMenu1BuildMenu(Sender: TObject;
  AItems: TksSlideMenuItemList);
begin
  AItems.AddItem('PAGE_1','First Screen', nil);
  AItems.AddItem('PAGE_2','Second Screen', nil);
  AItems.AddItem('PAGE_3','Last Screen', nil);
end;

procedure TForm24.ksSlideMenu1SelectMenuItemEvent(Sender: TObject; AId: string);
begin
  if AId = 'PAGE_1' then TabControl1.TabIndex := 0;
  if AId = 'PAGE_2' then TabControl1.TabIndex := 1;
  if AId = 'PAGE_3' then TabControl1.TabIndex := 2;

end;

procedure TForm24.ksToolbar1MenuButtonClick(Sender: TObject);
begin
  ksSlideMenu1.OpenMenu(Self);
end;

end.
