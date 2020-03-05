unit untMenu;

interface

uses
  System.SysUtils, System.Classes, ksSlideMenu;

type
  TdmMenu = class(TDataModule)
    menuLeft: TksSlideMenu;
    menuRight: TksSlideMenu;
    procedure menuLeftBuildMenu(Sender: TObject;
      AItems: TksSlideMenuItemList);
    procedure menuRightBuildMenu(Sender: TObject; AItems: TksSlideMenuItemList);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmMenu: TdmMenu;

implementation

uses untForm1, untForm2, untForm3, ksTypes;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TdmMenu.menuLeftBuildMenu(Sender: TObject;
  AItems: TksSlideMenuItemList);
begin
  AItems.AddItem('','First Form', Form1, TksStandardIcon.Calendar);
  AItems.AddItem('','Second Form', Form2, TksStandardIcon.BarChart);
  AItems.AddItem('','Third Form', Form3, TksStandardIcon.Settings);

end;

procedure TdmMenu.menuRightBuildMenu(Sender: TObject;
  AItems: TksSlideMenuItemList);
begin
  AItems.AddItem('','Right aligned menu', Form1, TksStandardIcon.Calendar);
  AItems.AddItem('','Item 2', Form2, TksStandardIcon.BarChart);
  AItems.AddItem('','Item 3', Form3, TksStandardIcon.Settings);

end;

end.
