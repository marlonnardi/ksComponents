program SlideMenuMultiForm;

uses
  System.StartUpCopy,
  FMX.Forms,
  untForm1 in 'untForm1.pas' {Form1},
  untForm2 in 'untForm2.pas' {Form2},
  untForm3 in 'untForm3.pas' {Form3},
  untMenu in 'untMenu.pas' {dmMenu: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TdmMenu, dmMenu);
  Application.Run;
end.
