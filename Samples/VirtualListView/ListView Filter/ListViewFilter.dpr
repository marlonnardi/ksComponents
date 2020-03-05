program ListViewFilter;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in 'untMain.pas' {Form5};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
