program ItemSegmentButtons;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in 'untMain.pas' {Form106};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm106, Form106);
  Application.Run;
end.
