program ActionButtons;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  untMain in 'untMain.pas' {Form76};

{$R *.res}

begin
  GlobalUseSkia := True;
  GlobalUseMetal := True;
  Application.Initialize;
  Application.CreateForm(TForm76, Form76);
  Application.Run;
end.
