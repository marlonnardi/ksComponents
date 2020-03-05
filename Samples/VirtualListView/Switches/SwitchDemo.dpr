program SwitchDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  untMain in 'untMain.pas' {Form99};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.CreateForm(TForm99, Form99);
  Application.Run;
end.
