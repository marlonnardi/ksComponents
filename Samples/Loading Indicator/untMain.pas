unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TForm99 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbFade: TCheckBox;
    Button3: TButton;
    timerModal: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure timerModalTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form99: TForm99;

implementation

uses ksLoadingIndicator;

{$R *.fmx}

procedure TForm99.Button1Click(Sender: TObject);
begin

  ShowLoadingIndicator(Self, cbFade.IsChecked);
end;

procedure TForm99.Button2Click(Sender: TObject);
begin
  HideLoadingIndicator(Self);
end;

procedure TForm99.Button3Click(Sender: TObject);
begin
  ShowLoadingIndicator(Self, cbFade.IsChecked, True);
  timerModal.Enabled := True;
end;

procedure TForm99.timerModalTimer(Sender: TObject);
begin
  HideLoadingIndicator(Self);
  timerModal.Enabled := False;
end;

end.
