unit untMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ksPinCode,
  ksTypes, ksSegmentButtons;

type
  TForm4 = class(TForm)
    ksPinCode1: TksPinCode;
    procedure ksPinCode1Submit(Sender: TObject; ACode: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.ksPinCode1Submit(Sender: TObject; ACode: string);
begin
  ShowMessage(ACode);
  ksPinCode1.Reset;
end;

end.
