unit FormResize;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin;

type
  TFrmResize = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    BottomValue: TSpinEdit;
    LeftValue: TSpinEdit;
    RightValue: TSpinEdit;
    TopValue: TSpinEdit;
    Button1: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    O : Boolean;
  end;

var
  FrmResize: TFrmResize;

implementation

{$R *.dfm}

procedure TFrmResize.FormCreate(Sender: TObject);
begin
 Panel1.Left := ClientWidth div 2 - Panel1.Width div 2;
end;

procedure TFrmResize.FormShow(Sender: TObject);
begin
 TopValue.Value    := 0;
 BottomValue.Value := 0;
 LeftValue.Value   := 0;
 RightValue.Value  := 0;
 O := False;
end;

procedure TFrmResize.Button1Click(Sender: TObject);
begin
 O := True;
 Close;
end;

procedure TFrmResize.Button2Click(Sender: TObject);
begin
 Close;
end;

end.
