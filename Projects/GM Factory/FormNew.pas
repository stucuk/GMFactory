unit FormNew;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFrmNew = class(TForm)
    WidthEdit: TEdit;
    Label1: TLabel;
    HeightEdit: TEdit;
    Label2: TLabel;
    Bevel1: TBevel;
    Button2: TButton;
    Button1: TButton;
    FrameEdit: TEdit;
    Label3: TLabel;
    procedure HeightEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    O : Boolean;
  end;

var
  FrmNew: TFrmNew;

implementation

{$R *.dfm}

procedure TFrmNew.HeightEditChange(Sender: TObject);
var
 X : Integer;
 S,
 T : AnsiString;
begin
 S := '';
 T := TEdit(Sender).Text;
 for X := 1 to Length(T) do
 if T[X] in ['0'..'9'] then
 S := S + T[X];

 if S <> T then
 TEdit(Sender).Text := S;
end;

procedure TFrmNew.FormShow(Sender: TObject);
begin
 O := False;
end;

procedure TFrmNew.Button2Click(Sender: TObject);
begin
 O := True;
 Close;
end;

procedure TFrmNew.Button1Click(Sender: TObject);
begin
 Close;
end;

end.
