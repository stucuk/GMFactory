unit FormAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, pngimage;

type
  TFrmAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    VersionLabel: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OkButton: TButton;
    Label2: TLabel;
    procedure Label5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmAbout: TFrmAbout;

implementation

{$R *.dfm}

uses ShellAPI, FormMain;

procedure TFrmAbout.Label5Click(Sender: TObject);
begin
 ShellExecute(Application.Handle, PChar('open'), PChar(TLabel(Sender).Hint), nil, nil, SW_SHOW);
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
 VersionLabel.Caption := 'Version: ' + APP_VER;
 OkButton.Left := ClientWidth div 2 - OkButton.Width div 2;
end;

procedure TFrmAbout.OkButtonClick(Sender: TObject);
begin
 Close;
end;

end.
