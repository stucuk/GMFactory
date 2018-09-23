unit FormChangeOffset;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TFrmChangeOffset = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    Bevel1: TBevel;
    Timer: TTimer;
    Image2: TImage;
    XOff: TEdit;
    YOff: TEdit;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure XOffChange(Sender: TObject);
    procedure YOffChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure XOffKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    O : Boolean;
    procedure LoadGM(Filename : AnsiString);
  end;

var
  FrmChangeOffset: TFrmChangeOffset;

implementation

{$R *.dfm}

uses GM_File, OW_Palette, pngimage;

var
 GM       : TGM_File = Nil;
 GM2      : TGM_File = Nil;
 Palette  : TGM_Palette;
 CurFrame : Integer = 0;
 FN : AnsiString;

procedure TFrmChangeOffset.LoadGM(Filename : AnsiString);
begin
 FN := Filename;

 if Assigned(GM) then
 GM.Free;

 if Assigned(GM2) then
 GM2.Free;

 GM := TGM_File.Create(0);
 GM.LoadFromFile(Filename);
 GM2 := TGM_File.Create(0);
 GM2.LoadFromFile(Filename);

 LoadOWPalette(ChangeFileExt(Filename,'.p16'),Palette);

 CurFrame := 0;

 O := False;
end;

procedure TFrmChangeOffset.Button1Click(Sender: TObject);
begin
 GM2.SaveToFile(FN);

 O := True;
 Close;
end;

procedure TFrmChangeOffset.Button2Click(Sender: TObject);
begin
 Close;
end;

procedure TFrmChangeOffset.FormShow(Sender: TObject);
begin
 Timer.Enabled := True;
end;

procedure UpdateOffsets();
var
 F : Integer;
 FrameA,
 FrameB : PGM_Frame;
 X,Y   : SmallInt;
begin
 with FrmChangeOffset do
 begin
  X := StrToIntDef(XOff.Text,0);
  Y := StrToIntDef(YOff.Text,0);
 end;

 for F := 0 to GM2.FrameCount-1 do
 begin
  FrameA := GM.Frame[F];
  FrameB := GM2.Frame[F];

  FrameB.Offset[0] := FrameA.Offset[0] + X;
  FrameB.Offset[1] := FrameA.Offset[1] + Y;
 end;

 GM2.WorkOutDimentions;
end;

procedure TFrmChangeOffset.XOffChange(Sender: TObject);
begin
 UpdateOffsets();
end;

procedure TFrmChangeOffset.YOffChange(Sender: TObject);
begin
 UpdateOffsets();
end;

var
 TransPNG : TPNGObject = Nil;
 BMP      : TBitmap = Nil;

procedure GetImageFrame(GM : TGM_File; Frame : Integer);
var
 RGB,
 RGBT   : PRGBTriple;
 Alpha,
 AlphaT : PByte;
 Y : Integer;
begin
 if not Assigned(TransPNG) then
 begin
  TransPNG := TPNGObject.Create;
  BMP      := TBitmap.Create;
 end;

 RGB    := GM.GetFrameRGB(Frame,Palette);
 RGBT   := RGB;
 Alpha  := GM.GetFrameTrans(Frame);
 AlphaT := Alpha;

 BMP.Width  := GM.Width;
 BMP.Height := GM.Height;
 TransPNG.Assign(BMP);
 TransPNG.CreateAlpha;

 for Y := 0 to GM.Height-1 do
 begin
  CopyMemory(TransPNG.Scanline[Y],RGBT,GM.Width*3);
  Inc(Cardinal(RGBT),GM.Width*3);
  CopyMemory(TransPNG.AlphaScanline[Y],AlphaT,GM.Width);
  Inc(Cardinal(AlphaT),GM.Width);
 end;

 FreeMem(RGB);
 FreeMem(Alpha);
end;

procedure TFrmChangeOffset.TimerTimer(Sender: TObject);
begin
 GetImageFrame(GM,CurFrame);
 Image1.Picture.Assign(TransPNG);

 GetImageFrame(GM2,CurFrame);
 Image2.Picture.Assign(TransPNG);

 Inc(CurFrame);
 if CurFrame >= GM.FrameCount then
 CurFrame := 0;
end;

procedure TFrmChangeOffset.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 Timer.Enabled := False;

 if Assigned(GM) then
 GM.Free;
 if Assigned(GM2) then
 GM2.Free;
 GM  := Nil;
 GM2 := Nil;

 CanClose := True;
end;

procedure TFrmChangeOffset.FormCreate(Sender: TObject);
begin
 DoubleBuffered := True;
end;

procedure TFrmChangeOffset.XOffKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key > #13) and not (Key in ['0'..'9']) then
 Key := #0;
end;

end.
