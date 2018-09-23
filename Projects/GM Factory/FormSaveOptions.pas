unit FormSaveOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GMF, GM_File;

type
  TFrmSaveOptions = class(TForm)
    PreviewInnerPanel: TPanel;
    PreviewBack: TImage;
    PreviewImage: TImage;
    Panel1: TPanel;
    PaletteImg: TImage;
    HasSideCols: TCheckBox;
    Label1: TLabel;
    Bevel1: TBevel;
    Button2: TButton;
    Button1: TButton;
    Timer: TTimer;
    IsUnitCheck: TCheckBox;
    UnitFallCheck: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HasSideColsClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IsUnitCheckClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdatePreview(Full : Boolean);
  public
    { Public declarations }
    O : Boolean;
    TheGMF     : TGMF;
    GMFPal : TGMFPalette;
    ThePalette : TGM_Palette;
    TheGM      : TGM_File;
    procedure SetGM(Filename : AnsiString; GMFactory : TGMF);
  end;

var
  FrmSaveOptions: TFrmSaveOptions;

implementation

{$R *.dfm}

uses CheckerUnit, pngimage;

var
 CurFrame : Integer = 0;

procedure TFrmSaveOptions.Button1Click(Sender: TObject);
begin
 Close;
end;

procedure TFrmSaveOptions.Button2Click(Sender: TObject);
begin
 O := True;
 Close;
end;

procedure TFrmSaveOptions.FormShow(Sender: TObject);
begin
 O := False;
 UnitFallCheck.Enabled := IsUnitCheck.Checked;
end;

procedure TFrmSaveOptions.FormCreate(Sender: TObject);
begin
 TileImage(PreviewBack,CTB16x16);
 PreviewBack.Canvas.Brush.Style   := bsClear;
 TheGM                            := TGM_File.Create(GM_Type_Z);
 PreviewInnerPanel.DoubleBuffered := True;
end;

procedure UpdatePaletteImg(Img : TImage; ThePalette : TGM_Palette);
var
 X,Y,C : Integer;
 BMP : TBitmap;
 W,H : Integer;
begin
 BMP             := Img.Picture.Bitmap;
 BMP.Width       := Img.Width;
 BMP.Height      := Img.Height;
 BMP.PixelFormat := pf24bit;

 W := BMP.Width  div 16;
 H := BMP.Height div 16;

 C := 0;
 for X := 0 to 15 do
 for Y := 0 to 15 do
 begin
  BMP.Canvas.Brush.Color := RGB(ThePalette[C].rgbtRed,ThePalette[C].rgbtGreen,ThePalette[C].rgbtBlue);
  BMP.Canvas.FillRect(Rect(X*W,Y*H,X*W+W,Y*H+H));
  Inc(C);
 end;
end;

var
 PreviewBMP : TBitmap    = Nil;
 PreviewPNG : TPNGObject = Nil;

procedure TFrmSaveOptions.UpdatePreview(Full : Boolean);
var
 X,Y    : Integer;
 Frame,
 ZBuf   : PByte;          
 RGB    : PRGBTriple;
 A      : PByte;
 T      : Pointer;
begin
 if Full then
 begin
  GMFPal := TheGMF.MakePalette(HasSideCols.Checked,IsUnitCheck.Checked,UnitFallCheck.Checked);

  for X := 0 to 255 do
  ThePalette[X] := GMFPal[X];

  TheGM.Clear;
  TheGM.Width  := TheGMF.Width;
  TheGM.Height := TheGMF.Height;
  TheGM.AddFrames(TheGMF.FrameCount);

  if not Assigned(PreviewBMP) then
  begin
   PreviewBMP := TBitmap.Create;
   PreviewPNG := TPNGObject.Create;
   PreviewBMP.PixelFormat := pf24bit;
  end;

  PreviewBMP.Width  := TheGMF.Width;
  PreviewBMP.Height := TheGMF.Height;

  PreviewPNG.Assign(PreviewBMP);
  PreviewPNG.CreateAlpha;

  UpdatePaletteImg(PaletteImg,ThePalette);
 end;

 if TheGM.Frame[CurFrame].RGB = Nil then
 begin
  GetMem(ZBuf,TheGMF.Width*TheGMF.Height);
  Frame := TheGMF.GetFrameWithPalette(TheGMF.Frame[CurFrame],GMFPal,HasSideCols.Checked);
   TheGM.SetFrameRGB8(CurFrame,Frame,ZBuf);
  FreeMem(Frame);
  FreeMem(ZBuf);
 end;

 T := TheGM.GetFrameRGB(CurFrame,ThePalette);
 RGB := T;
  for Y := 0 to TheGMF.Height-1 do
  begin
   CopyMemory(PreviewPNG.Scanline[Y],RGB,TheGMF.Width*3);
   Inc(Cardinal(RGB),TheGMF.Width*3);
  end;
 FreeMem(T);

 T := TheGM.GetFrameTrans(CurFrame);
 A := T;
  for Y := 0 to TheGMF.Height-1 do
  begin
   CopyMemory(PreviewPNG.AlphaScanline[Y],A,TheGMF.Width);
   Inc(Cardinal(A),TheGMF.Width);
  end;
 FreeMem(T);

 PreviewImage.Picture.Assign(PreviewPNG);
end;

procedure TFrmSaveOptions.SetGM(Filename : AnsiString; GMFactory : TGMF);
begin
 Label1.Caption := ExtractFileName(Filename);
 TheGMF         := GMFactory;
 CurFrame       := 0;

 PreviewImage.Stretch      := (TheGMF.Width > PreviewImage.Width) or (TheGMF.Height > PreviewImage.Height);
 PreviewImage.Proportional := PreviewImage.Stretch;

 UpdatePreview(True);

 Timer.Enabled := True;
end;

procedure TFrmSaveOptions.HasSideColsClick(Sender: TObject);
begin
 UpdatePreview(True);
end;

procedure TFrmSaveOptions.TimerTimer(Sender: TObject);
begin
 Inc(CurFrame);
 if CurFrame >= TheGMF.FrameCount then
 CurFrame := 0;
 
 UpdatePreview(False);
end;

procedure TFrmSaveOptions.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Timer.Enabled := False;
end;

procedure TFrmSaveOptions.IsUnitCheckClick(Sender: TObject);
begin
 UpdatePreview(True);

 UnitFallCheck.Enabled := IsUnitCheck.Checked;
end;

end.
