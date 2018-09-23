unit FormChangeExtraData;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, PngSpeedButton;

type
  TFrmChangeExtraData = class(TForm)
    ScrollBox1: TScrollBox;
    Image1: TImage;
    ExtraDataList: TListBox;
    EDX: TEdit;
    EDY: TEdit;
    EDZ: TEdit;
    EDAddButton: TPngSpeedButton;
    EDDeleteButton: TPngSpeedButton;
    Label1: TLabel;
    Label3: TLabel;
    StartFrameLabel: TLabel;
    CountLabel: TLabel;
    Button2: TButton;
    Button1: TButton;
    Bevel1: TBevel;
    ZoomCombo: TComboBox;
    Label2: TLabel;
    Button3: TButton;
    PixelLabel: TLabel;
    EDEditButton: TPngSpeedButton;
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EDAddButtonClick(Sender: TObject);
    procedure ExtraDataListClick(Sender: TObject);
    procedure ExtraDataListKeyPress(Sender: TObject; var Key: Char);
    procedure EDDeleteButtonClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure EDEditButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Load(Filename1 : AnsiString);
  end;

var
  FrmChangeExtraData: TFrmChangeExtraData;

implementation

{$R *.dfm}

uses GM_File, GMF, pngimage, OW_Palette, Math;

var
 GM     : TGM_File   = Nil;
 GMFile : TGMF       = Nil;
 PNG1   : TPNGObject = Nil;
 GMPal  : TGM16_Palette;
 FNGMZ  : AnsiString;

procedure SetPNGWH(PNG : TPNGObject; W,H : Integer);
var
 BMP : TBitmap;
begin
 BMP := TBitmap.Create;
  BMP.Width  := W;
  BMP.Height := H;
  BMP.Canvas.Brush.Color := clBlack;
  BMP.Canvas.FillRect(Rect(0,0,W,H));
  BMP.Canvas.Brush.Color := clSilver;
  BMP.Canvas.FillRect(Rect(0,0,W,H-20));
  PNG.Assign(BMP);
 BMP.Free;
end;

procedure TextOut(Canvas : TCanvas; Text : AnsiString; X,W,H,TH2 : Integer);
begin
 Canvas.TextOut(X+(W div 2)-(Canvas.TextWidth(Text) div 2),H-10-TH2,Text);
end;

procedure AddFrameBars(PNG : TPNGObject; W,C : Integer);
var
 X,TH2 : Integer;
begin
 PNG.Canvas.Brush.Color := clBlack;
 PNG.Canvas.Font.Color := clWhite;
 PNG.Canvas.FillRect(Rect(0,0,2,PNG.Height));

 TH2 := PNG.Canvas.TextHeight('0123456789') div 2;

 for X := 0 to C-1 do
 begin
  PNG.Canvas.FillRect(Rect((X+1)*(W+4)-2,0,(X+1)*(W+4),PNG.Height));
  TextOut(PNG.Canvas,IntToStr(X),X*(W+4),W+4,PNG.Height,TH2);
 end;
end;

var
 GMFZ       : PByte = Nil;
 GMZSideCol : PByte = Nil;

procedure SetGMFFrame(Index : Integer; RGB16 : PWORD; Trans : PBYTE; ExtraData : Pointer = Nil; ExtraDataSize : Integer = 0);
var
 Frame : PGMFFrame;
 Layer : PGMFLayer;
begin
 Frame := GMFile.Frame[Index];
 Frame.Edited := True;

 if Assigned(Frame.SideCol) then
  ZeroMemory(Frame.SideCol,GMFile.Width*GMFile.Height);

 if not Assigned(Frame.ZBuff) then
  GetMem(Frame.ZBuff,GMFile.Width*GMFile.Height);

 ZeroMemory(Frame.ZBuff,GMFile.Width*GMFile.Height);

 Layer := GMFile.GetLayer(Frame,1);
 GMFile.SetupLayer(Layer);

 CopyMemory(Layer.RGB16,RGB16,GMFile.Width*GMFile.Height*2);
 CopyMemory(Layer.Trans,Trans,GMFile.Width*GMFile.Height);

 if Assigned(ExtraData) then
 begin
  if Assigned(Frame.ExtraData) then
  FreeMem(Frame.ExtraData);

  Frame.ExtraDataSize := ExtraDataSize-4;

  if (ExtraDataSize > 4) then
  begin
   GetMem(Frame.ExtraData,Frame.ExtraDataSize);
   CopyMemory(Frame.ExtraData,Pointer(Cardinal(ExtraData)+4),Frame.ExtraDataSize);
  end
  else
   Frame.ExtraData := Nil;
 end;

 if Assigned(GMFZ) then
 begin
  CopyMemory(Frame.ZBuff,GMFZ,GMFile.Width*GMFile.Height);
  FreeMem(GMFZ);
  GMFZ := Nil;
 end;

 if Assigned(GMZSideCol) then
 begin
  if not Assigned(Frame.SideCol) then
  GetMem(Frame.SideCol,GMFile.Width*GMFile.Height);
  CopyMemory(Frame.SideCol,GMZSideCol,GMFile.Width*GMFile.Height);
  FreeMem(GMZSideCol);
  GMZSideCol := Nil;
 end;
end;

procedure CopyFramesFromGM(Start,Count,Dest : Integer);
var
 X : Integer;
 RGB16 : PWORD;
 Trans : PByte;
begin
 for X := 0 to Count-1 do
 begin
  RGB16 := GM.GetFrameRGB16(Start+X,GMPal);
   Trans := GM.GetFrameTrans(Start+X);

    GMZSideCol := GM.GetFrameSideCol(Start+X);
    GMFZ := GM.GetFrameZBuffer(Start+X);

    SetGMFFrame(Dest+X,RGB16,Trans,GM.Frame[Start+X].ExtraData,GM.Frame[Start+X].ExtraDataSize);
   FreeMem(Trans);
  FreeMem(RGB16);
 end;
end;

procedure UpdateGMFPNG(Count,Dest : Integer);
var
 PNG : TPNGObject;
 X : Integer;
begin
 PNG1.Canvas.Brush.Color := clSilver;

 PNG := TPNGObject.Create;
 for X := 0 to Count-1 do
 begin
  PNG1.Canvas.FillRect(Rect(2+(Dest+X)*(GMFile.Width+4),0,-2+(Dest+X+1)*(GMFile.Width+4),GMFile.Height));
  GMFile.LayerToPNG(GMFile.Frame[Dest+X],PNG,0);
  PNG1.Canvas.Draw(2+(Dest+X)*(GMFile.Width+4),0,PNG);
 end;
 PNG.Free;

 FrmChangeExtraData.Image1.Picture.Assign(PNG1);
end;

var
 StartF,
 CountF : Integer;

procedure UpdateLabels;
begin
 with FrmChangeExtraData do
 begin
  StartFrameLabel.Caption := IntToStr(StartF);
  CountLabel.Caption      := IntToStr(CountF);
 end;
end;

procedure TFrmChangeExtraData.Load(Filename1 : AnsiString);
var
 X : Integer;
begin
 FNGMZ := Filename1;

 if Assigned(GM) then
 GM.Free;
 if Assigned(GMFile) then
 GMFile.Free;

 GM := TGM_File.Create(GM_Type_Z);
 GM.LoadFromFile(Filename1);

 LoadOWPalette16(ChangeFileExt(Filename1,'.p16'),GMPal);

 GMFile := TGMF.Create(GM.Width,GM.Height);
 GMFile.AddFrame(GM.FrameCount);

 if not Assigned(PNG1) then
 PNG1 := TPNGObject.Create;

 SetPNGWH(PNG1,(GMFile.Width+4)*GMFile.FrameCount,GMFile.Height+20);

 AddFrameBars(PNG1,GMFile.Width,GMFile.FrameCount);

 CopyFramesFromGM(0,GM.FrameCount,0);

 UpdateGMFPNG(GM.FrameCount,0);

 StartF  := 0;
 CountF  := 1;
 UpdateLabels;
end;

procedure TFrmChangeExtraData.Image1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if (X < 0) then X := 32767+(32767+X);

 X := Abs(X) div (GMFile.Width+4);

 if (X < 0) or (X >= GMFile.FrameCount) then Exit;

 if Button = mbLeft then
  StartF := X
 else
  CountF := (X-StartF)+1;

 UpdateLabels;
end;

procedure TFrmChangeExtraData.Image1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if (X < 0) then X := 32767+(32767+X);

 X := (Abs(X) mod (GMFile.Width+4));

// X := X-2;

 PixelLabel.Caption := IntToStr(X) + ',' + IntToStr(Y);
end;

procedure TFrmChangeExtraData.EDAddButtonClick(Sender: TObject);
begin
 ExtraDataList.Items.Add(EDX.Text+','+EDY.Text+','+EDZ.Text);
end;

procedure TFrmChangeExtraData.ExtraDataListClick(Sender: TObject);
begin
 EDDeleteButton.Enabled := (ExtraDataList.ItemIndex > -1) and (ExtraDataList.ItemIndex < ExtraDataList.Count);
 EDEditButton.Enabled   := EDDeleteButton.Enabled;
end;

procedure TFrmChangeExtraData.ExtraDataListKeyPress(Sender: TObject;
  var Key: Char);
begin
 EDDeleteButton.Enabled := (ExtraDataList.ItemIndex > -1) and (ExtraDataList.ItemIndex < ExtraDataList.Count);
 EDEditButton.Enabled   := EDDeleteButton.Enabled;
end;

procedure TFrmChangeExtraData.EDDeleteButtonClick(Sender: TObject);
begin
 ExtraDataList.DeleteSelected;
 EDDeleteButton.Enabled := (ExtraDataList.ItemIndex > -1) and (ExtraDataList.ItemIndex < ExtraDataList.Count);
 EDEditButton.Enabled   := EDDeleteButton.Enabled;
end;

procedure GetXYZ(Input : AnsiString; var X,Y,Z : SmallInt);
var
 P : Integer;
begin
 P := Pos(',',Input);
 X := StrToIntDef(Copy(Input,1,P-1),0);
 Delete(Input,1,P);
 P := Pos(',',Input);
 Y := StrToIntDef(Copy(Input,1,P-1),0);
 Delete(Input,1,P);
 Z := StrToIntDef(Input,0);
end;

procedure TFrmChangeExtraData.Button3Click(Sender: TObject);
var
 F,FS,FE,
 ED      : Integer;
 Frame   : PGMFFrame;
 D       : PSmallInt;
 X,Y,Z   : SmallInt;
begin
 FS := Min(Max(StartF,0),GMFile.FrameCount-1);
 FE := Min(FS + Max(CountF-1,0),GMFile.FrameCount-1);

 for F := FS to FE do
 begin
  Frame := GMFile.Frame[F];
  if Assigned(Frame.ExtraData) then
  begin
   FreeMem(Frame.ExtraData);
   Frame.ExtraData     := Nil;
   Frame.ExtraDataSize := 0;
  end;

  if ExtraDataList.Count = 0 then
  Continue;

  Frame.ExtraDataSize := ExtraDataList.Count*6;
  GetMem(Frame.ExtraData,Frame.ExtraDataSize);

  D := Frame.ExtraData;
  for ED := 0 to ExtraDataList.Count-1 do
  begin
   GetXYZ(ExtraDataList.Items[ED],X,Y,Z);
   D^ := X-GMFile.Width div 2;
   Inc(Cardinal(D),2);
   D^ := Y-GMFile.Height div 2;
   Inc(Cardinal(D),2);
   D^ := Z;
   Inc(Cardinal(D),2);
  end;
 end;
end;

procedure TFrmChangeExtraData.EDEditButtonClick(Sender: TObject);
var
 I : Integer;
begin
 I                       := ExtraDataList.ItemIndex;
 ExtraDataList.Items[I]  := EDX.Text+','+EDY.Text+','+EDZ.Text;
 ExtraDataList.ItemIndex := I;
end;

procedure TFrmChangeExtraData.Button2Click(Sender: TObject);
begin
 GMFile.SaveToFile(ChangeFileExt(FNGMZ,'.gmf'));
 Close;
end;

procedure TFrmChangeExtraData.Button1Click(Sender: TObject);
begin
 Close;
end;

end.
