unit FormSHPImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SHP, GM_File, ExtCtrls, StdCtrls, Buttons, PngSpeedButton, pngimage,
  GMF, OW_Palette;

type
  TFrmSHPImport = class(TForm)
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ListBox: TListBox;
    PngSpeedButton1: TPngSpeedButton;
    DeleteButton: TPngSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StartFrameLabel: TLabel;
    CountLabel: TLabel;
    DestFrameLabel: TLabel;
    Image2: TImage;
    Button2: TButton;
    Button1: TButton;
    Count2Label: TLabel;
    Label5: TLabel;
    Image1: TImage;
    HasShadowsCheck: TCheckBox;
    PngSpeedButton2: TPngSpeedButton;
    OpenDialog: TOpenDialog;
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PngSpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PngSpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoList;
    procedure DoList_Shadows;
  public
    { Public declarations }
    procedure Load(Filename1,Filename2 : AnsiString);
  end;

var
  FrmSHPImport: TFrmSHPImport;

  ASHP : TSHP     = Nil;
  GM : TGM_File = Nil;
  GMFile : TGMF = Nil;
  PNG1 : TPNGObject = Nil;
  PNG2 : TPNGObject = Nil;
  GMPal : TGM16_Palette;

  FNGMZ : AnsiString;

implementation

{$R *.dfm}

uses Math;

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

function ExpandRGB16(Input : PWord; W,H : Word) : PWord;
var
 X,Y,OX,OY : Integer;
begin
 if (GMFile.Width = W) and (GMFile.Height = H) then
 begin
  Result := Input;
  Exit;
 end;

 OX := (GMFile.Width-W) div 2;
 OY := (GMFile.Height-H) div 2;

 GetMem(Result,GMFile.Width*GMFile.Height*2);
 ZeroMemory(Result,GMFile.Width*GMFile.Height*2);

 for Y := 0 to H-1 do
 begin
  if (Y+OY >= GMFile.Height) then
  Continue;

  CopyMemory(Pointer(Cardinal(Result)+((Y+OY)*GMFile.Width+OX)*2),Pointer(Cardinal(Input)+(Y*W)*2),W*2);
 end;

 FreeMem(Input);
end;

function ExpandTrans(Input : PByte; W,H : Word; FreeTrans : Boolean = True) : PByte;
var
 X,Y,OX,OY : Integer;
begin
 if (GMFile.Width = W) and (GMFile.Height = H) then
 begin
  if not FreeTrans then
  begin
   GetMem(Result,GMFile.Width*GMFile.Height);
   CopyMemory(Result,Input,GMFile.Width*GMFile.Height);
  end
  else
  Result := Input;

  Exit;
 end;

 OX := (GMFile.Width-W) div 2;
 OY := (GMFile.Height-H) div 2;

 GetMem(Result,GMFile.Width*GMFile.Height);
 ZeroMemory(Result,GMFile.Width*GMFile.Height);

 for Y := 0 to H-1 do
 begin
  if (Y+OY >= GMFile.Height) then
  Continue;

  CopyMemory(Pointer(Cardinal(Result)+((Y+OY)*GMFile.Width+OX)),Pointer(Cardinal(Input)+(Y*W)),W);
 end;

 if FreeTrans then
 FreeMem(Input);
end;

procedure CopyFramesFromGM(Start,Count,Dest : Integer);
var
 X : Integer;
 RGB16 : PWORD;
 Trans : PByte;
begin
 for X := 0 to Count-1 do
 begin
  RGB16 := ExpandRGB16(GM.GetFrameRGB16(Start+X,GMPal),GM.Width,GM.Height);
   Trans := ExpandTrans(GM.GetFrameTrans(Start+X),GM.Width,GM.Height);

    GMZSideCol := ExpandTrans(GM.GetFrameSideCol(Start+X),GM.Width,GM.Height);
    GMFZ := ExpandTrans(GM.GetFrameZBuffer(Start+X),GM.Width,GM.Height);

    SetGMFFrame(Dest+X,RGB16,Trans,GM.Frame[Start+X].ExtraData,GM.Frame[Start+X].ExtraDataSize);
   FreeMem(Trans);
  FreeMem(RGB16);
 end;
end;

function TripleToWord(Input : PRGBTriple; W,H : Integer) : PWord;
var
 X,Y : Integer;
 I : PRGBTriple;
 R : PWord;
begin
 GetMem(Result,W*H*2);
 I := Input;
 R := Result;
 for Y := 0 to H-1 do
 for X := 0 to W-1 do
 begin
  R^ := color_to_hicolor(I^);
  Inc(Cardinal(I),3);
  Inc(Cardinal(R),2);
 end;
end;

function RoundUp(Value : Extended) : Integer;
begin
 Result := Trunc(Value);
 if Result <> Value then
 Inc(Result);
end;

function GetID(X,Count,Count2 : Integer) : Integer;
begin
 if (Count = Count2) then
 begin
  Result := X;
  Exit;
 end;

 Result := RoundUp((X+1)/(Count2)*(Count))-1;
end;

var
 MinMax : Array [0..3] of Word;

procedure MakeZBuffer(Trans : PByte);
var
 Min : Word;
 X,Y : Integer;
 T,Z : PByte;
begin
 if not Assigned(Trans) then Exit;

 Min := GMFile.Height;

 T := Trans;
 for Y := 0 to GMFile.Height-1 do
 for X := 0 to GMFile.Width-1 do
 begin
  if (T^ > 0) then
  begin
   if GMFile.Height-Y < Min then
   Min := GMFile.Height-Y;
  end;
  Inc(Cardinal(T),1);
 end;

 GetMem(GMFZ,GMFile.Width*GMFile.Height);
 ZeroMemory(GMFZ,GMFile.Width*GMFile.Height);

 T := Trans;
 Z := GMFZ;
 for Y := 0 to GMFile.Height-1 do
 for X := 0 to GMFile.Width-1 do
 begin
  if (T^ > 0) then
   Z^ := RoundUp(((GMFile.Height-Y)-Min)/MinMax[3]*MinMax[1])+MinMax[0];
  Inc(Cardinal(T),1);
  Inc(Cardinal(Z),1);
 end;
end;

procedure CopyFramesFromSHP(Start,Count,Dest,Count2 : Integer);
var
 X,ID : Integer;
 RGB16 : PWORD;
 Trans : PByte;
begin
 for X := 0 to Count2-1 do
 begin
  ID := GetID(X,Count,Count2);

  RGB16 := ExpandRGB16(TripleToWord(ASHP.RGBFrame[Start+ID],ASHP.Width,ASHP.Height),ASHP.Width,ASHP.Height);
   Trans := ExpandTrans(ASHP.AlphaFrame[Start+ID],ASHP.Width,ASHP.Height,False);
   GMZSideCol := ExpandTrans(ASHP.SideColFrame[Start+ID],ASHP.Width,ASHP.Height,False);
    MakeZBuffer(Trans);
    SetGMFFrame(Dest+X,RGB16,Trans);
   FreeMem(Trans);
  FreeMem(RGB16);
 end;
end;

procedure UpdateGMFPNG(Count,Dest : Integer);
var
 PNG : TPNGObject;
 X : Integer;
begin
 PNG2.Canvas.Brush.Color := clSilver;

 PNG := TPNGObject.Create;
 for X := 0 to Count-1 do
 begin
  PNG2.Canvas.FillRect(Rect(2+(Dest+X)*(GMFile.Width+4),0,2+(Dest+X+1)*(GMFile.Width+4),GMFile.Height));
  GMFile.LayerToPNG(GMFile.Frame[Dest+X],PNG,0);
  PNG2.Canvas.Draw(2+(Dest+X)*(GMFile.Width+4),0,PNG);
 end;
 PNG.Free;

 FrmSHPImport.Image2.Picture.Assign(PNG2);
end;

procedure SHPToPNG(PNG : TPNGObject; RGB : PRGBTriple; Alpha : PByte; W,H : Integer);
var
 Y   : Integer;
 BMP : TBitmap;
begin
 BMP := TBitmap.Create;
  BMP.Width       := W;
  BMP.Height      := H;
  BMP.PixelFormat := pf24bit;

  for Y := 0 to H-1 do
  begin
   CopyMemory(BMP.ScanLine[Y],RGB,W*3);
   Inc(Cardinal(RGB),W*3);
  end;

  PNG.Assign(BMP);
 BMP.Free;

 if PNG.AlphaScanline[0] = Nil then
 PNG.CreateAlpha;

 for Y := 0 to H-1 do
 begin
  CopyMemory(PNG.AlphaScanline[Y],Alpha,W);
  Inc(Cardinal(Alpha),W);
 end;
end;

procedure UpdateSHPPNG();
var
 PNG : TPNGObject;
 X : Integer;
begin
 PNG := TPNGObject.Create;
 for X := 0 to ASHP.Count-1 do
 begin
  try
  SHPToPNG(PNG,ASHP.RGBFrame[X],ASHP.AlphaFrame[X],ASHP.Width,ASHP.Height);

  PNG1.Canvas.Draw(2+(X)*(ASHP.Width+4),0,PNG);
  except
  end;
 end;
 PNG.Free;

 FrmSHPImport.Image1.Picture.Assign(PNG1);
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
 StartF,
 CountF,
 Count2F,
 DestF : Integer;

procedure UpdateLabels;
begin
 with FrmSHPImport do
 begin
  StartFrameLabel.Caption := IntToStr(StartF);
  CountLabel.Caption      := IntToStr(CountF);
  DestFrameLabel.Caption  := IntToStr(DestF);
  Count2Label.Caption     := IntToStr(Count2F);
 end;
end;

procedure GetValuesFromList(Input : AnsiString; var Start,Count,Dest,Count2 : Integer);
var
 P : Integer;
begin
 P := Pos(',',Input);
 Start := StrToIntDef(Copy(Input,1,P-1),0);
 Delete(Input,1,P);
 P := Pos(',',Input);
 Count := StrToIntDef(Copy(Input,1,P-1),0);
 Delete(Input,1,P);
 P := Pos(',',Input);
 Dest := StrToIntDef(Copy(Input,1,P-1),0);
 Delete(Input,1,P);
 Count2 := StrToIntDef(Input,0);
end;

procedure TFrmSHPImport.DoList;
var
 X : Integer;
 Start,Count,Dest,Count2 : Integer;
begin
 for X := 0 to ListBox.Count-1 do
 begin
  GetValuesFromList(ListBox.Items[X],Start,Count,Dest,Count2);
  CopyFramesFromSHP(Start,Count,Dest,Count2);
 end;
end;

procedure TFrmSHPImport.DoList_Shadows;
var
 X : Integer;
 Start,Count,Dest,Count2 : Integer;
begin
 for X := 0 to ListBox.Count-1 do
 begin
  GetValuesFromList(ListBox.Items[X],Start,Count,Dest,Count2);
  CopyFramesFromSHP(Min(Start+(ASHP.Count div 2),ASHP.Count),Count,Dest,Count2);
 end;
end;

procedure GetMinMaxGMZ();
var
 X,Z   : Integer;
 ZBuff,
 Trans : PByte;
begin
 MinMax[0] := 255;
 MinMax[1] := 0;

 for X := 0 to GMFile.FrameCount-1 do
 begin
  ZBuff := GMFile.Frame[X].ZBuff;
  if Assigned(ZBuff) then
  for Z := 0 to GMFile.Width*GMFile.Height-1 do
  begin
   if ZBuff^ > MinMax[1] then
   MinMax[1] := ZBuff^;
   if ZBuff^ < MinMax[0] then
   MinMax[0] := ZBuff^;
   Inc(Cardinal(ZBuff),1);
  end;
 end;

 MinMax[1] := MinMax[1]-MinMax[0];
end;

procedure GetMinMaxSHP();
var
 C,X,Y : Integer;
 Alpha : PByte;
begin
 MinMax[2] := ASHP.Height;
 MinMax[3] := 0;

 for C := 0 to ASHP.Count-1 do
 begin
  Alpha := ASHP.AlphaFrame[C];
  if Assigned(Alpha) then
  for Y := 0 to ASHP.Height-1 do
  for X := 0 to ASHP.Width-1 do
  begin
   if (Alpha^ > 0) then
   begin
    if ASHP.Height-Y > MinMax[3] then
    MinMax[3] := ASHP.Height-Y;
    if ASHP.Height-Y < MinMax[2] then
    MinMax[2] := ASHP.Height-Y;
   end;

   Inc(Cardinal(Alpha),1);
  end;
 end;

 MinMax[3] := MinMax[3]-MinMax[2];
end;

procedure TFrmSHPImport.Load(Filename1,Filename2 : AnsiString);
var
 X : Integer;
begin
 DeleteButton.Enabled := False;

 FNGMZ := Filename2;

 ListBox.Clear;

 if FileExists(ChangeFileExt(Filename1,'.import')) then
 ListBox.Items.LoadFromFile(ChangeFileExt(Filename1,'.import'));

 if Assigned(ASHP) then
 ASHP.Free;
 if Assigned(GM) then
 GM.Free;
 if Assigned(GMFile) then
 GMFile.Free;

 ASHP := TSHP.Create;
 ASHP.LoadFromFile(Filename1);
 ASHP.LoadPaletteFromDir(SHP_Palette_Directory);

 GM := TGM_File.Create(GM_Type_Z);
 GM.LoadFromFile(Filename2);

 LoadOWPalette16(ChangeFileExt(Filename2,'.p16'),GMPal);

 GMFile := TGMF.Create(Max(ASHP.Width,GM.Width),Max(ASHP.Height,GM.Height));
 GMFile.AddFrame(GM.FrameCount);

 if not Assigned(PNG1) then
 PNG1 := TPNGObject.Create;
 if not Assigned(PNG2) then
 PNG2 := TPNGObject.Create;

 SetPNGWH(PNG1,(ASHP.Width+4)*ASHP.Count,ASHP.Height+20);
 SetPNGWH(PNG2,(GMFile.Width+4)*GMFile.FrameCount,GMFile.Height+20);

 AddFrameBars(PNG1,ASHP.Width,ASHP.Count);
 AddFrameBars(PNG2,GMFile.Width,GMFile.FrameCount);

 CopyFramesFromGM(0,GM.FrameCount,0);

 GetMinMaxGMZ();
 GetMinMaxSHP();

 if ListBox.Count > 0 then
 DoList;

 UpdateGMFPNG(GM.FrameCount,0);
 UpdateSHPPNG();

 StartF  := 0;
 DestF   := 0;
 CountF  := 1;
 Count2F := 1;
 UpdateLabels;
end;

procedure TFrmSHPImport.Image1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if (X < 0) then X := 32767+(32767+X);

 X := Abs(X) div (ASHP.Width+4);

 if (X < 0) or (X >= ASHP.Count) then Exit;

 if Button = mbLeft then
  StartF := X
 else
  CountF := (X-StartF)+1;

 UpdateLabels;
end;

procedure TFrmSHPImport.Image2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if (X < 0) then X := 32767+(32767+X);

 X := Abs(X) div (GMFile.Width+4);

 if (X < 0) or (X >= GMFile.FrameCount) then Exit;

 if Button = mbLeft then
  DestF := X
 else
  Count2F := (X-DestF)+1;

 UpdateLabels;
end;

procedure TFrmSHPImport.PngSpeedButton1Click(Sender: TObject);
begin
 ListBox.Items.Add(IntToStr(StartF)+','+IntToStr(CountF)+','+IntToStr(DestF)+','+IntToStr(Count2F));
 CopyFramesFromSHP(StartF,CountF,DestF,Count2F);
 UpdateGMFPNG(Count2F,DestF);
end;

procedure TFrmSHPImport.Button1Click(Sender: TObject);
begin
 Close;
end;

procedure MakeShadows(Filename : AnsiString);
begin
 if not FileExists(ChangeFileExt(FNGMZ,'.gms')) then Exit;

 GMFile.Free;
 GM.Free;

 GM := TGM_File.Create(0);
  GM.LoadFromFile(ChangeFileExt(FNGMZ,'.gms'));

 GMFile := TGMF.Create(Max(ASHP.Width,GM.Width),Max(ASHP.Height,GM.Height));
 GMFile.AddFrame(GM.FrameCount);

 CopyFramesFromGM(0,GM.FrameCount,0);

 if FrmSHPImport.ListBox.Count > 0 then
 FrmSHPImport.DoList_Shadows;

 GMFile.SaveToFile(Filename);
end;

procedure TFrmSHPImport.Button2Click(Sender: TObject);
begin
 GMFile.SaveToFile(ChangeFileExt(ASHP.Filename,'.gmf'));
 ListBox.Items.SaveToFile(ChangeFileExt(ASHP.Filename,'.import'));

 if HasShadowsCheck.Checked then
 MakeShadows(ChangeFileExt(ASHP.Filename,'_shadows.gmf'));

 Close;
end;

procedure TFrmSHPImport.ListBoxClick(Sender: TObject);
begin
 DeleteButton.Enabled := (ListBox.ItemIndex > -1) and (ListBox.ItemIndex < ListBox.Count);
end;

procedure TFrmSHPImport.ListBoxKeyPress(Sender: TObject; var Key: Char);
begin
 ListBoxClick(Nil);
end;

procedure TFrmSHPImport.DeleteButtonClick(Sender: TObject);
var
 Start,Count,Dest,Count2 : Integer;
begin
 GetValuesFromList(ListBox.Items[ListBox.ItemIndex],Start,Count,Dest,Count2);
 CopyFramesFromGM(Dest,Count2,Dest);
 ListBox.DeleteSelected;
 ListBox.ItemIndex := -1;
 DeleteButton.Enabled := False;
 UpdateGMFPNG(Count2,Dest);
end;

procedure TFrmSHPImport.FormCreate(Sender: TObject);
begin
 ScrollBox1.DoubleBuffered := True;
 ScrollBox2.DoubleBuffered := True;
end;

procedure TFrmSHPImport.PngSpeedButton2Click(Sender: TObject);
begin
 OpenDialog.InitialDir := ExtractFileDir(ASHP.Filename)+'\';
 if not OpenDialog.Execute then Exit;

 ListBox.Items.LoadFromFile(OpenDialog.FileName);
 CopyFramesFromGM(0,GMFile.FrameCount,0);
 DoList;
 UpdateGMFPNG(GMFile.FrameCount,0);
end;

end.
