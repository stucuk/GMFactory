unit GMF;
{
  Copyright (C) 2017 Stuart Carey

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  excluding commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
}

{
 GM Factory File Format
 By Stuart "Stucuk" Carey
 Started on 2017/02/13
}

interface

uses Windows, SysUtils, Graphics, Classes, PNGImage;

type
TGMFID = Array [0..3] of Char;

const
GMF_VER = 0;
GMF_ID  : TGMFID = ('G','M','F',Char(GMF_VER));

{
 GMF File Format Breakdown:

 TGMFHeader
 TGMFFrameHeaders
 TGMFLayerHeaders
 SideColours
 ZBuffers
 RGB16
 Trans
}

type
TGMFHeader = Record
 ID         : TGMFID;
 Width,
 Height     : Word;
 FrameCount : Word;
 LayerCount : Cardinal;
 HasSideCol : Boolean;
 HasZBuff   : Boolean;
end;

TGMFFrameHeader = Record
 Count    : Byte;
 Selected : Byte;
 Layer    : Cardinal; //ID to the first TGMFLayerHeader
end;
PGMFFrameHeader = ^TGMFFrameHeader;

TGMFLayerHeader = Record
 Vis   : Boolean;
end;
PGMFLayerHeader = ^TGMFLayerHeader;

TGMFLayer = Record
 RGB16 : PWord;
 Trans : PByte;
 Vis   : Boolean; //EDITOR
end;
PGMFLayer = ^TGMFLayer;

TGMFFrame = Record
 Layers   : PGMFLayer; // Always at least 2 layers. Layer 0 is the fully composed image.
 Count    : Byte;
 Edited   : Boolean;//True if Layer 0 needs to be updated.
 Selected : Byte; //EDITOR
 SideCol,
 ZBuff    : PByte;
end;
PGMFFrame = ^TGMFFrame;

TGMFPalette = Array [0..255] of TRGBTriple;

TGMF = Class(TObject)
protected
 FFilename : AnsiString;
 FFrames   : TList;
 FW,
 FH        : Word;
 FOneFrame : Boolean; //Load only a single frame!
 procedure FreeFrame(Frame : PGMFFrame);
 function MakeBlankFrame : PGMFFrame;
 function GetFrame(Index : Integer) : PGMFFrame;
 function GetFrameCount : Integer;
 procedure UpdateLayer0(Frame : PGMFFrame);
public
 constructor Create(Width, Height : Word);  overload;
 constructor Create(Filename : AnsiString; OneFrame : Boolean = False); overload;
 destructor Destroy; override;

 procedure Clear;

 procedure LoadFromFile(Filename : AnsiString);
 procedure SaveToFile(Filename : AnsiString);

 procedure LoadFromStream(Stream : TStream);
 procedure SaveToStream(Stream : TStream);

 procedure Resize(Width,Height : Word);

 procedure AddFrame(Count : Integer);
 procedure InsertFrame(Index : Integer; Count : Integer);
 procedure DeleteFrame(Index : Integer);

 procedure AddLayer(Frame : PGMFFrame; Count : Byte);
 procedure InsertLayer(Frame : PGMFFrame; Index : Byte; Count : Byte);
 procedure DeleteLayer(Frame : PGMFFrame; Index : Byte);

 function GetLayer(Frame : PGMFFrame; Index : Byte) : PGMFLayer;

 procedure LayerToPNG(Frame : PGMFFrame; PNG : TPNGObject; Index : Byte);
 procedure SideColToPNG(Frame : PGMFFrame; PNG : TPNGObject);
 procedure ZBuffToPNG(Frame : PGMFFrame; PNG : TPNGObject);
 procedure FrameToPNG(Frame : PGMFFrame; PNG : TPNGObject);

 procedure SetupLayer(Layer : PGMFLayer); // If the layers RGB16 or Trans Pointers are not setup then allocate them.
 procedure CopyLayerFromLayer(Dest,Source : PGMFLayer);
 procedure CopyLayer(Dest : PGMFLayer; RGB16 : PWord; Trans : PByte);
 procedure AddLayerToLayer(Dest,Source : PGMFLayer);

 function MakePalette(UseSideCols : Boolean) : TGMFPalette;
 function GetFrameWithPalette(Frame : PGMFFrame; const Palette : TGMFPalette; UseSideCols : Boolean) : PByte;

 property Width  : Word read FW;
 property Height : Word read FH;
 property Frame[Index : Integer] : PGMFFrame read GetFrame;
 property FrameCount : Integer read GetFrameCount;

 property Filename : AnsiString read FFilename write FFilename;

 property OneFrame : Boolean read FOneFrame write FOneFrame;
end;

function MakeGrayDarker(Color : Word) : TColor;
function MakeGrayGreen(Color : Word) : TColor;

implementation

uses OW_Palette, ColorQuantizationLibrary, PaletteLibrary, HSLUnit, Math, RGBHelper;

constructor TGMF.Create(Width, Height : Word);
begin
  Inherited Create();
  FFilename := 'Untitled';
  FFrames   := TList.Create;
  FW        := Width;
  FH        := Height;
  FOneFrame := False;
end;

constructor TGMF.Create(Filename : AnsiString; OneFrame : Boolean = False);
begin
 Create(0,0);
 FOneFrame := OneFrame;
 LoadFromFile(Filename);
end;

procedure TGMF.FreeFrame(Frame : PGMFFrame);
var
 L     : Integer;
 Layer : PGMFLayer;
begin
 for L := 0 to Frame.Count do
  begin
   Layer := GetLayer(Frame,L);
   if Assigned(Layer.RGB16) then
   FreeMem(Layer.RGB16);
   Layer.RGB16 := Nil;
   if Assigned(Layer.Trans) then
   FreeMem(Layer.Trans);
   Layer.Trans := Nil;
  end;

  FreeMem(Frame.Layers);
  Frame.Layers := Nil;
  if Assigned(Frame.SideCol) then
  FreeMem(Frame.SideCol);
  Frame.SideCol := Nil;

  if Assigned(Frame.ZBuff) then
  FreeMem(Frame.ZBuff);
  Frame.ZBuff := Nil;

  FreeMem(Frame);
end;

procedure TGMF.Clear;
begin
 While FFrames.Count > 0 Do
 begin
  FreeFrame(GetFrame(0));
  FFrames.Delete(0);
 end;
end;

destructor TGMF.Destroy;
begin
 Inherited Destroy;

 Clear;

 FFrames.Free;
end;

function TGMF.GetFrame(Index : Integer) : PGMFFrame;
begin
 Result := PGMFFrame(FFrames[Index]);
end;

function TGMF.GetFrameCount : Integer;
begin
 Result := FFrames.Count;
end;

procedure TGMF.LoadFromFile(Filename : AnsiString);
var
 Stream : TStream;
begin
 FFilename := Filename;
 Stream := TFileStream.Create(Filename,fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Stream);
 Stream.Free;
end;

procedure TGMF.SaveToFile(Filename : AnsiString);
var
 Stream : TStream;
begin
 FFilename := Filename;

 if FileExists(Filename) then
 DeleteFile(Filename);

 ForceDirectories(ExtractFileDir(Filename)+'\');

 Stream := TFileStream.Create(Filename,fmCreate);
  SaveToStream(Stream);
 Stream.Free;
end;

procedure TGMF.LoadFromStream(Stream : TStream);
var
 Header   : TGMFHeader;
 Frames   : PGMFFrameHeader;
 Layers   : PGMFLayerHeader;
 SideCol,
 ZBuffers,
 RGB16,
 Trans    : Pointer;

 FRH : PGMFFrameHeader;
 LAH : PGMFLayerHeader;
 SC,ZB,R16,TR : Pointer;

 F,L,WH,WH2 : Integer;

 AFrame : PGMFFrame;
 ALayer : PGMFLayer;
 FC,LC,
 AFLC   : Integer;
begin
 Clear;

 Stream.Read(Header,SizeOf(TGMFHeader));
 if Header.ID <> GMF_ID then Exit;

 FW := Header.Width;
 FH := Header.Height;

 WH  := FW*FH;
 WH2 := WH*2;

 if OneFrame then
 begin
  FC := 1;
  LC := 1;
 end
 else
 begin
  FC := Header.FrameCount;
  LC := Header.LayerCount;
 end;

 AddFrame(FC);

 GetMem(Frames,SizeOf(TGMFFrameHeader)*FC);
 GetMem(Layers,SizeOf(TGMFLayerHeader)*LC);
 GetMem(RGB16,WH2*LC);
 GetMem(Trans,WH*LC);

 Stream.Read(Frames^,SizeOf(TGMFFrameHeader)*FC);
 if OneFrame then
 Stream.Seek(SizeOf(TGMFFrameHeader)*(Header.FrameCount-1),soFromCurrent);
 Stream.Read(Layers^,SizeOf(TGMFLayerHeader)*LC);
 if OneFrame then
 Stream.Seek(SizeOf(TGMFLayerHeader)*(Header.LayerCount-1),soFromCurrent);

 if Header.HasSideCol then
 begin
  GetMem(SideCol,WH*FC);
  Stream.Read(SideCol^,WH*FC);
  if OneFrame then
  Stream.Seek(WH*(Header.FrameCount-1),soFromCurrent);
 end;
 if Header.HasZBuff then
 begin
  GetMem(ZBuffers,WH*FC);
  Stream.Read(ZBuffers^,WH*FC);
  if OneFrame then
  Stream.Seek(WH*(Header.FrameCount-1),soFromCurrent);
 end;

 Stream.Read(RGB16^,WH2*LC);
 if OneFrame then
 Stream.Seek(WH2*(Header.LayerCount-1),soFromCurrent);
 Stream.Read(Trans^,WH*LC);
 if OneFrame then
 Stream.Seek(WH*(Header.LayerCount-1),soFromCurrent);

 FRH := Frames;
 LAH := Layers;

 SC  := SideCol;
 ZB  := ZBuffers;
 R16 := RGB16;
 TR  := Trans;

 for F := 0 to FFrames.Count-1 do
 begin
  AFrame := Frame[F];
  AFrame.Selected := FRH.Selected;
  if not OneFrame then
  AddLayer(AFrame,FRH.Count-1);

  Inc(Cardinal(FRH),SizeOf(TGMFFrameHeader));

  if Header.HasSideCol then
  begin
   GetMem(AFrame.SideCol,WH);
   CopyMemory(AFrame.SideCol,SC,WH);
   Inc(Cardinal(SC),WH);
  end;

  if Header.HasZBuff then
  begin
   GetMem(AFrame.ZBuff,WH);
   CopyMemory(AFrame.ZBuff,ZB,WH);
   Inc(Cardinal(ZB),WH);
  end;

  if OneFrame then
   AFLC := 0
  else
   AFLC := AFrame.Count;

  for L := 0 to AFLC do
  begin
   ALayer := GetLayer(AFrame,L);
   SetupLayer(ALayer);

   ALayer.Vis := LAH.Vis;
   Inc(Cardinal(LAH),SizeOf(TGMFLayerHeader));

   CopyMemory(ALayer.RGB16,R16,WH2);
   Inc(Cardinal(R16),WH2);
   CopyMemory(ALayer.Trans,TR,WH);
   Inc(Cardinal(TR),WH);
  end;
 end;

 FreeMem(Frames);
 FreeMem(Layers);
 if Header.HasSideCol then
 FreeMem(SideCol);
 if Header.HasZBuff then
 FreeMem(ZBuffers);
 FreeMem(RGB16);
 FreeMem(Trans);
end;

procedure TGMF.SaveToStream(Stream : TStream);
var
 Header   : TGMFHeader;
 Frames   : PGMFFrameHeader;
 Layers   : PGMFLayerHeader;
 SideCol,
 ZBuffers,
 RGB16,
 Trans    : Pointer;

 FRH : PGMFFrameHeader;
 LAH : PGMFLayerHeader;
 SC,ZB,R16,TR : Pointer;

 F,L,WH,WH2 : Integer;

 AFrame : PGMFFrame;
 ALayer : PGMFLayer;
begin
 Header.ID         := GMF_ID;
 Header.Width      := FW;
 Header.Height     := FH;
 Header.FrameCount := FFrames.Count;
 Header.LayerCount := 0;
 Header.HasSideCol := Assigned(Frame[0].SideCol);
 Header.HasZBuff   := Assigned(Frame[0].ZBuff);

 WH  := FW*FH;
 WH2 := WH*2;

 for F := 0 to FFrames.Count-1 do
 Inc(Header.LayerCount,Frame[F].Count+1);

 GetMem(Frames,SizeOf(TGMFFrameHeader)*Header.FrameCount);
 GetMem(Layers,SizeOf(TGMFLayerHeader)*Header.LayerCount);
 if Header.HasSideCol then
  GetMem(SideCol,Header.FrameCount*WH)
 else
  SideCol := Nil;

 if Header.HasZBuff then
  GetMem(ZBuffers,Header.FrameCount*WH)
 else
  ZBuffers := Nil;

 GetMem(RGB16,Header.LayerCount*WH2);
 GetMem(Trans,Header.LayerCount*WH);

 Stream.Write(Header,SizeOf(TGMFHeader));

 FRH := Frames;
 LAH := Layers;

 SC  := SideCol;
 ZB  := ZBuffers;
 R16 := RGB16;
 TR  := Trans;

 for F := 0 to FFrames.Count-1 do
 begin
  AFrame := Frame[F];
  if Header.HasSideCol then
  begin
   CopyMemory(SC,AFrame.SideCol,WH);
   Inc(Cardinal(SC),WH);
  end;
  if Header.HasZBuff then
  begin
   CopyMemory(ZB,AFrame.ZBuff,WH);
   Inc(Cardinal(ZB),WH);
  end;

  FRH.Count    := AFrame.Count;
  FRH.Selected := AFrame.Selected;
  FRH.Layer    := (Cardinal(LAH)-Cardinal(Layers)) div SizeOf(TGMFLayerHeader); //ID

  Inc(Cardinal(FRH),SizeOf(TGMFFrameHeader));

  for L := 0 to AFrame.Count do
  begin
   ALayer  := GetLayer(AFrame,L);
   SetupLayer(ALayer);

   CopyMemory(R16,ALayer.RGB16,WH2);
   Inc(Cardinal(R16),WH2);

   CopyMemory(TR,ALayer.Trans,WH);
   Inc(Cardinal(TR),WH); 

   LAH.Vis := ALayer.Vis;
   Inc(Cardinal(LAH),SizeOf(TGMFLayerHeader));
  end;
 end;

 Stream.Write(Frames^,SizeOf(TGMFFrameHeader)*Header.FrameCount);
 FreeMem(Frames);

 Stream.Write(Layers^,SizeOf(TGMFLayerHeader)*Header.LayerCount);
 FreeMem(Layers);

 if Header.HasSideCol then
 begin
  Stream.Write(SideCol^,WH*Header.FrameCount);
  FreeMem(SideCol);
 end;
 
 if Header.HasZBuff then
 begin
  Stream.Write(ZBuffers^,WH*Header.FrameCount);
  FreeMem(ZBuffers);
 end;

 Stream.Write(RGB16^,WH2*Header.LayerCount);
 FreeMem(RGB16);
 Stream.Write(Trans^,WH*Header.LayerCount);
 FreeMem(Trans);
end;

procedure TGMF.Resize(Width,Height : Word);
var
 OW,OH : Word;
begin
 OW := FW;
 OH := FH;
 FW := Width;
 FH := Height;
 //TODO
end;

function TGMF.MakeBlankFrame : PGMFFrame;
begin
 GetMem(Result,SizeOf(TGMFFrame));
 Result.Count  := 1;
 Result.Edited := False;
 GetMem(Result.Layers,SizeOf(TGMFLayer)*2);
 ZeroMemory(Result.Layers,SizeOf(TGMFLayer)*2);
 Result.SideCol := Nil;
 Result.ZBuff   := Nil;
 Result.Selected := 1;
end;

procedure TGMF.AddFrame(Count : Integer);
var
 X : Integer;
begin
 for X := 1 to Count do
  FFrames.Add(MakeBlankFrame);
end;

procedure TGMF.InsertFrame(Index : Integer; Count : Integer);
var
 X : Integer;
begin
 for X := 1 to Count do
  FFrames.Insert(Index,MakeBlankFrame);
end;

procedure TGMF.DeleteFrame(Index : Integer);
begin
 FreeFrame(GetFrame(Index));
 FFrames.Delete(Index);
end;

procedure TGMF.AddLayer(Frame : PGMFFrame; Count : Byte);
begin
 if Count = 0 then Exit;
 Frame.Layers := ReallocMemory(Frame.Layers,SizeOf(TGMFLayer)*(Frame.Count+1+Count));
 ZeroMemory(GetLayer(Frame,Frame.Count+1),SizeOf(TGMFLayer)*Count);
 Inc(Frame.Count,Count);
end;

procedure TGMF.InsertLayer(Frame : PGMFFrame; Index : Byte; Count : Byte);
begin
 if Count = 0 then Exit;
 if (Index < 1) then
  Exit;

 Frame.Layers := ReallocMemory(Frame.Layers,SizeOf(TGMFLayer)*(Frame.Count+1+Count));
 CopyMemory(GetLayer(Frame,Index+Count),GetLayer(Frame,Index),SizeOf(TGMFLayer)*(Frame.Count-Index+1));

 ZeroMemory(GetLayer(Frame,Index),SizeOf(TGMFLayer)*Count);
 Inc(Frame.Count,Count);
end;

procedure TGMF.DeleteLayer(Frame : PGMFFrame; Index : Byte);
begin
 CopyMemory(GetLayer(Frame,Index),GetLayer(Frame,Index+1),SizeOf(TGMFLayer)*(Frame.Count-Index));
 Dec(Frame.Count,1);
 Frame.Layers := ReallocMemory(Frame.Layers,SizeOf(TGMFLayer)*(Frame.Count+1));
end;

function TGMF.GetLayer(Frame : PGMFFrame; Index : Byte) : PGMFLayer;
begin
 if Index = 0 then
 begin
  UpdateLayer0(Frame);
  Result := Frame.Layers;
 end
 else
  Result := Pointer(Cardinal(Frame.Layers)+SizeOf(TGMFLayer)*Index);
end;

procedure TGMF.LayerToPNG(Frame : PGMFFrame; PNG : TPNGObject; Index : Byte);
var
 BMP   : TBitmap;
 Layer : PGMFLayer;
 Y     : Integer;
 L     : Pointer;
begin
 BMP := TBitmap.Create;
  BMP.Width       := FW;
  BMP.Height      := FH;
  BMP.PixelFormat := pf16bit;

  Layer := GetLayer(Frame,Index);
  SetupLayer(Layer);
  L := Layer.RGB16;
  for Y := 0 to FH-1 do
  begin
   CopyMemory(BMP.ScanLine[Y],L,FW*2);
   Inc(Cardinal(L),FW*2);
  end;

  PNG.Assign(BMP);
 BMP.Free;

 if PNG.AlphaScanline[0] = Nil then
 PNG.CreateAlpha;
                        
 L := Layer.Trans;
 for Y := 0 to FH-1 do
 begin
  CopyMemory(PNG.AlphaScanline[Y],L,FW);
  Inc(Cardinal(L),FW);
 end;
end;

function MakeGrayDarker(Color : Word) : TColor;
var
 V : Byte;
begin
 Result := hicolor_to_tcolor(Color);
 V := (GetRValue(Result)+GetGValue(Result)+GetBValue(Result)) div 4;
 Result := RGB(V,V,V);
end;

function MakeGrayGreen(Color : Word) : TColor;
var
 V : Byte;
begin
 Result := hicolor_to_tcolor(Color);
 V := (GetRValue(Result)+GetGValue(Result)+GetBValue(Result)) div 3;
 V := Min((V+100),255);
 Result := RGB(0,V,0);
end;

procedure TGMF.SideColToPNG(Frame : PGMFFrame; PNG : TPNGObject);
var
 BMP   : TBitmap;
 Y,X   : Integer;
 L,
 RGB   : Pointer;
 Col   : Word;
 SL    : Pointer;
begin
 BMP := TBitmap.Create;
  BMP.Width       := FW;
  BMP.Height      := FH;
  BMP.PixelFormat := pf24bit;

  L := Frame.SideCol;
  RGB := GetLayer(Frame,0).RGB16;

  for Y := 0 to FH-1 do
  begin
   SL := BMP.ScanLine[Y];
   for X := 0 to FW-1 do
   begin
    if Assigned(L) and (Byte(L^) > 0) then
     TRGBTriple(SL^) := SetRGBTriple(MakeGrayGreen(Word(RGB^)))
    else
     TRGBTriple(SL^) := SetRGBTriple(MakeGrayDarker(Word(RGB^)));
    Inc(Cardinal(SL),3);
    Inc(Cardinal(RGB),2);
    if Assigned(L) then
    Inc(Cardinal(L),1);
   end;
  end;

  PNG.Assign(BMP);
 BMP.Free;

 if PNG.AlphaScanline[0] = Nil then
 PNG.CreateAlpha;

 L := GetLayer(Frame,0).Trans;

 for Y := 0 to FH-1 do
 if Assigned(L) then
 begin
  SL := PNG.AlphaScanline[Y];
  for X := 0 to FW-1 do
  begin
   Byte(SL^) := Byte(L^);
   Inc(Cardinal(SL),1);
   Inc(Cardinal(L),1);
  end;
 end
 else
 begin
  SL := PNG.AlphaScanline[Y];
  for X := 0 to FW-1 do
  begin
   Byte(SL^) := 0;
   Inc(Cardinal(SL),1);
  end;
 end;
end;

procedure TGMF.ZBuffToPNG(Frame : PGMFFrame; PNG : TPNGObject);
var
 BMP   : TBitmap;
 Y,X   : Integer;
 L     : Pointer;
 SL    : Pointer;
 Z : Byte;
begin
 BMP := TBitmap.Create;
  BMP.Width       := FW;
  BMP.Height      := FH;
  BMP.PixelFormat := pf16bit;

  L := Frame.ZBuff;
  for Y := 0 to FH-1 do
  begin
   SL := BMP.ScanLine[Y];
   if not Assigned(L) then
    ZeroMemory(SL,FW*2)
   else
    for X := 0 to FW-1 do
    begin
     Z := Byte(L^);
     Word(SL^) := color_to_hicolor(RGB(Z,Z,Z));
     Inc(Cardinal(SL),2);
     Inc(Cardinal(L),1);
    end;
  end;

  PNG.Assign(BMP);
 BMP.Free;

 if PNG.AlphaScanline[0] = Nil then
 PNG.CreateAlpha;

 L := GetLayer(Frame,0).Trans;

 for Y := 0 to FH-1 do
 if Assigned(L) then
 begin
  SL := PNG.AlphaScanline[Y];
  for X := 0 to FW-1 do
  begin
   Byte(SL^) := Byte(L^);
   Inc(Cardinal(SL),1);
   Inc(Cardinal(L),1);
  end;
 end
 else
 begin
  SL := PNG.AlphaScanline[Y];
  for X := 0 to FW-1 do
  begin
   Byte(SL^) := 0;
   Inc(Cardinal(SL),1);
  end;
 end;
end;

procedure TGMF.FrameToPNG(Frame : PGMFFrame; PNG : TPNGObject);
begin
 LayerToPNG(Frame,PNG,0);
end;

procedure TGMF.SetupLayer(Layer : PGMFLayer);
begin
 if not Assigned(Layer.RGB16) then
 begin
  GetMem(Layer.RGB16,FW*FH*2);
  ZeroMemory(Layer.RGB16,FW*FH*2);
  Layer.Vis := True;
 end;

 if not Assigned(Layer.Trans) then
 begin
  GetMem(Layer.Trans,FW*FH);
  ZeroMemory(Layer.Trans,FW*FH);
 end;
end;

procedure TGMF.CopyLayerFromLayer(Dest,Source : PGMFLayer);
begin
 CopyLayer(Dest,Source.RGB16,Source.Trans);
end;

procedure TGMF.CopyLayer(Dest : PGMFLayer; RGB16 : PWord; Trans : PByte);
begin
 SetupLayer(Dest);
 CopyMemory(Dest.RGB16,RGB16,FW*FH*2);
 CopyMemory(Dest.Trans,Trans,FW*FH);
end;

procedure TGMF.AddLayerToLayer(Dest,Source : PGMFLayer);
var
 I : Integer;
 DP,DT,SP,ST : Pointer;
begin     
 DP := Dest.RGB16;
 DT := Dest.Trans;
 SP := Source.RGB16;
 ST := Source.Trans;
 for I := 0 to FW*FH-1 do
 begin
  if Byte(ST^) > 0 then
  begin
   Word(DP^) := Word(SP^);
   Byte(DT^) := 255;
  end;
  Inc(Cardinal(DP),2);
  Inc(Cardinal(SP),2);
  Inc(Cardinal(DT),1);
  Inc(Cardinal(ST),1);
 end;
end;

procedure TGMF.UpdateLayer0(Frame : PGMFFrame);
var
 Layer0,
 Layer   : PGMFLayer;
 L : Integer;
begin
 if not Frame.Edited then
 Exit;

 Layer0 := Frame.Layers;
 Layer  := GetLayer(Frame,1);
 SetupLayer(Layer0);
 SetupLayer(Layer);
 CopyLayerFromLayer(Layer0,Layer);

 for L := 2 to Frame.Count do
 begin
  Layer := GetLayer(Frame,L);
  if Layer.Vis then
  AddLayerToLayer(Layer0,Layer);
 end;

 Frame.Edited := False;
end;

type
TRGBTripleArray = Array of TRGBTriple;

procedure AddIfUnique(var Cols : TRGBTripleArray; var Count : Integer; const Col : TRGBTriple);
var
 X : Integer;
begin
 for X := 0 to Count-1 do
 if (Cols[X].rgbtBlue = Col.rgbtBlue) and (Cols[X].rgbtGreen = Col.rgbtGreen) and (Cols[X].rgbtRed = Col.rgbtRed) then
 Exit;

 Inc(Count);
 if Count > High(Cols)+1 then
 SetLength(Cols,High(Cols)+1001);
 Cols[Count-1] := Col;
end;

function Get8BitPalette(Source: TBitmap; IgnoreCount : Byte): TRGBQuadArray;
var
  ColorQuantizer     : TColorQuantizer;
  QA : TRGBQuadArray;
  X,MAX : Integer;
  RGB : PRGBTriple;
begin
  ColorQuantizer := TColorQuantizer.Create(256-IgnoreCount, 8);
  try
    ColorQuantizer.ProcessImage(Source.Handle);
    ZeroMemory(@Result[0],SizeOf(TRGBQuadArray));
    ZeroMemory(@QA[0],SizeOf(TRGBQuadArray));
    MAX := ColorQuantizer.GetColorTable(QA);

    for X := 0 to 255 do
    Result[X] := SetRGBAQuad(255,0,255,255);

    CopyMemory(@Result[IgnoreCount],@QA[0],(MAX)*SizeOf(TRGBQuad));
  finally
    ColorQuantizer.Free
  end;
end;

function TGMF.MakePalette(UseSideCols : Boolean) : TGMFPalette;
var
 F,C,Y,X : Integer;
 Cols : TRGBTripleArray;
 RGB : PRGBTriple;
 BMP : TBitmap;

 AFrame : PGMFFrame;
 ALayer : PGMFLayer;
 R16 : PWord;
 SC,
 TR  : PByte;

 SCA : Byte;

 Palette : TRGBQuadArray;
 IgnoreC   : Byte;
begin
 C := 0;
 SetLength(Cols,1000);

 AddIfUnique(Cols,C,SetRGBTriple(255,0,255));

 if UseSideCols then
 begin
  for X := 1 to 24 do
  AddIfUnique(Cols,C,SetRGBTriple(0,X*5,Trunc(((240-8)/8)*X)));

  IgnoreC := 25;
 end
 else
 IgnoreC := 1;


 for F := 0 to FFrames.Count-1 do
 begin
  AFrame := Frame[F];
  ALayer := GetLayer(AFrame,0);

  R16 := ALayer.RGB16;
  TR  := ALayer.Trans;
  SC  := AFrame.SideCol;

  for Y := 0 to FH-1 do
   for X := 0 to FW-1 do
   begin
    if Assigned(SC) then
     SCA := SC^
    else
     SCA := 0;

    if (TR^ > 0) and ((SCA = 0) or (not UseSideCols)) then
    AddIfUnique(Cols,C,hicolor_to_color(R16^));
    Inc(Cardinal(R16),2);
    Inc(Cardinal(TR),1);
    Inc(Cardinal(SC),1);
   end;
 end;

 BMP := TBitmap.Create;
 BMP.PixelFormat := pf24bit;
 BMP.Width  := C-IgnoreC;
 BMP.Height := 1;

 RGB := BMP.ScanLine[0];

 for X := IgnoreC to C-1 do
 begin
  RGB^ := Cols[X];
  //CheckColour(RGB^);
  Inc(Cardinal(RGB),3);
 end;

 if BMP.Width > 0 then
 Palette := Get8BitPalette(BMP,IgnoreC);

 Palette[0] := SetRGBAQuad(32,96,32,0);

 //8 0 0 -> 120 28 16 -> 248 128 96


 if UseSideCols then
 begin
  for Y := 1 to 12 do
  Palette[Y] := SetRGBAQuad(8+Trunc((Y-1)*(112/11)),Trunc((Y-1)*(28/11)),Trunc((Y-1)*(16/11)),0);

  for Y := 1 to 12 do
  Palette[Y+12] := SetRGBAQuad(120+Trunc((Y-1)*((248-120)/11)),28+Trunc((Y-1)*(100/11)),16+Trunc((Y-1)*(80/11)),0);
 end;

 for Y := 0 to 255 do
 Result[Y] := SetRGBTriple(Palette[Y]);

 BMP.Free;
end;

function FindClosestFromPalette(RGB : TRGBTriple; Palette : TRGBQuadArray; PaletteL : Array of Byte) : Byte;
var
 X    : Integer;
 Diff,
 Test : Integer;
 L,RL,
 TL : Byte;
begin
 Result := 0;
 Diff   := -1;

 L  := GetLumonisity(RGB);
 RL := L;

 for X := 0 to 255 do
 begin
  Test := abs(RGB.rgbtRed-Palette[x].rgbRed) + abs(RGB.rgbtGreen-Palette[x].rgbGreen) + abs(RGB.rgbtBlue-Palette[x].rgbBlue);
  if (Diff = -1) or (Test < Diff) or ((Test = Diff) and (abs(L-PaletteL[x]) < abs(L-RL))) then
  begin
   Diff   := Test;
   Result := X;
   RL     := PaletteL[x];
  end;
 end;
end;

procedure SetBMP8Colours(var BMP : TBitmap; const Source : TBitmap; Palette : TRGBQuadArray; PaletteL : Array of Byte);
var
 X,Y : Integer;
 SL  : PByte;
 SL2 : PWord;
begin
 for Y := 0 to BMP.Height-1 do
 begin
  SL  := BMP.ScanLine[Y];
  SL2 := Source.ScanLine[Y];
  for X := 0 to BMP.Width-1 do
  begin
   SL^ := FindClosestFromPalette(hicolor_to_color(SL2^),Palette,PaletteL);

   Inc(Cardinal(SL),1);
   Inc(Cardinal(SL2),2);
  end;
 end;
end;

procedure Make8BitBMP(var Source : TBitmap; Palette : TRGBQuadArray);
var
 BMP : TBitmap;
 PaletteL : Array [0..255] of Byte;
 X : Integer;
begin
 BMP := TBitmap.Create;
  BMP.PixelFormat := pf8bit;
  BMP.Width       := Source.Width;
  BMP.Height      := Source.Height;
  BMP.HandleType  := bmDIB;
  SetDIBColorTable(BMP.Canvas.Handle, 0, 256, Palette);

  for X := 0 to 255 do
  PaletteL[X] := GetLumonisity(Palette[X]);
  SetBMP8Colours(BMP,Source,Palette,PaletteL);

  Source.PixelFormat := pf8bit;
  Source.HandleType  := bmDIB;
  SetDIBColorTable(Source.Canvas.Handle, 0, 256, Palette);
  Source.Canvas.Draw(0,0,BMP);
 BMP.Free;
end;

function MakeGray(RGB : TRGBTriple) : TRGBTriple; overload;
begin
 Result.rgbtRed   := (RGB.rgbtRed+RGB.rgbtGreen+RGB.rgbtBlue) div 3;
 Result.rgbtGreen := Result.rgbtRed;
 Result.rgbtBlue  := Result.rgbtRed;
end;

function GetSideCol(Luminosity : Byte; const Palette : TGMFPalette) : Byte;
var
 X,Best,Diff : Integer;
begin
 Best := 1;
 Diff := Abs(Luminosity-GetLumonisity(MakeGray(Palette[1])));
 for X := 2 to 24 do
 if Abs(Luminosity-GetLumonisity(MakeGray(Palette[X]))) < Diff then
 begin
  Best := X;
  Diff := Abs(Luminosity-GetLumonisity(MakeGray(Palette[X])));
 end;

 Result := Best;
end;

function TGMF.GetFrameWithPalette(Frame : PGMFFrame; const Palette : TGMFPalette; UseSideCols : Boolean) : PByte;
var
 X,Y     : Integer;
 Pal     : TRGBQuadArray;
 BMP     : TBitmap;
 SL,
 R16     : PWord;
 R8,
 TR,
 SC      : PByte;

 Layer   : PGMFLayer;
 SCP     : Word;
 SideCol : Byte;
 SCT     : Byte;
begin
 Layer := GetLayer(Frame,0);

 Pal[0] := SetRGBAQuad(255,0,255,0);

 for X := 1 to 255 do
 Pal[X] := SetRGBAQuad(Palette[X]);

 if UseSideCols then
 for X := 1 to 24 do
  Pal[X] := SetRGBAQuad(255,0,255,0);

 SCP := color_to_hicolor(SetRGBTriple(Pal[0]));

 BMP := TBitmap.Create();
  BMP.Width       := FW;
  BMP.Height      := FH;
  BMP.PixelFormat := pf24bit;

  R16 := Layer.RGB16;
  TR  := Layer.Trans;
  SC := Frame.SideCol;

  for Y := 0 to FH-1 do
  begin
   SL := BMP.ScanLine[Y];
   for X := 0 to FW-1 do
   begin
    if not Assigned(SC) then
     SCT := 0
    else
    begin
     SCT := SC^;
     Inc(Cardinal(SC),1);
    end;

    if ((SCT = 0) or (not UseSideCols)) and (TR^ > 0) then
     SL^ := R16^
    else
     SL^ := SCP;

    Inc(Cardinal(R16),2);
    Inc(Cardinal(SL),2);
    Inc(Cardinal(TR),1);
   end;
  end;

  Make8BitBMP(BMP,Pal);

  GetMem(Result,FW*FH);

  R8 := Result;
  for Y := 0 to FH-1 do
  begin
   CopyMemory(R8,BMP.ScanLine[Y],FW);
   Inc(Cardinal(R8),FW);
  end;
 BMP.Free;

 //Set Side Colours
 if UseSideCols then
 begin
  R8  := Result;
  R16 := Layer.RGB16;
  SC  := Frame.SideCol;
  for Y := 0 to FW*FH-1 do
  begin
   if SC^ > 0 then
   begin
    SideCol := GetSideCol(Max(GetLumonisity({MakeGray}(hicolor_to_color(R16^))),0),Palette);
    if SideCol > 0 then
    R8^ := SideCol;
   end;
   Inc(Cardinal(SC),1);
   Inc(Cardinal(R8),1);
   Inc(Cardinal(R16),2);
  end;
 end;
end;

end.
