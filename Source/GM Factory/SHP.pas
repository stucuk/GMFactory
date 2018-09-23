unit SHP;
{
 Started: 2016/08/17
 By: Stuart "Stucuk" Carey
}

interface

uses Windows, Graphics, Classes;

 const
  IMGAREA_X = 0;
  IMGAREA_Y = 1;
  IMGAREA_W = 2;
  IMGAREA_H = 3;

 SHPSideCol : Array [0..7] of TRGBTriple = (
 //{GOLD}   (rgbtBlue: 20;rgbtGreen:220;rgbtRed:230;),
 {GOLD}   (rgbtBlue:120;rgbtGreen:214;rgbtRed:245;),
 {RED}    (rgbtBlue:  0;rgbtGreen:  0;rgbtRed:255;),
 {Blue}   (rgbtBlue:233;rgbtGreen: 56;rgbtRed: 12;),
 {Green}  (rgbtBlue:  0;rgbtGreen:200;rgbtRed:  0;),
 {Orange} (rgbtBlue:  0;rgbtGreen:128;rgbtRed:255;),
 {SkyBlue}(rgbtBlue:240;rgbtGreen:202;rgbtRed:166;),
 {Pink}   (rgbtBlue:255;rgbtGreen:  0;rgbtRed:255;),
 {Purple} (rgbtBlue:128;rgbtGreen:  0;rgbtRed:128;)
 );

 type
 TImgArea = Array [0..3] of Word;

 TSHP_Palette = Array [0..255] of TRGBTriple;
 PSHP_Palette = ^TSHP_Palette;

 TSHP_Frame = Packed Record
  UsedArea : TImgArea;   // X,Y,W,H
  PalData  : PByte;      // Holds Image as Palette colours
  RGBData  : PRGBTriple; // Holds Image as RGB colours
  AData    : PByte;      // Holds Image Alpha
  SideCol  : PByte;      // Holds if Side Colour
  NeedUpd  : Boolean;    // When true RGBData holds an old image
  RadarCol : TRGBQuad;
 end;
 PSHP_Frame = ^TSHP_Frame;

 TSHP_Type    = (stAnimation, stBuilding, stIsoSnoBuilding, stIsoTemBuilding, stIsoUrbBuilding, stCameo, stUnit);
 TSHP_PalType = (sptDefault, sptTem, sptSno, sptUrb, sptLun, sptDes, sptNewurb, sptInt, sptWin);
 TSHP_Game    = (sgTD, sgRA1, sgTS, sgRA2);

 const
  stIsoBuilding    = [stIsoSnoBuilding,stIsoTemBuilding,stIsoUrbBuilding];
  stBuildings      = [stBuilding] + stIsoBuilding;
  stBuildingorUnit = [stUnit]+stBuildings;

 const
 SHP_Type_Str     : Array [0..Ord(stUnit)]  of AnsiString = ('Animation', 'Building', 'Building (IsoSnow)', 'Building (IsoTemperate)', 'Building (IsoUrban)', 'Cameo', 'Unit');
 SHP_Pal_Type_Str : Array [0..Ord(sptWin)]  of AnsiString = ('Default','Temperate', 'Snow', 'Urban', 'Lunar', 'Desert', 'New Urban', 'Interior', 'Winter');
 SHP_Game_Str     : Array [0..Ord(sgRA2)]   of AnsiString = ('Tiberian Dawn', 'Red Alert', 'Tiberian Sun', 'Red Alert 2');
 SHP_GameF_Str    : Array [0..Ord(sgRA2)]   of AnsiString = ('TD', 'RA1', 'TS', 'RA2');

 COMPMODE_AUTO = 0;
 COMPMODE_HALF = 1;
 COMPMODE_ALL  = 2;
 COMPMODE_NONE = 3;

 TS_FLAG_TRANSPARENT = $1;
 TS_FLAG_COMPRESSION = $2;

 type
 TSHP_Header = record
  Zero, Width, Height, Count : Word;
 end;

 TSHP_Header_Image = record
  X, Y, W, H  : Word;
  Flags       : Word;
  Unknown     : Word; // Part of Flags? Something else?
  RadarColor  : TRGBTriple;
  Unknown1    : Byte; // Part of Radar Color? Seperate?
  Unknown2,           // ????
  Offset      : Cardinal;
 end;

 PSHP_Header_Image = ^TSHP_Header_Image;

 TThreeByte = Array [0..2] of Byte;

// SHP_RA_File.Pas
 TSHP_Header_TD = record
  Count, X,
  Y,Width,
  Height,
  MaxFrameSize,
  Flags         : Word;
 end;

 TSHP_Offset_TD = packed record
  Offset           : TThreeByte;
  Compression      : Byte;
  ReOffset         : TThreeByte;
  CompressionExtra : Byte;        // ?????
 end;
 PSHP_Offset_TD = ^TSHP_Offset_TD;
 // SHP_RA_File.Pas END

 TPalExt = Record
  EXT     : AnsiString;
  PalType : TSHP_PalType;
 end;

 const
 Pal_EXTS : Array [0..Ord(sptWin)] of TPalExt = ((EXT:'.shp';PalType:sptDefault),(EXT:'.tem';PalType:sptTem),
                                                 (EXT:'.sno';PalType:sptSno),(EXT:'.urb';PalType:sptUrb),
                                                 (EXT:'.lun';PalType:sptLun),(EXT:'.des';PalType:sptDes),
                                                 (EXT:'.ubn';PalType:sptNewurb),(EXT:'.int';PalType:sptInt),
                                                 (EXT:'.win';PalType:sptWin));
 type
 TSHP_Extra_Header = Packed Record
  Ver  : Word;
  ID   : Array [0..5] of Char; // #0EXTRA
 end;

 //SHPToolz Extra Data (Saved to the end of SHP's)
 TSHP_Extra = Packed Record
  Palette : TSHP_Palette;
  Game    : Byte;
  _Type   : Byte;
 end;

 TSHP_FileType = (sftNone,sftTD,sftTS);

 TSHP = Class(TObject)
 protected
  FFilename      : AnsiString;
  FW,FH          : Integer;
  FFrames        : Array of TSHP_Frame;
  FPalette       : TSHP_Palette;
  FPaletteISO    : PSHP_Palette;
  FTransCol      : TRGBTriple;
  FSHPFileType   : TSHP_FileType;
  FGame          : TSHP_Game;
  FType          : TSHP_Type;
  FPalType       : TSHP_PalType;
  FSHPExtra      : Boolean;
  FSaveExtra     : Boolean;
  FOneFrameLoad  : Boolean;    // When enabled it will only load 1 frame. Useful for previewing a file.
  FSideCol       : TRGBTriple;
  FUseSideCol    : Boolean;    // When enabled remappable colours will be generated based on FSideCol
  FKeepHeaders   : Boolean;    // Hangs onto headers instead of freeing them
  FStoredHeaders : Array [0..1] of Pointer;
  FIgnoreFrames  : Boolean;    // Ignores Frame Data when loading. For debugging only.
  FAutoPalette   : AnsiString; // Auto Selected Palette
  FTransparentShadows : Boolean;
  FTD_LoopFrame  : Integer;
  FAmbientLight  : Smallint;
  FForceFlags      : Word;       //Force Flags on Save!
  FCompressionMode : Byte;       //Compression mode to use on Save!
  FSaved           : Boolean;    // TSHP doesn't care about this but an editor would.
  FWasLoaded       : Boolean;    // TSHP doesn't care about this but an editor would. Its only true if its loaded or saved
  procedure Decode3 (Input : TStream; Frame : Integer);
  procedure Decode1 (Input : TStream; Frame : Integer);
  procedure TDRA1Decode(Input : TStream; DecodeFrame,Frame : Integer);
  function  GetFrame(Index : Integer)    : PSHP_Frame;
  function  GetRGBFrame(Index : Integer) : PRGBTriple; virtual;
  function  GetAlphaFrame(Index : Integer) : PByte;
  function  GetSideColFrame(Index : Integer) : PByte;
  procedure EmptyFrameRGBA(Index : Integer; Buffer : Pointer);
  procedure EmptyFrameRGB(Index : Integer);
  procedure BuildFrameRGB(Index : Integer);
  function  GetCount() : Integer;
  procedure SetTransCol(Value : TRGBTriple);
  procedure MarkNeedUpd;

  procedure ReadExtra(Stream : TStream);
  procedure WriteExtra(Stream : TStream);

  function LoadTSSHPFromStream(Stream : TStream) : Boolean;
  function LoadTDRASHPFromStream(Stream : TStream) : Boolean;

  procedure GetUsedAreaFrame(Buffer : Pointer; Frame : Integer);
  function GetFrameAverageColour(Frame : Integer) : TRGBTriple;
  procedure SaveTSRA2SHPToStream(Stream : TStream);
//  procedure SaveTDRASHPToStream(Stream : TStream);

  function GetRadarCol(Index : Integer) : TColor;
  procedure SetRadarCol(Index : Integer; Value : TColor);

  function GetPalette(Index : Integer) : TSHP_Palette;

  procedure SetSideCol(Value : TRGBTriple);
  procedure SetUseSideCol(Value : Boolean);
  procedure Clear; virtual;
  procedure FindSHPType(Count : Integer);
  function GetIsTransparent : Boolean;
  procedure SetTransparentShadows(Value : Boolean);
  function ScanFrameForColor(Color : Byte; Frame : Integer) : Boolean;
  procedure IsRA2NotTSUnitCheck;
  function GetISOPalette : TSHP_Palette;
  procedure SetType(Value : TSHP_Type);
  procedure SetAmbientLight(Value : SmallInt);
  procedure GetUsedPaletteColoursForFrame(Frame : Integer; Result : PByte);

  procedure SetFrameAmount(Value : Integer); virtual;
 public
  constructor Create;
  destructor Destroy; override;

  procedure LoadFromFile(Filename : AnsiString);
  procedure SaveToFile(Filename : AnsiString);

  procedure LoadFromStream(Stream : TStream);
  procedure SaveToStream(Stream : TStream);

  procedure SetPalette(Palette : TSHP_Palette);
  procedure SetPaletteRGBQ(RGBQ : array of TRGBQuad);
  procedure LoadPalette(Filename : AnsiString);
  procedure LoadPaletteFromDir(Directory : AnsiString);

  procedure FreeFrameMem(Index : Integer);
  procedure FreeDebugHeaders;

  procedure AddFrames(Amount,Width,Height : Integer);

  procedure DrawFrameToBMP(Index : Integer; var BMP : TBitmap); //Helper
  procedure DrawFrameToBMP8(Index : Integer; var BMP : TBitmap; const X,Y : Integer); //Helper

  function ShouldDrawShadow : Boolean; //Helper
  function BuildFrameRGBA(Index : Integer) : PRGBQuad; // Helper for getting OpenGL Texture like buffer - NOTE: NOT TESTED!!!
  function GetFramePixel(Frame, X, Y : Integer) : Byte;

  procedure UpdateUsedArea(Frame : Integer);

  function GetUsedPaletteColours(Frame : Integer) : PByte; // If Frame is -1 it will return colours for all frames. Result is always 256 bytes (Boolean value).

  property Frame[Index : Integer]         : PSHP_Frame    read GetFrame;
  property RGBFrame[Index : Integer]      : PRGBTriple    read GetRGBFrame;
  property AlphaFrame[Index : Integer]    : PByte         read GetAlphaFrame;
  property SideColFrame[Index : Integer]  : PByte         read GetSideColFrame;
  property FrameRadarCol[Index : Integer] : TColor        read GetRadarCol         write SetRadarCol;

  property Width                          : Integer       read FW;
  property Height                         : Integer       read FH;
  property Count                          : Integer       read GetCount;

  property TransparentColour              : TRGBTriple    read FTransCol           write SetTransCol;
  property SHPFileType                    : TSHP_FileType read FSHPFileType;

  property Palette[Index : Integer]       : TSHP_Palette  read GetPalette;  //Returns Palette with 0 replaced with FTransCol
  property Palette_True                   : TSHP_Palette  read FPalette;    //Returns True Palette

  property Filename                       : AnsiString    read FFilename           write FFilename;

  property SHPGame                        : TSHP_Game     read FGame               write FGame;
  property SHPType                        : TSHP_Type     read FType               write SetType;
  property SHPPalType                     : TSHP_PalType  read FPalType            write FPalType;
  property SHPExtra                       : Boolean       read FSHPExtra           write FSHPExtra;

  property SideColor                      : TRGBTriple    read FSideCol            write SetSideCol;
  property UseSideColour                  : Boolean       read FUseSideCol         write SetUseSideCol;

  property KeepHeaders                    : Boolean       read FKeepHeaders        write FKeepHeaders; // Store Headers
  property Debug_Header0                  : Pointer       read FStoredHeaders[0];               // Main Header
  property Debug_Header1                  : Pointer       read FStoredHeaders[1];               // Frame Header

  property IgnoreFrames                   : Boolean       read FIgnoreFrames       write FIgnoreFrames; // Don't load Frame Data (Debug)

  property OneFrameLoad                   : Boolean       read FOneFrameLoad       write FOneFrameLoad; // Used for Previewing first Frame

  property AutoPalette                    : AnsiString    read FAutoPalette        write FAutoPalette; // Auto Selected Palette
  property IsTransparent                  : Boolean       read GetIsTransparent;                 // Tries to work out based on game/type

  property TransparentShadows             : Boolean       read FTransparentShadows write SetTransparentShadows; //Alpha is needed for this to render right

  property AmbientLight                   : Smallint      read FAmbientLight       write SetAmbientLight;

  property ForceFlags                     : Word          read FForceFlags         write FForceFlags;
  property CompressionMode                : Byte          read FCompressionMode    write FCompressionMode;

  property Saved                          : Boolean       read FSaved              write FSaved;
  property WasLoaded                      : Boolean       read FWasLoaded          write FWasLoaded;
 end;

 //Helpers
 function SetRGBTriple(R,G,B : Byte) : TRGBTriple;
 function MakeCheckerTileBMP : TBitmap; // Helper for making photoshop style background
 function AddTrailer(Input : AnsiString) : AnsiString;
 function ThreeByteToCardinal(V : TThreeByte) : Cardinal;
 function IsSHPExt(EXT : AnsiString) : Boolean;
 function TripleToQuad(RGB : TRGBTriple) : TRGBQuad;
 function QuadToTriple(RGBQ : TRGBQuad) : TRGBTriple;
 function IsValidSHP(Filename : AnsiString) : Boolean;
 function IsTDSHP(Filename : AnsiString) : Boolean; overload;
 function IsTDSHP(Stream : TStream) : Boolean;      overload;
 function GetSHPFileType(Stream : TStream) : TSHP_FileType;      overload;
 function GetSHPFileType(Filename : AnsiString) : TSHP_FileType; overload;
 function SetArea(X,Y,W,H : Word) : TImgArea;
 //

 var
 SHPPalettes     : Array [sgTD..sgRA2,stAnimation..stUnit] of AnsiString;
 SHPPalettesOver : Array [sgTD..sgRA2,sptDefault..sptWin]   of AnsiString;
 SHP_Palette_Directory : AnsiString = '';

implementation

uses SysUtils, Math;

function TripleToQuad(RGB : TRGBTriple) : TRGBQuad;
begin
 Result.rgbRed      := RGB.rgbtRed;
 Result.rgbGreen    := RGB.rgbtGreen;
 Result.rgbBlue     := RGB.rgbtBlue;
 Result.rgbReserved := 255;
end;

function QuadToTriple(RGBQ : TRGBQuad) : TRGBTriple;
begin
 Result.rgbtRed   := RGBQ.rgbRed;
 Result.rgbtGreen := RGBQ.rgbGreen;
 Result.rgbtBlue  := RGBQ.rgbBlue;
end;

function SetRGBTriple(R,G,B : Byte) : TRGBTriple;
begin
 Result.rgbtRed   := R;
 Result.rgbtGreen := G;
 Result.rgbtBlue  := B;
end;

function MakeCheckerTileBMP : TBitmap;
var
 Y : Integer;
 S : Pointer;
begin
 Result := TBitmap.Create;
 Result.Width  := 16;
 Result.Height := 16;
 Result.PixelFormat := pf24bit;

 for Y := 0 to 7 do
 begin
  S := Result.ScanLine[Y];
  FillChar(S^,8*3,200);
  Inc(Cardinal(S),8*3);
  FillChar(S^,8*3,250);
 end;

 for Y := 8 to 15 do
 begin
  S := Result.ScanLine[Y];
  FillChar(S^,8*3,250);
  Inc(Cardinal(S),8*3);
  FillChar(S^,8*3,200);
 end;
end;

function AddTrailer(Input : AnsiString) : AnsiString;
begin
 Result := Input;
 if (Result = '') or (Result[Length(Result)] = '\') then
 Exit;

 if Result[Length(Result)] = '/' then
 Result[Length(Result)] := '\'
 else
 Result := Result + '\';
end;

function HasFlag(Flag,Value : Word) : Boolean;
begin
 Result := (Flag and Value) > 0;
end;

function IsSHPExt(EXT : AnsiString) : Boolean;
var
 X : Integer;
begin
 EXT := Lowercase(EXT);
 for X := 0 to High(Pal_EXTS) do
 if Pal_EXTS[X].EXT = EXT then
 begin
  Result := True;
  Exit;
 end;
 Result := False;
end;

function ThreeByteToCardinal(V : TThreeByte) : Cardinal;
begin
 Result := V[0] + V[1] shl 8 + V[2] shl 16;
end;

function IsTDSHP(Filename : AnsiString) : Boolean;
var
 Stream  : TStream;
begin
 Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
 Result := IsTDSHP(Stream);
 Stream.Free;
end;

function IsTDSHP(Stream : TStream) : Boolean;
var
 Header : TSHP_Header_TD;
 Offset : Array [0..1] of TSHP_Offset_TD;
begin
 Result := False;
 Stream.Position := 0;
 Stream.Read(Header,SizeOf(TSHP_Header_TD));

 if (Header.Width = 0) or (Header.Height = 0) or (Header.Count = 0) then Exit;

 if Stream.Size-Stream.Position < (Header.Count+2)*SizeOf(TSHP_Offset_TD) then Exit;
 
 Stream.Seek(Header.Count*SizeOf(TSHP_Offset_TD),soFromCurrent);
 Stream.Read(Offset[0],SizeOf(TSHP_Offset_TD)*2);

 if (ThreeByteToCardinal(Offset[0].Offset) <> Stream.Size) and (ThreeByteToCardinal(Offset[1].Offset) <> Stream.Size) then Exit;

 Result := True;
end;

function IsTSSHP(Stream : TStream) : Boolean;
var
 Header : TSHP_Header;
begin
 Result := False;
 Stream.Position := 0;
 Stream.Read(Header,SizeOf(TSHP_Header));

 if ((Header.Zero > 0) or (Header.Width = 0) or (Header.Height = 0) or (Header.Count = 0)) and not ((Header.Zero = 0) and (Header.Count = 1)) then
  Exit;

 if Stream.Size-Stream.Position < (Header.Count)*SizeOf(TSHP_Header_Image) then Exit;

 Result := True;
end;

function IsValidSHP(Filename : AnsiString) : Boolean;
var
 Stream  : TStream;
begin
 Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
 Result := IsTDSHP(Stream) or IsTSSHP(Stream);
 Stream.Free;
end;

function GetSHPFileType(Stream : TStream) : TSHP_FileType;
begin
 if IsTDSHP(Stream) then
  Result := sftTD
 else
 if IsTSSHP(Stream) then
  Result := sftTS
 else
  Result := sftNone;
end;

function GetSHPFileType(Filename : AnsiString) : TSHP_FileType;
var
 Stream  : TStream;
begin
 Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  Result := GetSHPFileType(Stream);
 Stream.Free;
end;

//////////////////////////////////////////////////////////////////////

constructor TSHP.Create;
var
 X : Integer;
 C : Byte;
begin
 Inherited;
 SetFrameAmount(0);

 // Make Black and White Palette as fallback!
 for X := 0 to 254 do
 begin
  C := Trunc(X/254*255);
  FPalette[X+1] := SetRGBTriple(C,C,C);
 end;

 FTransCol     := SetRGBTriple(1,2,255);
 FPalette[0]   := FTransCol;
 FSaveExtra    := False;//True;
 FOneFrameLoad := False;
 FSideCol      := SetRGBTriple(255,0,0);
 FUseSideCol   := False;

 FKeepHeaders  := False;
 ZeroMemory(@FStoredHeaders,SizeOf(Pointer)*2);
 FIgnoreFrames := False;
 TransparentShadows := False;

 FFilename        := 'unknown.shp';
 FAutoPalette     := '';

 FPalType         := sptDefault;
 FPaletteISO      := Nil;

 FAmbientLight    := 0;
 FForceFlags      := 0;
 FCompressionMode := 0;
 FSaved           := False;
 FWasLoaded       := False;
end;

procedure TSHP.FreeDebugHeaders;
begin
 if Assigned(FStoredHeaders[0]) then
 begin
  FreeMem(FStoredHeaders[0]);
  FStoredHeaders[0] := Nil;
 end;
 if Assigned(FStoredHeaders[1]) then
 begin
  FreeMem(FStoredHeaders[1]);
  FStoredHeaders[1] := Nil;
 end;
end;

procedure TSHP.Clear;
var
 X : Integer;
begin
 for X := 0 to High(FFrames) do
 begin
  FreeMem(FFrames[X].PalData);
  FreeMem(FFrames[X].RGBData);
  FreeMem(FFrames[X].AData);
  FreeMem(FFrames[X].SideCol);
 end;

 SetFrameAmount(0);

 if Assigned(FPaletteISO) then
 begin
  FreeMem(FPaletteISO);
  FPaletteISO := Nil;
 end;

 FreeDebugHeaders;
end;

destructor TSHP.Destroy;
begin
 Inherited;

 Clear;
end;

procedure TSHP.SetTransCol(Value : TRGBTriple);
begin
 FTransCol   := Value;

 MarkNeedUpd;
end;

function GetPalType(EXT : AnsiString) : TSHP_PalType;
var
 X : Integer;
begin
 for X := 0 to Ord(sptWin) do
 if EXT = Pal_EXTS[X].EXT then
 begin
  Result := Pal_EXTS[X].PalType;
  Exit;
 end;

 Result := sptDefault;
end;

procedure TSHP.LoadFromFile(Filename : AnsiString);
var
 Stream : TStream;
begin
 Clear;

 FPalType := GetPalType(ExtractFileExt(Filename));

 Stream := TFileStream.Create(Filename,fmOpenRead or fmShareDenyWrite);
  FFilename := Filename;
  LoadFromStream(Stream);
 Stream.Free;
end;

procedure TSHP.SaveToFile(Filename : AnsiString);
var
 Stream : TStream;
begin
 FFilename := Filename;

 Stream := TFileStream.Create(Filename,fmCreate);
  SaveToStream(Stream);
 Stream.Free;
end;

function SetArea(X,Y,W,H : Word) : TImgArea;
begin
 Result[0] := X;
 Result[1] := Y;
 Result[2] := W;
 Result[3] := H;
end;

function TSHP.GetFrame(Index : Integer) : PSHP_Frame;
begin
 Result := @FFrames[Index];
end;

function TSHP.GetRadarCol(Index : Integer) : TColor;
begin
 Result := TColor(RGB(FFrames[Index].RadarCol.rgbRed,FFrames[Index].RadarCol.rgbGreen,FFrames[Index].RadarCol.rgbBlue));
end;

procedure TSHP.SetRadarCol(Index : Integer; Value : TColor);
begin
 FFrames[Index].RadarCol.rgbRed   := GetRValue(Value);
 FFrames[Index].RadarCol.rgbGreen := GetRValue(Value);
 FFrames[Index].RadarCol.rgbBlue  := GetRValue(Value);
 FFrames[Index].RadarCol.rgbReserved := 0;
end;

function TSHP.GetRGBFrame(Index : Integer) : PRGBTriple;
begin
 BuildFrameRGB(Index);

 Result := FFrames[Index].RGBData;
end;

function TSHP.GetAlphaFrame(Index : Integer) : PByte;
begin
 BuildFrameRGB(Index);

 Result := FFrames[Index].AData;
end;

function TSHP.GetSideColFrame(Index : Integer) : PByte;
begin
 BuildFrameRGB(Index);

 Result := FFrames[Index].SideCol;
end;

procedure TSHP.FreeFrameMem(Index : Integer);
begin
 if (Index < 0) or (Index > High(FFrames)) then Exit;

 if Assigned(FFrames[Index].AData) then
 begin
  FreeMem(FFrames[Index].AData);
  FFrames[Index].AData := Nil;
 end;
 if Assigned(FFrames[Index].SideCol) then
 begin
  FreeMem(FFrames[Index].SideCol);
  FFrames[Index].SideCol := Nil;
 end;
 if Assigned(FFrames[Index].RGBData) then
 begin
  FreeMem(FFrames[Index].RGBData);
  FFrames[Index].RGBData := Nil;
 end;
end;

function MakeBlankLineRaw(Input : Pointer; L,Size : Integer) : Pointer;
var
 X : Integer;
 T : Pointer;
begin
 GetMem(Result,L*Size);
 T := Result;
 for X := 0 to L-1 do
 begin
  CopyMemory(T,Input,Size);
  Inc(Cardinal(T),Size);
 end;
end;

function MakeBlankLine(Color : TRGBTriple; L : Integer) : PRGBTriple;
begin
 Result := MakeBlankLineRaw(@Color,L,3);
end;

procedure EmptyFrameBuffer(Buffer,Line : Pointer; W,H,Size : Integer);
var
 T : Pointer;
 Y : Integer;
begin
 T := Buffer;
 for Y := 0 to H-1 do
 begin
  CopyMemory(T,Line,W*Size);
  Inc(Cardinal(T),W*Size);
 end;
end;

procedure TSHP.EmptyFrameRGB(Index : Integer);
var
 Blank : PRGBTriple;
begin
 if not Assigned(FFrames[Index].AData) then
 GetMem(FFrames[Index].AData,FW*FH);
 ZeroMemory(FFrames[Index].AData,FW*FH);

 if not Assigned(FFrames[Index].SideCol) then
 GetMem(FFrames[Index].SideCol,FW*FH);
 ZeroMemory(FFrames[Index].SideCol,FW*FH);

 FFrames[Index].NeedUpd := False;

 Blank := MakeBlankLine(FTransCol,FW);
  EmptyFrameBuffer(FFrames[Index].RGBData,Blank,FW,FH,3);
 FreeMem(Blank);
end;

function MakeBlankLineRGBA(Color : TRGBTriple; L : Integer) : PRGBQuad;
var
 C : TRGBQuad;
begin
 C.rgbRed      := Color.rgbtRed;
 C.rgbGreen    := Color.rgbtGreen;
 C.rgbBlue     := Color.rgbtBlue;
 C.rgbReserved := 0;

 Result := MakeBlankLineRaw(@C,L,4);
end;

procedure TSHP.EmptyFrameRGBA(Index : Integer; Buffer : Pointer);
var
 Blank : PRGBQuad;
begin
 Blank := MakeBlankLineRGBA(FTransCol,FW);
  EmptyFrameBuffer(Buffer,Blank,FW,FH,4);
 FreeMem(Blank);
end;

function LS(Data : Pointer; Y : Integer; Area : TImgArea; W : Integer; Bytes : Byte = 1) : Pointer;
begin
 Result := Pointer(Cardinal(Data) + ((Y+Area[IMGAREA_Y])*W+Area[IMGAREA_X])*Bytes);
end;

function RGBLineStart(Data : Pointer; Y : Integer; Area : TImgArea; W : Integer) : Pointer;
begin
 Result := LS(Data,Y,Area,W,3);
end;

function RGBALineStart(Data : Pointer; Y : Integer; Area : TImgArea; W : Integer) : Pointer;
begin
 Result := LS(Data,Y,Area,W,4);
end;

function PALLineStart(Data : Pointer; Y : Integer; Area : TImgArea; W : Integer) : Pointer;
begin
 Result := LS(Data,Y,Area,W,1);
end;

procedure TSHP.BuildFrameRGB(Index : Integer);
const
 ShadowPal : Array [sgTD..sgRA2] of Byte = (4,4,0,0);
 ShadowCol : TRGBTriple = (rgbtBlue : 0; rgbtGreen : 0; rgbtRed : 0;);
var
 RGB : PRGBTriple;
 PAL : PByte;
 AD,
 SD  : PByte;
 X,Y : Integer;
 SP : Byte;
 TempPal : TSHP_Palette;
 isShadowFrame : Boolean;
begin
 if (not FFrames[Index].NeedUpd and Assigned(FFrames[Index].RGBData)) then
 Exit;

 if (FW = 0) or (FH = 0) then Exit;

 if not Assigned(FFrames[Index].RGBData) then
 GetMem(FFrames[Index].RGBData,FW*FH*3);

 EmptyFrameRGB(Index);

 isShadowFrame := (FGame in [sgTS,sgRA2]) and (FType in stBuildingOrUnit) and (Index > 0) and (Index >= Count div 2);

 SP := 0;                   

 if FTransparentShadows then
 if (FGame in [sgTD,sgRA1]) or (isShadowFrame) then
   SP := ShadowPal[FGame];

 TempPal := Palette[Index]; //Palette rather than FPalette as we want FTransCol to be Palette[0]

 for Y := 0 to FFrames[Index].UsedArea[IMGAREA_H]-1 do
 begin
  RGB := RGBLineStart(FFrames[Index].RGBData,Y,FFrames[Index].UsedArea,FW);
  PAL := PALLineStart(FFrames[Index].PalData,Y,FFrames[Index].UsedArea,FW);
  AD  := PALLineStart(FFrames[Index].AData,Y,FFrames[Index].UsedArea,FW);
  SD  := PALLineStart(FFrames[Index].SideCol,Y,FFrames[Index].UsedArea,FW);
  for X := 0 to FFrames[Index].UsedArea[IMGAREA_W]-1 do
  begin
   RGB^ := TempPal[PAL^];

   if (PAL^ >= 16) and (PAL^ <= 31) then
   SD^ := 255;

   if isShadowFrame then
   begin
    if (PAL^ > 0) then
    if FTransparentShadows then
    begin
     RGB^ := ShadowCol;
     AD^  := 127;
    end
    else
    AD^  := 255;
   end
   else
   begin
    if (PAL^ > 0) then
    begin
     if (SP > 0) and (PAL^ = SP) then// ((PAL^ in [1,4,12]{SP}){ or (PAL^ = 1)}) then
     begin
      RGB^ := ShadowCol;
      AD^  := 127;//+(PAL^*6);
     end
     else
     AD^ := 255;
    end
    else
    if not IsTransparent then
     AD^ := 255;
   end;

   Inc(Cardinal(RGB),3);
   Inc(Cardinal(PAL),1);
   Inc(Cardinal(AD),1);
   Inc(Cardinal(SD),1);
  end;
 end;
end;

function TSHP.BuildFrameRGBA(Index : Integer) : PRGBQuad; //NOT TESTED!!! Needs to be updated to match RGB version!
const
 ShadowPal : Array [sgTD..sgRA2] of Byte = (4,4,1,12);
 ShadowCol : TRGBTriple = (rgbtBlue : 0; rgbtGreen : 0; rgbtRed : 0;);
var
 RGBA : Pointer;
 PAL  : PByte;
 X,Y  : Integer;
 SP   : Byte;
 Sha  : Boolean;
 TempPal : TSHP_Palette;
begin
 Result := Nil;
 if (FW = 0) or (FH = 0) then Exit;

 GetMem(Result,FW*FH*4);

 EmptyFrameRGBA(Index,Result);

 SP := 0;

 if FTransparentShadows then
 if (FType in stBuildingorUnit) then// if (FGame in [sgRA1,sgTS,sgRA2]) then
 begin
  if (FType = stUnit) or (FGame in [sgTD,sgRA1]) then
   SP := ShadowPal[FGame]
  else
   SP := 1;
 end;

 Sha     := False;
 TempPal := Palette[Index]; //Palette rather than FPalette as we want FTransCol to be Palette[0]

 for Y := 0 to FFrames[Index].UsedArea[IMGAREA_H]-1 do
 begin
  RGBA := RGBALineStart(Result,Y,FFrames[Index].UsedArea,FW);
  PAL  := PALLineStart(FFrames[Index].PalData,Y,FFrames[Index].UsedArea,FW);
  for X := 0 to FFrames[Index].UsedArea[IMGAREA_W]-1 do
  begin
   if (PAL^ > 0) then
   begin
    if (SP > 0) and ((PAL^ = SP) or (PAL^ = 1)) then
    begin
     PByte(RGBA)^ := 127;
     Sha := True;
    end
    else
     PByte(RGBA)^ := 255;
   end
   else
   if not IsTransparent then
    PByte(RGBA)^ := 255;

   Inc(Cardinal(RGBA),1);
   if Sha then
   begin
    PRGBTriple(RGBA)^ := ShadowCol;
    Sha := False;
   end
   else
    PRGBTriple(RGBA)^ := TempPal[PAL^];

   Inc(Cardinal(RGBA),3);
   Inc(Cardinal(PAL),1);
  end;
 end;
end;

//Based on Decode3 from SHP_File.Pas
procedure TSHP.Decode3(Input : TStream; Frame : Integer);
var
 Output : Pointer;
 Count  : Word;
 V      : Byte;
 Y      : Integer;
begin
 try
  for Y := 0 to FFrames[Frame].UsedArea[IMGAREA_H]-1 do
  begin
   Output := PALLineStart(FFrames[Frame].PalData,Y,FFrames[Frame].UsedArea,FW);
   Input.Read(Count,2);
   Dec(Count,2);
   while Count > 0 do
   begin
    Dec(Count);
    Input.Read(V,1);
    if V <> 0 then
    begin
     Byte(Output^) := V;
     Inc(Cardinal(Output),1);
    end
    else
    begin
     Dec(Count);
     Input.Read(V,1);
     Inc(Cardinal(Output),V);
    end;
   end;
  end;
 except
 end;
end;

procedure TSHP.Decode1(Input : TStream; Frame : Integer);
var
 Y : Integer;
begin
 try
  for Y := 0 to FFrames[Frame].UsedArea[IMGAREA_H]-1 do
   Input.Read(PALLineStart(FFrames[Frame].PalData,Y,FFrames[Frame].UsedArea,FW)^,FFrames[Frame].UsedArea[IMGAREA_W]);
 except

 end;
end;

const
 EXTRA_VER = 0;

procedure TSHP.ReadExtra(Stream : TStream);
var
 Header : TSHP_Extra_Header;
 Extra  : TSHP_Extra;
begin
 Stream.Seek(-SizeOf(TSHP_Extra_Header),soFromEnd);
 Stream.Read(Header,SizeOf(TSHP_Extra_Header));
 if not ((Header.ID[0] = #0) and (Header.ID[1] = 'E') and (Header.ID[2] = 'X') and (Header.ID[3] = 'T') and (Header.ID[4] = 'R') and (Header.ID[5] = 'A')) then Exit;

 //Note: Previous version headers should NEVER change, Versions should add extra headers.
 //      Thus always forwards compatible (Opening with older app).

 Stream.Seek(-(SizeOf(TSHP_Extra_Header)+SizeOf(TSHP_Extra)),soFromEnd);
 Stream.Read(Extra,SizeOf(TSHP_Extra));

 FGame    := TSHP_Game(Extra.Game);
 FType    := TSHP_Type(Extra._Type);
 SetPalette(Extra.Palette);
 FSHPExtra := True;
end;

procedure TSHP.WriteExtra(Stream : TStream);
var
 Header : TSHP_Extra_Header;
 Extra  : TSHP_Extra;
begin
 if not FSaveExtra then Exit;

 Stream.Seek(0,soFromEnd);

 Extra.Game    := Ord(FGame);
 Extra._Type   := Ord(FType);
 Extra.Palette := FPalette;

 Stream.Write(Extra,SizeOf(TSHP_Extra));

 Header.ID[0] := #0;
 Header.ID[1] := 'E';
 Header.ID[2] := 'X';
 Header.ID[3] := 'T';
 Header.ID[4] := 'R';
 Header.ID[5] := 'A';
 Header.Ver   := EXTRA_VER;

 Stream.Write(Header,SizeOf(TSHP_Extra_Header));
 FSHPExtra := True;
end;

function TSHP.LoadTSSHPFromStream(Stream : TStream) : Boolean;
var
 Header        : TSHP_Header;
 Image_Header,
 Image_Headers : PSHP_Header_Image;
 X             : Integer;
 Mem           : TMemoryStream;
 MemStart      : Int64;
 C             : Integer;
begin
 Stream.Read(Header,SizeOf(TSHP_Header));
 if (Header.Zero <> 0) or (Header.Width = 0) or (Header.Height = 0) or (Header.Count = 0) then
 begin
  if (Header.Zero = 0) and (Header.Count = 1) then //Special Case for TS's Null.shp
  begin
   Result := True;
   FW     := 0;
   FH     := 0;
   SetFrameAmount(1);
  end
  else
   Result := False;
  Exit;
 end;

 FGame := sgTS;

 FW := Header.Width;
 FH := Header.Height;

 C := Header.Count;

 if FOneFrameLoad then
 begin
  FindSHPType(Header.Count);
  if ((FType = stAnimation) and (Header.Count mod 2 = 0)) or ((FType in stBuildingOrUnit) and (FGame in [sgTS,sgRA2])) then // Load 2 Frames if we are an Animation so far so we can check for shadows
   Header.Count := 2
  else
  begin
   Header.Count := 1;
   C            := Header.Count;
  end;
 end;

 SetFrameAmount(Header.Count);
 GetMem(Image_Headers,SizeOf(TSHP_Header_Image)*C);
 Stream.Read(Image_Headers^,SizeOf(TSHP_Header_Image)*C);

 MemStart := Stream.Position;
 //Load the rest of the file in a MemoryStream to reduce overheads
 Mem := TMemoryStream.Create;
 Mem.CopyFrom(Stream,Stream.Size-Stream.Position);
 Mem.Position := 0;

 if not FOneFrameLoad then
 if Image_Headers^.Offset <> MemStart then
 asm nop end;

 Image_Header := Image_Headers;
 for X := 0 to Header.Count-1 do
 begin
  if FOneFrameLoad and (C <> Header.Count) and (X = 1) then
  begin
   Image_Header := Image_Headers;
   Inc(Cardinal(Image_Header),SizeOf(TSHP_Header_Image)*(C div 2)); // Load the first shadow frame (Assuming it is one)
  end;

  FFrames[X].PalData  := Nil;
  FFrames[X].RGBData  := Nil;
  FFrames[X].AData    := Nil;
  FFrames[X].SideCol  := Nil;
  FFrames[X].NeedUpd  := True;
  FFrames[X].UsedArea := SetArea(0,0,0,0);

  if Image_Header^.offset <> 0 then
  begin
   FFrames[X].UsedArea := SetArea(Image_Header^.X,Image_Header^.Y,Image_Header^.W,Image_Header^.H);
   FFrames[X].RadarCol.rgbRed      := Image_Header^.RadarColor.rgbtRed;
   FFrames[X].RadarCol.rgbGreen    := Image_Header^.RadarColor.rgbtGreen;
   FFrames[X].RadarCol.rgbBlue     := Image_Header^.RadarColor.rgbtBlue;
   FFrames[X].RadarCol.rgbReserved := Image_Header^.Unknown1;
   GetMem(FFrames[X].PalData,FW*FH);
   ZeroMemory(FFrames[X].PalData,FW*FH);


   Mem.Seek(Image_Header^.offset-MemStart,soFromBeginning);
   if not FIgnoreFrames then
   if (Image_Header^.Flags and TS_FLAG_COMPRESSION) > 0 then
   Decode3(Mem,X)
   else
   Decode1(Mem,X);
  end;

  Inc(Cardinal(Image_Header),SizeOf(TSHP_Header_Image));
 end;

 if FKeepHeaders then
 begin
  GetMem(FStoredHeaders[0],SizeOf(TSHP_Header));
  CopyMemory(FStoredHeaders[0],@Header,SizeOf(TSHP_Header));
  FStoredHeaders[1] := Image_Headers;
 end
 else
 FreeMem(Image_Headers);

 ReadExtra(Stream);

 Mem.Free;
 Result := True;
end;

// read_w from SHP_RA_Code.Pas
function read_w(var Source: PByte): word;
begin
   Result := word(PWord(Source)^);
   Inc(Source, 2);
end;

var
 DecodeError : Boolean = False;

// decode80d from SHP_RA_Code.Pas
function decode80d(const image_in: PByte; var image_out: PByte; const maxsize : Integer): integer;
var
   copyp, readp, writep: PByte;
   code, Counter: integer;
   Max : Integer;
begin
   Max := maxsize;
   readp  := image_in;
   writep := image_out;
   DecodeError := False;
   while (True) do
   begin
      if DecodeError then Break;
      code := readp^;
      Inc(readp);
      if ((not code) and $80) <> 0 then
      begin
         //bit 7 = 0
         //command 0 (0cccpppp p): copy
         Counter := (code shr 4) + 3;
         integer(copyp) := integer(writep) - (((code and $f) shl 8) + readp^);
         Inc(readp);
         while (Counter <> 0) do
         begin
            Dec(Counter);
            writep^ := copyp^;
            Inc(writep);
            Inc(copyp);
            Dec(max);
            if Max = 0 then
                  begin
                   DecodeError := True;
                   Break;
                  end;
         end;
      end
      else
      begin
         //bit 7 = 1
         Counter := code and $3f;
         if ((not code) and $40) <> 0 then
         begin
            //bit 6 = 0
            if (Counter = 0) then
               //end of image
               break;
            //command 1 (10cccccc): copy
            while (Counter <> 0) do
            begin
               Dec(Counter);
               writep^ := readp^;
               Inc(writep);
               Inc(readp);
               Dec(max);
               if Max = 0 then
                  begin
                   DecodeError := True;
                   Break;
                  end;
            end;
         end
         else
         begin
            //bit 6 = 1
            if (Counter < $3e) then
            begin
               //command 2 (11cccccc p p): copy
               Inc(Counter, 3);
               copyp := image_out;
               Inc(copyp, read_w(readp));
               while (Counter <> 0) do
               begin
                  Dec(Counter);
                  writep^ := copyp^;
                  Inc(writep);
                  Inc(copyp);
                  Dec(max);
                  if Max = 0 then
                  begin
                   DecodeError := True;
                   Break;
                  end;
               end;
            end
            else
            if (Counter = $3e) then
            begin
               //command 3 (11111110 c c v): fill
               Counter := read_w(readp);
               code    := readp^;
               Inc(readp);
               while (Counter <> 0) do
               begin
                  Dec(Counter);
                  writep^ := byte(code);
                  Inc(writep);
                  Dec(max);
                  if Max = 0 then
                  begin
                   DecodeError := True;
                   Break;
                  end;
               end;
            end
            else
            begin
               //command 4 (copy 11111111 c c p p): copy
               Counter := read_w(readp);
               copyp   := image_out;
               Inc(copyp, read_w(readp));
               while (Counter <> 0) do
               begin
                  try
                  Dec(Counter);
                  writep^ := copyp^;
                  Inc(writep);
                  Inc(copyp);
                  Dec(max);
                  if Max = 0 then
                  begin
                   DecodeError := True;
                   Break;
                  end;
                  except
                   DecodeError := True;
                   Break;
                  end;
               end;
            end;
         end;
      end;
   end;
   //  assert(cb_in == readp - image_in);
   Result := integer(writep) - integer(image_out);
end;

// decode40Tri from SHP_RA_Code.Pas
function Decode40Tri(const Source: PByte; const XorDest: PByte; var Dest: PByte): integer;
var
   SP:      PByte;
   XP:      PByte;
   DP:      PByte;
   Counter: integer;
   Code:    integer;
begin
   SP := Source;
   XP := XorDest;
   DP := Dest;
   while True do
   begin
      Code := SP^;
      Inc(SP);
      if ((not Code) and $80) <> 0 then
      begin
         //bit 7 = 0
         if (Code = 0) then
         begin
            //command 0 (00000000 c v): fill
            Counter := SP^;
            Inc(SP);
            Code := SP^;
            Inc(SP);
            while (Counter > 0) do
            begin
               Dec(Counter);
               DP^ := XP^ xor Code;
               Inc(XP);
               Inc(DP);
            end;
         end
         else
         begin
            //command 1 (0ccccccc): copy
            Counter := Code;
            while (Counter > 0) do
            begin
               Dec(Counter);
               DP^ := XP^ xor SP^;
               Inc(XP);
               Inc(DP);
               Inc(SP);
            end;
         end;

      end
      else
      begin
         //bit 7 = 1
         Counter := Code and $7f;
         if (Counter = 0) then
         begin
            Counter := read_w(SP);
            Code    := Counter shr 8;
            if ((not code) and $80) <> 0 then
            begin
               //bit 7 = 0
               //command 2 (10000000 c 0ccccccc): skip
               if (Counter = 0) then
               begin
                  // end of image
                  Result := integer(DP) - integer(Dest);
                  exit;
               end;
               while Counter > 0 do
               begin
                  DP^ := XP^;
                  Inc(DP);
                  Inc(XP);
                  Dec(Counter);
               end;
            end
            else
            begin
               //bit 7 = 1
               Counter := Counter and $3fff;
               if ((not Code) and $40) <> 0 then
               begin
                  //bit 6 = 0
                  //command 3 (10000000 c 10cccccc): copy
                  while (Counter > 0) do
                  begin
                     Dec(Counter);
                     DP^ := XP^ xor SP^;
                     Inc(XP);
                     Inc(DP);
                     Inc(SP);
                  end;
               end
               else
               begin
                  //bit 6 = 1
                  //command 4 (10000000 c 11cccccc v): fill
                  Code := SP^;
                  Inc(SP);
                  while (Counter > 0) do
                  begin
                     Dec(Counter);
                     DP^ := XP^ xor Code;
                     Inc(XP);
                     Inc(DP);
                  end;
               end;
            end;
         end
         else
         begin
            //command 5 (1ccccccc): skip
            while Counter > 0 do
            begin
               DP^ := XP^;
               Inc(DP);
               Inc(XP);
               Dec(Counter);
            end;
         end;
      end;
   end;
end;

procedure TSHP.TDRA1Decode(Input : TStream; DecodeFrame,Frame : Integer);
var
 I : PByte;
begin
 GetMem(I,FW*FH);
 ZeroMemory(I,FW*FH);
 Input.Read(I^,FW*FH);

 if DecodeFrame = -1 then
 try
 if decode80d(I,FFrames[Frame].PalData,FW*FH) <> FW*FH then
 asm nop end;
 except
  asm nop end;
 end
 else
 decode40Tri(I,FFrames[DecodeFrame].PalData,FFrames[Frame].PalData);

 FreeMem(I);
end;

function FindOffset(Offset : Cardinal; Header : PSHP_Offset_TD; Frame : Integer) : Integer;
begin
 Result := Frame;
 Dec(Cardinal(Header),SizeOf(TSHP_Offset_TD));
 Dec(Result);
 if Result < 0 then Exit;

 While Result > -1 do
 begin
  if ThreeByteToCardinal(Header^.Offset) = Offset then
   Exit;
  Dec(Result);
  Dec(Cardinal(Header),SizeOf(TSHP_Offset_TD));
 end;
end;

function TSHP.LoadTDRASHPFromStream(Stream : TStream) : Boolean;
var
 Header      : TSHP_Header_TD;
 Offsets     : PSHP_Offset_TD;
 Offset      : PSHP_Offset_TD;
 ExtraOffset : TSHP_Offset_TD;
 Mem         : TMemoryStream;
 MemStart    : Int64;
 X,
 DecodeFrame : Integer;
 C           : Integer;
 T           : Cardinal;
begin
 Result := False;
 Stream.Read(Header,SizeOf(TSHP_Header_TD));
 if (Header.Count = 0) or (Header.Width = 0) or (Header.Height = 0) then
 begin
  Exit;
 end;

 FGame := sgTD;

 FW := Header.Width;
 FH := Header.Height;

 C := Header.Count;

 if FOneFrameLoad then
 begin
  FindSHPType(Header.Count);
  Header.Count := 1;
 end;

 SetFrameAmount(Header.Count);

 GetMem(Offsets,SizeOf(TSHP_Offset_TD)*Header.Count);
 Stream.Read(Offsets^,SizeOf(TSHP_Offset_TD)*Header.Count);

 T := ThreeByteToCardinal(Offsets^.Offset)-(SizeOf(TSHP_Offset_TD)*C+SizeOf(TSHP_Header_TD));
 if not (FOneFrameLoad) and (T = 16) then
 begin
  Stream.Read(ExtraOffset,SizeOf(TSHP_Offset_TD));

  Offset := Offsets;
  Inc(Cardinal(Offset),SizeOf(TSHP_Offset_TD)*Header.Count);

  if ThreeByteToCardinal(ExtraOffset.Offset) <> Stream.Size then
  asm nop end;

  FTD_LoopFrame := FindOffset(ThreeByteToCardinal(ExtraOffset.Offset),Offset,Header.Count);
  if FTD_LoopFrame = Header.Count then
  FTD_LoopFrame := -1;
 end
 else
 FTD_LoopFrame := -1;

 MemStart := Stream.Position;
 //Load the rest of the file in a MemoryStream to reduce overheads
 Mem := TMemoryStream.Create;
 Mem.CopyFrom(Stream,Stream.Size-Stream.Position);
 Mem.Position := 0;

 Offset := Offsets;

 for X := 0 to High(FFrames) do
 begin
  FFrames[X].PalData := Nil;
  FFrames[X].RGBData := Nil;
  FFrames[X].AData   := Nil;
  FFrames[X].SideCol := Nil;
  FFrames[X].NeedUpd := True;

   FFrames[X].UsedArea := SetArea(0,0,FW,FH);
   SetRadarCol(X,0);
   GetMem(FFrames[X].PalData,FW*FH);
   ZeroMemory(FFrames[X].PalData,FW*FH);

   if not FIgnoreFrames then
   begin
    case Offset^.Compression of //Work out the DecodeFrame!
     $80 : DecodeFrame := -1;
     $40 : DecodeFrame := FindOffset(ThreeByteToCardinal(Offset^.ReOffset),Offset,X);
     $20 : DecodeFrame := X-1;
     else
     begin
      FFrames[X].UsedArea := SetArea(0,0,0,0);
      DecodeFrame := -2; //Error ignore frame
      asm nop end; //Break Point
     end;
    end;

    if DecodeFrame > -2 then
    begin
     Mem.Seek(ThreeByteToCardinal(Offset^.Offset)-MemStart,soFromBeginning);
     TDRA1Decode(Mem,DecodeFrame,X);
     if DecodeError then
     begin
      {FFrames[X].PalData  := Nil;
      FFrames[X].UsedArea := SetArea(0,0,0,0); }
     end;
    end;
   end;

  Inc(Cardinal(Offset),SizeOf(TSHP_Offset_TD));
 end;

 ReadExtra(Mem);
 Mem.Free;

 if FKeepHeaders then
 begin
  GetMem(FStoredHeaders[0],SizeOf(TSHP_Header_TD));
  CopyMemory(FStoredHeaders[0],@Header,SizeOf(TSHP_Header_TD));
  FStoredHeaders[1] := Offsets;
 end
 else
  FreeMem(Offsets);

 Result := True;
end;

procedure TSHP.IsRA2NotTSUnitCheck;
begin
 if (FType = stUnit) and (FGame = sgTS) then
 if ScanFrameForColor(12,0) then
 FGame := sgRA2;

 if (FType = stAnimation) then
 if ScanFrameForColor(1,(High(FFrames)+1) div 2) then
 FType := stBuilding;//stBuildAnim;

 if (FGame = sgRA2) and (FType = stBuilding) and (Count = 8) then
 FType := stIsoSnoBuilding;
end;

procedure TSHP.LoadFromStream(Stream : TStream);
begin
 FType       := stUnit;
 FSHPExtra   := False;
 FreeDebugHeaders;

 FSHPFileType    := GetSHPFileType(Stream);
 Stream.Position := 0;

 case FSHPFileType of
  sftNone : Exit;
  sftTD   : LoadTDRASHPFromStream(Stream);
  sftTS   : LoadTSSHPFromStream(Stream);
 end;

 if not FSHPExtra then
 begin
  if not FOneFrameLoad then
  FindSHPType(Count);
  if FSHPFileType = sftTS then
  IsRA2NotTSUnitCheck;
 end;

 FSaved     := True;
 FWasLoaded := True;
end;

procedure TSHP.GetUsedAreaFrame(Buffer : Pointer; Frame : Integer);
var
 Y   : Integer;
 S,D : Pointer;
begin
 D := Buffer;
 for Y := 0 to FFrames[Frame].UsedArea[IMGAREA_H]-1 do
 begin
  S := PALLineStart(FFrames[Frame].PalData,Y,FFrames[Frame].UsedArea,FW);
  CopyMemory(D,S,FFrames[Frame].UsedArea[IMGAREA_W]);
  Inc(Cardinal(D),FFrames[Frame].UsedArea[IMGAREA_W]);
 end;
end;

function GetEncode3(Data : Pointer; var Size : Integer; const W,H : Integer) : Pointer;
var
 Y,X      : Integer;
 C        : Word;
 Temp,T,
 RS,Input : Pointer;
begin
 Input := Data;
 GetMem(Result,W*H+(H*10));
 RS := Result;
 GetMem(Temp,W*2);
 try
 for Y := 0 to H-1 do
 begin
  T  := Temp;
  C  := 0;
  for X := 0 to W-1 do
  begin
   if Byte(Input^) <> 0 then
   begin
    if (C > 0) then
    begin
     Byte(T^) := 0;
     Inc(Cardinal(T),1);
     Byte(T^) := C;
     Inc(Cardinal(T),1);
     C := 0;
    end;

    Byte(T^) := Byte(Input^);
    Inc(Cardinal(T),1);
   end
   else
   begin
    Inc(C);
    if C >= 255 then
    begin
     Byte(T^) := 0;
     Inc(Cardinal(T),1);
     Byte(T^) := C;
     Inc(Cardinal(T),1);
     C := 0;
    end;
   end;

   Inc(Cardinal(Input),1);
  end;

  if (C > 0) then
  begin
   Byte(T^) := 0;
   Inc(Cardinal(T),1);
   Byte(T^) := C;
   Inc(Cardinal(T),1);
  end;

  C          := (Cardinal(T)-Cardinal(Temp))+2;
  PWord(RS)^ := C;
  Inc(Cardinal(RS),2);
  if C <= 2 then Continue;

  CopyMemory(RS,Temp,C-2);
  Inc(Cardinal(RS),C-2);
 end;

 Size := Cardinal(RS)-Cardinal(Result);
 except
  Size := 99999999;
  FreeMem(Result);
  Result := Nil;
 end;
 FreeMem(Temp);
end;

function TSHP.GetFrameAverageColour(Frame : Integer) : TRGBTriple;
var
 R,G,B   : Integer;
 C       : Integer;
 S       : PByte;
 Palette : TSHP_Palette;
 X,Y     : Integer;
begin
 R := 0;
 G := 0;
 B := 0;
 C := 0;

 Palette := FPalette;

 for Y := 0 to FFrames[Frame].UsedArea[IMGAREA_H]-1 do
 begin
  S := PALLineStart(FFrames[Frame].PalData,Y,FFrames[Frame].UsedArea,FW);
  for X := 0 to FFrames[Frame].UsedArea[IMGAREA_W]-1 do
  begin
   if S^ > 0 then
   begin
    Inc(R,Palette[S^].rgbtRed);
    Inc(G,Palette[S^].rgbtGreen);
    Inc(B,Palette[S^].rgbtBlue);
    Inc(C);
    if C >= 2550 then
    begin
     R := R div C;
     G := G div C;
     B := B div C;
     C := 0;
    end;
   end;
   Inc(Cardinal(S),1);
  end;
 end;

 if C > 0 then
 begin
  R := R div C;
  G := G div C;
  B := B div C;
 end;

 Result := SetRGBTriple(Min(Max(R,255),0),Min(Max(G,255),0),Min(Max(B,255),0));
end;

procedure TSHP.SaveTSRA2SHPToStream(Stream : TStream);
var
 Header       : TSHP_Header;
 Header_Imgs  : PSHP_Header_Image;
 Header_Img   : PSHP_Header_Image;
 ImgHeadStart : Int64;
 X,Size,
 LastSize     : Integer;
 FrameBuffs   : Array [0..1] of Pointer;
 Half         : Integer;
begin
 Header.Zero   := 0;
 Header.Width  := FW;
 Header.Height := FH;
 Header.Count  := High(FFrames)+1;

 Stream.Write(Header,SizeOf(TSHP_Header));
 ImgHeadStart := Stream.Position;
 GetMem(Header_Imgs,SizeOf(TSHP_Header_Image)*Header.Count);
 ZeroMemory(Header_Imgs,SizeOf(TSHP_Header_Image)*Header.Count);
 Stream.Write(Header_Imgs^,SizeOf(TSHP_Header_Image)*Header.Count);

 Half := Header.Count div 2;

 Header_Img := Header_Imgs;
 for X := 0 to Header.Count-1 do
 begin
  Header_Img^.X           := FFrames[X].UsedArea[IMGAREA_X];
  Header_Img^.Y           := FFrames[X].UsedArea[IMGAREA_Y];
  Header_Img^.W           := FFrames[X].UsedArea[IMGAREA_W];
  Header_Img^.H           := FFrames[X].UsedArea[IMGAREA_H];
  Header_Img^.Offset      := Stream.Position;
  Header_Img^.RadarColor  := GetFrameAverageColour(X);//SetRGBTriple(FFrames[X].RadarCol.rgbRed,FFrames[X].RadarCol.rgbGreen,FFrames[X].RadarCol.rgbBlue);
  Header_Img^.Unknown1    := 0;//FFrames[X].RadarCol.rgbReserved;
  Header_Img^.Flags       := FForceFlags;

  //Header_Img^.Unknown     := $CCCC; //TEST!

  if ScanFrameForColor(0,X) then
  Header_Img^.Flags := Header_Img^.Flags or TS_FLAG_TRANSPARENT;

  if (Header_Img^.W = 0) or (Header_Img^.H = 0) then
  begin
   Header_Img^.Offset := 0;
   Continue;
  end;

  LastSize := FFrames[X].UsedArea[IMGAREA_W]*FFrames[X].UsedArea[IMGAREA_H];

  ZeroMemory(@FrameBuffs[0],SizeOf(Pointer)*3);

  GetMem(FrameBuffs[0],LastSize);
  GetUsedAreaFrame(FrameBuffs[0],X);

  try
  if (CompressionMode in [COMPMODE_AUTO,COMPMODE_ALL] ) or ((CompressionMode = COMPMODE_HALF) and (X < Half)) then
  begin
   FrameBuffs[1] := GetEncode3(FrameBuffs[0],Size,FFrames[X].UsedArea[IMGAREA_W],FFrames[X].UsedArea[IMGAREA_H]);
   if (Size >= LastSize) and not (CompressionMode in [COMPMODE_ALL,COMPMODE_HALF])  then
   begin
    FreeMem(FrameBuffs[1]);
    FrameBuffs[1] := Nil;
   end
   else
   begin
    LastSize := Size;
    Header_Img^.Flags := Header_Img^.Flags or TS_FLAG_COMPRESSION;
   end;
  end;

  if HasFlag(Header_Img^.Flags,TS_FLAG_COMPRESSION) then
   Stream.Write(FrameBuffs[1]^,LastSize)
  else
   Stream.Write(FrameBuffs[0]^,LastSize);

  FreeMem(FrameBuffs[0]);
  if Assigned(FrameBuffs[1]) then
  FreeMem(FrameBuffs[1]);
  except
   asm nop end;
  end;

  Inc(Cardinal(Header_Img),SizeOf(TSHP_Header_Image));
 end;

 WriteExtra(Stream);

 Stream.Position := ImgHeadStart;
 Stream.Write(Header_Imgs^,SizeOf(TSHP_Header_Image)*Header.Count);
end;

procedure TSHP.SaveToStream(Stream : TStream);
begin
 Assert(not FOneFrameLoad,'Can not save a SHP with OneFrameLoad enabled....');

 if FSHPFileType = sftTS then
  SaveTSRA2SHPToStream(Stream)
 else
  ;
  //SaveTDRASHPToStream(Stream);

 FSaved     := True;
 FWasLoaded := True;
end;

procedure TSHP.SetPalette(Palette : TSHP_Palette);
begin
 CopyMemory(@FPalette[0],@Palette[0],SizeOf(TSHP_Palette));

 MarkNeedUpd;
end;

procedure TSHP.SetPaletteRGBQ(RGBQ : array of TRGBQuad);
var
 X : Integer;
begin
 SHPExtra := True; //Stops it from auto assigning a palette!

 for X := 0 to 255 do
 FPalette[X] := QuadToTriple(RGBQ[X]);

 MarkNeedUpd;
end;

procedure TSHP.MarkNeedUpd;
var
 X : Integer;
begin
 for X := 0 to High(FFrames) do
 FFrames[X].NeedUpd := True;
end;

function TSHP.GetCount() : Integer;
begin
 Result := High(FFrames)+1;
end;

procedure LoadAPalette(var Palette : TSHP_Palette; Filename : AnsiString);
var
 Stream : TStream;
 X      : Integer;
 T      : Byte;
begin
 Stream := TFileStream.Create(Filename,fmOpenRead or fmShareDenyWrite);
  Stream.Read(Palette[0],SizeOf(TSHP_Palette));
 Stream.Free;

 for X := 0 to 255 do
 begin
  T                    := Palette[X].rgbtBlue;
  Palette[X].rgbtBlue  := Palette[X].rgbtRed*4;
  Palette[X].rgbtGreen := Palette[X].rgbtGreen*4;
  Palette[X].rgbtRed   := T*4;
 end;
end;

procedure TSHP.LoadPalette(Filename : AnsiString);
begin
 LoadAPalette(FPalette,Filename);

 MarkNeedUpd;
end;

function TSHP.GetISOPalette : TSHP_Palette;
var
 Pal : AnsiString;
begin
 if not Assigned(FPaletteISO) then
 begin
  GetMem(FPaletteISO,SizeOf(TSHP_Palette));
  
  if FType = stIsoSnoBuilding then
  Pal := 'isosno'
  else
  if FType = stIsoTemBuilding then
  Pal := 'isotem'
  else
  Pal := 'isourb';

  LoadAPalette(FPaletteIso^,SHP_Palette_Directory + SHP_GameF_Str[Ord(FGame)] + '\'+Pal+'.pal');
 end;

 Result := FPaletteIso^;
end;

procedure TSHP.LoadPaletteFromDir(Directory : AnsiString);
var
 FN : AnsiString;
begin
 Directory := AddTrailer(Directory) + SHP_GameF_Str[Ord(FGame)] + '\';

 if (SHPPalettesOver[FGame,FPalType] <> '') then
 FN := SHPPalettesOver[FGame,FPalType]
 else
 if (SHPPalettes[FGame,FType] = '') then
 FN := SHPPalettes[FGame,stUnit]
 else
 FN := SHPPalettes[FGame,FType];

 FAutoPalette := '';

 if FileExists(Directory + FN + '.pal') then
 begin
  LoadPalette(Directory + FN + '.pal');
  FAutoPalette := SHP_GameF_Str[Ord(FGame)] + '\' + FN + '.pal';
 end
 else
 if FileExists(Directory + 'unittem.pal') then
  LoadPalette(Directory + 'unittem.pal');
end;

function Min3(A,B,C : Single) : Single;
begin
 Result := Min(Min(A,B),C);
end;

type
 THSLArray = Array [0..2] of Single;
const
 HSL_H = 0;
 HSL_S = 1;
 HSL_L = 2;

// RGBToHSL - http://www.easyrgb.com/index.php?X=MATH&H=18#text18
function RGBToHSL(RGB : TRGBTriple) : THSLArray;
var
 Rf,Gf,Bf,
 Minf,Maxf,
 del_max,
 dR,dG,dB  : Single;
begin
 Rf := ( RGB.rgbtRed / 255 );                    //RGB from 0 to 255
 Gf := ( RGB.rgbtGreen / 255 );
 Bf := ( RGB.rgbtBlue / 255 );

 Minf := min(min(Rf,Gf),Bf);    //Min. value of RGB
 Maxf := max(max(Rf,Gf),Bf);    //Max. value of RGB
 del_Max := Maxf - Minf;        //Delta RGB value

 Result[HSL_L] := (Maxf + Minf) / 2;

 if ( del_Max = 0 ) then                     //This is a gray, no chroma...
 begin
  Result[HSL_H] := 0;                                //HSL results from 0 to 1
  Result[HSL_S] := 0
 end
 else                                    //Chromatic data...
 begin
  if ( Result[HSL_L] < 0.5 ) then
   Result[HSL_S] := del_Max / (Maxf + Minf)
  else
   Result[HSL_S] := del_Max / ( 2 - Maxf - Minf );

  dR := ( ( ( Maxf - Rf ) / 6 ) + ( del_Max / 2 ) ) / del_Max;
  dG := ( ( ( Maxf - Gf ) / 6 ) + ( del_Max / 2 ) ) / del_Max;
  dB := ( ( ( Maxf - Bf ) / 6 ) + ( del_Max / 2 ) ) / del_Max;

  if ( Rf = Maxf ) then
   Result[HSL_H] := dB - dG
  else
  if ( Gf = Maxf ) then
   Result[HSL_H] := ( 1 / 3 ) + dR - dB
  else
  if ( Bf = Maxf ) then
   Result[HSL_H] := ( 2 / 3 ) + dG - dR;

  if ( Result[HSL_H] < 0 ) then
  Result[HSL_H] := Result[HSL_H] + 1;
  if ( Result[HSL_H] > 1 ) then
  Result[HSL_H] := Result[HSL_H] - 1;
 end;
end;

// HSLToRGB - http://www.easyrgb.com/index.php?X=MATH&H=18#text18
function HSLToRGB(HSL : THSLArray) : TRGBTriple;

function Hue_2_RGB( v1, v2, vH : Single ) : Single;             //Function Hue_2_RGB
begin
   if ( vH < 0 ) then
    vH := vH + 1;
   if ( vH > 1 ) then
    vH := vH - 1;

   if ( ( 6 * vH ) < 1 ) then
    Result := ( v1 + ( v2 - v1 ) * 6 * vH )
   else
   if ( ( 2 * vH ) < 1 ) then
    Result := v2
   else
   if ( ( 3 * vH ) < 2 ) then
    Result := ( v1 + ( v2 - v1 ) * ( ( 2 / 3 ) - vH ) * 6 )
   else
    Result := v1;
end;

var
 v1,v2 : Single;
begin
 if ( HSL[HSL_S] = 0 ) then                       //HSL from 0 to 1
 begin
  Result.rgbtRed   := Trunc(HSL[HSL_L] * 255);                      //RGB results from 0 to 255
  Result.rgbtGreen := Result.rgbtRed;
  Result.rgbtBlue  := Result.rgbtRed;
 end
 else
 begin
  if ( HSL[HSL_L] < 0.5 ) then
   v2 := HSL[HSL_L] * ( 1 + HSL[HSL_S] )
  else
   v2 := ( HSL[HSL_L] + HSL[HSL_S] ) - ( HSL[HSL_S] * HSL[HSL_L] );

  v1 := 2 * HSL[HSL_L] - v2;

  Result.rgbtRed   := Trunc(255 * Hue_2_RGB( v1, v2, HSL[HSL_H] + ( 1 / 3 ) ));
  Result.rgbtGreen := Trunc(255 * Hue_2_RGB( v1, v2, HSL[HSL_H] ));
  Result.rgbtBlue  := Trunc(255 * Hue_2_RGB( v1, v2, HSL[HSL_H] - ( 1 / 3 ) ));
 end;
end;

function TSHP.GetPalette(Index : Integer) : TSHP_Palette;
var
 X       : Integer;
 R,
 G,
 B       : Extended;
 SideOff : Byte;
 HSL     : THSLArray;
begin
 if (FType in stIsoBuilding) and (Index = 3) then
 begin
  Result := GetISOPalette;
  Exit;
 end;

 Result    := FPalette;
 Result[0] := FTransCol; //Override with the transparent colour we want
 if FUseSideCol then
 begin
  if FGame in [sgTD] then
  begin
   R := (FSideCol.rgbtRed/9);
   G := (FSideCol.rgbtGreen/9);
   B := (FSideCol.rgbtBlue/9);

   SideOff := 16*11;

   for X := 0 to 7 do
    Result[(7-X)+SideOff] := SetRGBTriple(Trunc(R*(X+1)),Trunc(G*(X+1)),Trunc(B*(X+1)));

   R := (FSideCol.rgbtRed/17);
   G := (FSideCol.rgbtGreen/17);
   B := (FSideCol.rgbtBlue/17);

   for X := 0 to 7 do
    Result[(7-X)+8+SideOff] := SetRGBTriple(Trunc(R*(X+5)),Trunc(G*(X+5)),Trunc(B*(X+5)));
  end
  else
  begin
   R := (FSideCol.rgbtRed/17);
   G := (FSideCol.rgbtGreen/17);
   B := (FSideCol.rgbtBlue/17);

   if FGame in [sgRA1] then
   SideOff := 16*5
   else
   SideOff := 16;

   for X := 0 to 15 do
   Result[(15-X)+SideOff] := SetRGBTriple(Trunc(R*(X+2)),Trunc(G*(X+2)),Trunc(B*(X+2)));
  end;
 end;

 if FAmbientLight <> 0 then
 begin
  for X := 1 to 255 do
  if not (X in [240..254]) then
  begin
   HSL := RGBToHSL(Result[X]);
   HSL[HSL_L] := Max(Min(HSL[HSL_L] + (FAmbientLight/255),1),0);
   Result[X] := HSLToRGB(HSL);
  end;
 end;
end;

procedure TSHP.SetSideCol(Value : TRGBTriple);
begin
 FSideCol := Value;
 if FUseSideCol then
 MarkNeedUpd;
end;

procedure TSHP.SetUseSideCol(Value : Boolean);
begin
 if FUseSideCol = Value then
 Exit;

 FUseSideCol := Value;
 MarkNeedUpd;
end;

procedure TSHP.DrawFrameToBMP(Index : Integer; var BMP : TBitmap);
var
 S,D : Pointer;
 Y   : Integer;
begin
 S := RGBFrame[Index];

 BMP.Width  := FW;
 BMP.Height := FH;

 if not Assigned(S) then Exit;

 for Y := 0 to FH-1 do
 begin
  D := BMP.ScanLine[Y];
  CopyMemory(D,S,FW*3);
  Inc(Cardinal(S),FW*3);
 end;
end;

procedure TSHP.DrawFrameToBMP8(Index : Integer; var BMP : TBitmap; const X,Y : Integer);
var
 S,D : Pointer;
 L   : Integer;
begin
 S := FFrames[Index].PalData;
 if not Assigned(S) then Exit;

 for L := 0 to FH-1 do
 begin
  D := BMP.ScanLine[L+Y];
  Inc(Cardinal(D),X);

  CopyMemory(D,S,FW);
  Inc(Cardinal(S),FW);
 end;
end;

//FindSHPType - SHP_Engine.Pas
procedure TSHP.FindSHPType(Count : Integer);
begin
   // Detect cameo:
   if Count in [1,2] then
   begin
    if (FH = 48) then
    begin
     if (FW = 60) and (Ord(FGame) >= Ord(sgTS)) then
     begin
      FType := stCameo;
      FGame := sgRA2;
      Exit;
     end
     else
     if (FW = 64) and not (FGame = sgRA1) then
     begin
      FType := stCameo;
      Exit;
     end;
    end;
   end;

   // Detect non RA2 buildings
   if ((FW mod 24) = 0) and ((FH mod 24) = 0) then
   begin
      if (FGame = sgTS) then
      begin
         if (Count in [2..6]) then
         begin
            FType := stBuilding; // If less than 7 but more than 1 assume building
            Exit;
         end
         else
         begin
            if ((Count mod 2) = 0) then
            begin
               FType := stBuilding;//stBuildAnim;
               Exit;
            end;
         end;
      end
      else if (FGame in [sgTD,sgRA1]) then
      begin
         if Length(ExtractFileName(FFilename)) <= 8 then   // RA1 building names are a maximum of XXXX.shp
         begin
            FType := stBuilding;
            exit;
         end
         else // Buildups are XXXXmake.shp
         begin
            FType := stBuilding;//stBuildAnim;
            exit;
         end;
      end;
   end;

   // Detect RA2 buildings.
   if (FGame in [sgTS,sgRA2]) then
   begin
      if (Count = 6) or (Count = 8) then
      begin
         FGame := sgRA2;
         FType := stBuilding;
         exit;
      end;
   end;

   // Detect random animations
   if (((Count < 100) and (Count > 3)) or ((Count mod 2) <> 0)) then
      FType := stAnimation; // If less than 100 but more than 3 assume animation

   // Detect units
   if ((Count mod 8) = 0) and (Count > 60) then
      FType := stUnit;
end;

function TSHP.GetIsTransparent : Boolean;
begin
 Result := True;//(FType <> stCameo) or (FGame in [sgRA2,sgTS]);
end;

procedure TSHP.SetTransparentShadows(Value : Boolean);
begin
 if FTransparentShadows = Value then Exit;

 FTransparentShadows := Value;
 MarkNeedUpd;
end;

function TSHP.ScanFrameForColor(Color : Byte; Frame : Integer) : Boolean;
var
 Y,X : Integer;
 PAL : PByte;
begin
 Result := False;

 if (Frame < 0) or (Frame > High(FFrames)) or not Assigned(FFrames[Frame].PalData) then Exit;

 for Y := 0 to FFrames[Frame].UsedArea[IMGAREA_H]-1 do
 begin
  PAL := PALLineStart(FFrames[Frame].PalData,Y,FFrames[Frame].UsedArea,FW);
  for X := 0 to FFrames[Frame].UsedArea[IMGAREA_W]-1 do
  begin
   if PAL^ = Color then
   begin
    Result := True;
    Exit;
   end;
   Inc(Cardinal(PAL),1);
  end;
 end;
end;

function TSHP.ShouldDrawShadow : Boolean;
begin
 Result := False;
 if not ((FGame in [sgTS,sgRA2]) and (FType in stBuildingorUnit)) then Exit;

// if (FGame = sgRA2) and (FType = stUnit) then Exit;

 Result := True;   
end;

procedure TSHP.SetType(Value : TSHP_Type);
begin
 if Value = FType then Exit;

 if (FType in stIsoBuilding) and Assigned(FPaletteISO) then
 begin
  FreeMem(FPaletteISO);
  FPaletteISO := Nil;
 end;

 FType := Value;
end;

function TSHP.GetFramePixel(Frame, X, Y : Integer) : Byte;
begin
 if Assigned(FFrames[Frame].PalData) then
 Result := PByte(Pointer(Cardinal(FFrames[Frame].PalData) + (Y*FW+X)))^
 else
 Result := 0;
end;

procedure TSHP.SetAmbientLight(Value : SmallInt);
begin
 if Value = FAmbientLight then Exit;

 FAmbientLight := Value;
 MarkNeedUpd;
end;

procedure TSHP.AddFrames(Amount,Width,Height : Integer);
var
 X, Index : Integer;
begin
 Assert(Amount > 0,'TSHP.AddFrames - Amount <= 0');

 if FSHPFileType = sftNone then
 FSHPFileType := sftTS;

 // WARNING: There is no code to actually update any existing frames
 //          if the width or height changes!!!!!
 FW := Width;
 FH := Height;

 Index := Count;
 SetFrameAmount(Count+Amount);
 ZeroMemory(@FFrames[Index],SizeOf(TSHP_Frame)*Amount);

 for X := Index to Count-1 do
 begin
  FFrames[X].NeedUpd := True;
  GetMem(FFrames[X].PalData,Width*Height);
  ZeroMemory(FFrames[X].PalData,Width*Height);
  FFrames[X].UsedArea := SetArea(0,0,Width,Height);
 end;
end;

procedure TSHP.UpdateUsedArea(Frame : Integer);
var
 X,Y   : Integer;
 Area  : TImgArea;
 Empty : Boolean;
 Pal   : PByte;
begin
 Area  := SetArea(FW-1,FH-1,0,0);
 Empty := True;
 Pal   := FFrames[Frame].PalData;

 if not Assigned(Pal) then Exit;

 for Y := 0 to FH-1 do
 for X := 0 to FW-1 do
 begin
  if Pal^ > 0 then
  begin
   if (X < Area[IMGAREA_X]) then
   Area[IMGAREA_X] := X;
   if (X > Area[IMGAREA_W]) then
   Area[IMGAREA_W] := X;
   if (Y < Area[IMGAREA_Y]) then
   Area[IMGAREA_Y] := Y;
   if (Y > Area[IMGAREA_H]) then
   Area[IMGAREA_H] := Y;
   Empty := False;
  end;

  Inc(Cardinal(Pal),1);
 end;

 if Empty then
  FFrames[Frame].UsedArea := SetArea(0,0,0,0)
 else
 begin
  Area[IMGAREA_W]         := Area[IMGAREA_W]-Area[IMGAREA_X]+1;
  Area[IMGAREA_H]         := Area[IMGAREA_H]-Area[IMGAREA_Y]+1;
  FFrames[Frame].UsedArea := Area;
 end;
end;

procedure TSHP.GetUsedPaletteColoursForFrame(Frame : Integer; Result : PByte);
type
 TPBArray  = Array [0..255] of Byte;
 PTPBArray = ^TPBArray;
var
 X : Integer;
 D : PByte;
 R : PTPBArray;
begin
 if not Assigned(FFrames[Frame].PalData) then Exit;

 D := FFrames[Frame].PalData;
 R := PTPBArray(Result); //Easier if we access it as a Array.
 for X := 0 to FW*FH-1 do
 begin
  R^[D^] := 1; // There is no smart way to do it other than "Brute Force".
  Inc(Cardinal(D),1);
 end;
end;

function TSHP.GetUsedPaletteColours(Frame : Integer) : PByte; // If Frame is -1 it will return colours for all frames. Result is always 256 bytes (Boolean value).
var
 X : Integer;
begin
 GetMem(Result,256);
 ZeroMemory(Result,256);

 if Frame > -1 then
  GetUsedPaletteColoursForFrame(Frame,Result)
 else
  for X := 0 to Count-1 do
  GetUsedPaletteColoursForFrame(X,Result);
end;

procedure TSHP.SetFrameAmount(Value : Integer);
begin
 SetLength(FFrames,Value);
end;

var
X,Y : Integer;

initialization
 //Make sure its clear!
 for X := 0 to Ord(sgRA2) do
 for Y := 0 to Ord(stUnit) do
 SHPPalettes[TSHP_Game(X),TSHP_Type(Y)] := '';

 for X := 0 to Ord(sgRA2) do
 for Y := 0 to Ord(sptWin) do
 SHPPalettesOver[TSHP_Game(X),TSHP_PalType(Y)] := '';

 SHPPalettes[sgTD,stUnit]      := 'temperat';
 SHPPalettesOver[sgTD,sptDes]  := 'desert';
 SHPPalettesOver[sgTD,sptTem]  := 'temperat';
 SHPPalettesOver[sgTD,sptWin]  := 'winter';


 SHPPalettes[sgRA1,stUnit]      := 'temperat';
 SHPPalettes[sgRA1,stBuilding]  := 'temperat';
 SHPPalettes[sgRA1,stCameo]     := 'temperat';
 SHPPalettes[sgRA1,stAnimation] := 'temperat';
// SHPPalettes[sgRA1,stBuildAnim] := 'temperat';
 SHPPalettesOver[sgRA1,sptTem]       := 'temperat';
 SHPPalettesOver[sgRA1,sptSno]       := 'snow';
 SHPPalettesOver[sgRA1,sptInt]       := 'interior';

 SHPPalettes[sgTS,stUnit]       := 'unittem';
 SHPPalettes[sgTS,stBuilding]   := 'unittem';
 SHPPalettes[sgTS,stCameo]      := 'cameo';
 SHPPalettes[sgTS,stAnimation]  := 'anim';
// SHPPalettes[sgTS,stBuildAnim]  := 'unittem';
 SHPPalettesOver[sgTS,sptTem]        := 'isotem';
 SHPPalettesOver[sgTS,sptSno]        := 'isosno';

 SHPPalettes[sgRA2,stUnit]      := 'unittem';
 SHPPalettes[sgRA2,stBuilding]  := 'unittem';
 SHPPalettes[sgRA2,stCameo]     := 'cameo';
 SHPPalettes[sgRA2,stAnimation] := 'anim';
// SHPPalettes[sgRA2,stBuildAnim] := 'unittem';
 SHPPalettesOver[sgRA2,sptTem]       := 'isotem';
 SHPPalettesOver[sgRA2,sptSno]       := 'isosno';
 SHPPalettesOver[sgRA2,sptUrb]       := 'isourb';
 SHPPalettesOver[sgRA2,sptLun]       := 'isolun';
 SHPPalettesOver[sgRA2,sptDes]       := 'isodes';
 SHPPalettesOver[sgRA2,sptNewurb]    := 'isourn';

 SHP_Palette_Directory := ExtractFileDir(ParamStr(0))+'\palette\';

finalization

end.
