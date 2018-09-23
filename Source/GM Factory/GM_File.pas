{
  Copyright (C) 2016-2017 Stuart Carey

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


unit GM_File;
{
 This file is designed to load Original War GM[S,X,Z,A] files
 By: Stucuk
 Date: 2016/10/29
}

interface

uses Windows, SysUtils, Classes;

const
 GM_Type_X = Ord('X');
 GM_Type_S = Ord('S');
 GM_Type_Z = Ord('Z');
 GM_Type_A = Ord('A');
 GM_Type_E = Ord('E');
 GM_Type_Q = Ord('Q');

 GM_G      = Ord('G');
 GM_Dot    = Ord('.');
 GM_M      = Ord('M');

type
TGM_Frame = Record
 RawData       : Pointer;
 RawDataSize   : Integer;
 RGB,
 ZBuffer       : PByte;
 Alpha         : PByte;

 ExtraData     : Pointer; // First 4 Bytes is always Offset. Anything after is coordinates for placing other GM files onto.
                          // Like a wheel. Note that for convenience we extract the Offset and put it back when saving.
                          // So ignore the first 4 bytes of ExtraData as it will get overwritten when saving.
 ExtraDataSize : Word;
 Offset        : Array [0..1] of SmallInt; // X and Y Offset. Extracted from ExtraData when loading and placed in when saving.
 Size          : Array [0..1] of Word;
end;
PGM_Frame = ^TGM_Frame;

//Note: When loading we ignore everything in the header except skip. Though skip should always be 0.
TGM_Header = Packed Record
 Unknown,              // Always 255
 Small,                // 'g' = small 'G' = normal
 ZBufConverted,        // '.' = true  'M' = false  Note: GMZ's should use '.' as the game will complain otherwise.
 Typ           : Byte; // Matches Type but is ignored by game.
 Skip          : Word;
end;

TGM_Palette   = Array [0..255] of TRGBTriple;
TGM16_Palette = Array [0..255] of Word;

TMinMax = Record
 X : Array [0..1] of Integer;
 Y : Array [0..1] of Integer;
end;

TGM_File = Class(TObject)
protected
 FFrames  : Array of TGM_Frame;
 FType    : Byte;
 FW,FH    : Integer;
 FLoadOneFrame : Boolean; //Load a Single frame. Used for previews.
 procedure ProcessRawData_X(Frame : PGM_Frame);
 procedure ProcessRawData_S(Frame : PGM_Frame);
 procedure ProcessRawData_Z(Frame : PGM_Frame);
 procedure ProcessRawData_A(Frame : PGM_Frame);
 procedure ProcessRawData_E(Frame : PGM_Frame);
 procedure ProcessRawData_Q(Frame : PGM_Frame);
 procedure ProcessRawData(Frame : PGM_Frame);
 procedure MakeRawData_X(var Frame : PGM_Frame);
 procedure MakeRawData_S(var Frame : PGM_Frame);
 procedure MakeRawData_Z(var Frame : PGM_Frame);
 procedure MakeRawData_A(var Frame : PGM_Frame);
 procedure MakeRawData_E(var Frame : PGM_Frame);
 procedure MakeRawData(var Frame : PGM_Frame);
 procedure WriteFrame(ID : Integer; Stream : TStream);
 procedure ReadFrame(ID : Integer; Stream : TStream);
 function GetFrame(ID : Integer) : PGM_Frame;
 function GetFrameCount() : Integer;
 function MinMaxRGB(RGB : PByte; Size : Integer) : TMinMax;
 procedure GetFrameRGBA_TYPE_A(Frame : PGM_Frame; Result : Pointer; OX,OY : Integer);
public
 constructor Create(GM_Type : Byte);
 destructor Destroy; override;

 procedure LoadFromFile(Filename : AnsiString);
 procedure LoadFromStream(Stream : TStream);
 procedure SaveToFile(Filename : AnsiString);
 procedure SaveToStream(Stream : TStream);

 procedure Clear;
 procedure AddFrames(Count : Integer);
 
 function GetFrameRGB(ID : Integer; Palette : TGM_Palette) : PRGBTriple;
 function GetFrameRGBA(ID : Integer; Palette : TGM_Palette) : PRGBQuad;
 function GetFrameRGB16(ID : Integer; Palette : TGM16_Palette) : PWord;
 function GetFrameTrans(ID : Integer) : PByte;
 function GetFrameZBuffer(ID : Integer) : PByte;
 function GetFrameSideCol(ID : Integer) : PByte;
 function GetFrameAlpha(ID : Integer) : PByte;
 procedure SetFrameRGB8(ID : Integer; RGB, ZBuffer : PByte);
 procedure SetFrameRGB(ID : Integer; RGB,Alpha : Pointer);

 procedure WorkOutDimentions;

 property Frame[Index : Integer] : PGM_Frame read GetFrame;
 property FrameCount             : Integer   read GetFrameCount;
 property Width                  : Integer   read FW write FW;
 property Height                 : Integer   read FH write FH;
 property GM_Type                : Byte      read FType write FType;
 property LoadOneFrame           : Boolean   read FLoadOneFrame write FLoadOneFrame;
end;

implementation

uses Math, OW_Palette;

constructor TGM_File.Create(GM_Type : Byte);
begin
 Inherited Create;
 FType         := GM_Type;
 FLoadOneFrame := False;
end;

destructor TGM_File.Destroy;
begin
 Inherited;
 Clear;
end;

procedure TGM_File.Clear;
var
 X : Integer;
 Frame : PGM_Frame;
begin
 for X := 0 to High(FFrames) do
 begin
  Frame := @FFrames[X];
  if Assigned(Frame^.ExtraData) then
  FreeMem(Frame^.ExtraData);
  if Assigned(Frame^.RGB) then
  FreeMem(Frame^.RGB);
  if Assigned(Frame^.ZBuffer) then
  FreeMem(Frame^.ZBuffer);
  if Assigned(Frame^.Alpha) then
  FreeMem(Frame^.Alpha);
 end;

 SetLength(FFrames,0);
end;

procedure TGM_File.AddFrames(Count : Integer);
var
 Cur : Integer;
begin
 Cur := High(FFrames)+1;
 SetLength(FFrames,Cur+Count);

 ZeroMemory(@FFrames[Cur],SizeOf(TGM_Frame)*Count);
end;

procedure TGM_File.WorkOutDimentions;
var
 X : Word;
begin
 FW := 0;
 FH := 0;
 for X := 0 to High(FFrames) do
 begin
  if FFrames[X].Size[0] > FW then
  FW := FFrames[X].Size[0];
  if FFrames[X].Size[1] > FH then
  FH := FFrames[X].Size[1];

  if odd(FW) then inc(FW);
  if odd(FH) then inc(FH);
 end;

 for X := 0 to High(FFrames) do
 begin
  if FW div 2 + FFrames[X].Offset[0] < 0 then
  FW := FW + abs(FW div 2 + FFrames[X].Offset[0])*2;

  if FH div 2 + FFrames[X].Offset[1] < 0 then
  FH := FH + abs(FH div 2 + FFrames[X].Offset[1])*2;

  if odd(FW) then inc(FW);
  if odd(FH) then inc(FH);

  if FW < abs(FFrames[X].Offset[0] + FFrames[X].Size[0])*2 then
  FW := abs(FFrames[X].Offset[0] + FFrames[X].Size[0])*2;

  if FH < abs(FFrames[X].Offset[1] + FFrames[X].Size[1])*2 then
  FH := abs(FFrames[X].Offset[1] + FFrames[X].Size[1])*2;

  if odd(FW) then inc(FW);
  if odd(FH) then inc(FH);
 end;
end;

procedure TGM_File.LoadFromStream(Stream : TStream);
var
 Header : TGM_Header;
 Count  : Word;
 X      : Integer;
begin
 Stream.Read(Header,SizeOf(TGM_Header));
 Stream.Seek(Header.Skip,soFromCurrent);
 Stream.Read(FType,SizeOf(Byte));
 FType := Ord(UpCase(Char(FType)));
 Stream.Read(Count,SizeOf(Word));
 if FLoadOneFrame and (Count > 0) then
 Count := 1;
 SetLength(FFrames,Count);

 for X := 0 to High(FFrames) do
 ReadFrame(X,Stream);

 WorkOutDimentions;
end;

procedure IncZRGB(var Z,RGB : PByte; const Count : Integer);
begin
 Inc(Cardinal(Z),Count);
 Inc(Cardinal(RGB),Count);
end;

procedure IncD(Var D : PByte; Const Amount : Integer; Var C : Integer);
begin
 Inc(Cardinal(D),Amount);
 Inc(C,Amount);
end;

procedure SetD(Var D : PByte; Const Value : Byte; Var C : Integer);
begin
 D^ := Value;
 IncD(D,1,C);
end;

procedure SetD2(Var D : PByte; Const Value1,Value2 : Byte; Var C : Integer);
begin
 SetD(D,Value1,C);
 SetD(D,Value2,C);
end;

procedure TGM_File.ProcessRawData_Z(Frame : PGM_Frame);
var
 D,Z,RGB : PByte;
 C       : Integer;
begin
 D   := Frame^.RawData;
 Z   := Frame^.ZBuffer;
 RGB := Frame^.RGB;
 Inc(Cardinal(D),Frame^.Size[1]*4); // Skip the Line Index since we don't use it

 C    := Frame^.Size[1]*4;

 While C < Frame^.RawDataSize do
 begin
  if (D^ = 0) then
  begin
   IncD(D,1,C);
   IncZRGB(Z,RGB,D^);
   IncD(D,1,C);
  end
  else
  begin
   RGB^ := D^;
   IncD(D,1,C);
   Z^   := D^;
   IncD(D,1,C);
   IncZRGB(Z,RGB,1);
  end;
 end;

 FreeMem(Frame^.RawData);
 Frame^.RawData := Nil;
end;

procedure FillColour(var RGB : PByte; Const Colour : Byte; Count : Byte);
begin
 FillChar(RGB^,Count,Colour);
 Inc(Cardinal(RGB),Count);
end;

procedure TGM_File.ProcessRawData_X(Frame : PGM_Frame);
var
 D,RGB : PByte;
 Count,
 Mode : Byte;
 C    : Integer;
begin
 D   := Frame^.RawData;
 RGB := Frame^.RGB;

 C    := 0;

 While C < Frame^.RawDataSize do
 begin
  Mode := D^;
  IncD(D,1,C);

  if (Mode and $80) = 0 then
   FillColour(RGB,0,Mode and $7F)
  else
  begin
   Count  := Mode and $3F;
   if (Mode and $40) = 0 then
   begin
    FillColour(RGB,D^,Count);
    IncD(D,1,C);
   end
   else
   begin
    CopyMemory(RGB,D,Count);
    Inc(Cardinal(RGB),Count);
    IncD(D,Count,C);
   end;
  end;

 end;

 FreeMem(Frame^.RawData);
 Frame^.RawData := Nil;
end;

procedure TGM_File.ProcessRawData_S(Frame : PGM_Frame);
var
 D,RGB : PByte;
 C     : Integer;
begin
 D   := Frame^.RawData;
 RGB := Frame^.RGB;

 C    := 0;
 try
 While C < Frame^.RawDataSize do
 begin
  if (D^ and $80) = $80 then
   FillColour(RGB,1,D^ and $7F)
  else
   FillColour(RGB,0,D^);

  IncD(D,1,C);
 end;
 except
  asm nop end;
 end;

 if (Cardinal(RGB)-Cardinal(Frame^.RGB)) <> Frame^.Size[0]*Frame^.Size[1] then
 asm nop end;


 FreeMem(Frame^.RawData);
 Frame^.RawData := Nil;
end;

procedure SetPByteValue(var P : PByte; const Data : Byte);
begin
 P^ := Data;
 Inc(Cardinal(P),1);
end;

procedure TGM_File.ProcessRawData_A(Frame : PGM_Frame);
var
 D,A,RGB : PByte;
 C,S,X   : Integer;
 R,G,B   : Byte;
begin
 D   := Frame^.RawData;
 RGB := Frame^.RGB;
 A   := Frame^.Alpha;
              
 C    := 0;
 try
 While C < Frame^.RawDataSize do
 begin
  if (D^ and $80) = $80 then
  begin
   A^ := ((D^ and 15) * 32) - 4;
   IncD(D,1,C);
   w5652rgb(Word(Pointer(D)^),r,g,b);
   IncD(D,2,C);
   unfadergba(r,g,b,A^);
   SetPByteValue(RGB,B);
   SetPByteValue(RGB,G);
   SetPByteValue(RGB,R);
   Inc(Cardinal(A),1);
  end
  else
  begin
   S := D^ and $7f;
   IncD(D,1,C);
   if S = 0 then S := 128;
   Inc(Cardinal(RGB),S*3);
   Inc(Cardinal(A),S);
  end;
 end;
 except
  asm nop end;
 end;

 FreeMem(Frame^.RawData);
 Frame^.RawData := Nil;
end;

procedure TGM_File.ProcessRawData_E(Frame : PGM_Frame);
var
 D,A,RGB : PByte;
 C,S,X   : Integer;
 R,G,B   : Byte;
begin
 D   := Frame^.RawData;
 RGB := Frame^.RGB;
 A   := Frame^.Alpha;
              
 C   := 0;
 try
 While C < Frame^.RawDataSize do
 begin
  if D^ > 0 then
  begin
   A^   := 255;
   RGB^ := D^;
  end;           

  Inc(Cardinal(RGB),3);
  Inc(Cardinal(A),1);
  IncD(D,1,C);
 end;
 except
  asm nop end;
 end;

 FreeMem(Frame^.RawData);
 Frame^.RawData := Nil;
end;

procedure TGM_File.ProcessRawData_Q(Frame : PGM_Frame);
var
 D,A,RGB : PByte;
 C,S,X   : Integer;
 R,G,B   : Byte;
begin
 D   := Frame^.RawData;
 RGB := Frame^.RGB;

 if (Frame^.Size[0]*Frame^.Size[1]) <> Frame^.RawDataSize then
 begin
  asm nop end;
  Exit;
 end;

 CopyMemory(Frame^.RGB,Frame^.RawData,Frame^.RawDataSize);

 FreeMem(Frame^.RawData);
 Frame^.RawData := Nil;
end;

procedure TGM_File.ProcessRawData(Frame : PGM_Frame);
begin
 case FType of
  GM_Type_X : ProcessRawData_X(Frame);
  GM_Type_S : ProcessRawData_S(Frame);
  GM_Type_Z : ProcessRawData_Z(Frame);
  GM_Type_A : ProcessRawData_A(Frame);
  GM_Type_E : ProcessRawData_E(Frame);
  GM_Type_Q : ProcessRawData_Q(Frame);
 else
  asm nop end;
 end;
end;

procedure TGM_File.ReadFrame(ID : Integer; Stream : TStream);
var
 Frame : PGM_Frame;
begin
 Frame := @FFrames[ID];

 Frame^.RGB       := Nil;
 Frame^.RawData   := Nil;
 Frame^.ZBuffer   := Nil;
 Frame^.Alpha     := Nil;
 Frame^.ExtraData := Nil;
 Frame^.Offset[0] := 0;
 Frame^.Offset[1] := 0;

 Stream.Read(Frame^.ExtraDataSize,SizeOf(Word));
 if (Frame^.ExtraDataSize > 0) then
 begin
  GetMem(Frame^.ExtraData,Frame^.ExtraDataSize);
  Stream.Read(Frame^.ExtraData^,Frame^.ExtraDataSize);

  CopyMemory(@Frame^.Offset[0],Frame^.ExtraData,SizeOf(SmallInt)*2);
 end;

 Stream.Read(Frame^.Size[0],SizeOf(Word)*2);
 Stream.Read(Frame^.RawDataSize,SizeOf(Integer));
 if (Frame^.Size[0] = 0) or (Frame^.Size[1] = 0) or (Frame^.RawDataSize = 0) then
 Exit;

 GetMem(Frame^.RawData,Frame^.RawDataSize);
 Stream.Read(Frame^.RawData^,Frame^.RawDataSize);

 if (FType = GM_Type_Z) then
 begin
  GetMem(Frame^.ZBuffer,Frame^.Size[0]*Frame^.Size[1]);
  ZeroMemory(Frame^.ZBuffer,Frame^.Size[0]*Frame^.Size[1]);
 end;

 if (FType in [GM_Type_A,GM_Type_E]) then
 begin
  GetMem(Frame^.Alpha,Frame^.Size[0]*Frame^.Size[1]);
  ZeroMemory(Frame^.Alpha,Frame^.Size[0]*Frame^.Size[1]);

  GetMem(Frame^.RGB,Frame^.Size[0]*Frame^.Size[1]*3);
  ZeroMemory(Frame^.RGB,Frame^.Size[0]*Frame^.Size[1]*3);
 end
 else
 begin
  GetMem(Frame^.RGB,Frame^.Size[0]*Frame^.Size[1]);
  ZeroMemory(Frame^.RGB,Frame^.Size[0]*Frame^.Size[1]);
 end;

 ProcessRawData(Frame);
end;

function MakeColourRow(Colour : Pointer; Count,Size : Integer) : Pointer;
var
 X : Integer;
 P : Pointer;
begin
 GetMem(Result,Count*Size);
 P := Result;
 for X := 1 to Count do
 begin
  CopyMemory(P,Colour,Size);
  Inc(Cardinal(P),Size);
 end;
end;

procedure CopyRGBRowTriple(var Destination : PRGBTriple; const Source : Pointer; Count : Integer);
begin
 CopyMemory(Destination,Source,3*Count);
 Inc(Cardinal(Destination),3*Count);
end;

procedure CopyRGBRowQuad(var Destination : PRGBQuad; const Source : Pointer; Count : Integer);
begin
 CopyMemory(Destination,Source,4*Count);
 Inc(Cardinal(Destination),4*Count);
end;

procedure CopyRGBRowWord(var Destination : PWord; const Source : Pointer; Count : Integer);
begin
 CopyMemory(Destination,Source,2*Count);
 Inc(Cardinal(Destination),2*Count);
end;

function TGM_File.GetFrameRGB(ID : Integer; Palette : TGM_Palette) : PRGBTriple;
var
 Row   : PRGBTriple;
 RGB   : PRGBTriple;
 RGB8  : PByte;
 Frame : PGM_Frame;
 X,Y,
 OX,OY : Integer;
begin
 GetMem(Result,FW*FH*3);
 Row := MakeColourRow(@Palette[0],FW,3);
 RGB := Result;

 for Y := 0 to FH-1 do //Fill with Background Colour
 CopyRGBRowTriple(RGB,Row,FW);

 FreeMem(Row);

 Frame := @FFrames[ID];

 OX := FW div 2 + Frame.Offset[0];
 OY := FH div 2 + Frame.Offset[1];

 RGB8 := Frame.RGB;
 for Y := 0 to Frame.Size[1]-1 do
 begin
  RGB := Pointer(Cardinal(Result) + ((Y+OY)*FW+OX)*3);
  for X := 0 to Frame.Size[0]-1 do
  begin
   if FType in [GM_Type_A,GM_Type_E] then
   begin
    RGB^ := TRGBTriple(Pointer(RGB8)^);
    Inc(Cardinal(RGB8),3);
   end
   else
   begin
    RGB^ := Palette[RGB8^];
    Inc(Cardinal(RGB8),1);
   end;

   Inc(Cardinal(RGB),3);
  end;
 end;
end;

function TGM_File.GetFrameRGB16(ID : Integer; Palette : TGM16_Palette) : PWord;
var
 Row   : PWord;
 RGB   : PWord;
 RGB8  : PByte;
 Frame : PGM_Frame;
 X,Y,
 OX,OY : Integer;
begin
 GetMem(Result,FW*FH*2);
 Row := MakeColourRow(@Palette[0],FW,2);
 RGB := Result;

 for Y := 0 to FH-1 do //Fill with Background Colour
 CopyRGBRowWord(RGB,Row,FW);

 FreeMem(Row);

 Frame := @FFrames[ID];

 OX := FW div 2 + Frame.Offset[0];
 OY := FH div 2 + Frame.Offset[1];

 RGB8 := Frame.RGB;
 for Y := 0 to Frame.Size[1]-1 do
 begin
  RGB := Pointer(Cardinal(Result) + ((Y+OY)*FW+OX)*2);
  for X := 0 to Frame.Size[0]-1 do
  begin
   if FType in [GM_Type_A,GM_Type_E] then
   begin
    RGB^ := color_to_hicolor(TRGBTriple(Pointer(RGB8)^));
    if RGB^ > 0 then
    asm nop end;
    Inc(Cardinal(RGB8),3);
   end
   else
   begin
    RGB^ := Palette[RGB8^];
    Inc(Cardinal(RGB8),1);
   end;

   Inc(Cardinal(RGB),2);
  end;
 end;
end;

function TGM_File.GetFrameTrans(ID : Integer) : PByte;
var
 Trans : PByte;
 RGB8  : PByte;
 Frame : PGM_Frame;
 X,Y,
 OX,OY : Integer;
 TransCol : Byte;
begin
 GetMem(Result,FW*FH);
 ZeroMemory(Result,FW*FH);

 Frame := @FFrames[ID];

 OX := FW div 2 + Frame.Offset[0];
 OY := FH div 2 + Frame.Offset[1];

 if GM_Type = GM_Type_S then
 TransCol := 127
 else
 TransCol := 255;

 if FType in [GM_Type_A,GM_Type_E] then
  RGB8 := Frame.Alpha
 else
  RGB8 := Frame.RGB;
 for Y := 0 to Frame.Size[1]-1 do
 begin
  Trans := Pointer(Cardinal(Result) + ((Y+OY)*FW+OX)*1);
  for X := 0 to Frame.Size[0]-1 do
  begin
   if RGB8^ > 0 then
    if FType in [GM_Type_A,GM_Type_E] then
     Trans^ := RGB8^
    else
     Trans^ := TransCol;
   Inc(Cardinal(Trans),1);
   Inc(Cardinal(RGB8),1);
  end;
 end;
end;

function TGM_File.GetFrameZBuffer(ID : Integer) : PByte;
var
 ZBuf,
 Z     : PByte;
 Frame : PGM_Frame;
 X,Y,
 OX,OY : Integer;
begin
 GetMem(Result,FW*FH);
 ZeroMemory(Result,FW*FH);

 Frame := @FFrames[ID];

 OX := FW div 2 + Frame.Offset[0];
 OY := FH div 2 + Frame.Offset[1];

 ZBuf := Frame.ZBuffer;
 for Y := 0 to Frame.Size[1]-1 do
 begin
  Z := Pointer(Cardinal(Result) + ((Y+OY)*FW+OX)*1);
  for X := 0 to Frame.Size[0]-1 do
  begin
   if Assigned(ZBuf) then
   begin
    Z^ := ZBuf^;
    Inc(Cardinal(ZBuf),1);
   end
   else
    Z^ := 0;
   Inc(Cardinal(Z),1);
  end;
 end;
end;

function TGM_File.GetFrameAlpha(ID : Integer) : PByte;
var
 Alpha,
 A     : PByte;
 Frame : PGM_Frame;
 X,Y,
 OX,OY : Integer;
begin
 GetMem(Result,FW*FH);
 ZeroMemory(Result,FW*FH);

 Frame := @FFrames[ID];

 OX := FW div 2 + Frame.Offset[0];
 OY := FH div 2 + Frame.Offset[1];

 Alpha := Frame.Alpha;
 for Y := 0 to Frame.Size[1]-1 do
 begin
  A := Pointer(Cardinal(Result) + ((Y+OY)*FW+OX)*1);
  for X := 0 to Frame.Size[0]-1 do
  begin
   if Assigned(Alpha) then
   begin
    A^ := Alpha^;
    Inc(Cardinal(Alpha),1);
   end
   else
    A^ := 0;
   Inc(Cardinal(A),1);
  end;
 end;
end;

function TGM_File.GetFrameSideCol(ID : Integer) : PByte;
var
 Side  : PByte;
 RGB8  : PByte;
 Frame : PGM_Frame;
 X,Y,
 OX,OY : Integer;
begin
 GetMem(Result,FW*FH);
 ZeroMemory(Result,FW*FH);

 Frame := @FFrames[ID];

 OX := FW div 2 + Frame.Offset[0];
 OY := FH div 2 + Frame.Offset[1];

 RGB8 := Frame.RGB;
 for Y := 0 to Frame.Size[1]-1 do
 begin
  Side := Pointer(Cardinal(Result) + ((Y+OY)*FW+OX)*1);
  for X := 0 to Frame.Size[0]-1 do
  begin
   if (RGB8^ > 0) and (RGB8^ < 25) then
   Side^ := 255;
   Inc(Cardinal(Side),1);
   Inc(Cardinal(RGB8),1);
  end;
 end;
end;

function RGBTripleToRGBQuad(Triple : TRGBTriple; Alpha : Byte) : TRGBQuad;
begin
 Result.rgbRed      := Triple.rgbtRed;
 Result.rgbGreen    := Triple.rgbtGreen;
 Result.rgbBlue     := Triple.rgbtBlue;
 Result.rgbReserved := Alpha;
end;

procedure TGM_File.GetFrameRGBA_TYPE_A(Frame : PGM_Frame; Result : Pointer; OX,OY : Integer);
var
 RGB  : PRGBTriple;
 RGBA : PRGBQuad;
 A    : PByte;
 X,Y  : Integer;
begin
 RGB := Pointer(Frame.RGB);
 A   := Frame.Alpha;
 for Y := 0 to Frame.Size[1]-1 do
 begin
  RGBA := Pointer(Cardinal(Result) + ((Y+OY)*FW+OX)*4);
  for X := 0 to Frame.Size[0]-1 do
  begin
   if A^ > 0 then
   RGBA^ := RGBTripleToRGBQuad(RGB^,A^);

   Inc(Cardinal(A),1);
   Inc(Cardinal(RGB),3);
   Inc(Cardinal(RGBA),4);
  end;
 end;
end;

function TGM_File.GetFrameRGBA(ID : Integer; Palette : TGM_Palette) : PRGBQuad;
var
 Row   : PRGBQuad;
 RGB   : PRGBQuad;
 RGB8  : PByte;
 Frame : PGM_Frame;
 X,Y,
 OX,OY : Integer;
 BG : TRGBQuad;
begin
 GetMem(Result,FW*FH*4);
 BG  := RGBTripleToRGBQuad(Palette[0],0);
 Row := MakeColourRow(@BG,FW,4);
 RGB := Result;

 for Y := 0 to FH-1 do //Fill with Background Colour
 CopyRGBRowQuad(RGB,Row,FW);

 FreeMem(Row);

 Frame := @FFrames[ID];

 OX := FW div 2 + Frame.Offset[0];
 OY := FH div 2 + Frame.Offset[1];

 if FType in [GM_Type_A,GM_Type_E] then
 begin
  GetFrameRGBA_TYPE_A(Frame,Result,OX,OY);
  Exit;
 end;

 RGB8 := Frame.RGB;
 for Y := 0 to Frame.Size[1]-1 do
 begin
  RGB := Pointer(Cardinal(Result) + ((Y+OY)*FW+OX)*4);
  for X := 0 to Frame.Size[0]-1 do
  if RGB8^ = 0 then
  begin //We already set the background ones so skip
   Inc(Cardinal(RGB),4);
   Inc(Cardinal(RGB8),1);
  end
  else
  begin
   PRGBTriple(RGB)^ := Palette[RGB8^];
   Inc(Cardinal(RGB),3);
   Inc(Cardinal(RGB8),1);
   PByte(RGB)^ := 255;
   Inc(Cardinal(RGB),1);
  end;
 end;
end;

function TGM_File.MinMaxRGB(RGB : PByte; Size : Integer) : TMinMax;
var
 P,
 X,Y : Integer;
 D   : PByte;
 F   : Boolean;
begin
 Result.X[0] := 999999;
 Result.X[1] := -999999;
 Result.Y[0] := 999999;
 Result.Y[1] := -999999;

 D := RGB;
 P := 0;
 F := False;
 While P < Size do
 begin
  if (D^ > 0) then
  begin
   X := P mod FW;
   Y := P div FW;
   if X < Result.X[0] then
   Result.X[0] := X;
   if X > Result.X[1] then
   Result.X[1] := X;
   if Y < Result.Y[0] then
   Result.Y[0] := Y;
   if Y > Result.Y[1] then
   Result.Y[1] := Y;

   F := True;
  end;
  Inc(P);
  Inc(Cardinal(D),1);
 end;

 if not F then
 begin
  Result.X[0] := 0;
  Result.X[1] := FW-1;
  Result.Y[0] := 0;
  Result.Y[1] := FH-1;
 end;
end;

procedure TGM_File.SetFrameRGB8(ID : Integer; RGB,ZBuffer : PByte);
var
 MM   : TMinMax;
 SR,SZ,
 DR,DZ : PByte;
 Y     : Integer;       
 Frame : PGM_Frame;
begin
 Frame := @FFrames[ID];

 MM := MinMaxRGB(RGB,FW*FH);

 Frame.Size[0]   := (MM.X[1]-MM.X[0])+1;
 Frame.Size[1]   := (MM.Y[1]-MM.Y[0])+1;
 Frame.Offset[0] := MM.X[0];
 Frame.Offset[1] := MM.Y[0];

 if Assigned(Frame.RGB) then
 FreeMem(Frame.RGB);
 if Assigned(Frame.ZBuffer) then
 FreeMem(Frame.ZBuffer);
 if Assigned(Frame.ExtraData) then
 FreeMem(Frame.ExtraData);
 GetMem(Frame.ExtraData,4);
 Frame.ExtraDataSize := 4;

 GetMem(Frame.RGB,Frame.Size[0]*Frame.Size[1]);
 GetMem(Frame.ZBuffer,Frame.Size[0]*Frame.Size[1]);

 for Y := 0 to Frame.Size[1]-1 do
 begin
  SR := Pointer(Cardinal(RGB)           + (FW*(Y+Frame.Offset[1]))+Frame.Offset[0]);
  SZ := Pointer(Cardinal(ZBuffer)       + (FW*(Y+Frame.Offset[1]))+Frame.Offset[0]);
  DR := Pointer(Cardinal(Frame.RGB)     + Frame.Size[0]*Y);
  DZ := Pointer(Cardinal(Frame.ZBuffer) + Frame.Size[0]*Y);

  CopyMemory(DR,SR,Frame.Size[0]);
  CopyMemory(DZ,SZ,Frame.Size[0]);
 end;

 Frame.Offset[0] := Frame.Offset[0]-(FW div 2);
 Frame.Offset[1] := Frame.Offset[1]-(FH div 2);
end;

procedure TGM_File.SetFrameRGB(ID : Integer; RGB,Alpha : Pointer);
var
 MM   : TMinMax;
 SR,SZ,
 DR,DZ : PByte;
 Y     : Integer;       
 Frame : PGM_Frame;
begin
 Frame := @FFrames[ID];

 MM := MinMaxRGB(Alpha,FW*FH);

 Frame.Size[0]   := (MM.X[1]-MM.X[0])+1;
 Frame.Size[1]   := (MM.Y[1]-MM.Y[0])+1;
 Frame.Offset[0] := MM.X[0];
 Frame.Offset[1] := MM.Y[0];

 if Assigned(Frame.RGB) then
 FreeMem(Frame.RGB);
 if Assigned(Frame.Alpha) then
 FreeMem(Frame.Alpha);
 if Assigned(Frame.ExtraData) then
 FreeMem(Frame.ExtraData);
 GetMem(Frame.ExtraData,4);
 Frame.ExtraDataSize := 4;

 GetMem(Frame.RGB,Frame.Size[0]*Frame.Size[1]*3);
 GetMem(Frame.Alpha,Frame.Size[0]*Frame.Size[1]);

 for Y := 0 to Frame.Size[1]-1 do
 begin
  SR := Pointer(Cardinal(RGB)           + ((FW*(Y+Frame.Offset[1]))+Frame.Offset[0])*3);
  SZ := Pointer(Cardinal(Alpha)         + (FW*(Y+Frame.Offset[1]))+Frame.Offset[0]);
  DR := Pointer(Cardinal(Frame.RGB)     + Frame.Size[0]*Y*3);
  DZ := Pointer(Cardinal(Frame.Alpha)   + Frame.Size[0]*Y);

  CopyMemory(DR,SR,Frame.Size[0]*3);
  CopyMemory(DZ,SZ,Frame.Size[0]);
 end;

 Frame.Offset[0] := Frame.Offset[0]-(FW div 2);
 Frame.Offset[1] := Frame.Offset[1]-(FH div 2);
end;

function TGM_File.GetFrame(ID : Integer) : PGM_Frame;
begin
 Result := @FFrames[ID];
end;

function TGM_File.GetFrameCount() : Integer;
begin
 Result := High(FFrames)+1;
end;

procedure TGM_File.MakeRawData_S(var Frame : PGM_Frame);
var
 D,RGB : PByte;
 X,C,S   : Integer;
 Last  : Boolean;
begin
 GetMem(Frame^.RawData,FW*FH*2{Frame^.Size[0]*Frame^.Size[1]});

 D   := Frame^.RawData;
 RGB := Frame^.RGB;

 Frame^.RawDataSize := 0;
 Last := RGB^ > 0;
 C    := 1;
 S := 0;

 for X := 1 to Frame^.Size[0]*Frame^.Size[1]-1 do
 begin
  Inc(Cardinal(RGB),1);

  if S+C > X then
  asm nop end;

  if (Last = (RGB^ > 0)) then
  begin
   Inc(C);
   if C < 127 then
   Continue
   else
   begin
    Dec(C);
    asm nop end;
   end;
  end;

  D^ := C;
  Inc(S,D^);
  if Last then
  Inc(D^,128);

  IncD(D,1,Frame^.RawDataSize);

  Last := RGB^ > 0;
  C    := 1;
 end;

 if S > (Frame^.Size[0]*Frame^.Size[1]) then
 asm nop end;

 D^ := C;
 Inc(S,D^);
 if Last then
 Inc(D^,128);

 IncD(D,1,Frame^.RawDataSize);

 if S <> (Frame^.Size[0]*Frame^.Size[1]) then
 asm nop end;

 ReallocMemory(Frame^.RawData,Frame^.RawDataSize);
end;

type
 TFourBytes = Array [0..3] of Byte;
 PFourBytes = ^TFourBytes;

function IsFourSame(FourBytes : PFourBytes) : Boolean;
begin
 Result := (PFourBytes(FourBytes)[0] = PFourBytes(FourBytes)[1]) and (PFourBytes(FourBytes)[1] = PFourBytes(FourBytes)[2]) and (PFourBytes(FourBytes)[2] = PFourBytes(FourBytes)[3]);
end;

function FindNextFour(RGB : Pointer; Size : Integer) : Integer;
var
 D : Pointer;
 P : Integer;
begin
 Result := Size;
 if Size < 4 then
 Exit;

 D := RGB;
 P := 0;

 While P < Size-3 do
 begin
  if IsFourSame(D) then
  begin
   Result := P;
   Exit;
  end;
  Inc(P);
  Inc(Cardinal(D),1);
 end;
end;

function MatchEnd(RGB : PByte; Size : Integer) : Integer;
var
 Match : Byte;
 P     : Integer;
begin
 Match := RGB^;
 Inc(Cardinal(RGB),1);
 P := 1;
 Result := 1;
 While P < Size do
 begin
  if (Match <> RGB^) then Exit;

  Inc(Result);
  Inc(P);
  Inc(Cardinal(RGB),1);
 end;          
end;

procedure TGM_File.MakeRawData_X(var Frame : PGM_Frame);
var
 D,RGB : PByte;
 P,
 Next,
 Size  : Integer;
 V     : Byte;
begin
 GetMem(Frame^.RawData,FW*FH*2{Frame^.Size[0]*Frame^.Size[1]});

 D   := Frame^.RawData;
 RGB := Frame^.RGB;

 Frame^.RawDataSize := 0;
 P    := 0;
 Size := (Frame^.Size[0]*Frame^.Size[1]);

 while P < Size do
 begin
  Next := FindNextFour(RGB,(Size-1)-P);

  While Next > 0 do
  begin
   V := Min(Next,$3F);
   SetD(D,V or $80 or $40,Frame^.RawDataSize);
   CopyMemory(D,RGB,V);
   IncD(D,V,Frame^.RawDataSize);

   Inc(Cardinal(RGB),V);
   Inc(P,V);
   Dec(Next,V);
  end;

  Next := MatchEnd(RGB,(Size-1)-P);
  While Next > 0 do
  begin
   if (RGB^ = 0) then
   begin
    V := Min(Next,$7F);
    SetD(D,V,Frame^.RawDataSize);
   end
   else
   begin
    V := Min(Next,$3F);
    SetD(D,V or $80,Frame^.RawDataSize);
    SetD(D,RGB^,Frame^.RawDataSize);
   end;

   Inc(Cardinal(RGB),V);
   Inc(P,V);
   Dec(Next,V);
  end;
 end;

 ReallocMemory(Frame^.RawData,Frame^.RawDataSize);
end;

procedure TGM_File.MakeRawData_Z(var Frame : PGM_Frame);
var
 D,RGB,
 ZBuf,
 Line  : PByte;
 X,
 Skip,
 XP  : Integer;
 V,Z   : Byte;
begin
 //Frame^.Size[0]*Frame^.Size[1]
 Frame^.RawData := Nil;
 GetMem(Frame^.RawData,FW*FH*2+(Frame^.Size[1]*4));

 D    := Frame^.RawData;
 Line := D;
 Inc(Cardinal(D),Frame^.Size[1]*4);
 RGB  := Frame^.RGB;
 ZBuf := Frame^.ZBuffer;

 Frame^.RawDataSize := 0;
 Skip := 0;
 XP   := 0;

 for X := 0 to Frame^.Size[0]*Frame^.Size[1]-1 do
 begin
  V := RGB^;
  Z := ZBuf^;   
  IncZRGB(ZBuf,RGB,1);

  if XP >= Frame^.Size[0] then
  begin
   XP := 0;
   if Skip > 0 then
   begin
    SetD2(D,0,Skip,Frame^.RawDataSize);
    Skip := 0;
   end;
  end;

  if XP = 0 then
  begin
   PInteger(Line)^ := Frame^.RawDataSize;
   Inc(Cardinal(Line),4);
  end;

  Inc(XP);

  if (V = 0) then
  begin
   Inc(Skip);
   if Skip = 255 then
   begin
    SetD2(D,0,255,Frame^.RawDataSize);
    Skip := 0;
   end;
   Continue;
  end;

  if Skip > 0 then
  begin
   SetD2(D,0,Skip,Frame^.RawDataSize);
   Skip := 0;
  end;

  SetD2(D,V,Z,Frame^.RawDataSize);
 end;

 if Skip > 0 then
  SetD2(D,0,Skip,Frame^.RawDataSize);

 Inc(Frame^.RawDataSize,Frame^.Size[1]*4);

 Frame^.RawData := ReallocMemory(Frame^.RawData,Frame^.RawDataSize);
end;

function GetAlphaZeroLength(var A : PByte; const Max : Byte) : Integer;
var
 M : Byte;
begin
 Result := 0;
 M := Max;
 While (M > 0) and (A^ = 0) do
 begin
  Inc(Result);
  Dec(M);
  Inc(Cardinal(A),1);
 end;
end;

procedure GetRGBValues(var RGB : PByte; var R,G,B : Byte);
begin
 R := RGB^;
 Inc(Cardinal(RGB),1);
 G := RGB^;
 Inc(Cardinal(RGB),1);
 B := RGB^;
 Inc(Cardinal(RGB),1);
end;

function FadeRGB(Alpha : Byte; Source : Word) : Word;
begin
 case Alpha of
  0 : Result := 0;
  1 : Result := (Source and $C718) shr 3;
  2 : Result := (Source and $E79C) shr 2;
  3 : Result := (Source and $C718) shr 3 + (Source and $E79C) shr 2;
  4 : Result := (Source and $F7DE) shr 1;
  5 : Result := (Source and $C718) shr 3 + (Source and $F7DE) shr 1;
  6 : Result := (Source and $E79C) shr 2 + (Source and $F7DE) shr 1;
  7 : Result := Source - (Source and $C718) shr 3;
  else
   Result := Source;
 end;
end;

procedure TGM_File.MakeRawData_A(var Frame : PGM_Frame);
var
 D,RGB,
 A     : PByte;
 X,
 Skip  : Integer;
 R,G,B,
 Alpha : Byte;
begin             
 Frame^.RawData := Nil;
 GetMem(Frame^.RawData,FW*FH*4*2);

 D    := Frame^.RawData;
 RGB  := Frame^.RGB;
 A    := Frame^.Alpha;

 Frame^.RawDataSize := 0;
 Skip := 0;

 for X := 0 to Frame^.Size[0]*Frame^.Size[1]-1 do
 begin
  if Skip > 0 then
  begin
   Dec(Skip);
   Continue;
  end;

  if A^ = 0 then
  begin
   Skip := GetAlphaZeroLength(A,Max(Min(127,(Frame^.Size[0]*Frame^.Size[1])-X),1));
   Inc(Cardinal(RGB),Skip*3);
   D^ := Skip;
   IncD(D,1,Frame^.RawDataSize);
   Dec(Skip);
   Continue;
  end;

  Alpha := ((A^+4) div 32);
  Inc(Cardinal(A),1);
  D^ := $80 + Alpha;
  IncD(D,1,Frame^.RawDataSize);

  GetRGBValues(RGB,R,G,B);
  Word(Pointer(D)^) := FadeRGB(Alpha,rgb2w565(B,G,R));//{(Alpha * }rgb2w565(B,G,R){) div 8};
  IncD(D,2,Frame^.RawDataSize);
 end;

 Frame^.RawData := ReallocMemory(Frame^.RawData,Frame^.RawDataSize);
end;

procedure TGM_File.MakeRawData_E(var Frame : PGM_Frame);
var
 D,RGB,
 A  : PByte;
 X  : Integer;
begin             
 Frame^.RawData := Nil;
 GetMem(Frame^.RawData,FW*FH*2);

 D    := Frame^.RawData;
 RGB  := Frame^.RGB;
 A    := Frame^.Alpha;

 Frame^.RawDataSize := 0;

 for X := 0 to Frame^.Size[0]*Frame^.Size[1]-1 do
 begin
  if A^ = 0 then
   D^ := 0
  else
   D^ := RGB^;

  IncD(D,1,Frame^.RawDataSize);
  Inc(Cardinal(A),1);
  Inc(Cardinal(RGB),3);
 end;

 Frame^.RawData := ReallocMemory(Frame^.RawData,Frame^.RawDataSize);
end;

procedure TGM_File.MakeRawData(var Frame : PGM_Frame);
begin
 case FType of
  GM_Type_X : MakeRawData_X(Frame);
  GM_Type_S : MakeRawData_S(Frame);
  GM_Type_Z : MakeRawData_Z(Frame);
  GM_Type_A : MakeRawData_A(Frame);
  GM_Type_E : MakeRawData_E(Frame);
 end;
end;

procedure TGM_File.WriteFrame(ID : Integer; Stream : TStream);
var
 Frame : PGM_Frame;
begin
 Frame := @FFrames[ID];
 Frame^.RawDataSize := 0;

 if (Frame^.Size[0]*Frame^.Size[1] > 0) then
 MakeRawData(Frame);

 Stream.Write(Frame^.ExtraDataSize,SizeOf(Word));

 if (Frame^.ExtraDataSize > 0) then
 begin
  CopyMemory(Frame^.ExtraData,@Frame^.Offset[0],SizeOf(SmallInt)*2);
  Stream.Write(Frame^.ExtraData^,Frame^.ExtraDataSize);
 end;

 Stream.Write(Frame^.Size[0],SizeOf(Word)*2);
 Stream.Write(Frame^.RawDataSize,SizeOf(Integer));
 if (Frame^.Size[0] = 0) or (Frame^.Size[1] = 0) or (Frame^.RawDataSize = 0) then
 Exit;

 Stream.Write(Frame^.RawData^,Frame^.RawDataSize);
 FreeMem(Frame^.RawData);
end;

procedure TGM_File.SaveToStream(Stream : TStream);
var
 Header : TGM_Header;
 Count  : Word;
 X      : Integer;
begin
 Header.Unknown        := 255;
 Header.Small          := GM_G;
 if FType = GM_Type_Z then
  Header.ZBufConverted := GM_Dot
 else
  Header.ZBufConverted := GM_M;
 Header.Typ            := FType;
 Header.Skip           := 0;

 Stream.Write(Header,SizeOf(TGM_Header));
 Stream.Write(FType,SizeOf(Byte));
 Count := High(FFrames)+1;
 Stream.Write(Count,SizeOf(Word));

 for X := 0 to High(FFrames) do
 WriteFrame(X,Stream);
end;

procedure TGM_File.LoadFromFile(Filename : AnsiString);
var
 Stream : TStream;
begin
 Stream := TFileStream.Create(Filename,fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Stream);
 Stream.Free;
end;

procedure TGM_File.SaveToFile(Filename : AnsiString);
var
 Stream : TStream;
begin
 if FileExists(Filename) then
 DeleteFile(Filename);

 ForceDirectories(ExtractFileDir(Filename));

 Stream := TFileStream.Create(Filename,fmCreate or fmShareDenyRead);
  SaveToStream(Stream);
 Stream.Free;
end;

end.
