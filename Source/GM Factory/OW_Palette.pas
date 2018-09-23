unit OW_Palette;

interface

uses Windows, GM_File, Classes;

procedure LoadOWPalette(FN : AnsiString; var Palette : TGM_Palette); overload;
procedure LoadOWPalette(Stream : TStream; var Palette : TGM_Palette); overload;
procedure SaveOWPalette(FN : AnsiString; Palette : TGM_Palette);

procedure LoadOWPalette16(FN : AnsiString; var Palette : TGM16_Palette); overload;
procedure LoadOWPalette16(Stream : TStream; var Palette : TGM16_Palette); overload;
procedure SaveOWPalette16(FN : AnsiString; Palette : TGM16_Palette);

function hicolor_to_color(col:word): TRGBTriple;
function hicolor_to_tcolor(col:word): Integer;
function color_to_hicolor(col : integer):word;     overload;
function color_to_hicolor(RGBT : TRGBTriple):word; overload;

function rgb2w565(r,g,b:byte):word;
procedure w5652rgb(c:word; var r,g,b:byte);
procedure unfadergba(var r,g,b,a:byte);

implementation

uses SysUtils, Graphics, Math;

function hicolor_to_color(col:word): TRGBTriple;
begin
 Result.rgbtRed   := ((col div $800)*8);
 Result.rgbtGreen := ((col div $20 and $3f)*4);
 Result.rgbtBlue  := ((col and $1f)*8);
end;

function hicolor_to_tcolor(col:word): Integer;
begin
 Result := RGB(((col div $800)*8),((col div $20 and $3f)*4),((col and $1f)*8));
end;

function color_to_hicolor(col : integer):word;
begin
 Result:=col and $F80000 shr 19 + col and $00FC00 shr 5 + col and $0000F8 shl 8;
end;

function color_to_hicolor(RGBT : TRGBTriple):word;
begin
 Result := color_to_hicolor(RGB(RGBT.rgbtRed,RGBT.rgbtGreen,RGBT.rgbtBlue));
end;

function rgb2w565(r,g,b:byte):word;
begin
 Result := (r div 8*$800) + (g div 4 * $20)+ (b div 8);
end;

procedure w5652rgb(c:word; var r,g,b:byte);
begin
 r:=(c div $800)*8;
 g:=(c div $20 and $3f)*4;
 b:=(c and $1f)*8;
end;

function ByteRange(I : Real) : Byte;
begin
 Result := Round(Max(Min(I,255),0));
end;

procedure unfadergba(var r,g,b,a:byte);
var
 rr,rg,rb,ra : real;
begin
 ra := a/255;

 rr := (1/ra)*(r/255);
 rg := (1/ra)*(g/255);
 rb := (1/ra)*(b/255);

 r := ByteRange(rr*255);
 g := ByteRange(rg*255);
 b := ByteRange(rb*255);
end;

//----------------------------------------------------------------------------//

procedure LoadOWPalette(FN : AnsiString; var Palette : TGM_Palette);
var
 Pal16 : TGM16_Palette;
 X     : Integer;
begin
 LoadOWPalette16(FN,Pal16);

 for X := 0 to 255 do
 Palette[X] := hicolor_to_color(Pal16[X]);
end;

procedure LoadOWPalette(Stream : TStream; var Palette : TGM_Palette);
var
 Pal16 : TGM16_Palette;
 X     : Integer;
begin
 LoadOWPalette16(Stream,Pal16);

 for X := 0 to 255 do
 Palette[X] := hicolor_to_color(Pal16[X]);
end;

procedure SaveOWPalette(FN : AnsiString; Palette : TGM_Palette);
var
 Pal16 : TGM16_Palette;
 X     : Integer;
begin
 for X := 0 to 255 do
 Pal16[X] := color_to_hicolor(Palette[X]);

 SaveOWPalette16(FN,Pal16);
end;

procedure LoadOWPalette16(FN : AnsiString; var Palette : TGM16_Palette);
var
 Stream : TStream;
begin
 if FileExists(FN) then
 begin
  Stream := TFileStream.Create(FN,fmOpenRead or fmShareDenyWrite);
   LoadOWPalette16(Stream,Palette);
  Stream.Free;
 end;
end;

procedure LoadOWPalette16(Stream : TStream; var Palette : TGM16_Palette);
begin
 Stream.Read(Palette[0],SizeOf(Word)*256);
end;

procedure SaveOWPalette16(FN : AnsiString; Palette : TGM16_Palette);
var
 Stream : TStream;
begin
 if FileExists(FN) then
 DeleteFile(FN);

 Stream := TFileStream.Create(FN,fmCreate or fmShareDenyRead);
  Stream.Write(Palette[0],SizeOf(Word)*256);
 Stream.Free;
end;

end.
