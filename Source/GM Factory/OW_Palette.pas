unit OW_Palette;

interface

uses Windows, GM_File;

procedure LoadOWPalette(FN : AnsiString; var Palette : TGM_Palette);
procedure SaveOWPalette(FN : AnsiString; Palette : TGM_Palette);

procedure LoadOWPalette16(FN : AnsiString; var Palette : TGM16_Palette);
procedure SaveOWPalette16(FN : AnsiString; Palette : TGM16_Palette);

function hicolor_to_color(col:word): TRGBTriple;
function hicolor_to_tcolor(col:word): Integer;
function color_to_hicolor(col : integer):word;     overload;
function color_to_hicolor(RGBT : TRGBTriple):word; overload;

implementation

uses SysUtils, Graphics, Classes;

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
   Stream.Read(Palette[0],SizeOf(Word)*256);
  Stream.Free;
 end;
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
