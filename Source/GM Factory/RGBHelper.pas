unit RGBHelper;

interface

uses Windows;

function SetRGBAQuad(Colour : Integer; A : Byte) : TRGBQuad; overload;
function SetRGBAQuad(R,G,B,A : Byte) : TRGBQuad; overload;
function SetRGBAQuad(RGB : TRGBTriple) : TRGBQuad; overload;
function SetRGBAQuad(RGB : TRGBTriple; A : Byte) : TRGBQuad; overload;
function SetRGBTriple(Colour : Integer) : TRGBTriple; overload;
function SetRGBTriple(R,G,B : Byte) : TRGBTriple; overload;
function SetRGBTriple(RGB : TRGBQuad) : TRGBTriple; overload;

implementation

function SetRGBAQuad(Colour : Integer; A : Byte) : TRGBQuad; overload;
begin
 Result := SetRGBAQuad(GetRValue(Colour),GetGValue(Colour),GetBValue(Colour),A);
end;

function SetRGBAQuad(R,G,B,A : Byte) : TRGBQuad; overload;
begin
 Result.rgbRed      := R;
 Result.rgbGreen    := G;
 Result.rgbBlue     := B;
 Result.rgbReserved := A;
end;

function SetRGBAQuad(RGB : TRGBTriple; A : Byte) : TRGBQuad; overload;
begin
 Result := SetRGBAQuad(RGB.rgbtRed,RGB.rgbtGreen,RGB.rgbtBlue,A);
end;

function SetRGBAQuad(RGB : TRGBTriple) : TRGBQuad; overload;
begin
 Result := SetRGBAQuad(RGB,0);
end;

function SetRGBTriple(Colour : Integer) : TRGBTriple; overload;
begin
 Result := SetRGBTriple(GetRValue(Colour),GetGValue(Colour),GetBValue(Colour));
end;

function SetRGBTriple(R,G,B : Byte) : TRGBTriple; overload;
begin
 Result.rgbtRed   := R;
 Result.rgbtGreen := G;
 Result.rgbtBlue  := B;
end;

function SetRGBTriple(RGB : TRGBQuad) : TRGBTriple; overload;
begin
 Result := SetRGBTriple(RGB.rgbRed,RGB.rgbGreen,RGB.rgbBlue);
end;

end.
