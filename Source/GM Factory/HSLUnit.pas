unit HSLUnit;

interface

uses Windows;

type
 THSLArray = Array [0..2] of Single;
const
 HSL_H = 0;
 HSL_S = 1;
 HSL_L = 2;

function HSLToRGB(HSL : THSLArray) : TRGBTriple;
function HSLToRGB2(H,S,L : Single) : TRGBTriple;

function RGBToHSL(RGB : TRGBTriple) : THSLArray;
function GetLumonisity(RGB : TRGBQuad) : Byte; overload;
function GetLumonisity(RGB : TRGBTriple) : Byte; overload;

implementation

uses Math;

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

function HSLToRGB2(H,S,L : Single) : TRGBTriple;
var
 HSL : THSLArray;
begin
 HSL[HSL_H] := H;
 HSL[HSL_S] := S;
 HSL[HSL_L] := L;
 Result := HSLToRGB(HSL);
end;

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

function SetRGBTriple(R,G,B : Byte) : TRGBTriple; overload;
begin
 Result.rgbtRed   := R;
 Result.rgbtGreen := G;
 Result.rgbtBlue  := B;
end;

function GetLumonisity(RGB : TRGBQuad) : Byte; overload;
begin
 Result := Trunc(RGBToHSL(SetRGBTriple(RGB.rgbRed,RGB.rgbGreen,RGB.rgbBlue))[HSL_L]*255);
end;

function GetLumonisity(RGB : TRGBTriple) : Byte; overload;
begin
 Result := Trunc(RGBToHSL(RGB)[HSL_L]*255);
end;

end.
