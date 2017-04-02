unit CheckerUnit;
{
 Started: 2016/08/31
 By: Stuart "Stucuk" Carey
}

interface

uses Graphics, ExtCtrls;

procedure TileImage(Img : TImage; Tile : TBitmap);

var
 CTB2x2   : TBitmap = Nil;
 CTB4x4   : TBitmap = Nil;
 CTB12x12 : TBitmap = Nil;
 CTB16x16 : TBitmap = Nil;

implementation

procedure TileImage(Img : TImage; Tile : TBitmap);
var
 BMP : TBitmap;
 W,H,X,Y : Integer;
begin
 BMP := TBitmap.Create;

 W := (Img.Width div Tile.Width)+2;
 H := (Img.Height div Tile.Height)+2;

 BMP.Width       := Img.Width;
 BMP.Height      := Img.Height;
 BMP.PixelFormat := pf24bit;

 for Y := 0 to H-1 do
 for X := 0 to W-1 do
  BMP.Canvas.Draw(X*Tile.Height,Y*Tile.Width,Tile);

 Img.Picture.Assign(BMP);
end;

procedure MakeCheckerTileBMP12x12;
var
 Y : Integer;
 S : Pointer;
begin
 if Assigned(CTB12x12) then Exit;

 CTB12x12             := TBitmap.Create;
 CTB12x12.Width       := 12;
 CTB12x12.Height      := 12;
 CTB12x12.PixelFormat := pf24bit;

 for Y := 0 to 5 do
 begin
  S := CTB12x12.ScanLine[Y];
  FillChar(S^,6*3,200 div 8);
  Inc(Cardinal(S),6*3);
  FillChar(S^,6*3,250 div 4);
 end;

 for Y := 6 to 11 do
 begin
  S := CTB12x12.ScanLine[Y];
  FillChar(S^,6*3,250 div 4);
  Inc(Cardinal(S),6*3);
  FillChar(S^,6*3,200 div 8);
 end;
end;

procedure MakeCheckerTileBMP16x16;
var
 Y : Integer;
 S : Pointer;
begin
 CTB16x16 := TBitmap.Create;
 CTB16x16.Width  := 16;
 CTB16x16.Height := 16;
 CTB16x16.PixelFormat := pf24bit;

 for Y := 0 to 7 do
 begin
  S := CTB16x16.ScanLine[Y];
  FillChar(S^,8*3,200);
  Inc(Cardinal(S),8*3);
  FillChar(S^,8*3,250);
 end;

 for Y := 8 to 15 do
 begin
  S := CTB16x16.ScanLine[Y];
  FillChar(S^,8*3,250);
  Inc(Cardinal(S),8*3);
  FillChar(S^,8*3,200);
 end;
end;

procedure MakeCheckerTileBMP4x4;
var
 Y : Integer;
 S : Pointer;
begin
 CTB4x4 := TBitmap.Create;
 CTB4x4.Width  := 4;
 CTB4x4.Height := 4;
 CTB4x4.PixelFormat := pf24bit;

 for Y := 0 to 1 do
 begin
  S := CTB4x4.ScanLine[Y];
  FillChar(S^,2*3,200);
  Inc(Cardinal(S),2*3);
  FillChar(S^,2*3,250);
 end;

 for Y := 2 to 3 do
 begin
  S := CTB4x4.ScanLine[Y];
  FillChar(S^,2*3,250);
  Inc(Cardinal(S),2*3);
  FillChar(S^,2*3,200);
 end;
end;

procedure MakeCheckerTileBMP2x2;
var
 Y : Integer;
 S : Pointer;
begin
 CTB2x2 := TBitmap.Create;
 CTB2x2.Width  := 2;
 CTB2x2.Height := 2;
 CTB2x2.PixelFormat := pf24bit;

// for Y := 0 to 0 do
 begin
  S := CTB2x2.ScanLine[0];
  FillChar(S^,3,200);
  Inc(Cardinal(S),3);
  FillChar(S^,3,250);
 end;

// for Y := 1 to 1 do
 begin
  S := CTB2x2.ScanLine[1];
  FillChar(S^,3,250);
  Inc(Cardinal(S),3);
  FillChar(S^,3,200);
 end;
end;

initialization
 MakeCheckerTileBMP2x2;
 MakeCheckerTileBMP4x4;
 MakeCheckerTileBMP12x12;
 MakeCheckerTileBMP16x16;

finalization
 if Assigned(CTB2x2) then
 CTB2x2.Free;
 if Assigned(CTB4x4) then
 CTB4x4.Free;
 if Assigned(CTB12x12) then
 CTB12x12.Free;
 if Assigned(CTB16x16) then
 CTB16x16.Free;

end.
