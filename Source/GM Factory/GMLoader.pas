unit GMLoader;

interface

uses GMF;

procedure LoadGM(Filename : AnsiString; OneFrame : Boolean; var Factory : TGMF);
procedure LoadGMZXS(Filename : AnsiString; OneFrame : Boolean; var Factory : TGMF);

implementation

uses Windows, SysUtils, GM_File, OW_Palette;

procedure LoadGMZXS(Filename : AnsiString; OneFrame : Boolean; var Factory : TGMF);
var
 GM      : TGM_File;
 F       : Integer;
 RGB16   : PWord;
 Trans   : PByte;
 Frame   : PGMFFrame;
 Layer   : PGMFLayer;
 Palette : TGM16_Palette;
begin
 for F := 0 to 255 do
 begin
  Palette[F] := color_to_hicolor(RGB(F,F,F));
 end;

 LoadOWPalette16(ChangeFileExt(Filename,'.p16'),Palette);

 GM := TGM_File.Create(0);
  GM.LoadOneFrame := OneFrame;
  GM.LoadFromFile(Filename);

  Factory := TGMF.Create(GM.Width,GM.Height);
  Factory.Filename := FileName;
  Factory.AddFrame(GM.FrameCount);

  for F := 0 to GM.FrameCount-1 do
  begin
   RGB16 := GM.GetFrameRGB16(F,Palette);
   Trans := GM.GetFrameTrans(F);

   Frame := Factory.Frame[F];
   if GM.GM_Type = GM_Type_Z then
   Frame.SideCol := GM.GetFrameSideCol(F);
   if Assigned(GM.Frame[F].ZBuffer) then
   Frame.ZBuff := GM.GetFrameZBuffer(F);

   Frame.ExtraDataSize := GM.Frame[F].ExtraDataSize;
   if Frame.ExtraDataSize > 0 then
   begin
    Dec(Frame.ExtraDataSize,4);
    if Frame.ExtraDataSize > 0 then
    begin
     GetMem(Frame.ExtraData,Frame.ExtraDataSize);
     CopyMemory(Frame.ExtraData,Pointer(Cardinal(GM.Frame[F].ExtraData)+4),Frame.ExtraDataSize);
    end;
   end;

   Layer := Factory.GetLayer(Frame,1);
   Factory.CopyLayer(Layer,RGB16,Trans);
   FreeMem(RGB16);
   FreeMem(Trans);
   Frame.Edited := True;
  end;
 GM.Free;
end;

procedure LoadGM(Filename : AnsiString; OneFrame : Boolean; var Factory : TGMF);
begin
 if Lowercase(ExtractFileExt(FileName)) = '.gmf' then
  Factory := TGMF.Create(FileName,OneFrame)
 else
  LoadGMZXS(FileName,OneFrame,Factory);
end;

end.
