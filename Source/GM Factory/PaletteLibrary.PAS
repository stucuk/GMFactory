//  Palette Library
//  Earl F. Glynn, March 1998

UNIT PaletteLibrary;

INTERFACE

 USES
    WinTypes,  // hDC, hPalette
    Messages,  // TWMQueryNewPalette
    Forms,     // TForm
    ExtCtrls,  // TImage
    Windows,   // TPaletteEntry, TMaxLogPalette,
    Graphics,
    StdCtrls;  // TLabel

  CONST
    // 16 of the 20 System Palette Colors are defined in Graphics.PAS.
    // The additional 4 colors that NEVER dither even in 256-color mode
    // are as follows:  (See Microsoft Systems Journal, Sept. 91,
    // page 119, for Windows 3.0 Default Palette.  Interestingly,
    // some of the "standard" colors weren't always the same!
    clMoneyGreen = TColor($C0DCC0);   // Color   "8"  RGB:  192 220 192
    clSkyBlue    = TColor($F0CAA6);   // Color   "9"  RGB:  166 202 240
    clCream      = TColor($F0FBFF);   // Color "246"  RGB:  255 251 240
    clMediumGray = TColor($A4A0A0);   // Color "247"  RGB:  160 160 164

    NonDitherColors:  ARRAY[0..19] OF TColor =
      (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clSilver,
       clMoneyGreen, clSkyblue, clCream, clMediumGray,
       clGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);

  TYPE
    TPaletteEntries = ARRAY[BYTE] OF TPaletteEntry;
    TRGBQuadArray   = ARRAY[BYTE] OF TRGBQUAD;

    TGetBitmapCallback = PROCEDURE (CONST Counter:  INTEGER;
                                    VAR OK:  BOOLEAN; VAR Bitmap:  TBitmap);


  FUNCTION  GetColorDepth:  INTEGER;
  FUNCTION  GetPixelDepth:  INTEGER;

  FUNCTION  GetDesktopPalette:  hPalette;
  FUNCTION  GetGradientPalette(CONST red, green, blue:  BOOLEAN):  hPalette;
  FUNCTION  CreateOptimizedPaletteForSingleBitmap(CONST Bitmap:  TBitmap;
                                                  CONST ColorBits:  INTEGER): hPalette;
  FUNCTION  CreateOptimizedPaletteForManyBitmaps(CONST CallBack:  TGetBitmapCallback;
                                                 CONST ColorBits:  INTEGER): hPalette;

  FUNCTION  IsPaletteDevice:  BOOLEAN;

  PROCEDURE LogicalPaletteToRGBQuad(CONST Start         :  WORD;
                                    CONST Count         :  WORD;
                                    VAR   LogicalPalette:  TPaletteEntries;
                                    VAR   RGBQuad       :  TRGBQuadArray);

  FUNCTION  ReadAndCreatePaletteFromFractIntFile(CONST Filename:  STRING):  hPalette;

  PROCEDURE WMQueryNewPalette(Form:  TForm; PaletteHandle:  hPalette;
                              VAR Msg:  TWMQueryNewPalette);

  PROCEDURE WMPaletteChanged(Form:  TForm; PaletteHandle:  hPalette;
                            VAR Msg:  TWMPaletteChanged);

  VAR
    PaletteColorCount:  INTEGER;


IMPLEMENTATION

  USES
    ColorQuantizationLibrary,  // TColorQuantizer
    SysUtils;                  // FileExists

  CONST
    PaletteVersion = $0300;    // "Magic Number" for Window's LOGPALETTE


  // Adapted from Tommy Andersen's ColorDepth post to
  // comp.lang.pascal.delphi.components.misc, 5 Oct 1997.
  //
  // According to Tim Robert's post "Bitmaps with > 256 Colors" to
  // compl.lang.pascal.delphi.misc, 17 Apr 1997, "in a windows bitmap,
  // one of either the bits-per-plane or the number of planes must be 1.  In
  // fact, the planar form is only supported at 4 bits per pixel.  A 24-bit
  // true color bitmap must have bits-per-pixel set to 24 and planes set to 1.
  FUNCTION GetColorDepth:  INTEGER;
    VAR
      DeviceContext:  hDC;
  BEGIN
    // Get the screen's DC since memory DCs are not reliable
    DeviceContext := GetDC(0);

    TRY
      RESULT := 1 SHL (GetDeviceCaps(DeviceContext, PLANES) *
                       GetDeviceCaps(DeviceContext, BITSPIXEL))
    FINALLY
      // Give back the screen DC
      ReleaseDC(0, DeviceContext)
    END;
  END {GetColorDepth};


  FUNCTION  GetPixelDepth:  INTEGER;
    VAR
      DeviceContext:  hDC;
  BEGIN
    // Get the screen's DC since memory DCs are not reliable
    DeviceContext := GetDC(0);

    TRY
      RESULT := GetDeviceCaps(DeviceContext, BITSPIXEL)
    FINALLY
      // Give back the screen DC
      ReleaseDC(0, DeviceContext)
    END;
  END {GetPixelDepth};


  // Adapted from PaletteFromDesktop function in "Delphi 2 Multimedia," Jarol,
  // et al, 1996, Coriolis Group Books, p. 307.  Unlike book version, use
  // TMaxLogPalette and avoid allocating and freeing a pLogPalette area.
  FUNCTION GetDesktopPalette:  hPalette;
     VAR
      LogicalPalette     :  TMaxLogPalette;
      ScreenDeviceContext:  hDC;
      ReturnCode         :  INTEGER;
  BEGIN
    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreenDeviceContext := GetDC(0);
    TRY
      // Get all 256 entries
      ReturnCode :=  GetSystemPaletteEntries(ScreenDeviceContext,
                                             0, 255, LogicalPalette.palPalEntry)
    FINALLY
      ReleaseDC(0, ScreenDeviceContext)
    END;

    IF   ReturnCode >0
    THEN RESULT := CreatePalette(pLogPalette(@LogicalPalette)^)
    ELSE RESULT := 0
  END {GetDesktopPalette};


  // Parameters identify which planes participate.
  // Use all TRUE for shades of gray.
  FUNCTION  GetGradientPalette(CONST red, green, blue:  BOOLEAN):  hPalette;
    VAR
      ScreenDeviceContext:  hDC;
      i                  :  INTEGER;
      LogicalPalette     :  TMaxLogPalette;

    FUNCTION ScaleValue(CONST flag:  BOOLEAN; CONST i:  INTEGER):  INTEGER;
    BEGIN
      IF   flag
      THEN RESULT := MulDiv(i, 255, 235)
      ELSE RESULT := 0
    END {ScaleValue};

  BEGIN
    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreenDeviceContext := GetDC(0);
    TRY
      GetSystemPaletteEntries(ScreenDeviceContext,
                                0, 10, LogicalPalette.palPalEntry[0]);   // Maintain first 10
      GetSystemPaletteEntries(ScreenDeviceContext,
                              246, 10, LogicalPalette.palPalEntry[246]); // Maintain last  10
    FINALLY
      ReleaseDC(0, ScreenDeviceContext)
    END;

    // Skip over first 10 and last 10 "fixed" entries
    FOR i := 0 TO 255-20 DO
    BEGIN
      // Skip over first 10 "fixed" entries in system palette
      LogicalPalette.palPalEntry[10+i].peRed   := ScaleValue(Red,   i);
      LogicalPalette.palPalEntry[10+i].peGreen := ScaleValue(Green, i);
      LogicalPalette.palPalEntry[10+i].peBlue  := ScaleValue(Blue,  i);
      LogicalPalette.palPalEntry[10+i].peFlags := pC_RESERVED;
    END;

    RESULT := CreatePalette(pLogPalette(@LogicalPalette)^)
  END {GetGradientPalette};


  // Adapted from MSJ "Wicked Code" article, Oct 97, pp. 79-84
  // Bitmap must be a DIB.
  FUNCTION  CreateOptimizedPaletteForSingleBitmap(CONST Bitmap:  TBitmap; CONST ColorBits:  INTEGER): hPalette;
    VAR
      ColorQuantizer     :  TColorQuantizer;
      ScreenDeviceContext:  hDC;
      i                  :  INTEGER;
      LogicalPalette     :  TMaxLogPalette;
      RGBQuadArray       :  TRGBQuadArray;
  BEGIN
    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreenDeviceContext := GetDC(0);
    TRY
      GetSystemPaletteEntries(ScreenDeviceContext,
                                0,256, LogicalPalette.palPalEntry[0]);   // Need first 10 and last 10
    FINALLY
      ReleaseDC(0, ScreenDeviceContext)
    END;

    // Normally for 24-bit images, use ColorBits of 5 or 6.  For 8-bit images
    // use ColorBits = 8.
    ColorQuantizer := TColorQuantizer.Create(236, ColorBits);
    TRY
      ColorQuantizer.ProcessImage(Bitmap.Handle);
      ColorQuantizer.GetColorTable(RGBQuadArray);

      // Skip over first 10 and last 10 "fixed" entries
      FOR i := 0 TO 255-20 DO
      BEGIN
        // Skip over first 10 "fixed" entries in system palette
        LogicalPalette.palPalEntry[10+i].peRed   := RGBQuadArray[i].rgbRed;
        LogicalPalette.palPalEntry[10+i].peGreen := RGBQuadArray[i].rgbGreen;
        LogicalPalette.palPalEntry[10+i].peBlue  := RGBQuadArray[i].rgbBlue;
        LogicalPalette.palPalEntry[10+i].peFlags := RGBQuadArray[i].rgbReserved
      END;
      RESULT := CreatePalette(pLogPalette(@LogicalPalette)^)
    FINALLY
      ColorQuantizer.Free
    END
  END {GetOptimizedPaletteForSingleBitmap};


  // This separate function is for convenience in processing many bitmaps to
  // obtain an optimized palette.  The CallBack is called until a NIL pointer
  // is returned.
  FUNCTION  CreateOptimizedPaletteForManyBitmaps(CONST CallBack:  TGetBitmapCallback;
                                                 CONST ColorBits:  INTEGER): hPalette;
   VAR
      Bitmap             :  TBitmap;
      ColorQuantizer     :  TColorQuantizer;
      Counter            :  INTEGER;
      i                  :  INTEGER;
      LogicalPalette     :  TMaxLogPalette;
      OK                 :  BOOLEAN;
      RGBQuadArray       :  TRGBQuadArray;
      ScreenDeviceContext:  hDC;
  BEGIN
    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreenDeviceContext := GetDC(0);
    TRY
      GetSystemPaletteEntries(ScreenDeviceContext,
                                0,256, LogicalPalette.palPalEntry[0]);   // Need first 10 and last 10
    FINALLY
      ReleaseDC(0, ScreenDeviceContext)
    END;

    // Normally for 24-bit images, use ColorBits of 5 or 6.  For 8-bit images
    // use ColorBits = 8.
    ColorQuantizer := TColorQuantizer.Create(236, ColorBits);
    TRY

      // Keep calling the callback for more bitmaps until a NIL pointer is
      // returned.  Use Counter and OK parameters is for convenience.
      Counter := 1;
      CallBack(Counter, OK, Bitmap);
      WHILE (Bitmap <> NIL) DO
      BEGIN
        IF   OK
        THEN ColorQuantizer.ProcessImage(Bitmap.Handle);

        INC(Counter);
        CallBack(Counter, OK, Bitmap)
      END;

      ColorQuantizer.GetColorTable(RGBQuadArray);

      // Skip over first 10 and last 10 "fixed" entries
      FOR i := 0 TO 255-20 DO
      BEGIN
        // Skip over first 10 "fixed" entries in system palette
        LogicalPalette.palPalEntry[10+i].peRed   := RGBQuadArray[i].rgbRed;
        LogicalPalette.palPalEntry[10+i].peGreen := RGBQuadArray[i].rgbGreen;
        LogicalPalette.palPalEntry[10+i].peBlue  := RGBQuadArray[i].rgbBlue;
        LogicalPalette.palPalEntry[10+i].peFlags := RGBQuadArray[i].rgbReserved
      END;
      RESULT := CreatePalette(pLogPalette(@LogicalPalette)^)
    FINALLY
      ColorQuantizer.Free
    END
  END {GetOptimizedPalette};


  //  Adapted from Joe C. Hecht's BitTBitmapAsDIB post to
  //  borland.public.delphi.winapi, 12 Oct 1997.
  FUNCTION IsPaletteDevice:  BOOLEAN;
    VAR
      DeviceContext:  hDC;
  BEGIN
    // Get the screen's DC since memory DCs are not reliable
    DeviceContext := GetDC(0);

    TRY
      RESULT := GetDeviceCaps(DeviceContext, RASTERCAPS) AND RC_PALETTE = RC_PALETTE
    FINALLY
      // Give back the screen DC
      ReleaseDC(0, DeviceContext)
    END
  END {IsPaletteDevice};


  PROCEDURE LogicalPaletteToRGBQuad(CONST Start       :  WORD;
                                    CONST Count       :  WORD;
                                    VAR LogicalPalette:  TPaletteEntries;
                                    VAr RGBQuad       :  TRGBQuadArray);
    VAR
      i:  INTEGER;
  BEGIN
    FOR i := Start TO Start+Count-1 DO
    BEGIN
      RGBQuad[i].rgbRed      := LogicalPalette[i].peRed;
      RGBQuad[i].rgbGreen    := LogicalPalette[i].peGreen;
      RGBQuad[i].rgbBlue     := LogicalPalette[i].peBlue;
      RGBQuad[i].rgbReserved := LogicalPalette[i].peFlags
    END
  END {LogicalPaletteToRGBQuad};


  //  Adapted from "DibDemo" by John Biddiscombe, J.Biddiscombe@r1.ac.uk
  FUNCTION ReadAndCreatePaletteFromFractIntFile(CONST Filename      :  STRING):  hPalette;
    VAR
      Blue               :  INTEGER;
      Green              :  INTEGER;
      ScreenDeviceContext:  hDC;
      i                  :  INTEGER;
      LogicalPalette     :  TMaxLogPalette;
      FractIntPalFile    :  TextFile;
      Red                :  INTEGER;
  BEGIN
    AssignFile(FractIntPalFile, Filename);
    RESET(FractIntPalFile);

    READLN(FractIntPalFile, PaletteColorCount);
    IF   PaletteColorCount >  236
    THEN Palettecolorcount := 236;

    LogicalPalette.palVersion    := PaletteVersion;
    LogicalPalette.palNumEntries := 256;

    ScreenDeviceContext := GetDC(0);
    TRY
      GetSystemPaletteEntries(ScreenDeviceContext,
                                0, 10, LogicalPalette.palPalEntry[0]);   // Maintain first 10
      GetSystemPaletteEntries(ScreenDeviceContext,
                              246, 10, LogicalPalette.palPalEntry[246]); // Maintain last  10
    FINALLY
      ReleaseDC(0, ScreenDeviceContext)
    END;

    FOR i := 0 TO PaletteColorCount-1 DO
    BEGIN
      READLN(FractIntPalFile, Red, Green, Blue);

      // Skip over first 10 "fixed" entries in system palette
      LogicalPalette.palPalEntry[10+i].peRed   := Red;
      LogicalPalette.palPalEntry[10+i].peGreen := Green;
      LogicalPalette.palPalEntry[10+i].peBlue  := Blue;

      // Must be PC_RESERVED if AnimatePalette is to be used
      LogicalPalette.palPalEntry[10+i].peFlags := PC_RESERVED;
    END;

    IF   PaletteColorCount-1 < 235
    THEN BEGIN
      FOR i := PaletteColorCount TO 235
      DO BEGIN
        LogicalPalette.palPalEntry[10+i].peRed   := LogicalPalette.palPalEntry[10+i-PaletteColorCount].peRed;
        LogicalPalette.palPalEntry[10+i].peGreen := LogicalPalette.palPalEntry[10+i-PaletteColorCount].peGreen;
        LogicalPalette.palPalEntry[10+i].peBlue  := LogicalPalette.palPalEntry[10+i-PaletteColorCount].peBlue;
        LogicalPalette.palPalEntry[10+i].peFlags := pc_Reserved;
      END
    END;

    RESULT := CreatePalette(pLogPalette(@LogicalPalette)^);

    CloseFile(FractIntPalFile);
  END {ReadAndCreatePaletteFromFractIntFile};

 //  This message informs a window that it is about to receive the
  //  keyboard focus, giving the window the opportunity to realize its
  //  logical palette when it receives the focus.
  PROCEDURE WMQueryNewPalette(Form:  TForm; PaletteHandle:  hPalette;
                              VAR Msg:  TWMQueryNewPalette);
    VAR
      DeviceContext:  hDC;
      Palette      :  hPalette;
      ReturnCode   :  INTEGER;
  BEGIN
    DeviceContext := Form.Canvas.Handle;

    // Select the specified palette into a device context.
    // FALSE parameter forces logical palette to be copied into the device palette
    // when the application is in the foreground.
    Palette := SelectPalette(DeviceContext, PaletteHandle, FALSE);

    // Map palette entries from the current logical palette to the system palette.
    // Returned value is the number of entries in the logical palette mapped to
    // the system palette.
    ReturnCode := RealizePalette(DeviceContext);

    // Restore the old palette into the device context.
    SelectPalette(DeviceContext, Palette, FALSE);

    // If new entries were mapped to the system palette, then invalidate the
    // image so it gets repainted.
    IF   ReturnCode > 0
    THEN Form.Invalidate;

    Msg.Result := ReturnCode
  END {WMQueryNewPalette};


  // This message is sent to all top-level and overlapped windows after the
  // window with keyboard focus has realized its logical palette, thereby
  // changing the system palette.  This message enables a window that uses a
  // color palette but does not have the keyboard focus to realize its logical
  // palette and update its client area.
  PROCEDURE WMPaletteChanged(Form:  TForm; PaletteHandle:  hPalette;
                            VAR Msg:  TWMPaletteChanged);
    VAR
      DeviceContext:  hDC;
      Palette      :  hPalette;
  BEGIN
    IF  Msg.PalChg = Form.Handle
    THEN BEGIN
      // To avoid creating an infinite loop, a window that receives this message
      // must not realize its palette (unless it determines the window that
      // caused the palette to change is not its own)
      Msg.Result := 0
    END
    ELSE BEGIN
      DeviceContext := Form.Canvas.Handle;

      // Select the specified palette into a device context.
      // A TRUE parameter causes the logical palette to be mapped to the colors
      // already in the physical palette in the best possible way.
      Palette := SelectPalette(DeviceContext, PaletteHandle, TRUE);

      // Map palette entries from the current logical palette to the system palette.
      // Returned value is the number of entries in the logical palette mapped to
      // the system palette.
      RealizePalette(DeviceContext);

      // UpdateColors is Microsoft's preferred way to refresh an on-screen Window
      // if the system palette changes.  UpdateColors updates the clinet area
      // of the specified device contenxt by remapping the current colors in the
      // client area to the currently realized logical palette.  An inactive window
      // with a realized logical palette may call UpdateColors as an alternative
      // to redrawing its client area when the system palette changes.
      UpdateColors(DeviceContext);

      // Restore the old palette into the device context.
      SelectPalette(DeviceContext, Palette, FALSE);

      Msg.Result := 1;
    END
  END {WMPaletteChanged};

END.
