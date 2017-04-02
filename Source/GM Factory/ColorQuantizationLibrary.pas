//  Gervautz-Purgathofer Octree Color Quantization Algorithm

//  Delphi 3 version of C++ CQuantizer class in Microsoft Systems Journal,
//  "Wicked Code" column, October 1997, pp. 79-84, in the "PalGen" utility.
//  http://www.microsoft.com/msj/codelist.htm

//  Delphi code follows C++ code fairly tightly, except when IMHO an
//  improvement was in order.  For example, I used TOctreeNode Class instead
//  of a Record, which would have been the equivalent of the C _NODE Struct.

//  Earl F. Glynn, March 1998.

UNIT ColorQuantizationLibrary;

INTERFACE

  USES
    Windows,         // THandle, TRGBTriple, TRGBQuad, GetObject
    PaletteLibrary;  // TRGBQuadArray

  TYPE
    TOctreeNode = CLASS;  // Forward definition so TReducibleNodes can be declared

    TReducibleNodes = ARRAY[0..7] OF TOctreeNode;

    TOctreeNode     =
      CLASS(TObject)
        IsLeaf      :  BOOLEAN;
        PixelCount  :  INTEGER;
        RedSum      :  INTEGER;
        GreenSum    :  INTEGER;
        BlueSum     :  INTEGER;
        Next        :  TOctreeNode;
        Child       :  TReducibleNodes;

        CONSTRUCTOR Create (CONST Level         :  INTEGER;
                            CONST ColorBits     :  INTEGER;
                            VAR   LeafCount     :  INTEGER;
                            VAR   ReducibleNodes:  TReducibleNodes);
        DESTRUCTOR  Destroy;  OVERRIDE;

      END;

    TColorQuantizer =
      CLASS(TOBject)
        PRIVATE
          FTree          :  TOctreeNode;
          FLeafCount     :  INTEGER;
          FReducibleNodes:  TReducibleNodes;
          FMaxColors     :  INTEGER;
          FColorBits     :  INTEGER;

        PROTECTED
          PROCEDURE   AddColor(VAR   Node          :  TOctreeNode;
                               CONST r,g,b         :  BYTE;
                               CONST ColorBits     :  INTEGER;
                               CONST Level         :  INTEGER;
                               VAR   LeafCount     :  INTEGER;
                               VAR   ReducibleNodes:  TReducibleNodes);
          PROCEDURE   DeleteTree(VAR Node:  TOctreeNode);
          PROCEDURE   GetPaletteColors(CONST Node        :  TOctreeNode;
                                       VAR   RGBQuadArray:  TRGBQuadArray;
                                       VAR   Index       :  INTEGER);
          PROCEDURE   ReduceTree(CONST ColorBits:  INTEGER;
                                 VAR   LeafCount:  INTEGER;
                                 VAR   ReducibleNodes:  TReducibleNodes);

        PUBLIC
          CONSTRUCTOR Create(CONST MaxColors:  INTEGER; CONST ColorBits:  INTEGER);
          DESTRUCTOR  Destroy;  OVERRIDE;

          FUNCTION    GetColorTable(VAR RGBQuadArray:  TRGBQuadArray) : INTEGER;
          FUNCTION    ProcessImage(CONST Handle:  THandle):  BOOLEAN;

          PROPERTY    ColorCount:  INTEGER  READ FLeafCount;

      END;


IMPLEMENTATION

//// TOctreeNode  ///////////////////////////////////////////////////////////

  CONSTRUCTOR TOctreeNode.Create (CONST Level         :  INTEGER;
                                  CONST ColorBits     :  INTEGER;
                                  VAR   LeafCount     :  INTEGER;
                                  VAR   ReducibleNodes:  TReducibleNodes);
    VAR
      i:  INTEGER;
  BEGIN
    PixelCount  := 0;
    RedSum      := 0;
    GreenSum    := 0;
    BlueSum     := 0;
    FOR i := Low(Child) TO High(Child) DO
      Child[i] := NIL;

    IsLeaf := (Level = ColorBits);
    IF   IsLeaf
    THEN BEGIN
      Next := NIL;
      INC(LeafCount);
    END
    ELSE BEGIN
      Next := ReducibleNodes[Level];
      ReducibleNodes[Level] := SELF
    END
  END {Create};


  DESTRUCTOR TOctreeNode.Destroy;
    VAR
      i:  INTEGER;
  BEGIN
      FOR i := Low(Child) TO High(Child) DO
        Child[i].Free
  END {Destroy};


//// TColorQuantizer  ///////////////////////////////////////////////////////

  CONSTRUCTOR TColorQuantizer.Create(CONST MaxColors:  INTEGER; CONST ColorBits:  INTEGER);
    VAR
      i:  INTEGER;
  BEGIN
    ASSERT (ColorBits <= 8);

    FTree := NIL;
    FLeafCount := 0;

    // Initialize all nodes even though only ColorBits+1 of them are needed
    FOR i := Low(FReducibleNodes) TO High(FReducibleNodes) DO
      FReducibleNodes[i] := NIL;

    FMaxColors := MaxColors;
    FColorBits := ColorBits
  END {Create};


  DESTRUCTOR  TColorQuantizer.Destroy;
  BEGIN
    IF   FTree <> NIL
    THEN DeleteTree(FTree)
  END {Destroy};


  FUNCTION TColorQuantizer.GetColorTable(VAR RGBQuadArray:  TRGBQuadArray) : INTEGER;
  BEGIN
    Result := 0;
    GetPaletteColors(FTree, RGBQuadArray, Result);
  END {GetColorTable};


  // Handles passed to ProcessImage should refer to DIB sections, not DDBs.
  // In certain cases, specifically when it's called upon to process 1, 4, or
  // 8-bit per pixel images on systems with palettized display adapters,
  // ProcessImage can produce incorrect results if it's passed a handle to a
  // DDB.
  FUNCTION TColorQuantizer.ProcessImage(CONST Handle:  THandle):  BOOLEAN;
    CONST
      MaxPixelCount = 1048576;  // 2^20 shouldn't be much of a limit here

    TYPE
      pRGBArray = ^TRGBArray;
      TRGBArray = ARRAY[0..MaxPixelCount-1] OF TRGBTriple;

    VAR
      Bytes     :  INTEGER;
      DIBSection:  TDIBSection;

    // Process 1, 4, or 8-bit DIB:
    // The strategy here is to use GetDIBits to convert the image into
    // a 24-bit DIB one scan line at a time.  A pleasant side effect
    // of using GetDIBits in this manner is that RLE-encoded 4-bit and
    // 8-bit DIBs will be uncompressed.

    // Implemented as in article, but doesn't work (yet) as I would expect.
    PROCEDURE ProcessLowBitDIB;
      VAR
        BitmapInfo   :  TBitmapInfo;
        DeviceContext:  hDC;
        i            :  INTEGER;
        j            :  INTEGER;
        ScanLine     :  pRGBArray;
    BEGIN
      GetMem(ScanLine, 3*DIBSection.dsBmih.biWidth);
      TRY
        ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
        WITH BitmapInfo DO
        BEGIN
          bmiHeader.biSize        := SizeOf(TBitmapInfo);
          bmiHeader.biWidth       := DIBSection.dsBmih.biWidth;
          bmiHeader.biHeight      := DIBSection.dsBmih.biHeight;
          bmiHeader.biPlanes      := 1;
          bmiHeader.biBitCount    := 24;
          bmiHeader.biCompression := BI_RGB;

        END;

        DeviceContext := GetDC(0);
        TRY
          FOR j := 0 TO DIBSection.dsBmih.biHeight-1 DO
          BEGIN
            GetDIBits (DeviceContext, Handle, j, 1, ScanLine, BitmapInfo, DIB_RGB_COLORS);
            FOR i := 0 TO DIBSection.dsBmih.biWidth-1 DO
            BEGIN
              WITH Scanline[i] DO
                AddColor(FTree, rgbtRed, rgbtGreen, rgbtBlue,
                         FColorBits, 0, FLeafCount, FReducibleNodes);

              WHILE FLeafCount > FMaxColors DO
                ReduceTree(FColorbits, FLeafCount, FReducibleNodes)

            END
          END

        FINALLY
          ReleaseDC(0, DeviceContext);
        END

      FINALLY
        FreeMem(ScanLine)
      END
    END {ProcessLowBitDIB};


    PROCEDURE Process16BitDIB;
    BEGIN
      // Not yet implemented
    END {Process16BitDIB};


    PROCEDURE Process24BitDIB;
      VAR
        i       :  INTEGER;
        j       :  INTEGER;
        ScanLine:  pRGBArray;
    BEGIN
      Scanline := pRGBArray(DIBSection.dsBm.bmBits);
      FOR j := 0 TO DIBSection.dsBmih.biHeight-1 DO
      BEGIN

        FOR i := 0 TO DIBSection.dsBmih.biWidth-1 DO
        BEGIN
          WITH Scanline[i] DO
            AddColor(FTree, rgbtRed, rgbtGreen, rgbtBlue,
                     FColorBits, 0, FLeafCount, FReducibleNodes);

          WHILE FLeafCount > FMaxColors DO
            ReduceTree(FColorbits, FLeafCount, FReducibleNodes)

        END;

        ScanLine := pRGBArray(INTEGER(Scanline) + DIBSection.dsBm.bmWidthBytes);
      END
    END {Process24BitDIB};


    PROCEDURE Process32BitDIB;
    BEGIN
      // Not yet implemented
    END {Process32BitDIB};

  BEGIN {ProcessImage}
    RESULT := FALSE;

    Bytes := GetObject(Handle, SizeOF(DIBSection), @DIBSection);

    IF   Bytes > 0   // Invalid Bitmap if Bytes = 0
    THEN BEGIN
//    PadBytes := DIBSECTION.dsBm.bmWidthBytes -
//                (((DIBSection.dsBmih.biWidth * DIBSection.dsBmih.biBitCount) + 7) DIV 8);
      ASSERT (DIBSection.dsBmih.biHeight < MaxPixelCount);
      ASSERT (DIBSection.dsBmih.biWidth  < MaxPixelCount);

      CASE  DIBSection.dsBmih.biBitCount OF
         1:  ProcessLowBitDIB;
         4:  ProcessLowBitDIB;
         8:  ProcessLowBitDIB;
        16:  Process16bitDIB;
        24:  Process24bitDIB;
        32:  Process32bitDIB
        ELSE
          // Do nothing.  Default RESULT is already FALSE
      END

    END
  END {ProcessImage};


  //// PROTECTED Methods //////////////////////////////////////////////////////

  PROCEDURE TColorQuantizer.AddColor(VAR   Node          :  TOctreeNode;
                                     CONST r,g,b         :  BYTE;
                                     CONST ColorBits     :  INTEGER;
                                     CONST Level         :  INTEGER;
                                     VAR   LeafCount     :  INTEGER;
                                     VAR   ReducibleNodes:  TReducibleNodes);
    CONST
      Mask:  ARRAY[0..7] OF BYTE = ($80, $40, $20, $10, $08, $04, $02, $01);

    VAR
      Index    :  INTEGER;
      Shift    :  INTEGER;
  BEGIN
    // If the node doesn't exist, create it.
    IF   Node = NIL
    THEN Node := TOctreeNode.Create(Level, ColorBits, LeafCount, ReducibleNodes);

    IF   Node.IsLeaf
    THEN BEGIN
      INC(Node.PixelCount);
      INC(Node.RedSum,   r);
      INC(Node.GreenSum, g);
      INC(Node.BlueSum,  b)
    END
    ELSE BEGIN
      // Recurse a level deeper if the node is not a leaf.
      Shift := 7 - Level;

      Index := (((r AND mask[Level]) SHR Shift) SHL 2)  OR
               (((g AND mask[Level]) SHR Shift) SHL 1)  OR
                ((b AND mask[Level]) SHR Shift);
      AddColor(Node.Child[Index], r, g, b, ColorBits, Level+1,
               LeafCount, ReducibleNodes)
    END

  END {AddColor};



  PROCEDURE TColorQuantizer.DeleteTree(VAR Node:  TOctreeNode);
    VAR
      i        :  INTEGER;
  BEGIN
    FOR i := Low(TReducibleNodes) TO High(TReducibleNodes) DO
    BEGIN
      IF   Node.Child[i] <> NIL
      THEN DeleteTree(Node.Child[i]);
    END;

    Node.Free;

    Node := NIL;
  END {DeleteTree};


  PROCEDURE TColorQuantizer.GetPaletteColors(CONST Node        :  TOctreeNode;
                                             VAR   RGBQuadArray:  TRGBQuadArray;
                                             VAR   Index       :  INTEGER);
    VAR
      i:  INTEGER;
  BEGIN
    IF   Node.IsLeaf
    THEN BEGIN
      WITH RGBQuadArray[Index] DO
      BEGIN
        TRY
          rgbRed   := BYTE(Node.RedSum   DIV Node.PixelCount);
          rgbGreen := BYTE(Node.GreenSum DIV Node.PixelCount);
          rgbBlue  := BYTE(Node.BlueSum  DIV Node.PixelCount)
        EXCEPT
          rgbRed   := 0;
          rgbGreen := 0;
          rgbBlue  := 0
        END;

        rgbReserved := 0
      END;
      INC(Index)
    END
    ELSE BEGIN
      FOR i := Low(Node.Child) TO High(Node.Child) DO
      BEGIN
        IF   Node.Child[i] <> NIL
        THEN GetPaletteColors(Node.Child[i], RGBQuadArray, Index)
      END
    END
  END {GetPaletteColors};


  PROCEDURE TColorQuantizer.ReduceTree(CONST ColorBits:  INTEGER;
                                       VAR   LeafCount:  INTEGER;
                                       VAR   ReducibleNodes:  TReducibleNodes);
    VAR
      BlueSum :  INTEGER;
      Children:  INTEGER;
      GreenSum:  INTEGER;
      i       :  INTEGER;
      Node    :  TOctreeNode;
      RedSum  :  INTEGER;
  BEGIN
    // Find the deepest level containing at least one reducible node
    i := Colorbits - 1;
    WHILE (i > 0) AND (ReducibleNodes[i] = NIL) DO
      DEC(i);

    // Reduce the node most recently added to the list at level i.
    Node := ReducibleNodes[i];
    ReducibleNodes[i] := Node.Next;

    RedSum   := 0;
    GreenSum := 0;
    BlueSum  := 0;
    Children := 0;

    FOR i := Low(ReducibleNodes) TO High(ReducibleNodes) DO
    BEGIN
      IF   Node.Child[i] <> NIL
      THEN BEGIN
        INC(RedSum,          Node.Child[i].RedSum);
        INC(GreenSum,        Node.Child[i].GreenSum);
        INC(BlueSum,         Node.Child[i].BlueSum);
        INC(Node.PixelCount, Node.Child[i].PixelCount);
        Node.Child[i].Free;
        Node.Child[i] := NIL;
        INC(Children)
      END
    END;

    Node.IsLeaf   := TRUE;
    Node.RedSum   := RedSum;
    Node.GreenSum := GreenSum;
    Node.BlueSum  := BlueSum;
    DEC(LeafCount, Children-1)
  END {ReduceTree};

  /////////////////////////////////////////////////////////////////////////////

END.
