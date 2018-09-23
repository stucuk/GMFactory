unit FormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, GMF, GM_File, ComCtrls, ExtCtrls, pngimage, CheckLst,
  Buttons, PngSpeedButton, ToolWin, Menus, ImgList, PngImageList,
  VertToolbar, HSLUnit, UndoSystem, ExtDlgs;

const
 APP_TITLE = 'GM Factory';
 APP_VER   = '1.0.4.0';
 WND_TITLE = APP_TITLE + ' v' + APP_VER;

type
  TSTrackBar = Class(TTrackBar)
  protected
   procedure WndProc(var Message: TMessage); override;
  end;

  TFrmMain = class(TForm)
    OpenDialog: TOpenDialog;
    SidePanel: TPanel;
    StatusBar: TStatusBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    PngImageList: TPngImageList;
    DrawLockTimer: TTimer;
    LayerPanel: TPanel;
    LayersBox: TScrollBox;
    AddLayerButton: TPngSpeedButton;
    DeleteLayerButton: TPngSpeedButton;
    FramePanel: TPanel;
    NextFrameButton: TPngSpeedButton;
    PrevFrameButton: TPngSpeedButton;
    ColorPanel: TPanel;
    PaletteImg: TImage;
    Col1Lum: TImage;
    Col2Lum: TImage;
    PreviewPanel: TPanel;
    PreviewTimer: TTimer;
    PreviewInnerPanel: TPanel;
    PreviewImage: TImage;
    PreviewBack: TImage;
    FrameLabel: TLabel;
    BackLeftPanel: TPanel;
    MainToolBar: TToolBar;
    ToolButton7: TToolButton;
    NewButton: TPngSpeedButton;
    OpenButton: TPngSpeedButton;
    ToolButton5: TToolButton;
    SaveButton: TPngSpeedButton;
    SaveAsButton: TPngSpeedButton;
    ToolButton6: TToolButton;
    FakeLevelBackgroundButton: TPngSpeedButton;
    ToolButton1: TToolButton;
    OriginalSizeButton: TPngSpeedButton;
    ZoomOutButton: TPngSpeedButton;
    ZoomInButton: TPngSpeedButton;
    ToolButton2: TToolButton;
    SideColsButton: TPngSpeedButton;
    ZBufferButton: TPngSpeedButton;
    VerticalToolBar1: TVerticalToolBar;
    PencilButton: TPngSpeedButton;
    DropperButton: TPngSpeedButton;
    EraserButton: TPngSpeedButton;
    MainPanel: TPanel;
    Bevel1: TBevel;
    ScrollBox: TScrollBox;
    EditingPanel: TPanel;
    BackgroundImg: TImage;
    Bevel2: TBevel;
    Bevel7: TBevel;
    Bevel3: TBevel;
    Panel2: TPanel;
    ColPanel1: TPanel;
    ColPanel2: TPanel;
    SaveDialog: TSaveDialog;
    BackgroundCollection: TPngImageCollection;
    BGImagePopup: TPopupMenu;
    PrevFrameButton2: TPngSpeedButton;
    ToolButton3: TToolButton;
    NextFrameButton2: TPngSpeedButton;
    Bevel4: TBevel;
    Bevel5: TBevel;
    ColLum1Over: TImage;
    ColLum2Over: TImage;
    Bevel6: TBevel;
    EditTypePanel: TPanel;
    RGBEditType: TSpeedButton;
    EditTypeSideCols: TSpeedButton;
    EditTypeZBuffer: TSpeedButton;
    ColorPanelZBuff: TPanel;
    ZBuffCol1: TTrackBar;
    ZBuffCol1Label: TLabel;
    ZBuffCol2Label: TLabel;
    ZBuffCol2: TTrackBar;
    Reopen1: TMenuItem;
    MenuPngImageList: TPngImageList;
    Help1: TMenuItem;
    About1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    UndoUpdateTimer: TTimer;
    UndoButton: TPngSpeedButton;
    RedoButton: TPngSpeedButton;
    ToolButton4: TToolButton;
    SelectionButton: TPngSpeedButton;
    SelectionTimer: TTimer;
    Selection1: TMenuItem;
    SelectAll1: TMenuItem;
    SelectNone1: TMenuItem;
    Copy1: TMenuItem;
    N3: TMenuItem;
    Button1: TButton;
    Cut1: TMenuItem;
    Delete1: TMenuItem;
    Paste1: TMenuItem;
    ExtraDataPanel: TPanel;
    ExtraDataList: TListBox;
    EDPButton: TPngSpeedButton;
    ToolButton8: TToolButton;
    EDX: TEdit;
    EDY: TEdit;
    EDZ: TEdit;
    EDAddButton: TPngSpeedButton;
    EDDeleteButton: TPngSpeedButton;
    EDEditButton: TPngSpeedButton;
    ools1: TMenuItem;
    ChangeOffset1: TMenuItem;
    OpenDialogGMs: TOpenDialog;
    CopyExtraData1: TMenuItem;
    Import1: TMenuItem;
    N4: TMenuItem;
    Export1: TMenuItem;
    AsPNG1: TMenuItem;
    FromPNGs1: TMenuItem;
    OpenPNGDialog: TOpenPictureDialog;
    SavePNGDialog: TSavePictureDialog;
    MassZBufferChange1: TMenuItem;
    SHPImport1: TMenuItem;
    ChangeExtraData1: TMenuItem;
    Resize1: TMenuItem;
    N6: TMenuItem;
    FromPNGPreserveTransparency1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure EditingPanelResize(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure OriginalSizeButtonClick(Sender: TObject);
    procedure ZoomOutButtonClick(Sender: TObject);
    procedure ZoomInButtonClick(Sender: TObject);
    procedure LayerCheckClick(Sender: TObject);
    procedure LayerClick(Sender: TObject);
    procedure PanelWndProc(var Message: TMessage);
    procedure ImgWndProc(var Message: TMessage);
    procedure PaletteImgMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaletteImgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LayerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LayerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure DrawLockTimerTimer(Sender: TObject);
    function SetFocusedControl(Control: TWinControl): Boolean; override;
    procedure PrevFrameButtonClick(Sender: TObject);
    procedure NextFrameButtonClick(Sender: TObject);
    procedure AddLayerButtonClick(Sender: TObject);
    procedure PreviewTimerTimer(Sender: TObject);
    procedure ZBufferButtonClick(Sender: TObject);
    procedure PencilButtonClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure SaveAsButtonClick(Sender: TObject);
    procedure FakeLevelBackgroundButtonClick(Sender: TObject);
    procedure BGMenuItemClick(Sender: TObject);
    procedure ReOpenClick(Sender : TObject);
    procedure DeleteLayerButtonClick(Sender: TObject);
    procedure Col1LumMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Col1LumMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ColLum1OverMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColLum1OverMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure New1Click(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    function CheckFileSave : Boolean;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RGBEditTypeClick(Sender: TObject);
    procedure ZBuffCol1Change(Sender: TObject);
    procedure ZBuffCol2Change(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure UndoUpdateTimerTimer(Sender: TObject);
    procedure SelectionTimerTimer(Sender: TObject);
    procedure SelectNone1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure EDPButtonClick(Sender: TObject);
    procedure ExtraDataListClick(Sender: TObject);
    procedure ExtraDataListKeyPress(Sender: TObject; var Key: Char);
    procedure EDXKeyPress(Sender: TObject; var Key: Char);
    procedure EDAddButtonClick(Sender: TObject);
    procedure EDEditButtonClick(Sender: TObject);
    procedure EDDeleteButtonClick(Sender: TObject);
    procedure ChangeOffset1Click(Sender: TObject);
    procedure CopyExtraData1Click(Sender: TObject);
    procedure FromPNGs1Click(Sender: TObject);
    procedure AsPNG1Click(Sender: TObject);
    procedure MassZBufferChange1Click(Sender: TObject);
    procedure SHPImport1Click(Sender: TObject);
    procedure ChangeExtraData1Click(Sender: TObject);
    procedure Resize1Click(Sender: TObject);
    procedure FromPNGPreserveTransparency1Click(Sender: TObject);
  private
    { Private declarations }
    procedure FinishedDrawing(FullRedraw : Boolean);
    procedure DrawOnLayer_Pencil(X,Y : Integer; Colour : TColor);
    procedure DrawOnLayer_Dropper(X,Y : Integer; Left : Boolean);
    procedure DrawOnLayer_Eraser(X,Y : Integer);
    procedure BuildBGList;
    procedure SetColByLum(Sender : TObject; Y : Integer);
    procedure SetColLumOver(CT : Boolean);
    procedure Save(Filename : AnsiString);
    procedure OpenFile(Filename : AnsiString);
  public
    { Public declarations }
    PanelOLDWNDPROC : TWndMethod;
    ImgOLDWNDPROC   : TWndMethod;
    TrackBar : TSTrackBar;
    procedure SetZoom(Value : Byte);
    procedure StartLoad;
    procedure EnableToolBar;
    procedure AfterLoad;
    procedure DrawOnLayer(X,Y : Integer; Left : Boolean);
    procedure SetCaption;
    procedure SetColour(Value : TColor; Col1 : Boolean; NoLumRebuild : Boolean = False);
    procedure BuildExtraDataList();
    procedure ImportPNG(KeepTransparency : Boolean);
  end;

const
 Filter_GMALL : AnsiString = 'All Files (*.gmz;*.gmx;*.gms;*.gma;*.gme)|*.gmz;*.gmx;*.gms;*.gma;*.gme';
 Filter_GMZ   : AnsiString = '*.gmz|*.gmz';
 Filter_GMZXA : AnsiString = '*.gmz|*.gmz|*.gmx|*.gmx|*.gma|*.gma|*.gme|*.gme';
 Filter_SHP   : AnsiString = '*.shp|*.shp';

var
  FrmMain     : TFrmMain;  
  Colours     : Array [BYTE,BOOLEAN] of TColor;
  RecentFiles : TStringList = Nil;
  LastFrame   : Integer = -1;
  Selection   : TRect;
  SelectionDown : Array [0..1] of Integer;

type
 TDRAWTOOLS = (DRAW_PENCIL,DRAW_DROPPER,DRAW_ERASER,DRAW_SELECT);

procedure TheUndoRedoProc(Data : PUndoRedoData; UR : Byte);
procedure UpdateSelection(X1,Y1,X2,Y2 : Integer);

implementation

{$R *.dfm}

uses OW_Palette, CheckerUnit, CommCtrl, RGBHelper, Math, GMLoader, FormOpen,
  FormNew, CommDlg, FormSaveOptions, FormAbout, Types, clipbrd, EdUnit,
  FormChangeOffset, FormZBufferChange, FormSHPImport, FormChangeExtraData,
  FormResize;

type
TUndo = Record
 Data  : Pointer;
 Layer : Integer;
 R     : TRect;
end;

var
 Undo : TUndo;

function GetUndoLayerSize : Integer;
begin
 case Ed.CurEditType of
   1,
   2 : Result := 1;
  else
   Result := 3;
 end;
end;

procedure GetUndoData(var U : PUndoRedoData);
var
 Y  : Integer;
 SD,DD : PByte;
begin
 GetMem(U.Data,U.W*U.H);
 DD := U.Data;
 for Y := Undo.R.Top to Undo.R.Bottom do
 begin
  SD := Pointer(Cardinal(Undo.Data) + (Y*Ed.Width+Undo.R.Left));
  CopyMemory(DD,SD,U.W);
  Inc(Cardinal(DD),U.W);
 end;
end;

procedure GetUndoDataWithTrans(var U : PUndoRedoData);
var
 Y  : Integer;
 SD,DD,
 SD2,DD2,
 TOff : PByte;
begin
 GetMem(U.Data,U.W*U.H*3);
 DD   := U.Data;
 DD2  := Pointer(Cardinal(U.Data)+U.W*U.H*2);
 TOff := Pointer(Cardinal(Undo.Data)+Ed.Width*Ed.Height*2);
 for Y := Undo.R.Top to Undo.R.Bottom do
 begin
  SD  := Pointer(Cardinal(Undo.Data) + (Y*Ed.Width+Undo.R.Left)*2);
  SD2 := Pointer(Cardinal(TOff) + (Y*Ed.Width+Undo.R.Left));
  CopyMemory(DD,SD,U.W*2);
  Inc(Cardinal(DD),U.W*2);
  CopyMemory(DD2,SD2,U.W);
  Inc(Cardinal(DD2),U.W);
 end;
end;

procedure DoUpdateUndo(UR : Byte; IsNew : Boolean);
var
 U : PUndoRedoData;
begin
 if Undo.R.Left > -1 then
 begin
  GetMem(U,SizeOf(TUndoRedoData));
  U.X     := Undo.R.Left;
  U.Y     := Undo.R.Top;
  U.W     := Undo.R.Right-Undo.R.Left+1;
  U.H     := Undo.R.Bottom-Undo.R.Top+1;
  U.Layer := Undo.Layer;
  if U.Layer < 256 then
   GetUndoDataWithTrans(U)
  else
   GetUndoData(U);
  AddUndo(U,UR,IsNew);
  Undo.R.Left := -1;
 end;
end;

procedure UpdateUndoData;
begin
 FrmMain.UndoUpdateTimer.Enabled := False;

 DoUpdateUndo(0,True);

 if Assigned(Undo.Data) then
 FreeMem(Undo.Data);

 GetMem(Undo.Data,Ed.Width*Ed.Height*GetUndoLayerSize);
 case Ed.CurEditType of
   1 : begin
        Ed.SetupSideCol;
        CopyMemory(Undo.Data,Ed.CurFrame.SideCol,Ed.Width*Ed.Height);
        Undo.Layer := 256;
       end;
   2 : begin
        Ed.SetupZBuff;
        CopyMemory(Undo.Data,Ed.CurFrame.ZBuff,Ed.Width*Ed.Height);
        Undo.Layer := 257;
       end;
  else
  begin
   CopyMemory(Undo.Data,Ed.CurLayer.RGB16,Ed.Width*Ed.Height*2);
   CopyMemory(Pointer(Cardinal(Undo.Data)+Ed.Width*Ed.Height*2),Ed.CurLayer.Trans,Ed.Width*Ed.Height);
   Undo.Layer := Ed.CurFrame.Selected;
  end;
 end;

 FrmMain.Undo1.Enabled := GetUndoRedoCount(0,Undo.Layer) > 0;
 FrmMain.Redo1.Enabled := GetUndoRedoCount(1,Undo.Layer) > 0;

 FrmMain.UndoButton.Enabled := FrmMain.Undo1.Enabled;
 FrmMain.RedoButton.Enabled := FrmMain.Redo1.Enabled;
end;

procedure UndoAddPixel(X,Y : Integer);
begin
 if Undo.R.Left = -1 then
 begin
  Undo.R := Rect(X,Y,X,Y);
  Exit;
 end;

 if X < Undo.R.Left then
 Undo.R.Left := X;

 if X > Undo.R.Right then
 Undo.R.Right := X;

 if Y < Undo.R.Top then
 Undo.R.Top := Y;

 if Y > Undo.R.Bottom then
 Undo.R.Bottom := Y;

 FrmMain.UndoUpdateTimer.Enabled := False;
 FrmMain.UndoUpdateTimer.Enabled := True;
end;

procedure UndoAddRect(R : TRect);
begin
 if Undo.R.Left = -1 then
 begin
  Undo.R := R;
  FrmMain.UndoUpdateTimer.Enabled := False;
  FrmMain.UndoUpdateTimer.Enabled := True;
  Exit;
 end;

 UndoAddPixel(R.Left,R.Top);
 UndoAddPixel(R.Right,R.Bottom);
end;

procedure BuildRecentFiles();
var
 Menu,
 Item : TMenuItem;
 X : Integer;
begin
 Menu := FrmMain.Reopen1;

 Menu.Clear;

 X := 0;
 if RecentFiles.Count > 0 then
 Repeat
  if not FileExists(RecentFiles[X]) then
   RecentFiles.Delete(X)
  else
   Inc(X);
 Until X = RecentFiles.Count;

 for X := 0 to RecentFiles.Count-1 do
 begin
  Item := TMenuItem.Create(Menu);
  Item.Caption := ExtractFileName(RecentFiles[X]);
  Item.Tag := X;
  Item.OnClick := FrmMain.ReOpenClick;

  Menu.Add(Item);
 end;

 Menu.Enabled := RecentFiles.Count > 0;
end;

procedure AddToRecent();
var
 I : Integer;
begin
 I := RecentFiles.IndexOf(Ed.Filename);
 if I > -1 then
 RecentFiles.Delete(I);

 RecentFiles.Insert(0,Ed.Filename);

 if RecentFiles.Count > 10 then
 RecentFiles.Delete(10);

 BuildRecentFiles();
end;

function MakeGray(Value : Byte) : TColor;
begin
 Result := RGB(Value,Value,Value);
end;

procedure MakePaletteImg(Img : TImage; Gray : Boolean);
var
 X,Y : Integer;
 SL  : PRGBTriple;
 B : TBitmap;
 G : Integer;
begin
 B := Img.Picture.Bitmap;
 B.Width  := Img.Width;
 B.Height := Img.Height;
 B.PixelFormat := pf24bit;

 for Y := 0 to B.Height-1 do
 begin
  SL := B.ScanLine[Y];
  for X := 0 to B.Width-1 do
  begin
   SL^ := hicolor_to_color(color_to_hicolor(HSLToRGB2(X/B.Width,(B.Height-1-Y)/B.Height,0.5)));
   if Gray then
   begin
    G := (SL^.rgbtRed+SL^.rgbtGreen+SL^.rgbtBlue) div 3;
    SL^.rgbtRed   := G;
    SL^.rgbtGreen := G;
    SL^.rgbtBlue  := G;
   end;
   Inc(Cardinal(SL),3);
  end;
 end;

 Img.Invalidate;
end;

procedure MakePaletteLumImg(Img : TImage; Colour : TColor);
var
 X,Y : Integer;
 SL  : PRGBTriple;
 SLC : TRGBTriple;
 B : TBitmap;
// G : Integer;
 HSL : THSLArray;
begin
 B := Img.Picture.Bitmap;
 B.Width       := Img.Width;
 B.Height      := Img.Height;
 B.PixelFormat := pf24bit;

 HSL := RGBToHSL(SetRGBTriple(Colour));

 for Y := 0 to B.Height-1 do
 begin
  SL  := B.ScanLine[(B.Height-1)-Y];
  SLC := hicolor_to_color(color_to_hicolor(HSLToRGB2(HSL[HSL_H],HSL[HSL_S],Y/(B.Height-1))));

  for X := 0 to B.Width-1 do
  begin
   CopyMemory(SL,@SLC,3);
   Inc(Cardinal(SL),3);
  end;
 end;

 Img.Invalidate;
end;

procedure TFrmMain.SetColour(Value : TColor; Col1 : Boolean; NoLumRebuild : Boolean = False);
var
 Img   : TImage;
 Panel : TPanel;
begin
 if Col1 then
 begin
  Img   := Col1Lum;
  Panel := ColPanel1;
 end
 else
 begin
  Img   := Col2Lum;
  Panel := ColPanel2;
 end;

 if Panel.Color = Value then Exit; //Its the same....

 Colours[Ed.CurEditType][Col1] := Value;
 Panel.Color := Value;
 if not NoLumRebuild then
 MakePaletteLumImg(Img,Value);

 SetColLumOver(Col1);

 if Ed.CurEditType = 2 then
 begin
  if Col1 then
   ZBuffCol1.Position := 255-GetRValue(Value)
  else
   ZBuffCol2.Position := 255-GetRValue(Value);
 end;
end;

procedure TileImage2(Img : TImage; Tile : TBitmap; IW,IH : Integer);
var
 BMP : TBitmap;
 W,H,X,Y : Integer;
begin
 BMP := TBitmap.Create;

 W := (IW div Tile.Width)+2;
 H := (IH div Tile.Height)+2;

 BMP.Width       := IW;
 BMP.Height      := IH;
 BMP.PixelFormat := pf24bit;

 for Y := 0 to H-1 do
 for X := 0 to W-1 do
  BMP.Canvas.Draw(X*Tile.Height,Y*Tile.Width,Tile);

 BMP.Width       := IW;
 BMP.Height      := IH;
 Img.Picture.Assign(BMP);
end;

function GetEdited : AnsiString;
begin
 if Ed.Edited then
  Result := '*'
 else
  Result := '';
end;

procedure TFrmMain.SetCaption;
var
 NewCap : AnsiString;
begin
 NewCap := ' ' + WND_TITLE;

 if Assigned(Ed.GMFactory) then
  NewCap := NewCap + ' ['+ ExtractFileName(Ed.Filename) + GetEdited +'] ' + IntToStr(Ed.Zoom)+'x';

 if NewCap <> Caption then
  Caption := NewCap;

 if Assigned(Ed.GMFactory) then
  NewCap := ExtractFileName(Ed.Filename)
 else
  NewCap := ' ' + WND_TITLE;

 if NewCap <> Application.Title then
  Application.Title := NewCap;
end;

procedure TFrmMain.BuildBGList;
var
 Item : TMenuItem;
 sr : TSearchRec;
 ID : Integer;
 PNGItem : TPngImageCollectionItem;
begin
 ID := 0;
 if FindFirst(ExtractFileDir(ParamStr(0))+'\Backgrounds\*.png', faAnyFile, sr) = 0 then
 begin
  repeat
   Item := TMenuItem.Create(BGImagePopup);
   Item.Tag     := ID;
   Item.Caption := ChangeFileExt(sr.Name,'');
   Item.OnClick := BGMenuItemClick;
   BGImagePopup.Items.Add(Item);
   PNGItem := BackgroundCollection.Items.Add();
   PNGItem.PngImage.LoadFromFile(ExtractFileDir(ParamStr(0))+'\Backgrounds\'+sr.Name);
   Inc(ID);
  until FindNext(sr) <> 0;
  FindClose(sr);
 end;

 if ID > 0 then
 Ed.BGImage.Canvas.Draw(0,0,BackgroundCollection.Items[0].PngImage);
end;

procedure SetColourHRSLUM(Left : Boolean; Colour : TColor);
begin
 Ed.HSR[Left]  := RGBToHSL(SetRGBTriple(Colour));
 Ed.LLum[Left] := Ed.HSR[Left][HSL_L];
 FrmMain.SetColour(Colour,Left);
end;

procedure SetColoursFromEditMode();
begin
 SetColourHRSLUM(TRUE,Colours[Ed.CurEditType][TRUE]);
 SetColourHRSLUM(FALSE,Colours[Ed.CurEditType][FALSE]);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
 L : Integer;
begin
 Application.Title := WND_TITLE;
 SetCaption;

 Undo.Data   := Nil;
 Undo.R.Left := -1;

 UndoRedoProc := TheUndoRedoProc;

 Selection.Left := -1;

 RecentFiles := TStringList.Create;
 if FileExists(ExtractFileDir(ParamStr(0))+'\recentfiles') then
 RecentFiles.LoadFromFile(ExtractFileDir(ParamStr(0))+'\recentfiles');

 BuildRecentFiles();

 BuildBGList;

 PanelOLDWNDPROC := EditingPanel.WindowProc;
 EditingPanel.WindowProc := PanelWndProc;

 ImgOLDWNDPROC := BackgroundImg.WindowProc;
 BackgroundImg.WindowProc := ImgWndProc;

 MakePaletteImg(PaletteImg,True);

 TileImage(BackgroundImg,CTB4x4);
 BackgroundImg.Canvas.Brush.Style := bsClear;

 Ed.CurBackground := CTB2x2;

 TileImage(PreviewBack,CTB16x16);
 PreviewBack.Canvas.Brush.Style := bsClear;

 DoubleBuffered              := True;
 ScrollBox.DoubleBuffered    := True;
 LayersBox.DoubleBuffered    := True;
 EditingPanel.DoubleBuffered := True;
 PreviewInnerPanel.DoubleBuffered := True;
 ColorPanel.DoubleBuffered := True;

 Colours[0][TRUE]  := clWhite;
 Colours[0][FALSE] := clBlack;

 Colours[1][TRUE]  := clGreen;
 Colours[1][FALSE] := clGreen;

 Colours[2][TRUE]  := clWhite;
 Colours[2][FALSE] := clBlack;

 ZBuffCol1.Position := 0;
 ZBuffCol2.Position := 255;

 ZBuffCol1Label.Caption := IntToStr(255-ZBuffCol1.Position);
 ZBuffCol2Label.Caption := IntToStr(255-ZBuffCol2.Position);

 SetColoursFromEditMode();

 PreviewInnerPanel.Left := PreviewPanel.ClientWidth div 2 - PreviewInnerPanel.Width div 2;

 TrackBar := TSTrackBar.Create(FramePanel);
 TrackBar.Left   := 30;
 TrackBar.Top    := (FramePanel.ClientHeight div 2)-11;
 TrackBar.Width  := FramePanel.ClientWidth-(TrackBar.Left*2);
 TrackBar.Height := 25;
 TrackBar.Ctl3D  := True;
 TrackBar.Enabled := False;
 TrackBar.Max := 1;
 TrackBar.Orientation := trHorizontal;
 TrackBar.ParentCtl3D := False;
 TrackBar.Frequency := 1;
 TrackBar.Position := 0;
 TrackBar.SelEnd := 0;
 TrackBar.SelStart := 0;
 TrackBar.TabOrder := 1;
 TrackBar.ThumbLength := 18;
 TrackBar.TickMarks := tmBottomRight;
 TrackBar.TickStyle := tsNone;
 TrackBar.OnChange := TrackBarChange;
 TrackBar.Parent := FramePanel;

 NextFrameButton.Left := FramePanel.ClientWidth-PrevFrameButton.Left-NextFrameButton.Width-1;

 Ed.CreateLayers(LayersBox,PngImageList.PngImages.Items[0].PngImage,PngImageList.PngImages.Items[1].PngImage, LayerCheckClick, LayerClick);

 for L := 0 to 255 do
  Ed.CreateLayerImg(L,EditingPanel,LayerMouseDown,LayerMouseMove);

 Ed.CreateLayerImg(256,EditingPanel,LayerMouseDown,LayerMouseMove);
 Ed.CreateLayerImg(257,EditingPanel,LayerMouseDown,LayerMouseMove);
 Ed.CreateLayerImg(258,EditingPanel,LayerMouseDown,LayerMouseMove);
end;

procedure TFrmMain.BuildExtraDataList();
var
 L : Integer;
 T : PSmallInt;
 X,Y,Z : SmallInt;
begin
 ExtraDataList.Clear;
 ExtraDataList.Items.BeginUpdate;

 if (Ed.CurFrame.ExtraDataSize > 0) then
 begin
  T := Ed.CurFrame.ExtraData;
  for L := 0 to (Ed.CurFrame.ExtraDataSize div 6)-1 do
  begin
   X := T^+ED.Width div 2;
   Inc(Cardinal(T),2);
   Y := T^+Ed.Height div 2;
   Inc(Cardinal(T),2);
   Z := T^;
   Inc(Cardinal(T),2);
   ExtraDataList.Items.Add(IntToStr(X) + ', ' + IntToStr(Y) + ', ' + IntToStr(Z));
  end;
 end;

 ExtraDataList.Items.EndUpdate;
end;

procedure TFrmMain.TrackBarChange(Sender: TObject);
var
 L : Integer;
begin
 if Ed.Loading or not Assigned(Ed.GMFactory) then Exit;

 if UndoUpdateTimer.Enabled then
 UndoUpdateTimerTimer(Nil);

 Ed.Loading := True;

 Ed.CurFrame := Ed.GMFactory.Frame[TrackBar.Position];

 FrameLabel.Caption := IntToStr(TrackBar.Position+1)+'/'+IntToStr(Ed.GMFactory.FrameCount);

 for L := 1 to Ed.CurFrame.Count do
  Ed.UpdateLayerImg(L);

 if Ed.LastLayers > Ed.CurFrame.Count then
 for L := Ed.CurFrame.Count+1 to Ed.LastLayers do
 begin
  Ed.LayerImgs[L-1].Visible := False;
  Ed.EdLayers.Layers[L-1].Panel.Visible := False;
 end;

 Ed.UpdateSideColLayerImg();
 Ed.UpdateZBuffLayerImg();
                            
 Ed.LastLayers := Ed.CurFrame.Count;

 Ed.CurLayer := Ed.GetLayer(Ed.CurFrame,Ed.CurFrame.Selected);
 DeleteLayerButton.Enabled := Ed.CurFrame.Selected > 1;

 UpdateUndoData;

 if LastFrame <> TrackBar.Position then
 ClearUndoRedo;

 LastFrame := TrackBar.Position;

 PreviewTimerTimer(Nil);

 BuildExtraDataList();
 EDEditButton.Enabled := False;
 EDDeleteButton.Enabled := False;

 Ed.Loading := False;
end;

function EvenNumber(Value : Integer) : Integer;
begin
 Result := Value;
 if Odd(Result) then
 Inc(Result);
end;

procedure TFrmMain.EditingPanelResize(Sender: TObject);
begin
 BackgroundImg.SetBounds(BackgroundImg.Left,BackgroundImg.Top,EvenNumber(EditingPanel.ClientWidth),EvenNumber(EditingPanel.ClientHeight));
end;

procedure TFrmMain.Exit1Click(Sender: TObject);
begin
 Close;
end;

procedure TFrmMain.SetZoom(Value : Byte);
begin
 if (Ed.Zoom = Value) or (Value < 1) then Exit;

 if not Assigned(Ed.GMFactory) then
  TileImage(BackgroundImg,CTB2x2)
 else
 if not FakeLevelBackgroundButton.Down and (((Ed.Zoom > 1) or (Ed.Zoom = 0)) and (Value = 1)) then
  TileImage2(BackgroundImg,CTB16x16,EvenNumber(Ed.Width),EvenNumber(Ed.Height))
 else
 if FakeLevelBackgroundButton.Down or ((Value > 1) and (Ed.Zoom <= 1)) then
  TileImage2(BackgroundImg,Ed.CurBackground,EvenNumber(Ed.Width),EvenNumber(Ed.Height));

 Ed.Zoom := Value;

 EditingPanel.ClientWidth  := Ed.Zoom*Ed.Width;
 EditingPanel.ClientHeight := Ed.Zoom*Ed.Height;

 ZoomInButton.Enabled  := Ed.Zoom < 8;
 ZoomOutButton.Enabled := Ed.Zoom > 1;

 SelectionTimerTimer(Nil);

 SetCaption;
end;

procedure TFrmMain.StartLoad;
begin
 Ed.Loading := True;

 UndoUpdateTimer.Enabled := False;

 ActiveControl := Nil;

 if Assigned(Ed.GMFactory) then
 begin
  Ed.GMFactory.Free;
  Ed.GMFactory := Nil;
 end;

 EditingPanel.Visible := False;

 UpdateSelection(-1,-1,-1,-1);

 LastFrame := -1;
end;

procedure TFrmMain.EnableToolBar;
begin
 OriginalSizeButton.Enabled := True;
 ZoomOutButton.Enabled      := True;
 ZoomInButton.Enabled       := True;

 TrackBar.Enabled           := True;
 PrevFrameButton.Enabled    := Ed.GMFactory.FrameCount > 1;
 NextFrameButton.Enabled    := Ed.GMFactory.FrameCount > 1;
 PrevFrameButton2.Enabled   := Ed.GMFactory.FrameCount > 1;
 NextFrameButton2.Enabled   := Ed.GMFactory.FrameCount > 1;

 AddLayerButton.Enabled     := True;

 SideColsButton.Enabled     := Assigned(Ed.GMFactory.Frame[0].SideCol);
 ZBufferButton.Enabled      := Assigned(Ed.GMFactory.Frame[0].ZBuff);
 ZBufferButtonClick(Nil);

 FrameLabel.Enabled         := True;

 SelectionButton.Enabled    := True;
 PencilButton.Enabled       := True;
 DropperButton.Enabled      := True;
 EraserButton.Enabled       := True;

 SaveAsButton.Enabled       := True;
 SaveAs1.Enabled            := True;
 Resize1.Enabled            := True;

 FakeLevelBackgroundButton.Enabled := True;

 Edit1.Visible := True;
 Selection1.Visible := True;

 EDPButton.Enabled := True;

 ColorPanel.Enabled         := True;
 EditTypePanel.Enabled      := True;

 Export1.Enabled := True;

 MakePaletteImg(PaletteImg,False);
end;

procedure TFrmMain.AfterLoad;
begin
 TrackBar.Position := 0;
 TrackBar.Max      := Ed.GMFactory.FrameCount-1;

 Ed.Zoom := 0;
 SetZoom(4);

 Ed.DrawLock := True;
 Ed.Loading := False;

 EditingPanel.Visible := True;

 TrackBarChange(Nil);
 EnableToolBar;

 StatusBar.Panels[1].Text := IntToStr(Ed.Width)+'x'+IntToStr(Ed.Height);

 Ed.Edited := False;
 SetCaption;

 DrawLockTimer.Enabled := True;
end;

procedure TFrmMain.OriginalSizeButtonClick(Sender: TObject);
begin
 SetZoom(1);
end;

procedure TFrmMain.ZoomOutButtonClick(Sender: TObject);
begin
 SetZoom(Ed.Zoom div 2);
end;

procedure TFrmMain.ZoomInButtonClick(Sender: TObject);
begin
 SetZoom(Ed.Zoom * 2);
end;

procedure TFrmMain.LayerCheckClick(Sender: TObject);
begin
 PreviewTimer.Enabled := True;
end;

procedure TFrmMain.LayerClick(Sender: TObject);
begin
 DeleteLayerButton.Enabled := Ed.CurFrame.Selected > 1;

 UpdateUndoData;
end;

procedure TFrmMain.PanelWndProc(var Message: TMessage);
begin
 if Message.Msg = WM_ERASEBKGND then
  asm nop end
 else
  PanelOLDWNDPROC(Message);
end;

procedure TFrmMain.ImgWndProc(var Message: TMessage);
begin
 if Message.Msg = WM_ERASEBKGND then
  asm nop end
 else
  ImgOLDWNDPROC(Message);
end;

function PalImgCol(X,Y : Integer; PaletteImg : TImage; Left : Boolean) : TColor;
begin
 Result := hicolor_to_tcolor(color_to_hicolor(HSLToRGB2(X/PaletteImg.Width,(PaletteImg.Height-1-Y)/PaletteImg.Height,0.5)));
end;

procedure TFrmMain.PaletteImgMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 Color : TColor;
 CT : Boolean;
begin
 CT := Button <> mbRight;
 Color := PalImgCol(X,Y,PaletteImg,CT);
 Ed.HSR[CT] := RGBToHSL(SetRGBTriple(Color));
 Ed.HSR[CT][HSL_L] := Ed.LLum[CT];
 SetColour(Color,CT);
 Color := hicolor_to_tcolor(color_to_hicolor(HSLToRGB2(Ed.HSR[CT][HSL_H],Ed.HSR[CT][HSL_S],Ed.LLum[CT])));
 SetColour(Color,CT,True);
end;

procedure TFrmMain.PaletteImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
 Color : TColor;
 CT : Boolean;
begin
 if not ((ssLeft in Shift) or (ssRight in Shift)) then Exit;

 CT := (ssLeft in Shift);

 Color := PalImgCol(X,Y,PaletteImg,CT);
 Ed.HSR[CT] := RGBToHSL(SetRGBTriple(Color));
 Ed.HSR[CT][HSL_L] := Ed.LLum[CT];
 SetColour(Color,CT);
 Color := hicolor_to_tcolor(color_to_hicolor(HSLToRGB2(Ed.HSR[CT][HSL_H],Ed.HSR[CT][HSL_S],Ed.LLum[CT])));
 SetColour(Color,CT,True);
end;

function LayerRGB16Pixel(X,Y : Integer) : Pointer;
begin
 Result := Pointer(Cardinal(Ed.CurLayer.RGB16) + (Y*Ed.Width+X)*2);
end;

function LayerTransPixel(X,Y : Integer) : Pointer;
begin
 Result := Pointer(Cardinal(Ed.CurLayer.Trans) + (Y*Ed.Width+X));
end;

function FrameSideColPixel(X,Y : Integer) : Pointer;
begin
 Result := Pointer(Cardinal(Ed.CurFrame.SideCol) + (Y*Ed.Width+X));
end;

function FrameZBufferPixel(X,Y : Integer) : Pointer;
begin
 Result := Pointer(Cardinal(Ed.CurFrame.ZBuff) + (Y*Ed.Width+X));
end;

procedure TFrmMain.FinishedDrawing(FullRedraw : Boolean);
begin
 Ed.CurFrame.Edited := True;

 if FullRedraw then
 case Ed.CurEditType of
  0 : begin
       Ed.UpdateSelectedLayerImg();
      end;
  1 : begin
       Ed.UpdateSideColLayerImg();
      end;
  2 : begin
       Ed.UpdateZBuffLayerImg();
      end;
  end;

 PreviewTimer.Enabled := True;
 if not Ed.Edited then
 begin
  Ed.Edited := True;
  SetCaption;
 end;
end;

//Yo make it faster update the image's pixels to match the data. Rather then copying the whole image over...
procedure SetLayerImgPixel(Img : TImage; X,Y : Integer; Colour : TColor; Alpha : Byte);
var
 PNG : TPNGObject;
 Buf : Pointer;
begin
 PNG := TPNGObject(Img.Picture.Graphic);

 Buf := PNG.Scanline[Y];
 Inc(Cardinal(Buf),X*3);
 TRGBTriple(Buf^) := SetRGBTriple(Colour);

 Buf := PNG.AlphaScanline[Y];
 Inc(Cardinal(Buf),X);
 Byte(Buf^) := Alpha;

 Img.Invalidate;
end;

procedure TFrmMain.DrawOnLayer_Pencil(X,Y : Integer; Colour : TColor);
begin
 case Ed.CurEditType of
  0 : begin
       Word(LayerRGB16Pixel(X,Y)^) := color_to_hicolor(Colour);
       Byte(LayerTransPixel(X,Y)^) := 255;
       SetLayerImgPixel(Ed.LayerImgs[Ed.CurFrame.Selected-1],X,Y,Colour,255);
      end;
  1 : if Byte(LayerTransPixel(X,Y)^) > 0 then begin
       Byte(FrameSideColPixel(X,Y)^) := 255;
       SetLayerImgPixel(Ed.LayerImgs[256],X,Y,MakeGrayGreen(Word(LayerRGB16Pixel(X,Y)^)),255);
      end;
  2 : if Byte(LayerTransPixel(X,Y)^) > 0 then begin
       Byte(FrameZBufferPixel(X,Y)^) := GetRValue(Colour);
       SetLayerImgPixel(Ed.LayerImgs[257],X,Y,Colour,255);
      end;
 end;

 UndoAddPixel(X,Y);

 FinishedDrawing(False);
end;

procedure TFrmMain.DrawOnLayer_Dropper(X,Y : Integer; Left : Boolean);
var
 Colour : Integer;
begin
 if Byte(LayerTransPixel(X,Y)^) <= 0 then Exit;

 case Ed.CurEditType of
  0 : Colour := hicolor_to_tcolor(Word(LayerRGB16Pixel(X,Y)^));
  2 : Colour := MakeGray(Byte(FrameZBufferPixel(X,Y)^));
  else
   Exit;
 end;

 Ed.HSR[Left] := RGBToHSL(SetRGBTriple(Colour));
 Ed.LLum[Left] := Ed.HSR[Left][HSL_L];

 SetColour(Colour,Left);
end;

procedure TFrmMain.DrawOnLayer_Eraser(X,Y : Integer);
begin
 case Ed.CurEditType of
  0 : begin
       Byte(LayerTransPixel(X,Y)^) := 0;
       SetLayerImgPixel(Ed.LayerImgs[Ed.CurFrame.Selected-1],X,Y,RGB(0,0,0),0);
      end;
  1 : begin
       Byte(FrameSideColPixel(X,Y)^) := 0;
       SetLayerImgPixel(Ed.LayerImgs[256],X,Y,MakeGrayDarker(Word(LayerRGB16Pixel(X,Y)^)),255);
      end;
  2 : if Byte(LayerTransPixel(X,Y)^) > 0 then
      begin
       Byte(FrameZBufferPixel(X,Y)^) := 0;
       SetLayerImgPixel(Ed.LayerImgs[257],X,Y,RGB(0,0,0),255);
      end;
  else
   Exit;
 end;

 UndoAddPixel(X,Y);

 FinishedDrawing(False);
end;

procedure TFrmMain.DrawOnLayer(X,Y : Integer; Left : Boolean);
var
 Colour : TColor;
begin
 if Ed.Loading or Ed.DrawLock or (X < 0) or (Y < 0) or (X >= Ed.Width) or (Y >= Ed.Height) then Exit;

 Colour := Colours[Ed.CurEditType][Left];

 case Ed.CurEditType of
  0 : begin
      end;
  1 : if not Assigned(Ed.CurFrame.SideCol) then
      begin
       GetMem(Ed.CurFrame.SideCol,Ed.Width*Ed.Height);
       ZeroMemory(Ed.CurFrame.SideCol,Ed.Width*Ed.Height);
       SideColsButton.Enabled := True;
      end;
  2 : if not Assigned(Ed.CurFrame.ZBuff) then
      begin
       GetMem(Ed.CurFrame.ZBuff,Ed.Width*Ed.Height);
       ZeroMemory(Ed.CurFrame.ZBuff,Ed.Width*Ed.Height);
      end;
 end;

 case TDRAWTOOLS(Ed.DrawTool) of
  DRAW_PENCIL  : DrawOnLayer_Pencil(X,Y,Colour);
  DRAW_DROPPER : DrawOnLayer_Dropper(X,Y,Left);
  DRAW_ERASER  : DrawOnLayer_Eraser(X,Y);
 end;
end;

procedure UpdateSelection(X1,Y1,X2,Y2 : Integer);
begin
 if X1 < X2 then
 begin
  Selection.Left  := X1;
  Selection.Right := X2;
 end
 else
 begin
  Selection.Left  := X2;
  Selection.Right := X1;
 end;

 if Y1 < Y2 then
 begin
  Selection.Top    := Y1;
  Selection.Bottom := Y2;
 end
 else
 begin
  Selection.Top    := Y2;
  Selection.Bottom := Y1;
 end;

// FrmMain.SelectionTimer.Enabled := True;
 FrmMain.SelectionTimerTimer(Nil);

 FrmMain.Copy1.Enabled   := (Selection.Left >= 0) and (Selection.Right-Selection.Left+Selection.Bottom-Selection.Top > 0);
 FrmMain.Cut1.Enabled    := FrmMain.Copy1.Enabled;
 FrmMain.Delete1.Enabled := FrmMain.Copy1.Enabled;
end;

procedure TFrmMain.LayerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 SX,SY : Integer;
begin
 if TDrawTools(Ed.DrawTool) = DRAW_SELECT then
 begin
  SX := Min(Max(0,X div Ed.Zoom),Ed.Width-1);
  SY := Min(Max(0,Y div Ed.Zoom),Ed.Height-1);
  SelectionDown[0] := SX;
  SelectionDown[1] := SY;
  UpdateSelection(SX,SY,SX,SY);
 end
 else
  DrawOnLayer(X div Ed.Zoom,Y div Ed.Zoom,Button <> mbRight);
end;

procedure TFrmMain.LayerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
 SX,SY : Integer;
begin
 StatusBar.Panels[0].Text := IntToStr(X div Ed.Zoom) + ',' + IntToStr(Y div Ed.Zoom);

 if not ((ssLeft in Shift) or (ssRight in Shift)) then Exit;

 if TDrawTools(Ed.DrawTool) = DRAW_SELECT then
 begin
  SX := Min(Max(0,X div Ed.Zoom),Ed.Width-1);
  SY := Min(Max(0,Y div Ed.Zoom),Ed.Height-1);
  UpdateSelection(SelectionDown[0],SelectionDown[1],SX,SY)
 end
 else
  DrawOnLayer(X div Ed.Zoom,Y div Ed.Zoom,ssLeft in Shift);
end;

procedure TFrmMain.DrawLockTimerTimer(Sender: TObject);
begin
 DrawLockTimer.Enabled := False;
 Ed.DrawLock := False;
end;

function TFrmMain.SetFocusedControl(Control: TWinControl): Boolean;
begin
 if (Control is TTrackBar) then
 Result := False
 else
 Result := Inherited SetFocusedControl(Control);

 if (ActiveControl is TTrackBar) then
 ActiveControl := Nil;
end;

//////////////////////////////////////////////////////////////////////////////


//Not needed anymore since SetFocusedControl works better.
procedure TSTrackBar.WndProc(var Message: TMessage);
begin
 case Message.Msg of
  WM_SETFOCUS: Exit;
 end;

 Inherited WndProc(Message);
end;

//////////////////////////////////////////////////////////////////////////////

procedure TFrmMain.PrevFrameButtonClick(Sender: TObject);
begin
 if TrackBar.Position = 0 then
  TrackBar.Position := TrackBar.Max
 else
  TrackBar.Position := TrackBar.Position-1;
end;

procedure TFrmMain.NextFrameButtonClick(Sender: TObject);
begin
 if TrackBar.Position = TrackBar.Max then
  TrackBar.Position := 0
 else
  TrackBar.Position := TrackBar.Position+1;
end;

procedure TFrmMain.AddLayerButtonClick(Sender: TObject);
var
 NID : Byte;
begin
 if (Ed.CurFrame.Selected = 1) or (Ed.CurFrame.Selected = Ed.CurFrame.Count) then
 begin
  Ed.GMFactory.AddLayer(Ed.CurFrame,1);
  Ed.CurLayer := Ed.GMFactory.GetLayer(Ed.CurFrame,Ed.CurFrame^.Count);

  Ed.CurFrame.Selected := Ed.CurFrame^.Count;
 end
 else
 begin
  IncreaseUndoLayers(Ed.CurFrame.Selected);
  Ed.GMFactory.InsertLayer(Ed.CurFrame,Ed.CurFrame.Selected,1);
  Ed.CurLayer := Ed.GMFactory.GetLayer(Ed.CurFrame,Ed.CurFrame.Selected);
 end;
 
 Ed.GMFactory.SetupLayer(Ed.CurLayer);
 ZeroMemory(Ed.CurLayer.RGB16,Ed.Width*Ed.Height*2);
 ZeroMemory(Ed.CurLayer.Trans,Ed.Width*Ed.Height);
 Ed.CurFrame.Edited := True;

 TrackBarChange(Nil);

 DeleteLayerButton.Enabled := Ed.CurFrame.Selected > 1;
end;

procedure TFrmMain.PreviewTimerTimer(Sender: TObject);
begin
 PreviewTimer.Enabled := False;

 try
  case Ed.CurEditType of
   1 : PreviewImage.Picture.Assign(Ed.GetSideColAsPNG);
   2 : PreviewImage.Picture.Assign(Ed.GetZBuffAsPNG);
   else
    PreviewImage.Picture.Assign(Ed.GetLayerAsPNG(0));
  end;
 except
  asm nop end;
 end;

 PreviewImage.Stretch := (Ed.Width >= PreviewImage.Width) or (Ed.Height >= PreviewImage.Height);
end;

procedure TFrmMain.ZBufferButtonClick(Sender: TObject);
var
 L : Integer;
 ShowZBuff : Boolean;
 ShowSide  : Boolean;
 Layer : PGMFLayer;
begin
 ShowZBuff := ZBufferButton.Down;
 ShowSide  := SideColsButton.Down;

 for L := 0 to Ed.CurFrame.Count-1 do
 begin
  Layer := Ed.GMFactory.GetLayer(Ed.CurFrame,L+1);
  Ed.LayerImgs[L].Visible := (not (ShowZBuff or ShowSide)) and (Layer.Vis);
 end;

 Ed.LayerImgs[257].Visible := ShowZBuff;

 if ShowZBuff then
  Ed.UpdateZBuffLayerImg;

 Ed.LayerImgs[256].Visible := ShowSide;

 if ShowSide then
  Ed.UpdateSideColLayerImg;
end;

procedure TFrmMain.PencilButtonClick(Sender: TObject);
begin
 Ed.DrawTool := TComponent(Sender).Tag;
end;

procedure TFrmMain.OpenFile(Filename : AnsiString);
begin
 StartLoad;
  Ed.LoadGM(Filename,False);
 AfterLoad;

 SaveButton.Enabled := True;
 Save1.Enabled      := SaveButton.Enabled;

 AddToRecent();
end;

procedure TFrmMain.Open1Click(Sender: TObject);
begin
 if CheckFileSave() then Exit;

 FrmOpen.ShowModal;
 if not FrmOpen.O then Exit;

 OpenFile(FrmOpen.Filename);
end;

procedure SaveAsGM(Filename : AnsiString; UseSideCols : Boolean = True; IsUnit : Boolean = False; HasHair : Boolean = False);
var
 GM : TGM_File;
 GMType : Byte;
 EXT : AnsiString;
 F : Integer;
 Frame : PGMFFrame;
 Layer : PGMFLayer;
 Palette : TGMFPalette;
 ZBuffer,
 Z : PByte;
 NoZBuff : Boolean;
 RGB : PByte;
 GMPal : TGM_Palette;
begin
 Ed.Edited := False;

 EXT := LowerCase(ExtractFileExt(Filename));

 if EXT = '.gma' then
  GMType := GM_TYPE_A
 else
 if EXT = '.gme' then
  GMType := GM_TYPE_E
 else
 if EXT = '.gms' then
  GMType := GM_TYPE_S
 else
 if EXT = '.gmx' then
  GMType := GM_TYPE_X
 else
  GMType := GM_TYPE_Z;

 NoZBuff := GMType <> GM_TYPE_Z;

 GM := TGM_File.Create(GMType);
  GM.Width  := Ed.Width;
  GM.Height := Ed.Height;
  GM.AddFrames(Ed.GMFactory.FrameCount);

  Palette := Ed.GMFactory.MakePalette(UseSideCols,IsUnit,HasHair);

  GetMem(ZBuffer,Ed.Width*Ed.Height);
  ZeroMemory(ZBuffer,Ed.Width*Ed.Height);

  for F := 0 to Ed.GMFactory.FrameCount-1 do
  begin
   Frame := Ed.GMFactory.Frame[F];

   if GMType in [GM_TYPE_A,GM_TYPE_E] then
   begin
    RGB := Pointer(Ed.GMFactory.GetFrameRGB(Frame));
     GM.SetFrameRGB(F,RGB,Ed.GMFactory.GetLayer(Frame,0).Trans);
    FreeMem(RGB);
   end
   else
   begin
    if Assigned(Frame.ZBuff) then
     Z := Frame.ZBuff
    else
     Z := ZBuffer;

    RGB := Ed.GMFactory.GetFrameWithPalette(Frame,Palette,UseSideCols);
     GM.SetFrameRGB8(F,RGB,Z);
    FreeMem(RGB);
   end;

   if Frame.ExtraDataSize > 0 then
   begin
    if Assigned(GM.Frame[F].ExtraData) then
    FreeMem(GM.Frame[F].ExtraData);

    GM.Frame[F].ExtraDataSize := Frame.ExtraDataSize+4;

    GetMem(GM.Frame[F].ExtraData,GM.Frame[F].ExtraDataSize);
    CopyMemory(Pointer(Cardinal(GM.Frame[F].ExtraData)+4),Frame.ExtraData,Frame.ExtraDataSize);
   end;
  end;

  FreeMem(ZBuffer);

  GM.SaveToFile(Filename);
 GM.Free;

 if not (GMType in [GM_TYPE_S,GM_TYPE_A,GM_TYPE_E]) then
 begin
  for F := 0 to 255 do
  GMPal[F] := Palette[F];

  SaveOWPalette(ChangeFileExt(Filename,'.p16'),GMPal);
 end;
end;

procedure SaveAsGMOptions(Filename : AnsiString);
begin
 FrmSaveOptions.SetGM(Filename,Ed.GMFactory);
 FrmSaveOptions.ShowModal;

 if not FrmSaveOptions.O then Exit;

 SaveAsGM(Filename,FrmSaveOptions.HasSideCols.Checked,FrmSaveOptions.IsUnitCheck.Checked,FrmSaveOptions.UnitFallCheck.Checked);
end;

function OpenSaveFileDialog(Parent: TWinControl; const DefExt, Filter, InitialDir, Title: string; var FileName: string;
                            MustExist, OverwritePrompt, NoChangeDir, DoOpen: Boolean; var FilterIndex : DWORD): Boolean;
var
  ofn: TOpenFileName;
  szFile: array[0..MAX_PATH] of Char;
begin
  Result := False;
  FillChar(ofn, SizeOf(TOpenFileName), 0);
  with ofn do
  begin
    lStructSize := SizeOf(TOpenFileName);
    hwndOwner := Parent.Handle;
    lpstrFile := szFile;
    nMaxFile := SizeOf(szFile);
    if (Title <> '') then
      lpstrTitle := PChar(Title);
    if (InitialDir <> '') then
      lpstrInitialDir := PChar(InitialDir);
    StrPCopy(lpstrFile, FileName);
    lpstrFilter := PChar(StringReplace(Filter, '|', #0,[rfReplaceAll])+#0#0);
    if DefExt <> '' then
      lpstrDefExt := PChar(DefExt);
    nFilterIndex := FilterIndex;
  end;

  if MustExist then
    ofn.Flags := ofn.Flags or OFN_FILEMUSTEXIST;

  if OverwritePrompt then
    ofn.Flags := ofn.Flags or OFN_OVERWRITEPROMPT;

  if NoChangeDir then
    ofn.Flags := ofn.Flags or OFN_NOCHANGEDIR;

  if DoOpen then
  begin
    if GetOpenFileName(ofn) then
    begin
      Result := True;
      FileName := StrPas(szFile);
    end;
  end
  else
  begin
    if GetSaveFileName(ofn) then
    begin
      Result := True;
      FileName := StrPas(szFile);
    end;
  end;

  FilterIndex := ofn.nFilterIndex;
end;

function SaveDialogExecute(Dialog : TSaveDialog) : Boolean;
var
 Filename : AnsiString;
 FilterIndex : DWORD;
begin
 Filename := Dialog.FileName;
 FilterIndex := Dialog.FilterIndex;
 Result := OpenSaveFileDialog(FrmMain,Dialog.DefaultExt,Dialog.Filter,Dialog.InitialDir,'Save As',Filename,False,True,False,False,FilterIndex);
 Dialog.FilterIndex := FilterIndex;
 Dialog.FileName    := Filename;
end;

procedure TFrmMain.Save(Filename : AnsiString);
begin
 if Lowercase(ExtractFileExt(FileName)) = '.gmf' then
 begin
  Ed.GMFactory.SaveToFile(FileName);
  Ed.Edited := False;
  SaveButton.Enabled := True;
 end
 else
 begin
  if (Lowercase(ExtractFileExt(FileName)) = '.gms') or (Lowercase(ExtractFileExt(FileName)) = '.gma') or (Lowercase(ExtractFileExt(FileName)) = '.gme') then
   SaveAsGM(Filename)
  else
   SaveAsGMOptions(Filename);
 end;

 SetCaption;
           
 AddToRecent();
end;

procedure TFrmMain.SaveAsButtonClick(Sender: TObject);
begin
 SaveDialog.FileName := ChangeFileExt(ExtractFileName(Ed.GMFactory.Filename),'');
 if not SaveDialogExecute(SaveDialog){SaveDialog.Execute} then Exit;

 Save(SaveDialog.FileName);

 SaveButton.Enabled := Ed.GMFactory.Filename <> 'Untitled';
 Save1.Enabled      := SaveButton.Enabled;
end;

procedure TFrmMain.FakeLevelBackgroundButtonClick(Sender: TObject);
var
 Z : Byte;
begin
// BackgroundCollection.Items.Items[0].PngImage

 if FakeLevelBackgroundButton.Down then
 Ed.CurBackground := Ed.BGImage
 else
 Ed.CurBackground := CTB2x2;

 if FakeLevelBackgroundButton.Down then
  TileImage(PreviewBack,Ed.CurBackground)
 else
  TileImage(PreviewBack,CTB16x16);

 Z := Ed.Zoom;
 Ed.Zoom := 0;
 SetZoom(Z);
end;

procedure TFrmMain.BGMenuItemClick(Sender: TObject);
begin
 Ed.BGImage.Canvas.Draw(0,0,BackgroundCollection.Items[TMenuItem(Sender).Tag].PngImage);
 FakeLevelBackgroundButtonClick(Nil);
end;

procedure TFrmMain.DeleteLayerButtonClick(Sender: TObject);
begin
 RemoveUndoRedos(Ed.CurFrame.Selected);

 Ed.GMFactory.DeleteLayer(Ed.CurFrame,Ed.CurFrame.Selected);
 if Ed.CurFrame.Selected > Ed.CurFrame.Count then
 Ed.CurFrame.Selected := Ed.CurFrame.Count;

 TrackBarChange(Nil);
end;

function GetColLumOverPos(Img : TImage; CT : Boolean) : Integer;
begin
 Result := Img.Top-3+(Img.Height-1)-Trunc(Ed.LLum[CT]*Img.Height);
end;

procedure TFrmMain.SetColLumOver(CT : Boolean);
begin
 if CT then
  ColLum1Over.Top := GetColLumOverPos(Col1Lum,CT)
 else
  ColLum2Over.Top := GetColLumOverPos(Col2Lum,CT);
end;

procedure TFrmMain.SetColByLum(Sender : TObject; Y : Integer);
var
 H   : Integer;
 HSL : THSLArray;
 CT  : Boolean;
begin
 H := TImage(Sender).Height;

 if (Y < 0) or (Y >= H) then Exit;

 Y := (H-1)-Y;

 CT := TImage(Sender).Tag = 0;

 Ed.HSR[CT][HSL_L] := Y/(H-1);
 Ed.LLum[CT] := Ed.HSR[CT][HSL_L];

 SetColLumOver(CT);

 SetColour(hicolor_to_tcolor(color_to_hicolor(HSLToRGB2(Ed.HSR[CT][HSL_H],Ed.HSR[CT][HSL_S],Ed.HSR[CT][HSL_L]))),TImage(Sender).Tag = 0,True);
end;

procedure TFrmMain.Col1LumMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 //if (Button <> mbLeft) then Exit;

 if TImage(Sender).Tag = 0 then
  SetColByLum(Col1Lum,Y)
 else
  SetColByLum(Col2Lum,Y);
end;

procedure TFrmMain.Col1LumMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if not ((ssLeft in Shift) or (ssRight in Shift)) then Exit;

 SetColByLum(Sender,Y);

 if TImage(Sender).Tag = 0 then
  SetColByLum(Col1Lum,Y)
 else
  SetColByLum(Col2Lum,Y);
end;

procedure TFrmMain.ColLum1OverMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 Col1LumMouseDown(Sender,Button,Shift,X,TImage(Sender).Top-Col1Lum.Top+Y);
end;

procedure TFrmMain.ColLum1OverMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 Col1LumMouseMove(Sender,Shift,X,TImage(Sender).Top-Col1Lum.Top+Y);
end;

function TFrmMain.CheckFileSave : Boolean;
var
 Option : Integer;
 T : AnsiString;
begin
 Result := Ed.Edited;
 if not Result then Exit;

 T := Application.Title;
 Application.Title := 'Save?';
 Option := messagedlg('Do you want to save <' + ExtractFileName(Ed.GMFactory.Filename) + '> before continuing?',mtCustom,[mbYes,mbNo,mbCancel], 0);
 Application.Title := T;

 Result := False;

 case Option of
   mrYes    : if Ed.GMFactory.Filename <> 'Untitled' then Save(Ed.GMFactory.Filename) else SaveAsButtonClick(Nil);
   mrCancel : Result := True;
  else
   ;
 end;
end;

procedure TFrmMain.New1Click(Sender: TObject);
var
 Frame : PGMFFrame;
 Layer : PGMFLayer;
 F,L : Integer;
begin
 if CheckFileSave() then Exit;

 FrmNew.ShowModal;
 if not FrmNew.O then Exit;

 SaveButton.Enabled := False;
 Save1.Enabled      := SaveButton.Enabled;

 StartLoad;
  Ed.GMFactory := TGMF.Create(StrToInt(FrmNew.WidthEdit.Text),StrToInt(FrmNew.HeightEdit.Text));
  Ed.GMFactory.AddFrame(StrToInt(FrmNew.FrameEdit.Text));

  //We may not need to setup all layers, but to be safe lets!
  for F := 0 to Ed.GMFactory.FrameCount-1 do
  begin
   Frame := Ed.GMFactory.Frame[F];
   Frame.Edited := True;
   for L := 1 to Frame.Count do
    Ed.GMFactory.SetupLayer(Ed.GMFactory.GetLayer(Frame,L));
  end;

  Ed.GMFactory.Filename := 'Untitled';
 AfterLoad;

 Ed.Edited   := True;
 SetCaption;
end;

procedure TFrmMain.SaveButtonClick(Sender: TObject);
begin
 Save(Ed.GMFactory.Filename);
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := not CheckFileSave();

 if FileExists(ExtractFileDir(ParamStr(0))+'\recentfiles') then
 DeleteFile(ExtractFileDir(ParamStr(0))+'\recentfiles');
 RecentFiles.SaveToFile(ExtractFileDir(ParamStr(0))+'\recentfiles');
end;

procedure TFrmMain.RGBEditTypeClick(Sender: TObject);
begin
 if UndoUpdateTimer.Enabled then
 UndoUpdateTimerTimer(Nil);

 Ed.CurEditType := TComponent(Sender).Tag;
 SetColoursFromEditMode();

// EraserButton.Enabled:= CurEditType <> 2;
 SideColsButton.Down := Ed.CurEditType = 1;
 ZBufferButton.Down  := Ed.CurEditType = 2;

 ColorPanel.Visible  := Ed.CurEditType = 0;
 LayerPanel.Visible  := Ed.CurEditType = 0;

 ColorPanelZBuff.Visible := Ed.CurEditType = 2;
 ColorPanelZBuff.Align := alClient;

 DropperButton.Enabled := Ed.CurEditType <> 1;

 ZBufferButtonClick(Nil);

 UpdateUndoData;

 PreviewTimerTimer(Nil);
end;

procedure TFrmMain.ZBuffCol1Change(Sender: TObject);
begin
 SetColour(MakeGray(255-ZBuffCol1.Position),True);
 ZBuffCol1Label.Caption := IntToStr(255-ZBuffCol1.Position);
end;

procedure TFrmMain.ZBuffCol2Change(Sender: TObject);
begin
 SetColour(MakeGray(255-ZBuffCol2.Position),False);
 ZBuffCol2Label.Caption := IntToStr(255-ZBuffCol2.Position);
end;

procedure TFrmMain.ReOpenClick(Sender : TObject);
begin
 if CheckFileSave() then Exit;

 if FileExists(RecentFiles[TMenuItem(Sender).Tag]) then
 OpenFile(RecentFiles[TMenuItem(Sender).Tag])
 else
 ShowMessage('Error: File no longer exists!');
end;

procedure TFrmMain.About1Click(Sender: TObject);
begin
 FrmAbout.ShowModal;
end;

procedure TFrmMain.Undo1Click(Sender: TObject);
begin
 if UndoUpdateTimer.Enabled then
 UndoUpdateTimerTimer(Nil);

 UndoSystem.Undo(Undo.Layer);
end;

procedure TFrmMain.Redo1Click(Sender: TObject);
begin
 if UndoUpdateTimer.Enabled then
 UndoUpdateTimerTimer(Nil);

 UndoSystem.Redo(Undo.Layer);
end;

procedure CopyToBuff(Dest, Source : Pointer; DX,DY,SW,SH : Integer; Size : Integer);
var
 S : Pointer;
 Y : Integer;
begin
 S := Source;
 for Y := 0 to SH-1 do
 begin
  CopyMemory(Pointer(Cardinal(Dest) + ((Y+DY)*Ed.Width+DX)*Size),S,SW*Size);
  Inc(Cardinal(S),SW*Size);
 end;
end;

procedure TheUndoRedoProc(Data : PUndoRedoData; UR : Byte);
var
 L : PGMFLayer;
begin
 Undo.R := Rect(Data.X,Data.Y,Data.X+Data.W,Data.Y+Data.H);
 DoUpdateUndo(UR,False);

 case Data.Layer of
   256 : begin
          CopyToBuff(Ed.CurFrame.SideCol,Data.Data,Data.X,Data.Y,Data.W,Data.H,1);

          Ed.UpdateSideColLayerImg;
         end;
   257 : begin
          CopyToBuff(Ed.CurFrame.ZBuff,Data.Data,Data.X,Data.Y,Data.W,Data.H,1);

          Ed.UpdateZBuffLayerImg;
         end;
  else
   begin
    L := Ed.GMFactory.GetLayer(Ed.CurFrame,Data.Layer);
    CopyToBuff(L.RGB16,Data.Data,Data.X,Data.Y,Data.W,Data.H,2);
    CopyToBuff(L.Trans,Pointer(Cardinal(Data.Data)+Data.W*Data.H*2),Data.X,Data.Y,Data.W,Data.H,1);

    Ed.UpdateALayerImg(Data.Layer);
   end;
 end;

 Ed.CurFrame.Edited := True;

 FrmMain.PreviewTimer.Enabled := True;

 UpdateUndoData;
end;

procedure TFrmMain.UndoUpdateTimerTimer(Sender: TObject);
begin
 UndoUpdateTimer.Enabled := False;
 UpdateUndoData;
end;

procedure TFrmMain.SelectionTimerTimer(Sender: TObject);
var
 BMP : TBitmap;
 PNG : TPNGObject;
 X,Y : Integer;
 SL,
 ASL : Pointer;
 White : TRGBTriple;
 Area : TRect;
 Scale : Byte;
const
 Trans : Byte = 150;
begin
 SelectionTimer.Enabled := False;

 if (Selection.Left  = -1) or (Selection.Right-Selection.Left+Selection.Bottom-Selection.Top = 0) then
 begin
  Ed.LayerImgs[258].Visible := False;
  Exit;
 end;

 if Ed.Zoom < 2 then
 begin
  Area  := Rect(Selection.Left,Selection.Top,Selection.Right,Selection.Bottom);
  Scale := 1;
 end
 else
 begin
  Area  := Rect(Selection.Left*2,Selection.Top*2,Selection.Right*2+1,Selection.Bottom*2+1);
  Scale := 2;
 end;

 White := SetRGBTriple(clYellow{White});

 PNG := TPNGObject.Create;
  BMP := TBitmap.Create;
   BMP.Width       := Ed.Width*Scale;
   BMP.Height      := Ed.Height*Scale;
   BMP.PixelFormat := pf24bit;

   PNG.Assign(BMP);
   PNG.CreateAlpha;
  BMP.Free;

   for Y := 0 to PNG.Height-1 do
   begin
    ZeroMemory(PNG.ScanLine[Y],PNG.Width*3);
    ZeroMemory(PNG.AlphaScanLine[Y],PNG.Width);
   end;

   for Y := Area.Top to Area.Bottom do
   begin
    SL := PNG.ScanLine[Y];
    Inc(Cardinal(SL),Area.Left*3);

    ASL := PNG.AlphaScanLine[Y];
    Inc(Cardinal(ASL),Area.Left);
    if (Y = Area.Top) or (Y = Area.Bottom) then
     for X := 0 to (Area.Right-Area.Left) do
     begin
      if Odd(X) then
      TRGBTriple(SL^) := White;
      Inc(Cardinal(SL),3);
      Byte(ASL^) := Trans;
      Inc(Cardinal(ASL),1);
     end
    else
    begin
     if not Odd(Y-Area.Top) then
     begin
      TRGBTriple(SL^) := White;
      if (Area.Right-Area.Left+1)-2 > 0 then
     for X := 0 to (Area.Right-Area.Left)-2 do
     begin
      Inc(Cardinal(SL),3);
      TRGBTriple(SL^) := SetRGBTriple(clWhite);
     end;
      Inc(Cardinal(SL),{((Selection.Right-Selection.Left))*}3);
      TRGBTriple(SL^) := White;
     end
     else
     begin
      if (Area.Right-Area.Left+1)-2 > 0 then
      for X := 0 to (Area.Right-Area.Left-2) do
      begin
       Inc(Cardinal(SL),3);
       TRGBTriple(SL^) := SetRGBTriple(clWhite);
      end;
     end;

     Byte(ASL^) := Trans;
     if (Area.Right-Area.Left+1)-2 > 0 then
     for X := 0 to (Area.Right-Area.Left-2) do
     begin
      Inc(Cardinal(ASL),1);
      Byte(ASL^) := 50;
     end; 
     Inc(Cardinal(ASL),1);//(Selection.Right-Selection.Left));
     Byte(ASL^) := Trans;
    end;
   end;

  Ed.LayerImgs[258].Picture.Assign(PNG);
  Ed.LayerImgs[258].Visible := True;
 PNG.Free;
end;

procedure TFrmMain.SelectNone1Click(Sender: TObject);
begin
 UpdateSelection(-1,-1,-1,-1);
end;

procedure TFrmMain.SelectAll1Click(Sender: TObject);
begin
 UpdateSelection(0,0,Ed.Width-1,Ed.Height-1);
end;

function CreatePNGWithSize(W,H : Integer; Trans : Boolean) : TPNGObject;
var
 BMP : TBitmap;
begin
 Result := TPNGObject.Create;
 BMP := TBitmap.Create;
  BMP.Width  := W;
  BMP.Height := H;
  BMP.PixelFormat := pf24bit;
  Result.Assign(BMP);
 BMP.Free;
 if Trans then
 Result.CreateAlpha;
end;

procedure SetClipboardBMP(BMP : TBitmap);
var
 Data: THandle;
 Format: Word;
 Palette: HPALETTE;
begin
 Palette := 0;
 BMP.SaveToClipboardFormat(Format, Data, Palette);
 SetClipboardData(Format, Data);
 if Palette <> 0 then SetClipboardData(CF_PALETTE, Palette);
end;

procedure TFrmMain.Copy1Click(Sender: TObject);
var
 BMP : TBitmap;
 W,H : Integer;
 S,
 SA,D
 ,BD: Pointer;
 X,Y : Integer;
 Hand : Cardinal;
 BitmapInfo  : TBitmapInfoHeader;
 Buf : Pointer;
begin
 if Selection.Left < 0 then Exit;

 W := Selection.Right-Selection.Left+1;
 H := Selection.Bottom-Selection.Top+1;

 BMP := TBitmap.Create;
  BMP.PixelFormat := pf24bit;
  BMP.Width       := W;
  BMP.Height      := H;

  case Ed.CurEditType of
   0 : Ed.GMFactory.LayerToPNG(Ed.CurFrame,Ed.TransferPNG,Ed.CurFrame.Selected);
   1 : Ed.GMFactory.SideColToPNG(Ed.CurFrame,Ed.TransferPNG);
   2 : Ed.GMFactory.ZBuffToPNG(Ed.CurFrame,Ed.TransferPNG);
  end;

  Hand := GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, W*H*4+SizeOf(TBitmapInfoHeader));
  Buf := GlobalLock(Hand);
  with TBitmapInfoHeader(Buf^) do
  begin
   biSize:= sizeof(TBitmapInfoHeader);
   biWidth:= W;
   biHeight:= -H;
   biPlanes:= 1;
   biBitCount:= 32;
   biCompression:= BI_RGB;
   biSizeImage:= 0;
   biXPelsPerMeter:= 0;
   biYPelsPerMeter:= 0;
   biClrUsed:= 0;
   biClrImportant:= 0;
  end;

  Inc(Cardinal(Buf),SizeOf(TBitmapInfoHeader));
  D := Buf;

  for Y := Selection.Top to Selection.Bottom do
  begin
   S  := Ed.TransferPNG.Scanline[Y];
   SA := Ed.TransferPNG.AlphaScanline[Y];
   BD  := BMP.ScanLine[Y-Selection.Top];
   Inc(Cardinal(S),Selection.Left*3);
   Inc(Cardinal(SA),Selection.Left);

   for X := 0 to W-1 do
   begin
    TRGBQuad(D^)  := SetRGBAQuad(TRGBTriple(S^),Byte(SA^));
    TRGBTriple(BD^) := TRGBTriple(S^);
    Inc(Cardinal(D),4);
    Inc(Cardinal(BD),3);
    Inc(Cardinal(S),3);
    Inc(Cardinal(SA),1);
   end;
  end;

  Clipboard.Open;
   EmptyClipboard;
   SetClipboardData(CF_DIB,Hand);
   SetClipboardBMP(BMP); //Fallback. If an app doesn't support our 32Bit DIB they can use this
  Clipboard.Close;
  GlobalUnLock(Hand); 
  BMP.Free;
end;

function ClipboardPaste32DIB(var PNG : TPNGObject) : Boolean;
var
 BitmapInfo  : TBitmapInfoHeader;
 H : Cardinal;
 Data : Pointer;
 Flip : Boolean;
 S,
 DC,
 DA  : Pointer;
 Y,W,
 X : Integer;
begin
 Result := False;

 if not IsClipboardFormatAvailable(CF_DIB) then Exit;

 H := Clipboard.GetAsHandle(CF_DIB);
 if H = 0 then Exit;

 Data := GlobalLock(H);
  BitmapInfo := TBitmapInfoHeader(Data^);

  if (BitmapInfo.biCompression <> BI_RGB) or (BitmapInfo.biBitCount <> 32) then
  begin
   GlobalUnlock(H);
   Exit;
  end;

  Inc(Cardinal(Data),SizeOf(TBitmapInfoHeader));
  Flip := BitmapInfo.biHeight >= 0;
  BitmapInfo.biHeight := Abs(BitmapInfo.biHeight);

  PNG := CreatePNGWithSize(BitmapInfo.biWidth,BitmapInfo.biHeight,true);
  W := BitmapInfo.biWidth;
   S := Data;
   for Y := 0 to PNG.Height-1 do
   begin
    if Flip then
    begin
     DC := PNG.Scanline[(PNG.Height-1)-Y];
     DA := PNG.AlphaScanline[(PNG.Height-1)-Y];
    end
    else
    begin
     DC := PNG.Scanline[Y];
     DA := PNG.AlphaScanline[Y];
    end;

    for X := 0 to W-1 do
    begin
     TRGBTriple(DC^) := SetRGBTriple(TRGBQuad(S^));
     Inc(Cardinal(DC),3);
     Inc(Cardinal(S),3);
     Byte(DA^) := Byte(S^);
     Inc(Cardinal(S),1);
     Inc(Cardinal(DA),1);
    end;
   end;
 GlobalUnlock(H);
 Result := True;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
var
 BMP : TBitmap;
 BitmapInfo  : TBitmapInfo;
 H : Cardinal;
 Data : Pointer;
 Flip : Boolean;
 PNG : TPNGObject;
begin
 if ClipboardPaste32DIB(PNG) then
 begin
  Ed.LayerImgs[0].Picture.Assign(PNG);
  PNG.Free;
  Exit;
 end;
              
 BMP := TBitmap.Create;
 BMP.Assign(Clipboard);
 BMP.PixelFormat := pf32bit;
 Ed.LayerImgs[0].Picture.Assign(BMP);
  BMP.Free;
end;

procedure TFrmMain.Cut1Click(Sender: TObject);
begin
 Copy1Click(Sender);
 Delete1Click(Sender);
end;

procedure TFrmMain.Delete1Click(Sender: TObject);
var
 P,D : Pointer;
 Y,W : Integer;
begin
 if Selection.Left < 0 then Exit;

 if UndoUpdateTimer.Enabled then
 UndoUpdateTimerTimer(Nil);

 W := Selection.Right-Selection.Left+1;

 case Ed.CurEditType of
   0 : P := Ed.CurLayer.Trans;
   1 : P := Ed.CurFrame.SideCol;
   2 : P := Ed.CurFrame.ZBuff;
 end;

 for Y := Selection.Top to Selection.Bottom do
 begin
  D := Pointer(Cardinal(P) + Y*Ed.Width+Selection.Left);
  ZeroMemory(D,W);
 end;

 case Ed.CurEditType of
   0 : Ed.UpdateSelectedLayerImg();
   1 : Ed.UpdateSideColLayerImg();
   2 : Ed.UpdateZBuffLayerImg();
 end;

 UndoAddRect(Selection);
 Ed.CurFrame.Edited := True;
 PreviewTimer.Enabled := True;
end;

procedure TFrmMain.EDPButtonClick(Sender: TObject);
begin
 ExtraDataPanel.Visible := EDPButton.Down;
end;

procedure GetXYZFromEDL(var X,Y,Z : SmallInt);
var
 S : AnsiString;
 P : Integer;
begin
 with FrmMain do
 begin
  if ExtraDataList.ItemIndex < 0 then Exit;
  S := ExtraDataList.Items.Strings[ExtraDataList.ItemIndex];
  P := Pos(',',S);
  X := StrToIntDef(Copy(S,1,P-1),0);
  Delete(S,1,P+1);
  P := Pos(',',S);
  Y := StrToIntDef(Copy(S,1,P-1),0);
  Delete(S,1,P+1);
  Z := StrToIntDef(Trim(S),0);
 end;
end;

procedure TFrmMain.ExtraDataListClick(Sender: TObject);
var
 X,Y,Z : SmallInt;
begin
 EDDeleteButton.Enabled := ExtraDataList.ItemIndex > -1;
 EDEditButton.Enabled   := EDDeleteButton.Enabled;

 if ExtraDataList.ItemIndex > -1 then
 begin
  GetXYZFromEDL(X,Y,Z);
  EDX.Text := IntToStr(X);
  EDY.Text := IntToStr(Y);
  EDZ.Text := IntToStr(Z);
 end;
end;

procedure TFrmMain.ExtraDataListKeyPress(Sender: TObject; var Key: Char);
begin
 ExtraDataListClick(Sender);
end;

procedure TFrmMain.EDXKeyPress(Sender: TObject; var Key: Char);
begin
 if not (Key in ['0'..'9']) and (Key > #13) then
 Key := #0;
end;

procedure TFrmMain.EDAddButtonClick(Sender: TObject);
var
 T : PSmallInt;
begin
 GetMem(T,Ed.CurFrame.ExtraDataSize+6);
 ZeroMemory(T,Ed.CurFrame.ExtraDataSize+6);

 if Assigned(Ed.CurFrame.ExtraData) then
 begin
  CopyMemory(T,Ed.CurFrame.ExtraData,Ed.CurFrame.ExtraDataSize);
  FreeMem(Ed.CurFrame.ExtraData);
 end;
 Ed.CurFrame.ExtraData := T;
 T := Pointer(Cardinal(Ed.CurFrame.ExtraData) + ExtraDataList.Items.Count * 6);

 T^ := StrToIntDef(EDX.Text,0)-ED.Width div 2;
 Inc(Cardinal(T),2);
 T^ := StrToIntDef(EDY.Text,0)-ED.Height div 2;
 Inc(Cardinal(T),2);
 T^ := StrToIntDef(EDZ.Text,0);
 Inc(Ed.CurFrame.ExtraDataSize,6);

 BuildExtraDataList();
 ExtraDataList.ItemIndex := ExtraDataList.Items.Count-1;
 ExtraDataListClick(Nil);
end;

procedure TFrmMain.EDEditButtonClick(Sender: TObject);
type
TThreeSmallInt = Packed Record
 X,Y,Z : SmallInt;
end;
PThreeSmallInt = ^TThreeSmallInt;
var
 T : PThreeSmallInt;
 ID : Integer;
begin
 ID := ExtraDataList.ItemIndex;
 T := Pointer(Cardinal(ED.CurFrame.ExtraData)+ID*6);
 T^.X := StrToIntDef(EDX.Text,0)-ED.Width div 2;
 T^.Y := StrToIntDef(EDY.Text,0)-ED.Height div 2;
 T^.Z := StrToIntDef(EDZ.Text,0);

 ExtraDataList.Items.Strings[ID] := IntToStr(T^.X+ED.Width div 2) + ', ' + IntToStr(T^.Y+ED.Height div 2) + ', ' + IntToStr(T^.Z);
 ExtraDataList.ItemIndex := ID;
 ExtraDataListClick(Nil);
end;

procedure TFrmMain.EDDeleteButtonClick(Sender: TObject);
var
 T  : Pointer;
 ID : Integer;
begin
 if ED.CurFrame.ExtraDataSize-6 <= 0 then
 begin
  if Assigned(ED.CurFrame.ExtraData) then
  FreeMem(ED.CurFrame.ExtraData);
  ED.CurFrame.ExtraDataSize := 0;

  BuildExtraDataList;
  ExtraDataListClick(Nil);
  Exit;
 end;

 Dec(ED.CurFrame.ExtraDataSize,6);
 GetMem(T,ED.CurFrame.ExtraDataSize);

 if ExtraDataList.ItemIndex > 0 then
  CopyMemory(T,ED.CurFrame.ExtraData,6*ExtraDataList.ItemIndex);

 if ExtraDataList.ItemIndex+1 < ExtraDataList.Count then
  CopyMemory(Pointer(Cardinal(T)+6*ExtraDataList.ItemIndex),Pointer(Cardinal(ED.CurFrame.ExtraData)+(ExtraDataList.ItemIndex+1)*6),6*(ExtraDataList.Count-(ExtraDataList.ItemIndex+1)));

 FreeMem(ED.CurFrame.ExtraData);
 ED.CurFrame.ExtraData := T;

 ID := ExtraDataList.ItemIndex;
 BuildExtraDataList;
 ExtraDataList.ItemIndex := Min(ID,ExtraDataList.Count-1);
 ExtraDataListClick(Nil);
end;

procedure TFrmMain.ChangeOffset1Click(Sender: TObject);
begin
 OpenDialogGMs.Filter := Filter_GMALL;
 if not OpenDialogGMs.Execute then Exit;

 FrmChangeOffset.LoadGM(OpenDialogGMs.FileName);
 FrmChangeOffset.ShowModal;
end;

procedure TFrmMain.CopyExtraData1Click(Sender: TObject);
var
 GM,GM2 : TGM_File;
 FN1,FN2 : AnsiString;
 F : Integer;
 FrameA, FrameB : PGM_Frame;
begin
 OpenDialogGMs.Filter := Filter_GMALL;
 if not OpenDialogGMs.Execute then Exit;
 GM := TGM_File.Create(0);
  FN1 := OpenDialogGMs.Filename;
  GM.LoadFromFile(FN1);

 if not OpenDialogGMs.Execute then
 begin
  GM.Free;
  Exit;
 end;

 GM2 := TGM_File.Create(0);
  FN2 := OpenDialogGMs.Filename;
  GM2.LoadFromFile(FN2);

 if GM.FrameCount <> GM2.FrameCount then
 begin
  ShowMessage('GM files contain different number of frames! Aborting!');
  GM.Free;
  GM2.Free;
  Exit;
 end;

 if Messagedlg('Copy Extra Data From ' + ExtractFileName(FN1) + ' to ' + ExtractFileName(FN2) + ' ?',mtError, mbOKCancel, 0) = mrOk then
 begin
  for F := 0 to GM.FrameCount-1 do
  begin
   FrameA := GM.Frame[F];
   FrameB := GM2.Frame[F];

   FrameB.ExtraDataSize := FrameA.ExtraDataSize;

   if Assigned(FrameB.ExtraData) then
   FreeMem(FrameB.ExtraData);

   GetMem(FrameB.ExtraData,FrameB.ExtraDataSize);
   CopyMemory(FrameB.ExtraData,FrameA.ExtraData,FrameB.ExtraDataSize);
  end;

  GM2.SaveToFile(FN2);
 end;

 GM.Free;
 GM2.Free;
end;

function IntToStr4Z(Value : Integer) : AnsiString;
begin
 Result := IntToStr(Value);
 while Length(Result) < 4 do
 Result := '0' + Result;
end;

procedure FindPNGs(Filename : AnsiString; var List : TStringList);
var
 C : Integer;
 FN : AnsiString;
begin
 List := TStringList.Create;

 List.BeginUpdate;
 Filename := Copy(Filename,1,Length(Filename)-8);
 C := 0;
 Repeat
  FN := Filename + IntToStr4Z(C) + '.png';
  if not FileExists(FN) then
  Break;

  List.Add(FN);
  Inc(C);
 Until 1 = 2;
 List.EndUpdate;
end;

procedure GetSideColFromPNG(Frame : PGMFFrame; FN : AnsiString; PNG : TPNGObject);
var
 RGB : PRGBTriple;
 S   : PByte;
 Y,X : Integer;
begin
 if not FileExists(FN) then Exit;

 PNG.LoadFromFile(FN);

 if not Assigned(Frame.SideCol) then
 GetMem(Frame.SideCol,PNG.Width*PNG.Height);

 S := Frame.SideCol;

 for Y := 0 to PNG.Height-1 do
 begin
  RGB := PNG.Scanline[Y];
  for X := 0 to PNG.Width-1 do
  begin
   if (RGB.rgbtRed = 0) and (RGB.rgbtGreen = 255) and (RGB.rgbtBlue = 0) then
   S^ := 255
   else
   S^ := 0;

   Inc(Cardinal(RGB),3);
   Inc(Cardinal(S),1);
  end;
 end;
end;

procedure GetZBuffFromPNG(Frame : PGMFFrame; FN : AnsiString; PNG : TPNGObject);
var
 RGB : PRGBTriple;
 Z   : PByte;
 Y,X : Integer;
begin
 if not FileExists(FN) then Exit;

 PNG.LoadFromFile(FN);

 if not Assigned(Frame.ZBuff) then
 GetMem(Frame.ZBuff,PNG.Width*PNG.Height);

 Z := Frame.ZBuff;

 for Y := 0 to PNG.Height-1 do
 begin
  RGB := PNG.Scanline[Y];
  for X := 0 to PNG.Width-1 do
  begin
   Z^ := RGB.rgbtRed;

   Inc(Cardinal(RGB),3);
   Inc(Cardinal(Z),1);
  end;
 end;
end;

procedure TFrmMain.ImportPNG(KeepTransparency : Boolean);
var
 PNG     : TPNGObject;
 PNGList : TStringList;
 X,Y     : Integer;
 Frame   : PGMFFrame;
 Layer   : PGMFLayer;
 BMP     : TBitmap;
 Factory : TGMF;
 RGB,
 Alpha     : Pointer;
begin
 if CheckFileSave() then Exit;

 if not OpenPNGDialog.Execute then Exit;

 StartLoad;
  PNG := TPNGObject.Create;
   PNG.LoadFromFile(OpenPNGDialog.Filename);
   ED.GMFactory := TGMF.Create(PNG.Width,PNG.Height);

  Factory := ED.GMFactory;

  FindPNGs(OpenPNGDialog.Filename,PNGList);

  Factory.AddFrame(PNGList.Count);
                                    
  BMP := TBitmap.Create;
  BMP.Width  := Factory.Width;
  BMP.Height := Factory.Height;
  BMP.PixelFormat := pf16bit;

  for X := 0 to PNGList.Count-1 do
  begin
   Frame := ED.GMFactory.Frame[X];
   Frame.Edited := True;
   Layer := ED.GetLayer(Frame,1);
   ED.GMFactory.SetupLayer(Layer);

   PNG.LoadFromFile(PNGList[X]);

   Assert(PNG.Header.ColorType <> COLOR_PALETTE,'Unable to Import COLOR_PALETTE PNGs');

   if not Assigned(PNG.AlphaScanLine[PNG.Height-1]) then
   PNG.CreateAlpha;

   BMP.Canvas.FillRect(Rect(0,0,BMP.Width,BMP.Height));
   BMP.Canvas.Draw(0,0,PNG);
   RGB   := Layer.RGB16;
   Alpha := Layer.Trans;
   for Y := 0 to PNG.Height-1 do
   begin
    CopyMemory(RGB,BMP.ScanLine[Y],2*PNG.Width);
    Inc(Cardinal(RGB),2*PNG.Width);
    CopyMemory(Alpha,PNG.AlphaScanLine[Y],PNG.Width);
    Inc(Cardinal(Alpha),PNG.Width);
   end;

   if not KeepTransparency then
   begin
    // Fix Alpha
    Alpha := Layer.Trans;
    for Y := 0 to PNG.Height*PNG.Width-1 do
    begin
     if Byte(Alpha^) <= 100 then
      Byte(Alpha^) := 0
     else
      Byte(Alpha^) := 255;
     Inc(Cardinal(Alpha),1);
    end;
   end;

   GetSideColFromPNG(Frame,ChangeFileExt(PNGList[X],'s.png'),PNG);
   GetZBuffFromPNG(Frame,ChangeFileExt(PNGList[X],'z.png'),PNG);
  end;

  BMP.Free;
  PNG.Free;
 AfterLoad;

 SaveButton.Enabled := False;
 Save1.Enabled      := SaveButton.Enabled;

 Ed.Edited   := True;
 SetCaption;
end;

procedure TFrmMain.FromPNGs1Click(Sender: TObject);
begin
 ImportPNG(False);
end;

procedure TFrmMain.AsPNG1Click(Sender: TObject);
var
 Frame : PGMFFrame;
 F     : Integer;
 PNG   : TPNGObject;
 FN    : AnsiString;
begin
 SavePNGDialog.FileName := ChangeFileExt(ED.GMFactory.Filename,'0000.png');

 if not SavePNGDialog.Execute then Exit;

 FN := Copy(SavePNGDialog.FileName,1,Length(SavePNGDialog.FileName)-8);

 PNG := TPNGObject.Create;

 for F := 0 to ED.GMFactory.FrameCount-1 do
 begin
  Frame := ED.GMFactory.Frame[F];

  ED.GMFactory.LayerToPNG(Frame,PNG,0);
  PNG.SaveToFile(FN+IntToStr4Z(F)+'.png');

  ED.GMFactory.SideColToPNG_Actual(Frame,PNG);
  PNG.SaveToFile(FN+IntToStr4Z(F)+'s.png');

  ED.GMFactory.ZBuffToPNG(Frame,PNG);
  PNG.SaveToFile(FN+IntToStr4Z(F)+'z.png');
 end;

 PNG.Free;
end;

procedure TFrmMain.MassZBufferChange1Click(Sender: TObject);
begin
 OpenDialogGMs.Filter := Filter_GMZ;

 if not OpenDialogGMs.Execute then Exit;

 FrmZBufferChange.LoadGM(OpenDialogGMs.FileName);
 FrmZBufferChange.ShowModal;
end;

procedure TFrmMain.SHPImport1Click(Sender: TObject);
var
 FN1 : AnsiString;
begin
 OpenDialogGMs.Filter := Filter_SHP;
 if not OpenDialogGMs.Execute then Exit;
 FN1 := OpenDialogGMs.FileName;

 OpenDialogGMs.Filter := Filter_GMZXA;
 if not OpenDialogGMs.Execute then Exit;

 FrmSHPImport.Load(FN1,OpenDialogGMs.FileName);
 FrmSHPImport.ShowModal;
end;

procedure TFrmMain.ChangeExtraData1Click(Sender: TObject);
begin
 OpenDialogGMs.Filter := Filter_GMALL;
 if not OpenDialogGMs.Execute then Exit;

 FrmChangeExtraData.Load(OpenDialogGMs.FileName);
 FrmChangeExtraData.ShowModal;
end;

procedure TFrmMain.Resize1Click(Sender: TObject);
var
 Zoom : Byte;
begin
 FrmResize.ShowModal;
 if not FrmResize.O then Exit;

 with FrmResize do
 ED.GMFactory.Resize(LeftValue.Value,TopValue.Value,RightValue.Value,BottomValue.Value);

 ED.Edited := True;
 Zoom := Ed.Zoom;
 Ed.Zoom := 0;

 SetZoom(Zoom);

 TrackBarChange(Nil);

 SetCaption;
end;

procedure TFrmMain.FromPNGPreserveTransparency1Click(Sender: TObject);
begin
 ImportPNG(True);
end;

end.
