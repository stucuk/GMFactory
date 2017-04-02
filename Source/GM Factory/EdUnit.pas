unit EdUnit;

interface

uses Windows, ExtCtrls, pngimage, GMF, Graphics, HSLUnit, Controls, EdLayers, Forms;

type
TLayerImgs = Array [0..258] of TImage;
THSREDArray = Array [BOOLEAN] of THSLArray;
TEDLum = Array [BOOLEAN] of Single;

TEditorStuff = Class(TObject)
protected
 FGMFactory   : TGMF;
 FLoading     : Boolean;
 FDrawLock    : Boolean;
 FTransferPNG : TPNGObject;
 FLayerImgs   : TLayerImgs; //256 is Side Colour, 257 is ZBuffer, 258 is Selection
 FLastLayers  : Integer;
 FZoom        : Byte;
 FCurFrame    : PGMFFrame;
 FCurLayer    : PGMFLayer;
 FDrawTool    : Byte;
 FEdited      : Boolean;
 FCurBackground : TBitmap;
 FBGImage     : TBitmap;
 FCurEditType : Byte; //RGB, Side Cols, ZBuffer
 FEdLayers    : TEdLayers;
 function GetFilename : AnsiString;
 procedure SetFilename(Value : AnsiString);
public
 HSR         : THSREDArray;
 LLum        : TEDLum;

 constructor Create;
// destructor Destroy; override;

 function GetLayerAsPNG() : TPNGObject; overload; //Don't Free Result
 function GetLayerAsPNG(ID : Integer) : TPNGObject; overload; //Don't Free Result
 function GetSideColAsPNG() : TPNGObject; //Don't Free Result
 function GetZBuffAsPNG() : TPNGObject; //Don't Free Result
 function GetCurrentAsPNG() : TPNGObject; //Don't Free Result

 function GetLayer(Frame : PGMFFrame; Index : Byte) : PGMFLayer;

 function Width : Word;
 function Height : Word;

 procedure SetupSideCol;
 procedure SetupZBuff;

 procedure CreateLayerImg(L : Integer; Panel : TPanel; MouseDown : TMouseEvent; MouseMove : TMouseMoveEvent);
 procedure CreateLayers(LayerBox : TScrollBox; Vis0,Vis1 : TPNGObject; LCCCallback, LCCallback : TLCCCallback);

 procedure UpdateLayerImg(L : Integer);
 procedure UpdateSideColLayerImg();
 procedure UpdateZBuffLayerImg();
 procedure UpdateALayerImg(L : Byte);
 procedure UpdateSelectedLayerImg();

 procedure LoadGM(Filename : AnsiString; OneFrame : Boolean);

 property Loading     : Boolean read FLoading write FLoading;
 property DrawLock    : Boolean read FDrawLock write FDrawLock;
 property TransferPNG : TPNGObject read FTransferPNG write FTransferPNG;
 property LayerImgs   : TLayerImgs read FLayerImgs; //256 is Side Colour, 257 is ZBuffer, 258 is Selection
 property EdLayers    : TEdLayers read FEdLayers;
 property LastLayers  : Integer read FLastLayers write FLastLayers;
 property Zoom        : Byte read FZoom write FZoom;
 property CurFrame    : PGMFFrame read FCurFrame write FCurFrame;
 property CurLayer    : PGMFLayer read FCurLayer write FCurLayer;
 property DrawTool    : Byte read FDrawTool write FDrawTool;
 property Edited      : Boolean read FEdited write FEdited;
 property CurBackground : TBitmap read FCurBackground write FCurBackground;
 property BGImage     : TBitmap read FBGImage;
 property CurEditType : Byte read FCurEditType write FCurEditType; //RGB, Side Cols, ZBuffer

 property GMFactory : TGMF read FGMFactory write FGMFactory;

 property Filename : AnsiString read GetFilename write SetFilename;
end;

var
 ED : TEditorStuff = Nil;

implementation

uses GMLoader;

constructor TEditorStuff.Create;
begin
 Inherited;

 FGMFactory := Nil;

 FLoading     := False;
 FDrawLock    := True;
 FTransferPNG := TPNGObject.Create;
 //FLayerImgs   : TLayerImgs; //256 is Side Colour, 257 is ZBuffer, 258 is Selection
 FLastLayers  := 0;
 FZoom        := 2;
 FCurFrame    := Nil;
 FCurLayer    := Nil;
 FDrawTool    := 0;
 FEdited      := False;
 FCurBackground := Nil;
 FBGImage             := TBitmap.Create;
 FBGImage.Width       := 64;
 FBGImage.Height      := 64;
 FBGImage.PixelFormat := pf24bit;
 //FHSR         : Array [BOOLEAN] of THSLArray;
 //FLLum        : Array [BOOLEAN] of Single;
 FCurEditType := 0;
 FEdLayers := Nil;
end;

procedure TEditorStuff.CreateLayers(LayerBox : TScrollBox; Vis0,Vis1 : TPNGObject; LCCCallback, LCCallback : TLCCCallback);
begin
 FEdLayers := TEdLayers.Create(LayerBox,Vis0,Vis1, LCCCallback, LCCallback);
end;

procedure TEditorStuff.UpdateSideColLayerImg();
begin
 FLayerImgs[256].Picture.Assign(GetSideColAsPNG());
end;

procedure TEditorStuff.UpdateZBuffLayerImg();
begin
 FLayerImgs[257].Picture.Assign(GetZBuffAsPNG());
end;

procedure TEditorStuff.UpdateALayerImg(L : Byte);
begin
 FLayerImgs[L-1].Picture.Assign(GetLayerAsPNG(L));
end;

procedure TEditorStuff.UpdateSelectedLayerImg();
begin
 UpdateALayerImg(FCurFrame.Selected);
end;

function TEditorStuff.GetLayerAsPNG() : TPNGObject; //Don't Free Result
begin
 Result := GetLayerAsPNG(FCurFrame.Selected);
end;

function TEditorStuff.GetLayerAsPNG(ID : Integer) : TPNGObject; //Don't Free Result
begin
 Result := FTransferPNG;
 GMFactory.LayerToPNG(FCurFrame,Result,ID);
end;

function TEditorStuff.GetSideColAsPNG() : TPNGObject; //Don't Free Result
begin
 Result := FTransferPNG;
 GMFactory.SideColToPNG(FCurFrame,Result);
end;

function TEditorStuff.GetZBuffAsPNG() : TPNGObject; //Don't Free Result
begin
 Result := FTransferPNG;
 GMFactory.ZBuffToPNG(FCurFrame,Result);
end;

function TEditorStuff.GetCurrentAsPNG() : TPNGObject; //Don't Free Result
begin
 case FCurEditType of
   1 : Result := GetSideColAsPNG();
   2 : Result := GetZBuffAsPNG();
  else
   Result := GetLayerAsPNG();
 end;
end;

function TEditorStuff.GetLayer(Frame : PGMFFrame; Index : Byte) : PGMFLayer;
begin
 Result := FGMFactory.GetLayer(Frame,Index);
end;

function TEditorStuff.Width : Word;
begin
 Result := FGMFactory.Width;
end;

function TEditorStuff.Height : Word;
begin
 Result := FGMFactory.Height;
end;

procedure TEditorStuff.SetupSideCol;
begin
 if not Assigned(FCurFrame.SideCol) then
 begin
  GetMem(FCurFrame.SideCol,Width*Height);
  ZeroMemory(FCurFrame.SideCol,Width*Height);
 end;
end;

procedure TEditorStuff.SetupZBuff;
begin
 if not Assigned(FCurFrame.ZBuff) then
 begin
  GetMem(FCurFrame.ZBuff,Width*Height);
  ZeroMemory(FCurFrame.ZBuff,Width*Height);
 end;
end;

function TEditorStuff.GetFilename : AnsiString;
begin
 Result := FGMFactory.Filename;
end;

procedure TEditorStuff.SetFilename(Value : AnsiString);
begin
 FGMFactory.Filename := Value;
end;

procedure TEditorStuff.CreateLayerImg(L : Integer; Panel : TPanel; MouseDown : TMouseEvent; MouseMove : TMouseMoveEvent);
begin
 FLayerImgs[L] := TImage.Create(Panel);
 FLayerImgs[L].Parent       := Panel;
 FLayerImgs[L].Proportional := True;
 FLayerImgs[L].Stretch      := True;
 FLayerImgs[L].Align        := alClient;
 FLayerImgs[L].OnMouseDown  := MouseDown;
 FLayerImgs[L].OnMouseMove  := MouseMove;
 FLayerImgs[L].Visible      := True;
 FLayerImgs[L].Cursor       := crCross;
end;

procedure TEditorStuff.UpdateLayerImg(L : Integer);
var
 Layer : PGMFLayer;
begin
 Layer := FGMFactory.GetLayer(FCurFrame,L);

 FLayerImgs[L-1].Picture.Assign(GetLayerAsPNG(L));
 FEdLayers.SetVis(L-1,Layer.Vis);

 FEdLayers.Layers[L-1].Panel.Visible := True;

 if CurFrame.Selected = L then
  FEdLayers.Layers[L-1].Panel.Color := clBtnHighlight
 else
  FEdLayers.Layers[L-1].Panel.Color := clBtnFace;

 if Layer.Vis then
 begin
  if not FLayerImgs[L-1].Visible then
  FLayerImgs[L-1].Visible := True;
 end
 else
  FLayerImgs[L-1].Visible := False;
end;

procedure TEditorStuff.LoadGM(Filename : AnsiString; OneFrame : Boolean);
begin
 GMLoader.LoadGM(Filename,OneFrame,FGMFactory);
end;

initialization
 ED := TEditorStuff.Create;

finalization
 ED.Free;

end.
