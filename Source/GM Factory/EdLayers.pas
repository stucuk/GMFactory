unit EdLayers;

interface

uses pngimage, forms, PngSpeedButton, ExtCtrls, StdCtrls;

type
TEdLayer = Record
 Vis      : TPngSpeedButton;
 Panel    : TPanel;
 Caption  : TLabel;
end;

TLCCCallback = Procedure (Sender : TObject) of object;

TEdLayerArray = Array [0..255] of TEdLayer;

TEdLayers = Class(TObject)
protected
 FLayerBox : TScrollBox;
 FVis0,
 FVis1     : TPNGObject;
 FLCCallback,
 FLCCCallback : TLCCCallback;
 FLayers   : TEdLayerArray;
 procedure CreateLayer(L : Integer);
 procedure LayerCheckClick(Sender: TObject);
 procedure LayerClick(Sender: TObject);
public
 constructor Create(LayerBox : TScrollBox; Vis0,Vis1 : TPNGObject; LCCCallback, LCCallback : TLCCCallback);
 destructor Destroy; override;

 procedure SetVis(L : Integer; Value : Boolean);

 property Layers : TEdLayerArray read FLayers write FLayers;
end;

implementation

uses SysUtils, Graphics, EdUnit, GMF, Controls;

constructor TEdLayers.Create(LayerBox : TScrollBox; Vis0,Vis1 : TPNGObject; LCCCallback, LCCallback : TLCCCallback);
var
 X : Integer;
begin
 Inherited Create();
 FLayerBox := LayerBox;
 FVis0     := Vis0;
 FVis1     := Vis1;
 FLCCCallback := LCCCallback;
 FLCCallback := LCCallback;

 for X := 0 to High(FLayers) do
 CreateLayer(X);
end;

destructor TEdLayers.Destroy;
begin
 Inherited;
end;

procedure TEdLayers.SetVis(L : Integer; Value : Boolean);
begin
 FLayers[L].Vis.Down := Value;

 if Value then
  FLayers[L].Vis.PngImage := FVis0
 else
  FLayers[L].Vis.PngImage := FVis1;
end;

procedure TEdLayers.LayerCheckClick(Sender: TObject);
var
 Button : TPngSpeedButton;
 Layer : PGMFLayer;
begin
 if Ed.Loading then Exit;

 Button := TPngSpeedButton(Sender);

 Ed.LayerImgs[Button.Tag].Visible := Button.Down;

 Layer := Ed.GMFactory.GetLayer(Ed.CurFrame,Button.Tag+1);
 Layer.Vis := Button.Down;
 Ed.CurFrame.Edited := True;

 if Button.Down then
  Button.PngImage := FVis0
 else
  Button.PngImage := FVis1;

 FLCCCallback(Sender);
end;

procedure TEdLayers.LayerClick(Sender: TObject);
begin
 if Ed.CurFrame.Selected = TControl(Sender).Tag+1 then Exit;

 FLayers[Ed.CurFrame.Selected-1].Panel.Color := clBtnFace;
 FLayers[TControl(Sender).Tag].Panel.Color          := clBtnHighlight;

 Ed.CurFrame.Selected := TControl(Sender).Tag+1;
 Ed.CurLayer := Ed.GMFactory.GetLayer(Ed.CurFrame,Ed.CurFrame.Selected);

 FLCCallback(Sender);
end;

procedure SetupControl(Control : TControl; Parent : TWinControl; L,T,W,H : Integer);
begin
 Control.Parent     := Parent;
 Control.SetBounds(L,T,W,H);
end;

function CreatePanel(Parent : TWinControl; L,T,W,H : Integer) : TPanel;
begin
 Result := TPanel.Create(Parent);
 SetupControl(Result,Parent,L,T,W,H);
end;

function CreateSpeedButton(Parent : TWinControl; L,T,W,H : Integer) : TPngSpeedButton;
begin
 Result := TPngSpeedButton.Create(Parent);
 SetupControl(Result,Parent,L,T,W,H);
end;

function CreateLabel(Parent : TWinControl; L,T,W,H : Integer) : TLabel;
begin
 Result := TLabel.Create(Parent);
 SetupControl(Result,Parent,L,T,W,H);
end;

procedure TEdLayers.CreateLayer(L : Integer);
begin
 FLayers[L].Panel            := CreatePanel(FLayerBox,0,20*L,FLayerBox.ClientWidth,20);
 FLayers[L].Panel.Visible    := False;
 FLayers[L].Panel.Caption    := '';
 FLayers[L].Panel.DoubleBuffered := True;

 FLayers[L].Vis              := CreateSpeedButton(FLayers[L].Panel,2,2,16,16);
 FLayers[L].Vis.GroupIndex   := 345+L;
 FLayers[L].Vis.AllowAllUp   := True;
 FLayers[L].Vis.PngImage     := FVis0;
 FLayers[L].Vis.Tag          := L;
 FLayers[L].Vis.OnClick      := LayerCheckClick;
 FLayers[L].Vis.Flat         := True;
 if L = 0 then
 FLayers[L].Vis.Enabled      := False;

 FLayers[L].Caption          := CreateLabel(FLayers[L].Panel,20,0,FLayers[L].Panel.ClientWidth-20,FLayers[L].Panel.ClientHeight);
 FLayers[L].Caption.AutoSize := False;
 FLayers[L].Caption.Caption  := 'Layer ' + IntToStr(L);
 FLayers[L].Caption.Transparent := True;
 FLayers[L].Caption.Layout   := tlCenter;
 FLayers[L].Caption.Tag      := L;
 FLayers[L].Caption.OnClick  := LayerClick;
end;

end.
