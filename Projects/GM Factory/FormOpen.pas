unit FormOpen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, PngImageList, ExtCtrls, StdCtrls, FileCtrl, ComCtrls,
  ShellCtrls, MPCommonObjects,MPCommonUtilities, EasyListview, Buttons, PngSpeedButton,
  pngimage;

type
  TFrmOpen = class(TForm)
    UpFolderButton: TPngSpeedButton;
    FileList: TEasyListview;
    FilterComboBox1: TFilterComboBox;
    PreviewPanel: TPanel;
    CheckBGImage: TImage;
    PreviewImage: TImage;
    FileNameEdit: TEdit;
    OkButton: TButton;
    CloseButton: TButton;
    ShellComboBox: TShellComboBox;
    DirectoryListBox: TDirectoryListBox;
    PreviewTimer: TTimer;
    PngImageCollection: TPngImageCollection;
    ScrollBox: TScrollBox;
    PreviewAllImg: TImage;
    procedure UpFolderButtonClick(Sender: TObject);
    procedure ShellComboBoxChange(Sender: TObject);
    procedure ShellComboBoxKeyPress(Sender: TObject; var Key: Char);
    procedure FileListDblClick(Sender: TCustomEasyListview;
      Button: TCommonMouseButton; MousePos: TPoint;
      ShiftState: TShiftState; var Handled: Boolean);
    procedure FileListItemImageDraw(Sender: TCustomEasyListview;
      Item: TEasyItem; Column: TEasyColumn; ACanvas: TCanvas;
      const RectArray: TEasyRectArrayObject;
      AlphaBlender: TEasyAlphaBlender);
    procedure FileListItemImageDrawIsCustom(Sender: TCustomEasyListview;
      Item: TEasyItem; Column: TEasyColumn; var IsCustom: Boolean);
    procedure FileListItemSelectionsChanged(Sender: TCustomEasyListview);
    procedure FileListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FileNameEditChange(Sender: TObject);
    procedure DirectoryListBoxChange(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CloseButtonClick(Sender: TObject);
    procedure FileListKeyAction(Sender: TCustomEasyListview;
      var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure PreviewTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure FileListClick;
    procedure DrawFullPreview;
  public
    { Public declarations }
    Filename : AnsiString;
    O : Boolean;
    procedure BuildFileList;
  end;

var
  FrmOpen: TFrmOpen;
  PNGs    : Array of TPNGObject;

implementation

{$R *.dfm}

uses GMF, GMLoader, CheckerUnit, INIFiles;

var
PreviewGMF   : TGMF = Nil;
PreviewFrame : Integer = 0;
PreviewPNG   : TPNGObject = Nil;
CheckBMP : TBitmap = Nil;

procedure DrawPreview;
begin
 if PreviewGMF.FrameCount < 1 then
 begin
  FrmOpen.PreviewImage.Picture.Bitmap.Width := 0;
  Exit;
 end;

 if not Assigned(PreviewPNG) then
 PreviewPNG := TPNGObject.Create;

 PreviewGMF.FrameToPNG(PreviewGMF.Frame[PreviewFrame],PreviewPNG);

 FrmOpen.PreviewImage.Picture.Assign(PreviewPNG);

 FrmOpen.PreviewImage.Invalidate;
end;

function FileDirectory : AnsiString;
begin
 Result := FrmOpen.DirectoryListBox.Directory;
end;

procedure ClearPNGs;
var
 X : Integer;
begin
 for X := 0 to High(PNGs) do
 if Assigned(PNGs[X]) then
 PNGs[X].Free;
end;

procedure SetFileDirectory(Value : AnsiString);
begin
 if Pos(':\',Value) > 1 then
  FrmOpen.DirectoryListBox.Directory := Value
 else
 begin
  FrmOpen.FileList.Items.Clear;
  ClearPNGs;
  SetLength(PNGs,0);
  FrmOpen.UpFolderButton.Enabled := False;
 end;
end;

function AddTrailer(Input : AnsiString) : AnsiString;
begin
 Result := Input;
 if (Result = '') or (Result[Length(Result)] = '\') then
 Exit;

 if Result[Length(Result)] = '/' then
 Result[Length(Result)] := '\'
 else
 Result := Result + '\';
end;

procedure TFrmOpen.UpFolderButtonClick(Sender: TObject);
var
 D : AnsiString;
begin
 D := AddTrailer(FileDirectory) + '..\';
 if not DirectoryExists(D) then Exit;
 SetFileDirectory(D);
end;

procedure TFrmOpen.ShellComboBoxChange(Sender: TObject);
begin
 SetFileDirectory(ShellComboBox.Path);
end;

procedure TFrmOpen.ShellComboBoxKeyPress(Sender: TObject; var Key: Char);
begin
 if Key = #13 then
 begin
  if DirectoryExists(ShellComboBox.Text) then
   SetFileDirectory(ShellComboBox.Text);
 end;
end;

procedure TFrmOpen.FileListClick;
var
 Item : TEasyItem;
 D : AnsiString;
begin
 Item := FileList.Selection.First;

 if not Assigned(Item) then Exit;

 if ((ExtractFileExt(Item.Caption) <> '') and (Item.Caption <> '..') and not DirectoryExists(AddTrailer(FileDirectory) + Item.Caption + '\')) then
 begin
  OkButtonClick(Nil);
  Exit;
 end;

 D := AddTrailer(FileDirectory) + Item.Caption + '\';
 if not DirectoryExists(D) then Exit;
 SetFileDirectory(D);
end;

procedure TFrmOpen.FileListDblClick(Sender: TCustomEasyListview;
  Button: TCommonMouseButton; MousePos: TPoint; ShiftState: TShiftState;
  var Handled: Boolean);
begin
 FileListClick;
end;

function GetPNGRectangle(IconRect : TRect; PNG : TPNGObject) : TRect;
var
  W,
  H : Integer;
  S : Extended;
begin
 W := IconRect.Right-IconRect.Left;
 H := IconRect.Bottom-IconRect.Top;

 if (PNG.Width < W) and (PNG.Height < H) then
 begin
  W := PNG.Width;
  H := PNG.Height;
 end
 else
 if PNG.Width > PNG.Height then
 begin
  S := PNG.Height/PNG.Width;
  H := Trunc(H*S);
 end
 else
 begin
  S := PNG.Width/PNG.Height;
  W := Trunc(W*S);
 end;

 Result.Left   := IconRect.Left+((IconRect.Right-IconRect.Left)-W) div 2;
 Result.Top    := IconRect.Top+((IconRect.Bottom-IconRect.Top)-H) div 2;
 Result.Right  := Result.Left + W;
 Result.Bottom := Result.Top  + H;
end;

procedure GetPNG(Item : TEasyItem);
var
 PNG : TPNGObject;
 Factory : TGMF;
begin
 LoadGM(AddTrailer(FileDirectory) + Item.Caption,True,Factory);

 PNG := TPNGObject.Create;
 Factory.FrameToPNG(Factory.Frame[0],PNG);
 PNGs[Item.ImageIndex] := PNG;
end;

procedure BuildCheckBMP(W,H : Integer);
var
 X,Y  : Integer;
begin
 if Assigned(CheckBMP) then Exit;

 CheckBMP             := TBitmap.Create;
 CheckBMP.Width       := W;
 CheckBMP.Height      := H;
 CheckBMP.PixelFormat := pf24bit;

 for Y := 0 to ((CheckBMP.Height+16) div 16)-1 do
 for X := 0 to ((CheckBMP.Width+16) div 16)-1 do
 CheckBMP.Canvas.Draw(X*16,Y*16,CTB16x16);
end;

procedure TFrmOpen.FileListItemImageDraw(Sender: TCustomEasyListview;
  Item: TEasyItem; Column: TEasyColumn; ACanvas: TCanvas;
  const RectArray: TEasyRectArrayObject; AlphaBlender: TEasyAlphaBlender);
var
 PNG : TPNGObject;
begin
 if (Item.ImageIndex > High(PNGs)) then Exit;

 if (Item.ImageIndex < 0) then
 begin
  if Item.Caption = '..' then
  PNG := PngImageCollection.Items[1].PngImage
  else
  PNG := PngImageCollection.Items[0].PngImage;

  ACanvas.StretchDraw(GetPNGRectangle(RectArray.IconRect,PNG),PNG);
  Exit;
 end;

 if not Assigned(PNGs[Item.ImageIndex]) then
 begin
              {
  if not IsValidSHP(AddTrailer(SHPDirectory) + Item.Caption) then
  begin
   Item.Visible := False;
   Exit;
  end;      }

  GetPNG(Item);
 end;

 BuildCheckBMP(192,192);

// ACanvas.StretchDraw(RectArray.IconRect,CheckBMP);
 ACanvas.CopyRect(RectArray.IconRect,CheckBMP.Canvas,Rect(0,0,RectArray.IconRect.Right-RectArray.IconRect.Left,RectArray.IconRect.Bottom-RectArray.IconRect.Top));
 ACanvas.StretchDraw(GetPNGRectangle(RectArray.IconRect,PNGs[Item.ImageIndex]),PNGs[Item.ImageIndex]);
end;

procedure TFrmOpen.FileListItemImageDrawIsCustom(
  Sender: TCustomEasyListview; Item: TEasyItem; Column: TEasyColumn;
  var IsCustom: Boolean);
begin
 IsCustom := True;
end;

function IsValidExt(EXT : AnsiString) : Boolean;
begin
 Result := (EXT = '.gmf') or (EXT = '.gmz') or (EXT = '.gms') or (EXT = '.gmx');
end;

procedure TFrmOpen.DrawFullPreview;
var
 PNG : TPNGObject;
 F   : Integer;
begin
 ScrollBox.VertScrollBar.Position := 0;
 ScrollBox.HorzScrollBar.Position := 0;
 PreviewAllImg.Width  := PreviewGMF.Width;
 PreviewAllImg.Height := PreviewGMF.Height*PreviewGMF.FrameCount;
 TileImage(PreviewAllImg,CTB16x16);
 //PreviewAllImg.Width  := 158;
 //PreviewAllImg.Height := 158*PreviewGMF.FrameCount;

 PNG := TPNGObject.Create;

 for F := 0 to PreviewGMF.FrameCount-1 do
 begin
  PreviewGMF.FrameToPNG(PreviewGMF.Frame[F],PNG);
  PreviewAllImg.Picture.Bitmap.Canvas.Draw(0,F*PreviewGMF.Height,PNG);
 end;

 PNG.Free;
end;

procedure TFrmOpen.FileListItemSelectionsChanged(
  Sender: TCustomEasyListview);
var
 Item : TEasyItem;
begin
 Item := FileList.Selection.First;

 if not Assigned(Item) or not IsValidExt(ExtractFileExt(Item.Caption)) then
 begin
  FileNameEdit.Text := '';
  Filename := '';
  PreviewTimer.Enabled := False;
  PreviewImage.Picture.Bitmap.Width := 0;
  PreviewAllImg.Picture.Bitmap.Width := 0;
  Exit;
 end;

 if Assigned(PreviewGMF) then
 PreviewGMF.Free;

 Filename := AddTrailer(FileDirectory) + Item.Caption;
 FileNameEdit.Text := ExtractFileName(Filename);

 LoadGM(Filename,False,PreviewGMF);

 PreviewFrame := 0;

 DrawPreview;
 DrawFullPreview;

 PreviewTimer.Enabled := PreviewGMF.FrameCount > 1;
end;

procedure TFrmOpen.FileListMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
 ActiveControl := FileList;
end;

procedure TFrmOpen.FileNameEditChange(Sender: TObject);
begin
 OkButton.Enabled := FileNameEdit.Text <> '';
end;

var
 LastDir   : AnsiString = '';
 FileFNList : TStringList = Nil;

procedure GetFileFNList;
var
 sr : TSearchRec;
begin
 if not Assigned(FileFNList) then
  FileFNList := TStringList.Create
 else
  FileFNList.Clear;

    if FindFirst(LastDir+'\*', faAnyFile, sr) = 0 then
    begin
      repeat
       if (sr.Name = '.') then Continue;

        if ((sr.Attr and faDirectory) > 0) then
         FileFNList.Add(sr.Name);
         
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

    if FindFirst(LastDir+'\*', faAnyFile, sr) = 0 then
    begin
      repeat
       if (sr.Name = '..') or (sr.Name = '.') then Continue;

        if ((sr.Attr and faDirectory) > 0) then
         Continue
        else
        if (IsValidExt(lowercase(ExtractFileExt(sr.Name)))) then
         FileFNList.Add(sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
end;

procedure TFrmOpen.BuildFileList;
var
 X,C  : Integer;
 Item : TEasyItem;
begin
 if FileDirectory = LastDir then Exit;

 LastDir := FileDirectory;
 ShellComboBox.Path := LastDir;

 UpFolderButton.Enabled := False;

 FileList.BeginUpdate;
  FileList.Items.Clear;
  ClearPNGs;
  SetLength(PNGs,0);

  GetFileFNList;

  C := 0;
  for X := 0 to FileFNList.Count-1 do
  begin
   Item            := FileList.Items.Add();
   if IsValidExt(ExtractFileExt(FileFNList[X])) then
   begin
    Item.ImageIndex := C;
    Inc(C);
   end
   else
    Item.ImageIndex := -1;
   Item.Caption    := FileFNList[X];
   if FileFNList[X] = '..' then
   UpFolderButton.Enabled := True;
  end;

  SetLength(PNGs,C);

  FileFNList.Clear;

 FileList.EndUpdate();
end;

procedure TFrmOpen.DirectoryListBoxChange(Sender: TObject);
begin
 BuildFileList;
end;

procedure TFrmOpen.OkButtonClick(Sender: TObject);
begin
 O := True;
 Close;
end;



procedure TFrmOpen.FormCreate(Sender: TObject);
var
 INI : TINIFile;
 Dir : AnsiString;
begin
 O := False;
 SetLength(PNGs,0);
 PreviewPanel.DoubleBuffered := True;
 ScrollBox.DoubleBuffered := True;
 FileList.DoubleBuffered := True;

 BuildCheckBMP(192,192);
 CheckBGImage.Picture.Assign(CheckBMP);

 INI := TIniFile.Create(ExtractFileDir(ParamStr(0))+'\opendialog.ini');
  Dir := INI.ReadString('Main','Directory',ExtractFileDir(ParamStr(0))+'\');
 INI.Free;

 if not DirectoryExists(Dir) then
 Dir := ExtractFileDir(ParamStr(0))+'\';
 
 SetFileDirectory(Dir);
end;

procedure TFrmOpen.FormClose(Sender: TObject; var Action: TCloseAction);
var
 INI : TINIFile;
begin
 PreviewTimer.Enabled := False;

 INI := TIniFile.Create(ExtractFileDir(ParamStr(0))+'\opendialog.ini');
  INI.WriteString('Main','Directory',FileDirectory);
  INI.UpdateFile;
 INI.Free;
end;

procedure TFrmOpen.CloseButtonClick(Sender: TObject);
begin
 Close;
end;

procedure TFrmOpen.FileListKeyAction(Sender: TCustomEasyListview;
  var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
 if CharCode = 13 then
 FileListClick;
end;

procedure TFrmOpen.PreviewTimerTimer(Sender: TObject);
begin
 Inc(PreviewFrame);

 if (PreviewFrame >= PreviewGMF.FrameCount) then
 PreviewFrame := 0;
 DrawPreview;
end;

procedure TFrmOpen.FormShow(Sender: TObject);
begin
 O := False;
 LastDir := '';
 BuildFileList;
 FileName := '';
 FileNameEdit.Text := '';
 FileNameEditChange(Nil);
end;

end.
