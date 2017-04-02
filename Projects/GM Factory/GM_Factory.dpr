program GM_Factory;

uses
  Forms,
  FormMain in 'FormMain.pas' {FrmMain},
  GMF in '..\..\Source\GM Factory\GMF.pas',
  GM_File in '..\..\Source\GM Factory\GM_File.pas',
  OW_Palette in '..\..\Source\GM Factory\OW_Palette.pas',
  CheckerUnit in '..\..\Source\GM Factory\CheckerUnit.pas',
  pngimage in '..\..\Source\GM Factory\pngdelphi-master\PNGImage.pas',
  VertToolbar in '..\..\Source\GM Factory\VertToolbar.pas',
  HSLUnit in '..\..\Source\GM Factory\HSLUnit.pas',
  RGBHelper in '..\..\Source\GM Factory\RGBHelper.pas',
  FormOpen in 'FormOpen.pas' {FrmOpen},
  GMLoader in '..\..\Source\GM Factory\GMLoader.pas',
  FormNew in 'FormNew.pas' {FrmNew},
  FormSaveOptions in 'FormSaveOptions.pas' {FrmSaveOptions},
  FormAbout in 'FormAbout.pas' {FrmAbout},
  UndoSystem in '..\..\Source\GM Factory\UndoSystem.pas',
  EdUnit in '..\..\Source\GM Factory\EdUnit.pas',
  EdLayers in '..\..\Source\GM Factory\EdLayers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TFrmOpen, FrmOpen);
  Application.CreateForm(TFrmNew, FrmNew);
  Application.CreateForm(TFrmSaveOptions, FrmSaveOptions);
  Application.CreateForm(TFrmAbout, FrmAbout);
  Application.Run;
end.
