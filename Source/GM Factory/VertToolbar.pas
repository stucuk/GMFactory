unit VertToolbar;

{
 Started 2017/02/16
 By Stuart "Stucuk" Carey
}

interface

uses ComCtrls, Controls, Classes;

type
TVerticalToolBar = Class(TToolBar)
protected
 procedure CreateParams(var Params: TCreateParams); override;
end;

procedure Register;

implementation

uses CommCtrl;

procedure Register;
begin
  RegisterComponents('Stucuk', [TVerticalToolBar]);
end;

procedure TVerticalToolBar.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or CCS_LEFT or CCS_NOPARENTALIGN or TBSTYLE_WRAPABLE;
end;

end.
