object FrmNew: TFrmNew
  Left = 192
  Top = 124
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = ' New'
  ClientHeight = 149
  ClientWidth = 164
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    164
    149)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 16
    Width = 41
    Height = 21
    Alignment = taCenter
    AutoSize = False
    Caption = 'Width'
    Layout = tlCenter
  end
  object Label2: TLabel
    Left = 32
    Top = 40
    Width = 41
    Height = 21
    Alignment = taCenter
    AutoSize = False
    Caption = 'Height'
    Layout = tlCenter
  end
  object Bevel1: TBevel
    Left = 8
    Top = 109
    Width = 145
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object Label3: TLabel
    Left = 32
    Top = 72
    Width = 41
    Height = 21
    Alignment = taCenter
    AutoSize = False
    Caption = 'Frames'
    Layout = tlCenter
  end
  object WidthEdit: TEdit
    Left = 80
    Top = 16
    Width = 41
    Height = 21
    BevelKind = bkFlat
    BorderStyle = bsNone
    MaxLength = 4
    TabOrder = 0
    Text = '100'
    OnChange = HeightEditChange
  end
  object HeightEdit: TEdit
    Left = 80
    Top = 40
    Width = 41
    Height = 21
    BevelKind = bkFlat
    BorderStyle = bsNone
    MaxLength = 4
    TabOrder = 1
    Text = '100'
    OnChange = HeightEditChange
  end
  object Button2: TButton
    Left = 8
    Top = 117
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 80
    Top = 117
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button1Click
  end
  object FrameEdit: TEdit
    Left = 80
    Top = 72
    Width = 41
    Height = 21
    BevelKind = bkFlat
    BorderStyle = bsNone
    MaxLength = 4
    TabOrder = 4
    Text = '8'
    OnChange = HeightEditChange
  end
end
