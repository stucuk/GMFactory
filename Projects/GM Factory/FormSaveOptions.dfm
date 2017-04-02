object FrmSaveOptions: TFrmSaveOptions
  Left = 320
  Top = 269
  BorderStyle = bsToolWindow
  Caption = ' Save Options'
  ClientHeight = 298
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 393
    Height = 17
    AutoSize = False
    Caption = 'Name: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
  object Bevel1: TBevel
    Left = 8
    Top = 256
    Width = 393
    Height = 10
    Shape = bsTopLine
  end
  object PreviewInnerPanel: TPanel
    Left = 8
    Top = 32
    Width = 194
    Height = 194
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 0
    object PreviewBack: TImage
      Left = 1
      Top = 1
      Width = 192
      Height = 192
      Align = alClient
    end
    object PreviewImage: TImage
      Left = 1
      Top = 1
      Width = 192
      Height = 192
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
      Transparent = True
    end
  end
  object Panel1: TPanel
    Left = 208
    Top = 32
    Width = 194
    Height = 194
    BevelInner = bvLowered
    BevelOuter = bvNone
    TabOrder = 1
    object PaletteImg: TImage
      Left = 1
      Top = 1
      Width = 192
      Height = 192
      Align = alClient
    end
  end
  object HasSideCols: TCheckBox
    Left = 208
    Top = 232
    Width = 193
    Height = 17
    Caption = 'Has Side Colours'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = HasSideColsClick
  end
  object Button2: TButton
    Left = 255
    Top = 264
    Width = 74
    Height = 25
    Caption = 'Ok'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button1: TButton
    Left = 328
    Top = 264
    Width = 73
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Timer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerTimer
    Left = 144
    Top = 224
  end
end
