object FrmChangeOffset: TFrmChangeOffset
  Left = 742
  Top = 125
  Width = 617
  Height = 291
  BorderIcons = [biSystemMenu]
  Caption = ' '
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    601
    253)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 185
    Height = 185
    Center = True
  end
  object Bevel1: TBevel
    Left = 8
    Top = 207
    Width = 585
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsBottomLine
  end
  object Image2: TImage
    Left = 408
    Top = 8
    Width = 185
    Height = 185
    Center = True
  end
  object Bevel2: TBevel
    Left = 408
    Top = 8
    Width = 185
    Height = 185
  end
  object Bevel3: TBevel
    Left = 8
    Top = 8
    Width = 185
    Height = 185
  end
  object Label1: TLabel
    Left = 240
    Top = 56
    Width = 50
    Height = 13
    Caption = '+- X Offset'
  end
  object Label2: TLabel
    Left = 240
    Top = 96
    Width = 50
    Height = 13
    Caption = '+- Y Offset'
  end
  object Button1: TButton
    Left = 450
    Top = 222
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 522
    Top = 222
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button2Click
  end
  object XOff: TEdit
    Left = 240
    Top = 72
    Width = 121
    Height = 21
    BevelKind = bkFlat
    BorderStyle = bsNone
    TabOrder = 2
    OnChange = XOffChange
    OnKeyPress = XOffKeyPress
  end
  object YOff: TEdit
    Left = 240
    Top = 112
    Width = 121
    Height = 21
    BevelKind = bkFlat
    BorderStyle = bsNone
    TabOrder = 3
    OnChange = YOffChange
    OnKeyPress = XOffKeyPress
  end
  object Timer: TTimer
    Enabled = False
    Interval = 250
    OnTimer = TimerTimer
    Left = 240
    Top = 128
  end
end
