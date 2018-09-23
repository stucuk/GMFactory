object FrmResize: TFrmResize
  Left = 192
  Top = 124
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = ' Resize'
  ClientHeight = 225
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    440
    225)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 184
    Width = 425
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 265
    Height = 161
    BevelOuter = bvNone
    TabOrder = 0
    object Image1: TImage
      Left = 80
      Top = 24
      Width = 105
      Height = 105
    end
    object BottomValue: TSpinEdit
      Left = 96
      Top = 136
      Width = 73
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object LeftValue: TSpinEdit
      Left = 0
      Top = 64
      Width = 73
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
    end
    object RightValue: TSpinEdit
      Left = 192
      Top = 64
      Width = 73
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
    object TopValue: TSpinEdit
      Left = 96
      Top = 0
      Width = 73
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
  end
  object Button1: TButton
    Left = 291
    Top = 195
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 363
    Top = 195
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
end
