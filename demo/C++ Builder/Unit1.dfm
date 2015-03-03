object Form1: TForm1
  Left = 193
  Top = 117
  Caption = 'Demo'
  ClientHeight = 524
  ClientWidth = 684
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 684
    Height = 483
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 524
    object TabSheet1: TTabSheet
      Caption = 'Delphi'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 496
      object ListBox1: TListBox
        Left = 0
        Top = 0
        Width = 676
        Height = 455
        Align = alClient
        ItemHeight = 13
        PopupMenu = PopupMenu1
        TabOrder = 0
        OnDrawItem = ListBox1DrawItem
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Asm'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ListBox2: TListBox
        Left = 0
        Top = 0
        Width = 676
        Height = 455
        Align = alClient
        ItemHeight = 13
        PopupMenu = PopupMenu2
        TabOrder = 0
        OnDrawItem = ListBox2DrawItem
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Hex'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ListBox3: TListBox
        Left = 0
        Top = 0
        Width = 676
        Height = 455
        Align = alClient
        ItemHeight = 13
        PopupMenu = PopupMenu3
        TabOrder = 0
        OnDrawItem = ListBox3DrawItem
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 483
    Width = 684
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 192
    ExplicitTop = 224
    ExplicitWidth = 185
    object Button1: TButton
      Left = 16
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Settings'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 20
    Top = 24
    object Redraw1: TMenuItem
      Caption = 'Redraw'
      OnClick = Redraw1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 68
    Top = 24
    object Redraw2: TMenuItem
      Caption = 'Redraw'
      OnClick = Redraw2Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
  end
  object PopupMenu3: TPopupMenu
    Left = 116
    Top = 24
    object Redraw3: TMenuItem
      Caption = 'Redraw'
      OnClick = Redraw3Click
    end
    object N3: TMenuItem
      Caption = '-'
    end
  end
end
