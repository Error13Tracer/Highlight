object Form1: TForm1
  Left = 193
  Top = 117
  Width = 692
  Height = 555
  Caption = 'Demo'
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
    Width = 676
    Height = 475
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Delphi'
      object ListBox1: TListBox
        Left = 0
        Top = 0
        Width = 668
        Height = 447
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
      object ListBox2: TListBox
        Left = 0
        Top = 0
        Width = 668
        Height = 447
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
      object ListBox3: TListBox
        Left = 0
        Top = 0
        Width = 668
        Height = 447
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
    Top = 475
    Width = 676
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Settings'
      Default = True
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
