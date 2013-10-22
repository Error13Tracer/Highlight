object fHighlightSettings: TfHighlightSettings
  Left = 192
  Top = 116
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Highlight Settings'
  ClientHeight = 427
  ClientWidth = 704
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object cBottomPanel: TPanel
    Left = 0
    Top = 387
    Width = 704
    Height = 40
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 0
    object cOk: TButton
      Left = 440
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = cOkClick
    end
    object cCancel: TButton
      Left = 616
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = cCancelClick
    end
    object cApply: TButton
      Left = 528
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 2
      OnClick = cApplyClick
    end
    object cAutosave: TCheckBox
      Left = 8
      Top = 16
      Width = 185
      Height = 17
      Caption = 'Autosave without confirmation'
      TabOrder = 3
      OnClick = cAutosaveClick
    end
  end
  object cPages: TPageControl
    Left = 0
    Top = 0
    Width = 704
    Height = 387
    ActivePage = cGeneral
    Align = alClient
    TabOrder = 1
    OnChange = cPagesChange
    object cGeneral: TTabSheet
      Caption = 'General'
      object cUseHighlightPanel: TPanel
        Left = 0
        Top = 0
        Width = 696
        Height = 33
        Align = alTop
        BevelInner = bvLowered
        TabOrder = 0
        object cUseHighlight: TCheckBox
          Left = 8
          Top = 8
          Width = 217
          Height = 17
          Caption = 'Use Highlight (you must restart program)'
          TabOrder = 0
          OnClick = cUseHighlightClick
        end
      end
      object cThemeSettingsPanel: TPanel
        Left = 0
        Top = 33
        Width = 696
        Height = 326
        Align = alClient
        TabOrder = 1
        object cGeneralPanel: TPanel
          Left = 58
          Top = 1
          Width = 637
          Height = 324
          Align = alClient
          Caption = 'cGeneralPanel'
          TabOrder = 0
          object cGeneralPreview: TListBox
            Left = 1
            Top = 48
            Width = 635
            Height = 275
            Style = lbOwnerDrawFixed
            Align = alClient
            BevelInner = bvNone
            BevelKind = bkFlat
            BevelOuter = bvNone
            ItemHeight = 16
            TabOrder = 0
            OnDrawItem = cGeneralPreviewDrawItem
          end
          object cGlobalThemeGB: TGroupBox
            Left = 1
            Top = 1
            Width = 635
            Height = 47
            Align = alTop
            Caption = 'Theme'
            TabOrder = 1
            object cGlobalTheme: TComboBox
              Left = 8
              Top = 16
              Width = 617
              Height = 21
              BevelInner = bvLowered
              BevelKind = bkFlat
              BevelOuter = bvRaised
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 0
              OnClick = cGlobalThemeClick
            end
          end
        end
        object cGeneralLang: TListBox
          Left = 1
          Top = 1
          Width = 57
          Height = 324
          Align = alLeft
          BevelInner = bvNone
          BevelKind = bkFlat
          BevelOuter = bvNone
          ItemHeight = 13
          Items.Strings = (
            'Asm'
            'HEX'
            'Delphi')
          TabOrder = 1
          OnClick = cGeneralLangClick
        end
      end
    end
    object cHighlight: TTabSheet
      Caption = 'Highlight'
      ImageIndex = 1
      object cHighlightSettingsPanel: TPanel
        Left = 57
        Top = 0
        Width = 639
        Height = 359
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object cHighlightThemeGB: TGroupBox
          Left = 0
          Top = 0
          Width = 639
          Height = 47
          Align = alTop
          Caption = 'Theme'
          TabOrder = 0
          object cThemeH: TComboBox
            Left = 8
            Top = 16
            Width = 617
            Height = 21
            BevelKind = bkFlat
            Style = csDropDownList
            ItemHeight = 0
            PopupMenu = cPopupMenu
            TabOrder = 0
            OnClick = cThemeHClick
          end
        end
        object cHighlightPreview: TListBox
          Left = 0
          Top = 225
          Width = 639
          Height = 134
          Style = lbOwnerDrawFixed
          Align = alBottom
          BevelInner = bvNone
          BevelKind = bkFlat
          BevelOuter = bvNone
          ItemHeight = 16
          TabOrder = 1
          OnDrawItem = cHighlightPreviewDrawItem
        end
      end
      object cHighlightLang: TListBox
        Left = 0
        Top = 0
        Width = 57
        Height = 359
        Align = alLeft
        BevelInner = bvNone
        BevelKind = bkFlat
        BevelOuter = bvNone
        ItemHeight = 13
        Items.Strings = (
          'Asm'
          'HEX'
          'Delphi')
        TabOrder = 1
        OnClick = cHighlightLangClick
      end
    end
  end
  object cPopupMenu: TPopupMenu
    Images = cImgList
    Left = 69
    Top = 56
    object cAdd: TMenuItem
      Caption = 'Add'
      ImageIndex = 0
      OnClick = cAddClick
    end
    object cDelete: TMenuItem
      Caption = 'Delete'
      ImageIndex = 1
      OnClick = cDeleteClick
    end
  end
  object cImgList: TImageList
    Height = 13
    Width = 13
    Left = 109
    Top = 48
  end
end
