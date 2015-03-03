unit uHighlightSettings;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls, ImgList,
  uHighlight, uHighlightTools;

type
  //Редактор части подсветки
  THighlightPart = class(TPanel)
  protected
    cFontLabel   : TLabel;
    cFontPanel   : TPanel;
    cColorDlg    : TColorDialog;
    cFontDlg     : TFontDialog;
  private
    function  GetBgColor: TColor;
    function  GetFontStyle: TFont;
    function  GetCaption: string;
    procedure SetBgColor(const Value: TColor);
    procedure SetFontStyle(const Value: TFont);
    procedure cFontStyleMouseDown(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
    procedure SetCaption(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property FontStyle: TFont  read GetFontStyle write SetFontStyle;
    property BgColor  : TColor read GetBgColor   write SetBgColor;
    property Caption  : string read GetCaption   write SetCaption;
  end;

  //Редактор подсветки
  TThemeSettings     = class(TPanel)
  protected
    cSettingsPanel: TPanel;
      cNamePanel    : TPanel;
        cNameLabel    : TLabel;
        cNameEdit     : TEdit;
      cBgGlobalPanel: TPanel;
        cBgColorLabel : TLabel;
        cBgColorPanel : TPanel;
    cHighlightsBox: TScrollBox;
    cColorDlg     : TColorDialog;
    cHighlightPart : array of THighlightPart;
  private
    procedure   cBgColorClick(Sender: TObject);
    procedure cNameEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   RefreshCustomTheme;
  end;
  
  TfHighlightSettings = class(TForm)
    cPages            : TPageControl;
      cGeneral          : TTabSheet;
        cUseHighlightPanel: TPanel;
          cUseHighlight     : TCheckBox;
        cThemeSettingsPanel: TPanel;
          cGeneralLang      : TListBox;
          cGeneralPanel     : TPanel;
            cGlobalThemeGB: TGroupBox;
              cGlobalTheme      : TComboBox;
            cGeneralPreview   : TListBox;
      cHighlight        : TTabSheet;
        cHighlightLang    : TListBox;
        cHighlightSettingsPanel: TPanel;
          cHighlightThemeGB: TGroupBox;
            cThemeH           : TComboBox;
          cHighlightPreview : TListBox;
      cPopupMenu          : TPopupMenu;
        cAdd                : TMenuItem;
        cDelete             : TMenuItem;
      cImgList            : TImageList;
      cBottomPanel      : TPanel;
        cOk               : TButton;
        cCancel           : TButton;
        cApply            : TButton;
    cAutosave: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure cGeneralLangClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cGeneralPreviewDrawItem(Control: TWinControl; Index: Integer;
                                      Rect: TRect; State: TOwnerDrawState);
    procedure cGlobalThemeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cCancelClick(Sender: TObject);
    procedure cUseHighlightClick(Sender: TObject);
    procedure cApplyClick(Sender: TObject);
    procedure cOkClick(Sender: TObject);
    procedure cHighlightLangClick(Sender: TObject);
    procedure cHighlightPreviewDrawItem(Control: TWinControl;
                                        Index: Integer; Rect: TRect;
                                        State: TOwnerDrawState);
    procedure cThemeHClick(Sender: TObject);
    procedure cAddClick(Sender: TObject);
    procedure cDeleteClick(Sender: TObject);
    procedure cPagesChange(Sender: TObject);
    procedure cAutosaveClick(Sender: TObject);
  private
    BackUp              : TStringList;
    ThemeSettings       : TThemeSettings;
    cThemeHLastId       : Integer;
    CurrentLangId       : Integer;
    procedure SaveConfig();
    procedure General_LoadLang(Index: Integer);
    procedure Highlight_LoadLang(Index: Integer);
    procedure LoadThemes(LangId: DWORD; ComboBox: TComboBox);
    procedure LoadFromStream(ListBox: TListBox; Language: string);
    procedure UpdateCustomTheme(HOpt: THighlightPart);
  end;

  procedure ConfirmDlg();
  function  IdToThemeLang(Id: DWORD): TThemeLang;
  function  Max(a, b: integer): integer;

var
  fHighlightSettings: TfHighlightSettings;

implementation

{$R *.dfm}

const
  STR_ASM     = 'ASM';
  STR_HEX     = 'HEX';
  STR_DELPHI  = 'DELPHI';
var
  GeneralPreviewId   : TControlHighlight;
  HighlightPreviewId : TControlHighlight;

//-------------------------------------------------------------------------------------------
//GENERAL

procedure TfHighlightSettings.General_LoadLang(Index: Integer);
begin
  cGeneralPreview.Clear;
  case Index of
    0 :
      begin
        GeneralPreviewId.HighlightLanguage := ID_ASM;
        LoadFromStream(cGeneralPreview, STR_ASM);
        LoadThemes(ID_ASM, cGlobalTheme);
      end;
    1 :
      begin
        GeneralPreviewId.HighlightLanguage := ID_HEX;
        LoadFromStream(cGeneralPreview, STR_HEX);
        LoadThemes(ID_HEX, cGlobalTheme);
      end;
    2 :
      begin
        GeneralPreviewId.HighlightLanguage := ID_DELPHI;
        LoadFromStream(cGeneralPreview, STR_DELPHI);
        LoadThemes(ID_DELPHI, cGlobalTheme);
      end;
    else
      begin
        cGeneralPreview.Clear;
        cGlobalTheme.Clear;
      end;
  end;
end;

procedure TfHighlightSettings.cGeneralLangClick(Sender: TObject);
begin
  ConfirmDlg();
  General_LoadLang(cGeneralLang.ItemIndex);
end;

procedure TfHighlightSettings.cGlobalThemeClick(Sender: TObject);
begin
  case cGeneralLang.ItemIndex of
    0 :
      begin
        SetThemeId(tlAsm, DWORD(cGlobalTheme.ItemIndex), False);
      end;
    1 :
      begin
        SetThemeId(tlHex, DWORD(cGlobalTheme.ItemIndex), False);
      end;
    2 :
      begin
        SetThemeId(tlDelphi, DWORD(cGlobalTheme.ItemIndex), False);
      end;
  end;
  GeneralPreviewId.HighlightTheme := DWORD(cGlobalTheme.ItemIndex);
  cApply.Enabled := True;
end;

procedure TfHighlightSettings.cUseHighlightClick(Sender: TObject);
begin
  cThemeSettingsPanel.Visible := cUseHighlight.Checked;
  bNoHighlight := not cUseHighlight.Checked;
  cApply.Enabled := True;
end;

procedure TfHighlightSettings.cGeneralPreviewDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  GeneralPreviewId.OnDrawItem(Index, Rect, BOOL(Random(2)));
end;

//-------------------------------------------------------------------------------------------
//GLOBAL

procedure TfHighlightSettings.LoadThemes(LangId: DWORD; ComboBox: TComboBox);
var
  i, n : integer;
begin
  ComboBox.Clear;
  n := ThemesCount(IdToThemeLang(LangId));
  if n > 0 then
  begin
    for i := 0 to Pred(n) do
      ComboBox.Items.Add(GetThemeNameById(IdToThemeLang(LangId), DWORD(i)));
    ComboBox.ItemIndex := GetThemeId(IdToThemeLang(LangId));
    if ComboBox.Handle = cThemeH.Handle then
      cThemeHLastId      := ComboBox.ItemIndex;
  end;
end;

procedure TfHighlightSettings.FormCreate(Sender: TObject);
begin
  cImgList.GetInstRes(hInstance, rtBitmap, 'IMAGES', 13, [lrDefaultColor], clOlive);

  BackUp := TStringList.Create;
  GeneralPreviewId := TControlHighlight.Create;
  with GeneralPreviewId do
  begin
    ControlHandle     := cGeneralPreview.Handle;
    HighlightLanguage := ID_ASM;
    HighlightTheme    := ID_CFG_THEME;
    PreviewMode       := True;
  end;

  HighlightPreviewId := TControlHighlight.Create;
  with HighlightPreviewId do
  begin
    ControlHandle     := cHighlightPreview.Handle;
    HighlightLanguage := ID_ASM;
    HighlightTheme    := ID_CFG_THEME;
    PreviewMode       := True;
  end;
  ThemeSettings := TThemeSettings.Create(cHighlightSettingsPanel);
end;

procedure TfHighlightSettings.FormShow(Sender: TObject);
begin
  CurrentLangId := 0;
  
  BackUp.LoadFromFile(szConfigFile);
  cPages.ActivePage := cGeneral;

  cGeneralLang.ItemIndex := 0;
  General_LoadLang(cGeneralLang.ItemIndex);

  cHighlightLang.ItemIndex := 0;
  Highlight_LoadLang(cHighlightLang.ItemIndex);

  cUseHighlight.Checked  := StrToBoolDef(GetConfigString(CONFIG_NAME, CONFIG_USE, '1'), True);
  cThemeSettingsPanel.Visible := cUseHighlight.Checked;

  cAutosave.Checked  := StrToBoolDef(GetConfigString(CONFIG_NAME, CONFIG_AUTOSAVE, '0'), True);

  cApply.Enabled := False;
end;

procedure TfHighlightSettings.FormDestroy(Sender: TObject);
begin
  GeneralPreviewId.Free;
  HighlightPreviewId.Free;
  BackUp.Free;
  ThemeSettings.Free;
end;

procedure TfHighlightSettings.cCancelClick(Sender: TObject);
begin
  BackUp.SaveToFile(szConfigFile);
  Close;
end;

procedure TfHighlightSettings.LoadFromStream(ListBox: TListBox; Language: string);
var
  RS: TResourceStream;
begin
  RS := TResourceStream.Create(hInstance, Language, 'EXAMPLE');
  try
    ListBox.Clear;
    ListBox.Items.LoadFromStream(RS);
  finally
    RS.Free;
  end;
end;

function IdToThemeLang(Id: DWORD): TThemeLang;
begin
  case Id of
    ID_ASM    : Result := tlAsm;
    ID_HEX    : Result := tlHex;
    ID_DELPHI : Result := tlDelphi;
    else
      Result := tlUnknown;
  end;
end;

function Max(a, b: integer): integer;
begin
  if a > b then Result := a else Result := b;
end;

procedure TfHighlightSettings.SaveConfig();
begin
  case CurrentLangId of
    0: SaveTheme(tlAsm,    cThemeHLastId, HighlightPreviewId.cHighlightTheme);
    1: SaveTheme(tlHex,    cThemeHLastId, HighlightPreviewId.cHighlightTheme);
    2: SaveTheme(tlDelphi, cThemeHLastId, HighlightPreviewId.cHighlightTheme);
  end;
  case cGeneralLang.ItemIndex of
    0 :
      begin
        SetThemeId(tlAsm, DWORD(cGlobalTheme.ItemIndex), True);
      end;
    1 :
      begin
        SetThemeId(tlHex, DWORD(cGlobalTheme.ItemIndex), True);
      end;
    2 :
      begin
        SetThemeId(tlDelphi, DWORD(cGlobalTheme.ItemIndex), True);
      end;
  end;
  cApply.Enabled := False;
  SetConfigString(CONFIG_NAME, CONFIG_USE, IntToStr(Integer(cUseHighlight.Checked)));
  BackUp.LoadFromFile(szConfigFile);
end;

procedure TfHighlightSettings.cApplyClick(Sender: TObject);
begin
  SaveConfig();
end;

procedure TfHighlightSettings.cOkClick(Sender: TObject);
begin
  SaveConfig();
  Close;
end;

procedure ConfirmDlg();
const
  STR_CONFIRM_CAPTION = 'Confirm';
  STR_CONFIRM_TEXT    = 'Settings have been modified. Save changes?';
begin
  if (fHighlightSettings.cApply.Enabled)and(fHighlightSettings.CurrentLangId >= 0) then
  begin
    if fHighlightSettings.cAutosave.Checked then
    begin
      fHighlightSettings.SaveConfig();
    end
    else
      if MessageBoxA(fHighlightSettings.Handle, STR_CONFIRM_TEXT,
                     STR_CONFIRM_CAPTION, MB_ICONQUESTION or MB_YESNO) = idYes then
        fHighlightSettings.SaveConfig();
  end;
end;

//-------------------------------------------------------------------------------------------
//HIGHLIGHT

procedure TfHighlightSettings.Highlight_LoadLang(Index: Integer);
begin
  cHighlightPreview.Clear;
  case Index of
    0 :
      begin
        HighlightPreviewId.HighlightLanguage := ID_ASM;
        LoadFromStream(cHighlightPreview, STR_ASM);
        LoadThemes(ID_ASM, cThemeH);
        ThemeSettings.RefreshCustomTheme;
      end;
    1 :
      begin
        HighlightPreviewId.HighlightLanguage := ID_HEX;
        LoadFromStream(cHighlightPreview, STR_HEX);
        LoadThemes(ID_HEX, cThemeH);
        ThemeSettings.RefreshCustomTheme;
      end;
    2 :
      begin
        HighlightPreviewId.HighlightLanguage := ID_DELPHI;
        LoadFromStream(cHighlightPreview, STR_DELPHI);
        LoadThemes(ID_DELPHI, cThemeH);
        ThemeSettings.RefreshCustomTheme;
      end;
    else
      begin
        cHighlightPreview.Clear;
        cThemeH.Clear;
      end;
  end;
end;

procedure TfHighlightSettings.cHighlightLangClick(Sender: TObject);
begin
  ConfirmDlg();
  Highlight_LoadLang(cHighlightLang.ItemIndex);
  CurrentLangId := cHighlightLang.ItemIndex;
end;

procedure TfHighlightSettings.cHighlightPreviewDrawItem(
  Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  HighlightPreviewId.OnDrawItem(Index, Rect, BOOL(Random(2)));
end;

procedure TfHighlightSettings.cThemeHClick(Sender: TObject);
begin
  ConfirmDlg();
  case cHighlightLang.ItemIndex of
    0 :
      begin
        HighlightPreviewId.HighlightTheme := DWORD(cThemeH.ItemIndex);
      end;
    1 :
      begin
        HighlightPreviewId.HighlightTheme := DWORD(cThemeH.ItemIndex);
      end;
    2 :
      begin
        HighlightPreviewId.HighlightTheme := DWORD(cThemeH.ItemIndex);
      end;
  end;
  cThemeHLastId := cThemeH.ItemIndex;
  ThemeSettings.RefreshCustomTheme;
end;

//Загрузка значений с формы
procedure TfHighlightSettings.UpdateCustomTheme(HOpt: THighlightPart);
var
  i : integer;
begin
  for i := Low(ThemeSettings.cHighlightPart) to High(ThemeSettings.cHighlightPart) do
  begin
    if ThemeSettings.cHighlightPart[i] = HOpt then
    begin
      if cHighlightLang.ItemIndex = 1 {HEX} then
      with HighlightPreviewId.cHighlightTheme do
      begin
        case i of
          0 :
            begin
              Default.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              Default.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
          1 :
            begin
              HEX.Addresses.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              HEX.Addresses.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
          2 :
            begin
              HEX.Bytes.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              HEX.Bytes.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
          3 :
            begin
              HEX.Chars.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              HEX.Chars.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
        end;
      end
      else
      with HighlightPreviewId.cHighlightTheme do
      begin
        case i of
          0 :
            begin
              Default.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              Default.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
          1 :
            begin
              Lighten.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              Lighten.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
          2 :
            begin
              Comments.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              Comments.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
          3 :
            begin
              Numbers.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              Numbers.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
          4 :
            begin
              Strings.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              Strings.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
          5 :
            begin
              Additional.Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              Additional.BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
          else
            begin
              KeyWords[i-6].Font.Assign(ThemeSettings.cHighlightPart[i].FontStyle);
              KeyWords[i-6].BgColor := ThemeSettings.cHighlightPart[i].BgColor;
            end;
        end;
      end;
      HighlightPreviewId.Redraw;
    end;
  end;
end;

//Добавление темы
procedure TfHighlightSettings.cAddClick(Sender: TObject);
var
  ThemeLang : TThemeLang;
begin
  case cHighlightLang.ItemIndex of
    0: ThemeLang := tlAsm;
    1: ThemeLang := tlHex;
    2: ThemeLang := tlDelphi;
    else
      ThemeLang := tlUnknown;
  end;
  cThemeH.Items.Add('New Theme');
  cThemeH.ItemIndex := cThemeH.Items.Count - 1;
  LoadTheme(ThemeLang,
            ThemesCount(ThemeLang),
            True,
            @HighlightPreviewId.cHighlightTheme,
            True);
  cThemeH.OnClick(Self);
  cApply.Enabled := True;
end;

//Удаление темы
procedure TfHighlightSettings.cDeleteClick(Sender: TObject);
var
  ThemeLang : TThemeLang;
begin
  case cHighlightLang.ItemIndex of
    0: ThemeLang := tlAsm;
    1: ThemeLang := tlHex;
    2: ThemeLang := tlDelphi;
    else
      ThemeLang := tlUnknown;
  end;
  DeleteTheme(ThemeLang, cThemeH.ItemIndex);
  cGeneralLang.ItemIndex := 0;
  General_LoadLang(cGeneralLang.ItemIndex);

  cHighlightLang.ItemIndex := 0;
  Highlight_LoadLang(cHighlightLang.ItemIndex);

  cApply.Enabled := True;
end;

//-------------------------------------------------------------------------------------------
//THighlightPart

constructor THighlightPart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelInner := bvLowered;
  BevelOuter := bvRaised;
  Align      := alTop;
  Parent     := (AOwner as TWinControl);
  Width      := (AOwner as TWinControl).ClientWidth;
  Height     := 41;
  cFontLabel := TLabel.Create(Self);
  with cFontLabel do
  begin
    AutoSize := True;
    Left    := 8;
    Top     := 18;
    Caption := 'Font';
    Parent  := Self;
  end;
  cFontPanel   := TPanel.Create(Self);
  with cFontPanel do
  begin
    Left    := 80;
    Top     := 8;
    Width   := 320;
    Height  := 25;
    Caption := 'Font Style';
    Parent  := Self;
    Color   := clWhite;
    BevelInner := bvRaised;
    BevelOuter := bvSpace;
    OnMouseDown:= cFontStyleMouseDown;
    Hint       := 'Left - Font; Left+Ctrl - Extnded Font Color; Right - BgColor';
  end;
  cColorDlg := TColorDialog.Create(Self);
  cFontDlg  := TFontDialog.Create(Self);
end;

destructor THighlightPart.Destroy;
begin
  cFontLabel.Free;
  cFontPanel.Free;
  cColorDlg.Free;
  cFontDlg.Free;
  inherited Destroy;
end;

function THighlightPart.GetBgColor: TColor;
begin
  Result := cFontPanel.Color;
end;

function THighlightPart.GetFontStyle: TFont;
begin
  Result := cFontPanel.Font;
end;

procedure THighlightPart.SetBgColor(const Value: TColor);
begin
  cFontPanel.Color := Value;
end;

procedure THighlightPart.SetFontStyle(const Value: TFont);
begin
  cFontPanel.Font.Assign(Value);
end;

procedure THighlightPart.cFontStyleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  function CtrlDown(): Boolean;
  var
    State : TKeyboardState;
  begin
    GetKeyboardState(State);
    Result := ((State[VK_CONTROL] and 128) <> 0);
  end;

begin
  case Button of
    mbLeft :
      begin
        if not CtrlDown() then
        begin
          cFontDlg.Font := cFontPanel.Font;
          if cFontDlg.Execute then
          begin
            cFontPanel.Font.Assign(cFontDlg.Font);
            fHighlightSettings.UpdateCustomTheme(Self);
            fHighlightSettings.cApply.Enabled := True;
          end;
        end
        else
        begin
          cColorDlg.Color := cFontPanel.Color;
          if cColorDlg.Execute then
          begin
            cFontPanel.Font.Color := cColorDlg.Color;
            fHighlightSettings.UpdateCustomTheme(Self);
            fHighlightSettings.cApply.Enabled := True;
          end;
        end;
      end;
    mbRight:
      begin
        cColorDlg.Color := cFontPanel.Color;
        if cColorDlg.Execute then
        begin
          cFontPanel.Color := cColorDlg.Color;
          fHighlightSettings.UpdateCustomTheme(Self);
          fHighlightSettings.cApply.Enabled := True;
        end;
      end;
  end;
end;

function THighlightPart.GetCaption: string;
begin
  Result := cFontLabel.Caption;
end;

procedure THighlightPart.SetCaption(const Value: string);
begin
  cFontLabel.Caption := Value;
end;

//-------------------------------------------------------------------------------------------
//TThemeSettings

procedure TThemeSettings.cBgColorClick(Sender: TObject);
begin
  cColorDlg.Color := cBgColorPanel.Color;
  if cColorDlg.Execute then
  begin
    cBgColorPanel.Color := cColorDlg.Color;
    HighlightPreviewId.cHighlightTheme.BgColor := cColorDlg.Color;
    HighlightPreviewId.Redraw;
    fHighlightSettings.cApply.Enabled := True;
  end;
end;

procedure TThemeSettings.cNameEditKeyUp(Sender: TObject; var Key: Word;
                                    Shift: TShiftState);
begin
  HighlightPreviewId.cHighlightTheme.Name := cNameEdit.Text;
  fHighlightSettings.cApply.Enabled := True;
end;

constructor TThemeSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Align      := alClient;
  Parent     := (AOwner as TWinControl);
  Width      := 208;
  Height     := 100;
  cSettingsPanel := TPanel.Create(Self);
  with cSettingsPanel do
  begin
    Align   := alLeft;
    Left    := 0;
    Top     := 0;
    Width   := 208;
    Height  := 41;
    Caption := '';
    Parent  := Self;
    BevelInner := bvNone;
    BevelOuter := bvNone;
  end;
  cNamePanel := TPanel.Create(cSettingsPanel);
  with cNamePanel do
  begin
    BevelInner := bvLowered;
    BevelOuter := bvRaised;
    Align      := alTop;
    Parent     := cSettingsPanel;
    Width      := 208;
    Height     := 41;
  end;
  cNameLabel := TLabel.Create(cNamePanel);
  with cNameLabel do
  begin
    AutoSize := True;
    Left     := 8;
    Top      := 18;
    Caption  := 'Name';
    Parent   := cNamePanel;
  end;
  cNameEdit   := TEdit.Create(cNamePanel);
  with cNameEdit do
  begin
    Left       := 80;
    Top        := 11;
    Width      := 120;
    Height     := 25;
    Text       := '';
    Parent     := cNamePanel;
    BevelInner := bvRaised;
    BevelOuter := bvSpace;
    BevelKind  := bkFlat;
    OnKeyUp    := cNameEditKeyUp;
  end;
  cBgGlobalPanel := TPanel.Create(cSettingsPanel);
  with cBgGlobalPanel do
  begin
    BevelInner := bvLowered;
    BevelOuter := bvRaised;
    Align      := alTop;
    Parent     := cSettingsPanel;
    Width      := 208;
    Height     := 41;
  end;
  cBgColorLabel := TLabel.Create(cBgGlobalPanel);
  with cBgColorLabel do
  begin
    AutoSize := True;
    Left     := 8;
    Top      := 18;
    Caption  := 'BgColor';
    Parent   := cBgGlobalPanel;
  end;
  cBgColorPanel   := TPanel.Create(cBgGlobalPanel);
  with cBgColorPanel do
  begin
    Left       := 80;
    Top        := 8;
    Width      := 120;
    Height     := 25;
    Caption    := '';
    Parent     := cBgGlobalPanel;
    Color      := clWhite;
    BevelInner := bvRaised;
    BevelOuter := bvSpace;
    OnClick := cBgColorClick;
  end;
  cColorDlg      := TColorDialog.Create(Self);
  cHighlightsBox := TScrollBox.Create(Self);
  with cHighlightsBox do
  begin
    Align               := alClient;
    Left                := 0;
    Top                 := 0;
    Width               := 208;
    Height              := 41;
    Caption             := '';
    Parent              := Self;
    BevelInner          := bvNone;
    BevelOuter          := bvNone;
    HorzScrollBar.Style := ssFlat;
    VertScrollBar.Style := ssFlat;
  end;
end;

destructor TThemeSettings.Destroy;
begin
  cNameLabel.Free;
  cNameEdit.Free;
  cNamePanel.Free;
  cBgColorLabel.Free;
  cBgColorPanel.Free;
  cBgGlobalPanel.Free;
  cHighlightsBox.Free;
  cColorDlg.Free;
  cSettingsPanel.Free;
  Finalize(cHighlightPart);
  inherited Destroy;
end;

procedure TThemeSettings.RefreshCustomTheme;
var
  i, j : integer;
begin
  cHighlightsBox.Visible := False;
  for i := Low(cHighlightPart) to High(cHighlightPart) do
  begin
    cHighlightPart[i].Free;
  end;
  case fHighlightSettings.cHighlightLang.ItemIndex of
    0: //tlAsm
      begin
        SetLength(cHighlightPart, 6 + Length(AsmLang.KeyWords));
      end;
    1: //tlHex
      begin
        SetLength(cHighlightPart, 1 + 3);
      end;
    2: //tlDelphi
      begin
        SetLength(cHighlightPart, 6 + Length(DelphiLang.KeyWords));
      end;
    else
      Exit;
  end;
  cNameEdit.Text      := HighlightPreviewId.cHighlightTheme.Name;
  cBgColorPanel.Color := HighlightPreviewId.cHighlightTheme.BgColor;
  if fHighlightSettings.cHighlightLang.ItemIndex = 1{HEX} then
  begin
    for i := Low(cHighlightPart) to High(cHighlightPart) do
    begin
      cHighlightPart[i] := THighlightPart.Create(cHighlightsBox);
      cHighlightPart[i].Top := (i + 1) * cHighlightPart[i].Height;
      case i of
        0 :
          begin
            cHighlightPart[i].Caption   := THEME_DEFAULT;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.Default.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.Default.BgColor;
          end;
        1 :
          begin
            cHighlightPart[i].Caption   := HEX_ADDRESSES;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.HEX.Addresses.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.HEX.Addresses.BgColor;
          end;
        2 :
          begin
            cHighlightPart[i].Caption   := HEX_BYTES;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.HEX.Bytes.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.HEX.Bytes.BgColor;
          end;
        3 :
          begin
            cHighlightPart[i].Caption   := HEX_CHARS;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.HEX.Chars.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.HEX.Chars.BgColor;
          end;
      end;
    end;
  end
  else
  begin
    j := 0;
    for i := Low(cHighlightPart) to High(cHighlightPart) do
    begin
      cHighlightPart[i] := THighlightPart.Create(cHighlightsBox);
      cHighlightPart[i].Top := (i + 1) * cHighlightPart[i].Height;
      case i of
        0 :
          begin
            cHighlightPart[i].Caption   := THEME_DEFAULT;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.Default.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.Default.BgColor;
          end;
        1 :
          begin
            cHighlightPart[i].Caption   := THEME_LIGHTEN;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.Lighten.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.Lighten.BgColor;
          end;
        2 :
          begin
            cHighlightPart[i].Caption   := THEME_COMMENTS;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.Comments.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.Comments.BgColor;
          end;
        3 :
          begin
            cHighlightPart[i].Caption   := THEME_NUMBERS;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.Numbers.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.Numbers.BgColor;
          end;
        4 :
          begin
            cHighlightPart[i].Caption   := THEME_STRINGS;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.Strings.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.Strings.BgColor;
          end;
        5 :
          begin
            cHighlightPart[i].Caption   := THEME_ADDITIONAL;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.Additional.Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.Additional.BgColor;
          end;
        else begin
            cHighlightPart[i].Caption   := THEME_KEYWORDS + IntToStr(j);
            if (not Assigned(cHighlightPart[i].FontStyle)) or (cHighlightPart[i].FontStyle = nil) then
              cHighlightPart[i].FontStyle := TFont.Create;
            cHighlightPart[i].FontStyle.Assign(HighlightPreviewId.cHighlightTheme.KeyWords[j].Font);
            cHighlightPart[i].BgColor   := HighlightPreviewId.cHighlightTheme.KeyWords[j].BgColor;
            Inc(j);
          end;
      end;
    end;
  end;
  cHighlightsBox.Visible := True;
  HighlightPreviewId.Redraw;
end;

procedure TfHighlightSettings.cPagesChange(Sender: TObject);
begin
  ConfirmDlg();
end;

procedure TfHighlightSettings.cAutosaveClick(Sender: TObject);
begin
  SetConfigString(CONFIG_NAME, CONFIG_AUTOSAVE, IntToStr(Integer(cAutosave.Checked)));
end;

initialization
  Randomize;
end.
