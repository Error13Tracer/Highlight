unit Unit1;

interface

uses
  Windows, Classes, Controls, Forms,
  Highlight, Menus, StdCtrls, ComCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    Redraw1: TMenuItem;
    Redraw2: TMenuItem;
    Redraw3: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Panel1: TPanel;
    Button1: TButton;
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBox2DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBox3DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Redraw1Click(Sender: TObject);
    procedure Redraw2Click(Sender: TObject);
    procedure Redraw3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure OnClickM1(Sender: TObject);
    procedure OnClickM2(Sender: TObject);
    procedure OnClickM3(Sender: TObject);
    procedure LoadFromStream(ListBox: TListBox; Language: string);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  AsmLbId   : DWORD;
  HexLbId   : DWORD;
  DelphiLbId: DWORD;
  i         : integer;
  m1,m2,m3  : array of TMenuItem;

procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  HighlightDrawItem(DelphiLbId, Index, Rect);
end;

procedure TForm1.ListBox2DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  HighlightDrawItem(AsmLbId, Index, Rect);
end;

procedure TForm1.ListBox3DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  HighlightDrawItem(HexLbId, Index, Rect);
end;

procedure TForm1.LoadFromStream(ListBox: TListBox; Language: string);
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

procedure TForm1.FormCreate(Sender: TObject);
var
  buf : array [0..4095] of AnsiChar;
begin
  if not InitHighlight then
  begin
    MessageBoxA(0, PAnsiChar(HighlightDll + ' not found'), 'Error', MB_ICONERROR);
    ExitProcess(0);
  end;
  ListBox1.Style := lbOwnerDrawFixed;
  ListBox2.Style := lbOwnerDrawFixed;
  ListBox3.Style := lbOwnerDrawFixed;
  DelphiLbId := CreateHighlight(ListBox1.Handle, ID_DELPHI);
  AsmLbId := CreateHighlight(ListBox2.Handle, ID_ASM);
  HexLbId := CreateHighlight(ListBox3.Handle, ID_HEX);
  LoadFromStream(ListBox1, 'DELPHI');
  LoadFromStream(ListBox2, 'ASM');
  LoadFromStream(ListBox3, 'HEX');
  //Для установки тем из меню
  for i := 0 to GetThemesCount(ID_DELPHI) - 1 do
  begin
    SetLength(m1, Length(m1) + 1);
    m1[i] := TMenuItem.Create(Self);
    GetThemeName(ID_DELPHI, DWORD(i), buf, Length(buf));
    m1[i].Caption := string(buf);
    m1[i].OnClick := OnClickM1;
    PopupMenu1.Items.Add(m1[i]);
  end;
  for i := 0 to GetThemesCount(ID_ASM) - 1 do
  begin
    SetLength(m2, Length(m2) + 1);
    m2[i] := TMenuItem.Create(Self);
    GetThemeName(ID_ASM, DWORD(i), buf, Length(buf));
    m2[i].Caption := string(buf);
    m2[i].OnClick := OnClickM2;
    PopupMenu2.Items.Add(m2[i]);
  end;
  for i := 0 to GetThemesCount(ID_HEX) - 1 do
  begin
    SetLength(m3, Length(m3) + 1);
    m3[i] := TMenuItem.Create(Self);
    GetThemeName(ID_HEX, DWORD(i), buf, Length(buf));
    m3[i].Caption := string(buf);
    m3[i].OnClick := OnClickM3;
    PopupMenu3.Items.Add(m3[i]);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  for i := Low(m1) to High(m1) do
    m1[i].Free;
  for i := Low(m2) to High(m2) do
    m2[i].Free;
  for i := Low(m3) to High(m3) do
    m3[i].Free;
  Finalize(m1);
  Finalize(m2);
  Finalize(m3);
  FreeHighlight;
end;

procedure TForm1.OnClickM1(Sender: TObject);
begin
  for i := Low(m1) to High(m1) do
    if (Sender as TMenuItem) = m1[i] then
    begin
      //ChangeGlobalTheme(ID_DELPHI, i);
      ChangeTheme(DelphiLbId, i);
    end;
end;

procedure TForm1.OnClickM2(Sender: TObject);
begin
  for i := Low(m2) to High(m2) do
    if (Sender as TMenuItem) = m2[i] then
    begin
      //ChangeGlobalTheme(ID_ASM, i);
      ChangeTheme(AsmLbId, i);
    end;
end;

procedure TForm1.OnClickM3(Sender: TObject);
begin
  for i := Low(m3) to High(m3) do
    if (Sender as TMenuItem) = m3[i] then
    begin
      //ChangeGlobalTheme(ID_HEX, i);
      ChangeTheme(HexLbId, i);
    end;
end;

procedure TForm1.Redraw1Click(Sender: TObject);
begin
  HighlightRedraw(DelphiLbId);
end;

procedure TForm1.Redraw2Click(Sender: TObject);
begin
  HighlightRedraw(AsmLbId);
end;

procedure TForm1.Redraw3Click(Sender: TObject);
begin
  HighlightRedraw(HexLbId);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ModalResult : Integer;
begin
  ModalResult := SettingsShowModal(Handle);
end;

end.
