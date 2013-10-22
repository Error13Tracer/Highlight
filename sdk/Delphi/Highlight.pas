unit Highlight;
//////////////////////////
//Highlight SDK         //
//Version: 1.1.0        //
//Date   : 22.10.2013   //
//Author : Error13Tracer//
//////////////////////////
interface

uses Windows;

const
  HighlightDll : LPCSTR = 'Highlight.dll';
//�����
  ID_UNKNOWN   = $FFFFFFFF;
  ID_DELPHI    = $00000000;
  ID_ASM       = $00000001;
  ID_HEX       = $00000002;
//���� �� �������
  ID_CFG_THEME = $FFFFFFFF;

type
  //��������� ���������
  TCreateHighlight   = function  (Handle  : HWND;
                                  Language: DWORD): DWORD; stdcall;
  //����� ListBox
  //����  ���������
  //������� - ID ��������

  //��������� � ������� OnDrawItem
  THighlightDrawItem = procedure (ID   : DWORD;
                                  Index: LongInt;
                                  Rect : TRect); stdcall;
  //ID ��������
  //Index
  //Rect

  //����������� ��������
  THighlightRedraw   = procedure (ID: DWORD); stdcall;
  //ID ��������

  //��������� �����
  TChangeLanguage    = procedure (ID      : DWORD;
                                  Language: DWORD); stdcall;
  //ID ��������
  //����

  //��������� ����
  TChangeTheme       = procedure (ID     : DWORD;
                                  ThemeID: DWORD); stdcall;
  //ID ��������
  //ID ����

  //��������� ���������� ���� � ����������� � ������
  TChangeGlobalTheme = procedure (Language: DWORD;
                                  ThemeID : DWORD); stdcall;
  //����
  //ID ����

  //�������� ���������
  TDeleteHighlight   = procedure (ID: DWORD); stdcall;
  //ID ��������

  //ID ������� ����
  TGetThemeId        = function  (ID: DWORD): DWORD; stdcall;
  //ID ��������
  //������� - ID ����

  //���-�� ��� ��� �����
  TGetThemesCount    = function  (Language: DWORD): DWORD; stdcall;
  //����
  //������� - ���-�� ���

  //��� ����
  TGetThemeName      = function  (Language: DWORD;
                                  ThemeID : DWORD;
                                  Buffer  : LPCSTR;
                                  BufSize : DWORD): DWORD; stdcall;
  //����
  //ID ����
  //�����
  //������ ������
  //������� - ����� ����� ����; ����� - ��� ����
  
  //������ ��������
  TSettingsShowModal = function  (WindowHandle: HWND): Integer; stdcall;
  //����� ����, ������ �������� ����������
  //������� - ModalResult
  
var
  CreateHighlight   : TCreateHighlight   = nil;
  HighlightDrawItem : THighlightDrawItem = nil;
  HighlightRedraw   : THighlightRedraw   = nil;
  ChangeLanguage    : TChangeLanguage    = nil;
  ChangeTheme       : TChangeTheme       = nil;
  ChangeGlobalTheme : TChangeGlobalTheme = nil;
  DeleteHighlight   : TDeleteHighlight   = nil;
  GetThemeId        : TGetThemeId        = nil;
  GetThemesCount    : TGetThemesCount    = nil;
  GetThemeName      : TGetThemeName      = nil;
  SettingsShowModal : TSettingsShowModal = nil;
  
  function  InitHighlight(): bool;
  procedure FreeHighlight();
 
implementation

var
  hHighlight : HMODULE;

function InitHighlight(): bool;
begin
  hHighlight := LoadLibraryA(HighlightDll);
  if (hHighlight <> 0) then
  begin
    CreateHighlight   := GetProcAddress(hHighlight, {01}'CreateHighlight@8');
    HighlightDrawItem := GetProcAddress(hHighlight, {02}'HighlightDrawItem@12');
    HighlightRedraw   := GetProcAddress(hHighlight, {03}'HighlightRedraw@4');
    ChangeLanguage    := GetProcAddress(hHighlight, {04}'ChangeLanguage@8');
    ChangeTheme       := GetProcAddress(hHighlight, {05}'ChangeTheme@8');
    ChangeGlobalTheme := GetProcAddress(hHighlight, {06}'ChangeGlobalTheme@8');
    DeleteHighlight   := GetProcAddress(hHighlight, {07}'DeleteHighlight@4');
    GetThemeId        := GetProcAddress(hHighlight, {08}'GetThemeId@4');
    GetThemesCount    := GetProcAddress(hHighlight, {09}'GetThemesCount@4');
    GetThemeName      := GetProcAddress(hHighlight, {0A}'GetThemeName@16');
    SettingsShowModal := GetProcAddress(hHighlight, {0B}'SettingsShowModal@4');
    Result := ((@CreateHighlight <> nil) and (@ChangeLanguage    <> nil) and (@ChangeTheme       <> nil) and 
               (@GetThemesCount  <> nil) and (@DeleteHighlight   <> nil) and (@HighlightDrawItem <> nil) and 
               (@HighlightRedraw <> nil) and (@GetThemeName      <> nil) and (@ChangeGlobalTheme <> nil) and 
               (@GetThemeId      <> nil) and (@SettingsShowModal <> nil));
  end else
    Result := False;
end;

procedure FreeHighlight();
begin
  if (hHighlight <> 0) then
    FreeLibrary(hHighlight);
end;

end.
