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
//Языки
  ID_UNKNOWN   = $FFFFFFFF;
  ID_DELPHI    = $00000000;
  ID_ASM       = $00000001;
  ID_HEX       = $00000002;
//Тема из конфига
  ID_CFG_THEME = $FFFFFFFF;

type
  //Установка подсветки
  TCreateHighlight   = function  (Handle  : HWND;
                                  Language: DWORD): DWORD; stdcall;
  //Хендл ListBox
  //Язык  подсветки
  //Возврат - ID контрола

  //Отрисовка в событии OnDrawItem
  THighlightDrawItem = procedure (ID   : DWORD;
                                  Index: LongInt;
                                  Rect : TRect); stdcall;
  //ID контрола
  //Index
  //Rect

  //Перерисовка контрола
  THighlightRedraw   = procedure (ID: DWORD); stdcall;
  //ID контрола

  //Установка языка
  TChangeLanguage    = procedure (ID      : DWORD;
                                  Language: DWORD); stdcall;
  //ID контрола
  //Язык

  //Установка темы
  TChangeTheme       = procedure (ID     : DWORD;
                                  ThemeID: DWORD); stdcall;
  //ID контрола
  //ID темы

  //Установка глабальной темы с сохранением в конфиг
  TChangeGlobalTheme = procedure (Language: DWORD;
                                  ThemeID : DWORD); stdcall;
  //Язык
  //ID темы

  //Удаление подсветки
  TDeleteHighlight   = procedure (ID: DWORD); stdcall;
  //ID контрола

  //ID текущей темы
  TGetThemeId        = function  (ID: DWORD): DWORD; stdcall;
  //ID контрола
  //Возврат - ID темы

  //Кол-во тем для языка
  TGetThemesCount    = function  (Language: DWORD): DWORD; stdcall;
  //Язык
  //Возврат - кол-во тем

  //Имя темы
  TGetThemeName      = function  (Language: DWORD;
                                  ThemeID : DWORD;
                                  Buffer  : LPCSTR;
                                  BufSize : DWORD): DWORD; stdcall;
  //Язык
  //ID темы
  //Буфер
  //Размер буфера
  //Возврат - длина имени темы; Буфер - имя темы
  
  //Диалог настроек
  TSettingsShowModal = function  (WindowHandle: HWND): Integer; stdcall;
  //Хендл окна, поверх которого отображать
  //Возврат - ModalResult
  
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
