unit uExports;

interface

implementation

uses
  Windows, uHighlight, uHighlightTools, uHighlightSettings;

var
  aHighlight : array of TControlHighlight;

//Создание подсветки
function CreateHighlight(Handle  : HWND;
                         Language: DWORD): DWORD; stdcall; export;{01}
var
  i : integer;
  b : BOOL;
begin
  SetLength(aHighlight, Length(aHighlight) + 1);
  aHighlight[High(aHighlight)] := TControlHighlight.Create;
  with aHighlight[High(aHighlight)] do
  begin
    ControlHandle     := Handle;
    HighlightLanguage := Language;
    if Length(aHighlight) > 1 then
    begin
      repeat
        ID := Random(MaxDword) + 1;
        b  := False;
        for i := Low(aHighlight) to High(aHighlight) - 1 do
          if aHighlight[i].ID = ID then
          begin
            b := True;
            Break;
          end;
      until b = False;
    end else
      ID := Random(MaxDword) + 1;
    Result := DWORD(ID);
  end;
end;

//Обработчик события OnDrawItem для подсветки синтаксиса
procedure HighlightDrawItem(ID   : DWORD;
                            Index: LongInt;
                            Rect : TRect); stdcall; export;{02}
var
  i : integer;
begin
  for i := Low(aHighlight) to High(aHighlight) do
    if aHighlight[i].ID = ID then
    begin
      aHighlight[i].OnDrawItem(Index, Rect);
      Break;
    end;
end;

//Перерисовка всего контрола
procedure HighlightRedraw(ID: DWORD); stdcall; export;{03}
var
  i : integer;
begin
  for i := Low(aHighlight) to High(aHighlight) do
    if aHighlight[i].ID = ID then
    begin
      aHighlight[i].Update;
      Break;
    end;
end;

//Назначение языка
procedure ChangeLanguage(ID      : DWORD;
                         Language: DWORD); stdcall; export;{04}
var
  i : integer;
begin
  for i := Low(aHighlight) to High(aHighlight) do
    if aHighlight[i].ID = ID then
    begin
      aHighlight[i].HighlightLanguage := Language;
      Break;
    end;
end;

//Назначение темы
procedure ChangeTheme(ID     : DWORD;
                      ThemeID: DWORD); stdcall; export;{05}
var
  i : integer;
begin
  for i := Low(aHighlight) to High(aHighlight) do
    if aHighlight[i].ID = ID then
    begin
      aHighlight[i].HighlightTheme := ThemeID;
      Break;
    end;
end;

//Назначение глобальной темы для всех контролов с сохранением в конфиг
procedure ChangeGlobalTheme(Language: DWORD;
                            ThemeID : DWORD); stdcall; export;{06}
var
  i : integer;
begin
  case Language of
    ID_ASM    : SetThemeId(tlAsm, ThemeID, True);
    ID_HEX    : SetThemeId(tlHex, ThemeID, True);
    ID_DELPHI : SetThemeId(tlDelphi, ThemeID, True);
    else
      Exit;
  end;
  for i := Low(aHighlight) to High(aHighlight) do
    if aHighlight[i].HighlightLanguage = Language then
    begin
      aHighlight[i].HighlightTheme := ThemeID;
    end;
end;

//Удаление подсветки
procedure DeleteHighlight(ID: DWORD); stdcall; export;{07}
var
  i : integer;
begin
  for i := Low(aHighlight) to High(aHighlight) do
    if aHighlight[i].ID = ID then
    begin
      aHighlight[i].Free;
      if i <> High(aHighlight) then
        aHighlight[i] := aHighlight[High(aHighlight)];
      SetLength(aHighlight, Length(aHighlight) - 1);
      Break;
    end;
end;

//Возвращает Id текущей темы контрола
function GetThemeId(ID: DWORD): DWORD; stdcall; export;{08}
var
  i : integer;
begin
  Result := $FFFFFFFF;
  for i := Low(aHighlight) to High(aHighlight) do
    if aHighlight[i].ID = ID then
    begin
      Result := aHighlight[i].HighlightTheme;
      Break;
    end;
end;

//Возвращает кол-во доступных тем для языка
function GetThemesCount(Language: DWORD): DWORD; stdcall; export;{09}
begin
  case Language of
    ID_ASM    : Result := ThemesCount(tlAsm);
    ID_HEX    : Result := ThemesCount(tlHex);
    ID_DELPHI : Result := ThemesCount(tlDelphi);
    else
      Result := 0;
  end;
end;

//Возвращает в Buffer название темы по Id, результат - длина строки
function GetThemeName(Language: DWORD;
                      ThemeID : DWORD;
                      Buffer  : LPCSTR;
                      BufSize : DWORD): DWORD; stdcall; export;{0A}
var
  szBuf: string;
begin
  Result := 0;
  if (Buffer = nil) or (BufSize < 2) then
    Exit;
  ZeroMemory(Buffer, BufSize);
  szBuf := '';
  case Language of
    ID_ASM    : szBuf := GetThemeNameById(tlAsm, ThemeID);
    ID_HEX    : szBuf := GetThemeNameById(tlHex, ThemeID);
    ID_DELPHI : szBuf := GetThemeNameById(tlDelphi, ThemeID);
    else
      Exit;
  end;
  Result := Length(szBuf);
  if Result <> 0 then
    lstrcpy(Buffer, @szBuf[1]);
end;

//Возвращает ModalResult
function SettingsShowModal(WindowHandle: HWND): Integer; stdcall; export;
begin
  fHighlightSettings := TfHighlightSettings.CreateParented(WindowHandle);
  try
    Result := fHighlightSettings.ShowModal;
  finally
    fHighlightSettings.Free;
  end;
end;

exports
  CreateHighlight   index $01 name 'CreateHighlight@8',
  HighlightDrawItem index $02 name 'HighlightDrawItem@12',
  HighlightRedraw   index $03 name 'HighlightRedraw@4',
  ChangeLanguage    index $04 name 'ChangeLanguage@8',
  ChangeTheme       index $05 name 'ChangeTheme@8',
  ChangeGlobalTheme index $06 name 'ChangeGlobalTheme@8',
  DeleteHighlight   index $07 name 'DeleteHighlight@4',
  GetThemeId        index $08 name 'GetThemeId@4',
  GetThemesCount    index $09 name 'GetThemesCount@4',
  GetThemeName      index $0A name 'GetThemeName@16',
  SettingsShowModal index $0B name 'SettingsShowModal@4';
var
  i : integer;
initialization
  Randomize;
finalization
  for i := Low(aHighlight) to High(aHighlight) do
    aHighlight[i].Free;
  Finalize(aHighlight);
end.
