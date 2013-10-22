unit uHighlightTools;

interface

uses
  Windows, SysUtils, Classes, Graphics, uResIniFile;

const
  ID_CFG_THEME = $FFFFFFFF; //Тема из конфига

type
//-------------------------------------------------------------------------------------------
//Языки
  TArrayOfString = array of string;
  PLanguage = ^TLanguage;
  TLanguage = packed record
    KeyWords      : array of TArrayOfString;
    CommentsLine  : TArrayOfString;
    CommentsBegin : TArrayOfString;
    CommentsEnd   : TArrayOfString;
    Additional    : TArrayOfString;
  end;
  TThemeLang = (tlAsm, tlDelphi, tlHex, tlUnknown);
  THighlight = packed record
    Font    : TFont;
    BgColor : TColor;
  end;
//-------------------------------------------------------------------------------------------
//Темы
  PTheme    = ^TTheme;
  TTheme    = packed record
    Name      : string;
    BgColor   : TColor;
    Default   : THighlight;
    Numbers   : THighlight;
    Strings   : THighlight;
    Comments  : THighlight;
    Additional: THighlight;
    KeyWords  : array of THighlight;
    HEX       : packed record
      Addresses : THighlight;
      Bytes     : THighlight;
      Chars     : THighlight;
    end;
  end;

var
  bNoHighlight : bool = False;
  DelphiLang   : TLanguage;
  AsmLang      : TLanguage;
  HexLang      : TLanguage;
  szConfigFile : string;

  procedure InitHighlightModule;

  function  IniEraseSection  (Section: string): bool;
  function  SetConfigString  (szAppName, szKeyName, szValue: string): bool;
  function  GetConfigString  (szAppName, szKeyName: string;
                              const szDefault: string = '';
                              const pbResult: PBool = nil): string;
  function  GetConfigStringEx(szAppName, szKeyName: string;
                              var szResult: string;
                              const szDefault: string = ''): bool;

  function  DwordToFontStyles(Value: dword): TFontStyles;
  function  FontStylesToDword(Value: TFontStyles): dword;

  procedure LoadLanguage     (ThemeLang: TThemeLang; PLang: PLanguage);
  function  LoadTheme        (ThemeLang: TThemeLang;
                              const ThemeNumber: DWORD = ID_CFG_THEME;
                              const Apply: bool = False;
                              const pvTheme: PTheme = nil;
                              const Empty: bool = False): boolean;
                              
  function  SaveTheme        (Language: TThemeLang; ThemeId: DWORD;
                              Theme: TTheme): bool;

  procedure DeleteTheme      (ThemeLang: TThemeLang; ThemeId: DWORD);

  function  ThemesCount      (ThemeLang: TThemeLang): integer;
  function  SetThemeId       (Lang: TThemeLang; ID: DWORD;
                              const Save: bool = False): bool;
  function  GetThemeId       (Lang: TThemeLang): DWORD;
  function  GetThemeNameById (ThemeLang: TThemeLang;
                              ThemeNumber: DWORD): string;

  function  SplitString      (const S: String;
                              Delimiters: TSysCharSet): TArrayOfString;
  function  ExtractRes       (ResType, ResName, ResNewName: string): Boolean;

const
  DELPHI_LANG       = 'DelphiLang';
  ASM_LANG          = 'AsmLang';
  HEX_LANG          = 'HexLang';
  KEYWORDS          = 'KeyWords';
  COMMENTSBEGIN     = 'CommentsBegin';
  COMMENTSEND       = 'CommentsEnd';
  COMMENTSLINE      = 'CommentsLine';
  ADDITIONAL        = 'Additional';
  HEX_ADDRESSES     = 'Addresses';
  HEX_BYTES         = 'Bytes';
  HEX_CHARS         = 'Chars';
  DELPHI_THEME      = 'DelphiTheme';
  ASM_THEME         = 'AsmTheme';
  HEX_THEME         = 'HexTheme';
  THEME_KEYWORDS    = 'KeyWords';
  THEME_NAME        = 'ThemeName';
  THEME_BGCOLOR     = 'BgColor';
  THEME_DEFAULT     = 'Default';
  THEME_COMMENTS    = 'Comments';
  THEME_NUMBERS     = 'Numbers';
  THEME_STRINGS     = 'Strings';
  THEME_ADDITIONAL  = 'Additional';
  CONFIG_NAME       = 'Highlight';
  CONFIG_USE        = 'UseHighlight';
  CONFIG_AUTOSAVE   = 'Autosave';
  FS_NORMAL         = 0;
  FS_BOLD           = 1;
  FS_ITALIC         = 2;
  FS_UNDERLINE      = 4;
  FS_STRIKEOUT      = 8;
  DEF_HIGHLIGHT     = 'Fixedsys,000000,0,8,FFFFFF';
  DEF_BGCOLOR       = $FFFFFF;  

implementation

var
  i : integer;

//-------------------------------------------------------------------------------------------
//Работа с файлом конфигурации

function IniEraseSection(Section: string): bool;
begin
 Result := WritePrivateProfileString(PAnsiChar(Section), nil, nil,
                                     PAnsiChar(szConfigFile));
end;

function SetConfigString(szAppName, szKeyName, szValue: string): bool;
begin
  Result := WritePrivateProfileStringA(PAnsiChar(szAppName), PAnsiChar(szKeyName),
                                       PAnsiChar(szValue), PAnsiChar(szConfigFile));
end;

function GetConfigStringEx(szAppName, szKeyName: string;
 var szResult: string; const szDefault: string = ''): bool;
var
  Buffer  : array [0..4095] of AnsiChar;
begin
  Result := True;
  ZeroMemory(@Buffer, Length(Buffer));
  if not (GetPrivateProfileStringA(PAnsiChar(szAppName), PAnsiChar(szKeyName), nil,
                                   Buffer, Length(Buffer), PAnsiChar(szConfigFile)) > 0) then
  begin
    if szDefault <> '' then
      WritePrivateProfileStringA(PAnsiChar(szAppName), PAnsiChar(szKeyName), PAnsiChar(szDefault),
                                 PAnsiChar(szConfigFile));
    Result := False;
    szResult := szDefault;
  end else
    szResult := StrPas(Buffer);
end;

function GetConfigString(szAppName, szKeyName: string;
 const szDefault: string = ''; const pbResult: PBool = nil): string;
begin
  if pbResult <> nil then
    pbResult^ := GetConfigStringEx(szAppName, szKeyName, Result, szDefault)
  else
    GetConfigStringEx(szAppName, szKeyName, Result, szDefault);
end;

//-------------------------------------------------------------------------------------------
//Преобразование стилей шрифтов

function DwordToFontStyles(Value: dword): TFontStyles;
begin
  Result := [];
  if (FS_BOLD and Value) <> 0 then Result := Result + [fsBold];
  if (FS_ITALIC and Value) <> 0 then Result := Result + [fsItalic];
  if (FS_UNDERLINE and Value) <> 0 then Result := Result + [fsUnderline];
  if (FS_STRIKEOUT and Value) <> 0 then Result := Result + [fsStrikeOut];
end;

function FontStylesToDword(Value: TFontStyles): dword;
begin
  Result := 0;
  if (fsBold in Value) then Result := Result or FS_BOLD;
  if (fsItalic in Value) then Result := Result or FS_ITALIC;
  if (fsUnderline in Value) then Result := Result or FS_UNDERLINE;
  if (fsStrikeOut in Value) then Result := Result or FS_STRIKEOUT;
end;

//-------------------------------------------------------------------------------------------
//Загрузка и установка языков и стилей

procedure LoadLanguage(ThemeLang: TThemeLang; PLang: PLanguage);
var
  iCounter: integer;
  szBuffer: string;
  szAppKey: string;
  ResIni  : TResIniFile;
begin
  case ThemeLang of
    tlAsm   : szAppKey := ASM_LANG;
    tlDelphi: szAppKey := DELPHI_LANG;
    tlHex   : szAppKey := HEX_LANG;
  end;
  with PLang^ do begin
    for iCounter := Low(KeyWords) to High(KeyWords) do
      SetLength(KeyWords[iCounter], 0);
    SetLength(KeyWords, 0);
    SetLength(CommentsLine, 0);
    SetLength(CommentsBegin, 0);
    SetLength(CommentsEnd, 0);
    SetLength(Additional, 0);
  end;
  ResIni := TResIniFile.Create('CONFIG', 'LANGS');
  try
    //Загрузка ключевых слов
    iCounter := 0;
    while ResIni.ReadStringEx(szAppKey, KEYWORDS + IntToStr(iCounter), szBuffer) do
    begin
      SetLength(PLang^.KeyWords, iCounter + 1);
      PLang^.KeyWords[iCounter] := SplitString(szBuffer, [#32]);
      Inc(iCounter);
    end;
    //Загрузка начал комменариев
    PLang^.CommentsBegin := SplitString(ResIni.ReadString(szAppKey, COMMENTSBEGIN), [#32]);
    //Загрузка концов комментариев
    PLang^.CommentsEnd   := SplitString(ResIni.ReadString(szAppKey, COMMENTSEND), [#32]);
    //Загрузка строчных комментариев
    PLang^.CommentsLine  := SplitString(ResIni.ReadString(szAppKey, COMMENTSLINE), [#32]);
    //Загрузка дополнительной подсветки (изначально < > в дизасме IDR)
    PLang^.Additional    := SplitString(ResIni.ReadString(szAppKey, ADDITIONAL), [#32]);
  finally
    ResIni.Free;
  end;
end;

function LoadTheme(ThemeLang: TThemeLang; const ThemeNumber: DWORD = ID_CFG_THEME;
 const Apply: bool = False; const pvTheme: PTheme = nil; const Empty: bool = False): boolean;

  function LoadThemeHighlight(var AHighlight: THighlight; VarArrOfStr: TArrayOfString): bool;
  begin
    if Length(VarArrOfStr) = 5 then
    begin
      with AHighlight do
      begin
        Font.Name  := Trim(VarArrOfStr[0]);
        Font.Color := StrToIntDef('$' + Trim(VarArrOfStr[1]), clBlack);
        Font.Style := DwordToFontStyles(StrToIntDef(Trim(VarArrOfStr[2]), 0));
        Font.Size  := StrToIntDef(Trim(VarArrOfStr[3]), 0);
        BgColor    := StrToIntDef('$' + Trim(VarArrOfStr[4]), clBlack);
      end;
      Result := True;
    end else
      Result := False;
  end;

var
  i             : integer;
  iThemeNumber  : integer;
  szBuffer      : string;
  szDefHighlight: string;
  szThemeLang   : string;
  lpvLang       : PLanguage;
begin
  Result := False;
  case ThemeLang of
    tlAsm   :
    begin
      szThemeLang := ASM_THEME;
      lpvLang     := @AsmLang;
    end;
    tlDelphi:
    begin
      szThemeLang := DELPHI_THEME;
      lpvLang     := @DelphiLang;
    end;
    tlHex   :
    begin
      szThemeLang := HEX_THEME;
      lpvLang     := @HexLang;
    end;
  end;
  if ThemeNumber = ID_CFG_THEME then
    iThemeNumber := StrToIntDef(GetConfigString(CONFIG_NAME, szThemeLang, '0'), 0)
  else
    iThemeNumber := ThemeNumber;
  if not Empty then
  begin
    if not GetConfigStringEx(szThemeLang + IntToStr(iThemeNumber), THEME_NAME, szBuffer) then
      Exit
    else
      Result := True;
  end else begin
    szBuffer := 'New Theme';
    SetConfigString(szThemeLang + IntToStr(iThemeNumber), THEME_NAME, szBuffer);
  end;
  if (not Apply) or (pvTheme = nil) then
    Exit;
  pvTheme^.Name := Trim(szBuffer);
  pvTheme^.BgColor := StrToIntDef('$' + GetConfigString(szThemeLang + IntToStr(iThemeNumber), THEME_BGCOLOR, IntToHex(DEF_BGCOLOR, 6)), DEF_BGCOLOR);
  szDefHighlight := GetConfigString(szThemeLang + IntToStr(iThemeNumber), THEME_DEFAULT, DEF_HIGHLIGHT);
  LoadThemeHighlight(pvTheme^.Default, SplitString(szDefHighlight, [',']));
  if ThemeLang = tlHex then
  begin
    LoadThemeHighlight(pvTheme^.HEX.Addresses, SplitString(GetConfigString(szThemeLang + IntToStr(iThemeNumber), HEX_ADDRESSES, szDefHighlight), [',']));
    LoadThemeHighlight(pvTheme^.HEX.Bytes, SplitString(GetConfigString(szThemeLang + IntToStr(iThemeNumber), HEX_BYTES, szDefHighlight), [',']));
    LoadThemeHighlight(pvTheme^.HEX.Chars, SplitString(GetConfigString(szThemeLang + IntToStr(iThemeNumber), HEX_CHARS, szDefHighlight), [',']));
    Exit;
  end;
  LoadThemeHighlight(pvTheme^.Comments, SplitString(GetConfigString(szThemeLang + IntToStr(iThemeNumber), THEME_COMMENTS, szDefHighlight), [',']));
  LoadThemeHighlight(pvTheme^.Numbers, SplitString(GetConfigString(szThemeLang + IntToStr(iThemeNumber), THEME_NUMBERS, szDefHighlight), [',']));
  LoadThemeHighlight(pvTheme^.Strings, SplitString(GetConfigString(szThemeLang + IntToStr(iThemeNumber), THEME_STRINGS, szDefHighlight), [',']));
  LoadThemeHighlight(pvTheme^.Additional, SplitString(GetConfigString(szThemeLang + IntToStr(iThemeNumber), THEME_ADDITIONAL, szDefHighlight), [',']));
  for i := Low(pvTheme^.KeyWords) to High(pvTheme^.KeyWords) do
    if Assigned(pvTheme^.KeyWords[i].Font) then
      pvTheme^.KeyWords[i].Font.Free;
  SetLength(pvTheme^.KeyWords, Length(lpvLang^.KeyWords));
  for i := Low(lpvLang^.KeyWords) to High(lpvLang^.KeyWords) do
  begin
    pvTheme^.KeyWords[i].Font := TFont.Create;
    LoadThemeHighlight(pvTheme^.KeyWords[i], SplitString(GetConfigString(szThemeLang + IntToStr(iThemeNumber), THEME_KEYWORDS + IntToStr(i), szDefHighlight), [',']));
  end;
end;

//-------------------------------------------------------------------------------------------
//Сохранение тем

function SaveTheme(Language: TThemeLang; ThemeId: DWORD; Theme: TTheme): bool;

  function HighlightToString(AHighlight: THighlight): string;
  begin
    with AHighlight do
    begin
      Result := Font.Name + ','
              + IntToHex(DWORD(Font.Color), 6) + ','
              + IntToStr(FontStylesToDword(Font.Style)) + ','
              + IntToStr(Font.Size) + ','
              + IntToHex(DWORD(BgColor), 6);
    end;
  end;

var
  i           : integer;
  szThemeLang : string;
  lpvLang     : PLanguage;
begin
  Result := True;
  case Language of
    tlAsm   :
    begin
      szThemeLang := ASM_THEME + IntToStr(ThemeId);
      lpvLang     := @AsmLang;
    end;
    tlDelphi:
    begin
      szThemeLang := DELPHI_THEME + IntToStr(ThemeId);
      lpvLang     := @DelphiLang;
    end;
    tlHex   :
    begin
      szThemeLang := HEX_THEME + IntToStr(ThemeId);
      lpvLang     := @HexLang;
    end;
  end;
  Result := Result and SetConfigString(szThemeLang, THEME_NAME, Theme.Name);
  Result := Result and SetConfigString(szThemeLang, THEME_BGCOLOR, IntToHex(DWORD(Theme.BgColor), 6));
  Result := Result and SetConfigString(szThemeLang, THEME_DEFAULT, HighlightToString(Theme.Default));
  if Language = tlHex then
  begin
    Result := Result and SetConfigString(szThemeLang, HEX_ADDRESSES, HighlightToString(Theme.HEX.Addresses));
    Result := Result and SetConfigString(szThemeLang, HEX_BYTES, HighlightToString(Theme.HEX.Bytes));
    Result := Result and SetConfigString(szThemeLang, HEX_CHARS, HighlightToString(Theme.HEX.Chars));
    Exit;
  end;
  Result := Result and SetConfigString(szThemeLang, THEME_COMMENTS, HighlightToString(Theme.Comments));
  Result := Result and SetConfigString(szThemeLang, THEME_NUMBERS, HighlightToString(Theme.Numbers));
  Result := Result and SetConfigString(szThemeLang, THEME_STRINGS, HighlightToString(Theme.Strings));
  Result := Result and SetConfigString(szThemeLang, THEME_ADDITIONAL, HighlightToString(Theme.Additional));
  for i := Low(lpvLang^.KeyWords) to High(lpvLang^.KeyWords) do
    Result := Result and SetConfigString(szThemeLang, THEME_KEYWORDS + IntToStr(i), HighlightToString(Theme.KeyWords[i]));
end;

//-------------------------------------------------------------------------------------------
//Удаление тем
procedure DeleteTheme(ThemeLang: TThemeLang; ThemeId: DWORD);
var
  szAppKey: string;
  Count   : dword;
  i, j    : integer;
  Theme   : TTheme;
begin
  case ThemeLang of
    tlAsm   : szAppKey := ASM_THEME;
    tlDelphi: szAppKey := DELPHI_THEME;
    tlHex   : szAppKey := HEX_THEME;
  end;
  Count := ThemesCount(ThemeLang);
  for i := ThemeId to Pred(Count) do
  begin
    if i = Pred(Integer(Count)) then
      Break;
    with Theme do begin
      Default.Font    := TFont.Create;
      Comments.Font   := TFont.Create;
      Numbers.Font    := TFont.Create;
      Strings.Font    := TFont.Create;
      Additional.Font := TFont.Create;
      HEX.Addresses.Font := TFont.Create;
      HEX.Bytes.Font     := TFont.Create;
      HEX.Chars.Font     := TFont.Create;
    end;
    LoadTheme(ThemeLang, i + 1, True, @Theme);
    SaveTheme(ThemeLang, i, Theme);
    with Theme do begin
      Default.Font.Free;
      Comments.Font.Free;
      Numbers.Font.Free;
      for j := Low(KeyWords) to High(KeyWords) do
        KeyWords[j].Font.Free;
      Strings.Font.Free;
      Additional.Font.Free;
      HEX.Addresses.Font.Free;
      HEX.Bytes.Font.Free;
      HEX.Chars.Font.Free;
    end;
  end;
  IniEraseSection(szAppKey + IntToStr(Pred(ThemesCount(ThemeLang))));
  Finalize(Theme.KeyWords);  
end;

//-------------------------------------------------------------------------------------------
//Информационные функции работы с темами

function ThemesCount(ThemeLang: TThemeLang): integer;
begin
  Result := 0;
  while LoadTheme(ThemeLang, Result) do
    Inc(Result);
end;

function GetThemeId(Lang: TThemeLang): DWORD;
begin
  case Lang of
    tlAsm    : Result := StrToIntDef(GetConfigString(CONFIG_NAME, ASM_THEME, '0'), 0);
    tlHex    : Result := StrToIntDef(GetConfigString(CONFIG_NAME, HEX_THEME, '0'), 0);
    tlDelphi : Result := StrToIntDef(GetConfigString(CONFIG_NAME, DELPHI_THEME, '0'), 0);
    else
      Result := $FFFFFFFF;
  end;
end;

function SetThemeId(Lang: TThemeLang; ID: DWORD; const Save: bool = False): bool;
begin
  case Lang of
    tlAsm    :
    begin
      Result := (LoadTheme(tlAsm, ID, False));
      if Result and Save then
        Result := (SetConfigString(CONFIG_NAME, ASM_THEME, IntToStr(Integer(ID))));
    end;
    tlHex    :
    begin
      Result := (LoadTheme(tlHex, ID, False));
      if Result and Save then
        Result := (SetConfigString(CONFIG_NAME, HEX_THEME, IntToStr(Integer(ID))));
    end;
    tlDelphi :
    begin
      Result := (LoadTheme(tlDelphi, ID, False));
      if Result and Save then
        Result := (SetConfigString(CONFIG_NAME, DELPHI_THEME, IntToStr(Integer(ID))));
    end;
    else
      Result := False;
  end;
end;

function GetThemeNameById(ThemeLang: TThemeLang; ThemeNumber: DWORD): string;
var
  iThemeNumber  : integer;
  szBuffer      : string;
begin
  Result := '';
  case ThemeLang of
    tlAsm   : szBuffer := ASM_THEME;
    tlDelphi: szBuffer := DELPHI_THEME;
    tlHex   : szBuffer := HEX_THEME;
    else
      Exit;
  end;
  if ThemeNumber = ID_CFG_THEME then
    iThemeNumber := StrToIntDef(GetConfigString(CONFIG_NAME, szBuffer, '0'), 0)
  else
    iThemeNumber := ThemeNumber;
  if GetConfigStringEx(szBuffer + IntToStr(iThemeNumber), THEME_NAME, szBuffer) then
    Result := szBuffer;
end;

//-------------------------------------------------------------------------------------------
//Дополнительные функции

function SplitString(const S: String; Delimiters: TSysCharSet): TArrayOfString;
var
  len, idx1, idx2, idx: integer;
begin
  Result := nil;
  if Length(S) = 0 then
    Exit;
  len := Length(S);
  SetLength(Result, len);
  idx2 := 1;
  idx := 0;
  repeat
    idx1 := idx2;
    while (idx2 <= len) and not(S[idx2] in Delimiters) do
      inc(idx2);
    if idx1 <= idx2 then
    begin
      Result[idx] := (Copy(S, idx1, idx2-idx1));
      inc(idx);
    end;
    if (idx2 <= len) and (S[idx2] in Delimiters) then
      inc(idx2);
  until idx2 > len;
  SetLength(Result, idx);
end;

function ExtractRes(ResType, ResName, ResNewName: string): Boolean;
var
	Res: TResourceStream;
begin
	Result := (FindResource(Hinstance, PAnsiChar(ResName), PAnsiChar(ResType)) <> 0);
  if not Result then
    Exit;
	Res := TResourceStream.Create(Hinstance, Resname, PChar(ResType));
	try
		Res.SavetoFile(ResNewName);
		Result := True;
	finally
		Res.Free;
	end;
end;

//-------------------------------------------------------------------------------------------
//Инициализация модуля

procedure InitHighlightModule;
label
  _reset, _init;
const
  CRITICAL_ERROR: PAnsiChar = 'Critical error. Can''t extract configuration file.';
  ERROR         : PAnsiChar = 'Highlight error';
  RES_TYPE      : PAnsiChar = 'CONFIG';
  RES_NAME      : PAnsiChar = 'INI';
  CFG_FILE_NAME : PAnsiChar = 'Highlight.ini';
  BACKUP_EXT    : PAnsiChar = '.bak';
var
  ThNum: integer;
  DelphiThemesCount: integer;
  AsmThemesCount   : integer;
  HexThemesCount   : integer;
begin
  szConfigFile := ExtractFilePath(ParamStr(0)) + CFG_FILE_NAME;
  if not FileExists(szConfigFile) then
    if not ExtractRes(RES_TYPE, RES_NAME, szConfigFile) then
    begin
      bNoHighlight := True;
      MessageBoxA(0, CRITICAL_ERROR, ERROR, MB_ICONERROR);
      Exit;
    end;
  goto _init;
_reset:
  if FileExists(szConfigFile + BACKUP_EXT) then
    DeleteFileA(PAnsiChar(szConfigFile + BACKUP_EXT));
  if (not MoveFile(PAnsiChar(szConfigFile), PAnsiChar(szConfigFile + BACKUP_EXT)))or
   (not ExtractRes(RES_TYPE, RES_NAME, szConfigFile)) then
  begin
    bNoHighlight := True;
    MessageBoxA(0, CRITICAL_ERROR, ERROR, MB_ICONERROR);
    Exit;
  end;
_init:
  bNoHighlight :=  not StrToBoolDef(GetConfigString(CONFIG_NAME, CONFIG_USE, '1'), True);
  LoadLanguage(tlAsm, @AsmLang);
  LoadLanguage(tlDelphi, @DelphiLang);
  AsmThemesCount    := ThemesCount(tlAsm);
  DelphiThemesCount := ThemesCount(tlDelphi);
  HexThemesCount    := ThemesCount(tlHex);
  if (AsmThemesCount = 0) or (DelphiThemesCount = 0) or (HexThemesCount = 0) then
    goto _reset;
  ThNum := StrToIntDef(GetConfigString(CONFIG_NAME, ASM_THEME, '0'), 0);
  if (ThNum < 0) then
    ThNum := 0;
  while (not LoadTheme(tlAsm, ThNum, False)) do
  begin
    dec(ThNum);
    if ThNum < 0 then
      goto _reset;
  end;
  if ThNum <> StrToIntDef(GetConfigString(CONFIG_NAME, ASM_THEME, '0'), 0) then
    SetConfigString(CONFIG_NAME, ASM_THEME, IntToStr(ThNum));
  ThNum := StrToIntDef(GetConfigString(CONFIG_NAME, DELPHI_THEME, '0'), 0);
  if (ThNum < 0) then
    ThNum := 0;
  while (not LoadTheme(tlDelphi, ThNum, False)) do
  begin
    dec(ThNum);
    if ThNum < 0 then
      goto _reset;
  end;
  if ThNum <> StrToIntDef(GetConfigString(CONFIG_NAME, DELPHI_THEME, '0'), 0) then
    SetConfigString(CONFIG_NAME, DELPHI_THEME, IntToStr(ThNum));
  ThNum := StrToIntDef(GetConfigString(CONFIG_NAME, HEX_THEME, '0'), 0);
  if (ThNum < 0) then
    ThNum := 0;
  while (not LoadTheme(tlHex, ThNum, False)) do
  begin
    dec(ThNum);
    if ThNum < 0 then
      goto _reset;
  end;
  if ThNum <> StrToIntDef(GetConfigString(CONFIG_NAME, HEX_THEME, '0'), 0) then
    SetConfigString(CONFIG_NAME, HEX_THEME, IntToStr(ThNum));
end;

//-------------------------------------------------------------------------------------------

initialization
  InitHighlightModule;
finalization
  with DelphiLang do begin
    for i := Low(KeyWords) to High(KeyWords) do
      Finalize(KeyWords[i]);
    Finalize(KeyWords);
    Finalize(CommentsLine);
    Finalize(CommentsBegin);
    Finalize(CommentsEnd);
    Finalize(Additional);
  end;
  with AsmLang do begin
    for i := Low(KeyWords) to High(KeyWords) do
      Finalize(KeyWords[i]);
    Finalize(KeyWords);
    Finalize(CommentsLine);
    Finalize(CommentsBegin);
    Finalize(CommentsEnd);
    Finalize(Additional);
  end;
  with HexLang do begin
    for i := Low(KeyWords) to High(KeyWords) do
      Finalize(KeyWords[i]);
    Finalize(KeyWords);
    Finalize(CommentsLine);
    Finalize(CommentsBegin);
    Finalize(CommentsEnd);
    Finalize(Additional);
  end;
end.
