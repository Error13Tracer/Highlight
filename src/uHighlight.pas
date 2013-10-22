unit uHighlight;

interface

uses
  Windows, Messages, SysUtils, StrUtils, Classes, Graphics, uHighlightTools;

const
  ID_UNKNOWN = $FFFFFFFF; //Неизвестный язык
  ID_DELPHI  = $00000000; //Delphi
  ID_ASM     = $00000001; //Assembler
  ID_HEX     = $00000002; //HEX

type
  TControlHighlight = class
  protected
    procedure DelphiHighlight(Index: LongInt; Rect: TRect);
    procedure AsmHighlight   (Index: LongInt; Rect: TRect);
    procedure HexHighlight   (Index: LongInt; Rect: TRect);
    function  DrawDefault    (Index: LongInt; Rect: TRect): BOOL;
    function  GetText        (Index: LongInt)             : string;
  private
    cNoHighlight   : BOOL;
    cPreviewMode   : BOOL;
    cID            : DWORD;
    cThemeId       : DWORD;
    cControlHandle : HWND;
    cHighlightLang : TThemeLang;
    cCanvas        : TCanvas;
    procedure SetControlHandle           (const Value: HWND);
    procedure SetControlHighlightLanguage(const Value: DWORD);
    procedure SetControlHighlightTheme   (const Value: DWORD);
    function  GetControlHighlightLanguage: DWORD;
    function  GetControlHighlightTheme   : DWORD;
    procedure SetPreviewMode(const Value: BOOL);
  public
    cHighlightTheme: TTheme;
    constructor Create;
    destructor  Destroy; override;
    procedure   OnDrawItem(Index: LongInt; Rect: TRect);
    procedure   Update;
    procedure   Redraw;
    procedure   ChangeCustomTheme(CustomTheme: TTheme);
    procedure   ChangeCustomThemeEx(CustomTheme: TTheme);
  published
    property ID: DWORD                read cID                         write cID;
    property ControlHandle: HWND      read cControlHandle              write SetControlHandle;
    property HighlightLanguage: DWORD read GetControlHighlightLanguage write SetControlHighlightLanguage;
    property HighlightTheme: DWORD    read GetControlHighlightTheme    write SetControlHighlightTheme;
    property PreviewMode: BOOL        read cPreviewMode                write SetPreviewMode;
    //property CustomTheme: TTheme      read cHighlightTheme             write ChangeCustomTheme;
    //property CustomFont : TFont      read cHighlightTheme.Default.Font         write cHighlightTheme.Default.Font;//ChangeCustomTheme;
  end;

implementation

//-------------------------------------------------------------------------------------------
//PROTECTED

//Подсветка Delphi
procedure TControlHighlight.DelphiHighlight(Index: LongInt; Rect: TRect);
label
  _ToDraw;
const
  INDENT_VALUE = 1;
var
  i, i1, i2: integer;
  slen    : integer;
  OutPos  : integer;
  SrchPos : integer;
  ItemText: string;
  OutStr  : string;
  BufStr  : string;
begin
  if DrawDefault(Index, Rect) then
    Exit;
  ItemText := TrimRight(GetText(Index));
  slen     := Length(ItemText);
  with cCanvas do begin
    Brush.Color := cHighlightTheme.BgColor;
    FillRect(Rect);
    i := 1;
    OutPos := Rect.Left + INDENT_VALUE;
    if (slen <> 0) then
    repeat
      OutStr := '';
      case ItemText[i] of
        #32, #09: //Пробел и табуляция
        begin
          while ItemText[i] in [#32, #09] do
          begin
            OutStr := OutStr + ItemText[i];
            inc(i);
          end;
          Font        := cHighlightTheme.Default.Font;
          Brush.Color := cHighlightTheme.Default.BgColor;
        end;
        '$', '0'..'9':
        begin
          //if ItemText[i] = '$' then //HEX числа
          begin
            OutStr := ItemText[i];
            inc(i);
            while ItemText[i] in ['0'..'9', 'A'..'F', 'a'..'f', '.'] do
            begin
              OutStr := OutStr + ItemText[i];
              inc(i);
            end;
          end;
          {else //десятичные числа, включая с плавающей точкой
            while ItemText[i] in ['0'..'9', '.'] do
            begin
              OutStr := OutStr + ItemText[i];
              inc(i);
            end;  }
          Font        := cHighlightTheme.Numbers.Font;
          Brush.Color := cHighlightTheme.Numbers.BgColor;
        end;
        '_','A'..'Z','a'..'z': //Слово (либо код, либо ключевое)
        begin
          while ItemText[i] in ['_','A'..'Z','a'..'z', '0'..'9'] do
          begin
            OutStr := OutStr + ItemText[i];
            inc(i);
          end;
          for i1 := Low(DelphiLang.KeyWords) to High(DelphiLang.KeyWords) do
            for i2 := Low(DelphiLang.KeyWords[i1]) to High(DelphiLang.KeyWords[i1]) do
              if LowerCase(OutStr) = LowerCase(DelphiLang.KeyWords[i1, i2]) then
              begin
                Font        := cHighlightTheme.KeyWords[i1].Font;
                Brush.Color := cHighlightTheme.KeyWords[i1].BgColor;
                goto _ToDraw;
              end;
          Font        := cHighlightTheme.Default.Font;
          Brush.Color := cHighlightTheme.Default.BgColor;
        end;
        #39: //(Кавычка) Строки
        begin
          repeat
            OutStr := OutStr + ItemText[i];
            inc(i);
            while ItemText[i] <> #39 do
            begin
              OutStr := OutStr + ItemText[i];
              inc(i);
            end;
            OutStr := OutStr + ItemText[i];
            if ItemText[i + 1] = #39 then
              Continue;
          until (ItemText[i] = #39)or(i >= slen);
          inc(i);
          Font        := cHighlightTheme.Strings.Font;
          Brush.Color := cHighlightTheme.Strings.BgColor;
        end;
        '#': //Символы (Char)
        begin
          OutStr := ItemText[i];
          inc(i);
          if ItemText[i] = '$' then //HEX числа
          begin
            OutStr := OutStr + ItemText[i];
            inc(i);
            while ItemText[i] in ['0'..'9', 'A'..'F', 'a'..'f'] do
            begin
              OutStr := OutStr + ItemText[i];
              inc(i);
            end;
          end
          else //десятичные числа, включая с плавающей точкой
            while ItemText[i] in ['0'..'9', '.'] do
            begin
              OutStr := OutStr + ItemText[i];
              inc(i);
            end;
          Font        := cHighlightTheme.Strings.Font;
          Brush.Color := cHighlightTheme.Strings.BgColor;
        end;
        else
        begin
          //Комментарии с завершающим словом
          for i1 := Low(DelphiLang.CommentsBegin) to High(DelphiLang.CommentsBegin) do
          begin
            if (i + Length(DelphiLang.CommentsBegin[i1])) > slen then
              Continue;
            BufStr := Copy(ItemText, i, Length(DelphiLang.CommentsBegin[i1]));
            if BufStr = DelphiLang.CommentsBegin[i1] then
            begin
              SrchPos := PosEx(DelphiLang.CommentsEnd[i1], ItemText,
                               i + Length(DelphiLang.CommentsBegin[i1]));
              if SrchPos <> 0 then
              begin
                SrchPos     := SrchPos + Length(DelphiLang.CommentsEnd[i1]) - 1;
              end else begin
                //Здесь ситуация, когда комент не заканчивается на этой строке (пока пусть так будет)
                SrchPos := slen;
              end;
              OutStr      := Copy(ItemText, i, SrchPos - i + 1);
              i           := SrchPos + 1;
              Font        := cHighlightTheme.Comments.Font;
              Brush.Color := cHighlightTheme.Comments.BgColor;
              goto _ToDraw;
            end;
          end;
          //строка комментария
          for i1 := Low(DelphiLang.CommentsLine) to High(DelphiLang.CommentsLine) do
          begin
            if (i + Length(DelphiLang.CommentsLine[i1])) > slen then
              Continue;
            BufStr := Copy(ItemText, i, Length(DelphiLang.CommentsLine[i1]));
            if BufStr = DelphiLang.CommentsLine[i1] then
            begin
              OutStr      := Copy(ItemText, i, slen - i + 1);
              i           := slen + 1;
              Font        := cHighlightTheme.Comments.Font;
              Brush.Color := cHighlightTheme.Comments.BgColor;
              goto _ToDraw;
            end;
          end;
          //дополнительные символы/слова
          for i1 := Low(DelphiLang.Additional) to High(DelphiLang.Additional) do
          begin
            if (i + Length(DelphiLang.Additional[i1])) > slen then
              Continue;
            BufStr := Copy(ItemText, i, Length(DelphiLang.Additional[i1]));
            if BufStr = DelphiLang.Additional[i1] then
            begin
              OutStr      := DelphiLang.Additional[i1];
              i           := Length(DelphiLang.Additional[i1]) + 1;
              Font        := cHighlightTheme.Additional.Font;
              Brush.Color := cHighlightTheme.Additional.BgColor;
              goto _ToDraw;
            end;
          end;
          OutStr      := ItemText[i];
          Font        := cHighlightTheme.Default.Font;
          Brush.Color := cHighlightTheme.Default.BgColor;
          inc(i);
        end;
      end;
    _ToDraw:
      TextOut(OutPos + INDENT_VALUE, Rect.Top, OutStr);
      OutPos := OutPos + TextWidth(OutStr) + INDENT_VALUE;
    until i > slen;
  end;
end;

//Подсветка Assembler
procedure TControlHighlight.AsmHighlight(Index: LongInt; Rect: TRect);
label
  _ToDraw;
const
  INDENT_VALUE = 1;
var
  i, i1, i2: integer;
  slen    : integer;
  OutPos  : integer;
  SrchPos : integer;
  ItemText: string;
  OutStr  : string;
  BufStr  : string;
begin
  if DrawDefault(Index, Rect) then
    Exit;
  ItemText := TrimRight(GetText(Index));
  slen     := Length(ItemText);
  with cCanvas do begin
    Brush.Color := cHighlightTheme.BgColor;
    FillRect(Rect);
    i := 1;
    OutPos := Rect.Left + INDENT_VALUE;
    if (slen <> 0) then
    repeat
      OutStr := '';
      case ItemText[i] of
        #32, #09: //пробел и табуляция
        begin
          while ItemText[i] in [#32, #09] do
          begin
            OutStr := OutStr + ItemText[i];
            inc(i);
          end;
          Font        := cHighlightTheme.Default.Font;
          Brush.Color := cHighlightTheme.Default.BgColor;
        end;
        '0'..'9':
        begin
          while ItemText[i] in ['0'..'9', 'A'..'F', 'a'..'f', 'h', 'H'] do
          begin
            OutStr := OutStr + ItemText[i];
            inc(i);
          end;
          Font        := cHighlightTheme.Numbers.Font;
          Brush.Color := cHighlightTheme.Numbers.BgColor;
        end;
        '_','A'..'Z','a'..'z','.','%','@','$','?': //Слово (либо код, либо ключевое)
        begin
          while ItemText[i] in ['_','A'..'Z','a'..'z', '0'..'9', '.', '%', '@', '$', '?'] do
          begin
            OutStr := OutStr + ItemText[i];
            inc(i);
          end;
          for i1 := Low(AsmLang.KeyWords) to High(AsmLang.KeyWords) do
            for i2 := Low(AsmLang.KeyWords[i1]) to High(AsmLang.KeyWords[i1]) do
              if LowerCase(OutStr) = LowerCase(AsmLang.KeyWords[i1, i2]) then
              begin
                Font        := cHighlightTheme.KeyWords[i1].Font;
                Brush.Color := cHighlightTheme.KeyWords[i1].BgColor;
                goto _ToDraw;
              end;
          Font        := cHighlightTheme.Default.Font;
          Brush.Color := cHighlightTheme.Default.BgColor;
        end;
        #34: //Строки (если есть) - кавычки (")
        begin
          repeat
            OutStr := OutStr + ItemText[i];
            inc(i);
            while ItemText[i] in [#34] do
            begin
              OutStr := OutStr + ItemText[i];
              inc(i);
            end;
            OutStr := OutStr + ItemText[i];
            if ItemText[i + 1] in [#34] then
              Continue;
          until (ItemText[i] in [#34])or(i >= slen);
          inc(i);
          Font        := cHighlightTheme.Strings.Font;
          Brush.Color := cHighlightTheme.Strings.BgColor;
        end;
        else
        begin
          //Комментарии с завершающим словом
          for i1 := Low(AsmLang.CommentsBegin) to High(AsmLang.CommentsBegin) do
          begin
            if (i + Length(AsmLang.CommentsBegin[i1])) > slen then
              Continue;
            BufStr := Copy(ItemText, i, Length(AsmLang.CommentsBegin[i1]));
            if BufStr = AsmLang.CommentsBegin[i1] then
            begin
              SrchPos := PosEx(AsmLang.CommentsEnd[i1], ItemText,
                               i + Length(AsmLang.CommentsBegin[i1]));
              if SrchPos <> 0 then
              begin
                SrchPos     := SrchPos + Length(AsmLang.CommentsEnd[i1]) - 1;
              end else begin
                //Здесь ситуация, когда комент не заканчивается на этой строке (пока пусть так будет)
                SrchPos := slen;
              end;
              OutStr      := Copy(ItemText, i, SrchPos - i + 1);
              i           := SrchPos + 1;
              Font        := cHighlightTheme.Comments.Font;
              Brush.Color := cHighlightTheme.Comments.BgColor;
              goto _ToDraw;
            end;
          end;
          //строка комментария
          for i1 := Low(AsmLang.CommentsLine) to High(AsmLang.CommentsLine) do
          begin
            if (i + Length(AsmLang.CommentsLine[i1])) > slen then
              Continue;
            BufStr := Copy(ItemText, i, Length(AsmLang.CommentsLine[i1]));
            if BufStr = AsmLang.CommentsLine[i1] then
            begin
              OutStr      := Copy(ItemText, i, slen - i + 1);
              i           := slen + 1;
              Font        := cHighlightTheme.Comments.Font;
              Brush.Color := cHighlightTheme.Comments.BgColor;
              goto _ToDraw;
            end;
          end;
          //дополнительные символы/слова
          for i1 := Low(AsmLang.Additional) to High(AsmLang.Additional) do
          begin
            if (i + Length(AsmLang.Additional[i1])) > slen then
              Continue;
            BufStr := Copy(ItemText, i, Length(AsmLang.Additional[i1]));
            if BufStr = AsmLang.Additional[i1] then
            begin
              OutStr      := AsmLang.Additional[i1];
              i           := Length(AsmLang.Additional[i1]) + 1;
              Font        := cHighlightTheme.Additional.Font;
              Brush.Color := cHighlightTheme.Additional.BgColor;
              goto _ToDraw;
            end;
          end;
          OutStr      := ItemText[i];
          Font        := cHighlightTheme.Default.Font;
          Brush.Color := cHighlightTheme.Default.BgColor;
          inc(i);
        end;
      end;
    _ToDraw:
      TextOut(OutPos + INDENT_VALUE, Rect.Top, OutStr);
      OutPos := OutPos + TextWidth(OutStr) + INDENT_VALUE;
    until i > slen;
  end;
end;

//Подсветка HEX
procedure TControlHighlight.HexHighlight(Index: Integer; Rect: TRect);
const
  INDENT_VALUE = 1;
var
  i       : integer;
  slen    : integer;
  OutPos  : integer;
  ItemText: string;
  OutStr  : string;
begin
  if DrawDefault(Index, Rect) then
    Exit;
  ItemText := TrimRight(GetText(Index));
  slen     := Length(ItemText);
  with cCanvas do begin
    Brush.Color := cHighlightTheme.BgColor;
    FillRect(Rect);
    i := 1;
    OutPos := Rect.Left + INDENT_VALUE;
    if (slen <> 0) then
    begin
      OutStr := Copy(ItemText, i, 8);
      Inc(i, 8);
      Font        := cHighlightTheme.HEX.Addresses.Font;
      Brush.Color := cHighlightTheme.HEX.Addresses.BgColor;
      TextOut(OutPos + INDENT_VALUE, Rect.Top, OutStr);
      OutPos := OutPos + TextWidth(OutStr) + INDENT_VALUE;
      OutStr := Copy(ItemText, i, 2);
      Inc(i, 2);
      Font        := cHighlightTheme.Default.Font;
      Brush.Color := cHighlightTheme.Default.BgColor;
      TextOut(OutPos + INDENT_VALUE, Rect.Top, OutStr);
      OutPos := OutPos + TextWidth(OutStr) + INDENT_VALUE;
      while (ItemText[i] <> #32) and (i <= slen) do
      begin
        OutStr := Copy(ItemText, i, 2);
        Inc(i, 2);
        Font        := cHighlightTheme.HEX.Bytes.Font;
        Brush.Color := cHighlightTheme.HEX.Bytes.BgColor;
        TextOut(OutPos + INDENT_VALUE, Rect.Top, OutStr);
        OutPos := OutPos + TextWidth(OutStr) + INDENT_VALUE;
        OutStr := ItemText[i];
        Inc(i, 1);
        Font        := cHighlightTheme.Default.Font;
        Brush.Color := cHighlightTheme.Default.BgColor;
        TextOut(OutPos + INDENT_VALUE, Rect.Top, OutStr);
        OutPos := OutPos + TextWidth(OutStr);
      end;
      if i < slen - 1 then
      begin
        OutStr := ItemText[i];
        Inc(i, 1);
        Font        := cHighlightTheme.Default.Font;
        Brush.Color := cHighlightTheme.Default.BgColor;
        TextOut(OutPos + INDENT_VALUE, Rect.Top, OutStr);
        OutPos := OutPos + TextWidth(OutStr) + INDENT_VALUE;      
        OutStr := Copy(ItemText, i + 2, slen - i - 2);
        Font        := cHighlightTheme.HEX.Chars.Font;
        Brush.Color := cHighlightTheme.HEX.Chars.BgColor;
        TextOut(OutPos + INDENT_VALUE, Rect.Top, OutStr);
      end;
    end;
  end;
end;

//Отрисовка без подсветки
function TControlHighlight.DrawDefault(Index: Integer; Rect: TRect): BOOL;
begin
  with cCanvas do begin
    if (bNoHighlight or cNoHighlight) and (not cPreviewMode) then
    begin
      Font.Name   := 'Fixedsys';
      Font.Style  := [];
      Font.Size   := 8;
      Font.Color  := clWindowText;
      Pen.Color   := clWindowText;
      Pen.Style   := psSolid;
      Brush.Color := clWindow;
      Brush.Style := bsSolid;
      FillRect(Rect);
      TextOut(Rect.Left, Rect.Top, TrimRight(GetText(Index)));
      Result := True;
    end else
      Result := False;
  end;
end;

function TControlHighlight.GetText(Index: Integer): string;
var
  Buffer: array [0..4095] of AnsiChar;
begin
  SendMessage(cControlHandle, LB_GETTEXT, Index, Integer(@Buffer));
  Result := StrPas(Buffer);
end;

//-------------------------------------------------------------------------------------------
//PRIVATE

procedure TControlHighlight.SetControlHandle(const Value: HWND);
begin
  if (GetDC(cControlHandle) <> 0) then begin
    cControlHandle := Value;
    cNoHighlight   := bNoHighlight;
  end else begin
    cControlHandle := 0;
    cNoHighlight   := True;
  end;
  cCanvas.Handle := GetDC(cControlHandle);
end;

procedure TControlHighlight.SetControlHighlightLanguage(const Value: DWORD);
begin
  case Value of
    ID_ASM    : cHighlightLang := tlAsm;
    ID_HEX    : cHighlightLang := tlHex;
    ID_DELPHI : cHighlightLang := tlDelphi;
    else
      cHighlightLang := tlUnknown;
  end;
  if cHighlightLang <> tlUnknown then
  begin
    cCanvas.Handle := GetDC(cControlHandle);
    cThemeId       := GetThemeId(cHighlightLang);
    Update;
  end;
end;

procedure TControlHighlight.SetControlHighlightTheme(const Value: DWORD);
begin
  if SetThemeId(cHighlightLang, Value) then
  begin
    cThemeId := Value;
    Update;
  end else
    cNoHighlight := True;
end;

function TControlHighlight.GetControlHighlightLanguage: DWORD;
begin
  case cHighlightLang of
    tlAsm    : Result := ID_ASM;
    tlHex    : Result := ID_HEX;
    tlDelphi : Result := ID_DELPHI;
    else
      Result := ID_UNKNOWN;
  end;
end;

function TControlHighlight.GetControlHighlightTheme: DWORD;
begin
  Result := cThemeId;
end;

//-------------------------------------------------------------------------------------------
//PUBLIC

constructor TControlHighlight.Create;
begin
  cCanvas := TCanvas.Create;
  with cHighlightTheme do begin
    Default.Font    := TFont.Create;
    Comments.Font   := TFont.Create;
    Numbers.Font    := TFont.Create;
    Strings.Font    := TFont.Create;
    Additional.Font := TFont.Create;
    HEX.Addresses.Font := TFont.Create;
    HEX.Bytes.Font     := TFont.Create;
    HEX.Chars.Font     := TFont.Create;
  end;
  cHighlightLang := tlUnknown;
  cPreviewMode   := False;
  cControlHandle := 0;
  cNoHighlight   := bNoHighlight;
  cThemeId       := GetThemeId(cHighlightLang);
end;

destructor TControlHighlight.Destroy;
var
  i: integer;
begin
  with cHighlightTheme do begin
    Default.Font.Free;
    Comments.Font.Free;
    Numbers.Font.Free;
    for i := Low(KeyWords) to High(KeyWords) do
      KeyWords[i].Font.Free;
    Finalize(KeyWords);
    Strings.Font.Free;
    Additional.Font.Free;
    HEX.Addresses.Font.Free;
    HEX.Bytes.Font.Free;
    HEX.Chars.Font.Free;
  end;
  cCanvas.Free;
  inherited;
end;

procedure TControlHighlight.OnDrawItem(Index: Integer; Rect: TRect);
begin
  if cControlHandle <> 0 then
    case cHighlightLang of
      tlAsm    : AsmHighlight(Index, Rect);
      tlHex    : HexHighlight(Index, Rect);
      tlDelphi : DelphiHighlight(Index, Rect);
      else begin
        cNoHighlight := True;
        DrawDefault(Index, Rect);
      end;
    end;
end;

procedure TControlHighlight.Update;
begin
  cNoHighlight := not LoadTheme(cHighlightLang, cThemeId, True, @cHighlightTheme);
  Redraw;
end;

procedure TControlHighlight.ChangeCustomTheme(CustomTheme: TTheme);
begin
  cHighlightTheme := CustomTheme;
  Update;
end;

procedure TControlHighlight.ChangeCustomThemeEx(CustomTheme: TTheme);
begin
  cHighlightTheme := CustomTheme;
  Redraw;
end;

procedure TControlHighlight.Redraw;
begin
  InvalidateRect(cControlHandle, nil, True);
end;

procedure TControlHighlight.SetPreviewMode(const Value: BOOL);
begin
  cPreviewMode := Value;
  Redraw;
end;

end.
