unit uResIniFile;

interface

uses
  Windows, Classes, SysUtils;

type
  TResIniFile = class
  protected
    MemFile : TStringList;
  public
    constructor Create(ResType: string; ResName: string);
    destructor  Destroy; override;
    function ReadString(const Section, Ident: string; const Default: string = ''): string;
    function ReadStringEx(const Section, Ident: String;
                          var szResult: string; const Default: string = ''): bool;
  end;

implementation

{ TResIniFile }

constructor TResIniFile.Create(ResType, ResName: string);
var
  i  : integer;
	Res: TResourceStream;
begin
  MemFile := TStringList.Create;
  if (FindResource(Hinstance, PAnsiChar(ResName), PAnsiChar(ResType)) <> 0) then
  begin
	  Res := TResourceStream.Create(Hinstance, Resname, PChar(ResType));
	  try
      MemFile.LoadFromStream(Res);
	  finally
		  Res.Free;
	  end;
  end;
  i := 0;
  while i < MemFile.Count do
  begin
    MemFile[i] := Trim(MemFile[i]);
    if Length(MemFile[i]) > 0 then
    begin
      if MemFile[i][1] = ';' then
        MemFile.Delete(i)
      else
        Inc(i);
    end else
      MemFile.Delete(i);
  end;
end;

destructor TResIniFile.Destroy;
begin
  MemFile.Free;
  inherited;
end;

function TResIniFile.ReadStringEx(const Section, Ident: String;
  var szResult: string; const Default: string = ''): bool;
var
  i, sect, posit : integer;
begin
  Result := False;
  if MemFile.Count > 0 then
  begin
    sect := MemFile.IndexOf('[' + Section + ']');
    if (sect >= 0) and (sect < Pred(MemFile.Count)) then
    begin
      for i := sect + 1 to Pred(MemFile.Count) do
      begin
        if MemFile[i][1] = '[' then
          Break;
        posit := Pos('=', MemFile[i]);
        if UpperCase(Trim(Copy(MemFile[i], 1, posit - 1))) = UpperCase(Ident) then
        begin
          szResult := Trim(Copy(MemFile[i], posit + 1, Length(MemFile[i]) - posit));
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
  szResult := Default;
end;

function TResIniFile.ReadString(const Section, Ident: string;
  const Default: string = ''): string;
var
  i, sect, posit : integer;
begin
  if MemFile.Count > 0 then
  begin
    sect := MemFile.IndexOf('[' + Section + ']');
    if (sect >= 0) and (sect < Pred(MemFile.Count)) then
    begin
      for i := sect + 1 to Pred(MemFile.Count) do
      begin
        if MemFile[i][1] = '[' then
          Break;
        posit := Pos('=', MemFile[i]);
        if UpperCase(Trim(Copy(MemFile[i], 1, posit - 1))) = UpperCase(Ident) then
        begin
          Result := Trim(Copy(MemFile[i], posit + 1, Length(MemFile[i]) - posit));
          Exit;
        end;
      end;
    end;
  end;
  Result := Default;
end;

end.
