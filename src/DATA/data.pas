function TListBoxStrings.Add(S: String):Integer;
begin
  //00427F18
  _ecx_ := 0;
  lvar_8 := _ecx_;
  lvar_4 := S;
  _ebx_ := Self;
  try
    //00427F39
    _esi_ := -1;
    _edi_ := ListBox;
    _eax_ := ListBox.FStyle;
    _eax_ := _eax_ + $FD{253};//ListBox.FStyle + $FD{253}
    _eax_ := _eax_ - 2;//ListBox.FStyle + $FD{253} - 2
    if (_eax_ >= 0) then
    begin
      //00427F4B
      _eax_ := S
      _eax_ := PChar(S);
      _eax_ := _edi_;//ListBox
      _eax_ := ListBox.GetHandle;
      _eax_ := SendMessageA(:-), :-), :-), :-));
      _esi_ := SendMessageA(:-), :-), :-), :-));
      if (_esi_ < 0) then
      begin
        //00427F6E
        lvar_8 := LoadResString(:-));
        _ecx_ := lvar_8
        _edx_ := 1;
        _eax_ := EOutOfResources.Create(:-));
        raise EOutOfResources.Create(:-));
      end;
    end;
  finally
    //00427F9C
    lvar_8 := 'string';
  end;
  _eax_ := _esi_;
end;
