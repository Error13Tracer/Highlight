        ��  ��                  ~  0   E X A M P L E   A S M       0          StdCtrls::TListBoxStrings.Add
 00427F18    push       ebp
 00427F19    mov        ebp,esp
 00427F1B    add        esp,0FFFFFFF8
 00427F1E    push       ebx
 00427F1F    push       esi
 00427F20    push       edi
 00427F21    xor        ecx,ecx
 00427F23    mov        dword ptr [ebp-8],ecx
 00427F26    mov        dword ptr [ebp-4],edx
 00427F29    mov        ebx,eax
 00427F2B    xor        eax,eax
 00427F2D    push       ebp
 00427F2E    push       427FA5
 00427F33    push       dword ptr fs:[eax]
 00427F36    mov        dword ptr fs:[eax],esp
 00427F39    or         esi,0FFFFFFFF
 00427F3C    mov        edi,dword ptr [ebx+10]
 00427F3F    mov        al,byte ptr [edi+238]
 00427F45    add        al,0FD
 00427F47    sub        al,2
>00427F49    jb         00427F8F
 00427F4B    mov        eax,dword ptr [ebp-4]
 00427F4E    call       @LStrToPChar
 00427F53    push       eax
 00427F54    push       0
 00427F56    push       180
 00427F5B    mov        eax,edi
 00427F5D    call       TWinControl.GetHandle
 00427F62    push       eax
 00427F63    call       user32.SendMessageA
 00427F68    mov        esi,eax
 00427F6A    test       esi,esi
>00427F6C    jge        00427F8F
 00427F6E    lea        edx,[ebp-8]
 00427F71    mov        eax,[00452E70]; SInsertLineError:System.AnsiString
 00427F76    call       LoadResString
 00427F7B    mov        ecx,dword ptr [ebp-8]
 00427F7E    mov        dl,1
 00427F80    mov        eax,[004130A4]; EOutOfResources
 00427F85    call       Exception.Create
 00427F8A    call       @RaiseExcept
 00427F8F    xor        eax,eax
 00427F91    pop        edx
 00427F92    pop        ecx
 00427F93    pop        ecx
 00427F94    mov        dword ptr fs:[eax],edx
 00427F97    push       427FAC
 00427F9C    lea        eax,[ebp-8]
 00427F9F    call       @LStrClr
 00427FA4    ret
<00427FA5    jmp        @HandleFinally
<00427FAA    jmp        00427F9C
 00427FAC    mov        eax,esi
 00427FAE    pop        edi
 00427FAF    pop        esi
 00427FB0    pop        ebx
 00427FB1    pop        ecx
 00427FB2    pop        ecx
 00427FB3    pop        ebp
 00427FB4    ret
  >  0   E X A M P L E   H E X       0         00454EA8  FF 25 48 97 45 00 8B C0 FF 25 44 97 45 00 8B C0  �%H�E ���%D�E ��
00454EB8  FF 25 40 97 45 00 8B C0 FF 25 3C 97 45 00 8B C0  �%@�E ���%<�E ��
00454EC8  55 8B EC 33 C0 55 68 ED 4E 45 00 64 FF 30 64 89  U��3�Uh�NE d�0d�
00454ED8  20 FF 05 64 8B 45 00 33 C0 5A 59 59 64 89 10 68   � d�E 3�ZYYd� h
00454EE8  F4 4E 45 00 C3 E9 F6 F5 FA FF EB F8 5D C3 8B C0  �NE ��������]Ë�
00454EF8  83 2D 64 8B 45 00 01 C3 4C 4F 45 00 00 00 00 00  �-d�E  �LOE     
00454F08  00 00 00 00 00 00 00 00 9C 51 45 00 40 50 45 00          �QE @PE 
00454F18  D8 50 45 00 00 00 00 00 7C 51 45 00 20 03 00 00  �PE     |QE     
00454F28  20 95 44 00 10 B8 41 00 68 C1 44 00 1C C3 44 00   �D  �A h�D  �D 
00454F38  F0 3F 40 00 B0 E7 44 00 30 3D 40 00 4C 3D 40 00  �?@ ��D 0=@ L=@ 
00454F48  84 C3 44 00 DC B2 43 00 04 C9 44 00 E8 5B 41 00  ��D ܲC  �D �[A 
00454F58  44 C5 44 00 80 C5 44 00 8C C6 44 00 C0 3D 43 00  D�D ��D ��D �=C 
00454F68  28 B8 41 00 2C CF 44 00 F8 B4 41 00 D4 0A 45 00  (�A ,�D ��A � E 
00454F78  DC BF 44 00 F0 AD 43 00 0C B3 43 00 58 B2 43 00  ܿD �C  �C X�C 
00454F88  1C 33 43 00 B0 A8 43 00 0C CC 44 00 E8 A5 43 00   3C ��C  �D �C 
00454F98  64 32 43 00 68 32 43 00 00 CD 44 00 A4 62 43 00  d2C h2C  �D �bC 
00454FA8  A8 3D 43 00 E0 33 43 00 34 3F 43 00 F4 CE 44 00  �=C �3C 4?C ��D 
00454FB8  C0 CD 44 00 6C 40 43 00 6C CF 44 00 28 63 43 00  ��D l@C l�D (cC 
00454FC8  3C A6 43 00 F8 A6 43 00 F4 A1 43 00 DC A6 43 00  <�C ��C ��C ܦC 
00454FD8  DC BE 44 00 10 D6 44 00 80 7C 43 00 D4 E2 44 00  ܾD  �D �|C ��D 
00454FE8  34 E6 44 00 F0 E4 44 00 18 7D 43 00 1C 7D 43 00  4�D ��D  }C  }C 
00454FF8  6C E7 44 00 7C 7B 43 00 1C A8 43 00 68 EE 44 00  l�D |{C  �C h�D 
00455008  AC B9 43 00 98 A4 43 00 7C 03 45 00 D0 B8 44 00  ��C ��C | E иD 
00455018  80 BB 44 00 5C F4 44 00 68 C4 44 00 E4 C4 44 00  ��D \�D h�D ��D 
00455028  5C 07 45 00 A0 C1 44 00 60 02 45 00 38 E9 44 00  \ E ��D ` E 8�D 
00455038  14 04 45 00 F0 CE 44 00 0A 00 83 51 45 00 F8 02    E ��D   �QE � 
00455048  00 00 00 00 08 4C 69 73 74 42 6F 78 31 FC 02 00       ListBox1�  
00455058  00 00 00 08 4C 69 73 74 42 6F 78 32 00 03 00 00      ListBox2    
00455068  01 00 06 50 61 6E 65 6C 31 04 03 00 00 02 00 07     Panel1       
00455078  42 75 74 74 6F 6E 31 08 03 00 00 02 00 07 42 75  Button1       Bu
00455088  74 74 6F 6E 32 0C 03 00 00 02 00 07 42 75 74 74  tton2       Butt
00455098  6F 6E 33 10 03 00 00 02 00 07 42 75 74 74 6F 6E  on3       Button
004550A8  34 14 03 00 00 00 00 08 4C 69 73 74 42 6F 78 33  4       ListBox3
004550B8  18 03 00 00 03 00 09 53 70 6C 69 74 74 65 72 31         Splitter1
004550C8  1C 03 00 00 03 00 09 53 70 6C 69 74 74 65 72 32         Splitter2
004550D8  08 00 11 00 B8 51 45 00 0A 46 6F 72 6D 43 72 65      �QE  FormCre
004550E8  61 74 65 17 00 34 53 45 00 10 4C 69 73 74 42 6F  ate  4SE  ListBo
004550F8  78 32 44 72 61 77 49 74 65 6D 17 00 68 53 45 00  x2DrawItem  hSE 
00455108  10 4C 69 73 74 42 6F 78 31 44 72 61 77 49 74 65   ListBox1DrawIte
00455118  6D 13 00 9C 53 45 00 0C 42 75 74 74 6F 6E 31 43  m  �SE  Button1C
00455128  6C 69 63 6B 13 00 38 54 45 00 0C 42 75 74 74 6F  lick  8TE  Butto
00455138  6E 32 43 6C 69 63 6B 13 00 50 54 45 00 0C 42 75  n2Click  PTE  Bu
00455148  74 74 6F 6E 33 43 6C 69 63 6B 13 00 5C 54 45 00  tton3Click  \TE 
00455158  0C 42 75 74 74 6F 6E 34 43 6C 69 63 6B 17 00 04   Button4Click   
00455168  55 45 00 10 4C 69 73 74 42 6F 78 33 44 72 61 77  UE  ListBox3Draw
00455178  49 74 65 6D 06 54 46 6F 72 6D 31 04 00 B4 7C 42  Item TForm1  �|B
00455188  00 00 AF 42 00 34 72 42 00 D8 B8 42 00 8D 40 00    �B 4rB ظB �@ 
00455198  9C 51 45 00 07 06 54 46 6F 72 6D 31 4C 4F 45 00  �QE   TForm1LOE 
004551A8  68 96 44 00 5E 00 05 75 54 65 73 74 00 00 8B C0  h�D ^  uTest  ��
004551B8  55 8B EC 33 C9 51 51 51 51 51 51 56 8B F0 33 C0  U��3�QQQQQQV��3�
004551C8  55 68 DE 52 45 00 64 FF 30 64 89 20 E8 C7 DC FA  Uh�RE d�0d� ����
004551D8  FF 6A 01 8B 86 F8 02 00 00 E8 22 56 FE FF 50 E8  �j ���   �"V��P�
004551E8  B4 FC FF FF A3 6C 8B 45 00 6A 00 8B 86 FC 02 00  �����l�E j ���  
004551F8  00 E8 0A 56 FE FF 50 E8 9C FC FF FF A3 70 8B 45   � V��P�����p�E
00455208  00 6A 02 8B 86 14 03 00 00 E8 F2 55 FE FF 50 E8   j ��    ��U��P�
00455218  84 FC FF FF A3 74 8B 45 00 8D 55 F8 33 C0 E8 11  �����t�E �U�3�� 
00455228  DC FA FF 8B 45 F8 8D 55 FC E8 CA 4C FB FF 8D 45  ����E��U���L���E
00455238  FC BA F4 52 45 00 E8 91 FB FA FF 8B 55 FC 8B 86  ���RE �����U���
00455248  F8 02 00 00 8B 80 18 02 00 00 8B 08 FF 51 68 8D  �   ��    � �Qh�
00455258  55 F0 33 C0 E8 DB DB FA FF 8B 45 F0 8D 55 F4 E8  U�3�������E��U��
00455268  94 4C FB FF 8D 45 F4 BA 0C 53 45 00 E8 5B FB FA  �L���E�� SE �[��
00455278  FF 8B 55 F4 8B 86 FC 02 00 00 8B 80 18 02 00 00  ��U��   ��    
00455288  8B 08 FF 51 68 8D 55 E8 33 C0 E8 A5 DB FA FF 8B  � �Qh�U�3������
00455298  45 E8 8D 55 EC E8 5E 4C FB FF 8D 45 EC BA 24 53  E�U��^L���E�$S  �  8   E X A M P L E   D E L P H I         0         function TListBoxStrings.Add(S: String):Integer;
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
  