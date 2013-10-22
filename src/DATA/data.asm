 StdCtrls::TListBoxStrings.Add
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
