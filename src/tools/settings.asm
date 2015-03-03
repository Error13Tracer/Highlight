.386
.model flat,stdcall
option casemap:none
include \masm32\include\windows.inc
include \masm32\include\user32.inc
include \masm32\include\kernel32.inc
includelib \masm32\lib\kernel32.lib
includelib \masm32\lib\user32.lib

.data?
hLib dd ?               
SettingsShowModal dd ?

.code
start:
  jmp @_start_
 @LibName:
  db "highlight.dll", 0
 @FunctionName: 
  db "SettingsShowModal@4", 0
 @DllNotFound: 
  db "Cannot load library", 0
 @AppName: 
  db "Highlight settings", 0
 @FunctionNotFound: 
  db "Function 'SettingsShowModal@4' not found", 0
 @_start_:
  invoke LoadLibrary, addr @LibName
  .if eax == NULL
    invoke MessageBox, NULL, addr @DllNotFound, addr @AppName, MB_OK
  .else
    mov hLib,eax
    invoke GetProcAddress, hLib, addr @FunctionName
    .if eax==NULL
      invoke MessageBox, NULL, addr @FunctionNotFound, addr @AppName, MB_OK
    .else
      mov SettingsShowModal, eax
      push 0
      call [SettingsShowModal]
    .endif
    invoke FreeLibrary, hLib
  .endif
  invoke ExitProcess, NULL
end start
