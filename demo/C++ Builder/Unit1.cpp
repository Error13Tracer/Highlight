//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "Highlight.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
#pragma resource "data.res"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
DWORD AsmLbId;
DWORD HexLbId;
DWORD DelphiLbId;
int DelphiThemesCount;
int AsmThemesCount;
int HexThemesCount;
TMenuItem *m1[10];
TMenuItem *m2[10];
TMenuItem *m3[10];

void __fastcall TForm1::ListBox1DrawItem(TWinControl *Control, int Index, TRect &Rect,
          TOwnerDrawState State)
{
	HighlightDrawItem(DelphiLbId, Index, Rect, (rand() % 2 == 0 ? true : false));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ListBox2DrawItem(TWinControl *Control, int Index, TRect &Rect,
          TOwnerDrawState State)
{
	HighlightDrawItem(AsmLbId, Index, Rect, (rand() % 2 == 0 ? true : false));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ListBox3DrawItem(TWinControl *Control, int Index, TRect &Rect,
          TOwnerDrawState State)
{
	HighlightDrawItem(HexLbId, Index, Rect, (rand() % 2 == 0 ? true : false));
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Redraw1Click(TObject *Sender)
{
	HighlightRedraw(DelphiLbId);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Redraw2Click(TObject *Sender)
{
	HighlightRedraw(AsmLbId);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Redraw3Click(TObject *Sender)
{
	HighlightRedraw(HexLbId);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::LoadFromStream(TListBox *ListBox, char *Lang)
{
	TResourceStream *RS = new TResourceStream((int)HInstance, UnicodeString(Lang), L"EXAMPLE");
	ListBox->Items->Clear();
	ListBox->Items->LoadFromStream(RS);
	delete RS;
};

void __fastcall TForm1::FormCreate(TObject *Sender)
{

	if (!InitHighlight()){
      	MessageBoxA(NULL, "Highlight.dll not found", "Error", MB_ICONERROR);
		ExitProcess(NULL);
	};
	randomize;
	ListBox1->Style = lbOwnerDrawFixed;
	ListBox2->Style = lbOwnerDrawFixed;
	ListBox3->Style = lbOwnerDrawFixed;
	DelphiLbId = CreateHighlight((HWND)ListBox1->Handle, (DWORD)ID_DELPHI);
	AsmLbId    = CreateHighlight((HWND)ListBox2->Handle, (DWORD)ID_ASM);
	HexLbId    = CreateHighlight((HWND)ListBox3->Handle, (DWORD)ID_HEX);
	LoadFromStream(ListBox1, "DELPHI");
	LoadFromStream(ListBox2, "ASM");
	LoadFromStream(ListBox3, "HEX");
	DelphiThemesCount = GetThemesCount(ID_DELPHI);
	AsmThemesCount    = GetThemesCount(ID_ASM);
	HexThemesCount    = GetThemesCount(ID_HEX);
	LPCSTR buf[256];
	int i = 0;
	if (DelphiThemesCount > 0) {
		for (i = 0; i < DelphiThemesCount; i++){
			m1[i] = new TMenuItem(PopupMenu1->Items);
			GetThemeName(ID_DELPHI, (DWORD)i,(LPCSTR)buf, 256);
			m1[i]->Caption = UnicodeString((char *)buf);
			m1[i]->OnClick = OnClickM1;
			PopupMenu1->Items->Add(m1[i]);
		}
	}
	if (AsmThemesCount > 0) {
		for (i = 0; i < AsmThemesCount; i++){
			m2[i] = new TMenuItem(PopupMenu2->Items);
			GetThemeName(ID_ASM, (DWORD)i,(LPCSTR)buf, 256);
			m2[i]->Caption = UnicodeString((char *)buf);
			m2[i]->OnClick = OnClickM2;
			PopupMenu2->Items->Add(m2[i]);
		}
	}
	if (HexThemesCount > 0) {
		for (i = 0; i < HexThemesCount; i++){
			m3[i] = new TMenuItem(PopupMenu3->Items);
			GetThemeName(ID_HEX, (DWORD)i,(LPCSTR)buf, 256);
			m3[i]->Caption = UnicodeString((char *)buf);
			m3[i]->OnClick = OnClickM3;
			PopupMenu3->Items->Add(m3[i]);
		}
	}
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormDestroy(TObject *Sender)
{

	int i = 0;
	if (DelphiThemesCount > 0) {
		for (i = 0; i < DelphiThemesCount; i++){
			FreeAndNil(m1);
		}
	}
	if (AsmThemesCount > 0) {
		for (i = 0; i < AsmThemesCount; i++){
			FreeAndNil(m2);
		}
	}
	if (HexThemesCount > 0) {
		for (i = 0; i < HexThemesCount; i++){
			FreeAndNil(m3);
		}
	}
	FreeHighlight();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::OnClickM1(TObject *Sender)
{
	if (DelphiThemesCount > 0) {
		for (int i = 0; i < DelphiThemesCount; i++){
			if ((Sender) == m1[i]){
				//ChangeGlobalTheme(ID_DELPHI, i);
				ChangeTheme(DelphiLbId, i);
			}
		}
	}
}

void __fastcall TForm1::OnClickM2(TObject *Sender)
{
	if (AsmThemesCount > 0) {
		for (int i = 0; i < AsmThemesCount; i++){
			if ((Sender) == m2[i]){
				//ChangeGlobalTheme(ID_ASM, i);
				ChangeTheme(AsmLbId, i);
			}
		}
	}
}

void __fastcall TForm1::OnClickM3(TObject *Sender)
{
	if (HexThemesCount > 0) {
		for (int i = 0; i < HexThemesCount; i++){
			if ((Sender) == m3[i]){
				//ChangeGlobalTheme(ID_HEX, i);
				ChangeTheme(HexLbId, i);
			}
		}
	}
}

void __fastcall TForm1::Button1Click(TObject *Sender)
{
	int ModalResult = SettingsShowModal(Handle);
}
//---------------------------------------------------------------------------

