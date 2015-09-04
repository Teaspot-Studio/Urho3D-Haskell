
#include <Urho3D/UI/Text.h>

using namespace Urho3D;

extern "C" {
StringHash * inline_c_Graphics_Urho3D_UI_Text_0_7c8c43abe66a6bbaeb9a64240966fca8f09f4163() {
 
      static StringHash h = Text::GetTypeStatic();  
      return &h;
    
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Text_1_bcd2479b9bc4b89a1ea8721886a8583b79064866(Text * ptr_inline_c_0) {
return ((UIElement*)ptr_inline_c_0);
}

}

extern "C" {
Text * inline_c_Graphics_Urho3D_UI_Text_2_58d14ec066648a5a80addf4d1a5d913bd18262f7(UIElement * ptr_inline_c_0) {
return ((Text*)ptr_inline_c_0);
}

}

extern "C" {
Text * inline_c_Graphics_Urho3D_UI_Text_3_20c2e5921d0e43b5a33ce77f509c38be82a5dd17(Context * ptr_inline_c_0) {
return ( new Text( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Text_4_e72bf82fda6d4da13c01214b0fed214d39ca302e(Text * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<Text> SharedText;

extern "C" {
void inline_c_Graphics_Urho3D_UI_Text_5_678c40dec995beca6827f9dcdf39171c9e72adb6(SharedText * ptr_inline_c_0, SharedText * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedText * inline_c_Graphics_Urho3D_UI_Text_6_3a140706b2e21ba89402ae600847ee930e23b4c7(Text * ptr_inline_c_0) {
return ( new SharedText(ptr_inline_c_0) );
}

}

extern "C" {
Text * inline_c_Graphics_Urho3D_UI_Text_7_db5d6b949c9fe2148afaa47749b4af3dcabefad6(SharedText * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Text_8_dd7339520692a9462ba59f656ee874211f13a10c(Text * ptr_inline_c_0, wchar_t * str_27_inline_c_1) {
 ptr_inline_c_0->SetText(String(str_27_inline_c_1)) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Text_9_73566a7a0788a4a54847b9fe4fadae0bd6fe4d0e(Text * ptrText_inline_c_0, Font * ptrFont_inline_c_1, int fsize_27_inline_c_2) {
 ptrText_inline_c_0->SetFont(ptrFont_inline_c_1, fsize_27_inline_c_2) ;
}

}
