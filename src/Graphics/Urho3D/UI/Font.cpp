
#include <Urho3D/UI/Font.h>

using namespace Urho3D;

extern "C" {
StringHash * inline_c_Graphics_Urho3D_UI_Font_0_08dd36f555929cd647c4fa40a13ca9c72a2c24fb() {
 
    static StringHash h = Font::GetTypeStatic(); 
    return &h; 
    
}

}

extern "C" {
Font * inline_c_Graphics_Urho3D_UI_Font_1_67a9912751281f8e0eeaccab64dcb5aeff68dafe(Context * ptr_inline_c_0) {
return ( new Font( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Font_2_b201450849c4d31d29b6f248338a544f49fbe586(Font * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<Font> SharedFont;

extern "C" {
void inline_c_Graphics_Urho3D_UI_Font_3_8d235930192cdb1340e4f38ff355992a29782462(SharedFont * ptr_inline_c_0, SharedFont * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedFont * inline_c_Graphics_Urho3D_UI_Font_4_e38d69516a7706766f298b18a4f29909928a891a(Font * ptr_inline_c_0) {
return ( new SharedFont(ptr_inline_c_0) );
}

}

extern "C" {
Font * inline_c_Graphics_Urho3D_UI_Font_5_5d114e87cd774d1a99b3c4fcfd28e44cf023fc05(SharedFont * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}
