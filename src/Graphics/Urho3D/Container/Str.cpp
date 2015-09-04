
#include <Urho3D/Container/Str.h>

using namespace Urho3D;

extern "C" {
const char * inline_c_Graphics_Urho3D_Container_Str_0_30e64941d126255871de6f8a6a0331c2cb9c1a7d(String * ptr_inline_c_0) {
return ( ptr_inline_c_0->CString() );
}

}

extern "C" {
WString * inline_c_Graphics_Urho3D_Container_Str_1_bf58e9ca1194f97a155b2c45618f0b8cb85c62b6(String * ptr_inline_c_0) {
return ( new WString(*ptr_inline_c_0) );
}

}

extern "C" {
const wchar_t * inline_c_Graphics_Urho3D_Container_Str_2_1cbb15ddf6587f87606f719ce63ce0b1b17d5c67(WString * wstr_inline_c_0) {
return ( wstr_inline_c_0->CString() );
}

}

extern "C" {
WString * inline_c_Graphics_Urho3D_Container_Str_3_bfd9d0e4a12fac5b5182708e75eee392ce97049d(const wchar_t * s_27_inline_c_0) {
return ( new WString(s_27_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Container_Str_4_83946c9f41ba1486258cb8640dfe9933b0f9fc31(WString * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

extern "C" {
String * inline_c_Graphics_Urho3D_Container_Str_5_09be2b3b1a54835d42fb40eb08ac447e4967fd4a(const char * s_27_inline_c_0) {
return ( new String(s_27_inline_c_0) );
}

}

extern "C" {
String * inline_c_Graphics_Urho3D_Container_Str_6_4a00ee0062b90705259387a65363674a7651e564(const wchar_t * s_27_inline_c_0) {
return ( new String(s_27_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Container_Str_7_a3edaa805ad5cf00c51301aad304dd7fc3debc9a(String * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}
