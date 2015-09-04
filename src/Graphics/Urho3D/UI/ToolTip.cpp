
#include <Urho3D/UI/ToolTip.h>

using namespace Urho3D;

extern "C" {
StringHash * inline_c_Graphics_Urho3D_UI_ToolTip_0_b1d6bb887e9d795247c7f11820dc97dcf3f36aef() {
 
      static StringHash h = ToolTip::GetTypeStatic();  
      return &h;
    
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_ToolTip_1_97dfc6048e556ed22ff914d30ad0f044d6cf6a90(ToolTip * ptr_inline_c_0) {
return ((UIElement*)ptr_inline_c_0);
}

}

extern "C" {
ToolTip * inline_c_Graphics_Urho3D_UI_ToolTip_2_7bd5c77ff7864f4bcfca3f5d1157dfba2dd13244(UIElement * ptr_inline_c_0) {
return ((ToolTip*)ptr_inline_c_0);
}

}

extern "C" {
ToolTip * inline_c_Graphics_Urho3D_UI_ToolTip_3_b9538d25963beb860c900a6ed910ea1b1030c244(Context * ptr_inline_c_0) {
return ( new ToolTip( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_ToolTip_4_25bc3d38cad8a41417d05617d482d0b54e9ec6b6(ToolTip * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<ToolTip> SharedToolTip;

extern "C" {
void inline_c_Graphics_Urho3D_UI_ToolTip_5_df48fcba52e43bbce9a3e16fd0e23e22d587f204(SharedToolTip * ptr_inline_c_0, SharedToolTip * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedToolTip * inline_c_Graphics_Urho3D_UI_ToolTip_6_e162b670acb9cd62fb37122b3a347d99854e1878(ToolTip * ptr_inline_c_0) {
return ( new SharedToolTip(ptr_inline_c_0) );
}

}

extern "C" {
ToolTip * inline_c_Graphics_Urho3D_UI_ToolTip_7_d2500888df006de8b2a982534fba5a320e7e75a9(SharedToolTip * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}
