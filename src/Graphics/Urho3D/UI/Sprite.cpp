
#include <Urho3D/UI/Sprite.h>

using namespace Urho3D;

extern "C" {
Sprite * inline_c_Graphics_Urho3D_UI_Sprite_0_9eb3950e89fa04c9d7bbdcd2c00ea40078daef92(Context * ptr_inline_c_0) {
return ( new Sprite( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_1_d14ba6e944a3cfb0c91d37312b26f5bfb06ca90d(Sprite * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

extern "C" {
StringHash * inline_c_Graphics_Urho3D_UI_Sprite_2_855e648c1feca62ae761f0633962ec1106191937() {
 
      static StringHash h = Sprite::GetTypeStatic();  
      return &h;
    
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Sprite_3_7720be01aed9d30906f8dca32edea44c487d0ed1(Sprite * ptr_inline_c_0) {
return ((UIElement*)ptr_inline_c_0);
}

}

extern "C" {
Sprite * inline_c_Graphics_Urho3D_UI_Sprite_4_c42a473890ef2f8fe73bbce062756b1714cf59c5(UIElement * ptr_inline_c_0) {
return ((Sprite*)ptr_inline_c_0);
}

}

typedef SharedPtr<Sprite> SharedSprite;

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_5_1e4c6e2053c6c05b3729292fe21d5dd5f9f04d1c(SharedSprite * ptr_inline_c_0, SharedSprite * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedSprite * inline_c_Graphics_Urho3D_UI_Sprite_6_7bb23e0eb55d94f2aeb94f4412bdd4a38695fb90(Sprite * ptr_inline_c_0) {
return ( new SharedSprite(ptr_inline_c_0) );
}

}

extern "C" {
Sprite * inline_c_Graphics_Urho3D_UI_Sprite_7_e12995ec8cf10d19f24bf71e5be05113549221d2(SharedSprite * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_8_720f425309ec53f30f8cb98d6f366285c848af58(Sprite * ptr_inline_c_0, Vector2 * v_27_inline_c_1) {
ptr_inline_c_0->SetPosition(*v_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_9_e97d969e328401468dfed1edc28c9ad8d5a2180a(Sprite * ptr_inline_c_0, float xv_27_inline_c_1, float yv_27_inline_c_2) {
ptr_inline_c_0->SetPosition(xv_27_inline_c_1, yv_27_inline_c_2);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_10_56424e5c8f45d47ff63634b39d99997f0fe1d673(Sprite * ptr_inline_c_0, IntVector2 * v_27_inline_c_1) {
ptr_inline_c_0->SetHotSpot(*v_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_11_bbfccb962c42fe7502e910c4c06f30e6dda0ce48(Sprite * ptr_27_inline_c_0, int x_27_inline_c_1, int y_27_inline_c_2) {
 ptr_27_inline_c_0->SetHotSpot(x_27_inline_c_1, y_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_12_952c65522b0c3c96fafc2d54603290f0f21f2d21(Sprite * ptr_inline_c_0, Vector2 * v_27_inline_c_1) {
ptr_inline_c_0->SetScale(*v_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_13_0892a470e328dede98950d0c26f05bccf6013e05(Sprite * ptr_inline_c_0, float xv_27_inline_c_1, float yv_27_inline_c_2) {
ptr_inline_c_0->SetScale(xv_27_inline_c_1, yv_27_inline_c_2);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_14_e6fed04d3e83a3dcc82c8adacb2a33789238d11b(Sprite * ptr_inline_c_0, float v_27_inline_c_1) {
ptr_inline_c_0->SetRotation(v_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_15_373fcbb62575cff3e65667b69a159ef5f8ef50b9(Sprite * ptr_27_inline_c_0, Texture * tex_27_inline_c_1) {
 ptr_27_inline_c_0->SetTexture(tex_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_16_8f2835749bf8cf66d4e588c1a5a1aeccaf7b0d4e(Sprite * ptr_inline_c_0, IntRect * r_27_inline_c_1) {
ptr_inline_c_0->SetImageRect(*r_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_17_a825fd13ed4ff3e06204d99a400400a089355b51(Sprite * ptr_inline_c_0) {
ptr_inline_c_0->SetFullImageRect();
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_18_42849536d5685b9219d38ba6db01057ff320c96d(Sprite * ptr_inline_c_0, int bm_27_inline_c_1) {
ptr_inline_c_0->SetBlendMode((BlendMode)bm_27_inline_c_1);
}

}

extern "C" {
const Vector2 * inline_c_Graphics_Urho3D_UI_Sprite_19_2fe549580f1338ab935bd94cea78324cfddb0c5b(Sprite * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetPosition() );
}

}

extern "C" {
const IntVector2 * inline_c_Graphics_Urho3D_UI_Sprite_20_3c60a85d06f3737a58ce3bb9743248ee525c08e7(Sprite * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetHotSpot() );
}

}

extern "C" {
const Vector2 * inline_c_Graphics_Urho3D_UI_Sprite_21_99859e78fa858a4bf4bff7d4efbf7bd71d885700(Sprite * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetScale() );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_UI_Sprite_22_68ae2bf5b33b91ae75d5424d5ca9c433c0fc04fc(Sprite * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetRotation() );
}

}

extern "C" {
Texture * inline_c_Graphics_Urho3D_UI_Sprite_23_65535f3f77dd9f69a9145a0af4014f5a5f3d73f4(Sprite * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetTexture() );
}

}

extern "C" {
const IntRect * inline_c_Graphics_Urho3D_UI_Sprite_24_f3432aa8af45fbb70b61854373ba388446008fb1(Sprite * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetImageRect() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Sprite_25_5b64db0f8cd89f58cd38a8ce4bc4bec87e5d78a2(Sprite * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetBlendMode() );
}

}
