
#include <Urho3D/UI/BorderImage.h>

using namespace Urho3D;

extern "C" {
BorderImage * inline_c_Graphics_Urho3D_UI_BorderImage_0_719c682bfb37d51db0911ace43e18065aa9c4e30(Context * ptr_inline_c_0) {
return ( new BorderImage( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_BorderImage_1_8e118542e7749fd9165d6ce0fb9a46c79bfe8059(BorderImage * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_BorderImage_2_1d5b24817ec0e738e6aa6be2ca8e0f1867790f27(BorderImage * ptr_inline_c_0, Texture * tex_inline_c_1) {
 ptr_inline_c_0->SetTexture(tex_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_BorderImage_3_369d2d2814ec1142efe8080d3e8b6f0e590b206f(BorderImage * ptr_inline_c_0, IntRect * r_27_inline_c_1) {
ptr_inline_c_0->SetImageRect(*r_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_BorderImage_4_a9d8db4d244b545dffc4ca1f50776ce8e978b50f(BorderImage * ptr_inline_c_0) {
ptr_inline_c_0->SetFullImageRect();
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_BorderImage_5_5765540f078832d59ce056b7c98f93d00d55730f(BorderImage * ptr_inline_c_0, IntRect * r_27_inline_c_1) {
ptr_inline_c_0->SetBorder(*r_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_BorderImage_6_a7ccad724cd723151529c29cc66c1c8d384be1d2(BorderImage * ptr_inline_c_0, IntRect * r_27_inline_c_1) {
ptr_inline_c_0->SetImageBorder(*r_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_BorderImage_7_f99fd6cddc010e988f8e4d58315dc2dbdfabfe23(BorderImage * ptr_inline_c_0, IntVector2 * v_27_inline_c_1) {
ptr_inline_c_0->SetHoverOffset(*v_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_BorderImage_8_4010924d5319178b0506a5568a9d4c0fe6a6068c(BorderImage * ptr_inline_c_0, int bm_27_inline_c_1) {
ptr_inline_c_0->SetBlendMode((BlendMode)bm_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_BorderImage_9_d0f7b6ddc8fe747de02ed01d9b7cb8ca8eccfcc1(BorderImage * ptr_inline_c_0, int b_27_inline_c_1) {
ptr_inline_c_0->SetTiled(b_27_inline_c_1 != 0);
}

}

extern "C" {
Texture * inline_c_Graphics_Urho3D_UI_BorderImage_10_255d189a4e71324e632a4e5cd5213fffd66844ff(BorderImage * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetTexture() );
}

}

extern "C" {
const IntRect * inline_c_Graphics_Urho3D_UI_BorderImage_11_75d76e71e28079a1d119ea5363a5b255ae2c4142(BorderImage * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetImageRect() );
}

}

extern "C" {
const IntRect * inline_c_Graphics_Urho3D_UI_BorderImage_12_3b2fa610e9616b244dc8d52081e009335ff7d1af(BorderImage * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetBorder() );
}

}

extern "C" {
const IntVector2 * inline_c_Graphics_Urho3D_UI_BorderImage_13_ddb341175493606e4604347cd52df399f7d73c74(BorderImage * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetHoverOffset() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_BorderImage_14_98a665126ae977123bc8f72d6e96daa76d281c4b(BorderImage * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetBlendMode() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_BorderImage_15_36660e76806e8c26e27bd8c021629dc5b501d085(BorderImage * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsTiled() );
}

}

extern "C" {
StringHash * inline_c_Graphics_Urho3D_UI_BorderImage_16_5c49e2d30402c3a6a1eb490fff34f899148ee3dd() {
 
      static StringHash h = BorderImage::GetTypeStatic();  
      return &h;
    
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_BorderImage_17_9b892cd71fff428c0133487cf78f4496bbfe8c63(BorderImage * ptr_inline_c_0) {
return ((UIElement*)ptr_inline_c_0);
}

}

extern "C" {
BorderImage * inline_c_Graphics_Urho3D_UI_BorderImage_18_6e9f857451383af3a46c544f5d0123c06222d7ac(UIElement * ptr_inline_c_0) {
return ((BorderImage*)ptr_inline_c_0);
}

}
