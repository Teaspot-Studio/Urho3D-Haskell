
#include <Urho3D/Graphics/Renderer.h>

using namespace Urho3D;

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Renderer_0_73ce54b8cde1876083f174c4a43ef2cc5882ece7(Renderer * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetTextureQuality() );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Renderer_1_5863555d77a6946707371f64612574ad0f358df4(Renderer * ptr_inline_c_0, int e_inline_c_1) {
ptr_inline_c_0->SetTextureQuality(e_inline_c_1);
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Renderer_2_865d4acfc54be3d8904d6ba60b434d10b84b4d3e(Renderer * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetMaterialQuality() );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Renderer_3_1173348d6b4010c08303e5a255e1945657def556(Renderer * ptr_inline_c_0, int e_inline_c_1) {
ptr_inline_c_0->SetMaterialQuality(e_inline_c_1);
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Renderer_4_b6aa06a3d7aed07bca3299d75c84c92730bebc03(Renderer * ptr_inline_c_0) {
return (ptr_inline_c_0->GetSpecularLighting());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Renderer_5_d2fab6ef3af558258679180b1f311852c0fc30fe(Renderer * ptr_inline_c_0, int flag_27_inline_c_1) {
ptr_inline_c_0->SetSpecularLighting(flag_27_inline_c_1 != 0);
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Renderer_6_b8d232120d38b96b7774c6fa59a8a4c91712f806(Renderer * ptr_inline_c_0) {
return (ptr_inline_c_0->GetDrawShadows());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Renderer_7_5787e7808e5d962fb79a901720e6ed38359151ae(Renderer * ptr_inline_c_0, int flag_27_inline_c_1) {
ptr_inline_c_0->SetDrawShadows(flag_27_inline_c_1 != 0);
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Renderer_8_29b709998d4f55d364f7466ec458b4d1a43862b3(Renderer * ptr_inline_c_0) {
return (ptr_inline_c_0->GetShadowMapSize());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Renderer_9_e615fbd1c16e044bc76b6ff54c57f37cff9cd5ba(Renderer * ptr_inline_c_0, int s_27_inline_c_1) {
ptr_inline_c_0->SetShadowMapSize(s_27_inline_c_1);
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Renderer_10_58c9758fa580f5ef19c12f61dc239623bba31beb(Renderer * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetShadowQuality() );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Renderer_11_c85019d11f822c848bff197a1279a82ed3f749e9(Renderer * ptr_inline_c_0, int e_inline_c_1) {
ptr_inline_c_0->SetShadowQuality(e_inline_c_1);
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Renderer_12_798bd4567537424e8c166f696ddcc5cbebd0084e(Renderer * ptr_inline_c_0) {
return (ptr_inline_c_0->GetMaxOccluderTriangles());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Renderer_13_30b30618c220a0360211fe15a8bdad05a5abc839(Renderer * ptr_inline_c_0, int s_27_inline_c_1) {
ptr_inline_c_0->SetMaxOccluderTriangles(s_27_inline_c_1);
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Renderer_14_6ea66132348e557bec84656e9f7dc2f16bbf457e(Renderer * ptr_inline_c_0) {
return (ptr_inline_c_0->GetDynamicInstancing());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Renderer_15_651dd41aea63f366d60a52de6be20af4986b2dd4(Renderer * ptr_inline_c_0, int flag_27_inline_c_1) {
ptr_inline_c_0->SetDynamicInstancing(flag_27_inline_c_1 != 0);
}

}

extern "C" {
Renderer * inline_c_Graphics_Urho3D_Graphics_Renderer_16_105f11514412f5c58e11261a9d03cef8dfe73752(Object * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetSubsystem<Renderer>() );
}

}

extern "C" {
Object * inline_c_Graphics_Urho3D_Graphics_Renderer_17_8c9ddd7131e53e6d8ecd03060160705d720a9cc7(Renderer * ptr_inline_c_0) {
return ( (Object*)ptr_inline_c_0 );
}

}

extern "C" {
Renderer * inline_c_Graphics_Urho3D_Graphics_Renderer_18_04100f6cb98c953ae54dbf81c6648b084f8b4228(Object * ptr_inline_c_0) {
return ( (Renderer*)ptr_inline_c_0 );
}

}
