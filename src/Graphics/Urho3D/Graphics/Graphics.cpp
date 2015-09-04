
#include <Urho3D/Graphics/Graphics.h>

using namespace Urho3D;

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Graphics_0_b759d8d9b63349c2f7f244811524d28fefe10178(Graphics * ptr_27_inline_c_0, Image * icon_inline_c_1) {
 ptr_27_inline_c_0->SetWindowIcon(icon_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Graphics_1_7fd59aca69b4742b93db86a3aa60d2eed21fc7f4(Graphics * ptr_27_inline_c_0, const char * str_27_inline_c_1) {
 ptr_27_inline_c_0->SetWindowTitle(String(str_27_inline_c_1)) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Graphics_Graphics_2_65d3ced19cd810e2b19391ec37fc74778ce01181(Graphics * ptr_inline_c_0, Image * img_inline_c_1) {
 ptr_inline_c_0->TakeScreenShot(*img_inline_c_1) ;
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Graphics_3_40ca2abcb6b0fec58e94fb2ea3f03b4523f4045d(Graphics * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetHeight() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Graphics_Graphics_4_eb506a757ec1de63c7a2a0a858e46d34a6a8f483(Graphics * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetWidth() );
}

}

extern "C" {
Graphics * inline_c_Graphics_Urho3D_Graphics_Graphics_5_e0eb8ae3b8c164583400aefb5dcaeef2a458e3a3(Object * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetSubsystem<Graphics>() );
}

}

extern "C" {
Object * inline_c_Graphics_Urho3D_Graphics_Graphics_6_7d8207065ef22a30652819da069c66d694e2724f(Graphics * ptr_inline_c_0) {
return ( (Object*)ptr_inline_c_0 );
}

}

extern "C" {
Graphics * inline_c_Graphics_Urho3D_Graphics_Graphics_7_4bcae3ee7d7339b8b73b0a5d7c8e7ca720419cd4(Object * ptr_inline_c_0) {
return ( (Graphics*)ptr_inline_c_0 );
}

}
