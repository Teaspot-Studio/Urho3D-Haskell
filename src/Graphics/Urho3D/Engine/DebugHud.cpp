
#include <Urho3D/Engine/DebugHud.h>

using namespace Urho3D;

extern "C" {
DebugHud * inline_c_Graphics_Urho3D_Engine_DebugHud_0_518e689c53e8a5af5787167f85b34982525db4f2(Context * ptr_inline_c_0) {
return (new DebugHud(ptr_inline_c_0));
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_DebugHud_1_83ce1a2e3712d816be013494a301e4b626a61f78(DebugHud * ptr_inline_c_0) {
delete ptr_inline_c_0;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_DebugHud_2_d3d0fe979509497103bd44e57aa578f296aac98b(DebugHud * ptr_inline_c_0, XMLFile * file_inline_c_1) {
 ptr_inline_c_0->SetDefaultStyle(file_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_DebugHud_3_60bc6eb4c3ec6c8c0984c32546845a35cf700b32(DebugHud * ptr_inline_c_0) {
 ptr_inline_c_0->ToggleAll() ;
}

}

extern "C" {
DebugHud * inline_c_Graphics_Urho3D_Engine_DebugHud_4_fc71810dc95b2c58ae76542ee0616c62594eee7c(Object * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetSubsystem<DebugHud>() );
}

}

extern "C" {
Object * inline_c_Graphics_Urho3D_Engine_DebugHud_5_a1b302ab2a4355859676454b96c5b372bbebef41(DebugHud * ptr_inline_c_0) {
return ((Object*)ptr_inline_c_0);
}

}

extern "C" {
DebugHud * inline_c_Graphics_Urho3D_Engine_DebugHud_6_98f5f59855c7f0bf782f23a8317a3b738c781c37(Object * ptr_inline_c_0) {
return ((DebugHud*)ptr_inline_c_0);
}

}
