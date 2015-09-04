
#include <Urho3D/Engine/Console.h>

using namespace Urho3D;

extern "C" {
Console * inline_c_Graphics_Urho3D_Engine_Console_0_f485880502192cd253a1dce243cf7c842fc14fb5(Context * ptr_inline_c_0) {
return (new Console(ptr_inline_c_0));
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Console_1_6f120d9f328cd0de32f2a052933342251da20433(Console * ptr_inline_c_0) {
delete ptr_inline_c_0;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Console_2_fff0c00c5acc6c79159e65d7246aae0b03cd0242(Console * ptr_inline_c_0, XMLFile * file_inline_c_1) {
 ptr_inline_c_0->SetDefaultStyle(file_inline_c_1) ;
}

}

extern "C" {
BorderImage * inline_c_Graphics_Urho3D_Engine_Console_3_efc4dc77a7ac6c57c2f84cc3a521b9f509079d45(Console * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetBackground() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Engine_Console_4_f52e6d76cfad776099bcec17ce832aa74b1b7fa4(Console * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsVisible());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Console_5_8877404bdba1182bd1135300cb04b9e540f2c067(Console * ptr_inline_c_0, int flag_27_inline_c_1) {
 ptr_inline_c_0->SetVisible(flag_27_inline_c_1 != 0);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Console_6_4a7235b521b4272297ead506762938699d402eea(Console * ptr_inline_c_0) {
 ptr_inline_c_0->Toggle() ;
}

}

extern "C" {
Console * inline_c_Graphics_Urho3D_Engine_Console_7_a1b899c7b8b7bd5a9d74a3131c41e060210f6db7(Object * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetSubsystem<Console>() );
}

}

extern "C" {
Object * inline_c_Graphics_Urho3D_Engine_Console_8_a7fe86ca57a6192620a47411cc2d479da6239a44(Console * ptr_inline_c_0) {
return ((Object*)ptr_inline_c_0);
}

}

extern "C" {
Console * inline_c_Graphics_Urho3D_Engine_Console_9_9465596a9056eae5f710253d3d7c5f25ccf1e83b(Object * ptr_inline_c_0) {
return ((Console*)ptr_inline_c_0);
}

}
