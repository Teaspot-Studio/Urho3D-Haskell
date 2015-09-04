
#include <Urho3D/Engine/Engine.h>

using namespace Urho3D;

extern "C" {
Engine * inline_c_Graphics_Urho3D_Engine_Engine_0_d698978671800511db94ae0e8f803bf04fc84af0(Context * ptr_inline_c_0) {
return (new Engine(ptr_inline_c_0));
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Engine_1_797a95b21aae06631706c6209024896a46c6d659(Engine * ptr_inline_c_0) {
delete ptr_inline_c_0;
}

}

extern "C" {
Object * inline_c_Graphics_Urho3D_Engine_Engine_2_1766fb2a916d0c638a36004028932bf10ed85bee(Engine * ptr_inline_c_0) {
return ((Object*)ptr_inline_c_0);
}

}

extern "C" {
Engine * inline_c_Graphics_Urho3D_Engine_Engine_3_f38a9d386fb3d5ab1efc5bf2e75333615f451705(Object * ptr_inline_c_0) {
return ((Engine*)ptr_inline_c_0);
}

}

typedef SharedPtr<Engine> SharedEngine;

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Engine_4_88535ba3328585b30074f5aac8f5f74a487c6bca(SharedEngine * ptr_inline_c_0, SharedEngine * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedEngine * inline_c_Graphics_Urho3D_Engine_Engine_5_e2ebe28d015efe851928910649b4b4e5b82e0c2e(Engine * ptr_inline_c_0) {
return ( new SharedEngine(ptr_inline_c_0) );
}

}

extern "C" {
Engine * inline_c_Graphics_Urho3D_Engine_Engine_6_b42705198a9c8fa0023f4e2a465265486b3d240a(SharedEngine * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Engine_7_1f5c7c067e66c4def05967278999ecd34cd57658(Engine * ptr_inline_c_0, int v_inline_c_1) {
ptr_inline_c_0->DumpResources(v_inline_c_1 != 0);
}

}

extern "C" {
Console * inline_c_Graphics_Urho3D_Engine_Engine_8_6493b655fcc1fb9790dc400dc466dbf24e4679da(Engine * ptr_inline_c_0) {
return ( ptr_inline_c_0->CreateConsole() );
}

}

extern "C" {
DebugHud * inline_c_Graphics_Urho3D_Engine_Engine_9_687538df47d402c66153e48a54edd5a878289cec(Engine * ptr_inline_c_0) {
return ( ptr_inline_c_0->CreateDebugHud() );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Engine_10_f9defe54853c1358475fb7437cad702860c50648(Engine * ptr_inline_c_0) {
 ptr_inline_c_0->Exit() ;
}

}
