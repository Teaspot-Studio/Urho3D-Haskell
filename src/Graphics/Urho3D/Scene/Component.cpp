
#include <Urho3D/Scene/Component.h>

using namespace Urho3D;

extern "C" {
Component * inline_c_Graphics_Urho3D_Scene_Component_0_f7d99e010373db173ce6718e7139b01697682f71(Context * ptr_inline_c_0) {
return ( new Component(ptr_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Component_1_24075794633c33531fc85931cbc9c4ea85d0d6e5(Component * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<Component> SharedComponent;

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Component_2_bed4007385dd878f2e7867c3139efcd4a976d73d(SharedComponent * ptr_inline_c_0, SharedComponent * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedComponent * inline_c_Graphics_Urho3D_Scene_Component_3_ba6889ab0ac5ac477407e938208f3c7c5f1a68d8(Component * ptr_inline_c_0) {
return ( new SharedComponent(ptr_inline_c_0) );
}

}

extern "C" {
Component * inline_c_Graphics_Urho3D_Scene_Component_4_4ab89c0d6f1938d1505b81a49ce49d0817646094(SharedComponent * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}
