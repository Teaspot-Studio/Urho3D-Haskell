
#include <Urho3D/Scene/Node.h>

using namespace Urho3D;

extern "C" {
Node * inline_c_Graphics_Urho3D_Scene_Node_0_e6492b9b10e765a2c3137b83e7f59bcb6193e518(Context * ptr_inline_c_0) {
return ( new Node(ptr_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_1_5f333e2eb83f5bbd0184442a2cdadeac3cbb0210(Node * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<Node> SharedNode;

extern "C" {
SharedNode * inline_c_Graphics_Urho3D_Scene_Node_2_dca769aebcca3204153aaa000cd23089a1434e5a(Node * ptr_inline_c_0) {
return ( new SharedNode(ptr_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_3_b1470ee3a6ff062ae6f4e35c247d07a22671d54c(SharedNode * ptr_inline_c_0) {
 delete ptr_inline_c_0;
}

}
