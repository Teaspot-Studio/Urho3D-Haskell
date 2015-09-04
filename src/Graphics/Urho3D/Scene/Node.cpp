
#include <Urho3D/Scene/Node.h>

#include <Urho3D/Scene/Component.h>

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
void inline_c_Graphics_Urho3D_Scene_Node_2_afab0529492a4ff4243893bba554cfcc8bfe07b2(SharedNode * ptr_inline_c_0, SharedNode * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedNode * inline_c_Graphics_Urho3D_Scene_Node_3_dca769aebcca3204153aaa000cd23089a1434e5a(Node * ptr_inline_c_0) {
return ( new SharedNode(ptr_inline_c_0) );
}

}

extern "C" {
Node * inline_c_Graphics_Urho3D_Scene_Node_4_4b67d79bd16df5d5d7b03130e157e04f6697d14a(SharedNode * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_5_3fd163b2af44d281d8b019b02026179600765201(Node * ptr_inline_c_0, const char * str_27_inline_c_1) {
 ptr_inline_c_0->SetName(String(str_27_inline_c_1)) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_6_4fb32422f93c6fd97985641b1a692dd9320e63e4(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1) {
 ptr_inline_c_0->SetPosition(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_7_48ed40ca6a264404f29c4508ca2555e35ef9fcea(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1) {
 ptr_inline_c_0->SetPosition2D(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_8_c1f1b3330b70841fd8aef0e3f33b47eb97768f2c(Node * ptr_inline_c_0, float x_27_inline_c_1, float y_27_inline_c_2) {
 ptr_inline_c_0->SetPosition2D(x_27_inline_c_1, y_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_9_91aa65f84035f889f7e389909ba5e8a46176750a(Node * ptr_inline_c_0, Quaternion * q_27_inline_c_1) {
ptr_inline_c_0->SetRotation(*q_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_10_661d01b06be77f2398b1fa46de6be59826d2808e(Node * ptr_inline_c_0, float a_27_inline_c_1) {
ptr_inline_c_0->SetRotation2D(a_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_11_e07c4ef8be21eb70f27aa3bdf7b0f63f901ab4fc(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1) {
 ptr_inline_c_0->SetDirection(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_12_864284337bdce5bfb22d5a6510f0bd0d96f5b818(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1) {
 ptr_inline_c_0->SetScale(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_13_ed29435c65eebc229b5cb2c5dd8dac74939bbecd(Node * ptr_inline_c_0, float a_27_inline_c_1) {
ptr_inline_c_0->SetScale(a_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_14_e8f86ca70aca40fdb6afe42f7ac36fcc847f5479(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1) {
 ptr_inline_c_0->SetScale2D(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_15_a9f5fe2b751db16e3ca3f95384c60dadb689ff79(Node * ptr_inline_c_0, float x_27_inline_c_1, float y_27_inline_c_2) {
 ptr_inline_c_0->SetScale2D(x_27_inline_c_1, y_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_16_e03bcbbe62a4b3de0b6ddcf6c3f8b8f41e732a06(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1, Quaternion * q_27_inline_c_2) {
 ptr_inline_c_0->SetTransform(*v_27_inline_c_1, *q_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_17_c55a55e47eaa69c1453670c7c0c99d0e1ca66ecc(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1, Quaternion * q_27_inline_c_2, Vector3 * s_27_inline_c_3) {
 ptr_inline_c_0->SetTransform(*v_27_inline_c_1, *q_27_inline_c_2, *s_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_18_20b983e79548b62c6c91c65984fabf60d61c1e07(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1, Quaternion * q_27_inline_c_2, float s_27_inline_c_3) {
 ptr_inline_c_0->SetTransform(*v_27_inline_c_1, *q_27_inline_c_2, s_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_19_e7a8566577b7e4bd73402ed7d313439cafab356c(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1, float q_27_inline_c_2) {
 ptr_inline_c_0->SetTransform2D(*v_27_inline_c_1, q_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_20_84e5d46a913abb5abc72d2b92184ec344507977c(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1, float q_27_inline_c_2, Vector2 * s_27_inline_c_3) {
 ptr_inline_c_0->SetTransform2D(*v_27_inline_c_1, q_27_inline_c_2, *s_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_21_6aafa460f75b3384ec9bb1f28672da816ad1782b(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1, float q_27_inline_c_2, float s_27_inline_c_3) {
 ptr_inline_c_0->SetTransform2D(*v_27_inline_c_1, q_27_inline_c_2, s_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_22_c6f05ee1f0cad0dadf38371fc511590325a0cdf9(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1) {
 ptr_inline_c_0->SetWorldPosition(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_23_d2fc1b816161743baae78765d298effe3c30036a(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1) {
 ptr_inline_c_0->SetWorldPosition2D(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_24_40e0e4c1134e22a6b7ef29940f09d139a0798381(Node * ptr_inline_c_0, float x_27_inline_c_1, float y_27_inline_c_2) {
 ptr_inline_c_0->SetWorldPosition2D(x_27_inline_c_1, y_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_25_6a5d2112f76188261a707fd23f5a7d0d6c88b35b(Node * ptr_inline_c_0, Quaternion * q_27_inline_c_1) {
ptr_inline_c_0->SetWorldRotation(*q_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_26_dc30dfa1a06cc8f65ce1706ca45d31cb27ebe7ed(Node * ptr_inline_c_0, float a_27_inline_c_1) {
ptr_inline_c_0->SetWorldRotation2D(a_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_27_df1c980e3f4ec3511faab682476a360220643510(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1) {
 ptr_inline_c_0->SetWorldDirection(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_28_40b4fa098e7a67738b1248e3dc6c7d40a5b6a2e1(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1) {
 ptr_inline_c_0->SetWorldScale(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_29_c20b8d9d91f1a5e5ac8efb81e52c48c352ef52cb(Node * ptr_inline_c_0, float a_27_inline_c_1) {
ptr_inline_c_0->SetWorldScale(a_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_30_462350b6cf5a618cfa2b34d8540874688987db88(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1) {
 ptr_inline_c_0->SetWorldScale2D(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_31_b3f6bf6ea2f55ebd3980da493308b93ef1822737(Node * ptr_inline_c_0, float x_27_inline_c_1, float y_27_inline_c_2) {
 ptr_inline_c_0->SetWorldScale2D(x_27_inline_c_1, y_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_32_45380421f621128b876b11710d4c9ab6bdf94ec3(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1, Quaternion * q_27_inline_c_2) {
 ptr_inline_c_0->SetWorldTransform(*v_27_inline_c_1, *q_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_33_183606d0885c7348ef96b5459bbde6465e818a68(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1, Quaternion * q_27_inline_c_2, Vector3 * s_27_inline_c_3) {
 ptr_inline_c_0->SetWorldTransform(*v_27_inline_c_1, *q_27_inline_c_2, *s_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_34_4e92d0f903a917162fd4068a709bf4963eab49be(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1, Quaternion * q_27_inline_c_2, float s_27_inline_c_3) {
 ptr_inline_c_0->SetWorldTransform(*v_27_inline_c_1, *q_27_inline_c_2, s_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_35_fff248fc0c82179dfa191df024cd89ef692750c6(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1, float q_27_inline_c_2) {
 ptr_inline_c_0->SetWorldTransform2D(*v_27_inline_c_1, q_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_36_7a95d4c8b3cf58949140dcc60b73b17dd2e57f7b(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1, float q_27_inline_c_2, Vector2 * s_27_inline_c_3) {
 ptr_inline_c_0->SetWorldTransform2D(*v_27_inline_c_1, q_27_inline_c_2, *s_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_37_a268898af9027178c1fe5fa47142acd11ce4048d(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1, float q_27_inline_c_2, float s_27_inline_c_3) {
 ptr_inline_c_0->SetWorldTransform2D(*v_27_inline_c_1, q_27_inline_c_2, s_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_38_23afd57858c30f50f49a3a382bab89a50a1ca4e5(Node * ptr_inline_c_0, Vector3 * v_27_inline_c_1, int s_27_inline_c_2) {
 ptr_inline_c_0->Translate(*v_27_inline_c_1, (TransformSpace)s_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_39_c2370ce801d5828151a99c5c57b27875fb1713f1(Node * ptr_inline_c_0, Vector2 * v_27_inline_c_1, int s_27_inline_c_2) {
 ptr_inline_c_0->Translate2D(*v_27_inline_c_1, (TransformSpace)s_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_40_9ec5e50c27fa1754c88e2b2ada682119fe8883ba(Node * ptr_inline_c_0, Quaternion * q_27_inline_c_1, int s_27_inline_c_2) {
 ptr_inline_c_0->Rotate(*q_27_inline_c_1, (TransformSpace)s_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_41_b37643df3acbac68dd0d26194ff66f2263330d83(Node * ptr_inline_c_0, float q_27_inline_c_1, int s_27_inline_c_2) {
 ptr_inline_c_0->Rotate(q_27_inline_c_1, (TransformSpace)s_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_42_986e3997c509ebfbbbe10031036d8114cc565d42(Node * ptr_inline_c_0, Vector3 * pv_27_inline_c_1, Quaternion * q_27_inline_c_2, int s_27_inline_c_3) {
 ptr_inline_c_0->RotateAround(*pv_27_inline_c_1, *q_27_inline_c_2, (TransformSpace)s_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Node_43_ad26d3841762f087e697218329299e127e340c7c(Node * ptr_inline_c_0, Vector2 * pv_27_inline_c_1, float q_27_inline_c_2, int s_27_inline_c_3) {
 ptr_inline_c_0->RotateAround(*pv_27_inline_c_1, q_27_inline_c_2, (TransformSpace)s_27_inline_c_3) ;
}

}

extern "C" {
Component * inline_c_Graphics_Urho3D_Scene_Node_44_d052d08ffc4beaf557b3e7d2956a111f905d0872(Node * ptr_inline_c_0, StringHash * ct_inline_c_1) {
return ( ptr_inline_c_0->GetComponent(*ct_inline_c_1) );
}

}

extern "C" {
const Quaternion * inline_c_Graphics_Urho3D_Scene_Node_45_be4895379d67b660c0edd657e8be0b8cb3369817(Node * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetRotation() );
}

}

extern "C" {
StringHash * inline_c_Graphics_Urho3D_Scene_Node_46_2c3327965a2a9316cb0f2b4b957b73ce7fb8f8b3() {

    static StringHash h = Component::GetTypeStatic();
    return &h;
  
}

}
