
#include <Urho3D/Math/Quaternion.h>

using namespace Urho3D;


template <class T>
class Traits
{
public:
    struct AlignmentFinder
    {
      char a; 
      T b;
    };

    enum {AlignmentOf = sizeof(AlignmentFinder) - sizeof(T)};
};


extern "C" {
void inline_c_Graphics_Urho3D_Math_Quaternion_0_331abaff17ad01ed0320903a08de1dd711796c34(Quaternion * qp_inline_c_0) {
 delete qp_inline_c_0 ;
}

}

extern "C" {
Quaternion * inline_c_Graphics_Urho3D_Math_Quaternion_1_585f703320bdf1cd29b8015310d296f4c38f588b(float x_27_inline_c_0, float y_27_inline_c_1, float z_27_inline_c_2) {
return (
    new Quaternion(x_27_inline_c_0, y_27_inline_c_1, z_27_inline_c_2)
  );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Quaternion_2_d121abe29969b6614869f8f58d21ba45a49ce0fc() {
return ( (int)sizeof(Quaternion) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Quaternion_3_b16e00042643d01a0deb9273737d4c4b64cd2bbf() {
return ( (int)Traits<Quaternion>::AlignmentOf );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Quaternion_4_b71620f8a8da3a24321e8e91d42b9993a077011b(Quaternion * ptr_inline_c_0) {
return ( ptr_inline_c_0->x_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Quaternion_5_e6a15b129c0cd24b66aa213399ea74c35d775633(Quaternion * ptr_inline_c_0) {
return ( ptr_inline_c_0->y_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Quaternion_6_8fade5e4a15901cba2ab20ab990ef54016ba5b61(Quaternion * ptr_inline_c_0) {
return ( ptr_inline_c_0->z_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Quaternion_7_0033b09f83035a6d452b0251f816ede83db8f2f7(Quaternion * ptr_inline_c_0) {
return ( ptr_inline_c_0->w_ );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Math_Quaternion_8_525e1f73e166578f303cc7e0f053f66391c9145e(Quaternion * ptr_inline_c_0, float vx_27_inline_c_1, Quaternion * ptr_inline_c_2, float vy_27_inline_c_3, Quaternion * ptr_inline_c_4, float vz_27_inline_c_5, Quaternion * ptr_inline_c_6, float vw_27_inline_c_7) {
 
    ptr_inline_c_0->x_ = vx_27_inline_c_1;
    ptr_inline_c_2->y_ = vy_27_inline_c_3;
    ptr_inline_c_4->z_ = vz_27_inline_c_5;
    ptr_inline_c_6->w_ = vw_27_inline_c_7;
    
}

}
