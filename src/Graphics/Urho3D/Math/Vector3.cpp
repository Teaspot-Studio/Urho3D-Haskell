
#include <Urho3D/Math/Vector3.h>

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
int inline_c_Graphics_Urho3D_Math_Vector3_0_3d7e04ade11deb0ef13d634c015674ea9c22e126() {
return ( (int)sizeof(Vector3) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Vector3_1_ca429397721074b301aaf418215538c6e38cde3e() {
return ( (int)Traits<Vector3>::AlignmentOf );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Vector3_2_9e1e04fb6ce838e436adac5d3a5d13c413c58851(Vector3 * ptr_inline_c_0) {
return ( ptr_inline_c_0->x_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Vector3_3_d5d2e71803908b2ff5c53a2f2d8f8397e57fbdde(Vector3 * ptr_inline_c_0) {
return ( ptr_inline_c_0->y_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Vector3_4_5b4670ac9d55f3c35b26ffca88ca5a9dfff91286(Vector3 * ptr_inline_c_0) {
return ( ptr_inline_c_0->z_ );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Math_Vector3_5_d342f7de5250927d8bcdd39ca665bb3c9219514f(Vector3 * ptr_inline_c_0, float vx_27_inline_c_1, Vector3 * ptr_inline_c_2, float vy_27_inline_c_3, Vector3 * ptr_inline_c_4, float vz_27_inline_c_5) {
 
    ptr_inline_c_0->x_ = vx_27_inline_c_1;
    ptr_inline_c_2->y_ = vy_27_inline_c_3;
    ptr_inline_c_4->z_ = vz_27_inline_c_5;
    
}

}
