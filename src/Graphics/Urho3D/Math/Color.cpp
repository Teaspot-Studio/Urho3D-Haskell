
#include <Urho3D/Math/Color.h>

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
int inline_c_Graphics_Urho3D_Math_Color_0_292e03ea79394bb220477719699f4e4f8be758e0() {
return ( (int)sizeof(Color) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Color_1_10d20b74bbbefedbde97374fbdf268d33a479d91() {
return ( (int)Traits<Color>::AlignmentOf );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Color_2_9a883a30b4421dfdfbc15656fdcda158bdbee88c(Color * ptr_inline_c_0) {
return ( ptr_inline_c_0->r_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Color_3_03203ea7bae05a6c426b20ed53173cebc37f138e(Color * ptr_inline_c_0) {
return ( ptr_inline_c_0->g_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Color_4_c7e5106790c9103ce75321fe5b924942eab16601(Color * ptr_inline_c_0) {
return ( ptr_inline_c_0->b_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Color_5_ef65068ae178f900992f91502452746113a8393d(Color * ptr_inline_c_0) {
return ( ptr_inline_c_0->a_ );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Math_Color_6_5501bfe3f0fc6775e436619a634fd48a8db0c4b7(Color * ptr_inline_c_0, float vr_27_inline_c_1, Color * ptr_inline_c_2, float vg_27_inline_c_3, Color * ptr_inline_c_4, float vb_27_inline_c_5, Color * ptr_inline_c_6, float va_27_inline_c_7) {
 
    ptr_inline_c_0->r_ = vr_27_inline_c_1;
    ptr_inline_c_2->g_ = vg_27_inline_c_3;
    ptr_inline_c_4->b_ = vb_27_inline_c_5;
    ptr_inline_c_6->a_ = va_27_inline_c_7;
    
}

}
