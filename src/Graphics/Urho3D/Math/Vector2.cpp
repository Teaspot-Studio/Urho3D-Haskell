
#include <Urho3D/Math/Vector2.h>

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
int inline_c_Graphics_Urho3D_Math_Vector2_0_2644c91de5c9d2fa99ff0e511d4a95adc2244056() {
return ( (int)sizeof(IntVector2) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Vector2_1_992d9738dbbdab64d3d8b5619c031389d092bf1f() {
return ( (int)Traits<IntVector2>::AlignmentOf );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Vector2_2_d578ba9101b269c5e879a1e16566af0174f11c4b(IntVector2 * ptr_inline_c_0) {
return ( ptr_inline_c_0->x_ );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Vector2_3_27b40f218712073fb288434a3545e4c62c965abf(IntVector2 * ptr_inline_c_0) {
return ( ptr_inline_c_0->y_ );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Math_Vector2_4_57cf1e4a67914e0436c9093bcc246ddf2551f3e7(IntVector2 * ptr_inline_c_0, int vx_27_inline_c_1, IntVector2 * ptr_inline_c_2, int vy_27_inline_c_3) {
 
    ptr_inline_c_0->x_ = vx_27_inline_c_1;
    ptr_inline_c_2->y_ = vy_27_inline_c_3;
    
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Vector2_5_5b1f292ab80d425cddf82b7906cee0079f23536b() {
return ( (int)sizeof(Vector2) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Vector2_6_67464befd1b6079c3bf161d60c29862f4a935ed1() {
return ( (int)Traits<Vector2>::AlignmentOf );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Vector2_7_1ec805be1e35ce4d2b203a14f955a47b3366c32e(Vector2 * ptr_inline_c_0) {
return ( ptr_inline_c_0->x_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Vector2_8_5acee6dcd342577486c00ec03a2e0041f4a38496(Vector2 * ptr_inline_c_0) {
return ( ptr_inline_c_0->y_ );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Math_Vector2_9_b13d6b9986ce20f79d2b712a677e0ca153128969(Vector2 * ptr_inline_c_0, float vx_27_inline_c_1, Vector2 * ptr_inline_c_2, float vy_27_inline_c_3) {
 
    ptr_inline_c_0->x_ = vx_27_inline_c_1;
    ptr_inline_c_2->y_ = vy_27_inline_c_3;
    
}

}
