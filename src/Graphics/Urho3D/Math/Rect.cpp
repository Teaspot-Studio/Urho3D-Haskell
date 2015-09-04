
#include <Urho3D/Math/Rect.h>

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
int inline_c_Graphics_Urho3D_Math_Rect_0_0180bb867d6c4c8354e557b4f64d99648c1a0eb1() {
return ( (int)sizeof(IntRect) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Rect_1_3b19955e5030debc4fbb220b67a1ba86342bca4a() {
return ( (int)Traits<IntRect>::AlignmentOf );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Rect_2_985d141eec4666befaceb8510a6d1157591f13dc(IntRect * ptr_inline_c_0) {
return ( ptr_inline_c_0->left_ );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Rect_3_5ab029f441858e34e73962ef41f4a3dd74744b85(IntRect * ptr_inline_c_0) {
return ( ptr_inline_c_0->top_ );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Rect_4_33c19a3c0b8b66c8902e3b80e435651e0bf88156(IntRect * ptr_inline_c_0) {
return ( ptr_inline_c_0->right_ );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Rect_5_d2804a464da191e149e7e593592407412acc5ff1(IntRect * ptr_inline_c_0) {
return ( ptr_inline_c_0->bottom_ );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Math_Rect_6_b1d2d3a08481c86f8f9e936c2145ce9d11d65d36(IntRect * ptr_inline_c_0, int vleft_27_inline_c_1, IntRect * ptr_inline_c_2, int vtop_27_inline_c_3, IntRect * ptr_inline_c_4, int vright_27_inline_c_5, IntRect * ptr_inline_c_6, int vbottom_27_inline_c_7) {
 
    ptr_inline_c_0->left_ = vleft_27_inline_c_1;
    ptr_inline_c_2->top_ = vtop_27_inline_c_3;
    ptr_inline_c_4->right_ = vright_27_inline_c_5;
    ptr_inline_c_6->bottom_ = vbottom_27_inline_c_7;
    
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Rect_7_8196985ad00cb6b778166921536596fa7b900e22() {
return ( (int)sizeof(Rect) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Rect_8_f9895c66195dfd60782b4bdd3905bc6a37e7337d() {
return ( (int)Traits<Rect>::AlignmentOf );
}

}

extern "C" {
Vector2 * inline_c_Graphics_Urho3D_Math_Rect_9_77f92b2cc9c35650697c75179b224addc69975bf(Rect * ptr_inline_c_0) {
return ( &ptr_inline_c_0->min_ );
}

}

extern "C" {
Vector2 * inline_c_Graphics_Urho3D_Math_Rect_10_06ef05c541b857c9ee3fdd21cdb964b57bca9a23(Rect * ptr_inline_c_0) {
return ( &ptr_inline_c_0->max_ );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Math_Rect_11_1874c8e73095707498319250183f4e2bfa0faf14(Rect * ptr_inline_c_0, Vector2 * vmin_27_inline_c_1, Rect * ptr_inline_c_2, Vector2 * vmax_27_inline_c_3) {
 
    ptr_inline_c_0->min_ = *vmin_27_inline_c_1;
    ptr_inline_c_2->max_ = *vmax_27_inline_c_3;
    
}

}
