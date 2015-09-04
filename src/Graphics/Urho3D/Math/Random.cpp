
#include <Urho3D/Math/Random.h>

using namespace Urho3D;

extern "C" {
void inline_c_Graphics_Urho3D_Math_Random_0_7963eed33499643919f298f28c52a943040ff344(unsigned seed_27_inline_c_0) {
 SetRandomSeed(seed_27_inline_c_0) ;
}

}

extern "C" {
unsigned inline_c_Graphics_Urho3D_Math_Random_1_63e5bc23c15173550b843f489593fee13632b55d() {
return ( GetRandomSeed() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Random_2_894bc21ba1b666672598f41805921d1269a2da08() {
return ( Rand() );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Random_3_5c5752a59e05c58c6ce848aae658fd2cb5cdbb5e() {
return ( RandStandardNormal() );
}

}
