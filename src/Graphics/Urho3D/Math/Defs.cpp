
#include <Urho3D/Math/MathDefs.h>

using namespace Urho3D;

extern "C" {
float inline_c_Graphics_Urho3D_Math_Defs_0_5cc646d6b06eeba790a416df1618654c54a2e2a1() {
return ( (float)HUGE_VAL );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Defs_1_ccd0c6671cc2e68a3a3b9659e56bd72a141b60ea(float lhs_27_inline_c_0, float rhs_27_inline_c_1) {
return ((int)Equals(lhs_27_inline_c_0, rhs_27_inline_c_1));
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Defs_2_e7bd7f3acfdfae5f12d4234241abcfda8723067a(float lhs_27_inline_c_0, float rhs_27_inline_c_1, float t_27_inline_c_2) {
return (Lerp(lhs_27_inline_c_0, rhs_27_inline_c_1, t_27_inline_c_2));
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Defs_3_bb6008db17908117d0164640d0fa8760b2603646(float lhs_27_inline_c_0, float rhs_27_inline_c_1, float t_27_inline_c_2) {
return (SmoothStep(lhs_27_inline_c_0, rhs_27_inline_c_1, t_27_inline_c_2));
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Defs_4_038840320865410a6951b18691a399833d1494d4(unsigned value_27_inline_c_0) {
return ((int)IsPowerOfTwo(value_27_inline_c_0));
}

}

extern "C" {
unsigned inline_c_Graphics_Urho3D_Math_Defs_5_9b0410bb814023ccfebe852a1613fe4b03b1945e(unsigned value_27_inline_c_0) {
return (NextPowerOfTwo(value_27_inline_c_0));
}

}

extern "C" {
unsigned inline_c_Graphics_Urho3D_Math_Defs_6_de4eb997a0553e5d2920ec599893d9a84b44681d(unsigned value_27_inline_c_0) {
return (CountSetBits(value_27_inline_c_0));
}

}

extern "C" {
unsigned inline_c_Graphics_Urho3D_Math_Defs_7_3aa87ca7850311bc51984f3fa2dee62f61f33f1c(unsigned hash_27_inline_c_0, unsigned char w_27_inline_c_1) {
return (SDBMHash(hash_27_inline_c_0, w_27_inline_c_1));
}

}

extern "C" {
unsigned short inline_c_Graphics_Urho3D_Math_Defs_8_5362e13d8594c14954a8fb893b64c6d5e63a73d7(float value_27_inline_c_0) {
return (FloatToHalf(value_27_inline_c_0));
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Defs_9_09b1d379cf6656a9f4dd7358f7c5a65911b17ae8(unsigned short value_27_inline_c_0) {
return (HalfToFloat(value_27_inline_c_0));
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Defs_10_e3b87a5f2be69b03d6791b831549de2bd16b4d44() {
return (Random());
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Defs_11_96983b26268a91dc94062d797c42ec0f0bff8f7a(float max_27_inline_c_0) {
return (Random(max_27_inline_c_0));
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Defs_12_2f45e07560fba891e8b0d18c077b6cb799f70dd1(float min_27_inline_c_0, float max_27_inline_c_1) {
return (Random(min_27_inline_c_0, max_27_inline_c_1));
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Defs_13_43dfd09e76b6a6c889a2015d10455b62d12bb0fe() {
return (Random(2));
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Defs_14_7e9359d648d28d81957351ae6e3fc9dcd08fb491(int max_27_inline_c_0) {
return (Random(max_27_inline_c_0));
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Defs_15_5a204ae579d62d6606af879b90fe2dd0e1cef65e(int min_27_inline_c_0, int max_27_inline_c_1) {
return (Random(min_27_inline_c_0, max_27_inline_c_1));
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Math_Defs_16_f070536447f7201d477746df3075a4c064eb924e(int value_27_inline_c_0, int min_27_inline_c_1, int max_27_inline_c_2) {
return (Clamp(value_27_inline_c_0, min_27_inline_c_1, max_27_inline_c_2));
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Math_Defs_17_67ceb3a5d9c5182a2ad941cd0d5916a36df6aaa7(float value_27_inline_c_0, float min_27_inline_c_1, float max_27_inline_c_2) {
return (Clamp(value_27_inline_c_0, min_27_inline_c_1, max_27_inline_c_2));
}

}
