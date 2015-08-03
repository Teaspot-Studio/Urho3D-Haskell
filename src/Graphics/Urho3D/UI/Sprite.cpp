
#include <Urho3D/UI/Sprite.h>

using namespace Urho3D;

extern "C" {
Sprite * inline_c_Graphics_Urho3D_UI_Sprite_0_9eb3950e89fa04c9d7bbdcd2c00ea40078daef92(Context * ptr_inline_c_0) {
return ( new Sprite( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_1_d14ba6e944a3cfb0c91d37312b26f5bfb06ca90d(Sprite * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<Sprite> SharedSprite;

extern "C" {
SharedSprite * inline_c_Graphics_Urho3D_UI_Sprite_2_7bb23e0eb55d94f2aeb94f4412bdd4a38695fb90(Sprite * ptr_inline_c_0) {
return ( new SharedSprite(ptr_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Sprite_3_08827c22ac8e6c71ecb85f9f77bc11a727113e83(SharedSprite * ptr_inline_c_0) {
 delete ptr_inline_c_0;
}

}
