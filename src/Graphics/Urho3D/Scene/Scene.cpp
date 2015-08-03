
#include <Urho3D/Scene/Scene.h>

using namespace Urho3D;

extern "C" {
Scene * inline_c_Graphics_Urho3D_Scene_Scene_0_71bd9f03fe3be5799e8ecad5070bed138348ec24(Context * ptr_inline_c_0) {
return ( new Scene(ptr_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Scene_1_3e88d70dfcf5c3e5c84b1766711ec08173dc3a52(Scene * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<Scene> SharedScene;

extern "C" {
SharedScene * inline_c_Graphics_Urho3D_Scene_Scene_2_29de1b9e7b46d83ebdfc829d2087085e1d472cf2(Scene * ptr_inline_c_0) {
return ( new SharedScene(ptr_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Scene_Scene_3_21066cb0438e94b707a5fba59482881e3cce8ab1(SharedScene * ptr_inline_c_0) {
 delete ptr_inline_c_0;
}

}
