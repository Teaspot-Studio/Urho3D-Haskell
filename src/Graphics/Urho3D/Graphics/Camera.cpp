
#include <Urho3D/Graphics/Camera.h>

using namespace Urho3D;

extern "C" {
float inline_c_Graphics_Urho3D_Graphics_Camera_0_06c13811ae2090db996dbee9cbbd43b6b214985e(Camera * ptr_inline_c_0) {
return (ptr_inline_c_0->GetFov());
}

}

extern "C" {
StringHash * inline_c_Graphics_Urho3D_Graphics_Camera_1_d84bb22a0826d2b0b5b36b4e85b923138918f7a7() {

    static StringHash h = Camera::GetTypeStatic();
    return &h;
  
}

}

extern "C" {
Component * inline_c_Graphics_Urho3D_Graphics_Camera_2_adde23f4f0ec327691eddccbe7385718e4962580(Camera * ptr_inline_c_0) {
return ( (Component*)ptr_inline_c_0 );
}

}

extern "C" {
Camera * inline_c_Graphics_Urho3D_Graphics_Camera_3_1c727a4618ffe6ac2dee5b57012b264ce0a0847d(Component * ptr_inline_c_0) {
return ( (Camera*)ptr_inline_c_0 );
}

}
