
#include <Urho3D/Engine/Engine.h>

#include <Urho3D/Engine/Application.h>

using namespace Urho3D;

extern "C" {
Application * inline_c_Graphics_Urho3D_Engine_Application_0_1b9f750bace64ad95cfe950023d134ef75f68867(Context * ptr_inline_c_0) {
return ( new Application(ptr_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Application_1_ab4d42cee124936b6ce77709f7d5682e5a468dd0(Application * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}
