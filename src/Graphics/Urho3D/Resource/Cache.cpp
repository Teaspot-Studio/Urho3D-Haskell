
#include <Urho3D/Resource/ResourceCache.h>

using namespace Urho3D;

extern "C" {
ResourceCache * inline_c_Graphics_Urho3D_Resource_Cache_0_fab4f43bbff98d304ec84e2b2b144c379ee26190(Context * ptr_inline_c_0) {
return ( new ResourceCache( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Resource_Cache_1_99f8084f61ef01a56b53af6fa835773d23817d9c(ResourceCache * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

extern "C" {
Resource * inline_c_Graphics_Urho3D_Resource_Cache_2_8edf90745c579adbd2c19f5ad8d0500f93e12196(ResourceCache * ptr_inline_c_0, StringHash * rest_inline_c_1, const char * name_27_inline_c_2, int sendEvent_27_inline_c_3) {
return ( ptr_inline_c_0->GetResource(*rest_inline_c_1, String(name_27_inline_c_2), sendEvent_27_inline_c_3 != 0) );
}

}

extern "C" {
ResourceCache * inline_c_Graphics_Urho3D_Resource_Cache_3_221faa70d974126bea81c7caa0644e3d4cdfea48(Object * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetSubsystem<ResourceCache>() );
}

}
