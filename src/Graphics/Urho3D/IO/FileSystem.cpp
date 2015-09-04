
#include <Urho3D/IO/FileSystem.h>

using namespace Urho3D;

extern "C" {
String * inline_c_Graphics_Urho3D_IO_FileSystem_0_72084671619105850245df3f429d868e178d4f8f(FileSystem * ptr_inline_c_0, const char * org_27_inline_c_1, const char * app_27_inline_c_2) {
return (
      new String( ptr_inline_c_0->GetAppPreferencesDir(String(org_27_inline_c_1), String(app_27_inline_c_2)) )
    );
}

}

extern "C" {
String * inline_c_Graphics_Urho3D_IO_FileSystem_1_2f89e33fe41f09a94c32b7d03a2f2954b5414eee(FileSystem * ptr_inline_c_0) {
return ( new String(ptr_inline_c_0->GetProgramDir()) );
}

}

extern "C" {
FileSystem * inline_c_Graphics_Urho3D_IO_FileSystem_2_102c836286e86555c31f634ed8284eab1bc6f76e(Object * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetSubsystem<FileSystem>() );
}

}

extern "C" {
Object * inline_c_Graphics_Urho3D_IO_FileSystem_3_9e81d9b696527f44888f0bc53852939dcbd53d7a(FileSystem * ptr_inline_c_0) {
return ( (Object*)ptr_inline_c_0 );
}

}

extern "C" {
FileSystem * inline_c_Graphics_Urho3D_IO_FileSystem_4_4ac77de9f2839c20c220af99c9b74ddb371e8a6d(Object * ptr_inline_c_0) {
return ( (FileSystem*)ptr_inline_c_0 );
}

}
