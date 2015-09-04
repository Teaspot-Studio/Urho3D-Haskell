
#include <Urho3D/Resource/XMLFile.h>

using namespace Urho3D;

extern "C" {
XMLFile * inline_c_Graphics_Urho3D_Resource_XMLFile_0_dc522d9170ec58e30f3dc99a3220dbc3926dd361(Context * ptr_inline_c_0) {
return ( new XMLFile( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Resource_XMLFile_1_da8432f862217d8559530de8dc12e5217d70b86e(XMLFile * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Resource_XMLFile_2_1dd1a1cacf1aedf71b020d1177cd984eaddca549(XMLFile * ptr_inline_c_0, const char * s_27_inline_c_1) {
return ( (int)ptr_inline_c_0->FromString(String(s_27_inline_c_1)) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Resource_XMLFile_3_5aa90bf3a6988cf629dc7a21184b39fa1ba0ebb7(XMLFile * ptr_inline_c_0, XMLFile * patch_inline_c_1) {
 ptr_inline_c_0->Patch(patch_inline_c_1) ;
}

}

extern "C" {
StringHash * inline_c_Graphics_Urho3D_Resource_XMLFile_4_c40151210db8c291f480ee91fe0b0b59ed7f6665() {
 
    static StringHash h = XMLFile::GetTypeStatic(); 
    return &h; 
    
}

}
