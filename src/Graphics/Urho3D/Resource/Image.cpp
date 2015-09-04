
#include <Urho3D/Resource/Image.h>

using namespace Urho3D;

extern "C" {
Image * inline_c_Graphics_Urho3D_Resource_Image_0_a64ccbea9ac696e73f1932cf7e7b1291728f38b4(Context * ptr_inline_c_0) {
return ( new Image( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Resource_Image_1_3ac5cb5b18df9bd0b04444433d88fc9a02c59e75(Image * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Resource_Image_2_2c6d3f6722c6086c72b8269f1818aee7f4707573(Image * ptr_inline_c_0, const char * path_27_inline_c_1) {
ptr_inline_c_0->SavePNG(String(path_27_inline_c_1));
}

}

extern "C" {
StringHash * inline_c_Graphics_Urho3D_Resource_Image_3_5866b087dbf38fd2c72b8711a23adc19dceff957() {
 
    static StringHash h = Image::GetTypeStatic(); 
    return &h; 
    
}

}
