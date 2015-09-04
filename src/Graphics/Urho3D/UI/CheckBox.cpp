
#include <Urho3D/UI/CheckBox.h>

using namespace Urho3D;

extern "C" {
StringHash * inline_c_Graphics_Urho3D_UI_CheckBox_0_747e4fe9a3861cd1f034329105f1a9b2e7f4deb6() {
 
      static StringHash h = CheckBox::GetTypeStatic();  
      return &h;
    
}

}

extern "C" {
BorderImage * inline_c_Graphics_Urho3D_UI_CheckBox_1_c1292fa2ece7c963c980e8b881a77982f953bda3(CheckBox * ptr_inline_c_0) {
return ((BorderImage*)ptr_inline_c_0);
}

}

extern "C" {
CheckBox * inline_c_Graphics_Urho3D_UI_CheckBox_2_1c80fd12aeba99f1c2e97dd61d5b440c1b61e4bc(BorderImage * ptr_inline_c_0) {
return ((CheckBox*)ptr_inline_c_0);
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_CheckBox_3_701b04c98e1ac6646d69277c10a90b46814962d2(CheckBox * ptr_inline_c_0) {
return ((UIElement*)ptr_inline_c_0);
}

}

extern "C" {
CheckBox * inline_c_Graphics_Urho3D_UI_CheckBox_4_597f03b3e170745683e32794ffd8576babf97a63(UIElement * ptr_inline_c_0) {
return ((CheckBox*)ptr_inline_c_0);
}

}

extern "C" {
CheckBox * inline_c_Graphics_Urho3D_UI_CheckBox_5_edbd5a4e48b5b1bd1336751067e9da19eab1a0bd(Context * ptr_inline_c_0) {
return ( new CheckBox( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_CheckBox_6_6191e1903a7b65725989bd7618e00453599c9274(CheckBox * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<CheckBox> SharedCheckBox;

extern "C" {
void inline_c_Graphics_Urho3D_UI_CheckBox_7_025285059bf6481c1f3288a30b1de3094bbe466f(SharedCheckBox * ptr_inline_c_0, SharedCheckBox * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedCheckBox * inline_c_Graphics_Urho3D_UI_CheckBox_8_dc51e088dad648e9a521979fb2ead66dbdb8c244(CheckBox * ptr_inline_c_0) {
return ( new SharedCheckBox(ptr_inline_c_0) );
}

}

extern "C" {
CheckBox * inline_c_Graphics_Urho3D_UI_CheckBox_9_2fb80841d6e0ff803b27bf0d6bf2d108d33dfae3(SharedCheckBox * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}
