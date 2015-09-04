
#include <Urho3D/UI/Button.h>

using namespace Urho3D;

extern "C" {
StringHash * inline_c_Graphics_Urho3D_UI_Button_0_0da147d74681015b7cf41016ef84922aa109102c() {
 
      static StringHash h = Button::GetTypeStatic();  
      return &h;
    
}

}

extern "C" {
Object * inline_c_Graphics_Urho3D_UI_Button_1_81fb13ebd7f016aeea13fe8f26f951853a3d6640(Button * ptr_inline_c_0) {
return ((Object*)ptr_inline_c_0);
}

}

extern "C" {
Button * inline_c_Graphics_Urho3D_UI_Button_2_25f833cbe07116460f97493e5402c0e4ca2fa09c(Object * ptr_inline_c_0) {
return ((Button*)ptr_inline_c_0);
}

}

extern "C" {
BorderImage * inline_c_Graphics_Urho3D_UI_Button_3_232a42f2617554ed8f58ec4bfc8f0914af59eb5d(Button * ptr_inline_c_0) {
return ((BorderImage*)ptr_inline_c_0);
}

}

extern "C" {
Button * inline_c_Graphics_Urho3D_UI_Button_4_973d1d1ac036ab249a28cbc0c05e4af2839d4541(BorderImage * ptr_inline_c_0) {
return ((Button*)ptr_inline_c_0);
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Button_5_36e6dd9b0bb6e652491c1bf9846e5a8a844b5033(Button * ptr_inline_c_0) {
return ((UIElement*)ptr_inline_c_0);
}

}

extern "C" {
Button * inline_c_Graphics_Urho3D_UI_Button_6_a1ae1c21d7f18fdc3f16a38994b121474e2c02f3(UIElement * ptr_inline_c_0) {
return ((Button*)ptr_inline_c_0);
}

}

extern "C" {
Button * inline_c_Graphics_Urho3D_UI_Button_7_2bce09688565a12a67c181e3c1b6afabafa5b772(Context * ptr_inline_c_0) {
return ( new Button( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Button_8_017f7e788949704f5b5a528135cba7e00bca5f34(Button * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<Button> SharedButton;

extern "C" {
void inline_c_Graphics_Urho3D_UI_Button_9_8110cdd2ac53fe52058271a942f85d0f4f13fdaf(SharedButton * ptr_inline_c_0, SharedButton * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedButton * inline_c_Graphics_Urho3D_UI_Button_10_ef754f6c273ae32a0786938c5fb8cc77a97ebafb(Button * ptr_inline_c_0) {
return ( new SharedButton(ptr_inline_c_0) );
}

}

extern "C" {
Button * inline_c_Graphics_Urho3D_UI_Button_11_3ff9734996de07a1affd9969365b6fdfd3f8160a(SharedButton * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}
