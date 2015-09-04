
#include <Urho3D/UI/LineEdit.h>

using namespace Urho3D;

extern "C" {
StringHash * inline_c_Graphics_Urho3D_UI_LineEdit_0_f23b1f2a52cb99d5a6851a7d99546d6a92969e94() {
 
      static StringHash h = LineEdit::GetTypeStatic();  
      return &h;
    
}

}

extern "C" {
BorderImage * inline_c_Graphics_Urho3D_UI_LineEdit_1_3fc0a81d1db274c15d94a5425db76d1e25aad507(LineEdit * ptr_inline_c_0) {
return ((BorderImage*)ptr_inline_c_0);
}

}

extern "C" {
LineEdit * inline_c_Graphics_Urho3D_UI_LineEdit_2_337c2d4240e4414f9cb83f1eeffb689eb54ab72a(BorderImage * ptr_inline_c_0) {
return ((LineEdit*)ptr_inline_c_0);
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_LineEdit_3_5f575e179459c3759abf20ec23fc6745a2344ee1(LineEdit * ptr_inline_c_0) {
return ((UIElement*)ptr_inline_c_0);
}

}

extern "C" {
LineEdit * inline_c_Graphics_Urho3D_UI_LineEdit_4_41319d54ca18ca864a0725888f533a979a0e2550(UIElement * ptr_inline_c_0) {
return ((LineEdit*)ptr_inline_c_0);
}

}

extern "C" {
LineEdit * inline_c_Graphics_Urho3D_UI_LineEdit_5_267c812dc15aa4824f823d8c5084568a46ce0ffa(Context * ptr_inline_c_0) {
return ( new LineEdit( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_LineEdit_6_c6919efdedf060817096cb2d39b98c3d67647ccd(LineEdit * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<LineEdit> SharedLineEdit;

extern "C" {
void inline_c_Graphics_Urho3D_UI_LineEdit_7_fff07e163307faa38ddb84f3427fa39744f3205d(SharedLineEdit * ptr_inline_c_0, SharedLineEdit * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedLineEdit * inline_c_Graphics_Urho3D_UI_LineEdit_8_42304ae3f18e6777cc9f8ac7f19ce2a7e2e5aa0a(LineEdit * ptr_inline_c_0) {
return ( new SharedLineEdit(ptr_inline_c_0) );
}

}

extern "C" {
LineEdit * inline_c_Graphics_Urho3D_UI_LineEdit_9_dff6be1146ef67db6025d72b1deeda952f804dd8(SharedLineEdit * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}
