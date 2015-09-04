
#include <Urho3D/UI/Window.h>

using namespace Urho3D;

extern "C" {
StringHash * inline_c_Graphics_Urho3D_UI_Window_0_3a404ef3180b371777e6ccc366564486ea302c67() {
 
      static StringHash h = Window::GetTypeStatic();  
      return &h;
    
}

}

extern "C" {
BorderImage * inline_c_Graphics_Urho3D_UI_Window_1_36cfe9fe81bc5bea4c512d72971e6b03c70be7dc(Window * ptr_inline_c_0) {
return ((BorderImage*)ptr_inline_c_0);
}

}

extern "C" {
Window * inline_c_Graphics_Urho3D_UI_Window_2_384c42ea42ffa11162103caf62d709963a60d986(BorderImage * ptr_inline_c_0) {
return ((Window*)ptr_inline_c_0);
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Window_3_bf44b84cf96b77dae916b2541eb0f2d5e72451df(Window * ptr_inline_c_0) {
return ((UIElement*)ptr_inline_c_0);
}

}

extern "C" {
Window * inline_c_Graphics_Urho3D_UI_Window_4_e93beffc7a99833beb1793ee2e2ffedff4e09368(UIElement * ptr_inline_c_0) {
return ((Window*)ptr_inline_c_0);
}

}

extern "C" {
Window * inline_c_Graphics_Urho3D_UI_Window_5_0777ba0701b0ee19e3aefa6fe6070fdea9257c0e(Context * ptr_inline_c_0) {
return ( new Window( ptr_inline_c_0 ) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Window_6_a5ef574a598950cdb4128d675cb08e524fc9c921(Window * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef SharedPtr<Window> SharedWindow;

extern "C" {
void inline_c_Graphics_Urho3D_UI_Window_7_420ffed8d82fd710fc9852760909762b8b486d3d(SharedWindow * ptr_inline_c_0, SharedWindow * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedWindow * inline_c_Graphics_Urho3D_UI_Window_8_02875783e6aa1c50473fa8bac76131df4d915e9f(Window * ptr_inline_c_0) {
return ( new SharedWindow(ptr_inline_c_0) );
}

}

extern "C" {
Window * inline_c_Graphics_Urho3D_UI_Window_9_3c329c2292cedc26f8c43d69ef65bb78dccdc7c7(SharedWindow * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}
