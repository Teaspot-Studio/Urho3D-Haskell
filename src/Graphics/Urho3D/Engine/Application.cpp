
#include <Urho3D/Engine/Engine.h>

#include <Urho3D/Engine/Application.h>

#include <iostream>

using namespace Urho3D;


extern "C" typedef void (*haskellIOFunc)();

class ApplicationH : public Application {

  OBJECT(ApplicationH);

  public:

  ApplicationH(Context* context
    , haskellIOFunc setupFunc_
    , haskellIOFunc startFunc_
    , haskellIOFunc stopFunc_ ) : 
      Application(context)
    , setupFunc(setupFunc_)
    , startFunc(startFunc_)
    , stopFunc(stopFunc_)
  {

  }

  void setEngineParameter(const char* name, Variant* value) {
    engineParameters_[name] = *value;
  }

  SharedPtr<Engine>* getEgine() {
    return new SharedPtr<Engine>(engine_);
  }

  void Setup() {
    if (setupFunc) setupFunc();
  }

  void Start() {
    if (startFunc) startFunc();
  }

  void Stop() {
    if (stopFunc) stopFunc();
  }

  private:
  haskellIOFunc setupFunc = NULL;
  haskellIOFunc startFunc = NULL;
  haskellIOFunc stopFunc = NULL;
};


extern "C" {
ApplicationH * inline_c_Graphics_Urho3D_Engine_Application_0_51933debb8cd8845c36b6ac5517f9a3e384f3e2f(Context * ptr_inline_c_0, void (* setupFunc_inline_c_1)(), void (* startFunc_inline_c_2)(), void (* stopFunc_inline_c_3)()) {
return ( 
    new ApplicationH(ptr_inline_c_0
      , setupFunc_inline_c_1, startFunc_inline_c_2, stopFunc_inline_c_3)
  );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Application_1_35bf11e8cb8eb2ddc5ef56f4be4b588d9a0b6560(ApplicationH * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

extern "C" {
Object * inline_c_Graphics_Urho3D_Engine_Application_2_f8aafebbed1c6be3596849a76ecef97c0fb5ca2e(ApplicationH * ptr_inline_c_0) {
return ((Object*)ptr_inline_c_0);
}

}

extern "C" {
ApplicationH * inline_c_Graphics_Urho3D_Engine_Application_3_fbf44eaf19f6a2ad18a3c8313d6aabf142e8ab86(Object * ptr_inline_c_0) {
return ((ApplicationH*)ptr_inline_c_0);
}

}

typedef SharedPtr<ApplicationH> SharedApplicationH;

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Application_4_bbd38777cfefccd6aa7f6ac035f420883bcb3dc9(SharedApplicationH * ptr_inline_c_0, SharedApplicationH * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedApplicationH * inline_c_Graphics_Urho3D_Engine_Application_5_80599cea2dce6859335f9a7b0cd8a376faf49630(ApplicationH * ptr_inline_c_0) {
return ( new SharedApplicationH(ptr_inline_c_0) );
}

}

extern "C" {
ApplicationH * inline_c_Graphics_Urho3D_Engine_Application_6_429080893604d325c089145a0a992798c9986797(SharedApplicationH * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Application_7_a0dd09d013d5d18a59ee0800f2b8dece19d705e7(ApplicationH * ptr_inline_c_0, char * cname_inline_c_1, Variant * var_inline_c_2) {
 ptr_inline_c_0->setEngineParameter(cname_inline_c_1, var_inline_c_2) ;
}

}

typedef SharedPtr<Engine> SharedEngine;

extern "C" {
SharedEngine * inline_c_Graphics_Urho3D_Engine_Application_8_8ba75ba68111069666a3cc6cf5c00a481aa0d43c(ApplicationH * ptr_inline_c_0) {
return ( ptr_inline_c_0->getEgine() );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Engine_Application_9_b9139a718a9620ba008553cd2e68d76569b9771f(ApplicationH * ptr_inline_c_0) {
 ptr_inline_c_0->Run(); 
}

}
