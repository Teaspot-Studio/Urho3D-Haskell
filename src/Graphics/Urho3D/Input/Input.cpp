
#include <Urho3D/Input/Input.h>

using namespace Urho3D;

extern "C" {
Input * inline_c_Graphics_Urho3D_Input_Input_0_dc84e856eb2878d2fff9e0a75a98e1263155831b(Object * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetSubsystem<Input>() );
}

}

extern "C" {
Object * inline_c_Graphics_Urho3D_Input_Input_1_61cd612402e00bf4d78d3678ead0aed6b08cfa6d(Input * ptr_inline_c_0) {
return ( (Object*)ptr_inline_c_0 );
}

}

extern "C" {
Input * inline_c_Graphics_Urho3D_Input_Input_2_be4642f3d7c8cf2a75e83261467a8ee8bfa19c30(Object * ptr_inline_c_0) {
return ( (Input*)ptr_inline_c_0 );
}

}


template <class T>
class Traits
{
public:
    struct AlignmentFinder
    {
      char a; 
      T b;
    };

    enum {AlignmentOf = sizeof(AlignmentFinder) - sizeof(T)};
};


extern "C" {
int inline_c_Graphics_Urho3D_Input_Input_3_b4b2b4bcc93622d3be532ae07583a451d4be26bd(Input * ptr_inline_c_0) {
return (ptr_inline_c_0->GetNumJoysticks());
}

}

extern "C" {
SDL_JoystickID inline_c_Graphics_Urho3D_Input_Input_4_3904ee7f232605f01d5e74ee598dbd67e0785d29(Input * ptr_inline_c_0, XMLFile * layoutFile_inline_c_1, XMLFile * styleFile_inline_c_2) {
return ( ptr_inline_c_0->AddScreenJoystick(layoutFile_inline_c_1, styleFile_inline_c_2) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Input_Input_5_bc0a558f5cc81395596e817a1cd00b681159a2dd(Input * ptr_inline_c_0, SDL_JoystickID jid_inline_c_1, int flag_27_inline_c_2) {
 ptr_inline_c_0->SetScreenJoystickVisible(jid_inline_c_1, flag_27_inline_c_2 != 0) ;
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Input_Input_6_b25ecaaf13aa190ff152247cc56fea74a706661b(Input * ptr_inline_c_0) {
return (ptr_inline_c_0->GetNumTouches());
}

}

extern "C" {
TouchState * inline_c_Graphics_Urho3D_Input_Input_7_cb78c1075eee1671ff396de7a66284b4b5f6ce44(Input * ptr_inline_c_0, int i_27_inline_c_1) {
return (ptr_inline_c_0->GetTouch(i_27_inline_c_1));
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Input_Input_8_98c169a3499ee3b7433ecf2e0ed36812897f11a9(Input * ptr_inline_c_0, int flag_27_inline_c_1) {
 ptr_inline_c_0->SetMouseVisible(flag_27_inline_c_1) ;
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Input_Input_9_e9df97cfa4c596274e598b6a6a05b869466380dc() {
return ( (int)sizeof(TouchState) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Input_Input_10_2482d0c81c2d5857265b3f907d9b432ce0cb4988() {
return ( (int)Traits<TouchState>::AlignmentOf );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_Input_Input_11_0ce2a01b6990907ac7ad9d8e4056037e4372c6b1(TouchState * ptr_inline_c_0) {
return ( ptr_inline_c_0->touchID_ );
}

}

extern "C" {
IntVector2 * inline_c_Graphics_Urho3D_Input_Input_12_3977a891bb196c92d5d04421029ca57e62f69cc3(TouchState * ptr_inline_c_0) {
return ( &ptr_inline_c_0->position_ );
}

}

extern "C" {
IntVector2 * inline_c_Graphics_Urho3D_Input_Input_13_9b28f878dbf7d7203799f8a4644d0f0f5e2ed15f(TouchState * ptr_inline_c_0) {
return ( &ptr_inline_c_0->lastPosition_ );
}

}

extern "C" {
IntVector2 * inline_c_Graphics_Urho3D_Input_Input_14_8dfca01ef40c97125bc53d33d327537199127d00(TouchState * ptr_inline_c_0) {
return ( &ptr_inline_c_0->delta_ );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_Input_Input_15_f1b6b01c98b2222229b0d6345e314ba147862e76(TouchState * ptr_inline_c_0) {
return ( ptr_inline_c_0->pressure_ );
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_Input_Input_16_9033fb318795bec2e3ad8dbbaaefe96d71efc638(TouchState * ptr_inline_c_0) {
return ( ptr_inline_c_0->touchedElement_.Get() );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Input_Input_17_7d7ff33b5adbca5660db2a565a0789f7900f6b61(TouchState * ptr_inline_c_0, float tid_inline_c_1, TouchState * ptr_inline_c_2, IntVector2 * pv_inline_c_3, TouchState * ptr_inline_c_4, IntVector2 * lpv_inline_c_5, TouchState * ptr_inline_c_6, IntVector2 * dv_inline_c_7, TouchState * ptr_inline_c_8, float pr_inline_c_9, TouchState * ptr_inline_c_10, UIElement * e_inline_c_11) {
 
            ptr_inline_c_0->touchID_ = tid_inline_c_1;
            ptr_inline_c_2->position_ = *pv_inline_c_3;
            ptr_inline_c_4->lastPosition_ = *lpv_inline_c_5;
            ptr_inline_c_6->delta_ = *dv_inline_c_7;
            ptr_inline_c_8->pressure_ = pr_inline_c_9;
            ptr_inline_c_10->touchedElement_ = WeakPtr<UIElement>(e_inline_c_11);
          
}

}
