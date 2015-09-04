
#include <Urho3D/Core/Object.h>

using namespace Urho3D;



typedef HashMap<StringHash, Variant> HashMapStringHashVariant;
extern "C" typedef void (*Handler)(HashMapStringHashVariant*);

class HaskellHandler : public Object {
  OBJECT(HaskellHandler);

  Handler handler_;
public:
  HaskellHandler(Context* cntx, Handler handler) : 
      Object(cntx)
    , handler_(handler) 
  {
  }

  void runHanlder(StringHash, VariantMap& vm) {
    handler_(&vm);
  }
};


extern "C" {
HaskellHandler * inline_c_Graphics_Urho3D_Core_Object_0_3e38cd99bbfc470db547dc99550233f9442d4c10(Object * receiver_inline_c_0, void (* funImpl_inline_c_1)(HashMapStringHashVariant *)) {
 
    Context* cntx = receiver_inline_c_0->GetContext();
    return new HaskellHandler(cntx, funImpl_inline_c_1);
  
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Core_Object_1_8dd72ad2c11761924dc27908a1146bf29d3a5bf0(HaskellHandler * handler_inline_c_0) {
 delete handler_inline_c_0 ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Core_Object_2_594e04e9f54f57d3a1c81cd7d215f4f9ca392507(Object * objPtr_inline_c_0, StringHash * eventType_inline_c_1, HaskellHandler * handler_inline_c_2) {

    objPtr_inline_c_0->SubscribeToEvent(*eventType_inline_c_1
        , new EventHandlerImpl<HaskellHandler>(handler_inline_c_2, &HaskellHandler::runHanlder)
        );
    
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Core_Object_3_2f316b8f562c924798639e51d2ff4c52e0490a8e(Object * objPtr_inline_c_0, Object * senderPtr_inline_c_1, StringHash * eventType_inline_c_2, HaskellHandler * handler_inline_c_3) {

    objPtr_inline_c_0->SubscribeToEvent(senderPtr_inline_c_1, *eventType_inline_c_2
        , new EventHandlerImpl<HaskellHandler>(handler_inline_c_3, &HaskellHandler::runHanlder)
        );
    
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Core_Object_4_396d9556b09165a10062752e47605d8938b8f0a8(Object * ptr_inline_c_0, StringHash * eventType_inline_c_1) {
 ptr_inline_c_0->UnsubscribeFromEvent(*eventType_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_Core_Object_5_46aedf1deda3835b3228eb42707805e473aa8c84(Object * ptr_inline_c_0, Object * senderPtr_inline_c_1, StringHash * eventType_inline_c_2) {
 ptr_inline_c_0->UnsubscribeFromEvent(senderPtr_inline_c_1, *eventType_inline_c_2) ;
}

}

extern "C" {
Context * inline_c_Graphics_Urho3D_Core_Object_6_64384831791b800ea66d54249973642c80f28828(Object * objPtr_inline_c_0) {
return ( objPtr_inline_c_0->GetContext() );
}

}
