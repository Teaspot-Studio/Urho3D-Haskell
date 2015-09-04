
#include <Urho3D/UI/UIElement.h>

using namespace Urho3D;

typedef HashMap<StringHash, Variant> HashMapStringHashVariant;

typedef SharedPtr<UIElement> SharedUIElement;

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_0_31698d57024225043f71a2f6bfde659a5ab8149b(SharedUIElement * ptr_inline_c_0, SharedUIElement * ptr_inline_c_1) {
 if (ptr_inline_c_0) { delete ptr_inline_c_1; } ;
}

}

extern "C" {
SharedUIElement * inline_c_Graphics_Urho3D_UI_Element_1_8c2357ee127d556f68a6618889b21b90cc90840a(UIElement * ptr_inline_c_0) {
return ( new SharedUIElement(ptr_inline_c_0) );
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_2_25b7573ee077d513b01c5560827b9f626bbf466f(SharedUIElement * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Get());
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_3_f2038baa6d30e1eec6746e3f82fa03057c7efc71(UIElement * ptr_27_inline_c_0, StringHash * hash_inline_c_1, const char * name_27_inline_c_2, int index_27_inline_c_3) {
return ( ptr_27_inline_c_0->CreateChild(*hash_inline_c_1, String(name_27_inline_c_2), index_27_inline_c_3) );
}

}

extern "C" {
unsigned inline_c_Graphics_Urho3D_UI_Element_4_527dab13009817c6da189f6a85d908436ea2538f() {
return ( M_MAX_UNSIGNED );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_5_2ca8132b2f13ab5688adc52fd5a63f6f34ae9986(UIElement * ptr_27_inline_c_0, IntVector2 * v_27_inline_c_1) {
 ptr_27_inline_c_0->SetSize(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_6_cd158deec78dbaa21b768d9abfb74a289670d95b(UIElement * ptr_27_inline_c_0, int width_27_inline_c_1, int height_27_inline_c_2) {
 ptr_27_inline_c_0->SetSize(width_27_inline_c_1, height_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_7_e03151d2799d6b846b013971573bef464d341f88(UIElement * ptr_inline_c_0, int width_27_inline_c_1) {
 ptr_inline_c_0->SetWidth(width_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_8_28ae5f93769e57da5dbab0bff42203aa12ca062c(UIElement * ptr_inline_c_0, int height_27_inline_c_1) {
 ptr_inline_c_0->SetWidth(height_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_9_1e092b22f31058235e51eff502854d8d31f72dc0(UIElement * ptr_inline_c_0, int w_27_inline_c_1) {
 ptr_inline_c_0->SetMinWidth(w_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_10_8e5995e9c33a1c7f9a6c586ee4a7d3085150b73e(UIElement * ptr_inline_c_0, int h_27_inline_c_1) {
 ptr_inline_c_0->SetMinHeight(h_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_11_a529f08b428caa689d9a91bc14594aca7dd81bf4(UIElement * ptr_inline_c_0, IntVector2 * v_27_inline_c_1) {
 ptr_inline_c_0->SetMinSize(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_12_b397b253941d3279c0633e4c5cd96d3390931d87(UIElement * ptr_inline_c_0, int mw_27_inline_c_1, int mh_27_inline_c_2) {
 ptr_inline_c_0->SetMinSize(mw_27_inline_c_1, mh_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_13_c7f243a0526bd093485c0ca541715f24411954a9(UIElement * ptr_inline_c_0, int w_27_inline_c_1) {
 ptr_inline_c_0->SetMaxWidth(w_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_14_faaac0311b40c7f5645fad0c643b6d67d1fa2c9e(UIElement * ptr_inline_c_0, int h_27_inline_c_1) {
 ptr_inline_c_0->SetMaxHeight(h_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_15_ce43e4ed8af0fad34c1540fce9a6d55482229287(UIElement * ptr_inline_c_0, IntVector2 * v_27_inline_c_1) {
 ptr_inline_c_0->SetMaxSize(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_16_e68ce34c5b5d98977ee8dcdb099d485f82c04e7c(UIElement * ptr_inline_c_0, int mw_27_inline_c_1, int mh_27_inline_c_2) {
 ptr_inline_c_0->SetMaxSize(mw_27_inline_c_1, mh_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_17_1a48f811e8d37ab7fbb7ac7f6ef422d430c859ba(UIElement * ptr_inline_c_0, int w_27_inline_c_1) {
 ptr_inline_c_0->SetFixedWidth(w_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_18_2c21f9d25c1e60c553f7fe36f7751cab8b5bf436(UIElement * ptr_inline_c_0, int h_27_inline_c_1) {
 ptr_inline_c_0->SetFixedHeight(h_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_19_20b597c5f934f677c4b631fb1cd24173dc167285(UIElement * ptr_inline_c_0, IntVector2 * v_27_inline_c_1) {
 ptr_inline_c_0->SetFixedSize(*v_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_20_40087347d2b53d232dc7bda91d9dd9cf346c9d0b(UIElement * ptr_inline_c_0, int mw_27_inline_c_1, int mh_27_inline_c_2) {
 ptr_inline_c_0->SetFixedSize(mw_27_inline_c_1, mh_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_21_932e95cf6336883620f20fbf72a6384e349e490d(UIElement * ptr_27_inline_c_0, int ha_27_inline_c_1, int va_27_inline_c_2) {
 ptr_27_inline_c_0->SetAlignment((HorizontalAlignment)ha_27_inline_c_1, (VerticalAlignment)va_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_22_4cf351d201b4815492409c63f3cde5cc25d35439(UIElement * ptr_27_inline_c_0, int ha_27_inline_c_1) {
 ptr_27_inline_c_0->SetHorizontalAlignment((HorizontalAlignment)ha_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_23_9b37d858e9a04a2923f7d62869b86bb8c76a6350(UIElement * ptr_27_inline_c_0, int va_27_inline_c_1) {
 ptr_27_inline_c_0->SetVerticalAlignment((VerticalAlignment)va_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_24_f181e5865dd9fd98742a47a59d5363b49ff451f6(UIElement * ptr_inline_c_0, IntRect * r_27_inline_c_1) {
 ptr_inline_c_0->SetClipBorder(*r_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_25_4b2838f87e71c41d050c76777ce55d038ebb2a10(UIElement * ptr_27_inline_c_0, float opacity_27_inline_c_1) {
 ptr_27_inline_c_0->SetOpacity(opacity_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_26_aa8f4268f1ca47e305b71b45393ab278faf2b584(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetBringToFront(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_27_a468ad038c15c647e210057ffa67dd2a7b8658a5(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetBringToBack(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_28_1afca7b31c9a9b83cf5cfd992782a8b970a09808(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetClipChildren(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_29_8a4e0e5bc4a7ff9046a5d771c58688efb752ab0b(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetSortChildren(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_30_aaf4c8efd454dce1c7af199c7102ae83c74ebc5f(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetUseDerivedOpacity(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_31_21ceb2e104ac71693d7a2f93766d97d8c180efb4(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetEnabled(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_32_0e6cf61b1f5dcaf23c91d8a3324150ae27c8ead2(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetDeepEnabled(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_33_d36f50d23753fe85b49386d9f2c75c2b7ebcec07(UIElement * ptr_inline_c_0) {
 ptr_inline_c_0->ResetDeepEnabled() ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_34_bf75e8685cd9f92cd43185d1c1084575df2f7e80(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetEnabledRecursive(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_35_d235149d4966a7de9f6ee682e4ab869d415f8e1d(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetEditable(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_36_9bf487bcda25ffd170a742b940b052f0bfbc6848(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetFocus(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_37_a62779ca3deffbff27309c744a64d882988375e9(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetSelected(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_38_38d18bee60811390215780282b1d61d189d8d803(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetVisible(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_39_9d449fbe5e570bd390f30f8be6d4adec3579f98f(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetInternal(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_40_5c0f8216e70db18cd55f5993e48b44c72c3a3403(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetElementEventSender(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_41_f14bf2a4bd64938b7759c67eb13077ac7f7665dd(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetHovering(b_27_inline_c_1 != 0) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_42_6779f08a0b58ee88e6ef755be75655c938d92b9c(UIElement * ptr_inline_c_0, IntVector2 * o_27_inline_c_1) {
 ptr_inline_c_0->SetChildOffset(*o_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_43_6e616bc74198617066fbe365ada00d7d4fa355df(UIElement * ptr_inline_c_0, IntRect * s_27_inline_c_1) {
 ptr_inline_c_0->AdjustScissor(*s_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_44_b424f0fc8a0de236e41c9a3095ac61f280261c62(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetFocusMode((FocusMode)b_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_45_793f4add8b7bdd8e4ca89af884208ee10bb36cf2(UIElement * ptr_27_inline_c_0, int p_27_inline_c_1) {
 ptr_27_inline_c_0->SetDragDropMode(p_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_46_70e21e201786b16d35154ee34a56aa98771cccd6(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
 ptr_inline_c_0->SetTraversalMode((TraversalMode)b_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_47_e0603e651445ce5c1de92536bd7e9844af521233(UIElement * ptr_27_inline_c_0, int p_27_inline_c_1) {
 ptr_27_inline_c_0->SetIndent(p_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_48_08673e3db73e7f3e4047c7f4625270629f93118c(UIElement * ptr_27_inline_c_0, int p_27_inline_c_1) {
 ptr_27_inline_c_0->SetIndentSpacing(p_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_49_ee17d724186aca5ea7a6113400ec19794f456c28(UIElement * ptr_27_inline_c_0, int p_27_inline_c_1) {
 ptr_27_inline_c_0->SetPriority(p_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_50_1015a52130dddfb327aad85e3e08e7f9504a58dc(UIElement * ptr_inline_c_0) {
 ptr_inline_c_0->UpdateLayout() ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_51_9c724aa3a6a6bed425c1c9635b8ddeb27da4413f(UIElement * ptr_inline_c_0) {
 ptr_inline_c_0->DisableLayoutUpdate() ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_52_1ae5e2d2e124614aa756469952cad62c93ae828f(UIElement * ptr_inline_c_0) {
 ptr_inline_c_0->EnableLayoutUpdate() ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_53_313463404fc6ab5886e93ad783e169498487f9c4(UIElement * ptr_inline_c_0) {
 ptr_inline_c_0->BringToFront() ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_54_f9feb46714a21a68e71c9daccb3e1470b261e40a(UIElement * ptr_inline_c_0, IntVector2 * v_27_inline_c_1) {
 ptr_inline_c_0->SetPosition(*v_27_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_55_ee9dda1d5e764d8d54870bd8a44f7ca19450079d(UIElement * ptr1_inline_c_0, UIElement * ptr2_inline_c_1) {
 ptr1_inline_c_0->AddChild(ptr2_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_56_f593f823580ad8e6fe155d0655e6193d69f84449(UIElement * ptr1_inline_c_0, int i_27_inline_c_1, UIElement * ptr2_inline_c_2) {
 ptr1_inline_c_0->InsertChild(i_27_inline_c_1, ptr2_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_57_f1c0f671429a0c5360b804d1d66e07c0574e24e0(UIElement * ptr1_inline_c_0, UIElement * ptr2_inline_c_1, int i_27_inline_c_2) {
 ptr1_inline_c_0->RemoveChild(ptr2_inline_c_1, i_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_58_c9447af034f3075b16ce8582517d9cbe8635dd4b(UIElement * ptr_inline_c_0, int i_27_inline_c_1) {
 ptr_inline_c_0->RemoveChildAtIndex(i_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_59_0a16e9d547c30a77417cc2f9c1c1c92ea15bd275(UIElement * ptr_inline_c_0) {
 ptr_inline_c_0->RemoveAllChildren() ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_60_a1ffed7bcef2c48e272b82c99404534b1adbbd1a(UIElement * ptr_inline_c_0) {
 ptr_inline_c_0->Remove() ;
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_61_b8e5930a3c81ff539a8af6f6256b9d4e99269e06(UIElement * ptr1_inline_c_0, UIElement * ptr2_inline_c_1) {
return ( ptr1_inline_c_0->FindChild(ptr2_inline_c_1) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_62_6dfeaa2188e67e39ebb58323c0653232990cb54a() {
return (M_MAX_UNSIGNED);
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_63_6dfeaa2188e67e39ebb58323c0653232990cb54a() {
return (M_MAX_UNSIGNED);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_64_e764a5ae8d28d6ff9860af141505e2e9cb79e717(UIElement * ptr1_inline_c_0, UIElement * ptr2_inline_c_1, int i_27_inline_c_2) {
 ptr1_inline_c_0->SetParent(ptr2_inline_c_1, i_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_65_7e0ca389d3405846311eaf9eb0ed7338f2ced647(UIElement * ptr_inline_c_0, StringHash * name_27_inline_c_1, Variant * a_27_inline_c_2) {
 ptr_inline_c_0->SetVar(*name_27_inline_c_1, *a_27_inline_c_2) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_66_989aacd5eeb4045f88fd41854e28252a6637fd8e(UIElement * ptr_inline_c_0, XMLFile * style_inline_c_1) {
 ptr_inline_c_0->SetDefaultStyle(style_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_67_5c3c9d2e62ff739d0739a2a9a3e9a60a57a5158e(UIElement * ptr_inline_c_0, const char * str_inline_c_1) {
 ptr_inline_c_0->SetName(str_inline_c_1) ;
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_68_14806bc6e4fb74ae46c28250157622e2045a65d4(UIElement * ptr_inline_c_0, XMLFile * xml_27_inline_c_1) {
return ( (int)ptr_inline_c_0->SetStyleAuto(xml_27_inline_c_1) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_69_04f931c96bca0f8ede96b3a4557f7aa997393884(UIElement * ptr_inline_c_0, const char * str_27_inline_c_1, XMLFile * xml_27_inline_c_2) {
return ( ptr_inline_c_0->SetStyle(String(str_27_inline_c_1), xml_27_inline_c_2) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_70_c6dcd8699b1cf1bda71ea70f928f4b514740d652(UIElement * ptr_inline_c_0, XMLElement * xml_27_inline_c_1) {
return ( ptr_inline_c_0->SetStyle(*xml_27_inline_c_1) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_71_9eedc1b61b404c08147e3cb4adc754f0a686feef(UIElement * ptr_inline_c_0, int mode_27_inline_c_1, int spacing_27_inline_c_2, IntRect * border_27_inline_c_3) {
 ptr_inline_c_0->SetLayout((LayoutMode)mode_27_inline_c_1, spacing_27_inline_c_2, *border_27_inline_c_3) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_72_0ca6544165720f9a924bdbf3aff2d246e29266fa(UIElement * ptr_inline_c_0, int mode_27_inline_c_1) {
 ptr_inline_c_0->SetLayoutMode((LayoutMode)mode_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_73_6a04881c3950eea7ae4be274a4bc806c17a7e8b0(UIElement * ptr_inline_c_0, int spacing_27_inline_c_1) {
 ptr_inline_c_0->SetLayoutSpacing(spacing_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_74_933348a9d7254ce490603cde8c2b81d15d24e188(UIElement * ptr_inline_c_0, IntRect * border_27_inline_c_1) {
 ptr_inline_c_0->SetLayoutBorder(*border_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_75_846ab131afb5827b410f645981ca802f93693d71(UIElement * ptr_inline_c_0, Vector2 * scale_27_inline_c_1) {
 ptr_inline_c_0->SetLayoutFlexScale(*scale_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_76_03ca8f82ce30190472f5de35e23b47cab9eddfa6(UIElement * ptr_inline_c_0, Color * color_27_inline_c_1) {
 ptr_inline_c_0->SetColor(*color_27_inline_c_1) ;
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_77_ac282728305e8f5dbcbe59233418923cb5df0104(UIElement * ptr_inline_c_0, int corner_27_inline_c_1, Color * color_27_inline_c_2) {
 ptr_inline_c_0->SetColor((Corner)corner_27_inline_c_1, *color_27_inline_c_2) ;
}

}

extern "C" {
const IntVector2 * inline_c_Graphics_Urho3D_UI_Element_78_7e8eba7c727b49f3f44e508c6d97c1881e169d0d(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetSize() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_79_10f2c2b5629e7d8f1d91ade03a51230415d0d017(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetWidth() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_80_4fc87f5af3be2da765759897348627a2982f4fa6(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetHeight() );
}

}

extern "C" {
const IntVector2 * inline_c_Graphics_Urho3D_UI_Element_81_32bcf11fb46f9827dc81e62cc540c405befef18a(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetPosition() );
}

}

extern "C" {
const String * inline_c_Graphics_Urho3D_UI_Element_82_a00512e543d47987b39f2b1df7f2e5d51dbe82d5(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetName() );
}

}

extern "C" {
const Color * inline_c_Graphics_Urho3D_UI_Element_83_face787fcb975bda49896743f171aad3d5fda9bd(UIElement * ptr_inline_c_0, int corner_27_inline_c_1) {
return ( &ptr_inline_c_0->GetColor((Corner)corner_27_inline_c_1) );
}

}

extern "C" {
const IntVector2 * inline_c_Graphics_Urho3D_UI_Element_84_e50c98d5deda9929b08aa681997260af77c10775(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetMinSize() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_85_29106b2e4a39ffd7012896d4f31760825af018ea(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetMinWidth() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_86_f94a3122f206d17b4194a9dc0e5d8ac06290b2a6(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetMinHeight() );
}

}

extern "C" {
const IntVector2 * inline_c_Graphics_Urho3D_UI_Element_87_1c103878a05329b8eb4492048a86eeb70b697f59(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetMaxSize() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_88_593a9cf6d30a62aea415316ff61cef2704b4c6e8(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetMaxWidth() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_89_25a2c70ace5fdc365e87d8f499cc2fe856037afd(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetMaxHeight() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_90_3149180ab74da707c11c024a0e64ac6dbb6c7bce(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsFixedSize() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_91_5ae4de1c8412d8dbcc172215b1fa3445687982a0(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsFixedWidth() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_92_57d34760d2ed93d557e46f84fecc42ac75fcd849(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsFixedHeight() );
}

}

extern "C" {
const IntVector2 * inline_c_Graphics_Urho3D_UI_Element_93_ef7e1659268c3732dd5ed7ee9e3c4706650bc76a(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetChildOffset() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_94_2305232b0ce0fb5db1ee7ceebc88e9e1956559ef(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetHorizontalAlignment() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_95_6826171928ca38ea8cb67ddbda68c2fa06038601(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetVerticalAlignment() );
}

}

extern "C" {
const IntRect * inline_c_Graphics_Urho3D_UI_Element_96_05d031795a6ddefd69185286e5a3890887f4a0a2(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetClipBorder() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_97_23c712a5a504dcd9691d2aa0f490b8e521c0ee1a(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetPriority() );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_UI_Element_98_9103a1ee415cb74117433e250ebc579f57db9743(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetOpacity() );
}

}

extern "C" {
float inline_c_Graphics_Urho3D_UI_Element_99_c300eefc854228e976f6381d10e711e75090fdbd(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetDerivedOpacity() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_100_d8324845e84b90df5a65492378219ec9bae764c0(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetBringToFront() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_101_0a2ac32987af09d26aea6b54ab4ee563ade5245b(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetBringToBack() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_102_3ae091494d97defbe1996d115d04ba98cee887a8(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetClipChildren() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_103_5664943bff061e712e4679fb074d5fbcc61d4ebd(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetSortChildren() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_104_20cf0bf748f76f03031d24c63ff5a6457ecb4f65(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetUseDerivedOpacity() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_105_83d051d611cdf763bc7f9600d9fa896faa7edff7(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->HasFocus() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_106_b207ca952f10d576f063c3b7dfd39f900395caff(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsEnabled() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_107_9407b43e1e0dda14547acd4627dca0056a92434e(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsEnabledSelf() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_108_590190572713dc353dadfb585716a6f1dca65627(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsEditable() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_109_cb5d9316a079ac6f9badce6c6f16470b82db2704(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsSelected() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_110_560eb54fe79ac7d0e9c30046b9d070368a677dd9(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsVisible() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_111_f325b9f83aa9ce1c1651bc3a9b3320fd9fc09185(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsVisibleEffective() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_112_31d753761895345527b96a2b776bc759c50d0b82(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsHovering() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_113_09ddf77d0816cbc76fc27c35b4509be24e68e988(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsInternal() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_114_efbacacae695e33d9154b249c840061dc3f79fe4(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->HasColorGradient() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_115_bcb01a04d3506f02e84acc5c0315000057635f6b(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetFocusMode() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_116_0d28a2e5645621754e53de22518f58375e807e53(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetDragDropMode() );
}

}

extern "C" {
const char * inline_c_Graphics_Urho3D_UI_Element_117_fcd62dfa126f720476a2af243fcd61335aa460a7(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetAppliedStyle().CString() );
}

}

extern "C" {
XMLFile * inline_c_Graphics_Urho3D_UI_Element_118_e279476e4b52f24bb2c6ebb0b37557da8f1c34e0(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
return ( ptr_inline_c_0->GetDefaultStyle(b_27_inline_c_1 != 0) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_119_92205aaddc0215c79d31657c93a1275be4d21eda(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetLayoutMode() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_120_dbbfc796758fd3f511b64116cf26796c688a35f3(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetLayoutSpacing() );
}

}

extern "C" {
const IntRect * inline_c_Graphics_Urho3D_UI_Element_121_6f7320d03c1be9f063679e59b4e75c3f293ee7fb(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetLayoutBorder() );
}

}

extern "C" {
const Vector2 * inline_c_Graphics_Urho3D_UI_Element_122_9422fc65797d3a964947921e4bcb07131dd012ce(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetLayoutFlexScale() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_123_25c7c13ce3342371010bea8da84c6edd90533c61(UIElement * ptr_inline_c_0, int b_27_inline_c_1) {
return ( ptr_inline_c_0->GetNumChildren(b_27_inline_c_1 != 0) );
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_124_6cc7c38dbf7a30d83aa710a77f9a53033448090e(UIElement * ptr_inline_c_0, unsigned i_27_inline_c_1) {
return ( ptr_inline_c_0->GetChild(i_27_inline_c_1) );
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_125_f6284331621be3163f70eed6b0d529ff8583cc01(UIElement * ptr_inline_c_0, const char * name_27_inline_c_1, int b_27_inline_c_2) {
return ( ptr_inline_c_0->GetChild(String(name_27_inline_c_1), b_27_inline_c_2 != 0) );
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_126_13bcaacb58c9270b62c2d9fe290ed8546b189d20(UIElement * ptr_inline_c_0, StringHash * key_27_inline_c_1, Variant * a_inline_c_2, int b_27_inline_c_3) {
return ( ptr_inline_c_0->GetChild(*key_27_inline_c_1, *a_inline_c_2, b_27_inline_c_3 != 0) );
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_127_10d1e2cef59bae89d6b3d8792b64f4de8439855e(Context * ptr_inline_c_0) {
return ( new UIElement(ptr_inline_c_0) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_128_a60fa8afa17088391f96836ea17f62fe6867663c(UIElement * ptr_inline_c_0) {
 delete ptr_inline_c_0 ;
}

}

typedef PODVector<UIElement*> PODVectorUIElementPtr;

extern "C" {
PODVectorUIElementPtr * inline_c_Graphics_Urho3D_UI_Element_129_cf11f6f28c60f375e7d2104305d932d2858f0d0d() {
return (new PODVector<UIElement*>());
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_130_52e8af7ee7f7d7e43538c26892d7983893b924b6(PODVectorUIElementPtr * _ptr_inline_c_0) {
 delete _ptr_inline_c_0;
}

}

extern "C" {
unsigned inline_c_Graphics_Urho3D_UI_Element_131_42dda8ec43f862d86a99f594ca66b51e622d5213(PODVectorUIElementPtr * _ptr_inline_c_0) {
return (_ptr_inline_c_0->Size());
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_132_82f728c4caf64aec3858982c505a0b864042a991(PODVectorUIElementPtr * _ptr_inline_c_0, int _i_27_inline_c_1) {
return ((*_ptr_inline_c_0)[_i_27_inline_c_1]);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_133_5bd739c36183ef5208edb85e3751f401cc1fe1de(PODVectorUIElementPtr * _ptr_inline_c_0, UIElement * _elem_inline_c_1) {
_ptr_inline_c_0->Push(_elem_inline_c_1);
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_134_69dc79d8ffc5fd0035d49e8bd1bb8b0a713a81b9(UIElement * ptr_inline_c_0, PODVectorUIElementPtr * v_inline_c_1, int b_27_inline_c_2) {
 ptr_inline_c_0->GetChildren(*v_inline_c_1, b_27_inline_c_2 != 0) ;
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_135_4a58ec4621a5e0db266a6d9d5e5a49f1f5057914(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetParent() );
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_136_ed8b5f139382615a0335f9b653ccfc3386baf7d4(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetRoot() );
}

}

extern "C" {
const Color * inline_c_Graphics_Urho3D_UI_Element_137_77b94a05f18c35f26df47477b250445617ae6b71(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetDerivedColor() );
}

}

extern "C" {
const Variant * inline_c_Graphics_Urho3D_UI_Element_138_306a0a8fe90b9070019c1cbd939adec7efa686b7(UIElement * ptr_inline_c_0, StringHash * key_27_inline_c_1) {
return ( &ptr_inline_c_0->GetVar(*key_27_inline_c_1) );
}

}

extern "C" {
const HashMapStringHashVariant * inline_c_Graphics_Urho3D_UI_Element_139_7959f3aac6c989922bb409e34a767a5575f6a740(UIElement * ptr_inline_c_0) {
return ( &ptr_inline_c_0->GetVars() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_140_3bd25f94f9b1fd6716c99e650326a366db14e9d1(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetDragButtonCombo() );
}

}

extern "C" {
unsigned inline_c_Graphics_Urho3D_UI_Element_141_7b4aa54c39d1caddc10b624c407efe2068795a82(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetDragButtonCount() );
}

}

extern "C" {
IntVector2 * inline_c_Graphics_Urho3D_UI_Element_142_55c8a016335da7315bb8f9206febc93a1fdf77e6(UIElement * ptr_inline_c_0, IntVector2 * v_27_inline_c_1) {
return ( new IntVector2(ptr_inline_c_0->ScreenToElement(*v_27_inline_c_1)) );
}

}

extern "C" {
IntVector2 * inline_c_Graphics_Urho3D_UI_Element_143_5218f1452f22871734c1c78c4f3e47f8763703f3(UIElement * ptr_inline_c_0, IntVector2 * v_27_inline_c_1) {
return ( new IntVector2(ptr_inline_c_0->ElementToScreen(*v_27_inline_c_1)) );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_144_0771a92c6cd22ef5dd27505ceb94309d477b8bdd(UIElement * ptr_inline_c_0, IntVector2 * v_27_inline_c_1, int b_27_inline_c_2) {
return (ptr_inline_c_0->IsInside(*v_27_inline_c_1, b_27_inline_c_2 != 0));
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_145_03580c4112ef9075be1673bbe6fb9738892d6343(UIElement * ptr_inline_c_0, IntVector2 * v_27_inline_c_1, int b_27_inline_c_2) {
return (ptr_inline_c_0->IsInsideCombined(*v_27_inline_c_1, b_27_inline_c_2 != 0));
}

}

extern "C" {
IntRect * inline_c_Graphics_Urho3D_UI_Element_146_8bf9354e12292b7248b580f5c9616ce0bc497792(UIElement * ptr_inline_c_0) {
return ( new IntRect(ptr_inline_c_0->GetCombinedScreenRect()) );
}

}

extern "C" {
void inline_c_Graphics_Urho3D_UI_Element_147_c8a5b3a71e07acbd8fa735f5ed132e8c8664427e(UIElement * ptr_inline_c_0) {
 ptr_inline_c_0->SortChildren() ;
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_148_aade9a4fbf71bf0bdb345a8b7b7b0cd3addcdc82(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetLayoutMinSize() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_149_6473ad02298465cfa56e0e9d93bbeed349156179(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetLayoutMaxSize() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_150_ff587938dc680c2df523e70c7b027b8b0bce1404(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetIndent() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_151_0de545ccc355c03d00c2ba2ca63b1cf0947955f9(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetIndentSpacing() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_152_d66f4055286d065084221607fcca23c9c3cf72ea(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetIndentWidth() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_153_e09c80456b1adca5cb1320a5e5b7f6f41ea3cf21(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->GetTraversalMode() );
}

}

extern "C" {
int inline_c_Graphics_Urho3D_UI_Element_154_bd6618612c548c4cd8cdb54497363f1b3157058b(UIElement * ptr_inline_c_0) {
return ( (int)ptr_inline_c_0->IsElementEventSender() );
}

}

extern "C" {
UIElement * inline_c_Graphics_Urho3D_UI_Element_155_b5fede0e4d92270f97d61924af968f38fffdbceb(UIElement * ptr_inline_c_0) {
return ( ptr_inline_c_0->GetElementEventSender() );
}

}
