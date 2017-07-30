#pragma once

namespace Urho3D
{

extern "C" typedef void (*OnHoverCallback)(const IntVector2* position, const IntVector2* screenPosition, int buttons, int qualifiers, Cursor* cursor);
extern "C" typedef void (*OnClickBeginCallback)(const IntVector2* position, const IntVector2* screenPosition, int button, int buttons, int qualifiers, Cursor* cursor);
extern "C" typedef void (*OnClickEndCallback)(const IntVector2* position, const IntVector2* screenPosition, int button, int buttons, int qualifiers, Cursor* cursor, UIElement* beginElement);
extern "C" typedef void (*OnDoubleClickCallback)(const IntVector2* position, const IntVector2* screenPosition, int button, int buttons, int qualifiers, Cursor* cursor);
extern "C" typedef void (*OnDragBeginCallback)(const IntVector2* position, const IntVector2* screenPosition, int buttons, int qualifiers, Cursor* cursor);
extern "C" typedef void (*OnDragMoveCallback)(const IntVector2* position, const IntVector2* screenPosition, const IntVector2* deltaPos, int buttons, int qualifiers, Cursor* cursor);
extern "C" typedef void (*OnDragEndCallback)(const IntVector2* position, const IntVector2* screenPosition, int dragButtons, int releaseButton, Cursor* cursor);
extern "C" typedef void (*OnDragCancelCallback)(const IntVector2* position, const IntVector2* screenPosition, int dragButtons, int cancelButton, Cursor* cursor);
extern "C" typedef int (*OnDragDropTestCallback)(UIElement* source);
extern "C" typedef int (*OnDragDropFinishCallback)(UIElement* source);
extern "C" typedef void (*OnWheelCallback)(int delta, int buttons, int qualifiers);
extern "C" typedef void (*OnKeyCallback)(int key, int buttons, int qualifiers);
extern "C" typedef void (*OnTextInputCallback)(const String* text);
extern "C" typedef void (*OnResizeCallback)(const IntVector2* newSize, const IntVector2* delta);
extern "C" typedef void (*OnPositionSetCallback)(const IntVector2* newPosition);
extern "C" typedef void (*OnSetEditableCallback)();
extern "C" typedef void (*OnIndentSetCallback)();

/// %UIElement extention that allows setting callbacks for events
class URHO3D_API HUIElement : public UIElement
{
  URHO3D_OBJECT(HUIElement, UIElement);

public:
  /// Construct
  HUIElement(Context* context) : UIElement(context) {}
  /// Destruct
  virtual ~HUIElement() {}

  /// Setup callback for OnHover event
  void SetOnHoverCallback(OnHoverCallback callback) {
    onHoverCallback = callback;
  }
  /// React to mouse hover.
  virtual void OnHover(const IntVector2& position, const IntVector2& screenPosition, int buttons, int qualifiers, Cursor* cursor)
  {
    if (onHoverCallback) onHoverCallback(&position, &screenPosition, buttons, qualifiers, cursor);
  }

  /// Setup callback for OnClickBegin event
  void SetOnClickBeginCallback(OnClickBeginCallback callback) {
    onClickBeginCallback = callback;
  }
  /// React to mouse click begin.
  virtual void OnClickBegin(const IntVector2& position, const IntVector2& screenPosition, int button, int buttons, int qualifiers, Cursor* cursor)
  {
    if (onClickBeginCallback) onClickBeginCallback(&position, &screenPosition, button, buttons, qualifiers, cursor);
  }

  /// Setup callback for OnClickEnd event
  void SetOnClickEndCallback(OnClickEndCallback callback) {
    onClickEndCallback = callback;
  }
  /// React to mouse click end.
  virtual void OnClickEnd(const IntVector2& position, const IntVector2& screenPosition, int button, int buttons, int qualifiers, Cursor* cursor,
      UIElement* beginElement)
  {
    if (onClickEndCallback) onClickEndCallback(&position, &screenPosition, button, buttons, qualifiers, cursor, beginElement);
  }

  /// Setup callback for OnDoubleClick event
  void SetOnDoubleClickCallback(OnDoubleClickCallback callback) {
    onDoubleClickCallback = callback;
  }
  /// React to mouse double click.
  virtual void OnDoubleClick(const IntVector2& position, const IntVector2& screenPosition, int button, int buttons, int qualifiers, Cursor* cursor)
  {
    if (onDoubleClickCallback) onDoubleClickCallback(&position, &screenPosition, button, buttons, qualifiers, cursor);
  }

  /// Setup callback for OnDragBegin event
  void SetOnDragBeginCallback(OnDragBeginCallback callback) {
    onDragBeginCallback = callback;
  }
  /// React to mouse drag begin.
  virtual void OnDragBegin(const IntVector2& position, const IntVector2& screenPosition, int buttons, int qualifiers, Cursor* cursor)
  {
    if (onDragBeginCallback) onDragBeginCallback(&position, &screenPosition, buttons, qualifiers, cursor);
  }

  /// Setup callback for OnDragMove event
  void SetOnDragMoveCallback(OnDragMoveCallback callback) {
    onDragMoveCallback = callback;
  }
  /// React to mouse drag motion.
  virtual void OnDragMove(const IntVector2& position, const IntVector2& screenPosition, const IntVector2& deltaPos, int buttons, int qualifiers,
      Cursor* cursor)
  {
    if (onDragMoveCallback) onDragMoveCallback(&position, &screenPosition, &deltaPos, buttons, qualifiers, cursor);
  }

  /// Setup callback for OnDragEnd event
  void SetOnDragEndCallback(OnDragEndCallback callback) {
    onDragEndCallback = callback;
  }
  /// React to mouse drag end.
  virtual void OnDragEnd(const IntVector2& position, const IntVector2& screenPosition, int dragButtons, int releaseButton, Cursor* cursor)
  {
    if (onDragEndCallback) onDragEndCallback(&position, &screenPosition, dragButtons, releaseButton, cursor);
  }

  /// Setup callback for OnDragCancel event
  void SetOnDragCancelCallback(OnDragCancelCallback callback) {
    onDragCancelCallback = callback;
  }
  /// React to a mouse drag cancel event (ie, when an extra button is pressed)
  virtual void OnDragCancel(const IntVector2& position, const IntVector2& screenPosition, int dragButtons, int cancelButton, Cursor* cursor)
  {
    if (onDragCancelCallback) onDragCancelCallback(&position, &screenPosition, dragButtons, cancelButton, cursor);
  }

  /// Setup callback for OnDragDropTest event
  void SetOnDragDropTestCallback(OnDragDropTestCallback callback) {
    onDragDropTestCallback = callback;
  }
  /// React to drag and drop test. Return true to signal that the drop is acceptable.
  virtual bool OnDragDropTest(UIElement* source)
  {
    if (onDragDropTestCallback) return onDragDropTestCallback(source) != 0;
      else return false;
  }

  /// Setup callback for OnDragDropFinish event
  void SetOnDragDropFinishCallback(OnDragDropFinishCallback callback) {
    onDragDropFinishCallback = callback;
  }
  /// React to drag and drop finish. Return true to signal that the drop was accepted.
  virtual bool OnDragDropFinish(UIElement* source)
  {
    if (onDragDropFinishCallback) return onDragDropFinishCallback(source) != 0;
      else return false;
  }

  /// Setup callback for OnWheel event
  void SetOnWheelCallback(OnWheelCallback callback) {
    onWheelCallback = callback;
  }
  /// React to mouse wheel.
  virtual void OnWheel(int delta, int buttons, int qualifiers)
  {
    if (onWheelCallback) onWheelCallback(delta, buttons, qualifiers);
  }

  /// Setup callback for OnKey event
  void SetOnKeyCallback(OnKeyCallback callback) {
    onKeyCallback = callback;
  }
  /// React to a key press.
  virtual void OnKey(int key, int buttons, int qualifiers)
  {
    if (onKeyCallback) onKeyCallback(key, buttons, qualifiers);
  }

  /// Setup callback for OnTextInput event
  void SetOnTextInputCallback(OnTextInputCallback callback) {
    onTextInputCallback = callback;
  }
  /// React to text input event.
  virtual void OnTextInput(const String& text)
  {
    if (onTextInputCallback) onTextInputCallback(&text);
  }

  /// Setup callback for OnResize event
  void SetOnResizeCallback(OnResizeCallback callback) {
    onResizeCallback = callback;
  }
  /// React to resize.
  virtual void OnResize(const IntVector2& newSize, const IntVector2& delta)
  {
    if (onResizeCallback) onResizeCallback(&newSize, &delta);
  }

  /// Setup callback for OnPositionSet event
  void SetOnPositionSetCallback(OnPositionSetCallback callback) {
    onPositionSetCallback = callback;
  }
  /// React to position change.
  virtual void OnPositionSet(const IntVector2& newPosition)
  {
    if (onPositionSetCallback) onPositionSetCallback(&newPosition);
  }

  /// Setup callback for OnSetEditable event
  void SetOnSetEditableCallback(OnSetEditableCallback callback) {
    onSetEditableCallback = callback;
  }
  /// React to editable status change.
  virtual void OnSetEditable()
  {
    if (onSetEditableCallback) onSetEditableCallback();
  }

  /// Setup callback for OnIndentSet event
  void SetOnIndentSetCallback(OnIndentSetCallback callback) {
    onIndentSetCallback = callback;
  }
  /// React to indent change.
  virtual void OnIndentSet()
  {
    if (onIndentSetCallback) onIndentSetCallback();
  }
private:

  OnHoverCallback onHoverCallback = NULL;
  OnClickBeginCallback onClickBeginCallback = NULL;
  OnClickEndCallback onClickEndCallback = NULL;
  OnDoubleClickCallback onDoubleClickCallback = NULL;
  OnDragBeginCallback onDragBeginCallback = NULL;
  OnDragMoveCallback onDragMoveCallback = NULL;
  OnDragEndCallback onDragEndCallback = NULL;
  OnDragCancelCallback onDragCancelCallback = NULL;
  OnDragDropTestCallback onDragDropTestCallback = NULL;
  OnDragDropFinishCallback onDragDropFinishCallback = NULL;
  OnWheelCallback onWheelCallback = NULL;
  OnKeyCallback onKeyCallback = NULL;
  OnTextInputCallback onTextInputCallback = NULL;
  OnResizeCallback onResizeCallback = NULL;
  OnPositionSetCallback onPositionSetCallback = NULL;
  OnSetEditableCallback onSetEditableCallback = NULL;
  OnIndentSetCallback onIndentSetCallback = NULL;
};

}
