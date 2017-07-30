#pragma once

namespace Urho3D
{

extern "C" typedef void (*OnHoverCallback)(const IntVector2* position, const IntVector2* screenPosition, int buttons, int qualifiers, Cursor* cursor);

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
  /// Get current callback for OnHover event
  OnHoverCallback GetOnHoverCallback() {
    return onHoverCallback;
  }
  /// React to mouse hover.
  virtual void OnHover(const IntVector2& position, const IntVector2& screenPosition, int buttons, int qualifiers, Cursor* cursor)
  {
    if (onHoverCallback) onHoverCallback(&position, &screenPosition, buttons, qualifiers, cursor);
  }

private:

  OnHoverCallback onHoverCallback = NULL;
};

}
