{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Urho3D.Math.Rect(
    Rect(..)
  , IntRect(..)
  , HasMinPoint(..)
  , HasMaxPoint(..)
  , rectContext
  ) where

import qualified Language.C.Inline as C 
import qualified Language.C.Inline.Cpp as C

import Graphics.Urho3D.Math.Internal.Rect
import Graphics.Urho3D.Math.Vector2
import Data.Monoid
import Foreign 
import Text.RawString.QQ

C.context (C.cppCtx <> rectCntx <> vector2Context)
C.include "<Urho3D/Math/Rect.h>"
C.using "namespace Urho3D"

rectContext :: C.Context 
rectContext = rectCntx

C.verbatim [r|
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
|]

instance Storable Rect where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(Rect) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<Rect>::AlignmentOf } |]
  peek ptr = do 
    vmin <- peek =<< [C.exp| Vector2* { &$(Rect* ptr)->min_ } |]
    vmax <- peek =<< [C.exp| Vector2* { &$(Rect* ptr)->max_ } |]
    return $ Rect vmin vmax
  poke ptr (Rect vmin vmax) = with vmin $ \vmin' -> with vmax $ \vmax' -> [C.block| void { 
    $(Rect* ptr)->min_ = *$(Vector2* vmin');
    $(Rect* ptr)->max_ = *$(Vector2* vmax');
    } |]

instance Storable IntRect where 
  sizeOf _ = fromIntegral $ [C.pure| int { (int)sizeof(IntRect) } |]
  alignment _ = fromIntegral $ [C.pure| int { (int)Traits<IntRect>::AlignmentOf } |]
  peek ptr = do 
    vleft <- fromIntegral <$> [C.exp| int { $(IntRect* ptr)->left_ } |]
    vtop <- fromIntegral <$> [C.exp| int { $(IntRect* ptr)->top_ } |]
    vright <- fromIntegral <$> [C.exp| int { $(IntRect* ptr)->right_ } |]
    vbottom <- fromIntegral <$> [C.exp| int { $(IntRect* ptr)->bottom_ } |]
    return $ IntRect vleft vtop vright vbottom
  poke ptr (IntRect vleft vtop vright vbottom) = [C.block| void { 
    $(IntRect* ptr)->left_ = $(int vleft');
    $(IntRect* ptr)->top_ = $(int vtop');
    $(IntRect* ptr)->right_ = $(int vright');
    $(IntRect* ptr)->bottom_ = $(int vbottom');
    } |]
    where 
      vleft' = fromIntegral vleft 
      vtop' = fromIntegral vtop 
      vright' = fromIntegral vright 
      vbottom' = fromIntegral vbottom