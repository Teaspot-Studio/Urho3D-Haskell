{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Urho3D.Template(
    inline
  , (^::)
  , (^=)
  , mkFunc1
  , mkFuncN
  ) where

import Language.Haskell.TH as TH
import Language.C.Inline

inline :: String -> TH.DecsQ
inline s = verbatim s

(^::) :: String -> Q Type -> Q Dec
name ^:: typeQ = sigD (mkName name) typeQ

(^=) :: String -> Q Exp -> Q Dec
name ^= bodyQ = valD (varP (mkName name)) (normalB bodyQ) []

mkFunc1 :: String -> String -> (Name -> Q Exp) -> Q Dec 
mkFunc1 name par bodyQ = do
  let parName = mkName par 
  funD (mkName name) [clause [varP parName] (normalB $ bodyQ parName) []] 

mkFuncN :: String -> [String] -> ([Name] -> ExpQ ) -> DecQ 
mkFuncN name ps bodyQ = do
  let pns = mkName <$> ps
  funD (mkName name) [clause (varP <$> pns) (normalB $ bodyQ pns) []]