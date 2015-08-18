{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Urho3D.Template(
    inline
  , (^::)
  , (^=)
  , lam
  , mkFunc1
  , mkFunc1Con
  , mkFuncN
  , mkNewType
  ) where

import Language.Haskell.TH as TH
import Language.C.Inline

inline :: String -> TH.DecsQ
inline s = verbatim s

(^::) :: String -> Q Type -> Q Dec
name ^:: typeQ = sigD (mkName name) typeQ

(^=) :: String -> Q Exp -> Q Dec
name ^= bodyQ = valD (varP (mkName name)) (normalB bodyQ) []

-- | Makes lamda expression with give args
lam :: [String] -> Q Exp -> Q Exp 
lam args = lamE (varP . mkName <$> args)

mkFunc1 :: String -> String -> (Name -> Q Exp) -> Q Dec 
mkFunc1 name par bodyQ = do
  let parName = mkName par 
  funD (mkName name) [clause [varP parName] (normalB $ bodyQ parName) []] 

mkFuncN :: String -> [String] -> ([Name] -> ExpQ ) -> DecQ 
mkFuncN name ps bodyQ = do
  let pns = mkName <$> ps
  funD (mkName name) [clause (varP <$> pns) (normalB $ bodyQ pns) []]

mkNewType :: String -> TypeQ -> DecQ 
mkNewType tname t = 
  newtypeD (return []) (mkName tname) [] (recC (mkName tname) [varStrictType (mkName $ "un"++tname) $ strictType notStrict t]) []

mkFunc1Con :: String -> String -> String -> (Name -> Q Exp) -> Q Dec 
mkFunc1Con name con par bodyQ = do 
  let parName = mkName par 
      conName = mkName con
  funD (mkName name) [clause [conP conName [varP parName]] (normalB $ bodyQ parName) []] 