module Graphics.Urho3D.Interface.AST.Type(
    CppType(..)
  , CppPodType(..)
  , CppInt(..)
  , Signess(..)
  , Longness(..)
  , cppType
  ) where 

import Control.DeepSeq
import Data.Foldable
import Data.Monoid 
import GHC.Generics 
import Graphics.Urho3D.Interface.AST.Lexer
import Text.Megaparsec
import Text.Megaparsec.Prim

-- | C++ type
data CppType = 
    CppPodType CppPodType -- ^ Bultin types
  | CppPointer CppType -- ^ Pointer to a type
  | CppUserType {
      cppTypeName :: String
    }
  deriving (Eq, Show, Generic)

instance NFData CppType

-- | With sign or without sign the type is
data Signess = Signed | Unsigned 
  deriving (Eq, Show, Generic)

instance NFData Signess

-- | Size modifier of integral type
data Longness = Short | Long | LongLong
  deriving (Eq, Show, Generic)

instance NFData Longness

-- | Built in types
data CppPodType = 
    CppBool
  | CppChar (Maybe Signess)
  | CppDouble
  | CppFloat
  | CppIntegral CppInt
  | CppLongDouble
  | CppVoid
  | CppWChar
  deriving (Eq, Show, Generic)

instance NFData CppPodType

-- | Some neat type to track int modifiers
data CppInt = CppInt (Maybe Signess) (Maybe Longness)
  deriving (Eq, Show, Generic)

instance NFData CppInt 

-- | Parse type signess
signess :: MonadParsec s m Char => m Signess
signess = (cppSymbol "signed" >> return Signed) 
  <|> (cppSymbol "unsigned" >> return Unsigned)

-- | Parse int length modifier
longness :: MonadParsec s m Char => m Longness
longness = (cppSymbol "short" >> return Short)
  <|> (cppSymbol "long" >> return Long)

-- | Parsing a C++ type
cppType :: forall s m . MonadParsec s m Char
  => m CppType
cppType = do 
  t <- parseBultin <|> parseUserType
  wrapPointer t 
  where 
  wrapPointer t = do 
    mp <- optional $ cppSymbol "*"
    case mp of 
      Nothing -> return t 
      Just _ -> wrapPointer $ CppPointer t

  parseUserType = CppUserType <$> cppIdentifier
  parseBultin = CppPodType <$> choice [
      cppSymbol "bool" >> return CppBool
    , cppSymbol "void" >> return CppVoid
    , cppSymbol "double" >> return CppDouble
    , cppSymbol "float" >> return CppFloat
    , cppSymbol "wchar_t" >> return CppWChar
    , try parseLongDouble
    , try parseChar
    , try parseInt
    ]

  parseLongDouble = cppSymbol "long" >> cppSymbol "double" >> return CppLongDouble
  parseChar = CppChar <$> optional signess <* cppSymbol "char"
  parseInt = CppIntegral <$> do 
    let modsParsers = 
              (CppInt <$> (fmap Just signess) <*> pure Nothing :: m CppInt)
          <|> (CppInt <$> pure Nothing <*> (fmap Just longness))
    (mods :: [CppInt]) <- many modsParsers
    if null mods then cppSymbol "int" >> return (CppInt Nothing Nothing)
      else do 
        _ <- optional $ cppSymbol "int"
        foldlM combInts (CppInt Nothing Nothing) mods
    where 
      combSigness (Just s1) (Just s2) = fail $ "Duplicated signess modifiers: "
        <> show s1 <> " and " <> show s2 
      combSigness Nothing (Just s) = return $ Just s 
      combSigness (Just s) Nothing = return $ Just s 
      combSigness Nothing Nothing = return Nothing

      combLongness (Just Long) (Just Long) = return $ Just LongLong
      combLongness (Just s1) (Just s2) = fail $ "Duplicated longness modifiers: "
        <> show s1 <> " and " <> show s2
      combLongness Nothing (Just s) = return $ Just s 
      combLongness (Just s) Nothing = return $ Just s 
      combLongness Nothing Nothing = return Nothing

      combInts (CppInt a1 a2) (CppInt b1 b2) = CppInt 
        <$> combSigness a1 b1 
        <*> combLongness a2 b2
