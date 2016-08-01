module Graphics.Urho3D.Interface.AST.Lexer(
    cppSpace
  , cppLexeme
  , cppSymbol
  , cppIdentifier
  ) where 

import Control.Monad (void)
import Data.Monoid 
import qualified Text.Megaparsec.Lexer as L 
import Text.Megaparsec
import Text.Megaparsec.Prim

-- | Defines spaces and comments
cppSpace :: MonadParsec s m Char => m ()
cppSpace = L.space (void spaceChar)
  (L.skipLineComment "//") 
  (L.skipBlockComment "/*" "*/")

-- | Lexeme that is followed by spaces
cppLexeme :: MonadParsec s m Char 
  => m a -- ^ lexeme parser
  -> m a 
cppLexeme = L.lexeme cppSpace 

-- | Fixed symbol
cppSymbol :: MonadParsec s m Char
  => String -- ^ symbol
  -> m ()
cppSymbol = void . L.symbol cppSpace

-- | C++ identifier
cppIdentifier :: MonadParsec s m Char 
  => m String 
cppIdentifier = cppLexeme $ do
  c <- letterChar
  cs <- many alphaNumChar
  return $ c : cs