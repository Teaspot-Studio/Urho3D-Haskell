module Graphics.Urho3D.Interface.AST.Lexer(
    cppSpace
  , cppLexeme
  , cppSymbol
  , cppIdentifier
  ) where

import Control.Monad (void)
import Data.Monoid
import Data.Text (Text, pack)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Defines spaces and comments
cppSpace :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text) => m ()
cppSpace = L.space (void spaceChar)
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- | Lexeme that is followed by spaces
cppLexeme :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text)
  => m a -- ^ lexeme parser
  -> m a
cppLexeme = L.lexeme cppSpace

-- | Fixed symbol
cppSymbol :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text)
  => Text -- ^ symbol
  -> m ()
cppSymbol = void . L.symbol cppSpace

-- | C++ identifier
cppIdentifier :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ Text)
  => m Text
cppIdentifier = cppLexeme $ do
  c <- letterChar
  cs <- many alphaNumChar
  return $ pack $ c : cs
