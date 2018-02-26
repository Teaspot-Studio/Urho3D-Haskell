module Graphics.Urho3D.Interface.AST(
    runCppParser
  , fullParser
  , Parser
  , module Reexport
  ) where

import Graphics.Urho3D.Interface.AST.Type as Reexport
import Graphics.Urho3D.Interface.AST.Lexer

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Our wrapper around generic parser
type Parser a = Parsec () Text a

-- | Running parser
runCppParser :: Parser a -- ^ One of the parsers
  -> String -- ^ Name of file
  -> Text -- ^ Input
  -> Either (ParseError Char ()) a
runCppParser = runParser

-- | Helper to parse all input
fullParser :: Parser a -> Parser a
fullParser p = do
  _ <- cppSpace
  a <- p
  eof
  return a
