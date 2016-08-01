module Graphics.Urho3D.Interface.AST(
    runCppParser
  , Parser
  , module Reexport
  ) where 

import Graphics.Urho3D.Interface.AST.Type as Reexport

import Text.Megaparsec
import Text.Megaparsec.String

-- | Running parser
runCppParser :: Parser a -- ^ One of the parsers 
  -> String -- ^ Name of file
  -> String -- ^ Input
  -> Either ParseError a 
runCppParser = runParser