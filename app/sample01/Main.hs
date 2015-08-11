{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Text.RawString.QQ
import Control.Lens hiding (Context)
import Foreign

import Graphics.Urho3D
import Sample

joysticPatch :: String 
joysticPatch = [r|
<patch>
    <add sel=\"/element/element[./attribute[@name='Name' and @value='Hat0']]\">
        <attribute name=\"Is Visible\" value=\"false\" />
    </add>
</patch>
|]

main :: IO ()
main = withObject () $ \cntx -> do 
  sample <- newSample cntx "HelloWorld" joysticPatch

  sampleSetup sample 
  sampleStart sample 
  customStart sample
  applicationRun $ sample^.sampleApplication
  sampleStop sample 
  deleteSample sample 

customStart :: Sample -> IO ()
customStart = undefined