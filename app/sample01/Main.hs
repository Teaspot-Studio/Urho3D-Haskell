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
  bracket (newSample cntx "HelloWorld" joysticPatch customStart) 
    deleteSample runSample

customStart :: SampleRef -> IO ()
customStart sr = return ()