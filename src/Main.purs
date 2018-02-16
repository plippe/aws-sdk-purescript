module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)

apisMetadataFilePath = "./aws-sdk-js/apis/metadata.json" :: FilePath

main = launchAff do
  apiMetadataFileContent <- readTextFile UTF8 apisMetadataFilePath
  liftEff $ log "Hello sailor!"
