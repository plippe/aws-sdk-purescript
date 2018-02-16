module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Foreign.Generic (decodeJSON)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)

import Aws (Metadata)
import Eff (liftExcept)

apisMetadataFilePath = "./aws-sdk-js/apis/metadata.json" :: FilePath

main = launchAff do
  apiMetadataFileContent <- readTextFile UTF8 apisMetadataFilePath
  metadata :: Metadata <- decodeJSON apiMetadataFileContent # liftExcept # liftEff

  liftEff $ log "Hello sailor!"
