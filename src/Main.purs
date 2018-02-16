module Main where

import Prelude
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Parallel (parTraverse)
import Data.Array (find)
import Data.Either (Either)
import Data.Foreign.Generic (decodeJSON)
import Data.Maybe (Maybe)
import Data.String.Regex (Regex, test)
import Data.StrMap (values)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, readdir, readTextFile)
import Node.Path (FilePath, concat)

import Aws (Metadata(..), ServiceMetadata, Service, serviceMetadataFileRegex)
import Eff (liftEither, liftExcept, liftMaybe)

apisMetadataFilePath = "./aws-sdk-js/apis/metadata.json" :: FilePath
apisPath = "./aws-sdk-js/apis/" :: FilePath

serviceMetadataWithFileRegex :: ServiceMetadata -> Either String (Tuple ServiceMetadata Regex)
serviceMetadataWithFileRegex serviceMetadata = serviceMetadataFileRegex serviceMetadata
  # map (\pattern -> Tuple serviceMetadata pattern)

serviceMetadataWithFileName :: Array FilePath -> Tuple ServiceMetadata Regex -> Maybe (Tuple ServiceMetadata FilePath)
serviceMetadataWithFileName fileNames (Tuple serviceMetadata pattern) = find (test pattern) fileNames
  # map (\fileName -> Tuple serviceMetadata fileName)

serviceMetadataWithFilePath :: FilePath -> Tuple ServiceMetadata FilePath -> Tuple ServiceMetadata FilePath
serviceMetadataWithFilePath path (Tuple serviceMetadata fileName) = Tuple serviceMetadata (concat [path, fileName])

serviceMetadataWithService :: forall eff. Tuple ServiceMetadata String -> Aff (exception :: EXCEPTION, fs :: FS | eff) (Tuple ServiceMetadata Service)
serviceMetadataWithService (Tuple serviceMetadata filePath) = do
  jsonString <- readTextFile UTF8 filePath
  service <- decodeJSON jsonString # liftExcept # liftEff
  pure $ Tuple serviceMetadata service

main = launchAff do
  apiMetadataFileContent <- readTextFile UTF8 apisMetadataFilePath
  Metadata metadata <- decodeJSON apiMetadataFileContent # liftExcept # liftEff
  let servicesMetadata = values metadata

  servicesMetadataWithFileRegex <- map serviceMetadataWithFileRegex servicesMetadata
    # parTraverse (liftEither >>> liftEff)

  apiFileNames <- readdir apisPath
  servicesMetadataWithFileName <- map (serviceMetadataWithFileName apiFileNames) servicesMetadataWithFileRegex
    # parTraverse (liftMaybe >>> liftEff)

  let servicesMetadataWithFilePaths = map (serviceMetadataWithFilePath apisPath) servicesMetadataWithFileName
  servicesMetadataWithServices <- parTraverse serviceMetadataWithService servicesMetadataWithFilePaths

  liftEff $ log "Hello sailor!"
