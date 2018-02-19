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
import Node.FS.Aff (FS, readdir, readTextFile, writeTextFile)
import Node.Path (FilePath, concat)

import Aws (Metadata(Metadata), Service, ServiceMetadata(ServiceMetadata), serviceMetadataFileRegex)
import Eff (liftEither, liftExcept, liftMaybe)
import Printer.PureScript (client, clientFilePath)

apisMetadataFilePath = "./aws-sdk-js/apis/metadata.json" :: FilePath
apisPath = "./aws-sdk-js/apis/" :: FilePath

clientsPath = "./lib/AwsGenerated" :: FilePath

serviceMetadataWithApiFileRegex :: ServiceMetadata -> Either String (Tuple ServiceMetadata Regex)
serviceMetadataWithApiFileRegex serviceMetadata = serviceMetadataFileRegex serviceMetadata
  # map (\pattern -> Tuple serviceMetadata pattern)

serviceMetadataWithApiFileName :: Array FilePath -> Tuple ServiceMetadata Regex -> Maybe (Tuple ServiceMetadata FilePath)
serviceMetadataWithApiFileName fileNames (Tuple serviceMetadata pattern) = find (test pattern) fileNames
  # map (\fileName -> Tuple serviceMetadata fileName)

serviceMetadataWithApiFilePath :: FilePath -> Tuple ServiceMetadata FilePath -> Tuple ServiceMetadata FilePath
serviceMetadataWithApiFilePath path (Tuple serviceMetadata fileName) = Tuple serviceMetadata (concat [path, fileName])

serviceMetadataWithService :: forall eff. Tuple ServiceMetadata String -> Aff (exception :: EXCEPTION, fs :: FS | eff) (Tuple ServiceMetadata Service)
serviceMetadataWithService (Tuple serviceMetadata filePath) = do
  jsonString <- readTextFile UTF8 filePath
  service <- decodeJSON jsonString # liftExcept # liftEff
  pure $ Tuple serviceMetadata service

serviceMetadataWithClientFile :: forall eff. FilePath -> Tuple ServiceMetadata Service -> Aff (fs :: FS | eff) (Tuple ServiceMetadata FilePath)
serviceMetadataWithClientFile path (Tuple serviceMetadata@(ServiceMetadata { name }) service) = do
  let filePath = clientFilePath path serviceMetadata service
  let file = client serviceMetadata service

  _ <- writeTextFile UTF8 filePath file
  pure $ Tuple serviceMetadata filePath


main = launchAff do
  apiMetadataFileContent <- readTextFile UTF8 apisMetadataFilePath
  Metadata metadata <- decodeJSON apiMetadataFileContent # liftExcept # liftEff
  let servicesMetadata = values metadata

  servicesMetadataWithApiFileRegex <- map serviceMetadataWithApiFileRegex servicesMetadata
    # parTraverse (liftEither >>> liftEff)

  apiFileNames <- readdir apisPath
  servicesMetadataWithApiFileName <- map (serviceMetadataWithApiFileName apiFileNames) servicesMetadataWithApiFileRegex
    # parTraverse (liftMaybe >>> liftEff)

  let servicesMetadataWithApiFilePaths = map (serviceMetadataWithApiFilePath apisPath) servicesMetadataWithApiFileName
  servicesMetadataWithServices <- parTraverse serviceMetadataWithService servicesMetadataWithApiFilePaths
  servicesMetadataWithClientFiles <- parTraverse (serviceMetadataWithClientFile clientsPath) servicesMetadataWithServices

  liftEff $ log "Hello sailor!"
