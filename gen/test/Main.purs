module Test.Main where

import Prelude
import Control.Monad.Aff (Aff, attempt, launchAff, throwError)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (error, message)
import Data.Either (Either(..))

import Aws.Service (AwsError, request)

main = launchAff do
    _ <- testRequestUnknownService
    _ <- testRequestUnknownMethod
    _ <- testRequestMissingParameters
    liftEff $ log "You should add some tests."

testRequestUnknownService :: forall eff. Aff (err :: AwsError | eff) Unit
testRequestUnknownService = do
    errOrSuccess <- attempt $ request "unknown" "" ""
    case errOrSuccess of
        Right succ -> throwError $ error "AWS service unknown shouldn't exist"
        Left err -> if (message err) == "awsSdk[serviceName] is not a constructor"
            then pure unit
            else throwError err

testRequestUnknownMethod :: forall eff. Aff (err :: AwsError | eff) Unit
testRequestUnknownMethod = do
    errOrSuccess <- attempt $ request "S3" "unknown" ""
    case errOrSuccess of
        Right succ -> throwError $ error "AWS S3 method unknown shouldn't exist"
        Left err -> if (message err) == "awsService[methodName] is not a function"
            then pure unit
            else throwError err

testRequestMissingParameters :: forall eff. Aff (err :: AwsError | eff) Unit
testRequestMissingParameters = do
    errOrSuccess <- attempt $ request "S3" "getBucketVersioning" ""
    case errOrSuccess of
        Right succ -> throwError $ error "AWS S3 getBucketVersioning should require parameters"
        Left err -> if (message err) == "Missing required key 'Bucket' in params"
            then pure unit
            else throwError err
