module Test.Main where

import Prelude (Unit, bind, pure, unit, ($), (==))
import Control.Monad.Aff (Aff, Fiber, attempt, launchAff, throwError)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error, message)
import Data.Either (Either(..))

import AWS.Request (RequestError, request)

main :: forall eff. Eff (err :: RequestError, console :: CONSOLE | eff) (Fiber (err :: RequestError, console :: CONSOLE | eff) Unit)
main = launchAff do
    _ <- testRequestUnknownService
    _ <- liftEff $ log "OK. testRequestUnknownService"
    _ <- testRequestUnknownMethod
    _ <- liftEff $ log "OK. testRequestUnknownMethod"
    _ <- testRequestMissingParameters
    _ <- liftEff $ log "OK. testRequestMissingParameters"

    pure unit

testRequestUnknownService :: forall eff. Aff (err :: RequestError | eff) Unit
testRequestUnknownService = do
    errOrSuccess <- attempt $ request "unknown" "" ""
    case errOrSuccess of
        Right succ -> throwError $ error "AWS service unknown shouldn't exist"
        Left err -> if (message err) == "awsSdk[serviceName] is not a constructor"
            then pure unit
            else throwError err

testRequestUnknownMethod :: forall eff. Aff (err :: RequestError | eff) Unit
testRequestUnknownMethod = do
    errOrSuccess <- attempt $ request "S3" "unknown" ""
    case errOrSuccess of
        Right succ -> throwError $ error "AWS S3 method unknown shouldn't exist"
        Left err -> if (message err) == "awsService[methodName] is not a function"
            then pure unit
            else throwError err

testRequestMissingParameters :: forall eff. Aff (err :: RequestError | eff) Unit
testRequestMissingParameters = do
    errOrSuccess <- attempt $ request "S3" "getBucketVersioning" ""
    case errOrSuccess of
        Right succ -> throwError $ error "AWS S3 getBucketVersioning should require parameters"
        Left err -> if (message err) == "Missing required key 'Bucket' in params"
            then pure unit
            else throwError err
