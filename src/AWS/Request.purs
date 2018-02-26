module AWS.Request (RequestError, request) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (kind Effect)

foreign import data RequestError :: Effect

foreign import requestImpl :: forall eff i o. String -> String -> i -> EffFnAff (err :: RequestError | eff) o
request :: forall eff i o. String -> String -> i -> Aff (err :: RequestError | eff) o
request service method = fromEffFnAff <<< requestImpl service method
