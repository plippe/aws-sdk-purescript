

-- | <p> Amazon SageMaker runtime API. </p>
module AWS.SageMaker where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "SageMaker" :: String


-- | <p>After you deploy a model into production using Amazon SageMaker hosting services, your client applications use this API to get inferences from the model hosted at the specified endpoint. </p> <p>For an overview of Amazon SageMaker, see <a href="http://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html">How It Works</a> </p> <p> Amazon SageMaker strips all POST headers except those supported by the API. Amazon SageMaker might add additional headers. You should not rely on the behavior of headers outside those enumerated in the request syntax. </p>
invokeEndpoint :: forall eff. InvokeEndpointInput -> Aff (err :: AWS.RequestError | eff) InvokeEndpointOutput
invokeEndpoint = AWS.request serviceName "invokeEndpoint" 


newtype BodyBlob = BodyBlob String
derive instance newtypeBodyBlob :: Newtype BodyBlob _


newtype EndpointName = EndpointName String
derive instance newtypeEndpointName :: Newtype EndpointName _


newtype Header = Header String
derive instance newtypeHeader :: Newtype Header _


-- | <p> Internal failure occurred. </p>
newtype InternalFailure = InternalFailure 
  { "Message" :: NullOrUndefined (Message)
  }
derive instance newtypeInternalFailure :: Newtype InternalFailure _


newtype InvokeEndpointInput = InvokeEndpointInput 
  { "EndpointName" :: (EndpointName)
  , "Body" :: (BodyBlob)
  , "ContentType" :: NullOrUndefined (Header)
  , "Accept" :: NullOrUndefined (Header)
  }
derive instance newtypeInvokeEndpointInput :: Newtype InvokeEndpointInput _


newtype InvokeEndpointOutput = InvokeEndpointOutput 
  { "Body" :: (BodyBlob)
  , "ContentType" :: NullOrUndefined (Header)
  , "InvokedProductionVariant" :: NullOrUndefined (Header)
  }
derive instance newtypeInvokeEndpointOutput :: Newtype InvokeEndpointOutput _


newtype LogStreamArn = LogStreamArn String
derive instance newtypeLogStreamArn :: Newtype LogStreamArn _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


-- | <p> Model (owned by the customer in the container) returned an error 500. </p>
newtype ModelError = ModelError 
  { "Message" :: NullOrUndefined (Message)
  , "OriginalStatusCode" :: NullOrUndefined (StatusCode)
  , "OriginalMessage" :: NullOrUndefined (Message)
  , "LogStreamArn" :: NullOrUndefined (LogStreamArn)
  }
derive instance newtypeModelError :: Newtype ModelError _


-- | <p> Service is unavailable. Try your call again. </p>
newtype ServiceUnavailable = ServiceUnavailable 
  { "Message" :: NullOrUndefined (Message)
  }
derive instance newtypeServiceUnavailable :: Newtype ServiceUnavailable _


newtype StatusCode = StatusCode Int
derive instance newtypeStatusCode :: Newtype StatusCode _


-- | <p> Inspect your request and try again. </p>
newtype ValidationError = ValidationError 
  { "Message" :: NullOrUndefined (Message)
  }
derive instance newtypeValidationError :: Newtype ValidationError _
