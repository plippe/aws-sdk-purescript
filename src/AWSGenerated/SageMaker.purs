

-- | <p> Amazon SageMaker runtime API. </p>
module AWS.SageMaker where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "SageMaker" :: String


-- | <p>After you deploy a model into production using Amazon SageMaker hosting services, your client applications use this API to get inferences from the model hosted at the specified endpoint. </p> <p>For an overview of Amazon SageMaker, see <a href="http://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html">How It Works</a> </p> <p> Amazon SageMaker strips all POST headers except those supported by the API. Amazon SageMaker might add additional headers. You should not rely on the behavior of headers outside those enumerated in the request syntax. </p>
invokeEndpoint :: forall eff. InvokeEndpointInput -> Aff (err :: AWS.RequestError | eff) InvokeEndpointOutput
invokeEndpoint = AWS.request serviceName "InvokeEndpoint" 


newtype BodyBlob = BodyBlob String


newtype EndpointName = EndpointName String


newtype Header = Header String


-- | <p> Internal failure occurred. </p>
newtype InternalFailure = InternalFailure 
  { "Message" :: NullOrUndefined (Message)
  }


newtype InvokeEndpointInput = InvokeEndpointInput 
  { "EndpointName" :: (EndpointName)
  , "Body" :: (BodyBlob)
  , "ContentType" :: NullOrUndefined (Header)
  , "Accept" :: NullOrUndefined (Header)
  }


newtype InvokeEndpointOutput = InvokeEndpointOutput 
  { "Body" :: (BodyBlob)
  , "ContentType" :: NullOrUndefined (Header)
  , "InvokedProductionVariant" :: NullOrUndefined (Header)
  }


newtype LogStreamArn = LogStreamArn String


newtype Message = Message String


-- | <p> Model (owned by the customer in the container) returned an error 500. </p>
newtype ModelError = ModelError 
  { "Message" :: NullOrUndefined (Message)
  , "OriginalStatusCode" :: NullOrUndefined (StatusCode)
  , "OriginalMessage" :: NullOrUndefined (Message)
  , "LogStreamArn" :: NullOrUndefined (LogStreamArn)
  }


-- | <p> Service is unavailable. Try your call again. </p>
newtype ServiceUnavailable = ServiceUnavailable 
  { "Message" :: NullOrUndefined (Message)
  }


newtype StatusCode = StatusCode Int


-- | <p> Inspect your request and try again. </p>
newtype ValidationError = ValidationError 
  { "Message" :: NullOrUndefined (Message)
  }
