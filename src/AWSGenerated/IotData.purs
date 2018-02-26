

-- | <fullname>AWS IoT</fullname> <p>AWS IoT-Data enables secure, bi-directional communication between Internet-connected things (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. It implements a broker for applications and things to publish messages over HTTP (Publish) and retrieve, update, and delete thing shadows. A thing shadow is a persistent representation of your things and their state in the AWS cloud.</p>
module AWS.IotData where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "IotData" :: String


-- | <p>Deletes the thing shadow for the specified thing.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/API_DeleteThingShadow.html">DeleteThingShadow</a> in the <i>AWS IoT Developer Guide</i>.</p>
deleteThingShadow :: forall eff. DeleteThingShadowRequest -> Aff (err :: AWS.RequestError | eff) DeleteThingShadowResponse
deleteThingShadow = AWS.request serviceName "DeleteThingShadow" 


-- | <p>Gets the thing shadow for the specified thing.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/API_GetThingShadow.html">GetThingShadow</a> in the <i>AWS IoT Developer Guide</i>.</p>
getThingShadow :: forall eff. GetThingShadowRequest -> Aff (err :: AWS.RequestError | eff) GetThingShadowResponse
getThingShadow = AWS.request serviceName "GetThingShadow" 


-- | <p>Publishes state information.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/protocols.html#http">HTTP Protocol</a> in the <i>AWS IoT Developer Guide</i>.</p>
publish :: forall eff. PublishRequest -> Aff (err :: AWS.RequestError | eff) Unit
publish = AWS.request serviceName "Publish" 


-- | <p>Updates the thing shadow for the specified thing.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/API_UpdateThingShadow.html">UpdateThingShadow</a> in the <i>AWS IoT Developer Guide</i>.</p>
updateThingShadow :: forall eff. UpdateThingShadowRequest -> Aff (err :: AWS.RequestError | eff) UpdateThingShadowResponse
updateThingShadow = AWS.request serviceName "UpdateThingShadow" 


-- | <p>The specified version does not match the version of the document.</p>
newtype ConflictException = ConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The input for the DeleteThingShadow operation.</p>
newtype DeleteThingShadowRequest = DeleteThingShadowRequest 
  { "ThingName'" :: (ThingName)
  }


-- | <p>The output from the DeleteThingShadow operation.</p>
newtype DeleteThingShadowResponse = DeleteThingShadowResponse 
  { "Payload'" :: (JsonDocument)
  }


newtype ErrorMessage = ErrorMessage String


-- | <p>The input for the GetThingShadow operation.</p>
newtype GetThingShadowRequest = GetThingShadowRequest 
  { "ThingName'" :: (ThingName)
  }


-- | <p>The output from the GetThingShadow operation.</p>
newtype GetThingShadowResponse = GetThingShadowResponse 
  { "Payload'" :: NullOrUndefined (JsonDocument)
  }


-- | <p>An unexpected error has occurred.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The request is not valid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype JsonDocument = JsonDocument String


-- | <p>The specified combination of HTTP verb and URI is not supported.</p>
newtype MethodNotAllowedException = MethodNotAllowedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype Payload = Payload String


-- | <p>The input for the Publish operation.</p>
newtype PublishRequest = PublishRequest 
  { "Topic'" :: (Topic)
  , "Qos'" :: NullOrUndefined (Qos)
  , "Payload'" :: NullOrUndefined (Payload)
  }


newtype Qos = Qos Int


-- | <p>The payload exceeds the maximum size allowed.</p>
newtype RequestEntityTooLargeException = RequestEntityTooLargeException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The service is temporarily unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype ThingName = ThingName String


-- | <p>The rate exceeds the limit.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype Topic = Topic String


-- | <p>You are not authorized to perform this operation.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The document encoding is not supported.</p>
newtype UnsupportedDocumentEncodingException = UnsupportedDocumentEncodingException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The input for the UpdateThingShadow operation.</p>
newtype UpdateThingShadowRequest = UpdateThingShadowRequest 
  { "ThingName'" :: (ThingName)
  , "Payload'" :: (JsonDocument)
  }


-- | <p>The output from the UpdateThingShadow operation.</p>
newtype UpdateThingShadowResponse = UpdateThingShadowResponse 
  { "Payload'" :: NullOrUndefined (JsonDocument)
  }


newtype ErrorMessage' = ErrorMessage' String
