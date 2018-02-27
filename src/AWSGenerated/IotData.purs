

-- | <fullname>AWS IoT</fullname> <p>AWS IoT-Data enables secure, bi-directional communication between Internet-connected things (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. It implements a broker for applications and things to publish messages over HTTP (Publish) and retrieve, update, and delete thing shadows. A thing shadow is a persistent representation of your things and their state in the AWS cloud.</p>
module AWS.IotData where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "IotData" :: String


-- | <p>Deletes the thing shadow for the specified thing.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/API_DeleteThingShadow.html">DeleteThingShadow</a> in the <i>AWS IoT Developer Guide</i>.</p>
deleteThingShadow :: forall eff. DeleteThingShadowRequest -> Aff (err :: AWS.RequestError | eff) DeleteThingShadowResponse
deleteThingShadow = AWS.request serviceName "deleteThingShadow" 


-- | <p>Gets the thing shadow for the specified thing.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/API_GetThingShadow.html">GetThingShadow</a> in the <i>AWS IoT Developer Guide</i>.</p>
getThingShadow :: forall eff. GetThingShadowRequest -> Aff (err :: AWS.RequestError | eff) GetThingShadowResponse
getThingShadow = AWS.request serviceName "getThingShadow" 


-- | <p>Publishes state information.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/protocols.html#http">HTTP Protocol</a> in the <i>AWS IoT Developer Guide</i>.</p>
publish :: forall eff. PublishRequest -> Aff (err :: AWS.RequestError | eff) Unit
publish = AWS.request serviceName "publish" 


-- | <p>Updates the thing shadow for the specified thing.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/API_UpdateThingShadow.html">UpdateThingShadow</a> in the <i>AWS IoT Developer Guide</i>.</p>
updateThingShadow :: forall eff. UpdateThingShadowRequest -> Aff (err :: AWS.RequestError | eff) UpdateThingShadowResponse
updateThingShadow = AWS.request serviceName "updateThingShadow" 


-- | <p>The specified version does not match the version of the document.</p>
newtype ConflictException = ConflictException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConflictException :: Newtype ConflictException _


-- | <p>The input for the DeleteThingShadow operation.</p>
newtype DeleteThingShadowRequest = DeleteThingShadowRequest 
  { "ThingName'" :: (ThingName)
  }
derive instance newtypeDeleteThingShadowRequest :: Newtype DeleteThingShadowRequest _


-- | <p>The output from the DeleteThingShadow operation.</p>
newtype DeleteThingShadowResponse = DeleteThingShadowResponse 
  { "Payload'" :: (JsonDocument)
  }
derive instance newtypeDeleteThingShadowResponse :: Newtype DeleteThingShadowResponse _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>The input for the GetThingShadow operation.</p>
newtype GetThingShadowRequest = GetThingShadowRequest 
  { "ThingName'" :: (ThingName)
  }
derive instance newtypeGetThingShadowRequest :: Newtype GetThingShadowRequest _


-- | <p>The output from the GetThingShadow operation.</p>
newtype GetThingShadowResponse = GetThingShadowResponse 
  { "Payload'" :: NullOrUndefined (JsonDocument)
  }
derive instance newtypeGetThingShadowResponse :: Newtype GetThingShadowResponse _


-- | <p>An unexpected error has occurred.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _


-- | <p>The request is not valid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _


newtype JsonDocument = JsonDocument String
derive instance newtypeJsonDocument :: Newtype JsonDocument _


-- | <p>The specified combination of HTTP verb and URI is not supported.</p>
newtype MethodNotAllowedException = MethodNotAllowedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMethodNotAllowedException :: Newtype MethodNotAllowedException _


newtype Payload = Payload String
derive instance newtypePayload :: Newtype Payload _


-- | <p>The input for the Publish operation.</p>
newtype PublishRequest = PublishRequest 
  { "Topic'" :: (Topic)
  , "Qos'" :: NullOrUndefined (Qos)
  , "Payload'" :: NullOrUndefined (Payload)
  }
derive instance newtypePublishRequest :: Newtype PublishRequest _


newtype Qos = Qos Int
derive instance newtypeQos :: Newtype Qos _


-- | <p>The payload exceeds the maximum size allowed.</p>
newtype RequestEntityTooLargeException = RequestEntityTooLargeException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeRequestEntityTooLargeException :: Newtype RequestEntityTooLargeException _


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>The service is temporarily unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _


newtype ThingName = ThingName String
derive instance newtypeThingName :: Newtype ThingName _


-- | <p>The rate exceeds the limit.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _


newtype Topic = Topic String
derive instance newtypeTopic :: Newtype Topic _


-- | <p>You are not authorized to perform this operation.</p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _


-- | <p>The document encoding is not supported.</p>
newtype UnsupportedDocumentEncodingException = UnsupportedDocumentEncodingException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeUnsupportedDocumentEncodingException :: Newtype UnsupportedDocumentEncodingException _


-- | <p>The input for the UpdateThingShadow operation.</p>
newtype UpdateThingShadowRequest = UpdateThingShadowRequest 
  { "ThingName'" :: (ThingName)
  , "Payload'" :: (JsonDocument)
  }
derive instance newtypeUpdateThingShadowRequest :: Newtype UpdateThingShadowRequest _


-- | <p>The output from the UpdateThingShadow operation.</p>
newtype UpdateThingShadowResponse = UpdateThingShadowResponse 
  { "Payload'" :: NullOrUndefined (JsonDocument)
  }
derive instance newtypeUpdateThingShadowResponse :: Newtype UpdateThingShadowResponse _


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
