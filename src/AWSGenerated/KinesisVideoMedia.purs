

-- | <p/>
module AWS.KinesisVideoMedia where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "KinesisVideoMedia" :: String


-- | <p> Use this API to retrieve media content from a Kinesis video stream. In the request, you identify stream name or stream Amazon Resource Name (ARN), and the starting chunk. Kinesis Video Streams then returns a stream of chunks in order by fragment number.</p> <note> <p> You must first call the <code>GetDataEndpoint</code> API to get an endpoint to which you can then send the <code>GetMedia</code> requests. </p> </note> <p>When you put media data (fragments) on a stream, Kinesis Video Streams stores each incoming fragment and related metadata in what is called a "chunk." For more information, see . The <code>GetMedia</code> API returns a stream of these chunks starting from the chunk that you specify in the request. </p> <p>The following limits apply when using the <code>GetMedia</code> API:</p> <ul> <li> <p>A client can call <code>GetMedia</code> up to five times per second per stream. </p> </li> <li> <p>Kinesis Video Streams sends media data at a rate of up to 25 megabytes per second (or 200 megabits per second) during a <code>GetMedia</code> session. </p> </li> </ul>
getMedia :: forall eff. GetMediaInput -> Aff (exception :: EXCEPTION | eff) GetMediaOutput
getMedia = Request.request serviceName "getMedia" 


-- | <p>Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.</p>
newtype ClientLimitExceededException = ClientLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeClientLimitExceededException :: Newtype ClientLimitExceededException _
derive instance repGenericClientLimitExceededException :: Generic ClientLimitExceededException _
instance showClientLimitExceededException :: Show ClientLimitExceededException where
  show = genericShow
instance decodeClientLimitExceededException :: Decode ClientLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientLimitExceededException :: Encode ClientLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client connections.</p>
newtype ConnectionLimitExceededException = ConnectionLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConnectionLimitExceededException :: Newtype ConnectionLimitExceededException _
derive instance repGenericConnectionLimitExceededException :: Generic ConnectionLimitExceededException _
instance showConnectionLimitExceededException :: Show ConnectionLimitExceededException where
  show = genericShow
instance decodeConnectionLimitExceededException :: Decode ConnectionLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionLimitExceededException :: Encode ConnectionLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _
derive instance repGenericContentType :: Generic ContentType _
instance showContentType :: Show ContentType where
  show = genericShow
instance decodeContentType :: Decode ContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContentType :: Encode ContentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContinuationToken = ContinuationToken String
derive instance newtypeContinuationToken :: Newtype ContinuationToken _
derive instance repGenericContinuationToken :: Generic ContinuationToken _
instance showContinuationToken :: Show ContinuationToken where
  show = genericShow
instance decodeContinuationToken :: Decode ContinuationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContinuationToken :: Encode ContinuationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FragmentNumberString = FragmentNumberString String
derive instance newtypeFragmentNumberString :: Newtype FragmentNumberString _
derive instance repGenericFragmentNumberString :: Generic FragmentNumberString _
instance showFragmentNumberString :: Show FragmentNumberString where
  show = genericShow
instance decodeFragmentNumberString :: Decode FragmentNumberString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFragmentNumberString :: Encode FragmentNumberString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMediaInput = GetMediaInput 
  { "StreamName" :: NullOrUndefined.NullOrUndefined (StreamName)
  , "StreamARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  , "StartSelector" :: (StartSelector)
  }
derive instance newtypeGetMediaInput :: Newtype GetMediaInput _
derive instance repGenericGetMediaInput :: Generic GetMediaInput _
instance showGetMediaInput :: Show GetMediaInput where
  show = genericShow
instance decodeGetMediaInput :: Decode GetMediaInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMediaInput :: Encode GetMediaInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMediaOutput = GetMediaOutput 
  { "ContentType" :: NullOrUndefined.NullOrUndefined (ContentType)
  , "Payload" :: NullOrUndefined.NullOrUndefined (Payload)
  }
derive instance newtypeGetMediaOutput :: Newtype GetMediaOutput _
derive instance repGenericGetMediaOutput :: Generic GetMediaOutput _
instance showGetMediaOutput :: Show GetMediaOutput where
  show = genericShow
instance decodeGetMediaOutput :: Decode GetMediaOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMediaOutput :: Encode GetMediaOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value for this input parameter is invalid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArgumentException :: Newtype InvalidArgumentException _
derive instance repGenericInvalidArgumentException :: Generic InvalidArgumentException _
instance showInvalidArgumentException :: Show InvalidArgumentException where
  show = genericShow
instance decodeInvalidArgumentException :: Decode InvalidArgumentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArgumentException :: Encode InvalidArgumentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Status Code: 400, Caller used wrong endpoint to write data to a stream. On receiving such an exception, the user must call <code>GetDataEndpoint</code> with <code>AccessMode</code> set to "READ" and use the endpoint Kinesis Video returns in the next <code>GetMedia</code> call. </p>
newtype InvalidEndpointException = InvalidEndpointException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidEndpointException :: Newtype InvalidEndpointException _
derive instance repGenericInvalidEndpointException :: Generic InvalidEndpointException _
instance showInvalidEndpointException :: Show InvalidEndpointException where
  show = genericShow
instance decodeInvalidEndpointException :: Decode InvalidEndpointException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidEndpointException :: Encode InvalidEndpointException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Status Code: 403, The caller is not authorized to perform an operation on the given stream, or the token has expired.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNotAuthorizedException :: Newtype NotAuthorizedException _
derive instance repGenericNotAuthorizedException :: Generic NotAuthorizedException _
instance showNotAuthorizedException :: Show NotAuthorizedException where
  show = genericShow
instance decodeNotAuthorizedException :: Decode NotAuthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotAuthorizedException :: Encode NotAuthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Payload = Payload String
derive instance newtypePayload :: Newtype Payload _
derive instance repGenericPayload :: Generic Payload _
instance showPayload :: Show Payload where
  show = genericShow
instance decodePayload :: Decode Payload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePayload :: Encode Payload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _
derive instance repGenericResourceARN :: Generic ResourceARN _
instance showResourceARN :: Show ResourceARN where
  show = genericShow
instance decodeResourceARN :: Decode ResourceARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceARN :: Encode ResourceARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Status Code: 404, The stream with the given name does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies the chunk on the Kinesis video stream where you want the <code>GetMedia</code> API to start returning media data. You have the following options to identify the starting chunk: </p> <ul> <li> <p>Choose the latest (or oldest) chunk.</p> </li> <li> <p>Identify a specific chunk. You can identify a specific chunk either by providing a fragment number or time stamp (server or producer). </p> </li> <li> <p>Each chunk's metadata includes a continuation token as a Matroska (MKV) tag (<code>AWS_KINESISVIDEO_CONTINUATION_TOKEN</code>). If your previous <code>GetMedia</code> request terminated, you can use this tag value in your next <code>GetMedia</code> request. The API then starts returning chunks starting where the last API ended.</p> </li> </ul>
newtype StartSelector = StartSelector 
  { "StartSelectorType" :: (StartSelectorType)
  , "AfterFragmentNumber" :: NullOrUndefined.NullOrUndefined (FragmentNumberString)
  , "StartTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "ContinuationToken" :: NullOrUndefined.NullOrUndefined (ContinuationToken)
  }
derive instance newtypeStartSelector :: Newtype StartSelector _
derive instance repGenericStartSelector :: Generic StartSelector _
instance showStartSelector :: Show StartSelector where
  show = genericShow
instance decodeStartSelector :: Decode StartSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartSelector :: Encode StartSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartSelectorType = StartSelectorType String
derive instance newtypeStartSelectorType :: Newtype StartSelectorType _
derive instance repGenericStartSelectorType :: Generic StartSelectorType _
instance showStartSelectorType :: Show StartSelectorType where
  show = genericShow
instance decodeStartSelectorType :: Decode StartSelectorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartSelectorType :: Encode StartSelectorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamName = StreamName String
derive instance newtypeStreamName :: Newtype StreamName _
derive instance repGenericStreamName :: Generic StreamName _
instance showStreamName :: Show StreamName where
  show = genericShow
instance decodeStreamName :: Decode StreamName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamName :: Encode StreamName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
