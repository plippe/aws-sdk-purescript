

-- | <p/>
module AWS.KinesisVideoArchivedMedia where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "KinesisVideoArchivedMedia" :: String


-- | <p>Gets media for a list of fragments (specified by fragment number) from the archived data in a Kinesis video stream.</p> <note> <p>This operation is only available for the AWS SDK for Java. It is not supported in AWS SDKs for other languages.</p> </note> <p>The following limits apply when using the <code>GetMediaForFragmentList</code> API:</p> <ul> <li> <p>A client can call <code>GetMediaForFragmentList</code> up to five times per second per stream. </p> </li> <li> <p>Kinesis Video Streams sends media data at a rate of up to 25 megabytes per second (or 200 megabits per second) during a <code>GetMediaForFragmentList</code> session. </p> </li> </ul>
getMediaForFragmentList :: forall eff. GetMediaForFragmentListInput -> Aff (err :: AWS.RequestError | eff) GetMediaForFragmentListOutput
getMediaForFragmentList = AWS.request serviceName "GetMediaForFragmentList" 


-- | <p>Returns a list of <a>Fragment</a> objects from the specified stream and start location within the archived data.</p>
listFragments :: forall eff. ListFragmentsInput -> Aff (err :: AWS.RequestError | eff) ListFragmentsOutput
listFragments = AWS.request serviceName "ListFragments" 


-- | <p>Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.</p>
newtype ClientLimitExceededException = ClientLimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ContentType = ContentType String


newtype ErrorMessage = ErrorMessage String


-- | <p>Represents a segment of video or other time-delimited data.</p>
newtype Fragment = Fragment 
  { "FragmentNumber" :: NullOrUndefined (String)
  , "FragmentSizeInBytes" :: NullOrUndefined (Number)
  , "ProducerTimestamp" :: NullOrUndefined (Number)
  , "ServerTimestamp" :: NullOrUndefined (Number)
  , "FragmentLengthInMilliseconds" :: NullOrUndefined (Number)
  }


newtype FragmentList = FragmentList (Array Fragment)


newtype FragmentNumberList = FragmentNumberList (Array FragmentNumberString)


newtype FragmentNumberString = FragmentNumberString String


-- | <p>Describes the time stamp range and time stamp origin of a range of fragments.</p>
newtype FragmentSelector = FragmentSelector 
  { "FragmentSelectorType" :: (FragmentSelectorType)
  , "TimestampRange" :: (TimestampRange)
  }


newtype FragmentSelectorType = FragmentSelectorType String


newtype GetMediaForFragmentListInput = GetMediaForFragmentListInput 
  { "StreamName" :: (StreamName)
  , "Fragments" :: (FragmentNumberList)
  }


newtype GetMediaForFragmentListOutput = GetMediaForFragmentListOutput 
  { "ContentType" :: NullOrUndefined (ContentType)
  , "Payload" :: NullOrUndefined (Payload)
  }


-- | <p>A specified parameter exceeds its restrictions, is not supported, or can't be used.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ListFragmentsInput = ListFragmentsInput 
  { "StreamName" :: (StreamName)
  , "MaxResults" :: NullOrUndefined (PageLimit)
  , "NextToken" :: NullOrUndefined (String)
  , "FragmentSelector" :: NullOrUndefined (FragmentSelector)
  }


newtype ListFragmentsOutput = ListFragmentsOutput 
  { "Fragments" :: NullOrUndefined (FragmentList)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | <p>Status Code: 403, The caller is not authorized to perform an operation on the given stream, or the token has expired.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype PageLimit = PageLimit Number


newtype Payload = Payload String


-- | <p>Kinesis Video Streams can't find the stream that you specified.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype StreamName = StreamName String


-- | <p>The range of time stamps for which to return fragments.</p>
newtype TimestampRange = TimestampRange 
  { "StartTimestamp" :: (Number)
  , "EndTimestamp" :: (Number)
  }
