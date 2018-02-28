

-- | <fullname>AWS CloudTrail</fullname> <p>This is the CloudTrail API Reference. It provides descriptions of actions, data types, common parameters, and common errors for CloudTrail.</p> <p>CloudTrail is a web service that records AWS API calls for your AWS account and delivers log files to an Amazon S3 bucket. The recorded information includes the identity of the user, the start time of the AWS API call, the source IP address, the request parameters, and the response elements returned by the service.</p> <note> <p>As an alternative to the API, you can use one of the AWS SDKs, which consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .NET, iOS, Android, etc.). The SDKs provide a convenient way to create programmatic access to AWSCloudTrail. For example, the SDKs take care of cryptographically signing requests, managing errors, and retrying requests automatically. For information about the AWS SDKs, including how to download and install them, see the <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services page</a>.</p> </note> <p>See the <a href="http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-user-guide.html">AWS CloudTrail User Guide</a> for information about the data that is included with each AWS API call listed in the log files.</p>
module AWS.CloudTrail where

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

serviceName = "CloudTrail" :: String


-- | <p>Adds one or more tags to a trail, up to a limit of 50. Tags must be unique per trail. Overwrites an existing tag's value when a new value is specified for an existing tag key. If you specify a key without a value, the tag will be created with the specified key and a value of null. You can tag a trail that applies to all regions only from the region in which the trail was created (that is, from its home region).</p>
addTags :: forall eff. AddTagsRequest -> Aff (exception :: EXCEPTION | eff) AddTagsResponse
addTags = Request.request serviceName "addTags" 


-- | <p>Creates a trail that specifies the settings for delivery of log data to an Amazon S3 bucket. A maximum of five trails can exist in a region, irrespective of the region in which they were created.</p>
createTrail :: forall eff. CreateTrailRequest -> Aff (exception :: EXCEPTION | eff) CreateTrailResponse
createTrail = Request.request serviceName "createTrail" 


-- | <p>Deletes a trail. This operation must be called from the region in which the trail was created. <code>DeleteTrail</code> cannot be called on the shadow trails (replicated trails in other regions) of a trail that is enabled in all regions.</p>
deleteTrail :: forall eff. DeleteTrailRequest -> Aff (exception :: EXCEPTION | eff) DeleteTrailResponse
deleteTrail = Request.request serviceName "deleteTrail" 


-- | <p>Retrieves settings for the trail associated with the current region for your account.</p>
describeTrails :: forall eff. DescribeTrailsRequest -> Aff (exception :: EXCEPTION | eff) DescribeTrailsResponse
describeTrails = Request.request serviceName "describeTrails" 


-- | <p>Describes the settings for the event selectors that you configured for your trail. The information returned for your event selectors includes the following:</p> <ul> <li> <p>The S3 objects that you are logging for data events.</p> </li> <li> <p>If your event selector includes management events.</p> </li> <li> <p>If your event selector includes read-only events, write-only events, or all. </p> </li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html">Logging Data and Management Events for Trails </a> in the <i>AWS CloudTrail User Guide</i>.</p>
getEventSelectors :: forall eff. GetEventSelectorsRequest -> Aff (exception :: EXCEPTION | eff) GetEventSelectorsResponse
getEventSelectors = Request.request serviceName "getEventSelectors" 


-- | <p>Returns a JSON-formatted list of information about the specified trail. Fields include information on delivery errors, Amazon SNS and Amazon S3 errors, and start and stop logging times for each trail. This operation returns trail status from a single region. To return trail status from all regions, you must call the operation on each region.</p>
getTrailStatus :: forall eff. GetTrailStatusRequest -> Aff (exception :: EXCEPTION | eff) GetTrailStatusResponse
getTrailStatus = Request.request serviceName "getTrailStatus" 


-- | <p>Returns all public keys whose private keys were used to sign the digest files within the specified time range. The public key is needed to validate digest files that were signed with its corresponding private key.</p> <note> <p>CloudTrail uses different private/public key pairs per region. Each digest file is signed with a private key unique to its region. Therefore, when you validate a digest file from a particular region, you must look in the same region for its corresponding public key.</p> </note>
listPublicKeys :: forall eff. ListPublicKeysRequest -> Aff (exception :: EXCEPTION | eff) ListPublicKeysResponse
listPublicKeys = Request.request serviceName "listPublicKeys" 


-- | <p>Lists the tags for the trail in the current region.</p>
listTags :: forall eff. ListTagsRequest -> Aff (exception :: EXCEPTION | eff) ListTagsResponse
listTags = Request.request serviceName "listTags" 


-- | <p>Looks up API activity events captured by CloudTrail that create, update, or delete resources in your account. Events for a region can be looked up for the times in which you had CloudTrail turned on in that region during the last seven days. Lookup supports the following attributes:</p> <ul> <li> <p>Event ID</p> </li> <li> <p>Event name</p> </li> <li> <p>Event source</p> </li> <li> <p>Resource name</p> </li> <li> <p>Resource type</p> </li> <li> <p>User name</p> </li> </ul> <p>All attributes are optional. The default number of results returned is 10, with a maximum of 50 possible. The response includes a token that you can use to get the next page of results.</p> <important> <p>The rate of lookup requests is limited to one per second per account. If this limit is exceeded, a throttling error occurs.</p> </important> <important> <p>Events that occurred during the selected time range will not be available for lookup if CloudTrail logging was not enabled when the events occurred.</p> </important>
lookupEvents :: forall eff. LookupEventsRequest -> Aff (exception :: EXCEPTION | eff) LookupEventsResponse
lookupEvents = Request.request serviceName "lookupEvents" 


-- | <p>Configures an event selector for your trail. Use event selectors to specify whether you want your trail to log management and/or data events. When an event occurs in your account, CloudTrail evaluates the event selectors in all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event. </p> <p>Example</p> <ol> <li> <p>You create an event selector for a trail and specify that you want write-only events.</p> </li> <li> <p>The EC2 <code>GetConsoleOutput</code> and <code>RunInstances</code> API operations occur in your account.</p> </li> <li> <p>CloudTrail evaluates whether the events match your event selectors.</p> </li> <li> <p>The <code>RunInstances</code> is a write-only event and it matches your event selector. The trail logs the event.</p> </li> <li> <p>The <code>GetConsoleOutput</code> is a read-only event but it doesn't match your event selector. The trail doesn't log the event. </p> </li> </ol> <p>The <code>PutEventSelectors</code> operation must be called from the region in which the trail was created; otherwise, an <code>InvalidHomeRegionException</code> is thrown.</p> <p>You can configure up to five event selectors for each trail. For more information, see <a href="http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html">Logging Data and Management Events for Trails </a> in the <i>AWS CloudTrail User Guide</i>.</p>
putEventSelectors :: forall eff. PutEventSelectorsRequest -> Aff (exception :: EXCEPTION | eff) PutEventSelectorsResponse
putEventSelectors = Request.request serviceName "putEventSelectors" 


-- | <p>Removes the specified tags from a trail.</p>
removeTags :: forall eff. RemoveTagsRequest -> Aff (exception :: EXCEPTION | eff) RemoveTagsResponse
removeTags = Request.request serviceName "removeTags" 


-- | <p>Starts the recording of AWS API calls and log file delivery for a trail. For a trail that is enabled in all regions, this operation must be called from the region in which the trail was created. This operation cannot be called on the shadow trails (replicated trails in other regions) of a trail that is enabled in all regions.</p>
startLogging :: forall eff. StartLoggingRequest -> Aff (exception :: EXCEPTION | eff) StartLoggingResponse
startLogging = Request.request serviceName "startLogging" 


-- | <p>Suspends the recording of AWS API calls and log file delivery for the specified trail. Under most circumstances, there is no need to use this action. You can update a trail without stopping it first. This action is the only way to stop recording. For a trail enabled in all regions, this operation must be called from the region in which the trail was created, or an <code>InvalidHomeRegionException</code> will occur. This operation cannot be called on the shadow trails (replicated trails in other regions) of a trail enabled in all regions.</p>
stopLogging :: forall eff. StopLoggingRequest -> Aff (exception :: EXCEPTION | eff) StopLoggingResponse
stopLogging = Request.request serviceName "stopLogging" 


-- | <p>Updates the settings that specify delivery of log files. Changes to a trail do not require stopping the CloudTrail service. Use this action to designate an existing bucket for log delivery. If the existing bucket has previously been a target for CloudTrail log files, an IAM policy exists for the bucket. <code>UpdateTrail</code> must be called from the region in which the trail was created; otherwise, an <code>InvalidHomeRegionException</code> is thrown.</p>
updateTrail :: forall eff. UpdateTrailRequest -> Aff (exception :: EXCEPTION | eff) UpdateTrailResponse
updateTrail = Request.request serviceName "updateTrail" 


-- | <p>Specifies the tags to add to a trail.</p>
newtype AddTagsRequest = AddTagsRequest 
  { "ResourceId" :: (String)
  , "TagsList" :: NullOrUndefined.NullOrUndefined (TagsList)
  }
derive instance newtypeAddTagsRequest :: Newtype AddTagsRequest _
derive instance repGenericAddTagsRequest :: Generic AddTagsRequest _
instance showAddTagsRequest :: Show AddTagsRequest where
  show = genericShow
instance decodeAddTagsRequest :: Decode AddTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsRequest :: Encode AddTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype AddTagsResponse = AddTagsResponse Types.NoArguments
derive instance newtypeAddTagsResponse :: Newtype AddTagsResponse _
derive instance repGenericAddTagsResponse :: Generic AddTagsResponse _
instance showAddTagsResponse :: Show AddTagsResponse where
  show = genericShow
instance decodeAddTagsResponse :: Decode AddTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsResponse :: Encode AddTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ByteBuffer = ByteBuffer String
derive instance newtypeByteBuffer :: Newtype ByteBuffer _
derive instance repGenericByteBuffer :: Generic ByteBuffer _
instance showByteBuffer :: Show ByteBuffer where
  show = genericShow
instance decodeByteBuffer :: Decode ByteBuffer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeByteBuffer :: Encode ByteBuffer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when an operation is called with an invalid trail ARN. The format of a trail ARN is:</p> <p> <code>arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail</code> </p>
newtype CloudTrailARNInvalidException = CloudTrailARNInvalidException Types.NoArguments
derive instance newtypeCloudTrailARNInvalidException :: Newtype CloudTrailARNInvalidException _
derive instance repGenericCloudTrailARNInvalidException :: Generic CloudTrailARNInvalidException _
instance showCloudTrailARNInvalidException :: Show CloudTrailARNInvalidException where
  show = genericShow
instance decodeCloudTrailARNInvalidException :: Decode CloudTrailARNInvalidException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudTrailARNInvalidException :: Encode CloudTrailARNInvalidException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Cannot set a CloudWatch Logs delivery for this region.</p>
newtype CloudWatchLogsDeliveryUnavailableException = CloudWatchLogsDeliveryUnavailableException Types.NoArguments
derive instance newtypeCloudWatchLogsDeliveryUnavailableException :: Newtype CloudWatchLogsDeliveryUnavailableException _
derive instance repGenericCloudWatchLogsDeliveryUnavailableException :: Generic CloudWatchLogsDeliveryUnavailableException _
instance showCloudWatchLogsDeliveryUnavailableException :: Show CloudWatchLogsDeliveryUnavailableException where
  show = genericShow
instance decodeCloudWatchLogsDeliveryUnavailableException :: Decode CloudWatchLogsDeliveryUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLogsDeliveryUnavailableException :: Encode CloudWatchLogsDeliveryUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the settings for each trail.</p>
newtype CreateTrailRequest = CreateTrailRequest 
  { "Name" :: (String)
  , "S3BucketName" :: (String)
  , "S3KeyPrefix" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined.NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "EnableLogFileValidation" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined.NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateTrailRequest :: Newtype CreateTrailRequest _
derive instance repGenericCreateTrailRequest :: Generic CreateTrailRequest _
instance showCreateTrailRequest :: Show CreateTrailRequest where
  show = genericShow
instance decodeCreateTrailRequest :: Decode CreateTrailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrailRequest :: Encode CreateTrailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype CreateTrailResponse = CreateTrailResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "S3BucketName" :: NullOrUndefined.NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicARN" :: NullOrUndefined.NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "TrailARN" :: NullOrUndefined.NullOrUndefined (String)
  , "LogFileValidationEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined.NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateTrailResponse :: Newtype CreateTrailResponse _
derive instance repGenericCreateTrailResponse :: Generic CreateTrailResponse _
instance showCreateTrailResponse :: Show CreateTrailResponse where
  show = genericShow
instance decodeCreateTrailResponse :: Decode CreateTrailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrailResponse :: Encode CreateTrailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon S3 objects that you specify in your event selectors for your trail to log data events. Data events are object-level API operations that access S3 objects, such as <code>GetObject</code>, <code>DeleteObject</code>, and <code>PutObject</code>. You can specify up to 250 S3 buckets and object prefixes for a trail. </p> <p>Example</p> <ol> <li> <p>You create an event selector for a trail and specify an S3 bucket and an empty prefix, such as <code>arn:aws:s3:::bucket-1/</code>.</p> </li> <li> <p>You upload an image file to <code>bucket-1</code>.</p> </li> <li> <p>The <code>PutObject</code> API operation occurs on an object in the S3 bucket that you specified in the event selector. The trail processes and logs the event.</p> </li> <li> <p>You upload another image file to a different S3 bucket named <code>arn:aws:s3:::bucket-2</code>.</p> </li> <li> <p>The event occurs on an object in an S3 bucket that you didn't specify in the event selector. The trail doesn’t log the event.</p> </li> </ol>
newtype DataResource = DataResource 
  { "Type" :: NullOrUndefined.NullOrUndefined (String)
  , "Values" :: NullOrUndefined.NullOrUndefined (DataResourceValues)
  }
derive instance newtypeDataResource :: Newtype DataResource _
derive instance repGenericDataResource :: Generic DataResource _
instance showDataResource :: Show DataResource where
  show = genericShow
instance decodeDataResource :: Decode DataResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataResource :: Encode DataResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataResourceValues = DataResourceValues (Array String)
derive instance newtypeDataResourceValues :: Newtype DataResourceValues _
derive instance repGenericDataResourceValues :: Generic DataResourceValues _
instance showDataResourceValues :: Show DataResourceValues where
  show = genericShow
instance decodeDataResourceValues :: Decode DataResourceValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataResourceValues :: Encode DataResourceValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataResources = DataResources (Array DataResource)
derive instance newtypeDataResources :: Newtype DataResources _
derive instance repGenericDataResources :: Generic DataResources _
instance showDataResources :: Show DataResources where
  show = genericShow
instance decodeDataResources :: Decode DataResources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataResources :: Encode DataResources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where
  show = genericShow
instance decodeDate :: Decode Date where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDate :: Encode Date where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request that specifies the name of a trail to delete.</p>
newtype DeleteTrailRequest = DeleteTrailRequest 
  { "Name" :: (String)
  }
derive instance newtypeDeleteTrailRequest :: Newtype DeleteTrailRequest _
derive instance repGenericDeleteTrailRequest :: Generic DeleteTrailRequest _
instance showDeleteTrailRequest :: Show DeleteTrailRequest where
  show = genericShow
instance decodeDeleteTrailRequest :: Decode DeleteTrailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTrailRequest :: Encode DeleteTrailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype DeleteTrailResponse = DeleteTrailResponse Types.NoArguments
derive instance newtypeDeleteTrailResponse :: Newtype DeleteTrailResponse _
derive instance repGenericDeleteTrailResponse :: Generic DeleteTrailResponse _
instance showDeleteTrailResponse :: Show DeleteTrailResponse where
  show = genericShow
instance decodeDeleteTrailResponse :: Decode DeleteTrailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTrailResponse :: Encode DeleteTrailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about the trail.</p>
newtype DescribeTrailsRequest = DescribeTrailsRequest 
  { "TrailNameList'" :: NullOrUndefined.NullOrUndefined (TrailNameList)
  , "IncludeShadowTrails'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDescribeTrailsRequest :: Newtype DescribeTrailsRequest _
derive instance repGenericDescribeTrailsRequest :: Generic DescribeTrailsRequest _
instance showDescribeTrailsRequest :: Show DescribeTrailsRequest where
  show = genericShow
instance decodeDescribeTrailsRequest :: Decode DescribeTrailsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTrailsRequest :: Encode DescribeTrailsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype DescribeTrailsResponse = DescribeTrailsResponse 
  { "TrailList'" :: NullOrUndefined.NullOrUndefined (TrailList)
  }
derive instance newtypeDescribeTrailsResponse :: Newtype DescribeTrailsResponse _
derive instance repGenericDescribeTrailsResponse :: Generic DescribeTrailsResponse _
instance showDescribeTrailsResponse :: Show DescribeTrailsResponse where
  show = genericShow
instance decodeDescribeTrailsResponse :: Decode DescribeTrailsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTrailsResponse :: Encode DescribeTrailsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about an event that was returned by a lookup request. The result includes a representation of a CloudTrail event.</p>
newtype Event = Event 
  { "EventId" :: NullOrUndefined.NullOrUndefined (String)
  , "EventName" :: NullOrUndefined.NullOrUndefined (String)
  , "EventTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EventSource" :: NullOrUndefined.NullOrUndefined (String)
  , "Username" :: NullOrUndefined.NullOrUndefined (String)
  , "Resources" :: NullOrUndefined.NullOrUndefined (ResourceList)
  , "CloudTrailEvent" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEvent :: Newtype Event _
derive instance repGenericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance decodeEvent :: Decode Event where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvent :: Encode Event where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Use event selectors to specify whether you want your trail to log management and/or data events. When an event occurs in your account, CloudTrail evaluates the event selector for all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event.</p> <p>You can configure up to five event selectors for a trail.</p>
newtype EventSelector = EventSelector 
  { "ReadWriteType" :: NullOrUndefined.NullOrUndefined (ReadWriteType)
  , "IncludeManagementEvents" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DataResources" :: NullOrUndefined.NullOrUndefined (DataResources)
  }
derive instance newtypeEventSelector :: Newtype EventSelector _
derive instance repGenericEventSelector :: Generic EventSelector _
instance showEventSelector :: Show EventSelector where
  show = genericShow
instance decodeEventSelector :: Decode EventSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSelector :: Encode EventSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventSelectors = EventSelectors (Array EventSelector)
derive instance newtypeEventSelectors :: Newtype EventSelectors _
derive instance repGenericEventSelectors :: Generic EventSelectors _
instance showEventSelectors :: Show EventSelectors where
  show = genericShow
instance decodeEventSelectors :: Decode EventSelectors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSelectors :: Encode EventSelectors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventsList = EventsList (Array Event)
derive instance newtypeEventsList :: Newtype EventsList _
derive instance repGenericEventsList :: Generic EventsList _
instance showEventsList :: Show EventsList where
  show = genericShow
instance decodeEventsList :: Decode EventsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventsList :: Encode EventsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEventSelectorsRequest = GetEventSelectorsRequest 
  { "TrailName" :: (String)
  }
derive instance newtypeGetEventSelectorsRequest :: Newtype GetEventSelectorsRequest _
derive instance repGenericGetEventSelectorsRequest :: Generic GetEventSelectorsRequest _
instance showGetEventSelectorsRequest :: Show GetEventSelectorsRequest where
  show = genericShow
instance decodeGetEventSelectorsRequest :: Decode GetEventSelectorsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEventSelectorsRequest :: Encode GetEventSelectorsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetEventSelectorsResponse = GetEventSelectorsResponse 
  { "TrailARN" :: NullOrUndefined.NullOrUndefined (String)
  , "EventSelectors" :: NullOrUndefined.NullOrUndefined (EventSelectors)
  }
derive instance newtypeGetEventSelectorsResponse :: Newtype GetEventSelectorsResponse _
derive instance repGenericGetEventSelectorsResponse :: Generic GetEventSelectorsResponse _
instance showGetEventSelectorsResponse :: Show GetEventSelectorsResponse where
  show = genericShow
instance decodeGetEventSelectorsResponse :: Decode GetEventSelectorsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetEventSelectorsResponse :: Encode GetEventSelectorsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of a trail about which you want the current status.</p>
newtype GetTrailStatusRequest = GetTrailStatusRequest 
  { "Name" :: (String)
  }
derive instance newtypeGetTrailStatusRequest :: Newtype GetTrailStatusRequest _
derive instance repGenericGetTrailStatusRequest :: Generic GetTrailStatusRequest _
instance showGetTrailStatusRequest :: Show GetTrailStatusRequest where
  show = genericShow
instance decodeGetTrailStatusRequest :: Decode GetTrailStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTrailStatusRequest :: Encode GetTrailStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype GetTrailStatusResponse = GetTrailStatusResponse 
  { "IsLogging" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LatestDeliveryError" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestNotificationError" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestDeliveryTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LatestNotificationTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "StartLoggingTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "StopLoggingTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LatestCloudWatchLogsDeliveryError" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestCloudWatchLogsDeliveryTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LatestDigestDeliveryTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LatestDigestDeliveryError" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestDeliveryAttemptTime" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestNotificationAttemptTime" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestNotificationAttemptSucceeded" :: NullOrUndefined.NullOrUndefined (String)
  , "LatestDeliveryAttemptSucceeded" :: NullOrUndefined.NullOrUndefined (String)
  , "TimeLoggingStarted" :: NullOrUndefined.NullOrUndefined (String)
  , "TimeLoggingStopped" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetTrailStatusResponse :: Newtype GetTrailStatusResponse _
derive instance repGenericGetTrailStatusResponse :: Generic GetTrailStatusResponse _
instance showGetTrailStatusResponse :: Show GetTrailStatusResponse where
  show = genericShow
instance decodeGetTrailStatusResponse :: Decode GetTrailStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTrailStatusResponse :: Encode GetTrailStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the policy on the S3 bucket or KMS key is not sufficient.</p>
newtype InsufficientEncryptionPolicyException = InsufficientEncryptionPolicyException Types.NoArguments
derive instance newtypeInsufficientEncryptionPolicyException :: Newtype InsufficientEncryptionPolicyException _
derive instance repGenericInsufficientEncryptionPolicyException :: Generic InsufficientEncryptionPolicyException _
instance showInsufficientEncryptionPolicyException :: Show InsufficientEncryptionPolicyException where
  show = genericShow
instance decodeInsufficientEncryptionPolicyException :: Decode InsufficientEncryptionPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientEncryptionPolicyException :: Encode InsufficientEncryptionPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the policy on the S3 bucket is not sufficient.</p>
newtype InsufficientS3BucketPolicyException = InsufficientS3BucketPolicyException Types.NoArguments
derive instance newtypeInsufficientS3BucketPolicyException :: Newtype InsufficientS3BucketPolicyException _
derive instance repGenericInsufficientS3BucketPolicyException :: Generic InsufficientS3BucketPolicyException _
instance showInsufficientS3BucketPolicyException :: Show InsufficientS3BucketPolicyException where
  show = genericShow
instance decodeInsufficientS3BucketPolicyException :: Decode InsufficientS3BucketPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientS3BucketPolicyException :: Encode InsufficientS3BucketPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the policy on the SNS topic is not sufficient.</p>
newtype InsufficientSnsTopicPolicyException = InsufficientSnsTopicPolicyException Types.NoArguments
derive instance newtypeInsufficientSnsTopicPolicyException :: Newtype InsufficientSnsTopicPolicyException _
derive instance repGenericInsufficientSnsTopicPolicyException :: Generic InsufficientSnsTopicPolicyException _
instance showInsufficientSnsTopicPolicyException :: Show InsufficientSnsTopicPolicyException where
  show = genericShow
instance decodeInsufficientSnsTopicPolicyException :: Decode InsufficientSnsTopicPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientSnsTopicPolicyException :: Encode InsufficientSnsTopicPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the provided CloudWatch log group is not valid.</p>
newtype InvalidCloudWatchLogsLogGroupArnException = InvalidCloudWatchLogsLogGroupArnException Types.NoArguments
derive instance newtypeInvalidCloudWatchLogsLogGroupArnException :: Newtype InvalidCloudWatchLogsLogGroupArnException _
derive instance repGenericInvalidCloudWatchLogsLogGroupArnException :: Generic InvalidCloudWatchLogsLogGroupArnException _
instance showInvalidCloudWatchLogsLogGroupArnException :: Show InvalidCloudWatchLogsLogGroupArnException where
  show = genericShow
instance decodeInvalidCloudWatchLogsLogGroupArnException :: Decode InvalidCloudWatchLogsLogGroupArnException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCloudWatchLogsLogGroupArnException :: Encode InvalidCloudWatchLogsLogGroupArnException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the provided role is not valid.</p>
newtype InvalidCloudWatchLogsRoleArnException = InvalidCloudWatchLogsRoleArnException Types.NoArguments
derive instance newtypeInvalidCloudWatchLogsRoleArnException :: Newtype InvalidCloudWatchLogsRoleArnException _
derive instance repGenericInvalidCloudWatchLogsRoleArnException :: Generic InvalidCloudWatchLogsRoleArnException _
instance showInvalidCloudWatchLogsRoleArnException :: Show InvalidCloudWatchLogsRoleArnException where
  show = genericShow
instance decodeInvalidCloudWatchLogsRoleArnException :: Decode InvalidCloudWatchLogsRoleArnException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCloudWatchLogsRoleArnException :: Encode InvalidCloudWatchLogsRoleArnException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the <code>PutEventSelectors</code> operation is called with an invalid number of event selectors, data resources, or an invalid value for a parameter:</p> <ul> <li> <p>Specify a valid number of event selectors (1 to 5) for a trail.</p> </li> <li> <p>Specify a valid number of data resources (1 to 250) for an event selector.</p> </li> <li> <p>Specify a valid value for a parameter. For example, specifying the <code>ReadWriteType</code> parameter with a value of <code>read-only</code> is invalid.</p> </li> </ul>
newtype InvalidEventSelectorsException = InvalidEventSelectorsException Types.NoArguments
derive instance newtypeInvalidEventSelectorsException :: Newtype InvalidEventSelectorsException _
derive instance repGenericInvalidEventSelectorsException :: Generic InvalidEventSelectorsException _
instance showInvalidEventSelectorsException :: Show InvalidEventSelectorsException where
  show = genericShow
instance decodeInvalidEventSelectorsException :: Decode InvalidEventSelectorsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidEventSelectorsException :: Encode InvalidEventSelectorsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when an operation is called on a trail from a region other than the region in which the trail was created.</p>
newtype InvalidHomeRegionException = InvalidHomeRegionException Types.NoArguments
derive instance newtypeInvalidHomeRegionException :: Newtype InvalidHomeRegionException _
derive instance repGenericInvalidHomeRegionException :: Generic InvalidHomeRegionException _
instance showInvalidHomeRegionException :: Show InvalidHomeRegionException where
  show = genericShow
instance decodeInvalidHomeRegionException :: Decode InvalidHomeRegionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidHomeRegionException :: Encode InvalidHomeRegionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the KMS key ARN is invalid.</p>
newtype InvalidKmsKeyIdException = InvalidKmsKeyIdException Types.NoArguments
derive instance newtypeInvalidKmsKeyIdException :: Newtype InvalidKmsKeyIdException _
derive instance repGenericInvalidKmsKeyIdException :: Generic InvalidKmsKeyIdException _
instance showInvalidKmsKeyIdException :: Show InvalidKmsKeyIdException where
  show = genericShow
instance decodeInvalidKmsKeyIdException :: Decode InvalidKmsKeyIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidKmsKeyIdException :: Encode InvalidKmsKeyIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Occurs when an invalid lookup attribute is specified.</p>
newtype InvalidLookupAttributesException = InvalidLookupAttributesException Types.NoArguments
derive instance newtypeInvalidLookupAttributesException :: Newtype InvalidLookupAttributesException _
derive instance repGenericInvalidLookupAttributesException :: Generic InvalidLookupAttributesException _
instance showInvalidLookupAttributesException :: Show InvalidLookupAttributesException where
  show = genericShow
instance decodeInvalidLookupAttributesException :: Decode InvalidLookupAttributesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidLookupAttributesException :: Encode InvalidLookupAttributesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown if the limit specified is invalid.</p>
newtype InvalidMaxResultsException = InvalidMaxResultsException Types.NoArguments
derive instance newtypeInvalidMaxResultsException :: Newtype InvalidMaxResultsException _
derive instance repGenericInvalidMaxResultsException :: Generic InvalidMaxResultsException _
instance showInvalidMaxResultsException :: Show InvalidMaxResultsException where
  show = genericShow
instance decodeInvalidMaxResultsException :: Decode InvalidMaxResultsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidMaxResultsException :: Encode InvalidMaxResultsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Invalid token or token that was previously used in a request with different parameters. This exception is thrown if the token is invalid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException Types.NoArguments
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the combination of parameters provided is not valid.</p>
newtype InvalidParameterCombinationException = InvalidParameterCombinationException Types.NoArguments
derive instance newtypeInvalidParameterCombinationException :: Newtype InvalidParameterCombinationException _
derive instance repGenericInvalidParameterCombinationException :: Generic InvalidParameterCombinationException _
instance showInvalidParameterCombinationException :: Show InvalidParameterCombinationException where
  show = genericShow
instance decodeInvalidParameterCombinationException :: Decode InvalidParameterCombinationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterCombinationException :: Encode InvalidParameterCombinationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the provided S3 bucket name is not valid.</p>
newtype InvalidS3BucketNameException = InvalidS3BucketNameException Types.NoArguments
derive instance newtypeInvalidS3BucketNameException :: Newtype InvalidS3BucketNameException _
derive instance repGenericInvalidS3BucketNameException :: Generic InvalidS3BucketNameException _
instance showInvalidS3BucketNameException :: Show InvalidS3BucketNameException where
  show = genericShow
instance decodeInvalidS3BucketNameException :: Decode InvalidS3BucketNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidS3BucketNameException :: Encode InvalidS3BucketNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the provided S3 prefix is not valid.</p>
newtype InvalidS3PrefixException = InvalidS3PrefixException Types.NoArguments
derive instance newtypeInvalidS3PrefixException :: Newtype InvalidS3PrefixException _
derive instance repGenericInvalidS3PrefixException :: Generic InvalidS3PrefixException _
instance showInvalidS3PrefixException :: Show InvalidS3PrefixException where
  show = genericShow
instance decodeInvalidS3PrefixException :: Decode InvalidS3PrefixException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidS3PrefixException :: Encode InvalidS3PrefixException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the provided SNS topic name is not valid.</p>
newtype InvalidSnsTopicNameException = InvalidSnsTopicNameException Types.NoArguments
derive instance newtypeInvalidSnsTopicNameException :: Newtype InvalidSnsTopicNameException _
derive instance repGenericInvalidSnsTopicNameException :: Generic InvalidSnsTopicNameException _
instance showInvalidSnsTopicNameException :: Show InvalidSnsTopicNameException where
  show = genericShow
instance decodeInvalidSnsTopicNameException :: Decode InvalidSnsTopicNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSnsTopicNameException :: Encode InvalidSnsTopicNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the key or value specified for the tag does not match the regular expression <code>^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-@]*)$</code>.</p>
newtype InvalidTagParameterException = InvalidTagParameterException Types.NoArguments
derive instance newtypeInvalidTagParameterException :: Newtype InvalidTagParameterException _
derive instance repGenericInvalidTagParameterException :: Generic InvalidTagParameterException _
instance showInvalidTagParameterException :: Show InvalidTagParameterException where
  show = genericShow
instance decodeInvalidTagParameterException :: Decode InvalidTagParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTagParameterException :: Encode InvalidTagParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Occurs if the timestamp values are invalid. Either the start time occurs after the end time or the time range is outside the range of possible values.</p>
newtype InvalidTimeRangeException = InvalidTimeRangeException Types.NoArguments
derive instance newtypeInvalidTimeRangeException :: Newtype InvalidTimeRangeException _
derive instance repGenericInvalidTimeRangeException :: Generic InvalidTimeRangeException _
instance showInvalidTimeRangeException :: Show InvalidTimeRangeException where
  show = genericShow
instance decodeInvalidTimeRangeException :: Decode InvalidTimeRangeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTimeRangeException :: Encode InvalidTimeRangeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Reserved for future use.</p>
newtype InvalidTokenException = InvalidTokenException Types.NoArguments
derive instance newtypeInvalidTokenException :: Newtype InvalidTokenException _
derive instance repGenericInvalidTokenException :: Generic InvalidTokenException _
instance showInvalidTokenException :: Show InvalidTokenException where
  show = genericShow
instance decodeInvalidTokenException :: Decode InvalidTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTokenException :: Encode InvalidTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the provided trail name is not valid. Trail names must meet the following requirements:</p> <ul> <li> <p>Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)</p> </li> <li> <p>Start with a letter or number, and end with a letter or number</p> </li> <li> <p>Be between 3 and 128 characters</p> </li> <li> <p>Have no adjacent periods, underscores or dashes. Names like <code>my-_namespace</code> and <code>my--namespace</code> are invalid.</p> </li> <li> <p>Not be in IP address format (for example, 192.168.5.4)</p> </li> </ul>
newtype InvalidTrailNameException = InvalidTrailNameException Types.NoArguments
derive instance newtypeInvalidTrailNameException :: Newtype InvalidTrailNameException _
derive instance repGenericInvalidTrailNameException :: Generic InvalidTrailNameException _
instance showInvalidTrailNameException :: Show InvalidTrailNameException where
  show = genericShow
instance decodeInvalidTrailNameException :: Decode InvalidTrailNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTrailNameException :: Encode InvalidTrailNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when there is an issue with the specified KMS key and the trail can’t be updated.</p>
newtype KmsException = KmsException Types.NoArguments
derive instance newtypeKmsException :: Newtype KmsException _
derive instance repGenericKmsException :: Generic KmsException _
instance showKmsException :: Show KmsException where
  show = genericShow
instance decodeKmsException :: Decode KmsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKmsException :: Encode KmsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is deprecated.</p>
newtype KmsKeyDisabledException = KmsKeyDisabledException Types.NoArguments
derive instance newtypeKmsKeyDisabledException :: Newtype KmsKeyDisabledException _
derive instance repGenericKmsKeyDisabledException :: Generic KmsKeyDisabledException _
instance showKmsKeyDisabledException :: Show KmsKeyDisabledException where
  show = genericShow
instance decodeKmsKeyDisabledException :: Decode KmsKeyDisabledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKmsKeyDisabledException :: Encode KmsKeyDisabledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the KMS key does not exist, or when the S3 bucket and the KMS key are not in the same region.</p>
newtype KmsKeyNotFoundException = KmsKeyNotFoundException Types.NoArguments
derive instance newtypeKmsKeyNotFoundException :: Newtype KmsKeyNotFoundException _
derive instance repGenericKmsKeyNotFoundException :: Generic KmsKeyNotFoundException _
instance showKmsKeyNotFoundException :: Show KmsKeyNotFoundException where
  show = genericShow
instance decodeKmsKeyNotFoundException :: Decode KmsKeyNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKmsKeyNotFoundException :: Encode KmsKeyNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Requests the public keys for a specified time range.</p>
newtype ListPublicKeysRequest = ListPublicKeysRequest 
  { "StartTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListPublicKeysRequest :: Newtype ListPublicKeysRequest _
derive instance repGenericListPublicKeysRequest :: Generic ListPublicKeysRequest _
instance showListPublicKeysRequest :: Show ListPublicKeysRequest where
  show = genericShow
instance decodeListPublicKeysRequest :: Decode ListPublicKeysRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPublicKeysRequest :: Encode ListPublicKeysRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype ListPublicKeysResponse = ListPublicKeysResponse 
  { "PublicKeyList" :: NullOrUndefined.NullOrUndefined (PublicKeyList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListPublicKeysResponse :: Newtype ListPublicKeysResponse _
derive instance repGenericListPublicKeysResponse :: Generic ListPublicKeysResponse _
instance showListPublicKeysResponse :: Show ListPublicKeysResponse where
  show = genericShow
instance decodeListPublicKeysResponse :: Decode ListPublicKeysResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPublicKeysResponse :: Encode ListPublicKeysResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a list of trail tags to return.</p>
newtype ListTagsRequest = ListTagsRequest 
  { "ResourceIdList" :: (ResourceIdList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListTagsRequest :: Newtype ListTagsRequest _
derive instance repGenericListTagsRequest :: Generic ListTagsRequest _
instance showListTagsRequest :: Show ListTagsRequest where
  show = genericShow
instance decodeListTagsRequest :: Decode ListTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsRequest :: Encode ListTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype ListTagsResponse = ListTagsResponse 
  { "ResourceTagList" :: NullOrUndefined.NullOrUndefined (ResourceTagList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListTagsResponse :: Newtype ListTagsResponse _
derive instance repGenericListTagsResponse :: Generic ListTagsResponse _
instance showListTagsResponse :: Show ListTagsResponse where
  show = genericShow
instance decodeListTagsResponse :: Decode ListTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsResponse :: Encode ListTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies an attribute and value that filter the events returned.</p>
newtype LookupAttribute = LookupAttribute 
  { "AttributeKey" :: (LookupAttributeKey)
  , "AttributeValue" :: (String)
  }
derive instance newtypeLookupAttribute :: Newtype LookupAttribute _
derive instance repGenericLookupAttribute :: Generic LookupAttribute _
instance showLookupAttribute :: Show LookupAttribute where
  show = genericShow
instance decodeLookupAttribute :: Decode LookupAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLookupAttribute :: Encode LookupAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LookupAttributeKey = LookupAttributeKey String
derive instance newtypeLookupAttributeKey :: Newtype LookupAttributeKey _
derive instance repGenericLookupAttributeKey :: Generic LookupAttributeKey _
instance showLookupAttributeKey :: Show LookupAttributeKey where
  show = genericShow
instance decodeLookupAttributeKey :: Decode LookupAttributeKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLookupAttributeKey :: Encode LookupAttributeKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LookupAttributesList = LookupAttributesList (Array LookupAttribute)
derive instance newtypeLookupAttributesList :: Newtype LookupAttributesList _
derive instance repGenericLookupAttributesList :: Generic LookupAttributesList _
instance showLookupAttributesList :: Show LookupAttributesList where
  show = genericShow
instance decodeLookupAttributesList :: Decode LookupAttributesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLookupAttributesList :: Encode LookupAttributesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a request for LookupEvents.</p>
newtype LookupEventsRequest = LookupEventsRequest 
  { "LookupAttributes" :: NullOrUndefined.NullOrUndefined (LookupAttributesList)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeLookupEventsRequest :: Newtype LookupEventsRequest _
derive instance repGenericLookupEventsRequest :: Generic LookupEventsRequest _
instance showLookupEventsRequest :: Show LookupEventsRequest where
  show = genericShow
instance decodeLookupEventsRequest :: Decode LookupEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLookupEventsRequest :: Encode LookupEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a response to a LookupEvents action.</p>
newtype LookupEventsResponse = LookupEventsResponse 
  { "Events" :: NullOrUndefined.NullOrUndefined (EventsList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeLookupEventsResponse :: Newtype LookupEventsResponse _
derive instance repGenericLookupEventsResponse :: Generic LookupEventsResponse _
instance showLookupEventsResponse :: Show LookupEventsResponse where
  show = genericShow
instance decodeLookupEventsResponse :: Decode LookupEventsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLookupEventsResponse :: Encode LookupEventsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the maximum number of trails is reached.</p>
newtype MaximumNumberOfTrailsExceededException = MaximumNumberOfTrailsExceededException Types.NoArguments
derive instance newtypeMaximumNumberOfTrailsExceededException :: Newtype MaximumNumberOfTrailsExceededException _
derive instance repGenericMaximumNumberOfTrailsExceededException :: Generic MaximumNumberOfTrailsExceededException _
instance showMaximumNumberOfTrailsExceededException :: Show MaximumNumberOfTrailsExceededException where
  show = genericShow
instance decodeMaximumNumberOfTrailsExceededException :: Decode MaximumNumberOfTrailsExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaximumNumberOfTrailsExceededException :: Encode MaximumNumberOfTrailsExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the requested operation is not permitted.</p>
newtype OperationNotPermittedException = OperationNotPermittedException Types.NoArguments
derive instance newtypeOperationNotPermittedException :: Newtype OperationNotPermittedException _
derive instance repGenericOperationNotPermittedException :: Generic OperationNotPermittedException _
instance showOperationNotPermittedException :: Show OperationNotPermittedException where
  show = genericShow
instance decodeOperationNotPermittedException :: Decode OperationNotPermittedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationNotPermittedException :: Encode OperationNotPermittedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a returned public key.</p>
newtype PublicKey = PublicKey 
  { "Value" :: NullOrUndefined.NullOrUndefined (ByteBuffer)
  , "ValidityStartTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "ValidityEndTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "Fingerprint" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePublicKey :: Newtype PublicKey _
derive instance repGenericPublicKey :: Generic PublicKey _
instance showPublicKey :: Show PublicKey where
  show = genericShow
instance decodePublicKey :: Decode PublicKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePublicKey :: Encode PublicKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PublicKeyList = PublicKeyList (Array PublicKey)
derive instance newtypePublicKeyList :: Newtype PublicKeyList _
derive instance repGenericPublicKeyList :: Generic PublicKeyList _
instance showPublicKeyList :: Show PublicKeyList where
  show = genericShow
instance decodePublicKeyList :: Decode PublicKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePublicKeyList :: Encode PublicKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutEventSelectorsRequest = PutEventSelectorsRequest 
  { "TrailName" :: (String)
  , "EventSelectors" :: (EventSelectors)
  }
derive instance newtypePutEventSelectorsRequest :: Newtype PutEventSelectorsRequest _
derive instance repGenericPutEventSelectorsRequest :: Generic PutEventSelectorsRequest _
instance showPutEventSelectorsRequest :: Show PutEventSelectorsRequest where
  show = genericShow
instance decodePutEventSelectorsRequest :: Decode PutEventSelectorsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventSelectorsRequest :: Encode PutEventSelectorsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutEventSelectorsResponse = PutEventSelectorsResponse 
  { "TrailARN" :: NullOrUndefined.NullOrUndefined (String)
  , "EventSelectors" :: NullOrUndefined.NullOrUndefined (EventSelectors)
  }
derive instance newtypePutEventSelectorsResponse :: Newtype PutEventSelectorsResponse _
derive instance repGenericPutEventSelectorsResponse :: Generic PutEventSelectorsResponse _
instance showPutEventSelectorsResponse :: Show PutEventSelectorsResponse where
  show = genericShow
instance decodePutEventSelectorsResponse :: Decode PutEventSelectorsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventSelectorsResponse :: Encode PutEventSelectorsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReadWriteType = ReadWriteType String
derive instance newtypeReadWriteType :: Newtype ReadWriteType _
derive instance repGenericReadWriteType :: Generic ReadWriteType _
instance showReadWriteType :: Show ReadWriteType where
  show = genericShow
instance decodeReadWriteType :: Decode ReadWriteType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReadWriteType :: Encode ReadWriteType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the tags to remove from a trail.</p>
newtype RemoveTagsRequest = RemoveTagsRequest 
  { "ResourceId" :: (String)
  , "TagsList" :: NullOrUndefined.NullOrUndefined (TagsList)
  }
derive instance newtypeRemoveTagsRequest :: Newtype RemoveTagsRequest _
derive instance repGenericRemoveTagsRequest :: Generic RemoveTagsRequest _
instance showRemoveTagsRequest :: Show RemoveTagsRequest where
  show = genericShow
instance decodeRemoveTagsRequest :: Decode RemoveTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsRequest :: Encode RemoveTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype RemoveTagsResponse = RemoveTagsResponse Types.NoArguments
derive instance newtypeRemoveTagsResponse :: Newtype RemoveTagsResponse _
derive instance repGenericRemoveTagsResponse :: Generic RemoveTagsResponse _
instance showRemoveTagsResponse :: Show RemoveTagsResponse where
  show = genericShow
instance decodeRemoveTagsResponse :: Decode RemoveTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsResponse :: Encode RemoveTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the type and name of a resource referenced by an event.</p>
newtype Resource = Resource 
  { "ResourceType" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResource :: Newtype Resource _
derive instance repGenericResource :: Generic Resource _
instance showResource :: Show Resource where
  show = genericShow
instance decodeResource :: Decode Resource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResource :: Encode Resource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceIdList = ResourceIdList (Array String)
derive instance newtypeResourceIdList :: Newtype ResourceIdList _
derive instance repGenericResourceIdList :: Generic ResourceIdList _
instance showResourceIdList :: Show ResourceIdList where
  show = genericShow
instance decodeResourceIdList :: Decode ResourceIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceIdList :: Encode ResourceIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of resources referenced by the event returned.</p>
newtype ResourceList = ResourceList (Array Resource)
derive instance newtypeResourceList :: Newtype ResourceList _
derive instance repGenericResourceList :: Generic ResourceList _
instance showResourceList :: Show ResourceList where
  show = genericShow
instance decodeResourceList :: Decode ResourceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceList :: Encode ResourceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the specified resource is not found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException Types.NoArguments
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A resource tag.</p>
newtype ResourceTag = ResourceTag 
  { "ResourceId" :: NullOrUndefined.NullOrUndefined (String)
  , "TagsList" :: NullOrUndefined.NullOrUndefined (TagsList)
  }
derive instance newtypeResourceTag :: Newtype ResourceTag _
derive instance repGenericResourceTag :: Generic ResourceTag _
instance showResourceTag :: Show ResourceTag where
  show = genericShow
instance decodeResourceTag :: Decode ResourceTag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTag :: Encode ResourceTag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceTagList = ResourceTagList (Array ResourceTag)
derive instance newtypeResourceTagList :: Newtype ResourceTagList _
derive instance repGenericResourceTagList :: Generic ResourceTagList _
instance showResourceTagList :: Show ResourceTagList where
  show = genericShow
instance decodeResourceTagList :: Decode ResourceTagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTagList :: Encode ResourceTagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the specified resource type is not supported by CloudTrail.</p>
newtype ResourceTypeNotSupportedException = ResourceTypeNotSupportedException Types.NoArguments
derive instance newtypeResourceTypeNotSupportedException :: Newtype ResourceTypeNotSupportedException _
derive instance repGenericResourceTypeNotSupportedException :: Generic ResourceTypeNotSupportedException _
instance showResourceTypeNotSupportedException :: Show ResourceTypeNotSupportedException where
  show = genericShow
instance decodeResourceTypeNotSupportedException :: Decode ResourceTypeNotSupportedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTypeNotSupportedException :: Encode ResourceTypeNotSupportedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the specified S3 bucket does not exist.</p>
newtype S3BucketDoesNotExistException = S3BucketDoesNotExistException Types.NoArguments
derive instance newtypeS3BucketDoesNotExistException :: Newtype S3BucketDoesNotExistException _
derive instance repGenericS3BucketDoesNotExistException :: Generic S3BucketDoesNotExistException _
instance showS3BucketDoesNotExistException :: Show S3BucketDoesNotExistException where
  show = genericShow
instance decodeS3BucketDoesNotExistException :: Decode S3BucketDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3BucketDoesNotExistException :: Encode S3BucketDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request to CloudTrail to start logging AWS API calls for an account.</p>
newtype StartLoggingRequest = StartLoggingRequest 
  { "Name" :: (String)
  }
derive instance newtypeStartLoggingRequest :: Newtype StartLoggingRequest _
derive instance repGenericStartLoggingRequest :: Generic StartLoggingRequest _
instance showStartLoggingRequest :: Show StartLoggingRequest where
  show = genericShow
instance decodeStartLoggingRequest :: Decode StartLoggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartLoggingRequest :: Encode StartLoggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype StartLoggingResponse = StartLoggingResponse Types.NoArguments
derive instance newtypeStartLoggingResponse :: Newtype StartLoggingResponse _
derive instance repGenericStartLoggingResponse :: Generic StartLoggingResponse _
instance showStartLoggingResponse :: Show StartLoggingResponse where
  show = genericShow
instance decodeStartLoggingResponse :: Decode StartLoggingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartLoggingResponse :: Encode StartLoggingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Passes the request to CloudTrail to stop logging AWS API calls for the specified account.</p>
newtype StopLoggingRequest = StopLoggingRequest 
  { "Name" :: (String)
  }
derive instance newtypeStopLoggingRequest :: Newtype StopLoggingRequest _
derive instance repGenericStopLoggingRequest :: Generic StopLoggingRequest _
instance showStopLoggingRequest :: Show StopLoggingRequest where
  show = genericShow
instance decodeStopLoggingRequest :: Decode StopLoggingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopLoggingRequest :: Encode StopLoggingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype StopLoggingResponse = StopLoggingResponse Types.NoArguments
derive instance newtypeStopLoggingResponse :: Newtype StopLoggingResponse _
derive instance repGenericStopLoggingResponse :: Generic StopLoggingResponse _
instance showStopLoggingResponse :: Show StopLoggingResponse where
  show = genericShow
instance decodeStopLoggingResponse :: Decode StopLoggingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopLoggingResponse :: Encode StopLoggingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A custom key-value pair associated with a resource such as a CloudTrail trail.</p>
newtype Tag = Tag 
  { "Key" :: (String)
  , "Value" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of tags per trail has exceeded the permitted amount. Currently, the limit is 50.</p>
newtype TagsLimitExceededException = TagsLimitExceededException Types.NoArguments
derive instance newtypeTagsLimitExceededException :: Newtype TagsLimitExceededException _
derive instance repGenericTagsLimitExceededException :: Generic TagsLimitExceededException _
instance showTagsLimitExceededException :: Show TagsLimitExceededException where
  show = genericShow
instance decodeTagsLimitExceededException :: Decode TagsLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagsLimitExceededException :: Encode TagsLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of tags.</p>
newtype TagsList = TagsList (Array Tag)
derive instance newtypeTagsList :: Newtype TagsList _
derive instance repGenericTagsList :: Generic TagsList _
instance showTagsList :: Show TagsList where
  show = genericShow
instance decodeTagsList :: Decode TagsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagsList :: Encode TagsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The settings for a trail.</p>
newtype Trail = Trail 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "S3BucketName" :: NullOrUndefined.NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicARN" :: NullOrUndefined.NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HomeRegion" :: NullOrUndefined.NullOrUndefined (String)
  , "TrailARN" :: NullOrUndefined.NullOrUndefined (String)
  , "LogFileValidationEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined.NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  , "HasCustomEventSelectors" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeTrail :: Newtype Trail _
derive instance repGenericTrail :: Generic Trail _
instance showTrail :: Show Trail where
  show = genericShow
instance decodeTrail :: Decode Trail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrail :: Encode Trail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the specified trail already exists.</p>
newtype TrailAlreadyExistsException = TrailAlreadyExistsException Types.NoArguments
derive instance newtypeTrailAlreadyExistsException :: Newtype TrailAlreadyExistsException _
derive instance repGenericTrailAlreadyExistsException :: Generic TrailAlreadyExistsException _
instance showTrailAlreadyExistsException :: Show TrailAlreadyExistsException where
  show = genericShow
instance decodeTrailAlreadyExistsException :: Decode TrailAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrailAlreadyExistsException :: Encode TrailAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrailList = TrailList (Array Trail)
derive instance newtypeTrailList :: Newtype TrailList _
derive instance repGenericTrailList :: Generic TrailList _
instance showTrailList :: Show TrailList where
  show = genericShow
instance decodeTrailList :: Decode TrailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrailList :: Encode TrailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrailNameList = TrailNameList (Array String)
derive instance newtypeTrailNameList :: Newtype TrailNameList _
derive instance repGenericTrailNameList :: Generic TrailNameList _
instance showTrailNameList :: Show TrailNameList where
  show = genericShow
instance decodeTrailNameList :: Decode TrailNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrailNameList :: Encode TrailNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the trail with the given name is not found.</p>
newtype TrailNotFoundException = TrailNotFoundException Types.NoArguments
derive instance newtypeTrailNotFoundException :: Newtype TrailNotFoundException _
derive instance repGenericTrailNotFoundException :: Generic TrailNotFoundException _
instance showTrailNotFoundException :: Show TrailNotFoundException where
  show = genericShow
instance decodeTrailNotFoundException :: Decode TrailNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrailNotFoundException :: Encode TrailNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is deprecated.</p>
newtype TrailNotProvidedException = TrailNotProvidedException Types.NoArguments
derive instance newtypeTrailNotProvidedException :: Newtype TrailNotProvidedException _
derive instance repGenericTrailNotProvidedException :: Generic TrailNotProvidedException _
instance showTrailNotProvidedException :: Show TrailNotProvidedException where
  show = genericShow
instance decodeTrailNotProvidedException :: Decode TrailNotProvidedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrailNotProvidedException :: Encode TrailNotProvidedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the requested operation is not supported.</p>
newtype UnsupportedOperationException = UnsupportedOperationException Types.NoArguments
derive instance newtypeUnsupportedOperationException :: Newtype UnsupportedOperationException _
derive instance repGenericUnsupportedOperationException :: Generic UnsupportedOperationException _
instance showUnsupportedOperationException :: Show UnsupportedOperationException where
  show = genericShow
instance decodeUnsupportedOperationException :: Decode UnsupportedOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedOperationException :: Encode UnsupportedOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies settings to update for the trail.</p>
newtype UpdateTrailRequest = UpdateTrailRequest 
  { "Name" :: (String)
  , "S3BucketName" :: NullOrUndefined.NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined.NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "EnableLogFileValidation" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined.NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateTrailRequest :: Newtype UpdateTrailRequest _
derive instance repGenericUpdateTrailRequest :: Generic UpdateTrailRequest _
instance showUpdateTrailRequest :: Show UpdateTrailRequest where
  show = genericShow
instance decodeUpdateTrailRequest :: Decode UpdateTrailRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTrailRequest :: Encode UpdateTrailRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype UpdateTrailResponse = UpdateTrailResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "S3BucketName" :: NullOrUndefined.NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicARN" :: NullOrUndefined.NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "TrailARN" :: NullOrUndefined.NullOrUndefined (String)
  , "LogFileValidationEnabled" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined.NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateTrailResponse :: Newtype UpdateTrailResponse _
derive instance repGenericUpdateTrailResponse :: Generic UpdateTrailResponse _
instance showUpdateTrailResponse :: Show UpdateTrailResponse where
  show = genericShow
instance decodeUpdateTrailResponse :: Decode UpdateTrailResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTrailResponse :: Encode UpdateTrailResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
