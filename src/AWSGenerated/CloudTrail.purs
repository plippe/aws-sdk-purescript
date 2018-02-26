

-- | <fullname>AWS CloudTrail</fullname> <p>This is the CloudTrail API Reference. It provides descriptions of actions, data types, common parameters, and common errors for CloudTrail.</p> <p>CloudTrail is a web service that records AWS API calls for your AWS account and delivers log files to an Amazon S3 bucket. The recorded information includes the identity of the user, the start time of the AWS API call, the source IP address, the request parameters, and the response elements returned by the service.</p> <note> <p>As an alternative to the API, you can use one of the AWS SDKs, which consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .NET, iOS, Android, etc.). The SDKs provide a convenient way to create programmatic access to AWSCloudTrail. For example, the SDKs take care of cryptographically signing requests, managing errors, and retrying requests automatically. For information about the AWS SDKs, including how to download and install them, see the <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services page</a>.</p> </note> <p>See the <a href="http://docs.aws.amazon.com/awscloudtrail/latest/userguide/cloudtrail-user-guide.html">AWS CloudTrail User Guide</a> for information about the data that is included with each AWS API call listed in the log files.</p>
module AWS.CloudTrail where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudTrail" :: String


-- | <p>Adds one or more tags to a trail, up to a limit of 50. Tags must be unique per trail. Overwrites an existing tag's value when a new value is specified for an existing tag key. If you specify a key without a value, the tag will be created with the specified key and a value of null. You can tag a trail that applies to all regions only from the region in which the trail was created (that is, from its home region).</p>
addTags :: forall eff. AddTagsRequest -> Aff (err :: AWS.RequestError | eff) AddTagsResponse
addTags = AWS.request serviceName "AddTags" 


-- | <p>Creates a trail that specifies the settings for delivery of log data to an Amazon S3 bucket. A maximum of five trails can exist in a region, irrespective of the region in which they were created.</p>
createTrail :: forall eff. CreateTrailRequest -> Aff (err :: AWS.RequestError | eff) CreateTrailResponse
createTrail = AWS.request serviceName "CreateTrail" 


-- | <p>Deletes a trail. This operation must be called from the region in which the trail was created. <code>DeleteTrail</code> cannot be called on the shadow trails (replicated trails in other regions) of a trail that is enabled in all regions.</p>
deleteTrail :: forall eff. DeleteTrailRequest -> Aff (err :: AWS.RequestError | eff) DeleteTrailResponse
deleteTrail = AWS.request serviceName "DeleteTrail" 


-- | <p>Retrieves settings for the trail associated with the current region for your account.</p>
describeTrails :: forall eff. DescribeTrailsRequest -> Aff (err :: AWS.RequestError | eff) DescribeTrailsResponse
describeTrails = AWS.request serviceName "DescribeTrails" 


-- | <p>Describes the settings for the event selectors that you configured for your trail. The information returned for your event selectors includes the following:</p> <ul> <li> <p>The S3 objects that you are logging for data events.</p> </li> <li> <p>If your event selector includes management events.</p> </li> <li> <p>If your event selector includes read-only events, write-only events, or all. </p> </li> </ul> <p>For more information, see <a href="http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html">Logging Data and Management Events for Trails </a> in the <i>AWS CloudTrail User Guide</i>.</p>
getEventSelectors :: forall eff. GetEventSelectorsRequest -> Aff (err :: AWS.RequestError | eff) GetEventSelectorsResponse
getEventSelectors = AWS.request serviceName "GetEventSelectors" 


-- | <p>Returns a JSON-formatted list of information about the specified trail. Fields include information on delivery errors, Amazon SNS and Amazon S3 errors, and start and stop logging times for each trail. This operation returns trail status from a single region. To return trail status from all regions, you must call the operation on each region.</p>
getTrailStatus :: forall eff. GetTrailStatusRequest -> Aff (err :: AWS.RequestError | eff) GetTrailStatusResponse
getTrailStatus = AWS.request serviceName "GetTrailStatus" 


-- | <p>Returns all public keys whose private keys were used to sign the digest files within the specified time range. The public key is needed to validate digest files that were signed with its corresponding private key.</p> <note> <p>CloudTrail uses different private/public key pairs per region. Each digest file is signed with a private key unique to its region. Therefore, when you validate a digest file from a particular region, you must look in the same region for its corresponding public key.</p> </note>
listPublicKeys :: forall eff. ListPublicKeysRequest -> Aff (err :: AWS.RequestError | eff) ListPublicKeysResponse
listPublicKeys = AWS.request serviceName "ListPublicKeys" 


-- | <p>Lists the tags for the trail in the current region.</p>
listTags :: forall eff. ListTagsRequest -> Aff (err :: AWS.RequestError | eff) ListTagsResponse
listTags = AWS.request serviceName "ListTags" 


-- | <p>Looks up API activity events captured by CloudTrail that create, update, or delete resources in your account. Events for a region can be looked up for the times in which you had CloudTrail turned on in that region during the last seven days. Lookup supports the following attributes:</p> <ul> <li> <p>Event ID</p> </li> <li> <p>Event name</p> </li> <li> <p>Event source</p> </li> <li> <p>Resource name</p> </li> <li> <p>Resource type</p> </li> <li> <p>User name</p> </li> </ul> <p>All attributes are optional. The default number of results returned is 10, with a maximum of 50 possible. The response includes a token that you can use to get the next page of results.</p> <important> <p>The rate of lookup requests is limited to one per second per account. If this limit is exceeded, a throttling error occurs.</p> </important> <important> <p>Events that occurred during the selected time range will not be available for lookup if CloudTrail logging was not enabled when the events occurred.</p> </important>
lookupEvents :: forall eff. LookupEventsRequest -> Aff (err :: AWS.RequestError | eff) LookupEventsResponse
lookupEvents = AWS.request serviceName "LookupEvents" 


-- | <p>Configures an event selector for your trail. Use event selectors to specify whether you want your trail to log management and/or data events. When an event occurs in your account, CloudTrail evaluates the event selectors in all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event. </p> <p>Example</p> <ol> <li> <p>You create an event selector for a trail and specify that you want write-only events.</p> </li> <li> <p>The EC2 <code>GetConsoleOutput</code> and <code>RunInstances</code> API operations occur in your account.</p> </li> <li> <p>CloudTrail evaluates whether the events match your event selectors.</p> </li> <li> <p>The <code>RunInstances</code> is a write-only event and it matches your event selector. The trail logs the event.</p> </li> <li> <p>The <code>GetConsoleOutput</code> is a read-only event but it doesn't match your event selector. The trail doesn't log the event. </p> </li> </ol> <p>The <code>PutEventSelectors</code> operation must be called from the region in which the trail was created; otherwise, an <code>InvalidHomeRegionException</code> is thrown.</p> <p>You can configure up to five event selectors for each trail. For more information, see <a href="http://docs.aws.amazon.com/awscloudtrail/latest/userguide/logging-management-and-data-events-with-cloudtrail.html">Logging Data and Management Events for Trails </a> in the <i>AWS CloudTrail User Guide</i>.</p>
putEventSelectors :: forall eff. PutEventSelectorsRequest -> Aff (err :: AWS.RequestError | eff) PutEventSelectorsResponse
putEventSelectors = AWS.request serviceName "PutEventSelectors" 


-- | <p>Removes the specified tags from a trail.</p>
removeTags :: forall eff. RemoveTagsRequest -> Aff (err :: AWS.RequestError | eff) RemoveTagsResponse
removeTags = AWS.request serviceName "RemoveTags" 


-- | <p>Starts the recording of AWS API calls and log file delivery for a trail. For a trail that is enabled in all regions, this operation must be called from the region in which the trail was created. This operation cannot be called on the shadow trails (replicated trails in other regions) of a trail that is enabled in all regions.</p>
startLogging :: forall eff. StartLoggingRequest -> Aff (err :: AWS.RequestError | eff) StartLoggingResponse
startLogging = AWS.request serviceName "StartLogging" 


-- | <p>Suspends the recording of AWS API calls and log file delivery for the specified trail. Under most circumstances, there is no need to use this action. You can update a trail without stopping it first. This action is the only way to stop recording. For a trail enabled in all regions, this operation must be called from the region in which the trail was created, or an <code>InvalidHomeRegionException</code> will occur. This operation cannot be called on the shadow trails (replicated trails in other regions) of a trail enabled in all regions.</p>
stopLogging :: forall eff. StopLoggingRequest -> Aff (err :: AWS.RequestError | eff) StopLoggingResponse
stopLogging = AWS.request serviceName "StopLogging" 


-- | <p>Updates the settings that specify delivery of log files. Changes to a trail do not require stopping the CloudTrail service. Use this action to designate an existing bucket for log delivery. If the existing bucket has previously been a target for CloudTrail log files, an IAM policy exists for the bucket. <code>UpdateTrail</code> must be called from the region in which the trail was created; otherwise, an <code>InvalidHomeRegionException</code> is thrown.</p>
updateTrail :: forall eff. UpdateTrailRequest -> Aff (err :: AWS.RequestError | eff) UpdateTrailResponse
updateTrail = AWS.request serviceName "UpdateTrail" 


-- | <p>Specifies the tags to add to a trail.</p>
newtype AddTagsRequest = AddTagsRequest 
  { "ResourceId" :: (String)
  , "TagsList" :: NullOrUndefined (TagsList)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype AddTagsResponse = AddTagsResponse 
  { 
  }


newtype ByteBuffer = ByteBuffer String


-- | <p>This exception is thrown when an operation is called with an invalid trail ARN. The format of a trail ARN is:</p> <p> <code>arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail</code> </p>
newtype CloudTrailARNInvalidException = CloudTrailARNInvalidException 
  { 
  }


-- | <p>Cannot set a CloudWatch Logs delivery for this region.</p>
newtype CloudWatchLogsDeliveryUnavailableException = CloudWatchLogsDeliveryUnavailableException 
  { 
  }


-- | <p>Specifies the settings for each trail.</p>
newtype CreateTrailRequest = CreateTrailRequest 
  { "Name" :: (String)
  , "S3BucketName" :: (String)
  , "S3KeyPrefix" :: NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined (Boolean)
  , "EnableLogFileValidation" :: NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined (String)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype CreateTrailResponse = CreateTrailResponse 
  { "Name" :: NullOrUndefined (String)
  , "S3BucketName" :: NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined (String)
  , "SnsTopicARN" :: NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined (Boolean)
  , "TrailARN" :: NullOrUndefined (String)
  , "LogFileValidationEnabled" :: NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined (String)
  }


-- | <p>The Amazon S3 objects that you specify in your event selectors for your trail to log data events. Data events are object-level API operations that access S3 objects, such as <code>GetObject</code>, <code>DeleteObject</code>, and <code>PutObject</code>. You can specify up to 250 S3 buckets and object prefixes for a trail. </p> <p>Example</p> <ol> <li> <p>You create an event selector for a trail and specify an S3 bucket and an empty prefix, such as <code>arn:aws:s3:::bucket-1/</code>.</p> </li> <li> <p>You upload an image file to <code>bucket-1</code>.</p> </li> <li> <p>The <code>PutObject</code> API operation occurs on an object in the S3 bucket that you specified in the event selector. The trail processes and logs the event.</p> </li> <li> <p>You upload another image file to a different S3 bucket named <code>arn:aws:s3:::bucket-2</code>.</p> </li> <li> <p>The event occurs on an object in an S3 bucket that you didn't specify in the event selector. The trail doesn’t log the event.</p> </li> </ol>
newtype DataResource = DataResource 
  { "Type" :: NullOrUndefined (String)
  , "Values" :: NullOrUndefined (DataResourceValues)
  }


newtype DataResourceValues = DataResourceValues (Array String)


newtype DataResources = DataResources (Array DataResource)


newtype Date = Date Number


-- | <p>The request that specifies the name of a trail to delete.</p>
newtype DeleteTrailRequest = DeleteTrailRequest 
  { "Name" :: (String)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype DeleteTrailResponse = DeleteTrailResponse 
  { 
  }


-- | <p>Returns information about the trail.</p>
newtype DescribeTrailsRequest = DescribeTrailsRequest 
  { "TrailNameList'" :: NullOrUndefined (TrailNameList)
  , "IncludeShadowTrails'" :: NullOrUndefined (Boolean)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype DescribeTrailsResponse = DescribeTrailsResponse 
  { "TrailList'" :: NullOrUndefined (TrailList)
  }


-- | <p>Contains information about an event that was returned by a lookup request. The result includes a representation of a CloudTrail event.</p>
newtype Event = Event 
  { "EventId" :: NullOrUndefined (String)
  , "EventName" :: NullOrUndefined (String)
  , "EventTime" :: NullOrUndefined (Date)
  , "EventSource" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  , "Resources" :: NullOrUndefined (ResourceList)
  , "CloudTrailEvent" :: NullOrUndefined (String)
  }


-- | <p>Use event selectors to specify whether you want your trail to log management and/or data events. When an event occurs in your account, CloudTrail evaluates the event selector for all trails. For each trail, if the event matches any event selector, the trail processes and logs the event. If the event doesn't match any event selector, the trail doesn't log the event.</p> <p>You can configure up to five event selectors for a trail.</p>
newtype EventSelector = EventSelector 
  { "ReadWriteType" :: NullOrUndefined (ReadWriteType)
  , "IncludeManagementEvents" :: NullOrUndefined (Boolean)
  , "DataResources" :: NullOrUndefined (DataResources)
  }


newtype EventSelectors = EventSelectors (Array EventSelector)


newtype EventsList = EventsList (Array Event)


newtype GetEventSelectorsRequest = GetEventSelectorsRequest 
  { "TrailName" :: (String)
  }


newtype GetEventSelectorsResponse = GetEventSelectorsResponse 
  { "TrailARN" :: NullOrUndefined (String)
  , "EventSelectors" :: NullOrUndefined (EventSelectors)
  }


-- | <p>The name of a trail about which you want the current status.</p>
newtype GetTrailStatusRequest = GetTrailStatusRequest 
  { "Name" :: (String)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype GetTrailStatusResponse = GetTrailStatusResponse 
  { "IsLogging" :: NullOrUndefined (Boolean)
  , "LatestDeliveryError" :: NullOrUndefined (String)
  , "LatestNotificationError" :: NullOrUndefined (String)
  , "LatestDeliveryTime" :: NullOrUndefined (Date)
  , "LatestNotificationTime" :: NullOrUndefined (Date)
  , "StartLoggingTime" :: NullOrUndefined (Date)
  , "StopLoggingTime" :: NullOrUndefined (Date)
  , "LatestCloudWatchLogsDeliveryError" :: NullOrUndefined (String)
  , "LatestCloudWatchLogsDeliveryTime" :: NullOrUndefined (Date)
  , "LatestDigestDeliveryTime" :: NullOrUndefined (Date)
  , "LatestDigestDeliveryError" :: NullOrUndefined (String)
  , "LatestDeliveryAttemptTime" :: NullOrUndefined (String)
  , "LatestNotificationAttemptTime" :: NullOrUndefined (String)
  , "LatestNotificationAttemptSucceeded" :: NullOrUndefined (String)
  , "LatestDeliveryAttemptSucceeded" :: NullOrUndefined (String)
  , "TimeLoggingStarted" :: NullOrUndefined (String)
  , "TimeLoggingStopped" :: NullOrUndefined (String)
  }


-- | <p>This exception is thrown when the policy on the S3 bucket or KMS key is not sufficient.</p>
newtype InsufficientEncryptionPolicyException = InsufficientEncryptionPolicyException 
  { 
  }


-- | <p>This exception is thrown when the policy on the S3 bucket is not sufficient.</p>
newtype InsufficientS3BucketPolicyException = InsufficientS3BucketPolicyException 
  { 
  }


-- | <p>This exception is thrown when the policy on the SNS topic is not sufficient.</p>
newtype InsufficientSnsTopicPolicyException = InsufficientSnsTopicPolicyException 
  { 
  }


-- | <p>This exception is thrown when the provided CloudWatch log group is not valid.</p>
newtype InvalidCloudWatchLogsLogGroupArnException = InvalidCloudWatchLogsLogGroupArnException 
  { 
  }


-- | <p>This exception is thrown when the provided role is not valid.</p>
newtype InvalidCloudWatchLogsRoleArnException = InvalidCloudWatchLogsRoleArnException 
  { 
  }


-- | <p>This exception is thrown when the <code>PutEventSelectors</code> operation is called with an invalid number of event selectors, data resources, or an invalid value for a parameter:</p> <ul> <li> <p>Specify a valid number of event selectors (1 to 5) for a trail.</p> </li> <li> <p>Specify a valid number of data resources (1 to 250) for an event selector.</p> </li> <li> <p>Specify a valid value for a parameter. For example, specifying the <code>ReadWriteType</code> parameter with a value of <code>read-only</code> is invalid.</p> </li> </ul>
newtype InvalidEventSelectorsException = InvalidEventSelectorsException 
  { 
  }


-- | <p>This exception is thrown when an operation is called on a trail from a region other than the region in which the trail was created.</p>
newtype InvalidHomeRegionException = InvalidHomeRegionException 
  { 
  }


-- | <p>This exception is thrown when the KMS key ARN is invalid.</p>
newtype InvalidKmsKeyIdException = InvalidKmsKeyIdException 
  { 
  }


-- | <p>Occurs when an invalid lookup attribute is specified.</p>
newtype InvalidLookupAttributesException = InvalidLookupAttributesException 
  { 
  }


-- | <p>This exception is thrown if the limit specified is invalid.</p>
newtype InvalidMaxResultsException = InvalidMaxResultsException 
  { 
  }


-- | <p>Invalid token or token that was previously used in a request with different parameters. This exception is thrown if the token is invalid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { 
  }


-- | <p>This exception is thrown when the combination of parameters provided is not valid.</p>
newtype InvalidParameterCombinationException = InvalidParameterCombinationException 
  { 
  }


-- | <p>This exception is thrown when the provided S3 bucket name is not valid.</p>
newtype InvalidS3BucketNameException = InvalidS3BucketNameException 
  { 
  }


-- | <p>This exception is thrown when the provided S3 prefix is not valid.</p>
newtype InvalidS3PrefixException = InvalidS3PrefixException 
  { 
  }


-- | <p>This exception is thrown when the provided SNS topic name is not valid.</p>
newtype InvalidSnsTopicNameException = InvalidSnsTopicNameException 
  { 
  }


-- | <p>This exception is thrown when the key or value specified for the tag does not match the regular expression <code>^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-@]*)$</code>.</p>
newtype InvalidTagParameterException = InvalidTagParameterException 
  { 
  }


-- | <p>Occurs if the timestamp values are invalid. Either the start time occurs after the end time or the time range is outside the range of possible values.</p>
newtype InvalidTimeRangeException = InvalidTimeRangeException 
  { 
  }


-- | <p>Reserved for future use.</p>
newtype InvalidTokenException = InvalidTokenException 
  { 
  }


-- | <p>This exception is thrown when the provided trail name is not valid. Trail names must meet the following requirements:</p> <ul> <li> <p>Contain only ASCII letters (a-z, A-Z), numbers (0-9), periods (.), underscores (_), or dashes (-)</p> </li> <li> <p>Start with a letter or number, and end with a letter or number</p> </li> <li> <p>Be between 3 and 128 characters</p> </li> <li> <p>Have no adjacent periods, underscores or dashes. Names like <code>my-_namespace</code> and <code>my--namespace</code> are invalid.</p> </li> <li> <p>Not be in IP address format (for example, 192.168.5.4)</p> </li> </ul>
newtype InvalidTrailNameException = InvalidTrailNameException 
  { 
  }


-- | <p>This exception is thrown when there is an issue with the specified KMS key and the trail can’t be updated.</p>
newtype KmsException = KmsException 
  { 
  }


-- | <p>This exception is deprecated.</p>
newtype KmsKeyDisabledException = KmsKeyDisabledException 
  { 
  }


-- | <p>This exception is thrown when the KMS key does not exist, or when the S3 bucket and the KMS key are not in the same region.</p>
newtype KmsKeyNotFoundException = KmsKeyNotFoundException 
  { 
  }


-- | <p>Requests the public keys for a specified time range.</p>
newtype ListPublicKeysRequest = ListPublicKeysRequest 
  { "StartTime" :: NullOrUndefined (Date)
  , "EndTime" :: NullOrUndefined (Date)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype ListPublicKeysResponse = ListPublicKeysResponse 
  { "PublicKeyList" :: NullOrUndefined (PublicKeyList)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | <p>Specifies a list of trail tags to return.</p>
newtype ListTagsRequest = ListTagsRequest 
  { "ResourceIdList" :: (ResourceIdList)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype ListTagsResponse = ListTagsResponse 
  { "ResourceTagList" :: NullOrUndefined (ResourceTagList)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | <p>Specifies an attribute and value that filter the events returned.</p>
newtype LookupAttribute = LookupAttribute 
  { "AttributeKey" :: (LookupAttributeKey)
  , "AttributeValue" :: (String)
  }


newtype LookupAttributeKey = LookupAttributeKey String


newtype LookupAttributesList = LookupAttributesList (Array LookupAttribute)


-- | <p>Contains a request for LookupEvents.</p>
newtype LookupEventsRequest = LookupEventsRequest 
  { "LookupAttributes" :: NullOrUndefined (LookupAttributesList)
  , "StartTime" :: NullOrUndefined (Date)
  , "EndTime" :: NullOrUndefined (Date)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Contains a response to a LookupEvents action.</p>
newtype LookupEventsResponse = LookupEventsResponse 
  { "Events" :: NullOrUndefined (EventsList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype MaxResults = MaxResults Int


-- | <p>This exception is thrown when the maximum number of trails is reached.</p>
newtype MaximumNumberOfTrailsExceededException = MaximumNumberOfTrailsExceededException 
  { 
  }


newtype NextToken = NextToken String


-- | <p>This exception is thrown when the requested operation is not permitted.</p>
newtype OperationNotPermittedException = OperationNotPermittedException 
  { 
  }


-- | <p>Contains information about a returned public key.</p>
newtype PublicKey = PublicKey 
  { "Value" :: NullOrUndefined (ByteBuffer)
  , "ValidityStartTime" :: NullOrUndefined (Date)
  , "ValidityEndTime" :: NullOrUndefined (Date)
  , "Fingerprint" :: NullOrUndefined (String)
  }


newtype PublicKeyList = PublicKeyList (Array PublicKey)


newtype PutEventSelectorsRequest = PutEventSelectorsRequest 
  { "TrailName" :: (String)
  , "EventSelectors" :: (EventSelectors)
  }


newtype PutEventSelectorsResponse = PutEventSelectorsResponse 
  { "TrailARN" :: NullOrUndefined (String)
  , "EventSelectors" :: NullOrUndefined (EventSelectors)
  }


newtype ReadWriteType = ReadWriteType String


-- | <p>Specifies the tags to remove from a trail.</p>
newtype RemoveTagsRequest = RemoveTagsRequest 
  { "ResourceId" :: (String)
  , "TagsList" :: NullOrUndefined (TagsList)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype RemoveTagsResponse = RemoveTagsResponse 
  { 
  }


-- | <p>Specifies the type and name of a resource referenced by an event.</p>
newtype Resource = Resource 
  { "ResourceType" :: NullOrUndefined (String)
  , "ResourceName" :: NullOrUndefined (String)
  }


newtype ResourceIdList = ResourceIdList (Array String)


-- | <p>A list of resources referenced by the event returned.</p>
newtype ResourceList = ResourceList (Array Resource)


-- | <p>This exception is thrown when the specified resource is not found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }


-- | <p>A resource tag.</p>
newtype ResourceTag = ResourceTag 
  { "ResourceId" :: NullOrUndefined (String)
  , "TagsList" :: NullOrUndefined (TagsList)
  }


newtype ResourceTagList = ResourceTagList (Array ResourceTag)


-- | <p>This exception is thrown when the specified resource type is not supported by CloudTrail.</p>
newtype ResourceTypeNotSupportedException = ResourceTypeNotSupportedException 
  { 
  }


-- | <p>This exception is thrown when the specified S3 bucket does not exist.</p>
newtype S3BucketDoesNotExistException = S3BucketDoesNotExistException 
  { 
  }


-- | <p>The request to CloudTrail to start logging AWS API calls for an account.</p>
newtype StartLoggingRequest = StartLoggingRequest 
  { "Name" :: (String)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype StartLoggingResponse = StartLoggingResponse 
  { 
  }


-- | <p>Passes the request to CloudTrail to stop logging AWS API calls for the specified account.</p>
newtype StopLoggingRequest = StopLoggingRequest 
  { "Name" :: (String)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype StopLoggingResponse = StopLoggingResponse 
  { 
  }


-- | <p>A custom key-value pair associated with a resource such as a CloudTrail trail.</p>
newtype Tag = Tag 
  { "Key" :: (String)
  , "Value" :: NullOrUndefined (String)
  }


-- | <p>The number of tags per trail has exceeded the permitted amount. Currently, the limit is 50.</p>
newtype TagsLimitExceededException = TagsLimitExceededException 
  { 
  }


-- | <p>A list of tags.</p>
newtype TagsList = TagsList (Array Tag)


-- | <p>The settings for a trail.</p>
newtype Trail = Trail 
  { "Name" :: NullOrUndefined (String)
  , "S3BucketName" :: NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined (String)
  , "SnsTopicARN" :: NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined (Boolean)
  , "HomeRegion" :: NullOrUndefined (String)
  , "TrailARN" :: NullOrUndefined (String)
  , "LogFileValidationEnabled" :: NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined (String)
  , "HasCustomEventSelectors" :: NullOrUndefined (Boolean)
  }


-- | <p>This exception is thrown when the specified trail already exists.</p>
newtype TrailAlreadyExistsException = TrailAlreadyExistsException 
  { 
  }


newtype TrailList = TrailList (Array Trail)


newtype TrailNameList = TrailNameList (Array String)


-- | <p>This exception is thrown when the trail with the given name is not found.</p>
newtype TrailNotFoundException = TrailNotFoundException 
  { 
  }


-- | <p>This exception is deprecated.</p>
newtype TrailNotProvidedException = TrailNotProvidedException 
  { 
  }


-- | <p>This exception is thrown when the requested operation is not supported.</p>
newtype UnsupportedOperationException = UnsupportedOperationException 
  { 
  }


-- | <p>Specifies settings to update for the trail.</p>
newtype UpdateTrailRequest = UpdateTrailRequest 
  { "Name" :: (String)
  , "S3BucketName" :: NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined (Boolean)
  , "EnableLogFileValidation" :: NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined (String)
  }


-- | <p>Returns the objects or data listed below if successful. Otherwise, returns an error.</p>
newtype UpdateTrailResponse = UpdateTrailResponse 
  { "Name" :: NullOrUndefined (String)
  , "S3BucketName" :: NullOrUndefined (String)
  , "S3KeyPrefix" :: NullOrUndefined (String)
  , "SnsTopicName" :: NullOrUndefined (String)
  , "SnsTopicARN" :: NullOrUndefined (String)
  , "IncludeGlobalServiceEvents" :: NullOrUndefined (Boolean)
  , "IsMultiRegionTrail" :: NullOrUndefined (Boolean)
  , "TrailARN" :: NullOrUndefined (String)
  , "LogFileValidationEnabled" :: NullOrUndefined (Boolean)
  , "CloudWatchLogsLogGroupArn" :: NullOrUndefined (String)
  , "CloudWatchLogsRoleArn" :: NullOrUndefined (String)
  , "KmsKeyId" :: NullOrUndefined (String)
  }
