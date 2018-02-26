

-- | <p>Welcome to the <i>Amazon Simple Queue Service API Reference</i>.</p> <p>Amazon Simple Queue Service (Amazon SQS) is a reliable, highly-scalable hosted queue for storing messages as they travel between applications or microservices. Amazon SQS moves data between distributed application components and helps you decouple these components.</p> <note> <p> <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/standard-queues.html">Standard queues</a> are available in all regions. <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html">FIFO queues</a> are available in the US East (N. Virginia), US East (Ohio), US West (Oregon), and EU (Ireland) regions.</p> </note> <p>You can use <a href="http://aws.amazon.com/tools/#sdk">AWS SDKs</a> to access Amazon SQS using your favorite programming language. The SDKs perform tasks such as the following automatically:</p> <ul> <li> <p>Cryptographically sign your service requests</p> </li> <li> <p>Retry requests</p> </li> <li> <p>Handle error responses</p> </li> </ul> <p> <b>Additional Information</b> </p> <ul> <li> <p> <a href="http://aws.amazon.com/sqs/">Amazon SQS Product Page</a> </p> </li> <li> <p> <i>Amazon Simple Queue Service Developer Guide</i> </p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/MakingRequestsArticle.html">Making API Requests</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html">Using Amazon SQS Message Attributes</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html">Using Amazon SQS Dead-Letter Queues</a> </p> </li> </ul> </li> <li> <p> <i>Amazon Web Services General Reference</i> </p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#sqs_region">Regions and Endpoints</a> </p> </li> </ul> </li> </ul>
module AWS.SQS where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "SQS" :: String


-- | <p>Adds a permission to a queue for a specific <a href="http://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P">principal</a>. This allows sharing access to the queue.</p> <p>When you create a queue, you have full control access rights for the queue. Only you, the owner of the queue, can grant or deny permissions to the queue. For more information about these permissions, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html">Shared Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <note> <p> <code>AddPermission</code> writes an Amazon-SQS-generated policy. If you want to write your own policy, use <code> <a>SetQueueAttributes</a> </code> to upload your policy. For more information about writing your own policy, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AccessPolicyLanguage.html">Using The Access Policy Language</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
addPermission :: forall eff. AddPermissionRequest -> Aff (err :: AWS.RequestError | eff) Unit
addPermission = AWS.request serviceName "AddPermission" 


-- | <p>Changes the visibility timeout of a specified message in a queue to a new value. The maximum allowed timeout value is 12 hours. Thus, you can't extend the timeout of a message in an existing queue to more than a total visibility timeout of 12 hours. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html">Visibility Timeout</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>For example, you have a message with a visibility timeout of 5 minutes. After 3 minutes, you call <code>ChangeMessageVisiblity</code> with a timeout of 10 minutes. At that time, the timeout for the message is extended by 10 minutes beyond the time of the <code>ChangeMessageVisibility</code> action. This results in a total visibility timeout of 13 minutes. You can continue to call the <code>ChangeMessageVisibility</code> to extend the visibility timeout to a maximum of 12 hours. If you try to extend the visibility timeout beyond 12 hours, your request is rejected.</p> <p>A message is considered to be <i>in flight</i> after it's received from a queue by a consumer, but not yet deleted from the queue.</p> <p>For standard queues, there can be a maximum of 120,000 inflight messages per queue. If you reach this limit, Amazon SQS returns the <code>OverLimit</code> error message. To avoid reaching the limit, you should delete messages from the queue after they're processed. You can also increase the number of queues you use to process your messages.</p> <p>For FIFO queues, there can be a maximum of 20,000 inflight messages per queue. If you reach this limit, Amazon SQS returns no error messages.</p> <important> <p>If you attempt to set the <code>VisibilityTimeout</code> to a value greater than the maximum time left, Amazon SQS returns an error. Amazon SQS doesn't automatically recalculate and increase the timeout to the maximum remaining time.</p> <p>Unlike with a queue, when you change the visibility timeout for a specific message the timeout value is applied immediately but isn't saved in memory for that message. If you don't delete a message after it is received, the visibility timeout for the message reverts to the original timeout value (not to the value you set using the <code>ChangeMessageVisibility</code> action) the next time the message is received.</p> </important>
changeMessageVisibility :: forall eff. ChangeMessageVisibilityRequest -> Aff (err :: AWS.RequestError | eff) Unit
changeMessageVisibility = AWS.request serviceName "ChangeMessageVisibility" 


-- | <p>Changes the visibility timeout of multiple messages. This is a batch version of <code> <a>ChangeMessageVisibility</a>.</code> The result of the action on each message is reported individually in the response. You can send up to 10 <code> <a>ChangeMessageVisibility</a> </code> requests with each <code>ChangeMessageVisibilityBatch</code> action.</p> <important> <p>Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of <code>200</code>.</p> </important> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
changeMessageVisibilityBatch :: forall eff. ChangeMessageVisibilityBatchRequest -> Aff (err :: AWS.RequestError | eff) ChangeMessageVisibilityBatchResult
changeMessageVisibilityBatch = AWS.request serviceName "ChangeMessageVisibilityBatch" 


-- | <p>Creates a new standard or FIFO queue. You can pass one or more attributes in the request. Keep the following caveats in mind:</p> <ul> <li> <p>If you don't specify the <code>FifoQueue</code> attribute, Amazon SQS creates a standard queue.</p> <note> <p> You can't change the queue type after you create it and you can't convert an existing standard queue into a FIFO queue. You must either create a new FIFO queue for your application or delete your existing standard queue and recreate it as a FIFO queue. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-moving"> Moving From a Standard Queue to a FIFO Queue</a> in the <i>Amazon Simple Queue Service Developer Guide</i>. </p> </note> </li> <li> <p>If you don't provide a value for an attribute, the queue is created with the default value for the attribute.</p> </li> <li> <p>If you delete a queue, you must wait at least 60 seconds before creating a queue with the same name.</p> </li> </ul> <p>To successfully create a new queue, you must provide a queue name that adheres to the <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html">limits related to queues</a> and is unique within the scope of your queues.</p> <p>To get the queue URL, use the <code> <a>GetQueueUrl</a> </code> action. <code> <a>GetQueueUrl</a> </code> requires only the <code>QueueName</code> parameter. be aware of existing queue names:</p> <ul> <li> <p>If you provide the name of an existing queue along with the exact names and values of all the queue's attributes, <code>CreateQueue</code> returns the queue URL for the existing queue.</p> </li> <li> <p>If the queue name, attribute names, or attribute values don't match an existing queue, <code>CreateQueue</code> returns an error.</p> </li> </ul> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
createQueue :: forall eff. CreateQueueRequest -> Aff (err :: AWS.RequestError | eff) CreateQueueResult
createQueue = AWS.request serviceName "CreateQueue" 


-- | <p>Deletes the specified message from the specified queue. You specify the message by using the message's <i>receipt handle</i> and not the <i>MessageId</i> you receive when you send the message. Even if the message is locked by another reader due to the visibility timeout setting, it is still deleted from the queue. If you leave a message in the queue for longer than the queue's configured retention period, Amazon SQS automatically deletes the message. </p> <note> <p> The receipt handle is associated with a specific instance of receiving the message. If you receive a message more than once, the receipt handle you get each time you receive the message is different. If you don't provide the most recently received receipt handle for the message when you use the <code>DeleteMessage</code> action, the request succeeds, but the message might not be deleted.</p> <p>For standard queues, it is possible to receive a message even after you delete it. This might happen on rare occasions if one of the servers storing a copy of the message is unavailable when you send the request to delete the message. The copy remains on the server and might be returned to you on a subsequent receive request. You should ensure that your application is idempotent, so that receiving a message more than once does not cause issues.</p> </note>
deleteMessage :: forall eff. DeleteMessageRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteMessage = AWS.request serviceName "DeleteMessage" 


-- | <p>Deletes up to ten messages from the specified queue. This is a batch version of <code> <a>DeleteMessage</a>.</code> The result of the action on each message is reported individually in the response.</p> <important> <p>Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of <code>200</code>.</p> </important> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
deleteMessageBatch :: forall eff. DeleteMessageBatchRequest -> Aff (err :: AWS.RequestError | eff) DeleteMessageBatchResult
deleteMessageBatch = AWS.request serviceName "DeleteMessageBatch" 


-- | <p>Deletes the queue specified by the <code>QueueUrl</code>, regardless of the queue's contents. If the specified queue doesn't exist, Amazon SQS returns a successful response.</p> <important> <p>Be careful with the <code>DeleteQueue</code> action: When you delete a queue, any messages in the queue are no longer available. </p> </important> <p>When you delete a queue, the deletion process takes up to 60 seconds. Requests you send involving that queue during the 60 seconds might succeed. For example, a <code> <a>SendMessage</a> </code> request might succeed, but after 60 seconds the queue and the message you sent no longer exist.</p> <p>When you delete a queue, you must wait at least 60 seconds before creating a queue with the same name. </p>
deleteQueue :: forall eff. DeleteQueueRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteQueue = AWS.request serviceName "DeleteQueue" 


-- | <p>Gets attributes for the specified queue.</p> <note> <p>To determine whether a queue is <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html">FIFO</a>, you can check whether <code>QueueName</code> ends with the <code>.fifo</code> suffix.</p> </note> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
getQueueAttributes :: forall eff. GetQueueAttributesRequest -> Aff (err :: AWS.RequestError | eff) GetQueueAttributesResult
getQueueAttributes = AWS.request serviceName "GetQueueAttributes" 


-- | <p>Returns the URL of an existing queue. This action provides a simple way to retrieve the URL of an Amazon SQS queue.</p> <p>To access a queue that belongs to another AWS account, use the <code>QueueOwnerAWSAccountId</code> parameter to specify the account ID of the queue's owner. The queue's owner must grant you permission to access the queue. For more information about shared queue access, see <code> <a>AddPermission</a> </code> or see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html">Shared Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>. </p>
getQueueUrl :: forall eff. GetQueueUrlRequest -> Aff (err :: AWS.RequestError | eff) GetQueueUrlResult
getQueueUrl = AWS.request serviceName "GetQueueUrl" 


-- | <p>Returns a list of your queues that have the <code>RedrivePolicy</code> queue attribute configured with a dead-letter queue.</p> <p>For more information about using dead-letter queues, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html">Using Amazon SQS Dead-Letter Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
listDeadLetterSourceQueues :: forall eff. ListDeadLetterSourceQueuesRequest -> Aff (err :: AWS.RequestError | eff) ListDeadLetterSourceQueuesResult
listDeadLetterSourceQueues = AWS.request serviceName "ListDeadLetterSourceQueues" 


-- | <p>List all cost allocation tags added to the specified Amazon SQS queue. For an overview, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-tagging-queues.html">Tagging Amazon SQS Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>When you use queue tags, keep the following guidelines in mind:</p> <ul> <li> <p>Adding more than 50 tags to a queue isn't recommended.</p> </li> <li> <p>Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.</p> </li> <li> <p>Tags are case-sensitive.</p> </li> <li> <p>A new tag with a key identical to that of an existing tag overwrites the existing tag.</p> </li> <li> <p>Tagging API actions are limited to 5 TPS per AWS account. If your application requires a higher throughput, file a <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=technical">technical support request</a>.</p> </li> </ul> <p>For a full list of tag restrictions, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html">Limits Related to Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
listQueueTags :: forall eff. ListQueueTagsRequest -> Aff (err :: AWS.RequestError | eff) ListQueueTagsResult
listQueueTags = AWS.request serviceName "ListQueueTags" 


-- | <p>Returns a list of your queues. The maximum number of queues that can be returned is 1,000. If you specify a value for the optional <code>QueueNamePrefix</code> parameter, only queues with a name that begins with the specified value are returned.</p>
listQueues :: forall eff. ListQueuesRequest -> Aff (err :: AWS.RequestError | eff) ListQueuesResult
listQueues = AWS.request serviceName "ListQueues" 


-- | <p>Deletes the messages in a queue specified by the <code>QueueURL</code> parameter.</p> <important> <p>When you use the <code>PurgeQueue</code> action, you can't retrieve a message deleted from a queue.</p> </important> <p>When you purge a queue, the message deletion process takes up to 60 seconds. All messages sent to the queue before calling the <code>PurgeQueue</code> action are deleted. Messages sent to the queue while it is being purged might be deleted. While the queue is being purged, messages sent to the queue before <code>PurgeQueue</code> is called might be received, but are deleted within the next minute.</p>
purgeQueue :: forall eff. PurgeQueueRequest -> Aff (err :: AWS.RequestError | eff) Unit
purgeQueue = AWS.request serviceName "PurgeQueue" 


-- | <p>Retrieves one or more messages (up to 10), from the specified queue. Using the <code>WaitTimeSeconds</code> parameter enables long-poll support. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-long-polling.html">Amazon SQS Long Polling</a> in the <i>Amazon Simple Queue Service Developer Guide</i>. </p> <p>Short poll is the default behavior where a weighted random set of machines is sampled on a <code>ReceiveMessage</code> call. Thus, only the messages on the sampled machines are returned. If the number of messages in the queue is small (fewer than 1,000), you most likely get fewer messages than you requested per <code>ReceiveMessage</code> call. If the number of messages in the queue is extremely small, you might not receive any messages in a particular <code>ReceiveMessage</code> response. If this happens, repeat the request. </p> <p>For each message returned, the response includes the following:</p> <ul> <li> <p>The message body.</p> </li> <li> <p>An MD5 digest of the message body. For information about MD5, see <a href="https://www.ietf.org/rfc/rfc1321.txt">RFC1321</a>.</p> </li> <li> <p>The <code>MessageId</code> you received when you sent the message to the queue.</p> </li> <li> <p>The receipt handle.</p> </li> <li> <p>The message attributes.</p> </li> <li> <p>An MD5 digest of the message attributes.</p> </li> </ul> <p>The receipt handle is the identifier you must provide when deleting the message. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html">Queue and Message Identifiers</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>You can provide the <code>VisibilityTimeout</code> parameter in your request. The parameter is applied to the messages that Amazon SQS returns in the response. If you don't include the parameter, the overall visibility timeout for the queue is used for the returned messages. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html">Visibility Timeout</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>A message that isn't deleted or a message whose visibility isn't extended before the visibility timeout expires counts as a failed receive. Depending on the configuration of the queue, the message might be sent to the dead-letter queue.</p> <note> <p>In the future, new attributes might be added. If you write code that calls this action, we recommend that you structure your code so that it can handle new attributes gracefully.</p> </note>
receiveMessage :: forall eff. ReceiveMessageRequest -> Aff (err :: AWS.RequestError | eff) ReceiveMessageResult
receiveMessage = AWS.request serviceName "ReceiveMessage" 


-- | <p>Revokes any permissions in the queue policy that matches the specified <code>Label</code> parameter. Only the owner of the queue can remove permissions.</p>
removePermission :: forall eff. RemovePermissionRequest -> Aff (err :: AWS.RequestError | eff) Unit
removePermission = AWS.request serviceName "RemovePermission" 


-- | <p>Delivers a message to the specified queue.</p> <important> <p>A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:</p> <p> <code>#x9</code> | <code>#xA</code> | <code>#xD</code> | <code>#x20</code> to <code>#xD7FF</code> | <code>#xE000</code> to <code>#xFFFD</code> | <code>#x10000</code> to <code>#x10FFFF</code> </p> <p>Any characters not included in this list will be rejected. For more information, see the <a href="http://www.w3.org/TR/REC-xml/#charsets">W3C specification for characters</a>.</p> </important>
sendMessage :: forall eff. SendMessageRequest -> Aff (err :: AWS.RequestError | eff) SendMessageResult
sendMessage = AWS.request serviceName "SendMessage" 


-- | <p>Delivers up to ten messages to the specified queue. This is a batch version of <code> <a>SendMessage</a>.</code> For a FIFO queue, multiple messages within a single batch are enqueued in the order they are sent.</p> <p>The result of sending each message is reported individually in the response. Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of <code>200</code>.</p> <p>The maximum allowed individual message size and the maximum total payload size (the sum of the individual lengths of all of the batched messages) are both 256 KB (262,144 bytes).</p> <important> <p>A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:</p> <p> <code>#x9</code> | <code>#xA</code> | <code>#xD</code> | <code>#x20</code> to <code>#xD7FF</code> | <code>#xE000</code> to <code>#xFFFD</code> | <code>#x10000</code> to <code>#x10FFFF</code> </p> <p>Any characters not included in this list will be rejected. For more information, see the <a href="http://www.w3.org/TR/REC-xml/#charsets">W3C specification for characters</a>.</p> </important> <p>If you don't specify the <code>DelaySeconds</code> parameter for an entry, Amazon SQS uses the default value for the queue.</p> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
sendMessageBatch :: forall eff. SendMessageBatchRequest -> Aff (err :: AWS.RequestError | eff) SendMessageBatchResult
sendMessageBatch = AWS.request serviceName "SendMessageBatch" 


-- | <p>Sets the value of one or more queue attributes. When you change a queue's attributes, the change can take up to 60 seconds for most of the attributes to propagate throughout the Amazon SQS system. Changes made to the <code>MessageRetentionPeriod</code> attribute can take up to 15 minutes.</p> <note> <p>In the future, new attributes might be added. If you write code that calls this action, we recommend that you structure your code so that it can handle new attributes gracefully.</p> </note>
setQueueAttributes :: forall eff. SetQueueAttributesRequest -> Aff (err :: AWS.RequestError | eff) Unit
setQueueAttributes = AWS.request serviceName "SetQueueAttributes" 


-- | <p>Add cost allocation tags to the specified Amazon SQS queue. For an overview, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-tagging-queues.html">Tagging Amazon SQS Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>When you use queue tags, keep the following guidelines in mind:</p> <ul> <li> <p>Adding more than 50 tags to a queue isn't recommended.</p> </li> <li> <p>Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.</p> </li> <li> <p>Tags are case-sensitive.</p> </li> <li> <p>A new tag with a key identical to that of an existing tag overwrites the existing tag.</p> </li> <li> <p>Tagging API actions are limited to 5 TPS per AWS account. If your application requires a higher throughput, file a <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=technical">technical support request</a>.</p> </li> </ul> <p>For a full list of tag restrictions, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html">Limits Related to Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
tagQueue :: forall eff. TagQueueRequest -> Aff (err :: AWS.RequestError | eff) Unit
tagQueue = AWS.request serviceName "TagQueue" 


-- | <p>Remove cost allocation tags from the specified Amazon SQS queue. For an overview, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-tagging-queues.html">Tagging Amazon SQS Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>When you use queue tags, keep the following guidelines in mind:</p> <ul> <li> <p>Adding more than 50 tags to a queue isn't recommended.</p> </li> <li> <p>Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.</p> </li> <li> <p>Tags are case-sensitive.</p> </li> <li> <p>A new tag with a key identical to that of an existing tag overwrites the existing tag.</p> </li> <li> <p>Tagging API actions are limited to 5 TPS per AWS account. If your application requires a higher throughput, file a <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=technical">technical support request</a>.</p> </li> </ul> <p>For a full list of tag restrictions, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html">Limits Related to Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
untagQueue :: forall eff. UntagQueueRequest -> Aff (err :: AWS.RequestError | eff) Unit
untagQueue = AWS.request serviceName "UntagQueue" 


newtype AWSAccountIdList = AWSAccountIdList (Array String)


newtype ActionNameList = ActionNameList (Array String)


-- | <p/>
newtype AddPermissionRequest = AddPermissionRequest 
  { "QueueUrl" :: (String)
  , "Label" :: (String)
  , "AWSAccountIds" :: (AWSAccountIdList)
  , "Actions" :: (ActionNameList)
  }


newtype AttributeNameList = AttributeNameList (Array QueueAttributeName)


-- | <p>Two or more batch entries in the request have the same <code>Id</code>.</p>
newtype BatchEntryIdsNotDistinct = BatchEntryIdsNotDistinct 
  { 
  }


-- | <p>The length of all the messages put together is more than the limit.</p>
newtype BatchRequestTooLong = BatchRequestTooLong 
  { 
  }


-- | <p>This is used in the responses of batch API to give a detailed description of the result of an action on each entry in the request.</p>
newtype BatchResultErrorEntry = BatchResultErrorEntry 
  { "Id" :: (String)
  , "SenderFault" :: (Boolean)
  , "Code" :: (String)
  , "Message" :: NullOrUndefined (String)
  }


newtype BatchResultErrorEntryList = BatchResultErrorEntryList (Array BatchResultErrorEntry)


newtype Binary = Binary String


newtype BinaryList = BinaryList (Array Binary)


-- | <p/>
newtype ChangeMessageVisibilityBatchRequest = ChangeMessageVisibilityBatchRequest 
  { "QueueUrl" :: (String)
  , "Entries" :: (ChangeMessageVisibilityBatchRequestEntryList)
  }


-- | <p>Encloses a receipt handle and an entry id for each message in <code> <a>ChangeMessageVisibilityBatch</a>.</code> </p> <important> <p>All of the following list parameters must be prefixed with <code>ChangeMessageVisibilityBatchRequestEntry.n</code>, where <code>n</code> is an integer value starting with <code>1</code>. For example, a parameter list for this action might look like this:</p> </important> <p> <code>&amp;amp;ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2</code> </p> <p> <code>&amp;amp;ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=&lt;replaceable&gt;Your_Receipt_Handle&lt;/replaceable&gt;</code> </p> <p> <code>&amp;amp;ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45</code> </p>
newtype ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry 
  { "Id" :: (String)
  , "ReceiptHandle" :: (String)
  , "VisibilityTimeout" :: NullOrUndefined (Int)
  }


newtype ChangeMessageVisibilityBatchRequestEntryList = ChangeMessageVisibilityBatchRequestEntryList (Array ChangeMessageVisibilityBatchRequestEntry)


-- | <p>For each message in the batch, the response contains a <code> <a>ChangeMessageVisibilityBatchResultEntry</a> </code> tag if the message succeeds or a <code> <a>BatchResultErrorEntry</a> </code> tag if the message fails.</p>
newtype ChangeMessageVisibilityBatchResult = ChangeMessageVisibilityBatchResult 
  { "Successful" :: (ChangeMessageVisibilityBatchResultEntryList)
  , "Failed" :: (BatchResultErrorEntryList)
  }


-- | <p>Encloses the <code>Id</code> of an entry in <code> <a>ChangeMessageVisibilityBatch</a>.</code> </p>
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry 
  { "Id" :: (String)
  }


newtype ChangeMessageVisibilityBatchResultEntryList = ChangeMessageVisibilityBatchResultEntryList (Array ChangeMessageVisibilityBatchResultEntry)


newtype ChangeMessageVisibilityRequest = ChangeMessageVisibilityRequest 
  { "QueueUrl" :: (String)
  , "ReceiptHandle" :: (String)
  , "VisibilityTimeout" :: (Int)
  }


-- | <p/>
newtype CreateQueueRequest = CreateQueueRequest 
  { "QueueName" :: (String)
  , "Attributes" :: NullOrUndefined (QueueAttributeMap)
  }


-- | <p>Returns the <code>QueueUrl</code> attribute of the created queue.</p>
newtype CreateQueueResult = CreateQueueResult 
  { "QueueUrl" :: NullOrUndefined (String)
  }


-- | <p/>
newtype DeleteMessageBatchRequest = DeleteMessageBatchRequest 
  { "QueueUrl" :: (String)
  , "Entries" :: (DeleteMessageBatchRequestEntryList)
  }


-- | <p>Encloses a receipt handle and an identifier for it.</p>
newtype DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry 
  { "Id" :: (String)
  , "ReceiptHandle" :: (String)
  }


newtype DeleteMessageBatchRequestEntryList = DeleteMessageBatchRequestEntryList (Array DeleteMessageBatchRequestEntry)


-- | <p>For each message in the batch, the response contains a <code> <a>DeleteMessageBatchResultEntry</a> </code> tag if the message is deleted or a <code> <a>BatchResultErrorEntry</a> </code> tag if the message can't be deleted.</p>
newtype DeleteMessageBatchResult = DeleteMessageBatchResult 
  { "Successful" :: (DeleteMessageBatchResultEntryList)
  , "Failed" :: (BatchResultErrorEntryList)
  }


-- | <p>Encloses the <code>Id</code> of an entry in <code> <a>DeleteMessageBatch</a>.</code> </p>
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry 
  { "Id" :: (String)
  }


newtype DeleteMessageBatchResultEntryList = DeleteMessageBatchResultEntryList (Array DeleteMessageBatchResultEntry)


-- | <p/>
newtype DeleteMessageRequest = DeleteMessageRequest 
  { "QueueUrl" :: (String)
  , "ReceiptHandle" :: (String)
  }


-- | <p/>
newtype DeleteQueueRequest = DeleteQueueRequest 
  { "QueueUrl" :: (String)
  }


-- | <p>The batch request doesn't contain any entries.</p>
newtype EmptyBatchRequest = EmptyBatchRequest 
  { 
  }


-- | <p/>
newtype GetQueueAttributesRequest = GetQueueAttributesRequest 
  { "QueueUrl" :: (String)
  , "AttributeNames" :: NullOrUndefined (AttributeNameList)
  }


-- | <p>A list of returned queue attributes.</p>
newtype GetQueueAttributesResult = GetQueueAttributesResult 
  { "Attributes" :: NullOrUndefined (QueueAttributeMap)
  }


-- | <p/>
newtype GetQueueUrlRequest = GetQueueUrlRequest 
  { "QueueName" :: (String)
  , "QueueOwnerAWSAccountId" :: NullOrUndefined (String)
  }


-- | <p>For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/UnderstandingResponses.html">Responses</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
newtype GetQueueUrlResult = GetQueueUrlResult 
  { "QueueUrl" :: NullOrUndefined (String)
  }


-- | <p>The attribute referred to doesn't exist.</p>
newtype InvalidAttributeName = InvalidAttributeName 
  { 
  }


-- | <p>The <code>Id</code> of a batch entry in a batch request doesn't abide by the specification.</p>
newtype InvalidBatchEntryId = InvalidBatchEntryId 
  { 
  }


-- | <p>The receipt handle isn't valid for the current version.</p>
newtype InvalidIdFormat = InvalidIdFormat 
  { 
  }


-- | <p>The message contains characters outside the allowed set.</p>
newtype InvalidMessageContents = InvalidMessageContents 
  { 
  }


-- | <p/>
newtype ListDeadLetterSourceQueuesRequest = ListDeadLetterSourceQueuesRequest 
  { "QueueUrl" :: (String)
  }


-- | <p>A list of your dead letter source queues.</p>
newtype ListDeadLetterSourceQueuesResult = ListDeadLetterSourceQueuesResult 
  { "QueueUrls'" :: (QueueUrlList)
  }


newtype ListQueueTagsRequest = ListQueueTagsRequest 
  { "QueueUrl" :: (String)
  }


newtype ListQueueTagsResult = ListQueueTagsResult 
  { "Tags" :: NullOrUndefined (TagMap)
  }


-- | <p/>
newtype ListQueuesRequest = ListQueuesRequest 
  { "QueueNamePrefix" :: NullOrUndefined (String)
  }


-- | <p>A list of your queues.</p>
newtype ListQueuesResult = ListQueuesResult 
  { "QueueUrls" :: NullOrUndefined (QueueUrlList)
  }


-- | <p>An Amazon SQS message.</p>
newtype Message = Message 
  { "MessageId" :: NullOrUndefined (String)
  , "ReceiptHandle" :: NullOrUndefined (String)
  , "MD5OfBody" :: NullOrUndefined (String)
  , "Body" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (MessageSystemAttributeMap)
  , "MD5OfMessageAttributes" :: NullOrUndefined (String)
  , "MessageAttributes" :: NullOrUndefined (MessageBodyAttributeMap)
  }


newtype MessageAttributeName = MessageAttributeName String


newtype MessageAttributeNameList = MessageAttributeNameList (Array MessageAttributeName)


-- | <p>The user-specified message attribute value. For string data types, the <code>Value</code> attribute has the same restrictions on the content as the message body. For more information, see <code> <a>SendMessage</a>.</code> </p> <p> <code>Name</code>, <code>type</code>, <code>value</code> and the message body must not be empty or null. All parts of the message attribute, including <code>Name</code>, <code>Type</code>, and <code>Value</code>, are part of the message size restriction (256 KB or 262,144 bytes).</p>
newtype MessageAttributeValue = MessageAttributeValue 
  { "StringValue" :: NullOrUndefined (String)
  , "BinaryValue" :: NullOrUndefined (Binary)
  , "StringListValues" :: NullOrUndefined (StringList)
  , "BinaryListValues" :: NullOrUndefined (BinaryList)
  , "DataType" :: (String)
  }


newtype MessageBodyAttributeMap = MessageBodyAttributeMap (Map String MessageAttributeValue)


newtype MessageList = MessageList (Array Message)


-- | <p>The message referred to isn't in flight.</p>
newtype MessageNotInflight = MessageNotInflight 
  { 
  }


newtype MessageSystemAttributeMap = MessageSystemAttributeMap (Map MessageSystemAttributeName String)


newtype MessageSystemAttributeName = MessageSystemAttributeName String


-- | <p>The action that you requested would violate a limit. For example, <code>ReceiveMessage</code> returns this error if the maximum number of inflight messages is reached. <code> <a>AddPermission</a> </code> returns this error if the maximum number of permissions for the queue is reached.</p>
newtype OverLimit = OverLimit 
  { 
  }


-- | <p>Indicates that the specified queue previously received a <code>PurgeQueue</code> request within the last 60 seconds (the time it can take to delete the messages in the queue).</p>
newtype PurgeQueueInProgress = PurgeQueueInProgress 
  { 
  }


-- | <p/>
newtype PurgeQueueRequest = PurgeQueueRequest 
  { "QueueUrl" :: (String)
  }


newtype QueueAttributeMap = QueueAttributeMap (Map QueueAttributeName String)


newtype QueueAttributeName = QueueAttributeName String


-- | <p>You must wait 60 seconds after deleting a queue before you can create another one with the same name.</p>
newtype QueueDeletedRecently = QueueDeletedRecently 
  { 
  }


-- | <p>The queue referred to doesn't exist.</p>
newtype QueueDoesNotExist = QueueDoesNotExist 
  { 
  }


-- | <p>A queue already exists with this name. Amazon SQS returns this error only if the request includes attributes whose values differ from those of the existing queue.</p>
newtype QueueNameExists = QueueNameExists 
  { 
  }


newtype QueueUrlList = QueueUrlList (Array String)


-- | <p>The receipt handle provided isn't valid.</p>
newtype ReceiptHandleIsInvalid = ReceiptHandleIsInvalid 
  { 
  }


-- | <p/>
newtype ReceiveMessageRequest = ReceiveMessageRequest 
  { "QueueUrl" :: (String)
  , "AttributeNames" :: NullOrUndefined (AttributeNameList)
  , "MessageAttributeNames" :: NullOrUndefined (MessageAttributeNameList)
  , "MaxNumberOfMessages" :: NullOrUndefined (Int)
  , "VisibilityTimeout" :: NullOrUndefined (Int)
  , "WaitTimeSeconds" :: NullOrUndefined (Int)
  , "ReceiveRequestAttemptId" :: NullOrUndefined (String)
  }


-- | <p>A list of received messages.</p>
newtype ReceiveMessageResult = ReceiveMessageResult 
  { "Messages" :: NullOrUndefined (MessageList)
  }


-- | <p/>
newtype RemovePermissionRequest = RemovePermissionRequest 
  { "QueueUrl" :: (String)
  , "Label" :: (String)
  }


-- | <p/>
newtype SendMessageBatchRequest = SendMessageBatchRequest 
  { "QueueUrl" :: (String)
  , "Entries" :: (SendMessageBatchRequestEntryList)
  }


-- | <p>Contains the details of a single Amazon SQS message along with an <code>Id</code>.</p>
newtype SendMessageBatchRequestEntry = SendMessageBatchRequestEntry 
  { "Id" :: (String)
  , "MessageBody" :: (String)
  , "DelaySeconds" :: NullOrUndefined (Int)
  , "MessageAttributes" :: NullOrUndefined (MessageBodyAttributeMap)
  , "MessageDeduplicationId" :: NullOrUndefined (String)
  , "MessageGroupId" :: NullOrUndefined (String)
  }


newtype SendMessageBatchRequestEntryList = SendMessageBatchRequestEntryList (Array SendMessageBatchRequestEntry)


-- | <p>For each message in the batch, the response contains a <code> <a>SendMessageBatchResultEntry</a> </code> tag if the message succeeds or a <code> <a>BatchResultErrorEntry</a> </code> tag if the message fails.</p>
newtype SendMessageBatchResult = SendMessageBatchResult 
  { "Successful" :: (SendMessageBatchResultEntryList)
  , "Failed" :: (BatchResultErrorEntryList)
  }


-- | <p>Encloses a <code>MessageId</code> for a successfully-enqueued message in a <code> <a>SendMessageBatch</a>.</code> </p>
newtype SendMessageBatchResultEntry = SendMessageBatchResultEntry 
  { "Id" :: (String)
  , "MessageId" :: (String)
  , "MD5OfMessageBody" :: (String)
  , "MD5OfMessageAttributes" :: NullOrUndefined (String)
  , "SequenceNumber" :: NullOrUndefined (String)
  }


newtype SendMessageBatchResultEntryList = SendMessageBatchResultEntryList (Array SendMessageBatchResultEntry)


-- | <p/>
newtype SendMessageRequest = SendMessageRequest 
  { "QueueUrl" :: (String)
  , "MessageBody" :: (String)
  , "DelaySeconds" :: NullOrUndefined (Int)
  , "MessageAttributes" :: NullOrUndefined (MessageBodyAttributeMap)
  , "MessageDeduplicationId" :: NullOrUndefined (String)
  , "MessageGroupId" :: NullOrUndefined (String)
  }


-- | <p>The <code>MD5OfMessageBody</code> and <code>MessageId</code> elements.</p>
newtype SendMessageResult = SendMessageResult 
  { "MD5OfMessageBody" :: NullOrUndefined (String)
  , "MD5OfMessageAttributes" :: NullOrUndefined (String)
  , "MessageId" :: NullOrUndefined (String)
  , "SequenceNumber" :: NullOrUndefined (String)
  }


-- | <p/>
newtype SetQueueAttributesRequest = SetQueueAttributesRequest 
  { "QueueUrl" :: (String)
  , "Attributes" :: (QueueAttributeMap)
  }


newtype StringList = StringList (Array String)


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagMap = TagMap (Map TagKey TagValue)


newtype TagQueueRequest = TagQueueRequest 
  { "QueueUrl" :: (String)
  , "Tags" :: (TagMap)
  }


newtype TagValue = TagValue String


-- | <p>The batch request contains more entries than permissible.</p>
newtype TooManyEntriesInBatchRequest = TooManyEntriesInBatchRequest 
  { 
  }


-- | <p>Error code 400. Unsupported operation.</p>
newtype UnsupportedOperation = UnsupportedOperation 
  { 
  }


newtype UntagQueueRequest = UntagQueueRequest 
  { "QueueUrl" :: (String)
  , "TagKeys" :: (TagKeyList)
  }
