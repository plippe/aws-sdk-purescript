

-- | <p>Welcome to the <i>Amazon Simple Queue Service API Reference</i>.</p> <p>Amazon Simple Queue Service (Amazon SQS) is a reliable, highly-scalable hosted queue for storing messages as they travel between applications or microservices. Amazon SQS moves data between distributed application components and helps you decouple these components.</p> <note> <p> <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/standard-queues.html">Standard queues</a> are available in all regions. <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html">FIFO queues</a> are available in the US East (N. Virginia), US East (Ohio), US West (Oregon), and EU (Ireland) regions.</p> </note> <p>You can use <a href="http://aws.amazon.com/tools/#sdk">AWS SDKs</a> to access Amazon SQS using your favorite programming language. The SDKs perform tasks such as the following automatically:</p> <ul> <li> <p>Cryptographically sign your service requests</p> </li> <li> <p>Retry requests</p> </li> <li> <p>Handle error responses</p> </li> </ul> <p> <b>Additional Information</b> </p> <ul> <li> <p> <a href="http://aws.amazon.com/sqs/">Amazon SQS Product Page</a> </p> </li> <li> <p> <i>Amazon Simple Queue Service Developer Guide</i> </p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/MakingRequestsArticle.html">Making API Requests</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-message-attributes.html">Using Amazon SQS Message Attributes</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html">Using Amazon SQS Dead-Letter Queues</a> </p> </li> </ul> </li> <li> <p> <i>Amazon Web Services General Reference</i> </p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#sqs_region">Regions and Endpoints</a> </p> </li> </ul> </li> </ul>
module AWS.SQS where

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

serviceName = "SQS" :: String


-- | <p>Adds a permission to a queue for a specific <a href="http://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P">principal</a>. This allows sharing access to the queue.</p> <p>When you create a queue, you have full control access rights for the queue. Only you, the owner of the queue, can grant or deny permissions to the queue. For more information about these permissions, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html">Shared Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <note> <p> <code>AddPermission</code> writes an Amazon-SQS-generated policy. If you want to write your own policy, use <code> <a>SetQueueAttributes</a> </code> to upload your policy. For more information about writing your own policy, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AccessPolicyLanguage.html">Using The Access Policy Language</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
addPermission :: forall eff. AddPermissionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
addPermission = Request.request serviceName "addPermission" 


-- | <p>Changes the visibility timeout of a specified message in a queue to a new value. The maximum allowed timeout value is 12 hours. Thus, you can't extend the timeout of a message in an existing queue to more than a total visibility timeout of 12 hours. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html">Visibility Timeout</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>For example, you have a message with a visibility timeout of 5 minutes. After 3 minutes, you call <code>ChangeMessageVisiblity</code> with a timeout of 10 minutes. At that time, the timeout for the message is extended by 10 minutes beyond the time of the <code>ChangeMessageVisibility</code> action. This results in a total visibility timeout of 13 minutes. You can continue to call the <code>ChangeMessageVisibility</code> to extend the visibility timeout to a maximum of 12 hours. If you try to extend the visibility timeout beyond 12 hours, your request is rejected.</p> <p>A message is considered to be <i>in flight</i> after it's received from a queue by a consumer, but not yet deleted from the queue.</p> <p>For standard queues, there can be a maximum of 120,000 inflight messages per queue. If you reach this limit, Amazon SQS returns the <code>OverLimit</code> error message. To avoid reaching the limit, you should delete messages from the queue after they're processed. You can also increase the number of queues you use to process your messages.</p> <p>For FIFO queues, there can be a maximum of 20,000 inflight messages per queue. If you reach this limit, Amazon SQS returns no error messages.</p> <important> <p>If you attempt to set the <code>VisibilityTimeout</code> to a value greater than the maximum time left, Amazon SQS returns an error. Amazon SQS doesn't automatically recalculate and increase the timeout to the maximum remaining time.</p> <p>Unlike with a queue, when you change the visibility timeout for a specific message the timeout value is applied immediately but isn't saved in memory for that message. If you don't delete a message after it is received, the visibility timeout for the message reverts to the original timeout value (not to the value you set using the <code>ChangeMessageVisibility</code> action) the next time the message is received.</p> </important>
changeMessageVisibility :: forall eff. ChangeMessageVisibilityRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
changeMessageVisibility = Request.request serviceName "changeMessageVisibility" 


-- | <p>Changes the visibility timeout of multiple messages. This is a batch version of <code> <a>ChangeMessageVisibility</a>.</code> The result of the action on each message is reported individually in the response. You can send up to 10 <code> <a>ChangeMessageVisibility</a> </code> requests with each <code>ChangeMessageVisibilityBatch</code> action.</p> <important> <p>Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of <code>200</code>.</p> </important> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
changeMessageVisibilityBatch :: forall eff. ChangeMessageVisibilityBatchRequest -> Aff (exception :: EXCEPTION | eff) ChangeMessageVisibilityBatchResult
changeMessageVisibilityBatch = Request.request serviceName "changeMessageVisibilityBatch" 


-- | <p>Creates a new standard or FIFO queue. You can pass one or more attributes in the request. Keep the following caveats in mind:</p> <ul> <li> <p>If you don't specify the <code>FifoQueue</code> attribute, Amazon SQS creates a standard queue.</p> <note> <p> You can't change the queue type after you create it and you can't convert an existing standard queue into a FIFO queue. You must either create a new FIFO queue for your application or delete your existing standard queue and recreate it as a FIFO queue. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html#FIFO-queues-moving"> Moving From a Standard Queue to a FIFO Queue</a> in the <i>Amazon Simple Queue Service Developer Guide</i>. </p> </note> </li> <li> <p>If you don't provide a value for an attribute, the queue is created with the default value for the attribute.</p> </li> <li> <p>If you delete a queue, you must wait at least 60 seconds before creating a queue with the same name.</p> </li> </ul> <p>To successfully create a new queue, you must provide a queue name that adheres to the <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html">limits related to queues</a> and is unique within the scope of your queues.</p> <p>To get the queue URL, use the <code> <a>GetQueueUrl</a> </code> action. <code> <a>GetQueueUrl</a> </code> requires only the <code>QueueName</code> parameter. be aware of existing queue names:</p> <ul> <li> <p>If you provide the name of an existing queue along with the exact names and values of all the queue's attributes, <code>CreateQueue</code> returns the queue URL for the existing queue.</p> </li> <li> <p>If the queue name, attribute names, or attribute values don't match an existing queue, <code>CreateQueue</code> returns an error.</p> </li> </ul> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
createQueue :: forall eff. CreateQueueRequest -> Aff (exception :: EXCEPTION | eff) CreateQueueResult
createQueue = Request.request serviceName "createQueue" 


-- | <p>Deletes the specified message from the specified queue. You specify the message by using the message's <i>receipt handle</i> and not the <i>MessageId</i> you receive when you send the message. Even if the message is locked by another reader due to the visibility timeout setting, it is still deleted from the queue. If you leave a message in the queue for longer than the queue's configured retention period, Amazon SQS automatically deletes the message. </p> <note> <p> The receipt handle is associated with a specific instance of receiving the message. If you receive a message more than once, the receipt handle you get each time you receive the message is different. If you don't provide the most recently received receipt handle for the message when you use the <code>DeleteMessage</code> action, the request succeeds, but the message might not be deleted.</p> <p>For standard queues, it is possible to receive a message even after you delete it. This might happen on rare occasions if one of the servers storing a copy of the message is unavailable when you send the request to delete the message. The copy remains on the server and might be returned to you on a subsequent receive request. You should ensure that your application is idempotent, so that receiving a message more than once does not cause issues.</p> </note>
deleteMessage :: forall eff. DeleteMessageRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteMessage = Request.request serviceName "deleteMessage" 


-- | <p>Deletes up to ten messages from the specified queue. This is a batch version of <code> <a>DeleteMessage</a>.</code> The result of the action on each message is reported individually in the response.</p> <important> <p>Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of <code>200</code>.</p> </important> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
deleteMessageBatch :: forall eff. DeleteMessageBatchRequest -> Aff (exception :: EXCEPTION | eff) DeleteMessageBatchResult
deleteMessageBatch = Request.request serviceName "deleteMessageBatch" 


-- | <p>Deletes the queue specified by the <code>QueueUrl</code>, regardless of the queue's contents. If the specified queue doesn't exist, Amazon SQS returns a successful response.</p> <important> <p>Be careful with the <code>DeleteQueue</code> action: When you delete a queue, any messages in the queue are no longer available. </p> </important> <p>When you delete a queue, the deletion process takes up to 60 seconds. Requests you send involving that queue during the 60 seconds might succeed. For example, a <code> <a>SendMessage</a> </code> request might succeed, but after 60 seconds the queue and the message you sent no longer exist.</p> <p>When you delete a queue, you must wait at least 60 seconds before creating a queue with the same name. </p>
deleteQueue :: forall eff. DeleteQueueRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteQueue = Request.request serviceName "deleteQueue" 


-- | <p>Gets attributes for the specified queue.</p> <note> <p>To determine whether a queue is <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/FIFO-queues.html">FIFO</a>, you can check whether <code>QueueName</code> ends with the <code>.fifo</code> suffix.</p> </note> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
getQueueAttributes :: forall eff. GetQueueAttributesRequest -> Aff (exception :: EXCEPTION | eff) GetQueueAttributesResult
getQueueAttributes = Request.request serviceName "getQueueAttributes" 


-- | <p>Returns the URL of an existing queue. This action provides a simple way to retrieve the URL of an Amazon SQS queue.</p> <p>To access a queue that belongs to another AWS account, use the <code>QueueOwnerAWSAccountId</code> parameter to specify the account ID of the queue's owner. The queue's owner must grant you permission to access the queue. For more information about shared queue access, see <code> <a>AddPermission</a> </code> or see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html">Shared Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>. </p>
getQueueUrl :: forall eff. GetQueueUrlRequest -> Aff (exception :: EXCEPTION | eff) GetQueueUrlResult
getQueueUrl = Request.request serviceName "getQueueUrl" 


-- | <p>Returns a list of your queues that have the <code>RedrivePolicy</code> queue attribute configured with a dead-letter queue.</p> <p>For more information about using dead-letter queues, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-dead-letter-queues.html">Using Amazon SQS Dead-Letter Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
listDeadLetterSourceQueues :: forall eff. ListDeadLetterSourceQueuesRequest -> Aff (exception :: EXCEPTION | eff) ListDeadLetterSourceQueuesResult
listDeadLetterSourceQueues = Request.request serviceName "listDeadLetterSourceQueues" 


-- | <p>List all cost allocation tags added to the specified Amazon SQS queue. For an overview, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-tagging-queues.html">Tagging Amazon SQS Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>When you use queue tags, keep the following guidelines in mind:</p> <ul> <li> <p>Adding more than 50 tags to a queue isn't recommended.</p> </li> <li> <p>Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.</p> </li> <li> <p>Tags are case-sensitive.</p> </li> <li> <p>A new tag with a key identical to that of an existing tag overwrites the existing tag.</p> </li> <li> <p>Tagging API actions are limited to 5 TPS per AWS account. If your application requires a higher throughput, file a <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=technical">technical support request</a>.</p> </li> </ul> <p>For a full list of tag restrictions, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html">Limits Related to Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
listQueueTags :: forall eff. ListQueueTagsRequest -> Aff (exception :: EXCEPTION | eff) ListQueueTagsResult
listQueueTags = Request.request serviceName "listQueueTags" 


-- | <p>Returns a list of your queues. The maximum number of queues that can be returned is 1,000. If you specify a value for the optional <code>QueueNamePrefix</code> parameter, only queues with a name that begins with the specified value are returned.</p>
listQueues :: forall eff. ListQueuesRequest -> Aff (exception :: EXCEPTION | eff) ListQueuesResult
listQueues = Request.request serviceName "listQueues" 


-- | <p>Deletes the messages in a queue specified by the <code>QueueURL</code> parameter.</p> <important> <p>When you use the <code>PurgeQueue</code> action, you can't retrieve a message deleted from a queue.</p> </important> <p>When you purge a queue, the message deletion process takes up to 60 seconds. All messages sent to the queue before calling the <code>PurgeQueue</code> action are deleted. Messages sent to the queue while it is being purged might be deleted. While the queue is being purged, messages sent to the queue before <code>PurgeQueue</code> is called might be received, but are deleted within the next minute.</p>
purgeQueue :: forall eff. PurgeQueueRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
purgeQueue = Request.request serviceName "purgeQueue" 


-- | <p>Retrieves one or more messages (up to 10), from the specified queue. Using the <code>WaitTimeSeconds</code> parameter enables long-poll support. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-long-polling.html">Amazon SQS Long Polling</a> in the <i>Amazon Simple Queue Service Developer Guide</i>. </p> <p>Short poll is the default behavior where a weighted random set of machines is sampled on a <code>ReceiveMessage</code> call. Thus, only the messages on the sampled machines are returned. If the number of messages in the queue is small (fewer than 1,000), you most likely get fewer messages than you requested per <code>ReceiveMessage</code> call. If the number of messages in the queue is extremely small, you might not receive any messages in a particular <code>ReceiveMessage</code> response. If this happens, repeat the request. </p> <p>For each message returned, the response includes the following:</p> <ul> <li> <p>The message body.</p> </li> <li> <p>An MD5 digest of the message body. For information about MD5, see <a href="https://www.ietf.org/rfc/rfc1321.txt">RFC1321</a>.</p> </li> <li> <p>The <code>MessageId</code> you received when you sent the message to the queue.</p> </li> <li> <p>The receipt handle.</p> </li> <li> <p>The message attributes.</p> </li> <li> <p>An MD5 digest of the message attributes.</p> </li> </ul> <p>The receipt handle is the identifier you must provide when deleting the message. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-queue-message-identifiers.html">Queue and Message Identifiers</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>You can provide the <code>VisibilityTimeout</code> parameter in your request. The parameter is applied to the messages that Amazon SQS returns in the response. If you don't include the parameter, the overall visibility timeout for the queue is used for the returned messages. For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-visibility-timeout.html">Visibility Timeout</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>A message that isn't deleted or a message whose visibility isn't extended before the visibility timeout expires counts as a failed receive. Depending on the configuration of the queue, the message might be sent to the dead-letter queue.</p> <note> <p>In the future, new attributes might be added. If you write code that calls this action, we recommend that you structure your code so that it can handle new attributes gracefully.</p> </note>
receiveMessage :: forall eff. ReceiveMessageRequest -> Aff (exception :: EXCEPTION | eff) ReceiveMessageResult
receiveMessage = Request.request serviceName "receiveMessage" 


-- | <p>Revokes any permissions in the queue policy that matches the specified <code>Label</code> parameter. Only the owner of the queue can remove permissions.</p>
removePermission :: forall eff. RemovePermissionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
removePermission = Request.request serviceName "removePermission" 


-- | <p>Delivers a message to the specified queue.</p> <important> <p>A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:</p> <p> <code>#x9</code> | <code>#xA</code> | <code>#xD</code> | <code>#x20</code> to <code>#xD7FF</code> | <code>#xE000</code> to <code>#xFFFD</code> | <code>#x10000</code> to <code>#x10FFFF</code> </p> <p>Any characters not included in this list will be rejected. For more information, see the <a href="http://www.w3.org/TR/REC-xml/#charsets">W3C specification for characters</a>.</p> </important>
sendMessage :: forall eff. SendMessageRequest -> Aff (exception :: EXCEPTION | eff) SendMessageResult
sendMessage = Request.request serviceName "sendMessage" 


-- | <p>Delivers up to ten messages to the specified queue. This is a batch version of <code> <a>SendMessage</a>.</code> For a FIFO queue, multiple messages within a single batch are enqueued in the order they are sent.</p> <p>The result of sending each message is reported individually in the response. Because the batch request can result in a combination of successful and unsuccessful actions, you should check for batch errors even when the call returns an HTTP status code of <code>200</code>.</p> <p>The maximum allowed individual message size and the maximum total payload size (the sum of the individual lengths of all of the batched messages) are both 256 KB (262,144 bytes).</p> <important> <p>A message can include only XML, JSON, and unformatted text. The following Unicode characters are allowed:</p> <p> <code>#x9</code> | <code>#xA</code> | <code>#xD</code> | <code>#x20</code> to <code>#xD7FF</code> | <code>#xE000</code> to <code>#xFFFD</code> | <code>#x10000</code> to <code>#x10FFFF</code> </p> <p>Any characters not included in this list will be rejected. For more information, see the <a href="http://www.w3.org/TR/REC-xml/#charsets">W3C specification for characters</a>.</p> </important> <p>If you don't specify the <code>DelaySeconds</code> parameter for an entry, Amazon SQS uses the default value for the queue.</p> <note> <p>Some actions take lists of parameters. These lists are specified using the <code>param.n</code> notation. Values of <code>n</code> are integers starting from 1. For example, a parameter list with two elements looks like this:</p> <p> <code>&amp;Attribute.1=this</code> </p> <p> <code>&amp;Attribute.2=that</code> </p> </note>
sendMessageBatch :: forall eff. SendMessageBatchRequest -> Aff (exception :: EXCEPTION | eff) SendMessageBatchResult
sendMessageBatch = Request.request serviceName "sendMessageBatch" 


-- | <p>Sets the value of one or more queue attributes. When you change a queue's attributes, the change can take up to 60 seconds for most of the attributes to propagate throughout the Amazon SQS system. Changes made to the <code>MessageRetentionPeriod</code> attribute can take up to 15 minutes.</p> <note> <p>In the future, new attributes might be added. If you write code that calls this action, we recommend that you structure your code so that it can handle new attributes gracefully.</p> </note>
setQueueAttributes :: forall eff. SetQueueAttributesRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setQueueAttributes = Request.request serviceName "setQueueAttributes" 


-- | <p>Add cost allocation tags to the specified Amazon SQS queue. For an overview, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-tagging-queues.html">Tagging Amazon SQS Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>When you use queue tags, keep the following guidelines in mind:</p> <ul> <li> <p>Adding more than 50 tags to a queue isn't recommended.</p> </li> <li> <p>Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.</p> </li> <li> <p>Tags are case-sensitive.</p> </li> <li> <p>A new tag with a key identical to that of an existing tag overwrites the existing tag.</p> </li> <li> <p>Tagging API actions are limited to 5 TPS per AWS account. If your application requires a higher throughput, file a <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=technical">technical support request</a>.</p> </li> </ul> <p>For a full list of tag restrictions, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html">Limits Related to Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
tagQueue :: forall eff. TagQueueRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
tagQueue = Request.request serviceName "tagQueue" 


-- | <p>Remove cost allocation tags from the specified Amazon SQS queue. For an overview, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/sqs-tagging-queues.html">Tagging Amazon SQS Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p> <p>When you use queue tags, keep the following guidelines in mind:</p> <ul> <li> <p>Adding more than 50 tags to a queue isn't recommended.</p> </li> <li> <p>Tags don't have any semantic meaning. Amazon SQS interprets tags as character strings.</p> </li> <li> <p>Tags are case-sensitive.</p> </li> <li> <p>A new tag with a key identical to that of an existing tag overwrites the existing tag.</p> </li> <li> <p>Tagging API actions are limited to 5 TPS per AWS account. If your application requires a higher throughput, file a <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=technical">technical support request</a>.</p> </li> </ul> <p>For a full list of tag restrictions, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/limits-queues.html">Limits Related to Queues</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
untagQueue :: forall eff. UntagQueueRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
untagQueue = Request.request serviceName "untagQueue" 


newtype AWSAccountIdList = AWSAccountIdList (Array String)
derive instance newtypeAWSAccountIdList :: Newtype AWSAccountIdList _
derive instance repGenericAWSAccountIdList :: Generic AWSAccountIdList _
instance showAWSAccountIdList :: Show AWSAccountIdList where
  show = genericShow
instance decodeAWSAccountIdList :: Decode AWSAccountIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAWSAccountIdList :: Encode AWSAccountIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionNameList = ActionNameList (Array String)
derive instance newtypeActionNameList :: Newtype ActionNameList _
derive instance repGenericActionNameList :: Generic ActionNameList _
instance showActionNameList :: Show ActionNameList where
  show = genericShow
instance decodeActionNameList :: Decode ActionNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionNameList :: Encode ActionNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AddPermissionRequest = AddPermissionRequest 
  { "QueueUrl" :: (String)
  , "Label" :: (String)
  , "AWSAccountIds" :: (AWSAccountIdList)
  , "Actions" :: (ActionNameList)
  }
derive instance newtypeAddPermissionRequest :: Newtype AddPermissionRequest _
derive instance repGenericAddPermissionRequest :: Generic AddPermissionRequest _
instance showAddPermissionRequest :: Show AddPermissionRequest where
  show = genericShow
instance decodeAddPermissionRequest :: Decode AddPermissionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddPermissionRequest :: Encode AddPermissionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeNameList = AttributeNameList (Array QueueAttributeName)
derive instance newtypeAttributeNameList :: Newtype AttributeNameList _
derive instance repGenericAttributeNameList :: Generic AttributeNameList _
instance showAttributeNameList :: Show AttributeNameList where
  show = genericShow
instance decodeAttributeNameList :: Decode AttributeNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeNameList :: Encode AttributeNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Two or more batch entries in the request have the same <code>Id</code>.</p>
newtype BatchEntryIdsNotDistinct = BatchEntryIdsNotDistinct Types.NoArguments
derive instance newtypeBatchEntryIdsNotDistinct :: Newtype BatchEntryIdsNotDistinct _
derive instance repGenericBatchEntryIdsNotDistinct :: Generic BatchEntryIdsNotDistinct _
instance showBatchEntryIdsNotDistinct :: Show BatchEntryIdsNotDistinct where
  show = genericShow
instance decodeBatchEntryIdsNotDistinct :: Decode BatchEntryIdsNotDistinct where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchEntryIdsNotDistinct :: Encode BatchEntryIdsNotDistinct where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The length of all the messages put together is more than the limit.</p>
newtype BatchRequestTooLong = BatchRequestTooLong Types.NoArguments
derive instance newtypeBatchRequestTooLong :: Newtype BatchRequestTooLong _
derive instance repGenericBatchRequestTooLong :: Generic BatchRequestTooLong _
instance showBatchRequestTooLong :: Show BatchRequestTooLong where
  show = genericShow
instance decodeBatchRequestTooLong :: Decode BatchRequestTooLong where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchRequestTooLong :: Encode BatchRequestTooLong where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This is used in the responses of batch API to give a detailed description of the result of an action on each entry in the request.</p>
newtype BatchResultErrorEntry = BatchResultErrorEntry 
  { "Id" :: (String)
  , "SenderFault" :: (Boolean)
  , "Code" :: (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBatchResultErrorEntry :: Newtype BatchResultErrorEntry _
derive instance repGenericBatchResultErrorEntry :: Generic BatchResultErrorEntry _
instance showBatchResultErrorEntry :: Show BatchResultErrorEntry where
  show = genericShow
instance decodeBatchResultErrorEntry :: Decode BatchResultErrorEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchResultErrorEntry :: Encode BatchResultErrorEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchResultErrorEntryList = BatchResultErrorEntryList (Array BatchResultErrorEntry)
derive instance newtypeBatchResultErrorEntryList :: Newtype BatchResultErrorEntryList _
derive instance repGenericBatchResultErrorEntryList :: Generic BatchResultErrorEntryList _
instance showBatchResultErrorEntryList :: Show BatchResultErrorEntryList where
  show = genericShow
instance decodeBatchResultErrorEntryList :: Decode BatchResultErrorEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchResultErrorEntryList :: Encode BatchResultErrorEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Binary = Binary String
derive instance newtypeBinary :: Newtype Binary _
derive instance repGenericBinary :: Generic Binary _
instance showBinary :: Show Binary where
  show = genericShow
instance decodeBinary :: Decode Binary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBinary :: Encode Binary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BinaryList = BinaryList (Array Binary)
derive instance newtypeBinaryList :: Newtype BinaryList _
derive instance repGenericBinaryList :: Generic BinaryList _
instance showBinaryList :: Show BinaryList where
  show = genericShow
instance decodeBinaryList :: Decode BinaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBinaryList :: Encode BinaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ChangeMessageVisibilityBatchRequest = ChangeMessageVisibilityBatchRequest 
  { "QueueUrl" :: (String)
  , "Entries" :: (ChangeMessageVisibilityBatchRequestEntryList)
  }
derive instance newtypeChangeMessageVisibilityBatchRequest :: Newtype ChangeMessageVisibilityBatchRequest _
derive instance repGenericChangeMessageVisibilityBatchRequest :: Generic ChangeMessageVisibilityBatchRequest _
instance showChangeMessageVisibilityBatchRequest :: Show ChangeMessageVisibilityBatchRequest where
  show = genericShow
instance decodeChangeMessageVisibilityBatchRequest :: Decode ChangeMessageVisibilityBatchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeMessageVisibilityBatchRequest :: Encode ChangeMessageVisibilityBatchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Encloses a receipt handle and an entry id for each message in <code> <a>ChangeMessageVisibilityBatch</a>.</code> </p> <important> <p>All of the following list parameters must be prefixed with <code>ChangeMessageVisibilityBatchRequestEntry.n</code>, where <code>n</code> is an integer value starting with <code>1</code>. For example, a parameter list for this action might look like this:</p> </important> <p> <code>&amp;amp;ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2</code> </p> <p> <code>&amp;amp;ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=&lt;replaceable&gt;Your_Receipt_Handle&lt;/replaceable&gt;</code> </p> <p> <code>&amp;amp;ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45</code> </p>
newtype ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry 
  { "Id" :: (String)
  , "ReceiptHandle" :: (String)
  , "VisibilityTimeout" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeChangeMessageVisibilityBatchRequestEntry :: Newtype ChangeMessageVisibilityBatchRequestEntry _
derive instance repGenericChangeMessageVisibilityBatchRequestEntry :: Generic ChangeMessageVisibilityBatchRequestEntry _
instance showChangeMessageVisibilityBatchRequestEntry :: Show ChangeMessageVisibilityBatchRequestEntry where
  show = genericShow
instance decodeChangeMessageVisibilityBatchRequestEntry :: Decode ChangeMessageVisibilityBatchRequestEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeMessageVisibilityBatchRequestEntry :: Encode ChangeMessageVisibilityBatchRequestEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeMessageVisibilityBatchRequestEntryList = ChangeMessageVisibilityBatchRequestEntryList (Array ChangeMessageVisibilityBatchRequestEntry)
derive instance newtypeChangeMessageVisibilityBatchRequestEntryList :: Newtype ChangeMessageVisibilityBatchRequestEntryList _
derive instance repGenericChangeMessageVisibilityBatchRequestEntryList :: Generic ChangeMessageVisibilityBatchRequestEntryList _
instance showChangeMessageVisibilityBatchRequestEntryList :: Show ChangeMessageVisibilityBatchRequestEntryList where
  show = genericShow
instance decodeChangeMessageVisibilityBatchRequestEntryList :: Decode ChangeMessageVisibilityBatchRequestEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeMessageVisibilityBatchRequestEntryList :: Encode ChangeMessageVisibilityBatchRequestEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>For each message in the batch, the response contains a <code> <a>ChangeMessageVisibilityBatchResultEntry</a> </code> tag if the message succeeds or a <code> <a>BatchResultErrorEntry</a> </code> tag if the message fails.</p>
newtype ChangeMessageVisibilityBatchResult = ChangeMessageVisibilityBatchResult 
  { "Successful" :: (ChangeMessageVisibilityBatchResultEntryList)
  , "Failed" :: (BatchResultErrorEntryList)
  }
derive instance newtypeChangeMessageVisibilityBatchResult :: Newtype ChangeMessageVisibilityBatchResult _
derive instance repGenericChangeMessageVisibilityBatchResult :: Generic ChangeMessageVisibilityBatchResult _
instance showChangeMessageVisibilityBatchResult :: Show ChangeMessageVisibilityBatchResult where
  show = genericShow
instance decodeChangeMessageVisibilityBatchResult :: Decode ChangeMessageVisibilityBatchResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeMessageVisibilityBatchResult :: Encode ChangeMessageVisibilityBatchResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Encloses the <code>Id</code> of an entry in <code> <a>ChangeMessageVisibilityBatch</a>.</code> </p>
newtype ChangeMessageVisibilityBatchResultEntry = ChangeMessageVisibilityBatchResultEntry 
  { "Id" :: (String)
  }
derive instance newtypeChangeMessageVisibilityBatchResultEntry :: Newtype ChangeMessageVisibilityBatchResultEntry _
derive instance repGenericChangeMessageVisibilityBatchResultEntry :: Generic ChangeMessageVisibilityBatchResultEntry _
instance showChangeMessageVisibilityBatchResultEntry :: Show ChangeMessageVisibilityBatchResultEntry where
  show = genericShow
instance decodeChangeMessageVisibilityBatchResultEntry :: Decode ChangeMessageVisibilityBatchResultEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeMessageVisibilityBatchResultEntry :: Encode ChangeMessageVisibilityBatchResultEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeMessageVisibilityBatchResultEntryList = ChangeMessageVisibilityBatchResultEntryList (Array ChangeMessageVisibilityBatchResultEntry)
derive instance newtypeChangeMessageVisibilityBatchResultEntryList :: Newtype ChangeMessageVisibilityBatchResultEntryList _
derive instance repGenericChangeMessageVisibilityBatchResultEntryList :: Generic ChangeMessageVisibilityBatchResultEntryList _
instance showChangeMessageVisibilityBatchResultEntryList :: Show ChangeMessageVisibilityBatchResultEntryList where
  show = genericShow
instance decodeChangeMessageVisibilityBatchResultEntryList :: Decode ChangeMessageVisibilityBatchResultEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeMessageVisibilityBatchResultEntryList :: Encode ChangeMessageVisibilityBatchResultEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeMessageVisibilityRequest = ChangeMessageVisibilityRequest 
  { "QueueUrl" :: (String)
  , "ReceiptHandle" :: (String)
  , "VisibilityTimeout" :: (Int)
  }
derive instance newtypeChangeMessageVisibilityRequest :: Newtype ChangeMessageVisibilityRequest _
derive instance repGenericChangeMessageVisibilityRequest :: Generic ChangeMessageVisibilityRequest _
instance showChangeMessageVisibilityRequest :: Show ChangeMessageVisibilityRequest where
  show = genericShow
instance decodeChangeMessageVisibilityRequest :: Decode ChangeMessageVisibilityRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeMessageVisibilityRequest :: Encode ChangeMessageVisibilityRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateQueueRequest = CreateQueueRequest 
  { "QueueName" :: (String)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (QueueAttributeMap)
  }
derive instance newtypeCreateQueueRequest :: Newtype CreateQueueRequest _
derive instance repGenericCreateQueueRequest :: Generic CreateQueueRequest _
instance showCreateQueueRequest :: Show CreateQueueRequest where
  show = genericShow
instance decodeCreateQueueRequest :: Decode CreateQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateQueueRequest :: Encode CreateQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns the <code>QueueUrl</code> attribute of the created queue.</p>
newtype CreateQueueResult = CreateQueueResult 
  { "QueueUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateQueueResult :: Newtype CreateQueueResult _
derive instance repGenericCreateQueueResult :: Generic CreateQueueResult _
instance showCreateQueueResult :: Show CreateQueueResult where
  show = genericShow
instance decodeCreateQueueResult :: Decode CreateQueueResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateQueueResult :: Encode CreateQueueResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteMessageBatchRequest = DeleteMessageBatchRequest 
  { "QueueUrl" :: (String)
  , "Entries" :: (DeleteMessageBatchRequestEntryList)
  }
derive instance newtypeDeleteMessageBatchRequest :: Newtype DeleteMessageBatchRequest _
derive instance repGenericDeleteMessageBatchRequest :: Generic DeleteMessageBatchRequest _
instance showDeleteMessageBatchRequest :: Show DeleteMessageBatchRequest where
  show = genericShow
instance decodeDeleteMessageBatchRequest :: Decode DeleteMessageBatchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMessageBatchRequest :: Encode DeleteMessageBatchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Encloses a receipt handle and an identifier for it.</p>
newtype DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry 
  { "Id" :: (String)
  , "ReceiptHandle" :: (String)
  }
derive instance newtypeDeleteMessageBatchRequestEntry :: Newtype DeleteMessageBatchRequestEntry _
derive instance repGenericDeleteMessageBatchRequestEntry :: Generic DeleteMessageBatchRequestEntry _
instance showDeleteMessageBatchRequestEntry :: Show DeleteMessageBatchRequestEntry where
  show = genericShow
instance decodeDeleteMessageBatchRequestEntry :: Decode DeleteMessageBatchRequestEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMessageBatchRequestEntry :: Encode DeleteMessageBatchRequestEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMessageBatchRequestEntryList = DeleteMessageBatchRequestEntryList (Array DeleteMessageBatchRequestEntry)
derive instance newtypeDeleteMessageBatchRequestEntryList :: Newtype DeleteMessageBatchRequestEntryList _
derive instance repGenericDeleteMessageBatchRequestEntryList :: Generic DeleteMessageBatchRequestEntryList _
instance showDeleteMessageBatchRequestEntryList :: Show DeleteMessageBatchRequestEntryList where
  show = genericShow
instance decodeDeleteMessageBatchRequestEntryList :: Decode DeleteMessageBatchRequestEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMessageBatchRequestEntryList :: Encode DeleteMessageBatchRequestEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>For each message in the batch, the response contains a <code> <a>DeleteMessageBatchResultEntry</a> </code> tag if the message is deleted or a <code> <a>BatchResultErrorEntry</a> </code> tag if the message can't be deleted.</p>
newtype DeleteMessageBatchResult = DeleteMessageBatchResult 
  { "Successful" :: (DeleteMessageBatchResultEntryList)
  , "Failed" :: (BatchResultErrorEntryList)
  }
derive instance newtypeDeleteMessageBatchResult :: Newtype DeleteMessageBatchResult _
derive instance repGenericDeleteMessageBatchResult :: Generic DeleteMessageBatchResult _
instance showDeleteMessageBatchResult :: Show DeleteMessageBatchResult where
  show = genericShow
instance decodeDeleteMessageBatchResult :: Decode DeleteMessageBatchResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMessageBatchResult :: Encode DeleteMessageBatchResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Encloses the <code>Id</code> of an entry in <code> <a>DeleteMessageBatch</a>.</code> </p>
newtype DeleteMessageBatchResultEntry = DeleteMessageBatchResultEntry 
  { "Id" :: (String)
  }
derive instance newtypeDeleteMessageBatchResultEntry :: Newtype DeleteMessageBatchResultEntry _
derive instance repGenericDeleteMessageBatchResultEntry :: Generic DeleteMessageBatchResultEntry _
instance showDeleteMessageBatchResultEntry :: Show DeleteMessageBatchResultEntry where
  show = genericShow
instance decodeDeleteMessageBatchResultEntry :: Decode DeleteMessageBatchResultEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMessageBatchResultEntry :: Encode DeleteMessageBatchResultEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMessageBatchResultEntryList = DeleteMessageBatchResultEntryList (Array DeleteMessageBatchResultEntry)
derive instance newtypeDeleteMessageBatchResultEntryList :: Newtype DeleteMessageBatchResultEntryList _
derive instance repGenericDeleteMessageBatchResultEntryList :: Generic DeleteMessageBatchResultEntryList _
instance showDeleteMessageBatchResultEntryList :: Show DeleteMessageBatchResultEntryList where
  show = genericShow
instance decodeDeleteMessageBatchResultEntryList :: Decode DeleteMessageBatchResultEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMessageBatchResultEntryList :: Encode DeleteMessageBatchResultEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteMessageRequest = DeleteMessageRequest 
  { "QueueUrl" :: (String)
  , "ReceiptHandle" :: (String)
  }
derive instance newtypeDeleteMessageRequest :: Newtype DeleteMessageRequest _
derive instance repGenericDeleteMessageRequest :: Generic DeleteMessageRequest _
instance showDeleteMessageRequest :: Show DeleteMessageRequest where
  show = genericShow
instance decodeDeleteMessageRequest :: Decode DeleteMessageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMessageRequest :: Encode DeleteMessageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteQueueRequest = DeleteQueueRequest 
  { "QueueUrl" :: (String)
  }
derive instance newtypeDeleteQueueRequest :: Newtype DeleteQueueRequest _
derive instance repGenericDeleteQueueRequest :: Generic DeleteQueueRequest _
instance showDeleteQueueRequest :: Show DeleteQueueRequest where
  show = genericShow
instance decodeDeleteQueueRequest :: Decode DeleteQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteQueueRequest :: Encode DeleteQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The batch request doesn't contain any entries.</p>
newtype EmptyBatchRequest = EmptyBatchRequest Types.NoArguments
derive instance newtypeEmptyBatchRequest :: Newtype EmptyBatchRequest _
derive instance repGenericEmptyBatchRequest :: Generic EmptyBatchRequest _
instance showEmptyBatchRequest :: Show EmptyBatchRequest where
  show = genericShow
instance decodeEmptyBatchRequest :: Decode EmptyBatchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmptyBatchRequest :: Encode EmptyBatchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype GetQueueAttributesRequest = GetQueueAttributesRequest 
  { "QueueUrl" :: (String)
  , "AttributeNames" :: NullOrUndefined.NullOrUndefined (AttributeNameList)
  }
derive instance newtypeGetQueueAttributesRequest :: Newtype GetQueueAttributesRequest _
derive instance repGenericGetQueueAttributesRequest :: Generic GetQueueAttributesRequest _
instance showGetQueueAttributesRequest :: Show GetQueueAttributesRequest where
  show = genericShow
instance decodeGetQueueAttributesRequest :: Decode GetQueueAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetQueueAttributesRequest :: Encode GetQueueAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of returned queue attributes.</p>
newtype GetQueueAttributesResult = GetQueueAttributesResult 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (QueueAttributeMap)
  }
derive instance newtypeGetQueueAttributesResult :: Newtype GetQueueAttributesResult _
derive instance repGenericGetQueueAttributesResult :: Generic GetQueueAttributesResult _
instance showGetQueueAttributesResult :: Show GetQueueAttributesResult where
  show = genericShow
instance decodeGetQueueAttributesResult :: Decode GetQueueAttributesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetQueueAttributesResult :: Encode GetQueueAttributesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype GetQueueUrlRequest = GetQueueUrlRequest 
  { "QueueName" :: (String)
  , "QueueOwnerAWSAccountId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetQueueUrlRequest :: Newtype GetQueueUrlRequest _
derive instance repGenericGetQueueUrlRequest :: Generic GetQueueUrlRequest _
instance showGetQueueUrlRequest :: Show GetQueueUrlRequest where
  show = genericShow
instance decodeGetQueueUrlRequest :: Decode GetQueueUrlRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetQueueUrlRequest :: Encode GetQueueUrlRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>For more information, see <a href="http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/UnderstandingResponses.html">Responses</a> in the <i>Amazon Simple Queue Service Developer Guide</i>.</p>
newtype GetQueueUrlResult = GetQueueUrlResult 
  { "QueueUrl" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetQueueUrlResult :: Newtype GetQueueUrlResult _
derive instance repGenericGetQueueUrlResult :: Generic GetQueueUrlResult _
instance showGetQueueUrlResult :: Show GetQueueUrlResult where
  show = genericShow
instance decodeGetQueueUrlResult :: Decode GetQueueUrlResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetQueueUrlResult :: Encode GetQueueUrlResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The attribute referred to doesn't exist.</p>
newtype InvalidAttributeName = InvalidAttributeName Types.NoArguments
derive instance newtypeInvalidAttributeName :: Newtype InvalidAttributeName _
derive instance repGenericInvalidAttributeName :: Generic InvalidAttributeName _
instance showInvalidAttributeName :: Show InvalidAttributeName where
  show = genericShow
instance decodeInvalidAttributeName :: Decode InvalidAttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAttributeName :: Encode InvalidAttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Id</code> of a batch entry in a batch request doesn't abide by the specification.</p>
newtype InvalidBatchEntryId = InvalidBatchEntryId Types.NoArguments
derive instance newtypeInvalidBatchEntryId :: Newtype InvalidBatchEntryId _
derive instance repGenericInvalidBatchEntryId :: Generic InvalidBatchEntryId _
instance showInvalidBatchEntryId :: Show InvalidBatchEntryId where
  show = genericShow
instance decodeInvalidBatchEntryId :: Decode InvalidBatchEntryId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidBatchEntryId :: Encode InvalidBatchEntryId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The receipt handle isn't valid for the current version.</p>
newtype InvalidIdFormat = InvalidIdFormat Types.NoArguments
derive instance newtypeInvalidIdFormat :: Newtype InvalidIdFormat _
derive instance repGenericInvalidIdFormat :: Generic InvalidIdFormat _
instance showInvalidIdFormat :: Show InvalidIdFormat where
  show = genericShow
instance decodeInvalidIdFormat :: Decode InvalidIdFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidIdFormat :: Encode InvalidIdFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The message contains characters outside the allowed set.</p>
newtype InvalidMessageContents = InvalidMessageContents Types.NoArguments
derive instance newtypeInvalidMessageContents :: Newtype InvalidMessageContents _
derive instance repGenericInvalidMessageContents :: Generic InvalidMessageContents _
instance showInvalidMessageContents :: Show InvalidMessageContents where
  show = genericShow
instance decodeInvalidMessageContents :: Decode InvalidMessageContents where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidMessageContents :: Encode InvalidMessageContents where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ListDeadLetterSourceQueuesRequest = ListDeadLetterSourceQueuesRequest 
  { "QueueUrl" :: (String)
  }
derive instance newtypeListDeadLetterSourceQueuesRequest :: Newtype ListDeadLetterSourceQueuesRequest _
derive instance repGenericListDeadLetterSourceQueuesRequest :: Generic ListDeadLetterSourceQueuesRequest _
instance showListDeadLetterSourceQueuesRequest :: Show ListDeadLetterSourceQueuesRequest where
  show = genericShow
instance decodeListDeadLetterSourceQueuesRequest :: Decode ListDeadLetterSourceQueuesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeadLetterSourceQueuesRequest :: Encode ListDeadLetterSourceQueuesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of your dead letter source queues.</p>
newtype ListDeadLetterSourceQueuesResult = ListDeadLetterSourceQueuesResult 
  { "QueueUrls'" :: (QueueUrlList)
  }
derive instance newtypeListDeadLetterSourceQueuesResult :: Newtype ListDeadLetterSourceQueuesResult _
derive instance repGenericListDeadLetterSourceQueuesResult :: Generic ListDeadLetterSourceQueuesResult _
instance showListDeadLetterSourceQueuesResult :: Show ListDeadLetterSourceQueuesResult where
  show = genericShow
instance decodeListDeadLetterSourceQueuesResult :: Decode ListDeadLetterSourceQueuesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeadLetterSourceQueuesResult :: Encode ListDeadLetterSourceQueuesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListQueueTagsRequest = ListQueueTagsRequest 
  { "QueueUrl" :: (String)
  }
derive instance newtypeListQueueTagsRequest :: Newtype ListQueueTagsRequest _
derive instance repGenericListQueueTagsRequest :: Generic ListQueueTagsRequest _
instance showListQueueTagsRequest :: Show ListQueueTagsRequest where
  show = genericShow
instance decodeListQueueTagsRequest :: Decode ListQueueTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListQueueTagsRequest :: Encode ListQueueTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListQueueTagsResult = ListQueueTagsResult 
  { "Tags" :: NullOrUndefined.NullOrUndefined (TagMap)
  }
derive instance newtypeListQueueTagsResult :: Newtype ListQueueTagsResult _
derive instance repGenericListQueueTagsResult :: Generic ListQueueTagsResult _
instance showListQueueTagsResult :: Show ListQueueTagsResult where
  show = genericShow
instance decodeListQueueTagsResult :: Decode ListQueueTagsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListQueueTagsResult :: Encode ListQueueTagsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ListQueuesRequest = ListQueuesRequest 
  { "QueueNamePrefix" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListQueuesRequest :: Newtype ListQueuesRequest _
derive instance repGenericListQueuesRequest :: Generic ListQueuesRequest _
instance showListQueuesRequest :: Show ListQueuesRequest where
  show = genericShow
instance decodeListQueuesRequest :: Decode ListQueuesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListQueuesRequest :: Encode ListQueuesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of your queues.</p>
newtype ListQueuesResult = ListQueuesResult 
  { "QueueUrls" :: NullOrUndefined.NullOrUndefined (QueueUrlList)
  }
derive instance newtypeListQueuesResult :: Newtype ListQueuesResult _
derive instance repGenericListQueuesResult :: Generic ListQueuesResult _
instance showListQueuesResult :: Show ListQueuesResult where
  show = genericShow
instance decodeListQueuesResult :: Decode ListQueuesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListQueuesResult :: Encode ListQueuesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Amazon SQS message.</p>
newtype Message = Message 
  { "MessageId" :: NullOrUndefined.NullOrUndefined (String)
  , "ReceiptHandle" :: NullOrUndefined.NullOrUndefined (String)
  , "MD5OfBody" :: NullOrUndefined.NullOrUndefined (String)
  , "Body" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (MessageSystemAttributeMap)
  , "MD5OfMessageAttributes" :: NullOrUndefined.NullOrUndefined (String)
  , "MessageAttributes" :: NullOrUndefined.NullOrUndefined (MessageBodyAttributeMap)
  }
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageAttributeName = MessageAttributeName String
derive instance newtypeMessageAttributeName :: Newtype MessageAttributeName _
derive instance repGenericMessageAttributeName :: Generic MessageAttributeName _
instance showMessageAttributeName :: Show MessageAttributeName where
  show = genericShow
instance decodeMessageAttributeName :: Decode MessageAttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageAttributeName :: Encode MessageAttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageAttributeNameList = MessageAttributeNameList (Array MessageAttributeName)
derive instance newtypeMessageAttributeNameList :: Newtype MessageAttributeNameList _
derive instance repGenericMessageAttributeNameList :: Generic MessageAttributeNameList _
instance showMessageAttributeNameList :: Show MessageAttributeNameList where
  show = genericShow
instance decodeMessageAttributeNameList :: Decode MessageAttributeNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageAttributeNameList :: Encode MessageAttributeNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user-specified message attribute value. For string data types, the <code>Value</code> attribute has the same restrictions on the content as the message body. For more information, see <code> <a>SendMessage</a>.</code> </p> <p> <code>Name</code>, <code>type</code>, <code>value</code> and the message body must not be empty or null. All parts of the message attribute, including <code>Name</code>, <code>Type</code>, and <code>Value</code>, are part of the message size restriction (256 KB or 262,144 bytes).</p>
newtype MessageAttributeValue = MessageAttributeValue 
  { "StringValue" :: NullOrUndefined.NullOrUndefined (String)
  , "BinaryValue" :: NullOrUndefined.NullOrUndefined (Binary)
  , "StringListValues" :: NullOrUndefined.NullOrUndefined (StringList)
  , "BinaryListValues" :: NullOrUndefined.NullOrUndefined (BinaryList)
  , "DataType" :: (String)
  }
derive instance newtypeMessageAttributeValue :: Newtype MessageAttributeValue _
derive instance repGenericMessageAttributeValue :: Generic MessageAttributeValue _
instance showMessageAttributeValue :: Show MessageAttributeValue where
  show = genericShow
instance decodeMessageAttributeValue :: Decode MessageAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageAttributeValue :: Encode MessageAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageBodyAttributeMap = MessageBodyAttributeMap (StrMap.StrMap MessageAttributeValue)
derive instance newtypeMessageBodyAttributeMap :: Newtype MessageBodyAttributeMap _
derive instance repGenericMessageBodyAttributeMap :: Generic MessageBodyAttributeMap _
instance showMessageBodyAttributeMap :: Show MessageBodyAttributeMap where
  show = genericShow
instance decodeMessageBodyAttributeMap :: Decode MessageBodyAttributeMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageBodyAttributeMap :: Encode MessageBodyAttributeMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageList = MessageList (Array Message)
derive instance newtypeMessageList :: Newtype MessageList _
derive instance repGenericMessageList :: Generic MessageList _
instance showMessageList :: Show MessageList where
  show = genericShow
instance decodeMessageList :: Decode MessageList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageList :: Encode MessageList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The message referred to isn't in flight.</p>
newtype MessageNotInflight = MessageNotInflight Types.NoArguments
derive instance newtypeMessageNotInflight :: Newtype MessageNotInflight _
derive instance repGenericMessageNotInflight :: Generic MessageNotInflight _
instance showMessageNotInflight :: Show MessageNotInflight where
  show = genericShow
instance decodeMessageNotInflight :: Decode MessageNotInflight where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageNotInflight :: Encode MessageNotInflight where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageSystemAttributeMap = MessageSystemAttributeMap (StrMap.StrMap String)
derive instance newtypeMessageSystemAttributeMap :: Newtype MessageSystemAttributeMap _
derive instance repGenericMessageSystemAttributeMap :: Generic MessageSystemAttributeMap _
instance showMessageSystemAttributeMap :: Show MessageSystemAttributeMap where
  show = genericShow
instance decodeMessageSystemAttributeMap :: Decode MessageSystemAttributeMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageSystemAttributeMap :: Encode MessageSystemAttributeMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageSystemAttributeName = MessageSystemAttributeName String
derive instance newtypeMessageSystemAttributeName :: Newtype MessageSystemAttributeName _
derive instance repGenericMessageSystemAttributeName :: Generic MessageSystemAttributeName _
instance showMessageSystemAttributeName :: Show MessageSystemAttributeName where
  show = genericShow
instance decodeMessageSystemAttributeName :: Decode MessageSystemAttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageSystemAttributeName :: Encode MessageSystemAttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The action that you requested would violate a limit. For example, <code>ReceiveMessage</code> returns this error if the maximum number of inflight messages is reached. <code> <a>AddPermission</a> </code> returns this error if the maximum number of permissions for the queue is reached.</p>
newtype OverLimit = OverLimit Types.NoArguments
derive instance newtypeOverLimit :: Newtype OverLimit _
derive instance repGenericOverLimit :: Generic OverLimit _
instance showOverLimit :: Show OverLimit where
  show = genericShow
instance decodeOverLimit :: Decode OverLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOverLimit :: Encode OverLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that the specified queue previously received a <code>PurgeQueue</code> request within the last 60 seconds (the time it can take to delete the messages in the queue).</p>
newtype PurgeQueueInProgress = PurgeQueueInProgress Types.NoArguments
derive instance newtypePurgeQueueInProgress :: Newtype PurgeQueueInProgress _
derive instance repGenericPurgeQueueInProgress :: Generic PurgeQueueInProgress _
instance showPurgeQueueInProgress :: Show PurgeQueueInProgress where
  show = genericShow
instance decodePurgeQueueInProgress :: Decode PurgeQueueInProgress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurgeQueueInProgress :: Encode PurgeQueueInProgress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype PurgeQueueRequest = PurgeQueueRequest 
  { "QueueUrl" :: (String)
  }
derive instance newtypePurgeQueueRequest :: Newtype PurgeQueueRequest _
derive instance repGenericPurgeQueueRequest :: Generic PurgeQueueRequest _
instance showPurgeQueueRequest :: Show PurgeQueueRequest where
  show = genericShow
instance decodePurgeQueueRequest :: Decode PurgeQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurgeQueueRequest :: Encode PurgeQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueueAttributeMap = QueueAttributeMap (StrMap.StrMap String)
derive instance newtypeQueueAttributeMap :: Newtype QueueAttributeMap _
derive instance repGenericQueueAttributeMap :: Generic QueueAttributeMap _
instance showQueueAttributeMap :: Show QueueAttributeMap where
  show = genericShow
instance decodeQueueAttributeMap :: Decode QueueAttributeMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueAttributeMap :: Encode QueueAttributeMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueueAttributeName = QueueAttributeName String
derive instance newtypeQueueAttributeName :: Newtype QueueAttributeName _
derive instance repGenericQueueAttributeName :: Generic QueueAttributeName _
instance showQueueAttributeName :: Show QueueAttributeName where
  show = genericShow
instance decodeQueueAttributeName :: Decode QueueAttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueAttributeName :: Encode QueueAttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You must wait 60 seconds after deleting a queue before you can create another one with the same name.</p>
newtype QueueDeletedRecently = QueueDeletedRecently Types.NoArguments
derive instance newtypeQueueDeletedRecently :: Newtype QueueDeletedRecently _
derive instance repGenericQueueDeletedRecently :: Generic QueueDeletedRecently _
instance showQueueDeletedRecently :: Show QueueDeletedRecently where
  show = genericShow
instance decodeQueueDeletedRecently :: Decode QueueDeletedRecently where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueDeletedRecently :: Encode QueueDeletedRecently where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The queue referred to doesn't exist.</p>
newtype QueueDoesNotExist = QueueDoesNotExist Types.NoArguments
derive instance newtypeQueueDoesNotExist :: Newtype QueueDoesNotExist _
derive instance repGenericQueueDoesNotExist :: Generic QueueDoesNotExist _
instance showQueueDoesNotExist :: Show QueueDoesNotExist where
  show = genericShow
instance decodeQueueDoesNotExist :: Decode QueueDoesNotExist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueDoesNotExist :: Encode QueueDoesNotExist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A queue already exists with this name. Amazon SQS returns this error only if the request includes attributes whose values differ from those of the existing queue.</p>
newtype QueueNameExists = QueueNameExists Types.NoArguments
derive instance newtypeQueueNameExists :: Newtype QueueNameExists _
derive instance repGenericQueueNameExists :: Generic QueueNameExists _
instance showQueueNameExists :: Show QueueNameExists where
  show = genericShow
instance decodeQueueNameExists :: Decode QueueNameExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueNameExists :: Encode QueueNameExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueueUrlList = QueueUrlList (Array String)
derive instance newtypeQueueUrlList :: Newtype QueueUrlList _
derive instance repGenericQueueUrlList :: Generic QueueUrlList _
instance showQueueUrlList :: Show QueueUrlList where
  show = genericShow
instance decodeQueueUrlList :: Decode QueueUrlList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueUrlList :: Encode QueueUrlList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The receipt handle provided isn't valid.</p>
newtype ReceiptHandleIsInvalid = ReceiptHandleIsInvalid Types.NoArguments
derive instance newtypeReceiptHandleIsInvalid :: Newtype ReceiptHandleIsInvalid _
derive instance repGenericReceiptHandleIsInvalid :: Generic ReceiptHandleIsInvalid _
instance showReceiptHandleIsInvalid :: Show ReceiptHandleIsInvalid where
  show = genericShow
instance decodeReceiptHandleIsInvalid :: Decode ReceiptHandleIsInvalid where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiptHandleIsInvalid :: Encode ReceiptHandleIsInvalid where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ReceiveMessageRequest = ReceiveMessageRequest 
  { "QueueUrl" :: (String)
  , "AttributeNames" :: NullOrUndefined.NullOrUndefined (AttributeNameList)
  , "MessageAttributeNames" :: NullOrUndefined.NullOrUndefined (MessageAttributeNameList)
  , "MaxNumberOfMessages" :: NullOrUndefined.NullOrUndefined (Int)
  , "VisibilityTimeout" :: NullOrUndefined.NullOrUndefined (Int)
  , "WaitTimeSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "ReceiveRequestAttemptId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeReceiveMessageRequest :: Newtype ReceiveMessageRequest _
derive instance repGenericReceiveMessageRequest :: Generic ReceiveMessageRequest _
instance showReceiveMessageRequest :: Show ReceiveMessageRequest where
  show = genericShow
instance decodeReceiveMessageRequest :: Decode ReceiveMessageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiveMessageRequest :: Encode ReceiveMessageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of received messages.</p>
newtype ReceiveMessageResult = ReceiveMessageResult 
  { "Messages" :: NullOrUndefined.NullOrUndefined (MessageList)
  }
derive instance newtypeReceiveMessageResult :: Newtype ReceiveMessageResult _
derive instance repGenericReceiveMessageResult :: Generic ReceiveMessageResult _
instance showReceiveMessageResult :: Show ReceiveMessageResult where
  show = genericShow
instance decodeReceiveMessageResult :: Decode ReceiveMessageResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReceiveMessageResult :: Encode ReceiveMessageResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype RemovePermissionRequest = RemovePermissionRequest 
  { "QueueUrl" :: (String)
  , "Label" :: (String)
  }
derive instance newtypeRemovePermissionRequest :: Newtype RemovePermissionRequest _
derive instance repGenericRemovePermissionRequest :: Generic RemovePermissionRequest _
instance showRemovePermissionRequest :: Show RemovePermissionRequest where
  show = genericShow
instance decodeRemovePermissionRequest :: Decode RemovePermissionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemovePermissionRequest :: Encode RemovePermissionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype SendMessageBatchRequest = SendMessageBatchRequest 
  { "QueueUrl" :: (String)
  , "Entries" :: (SendMessageBatchRequestEntryList)
  }
derive instance newtypeSendMessageBatchRequest :: Newtype SendMessageBatchRequest _
derive instance repGenericSendMessageBatchRequest :: Generic SendMessageBatchRequest _
instance showSendMessageBatchRequest :: Show SendMessageBatchRequest where
  show = genericShow
instance decodeSendMessageBatchRequest :: Decode SendMessageBatchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessageBatchRequest :: Encode SendMessageBatchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the details of a single Amazon SQS message along with an <code>Id</code>.</p>
newtype SendMessageBatchRequestEntry = SendMessageBatchRequestEntry 
  { "Id" :: (String)
  , "MessageBody" :: (String)
  , "DelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "MessageAttributes" :: NullOrUndefined.NullOrUndefined (MessageBodyAttributeMap)
  , "MessageDeduplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "MessageGroupId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSendMessageBatchRequestEntry :: Newtype SendMessageBatchRequestEntry _
derive instance repGenericSendMessageBatchRequestEntry :: Generic SendMessageBatchRequestEntry _
instance showSendMessageBatchRequestEntry :: Show SendMessageBatchRequestEntry where
  show = genericShow
instance decodeSendMessageBatchRequestEntry :: Decode SendMessageBatchRequestEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessageBatchRequestEntry :: Encode SendMessageBatchRequestEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendMessageBatchRequestEntryList = SendMessageBatchRequestEntryList (Array SendMessageBatchRequestEntry)
derive instance newtypeSendMessageBatchRequestEntryList :: Newtype SendMessageBatchRequestEntryList _
derive instance repGenericSendMessageBatchRequestEntryList :: Generic SendMessageBatchRequestEntryList _
instance showSendMessageBatchRequestEntryList :: Show SendMessageBatchRequestEntryList where
  show = genericShow
instance decodeSendMessageBatchRequestEntryList :: Decode SendMessageBatchRequestEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessageBatchRequestEntryList :: Encode SendMessageBatchRequestEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>For each message in the batch, the response contains a <code> <a>SendMessageBatchResultEntry</a> </code> tag if the message succeeds or a <code> <a>BatchResultErrorEntry</a> </code> tag if the message fails.</p>
newtype SendMessageBatchResult = SendMessageBatchResult 
  { "Successful" :: (SendMessageBatchResultEntryList)
  , "Failed" :: (BatchResultErrorEntryList)
  }
derive instance newtypeSendMessageBatchResult :: Newtype SendMessageBatchResult _
derive instance repGenericSendMessageBatchResult :: Generic SendMessageBatchResult _
instance showSendMessageBatchResult :: Show SendMessageBatchResult where
  show = genericShow
instance decodeSendMessageBatchResult :: Decode SendMessageBatchResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessageBatchResult :: Encode SendMessageBatchResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Encloses a <code>MessageId</code> for a successfully-enqueued message in a <code> <a>SendMessageBatch</a>.</code> </p>
newtype SendMessageBatchResultEntry = SendMessageBatchResultEntry 
  { "Id" :: (String)
  , "MessageId" :: (String)
  , "MD5OfMessageBody" :: (String)
  , "MD5OfMessageAttributes" :: NullOrUndefined.NullOrUndefined (String)
  , "SequenceNumber" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSendMessageBatchResultEntry :: Newtype SendMessageBatchResultEntry _
derive instance repGenericSendMessageBatchResultEntry :: Generic SendMessageBatchResultEntry _
instance showSendMessageBatchResultEntry :: Show SendMessageBatchResultEntry where
  show = genericShow
instance decodeSendMessageBatchResultEntry :: Decode SendMessageBatchResultEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessageBatchResultEntry :: Encode SendMessageBatchResultEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendMessageBatchResultEntryList = SendMessageBatchResultEntryList (Array SendMessageBatchResultEntry)
derive instance newtypeSendMessageBatchResultEntryList :: Newtype SendMessageBatchResultEntryList _
derive instance repGenericSendMessageBatchResultEntryList :: Generic SendMessageBatchResultEntryList _
instance showSendMessageBatchResultEntryList :: Show SendMessageBatchResultEntryList where
  show = genericShow
instance decodeSendMessageBatchResultEntryList :: Decode SendMessageBatchResultEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessageBatchResultEntryList :: Encode SendMessageBatchResultEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype SendMessageRequest = SendMessageRequest 
  { "QueueUrl" :: (String)
  , "MessageBody" :: (String)
  , "DelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "MessageAttributes" :: NullOrUndefined.NullOrUndefined (MessageBodyAttributeMap)
  , "MessageDeduplicationId" :: NullOrUndefined.NullOrUndefined (String)
  , "MessageGroupId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSendMessageRequest :: Newtype SendMessageRequest _
derive instance repGenericSendMessageRequest :: Generic SendMessageRequest _
instance showSendMessageRequest :: Show SendMessageRequest where
  show = genericShow
instance decodeSendMessageRequest :: Decode SendMessageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessageRequest :: Encode SendMessageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>MD5OfMessageBody</code> and <code>MessageId</code> elements.</p>
newtype SendMessageResult = SendMessageResult 
  { "MD5OfMessageBody" :: NullOrUndefined.NullOrUndefined (String)
  , "MD5OfMessageAttributes" :: NullOrUndefined.NullOrUndefined (String)
  , "MessageId" :: NullOrUndefined.NullOrUndefined (String)
  , "SequenceNumber" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSendMessageResult :: Newtype SendMessageResult _
derive instance repGenericSendMessageResult :: Generic SendMessageResult _
instance showSendMessageResult :: Show SendMessageResult where
  show = genericShow
instance decodeSendMessageResult :: Decode SendMessageResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendMessageResult :: Encode SendMessageResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype SetQueueAttributesRequest = SetQueueAttributesRequest 
  { "QueueUrl" :: (String)
  , "Attributes" :: (QueueAttributeMap)
  }
derive instance newtypeSetQueueAttributesRequest :: Newtype SetQueueAttributesRequest _
derive instance repGenericSetQueueAttributesRequest :: Generic SetQueueAttributesRequest _
instance showSetQueueAttributesRequest :: Show SetQueueAttributesRequest where
  show = genericShow
instance decodeSetQueueAttributesRequest :: Decode SetQueueAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetQueueAttributesRequest :: Encode SetQueueAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _
derive instance repGenericStringList :: Generic StringList _
instance showStringList :: Show StringList where
  show = genericShow
instance decodeStringList :: Decode StringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringList :: Encode StringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagMap = TagMap (StrMap.StrMap TagValue)
derive instance newtypeTagMap :: Newtype TagMap _
derive instance repGenericTagMap :: Generic TagMap _
instance showTagMap :: Show TagMap where
  show = genericShow
instance decodeTagMap :: Decode TagMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagMap :: Encode TagMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagQueueRequest = TagQueueRequest 
  { "QueueUrl" :: (String)
  , "Tags" :: (TagMap)
  }
derive instance newtypeTagQueueRequest :: Newtype TagQueueRequest _
derive instance repGenericTagQueueRequest :: Generic TagQueueRequest _
instance showTagQueueRequest :: Show TagQueueRequest where
  show = genericShow
instance decodeTagQueueRequest :: Decode TagQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagQueueRequest :: Encode TagQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The batch request contains more entries than permissible.</p>
newtype TooManyEntriesInBatchRequest = TooManyEntriesInBatchRequest Types.NoArguments
derive instance newtypeTooManyEntriesInBatchRequest :: Newtype TooManyEntriesInBatchRequest _
derive instance repGenericTooManyEntriesInBatchRequest :: Generic TooManyEntriesInBatchRequest _
instance showTooManyEntriesInBatchRequest :: Show TooManyEntriesInBatchRequest where
  show = genericShow
instance decodeTooManyEntriesInBatchRequest :: Decode TooManyEntriesInBatchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyEntriesInBatchRequest :: Encode TooManyEntriesInBatchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Error code 400. Unsupported operation.</p>
newtype UnsupportedOperation = UnsupportedOperation Types.NoArguments
derive instance newtypeUnsupportedOperation :: Newtype UnsupportedOperation _
derive instance repGenericUnsupportedOperation :: Generic UnsupportedOperation _
instance showUnsupportedOperation :: Show UnsupportedOperation where
  show = genericShow
instance decodeUnsupportedOperation :: Decode UnsupportedOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedOperation :: Encode UnsupportedOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagQueueRequest = UntagQueueRequest 
  { "QueueUrl" :: (String)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeUntagQueueRequest :: Newtype UntagQueueRequest _
derive instance repGenericUntagQueueRequest :: Generic UntagQueueRequest _
instance showUntagQueueRequest :: Show UntagQueueRequest where
  show = genericShow
instance decodeUntagQueueRequest :: Decode UntagQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagQueueRequest :: Encode UntagQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
