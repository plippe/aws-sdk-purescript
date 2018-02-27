

-- | <p>Amazon Lex provides both build and runtime endpoints. Each endpoint provides a set of operations (API). Your conversational bot uses the runtime API to understand user utterances (user input text or voice). For example, suppose a user says "I want pizza", your bot sends this input to Amazon Lex using the runtime API. Amazon Lex recognizes that the user request is for the OrderPizza intent (one of the intents defined in the bot). Then Amazon Lex engages in user conversation on behalf of the bot to elicit required information (slot values, such as pizza size and crust type), and then performs fulfillment activity (that you configured when you created the bot). You use the build-time API to create and manage your Amazon Lex bot. For a list of build-time operations, see the build-time API, . </p>
module AWS.LexRuntime where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "LexRuntime" :: String


-- | <p> Sends user input (text or speech) to Amazon Lex. Clients use this API to send text and audio requests to Amazon Lex at runtime. Amazon Lex interprets the user input using the machine learning model that it built for the bot. </p> <p>The <code>PostContent</code> operation supports audio input at 8kHz and 16kHz. You can use 8kHz audio to achieve higher speech recognition accuracy in telephone audio applications. </p> <p> In response, Amazon Lex returns the next message to convey to the user. Consider the following example messages: </p> <ul> <li> <p> For a user input "I would like a pizza," Amazon Lex might return a response with a message eliciting slot data (for example, <code>PizzaSize</code>): "What size pizza would you like?". </p> </li> <li> <p> After the user provides all of the pizza order information, Amazon Lex might return a response with a message to get user confirmation: "Order the pizza?". </p> </li> <li> <p> After the user replies "Yes" to the confirmation prompt, Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.". </p> </li> </ul> <p> Not all Amazon Lex messages require a response from the user. For example, conclusion statements do not require a response. Some messages require only a yes or no response. In addition to the <code>message</code>, Amazon Lex provides additional context about the message in the response that you can use to enhance client behavior, such as displaying the appropriate client user interface. Consider the following examples: </p> <ul> <li> <p> If the message is to elicit slot data, Amazon Lex returns the following context information: </p> <ul> <li> <p> <code>x-amz-lex-dialog-state</code> header set to <code>ElicitSlot</code> </p> </li> <li> <p> <code>x-amz-lex-intent-name</code> header set to the intent name in the current context </p> </li> <li> <p> <code>x-amz-lex-slot-to-elicit</code> header set to the slot name for which the <code>message</code> is eliciting information </p> </li> <li> <p> <code>x-amz-lex-slots</code> header set to a map of slots configured for the intent with their current values </p> </li> </ul> </li> <li> <p> If the message is a confirmation prompt, the <code>x-amz-lex-dialog-state</code> header is set to <code>Confirmation</code> and the <code>x-amz-lex-slot-to-elicit</code> header is omitted. </p> </li> <li> <p> If the message is a clarification prompt configured for the intent, indicating that the user intent is not understood, the <code>x-amz-dialog-state</code> header is set to <code>ElicitIntent</code> and the <code>x-amz-slot-to-elicit</code> header is omitted. </p> </li> </ul> <p> In addition, Amazon Lex also returns your application-specific <code>sessionAttributes</code>. For more information, see <a href="http://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html">Managing Conversation Context</a>. </p>
postContent :: forall eff. PostContentRequest -> Aff (err :: AWS.RequestError | eff) PostContentResponse
postContent = AWS.request serviceName "postContent" 


-- | <p>Sends user input (text-only) to Amazon Lex. Client applications can use this API to send requests to Amazon Lex at runtime. Amazon Lex then interprets the user input using the machine learning model it built for the bot. </p> <p> In response, Amazon Lex returns the next <code>message</code> to convey to the user an optional <code>responseCard</code> to display. Consider the following example messages: </p> <ul> <li> <p> For a user input "I would like a pizza", Amazon Lex might return a response with a message eliciting slot data (for example, PizzaSize): "What size pizza would you like?" </p> </li> <li> <p> After the user provides all of the pizza order information, Amazon Lex might return a response with a message to obtain user confirmation "Proceed with the pizza order?". </p> </li> <li> <p> After the user replies to a confirmation prompt with a "yes", Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.". </p> </li> </ul> <p> Not all Amazon Lex messages require a user response. For example, a conclusion statement does not require a response. Some messages require only a "yes" or "no" user response. In addition to the <code>message</code>, Amazon Lex provides additional context about the message in the response that you might use to enhance client behavior, for example, to display the appropriate client user interface. These are the <code>slotToElicit</code>, <code>dialogState</code>, <code>intentName</code>, and <code>slots</code> fields in the response. Consider the following examples: </p> <ul> <li> <p>If the message is to elicit slot data, Amazon Lex returns the following context information:</p> <ul> <li> <p> <code>dialogState</code> set to ElicitSlot </p> </li> <li> <p> <code>intentName</code> set to the intent name in the current context </p> </li> <li> <p> <code>slotToElicit</code> set to the slot name for which the <code>message</code> is eliciting information </p> </li> <li> <p> <code>slots</code> set to a map of slots, configured for the intent, with currently known values </p> </li> </ul> </li> <li> <p> If the message is a confirmation prompt, the <code>dialogState</code> is set to ConfirmIntent and <code>SlotToElicit</code> is set to null. </p> </li> <li> <p>If the message is a clarification prompt (configured for the intent) that indicates that user intent is not understood, the <code>dialogState</code> is set to ElicitIntent and <code>slotToElicit</code> is set to null. </p> </li> </ul> <p> In addition, Amazon Lex also returns your application-specific <code>sessionAttributes</code>. For more information, see <a href="http://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html">Managing Conversation Context</a>. </p>
postText :: forall eff. PostTextRequest -> Aff (err :: AWS.RequestError | eff) PostTextResponse
postText = AWS.request serviceName "postText" 


newtype Accept = Accept String
derive instance newtypeAccept :: Newtype Accept _


newtype AttributesString = AttributesString String
derive instance newtypeAttributesString :: Newtype AttributesString _


-- | <p>Either the Amazon Lex bot is still building, or one of the dependent services (Amazon Polly, AWS Lambda) failed with an internal service error.</p>
newtype BadGatewayException = BadGatewayException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBadGatewayException :: Newtype BadGatewayException _


-- | <p> Request validation failed, there is no usable message in the context, or the bot build failed, is still in progress, or contains unbuilt changes. </p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


newtype BlobStream = BlobStream String
derive instance newtypeBlobStream :: Newtype BlobStream _


newtype BotAlias = BotAlias String
derive instance newtypeBotAlias :: Newtype BotAlias _


newtype BotName = BotName String
derive instance newtypeBotName :: Newtype BotName _


-- | <p>Represents an option to be shown on the client platform (Facebook, Slack, etc.)</p>
newtype Button = Button 
  { "Text'" :: (ButtonTextStringWithLength)
  , "Value'" :: (ButtonValueStringWithLength)
  }
derive instance newtypeButton :: Newtype Button _


newtype ButtonTextStringWithLength = ButtonTextStringWithLength String
derive instance newtypeButtonTextStringWithLength :: Newtype ButtonTextStringWithLength _


newtype ButtonValueStringWithLength = ButtonValueStringWithLength String
derive instance newtypeButtonValueStringWithLength :: Newtype ButtonValueStringWithLength _


-- | <p> Two clients are using the same AWS account, Amazon Lex bot, and user ID. </p>
newtype ConflictException = ConflictException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _


newtype ContentType = ContentType String
derive instance newtypeContentType :: Newtype ContentType _


-- | <p> One of the dependencies, such as AWS Lambda or Amazon Polly, threw an exception. For example, </p> <ul> <li> <p>If Amazon Lex does not have sufficient permissions to call a Lambda function.</p> </li> <li> <p>If a Lambda function takes longer than 30 seconds to execute.</p> </li> <li> <p>If a fulfillment Lambda function returns a <code>Delegate</code> dialog action without removing any slot values.</p> </li> </ul>
newtype DependencyFailedException = DependencyFailedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDependencyFailedException :: Newtype DependencyFailedException _


newtype DialogState = DialogState String
derive instance newtypeDialogState :: Newtype DialogState _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>Represents an option rendered to the user when a prompt is shown. It could be an image, a button, a link, or text. </p>
newtype GenericAttachment = GenericAttachment 
  { "Title'" :: NullOrUndefined (StringWithLength)
  , "SubTitle'" :: NullOrUndefined (StringWithLength)
  , "AttachmentLinkUrl'" :: NullOrUndefined (StringUrlWithLength)
  , "ImageUrl'" :: NullOrUndefined (StringUrlWithLength)
  , "Buttons'" :: NullOrUndefined (ListOfButtons')
  }
derive instance newtypeGenericAttachment :: Newtype GenericAttachment _


newtype HttpContentType = HttpContentType String
derive instance newtypeHttpContentType :: Newtype HttpContentType _


newtype IntentName = IntentName String
derive instance newtypeIntentName :: Newtype IntentName _


-- | <p>Internal service error. Retry the call.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _


-- | <p>Exceeded a limit.</p>
newtype LimitExceededException = LimitExceededException 
  { "RetryAfterSeconds'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


-- | <p>This exception is not used.</p>
newtype LoopDetectedException = LoopDetectedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLoopDetectedException :: Newtype LoopDetectedException _


newtype MessageFormatType = MessageFormatType String
derive instance newtypeMessageFormatType :: Newtype MessageFormatType _


-- | <p>The accept header in the request does not have a valid value.</p>
newtype NotAcceptableException = NotAcceptableException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeNotAcceptableException :: Newtype NotAcceptableException _


-- | <p>The resource (such as the Amazon Lex bot or an alias) that is referred to is not found.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


newtype PostContentRequest = PostContentRequest 
  { "BotName'" :: (BotName)
  , "BotAlias'" :: (BotAlias)
  , "UserId'" :: (UserId)
  , "SessionAttributes'" :: NullOrUndefined (AttributesString)
  , "RequestAttributes'" :: NullOrUndefined (AttributesString)
  , "ContentType'" :: (HttpContentType)
  , "Accept'" :: NullOrUndefined (Accept)
  , "InputStream'" :: (BlobStream)
  }
derive instance newtypePostContentRequest :: Newtype PostContentRequest _


newtype PostContentResponse = PostContentResponse 
  { "ContentType'" :: NullOrUndefined (HttpContentType)
  , "IntentName'" :: NullOrUndefined (IntentName)
  , "Slots'" :: NullOrUndefined (String)
  , "SessionAttributes'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (Text)
  , "MessageFormat'" :: NullOrUndefined (MessageFormatType)
  , "DialogState'" :: NullOrUndefined (DialogState)
  , "SlotToElicit'" :: NullOrUndefined (String)
  , "InputTranscript'" :: NullOrUndefined (String)
  , "AudioStream'" :: NullOrUndefined (BlobStream)
  }
derive instance newtypePostContentResponse :: Newtype PostContentResponse _


newtype PostTextRequest = PostTextRequest 
  { "BotName'" :: (BotName)
  , "BotAlias'" :: (BotAlias)
  , "UserId'" :: (UserId)
  , "SessionAttributes'" :: NullOrUndefined (StringMap)
  , "RequestAttributes'" :: NullOrUndefined (StringMap)
  , "InputText'" :: (Text)
  }
derive instance newtypePostTextRequest :: Newtype PostTextRequest _


newtype PostTextResponse = PostTextResponse 
  { "IntentName'" :: NullOrUndefined (IntentName)
  , "Slots'" :: NullOrUndefined (StringMap)
  , "SessionAttributes'" :: NullOrUndefined (StringMap)
  , "Message'" :: NullOrUndefined (Text)
  , "MessageFormat'" :: NullOrUndefined (MessageFormatType)
  , "DialogState'" :: NullOrUndefined (DialogState)
  , "SlotToElicit'" :: NullOrUndefined (String)
  , "ResponseCard'" :: NullOrUndefined (ResponseCard)
  }
derive instance newtypePostTextResponse :: Newtype PostTextResponse _


-- | <p>The input speech is too long.</p>
newtype RequestTimeoutException = RequestTimeoutException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeRequestTimeoutException :: Newtype RequestTimeoutException _


-- | <p>If you configure a response card when creating your bots, Amazon Lex substitutes the session attributes and slot values that are available, and then returns it. The response card can also come from a Lambda function ( <code>dialogCodeHook</code> and <code>fulfillmentActivity</code> on an intent).</p>
newtype ResponseCard = ResponseCard 
  { "Version'" :: NullOrUndefined (String)
  , "ContentType'" :: NullOrUndefined (ContentType)
  , "GenericAttachments'" :: NullOrUndefined (GenericAttachmentList')
  }
derive instance newtypeResponseCard :: Newtype ResponseCard _


newtype StringMap = StringMap (Map String String)
derive instance newtypeStringMap :: Newtype StringMap _


newtype StringUrlWithLength = StringUrlWithLength String
derive instance newtypeStringUrlWithLength :: Newtype StringUrlWithLength _


newtype StringWithLength = StringWithLength String
derive instance newtypeStringWithLength :: Newtype StringWithLength _


newtype Text = Text String
derive instance newtypeText :: Newtype Text _


-- | <p>The Content-Type header (<code>PostContent</code> API) has an invalid value. </p>
newtype UnsupportedMediaTypeException = UnsupportedMediaTypeException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeUnsupportedMediaTypeException :: Newtype UnsupportedMediaTypeException _


newtype UserId = UserId String
derive instance newtypeUserId :: Newtype UserId _


newtype GenericAttachmentList' = GenericAttachmentList' (Array GenericAttachment)
derive instance newtypeGenericAttachmentList' :: Newtype GenericAttachmentList' _


newtype ListOfButtons' = ListOfButtons' (Array Button)
derive instance newtypeListOfButtons' :: Newtype ListOfButtons' _
