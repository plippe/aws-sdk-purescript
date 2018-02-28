

-- | <p>Amazon Lex provides both build and runtime endpoints. Each endpoint provides a set of operations (API). Your conversational bot uses the runtime API to understand user utterances (user input text or voice). For example, suppose a user says "I want pizza", your bot sends this input to Amazon Lex using the runtime API. Amazon Lex recognizes that the user request is for the OrderPizza intent (one of the intents defined in the bot). Then Amazon Lex engages in user conversation on behalf of the bot to elicit required information (slot values, such as pizza size and crust type), and then performs fulfillment activity (that you configured when you created the bot). You use the build-time API to create and manage your Amazon Lex bot. For a list of build-time operations, see the build-time API, . </p>
module AWS.LexRuntime where

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

serviceName = "LexRuntime" :: String


-- | <p> Sends user input (text or speech) to Amazon Lex. Clients use this API to send text and audio requests to Amazon Lex at runtime. Amazon Lex interprets the user input using the machine learning model that it built for the bot. </p> <p>The <code>PostContent</code> operation supports audio input at 8kHz and 16kHz. You can use 8kHz audio to achieve higher speech recognition accuracy in telephone audio applications. </p> <p> In response, Amazon Lex returns the next message to convey to the user. Consider the following example messages: </p> <ul> <li> <p> For a user input "I would like a pizza," Amazon Lex might return a response with a message eliciting slot data (for example, <code>PizzaSize</code>): "What size pizza would you like?". </p> </li> <li> <p> After the user provides all of the pizza order information, Amazon Lex might return a response with a message to get user confirmation: "Order the pizza?". </p> </li> <li> <p> After the user replies "Yes" to the confirmation prompt, Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.". </p> </li> </ul> <p> Not all Amazon Lex messages require a response from the user. For example, conclusion statements do not require a response. Some messages require only a yes or no response. In addition to the <code>message</code>, Amazon Lex provides additional context about the message in the response that you can use to enhance client behavior, such as displaying the appropriate client user interface. Consider the following examples: </p> <ul> <li> <p> If the message is to elicit slot data, Amazon Lex returns the following context information: </p> <ul> <li> <p> <code>x-amz-lex-dialog-state</code> header set to <code>ElicitSlot</code> </p> </li> <li> <p> <code>x-amz-lex-intent-name</code> header set to the intent name in the current context </p> </li> <li> <p> <code>x-amz-lex-slot-to-elicit</code> header set to the slot name for which the <code>message</code> is eliciting information </p> </li> <li> <p> <code>x-amz-lex-slots</code> header set to a map of slots configured for the intent with their current values </p> </li> </ul> </li> <li> <p> If the message is a confirmation prompt, the <code>x-amz-lex-dialog-state</code> header is set to <code>Confirmation</code> and the <code>x-amz-lex-slot-to-elicit</code> header is omitted. </p> </li> <li> <p> If the message is a clarification prompt configured for the intent, indicating that the user intent is not understood, the <code>x-amz-dialog-state</code> header is set to <code>ElicitIntent</code> and the <code>x-amz-slot-to-elicit</code> header is omitted. </p> </li> </ul> <p> In addition, Amazon Lex also returns your application-specific <code>sessionAttributes</code>. For more information, see <a href="http://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html">Managing Conversation Context</a>. </p>
postContent :: forall eff. PostContentRequest -> Aff (exception :: EXCEPTION | eff) PostContentResponse
postContent = Request.request serviceName "postContent" 


-- | <p>Sends user input (text-only) to Amazon Lex. Client applications can use this API to send requests to Amazon Lex at runtime. Amazon Lex then interprets the user input using the machine learning model it built for the bot. </p> <p> In response, Amazon Lex returns the next <code>message</code> to convey to the user an optional <code>responseCard</code> to display. Consider the following example messages: </p> <ul> <li> <p> For a user input "I would like a pizza", Amazon Lex might return a response with a message eliciting slot data (for example, PizzaSize): "What size pizza would you like?" </p> </li> <li> <p> After the user provides all of the pizza order information, Amazon Lex might return a response with a message to obtain user confirmation "Proceed with the pizza order?". </p> </li> <li> <p> After the user replies to a confirmation prompt with a "yes", Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.". </p> </li> </ul> <p> Not all Amazon Lex messages require a user response. For example, a conclusion statement does not require a response. Some messages require only a "yes" or "no" user response. In addition to the <code>message</code>, Amazon Lex provides additional context about the message in the response that you might use to enhance client behavior, for example, to display the appropriate client user interface. These are the <code>slotToElicit</code>, <code>dialogState</code>, <code>intentName</code>, and <code>slots</code> fields in the response. Consider the following examples: </p> <ul> <li> <p>If the message is to elicit slot data, Amazon Lex returns the following context information:</p> <ul> <li> <p> <code>dialogState</code> set to ElicitSlot </p> </li> <li> <p> <code>intentName</code> set to the intent name in the current context </p> </li> <li> <p> <code>slotToElicit</code> set to the slot name for which the <code>message</code> is eliciting information </p> </li> <li> <p> <code>slots</code> set to a map of slots, configured for the intent, with currently known values </p> </li> </ul> </li> <li> <p> If the message is a confirmation prompt, the <code>dialogState</code> is set to ConfirmIntent and <code>SlotToElicit</code> is set to null. </p> </li> <li> <p>If the message is a clarification prompt (configured for the intent) that indicates that user intent is not understood, the <code>dialogState</code> is set to ElicitIntent and <code>slotToElicit</code> is set to null. </p> </li> </ul> <p> In addition, Amazon Lex also returns your application-specific <code>sessionAttributes</code>. For more information, see <a href="http://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html">Managing Conversation Context</a>. </p>
postText :: forall eff. PostTextRequest -> Aff (exception :: EXCEPTION | eff) PostTextResponse
postText = Request.request serviceName "postText" 


newtype Accept = Accept String
derive instance newtypeAccept :: Newtype Accept _
derive instance repGenericAccept :: Generic Accept _
instance showAccept :: Show Accept where
  show = genericShow
instance decodeAccept :: Decode Accept where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccept :: Encode Accept where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributesString = AttributesString String
derive instance newtypeAttributesString :: Newtype AttributesString _
derive instance repGenericAttributesString :: Generic AttributesString _
instance showAttributesString :: Show AttributesString where
  show = genericShow
instance decodeAttributesString :: Decode AttributesString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributesString :: Encode AttributesString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Either the Amazon Lex bot is still building, or one of the dependent services (Amazon Polly, AWS Lambda) failed with an internal service error.</p>
newtype BadGatewayException = BadGatewayException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBadGatewayException :: Newtype BadGatewayException _
derive instance repGenericBadGatewayException :: Generic BadGatewayException _
instance showBadGatewayException :: Show BadGatewayException where
  show = genericShow
instance decodeBadGatewayException :: Decode BadGatewayException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadGatewayException :: Encode BadGatewayException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request validation failed, there is no usable message in the context, or the bot build failed, is still in progress, or contains unbuilt changes. </p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BlobStream = BlobStream String
derive instance newtypeBlobStream :: Newtype BlobStream _
derive instance repGenericBlobStream :: Generic BlobStream _
instance showBlobStream :: Show BlobStream where
  show = genericShow
instance decodeBlobStream :: Decode BlobStream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlobStream :: Encode BlobStream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BotAlias = BotAlias String
derive instance newtypeBotAlias :: Newtype BotAlias _
derive instance repGenericBotAlias :: Generic BotAlias _
instance showBotAlias :: Show BotAlias where
  show = genericShow
instance decodeBotAlias :: Decode BotAlias where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotAlias :: Encode BotAlias where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BotName = BotName String
derive instance newtypeBotName :: Newtype BotName _
derive instance repGenericBotName :: Generic BotName _
instance showBotName :: Show BotName where
  show = genericShow
instance decodeBotName :: Decode BotName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBotName :: Encode BotName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an option to be shown on the client platform (Facebook, Slack, etc.)</p>
newtype Button = Button 
  { "Text'" :: (ButtonTextStringWithLength)
  , "Value'" :: (ButtonValueStringWithLength)
  }
derive instance newtypeButton :: Newtype Button _
derive instance repGenericButton :: Generic Button _
instance showButton :: Show Button where
  show = genericShow
instance decodeButton :: Decode Button where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeButton :: Encode Button where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ButtonTextStringWithLength = ButtonTextStringWithLength String
derive instance newtypeButtonTextStringWithLength :: Newtype ButtonTextStringWithLength _
derive instance repGenericButtonTextStringWithLength :: Generic ButtonTextStringWithLength _
instance showButtonTextStringWithLength :: Show ButtonTextStringWithLength where
  show = genericShow
instance decodeButtonTextStringWithLength :: Decode ButtonTextStringWithLength where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeButtonTextStringWithLength :: Encode ButtonTextStringWithLength where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ButtonValueStringWithLength = ButtonValueStringWithLength String
derive instance newtypeButtonValueStringWithLength :: Newtype ButtonValueStringWithLength _
derive instance repGenericButtonValueStringWithLength :: Generic ButtonValueStringWithLength _
instance showButtonValueStringWithLength :: Show ButtonValueStringWithLength where
  show = genericShow
instance decodeButtonValueStringWithLength :: Decode ButtonValueStringWithLength where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeButtonValueStringWithLength :: Encode ButtonValueStringWithLength where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Two clients are using the same AWS account, Amazon Lex bot, and user ID. </p>
newtype ConflictException = ConflictException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _
derive instance repGenericConflictException :: Generic ConflictException _
instance showConflictException :: Show ConflictException where
  show = genericShow
instance decodeConflictException :: Decode ConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConflictException :: Encode ConflictException where
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


-- | <p> One of the dependencies, such as AWS Lambda or Amazon Polly, threw an exception. For example, </p> <ul> <li> <p>If Amazon Lex does not have sufficient permissions to call a Lambda function.</p> </li> <li> <p>If a Lambda function takes longer than 30 seconds to execute.</p> </li> <li> <p>If a fulfillment Lambda function returns a <code>Delegate</code> dialog action without removing any slot values.</p> </li> </ul>
newtype DependencyFailedException = DependencyFailedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDependencyFailedException :: Newtype DependencyFailedException _
derive instance repGenericDependencyFailedException :: Generic DependencyFailedException _
instance showDependencyFailedException :: Show DependencyFailedException where
  show = genericShow
instance decodeDependencyFailedException :: Decode DependencyFailedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDependencyFailedException :: Encode DependencyFailedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DialogState = DialogState String
derive instance newtypeDialogState :: Newtype DialogState _
derive instance repGenericDialogState :: Generic DialogState _
instance showDialogState :: Show DialogState where
  show = genericShow
instance decodeDialogState :: Decode DialogState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDialogState :: Encode DialogState where
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


-- | <p>Represents an option rendered to the user when a prompt is shown. It could be an image, a button, a link, or text. </p>
newtype GenericAttachment = GenericAttachment 
  { "Title'" :: NullOrUndefined.NullOrUndefined (StringWithLength)
  , "SubTitle'" :: NullOrUndefined.NullOrUndefined (StringWithLength)
  , "AttachmentLinkUrl'" :: NullOrUndefined.NullOrUndefined (StringUrlWithLength)
  , "ImageUrl'" :: NullOrUndefined.NullOrUndefined (StringUrlWithLength)
  , "Buttons'" :: NullOrUndefined.NullOrUndefined (ListOfButtons')
  }
derive instance newtypeGenericAttachment :: Newtype GenericAttachment _
derive instance repGenericGenericAttachment :: Generic GenericAttachment _
instance showGenericAttachment :: Show GenericAttachment where
  show = genericShow
instance decodeGenericAttachment :: Decode GenericAttachment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenericAttachment :: Encode GenericAttachment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HttpContentType = HttpContentType String
derive instance newtypeHttpContentType :: Newtype HttpContentType _
derive instance repGenericHttpContentType :: Generic HttpContentType _
instance showHttpContentType :: Show HttpContentType where
  show = genericShow
instance decodeHttpContentType :: Decode HttpContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHttpContentType :: Encode HttpContentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntentName = IntentName String
derive instance newtypeIntentName :: Newtype IntentName _
derive instance repGenericIntentName :: Generic IntentName _
instance showIntentName :: Show IntentName where
  show = genericShow
instance decodeIntentName :: Decode IntentName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntentName :: Encode IntentName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Internal service error. Retry the call.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _
derive instance repGenericInternalFailureException :: Generic InternalFailureException _
instance showInternalFailureException :: Show InternalFailureException where
  show = genericShow
instance decodeInternalFailureException :: Decode InternalFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalFailureException :: Encode InternalFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Exceeded a limit.</p>
newtype LimitExceededException = LimitExceededException 
  { "RetryAfterSeconds'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is not used.</p>
newtype LoopDetectedException = LoopDetectedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLoopDetectedException :: Newtype LoopDetectedException _
derive instance repGenericLoopDetectedException :: Generic LoopDetectedException _
instance showLoopDetectedException :: Show LoopDetectedException where
  show = genericShow
instance decodeLoopDetectedException :: Decode LoopDetectedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoopDetectedException :: Encode LoopDetectedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageFormatType = MessageFormatType String
derive instance newtypeMessageFormatType :: Newtype MessageFormatType _
derive instance repGenericMessageFormatType :: Generic MessageFormatType _
instance showMessageFormatType :: Show MessageFormatType where
  show = genericShow
instance decodeMessageFormatType :: Decode MessageFormatType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageFormatType :: Encode MessageFormatType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The accept header in the request does not have a valid value.</p>
newtype NotAcceptableException = NotAcceptableException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotAcceptableException :: Newtype NotAcceptableException _
derive instance repGenericNotAcceptableException :: Generic NotAcceptableException _
instance showNotAcceptableException :: Show NotAcceptableException where
  show = genericShow
instance decodeNotAcceptableException :: Decode NotAcceptableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotAcceptableException :: Encode NotAcceptableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource (such as the Amazon Lex bot or an alias) that is referred to is not found.</p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostContentRequest = PostContentRequest 
  { "BotName'" :: (BotName)
  , "BotAlias'" :: (BotAlias)
  , "UserId'" :: (UserId)
  , "SessionAttributes'" :: NullOrUndefined.NullOrUndefined (AttributesString)
  , "RequestAttributes'" :: NullOrUndefined.NullOrUndefined (AttributesString)
  , "ContentType'" :: (HttpContentType)
  , "Accept'" :: NullOrUndefined.NullOrUndefined (Accept)
  , "InputStream'" :: (BlobStream)
  }
derive instance newtypePostContentRequest :: Newtype PostContentRequest _
derive instance repGenericPostContentRequest :: Generic PostContentRequest _
instance showPostContentRequest :: Show PostContentRequest where
  show = genericShow
instance decodePostContentRequest :: Decode PostContentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostContentRequest :: Encode PostContentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostContentResponse = PostContentResponse 
  { "ContentType'" :: NullOrUndefined.NullOrUndefined (HttpContentType)
  , "IntentName'" :: NullOrUndefined.NullOrUndefined (IntentName)
  , "Slots'" :: NullOrUndefined.NullOrUndefined (String)
  , "SessionAttributes'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Text)
  , "MessageFormat'" :: NullOrUndefined.NullOrUndefined (MessageFormatType)
  , "DialogState'" :: NullOrUndefined.NullOrUndefined (DialogState)
  , "SlotToElicit'" :: NullOrUndefined.NullOrUndefined (String)
  , "InputTranscript'" :: NullOrUndefined.NullOrUndefined (String)
  , "AudioStream'" :: NullOrUndefined.NullOrUndefined (BlobStream)
  }
derive instance newtypePostContentResponse :: Newtype PostContentResponse _
derive instance repGenericPostContentResponse :: Generic PostContentResponse _
instance showPostContentResponse :: Show PostContentResponse where
  show = genericShow
instance decodePostContentResponse :: Decode PostContentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostContentResponse :: Encode PostContentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostTextRequest = PostTextRequest 
  { "BotName'" :: (BotName)
  , "BotAlias'" :: (BotAlias)
  , "UserId'" :: (UserId)
  , "SessionAttributes'" :: NullOrUndefined.NullOrUndefined (StringMap)
  , "RequestAttributes'" :: NullOrUndefined.NullOrUndefined (StringMap)
  , "InputText'" :: (Text)
  }
derive instance newtypePostTextRequest :: Newtype PostTextRequest _
derive instance repGenericPostTextRequest :: Generic PostTextRequest _
instance showPostTextRequest :: Show PostTextRequest where
  show = genericShow
instance decodePostTextRequest :: Decode PostTextRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostTextRequest :: Encode PostTextRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PostTextResponse = PostTextResponse 
  { "IntentName'" :: NullOrUndefined.NullOrUndefined (IntentName)
  , "Slots'" :: NullOrUndefined.NullOrUndefined (StringMap)
  , "SessionAttributes'" :: NullOrUndefined.NullOrUndefined (StringMap)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Text)
  , "MessageFormat'" :: NullOrUndefined.NullOrUndefined (MessageFormatType)
  , "DialogState'" :: NullOrUndefined.NullOrUndefined (DialogState)
  , "SlotToElicit'" :: NullOrUndefined.NullOrUndefined (String)
  , "ResponseCard'" :: NullOrUndefined.NullOrUndefined (ResponseCard)
  }
derive instance newtypePostTextResponse :: Newtype PostTextResponse _
derive instance repGenericPostTextResponse :: Generic PostTextResponse _
instance showPostTextResponse :: Show PostTextResponse where
  show = genericShow
instance decodePostTextResponse :: Decode PostTextResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePostTextResponse :: Encode PostTextResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input speech is too long.</p>
newtype RequestTimeoutException = RequestTimeoutException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRequestTimeoutException :: Newtype RequestTimeoutException _
derive instance repGenericRequestTimeoutException :: Generic RequestTimeoutException _
instance showRequestTimeoutException :: Show RequestTimeoutException where
  show = genericShow
instance decodeRequestTimeoutException :: Decode RequestTimeoutException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestTimeoutException :: Encode RequestTimeoutException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>If you configure a response card when creating your bots, Amazon Lex substitutes the session attributes and slot values that are available, and then returns it. The response card can also come from a Lambda function ( <code>dialogCodeHook</code> and <code>fulfillmentActivity</code> on an intent).</p>
newtype ResponseCard = ResponseCard 
  { "Version'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContentType'" :: NullOrUndefined.NullOrUndefined (ContentType)
  , "GenericAttachments'" :: NullOrUndefined.NullOrUndefined (GenericAttachmentList')
  }
derive instance newtypeResponseCard :: Newtype ResponseCard _
derive instance repGenericResponseCard :: Generic ResponseCard _
instance showResponseCard :: Show ResponseCard where
  show = genericShow
instance decodeResponseCard :: Decode ResponseCard where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResponseCard :: Encode ResponseCard where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringMap = StringMap (StrMap.StrMap String)
derive instance newtypeStringMap :: Newtype StringMap _
derive instance repGenericStringMap :: Generic StringMap _
instance showStringMap :: Show StringMap where
  show = genericShow
instance decodeStringMap :: Decode StringMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringMap :: Encode StringMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringUrlWithLength = StringUrlWithLength String
derive instance newtypeStringUrlWithLength :: Newtype StringUrlWithLength _
derive instance repGenericStringUrlWithLength :: Generic StringUrlWithLength _
instance showStringUrlWithLength :: Show StringUrlWithLength where
  show = genericShow
instance decodeStringUrlWithLength :: Decode StringUrlWithLength where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringUrlWithLength :: Encode StringUrlWithLength where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringWithLength = StringWithLength String
derive instance newtypeStringWithLength :: Newtype StringWithLength _
derive instance repGenericStringWithLength :: Generic StringWithLength _
instance showStringWithLength :: Show StringWithLength where
  show = genericShow
instance decodeStringWithLength :: Decode StringWithLength where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringWithLength :: Encode StringWithLength where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Text = Text String
derive instance newtypeText :: Newtype Text _
derive instance repGenericText :: Generic Text _
instance showText :: Show Text where
  show = genericShow
instance decodeText :: Decode Text where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeText :: Encode Text where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Content-Type header (<code>PostContent</code> API) has an invalid value. </p>
newtype UnsupportedMediaTypeException = UnsupportedMediaTypeException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnsupportedMediaTypeException :: Newtype UnsupportedMediaTypeException _
derive instance repGenericUnsupportedMediaTypeException :: Generic UnsupportedMediaTypeException _
instance showUnsupportedMediaTypeException :: Show UnsupportedMediaTypeException where
  show = genericShow
instance decodeUnsupportedMediaTypeException :: Decode UnsupportedMediaTypeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedMediaTypeException :: Encode UnsupportedMediaTypeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserId = UserId String
derive instance newtypeUserId :: Newtype UserId _
derive instance repGenericUserId :: Generic UserId _
instance showUserId :: Show UserId where
  show = genericShow
instance decodeUserId :: Decode UserId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserId :: Encode UserId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenericAttachmentList' = GenericAttachmentList' (Array GenericAttachment)
derive instance newtypeGenericAttachmentList' :: Newtype GenericAttachmentList' _
derive instance repGenericGenericAttachmentList' :: Generic GenericAttachmentList' _
instance showGenericAttachmentList' :: Show GenericAttachmentList' where
  show = genericShow
instance decodeGenericAttachmentList' :: Decode GenericAttachmentList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenericAttachmentList' :: Encode GenericAttachmentList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfButtons' = ListOfButtons' (Array Button)
derive instance newtypeListOfButtons' :: Newtype ListOfButtons' _
derive instance repGenericListOfButtons' :: Generic ListOfButtons' _
instance showListOfButtons' :: Show ListOfButtons' where
  show = genericShow
instance decodeListOfButtons' :: Decode ListOfButtons' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfButtons' :: Encode ListOfButtons' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
