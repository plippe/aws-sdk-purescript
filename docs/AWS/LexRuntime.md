## Module AWS.LexRuntime

<p>Amazon Lex provides both build and runtime endpoints. Each endpoint provides a set of operations (API). Your conversational bot uses the runtime API to understand user utterances (user input text or voice). For example, suppose a user says "I want pizza", your bot sends this input to Amazon Lex using the runtime API. Amazon Lex recognizes that the user request is for the OrderPizza intent (one of the intents defined in the bot). Then Amazon Lex engages in user conversation on behalf of the bot to elicit required information (slot values, such as pizza size and crust type), and then performs fulfillment activity (that you configured when you created the bot). You use the build-time API to create and manage your Amazon Lex bot. For a list of build-time operations, see the build-time API, . </p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `postContent`

``` purescript
postContent :: forall eff. PostContentRequest -> Aff (err :: RequestError | eff) PostContentResponse
```

<p> Sends user input (text or speech) to Amazon Lex. Clients use this API to send text and audio requests to Amazon Lex at runtime. Amazon Lex interprets the user input using the machine learning model that it built for the bot. </p> <p>The <code>PostContent</code> operation supports audio input at 8kHz and 16kHz. You can use 8kHz audio to achieve higher speech recognition accuracy in telephone audio applications. </p> <p> In response, Amazon Lex returns the next message to convey to the user. Consider the following example messages: </p> <ul> <li> <p> For a user input "I would like a pizza," Amazon Lex might return a response with a message eliciting slot data (for example, <code>PizzaSize</code>): "What size pizza would you like?". </p> </li> <li> <p> After the user provides all of the pizza order information, Amazon Lex might return a response with a message to get user confirmation: "Order the pizza?". </p> </li> <li> <p> After the user replies "Yes" to the confirmation prompt, Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.". </p> </li> </ul> <p> Not all Amazon Lex messages require a response from the user. For example, conclusion statements do not require a response. Some messages require only a yes or no response. In addition to the <code>message</code>, Amazon Lex provides additional context about the message in the response that you can use to enhance client behavior, such as displaying the appropriate client user interface. Consider the following examples: </p> <ul> <li> <p> If the message is to elicit slot data, Amazon Lex returns the following context information: </p> <ul> <li> <p> <code>x-amz-lex-dialog-state</code> header set to <code>ElicitSlot</code> </p> </li> <li> <p> <code>x-amz-lex-intent-name</code> header set to the intent name in the current context </p> </li> <li> <p> <code>x-amz-lex-slot-to-elicit</code> header set to the slot name for which the <code>message</code> is eliciting information </p> </li> <li> <p> <code>x-amz-lex-slots</code> header set to a map of slots configured for the intent with their current values </p> </li> </ul> </li> <li> <p> If the message is a confirmation prompt, the <code>x-amz-lex-dialog-state</code> header is set to <code>Confirmation</code> and the <code>x-amz-lex-slot-to-elicit</code> header is omitted. </p> </li> <li> <p> If the message is a clarification prompt configured for the intent, indicating that the user intent is not understood, the <code>x-amz-dialog-state</code> header is set to <code>ElicitIntent</code> and the <code>x-amz-slot-to-elicit</code> header is omitted. </p> </li> </ul> <p> In addition, Amazon Lex also returns your application-specific <code>sessionAttributes</code>. For more information, see <a href="http://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html">Managing Conversation Context</a>. </p>

#### `postText`

``` purescript
postText :: forall eff. PostTextRequest -> Aff (err :: RequestError | eff) PostTextResponse
```

<p>Sends user input (text-only) to Amazon Lex. Client applications can use this API to send requests to Amazon Lex at runtime. Amazon Lex then interprets the user input using the machine learning model it built for the bot. </p> <p> In response, Amazon Lex returns the next <code>message</code> to convey to the user an optional <code>responseCard</code> to display. Consider the following example messages: </p> <ul> <li> <p> For a user input "I would like a pizza", Amazon Lex might return a response with a message eliciting slot data (for example, PizzaSize): "What size pizza would you like?" </p> </li> <li> <p> After the user provides all of the pizza order information, Amazon Lex might return a response with a message to obtain user confirmation "Proceed with the pizza order?". </p> </li> <li> <p> After the user replies to a confirmation prompt with a "yes", Amazon Lex might return a conclusion statement: "Thank you, your cheese pizza has been ordered.". </p> </li> </ul> <p> Not all Amazon Lex messages require a user response. For example, a conclusion statement does not require a response. Some messages require only a "yes" or "no" user response. In addition to the <code>message</code>, Amazon Lex provides additional context about the message in the response that you might use to enhance client behavior, for example, to display the appropriate client user interface. These are the <code>slotToElicit</code>, <code>dialogState</code>, <code>intentName</code>, and <code>slots</code> fields in the response. Consider the following examples: </p> <ul> <li> <p>If the message is to elicit slot data, Amazon Lex returns the following context information:</p> <ul> <li> <p> <code>dialogState</code> set to ElicitSlot </p> </li> <li> <p> <code>intentName</code> set to the intent name in the current context </p> </li> <li> <p> <code>slotToElicit</code> set to the slot name for which the <code>message</code> is eliciting information </p> </li> <li> <p> <code>slots</code> set to a map of slots, configured for the intent, with currently known values </p> </li> </ul> </li> <li> <p> If the message is a confirmation prompt, the <code>dialogState</code> is set to ConfirmIntent and <code>SlotToElicit</code> is set to null. </p> </li> <li> <p>If the message is a clarification prompt (configured for the intent) that indicates that user intent is not understood, the <code>dialogState</code> is set to ElicitIntent and <code>slotToElicit</code> is set to null. </p> </li> </ul> <p> In addition, Amazon Lex also returns your application-specific <code>sessionAttributes</code>. For more information, see <a href="http://docs.aws.amazon.com/lex/latest/dg/context-mgmt.html">Managing Conversation Context</a>. </p>

#### `Accept`

``` purescript
newtype Accept
  = Accept String
```

##### Instances
``` purescript
Newtype Accept _
```

#### `AttributesString`

``` purescript
newtype AttributesString
  = AttributesString String
```

##### Instances
``` purescript
Newtype AttributesString _
```

#### `BadGatewayException`

``` purescript
newtype BadGatewayException
  = BadGatewayException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Either the Amazon Lex bot is still building, or one of the dependent services (Amazon Polly, AWS Lambda) failed with an internal service error.</p>

##### Instances
``` purescript
Newtype BadGatewayException _
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message'" :: NullOrUndefined (String) }
```

<p> Request validation failed, there is no usable message in the context, or the bot build failed, is still in progress, or contains unbuilt changes. </p>

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `BlobStream`

``` purescript
newtype BlobStream
  = BlobStream String
```

##### Instances
``` purescript
Newtype BlobStream _
```

#### `BotAlias`

``` purescript
newtype BotAlias
  = BotAlias String
```

##### Instances
``` purescript
Newtype BotAlias _
```

#### `BotName`

``` purescript
newtype BotName
  = BotName String
```

##### Instances
``` purescript
Newtype BotName _
```

#### `Button`

``` purescript
newtype Button
  = Button { "Text'" :: ButtonTextStringWithLength, "Value'" :: ButtonValueStringWithLength }
```

<p>Represents an option to be shown on the client platform (Facebook, Slack, etc.)</p>

##### Instances
``` purescript
Newtype Button _
```

#### `ButtonTextStringWithLength`

``` purescript
newtype ButtonTextStringWithLength
  = ButtonTextStringWithLength String
```

##### Instances
``` purescript
Newtype ButtonTextStringWithLength _
```

#### `ButtonValueStringWithLength`

``` purescript
newtype ButtonValueStringWithLength
  = ButtonValueStringWithLength String
```

##### Instances
``` purescript
Newtype ButtonValueStringWithLength _
```

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "Message'" :: NullOrUndefined (String) }
```

<p> Two clients are using the same AWS account, Amazon Lex bot, and user ID. </p>

##### Instances
``` purescript
Newtype ConflictException _
```

#### `ContentType`

``` purescript
newtype ContentType
  = ContentType String
```

##### Instances
``` purescript
Newtype ContentType _
```

#### `DependencyFailedException`

``` purescript
newtype DependencyFailedException
  = DependencyFailedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p> One of the dependencies, such as AWS Lambda or Amazon Polly, threw an exception. For example, </p> <ul> <li> <p>If Amazon Lex does not have sufficient permissions to call a Lambda function.</p> </li> <li> <p>If a Lambda function takes longer than 30 seconds to execute.</p> </li> <li> <p>If a fulfillment Lambda function returns a <code>Delegate</code> dialog action without removing any slot values.</p> </li> </ul>

##### Instances
``` purescript
Newtype DependencyFailedException _
```

#### `DialogState`

``` purescript
newtype DialogState
  = DialogState String
```

##### Instances
``` purescript
Newtype DialogState _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `GenericAttachment`

``` purescript
newtype GenericAttachment
  = GenericAttachment { "Title'" :: NullOrUndefined (StringWithLength), "SubTitle'" :: NullOrUndefined (StringWithLength), "AttachmentLinkUrl'" :: NullOrUndefined (StringUrlWithLength), "ImageUrl'" :: NullOrUndefined (StringUrlWithLength), "Buttons'" :: NullOrUndefined (ListOfButtons') }
```

<p>Represents an option rendered to the user when a prompt is shown. It could be an image, a button, a link, or text. </p>

##### Instances
``` purescript
Newtype GenericAttachment _
```

#### `HttpContentType`

``` purescript
newtype HttpContentType
  = HttpContentType String
```

##### Instances
``` purescript
Newtype HttpContentType _
```

#### `IntentName`

``` purescript
newtype IntentName
  = IntentName String
```

##### Instances
``` purescript
Newtype IntentName _
```

#### `InternalFailureException`

``` purescript
newtype InternalFailureException
  = InternalFailureException { "Message'" :: NullOrUndefined (String) }
```

<p>Internal service error. Retry the call.</p>

##### Instances
``` purescript
Newtype InternalFailureException _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "RetryAfterSeconds'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String) }
```

<p>Exceeded a limit.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `LoopDetectedException`

``` purescript
newtype LoopDetectedException
  = LoopDetectedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>This exception is not used.</p>

##### Instances
``` purescript
Newtype LoopDetectedException _
```

#### `MessageFormatType`

``` purescript
newtype MessageFormatType
  = MessageFormatType String
```

##### Instances
``` purescript
Newtype MessageFormatType _
```

#### `NotAcceptableException`

``` purescript
newtype NotAcceptableException
  = NotAcceptableException { "Message'" :: NullOrUndefined (String) }
```

<p>The accept header in the request does not have a valid value.</p>

##### Instances
``` purescript
Newtype NotAcceptableException _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message'" :: NullOrUndefined (String) }
```

<p>The resource (such as the Amazon Lex bot or an alias) that is referred to is not found.</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `PostContentRequest`

``` purescript
newtype PostContentRequest
  = PostContentRequest { "BotName'" :: BotName, "BotAlias'" :: BotAlias, "UserId'" :: UserId, "SessionAttributes'" :: NullOrUndefined (AttributesString), "RequestAttributes'" :: NullOrUndefined (AttributesString), "ContentType'" :: HttpContentType, "Accept'" :: NullOrUndefined (Accept), "InputStream'" :: BlobStream }
```

##### Instances
``` purescript
Newtype PostContentRequest _
```

#### `PostContentResponse`

``` purescript
newtype PostContentResponse
  = PostContentResponse { "ContentType'" :: NullOrUndefined (HttpContentType), "IntentName'" :: NullOrUndefined (IntentName), "Slots'" :: NullOrUndefined (String), "SessionAttributes'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (Text), "MessageFormat'" :: NullOrUndefined (MessageFormatType), "DialogState'" :: NullOrUndefined (DialogState), "SlotToElicit'" :: NullOrUndefined (String), "InputTranscript'" :: NullOrUndefined (String), "AudioStream'" :: NullOrUndefined (BlobStream) }
```

##### Instances
``` purescript
Newtype PostContentResponse _
```

#### `PostTextRequest`

``` purescript
newtype PostTextRequest
  = PostTextRequest { "BotName'" :: BotName, "BotAlias'" :: BotAlias, "UserId'" :: UserId, "SessionAttributes'" :: NullOrUndefined (StringMap), "RequestAttributes'" :: NullOrUndefined (StringMap), "InputText'" :: Text }
```

##### Instances
``` purescript
Newtype PostTextRequest _
```

#### `PostTextResponse`

``` purescript
newtype PostTextResponse
  = PostTextResponse { "IntentName'" :: NullOrUndefined (IntentName), "Slots'" :: NullOrUndefined (StringMap), "SessionAttributes'" :: NullOrUndefined (StringMap), "Message'" :: NullOrUndefined (Text), "MessageFormat'" :: NullOrUndefined (MessageFormatType), "DialogState'" :: NullOrUndefined (DialogState), "SlotToElicit'" :: NullOrUndefined (String), "ResponseCard'" :: NullOrUndefined (ResponseCard) }
```

##### Instances
``` purescript
Newtype PostTextResponse _
```

#### `RequestTimeoutException`

``` purescript
newtype RequestTimeoutException
  = RequestTimeoutException { "Message'" :: NullOrUndefined (String) }
```

<p>The input speech is too long.</p>

##### Instances
``` purescript
Newtype RequestTimeoutException _
```

#### `ResponseCard`

``` purescript
newtype ResponseCard
  = ResponseCard { "Version'" :: NullOrUndefined (String), "ContentType'" :: NullOrUndefined (ContentType), "GenericAttachments'" :: NullOrUndefined (GenericAttachmentList') }
```

<p>If you configure a response card when creating your bots, Amazon Lex substitutes the session attributes and slot values that are available, and then returns it. The response card can also come from a Lambda function ( <code>dialogCodeHook</code> and <code>fulfillmentActivity</code> on an intent).</p>

##### Instances
``` purescript
Newtype ResponseCard _
```

#### `StringMap`

``` purescript
newtype StringMap
  = StringMap (Map String String)
```

##### Instances
``` purescript
Newtype StringMap _
```

#### `StringUrlWithLength`

``` purescript
newtype StringUrlWithLength
  = StringUrlWithLength String
```

##### Instances
``` purescript
Newtype StringUrlWithLength _
```

#### `StringWithLength`

``` purescript
newtype StringWithLength
  = StringWithLength String
```

##### Instances
``` purescript
Newtype StringWithLength _
```

#### `Text`

``` purescript
newtype Text
  = Text String
```

##### Instances
``` purescript
Newtype Text _
```

#### `UnsupportedMediaTypeException`

``` purescript
newtype UnsupportedMediaTypeException
  = UnsupportedMediaTypeException { "Message'" :: NullOrUndefined (String) }
```

<p>The Content-Type header (<code>PostContent</code> API) has an invalid value. </p>

##### Instances
``` purescript
Newtype UnsupportedMediaTypeException _
```

#### `UserId`

``` purescript
newtype UserId
  = UserId String
```

##### Instances
``` purescript
Newtype UserId _
```

#### `GenericAttachmentList'`

``` purescript
newtype GenericAttachmentList'
  = GenericAttachmentList' (Array GenericAttachment)
```

##### Instances
``` purescript
Newtype GenericAttachmentList' _
```

#### `ListOfButtons'`

``` purescript
newtype ListOfButtons'
  = ListOfButtons' (Array Button)
```

##### Instances
``` purescript
Newtype ListOfButtons' _
```


