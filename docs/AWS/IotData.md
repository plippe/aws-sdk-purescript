## Module AWS.IotData

<fullname>AWS IoT</fullname> <p>AWS IoT-Data enables secure, bi-directional communication between Internet-connected things (such as sensors, actuators, embedded devices, or smart appliances) and the AWS cloud. It implements a broker for applications and things to publish messages over HTTP (Publish) and retrieve, update, and delete thing shadows. A thing shadow is a persistent representation of your things and their state in the AWS cloud.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `deleteThingShadow`

``` purescript
deleteThingShadow :: forall eff. DeleteThingShadowRequest -> Aff (err :: RequestError | eff) DeleteThingShadowResponse
```

<p>Deletes the thing shadow for the specified thing.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/API_DeleteThingShadow.html">DeleteThingShadow</a> in the <i>AWS IoT Developer Guide</i>.</p>

#### `getThingShadow`

``` purescript
getThingShadow :: forall eff. GetThingShadowRequest -> Aff (err :: RequestError | eff) GetThingShadowResponse
```

<p>Gets the thing shadow for the specified thing.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/API_GetThingShadow.html">GetThingShadow</a> in the <i>AWS IoT Developer Guide</i>.</p>

#### `publish`

``` purescript
publish :: forall eff. PublishRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Publishes state information.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/protocols.html#http">HTTP Protocol</a> in the <i>AWS IoT Developer Guide</i>.</p>

#### `updateThingShadow`

``` purescript
updateThingShadow :: forall eff. UpdateThingShadowRequest -> Aff (err :: RequestError | eff) UpdateThingShadowResponse
```

<p>Updates the thing shadow for the specified thing.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/iot/latest/developerguide/API_UpdateThingShadow.html">UpdateThingShadow</a> in the <i>AWS IoT Developer Guide</i>.</p>

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified version does not match the version of the document.</p>

#### `DeleteThingShadowRequest`

``` purescript
newtype DeleteThingShadowRequest
  = DeleteThingShadowRequest { "ThingName'" :: ThingName }
```

<p>The input for the DeleteThingShadow operation.</p>

#### `DeleteThingShadowResponse`

``` purescript
newtype DeleteThingShadowResponse
  = DeleteThingShadowResponse { "Payload'" :: JsonDocument }
```

<p>The output from the DeleteThingShadow operation.</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `GetThingShadowRequest`

``` purescript
newtype GetThingShadowRequest
  = GetThingShadowRequest { "ThingName'" :: ThingName }
```

<p>The input for the GetThingShadow operation.</p>

#### `GetThingShadowResponse`

``` purescript
newtype GetThingShadowResponse
  = GetThingShadowResponse { "Payload'" :: NullOrUndefined (JsonDocument) }
```

<p>The output from the GetThingShadow operation.</p>

#### `InternalFailureException`

``` purescript
newtype InternalFailureException
  = InternalFailureException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An unexpected error has occurred.</p>

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The request is not valid.</p>

#### `JsonDocument`

``` purescript
newtype JsonDocument
  = JsonDocument String
```

#### `MethodNotAllowedException`

``` purescript
newtype MethodNotAllowedException
  = MethodNotAllowedException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified combination of HTTP verb and URI is not supported.</p>

#### `Payload`

``` purescript
newtype Payload
  = Payload String
```

#### `PublishRequest`

``` purescript
newtype PublishRequest
  = PublishRequest { "Topic'" :: Topic, "Qos'" :: NullOrUndefined (Qos), "Payload'" :: NullOrUndefined (Payload) }
```

<p>The input for the Publish operation.</p>

#### `Qos`

``` purescript
newtype Qos
  = Qos Int
```

#### `RequestEntityTooLargeException`

``` purescript
newtype RequestEntityTooLargeException
  = RequestEntityTooLargeException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The payload exceeds the maximum size allowed.</p>

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The specified resource does not exist.</p>

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The service is temporarily unavailable.</p>

#### `ThingName`

``` purescript
newtype ThingName
  = ThingName String
```

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The rate exceeds the limit.</p>

#### `Topic`

``` purescript
newtype Topic
  = Topic String
```

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>You are not authorized to perform this operation.</p>

#### `UnsupportedDocumentEncodingException`

``` purescript
newtype UnsupportedDocumentEncodingException
  = UnsupportedDocumentEncodingException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The document encoding is not supported.</p>

#### `UpdateThingShadowRequest`

``` purescript
newtype UpdateThingShadowRequest
  = UpdateThingShadowRequest { "ThingName'" :: ThingName, "Payload'" :: JsonDocument }
```

<p>The input for the UpdateThingShadow operation.</p>

#### `UpdateThingShadowResponse`

``` purescript
newtype UpdateThingShadowResponse
  = UpdateThingShadowResponse { "Payload'" :: NullOrUndefined (JsonDocument) }
```

<p>The output from the UpdateThingShadow operation.</p>

#### `ErrorMessage'`

``` purescript
newtype ErrorMessage'
  = ErrorMessage' String
```


