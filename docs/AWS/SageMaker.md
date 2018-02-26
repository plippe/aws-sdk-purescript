## Module AWS.SageMaker

<p> Amazon SageMaker runtime API. </p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `invokeEndpoint`

``` purescript
invokeEndpoint :: forall eff. InvokeEndpointInput -> Aff (err :: RequestError | eff) InvokeEndpointOutput
```

<p>After you deploy a model into production using Amazon SageMaker hosting services, your client applications use this API to get inferences from the model hosted at the specified endpoint. </p> <p>For an overview of Amazon SageMaker, see <a href="http://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html">How It Works</a> </p> <p> Amazon SageMaker strips all POST headers except those supported by the API. Amazon SageMaker might add additional headers. You should not rely on the behavior of headers outside those enumerated in the request syntax. </p>

#### `BodyBlob`

``` purescript
newtype BodyBlob
  = BodyBlob String
```

#### `EndpointName`

``` purescript
newtype EndpointName
  = EndpointName String
```

#### `Header`

``` purescript
newtype Header
  = Header String
```

#### `InternalFailure`

``` purescript
newtype InternalFailure
  = InternalFailure { "Message" :: NullOrUndefined (Message) }
```

<p> Internal failure occurred. </p>

#### `InvokeEndpointInput`

``` purescript
newtype InvokeEndpointInput
  = InvokeEndpointInput { "EndpointName" :: EndpointName, "Body" :: BodyBlob, "ContentType" :: NullOrUndefined (Header), "Accept" :: NullOrUndefined (Header) }
```

#### `InvokeEndpointOutput`

``` purescript
newtype InvokeEndpointOutput
  = InvokeEndpointOutput { "Body" :: BodyBlob, "ContentType" :: NullOrUndefined (Header), "InvokedProductionVariant" :: NullOrUndefined (Header) }
```

#### `LogStreamArn`

``` purescript
newtype LogStreamArn
  = LogStreamArn String
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

#### `ModelError`

``` purescript
newtype ModelError
  = ModelError { "Message" :: NullOrUndefined (Message), "OriginalStatusCode" :: NullOrUndefined (StatusCode), "OriginalMessage" :: NullOrUndefined (Message), "LogStreamArn" :: NullOrUndefined (LogStreamArn) }
```

<p> Model (owned by the customer in the container) returned an error 500. </p>

#### `ServiceUnavailable`

``` purescript
newtype ServiceUnavailable
  = ServiceUnavailable { "Message" :: NullOrUndefined (Message) }
```

<p> Service is unavailable. Try your call again. </p>

#### `StatusCode`

``` purescript
newtype StatusCode
  = StatusCode Int
```

#### `ValidationError`

``` purescript
newtype ValidationError
  = ValidationError { "Message" :: NullOrUndefined (Message) }
```

<p> Inspect your request and try again. </p>


