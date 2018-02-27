## Module AWS.SageMakerRuntime

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

##### Instances
``` purescript
Newtype BodyBlob _
```

#### `EndpointName`

``` purescript
newtype EndpointName
  = EndpointName String
```

##### Instances
``` purescript
Newtype EndpointName _
```

#### `Header`

``` purescript
newtype Header
  = Header String
```

##### Instances
``` purescript
Newtype Header _
```

#### `InternalFailure`

``` purescript
newtype InternalFailure
  = InternalFailure { "Message" :: NullOrUndefined (Message) }
```

<p> Internal failure occurred. </p>

##### Instances
``` purescript
Newtype InternalFailure _
```

#### `InvokeEndpointInput`

``` purescript
newtype InvokeEndpointInput
  = InvokeEndpointInput { "EndpointName" :: EndpointName, "Body" :: BodyBlob, "ContentType" :: NullOrUndefined (Header), "Accept" :: NullOrUndefined (Header) }
```

##### Instances
``` purescript
Newtype InvokeEndpointInput _
```

#### `InvokeEndpointOutput`

``` purescript
newtype InvokeEndpointOutput
  = InvokeEndpointOutput { "Body" :: BodyBlob, "ContentType" :: NullOrUndefined (Header), "InvokedProductionVariant" :: NullOrUndefined (Header) }
```

##### Instances
``` purescript
Newtype InvokeEndpointOutput _
```

#### `LogStreamArn`

``` purescript
newtype LogStreamArn
  = LogStreamArn String
```

##### Instances
``` purescript
Newtype LogStreamArn _
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

##### Instances
``` purescript
Newtype Message _
```

#### `ModelError`

``` purescript
newtype ModelError
  = ModelError { "Message" :: NullOrUndefined (Message), "OriginalStatusCode" :: NullOrUndefined (StatusCode), "OriginalMessage" :: NullOrUndefined (Message), "LogStreamArn" :: NullOrUndefined (LogStreamArn) }
```

<p> Model (owned by the customer in the container) returned an error 500. </p>

##### Instances
``` purescript
Newtype ModelError _
```

#### `ServiceUnavailable`

``` purescript
newtype ServiceUnavailable
  = ServiceUnavailable { "Message" :: NullOrUndefined (Message) }
```

<p> Service is unavailable. Try your call again. </p>

##### Instances
``` purescript
Newtype ServiceUnavailable _
```

#### `StatusCode`

``` purescript
newtype StatusCode
  = StatusCode Int
```

##### Instances
``` purescript
Newtype StatusCode _
```

#### `ValidationError`

``` purescript
newtype ValidationError
  = ValidationError { "Message" :: NullOrUndefined (Message) }
```

<p> Inspect your request and try again. </p>

##### Instances
``` purescript
Newtype ValidationError _
```


