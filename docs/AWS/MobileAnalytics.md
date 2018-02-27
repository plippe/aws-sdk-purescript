## Module AWS.MobileAnalytics

<p>Amazon Mobile Analytics is a service for collecting, visualizing, and understanding app usage data at scale.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `putEvents`

``` purescript
putEvents :: forall eff. PutEventsInput -> Aff (err :: RequestError | eff) Unit
```

<p>The PutEvents operation records one or more events. You can have up to 1,500 unique custom events per app, any combination of up to 40 attributes and metrics per custom event, and any number of attribute or metric values.</p>

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message'" :: NullOrUndefined (String) }
```

<p>An exception object returned when a request fails.</p>

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `Event`

``` purescript
newtype Event
  = Event { "EventType'" :: String50Chars, "Number" :: ISO8601Timestamp, "Session'" :: NullOrUndefined (Session), "Version'" :: NullOrUndefined (String10Chars), "Attributes'" :: NullOrUndefined (MapOfStringToString), "Metrics'" :: NullOrUndefined (MapOfStringToNumber) }
```

<p>A JSON object representing a batch of unique event occurrences in your app.</p>

##### Instances
``` purescript
Newtype Event _
```

#### `EventListDefinition`

``` purescript
newtype EventListDefinition
  = EventListDefinition (Array Event)
```

##### Instances
``` purescript
Newtype EventListDefinition _
```

#### `ISO8601Timestamp`

``` purescript
newtype ISO8601Timestamp
  = ISO8601Timestamp String
```

##### Instances
``` purescript
Newtype ISO8601Timestamp _
```

#### `MapOfStringToNumber`

``` purescript
newtype MapOfStringToNumber
  = MapOfStringToNumber (Map String50Chars Number)
```

##### Instances
``` purescript
Newtype MapOfStringToNumber _
```

#### `MapOfStringToString`

``` purescript
newtype MapOfStringToString
  = MapOfStringToString (Map String50Chars String0to1000Chars)
```

##### Instances
``` purescript
Newtype MapOfStringToString _
```

#### `PutEventsInput`

``` purescript
newtype PutEventsInput
  = PutEventsInput { "Events'" :: EventListDefinition, "ClientContext'" :: String, "ClientContextEncoding'" :: NullOrUndefined (String) }
```

<p>A container for the data needed for a PutEvent operation</p>

##### Instances
``` purescript
Newtype PutEventsInput _
```

#### `Session`

``` purescript
newtype Session
  = Session { "Id'" :: NullOrUndefined (String50Chars), "Duration'" :: NullOrUndefined (Number), "StartTimestamp'" :: NullOrUndefined (ISO8601Timestamp), "StopTimestamp'" :: NullOrUndefined (ISO8601Timestamp) }
```

<p>Describes the session. Session information is required on ALL events.</p>

##### Instances
``` purescript
Newtype Session _
```

#### `String0to1000Chars`

``` purescript
newtype String0to1000Chars
  = String0to1000Chars String
```

##### Instances
``` purescript
Newtype String0to1000Chars _
```

#### `String10Chars`

``` purescript
newtype String10Chars
  = String10Chars String
```

##### Instances
``` purescript
Newtype String10Chars _
```

#### `String50Chars`

``` purescript
newtype String50Chars
  = String50Chars String
```

##### Instances
``` purescript
Newtype String50Chars _
```


