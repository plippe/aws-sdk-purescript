## Module AWS.MediaPackage

AWS Elemental MediaPackage

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createChannel`

``` purescript
createChannel :: forall eff. CreateChannelRequest -> Aff (err :: RequestError | eff) CreateChannelResponse
```

Creates a new Channel.

#### `createOriginEndpoint`

``` purescript
createOriginEndpoint :: forall eff. CreateOriginEndpointRequest -> Aff (err :: RequestError | eff) CreateOriginEndpointResponse
```

Creates a new OriginEndpoint record.

#### `deleteChannel`

``` purescript
deleteChannel :: forall eff. DeleteChannelRequest -> Aff (err :: RequestError | eff) DeleteChannelResponse
```

Deletes an existing Channel.

#### `deleteOriginEndpoint`

``` purescript
deleteOriginEndpoint :: forall eff. DeleteOriginEndpointRequest -> Aff (err :: RequestError | eff) DeleteOriginEndpointResponse
```

Deletes an existing OriginEndpoint.

#### `describeChannel`

``` purescript
describeChannel :: forall eff. DescribeChannelRequest -> Aff (err :: RequestError | eff) DescribeChannelResponse
```

Gets details about a Channel.

#### `describeOriginEndpoint`

``` purescript
describeOriginEndpoint :: forall eff. DescribeOriginEndpointRequest -> Aff (err :: RequestError | eff) DescribeOriginEndpointResponse
```

Gets details about an existing OriginEndpoint.

#### `listChannels`

``` purescript
listChannels :: forall eff. ListChannelsRequest -> Aff (err :: RequestError | eff) ListChannelsResponse
```

Returns a collection of Channels.

#### `listOriginEndpoints`

``` purescript
listOriginEndpoints :: forall eff. ListOriginEndpointsRequest -> Aff (err :: RequestError | eff) ListOriginEndpointsResponse
```

Returns a collection of OriginEndpoint records.

#### `rotateChannelCredentials`

``` purescript
rotateChannelCredentials :: forall eff. RotateChannelCredentialsRequest -> Aff (err :: RequestError | eff) RotateChannelCredentialsResponse
```

Changes the Channel ingest username and password.

#### `updateChannel`

``` purescript
updateChannel :: forall eff. UpdateChannelRequest -> Aff (err :: RequestError | eff) UpdateChannelResponse
```

Updates an existing Channel.

#### `updateOriginEndpoint`

``` purescript
updateOriginEndpoint :: forall eff. UpdateOriginEndpointRequest -> Aff (err :: RequestError | eff) UpdateOriginEndpointResponse
```

Updates an existing OriginEndpoint.

#### `AdMarkers`

``` purescript
newtype AdMarkers
  = AdMarkers String
```

#### `Channel`

``` purescript
newtype Channel
  = Channel { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

A Channel resource configuration.

#### `ChannelCreateParameters`

``` purescript
newtype ChannelCreateParameters
  = ChannelCreateParameters { "Description" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String) }
```

Configuration parameters for a new Channel.

#### `ChannelList`

``` purescript
newtype ChannelList
  = ChannelList { "Channels" :: NullOrUndefined (ListOfChannel), "NextToken" :: NullOrUndefined (String) }
```

A collection of Channel records.

#### `ChannelUpdateParameters`

``` purescript
newtype ChannelUpdateParameters
  = ChannelUpdateParameters { "Description" :: NullOrUndefined (String) }
```

Configuration parameters for updating an existing Channel.

#### `CreateChannelRequest`

``` purescript
newtype CreateChannelRequest
  = CreateChannelRequest { "Description" :: NullOrUndefined (String), "Id" :: String }
```

A new Channel configuration.

#### `CreateChannelResponse`

``` purescript
newtype CreateChannelResponse
  = CreateChannelResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

#### `CreateOriginEndpointRequest`

``` purescript
newtype CreateOriginEndpointRequest
  = CreateOriginEndpointRequest { "ChannelId" :: String, "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: String, "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

Configuration parameters used to create a new OriginEndpoint.

#### `CreateOriginEndpointResponse`

``` purescript
newtype CreateOriginEndpointResponse
  = CreateOriginEndpointResponse { "Arn" :: NullOrUndefined (String), "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Url" :: NullOrUndefined (String), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

#### `DashEncryption`

``` purescript
newtype DashEncryption
  = DashEncryption { "KeyRotationIntervalSeconds" :: NullOrUndefined (Int), "SpekeKeyProvider" :: SpekeKeyProvider }
```

A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.

#### `DashPackage`

``` purescript
newtype DashPackage
  = DashPackage { "Encryption" :: NullOrUndefined (DashEncryption), "ManifestWindowSeconds" :: NullOrUndefined (Int), "MinBufferTimeSeconds" :: NullOrUndefined (Int), "MinUpdatePeriodSeconds" :: NullOrUndefined (Int), "Profile" :: NullOrUndefined (Profile), "SegmentDurationSeconds" :: NullOrUndefined (Int), "StreamSelection" :: NullOrUndefined (StreamSelection), "SuggestedPresentationDelaySeconds" :: NullOrUndefined (Int) }
```

A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.

#### `DeleteChannelRequest`

``` purescript
newtype DeleteChannelRequest
  = DeleteChannelRequest { "Id" :: String }
```

#### `DeleteChannelResponse`

``` purescript
newtype DeleteChannelResponse
  = DeleteChannelResponse {  }
```

#### `DeleteOriginEndpointRequest`

``` purescript
newtype DeleteOriginEndpointRequest
  = DeleteOriginEndpointRequest { "Id" :: String }
```

#### `DeleteOriginEndpointResponse`

``` purescript
newtype DeleteOriginEndpointResponse
  = DeleteOriginEndpointResponse {  }
```

#### `DescribeChannelRequest`

``` purescript
newtype DescribeChannelRequest
  = DescribeChannelRequest { "Id" :: String }
```

#### `DescribeChannelResponse`

``` purescript
newtype DescribeChannelResponse
  = DescribeChannelResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

#### `DescribeOriginEndpointRequest`

``` purescript
newtype DescribeOriginEndpointRequest
  = DescribeOriginEndpointRequest { "Id" :: String }
```

#### `DescribeOriginEndpointResponse`

``` purescript
newtype DescribeOriginEndpointResponse
  = DescribeOriginEndpointResponse { "Arn" :: NullOrUndefined (String), "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Url" :: NullOrUndefined (String), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

#### `EncryptionMethod`

``` purescript
newtype EncryptionMethod
  = EncryptionMethod String
```

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "Message" :: NullOrUndefined (String) }
```

The client is not authorized to access the requested resource.

#### `HlsEncryption`

``` purescript
newtype HlsEncryption
  = HlsEncryption { "ConstantInitializationVector" :: NullOrUndefined (String), "EncryptionMethod" :: NullOrUndefined (EncryptionMethod), "KeyRotationIntervalSeconds" :: NullOrUndefined (Int), "RepeatExtXKey" :: NullOrUndefined (Boolean), "SpekeKeyProvider" :: SpekeKeyProvider }
```

An HTTP Live Streaming (HLS) encryption configuration.

#### `HlsIngest`

``` purescript
newtype HlsIngest
  = HlsIngest { "IngestEndpoints" :: NullOrUndefined (ListOfIngestEndpoint) }
```

An HTTP Live Streaming (HLS) ingest resource configuration.

#### `HlsPackage`

``` purescript
newtype HlsPackage
  = HlsPackage { "AdMarkers" :: NullOrUndefined (AdMarkers), "Encryption" :: NullOrUndefined (HlsEncryption), "IncludeIframeOnlyStream" :: NullOrUndefined (Boolean), "PlaylistType" :: NullOrUndefined (PlaylistType), "PlaylistWindowSeconds" :: NullOrUndefined (Int), "ProgramDateTimeIntervalSeconds" :: NullOrUndefined (Int), "SegmentDurationSeconds" :: NullOrUndefined (Int), "StreamSelection" :: NullOrUndefined (StreamSelection), "UseAudioRenditionGroup" :: NullOrUndefined (Boolean) }
```

An HTTP Live Streaming (HLS) packaging configuration.

#### `IngestEndpoint`

``` purescript
newtype IngestEndpoint
  = IngestEndpoint { "Password" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

An endpoint for ingesting source content for a Channel.

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "Message" :: NullOrUndefined (String) }
```

An unexpected error occurred.

#### `ListChannelsRequest`

``` purescript
newtype ListChannelsRequest
  = ListChannelsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListChannelsResponse`

``` purescript
newtype ListChannelsResponse
  = ListChannelsResponse { "Channels" :: NullOrUndefined (ListOfChannel), "NextToken" :: NullOrUndefined (String) }
```

#### `ListOfChannel`

``` purescript
newtype ListOfChannel
  = ListOfChannel (Array Channel)
```

#### `ListOfIngestEndpoint`

``` purescript
newtype ListOfIngestEndpoint
  = ListOfIngestEndpoint (Array IngestEndpoint)
```

#### `ListOfOriginEndpoint`

``` purescript
newtype ListOfOriginEndpoint
  = ListOfOriginEndpoint (Array OriginEndpoint)
```

#### `ListOf__string`

``` purescript
newtype ListOf__string
  = ListOf__string (Array String)
```

#### `ListOriginEndpointsRequest`

``` purescript
newtype ListOriginEndpointsRequest
  = ListOriginEndpointsRequest { "ChannelId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListOriginEndpointsResponse`

``` purescript
newtype ListOriginEndpointsResponse
  = ListOriginEndpointsResponse { "NextToken" :: NullOrUndefined (String), "OriginEndpoints" :: NullOrUndefined (ListOfOriginEndpoint) }
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `MssEncryption`

``` purescript
newtype MssEncryption
  = MssEncryption { "SpekeKeyProvider" :: SpekeKeyProvider }
```

A Microsoft Smooth Streaming (MSS) encryption configuration.

#### `MssPackage`

``` purescript
newtype MssPackage
  = MssPackage { "Encryption" :: NullOrUndefined (MssEncryption), "ManifestWindowSeconds" :: NullOrUndefined (Int), "SegmentDurationSeconds" :: NullOrUndefined (Int), "StreamSelection" :: NullOrUndefined (StreamSelection) }
```

A Microsoft Smooth Streaming (MSS) packaging configuration.

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (String) }
```

The requested resource does not exist.

#### `OriginEndpoint`

``` purescript
newtype OriginEndpoint
  = OriginEndpoint { "Arn" :: NullOrUndefined (String), "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Url" :: NullOrUndefined (String), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

An OriginEndpoint resource configuration.

#### `OriginEndpointCreateParameters`

``` purescript
newtype OriginEndpointCreateParameters
  = OriginEndpointCreateParameters { "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

Configuration parameters for a new OriginEndpoint.

#### `OriginEndpointList`

``` purescript
newtype OriginEndpointList
  = OriginEndpointList { "NextToken" :: NullOrUndefined (String), "OriginEndpoints" :: NullOrUndefined (ListOfOriginEndpoint) }
```

A collection of OriginEndpoint records.

#### `OriginEndpointUpdateParameters`

``` purescript
newtype OriginEndpointUpdateParameters
  = OriginEndpointUpdateParameters { "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

Configuration parameters for updating an existing OriginEndpoint.

#### `PlaylistType`

``` purescript
newtype PlaylistType
  = PlaylistType String
```

#### `Profile`

``` purescript
newtype Profile
  = Profile String
```

#### `RotateChannelCredentialsRequest`

``` purescript
newtype RotateChannelCredentialsRequest
  = RotateChannelCredentialsRequest { "Id" :: String }
```

#### `RotateChannelCredentialsResponse`

``` purescript
newtype RotateChannelCredentialsResponse
  = RotateChannelCredentialsResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message" :: NullOrUndefined (String) }
```

An unexpected error occurred.

#### `SpekeKeyProvider`

``` purescript
newtype SpekeKeyProvider
  = SpekeKeyProvider { "ResourceId" :: String, "RoleArn" :: String, "SystemIds" :: ListOf__string, "Url" :: String }
```

A configuration for accessing an external Secure Packager and Encoder Key Exchange (SPEKE) service that will provide encryption keys.

#### `StreamOrder`

``` purescript
newtype StreamOrder
  = StreamOrder String
```

#### `StreamSelection`

``` purescript
newtype StreamSelection
  = StreamSelection { "MaxVideoBitsPerSecond" :: NullOrUndefined (Int), "MinVideoBitsPerSecond" :: NullOrUndefined (Int), "StreamOrder" :: NullOrUndefined (StreamOrder) }
```

A StreamSelection configuration.

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String) }
```

The client has exceeded their resource or throttling limits.

#### `UnprocessableEntityException`

``` purescript
newtype UnprocessableEntityException
  = UnprocessableEntityException { "Message" :: NullOrUndefined (String) }
```

The parameters sent in the request are not valid.

#### `UpdateChannelRequest`

``` purescript
newtype UpdateChannelRequest
  = UpdateChannelRequest { "Description" :: NullOrUndefined (String), "Id" :: String }
```

Configuration parameters used to update the Channel.

#### `UpdateChannelResponse`

``` purescript
newtype UpdateChannelResponse
  = UpdateChannelResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

#### `UpdateOriginEndpointRequest`

``` purescript
newtype UpdateOriginEndpointRequest
  = UpdateOriginEndpointRequest { "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: String, "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

Configuration parameters used to update an existing OriginEndpoint.

#### `UpdateOriginEndpointResponse`

``` purescript
newtype UpdateOriginEndpointResponse
  = UpdateOriginEndpointResponse { "Arn" :: NullOrUndefined (String), "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Url" :: NullOrUndefined (String), "Whitelist" :: NullOrUndefined (ListOf__string) }
```


