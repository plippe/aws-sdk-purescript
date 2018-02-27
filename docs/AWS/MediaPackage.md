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

##### Instances
``` purescript
Newtype AdMarkers _
```

#### `Channel`

``` purescript
newtype Channel
  = Channel { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

A Channel resource configuration.

##### Instances
``` purescript
Newtype Channel _
```

#### `ChannelCreateParameters`

``` purescript
newtype ChannelCreateParameters
  = ChannelCreateParameters { "Description" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String) }
```

Configuration parameters for a new Channel.

##### Instances
``` purescript
Newtype ChannelCreateParameters _
```

#### `ChannelList`

``` purescript
newtype ChannelList
  = ChannelList { "Channels" :: NullOrUndefined (ListOfChannel), "NextToken" :: NullOrUndefined (String) }
```

A collection of Channel records.

##### Instances
``` purescript
Newtype ChannelList _
```

#### `ChannelUpdateParameters`

``` purescript
newtype ChannelUpdateParameters
  = ChannelUpdateParameters { "Description" :: NullOrUndefined (String) }
```

Configuration parameters for updating an existing Channel.

##### Instances
``` purescript
Newtype ChannelUpdateParameters _
```

#### `CreateChannelRequest`

``` purescript
newtype CreateChannelRequest
  = CreateChannelRequest { "Description" :: NullOrUndefined (String), "Id" :: String }
```

A new Channel configuration.

##### Instances
``` purescript
Newtype CreateChannelRequest _
```

#### `CreateChannelResponse`

``` purescript
newtype CreateChannelResponse
  = CreateChannelResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateChannelResponse _
```

#### `CreateOriginEndpointRequest`

``` purescript
newtype CreateOriginEndpointRequest
  = CreateOriginEndpointRequest { "ChannelId" :: String, "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: String, "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

Configuration parameters used to create a new OriginEndpoint.

##### Instances
``` purescript
Newtype CreateOriginEndpointRequest _
```

#### `CreateOriginEndpointResponse`

``` purescript
newtype CreateOriginEndpointResponse
  = CreateOriginEndpointResponse { "Arn" :: NullOrUndefined (String), "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Url" :: NullOrUndefined (String), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

##### Instances
``` purescript
Newtype CreateOriginEndpointResponse _
```

#### `DashEncryption`

``` purescript
newtype DashEncryption
  = DashEncryption { "KeyRotationIntervalSeconds" :: NullOrUndefined (Int), "SpekeKeyProvider" :: SpekeKeyProvider }
```

A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.

##### Instances
``` purescript
Newtype DashEncryption _
```

#### `DashPackage`

``` purescript
newtype DashPackage
  = DashPackage { "Encryption" :: NullOrUndefined (DashEncryption), "ManifestWindowSeconds" :: NullOrUndefined (Int), "MinBufferTimeSeconds" :: NullOrUndefined (Int), "MinUpdatePeriodSeconds" :: NullOrUndefined (Int), "Profile" :: NullOrUndefined (Profile), "SegmentDurationSeconds" :: NullOrUndefined (Int), "StreamSelection" :: NullOrUndefined (StreamSelection), "SuggestedPresentationDelaySeconds" :: NullOrUndefined (Int) }
```

A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.

##### Instances
``` purescript
Newtype DashPackage _
```

#### `DeleteChannelRequest`

``` purescript
newtype DeleteChannelRequest
  = DeleteChannelRequest { "Id" :: String }
```

##### Instances
``` purescript
Newtype DeleteChannelRequest _
```

#### `DeleteChannelResponse`

``` purescript
newtype DeleteChannelResponse
  = DeleteChannelResponse {  }
```

##### Instances
``` purescript
Newtype DeleteChannelResponse _
```

#### `DeleteOriginEndpointRequest`

``` purescript
newtype DeleteOriginEndpointRequest
  = DeleteOriginEndpointRequest { "Id" :: String }
```

##### Instances
``` purescript
Newtype DeleteOriginEndpointRequest _
```

#### `DeleteOriginEndpointResponse`

``` purescript
newtype DeleteOriginEndpointResponse
  = DeleteOriginEndpointResponse {  }
```

##### Instances
``` purescript
Newtype DeleteOriginEndpointResponse _
```

#### `DescribeChannelRequest`

``` purescript
newtype DescribeChannelRequest
  = DescribeChannelRequest { "Id" :: String }
```

##### Instances
``` purescript
Newtype DescribeChannelRequest _
```

#### `DescribeChannelResponse`

``` purescript
newtype DescribeChannelResponse
  = DescribeChannelResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeChannelResponse _
```

#### `DescribeOriginEndpointRequest`

``` purescript
newtype DescribeOriginEndpointRequest
  = DescribeOriginEndpointRequest { "Id" :: String }
```

##### Instances
``` purescript
Newtype DescribeOriginEndpointRequest _
```

#### `DescribeOriginEndpointResponse`

``` purescript
newtype DescribeOriginEndpointResponse
  = DescribeOriginEndpointResponse { "Arn" :: NullOrUndefined (String), "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Url" :: NullOrUndefined (String), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

##### Instances
``` purescript
Newtype DescribeOriginEndpointResponse _
```

#### `EncryptionMethod`

``` purescript
newtype EncryptionMethod
  = EncryptionMethod String
```

##### Instances
``` purescript
Newtype EncryptionMethod _
```

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "Message" :: NullOrUndefined (String) }
```

The client is not authorized to access the requested resource.

##### Instances
``` purescript
Newtype ForbiddenException _
```

#### `HlsEncryption`

``` purescript
newtype HlsEncryption
  = HlsEncryption { "ConstantInitializationVector" :: NullOrUndefined (String), "EncryptionMethod" :: NullOrUndefined (EncryptionMethod), "KeyRotationIntervalSeconds" :: NullOrUndefined (Int), "RepeatExtXKey" :: NullOrUndefined (Boolean), "SpekeKeyProvider" :: SpekeKeyProvider }
```

An HTTP Live Streaming (HLS) encryption configuration.

##### Instances
``` purescript
Newtype HlsEncryption _
```

#### `HlsIngest`

``` purescript
newtype HlsIngest
  = HlsIngest { "IngestEndpoints" :: NullOrUndefined (ListOfIngestEndpoint) }
```

An HTTP Live Streaming (HLS) ingest resource configuration.

##### Instances
``` purescript
Newtype HlsIngest _
```

#### `HlsPackage`

``` purescript
newtype HlsPackage
  = HlsPackage { "AdMarkers" :: NullOrUndefined (AdMarkers), "Encryption" :: NullOrUndefined (HlsEncryption), "IncludeIframeOnlyStream" :: NullOrUndefined (Boolean), "PlaylistType" :: NullOrUndefined (PlaylistType), "PlaylistWindowSeconds" :: NullOrUndefined (Int), "ProgramDateTimeIntervalSeconds" :: NullOrUndefined (Int), "SegmentDurationSeconds" :: NullOrUndefined (Int), "StreamSelection" :: NullOrUndefined (StreamSelection), "UseAudioRenditionGroup" :: NullOrUndefined (Boolean) }
```

An HTTP Live Streaming (HLS) packaging configuration.

##### Instances
``` purescript
Newtype HlsPackage _
```

#### `IngestEndpoint`

``` purescript
newtype IngestEndpoint
  = IngestEndpoint { "Password" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

An endpoint for ingesting source content for a Channel.

##### Instances
``` purescript
Newtype IngestEndpoint _
```

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "Message" :: NullOrUndefined (String) }
```

An unexpected error occurred.

##### Instances
``` purescript
Newtype InternalServerErrorException _
```

#### `ListChannelsRequest`

``` purescript
newtype ListChannelsRequest
  = ListChannelsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListChannelsRequest _
```

#### `ListChannelsResponse`

``` purescript
newtype ListChannelsResponse
  = ListChannelsResponse { "Channels" :: NullOrUndefined (ListOfChannel), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListChannelsResponse _
```

#### `ListOfChannel`

``` purescript
newtype ListOfChannel
  = ListOfChannel (Array Channel)
```

##### Instances
``` purescript
Newtype ListOfChannel _
```

#### `ListOfIngestEndpoint`

``` purescript
newtype ListOfIngestEndpoint
  = ListOfIngestEndpoint (Array IngestEndpoint)
```

##### Instances
``` purescript
Newtype ListOfIngestEndpoint _
```

#### `ListOfOriginEndpoint`

``` purescript
newtype ListOfOriginEndpoint
  = ListOfOriginEndpoint (Array OriginEndpoint)
```

##### Instances
``` purescript
Newtype ListOfOriginEndpoint _
```

#### `ListOf__string`

``` purescript
newtype ListOf__string
  = ListOf__string (Array String)
```

##### Instances
``` purescript
Newtype ListOf__string _
```

#### `ListOriginEndpointsRequest`

``` purescript
newtype ListOriginEndpointsRequest
  = ListOriginEndpointsRequest { "ChannelId" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListOriginEndpointsRequest _
```

#### `ListOriginEndpointsResponse`

``` purescript
newtype ListOriginEndpointsResponse
  = ListOriginEndpointsResponse { "NextToken" :: NullOrUndefined (String), "OriginEndpoints" :: NullOrUndefined (ListOfOriginEndpoint) }
```

##### Instances
``` purescript
Newtype ListOriginEndpointsResponse _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `MssEncryption`

``` purescript
newtype MssEncryption
  = MssEncryption { "SpekeKeyProvider" :: SpekeKeyProvider }
```

A Microsoft Smooth Streaming (MSS) encryption configuration.

##### Instances
``` purescript
Newtype MssEncryption _
```

#### `MssPackage`

``` purescript
newtype MssPackage
  = MssPackage { "Encryption" :: NullOrUndefined (MssEncryption), "ManifestWindowSeconds" :: NullOrUndefined (Int), "SegmentDurationSeconds" :: NullOrUndefined (Int), "StreamSelection" :: NullOrUndefined (StreamSelection) }
```

A Microsoft Smooth Streaming (MSS) packaging configuration.

##### Instances
``` purescript
Newtype MssPackage _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (String) }
```

The requested resource does not exist.

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `OriginEndpoint`

``` purescript
newtype OriginEndpoint
  = OriginEndpoint { "Arn" :: NullOrUndefined (String), "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Url" :: NullOrUndefined (String), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

An OriginEndpoint resource configuration.

##### Instances
``` purescript
Newtype OriginEndpoint _
```

#### `OriginEndpointCreateParameters`

``` purescript
newtype OriginEndpointCreateParameters
  = OriginEndpointCreateParameters { "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

Configuration parameters for a new OriginEndpoint.

##### Instances
``` purescript
Newtype OriginEndpointCreateParameters _
```

#### `OriginEndpointList`

``` purescript
newtype OriginEndpointList
  = OriginEndpointList { "NextToken" :: NullOrUndefined (String), "OriginEndpoints" :: NullOrUndefined (ListOfOriginEndpoint) }
```

A collection of OriginEndpoint records.

##### Instances
``` purescript
Newtype OriginEndpointList _
```

#### `OriginEndpointUpdateParameters`

``` purescript
newtype OriginEndpointUpdateParameters
  = OriginEndpointUpdateParameters { "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

Configuration parameters for updating an existing OriginEndpoint.

##### Instances
``` purescript
Newtype OriginEndpointUpdateParameters _
```

#### `PlaylistType`

``` purescript
newtype PlaylistType
  = PlaylistType String
```

##### Instances
``` purescript
Newtype PlaylistType _
```

#### `Profile`

``` purescript
newtype Profile
  = Profile String
```

##### Instances
``` purescript
Newtype Profile _
```

#### `RotateChannelCredentialsRequest`

``` purescript
newtype RotateChannelCredentialsRequest
  = RotateChannelCredentialsRequest { "Id" :: String }
```

##### Instances
``` purescript
Newtype RotateChannelCredentialsRequest _
```

#### `RotateChannelCredentialsResponse`

``` purescript
newtype RotateChannelCredentialsResponse
  = RotateChannelCredentialsResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype RotateChannelCredentialsResponse _
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message" :: NullOrUndefined (String) }
```

An unexpected error occurred.

##### Instances
``` purescript
Newtype ServiceUnavailableException _
```

#### `SpekeKeyProvider`

``` purescript
newtype SpekeKeyProvider
  = SpekeKeyProvider { "ResourceId" :: String, "RoleArn" :: String, "SystemIds" :: ListOf__string, "Url" :: String }
```

A configuration for accessing an external Secure Packager and Encoder Key Exchange (SPEKE) service that will provide encryption keys.

##### Instances
``` purescript
Newtype SpekeKeyProvider _
```

#### `StreamOrder`

``` purescript
newtype StreamOrder
  = StreamOrder String
```

##### Instances
``` purescript
Newtype StreamOrder _
```

#### `StreamSelection`

``` purescript
newtype StreamSelection
  = StreamSelection { "MaxVideoBitsPerSecond" :: NullOrUndefined (Int), "MinVideoBitsPerSecond" :: NullOrUndefined (Int), "StreamOrder" :: NullOrUndefined (StreamOrder) }
```

A StreamSelection configuration.

##### Instances
``` purescript
Newtype StreamSelection _
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String) }
```

The client has exceeded their resource or throttling limits.

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `UnprocessableEntityException`

``` purescript
newtype UnprocessableEntityException
  = UnprocessableEntityException { "Message" :: NullOrUndefined (String) }
```

The parameters sent in the request are not valid.

##### Instances
``` purescript
Newtype UnprocessableEntityException _
```

#### `UpdateChannelRequest`

``` purescript
newtype UpdateChannelRequest
  = UpdateChannelRequest { "Description" :: NullOrUndefined (String), "Id" :: String }
```

Configuration parameters used to update the Channel.

##### Instances
``` purescript
Newtype UpdateChannelRequest _
```

#### `UpdateChannelResponse`

``` purescript
newtype UpdateChannelResponse
  = UpdateChannelResponse { "Arn" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "HlsIngest" :: NullOrUndefined (HlsIngest), "Id" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateChannelResponse _
```

#### `UpdateOriginEndpointRequest`

``` purescript
newtype UpdateOriginEndpointRequest
  = UpdateOriginEndpointRequest { "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: String, "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

Configuration parameters used to update an existing OriginEndpoint.

##### Instances
``` purescript
Newtype UpdateOriginEndpointRequest _
```

#### `UpdateOriginEndpointResponse`

``` purescript
newtype UpdateOriginEndpointResponse
  = UpdateOriginEndpointResponse { "Arn" :: NullOrUndefined (String), "ChannelId" :: NullOrUndefined (String), "DashPackage" :: NullOrUndefined (DashPackage), "Description" :: NullOrUndefined (String), "HlsPackage" :: NullOrUndefined (HlsPackage), "Id" :: NullOrUndefined (String), "ManifestName" :: NullOrUndefined (String), "MssPackage" :: NullOrUndefined (MssPackage), "StartoverWindowSeconds" :: NullOrUndefined (Int), "TimeDelaySeconds" :: NullOrUndefined (Int), "Url" :: NullOrUndefined (String), "Whitelist" :: NullOrUndefined (ListOf__string) }
```

##### Instances
``` purescript
Newtype UpdateOriginEndpointResponse _
```


