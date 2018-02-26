

-- | AWS Elemental MediaPackage
module AWS.MediaPackage where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MediaPackage" :: String


-- | Creates a new Channel.
createChannel :: forall eff. CreateChannelRequest -> Aff (err :: AWS.RequestError | eff) CreateChannelResponse
createChannel = AWS.request serviceName "CreateChannel" 


-- | Creates a new OriginEndpoint record.
createOriginEndpoint :: forall eff. CreateOriginEndpointRequest -> Aff (err :: AWS.RequestError | eff) CreateOriginEndpointResponse
createOriginEndpoint = AWS.request serviceName "CreateOriginEndpoint" 


-- | Deletes an existing Channel.
deleteChannel :: forall eff. DeleteChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteChannelResponse
deleteChannel = AWS.request serviceName "DeleteChannel" 


-- | Deletes an existing OriginEndpoint.
deleteOriginEndpoint :: forall eff. DeleteOriginEndpointRequest -> Aff (err :: AWS.RequestError | eff) DeleteOriginEndpointResponse
deleteOriginEndpoint = AWS.request serviceName "DeleteOriginEndpoint" 


-- | Gets details about a Channel.
describeChannel :: forall eff. DescribeChannelRequest -> Aff (err :: AWS.RequestError | eff) DescribeChannelResponse
describeChannel = AWS.request serviceName "DescribeChannel" 


-- | Gets details about an existing OriginEndpoint.
describeOriginEndpoint :: forall eff. DescribeOriginEndpointRequest -> Aff (err :: AWS.RequestError | eff) DescribeOriginEndpointResponse
describeOriginEndpoint = AWS.request serviceName "DescribeOriginEndpoint" 


-- | Returns a collection of Channels.
listChannels :: forall eff. ListChannelsRequest -> Aff (err :: AWS.RequestError | eff) ListChannelsResponse
listChannels = AWS.request serviceName "ListChannels" 


-- | Returns a collection of OriginEndpoint records.
listOriginEndpoints :: forall eff. ListOriginEndpointsRequest -> Aff (err :: AWS.RequestError | eff) ListOriginEndpointsResponse
listOriginEndpoints = AWS.request serviceName "ListOriginEndpoints" 


-- | Changes the Channel ingest username and password.
rotateChannelCredentials :: forall eff. RotateChannelCredentialsRequest -> Aff (err :: AWS.RequestError | eff) RotateChannelCredentialsResponse
rotateChannelCredentials = AWS.request serviceName "RotateChannelCredentials" 


-- | Updates an existing Channel.
updateChannel :: forall eff. UpdateChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateChannelResponse
updateChannel = AWS.request serviceName "UpdateChannel" 


-- | Updates an existing OriginEndpoint.
updateOriginEndpoint :: forall eff. UpdateOriginEndpointRequest -> Aff (err :: AWS.RequestError | eff) UpdateOriginEndpointResponse
updateOriginEndpoint = AWS.request serviceName "UpdateOriginEndpoint" 


newtype AdMarkers = AdMarkers String


-- | A Channel resource configuration.
newtype Channel = Channel 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }


-- | Configuration parameters for a new Channel.
newtype ChannelCreateParameters = ChannelCreateParameters 
  { "Description" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  }


-- | A collection of Channel records.
newtype ChannelList = ChannelList 
  { "Channels" :: NullOrUndefined (ListOfChannel)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Configuration parameters for updating an existing Channel.
newtype ChannelUpdateParameters = ChannelUpdateParameters 
  { "Description" :: NullOrUndefined (String)
  }


-- | A new Channel configuration.
newtype CreateChannelRequest = CreateChannelRequest 
  { "Description" :: NullOrUndefined (String)
  , "Id" :: (String)
  }


newtype CreateChannelResponse = CreateChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }


-- | Configuration parameters used to create a new OriginEndpoint.
newtype CreateOriginEndpointRequest = CreateOriginEndpointRequest 
  { "ChannelId" :: (String)
  , "DashPackage" :: NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined (HlsPackage)
  , "Id" :: (String)
  , "ManifestName" :: NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined (Int)
  , "Whitelist" :: NullOrUndefined (ListOf__string)
  }


newtype CreateOriginEndpointResponse = CreateOriginEndpointResponse 
  { "Arn" :: NullOrUndefined (String)
  , "ChannelId" :: NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined (Int)
  , "Url" :: NullOrUndefined (String)
  , "Whitelist" :: NullOrUndefined (ListOf__string)
  }


-- | A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.
newtype DashEncryption = DashEncryption 
  { "KeyRotationIntervalSeconds" :: NullOrUndefined (Int)
  , "SpekeKeyProvider" :: (SpekeKeyProvider)
  }


-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
newtype DashPackage = DashPackage 
  { "Encryption" :: NullOrUndefined (DashEncryption)
  , "ManifestWindowSeconds" :: NullOrUndefined (Int)
  , "MinBufferTimeSeconds" :: NullOrUndefined (Int)
  , "MinUpdatePeriodSeconds" :: NullOrUndefined (Int)
  , "Profile" :: NullOrUndefined (Profile)
  , "SegmentDurationSeconds" :: NullOrUndefined (Int)
  , "StreamSelection" :: NullOrUndefined (StreamSelection)
  , "SuggestedPresentationDelaySeconds" :: NullOrUndefined (Int)
  }


newtype DeleteChannelRequest = DeleteChannelRequest 
  { "Id" :: (String)
  }


newtype DeleteChannelResponse = DeleteChannelResponse 
  { 
  }


newtype DeleteOriginEndpointRequest = DeleteOriginEndpointRequest 
  { "Id" :: (String)
  }


newtype DeleteOriginEndpointResponse = DeleteOriginEndpointResponse 
  { 
  }


newtype DescribeChannelRequest = DescribeChannelRequest 
  { "Id" :: (String)
  }


newtype DescribeChannelResponse = DescribeChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }


newtype DescribeOriginEndpointRequest = DescribeOriginEndpointRequest 
  { "Id" :: (String)
  }


newtype DescribeOriginEndpointResponse = DescribeOriginEndpointResponse 
  { "Arn" :: NullOrUndefined (String)
  , "ChannelId" :: NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined (Int)
  , "Url" :: NullOrUndefined (String)
  , "Whitelist" :: NullOrUndefined (ListOf__string)
  }


newtype EncryptionMethod = EncryptionMethod String


-- | The client is not authorized to access the requested resource.
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined (String)
  }


-- | An HTTP Live Streaming (HLS) encryption configuration.
newtype HlsEncryption = HlsEncryption 
  { "ConstantInitializationVector" :: NullOrUndefined (String)
  , "EncryptionMethod" :: NullOrUndefined (EncryptionMethod)
  , "KeyRotationIntervalSeconds" :: NullOrUndefined (Int)
  , "RepeatExtXKey" :: NullOrUndefined (Boolean)
  , "SpekeKeyProvider" :: (SpekeKeyProvider)
  }


-- | An HTTP Live Streaming (HLS) ingest resource configuration.
newtype HlsIngest = HlsIngest 
  { "IngestEndpoints" :: NullOrUndefined (ListOfIngestEndpoint)
  }


-- | An HTTP Live Streaming (HLS) packaging configuration.
newtype HlsPackage = HlsPackage 
  { "AdMarkers" :: NullOrUndefined (AdMarkers)
  , "Encryption" :: NullOrUndefined (HlsEncryption)
  , "IncludeIframeOnlyStream" :: NullOrUndefined (Boolean)
  , "PlaylistType" :: NullOrUndefined (PlaylistType)
  , "PlaylistWindowSeconds" :: NullOrUndefined (Int)
  , "ProgramDateTimeIntervalSeconds" :: NullOrUndefined (Int)
  , "SegmentDurationSeconds" :: NullOrUndefined (Int)
  , "StreamSelection" :: NullOrUndefined (StreamSelection)
  , "UseAudioRenditionGroup" :: NullOrUndefined (Boolean)
  }


-- | An endpoint for ingesting source content for a Channel.
newtype IngestEndpoint = IngestEndpoint 
  { "Password" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }


-- | An unexpected error occurred.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (String)
  }


newtype ListChannelsRequest = ListChannelsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListChannelsResponse = ListChannelsResponse 
  { "Channels" :: NullOrUndefined (ListOfChannel)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListOfChannel = ListOfChannel (Array Channel)


newtype ListOfIngestEndpoint = ListOfIngestEndpoint (Array IngestEndpoint)


newtype ListOfOriginEndpoint = ListOfOriginEndpoint (Array OriginEndpoint)


newtype ListOf__string = ListOf__string (Array String)


newtype ListOriginEndpointsRequest = ListOriginEndpointsRequest 
  { "ChannelId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListOriginEndpointsResponse = ListOriginEndpointsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "OriginEndpoints" :: NullOrUndefined (ListOfOriginEndpoint)
  }


newtype MaxResults = MaxResults Int


-- | A Microsoft Smooth Streaming (MSS) encryption configuration.
newtype MssEncryption = MssEncryption 
  { "SpekeKeyProvider" :: (SpekeKeyProvider)
  }


-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
newtype MssPackage = MssPackage 
  { "Encryption" :: NullOrUndefined (MssEncryption)
  , "ManifestWindowSeconds" :: NullOrUndefined (Int)
  , "SegmentDurationSeconds" :: NullOrUndefined (Int)
  , "StreamSelection" :: NullOrUndefined (StreamSelection)
  }


-- | The requested resource does not exist.
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (String)
  }


-- | An OriginEndpoint resource configuration.
newtype OriginEndpoint = OriginEndpoint 
  { "Arn" :: NullOrUndefined (String)
  , "ChannelId" :: NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined (Int)
  , "Url" :: NullOrUndefined (String)
  , "Whitelist" :: NullOrUndefined (ListOf__string)
  }


-- | Configuration parameters for a new OriginEndpoint.
newtype OriginEndpointCreateParameters = OriginEndpointCreateParameters 
  { "ChannelId" :: NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined (Int)
  , "Whitelist" :: NullOrUndefined (ListOf__string)
  }


-- | A collection of OriginEndpoint records.
newtype OriginEndpointList = OriginEndpointList 
  { "NextToken" :: NullOrUndefined (String)
  , "OriginEndpoints" :: NullOrUndefined (ListOfOriginEndpoint)
  }


-- | Configuration parameters for updating an existing OriginEndpoint.
newtype OriginEndpointUpdateParameters = OriginEndpointUpdateParameters 
  { "DashPackage" :: NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined (HlsPackage)
  , "ManifestName" :: NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined (Int)
  , "Whitelist" :: NullOrUndefined (ListOf__string)
  }


newtype PlaylistType = PlaylistType String


newtype Profile = Profile String


newtype RotateChannelCredentialsRequest = RotateChannelCredentialsRequest 
  { "Id" :: (String)
  }


newtype RotateChannelCredentialsResponse = RotateChannelCredentialsResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }


-- | An unexpected error occurred.
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message" :: NullOrUndefined (String)
  }


-- | A configuration for accessing an external Secure Packager and Encoder Key Exchange (SPEKE) service that will provide encryption keys.
newtype SpekeKeyProvider = SpekeKeyProvider 
  { "ResourceId" :: (String)
  , "RoleArn" :: (String)
  , "SystemIds" :: (ListOf__string)
  , "Url" :: (String)
  }


newtype StreamOrder = StreamOrder String


-- | A StreamSelection configuration.
newtype StreamSelection = StreamSelection 
  { "MaxVideoBitsPerSecond" :: NullOrUndefined (Int)
  , "MinVideoBitsPerSecond" :: NullOrUndefined (Int)
  , "StreamOrder" :: NullOrUndefined (StreamOrder)
  }


-- | The client has exceeded their resource or throttling limits.
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  }


-- | The parameters sent in the request are not valid.
newtype UnprocessableEntityException = UnprocessableEntityException 
  { "Message" :: NullOrUndefined (String)
  }


-- | Configuration parameters used to update the Channel.
newtype UpdateChannelRequest = UpdateChannelRequest 
  { "Description" :: NullOrUndefined (String)
  , "Id" :: (String)
  }


newtype UpdateChannelResponse = UpdateChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }


-- | Configuration parameters used to update an existing OriginEndpoint.
newtype UpdateOriginEndpointRequest = UpdateOriginEndpointRequest 
  { "DashPackage" :: NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined (HlsPackage)
  , "Id" :: (String)
  , "ManifestName" :: NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined (Int)
  , "Whitelist" :: NullOrUndefined (ListOf__string)
  }


newtype UpdateOriginEndpointResponse = UpdateOriginEndpointResponse 
  { "Arn" :: NullOrUndefined (String)
  , "ChannelId" :: NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined (Int)
  , "Url" :: NullOrUndefined (String)
  , "Whitelist" :: NullOrUndefined (ListOf__string)
  }
