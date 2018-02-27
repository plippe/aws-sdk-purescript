

-- | AWS Elemental MediaPackage
module AWS.MediaPackage where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeAdMarkers :: Newtype AdMarkers _


-- | A Channel resource configuration.
newtype Channel = Channel 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }
derive instance newtypeChannel :: Newtype Channel _


-- | Configuration parameters for a new Channel.
newtype ChannelCreateParameters = ChannelCreateParameters 
  { "Description" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  }
derive instance newtypeChannelCreateParameters :: Newtype ChannelCreateParameters _


-- | A collection of Channel records.
newtype ChannelList = ChannelList 
  { "Channels" :: NullOrUndefined (ListOfChannel)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeChannelList :: Newtype ChannelList _


-- | Configuration parameters for updating an existing Channel.
newtype ChannelUpdateParameters = ChannelUpdateParameters 
  { "Description" :: NullOrUndefined (String)
  }
derive instance newtypeChannelUpdateParameters :: Newtype ChannelUpdateParameters _


-- | A new Channel configuration.
newtype CreateChannelRequest = CreateChannelRequest 
  { "Description" :: NullOrUndefined (String)
  , "Id" :: (String)
  }
derive instance newtypeCreateChannelRequest :: Newtype CreateChannelRequest _


newtype CreateChannelResponse = CreateChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }
derive instance newtypeCreateChannelResponse :: Newtype CreateChannelResponse _


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
derive instance newtypeCreateOriginEndpointRequest :: Newtype CreateOriginEndpointRequest _


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
derive instance newtypeCreateOriginEndpointResponse :: Newtype CreateOriginEndpointResponse _


-- | A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.
newtype DashEncryption = DashEncryption 
  { "KeyRotationIntervalSeconds" :: NullOrUndefined (Int)
  , "SpekeKeyProvider" :: (SpekeKeyProvider)
  }
derive instance newtypeDashEncryption :: Newtype DashEncryption _


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
derive instance newtypeDashPackage :: Newtype DashPackage _


newtype DeleteChannelRequest = DeleteChannelRequest 
  { "Id" :: (String)
  }
derive instance newtypeDeleteChannelRequest :: Newtype DeleteChannelRequest _


newtype DeleteChannelResponse = DeleteChannelResponse 
  { 
  }
derive instance newtypeDeleteChannelResponse :: Newtype DeleteChannelResponse _


newtype DeleteOriginEndpointRequest = DeleteOriginEndpointRequest 
  { "Id" :: (String)
  }
derive instance newtypeDeleteOriginEndpointRequest :: Newtype DeleteOriginEndpointRequest _


newtype DeleteOriginEndpointResponse = DeleteOriginEndpointResponse 
  { 
  }
derive instance newtypeDeleteOriginEndpointResponse :: Newtype DeleteOriginEndpointResponse _


newtype DescribeChannelRequest = DescribeChannelRequest 
  { "Id" :: (String)
  }
derive instance newtypeDescribeChannelRequest :: Newtype DescribeChannelRequest _


newtype DescribeChannelResponse = DescribeChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeChannelResponse :: Newtype DescribeChannelResponse _


newtype DescribeOriginEndpointRequest = DescribeOriginEndpointRequest 
  { "Id" :: (String)
  }
derive instance newtypeDescribeOriginEndpointRequest :: Newtype DescribeOriginEndpointRequest _


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
derive instance newtypeDescribeOriginEndpointResponse :: Newtype DescribeOriginEndpointResponse _


newtype EncryptionMethod = EncryptionMethod String
derive instance newtypeEncryptionMethod :: Newtype EncryptionMethod _


-- | The client is not authorized to access the requested resource.
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _


-- | An HTTP Live Streaming (HLS) encryption configuration.
newtype HlsEncryption = HlsEncryption 
  { "ConstantInitializationVector" :: NullOrUndefined (String)
  , "EncryptionMethod" :: NullOrUndefined (EncryptionMethod)
  , "KeyRotationIntervalSeconds" :: NullOrUndefined (Int)
  , "RepeatExtXKey" :: NullOrUndefined (Boolean)
  , "SpekeKeyProvider" :: (SpekeKeyProvider)
  }
derive instance newtypeHlsEncryption :: Newtype HlsEncryption _


-- | An HTTP Live Streaming (HLS) ingest resource configuration.
newtype HlsIngest = HlsIngest 
  { "IngestEndpoints" :: NullOrUndefined (ListOfIngestEndpoint)
  }
derive instance newtypeHlsIngest :: Newtype HlsIngest _


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
derive instance newtypeHlsPackage :: Newtype HlsPackage _


-- | An endpoint for ingesting source content for a Channel.
newtype IngestEndpoint = IngestEndpoint 
  { "Password" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }
derive instance newtypeIngestEndpoint :: Newtype IngestEndpoint _


-- | An unexpected error occurred.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _


newtype ListChannelsRequest = ListChannelsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListChannelsRequest :: Newtype ListChannelsRequest _


newtype ListChannelsResponse = ListChannelsResponse 
  { "Channels" :: NullOrUndefined (ListOfChannel)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListChannelsResponse :: Newtype ListChannelsResponse _


newtype ListOfChannel = ListOfChannel (Array Channel)
derive instance newtypeListOfChannel :: Newtype ListOfChannel _


newtype ListOfIngestEndpoint = ListOfIngestEndpoint (Array IngestEndpoint)
derive instance newtypeListOfIngestEndpoint :: Newtype ListOfIngestEndpoint _


newtype ListOfOriginEndpoint = ListOfOriginEndpoint (Array OriginEndpoint)
derive instance newtypeListOfOriginEndpoint :: Newtype ListOfOriginEndpoint _


newtype ListOf__string = ListOf__string (Array String)
derive instance newtypeListOf__string :: Newtype ListOf__string _


newtype ListOriginEndpointsRequest = ListOriginEndpointsRequest 
  { "ChannelId" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListOriginEndpointsRequest :: Newtype ListOriginEndpointsRequest _


newtype ListOriginEndpointsResponse = ListOriginEndpointsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "OriginEndpoints" :: NullOrUndefined (ListOfOriginEndpoint)
  }
derive instance newtypeListOriginEndpointsResponse :: Newtype ListOriginEndpointsResponse _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | A Microsoft Smooth Streaming (MSS) encryption configuration.
newtype MssEncryption = MssEncryption 
  { "SpekeKeyProvider" :: (SpekeKeyProvider)
  }
derive instance newtypeMssEncryption :: Newtype MssEncryption _


-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
newtype MssPackage = MssPackage 
  { "Encryption" :: NullOrUndefined (MssEncryption)
  , "ManifestWindowSeconds" :: NullOrUndefined (Int)
  , "SegmentDurationSeconds" :: NullOrUndefined (Int)
  , "StreamSelection" :: NullOrUndefined (StreamSelection)
  }
derive instance newtypeMssPackage :: Newtype MssPackage _


-- | The requested resource does not exist.
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


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
derive instance newtypeOriginEndpoint :: Newtype OriginEndpoint _


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
derive instance newtypeOriginEndpointCreateParameters :: Newtype OriginEndpointCreateParameters _


-- | A collection of OriginEndpoint records.
newtype OriginEndpointList = OriginEndpointList 
  { "NextToken" :: NullOrUndefined (String)
  , "OriginEndpoints" :: NullOrUndefined (ListOfOriginEndpoint)
  }
derive instance newtypeOriginEndpointList :: Newtype OriginEndpointList _


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
derive instance newtypeOriginEndpointUpdateParameters :: Newtype OriginEndpointUpdateParameters _


newtype PlaylistType = PlaylistType String
derive instance newtypePlaylistType :: Newtype PlaylistType _


newtype Profile = Profile String
derive instance newtypeProfile :: Newtype Profile _


newtype RotateChannelCredentialsRequest = RotateChannelCredentialsRequest 
  { "Id" :: (String)
  }
derive instance newtypeRotateChannelCredentialsRequest :: Newtype RotateChannelCredentialsRequest _


newtype RotateChannelCredentialsResponse = RotateChannelCredentialsResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }
derive instance newtypeRotateChannelCredentialsResponse :: Newtype RotateChannelCredentialsResponse _


-- | An unexpected error occurred.
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _


-- | A configuration for accessing an external Secure Packager and Encoder Key Exchange (SPEKE) service that will provide encryption keys.
newtype SpekeKeyProvider = SpekeKeyProvider 
  { "ResourceId" :: (String)
  , "RoleArn" :: (String)
  , "SystemIds" :: (ListOf__string)
  , "Url" :: (String)
  }
derive instance newtypeSpekeKeyProvider :: Newtype SpekeKeyProvider _


newtype StreamOrder = StreamOrder String
derive instance newtypeStreamOrder :: Newtype StreamOrder _


-- | A StreamSelection configuration.
newtype StreamSelection = StreamSelection 
  { "MaxVideoBitsPerSecond" :: NullOrUndefined (Int)
  , "MinVideoBitsPerSecond" :: NullOrUndefined (Int)
  , "StreamOrder" :: NullOrUndefined (StreamOrder)
  }
derive instance newtypeStreamSelection :: Newtype StreamSelection _


-- | The client has exceeded their resource or throttling limits.
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


-- | The parameters sent in the request are not valid.
newtype UnprocessableEntityException = UnprocessableEntityException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeUnprocessableEntityException :: Newtype UnprocessableEntityException _


-- | Configuration parameters used to update the Channel.
newtype UpdateChannelRequest = UpdateChannelRequest 
  { "Description" :: NullOrUndefined (String)
  , "Id" :: (String)
  }
derive instance newtypeUpdateChannelRequest :: Newtype UpdateChannelRequest _


newtype UpdateChannelResponse = UpdateChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateChannelResponse :: Newtype UpdateChannelResponse _


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
derive instance newtypeUpdateOriginEndpointRequest :: Newtype UpdateOriginEndpointRequest _


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
derive instance newtypeUpdateOriginEndpointResponse :: Newtype UpdateOriginEndpointResponse _
