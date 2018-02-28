

-- | AWS Elemental MediaPackage
module AWS.MediaPackage where

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

serviceName = "MediaPackage" :: String


-- | Creates a new Channel.
createChannel :: forall eff. CreateChannelRequest -> Aff (exception :: EXCEPTION | eff) CreateChannelResponse
createChannel = Request.request serviceName "createChannel" 


-- | Creates a new OriginEndpoint record.
createOriginEndpoint :: forall eff. CreateOriginEndpointRequest -> Aff (exception :: EXCEPTION | eff) CreateOriginEndpointResponse
createOriginEndpoint = Request.request serviceName "createOriginEndpoint" 


-- | Deletes an existing Channel.
deleteChannel :: forall eff. DeleteChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteChannelResponse
deleteChannel = Request.request serviceName "deleteChannel" 


-- | Deletes an existing OriginEndpoint.
deleteOriginEndpoint :: forall eff. DeleteOriginEndpointRequest -> Aff (exception :: EXCEPTION | eff) DeleteOriginEndpointResponse
deleteOriginEndpoint = Request.request serviceName "deleteOriginEndpoint" 


-- | Gets details about a Channel.
describeChannel :: forall eff. DescribeChannelRequest -> Aff (exception :: EXCEPTION | eff) DescribeChannelResponse
describeChannel = Request.request serviceName "describeChannel" 


-- | Gets details about an existing OriginEndpoint.
describeOriginEndpoint :: forall eff. DescribeOriginEndpointRequest -> Aff (exception :: EXCEPTION | eff) DescribeOriginEndpointResponse
describeOriginEndpoint = Request.request serviceName "describeOriginEndpoint" 


-- | Returns a collection of Channels.
listChannels :: forall eff. ListChannelsRequest -> Aff (exception :: EXCEPTION | eff) ListChannelsResponse
listChannels = Request.request serviceName "listChannels" 


-- | Returns a collection of OriginEndpoint records.
listOriginEndpoints :: forall eff. ListOriginEndpointsRequest -> Aff (exception :: EXCEPTION | eff) ListOriginEndpointsResponse
listOriginEndpoints = Request.request serviceName "listOriginEndpoints" 


-- | Changes the Channel ingest username and password.
rotateChannelCredentials :: forall eff. RotateChannelCredentialsRequest -> Aff (exception :: EXCEPTION | eff) RotateChannelCredentialsResponse
rotateChannelCredentials = Request.request serviceName "rotateChannelCredentials" 


-- | Updates an existing Channel.
updateChannel :: forall eff. UpdateChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateChannelResponse
updateChannel = Request.request serviceName "updateChannel" 


-- | Updates an existing OriginEndpoint.
updateOriginEndpoint :: forall eff. UpdateOriginEndpointRequest -> Aff (exception :: EXCEPTION | eff) UpdateOriginEndpointResponse
updateOriginEndpoint = Request.request serviceName "updateOriginEndpoint" 


newtype AdMarkers = AdMarkers String
derive instance newtypeAdMarkers :: Newtype AdMarkers _
derive instance repGenericAdMarkers :: Generic AdMarkers _
instance showAdMarkers :: Show AdMarkers where
  show = genericShow
instance decodeAdMarkers :: Decode AdMarkers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdMarkers :: Encode AdMarkers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A Channel resource configuration.
newtype Channel = Channel 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined.NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeChannel :: Newtype Channel _
derive instance repGenericChannel :: Generic Channel _
instance showChannel :: Show Channel where
  show = genericShow
instance decodeChannel :: Decode Channel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannel :: Encode Channel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Configuration parameters for a new Channel.
newtype ChannelCreateParameters = ChannelCreateParameters 
  { "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeChannelCreateParameters :: Newtype ChannelCreateParameters _
derive instance repGenericChannelCreateParameters :: Generic ChannelCreateParameters _
instance showChannelCreateParameters :: Show ChannelCreateParameters where
  show = genericShow
instance decodeChannelCreateParameters :: Decode ChannelCreateParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelCreateParameters :: Encode ChannelCreateParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A collection of Channel records.
newtype ChannelList = ChannelList 
  { "Channels" :: NullOrUndefined.NullOrUndefined (ListOfChannel)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeChannelList :: Newtype ChannelList _
derive instance repGenericChannelList :: Generic ChannelList _
instance showChannelList :: Show ChannelList where
  show = genericShow
instance decodeChannelList :: Decode ChannelList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelList :: Encode ChannelList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Configuration parameters for updating an existing Channel.
newtype ChannelUpdateParameters = ChannelUpdateParameters 
  { "Description" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeChannelUpdateParameters :: Newtype ChannelUpdateParameters _
derive instance repGenericChannelUpdateParameters :: Generic ChannelUpdateParameters _
instance showChannelUpdateParameters :: Show ChannelUpdateParameters where
  show = genericShow
instance decodeChannelUpdateParameters :: Decode ChannelUpdateParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelUpdateParameters :: Encode ChannelUpdateParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A new Channel configuration.
newtype CreateChannelRequest = CreateChannelRequest 
  { "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: (String)
  }
derive instance newtypeCreateChannelRequest :: Newtype CreateChannelRequest _
derive instance repGenericCreateChannelRequest :: Generic CreateChannelRequest _
instance showCreateChannelRequest :: Show CreateChannelRequest where
  show = genericShow
instance decodeCreateChannelRequest :: Decode CreateChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateChannelRequest :: Encode CreateChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateChannelResponse = CreateChannelResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined.NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateChannelResponse :: Newtype CreateChannelResponse _
derive instance repGenericCreateChannelResponse :: Generic CreateChannelResponse _
instance showCreateChannelResponse :: Show CreateChannelResponse where
  show = genericShow
instance decodeCreateChannelResponse :: Decode CreateChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateChannelResponse :: Encode CreateChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Configuration parameters used to create a new OriginEndpoint.
newtype CreateOriginEndpointRequest = CreateOriginEndpointRequest 
  { "ChannelId" :: (String)
  , "DashPackage" :: NullOrUndefined.NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined.NullOrUndefined (HlsPackage)
  , "Id" :: (String)
  , "ManifestName" :: NullOrUndefined.NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined.NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "Whitelist" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeCreateOriginEndpointRequest :: Newtype CreateOriginEndpointRequest _
derive instance repGenericCreateOriginEndpointRequest :: Generic CreateOriginEndpointRequest _
instance showCreateOriginEndpointRequest :: Show CreateOriginEndpointRequest where
  show = genericShow
instance decodeCreateOriginEndpointRequest :: Decode CreateOriginEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateOriginEndpointRequest :: Encode CreateOriginEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateOriginEndpointResponse = CreateOriginEndpointResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "ChannelId" :: NullOrUndefined.NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined.NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined.NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined.NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined.NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  , "Whitelist" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeCreateOriginEndpointResponse :: Newtype CreateOriginEndpointResponse _
derive instance repGenericCreateOriginEndpointResponse :: Generic CreateOriginEndpointResponse _
instance showCreateOriginEndpointResponse :: Show CreateOriginEndpointResponse where
  show = genericShow
instance decodeCreateOriginEndpointResponse :: Decode CreateOriginEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateOriginEndpointResponse :: Encode CreateOriginEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A Dynamic Adaptive Streaming over HTTP (DASH) encryption configuration.
newtype DashEncryption = DashEncryption 
  { "KeyRotationIntervalSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "SpekeKeyProvider" :: (SpekeKeyProvider)
  }
derive instance newtypeDashEncryption :: Newtype DashEncryption _
derive instance repGenericDashEncryption :: Generic DashEncryption _
instance showDashEncryption :: Show DashEncryption where
  show = genericShow
instance decodeDashEncryption :: Decode DashEncryption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashEncryption :: Encode DashEncryption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
newtype DashPackage = DashPackage 
  { "Encryption" :: NullOrUndefined.NullOrUndefined (DashEncryption)
  , "ManifestWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinBufferTimeSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinUpdatePeriodSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "Profile" :: NullOrUndefined.NullOrUndefined (Profile)
  , "SegmentDurationSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "StreamSelection" :: NullOrUndefined.NullOrUndefined (StreamSelection)
  , "SuggestedPresentationDelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDashPackage :: Newtype DashPackage _
derive instance repGenericDashPackage :: Generic DashPackage _
instance showDashPackage :: Show DashPackage where
  show = genericShow
instance decodeDashPackage :: Decode DashPackage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashPackage :: Encode DashPackage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteChannelRequest = DeleteChannelRequest 
  { "Id" :: (String)
  }
derive instance newtypeDeleteChannelRequest :: Newtype DeleteChannelRequest _
derive instance repGenericDeleteChannelRequest :: Generic DeleteChannelRequest _
instance showDeleteChannelRequest :: Show DeleteChannelRequest where
  show = genericShow
instance decodeDeleteChannelRequest :: Decode DeleteChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteChannelRequest :: Encode DeleteChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteChannelResponse = DeleteChannelResponse Types.NoArguments
derive instance newtypeDeleteChannelResponse :: Newtype DeleteChannelResponse _
derive instance repGenericDeleteChannelResponse :: Generic DeleteChannelResponse _
instance showDeleteChannelResponse :: Show DeleteChannelResponse where
  show = genericShow
instance decodeDeleteChannelResponse :: Decode DeleteChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteChannelResponse :: Encode DeleteChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteOriginEndpointRequest = DeleteOriginEndpointRequest 
  { "Id" :: (String)
  }
derive instance newtypeDeleteOriginEndpointRequest :: Newtype DeleteOriginEndpointRequest _
derive instance repGenericDeleteOriginEndpointRequest :: Generic DeleteOriginEndpointRequest _
instance showDeleteOriginEndpointRequest :: Show DeleteOriginEndpointRequest where
  show = genericShow
instance decodeDeleteOriginEndpointRequest :: Decode DeleteOriginEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteOriginEndpointRequest :: Encode DeleteOriginEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteOriginEndpointResponse = DeleteOriginEndpointResponse Types.NoArguments
derive instance newtypeDeleteOriginEndpointResponse :: Newtype DeleteOriginEndpointResponse _
derive instance repGenericDeleteOriginEndpointResponse :: Generic DeleteOriginEndpointResponse _
instance showDeleteOriginEndpointResponse :: Show DeleteOriginEndpointResponse where
  show = genericShow
instance decodeDeleteOriginEndpointResponse :: Decode DeleteOriginEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteOriginEndpointResponse :: Encode DeleteOriginEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeChannelRequest = DescribeChannelRequest 
  { "Id" :: (String)
  }
derive instance newtypeDescribeChannelRequest :: Newtype DescribeChannelRequest _
derive instance repGenericDescribeChannelRequest :: Generic DescribeChannelRequest _
instance showDescribeChannelRequest :: Show DescribeChannelRequest where
  show = genericShow
instance decodeDescribeChannelRequest :: Decode DescribeChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeChannelRequest :: Encode DescribeChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeChannelResponse = DescribeChannelResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined.NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeChannelResponse :: Newtype DescribeChannelResponse _
derive instance repGenericDescribeChannelResponse :: Generic DescribeChannelResponse _
instance showDescribeChannelResponse :: Show DescribeChannelResponse where
  show = genericShow
instance decodeDescribeChannelResponse :: Decode DescribeChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeChannelResponse :: Encode DescribeChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeOriginEndpointRequest = DescribeOriginEndpointRequest 
  { "Id" :: (String)
  }
derive instance newtypeDescribeOriginEndpointRequest :: Newtype DescribeOriginEndpointRequest _
derive instance repGenericDescribeOriginEndpointRequest :: Generic DescribeOriginEndpointRequest _
instance showDescribeOriginEndpointRequest :: Show DescribeOriginEndpointRequest where
  show = genericShow
instance decodeDescribeOriginEndpointRequest :: Decode DescribeOriginEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeOriginEndpointRequest :: Encode DescribeOriginEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeOriginEndpointResponse = DescribeOriginEndpointResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "ChannelId" :: NullOrUndefined.NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined.NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined.NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined.NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined.NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  , "Whitelist" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeDescribeOriginEndpointResponse :: Newtype DescribeOriginEndpointResponse _
derive instance repGenericDescribeOriginEndpointResponse :: Generic DescribeOriginEndpointResponse _
instance showDescribeOriginEndpointResponse :: Show DescribeOriginEndpointResponse where
  show = genericShow
instance decodeDescribeOriginEndpointResponse :: Decode DescribeOriginEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeOriginEndpointResponse :: Encode DescribeOriginEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptionMethod = EncryptionMethod String
derive instance newtypeEncryptionMethod :: Newtype EncryptionMethod _
derive instance repGenericEncryptionMethod :: Generic EncryptionMethod _
instance showEncryptionMethod :: Show EncryptionMethod where
  show = genericShow
instance decodeEncryptionMethod :: Decode EncryptionMethod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionMethod :: Encode EncryptionMethod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The client is not authorized to access the requested resource.
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _
derive instance repGenericForbiddenException :: Generic ForbiddenException _
instance showForbiddenException :: Show ForbiddenException where
  show = genericShow
instance decodeForbiddenException :: Decode ForbiddenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForbiddenException :: Encode ForbiddenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An HTTP Live Streaming (HLS) encryption configuration.
newtype HlsEncryption = HlsEncryption 
  { "ConstantInitializationVector" :: NullOrUndefined.NullOrUndefined (String)
  , "EncryptionMethod" :: NullOrUndefined.NullOrUndefined (EncryptionMethod)
  , "KeyRotationIntervalSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "RepeatExtXKey" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SpekeKeyProvider" :: (SpekeKeyProvider)
  }
derive instance newtypeHlsEncryption :: Newtype HlsEncryption _
derive instance repGenericHlsEncryption :: Generic HlsEncryption _
instance showHlsEncryption :: Show HlsEncryption where
  show = genericShow
instance decodeHlsEncryption :: Decode HlsEncryption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsEncryption :: Encode HlsEncryption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An HTTP Live Streaming (HLS) ingest resource configuration.
newtype HlsIngest = HlsIngest 
  { "IngestEndpoints" :: NullOrUndefined.NullOrUndefined (ListOfIngestEndpoint)
  }
derive instance newtypeHlsIngest :: Newtype HlsIngest _
derive instance repGenericHlsIngest :: Generic HlsIngest _
instance showHlsIngest :: Show HlsIngest where
  show = genericShow
instance decodeHlsIngest :: Decode HlsIngest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsIngest :: Encode HlsIngest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An HTTP Live Streaming (HLS) packaging configuration.
newtype HlsPackage = HlsPackage 
  { "AdMarkers" :: NullOrUndefined.NullOrUndefined (AdMarkers)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (HlsEncryption)
  , "IncludeIframeOnlyStream" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PlaylistType" :: NullOrUndefined.NullOrUndefined (PlaylistType)
  , "PlaylistWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "ProgramDateTimeIntervalSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "SegmentDurationSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "StreamSelection" :: NullOrUndefined.NullOrUndefined (StreamSelection)
  , "UseAudioRenditionGroup" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeHlsPackage :: Newtype HlsPackage _
derive instance repGenericHlsPackage :: Generic HlsPackage _
instance showHlsPackage :: Show HlsPackage where
  show = genericShow
instance decodeHlsPackage :: Decode HlsPackage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsPackage :: Encode HlsPackage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An endpoint for ingesting source content for a Channel.
newtype IngestEndpoint = IngestEndpoint 
  { "Password" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  , "Username" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeIngestEndpoint :: Newtype IngestEndpoint _
derive instance repGenericIngestEndpoint :: Generic IngestEndpoint _
instance showIngestEndpoint :: Show IngestEndpoint where
  show = genericShow
instance decodeIngestEndpoint :: Decode IngestEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIngestEndpoint :: Encode IngestEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An unexpected error occurred.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _
derive instance repGenericInternalServerErrorException :: Generic InternalServerErrorException _
instance showInternalServerErrorException :: Show InternalServerErrorException where
  show = genericShow
instance decodeInternalServerErrorException :: Decode InternalServerErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerErrorException :: Encode InternalServerErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListChannelsRequest = ListChannelsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListChannelsRequest :: Newtype ListChannelsRequest _
derive instance repGenericListChannelsRequest :: Generic ListChannelsRequest _
instance showListChannelsRequest :: Show ListChannelsRequest where
  show = genericShow
instance decodeListChannelsRequest :: Decode ListChannelsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListChannelsRequest :: Encode ListChannelsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListChannelsResponse = ListChannelsResponse 
  { "Channels" :: NullOrUndefined.NullOrUndefined (ListOfChannel)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListChannelsResponse :: Newtype ListChannelsResponse _
derive instance repGenericListChannelsResponse :: Generic ListChannelsResponse _
instance showListChannelsResponse :: Show ListChannelsResponse where
  show = genericShow
instance decodeListChannelsResponse :: Decode ListChannelsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListChannelsResponse :: Encode ListChannelsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfChannel = ListOfChannel (Array Channel)
derive instance newtypeListOfChannel :: Newtype ListOfChannel _
derive instance repGenericListOfChannel :: Generic ListOfChannel _
instance showListOfChannel :: Show ListOfChannel where
  show = genericShow
instance decodeListOfChannel :: Decode ListOfChannel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfChannel :: Encode ListOfChannel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfIngestEndpoint = ListOfIngestEndpoint (Array IngestEndpoint)
derive instance newtypeListOfIngestEndpoint :: Newtype ListOfIngestEndpoint _
derive instance repGenericListOfIngestEndpoint :: Generic ListOfIngestEndpoint _
instance showListOfIngestEndpoint :: Show ListOfIngestEndpoint where
  show = genericShow
instance decodeListOfIngestEndpoint :: Decode ListOfIngestEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfIngestEndpoint :: Encode ListOfIngestEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfOriginEndpoint = ListOfOriginEndpoint (Array OriginEndpoint)
derive instance newtypeListOfOriginEndpoint :: Newtype ListOfOriginEndpoint _
derive instance repGenericListOfOriginEndpoint :: Generic ListOfOriginEndpoint _
instance showListOfOriginEndpoint :: Show ListOfOriginEndpoint where
  show = genericShow
instance decodeListOfOriginEndpoint :: Decode ListOfOriginEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOriginEndpoint :: Encode ListOfOriginEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOf__string = ListOf__string (Array String)
derive instance newtypeListOf__string :: Newtype ListOf__string _
derive instance repGenericListOf__string :: Generic ListOf__string _
instance showListOf__string :: Show ListOf__string where
  show = genericShow
instance decodeListOf__string :: Decode ListOf__string where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOf__string :: Encode ListOf__string where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOriginEndpointsRequest = ListOriginEndpointsRequest 
  { "ChannelId" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListOriginEndpointsRequest :: Newtype ListOriginEndpointsRequest _
derive instance repGenericListOriginEndpointsRequest :: Generic ListOriginEndpointsRequest _
instance showListOriginEndpointsRequest :: Show ListOriginEndpointsRequest where
  show = genericShow
instance decodeListOriginEndpointsRequest :: Decode ListOriginEndpointsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOriginEndpointsRequest :: Encode ListOriginEndpointsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOriginEndpointsResponse = ListOriginEndpointsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "OriginEndpoints" :: NullOrUndefined.NullOrUndefined (ListOfOriginEndpoint)
  }
derive instance newtypeListOriginEndpointsResponse :: Newtype ListOriginEndpointsResponse _
derive instance repGenericListOriginEndpointsResponse :: Generic ListOriginEndpointsResponse _
instance showListOriginEndpointsResponse :: Show ListOriginEndpointsResponse where
  show = genericShow
instance decodeListOriginEndpointsResponse :: Decode ListOriginEndpointsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOriginEndpointsResponse :: Encode ListOriginEndpointsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A Microsoft Smooth Streaming (MSS) encryption configuration.
newtype MssEncryption = MssEncryption 
  { "SpekeKeyProvider" :: (SpekeKeyProvider)
  }
derive instance newtypeMssEncryption :: Newtype MssEncryption _
derive instance repGenericMssEncryption :: Generic MssEncryption _
instance showMssEncryption :: Show MssEncryption where
  show = genericShow
instance decodeMssEncryption :: Decode MssEncryption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMssEncryption :: Encode MssEncryption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
newtype MssPackage = MssPackage 
  { "Encryption" :: NullOrUndefined.NullOrUndefined (MssEncryption)
  , "ManifestWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "SegmentDurationSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "StreamSelection" :: NullOrUndefined.NullOrUndefined (StreamSelection)
  }
derive instance newtypeMssPackage :: Newtype MssPackage _
derive instance repGenericMssPackage :: Generic MssPackage _
instance showMssPackage :: Show MssPackage where
  show = genericShow
instance decodeMssPackage :: Decode MssPackage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMssPackage :: Encode MssPackage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The requested resource does not exist.
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An OriginEndpoint resource configuration.
newtype OriginEndpoint = OriginEndpoint 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "ChannelId" :: NullOrUndefined.NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined.NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined.NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined.NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined.NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  , "Whitelist" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeOriginEndpoint :: Newtype OriginEndpoint _
derive instance repGenericOriginEndpoint :: Generic OriginEndpoint _
instance showOriginEndpoint :: Show OriginEndpoint where
  show = genericShow
instance decodeOriginEndpoint :: Decode OriginEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginEndpoint :: Encode OriginEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Configuration parameters for a new OriginEndpoint.
newtype OriginEndpointCreateParameters = OriginEndpointCreateParameters 
  { "ChannelId" :: NullOrUndefined.NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined.NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined.NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined.NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined.NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "Whitelist" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeOriginEndpointCreateParameters :: Newtype OriginEndpointCreateParameters _
derive instance repGenericOriginEndpointCreateParameters :: Generic OriginEndpointCreateParameters _
instance showOriginEndpointCreateParameters :: Show OriginEndpointCreateParameters where
  show = genericShow
instance decodeOriginEndpointCreateParameters :: Decode OriginEndpointCreateParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginEndpointCreateParameters :: Encode OriginEndpointCreateParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A collection of OriginEndpoint records.
newtype OriginEndpointList = OriginEndpointList 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "OriginEndpoints" :: NullOrUndefined.NullOrUndefined (ListOfOriginEndpoint)
  }
derive instance newtypeOriginEndpointList :: Newtype OriginEndpointList _
derive instance repGenericOriginEndpointList :: Generic OriginEndpointList _
instance showOriginEndpointList :: Show OriginEndpointList where
  show = genericShow
instance decodeOriginEndpointList :: Decode OriginEndpointList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginEndpointList :: Encode OriginEndpointList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Configuration parameters for updating an existing OriginEndpoint.
newtype OriginEndpointUpdateParameters = OriginEndpointUpdateParameters 
  { "DashPackage" :: NullOrUndefined.NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined.NullOrUndefined (HlsPackage)
  , "ManifestName" :: NullOrUndefined.NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined.NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "Whitelist" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeOriginEndpointUpdateParameters :: Newtype OriginEndpointUpdateParameters _
derive instance repGenericOriginEndpointUpdateParameters :: Generic OriginEndpointUpdateParameters _
instance showOriginEndpointUpdateParameters :: Show OriginEndpointUpdateParameters where
  show = genericShow
instance decodeOriginEndpointUpdateParameters :: Decode OriginEndpointUpdateParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOriginEndpointUpdateParameters :: Encode OriginEndpointUpdateParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlaylistType = PlaylistType String
derive instance newtypePlaylistType :: Newtype PlaylistType _
derive instance repGenericPlaylistType :: Generic PlaylistType _
instance showPlaylistType :: Show PlaylistType where
  show = genericShow
instance decodePlaylistType :: Decode PlaylistType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlaylistType :: Encode PlaylistType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Profile = Profile String
derive instance newtypeProfile :: Newtype Profile _
derive instance repGenericProfile :: Generic Profile _
instance showProfile :: Show Profile where
  show = genericShow
instance decodeProfile :: Decode Profile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProfile :: Encode Profile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RotateChannelCredentialsRequest = RotateChannelCredentialsRequest 
  { "Id" :: (String)
  }
derive instance newtypeRotateChannelCredentialsRequest :: Newtype RotateChannelCredentialsRequest _
derive instance repGenericRotateChannelCredentialsRequest :: Generic RotateChannelCredentialsRequest _
instance showRotateChannelCredentialsRequest :: Show RotateChannelCredentialsRequest where
  show = genericShow
instance decodeRotateChannelCredentialsRequest :: Decode RotateChannelCredentialsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRotateChannelCredentialsRequest :: Encode RotateChannelCredentialsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RotateChannelCredentialsResponse = RotateChannelCredentialsResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined.NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRotateChannelCredentialsResponse :: Newtype RotateChannelCredentialsResponse _
derive instance repGenericRotateChannelCredentialsResponse :: Generic RotateChannelCredentialsResponse _
instance showRotateChannelCredentialsResponse :: Show RotateChannelCredentialsResponse where
  show = genericShow
instance decodeRotateChannelCredentialsResponse :: Decode RotateChannelCredentialsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRotateChannelCredentialsResponse :: Encode RotateChannelCredentialsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An unexpected error occurred.
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A configuration for accessing an external Secure Packager and Encoder Key Exchange (SPEKE) service that will provide encryption keys.
newtype SpekeKeyProvider = SpekeKeyProvider 
  { "ResourceId" :: (String)
  , "RoleArn" :: (String)
  , "SystemIds" :: (ListOf__string)
  , "Url" :: (String)
  }
derive instance newtypeSpekeKeyProvider :: Newtype SpekeKeyProvider _
derive instance repGenericSpekeKeyProvider :: Generic SpekeKeyProvider _
instance showSpekeKeyProvider :: Show SpekeKeyProvider where
  show = genericShow
instance decodeSpekeKeyProvider :: Decode SpekeKeyProvider where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSpekeKeyProvider :: Encode SpekeKeyProvider where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamOrder = StreamOrder String
derive instance newtypeStreamOrder :: Newtype StreamOrder _
derive instance repGenericStreamOrder :: Generic StreamOrder _
instance showStreamOrder :: Show StreamOrder where
  show = genericShow
instance decodeStreamOrder :: Decode StreamOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamOrder :: Encode StreamOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A StreamSelection configuration.
newtype StreamSelection = StreamSelection 
  { "MaxVideoBitsPerSecond" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinVideoBitsPerSecond" :: NullOrUndefined.NullOrUndefined (Int)
  , "StreamOrder" :: NullOrUndefined.NullOrUndefined (StreamOrder)
  }
derive instance newtypeStreamSelection :: Newtype StreamSelection _
derive instance repGenericStreamSelection :: Generic StreamSelection _
instance showStreamSelection :: Show StreamSelection where
  show = genericShow
instance decodeStreamSelection :: Decode StreamSelection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamSelection :: Encode StreamSelection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The client has exceeded their resource or throttling limits.
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The parameters sent in the request are not valid.
newtype UnprocessableEntityException = UnprocessableEntityException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnprocessableEntityException :: Newtype UnprocessableEntityException _
derive instance repGenericUnprocessableEntityException :: Generic UnprocessableEntityException _
instance showUnprocessableEntityException :: Show UnprocessableEntityException where
  show = genericShow
instance decodeUnprocessableEntityException :: Decode UnprocessableEntityException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnprocessableEntityException :: Encode UnprocessableEntityException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Configuration parameters used to update the Channel.
newtype UpdateChannelRequest = UpdateChannelRequest 
  { "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: (String)
  }
derive instance newtypeUpdateChannelRequest :: Newtype UpdateChannelRequest _
derive instance repGenericUpdateChannelRequest :: Generic UpdateChannelRequest _
instance showUpdateChannelRequest :: Show UpdateChannelRequest where
  show = genericShow
instance decodeUpdateChannelRequest :: Decode UpdateChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateChannelRequest :: Encode UpdateChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateChannelResponse = UpdateChannelResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsIngest" :: NullOrUndefined.NullOrUndefined (HlsIngest)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateChannelResponse :: Newtype UpdateChannelResponse _
derive instance repGenericUpdateChannelResponse :: Generic UpdateChannelResponse _
instance showUpdateChannelResponse :: Show UpdateChannelResponse where
  show = genericShow
instance decodeUpdateChannelResponse :: Decode UpdateChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateChannelResponse :: Encode UpdateChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Configuration parameters used to update an existing OriginEndpoint.
newtype UpdateOriginEndpointRequest = UpdateOriginEndpointRequest 
  { "DashPackage" :: NullOrUndefined.NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined.NullOrUndefined (HlsPackage)
  , "Id" :: (String)
  , "ManifestName" :: NullOrUndefined.NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined.NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "Whitelist" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeUpdateOriginEndpointRequest :: Newtype UpdateOriginEndpointRequest _
derive instance repGenericUpdateOriginEndpointRequest :: Generic UpdateOriginEndpointRequest _
instance showUpdateOriginEndpointRequest :: Show UpdateOriginEndpointRequest where
  show = genericShow
instance decodeUpdateOriginEndpointRequest :: Decode UpdateOriginEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateOriginEndpointRequest :: Encode UpdateOriginEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateOriginEndpointResponse = UpdateOriginEndpointResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "ChannelId" :: NullOrUndefined.NullOrUndefined (String)
  , "DashPackage" :: NullOrUndefined.NullOrUndefined (DashPackage)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "HlsPackage" :: NullOrUndefined.NullOrUndefined (HlsPackage)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "ManifestName" :: NullOrUndefined.NullOrUndefined (String)
  , "MssPackage" :: NullOrUndefined.NullOrUndefined (MssPackage)
  , "StartoverWindowSeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimeDelaySeconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  , "Whitelist" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeUpdateOriginEndpointResponse :: Newtype UpdateOriginEndpointResponse _
derive instance repGenericUpdateOriginEndpointResponse :: Generic UpdateOriginEndpointResponse _
instance showUpdateOriginEndpointResponse :: Show UpdateOriginEndpointResponse where
  show = genericShow
instance decodeUpdateOriginEndpointResponse :: Decode UpdateOriginEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateOriginEndpointResponse :: Encode UpdateOriginEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
