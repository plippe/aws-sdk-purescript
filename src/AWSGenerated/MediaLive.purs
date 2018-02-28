

-- | API for AWS Elemental MediaLive
module AWS.MediaLive where

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

serviceName = "MediaLive" :: String


-- | Creates a new channel
createChannel :: forall eff. CreateChannelRequest -> Aff (exception :: EXCEPTION | eff) CreateChannelResponse
createChannel = Request.request serviceName "createChannel" 


-- | Create an input
createInput :: forall eff. CreateInputRequest -> Aff (exception :: EXCEPTION | eff) CreateInputResponse
createInput = Request.request serviceName "createInput" 


-- | Creates a Input Security Group
createInputSecurityGroup :: forall eff. CreateInputSecurityGroupRequest -> Aff (exception :: EXCEPTION | eff) CreateInputSecurityGroupResponse
createInputSecurityGroup = Request.request serviceName "createInputSecurityGroup" 


-- | Starts deletion of channel. The associated outputs are also deleted.
deleteChannel :: forall eff. DeleteChannelRequest -> Aff (exception :: EXCEPTION | eff) DeleteChannelResponse
deleteChannel = Request.request serviceName "deleteChannel" 


-- | Deletes the input end point
deleteInput :: forall eff. DeleteInputRequest -> Aff (exception :: EXCEPTION | eff) DeleteInputResponse
deleteInput = Request.request serviceName "deleteInput" 


-- | Deletes an Input Security Group
deleteInputSecurityGroup :: forall eff. DeleteInputSecurityGroupRequest -> Aff (exception :: EXCEPTION | eff) DeleteInputSecurityGroupResponse
deleteInputSecurityGroup = Request.request serviceName "deleteInputSecurityGroup" 


-- | Gets details about a channel
describeChannel :: forall eff. DescribeChannelRequest -> Aff (exception :: EXCEPTION | eff) DescribeChannelResponse
describeChannel = Request.request serviceName "describeChannel" 


-- | Produces details about an input
describeInput :: forall eff. DescribeInputRequest -> Aff (exception :: EXCEPTION | eff) DescribeInputResponse
describeInput = Request.request serviceName "describeInput" 


-- | Produces a summary of an Input Security Group
describeInputSecurityGroup :: forall eff. DescribeInputSecurityGroupRequest -> Aff (exception :: EXCEPTION | eff) DescribeInputSecurityGroupResponse
describeInputSecurityGroup = Request.request serviceName "describeInputSecurityGroup" 


-- | Produces list of channels that have been created
listChannels :: forall eff. ListChannelsRequest -> Aff (exception :: EXCEPTION | eff) ListChannelsResponse
listChannels = Request.request serviceName "listChannels" 


-- | Produces a list of Input Security Groups for an account
listInputSecurityGroups :: forall eff. ListInputSecurityGroupsRequest -> Aff (exception :: EXCEPTION | eff) ListInputSecurityGroupsResponse
listInputSecurityGroups = Request.request serviceName "listInputSecurityGroups" 


-- | Produces list of inputs that have been created
listInputs :: forall eff. ListInputsRequest -> Aff (exception :: EXCEPTION | eff) ListInputsResponse
listInputs = Request.request serviceName "listInputs" 


-- | Starts an existing channel
startChannel :: forall eff. StartChannelRequest -> Aff (exception :: EXCEPTION | eff) StartChannelResponse
startChannel = Request.request serviceName "startChannel" 


-- | Stops a running channel
stopChannel :: forall eff. StopChannelRequest -> Aff (exception :: EXCEPTION | eff) StopChannelResponse
stopChannel = Request.request serviceName "stopChannel" 


-- | Updates a channel.
updateChannel :: forall eff. UpdateChannelRequest -> Aff (exception :: EXCEPTION | eff) UpdateChannelResponse
updateChannel = Request.request serviceName "updateChannel" 


-- | Placeholder documentation for AacCodingMode
newtype AacCodingMode = AacCodingMode String
derive instance newtypeAacCodingMode :: Newtype AacCodingMode _
derive instance repGenericAacCodingMode :: Generic AacCodingMode _
instance showAacCodingMode :: Show AacCodingMode where
  show = genericShow
instance decodeAacCodingMode :: Decode AacCodingMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacCodingMode :: Encode AacCodingMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AacInputType
newtype AacInputType = AacInputType String
derive instance newtypeAacInputType :: Newtype AacInputType _
derive instance repGenericAacInputType :: Generic AacInputType _
instance showAacInputType :: Show AacInputType where
  show = genericShow
instance decodeAacInputType :: Decode AacInputType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacInputType :: Encode AacInputType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AacProfile
newtype AacProfile = AacProfile String
derive instance newtypeAacProfile :: Newtype AacProfile _
derive instance repGenericAacProfile :: Generic AacProfile _
instance showAacProfile :: Show AacProfile where
  show = genericShow
instance decodeAacProfile :: Decode AacProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacProfile :: Encode AacProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AacRateControlMode
newtype AacRateControlMode = AacRateControlMode String
derive instance newtypeAacRateControlMode :: Newtype AacRateControlMode _
derive instance repGenericAacRateControlMode :: Generic AacRateControlMode _
instance showAacRateControlMode :: Show AacRateControlMode where
  show = genericShow
instance decodeAacRateControlMode :: Decode AacRateControlMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacRateControlMode :: Encode AacRateControlMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AacRawFormat
newtype AacRawFormat = AacRawFormat String
derive instance newtypeAacRawFormat :: Newtype AacRawFormat _
derive instance repGenericAacRawFormat :: Generic AacRawFormat _
instance showAacRawFormat :: Show AacRawFormat where
  show = genericShow
instance decodeAacRawFormat :: Decode AacRawFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacRawFormat :: Encode AacRawFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AacSettings
newtype AacSettings = AacSettings 
  { "Bitrate" :: NullOrUndefined.NullOrUndefined (Number)
  , "CodingMode" :: NullOrUndefined.NullOrUndefined (AacCodingMode)
  , "InputType" :: NullOrUndefined.NullOrUndefined (AacInputType)
  , "Profile" :: NullOrUndefined.NullOrUndefined (AacProfile)
  , "RateControlMode" :: NullOrUndefined.NullOrUndefined (AacRateControlMode)
  , "RawFormat" :: NullOrUndefined.NullOrUndefined (AacRawFormat)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (Number)
  , "Spec" :: NullOrUndefined.NullOrUndefined (AacSpec)
  , "VbrQuality" :: NullOrUndefined.NullOrUndefined (AacVbrQuality)
  }
derive instance newtypeAacSettings :: Newtype AacSettings _
derive instance repGenericAacSettings :: Generic AacSettings _
instance showAacSettings :: Show AacSettings where
  show = genericShow
instance decodeAacSettings :: Decode AacSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacSettings :: Encode AacSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AacSpec
newtype AacSpec = AacSpec String
derive instance newtypeAacSpec :: Newtype AacSpec _
derive instance repGenericAacSpec :: Generic AacSpec _
instance showAacSpec :: Show AacSpec where
  show = genericShow
instance decodeAacSpec :: Decode AacSpec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacSpec :: Encode AacSpec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AacVbrQuality
newtype AacVbrQuality = AacVbrQuality String
derive instance newtypeAacVbrQuality :: Newtype AacVbrQuality _
derive instance repGenericAacVbrQuality :: Generic AacVbrQuality _
instance showAacVbrQuality :: Show AacVbrQuality where
  show = genericShow
instance decodeAacVbrQuality :: Decode AacVbrQuality where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacVbrQuality :: Encode AacVbrQuality where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Ac3BitstreamMode
newtype Ac3BitstreamMode = Ac3BitstreamMode String
derive instance newtypeAc3BitstreamMode :: Newtype Ac3BitstreamMode _
derive instance repGenericAc3BitstreamMode :: Generic Ac3BitstreamMode _
instance showAc3BitstreamMode :: Show Ac3BitstreamMode where
  show = genericShow
instance decodeAc3BitstreamMode :: Decode Ac3BitstreamMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3BitstreamMode :: Encode Ac3BitstreamMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Ac3CodingMode
newtype Ac3CodingMode = Ac3CodingMode String
derive instance newtypeAc3CodingMode :: Newtype Ac3CodingMode _
derive instance repGenericAc3CodingMode :: Generic Ac3CodingMode _
instance showAc3CodingMode :: Show Ac3CodingMode where
  show = genericShow
instance decodeAc3CodingMode :: Decode Ac3CodingMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3CodingMode :: Encode Ac3CodingMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Ac3DrcProfile
newtype Ac3DrcProfile = Ac3DrcProfile String
derive instance newtypeAc3DrcProfile :: Newtype Ac3DrcProfile _
derive instance repGenericAc3DrcProfile :: Generic Ac3DrcProfile _
instance showAc3DrcProfile :: Show Ac3DrcProfile where
  show = genericShow
instance decodeAc3DrcProfile :: Decode Ac3DrcProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3DrcProfile :: Encode Ac3DrcProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Ac3LfeFilter
newtype Ac3LfeFilter = Ac3LfeFilter String
derive instance newtypeAc3LfeFilter :: Newtype Ac3LfeFilter _
derive instance repGenericAc3LfeFilter :: Generic Ac3LfeFilter _
instance showAc3LfeFilter :: Show Ac3LfeFilter where
  show = genericShow
instance decodeAc3LfeFilter :: Decode Ac3LfeFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3LfeFilter :: Encode Ac3LfeFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Ac3MetadataControl
newtype Ac3MetadataControl = Ac3MetadataControl String
derive instance newtypeAc3MetadataControl :: Newtype Ac3MetadataControl _
derive instance repGenericAc3MetadataControl :: Generic Ac3MetadataControl _
instance showAc3MetadataControl :: Show Ac3MetadataControl where
  show = genericShow
instance decodeAc3MetadataControl :: Decode Ac3MetadataControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3MetadataControl :: Encode Ac3MetadataControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Ac3Settings
newtype Ac3Settings = Ac3Settings 
  { "Bitrate" :: NullOrUndefined.NullOrUndefined (Number)
  , "BitstreamMode" :: NullOrUndefined.NullOrUndefined (Ac3BitstreamMode)
  , "CodingMode" :: NullOrUndefined.NullOrUndefined (Ac3CodingMode)
  , "Dialnorm" :: NullOrUndefined.NullOrUndefined (Int)
  , "DrcProfile" :: NullOrUndefined.NullOrUndefined (Ac3DrcProfile)
  , "LfeFilter" :: NullOrUndefined.NullOrUndefined (Ac3LfeFilter)
  , "MetadataControl" :: NullOrUndefined.NullOrUndefined (Ac3MetadataControl)
  }
derive instance newtypeAc3Settings :: Newtype Ac3Settings _
derive instance repGenericAc3Settings :: Generic Ac3Settings _
instance showAc3Settings :: Show Ac3Settings where
  show = genericShow
instance decodeAc3Settings :: Decode Ac3Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3Settings :: Encode Ac3Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AccessDenied
newtype AccessDenied = AccessDenied 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAccessDenied :: Newtype AccessDenied _
derive instance repGenericAccessDenied :: Generic AccessDenied _
instance showAccessDenied :: Show AccessDenied where
  show = genericShow
instance decodeAccessDenied :: Decode AccessDenied where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDenied :: Encode AccessDenied where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AfdSignaling
newtype AfdSignaling = AfdSignaling String
derive instance newtypeAfdSignaling :: Newtype AfdSignaling _
derive instance repGenericAfdSignaling :: Generic AfdSignaling _
instance showAfdSignaling :: Show AfdSignaling where
  show = genericShow
instance decodeAfdSignaling :: Decode AfdSignaling where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAfdSignaling :: Encode AfdSignaling where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ArchiveContainerSettings
newtype ArchiveContainerSettings = ArchiveContainerSettings 
  { "M2tsSettings" :: NullOrUndefined.NullOrUndefined (M2tsSettings)
  }
derive instance newtypeArchiveContainerSettings :: Newtype ArchiveContainerSettings _
derive instance repGenericArchiveContainerSettings :: Generic ArchiveContainerSettings _
instance showArchiveContainerSettings :: Show ArchiveContainerSettings where
  show = genericShow
instance decodeArchiveContainerSettings :: Decode ArchiveContainerSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArchiveContainerSettings :: Encode ArchiveContainerSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ArchiveGroupSettings
newtype ArchiveGroupSettings = ArchiveGroupSettings 
  { "Destination" :: NullOrUndefined.NullOrUndefined (OutputLocationRef)
  , "RolloverInterval" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeArchiveGroupSettings :: Newtype ArchiveGroupSettings _
derive instance repGenericArchiveGroupSettings :: Generic ArchiveGroupSettings _
instance showArchiveGroupSettings :: Show ArchiveGroupSettings where
  show = genericShow
instance decodeArchiveGroupSettings :: Decode ArchiveGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArchiveGroupSettings :: Encode ArchiveGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ArchiveOutputSettings
newtype ArchiveOutputSettings = ArchiveOutputSettings 
  { "ContainerSettings" :: NullOrUndefined.NullOrUndefined (ArchiveContainerSettings)
  , "Extension" :: NullOrUndefined.NullOrUndefined (String)
  , "NameModifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeArchiveOutputSettings :: Newtype ArchiveOutputSettings _
derive instance repGenericArchiveOutputSettings :: Generic ArchiveOutputSettings _
instance showArchiveOutputSettings :: Show ArchiveOutputSettings where
  show = genericShow
instance decodeArchiveOutputSettings :: Decode ArchiveOutputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArchiveOutputSettings :: Encode ArchiveOutputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AribDestinationSettings
newtype AribDestinationSettings = AribDestinationSettings Types.NoArguments
derive instance newtypeAribDestinationSettings :: Newtype AribDestinationSettings _
derive instance repGenericAribDestinationSettings :: Generic AribDestinationSettings _
instance showAribDestinationSettings :: Show AribDestinationSettings where
  show = genericShow
instance decodeAribDestinationSettings :: Decode AribDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAribDestinationSettings :: Encode AribDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AribSourceSettings
newtype AribSourceSettings = AribSourceSettings Types.NoArguments
derive instance newtypeAribSourceSettings :: Newtype AribSourceSettings _
derive instance repGenericAribSourceSettings :: Generic AribSourceSettings _
instance showAribSourceSettings :: Show AribSourceSettings where
  show = genericShow
instance decodeAribSourceSettings :: Decode AribSourceSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAribSourceSettings :: Encode AribSourceSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioChannelMapping
newtype AudioChannelMapping = AudioChannelMapping 
  { "InputChannelLevels" :: NullOrUndefined.NullOrUndefined (ListOfInputChannelLevel)
  , "OutputChannel" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAudioChannelMapping :: Newtype AudioChannelMapping _
derive instance repGenericAudioChannelMapping :: Generic AudioChannelMapping _
instance showAudioChannelMapping :: Show AudioChannelMapping where
  show = genericShow
instance decodeAudioChannelMapping :: Decode AudioChannelMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioChannelMapping :: Encode AudioChannelMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioCodecSettings
newtype AudioCodecSettings = AudioCodecSettings 
  { "AacSettings" :: NullOrUndefined.NullOrUndefined (AacSettings)
  , "Ac3Settings" :: NullOrUndefined.NullOrUndefined (Ac3Settings)
  , "Eac3Settings" :: NullOrUndefined.NullOrUndefined (Eac3Settings)
  , "Mp2Settings" :: NullOrUndefined.NullOrUndefined (Mp2Settings)
  , "PassThroughSettings" :: NullOrUndefined.NullOrUndefined (PassThroughSettings)
  }
derive instance newtypeAudioCodecSettings :: Newtype AudioCodecSettings _
derive instance repGenericAudioCodecSettings :: Generic AudioCodecSettings _
instance showAudioCodecSettings :: Show AudioCodecSettings where
  show = genericShow
instance decodeAudioCodecSettings :: Decode AudioCodecSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioCodecSettings :: Encode AudioCodecSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioDescription
newtype AudioDescription = AudioDescription 
  { "AudioNormalizationSettings" :: NullOrUndefined.NullOrUndefined (AudioNormalizationSettings)
  , "AudioSelectorName" :: NullOrUndefined.NullOrUndefined (String)
  , "AudioType" :: NullOrUndefined.NullOrUndefined (AudioType)
  , "AudioTypeControl" :: NullOrUndefined.NullOrUndefined (AudioDescriptionAudioTypeControl)
  , "CodecSettings" :: NullOrUndefined.NullOrUndefined (AudioCodecSettings)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (String)
  , "LanguageCodeControl" :: NullOrUndefined.NullOrUndefined (AudioDescriptionLanguageCodeControl)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "RemixSettings" :: NullOrUndefined.NullOrUndefined (RemixSettings)
  , "StreamName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAudioDescription :: Newtype AudioDescription _
derive instance repGenericAudioDescription :: Generic AudioDescription _
instance showAudioDescription :: Show AudioDescription where
  show = genericShow
instance decodeAudioDescription :: Decode AudioDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioDescription :: Encode AudioDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioDescriptionAudioTypeControl
newtype AudioDescriptionAudioTypeControl = AudioDescriptionAudioTypeControl String
derive instance newtypeAudioDescriptionAudioTypeControl :: Newtype AudioDescriptionAudioTypeControl _
derive instance repGenericAudioDescriptionAudioTypeControl :: Generic AudioDescriptionAudioTypeControl _
instance showAudioDescriptionAudioTypeControl :: Show AudioDescriptionAudioTypeControl where
  show = genericShow
instance decodeAudioDescriptionAudioTypeControl :: Decode AudioDescriptionAudioTypeControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioDescriptionAudioTypeControl :: Encode AudioDescriptionAudioTypeControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioDescriptionLanguageCodeControl
newtype AudioDescriptionLanguageCodeControl = AudioDescriptionLanguageCodeControl String
derive instance newtypeAudioDescriptionLanguageCodeControl :: Newtype AudioDescriptionLanguageCodeControl _
derive instance repGenericAudioDescriptionLanguageCodeControl :: Generic AudioDescriptionLanguageCodeControl _
instance showAudioDescriptionLanguageCodeControl :: Show AudioDescriptionLanguageCodeControl where
  show = genericShow
instance decodeAudioDescriptionLanguageCodeControl :: Decode AudioDescriptionLanguageCodeControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioDescriptionLanguageCodeControl :: Encode AudioDescriptionLanguageCodeControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioLanguageSelection
newtype AudioLanguageSelection = AudioLanguageSelection 
  { "LanguageCode" :: NullOrUndefined.NullOrUndefined (String)
  , "LanguageSelectionPolicy" :: NullOrUndefined.NullOrUndefined (AudioLanguageSelectionPolicy)
  }
derive instance newtypeAudioLanguageSelection :: Newtype AudioLanguageSelection _
derive instance repGenericAudioLanguageSelection :: Generic AudioLanguageSelection _
instance showAudioLanguageSelection :: Show AudioLanguageSelection where
  show = genericShow
instance decodeAudioLanguageSelection :: Decode AudioLanguageSelection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioLanguageSelection :: Encode AudioLanguageSelection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioLanguageSelectionPolicy
newtype AudioLanguageSelectionPolicy = AudioLanguageSelectionPolicy String
derive instance newtypeAudioLanguageSelectionPolicy :: Newtype AudioLanguageSelectionPolicy _
derive instance repGenericAudioLanguageSelectionPolicy :: Generic AudioLanguageSelectionPolicy _
instance showAudioLanguageSelectionPolicy :: Show AudioLanguageSelectionPolicy where
  show = genericShow
instance decodeAudioLanguageSelectionPolicy :: Decode AudioLanguageSelectionPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioLanguageSelectionPolicy :: Encode AudioLanguageSelectionPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioNormalizationAlgorithm
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm String
derive instance newtypeAudioNormalizationAlgorithm :: Newtype AudioNormalizationAlgorithm _
derive instance repGenericAudioNormalizationAlgorithm :: Generic AudioNormalizationAlgorithm _
instance showAudioNormalizationAlgorithm :: Show AudioNormalizationAlgorithm where
  show = genericShow
instance decodeAudioNormalizationAlgorithm :: Decode AudioNormalizationAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioNormalizationAlgorithm :: Encode AudioNormalizationAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioNormalizationAlgorithmControl
newtype AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl String
derive instance newtypeAudioNormalizationAlgorithmControl :: Newtype AudioNormalizationAlgorithmControl _
derive instance repGenericAudioNormalizationAlgorithmControl :: Generic AudioNormalizationAlgorithmControl _
instance showAudioNormalizationAlgorithmControl :: Show AudioNormalizationAlgorithmControl where
  show = genericShow
instance decodeAudioNormalizationAlgorithmControl :: Decode AudioNormalizationAlgorithmControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioNormalizationAlgorithmControl :: Encode AudioNormalizationAlgorithmControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioNormalizationSettings
newtype AudioNormalizationSettings = AudioNormalizationSettings 
  { "Algorithm" :: NullOrUndefined.NullOrUndefined (AudioNormalizationAlgorithm)
  , "AlgorithmControl" :: NullOrUndefined.NullOrUndefined (AudioNormalizationAlgorithmControl)
  , "TargetLkfs" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeAudioNormalizationSettings :: Newtype AudioNormalizationSettings _
derive instance repGenericAudioNormalizationSettings :: Generic AudioNormalizationSettings _
instance showAudioNormalizationSettings :: Show AudioNormalizationSettings where
  show = genericShow
instance decodeAudioNormalizationSettings :: Decode AudioNormalizationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioNormalizationSettings :: Encode AudioNormalizationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioOnlyHlsSettings
newtype AudioOnlyHlsSettings = AudioOnlyHlsSettings 
  { "AudioGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "AudioOnlyImage" :: NullOrUndefined.NullOrUndefined (InputLocation)
  , "AudioTrackType" :: NullOrUndefined.NullOrUndefined (AudioOnlyHlsTrackType)
  }
derive instance newtypeAudioOnlyHlsSettings :: Newtype AudioOnlyHlsSettings _
derive instance repGenericAudioOnlyHlsSettings :: Generic AudioOnlyHlsSettings _
instance showAudioOnlyHlsSettings :: Show AudioOnlyHlsSettings where
  show = genericShow
instance decodeAudioOnlyHlsSettings :: Decode AudioOnlyHlsSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioOnlyHlsSettings :: Encode AudioOnlyHlsSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioOnlyHlsTrackType
newtype AudioOnlyHlsTrackType = AudioOnlyHlsTrackType String
derive instance newtypeAudioOnlyHlsTrackType :: Newtype AudioOnlyHlsTrackType _
derive instance repGenericAudioOnlyHlsTrackType :: Generic AudioOnlyHlsTrackType _
instance showAudioOnlyHlsTrackType :: Show AudioOnlyHlsTrackType where
  show = genericShow
instance decodeAudioOnlyHlsTrackType :: Decode AudioOnlyHlsTrackType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioOnlyHlsTrackType :: Encode AudioOnlyHlsTrackType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioPidSelection
newtype AudioPidSelection = AudioPidSelection 
  { "Pid" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAudioPidSelection :: Newtype AudioPidSelection _
derive instance repGenericAudioPidSelection :: Generic AudioPidSelection _
instance showAudioPidSelection :: Show AudioPidSelection where
  show = genericShow
instance decodeAudioPidSelection :: Decode AudioPidSelection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioPidSelection :: Encode AudioPidSelection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioSelector
newtype AudioSelector = AudioSelector 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "SelectorSettings" :: NullOrUndefined.NullOrUndefined (AudioSelectorSettings)
  }
derive instance newtypeAudioSelector :: Newtype AudioSelector _
derive instance repGenericAudioSelector :: Generic AudioSelector _
instance showAudioSelector :: Show AudioSelector where
  show = genericShow
instance decodeAudioSelector :: Decode AudioSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioSelector :: Encode AudioSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioSelectorSettings
newtype AudioSelectorSettings = AudioSelectorSettings 
  { "AudioLanguageSelection" :: NullOrUndefined.NullOrUndefined (AudioLanguageSelection)
  , "AudioPidSelection" :: NullOrUndefined.NullOrUndefined (AudioPidSelection)
  }
derive instance newtypeAudioSelectorSettings :: Newtype AudioSelectorSettings _
derive instance repGenericAudioSelectorSettings :: Generic AudioSelectorSettings _
instance showAudioSelectorSettings :: Show AudioSelectorSettings where
  show = genericShow
instance decodeAudioSelectorSettings :: Decode AudioSelectorSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioSelectorSettings :: Encode AudioSelectorSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AudioType
newtype AudioType = AudioType String
derive instance newtypeAudioType :: Newtype AudioType _
derive instance repGenericAudioType :: Generic AudioType _
instance showAudioType :: Show AudioType where
  show = genericShow
instance decodeAudioType :: Decode AudioType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioType :: Encode AudioType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AvailBlanking
newtype AvailBlanking = AvailBlanking 
  { "AvailBlankingImage" :: NullOrUndefined.NullOrUndefined (InputLocation)
  , "State" :: NullOrUndefined.NullOrUndefined (AvailBlankingState)
  }
derive instance newtypeAvailBlanking :: Newtype AvailBlanking _
derive instance repGenericAvailBlanking :: Generic AvailBlanking _
instance showAvailBlanking :: Show AvailBlanking where
  show = genericShow
instance decodeAvailBlanking :: Decode AvailBlanking where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailBlanking :: Encode AvailBlanking where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AvailBlankingState
newtype AvailBlankingState = AvailBlankingState String
derive instance newtypeAvailBlankingState :: Newtype AvailBlankingState _
derive instance repGenericAvailBlankingState :: Generic AvailBlankingState _
instance showAvailBlankingState :: Show AvailBlankingState where
  show = genericShow
instance decodeAvailBlankingState :: Decode AvailBlankingState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailBlankingState :: Encode AvailBlankingState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AvailConfiguration
newtype AvailConfiguration = AvailConfiguration 
  { "AvailSettings" :: NullOrUndefined.NullOrUndefined (AvailSettings)
  }
derive instance newtypeAvailConfiguration :: Newtype AvailConfiguration _
derive instance repGenericAvailConfiguration :: Generic AvailConfiguration _
instance showAvailConfiguration :: Show AvailConfiguration where
  show = genericShow
instance decodeAvailConfiguration :: Decode AvailConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailConfiguration :: Encode AvailConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for AvailSettings
newtype AvailSettings = AvailSettings 
  { "Scte35SpliceInsert" :: NullOrUndefined.NullOrUndefined (Scte35SpliceInsert)
  , "Scte35TimeSignalApos" :: NullOrUndefined.NullOrUndefined (Scte35TimeSignalApos)
  }
derive instance newtypeAvailSettings :: Newtype AvailSettings _
derive instance repGenericAvailSettings :: Generic AvailSettings _
instance showAvailSettings :: Show AvailSettings where
  show = genericShow
instance decodeAvailSettings :: Decode AvailSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailSettings :: Encode AvailSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BadGatewayException
newtype BadGatewayException = BadGatewayException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadGatewayException :: Newtype BadGatewayException _
derive instance repGenericBadGatewayException :: Generic BadGatewayException _
instance showBadGatewayException :: Show BadGatewayException where
  show = genericShow
instance decodeBadGatewayException :: Decode BadGatewayException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadGatewayException :: Encode BadGatewayException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BadRequestException
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BlackoutSlate
newtype BlackoutSlate = BlackoutSlate 
  { "BlackoutSlateImage" :: NullOrUndefined.NullOrUndefined (InputLocation)
  , "NetworkEndBlackout" :: NullOrUndefined.NullOrUndefined (BlackoutSlateNetworkEndBlackout)
  , "NetworkEndBlackoutImage" :: NullOrUndefined.NullOrUndefined (InputLocation)
  , "NetworkId" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (BlackoutSlateState)
  }
derive instance newtypeBlackoutSlate :: Newtype BlackoutSlate _
derive instance repGenericBlackoutSlate :: Generic BlackoutSlate _
instance showBlackoutSlate :: Show BlackoutSlate where
  show = genericShow
instance decodeBlackoutSlate :: Decode BlackoutSlate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlackoutSlate :: Encode BlackoutSlate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BlackoutSlateNetworkEndBlackout
newtype BlackoutSlateNetworkEndBlackout = BlackoutSlateNetworkEndBlackout String
derive instance newtypeBlackoutSlateNetworkEndBlackout :: Newtype BlackoutSlateNetworkEndBlackout _
derive instance repGenericBlackoutSlateNetworkEndBlackout :: Generic BlackoutSlateNetworkEndBlackout _
instance showBlackoutSlateNetworkEndBlackout :: Show BlackoutSlateNetworkEndBlackout where
  show = genericShow
instance decodeBlackoutSlateNetworkEndBlackout :: Decode BlackoutSlateNetworkEndBlackout where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlackoutSlateNetworkEndBlackout :: Encode BlackoutSlateNetworkEndBlackout where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BlackoutSlateState
newtype BlackoutSlateState = BlackoutSlateState String
derive instance newtypeBlackoutSlateState :: Newtype BlackoutSlateState _
derive instance repGenericBlackoutSlateState :: Generic BlackoutSlateState _
instance showBlackoutSlateState :: Show BlackoutSlateState where
  show = genericShow
instance decodeBlackoutSlateState :: Decode BlackoutSlateState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlackoutSlateState :: Encode BlackoutSlateState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BurnInAlignment
newtype BurnInAlignment = BurnInAlignment String
derive instance newtypeBurnInAlignment :: Newtype BurnInAlignment _
derive instance repGenericBurnInAlignment :: Generic BurnInAlignment _
instance showBurnInAlignment :: Show BurnInAlignment where
  show = genericShow
instance decodeBurnInAlignment :: Decode BurnInAlignment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurnInAlignment :: Encode BurnInAlignment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BurnInBackgroundColor
newtype BurnInBackgroundColor = BurnInBackgroundColor String
derive instance newtypeBurnInBackgroundColor :: Newtype BurnInBackgroundColor _
derive instance repGenericBurnInBackgroundColor :: Generic BurnInBackgroundColor _
instance showBurnInBackgroundColor :: Show BurnInBackgroundColor where
  show = genericShow
instance decodeBurnInBackgroundColor :: Decode BurnInBackgroundColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurnInBackgroundColor :: Encode BurnInBackgroundColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BurnInDestinationSettings
newtype BurnInDestinationSettings = BurnInDestinationSettings 
  { "Alignment" :: NullOrUndefined.NullOrUndefined (BurnInAlignment)
  , "BackgroundColor" :: NullOrUndefined.NullOrUndefined (BurnInBackgroundColor)
  , "BackgroundOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "Font" :: NullOrUndefined.NullOrUndefined (InputLocation)
  , "FontColor" :: NullOrUndefined.NullOrUndefined (BurnInFontColor)
  , "FontOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontResolution" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontSize" :: NullOrUndefined.NullOrUndefined (String)
  , "OutlineColor" :: NullOrUndefined.NullOrUndefined (BurnInOutlineColor)
  , "OutlineSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowColor" :: NullOrUndefined.NullOrUndefined (BurnInShadowColor)
  , "ShadowOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowXOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowYOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "TeletextGridControl" :: NullOrUndefined.NullOrUndefined (BurnInTeletextGridControl)
  , "XPosition" :: NullOrUndefined.NullOrUndefined (Int)
  , "YPosition" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeBurnInDestinationSettings :: Newtype BurnInDestinationSettings _
derive instance repGenericBurnInDestinationSettings :: Generic BurnInDestinationSettings _
instance showBurnInDestinationSettings :: Show BurnInDestinationSettings where
  show = genericShow
instance decodeBurnInDestinationSettings :: Decode BurnInDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurnInDestinationSettings :: Encode BurnInDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BurnInFontColor
newtype BurnInFontColor = BurnInFontColor String
derive instance newtypeBurnInFontColor :: Newtype BurnInFontColor _
derive instance repGenericBurnInFontColor :: Generic BurnInFontColor _
instance showBurnInFontColor :: Show BurnInFontColor where
  show = genericShow
instance decodeBurnInFontColor :: Decode BurnInFontColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurnInFontColor :: Encode BurnInFontColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BurnInOutlineColor
newtype BurnInOutlineColor = BurnInOutlineColor String
derive instance newtypeBurnInOutlineColor :: Newtype BurnInOutlineColor _
derive instance repGenericBurnInOutlineColor :: Generic BurnInOutlineColor _
instance showBurnInOutlineColor :: Show BurnInOutlineColor where
  show = genericShow
instance decodeBurnInOutlineColor :: Decode BurnInOutlineColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurnInOutlineColor :: Encode BurnInOutlineColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BurnInShadowColor
newtype BurnInShadowColor = BurnInShadowColor String
derive instance newtypeBurnInShadowColor :: Newtype BurnInShadowColor _
derive instance repGenericBurnInShadowColor :: Generic BurnInShadowColor _
instance showBurnInShadowColor :: Show BurnInShadowColor where
  show = genericShow
instance decodeBurnInShadowColor :: Decode BurnInShadowColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurnInShadowColor :: Encode BurnInShadowColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for BurnInTeletextGridControl
newtype BurnInTeletextGridControl = BurnInTeletextGridControl String
derive instance newtypeBurnInTeletextGridControl :: Newtype BurnInTeletextGridControl _
derive instance repGenericBurnInTeletextGridControl :: Generic BurnInTeletextGridControl _
instance showBurnInTeletextGridControl :: Show BurnInTeletextGridControl where
  show = genericShow
instance decodeBurnInTeletextGridControl :: Decode BurnInTeletextGridControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurnInTeletextGridControl :: Encode BurnInTeletextGridControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
newtype CaptionDescription = CaptionDescription 
  { "CaptionSelectorName" :: NullOrUndefined.NullOrUndefined (String)
  , "DestinationSettings" :: NullOrUndefined.NullOrUndefined (CaptionDestinationSettings)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (String)
  , "LanguageDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCaptionDescription :: Newtype CaptionDescription _
derive instance repGenericCaptionDescription :: Generic CaptionDescription _
instance showCaptionDescription :: Show CaptionDescription where
  show = genericShow
instance decodeCaptionDescription :: Decode CaptionDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionDescription :: Encode CaptionDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CaptionDestinationSettings
newtype CaptionDestinationSettings = CaptionDestinationSettings 
  { "AribDestinationSettings" :: NullOrUndefined.NullOrUndefined (AribDestinationSettings)
  , "BurnInDestinationSettings" :: NullOrUndefined.NullOrUndefined (BurnInDestinationSettings)
  , "DvbSubDestinationSettings" :: NullOrUndefined.NullOrUndefined (DvbSubDestinationSettings)
  , "EmbeddedDestinationSettings" :: NullOrUndefined.NullOrUndefined (EmbeddedDestinationSettings)
  , "EmbeddedPlusScte20DestinationSettings" :: NullOrUndefined.NullOrUndefined (EmbeddedPlusScte20DestinationSettings)
  , "Scte20PlusEmbeddedDestinationSettings" :: NullOrUndefined.NullOrUndefined (Scte20PlusEmbeddedDestinationSettings)
  , "Scte27DestinationSettings" :: NullOrUndefined.NullOrUndefined (Scte27DestinationSettings)
  , "SmpteTtDestinationSettings" :: NullOrUndefined.NullOrUndefined (SmpteTtDestinationSettings)
  , "TeletextDestinationSettings" :: NullOrUndefined.NullOrUndefined (TeletextDestinationSettings)
  , "TtmlDestinationSettings" :: NullOrUndefined.NullOrUndefined (TtmlDestinationSettings)
  , "WebvttDestinationSettings" :: NullOrUndefined.NullOrUndefined (WebvttDestinationSettings)
  }
derive instance newtypeCaptionDestinationSettings :: Newtype CaptionDestinationSettings _
derive instance repGenericCaptionDestinationSettings :: Generic CaptionDestinationSettings _
instance showCaptionDestinationSettings :: Show CaptionDestinationSettings where
  show = genericShow
instance decodeCaptionDestinationSettings :: Decode CaptionDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionDestinationSettings :: Encode CaptionDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Maps a caption channel to an ISO 693-2 language code (http://www.loc.gov/standards/iso639-2), with an optional description.
newtype CaptionLanguageMapping = CaptionLanguageMapping 
  { "CaptionChannel" :: NullOrUndefined.NullOrUndefined (Int)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (String)
  , "LanguageDescription" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCaptionLanguageMapping :: Newtype CaptionLanguageMapping _
derive instance repGenericCaptionLanguageMapping :: Generic CaptionLanguageMapping _
instance showCaptionLanguageMapping :: Show CaptionLanguageMapping where
  show = genericShow
instance decodeCaptionLanguageMapping :: Decode CaptionLanguageMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionLanguageMapping :: Encode CaptionLanguageMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
newtype CaptionSelector = CaptionSelector 
  { "LanguageCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "SelectorSettings" :: NullOrUndefined.NullOrUndefined (CaptionSelectorSettings)
  }
derive instance newtypeCaptionSelector :: Newtype CaptionSelector _
derive instance repGenericCaptionSelector :: Generic CaptionSelector _
instance showCaptionSelector :: Show CaptionSelector where
  show = genericShow
instance decodeCaptionSelector :: Decode CaptionSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionSelector :: Encode CaptionSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CaptionSelectorSettings
newtype CaptionSelectorSettings = CaptionSelectorSettings 
  { "AribSourceSettings" :: NullOrUndefined.NullOrUndefined (AribSourceSettings)
  , "DvbSubSourceSettings" :: NullOrUndefined.NullOrUndefined (DvbSubSourceSettings)
  , "EmbeddedSourceSettings" :: NullOrUndefined.NullOrUndefined (EmbeddedSourceSettings)
  , "Scte20SourceSettings" :: NullOrUndefined.NullOrUndefined (Scte20SourceSettings)
  , "Scte27SourceSettings" :: NullOrUndefined.NullOrUndefined (Scte27SourceSettings)
  , "TeletextSourceSettings" :: NullOrUndefined.NullOrUndefined (TeletextSourceSettings)
  }
derive instance newtypeCaptionSelectorSettings :: Newtype CaptionSelectorSettings _
derive instance repGenericCaptionSelectorSettings :: Generic CaptionSelectorSettings _
instance showCaptionSelectorSettings :: Show CaptionSelectorSettings where
  show = genericShow
instance decodeCaptionSelectorSettings :: Decode CaptionSelectorSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionSelectorSettings :: Encode CaptionSelectorSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Channel
newtype Channel = Channel 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined.NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined.NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined.NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (ChannelState)
  }
derive instance newtypeChannel :: Newtype Channel _
derive instance repGenericChannel :: Generic Channel _
instance showChannel :: Show Channel where
  show = genericShow
instance decodeChannel :: Decode Channel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannel :: Encode Channel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ChannelConfigurationValidationError
newtype ChannelConfigurationValidationError = ChannelConfigurationValidationError 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "ValidationErrors" :: NullOrUndefined.NullOrUndefined (ListOfValidationError)
  }
derive instance newtypeChannelConfigurationValidationError :: Newtype ChannelConfigurationValidationError _
derive instance repGenericChannelConfigurationValidationError :: Generic ChannelConfigurationValidationError _
instance showChannelConfigurationValidationError :: Show ChannelConfigurationValidationError where
  show = genericShow
instance decodeChannelConfigurationValidationError :: Decode ChannelConfigurationValidationError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelConfigurationValidationError :: Encode ChannelConfigurationValidationError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ChannelEgressEndpoint
newtype ChannelEgressEndpoint = ChannelEgressEndpoint 
  { "SourceIp" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeChannelEgressEndpoint :: Newtype ChannelEgressEndpoint _
derive instance repGenericChannelEgressEndpoint :: Generic ChannelEgressEndpoint _
instance showChannelEgressEndpoint :: Show ChannelEgressEndpoint where
  show = genericShow
instance decodeChannelEgressEndpoint :: Decode ChannelEgressEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelEgressEndpoint :: Encode ChannelEgressEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ChannelState
newtype ChannelState = ChannelState String
derive instance newtypeChannelState :: Newtype ChannelState _
derive instance repGenericChannelState :: Generic ChannelState _
instance showChannelState :: Show ChannelState where
  show = genericShow
instance decodeChannelState :: Decode ChannelState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelState :: Encode ChannelState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ChannelSummary
newtype ChannelSummary = ChannelSummary 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined.NullOrUndefined (ListOfChannelEgressEndpoint)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined.NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (ChannelState)
  }
derive instance newtypeChannelSummary :: Newtype ChannelSummary _
derive instance repGenericChannelSummary :: Generic ChannelSummary _
instance showChannelSummary :: Show ChannelSummary where
  show = genericShow
instance decodeChannelSummary :: Decode ChannelSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelSummary :: Encode ChannelSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ConflictException
newtype ConflictException = ConflictException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _
derive instance repGenericConflictException :: Generic ConflictException _
instance showConflictException :: Show ConflictException where
  show = genericShow
instance decodeConflictException :: Decode ConflictException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConflictException :: Encode ConflictException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CreateChannel
newtype CreateChannel = CreateChannel 
  { "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined.NullOrUndefined (EncoderSettings)
  , "InputAttachments" :: NullOrUndefined.NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "Reserved" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateChannel :: Newtype CreateChannel _
derive instance repGenericCreateChannel :: Generic CreateChannel _
instance showCreateChannel :: Show CreateChannel where
  show = genericShow
instance decodeCreateChannel :: Decode CreateChannel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateChannel :: Encode CreateChannel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A request to create a channel
newtype CreateChannelRequest = CreateChannelRequest 
  { "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined.NullOrUndefined (EncoderSettings)
  , "InputAttachments" :: NullOrUndefined.NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "Reserved" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateChannelRequest :: Newtype CreateChannelRequest _
derive instance repGenericCreateChannelRequest :: Generic CreateChannelRequest _
instance showCreateChannelRequest :: Show CreateChannelRequest where
  show = genericShow
instance decodeCreateChannelRequest :: Decode CreateChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateChannelRequest :: Encode CreateChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CreateChannelResponse
newtype CreateChannelResponse = CreateChannelResponse 
  { "Channel" :: NullOrUndefined.NullOrUndefined (Channel)
  }
derive instance newtypeCreateChannelResponse :: Newtype CreateChannelResponse _
derive instance repGenericCreateChannelResponse :: Generic CreateChannelResponse _
instance showCreateChannelResponse :: Show CreateChannelResponse where
  show = genericShow
instance decodeCreateChannelResponse :: Decode CreateChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateChannelResponse :: Encode CreateChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CreateChannelResultModel
newtype CreateChannelResultModel = CreateChannelResultModel 
  { "Channel" :: NullOrUndefined.NullOrUndefined (Channel)
  }
derive instance newtypeCreateChannelResultModel :: Newtype CreateChannelResultModel _
derive instance repGenericCreateChannelResultModel :: Generic CreateChannelResultModel _
instance showCreateChannelResultModel :: Show CreateChannelResultModel where
  show = genericShow
instance decodeCreateChannelResultModel :: Decode CreateChannelResultModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateChannelResultModel :: Encode CreateChannelResultModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CreateInput
newtype CreateInput = CreateInput 
  { "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfInputDestinationRequest)
  , "InputSecurityGroups" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "Sources" :: NullOrUndefined.NullOrUndefined (ListOfInputSourceRequest)
  , "Type" :: NullOrUndefined.NullOrUndefined (InputType)
  }
derive instance newtypeCreateInput :: Newtype CreateInput _
derive instance repGenericCreateInput :: Generic CreateInput _
instance showCreateInput :: Show CreateInput where
  show = genericShow
instance decodeCreateInput :: Decode CreateInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInput :: Encode CreateInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The name of the input
newtype CreateInputRequest = CreateInputRequest 
  { "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfInputDestinationRequest)
  , "InputSecurityGroups" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (String)
  , "Sources" :: NullOrUndefined.NullOrUndefined (ListOfInputSourceRequest)
  , "Type" :: NullOrUndefined.NullOrUndefined (InputType)
  }
derive instance newtypeCreateInputRequest :: Newtype CreateInputRequest _
derive instance repGenericCreateInputRequest :: Generic CreateInputRequest _
instance showCreateInputRequest :: Show CreateInputRequest where
  show = genericShow
instance decodeCreateInputRequest :: Decode CreateInputRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInputRequest :: Encode CreateInputRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CreateInputResponse
newtype CreateInputResponse = CreateInputResponse 
  { "Input" :: NullOrUndefined.NullOrUndefined (Input)
  }
derive instance newtypeCreateInputResponse :: Newtype CreateInputResponse _
derive instance repGenericCreateInputResponse :: Generic CreateInputResponse _
instance showCreateInputResponse :: Show CreateInputResponse where
  show = genericShow
instance decodeCreateInputResponse :: Decode CreateInputResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInputResponse :: Encode CreateInputResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CreateInputResultModel
newtype CreateInputResultModel = CreateInputResultModel 
  { "Input" :: NullOrUndefined.NullOrUndefined (Input)
  }
derive instance newtypeCreateInputResultModel :: Newtype CreateInputResultModel _
derive instance repGenericCreateInputResultModel :: Generic CreateInputResultModel _
instance showCreateInputResultModel :: Show CreateInputResultModel where
  show = genericShow
instance decodeCreateInputResultModel :: Decode CreateInputResultModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInputResultModel :: Encode CreateInputResultModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The IPv4 CIDRs to whitelist for this Input Security Group
newtype CreateInputSecurityGroupRequest = CreateInputSecurityGroupRequest 
  { "WhitelistRules" :: NullOrUndefined.NullOrUndefined (ListOfInputWhitelistRuleCidr)
  }
derive instance newtypeCreateInputSecurityGroupRequest :: Newtype CreateInputSecurityGroupRequest _
derive instance repGenericCreateInputSecurityGroupRequest :: Generic CreateInputSecurityGroupRequest _
instance showCreateInputSecurityGroupRequest :: Show CreateInputSecurityGroupRequest where
  show = genericShow
instance decodeCreateInputSecurityGroupRequest :: Decode CreateInputSecurityGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInputSecurityGroupRequest :: Encode CreateInputSecurityGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CreateInputSecurityGroupResponse
newtype CreateInputSecurityGroupResponse = CreateInputSecurityGroupResponse 
  { "SecurityGroup" :: NullOrUndefined.NullOrUndefined (InputSecurityGroup)
  }
derive instance newtypeCreateInputSecurityGroupResponse :: Newtype CreateInputSecurityGroupResponse _
derive instance repGenericCreateInputSecurityGroupResponse :: Generic CreateInputSecurityGroupResponse _
instance showCreateInputSecurityGroupResponse :: Show CreateInputSecurityGroupResponse where
  show = genericShow
instance decodeCreateInputSecurityGroupResponse :: Decode CreateInputSecurityGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInputSecurityGroupResponse :: Encode CreateInputSecurityGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for CreateInputSecurityGroupResultModel
newtype CreateInputSecurityGroupResultModel = CreateInputSecurityGroupResultModel 
  { "SecurityGroup" :: NullOrUndefined.NullOrUndefined (InputSecurityGroup)
  }
derive instance newtypeCreateInputSecurityGroupResultModel :: Newtype CreateInputSecurityGroupResultModel _
derive instance repGenericCreateInputSecurityGroupResultModel :: Generic CreateInputSecurityGroupResultModel _
instance showCreateInputSecurityGroupResultModel :: Show CreateInputSecurityGroupResultModel where
  show = genericShow
instance decodeCreateInputSecurityGroupResultModel :: Decode CreateInputSecurityGroupResultModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateInputSecurityGroupResultModel :: Encode CreateInputSecurityGroupResultModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DeleteChannelRequest
newtype DeleteChannelRequest = DeleteChannelRequest 
  { "ChannelId" :: (String)
  }
derive instance newtypeDeleteChannelRequest :: Newtype DeleteChannelRequest _
derive instance repGenericDeleteChannelRequest :: Generic DeleteChannelRequest _
instance showDeleteChannelRequest :: Show DeleteChannelRequest where
  show = genericShow
instance decodeDeleteChannelRequest :: Decode DeleteChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteChannelRequest :: Encode DeleteChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DeleteChannelResponse
newtype DeleteChannelResponse = DeleteChannelResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined.NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined.NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined.NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (ChannelState)
  }
derive instance newtypeDeleteChannelResponse :: Newtype DeleteChannelResponse _
derive instance repGenericDeleteChannelResponse :: Generic DeleteChannelResponse _
instance showDeleteChannelResponse :: Show DeleteChannelResponse where
  show = genericShow
instance decodeDeleteChannelResponse :: Decode DeleteChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteChannelResponse :: Encode DeleteChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DeleteInputRequest
newtype DeleteInputRequest = DeleteInputRequest 
  { "InputId" :: (String)
  }
derive instance newtypeDeleteInputRequest :: Newtype DeleteInputRequest _
derive instance repGenericDeleteInputRequest :: Generic DeleteInputRequest _
instance showDeleteInputRequest :: Show DeleteInputRequest where
  show = genericShow
instance decodeDeleteInputRequest :: Decode DeleteInputRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInputRequest :: Encode DeleteInputRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DeleteInputResponse
newtype DeleteInputResponse = DeleteInputResponse Types.NoArguments
derive instance newtypeDeleteInputResponse :: Newtype DeleteInputResponse _
derive instance repGenericDeleteInputResponse :: Generic DeleteInputResponse _
instance showDeleteInputResponse :: Show DeleteInputResponse where
  show = genericShow
instance decodeDeleteInputResponse :: Decode DeleteInputResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInputResponse :: Encode DeleteInputResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DeleteInputSecurityGroupRequest
newtype DeleteInputSecurityGroupRequest = DeleteInputSecurityGroupRequest 
  { "InputSecurityGroupId" :: (String)
  }
derive instance newtypeDeleteInputSecurityGroupRequest :: Newtype DeleteInputSecurityGroupRequest _
derive instance repGenericDeleteInputSecurityGroupRequest :: Generic DeleteInputSecurityGroupRequest _
instance showDeleteInputSecurityGroupRequest :: Show DeleteInputSecurityGroupRequest where
  show = genericShow
instance decodeDeleteInputSecurityGroupRequest :: Decode DeleteInputSecurityGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInputSecurityGroupRequest :: Encode DeleteInputSecurityGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DeleteInputSecurityGroupResponse
newtype DeleteInputSecurityGroupResponse = DeleteInputSecurityGroupResponse Types.NoArguments
derive instance newtypeDeleteInputSecurityGroupResponse :: Newtype DeleteInputSecurityGroupResponse _
derive instance repGenericDeleteInputSecurityGroupResponse :: Generic DeleteInputSecurityGroupResponse _
instance showDeleteInputSecurityGroupResponse :: Show DeleteInputSecurityGroupResponse where
  show = genericShow
instance decodeDeleteInputSecurityGroupResponse :: Decode DeleteInputSecurityGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInputSecurityGroupResponse :: Encode DeleteInputSecurityGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DescribeChannelRequest
newtype DescribeChannelRequest = DescribeChannelRequest 
  { "ChannelId" :: (String)
  }
derive instance newtypeDescribeChannelRequest :: Newtype DescribeChannelRequest _
derive instance repGenericDescribeChannelRequest :: Generic DescribeChannelRequest _
instance showDescribeChannelRequest :: Show DescribeChannelRequest where
  show = genericShow
instance decodeDescribeChannelRequest :: Decode DescribeChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeChannelRequest :: Encode DescribeChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DescribeChannelResponse
newtype DescribeChannelResponse = DescribeChannelResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined.NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined.NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined.NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (ChannelState)
  }
derive instance newtypeDescribeChannelResponse :: Newtype DescribeChannelResponse _
derive instance repGenericDescribeChannelResponse :: Generic DescribeChannelResponse _
instance showDescribeChannelResponse :: Show DescribeChannelResponse where
  show = genericShow
instance decodeDescribeChannelResponse :: Decode DescribeChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeChannelResponse :: Encode DescribeChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DescribeInputRequest
newtype DescribeInputRequest = DescribeInputRequest 
  { "InputId" :: (String)
  }
derive instance newtypeDescribeInputRequest :: Newtype DescribeInputRequest _
derive instance repGenericDescribeInputRequest :: Generic DescribeInputRequest _
instance showDescribeInputRequest :: Show DescribeInputRequest where
  show = genericShow
instance decodeDescribeInputRequest :: Decode DescribeInputRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInputRequest :: Encode DescribeInputRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DescribeInputResponse
newtype DescribeInputResponse = DescribeInputResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "AttachedChannels" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfInputDestination)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "Sources" :: NullOrUndefined.NullOrUndefined (ListOfInputSource)
  , "State" :: NullOrUndefined.NullOrUndefined (InputState)
  , "Type" :: NullOrUndefined.NullOrUndefined (InputType)
  }
derive instance newtypeDescribeInputResponse :: Newtype DescribeInputResponse _
derive instance repGenericDescribeInputResponse :: Generic DescribeInputResponse _
instance showDescribeInputResponse :: Show DescribeInputResponse where
  show = genericShow
instance decodeDescribeInputResponse :: Decode DescribeInputResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInputResponse :: Encode DescribeInputResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DescribeInputSecurityGroupRequest
newtype DescribeInputSecurityGroupRequest = DescribeInputSecurityGroupRequest 
  { "InputSecurityGroupId" :: (String)
  }
derive instance newtypeDescribeInputSecurityGroupRequest :: Newtype DescribeInputSecurityGroupRequest _
derive instance repGenericDescribeInputSecurityGroupRequest :: Generic DescribeInputSecurityGroupRequest _
instance showDescribeInputSecurityGroupRequest :: Show DescribeInputSecurityGroupRequest where
  show = genericShow
instance decodeDescribeInputSecurityGroupRequest :: Decode DescribeInputSecurityGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInputSecurityGroupRequest :: Encode DescribeInputSecurityGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DescribeInputSecurityGroupResponse
newtype DescribeInputSecurityGroupResponse = DescribeInputSecurityGroupResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "WhitelistRules" :: NullOrUndefined.NullOrUndefined (ListOfInputWhitelistRule)
  }
derive instance newtypeDescribeInputSecurityGroupResponse :: Newtype DescribeInputSecurityGroupResponse _
derive instance repGenericDescribeInputSecurityGroupResponse :: Generic DescribeInputSecurityGroupResponse _
instance showDescribeInputSecurityGroupResponse :: Show DescribeInputSecurityGroupResponse where
  show = genericShow
instance decodeDescribeInputSecurityGroupResponse :: Decode DescribeInputSecurityGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInputSecurityGroupResponse :: Encode DescribeInputSecurityGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | DVB Network Information Table (NIT)
newtype DvbNitSettings = DvbNitSettings 
  { "NetworkId" :: NullOrUndefined.NullOrUndefined (Int)
  , "NetworkName" :: NullOrUndefined.NullOrUndefined (String)
  , "RepInterval" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDvbNitSettings :: Newtype DvbNitSettings _
derive instance repGenericDvbNitSettings :: Generic DvbNitSettings _
instance showDvbNitSettings :: Show DvbNitSettings where
  show = genericShow
instance decodeDvbNitSettings :: Decode DvbNitSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbNitSettings :: Encode DvbNitSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DvbSdtOutputSdt
newtype DvbSdtOutputSdt = DvbSdtOutputSdt String
derive instance newtypeDvbSdtOutputSdt :: Newtype DvbSdtOutputSdt _
derive instance repGenericDvbSdtOutputSdt :: Generic DvbSdtOutputSdt _
instance showDvbSdtOutputSdt :: Show DvbSdtOutputSdt where
  show = genericShow
instance decodeDvbSdtOutputSdt :: Decode DvbSdtOutputSdt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSdtOutputSdt :: Encode DvbSdtOutputSdt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | DVB Service Description Table (SDT)
newtype DvbSdtSettings = DvbSdtSettings 
  { "OutputSdt" :: NullOrUndefined.NullOrUndefined (DvbSdtOutputSdt)
  , "RepInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "ServiceName" :: NullOrUndefined.NullOrUndefined (String)
  , "ServiceProviderName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDvbSdtSettings :: Newtype DvbSdtSettings _
derive instance repGenericDvbSdtSettings :: Generic DvbSdtSettings _
instance showDvbSdtSettings :: Show DvbSdtSettings where
  show = genericShow
instance decodeDvbSdtSettings :: Decode DvbSdtSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSdtSettings :: Encode DvbSdtSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DvbSubDestinationAlignment
newtype DvbSubDestinationAlignment = DvbSubDestinationAlignment String
derive instance newtypeDvbSubDestinationAlignment :: Newtype DvbSubDestinationAlignment _
derive instance repGenericDvbSubDestinationAlignment :: Generic DvbSubDestinationAlignment _
instance showDvbSubDestinationAlignment :: Show DvbSubDestinationAlignment where
  show = genericShow
instance decodeDvbSubDestinationAlignment :: Decode DvbSubDestinationAlignment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubDestinationAlignment :: Encode DvbSubDestinationAlignment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DvbSubDestinationBackgroundColor
newtype DvbSubDestinationBackgroundColor = DvbSubDestinationBackgroundColor String
derive instance newtypeDvbSubDestinationBackgroundColor :: Newtype DvbSubDestinationBackgroundColor _
derive instance repGenericDvbSubDestinationBackgroundColor :: Generic DvbSubDestinationBackgroundColor _
instance showDvbSubDestinationBackgroundColor :: Show DvbSubDestinationBackgroundColor where
  show = genericShow
instance decodeDvbSubDestinationBackgroundColor :: Decode DvbSubDestinationBackgroundColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubDestinationBackgroundColor :: Encode DvbSubDestinationBackgroundColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DvbSubDestinationFontColor
newtype DvbSubDestinationFontColor = DvbSubDestinationFontColor String
derive instance newtypeDvbSubDestinationFontColor :: Newtype DvbSubDestinationFontColor _
derive instance repGenericDvbSubDestinationFontColor :: Generic DvbSubDestinationFontColor _
instance showDvbSubDestinationFontColor :: Show DvbSubDestinationFontColor where
  show = genericShow
instance decodeDvbSubDestinationFontColor :: Decode DvbSubDestinationFontColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubDestinationFontColor :: Encode DvbSubDestinationFontColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DvbSubDestinationOutlineColor
newtype DvbSubDestinationOutlineColor = DvbSubDestinationOutlineColor String
derive instance newtypeDvbSubDestinationOutlineColor :: Newtype DvbSubDestinationOutlineColor _
derive instance repGenericDvbSubDestinationOutlineColor :: Generic DvbSubDestinationOutlineColor _
instance showDvbSubDestinationOutlineColor :: Show DvbSubDestinationOutlineColor where
  show = genericShow
instance decodeDvbSubDestinationOutlineColor :: Decode DvbSubDestinationOutlineColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubDestinationOutlineColor :: Encode DvbSubDestinationOutlineColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DvbSubDestinationSettings
newtype DvbSubDestinationSettings = DvbSubDestinationSettings 
  { "Alignment" :: NullOrUndefined.NullOrUndefined (DvbSubDestinationAlignment)
  , "BackgroundColor" :: NullOrUndefined.NullOrUndefined (DvbSubDestinationBackgroundColor)
  , "BackgroundOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "Font" :: NullOrUndefined.NullOrUndefined (InputLocation)
  , "FontColor" :: NullOrUndefined.NullOrUndefined (DvbSubDestinationFontColor)
  , "FontOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontResolution" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontSize" :: NullOrUndefined.NullOrUndefined (String)
  , "OutlineColor" :: NullOrUndefined.NullOrUndefined (DvbSubDestinationOutlineColor)
  , "OutlineSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowColor" :: NullOrUndefined.NullOrUndefined (DvbSubDestinationShadowColor)
  , "ShadowOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowXOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowYOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "TeletextGridControl" :: NullOrUndefined.NullOrUndefined (DvbSubDestinationTeletextGridControl)
  , "XPosition" :: NullOrUndefined.NullOrUndefined (Int)
  , "YPosition" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDvbSubDestinationSettings :: Newtype DvbSubDestinationSettings _
derive instance repGenericDvbSubDestinationSettings :: Generic DvbSubDestinationSettings _
instance showDvbSubDestinationSettings :: Show DvbSubDestinationSettings where
  show = genericShow
instance decodeDvbSubDestinationSettings :: Decode DvbSubDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubDestinationSettings :: Encode DvbSubDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DvbSubDestinationShadowColor
newtype DvbSubDestinationShadowColor = DvbSubDestinationShadowColor String
derive instance newtypeDvbSubDestinationShadowColor :: Newtype DvbSubDestinationShadowColor _
derive instance repGenericDvbSubDestinationShadowColor :: Generic DvbSubDestinationShadowColor _
instance showDvbSubDestinationShadowColor :: Show DvbSubDestinationShadowColor where
  show = genericShow
instance decodeDvbSubDestinationShadowColor :: Decode DvbSubDestinationShadowColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubDestinationShadowColor :: Encode DvbSubDestinationShadowColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DvbSubDestinationTeletextGridControl
newtype DvbSubDestinationTeletextGridControl = DvbSubDestinationTeletextGridControl String
derive instance newtypeDvbSubDestinationTeletextGridControl :: Newtype DvbSubDestinationTeletextGridControl _
derive instance repGenericDvbSubDestinationTeletextGridControl :: Generic DvbSubDestinationTeletextGridControl _
instance showDvbSubDestinationTeletextGridControl :: Show DvbSubDestinationTeletextGridControl where
  show = genericShow
instance decodeDvbSubDestinationTeletextGridControl :: Decode DvbSubDestinationTeletextGridControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubDestinationTeletextGridControl :: Encode DvbSubDestinationTeletextGridControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for DvbSubSourceSettings
newtype DvbSubSourceSettings = DvbSubSourceSettings 
  { "Pid" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDvbSubSourceSettings :: Newtype DvbSubSourceSettings _
derive instance repGenericDvbSubSourceSettings :: Generic DvbSubSourceSettings _
instance showDvbSubSourceSettings :: Show DvbSubSourceSettings where
  show = genericShow
instance decodeDvbSubSourceSettings :: Decode DvbSubSourceSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubSourceSettings :: Encode DvbSubSourceSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | DVB Time and Date Table (SDT)
newtype DvbTdtSettings = DvbTdtSettings 
  { "RepInterval" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDvbTdtSettings :: Newtype DvbTdtSettings _
derive instance repGenericDvbTdtSettings :: Generic DvbTdtSettings _
instance showDvbTdtSettings :: Show DvbTdtSettings where
  show = genericShow
instance decodeDvbTdtSettings :: Decode DvbTdtSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbTdtSettings :: Encode DvbTdtSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3AttenuationControl
newtype Eac3AttenuationControl = Eac3AttenuationControl String
derive instance newtypeEac3AttenuationControl :: Newtype Eac3AttenuationControl _
derive instance repGenericEac3AttenuationControl :: Generic Eac3AttenuationControl _
instance showEac3AttenuationControl :: Show Eac3AttenuationControl where
  show = genericShow
instance decodeEac3AttenuationControl :: Decode Eac3AttenuationControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3AttenuationControl :: Encode Eac3AttenuationControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3BitstreamMode
newtype Eac3BitstreamMode = Eac3BitstreamMode String
derive instance newtypeEac3BitstreamMode :: Newtype Eac3BitstreamMode _
derive instance repGenericEac3BitstreamMode :: Generic Eac3BitstreamMode _
instance showEac3BitstreamMode :: Show Eac3BitstreamMode where
  show = genericShow
instance decodeEac3BitstreamMode :: Decode Eac3BitstreamMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3BitstreamMode :: Encode Eac3BitstreamMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3CodingMode
newtype Eac3CodingMode = Eac3CodingMode String
derive instance newtypeEac3CodingMode :: Newtype Eac3CodingMode _
derive instance repGenericEac3CodingMode :: Generic Eac3CodingMode _
instance showEac3CodingMode :: Show Eac3CodingMode where
  show = genericShow
instance decodeEac3CodingMode :: Decode Eac3CodingMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3CodingMode :: Encode Eac3CodingMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3DcFilter
newtype Eac3DcFilter = Eac3DcFilter String
derive instance newtypeEac3DcFilter :: Newtype Eac3DcFilter _
derive instance repGenericEac3DcFilter :: Generic Eac3DcFilter _
instance showEac3DcFilter :: Show Eac3DcFilter where
  show = genericShow
instance decodeEac3DcFilter :: Decode Eac3DcFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3DcFilter :: Encode Eac3DcFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3DrcLine
newtype Eac3DrcLine = Eac3DrcLine String
derive instance newtypeEac3DrcLine :: Newtype Eac3DrcLine _
derive instance repGenericEac3DrcLine :: Generic Eac3DrcLine _
instance showEac3DrcLine :: Show Eac3DrcLine where
  show = genericShow
instance decodeEac3DrcLine :: Decode Eac3DrcLine where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3DrcLine :: Encode Eac3DrcLine where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3DrcRf
newtype Eac3DrcRf = Eac3DrcRf String
derive instance newtypeEac3DrcRf :: Newtype Eac3DrcRf _
derive instance repGenericEac3DrcRf :: Generic Eac3DrcRf _
instance showEac3DrcRf :: Show Eac3DrcRf where
  show = genericShow
instance decodeEac3DrcRf :: Decode Eac3DrcRf where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3DrcRf :: Encode Eac3DrcRf where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3LfeControl
newtype Eac3LfeControl = Eac3LfeControl String
derive instance newtypeEac3LfeControl :: Newtype Eac3LfeControl _
derive instance repGenericEac3LfeControl :: Generic Eac3LfeControl _
instance showEac3LfeControl :: Show Eac3LfeControl where
  show = genericShow
instance decodeEac3LfeControl :: Decode Eac3LfeControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3LfeControl :: Encode Eac3LfeControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3LfeFilter
newtype Eac3LfeFilter = Eac3LfeFilter String
derive instance newtypeEac3LfeFilter :: Newtype Eac3LfeFilter _
derive instance repGenericEac3LfeFilter :: Generic Eac3LfeFilter _
instance showEac3LfeFilter :: Show Eac3LfeFilter where
  show = genericShow
instance decodeEac3LfeFilter :: Decode Eac3LfeFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3LfeFilter :: Encode Eac3LfeFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3MetadataControl
newtype Eac3MetadataControl = Eac3MetadataControl String
derive instance newtypeEac3MetadataControl :: Newtype Eac3MetadataControl _
derive instance repGenericEac3MetadataControl :: Generic Eac3MetadataControl _
instance showEac3MetadataControl :: Show Eac3MetadataControl where
  show = genericShow
instance decodeEac3MetadataControl :: Decode Eac3MetadataControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3MetadataControl :: Encode Eac3MetadataControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3PassthroughControl
newtype Eac3PassthroughControl = Eac3PassthroughControl String
derive instance newtypeEac3PassthroughControl :: Newtype Eac3PassthroughControl _
derive instance repGenericEac3PassthroughControl :: Generic Eac3PassthroughControl _
instance showEac3PassthroughControl :: Show Eac3PassthroughControl where
  show = genericShow
instance decodeEac3PassthroughControl :: Decode Eac3PassthroughControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3PassthroughControl :: Encode Eac3PassthroughControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3PhaseControl
newtype Eac3PhaseControl = Eac3PhaseControl String
derive instance newtypeEac3PhaseControl :: Newtype Eac3PhaseControl _
derive instance repGenericEac3PhaseControl :: Generic Eac3PhaseControl _
instance showEac3PhaseControl :: Show Eac3PhaseControl where
  show = genericShow
instance decodeEac3PhaseControl :: Decode Eac3PhaseControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3PhaseControl :: Encode Eac3PhaseControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3Settings
newtype Eac3Settings = Eac3Settings 
  { "AttenuationControl" :: NullOrUndefined.NullOrUndefined (Eac3AttenuationControl)
  , "Bitrate" :: NullOrUndefined.NullOrUndefined (Number)
  , "BitstreamMode" :: NullOrUndefined.NullOrUndefined (Eac3BitstreamMode)
  , "CodingMode" :: NullOrUndefined.NullOrUndefined (Eac3CodingMode)
  , "DcFilter" :: NullOrUndefined.NullOrUndefined (Eac3DcFilter)
  , "Dialnorm" :: NullOrUndefined.NullOrUndefined (Int)
  , "DrcLine" :: NullOrUndefined.NullOrUndefined (Eac3DrcLine)
  , "DrcRf" :: NullOrUndefined.NullOrUndefined (Eac3DrcRf)
  , "LfeControl" :: NullOrUndefined.NullOrUndefined (Eac3LfeControl)
  , "LfeFilter" :: NullOrUndefined.NullOrUndefined (Eac3LfeFilter)
  , "LoRoCenterMixLevel" :: NullOrUndefined.NullOrUndefined (Number)
  , "LoRoSurroundMixLevel" :: NullOrUndefined.NullOrUndefined (Number)
  , "LtRtCenterMixLevel" :: NullOrUndefined.NullOrUndefined (Number)
  , "LtRtSurroundMixLevel" :: NullOrUndefined.NullOrUndefined (Number)
  , "MetadataControl" :: NullOrUndefined.NullOrUndefined (Eac3MetadataControl)
  , "PassthroughControl" :: NullOrUndefined.NullOrUndefined (Eac3PassthroughControl)
  , "PhaseControl" :: NullOrUndefined.NullOrUndefined (Eac3PhaseControl)
  , "StereoDownmix" :: NullOrUndefined.NullOrUndefined (Eac3StereoDownmix)
  , "SurroundExMode" :: NullOrUndefined.NullOrUndefined (Eac3SurroundExMode)
  , "SurroundMode" :: NullOrUndefined.NullOrUndefined (Eac3SurroundMode)
  }
derive instance newtypeEac3Settings :: Newtype Eac3Settings _
derive instance repGenericEac3Settings :: Generic Eac3Settings _
instance showEac3Settings :: Show Eac3Settings where
  show = genericShow
instance decodeEac3Settings :: Decode Eac3Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3Settings :: Encode Eac3Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3StereoDownmix
newtype Eac3StereoDownmix = Eac3StereoDownmix String
derive instance newtypeEac3StereoDownmix :: Newtype Eac3StereoDownmix _
derive instance repGenericEac3StereoDownmix :: Generic Eac3StereoDownmix _
instance showEac3StereoDownmix :: Show Eac3StereoDownmix where
  show = genericShow
instance decodeEac3StereoDownmix :: Decode Eac3StereoDownmix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3StereoDownmix :: Encode Eac3StereoDownmix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3SurroundExMode
newtype Eac3SurroundExMode = Eac3SurroundExMode String
derive instance newtypeEac3SurroundExMode :: Newtype Eac3SurroundExMode _
derive instance repGenericEac3SurroundExMode :: Generic Eac3SurroundExMode _
instance showEac3SurroundExMode :: Show Eac3SurroundExMode where
  show = genericShow
instance decodeEac3SurroundExMode :: Decode Eac3SurroundExMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3SurroundExMode :: Encode Eac3SurroundExMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Eac3SurroundMode
newtype Eac3SurroundMode = Eac3SurroundMode String
derive instance newtypeEac3SurroundMode :: Newtype Eac3SurroundMode _
derive instance repGenericEac3SurroundMode :: Generic Eac3SurroundMode _
instance showEac3SurroundMode :: Show Eac3SurroundMode where
  show = genericShow
instance decodeEac3SurroundMode :: Decode Eac3SurroundMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3SurroundMode :: Encode Eac3SurroundMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for EmbeddedConvert608To708
newtype EmbeddedConvert608To708 = EmbeddedConvert608To708 String
derive instance newtypeEmbeddedConvert608To708 :: Newtype EmbeddedConvert608To708 _
derive instance repGenericEmbeddedConvert608To708 :: Generic EmbeddedConvert608To708 _
instance showEmbeddedConvert608To708 :: Show EmbeddedConvert608To708 where
  show = genericShow
instance decodeEmbeddedConvert608To708 :: Decode EmbeddedConvert608To708 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmbeddedConvert608To708 :: Encode EmbeddedConvert608To708 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for EmbeddedDestinationSettings
newtype EmbeddedDestinationSettings = EmbeddedDestinationSettings Types.NoArguments
derive instance newtypeEmbeddedDestinationSettings :: Newtype EmbeddedDestinationSettings _
derive instance repGenericEmbeddedDestinationSettings :: Generic EmbeddedDestinationSettings _
instance showEmbeddedDestinationSettings :: Show EmbeddedDestinationSettings where
  show = genericShow
instance decodeEmbeddedDestinationSettings :: Decode EmbeddedDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmbeddedDestinationSettings :: Encode EmbeddedDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for EmbeddedPlusScte20DestinationSettings
newtype EmbeddedPlusScte20DestinationSettings = EmbeddedPlusScte20DestinationSettings Types.NoArguments
derive instance newtypeEmbeddedPlusScte20DestinationSettings :: Newtype EmbeddedPlusScte20DestinationSettings _
derive instance repGenericEmbeddedPlusScte20DestinationSettings :: Generic EmbeddedPlusScte20DestinationSettings _
instance showEmbeddedPlusScte20DestinationSettings :: Show EmbeddedPlusScte20DestinationSettings where
  show = genericShow
instance decodeEmbeddedPlusScte20DestinationSettings :: Decode EmbeddedPlusScte20DestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmbeddedPlusScte20DestinationSettings :: Encode EmbeddedPlusScte20DestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for EmbeddedScte20Detection
newtype EmbeddedScte20Detection = EmbeddedScte20Detection String
derive instance newtypeEmbeddedScte20Detection :: Newtype EmbeddedScte20Detection _
derive instance repGenericEmbeddedScte20Detection :: Generic EmbeddedScte20Detection _
instance showEmbeddedScte20Detection :: Show EmbeddedScte20Detection where
  show = genericShow
instance decodeEmbeddedScte20Detection :: Decode EmbeddedScte20Detection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmbeddedScte20Detection :: Encode EmbeddedScte20Detection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for EmbeddedSourceSettings
newtype EmbeddedSourceSettings = EmbeddedSourceSettings 
  { "Convert608To708" :: NullOrUndefined.NullOrUndefined (EmbeddedConvert608To708)
  , "Scte20Detection" :: NullOrUndefined.NullOrUndefined (EmbeddedScte20Detection)
  , "Source608ChannelNumber" :: NullOrUndefined.NullOrUndefined (Int)
  , "Source608TrackNumber" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeEmbeddedSourceSettings :: Newtype EmbeddedSourceSettings _
derive instance repGenericEmbeddedSourceSettings :: Generic EmbeddedSourceSettings _
instance showEmbeddedSourceSettings :: Show EmbeddedSourceSettings where
  show = genericShow
instance decodeEmbeddedSourceSettings :: Decode EmbeddedSourceSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmbeddedSourceSettings :: Encode EmbeddedSourceSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Empty
newtype Empty = Empty Types.NoArguments
derive instance newtypeEmpty :: Newtype Empty _
derive instance repGenericEmpty :: Generic Empty _
instance showEmpty :: Show Empty where
  show = genericShow
instance decodeEmpty :: Decode Empty where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmpty :: Encode Empty where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for EncoderSettings
newtype EncoderSettings = EncoderSettings 
  { "AudioDescriptions" :: NullOrUndefined.NullOrUndefined (ListOfAudioDescription)
  , "AvailBlanking" :: NullOrUndefined.NullOrUndefined (AvailBlanking)
  , "AvailConfiguration" :: NullOrUndefined.NullOrUndefined (AvailConfiguration)
  , "BlackoutSlate" :: NullOrUndefined.NullOrUndefined (BlackoutSlate)
  , "CaptionDescriptions" :: NullOrUndefined.NullOrUndefined (ListOfCaptionDescription)
  , "GlobalConfiguration" :: NullOrUndefined.NullOrUndefined (GlobalConfiguration)
  , "OutputGroups" :: NullOrUndefined.NullOrUndefined (ListOfOutputGroup)
  , "TimecodeConfig" :: NullOrUndefined.NullOrUndefined (TimecodeConfig)
  , "VideoDescriptions" :: NullOrUndefined.NullOrUndefined (ListOfVideoDescription)
  }
derive instance newtypeEncoderSettings :: Newtype EncoderSettings _
derive instance repGenericEncoderSettings :: Generic EncoderSettings _
instance showEncoderSettings :: Show EncoderSettings where
  show = genericShow
instance decodeEncoderSettings :: Decode EncoderSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncoderSettings :: Encode EncoderSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for FecOutputIncludeFec
newtype FecOutputIncludeFec = FecOutputIncludeFec String
derive instance newtypeFecOutputIncludeFec :: Newtype FecOutputIncludeFec _
derive instance repGenericFecOutputIncludeFec :: Generic FecOutputIncludeFec _
instance showFecOutputIncludeFec :: Show FecOutputIncludeFec where
  show = genericShow
instance decodeFecOutputIncludeFec :: Decode FecOutputIncludeFec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFecOutputIncludeFec :: Encode FecOutputIncludeFec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for FecOutputSettings
newtype FecOutputSettings = FecOutputSettings 
  { "ColumnDepth" :: NullOrUndefined.NullOrUndefined (Int)
  , "IncludeFec" :: NullOrUndefined.NullOrUndefined (FecOutputIncludeFec)
  , "RowLength" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeFecOutputSettings :: Newtype FecOutputSettings _
derive instance repGenericFecOutputSettings :: Generic FecOutputSettings _
instance showFecOutputSettings :: Show FecOutputSettings where
  show = genericShow
instance decodeFecOutputSettings :: Decode FecOutputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFecOutputSettings :: Encode FecOutputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for FixedAfd
newtype FixedAfd = FixedAfd String
derive instance newtypeFixedAfd :: Newtype FixedAfd _
derive instance repGenericFixedAfd :: Generic FixedAfd _
instance showFixedAfd :: Show FixedAfd where
  show = genericShow
instance decodeFixedAfd :: Decode FixedAfd where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFixedAfd :: Encode FixedAfd where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ForbiddenException
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


-- | Placeholder documentation for GatewayTimeoutException
newtype GatewayTimeoutException = GatewayTimeoutException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGatewayTimeoutException :: Newtype GatewayTimeoutException _
derive instance repGenericGatewayTimeoutException :: Generic GatewayTimeoutException _
instance showGatewayTimeoutException :: Show GatewayTimeoutException where
  show = genericShow
instance decodeGatewayTimeoutException :: Decode GatewayTimeoutException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGatewayTimeoutException :: Encode GatewayTimeoutException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for GlobalConfiguration
newtype GlobalConfiguration = GlobalConfiguration 
  { "InitialAudioGain" :: NullOrUndefined.NullOrUndefined (Int)
  , "InputEndAction" :: NullOrUndefined.NullOrUndefined (GlobalConfigurationInputEndAction)
  , "InputLossBehavior" :: NullOrUndefined.NullOrUndefined (InputLossBehavior)
  , "OutputTimingSource" :: NullOrUndefined.NullOrUndefined (GlobalConfigurationOutputTimingSource)
  , "SupportLowFramerateInputs" :: NullOrUndefined.NullOrUndefined (GlobalConfigurationLowFramerateInputs)
  }
derive instance newtypeGlobalConfiguration :: Newtype GlobalConfiguration _
derive instance repGenericGlobalConfiguration :: Generic GlobalConfiguration _
instance showGlobalConfiguration :: Show GlobalConfiguration where
  show = genericShow
instance decodeGlobalConfiguration :: Decode GlobalConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGlobalConfiguration :: Encode GlobalConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for GlobalConfigurationInputEndAction
newtype GlobalConfigurationInputEndAction = GlobalConfigurationInputEndAction String
derive instance newtypeGlobalConfigurationInputEndAction :: Newtype GlobalConfigurationInputEndAction _
derive instance repGenericGlobalConfigurationInputEndAction :: Generic GlobalConfigurationInputEndAction _
instance showGlobalConfigurationInputEndAction :: Show GlobalConfigurationInputEndAction where
  show = genericShow
instance decodeGlobalConfigurationInputEndAction :: Decode GlobalConfigurationInputEndAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGlobalConfigurationInputEndAction :: Encode GlobalConfigurationInputEndAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for GlobalConfigurationLowFramerateInputs
newtype GlobalConfigurationLowFramerateInputs = GlobalConfigurationLowFramerateInputs String
derive instance newtypeGlobalConfigurationLowFramerateInputs :: Newtype GlobalConfigurationLowFramerateInputs _
derive instance repGenericGlobalConfigurationLowFramerateInputs :: Generic GlobalConfigurationLowFramerateInputs _
instance showGlobalConfigurationLowFramerateInputs :: Show GlobalConfigurationLowFramerateInputs where
  show = genericShow
instance decodeGlobalConfigurationLowFramerateInputs :: Decode GlobalConfigurationLowFramerateInputs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGlobalConfigurationLowFramerateInputs :: Encode GlobalConfigurationLowFramerateInputs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for GlobalConfigurationOutputTimingSource
newtype GlobalConfigurationOutputTimingSource = GlobalConfigurationOutputTimingSource String
derive instance newtypeGlobalConfigurationOutputTimingSource :: Newtype GlobalConfigurationOutputTimingSource _
derive instance repGenericGlobalConfigurationOutputTimingSource :: Generic GlobalConfigurationOutputTimingSource _
instance showGlobalConfigurationOutputTimingSource :: Show GlobalConfigurationOutputTimingSource where
  show = genericShow
instance decodeGlobalConfigurationOutputTimingSource :: Decode GlobalConfigurationOutputTimingSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGlobalConfigurationOutputTimingSource :: Encode GlobalConfigurationOutputTimingSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264AdaptiveQuantization
newtype H264AdaptiveQuantization = H264AdaptiveQuantization String
derive instance newtypeH264AdaptiveQuantization :: Newtype H264AdaptiveQuantization _
derive instance repGenericH264AdaptiveQuantization :: Generic H264AdaptiveQuantization _
instance showH264AdaptiveQuantization :: Show H264AdaptiveQuantization where
  show = genericShow
instance decodeH264AdaptiveQuantization :: Decode H264AdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264AdaptiveQuantization :: Encode H264AdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264ColorMetadata
newtype H264ColorMetadata = H264ColorMetadata String
derive instance newtypeH264ColorMetadata :: Newtype H264ColorMetadata _
derive instance repGenericH264ColorMetadata :: Generic H264ColorMetadata _
instance showH264ColorMetadata :: Show H264ColorMetadata where
  show = genericShow
instance decodeH264ColorMetadata :: Decode H264ColorMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264ColorMetadata :: Encode H264ColorMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264EntropyEncoding
newtype H264EntropyEncoding = H264EntropyEncoding String
derive instance newtypeH264EntropyEncoding :: Newtype H264EntropyEncoding _
derive instance repGenericH264EntropyEncoding :: Generic H264EntropyEncoding _
instance showH264EntropyEncoding :: Show H264EntropyEncoding where
  show = genericShow
instance decodeH264EntropyEncoding :: Decode H264EntropyEncoding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264EntropyEncoding :: Encode H264EntropyEncoding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264FlickerAq
newtype H264FlickerAq = H264FlickerAq String
derive instance newtypeH264FlickerAq :: Newtype H264FlickerAq _
derive instance repGenericH264FlickerAq :: Generic H264FlickerAq _
instance showH264FlickerAq :: Show H264FlickerAq where
  show = genericShow
instance decodeH264FlickerAq :: Decode H264FlickerAq where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264FlickerAq :: Encode H264FlickerAq where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264FramerateControl
newtype H264FramerateControl = H264FramerateControl String
derive instance newtypeH264FramerateControl :: Newtype H264FramerateControl _
derive instance repGenericH264FramerateControl :: Generic H264FramerateControl _
instance showH264FramerateControl :: Show H264FramerateControl where
  show = genericShow
instance decodeH264FramerateControl :: Decode H264FramerateControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264FramerateControl :: Encode H264FramerateControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264GopBReference
newtype H264GopBReference = H264GopBReference String
derive instance newtypeH264GopBReference :: Newtype H264GopBReference _
derive instance repGenericH264GopBReference :: Generic H264GopBReference _
instance showH264GopBReference :: Show H264GopBReference where
  show = genericShow
instance decodeH264GopBReference :: Decode H264GopBReference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264GopBReference :: Encode H264GopBReference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264GopSizeUnits
newtype H264GopSizeUnits = H264GopSizeUnits String
derive instance newtypeH264GopSizeUnits :: Newtype H264GopSizeUnits _
derive instance repGenericH264GopSizeUnits :: Generic H264GopSizeUnits _
instance showH264GopSizeUnits :: Show H264GopSizeUnits where
  show = genericShow
instance decodeH264GopSizeUnits :: Decode H264GopSizeUnits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264GopSizeUnits :: Encode H264GopSizeUnits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264Level
newtype H264Level = H264Level String
derive instance newtypeH264Level :: Newtype H264Level _
derive instance repGenericH264Level :: Generic H264Level _
instance showH264Level :: Show H264Level where
  show = genericShow
instance decodeH264Level :: Decode H264Level where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264Level :: Encode H264Level where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264LookAheadRateControl
newtype H264LookAheadRateControl = H264LookAheadRateControl String
derive instance newtypeH264LookAheadRateControl :: Newtype H264LookAheadRateControl _
derive instance repGenericH264LookAheadRateControl :: Generic H264LookAheadRateControl _
instance showH264LookAheadRateControl :: Show H264LookAheadRateControl where
  show = genericShow
instance decodeH264LookAheadRateControl :: Decode H264LookAheadRateControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264LookAheadRateControl :: Encode H264LookAheadRateControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264ParControl
newtype H264ParControl = H264ParControl String
derive instance newtypeH264ParControl :: Newtype H264ParControl _
derive instance repGenericH264ParControl :: Generic H264ParControl _
instance showH264ParControl :: Show H264ParControl where
  show = genericShow
instance decodeH264ParControl :: Decode H264ParControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264ParControl :: Encode H264ParControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264Profile
newtype H264Profile = H264Profile String
derive instance newtypeH264Profile :: Newtype H264Profile _
derive instance repGenericH264Profile :: Generic H264Profile _
instance showH264Profile :: Show H264Profile where
  show = genericShow
instance decodeH264Profile :: Decode H264Profile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264Profile :: Encode H264Profile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264RateControlMode
newtype H264RateControlMode = H264RateControlMode String
derive instance newtypeH264RateControlMode :: Newtype H264RateControlMode _
derive instance repGenericH264RateControlMode :: Generic H264RateControlMode _
instance showH264RateControlMode :: Show H264RateControlMode where
  show = genericShow
instance decodeH264RateControlMode :: Decode H264RateControlMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264RateControlMode :: Encode H264RateControlMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264ScanType
newtype H264ScanType = H264ScanType String
derive instance newtypeH264ScanType :: Newtype H264ScanType _
derive instance repGenericH264ScanType :: Generic H264ScanType _
instance showH264ScanType :: Show H264ScanType where
  show = genericShow
instance decodeH264ScanType :: Decode H264ScanType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264ScanType :: Encode H264ScanType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264SceneChangeDetect
newtype H264SceneChangeDetect = H264SceneChangeDetect String
derive instance newtypeH264SceneChangeDetect :: Newtype H264SceneChangeDetect _
derive instance repGenericH264SceneChangeDetect :: Generic H264SceneChangeDetect _
instance showH264SceneChangeDetect :: Show H264SceneChangeDetect where
  show = genericShow
instance decodeH264SceneChangeDetect :: Decode H264SceneChangeDetect where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264SceneChangeDetect :: Encode H264SceneChangeDetect where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264Settings
newtype H264Settings = H264Settings 
  { "AdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (H264AdaptiveQuantization)
  , "AfdSignaling" :: NullOrUndefined.NullOrUndefined (AfdSignaling)
  , "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "BufFillPct" :: NullOrUndefined.NullOrUndefined (Int)
  , "BufSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "ColorMetadata" :: NullOrUndefined.NullOrUndefined (H264ColorMetadata)
  , "EntropyEncoding" :: NullOrUndefined.NullOrUndefined (H264EntropyEncoding)
  , "FixedAfd" :: NullOrUndefined.NullOrUndefined (FixedAfd)
  , "FlickerAq" :: NullOrUndefined.NullOrUndefined (H264FlickerAq)
  , "FramerateControl" :: NullOrUndefined.NullOrUndefined (H264FramerateControl)
  , "FramerateDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "GopBReference" :: NullOrUndefined.NullOrUndefined (H264GopBReference)
  , "GopClosedCadence" :: NullOrUndefined.NullOrUndefined (Int)
  , "GopNumBFrames" :: NullOrUndefined.NullOrUndefined (Int)
  , "GopSize" :: NullOrUndefined.NullOrUndefined (Number)
  , "GopSizeUnits" :: NullOrUndefined.NullOrUndefined (H264GopSizeUnits)
  , "Level" :: NullOrUndefined.NullOrUndefined (H264Level)
  , "LookAheadRateControl" :: NullOrUndefined.NullOrUndefined (H264LookAheadRateControl)
  , "MaxBitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinIInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "NumRefFrames" :: NullOrUndefined.NullOrUndefined (Int)
  , "ParControl" :: NullOrUndefined.NullOrUndefined (H264ParControl)
  , "ParDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "Profile" :: NullOrUndefined.NullOrUndefined (H264Profile)
  , "RateControlMode" :: NullOrUndefined.NullOrUndefined (H264RateControlMode)
  , "ScanType" :: NullOrUndefined.NullOrUndefined (H264ScanType)
  , "SceneChangeDetect" :: NullOrUndefined.NullOrUndefined (H264SceneChangeDetect)
  , "Slices" :: NullOrUndefined.NullOrUndefined (Int)
  , "Softness" :: NullOrUndefined.NullOrUndefined (Int)
  , "SpatialAq" :: NullOrUndefined.NullOrUndefined (H264SpatialAq)
  , "Syntax" :: NullOrUndefined.NullOrUndefined (H264Syntax)
  , "TemporalAq" :: NullOrUndefined.NullOrUndefined (H264TemporalAq)
  , "TimecodeInsertion" :: NullOrUndefined.NullOrUndefined (H264TimecodeInsertionBehavior)
  }
derive instance newtypeH264Settings :: Newtype H264Settings _
derive instance repGenericH264Settings :: Generic H264Settings _
instance showH264Settings :: Show H264Settings where
  show = genericShow
instance decodeH264Settings :: Decode H264Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264Settings :: Encode H264Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264SpatialAq
newtype H264SpatialAq = H264SpatialAq String
derive instance newtypeH264SpatialAq :: Newtype H264SpatialAq _
derive instance repGenericH264SpatialAq :: Generic H264SpatialAq _
instance showH264SpatialAq :: Show H264SpatialAq where
  show = genericShow
instance decodeH264SpatialAq :: Decode H264SpatialAq where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264SpatialAq :: Encode H264SpatialAq where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264Syntax
newtype H264Syntax = H264Syntax String
derive instance newtypeH264Syntax :: Newtype H264Syntax _
derive instance repGenericH264Syntax :: Generic H264Syntax _
instance showH264Syntax :: Show H264Syntax where
  show = genericShow
instance decodeH264Syntax :: Decode H264Syntax where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264Syntax :: Encode H264Syntax where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264TemporalAq
newtype H264TemporalAq = H264TemporalAq String
derive instance newtypeH264TemporalAq :: Newtype H264TemporalAq _
derive instance repGenericH264TemporalAq :: Generic H264TemporalAq _
instance showH264TemporalAq :: Show H264TemporalAq where
  show = genericShow
instance decodeH264TemporalAq :: Decode H264TemporalAq where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264TemporalAq :: Encode H264TemporalAq where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for H264TimecodeInsertionBehavior
newtype H264TimecodeInsertionBehavior = H264TimecodeInsertionBehavior String
derive instance newtypeH264TimecodeInsertionBehavior :: Newtype H264TimecodeInsertionBehavior _
derive instance repGenericH264TimecodeInsertionBehavior :: Generic H264TimecodeInsertionBehavior _
instance showH264TimecodeInsertionBehavior :: Show H264TimecodeInsertionBehavior where
  show = genericShow
instance decodeH264TimecodeInsertionBehavior :: Decode H264TimecodeInsertionBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264TimecodeInsertionBehavior :: Encode H264TimecodeInsertionBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsAdMarkers
newtype HlsAdMarkers = HlsAdMarkers String
derive instance newtypeHlsAdMarkers :: Newtype HlsAdMarkers _
derive instance repGenericHlsAdMarkers :: Generic HlsAdMarkers _
instance showHlsAdMarkers :: Show HlsAdMarkers where
  show = genericShow
instance decodeHlsAdMarkers :: Decode HlsAdMarkers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsAdMarkers :: Encode HlsAdMarkers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsAkamaiHttpTransferMode
newtype HlsAkamaiHttpTransferMode = HlsAkamaiHttpTransferMode String
derive instance newtypeHlsAkamaiHttpTransferMode :: Newtype HlsAkamaiHttpTransferMode _
derive instance repGenericHlsAkamaiHttpTransferMode :: Generic HlsAkamaiHttpTransferMode _
instance showHlsAkamaiHttpTransferMode :: Show HlsAkamaiHttpTransferMode where
  show = genericShow
instance decodeHlsAkamaiHttpTransferMode :: Decode HlsAkamaiHttpTransferMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsAkamaiHttpTransferMode :: Encode HlsAkamaiHttpTransferMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsAkamaiSettings
newtype HlsAkamaiSettings = HlsAkamaiSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined.NullOrUndefined (Int)
  , "HttpTransferMode" :: NullOrUndefined.NullOrUndefined (HlsAkamaiHttpTransferMode)
  , "NumRetries" :: NullOrUndefined.NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined.NullOrUndefined (Int)
  , "Salt" :: NullOrUndefined.NullOrUndefined (String)
  , "Token" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHlsAkamaiSettings :: Newtype HlsAkamaiSettings _
derive instance repGenericHlsAkamaiSettings :: Generic HlsAkamaiSettings _
instance showHlsAkamaiSettings :: Show HlsAkamaiSettings where
  show = genericShow
instance decodeHlsAkamaiSettings :: Decode HlsAkamaiSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsAkamaiSettings :: Encode HlsAkamaiSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsBasicPutSettings
newtype HlsBasicPutSettings = HlsBasicPutSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined.NullOrUndefined (Int)
  , "NumRetries" :: NullOrUndefined.NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeHlsBasicPutSettings :: Newtype HlsBasicPutSettings _
derive instance repGenericHlsBasicPutSettings :: Generic HlsBasicPutSettings _
instance showHlsBasicPutSettings :: Show HlsBasicPutSettings where
  show = genericShow
instance decodeHlsBasicPutSettings :: Decode HlsBasicPutSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsBasicPutSettings :: Encode HlsBasicPutSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsCaptionLanguageSetting
newtype HlsCaptionLanguageSetting = HlsCaptionLanguageSetting String
derive instance newtypeHlsCaptionLanguageSetting :: Newtype HlsCaptionLanguageSetting _
derive instance repGenericHlsCaptionLanguageSetting :: Generic HlsCaptionLanguageSetting _
instance showHlsCaptionLanguageSetting :: Show HlsCaptionLanguageSetting where
  show = genericShow
instance decodeHlsCaptionLanguageSetting :: Decode HlsCaptionLanguageSetting where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsCaptionLanguageSetting :: Encode HlsCaptionLanguageSetting where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsCdnSettings
newtype HlsCdnSettings = HlsCdnSettings 
  { "HlsAkamaiSettings" :: NullOrUndefined.NullOrUndefined (HlsAkamaiSettings)
  , "HlsBasicPutSettings" :: NullOrUndefined.NullOrUndefined (HlsBasicPutSettings)
  , "HlsMediaStoreSettings" :: NullOrUndefined.NullOrUndefined (HlsMediaStoreSettings)
  , "HlsWebdavSettings" :: NullOrUndefined.NullOrUndefined (HlsWebdavSettings)
  }
derive instance newtypeHlsCdnSettings :: Newtype HlsCdnSettings _
derive instance repGenericHlsCdnSettings :: Generic HlsCdnSettings _
instance showHlsCdnSettings :: Show HlsCdnSettings where
  show = genericShow
instance decodeHlsCdnSettings :: Decode HlsCdnSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsCdnSettings :: Encode HlsCdnSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsClientCache
newtype HlsClientCache = HlsClientCache String
derive instance newtypeHlsClientCache :: Newtype HlsClientCache _
derive instance repGenericHlsClientCache :: Generic HlsClientCache _
instance showHlsClientCache :: Show HlsClientCache where
  show = genericShow
instance decodeHlsClientCache :: Decode HlsClientCache where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsClientCache :: Encode HlsClientCache where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsCodecSpecification
newtype HlsCodecSpecification = HlsCodecSpecification String
derive instance newtypeHlsCodecSpecification :: Newtype HlsCodecSpecification _
derive instance repGenericHlsCodecSpecification :: Generic HlsCodecSpecification _
instance showHlsCodecSpecification :: Show HlsCodecSpecification where
  show = genericShow
instance decodeHlsCodecSpecification :: Decode HlsCodecSpecification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsCodecSpecification :: Encode HlsCodecSpecification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsDirectoryStructure
newtype HlsDirectoryStructure = HlsDirectoryStructure String
derive instance newtypeHlsDirectoryStructure :: Newtype HlsDirectoryStructure _
derive instance repGenericHlsDirectoryStructure :: Generic HlsDirectoryStructure _
instance showHlsDirectoryStructure :: Show HlsDirectoryStructure where
  show = genericShow
instance decodeHlsDirectoryStructure :: Decode HlsDirectoryStructure where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsDirectoryStructure :: Encode HlsDirectoryStructure where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsEncryptionType
newtype HlsEncryptionType = HlsEncryptionType String
derive instance newtypeHlsEncryptionType :: Newtype HlsEncryptionType _
derive instance repGenericHlsEncryptionType :: Generic HlsEncryptionType _
instance showHlsEncryptionType :: Show HlsEncryptionType where
  show = genericShow
instance decodeHlsEncryptionType :: Decode HlsEncryptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsEncryptionType :: Encode HlsEncryptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsGroupSettings
newtype HlsGroupSettings = HlsGroupSettings 
  { "AdMarkers" :: NullOrUndefined.NullOrUndefined (ListOfHlsAdMarkers)
  , "BaseUrlContent" :: NullOrUndefined.NullOrUndefined (String)
  , "BaseUrlManifest" :: NullOrUndefined.NullOrUndefined (String)
  , "CaptionLanguageMappings" :: NullOrUndefined.NullOrUndefined (ListOfCaptionLanguageMapping)
  , "CaptionLanguageSetting" :: NullOrUndefined.NullOrUndefined (HlsCaptionLanguageSetting)
  , "ClientCache" :: NullOrUndefined.NullOrUndefined (HlsClientCache)
  , "CodecSpecification" :: NullOrUndefined.NullOrUndefined (HlsCodecSpecification)
  , "ConstantIv" :: NullOrUndefined.NullOrUndefined (String)
  , "Destination" :: NullOrUndefined.NullOrUndefined (OutputLocationRef)
  , "DirectoryStructure" :: NullOrUndefined.NullOrUndefined (HlsDirectoryStructure)
  , "EncryptionType" :: NullOrUndefined.NullOrUndefined (HlsEncryptionType)
  , "HlsCdnSettings" :: NullOrUndefined.NullOrUndefined (HlsCdnSettings)
  , "IndexNSegments" :: NullOrUndefined.NullOrUndefined (Int)
  , "InputLossAction" :: NullOrUndefined.NullOrUndefined (InputLossActionForHlsOut)
  , "IvInManifest" :: NullOrUndefined.NullOrUndefined (HlsIvInManifest)
  , "IvSource" :: NullOrUndefined.NullOrUndefined (HlsIvSource)
  , "KeepSegments" :: NullOrUndefined.NullOrUndefined (Int)
  , "KeyFormat" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyFormatVersions" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyProviderSettings" :: NullOrUndefined.NullOrUndefined (KeyProviderSettings)
  , "ManifestCompression" :: NullOrUndefined.NullOrUndefined (HlsManifestCompression)
  , "ManifestDurationFormat" :: NullOrUndefined.NullOrUndefined (HlsManifestDurationFormat)
  , "MinSegmentLength" :: NullOrUndefined.NullOrUndefined (Int)
  , "Mode" :: NullOrUndefined.NullOrUndefined (HlsMode)
  , "OutputSelection" :: NullOrUndefined.NullOrUndefined (HlsOutputSelection)
  , "ProgramDateTime" :: NullOrUndefined.NullOrUndefined (HlsProgramDateTime)
  , "ProgramDateTimePeriod" :: NullOrUndefined.NullOrUndefined (Int)
  , "SegmentLength" :: NullOrUndefined.NullOrUndefined (Int)
  , "SegmentationMode" :: NullOrUndefined.NullOrUndefined (HlsSegmentationMode)
  , "SegmentsPerSubdirectory" :: NullOrUndefined.NullOrUndefined (Int)
  , "StreamInfResolution" :: NullOrUndefined.NullOrUndefined (HlsStreamInfResolution)
  , "TimedMetadataId3Frame" :: NullOrUndefined.NullOrUndefined (HlsTimedMetadataId3Frame)
  , "TimedMetadataId3Period" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimestampDeltaMilliseconds" :: NullOrUndefined.NullOrUndefined (Int)
  , "TsFileMode" :: NullOrUndefined.NullOrUndefined (HlsTsFileMode)
  }
derive instance newtypeHlsGroupSettings :: Newtype HlsGroupSettings _
derive instance repGenericHlsGroupSettings :: Generic HlsGroupSettings _
instance showHlsGroupSettings :: Show HlsGroupSettings where
  show = genericShow
instance decodeHlsGroupSettings :: Decode HlsGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsGroupSettings :: Encode HlsGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsInputSettings
newtype HlsInputSettings = HlsInputSettings 
  { "Bandwidth" :: NullOrUndefined.NullOrUndefined (Int)
  , "BufferSegments" :: NullOrUndefined.NullOrUndefined (Int)
  , "Retries" :: NullOrUndefined.NullOrUndefined (Int)
  , "RetryInterval" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeHlsInputSettings :: Newtype HlsInputSettings _
derive instance repGenericHlsInputSettings :: Generic HlsInputSettings _
instance showHlsInputSettings :: Show HlsInputSettings where
  show = genericShow
instance decodeHlsInputSettings :: Decode HlsInputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsInputSettings :: Encode HlsInputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsIvInManifest
newtype HlsIvInManifest = HlsIvInManifest String
derive instance newtypeHlsIvInManifest :: Newtype HlsIvInManifest _
derive instance repGenericHlsIvInManifest :: Generic HlsIvInManifest _
instance showHlsIvInManifest :: Show HlsIvInManifest where
  show = genericShow
instance decodeHlsIvInManifest :: Decode HlsIvInManifest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsIvInManifest :: Encode HlsIvInManifest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsIvSource
newtype HlsIvSource = HlsIvSource String
derive instance newtypeHlsIvSource :: Newtype HlsIvSource _
derive instance repGenericHlsIvSource :: Generic HlsIvSource _
instance showHlsIvSource :: Show HlsIvSource where
  show = genericShow
instance decodeHlsIvSource :: Decode HlsIvSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsIvSource :: Encode HlsIvSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsManifestCompression
newtype HlsManifestCompression = HlsManifestCompression String
derive instance newtypeHlsManifestCompression :: Newtype HlsManifestCompression _
derive instance repGenericHlsManifestCompression :: Generic HlsManifestCompression _
instance showHlsManifestCompression :: Show HlsManifestCompression where
  show = genericShow
instance decodeHlsManifestCompression :: Decode HlsManifestCompression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsManifestCompression :: Encode HlsManifestCompression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsManifestDurationFormat
newtype HlsManifestDurationFormat = HlsManifestDurationFormat String
derive instance newtypeHlsManifestDurationFormat :: Newtype HlsManifestDurationFormat _
derive instance repGenericHlsManifestDurationFormat :: Generic HlsManifestDurationFormat _
instance showHlsManifestDurationFormat :: Show HlsManifestDurationFormat where
  show = genericShow
instance decodeHlsManifestDurationFormat :: Decode HlsManifestDurationFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsManifestDurationFormat :: Encode HlsManifestDurationFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsMediaStoreSettings
newtype HlsMediaStoreSettings = HlsMediaStoreSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined.NullOrUndefined (Int)
  , "MediaStoreStorageClass" :: NullOrUndefined.NullOrUndefined (HlsMediaStoreStorageClass)
  , "NumRetries" :: NullOrUndefined.NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeHlsMediaStoreSettings :: Newtype HlsMediaStoreSettings _
derive instance repGenericHlsMediaStoreSettings :: Generic HlsMediaStoreSettings _
instance showHlsMediaStoreSettings :: Show HlsMediaStoreSettings where
  show = genericShow
instance decodeHlsMediaStoreSettings :: Decode HlsMediaStoreSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsMediaStoreSettings :: Encode HlsMediaStoreSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsMediaStoreStorageClass
newtype HlsMediaStoreStorageClass = HlsMediaStoreStorageClass String
derive instance newtypeHlsMediaStoreStorageClass :: Newtype HlsMediaStoreStorageClass _
derive instance repGenericHlsMediaStoreStorageClass :: Generic HlsMediaStoreStorageClass _
instance showHlsMediaStoreStorageClass :: Show HlsMediaStoreStorageClass where
  show = genericShow
instance decodeHlsMediaStoreStorageClass :: Decode HlsMediaStoreStorageClass where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsMediaStoreStorageClass :: Encode HlsMediaStoreStorageClass where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsMode
newtype HlsMode = HlsMode String
derive instance newtypeHlsMode :: Newtype HlsMode _
derive instance repGenericHlsMode :: Generic HlsMode _
instance showHlsMode :: Show HlsMode where
  show = genericShow
instance decodeHlsMode :: Decode HlsMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsMode :: Encode HlsMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsOutputSelection
newtype HlsOutputSelection = HlsOutputSelection String
derive instance newtypeHlsOutputSelection :: Newtype HlsOutputSelection _
derive instance repGenericHlsOutputSelection :: Generic HlsOutputSelection _
instance showHlsOutputSelection :: Show HlsOutputSelection where
  show = genericShow
instance decodeHlsOutputSelection :: Decode HlsOutputSelection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsOutputSelection :: Encode HlsOutputSelection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsOutputSettings
newtype HlsOutputSettings = HlsOutputSettings 
  { "HlsSettings" :: NullOrUndefined.NullOrUndefined (HlsSettings)
  , "NameModifier" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentModifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHlsOutputSettings :: Newtype HlsOutputSettings _
derive instance repGenericHlsOutputSettings :: Generic HlsOutputSettings _
instance showHlsOutputSettings :: Show HlsOutputSettings where
  show = genericShow
instance decodeHlsOutputSettings :: Decode HlsOutputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsOutputSettings :: Encode HlsOutputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsProgramDateTime
newtype HlsProgramDateTime = HlsProgramDateTime String
derive instance newtypeHlsProgramDateTime :: Newtype HlsProgramDateTime _
derive instance repGenericHlsProgramDateTime :: Generic HlsProgramDateTime _
instance showHlsProgramDateTime :: Show HlsProgramDateTime where
  show = genericShow
instance decodeHlsProgramDateTime :: Decode HlsProgramDateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsProgramDateTime :: Encode HlsProgramDateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsSegmentationMode
newtype HlsSegmentationMode = HlsSegmentationMode String
derive instance newtypeHlsSegmentationMode :: Newtype HlsSegmentationMode _
derive instance repGenericHlsSegmentationMode :: Generic HlsSegmentationMode _
instance showHlsSegmentationMode :: Show HlsSegmentationMode where
  show = genericShow
instance decodeHlsSegmentationMode :: Decode HlsSegmentationMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsSegmentationMode :: Encode HlsSegmentationMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsSettings
newtype HlsSettings = HlsSettings 
  { "AudioOnlyHlsSettings" :: NullOrUndefined.NullOrUndefined (AudioOnlyHlsSettings)
  , "StandardHlsSettings" :: NullOrUndefined.NullOrUndefined (StandardHlsSettings)
  }
derive instance newtypeHlsSettings :: Newtype HlsSettings _
derive instance repGenericHlsSettings :: Generic HlsSettings _
instance showHlsSettings :: Show HlsSettings where
  show = genericShow
instance decodeHlsSettings :: Decode HlsSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsSettings :: Encode HlsSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsStreamInfResolution
newtype HlsStreamInfResolution = HlsStreamInfResolution String
derive instance newtypeHlsStreamInfResolution :: Newtype HlsStreamInfResolution _
derive instance repGenericHlsStreamInfResolution :: Generic HlsStreamInfResolution _
instance showHlsStreamInfResolution :: Show HlsStreamInfResolution where
  show = genericShow
instance decodeHlsStreamInfResolution :: Decode HlsStreamInfResolution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsStreamInfResolution :: Encode HlsStreamInfResolution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsTimedMetadataId3Frame
newtype HlsTimedMetadataId3Frame = HlsTimedMetadataId3Frame String
derive instance newtypeHlsTimedMetadataId3Frame :: Newtype HlsTimedMetadataId3Frame _
derive instance repGenericHlsTimedMetadataId3Frame :: Generic HlsTimedMetadataId3Frame _
instance showHlsTimedMetadataId3Frame :: Show HlsTimedMetadataId3Frame where
  show = genericShow
instance decodeHlsTimedMetadataId3Frame :: Decode HlsTimedMetadataId3Frame where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsTimedMetadataId3Frame :: Encode HlsTimedMetadataId3Frame where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsTsFileMode
newtype HlsTsFileMode = HlsTsFileMode String
derive instance newtypeHlsTsFileMode :: Newtype HlsTsFileMode _
derive instance repGenericHlsTsFileMode :: Generic HlsTsFileMode _
instance showHlsTsFileMode :: Show HlsTsFileMode where
  show = genericShow
instance decodeHlsTsFileMode :: Decode HlsTsFileMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsTsFileMode :: Encode HlsTsFileMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsWebdavHttpTransferMode
newtype HlsWebdavHttpTransferMode = HlsWebdavHttpTransferMode String
derive instance newtypeHlsWebdavHttpTransferMode :: Newtype HlsWebdavHttpTransferMode _
derive instance repGenericHlsWebdavHttpTransferMode :: Generic HlsWebdavHttpTransferMode _
instance showHlsWebdavHttpTransferMode :: Show HlsWebdavHttpTransferMode where
  show = genericShow
instance decodeHlsWebdavHttpTransferMode :: Decode HlsWebdavHttpTransferMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsWebdavHttpTransferMode :: Encode HlsWebdavHttpTransferMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for HlsWebdavSettings
newtype HlsWebdavSettings = HlsWebdavSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined.NullOrUndefined (Int)
  , "HttpTransferMode" :: NullOrUndefined.NullOrUndefined (HlsWebdavHttpTransferMode)
  , "NumRetries" :: NullOrUndefined.NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeHlsWebdavSettings :: Newtype HlsWebdavSettings _
derive instance repGenericHlsWebdavSettings :: Generic HlsWebdavSettings _
instance showHlsWebdavSettings :: Show HlsWebdavSettings where
  show = genericShow
instance decodeHlsWebdavSettings :: Decode HlsWebdavSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsWebdavSettings :: Encode HlsWebdavSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Input
newtype Input = Input 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "AttachedChannels" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfInputDestination)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "Sources" :: NullOrUndefined.NullOrUndefined (ListOfInputSource)
  , "State" :: NullOrUndefined.NullOrUndefined (InputState)
  , "Type" :: NullOrUndefined.NullOrUndefined (InputType)
  }
derive instance newtypeInput :: Newtype Input _
derive instance repGenericInput :: Generic Input _
instance showInput :: Show Input where
  show = genericShow
instance decodeInput :: Decode Input where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInput :: Encode Input where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputAttachment
newtype InputAttachment = InputAttachment 
  { "InputId" :: NullOrUndefined.NullOrUndefined (String)
  , "InputSettings" :: NullOrUndefined.NullOrUndefined (InputSettings)
  }
derive instance newtypeInputAttachment :: Newtype InputAttachment _
derive instance repGenericInputAttachment :: Generic InputAttachment _
instance showInputAttachment :: Show InputAttachment where
  show = genericShow
instance decodeInputAttachment :: Decode InputAttachment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputAttachment :: Encode InputAttachment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputChannelLevel
newtype InputChannelLevel = InputChannelLevel 
  { "Gain" :: NullOrUndefined.NullOrUndefined (Int)
  , "InputChannel" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeInputChannelLevel :: Newtype InputChannelLevel _
derive instance repGenericInputChannelLevel :: Generic InputChannelLevel _
instance showInputChannelLevel :: Show InputChannelLevel where
  show = genericShow
instance decodeInputChannelLevel :: Decode InputChannelLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputChannelLevel :: Encode InputChannelLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | codec in increasing order of complexity
newtype InputCodec = InputCodec String
derive instance newtypeInputCodec :: Newtype InputCodec _
derive instance repGenericInputCodec :: Generic InputCodec _
instance showInputCodec :: Show InputCodec where
  show = genericShow
instance decodeInputCodec :: Decode InputCodec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputCodec :: Encode InputCodec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputDeblockFilter
newtype InputDeblockFilter = InputDeblockFilter String
derive instance newtypeInputDeblockFilter :: Newtype InputDeblockFilter _
derive instance repGenericInputDeblockFilter :: Generic InputDeblockFilter _
instance showInputDeblockFilter :: Show InputDeblockFilter where
  show = genericShow
instance decodeInputDeblockFilter :: Decode InputDeblockFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputDeblockFilter :: Encode InputDeblockFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputDenoiseFilter
newtype InputDenoiseFilter = InputDenoiseFilter String
derive instance newtypeInputDenoiseFilter :: Newtype InputDenoiseFilter _
derive instance repGenericInputDenoiseFilter :: Generic InputDenoiseFilter _
instance showInputDenoiseFilter :: Show InputDenoiseFilter where
  show = genericShow
instance decodeInputDenoiseFilter :: Decode InputDenoiseFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputDenoiseFilter :: Encode InputDenoiseFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The settings for a PUSH type input.
newtype InputDestination = InputDestination 
  { "Ip" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInputDestination :: Newtype InputDestination _
derive instance repGenericInputDestination :: Generic InputDestination _
instance showInputDestination :: Show InputDestination where
  show = genericShow
instance decodeInputDestination :: Decode InputDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputDestination :: Encode InputDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Endpoint settings for a PUSH type input.
newtype InputDestinationRequest = InputDestinationRequest 
  { "StreamName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInputDestinationRequest :: Newtype InputDestinationRequest _
derive instance repGenericInputDestinationRequest :: Generic InputDestinationRequest _
instance showInputDestinationRequest :: Show InputDestinationRequest where
  show = genericShow
instance decodeInputDestinationRequest :: Decode InputDestinationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputDestinationRequest :: Encode InputDestinationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputFilter
newtype InputFilter = InputFilter String
derive instance newtypeInputFilter :: Newtype InputFilter _
derive instance repGenericInputFilter :: Generic InputFilter _
instance showInputFilter :: Show InputFilter where
  show = genericShow
instance decodeInputFilter :: Decode InputFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputFilter :: Encode InputFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputLocation
newtype InputLocation = InputLocation 
  { "PasswordParam" :: NullOrUndefined.NullOrUndefined (String)
  , "Uri" :: NullOrUndefined.NullOrUndefined (String)
  , "Username" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInputLocation :: Newtype InputLocation _
derive instance repGenericInputLocation :: Generic InputLocation _
instance showInputLocation :: Show InputLocation where
  show = genericShow
instance decodeInputLocation :: Decode InputLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLocation :: Encode InputLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputLossActionForHlsOut
newtype InputLossActionForHlsOut = InputLossActionForHlsOut String
derive instance newtypeInputLossActionForHlsOut :: Newtype InputLossActionForHlsOut _
derive instance repGenericInputLossActionForHlsOut :: Generic InputLossActionForHlsOut _
instance showInputLossActionForHlsOut :: Show InputLossActionForHlsOut where
  show = genericShow
instance decodeInputLossActionForHlsOut :: Decode InputLossActionForHlsOut where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLossActionForHlsOut :: Encode InputLossActionForHlsOut where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputLossActionForMsSmoothOut
newtype InputLossActionForMsSmoothOut = InputLossActionForMsSmoothOut String
derive instance newtypeInputLossActionForMsSmoothOut :: Newtype InputLossActionForMsSmoothOut _
derive instance repGenericInputLossActionForMsSmoothOut :: Generic InputLossActionForMsSmoothOut _
instance showInputLossActionForMsSmoothOut :: Show InputLossActionForMsSmoothOut where
  show = genericShow
instance decodeInputLossActionForMsSmoothOut :: Decode InputLossActionForMsSmoothOut where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLossActionForMsSmoothOut :: Encode InputLossActionForMsSmoothOut where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputLossActionForUdpOut
newtype InputLossActionForUdpOut = InputLossActionForUdpOut String
derive instance newtypeInputLossActionForUdpOut :: Newtype InputLossActionForUdpOut _
derive instance repGenericInputLossActionForUdpOut :: Generic InputLossActionForUdpOut _
instance showInputLossActionForUdpOut :: Show InputLossActionForUdpOut where
  show = genericShow
instance decodeInputLossActionForUdpOut :: Decode InputLossActionForUdpOut where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLossActionForUdpOut :: Encode InputLossActionForUdpOut where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputLossBehavior
newtype InputLossBehavior = InputLossBehavior 
  { "BlackFrameMsec" :: NullOrUndefined.NullOrUndefined (Int)
  , "InputLossImageColor" :: NullOrUndefined.NullOrUndefined (String)
  , "InputLossImageSlate" :: NullOrUndefined.NullOrUndefined (InputLocation)
  , "InputLossImageType" :: NullOrUndefined.NullOrUndefined (InputLossImageType)
  , "RepeatFrameMsec" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeInputLossBehavior :: Newtype InputLossBehavior _
derive instance repGenericInputLossBehavior :: Generic InputLossBehavior _
instance showInputLossBehavior :: Show InputLossBehavior where
  show = genericShow
instance decodeInputLossBehavior :: Decode InputLossBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLossBehavior :: Encode InputLossBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputLossImageType
newtype InputLossImageType = InputLossImageType String
derive instance newtypeInputLossImageType :: Newtype InputLossImageType _
derive instance repGenericInputLossImageType :: Generic InputLossImageType _
instance showInputLossImageType :: Show InputLossImageType where
  show = genericShow
instance decodeInputLossImageType :: Decode InputLossImageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLossImageType :: Encode InputLossImageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.
newtype InputMaximumBitrate = InputMaximumBitrate String
derive instance newtypeInputMaximumBitrate :: Newtype InputMaximumBitrate _
derive instance repGenericInputMaximumBitrate :: Generic InputMaximumBitrate _
instance showInputMaximumBitrate :: Show InputMaximumBitrate where
  show = genericShow
instance decodeInputMaximumBitrate :: Decode InputMaximumBitrate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputMaximumBitrate :: Encode InputMaximumBitrate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Input resolution based on lines of vertical resolution in the input; SD is less than 720 lines, HD is 720 to 1080 lines, UHD is greater than 1080 lines
-- | 
newtype InputResolution = InputResolution String
derive instance newtypeInputResolution :: Newtype InputResolution _
derive instance repGenericInputResolution :: Generic InputResolution _
instance showInputResolution :: Show InputResolution where
  show = genericShow
instance decodeInputResolution :: Decode InputResolution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputResolution :: Encode InputResolution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An Input Security Group
newtype InputSecurityGroup = InputSecurityGroup 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "WhitelistRules" :: NullOrUndefined.NullOrUndefined (ListOfInputWhitelistRule)
  }
derive instance newtypeInputSecurityGroup :: Newtype InputSecurityGroup _
derive instance repGenericInputSecurityGroup :: Generic InputSecurityGroup _
instance showInputSecurityGroup :: Show InputSecurityGroup where
  show = genericShow
instance decodeInputSecurityGroup :: Decode InputSecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSecurityGroup :: Encode InputSecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Request of IPv4 CIDR addresses to whitelist in a security group.
newtype InputSecurityGroupWhitelistRequest = InputSecurityGroupWhitelistRequest 
  { "WhitelistRules" :: NullOrUndefined.NullOrUndefined (ListOfInputWhitelistRuleCidr)
  }
derive instance newtypeInputSecurityGroupWhitelistRequest :: Newtype InputSecurityGroupWhitelistRequest _
derive instance repGenericInputSecurityGroupWhitelistRequest :: Generic InputSecurityGroupWhitelistRequest _
instance showInputSecurityGroupWhitelistRequest :: Show InputSecurityGroupWhitelistRequest where
  show = genericShow
instance decodeInputSecurityGroupWhitelistRequest :: Decode InputSecurityGroupWhitelistRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSecurityGroupWhitelistRequest :: Encode InputSecurityGroupWhitelistRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Live Event input parameters. There can be multiple inputs in a single Live Event.
newtype InputSettings = InputSettings 
  { "AudioSelectors" :: NullOrUndefined.NullOrUndefined (ListOfAudioSelector)
  , "CaptionSelectors" :: NullOrUndefined.NullOrUndefined (ListOfCaptionSelector)
  , "DeblockFilter" :: NullOrUndefined.NullOrUndefined (InputDeblockFilter)
  , "DenoiseFilter" :: NullOrUndefined.NullOrUndefined (InputDenoiseFilter)
  , "FilterStrength" :: NullOrUndefined.NullOrUndefined (Int)
  , "InputFilter" :: NullOrUndefined.NullOrUndefined (InputFilter)
  , "NetworkInputSettings" :: NullOrUndefined.NullOrUndefined (NetworkInputSettings)
  , "SourceEndBehavior" :: NullOrUndefined.NullOrUndefined (InputSourceEndBehavior)
  , "VideoSelector" :: NullOrUndefined.NullOrUndefined (VideoSelector)
  }
derive instance newtypeInputSettings :: Newtype InputSettings _
derive instance repGenericInputSettings :: Generic InputSettings _
instance showInputSettings :: Show InputSettings where
  show = genericShow
instance decodeInputSettings :: Decode InputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSettings :: Encode InputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The settings for a PULL type input.
newtype InputSource = InputSource 
  { "PasswordParam" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  , "Username" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInputSource :: Newtype InputSource _
derive instance repGenericInputSource :: Generic InputSource _
instance showInputSource :: Show InputSource where
  show = genericShow
instance decodeInputSource :: Decode InputSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSource :: Encode InputSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputSourceEndBehavior
newtype InputSourceEndBehavior = InputSourceEndBehavior String
derive instance newtypeInputSourceEndBehavior :: Newtype InputSourceEndBehavior _
derive instance repGenericInputSourceEndBehavior :: Generic InputSourceEndBehavior _
instance showInputSourceEndBehavior :: Show InputSourceEndBehavior where
  show = genericShow
instance decodeInputSourceEndBehavior :: Decode InputSourceEndBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSourceEndBehavior :: Encode InputSourceEndBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for for a PULL type input.
newtype InputSourceRequest = InputSourceRequest 
  { "PasswordParam" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  , "Username" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInputSourceRequest :: Newtype InputSourceRequest _
derive instance repGenericInputSourceRequest :: Generic InputSourceRequest _
instance showInputSourceRequest :: Show InputSourceRequest where
  show = genericShow
instance decodeInputSourceRequest :: Decode InputSourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSourceRequest :: Encode InputSourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputSpecification
newtype InputSpecification = InputSpecification 
  { "Codec" :: NullOrUndefined.NullOrUndefined (InputCodec)
  , "MaximumBitrate" :: NullOrUndefined.NullOrUndefined (InputMaximumBitrate)
  , "Resolution" :: NullOrUndefined.NullOrUndefined (InputResolution)
  }
derive instance newtypeInputSpecification :: Newtype InputSpecification _
derive instance repGenericInputSpecification :: Generic InputSpecification _
instance showInputSpecification :: Show InputSpecification where
  show = genericShow
instance decodeInputSpecification :: Decode InputSpecification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputSpecification :: Encode InputSpecification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputState
newtype InputState = InputState String
derive instance newtypeInputState :: Newtype InputState _
derive instance repGenericInputState :: Generic InputState _
instance showInputState :: Show InputState where
  show = genericShow
instance decodeInputState :: Decode InputState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputState :: Encode InputState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InputType
newtype InputType = InputType String
derive instance newtypeInputType :: Newtype InputType _
derive instance repGenericInputType :: Generic InputType _
instance showInputType :: Show InputType where
  show = genericShow
instance decodeInputType :: Decode InputType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputType :: Encode InputType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Whitelist rule
newtype InputWhitelistRule = InputWhitelistRule 
  { "Cidr" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInputWhitelistRule :: Newtype InputWhitelistRule _
derive instance repGenericInputWhitelistRule :: Generic InputWhitelistRule _
instance showInputWhitelistRule :: Show InputWhitelistRule where
  show = genericShow
instance decodeInputWhitelistRule :: Decode InputWhitelistRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputWhitelistRule :: Encode InputWhitelistRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An IPv4 CIDR to whitelist.
newtype InputWhitelistRuleCidr = InputWhitelistRuleCidr 
  { "Cidr" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInputWhitelistRuleCidr :: Newtype InputWhitelistRuleCidr _
derive instance repGenericInputWhitelistRuleCidr :: Generic InputWhitelistRuleCidr _
instance showInputWhitelistRuleCidr :: Show InputWhitelistRuleCidr where
  show = genericShow
instance decodeInputWhitelistRuleCidr :: Decode InputWhitelistRuleCidr where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputWhitelistRuleCidr :: Encode InputWhitelistRuleCidr where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InternalServerErrorException
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


-- | Placeholder documentation for InternalServiceError
newtype InternalServiceError = InternalServiceError 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalServiceError :: Newtype InternalServiceError _
derive instance repGenericInternalServiceError :: Generic InternalServiceError _
instance showInternalServiceError :: Show InternalServiceError where
  show = genericShow
instance decodeInternalServiceError :: Decode InternalServiceError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServiceError :: Encode InternalServiceError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for InvalidRequest
newtype InvalidRequest = InvalidRequest 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidRequest :: Newtype InvalidRequest _
derive instance repGenericInvalidRequest :: Generic InvalidRequest _
instance showInvalidRequest :: Show InvalidRequest where
  show = genericShow
instance decodeInvalidRequest :: Decode InvalidRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequest :: Encode InvalidRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for KeyProviderSettings
newtype KeyProviderSettings = KeyProviderSettings 
  { "StaticKeySettings" :: NullOrUndefined.NullOrUndefined (StaticKeySettings)
  }
derive instance newtypeKeyProviderSettings :: Newtype KeyProviderSettings _
derive instance repGenericKeyProviderSettings :: Generic KeyProviderSettings _
instance showKeyProviderSettings :: Show KeyProviderSettings where
  show = genericShow
instance decodeKeyProviderSettings :: Decode KeyProviderSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyProviderSettings :: Encode KeyProviderSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for LimitExceeded
newtype LimitExceeded = LimitExceeded 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLimitExceeded :: Newtype LimitExceeded _
derive instance repGenericLimitExceeded :: Generic LimitExceeded _
instance showLimitExceeded :: Show LimitExceeded where
  show = genericShow
instance decodeLimitExceeded :: Decode LimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceeded :: Encode LimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListChannelsRequest
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


-- | Placeholder documentation for ListChannelsResponse
newtype ListChannelsResponse = ListChannelsResponse 
  { "Channels" :: NullOrUndefined.NullOrUndefined (ListOfChannelSummary)
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


-- | Placeholder documentation for ListChannelsResultModel
newtype ListChannelsResultModel = ListChannelsResultModel 
  { "Channels" :: NullOrUndefined.NullOrUndefined (ListOfChannelSummary)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListChannelsResultModel :: Newtype ListChannelsResultModel _
derive instance repGenericListChannelsResultModel :: Generic ListChannelsResultModel _
instance showListChannelsResultModel :: Show ListChannelsResultModel where
  show = genericShow
instance decodeListChannelsResultModel :: Decode ListChannelsResultModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListChannelsResultModel :: Encode ListChannelsResultModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListInputSecurityGroupsRequest
newtype ListInputSecurityGroupsRequest = ListInputSecurityGroupsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListInputSecurityGroupsRequest :: Newtype ListInputSecurityGroupsRequest _
derive instance repGenericListInputSecurityGroupsRequest :: Generic ListInputSecurityGroupsRequest _
instance showListInputSecurityGroupsRequest :: Show ListInputSecurityGroupsRequest where
  show = genericShow
instance decodeListInputSecurityGroupsRequest :: Decode ListInputSecurityGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInputSecurityGroupsRequest :: Encode ListInputSecurityGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListInputSecurityGroupsResponse
newtype ListInputSecurityGroupsResponse = ListInputSecurityGroupsResponse 
  { "InputSecurityGroups" :: NullOrUndefined.NullOrUndefined (ListOfInputSecurityGroup)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListInputSecurityGroupsResponse :: Newtype ListInputSecurityGroupsResponse _
derive instance repGenericListInputSecurityGroupsResponse :: Generic ListInputSecurityGroupsResponse _
instance showListInputSecurityGroupsResponse :: Show ListInputSecurityGroupsResponse where
  show = genericShow
instance decodeListInputSecurityGroupsResponse :: Decode ListInputSecurityGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInputSecurityGroupsResponse :: Encode ListInputSecurityGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Result of input security group list request
newtype ListInputSecurityGroupsResultModel = ListInputSecurityGroupsResultModel 
  { "InputSecurityGroups" :: NullOrUndefined.NullOrUndefined (ListOfInputSecurityGroup)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListInputSecurityGroupsResultModel :: Newtype ListInputSecurityGroupsResultModel _
derive instance repGenericListInputSecurityGroupsResultModel :: Generic ListInputSecurityGroupsResultModel _
instance showListInputSecurityGroupsResultModel :: Show ListInputSecurityGroupsResultModel where
  show = genericShow
instance decodeListInputSecurityGroupsResultModel :: Decode ListInputSecurityGroupsResultModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInputSecurityGroupsResultModel :: Encode ListInputSecurityGroupsResultModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListInputsRequest
newtype ListInputsRequest = ListInputsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListInputsRequest :: Newtype ListInputsRequest _
derive instance repGenericListInputsRequest :: Generic ListInputsRequest _
instance showListInputsRequest :: Show ListInputsRequest where
  show = genericShow
instance decodeListInputsRequest :: Decode ListInputsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInputsRequest :: Encode ListInputsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListInputsResponse
newtype ListInputsResponse = ListInputsResponse 
  { "Inputs" :: NullOrUndefined.NullOrUndefined (ListOfInput)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListInputsResponse :: Newtype ListInputsResponse _
derive instance repGenericListInputsResponse :: Generic ListInputsResponse _
instance showListInputsResponse :: Show ListInputsResponse where
  show = genericShow
instance decodeListInputsResponse :: Decode ListInputsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInputsResponse :: Encode ListInputsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListInputsResultModel
newtype ListInputsResultModel = ListInputsResultModel 
  { "Inputs" :: NullOrUndefined.NullOrUndefined (ListOfInput)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListInputsResultModel :: Newtype ListInputsResultModel _
derive instance repGenericListInputsResultModel :: Generic ListInputsResultModel _
instance showListInputsResultModel :: Show ListInputsResultModel where
  show = genericShow
instance decodeListInputsResultModel :: Decode ListInputsResultModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInputsResultModel :: Encode ListInputsResultModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfAudioChannelMapping
newtype ListOfAudioChannelMapping = ListOfAudioChannelMapping (Array AudioChannelMapping)
derive instance newtypeListOfAudioChannelMapping :: Newtype ListOfAudioChannelMapping _
derive instance repGenericListOfAudioChannelMapping :: Generic ListOfAudioChannelMapping _
instance showListOfAudioChannelMapping :: Show ListOfAudioChannelMapping where
  show = genericShow
instance decodeListOfAudioChannelMapping :: Decode ListOfAudioChannelMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfAudioChannelMapping :: Encode ListOfAudioChannelMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfAudioDescription
newtype ListOfAudioDescription = ListOfAudioDescription (Array AudioDescription)
derive instance newtypeListOfAudioDescription :: Newtype ListOfAudioDescription _
derive instance repGenericListOfAudioDescription :: Generic ListOfAudioDescription _
instance showListOfAudioDescription :: Show ListOfAudioDescription where
  show = genericShow
instance decodeListOfAudioDescription :: Decode ListOfAudioDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfAudioDescription :: Encode ListOfAudioDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfAudioSelector
newtype ListOfAudioSelector = ListOfAudioSelector (Array AudioSelector)
derive instance newtypeListOfAudioSelector :: Newtype ListOfAudioSelector _
derive instance repGenericListOfAudioSelector :: Generic ListOfAudioSelector _
instance showListOfAudioSelector :: Show ListOfAudioSelector where
  show = genericShow
instance decodeListOfAudioSelector :: Decode ListOfAudioSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfAudioSelector :: Encode ListOfAudioSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfCaptionDescription
newtype ListOfCaptionDescription = ListOfCaptionDescription (Array CaptionDescription)
derive instance newtypeListOfCaptionDescription :: Newtype ListOfCaptionDescription _
derive instance repGenericListOfCaptionDescription :: Generic ListOfCaptionDescription _
instance showListOfCaptionDescription :: Show ListOfCaptionDescription where
  show = genericShow
instance decodeListOfCaptionDescription :: Decode ListOfCaptionDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfCaptionDescription :: Encode ListOfCaptionDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfCaptionLanguageMapping
newtype ListOfCaptionLanguageMapping = ListOfCaptionLanguageMapping (Array CaptionLanguageMapping)
derive instance newtypeListOfCaptionLanguageMapping :: Newtype ListOfCaptionLanguageMapping _
derive instance repGenericListOfCaptionLanguageMapping :: Generic ListOfCaptionLanguageMapping _
instance showListOfCaptionLanguageMapping :: Show ListOfCaptionLanguageMapping where
  show = genericShow
instance decodeListOfCaptionLanguageMapping :: Decode ListOfCaptionLanguageMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfCaptionLanguageMapping :: Encode ListOfCaptionLanguageMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfCaptionSelector
newtype ListOfCaptionSelector = ListOfCaptionSelector (Array CaptionSelector)
derive instance newtypeListOfCaptionSelector :: Newtype ListOfCaptionSelector _
derive instance repGenericListOfCaptionSelector :: Generic ListOfCaptionSelector _
instance showListOfCaptionSelector :: Show ListOfCaptionSelector where
  show = genericShow
instance decodeListOfCaptionSelector :: Decode ListOfCaptionSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfCaptionSelector :: Encode ListOfCaptionSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfChannelEgressEndpoint
newtype ListOfChannelEgressEndpoint = ListOfChannelEgressEndpoint (Array ChannelEgressEndpoint)
derive instance newtypeListOfChannelEgressEndpoint :: Newtype ListOfChannelEgressEndpoint _
derive instance repGenericListOfChannelEgressEndpoint :: Generic ListOfChannelEgressEndpoint _
instance showListOfChannelEgressEndpoint :: Show ListOfChannelEgressEndpoint where
  show = genericShow
instance decodeListOfChannelEgressEndpoint :: Decode ListOfChannelEgressEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfChannelEgressEndpoint :: Encode ListOfChannelEgressEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfChannelSummary
newtype ListOfChannelSummary = ListOfChannelSummary (Array ChannelSummary)
derive instance newtypeListOfChannelSummary :: Newtype ListOfChannelSummary _
derive instance repGenericListOfChannelSummary :: Generic ListOfChannelSummary _
instance showListOfChannelSummary :: Show ListOfChannelSummary where
  show = genericShow
instance decodeListOfChannelSummary :: Decode ListOfChannelSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfChannelSummary :: Encode ListOfChannelSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfHlsAdMarkers
newtype ListOfHlsAdMarkers = ListOfHlsAdMarkers (Array HlsAdMarkers)
derive instance newtypeListOfHlsAdMarkers :: Newtype ListOfHlsAdMarkers _
derive instance repGenericListOfHlsAdMarkers :: Generic ListOfHlsAdMarkers _
instance showListOfHlsAdMarkers :: Show ListOfHlsAdMarkers where
  show = genericShow
instance decodeListOfHlsAdMarkers :: Decode ListOfHlsAdMarkers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfHlsAdMarkers :: Encode ListOfHlsAdMarkers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInput
newtype ListOfInput = ListOfInput (Array Input)
derive instance newtypeListOfInput :: Newtype ListOfInput _
derive instance repGenericListOfInput :: Generic ListOfInput _
instance showListOfInput :: Show ListOfInput where
  show = genericShow
instance decodeListOfInput :: Decode ListOfInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInput :: Encode ListOfInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInputAttachment
newtype ListOfInputAttachment = ListOfInputAttachment (Array InputAttachment)
derive instance newtypeListOfInputAttachment :: Newtype ListOfInputAttachment _
derive instance repGenericListOfInputAttachment :: Generic ListOfInputAttachment _
instance showListOfInputAttachment :: Show ListOfInputAttachment where
  show = genericShow
instance decodeListOfInputAttachment :: Decode ListOfInputAttachment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputAttachment :: Encode ListOfInputAttachment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInputChannelLevel
newtype ListOfInputChannelLevel = ListOfInputChannelLevel (Array InputChannelLevel)
derive instance newtypeListOfInputChannelLevel :: Newtype ListOfInputChannelLevel _
derive instance repGenericListOfInputChannelLevel :: Generic ListOfInputChannelLevel _
instance showListOfInputChannelLevel :: Show ListOfInputChannelLevel where
  show = genericShow
instance decodeListOfInputChannelLevel :: Decode ListOfInputChannelLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputChannelLevel :: Encode ListOfInputChannelLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInputDestination
newtype ListOfInputDestination = ListOfInputDestination (Array InputDestination)
derive instance newtypeListOfInputDestination :: Newtype ListOfInputDestination _
derive instance repGenericListOfInputDestination :: Generic ListOfInputDestination _
instance showListOfInputDestination :: Show ListOfInputDestination where
  show = genericShow
instance decodeListOfInputDestination :: Decode ListOfInputDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputDestination :: Encode ListOfInputDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInputDestinationRequest
newtype ListOfInputDestinationRequest = ListOfInputDestinationRequest (Array InputDestinationRequest)
derive instance newtypeListOfInputDestinationRequest :: Newtype ListOfInputDestinationRequest _
derive instance repGenericListOfInputDestinationRequest :: Generic ListOfInputDestinationRequest _
instance showListOfInputDestinationRequest :: Show ListOfInputDestinationRequest where
  show = genericShow
instance decodeListOfInputDestinationRequest :: Decode ListOfInputDestinationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputDestinationRequest :: Encode ListOfInputDestinationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInputSecurityGroup
newtype ListOfInputSecurityGroup = ListOfInputSecurityGroup (Array InputSecurityGroup)
derive instance newtypeListOfInputSecurityGroup :: Newtype ListOfInputSecurityGroup _
derive instance repGenericListOfInputSecurityGroup :: Generic ListOfInputSecurityGroup _
instance showListOfInputSecurityGroup :: Show ListOfInputSecurityGroup where
  show = genericShow
instance decodeListOfInputSecurityGroup :: Decode ListOfInputSecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputSecurityGroup :: Encode ListOfInputSecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInputSource
newtype ListOfInputSource = ListOfInputSource (Array InputSource)
derive instance newtypeListOfInputSource :: Newtype ListOfInputSource _
derive instance repGenericListOfInputSource :: Generic ListOfInputSource _
instance showListOfInputSource :: Show ListOfInputSource where
  show = genericShow
instance decodeListOfInputSource :: Decode ListOfInputSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputSource :: Encode ListOfInputSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInputSourceRequest
newtype ListOfInputSourceRequest = ListOfInputSourceRequest (Array InputSourceRequest)
derive instance newtypeListOfInputSourceRequest :: Newtype ListOfInputSourceRequest _
derive instance repGenericListOfInputSourceRequest :: Generic ListOfInputSourceRequest _
instance showListOfInputSourceRequest :: Show ListOfInputSourceRequest where
  show = genericShow
instance decodeListOfInputSourceRequest :: Decode ListOfInputSourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputSourceRequest :: Encode ListOfInputSourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInputWhitelistRule
newtype ListOfInputWhitelistRule = ListOfInputWhitelistRule (Array InputWhitelistRule)
derive instance newtypeListOfInputWhitelistRule :: Newtype ListOfInputWhitelistRule _
derive instance repGenericListOfInputWhitelistRule :: Generic ListOfInputWhitelistRule _
instance showListOfInputWhitelistRule :: Show ListOfInputWhitelistRule where
  show = genericShow
instance decodeListOfInputWhitelistRule :: Decode ListOfInputWhitelistRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputWhitelistRule :: Encode ListOfInputWhitelistRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfInputWhitelistRuleCidr
newtype ListOfInputWhitelistRuleCidr = ListOfInputWhitelistRuleCidr (Array InputWhitelistRuleCidr)
derive instance newtypeListOfInputWhitelistRuleCidr :: Newtype ListOfInputWhitelistRuleCidr _
derive instance repGenericListOfInputWhitelistRuleCidr :: Generic ListOfInputWhitelistRuleCidr _
instance showListOfInputWhitelistRuleCidr :: Show ListOfInputWhitelistRuleCidr where
  show = genericShow
instance decodeListOfInputWhitelistRuleCidr :: Decode ListOfInputWhitelistRuleCidr where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputWhitelistRuleCidr :: Encode ListOfInputWhitelistRuleCidr where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfOutput
newtype ListOfOutput = ListOfOutput (Array Output)
derive instance newtypeListOfOutput :: Newtype ListOfOutput _
derive instance repGenericListOfOutput :: Generic ListOfOutput _
instance showListOfOutput :: Show ListOfOutput where
  show = genericShow
instance decodeListOfOutput :: Decode ListOfOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOutput :: Encode ListOfOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfOutputDestination
newtype ListOfOutputDestination = ListOfOutputDestination (Array OutputDestination)
derive instance newtypeListOfOutputDestination :: Newtype ListOfOutputDestination _
derive instance repGenericListOfOutputDestination :: Generic ListOfOutputDestination _
instance showListOfOutputDestination :: Show ListOfOutputDestination where
  show = genericShow
instance decodeListOfOutputDestination :: Decode ListOfOutputDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOutputDestination :: Encode ListOfOutputDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfOutputDestinationSettings
newtype ListOfOutputDestinationSettings = ListOfOutputDestinationSettings (Array OutputDestinationSettings)
derive instance newtypeListOfOutputDestinationSettings :: Newtype ListOfOutputDestinationSettings _
derive instance repGenericListOfOutputDestinationSettings :: Generic ListOfOutputDestinationSettings _
instance showListOfOutputDestinationSettings :: Show ListOfOutputDestinationSettings where
  show = genericShow
instance decodeListOfOutputDestinationSettings :: Decode ListOfOutputDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOutputDestinationSettings :: Encode ListOfOutputDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfOutputGroup
newtype ListOfOutputGroup = ListOfOutputGroup (Array OutputGroup)
derive instance newtypeListOfOutputGroup :: Newtype ListOfOutputGroup _
derive instance repGenericListOfOutputGroup :: Generic ListOfOutputGroup _
instance showListOfOutputGroup :: Show ListOfOutputGroup where
  show = genericShow
instance decodeListOfOutputGroup :: Decode ListOfOutputGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOutputGroup :: Encode ListOfOutputGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfValidationError
newtype ListOfValidationError = ListOfValidationError (Array ValidationError)
derive instance newtypeListOfValidationError :: Newtype ListOfValidationError _
derive instance repGenericListOfValidationError :: Generic ListOfValidationError _
instance showListOfValidationError :: Show ListOfValidationError where
  show = genericShow
instance decodeListOfValidationError :: Decode ListOfValidationError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfValidationError :: Encode ListOfValidationError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOfVideoDescription
newtype ListOfVideoDescription = ListOfVideoDescription (Array VideoDescription)
derive instance newtypeListOfVideoDescription :: Newtype ListOfVideoDescription _
derive instance repGenericListOfVideoDescription :: Generic ListOfVideoDescription _
instance showListOfVideoDescription :: Show ListOfVideoDescription where
  show = genericShow
instance decodeListOfVideoDescription :: Decode ListOfVideoDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfVideoDescription :: Encode ListOfVideoDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ListOf__string
newtype ListOf__string = ListOf__string (Array String)
derive instance newtypeListOf__string :: Newtype ListOf__string _
derive instance repGenericListOf__string :: Generic ListOf__string _
instance showListOf__string :: Show ListOf__string where
  show = genericShow
instance decodeListOf__string :: Decode ListOf__string where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOf__string :: Encode ListOf__string where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsAbsentInputAudioBehavior
newtype M2tsAbsentInputAudioBehavior = M2tsAbsentInputAudioBehavior String
derive instance newtypeM2tsAbsentInputAudioBehavior :: Newtype M2tsAbsentInputAudioBehavior _
derive instance repGenericM2tsAbsentInputAudioBehavior :: Generic M2tsAbsentInputAudioBehavior _
instance showM2tsAbsentInputAudioBehavior :: Show M2tsAbsentInputAudioBehavior where
  show = genericShow
instance decodeM2tsAbsentInputAudioBehavior :: Decode M2tsAbsentInputAudioBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsAbsentInputAudioBehavior :: Encode M2tsAbsentInputAudioBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsArib
newtype M2tsArib = M2tsArib String
derive instance newtypeM2tsArib :: Newtype M2tsArib _
derive instance repGenericM2tsArib :: Generic M2tsArib _
instance showM2tsArib :: Show M2tsArib where
  show = genericShow
instance decodeM2tsArib :: Decode M2tsArib where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsArib :: Encode M2tsArib where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsAribCaptionsPidControl
newtype M2tsAribCaptionsPidControl = M2tsAribCaptionsPidControl String
derive instance newtypeM2tsAribCaptionsPidControl :: Newtype M2tsAribCaptionsPidControl _
derive instance repGenericM2tsAribCaptionsPidControl :: Generic M2tsAribCaptionsPidControl _
instance showM2tsAribCaptionsPidControl :: Show M2tsAribCaptionsPidControl where
  show = genericShow
instance decodeM2tsAribCaptionsPidControl :: Decode M2tsAribCaptionsPidControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsAribCaptionsPidControl :: Encode M2tsAribCaptionsPidControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsAudioBufferModel
newtype M2tsAudioBufferModel = M2tsAudioBufferModel String
derive instance newtypeM2tsAudioBufferModel :: Newtype M2tsAudioBufferModel _
derive instance repGenericM2tsAudioBufferModel :: Generic M2tsAudioBufferModel _
instance showM2tsAudioBufferModel :: Show M2tsAudioBufferModel where
  show = genericShow
instance decodeM2tsAudioBufferModel :: Decode M2tsAudioBufferModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsAudioBufferModel :: Encode M2tsAudioBufferModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsAudioInterval
newtype M2tsAudioInterval = M2tsAudioInterval String
derive instance newtypeM2tsAudioInterval :: Newtype M2tsAudioInterval _
derive instance repGenericM2tsAudioInterval :: Generic M2tsAudioInterval _
instance showM2tsAudioInterval :: Show M2tsAudioInterval where
  show = genericShow
instance decodeM2tsAudioInterval :: Decode M2tsAudioInterval where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsAudioInterval :: Encode M2tsAudioInterval where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsAudioStreamType
newtype M2tsAudioStreamType = M2tsAudioStreamType String
derive instance newtypeM2tsAudioStreamType :: Newtype M2tsAudioStreamType _
derive instance repGenericM2tsAudioStreamType :: Generic M2tsAudioStreamType _
instance showM2tsAudioStreamType :: Show M2tsAudioStreamType where
  show = genericShow
instance decodeM2tsAudioStreamType :: Decode M2tsAudioStreamType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsAudioStreamType :: Encode M2tsAudioStreamType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsBufferModel
newtype M2tsBufferModel = M2tsBufferModel String
derive instance newtypeM2tsBufferModel :: Newtype M2tsBufferModel _
derive instance repGenericM2tsBufferModel :: Generic M2tsBufferModel _
instance showM2tsBufferModel :: Show M2tsBufferModel where
  show = genericShow
instance decodeM2tsBufferModel :: Decode M2tsBufferModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsBufferModel :: Encode M2tsBufferModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsCcDescriptor
newtype M2tsCcDescriptor = M2tsCcDescriptor String
derive instance newtypeM2tsCcDescriptor :: Newtype M2tsCcDescriptor _
derive instance repGenericM2tsCcDescriptor :: Generic M2tsCcDescriptor _
instance showM2tsCcDescriptor :: Show M2tsCcDescriptor where
  show = genericShow
instance decodeM2tsCcDescriptor :: Decode M2tsCcDescriptor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsCcDescriptor :: Encode M2tsCcDescriptor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsEbifControl
newtype M2tsEbifControl = M2tsEbifControl String
derive instance newtypeM2tsEbifControl :: Newtype M2tsEbifControl _
derive instance repGenericM2tsEbifControl :: Generic M2tsEbifControl _
instance showM2tsEbifControl :: Show M2tsEbifControl where
  show = genericShow
instance decodeM2tsEbifControl :: Decode M2tsEbifControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsEbifControl :: Encode M2tsEbifControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsEbpPlacement
newtype M2tsEbpPlacement = M2tsEbpPlacement String
derive instance newtypeM2tsEbpPlacement :: Newtype M2tsEbpPlacement _
derive instance repGenericM2tsEbpPlacement :: Generic M2tsEbpPlacement _
instance showM2tsEbpPlacement :: Show M2tsEbpPlacement where
  show = genericShow
instance decodeM2tsEbpPlacement :: Decode M2tsEbpPlacement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsEbpPlacement :: Encode M2tsEbpPlacement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsEsRateInPes
newtype M2tsEsRateInPes = M2tsEsRateInPes String
derive instance newtypeM2tsEsRateInPes :: Newtype M2tsEsRateInPes _
derive instance repGenericM2tsEsRateInPes :: Generic M2tsEsRateInPes _
instance showM2tsEsRateInPes :: Show M2tsEsRateInPes where
  show = genericShow
instance decodeM2tsEsRateInPes :: Decode M2tsEsRateInPes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsEsRateInPes :: Encode M2tsEsRateInPes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsKlv
newtype M2tsKlv = M2tsKlv String
derive instance newtypeM2tsKlv :: Newtype M2tsKlv _
derive instance repGenericM2tsKlv :: Generic M2tsKlv _
instance showM2tsKlv :: Show M2tsKlv where
  show = genericShow
instance decodeM2tsKlv :: Decode M2tsKlv where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsKlv :: Encode M2tsKlv where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsPcrControl
newtype M2tsPcrControl = M2tsPcrControl String
derive instance newtypeM2tsPcrControl :: Newtype M2tsPcrControl _
derive instance repGenericM2tsPcrControl :: Generic M2tsPcrControl _
instance showM2tsPcrControl :: Show M2tsPcrControl where
  show = genericShow
instance decodeM2tsPcrControl :: Decode M2tsPcrControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsPcrControl :: Encode M2tsPcrControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsRateMode
newtype M2tsRateMode = M2tsRateMode String
derive instance newtypeM2tsRateMode :: Newtype M2tsRateMode _
derive instance repGenericM2tsRateMode :: Generic M2tsRateMode _
instance showM2tsRateMode :: Show M2tsRateMode where
  show = genericShow
instance decodeM2tsRateMode :: Decode M2tsRateMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsRateMode :: Encode M2tsRateMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsScte35Control
newtype M2tsScte35Control = M2tsScte35Control String
derive instance newtypeM2tsScte35Control :: Newtype M2tsScte35Control _
derive instance repGenericM2tsScte35Control :: Generic M2tsScte35Control _
instance showM2tsScte35Control :: Show M2tsScte35Control where
  show = genericShow
instance decodeM2tsScte35Control :: Decode M2tsScte35Control where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsScte35Control :: Encode M2tsScte35Control where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsSegmentationMarkers
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers String
derive instance newtypeM2tsSegmentationMarkers :: Newtype M2tsSegmentationMarkers _
derive instance repGenericM2tsSegmentationMarkers :: Generic M2tsSegmentationMarkers _
instance showM2tsSegmentationMarkers :: Show M2tsSegmentationMarkers where
  show = genericShow
instance decodeM2tsSegmentationMarkers :: Decode M2tsSegmentationMarkers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsSegmentationMarkers :: Encode M2tsSegmentationMarkers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsSegmentationStyle
newtype M2tsSegmentationStyle = M2tsSegmentationStyle String
derive instance newtypeM2tsSegmentationStyle :: Newtype M2tsSegmentationStyle _
derive instance repGenericM2tsSegmentationStyle :: Generic M2tsSegmentationStyle _
instance showM2tsSegmentationStyle :: Show M2tsSegmentationStyle where
  show = genericShow
instance decodeM2tsSegmentationStyle :: Decode M2tsSegmentationStyle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsSegmentationStyle :: Encode M2tsSegmentationStyle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsSettings
newtype M2tsSettings = M2tsSettings 
  { "AbsentInputAudioBehavior" :: NullOrUndefined.NullOrUndefined (M2tsAbsentInputAudioBehavior)
  , "Arib" :: NullOrUndefined.NullOrUndefined (M2tsArib)
  , "AribCaptionsPid" :: NullOrUndefined.NullOrUndefined (String)
  , "AribCaptionsPidControl" :: NullOrUndefined.NullOrUndefined (M2tsAribCaptionsPidControl)
  , "AudioBufferModel" :: NullOrUndefined.NullOrUndefined (M2tsAudioBufferModel)
  , "AudioFramesPerPes" :: NullOrUndefined.NullOrUndefined (Int)
  , "AudioPids" :: NullOrUndefined.NullOrUndefined (String)
  , "AudioStreamType" :: NullOrUndefined.NullOrUndefined (M2tsAudioStreamType)
  , "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "BufferModel" :: NullOrUndefined.NullOrUndefined (M2tsBufferModel)
  , "CcDescriptor" :: NullOrUndefined.NullOrUndefined (M2tsCcDescriptor)
  , "DvbNitSettings" :: NullOrUndefined.NullOrUndefined (DvbNitSettings)
  , "DvbSdtSettings" :: NullOrUndefined.NullOrUndefined (DvbSdtSettings)
  , "DvbSubPids" :: NullOrUndefined.NullOrUndefined (String)
  , "DvbTdtSettings" :: NullOrUndefined.NullOrUndefined (DvbTdtSettings)
  , "DvbTeletextPid" :: NullOrUndefined.NullOrUndefined (String)
  , "Ebif" :: NullOrUndefined.NullOrUndefined (M2tsEbifControl)
  , "EbpAudioInterval" :: NullOrUndefined.NullOrUndefined (M2tsAudioInterval)
  , "EbpLookaheadMs" :: NullOrUndefined.NullOrUndefined (Int)
  , "EbpPlacement" :: NullOrUndefined.NullOrUndefined (M2tsEbpPlacement)
  , "EcmPid" :: NullOrUndefined.NullOrUndefined (String)
  , "EsRateInPes" :: NullOrUndefined.NullOrUndefined (M2tsEsRateInPes)
  , "EtvPlatformPid" :: NullOrUndefined.NullOrUndefined (String)
  , "EtvSignalPid" :: NullOrUndefined.NullOrUndefined (String)
  , "FragmentTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "Klv" :: NullOrUndefined.NullOrUndefined (M2tsKlv)
  , "KlvDataPids" :: NullOrUndefined.NullOrUndefined (String)
  , "NullPacketBitrate" :: NullOrUndefined.NullOrUndefined (Number)
  , "PatInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "PcrControl" :: NullOrUndefined.NullOrUndefined (M2tsPcrControl)
  , "PcrPeriod" :: NullOrUndefined.NullOrUndefined (Int)
  , "PcrPid" :: NullOrUndefined.NullOrUndefined (String)
  , "PmtInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "PmtPid" :: NullOrUndefined.NullOrUndefined (String)
  , "ProgramNum" :: NullOrUndefined.NullOrUndefined (Int)
  , "RateMode" :: NullOrUndefined.NullOrUndefined (M2tsRateMode)
  , "Scte27Pids" :: NullOrUndefined.NullOrUndefined (String)
  , "Scte35Control" :: NullOrUndefined.NullOrUndefined (M2tsScte35Control)
  , "Scte35Pid" :: NullOrUndefined.NullOrUndefined (String)
  , "SegmentationMarkers" :: NullOrUndefined.NullOrUndefined (M2tsSegmentationMarkers)
  , "SegmentationStyle" :: NullOrUndefined.NullOrUndefined (M2tsSegmentationStyle)
  , "SegmentationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "TimedMetadataBehavior" :: NullOrUndefined.NullOrUndefined (M2tsTimedMetadataBehavior)
  , "TimedMetadataPid" :: NullOrUndefined.NullOrUndefined (String)
  , "TransportStreamId" :: NullOrUndefined.NullOrUndefined (Int)
  , "VideoPid" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeM2tsSettings :: Newtype M2tsSettings _
derive instance repGenericM2tsSettings :: Generic M2tsSettings _
instance showM2tsSettings :: Show M2tsSettings where
  show = genericShow
instance decodeM2tsSettings :: Decode M2tsSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsSettings :: Encode M2tsSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M2tsTimedMetadataBehavior
newtype M2tsTimedMetadataBehavior = M2tsTimedMetadataBehavior String
derive instance newtypeM2tsTimedMetadataBehavior :: Newtype M2tsTimedMetadataBehavior _
derive instance repGenericM2tsTimedMetadataBehavior :: Generic M2tsTimedMetadataBehavior _
instance showM2tsTimedMetadataBehavior :: Show M2tsTimedMetadataBehavior where
  show = genericShow
instance decodeM2tsTimedMetadataBehavior :: Decode M2tsTimedMetadataBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsTimedMetadataBehavior :: Encode M2tsTimedMetadataBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M3u8PcrControl
newtype M3u8PcrControl = M3u8PcrControl String
derive instance newtypeM3u8PcrControl :: Newtype M3u8PcrControl _
derive instance repGenericM3u8PcrControl :: Generic M3u8PcrControl _
instance showM3u8PcrControl :: Show M3u8PcrControl where
  show = genericShow
instance decodeM3u8PcrControl :: Decode M3u8PcrControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM3u8PcrControl :: Encode M3u8PcrControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M3u8Scte35Behavior
newtype M3u8Scte35Behavior = M3u8Scte35Behavior String
derive instance newtypeM3u8Scte35Behavior :: Newtype M3u8Scte35Behavior _
derive instance repGenericM3u8Scte35Behavior :: Generic M3u8Scte35Behavior _
instance showM3u8Scte35Behavior :: Show M3u8Scte35Behavior where
  show = genericShow
instance decodeM3u8Scte35Behavior :: Decode M3u8Scte35Behavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM3u8Scte35Behavior :: Encode M3u8Scte35Behavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings information for the .m3u8 container
newtype M3u8Settings = M3u8Settings 
  { "AudioFramesPerPes" :: NullOrUndefined.NullOrUndefined (Int)
  , "AudioPids" :: NullOrUndefined.NullOrUndefined (String)
  , "EcmPid" :: NullOrUndefined.NullOrUndefined (String)
  , "PatInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "PcrControl" :: NullOrUndefined.NullOrUndefined (M3u8PcrControl)
  , "PcrPeriod" :: NullOrUndefined.NullOrUndefined (Int)
  , "PcrPid" :: NullOrUndefined.NullOrUndefined (String)
  , "PmtInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "PmtPid" :: NullOrUndefined.NullOrUndefined (String)
  , "ProgramNum" :: NullOrUndefined.NullOrUndefined (Int)
  , "Scte35Behavior" :: NullOrUndefined.NullOrUndefined (M3u8Scte35Behavior)
  , "Scte35Pid" :: NullOrUndefined.NullOrUndefined (String)
  , "TimedMetadataBehavior" :: NullOrUndefined.NullOrUndefined (M3u8TimedMetadataBehavior)
  , "TransportStreamId" :: NullOrUndefined.NullOrUndefined (Int)
  , "VideoPid" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeM3u8Settings :: Newtype M3u8Settings _
derive instance repGenericM3u8Settings :: Generic M3u8Settings _
instance showM3u8Settings :: Show M3u8Settings where
  show = genericShow
instance decodeM3u8Settings :: Decode M3u8Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM3u8Settings :: Encode M3u8Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for M3u8TimedMetadataBehavior
newtype M3u8TimedMetadataBehavior = M3u8TimedMetadataBehavior String
derive instance newtypeM3u8TimedMetadataBehavior :: Newtype M3u8TimedMetadataBehavior _
derive instance repGenericM3u8TimedMetadataBehavior :: Generic M3u8TimedMetadataBehavior _
instance showM3u8TimedMetadataBehavior :: Show M3u8TimedMetadataBehavior where
  show = genericShow
instance decodeM3u8TimedMetadataBehavior :: Decode M3u8TimedMetadataBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM3u8TimedMetadataBehavior :: Encode M3u8TimedMetadataBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for MaxResults
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Mp2CodingMode
newtype Mp2CodingMode = Mp2CodingMode String
derive instance newtypeMp2CodingMode :: Newtype Mp2CodingMode _
derive instance repGenericMp2CodingMode :: Generic Mp2CodingMode _
instance showMp2CodingMode :: Show Mp2CodingMode where
  show = genericShow
instance decodeMp2CodingMode :: Decode Mp2CodingMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMp2CodingMode :: Encode Mp2CodingMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Mp2Settings
newtype Mp2Settings = Mp2Settings 
  { "Bitrate" :: NullOrUndefined.NullOrUndefined (Number)
  , "CodingMode" :: NullOrUndefined.NullOrUndefined (Mp2CodingMode)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeMp2Settings :: Newtype Mp2Settings _
derive instance repGenericMp2Settings :: Generic Mp2Settings _
instance showMp2Settings :: Show Mp2Settings where
  show = genericShow
instance decodeMp2Settings :: Decode Mp2Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMp2Settings :: Encode Mp2Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for MsSmoothGroupSettings
newtype MsSmoothGroupSettings = MsSmoothGroupSettings 
  { "AcquisitionPointId" :: NullOrUndefined.NullOrUndefined (String)
  , "AudioOnlyTimecodeControl" :: NullOrUndefined.NullOrUndefined (SmoothGroupAudioOnlyTimecodeControl)
  , "CertificateMode" :: NullOrUndefined.NullOrUndefined (SmoothGroupCertificateMode)
  , "ConnectionRetryInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "Destination" :: NullOrUndefined.NullOrUndefined (OutputLocationRef)
  , "EventId" :: NullOrUndefined.NullOrUndefined (String)
  , "EventIdMode" :: NullOrUndefined.NullOrUndefined (SmoothGroupEventIdMode)
  , "EventStopBehavior" :: NullOrUndefined.NullOrUndefined (SmoothGroupEventStopBehavior)
  , "FilecacheDuration" :: NullOrUndefined.NullOrUndefined (Int)
  , "FragmentLength" :: NullOrUndefined.NullOrUndefined (Int)
  , "InputLossAction" :: NullOrUndefined.NullOrUndefined (InputLossActionForMsSmoothOut)
  , "NumRetries" :: NullOrUndefined.NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined.NullOrUndefined (Int)
  , "SegmentationMode" :: NullOrUndefined.NullOrUndefined (SmoothGroupSegmentationMode)
  , "SendDelayMs" :: NullOrUndefined.NullOrUndefined (Int)
  , "SparseTrackType" :: NullOrUndefined.NullOrUndefined (SmoothGroupSparseTrackType)
  , "StreamManifestBehavior" :: NullOrUndefined.NullOrUndefined (SmoothGroupStreamManifestBehavior)
  , "TimestampOffset" :: NullOrUndefined.NullOrUndefined (String)
  , "TimestampOffsetMode" :: NullOrUndefined.NullOrUndefined (SmoothGroupTimestampOffsetMode)
  }
derive instance newtypeMsSmoothGroupSettings :: Newtype MsSmoothGroupSettings _
derive instance repGenericMsSmoothGroupSettings :: Generic MsSmoothGroupSettings _
instance showMsSmoothGroupSettings :: Show MsSmoothGroupSettings where
  show = genericShow
instance decodeMsSmoothGroupSettings :: Decode MsSmoothGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMsSmoothGroupSettings :: Encode MsSmoothGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for MsSmoothOutputSettings
newtype MsSmoothOutputSettings = MsSmoothOutputSettings 
  { "NameModifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMsSmoothOutputSettings :: Newtype MsSmoothOutputSettings _
derive instance repGenericMsSmoothOutputSettings :: Generic MsSmoothOutputSettings _
instance showMsSmoothOutputSettings :: Show MsSmoothOutputSettings where
  show = genericShow
instance decodeMsSmoothOutputSettings :: Decode MsSmoothOutputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMsSmoothOutputSettings :: Encode MsSmoothOutputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for NetworkInputServerValidation
newtype NetworkInputServerValidation = NetworkInputServerValidation String
derive instance newtypeNetworkInputServerValidation :: Newtype NetworkInputServerValidation _
derive instance repGenericNetworkInputServerValidation :: Generic NetworkInputServerValidation _
instance showNetworkInputServerValidation :: Show NetworkInputServerValidation where
  show = genericShow
instance decodeNetworkInputServerValidation :: Decode NetworkInputServerValidation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInputServerValidation :: Encode NetworkInputServerValidation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Network source to transcode. Must be accessible to the Elemental Live node that is running the live event through a network connection.
newtype NetworkInputSettings = NetworkInputSettings 
  { "HlsInputSettings" :: NullOrUndefined.NullOrUndefined (HlsInputSettings)
  , "ServerValidation" :: NullOrUndefined.NullOrUndefined (NetworkInputServerValidation)
  }
derive instance newtypeNetworkInputSettings :: Newtype NetworkInputSettings _
derive instance repGenericNetworkInputSettings :: Generic NetworkInputSettings _
instance showNetworkInputSettings :: Show NetworkInputSettings where
  show = genericShow
instance decodeNetworkInputSettings :: Decode NetworkInputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInputSettings :: Encode NetworkInputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for NotFoundException
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


-- | Output settings. There can be multiple outputs within a group.
newtype Output = Output 
  { "AudioDescriptionNames" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "CaptionDescriptionNames" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "OutputName" :: NullOrUndefined.NullOrUndefined (String)
  , "OutputSettings" :: NullOrUndefined.NullOrUndefined (OutputSettings)
  , "VideoDescriptionName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOutput :: Newtype Output _
derive instance repGenericOutput :: Generic Output _
instance showOutput :: Show Output where
  show = genericShow
instance decodeOutput :: Decode Output where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutput :: Encode Output where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for OutputDestination
newtype OutputDestination = OutputDestination 
  { "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Settings" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestinationSettings)
  }
derive instance newtypeOutputDestination :: Newtype OutputDestination _
derive instance repGenericOutputDestination :: Generic OutputDestination _
instance showOutputDestination :: Show OutputDestination where
  show = genericShow
instance decodeOutputDestination :: Decode OutputDestination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputDestination :: Encode OutputDestination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for OutputDestinationSettings
newtype OutputDestinationSettings = OutputDestinationSettings 
  { "PasswordParam" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  , "Username" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOutputDestinationSettings :: Newtype OutputDestinationSettings _
derive instance repGenericOutputDestinationSettings :: Generic OutputDestinationSettings _
instance showOutputDestinationSettings :: Show OutputDestinationSettings where
  show = genericShow
instance decodeOutputDestinationSettings :: Decode OutputDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputDestinationSettings :: Encode OutputDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
newtype OutputGroup = OutputGroup 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "OutputGroupSettings" :: NullOrUndefined.NullOrUndefined (OutputGroupSettings)
  , "Outputs" :: NullOrUndefined.NullOrUndefined (ListOfOutput)
  }
derive instance newtypeOutputGroup :: Newtype OutputGroup _
derive instance repGenericOutputGroup :: Generic OutputGroup _
instance showOutputGroup :: Show OutputGroup where
  show = genericShow
instance decodeOutputGroup :: Decode OutputGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputGroup :: Encode OutputGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for OutputGroupSettings
newtype OutputGroupSettings = OutputGroupSettings 
  { "ArchiveGroupSettings" :: NullOrUndefined.NullOrUndefined (ArchiveGroupSettings)
  , "HlsGroupSettings" :: NullOrUndefined.NullOrUndefined (HlsGroupSettings)
  , "MsSmoothGroupSettings" :: NullOrUndefined.NullOrUndefined (MsSmoothGroupSettings)
  , "UdpGroupSettings" :: NullOrUndefined.NullOrUndefined (UdpGroupSettings)
  }
derive instance newtypeOutputGroupSettings :: Newtype OutputGroupSettings _
derive instance repGenericOutputGroupSettings :: Generic OutputGroupSettings _
instance showOutputGroupSettings :: Show OutputGroupSettings where
  show = genericShow
instance decodeOutputGroupSettings :: Decode OutputGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputGroupSettings :: Encode OutputGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Reference to an OutputDestination ID defined in the channel
newtype OutputLocationRef = OutputLocationRef 
  { "DestinationRefId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOutputLocationRef :: Newtype OutputLocationRef _
derive instance repGenericOutputLocationRef :: Generic OutputLocationRef _
instance showOutputLocationRef :: Show OutputLocationRef where
  show = genericShow
instance decodeOutputLocationRef :: Decode OutputLocationRef where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputLocationRef :: Encode OutputLocationRef where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for OutputSettings
newtype OutputSettings = OutputSettings 
  { "ArchiveOutputSettings" :: NullOrUndefined.NullOrUndefined (ArchiveOutputSettings)
  , "HlsOutputSettings" :: NullOrUndefined.NullOrUndefined (HlsOutputSettings)
  , "MsSmoothOutputSettings" :: NullOrUndefined.NullOrUndefined (MsSmoothOutputSettings)
  , "UdpOutputSettings" :: NullOrUndefined.NullOrUndefined (UdpOutputSettings)
  }
derive instance newtypeOutputSettings :: Newtype OutputSettings _
derive instance repGenericOutputSettings :: Generic OutputSettings _
instance showOutputSettings :: Show OutputSettings where
  show = genericShow
instance decodeOutputSettings :: Decode OutputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputSettings :: Encode OutputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for PassThroughSettings
newtype PassThroughSettings = PassThroughSettings Types.NoArguments
derive instance newtypePassThroughSettings :: Newtype PassThroughSettings _
derive instance repGenericPassThroughSettings :: Generic PassThroughSettings _
instance showPassThroughSettings :: Show PassThroughSettings where
  show = genericShow
instance decodePassThroughSettings :: Decode PassThroughSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePassThroughSettings :: Encode PassThroughSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for RemixSettings
newtype RemixSettings = RemixSettings 
  { "ChannelMappings" :: NullOrUndefined.NullOrUndefined (ListOfAudioChannelMapping)
  , "ChannelsIn" :: NullOrUndefined.NullOrUndefined (Int)
  , "ChannelsOut" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeRemixSettings :: Newtype RemixSettings _
derive instance repGenericRemixSettings :: Generic RemixSettings _
instance showRemixSettings :: Show RemixSettings where
  show = genericShow
instance decodeRemixSettings :: Decode RemixSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemixSettings :: Encode RemixSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ResourceConflict
newtype ResourceConflict = ResourceConflict 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceConflict :: Newtype ResourceConflict _
derive instance repGenericResourceConflict :: Generic ResourceConflict _
instance showResourceConflict :: Show ResourceConflict where
  show = genericShow
instance decodeResourceConflict :: Decode ResourceConflict where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceConflict :: Encode ResourceConflict where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ResourceNotFound
newtype ResourceNotFound = ResourceNotFound 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceNotFound :: Newtype ResourceNotFound _
derive instance repGenericResourceNotFound :: Generic ResourceNotFound _
instance showResourceNotFound :: Show ResourceNotFound where
  show = genericShow
instance decodeResourceNotFound :: Decode ResourceNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFound :: Encode ResourceNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte20Convert608To708
newtype Scte20Convert608To708 = Scte20Convert608To708 String
derive instance newtypeScte20Convert608To708 :: Newtype Scte20Convert608To708 _
derive instance repGenericScte20Convert608To708 :: Generic Scte20Convert608To708 _
instance showScte20Convert608To708 :: Show Scte20Convert608To708 where
  show = genericShow
instance decodeScte20Convert608To708 :: Decode Scte20Convert608To708 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte20Convert608To708 :: Encode Scte20Convert608To708 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte20PlusEmbeddedDestinationSettings
newtype Scte20PlusEmbeddedDestinationSettings = Scte20PlusEmbeddedDestinationSettings Types.NoArguments
derive instance newtypeScte20PlusEmbeddedDestinationSettings :: Newtype Scte20PlusEmbeddedDestinationSettings _
derive instance repGenericScte20PlusEmbeddedDestinationSettings :: Generic Scte20PlusEmbeddedDestinationSettings _
instance showScte20PlusEmbeddedDestinationSettings :: Show Scte20PlusEmbeddedDestinationSettings where
  show = genericShow
instance decodeScte20PlusEmbeddedDestinationSettings :: Decode Scte20PlusEmbeddedDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte20PlusEmbeddedDestinationSettings :: Encode Scte20PlusEmbeddedDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte20SourceSettings
newtype Scte20SourceSettings = Scte20SourceSettings 
  { "Convert608To708" :: NullOrUndefined.NullOrUndefined (Scte20Convert608To708)
  , "Source608ChannelNumber" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeScte20SourceSettings :: Newtype Scte20SourceSettings _
derive instance repGenericScte20SourceSettings :: Generic Scte20SourceSettings _
instance showScte20SourceSettings :: Show Scte20SourceSettings where
  show = genericShow
instance decodeScte20SourceSettings :: Decode Scte20SourceSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte20SourceSettings :: Encode Scte20SourceSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte27DestinationSettings
newtype Scte27DestinationSettings = Scte27DestinationSettings Types.NoArguments
derive instance newtypeScte27DestinationSettings :: Newtype Scte27DestinationSettings _
derive instance repGenericScte27DestinationSettings :: Generic Scte27DestinationSettings _
instance showScte27DestinationSettings :: Show Scte27DestinationSettings where
  show = genericShow
instance decodeScte27DestinationSettings :: Decode Scte27DestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte27DestinationSettings :: Encode Scte27DestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte27SourceSettings
newtype Scte27SourceSettings = Scte27SourceSettings 
  { "Pid" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeScte27SourceSettings :: Newtype Scte27SourceSettings _
derive instance repGenericScte27SourceSettings :: Generic Scte27SourceSettings _
instance showScte27SourceSettings :: Show Scte27SourceSettings where
  show = genericShow
instance decodeScte27SourceSettings :: Decode Scte27SourceSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte27SourceSettings :: Encode Scte27SourceSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte35AposNoRegionalBlackoutBehavior
newtype Scte35AposNoRegionalBlackoutBehavior = Scte35AposNoRegionalBlackoutBehavior String
derive instance newtypeScte35AposNoRegionalBlackoutBehavior :: Newtype Scte35AposNoRegionalBlackoutBehavior _
derive instance repGenericScte35AposNoRegionalBlackoutBehavior :: Generic Scte35AposNoRegionalBlackoutBehavior _
instance showScte35AposNoRegionalBlackoutBehavior :: Show Scte35AposNoRegionalBlackoutBehavior where
  show = genericShow
instance decodeScte35AposNoRegionalBlackoutBehavior :: Decode Scte35AposNoRegionalBlackoutBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte35AposNoRegionalBlackoutBehavior :: Encode Scte35AposNoRegionalBlackoutBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte35AposWebDeliveryAllowedBehavior
newtype Scte35AposWebDeliveryAllowedBehavior = Scte35AposWebDeliveryAllowedBehavior String
derive instance newtypeScte35AposWebDeliveryAllowedBehavior :: Newtype Scte35AposWebDeliveryAllowedBehavior _
derive instance repGenericScte35AposWebDeliveryAllowedBehavior :: Generic Scte35AposWebDeliveryAllowedBehavior _
instance showScte35AposWebDeliveryAllowedBehavior :: Show Scte35AposWebDeliveryAllowedBehavior where
  show = genericShow
instance decodeScte35AposWebDeliveryAllowedBehavior :: Decode Scte35AposWebDeliveryAllowedBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte35AposWebDeliveryAllowedBehavior :: Encode Scte35AposWebDeliveryAllowedBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte35SpliceInsert
newtype Scte35SpliceInsert = Scte35SpliceInsert 
  { "AdAvailOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "NoRegionalBlackoutFlag" :: NullOrUndefined.NullOrUndefined (Scte35SpliceInsertNoRegionalBlackoutBehavior)
  , "WebDeliveryAllowedFlag" :: NullOrUndefined.NullOrUndefined (Scte35SpliceInsertWebDeliveryAllowedBehavior)
  }
derive instance newtypeScte35SpliceInsert :: Newtype Scte35SpliceInsert _
derive instance repGenericScte35SpliceInsert :: Generic Scte35SpliceInsert _
instance showScte35SpliceInsert :: Show Scte35SpliceInsert where
  show = genericShow
instance decodeScte35SpliceInsert :: Decode Scte35SpliceInsert where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte35SpliceInsert :: Encode Scte35SpliceInsert where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte35SpliceInsertNoRegionalBlackoutBehavior
newtype Scte35SpliceInsertNoRegionalBlackoutBehavior = Scte35SpliceInsertNoRegionalBlackoutBehavior String
derive instance newtypeScte35SpliceInsertNoRegionalBlackoutBehavior :: Newtype Scte35SpliceInsertNoRegionalBlackoutBehavior _
derive instance repGenericScte35SpliceInsertNoRegionalBlackoutBehavior :: Generic Scte35SpliceInsertNoRegionalBlackoutBehavior _
instance showScte35SpliceInsertNoRegionalBlackoutBehavior :: Show Scte35SpliceInsertNoRegionalBlackoutBehavior where
  show = genericShow
instance decodeScte35SpliceInsertNoRegionalBlackoutBehavior :: Decode Scte35SpliceInsertNoRegionalBlackoutBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte35SpliceInsertNoRegionalBlackoutBehavior :: Encode Scte35SpliceInsertNoRegionalBlackoutBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte35SpliceInsertWebDeliveryAllowedBehavior
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior = Scte35SpliceInsertWebDeliveryAllowedBehavior String
derive instance newtypeScte35SpliceInsertWebDeliveryAllowedBehavior :: Newtype Scte35SpliceInsertWebDeliveryAllowedBehavior _
derive instance repGenericScte35SpliceInsertWebDeliveryAllowedBehavior :: Generic Scte35SpliceInsertWebDeliveryAllowedBehavior _
instance showScte35SpliceInsertWebDeliveryAllowedBehavior :: Show Scte35SpliceInsertWebDeliveryAllowedBehavior where
  show = genericShow
instance decodeScte35SpliceInsertWebDeliveryAllowedBehavior :: Decode Scte35SpliceInsertWebDeliveryAllowedBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte35SpliceInsertWebDeliveryAllowedBehavior :: Encode Scte35SpliceInsertWebDeliveryAllowedBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for Scte35TimeSignalApos
newtype Scte35TimeSignalApos = Scte35TimeSignalApos 
  { "AdAvailOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "NoRegionalBlackoutFlag" :: NullOrUndefined.NullOrUndefined (Scte35AposNoRegionalBlackoutBehavior)
  , "WebDeliveryAllowedFlag" :: NullOrUndefined.NullOrUndefined (Scte35AposWebDeliveryAllowedBehavior)
  }
derive instance newtypeScte35TimeSignalApos :: Newtype Scte35TimeSignalApos _
derive instance repGenericScte35TimeSignalApos :: Generic Scte35TimeSignalApos _
instance showScte35TimeSignalApos :: Show Scte35TimeSignalApos where
  show = genericShow
instance decodeScte35TimeSignalApos :: Decode Scte35TimeSignalApos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScte35TimeSignalApos :: Encode Scte35TimeSignalApos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for SmoothGroupAudioOnlyTimecodeControl
newtype SmoothGroupAudioOnlyTimecodeControl = SmoothGroupAudioOnlyTimecodeControl String
derive instance newtypeSmoothGroupAudioOnlyTimecodeControl :: Newtype SmoothGroupAudioOnlyTimecodeControl _
derive instance repGenericSmoothGroupAudioOnlyTimecodeControl :: Generic SmoothGroupAudioOnlyTimecodeControl _
instance showSmoothGroupAudioOnlyTimecodeControl :: Show SmoothGroupAudioOnlyTimecodeControl where
  show = genericShow
instance decodeSmoothGroupAudioOnlyTimecodeControl :: Decode SmoothGroupAudioOnlyTimecodeControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmoothGroupAudioOnlyTimecodeControl :: Encode SmoothGroupAudioOnlyTimecodeControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for SmoothGroupCertificateMode
newtype SmoothGroupCertificateMode = SmoothGroupCertificateMode String
derive instance newtypeSmoothGroupCertificateMode :: Newtype SmoothGroupCertificateMode _
derive instance repGenericSmoothGroupCertificateMode :: Generic SmoothGroupCertificateMode _
instance showSmoothGroupCertificateMode :: Show SmoothGroupCertificateMode where
  show = genericShow
instance decodeSmoothGroupCertificateMode :: Decode SmoothGroupCertificateMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmoothGroupCertificateMode :: Encode SmoothGroupCertificateMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for SmoothGroupEventIdMode
newtype SmoothGroupEventIdMode = SmoothGroupEventIdMode String
derive instance newtypeSmoothGroupEventIdMode :: Newtype SmoothGroupEventIdMode _
derive instance repGenericSmoothGroupEventIdMode :: Generic SmoothGroupEventIdMode _
instance showSmoothGroupEventIdMode :: Show SmoothGroupEventIdMode where
  show = genericShow
instance decodeSmoothGroupEventIdMode :: Decode SmoothGroupEventIdMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmoothGroupEventIdMode :: Encode SmoothGroupEventIdMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for SmoothGroupEventStopBehavior
newtype SmoothGroupEventStopBehavior = SmoothGroupEventStopBehavior String
derive instance newtypeSmoothGroupEventStopBehavior :: Newtype SmoothGroupEventStopBehavior _
derive instance repGenericSmoothGroupEventStopBehavior :: Generic SmoothGroupEventStopBehavior _
instance showSmoothGroupEventStopBehavior :: Show SmoothGroupEventStopBehavior where
  show = genericShow
instance decodeSmoothGroupEventStopBehavior :: Decode SmoothGroupEventStopBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmoothGroupEventStopBehavior :: Encode SmoothGroupEventStopBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for SmoothGroupSegmentationMode
newtype SmoothGroupSegmentationMode = SmoothGroupSegmentationMode String
derive instance newtypeSmoothGroupSegmentationMode :: Newtype SmoothGroupSegmentationMode _
derive instance repGenericSmoothGroupSegmentationMode :: Generic SmoothGroupSegmentationMode _
instance showSmoothGroupSegmentationMode :: Show SmoothGroupSegmentationMode where
  show = genericShow
instance decodeSmoothGroupSegmentationMode :: Decode SmoothGroupSegmentationMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmoothGroupSegmentationMode :: Encode SmoothGroupSegmentationMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for SmoothGroupSparseTrackType
newtype SmoothGroupSparseTrackType = SmoothGroupSparseTrackType String
derive instance newtypeSmoothGroupSparseTrackType :: Newtype SmoothGroupSparseTrackType _
derive instance repGenericSmoothGroupSparseTrackType :: Generic SmoothGroupSparseTrackType _
instance showSmoothGroupSparseTrackType :: Show SmoothGroupSparseTrackType where
  show = genericShow
instance decodeSmoothGroupSparseTrackType :: Decode SmoothGroupSparseTrackType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmoothGroupSparseTrackType :: Encode SmoothGroupSparseTrackType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for SmoothGroupStreamManifestBehavior
newtype SmoothGroupStreamManifestBehavior = SmoothGroupStreamManifestBehavior String
derive instance newtypeSmoothGroupStreamManifestBehavior :: Newtype SmoothGroupStreamManifestBehavior _
derive instance repGenericSmoothGroupStreamManifestBehavior :: Generic SmoothGroupStreamManifestBehavior _
instance showSmoothGroupStreamManifestBehavior :: Show SmoothGroupStreamManifestBehavior where
  show = genericShow
instance decodeSmoothGroupStreamManifestBehavior :: Decode SmoothGroupStreamManifestBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmoothGroupStreamManifestBehavior :: Encode SmoothGroupStreamManifestBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for SmoothGroupTimestampOffsetMode
newtype SmoothGroupTimestampOffsetMode = SmoothGroupTimestampOffsetMode String
derive instance newtypeSmoothGroupTimestampOffsetMode :: Newtype SmoothGroupTimestampOffsetMode _
derive instance repGenericSmoothGroupTimestampOffsetMode :: Generic SmoothGroupTimestampOffsetMode _
instance showSmoothGroupTimestampOffsetMode :: Show SmoothGroupTimestampOffsetMode where
  show = genericShow
instance decodeSmoothGroupTimestampOffsetMode :: Decode SmoothGroupTimestampOffsetMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmoothGroupTimestampOffsetMode :: Encode SmoothGroupTimestampOffsetMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for SmpteTtDestinationSettings
newtype SmpteTtDestinationSettings = SmpteTtDestinationSettings Types.NoArguments
derive instance newtypeSmpteTtDestinationSettings :: Newtype SmpteTtDestinationSettings _
derive instance repGenericSmpteTtDestinationSettings :: Generic SmpteTtDestinationSettings _
instance showSmpteTtDestinationSettings :: Show SmpteTtDestinationSettings where
  show = genericShow
instance decodeSmpteTtDestinationSettings :: Decode SmpteTtDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSmpteTtDestinationSettings :: Encode SmpteTtDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for StandardHlsSettings
newtype StandardHlsSettings = StandardHlsSettings 
  { "AudioRenditionSets" :: NullOrUndefined.NullOrUndefined (String)
  , "M3u8Settings" :: NullOrUndefined.NullOrUndefined (M3u8Settings)
  }
derive instance newtypeStandardHlsSettings :: Newtype StandardHlsSettings _
derive instance repGenericStandardHlsSettings :: Generic StandardHlsSettings _
instance showStandardHlsSettings :: Show StandardHlsSettings where
  show = genericShow
instance decodeStandardHlsSettings :: Decode StandardHlsSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStandardHlsSettings :: Encode StandardHlsSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for StartChannelRequest
newtype StartChannelRequest = StartChannelRequest 
  { "ChannelId" :: (String)
  }
derive instance newtypeStartChannelRequest :: Newtype StartChannelRequest _
derive instance repGenericStartChannelRequest :: Generic StartChannelRequest _
instance showStartChannelRequest :: Show StartChannelRequest where
  show = genericShow
instance decodeStartChannelRequest :: Decode StartChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartChannelRequest :: Encode StartChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for StartChannelResponse
newtype StartChannelResponse = StartChannelResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined.NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined.NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined.NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (ChannelState)
  }
derive instance newtypeStartChannelResponse :: Newtype StartChannelResponse _
derive instance repGenericStartChannelResponse :: Generic StartChannelResponse _
instance showStartChannelResponse :: Show StartChannelResponse where
  show = genericShow
instance decodeStartChannelResponse :: Decode StartChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartChannelResponse :: Encode StartChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for StaticKeySettings
newtype StaticKeySettings = StaticKeySettings 
  { "KeyProviderServer" :: NullOrUndefined.NullOrUndefined (InputLocation)
  , "StaticKeyValue" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeStaticKeySettings :: Newtype StaticKeySettings _
derive instance repGenericStaticKeySettings :: Generic StaticKeySettings _
instance showStaticKeySettings :: Show StaticKeySettings where
  show = genericShow
instance decodeStaticKeySettings :: Decode StaticKeySettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStaticKeySettings :: Encode StaticKeySettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for StopChannelRequest
newtype StopChannelRequest = StopChannelRequest 
  { "ChannelId" :: (String)
  }
derive instance newtypeStopChannelRequest :: Newtype StopChannelRequest _
derive instance repGenericStopChannelRequest :: Generic StopChannelRequest _
instance showStopChannelRequest :: Show StopChannelRequest where
  show = genericShow
instance decodeStopChannelRequest :: Decode StopChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopChannelRequest :: Encode StopChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for StopChannelResponse
newtype StopChannelResponse = StopChannelResponse 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined.NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined.NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined.NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (ChannelState)
  }
derive instance newtypeStopChannelResponse :: Newtype StopChannelResponse _
derive instance repGenericStopChannelResponse :: Generic StopChannelResponse _
instance showStopChannelResponse :: Show StopChannelResponse where
  show = genericShow
instance decodeStopChannelResponse :: Decode StopChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopChannelResponse :: Encode StopChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for TeletextDestinationSettings
newtype TeletextDestinationSettings = TeletextDestinationSettings Types.NoArguments
derive instance newtypeTeletextDestinationSettings :: Newtype TeletextDestinationSettings _
derive instance repGenericTeletextDestinationSettings :: Generic TeletextDestinationSettings _
instance showTeletextDestinationSettings :: Show TeletextDestinationSettings where
  show = genericShow
instance decodeTeletextDestinationSettings :: Decode TeletextDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTeletextDestinationSettings :: Encode TeletextDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for TeletextSourceSettings
newtype TeletextSourceSettings = TeletextSourceSettings 
  { "PageNumber" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTeletextSourceSettings :: Newtype TeletextSourceSettings _
derive instance repGenericTeletextSourceSettings :: Generic TeletextSourceSettings _
instance showTeletextSourceSettings :: Show TeletextSourceSettings where
  show = genericShow
instance decodeTeletextSourceSettings :: Decode TeletextSourceSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTeletextSourceSettings :: Encode TeletextSourceSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for TimecodeConfig
newtype TimecodeConfig = TimecodeConfig 
  { "Source" :: NullOrUndefined.NullOrUndefined (TimecodeConfigSource)
  , "SyncThreshold" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeTimecodeConfig :: Newtype TimecodeConfig _
derive instance repGenericTimecodeConfig :: Generic TimecodeConfig _
instance showTimecodeConfig :: Show TimecodeConfig where
  show = genericShow
instance decodeTimecodeConfig :: Decode TimecodeConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimecodeConfig :: Encode TimecodeConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for TimecodeConfigSource
newtype TimecodeConfigSource = TimecodeConfigSource String
derive instance newtypeTimecodeConfigSource :: Newtype TimecodeConfigSource _
derive instance repGenericTimecodeConfigSource :: Generic TimecodeConfigSource _
instance showTimecodeConfigSource :: Show TimecodeConfigSource where
  show = genericShow
instance decodeTimecodeConfigSource :: Decode TimecodeConfigSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimecodeConfigSource :: Encode TimecodeConfigSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for TooManyRequestsException
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


-- | Placeholder documentation for TtmlDestinationSettings
newtype TtmlDestinationSettings = TtmlDestinationSettings 
  { "StyleControl" :: NullOrUndefined.NullOrUndefined (TtmlDestinationStyleControl)
  }
derive instance newtypeTtmlDestinationSettings :: Newtype TtmlDestinationSettings _
derive instance repGenericTtmlDestinationSettings :: Generic TtmlDestinationSettings _
instance showTtmlDestinationSettings :: Show TtmlDestinationSettings where
  show = genericShow
instance decodeTtmlDestinationSettings :: Decode TtmlDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTtmlDestinationSettings :: Encode TtmlDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for TtmlDestinationStyleControl
newtype TtmlDestinationStyleControl = TtmlDestinationStyleControl String
derive instance newtypeTtmlDestinationStyleControl :: Newtype TtmlDestinationStyleControl _
derive instance repGenericTtmlDestinationStyleControl :: Generic TtmlDestinationStyleControl _
instance showTtmlDestinationStyleControl :: Show TtmlDestinationStyleControl where
  show = genericShow
instance decodeTtmlDestinationStyleControl :: Decode TtmlDestinationStyleControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTtmlDestinationStyleControl :: Encode TtmlDestinationStyleControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for UdpContainerSettings
newtype UdpContainerSettings = UdpContainerSettings 
  { "M2tsSettings" :: NullOrUndefined.NullOrUndefined (M2tsSettings)
  }
derive instance newtypeUdpContainerSettings :: Newtype UdpContainerSettings _
derive instance repGenericUdpContainerSettings :: Generic UdpContainerSettings _
instance showUdpContainerSettings :: Show UdpContainerSettings where
  show = genericShow
instance decodeUdpContainerSettings :: Decode UdpContainerSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUdpContainerSettings :: Encode UdpContainerSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for UdpGroupSettings
newtype UdpGroupSettings = UdpGroupSettings 
  { "InputLossAction" :: NullOrUndefined.NullOrUndefined (InputLossActionForUdpOut)
  , "TimedMetadataId3Frame" :: NullOrUndefined.NullOrUndefined (UdpTimedMetadataId3Frame)
  , "TimedMetadataId3Period" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeUdpGroupSettings :: Newtype UdpGroupSettings _
derive instance repGenericUdpGroupSettings :: Generic UdpGroupSettings _
instance showUdpGroupSettings :: Show UdpGroupSettings where
  show = genericShow
instance decodeUdpGroupSettings :: Decode UdpGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUdpGroupSettings :: Encode UdpGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for UdpOutputSettings
newtype UdpOutputSettings = UdpOutputSettings 
  { "BufferMsec" :: NullOrUndefined.NullOrUndefined (Int)
  , "ContainerSettings" :: NullOrUndefined.NullOrUndefined (UdpContainerSettings)
  , "Destination" :: NullOrUndefined.NullOrUndefined (OutputLocationRef)
  , "FecOutputSettings" :: NullOrUndefined.NullOrUndefined (FecOutputSettings)
  }
derive instance newtypeUdpOutputSettings :: Newtype UdpOutputSettings _
derive instance repGenericUdpOutputSettings :: Generic UdpOutputSettings _
instance showUdpOutputSettings :: Show UdpOutputSettings where
  show = genericShow
instance decodeUdpOutputSettings :: Decode UdpOutputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUdpOutputSettings :: Encode UdpOutputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for UdpTimedMetadataId3Frame
newtype UdpTimedMetadataId3Frame = UdpTimedMetadataId3Frame String
derive instance newtypeUdpTimedMetadataId3Frame :: Newtype UdpTimedMetadataId3Frame _
derive instance repGenericUdpTimedMetadataId3Frame :: Generic UdpTimedMetadataId3Frame _
instance showUdpTimedMetadataId3Frame :: Show UdpTimedMetadataId3Frame where
  show = genericShow
instance decodeUdpTimedMetadataId3Frame :: Decode UdpTimedMetadataId3Frame where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUdpTimedMetadataId3Frame :: Encode UdpTimedMetadataId3Frame where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for UnprocessableEntityException
newtype UnprocessableEntityException = UnprocessableEntityException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "ValidationErrors" :: NullOrUndefined.NullOrUndefined (ListOfValidationError)
  }
derive instance newtypeUnprocessableEntityException :: Newtype UnprocessableEntityException _
derive instance repGenericUnprocessableEntityException :: Generic UnprocessableEntityException _
instance showUnprocessableEntityException :: Show UnprocessableEntityException where
  show = genericShow
instance decodeUnprocessableEntityException :: Decode UnprocessableEntityException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnprocessableEntityException :: Encode UnprocessableEntityException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for UpdateChannel
newtype UpdateChannel = UpdateChannel 
  { "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined.NullOrUndefined (EncoderSettings)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateChannel :: Newtype UpdateChannel _
derive instance repGenericUpdateChannel :: Generic UpdateChannel _
instance showUpdateChannel :: Show UpdateChannel where
  show = genericShow
instance decodeUpdateChannel :: Decode UpdateChannel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateChannel :: Encode UpdateChannel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A request to update a channel.
newtype UpdateChannelRequest = UpdateChannelRequest 
  { "ChannelId" :: (String)
  , "Destinations" :: NullOrUndefined.NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined.NullOrUndefined (EncoderSettings)
  , "InputSpecification" :: NullOrUndefined.NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateChannelRequest :: Newtype UpdateChannelRequest _
derive instance repGenericUpdateChannelRequest :: Generic UpdateChannelRequest _
instance showUpdateChannelRequest :: Show UpdateChannelRequest where
  show = genericShow
instance decodeUpdateChannelRequest :: Decode UpdateChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateChannelRequest :: Encode UpdateChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for UpdateChannelResponse
newtype UpdateChannelResponse = UpdateChannelResponse 
  { "Channel" :: NullOrUndefined.NullOrUndefined (Channel)
  }
derive instance newtypeUpdateChannelResponse :: Newtype UpdateChannelResponse _
derive instance repGenericUpdateChannelResponse :: Generic UpdateChannelResponse _
instance showUpdateChannelResponse :: Show UpdateChannelResponse where
  show = genericShow
instance decodeUpdateChannelResponse :: Decode UpdateChannelResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateChannelResponse :: Encode UpdateChannelResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The updated channel's description.
newtype UpdateChannelResultModel = UpdateChannelResultModel 
  { "Channel" :: NullOrUndefined.NullOrUndefined (Channel)
  }
derive instance newtypeUpdateChannelResultModel :: Newtype UpdateChannelResultModel _
derive instance repGenericUpdateChannelResultModel :: Generic UpdateChannelResultModel _
instance showUpdateChannelResultModel :: Show UpdateChannelResultModel where
  show = genericShow
instance decodeUpdateChannelResultModel :: Decode UpdateChannelResultModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateChannelResultModel :: Encode UpdateChannelResultModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for ValidationError
newtype ValidationError = ValidationError 
  { "ElementPath" :: NullOrUndefined.NullOrUndefined (String)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeValidationError :: Newtype ValidationError _
derive instance repGenericValidationError :: Generic ValidationError _
instance showValidationError :: Show ValidationError where
  show = genericShow
instance decodeValidationError :: Decode ValidationError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationError :: Encode ValidationError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for VideoCodecSettings
newtype VideoCodecSettings = VideoCodecSettings 
  { "H264Settings" :: NullOrUndefined.NullOrUndefined (H264Settings)
  }
derive instance newtypeVideoCodecSettings :: Newtype VideoCodecSettings _
derive instance repGenericVideoCodecSettings :: Generic VideoCodecSettings _
instance showVideoCodecSettings :: Show VideoCodecSettings where
  show = genericShow
instance decodeVideoCodecSettings :: Decode VideoCodecSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoCodecSettings :: Encode VideoCodecSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Video settings for this stream.
newtype VideoDescription = VideoDescription 
  { "CodecSettings" :: NullOrUndefined.NullOrUndefined (VideoCodecSettings)
  , "Height" :: NullOrUndefined.NullOrUndefined (Int)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "RespondToAfd" :: NullOrUndefined.NullOrUndefined (VideoDescriptionRespondToAfd)
  , "ScalingBehavior" :: NullOrUndefined.NullOrUndefined (VideoDescriptionScalingBehavior)
  , "Sharpness" :: NullOrUndefined.NullOrUndefined (Int)
  , "Width" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeVideoDescription :: Newtype VideoDescription _
derive instance repGenericVideoDescription :: Generic VideoDescription _
instance showVideoDescription :: Show VideoDescription where
  show = genericShow
instance decodeVideoDescription :: Decode VideoDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoDescription :: Encode VideoDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for VideoDescriptionRespondToAfd
newtype VideoDescriptionRespondToAfd = VideoDescriptionRespondToAfd String
derive instance newtypeVideoDescriptionRespondToAfd :: Newtype VideoDescriptionRespondToAfd _
derive instance repGenericVideoDescriptionRespondToAfd :: Generic VideoDescriptionRespondToAfd _
instance showVideoDescriptionRespondToAfd :: Show VideoDescriptionRespondToAfd where
  show = genericShow
instance decodeVideoDescriptionRespondToAfd :: Decode VideoDescriptionRespondToAfd where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoDescriptionRespondToAfd :: Encode VideoDescriptionRespondToAfd where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for VideoDescriptionScalingBehavior
newtype VideoDescriptionScalingBehavior = VideoDescriptionScalingBehavior String
derive instance newtypeVideoDescriptionScalingBehavior :: Newtype VideoDescriptionScalingBehavior _
derive instance repGenericVideoDescriptionScalingBehavior :: Generic VideoDescriptionScalingBehavior _
instance showVideoDescriptionScalingBehavior :: Show VideoDescriptionScalingBehavior where
  show = genericShow
instance decodeVideoDescriptionScalingBehavior :: Decode VideoDescriptionScalingBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoDescriptionScalingBehavior :: Encode VideoDescriptionScalingBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies a particular video stream within an input source. An input may have only a single video selector.
newtype VideoSelector = VideoSelector 
  { "ColorSpace" :: NullOrUndefined.NullOrUndefined (VideoSelectorColorSpace)
  , "ColorSpaceUsage" :: NullOrUndefined.NullOrUndefined (VideoSelectorColorSpaceUsage)
  , "SelectorSettings" :: NullOrUndefined.NullOrUndefined (VideoSelectorSettings)
  }
derive instance newtypeVideoSelector :: Newtype VideoSelector _
derive instance repGenericVideoSelector :: Generic VideoSelector _
instance showVideoSelector :: Show VideoSelector where
  show = genericShow
instance decodeVideoSelector :: Decode VideoSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoSelector :: Encode VideoSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for VideoSelectorColorSpace
newtype VideoSelectorColorSpace = VideoSelectorColorSpace String
derive instance newtypeVideoSelectorColorSpace :: Newtype VideoSelectorColorSpace _
derive instance repGenericVideoSelectorColorSpace :: Generic VideoSelectorColorSpace _
instance showVideoSelectorColorSpace :: Show VideoSelectorColorSpace where
  show = genericShow
instance decodeVideoSelectorColorSpace :: Decode VideoSelectorColorSpace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoSelectorColorSpace :: Encode VideoSelectorColorSpace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for VideoSelectorColorSpaceUsage
newtype VideoSelectorColorSpaceUsage = VideoSelectorColorSpaceUsage String
derive instance newtypeVideoSelectorColorSpaceUsage :: Newtype VideoSelectorColorSpaceUsage _
derive instance repGenericVideoSelectorColorSpaceUsage :: Generic VideoSelectorColorSpaceUsage _
instance showVideoSelectorColorSpaceUsage :: Show VideoSelectorColorSpaceUsage where
  show = genericShow
instance decodeVideoSelectorColorSpaceUsage :: Decode VideoSelectorColorSpaceUsage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoSelectorColorSpaceUsage :: Encode VideoSelectorColorSpaceUsage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for VideoSelectorPid
newtype VideoSelectorPid = VideoSelectorPid 
  { "Pid" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeVideoSelectorPid :: Newtype VideoSelectorPid _
derive instance repGenericVideoSelectorPid :: Generic VideoSelectorPid _
instance showVideoSelectorPid :: Show VideoSelectorPid where
  show = genericShow
instance decodeVideoSelectorPid :: Decode VideoSelectorPid where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoSelectorPid :: Encode VideoSelectorPid where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for VideoSelectorProgramId
newtype VideoSelectorProgramId = VideoSelectorProgramId 
  { "ProgramId" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeVideoSelectorProgramId :: Newtype VideoSelectorProgramId _
derive instance repGenericVideoSelectorProgramId :: Generic VideoSelectorProgramId _
instance showVideoSelectorProgramId :: Show VideoSelectorProgramId where
  show = genericShow
instance decodeVideoSelectorProgramId :: Decode VideoSelectorProgramId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoSelectorProgramId :: Encode VideoSelectorProgramId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for VideoSelectorSettings
newtype VideoSelectorSettings = VideoSelectorSettings 
  { "VideoSelectorPid" :: NullOrUndefined.NullOrUndefined (VideoSelectorPid)
  , "VideoSelectorProgramId" :: NullOrUndefined.NullOrUndefined (VideoSelectorProgramId)
  }
derive instance newtypeVideoSelectorSettings :: Newtype VideoSelectorSettings _
derive instance repGenericVideoSelectorSettings :: Generic VideoSelectorSettings _
instance showVideoSelectorSettings :: Show VideoSelectorSettings where
  show = genericShow
instance decodeVideoSelectorSettings :: Decode VideoSelectorSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoSelectorSettings :: Encode VideoSelectorSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Placeholder documentation for WebvttDestinationSettings
newtype WebvttDestinationSettings = WebvttDestinationSettings Types.NoArguments
derive instance newtypeWebvttDestinationSettings :: Newtype WebvttDestinationSettings _
derive instance repGenericWebvttDestinationSettings :: Generic WebvttDestinationSettings _
instance showWebvttDestinationSettings :: Show WebvttDestinationSettings where
  show = genericShow
instance decodeWebvttDestinationSettings :: Decode WebvttDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWebvttDestinationSettings :: Encode WebvttDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
