

-- | API for AWS Elemental MediaLive
module AWS.MediaLive where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MediaLive" :: String


-- | Creates a new channel
createChannel :: forall eff. CreateChannelRequest -> Aff (err :: AWS.RequestError | eff) CreateChannelResponse
createChannel = AWS.request serviceName "CreateChannel" 


-- | Create an input
createInput :: forall eff. CreateInputRequest -> Aff (err :: AWS.RequestError | eff) CreateInputResponse
createInput = AWS.request serviceName "CreateInput" 


-- | Creates a Input Security Group
createInputSecurityGroup :: forall eff. CreateInputSecurityGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateInputSecurityGroupResponse
createInputSecurityGroup = AWS.request serviceName "CreateInputSecurityGroup" 


-- | Starts deletion of channel. The associated outputs are also deleted.
deleteChannel :: forall eff. DeleteChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteChannelResponse
deleteChannel = AWS.request serviceName "DeleteChannel" 


-- | Deletes the input end point
deleteInput :: forall eff. DeleteInputRequest -> Aff (err :: AWS.RequestError | eff) DeleteInputResponse
deleteInput = AWS.request serviceName "DeleteInput" 


-- | Deletes an Input Security Group
deleteInputSecurityGroup :: forall eff. DeleteInputSecurityGroupRequest -> Aff (err :: AWS.RequestError | eff) DeleteInputSecurityGroupResponse
deleteInputSecurityGroup = AWS.request serviceName "DeleteInputSecurityGroup" 


-- | Gets details about a channel
describeChannel :: forall eff. DescribeChannelRequest -> Aff (err :: AWS.RequestError | eff) DescribeChannelResponse
describeChannel = AWS.request serviceName "DescribeChannel" 


-- | Produces details about an input
describeInput :: forall eff. DescribeInputRequest -> Aff (err :: AWS.RequestError | eff) DescribeInputResponse
describeInput = AWS.request serviceName "DescribeInput" 


-- | Produces a summary of an Input Security Group
describeInputSecurityGroup :: forall eff. DescribeInputSecurityGroupRequest -> Aff (err :: AWS.RequestError | eff) DescribeInputSecurityGroupResponse
describeInputSecurityGroup = AWS.request serviceName "DescribeInputSecurityGroup" 


-- | Produces list of channels that have been created
listChannels :: forall eff. ListChannelsRequest -> Aff (err :: AWS.RequestError | eff) ListChannelsResponse
listChannels = AWS.request serviceName "ListChannels" 


-- | Produces a list of Input Security Groups for an account
listInputSecurityGroups :: forall eff. ListInputSecurityGroupsRequest -> Aff (err :: AWS.RequestError | eff) ListInputSecurityGroupsResponse
listInputSecurityGroups = AWS.request serviceName "ListInputSecurityGroups" 


-- | Produces list of inputs that have been created
listInputs :: forall eff. ListInputsRequest -> Aff (err :: AWS.RequestError | eff) ListInputsResponse
listInputs = AWS.request serviceName "ListInputs" 


-- | Starts an existing channel
startChannel :: forall eff. StartChannelRequest -> Aff (err :: AWS.RequestError | eff) StartChannelResponse
startChannel = AWS.request serviceName "StartChannel" 


-- | Stops a running channel
stopChannel :: forall eff. StopChannelRequest -> Aff (err :: AWS.RequestError | eff) StopChannelResponse
stopChannel = AWS.request serviceName "StopChannel" 


-- | Updates a channel.
updateChannel :: forall eff. UpdateChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateChannelResponse
updateChannel = AWS.request serviceName "UpdateChannel" 


-- | Placeholder documentation for AacCodingMode
newtype AacCodingMode = AacCodingMode String
derive instance newtypeAacCodingMode :: Newtype AacCodingMode _


-- | Placeholder documentation for AacInputType
newtype AacInputType = AacInputType String
derive instance newtypeAacInputType :: Newtype AacInputType _


-- | Placeholder documentation for AacProfile
newtype AacProfile = AacProfile String
derive instance newtypeAacProfile :: Newtype AacProfile _


-- | Placeholder documentation for AacRateControlMode
newtype AacRateControlMode = AacRateControlMode String
derive instance newtypeAacRateControlMode :: Newtype AacRateControlMode _


-- | Placeholder documentation for AacRawFormat
newtype AacRawFormat = AacRawFormat String
derive instance newtypeAacRawFormat :: Newtype AacRawFormat _


-- | Placeholder documentation for AacSettings
newtype AacSettings = AacSettings 
  { "Bitrate" :: NullOrUndefined (Number)
  , "CodingMode" :: NullOrUndefined (AacCodingMode)
  , "InputType" :: NullOrUndefined (AacInputType)
  , "Profile" :: NullOrUndefined (AacProfile)
  , "RateControlMode" :: NullOrUndefined (AacRateControlMode)
  , "RawFormat" :: NullOrUndefined (AacRawFormat)
  , "SampleRate" :: NullOrUndefined (Number)
  , "Spec" :: NullOrUndefined (AacSpec)
  , "VbrQuality" :: NullOrUndefined (AacVbrQuality)
  }
derive instance newtypeAacSettings :: Newtype AacSettings _


-- | Placeholder documentation for AacSpec
newtype AacSpec = AacSpec String
derive instance newtypeAacSpec :: Newtype AacSpec _


-- | Placeholder documentation for AacVbrQuality
newtype AacVbrQuality = AacVbrQuality String
derive instance newtypeAacVbrQuality :: Newtype AacVbrQuality _


-- | Placeholder documentation for Ac3BitstreamMode
newtype Ac3BitstreamMode = Ac3BitstreamMode String
derive instance newtypeAc3BitstreamMode :: Newtype Ac3BitstreamMode _


-- | Placeholder documentation for Ac3CodingMode
newtype Ac3CodingMode = Ac3CodingMode String
derive instance newtypeAc3CodingMode :: Newtype Ac3CodingMode _


-- | Placeholder documentation for Ac3DrcProfile
newtype Ac3DrcProfile = Ac3DrcProfile String
derive instance newtypeAc3DrcProfile :: Newtype Ac3DrcProfile _


-- | Placeholder documentation for Ac3LfeFilter
newtype Ac3LfeFilter = Ac3LfeFilter String
derive instance newtypeAc3LfeFilter :: Newtype Ac3LfeFilter _


-- | Placeholder documentation for Ac3MetadataControl
newtype Ac3MetadataControl = Ac3MetadataControl String
derive instance newtypeAc3MetadataControl :: Newtype Ac3MetadataControl _


-- | Placeholder documentation for Ac3Settings
newtype Ac3Settings = Ac3Settings 
  { "Bitrate" :: NullOrUndefined (Number)
  , "BitstreamMode" :: NullOrUndefined (Ac3BitstreamMode)
  , "CodingMode" :: NullOrUndefined (Ac3CodingMode)
  , "Dialnorm" :: NullOrUndefined (Int)
  , "DrcProfile" :: NullOrUndefined (Ac3DrcProfile)
  , "LfeFilter" :: NullOrUndefined (Ac3LfeFilter)
  , "MetadataControl" :: NullOrUndefined (Ac3MetadataControl)
  }
derive instance newtypeAc3Settings :: Newtype Ac3Settings _


-- | Placeholder documentation for AccessDenied
newtype AccessDenied = AccessDenied 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAccessDenied :: Newtype AccessDenied _


-- | Placeholder documentation for AfdSignaling
newtype AfdSignaling = AfdSignaling String
derive instance newtypeAfdSignaling :: Newtype AfdSignaling _


-- | Placeholder documentation for ArchiveContainerSettings
newtype ArchiveContainerSettings = ArchiveContainerSettings 
  { "M2tsSettings" :: NullOrUndefined (M2tsSettings)
  }
derive instance newtypeArchiveContainerSettings :: Newtype ArchiveContainerSettings _


-- | Placeholder documentation for ArchiveGroupSettings
newtype ArchiveGroupSettings = ArchiveGroupSettings 
  { "Destination" :: NullOrUndefined (OutputLocationRef)
  , "RolloverInterval" :: NullOrUndefined (Int)
  }
derive instance newtypeArchiveGroupSettings :: Newtype ArchiveGroupSettings _


-- | Placeholder documentation for ArchiveOutputSettings
newtype ArchiveOutputSettings = ArchiveOutputSettings 
  { "ContainerSettings" :: NullOrUndefined (ArchiveContainerSettings)
  , "Extension" :: NullOrUndefined (String)
  , "NameModifier" :: NullOrUndefined (String)
  }
derive instance newtypeArchiveOutputSettings :: Newtype ArchiveOutputSettings _


-- | Placeholder documentation for AribDestinationSettings
newtype AribDestinationSettings = AribDestinationSettings 
  { 
  }
derive instance newtypeAribDestinationSettings :: Newtype AribDestinationSettings _


-- | Placeholder documentation for AribSourceSettings
newtype AribSourceSettings = AribSourceSettings 
  { 
  }
derive instance newtypeAribSourceSettings :: Newtype AribSourceSettings _


-- | Placeholder documentation for AudioChannelMapping
newtype AudioChannelMapping = AudioChannelMapping 
  { "InputChannelLevels" :: NullOrUndefined (ListOfInputChannelLevel)
  , "OutputChannel" :: NullOrUndefined (Int)
  }
derive instance newtypeAudioChannelMapping :: Newtype AudioChannelMapping _


-- | Placeholder documentation for AudioCodecSettings
newtype AudioCodecSettings = AudioCodecSettings 
  { "AacSettings" :: NullOrUndefined (AacSettings)
  , "Ac3Settings" :: NullOrUndefined (Ac3Settings)
  , "Eac3Settings" :: NullOrUndefined (Eac3Settings)
  , "Mp2Settings" :: NullOrUndefined (Mp2Settings)
  , "PassThroughSettings" :: NullOrUndefined (PassThroughSettings)
  }
derive instance newtypeAudioCodecSettings :: Newtype AudioCodecSettings _


-- | Placeholder documentation for AudioDescription
newtype AudioDescription = AudioDescription 
  { "AudioNormalizationSettings" :: NullOrUndefined (AudioNormalizationSettings)
  , "AudioSelectorName" :: NullOrUndefined (String)
  , "AudioType" :: NullOrUndefined (AudioType)
  , "AudioTypeControl" :: NullOrUndefined (AudioDescriptionAudioTypeControl)
  , "CodecSettings" :: NullOrUndefined (AudioCodecSettings)
  , "LanguageCode" :: NullOrUndefined (String)
  , "LanguageCodeControl" :: NullOrUndefined (AudioDescriptionLanguageCodeControl)
  , "Name" :: NullOrUndefined (String)
  , "RemixSettings" :: NullOrUndefined (RemixSettings)
  , "StreamName" :: NullOrUndefined (String)
  }
derive instance newtypeAudioDescription :: Newtype AudioDescription _


-- | Placeholder documentation for AudioDescriptionAudioTypeControl
newtype AudioDescriptionAudioTypeControl = AudioDescriptionAudioTypeControl String
derive instance newtypeAudioDescriptionAudioTypeControl :: Newtype AudioDescriptionAudioTypeControl _


-- | Placeholder documentation for AudioDescriptionLanguageCodeControl
newtype AudioDescriptionLanguageCodeControl = AudioDescriptionLanguageCodeControl String
derive instance newtypeAudioDescriptionLanguageCodeControl :: Newtype AudioDescriptionLanguageCodeControl _


-- | Placeholder documentation for AudioLanguageSelection
newtype AudioLanguageSelection = AudioLanguageSelection 
  { "LanguageCode" :: NullOrUndefined (String)
  , "LanguageSelectionPolicy" :: NullOrUndefined (AudioLanguageSelectionPolicy)
  }
derive instance newtypeAudioLanguageSelection :: Newtype AudioLanguageSelection _


-- | Placeholder documentation for AudioLanguageSelectionPolicy
newtype AudioLanguageSelectionPolicy = AudioLanguageSelectionPolicy String
derive instance newtypeAudioLanguageSelectionPolicy :: Newtype AudioLanguageSelectionPolicy _


-- | Placeholder documentation for AudioNormalizationAlgorithm
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm String
derive instance newtypeAudioNormalizationAlgorithm :: Newtype AudioNormalizationAlgorithm _


-- | Placeholder documentation for AudioNormalizationAlgorithmControl
newtype AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl String
derive instance newtypeAudioNormalizationAlgorithmControl :: Newtype AudioNormalizationAlgorithmControl _


-- | Placeholder documentation for AudioNormalizationSettings
newtype AudioNormalizationSettings = AudioNormalizationSettings 
  { "Algorithm" :: NullOrUndefined (AudioNormalizationAlgorithm)
  , "AlgorithmControl" :: NullOrUndefined (AudioNormalizationAlgorithmControl)
  , "TargetLkfs" :: NullOrUndefined (Number)
  }
derive instance newtypeAudioNormalizationSettings :: Newtype AudioNormalizationSettings _


-- | Placeholder documentation for AudioOnlyHlsSettings
newtype AudioOnlyHlsSettings = AudioOnlyHlsSettings 
  { "AudioGroupId" :: NullOrUndefined (String)
  , "AudioOnlyImage" :: NullOrUndefined (InputLocation)
  , "AudioTrackType" :: NullOrUndefined (AudioOnlyHlsTrackType)
  }
derive instance newtypeAudioOnlyHlsSettings :: Newtype AudioOnlyHlsSettings _


-- | Placeholder documentation for AudioOnlyHlsTrackType
newtype AudioOnlyHlsTrackType = AudioOnlyHlsTrackType String
derive instance newtypeAudioOnlyHlsTrackType :: Newtype AudioOnlyHlsTrackType _


-- | Placeholder documentation for AudioPidSelection
newtype AudioPidSelection = AudioPidSelection 
  { "Pid" :: NullOrUndefined (Int)
  }
derive instance newtypeAudioPidSelection :: Newtype AudioPidSelection _


-- | Placeholder documentation for AudioSelector
newtype AudioSelector = AudioSelector 
  { "Name" :: NullOrUndefined (String)
  , "SelectorSettings" :: NullOrUndefined (AudioSelectorSettings)
  }
derive instance newtypeAudioSelector :: Newtype AudioSelector _


-- | Placeholder documentation for AudioSelectorSettings
newtype AudioSelectorSettings = AudioSelectorSettings 
  { "AudioLanguageSelection" :: NullOrUndefined (AudioLanguageSelection)
  , "AudioPidSelection" :: NullOrUndefined (AudioPidSelection)
  }
derive instance newtypeAudioSelectorSettings :: Newtype AudioSelectorSettings _


-- | Placeholder documentation for AudioType
newtype AudioType = AudioType String
derive instance newtypeAudioType :: Newtype AudioType _


-- | Placeholder documentation for AvailBlanking
newtype AvailBlanking = AvailBlanking 
  { "AvailBlankingImage" :: NullOrUndefined (InputLocation)
  , "State" :: NullOrUndefined (AvailBlankingState)
  }
derive instance newtypeAvailBlanking :: Newtype AvailBlanking _


-- | Placeholder documentation for AvailBlankingState
newtype AvailBlankingState = AvailBlankingState String
derive instance newtypeAvailBlankingState :: Newtype AvailBlankingState _


-- | Placeholder documentation for AvailConfiguration
newtype AvailConfiguration = AvailConfiguration 
  { "AvailSettings" :: NullOrUndefined (AvailSettings)
  }
derive instance newtypeAvailConfiguration :: Newtype AvailConfiguration _


-- | Placeholder documentation for AvailSettings
newtype AvailSettings = AvailSettings 
  { "Scte35SpliceInsert" :: NullOrUndefined (Scte35SpliceInsert)
  , "Scte35TimeSignalApos" :: NullOrUndefined (Scte35TimeSignalApos)
  }
derive instance newtypeAvailSettings :: Newtype AvailSettings _


-- | Placeholder documentation for BadGatewayException
newtype BadGatewayException = BadGatewayException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeBadGatewayException :: Newtype BadGatewayException _


-- | Placeholder documentation for BadRequestException
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


-- | Placeholder documentation for BlackoutSlate
newtype BlackoutSlate = BlackoutSlate 
  { "BlackoutSlateImage" :: NullOrUndefined (InputLocation)
  , "NetworkEndBlackout" :: NullOrUndefined (BlackoutSlateNetworkEndBlackout)
  , "NetworkEndBlackoutImage" :: NullOrUndefined (InputLocation)
  , "NetworkId" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (BlackoutSlateState)
  }
derive instance newtypeBlackoutSlate :: Newtype BlackoutSlate _


-- | Placeholder documentation for BlackoutSlateNetworkEndBlackout
newtype BlackoutSlateNetworkEndBlackout = BlackoutSlateNetworkEndBlackout String
derive instance newtypeBlackoutSlateNetworkEndBlackout :: Newtype BlackoutSlateNetworkEndBlackout _


-- | Placeholder documentation for BlackoutSlateState
newtype BlackoutSlateState = BlackoutSlateState String
derive instance newtypeBlackoutSlateState :: Newtype BlackoutSlateState _


-- | Placeholder documentation for BurnInAlignment
newtype BurnInAlignment = BurnInAlignment String
derive instance newtypeBurnInAlignment :: Newtype BurnInAlignment _


-- | Placeholder documentation for BurnInBackgroundColor
newtype BurnInBackgroundColor = BurnInBackgroundColor String
derive instance newtypeBurnInBackgroundColor :: Newtype BurnInBackgroundColor _


-- | Placeholder documentation for BurnInDestinationSettings
newtype BurnInDestinationSettings = BurnInDestinationSettings 
  { "Alignment" :: NullOrUndefined (BurnInAlignment)
  , "BackgroundColor" :: NullOrUndefined (BurnInBackgroundColor)
  , "BackgroundOpacity" :: NullOrUndefined (Int)
  , "Font" :: NullOrUndefined (InputLocation)
  , "FontColor" :: NullOrUndefined (BurnInFontColor)
  , "FontOpacity" :: NullOrUndefined (Int)
  , "FontResolution" :: NullOrUndefined (Int)
  , "FontSize" :: NullOrUndefined (String)
  , "OutlineColor" :: NullOrUndefined (BurnInOutlineColor)
  , "OutlineSize" :: NullOrUndefined (Int)
  , "ShadowColor" :: NullOrUndefined (BurnInShadowColor)
  , "ShadowOpacity" :: NullOrUndefined (Int)
  , "ShadowXOffset" :: NullOrUndefined (Int)
  , "ShadowYOffset" :: NullOrUndefined (Int)
  , "TeletextGridControl" :: NullOrUndefined (BurnInTeletextGridControl)
  , "XPosition" :: NullOrUndefined (Int)
  , "YPosition" :: NullOrUndefined (Int)
  }
derive instance newtypeBurnInDestinationSettings :: Newtype BurnInDestinationSettings _


-- | Placeholder documentation for BurnInFontColor
newtype BurnInFontColor = BurnInFontColor String
derive instance newtypeBurnInFontColor :: Newtype BurnInFontColor _


-- | Placeholder documentation for BurnInOutlineColor
newtype BurnInOutlineColor = BurnInOutlineColor String
derive instance newtypeBurnInOutlineColor :: Newtype BurnInOutlineColor _


-- | Placeholder documentation for BurnInShadowColor
newtype BurnInShadowColor = BurnInShadowColor String
derive instance newtypeBurnInShadowColor :: Newtype BurnInShadowColor _


-- | Placeholder documentation for BurnInTeletextGridControl
newtype BurnInTeletextGridControl = BurnInTeletextGridControl String
derive instance newtypeBurnInTeletextGridControl :: Newtype BurnInTeletextGridControl _


-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
newtype CaptionDescription = CaptionDescription 
  { "CaptionSelectorName" :: NullOrUndefined (String)
  , "DestinationSettings" :: NullOrUndefined (CaptionDestinationSettings)
  , "LanguageCode" :: NullOrUndefined (String)
  , "LanguageDescription" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeCaptionDescription :: Newtype CaptionDescription _


-- | Placeholder documentation for CaptionDestinationSettings
newtype CaptionDestinationSettings = CaptionDestinationSettings 
  { "AribDestinationSettings" :: NullOrUndefined (AribDestinationSettings)
  , "BurnInDestinationSettings" :: NullOrUndefined (BurnInDestinationSettings)
  , "DvbSubDestinationSettings" :: NullOrUndefined (DvbSubDestinationSettings)
  , "EmbeddedDestinationSettings" :: NullOrUndefined (EmbeddedDestinationSettings)
  , "EmbeddedPlusScte20DestinationSettings" :: NullOrUndefined (EmbeddedPlusScte20DestinationSettings)
  , "Scte20PlusEmbeddedDestinationSettings" :: NullOrUndefined (Scte20PlusEmbeddedDestinationSettings)
  , "Scte27DestinationSettings" :: NullOrUndefined (Scte27DestinationSettings)
  , "SmpteTtDestinationSettings" :: NullOrUndefined (SmpteTtDestinationSettings)
  , "TeletextDestinationSettings" :: NullOrUndefined (TeletextDestinationSettings)
  , "TtmlDestinationSettings" :: NullOrUndefined (TtmlDestinationSettings)
  , "WebvttDestinationSettings" :: NullOrUndefined (WebvttDestinationSettings)
  }
derive instance newtypeCaptionDestinationSettings :: Newtype CaptionDestinationSettings _


-- | Maps a caption channel to an ISO 693-2 language code (http://www.loc.gov/standards/iso639-2), with an optional description.
newtype CaptionLanguageMapping = CaptionLanguageMapping 
  { "CaptionChannel" :: NullOrUndefined (Int)
  , "LanguageCode" :: NullOrUndefined (String)
  , "LanguageDescription" :: NullOrUndefined (String)
  }
derive instance newtypeCaptionLanguageMapping :: Newtype CaptionLanguageMapping _


-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
newtype CaptionSelector = CaptionSelector 
  { "LanguageCode" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "SelectorSettings" :: NullOrUndefined (CaptionSelectorSettings)
  }
derive instance newtypeCaptionSelector :: Newtype CaptionSelector _


-- | Placeholder documentation for CaptionSelectorSettings
newtype CaptionSelectorSettings = CaptionSelectorSettings 
  { "AribSourceSettings" :: NullOrUndefined (AribSourceSettings)
  , "DvbSubSourceSettings" :: NullOrUndefined (DvbSubSourceSettings)
  , "EmbeddedSourceSettings" :: NullOrUndefined (EmbeddedSourceSettings)
  , "Scte20SourceSettings" :: NullOrUndefined (Scte20SourceSettings)
  , "Scte27SourceSettings" :: NullOrUndefined (Scte27SourceSettings)
  , "TeletextSourceSettings" :: NullOrUndefined (TeletextSourceSettings)
  }
derive instance newtypeCaptionSelectorSettings :: Newtype CaptionSelectorSettings _


-- | Placeholder documentation for Channel
newtype Channel = Channel 
  { "Arn" :: NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (ChannelState)
  }
derive instance newtypeChannel :: Newtype Channel _


-- | Placeholder documentation for ChannelConfigurationValidationError
newtype ChannelConfigurationValidationError = ChannelConfigurationValidationError 
  { "Message" :: NullOrUndefined (String)
  , "ValidationErrors" :: NullOrUndefined (ListOfValidationError)
  }
derive instance newtypeChannelConfigurationValidationError :: Newtype ChannelConfigurationValidationError _


-- | Placeholder documentation for ChannelEgressEndpoint
newtype ChannelEgressEndpoint = ChannelEgressEndpoint 
  { "SourceIp" :: NullOrUndefined (String)
  }
derive instance newtypeChannelEgressEndpoint :: Newtype ChannelEgressEndpoint _


-- | Placeholder documentation for ChannelState
newtype ChannelState = ChannelState String
derive instance newtypeChannelState :: Newtype ChannelState _


-- | Placeholder documentation for ChannelSummary
newtype ChannelSummary = ChannelSummary 
  { "Arn" :: NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint)
  , "Id" :: NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (ChannelState)
  }
derive instance newtypeChannelSummary :: Newtype ChannelSummary _


-- | Placeholder documentation for ConflictException
newtype ConflictException = ConflictException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _


-- | Placeholder documentation for CreateChannel
newtype CreateChannel = CreateChannel 
  { "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "InputAttachments" :: NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "Reserved" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }
derive instance newtypeCreateChannel :: Newtype CreateChannel _


-- | A request to create a channel
newtype CreateChannelRequest = CreateChannelRequest 
  { "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "InputAttachments" :: NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "Reserved" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }
derive instance newtypeCreateChannelRequest :: Newtype CreateChannelRequest _


-- | Placeholder documentation for CreateChannelResponse
newtype CreateChannelResponse = CreateChannelResponse 
  { "Channel" :: NullOrUndefined (Channel)
  }
derive instance newtypeCreateChannelResponse :: Newtype CreateChannelResponse _


-- | Placeholder documentation for CreateChannelResultModel
newtype CreateChannelResultModel = CreateChannelResultModel 
  { "Channel" :: NullOrUndefined (Channel)
  }
derive instance newtypeCreateChannelResultModel :: Newtype CreateChannelResultModel _


-- | Placeholder documentation for CreateInput
newtype CreateInput = CreateInput 
  { "Destinations" :: NullOrUndefined (ListOfInputDestinationRequest)
  , "InputSecurityGroups" :: NullOrUndefined (ListOf__string)
  , "Name" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "Sources" :: NullOrUndefined (ListOfInputSourceRequest)
  , "Type" :: NullOrUndefined (InputType)
  }
derive instance newtypeCreateInput :: Newtype CreateInput _


-- | The name of the input
newtype CreateInputRequest = CreateInputRequest 
  { "Destinations" :: NullOrUndefined (ListOfInputDestinationRequest)
  , "InputSecurityGroups" :: NullOrUndefined (ListOf__string)
  , "Name" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "Sources" :: NullOrUndefined (ListOfInputSourceRequest)
  , "Type" :: NullOrUndefined (InputType)
  }
derive instance newtypeCreateInputRequest :: Newtype CreateInputRequest _


-- | Placeholder documentation for CreateInputResponse
newtype CreateInputResponse = CreateInputResponse 
  { "Input" :: NullOrUndefined (Input)
  }
derive instance newtypeCreateInputResponse :: Newtype CreateInputResponse _


-- | Placeholder documentation for CreateInputResultModel
newtype CreateInputResultModel = CreateInputResultModel 
  { "Input" :: NullOrUndefined (Input)
  }
derive instance newtypeCreateInputResultModel :: Newtype CreateInputResultModel _


-- | The IPv4 CIDRs to whitelist for this Input Security Group
newtype CreateInputSecurityGroupRequest = CreateInputSecurityGroupRequest 
  { "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRuleCidr)
  }
derive instance newtypeCreateInputSecurityGroupRequest :: Newtype CreateInputSecurityGroupRequest _


-- | Placeholder documentation for CreateInputSecurityGroupResponse
newtype CreateInputSecurityGroupResponse = CreateInputSecurityGroupResponse 
  { "SecurityGroup" :: NullOrUndefined (InputSecurityGroup)
  }
derive instance newtypeCreateInputSecurityGroupResponse :: Newtype CreateInputSecurityGroupResponse _


-- | Placeholder documentation for CreateInputSecurityGroupResultModel
newtype CreateInputSecurityGroupResultModel = CreateInputSecurityGroupResultModel 
  { "SecurityGroup" :: NullOrUndefined (InputSecurityGroup)
  }
derive instance newtypeCreateInputSecurityGroupResultModel :: Newtype CreateInputSecurityGroupResultModel _


-- | Placeholder documentation for DeleteChannelRequest
newtype DeleteChannelRequest = DeleteChannelRequest 
  { "ChannelId" :: (String)
  }
derive instance newtypeDeleteChannelRequest :: Newtype DeleteChannelRequest _


-- | Placeholder documentation for DeleteChannelResponse
newtype DeleteChannelResponse = DeleteChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (ChannelState)
  }
derive instance newtypeDeleteChannelResponse :: Newtype DeleteChannelResponse _


-- | Placeholder documentation for DeleteInputRequest
newtype DeleteInputRequest = DeleteInputRequest 
  { "InputId" :: (String)
  }
derive instance newtypeDeleteInputRequest :: Newtype DeleteInputRequest _


-- | Placeholder documentation for DeleteInputResponse
newtype DeleteInputResponse = DeleteInputResponse 
  { 
  }
derive instance newtypeDeleteInputResponse :: Newtype DeleteInputResponse _


-- | Placeholder documentation for DeleteInputSecurityGroupRequest
newtype DeleteInputSecurityGroupRequest = DeleteInputSecurityGroupRequest 
  { "InputSecurityGroupId" :: (String)
  }
derive instance newtypeDeleteInputSecurityGroupRequest :: Newtype DeleteInputSecurityGroupRequest _


-- | Placeholder documentation for DeleteInputSecurityGroupResponse
newtype DeleteInputSecurityGroupResponse = DeleteInputSecurityGroupResponse 
  { 
  }
derive instance newtypeDeleteInputSecurityGroupResponse :: Newtype DeleteInputSecurityGroupResponse _


-- | Placeholder documentation for DescribeChannelRequest
newtype DescribeChannelRequest = DescribeChannelRequest 
  { "ChannelId" :: (String)
  }
derive instance newtypeDescribeChannelRequest :: Newtype DescribeChannelRequest _


-- | Placeholder documentation for DescribeChannelResponse
newtype DescribeChannelResponse = DescribeChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (ChannelState)
  }
derive instance newtypeDescribeChannelResponse :: Newtype DescribeChannelResponse _


-- | Placeholder documentation for DescribeInputRequest
newtype DescribeInputRequest = DescribeInputRequest 
  { "InputId" :: (String)
  }
derive instance newtypeDescribeInputRequest :: Newtype DescribeInputRequest _


-- | Placeholder documentation for DescribeInputResponse
newtype DescribeInputResponse = DescribeInputResponse 
  { "Arn" :: NullOrUndefined (String)
  , "AttachedChannels" :: NullOrUndefined (ListOf__string)
  , "Destinations" :: NullOrUndefined (ListOfInputDestination)
  , "Id" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "SecurityGroups" :: NullOrUndefined (ListOf__string)
  , "Sources" :: NullOrUndefined (ListOfInputSource)
  , "State" :: NullOrUndefined (InputState)
  , "Type" :: NullOrUndefined (InputType)
  }
derive instance newtypeDescribeInputResponse :: Newtype DescribeInputResponse _


-- | Placeholder documentation for DescribeInputSecurityGroupRequest
newtype DescribeInputSecurityGroupRequest = DescribeInputSecurityGroupRequest 
  { "InputSecurityGroupId" :: (String)
  }
derive instance newtypeDescribeInputSecurityGroupRequest :: Newtype DescribeInputSecurityGroupRequest _


-- | Placeholder documentation for DescribeInputSecurityGroupResponse
newtype DescribeInputSecurityGroupResponse = DescribeInputSecurityGroupResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRule)
  }
derive instance newtypeDescribeInputSecurityGroupResponse :: Newtype DescribeInputSecurityGroupResponse _


-- | DVB Network Information Table (NIT)
newtype DvbNitSettings = DvbNitSettings 
  { "NetworkId" :: NullOrUndefined (Int)
  , "NetworkName" :: NullOrUndefined (String)
  , "RepInterval" :: NullOrUndefined (Int)
  }
derive instance newtypeDvbNitSettings :: Newtype DvbNitSettings _


-- | Placeholder documentation for DvbSdtOutputSdt
newtype DvbSdtOutputSdt = DvbSdtOutputSdt String
derive instance newtypeDvbSdtOutputSdt :: Newtype DvbSdtOutputSdt _


-- | DVB Service Description Table (SDT)
newtype DvbSdtSettings = DvbSdtSettings 
  { "OutputSdt" :: NullOrUndefined (DvbSdtOutputSdt)
  , "RepInterval" :: NullOrUndefined (Int)
  , "ServiceName" :: NullOrUndefined (String)
  , "ServiceProviderName" :: NullOrUndefined (String)
  }
derive instance newtypeDvbSdtSettings :: Newtype DvbSdtSettings _


-- | Placeholder documentation for DvbSubDestinationAlignment
newtype DvbSubDestinationAlignment = DvbSubDestinationAlignment String
derive instance newtypeDvbSubDestinationAlignment :: Newtype DvbSubDestinationAlignment _


-- | Placeholder documentation for DvbSubDestinationBackgroundColor
newtype DvbSubDestinationBackgroundColor = DvbSubDestinationBackgroundColor String
derive instance newtypeDvbSubDestinationBackgroundColor :: Newtype DvbSubDestinationBackgroundColor _


-- | Placeholder documentation for DvbSubDestinationFontColor
newtype DvbSubDestinationFontColor = DvbSubDestinationFontColor String
derive instance newtypeDvbSubDestinationFontColor :: Newtype DvbSubDestinationFontColor _


-- | Placeholder documentation for DvbSubDestinationOutlineColor
newtype DvbSubDestinationOutlineColor = DvbSubDestinationOutlineColor String
derive instance newtypeDvbSubDestinationOutlineColor :: Newtype DvbSubDestinationOutlineColor _


-- | Placeholder documentation for DvbSubDestinationSettings
newtype DvbSubDestinationSettings = DvbSubDestinationSettings 
  { "Alignment" :: NullOrUndefined (DvbSubDestinationAlignment)
  , "BackgroundColor" :: NullOrUndefined (DvbSubDestinationBackgroundColor)
  , "BackgroundOpacity" :: NullOrUndefined (Int)
  , "Font" :: NullOrUndefined (InputLocation)
  , "FontColor" :: NullOrUndefined (DvbSubDestinationFontColor)
  , "FontOpacity" :: NullOrUndefined (Int)
  , "FontResolution" :: NullOrUndefined (Int)
  , "FontSize" :: NullOrUndefined (String)
  , "OutlineColor" :: NullOrUndefined (DvbSubDestinationOutlineColor)
  , "OutlineSize" :: NullOrUndefined (Int)
  , "ShadowColor" :: NullOrUndefined (DvbSubDestinationShadowColor)
  , "ShadowOpacity" :: NullOrUndefined (Int)
  , "ShadowXOffset" :: NullOrUndefined (Int)
  , "ShadowYOffset" :: NullOrUndefined (Int)
  , "TeletextGridControl" :: NullOrUndefined (DvbSubDestinationTeletextGridControl)
  , "XPosition" :: NullOrUndefined (Int)
  , "YPosition" :: NullOrUndefined (Int)
  }
derive instance newtypeDvbSubDestinationSettings :: Newtype DvbSubDestinationSettings _


-- | Placeholder documentation for DvbSubDestinationShadowColor
newtype DvbSubDestinationShadowColor = DvbSubDestinationShadowColor String
derive instance newtypeDvbSubDestinationShadowColor :: Newtype DvbSubDestinationShadowColor _


-- | Placeholder documentation for DvbSubDestinationTeletextGridControl
newtype DvbSubDestinationTeletextGridControl = DvbSubDestinationTeletextGridControl String
derive instance newtypeDvbSubDestinationTeletextGridControl :: Newtype DvbSubDestinationTeletextGridControl _


-- | Placeholder documentation for DvbSubSourceSettings
newtype DvbSubSourceSettings = DvbSubSourceSettings 
  { "Pid" :: NullOrUndefined (Int)
  }
derive instance newtypeDvbSubSourceSettings :: Newtype DvbSubSourceSettings _


-- | DVB Time and Date Table (SDT)
newtype DvbTdtSettings = DvbTdtSettings 
  { "RepInterval" :: NullOrUndefined (Int)
  }
derive instance newtypeDvbTdtSettings :: Newtype DvbTdtSettings _


-- | Placeholder documentation for Eac3AttenuationControl
newtype Eac3AttenuationControl = Eac3AttenuationControl String
derive instance newtypeEac3AttenuationControl :: Newtype Eac3AttenuationControl _


-- | Placeholder documentation for Eac3BitstreamMode
newtype Eac3BitstreamMode = Eac3BitstreamMode String
derive instance newtypeEac3BitstreamMode :: Newtype Eac3BitstreamMode _


-- | Placeholder documentation for Eac3CodingMode
newtype Eac3CodingMode = Eac3CodingMode String
derive instance newtypeEac3CodingMode :: Newtype Eac3CodingMode _


-- | Placeholder documentation for Eac3DcFilter
newtype Eac3DcFilter = Eac3DcFilter String
derive instance newtypeEac3DcFilter :: Newtype Eac3DcFilter _


-- | Placeholder documentation for Eac3DrcLine
newtype Eac3DrcLine = Eac3DrcLine String
derive instance newtypeEac3DrcLine :: Newtype Eac3DrcLine _


-- | Placeholder documentation for Eac3DrcRf
newtype Eac3DrcRf = Eac3DrcRf String
derive instance newtypeEac3DrcRf :: Newtype Eac3DrcRf _


-- | Placeholder documentation for Eac3LfeControl
newtype Eac3LfeControl = Eac3LfeControl String
derive instance newtypeEac3LfeControl :: Newtype Eac3LfeControl _


-- | Placeholder documentation for Eac3LfeFilter
newtype Eac3LfeFilter = Eac3LfeFilter String
derive instance newtypeEac3LfeFilter :: Newtype Eac3LfeFilter _


-- | Placeholder documentation for Eac3MetadataControl
newtype Eac3MetadataControl = Eac3MetadataControl String
derive instance newtypeEac3MetadataControl :: Newtype Eac3MetadataControl _


-- | Placeholder documentation for Eac3PassthroughControl
newtype Eac3PassthroughControl = Eac3PassthroughControl String
derive instance newtypeEac3PassthroughControl :: Newtype Eac3PassthroughControl _


-- | Placeholder documentation for Eac3PhaseControl
newtype Eac3PhaseControl = Eac3PhaseControl String
derive instance newtypeEac3PhaseControl :: Newtype Eac3PhaseControl _


-- | Placeholder documentation for Eac3Settings
newtype Eac3Settings = Eac3Settings 
  { "AttenuationControl" :: NullOrUndefined (Eac3AttenuationControl)
  , "Bitrate" :: NullOrUndefined (Number)
  , "BitstreamMode" :: NullOrUndefined (Eac3BitstreamMode)
  , "CodingMode" :: NullOrUndefined (Eac3CodingMode)
  , "DcFilter" :: NullOrUndefined (Eac3DcFilter)
  , "Dialnorm" :: NullOrUndefined (Int)
  , "DrcLine" :: NullOrUndefined (Eac3DrcLine)
  , "DrcRf" :: NullOrUndefined (Eac3DrcRf)
  , "LfeControl" :: NullOrUndefined (Eac3LfeControl)
  , "LfeFilter" :: NullOrUndefined (Eac3LfeFilter)
  , "LoRoCenterMixLevel" :: NullOrUndefined (Number)
  , "LoRoSurroundMixLevel" :: NullOrUndefined (Number)
  , "LtRtCenterMixLevel" :: NullOrUndefined (Number)
  , "LtRtSurroundMixLevel" :: NullOrUndefined (Number)
  , "MetadataControl" :: NullOrUndefined (Eac3MetadataControl)
  , "PassthroughControl" :: NullOrUndefined (Eac3PassthroughControl)
  , "PhaseControl" :: NullOrUndefined (Eac3PhaseControl)
  , "StereoDownmix" :: NullOrUndefined (Eac3StereoDownmix)
  , "SurroundExMode" :: NullOrUndefined (Eac3SurroundExMode)
  , "SurroundMode" :: NullOrUndefined (Eac3SurroundMode)
  }
derive instance newtypeEac3Settings :: Newtype Eac3Settings _


-- | Placeholder documentation for Eac3StereoDownmix
newtype Eac3StereoDownmix = Eac3StereoDownmix String
derive instance newtypeEac3StereoDownmix :: Newtype Eac3StereoDownmix _


-- | Placeholder documentation for Eac3SurroundExMode
newtype Eac3SurroundExMode = Eac3SurroundExMode String
derive instance newtypeEac3SurroundExMode :: Newtype Eac3SurroundExMode _


-- | Placeholder documentation for Eac3SurroundMode
newtype Eac3SurroundMode = Eac3SurroundMode String
derive instance newtypeEac3SurroundMode :: Newtype Eac3SurroundMode _


-- | Placeholder documentation for EmbeddedConvert608To708
newtype EmbeddedConvert608To708 = EmbeddedConvert608To708 String
derive instance newtypeEmbeddedConvert608To708 :: Newtype EmbeddedConvert608To708 _


-- | Placeholder documentation for EmbeddedDestinationSettings
newtype EmbeddedDestinationSettings = EmbeddedDestinationSettings 
  { 
  }
derive instance newtypeEmbeddedDestinationSettings :: Newtype EmbeddedDestinationSettings _


-- | Placeholder documentation for EmbeddedPlusScte20DestinationSettings
newtype EmbeddedPlusScte20DestinationSettings = EmbeddedPlusScte20DestinationSettings 
  { 
  }
derive instance newtypeEmbeddedPlusScte20DestinationSettings :: Newtype EmbeddedPlusScte20DestinationSettings _


-- | Placeholder documentation for EmbeddedScte20Detection
newtype EmbeddedScte20Detection = EmbeddedScte20Detection String
derive instance newtypeEmbeddedScte20Detection :: Newtype EmbeddedScte20Detection _


-- | Placeholder documentation for EmbeddedSourceSettings
newtype EmbeddedSourceSettings = EmbeddedSourceSettings 
  { "Convert608To708" :: NullOrUndefined (EmbeddedConvert608To708)
  , "Scte20Detection" :: NullOrUndefined (EmbeddedScte20Detection)
  , "Source608ChannelNumber" :: NullOrUndefined (Int)
  , "Source608TrackNumber" :: NullOrUndefined (Int)
  }
derive instance newtypeEmbeddedSourceSettings :: Newtype EmbeddedSourceSettings _


-- | Placeholder documentation for Empty
newtype Empty = Empty 
  { 
  }
derive instance newtypeEmpty :: Newtype Empty _


-- | Placeholder documentation for EncoderSettings
newtype EncoderSettings = EncoderSettings 
  { "AudioDescriptions" :: NullOrUndefined (ListOfAudioDescription)
  , "AvailBlanking" :: NullOrUndefined (AvailBlanking)
  , "AvailConfiguration" :: NullOrUndefined (AvailConfiguration)
  , "BlackoutSlate" :: NullOrUndefined (BlackoutSlate)
  , "CaptionDescriptions" :: NullOrUndefined (ListOfCaptionDescription)
  , "GlobalConfiguration" :: NullOrUndefined (GlobalConfiguration)
  , "OutputGroups" :: NullOrUndefined (ListOfOutputGroup)
  , "TimecodeConfig" :: NullOrUndefined (TimecodeConfig)
  , "VideoDescriptions" :: NullOrUndefined (ListOfVideoDescription)
  }
derive instance newtypeEncoderSettings :: Newtype EncoderSettings _


-- | Placeholder documentation for FecOutputIncludeFec
newtype FecOutputIncludeFec = FecOutputIncludeFec String
derive instance newtypeFecOutputIncludeFec :: Newtype FecOutputIncludeFec _


-- | Placeholder documentation for FecOutputSettings
newtype FecOutputSettings = FecOutputSettings 
  { "ColumnDepth" :: NullOrUndefined (Int)
  , "IncludeFec" :: NullOrUndefined (FecOutputIncludeFec)
  , "RowLength" :: NullOrUndefined (Int)
  }
derive instance newtypeFecOutputSettings :: Newtype FecOutputSettings _


-- | Placeholder documentation for FixedAfd
newtype FixedAfd = FixedAfd String
derive instance newtypeFixedAfd :: Newtype FixedAfd _


-- | Placeholder documentation for ForbiddenException
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _


-- | Placeholder documentation for GatewayTimeoutException
newtype GatewayTimeoutException = GatewayTimeoutException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeGatewayTimeoutException :: Newtype GatewayTimeoutException _


-- | Placeholder documentation for GlobalConfiguration
newtype GlobalConfiguration = GlobalConfiguration 
  { "InitialAudioGain" :: NullOrUndefined (Int)
  , "InputEndAction" :: NullOrUndefined (GlobalConfigurationInputEndAction)
  , "InputLossBehavior" :: NullOrUndefined (InputLossBehavior)
  , "OutputTimingSource" :: NullOrUndefined (GlobalConfigurationOutputTimingSource)
  , "SupportLowFramerateInputs" :: NullOrUndefined (GlobalConfigurationLowFramerateInputs)
  }
derive instance newtypeGlobalConfiguration :: Newtype GlobalConfiguration _


-- | Placeholder documentation for GlobalConfigurationInputEndAction
newtype GlobalConfigurationInputEndAction = GlobalConfigurationInputEndAction String
derive instance newtypeGlobalConfigurationInputEndAction :: Newtype GlobalConfigurationInputEndAction _


-- | Placeholder documentation for GlobalConfigurationLowFramerateInputs
newtype GlobalConfigurationLowFramerateInputs = GlobalConfigurationLowFramerateInputs String
derive instance newtypeGlobalConfigurationLowFramerateInputs :: Newtype GlobalConfigurationLowFramerateInputs _


-- | Placeholder documentation for GlobalConfigurationOutputTimingSource
newtype GlobalConfigurationOutputTimingSource = GlobalConfigurationOutputTimingSource String
derive instance newtypeGlobalConfigurationOutputTimingSource :: Newtype GlobalConfigurationOutputTimingSource _


-- | Placeholder documentation for H264AdaptiveQuantization
newtype H264AdaptiveQuantization = H264AdaptiveQuantization String
derive instance newtypeH264AdaptiveQuantization :: Newtype H264AdaptiveQuantization _


-- | Placeholder documentation for H264ColorMetadata
newtype H264ColorMetadata = H264ColorMetadata String
derive instance newtypeH264ColorMetadata :: Newtype H264ColorMetadata _


-- | Placeholder documentation for H264EntropyEncoding
newtype H264EntropyEncoding = H264EntropyEncoding String
derive instance newtypeH264EntropyEncoding :: Newtype H264EntropyEncoding _


-- | Placeholder documentation for H264FlickerAq
newtype H264FlickerAq = H264FlickerAq String
derive instance newtypeH264FlickerAq :: Newtype H264FlickerAq _


-- | Placeholder documentation for H264FramerateControl
newtype H264FramerateControl = H264FramerateControl String
derive instance newtypeH264FramerateControl :: Newtype H264FramerateControl _


-- | Placeholder documentation for H264GopBReference
newtype H264GopBReference = H264GopBReference String
derive instance newtypeH264GopBReference :: Newtype H264GopBReference _


-- | Placeholder documentation for H264GopSizeUnits
newtype H264GopSizeUnits = H264GopSizeUnits String
derive instance newtypeH264GopSizeUnits :: Newtype H264GopSizeUnits _


-- | Placeholder documentation for H264Level
newtype H264Level = H264Level String
derive instance newtypeH264Level :: Newtype H264Level _


-- | Placeholder documentation for H264LookAheadRateControl
newtype H264LookAheadRateControl = H264LookAheadRateControl String
derive instance newtypeH264LookAheadRateControl :: Newtype H264LookAheadRateControl _


-- | Placeholder documentation for H264ParControl
newtype H264ParControl = H264ParControl String
derive instance newtypeH264ParControl :: Newtype H264ParControl _


-- | Placeholder documentation for H264Profile
newtype H264Profile = H264Profile String
derive instance newtypeH264Profile :: Newtype H264Profile _


-- | Placeholder documentation for H264RateControlMode
newtype H264RateControlMode = H264RateControlMode String
derive instance newtypeH264RateControlMode :: Newtype H264RateControlMode _


-- | Placeholder documentation for H264ScanType
newtype H264ScanType = H264ScanType String
derive instance newtypeH264ScanType :: Newtype H264ScanType _


-- | Placeholder documentation for H264SceneChangeDetect
newtype H264SceneChangeDetect = H264SceneChangeDetect String
derive instance newtypeH264SceneChangeDetect :: Newtype H264SceneChangeDetect _


-- | Placeholder documentation for H264Settings
newtype H264Settings = H264Settings 
  { "AdaptiveQuantization" :: NullOrUndefined (H264AdaptiveQuantization)
  , "AfdSignaling" :: NullOrUndefined (AfdSignaling)
  , "Bitrate" :: NullOrUndefined (Int)
  , "BufFillPct" :: NullOrUndefined (Int)
  , "BufSize" :: NullOrUndefined (Int)
  , "ColorMetadata" :: NullOrUndefined (H264ColorMetadata)
  , "EntropyEncoding" :: NullOrUndefined (H264EntropyEncoding)
  , "FixedAfd" :: NullOrUndefined (FixedAfd)
  , "FlickerAq" :: NullOrUndefined (H264FlickerAq)
  , "FramerateControl" :: NullOrUndefined (H264FramerateControl)
  , "FramerateDenominator" :: NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined (Int)
  , "GopBReference" :: NullOrUndefined (H264GopBReference)
  , "GopClosedCadence" :: NullOrUndefined (Int)
  , "GopNumBFrames" :: NullOrUndefined (Int)
  , "GopSize" :: NullOrUndefined (Number)
  , "GopSizeUnits" :: NullOrUndefined (H264GopSizeUnits)
  , "Level" :: NullOrUndefined (H264Level)
  , "LookAheadRateControl" :: NullOrUndefined (H264LookAheadRateControl)
  , "MaxBitrate" :: NullOrUndefined (Int)
  , "MinIInterval" :: NullOrUndefined (Int)
  , "NumRefFrames" :: NullOrUndefined (Int)
  , "ParControl" :: NullOrUndefined (H264ParControl)
  , "ParDenominator" :: NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined (Int)
  , "Profile" :: NullOrUndefined (H264Profile)
  , "RateControlMode" :: NullOrUndefined (H264RateControlMode)
  , "ScanType" :: NullOrUndefined (H264ScanType)
  , "SceneChangeDetect" :: NullOrUndefined (H264SceneChangeDetect)
  , "Slices" :: NullOrUndefined (Int)
  , "Softness" :: NullOrUndefined (Int)
  , "SpatialAq" :: NullOrUndefined (H264SpatialAq)
  , "Syntax" :: NullOrUndefined (H264Syntax)
  , "TemporalAq" :: NullOrUndefined (H264TemporalAq)
  , "TimecodeInsertion" :: NullOrUndefined (H264TimecodeInsertionBehavior)
  }
derive instance newtypeH264Settings :: Newtype H264Settings _


-- | Placeholder documentation for H264SpatialAq
newtype H264SpatialAq = H264SpatialAq String
derive instance newtypeH264SpatialAq :: Newtype H264SpatialAq _


-- | Placeholder documentation for H264Syntax
newtype H264Syntax = H264Syntax String
derive instance newtypeH264Syntax :: Newtype H264Syntax _


-- | Placeholder documentation for H264TemporalAq
newtype H264TemporalAq = H264TemporalAq String
derive instance newtypeH264TemporalAq :: Newtype H264TemporalAq _


-- | Placeholder documentation for H264TimecodeInsertionBehavior
newtype H264TimecodeInsertionBehavior = H264TimecodeInsertionBehavior String
derive instance newtypeH264TimecodeInsertionBehavior :: Newtype H264TimecodeInsertionBehavior _


-- | Placeholder documentation for HlsAdMarkers
newtype HlsAdMarkers = HlsAdMarkers String
derive instance newtypeHlsAdMarkers :: Newtype HlsAdMarkers _


-- | Placeholder documentation for HlsAkamaiHttpTransferMode
newtype HlsAkamaiHttpTransferMode = HlsAkamaiHttpTransferMode String
derive instance newtypeHlsAkamaiHttpTransferMode :: Newtype HlsAkamaiHttpTransferMode _


-- | Placeholder documentation for HlsAkamaiSettings
newtype HlsAkamaiSettings = HlsAkamaiSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined (Int)
  , "HttpTransferMode" :: NullOrUndefined (HlsAkamaiHttpTransferMode)
  , "NumRetries" :: NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined (Int)
  , "Salt" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }
derive instance newtypeHlsAkamaiSettings :: Newtype HlsAkamaiSettings _


-- | Placeholder documentation for HlsBasicPutSettings
newtype HlsBasicPutSettings = HlsBasicPutSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined (Int)
  , "NumRetries" :: NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined (Int)
  }
derive instance newtypeHlsBasicPutSettings :: Newtype HlsBasicPutSettings _


-- | Placeholder documentation for HlsCaptionLanguageSetting
newtype HlsCaptionLanguageSetting = HlsCaptionLanguageSetting String
derive instance newtypeHlsCaptionLanguageSetting :: Newtype HlsCaptionLanguageSetting _


-- | Placeholder documentation for HlsCdnSettings
newtype HlsCdnSettings = HlsCdnSettings 
  { "HlsAkamaiSettings" :: NullOrUndefined (HlsAkamaiSettings)
  , "HlsBasicPutSettings" :: NullOrUndefined (HlsBasicPutSettings)
  , "HlsMediaStoreSettings" :: NullOrUndefined (HlsMediaStoreSettings)
  , "HlsWebdavSettings" :: NullOrUndefined (HlsWebdavSettings)
  }
derive instance newtypeHlsCdnSettings :: Newtype HlsCdnSettings _


-- | Placeholder documentation for HlsClientCache
newtype HlsClientCache = HlsClientCache String
derive instance newtypeHlsClientCache :: Newtype HlsClientCache _


-- | Placeholder documentation for HlsCodecSpecification
newtype HlsCodecSpecification = HlsCodecSpecification String
derive instance newtypeHlsCodecSpecification :: Newtype HlsCodecSpecification _


-- | Placeholder documentation for HlsDirectoryStructure
newtype HlsDirectoryStructure = HlsDirectoryStructure String
derive instance newtypeHlsDirectoryStructure :: Newtype HlsDirectoryStructure _


-- | Placeholder documentation for HlsEncryptionType
newtype HlsEncryptionType = HlsEncryptionType String
derive instance newtypeHlsEncryptionType :: Newtype HlsEncryptionType _


-- | Placeholder documentation for HlsGroupSettings
newtype HlsGroupSettings = HlsGroupSettings 
  { "AdMarkers" :: NullOrUndefined (ListOfHlsAdMarkers)
  , "BaseUrlContent" :: NullOrUndefined (String)
  , "BaseUrlManifest" :: NullOrUndefined (String)
  , "CaptionLanguageMappings" :: NullOrUndefined (ListOfCaptionLanguageMapping)
  , "CaptionLanguageSetting" :: NullOrUndefined (HlsCaptionLanguageSetting)
  , "ClientCache" :: NullOrUndefined (HlsClientCache)
  , "CodecSpecification" :: NullOrUndefined (HlsCodecSpecification)
  , "ConstantIv" :: NullOrUndefined (String)
  , "Destination" :: NullOrUndefined (OutputLocationRef)
  , "DirectoryStructure" :: NullOrUndefined (HlsDirectoryStructure)
  , "EncryptionType" :: NullOrUndefined (HlsEncryptionType)
  , "HlsCdnSettings" :: NullOrUndefined (HlsCdnSettings)
  , "IndexNSegments" :: NullOrUndefined (Int)
  , "InputLossAction" :: NullOrUndefined (InputLossActionForHlsOut)
  , "IvInManifest" :: NullOrUndefined (HlsIvInManifest)
  , "IvSource" :: NullOrUndefined (HlsIvSource)
  , "KeepSegments" :: NullOrUndefined (Int)
  , "KeyFormat" :: NullOrUndefined (String)
  , "KeyFormatVersions" :: NullOrUndefined (String)
  , "KeyProviderSettings" :: NullOrUndefined (KeyProviderSettings)
  , "ManifestCompression" :: NullOrUndefined (HlsManifestCompression)
  , "ManifestDurationFormat" :: NullOrUndefined (HlsManifestDurationFormat)
  , "MinSegmentLength" :: NullOrUndefined (Int)
  , "Mode" :: NullOrUndefined (HlsMode)
  , "OutputSelection" :: NullOrUndefined (HlsOutputSelection)
  , "ProgramDateTime" :: NullOrUndefined (HlsProgramDateTime)
  , "ProgramDateTimePeriod" :: NullOrUndefined (Int)
  , "SegmentLength" :: NullOrUndefined (Int)
  , "SegmentationMode" :: NullOrUndefined (HlsSegmentationMode)
  , "SegmentsPerSubdirectory" :: NullOrUndefined (Int)
  , "StreamInfResolution" :: NullOrUndefined (HlsStreamInfResolution)
  , "TimedMetadataId3Frame" :: NullOrUndefined (HlsTimedMetadataId3Frame)
  , "TimedMetadataId3Period" :: NullOrUndefined (Int)
  , "TimestampDeltaMilliseconds" :: NullOrUndefined (Int)
  , "TsFileMode" :: NullOrUndefined (HlsTsFileMode)
  }
derive instance newtypeHlsGroupSettings :: Newtype HlsGroupSettings _


-- | Placeholder documentation for HlsInputSettings
newtype HlsInputSettings = HlsInputSettings 
  { "Bandwidth" :: NullOrUndefined (Int)
  , "BufferSegments" :: NullOrUndefined (Int)
  , "Retries" :: NullOrUndefined (Int)
  , "RetryInterval" :: NullOrUndefined (Int)
  }
derive instance newtypeHlsInputSettings :: Newtype HlsInputSettings _


-- | Placeholder documentation for HlsIvInManifest
newtype HlsIvInManifest = HlsIvInManifest String
derive instance newtypeHlsIvInManifest :: Newtype HlsIvInManifest _


-- | Placeholder documentation for HlsIvSource
newtype HlsIvSource = HlsIvSource String
derive instance newtypeHlsIvSource :: Newtype HlsIvSource _


-- | Placeholder documentation for HlsManifestCompression
newtype HlsManifestCompression = HlsManifestCompression String
derive instance newtypeHlsManifestCompression :: Newtype HlsManifestCompression _


-- | Placeholder documentation for HlsManifestDurationFormat
newtype HlsManifestDurationFormat = HlsManifestDurationFormat String
derive instance newtypeHlsManifestDurationFormat :: Newtype HlsManifestDurationFormat _


-- | Placeholder documentation for HlsMediaStoreSettings
newtype HlsMediaStoreSettings = HlsMediaStoreSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined (Int)
  , "MediaStoreStorageClass" :: NullOrUndefined (HlsMediaStoreStorageClass)
  , "NumRetries" :: NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined (Int)
  }
derive instance newtypeHlsMediaStoreSettings :: Newtype HlsMediaStoreSettings _


-- | Placeholder documentation for HlsMediaStoreStorageClass
newtype HlsMediaStoreStorageClass = HlsMediaStoreStorageClass String
derive instance newtypeHlsMediaStoreStorageClass :: Newtype HlsMediaStoreStorageClass _


-- | Placeholder documentation for HlsMode
newtype HlsMode = HlsMode String
derive instance newtypeHlsMode :: Newtype HlsMode _


-- | Placeholder documentation for HlsOutputSelection
newtype HlsOutputSelection = HlsOutputSelection String
derive instance newtypeHlsOutputSelection :: Newtype HlsOutputSelection _


-- | Placeholder documentation for HlsOutputSettings
newtype HlsOutputSettings = HlsOutputSettings 
  { "HlsSettings" :: NullOrUndefined (HlsSettings)
  , "NameModifier" :: NullOrUndefined (String)
  , "SegmentModifier" :: NullOrUndefined (String)
  }
derive instance newtypeHlsOutputSettings :: Newtype HlsOutputSettings _


-- | Placeholder documentation for HlsProgramDateTime
newtype HlsProgramDateTime = HlsProgramDateTime String
derive instance newtypeHlsProgramDateTime :: Newtype HlsProgramDateTime _


-- | Placeholder documentation for HlsSegmentationMode
newtype HlsSegmentationMode = HlsSegmentationMode String
derive instance newtypeHlsSegmentationMode :: Newtype HlsSegmentationMode _


-- | Placeholder documentation for HlsSettings
newtype HlsSettings = HlsSettings 
  { "AudioOnlyHlsSettings" :: NullOrUndefined (AudioOnlyHlsSettings)
  , "StandardHlsSettings" :: NullOrUndefined (StandardHlsSettings)
  }
derive instance newtypeHlsSettings :: Newtype HlsSettings _


-- | Placeholder documentation for HlsStreamInfResolution
newtype HlsStreamInfResolution = HlsStreamInfResolution String
derive instance newtypeHlsStreamInfResolution :: Newtype HlsStreamInfResolution _


-- | Placeholder documentation for HlsTimedMetadataId3Frame
newtype HlsTimedMetadataId3Frame = HlsTimedMetadataId3Frame String
derive instance newtypeHlsTimedMetadataId3Frame :: Newtype HlsTimedMetadataId3Frame _


-- | Placeholder documentation for HlsTsFileMode
newtype HlsTsFileMode = HlsTsFileMode String
derive instance newtypeHlsTsFileMode :: Newtype HlsTsFileMode _


-- | Placeholder documentation for HlsWebdavHttpTransferMode
newtype HlsWebdavHttpTransferMode = HlsWebdavHttpTransferMode String
derive instance newtypeHlsWebdavHttpTransferMode :: Newtype HlsWebdavHttpTransferMode _


-- | Placeholder documentation for HlsWebdavSettings
newtype HlsWebdavSettings = HlsWebdavSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined (Int)
  , "HttpTransferMode" :: NullOrUndefined (HlsWebdavHttpTransferMode)
  , "NumRetries" :: NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined (Int)
  }
derive instance newtypeHlsWebdavSettings :: Newtype HlsWebdavSettings _


-- | Placeholder documentation for Input
newtype Input = Input 
  { "Arn" :: NullOrUndefined (String)
  , "AttachedChannels" :: NullOrUndefined (ListOf__string)
  , "Destinations" :: NullOrUndefined (ListOfInputDestination)
  , "Id" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "SecurityGroups" :: NullOrUndefined (ListOf__string)
  , "Sources" :: NullOrUndefined (ListOfInputSource)
  , "State" :: NullOrUndefined (InputState)
  , "Type" :: NullOrUndefined (InputType)
  }
derive instance newtypeInput :: Newtype Input _


-- | Placeholder documentation for InputAttachment
newtype InputAttachment = InputAttachment 
  { "InputId" :: NullOrUndefined (String)
  , "InputSettings" :: NullOrUndefined (InputSettings)
  }
derive instance newtypeInputAttachment :: Newtype InputAttachment _


-- | Placeholder documentation for InputChannelLevel
newtype InputChannelLevel = InputChannelLevel 
  { "Gain" :: NullOrUndefined (Int)
  , "InputChannel" :: NullOrUndefined (Int)
  }
derive instance newtypeInputChannelLevel :: Newtype InputChannelLevel _


-- | codec in increasing order of complexity
newtype InputCodec = InputCodec String
derive instance newtypeInputCodec :: Newtype InputCodec _


-- | Placeholder documentation for InputDeblockFilter
newtype InputDeblockFilter = InputDeblockFilter String
derive instance newtypeInputDeblockFilter :: Newtype InputDeblockFilter _


-- | Placeholder documentation for InputDenoiseFilter
newtype InputDenoiseFilter = InputDenoiseFilter String
derive instance newtypeInputDenoiseFilter :: Newtype InputDenoiseFilter _


-- | The settings for a PUSH type input.
newtype InputDestination = InputDestination 
  { "Ip" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  }
derive instance newtypeInputDestination :: Newtype InputDestination _


-- | Endpoint settings for a PUSH type input.
newtype InputDestinationRequest = InputDestinationRequest 
  { "StreamName" :: NullOrUndefined (String)
  }
derive instance newtypeInputDestinationRequest :: Newtype InputDestinationRequest _


-- | Placeholder documentation for InputFilter
newtype InputFilter = InputFilter String
derive instance newtypeInputFilter :: Newtype InputFilter _


-- | Placeholder documentation for InputLocation
newtype InputLocation = InputLocation 
  { "PasswordParam" :: NullOrUndefined (String)
  , "Uri" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }
derive instance newtypeInputLocation :: Newtype InputLocation _


-- | Placeholder documentation for InputLossActionForHlsOut
newtype InputLossActionForHlsOut = InputLossActionForHlsOut String
derive instance newtypeInputLossActionForHlsOut :: Newtype InputLossActionForHlsOut _


-- | Placeholder documentation for InputLossActionForMsSmoothOut
newtype InputLossActionForMsSmoothOut = InputLossActionForMsSmoothOut String
derive instance newtypeInputLossActionForMsSmoothOut :: Newtype InputLossActionForMsSmoothOut _


-- | Placeholder documentation for InputLossActionForUdpOut
newtype InputLossActionForUdpOut = InputLossActionForUdpOut String
derive instance newtypeInputLossActionForUdpOut :: Newtype InputLossActionForUdpOut _


-- | Placeholder documentation for InputLossBehavior
newtype InputLossBehavior = InputLossBehavior 
  { "BlackFrameMsec" :: NullOrUndefined (Int)
  , "InputLossImageColor" :: NullOrUndefined (String)
  , "InputLossImageSlate" :: NullOrUndefined (InputLocation)
  , "InputLossImageType" :: NullOrUndefined (InputLossImageType)
  , "RepeatFrameMsec" :: NullOrUndefined (Int)
  }
derive instance newtypeInputLossBehavior :: Newtype InputLossBehavior _


-- | Placeholder documentation for InputLossImageType
newtype InputLossImageType = InputLossImageType String
derive instance newtypeInputLossImageType :: Newtype InputLossImageType _


-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.
newtype InputMaximumBitrate = InputMaximumBitrate String
derive instance newtypeInputMaximumBitrate :: Newtype InputMaximumBitrate _


-- | Input resolution based on lines of vertical resolution in the input; SD is less than 720 lines, HD is 720 to 1080 lines, UHD is greater than 1080 lines
-- | 
newtype InputResolution = InputResolution String
derive instance newtypeInputResolution :: Newtype InputResolution _


-- | An Input Security Group
newtype InputSecurityGroup = InputSecurityGroup 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRule)
  }
derive instance newtypeInputSecurityGroup :: Newtype InputSecurityGroup _


-- | Request of IPv4 CIDR addresses to whitelist in a security group.
newtype InputSecurityGroupWhitelistRequest = InputSecurityGroupWhitelistRequest 
  { "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRuleCidr)
  }
derive instance newtypeInputSecurityGroupWhitelistRequest :: Newtype InputSecurityGroupWhitelistRequest _


-- | Live Event input parameters. There can be multiple inputs in a single Live Event.
newtype InputSettings = InputSettings 
  { "AudioSelectors" :: NullOrUndefined (ListOfAudioSelector)
  , "CaptionSelectors" :: NullOrUndefined (ListOfCaptionSelector)
  , "DeblockFilter" :: NullOrUndefined (InputDeblockFilter)
  , "DenoiseFilter" :: NullOrUndefined (InputDenoiseFilter)
  , "FilterStrength" :: NullOrUndefined (Int)
  , "InputFilter" :: NullOrUndefined (InputFilter)
  , "NetworkInputSettings" :: NullOrUndefined (NetworkInputSettings)
  , "SourceEndBehavior" :: NullOrUndefined (InputSourceEndBehavior)
  , "VideoSelector" :: NullOrUndefined (VideoSelector)
  }
derive instance newtypeInputSettings :: Newtype InputSettings _


-- | The settings for a PULL type input.
newtype InputSource = InputSource 
  { "PasswordParam" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }
derive instance newtypeInputSource :: Newtype InputSource _


-- | Placeholder documentation for InputSourceEndBehavior
newtype InputSourceEndBehavior = InputSourceEndBehavior String
derive instance newtypeInputSourceEndBehavior :: Newtype InputSourceEndBehavior _


-- | Settings for for a PULL type input.
newtype InputSourceRequest = InputSourceRequest 
  { "PasswordParam" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }
derive instance newtypeInputSourceRequest :: Newtype InputSourceRequest _


-- | Placeholder documentation for InputSpecification
newtype InputSpecification = InputSpecification 
  { "Codec" :: NullOrUndefined (InputCodec)
  , "MaximumBitrate" :: NullOrUndefined (InputMaximumBitrate)
  , "Resolution" :: NullOrUndefined (InputResolution)
  }
derive instance newtypeInputSpecification :: Newtype InputSpecification _


-- | Placeholder documentation for InputState
newtype InputState = InputState String
derive instance newtypeInputState :: Newtype InputState _


-- | Placeholder documentation for InputType
newtype InputType = InputType String
derive instance newtypeInputType :: Newtype InputType _


-- | Whitelist rule
newtype InputWhitelistRule = InputWhitelistRule 
  { "Cidr" :: NullOrUndefined (String)
  }
derive instance newtypeInputWhitelistRule :: Newtype InputWhitelistRule _


-- | An IPv4 CIDR to whitelist.
newtype InputWhitelistRuleCidr = InputWhitelistRuleCidr 
  { "Cidr" :: NullOrUndefined (String)
  }
derive instance newtypeInputWhitelistRuleCidr :: Newtype InputWhitelistRuleCidr _


-- | Placeholder documentation for InternalServerErrorException
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _


-- | Placeholder documentation for InternalServiceError
newtype InternalServiceError = InternalServiceError 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInternalServiceError :: Newtype InternalServiceError _


-- | Placeholder documentation for InvalidRequest
newtype InvalidRequest = InvalidRequest 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidRequest :: Newtype InvalidRequest _


-- | Placeholder documentation for KeyProviderSettings
newtype KeyProviderSettings = KeyProviderSettings 
  { "StaticKeySettings" :: NullOrUndefined (StaticKeySettings)
  }
derive instance newtypeKeyProviderSettings :: Newtype KeyProviderSettings _


-- | Placeholder documentation for LimitExceeded
newtype LimitExceeded = LimitExceeded 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeLimitExceeded :: Newtype LimitExceeded _


-- | Placeholder documentation for ListChannelsRequest
newtype ListChannelsRequest = ListChannelsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListChannelsRequest :: Newtype ListChannelsRequest _


-- | Placeholder documentation for ListChannelsResponse
newtype ListChannelsResponse = ListChannelsResponse 
  { "Channels" :: NullOrUndefined (ListOfChannelSummary)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListChannelsResponse :: Newtype ListChannelsResponse _


-- | Placeholder documentation for ListChannelsResultModel
newtype ListChannelsResultModel = ListChannelsResultModel 
  { "Channels" :: NullOrUndefined (ListOfChannelSummary)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListChannelsResultModel :: Newtype ListChannelsResultModel _


-- | Placeholder documentation for ListInputSecurityGroupsRequest
newtype ListInputSecurityGroupsRequest = ListInputSecurityGroupsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListInputSecurityGroupsRequest :: Newtype ListInputSecurityGroupsRequest _


-- | Placeholder documentation for ListInputSecurityGroupsResponse
newtype ListInputSecurityGroupsResponse = ListInputSecurityGroupsResponse 
  { "InputSecurityGroups" :: NullOrUndefined (ListOfInputSecurityGroup)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListInputSecurityGroupsResponse :: Newtype ListInputSecurityGroupsResponse _


-- | Result of input security group list request
newtype ListInputSecurityGroupsResultModel = ListInputSecurityGroupsResultModel 
  { "InputSecurityGroups" :: NullOrUndefined (ListOfInputSecurityGroup)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListInputSecurityGroupsResultModel :: Newtype ListInputSecurityGroupsResultModel _


-- | Placeholder documentation for ListInputsRequest
newtype ListInputsRequest = ListInputsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListInputsRequest :: Newtype ListInputsRequest _


-- | Placeholder documentation for ListInputsResponse
newtype ListInputsResponse = ListInputsResponse 
  { "Inputs" :: NullOrUndefined (ListOfInput)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListInputsResponse :: Newtype ListInputsResponse _


-- | Placeholder documentation for ListInputsResultModel
newtype ListInputsResultModel = ListInputsResultModel 
  { "Inputs" :: NullOrUndefined (ListOfInput)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListInputsResultModel :: Newtype ListInputsResultModel _


-- | Placeholder documentation for ListOfAudioChannelMapping
newtype ListOfAudioChannelMapping = ListOfAudioChannelMapping (Array AudioChannelMapping)
derive instance newtypeListOfAudioChannelMapping :: Newtype ListOfAudioChannelMapping _


-- | Placeholder documentation for ListOfAudioDescription
newtype ListOfAudioDescription = ListOfAudioDescription (Array AudioDescription)
derive instance newtypeListOfAudioDescription :: Newtype ListOfAudioDescription _


-- | Placeholder documentation for ListOfAudioSelector
newtype ListOfAudioSelector = ListOfAudioSelector (Array AudioSelector)
derive instance newtypeListOfAudioSelector :: Newtype ListOfAudioSelector _


-- | Placeholder documentation for ListOfCaptionDescription
newtype ListOfCaptionDescription = ListOfCaptionDescription (Array CaptionDescription)
derive instance newtypeListOfCaptionDescription :: Newtype ListOfCaptionDescription _


-- | Placeholder documentation for ListOfCaptionLanguageMapping
newtype ListOfCaptionLanguageMapping = ListOfCaptionLanguageMapping (Array CaptionLanguageMapping)
derive instance newtypeListOfCaptionLanguageMapping :: Newtype ListOfCaptionLanguageMapping _


-- | Placeholder documentation for ListOfCaptionSelector
newtype ListOfCaptionSelector = ListOfCaptionSelector (Array CaptionSelector)
derive instance newtypeListOfCaptionSelector :: Newtype ListOfCaptionSelector _


-- | Placeholder documentation for ListOfChannelEgressEndpoint
newtype ListOfChannelEgressEndpoint = ListOfChannelEgressEndpoint (Array ChannelEgressEndpoint)
derive instance newtypeListOfChannelEgressEndpoint :: Newtype ListOfChannelEgressEndpoint _


-- | Placeholder documentation for ListOfChannelSummary
newtype ListOfChannelSummary = ListOfChannelSummary (Array ChannelSummary)
derive instance newtypeListOfChannelSummary :: Newtype ListOfChannelSummary _


-- | Placeholder documentation for ListOfHlsAdMarkers
newtype ListOfHlsAdMarkers = ListOfHlsAdMarkers (Array HlsAdMarkers)
derive instance newtypeListOfHlsAdMarkers :: Newtype ListOfHlsAdMarkers _


-- | Placeholder documentation for ListOfInput
newtype ListOfInput = ListOfInput (Array Input)
derive instance newtypeListOfInput :: Newtype ListOfInput _


-- | Placeholder documentation for ListOfInputAttachment
newtype ListOfInputAttachment = ListOfInputAttachment (Array InputAttachment)
derive instance newtypeListOfInputAttachment :: Newtype ListOfInputAttachment _


-- | Placeholder documentation for ListOfInputChannelLevel
newtype ListOfInputChannelLevel = ListOfInputChannelLevel (Array InputChannelLevel)
derive instance newtypeListOfInputChannelLevel :: Newtype ListOfInputChannelLevel _


-- | Placeholder documentation for ListOfInputDestination
newtype ListOfInputDestination = ListOfInputDestination (Array InputDestination)
derive instance newtypeListOfInputDestination :: Newtype ListOfInputDestination _


-- | Placeholder documentation for ListOfInputDestinationRequest
newtype ListOfInputDestinationRequest = ListOfInputDestinationRequest (Array InputDestinationRequest)
derive instance newtypeListOfInputDestinationRequest :: Newtype ListOfInputDestinationRequest _


-- | Placeholder documentation for ListOfInputSecurityGroup
newtype ListOfInputSecurityGroup = ListOfInputSecurityGroup (Array InputSecurityGroup)
derive instance newtypeListOfInputSecurityGroup :: Newtype ListOfInputSecurityGroup _


-- | Placeholder documentation for ListOfInputSource
newtype ListOfInputSource = ListOfInputSource (Array InputSource)
derive instance newtypeListOfInputSource :: Newtype ListOfInputSource _


-- | Placeholder documentation for ListOfInputSourceRequest
newtype ListOfInputSourceRequest = ListOfInputSourceRequest (Array InputSourceRequest)
derive instance newtypeListOfInputSourceRequest :: Newtype ListOfInputSourceRequest _


-- | Placeholder documentation for ListOfInputWhitelistRule
newtype ListOfInputWhitelistRule = ListOfInputWhitelistRule (Array InputWhitelistRule)
derive instance newtypeListOfInputWhitelistRule :: Newtype ListOfInputWhitelistRule _


-- | Placeholder documentation for ListOfInputWhitelistRuleCidr
newtype ListOfInputWhitelistRuleCidr = ListOfInputWhitelistRuleCidr (Array InputWhitelistRuleCidr)
derive instance newtypeListOfInputWhitelistRuleCidr :: Newtype ListOfInputWhitelistRuleCidr _


-- | Placeholder documentation for ListOfOutput
newtype ListOfOutput = ListOfOutput (Array Output)
derive instance newtypeListOfOutput :: Newtype ListOfOutput _


-- | Placeholder documentation for ListOfOutputDestination
newtype ListOfOutputDestination = ListOfOutputDestination (Array OutputDestination)
derive instance newtypeListOfOutputDestination :: Newtype ListOfOutputDestination _


-- | Placeholder documentation for ListOfOutputDestinationSettings
newtype ListOfOutputDestinationSettings = ListOfOutputDestinationSettings (Array OutputDestinationSettings)
derive instance newtypeListOfOutputDestinationSettings :: Newtype ListOfOutputDestinationSettings _


-- | Placeholder documentation for ListOfOutputGroup
newtype ListOfOutputGroup = ListOfOutputGroup (Array OutputGroup)
derive instance newtypeListOfOutputGroup :: Newtype ListOfOutputGroup _


-- | Placeholder documentation for ListOfValidationError
newtype ListOfValidationError = ListOfValidationError (Array ValidationError)
derive instance newtypeListOfValidationError :: Newtype ListOfValidationError _


-- | Placeholder documentation for ListOfVideoDescription
newtype ListOfVideoDescription = ListOfVideoDescription (Array VideoDescription)
derive instance newtypeListOfVideoDescription :: Newtype ListOfVideoDescription _


-- | Placeholder documentation for ListOf__string
newtype ListOf__string = ListOf__string (Array String)
derive instance newtypeListOf__string :: Newtype ListOf__string _


-- | Placeholder documentation for M2tsAbsentInputAudioBehavior
newtype M2tsAbsentInputAudioBehavior = M2tsAbsentInputAudioBehavior String
derive instance newtypeM2tsAbsentInputAudioBehavior :: Newtype M2tsAbsentInputAudioBehavior _


-- | Placeholder documentation for M2tsArib
newtype M2tsArib = M2tsArib String
derive instance newtypeM2tsArib :: Newtype M2tsArib _


-- | Placeholder documentation for M2tsAribCaptionsPidControl
newtype M2tsAribCaptionsPidControl = M2tsAribCaptionsPidControl String
derive instance newtypeM2tsAribCaptionsPidControl :: Newtype M2tsAribCaptionsPidControl _


-- | Placeholder documentation for M2tsAudioBufferModel
newtype M2tsAudioBufferModel = M2tsAudioBufferModel String
derive instance newtypeM2tsAudioBufferModel :: Newtype M2tsAudioBufferModel _


-- | Placeholder documentation for M2tsAudioInterval
newtype M2tsAudioInterval = M2tsAudioInterval String
derive instance newtypeM2tsAudioInterval :: Newtype M2tsAudioInterval _


-- | Placeholder documentation for M2tsAudioStreamType
newtype M2tsAudioStreamType = M2tsAudioStreamType String
derive instance newtypeM2tsAudioStreamType :: Newtype M2tsAudioStreamType _


-- | Placeholder documentation for M2tsBufferModel
newtype M2tsBufferModel = M2tsBufferModel String
derive instance newtypeM2tsBufferModel :: Newtype M2tsBufferModel _


-- | Placeholder documentation for M2tsCcDescriptor
newtype M2tsCcDescriptor = M2tsCcDescriptor String
derive instance newtypeM2tsCcDescriptor :: Newtype M2tsCcDescriptor _


-- | Placeholder documentation for M2tsEbifControl
newtype M2tsEbifControl = M2tsEbifControl String
derive instance newtypeM2tsEbifControl :: Newtype M2tsEbifControl _


-- | Placeholder documentation for M2tsEbpPlacement
newtype M2tsEbpPlacement = M2tsEbpPlacement String
derive instance newtypeM2tsEbpPlacement :: Newtype M2tsEbpPlacement _


-- | Placeholder documentation for M2tsEsRateInPes
newtype M2tsEsRateInPes = M2tsEsRateInPes String
derive instance newtypeM2tsEsRateInPes :: Newtype M2tsEsRateInPes _


-- | Placeholder documentation for M2tsKlv
newtype M2tsKlv = M2tsKlv String
derive instance newtypeM2tsKlv :: Newtype M2tsKlv _


-- | Placeholder documentation for M2tsPcrControl
newtype M2tsPcrControl = M2tsPcrControl String
derive instance newtypeM2tsPcrControl :: Newtype M2tsPcrControl _


-- | Placeholder documentation for M2tsRateMode
newtype M2tsRateMode = M2tsRateMode String
derive instance newtypeM2tsRateMode :: Newtype M2tsRateMode _


-- | Placeholder documentation for M2tsScte35Control
newtype M2tsScte35Control = M2tsScte35Control String
derive instance newtypeM2tsScte35Control :: Newtype M2tsScte35Control _


-- | Placeholder documentation for M2tsSegmentationMarkers
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers String
derive instance newtypeM2tsSegmentationMarkers :: Newtype M2tsSegmentationMarkers _


-- | Placeholder documentation for M2tsSegmentationStyle
newtype M2tsSegmentationStyle = M2tsSegmentationStyle String
derive instance newtypeM2tsSegmentationStyle :: Newtype M2tsSegmentationStyle _


-- | Placeholder documentation for M2tsSettings
newtype M2tsSettings = M2tsSettings 
  { "AbsentInputAudioBehavior" :: NullOrUndefined (M2tsAbsentInputAudioBehavior)
  , "Arib" :: NullOrUndefined (M2tsArib)
  , "AribCaptionsPid" :: NullOrUndefined (String)
  , "AribCaptionsPidControl" :: NullOrUndefined (M2tsAribCaptionsPidControl)
  , "AudioBufferModel" :: NullOrUndefined (M2tsAudioBufferModel)
  , "AudioFramesPerPes" :: NullOrUndefined (Int)
  , "AudioPids" :: NullOrUndefined (String)
  , "AudioStreamType" :: NullOrUndefined (M2tsAudioStreamType)
  , "Bitrate" :: NullOrUndefined (Int)
  , "BufferModel" :: NullOrUndefined (M2tsBufferModel)
  , "CcDescriptor" :: NullOrUndefined (M2tsCcDescriptor)
  , "DvbNitSettings" :: NullOrUndefined (DvbNitSettings)
  , "DvbSdtSettings" :: NullOrUndefined (DvbSdtSettings)
  , "DvbSubPids" :: NullOrUndefined (String)
  , "DvbTdtSettings" :: NullOrUndefined (DvbTdtSettings)
  , "DvbTeletextPid" :: NullOrUndefined (String)
  , "Ebif" :: NullOrUndefined (M2tsEbifControl)
  , "EbpAudioInterval" :: NullOrUndefined (M2tsAudioInterval)
  , "EbpLookaheadMs" :: NullOrUndefined (Int)
  , "EbpPlacement" :: NullOrUndefined (M2tsEbpPlacement)
  , "EcmPid" :: NullOrUndefined (String)
  , "EsRateInPes" :: NullOrUndefined (M2tsEsRateInPes)
  , "EtvPlatformPid" :: NullOrUndefined (String)
  , "EtvSignalPid" :: NullOrUndefined (String)
  , "FragmentTime" :: NullOrUndefined (Number)
  , "Klv" :: NullOrUndefined (M2tsKlv)
  , "KlvDataPids" :: NullOrUndefined (String)
  , "NullPacketBitrate" :: NullOrUndefined (Number)
  , "PatInterval" :: NullOrUndefined (Int)
  , "PcrControl" :: NullOrUndefined (M2tsPcrControl)
  , "PcrPeriod" :: NullOrUndefined (Int)
  , "PcrPid" :: NullOrUndefined (String)
  , "PmtInterval" :: NullOrUndefined (Int)
  , "PmtPid" :: NullOrUndefined (String)
  , "ProgramNum" :: NullOrUndefined (Int)
  , "RateMode" :: NullOrUndefined (M2tsRateMode)
  , "Scte27Pids" :: NullOrUndefined (String)
  , "Scte35Control" :: NullOrUndefined (M2tsScte35Control)
  , "Scte35Pid" :: NullOrUndefined (String)
  , "SegmentationMarkers" :: NullOrUndefined (M2tsSegmentationMarkers)
  , "SegmentationStyle" :: NullOrUndefined (M2tsSegmentationStyle)
  , "SegmentationTime" :: NullOrUndefined (Number)
  , "TimedMetadataBehavior" :: NullOrUndefined (M2tsTimedMetadataBehavior)
  , "TimedMetadataPid" :: NullOrUndefined (String)
  , "TransportStreamId" :: NullOrUndefined (Int)
  , "VideoPid" :: NullOrUndefined (String)
  }
derive instance newtypeM2tsSettings :: Newtype M2tsSettings _


-- | Placeholder documentation for M2tsTimedMetadataBehavior
newtype M2tsTimedMetadataBehavior = M2tsTimedMetadataBehavior String
derive instance newtypeM2tsTimedMetadataBehavior :: Newtype M2tsTimedMetadataBehavior _


-- | Placeholder documentation for M3u8PcrControl
newtype M3u8PcrControl = M3u8PcrControl String
derive instance newtypeM3u8PcrControl :: Newtype M3u8PcrControl _


-- | Placeholder documentation for M3u8Scte35Behavior
newtype M3u8Scte35Behavior = M3u8Scte35Behavior String
derive instance newtypeM3u8Scte35Behavior :: Newtype M3u8Scte35Behavior _


-- | Settings information for the .m3u8 container
newtype M3u8Settings = M3u8Settings 
  { "AudioFramesPerPes" :: NullOrUndefined (Int)
  , "AudioPids" :: NullOrUndefined (String)
  , "EcmPid" :: NullOrUndefined (String)
  , "PatInterval" :: NullOrUndefined (Int)
  , "PcrControl" :: NullOrUndefined (M3u8PcrControl)
  , "PcrPeriod" :: NullOrUndefined (Int)
  , "PcrPid" :: NullOrUndefined (String)
  , "PmtInterval" :: NullOrUndefined (Int)
  , "PmtPid" :: NullOrUndefined (String)
  , "ProgramNum" :: NullOrUndefined (Int)
  , "Scte35Behavior" :: NullOrUndefined (M3u8Scte35Behavior)
  , "Scte35Pid" :: NullOrUndefined (String)
  , "TimedMetadataBehavior" :: NullOrUndefined (M3u8TimedMetadataBehavior)
  , "TransportStreamId" :: NullOrUndefined (Int)
  , "VideoPid" :: NullOrUndefined (String)
  }
derive instance newtypeM3u8Settings :: Newtype M3u8Settings _


-- | Placeholder documentation for M3u8TimedMetadataBehavior
newtype M3u8TimedMetadataBehavior = M3u8TimedMetadataBehavior String
derive instance newtypeM3u8TimedMetadataBehavior :: Newtype M3u8TimedMetadataBehavior _


-- | Placeholder documentation for MaxResults
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | Placeholder documentation for Mp2CodingMode
newtype Mp2CodingMode = Mp2CodingMode String
derive instance newtypeMp2CodingMode :: Newtype Mp2CodingMode _


-- | Placeholder documentation for Mp2Settings
newtype Mp2Settings = Mp2Settings 
  { "Bitrate" :: NullOrUndefined (Number)
  , "CodingMode" :: NullOrUndefined (Mp2CodingMode)
  , "SampleRate" :: NullOrUndefined (Number)
  }
derive instance newtypeMp2Settings :: Newtype Mp2Settings _


-- | Placeholder documentation for MsSmoothGroupSettings
newtype MsSmoothGroupSettings = MsSmoothGroupSettings 
  { "AcquisitionPointId" :: NullOrUndefined (String)
  , "AudioOnlyTimecodeControl" :: NullOrUndefined (SmoothGroupAudioOnlyTimecodeControl)
  , "CertificateMode" :: NullOrUndefined (SmoothGroupCertificateMode)
  , "ConnectionRetryInterval" :: NullOrUndefined (Int)
  , "Destination" :: NullOrUndefined (OutputLocationRef)
  , "EventId" :: NullOrUndefined (String)
  , "EventIdMode" :: NullOrUndefined (SmoothGroupEventIdMode)
  , "EventStopBehavior" :: NullOrUndefined (SmoothGroupEventStopBehavior)
  , "FilecacheDuration" :: NullOrUndefined (Int)
  , "FragmentLength" :: NullOrUndefined (Int)
  , "InputLossAction" :: NullOrUndefined (InputLossActionForMsSmoothOut)
  , "NumRetries" :: NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined (Int)
  , "SegmentationMode" :: NullOrUndefined (SmoothGroupSegmentationMode)
  , "SendDelayMs" :: NullOrUndefined (Int)
  , "SparseTrackType" :: NullOrUndefined (SmoothGroupSparseTrackType)
  , "StreamManifestBehavior" :: NullOrUndefined (SmoothGroupStreamManifestBehavior)
  , "TimestampOffset" :: NullOrUndefined (String)
  , "TimestampOffsetMode" :: NullOrUndefined (SmoothGroupTimestampOffsetMode)
  }
derive instance newtypeMsSmoothGroupSettings :: Newtype MsSmoothGroupSettings _


-- | Placeholder documentation for MsSmoothOutputSettings
newtype MsSmoothOutputSettings = MsSmoothOutputSettings 
  { "NameModifier" :: NullOrUndefined (String)
  }
derive instance newtypeMsSmoothOutputSettings :: Newtype MsSmoothOutputSettings _


-- | Placeholder documentation for NetworkInputServerValidation
newtype NetworkInputServerValidation = NetworkInputServerValidation String
derive instance newtypeNetworkInputServerValidation :: Newtype NetworkInputServerValidation _


-- | Network source to transcode. Must be accessible to the Elemental Live node that is running the live event through a network connection.
newtype NetworkInputSettings = NetworkInputSettings 
  { "HlsInputSettings" :: NullOrUndefined (HlsInputSettings)
  , "ServerValidation" :: NullOrUndefined (NetworkInputServerValidation)
  }
derive instance newtypeNetworkInputSettings :: Newtype NetworkInputSettings _


-- | Placeholder documentation for NotFoundException
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


-- | Output settings. There can be multiple outputs within a group.
newtype Output = Output 
  { "AudioDescriptionNames" :: NullOrUndefined (ListOf__string)
  , "CaptionDescriptionNames" :: NullOrUndefined (ListOf__string)
  , "OutputName" :: NullOrUndefined (String)
  , "OutputSettings" :: NullOrUndefined (OutputSettings)
  , "VideoDescriptionName" :: NullOrUndefined (String)
  }
derive instance newtypeOutput :: Newtype Output _


-- | Placeholder documentation for OutputDestination
newtype OutputDestination = OutputDestination 
  { "Id" :: NullOrUndefined (String)
  , "Settings" :: NullOrUndefined (ListOfOutputDestinationSettings)
  }
derive instance newtypeOutputDestination :: Newtype OutputDestination _


-- | Placeholder documentation for OutputDestinationSettings
newtype OutputDestinationSettings = OutputDestinationSettings 
  { "PasswordParam" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }
derive instance newtypeOutputDestinationSettings :: Newtype OutputDestinationSettings _


-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
newtype OutputGroup = OutputGroup 
  { "Name" :: NullOrUndefined (String)
  , "OutputGroupSettings" :: NullOrUndefined (OutputGroupSettings)
  , "Outputs" :: NullOrUndefined (ListOfOutput)
  }
derive instance newtypeOutputGroup :: Newtype OutputGroup _


-- | Placeholder documentation for OutputGroupSettings
newtype OutputGroupSettings = OutputGroupSettings 
  { "ArchiveGroupSettings" :: NullOrUndefined (ArchiveGroupSettings)
  , "HlsGroupSettings" :: NullOrUndefined (HlsGroupSettings)
  , "MsSmoothGroupSettings" :: NullOrUndefined (MsSmoothGroupSettings)
  , "UdpGroupSettings" :: NullOrUndefined (UdpGroupSettings)
  }
derive instance newtypeOutputGroupSettings :: Newtype OutputGroupSettings _


-- | Reference to an OutputDestination ID defined in the channel
newtype OutputLocationRef = OutputLocationRef 
  { "DestinationRefId" :: NullOrUndefined (String)
  }
derive instance newtypeOutputLocationRef :: Newtype OutputLocationRef _


-- | Placeholder documentation for OutputSettings
newtype OutputSettings = OutputSettings 
  { "ArchiveOutputSettings" :: NullOrUndefined (ArchiveOutputSettings)
  , "HlsOutputSettings" :: NullOrUndefined (HlsOutputSettings)
  , "MsSmoothOutputSettings" :: NullOrUndefined (MsSmoothOutputSettings)
  , "UdpOutputSettings" :: NullOrUndefined (UdpOutputSettings)
  }
derive instance newtypeOutputSettings :: Newtype OutputSettings _


-- | Placeholder documentation for PassThroughSettings
newtype PassThroughSettings = PassThroughSettings 
  { 
  }
derive instance newtypePassThroughSettings :: Newtype PassThroughSettings _


-- | Placeholder documentation for RemixSettings
newtype RemixSettings = RemixSettings 
  { "ChannelMappings" :: NullOrUndefined (ListOfAudioChannelMapping)
  , "ChannelsIn" :: NullOrUndefined (Int)
  , "ChannelsOut" :: NullOrUndefined (Int)
  }
derive instance newtypeRemixSettings :: Newtype RemixSettings _


-- | Placeholder documentation for ResourceConflict
newtype ResourceConflict = ResourceConflict 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeResourceConflict :: Newtype ResourceConflict _


-- | Placeholder documentation for ResourceNotFound
newtype ResourceNotFound = ResourceNotFound 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeResourceNotFound :: Newtype ResourceNotFound _


-- | Placeholder documentation for Scte20Convert608To708
newtype Scte20Convert608To708 = Scte20Convert608To708 String
derive instance newtypeScte20Convert608To708 :: Newtype Scte20Convert608To708 _


-- | Placeholder documentation for Scte20PlusEmbeddedDestinationSettings
newtype Scte20PlusEmbeddedDestinationSettings = Scte20PlusEmbeddedDestinationSettings 
  { 
  }
derive instance newtypeScte20PlusEmbeddedDestinationSettings :: Newtype Scte20PlusEmbeddedDestinationSettings _


-- | Placeholder documentation for Scte20SourceSettings
newtype Scte20SourceSettings = Scte20SourceSettings 
  { "Convert608To708" :: NullOrUndefined (Scte20Convert608To708)
  , "Source608ChannelNumber" :: NullOrUndefined (Int)
  }
derive instance newtypeScte20SourceSettings :: Newtype Scte20SourceSettings _


-- | Placeholder documentation for Scte27DestinationSettings
newtype Scte27DestinationSettings = Scte27DestinationSettings 
  { 
  }
derive instance newtypeScte27DestinationSettings :: Newtype Scte27DestinationSettings _


-- | Placeholder documentation for Scte27SourceSettings
newtype Scte27SourceSettings = Scte27SourceSettings 
  { "Pid" :: NullOrUndefined (Int)
  }
derive instance newtypeScte27SourceSettings :: Newtype Scte27SourceSettings _


-- | Placeholder documentation for Scte35AposNoRegionalBlackoutBehavior
newtype Scte35AposNoRegionalBlackoutBehavior = Scte35AposNoRegionalBlackoutBehavior String
derive instance newtypeScte35AposNoRegionalBlackoutBehavior :: Newtype Scte35AposNoRegionalBlackoutBehavior _


-- | Placeholder documentation for Scte35AposWebDeliveryAllowedBehavior
newtype Scte35AposWebDeliveryAllowedBehavior = Scte35AposWebDeliveryAllowedBehavior String
derive instance newtypeScte35AposWebDeliveryAllowedBehavior :: Newtype Scte35AposWebDeliveryAllowedBehavior _


-- | Placeholder documentation for Scte35SpliceInsert
newtype Scte35SpliceInsert = Scte35SpliceInsert 
  { "AdAvailOffset" :: NullOrUndefined (Int)
  , "NoRegionalBlackoutFlag" :: NullOrUndefined (Scte35SpliceInsertNoRegionalBlackoutBehavior)
  , "WebDeliveryAllowedFlag" :: NullOrUndefined (Scte35SpliceInsertWebDeliveryAllowedBehavior)
  }
derive instance newtypeScte35SpliceInsert :: Newtype Scte35SpliceInsert _


-- | Placeholder documentation for Scte35SpliceInsertNoRegionalBlackoutBehavior
newtype Scte35SpliceInsertNoRegionalBlackoutBehavior = Scte35SpliceInsertNoRegionalBlackoutBehavior String
derive instance newtypeScte35SpliceInsertNoRegionalBlackoutBehavior :: Newtype Scte35SpliceInsertNoRegionalBlackoutBehavior _


-- | Placeholder documentation for Scte35SpliceInsertWebDeliveryAllowedBehavior
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior = Scte35SpliceInsertWebDeliveryAllowedBehavior String
derive instance newtypeScte35SpliceInsertWebDeliveryAllowedBehavior :: Newtype Scte35SpliceInsertWebDeliveryAllowedBehavior _


-- | Placeholder documentation for Scte35TimeSignalApos
newtype Scte35TimeSignalApos = Scte35TimeSignalApos 
  { "AdAvailOffset" :: NullOrUndefined (Int)
  , "NoRegionalBlackoutFlag" :: NullOrUndefined (Scte35AposNoRegionalBlackoutBehavior)
  , "WebDeliveryAllowedFlag" :: NullOrUndefined (Scte35AposWebDeliveryAllowedBehavior)
  }
derive instance newtypeScte35TimeSignalApos :: Newtype Scte35TimeSignalApos _


-- | Placeholder documentation for SmoothGroupAudioOnlyTimecodeControl
newtype SmoothGroupAudioOnlyTimecodeControl = SmoothGroupAudioOnlyTimecodeControl String
derive instance newtypeSmoothGroupAudioOnlyTimecodeControl :: Newtype SmoothGroupAudioOnlyTimecodeControl _


-- | Placeholder documentation for SmoothGroupCertificateMode
newtype SmoothGroupCertificateMode = SmoothGroupCertificateMode String
derive instance newtypeSmoothGroupCertificateMode :: Newtype SmoothGroupCertificateMode _


-- | Placeholder documentation for SmoothGroupEventIdMode
newtype SmoothGroupEventIdMode = SmoothGroupEventIdMode String
derive instance newtypeSmoothGroupEventIdMode :: Newtype SmoothGroupEventIdMode _


-- | Placeholder documentation for SmoothGroupEventStopBehavior
newtype SmoothGroupEventStopBehavior = SmoothGroupEventStopBehavior String
derive instance newtypeSmoothGroupEventStopBehavior :: Newtype SmoothGroupEventStopBehavior _


-- | Placeholder documentation for SmoothGroupSegmentationMode
newtype SmoothGroupSegmentationMode = SmoothGroupSegmentationMode String
derive instance newtypeSmoothGroupSegmentationMode :: Newtype SmoothGroupSegmentationMode _


-- | Placeholder documentation for SmoothGroupSparseTrackType
newtype SmoothGroupSparseTrackType = SmoothGroupSparseTrackType String
derive instance newtypeSmoothGroupSparseTrackType :: Newtype SmoothGroupSparseTrackType _


-- | Placeholder documentation for SmoothGroupStreamManifestBehavior
newtype SmoothGroupStreamManifestBehavior = SmoothGroupStreamManifestBehavior String
derive instance newtypeSmoothGroupStreamManifestBehavior :: Newtype SmoothGroupStreamManifestBehavior _


-- | Placeholder documentation for SmoothGroupTimestampOffsetMode
newtype SmoothGroupTimestampOffsetMode = SmoothGroupTimestampOffsetMode String
derive instance newtypeSmoothGroupTimestampOffsetMode :: Newtype SmoothGroupTimestampOffsetMode _


-- | Placeholder documentation for SmpteTtDestinationSettings
newtype SmpteTtDestinationSettings = SmpteTtDestinationSettings 
  { 
  }
derive instance newtypeSmpteTtDestinationSettings :: Newtype SmpteTtDestinationSettings _


-- | Placeholder documentation for StandardHlsSettings
newtype StandardHlsSettings = StandardHlsSettings 
  { "AudioRenditionSets" :: NullOrUndefined (String)
  , "M3u8Settings" :: NullOrUndefined (M3u8Settings)
  }
derive instance newtypeStandardHlsSettings :: Newtype StandardHlsSettings _


-- | Placeholder documentation for StartChannelRequest
newtype StartChannelRequest = StartChannelRequest 
  { "ChannelId" :: (String)
  }
derive instance newtypeStartChannelRequest :: Newtype StartChannelRequest _


-- | Placeholder documentation for StartChannelResponse
newtype StartChannelResponse = StartChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (ChannelState)
  }
derive instance newtypeStartChannelResponse :: Newtype StartChannelResponse _


-- | Placeholder documentation for StaticKeySettings
newtype StaticKeySettings = StaticKeySettings 
  { "KeyProviderServer" :: NullOrUndefined (InputLocation)
  , "StaticKeyValue" :: NullOrUndefined (String)
  }
derive instance newtypeStaticKeySettings :: Newtype StaticKeySettings _


-- | Placeholder documentation for StopChannelRequest
newtype StopChannelRequest = StopChannelRequest 
  { "ChannelId" :: (String)
  }
derive instance newtypeStopChannelRequest :: Newtype StopChannelRequest _


-- | Placeholder documentation for StopChannelResponse
newtype StopChannelResponse = StopChannelResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "Id" :: NullOrUndefined (String)
  , "InputAttachments" :: NullOrUndefined (ListOfInputAttachment)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "PipelinesRunningCount" :: NullOrUndefined (Int)
  , "RoleArn" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (ChannelState)
  }
derive instance newtypeStopChannelResponse :: Newtype StopChannelResponse _


-- | Placeholder documentation for TeletextDestinationSettings
newtype TeletextDestinationSettings = TeletextDestinationSettings 
  { 
  }
derive instance newtypeTeletextDestinationSettings :: Newtype TeletextDestinationSettings _


-- | Placeholder documentation for TeletextSourceSettings
newtype TeletextSourceSettings = TeletextSourceSettings 
  { "PageNumber" :: NullOrUndefined (String)
  }
derive instance newtypeTeletextSourceSettings :: Newtype TeletextSourceSettings _


-- | Placeholder documentation for TimecodeConfig
newtype TimecodeConfig = TimecodeConfig 
  { "Source" :: NullOrUndefined (TimecodeConfigSource)
  , "SyncThreshold" :: NullOrUndefined (Int)
  }
derive instance newtypeTimecodeConfig :: Newtype TimecodeConfig _


-- | Placeholder documentation for TimecodeConfigSource
newtype TimecodeConfigSource = TimecodeConfigSource String
derive instance newtypeTimecodeConfigSource :: Newtype TimecodeConfigSource _


-- | Placeholder documentation for TooManyRequestsException
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


-- | Placeholder documentation for TtmlDestinationSettings
newtype TtmlDestinationSettings = TtmlDestinationSettings 
  { "StyleControl" :: NullOrUndefined (TtmlDestinationStyleControl)
  }
derive instance newtypeTtmlDestinationSettings :: Newtype TtmlDestinationSettings _


-- | Placeholder documentation for TtmlDestinationStyleControl
newtype TtmlDestinationStyleControl = TtmlDestinationStyleControl String
derive instance newtypeTtmlDestinationStyleControl :: Newtype TtmlDestinationStyleControl _


-- | Placeholder documentation for UdpContainerSettings
newtype UdpContainerSettings = UdpContainerSettings 
  { "M2tsSettings" :: NullOrUndefined (M2tsSettings)
  }
derive instance newtypeUdpContainerSettings :: Newtype UdpContainerSettings _


-- | Placeholder documentation for UdpGroupSettings
newtype UdpGroupSettings = UdpGroupSettings 
  { "InputLossAction" :: NullOrUndefined (InputLossActionForUdpOut)
  , "TimedMetadataId3Frame" :: NullOrUndefined (UdpTimedMetadataId3Frame)
  , "TimedMetadataId3Period" :: NullOrUndefined (Int)
  }
derive instance newtypeUdpGroupSettings :: Newtype UdpGroupSettings _


-- | Placeholder documentation for UdpOutputSettings
newtype UdpOutputSettings = UdpOutputSettings 
  { "BufferMsec" :: NullOrUndefined (Int)
  , "ContainerSettings" :: NullOrUndefined (UdpContainerSettings)
  , "Destination" :: NullOrUndefined (OutputLocationRef)
  , "FecOutputSettings" :: NullOrUndefined (FecOutputSettings)
  }
derive instance newtypeUdpOutputSettings :: Newtype UdpOutputSettings _


-- | Placeholder documentation for UdpTimedMetadataId3Frame
newtype UdpTimedMetadataId3Frame = UdpTimedMetadataId3Frame String
derive instance newtypeUdpTimedMetadataId3Frame :: Newtype UdpTimedMetadataId3Frame _


-- | Placeholder documentation for UnprocessableEntityException
newtype UnprocessableEntityException = UnprocessableEntityException 
  { "Message" :: NullOrUndefined (String)
  , "ValidationErrors" :: NullOrUndefined (ListOfValidationError)
  }
derive instance newtypeUnprocessableEntityException :: Newtype UnprocessableEntityException _


-- | Placeholder documentation for UpdateChannel
newtype UpdateChannel = UpdateChannel 
  { "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateChannel :: Newtype UpdateChannel _


-- | A request to update a channel.
newtype UpdateChannelRequest = UpdateChannelRequest 
  { "ChannelId" :: (String)
  , "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateChannelRequest :: Newtype UpdateChannelRequest _


-- | Placeholder documentation for UpdateChannelResponse
newtype UpdateChannelResponse = UpdateChannelResponse 
  { "Channel" :: NullOrUndefined (Channel)
  }
derive instance newtypeUpdateChannelResponse :: Newtype UpdateChannelResponse _


-- | The updated channel's description.
newtype UpdateChannelResultModel = UpdateChannelResultModel 
  { "Channel" :: NullOrUndefined (Channel)
  }
derive instance newtypeUpdateChannelResultModel :: Newtype UpdateChannelResultModel _


-- | Placeholder documentation for ValidationError
newtype ValidationError = ValidationError 
  { "ElementPath" :: NullOrUndefined (String)
  , "ErrorMessage" :: NullOrUndefined (String)
  }
derive instance newtypeValidationError :: Newtype ValidationError _


-- | Placeholder documentation for VideoCodecSettings
newtype VideoCodecSettings = VideoCodecSettings 
  { "H264Settings" :: NullOrUndefined (H264Settings)
  }
derive instance newtypeVideoCodecSettings :: Newtype VideoCodecSettings _


-- | Video settings for this stream.
newtype VideoDescription = VideoDescription 
  { "CodecSettings" :: NullOrUndefined (VideoCodecSettings)
  , "Height" :: NullOrUndefined (Int)
  , "Name" :: NullOrUndefined (String)
  , "RespondToAfd" :: NullOrUndefined (VideoDescriptionRespondToAfd)
  , "ScalingBehavior" :: NullOrUndefined (VideoDescriptionScalingBehavior)
  , "Sharpness" :: NullOrUndefined (Int)
  , "Width" :: NullOrUndefined (Int)
  }
derive instance newtypeVideoDescription :: Newtype VideoDescription _


-- | Placeholder documentation for VideoDescriptionRespondToAfd
newtype VideoDescriptionRespondToAfd = VideoDescriptionRespondToAfd String
derive instance newtypeVideoDescriptionRespondToAfd :: Newtype VideoDescriptionRespondToAfd _


-- | Placeholder documentation for VideoDescriptionScalingBehavior
newtype VideoDescriptionScalingBehavior = VideoDescriptionScalingBehavior String
derive instance newtypeVideoDescriptionScalingBehavior :: Newtype VideoDescriptionScalingBehavior _


-- | Specifies a particular video stream within an input source. An input may have only a single video selector.
newtype VideoSelector = VideoSelector 
  { "ColorSpace" :: NullOrUndefined (VideoSelectorColorSpace)
  , "ColorSpaceUsage" :: NullOrUndefined (VideoSelectorColorSpaceUsage)
  , "SelectorSettings" :: NullOrUndefined (VideoSelectorSettings)
  }
derive instance newtypeVideoSelector :: Newtype VideoSelector _


-- | Placeholder documentation for VideoSelectorColorSpace
newtype VideoSelectorColorSpace = VideoSelectorColorSpace String
derive instance newtypeVideoSelectorColorSpace :: Newtype VideoSelectorColorSpace _


-- | Placeholder documentation for VideoSelectorColorSpaceUsage
newtype VideoSelectorColorSpaceUsage = VideoSelectorColorSpaceUsage String
derive instance newtypeVideoSelectorColorSpaceUsage :: Newtype VideoSelectorColorSpaceUsage _


-- | Placeholder documentation for VideoSelectorPid
newtype VideoSelectorPid = VideoSelectorPid 
  { "Pid" :: NullOrUndefined (Int)
  }
derive instance newtypeVideoSelectorPid :: Newtype VideoSelectorPid _


-- | Placeholder documentation for VideoSelectorProgramId
newtype VideoSelectorProgramId = VideoSelectorProgramId 
  { "ProgramId" :: NullOrUndefined (Int)
  }
derive instance newtypeVideoSelectorProgramId :: Newtype VideoSelectorProgramId _


-- | Placeholder documentation for VideoSelectorSettings
newtype VideoSelectorSettings = VideoSelectorSettings 
  { "VideoSelectorPid" :: NullOrUndefined (VideoSelectorPid)
  , "VideoSelectorProgramId" :: NullOrUndefined (VideoSelectorProgramId)
  }
derive instance newtypeVideoSelectorSettings :: Newtype VideoSelectorSettings _


-- | Placeholder documentation for WebvttDestinationSettings
newtype WebvttDestinationSettings = WebvttDestinationSettings 
  { 
  }
derive instance newtypeWebvttDestinationSettings :: Newtype WebvttDestinationSettings _
