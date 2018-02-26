

-- | API for AWS Elemental MediaLive
module AWS.MediaLive where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
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


-- | Placeholder documentation for AacInputType
newtype AacInputType = AacInputType String


-- | Placeholder documentation for AacProfile
newtype AacProfile = AacProfile String


-- | Placeholder documentation for AacRateControlMode
newtype AacRateControlMode = AacRateControlMode String


-- | Placeholder documentation for AacRawFormat
newtype AacRawFormat = AacRawFormat String


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


-- | Placeholder documentation for AacSpec
newtype AacSpec = AacSpec String


-- | Placeholder documentation for AacVbrQuality
newtype AacVbrQuality = AacVbrQuality String


-- | Placeholder documentation for Ac3BitstreamMode
newtype Ac3BitstreamMode = Ac3BitstreamMode String


-- | Placeholder documentation for Ac3CodingMode
newtype Ac3CodingMode = Ac3CodingMode String


-- | Placeholder documentation for Ac3DrcProfile
newtype Ac3DrcProfile = Ac3DrcProfile String


-- | Placeholder documentation for Ac3LfeFilter
newtype Ac3LfeFilter = Ac3LfeFilter String


-- | Placeholder documentation for Ac3MetadataControl
newtype Ac3MetadataControl = Ac3MetadataControl String


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


-- | Placeholder documentation for AccessDenied
newtype AccessDenied = AccessDenied 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for AfdSignaling
newtype AfdSignaling = AfdSignaling String


-- | Placeholder documentation for ArchiveContainerSettings
newtype ArchiveContainerSettings = ArchiveContainerSettings 
  { "M2tsSettings" :: NullOrUndefined (M2tsSettings)
  }


-- | Placeholder documentation for ArchiveGroupSettings
newtype ArchiveGroupSettings = ArchiveGroupSettings 
  { "Destination" :: NullOrUndefined (OutputLocationRef)
  , "RolloverInterval" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for ArchiveOutputSettings
newtype ArchiveOutputSettings = ArchiveOutputSettings 
  { "ContainerSettings" :: NullOrUndefined (ArchiveContainerSettings)
  , "Extension" :: NullOrUndefined (String)
  , "NameModifier" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for AribDestinationSettings
newtype AribDestinationSettings = AribDestinationSettings 
  { 
  }


-- | Placeholder documentation for AribSourceSettings
newtype AribSourceSettings = AribSourceSettings 
  { 
  }


-- | Placeholder documentation for AudioChannelMapping
newtype AudioChannelMapping = AudioChannelMapping 
  { "InputChannelLevels" :: NullOrUndefined (ListOfInputChannelLevel)
  , "OutputChannel" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for AudioCodecSettings
newtype AudioCodecSettings = AudioCodecSettings 
  { "AacSettings" :: NullOrUndefined (AacSettings)
  , "Ac3Settings" :: NullOrUndefined (Ac3Settings)
  , "Eac3Settings" :: NullOrUndefined (Eac3Settings)
  , "Mp2Settings" :: NullOrUndefined (Mp2Settings)
  , "PassThroughSettings" :: NullOrUndefined (PassThroughSettings)
  }


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


-- | Placeholder documentation for AudioDescriptionAudioTypeControl
newtype AudioDescriptionAudioTypeControl = AudioDescriptionAudioTypeControl String


-- | Placeholder documentation for AudioDescriptionLanguageCodeControl
newtype AudioDescriptionLanguageCodeControl = AudioDescriptionLanguageCodeControl String


-- | Placeholder documentation for AudioLanguageSelection
newtype AudioLanguageSelection = AudioLanguageSelection 
  { "LanguageCode" :: NullOrUndefined (String)
  , "LanguageSelectionPolicy" :: NullOrUndefined (AudioLanguageSelectionPolicy)
  }


-- | Placeholder documentation for AudioLanguageSelectionPolicy
newtype AudioLanguageSelectionPolicy = AudioLanguageSelectionPolicy String


-- | Placeholder documentation for AudioNormalizationAlgorithm
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm String


-- | Placeholder documentation for AudioNormalizationAlgorithmControl
newtype AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl String


-- | Placeholder documentation for AudioNormalizationSettings
newtype AudioNormalizationSettings = AudioNormalizationSettings 
  { "Algorithm" :: NullOrUndefined (AudioNormalizationAlgorithm)
  , "AlgorithmControl" :: NullOrUndefined (AudioNormalizationAlgorithmControl)
  , "TargetLkfs" :: NullOrUndefined (Number)
  }


-- | Placeholder documentation for AudioOnlyHlsSettings
newtype AudioOnlyHlsSettings = AudioOnlyHlsSettings 
  { "AudioGroupId" :: NullOrUndefined (String)
  , "AudioOnlyImage" :: NullOrUndefined (InputLocation)
  , "AudioTrackType" :: NullOrUndefined (AudioOnlyHlsTrackType)
  }


-- | Placeholder documentation for AudioOnlyHlsTrackType
newtype AudioOnlyHlsTrackType = AudioOnlyHlsTrackType String


-- | Placeholder documentation for AudioPidSelection
newtype AudioPidSelection = AudioPidSelection 
  { "Pid" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for AudioSelector
newtype AudioSelector = AudioSelector 
  { "Name" :: NullOrUndefined (String)
  , "SelectorSettings" :: NullOrUndefined (AudioSelectorSettings)
  }


-- | Placeholder documentation for AudioSelectorSettings
newtype AudioSelectorSettings = AudioSelectorSettings 
  { "AudioLanguageSelection" :: NullOrUndefined (AudioLanguageSelection)
  , "AudioPidSelection" :: NullOrUndefined (AudioPidSelection)
  }


-- | Placeholder documentation for AudioType
newtype AudioType = AudioType String


-- | Placeholder documentation for AvailBlanking
newtype AvailBlanking = AvailBlanking 
  { "AvailBlankingImage" :: NullOrUndefined (InputLocation)
  , "State" :: NullOrUndefined (AvailBlankingState)
  }


-- | Placeholder documentation for AvailBlankingState
newtype AvailBlankingState = AvailBlankingState String


-- | Placeholder documentation for AvailConfiguration
newtype AvailConfiguration = AvailConfiguration 
  { "AvailSettings" :: NullOrUndefined (AvailSettings)
  }


-- | Placeholder documentation for AvailSettings
newtype AvailSettings = AvailSettings 
  { "Scte35SpliceInsert" :: NullOrUndefined (Scte35SpliceInsert)
  , "Scte35TimeSignalApos" :: NullOrUndefined (Scte35TimeSignalApos)
  }


-- | Placeholder documentation for BadGatewayException
newtype BadGatewayException = BadGatewayException 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for BadRequestException
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for BlackoutSlate
newtype BlackoutSlate = BlackoutSlate 
  { "BlackoutSlateImage" :: NullOrUndefined (InputLocation)
  , "NetworkEndBlackout" :: NullOrUndefined (BlackoutSlateNetworkEndBlackout)
  , "NetworkEndBlackoutImage" :: NullOrUndefined (InputLocation)
  , "NetworkId" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (BlackoutSlateState)
  }


-- | Placeholder documentation for BlackoutSlateNetworkEndBlackout
newtype BlackoutSlateNetworkEndBlackout = BlackoutSlateNetworkEndBlackout String


-- | Placeholder documentation for BlackoutSlateState
newtype BlackoutSlateState = BlackoutSlateState String


-- | Placeholder documentation for BurnInAlignment
newtype BurnInAlignment = BurnInAlignment String


-- | Placeholder documentation for BurnInBackgroundColor
newtype BurnInBackgroundColor = BurnInBackgroundColor String


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


-- | Placeholder documentation for BurnInFontColor
newtype BurnInFontColor = BurnInFontColor String


-- | Placeholder documentation for BurnInOutlineColor
newtype BurnInOutlineColor = BurnInOutlineColor String


-- | Placeholder documentation for BurnInShadowColor
newtype BurnInShadowColor = BurnInShadowColor String


-- | Placeholder documentation for BurnInTeletextGridControl
newtype BurnInTeletextGridControl = BurnInTeletextGridControl String


-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
newtype CaptionDescription = CaptionDescription 
  { "CaptionSelectorName" :: NullOrUndefined (String)
  , "DestinationSettings" :: NullOrUndefined (CaptionDestinationSettings)
  , "LanguageCode" :: NullOrUndefined (String)
  , "LanguageDescription" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


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


-- | Maps a caption channel to an ISO 693-2 language code (http://www.loc.gov/standards/iso639-2), with an optional description.
newtype CaptionLanguageMapping = CaptionLanguageMapping 
  { "CaptionChannel" :: NullOrUndefined (Int)
  , "LanguageCode" :: NullOrUndefined (String)
  , "LanguageDescription" :: NullOrUndefined (String)
  }


-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
newtype CaptionSelector = CaptionSelector 
  { "LanguageCode" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "SelectorSettings" :: NullOrUndefined (CaptionSelectorSettings)
  }


-- | Placeholder documentation for CaptionSelectorSettings
newtype CaptionSelectorSettings = CaptionSelectorSettings 
  { "AribSourceSettings" :: NullOrUndefined (AribSourceSettings)
  , "DvbSubSourceSettings" :: NullOrUndefined (DvbSubSourceSettings)
  , "EmbeddedSourceSettings" :: NullOrUndefined (EmbeddedSourceSettings)
  , "Scte20SourceSettings" :: NullOrUndefined (Scte20SourceSettings)
  , "Scte27SourceSettings" :: NullOrUndefined (Scte27SourceSettings)
  , "TeletextSourceSettings" :: NullOrUndefined (TeletextSourceSettings)
  }


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


-- | Placeholder documentation for ChannelConfigurationValidationError
newtype ChannelConfigurationValidationError = ChannelConfigurationValidationError 
  { "Message" :: NullOrUndefined (String)
  , "ValidationErrors" :: NullOrUndefined (ListOfValidationError)
  }


-- | Placeholder documentation for ChannelEgressEndpoint
newtype ChannelEgressEndpoint = ChannelEgressEndpoint 
  { "SourceIp" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ChannelState
newtype ChannelState = ChannelState String


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


-- | Placeholder documentation for ConflictException
newtype ConflictException = ConflictException 
  { "Message" :: NullOrUndefined (String)
  }


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


-- | Placeholder documentation for CreateChannelResponse
newtype CreateChannelResponse = CreateChannelResponse 
  { "Channel" :: NullOrUndefined (Channel)
  }


-- | Placeholder documentation for CreateChannelResultModel
newtype CreateChannelResultModel = CreateChannelResultModel 
  { "Channel" :: NullOrUndefined (Channel)
  }


-- | Placeholder documentation for CreateInput
newtype CreateInput = CreateInput 
  { "Destinations" :: NullOrUndefined (ListOfInputDestinationRequest)
  , "InputSecurityGroups" :: NullOrUndefined (ListOf__string)
  , "Name" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "Sources" :: NullOrUndefined (ListOfInputSourceRequest)
  , "Type" :: NullOrUndefined (InputType)
  }


-- | The name of the input
newtype CreateInputRequest = CreateInputRequest 
  { "Destinations" :: NullOrUndefined (ListOfInputDestinationRequest)
  , "InputSecurityGroups" :: NullOrUndefined (ListOf__string)
  , "Name" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "Sources" :: NullOrUndefined (ListOfInputSourceRequest)
  , "Type" :: NullOrUndefined (InputType)
  }


-- | Placeholder documentation for CreateInputResponse
newtype CreateInputResponse = CreateInputResponse 
  { "Input" :: NullOrUndefined (Input)
  }


-- | Placeholder documentation for CreateInputResultModel
newtype CreateInputResultModel = CreateInputResultModel 
  { "Input" :: NullOrUndefined (Input)
  }


-- | The IPv4 CIDRs to whitelist for this Input Security Group
newtype CreateInputSecurityGroupRequest = CreateInputSecurityGroupRequest 
  { "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRuleCidr)
  }


-- | Placeholder documentation for CreateInputSecurityGroupResponse
newtype CreateInputSecurityGroupResponse = CreateInputSecurityGroupResponse 
  { "SecurityGroup" :: NullOrUndefined (InputSecurityGroup)
  }


-- | Placeholder documentation for CreateInputSecurityGroupResultModel
newtype CreateInputSecurityGroupResultModel = CreateInputSecurityGroupResultModel 
  { "SecurityGroup" :: NullOrUndefined (InputSecurityGroup)
  }


-- | Placeholder documentation for DeleteChannelRequest
newtype DeleteChannelRequest = DeleteChannelRequest 
  { "ChannelId" :: (String)
  }


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


-- | Placeholder documentation for DeleteInputRequest
newtype DeleteInputRequest = DeleteInputRequest 
  { "InputId" :: (String)
  }


-- | Placeholder documentation for DeleteInputResponse
newtype DeleteInputResponse = DeleteInputResponse 
  { 
  }


-- | Placeholder documentation for DeleteInputSecurityGroupRequest
newtype DeleteInputSecurityGroupRequest = DeleteInputSecurityGroupRequest 
  { "InputSecurityGroupId" :: (String)
  }


-- | Placeholder documentation for DeleteInputSecurityGroupResponse
newtype DeleteInputSecurityGroupResponse = DeleteInputSecurityGroupResponse 
  { 
  }


-- | Placeholder documentation for DescribeChannelRequest
newtype DescribeChannelRequest = DescribeChannelRequest 
  { "ChannelId" :: (String)
  }


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


-- | Placeholder documentation for DescribeInputRequest
newtype DescribeInputRequest = DescribeInputRequest 
  { "InputId" :: (String)
  }


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


-- | Placeholder documentation for DescribeInputSecurityGroupRequest
newtype DescribeInputSecurityGroupRequest = DescribeInputSecurityGroupRequest 
  { "InputSecurityGroupId" :: (String)
  }


-- | Placeholder documentation for DescribeInputSecurityGroupResponse
newtype DescribeInputSecurityGroupResponse = DescribeInputSecurityGroupResponse 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRule)
  }


-- | DVB Network Information Table (NIT)
newtype DvbNitSettings = DvbNitSettings 
  { "NetworkId" :: NullOrUndefined (Int)
  , "NetworkName" :: NullOrUndefined (String)
  , "RepInterval" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for DvbSdtOutputSdt
newtype DvbSdtOutputSdt = DvbSdtOutputSdt String


-- | DVB Service Description Table (SDT)
newtype DvbSdtSettings = DvbSdtSettings 
  { "OutputSdt" :: NullOrUndefined (DvbSdtOutputSdt)
  , "RepInterval" :: NullOrUndefined (Int)
  , "ServiceName" :: NullOrUndefined (String)
  , "ServiceProviderName" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for DvbSubDestinationAlignment
newtype DvbSubDestinationAlignment = DvbSubDestinationAlignment String


-- | Placeholder documentation for DvbSubDestinationBackgroundColor
newtype DvbSubDestinationBackgroundColor = DvbSubDestinationBackgroundColor String


-- | Placeholder documentation for DvbSubDestinationFontColor
newtype DvbSubDestinationFontColor = DvbSubDestinationFontColor String


-- | Placeholder documentation for DvbSubDestinationOutlineColor
newtype DvbSubDestinationOutlineColor = DvbSubDestinationOutlineColor String


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


-- | Placeholder documentation for DvbSubDestinationShadowColor
newtype DvbSubDestinationShadowColor = DvbSubDestinationShadowColor String


-- | Placeholder documentation for DvbSubDestinationTeletextGridControl
newtype DvbSubDestinationTeletextGridControl = DvbSubDestinationTeletextGridControl String


-- | Placeholder documentation for DvbSubSourceSettings
newtype DvbSubSourceSettings = DvbSubSourceSettings 
  { "Pid" :: NullOrUndefined (Int)
  }


-- | DVB Time and Date Table (SDT)
newtype DvbTdtSettings = DvbTdtSettings 
  { "RepInterval" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for Eac3AttenuationControl
newtype Eac3AttenuationControl = Eac3AttenuationControl String


-- | Placeholder documentation for Eac3BitstreamMode
newtype Eac3BitstreamMode = Eac3BitstreamMode String


-- | Placeholder documentation for Eac3CodingMode
newtype Eac3CodingMode = Eac3CodingMode String


-- | Placeholder documentation for Eac3DcFilter
newtype Eac3DcFilter = Eac3DcFilter String


-- | Placeholder documentation for Eac3DrcLine
newtype Eac3DrcLine = Eac3DrcLine String


-- | Placeholder documentation for Eac3DrcRf
newtype Eac3DrcRf = Eac3DrcRf String


-- | Placeholder documentation for Eac3LfeControl
newtype Eac3LfeControl = Eac3LfeControl String


-- | Placeholder documentation for Eac3LfeFilter
newtype Eac3LfeFilter = Eac3LfeFilter String


-- | Placeholder documentation for Eac3MetadataControl
newtype Eac3MetadataControl = Eac3MetadataControl String


-- | Placeholder documentation for Eac3PassthroughControl
newtype Eac3PassthroughControl = Eac3PassthroughControl String


-- | Placeholder documentation for Eac3PhaseControl
newtype Eac3PhaseControl = Eac3PhaseControl String


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


-- | Placeholder documentation for Eac3StereoDownmix
newtype Eac3StereoDownmix = Eac3StereoDownmix String


-- | Placeholder documentation for Eac3SurroundExMode
newtype Eac3SurroundExMode = Eac3SurroundExMode String


-- | Placeholder documentation for Eac3SurroundMode
newtype Eac3SurroundMode = Eac3SurroundMode String


-- | Placeholder documentation for EmbeddedConvert608To708
newtype EmbeddedConvert608To708 = EmbeddedConvert608To708 String


-- | Placeholder documentation for EmbeddedDestinationSettings
newtype EmbeddedDestinationSettings = EmbeddedDestinationSettings 
  { 
  }


-- | Placeholder documentation for EmbeddedPlusScte20DestinationSettings
newtype EmbeddedPlusScte20DestinationSettings = EmbeddedPlusScte20DestinationSettings 
  { 
  }


-- | Placeholder documentation for EmbeddedScte20Detection
newtype EmbeddedScte20Detection = EmbeddedScte20Detection String


-- | Placeholder documentation for EmbeddedSourceSettings
newtype EmbeddedSourceSettings = EmbeddedSourceSettings 
  { "Convert608To708" :: NullOrUndefined (EmbeddedConvert608To708)
  , "Scte20Detection" :: NullOrUndefined (EmbeddedScte20Detection)
  , "Source608ChannelNumber" :: NullOrUndefined (Int)
  , "Source608TrackNumber" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for Empty
newtype Empty = Empty 
  { 
  }


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


-- | Placeholder documentation for FecOutputIncludeFec
newtype FecOutputIncludeFec = FecOutputIncludeFec String


-- | Placeholder documentation for FecOutputSettings
newtype FecOutputSettings = FecOutputSettings 
  { "ColumnDepth" :: NullOrUndefined (Int)
  , "IncludeFec" :: NullOrUndefined (FecOutputIncludeFec)
  , "RowLength" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for FixedAfd
newtype FixedAfd = FixedAfd String


-- | Placeholder documentation for ForbiddenException
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for GatewayTimeoutException
newtype GatewayTimeoutException = GatewayTimeoutException 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for GlobalConfiguration
newtype GlobalConfiguration = GlobalConfiguration 
  { "InitialAudioGain" :: NullOrUndefined (Int)
  , "InputEndAction" :: NullOrUndefined (GlobalConfigurationInputEndAction)
  , "InputLossBehavior" :: NullOrUndefined (InputLossBehavior)
  , "OutputTimingSource" :: NullOrUndefined (GlobalConfigurationOutputTimingSource)
  , "SupportLowFramerateInputs" :: NullOrUndefined (GlobalConfigurationLowFramerateInputs)
  }


-- | Placeholder documentation for GlobalConfigurationInputEndAction
newtype GlobalConfigurationInputEndAction = GlobalConfigurationInputEndAction String


-- | Placeholder documentation for GlobalConfigurationLowFramerateInputs
newtype GlobalConfigurationLowFramerateInputs = GlobalConfigurationLowFramerateInputs String


-- | Placeholder documentation for GlobalConfigurationOutputTimingSource
newtype GlobalConfigurationOutputTimingSource = GlobalConfigurationOutputTimingSource String


-- | Placeholder documentation for H264AdaptiveQuantization
newtype H264AdaptiveQuantization = H264AdaptiveQuantization String


-- | Placeholder documentation for H264ColorMetadata
newtype H264ColorMetadata = H264ColorMetadata String


-- | Placeholder documentation for H264EntropyEncoding
newtype H264EntropyEncoding = H264EntropyEncoding String


-- | Placeholder documentation for H264FlickerAq
newtype H264FlickerAq = H264FlickerAq String


-- | Placeholder documentation for H264FramerateControl
newtype H264FramerateControl = H264FramerateControl String


-- | Placeholder documentation for H264GopBReference
newtype H264GopBReference = H264GopBReference String


-- | Placeholder documentation for H264GopSizeUnits
newtype H264GopSizeUnits = H264GopSizeUnits String


-- | Placeholder documentation for H264Level
newtype H264Level = H264Level String


-- | Placeholder documentation for H264LookAheadRateControl
newtype H264LookAheadRateControl = H264LookAheadRateControl String


-- | Placeholder documentation for H264ParControl
newtype H264ParControl = H264ParControl String


-- | Placeholder documentation for H264Profile
newtype H264Profile = H264Profile String


-- | Placeholder documentation for H264RateControlMode
newtype H264RateControlMode = H264RateControlMode String


-- | Placeholder documentation for H264ScanType
newtype H264ScanType = H264ScanType String


-- | Placeholder documentation for H264SceneChangeDetect
newtype H264SceneChangeDetect = H264SceneChangeDetect String


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


-- | Placeholder documentation for H264SpatialAq
newtype H264SpatialAq = H264SpatialAq String


-- | Placeholder documentation for H264Syntax
newtype H264Syntax = H264Syntax String


-- | Placeholder documentation for H264TemporalAq
newtype H264TemporalAq = H264TemporalAq String


-- | Placeholder documentation for H264TimecodeInsertionBehavior
newtype H264TimecodeInsertionBehavior = H264TimecodeInsertionBehavior String


-- | Placeholder documentation for HlsAdMarkers
newtype HlsAdMarkers = HlsAdMarkers String


-- | Placeholder documentation for HlsAkamaiHttpTransferMode
newtype HlsAkamaiHttpTransferMode = HlsAkamaiHttpTransferMode String


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


-- | Placeholder documentation for HlsBasicPutSettings
newtype HlsBasicPutSettings = HlsBasicPutSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined (Int)
  , "NumRetries" :: NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for HlsCaptionLanguageSetting
newtype HlsCaptionLanguageSetting = HlsCaptionLanguageSetting String


-- | Placeholder documentation for HlsCdnSettings
newtype HlsCdnSettings = HlsCdnSettings 
  { "HlsAkamaiSettings" :: NullOrUndefined (HlsAkamaiSettings)
  , "HlsBasicPutSettings" :: NullOrUndefined (HlsBasicPutSettings)
  , "HlsMediaStoreSettings" :: NullOrUndefined (HlsMediaStoreSettings)
  , "HlsWebdavSettings" :: NullOrUndefined (HlsWebdavSettings)
  }


-- | Placeholder documentation for HlsClientCache
newtype HlsClientCache = HlsClientCache String


-- | Placeholder documentation for HlsCodecSpecification
newtype HlsCodecSpecification = HlsCodecSpecification String


-- | Placeholder documentation for HlsDirectoryStructure
newtype HlsDirectoryStructure = HlsDirectoryStructure String


-- | Placeholder documentation for HlsEncryptionType
newtype HlsEncryptionType = HlsEncryptionType String


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


-- | Placeholder documentation for HlsInputSettings
newtype HlsInputSettings = HlsInputSettings 
  { "Bandwidth" :: NullOrUndefined (Int)
  , "BufferSegments" :: NullOrUndefined (Int)
  , "Retries" :: NullOrUndefined (Int)
  , "RetryInterval" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for HlsIvInManifest
newtype HlsIvInManifest = HlsIvInManifest String


-- | Placeholder documentation for HlsIvSource
newtype HlsIvSource = HlsIvSource String


-- | Placeholder documentation for HlsManifestCompression
newtype HlsManifestCompression = HlsManifestCompression String


-- | Placeholder documentation for HlsManifestDurationFormat
newtype HlsManifestDurationFormat = HlsManifestDurationFormat String


-- | Placeholder documentation for HlsMediaStoreSettings
newtype HlsMediaStoreSettings = HlsMediaStoreSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined (Int)
  , "MediaStoreStorageClass" :: NullOrUndefined (HlsMediaStoreStorageClass)
  , "NumRetries" :: NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for HlsMediaStoreStorageClass
newtype HlsMediaStoreStorageClass = HlsMediaStoreStorageClass String


-- | Placeholder documentation for HlsMode
newtype HlsMode = HlsMode String


-- | Placeholder documentation for HlsOutputSelection
newtype HlsOutputSelection = HlsOutputSelection String


-- | Placeholder documentation for HlsOutputSettings
newtype HlsOutputSettings = HlsOutputSettings 
  { "HlsSettings" :: NullOrUndefined (HlsSettings)
  , "NameModifier" :: NullOrUndefined (String)
  , "SegmentModifier" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for HlsProgramDateTime
newtype HlsProgramDateTime = HlsProgramDateTime String


-- | Placeholder documentation for HlsSegmentationMode
newtype HlsSegmentationMode = HlsSegmentationMode String


-- | Placeholder documentation for HlsSettings
newtype HlsSettings = HlsSettings 
  { "AudioOnlyHlsSettings" :: NullOrUndefined (AudioOnlyHlsSettings)
  , "StandardHlsSettings" :: NullOrUndefined (StandardHlsSettings)
  }


-- | Placeholder documentation for HlsStreamInfResolution
newtype HlsStreamInfResolution = HlsStreamInfResolution String


-- | Placeholder documentation for HlsTimedMetadataId3Frame
newtype HlsTimedMetadataId3Frame = HlsTimedMetadataId3Frame String


-- | Placeholder documentation for HlsTsFileMode
newtype HlsTsFileMode = HlsTsFileMode String


-- | Placeholder documentation for HlsWebdavHttpTransferMode
newtype HlsWebdavHttpTransferMode = HlsWebdavHttpTransferMode String


-- | Placeholder documentation for HlsWebdavSettings
newtype HlsWebdavSettings = HlsWebdavSettings 
  { "ConnectionRetryInterval" :: NullOrUndefined (Int)
  , "FilecacheDuration" :: NullOrUndefined (Int)
  , "HttpTransferMode" :: NullOrUndefined (HlsWebdavHttpTransferMode)
  , "NumRetries" :: NullOrUndefined (Int)
  , "RestartDelay" :: NullOrUndefined (Int)
  }


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


-- | Placeholder documentation for InputAttachment
newtype InputAttachment = InputAttachment 
  { "InputId" :: NullOrUndefined (String)
  , "InputSettings" :: NullOrUndefined (InputSettings)
  }


-- | Placeholder documentation for InputChannelLevel
newtype InputChannelLevel = InputChannelLevel 
  { "Gain" :: NullOrUndefined (Int)
  , "InputChannel" :: NullOrUndefined (Int)
  }


-- | codec in increasing order of complexity
newtype InputCodec = InputCodec String


-- | Placeholder documentation for InputDeblockFilter
newtype InputDeblockFilter = InputDeblockFilter String


-- | Placeholder documentation for InputDenoiseFilter
newtype InputDenoiseFilter = InputDenoiseFilter String


-- | The settings for a PUSH type input.
newtype InputDestination = InputDestination 
  { "Ip" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  }


-- | Endpoint settings for a PUSH type input.
newtype InputDestinationRequest = InputDestinationRequest 
  { "StreamName" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for InputFilter
newtype InputFilter = InputFilter String


-- | Placeholder documentation for InputLocation
newtype InputLocation = InputLocation 
  { "PasswordParam" :: NullOrUndefined (String)
  , "Uri" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for InputLossActionForHlsOut
newtype InputLossActionForHlsOut = InputLossActionForHlsOut String


-- | Placeholder documentation for InputLossActionForMsSmoothOut
newtype InputLossActionForMsSmoothOut = InputLossActionForMsSmoothOut String


-- | Placeholder documentation for InputLossActionForUdpOut
newtype InputLossActionForUdpOut = InputLossActionForUdpOut String


-- | Placeholder documentation for InputLossBehavior
newtype InputLossBehavior = InputLossBehavior 
  { "BlackFrameMsec" :: NullOrUndefined (Int)
  , "InputLossImageColor" :: NullOrUndefined (String)
  , "InputLossImageSlate" :: NullOrUndefined (InputLocation)
  , "InputLossImageType" :: NullOrUndefined (InputLossImageType)
  , "RepeatFrameMsec" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for InputLossImageType
newtype InputLossImageType = InputLossImageType String


-- | Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.
newtype InputMaximumBitrate = InputMaximumBitrate String


-- | Input resolution based on lines of vertical resolution in the input; SD is less than 720 lines, HD is 720 to 1080 lines, UHD is greater than 1080 lines
-- | 
newtype InputResolution = InputResolution String


-- | An Input Security Group
newtype InputSecurityGroup = InputSecurityGroup 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRule)
  }


-- | Request of IPv4 CIDR addresses to whitelist in a security group.
newtype InputSecurityGroupWhitelistRequest = InputSecurityGroupWhitelistRequest 
  { "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRuleCidr)
  }


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


-- | The settings for a PULL type input.
newtype InputSource = InputSource 
  { "PasswordParam" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for InputSourceEndBehavior
newtype InputSourceEndBehavior = InputSourceEndBehavior String


-- | Settings for for a PULL type input.
newtype InputSourceRequest = InputSourceRequest 
  { "PasswordParam" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for InputSpecification
newtype InputSpecification = InputSpecification 
  { "Codec" :: NullOrUndefined (InputCodec)
  , "MaximumBitrate" :: NullOrUndefined (InputMaximumBitrate)
  , "Resolution" :: NullOrUndefined (InputResolution)
  }


-- | Placeholder documentation for InputState
newtype InputState = InputState String


-- | Placeholder documentation for InputType
newtype InputType = InputType String


-- | Whitelist rule
newtype InputWhitelistRule = InputWhitelistRule 
  { "Cidr" :: NullOrUndefined (String)
  }


-- | An IPv4 CIDR to whitelist.
newtype InputWhitelistRuleCidr = InputWhitelistRuleCidr 
  { "Cidr" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for InternalServerErrorException
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for InternalServiceError
newtype InternalServiceError = InternalServiceError 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for InvalidRequest
newtype InvalidRequest = InvalidRequest 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for KeyProviderSettings
newtype KeyProviderSettings = KeyProviderSettings 
  { "StaticKeySettings" :: NullOrUndefined (StaticKeySettings)
  }


-- | Placeholder documentation for LimitExceeded
newtype LimitExceeded = LimitExceeded 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ListChannelsRequest
newtype ListChannelsRequest = ListChannelsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ListChannelsResponse
newtype ListChannelsResponse = ListChannelsResponse 
  { "Channels" :: NullOrUndefined (ListOfChannelSummary)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ListChannelsResultModel
newtype ListChannelsResultModel = ListChannelsResultModel 
  { "Channels" :: NullOrUndefined (ListOfChannelSummary)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ListInputSecurityGroupsRequest
newtype ListInputSecurityGroupsRequest = ListInputSecurityGroupsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ListInputSecurityGroupsResponse
newtype ListInputSecurityGroupsResponse = ListInputSecurityGroupsResponse 
  { "InputSecurityGroups" :: NullOrUndefined (ListOfInputSecurityGroup)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Result of input security group list request
newtype ListInputSecurityGroupsResultModel = ListInputSecurityGroupsResultModel 
  { "InputSecurityGroups" :: NullOrUndefined (ListOfInputSecurityGroup)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ListInputsRequest
newtype ListInputsRequest = ListInputsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ListInputsResponse
newtype ListInputsResponse = ListInputsResponse 
  { "Inputs" :: NullOrUndefined (ListOfInput)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ListInputsResultModel
newtype ListInputsResultModel = ListInputsResultModel 
  { "Inputs" :: NullOrUndefined (ListOfInput)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ListOfAudioChannelMapping
newtype ListOfAudioChannelMapping = ListOfAudioChannelMapping (Array AudioChannelMapping)


-- | Placeholder documentation for ListOfAudioDescription
newtype ListOfAudioDescription = ListOfAudioDescription (Array AudioDescription)


-- | Placeholder documentation for ListOfAudioSelector
newtype ListOfAudioSelector = ListOfAudioSelector (Array AudioSelector)


-- | Placeholder documentation for ListOfCaptionDescription
newtype ListOfCaptionDescription = ListOfCaptionDescription (Array CaptionDescription)


-- | Placeholder documentation for ListOfCaptionLanguageMapping
newtype ListOfCaptionLanguageMapping = ListOfCaptionLanguageMapping (Array CaptionLanguageMapping)


-- | Placeholder documentation for ListOfCaptionSelector
newtype ListOfCaptionSelector = ListOfCaptionSelector (Array CaptionSelector)


-- | Placeholder documentation for ListOfChannelEgressEndpoint
newtype ListOfChannelEgressEndpoint = ListOfChannelEgressEndpoint (Array ChannelEgressEndpoint)


-- | Placeholder documentation for ListOfChannelSummary
newtype ListOfChannelSummary = ListOfChannelSummary (Array ChannelSummary)


-- | Placeholder documentation for ListOfHlsAdMarkers
newtype ListOfHlsAdMarkers = ListOfHlsAdMarkers (Array HlsAdMarkers)


-- | Placeholder documentation for ListOfInput
newtype ListOfInput = ListOfInput (Array Input)


-- | Placeholder documentation for ListOfInputAttachment
newtype ListOfInputAttachment = ListOfInputAttachment (Array InputAttachment)


-- | Placeholder documentation for ListOfInputChannelLevel
newtype ListOfInputChannelLevel = ListOfInputChannelLevel (Array InputChannelLevel)


-- | Placeholder documentation for ListOfInputDestination
newtype ListOfInputDestination = ListOfInputDestination (Array InputDestination)


-- | Placeholder documentation for ListOfInputDestinationRequest
newtype ListOfInputDestinationRequest = ListOfInputDestinationRequest (Array InputDestinationRequest)


-- | Placeholder documentation for ListOfInputSecurityGroup
newtype ListOfInputSecurityGroup = ListOfInputSecurityGroup (Array InputSecurityGroup)


-- | Placeholder documentation for ListOfInputSource
newtype ListOfInputSource = ListOfInputSource (Array InputSource)


-- | Placeholder documentation for ListOfInputSourceRequest
newtype ListOfInputSourceRequest = ListOfInputSourceRequest (Array InputSourceRequest)


-- | Placeholder documentation for ListOfInputWhitelistRule
newtype ListOfInputWhitelistRule = ListOfInputWhitelistRule (Array InputWhitelistRule)


-- | Placeholder documentation for ListOfInputWhitelistRuleCidr
newtype ListOfInputWhitelistRuleCidr = ListOfInputWhitelistRuleCidr (Array InputWhitelistRuleCidr)


-- | Placeholder documentation for ListOfOutput
newtype ListOfOutput = ListOfOutput (Array Output)


-- | Placeholder documentation for ListOfOutputDestination
newtype ListOfOutputDestination = ListOfOutputDestination (Array OutputDestination)


-- | Placeholder documentation for ListOfOutputDestinationSettings
newtype ListOfOutputDestinationSettings = ListOfOutputDestinationSettings (Array OutputDestinationSettings)


-- | Placeholder documentation for ListOfOutputGroup
newtype ListOfOutputGroup = ListOfOutputGroup (Array OutputGroup)


-- | Placeholder documentation for ListOfValidationError
newtype ListOfValidationError = ListOfValidationError (Array ValidationError)


-- | Placeholder documentation for ListOfVideoDescription
newtype ListOfVideoDescription = ListOfVideoDescription (Array VideoDescription)


-- | Placeholder documentation for ListOf__string
newtype ListOf__string = ListOf__string (Array String)


-- | Placeholder documentation for M2tsAbsentInputAudioBehavior
newtype M2tsAbsentInputAudioBehavior = M2tsAbsentInputAudioBehavior String


-- | Placeholder documentation for M2tsArib
newtype M2tsArib = M2tsArib String


-- | Placeholder documentation for M2tsAribCaptionsPidControl
newtype M2tsAribCaptionsPidControl = M2tsAribCaptionsPidControl String


-- | Placeholder documentation for M2tsAudioBufferModel
newtype M2tsAudioBufferModel = M2tsAudioBufferModel String


-- | Placeholder documentation for M2tsAudioInterval
newtype M2tsAudioInterval = M2tsAudioInterval String


-- | Placeholder documentation for M2tsAudioStreamType
newtype M2tsAudioStreamType = M2tsAudioStreamType String


-- | Placeholder documentation for M2tsBufferModel
newtype M2tsBufferModel = M2tsBufferModel String


-- | Placeholder documentation for M2tsCcDescriptor
newtype M2tsCcDescriptor = M2tsCcDescriptor String


-- | Placeholder documentation for M2tsEbifControl
newtype M2tsEbifControl = M2tsEbifControl String


-- | Placeholder documentation for M2tsEbpPlacement
newtype M2tsEbpPlacement = M2tsEbpPlacement String


-- | Placeholder documentation for M2tsEsRateInPes
newtype M2tsEsRateInPes = M2tsEsRateInPes String


-- | Placeholder documentation for M2tsKlv
newtype M2tsKlv = M2tsKlv String


-- | Placeholder documentation for M2tsPcrControl
newtype M2tsPcrControl = M2tsPcrControl String


-- | Placeholder documentation for M2tsRateMode
newtype M2tsRateMode = M2tsRateMode String


-- | Placeholder documentation for M2tsScte35Control
newtype M2tsScte35Control = M2tsScte35Control String


-- | Placeholder documentation for M2tsSegmentationMarkers
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers String


-- | Placeholder documentation for M2tsSegmentationStyle
newtype M2tsSegmentationStyle = M2tsSegmentationStyle String


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


-- | Placeholder documentation for M2tsTimedMetadataBehavior
newtype M2tsTimedMetadataBehavior = M2tsTimedMetadataBehavior String


-- | Placeholder documentation for M3u8PcrControl
newtype M3u8PcrControl = M3u8PcrControl String


-- | Placeholder documentation for M3u8Scte35Behavior
newtype M3u8Scte35Behavior = M3u8Scte35Behavior String


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


-- | Placeholder documentation for M3u8TimedMetadataBehavior
newtype M3u8TimedMetadataBehavior = M3u8TimedMetadataBehavior String


-- | Placeholder documentation for MaxResults
newtype MaxResults = MaxResults Int


-- | Placeholder documentation for Mp2CodingMode
newtype Mp2CodingMode = Mp2CodingMode String


-- | Placeholder documentation for Mp2Settings
newtype Mp2Settings = Mp2Settings 
  { "Bitrate" :: NullOrUndefined (Number)
  , "CodingMode" :: NullOrUndefined (Mp2CodingMode)
  , "SampleRate" :: NullOrUndefined (Number)
  }


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


-- | Placeholder documentation for MsSmoothOutputSettings
newtype MsSmoothOutputSettings = MsSmoothOutputSettings 
  { "NameModifier" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for NetworkInputServerValidation
newtype NetworkInputServerValidation = NetworkInputServerValidation String


-- | Network source to transcode. Must be accessible to the Elemental Live node that is running the live event through a network connection.
newtype NetworkInputSettings = NetworkInputSettings 
  { "HlsInputSettings" :: NullOrUndefined (HlsInputSettings)
  , "ServerValidation" :: NullOrUndefined (NetworkInputServerValidation)
  }


-- | Placeholder documentation for NotFoundException
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (String)
  }


-- | Output settings. There can be multiple outputs within a group.
newtype Output = Output 
  { "AudioDescriptionNames" :: NullOrUndefined (ListOf__string)
  , "CaptionDescriptionNames" :: NullOrUndefined (ListOf__string)
  , "OutputName" :: NullOrUndefined (String)
  , "OutputSettings" :: NullOrUndefined (OutputSettings)
  , "VideoDescriptionName" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for OutputDestination
newtype OutputDestination = OutputDestination 
  { "Id" :: NullOrUndefined (String)
  , "Settings" :: NullOrUndefined (ListOfOutputDestinationSettings)
  }


-- | Placeholder documentation for OutputDestinationSettings
newtype OutputDestinationSettings = OutputDestinationSettings 
  { "PasswordParam" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  }


-- | Output groups for this Live Event. Output groups contain information about where streams should be distributed.
newtype OutputGroup = OutputGroup 
  { "Name" :: NullOrUndefined (String)
  , "OutputGroupSettings" :: NullOrUndefined (OutputGroupSettings)
  , "Outputs" :: NullOrUndefined (ListOfOutput)
  }


-- | Placeholder documentation for OutputGroupSettings
newtype OutputGroupSettings = OutputGroupSettings 
  { "ArchiveGroupSettings" :: NullOrUndefined (ArchiveGroupSettings)
  , "HlsGroupSettings" :: NullOrUndefined (HlsGroupSettings)
  , "MsSmoothGroupSettings" :: NullOrUndefined (MsSmoothGroupSettings)
  , "UdpGroupSettings" :: NullOrUndefined (UdpGroupSettings)
  }


-- | Reference to an OutputDestination ID defined in the channel
newtype OutputLocationRef = OutputLocationRef 
  { "DestinationRefId" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for OutputSettings
newtype OutputSettings = OutputSettings 
  { "ArchiveOutputSettings" :: NullOrUndefined (ArchiveOutputSettings)
  , "HlsOutputSettings" :: NullOrUndefined (HlsOutputSettings)
  , "MsSmoothOutputSettings" :: NullOrUndefined (MsSmoothOutputSettings)
  , "UdpOutputSettings" :: NullOrUndefined (UdpOutputSettings)
  }


-- | Placeholder documentation for PassThroughSettings
newtype PassThroughSettings = PassThroughSettings 
  { 
  }


-- | Placeholder documentation for RemixSettings
newtype RemixSettings = RemixSettings 
  { "ChannelMappings" :: NullOrUndefined (ListOfAudioChannelMapping)
  , "ChannelsIn" :: NullOrUndefined (Int)
  , "ChannelsOut" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for ResourceConflict
newtype ResourceConflict = ResourceConflict 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for ResourceNotFound
newtype ResourceNotFound = ResourceNotFound 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for Scte20Convert608To708
newtype Scte20Convert608To708 = Scte20Convert608To708 String


-- | Placeholder documentation for Scte20PlusEmbeddedDestinationSettings
newtype Scte20PlusEmbeddedDestinationSettings = Scte20PlusEmbeddedDestinationSettings 
  { 
  }


-- | Placeholder documentation for Scte20SourceSettings
newtype Scte20SourceSettings = Scte20SourceSettings 
  { "Convert608To708" :: NullOrUndefined (Scte20Convert608To708)
  , "Source608ChannelNumber" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for Scte27DestinationSettings
newtype Scte27DestinationSettings = Scte27DestinationSettings 
  { 
  }


-- | Placeholder documentation for Scte27SourceSettings
newtype Scte27SourceSettings = Scte27SourceSettings 
  { "Pid" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for Scte35AposNoRegionalBlackoutBehavior
newtype Scte35AposNoRegionalBlackoutBehavior = Scte35AposNoRegionalBlackoutBehavior String


-- | Placeholder documentation for Scte35AposWebDeliveryAllowedBehavior
newtype Scte35AposWebDeliveryAllowedBehavior = Scte35AposWebDeliveryAllowedBehavior String


-- | Placeholder documentation for Scte35SpliceInsert
newtype Scte35SpliceInsert = Scte35SpliceInsert 
  { "AdAvailOffset" :: NullOrUndefined (Int)
  , "NoRegionalBlackoutFlag" :: NullOrUndefined (Scte35SpliceInsertNoRegionalBlackoutBehavior)
  , "WebDeliveryAllowedFlag" :: NullOrUndefined (Scte35SpliceInsertWebDeliveryAllowedBehavior)
  }


-- | Placeholder documentation for Scte35SpliceInsertNoRegionalBlackoutBehavior
newtype Scte35SpliceInsertNoRegionalBlackoutBehavior = Scte35SpliceInsertNoRegionalBlackoutBehavior String


-- | Placeholder documentation for Scte35SpliceInsertWebDeliveryAllowedBehavior
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior = Scte35SpliceInsertWebDeliveryAllowedBehavior String


-- | Placeholder documentation for Scte35TimeSignalApos
newtype Scte35TimeSignalApos = Scte35TimeSignalApos 
  { "AdAvailOffset" :: NullOrUndefined (Int)
  , "NoRegionalBlackoutFlag" :: NullOrUndefined (Scte35AposNoRegionalBlackoutBehavior)
  , "WebDeliveryAllowedFlag" :: NullOrUndefined (Scte35AposWebDeliveryAllowedBehavior)
  }


-- | Placeholder documentation for SmoothGroupAudioOnlyTimecodeControl
newtype SmoothGroupAudioOnlyTimecodeControl = SmoothGroupAudioOnlyTimecodeControl String


-- | Placeholder documentation for SmoothGroupCertificateMode
newtype SmoothGroupCertificateMode = SmoothGroupCertificateMode String


-- | Placeholder documentation for SmoothGroupEventIdMode
newtype SmoothGroupEventIdMode = SmoothGroupEventIdMode String


-- | Placeholder documentation for SmoothGroupEventStopBehavior
newtype SmoothGroupEventStopBehavior = SmoothGroupEventStopBehavior String


-- | Placeholder documentation for SmoothGroupSegmentationMode
newtype SmoothGroupSegmentationMode = SmoothGroupSegmentationMode String


-- | Placeholder documentation for SmoothGroupSparseTrackType
newtype SmoothGroupSparseTrackType = SmoothGroupSparseTrackType String


-- | Placeholder documentation for SmoothGroupStreamManifestBehavior
newtype SmoothGroupStreamManifestBehavior = SmoothGroupStreamManifestBehavior String


-- | Placeholder documentation for SmoothGroupTimestampOffsetMode
newtype SmoothGroupTimestampOffsetMode = SmoothGroupTimestampOffsetMode String


-- | Placeholder documentation for SmpteTtDestinationSettings
newtype SmpteTtDestinationSettings = SmpteTtDestinationSettings 
  { 
  }


-- | Placeholder documentation for StandardHlsSettings
newtype StandardHlsSettings = StandardHlsSettings 
  { "AudioRenditionSets" :: NullOrUndefined (String)
  , "M3u8Settings" :: NullOrUndefined (M3u8Settings)
  }


-- | Placeholder documentation for StartChannelRequest
newtype StartChannelRequest = StartChannelRequest 
  { "ChannelId" :: (String)
  }


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


-- | Placeholder documentation for StaticKeySettings
newtype StaticKeySettings = StaticKeySettings 
  { "KeyProviderServer" :: NullOrUndefined (InputLocation)
  , "StaticKeyValue" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for StopChannelRequest
newtype StopChannelRequest = StopChannelRequest 
  { "ChannelId" :: (String)
  }


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


-- | Placeholder documentation for TeletextDestinationSettings
newtype TeletextDestinationSettings = TeletextDestinationSettings 
  { 
  }


-- | Placeholder documentation for TeletextSourceSettings
newtype TeletextSourceSettings = TeletextSourceSettings 
  { "PageNumber" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for TimecodeConfig
newtype TimecodeConfig = TimecodeConfig 
  { "Source" :: NullOrUndefined (TimecodeConfigSource)
  , "SyncThreshold" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for TimecodeConfigSource
newtype TimecodeConfigSource = TimecodeConfigSource String


-- | Placeholder documentation for TooManyRequestsException
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for TtmlDestinationSettings
newtype TtmlDestinationSettings = TtmlDestinationSettings 
  { "StyleControl" :: NullOrUndefined (TtmlDestinationStyleControl)
  }


-- | Placeholder documentation for TtmlDestinationStyleControl
newtype TtmlDestinationStyleControl = TtmlDestinationStyleControl String


-- | Placeholder documentation for UdpContainerSettings
newtype UdpContainerSettings = UdpContainerSettings 
  { "M2tsSettings" :: NullOrUndefined (M2tsSettings)
  }


-- | Placeholder documentation for UdpGroupSettings
newtype UdpGroupSettings = UdpGroupSettings 
  { "InputLossAction" :: NullOrUndefined (InputLossActionForUdpOut)
  , "TimedMetadataId3Frame" :: NullOrUndefined (UdpTimedMetadataId3Frame)
  , "TimedMetadataId3Period" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for UdpOutputSettings
newtype UdpOutputSettings = UdpOutputSettings 
  { "BufferMsec" :: NullOrUndefined (Int)
  , "ContainerSettings" :: NullOrUndefined (UdpContainerSettings)
  , "Destination" :: NullOrUndefined (OutputLocationRef)
  , "FecOutputSettings" :: NullOrUndefined (FecOutputSettings)
  }


-- | Placeholder documentation for UdpTimedMetadataId3Frame
newtype UdpTimedMetadataId3Frame = UdpTimedMetadataId3Frame String


-- | Placeholder documentation for UnprocessableEntityException
newtype UnprocessableEntityException = UnprocessableEntityException 
  { "Message" :: NullOrUndefined (String)
  , "ValidationErrors" :: NullOrUndefined (ListOfValidationError)
  }


-- | Placeholder documentation for UpdateChannel
newtype UpdateChannel = UpdateChannel 
  { "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }


-- | A request to update a channel.
newtype UpdateChannelRequest = UpdateChannelRequest 
  { "ChannelId" :: (String)
  , "Destinations" :: NullOrUndefined (ListOfOutputDestination)
  , "EncoderSettings" :: NullOrUndefined (EncoderSettings)
  , "InputSpecification" :: NullOrUndefined (InputSpecification)
  , "Name" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for UpdateChannelResponse
newtype UpdateChannelResponse = UpdateChannelResponse 
  { "Channel" :: NullOrUndefined (Channel)
  }


-- | The updated channel's description.
newtype UpdateChannelResultModel = UpdateChannelResultModel 
  { "Channel" :: NullOrUndefined (Channel)
  }


-- | Placeholder documentation for ValidationError
newtype ValidationError = ValidationError 
  { "ElementPath" :: NullOrUndefined (String)
  , "ErrorMessage" :: NullOrUndefined (String)
  }


-- | Placeholder documentation for VideoCodecSettings
newtype VideoCodecSettings = VideoCodecSettings 
  { "H264Settings" :: NullOrUndefined (H264Settings)
  }


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


-- | Placeholder documentation for VideoDescriptionRespondToAfd
newtype VideoDescriptionRespondToAfd = VideoDescriptionRespondToAfd String


-- | Placeholder documentation for VideoDescriptionScalingBehavior
newtype VideoDescriptionScalingBehavior = VideoDescriptionScalingBehavior String


-- | Specifies a particular video stream within an input source. An input may have only a single video selector.
newtype VideoSelector = VideoSelector 
  { "ColorSpace" :: NullOrUndefined (VideoSelectorColorSpace)
  , "ColorSpaceUsage" :: NullOrUndefined (VideoSelectorColorSpaceUsage)
  , "SelectorSettings" :: NullOrUndefined (VideoSelectorSettings)
  }


-- | Placeholder documentation for VideoSelectorColorSpace
newtype VideoSelectorColorSpace = VideoSelectorColorSpace String


-- | Placeholder documentation for VideoSelectorColorSpaceUsage
newtype VideoSelectorColorSpaceUsage = VideoSelectorColorSpaceUsage String


-- | Placeholder documentation for VideoSelectorPid
newtype VideoSelectorPid = VideoSelectorPid 
  { "Pid" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for VideoSelectorProgramId
newtype VideoSelectorProgramId = VideoSelectorProgramId 
  { "ProgramId" :: NullOrUndefined (Int)
  }


-- | Placeholder documentation for VideoSelectorSettings
newtype VideoSelectorSettings = VideoSelectorSettings 
  { "VideoSelectorPid" :: NullOrUndefined (VideoSelectorPid)
  , "VideoSelectorProgramId" :: NullOrUndefined (VideoSelectorProgramId)
  }


-- | Placeholder documentation for WebvttDestinationSettings
newtype WebvttDestinationSettings = WebvttDestinationSettings 
  { 
  }
