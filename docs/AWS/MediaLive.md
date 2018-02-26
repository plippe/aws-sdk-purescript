## Module AWS.MediaLive

API for AWS Elemental MediaLive

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createChannel`

``` purescript
createChannel :: forall eff. CreateChannelRequest -> Aff (err :: RequestError | eff) CreateChannelResponse
```

Creates a new channel

#### `createInput`

``` purescript
createInput :: forall eff. CreateInputRequest -> Aff (err :: RequestError | eff) CreateInputResponse
```

Create an input

#### `createInputSecurityGroup`

``` purescript
createInputSecurityGroup :: forall eff. CreateInputSecurityGroupRequest -> Aff (err :: RequestError | eff) CreateInputSecurityGroupResponse
```

Creates a Input Security Group

#### `deleteChannel`

``` purescript
deleteChannel :: forall eff. DeleteChannelRequest -> Aff (err :: RequestError | eff) DeleteChannelResponse
```

Starts deletion of channel. The associated outputs are also deleted.

#### `deleteInput`

``` purescript
deleteInput :: forall eff. DeleteInputRequest -> Aff (err :: RequestError | eff) DeleteInputResponse
```

Deletes the input end point

#### `deleteInputSecurityGroup`

``` purescript
deleteInputSecurityGroup :: forall eff. DeleteInputSecurityGroupRequest -> Aff (err :: RequestError | eff) DeleteInputSecurityGroupResponse
```

Deletes an Input Security Group

#### `describeChannel`

``` purescript
describeChannel :: forall eff. DescribeChannelRequest -> Aff (err :: RequestError | eff) DescribeChannelResponse
```

Gets details about a channel

#### `describeInput`

``` purescript
describeInput :: forall eff. DescribeInputRequest -> Aff (err :: RequestError | eff) DescribeInputResponse
```

Produces details about an input

#### `describeInputSecurityGroup`

``` purescript
describeInputSecurityGroup :: forall eff. DescribeInputSecurityGroupRequest -> Aff (err :: RequestError | eff) DescribeInputSecurityGroupResponse
```

Produces a summary of an Input Security Group

#### `listChannels`

``` purescript
listChannels :: forall eff. ListChannelsRequest -> Aff (err :: RequestError | eff) ListChannelsResponse
```

Produces list of channels that have been created

#### `listInputSecurityGroups`

``` purescript
listInputSecurityGroups :: forall eff. ListInputSecurityGroupsRequest -> Aff (err :: RequestError | eff) ListInputSecurityGroupsResponse
```

Produces a list of Input Security Groups for an account

#### `listInputs`

``` purescript
listInputs :: forall eff. ListInputsRequest -> Aff (err :: RequestError | eff) ListInputsResponse
```

Produces list of inputs that have been created

#### `startChannel`

``` purescript
startChannel :: forall eff. StartChannelRequest -> Aff (err :: RequestError | eff) StartChannelResponse
```

Starts an existing channel

#### `stopChannel`

``` purescript
stopChannel :: forall eff. StopChannelRequest -> Aff (err :: RequestError | eff) StopChannelResponse
```

Stops a running channel

#### `updateChannel`

``` purescript
updateChannel :: forall eff. UpdateChannelRequest -> Aff (err :: RequestError | eff) UpdateChannelResponse
```

Updates a channel.

#### `AacCodingMode`

``` purescript
newtype AacCodingMode
  = AacCodingMode String
```

Placeholder documentation for AacCodingMode

#### `AacInputType`

``` purescript
newtype AacInputType
  = AacInputType String
```

Placeholder documentation for AacInputType

#### `AacProfile`

``` purescript
newtype AacProfile
  = AacProfile String
```

Placeholder documentation for AacProfile

#### `AacRateControlMode`

``` purescript
newtype AacRateControlMode
  = AacRateControlMode String
```

Placeholder documentation for AacRateControlMode

#### `AacRawFormat`

``` purescript
newtype AacRawFormat
  = AacRawFormat String
```

Placeholder documentation for AacRawFormat

#### `AacSettings`

``` purescript
newtype AacSettings
  = AacSettings { "Bitrate" :: NullOrUndefined (Number), "CodingMode" :: NullOrUndefined (AacCodingMode), "InputType" :: NullOrUndefined (AacInputType), "Profile" :: NullOrUndefined (AacProfile), "RateControlMode" :: NullOrUndefined (AacRateControlMode), "RawFormat" :: NullOrUndefined (AacRawFormat), "SampleRate" :: NullOrUndefined (Number), "Spec" :: NullOrUndefined (AacSpec), "VbrQuality" :: NullOrUndefined (AacVbrQuality) }
```

Placeholder documentation for AacSettings

#### `AacSpec`

``` purescript
newtype AacSpec
  = AacSpec String
```

Placeholder documentation for AacSpec

#### `AacVbrQuality`

``` purescript
newtype AacVbrQuality
  = AacVbrQuality String
```

Placeholder documentation for AacVbrQuality

#### `Ac3BitstreamMode`

``` purescript
newtype Ac3BitstreamMode
  = Ac3BitstreamMode String
```

Placeholder documentation for Ac3BitstreamMode

#### `Ac3CodingMode`

``` purescript
newtype Ac3CodingMode
  = Ac3CodingMode String
```

Placeholder documentation for Ac3CodingMode

#### `Ac3DrcProfile`

``` purescript
newtype Ac3DrcProfile
  = Ac3DrcProfile String
```

Placeholder documentation for Ac3DrcProfile

#### `Ac3LfeFilter`

``` purescript
newtype Ac3LfeFilter
  = Ac3LfeFilter String
```

Placeholder documentation for Ac3LfeFilter

#### `Ac3MetadataControl`

``` purescript
newtype Ac3MetadataControl
  = Ac3MetadataControl String
```

Placeholder documentation for Ac3MetadataControl

#### `Ac3Settings`

``` purescript
newtype Ac3Settings
  = Ac3Settings { "Bitrate" :: NullOrUndefined (Number), "BitstreamMode" :: NullOrUndefined (Ac3BitstreamMode), "CodingMode" :: NullOrUndefined (Ac3CodingMode), "Dialnorm" :: NullOrUndefined (Int), "DrcProfile" :: NullOrUndefined (Ac3DrcProfile), "LfeFilter" :: NullOrUndefined (Ac3LfeFilter), "MetadataControl" :: NullOrUndefined (Ac3MetadataControl) }
```

Placeholder documentation for Ac3Settings

#### `AccessDenied`

``` purescript
newtype AccessDenied
  = AccessDenied { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for AccessDenied

#### `AfdSignaling`

``` purescript
newtype AfdSignaling
  = AfdSignaling String
```

Placeholder documentation for AfdSignaling

#### `ArchiveContainerSettings`

``` purescript
newtype ArchiveContainerSettings
  = ArchiveContainerSettings { "M2tsSettings" :: NullOrUndefined (M2tsSettings) }
```

Placeholder documentation for ArchiveContainerSettings

#### `ArchiveGroupSettings`

``` purescript
newtype ArchiveGroupSettings
  = ArchiveGroupSettings { "Destination" :: NullOrUndefined (OutputLocationRef), "RolloverInterval" :: NullOrUndefined (Int) }
```

Placeholder documentation for ArchiveGroupSettings

#### `ArchiveOutputSettings`

``` purescript
newtype ArchiveOutputSettings
  = ArchiveOutputSettings { "ContainerSettings" :: NullOrUndefined (ArchiveContainerSettings), "Extension" :: NullOrUndefined (String), "NameModifier" :: NullOrUndefined (String) }
```

Placeholder documentation for ArchiveOutputSettings

#### `AribDestinationSettings`

``` purescript
newtype AribDestinationSettings
  = AribDestinationSettings {  }
```

Placeholder documentation for AribDestinationSettings

#### `AribSourceSettings`

``` purescript
newtype AribSourceSettings
  = AribSourceSettings {  }
```

Placeholder documentation for AribSourceSettings

#### `AudioChannelMapping`

``` purescript
newtype AudioChannelMapping
  = AudioChannelMapping { "InputChannelLevels" :: NullOrUndefined (ListOfInputChannelLevel), "OutputChannel" :: NullOrUndefined (Int) }
```

Placeholder documentation for AudioChannelMapping

#### `AudioCodecSettings`

``` purescript
newtype AudioCodecSettings
  = AudioCodecSettings { "AacSettings" :: NullOrUndefined (AacSettings), "Ac3Settings" :: NullOrUndefined (Ac3Settings), "Eac3Settings" :: NullOrUndefined (Eac3Settings), "Mp2Settings" :: NullOrUndefined (Mp2Settings), "PassThroughSettings" :: NullOrUndefined (PassThroughSettings) }
```

Placeholder documentation for AudioCodecSettings

#### `AudioDescription`

``` purescript
newtype AudioDescription
  = AudioDescription { "AudioNormalizationSettings" :: NullOrUndefined (AudioNormalizationSettings), "AudioSelectorName" :: NullOrUndefined (String), "AudioType" :: NullOrUndefined (AudioType), "AudioTypeControl" :: NullOrUndefined (AudioDescriptionAudioTypeControl), "CodecSettings" :: NullOrUndefined (AudioCodecSettings), "LanguageCode" :: NullOrUndefined (String), "LanguageCodeControl" :: NullOrUndefined (AudioDescriptionLanguageCodeControl), "Name" :: NullOrUndefined (String), "RemixSettings" :: NullOrUndefined (RemixSettings), "StreamName" :: NullOrUndefined (String) }
```

Placeholder documentation for AudioDescription

#### `AudioDescriptionAudioTypeControl`

``` purescript
newtype AudioDescriptionAudioTypeControl
  = AudioDescriptionAudioTypeControl String
```

Placeholder documentation for AudioDescriptionAudioTypeControl

#### `AudioDescriptionLanguageCodeControl`

``` purescript
newtype AudioDescriptionLanguageCodeControl
  = AudioDescriptionLanguageCodeControl String
```

Placeholder documentation for AudioDescriptionLanguageCodeControl

#### `AudioLanguageSelection`

``` purescript
newtype AudioLanguageSelection
  = AudioLanguageSelection { "LanguageCode" :: NullOrUndefined (String), "LanguageSelectionPolicy" :: NullOrUndefined (AudioLanguageSelectionPolicy) }
```

Placeholder documentation for AudioLanguageSelection

#### `AudioLanguageSelectionPolicy`

``` purescript
newtype AudioLanguageSelectionPolicy
  = AudioLanguageSelectionPolicy String
```

Placeholder documentation for AudioLanguageSelectionPolicy

#### `AudioNormalizationAlgorithm`

``` purescript
newtype AudioNormalizationAlgorithm
  = AudioNormalizationAlgorithm String
```

Placeholder documentation for AudioNormalizationAlgorithm

#### `AudioNormalizationAlgorithmControl`

``` purescript
newtype AudioNormalizationAlgorithmControl
  = AudioNormalizationAlgorithmControl String
```

Placeholder documentation for AudioNormalizationAlgorithmControl

#### `AudioNormalizationSettings`

``` purescript
newtype AudioNormalizationSettings
  = AudioNormalizationSettings { "Algorithm" :: NullOrUndefined (AudioNormalizationAlgorithm), "AlgorithmControl" :: NullOrUndefined (AudioNormalizationAlgorithmControl), "TargetLkfs" :: NullOrUndefined (Number) }
```

Placeholder documentation for AudioNormalizationSettings

#### `AudioOnlyHlsSettings`

``` purescript
newtype AudioOnlyHlsSettings
  = AudioOnlyHlsSettings { "AudioGroupId" :: NullOrUndefined (String), "AudioOnlyImage" :: NullOrUndefined (InputLocation), "AudioTrackType" :: NullOrUndefined (AudioOnlyHlsTrackType) }
```

Placeholder documentation for AudioOnlyHlsSettings

#### `AudioOnlyHlsTrackType`

``` purescript
newtype AudioOnlyHlsTrackType
  = AudioOnlyHlsTrackType String
```

Placeholder documentation for AudioOnlyHlsTrackType

#### `AudioPidSelection`

``` purescript
newtype AudioPidSelection
  = AudioPidSelection { "Pid" :: NullOrUndefined (Int) }
```

Placeholder documentation for AudioPidSelection

#### `AudioSelector`

``` purescript
newtype AudioSelector
  = AudioSelector { "Name" :: NullOrUndefined (String), "SelectorSettings" :: NullOrUndefined (AudioSelectorSettings) }
```

Placeholder documentation for AudioSelector

#### `AudioSelectorSettings`

``` purescript
newtype AudioSelectorSettings
  = AudioSelectorSettings { "AudioLanguageSelection" :: NullOrUndefined (AudioLanguageSelection), "AudioPidSelection" :: NullOrUndefined (AudioPidSelection) }
```

Placeholder documentation for AudioSelectorSettings

#### `AudioType`

``` purescript
newtype AudioType
  = AudioType String
```

Placeholder documentation for AudioType

#### `AvailBlanking`

``` purescript
newtype AvailBlanking
  = AvailBlanking { "AvailBlankingImage" :: NullOrUndefined (InputLocation), "State" :: NullOrUndefined (AvailBlankingState) }
```

Placeholder documentation for AvailBlanking

#### `AvailBlankingState`

``` purescript
newtype AvailBlankingState
  = AvailBlankingState String
```

Placeholder documentation for AvailBlankingState

#### `AvailConfiguration`

``` purescript
newtype AvailConfiguration
  = AvailConfiguration { "AvailSettings" :: NullOrUndefined (AvailSettings) }
```

Placeholder documentation for AvailConfiguration

#### `AvailSettings`

``` purescript
newtype AvailSettings
  = AvailSettings { "Scte35SpliceInsert" :: NullOrUndefined (Scte35SpliceInsert), "Scte35TimeSignalApos" :: NullOrUndefined (Scte35TimeSignalApos) }
```

Placeholder documentation for AvailSettings

#### `BadGatewayException`

``` purescript
newtype BadGatewayException
  = BadGatewayException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for BadGatewayException

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for BadRequestException

#### `BlackoutSlate`

``` purescript
newtype BlackoutSlate
  = BlackoutSlate { "BlackoutSlateImage" :: NullOrUndefined (InputLocation), "NetworkEndBlackout" :: NullOrUndefined (BlackoutSlateNetworkEndBlackout), "NetworkEndBlackoutImage" :: NullOrUndefined (InputLocation), "NetworkId" :: NullOrUndefined (String), "State" :: NullOrUndefined (BlackoutSlateState) }
```

Placeholder documentation for BlackoutSlate

#### `BlackoutSlateNetworkEndBlackout`

``` purescript
newtype BlackoutSlateNetworkEndBlackout
  = BlackoutSlateNetworkEndBlackout String
```

Placeholder documentation for BlackoutSlateNetworkEndBlackout

#### `BlackoutSlateState`

``` purescript
newtype BlackoutSlateState
  = BlackoutSlateState String
```

Placeholder documentation for BlackoutSlateState

#### `BurnInAlignment`

``` purescript
newtype BurnInAlignment
  = BurnInAlignment String
```

Placeholder documentation for BurnInAlignment

#### `BurnInBackgroundColor`

``` purescript
newtype BurnInBackgroundColor
  = BurnInBackgroundColor String
```

Placeholder documentation for BurnInBackgroundColor

#### `BurnInDestinationSettings`

``` purescript
newtype BurnInDestinationSettings
  = BurnInDestinationSettings { "Alignment" :: NullOrUndefined (BurnInAlignment), "BackgroundColor" :: NullOrUndefined (BurnInBackgroundColor), "BackgroundOpacity" :: NullOrUndefined (Int), "Font" :: NullOrUndefined (InputLocation), "FontColor" :: NullOrUndefined (BurnInFontColor), "FontOpacity" :: NullOrUndefined (Int), "FontResolution" :: NullOrUndefined (Int), "FontSize" :: NullOrUndefined (String), "OutlineColor" :: NullOrUndefined (BurnInOutlineColor), "OutlineSize" :: NullOrUndefined (Int), "ShadowColor" :: NullOrUndefined (BurnInShadowColor), "ShadowOpacity" :: NullOrUndefined (Int), "ShadowXOffset" :: NullOrUndefined (Int), "ShadowYOffset" :: NullOrUndefined (Int), "TeletextGridControl" :: NullOrUndefined (BurnInTeletextGridControl), "XPosition" :: NullOrUndefined (Int), "YPosition" :: NullOrUndefined (Int) }
```

Placeholder documentation for BurnInDestinationSettings

#### `BurnInFontColor`

``` purescript
newtype BurnInFontColor
  = BurnInFontColor String
```

Placeholder documentation for BurnInFontColor

#### `BurnInOutlineColor`

``` purescript
newtype BurnInOutlineColor
  = BurnInOutlineColor String
```

Placeholder documentation for BurnInOutlineColor

#### `BurnInShadowColor`

``` purescript
newtype BurnInShadowColor
  = BurnInShadowColor String
```

Placeholder documentation for BurnInShadowColor

#### `BurnInTeletextGridControl`

``` purescript
newtype BurnInTeletextGridControl
  = BurnInTeletextGridControl String
```

Placeholder documentation for BurnInTeletextGridControl

#### `CaptionDescription`

``` purescript
newtype CaptionDescription
  = CaptionDescription { "CaptionSelectorName" :: NullOrUndefined (String), "DestinationSettings" :: NullOrUndefined (CaptionDestinationSettings), "LanguageCode" :: NullOrUndefined (String), "LanguageDescription" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Output groups for this Live Event. Output groups contain information about where streams should be distributed.

#### `CaptionDestinationSettings`

``` purescript
newtype CaptionDestinationSettings
  = CaptionDestinationSettings { "AribDestinationSettings" :: NullOrUndefined (AribDestinationSettings), "BurnInDestinationSettings" :: NullOrUndefined (BurnInDestinationSettings), "DvbSubDestinationSettings" :: NullOrUndefined (DvbSubDestinationSettings), "EmbeddedDestinationSettings" :: NullOrUndefined (EmbeddedDestinationSettings), "EmbeddedPlusScte20DestinationSettings" :: NullOrUndefined (EmbeddedPlusScte20DestinationSettings), "Scte20PlusEmbeddedDestinationSettings" :: NullOrUndefined (Scte20PlusEmbeddedDestinationSettings), "Scte27DestinationSettings" :: NullOrUndefined (Scte27DestinationSettings), "SmpteTtDestinationSettings" :: NullOrUndefined (SmpteTtDestinationSettings), "TeletextDestinationSettings" :: NullOrUndefined (TeletextDestinationSettings), "TtmlDestinationSettings" :: NullOrUndefined (TtmlDestinationSettings), "WebvttDestinationSettings" :: NullOrUndefined (WebvttDestinationSettings) }
```

Placeholder documentation for CaptionDestinationSettings

#### `CaptionLanguageMapping`

``` purescript
newtype CaptionLanguageMapping
  = CaptionLanguageMapping { "CaptionChannel" :: NullOrUndefined (Int), "LanguageCode" :: NullOrUndefined (String), "LanguageDescription" :: NullOrUndefined (String) }
```

Maps a caption channel to an ISO 693-2 language code (http://www.loc.gov/standards/iso639-2), with an optional description.

#### `CaptionSelector`

``` purescript
newtype CaptionSelector
  = CaptionSelector { "LanguageCode" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SelectorSettings" :: NullOrUndefined (CaptionSelectorSettings) }
```

Output groups for this Live Event. Output groups contain information about where streams should be distributed.

#### `CaptionSelectorSettings`

``` purescript
newtype CaptionSelectorSettings
  = CaptionSelectorSettings { "AribSourceSettings" :: NullOrUndefined (AribSourceSettings), "DvbSubSourceSettings" :: NullOrUndefined (DvbSubSourceSettings), "EmbeddedSourceSettings" :: NullOrUndefined (EmbeddedSourceSettings), "Scte20SourceSettings" :: NullOrUndefined (Scte20SourceSettings), "Scte27SourceSettings" :: NullOrUndefined (Scte27SourceSettings), "TeletextSourceSettings" :: NullOrUndefined (TeletextSourceSettings) }
```

Placeholder documentation for CaptionSelectorSettings

#### `Channel`

``` purescript
newtype Channel
  = Channel { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for Channel

#### `ChannelConfigurationValidationError`

``` purescript
newtype ChannelConfigurationValidationError
  = ChannelConfigurationValidationError { "Message" :: NullOrUndefined (String), "ValidationErrors" :: NullOrUndefined (ListOfValidationError) }
```

Placeholder documentation for ChannelConfigurationValidationError

#### `ChannelEgressEndpoint`

``` purescript
newtype ChannelEgressEndpoint
  = ChannelEgressEndpoint { "SourceIp" :: NullOrUndefined (String) }
```

Placeholder documentation for ChannelEgressEndpoint

#### `ChannelState`

``` purescript
newtype ChannelState
  = ChannelState String
```

Placeholder documentation for ChannelState

#### `ChannelSummary`

``` purescript
newtype ChannelSummary
  = ChannelSummary { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for ChannelSummary

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for ConflictException

#### `CreateChannel`

``` purescript
newtype CreateChannel
  = CreateChannel { "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Reserved" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Placeholder documentation for CreateChannel

#### `CreateChannelRequest`

``` purescript
newtype CreateChannelRequest
  = CreateChannelRequest { "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Reserved" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

A request to create a channel

#### `CreateChannelResponse`

``` purescript
newtype CreateChannelResponse
  = CreateChannelResponse { "Channel" :: NullOrUndefined (Channel) }
```

Placeholder documentation for CreateChannelResponse

#### `CreateChannelResultModel`

``` purescript
newtype CreateChannelResultModel
  = CreateChannelResultModel { "Channel" :: NullOrUndefined (Channel) }
```

Placeholder documentation for CreateChannelResultModel

#### `CreateInput`

``` purescript
newtype CreateInput
  = CreateInput { "Destinations" :: NullOrUndefined (ListOfInputDestinationRequest), "InputSecurityGroups" :: NullOrUndefined (ListOf__string), "Name" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Sources" :: NullOrUndefined (ListOfInputSourceRequest), "Type" :: NullOrUndefined (InputType) }
```

Placeholder documentation for CreateInput

#### `CreateInputRequest`

``` purescript
newtype CreateInputRequest
  = CreateInputRequest { "Destinations" :: NullOrUndefined (ListOfInputDestinationRequest), "InputSecurityGroups" :: NullOrUndefined (ListOf__string), "Name" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Sources" :: NullOrUndefined (ListOfInputSourceRequest), "Type" :: NullOrUndefined (InputType) }
```

The name of the input

#### `CreateInputResponse`

``` purescript
newtype CreateInputResponse
  = CreateInputResponse { "Input" :: NullOrUndefined (Input) }
```

Placeholder documentation for CreateInputResponse

#### `CreateInputResultModel`

``` purescript
newtype CreateInputResultModel
  = CreateInputResultModel { "Input" :: NullOrUndefined (Input) }
```

Placeholder documentation for CreateInputResultModel

#### `CreateInputSecurityGroupRequest`

``` purescript
newtype CreateInputSecurityGroupRequest
  = CreateInputSecurityGroupRequest { "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRuleCidr) }
```

The IPv4 CIDRs to whitelist for this Input Security Group

#### `CreateInputSecurityGroupResponse`

``` purescript
newtype CreateInputSecurityGroupResponse
  = CreateInputSecurityGroupResponse { "SecurityGroup" :: NullOrUndefined (InputSecurityGroup) }
```

Placeholder documentation for CreateInputSecurityGroupResponse

#### `CreateInputSecurityGroupResultModel`

``` purescript
newtype CreateInputSecurityGroupResultModel
  = CreateInputSecurityGroupResultModel { "SecurityGroup" :: NullOrUndefined (InputSecurityGroup) }
```

Placeholder documentation for CreateInputSecurityGroupResultModel

#### `DeleteChannelRequest`

``` purescript
newtype DeleteChannelRequest
  = DeleteChannelRequest { "ChannelId" :: String }
```

Placeholder documentation for DeleteChannelRequest

#### `DeleteChannelResponse`

``` purescript
newtype DeleteChannelResponse
  = DeleteChannelResponse { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for DeleteChannelResponse

#### `DeleteInputRequest`

``` purescript
newtype DeleteInputRequest
  = DeleteInputRequest { "InputId" :: String }
```

Placeholder documentation for DeleteInputRequest

#### `DeleteInputResponse`

``` purescript
newtype DeleteInputResponse
  = DeleteInputResponse {  }
```

Placeholder documentation for DeleteInputResponse

#### `DeleteInputSecurityGroupRequest`

``` purescript
newtype DeleteInputSecurityGroupRequest
  = DeleteInputSecurityGroupRequest { "InputSecurityGroupId" :: String }
```

Placeholder documentation for DeleteInputSecurityGroupRequest

#### `DeleteInputSecurityGroupResponse`

``` purescript
newtype DeleteInputSecurityGroupResponse
  = DeleteInputSecurityGroupResponse {  }
```

Placeholder documentation for DeleteInputSecurityGroupResponse

#### `DescribeChannelRequest`

``` purescript
newtype DescribeChannelRequest
  = DescribeChannelRequest { "ChannelId" :: String }
```

Placeholder documentation for DescribeChannelRequest

#### `DescribeChannelResponse`

``` purescript
newtype DescribeChannelResponse
  = DescribeChannelResponse { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for DescribeChannelResponse

#### `DescribeInputRequest`

``` purescript
newtype DescribeInputRequest
  = DescribeInputRequest { "InputId" :: String }
```

Placeholder documentation for DescribeInputRequest

#### `DescribeInputResponse`

``` purescript
newtype DescribeInputResponse
  = DescribeInputResponse { "Arn" :: NullOrUndefined (String), "AttachedChannels" :: NullOrUndefined (ListOf__string), "Destinations" :: NullOrUndefined (ListOfInputDestination), "Id" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SecurityGroups" :: NullOrUndefined (ListOf__string), "Sources" :: NullOrUndefined (ListOfInputSource), "State" :: NullOrUndefined (InputState), "Type" :: NullOrUndefined (InputType) }
```

Placeholder documentation for DescribeInputResponse

#### `DescribeInputSecurityGroupRequest`

``` purescript
newtype DescribeInputSecurityGroupRequest
  = DescribeInputSecurityGroupRequest { "InputSecurityGroupId" :: String }
```

Placeholder documentation for DescribeInputSecurityGroupRequest

#### `DescribeInputSecurityGroupResponse`

``` purescript
newtype DescribeInputSecurityGroupResponse
  = DescribeInputSecurityGroupResponse { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRule) }
```

Placeholder documentation for DescribeInputSecurityGroupResponse

#### `DvbNitSettings`

``` purescript
newtype DvbNitSettings
  = DvbNitSettings { "NetworkId" :: NullOrUndefined (Int), "NetworkName" :: NullOrUndefined (String), "RepInterval" :: NullOrUndefined (Int) }
```

DVB Network Information Table (NIT)

#### `DvbSdtOutputSdt`

``` purescript
newtype DvbSdtOutputSdt
  = DvbSdtOutputSdt String
```

Placeholder documentation for DvbSdtOutputSdt

#### `DvbSdtSettings`

``` purescript
newtype DvbSdtSettings
  = DvbSdtSettings { "OutputSdt" :: NullOrUndefined (DvbSdtOutputSdt), "RepInterval" :: NullOrUndefined (Int), "ServiceName" :: NullOrUndefined (String), "ServiceProviderName" :: NullOrUndefined (String) }
```

DVB Service Description Table (SDT)

#### `DvbSubDestinationAlignment`

``` purescript
newtype DvbSubDestinationAlignment
  = DvbSubDestinationAlignment String
```

Placeholder documentation for DvbSubDestinationAlignment

#### `DvbSubDestinationBackgroundColor`

``` purescript
newtype DvbSubDestinationBackgroundColor
  = DvbSubDestinationBackgroundColor String
```

Placeholder documentation for DvbSubDestinationBackgroundColor

#### `DvbSubDestinationFontColor`

``` purescript
newtype DvbSubDestinationFontColor
  = DvbSubDestinationFontColor String
```

Placeholder documentation for DvbSubDestinationFontColor

#### `DvbSubDestinationOutlineColor`

``` purescript
newtype DvbSubDestinationOutlineColor
  = DvbSubDestinationOutlineColor String
```

Placeholder documentation for DvbSubDestinationOutlineColor

#### `DvbSubDestinationSettings`

``` purescript
newtype DvbSubDestinationSettings
  = DvbSubDestinationSettings { "Alignment" :: NullOrUndefined (DvbSubDestinationAlignment), "BackgroundColor" :: NullOrUndefined (DvbSubDestinationBackgroundColor), "BackgroundOpacity" :: NullOrUndefined (Int), "Font" :: NullOrUndefined (InputLocation), "FontColor" :: NullOrUndefined (DvbSubDestinationFontColor), "FontOpacity" :: NullOrUndefined (Int), "FontResolution" :: NullOrUndefined (Int), "FontSize" :: NullOrUndefined (String), "OutlineColor" :: NullOrUndefined (DvbSubDestinationOutlineColor), "OutlineSize" :: NullOrUndefined (Int), "ShadowColor" :: NullOrUndefined (DvbSubDestinationShadowColor), "ShadowOpacity" :: NullOrUndefined (Int), "ShadowXOffset" :: NullOrUndefined (Int), "ShadowYOffset" :: NullOrUndefined (Int), "TeletextGridControl" :: NullOrUndefined (DvbSubDestinationTeletextGridControl), "XPosition" :: NullOrUndefined (Int), "YPosition" :: NullOrUndefined (Int) }
```

Placeholder documentation for DvbSubDestinationSettings

#### `DvbSubDestinationShadowColor`

``` purescript
newtype DvbSubDestinationShadowColor
  = DvbSubDestinationShadowColor String
```

Placeholder documentation for DvbSubDestinationShadowColor

#### `DvbSubDestinationTeletextGridControl`

``` purescript
newtype DvbSubDestinationTeletextGridControl
  = DvbSubDestinationTeletextGridControl String
```

Placeholder documentation for DvbSubDestinationTeletextGridControl

#### `DvbSubSourceSettings`

``` purescript
newtype DvbSubSourceSettings
  = DvbSubSourceSettings { "Pid" :: NullOrUndefined (Int) }
```

Placeholder documentation for DvbSubSourceSettings

#### `DvbTdtSettings`

``` purescript
newtype DvbTdtSettings
  = DvbTdtSettings { "RepInterval" :: NullOrUndefined (Int) }
```

DVB Time and Date Table (SDT)

#### `Eac3AttenuationControl`

``` purescript
newtype Eac3AttenuationControl
  = Eac3AttenuationControl String
```

Placeholder documentation for Eac3AttenuationControl

#### `Eac3BitstreamMode`

``` purescript
newtype Eac3BitstreamMode
  = Eac3BitstreamMode String
```

Placeholder documentation for Eac3BitstreamMode

#### `Eac3CodingMode`

``` purescript
newtype Eac3CodingMode
  = Eac3CodingMode String
```

Placeholder documentation for Eac3CodingMode

#### `Eac3DcFilter`

``` purescript
newtype Eac3DcFilter
  = Eac3DcFilter String
```

Placeholder documentation for Eac3DcFilter

#### `Eac3DrcLine`

``` purescript
newtype Eac3DrcLine
  = Eac3DrcLine String
```

Placeholder documentation for Eac3DrcLine

#### `Eac3DrcRf`

``` purescript
newtype Eac3DrcRf
  = Eac3DrcRf String
```

Placeholder documentation for Eac3DrcRf

#### `Eac3LfeControl`

``` purescript
newtype Eac3LfeControl
  = Eac3LfeControl String
```

Placeholder documentation for Eac3LfeControl

#### `Eac3LfeFilter`

``` purescript
newtype Eac3LfeFilter
  = Eac3LfeFilter String
```

Placeholder documentation for Eac3LfeFilter

#### `Eac3MetadataControl`

``` purescript
newtype Eac3MetadataControl
  = Eac3MetadataControl String
```

Placeholder documentation for Eac3MetadataControl

#### `Eac3PassthroughControl`

``` purescript
newtype Eac3PassthroughControl
  = Eac3PassthroughControl String
```

Placeholder documentation for Eac3PassthroughControl

#### `Eac3PhaseControl`

``` purescript
newtype Eac3PhaseControl
  = Eac3PhaseControl String
```

Placeholder documentation for Eac3PhaseControl

#### `Eac3Settings`

``` purescript
newtype Eac3Settings
  = Eac3Settings { "AttenuationControl" :: NullOrUndefined (Eac3AttenuationControl), "Bitrate" :: NullOrUndefined (Number), "BitstreamMode" :: NullOrUndefined (Eac3BitstreamMode), "CodingMode" :: NullOrUndefined (Eac3CodingMode), "DcFilter" :: NullOrUndefined (Eac3DcFilter), "Dialnorm" :: NullOrUndefined (Int), "DrcLine" :: NullOrUndefined (Eac3DrcLine), "DrcRf" :: NullOrUndefined (Eac3DrcRf), "LfeControl" :: NullOrUndefined (Eac3LfeControl), "LfeFilter" :: NullOrUndefined (Eac3LfeFilter), "LoRoCenterMixLevel" :: NullOrUndefined (Number), "LoRoSurroundMixLevel" :: NullOrUndefined (Number), "LtRtCenterMixLevel" :: NullOrUndefined (Number), "LtRtSurroundMixLevel" :: NullOrUndefined (Number), "MetadataControl" :: NullOrUndefined (Eac3MetadataControl), "PassthroughControl" :: NullOrUndefined (Eac3PassthroughControl), "PhaseControl" :: NullOrUndefined (Eac3PhaseControl), "StereoDownmix" :: NullOrUndefined (Eac3StereoDownmix), "SurroundExMode" :: NullOrUndefined (Eac3SurroundExMode), "SurroundMode" :: NullOrUndefined (Eac3SurroundMode) }
```

Placeholder documentation for Eac3Settings

#### `Eac3StereoDownmix`

``` purescript
newtype Eac3StereoDownmix
  = Eac3StereoDownmix String
```

Placeholder documentation for Eac3StereoDownmix

#### `Eac3SurroundExMode`

``` purescript
newtype Eac3SurroundExMode
  = Eac3SurroundExMode String
```

Placeholder documentation for Eac3SurroundExMode

#### `Eac3SurroundMode`

``` purescript
newtype Eac3SurroundMode
  = Eac3SurroundMode String
```

Placeholder documentation for Eac3SurroundMode

#### `EmbeddedConvert608To708`

``` purescript
newtype EmbeddedConvert608To708
  = EmbeddedConvert608To708 String
```

Placeholder documentation for EmbeddedConvert608To708

#### `EmbeddedDestinationSettings`

``` purescript
newtype EmbeddedDestinationSettings
  = EmbeddedDestinationSettings {  }
```

Placeholder documentation for EmbeddedDestinationSettings

#### `EmbeddedPlusScte20DestinationSettings`

``` purescript
newtype EmbeddedPlusScte20DestinationSettings
  = EmbeddedPlusScte20DestinationSettings {  }
```

Placeholder documentation for EmbeddedPlusScte20DestinationSettings

#### `EmbeddedScte20Detection`

``` purescript
newtype EmbeddedScte20Detection
  = EmbeddedScte20Detection String
```

Placeholder documentation for EmbeddedScte20Detection

#### `EmbeddedSourceSettings`

``` purescript
newtype EmbeddedSourceSettings
  = EmbeddedSourceSettings { "Convert608To708" :: NullOrUndefined (EmbeddedConvert608To708), "Scte20Detection" :: NullOrUndefined (EmbeddedScte20Detection), "Source608ChannelNumber" :: NullOrUndefined (Int), "Source608TrackNumber" :: NullOrUndefined (Int) }
```

Placeholder documentation for EmbeddedSourceSettings

#### `Empty`

``` purescript
newtype Empty
  = Empty {  }
```

Placeholder documentation for Empty

#### `EncoderSettings`

``` purescript
newtype EncoderSettings
  = EncoderSettings { "AudioDescriptions" :: NullOrUndefined (ListOfAudioDescription), "AvailBlanking" :: NullOrUndefined (AvailBlanking), "AvailConfiguration" :: NullOrUndefined (AvailConfiguration), "BlackoutSlate" :: NullOrUndefined (BlackoutSlate), "CaptionDescriptions" :: NullOrUndefined (ListOfCaptionDescription), "GlobalConfiguration" :: NullOrUndefined (GlobalConfiguration), "OutputGroups" :: NullOrUndefined (ListOfOutputGroup), "TimecodeConfig" :: NullOrUndefined (TimecodeConfig), "VideoDescriptions" :: NullOrUndefined (ListOfVideoDescription) }
```

Placeholder documentation for EncoderSettings

#### `FecOutputIncludeFec`

``` purescript
newtype FecOutputIncludeFec
  = FecOutputIncludeFec String
```

Placeholder documentation for FecOutputIncludeFec

#### `FecOutputSettings`

``` purescript
newtype FecOutputSettings
  = FecOutputSettings { "ColumnDepth" :: NullOrUndefined (Int), "IncludeFec" :: NullOrUndefined (FecOutputIncludeFec), "RowLength" :: NullOrUndefined (Int) }
```

Placeholder documentation for FecOutputSettings

#### `FixedAfd`

``` purescript
newtype FixedAfd
  = FixedAfd String
```

Placeholder documentation for FixedAfd

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for ForbiddenException

#### `GatewayTimeoutException`

``` purescript
newtype GatewayTimeoutException
  = GatewayTimeoutException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for GatewayTimeoutException

#### `GlobalConfiguration`

``` purescript
newtype GlobalConfiguration
  = GlobalConfiguration { "InitialAudioGain" :: NullOrUndefined (Int), "InputEndAction" :: NullOrUndefined (GlobalConfigurationInputEndAction), "InputLossBehavior" :: NullOrUndefined (InputLossBehavior), "OutputTimingSource" :: NullOrUndefined (GlobalConfigurationOutputTimingSource), "SupportLowFramerateInputs" :: NullOrUndefined (GlobalConfigurationLowFramerateInputs) }
```

Placeholder documentation for GlobalConfiguration

#### `GlobalConfigurationInputEndAction`

``` purescript
newtype GlobalConfigurationInputEndAction
  = GlobalConfigurationInputEndAction String
```

Placeholder documentation for GlobalConfigurationInputEndAction

#### `GlobalConfigurationLowFramerateInputs`

``` purescript
newtype GlobalConfigurationLowFramerateInputs
  = GlobalConfigurationLowFramerateInputs String
```

Placeholder documentation for GlobalConfigurationLowFramerateInputs

#### `GlobalConfigurationOutputTimingSource`

``` purescript
newtype GlobalConfigurationOutputTimingSource
  = GlobalConfigurationOutputTimingSource String
```

Placeholder documentation for GlobalConfigurationOutputTimingSource

#### `H264AdaptiveQuantization`

``` purescript
newtype H264AdaptiveQuantization
  = H264AdaptiveQuantization String
```

Placeholder documentation for H264AdaptiveQuantization

#### `H264ColorMetadata`

``` purescript
newtype H264ColorMetadata
  = H264ColorMetadata String
```

Placeholder documentation for H264ColorMetadata

#### `H264EntropyEncoding`

``` purescript
newtype H264EntropyEncoding
  = H264EntropyEncoding String
```

Placeholder documentation for H264EntropyEncoding

#### `H264FlickerAq`

``` purescript
newtype H264FlickerAq
  = H264FlickerAq String
```

Placeholder documentation for H264FlickerAq

#### `H264FramerateControl`

``` purescript
newtype H264FramerateControl
  = H264FramerateControl String
```

Placeholder documentation for H264FramerateControl

#### `H264GopBReference`

``` purescript
newtype H264GopBReference
  = H264GopBReference String
```

Placeholder documentation for H264GopBReference

#### `H264GopSizeUnits`

``` purescript
newtype H264GopSizeUnits
  = H264GopSizeUnits String
```

Placeholder documentation for H264GopSizeUnits

#### `H264Level`

``` purescript
newtype H264Level
  = H264Level String
```

Placeholder documentation for H264Level

#### `H264LookAheadRateControl`

``` purescript
newtype H264LookAheadRateControl
  = H264LookAheadRateControl String
```

Placeholder documentation for H264LookAheadRateControl

#### `H264ParControl`

``` purescript
newtype H264ParControl
  = H264ParControl String
```

Placeholder documentation for H264ParControl

#### `H264Profile`

``` purescript
newtype H264Profile
  = H264Profile String
```

Placeholder documentation for H264Profile

#### `H264RateControlMode`

``` purescript
newtype H264RateControlMode
  = H264RateControlMode String
```

Placeholder documentation for H264RateControlMode

#### `H264ScanType`

``` purescript
newtype H264ScanType
  = H264ScanType String
```

Placeholder documentation for H264ScanType

#### `H264SceneChangeDetect`

``` purescript
newtype H264SceneChangeDetect
  = H264SceneChangeDetect String
```

Placeholder documentation for H264SceneChangeDetect

#### `H264Settings`

``` purescript
newtype H264Settings
  = H264Settings { "AdaptiveQuantization" :: NullOrUndefined (H264AdaptiveQuantization), "AfdSignaling" :: NullOrUndefined (AfdSignaling), "Bitrate" :: NullOrUndefined (Int), "BufFillPct" :: NullOrUndefined (Int), "BufSize" :: NullOrUndefined (Int), "ColorMetadata" :: NullOrUndefined (H264ColorMetadata), "EntropyEncoding" :: NullOrUndefined (H264EntropyEncoding), "FixedAfd" :: NullOrUndefined (FixedAfd), "FlickerAq" :: NullOrUndefined (H264FlickerAq), "FramerateControl" :: NullOrUndefined (H264FramerateControl), "FramerateDenominator" :: NullOrUndefined (Int), "FramerateNumerator" :: NullOrUndefined (Int), "GopBReference" :: NullOrUndefined (H264GopBReference), "GopClosedCadence" :: NullOrUndefined (Int), "GopNumBFrames" :: NullOrUndefined (Int), "GopSize" :: NullOrUndefined (Number), "GopSizeUnits" :: NullOrUndefined (H264GopSizeUnits), "Level" :: NullOrUndefined (H264Level), "LookAheadRateControl" :: NullOrUndefined (H264LookAheadRateControl), "MaxBitrate" :: NullOrUndefined (Int), "MinIInterval" :: NullOrUndefined (Int), "NumRefFrames" :: NullOrUndefined (Int), "ParControl" :: NullOrUndefined (H264ParControl), "ParDenominator" :: NullOrUndefined (Int), "ParNumerator" :: NullOrUndefined (Int), "Profile" :: NullOrUndefined (H264Profile), "RateControlMode" :: NullOrUndefined (H264RateControlMode), "ScanType" :: NullOrUndefined (H264ScanType), "SceneChangeDetect" :: NullOrUndefined (H264SceneChangeDetect), "Slices" :: NullOrUndefined (Int), "Softness" :: NullOrUndefined (Int), "SpatialAq" :: NullOrUndefined (H264SpatialAq), "Syntax" :: NullOrUndefined (H264Syntax), "TemporalAq" :: NullOrUndefined (H264TemporalAq), "TimecodeInsertion" :: NullOrUndefined (H264TimecodeInsertionBehavior) }
```

Placeholder documentation for H264Settings

#### `H264SpatialAq`

``` purescript
newtype H264SpatialAq
  = H264SpatialAq String
```

Placeholder documentation for H264SpatialAq

#### `H264Syntax`

``` purescript
newtype H264Syntax
  = H264Syntax String
```

Placeholder documentation for H264Syntax

#### `H264TemporalAq`

``` purescript
newtype H264TemporalAq
  = H264TemporalAq String
```

Placeholder documentation for H264TemporalAq

#### `H264TimecodeInsertionBehavior`

``` purescript
newtype H264TimecodeInsertionBehavior
  = H264TimecodeInsertionBehavior String
```

Placeholder documentation for H264TimecodeInsertionBehavior

#### `HlsAdMarkers`

``` purescript
newtype HlsAdMarkers
  = HlsAdMarkers String
```

Placeholder documentation for HlsAdMarkers

#### `HlsAkamaiHttpTransferMode`

``` purescript
newtype HlsAkamaiHttpTransferMode
  = HlsAkamaiHttpTransferMode String
```

Placeholder documentation for HlsAkamaiHttpTransferMode

#### `HlsAkamaiSettings`

``` purescript
newtype HlsAkamaiSettings
  = HlsAkamaiSettings { "ConnectionRetryInterval" :: NullOrUndefined (Int), "FilecacheDuration" :: NullOrUndefined (Int), "HttpTransferMode" :: NullOrUndefined (HlsAkamaiHttpTransferMode), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int), "Salt" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

Placeholder documentation for HlsAkamaiSettings

#### `HlsBasicPutSettings`

``` purescript
newtype HlsBasicPutSettings
  = HlsBasicPutSettings { "ConnectionRetryInterval" :: NullOrUndefined (Int), "FilecacheDuration" :: NullOrUndefined (Int), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int) }
```

Placeholder documentation for HlsBasicPutSettings

#### `HlsCaptionLanguageSetting`

``` purescript
newtype HlsCaptionLanguageSetting
  = HlsCaptionLanguageSetting String
```

Placeholder documentation for HlsCaptionLanguageSetting

#### `HlsCdnSettings`

``` purescript
newtype HlsCdnSettings
  = HlsCdnSettings { "HlsAkamaiSettings" :: NullOrUndefined (HlsAkamaiSettings), "HlsBasicPutSettings" :: NullOrUndefined (HlsBasicPutSettings), "HlsMediaStoreSettings" :: NullOrUndefined (HlsMediaStoreSettings), "HlsWebdavSettings" :: NullOrUndefined (HlsWebdavSettings) }
```

Placeholder documentation for HlsCdnSettings

#### `HlsClientCache`

``` purescript
newtype HlsClientCache
  = HlsClientCache String
```

Placeholder documentation for HlsClientCache

#### `HlsCodecSpecification`

``` purescript
newtype HlsCodecSpecification
  = HlsCodecSpecification String
```

Placeholder documentation for HlsCodecSpecification

#### `HlsDirectoryStructure`

``` purescript
newtype HlsDirectoryStructure
  = HlsDirectoryStructure String
```

Placeholder documentation for HlsDirectoryStructure

#### `HlsEncryptionType`

``` purescript
newtype HlsEncryptionType
  = HlsEncryptionType String
```

Placeholder documentation for HlsEncryptionType

#### `HlsGroupSettings`

``` purescript
newtype HlsGroupSettings
  = HlsGroupSettings { "AdMarkers" :: NullOrUndefined (ListOfHlsAdMarkers), "BaseUrlContent" :: NullOrUndefined (String), "BaseUrlManifest" :: NullOrUndefined (String), "CaptionLanguageMappings" :: NullOrUndefined (ListOfCaptionLanguageMapping), "CaptionLanguageSetting" :: NullOrUndefined (HlsCaptionLanguageSetting), "ClientCache" :: NullOrUndefined (HlsClientCache), "CodecSpecification" :: NullOrUndefined (HlsCodecSpecification), "ConstantIv" :: NullOrUndefined (String), "Destination" :: NullOrUndefined (OutputLocationRef), "DirectoryStructure" :: NullOrUndefined (HlsDirectoryStructure), "EncryptionType" :: NullOrUndefined (HlsEncryptionType), "HlsCdnSettings" :: NullOrUndefined (HlsCdnSettings), "IndexNSegments" :: NullOrUndefined (Int), "InputLossAction" :: NullOrUndefined (InputLossActionForHlsOut), "IvInManifest" :: NullOrUndefined (HlsIvInManifest), "IvSource" :: NullOrUndefined (HlsIvSource), "KeepSegments" :: NullOrUndefined (Int), "KeyFormat" :: NullOrUndefined (String), "KeyFormatVersions" :: NullOrUndefined (String), "KeyProviderSettings" :: NullOrUndefined (KeyProviderSettings), "ManifestCompression" :: NullOrUndefined (HlsManifestCompression), "ManifestDurationFormat" :: NullOrUndefined (HlsManifestDurationFormat), "MinSegmentLength" :: NullOrUndefined (Int), "Mode" :: NullOrUndefined (HlsMode), "OutputSelection" :: NullOrUndefined (HlsOutputSelection), "ProgramDateTime" :: NullOrUndefined (HlsProgramDateTime), "ProgramDateTimePeriod" :: NullOrUndefined (Int), "SegmentLength" :: NullOrUndefined (Int), "SegmentationMode" :: NullOrUndefined (HlsSegmentationMode), "SegmentsPerSubdirectory" :: NullOrUndefined (Int), "StreamInfResolution" :: NullOrUndefined (HlsStreamInfResolution), "TimedMetadataId3Frame" :: NullOrUndefined (HlsTimedMetadataId3Frame), "TimedMetadataId3Period" :: NullOrUndefined (Int), "TimestampDeltaMilliseconds" :: NullOrUndefined (Int), "TsFileMode" :: NullOrUndefined (HlsTsFileMode) }
```

Placeholder documentation for HlsGroupSettings

#### `HlsInputSettings`

``` purescript
newtype HlsInputSettings
  = HlsInputSettings { "Bandwidth" :: NullOrUndefined (Int), "BufferSegments" :: NullOrUndefined (Int), "Retries" :: NullOrUndefined (Int), "RetryInterval" :: NullOrUndefined (Int) }
```

Placeholder documentation for HlsInputSettings

#### `HlsIvInManifest`

``` purescript
newtype HlsIvInManifest
  = HlsIvInManifest String
```

Placeholder documentation for HlsIvInManifest

#### `HlsIvSource`

``` purescript
newtype HlsIvSource
  = HlsIvSource String
```

Placeholder documentation for HlsIvSource

#### `HlsManifestCompression`

``` purescript
newtype HlsManifestCompression
  = HlsManifestCompression String
```

Placeholder documentation for HlsManifestCompression

#### `HlsManifestDurationFormat`

``` purescript
newtype HlsManifestDurationFormat
  = HlsManifestDurationFormat String
```

Placeholder documentation for HlsManifestDurationFormat

#### `HlsMediaStoreSettings`

``` purescript
newtype HlsMediaStoreSettings
  = HlsMediaStoreSettings { "ConnectionRetryInterval" :: NullOrUndefined (Int), "FilecacheDuration" :: NullOrUndefined (Int), "MediaStoreStorageClass" :: NullOrUndefined (HlsMediaStoreStorageClass), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int) }
```

Placeholder documentation for HlsMediaStoreSettings

#### `HlsMediaStoreStorageClass`

``` purescript
newtype HlsMediaStoreStorageClass
  = HlsMediaStoreStorageClass String
```

Placeholder documentation for HlsMediaStoreStorageClass

#### `HlsMode`

``` purescript
newtype HlsMode
  = HlsMode String
```

Placeholder documentation for HlsMode

#### `HlsOutputSelection`

``` purescript
newtype HlsOutputSelection
  = HlsOutputSelection String
```

Placeholder documentation for HlsOutputSelection

#### `HlsOutputSettings`

``` purescript
newtype HlsOutputSettings
  = HlsOutputSettings { "HlsSettings" :: NullOrUndefined (HlsSettings), "NameModifier" :: NullOrUndefined (String), "SegmentModifier" :: NullOrUndefined (String) }
```

Placeholder documentation for HlsOutputSettings

#### `HlsProgramDateTime`

``` purescript
newtype HlsProgramDateTime
  = HlsProgramDateTime String
```

Placeholder documentation for HlsProgramDateTime

#### `HlsSegmentationMode`

``` purescript
newtype HlsSegmentationMode
  = HlsSegmentationMode String
```

Placeholder documentation for HlsSegmentationMode

#### `HlsSettings`

``` purescript
newtype HlsSettings
  = HlsSettings { "AudioOnlyHlsSettings" :: NullOrUndefined (AudioOnlyHlsSettings), "StandardHlsSettings" :: NullOrUndefined (StandardHlsSettings) }
```

Placeholder documentation for HlsSettings

#### `HlsStreamInfResolution`

``` purescript
newtype HlsStreamInfResolution
  = HlsStreamInfResolution String
```

Placeholder documentation for HlsStreamInfResolution

#### `HlsTimedMetadataId3Frame`

``` purescript
newtype HlsTimedMetadataId3Frame
  = HlsTimedMetadataId3Frame String
```

Placeholder documentation for HlsTimedMetadataId3Frame

#### `HlsTsFileMode`

``` purescript
newtype HlsTsFileMode
  = HlsTsFileMode String
```

Placeholder documentation for HlsTsFileMode

#### `HlsWebdavHttpTransferMode`

``` purescript
newtype HlsWebdavHttpTransferMode
  = HlsWebdavHttpTransferMode String
```

Placeholder documentation for HlsWebdavHttpTransferMode

#### `HlsWebdavSettings`

``` purescript
newtype HlsWebdavSettings
  = HlsWebdavSettings { "ConnectionRetryInterval" :: NullOrUndefined (Int), "FilecacheDuration" :: NullOrUndefined (Int), "HttpTransferMode" :: NullOrUndefined (HlsWebdavHttpTransferMode), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int) }
```

Placeholder documentation for HlsWebdavSettings

#### `Input`

``` purescript
newtype Input
  = Input { "Arn" :: NullOrUndefined (String), "AttachedChannels" :: NullOrUndefined (ListOf__string), "Destinations" :: NullOrUndefined (ListOfInputDestination), "Id" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SecurityGroups" :: NullOrUndefined (ListOf__string), "Sources" :: NullOrUndefined (ListOfInputSource), "State" :: NullOrUndefined (InputState), "Type" :: NullOrUndefined (InputType) }
```

Placeholder documentation for Input

#### `InputAttachment`

``` purescript
newtype InputAttachment
  = InputAttachment { "InputId" :: NullOrUndefined (String), "InputSettings" :: NullOrUndefined (InputSettings) }
```

Placeholder documentation for InputAttachment

#### `InputChannelLevel`

``` purescript
newtype InputChannelLevel
  = InputChannelLevel { "Gain" :: NullOrUndefined (Int), "InputChannel" :: NullOrUndefined (Int) }
```

Placeholder documentation for InputChannelLevel

#### `InputCodec`

``` purescript
newtype InputCodec
  = InputCodec String
```

codec in increasing order of complexity

#### `InputDeblockFilter`

``` purescript
newtype InputDeblockFilter
  = InputDeblockFilter String
```

Placeholder documentation for InputDeblockFilter

#### `InputDenoiseFilter`

``` purescript
newtype InputDenoiseFilter
  = InputDenoiseFilter String
```

Placeholder documentation for InputDenoiseFilter

#### `InputDestination`

``` purescript
newtype InputDestination
  = InputDestination { "Ip" :: NullOrUndefined (String), "Port" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

The settings for a PUSH type input.

#### `InputDestinationRequest`

``` purescript
newtype InputDestinationRequest
  = InputDestinationRequest { "StreamName" :: NullOrUndefined (String) }
```

Endpoint settings for a PUSH type input.

#### `InputFilter`

``` purescript
newtype InputFilter
  = InputFilter String
```

Placeholder documentation for InputFilter

#### `InputLocation`

``` purescript
newtype InputLocation
  = InputLocation { "PasswordParam" :: NullOrUndefined (String), "Uri" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

Placeholder documentation for InputLocation

#### `InputLossActionForHlsOut`

``` purescript
newtype InputLossActionForHlsOut
  = InputLossActionForHlsOut String
```

Placeholder documentation for InputLossActionForHlsOut

#### `InputLossActionForMsSmoothOut`

``` purescript
newtype InputLossActionForMsSmoothOut
  = InputLossActionForMsSmoothOut String
```

Placeholder documentation for InputLossActionForMsSmoothOut

#### `InputLossActionForUdpOut`

``` purescript
newtype InputLossActionForUdpOut
  = InputLossActionForUdpOut String
```

Placeholder documentation for InputLossActionForUdpOut

#### `InputLossBehavior`

``` purescript
newtype InputLossBehavior
  = InputLossBehavior { "BlackFrameMsec" :: NullOrUndefined (Int), "InputLossImageColor" :: NullOrUndefined (String), "InputLossImageSlate" :: NullOrUndefined (InputLocation), "InputLossImageType" :: NullOrUndefined (InputLossImageType), "RepeatFrameMsec" :: NullOrUndefined (Int) }
```

Placeholder documentation for InputLossBehavior

#### `InputLossImageType`

``` purescript
newtype InputLossImageType
  = InputLossImageType String
```

Placeholder documentation for InputLossImageType

#### `InputMaximumBitrate`

``` purescript
newtype InputMaximumBitrate
  = InputMaximumBitrate String
```

Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.

#### `InputResolution`

``` purescript
newtype InputResolution
  = InputResolution String
```

Input resolution based on lines of vertical resolution in the input; SD is less than 720 lines, HD is 720 to 1080 lines, UHD is greater than 1080 lines


#### `InputSecurityGroup`

``` purescript
newtype InputSecurityGroup
  = InputSecurityGroup { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRule) }
```

An Input Security Group

#### `InputSecurityGroupWhitelistRequest`

``` purescript
newtype InputSecurityGroupWhitelistRequest
  = InputSecurityGroupWhitelistRequest { "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRuleCidr) }
```

Request of IPv4 CIDR addresses to whitelist in a security group.

#### `InputSettings`

``` purescript
newtype InputSettings
  = InputSettings { "AudioSelectors" :: NullOrUndefined (ListOfAudioSelector), "CaptionSelectors" :: NullOrUndefined (ListOfCaptionSelector), "DeblockFilter" :: NullOrUndefined (InputDeblockFilter), "DenoiseFilter" :: NullOrUndefined (InputDenoiseFilter), "FilterStrength" :: NullOrUndefined (Int), "InputFilter" :: NullOrUndefined (InputFilter), "NetworkInputSettings" :: NullOrUndefined (NetworkInputSettings), "SourceEndBehavior" :: NullOrUndefined (InputSourceEndBehavior), "VideoSelector" :: NullOrUndefined (VideoSelector) }
```

Live Event input parameters. There can be multiple inputs in a single Live Event.

#### `InputSource`

``` purescript
newtype InputSource
  = InputSource { "PasswordParam" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

The settings for a PULL type input.

#### `InputSourceEndBehavior`

``` purescript
newtype InputSourceEndBehavior
  = InputSourceEndBehavior String
```

Placeholder documentation for InputSourceEndBehavior

#### `InputSourceRequest`

``` purescript
newtype InputSourceRequest
  = InputSourceRequest { "PasswordParam" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

Settings for for a PULL type input.

#### `InputSpecification`

``` purescript
newtype InputSpecification
  = InputSpecification { "Codec" :: NullOrUndefined (InputCodec), "MaximumBitrate" :: NullOrUndefined (InputMaximumBitrate), "Resolution" :: NullOrUndefined (InputResolution) }
```

Placeholder documentation for InputSpecification

#### `InputState`

``` purescript
newtype InputState
  = InputState String
```

Placeholder documentation for InputState

#### `InputType`

``` purescript
newtype InputType
  = InputType String
```

Placeholder documentation for InputType

#### `InputWhitelistRule`

``` purescript
newtype InputWhitelistRule
  = InputWhitelistRule { "Cidr" :: NullOrUndefined (String) }
```

Whitelist rule

#### `InputWhitelistRuleCidr`

``` purescript
newtype InputWhitelistRuleCidr
  = InputWhitelistRuleCidr { "Cidr" :: NullOrUndefined (String) }
```

An IPv4 CIDR to whitelist.

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for InternalServerErrorException

#### `InternalServiceError`

``` purescript
newtype InternalServiceError
  = InternalServiceError { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for InternalServiceError

#### `InvalidRequest`

``` purescript
newtype InvalidRequest
  = InvalidRequest { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for InvalidRequest

#### `KeyProviderSettings`

``` purescript
newtype KeyProviderSettings
  = KeyProviderSettings { "StaticKeySettings" :: NullOrUndefined (StaticKeySettings) }
```

Placeholder documentation for KeyProviderSettings

#### `LimitExceeded`

``` purescript
newtype LimitExceeded
  = LimitExceeded { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for LimitExceeded

#### `ListChannelsRequest`

``` purescript
newtype ListChannelsRequest
  = ListChannelsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListChannelsRequest

#### `ListChannelsResponse`

``` purescript
newtype ListChannelsResponse
  = ListChannelsResponse { "Channels" :: NullOrUndefined (ListOfChannelSummary), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListChannelsResponse

#### `ListChannelsResultModel`

``` purescript
newtype ListChannelsResultModel
  = ListChannelsResultModel { "Channels" :: NullOrUndefined (ListOfChannelSummary), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListChannelsResultModel

#### `ListInputSecurityGroupsRequest`

``` purescript
newtype ListInputSecurityGroupsRequest
  = ListInputSecurityGroupsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputSecurityGroupsRequest

#### `ListInputSecurityGroupsResponse`

``` purescript
newtype ListInputSecurityGroupsResponse
  = ListInputSecurityGroupsResponse { "InputSecurityGroups" :: NullOrUndefined (ListOfInputSecurityGroup), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputSecurityGroupsResponse

#### `ListInputSecurityGroupsResultModel`

``` purescript
newtype ListInputSecurityGroupsResultModel
  = ListInputSecurityGroupsResultModel { "InputSecurityGroups" :: NullOrUndefined (ListOfInputSecurityGroup), "NextToken" :: NullOrUndefined (String) }
```

Result of input security group list request

#### `ListInputsRequest`

``` purescript
newtype ListInputsRequest
  = ListInputsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputsRequest

#### `ListInputsResponse`

``` purescript
newtype ListInputsResponse
  = ListInputsResponse { "Inputs" :: NullOrUndefined (ListOfInput), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputsResponse

#### `ListInputsResultModel`

``` purescript
newtype ListInputsResultModel
  = ListInputsResultModel { "Inputs" :: NullOrUndefined (ListOfInput), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputsResultModel

#### `ListOfAudioChannelMapping`

``` purescript
newtype ListOfAudioChannelMapping
  = ListOfAudioChannelMapping (Array AudioChannelMapping)
```

Placeholder documentation for ListOfAudioChannelMapping

#### `ListOfAudioDescription`

``` purescript
newtype ListOfAudioDescription
  = ListOfAudioDescription (Array AudioDescription)
```

Placeholder documentation for ListOfAudioDescription

#### `ListOfAudioSelector`

``` purescript
newtype ListOfAudioSelector
  = ListOfAudioSelector (Array AudioSelector)
```

Placeholder documentation for ListOfAudioSelector

#### `ListOfCaptionDescription`

``` purescript
newtype ListOfCaptionDescription
  = ListOfCaptionDescription (Array CaptionDescription)
```

Placeholder documentation for ListOfCaptionDescription

#### `ListOfCaptionLanguageMapping`

``` purescript
newtype ListOfCaptionLanguageMapping
  = ListOfCaptionLanguageMapping (Array CaptionLanguageMapping)
```

Placeholder documentation for ListOfCaptionLanguageMapping

#### `ListOfCaptionSelector`

``` purescript
newtype ListOfCaptionSelector
  = ListOfCaptionSelector (Array CaptionSelector)
```

Placeholder documentation for ListOfCaptionSelector

#### `ListOfChannelEgressEndpoint`

``` purescript
newtype ListOfChannelEgressEndpoint
  = ListOfChannelEgressEndpoint (Array ChannelEgressEndpoint)
```

Placeholder documentation for ListOfChannelEgressEndpoint

#### `ListOfChannelSummary`

``` purescript
newtype ListOfChannelSummary
  = ListOfChannelSummary (Array ChannelSummary)
```

Placeholder documentation for ListOfChannelSummary

#### `ListOfHlsAdMarkers`

``` purescript
newtype ListOfHlsAdMarkers
  = ListOfHlsAdMarkers (Array HlsAdMarkers)
```

Placeholder documentation for ListOfHlsAdMarkers

#### `ListOfInput`

``` purescript
newtype ListOfInput
  = ListOfInput (Array Input)
```

Placeholder documentation for ListOfInput

#### `ListOfInputAttachment`

``` purescript
newtype ListOfInputAttachment
  = ListOfInputAttachment (Array InputAttachment)
```

Placeholder documentation for ListOfInputAttachment

#### `ListOfInputChannelLevel`

``` purescript
newtype ListOfInputChannelLevel
  = ListOfInputChannelLevel (Array InputChannelLevel)
```

Placeholder documentation for ListOfInputChannelLevel

#### `ListOfInputDestination`

``` purescript
newtype ListOfInputDestination
  = ListOfInputDestination (Array InputDestination)
```

Placeholder documentation for ListOfInputDestination

#### `ListOfInputDestinationRequest`

``` purescript
newtype ListOfInputDestinationRequest
  = ListOfInputDestinationRequest (Array InputDestinationRequest)
```

Placeholder documentation for ListOfInputDestinationRequest

#### `ListOfInputSecurityGroup`

``` purescript
newtype ListOfInputSecurityGroup
  = ListOfInputSecurityGroup (Array InputSecurityGroup)
```

Placeholder documentation for ListOfInputSecurityGroup

#### `ListOfInputSource`

``` purescript
newtype ListOfInputSource
  = ListOfInputSource (Array InputSource)
```

Placeholder documentation for ListOfInputSource

#### `ListOfInputSourceRequest`

``` purescript
newtype ListOfInputSourceRequest
  = ListOfInputSourceRequest (Array InputSourceRequest)
```

Placeholder documentation for ListOfInputSourceRequest

#### `ListOfInputWhitelistRule`

``` purescript
newtype ListOfInputWhitelistRule
  = ListOfInputWhitelistRule (Array InputWhitelistRule)
```

Placeholder documentation for ListOfInputWhitelistRule

#### `ListOfInputWhitelistRuleCidr`

``` purescript
newtype ListOfInputWhitelistRuleCidr
  = ListOfInputWhitelistRuleCidr (Array InputWhitelistRuleCidr)
```

Placeholder documentation for ListOfInputWhitelistRuleCidr

#### `ListOfOutput`

``` purescript
newtype ListOfOutput
  = ListOfOutput (Array Output)
```

Placeholder documentation for ListOfOutput

#### `ListOfOutputDestination`

``` purescript
newtype ListOfOutputDestination
  = ListOfOutputDestination (Array OutputDestination)
```

Placeholder documentation for ListOfOutputDestination

#### `ListOfOutputDestinationSettings`

``` purescript
newtype ListOfOutputDestinationSettings
  = ListOfOutputDestinationSettings (Array OutputDestinationSettings)
```

Placeholder documentation for ListOfOutputDestinationSettings

#### `ListOfOutputGroup`

``` purescript
newtype ListOfOutputGroup
  = ListOfOutputGroup (Array OutputGroup)
```

Placeholder documentation for ListOfOutputGroup

#### `ListOfValidationError`

``` purescript
newtype ListOfValidationError
  = ListOfValidationError (Array ValidationError)
```

Placeholder documentation for ListOfValidationError

#### `ListOfVideoDescription`

``` purescript
newtype ListOfVideoDescription
  = ListOfVideoDescription (Array VideoDescription)
```

Placeholder documentation for ListOfVideoDescription

#### `ListOf__string`

``` purescript
newtype ListOf__string
  = ListOf__string (Array String)
```

Placeholder documentation for ListOf__string

#### `M2tsAbsentInputAudioBehavior`

``` purescript
newtype M2tsAbsentInputAudioBehavior
  = M2tsAbsentInputAudioBehavior String
```

Placeholder documentation for M2tsAbsentInputAudioBehavior

#### `M2tsArib`

``` purescript
newtype M2tsArib
  = M2tsArib String
```

Placeholder documentation for M2tsArib

#### `M2tsAribCaptionsPidControl`

``` purescript
newtype M2tsAribCaptionsPidControl
  = M2tsAribCaptionsPidControl String
```

Placeholder documentation for M2tsAribCaptionsPidControl

#### `M2tsAudioBufferModel`

``` purescript
newtype M2tsAudioBufferModel
  = M2tsAudioBufferModel String
```

Placeholder documentation for M2tsAudioBufferModel

#### `M2tsAudioInterval`

``` purescript
newtype M2tsAudioInterval
  = M2tsAudioInterval String
```

Placeholder documentation for M2tsAudioInterval

#### `M2tsAudioStreamType`

``` purescript
newtype M2tsAudioStreamType
  = M2tsAudioStreamType String
```

Placeholder documentation for M2tsAudioStreamType

#### `M2tsBufferModel`

``` purescript
newtype M2tsBufferModel
  = M2tsBufferModel String
```

Placeholder documentation for M2tsBufferModel

#### `M2tsCcDescriptor`

``` purescript
newtype M2tsCcDescriptor
  = M2tsCcDescriptor String
```

Placeholder documentation for M2tsCcDescriptor

#### `M2tsEbifControl`

``` purescript
newtype M2tsEbifControl
  = M2tsEbifControl String
```

Placeholder documentation for M2tsEbifControl

#### `M2tsEbpPlacement`

``` purescript
newtype M2tsEbpPlacement
  = M2tsEbpPlacement String
```

Placeholder documentation for M2tsEbpPlacement

#### `M2tsEsRateInPes`

``` purescript
newtype M2tsEsRateInPes
  = M2tsEsRateInPes String
```

Placeholder documentation for M2tsEsRateInPes

#### `M2tsKlv`

``` purescript
newtype M2tsKlv
  = M2tsKlv String
```

Placeholder documentation for M2tsKlv

#### `M2tsPcrControl`

``` purescript
newtype M2tsPcrControl
  = M2tsPcrControl String
```

Placeholder documentation for M2tsPcrControl

#### `M2tsRateMode`

``` purescript
newtype M2tsRateMode
  = M2tsRateMode String
```

Placeholder documentation for M2tsRateMode

#### `M2tsScte35Control`

``` purescript
newtype M2tsScte35Control
  = M2tsScte35Control String
```

Placeholder documentation for M2tsScte35Control

#### `M2tsSegmentationMarkers`

``` purescript
newtype M2tsSegmentationMarkers
  = M2tsSegmentationMarkers String
```

Placeholder documentation for M2tsSegmentationMarkers

#### `M2tsSegmentationStyle`

``` purescript
newtype M2tsSegmentationStyle
  = M2tsSegmentationStyle String
```

Placeholder documentation for M2tsSegmentationStyle

#### `M2tsSettings`

``` purescript
newtype M2tsSettings
  = M2tsSettings { "AbsentInputAudioBehavior" :: NullOrUndefined (M2tsAbsentInputAudioBehavior), "Arib" :: NullOrUndefined (M2tsArib), "AribCaptionsPid" :: NullOrUndefined (String), "AribCaptionsPidControl" :: NullOrUndefined (M2tsAribCaptionsPidControl), "AudioBufferModel" :: NullOrUndefined (M2tsAudioBufferModel), "AudioFramesPerPes" :: NullOrUndefined (Int), "AudioPids" :: NullOrUndefined (String), "AudioStreamType" :: NullOrUndefined (M2tsAudioStreamType), "Bitrate" :: NullOrUndefined (Int), "BufferModel" :: NullOrUndefined (M2tsBufferModel), "CcDescriptor" :: NullOrUndefined (M2tsCcDescriptor), "DvbNitSettings" :: NullOrUndefined (DvbNitSettings), "DvbSdtSettings" :: NullOrUndefined (DvbSdtSettings), "DvbSubPids" :: NullOrUndefined (String), "DvbTdtSettings" :: NullOrUndefined (DvbTdtSettings), "DvbTeletextPid" :: NullOrUndefined (String), "Ebif" :: NullOrUndefined (M2tsEbifControl), "EbpAudioInterval" :: NullOrUndefined (M2tsAudioInterval), "EbpLookaheadMs" :: NullOrUndefined (Int), "EbpPlacement" :: NullOrUndefined (M2tsEbpPlacement), "EcmPid" :: NullOrUndefined (String), "EsRateInPes" :: NullOrUndefined (M2tsEsRateInPes), "EtvPlatformPid" :: NullOrUndefined (String), "EtvSignalPid" :: NullOrUndefined (String), "FragmentTime" :: NullOrUndefined (Number), "Klv" :: NullOrUndefined (M2tsKlv), "KlvDataPids" :: NullOrUndefined (String), "NullPacketBitrate" :: NullOrUndefined (Number), "PatInterval" :: NullOrUndefined (Int), "PcrControl" :: NullOrUndefined (M2tsPcrControl), "PcrPeriod" :: NullOrUndefined (Int), "PcrPid" :: NullOrUndefined (String), "PmtInterval" :: NullOrUndefined (Int), "PmtPid" :: NullOrUndefined (String), "ProgramNum" :: NullOrUndefined (Int), "RateMode" :: NullOrUndefined (M2tsRateMode), "Scte27Pids" :: NullOrUndefined (String), "Scte35Control" :: NullOrUndefined (M2tsScte35Control), "Scte35Pid" :: NullOrUndefined (String), "SegmentationMarkers" :: NullOrUndefined (M2tsSegmentationMarkers), "SegmentationStyle" :: NullOrUndefined (M2tsSegmentationStyle), "SegmentationTime" :: NullOrUndefined (Number), "TimedMetadataBehavior" :: NullOrUndefined (M2tsTimedMetadataBehavior), "TimedMetadataPid" :: NullOrUndefined (String), "TransportStreamId" :: NullOrUndefined (Int), "VideoPid" :: NullOrUndefined (String) }
```

Placeholder documentation for M2tsSettings

#### `M2tsTimedMetadataBehavior`

``` purescript
newtype M2tsTimedMetadataBehavior
  = M2tsTimedMetadataBehavior String
```

Placeholder documentation for M2tsTimedMetadataBehavior

#### `M3u8PcrControl`

``` purescript
newtype M3u8PcrControl
  = M3u8PcrControl String
```

Placeholder documentation for M3u8PcrControl

#### `M3u8Scte35Behavior`

``` purescript
newtype M3u8Scte35Behavior
  = M3u8Scte35Behavior String
```

Placeholder documentation for M3u8Scte35Behavior

#### `M3u8Settings`

``` purescript
newtype M3u8Settings
  = M3u8Settings { "AudioFramesPerPes" :: NullOrUndefined (Int), "AudioPids" :: NullOrUndefined (String), "EcmPid" :: NullOrUndefined (String), "PatInterval" :: NullOrUndefined (Int), "PcrControl" :: NullOrUndefined (M3u8PcrControl), "PcrPeriod" :: NullOrUndefined (Int), "PcrPid" :: NullOrUndefined (String), "PmtInterval" :: NullOrUndefined (Int), "PmtPid" :: NullOrUndefined (String), "ProgramNum" :: NullOrUndefined (Int), "Scte35Behavior" :: NullOrUndefined (M3u8Scte35Behavior), "Scte35Pid" :: NullOrUndefined (String), "TimedMetadataBehavior" :: NullOrUndefined (M3u8TimedMetadataBehavior), "TransportStreamId" :: NullOrUndefined (Int), "VideoPid" :: NullOrUndefined (String) }
```

Settings information for the .m3u8 container

#### `M3u8TimedMetadataBehavior`

``` purescript
newtype M3u8TimedMetadataBehavior
  = M3u8TimedMetadataBehavior String
```

Placeholder documentation for M3u8TimedMetadataBehavior

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

Placeholder documentation for MaxResults

#### `Mp2CodingMode`

``` purescript
newtype Mp2CodingMode
  = Mp2CodingMode String
```

Placeholder documentation for Mp2CodingMode

#### `Mp2Settings`

``` purescript
newtype Mp2Settings
  = Mp2Settings { "Bitrate" :: NullOrUndefined (Number), "CodingMode" :: NullOrUndefined (Mp2CodingMode), "SampleRate" :: NullOrUndefined (Number) }
```

Placeholder documentation for Mp2Settings

#### `MsSmoothGroupSettings`

``` purescript
newtype MsSmoothGroupSettings
  = MsSmoothGroupSettings { "AcquisitionPointId" :: NullOrUndefined (String), "AudioOnlyTimecodeControl" :: NullOrUndefined (SmoothGroupAudioOnlyTimecodeControl), "CertificateMode" :: NullOrUndefined (SmoothGroupCertificateMode), "ConnectionRetryInterval" :: NullOrUndefined (Int), "Destination" :: NullOrUndefined (OutputLocationRef), "EventId" :: NullOrUndefined (String), "EventIdMode" :: NullOrUndefined (SmoothGroupEventIdMode), "EventStopBehavior" :: NullOrUndefined (SmoothGroupEventStopBehavior), "FilecacheDuration" :: NullOrUndefined (Int), "FragmentLength" :: NullOrUndefined (Int), "InputLossAction" :: NullOrUndefined (InputLossActionForMsSmoothOut), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int), "SegmentationMode" :: NullOrUndefined (SmoothGroupSegmentationMode), "SendDelayMs" :: NullOrUndefined (Int), "SparseTrackType" :: NullOrUndefined (SmoothGroupSparseTrackType), "StreamManifestBehavior" :: NullOrUndefined (SmoothGroupStreamManifestBehavior), "TimestampOffset" :: NullOrUndefined (String), "TimestampOffsetMode" :: NullOrUndefined (SmoothGroupTimestampOffsetMode) }
```

Placeholder documentation for MsSmoothGroupSettings

#### `MsSmoothOutputSettings`

``` purescript
newtype MsSmoothOutputSettings
  = MsSmoothOutputSettings { "NameModifier" :: NullOrUndefined (String) }
```

Placeholder documentation for MsSmoothOutputSettings

#### `NetworkInputServerValidation`

``` purescript
newtype NetworkInputServerValidation
  = NetworkInputServerValidation String
```

Placeholder documentation for NetworkInputServerValidation

#### `NetworkInputSettings`

``` purescript
newtype NetworkInputSettings
  = NetworkInputSettings { "HlsInputSettings" :: NullOrUndefined (HlsInputSettings), "ServerValidation" :: NullOrUndefined (NetworkInputServerValidation) }
```

Network source to transcode. Must be accessible to the Elemental Live node that is running the live event through a network connection.

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for NotFoundException

#### `Output`

``` purescript
newtype Output
  = Output { "AudioDescriptionNames" :: NullOrUndefined (ListOf__string), "CaptionDescriptionNames" :: NullOrUndefined (ListOf__string), "OutputName" :: NullOrUndefined (String), "OutputSettings" :: NullOrUndefined (OutputSettings), "VideoDescriptionName" :: NullOrUndefined (String) }
```

Output settings. There can be multiple outputs within a group.

#### `OutputDestination`

``` purescript
newtype OutputDestination
  = OutputDestination { "Id" :: NullOrUndefined (String), "Settings" :: NullOrUndefined (ListOfOutputDestinationSettings) }
```

Placeholder documentation for OutputDestination

#### `OutputDestinationSettings`

``` purescript
newtype OutputDestinationSettings
  = OutputDestinationSettings { "PasswordParam" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

Placeholder documentation for OutputDestinationSettings

#### `OutputGroup`

``` purescript
newtype OutputGroup
  = OutputGroup { "Name" :: NullOrUndefined (String), "OutputGroupSettings" :: NullOrUndefined (OutputGroupSettings), "Outputs" :: NullOrUndefined (ListOfOutput) }
```

Output groups for this Live Event. Output groups contain information about where streams should be distributed.

#### `OutputGroupSettings`

``` purescript
newtype OutputGroupSettings
  = OutputGroupSettings { "ArchiveGroupSettings" :: NullOrUndefined (ArchiveGroupSettings), "HlsGroupSettings" :: NullOrUndefined (HlsGroupSettings), "MsSmoothGroupSettings" :: NullOrUndefined (MsSmoothGroupSettings), "UdpGroupSettings" :: NullOrUndefined (UdpGroupSettings) }
```

Placeholder documentation for OutputGroupSettings

#### `OutputLocationRef`

``` purescript
newtype OutputLocationRef
  = OutputLocationRef { "DestinationRefId" :: NullOrUndefined (String) }
```

Reference to an OutputDestination ID defined in the channel

#### `OutputSettings`

``` purescript
newtype OutputSettings
  = OutputSettings { "ArchiveOutputSettings" :: NullOrUndefined (ArchiveOutputSettings), "HlsOutputSettings" :: NullOrUndefined (HlsOutputSettings), "MsSmoothOutputSettings" :: NullOrUndefined (MsSmoothOutputSettings), "UdpOutputSettings" :: NullOrUndefined (UdpOutputSettings) }
```

Placeholder documentation for OutputSettings

#### `PassThroughSettings`

``` purescript
newtype PassThroughSettings
  = PassThroughSettings {  }
```

Placeholder documentation for PassThroughSettings

#### `RemixSettings`

``` purescript
newtype RemixSettings
  = RemixSettings { "ChannelMappings" :: NullOrUndefined (ListOfAudioChannelMapping), "ChannelsIn" :: NullOrUndefined (Int), "ChannelsOut" :: NullOrUndefined (Int) }
```

Placeholder documentation for RemixSettings

#### `ResourceConflict`

``` purescript
newtype ResourceConflict
  = ResourceConflict { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for ResourceConflict

#### `ResourceNotFound`

``` purescript
newtype ResourceNotFound
  = ResourceNotFound { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for ResourceNotFound

#### `Scte20Convert608To708`

``` purescript
newtype Scte20Convert608To708
  = Scte20Convert608To708 String
```

Placeholder documentation for Scte20Convert608To708

#### `Scte20PlusEmbeddedDestinationSettings`

``` purescript
newtype Scte20PlusEmbeddedDestinationSettings
  = Scte20PlusEmbeddedDestinationSettings {  }
```

Placeholder documentation for Scte20PlusEmbeddedDestinationSettings

#### `Scte20SourceSettings`

``` purescript
newtype Scte20SourceSettings
  = Scte20SourceSettings { "Convert608To708" :: NullOrUndefined (Scte20Convert608To708), "Source608ChannelNumber" :: NullOrUndefined (Int) }
```

Placeholder documentation for Scte20SourceSettings

#### `Scte27DestinationSettings`

``` purescript
newtype Scte27DestinationSettings
  = Scte27DestinationSettings {  }
```

Placeholder documentation for Scte27DestinationSettings

#### `Scte27SourceSettings`

``` purescript
newtype Scte27SourceSettings
  = Scte27SourceSettings { "Pid" :: NullOrUndefined (Int) }
```

Placeholder documentation for Scte27SourceSettings

#### `Scte35AposNoRegionalBlackoutBehavior`

``` purescript
newtype Scte35AposNoRegionalBlackoutBehavior
  = Scte35AposNoRegionalBlackoutBehavior String
```

Placeholder documentation for Scte35AposNoRegionalBlackoutBehavior

#### `Scte35AposWebDeliveryAllowedBehavior`

``` purescript
newtype Scte35AposWebDeliveryAllowedBehavior
  = Scte35AposWebDeliveryAllowedBehavior String
```

Placeholder documentation for Scte35AposWebDeliveryAllowedBehavior

#### `Scte35SpliceInsert`

``` purescript
newtype Scte35SpliceInsert
  = Scte35SpliceInsert { "AdAvailOffset" :: NullOrUndefined (Int), "NoRegionalBlackoutFlag" :: NullOrUndefined (Scte35SpliceInsertNoRegionalBlackoutBehavior), "WebDeliveryAllowedFlag" :: NullOrUndefined (Scte35SpliceInsertWebDeliveryAllowedBehavior) }
```

Placeholder documentation for Scte35SpliceInsert

#### `Scte35SpliceInsertNoRegionalBlackoutBehavior`

``` purescript
newtype Scte35SpliceInsertNoRegionalBlackoutBehavior
  = Scte35SpliceInsertNoRegionalBlackoutBehavior String
```

Placeholder documentation for Scte35SpliceInsertNoRegionalBlackoutBehavior

#### `Scte35SpliceInsertWebDeliveryAllowedBehavior`

``` purescript
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior
  = Scte35SpliceInsertWebDeliveryAllowedBehavior String
```

Placeholder documentation for Scte35SpliceInsertWebDeliveryAllowedBehavior

#### `Scte35TimeSignalApos`

``` purescript
newtype Scte35TimeSignalApos
  = Scte35TimeSignalApos { "AdAvailOffset" :: NullOrUndefined (Int), "NoRegionalBlackoutFlag" :: NullOrUndefined (Scte35AposNoRegionalBlackoutBehavior), "WebDeliveryAllowedFlag" :: NullOrUndefined (Scte35AposWebDeliveryAllowedBehavior) }
```

Placeholder documentation for Scte35TimeSignalApos

#### `SmoothGroupAudioOnlyTimecodeControl`

``` purescript
newtype SmoothGroupAudioOnlyTimecodeControl
  = SmoothGroupAudioOnlyTimecodeControl String
```

Placeholder documentation for SmoothGroupAudioOnlyTimecodeControl

#### `SmoothGroupCertificateMode`

``` purescript
newtype SmoothGroupCertificateMode
  = SmoothGroupCertificateMode String
```

Placeholder documentation for SmoothGroupCertificateMode

#### `SmoothGroupEventIdMode`

``` purescript
newtype SmoothGroupEventIdMode
  = SmoothGroupEventIdMode String
```

Placeholder documentation for SmoothGroupEventIdMode

#### `SmoothGroupEventStopBehavior`

``` purescript
newtype SmoothGroupEventStopBehavior
  = SmoothGroupEventStopBehavior String
```

Placeholder documentation for SmoothGroupEventStopBehavior

#### `SmoothGroupSegmentationMode`

``` purescript
newtype SmoothGroupSegmentationMode
  = SmoothGroupSegmentationMode String
```

Placeholder documentation for SmoothGroupSegmentationMode

#### `SmoothGroupSparseTrackType`

``` purescript
newtype SmoothGroupSparseTrackType
  = SmoothGroupSparseTrackType String
```

Placeholder documentation for SmoothGroupSparseTrackType

#### `SmoothGroupStreamManifestBehavior`

``` purescript
newtype SmoothGroupStreamManifestBehavior
  = SmoothGroupStreamManifestBehavior String
```

Placeholder documentation for SmoothGroupStreamManifestBehavior

#### `SmoothGroupTimestampOffsetMode`

``` purescript
newtype SmoothGroupTimestampOffsetMode
  = SmoothGroupTimestampOffsetMode String
```

Placeholder documentation for SmoothGroupTimestampOffsetMode

#### `SmpteTtDestinationSettings`

``` purescript
newtype SmpteTtDestinationSettings
  = SmpteTtDestinationSettings {  }
```

Placeholder documentation for SmpteTtDestinationSettings

#### `StandardHlsSettings`

``` purescript
newtype StandardHlsSettings
  = StandardHlsSettings { "AudioRenditionSets" :: NullOrUndefined (String), "M3u8Settings" :: NullOrUndefined (M3u8Settings) }
```

Placeholder documentation for StandardHlsSettings

#### `StartChannelRequest`

``` purescript
newtype StartChannelRequest
  = StartChannelRequest { "ChannelId" :: String }
```

Placeholder documentation for StartChannelRequest

#### `StartChannelResponse`

``` purescript
newtype StartChannelResponse
  = StartChannelResponse { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for StartChannelResponse

#### `StaticKeySettings`

``` purescript
newtype StaticKeySettings
  = StaticKeySettings { "KeyProviderServer" :: NullOrUndefined (InputLocation), "StaticKeyValue" :: NullOrUndefined (String) }
```

Placeholder documentation for StaticKeySettings

#### `StopChannelRequest`

``` purescript
newtype StopChannelRequest
  = StopChannelRequest { "ChannelId" :: String }
```

Placeholder documentation for StopChannelRequest

#### `StopChannelResponse`

``` purescript
newtype StopChannelResponse
  = StopChannelResponse { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for StopChannelResponse

#### `TeletextDestinationSettings`

``` purescript
newtype TeletextDestinationSettings
  = TeletextDestinationSettings {  }
```

Placeholder documentation for TeletextDestinationSettings

#### `TeletextSourceSettings`

``` purescript
newtype TeletextSourceSettings
  = TeletextSourceSettings { "PageNumber" :: NullOrUndefined (String) }
```

Placeholder documentation for TeletextSourceSettings

#### `TimecodeConfig`

``` purescript
newtype TimecodeConfig
  = TimecodeConfig { "Source" :: NullOrUndefined (TimecodeConfigSource), "SyncThreshold" :: NullOrUndefined (Int) }
```

Placeholder documentation for TimecodeConfig

#### `TimecodeConfigSource`

``` purescript
newtype TimecodeConfigSource
  = TimecodeConfigSource String
```

Placeholder documentation for TimecodeConfigSource

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for TooManyRequestsException

#### `TtmlDestinationSettings`

``` purescript
newtype TtmlDestinationSettings
  = TtmlDestinationSettings { "StyleControl" :: NullOrUndefined (TtmlDestinationStyleControl) }
```

Placeholder documentation for TtmlDestinationSettings

#### `TtmlDestinationStyleControl`

``` purescript
newtype TtmlDestinationStyleControl
  = TtmlDestinationStyleControl String
```

Placeholder documentation for TtmlDestinationStyleControl

#### `UdpContainerSettings`

``` purescript
newtype UdpContainerSettings
  = UdpContainerSettings { "M2tsSettings" :: NullOrUndefined (M2tsSettings) }
```

Placeholder documentation for UdpContainerSettings

#### `UdpGroupSettings`

``` purescript
newtype UdpGroupSettings
  = UdpGroupSettings { "InputLossAction" :: NullOrUndefined (InputLossActionForUdpOut), "TimedMetadataId3Frame" :: NullOrUndefined (UdpTimedMetadataId3Frame), "TimedMetadataId3Period" :: NullOrUndefined (Int) }
```

Placeholder documentation for UdpGroupSettings

#### `UdpOutputSettings`

``` purescript
newtype UdpOutputSettings
  = UdpOutputSettings { "BufferMsec" :: NullOrUndefined (Int), "ContainerSettings" :: NullOrUndefined (UdpContainerSettings), "Destination" :: NullOrUndefined (OutputLocationRef), "FecOutputSettings" :: NullOrUndefined (FecOutputSettings) }
```

Placeholder documentation for UdpOutputSettings

#### `UdpTimedMetadataId3Frame`

``` purescript
newtype UdpTimedMetadataId3Frame
  = UdpTimedMetadataId3Frame String
```

Placeholder documentation for UdpTimedMetadataId3Frame

#### `UnprocessableEntityException`

``` purescript
newtype UnprocessableEntityException
  = UnprocessableEntityException { "Message" :: NullOrUndefined (String), "ValidationErrors" :: NullOrUndefined (ListOfValidationError) }
```

Placeholder documentation for UnprocessableEntityException

#### `UpdateChannel`

``` purescript
newtype UpdateChannel
  = UpdateChannel { "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Placeholder documentation for UpdateChannel

#### `UpdateChannelRequest`

``` purescript
newtype UpdateChannelRequest
  = UpdateChannelRequest { "ChannelId" :: String, "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

A request to update a channel.

#### `UpdateChannelResponse`

``` purescript
newtype UpdateChannelResponse
  = UpdateChannelResponse { "Channel" :: NullOrUndefined (Channel) }
```

Placeholder documentation for UpdateChannelResponse

#### `UpdateChannelResultModel`

``` purescript
newtype UpdateChannelResultModel
  = UpdateChannelResultModel { "Channel" :: NullOrUndefined (Channel) }
```

The updated channel's description.

#### `ValidationError`

``` purescript
newtype ValidationError
  = ValidationError { "ElementPath" :: NullOrUndefined (String), "ErrorMessage" :: NullOrUndefined (String) }
```

Placeholder documentation for ValidationError

#### `VideoCodecSettings`

``` purescript
newtype VideoCodecSettings
  = VideoCodecSettings { "H264Settings" :: NullOrUndefined (H264Settings) }
```

Placeholder documentation for VideoCodecSettings

#### `VideoDescription`

``` purescript
newtype VideoDescription
  = VideoDescription { "CodecSettings" :: NullOrUndefined (VideoCodecSettings), "Height" :: NullOrUndefined (Int), "Name" :: NullOrUndefined (String), "RespondToAfd" :: NullOrUndefined (VideoDescriptionRespondToAfd), "ScalingBehavior" :: NullOrUndefined (VideoDescriptionScalingBehavior), "Sharpness" :: NullOrUndefined (Int), "Width" :: NullOrUndefined (Int) }
```

Video settings for this stream.

#### `VideoDescriptionRespondToAfd`

``` purescript
newtype VideoDescriptionRespondToAfd
  = VideoDescriptionRespondToAfd String
```

Placeholder documentation for VideoDescriptionRespondToAfd

#### `VideoDescriptionScalingBehavior`

``` purescript
newtype VideoDescriptionScalingBehavior
  = VideoDescriptionScalingBehavior String
```

Placeholder documentation for VideoDescriptionScalingBehavior

#### `VideoSelector`

``` purescript
newtype VideoSelector
  = VideoSelector { "ColorSpace" :: NullOrUndefined (VideoSelectorColorSpace), "ColorSpaceUsage" :: NullOrUndefined (VideoSelectorColorSpaceUsage), "SelectorSettings" :: NullOrUndefined (VideoSelectorSettings) }
```

Specifies a particular video stream within an input source. An input may have only a single video selector.

#### `VideoSelectorColorSpace`

``` purescript
newtype VideoSelectorColorSpace
  = VideoSelectorColorSpace String
```

Placeholder documentation for VideoSelectorColorSpace

#### `VideoSelectorColorSpaceUsage`

``` purescript
newtype VideoSelectorColorSpaceUsage
  = VideoSelectorColorSpaceUsage String
```

Placeholder documentation for VideoSelectorColorSpaceUsage

#### `VideoSelectorPid`

``` purescript
newtype VideoSelectorPid
  = VideoSelectorPid { "Pid" :: NullOrUndefined (Int) }
```

Placeholder documentation for VideoSelectorPid

#### `VideoSelectorProgramId`

``` purescript
newtype VideoSelectorProgramId
  = VideoSelectorProgramId { "ProgramId" :: NullOrUndefined (Int) }
```

Placeholder documentation for VideoSelectorProgramId

#### `VideoSelectorSettings`

``` purescript
newtype VideoSelectorSettings
  = VideoSelectorSettings { "VideoSelectorPid" :: NullOrUndefined (VideoSelectorPid), "VideoSelectorProgramId" :: NullOrUndefined (VideoSelectorProgramId) }
```

Placeholder documentation for VideoSelectorSettings

#### `WebvttDestinationSettings`

``` purescript
newtype WebvttDestinationSettings
  = WebvttDestinationSettings {  }
```

Placeholder documentation for WebvttDestinationSettings


