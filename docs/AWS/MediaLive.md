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

##### Instances
``` purescript
Newtype AacCodingMode _
```

#### `AacInputType`

``` purescript
newtype AacInputType
  = AacInputType String
```

Placeholder documentation for AacInputType

##### Instances
``` purescript
Newtype AacInputType _
```

#### `AacProfile`

``` purescript
newtype AacProfile
  = AacProfile String
```

Placeholder documentation for AacProfile

##### Instances
``` purescript
Newtype AacProfile _
```

#### `AacRateControlMode`

``` purescript
newtype AacRateControlMode
  = AacRateControlMode String
```

Placeholder documentation for AacRateControlMode

##### Instances
``` purescript
Newtype AacRateControlMode _
```

#### `AacRawFormat`

``` purescript
newtype AacRawFormat
  = AacRawFormat String
```

Placeholder documentation for AacRawFormat

##### Instances
``` purescript
Newtype AacRawFormat _
```

#### `AacSettings`

``` purescript
newtype AacSettings
  = AacSettings { "Bitrate" :: NullOrUndefined (Number), "CodingMode" :: NullOrUndefined (AacCodingMode), "InputType" :: NullOrUndefined (AacInputType), "Profile" :: NullOrUndefined (AacProfile), "RateControlMode" :: NullOrUndefined (AacRateControlMode), "RawFormat" :: NullOrUndefined (AacRawFormat), "SampleRate" :: NullOrUndefined (Number), "Spec" :: NullOrUndefined (AacSpec), "VbrQuality" :: NullOrUndefined (AacVbrQuality) }
```

Placeholder documentation for AacSettings

##### Instances
``` purescript
Newtype AacSettings _
```

#### `AacSpec`

``` purescript
newtype AacSpec
  = AacSpec String
```

Placeholder documentation for AacSpec

##### Instances
``` purescript
Newtype AacSpec _
```

#### `AacVbrQuality`

``` purescript
newtype AacVbrQuality
  = AacVbrQuality String
```

Placeholder documentation for AacVbrQuality

##### Instances
``` purescript
Newtype AacVbrQuality _
```

#### `Ac3BitstreamMode`

``` purescript
newtype Ac3BitstreamMode
  = Ac3BitstreamMode String
```

Placeholder documentation for Ac3BitstreamMode

##### Instances
``` purescript
Newtype Ac3BitstreamMode _
```

#### `Ac3CodingMode`

``` purescript
newtype Ac3CodingMode
  = Ac3CodingMode String
```

Placeholder documentation for Ac3CodingMode

##### Instances
``` purescript
Newtype Ac3CodingMode _
```

#### `Ac3DrcProfile`

``` purescript
newtype Ac3DrcProfile
  = Ac3DrcProfile String
```

Placeholder documentation for Ac3DrcProfile

##### Instances
``` purescript
Newtype Ac3DrcProfile _
```

#### `Ac3LfeFilter`

``` purescript
newtype Ac3LfeFilter
  = Ac3LfeFilter String
```

Placeholder documentation for Ac3LfeFilter

##### Instances
``` purescript
Newtype Ac3LfeFilter _
```

#### `Ac3MetadataControl`

``` purescript
newtype Ac3MetadataControl
  = Ac3MetadataControl String
```

Placeholder documentation for Ac3MetadataControl

##### Instances
``` purescript
Newtype Ac3MetadataControl _
```

#### `Ac3Settings`

``` purescript
newtype Ac3Settings
  = Ac3Settings { "Bitrate" :: NullOrUndefined (Number), "BitstreamMode" :: NullOrUndefined (Ac3BitstreamMode), "CodingMode" :: NullOrUndefined (Ac3CodingMode), "Dialnorm" :: NullOrUndefined (Int), "DrcProfile" :: NullOrUndefined (Ac3DrcProfile), "LfeFilter" :: NullOrUndefined (Ac3LfeFilter), "MetadataControl" :: NullOrUndefined (Ac3MetadataControl) }
```

Placeholder documentation for Ac3Settings

##### Instances
``` purescript
Newtype Ac3Settings _
```

#### `AccessDenied`

``` purescript
newtype AccessDenied
  = AccessDenied { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for AccessDenied

##### Instances
``` purescript
Newtype AccessDenied _
```

#### `AfdSignaling`

``` purescript
newtype AfdSignaling
  = AfdSignaling String
```

Placeholder documentation for AfdSignaling

##### Instances
``` purescript
Newtype AfdSignaling _
```

#### `ArchiveContainerSettings`

``` purescript
newtype ArchiveContainerSettings
  = ArchiveContainerSettings { "M2tsSettings" :: NullOrUndefined (M2tsSettings) }
```

Placeholder documentation for ArchiveContainerSettings

##### Instances
``` purescript
Newtype ArchiveContainerSettings _
```

#### `ArchiveGroupSettings`

``` purescript
newtype ArchiveGroupSettings
  = ArchiveGroupSettings { "Destination" :: NullOrUndefined (OutputLocationRef), "RolloverInterval" :: NullOrUndefined (Int) }
```

Placeholder documentation for ArchiveGroupSettings

##### Instances
``` purescript
Newtype ArchiveGroupSettings _
```

#### `ArchiveOutputSettings`

``` purescript
newtype ArchiveOutputSettings
  = ArchiveOutputSettings { "ContainerSettings" :: NullOrUndefined (ArchiveContainerSettings), "Extension" :: NullOrUndefined (String), "NameModifier" :: NullOrUndefined (String) }
```

Placeholder documentation for ArchiveOutputSettings

##### Instances
``` purescript
Newtype ArchiveOutputSettings _
```

#### `AribDestinationSettings`

``` purescript
newtype AribDestinationSettings
  = AribDestinationSettings {  }
```

Placeholder documentation for AribDestinationSettings

##### Instances
``` purescript
Newtype AribDestinationSettings _
```

#### `AribSourceSettings`

``` purescript
newtype AribSourceSettings
  = AribSourceSettings {  }
```

Placeholder documentation for AribSourceSettings

##### Instances
``` purescript
Newtype AribSourceSettings _
```

#### `AudioChannelMapping`

``` purescript
newtype AudioChannelMapping
  = AudioChannelMapping { "InputChannelLevels" :: NullOrUndefined (ListOfInputChannelLevel), "OutputChannel" :: NullOrUndefined (Int) }
```

Placeholder documentation for AudioChannelMapping

##### Instances
``` purescript
Newtype AudioChannelMapping _
```

#### `AudioCodecSettings`

``` purescript
newtype AudioCodecSettings
  = AudioCodecSettings { "AacSettings" :: NullOrUndefined (AacSettings), "Ac3Settings" :: NullOrUndefined (Ac3Settings), "Eac3Settings" :: NullOrUndefined (Eac3Settings), "Mp2Settings" :: NullOrUndefined (Mp2Settings), "PassThroughSettings" :: NullOrUndefined (PassThroughSettings) }
```

Placeholder documentation for AudioCodecSettings

##### Instances
``` purescript
Newtype AudioCodecSettings _
```

#### `AudioDescription`

``` purescript
newtype AudioDescription
  = AudioDescription { "AudioNormalizationSettings" :: NullOrUndefined (AudioNormalizationSettings), "AudioSelectorName" :: NullOrUndefined (String), "AudioType" :: NullOrUndefined (AudioType), "AudioTypeControl" :: NullOrUndefined (AudioDescriptionAudioTypeControl), "CodecSettings" :: NullOrUndefined (AudioCodecSettings), "LanguageCode" :: NullOrUndefined (String), "LanguageCodeControl" :: NullOrUndefined (AudioDescriptionLanguageCodeControl), "Name" :: NullOrUndefined (String), "RemixSettings" :: NullOrUndefined (RemixSettings), "StreamName" :: NullOrUndefined (String) }
```

Placeholder documentation for AudioDescription

##### Instances
``` purescript
Newtype AudioDescription _
```

#### `AudioDescriptionAudioTypeControl`

``` purescript
newtype AudioDescriptionAudioTypeControl
  = AudioDescriptionAudioTypeControl String
```

Placeholder documentation for AudioDescriptionAudioTypeControl

##### Instances
``` purescript
Newtype AudioDescriptionAudioTypeControl _
```

#### `AudioDescriptionLanguageCodeControl`

``` purescript
newtype AudioDescriptionLanguageCodeControl
  = AudioDescriptionLanguageCodeControl String
```

Placeholder documentation for AudioDescriptionLanguageCodeControl

##### Instances
``` purescript
Newtype AudioDescriptionLanguageCodeControl _
```

#### `AudioLanguageSelection`

``` purescript
newtype AudioLanguageSelection
  = AudioLanguageSelection { "LanguageCode" :: NullOrUndefined (String), "LanguageSelectionPolicy" :: NullOrUndefined (AudioLanguageSelectionPolicy) }
```

Placeholder documentation for AudioLanguageSelection

##### Instances
``` purescript
Newtype AudioLanguageSelection _
```

#### `AudioLanguageSelectionPolicy`

``` purescript
newtype AudioLanguageSelectionPolicy
  = AudioLanguageSelectionPolicy String
```

Placeholder documentation for AudioLanguageSelectionPolicy

##### Instances
``` purescript
Newtype AudioLanguageSelectionPolicy _
```

#### `AudioNormalizationAlgorithm`

``` purescript
newtype AudioNormalizationAlgorithm
  = AudioNormalizationAlgorithm String
```

Placeholder documentation for AudioNormalizationAlgorithm

##### Instances
``` purescript
Newtype AudioNormalizationAlgorithm _
```

#### `AudioNormalizationAlgorithmControl`

``` purescript
newtype AudioNormalizationAlgorithmControl
  = AudioNormalizationAlgorithmControl String
```

Placeholder documentation for AudioNormalizationAlgorithmControl

##### Instances
``` purescript
Newtype AudioNormalizationAlgorithmControl _
```

#### `AudioNormalizationSettings`

``` purescript
newtype AudioNormalizationSettings
  = AudioNormalizationSettings { "Algorithm" :: NullOrUndefined (AudioNormalizationAlgorithm), "AlgorithmControl" :: NullOrUndefined (AudioNormalizationAlgorithmControl), "TargetLkfs" :: NullOrUndefined (Number) }
```

Placeholder documentation for AudioNormalizationSettings

##### Instances
``` purescript
Newtype AudioNormalizationSettings _
```

#### `AudioOnlyHlsSettings`

``` purescript
newtype AudioOnlyHlsSettings
  = AudioOnlyHlsSettings { "AudioGroupId" :: NullOrUndefined (String), "AudioOnlyImage" :: NullOrUndefined (InputLocation), "AudioTrackType" :: NullOrUndefined (AudioOnlyHlsTrackType) }
```

Placeholder documentation for AudioOnlyHlsSettings

##### Instances
``` purescript
Newtype AudioOnlyHlsSettings _
```

#### `AudioOnlyHlsTrackType`

``` purescript
newtype AudioOnlyHlsTrackType
  = AudioOnlyHlsTrackType String
```

Placeholder documentation for AudioOnlyHlsTrackType

##### Instances
``` purescript
Newtype AudioOnlyHlsTrackType _
```

#### `AudioPidSelection`

``` purescript
newtype AudioPidSelection
  = AudioPidSelection { "Pid" :: NullOrUndefined (Int) }
```

Placeholder documentation for AudioPidSelection

##### Instances
``` purescript
Newtype AudioPidSelection _
```

#### `AudioSelector`

``` purescript
newtype AudioSelector
  = AudioSelector { "Name" :: NullOrUndefined (String), "SelectorSettings" :: NullOrUndefined (AudioSelectorSettings) }
```

Placeholder documentation for AudioSelector

##### Instances
``` purescript
Newtype AudioSelector _
```

#### `AudioSelectorSettings`

``` purescript
newtype AudioSelectorSettings
  = AudioSelectorSettings { "AudioLanguageSelection" :: NullOrUndefined (AudioLanguageSelection), "AudioPidSelection" :: NullOrUndefined (AudioPidSelection) }
```

Placeholder documentation for AudioSelectorSettings

##### Instances
``` purescript
Newtype AudioSelectorSettings _
```

#### `AudioType`

``` purescript
newtype AudioType
  = AudioType String
```

Placeholder documentation for AudioType

##### Instances
``` purescript
Newtype AudioType _
```

#### `AvailBlanking`

``` purescript
newtype AvailBlanking
  = AvailBlanking { "AvailBlankingImage" :: NullOrUndefined (InputLocation), "State" :: NullOrUndefined (AvailBlankingState) }
```

Placeholder documentation for AvailBlanking

##### Instances
``` purescript
Newtype AvailBlanking _
```

#### `AvailBlankingState`

``` purescript
newtype AvailBlankingState
  = AvailBlankingState String
```

Placeholder documentation for AvailBlankingState

##### Instances
``` purescript
Newtype AvailBlankingState _
```

#### `AvailConfiguration`

``` purescript
newtype AvailConfiguration
  = AvailConfiguration { "AvailSettings" :: NullOrUndefined (AvailSettings) }
```

Placeholder documentation for AvailConfiguration

##### Instances
``` purescript
Newtype AvailConfiguration _
```

#### `AvailSettings`

``` purescript
newtype AvailSettings
  = AvailSettings { "Scte35SpliceInsert" :: NullOrUndefined (Scte35SpliceInsert), "Scte35TimeSignalApos" :: NullOrUndefined (Scte35TimeSignalApos) }
```

Placeholder documentation for AvailSettings

##### Instances
``` purescript
Newtype AvailSettings _
```

#### `BadGatewayException`

``` purescript
newtype BadGatewayException
  = BadGatewayException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for BadGatewayException

##### Instances
``` purescript
Newtype BadGatewayException _
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for BadRequestException

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `BlackoutSlate`

``` purescript
newtype BlackoutSlate
  = BlackoutSlate { "BlackoutSlateImage" :: NullOrUndefined (InputLocation), "NetworkEndBlackout" :: NullOrUndefined (BlackoutSlateNetworkEndBlackout), "NetworkEndBlackoutImage" :: NullOrUndefined (InputLocation), "NetworkId" :: NullOrUndefined (String), "State" :: NullOrUndefined (BlackoutSlateState) }
```

Placeholder documentation for BlackoutSlate

##### Instances
``` purescript
Newtype BlackoutSlate _
```

#### `BlackoutSlateNetworkEndBlackout`

``` purescript
newtype BlackoutSlateNetworkEndBlackout
  = BlackoutSlateNetworkEndBlackout String
```

Placeholder documentation for BlackoutSlateNetworkEndBlackout

##### Instances
``` purescript
Newtype BlackoutSlateNetworkEndBlackout _
```

#### `BlackoutSlateState`

``` purescript
newtype BlackoutSlateState
  = BlackoutSlateState String
```

Placeholder documentation for BlackoutSlateState

##### Instances
``` purescript
Newtype BlackoutSlateState _
```

#### `BurnInAlignment`

``` purescript
newtype BurnInAlignment
  = BurnInAlignment String
```

Placeholder documentation for BurnInAlignment

##### Instances
``` purescript
Newtype BurnInAlignment _
```

#### `BurnInBackgroundColor`

``` purescript
newtype BurnInBackgroundColor
  = BurnInBackgroundColor String
```

Placeholder documentation for BurnInBackgroundColor

##### Instances
``` purescript
Newtype BurnInBackgroundColor _
```

#### `BurnInDestinationSettings`

``` purescript
newtype BurnInDestinationSettings
  = BurnInDestinationSettings { "Alignment" :: NullOrUndefined (BurnInAlignment), "BackgroundColor" :: NullOrUndefined (BurnInBackgroundColor), "BackgroundOpacity" :: NullOrUndefined (Int), "Font" :: NullOrUndefined (InputLocation), "FontColor" :: NullOrUndefined (BurnInFontColor), "FontOpacity" :: NullOrUndefined (Int), "FontResolution" :: NullOrUndefined (Int), "FontSize" :: NullOrUndefined (String), "OutlineColor" :: NullOrUndefined (BurnInOutlineColor), "OutlineSize" :: NullOrUndefined (Int), "ShadowColor" :: NullOrUndefined (BurnInShadowColor), "ShadowOpacity" :: NullOrUndefined (Int), "ShadowXOffset" :: NullOrUndefined (Int), "ShadowYOffset" :: NullOrUndefined (Int), "TeletextGridControl" :: NullOrUndefined (BurnInTeletextGridControl), "XPosition" :: NullOrUndefined (Int), "YPosition" :: NullOrUndefined (Int) }
```

Placeholder documentation for BurnInDestinationSettings

##### Instances
``` purescript
Newtype BurnInDestinationSettings _
```

#### `BurnInFontColor`

``` purescript
newtype BurnInFontColor
  = BurnInFontColor String
```

Placeholder documentation for BurnInFontColor

##### Instances
``` purescript
Newtype BurnInFontColor _
```

#### `BurnInOutlineColor`

``` purescript
newtype BurnInOutlineColor
  = BurnInOutlineColor String
```

Placeholder documentation for BurnInOutlineColor

##### Instances
``` purescript
Newtype BurnInOutlineColor _
```

#### `BurnInShadowColor`

``` purescript
newtype BurnInShadowColor
  = BurnInShadowColor String
```

Placeholder documentation for BurnInShadowColor

##### Instances
``` purescript
Newtype BurnInShadowColor _
```

#### `BurnInTeletextGridControl`

``` purescript
newtype BurnInTeletextGridControl
  = BurnInTeletextGridControl String
```

Placeholder documentation for BurnInTeletextGridControl

##### Instances
``` purescript
Newtype BurnInTeletextGridControl _
```

#### `CaptionDescription`

``` purescript
newtype CaptionDescription
  = CaptionDescription { "CaptionSelectorName" :: NullOrUndefined (String), "DestinationSettings" :: NullOrUndefined (CaptionDestinationSettings), "LanguageCode" :: NullOrUndefined (String), "LanguageDescription" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Output groups for this Live Event. Output groups contain information about where streams should be distributed.

##### Instances
``` purescript
Newtype CaptionDescription _
```

#### `CaptionDestinationSettings`

``` purescript
newtype CaptionDestinationSettings
  = CaptionDestinationSettings { "AribDestinationSettings" :: NullOrUndefined (AribDestinationSettings), "BurnInDestinationSettings" :: NullOrUndefined (BurnInDestinationSettings), "DvbSubDestinationSettings" :: NullOrUndefined (DvbSubDestinationSettings), "EmbeddedDestinationSettings" :: NullOrUndefined (EmbeddedDestinationSettings), "EmbeddedPlusScte20DestinationSettings" :: NullOrUndefined (EmbeddedPlusScte20DestinationSettings), "Scte20PlusEmbeddedDestinationSettings" :: NullOrUndefined (Scte20PlusEmbeddedDestinationSettings), "Scte27DestinationSettings" :: NullOrUndefined (Scte27DestinationSettings), "SmpteTtDestinationSettings" :: NullOrUndefined (SmpteTtDestinationSettings), "TeletextDestinationSettings" :: NullOrUndefined (TeletextDestinationSettings), "TtmlDestinationSettings" :: NullOrUndefined (TtmlDestinationSettings), "WebvttDestinationSettings" :: NullOrUndefined (WebvttDestinationSettings) }
```

Placeholder documentation for CaptionDestinationSettings

##### Instances
``` purescript
Newtype CaptionDestinationSettings _
```

#### `CaptionLanguageMapping`

``` purescript
newtype CaptionLanguageMapping
  = CaptionLanguageMapping { "CaptionChannel" :: NullOrUndefined (Int), "LanguageCode" :: NullOrUndefined (String), "LanguageDescription" :: NullOrUndefined (String) }
```

Maps a caption channel to an ISO 693-2 language code (http://www.loc.gov/standards/iso639-2), with an optional description.

##### Instances
``` purescript
Newtype CaptionLanguageMapping _
```

#### `CaptionSelector`

``` purescript
newtype CaptionSelector
  = CaptionSelector { "LanguageCode" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SelectorSettings" :: NullOrUndefined (CaptionSelectorSettings) }
```

Output groups for this Live Event. Output groups contain information about where streams should be distributed.

##### Instances
``` purescript
Newtype CaptionSelector _
```

#### `CaptionSelectorSettings`

``` purescript
newtype CaptionSelectorSettings
  = CaptionSelectorSettings { "AribSourceSettings" :: NullOrUndefined (AribSourceSettings), "DvbSubSourceSettings" :: NullOrUndefined (DvbSubSourceSettings), "EmbeddedSourceSettings" :: NullOrUndefined (EmbeddedSourceSettings), "Scte20SourceSettings" :: NullOrUndefined (Scte20SourceSettings), "Scte27SourceSettings" :: NullOrUndefined (Scte27SourceSettings), "TeletextSourceSettings" :: NullOrUndefined (TeletextSourceSettings) }
```

Placeholder documentation for CaptionSelectorSettings

##### Instances
``` purescript
Newtype CaptionSelectorSettings _
```

#### `Channel`

``` purescript
newtype Channel
  = Channel { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for Channel

##### Instances
``` purescript
Newtype Channel _
```

#### `ChannelConfigurationValidationError`

``` purescript
newtype ChannelConfigurationValidationError
  = ChannelConfigurationValidationError { "Message" :: NullOrUndefined (String), "ValidationErrors" :: NullOrUndefined (ListOfValidationError) }
```

Placeholder documentation for ChannelConfigurationValidationError

##### Instances
``` purescript
Newtype ChannelConfigurationValidationError _
```

#### `ChannelEgressEndpoint`

``` purescript
newtype ChannelEgressEndpoint
  = ChannelEgressEndpoint { "SourceIp" :: NullOrUndefined (String) }
```

Placeholder documentation for ChannelEgressEndpoint

##### Instances
``` purescript
Newtype ChannelEgressEndpoint _
```

#### `ChannelState`

``` purescript
newtype ChannelState
  = ChannelState String
```

Placeholder documentation for ChannelState

##### Instances
``` purescript
Newtype ChannelState _
```

#### `ChannelSummary`

``` purescript
newtype ChannelSummary
  = ChannelSummary { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for ChannelSummary

##### Instances
``` purescript
Newtype ChannelSummary _
```

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for ConflictException

##### Instances
``` purescript
Newtype ConflictException _
```

#### `CreateChannel`

``` purescript
newtype CreateChannel
  = CreateChannel { "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Reserved" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Placeholder documentation for CreateChannel

##### Instances
``` purescript
Newtype CreateChannel _
```

#### `CreateChannelRequest`

``` purescript
newtype CreateChannelRequest
  = CreateChannelRequest { "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Reserved" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

A request to create a channel

##### Instances
``` purescript
Newtype CreateChannelRequest _
```

#### `CreateChannelResponse`

``` purescript
newtype CreateChannelResponse
  = CreateChannelResponse { "Channel" :: NullOrUndefined (Channel) }
```

Placeholder documentation for CreateChannelResponse

##### Instances
``` purescript
Newtype CreateChannelResponse _
```

#### `CreateChannelResultModel`

``` purescript
newtype CreateChannelResultModel
  = CreateChannelResultModel { "Channel" :: NullOrUndefined (Channel) }
```

Placeholder documentation for CreateChannelResultModel

##### Instances
``` purescript
Newtype CreateChannelResultModel _
```

#### `CreateInput`

``` purescript
newtype CreateInput
  = CreateInput { "Destinations" :: NullOrUndefined (ListOfInputDestinationRequest), "InputSecurityGroups" :: NullOrUndefined (ListOf__string), "Name" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Sources" :: NullOrUndefined (ListOfInputSourceRequest), "Type" :: NullOrUndefined (InputType) }
```

Placeholder documentation for CreateInput

##### Instances
``` purescript
Newtype CreateInput _
```

#### `CreateInputRequest`

``` purescript
newtype CreateInputRequest
  = CreateInputRequest { "Destinations" :: NullOrUndefined (ListOfInputDestinationRequest), "InputSecurityGroups" :: NullOrUndefined (ListOf__string), "Name" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Sources" :: NullOrUndefined (ListOfInputSourceRequest), "Type" :: NullOrUndefined (InputType) }
```

The name of the input

##### Instances
``` purescript
Newtype CreateInputRequest _
```

#### `CreateInputResponse`

``` purescript
newtype CreateInputResponse
  = CreateInputResponse { "Input" :: NullOrUndefined (Input) }
```

Placeholder documentation for CreateInputResponse

##### Instances
``` purescript
Newtype CreateInputResponse _
```

#### `CreateInputResultModel`

``` purescript
newtype CreateInputResultModel
  = CreateInputResultModel { "Input" :: NullOrUndefined (Input) }
```

Placeholder documentation for CreateInputResultModel

##### Instances
``` purescript
Newtype CreateInputResultModel _
```

#### `CreateInputSecurityGroupRequest`

``` purescript
newtype CreateInputSecurityGroupRequest
  = CreateInputSecurityGroupRequest { "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRuleCidr) }
```

The IPv4 CIDRs to whitelist for this Input Security Group

##### Instances
``` purescript
Newtype CreateInputSecurityGroupRequest _
```

#### `CreateInputSecurityGroupResponse`

``` purescript
newtype CreateInputSecurityGroupResponse
  = CreateInputSecurityGroupResponse { "SecurityGroup" :: NullOrUndefined (InputSecurityGroup) }
```

Placeholder documentation for CreateInputSecurityGroupResponse

##### Instances
``` purescript
Newtype CreateInputSecurityGroupResponse _
```

#### `CreateInputSecurityGroupResultModel`

``` purescript
newtype CreateInputSecurityGroupResultModel
  = CreateInputSecurityGroupResultModel { "SecurityGroup" :: NullOrUndefined (InputSecurityGroup) }
```

Placeholder documentation for CreateInputSecurityGroupResultModel

##### Instances
``` purescript
Newtype CreateInputSecurityGroupResultModel _
```

#### `DeleteChannelRequest`

``` purescript
newtype DeleteChannelRequest
  = DeleteChannelRequest { "ChannelId" :: String }
```

Placeholder documentation for DeleteChannelRequest

##### Instances
``` purescript
Newtype DeleteChannelRequest _
```

#### `DeleteChannelResponse`

``` purescript
newtype DeleteChannelResponse
  = DeleteChannelResponse { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for DeleteChannelResponse

##### Instances
``` purescript
Newtype DeleteChannelResponse _
```

#### `DeleteInputRequest`

``` purescript
newtype DeleteInputRequest
  = DeleteInputRequest { "InputId" :: String }
```

Placeholder documentation for DeleteInputRequest

##### Instances
``` purescript
Newtype DeleteInputRequest _
```

#### `DeleteInputResponse`

``` purescript
newtype DeleteInputResponse
  = DeleteInputResponse {  }
```

Placeholder documentation for DeleteInputResponse

##### Instances
``` purescript
Newtype DeleteInputResponse _
```

#### `DeleteInputSecurityGroupRequest`

``` purescript
newtype DeleteInputSecurityGroupRequest
  = DeleteInputSecurityGroupRequest { "InputSecurityGroupId" :: String }
```

Placeholder documentation for DeleteInputSecurityGroupRequest

##### Instances
``` purescript
Newtype DeleteInputSecurityGroupRequest _
```

#### `DeleteInputSecurityGroupResponse`

``` purescript
newtype DeleteInputSecurityGroupResponse
  = DeleteInputSecurityGroupResponse {  }
```

Placeholder documentation for DeleteInputSecurityGroupResponse

##### Instances
``` purescript
Newtype DeleteInputSecurityGroupResponse _
```

#### `DescribeChannelRequest`

``` purescript
newtype DescribeChannelRequest
  = DescribeChannelRequest { "ChannelId" :: String }
```

Placeholder documentation for DescribeChannelRequest

##### Instances
``` purescript
Newtype DescribeChannelRequest _
```

#### `DescribeChannelResponse`

``` purescript
newtype DescribeChannelResponse
  = DescribeChannelResponse { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for DescribeChannelResponse

##### Instances
``` purescript
Newtype DescribeChannelResponse _
```

#### `DescribeInputRequest`

``` purescript
newtype DescribeInputRequest
  = DescribeInputRequest { "InputId" :: String }
```

Placeholder documentation for DescribeInputRequest

##### Instances
``` purescript
Newtype DescribeInputRequest _
```

#### `DescribeInputResponse`

``` purescript
newtype DescribeInputResponse
  = DescribeInputResponse { "Arn" :: NullOrUndefined (String), "AttachedChannels" :: NullOrUndefined (ListOf__string), "Destinations" :: NullOrUndefined (ListOfInputDestination), "Id" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SecurityGroups" :: NullOrUndefined (ListOf__string), "Sources" :: NullOrUndefined (ListOfInputSource), "State" :: NullOrUndefined (InputState), "Type" :: NullOrUndefined (InputType) }
```

Placeholder documentation for DescribeInputResponse

##### Instances
``` purescript
Newtype DescribeInputResponse _
```

#### `DescribeInputSecurityGroupRequest`

``` purescript
newtype DescribeInputSecurityGroupRequest
  = DescribeInputSecurityGroupRequest { "InputSecurityGroupId" :: String }
```

Placeholder documentation for DescribeInputSecurityGroupRequest

##### Instances
``` purescript
Newtype DescribeInputSecurityGroupRequest _
```

#### `DescribeInputSecurityGroupResponse`

``` purescript
newtype DescribeInputSecurityGroupResponse
  = DescribeInputSecurityGroupResponse { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRule) }
```

Placeholder documentation for DescribeInputSecurityGroupResponse

##### Instances
``` purescript
Newtype DescribeInputSecurityGroupResponse _
```

#### `DvbNitSettings`

``` purescript
newtype DvbNitSettings
  = DvbNitSettings { "NetworkId" :: NullOrUndefined (Int), "NetworkName" :: NullOrUndefined (String), "RepInterval" :: NullOrUndefined (Int) }
```

DVB Network Information Table (NIT)

##### Instances
``` purescript
Newtype DvbNitSettings _
```

#### `DvbSdtOutputSdt`

``` purescript
newtype DvbSdtOutputSdt
  = DvbSdtOutputSdt String
```

Placeholder documentation for DvbSdtOutputSdt

##### Instances
``` purescript
Newtype DvbSdtOutputSdt _
```

#### `DvbSdtSettings`

``` purescript
newtype DvbSdtSettings
  = DvbSdtSettings { "OutputSdt" :: NullOrUndefined (DvbSdtOutputSdt), "RepInterval" :: NullOrUndefined (Int), "ServiceName" :: NullOrUndefined (String), "ServiceProviderName" :: NullOrUndefined (String) }
```

DVB Service Description Table (SDT)

##### Instances
``` purescript
Newtype DvbSdtSettings _
```

#### `DvbSubDestinationAlignment`

``` purescript
newtype DvbSubDestinationAlignment
  = DvbSubDestinationAlignment String
```

Placeholder documentation for DvbSubDestinationAlignment

##### Instances
``` purescript
Newtype DvbSubDestinationAlignment _
```

#### `DvbSubDestinationBackgroundColor`

``` purescript
newtype DvbSubDestinationBackgroundColor
  = DvbSubDestinationBackgroundColor String
```

Placeholder documentation for DvbSubDestinationBackgroundColor

##### Instances
``` purescript
Newtype DvbSubDestinationBackgroundColor _
```

#### `DvbSubDestinationFontColor`

``` purescript
newtype DvbSubDestinationFontColor
  = DvbSubDestinationFontColor String
```

Placeholder documentation for DvbSubDestinationFontColor

##### Instances
``` purescript
Newtype DvbSubDestinationFontColor _
```

#### `DvbSubDestinationOutlineColor`

``` purescript
newtype DvbSubDestinationOutlineColor
  = DvbSubDestinationOutlineColor String
```

Placeholder documentation for DvbSubDestinationOutlineColor

##### Instances
``` purescript
Newtype DvbSubDestinationOutlineColor _
```

#### `DvbSubDestinationSettings`

``` purescript
newtype DvbSubDestinationSettings
  = DvbSubDestinationSettings { "Alignment" :: NullOrUndefined (DvbSubDestinationAlignment), "BackgroundColor" :: NullOrUndefined (DvbSubDestinationBackgroundColor), "BackgroundOpacity" :: NullOrUndefined (Int), "Font" :: NullOrUndefined (InputLocation), "FontColor" :: NullOrUndefined (DvbSubDestinationFontColor), "FontOpacity" :: NullOrUndefined (Int), "FontResolution" :: NullOrUndefined (Int), "FontSize" :: NullOrUndefined (String), "OutlineColor" :: NullOrUndefined (DvbSubDestinationOutlineColor), "OutlineSize" :: NullOrUndefined (Int), "ShadowColor" :: NullOrUndefined (DvbSubDestinationShadowColor), "ShadowOpacity" :: NullOrUndefined (Int), "ShadowXOffset" :: NullOrUndefined (Int), "ShadowYOffset" :: NullOrUndefined (Int), "TeletextGridControl" :: NullOrUndefined (DvbSubDestinationTeletextGridControl), "XPosition" :: NullOrUndefined (Int), "YPosition" :: NullOrUndefined (Int) }
```

Placeholder documentation for DvbSubDestinationSettings

##### Instances
``` purescript
Newtype DvbSubDestinationSettings _
```

#### `DvbSubDestinationShadowColor`

``` purescript
newtype DvbSubDestinationShadowColor
  = DvbSubDestinationShadowColor String
```

Placeholder documentation for DvbSubDestinationShadowColor

##### Instances
``` purescript
Newtype DvbSubDestinationShadowColor _
```

#### `DvbSubDestinationTeletextGridControl`

``` purescript
newtype DvbSubDestinationTeletextGridControl
  = DvbSubDestinationTeletextGridControl String
```

Placeholder documentation for DvbSubDestinationTeletextGridControl

##### Instances
``` purescript
Newtype DvbSubDestinationTeletextGridControl _
```

#### `DvbSubSourceSettings`

``` purescript
newtype DvbSubSourceSettings
  = DvbSubSourceSettings { "Pid" :: NullOrUndefined (Int) }
```

Placeholder documentation for DvbSubSourceSettings

##### Instances
``` purescript
Newtype DvbSubSourceSettings _
```

#### `DvbTdtSettings`

``` purescript
newtype DvbTdtSettings
  = DvbTdtSettings { "RepInterval" :: NullOrUndefined (Int) }
```

DVB Time and Date Table (SDT)

##### Instances
``` purescript
Newtype DvbTdtSettings _
```

#### `Eac3AttenuationControl`

``` purescript
newtype Eac3AttenuationControl
  = Eac3AttenuationControl String
```

Placeholder documentation for Eac3AttenuationControl

##### Instances
``` purescript
Newtype Eac3AttenuationControl _
```

#### `Eac3BitstreamMode`

``` purescript
newtype Eac3BitstreamMode
  = Eac3BitstreamMode String
```

Placeholder documentation for Eac3BitstreamMode

##### Instances
``` purescript
Newtype Eac3BitstreamMode _
```

#### `Eac3CodingMode`

``` purescript
newtype Eac3CodingMode
  = Eac3CodingMode String
```

Placeholder documentation for Eac3CodingMode

##### Instances
``` purescript
Newtype Eac3CodingMode _
```

#### `Eac3DcFilter`

``` purescript
newtype Eac3DcFilter
  = Eac3DcFilter String
```

Placeholder documentation for Eac3DcFilter

##### Instances
``` purescript
Newtype Eac3DcFilter _
```

#### `Eac3DrcLine`

``` purescript
newtype Eac3DrcLine
  = Eac3DrcLine String
```

Placeholder documentation for Eac3DrcLine

##### Instances
``` purescript
Newtype Eac3DrcLine _
```

#### `Eac3DrcRf`

``` purescript
newtype Eac3DrcRf
  = Eac3DrcRf String
```

Placeholder documentation for Eac3DrcRf

##### Instances
``` purescript
Newtype Eac3DrcRf _
```

#### `Eac3LfeControl`

``` purescript
newtype Eac3LfeControl
  = Eac3LfeControl String
```

Placeholder documentation for Eac3LfeControl

##### Instances
``` purescript
Newtype Eac3LfeControl _
```

#### `Eac3LfeFilter`

``` purescript
newtype Eac3LfeFilter
  = Eac3LfeFilter String
```

Placeholder documentation for Eac3LfeFilter

##### Instances
``` purescript
Newtype Eac3LfeFilter _
```

#### `Eac3MetadataControl`

``` purescript
newtype Eac3MetadataControl
  = Eac3MetadataControl String
```

Placeholder documentation for Eac3MetadataControl

##### Instances
``` purescript
Newtype Eac3MetadataControl _
```

#### `Eac3PassthroughControl`

``` purescript
newtype Eac3PassthroughControl
  = Eac3PassthroughControl String
```

Placeholder documentation for Eac3PassthroughControl

##### Instances
``` purescript
Newtype Eac3PassthroughControl _
```

#### `Eac3PhaseControl`

``` purescript
newtype Eac3PhaseControl
  = Eac3PhaseControl String
```

Placeholder documentation for Eac3PhaseControl

##### Instances
``` purescript
Newtype Eac3PhaseControl _
```

#### `Eac3Settings`

``` purescript
newtype Eac3Settings
  = Eac3Settings { "AttenuationControl" :: NullOrUndefined (Eac3AttenuationControl), "Bitrate" :: NullOrUndefined (Number), "BitstreamMode" :: NullOrUndefined (Eac3BitstreamMode), "CodingMode" :: NullOrUndefined (Eac3CodingMode), "DcFilter" :: NullOrUndefined (Eac3DcFilter), "Dialnorm" :: NullOrUndefined (Int), "DrcLine" :: NullOrUndefined (Eac3DrcLine), "DrcRf" :: NullOrUndefined (Eac3DrcRf), "LfeControl" :: NullOrUndefined (Eac3LfeControl), "LfeFilter" :: NullOrUndefined (Eac3LfeFilter), "LoRoCenterMixLevel" :: NullOrUndefined (Number), "LoRoSurroundMixLevel" :: NullOrUndefined (Number), "LtRtCenterMixLevel" :: NullOrUndefined (Number), "LtRtSurroundMixLevel" :: NullOrUndefined (Number), "MetadataControl" :: NullOrUndefined (Eac3MetadataControl), "PassthroughControl" :: NullOrUndefined (Eac3PassthroughControl), "PhaseControl" :: NullOrUndefined (Eac3PhaseControl), "StereoDownmix" :: NullOrUndefined (Eac3StereoDownmix), "SurroundExMode" :: NullOrUndefined (Eac3SurroundExMode), "SurroundMode" :: NullOrUndefined (Eac3SurroundMode) }
```

Placeholder documentation for Eac3Settings

##### Instances
``` purescript
Newtype Eac3Settings _
```

#### `Eac3StereoDownmix`

``` purescript
newtype Eac3StereoDownmix
  = Eac3StereoDownmix String
```

Placeholder documentation for Eac3StereoDownmix

##### Instances
``` purescript
Newtype Eac3StereoDownmix _
```

#### `Eac3SurroundExMode`

``` purescript
newtype Eac3SurroundExMode
  = Eac3SurroundExMode String
```

Placeholder documentation for Eac3SurroundExMode

##### Instances
``` purescript
Newtype Eac3SurroundExMode _
```

#### `Eac3SurroundMode`

``` purescript
newtype Eac3SurroundMode
  = Eac3SurroundMode String
```

Placeholder documentation for Eac3SurroundMode

##### Instances
``` purescript
Newtype Eac3SurroundMode _
```

#### `EmbeddedConvert608To708`

``` purescript
newtype EmbeddedConvert608To708
  = EmbeddedConvert608To708 String
```

Placeholder documentation for EmbeddedConvert608To708

##### Instances
``` purescript
Newtype EmbeddedConvert608To708 _
```

#### `EmbeddedDestinationSettings`

``` purescript
newtype EmbeddedDestinationSettings
  = EmbeddedDestinationSettings {  }
```

Placeholder documentation for EmbeddedDestinationSettings

##### Instances
``` purescript
Newtype EmbeddedDestinationSettings _
```

#### `EmbeddedPlusScte20DestinationSettings`

``` purescript
newtype EmbeddedPlusScte20DestinationSettings
  = EmbeddedPlusScte20DestinationSettings {  }
```

Placeholder documentation for EmbeddedPlusScte20DestinationSettings

##### Instances
``` purescript
Newtype EmbeddedPlusScte20DestinationSettings _
```

#### `EmbeddedScte20Detection`

``` purescript
newtype EmbeddedScte20Detection
  = EmbeddedScte20Detection String
```

Placeholder documentation for EmbeddedScte20Detection

##### Instances
``` purescript
Newtype EmbeddedScte20Detection _
```

#### `EmbeddedSourceSettings`

``` purescript
newtype EmbeddedSourceSettings
  = EmbeddedSourceSettings { "Convert608To708" :: NullOrUndefined (EmbeddedConvert608To708), "Scte20Detection" :: NullOrUndefined (EmbeddedScte20Detection), "Source608ChannelNumber" :: NullOrUndefined (Int), "Source608TrackNumber" :: NullOrUndefined (Int) }
```

Placeholder documentation for EmbeddedSourceSettings

##### Instances
``` purescript
Newtype EmbeddedSourceSettings _
```

#### `Empty`

``` purescript
newtype Empty
  = Empty {  }
```

Placeholder documentation for Empty

##### Instances
``` purescript
Newtype Empty _
```

#### `EncoderSettings`

``` purescript
newtype EncoderSettings
  = EncoderSettings { "AudioDescriptions" :: NullOrUndefined (ListOfAudioDescription), "AvailBlanking" :: NullOrUndefined (AvailBlanking), "AvailConfiguration" :: NullOrUndefined (AvailConfiguration), "BlackoutSlate" :: NullOrUndefined (BlackoutSlate), "CaptionDescriptions" :: NullOrUndefined (ListOfCaptionDescription), "GlobalConfiguration" :: NullOrUndefined (GlobalConfiguration), "OutputGroups" :: NullOrUndefined (ListOfOutputGroup), "TimecodeConfig" :: NullOrUndefined (TimecodeConfig), "VideoDescriptions" :: NullOrUndefined (ListOfVideoDescription) }
```

Placeholder documentation for EncoderSettings

##### Instances
``` purescript
Newtype EncoderSettings _
```

#### `FecOutputIncludeFec`

``` purescript
newtype FecOutputIncludeFec
  = FecOutputIncludeFec String
```

Placeholder documentation for FecOutputIncludeFec

##### Instances
``` purescript
Newtype FecOutputIncludeFec _
```

#### `FecOutputSettings`

``` purescript
newtype FecOutputSettings
  = FecOutputSettings { "ColumnDepth" :: NullOrUndefined (Int), "IncludeFec" :: NullOrUndefined (FecOutputIncludeFec), "RowLength" :: NullOrUndefined (Int) }
```

Placeholder documentation for FecOutputSettings

##### Instances
``` purescript
Newtype FecOutputSettings _
```

#### `FixedAfd`

``` purescript
newtype FixedAfd
  = FixedAfd String
```

Placeholder documentation for FixedAfd

##### Instances
``` purescript
Newtype FixedAfd _
```

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for ForbiddenException

##### Instances
``` purescript
Newtype ForbiddenException _
```

#### `GatewayTimeoutException`

``` purescript
newtype GatewayTimeoutException
  = GatewayTimeoutException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for GatewayTimeoutException

##### Instances
``` purescript
Newtype GatewayTimeoutException _
```

#### `GlobalConfiguration`

``` purescript
newtype GlobalConfiguration
  = GlobalConfiguration { "InitialAudioGain" :: NullOrUndefined (Int), "InputEndAction" :: NullOrUndefined (GlobalConfigurationInputEndAction), "InputLossBehavior" :: NullOrUndefined (InputLossBehavior), "OutputTimingSource" :: NullOrUndefined (GlobalConfigurationOutputTimingSource), "SupportLowFramerateInputs" :: NullOrUndefined (GlobalConfigurationLowFramerateInputs) }
```

Placeholder documentation for GlobalConfiguration

##### Instances
``` purescript
Newtype GlobalConfiguration _
```

#### `GlobalConfigurationInputEndAction`

``` purescript
newtype GlobalConfigurationInputEndAction
  = GlobalConfigurationInputEndAction String
```

Placeholder documentation for GlobalConfigurationInputEndAction

##### Instances
``` purescript
Newtype GlobalConfigurationInputEndAction _
```

#### `GlobalConfigurationLowFramerateInputs`

``` purescript
newtype GlobalConfigurationLowFramerateInputs
  = GlobalConfigurationLowFramerateInputs String
```

Placeholder documentation for GlobalConfigurationLowFramerateInputs

##### Instances
``` purescript
Newtype GlobalConfigurationLowFramerateInputs _
```

#### `GlobalConfigurationOutputTimingSource`

``` purescript
newtype GlobalConfigurationOutputTimingSource
  = GlobalConfigurationOutputTimingSource String
```

Placeholder documentation for GlobalConfigurationOutputTimingSource

##### Instances
``` purescript
Newtype GlobalConfigurationOutputTimingSource _
```

#### `H264AdaptiveQuantization`

``` purescript
newtype H264AdaptiveQuantization
  = H264AdaptiveQuantization String
```

Placeholder documentation for H264AdaptiveQuantization

##### Instances
``` purescript
Newtype H264AdaptiveQuantization _
```

#### `H264ColorMetadata`

``` purescript
newtype H264ColorMetadata
  = H264ColorMetadata String
```

Placeholder documentation for H264ColorMetadata

##### Instances
``` purescript
Newtype H264ColorMetadata _
```

#### `H264EntropyEncoding`

``` purescript
newtype H264EntropyEncoding
  = H264EntropyEncoding String
```

Placeholder documentation for H264EntropyEncoding

##### Instances
``` purescript
Newtype H264EntropyEncoding _
```

#### `H264FlickerAq`

``` purescript
newtype H264FlickerAq
  = H264FlickerAq String
```

Placeholder documentation for H264FlickerAq

##### Instances
``` purescript
Newtype H264FlickerAq _
```

#### `H264FramerateControl`

``` purescript
newtype H264FramerateControl
  = H264FramerateControl String
```

Placeholder documentation for H264FramerateControl

##### Instances
``` purescript
Newtype H264FramerateControl _
```

#### `H264GopBReference`

``` purescript
newtype H264GopBReference
  = H264GopBReference String
```

Placeholder documentation for H264GopBReference

##### Instances
``` purescript
Newtype H264GopBReference _
```

#### `H264GopSizeUnits`

``` purescript
newtype H264GopSizeUnits
  = H264GopSizeUnits String
```

Placeholder documentation for H264GopSizeUnits

##### Instances
``` purescript
Newtype H264GopSizeUnits _
```

#### `H264Level`

``` purescript
newtype H264Level
  = H264Level String
```

Placeholder documentation for H264Level

##### Instances
``` purescript
Newtype H264Level _
```

#### `H264LookAheadRateControl`

``` purescript
newtype H264LookAheadRateControl
  = H264LookAheadRateControl String
```

Placeholder documentation for H264LookAheadRateControl

##### Instances
``` purescript
Newtype H264LookAheadRateControl _
```

#### `H264ParControl`

``` purescript
newtype H264ParControl
  = H264ParControl String
```

Placeholder documentation for H264ParControl

##### Instances
``` purescript
Newtype H264ParControl _
```

#### `H264Profile`

``` purescript
newtype H264Profile
  = H264Profile String
```

Placeholder documentation for H264Profile

##### Instances
``` purescript
Newtype H264Profile _
```

#### `H264RateControlMode`

``` purescript
newtype H264RateControlMode
  = H264RateControlMode String
```

Placeholder documentation for H264RateControlMode

##### Instances
``` purescript
Newtype H264RateControlMode _
```

#### `H264ScanType`

``` purescript
newtype H264ScanType
  = H264ScanType String
```

Placeholder documentation for H264ScanType

##### Instances
``` purescript
Newtype H264ScanType _
```

#### `H264SceneChangeDetect`

``` purescript
newtype H264SceneChangeDetect
  = H264SceneChangeDetect String
```

Placeholder documentation for H264SceneChangeDetect

##### Instances
``` purescript
Newtype H264SceneChangeDetect _
```

#### `H264Settings`

``` purescript
newtype H264Settings
  = H264Settings { "AdaptiveQuantization" :: NullOrUndefined (H264AdaptiveQuantization), "AfdSignaling" :: NullOrUndefined (AfdSignaling), "Bitrate" :: NullOrUndefined (Int), "BufFillPct" :: NullOrUndefined (Int), "BufSize" :: NullOrUndefined (Int), "ColorMetadata" :: NullOrUndefined (H264ColorMetadata), "EntropyEncoding" :: NullOrUndefined (H264EntropyEncoding), "FixedAfd" :: NullOrUndefined (FixedAfd), "FlickerAq" :: NullOrUndefined (H264FlickerAq), "FramerateControl" :: NullOrUndefined (H264FramerateControl), "FramerateDenominator" :: NullOrUndefined (Int), "FramerateNumerator" :: NullOrUndefined (Int), "GopBReference" :: NullOrUndefined (H264GopBReference), "GopClosedCadence" :: NullOrUndefined (Int), "GopNumBFrames" :: NullOrUndefined (Int), "GopSize" :: NullOrUndefined (Number), "GopSizeUnits" :: NullOrUndefined (H264GopSizeUnits), "Level" :: NullOrUndefined (H264Level), "LookAheadRateControl" :: NullOrUndefined (H264LookAheadRateControl), "MaxBitrate" :: NullOrUndefined (Int), "MinIInterval" :: NullOrUndefined (Int), "NumRefFrames" :: NullOrUndefined (Int), "ParControl" :: NullOrUndefined (H264ParControl), "ParDenominator" :: NullOrUndefined (Int), "ParNumerator" :: NullOrUndefined (Int), "Profile" :: NullOrUndefined (H264Profile), "RateControlMode" :: NullOrUndefined (H264RateControlMode), "ScanType" :: NullOrUndefined (H264ScanType), "SceneChangeDetect" :: NullOrUndefined (H264SceneChangeDetect), "Slices" :: NullOrUndefined (Int), "Softness" :: NullOrUndefined (Int), "SpatialAq" :: NullOrUndefined (H264SpatialAq), "Syntax" :: NullOrUndefined (H264Syntax), "TemporalAq" :: NullOrUndefined (H264TemporalAq), "TimecodeInsertion" :: NullOrUndefined (H264TimecodeInsertionBehavior) }
```

Placeholder documentation for H264Settings

##### Instances
``` purescript
Newtype H264Settings _
```

#### `H264SpatialAq`

``` purescript
newtype H264SpatialAq
  = H264SpatialAq String
```

Placeholder documentation for H264SpatialAq

##### Instances
``` purescript
Newtype H264SpatialAq _
```

#### `H264Syntax`

``` purescript
newtype H264Syntax
  = H264Syntax String
```

Placeholder documentation for H264Syntax

##### Instances
``` purescript
Newtype H264Syntax _
```

#### `H264TemporalAq`

``` purescript
newtype H264TemporalAq
  = H264TemporalAq String
```

Placeholder documentation for H264TemporalAq

##### Instances
``` purescript
Newtype H264TemporalAq _
```

#### `H264TimecodeInsertionBehavior`

``` purescript
newtype H264TimecodeInsertionBehavior
  = H264TimecodeInsertionBehavior String
```

Placeholder documentation for H264TimecodeInsertionBehavior

##### Instances
``` purescript
Newtype H264TimecodeInsertionBehavior _
```

#### `HlsAdMarkers`

``` purescript
newtype HlsAdMarkers
  = HlsAdMarkers String
```

Placeholder documentation for HlsAdMarkers

##### Instances
``` purescript
Newtype HlsAdMarkers _
```

#### `HlsAkamaiHttpTransferMode`

``` purescript
newtype HlsAkamaiHttpTransferMode
  = HlsAkamaiHttpTransferMode String
```

Placeholder documentation for HlsAkamaiHttpTransferMode

##### Instances
``` purescript
Newtype HlsAkamaiHttpTransferMode _
```

#### `HlsAkamaiSettings`

``` purescript
newtype HlsAkamaiSettings
  = HlsAkamaiSettings { "ConnectionRetryInterval" :: NullOrUndefined (Int), "FilecacheDuration" :: NullOrUndefined (Int), "HttpTransferMode" :: NullOrUndefined (HlsAkamaiHttpTransferMode), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int), "Salt" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

Placeholder documentation for HlsAkamaiSettings

##### Instances
``` purescript
Newtype HlsAkamaiSettings _
```

#### `HlsBasicPutSettings`

``` purescript
newtype HlsBasicPutSettings
  = HlsBasicPutSettings { "ConnectionRetryInterval" :: NullOrUndefined (Int), "FilecacheDuration" :: NullOrUndefined (Int), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int) }
```

Placeholder documentation for HlsBasicPutSettings

##### Instances
``` purescript
Newtype HlsBasicPutSettings _
```

#### `HlsCaptionLanguageSetting`

``` purescript
newtype HlsCaptionLanguageSetting
  = HlsCaptionLanguageSetting String
```

Placeholder documentation for HlsCaptionLanguageSetting

##### Instances
``` purescript
Newtype HlsCaptionLanguageSetting _
```

#### `HlsCdnSettings`

``` purescript
newtype HlsCdnSettings
  = HlsCdnSettings { "HlsAkamaiSettings" :: NullOrUndefined (HlsAkamaiSettings), "HlsBasicPutSettings" :: NullOrUndefined (HlsBasicPutSettings), "HlsMediaStoreSettings" :: NullOrUndefined (HlsMediaStoreSettings), "HlsWebdavSettings" :: NullOrUndefined (HlsWebdavSettings) }
```

Placeholder documentation for HlsCdnSettings

##### Instances
``` purescript
Newtype HlsCdnSettings _
```

#### `HlsClientCache`

``` purescript
newtype HlsClientCache
  = HlsClientCache String
```

Placeholder documentation for HlsClientCache

##### Instances
``` purescript
Newtype HlsClientCache _
```

#### `HlsCodecSpecification`

``` purescript
newtype HlsCodecSpecification
  = HlsCodecSpecification String
```

Placeholder documentation for HlsCodecSpecification

##### Instances
``` purescript
Newtype HlsCodecSpecification _
```

#### `HlsDirectoryStructure`

``` purescript
newtype HlsDirectoryStructure
  = HlsDirectoryStructure String
```

Placeholder documentation for HlsDirectoryStructure

##### Instances
``` purescript
Newtype HlsDirectoryStructure _
```

#### `HlsEncryptionType`

``` purescript
newtype HlsEncryptionType
  = HlsEncryptionType String
```

Placeholder documentation for HlsEncryptionType

##### Instances
``` purescript
Newtype HlsEncryptionType _
```

#### `HlsGroupSettings`

``` purescript
newtype HlsGroupSettings
  = HlsGroupSettings { "AdMarkers" :: NullOrUndefined (ListOfHlsAdMarkers), "BaseUrlContent" :: NullOrUndefined (String), "BaseUrlManifest" :: NullOrUndefined (String), "CaptionLanguageMappings" :: NullOrUndefined (ListOfCaptionLanguageMapping), "CaptionLanguageSetting" :: NullOrUndefined (HlsCaptionLanguageSetting), "ClientCache" :: NullOrUndefined (HlsClientCache), "CodecSpecification" :: NullOrUndefined (HlsCodecSpecification), "ConstantIv" :: NullOrUndefined (String), "Destination" :: NullOrUndefined (OutputLocationRef), "DirectoryStructure" :: NullOrUndefined (HlsDirectoryStructure), "EncryptionType" :: NullOrUndefined (HlsEncryptionType), "HlsCdnSettings" :: NullOrUndefined (HlsCdnSettings), "IndexNSegments" :: NullOrUndefined (Int), "InputLossAction" :: NullOrUndefined (InputLossActionForHlsOut), "IvInManifest" :: NullOrUndefined (HlsIvInManifest), "IvSource" :: NullOrUndefined (HlsIvSource), "KeepSegments" :: NullOrUndefined (Int), "KeyFormat" :: NullOrUndefined (String), "KeyFormatVersions" :: NullOrUndefined (String), "KeyProviderSettings" :: NullOrUndefined (KeyProviderSettings), "ManifestCompression" :: NullOrUndefined (HlsManifestCompression), "ManifestDurationFormat" :: NullOrUndefined (HlsManifestDurationFormat), "MinSegmentLength" :: NullOrUndefined (Int), "Mode" :: NullOrUndefined (HlsMode), "OutputSelection" :: NullOrUndefined (HlsOutputSelection), "ProgramDateTime" :: NullOrUndefined (HlsProgramDateTime), "ProgramDateTimePeriod" :: NullOrUndefined (Int), "SegmentLength" :: NullOrUndefined (Int), "SegmentationMode" :: NullOrUndefined (HlsSegmentationMode), "SegmentsPerSubdirectory" :: NullOrUndefined (Int), "StreamInfResolution" :: NullOrUndefined (HlsStreamInfResolution), "TimedMetadataId3Frame" :: NullOrUndefined (HlsTimedMetadataId3Frame), "TimedMetadataId3Period" :: NullOrUndefined (Int), "TimestampDeltaMilliseconds" :: NullOrUndefined (Int), "TsFileMode" :: NullOrUndefined (HlsTsFileMode) }
```

Placeholder documentation for HlsGroupSettings

##### Instances
``` purescript
Newtype HlsGroupSettings _
```

#### `HlsInputSettings`

``` purescript
newtype HlsInputSettings
  = HlsInputSettings { "Bandwidth" :: NullOrUndefined (Int), "BufferSegments" :: NullOrUndefined (Int), "Retries" :: NullOrUndefined (Int), "RetryInterval" :: NullOrUndefined (Int) }
```

Placeholder documentation for HlsInputSettings

##### Instances
``` purescript
Newtype HlsInputSettings _
```

#### `HlsIvInManifest`

``` purescript
newtype HlsIvInManifest
  = HlsIvInManifest String
```

Placeholder documentation for HlsIvInManifest

##### Instances
``` purescript
Newtype HlsIvInManifest _
```

#### `HlsIvSource`

``` purescript
newtype HlsIvSource
  = HlsIvSource String
```

Placeholder documentation for HlsIvSource

##### Instances
``` purescript
Newtype HlsIvSource _
```

#### `HlsManifestCompression`

``` purescript
newtype HlsManifestCompression
  = HlsManifestCompression String
```

Placeholder documentation for HlsManifestCompression

##### Instances
``` purescript
Newtype HlsManifestCompression _
```

#### `HlsManifestDurationFormat`

``` purescript
newtype HlsManifestDurationFormat
  = HlsManifestDurationFormat String
```

Placeholder documentation for HlsManifestDurationFormat

##### Instances
``` purescript
Newtype HlsManifestDurationFormat _
```

#### `HlsMediaStoreSettings`

``` purescript
newtype HlsMediaStoreSettings
  = HlsMediaStoreSettings { "ConnectionRetryInterval" :: NullOrUndefined (Int), "FilecacheDuration" :: NullOrUndefined (Int), "MediaStoreStorageClass" :: NullOrUndefined (HlsMediaStoreStorageClass), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int) }
```

Placeholder documentation for HlsMediaStoreSettings

##### Instances
``` purescript
Newtype HlsMediaStoreSettings _
```

#### `HlsMediaStoreStorageClass`

``` purescript
newtype HlsMediaStoreStorageClass
  = HlsMediaStoreStorageClass String
```

Placeholder documentation for HlsMediaStoreStorageClass

##### Instances
``` purescript
Newtype HlsMediaStoreStorageClass _
```

#### `HlsMode`

``` purescript
newtype HlsMode
  = HlsMode String
```

Placeholder documentation for HlsMode

##### Instances
``` purescript
Newtype HlsMode _
```

#### `HlsOutputSelection`

``` purescript
newtype HlsOutputSelection
  = HlsOutputSelection String
```

Placeholder documentation for HlsOutputSelection

##### Instances
``` purescript
Newtype HlsOutputSelection _
```

#### `HlsOutputSettings`

``` purescript
newtype HlsOutputSettings
  = HlsOutputSettings { "HlsSettings" :: NullOrUndefined (HlsSettings), "NameModifier" :: NullOrUndefined (String), "SegmentModifier" :: NullOrUndefined (String) }
```

Placeholder documentation for HlsOutputSettings

##### Instances
``` purescript
Newtype HlsOutputSettings _
```

#### `HlsProgramDateTime`

``` purescript
newtype HlsProgramDateTime
  = HlsProgramDateTime String
```

Placeholder documentation for HlsProgramDateTime

##### Instances
``` purescript
Newtype HlsProgramDateTime _
```

#### `HlsSegmentationMode`

``` purescript
newtype HlsSegmentationMode
  = HlsSegmentationMode String
```

Placeholder documentation for HlsSegmentationMode

##### Instances
``` purescript
Newtype HlsSegmentationMode _
```

#### `HlsSettings`

``` purescript
newtype HlsSettings
  = HlsSettings { "AudioOnlyHlsSettings" :: NullOrUndefined (AudioOnlyHlsSettings), "StandardHlsSettings" :: NullOrUndefined (StandardHlsSettings) }
```

Placeholder documentation for HlsSettings

##### Instances
``` purescript
Newtype HlsSettings _
```

#### `HlsStreamInfResolution`

``` purescript
newtype HlsStreamInfResolution
  = HlsStreamInfResolution String
```

Placeholder documentation for HlsStreamInfResolution

##### Instances
``` purescript
Newtype HlsStreamInfResolution _
```

#### `HlsTimedMetadataId3Frame`

``` purescript
newtype HlsTimedMetadataId3Frame
  = HlsTimedMetadataId3Frame String
```

Placeholder documentation for HlsTimedMetadataId3Frame

##### Instances
``` purescript
Newtype HlsTimedMetadataId3Frame _
```

#### `HlsTsFileMode`

``` purescript
newtype HlsTsFileMode
  = HlsTsFileMode String
```

Placeholder documentation for HlsTsFileMode

##### Instances
``` purescript
Newtype HlsTsFileMode _
```

#### `HlsWebdavHttpTransferMode`

``` purescript
newtype HlsWebdavHttpTransferMode
  = HlsWebdavHttpTransferMode String
```

Placeholder documentation for HlsWebdavHttpTransferMode

##### Instances
``` purescript
Newtype HlsWebdavHttpTransferMode _
```

#### `HlsWebdavSettings`

``` purescript
newtype HlsWebdavSettings
  = HlsWebdavSettings { "ConnectionRetryInterval" :: NullOrUndefined (Int), "FilecacheDuration" :: NullOrUndefined (Int), "HttpTransferMode" :: NullOrUndefined (HlsWebdavHttpTransferMode), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int) }
```

Placeholder documentation for HlsWebdavSettings

##### Instances
``` purescript
Newtype HlsWebdavSettings _
```

#### `Input`

``` purescript
newtype Input
  = Input { "Arn" :: NullOrUndefined (String), "AttachedChannels" :: NullOrUndefined (ListOf__string), "Destinations" :: NullOrUndefined (ListOfInputDestination), "Id" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SecurityGroups" :: NullOrUndefined (ListOf__string), "Sources" :: NullOrUndefined (ListOfInputSource), "State" :: NullOrUndefined (InputState), "Type" :: NullOrUndefined (InputType) }
```

Placeholder documentation for Input

##### Instances
``` purescript
Newtype Input _
```

#### `InputAttachment`

``` purescript
newtype InputAttachment
  = InputAttachment { "InputId" :: NullOrUndefined (String), "InputSettings" :: NullOrUndefined (InputSettings) }
```

Placeholder documentation for InputAttachment

##### Instances
``` purescript
Newtype InputAttachment _
```

#### `InputChannelLevel`

``` purescript
newtype InputChannelLevel
  = InputChannelLevel { "Gain" :: NullOrUndefined (Int), "InputChannel" :: NullOrUndefined (Int) }
```

Placeholder documentation for InputChannelLevel

##### Instances
``` purescript
Newtype InputChannelLevel _
```

#### `InputCodec`

``` purescript
newtype InputCodec
  = InputCodec String
```

codec in increasing order of complexity

##### Instances
``` purescript
Newtype InputCodec _
```

#### `InputDeblockFilter`

``` purescript
newtype InputDeblockFilter
  = InputDeblockFilter String
```

Placeholder documentation for InputDeblockFilter

##### Instances
``` purescript
Newtype InputDeblockFilter _
```

#### `InputDenoiseFilter`

``` purescript
newtype InputDenoiseFilter
  = InputDenoiseFilter String
```

Placeholder documentation for InputDenoiseFilter

##### Instances
``` purescript
Newtype InputDenoiseFilter _
```

#### `InputDestination`

``` purescript
newtype InputDestination
  = InputDestination { "Ip" :: NullOrUndefined (String), "Port" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

The settings for a PUSH type input.

##### Instances
``` purescript
Newtype InputDestination _
```

#### `InputDestinationRequest`

``` purescript
newtype InputDestinationRequest
  = InputDestinationRequest { "StreamName" :: NullOrUndefined (String) }
```

Endpoint settings for a PUSH type input.

##### Instances
``` purescript
Newtype InputDestinationRequest _
```

#### `InputFilter`

``` purescript
newtype InputFilter
  = InputFilter String
```

Placeholder documentation for InputFilter

##### Instances
``` purescript
Newtype InputFilter _
```

#### `InputLocation`

``` purescript
newtype InputLocation
  = InputLocation { "PasswordParam" :: NullOrUndefined (String), "Uri" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

Placeholder documentation for InputLocation

##### Instances
``` purescript
Newtype InputLocation _
```

#### `InputLossActionForHlsOut`

``` purescript
newtype InputLossActionForHlsOut
  = InputLossActionForHlsOut String
```

Placeholder documentation for InputLossActionForHlsOut

##### Instances
``` purescript
Newtype InputLossActionForHlsOut _
```

#### `InputLossActionForMsSmoothOut`

``` purescript
newtype InputLossActionForMsSmoothOut
  = InputLossActionForMsSmoothOut String
```

Placeholder documentation for InputLossActionForMsSmoothOut

##### Instances
``` purescript
Newtype InputLossActionForMsSmoothOut _
```

#### `InputLossActionForUdpOut`

``` purescript
newtype InputLossActionForUdpOut
  = InputLossActionForUdpOut String
```

Placeholder documentation for InputLossActionForUdpOut

##### Instances
``` purescript
Newtype InputLossActionForUdpOut _
```

#### `InputLossBehavior`

``` purescript
newtype InputLossBehavior
  = InputLossBehavior { "BlackFrameMsec" :: NullOrUndefined (Int), "InputLossImageColor" :: NullOrUndefined (String), "InputLossImageSlate" :: NullOrUndefined (InputLocation), "InputLossImageType" :: NullOrUndefined (InputLossImageType), "RepeatFrameMsec" :: NullOrUndefined (Int) }
```

Placeholder documentation for InputLossBehavior

##### Instances
``` purescript
Newtype InputLossBehavior _
```

#### `InputLossImageType`

``` purescript
newtype InputLossImageType
  = InputLossImageType String
```

Placeholder documentation for InputLossImageType

##### Instances
``` purescript
Newtype InputLossImageType _
```

#### `InputMaximumBitrate`

``` purescript
newtype InputMaximumBitrate
  = InputMaximumBitrate String
```

Maximum input bitrate in megabits per second. Bitrates up to 50 Mbps are supported currently.

##### Instances
``` purescript
Newtype InputMaximumBitrate _
```

#### `InputResolution`

``` purescript
newtype InputResolution
  = InputResolution String
```

Input resolution based on lines of vertical resolution in the input; SD is less than 720 lines, HD is 720 to 1080 lines, UHD is greater than 1080 lines


##### Instances
``` purescript
Newtype InputResolution _
```

#### `InputSecurityGroup`

``` purescript
newtype InputSecurityGroup
  = InputSecurityGroup { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRule) }
```

An Input Security Group

##### Instances
``` purescript
Newtype InputSecurityGroup _
```

#### `InputSecurityGroupWhitelistRequest`

``` purescript
newtype InputSecurityGroupWhitelistRequest
  = InputSecurityGroupWhitelistRequest { "WhitelistRules" :: NullOrUndefined (ListOfInputWhitelistRuleCidr) }
```

Request of IPv4 CIDR addresses to whitelist in a security group.

##### Instances
``` purescript
Newtype InputSecurityGroupWhitelistRequest _
```

#### `InputSettings`

``` purescript
newtype InputSettings
  = InputSettings { "AudioSelectors" :: NullOrUndefined (ListOfAudioSelector), "CaptionSelectors" :: NullOrUndefined (ListOfCaptionSelector), "DeblockFilter" :: NullOrUndefined (InputDeblockFilter), "DenoiseFilter" :: NullOrUndefined (InputDenoiseFilter), "FilterStrength" :: NullOrUndefined (Int), "InputFilter" :: NullOrUndefined (InputFilter), "NetworkInputSettings" :: NullOrUndefined (NetworkInputSettings), "SourceEndBehavior" :: NullOrUndefined (InputSourceEndBehavior), "VideoSelector" :: NullOrUndefined (VideoSelector) }
```

Live Event input parameters. There can be multiple inputs in a single Live Event.

##### Instances
``` purescript
Newtype InputSettings _
```

#### `InputSource`

``` purescript
newtype InputSource
  = InputSource { "PasswordParam" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

The settings for a PULL type input.

##### Instances
``` purescript
Newtype InputSource _
```

#### `InputSourceEndBehavior`

``` purescript
newtype InputSourceEndBehavior
  = InputSourceEndBehavior String
```

Placeholder documentation for InputSourceEndBehavior

##### Instances
``` purescript
Newtype InputSourceEndBehavior _
```

#### `InputSourceRequest`

``` purescript
newtype InputSourceRequest
  = InputSourceRequest { "PasswordParam" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

Settings for for a PULL type input.

##### Instances
``` purescript
Newtype InputSourceRequest _
```

#### `InputSpecification`

``` purescript
newtype InputSpecification
  = InputSpecification { "Codec" :: NullOrUndefined (InputCodec), "MaximumBitrate" :: NullOrUndefined (InputMaximumBitrate), "Resolution" :: NullOrUndefined (InputResolution) }
```

Placeholder documentation for InputSpecification

##### Instances
``` purescript
Newtype InputSpecification _
```

#### `InputState`

``` purescript
newtype InputState
  = InputState String
```

Placeholder documentation for InputState

##### Instances
``` purescript
Newtype InputState _
```

#### `InputType`

``` purescript
newtype InputType
  = InputType String
```

Placeholder documentation for InputType

##### Instances
``` purescript
Newtype InputType _
```

#### `InputWhitelistRule`

``` purescript
newtype InputWhitelistRule
  = InputWhitelistRule { "Cidr" :: NullOrUndefined (String) }
```

Whitelist rule

##### Instances
``` purescript
Newtype InputWhitelistRule _
```

#### `InputWhitelistRuleCidr`

``` purescript
newtype InputWhitelistRuleCidr
  = InputWhitelistRuleCidr { "Cidr" :: NullOrUndefined (String) }
```

An IPv4 CIDR to whitelist.

##### Instances
``` purescript
Newtype InputWhitelistRuleCidr _
```

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for InternalServerErrorException

##### Instances
``` purescript
Newtype InternalServerErrorException _
```

#### `InternalServiceError`

``` purescript
newtype InternalServiceError
  = InternalServiceError { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for InternalServiceError

##### Instances
``` purescript
Newtype InternalServiceError _
```

#### `InvalidRequest`

``` purescript
newtype InvalidRequest
  = InvalidRequest { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for InvalidRequest

##### Instances
``` purescript
Newtype InvalidRequest _
```

#### `KeyProviderSettings`

``` purescript
newtype KeyProviderSettings
  = KeyProviderSettings { "StaticKeySettings" :: NullOrUndefined (StaticKeySettings) }
```

Placeholder documentation for KeyProviderSettings

##### Instances
``` purescript
Newtype KeyProviderSettings _
```

#### `LimitExceeded`

``` purescript
newtype LimitExceeded
  = LimitExceeded { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for LimitExceeded

##### Instances
``` purescript
Newtype LimitExceeded _
```

#### `ListChannelsRequest`

``` purescript
newtype ListChannelsRequest
  = ListChannelsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListChannelsRequest

##### Instances
``` purescript
Newtype ListChannelsRequest _
```

#### `ListChannelsResponse`

``` purescript
newtype ListChannelsResponse
  = ListChannelsResponse { "Channels" :: NullOrUndefined (ListOfChannelSummary), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListChannelsResponse

##### Instances
``` purescript
Newtype ListChannelsResponse _
```

#### `ListChannelsResultModel`

``` purescript
newtype ListChannelsResultModel
  = ListChannelsResultModel { "Channels" :: NullOrUndefined (ListOfChannelSummary), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListChannelsResultModel

##### Instances
``` purescript
Newtype ListChannelsResultModel _
```

#### `ListInputSecurityGroupsRequest`

``` purescript
newtype ListInputSecurityGroupsRequest
  = ListInputSecurityGroupsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputSecurityGroupsRequest

##### Instances
``` purescript
Newtype ListInputSecurityGroupsRequest _
```

#### `ListInputSecurityGroupsResponse`

``` purescript
newtype ListInputSecurityGroupsResponse
  = ListInputSecurityGroupsResponse { "InputSecurityGroups" :: NullOrUndefined (ListOfInputSecurityGroup), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputSecurityGroupsResponse

##### Instances
``` purescript
Newtype ListInputSecurityGroupsResponse _
```

#### `ListInputSecurityGroupsResultModel`

``` purescript
newtype ListInputSecurityGroupsResultModel
  = ListInputSecurityGroupsResultModel { "InputSecurityGroups" :: NullOrUndefined (ListOfInputSecurityGroup), "NextToken" :: NullOrUndefined (String) }
```

Result of input security group list request

##### Instances
``` purescript
Newtype ListInputSecurityGroupsResultModel _
```

#### `ListInputsRequest`

``` purescript
newtype ListInputsRequest
  = ListInputsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputsRequest

##### Instances
``` purescript
Newtype ListInputsRequest _
```

#### `ListInputsResponse`

``` purescript
newtype ListInputsResponse
  = ListInputsResponse { "Inputs" :: NullOrUndefined (ListOfInput), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputsResponse

##### Instances
``` purescript
Newtype ListInputsResponse _
```

#### `ListInputsResultModel`

``` purescript
newtype ListInputsResultModel
  = ListInputsResultModel { "Inputs" :: NullOrUndefined (ListOfInput), "NextToken" :: NullOrUndefined (String) }
```

Placeholder documentation for ListInputsResultModel

##### Instances
``` purescript
Newtype ListInputsResultModel _
```

#### `ListOfAudioChannelMapping`

``` purescript
newtype ListOfAudioChannelMapping
  = ListOfAudioChannelMapping (Array AudioChannelMapping)
```

Placeholder documentation for ListOfAudioChannelMapping

##### Instances
``` purescript
Newtype ListOfAudioChannelMapping _
```

#### `ListOfAudioDescription`

``` purescript
newtype ListOfAudioDescription
  = ListOfAudioDescription (Array AudioDescription)
```

Placeholder documentation for ListOfAudioDescription

##### Instances
``` purescript
Newtype ListOfAudioDescription _
```

#### `ListOfAudioSelector`

``` purescript
newtype ListOfAudioSelector
  = ListOfAudioSelector (Array AudioSelector)
```

Placeholder documentation for ListOfAudioSelector

##### Instances
``` purescript
Newtype ListOfAudioSelector _
```

#### `ListOfCaptionDescription`

``` purescript
newtype ListOfCaptionDescription
  = ListOfCaptionDescription (Array CaptionDescription)
```

Placeholder documentation for ListOfCaptionDescription

##### Instances
``` purescript
Newtype ListOfCaptionDescription _
```

#### `ListOfCaptionLanguageMapping`

``` purescript
newtype ListOfCaptionLanguageMapping
  = ListOfCaptionLanguageMapping (Array CaptionLanguageMapping)
```

Placeholder documentation for ListOfCaptionLanguageMapping

##### Instances
``` purescript
Newtype ListOfCaptionLanguageMapping _
```

#### `ListOfCaptionSelector`

``` purescript
newtype ListOfCaptionSelector
  = ListOfCaptionSelector (Array CaptionSelector)
```

Placeholder documentation for ListOfCaptionSelector

##### Instances
``` purescript
Newtype ListOfCaptionSelector _
```

#### `ListOfChannelEgressEndpoint`

``` purescript
newtype ListOfChannelEgressEndpoint
  = ListOfChannelEgressEndpoint (Array ChannelEgressEndpoint)
```

Placeholder documentation for ListOfChannelEgressEndpoint

##### Instances
``` purescript
Newtype ListOfChannelEgressEndpoint _
```

#### `ListOfChannelSummary`

``` purescript
newtype ListOfChannelSummary
  = ListOfChannelSummary (Array ChannelSummary)
```

Placeholder documentation for ListOfChannelSummary

##### Instances
``` purescript
Newtype ListOfChannelSummary _
```

#### `ListOfHlsAdMarkers`

``` purescript
newtype ListOfHlsAdMarkers
  = ListOfHlsAdMarkers (Array HlsAdMarkers)
```

Placeholder documentation for ListOfHlsAdMarkers

##### Instances
``` purescript
Newtype ListOfHlsAdMarkers _
```

#### `ListOfInput`

``` purescript
newtype ListOfInput
  = ListOfInput (Array Input)
```

Placeholder documentation for ListOfInput

##### Instances
``` purescript
Newtype ListOfInput _
```

#### `ListOfInputAttachment`

``` purescript
newtype ListOfInputAttachment
  = ListOfInputAttachment (Array InputAttachment)
```

Placeholder documentation for ListOfInputAttachment

##### Instances
``` purescript
Newtype ListOfInputAttachment _
```

#### `ListOfInputChannelLevel`

``` purescript
newtype ListOfInputChannelLevel
  = ListOfInputChannelLevel (Array InputChannelLevel)
```

Placeholder documentation for ListOfInputChannelLevel

##### Instances
``` purescript
Newtype ListOfInputChannelLevel _
```

#### `ListOfInputDestination`

``` purescript
newtype ListOfInputDestination
  = ListOfInputDestination (Array InputDestination)
```

Placeholder documentation for ListOfInputDestination

##### Instances
``` purescript
Newtype ListOfInputDestination _
```

#### `ListOfInputDestinationRequest`

``` purescript
newtype ListOfInputDestinationRequest
  = ListOfInputDestinationRequest (Array InputDestinationRequest)
```

Placeholder documentation for ListOfInputDestinationRequest

##### Instances
``` purescript
Newtype ListOfInputDestinationRequest _
```

#### `ListOfInputSecurityGroup`

``` purescript
newtype ListOfInputSecurityGroup
  = ListOfInputSecurityGroup (Array InputSecurityGroup)
```

Placeholder documentation for ListOfInputSecurityGroup

##### Instances
``` purescript
Newtype ListOfInputSecurityGroup _
```

#### `ListOfInputSource`

``` purescript
newtype ListOfInputSource
  = ListOfInputSource (Array InputSource)
```

Placeholder documentation for ListOfInputSource

##### Instances
``` purescript
Newtype ListOfInputSource _
```

#### `ListOfInputSourceRequest`

``` purescript
newtype ListOfInputSourceRequest
  = ListOfInputSourceRequest (Array InputSourceRequest)
```

Placeholder documentation for ListOfInputSourceRequest

##### Instances
``` purescript
Newtype ListOfInputSourceRequest _
```

#### `ListOfInputWhitelistRule`

``` purescript
newtype ListOfInputWhitelistRule
  = ListOfInputWhitelistRule (Array InputWhitelistRule)
```

Placeholder documentation for ListOfInputWhitelistRule

##### Instances
``` purescript
Newtype ListOfInputWhitelistRule _
```

#### `ListOfInputWhitelistRuleCidr`

``` purescript
newtype ListOfInputWhitelistRuleCidr
  = ListOfInputWhitelistRuleCidr (Array InputWhitelistRuleCidr)
```

Placeholder documentation for ListOfInputWhitelistRuleCidr

##### Instances
``` purescript
Newtype ListOfInputWhitelistRuleCidr _
```

#### `ListOfOutput`

``` purescript
newtype ListOfOutput
  = ListOfOutput (Array Output)
```

Placeholder documentation for ListOfOutput

##### Instances
``` purescript
Newtype ListOfOutput _
```

#### `ListOfOutputDestination`

``` purescript
newtype ListOfOutputDestination
  = ListOfOutputDestination (Array OutputDestination)
```

Placeholder documentation for ListOfOutputDestination

##### Instances
``` purescript
Newtype ListOfOutputDestination _
```

#### `ListOfOutputDestinationSettings`

``` purescript
newtype ListOfOutputDestinationSettings
  = ListOfOutputDestinationSettings (Array OutputDestinationSettings)
```

Placeholder documentation for ListOfOutputDestinationSettings

##### Instances
``` purescript
Newtype ListOfOutputDestinationSettings _
```

#### `ListOfOutputGroup`

``` purescript
newtype ListOfOutputGroup
  = ListOfOutputGroup (Array OutputGroup)
```

Placeholder documentation for ListOfOutputGroup

##### Instances
``` purescript
Newtype ListOfOutputGroup _
```

#### `ListOfValidationError`

``` purescript
newtype ListOfValidationError
  = ListOfValidationError (Array ValidationError)
```

Placeholder documentation for ListOfValidationError

##### Instances
``` purescript
Newtype ListOfValidationError _
```

#### `ListOfVideoDescription`

``` purescript
newtype ListOfVideoDescription
  = ListOfVideoDescription (Array VideoDescription)
```

Placeholder documentation for ListOfVideoDescription

##### Instances
``` purescript
Newtype ListOfVideoDescription _
```

#### `ListOf__string`

``` purescript
newtype ListOf__string
  = ListOf__string (Array String)
```

Placeholder documentation for ListOf__string

##### Instances
``` purescript
Newtype ListOf__string _
```

#### `M2tsAbsentInputAudioBehavior`

``` purescript
newtype M2tsAbsentInputAudioBehavior
  = M2tsAbsentInputAudioBehavior String
```

Placeholder documentation for M2tsAbsentInputAudioBehavior

##### Instances
``` purescript
Newtype M2tsAbsentInputAudioBehavior _
```

#### `M2tsArib`

``` purescript
newtype M2tsArib
  = M2tsArib String
```

Placeholder documentation for M2tsArib

##### Instances
``` purescript
Newtype M2tsArib _
```

#### `M2tsAribCaptionsPidControl`

``` purescript
newtype M2tsAribCaptionsPidControl
  = M2tsAribCaptionsPidControl String
```

Placeholder documentation for M2tsAribCaptionsPidControl

##### Instances
``` purescript
Newtype M2tsAribCaptionsPidControl _
```

#### `M2tsAudioBufferModel`

``` purescript
newtype M2tsAudioBufferModel
  = M2tsAudioBufferModel String
```

Placeholder documentation for M2tsAudioBufferModel

##### Instances
``` purescript
Newtype M2tsAudioBufferModel _
```

#### `M2tsAudioInterval`

``` purescript
newtype M2tsAudioInterval
  = M2tsAudioInterval String
```

Placeholder documentation for M2tsAudioInterval

##### Instances
``` purescript
Newtype M2tsAudioInterval _
```

#### `M2tsAudioStreamType`

``` purescript
newtype M2tsAudioStreamType
  = M2tsAudioStreamType String
```

Placeholder documentation for M2tsAudioStreamType

##### Instances
``` purescript
Newtype M2tsAudioStreamType _
```

#### `M2tsBufferModel`

``` purescript
newtype M2tsBufferModel
  = M2tsBufferModel String
```

Placeholder documentation for M2tsBufferModel

##### Instances
``` purescript
Newtype M2tsBufferModel _
```

#### `M2tsCcDescriptor`

``` purescript
newtype M2tsCcDescriptor
  = M2tsCcDescriptor String
```

Placeholder documentation for M2tsCcDescriptor

##### Instances
``` purescript
Newtype M2tsCcDescriptor _
```

#### `M2tsEbifControl`

``` purescript
newtype M2tsEbifControl
  = M2tsEbifControl String
```

Placeholder documentation for M2tsEbifControl

##### Instances
``` purescript
Newtype M2tsEbifControl _
```

#### `M2tsEbpPlacement`

``` purescript
newtype M2tsEbpPlacement
  = M2tsEbpPlacement String
```

Placeholder documentation for M2tsEbpPlacement

##### Instances
``` purescript
Newtype M2tsEbpPlacement _
```

#### `M2tsEsRateInPes`

``` purescript
newtype M2tsEsRateInPes
  = M2tsEsRateInPes String
```

Placeholder documentation for M2tsEsRateInPes

##### Instances
``` purescript
Newtype M2tsEsRateInPes _
```

#### `M2tsKlv`

``` purescript
newtype M2tsKlv
  = M2tsKlv String
```

Placeholder documentation for M2tsKlv

##### Instances
``` purescript
Newtype M2tsKlv _
```

#### `M2tsPcrControl`

``` purescript
newtype M2tsPcrControl
  = M2tsPcrControl String
```

Placeholder documentation for M2tsPcrControl

##### Instances
``` purescript
Newtype M2tsPcrControl _
```

#### `M2tsRateMode`

``` purescript
newtype M2tsRateMode
  = M2tsRateMode String
```

Placeholder documentation for M2tsRateMode

##### Instances
``` purescript
Newtype M2tsRateMode _
```

#### `M2tsScte35Control`

``` purescript
newtype M2tsScte35Control
  = M2tsScte35Control String
```

Placeholder documentation for M2tsScte35Control

##### Instances
``` purescript
Newtype M2tsScte35Control _
```

#### `M2tsSegmentationMarkers`

``` purescript
newtype M2tsSegmentationMarkers
  = M2tsSegmentationMarkers String
```

Placeholder documentation for M2tsSegmentationMarkers

##### Instances
``` purescript
Newtype M2tsSegmentationMarkers _
```

#### `M2tsSegmentationStyle`

``` purescript
newtype M2tsSegmentationStyle
  = M2tsSegmentationStyle String
```

Placeholder documentation for M2tsSegmentationStyle

##### Instances
``` purescript
Newtype M2tsSegmentationStyle _
```

#### `M2tsSettings`

``` purescript
newtype M2tsSettings
  = M2tsSettings { "AbsentInputAudioBehavior" :: NullOrUndefined (M2tsAbsentInputAudioBehavior), "Arib" :: NullOrUndefined (M2tsArib), "AribCaptionsPid" :: NullOrUndefined (String), "AribCaptionsPidControl" :: NullOrUndefined (M2tsAribCaptionsPidControl), "AudioBufferModel" :: NullOrUndefined (M2tsAudioBufferModel), "AudioFramesPerPes" :: NullOrUndefined (Int), "AudioPids" :: NullOrUndefined (String), "AudioStreamType" :: NullOrUndefined (M2tsAudioStreamType), "Bitrate" :: NullOrUndefined (Int), "BufferModel" :: NullOrUndefined (M2tsBufferModel), "CcDescriptor" :: NullOrUndefined (M2tsCcDescriptor), "DvbNitSettings" :: NullOrUndefined (DvbNitSettings), "DvbSdtSettings" :: NullOrUndefined (DvbSdtSettings), "DvbSubPids" :: NullOrUndefined (String), "DvbTdtSettings" :: NullOrUndefined (DvbTdtSettings), "DvbTeletextPid" :: NullOrUndefined (String), "Ebif" :: NullOrUndefined (M2tsEbifControl), "EbpAudioInterval" :: NullOrUndefined (M2tsAudioInterval), "EbpLookaheadMs" :: NullOrUndefined (Int), "EbpPlacement" :: NullOrUndefined (M2tsEbpPlacement), "EcmPid" :: NullOrUndefined (String), "EsRateInPes" :: NullOrUndefined (M2tsEsRateInPes), "EtvPlatformPid" :: NullOrUndefined (String), "EtvSignalPid" :: NullOrUndefined (String), "FragmentTime" :: NullOrUndefined (Number), "Klv" :: NullOrUndefined (M2tsKlv), "KlvDataPids" :: NullOrUndefined (String), "NullPacketBitrate" :: NullOrUndefined (Number), "PatInterval" :: NullOrUndefined (Int), "PcrControl" :: NullOrUndefined (M2tsPcrControl), "PcrPeriod" :: NullOrUndefined (Int), "PcrPid" :: NullOrUndefined (String), "PmtInterval" :: NullOrUndefined (Int), "PmtPid" :: NullOrUndefined (String), "ProgramNum" :: NullOrUndefined (Int), "RateMode" :: NullOrUndefined (M2tsRateMode), "Scte27Pids" :: NullOrUndefined (String), "Scte35Control" :: NullOrUndefined (M2tsScte35Control), "Scte35Pid" :: NullOrUndefined (String), "SegmentationMarkers" :: NullOrUndefined (M2tsSegmentationMarkers), "SegmentationStyle" :: NullOrUndefined (M2tsSegmentationStyle), "SegmentationTime" :: NullOrUndefined (Number), "TimedMetadataBehavior" :: NullOrUndefined (M2tsTimedMetadataBehavior), "TimedMetadataPid" :: NullOrUndefined (String), "TransportStreamId" :: NullOrUndefined (Int), "VideoPid" :: NullOrUndefined (String) }
```

Placeholder documentation for M2tsSettings

##### Instances
``` purescript
Newtype M2tsSettings _
```

#### `M2tsTimedMetadataBehavior`

``` purescript
newtype M2tsTimedMetadataBehavior
  = M2tsTimedMetadataBehavior String
```

Placeholder documentation for M2tsTimedMetadataBehavior

##### Instances
``` purescript
Newtype M2tsTimedMetadataBehavior _
```

#### `M3u8PcrControl`

``` purescript
newtype M3u8PcrControl
  = M3u8PcrControl String
```

Placeholder documentation for M3u8PcrControl

##### Instances
``` purescript
Newtype M3u8PcrControl _
```

#### `M3u8Scte35Behavior`

``` purescript
newtype M3u8Scte35Behavior
  = M3u8Scte35Behavior String
```

Placeholder documentation for M3u8Scte35Behavior

##### Instances
``` purescript
Newtype M3u8Scte35Behavior _
```

#### `M3u8Settings`

``` purescript
newtype M3u8Settings
  = M3u8Settings { "AudioFramesPerPes" :: NullOrUndefined (Int), "AudioPids" :: NullOrUndefined (String), "EcmPid" :: NullOrUndefined (String), "PatInterval" :: NullOrUndefined (Int), "PcrControl" :: NullOrUndefined (M3u8PcrControl), "PcrPeriod" :: NullOrUndefined (Int), "PcrPid" :: NullOrUndefined (String), "PmtInterval" :: NullOrUndefined (Int), "PmtPid" :: NullOrUndefined (String), "ProgramNum" :: NullOrUndefined (Int), "Scte35Behavior" :: NullOrUndefined (M3u8Scte35Behavior), "Scte35Pid" :: NullOrUndefined (String), "TimedMetadataBehavior" :: NullOrUndefined (M3u8TimedMetadataBehavior), "TransportStreamId" :: NullOrUndefined (Int), "VideoPid" :: NullOrUndefined (String) }
```

Settings information for the .m3u8 container

##### Instances
``` purescript
Newtype M3u8Settings _
```

#### `M3u8TimedMetadataBehavior`

``` purescript
newtype M3u8TimedMetadataBehavior
  = M3u8TimedMetadataBehavior String
```

Placeholder documentation for M3u8TimedMetadataBehavior

##### Instances
``` purescript
Newtype M3u8TimedMetadataBehavior _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

Placeholder documentation for MaxResults

##### Instances
``` purescript
Newtype MaxResults _
```

#### `Mp2CodingMode`

``` purescript
newtype Mp2CodingMode
  = Mp2CodingMode String
```

Placeholder documentation for Mp2CodingMode

##### Instances
``` purescript
Newtype Mp2CodingMode _
```

#### `Mp2Settings`

``` purescript
newtype Mp2Settings
  = Mp2Settings { "Bitrate" :: NullOrUndefined (Number), "CodingMode" :: NullOrUndefined (Mp2CodingMode), "SampleRate" :: NullOrUndefined (Number) }
```

Placeholder documentation for Mp2Settings

##### Instances
``` purescript
Newtype Mp2Settings _
```

#### `MsSmoothGroupSettings`

``` purescript
newtype MsSmoothGroupSettings
  = MsSmoothGroupSettings { "AcquisitionPointId" :: NullOrUndefined (String), "AudioOnlyTimecodeControl" :: NullOrUndefined (SmoothGroupAudioOnlyTimecodeControl), "CertificateMode" :: NullOrUndefined (SmoothGroupCertificateMode), "ConnectionRetryInterval" :: NullOrUndefined (Int), "Destination" :: NullOrUndefined (OutputLocationRef), "EventId" :: NullOrUndefined (String), "EventIdMode" :: NullOrUndefined (SmoothGroupEventIdMode), "EventStopBehavior" :: NullOrUndefined (SmoothGroupEventStopBehavior), "FilecacheDuration" :: NullOrUndefined (Int), "FragmentLength" :: NullOrUndefined (Int), "InputLossAction" :: NullOrUndefined (InputLossActionForMsSmoothOut), "NumRetries" :: NullOrUndefined (Int), "RestartDelay" :: NullOrUndefined (Int), "SegmentationMode" :: NullOrUndefined (SmoothGroupSegmentationMode), "SendDelayMs" :: NullOrUndefined (Int), "SparseTrackType" :: NullOrUndefined (SmoothGroupSparseTrackType), "StreamManifestBehavior" :: NullOrUndefined (SmoothGroupStreamManifestBehavior), "TimestampOffset" :: NullOrUndefined (String), "TimestampOffsetMode" :: NullOrUndefined (SmoothGroupTimestampOffsetMode) }
```

Placeholder documentation for MsSmoothGroupSettings

##### Instances
``` purescript
Newtype MsSmoothGroupSettings _
```

#### `MsSmoothOutputSettings`

``` purescript
newtype MsSmoothOutputSettings
  = MsSmoothOutputSettings { "NameModifier" :: NullOrUndefined (String) }
```

Placeholder documentation for MsSmoothOutputSettings

##### Instances
``` purescript
Newtype MsSmoothOutputSettings _
```

#### `NetworkInputServerValidation`

``` purescript
newtype NetworkInputServerValidation
  = NetworkInputServerValidation String
```

Placeholder documentation for NetworkInputServerValidation

##### Instances
``` purescript
Newtype NetworkInputServerValidation _
```

#### `NetworkInputSettings`

``` purescript
newtype NetworkInputSettings
  = NetworkInputSettings { "HlsInputSettings" :: NullOrUndefined (HlsInputSettings), "ServerValidation" :: NullOrUndefined (NetworkInputServerValidation) }
```

Network source to transcode. Must be accessible to the Elemental Live node that is running the live event through a network connection.

##### Instances
``` purescript
Newtype NetworkInputSettings _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for NotFoundException

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `Output`

``` purescript
newtype Output
  = Output { "AudioDescriptionNames" :: NullOrUndefined (ListOf__string), "CaptionDescriptionNames" :: NullOrUndefined (ListOf__string), "OutputName" :: NullOrUndefined (String), "OutputSettings" :: NullOrUndefined (OutputSettings), "VideoDescriptionName" :: NullOrUndefined (String) }
```

Output settings. There can be multiple outputs within a group.

##### Instances
``` purescript
Newtype Output _
```

#### `OutputDestination`

``` purescript
newtype OutputDestination
  = OutputDestination { "Id" :: NullOrUndefined (String), "Settings" :: NullOrUndefined (ListOfOutputDestinationSettings) }
```

Placeholder documentation for OutputDestination

##### Instances
``` purescript
Newtype OutputDestination _
```

#### `OutputDestinationSettings`

``` purescript
newtype OutputDestinationSettings
  = OutputDestinationSettings { "PasswordParam" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String) }
```

Placeholder documentation for OutputDestinationSettings

##### Instances
``` purescript
Newtype OutputDestinationSettings _
```

#### `OutputGroup`

``` purescript
newtype OutputGroup
  = OutputGroup { "Name" :: NullOrUndefined (String), "OutputGroupSettings" :: NullOrUndefined (OutputGroupSettings), "Outputs" :: NullOrUndefined (ListOfOutput) }
```

Output groups for this Live Event. Output groups contain information about where streams should be distributed.

##### Instances
``` purescript
Newtype OutputGroup _
```

#### `OutputGroupSettings`

``` purescript
newtype OutputGroupSettings
  = OutputGroupSettings { "ArchiveGroupSettings" :: NullOrUndefined (ArchiveGroupSettings), "HlsGroupSettings" :: NullOrUndefined (HlsGroupSettings), "MsSmoothGroupSettings" :: NullOrUndefined (MsSmoothGroupSettings), "UdpGroupSettings" :: NullOrUndefined (UdpGroupSettings) }
```

Placeholder documentation for OutputGroupSettings

##### Instances
``` purescript
Newtype OutputGroupSettings _
```

#### `OutputLocationRef`

``` purescript
newtype OutputLocationRef
  = OutputLocationRef { "DestinationRefId" :: NullOrUndefined (String) }
```

Reference to an OutputDestination ID defined in the channel

##### Instances
``` purescript
Newtype OutputLocationRef _
```

#### `OutputSettings`

``` purescript
newtype OutputSettings
  = OutputSettings { "ArchiveOutputSettings" :: NullOrUndefined (ArchiveOutputSettings), "HlsOutputSettings" :: NullOrUndefined (HlsOutputSettings), "MsSmoothOutputSettings" :: NullOrUndefined (MsSmoothOutputSettings), "UdpOutputSettings" :: NullOrUndefined (UdpOutputSettings) }
```

Placeholder documentation for OutputSettings

##### Instances
``` purescript
Newtype OutputSettings _
```

#### `PassThroughSettings`

``` purescript
newtype PassThroughSettings
  = PassThroughSettings {  }
```

Placeholder documentation for PassThroughSettings

##### Instances
``` purescript
Newtype PassThroughSettings _
```

#### `RemixSettings`

``` purescript
newtype RemixSettings
  = RemixSettings { "ChannelMappings" :: NullOrUndefined (ListOfAudioChannelMapping), "ChannelsIn" :: NullOrUndefined (Int), "ChannelsOut" :: NullOrUndefined (Int) }
```

Placeholder documentation for RemixSettings

##### Instances
``` purescript
Newtype RemixSettings _
```

#### `ResourceConflict`

``` purescript
newtype ResourceConflict
  = ResourceConflict { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for ResourceConflict

##### Instances
``` purescript
Newtype ResourceConflict _
```

#### `ResourceNotFound`

``` purescript
newtype ResourceNotFound
  = ResourceNotFound { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for ResourceNotFound

##### Instances
``` purescript
Newtype ResourceNotFound _
```

#### `Scte20Convert608To708`

``` purescript
newtype Scte20Convert608To708
  = Scte20Convert608To708 String
```

Placeholder documentation for Scte20Convert608To708

##### Instances
``` purescript
Newtype Scte20Convert608To708 _
```

#### `Scte20PlusEmbeddedDestinationSettings`

``` purescript
newtype Scte20PlusEmbeddedDestinationSettings
  = Scte20PlusEmbeddedDestinationSettings {  }
```

Placeholder documentation for Scte20PlusEmbeddedDestinationSettings

##### Instances
``` purescript
Newtype Scte20PlusEmbeddedDestinationSettings _
```

#### `Scte20SourceSettings`

``` purescript
newtype Scte20SourceSettings
  = Scte20SourceSettings { "Convert608To708" :: NullOrUndefined (Scte20Convert608To708), "Source608ChannelNumber" :: NullOrUndefined (Int) }
```

Placeholder documentation for Scte20SourceSettings

##### Instances
``` purescript
Newtype Scte20SourceSettings _
```

#### `Scte27DestinationSettings`

``` purescript
newtype Scte27DestinationSettings
  = Scte27DestinationSettings {  }
```

Placeholder documentation for Scte27DestinationSettings

##### Instances
``` purescript
Newtype Scte27DestinationSettings _
```

#### `Scte27SourceSettings`

``` purescript
newtype Scte27SourceSettings
  = Scte27SourceSettings { "Pid" :: NullOrUndefined (Int) }
```

Placeholder documentation for Scte27SourceSettings

##### Instances
``` purescript
Newtype Scte27SourceSettings _
```

#### `Scte35AposNoRegionalBlackoutBehavior`

``` purescript
newtype Scte35AposNoRegionalBlackoutBehavior
  = Scte35AposNoRegionalBlackoutBehavior String
```

Placeholder documentation for Scte35AposNoRegionalBlackoutBehavior

##### Instances
``` purescript
Newtype Scte35AposNoRegionalBlackoutBehavior _
```

#### `Scte35AposWebDeliveryAllowedBehavior`

``` purescript
newtype Scte35AposWebDeliveryAllowedBehavior
  = Scte35AposWebDeliveryAllowedBehavior String
```

Placeholder documentation for Scte35AposWebDeliveryAllowedBehavior

##### Instances
``` purescript
Newtype Scte35AposWebDeliveryAllowedBehavior _
```

#### `Scte35SpliceInsert`

``` purescript
newtype Scte35SpliceInsert
  = Scte35SpliceInsert { "AdAvailOffset" :: NullOrUndefined (Int), "NoRegionalBlackoutFlag" :: NullOrUndefined (Scte35SpliceInsertNoRegionalBlackoutBehavior), "WebDeliveryAllowedFlag" :: NullOrUndefined (Scte35SpliceInsertWebDeliveryAllowedBehavior) }
```

Placeholder documentation for Scte35SpliceInsert

##### Instances
``` purescript
Newtype Scte35SpliceInsert _
```

#### `Scte35SpliceInsertNoRegionalBlackoutBehavior`

``` purescript
newtype Scte35SpliceInsertNoRegionalBlackoutBehavior
  = Scte35SpliceInsertNoRegionalBlackoutBehavior String
```

Placeholder documentation for Scte35SpliceInsertNoRegionalBlackoutBehavior

##### Instances
``` purescript
Newtype Scte35SpliceInsertNoRegionalBlackoutBehavior _
```

#### `Scte35SpliceInsertWebDeliveryAllowedBehavior`

``` purescript
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior
  = Scte35SpliceInsertWebDeliveryAllowedBehavior String
```

Placeholder documentation for Scte35SpliceInsertWebDeliveryAllowedBehavior

##### Instances
``` purescript
Newtype Scte35SpliceInsertWebDeliveryAllowedBehavior _
```

#### `Scte35TimeSignalApos`

``` purescript
newtype Scte35TimeSignalApos
  = Scte35TimeSignalApos { "AdAvailOffset" :: NullOrUndefined (Int), "NoRegionalBlackoutFlag" :: NullOrUndefined (Scte35AposNoRegionalBlackoutBehavior), "WebDeliveryAllowedFlag" :: NullOrUndefined (Scte35AposWebDeliveryAllowedBehavior) }
```

Placeholder documentation for Scte35TimeSignalApos

##### Instances
``` purescript
Newtype Scte35TimeSignalApos _
```

#### `SmoothGroupAudioOnlyTimecodeControl`

``` purescript
newtype SmoothGroupAudioOnlyTimecodeControl
  = SmoothGroupAudioOnlyTimecodeControl String
```

Placeholder documentation for SmoothGroupAudioOnlyTimecodeControl

##### Instances
``` purescript
Newtype SmoothGroupAudioOnlyTimecodeControl _
```

#### `SmoothGroupCertificateMode`

``` purescript
newtype SmoothGroupCertificateMode
  = SmoothGroupCertificateMode String
```

Placeholder documentation for SmoothGroupCertificateMode

##### Instances
``` purescript
Newtype SmoothGroupCertificateMode _
```

#### `SmoothGroupEventIdMode`

``` purescript
newtype SmoothGroupEventIdMode
  = SmoothGroupEventIdMode String
```

Placeholder documentation for SmoothGroupEventIdMode

##### Instances
``` purescript
Newtype SmoothGroupEventIdMode _
```

#### `SmoothGroupEventStopBehavior`

``` purescript
newtype SmoothGroupEventStopBehavior
  = SmoothGroupEventStopBehavior String
```

Placeholder documentation for SmoothGroupEventStopBehavior

##### Instances
``` purescript
Newtype SmoothGroupEventStopBehavior _
```

#### `SmoothGroupSegmentationMode`

``` purescript
newtype SmoothGroupSegmentationMode
  = SmoothGroupSegmentationMode String
```

Placeholder documentation for SmoothGroupSegmentationMode

##### Instances
``` purescript
Newtype SmoothGroupSegmentationMode _
```

#### `SmoothGroupSparseTrackType`

``` purescript
newtype SmoothGroupSparseTrackType
  = SmoothGroupSparseTrackType String
```

Placeholder documentation for SmoothGroupSparseTrackType

##### Instances
``` purescript
Newtype SmoothGroupSparseTrackType _
```

#### `SmoothGroupStreamManifestBehavior`

``` purescript
newtype SmoothGroupStreamManifestBehavior
  = SmoothGroupStreamManifestBehavior String
```

Placeholder documentation for SmoothGroupStreamManifestBehavior

##### Instances
``` purescript
Newtype SmoothGroupStreamManifestBehavior _
```

#### `SmoothGroupTimestampOffsetMode`

``` purescript
newtype SmoothGroupTimestampOffsetMode
  = SmoothGroupTimestampOffsetMode String
```

Placeholder documentation for SmoothGroupTimestampOffsetMode

##### Instances
``` purescript
Newtype SmoothGroupTimestampOffsetMode _
```

#### `SmpteTtDestinationSettings`

``` purescript
newtype SmpteTtDestinationSettings
  = SmpteTtDestinationSettings {  }
```

Placeholder documentation for SmpteTtDestinationSettings

##### Instances
``` purescript
Newtype SmpteTtDestinationSettings _
```

#### `StandardHlsSettings`

``` purescript
newtype StandardHlsSettings
  = StandardHlsSettings { "AudioRenditionSets" :: NullOrUndefined (String), "M3u8Settings" :: NullOrUndefined (M3u8Settings) }
```

Placeholder documentation for StandardHlsSettings

##### Instances
``` purescript
Newtype StandardHlsSettings _
```

#### `StartChannelRequest`

``` purescript
newtype StartChannelRequest
  = StartChannelRequest { "ChannelId" :: String }
```

Placeholder documentation for StartChannelRequest

##### Instances
``` purescript
Newtype StartChannelRequest _
```

#### `StartChannelResponse`

``` purescript
newtype StartChannelResponse
  = StartChannelResponse { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for StartChannelResponse

##### Instances
``` purescript
Newtype StartChannelResponse _
```

#### `StaticKeySettings`

``` purescript
newtype StaticKeySettings
  = StaticKeySettings { "KeyProviderServer" :: NullOrUndefined (InputLocation), "StaticKeyValue" :: NullOrUndefined (String) }
```

Placeholder documentation for StaticKeySettings

##### Instances
``` purescript
Newtype StaticKeySettings _
```

#### `StopChannelRequest`

``` purescript
newtype StopChannelRequest
  = StopChannelRequest { "ChannelId" :: String }
```

Placeholder documentation for StopChannelRequest

##### Instances
``` purescript
Newtype StopChannelRequest _
```

#### `StopChannelResponse`

``` purescript
newtype StopChannelResponse
  = StopChannelResponse { "Arn" :: NullOrUndefined (String), "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EgressEndpoints" :: NullOrUndefined (ListOfChannelEgressEndpoint), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "Id" :: NullOrUndefined (String), "InputAttachments" :: NullOrUndefined (ListOfInputAttachment), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "PipelinesRunningCount" :: NullOrUndefined (Int), "RoleArn" :: NullOrUndefined (String), "State" :: NullOrUndefined (ChannelState) }
```

Placeholder documentation for StopChannelResponse

##### Instances
``` purescript
Newtype StopChannelResponse _
```

#### `TeletextDestinationSettings`

``` purescript
newtype TeletextDestinationSettings
  = TeletextDestinationSettings {  }
```

Placeholder documentation for TeletextDestinationSettings

##### Instances
``` purescript
Newtype TeletextDestinationSettings _
```

#### `TeletextSourceSettings`

``` purescript
newtype TeletextSourceSettings
  = TeletextSourceSettings { "PageNumber" :: NullOrUndefined (String) }
```

Placeholder documentation for TeletextSourceSettings

##### Instances
``` purescript
Newtype TeletextSourceSettings _
```

#### `TimecodeConfig`

``` purescript
newtype TimecodeConfig
  = TimecodeConfig { "Source" :: NullOrUndefined (TimecodeConfigSource), "SyncThreshold" :: NullOrUndefined (Int) }
```

Placeholder documentation for TimecodeConfig

##### Instances
``` purescript
Newtype TimecodeConfig _
```

#### `TimecodeConfigSource`

``` purescript
newtype TimecodeConfigSource
  = TimecodeConfigSource String
```

Placeholder documentation for TimecodeConfigSource

##### Instances
``` purescript
Newtype TimecodeConfigSource _
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String) }
```

Placeholder documentation for TooManyRequestsException

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `TtmlDestinationSettings`

``` purescript
newtype TtmlDestinationSettings
  = TtmlDestinationSettings { "StyleControl" :: NullOrUndefined (TtmlDestinationStyleControl) }
```

Placeholder documentation for TtmlDestinationSettings

##### Instances
``` purescript
Newtype TtmlDestinationSettings _
```

#### `TtmlDestinationStyleControl`

``` purescript
newtype TtmlDestinationStyleControl
  = TtmlDestinationStyleControl String
```

Placeholder documentation for TtmlDestinationStyleControl

##### Instances
``` purescript
Newtype TtmlDestinationStyleControl _
```

#### `UdpContainerSettings`

``` purescript
newtype UdpContainerSettings
  = UdpContainerSettings { "M2tsSettings" :: NullOrUndefined (M2tsSettings) }
```

Placeholder documentation for UdpContainerSettings

##### Instances
``` purescript
Newtype UdpContainerSettings _
```

#### `UdpGroupSettings`

``` purescript
newtype UdpGroupSettings
  = UdpGroupSettings { "InputLossAction" :: NullOrUndefined (InputLossActionForUdpOut), "TimedMetadataId3Frame" :: NullOrUndefined (UdpTimedMetadataId3Frame), "TimedMetadataId3Period" :: NullOrUndefined (Int) }
```

Placeholder documentation for UdpGroupSettings

##### Instances
``` purescript
Newtype UdpGroupSettings _
```

#### `UdpOutputSettings`

``` purescript
newtype UdpOutputSettings
  = UdpOutputSettings { "BufferMsec" :: NullOrUndefined (Int), "ContainerSettings" :: NullOrUndefined (UdpContainerSettings), "Destination" :: NullOrUndefined (OutputLocationRef), "FecOutputSettings" :: NullOrUndefined (FecOutputSettings) }
```

Placeholder documentation for UdpOutputSettings

##### Instances
``` purescript
Newtype UdpOutputSettings _
```

#### `UdpTimedMetadataId3Frame`

``` purescript
newtype UdpTimedMetadataId3Frame
  = UdpTimedMetadataId3Frame String
```

Placeholder documentation for UdpTimedMetadataId3Frame

##### Instances
``` purescript
Newtype UdpTimedMetadataId3Frame _
```

#### `UnprocessableEntityException`

``` purescript
newtype UnprocessableEntityException
  = UnprocessableEntityException { "Message" :: NullOrUndefined (String), "ValidationErrors" :: NullOrUndefined (ListOfValidationError) }
```

Placeholder documentation for UnprocessableEntityException

##### Instances
``` purescript
Newtype UnprocessableEntityException _
```

#### `UpdateChannel`

``` purescript
newtype UpdateChannel
  = UpdateChannel { "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Placeholder documentation for UpdateChannel

##### Instances
``` purescript
Newtype UpdateChannel _
```

#### `UpdateChannelRequest`

``` purescript
newtype UpdateChannelRequest
  = UpdateChannelRequest { "ChannelId" :: String, "Destinations" :: NullOrUndefined (ListOfOutputDestination), "EncoderSettings" :: NullOrUndefined (EncoderSettings), "InputSpecification" :: NullOrUndefined (InputSpecification), "Name" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

A request to update a channel.

##### Instances
``` purescript
Newtype UpdateChannelRequest _
```

#### `UpdateChannelResponse`

``` purescript
newtype UpdateChannelResponse
  = UpdateChannelResponse { "Channel" :: NullOrUndefined (Channel) }
```

Placeholder documentation for UpdateChannelResponse

##### Instances
``` purescript
Newtype UpdateChannelResponse _
```

#### `UpdateChannelResultModel`

``` purescript
newtype UpdateChannelResultModel
  = UpdateChannelResultModel { "Channel" :: NullOrUndefined (Channel) }
```

The updated channel's description.

##### Instances
``` purescript
Newtype UpdateChannelResultModel _
```

#### `ValidationError`

``` purescript
newtype ValidationError
  = ValidationError { "ElementPath" :: NullOrUndefined (String), "ErrorMessage" :: NullOrUndefined (String) }
```

Placeholder documentation for ValidationError

##### Instances
``` purescript
Newtype ValidationError _
```

#### `VideoCodecSettings`

``` purescript
newtype VideoCodecSettings
  = VideoCodecSettings { "H264Settings" :: NullOrUndefined (H264Settings) }
```

Placeholder documentation for VideoCodecSettings

##### Instances
``` purescript
Newtype VideoCodecSettings _
```

#### `VideoDescription`

``` purescript
newtype VideoDescription
  = VideoDescription { "CodecSettings" :: NullOrUndefined (VideoCodecSettings), "Height" :: NullOrUndefined (Int), "Name" :: NullOrUndefined (String), "RespondToAfd" :: NullOrUndefined (VideoDescriptionRespondToAfd), "ScalingBehavior" :: NullOrUndefined (VideoDescriptionScalingBehavior), "Sharpness" :: NullOrUndefined (Int), "Width" :: NullOrUndefined (Int) }
```

Video settings for this stream.

##### Instances
``` purescript
Newtype VideoDescription _
```

#### `VideoDescriptionRespondToAfd`

``` purescript
newtype VideoDescriptionRespondToAfd
  = VideoDescriptionRespondToAfd String
```

Placeholder documentation for VideoDescriptionRespondToAfd

##### Instances
``` purescript
Newtype VideoDescriptionRespondToAfd _
```

#### `VideoDescriptionScalingBehavior`

``` purescript
newtype VideoDescriptionScalingBehavior
  = VideoDescriptionScalingBehavior String
```

Placeholder documentation for VideoDescriptionScalingBehavior

##### Instances
``` purescript
Newtype VideoDescriptionScalingBehavior _
```

#### `VideoSelector`

``` purescript
newtype VideoSelector
  = VideoSelector { "ColorSpace" :: NullOrUndefined (VideoSelectorColorSpace), "ColorSpaceUsage" :: NullOrUndefined (VideoSelectorColorSpaceUsage), "SelectorSettings" :: NullOrUndefined (VideoSelectorSettings) }
```

Specifies a particular video stream within an input source. An input may have only a single video selector.

##### Instances
``` purescript
Newtype VideoSelector _
```

#### `VideoSelectorColorSpace`

``` purescript
newtype VideoSelectorColorSpace
  = VideoSelectorColorSpace String
```

Placeholder documentation for VideoSelectorColorSpace

##### Instances
``` purescript
Newtype VideoSelectorColorSpace _
```

#### `VideoSelectorColorSpaceUsage`

``` purescript
newtype VideoSelectorColorSpaceUsage
  = VideoSelectorColorSpaceUsage String
```

Placeholder documentation for VideoSelectorColorSpaceUsage

##### Instances
``` purescript
Newtype VideoSelectorColorSpaceUsage _
```

#### `VideoSelectorPid`

``` purescript
newtype VideoSelectorPid
  = VideoSelectorPid { "Pid" :: NullOrUndefined (Int) }
```

Placeholder documentation for VideoSelectorPid

##### Instances
``` purescript
Newtype VideoSelectorPid _
```

#### `VideoSelectorProgramId`

``` purescript
newtype VideoSelectorProgramId
  = VideoSelectorProgramId { "ProgramId" :: NullOrUndefined (Int) }
```

Placeholder documentation for VideoSelectorProgramId

##### Instances
``` purescript
Newtype VideoSelectorProgramId _
```

#### `VideoSelectorSettings`

``` purescript
newtype VideoSelectorSettings
  = VideoSelectorSettings { "VideoSelectorPid" :: NullOrUndefined (VideoSelectorPid), "VideoSelectorProgramId" :: NullOrUndefined (VideoSelectorProgramId) }
```

Placeholder documentation for VideoSelectorSettings

##### Instances
``` purescript
Newtype VideoSelectorSettings _
```

#### `WebvttDestinationSettings`

``` purescript
newtype WebvttDestinationSettings
  = WebvttDestinationSettings {  }
```

Placeholder documentation for WebvttDestinationSettings

##### Instances
``` purescript
Newtype WebvttDestinationSettings _
```


