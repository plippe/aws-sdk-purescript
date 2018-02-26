## Module AWS.MediaConvert

AWS Elemental MediaConvert

#### `serviceName`

``` purescript
serviceName :: String
```

#### `cancelJob`

``` purescript
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: RequestError | eff) CancelJobResponse
```

Permanently remove a job from a queue. Once you have canceled a job, you can't start it again. You can't delete a running job.

#### `createJob`

``` purescript
createJob :: forall eff. CreateJobRequest -> Aff (err :: RequestError | eff) CreateJobResponse
```

Create a new transcoding job. For information about jobs and job settings, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html

#### `createJobTemplate`

``` purescript
createJobTemplate :: forall eff. CreateJobTemplateRequest -> Aff (err :: RequestError | eff) CreateJobTemplateResponse
```

Create a new job template. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html

#### `createPreset`

``` purescript
createPreset :: forall eff. CreatePresetRequest -> Aff (err :: RequestError | eff) CreatePresetResponse
```

Create a new preset. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html

#### `createQueue`

``` purescript
createQueue :: forall eff. CreateQueueRequest -> Aff (err :: RequestError | eff) CreateQueueResponse
```

Create a new transcoding queue. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html

#### `deleteJobTemplate`

``` purescript
deleteJobTemplate :: forall eff. DeleteJobTemplateRequest -> Aff (err :: RequestError | eff) DeleteJobTemplateResponse
```

Permanently delete a job template you have created.

#### `deletePreset`

``` purescript
deletePreset :: forall eff. DeletePresetRequest -> Aff (err :: RequestError | eff) DeletePresetResponse
```

Permanently delete a preset you have created.

#### `deleteQueue`

``` purescript
deleteQueue :: forall eff. DeleteQueueRequest -> Aff (err :: RequestError | eff) DeleteQueueResponse
```

Permanently delete a queue you have created.

#### `describeEndpoints`

``` purescript
describeEndpoints :: forall eff. DescribeEndpointsRequest -> Aff (err :: RequestError | eff) DescribeEndpointsResponse
```

Send an request with an empty body to the regional API endpoint to get your account API endpoint.

#### `getJob`

``` purescript
getJob :: forall eff. GetJobRequest -> Aff (err :: RequestError | eff) GetJobResponse
```

Retrieve the JSON for a specific completed transcoding job.

#### `getJobTemplate`

``` purescript
getJobTemplate :: forall eff. GetJobTemplateRequest -> Aff (err :: RequestError | eff) GetJobTemplateResponse
```

Retrieve the JSON for a specific job template.

#### `getPreset`

``` purescript
getPreset :: forall eff. GetPresetRequest -> Aff (err :: RequestError | eff) GetPresetResponse
```

Retrieve the JSON for a specific preset.

#### `getQueue`

``` purescript
getQueue :: forall eff. GetQueueRequest -> Aff (err :: RequestError | eff) GetQueueResponse
```

Retrieve the JSON for a specific queue.

#### `listJobTemplates`

``` purescript
listJobTemplates :: forall eff. ListJobTemplatesRequest -> Aff (err :: RequestError | eff) ListJobTemplatesResponse
```

Retrieve a JSON array of up to twenty of your job templates. This will return the templates themselves, not just a list of them. To retrieve the next twenty templates, use the nextToken string returned with the array

#### `listJobs`

``` purescript
listJobs :: forall eff. ListJobsRequest -> Aff (err :: RequestError | eff) ListJobsResponse
```

Retrieve a JSON array of up to twenty of your most recently created jobs. This array includes in-process, completed, and errored jobs. This will return the jobs themselves, not just a list of the jobs. To retrieve the twenty next most recent jobs, use the nextToken string returned with the array.

#### `listPresets`

``` purescript
listPresets :: forall eff. ListPresetsRequest -> Aff (err :: RequestError | eff) ListPresetsResponse
```

Retrieve a JSON array of up to twenty of your presets. This will return the presets themselves, not just a list of them. To retrieve the next twenty presets, use the nextToken string returned with the array.

#### `listQueues`

``` purescript
listQueues :: forall eff. ListQueuesRequest -> Aff (err :: RequestError | eff) ListQueuesResponse
```

Retrieve a JSON array of up to twenty of your queues. This will return the queues themselves, not just a list of them. To retrieve the next twenty queues, use the nextToken string returned with the array.

#### `updateJobTemplate`

``` purescript
updateJobTemplate :: forall eff. UpdateJobTemplateRequest -> Aff (err :: RequestError | eff) UpdateJobTemplateResponse
```

Modify one of your existing job templates.

#### `updatePreset`

``` purescript
updatePreset :: forall eff. UpdatePresetRequest -> Aff (err :: RequestError | eff) UpdatePresetResponse
```

Modify one of your existing presets.

#### `updateQueue`

``` purescript
updateQueue :: forall eff. UpdateQueueRequest -> Aff (err :: RequestError | eff) UpdateQueueResponse
```

Modify one of your existing queues.

#### `AacAudioDescriptionBroadcasterMix`

``` purescript
newtype AacAudioDescriptionBroadcasterMix
  = AacAudioDescriptionBroadcasterMix String
```

Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.

#### `AacCodecProfile`

``` purescript
newtype AacCodecProfile
  = AacCodecProfile String
```

AAC Profile.

#### `AacCodingMode`

``` purescript
newtype AacCodingMode
  = AacCodingMode String
```

Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.

#### `AacRateControlMode`

``` purescript
newtype AacRateControlMode
  = AacRateControlMode String
```

Rate Control Mode.

#### `AacRawFormat`

``` purescript
newtype AacRawFormat
  = AacRawFormat String
```

Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.

#### `AacSettings`

``` purescript
newtype AacSettings
  = AacSettings { "AudioDescriptionBroadcasterMix" :: NullOrUndefined (AacAudioDescriptionBroadcasterMix), "Bitrate" :: NullOrUndefined (Int), "CodecProfile" :: NullOrUndefined (AacCodecProfile), "CodingMode" :: NullOrUndefined (AacCodingMode), "RateControlMode" :: NullOrUndefined (AacRateControlMode), "RawFormat" :: NullOrUndefined (AacRawFormat), "SampleRate" :: NullOrUndefined (Int), "Specification" :: NullOrUndefined (AacSpecification), "VbrQuality" :: NullOrUndefined (AacVbrQuality) }
```

Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC.

#### `AacSpecification`

``` purescript
newtype AacSpecification
  = AacSpecification String
```

Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.

#### `AacVbrQuality`

``` purescript
newtype AacVbrQuality
  = AacVbrQuality String
```

VBR Quality Level - Only used if rate_control_mode is VBR.

#### `Ac3BitstreamMode`

``` purescript
newtype Ac3BitstreamMode
  = Ac3BitstreamMode String
```

Specifies the "Bitstream Mode" (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.

#### `Ac3CodingMode`

``` purescript
newtype Ac3CodingMode
  = Ac3CodingMode String
```

Dolby Digital coding mode. Determines number of channels.

#### `Ac3DynamicRangeCompressionProfile`

``` purescript
newtype Ac3DynamicRangeCompressionProfile
  = Ac3DynamicRangeCompressionProfile String
```

If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.

#### `Ac3LfeFilter`

``` purescript
newtype Ac3LfeFilter
  = Ac3LfeFilter String
```

Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.

#### `Ac3MetadataControl`

``` purescript
newtype Ac3MetadataControl
  = Ac3MetadataControl String
```

When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.

#### `Ac3Settings`

``` purescript
newtype Ac3Settings
  = Ac3Settings { "Bitrate" :: NullOrUndefined (Int), "BitstreamMode" :: NullOrUndefined (Ac3BitstreamMode), "CodingMode" :: NullOrUndefined (Ac3CodingMode), "Dialnorm" :: NullOrUndefined (Int), "DynamicRangeCompressionProfile" :: NullOrUndefined (Ac3DynamicRangeCompressionProfile), "LfeFilter" :: NullOrUndefined (Ac3LfeFilter), "MetadataControl" :: NullOrUndefined (Ac3MetadataControl), "SampleRate" :: NullOrUndefined (Int) }
```

Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.

#### `AfdSignaling`

``` purescript
newtype AfdSignaling
  = AfdSignaling String
```

This setting only applies to H.264 and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to whether there are AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.

#### `AiffSettings`

``` purescript
newtype AiffSettings
  = AiffSettings { "BitDepth" :: NullOrUndefined (Int), "Channels" :: NullOrUndefined (Int), "SampleRate" :: NullOrUndefined (Int) }
```

Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.

#### `AncillarySourceSettings`

``` purescript
newtype AncillarySourceSettings
  = AncillarySourceSettings { "SourceAncillaryChannelNumber" :: NullOrUndefined (Int) }
```

Settings for ancillary captions source.

#### `AntiAlias`

``` purescript
newtype AntiAlias
  = AntiAlias String
```

Enable Anti-alias (AntiAlias) to enhance sharp edges in video output when your input resolution is much larger than your output resolution. Default is enabled.

#### `AudioCodec`

``` purescript
newtype AudioCodec
  = AudioCodec String
```

Type of Audio codec.

#### `AudioCodecSettings`

``` purescript
newtype AudioCodecSettings
  = AudioCodecSettings { "AacSettings" :: NullOrUndefined (AacSettings), "Ac3Settings" :: NullOrUndefined (Ac3Settings), "AiffSettings" :: NullOrUndefined (AiffSettings), "Codec" :: NullOrUndefined (AudioCodec), "Eac3Settings" :: NullOrUndefined (Eac3Settings), "Mp2Settings" :: NullOrUndefined (Mp2Settings), "WavSettings" :: NullOrUndefined (WavSettings) }
```

Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value you choose for Audio codec (Codec). For each codec enum you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings

#### `AudioDefaultSelection`

``` purescript
newtype AudioDefaultSelection
  = AudioDefaultSelection String
```

When an "Audio Description":#audio_description specifies an AudioSelector or AudioSelectorGroup  for which no matching source is found in the input, then the audio selector marked as DEFAULT will be used.  If none are marked as default, silence will be inserted for the duration of the input.

#### `AudioDescription`

``` purescript
newtype AudioDescription
  = AudioDescription { "AudioNormalizationSettings" :: NullOrUndefined (AudioNormalizationSettings), "AudioSourceName" :: NullOrUndefined (String), "AudioType" :: NullOrUndefined (Int), "AudioTypeControl" :: NullOrUndefined (AudioTypeControl), "CodecSettings" :: NullOrUndefined (AudioCodecSettings), "LanguageCode" :: NullOrUndefined (LanguageCode), "LanguageCodeControl" :: NullOrUndefined (AudioLanguageCodeControl), "RemixSettings" :: NullOrUndefined (RemixSettings), "StreamName" :: NullOrUndefined (String) }
```

Description of audio output

#### `AudioLanguageCodeControl`

``` purescript
newtype AudioLanguageCodeControl
  = AudioLanguageCodeControl String
```

Choosing FOLLOW_INPUT will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The language specified for languageCode' will be used when USE_CONFIGURED is selected or when FOLLOW_INPUT is selected but there is no ISO 639 language code specified by the input.

#### `AudioNormalizationAlgorithm`

``` purescript
newtype AudioNormalizationAlgorithm
  = AudioNormalizationAlgorithm String
```

Audio normalization algorithm to use. 1770-1 conforms to the CALM Act specification, 1770-2 conforms to the EBU R-128 specification.

#### `AudioNormalizationAlgorithmControl`

``` purescript
newtype AudioNormalizationAlgorithmControl
  = AudioNormalizationAlgorithmControl String
```

When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.

#### `AudioNormalizationLoudnessLogging`

``` purescript
newtype AudioNormalizationLoudnessLogging
  = AudioNormalizationLoudnessLogging String
```

If set to LOG, log each output's audio track loudness to a CSV file.

#### `AudioNormalizationPeakCalculation`

``` purescript
newtype AudioNormalizationPeakCalculation
  = AudioNormalizationPeakCalculation String
```

If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.

#### `AudioNormalizationSettings`

``` purescript
newtype AudioNormalizationSettings
  = AudioNormalizationSettings { "Algorithm" :: NullOrUndefined (AudioNormalizationAlgorithm), "AlgorithmControl" :: NullOrUndefined (AudioNormalizationAlgorithmControl), "CorrectionGateLevel" :: NullOrUndefined (Int), "LoudnessLogging" :: NullOrUndefined (AudioNormalizationLoudnessLogging), "PeakCalculation" :: NullOrUndefined (AudioNormalizationPeakCalculation), "TargetLkfs" :: NullOrUndefined (Number) }
```

Advanced audio normalization settings.

#### `AudioSelector`

``` purescript
newtype AudioSelector
  = AudioSelector { "DefaultSelection" :: NullOrUndefined (AudioDefaultSelection), "ExternalAudioFileInput" :: NullOrUndefined (String), "LanguageCode" :: NullOrUndefined (LanguageCode), "Offset" :: NullOrUndefined (Int), "Pids" :: NullOrUndefined (ListOf__integer), "ProgramSelection" :: NullOrUndefined (Int), "RemixSettings" :: NullOrUndefined (RemixSettings), "SelectorType" :: NullOrUndefined (AudioSelectorType), "Tracks" :: NullOrUndefined (ListOf__integer) }
```

Selector for Audio

#### `AudioSelectorGroup`

``` purescript
newtype AudioSelectorGroup
  = AudioSelectorGroup { "AudioSelectorNames" :: NullOrUndefined (ListOf__string) }
```

Group of Audio Selectors

#### `AudioSelectorType`

``` purescript
newtype AudioSelectorType
  = AudioSelectorType String
```

Specifies the type of the audio selector.

#### `AudioTypeControl`

``` purescript
newtype AudioTypeControl
  = AudioTypeControl String
```

When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.

#### `AvailBlanking`

``` purescript
newtype AvailBlanking
  = AvailBlanking { "AvailBlankingImage" :: NullOrUndefined (String) }
```

Settings for Avail Blanking

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message" :: NullOrUndefined (String) }
```

The service can't process your request because of a problem in the request. Please check your request form and syntax.

#### `BurninDestinationSettings`

``` purescript
newtype BurninDestinationSettings
  = BurninDestinationSettings { "Alignment" :: NullOrUndefined (BurninSubtitleAlignment), "BackgroundColor" :: NullOrUndefined (BurninSubtitleBackgroundColor), "BackgroundOpacity" :: NullOrUndefined (Int), "FontColor" :: NullOrUndefined (BurninSubtitleFontColor), "FontOpacity" :: NullOrUndefined (Int), "FontResolution" :: NullOrUndefined (Int), "FontSize" :: NullOrUndefined (Int), "OutlineColor" :: NullOrUndefined (BurninSubtitleOutlineColor), "OutlineSize" :: NullOrUndefined (Int), "ShadowColor" :: NullOrUndefined (BurninSubtitleShadowColor), "ShadowOpacity" :: NullOrUndefined (Int), "ShadowXOffset" :: NullOrUndefined (Int), "ShadowYOffset" :: NullOrUndefined (Int), "TeletextSpacing" :: NullOrUndefined (BurninSubtitleTeletextSpacing), "XPosition" :: NullOrUndefined (Int), "YPosition" :: NullOrUndefined (Int) }
```

Burn-In Destination Settings.

#### `BurninSubtitleAlignment`

``` purescript
newtype BurninSubtitleAlignment
  = BurninSubtitleAlignment String
```

If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.

#### `BurninSubtitleBackgroundColor`

``` purescript
newtype BurninSubtitleBackgroundColor
  = BurninSubtitleBackgroundColor String
```

Specifies the color of the rectangle behind the captions.
All burn-in and DVB-Sub font settings must match.

#### `BurninSubtitleFontColor`

``` purescript
newtype BurninSubtitleFontColor
  = BurninSubtitleFontColor String
```

Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.

#### `BurninSubtitleOutlineColor`

``` purescript
newtype BurninSubtitleOutlineColor
  = BurninSubtitleOutlineColor String
```

Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.

#### `BurninSubtitleShadowColor`

``` purescript
newtype BurninSubtitleShadowColor
  = BurninSubtitleShadowColor String
```

Specifies the color of the shadow cast by the captions.
All burn-in and DVB-Sub font settings must match.

#### `BurninSubtitleTeletextSpacing`

``` purescript
newtype BurninSubtitleTeletextSpacing
  = BurninSubtitleTeletextSpacing String
```

Controls whether a fixed grid size or proportional font spacing will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.

#### `CancelJobRequest`

``` purescript
newtype CancelJobRequest
  = CancelJobRequest { "Id" :: String }
```

#### `CancelJobResponse`

``` purescript
newtype CancelJobResponse
  = CancelJobResponse {  }
```

#### `CaptionDescription`

``` purescript
newtype CaptionDescription
  = CaptionDescription { "CaptionSelectorName" :: NullOrUndefined (String), "DestinationSettings" :: NullOrUndefined (CaptionDestinationSettings), "LanguageCode" :: NullOrUndefined (LanguageCode), "LanguageDescription" :: NullOrUndefined (String) }
```

Description of Caption output

#### `CaptionDescriptionPreset`

``` purescript
newtype CaptionDescriptionPreset
  = CaptionDescriptionPreset { "DestinationSettings" :: NullOrUndefined (CaptionDestinationSettings), "LanguageCode" :: NullOrUndefined (LanguageCode), "LanguageDescription" :: NullOrUndefined (String) }
```

Caption Description for preset

#### `CaptionDestinationSettings`

``` purescript
newtype CaptionDestinationSettings
  = CaptionDestinationSettings { "BurninDestinationSettings" :: NullOrUndefined (BurninDestinationSettings), "DestinationType" :: NullOrUndefined (CaptionDestinationType), "DvbSubDestinationSettings" :: NullOrUndefined (DvbSubDestinationSettings), "SccDestinationSettings" :: NullOrUndefined (SccDestinationSettings), "TeletextDestinationSettings" :: NullOrUndefined (TeletextDestinationSettings), "TtmlDestinationSettings" :: NullOrUndefined (TtmlDestinationSettings) }
```

Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.

#### `CaptionDestinationType`

``` purescript
newtype CaptionDestinationType
  = CaptionDestinationType String
```

Type of Caption output, including Burn-In, Embedded, SCC, SRT, TTML, WebVTT, DVB-Sub, Teletext.

#### `CaptionSelector`

``` purescript
newtype CaptionSelector
  = CaptionSelector { "LanguageCode" :: NullOrUndefined (LanguageCode), "SourceSettings" :: NullOrUndefined (CaptionSourceSettings) }
```

Caption inputs to be mapped to caption outputs.

#### `CaptionSourceSettings`

``` purescript
newtype CaptionSourceSettings
  = CaptionSourceSettings { "AncillarySourceSettings" :: NullOrUndefined (AncillarySourceSettings), "DvbSubSourceSettings" :: NullOrUndefined (DvbSubSourceSettings), "EmbeddedSourceSettings" :: NullOrUndefined (EmbeddedSourceSettings), "FileSourceSettings" :: NullOrUndefined (FileSourceSettings), "SourceType" :: NullOrUndefined (CaptionSourceType), "TeletextSourceSettings" :: NullOrUndefined (TeletextSourceSettings) }
```

Source settings (SourceSettings) contains the group of settings for captions in the input.

#### `CaptionSourceType`

``` purescript
newtype CaptionSourceType
  = CaptionSourceType String
```

Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.

#### `ChannelMapping`

``` purescript
newtype ChannelMapping
  = ChannelMapping { "OutputChannels" :: NullOrUndefined (ListOfOutputChannelMapping) }
```

Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).

#### `ColorCorrector`

``` purescript
newtype ColorCorrector
  = ColorCorrector { "Brightness" :: NullOrUndefined (Int), "ColorSpaceConversion" :: NullOrUndefined (ColorSpaceConversion), "Contrast" :: NullOrUndefined (Int), "Hdr10Metadata" :: NullOrUndefined (Hdr10Metadata), "Hue" :: NullOrUndefined (Int), "Saturation" :: NullOrUndefined (Int) }
```

Settings for color correction.

#### `ColorMetadata`

``` purescript
newtype ColorMetadata
  = ColorMetadata String
```

Enable Insert color metadata (ColorMetadata) to include color metadata in this output. This setting is enabled by default.

#### `ColorSpace`

``` purescript
newtype ColorSpace
  = ColorSpace String
```

Specifies the colorspace of an input. This setting works in tandem with "Color Corrector":#color_corrector > color_space_conversion to determine if any conversion will be performed.

#### `ColorSpaceConversion`

``` purescript
newtype ColorSpaceConversion
  = ColorSpaceConversion String
```

Determines if colorspace conversion will be performed. If set to _None_, no conversion will be performed. If _Force 601_ or _Force 709_ are selected, conversion will be performed for inputs with differing colorspaces. An input's colorspace can be specified explicitly in the "Video Selector":#inputs-video_selector if necessary.

#### `ColorSpaceUsage`

``` purescript
newtype ColorSpaceUsage
  = ColorSpaceUsage String
```

There are two sources for color metadata, the input file and the job configuration. This enum controls which takes precedence. FORCE: System will use color metadata supplied by user, if any. If the user does not supply color metadata the system will use data from the source. FALLBACK: System will use color metadata from the source. If source has no color metadata, the system will use user-supplied color metadata values if available.

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "Message" :: NullOrUndefined (String) }
```

The service could not complete your request because there is a conflict with the current state of the resource.

#### `ContainerSettings`

``` purescript
newtype ContainerSettings
  = ContainerSettings { "Container" :: NullOrUndefined (ContainerType), "F4vSettings" :: NullOrUndefined (F4vSettings), "M2tsSettings" :: NullOrUndefined (M2tsSettings), "M3u8Settings" :: NullOrUndefined (M3u8Settings), "MovSettings" :: NullOrUndefined (MovSettings), "Mp4Settings" :: NullOrUndefined (Mp4Settings) }
```

Container specific settings.

#### `ContainerType`

``` purescript
newtype ContainerType
  = ContainerType String
```

Container for this output. Some containers require a container settings object. If not specified, the default object will be created.

#### `CreateJobRequest`

``` purescript
newtype CreateJobRequest
  = CreateJobRequest { "ClientRequestToken" :: NullOrUndefined (String), "JobTemplate" :: NullOrUndefined (String), "Queue" :: NullOrUndefined (String), "Role" :: NullOrUndefined (String), "Settings" :: NullOrUndefined (JobSettings), "UserMetadata" :: NullOrUndefined (MapOf__string) }
```

#### `CreateJobResponse`

``` purescript
newtype CreateJobResponse
  = CreateJobResponse { "Job" :: NullOrUndefined (Job) }
```

#### `CreateJobTemplateRequest`

``` purescript
newtype CreateJobTemplateRequest
  = CreateJobTemplateRequest { "Category" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Queue" :: NullOrUndefined (String), "Settings" :: NullOrUndefined (JobTemplateSettings) }
```

#### `CreateJobTemplateResponse`

``` purescript
newtype CreateJobTemplateResponse
  = CreateJobTemplateResponse { "JobTemplate" :: NullOrUndefined (JobTemplate) }
```

#### `CreatePresetRequest`

``` purescript
newtype CreatePresetRequest
  = CreatePresetRequest { "Category" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Settings" :: NullOrUndefined (PresetSettings) }
```

#### `CreatePresetResponse`

``` purescript
newtype CreatePresetResponse
  = CreatePresetResponse { "Preset" :: NullOrUndefined (Preset) }
```

#### `CreateQueueRequest`

``` purescript
newtype CreateQueueRequest
  = CreateQueueRequest { "Description" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

#### `CreateQueueResponse`

``` purescript
newtype CreateQueueResponse
  = CreateQueueResponse { "Queue" :: NullOrUndefined (Queue) }
```

#### `DashIsoEncryptionSettings`

``` purescript
newtype DashIsoEncryptionSettings
  = DashIsoEncryptionSettings { "SpekeKeyProvider" :: NullOrUndefined (SpekeKeyProvider) }
```

Specifies DRM settings for DASH outputs.

#### `DashIsoGroupSettings`

``` purescript
newtype DashIsoGroupSettings
  = DashIsoGroupSettings { "BaseUrl" :: NullOrUndefined (String), "Destination" :: NullOrUndefined (String), "Encryption" :: NullOrUndefined (DashIsoEncryptionSettings), "FragmentLength" :: NullOrUndefined (Int), "HbbtvCompliance" :: NullOrUndefined (DashIsoHbbtvCompliance), "MinBufferTime" :: NullOrUndefined (Int), "SegmentControl" :: NullOrUndefined (DashIsoSegmentControl), "SegmentLength" :: NullOrUndefined (Int) }
```

Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.

#### `DashIsoHbbtvCompliance`

``` purescript
newtype DashIsoHbbtvCompliance
  = DashIsoHbbtvCompliance String
```

Supports HbbTV specification as indicated

#### `DashIsoSegmentControl`

``` purescript
newtype DashIsoSegmentControl
  = DashIsoSegmentControl String
```

When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.

#### `DeinterlaceAlgorithm`

``` purescript
newtype DeinterlaceAlgorithm
  = DeinterlaceAlgorithm String
```

Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.

#### `Deinterlacer`

``` purescript
newtype Deinterlacer
  = Deinterlacer { "Algorithm" :: NullOrUndefined (DeinterlaceAlgorithm), "Control" :: NullOrUndefined (DeinterlacerControl), "Mode" :: NullOrUndefined (DeinterlacerMode) }
```

Settings for deinterlacer

#### `DeinterlacerControl`

``` purescript
newtype DeinterlacerControl
  = DeinterlacerControl String
```

- When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.

#### `DeinterlacerMode`

``` purescript
newtype DeinterlacerMode
  = DeinterlacerMode String
```

Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.

#### `DeleteJobTemplateRequest`

``` purescript
newtype DeleteJobTemplateRequest
  = DeleteJobTemplateRequest { "Name" :: String }
```

#### `DeleteJobTemplateResponse`

``` purescript
newtype DeleteJobTemplateResponse
  = DeleteJobTemplateResponse {  }
```

#### `DeletePresetRequest`

``` purescript
newtype DeletePresetRequest
  = DeletePresetRequest { "Name" :: String }
```

#### `DeletePresetResponse`

``` purescript
newtype DeletePresetResponse
  = DeletePresetResponse {  }
```

#### `DeleteQueueRequest`

``` purescript
newtype DeleteQueueRequest
  = DeleteQueueRequest { "Name" :: String }
```

#### `DeleteQueueResponse`

``` purescript
newtype DeleteQueueResponse
  = DeleteQueueResponse {  }
```

#### `DescribeEndpointsRequest`

``` purescript
newtype DescribeEndpointsRequest
  = DescribeEndpointsRequest { "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

DescribeEndpointsRequest

#### `DescribeEndpointsResponse`

``` purescript
newtype DescribeEndpointsResponse
  = DescribeEndpointsResponse { "Endpoints" :: NullOrUndefined (ListOfEndpoint), "NextToken" :: NullOrUndefined (String) }
```

#### `DropFrameTimecode`

``` purescript
newtype DropFrameTimecode
  = DropFrameTimecode String
```

Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.

#### `DvbNitSettings`

``` purescript
newtype DvbNitSettings
  = DvbNitSettings { "NetworkId" :: NullOrUndefined (Int), "NetworkName" :: NullOrUndefined (String), "NitInterval" :: NullOrUndefined (Int) }
```

Inserts DVB Network Information Table (NIT) at the specified table repetition interval.

#### `DvbSdtSettings`

``` purescript
newtype DvbSdtSettings
  = DvbSdtSettings { "OutputSdt" :: NullOrUndefined (OutputSdt), "SdtInterval" :: NullOrUndefined (Int), "ServiceName" :: NullOrUndefined (String), "ServiceProviderName" :: NullOrUndefined (String) }
```

Inserts DVB Service Description Table (NIT) at the specified table repetition interval.

#### `DvbSubDestinationSettings`

``` purescript
newtype DvbSubDestinationSettings
  = DvbSubDestinationSettings { "Alignment" :: NullOrUndefined (DvbSubtitleAlignment), "BackgroundColor" :: NullOrUndefined (DvbSubtitleBackgroundColor), "BackgroundOpacity" :: NullOrUndefined (Int), "FontColor" :: NullOrUndefined (DvbSubtitleFontColor), "FontOpacity" :: NullOrUndefined (Int), "FontResolution" :: NullOrUndefined (Int), "FontSize" :: NullOrUndefined (Int), "OutlineColor" :: NullOrUndefined (DvbSubtitleOutlineColor), "OutlineSize" :: NullOrUndefined (Int), "ShadowColor" :: NullOrUndefined (DvbSubtitleShadowColor), "ShadowOpacity" :: NullOrUndefined (Int), "ShadowXOffset" :: NullOrUndefined (Int), "ShadowYOffset" :: NullOrUndefined (Int), "TeletextSpacing" :: NullOrUndefined (DvbSubtitleTeletextSpacing), "XPosition" :: NullOrUndefined (Int), "YPosition" :: NullOrUndefined (Int) }
```

DVB-Sub Destination Settings

#### `DvbSubSourceSettings`

``` purescript
newtype DvbSubSourceSettings
  = DvbSubSourceSettings { "Pid" :: NullOrUndefined (Int) }
```

DVB Sub Source Settings

#### `DvbSubtitleAlignment`

``` purescript
newtype DvbSubtitleAlignment
  = DvbSubtitleAlignment String
```

If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.

#### `DvbSubtitleBackgroundColor`

``` purescript
newtype DvbSubtitleBackgroundColor
  = DvbSubtitleBackgroundColor String
```

Specifies the color of the rectangle behind the captions.
All burn-in and DVB-Sub font settings must match.

#### `DvbSubtitleFontColor`

``` purescript
newtype DvbSubtitleFontColor
  = DvbSubtitleFontColor String
```

Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.

#### `DvbSubtitleOutlineColor`

``` purescript
newtype DvbSubtitleOutlineColor
  = DvbSubtitleOutlineColor String
```

Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.

#### `DvbSubtitleShadowColor`

``` purescript
newtype DvbSubtitleShadowColor
  = DvbSubtitleShadowColor String
```

Specifies the color of the shadow cast by the captions.
All burn-in and DVB-Sub font settings must match.

#### `DvbSubtitleTeletextSpacing`

``` purescript
newtype DvbSubtitleTeletextSpacing
  = DvbSubtitleTeletextSpacing String
```

Controls whether a fixed grid size or proportional font spacing will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.

#### `DvbTdtSettings`

``` purescript
newtype DvbTdtSettings
  = DvbTdtSettings { "TdtInterval" :: NullOrUndefined (Int) }
```

Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.

#### `Eac3AttenuationControl`

``` purescript
newtype Eac3AttenuationControl
  = Eac3AttenuationControl String
```

If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.

#### `Eac3BitstreamMode`

``` purescript
newtype Eac3BitstreamMode
  = Eac3BitstreamMode String
```

Specifies the "Bitstream Mode" (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.

#### `Eac3CodingMode`

``` purescript
newtype Eac3CodingMode
  = Eac3CodingMode String
```

Dolby Digital Plus coding mode. Determines number of channels.

#### `Eac3DcFilter`

``` purescript
newtype Eac3DcFilter
  = Eac3DcFilter String
```

Activates a DC highpass filter for all input channels.

#### `Eac3DynamicRangeCompressionLine`

``` purescript
newtype Eac3DynamicRangeCompressionLine
  = Eac3DynamicRangeCompressionLine String
```

Enables Dynamic Range Compression that restricts the absolute peak level for a signal.

#### `Eac3DynamicRangeCompressionRf`

``` purescript
newtype Eac3DynamicRangeCompressionRf
  = Eac3DynamicRangeCompressionRf String
```

Enables Heavy Dynamic Range Compression, ensures that the instantaneous signal peaks do not exceed specified levels.

#### `Eac3LfeControl`

``` purescript
newtype Eac3LfeControl
  = Eac3LfeControl String
```

When encoding 3/2 audio, controls whether the LFE channel is enabled

#### `Eac3LfeFilter`

``` purescript
newtype Eac3LfeFilter
  = Eac3LfeFilter String
```

Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.

#### `Eac3MetadataControl`

``` purescript
newtype Eac3MetadataControl
  = Eac3MetadataControl String
```

When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.

#### `Eac3PassthroughControl`

``` purescript
newtype Eac3PassthroughControl
  = Eac3PassthroughControl String
```

When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.

#### `Eac3PhaseControl`

``` purescript
newtype Eac3PhaseControl
  = Eac3PhaseControl String
```

Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.

#### `Eac3Settings`

``` purescript
newtype Eac3Settings
  = Eac3Settings { "AttenuationControl" :: NullOrUndefined (Eac3AttenuationControl), "Bitrate" :: NullOrUndefined (Int), "BitstreamMode" :: NullOrUndefined (Eac3BitstreamMode), "CodingMode" :: NullOrUndefined (Eac3CodingMode), "DcFilter" :: NullOrUndefined (Eac3DcFilter), "Dialnorm" :: NullOrUndefined (Int), "DynamicRangeCompressionLine" :: NullOrUndefined (Eac3DynamicRangeCompressionLine), "DynamicRangeCompressionRf" :: NullOrUndefined (Eac3DynamicRangeCompressionRf), "LfeControl" :: NullOrUndefined (Eac3LfeControl), "LfeFilter" :: NullOrUndefined (Eac3LfeFilter), "LoRoCenterMixLevel" :: NullOrUndefined (Number), "LoRoSurroundMixLevel" :: NullOrUndefined (Number), "LtRtCenterMixLevel" :: NullOrUndefined (Number), "LtRtSurroundMixLevel" :: NullOrUndefined (Number), "MetadataControl" :: NullOrUndefined (Eac3MetadataControl), "PassthroughControl" :: NullOrUndefined (Eac3PassthroughControl), "PhaseControl" :: NullOrUndefined (Eac3PhaseControl), "SampleRate" :: NullOrUndefined (Int), "StereoDownmix" :: NullOrUndefined (Eac3StereoDownmix), "SurroundExMode" :: NullOrUndefined (Eac3SurroundExMode), "SurroundMode" :: NullOrUndefined (Eac3SurroundMode) }
```

Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.

#### `Eac3StereoDownmix`

``` purescript
newtype Eac3StereoDownmix
  = Eac3StereoDownmix String
```

Stereo downmix preference. Only used for 3/2 coding mode.

#### `Eac3SurroundExMode`

``` purescript
newtype Eac3SurroundExMode
  = Eac3SurroundExMode String
```

When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.

#### `Eac3SurroundMode`

``` purescript
newtype Eac3SurroundMode
  = Eac3SurroundMode String
```

When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.

#### `EmbeddedConvert608To708`

``` purescript
newtype EmbeddedConvert608To708
  = EmbeddedConvert608To708 String
```

When set to UPCONVERT, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.

#### `EmbeddedSourceSettings`

``` purescript
newtype EmbeddedSourceSettings
  = EmbeddedSourceSettings { "Convert608To708" :: NullOrUndefined (EmbeddedConvert608To708), "Source608ChannelNumber" :: NullOrUndefined (Int), "Source608TrackNumber" :: NullOrUndefined (Int) }
```

Settings for embedded captions Source

#### `Endpoint`

``` purescript
newtype Endpoint
  = Endpoint { "Url" :: NullOrUndefined (String) }
```

Describes account specific API endpoint

#### `ExceptionBody`

``` purescript
newtype ExceptionBody
  = ExceptionBody { "Message" :: NullOrUndefined (String) }
```

#### `F4vMoovPlacement`

``` purescript
newtype F4vMoovPlacement
  = F4vMoovPlacement String
```

If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.

#### `F4vSettings`

``` purescript
newtype F4vSettings
  = F4vSettings { "MoovPlacement" :: NullOrUndefined (F4vMoovPlacement) }
```

Settings for F4v container

#### `FileGroupSettings`

``` purescript
newtype FileGroupSettings
  = FileGroupSettings { "Destination" :: NullOrUndefined (String) }
```

Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.

#### `FileSourceConvert608To708`

``` purescript
newtype FileSourceConvert608To708
  = FileSourceConvert608To708 String
```

If set to UPCONVERT, 608 caption data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.

#### `FileSourceSettings`

``` purescript
newtype FileSourceSettings
  = FileSourceSettings { "Convert608To708" :: NullOrUndefined (FileSourceConvert608To708), "SourceFile" :: NullOrUndefined (String), "TimeDelta" :: NullOrUndefined (Int) }
```

Settings for File-based Captions in Source

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "Message" :: NullOrUndefined (String) }
```

You don't have permissions for this action with the credentials you sent.

#### `FrameCaptureSettings`

``` purescript
newtype FrameCaptureSettings
  = FrameCaptureSettings { "FramerateDenominator" :: NullOrUndefined (Int), "FramerateNumerator" :: NullOrUndefined (Int), "MaxCaptures" :: NullOrUndefined (Int), "Quality" :: NullOrUndefined (Int) }
```

Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.

#### `GetJobRequest`

``` purescript
newtype GetJobRequest
  = GetJobRequest { "Id" :: String }
```

#### `GetJobResponse`

``` purescript
newtype GetJobResponse
  = GetJobResponse { "Job" :: NullOrUndefined (Job) }
```

#### `GetJobTemplateRequest`

``` purescript
newtype GetJobTemplateRequest
  = GetJobTemplateRequest { "Name" :: String }
```

#### `GetJobTemplateResponse`

``` purescript
newtype GetJobTemplateResponse
  = GetJobTemplateResponse { "JobTemplate" :: NullOrUndefined (JobTemplate) }
```

#### `GetPresetRequest`

``` purescript
newtype GetPresetRequest
  = GetPresetRequest { "Name" :: String }
```

#### `GetPresetResponse`

``` purescript
newtype GetPresetResponse
  = GetPresetResponse { "Preset" :: NullOrUndefined (Preset) }
```

#### `GetQueueRequest`

``` purescript
newtype GetQueueRequest
  = GetQueueRequest { "Name" :: String }
```

#### `GetQueueResponse`

``` purescript
newtype GetQueueResponse
  = GetQueueResponse { "Queue" :: NullOrUndefined (Queue) }
```

#### `H264AdaptiveQuantization`

``` purescript
newtype H264AdaptiveQuantization
  = H264AdaptiveQuantization String
```

Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.

#### `H264CodecLevel`

``` purescript
newtype H264CodecLevel
  = H264CodecLevel String
```

H.264 Level.

#### `H264CodecProfile`

``` purescript
newtype H264CodecProfile
  = H264CodecProfile String
```

H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.

#### `H264EntropyEncoding`

``` purescript
newtype H264EntropyEncoding
  = H264EntropyEncoding String
```

Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.

#### `H264FieldEncoding`

``` purescript
newtype H264FieldEncoding
  = H264FieldEncoding String
```

Choosing FORCE_FIELD disables PAFF encoding for interlaced outputs.

#### `H264FlickerAdaptiveQuantization`

``` purescript
newtype H264FlickerAdaptiveQuantization
  = H264FlickerAdaptiveQuantization String
```

Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.

#### `H264FramerateControl`

``` purescript
newtype H264FramerateControl
  = H264FramerateControl String
```

Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.

#### `H264FramerateConversionAlgorithm`

``` purescript
newtype H264FramerateConversionAlgorithm
  = H264FramerateConversionAlgorithm String
```

When set to INTERPOLATE, produces smoother motion during framerate conversion.

#### `H264GopBReference`

``` purescript
newtype H264GopBReference
  = H264GopBReference String
```

If enable, use reference B frames for GOP structures that have B frames > 1.

#### `H264GopSizeUnits`

``` purescript
newtype H264GopSizeUnits
  = H264GopSizeUnits String
```

Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.

#### `H264InterlaceMode`

``` purescript
newtype H264InterlaceMode
  = H264InterlaceMode String
```

Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.

#### `H264ParControl`

``` purescript
newtype H264ParControl
  = H264ParControl String
```

Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.

#### `H264QualityTuningLevel`

``` purescript
newtype H264QualityTuningLevel
  = H264QualityTuningLevel String
```

Use Quality tuning level (H264QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.

#### `H264RateControlMode`

``` purescript
newtype H264RateControlMode
  = H264RateControlMode String
```

Rate control mode. CQ uses constant quantizer (qp), ABR (average bitrate) does not write HRD parameters.

#### `H264RepeatPps`

``` purescript
newtype H264RepeatPps
  = H264RepeatPps String
```

Places a PPS header on each encoded picture, even if repeated.

#### `H264SceneChangeDetect`

``` purescript
newtype H264SceneChangeDetect
  = H264SceneChangeDetect String
```

Scene change detection (inserts I-frames on scene changes).

#### `H264Settings`

``` purescript
newtype H264Settings
  = H264Settings { "AdaptiveQuantization" :: NullOrUndefined (H264AdaptiveQuantization), "Bitrate" :: NullOrUndefined (Int), "CodecLevel" :: NullOrUndefined (H264CodecLevel), "CodecProfile" :: NullOrUndefined (H264CodecProfile), "EntropyEncoding" :: NullOrUndefined (H264EntropyEncoding), "FieldEncoding" :: NullOrUndefined (H264FieldEncoding), "FlickerAdaptiveQuantization" :: NullOrUndefined (H264FlickerAdaptiveQuantization), "FramerateControl" :: NullOrUndefined (H264FramerateControl), "FramerateConversionAlgorithm" :: NullOrUndefined (H264FramerateConversionAlgorithm), "FramerateDenominator" :: NullOrUndefined (Int), "FramerateNumerator" :: NullOrUndefined (Int), "GopBReference" :: NullOrUndefined (H264GopBReference), "GopClosedCadence" :: NullOrUndefined (Int), "GopSize" :: NullOrUndefined (Number), "GopSizeUnits" :: NullOrUndefined (H264GopSizeUnits), "HrdBufferInitialFillPercentage" :: NullOrUndefined (Int), "HrdBufferSize" :: NullOrUndefined (Int), "InterlaceMode" :: NullOrUndefined (H264InterlaceMode), "MaxBitrate" :: NullOrUndefined (Int), "MinIInterval" :: NullOrUndefined (Int), "NumberBFramesBetweenReferenceFrames" :: NullOrUndefined (Int), "NumberReferenceFrames" :: NullOrUndefined (Int), "ParControl" :: NullOrUndefined (H264ParControl), "ParDenominator" :: NullOrUndefined (Int), "ParNumerator" :: NullOrUndefined (Int), "QualityTuningLevel" :: NullOrUndefined (H264QualityTuningLevel), "RateControlMode" :: NullOrUndefined (H264RateControlMode), "RepeatPps" :: NullOrUndefined (H264RepeatPps), "SceneChangeDetect" :: NullOrUndefined (H264SceneChangeDetect), "Slices" :: NullOrUndefined (Int), "SlowPal" :: NullOrUndefined (H264SlowPal), "Softness" :: NullOrUndefined (Int), "SpatialAdaptiveQuantization" :: NullOrUndefined (H264SpatialAdaptiveQuantization), "Syntax" :: NullOrUndefined (H264Syntax), "Telecine" :: NullOrUndefined (H264Telecine), "TemporalAdaptiveQuantization" :: NullOrUndefined (H264TemporalAdaptiveQuantization), "UnregisteredSeiTimecode" :: NullOrUndefined (H264UnregisteredSeiTimecode) }
```

Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.

#### `H264SlowPal`

``` purescript
newtype H264SlowPal
  = H264SlowPal String
```

Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.

#### `H264SpatialAdaptiveQuantization`

``` purescript
newtype H264SpatialAdaptiveQuantization
  = H264SpatialAdaptiveQuantization String
```

Adjust quantization within each frame based on spatial variation of content complexity.

#### `H264Syntax`

``` purescript
newtype H264Syntax
  = H264Syntax String
```

Produces a bitstream compliant with SMPTE RP-2027.

#### `H264Telecine`

``` purescript
newtype H264Telecine
  = H264Telecine String
```

This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.

#### `H264TemporalAdaptiveQuantization`

``` purescript
newtype H264TemporalAdaptiveQuantization
  = H264TemporalAdaptiveQuantization String
```

Adjust quantization within each frame based on temporal variation of content complexity.

#### `H264UnregisteredSeiTimecode`

``` purescript
newtype H264UnregisteredSeiTimecode
  = H264UnregisteredSeiTimecode String
```

Inserts timecode for each frame as 4 bytes of an unregistered SEI message.

#### `H265AdaptiveQuantization`

``` purescript
newtype H265AdaptiveQuantization
  = H265AdaptiveQuantization String
```

Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.

#### `H265AlternateTransferFunctionSei`

``` purescript
newtype H265AlternateTransferFunctionSei
  = H265AlternateTransferFunctionSei String
```

Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).

#### `H265CodecLevel`

``` purescript
newtype H265CodecLevel
  = H265CodecLevel String
```

H.265 Level.

#### `H265CodecProfile`

``` purescript
newtype H265CodecProfile
  = H265CodecProfile String
```

Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.

#### `H265FlickerAdaptiveQuantization`

``` purescript
newtype H265FlickerAdaptiveQuantization
  = H265FlickerAdaptiveQuantization String
```

Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.

#### `H265FramerateControl`

``` purescript
newtype H265FramerateControl
  = H265FramerateControl String
```

Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.

#### `H265FramerateConversionAlgorithm`

``` purescript
newtype H265FramerateConversionAlgorithm
  = H265FramerateConversionAlgorithm String
```

When set to INTERPOLATE, produces smoother motion during framerate conversion.

#### `H265GopBReference`

``` purescript
newtype H265GopBReference
  = H265GopBReference String
```

If enable, use reference B frames for GOP structures that have B frames > 1.

#### `H265GopSizeUnits`

``` purescript
newtype H265GopSizeUnits
  = H265GopSizeUnits String
```

Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.

#### `H265InterlaceMode`

``` purescript
newtype H265InterlaceMode
  = H265InterlaceMode String
```

Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.

#### `H265ParControl`

``` purescript
newtype H265ParControl
  = H265ParControl String
```

Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.

#### `H265QualityTuningLevel`

``` purescript
newtype H265QualityTuningLevel
  = H265QualityTuningLevel String
```

Use Quality tuning level (H265QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.

#### `H265RateControlMode`

``` purescript
newtype H265RateControlMode
  = H265RateControlMode String
```

Rate control mode. CQ uses constant quantizer (qp), ABR (average bitrate) does not write HRD parameters.

#### `H265SampleAdaptiveOffsetFilterMode`

``` purescript
newtype H265SampleAdaptiveOffsetFilterMode
  = H265SampleAdaptiveOffsetFilterMode String
```

Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content

#### `H265SceneChangeDetect`

``` purescript
newtype H265SceneChangeDetect
  = H265SceneChangeDetect String
```

Scene change detection (inserts I-frames on scene changes).

#### `H265Settings`

``` purescript
newtype H265Settings
  = H265Settings { "AdaptiveQuantization" :: NullOrUndefined (H265AdaptiveQuantization), "AlternateTransferFunctionSei" :: NullOrUndefined (H265AlternateTransferFunctionSei), "Bitrate" :: NullOrUndefined (Int), "CodecLevel" :: NullOrUndefined (H265CodecLevel), "CodecProfile" :: NullOrUndefined (H265CodecProfile), "FlickerAdaptiveQuantization" :: NullOrUndefined (H265FlickerAdaptiveQuantization), "FramerateControl" :: NullOrUndefined (H265FramerateControl), "FramerateConversionAlgorithm" :: NullOrUndefined (H265FramerateConversionAlgorithm), "FramerateDenominator" :: NullOrUndefined (Int), "FramerateNumerator" :: NullOrUndefined (Int), "GopBReference" :: NullOrUndefined (H265GopBReference), "GopClosedCadence" :: NullOrUndefined (Int), "GopSize" :: NullOrUndefined (Number), "GopSizeUnits" :: NullOrUndefined (H265GopSizeUnits), "HrdBufferInitialFillPercentage" :: NullOrUndefined (Int), "HrdBufferSize" :: NullOrUndefined (Int), "InterlaceMode" :: NullOrUndefined (H265InterlaceMode), "MaxBitrate" :: NullOrUndefined (Int), "MinIInterval" :: NullOrUndefined (Int), "NumberBFramesBetweenReferenceFrames" :: NullOrUndefined (Int), "NumberReferenceFrames" :: NullOrUndefined (Int), "ParControl" :: NullOrUndefined (H265ParControl), "ParDenominator" :: NullOrUndefined (Int), "ParNumerator" :: NullOrUndefined (Int), "QualityTuningLevel" :: NullOrUndefined (H265QualityTuningLevel), "RateControlMode" :: NullOrUndefined (H265RateControlMode), "SampleAdaptiveOffsetFilterMode" :: NullOrUndefined (H265SampleAdaptiveOffsetFilterMode), "SceneChangeDetect" :: NullOrUndefined (H265SceneChangeDetect), "Slices" :: NullOrUndefined (Int), "SlowPal" :: NullOrUndefined (H265SlowPal), "SpatialAdaptiveQuantization" :: NullOrUndefined (H265SpatialAdaptiveQuantization), "Telecine" :: NullOrUndefined (H265Telecine), "TemporalAdaptiveQuantization" :: NullOrUndefined (H265TemporalAdaptiveQuantization), "TemporalIds" :: NullOrUndefined (H265TemporalIds), "Tiles" :: NullOrUndefined (H265Tiles), "UnregisteredSeiTimecode" :: NullOrUndefined (H265UnregisteredSeiTimecode) }
```

Settings for H265 codec

#### `H265SlowPal`

``` purescript
newtype H265SlowPal
  = H265SlowPal String
```

Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.

#### `H265SpatialAdaptiveQuantization`

``` purescript
newtype H265SpatialAdaptiveQuantization
  = H265SpatialAdaptiveQuantization String
```

Adjust quantization within each frame based on spatial variation of content complexity.

#### `H265Telecine`

``` purescript
newtype H265Telecine
  = H265Telecine String
```

This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.

#### `H265TemporalAdaptiveQuantization`

``` purescript
newtype H265TemporalAdaptiveQuantization
  = H265TemporalAdaptiveQuantization String
```

Adjust quantization within each frame based on temporal variation of content complexity.

#### `H265TemporalIds`

``` purescript
newtype H265TemporalIds
  = H265TemporalIds String
```

Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.

#### `H265Tiles`

``` purescript
newtype H265Tiles
  = H265Tiles String
```

Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.

#### `H265UnregisteredSeiTimecode`

``` purescript
newtype H265UnregisteredSeiTimecode
  = H265UnregisteredSeiTimecode String
```

Inserts timecode for each frame as 4 bytes of an unregistered SEI message.

#### `Hdr10Metadata`

``` purescript
newtype Hdr10Metadata
  = Hdr10Metadata { "BluePrimaryX" :: NullOrUndefined (Int), "BluePrimaryY" :: NullOrUndefined (Int), "GreenPrimaryX" :: NullOrUndefined (Int), "GreenPrimaryY" :: NullOrUndefined (Int), "MaxContentLightLevel" :: NullOrUndefined (Int), "MaxFrameAverageLightLevel" :: NullOrUndefined (Int), "MaxLuminance" :: NullOrUndefined (Int), "MinLuminance" :: NullOrUndefined (Int), "RedPrimaryX" :: NullOrUndefined (Int), "RedPrimaryY" :: NullOrUndefined (Int), "WhitePointX" :: NullOrUndefined (Int), "WhitePointY" :: NullOrUndefined (Int) }
```

Use the HDR master display (Hdr10Metadata) settings to provide values for HDR color. These values vary depending on the input video and must be provided by a color grader. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.

#### `HlsAdMarkers`

``` purescript
newtype HlsAdMarkers
  = HlsAdMarkers String
```

#### `HlsAudioTrackType`

``` purescript
newtype HlsAudioTrackType
  = HlsAudioTrackType String
```

Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO

#### `HlsCaptionLanguageMapping`

``` purescript
newtype HlsCaptionLanguageMapping
  = HlsCaptionLanguageMapping { "CaptionChannel" :: NullOrUndefined (Int), "LanguageCode" :: NullOrUndefined (LanguageCode), "LanguageDescription" :: NullOrUndefined (String) }
```

Caption Language Mapping

#### `HlsCaptionLanguageSetting`

``` purescript
newtype HlsCaptionLanguageSetting
  = HlsCaptionLanguageSetting String
```

Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.

#### `HlsClientCache`

``` purescript
newtype HlsClientCache
  = HlsClientCache String
```

When set to ENABLED, sets #EXT-X-ALLOW-CACHE:no tag, which prevents client from saving media segments for later replay.

#### `HlsCodecSpecification`

``` purescript
newtype HlsCodecSpecification
  = HlsCodecSpecification String
```

Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.

#### `HlsDirectoryStructure`

``` purescript
newtype HlsDirectoryStructure
  = HlsDirectoryStructure String
```

Indicates whether segments should be placed in subdirectories.

#### `HlsEncryptionSettings`

``` purescript
newtype HlsEncryptionSettings
  = HlsEncryptionSettings { "ConstantInitializationVector" :: NullOrUndefined (String), "EncryptionMethod" :: NullOrUndefined (HlsEncryptionType), "InitializationVectorInManifest" :: NullOrUndefined (HlsInitializationVectorInManifest), "SpekeKeyProvider" :: NullOrUndefined (SpekeKeyProvider), "StaticKeyProvider" :: NullOrUndefined (StaticKeyProvider), "Type" :: NullOrUndefined (HlsKeyProviderType) }
```

Settings for HLS encryption

#### `HlsEncryptionType`

``` purescript
newtype HlsEncryptionType
  = HlsEncryptionType String
```

Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.

#### `HlsGroupSettings`

``` purescript
newtype HlsGroupSettings
  = HlsGroupSettings { "AdMarkers" :: NullOrUndefined (ListOfHlsAdMarkers), "BaseUrl" :: NullOrUndefined (String), "CaptionLanguageMappings" :: NullOrUndefined (ListOfHlsCaptionLanguageMapping), "CaptionLanguageSetting" :: NullOrUndefined (HlsCaptionLanguageSetting), "ClientCache" :: NullOrUndefined (HlsClientCache), "CodecSpecification" :: NullOrUndefined (HlsCodecSpecification), "Destination" :: NullOrUndefined (String), "DirectoryStructure" :: NullOrUndefined (HlsDirectoryStructure), "Encryption" :: NullOrUndefined (HlsEncryptionSettings), "ManifestCompression" :: NullOrUndefined (HlsManifestCompression), "ManifestDurationFormat" :: NullOrUndefined (HlsManifestDurationFormat), "MinSegmentLength" :: NullOrUndefined (Int), "OutputSelection" :: NullOrUndefined (HlsOutputSelection), "ProgramDateTime" :: NullOrUndefined (HlsProgramDateTime), "ProgramDateTimePeriod" :: NullOrUndefined (Int), "SegmentControl" :: NullOrUndefined (HlsSegmentControl), "SegmentLength" :: NullOrUndefined (Int), "SegmentsPerSubdirectory" :: NullOrUndefined (Int), "StreamInfResolution" :: NullOrUndefined (HlsStreamInfResolution), "TimedMetadataId3Frame" :: NullOrUndefined (HlsTimedMetadataId3Frame), "TimedMetadataId3Period" :: NullOrUndefined (Int), "TimestampDeltaMilliseconds" :: NullOrUndefined (Int) }
```

Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.

#### `HlsIFrameOnlyManifest`

``` purescript
newtype HlsIFrameOnlyManifest
  = HlsIFrameOnlyManifest String
```

When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest

#### `HlsInitializationVectorInManifest`

``` purescript
newtype HlsInitializationVectorInManifest
  = HlsInitializationVectorInManifest String
```

The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.

#### `HlsKeyProviderType`

``` purescript
newtype HlsKeyProviderType
  = HlsKeyProviderType String
```

Indicates which type of key provider is used for encryption.

#### `HlsManifestCompression`

``` purescript
newtype HlsManifestCompression
  = HlsManifestCompression String
```

When set to GZIP, compresses HLS playlist.

#### `HlsManifestDurationFormat`

``` purescript
newtype HlsManifestDurationFormat
  = HlsManifestDurationFormat String
```

Indicates whether the output manifest should use floating point values for segment duration.

#### `HlsOutputSelection`

``` purescript
newtype HlsOutputSelection
  = HlsOutputSelection String
```

Indicates whether the .m3u8 manifest file should be generated for this HLS output group.

#### `HlsProgramDateTime`

``` purescript
newtype HlsProgramDateTime
  = HlsProgramDateTime String
```

Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.

#### `HlsSegmentControl`

``` purescript
newtype HlsSegmentControl
  = HlsSegmentControl String
```

When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.

#### `HlsSettings`

``` purescript
newtype HlsSettings
  = HlsSettings { "AudioGroupId" :: NullOrUndefined (String), "AudioRenditionSets" :: NullOrUndefined (String), "AudioTrackType" :: NullOrUndefined (HlsAudioTrackType), "IFrameOnlyManifest" :: NullOrUndefined (HlsIFrameOnlyManifest), "SegmentModifier" :: NullOrUndefined (String) }
```

Settings for HLS output groups

#### `HlsStreamInfResolution`

``` purescript
newtype HlsStreamInfResolution
  = HlsStreamInfResolution String
```

Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.

#### `HlsTimedMetadataId3Frame`

``` purescript
newtype HlsTimedMetadataId3Frame
  = HlsTimedMetadataId3Frame String
```

Indicates ID3 frame that has the timecode.

#### `Id3Insertion`

``` purescript
newtype Id3Insertion
  = Id3Insertion { "Id3" :: NullOrUndefined (String), "Timecode" :: NullOrUndefined (String) }
```

To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3) to specify the base 64 encoded string and use Timecode (TimeCode) to specify the time when the tag should be inserted. To insert multiple ID3 tags in your output, create mulitple instances of ID3 insertion (Id3Insertion).

#### `ImageInserter`

``` purescript
newtype ImageInserter
  = ImageInserter { "InsertableImages" :: NullOrUndefined (ListOfInsertableImage) }
```

Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.

#### `Input`

``` purescript
newtype Input
  = Input { "AudioSelectorGroups" :: NullOrUndefined (MapOfAudioSelectorGroup), "AudioSelectors" :: NullOrUndefined (MapOfAudioSelector), "CaptionSelectors" :: NullOrUndefined (MapOfCaptionSelector), "DeblockFilter" :: NullOrUndefined (InputDeblockFilter), "DenoiseFilter" :: NullOrUndefined (InputDenoiseFilter), "FileInput" :: NullOrUndefined (String), "FilterEnable" :: NullOrUndefined (InputFilterEnable), "FilterStrength" :: NullOrUndefined (Int), "InputClippings" :: NullOrUndefined (ListOfInputClipping), "ProgramNumber" :: NullOrUndefined (Int), "PsiControl" :: NullOrUndefined (InputPsiControl), "TimecodeSource" :: NullOrUndefined (InputTimecodeSource), "VideoSelector" :: NullOrUndefined (VideoSelector) }
```

Specifies media input

#### `InputClipping`

``` purescript
newtype InputClipping
  = InputClipping { "EndTimecode" :: NullOrUndefined (String), "StartTimecode" :: NullOrUndefined (String) }
```

Include one instance of (InputClipping) for each input clip.

#### `InputDeblockFilter`

``` purescript
newtype InputDeblockFilter
  = InputDeblockFilter String
```

Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manaully controllable for MPEG2 and uncompressed video inputs.

#### `InputDenoiseFilter`

``` purescript
newtype InputDenoiseFilter
  = InputDenoiseFilter String
```

Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.

#### `InputFilterEnable`

``` purescript
newtype InputFilterEnable
  = InputFilterEnable String
```

Use Filter enable (InputFilterEnable) to specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The in put is filtered regardless of input type.

#### `InputPsiControl`

``` purescript
newtype InputPsiControl
  = InputPsiControl String
```

Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.

#### `InputTemplate`

``` purescript
newtype InputTemplate
  = InputTemplate { "AudioSelectorGroups" :: NullOrUndefined (MapOfAudioSelectorGroup), "AudioSelectors" :: NullOrUndefined (MapOfAudioSelector), "CaptionSelectors" :: NullOrUndefined (MapOfCaptionSelector), "DeblockFilter" :: NullOrUndefined (InputDeblockFilter), "DenoiseFilter" :: NullOrUndefined (InputDenoiseFilter), "FilterEnable" :: NullOrUndefined (InputFilterEnable), "FilterStrength" :: NullOrUndefined (Int), "InputClippings" :: NullOrUndefined (ListOfInputClipping), "ProgramNumber" :: NullOrUndefined (Int), "PsiControl" :: NullOrUndefined (InputPsiControl), "TimecodeSource" :: NullOrUndefined (InputTimecodeSource), "VideoSelector" :: NullOrUndefined (VideoSelector) }
```

Specified video input in a template.

#### `InputTimecodeSource`

``` purescript
newtype InputTimecodeSource
  = InputTimecodeSource String
```

Use Timecode source (InputTimecodeSource) to specify how timecode information from your input is adjusted and encoded in all outputs for the job. Default is embedded. Set to Embedded (EMBEDDED) to use the timecode that is in the input video. If no embedded timecode is in the source, will set the timecode for the first frame to 00:00:00:00. Set to Start at 0 (ZEROBASED) to set the timecode of the initial frame to 00:00:00:00. Set to Specified start (SPECIFIEDSTART) to provide the initial timecode yourself the setting (Start).

#### `InsertableImage`

``` purescript
newtype InsertableImage
  = InsertableImage { "Duration" :: NullOrUndefined (Int), "FadeIn" :: NullOrUndefined (Int), "FadeOut" :: NullOrUndefined (Int), "Height" :: NullOrUndefined (Int), "ImageInserterInput" :: NullOrUndefined (String), "ImageX" :: NullOrUndefined (Int), "ImageY" :: NullOrUndefined (Int), "Layer" :: NullOrUndefined (Int), "Opacity" :: NullOrUndefined (Int), "StartTime" :: NullOrUndefined (String), "Width" :: NullOrUndefined (Int) }
```

Settings for Insertable Image

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "Message" :: NullOrUndefined (String) }
```

The service encountered an unexpected condition and cannot fulfill your request.

#### `Job`

``` purescript
newtype Job
  = Job { "Arn" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (Number), "ErrorCode" :: NullOrUndefined (Int), "ErrorMessage" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "JobTemplate" :: NullOrUndefined (String), "OutputGroupDetails" :: NullOrUndefined (ListOfOutputGroupDetail), "Queue" :: NullOrUndefined (String), "Role" :: NullOrUndefined (String), "Settings" :: NullOrUndefined (JobSettings), "Status" :: NullOrUndefined (JobStatus), "Timing" :: NullOrUndefined (Timing), "UserMetadata" :: NullOrUndefined (MapOf__string) }
```

Each job converts an input file into an output file or files. For more information, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html

#### `JobSettings`

``` purescript
newtype JobSettings
  = JobSettings { "AdAvailOffset" :: NullOrUndefined (Int), "AvailBlanking" :: NullOrUndefined (AvailBlanking), "Inputs" :: NullOrUndefined (ListOfInput), "NielsenConfiguration" :: NullOrUndefined (NielsenConfiguration), "OutputGroups" :: NullOrUndefined (ListOfOutputGroup), "TimecodeConfig" :: NullOrUndefined (TimecodeConfig), "TimedMetadataInsertion" :: NullOrUndefined (TimedMetadataInsertion) }
```

JobSettings contains all the transcode settings for a job.

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.

#### `JobTemplate`

``` purescript
newtype JobTemplate
  = JobTemplate { "Arn" :: NullOrUndefined (String), "Category" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (Number), "Description" :: NullOrUndefined (String), "LastUpdated" :: NullOrUndefined (Number), "Name" :: NullOrUndefined (String), "Queue" :: NullOrUndefined (String), "Settings" :: NullOrUndefined (JobTemplateSettings), "Type" :: NullOrUndefined (Type) }
```

A job template is a pre-made set of encoding instructions that you can use to quickly create a job.

#### `JobTemplateListBy`

``` purescript
newtype JobTemplateListBy
  = JobTemplateListBy String
```

Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.

#### `JobTemplateSettings`

``` purescript
newtype JobTemplateSettings
  = JobTemplateSettings { "AdAvailOffset" :: NullOrUndefined (Int), "AvailBlanking" :: NullOrUndefined (AvailBlanking), "Inputs" :: NullOrUndefined (ListOfInputTemplate), "NielsenConfiguration" :: NullOrUndefined (NielsenConfiguration), "OutputGroups" :: NullOrUndefined (ListOfOutputGroup), "TimecodeConfig" :: NullOrUndefined (TimecodeConfig), "TimedMetadataInsertion" :: NullOrUndefined (TimedMetadataInsertion) }
```

JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.

#### `LanguageCode`

``` purescript
newtype LanguageCode
  = LanguageCode String
```

Code to specify the language, following the specification "ISO 639-2 three-digit code":http://www.loc.gov/standards/iso639-2/

#### `ListJobTemplatesRequest`

``` purescript
newtype ListJobTemplatesRequest
  = ListJobTemplatesRequest { "Category" :: NullOrUndefined (String), "ListBy" :: NullOrUndefined (JobTemplateListBy), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Order" :: NullOrUndefined (Order) }
```

#### `ListJobTemplatesResponse`

``` purescript
newtype ListJobTemplatesResponse
  = ListJobTemplatesResponse { "JobTemplates" :: NullOrUndefined (ListOfJobTemplate), "NextToken" :: NullOrUndefined (String) }
```

#### `ListJobsRequest`

``` purescript
newtype ListJobsRequest
  = ListJobsRequest { "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Order" :: NullOrUndefined (Order), "Queue" :: NullOrUndefined (String), "Status" :: NullOrUndefined (JobStatus) }
```

#### `ListJobsResponse`

``` purescript
newtype ListJobsResponse
  = ListJobsResponse { "Jobs" :: NullOrUndefined (ListOfJob), "NextToken" :: NullOrUndefined (String) }
```

#### `ListOfAudioDescription`

``` purescript
newtype ListOfAudioDescription
  = ListOfAudioDescription (Array AudioDescription)
```

#### `ListOfCaptionDescription`

``` purescript
newtype ListOfCaptionDescription
  = ListOfCaptionDescription (Array CaptionDescription)
```

#### `ListOfCaptionDescriptionPreset`

``` purescript
newtype ListOfCaptionDescriptionPreset
  = ListOfCaptionDescriptionPreset (Array CaptionDescriptionPreset)
```

#### `ListOfEndpoint`

``` purescript
newtype ListOfEndpoint
  = ListOfEndpoint (Array Endpoint)
```

#### `ListOfHlsAdMarkers`

``` purescript
newtype ListOfHlsAdMarkers
  = ListOfHlsAdMarkers (Array HlsAdMarkers)
```

#### `ListOfHlsCaptionLanguageMapping`

``` purescript
newtype ListOfHlsCaptionLanguageMapping
  = ListOfHlsCaptionLanguageMapping (Array HlsCaptionLanguageMapping)
```

#### `ListOfId3Insertion`

``` purescript
newtype ListOfId3Insertion
  = ListOfId3Insertion (Array Id3Insertion)
```

#### `ListOfInput`

``` purescript
newtype ListOfInput
  = ListOfInput (Array Input)
```

#### `ListOfInputClipping`

``` purescript
newtype ListOfInputClipping
  = ListOfInputClipping (Array InputClipping)
```

#### `ListOfInputTemplate`

``` purescript
newtype ListOfInputTemplate
  = ListOfInputTemplate (Array InputTemplate)
```

#### `ListOfInsertableImage`

``` purescript
newtype ListOfInsertableImage
  = ListOfInsertableImage (Array InsertableImage)
```

#### `ListOfJob`

``` purescript
newtype ListOfJob
  = ListOfJob (Array Job)
```

#### `ListOfJobTemplate`

``` purescript
newtype ListOfJobTemplate
  = ListOfJobTemplate (Array JobTemplate)
```

#### `ListOfOutput`

``` purescript
newtype ListOfOutput
  = ListOfOutput (Array Output)
```

#### `ListOfOutputChannelMapping`

``` purescript
newtype ListOfOutputChannelMapping
  = ListOfOutputChannelMapping (Array OutputChannelMapping)
```

#### `ListOfOutputDetail`

``` purescript
newtype ListOfOutputDetail
  = ListOfOutputDetail (Array OutputDetail)
```

#### `ListOfOutputGroup`

``` purescript
newtype ListOfOutputGroup
  = ListOfOutputGroup (Array OutputGroup)
```

#### `ListOfOutputGroupDetail`

``` purescript
newtype ListOfOutputGroupDetail
  = ListOfOutputGroupDetail (Array OutputGroupDetail)
```

#### `ListOfPreset`

``` purescript
newtype ListOfPreset
  = ListOfPreset (Array Preset)
```

#### `ListOfQueue`

``` purescript
newtype ListOfQueue
  = ListOfQueue (Array Queue)
```

#### `ListOf__integer`

``` purescript
newtype ListOf__integer
  = ListOf__integer (Array Int)
```

#### `ListOf__string`

``` purescript
newtype ListOf__string
  = ListOf__string (Array String)
```

#### `ListPresetsRequest`

``` purescript
newtype ListPresetsRequest
  = ListPresetsRequest { "Category" :: NullOrUndefined (String), "ListBy" :: NullOrUndefined (PresetListBy), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Order" :: NullOrUndefined (Order) }
```

#### `ListPresetsResponse`

``` purescript
newtype ListPresetsResponse
  = ListPresetsResponse { "NextToken" :: NullOrUndefined (String), "Presets" :: NullOrUndefined (ListOfPreset) }
```

#### `ListQueuesRequest`

``` purescript
newtype ListQueuesRequest
  = ListQueuesRequest { "ListBy" :: NullOrUndefined (QueueListBy), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String), "Order" :: NullOrUndefined (Order) }
```

#### `ListQueuesResponse`

``` purescript
newtype ListQueuesResponse
  = ListQueuesResponse { "NextToken" :: NullOrUndefined (String), "Queues" :: NullOrUndefined (ListOfQueue) }
```

#### `M2tsAudioBufferModel`

``` purescript
newtype M2tsAudioBufferModel
  = M2tsAudioBufferModel String
```

Selects between the DVB and ATSC buffer models for Dolby Digital audio.

#### `M2tsBufferModel`

``` purescript
newtype M2tsBufferModel
  = M2tsBufferModel String
```

Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.

#### `M2tsEbpAudioInterval`

``` purescript
newtype M2tsEbpAudioInterval
  = M2tsEbpAudioInterval String
```

When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).

#### `M2tsEbpPlacement`

``` purescript
newtype M2tsEbpPlacement
  = M2tsEbpPlacement String
```

Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).

#### `M2tsEsRateInPes`

``` purescript
newtype M2tsEsRateInPes
  = M2tsEsRateInPes String
```

Controls whether to include the ES Rate field in the PES header.

#### `M2tsNielsenId3`

``` purescript
newtype M2tsNielsenId3
  = M2tsNielsenId3 String
```

If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.

#### `M2tsPcrControl`

``` purescript
newtype M2tsPcrControl
  = M2tsPcrControl String
```

When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.

#### `M2tsRateMode`

``` purescript
newtype M2tsRateMode
  = M2tsRateMode String
```

When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.

#### `M2tsScte35Source`

``` purescript
newtype M2tsScte35Source
  = M2tsScte35Source String
```

Enables SCTE-35 passthrough (scte35Source) to pass any SCTE-35 signals from input to output.

#### `M2tsSegmentationMarkers`

``` purescript
newtype M2tsSegmentationMarkers
  = M2tsSegmentationMarkers String
```

Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.

#### `M2tsSegmentationStyle`

``` purescript
newtype M2tsSegmentationStyle
  = M2tsSegmentationStyle String
```

The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.

#### `M2tsSettings`

``` purescript
newtype M2tsSettings
  = M2tsSettings { "AudioBufferModel" :: NullOrUndefined (M2tsAudioBufferModel), "AudioFramesPerPes" :: NullOrUndefined (Int), "AudioPids" :: NullOrUndefined (ListOf__integer), "Bitrate" :: NullOrUndefined (Int), "BufferModel" :: NullOrUndefined (M2tsBufferModel), "DvbNitSettings" :: NullOrUndefined (DvbNitSettings), "DvbSdtSettings" :: NullOrUndefined (DvbSdtSettings), "DvbSubPids" :: NullOrUndefined (ListOf__integer), "DvbTdtSettings" :: NullOrUndefined (DvbTdtSettings), "DvbTeletextPid" :: NullOrUndefined (Int), "EbpAudioInterval" :: NullOrUndefined (M2tsEbpAudioInterval), "EbpPlacement" :: NullOrUndefined (M2tsEbpPlacement), "EsRateInPes" :: NullOrUndefined (M2tsEsRateInPes), "FragmentTime" :: NullOrUndefined (Number), "MaxPcrInterval" :: NullOrUndefined (Int), "MinEbpInterval" :: NullOrUndefined (Int), "NielsenId3" :: NullOrUndefined (M2tsNielsenId3), "NullPacketBitrate" :: NullOrUndefined (Number), "PatInterval" :: NullOrUndefined (Int), "PcrControl" :: NullOrUndefined (M2tsPcrControl), "PcrPid" :: NullOrUndefined (Int), "PmtInterval" :: NullOrUndefined (Int), "PmtPid" :: NullOrUndefined (Int), "PrivateMetadataPid" :: NullOrUndefined (Int), "ProgramNumber" :: NullOrUndefined (Int), "RateMode" :: NullOrUndefined (M2tsRateMode), "Scte35Pid" :: NullOrUndefined (Int), "Scte35Source" :: NullOrUndefined (M2tsScte35Source), "SegmentationMarkers" :: NullOrUndefined (M2tsSegmentationMarkers), "SegmentationStyle" :: NullOrUndefined (M2tsSegmentationStyle), "SegmentationTime" :: NullOrUndefined (Number), "TimedMetadataPid" :: NullOrUndefined (Int), "TransportStreamId" :: NullOrUndefined (Int), "VideoPid" :: NullOrUndefined (Int) }
```

Settings for M2TS Container.

#### `M3u8NielsenId3`

``` purescript
newtype M3u8NielsenId3
  = M3u8NielsenId3 String
```

If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.

#### `M3u8PcrControl`

``` purescript
newtype M3u8PcrControl
  = M3u8PcrControl String
```

When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.

#### `M3u8Scte35Source`

``` purescript
newtype M3u8Scte35Source
  = M3u8Scte35Source String
```

Enables SCTE-35 passthrough (scte35Source) to pass any SCTE-35 signals from input to output.

#### `M3u8Settings`

``` purescript
newtype M3u8Settings
  = M3u8Settings { "AudioFramesPerPes" :: NullOrUndefined (Int), "AudioPids" :: NullOrUndefined (ListOf__integer), "NielsenId3" :: NullOrUndefined (M3u8NielsenId3), "PatInterval" :: NullOrUndefined (Int), "PcrControl" :: NullOrUndefined (M3u8PcrControl), "PcrPid" :: NullOrUndefined (Int), "PmtInterval" :: NullOrUndefined (Int), "PmtPid" :: NullOrUndefined (Int), "PrivateMetadataPid" :: NullOrUndefined (Int), "ProgramNumber" :: NullOrUndefined (Int), "Scte35Pid" :: NullOrUndefined (Int), "Scte35Source" :: NullOrUndefined (M3u8Scte35Source), "TimedMetadata" :: NullOrUndefined (TimedMetadata), "TimedMetadataPid" :: NullOrUndefined (Int), "TransportStreamId" :: NullOrUndefined (Int), "VideoPid" :: NullOrUndefined (Int) }
```

Settings for TS segments in HLS

#### `MapOfAudioSelector`

``` purescript
newtype MapOfAudioSelector
  = MapOfAudioSelector (Map String AudioSelector)
```

#### `MapOfAudioSelectorGroup`

``` purescript
newtype MapOfAudioSelectorGroup
  = MapOfAudioSelectorGroup (Map String AudioSelectorGroup)
```

#### `MapOfCaptionSelector`

``` purescript
newtype MapOfCaptionSelector
  = MapOfCaptionSelector (Map String CaptionSelector)
```

#### `MapOf__string`

``` purescript
newtype MapOf__string
  = MapOf__string (Map String String)
```

#### `MovClapAtom`

``` purescript
newtype MovClapAtom
  = MovClapAtom String
```

When enabled, include 'clap' atom if appropriate for the video output settings.

#### `MovCslgAtom`

``` purescript
newtype MovCslgAtom
  = MovCslgAtom String
```

When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.

#### `MovMpeg2FourCCControl`

``` purescript
newtype MovMpeg2FourCCControl
  = MovMpeg2FourCCControl String
```

When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.

#### `MovPaddingControl`

``` purescript
newtype MovPaddingControl
  = MovPaddingControl String
```

If set to OMNEON, inserts Omneon-compatible padding

#### `MovReference`

``` purescript
newtype MovReference
  = MovReference String
```

A value of 'external' creates separate media files and the wrapper file (.mov) contains references to these media files. A value of 'self_contained' creates only a wrapper (.mov) file and this file contains all of the media.

#### `MovSettings`

``` purescript
newtype MovSettings
  = MovSettings { "ClapAtom" :: NullOrUndefined (MovClapAtom), "CslgAtom" :: NullOrUndefined (MovCslgAtom), "Mpeg2FourCCControl" :: NullOrUndefined (MovMpeg2FourCCControl), "PaddingControl" :: NullOrUndefined (MovPaddingControl), "Reference" :: NullOrUndefined (MovReference) }
```

Settings for MOV Container.

#### `Mp2Settings`

``` purescript
newtype Mp2Settings
  = Mp2Settings { "Bitrate" :: NullOrUndefined (Int), "Channels" :: NullOrUndefined (Int), "SampleRate" :: NullOrUndefined (Int) }
```

Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.

#### `Mp4CslgAtom`

``` purescript
newtype Mp4CslgAtom
  = Mp4CslgAtom String
```

When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.

#### `Mp4FreeSpaceBox`

``` purescript
newtype Mp4FreeSpaceBox
  = Mp4FreeSpaceBox String
```

Inserts a free-space box immediately after the moov box.

#### `Mp4MoovPlacement`

``` purescript
newtype Mp4MoovPlacement
  = Mp4MoovPlacement String
```

If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.

#### `Mp4Settings`

``` purescript
newtype Mp4Settings
  = Mp4Settings { "CslgAtom" :: NullOrUndefined (Mp4CslgAtom), "FreeSpaceBox" :: NullOrUndefined (Mp4FreeSpaceBox), "MoovPlacement" :: NullOrUndefined (Mp4MoovPlacement), "Mp4MajorBrand" :: NullOrUndefined (String) }
```

Settings for MP4 Container

#### `Mpeg2AdaptiveQuantization`

``` purescript
newtype Mpeg2AdaptiveQuantization
  = Mpeg2AdaptiveQuantization String
```

Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.

#### `Mpeg2CodecLevel`

``` purescript
newtype Mpeg2CodecLevel
  = Mpeg2CodecLevel String
```

Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.

#### `Mpeg2CodecProfile`

``` purescript
newtype Mpeg2CodecProfile
  = Mpeg2CodecProfile String
```

Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.

#### `Mpeg2FramerateControl`

``` purescript
newtype Mpeg2FramerateControl
  = Mpeg2FramerateControl String
```

Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.

#### `Mpeg2FramerateConversionAlgorithm`

``` purescript
newtype Mpeg2FramerateConversionAlgorithm
  = Mpeg2FramerateConversionAlgorithm String
```

When set to INTERPOLATE, produces smoother motion during framerate conversion.

#### `Mpeg2GopSizeUnits`

``` purescript
newtype Mpeg2GopSizeUnits
  = Mpeg2GopSizeUnits String
```

Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.

#### `Mpeg2InterlaceMode`

``` purescript
newtype Mpeg2InterlaceMode
  = Mpeg2InterlaceMode String
```

Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.

#### `Mpeg2IntraDcPrecision`

``` purescript
newtype Mpeg2IntraDcPrecision
  = Mpeg2IntraDcPrecision String
```

Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.

#### `Mpeg2ParControl`

``` purescript
newtype Mpeg2ParControl
  = Mpeg2ParControl String
```

Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.

#### `Mpeg2QualityTuningLevel`

``` purescript
newtype Mpeg2QualityTuningLevel
  = Mpeg2QualityTuningLevel String
```

Use Quality tuning level (Mpeg2QualityTuningLevel) to specifiy whether to use single-pass or multipass video encoding.

#### `Mpeg2RateControlMode`

``` purescript
newtype Mpeg2RateControlMode
  = Mpeg2RateControlMode String
```

Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).

#### `Mpeg2SceneChangeDetect`

``` purescript
newtype Mpeg2SceneChangeDetect
  = Mpeg2SceneChangeDetect String
```

Scene change detection (inserts I-frames on scene changes).

#### `Mpeg2Settings`

``` purescript
newtype Mpeg2Settings
  = Mpeg2Settings { "AdaptiveQuantization" :: NullOrUndefined (Mpeg2AdaptiveQuantization), "Bitrate" :: NullOrUndefined (Int), "CodecLevel" :: NullOrUndefined (Mpeg2CodecLevel), "CodecProfile" :: NullOrUndefined (Mpeg2CodecProfile), "FramerateControl" :: NullOrUndefined (Mpeg2FramerateControl), "FramerateConversionAlgorithm" :: NullOrUndefined (Mpeg2FramerateConversionAlgorithm), "FramerateDenominator" :: NullOrUndefined (Int), "FramerateNumerator" :: NullOrUndefined (Int), "GopClosedCadence" :: NullOrUndefined (Int), "GopSize" :: NullOrUndefined (Number), "GopSizeUnits" :: NullOrUndefined (Mpeg2GopSizeUnits), "HrdBufferInitialFillPercentage" :: NullOrUndefined (Int), "HrdBufferSize" :: NullOrUndefined (Int), "InterlaceMode" :: NullOrUndefined (Mpeg2InterlaceMode), "IntraDcPrecision" :: NullOrUndefined (Mpeg2IntraDcPrecision), "MaxBitrate" :: NullOrUndefined (Int), "MinIInterval" :: NullOrUndefined (Int), "NumberBFramesBetweenReferenceFrames" :: NullOrUndefined (Int), "ParControl" :: NullOrUndefined (Mpeg2ParControl), "ParDenominator" :: NullOrUndefined (Int), "ParNumerator" :: NullOrUndefined (Int), "QualityTuningLevel" :: NullOrUndefined (Mpeg2QualityTuningLevel), "RateControlMode" :: NullOrUndefined (Mpeg2RateControlMode), "SceneChangeDetect" :: NullOrUndefined (Mpeg2SceneChangeDetect), "SlowPal" :: NullOrUndefined (Mpeg2SlowPal), "Softness" :: NullOrUndefined (Int), "SpatialAdaptiveQuantization" :: NullOrUndefined (Mpeg2SpatialAdaptiveQuantization), "Syntax" :: NullOrUndefined (Mpeg2Syntax), "Telecine" :: NullOrUndefined (Mpeg2Telecine), "TemporalAdaptiveQuantization" :: NullOrUndefined (Mpeg2TemporalAdaptiveQuantization) }
```

Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.

#### `Mpeg2SlowPal`

``` purescript
newtype Mpeg2SlowPal
  = Mpeg2SlowPal String
```

Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.

#### `Mpeg2SpatialAdaptiveQuantization`

``` purescript
newtype Mpeg2SpatialAdaptiveQuantization
  = Mpeg2SpatialAdaptiveQuantization String
```

Adjust quantization within each frame based on spatial variation of content complexity.

#### `Mpeg2Syntax`

``` purescript
newtype Mpeg2Syntax
  = Mpeg2Syntax String
```

Produces a Type D-10 compatible bitstream (SMPTE 356M-2001).

#### `Mpeg2Telecine`

``` purescript
newtype Mpeg2Telecine
  = Mpeg2Telecine String
```

Only use Telecine (Mpeg2Telecine) when you set Framerate (Framerate) to 29.970. Set Telecine (Mpeg2Telecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.

#### `Mpeg2TemporalAdaptiveQuantization`

``` purescript
newtype Mpeg2TemporalAdaptiveQuantization
  = Mpeg2TemporalAdaptiveQuantization String
```

Adjust quantization within each frame based on temporal variation of content complexity.

#### `MsSmoothAudioDeduplication`

``` purescript
newtype MsSmoothAudioDeduplication
  = MsSmoothAudioDeduplication String
```

COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.

#### `MsSmoothEncryptionSettings`

``` purescript
newtype MsSmoothEncryptionSettings
  = MsSmoothEncryptionSettings { "SpekeKeyProvider" :: NullOrUndefined (SpekeKeyProvider) }
```

If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.

#### `MsSmoothGroupSettings`

``` purescript
newtype MsSmoothGroupSettings
  = MsSmoothGroupSettings { "AudioDeduplication" :: NullOrUndefined (MsSmoothAudioDeduplication), "Destination" :: NullOrUndefined (String), "Encryption" :: NullOrUndefined (MsSmoothEncryptionSettings), "FragmentLength" :: NullOrUndefined (Int), "ManifestEncoding" :: NullOrUndefined (MsSmoothManifestEncoding) }
```

Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.

#### `MsSmoothManifestEncoding`

``` purescript
newtype MsSmoothManifestEncoding
  = MsSmoothManifestEncoding String
```

Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.

#### `NielsenConfiguration`

``` purescript
newtype NielsenConfiguration
  = NielsenConfiguration { "BreakoutCode" :: NullOrUndefined (Int), "DistributorId" :: NullOrUndefined (String) }
```

Settings for Nielsen Configuration

#### `NoiseReducer`

``` purescript
newtype NoiseReducer
  = NoiseReducer { "Filter" :: NullOrUndefined (NoiseReducerFilter), "FilterSettings" :: NullOrUndefined (NoiseReducerFilterSettings), "SpatialFilterSettings" :: NullOrUndefined (NoiseReducerSpatialFilterSettings) }
```

Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default. When you enable Noise reducer (NoiseReducer), you must also select a value for Noise reducer filter (NoiseReducerFilter).

#### `NoiseReducerFilter`

``` purescript
newtype NoiseReducerFilter
  = NoiseReducerFilter String
```

Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral is an edge preserving noise reduction filter * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) are convolution filters * Conserve is a min/max noise reduction filter * Spatial is frequency-domain filter based on JND principles.

#### `NoiseReducerFilterSettings`

``` purescript
newtype NoiseReducerFilterSettings
  = NoiseReducerFilterSettings { "Strength" :: NullOrUndefined (Int) }
```

Settings for a noise reducer filter

#### `NoiseReducerSpatialFilterSettings`

``` purescript
newtype NoiseReducerSpatialFilterSettings
  = NoiseReducerSpatialFilterSettings { "PostFilterSharpenStrength" :: NullOrUndefined (Int), "Speed" :: NullOrUndefined (Int), "Strength" :: NullOrUndefined (Int) }
```

Noise reducer filter settings for spatial filter.

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (String) }
```

The resource you requested does not exist.

#### `Order`

``` purescript
newtype Order
  = Order String
```

When you request lists of resources, you can optionally specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.

#### `Output`

``` purescript
newtype Output
  = Output { "AudioDescriptions" :: NullOrUndefined (ListOfAudioDescription), "CaptionDescriptions" :: NullOrUndefined (ListOfCaptionDescription), "ContainerSettings" :: NullOrUndefined (ContainerSettings), "Extension" :: NullOrUndefined (String), "NameModifier" :: NullOrUndefined (String), "OutputSettings" :: NullOrUndefined (OutputSettings), "Preset" :: NullOrUndefined (String), "VideoDescription" :: NullOrUndefined (VideoDescription) }
```

An output object describes the settings for a single output file or stream in an output group.

#### `OutputChannelMapping`

``` purescript
newtype OutputChannelMapping
  = OutputChannelMapping { "InputChannels" :: NullOrUndefined (ListOf__integer) }
```

OutputChannel mapping settings.

#### `OutputDetail`

``` purescript
newtype OutputDetail
  = OutputDetail { "DurationInMs" :: NullOrUndefined (Int), "VideoDetails" :: NullOrUndefined (VideoDetail) }
```

Details regarding output

#### `OutputGroup`

``` purescript
newtype OutputGroup
  = OutputGroup { "CustomName" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "OutputGroupSettings" :: NullOrUndefined (OutputGroupSettings), "Outputs" :: NullOrUndefined (ListOfOutput) }
```

Group of outputs

#### `OutputGroupDetail`

``` purescript
newtype OutputGroupDetail
  = OutputGroupDetail { "OutputDetails" :: NullOrUndefined (ListOfOutputDetail) }
```

Contains details about the output groups specified in the job settings.

#### `OutputGroupSettings`

``` purescript
newtype OutputGroupSettings
  = OutputGroupSettings { "DashIsoGroupSettings" :: NullOrUndefined (DashIsoGroupSettings), "FileGroupSettings" :: NullOrUndefined (FileGroupSettings), "HlsGroupSettings" :: NullOrUndefined (HlsGroupSettings), "MsSmoothGroupSettings" :: NullOrUndefined (MsSmoothGroupSettings), "Type" :: NullOrUndefined (OutputGroupType) }
```

Output Group settings, including type

#### `OutputGroupType`

``` purescript
newtype OutputGroupType
  = OutputGroupType String
```

Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming)

#### `OutputSdt`

``` purescript
newtype OutputSdt
  = OutputSdt String
```

Selects method of inserting SDT information into output stream.  "Follow input SDT" copies SDT information from input stream to  output stream. "Follow input SDT if present" copies SDT information from  input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. Enter "SDT  Manually" means user will enter the SDT information. "No SDT" means output  stream will not contain SDT information.

#### `OutputSettings`

``` purescript
newtype OutputSettings
  = OutputSettings { "HlsSettings" :: NullOrUndefined (HlsSettings) }
```

Specific settings for this type of output.

#### `Preset`

``` purescript
newtype Preset
  = Preset { "Arn" :: NullOrUndefined (String), "Category" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (Number), "Description" :: NullOrUndefined (String), "LastUpdated" :: NullOrUndefined (Number), "Name" :: NullOrUndefined (String), "Settings" :: NullOrUndefined (PresetSettings), "Type" :: NullOrUndefined (Type) }
```

A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.

#### `PresetListBy`

``` purescript
newtype PresetListBy
  = PresetListBy String
```

Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.

#### `PresetSettings`

``` purescript
newtype PresetSettings
  = PresetSettings { "AudioDescriptions" :: NullOrUndefined (ListOfAudioDescription), "CaptionDescriptions" :: NullOrUndefined (ListOfCaptionDescriptionPreset), "ContainerSettings" :: NullOrUndefined (ContainerSettings), "VideoDescription" :: NullOrUndefined (VideoDescription) }
```

Settings for preset

#### `ProresCodecProfile`

``` purescript
newtype ProresCodecProfile
  = ProresCodecProfile String
```

Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.

#### `ProresFramerateControl`

``` purescript
newtype ProresFramerateControl
  = ProresFramerateControl String
```

Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.

#### `ProresFramerateConversionAlgorithm`

``` purescript
newtype ProresFramerateConversionAlgorithm
  = ProresFramerateConversionAlgorithm String
```

When set to INTERPOLATE, produces smoother motion during framerate conversion.

#### `ProresInterlaceMode`

``` purescript
newtype ProresInterlaceMode
  = ProresInterlaceMode String
```

Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.

#### `ProresParControl`

``` purescript
newtype ProresParControl
  = ProresParControl String
```

Use (ProresParControl) to specify how the service determines the pixel aspect ratio. Set to Follow source (INITIALIZE_FROM_SOURCE) to use the pixel aspect ratio from the input.  To specify a different pixel aspect ratio: Using the console, choose it from the dropdown menu. Using the API, set ProresParControl to (SPECIFIED) and provide  for (ParNumerator) and (ParDenominator).

#### `ProresSettings`

``` purescript
newtype ProresSettings
  = ProresSettings { "CodecProfile" :: NullOrUndefined (ProresCodecProfile), "FramerateControl" :: NullOrUndefined (ProresFramerateControl), "FramerateConversionAlgorithm" :: NullOrUndefined (ProresFramerateConversionAlgorithm), "FramerateDenominator" :: NullOrUndefined (Int), "FramerateNumerator" :: NullOrUndefined (Int), "InterlaceMode" :: NullOrUndefined (ProresInterlaceMode), "ParControl" :: NullOrUndefined (ProresParControl), "ParDenominator" :: NullOrUndefined (Int), "ParNumerator" :: NullOrUndefined (Int), "SlowPal" :: NullOrUndefined (ProresSlowPal), "Telecine" :: NullOrUndefined (ProresTelecine) }
```

Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.

#### `ProresSlowPal`

``` purescript
newtype ProresSlowPal
  = ProresSlowPal String
```

Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.

#### `ProresTelecine`

``` purescript
newtype ProresTelecine
  = ProresTelecine String
```

Only use Telecine (ProresTelecine) when you set Framerate (Framerate) to 29.970. Set Telecine (ProresTelecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.

#### `Queue`

``` purescript
newtype Queue
  = Queue { "Arn" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (Number), "Description" :: NullOrUndefined (String), "LastUpdated" :: NullOrUndefined (Number), "Name" :: NullOrUndefined (String), "Status" :: NullOrUndefined (QueueStatus), "Type" :: NullOrUndefined (Type) }
```

MediaConvert jobs are submitted to a queue. Unless specified otherwise jobs are submitted to a built-in default queue. User can create additional queues to separate the jobs of different categories or priority.

#### `QueueListBy`

``` purescript
newtype QueueListBy
  = QueueListBy String
```

Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.

#### `QueueStatus`

``` purescript
newtype QueueStatus
  = QueueStatus String
```

Queues can be ACTIVE or PAUSED. If you pause a queue, jobs in that queue will not begin. Jobs running when a queue is paused continue to run until they finish or error out.

#### `Rectangle`

``` purescript
newtype Rectangle
  = Rectangle { "Height" :: NullOrUndefined (Int), "Width" :: NullOrUndefined (Int), "X" :: NullOrUndefined (Int), "Y" :: NullOrUndefined (Int) }
```

Use Rectangle to identify a specific area of the video frame.

#### `RemixSettings`

``` purescript
newtype RemixSettings
  = RemixSettings { "ChannelMapping" :: NullOrUndefined (ChannelMapping), "ChannelsIn" :: NullOrUndefined (Int), "ChannelsOut" :: NullOrUndefined (Int) }
```

Use Manual audio remixing (RemixSettings) to adjust audio levels for each output channel. With audio remixing, you can output more or fewer audio channels than your input audio source provides.

#### `RespondToAfd`

``` purescript
newtype RespondToAfd
  = RespondToAfd String
```

Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.

#### `ScalingBehavior`

``` purescript
newtype ScalingBehavior
  = ScalingBehavior String
```

Applies only if your input aspect ratio is different from your output aspect ratio. Enable Stretch to output (StretchToOutput) to have the service stretch your video image to fit. Leave this setting disabled to allow the service to letterbox your video instead. This setting overrides any positioning value you specify elsewhere in the job.

#### `SccDestinationFramerate`

``` purescript
newtype SccDestinationFramerate
  = SccDestinationFramerate String
```

Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a framerate that matches the framerate of the associated video. If the video framerate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).

#### `SccDestinationSettings`

``` purescript
newtype SccDestinationSettings
  = SccDestinationSettings { "Framerate" :: NullOrUndefined (SccDestinationFramerate) }
```

Settings for SCC caption output.

#### `SpekeKeyProvider`

``` purescript
newtype SpekeKeyProvider
  = SpekeKeyProvider { "ResourceId" :: NullOrUndefined (String), "SystemIds" :: NullOrUndefined (ListOf__string), "Url" :: NullOrUndefined (String) }
```

Settings for use with a SPEKE key provider

#### `StaticKeyProvider`

``` purescript
newtype StaticKeyProvider
  = StaticKeyProvider { "KeyFormat" :: NullOrUndefined (String), "KeyFormatVersions" :: NullOrUndefined (String), "StaticKeyValue" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

Settings for use with a SPEKE key provider.

#### `TeletextDestinationSettings`

``` purescript
newtype TeletextDestinationSettings
  = TeletextDestinationSettings { "PageNumber" :: NullOrUndefined (String) }
```

Settings for Teletext caption output

#### `TeletextSourceSettings`

``` purescript
newtype TeletextSourceSettings
  = TeletextSourceSettings { "PageNumber" :: NullOrUndefined (String) }
```

Settings specific to Teletext caption sources, including Page number.

#### `TimecodeBurnin`

``` purescript
newtype TimecodeBurnin
  = TimecodeBurnin { "FontSize" :: NullOrUndefined (Int), "Position" :: NullOrUndefined (TimecodeBurninPosition), "Prefix" :: NullOrUndefined (String) }
```

Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.

#### `TimecodeBurninPosition`

``` purescript
newtype TimecodeBurninPosition
  = TimecodeBurninPosition String
```

Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.

#### `TimecodeConfig`

``` purescript
newtype TimecodeConfig
  = TimecodeConfig { "Anchor" :: NullOrUndefined (String), "Source" :: NullOrUndefined (TimecodeSource), "Start" :: NullOrUndefined (String), "TimestampOffset" :: NullOrUndefined (String) }
```

Contains settings used to acquire and adjust timecode information from inputs.

#### `TimecodeSource`

``` purescript
newtype TimecodeSource
  = TimecodeSource String
```

Use Timecode source (TimecodeSource) to set how timecodes are handled within this input. To make sure that your video, audio, captions, and markers are synchronized and that time-based features, such as image inserter, work correctly, choose the Timecode source option that matches your assets. All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). * Embedded (EMBEDDED) - Use the timecode that is in the input video. If no embedded timecode is in the source, the service will use Start at 0 (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set the timecode of the initial frame to a value other than zero. You use Start timecode (Start) to provide this value.

#### `TimedMetadata`

``` purescript
newtype TimedMetadata
  = TimedMetadata String
```

If PASSTHROUGH, inserts ID3 timed metadata from the timed_metadata REST command into this output.

#### `TimedMetadataInsertion`

``` purescript
newtype TimedMetadataInsertion
  = TimedMetadataInsertion { "Id3Insertions" :: NullOrUndefined (ListOfId3Insertion) }
```

Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in your job. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.

#### `Timing`

``` purescript
newtype Timing
  = Timing { "FinishTime" :: NullOrUndefined (Number), "StartTime" :: NullOrUndefined (Number), "SubmitTime" :: NullOrUndefined (Number) }
```

Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String) }
```

Too many requests have been sent in too short of a time. The service limits the rate at which it will accept requests.

#### `TtmlDestinationSettings`

``` purescript
newtype TtmlDestinationSettings
  = TtmlDestinationSettings { "StylePassthrough" :: NullOrUndefined (TtmlStylePassthrough) }
```

Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).

#### `TtmlStylePassthrough`

``` purescript
newtype TtmlStylePassthrough
  = TtmlStylePassthrough String
```

Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.

#### `Type`

``` purescript
newtype Type
  = Type String
```

#### `UpdateJobTemplateRequest`

``` purescript
newtype UpdateJobTemplateRequest
  = UpdateJobTemplateRequest { "Category" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Name" :: String, "Queue" :: NullOrUndefined (String), "Settings" :: NullOrUndefined (JobTemplateSettings) }
```

#### `UpdateJobTemplateResponse`

``` purescript
newtype UpdateJobTemplateResponse
  = UpdateJobTemplateResponse { "JobTemplate" :: NullOrUndefined (JobTemplate) }
```

#### `UpdatePresetRequest`

``` purescript
newtype UpdatePresetRequest
  = UpdatePresetRequest { "Category" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Name" :: String, "Settings" :: NullOrUndefined (PresetSettings) }
```

#### `UpdatePresetResponse`

``` purescript
newtype UpdatePresetResponse
  = UpdatePresetResponse { "Preset" :: NullOrUndefined (Preset) }
```

#### `UpdateQueueRequest`

``` purescript
newtype UpdateQueueRequest
  = UpdateQueueRequest { "Description" :: NullOrUndefined (String), "Name" :: String, "Status" :: NullOrUndefined (QueueStatus) }
```

#### `UpdateQueueResponse`

``` purescript
newtype UpdateQueueResponse
  = UpdateQueueResponse { "Queue" :: NullOrUndefined (Queue) }
```

#### `VideoCodec`

``` purescript
newtype VideoCodec
  = VideoCodec String
```

Type of video codec

#### `VideoCodecSettings`

``` purescript
newtype VideoCodecSettings
  = VideoCodecSettings { "Codec" :: NullOrUndefined (VideoCodec), "FrameCaptureSettings" :: NullOrUndefined (FrameCaptureSettings), "H264Settings" :: NullOrUndefined (H264Settings), "H265Settings" :: NullOrUndefined (H265Settings), "Mpeg2Settings" :: NullOrUndefined (Mpeg2Settings), "ProresSettings" :: NullOrUndefined (ProresSettings) }
```

Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value you choose for Video codec (Codec). For each codec enum you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * FRAME_CAPTURE, FrameCaptureSettings

#### `VideoDescription`

``` purescript
newtype VideoDescription
  = VideoDescription { "AfdSignaling" :: NullOrUndefined (AfdSignaling), "AntiAlias" :: NullOrUndefined (AntiAlias), "CodecSettings" :: NullOrUndefined (VideoCodecSettings), "ColorMetadata" :: NullOrUndefined (ColorMetadata), "Crop" :: NullOrUndefined (Rectangle), "DropFrameTimecode" :: NullOrUndefined (DropFrameTimecode), "FixedAfd" :: NullOrUndefined (Int), "Height" :: NullOrUndefined (Int), "Position" :: NullOrUndefined (Rectangle), "RespondToAfd" :: NullOrUndefined (RespondToAfd), "ScalingBehavior" :: NullOrUndefined (ScalingBehavior), "Sharpness" :: NullOrUndefined (Int), "TimecodeInsertion" :: NullOrUndefined (VideoTimecodeInsertion), "VideoPreprocessors" :: NullOrUndefined (VideoPreprocessor), "Width" :: NullOrUndefined (Int) }
```

Settings for video outputs

#### `VideoDetail`

``` purescript
newtype VideoDetail
  = VideoDetail { "HeightInPx" :: NullOrUndefined (Int), "WidthInPx" :: NullOrUndefined (Int) }
```

Contains details about the output's video stream

#### `VideoPreprocessor`

``` purescript
newtype VideoPreprocessor
  = VideoPreprocessor { "ColorCorrector" :: NullOrUndefined (ColorCorrector), "Deinterlacer" :: NullOrUndefined (Deinterlacer), "ImageInserter" :: NullOrUndefined (ImageInserter), "NoiseReducer" :: NullOrUndefined (NoiseReducer), "TimecodeBurnin" :: NullOrUndefined (TimecodeBurnin) }
```

Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.

#### `VideoSelector`

``` purescript
newtype VideoSelector
  = VideoSelector { "ColorSpace" :: NullOrUndefined (ColorSpace), "ColorSpaceUsage" :: NullOrUndefined (ColorSpaceUsage), "Hdr10Metadata" :: NullOrUndefined (Hdr10Metadata), "Pid" :: NullOrUndefined (Int), "ProgramNumber" :: NullOrUndefined (Int) }
```

Selector for video.

#### `VideoTimecodeInsertion`

``` purescript
newtype VideoTimecodeInsertion
  = VideoTimecodeInsertion String
```

Enable Timecode insertion to include timecode information in this output. Do this in the API by setting (VideoTimecodeInsertion) to (PIC_TIMING_SEI). To get timecodes to appear correctly in your output, also set up the timecode configuration for your job in the input settings. Only enable Timecode insertion when the input framerate is identical to output framerate. Disable this setting to remove the timecode from the output. Default is disabled.

#### `WavSettings`

``` purescript
newtype WavSettings
  = WavSettings { "BitDepth" :: NullOrUndefined (Int), "Channels" :: NullOrUndefined (Int), "SampleRate" :: NullOrUndefined (Int) }
```

Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.


