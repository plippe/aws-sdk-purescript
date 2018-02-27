

-- | AWS Elemental MediaConvert
module AWS.MediaConvert where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MediaConvert" :: String


-- | Permanently remove a job from a queue. Once you have canceled a job, you can't start it again. You can't delete a running job.
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: AWS.RequestError | eff) CancelJobResponse
cancelJob = AWS.request serviceName "CancelJob" 


-- | Create a new transcoding job. For information about jobs and job settings, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
createJob :: forall eff. CreateJobRequest -> Aff (err :: AWS.RequestError | eff) CreateJobResponse
createJob = AWS.request serviceName "CreateJob" 


-- | Create a new job template. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
createJobTemplate :: forall eff. CreateJobTemplateRequest -> Aff (err :: AWS.RequestError | eff) CreateJobTemplateResponse
createJobTemplate = AWS.request serviceName "CreateJobTemplate" 


-- | Create a new preset. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
createPreset :: forall eff. CreatePresetRequest -> Aff (err :: AWS.RequestError | eff) CreatePresetResponse
createPreset = AWS.request serviceName "CreatePreset" 


-- | Create a new transcoding queue. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
createQueue :: forall eff. CreateQueueRequest -> Aff (err :: AWS.RequestError | eff) CreateQueueResponse
createQueue = AWS.request serviceName "CreateQueue" 


-- | Permanently delete a job template you have created.
deleteJobTemplate :: forall eff. DeleteJobTemplateRequest -> Aff (err :: AWS.RequestError | eff) DeleteJobTemplateResponse
deleteJobTemplate = AWS.request serviceName "DeleteJobTemplate" 


-- | Permanently delete a preset you have created.
deletePreset :: forall eff. DeletePresetRequest -> Aff (err :: AWS.RequestError | eff) DeletePresetResponse
deletePreset = AWS.request serviceName "DeletePreset" 


-- | Permanently delete a queue you have created.
deleteQueue :: forall eff. DeleteQueueRequest -> Aff (err :: AWS.RequestError | eff) DeleteQueueResponse
deleteQueue = AWS.request serviceName "DeleteQueue" 


-- | Send an request with an empty body to the regional API endpoint to get your account API endpoint.
describeEndpoints :: forall eff. DescribeEndpointsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEndpointsResponse
describeEndpoints = AWS.request serviceName "DescribeEndpoints" 


-- | Retrieve the JSON for a specific completed transcoding job.
getJob :: forall eff. GetJobRequest -> Aff (err :: AWS.RequestError | eff) GetJobResponse
getJob = AWS.request serviceName "GetJob" 


-- | Retrieve the JSON for a specific job template.
getJobTemplate :: forall eff. GetJobTemplateRequest -> Aff (err :: AWS.RequestError | eff) GetJobTemplateResponse
getJobTemplate = AWS.request serviceName "GetJobTemplate" 


-- | Retrieve the JSON for a specific preset.
getPreset :: forall eff. GetPresetRequest -> Aff (err :: AWS.RequestError | eff) GetPresetResponse
getPreset = AWS.request serviceName "GetPreset" 


-- | Retrieve the JSON for a specific queue.
getQueue :: forall eff. GetQueueRequest -> Aff (err :: AWS.RequestError | eff) GetQueueResponse
getQueue = AWS.request serviceName "GetQueue" 


-- | Retrieve a JSON array of up to twenty of your job templates. This will return the templates themselves, not just a list of them. To retrieve the next twenty templates, use the nextToken string returned with the array
listJobTemplates :: forall eff. ListJobTemplatesRequest -> Aff (err :: AWS.RequestError | eff) ListJobTemplatesResponse
listJobTemplates = AWS.request serviceName "ListJobTemplates" 


-- | Retrieve a JSON array of up to twenty of your most recently created jobs. This array includes in-process, completed, and errored jobs. This will return the jobs themselves, not just a list of the jobs. To retrieve the twenty next most recent jobs, use the nextToken string returned with the array.
listJobs :: forall eff. ListJobsRequest -> Aff (err :: AWS.RequestError | eff) ListJobsResponse
listJobs = AWS.request serviceName "ListJobs" 


-- | Retrieve a JSON array of up to twenty of your presets. This will return the presets themselves, not just a list of them. To retrieve the next twenty presets, use the nextToken string returned with the array.
listPresets :: forall eff. ListPresetsRequest -> Aff (err :: AWS.RequestError | eff) ListPresetsResponse
listPresets = AWS.request serviceName "ListPresets" 


-- | Retrieve a JSON array of up to twenty of your queues. This will return the queues themselves, not just a list of them. To retrieve the next twenty queues, use the nextToken string returned with the array.
listQueues :: forall eff. ListQueuesRequest -> Aff (err :: AWS.RequestError | eff) ListQueuesResponse
listQueues = AWS.request serviceName "ListQueues" 


-- | Modify one of your existing job templates.
updateJobTemplate :: forall eff. UpdateJobTemplateRequest -> Aff (err :: AWS.RequestError | eff) UpdateJobTemplateResponse
updateJobTemplate = AWS.request serviceName "UpdateJobTemplate" 


-- | Modify one of your existing presets.
updatePreset :: forall eff. UpdatePresetRequest -> Aff (err :: AWS.RequestError | eff) UpdatePresetResponse
updatePreset = AWS.request serviceName "UpdatePreset" 


-- | Modify one of your existing queues.
updateQueue :: forall eff. UpdateQueueRequest -> Aff (err :: AWS.RequestError | eff) UpdateQueueResponse
updateQueue = AWS.request serviceName "UpdateQueue" 


-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
newtype AacAudioDescriptionBroadcasterMix = AacAudioDescriptionBroadcasterMix String
derive instance newtypeAacAudioDescriptionBroadcasterMix :: Newtype AacAudioDescriptionBroadcasterMix _


-- | AAC Profile.
newtype AacCodecProfile = AacCodecProfile String
derive instance newtypeAacCodecProfile :: Newtype AacCodecProfile _


-- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
newtype AacCodingMode = AacCodingMode String
derive instance newtypeAacCodingMode :: Newtype AacCodingMode _


-- | Rate Control Mode.
newtype AacRateControlMode = AacRateControlMode String
derive instance newtypeAacRateControlMode :: Newtype AacRateControlMode _


-- | Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
newtype AacRawFormat = AacRawFormat String
derive instance newtypeAacRawFormat :: Newtype AacRawFormat _


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC.
newtype AacSettings = AacSettings 
  { "AudioDescriptionBroadcasterMix" :: NullOrUndefined (AacAudioDescriptionBroadcasterMix)
  , "Bitrate" :: NullOrUndefined (Int)
  , "CodecProfile" :: NullOrUndefined (AacCodecProfile)
  , "CodingMode" :: NullOrUndefined (AacCodingMode)
  , "RateControlMode" :: NullOrUndefined (AacRateControlMode)
  , "RawFormat" :: NullOrUndefined (AacRawFormat)
  , "SampleRate" :: NullOrUndefined (Int)
  , "Specification" :: NullOrUndefined (AacSpecification)
  , "VbrQuality" :: NullOrUndefined (AacVbrQuality)
  }
derive instance newtypeAacSettings :: Newtype AacSettings _


-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
newtype AacSpecification = AacSpecification String
derive instance newtypeAacSpecification :: Newtype AacSpecification _


-- | VBR Quality Level - Only used if rate_control_mode is VBR.
newtype AacVbrQuality = AacVbrQuality String
derive instance newtypeAacVbrQuality :: Newtype AacVbrQuality _


-- | Specifies the "Bitstream Mode" (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
newtype Ac3BitstreamMode = Ac3BitstreamMode String
derive instance newtypeAc3BitstreamMode :: Newtype Ac3BitstreamMode _


-- | Dolby Digital coding mode. Determines number of channels.
newtype Ac3CodingMode = Ac3CodingMode String
derive instance newtypeAc3CodingMode :: Newtype Ac3CodingMode _


-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
newtype Ac3DynamicRangeCompressionProfile = Ac3DynamicRangeCompressionProfile String
derive instance newtypeAc3DynamicRangeCompressionProfile :: Newtype Ac3DynamicRangeCompressionProfile _


-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
newtype Ac3LfeFilter = Ac3LfeFilter String
derive instance newtypeAc3LfeFilter :: Newtype Ac3LfeFilter _


-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
newtype Ac3MetadataControl = Ac3MetadataControl String
derive instance newtypeAc3MetadataControl :: Newtype Ac3MetadataControl _


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
newtype Ac3Settings = Ac3Settings 
  { "Bitrate" :: NullOrUndefined (Int)
  , "BitstreamMode" :: NullOrUndefined (Ac3BitstreamMode)
  , "CodingMode" :: NullOrUndefined (Ac3CodingMode)
  , "Dialnorm" :: NullOrUndefined (Int)
  , "DynamicRangeCompressionProfile" :: NullOrUndefined (Ac3DynamicRangeCompressionProfile)
  , "LfeFilter" :: NullOrUndefined (Ac3LfeFilter)
  , "MetadataControl" :: NullOrUndefined (Ac3MetadataControl)
  , "SampleRate" :: NullOrUndefined (Int)
  }
derive instance newtypeAc3Settings :: Newtype Ac3Settings _


-- | This setting only applies to H.264 and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to whether there are AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
newtype AfdSignaling = AfdSignaling String
derive instance newtypeAfdSignaling :: Newtype AfdSignaling _


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
newtype AiffSettings = AiffSettings 
  { "BitDepth" :: NullOrUndefined (Int)
  , "Channels" :: NullOrUndefined (Int)
  , "SampleRate" :: NullOrUndefined (Int)
  }
derive instance newtypeAiffSettings :: Newtype AiffSettings _


-- | Settings for ancillary captions source.
newtype AncillarySourceSettings = AncillarySourceSettings 
  { "SourceAncillaryChannelNumber" :: NullOrUndefined (Int)
  }
derive instance newtypeAncillarySourceSettings :: Newtype AncillarySourceSettings _


-- | Enable Anti-alias (AntiAlias) to enhance sharp edges in video output when your input resolution is much larger than your output resolution. Default is enabled.
newtype AntiAlias = AntiAlias String
derive instance newtypeAntiAlias :: Newtype AntiAlias _


-- | Type of Audio codec.
newtype AudioCodec = AudioCodec String
derive instance newtypeAudioCodec :: Newtype AudioCodec _


-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value you choose for Audio codec (Codec). For each codec enum you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings
newtype AudioCodecSettings = AudioCodecSettings 
  { "AacSettings" :: NullOrUndefined (AacSettings)
  , "Ac3Settings" :: NullOrUndefined (Ac3Settings)
  , "AiffSettings" :: NullOrUndefined (AiffSettings)
  , "Codec" :: NullOrUndefined (AudioCodec)
  , "Eac3Settings" :: NullOrUndefined (Eac3Settings)
  , "Mp2Settings" :: NullOrUndefined (Mp2Settings)
  , "WavSettings" :: NullOrUndefined (WavSettings)
  }
derive instance newtypeAudioCodecSettings :: Newtype AudioCodecSettings _


-- | When an "Audio Description":#audio_description specifies an AudioSelector or AudioSelectorGroup  for which no matching source is found in the input, then the audio selector marked as DEFAULT will be used.  If none are marked as default, silence will be inserted for the duration of the input.
newtype AudioDefaultSelection = AudioDefaultSelection String
derive instance newtypeAudioDefaultSelection :: Newtype AudioDefaultSelection _


-- | Description of audio output
newtype AudioDescription = AudioDescription 
  { "AudioNormalizationSettings" :: NullOrUndefined (AudioNormalizationSettings)
  , "AudioSourceName" :: NullOrUndefined (String)
  , "AudioType" :: NullOrUndefined (Int)
  , "AudioTypeControl" :: NullOrUndefined (AudioTypeControl)
  , "CodecSettings" :: NullOrUndefined (AudioCodecSettings)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "LanguageCodeControl" :: NullOrUndefined (AudioLanguageCodeControl)
  , "RemixSettings" :: NullOrUndefined (RemixSettings)
  , "StreamName" :: NullOrUndefined (String)
  }
derive instance newtypeAudioDescription :: Newtype AudioDescription _


-- | Choosing FOLLOW_INPUT will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The language specified for languageCode' will be used when USE_CONFIGURED is selected or when FOLLOW_INPUT is selected but there is no ISO 639 language code specified by the input.
newtype AudioLanguageCodeControl = AudioLanguageCodeControl String
derive instance newtypeAudioLanguageCodeControl :: Newtype AudioLanguageCodeControl _


-- | Audio normalization algorithm to use. 1770-1 conforms to the CALM Act specification, 1770-2 conforms to the EBU R-128 specification.
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm String
derive instance newtypeAudioNormalizationAlgorithm :: Newtype AudioNormalizationAlgorithm _


-- | When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
newtype AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl String
derive instance newtypeAudioNormalizationAlgorithmControl :: Newtype AudioNormalizationAlgorithmControl _


-- | If set to LOG, log each output's audio track loudness to a CSV file.
newtype AudioNormalizationLoudnessLogging = AudioNormalizationLoudnessLogging String
derive instance newtypeAudioNormalizationLoudnessLogging :: Newtype AudioNormalizationLoudnessLogging _


-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
newtype AudioNormalizationPeakCalculation = AudioNormalizationPeakCalculation String
derive instance newtypeAudioNormalizationPeakCalculation :: Newtype AudioNormalizationPeakCalculation _


-- | Advanced audio normalization settings.
newtype AudioNormalizationSettings = AudioNormalizationSettings 
  { "Algorithm" :: NullOrUndefined (AudioNormalizationAlgorithm)
  , "AlgorithmControl" :: NullOrUndefined (AudioNormalizationAlgorithmControl)
  , "CorrectionGateLevel" :: NullOrUndefined (Int)
  , "LoudnessLogging" :: NullOrUndefined (AudioNormalizationLoudnessLogging)
  , "PeakCalculation" :: NullOrUndefined (AudioNormalizationPeakCalculation)
  , "TargetLkfs" :: NullOrUndefined (Number)
  }
derive instance newtypeAudioNormalizationSettings :: Newtype AudioNormalizationSettings _


-- | Selector for Audio
newtype AudioSelector = AudioSelector 
  { "DefaultSelection" :: NullOrUndefined (AudioDefaultSelection)
  , "ExternalAudioFileInput" :: NullOrUndefined (String)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "Offset" :: NullOrUndefined (Int)
  , "Pids" :: NullOrUndefined (ListOf__integer)
  , "ProgramSelection" :: NullOrUndefined (Int)
  , "RemixSettings" :: NullOrUndefined (RemixSettings)
  , "SelectorType" :: NullOrUndefined (AudioSelectorType)
  , "Tracks" :: NullOrUndefined (ListOf__integer)
  }
derive instance newtypeAudioSelector :: Newtype AudioSelector _


-- | Group of Audio Selectors
newtype AudioSelectorGroup = AudioSelectorGroup 
  { "AudioSelectorNames" :: NullOrUndefined (ListOf__string)
  }
derive instance newtypeAudioSelectorGroup :: Newtype AudioSelectorGroup _


-- | Specifies the type of the audio selector.
newtype AudioSelectorType = AudioSelectorType String
derive instance newtypeAudioSelectorType :: Newtype AudioSelectorType _


-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
newtype AudioTypeControl = AudioTypeControl String
derive instance newtypeAudioTypeControl :: Newtype AudioTypeControl _


-- | Settings for Avail Blanking
newtype AvailBlanking = AvailBlanking 
  { "AvailBlankingImage" :: NullOrUndefined (String)
  }
derive instance newtypeAvailBlanking :: Newtype AvailBlanking _


-- | The service can't process your request because of a problem in the request. Please check your request form and syntax.
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


-- | Burn-In Destination Settings.
newtype BurninDestinationSettings = BurninDestinationSettings 
  { "Alignment" :: NullOrUndefined (BurninSubtitleAlignment)
  , "BackgroundColor" :: NullOrUndefined (BurninSubtitleBackgroundColor)
  , "BackgroundOpacity" :: NullOrUndefined (Int)
  , "FontColor" :: NullOrUndefined (BurninSubtitleFontColor)
  , "FontOpacity" :: NullOrUndefined (Int)
  , "FontResolution" :: NullOrUndefined (Int)
  , "FontSize" :: NullOrUndefined (Int)
  , "OutlineColor" :: NullOrUndefined (BurninSubtitleOutlineColor)
  , "OutlineSize" :: NullOrUndefined (Int)
  , "ShadowColor" :: NullOrUndefined (BurninSubtitleShadowColor)
  , "ShadowOpacity" :: NullOrUndefined (Int)
  , "ShadowXOffset" :: NullOrUndefined (Int)
  , "ShadowYOffset" :: NullOrUndefined (Int)
  , "TeletextSpacing" :: NullOrUndefined (BurninSubtitleTeletextSpacing)
  , "XPosition" :: NullOrUndefined (Int)
  , "YPosition" :: NullOrUndefined (Int)
  }
derive instance newtypeBurninDestinationSettings :: Newtype BurninDestinationSettings _


-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleAlignment = BurninSubtitleAlignment String
derive instance newtypeBurninSubtitleAlignment :: Newtype BurninSubtitleAlignment _


-- | Specifies the color of the rectangle behind the captions.
-- | All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleBackgroundColor = BurninSubtitleBackgroundColor String
derive instance newtypeBurninSubtitleBackgroundColor :: Newtype BurninSubtitleBackgroundColor _


-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleFontColor = BurninSubtitleFontColor String
derive instance newtypeBurninSubtitleFontColor :: Newtype BurninSubtitleFontColor _


-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleOutlineColor = BurninSubtitleOutlineColor String
derive instance newtypeBurninSubtitleOutlineColor :: Newtype BurninSubtitleOutlineColor _


-- | Specifies the color of the shadow cast by the captions.
-- | All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleShadowColor = BurninSubtitleShadowColor String
derive instance newtypeBurninSubtitleShadowColor :: Newtype BurninSubtitleShadowColor _


-- | Controls whether a fixed grid size or proportional font spacing will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
newtype BurninSubtitleTeletextSpacing = BurninSubtitleTeletextSpacing String
derive instance newtypeBurninSubtitleTeletextSpacing :: Newtype BurninSubtitleTeletextSpacing _


newtype CancelJobRequest = CancelJobRequest 
  { "Id" :: (String)
  }
derive instance newtypeCancelJobRequest :: Newtype CancelJobRequest _


newtype CancelJobResponse = CancelJobResponse 
  { 
  }
derive instance newtypeCancelJobResponse :: Newtype CancelJobResponse _


-- | Description of Caption output
newtype CaptionDescription = CaptionDescription 
  { "CaptionSelectorName" :: NullOrUndefined (String)
  , "DestinationSettings" :: NullOrUndefined (CaptionDestinationSettings)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "LanguageDescription" :: NullOrUndefined (String)
  }
derive instance newtypeCaptionDescription :: Newtype CaptionDescription _


-- | Caption Description for preset
newtype CaptionDescriptionPreset = CaptionDescriptionPreset 
  { "DestinationSettings" :: NullOrUndefined (CaptionDestinationSettings)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "LanguageDescription" :: NullOrUndefined (String)
  }
derive instance newtypeCaptionDescriptionPreset :: Newtype CaptionDescriptionPreset _


-- | Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
newtype CaptionDestinationSettings = CaptionDestinationSettings 
  { "BurninDestinationSettings" :: NullOrUndefined (BurninDestinationSettings)
  , "DestinationType" :: NullOrUndefined (CaptionDestinationType)
  , "DvbSubDestinationSettings" :: NullOrUndefined (DvbSubDestinationSettings)
  , "SccDestinationSettings" :: NullOrUndefined (SccDestinationSettings)
  , "TeletextDestinationSettings" :: NullOrUndefined (TeletextDestinationSettings)
  , "TtmlDestinationSettings" :: NullOrUndefined (TtmlDestinationSettings)
  }
derive instance newtypeCaptionDestinationSettings :: Newtype CaptionDestinationSettings _


-- | Type of Caption output, including Burn-In, Embedded, SCC, SRT, TTML, WebVTT, DVB-Sub, Teletext.
newtype CaptionDestinationType = CaptionDestinationType String
derive instance newtypeCaptionDestinationType :: Newtype CaptionDestinationType _


-- | Caption inputs to be mapped to caption outputs.
newtype CaptionSelector = CaptionSelector 
  { "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "SourceSettings" :: NullOrUndefined (CaptionSourceSettings)
  }
derive instance newtypeCaptionSelector :: Newtype CaptionSelector _


-- | Source settings (SourceSettings) contains the group of settings for captions in the input.
newtype CaptionSourceSettings = CaptionSourceSettings 
  { "AncillarySourceSettings" :: NullOrUndefined (AncillarySourceSettings)
  , "DvbSubSourceSettings" :: NullOrUndefined (DvbSubSourceSettings)
  , "EmbeddedSourceSettings" :: NullOrUndefined (EmbeddedSourceSettings)
  , "FileSourceSettings" :: NullOrUndefined (FileSourceSettings)
  , "SourceType" :: NullOrUndefined (CaptionSourceType)
  , "TeletextSourceSettings" :: NullOrUndefined (TeletextSourceSettings)
  }
derive instance newtypeCaptionSourceSettings :: Newtype CaptionSourceSettings _


-- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
newtype CaptionSourceType = CaptionSourceType String
derive instance newtypeCaptionSourceType :: Newtype CaptionSourceType _


-- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
newtype ChannelMapping = ChannelMapping 
  { "OutputChannels" :: NullOrUndefined (ListOfOutputChannelMapping)
  }
derive instance newtypeChannelMapping :: Newtype ChannelMapping _


-- | Settings for color correction.
newtype ColorCorrector = ColorCorrector 
  { "Brightness" :: NullOrUndefined (Int)
  , "ColorSpaceConversion" :: NullOrUndefined (ColorSpaceConversion)
  , "Contrast" :: NullOrUndefined (Int)
  , "Hdr10Metadata" :: NullOrUndefined (Hdr10Metadata)
  , "Hue" :: NullOrUndefined (Int)
  , "Saturation" :: NullOrUndefined (Int)
  }
derive instance newtypeColorCorrector :: Newtype ColorCorrector _


-- | Enable Insert color metadata (ColorMetadata) to include color metadata in this output. This setting is enabled by default.
newtype ColorMetadata = ColorMetadata String
derive instance newtypeColorMetadata :: Newtype ColorMetadata _


-- | Specifies the colorspace of an input. This setting works in tandem with "Color Corrector":#color_corrector > color_space_conversion to determine if any conversion will be performed.
newtype ColorSpace = ColorSpace String
derive instance newtypeColorSpace :: Newtype ColorSpace _


-- | Determines if colorspace conversion will be performed. If set to _None_, no conversion will be performed. If _Force 601_ or _Force 709_ are selected, conversion will be performed for inputs with differing colorspaces. An input's colorspace can be specified explicitly in the "Video Selector":#inputs-video_selector if necessary.
newtype ColorSpaceConversion = ColorSpaceConversion String
derive instance newtypeColorSpaceConversion :: Newtype ColorSpaceConversion _


-- | There are two sources for color metadata, the input file and the job configuration. This enum controls which takes precedence. FORCE: System will use color metadata supplied by user, if any. If the user does not supply color metadata the system will use data from the source. FALLBACK: System will use color metadata from the source. If source has no color metadata, the system will use user-supplied color metadata values if available.
newtype ColorSpaceUsage = ColorSpaceUsage String
derive instance newtypeColorSpaceUsage :: Newtype ColorSpaceUsage _


-- | The service could not complete your request because there is a conflict with the current state of the resource.
newtype ConflictException = ConflictException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _


-- | Container specific settings.
newtype ContainerSettings = ContainerSettings 
  { "Container" :: NullOrUndefined (ContainerType)
  , "F4vSettings" :: NullOrUndefined (F4vSettings)
  , "M2tsSettings" :: NullOrUndefined (M2tsSettings)
  , "M3u8Settings" :: NullOrUndefined (M3u8Settings)
  , "MovSettings" :: NullOrUndefined (MovSettings)
  , "Mp4Settings" :: NullOrUndefined (Mp4Settings)
  }
derive instance newtypeContainerSettings :: Newtype ContainerSettings _


-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
newtype ContainerType = ContainerType String
derive instance newtypeContainerType :: Newtype ContainerType _


newtype CreateJobRequest = CreateJobRequest 
  { "ClientRequestToken" :: NullOrUndefined (String)
  , "JobTemplate" :: NullOrUndefined (String)
  , "Queue" :: NullOrUndefined (String)
  , "Role" :: NullOrUndefined (String)
  , "Settings" :: NullOrUndefined (JobSettings)
  , "UserMetadata" :: NullOrUndefined (MapOf__string)
  }
derive instance newtypeCreateJobRequest :: Newtype CreateJobRequest _


newtype CreateJobResponse = CreateJobResponse 
  { "Job" :: NullOrUndefined (Job)
  }
derive instance newtypeCreateJobResponse :: Newtype CreateJobResponse _


newtype CreateJobTemplateRequest = CreateJobTemplateRequest 
  { "Category" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Queue" :: NullOrUndefined (String)
  , "Settings" :: NullOrUndefined (JobTemplateSettings)
  }
derive instance newtypeCreateJobTemplateRequest :: Newtype CreateJobTemplateRequest _


newtype CreateJobTemplateResponse = CreateJobTemplateResponse 
  { "JobTemplate" :: NullOrUndefined (JobTemplate)
  }
derive instance newtypeCreateJobTemplateResponse :: Newtype CreateJobTemplateResponse _


newtype CreatePresetRequest = CreatePresetRequest 
  { "Category" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Settings" :: NullOrUndefined (PresetSettings)
  }
derive instance newtypeCreatePresetRequest :: Newtype CreatePresetRequest _


newtype CreatePresetResponse = CreatePresetResponse 
  { "Preset" :: NullOrUndefined (Preset)
  }
derive instance newtypeCreatePresetResponse :: Newtype CreatePresetResponse _


newtype CreateQueueRequest = CreateQueueRequest 
  { "Description" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeCreateQueueRequest :: Newtype CreateQueueRequest _


newtype CreateQueueResponse = CreateQueueResponse 
  { "Queue" :: NullOrUndefined (Queue)
  }
derive instance newtypeCreateQueueResponse :: Newtype CreateQueueResponse _


-- | Specifies DRM settings for DASH outputs.
newtype DashIsoEncryptionSettings = DashIsoEncryptionSettings 
  { "SpekeKeyProvider" :: NullOrUndefined (SpekeKeyProvider)
  }
derive instance newtypeDashIsoEncryptionSettings :: Newtype DashIsoEncryptionSettings _


-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
newtype DashIsoGroupSettings = DashIsoGroupSettings 
  { "BaseUrl" :: NullOrUndefined (String)
  , "Destination" :: NullOrUndefined (String)
  , "Encryption" :: NullOrUndefined (DashIsoEncryptionSettings)
  , "FragmentLength" :: NullOrUndefined (Int)
  , "HbbtvCompliance" :: NullOrUndefined (DashIsoHbbtvCompliance)
  , "MinBufferTime" :: NullOrUndefined (Int)
  , "SegmentControl" :: NullOrUndefined (DashIsoSegmentControl)
  , "SegmentLength" :: NullOrUndefined (Int)
  }
derive instance newtypeDashIsoGroupSettings :: Newtype DashIsoGroupSettings _


-- | Supports HbbTV specification as indicated
newtype DashIsoHbbtvCompliance = DashIsoHbbtvCompliance String
derive instance newtypeDashIsoHbbtvCompliance :: Newtype DashIsoHbbtvCompliance _


-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
newtype DashIsoSegmentControl = DashIsoSegmentControl String
derive instance newtypeDashIsoSegmentControl :: Newtype DashIsoSegmentControl _


-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
newtype DeinterlaceAlgorithm = DeinterlaceAlgorithm String
derive instance newtypeDeinterlaceAlgorithm :: Newtype DeinterlaceAlgorithm _


-- | Settings for deinterlacer
newtype Deinterlacer = Deinterlacer 
  { "Algorithm" :: NullOrUndefined (DeinterlaceAlgorithm)
  , "Control" :: NullOrUndefined (DeinterlacerControl)
  , "Mode" :: NullOrUndefined (DeinterlacerMode)
  }
derive instance newtypeDeinterlacer :: Newtype Deinterlacer _


-- | - When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.
newtype DeinterlacerControl = DeinterlacerControl String
derive instance newtypeDeinterlacerControl :: Newtype DeinterlacerControl _


-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
newtype DeinterlacerMode = DeinterlacerMode String
derive instance newtypeDeinterlacerMode :: Newtype DeinterlacerMode _


newtype DeleteJobTemplateRequest = DeleteJobTemplateRequest 
  { "Name" :: (String)
  }
derive instance newtypeDeleteJobTemplateRequest :: Newtype DeleteJobTemplateRequest _


newtype DeleteJobTemplateResponse = DeleteJobTemplateResponse 
  { 
  }
derive instance newtypeDeleteJobTemplateResponse :: Newtype DeleteJobTemplateResponse _


newtype DeletePresetRequest = DeletePresetRequest 
  { "Name" :: (String)
  }
derive instance newtypeDeletePresetRequest :: Newtype DeletePresetRequest _


newtype DeletePresetResponse = DeletePresetResponse 
  { 
  }
derive instance newtypeDeletePresetResponse :: Newtype DeletePresetResponse _


newtype DeleteQueueRequest = DeleteQueueRequest 
  { "Name" :: (String)
  }
derive instance newtypeDeleteQueueRequest :: Newtype DeleteQueueRequest _


newtype DeleteQueueResponse = DeleteQueueResponse 
  { 
  }
derive instance newtypeDeleteQueueResponse :: Newtype DeleteQueueResponse _


-- | DescribeEndpointsRequest
newtype DescribeEndpointsRequest = DescribeEndpointsRequest 
  { "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEndpointsRequest :: Newtype DescribeEndpointsRequest _


newtype DescribeEndpointsResponse = DescribeEndpointsResponse 
  { "Endpoints" :: NullOrUndefined (ListOfEndpoint)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEndpointsResponse :: Newtype DescribeEndpointsResponse _


-- | Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.
newtype DropFrameTimecode = DropFrameTimecode String
derive instance newtypeDropFrameTimecode :: Newtype DropFrameTimecode _


-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
newtype DvbNitSettings = DvbNitSettings 
  { "NetworkId" :: NullOrUndefined (Int)
  , "NetworkName" :: NullOrUndefined (String)
  , "NitInterval" :: NullOrUndefined (Int)
  }
derive instance newtypeDvbNitSettings :: Newtype DvbNitSettings _


-- | Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
newtype DvbSdtSettings = DvbSdtSettings 
  { "OutputSdt" :: NullOrUndefined (OutputSdt)
  , "SdtInterval" :: NullOrUndefined (Int)
  , "ServiceName" :: NullOrUndefined (String)
  , "ServiceProviderName" :: NullOrUndefined (String)
  }
derive instance newtypeDvbSdtSettings :: Newtype DvbSdtSettings _


-- | DVB-Sub Destination Settings
newtype DvbSubDestinationSettings = DvbSubDestinationSettings 
  { "Alignment" :: NullOrUndefined (DvbSubtitleAlignment)
  , "BackgroundColor" :: NullOrUndefined (DvbSubtitleBackgroundColor)
  , "BackgroundOpacity" :: NullOrUndefined (Int)
  , "FontColor" :: NullOrUndefined (DvbSubtitleFontColor)
  , "FontOpacity" :: NullOrUndefined (Int)
  , "FontResolution" :: NullOrUndefined (Int)
  , "FontSize" :: NullOrUndefined (Int)
  , "OutlineColor" :: NullOrUndefined (DvbSubtitleOutlineColor)
  , "OutlineSize" :: NullOrUndefined (Int)
  , "ShadowColor" :: NullOrUndefined (DvbSubtitleShadowColor)
  , "ShadowOpacity" :: NullOrUndefined (Int)
  , "ShadowXOffset" :: NullOrUndefined (Int)
  , "ShadowYOffset" :: NullOrUndefined (Int)
  , "TeletextSpacing" :: NullOrUndefined (DvbSubtitleTeletextSpacing)
  , "XPosition" :: NullOrUndefined (Int)
  , "YPosition" :: NullOrUndefined (Int)
  }
derive instance newtypeDvbSubDestinationSettings :: Newtype DvbSubDestinationSettings _


-- | DVB Sub Source Settings
newtype DvbSubSourceSettings = DvbSubSourceSettings 
  { "Pid" :: NullOrUndefined (Int)
  }
derive instance newtypeDvbSubSourceSettings :: Newtype DvbSubSourceSettings _


-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleAlignment = DvbSubtitleAlignment String
derive instance newtypeDvbSubtitleAlignment :: Newtype DvbSubtitleAlignment _


-- | Specifies the color of the rectangle behind the captions.
-- | All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleBackgroundColor = DvbSubtitleBackgroundColor String
derive instance newtypeDvbSubtitleBackgroundColor :: Newtype DvbSubtitleBackgroundColor _


-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleFontColor = DvbSubtitleFontColor String
derive instance newtypeDvbSubtitleFontColor :: Newtype DvbSubtitleFontColor _


-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleOutlineColor = DvbSubtitleOutlineColor String
derive instance newtypeDvbSubtitleOutlineColor :: Newtype DvbSubtitleOutlineColor _


-- | Specifies the color of the shadow cast by the captions.
-- | All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleShadowColor = DvbSubtitleShadowColor String
derive instance newtypeDvbSubtitleShadowColor :: Newtype DvbSubtitleShadowColor _


-- | Controls whether a fixed grid size or proportional font spacing will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
newtype DvbSubtitleTeletextSpacing = DvbSubtitleTeletextSpacing String
derive instance newtypeDvbSubtitleTeletextSpacing :: Newtype DvbSubtitleTeletextSpacing _


-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
newtype DvbTdtSettings = DvbTdtSettings 
  { "TdtInterval" :: NullOrUndefined (Int)
  }
derive instance newtypeDvbTdtSettings :: Newtype DvbTdtSettings _


-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
newtype Eac3AttenuationControl = Eac3AttenuationControl String
derive instance newtypeEac3AttenuationControl :: Newtype Eac3AttenuationControl _


-- | Specifies the "Bitstream Mode" (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.
newtype Eac3BitstreamMode = Eac3BitstreamMode String
derive instance newtypeEac3BitstreamMode :: Newtype Eac3BitstreamMode _


-- | Dolby Digital Plus coding mode. Determines number of channels.
newtype Eac3CodingMode = Eac3CodingMode String
derive instance newtypeEac3CodingMode :: Newtype Eac3CodingMode _


-- | Activates a DC highpass filter for all input channels.
newtype Eac3DcFilter = Eac3DcFilter String
derive instance newtypeEac3DcFilter :: Newtype Eac3DcFilter _


-- | Enables Dynamic Range Compression that restricts the absolute peak level for a signal.
newtype Eac3DynamicRangeCompressionLine = Eac3DynamicRangeCompressionLine String
derive instance newtypeEac3DynamicRangeCompressionLine :: Newtype Eac3DynamicRangeCompressionLine _


-- | Enables Heavy Dynamic Range Compression, ensures that the instantaneous signal peaks do not exceed specified levels.
newtype Eac3DynamicRangeCompressionRf = Eac3DynamicRangeCompressionRf String
derive instance newtypeEac3DynamicRangeCompressionRf :: Newtype Eac3DynamicRangeCompressionRf _


-- | When encoding 3/2 audio, controls whether the LFE channel is enabled
newtype Eac3LfeControl = Eac3LfeControl String
derive instance newtypeEac3LfeControl :: Newtype Eac3LfeControl _


-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
newtype Eac3LfeFilter = Eac3LfeFilter String
derive instance newtypeEac3LfeFilter :: Newtype Eac3LfeFilter _


-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
newtype Eac3MetadataControl = Eac3MetadataControl String
derive instance newtypeEac3MetadataControl :: Newtype Eac3MetadataControl _


-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
newtype Eac3PassthroughControl = Eac3PassthroughControl String
derive instance newtypeEac3PassthroughControl :: Newtype Eac3PassthroughControl _


-- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
newtype Eac3PhaseControl = Eac3PhaseControl String
derive instance newtypeEac3PhaseControl :: Newtype Eac3PhaseControl _


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
newtype Eac3Settings = Eac3Settings 
  { "AttenuationControl" :: NullOrUndefined (Eac3AttenuationControl)
  , "Bitrate" :: NullOrUndefined (Int)
  , "BitstreamMode" :: NullOrUndefined (Eac3BitstreamMode)
  , "CodingMode" :: NullOrUndefined (Eac3CodingMode)
  , "DcFilter" :: NullOrUndefined (Eac3DcFilter)
  , "Dialnorm" :: NullOrUndefined (Int)
  , "DynamicRangeCompressionLine" :: NullOrUndefined (Eac3DynamicRangeCompressionLine)
  , "DynamicRangeCompressionRf" :: NullOrUndefined (Eac3DynamicRangeCompressionRf)
  , "LfeControl" :: NullOrUndefined (Eac3LfeControl)
  , "LfeFilter" :: NullOrUndefined (Eac3LfeFilter)
  , "LoRoCenterMixLevel" :: NullOrUndefined (Number)
  , "LoRoSurroundMixLevel" :: NullOrUndefined (Number)
  , "LtRtCenterMixLevel" :: NullOrUndefined (Number)
  , "LtRtSurroundMixLevel" :: NullOrUndefined (Number)
  , "MetadataControl" :: NullOrUndefined (Eac3MetadataControl)
  , "PassthroughControl" :: NullOrUndefined (Eac3PassthroughControl)
  , "PhaseControl" :: NullOrUndefined (Eac3PhaseControl)
  , "SampleRate" :: NullOrUndefined (Int)
  , "StereoDownmix" :: NullOrUndefined (Eac3StereoDownmix)
  , "SurroundExMode" :: NullOrUndefined (Eac3SurroundExMode)
  , "SurroundMode" :: NullOrUndefined (Eac3SurroundMode)
  }
derive instance newtypeEac3Settings :: Newtype Eac3Settings _


-- | Stereo downmix preference. Only used for 3/2 coding mode.
newtype Eac3StereoDownmix = Eac3StereoDownmix String
derive instance newtypeEac3StereoDownmix :: Newtype Eac3StereoDownmix _


-- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
newtype Eac3SurroundExMode = Eac3SurroundExMode String
derive instance newtypeEac3SurroundExMode :: Newtype Eac3SurroundExMode _


-- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
newtype Eac3SurroundMode = Eac3SurroundMode String
derive instance newtypeEac3SurroundMode :: Newtype Eac3SurroundMode _


-- | When set to UPCONVERT, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
newtype EmbeddedConvert608To708 = EmbeddedConvert608To708 String
derive instance newtypeEmbeddedConvert608To708 :: Newtype EmbeddedConvert608To708 _


-- | Settings for embedded captions Source
newtype EmbeddedSourceSettings = EmbeddedSourceSettings 
  { "Convert608To708" :: NullOrUndefined (EmbeddedConvert608To708)
  , "Source608ChannelNumber" :: NullOrUndefined (Int)
  , "Source608TrackNumber" :: NullOrUndefined (Int)
  }
derive instance newtypeEmbeddedSourceSettings :: Newtype EmbeddedSourceSettings _


-- | Describes account specific API endpoint
newtype Endpoint = Endpoint 
  { "Url" :: NullOrUndefined (String)
  }
derive instance newtypeEndpoint :: Newtype Endpoint _


newtype ExceptionBody = ExceptionBody 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeExceptionBody :: Newtype ExceptionBody _


-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
newtype F4vMoovPlacement = F4vMoovPlacement String
derive instance newtypeF4vMoovPlacement :: Newtype F4vMoovPlacement _


-- | Settings for F4v container
newtype F4vSettings = F4vSettings 
  { "MoovPlacement" :: NullOrUndefined (F4vMoovPlacement)
  }
derive instance newtypeF4vSettings :: Newtype F4vSettings _


-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
newtype FileGroupSettings = FileGroupSettings 
  { "Destination" :: NullOrUndefined (String)
  }
derive instance newtypeFileGroupSettings :: Newtype FileGroupSettings _


-- | If set to UPCONVERT, 608 caption data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
newtype FileSourceConvert608To708 = FileSourceConvert608To708 String
derive instance newtypeFileSourceConvert608To708 :: Newtype FileSourceConvert608To708 _


-- | Settings for File-based Captions in Source
newtype FileSourceSettings = FileSourceSettings 
  { "Convert608To708" :: NullOrUndefined (FileSourceConvert608To708)
  , "SourceFile" :: NullOrUndefined (String)
  , "TimeDelta" :: NullOrUndefined (Int)
  }
derive instance newtypeFileSourceSettings :: Newtype FileSourceSettings _


-- | You don't have permissions for this action with the credentials you sent.
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _


-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
newtype FrameCaptureSettings = FrameCaptureSettings 
  { "FramerateDenominator" :: NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined (Int)
  , "MaxCaptures" :: NullOrUndefined (Int)
  , "Quality" :: NullOrUndefined (Int)
  }
derive instance newtypeFrameCaptureSettings :: Newtype FrameCaptureSettings _


newtype GetJobRequest = GetJobRequest 
  { "Id" :: (String)
  }
derive instance newtypeGetJobRequest :: Newtype GetJobRequest _


newtype GetJobResponse = GetJobResponse 
  { "Job" :: NullOrUndefined (Job)
  }
derive instance newtypeGetJobResponse :: Newtype GetJobResponse _


newtype GetJobTemplateRequest = GetJobTemplateRequest 
  { "Name" :: (String)
  }
derive instance newtypeGetJobTemplateRequest :: Newtype GetJobTemplateRequest _


newtype GetJobTemplateResponse = GetJobTemplateResponse 
  { "JobTemplate" :: NullOrUndefined (JobTemplate)
  }
derive instance newtypeGetJobTemplateResponse :: Newtype GetJobTemplateResponse _


newtype GetPresetRequest = GetPresetRequest 
  { "Name" :: (String)
  }
derive instance newtypeGetPresetRequest :: Newtype GetPresetRequest _


newtype GetPresetResponse = GetPresetResponse 
  { "Preset" :: NullOrUndefined (Preset)
  }
derive instance newtypeGetPresetResponse :: Newtype GetPresetResponse _


newtype GetQueueRequest = GetQueueRequest 
  { "Name" :: (String)
  }
derive instance newtypeGetQueueRequest :: Newtype GetQueueRequest _


newtype GetQueueResponse = GetQueueResponse 
  { "Queue" :: NullOrUndefined (Queue)
  }
derive instance newtypeGetQueueResponse :: Newtype GetQueueResponse _


-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
newtype H264AdaptiveQuantization = H264AdaptiveQuantization String
derive instance newtypeH264AdaptiveQuantization :: Newtype H264AdaptiveQuantization _


-- | H.264 Level.
newtype H264CodecLevel = H264CodecLevel String
derive instance newtypeH264CodecLevel :: Newtype H264CodecLevel _


-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
newtype H264CodecProfile = H264CodecProfile String
derive instance newtypeH264CodecProfile :: Newtype H264CodecProfile _


-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
newtype H264EntropyEncoding = H264EntropyEncoding String
derive instance newtypeH264EntropyEncoding :: Newtype H264EntropyEncoding _


-- | Choosing FORCE_FIELD disables PAFF encoding for interlaced outputs.
newtype H264FieldEncoding = H264FieldEncoding String
derive instance newtypeH264FieldEncoding :: Newtype H264FieldEncoding _


-- | Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
newtype H264FlickerAdaptiveQuantization = H264FlickerAdaptiveQuantization String
derive instance newtypeH264FlickerAdaptiveQuantization :: Newtype H264FlickerAdaptiveQuantization _


-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
newtype H264FramerateControl = H264FramerateControl String
derive instance newtypeH264FramerateControl :: Newtype H264FramerateControl _


-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
newtype H264FramerateConversionAlgorithm = H264FramerateConversionAlgorithm String
derive instance newtypeH264FramerateConversionAlgorithm :: Newtype H264FramerateConversionAlgorithm _


-- | If enable, use reference B frames for GOP structures that have B frames > 1.
newtype H264GopBReference = H264GopBReference String
derive instance newtypeH264GopBReference :: Newtype H264GopBReference _


-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
newtype H264GopSizeUnits = H264GopSizeUnits String
derive instance newtypeH264GopSizeUnits :: Newtype H264GopSizeUnits _


-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
newtype H264InterlaceMode = H264InterlaceMode String
derive instance newtypeH264InterlaceMode :: Newtype H264InterlaceMode _


-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
newtype H264ParControl = H264ParControl String
derive instance newtypeH264ParControl :: Newtype H264ParControl _


-- | Use Quality tuning level (H264QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.
newtype H264QualityTuningLevel = H264QualityTuningLevel String
derive instance newtypeH264QualityTuningLevel :: Newtype H264QualityTuningLevel _


-- | Rate control mode. CQ uses constant quantizer (qp), ABR (average bitrate) does not write HRD parameters.
newtype H264RateControlMode = H264RateControlMode String
derive instance newtypeH264RateControlMode :: Newtype H264RateControlMode _


-- | Places a PPS header on each encoded picture, even if repeated.
newtype H264RepeatPps = H264RepeatPps String
derive instance newtypeH264RepeatPps :: Newtype H264RepeatPps _


-- | Scene change detection (inserts I-frames on scene changes).
newtype H264SceneChangeDetect = H264SceneChangeDetect String
derive instance newtypeH264SceneChangeDetect :: Newtype H264SceneChangeDetect _


-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
newtype H264Settings = H264Settings 
  { "AdaptiveQuantization" :: NullOrUndefined (H264AdaptiveQuantization)
  , "Bitrate" :: NullOrUndefined (Int)
  , "CodecLevel" :: NullOrUndefined (H264CodecLevel)
  , "CodecProfile" :: NullOrUndefined (H264CodecProfile)
  , "EntropyEncoding" :: NullOrUndefined (H264EntropyEncoding)
  , "FieldEncoding" :: NullOrUndefined (H264FieldEncoding)
  , "FlickerAdaptiveQuantization" :: NullOrUndefined (H264FlickerAdaptiveQuantization)
  , "FramerateControl" :: NullOrUndefined (H264FramerateControl)
  , "FramerateConversionAlgorithm" :: NullOrUndefined (H264FramerateConversionAlgorithm)
  , "FramerateDenominator" :: NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined (Int)
  , "GopBReference" :: NullOrUndefined (H264GopBReference)
  , "GopClosedCadence" :: NullOrUndefined (Int)
  , "GopSize" :: NullOrUndefined (Number)
  , "GopSizeUnits" :: NullOrUndefined (H264GopSizeUnits)
  , "HrdBufferInitialFillPercentage" :: NullOrUndefined (Int)
  , "HrdBufferSize" :: NullOrUndefined (Int)
  , "InterlaceMode" :: NullOrUndefined (H264InterlaceMode)
  , "MaxBitrate" :: NullOrUndefined (Int)
  , "MinIInterval" :: NullOrUndefined (Int)
  , "NumberBFramesBetweenReferenceFrames" :: NullOrUndefined (Int)
  , "NumberReferenceFrames" :: NullOrUndefined (Int)
  , "ParControl" :: NullOrUndefined (H264ParControl)
  , "ParDenominator" :: NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined (Int)
  , "QualityTuningLevel" :: NullOrUndefined (H264QualityTuningLevel)
  , "RateControlMode" :: NullOrUndefined (H264RateControlMode)
  , "RepeatPps" :: NullOrUndefined (H264RepeatPps)
  , "SceneChangeDetect" :: NullOrUndefined (H264SceneChangeDetect)
  , "Slices" :: NullOrUndefined (Int)
  , "SlowPal" :: NullOrUndefined (H264SlowPal)
  , "Softness" :: NullOrUndefined (Int)
  , "SpatialAdaptiveQuantization" :: NullOrUndefined (H264SpatialAdaptiveQuantization)
  , "Syntax" :: NullOrUndefined (H264Syntax)
  , "Telecine" :: NullOrUndefined (H264Telecine)
  , "TemporalAdaptiveQuantization" :: NullOrUndefined (H264TemporalAdaptiveQuantization)
  , "UnregisteredSeiTimecode" :: NullOrUndefined (H264UnregisteredSeiTimecode)
  }
derive instance newtypeH264Settings :: Newtype H264Settings _


-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
newtype H264SlowPal = H264SlowPal String
derive instance newtypeH264SlowPal :: Newtype H264SlowPal _


-- | Adjust quantization within each frame based on spatial variation of content complexity.
newtype H264SpatialAdaptiveQuantization = H264SpatialAdaptiveQuantization String
derive instance newtypeH264SpatialAdaptiveQuantization :: Newtype H264SpatialAdaptiveQuantization _


-- | Produces a bitstream compliant with SMPTE RP-2027.
newtype H264Syntax = H264Syntax String
derive instance newtypeH264Syntax :: Newtype H264Syntax _


-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
newtype H264Telecine = H264Telecine String
derive instance newtypeH264Telecine :: Newtype H264Telecine _


-- | Adjust quantization within each frame based on temporal variation of content complexity.
newtype H264TemporalAdaptiveQuantization = H264TemporalAdaptiveQuantization String
derive instance newtypeH264TemporalAdaptiveQuantization :: Newtype H264TemporalAdaptiveQuantization _


-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
newtype H264UnregisteredSeiTimecode = H264UnregisteredSeiTimecode String
derive instance newtypeH264UnregisteredSeiTimecode :: Newtype H264UnregisteredSeiTimecode _


-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
newtype H265AdaptiveQuantization = H265AdaptiveQuantization String
derive instance newtypeH265AdaptiveQuantization :: Newtype H265AdaptiveQuantization _


-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
newtype H265AlternateTransferFunctionSei = H265AlternateTransferFunctionSei String
derive instance newtypeH265AlternateTransferFunctionSei :: Newtype H265AlternateTransferFunctionSei _


-- | H.265 Level.
newtype H265CodecLevel = H265CodecLevel String
derive instance newtypeH265CodecLevel :: Newtype H265CodecLevel _


-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
newtype H265CodecProfile = H265CodecProfile String
derive instance newtypeH265CodecProfile :: Newtype H265CodecProfile _


-- | Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
newtype H265FlickerAdaptiveQuantization = H265FlickerAdaptiveQuantization String
derive instance newtypeH265FlickerAdaptiveQuantization :: Newtype H265FlickerAdaptiveQuantization _


-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
newtype H265FramerateControl = H265FramerateControl String
derive instance newtypeH265FramerateControl :: Newtype H265FramerateControl _


-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
newtype H265FramerateConversionAlgorithm = H265FramerateConversionAlgorithm String
derive instance newtypeH265FramerateConversionAlgorithm :: Newtype H265FramerateConversionAlgorithm _


-- | If enable, use reference B frames for GOP structures that have B frames > 1.
newtype H265GopBReference = H265GopBReference String
derive instance newtypeH265GopBReference :: Newtype H265GopBReference _


-- | Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
newtype H265GopSizeUnits = H265GopSizeUnits String
derive instance newtypeH265GopSizeUnits :: Newtype H265GopSizeUnits _


-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
newtype H265InterlaceMode = H265InterlaceMode String
derive instance newtypeH265InterlaceMode :: Newtype H265InterlaceMode _


-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
newtype H265ParControl = H265ParControl String
derive instance newtypeH265ParControl :: Newtype H265ParControl _


-- | Use Quality tuning level (H265QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.
newtype H265QualityTuningLevel = H265QualityTuningLevel String
derive instance newtypeH265QualityTuningLevel :: Newtype H265QualityTuningLevel _


-- | Rate control mode. CQ uses constant quantizer (qp), ABR (average bitrate) does not write HRD parameters.
newtype H265RateControlMode = H265RateControlMode String
derive instance newtypeH265RateControlMode :: Newtype H265RateControlMode _


-- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
newtype H265SampleAdaptiveOffsetFilterMode = H265SampleAdaptiveOffsetFilterMode String
derive instance newtypeH265SampleAdaptiveOffsetFilterMode :: Newtype H265SampleAdaptiveOffsetFilterMode _


-- | Scene change detection (inserts I-frames on scene changes).
newtype H265SceneChangeDetect = H265SceneChangeDetect String
derive instance newtypeH265SceneChangeDetect :: Newtype H265SceneChangeDetect _


-- | Settings for H265 codec
newtype H265Settings = H265Settings 
  { "AdaptiveQuantization" :: NullOrUndefined (H265AdaptiveQuantization)
  , "AlternateTransferFunctionSei" :: NullOrUndefined (H265AlternateTransferFunctionSei)
  , "Bitrate" :: NullOrUndefined (Int)
  , "CodecLevel" :: NullOrUndefined (H265CodecLevel)
  , "CodecProfile" :: NullOrUndefined (H265CodecProfile)
  , "FlickerAdaptiveQuantization" :: NullOrUndefined (H265FlickerAdaptiveQuantization)
  , "FramerateControl" :: NullOrUndefined (H265FramerateControl)
  , "FramerateConversionAlgorithm" :: NullOrUndefined (H265FramerateConversionAlgorithm)
  , "FramerateDenominator" :: NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined (Int)
  , "GopBReference" :: NullOrUndefined (H265GopBReference)
  , "GopClosedCadence" :: NullOrUndefined (Int)
  , "GopSize" :: NullOrUndefined (Number)
  , "GopSizeUnits" :: NullOrUndefined (H265GopSizeUnits)
  , "HrdBufferInitialFillPercentage" :: NullOrUndefined (Int)
  , "HrdBufferSize" :: NullOrUndefined (Int)
  , "InterlaceMode" :: NullOrUndefined (H265InterlaceMode)
  , "MaxBitrate" :: NullOrUndefined (Int)
  , "MinIInterval" :: NullOrUndefined (Int)
  , "NumberBFramesBetweenReferenceFrames" :: NullOrUndefined (Int)
  , "NumberReferenceFrames" :: NullOrUndefined (Int)
  , "ParControl" :: NullOrUndefined (H265ParControl)
  , "ParDenominator" :: NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined (Int)
  , "QualityTuningLevel" :: NullOrUndefined (H265QualityTuningLevel)
  , "RateControlMode" :: NullOrUndefined (H265RateControlMode)
  , "SampleAdaptiveOffsetFilterMode" :: NullOrUndefined (H265SampleAdaptiveOffsetFilterMode)
  , "SceneChangeDetect" :: NullOrUndefined (H265SceneChangeDetect)
  , "Slices" :: NullOrUndefined (Int)
  , "SlowPal" :: NullOrUndefined (H265SlowPal)
  , "SpatialAdaptiveQuantization" :: NullOrUndefined (H265SpatialAdaptiveQuantization)
  , "Telecine" :: NullOrUndefined (H265Telecine)
  , "TemporalAdaptiveQuantization" :: NullOrUndefined (H265TemporalAdaptiveQuantization)
  , "TemporalIds" :: NullOrUndefined (H265TemporalIds)
  , "Tiles" :: NullOrUndefined (H265Tiles)
  , "UnregisteredSeiTimecode" :: NullOrUndefined (H265UnregisteredSeiTimecode)
  }
derive instance newtypeH265Settings :: Newtype H265Settings _


-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
newtype H265SlowPal = H265SlowPal String
derive instance newtypeH265SlowPal :: Newtype H265SlowPal _


-- | Adjust quantization within each frame based on spatial variation of content complexity.
newtype H265SpatialAdaptiveQuantization = H265SpatialAdaptiveQuantization String
derive instance newtypeH265SpatialAdaptiveQuantization :: Newtype H265SpatialAdaptiveQuantization _


-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
newtype H265Telecine = H265Telecine String
derive instance newtypeH265Telecine :: Newtype H265Telecine _


-- | Adjust quantization within each frame based on temporal variation of content complexity.
newtype H265TemporalAdaptiveQuantization = H265TemporalAdaptiveQuantization String
derive instance newtypeH265TemporalAdaptiveQuantization :: Newtype H265TemporalAdaptiveQuantization _


-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
newtype H265TemporalIds = H265TemporalIds String
derive instance newtypeH265TemporalIds :: Newtype H265TemporalIds _


-- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
newtype H265Tiles = H265Tiles String
derive instance newtypeH265Tiles :: Newtype H265Tiles _


-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
newtype H265UnregisteredSeiTimecode = H265UnregisteredSeiTimecode String
derive instance newtypeH265UnregisteredSeiTimecode :: Newtype H265UnregisteredSeiTimecode _


-- | Use the HDR master display (Hdr10Metadata) settings to provide values for HDR color. These values vary depending on the input video and must be provided by a color grader. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
newtype Hdr10Metadata = Hdr10Metadata 
  { "BluePrimaryX" :: NullOrUndefined (Int)
  , "BluePrimaryY" :: NullOrUndefined (Int)
  , "GreenPrimaryX" :: NullOrUndefined (Int)
  , "GreenPrimaryY" :: NullOrUndefined (Int)
  , "MaxContentLightLevel" :: NullOrUndefined (Int)
  , "MaxFrameAverageLightLevel" :: NullOrUndefined (Int)
  , "MaxLuminance" :: NullOrUndefined (Int)
  , "MinLuminance" :: NullOrUndefined (Int)
  , "RedPrimaryX" :: NullOrUndefined (Int)
  , "RedPrimaryY" :: NullOrUndefined (Int)
  , "WhitePointX" :: NullOrUndefined (Int)
  , "WhitePointY" :: NullOrUndefined (Int)
  }
derive instance newtypeHdr10Metadata :: Newtype Hdr10Metadata _


newtype HlsAdMarkers = HlsAdMarkers String
derive instance newtypeHlsAdMarkers :: Newtype HlsAdMarkers _


-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
newtype HlsAudioTrackType = HlsAudioTrackType String
derive instance newtypeHlsAudioTrackType :: Newtype HlsAudioTrackType _


-- | Caption Language Mapping
newtype HlsCaptionLanguageMapping = HlsCaptionLanguageMapping 
  { "CaptionChannel" :: NullOrUndefined (Int)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "LanguageDescription" :: NullOrUndefined (String)
  }
derive instance newtypeHlsCaptionLanguageMapping :: Newtype HlsCaptionLanguageMapping _


-- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
newtype HlsCaptionLanguageSetting = HlsCaptionLanguageSetting String
derive instance newtypeHlsCaptionLanguageSetting :: Newtype HlsCaptionLanguageSetting _


-- | When set to ENABLED, sets #EXT-X-ALLOW-CACHE:no tag, which prevents client from saving media segments for later replay.
newtype HlsClientCache = HlsClientCache String
derive instance newtypeHlsClientCache :: Newtype HlsClientCache _


-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
newtype HlsCodecSpecification = HlsCodecSpecification String
derive instance newtypeHlsCodecSpecification :: Newtype HlsCodecSpecification _


-- | Indicates whether segments should be placed in subdirectories.
newtype HlsDirectoryStructure = HlsDirectoryStructure String
derive instance newtypeHlsDirectoryStructure :: Newtype HlsDirectoryStructure _


-- | Settings for HLS encryption
newtype HlsEncryptionSettings = HlsEncryptionSettings 
  { "ConstantInitializationVector" :: NullOrUndefined (String)
  , "EncryptionMethod" :: NullOrUndefined (HlsEncryptionType)
  , "InitializationVectorInManifest" :: NullOrUndefined (HlsInitializationVectorInManifest)
  , "SpekeKeyProvider" :: NullOrUndefined (SpekeKeyProvider)
  , "StaticKeyProvider" :: NullOrUndefined (StaticKeyProvider)
  , "Type" :: NullOrUndefined (HlsKeyProviderType)
  }
derive instance newtypeHlsEncryptionSettings :: Newtype HlsEncryptionSettings _


-- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
newtype HlsEncryptionType = HlsEncryptionType String
derive instance newtypeHlsEncryptionType :: Newtype HlsEncryptionType _


-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
newtype HlsGroupSettings = HlsGroupSettings 
  { "AdMarkers" :: NullOrUndefined (ListOfHlsAdMarkers)
  , "BaseUrl" :: NullOrUndefined (String)
  , "CaptionLanguageMappings" :: NullOrUndefined (ListOfHlsCaptionLanguageMapping)
  , "CaptionLanguageSetting" :: NullOrUndefined (HlsCaptionLanguageSetting)
  , "ClientCache" :: NullOrUndefined (HlsClientCache)
  , "CodecSpecification" :: NullOrUndefined (HlsCodecSpecification)
  , "Destination" :: NullOrUndefined (String)
  , "DirectoryStructure" :: NullOrUndefined (HlsDirectoryStructure)
  , "Encryption" :: NullOrUndefined (HlsEncryptionSettings)
  , "ManifestCompression" :: NullOrUndefined (HlsManifestCompression)
  , "ManifestDurationFormat" :: NullOrUndefined (HlsManifestDurationFormat)
  , "MinSegmentLength" :: NullOrUndefined (Int)
  , "OutputSelection" :: NullOrUndefined (HlsOutputSelection)
  , "ProgramDateTime" :: NullOrUndefined (HlsProgramDateTime)
  , "ProgramDateTimePeriod" :: NullOrUndefined (Int)
  , "SegmentControl" :: NullOrUndefined (HlsSegmentControl)
  , "SegmentLength" :: NullOrUndefined (Int)
  , "SegmentsPerSubdirectory" :: NullOrUndefined (Int)
  , "StreamInfResolution" :: NullOrUndefined (HlsStreamInfResolution)
  , "TimedMetadataId3Frame" :: NullOrUndefined (HlsTimedMetadataId3Frame)
  , "TimedMetadataId3Period" :: NullOrUndefined (Int)
  , "TimestampDeltaMilliseconds" :: NullOrUndefined (Int)
  }
derive instance newtypeHlsGroupSettings :: Newtype HlsGroupSettings _


-- | When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
newtype HlsIFrameOnlyManifest = HlsIFrameOnlyManifest String
derive instance newtypeHlsIFrameOnlyManifest :: Newtype HlsIFrameOnlyManifest _


-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
newtype HlsInitializationVectorInManifest = HlsInitializationVectorInManifest String
derive instance newtypeHlsInitializationVectorInManifest :: Newtype HlsInitializationVectorInManifest _


-- | Indicates which type of key provider is used for encryption.
newtype HlsKeyProviderType = HlsKeyProviderType String
derive instance newtypeHlsKeyProviderType :: Newtype HlsKeyProviderType _


-- | When set to GZIP, compresses HLS playlist.
newtype HlsManifestCompression = HlsManifestCompression String
derive instance newtypeHlsManifestCompression :: Newtype HlsManifestCompression _


-- | Indicates whether the output manifest should use floating point values for segment duration.
newtype HlsManifestDurationFormat = HlsManifestDurationFormat String
derive instance newtypeHlsManifestDurationFormat :: Newtype HlsManifestDurationFormat _


-- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
newtype HlsOutputSelection = HlsOutputSelection String
derive instance newtypeHlsOutputSelection :: Newtype HlsOutputSelection _


-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
newtype HlsProgramDateTime = HlsProgramDateTime String
derive instance newtypeHlsProgramDateTime :: Newtype HlsProgramDateTime _


-- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
newtype HlsSegmentControl = HlsSegmentControl String
derive instance newtypeHlsSegmentControl :: Newtype HlsSegmentControl _


-- | Settings for HLS output groups
newtype HlsSettings = HlsSettings 
  { "AudioGroupId" :: NullOrUndefined (String)
  , "AudioRenditionSets" :: NullOrUndefined (String)
  , "AudioTrackType" :: NullOrUndefined (HlsAudioTrackType)
  , "IFrameOnlyManifest" :: NullOrUndefined (HlsIFrameOnlyManifest)
  , "SegmentModifier" :: NullOrUndefined (String)
  }
derive instance newtypeHlsSettings :: Newtype HlsSettings _


-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
newtype HlsStreamInfResolution = HlsStreamInfResolution String
derive instance newtypeHlsStreamInfResolution :: Newtype HlsStreamInfResolution _


-- | Indicates ID3 frame that has the timecode.
newtype HlsTimedMetadataId3Frame = HlsTimedMetadataId3Frame String
derive instance newtypeHlsTimedMetadataId3Frame :: Newtype HlsTimedMetadataId3Frame _


-- | To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3) to specify the base 64 encoded string and use Timecode (TimeCode) to specify the time when the tag should be inserted. To insert multiple ID3 tags in your output, create mulitple instances of ID3 insertion (Id3Insertion).
newtype Id3Insertion = Id3Insertion 
  { "Id3" :: NullOrUndefined (String)
  , "Timecode" :: NullOrUndefined (String)
  }
derive instance newtypeId3Insertion :: Newtype Id3Insertion _


-- | Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
newtype ImageInserter = ImageInserter 
  { "InsertableImages" :: NullOrUndefined (ListOfInsertableImage)
  }
derive instance newtypeImageInserter :: Newtype ImageInserter _


-- | Specifies media input
newtype Input = Input 
  { "AudioSelectorGroups" :: NullOrUndefined (MapOfAudioSelectorGroup)
  , "AudioSelectors" :: NullOrUndefined (MapOfAudioSelector)
  , "CaptionSelectors" :: NullOrUndefined (MapOfCaptionSelector)
  , "DeblockFilter" :: NullOrUndefined (InputDeblockFilter)
  , "DenoiseFilter" :: NullOrUndefined (InputDenoiseFilter)
  , "FileInput" :: NullOrUndefined (String)
  , "FilterEnable" :: NullOrUndefined (InputFilterEnable)
  , "FilterStrength" :: NullOrUndefined (Int)
  , "InputClippings" :: NullOrUndefined (ListOfInputClipping)
  , "ProgramNumber" :: NullOrUndefined (Int)
  , "PsiControl" :: NullOrUndefined (InputPsiControl)
  , "TimecodeSource" :: NullOrUndefined (InputTimecodeSource)
  , "VideoSelector" :: NullOrUndefined (VideoSelector)
  }
derive instance newtypeInput :: Newtype Input _


-- | Include one instance of (InputClipping) for each input clip.
newtype InputClipping = InputClipping 
  { "EndTimecode" :: NullOrUndefined (String)
  , "StartTimecode" :: NullOrUndefined (String)
  }
derive instance newtypeInputClipping :: Newtype InputClipping _


-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manaully controllable for MPEG2 and uncompressed video inputs.
newtype InputDeblockFilter = InputDeblockFilter String
derive instance newtypeInputDeblockFilter :: Newtype InputDeblockFilter _


-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
newtype InputDenoiseFilter = InputDenoiseFilter String
derive instance newtypeInputDenoiseFilter :: Newtype InputDenoiseFilter _


-- | Use Filter enable (InputFilterEnable) to specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The in put is filtered regardless of input type.
newtype InputFilterEnable = InputFilterEnable String
derive instance newtypeInputFilterEnable :: Newtype InputFilterEnable _


-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
newtype InputPsiControl = InputPsiControl String
derive instance newtypeInputPsiControl :: Newtype InputPsiControl _


-- | Specified video input in a template.
newtype InputTemplate = InputTemplate 
  { "AudioSelectorGroups" :: NullOrUndefined (MapOfAudioSelectorGroup)
  , "AudioSelectors" :: NullOrUndefined (MapOfAudioSelector)
  , "CaptionSelectors" :: NullOrUndefined (MapOfCaptionSelector)
  , "DeblockFilter" :: NullOrUndefined (InputDeblockFilter)
  , "DenoiseFilter" :: NullOrUndefined (InputDenoiseFilter)
  , "FilterEnable" :: NullOrUndefined (InputFilterEnable)
  , "FilterStrength" :: NullOrUndefined (Int)
  , "InputClippings" :: NullOrUndefined (ListOfInputClipping)
  , "ProgramNumber" :: NullOrUndefined (Int)
  , "PsiControl" :: NullOrUndefined (InputPsiControl)
  , "TimecodeSource" :: NullOrUndefined (InputTimecodeSource)
  , "VideoSelector" :: NullOrUndefined (VideoSelector)
  }
derive instance newtypeInputTemplate :: Newtype InputTemplate _


-- | Use Timecode source (InputTimecodeSource) to specify how timecode information from your input is adjusted and encoded in all outputs for the job. Default is embedded. Set to Embedded (EMBEDDED) to use the timecode that is in the input video. If no embedded timecode is in the source, will set the timecode for the first frame to 00:00:00:00. Set to Start at 0 (ZEROBASED) to set the timecode of the initial frame to 00:00:00:00. Set to Specified start (SPECIFIEDSTART) to provide the initial timecode yourself the setting (Start).
newtype InputTimecodeSource = InputTimecodeSource String
derive instance newtypeInputTimecodeSource :: Newtype InputTimecodeSource _


-- | Settings for Insertable Image
newtype InsertableImage = InsertableImage 
  { "Duration" :: NullOrUndefined (Int)
  , "FadeIn" :: NullOrUndefined (Int)
  , "FadeOut" :: NullOrUndefined (Int)
  , "Height" :: NullOrUndefined (Int)
  , "ImageInserterInput" :: NullOrUndefined (String)
  , "ImageX" :: NullOrUndefined (Int)
  , "ImageY" :: NullOrUndefined (Int)
  , "Layer" :: NullOrUndefined (Int)
  , "Opacity" :: NullOrUndefined (Int)
  , "StartTime" :: NullOrUndefined (String)
  , "Width" :: NullOrUndefined (Int)
  }
derive instance newtypeInsertableImage :: Newtype InsertableImage _


-- | The service encountered an unexpected condition and cannot fulfill your request.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _


-- | Each job converts an input file into an output file or files. For more information, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
newtype Job = Job 
  { "Arn" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (Number)
  , "ErrorCode" :: NullOrUndefined (Int)
  , "ErrorMessage" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "JobTemplate" :: NullOrUndefined (String)
  , "OutputGroupDetails" :: NullOrUndefined (ListOfOutputGroupDetail)
  , "Queue" :: NullOrUndefined (String)
  , "Role" :: NullOrUndefined (String)
  , "Settings" :: NullOrUndefined (JobSettings)
  , "Status" :: NullOrUndefined (JobStatus)
  , "Timing" :: NullOrUndefined (Timing)
  , "UserMetadata" :: NullOrUndefined (MapOf__string)
  }
derive instance newtypeJob :: Newtype Job _


-- | JobSettings contains all the transcode settings for a job.
newtype JobSettings = JobSettings 
  { "AdAvailOffset" :: NullOrUndefined (Int)
  , "AvailBlanking" :: NullOrUndefined (AvailBlanking)
  , "Inputs" :: NullOrUndefined (ListOfInput)
  , "NielsenConfiguration" :: NullOrUndefined (NielsenConfiguration)
  , "OutputGroups" :: NullOrUndefined (ListOfOutputGroup)
  , "TimecodeConfig" :: NullOrUndefined (TimecodeConfig)
  , "TimedMetadataInsertion" :: NullOrUndefined (TimedMetadataInsertion)
  }
derive instance newtypeJobSettings :: Newtype JobSettings _


-- | A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _


-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
newtype JobTemplate = JobTemplate 
  { "Arn" :: NullOrUndefined (String)
  , "Category" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (Number)
  , "Description" :: NullOrUndefined (String)
  , "LastUpdated" :: NullOrUndefined (Number)
  , "Name" :: NullOrUndefined (String)
  , "Queue" :: NullOrUndefined (String)
  , "Settings" :: NullOrUndefined (JobTemplateSettings)
  , "Type" :: NullOrUndefined (Type)
  }
derive instance newtypeJobTemplate :: Newtype JobTemplate _


-- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
newtype JobTemplateListBy = JobTemplateListBy String
derive instance newtypeJobTemplateListBy :: Newtype JobTemplateListBy _


-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
newtype JobTemplateSettings = JobTemplateSettings 
  { "AdAvailOffset" :: NullOrUndefined (Int)
  , "AvailBlanking" :: NullOrUndefined (AvailBlanking)
  , "Inputs" :: NullOrUndefined (ListOfInputTemplate)
  , "NielsenConfiguration" :: NullOrUndefined (NielsenConfiguration)
  , "OutputGroups" :: NullOrUndefined (ListOfOutputGroup)
  , "TimecodeConfig" :: NullOrUndefined (TimecodeConfig)
  , "TimedMetadataInsertion" :: NullOrUndefined (TimedMetadataInsertion)
  }
derive instance newtypeJobTemplateSettings :: Newtype JobTemplateSettings _


-- | Code to specify the language, following the specification "ISO 639-2 three-digit code":http://www.loc.gov/standards/iso639-2/
newtype LanguageCode = LanguageCode String
derive instance newtypeLanguageCode :: Newtype LanguageCode _


newtype ListJobTemplatesRequest = ListJobTemplatesRequest 
  { "Category" :: NullOrUndefined (String)
  , "ListBy" :: NullOrUndefined (JobTemplateListBy)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Order" :: NullOrUndefined (Order)
  }
derive instance newtypeListJobTemplatesRequest :: Newtype ListJobTemplatesRequest _


newtype ListJobTemplatesResponse = ListJobTemplatesResponse 
  { "JobTemplates" :: NullOrUndefined (ListOfJobTemplate)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListJobTemplatesResponse :: Newtype ListJobTemplatesResponse _


newtype ListJobsRequest = ListJobsRequest 
  { "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Order" :: NullOrUndefined (Order)
  , "Queue" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (JobStatus)
  }
derive instance newtypeListJobsRequest :: Newtype ListJobsRequest _


newtype ListJobsResponse = ListJobsResponse 
  { "Jobs" :: NullOrUndefined (ListOfJob)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListJobsResponse :: Newtype ListJobsResponse _


newtype ListOfAudioDescription = ListOfAudioDescription (Array AudioDescription)
derive instance newtypeListOfAudioDescription :: Newtype ListOfAudioDescription _


newtype ListOfCaptionDescription = ListOfCaptionDescription (Array CaptionDescription)
derive instance newtypeListOfCaptionDescription :: Newtype ListOfCaptionDescription _


newtype ListOfCaptionDescriptionPreset = ListOfCaptionDescriptionPreset (Array CaptionDescriptionPreset)
derive instance newtypeListOfCaptionDescriptionPreset :: Newtype ListOfCaptionDescriptionPreset _


newtype ListOfEndpoint = ListOfEndpoint (Array Endpoint)
derive instance newtypeListOfEndpoint :: Newtype ListOfEndpoint _


newtype ListOfHlsAdMarkers = ListOfHlsAdMarkers (Array HlsAdMarkers)
derive instance newtypeListOfHlsAdMarkers :: Newtype ListOfHlsAdMarkers _


newtype ListOfHlsCaptionLanguageMapping = ListOfHlsCaptionLanguageMapping (Array HlsCaptionLanguageMapping)
derive instance newtypeListOfHlsCaptionLanguageMapping :: Newtype ListOfHlsCaptionLanguageMapping _


newtype ListOfId3Insertion = ListOfId3Insertion (Array Id3Insertion)
derive instance newtypeListOfId3Insertion :: Newtype ListOfId3Insertion _


newtype ListOfInput = ListOfInput (Array Input)
derive instance newtypeListOfInput :: Newtype ListOfInput _


newtype ListOfInputClipping = ListOfInputClipping (Array InputClipping)
derive instance newtypeListOfInputClipping :: Newtype ListOfInputClipping _


newtype ListOfInputTemplate = ListOfInputTemplate (Array InputTemplate)
derive instance newtypeListOfInputTemplate :: Newtype ListOfInputTemplate _


newtype ListOfInsertableImage = ListOfInsertableImage (Array InsertableImage)
derive instance newtypeListOfInsertableImage :: Newtype ListOfInsertableImage _


newtype ListOfJob = ListOfJob (Array Job)
derive instance newtypeListOfJob :: Newtype ListOfJob _


newtype ListOfJobTemplate = ListOfJobTemplate (Array JobTemplate)
derive instance newtypeListOfJobTemplate :: Newtype ListOfJobTemplate _


newtype ListOfOutput = ListOfOutput (Array Output)
derive instance newtypeListOfOutput :: Newtype ListOfOutput _


newtype ListOfOutputChannelMapping = ListOfOutputChannelMapping (Array OutputChannelMapping)
derive instance newtypeListOfOutputChannelMapping :: Newtype ListOfOutputChannelMapping _


newtype ListOfOutputDetail = ListOfOutputDetail (Array OutputDetail)
derive instance newtypeListOfOutputDetail :: Newtype ListOfOutputDetail _


newtype ListOfOutputGroup = ListOfOutputGroup (Array OutputGroup)
derive instance newtypeListOfOutputGroup :: Newtype ListOfOutputGroup _


newtype ListOfOutputGroupDetail = ListOfOutputGroupDetail (Array OutputGroupDetail)
derive instance newtypeListOfOutputGroupDetail :: Newtype ListOfOutputGroupDetail _


newtype ListOfPreset = ListOfPreset (Array Preset)
derive instance newtypeListOfPreset :: Newtype ListOfPreset _


newtype ListOfQueue = ListOfQueue (Array Queue)
derive instance newtypeListOfQueue :: Newtype ListOfQueue _


newtype ListOf__integer = ListOf__integer (Array Int)
derive instance newtypeListOf__integer :: Newtype ListOf__integer _


newtype ListOf__string = ListOf__string (Array String)
derive instance newtypeListOf__string :: Newtype ListOf__string _


newtype ListPresetsRequest = ListPresetsRequest 
  { "Category" :: NullOrUndefined (String)
  , "ListBy" :: NullOrUndefined (PresetListBy)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Order" :: NullOrUndefined (Order)
  }
derive instance newtypeListPresetsRequest :: Newtype ListPresetsRequest _


newtype ListPresetsResponse = ListPresetsResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Presets" :: NullOrUndefined (ListOfPreset)
  }
derive instance newtypeListPresetsResponse :: Newtype ListPresetsResponse _


newtype ListQueuesRequest = ListQueuesRequest 
  { "ListBy" :: NullOrUndefined (QueueListBy)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  , "Order" :: NullOrUndefined (Order)
  }
derive instance newtypeListQueuesRequest :: Newtype ListQueuesRequest _


newtype ListQueuesResponse = ListQueuesResponse 
  { "NextToken" :: NullOrUndefined (String)
  , "Queues" :: NullOrUndefined (ListOfQueue)
  }
derive instance newtypeListQueuesResponse :: Newtype ListQueuesResponse _


-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
newtype M2tsAudioBufferModel = M2tsAudioBufferModel String
derive instance newtypeM2tsAudioBufferModel :: Newtype M2tsAudioBufferModel _


-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
newtype M2tsBufferModel = M2tsBufferModel String
derive instance newtypeM2tsBufferModel :: Newtype M2tsBufferModel _


-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
newtype M2tsEbpAudioInterval = M2tsEbpAudioInterval String
derive instance newtypeM2tsEbpAudioInterval :: Newtype M2tsEbpAudioInterval _


-- | Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
newtype M2tsEbpPlacement = M2tsEbpPlacement String
derive instance newtypeM2tsEbpPlacement :: Newtype M2tsEbpPlacement _


-- | Controls whether to include the ES Rate field in the PES header.
newtype M2tsEsRateInPes = M2tsEsRateInPes String
derive instance newtypeM2tsEsRateInPes :: Newtype M2tsEsRateInPes _


-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
newtype M2tsNielsenId3 = M2tsNielsenId3 String
derive instance newtypeM2tsNielsenId3 :: Newtype M2tsNielsenId3 _


-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
newtype M2tsPcrControl = M2tsPcrControl String
derive instance newtypeM2tsPcrControl :: Newtype M2tsPcrControl _


-- | When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
newtype M2tsRateMode = M2tsRateMode String
derive instance newtypeM2tsRateMode :: Newtype M2tsRateMode _


-- | Enables SCTE-35 passthrough (scte35Source) to pass any SCTE-35 signals from input to output.
newtype M2tsScte35Source = M2tsScte35Source String
derive instance newtypeM2tsScte35Source :: Newtype M2tsScte35Source _


-- | Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers String
derive instance newtypeM2tsSegmentationMarkers :: Newtype M2tsSegmentationMarkers _


-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
newtype M2tsSegmentationStyle = M2tsSegmentationStyle String
derive instance newtypeM2tsSegmentationStyle :: Newtype M2tsSegmentationStyle _


-- | Settings for M2TS Container.
newtype M2tsSettings = M2tsSettings 
  { "AudioBufferModel" :: NullOrUndefined (M2tsAudioBufferModel)
  , "AudioFramesPerPes" :: NullOrUndefined (Int)
  , "AudioPids" :: NullOrUndefined (ListOf__integer)
  , "Bitrate" :: NullOrUndefined (Int)
  , "BufferModel" :: NullOrUndefined (M2tsBufferModel)
  , "DvbNitSettings" :: NullOrUndefined (DvbNitSettings)
  , "DvbSdtSettings" :: NullOrUndefined (DvbSdtSettings)
  , "DvbSubPids" :: NullOrUndefined (ListOf__integer)
  , "DvbTdtSettings" :: NullOrUndefined (DvbTdtSettings)
  , "DvbTeletextPid" :: NullOrUndefined (Int)
  , "EbpAudioInterval" :: NullOrUndefined (M2tsEbpAudioInterval)
  , "EbpPlacement" :: NullOrUndefined (M2tsEbpPlacement)
  , "EsRateInPes" :: NullOrUndefined (M2tsEsRateInPes)
  , "FragmentTime" :: NullOrUndefined (Number)
  , "MaxPcrInterval" :: NullOrUndefined (Int)
  , "MinEbpInterval" :: NullOrUndefined (Int)
  , "NielsenId3" :: NullOrUndefined (M2tsNielsenId3)
  , "NullPacketBitrate" :: NullOrUndefined (Number)
  , "PatInterval" :: NullOrUndefined (Int)
  , "PcrControl" :: NullOrUndefined (M2tsPcrControl)
  , "PcrPid" :: NullOrUndefined (Int)
  , "PmtInterval" :: NullOrUndefined (Int)
  , "PmtPid" :: NullOrUndefined (Int)
  , "PrivateMetadataPid" :: NullOrUndefined (Int)
  , "ProgramNumber" :: NullOrUndefined (Int)
  , "RateMode" :: NullOrUndefined (M2tsRateMode)
  , "Scte35Pid" :: NullOrUndefined (Int)
  , "Scte35Source" :: NullOrUndefined (M2tsScte35Source)
  , "SegmentationMarkers" :: NullOrUndefined (M2tsSegmentationMarkers)
  , "SegmentationStyle" :: NullOrUndefined (M2tsSegmentationStyle)
  , "SegmentationTime" :: NullOrUndefined (Number)
  , "TimedMetadataPid" :: NullOrUndefined (Int)
  , "TransportStreamId" :: NullOrUndefined (Int)
  , "VideoPid" :: NullOrUndefined (Int)
  }
derive instance newtypeM2tsSettings :: Newtype M2tsSettings _


-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
newtype M3u8NielsenId3 = M3u8NielsenId3 String
derive instance newtypeM3u8NielsenId3 :: Newtype M3u8NielsenId3 _


-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
newtype M3u8PcrControl = M3u8PcrControl String
derive instance newtypeM3u8PcrControl :: Newtype M3u8PcrControl _


-- | Enables SCTE-35 passthrough (scte35Source) to pass any SCTE-35 signals from input to output.
newtype M3u8Scte35Source = M3u8Scte35Source String
derive instance newtypeM3u8Scte35Source :: Newtype M3u8Scte35Source _


-- | Settings for TS segments in HLS
newtype M3u8Settings = M3u8Settings 
  { "AudioFramesPerPes" :: NullOrUndefined (Int)
  , "AudioPids" :: NullOrUndefined (ListOf__integer)
  , "NielsenId3" :: NullOrUndefined (M3u8NielsenId3)
  , "PatInterval" :: NullOrUndefined (Int)
  , "PcrControl" :: NullOrUndefined (M3u8PcrControl)
  , "PcrPid" :: NullOrUndefined (Int)
  , "PmtInterval" :: NullOrUndefined (Int)
  , "PmtPid" :: NullOrUndefined (Int)
  , "PrivateMetadataPid" :: NullOrUndefined (Int)
  , "ProgramNumber" :: NullOrUndefined (Int)
  , "Scte35Pid" :: NullOrUndefined (Int)
  , "Scte35Source" :: NullOrUndefined (M3u8Scte35Source)
  , "TimedMetadata" :: NullOrUndefined (TimedMetadata)
  , "TimedMetadataPid" :: NullOrUndefined (Int)
  , "TransportStreamId" :: NullOrUndefined (Int)
  , "VideoPid" :: NullOrUndefined (Int)
  }
derive instance newtypeM3u8Settings :: Newtype M3u8Settings _


newtype MapOfAudioSelector = MapOfAudioSelector (Map String AudioSelector)
derive instance newtypeMapOfAudioSelector :: Newtype MapOfAudioSelector _


newtype MapOfAudioSelectorGroup = MapOfAudioSelectorGroup (Map String AudioSelectorGroup)
derive instance newtypeMapOfAudioSelectorGroup :: Newtype MapOfAudioSelectorGroup _


newtype MapOfCaptionSelector = MapOfCaptionSelector (Map String CaptionSelector)
derive instance newtypeMapOfCaptionSelector :: Newtype MapOfCaptionSelector _


newtype MapOf__string = MapOf__string (Map String String)
derive instance newtypeMapOf__string :: Newtype MapOf__string _


-- | When enabled, include 'clap' atom if appropriate for the video output settings.
newtype MovClapAtom = MovClapAtom String
derive instance newtypeMovClapAtom :: Newtype MovClapAtom _


-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
newtype MovCslgAtom = MovCslgAtom String
derive instance newtypeMovCslgAtom :: Newtype MovCslgAtom _


-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
newtype MovMpeg2FourCCControl = MovMpeg2FourCCControl String
derive instance newtypeMovMpeg2FourCCControl :: Newtype MovMpeg2FourCCControl _


-- | If set to OMNEON, inserts Omneon-compatible padding
newtype MovPaddingControl = MovPaddingControl String
derive instance newtypeMovPaddingControl :: Newtype MovPaddingControl _


-- | A value of 'external' creates separate media files and the wrapper file (.mov) contains references to these media files. A value of 'self_contained' creates only a wrapper (.mov) file and this file contains all of the media.
newtype MovReference = MovReference String
derive instance newtypeMovReference :: Newtype MovReference _


-- | Settings for MOV Container.
newtype MovSettings = MovSettings 
  { "ClapAtom" :: NullOrUndefined (MovClapAtom)
  , "CslgAtom" :: NullOrUndefined (MovCslgAtom)
  , "Mpeg2FourCCControl" :: NullOrUndefined (MovMpeg2FourCCControl)
  , "PaddingControl" :: NullOrUndefined (MovPaddingControl)
  , "Reference" :: NullOrUndefined (MovReference)
  }
derive instance newtypeMovSettings :: Newtype MovSettings _


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
newtype Mp2Settings = Mp2Settings 
  { "Bitrate" :: NullOrUndefined (Int)
  , "Channels" :: NullOrUndefined (Int)
  , "SampleRate" :: NullOrUndefined (Int)
  }
derive instance newtypeMp2Settings :: Newtype Mp2Settings _


-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
newtype Mp4CslgAtom = Mp4CslgAtom String
derive instance newtypeMp4CslgAtom :: Newtype Mp4CslgAtom _


-- | Inserts a free-space box immediately after the moov box.
newtype Mp4FreeSpaceBox = Mp4FreeSpaceBox String
derive instance newtypeMp4FreeSpaceBox :: Newtype Mp4FreeSpaceBox _


-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
newtype Mp4MoovPlacement = Mp4MoovPlacement String
derive instance newtypeMp4MoovPlacement :: Newtype Mp4MoovPlacement _


-- | Settings for MP4 Container
newtype Mp4Settings = Mp4Settings 
  { "CslgAtom" :: NullOrUndefined (Mp4CslgAtom)
  , "FreeSpaceBox" :: NullOrUndefined (Mp4FreeSpaceBox)
  , "MoovPlacement" :: NullOrUndefined (Mp4MoovPlacement)
  , "Mp4MajorBrand" :: NullOrUndefined (String)
  }
derive instance newtypeMp4Settings :: Newtype Mp4Settings _


-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
newtype Mpeg2AdaptiveQuantization = Mpeg2AdaptiveQuantization String
derive instance newtypeMpeg2AdaptiveQuantization :: Newtype Mpeg2AdaptiveQuantization _


-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
newtype Mpeg2CodecLevel = Mpeg2CodecLevel String
derive instance newtypeMpeg2CodecLevel :: Newtype Mpeg2CodecLevel _


-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
newtype Mpeg2CodecProfile = Mpeg2CodecProfile String
derive instance newtypeMpeg2CodecProfile :: Newtype Mpeg2CodecProfile _


-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
newtype Mpeg2FramerateControl = Mpeg2FramerateControl String
derive instance newtypeMpeg2FramerateControl :: Newtype Mpeg2FramerateControl _


-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
newtype Mpeg2FramerateConversionAlgorithm = Mpeg2FramerateConversionAlgorithm String
derive instance newtypeMpeg2FramerateConversionAlgorithm :: Newtype Mpeg2FramerateConversionAlgorithm _


-- | Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
newtype Mpeg2GopSizeUnits = Mpeg2GopSizeUnits String
derive instance newtypeMpeg2GopSizeUnits :: Newtype Mpeg2GopSizeUnits _


-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
newtype Mpeg2InterlaceMode = Mpeg2InterlaceMode String
derive instance newtypeMpeg2InterlaceMode :: Newtype Mpeg2InterlaceMode _


-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
newtype Mpeg2IntraDcPrecision = Mpeg2IntraDcPrecision String
derive instance newtypeMpeg2IntraDcPrecision :: Newtype Mpeg2IntraDcPrecision _


-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
newtype Mpeg2ParControl = Mpeg2ParControl String
derive instance newtypeMpeg2ParControl :: Newtype Mpeg2ParControl _


-- | Use Quality tuning level (Mpeg2QualityTuningLevel) to specifiy whether to use single-pass or multipass video encoding.
newtype Mpeg2QualityTuningLevel = Mpeg2QualityTuningLevel String
derive instance newtypeMpeg2QualityTuningLevel :: Newtype Mpeg2QualityTuningLevel _


-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
newtype Mpeg2RateControlMode = Mpeg2RateControlMode String
derive instance newtypeMpeg2RateControlMode :: Newtype Mpeg2RateControlMode _


-- | Scene change detection (inserts I-frames on scene changes).
newtype Mpeg2SceneChangeDetect = Mpeg2SceneChangeDetect String
derive instance newtypeMpeg2SceneChangeDetect :: Newtype Mpeg2SceneChangeDetect _


-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
newtype Mpeg2Settings = Mpeg2Settings 
  { "AdaptiveQuantization" :: NullOrUndefined (Mpeg2AdaptiveQuantization)
  , "Bitrate" :: NullOrUndefined (Int)
  , "CodecLevel" :: NullOrUndefined (Mpeg2CodecLevel)
  , "CodecProfile" :: NullOrUndefined (Mpeg2CodecProfile)
  , "FramerateControl" :: NullOrUndefined (Mpeg2FramerateControl)
  , "FramerateConversionAlgorithm" :: NullOrUndefined (Mpeg2FramerateConversionAlgorithm)
  , "FramerateDenominator" :: NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined (Int)
  , "GopClosedCadence" :: NullOrUndefined (Int)
  , "GopSize" :: NullOrUndefined (Number)
  , "GopSizeUnits" :: NullOrUndefined (Mpeg2GopSizeUnits)
  , "HrdBufferInitialFillPercentage" :: NullOrUndefined (Int)
  , "HrdBufferSize" :: NullOrUndefined (Int)
  , "InterlaceMode" :: NullOrUndefined (Mpeg2InterlaceMode)
  , "IntraDcPrecision" :: NullOrUndefined (Mpeg2IntraDcPrecision)
  , "MaxBitrate" :: NullOrUndefined (Int)
  , "MinIInterval" :: NullOrUndefined (Int)
  , "NumberBFramesBetweenReferenceFrames" :: NullOrUndefined (Int)
  , "ParControl" :: NullOrUndefined (Mpeg2ParControl)
  , "ParDenominator" :: NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined (Int)
  , "QualityTuningLevel" :: NullOrUndefined (Mpeg2QualityTuningLevel)
  , "RateControlMode" :: NullOrUndefined (Mpeg2RateControlMode)
  , "SceneChangeDetect" :: NullOrUndefined (Mpeg2SceneChangeDetect)
  , "SlowPal" :: NullOrUndefined (Mpeg2SlowPal)
  , "Softness" :: NullOrUndefined (Int)
  , "SpatialAdaptiveQuantization" :: NullOrUndefined (Mpeg2SpatialAdaptiveQuantization)
  , "Syntax" :: NullOrUndefined (Mpeg2Syntax)
  , "Telecine" :: NullOrUndefined (Mpeg2Telecine)
  , "TemporalAdaptiveQuantization" :: NullOrUndefined (Mpeg2TemporalAdaptiveQuantization)
  }
derive instance newtypeMpeg2Settings :: Newtype Mpeg2Settings _


-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
newtype Mpeg2SlowPal = Mpeg2SlowPal String
derive instance newtypeMpeg2SlowPal :: Newtype Mpeg2SlowPal _


-- | Adjust quantization within each frame based on spatial variation of content complexity.
newtype Mpeg2SpatialAdaptiveQuantization = Mpeg2SpatialAdaptiveQuantization String
derive instance newtypeMpeg2SpatialAdaptiveQuantization :: Newtype Mpeg2SpatialAdaptiveQuantization _


-- | Produces a Type D-10 compatible bitstream (SMPTE 356M-2001).
newtype Mpeg2Syntax = Mpeg2Syntax String
derive instance newtypeMpeg2Syntax :: Newtype Mpeg2Syntax _


-- | Only use Telecine (Mpeg2Telecine) when you set Framerate (Framerate) to 29.970. Set Telecine (Mpeg2Telecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
newtype Mpeg2Telecine = Mpeg2Telecine String
derive instance newtypeMpeg2Telecine :: Newtype Mpeg2Telecine _


-- | Adjust quantization within each frame based on temporal variation of content complexity.
newtype Mpeg2TemporalAdaptiveQuantization = Mpeg2TemporalAdaptiveQuantization String
derive instance newtypeMpeg2TemporalAdaptiveQuantization :: Newtype Mpeg2TemporalAdaptiveQuantization _


-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
newtype MsSmoothAudioDeduplication = MsSmoothAudioDeduplication String
derive instance newtypeMsSmoothAudioDeduplication :: Newtype MsSmoothAudioDeduplication _


-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
newtype MsSmoothEncryptionSettings = MsSmoothEncryptionSettings 
  { "SpekeKeyProvider" :: NullOrUndefined (SpekeKeyProvider)
  }
derive instance newtypeMsSmoothEncryptionSettings :: Newtype MsSmoothEncryptionSettings _


-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.
newtype MsSmoothGroupSettings = MsSmoothGroupSettings 
  { "AudioDeduplication" :: NullOrUndefined (MsSmoothAudioDeduplication)
  , "Destination" :: NullOrUndefined (String)
  , "Encryption" :: NullOrUndefined (MsSmoothEncryptionSettings)
  , "FragmentLength" :: NullOrUndefined (Int)
  , "ManifestEncoding" :: NullOrUndefined (MsSmoothManifestEncoding)
  }
derive instance newtypeMsSmoothGroupSettings :: Newtype MsSmoothGroupSettings _


-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
newtype MsSmoothManifestEncoding = MsSmoothManifestEncoding String
derive instance newtypeMsSmoothManifestEncoding :: Newtype MsSmoothManifestEncoding _


-- | Settings for Nielsen Configuration
newtype NielsenConfiguration = NielsenConfiguration 
  { "BreakoutCode" :: NullOrUndefined (Int)
  , "DistributorId" :: NullOrUndefined (String)
  }
derive instance newtypeNielsenConfiguration :: Newtype NielsenConfiguration _


-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default. When you enable Noise reducer (NoiseReducer), you must also select a value for Noise reducer filter (NoiseReducerFilter).
newtype NoiseReducer = NoiseReducer 
  { "Filter" :: NullOrUndefined (NoiseReducerFilter)
  , "FilterSettings" :: NullOrUndefined (NoiseReducerFilterSettings)
  , "SpatialFilterSettings" :: NullOrUndefined (NoiseReducerSpatialFilterSettings)
  }
derive instance newtypeNoiseReducer :: Newtype NoiseReducer _


-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral is an edge preserving noise reduction filter * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) are convolution filters * Conserve is a min/max noise reduction filter * Spatial is frequency-domain filter based on JND principles.
newtype NoiseReducerFilter = NoiseReducerFilter String
derive instance newtypeNoiseReducerFilter :: Newtype NoiseReducerFilter _


-- | Settings for a noise reducer filter
newtype NoiseReducerFilterSettings = NoiseReducerFilterSettings 
  { "Strength" :: NullOrUndefined (Int)
  }
derive instance newtypeNoiseReducerFilterSettings :: Newtype NoiseReducerFilterSettings _


-- | Noise reducer filter settings for spatial filter.
newtype NoiseReducerSpatialFilterSettings = NoiseReducerSpatialFilterSettings 
  { "PostFilterSharpenStrength" :: NullOrUndefined (Int)
  , "Speed" :: NullOrUndefined (Int)
  , "Strength" :: NullOrUndefined (Int)
  }
derive instance newtypeNoiseReducerSpatialFilterSettings :: Newtype NoiseReducerSpatialFilterSettings _


-- | The resource you requested does not exist.
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


-- | When you request lists of resources, you can optionally specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
newtype Order = Order String
derive instance newtypeOrder :: Newtype Order _


-- | An output object describes the settings for a single output file or stream in an output group.
newtype Output = Output 
  { "AudioDescriptions" :: NullOrUndefined (ListOfAudioDescription)
  , "CaptionDescriptions" :: NullOrUndefined (ListOfCaptionDescription)
  , "ContainerSettings" :: NullOrUndefined (ContainerSettings)
  , "Extension" :: NullOrUndefined (String)
  , "NameModifier" :: NullOrUndefined (String)
  , "OutputSettings" :: NullOrUndefined (OutputSettings)
  , "Preset" :: NullOrUndefined (String)
  , "VideoDescription" :: NullOrUndefined (VideoDescription)
  }
derive instance newtypeOutput :: Newtype Output _


-- | OutputChannel mapping settings.
newtype OutputChannelMapping = OutputChannelMapping 
  { "InputChannels" :: NullOrUndefined (ListOf__integer)
  }
derive instance newtypeOutputChannelMapping :: Newtype OutputChannelMapping _


-- | Details regarding output
newtype OutputDetail = OutputDetail 
  { "DurationInMs" :: NullOrUndefined (Int)
  , "VideoDetails" :: NullOrUndefined (VideoDetail)
  }
derive instance newtypeOutputDetail :: Newtype OutputDetail _


-- | Group of outputs
newtype OutputGroup = OutputGroup 
  { "CustomName" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "OutputGroupSettings" :: NullOrUndefined (OutputGroupSettings)
  , "Outputs" :: NullOrUndefined (ListOfOutput)
  }
derive instance newtypeOutputGroup :: Newtype OutputGroup _


-- | Contains details about the output groups specified in the job settings.
newtype OutputGroupDetail = OutputGroupDetail 
  { "OutputDetails" :: NullOrUndefined (ListOfOutputDetail)
  }
derive instance newtypeOutputGroupDetail :: Newtype OutputGroupDetail _


-- | Output Group settings, including type
newtype OutputGroupSettings = OutputGroupSettings 
  { "DashIsoGroupSettings" :: NullOrUndefined (DashIsoGroupSettings)
  , "FileGroupSettings" :: NullOrUndefined (FileGroupSettings)
  , "HlsGroupSettings" :: NullOrUndefined (HlsGroupSettings)
  , "MsSmoothGroupSettings" :: NullOrUndefined (MsSmoothGroupSettings)
  , "Type" :: NullOrUndefined (OutputGroupType)
  }
derive instance newtypeOutputGroupSettings :: Newtype OutputGroupSettings _


-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming)
newtype OutputGroupType = OutputGroupType String
derive instance newtypeOutputGroupType :: Newtype OutputGroupType _


-- | Selects method of inserting SDT information into output stream.  "Follow input SDT" copies SDT information from input stream to  output stream. "Follow input SDT if present" copies SDT information from  input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. Enter "SDT  Manually" means user will enter the SDT information. "No SDT" means output  stream will not contain SDT information.
newtype OutputSdt = OutputSdt String
derive instance newtypeOutputSdt :: Newtype OutputSdt _


-- | Specific settings for this type of output.
newtype OutputSettings = OutputSettings 
  { "HlsSettings" :: NullOrUndefined (HlsSettings)
  }
derive instance newtypeOutputSettings :: Newtype OutputSettings _


-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
newtype Preset = Preset 
  { "Arn" :: NullOrUndefined (String)
  , "Category" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (Number)
  , "Description" :: NullOrUndefined (String)
  , "LastUpdated" :: NullOrUndefined (Number)
  , "Name" :: NullOrUndefined (String)
  , "Settings" :: NullOrUndefined (PresetSettings)
  , "Type" :: NullOrUndefined (Type)
  }
derive instance newtypePreset :: Newtype Preset _


-- | Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
newtype PresetListBy = PresetListBy String
derive instance newtypePresetListBy :: Newtype PresetListBy _


-- | Settings for preset
newtype PresetSettings = PresetSettings 
  { "AudioDescriptions" :: NullOrUndefined (ListOfAudioDescription)
  , "CaptionDescriptions" :: NullOrUndefined (ListOfCaptionDescriptionPreset)
  , "ContainerSettings" :: NullOrUndefined (ContainerSettings)
  , "VideoDescription" :: NullOrUndefined (VideoDescription)
  }
derive instance newtypePresetSettings :: Newtype PresetSettings _


-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
newtype ProresCodecProfile = ProresCodecProfile String
derive instance newtypeProresCodecProfile :: Newtype ProresCodecProfile _


-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
newtype ProresFramerateControl = ProresFramerateControl String
derive instance newtypeProresFramerateControl :: Newtype ProresFramerateControl _


-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
newtype ProresFramerateConversionAlgorithm = ProresFramerateConversionAlgorithm String
derive instance newtypeProresFramerateConversionAlgorithm :: Newtype ProresFramerateConversionAlgorithm _


-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
newtype ProresInterlaceMode = ProresInterlaceMode String
derive instance newtypeProresInterlaceMode :: Newtype ProresInterlaceMode _


-- | Use (ProresParControl) to specify how the service determines the pixel aspect ratio. Set to Follow source (INITIALIZE_FROM_SOURCE) to use the pixel aspect ratio from the input.  To specify a different pixel aspect ratio: Using the console, choose it from the dropdown menu. Using the API, set ProresParControl to (SPECIFIED) and provide  for (ParNumerator) and (ParDenominator).
newtype ProresParControl = ProresParControl String
derive instance newtypeProresParControl :: Newtype ProresParControl _


-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
newtype ProresSettings = ProresSettings 
  { "CodecProfile" :: NullOrUndefined (ProresCodecProfile)
  , "FramerateControl" :: NullOrUndefined (ProresFramerateControl)
  , "FramerateConversionAlgorithm" :: NullOrUndefined (ProresFramerateConversionAlgorithm)
  , "FramerateDenominator" :: NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined (Int)
  , "InterlaceMode" :: NullOrUndefined (ProresInterlaceMode)
  , "ParControl" :: NullOrUndefined (ProresParControl)
  , "ParDenominator" :: NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined (Int)
  , "SlowPal" :: NullOrUndefined (ProresSlowPal)
  , "Telecine" :: NullOrUndefined (ProresTelecine)
  }
derive instance newtypeProresSettings :: Newtype ProresSettings _


-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
newtype ProresSlowPal = ProresSlowPal String
derive instance newtypeProresSlowPal :: Newtype ProresSlowPal _


-- | Only use Telecine (ProresTelecine) when you set Framerate (Framerate) to 29.970. Set Telecine (ProresTelecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
newtype ProresTelecine = ProresTelecine String
derive instance newtypeProresTelecine :: Newtype ProresTelecine _


-- | MediaConvert jobs are submitted to a queue. Unless specified otherwise jobs are submitted to a built-in default queue. User can create additional queues to separate the jobs of different categories or priority.
newtype Queue = Queue 
  { "Arn" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (Number)
  , "Description" :: NullOrUndefined (String)
  , "LastUpdated" :: NullOrUndefined (Number)
  , "Name" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (QueueStatus)
  , "Type" :: NullOrUndefined (Type)
  }
derive instance newtypeQueue :: Newtype Queue _


-- | Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.
newtype QueueListBy = QueueListBy String
derive instance newtypeQueueListBy :: Newtype QueueListBy _


-- | Queues can be ACTIVE or PAUSED. If you pause a queue, jobs in that queue will not begin. Jobs running when a queue is paused continue to run until they finish or error out.
newtype QueueStatus = QueueStatus String
derive instance newtypeQueueStatus :: Newtype QueueStatus _


-- | Use Rectangle to identify a specific area of the video frame.
newtype Rectangle = Rectangle 
  { "Height" :: NullOrUndefined (Int)
  , "Width" :: NullOrUndefined (Int)
  , "X" :: NullOrUndefined (Int)
  , "Y" :: NullOrUndefined (Int)
  }
derive instance newtypeRectangle :: Newtype Rectangle _


-- | Use Manual audio remixing (RemixSettings) to adjust audio levels for each output channel. With audio remixing, you can output more or fewer audio channels than your input audio source provides.
newtype RemixSettings = RemixSettings 
  { "ChannelMapping" :: NullOrUndefined (ChannelMapping)
  , "ChannelsIn" :: NullOrUndefined (Int)
  , "ChannelsOut" :: NullOrUndefined (Int)
  }
derive instance newtypeRemixSettings :: Newtype RemixSettings _


-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.
newtype RespondToAfd = RespondToAfd String
derive instance newtypeRespondToAfd :: Newtype RespondToAfd _


-- | Applies only if your input aspect ratio is different from your output aspect ratio. Enable Stretch to output (StretchToOutput) to have the service stretch your video image to fit. Leave this setting disabled to allow the service to letterbox your video instead. This setting overrides any positioning value you specify elsewhere in the job.
newtype ScalingBehavior = ScalingBehavior String
derive instance newtypeScalingBehavior :: Newtype ScalingBehavior _


-- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a framerate that matches the framerate of the associated video. If the video framerate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
newtype SccDestinationFramerate = SccDestinationFramerate String
derive instance newtypeSccDestinationFramerate :: Newtype SccDestinationFramerate _


-- | Settings for SCC caption output.
newtype SccDestinationSettings = SccDestinationSettings 
  { "Framerate" :: NullOrUndefined (SccDestinationFramerate)
  }
derive instance newtypeSccDestinationSettings :: Newtype SccDestinationSettings _


-- | Settings for use with a SPEKE key provider
newtype SpekeKeyProvider = SpekeKeyProvider 
  { "ResourceId" :: NullOrUndefined (String)
  , "SystemIds" :: NullOrUndefined (ListOf__string)
  , "Url" :: NullOrUndefined (String)
  }
derive instance newtypeSpekeKeyProvider :: Newtype SpekeKeyProvider _


-- | Settings for use with a SPEKE key provider.
newtype StaticKeyProvider = StaticKeyProvider 
  { "KeyFormat" :: NullOrUndefined (String)
  , "KeyFormatVersions" :: NullOrUndefined (String)
  , "StaticKeyValue" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  }
derive instance newtypeStaticKeyProvider :: Newtype StaticKeyProvider _


-- | Settings for Teletext caption output
newtype TeletextDestinationSettings = TeletextDestinationSettings 
  { "PageNumber" :: NullOrUndefined (String)
  }
derive instance newtypeTeletextDestinationSettings :: Newtype TeletextDestinationSettings _


-- | Settings specific to Teletext caption sources, including Page number.
newtype TeletextSourceSettings = TeletextSourceSettings 
  { "PageNumber" :: NullOrUndefined (String)
  }
derive instance newtypeTeletextSourceSettings :: Newtype TeletextSourceSettings _


-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
newtype TimecodeBurnin = TimecodeBurnin 
  { "FontSize" :: NullOrUndefined (Int)
  , "Position" :: NullOrUndefined (TimecodeBurninPosition)
  , "Prefix" :: NullOrUndefined (String)
  }
derive instance newtypeTimecodeBurnin :: Newtype TimecodeBurnin _


-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
newtype TimecodeBurninPosition = TimecodeBurninPosition String
derive instance newtypeTimecodeBurninPosition :: Newtype TimecodeBurninPosition _


-- | Contains settings used to acquire and adjust timecode information from inputs.
newtype TimecodeConfig = TimecodeConfig 
  { "Anchor" :: NullOrUndefined (String)
  , "Source" :: NullOrUndefined (TimecodeSource)
  , "Start" :: NullOrUndefined (String)
  , "TimestampOffset" :: NullOrUndefined (String)
  }
derive instance newtypeTimecodeConfig :: Newtype TimecodeConfig _


-- | Use Timecode source (TimecodeSource) to set how timecodes are handled within this input. To make sure that your video, audio, captions, and markers are synchronized and that time-based features, such as image inserter, work correctly, choose the Timecode source option that matches your assets. All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). * Embedded (EMBEDDED) - Use the timecode that is in the input video. If no embedded timecode is in the source, the service will use Start at 0 (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set the timecode of the initial frame to a value other than zero. You use Start timecode (Start) to provide this value.
newtype TimecodeSource = TimecodeSource String
derive instance newtypeTimecodeSource :: Newtype TimecodeSource _


-- | If PASSTHROUGH, inserts ID3 timed metadata from the timed_metadata REST command into this output.
newtype TimedMetadata = TimedMetadata String
derive instance newtypeTimedMetadata :: Newtype TimedMetadata _


-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in your job. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
newtype TimedMetadataInsertion = TimedMetadataInsertion 
  { "Id3Insertions" :: NullOrUndefined (ListOfId3Insertion)
  }
derive instance newtypeTimedMetadataInsertion :: Newtype TimedMetadataInsertion _


-- | Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
newtype Timing = Timing 
  { "FinishTime" :: NullOrUndefined (Number)
  , "StartTime" :: NullOrUndefined (Number)
  , "SubmitTime" :: NullOrUndefined (Number)
  }
derive instance newtypeTiming :: Newtype Timing _


-- | Too many requests have been sent in too short of a time. The service limits the rate at which it will accept requests.
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


-- | Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
newtype TtmlDestinationSettings = TtmlDestinationSettings 
  { "StylePassthrough" :: NullOrUndefined (TtmlStylePassthrough)
  }
derive instance newtypeTtmlDestinationSettings :: Newtype TtmlDestinationSettings _


-- | Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
newtype TtmlStylePassthrough = TtmlStylePassthrough String
derive instance newtypeTtmlStylePassthrough :: Newtype TtmlStylePassthrough _


newtype Type = Type String
derive instance newtypeType :: Newtype Type _


newtype UpdateJobTemplateRequest = UpdateJobTemplateRequest 
  { "Category" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Name" :: (String)
  , "Queue" :: NullOrUndefined (String)
  , "Settings" :: NullOrUndefined (JobTemplateSettings)
  }
derive instance newtypeUpdateJobTemplateRequest :: Newtype UpdateJobTemplateRequest _


newtype UpdateJobTemplateResponse = UpdateJobTemplateResponse 
  { "JobTemplate" :: NullOrUndefined (JobTemplate)
  }
derive instance newtypeUpdateJobTemplateResponse :: Newtype UpdateJobTemplateResponse _


newtype UpdatePresetRequest = UpdatePresetRequest 
  { "Category" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "Name" :: (String)
  , "Settings" :: NullOrUndefined (PresetSettings)
  }
derive instance newtypeUpdatePresetRequest :: Newtype UpdatePresetRequest _


newtype UpdatePresetResponse = UpdatePresetResponse 
  { "Preset" :: NullOrUndefined (Preset)
  }
derive instance newtypeUpdatePresetResponse :: Newtype UpdatePresetResponse _


newtype UpdateQueueRequest = UpdateQueueRequest 
  { "Description" :: NullOrUndefined (String)
  , "Name" :: (String)
  , "Status" :: NullOrUndefined (QueueStatus)
  }
derive instance newtypeUpdateQueueRequest :: Newtype UpdateQueueRequest _


newtype UpdateQueueResponse = UpdateQueueResponse 
  { "Queue" :: NullOrUndefined (Queue)
  }
derive instance newtypeUpdateQueueResponse :: Newtype UpdateQueueResponse _


-- | Type of video codec
newtype VideoCodec = VideoCodec String
derive instance newtypeVideoCodec :: Newtype VideoCodec _


-- | Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value you choose for Video codec (Codec). For each codec enum you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * FRAME_CAPTURE, FrameCaptureSettings
newtype VideoCodecSettings = VideoCodecSettings 
  { "Codec" :: NullOrUndefined (VideoCodec)
  , "FrameCaptureSettings" :: NullOrUndefined (FrameCaptureSettings)
  , "H264Settings" :: NullOrUndefined (H264Settings)
  , "H265Settings" :: NullOrUndefined (H265Settings)
  , "Mpeg2Settings" :: NullOrUndefined (Mpeg2Settings)
  , "ProresSettings" :: NullOrUndefined (ProresSettings)
  }
derive instance newtypeVideoCodecSettings :: Newtype VideoCodecSettings _


-- | Settings for video outputs
newtype VideoDescription = VideoDescription 
  { "AfdSignaling" :: NullOrUndefined (AfdSignaling)
  , "AntiAlias" :: NullOrUndefined (AntiAlias)
  , "CodecSettings" :: NullOrUndefined (VideoCodecSettings)
  , "ColorMetadata" :: NullOrUndefined (ColorMetadata)
  , "Crop" :: NullOrUndefined (Rectangle)
  , "DropFrameTimecode" :: NullOrUndefined (DropFrameTimecode)
  , "FixedAfd" :: NullOrUndefined (Int)
  , "Height" :: NullOrUndefined (Int)
  , "Position" :: NullOrUndefined (Rectangle)
  , "RespondToAfd" :: NullOrUndefined (RespondToAfd)
  , "ScalingBehavior" :: NullOrUndefined (ScalingBehavior)
  , "Sharpness" :: NullOrUndefined (Int)
  , "TimecodeInsertion" :: NullOrUndefined (VideoTimecodeInsertion)
  , "VideoPreprocessors" :: NullOrUndefined (VideoPreprocessor)
  , "Width" :: NullOrUndefined (Int)
  }
derive instance newtypeVideoDescription :: Newtype VideoDescription _


-- | Contains details about the output's video stream
newtype VideoDetail = VideoDetail 
  { "HeightInPx" :: NullOrUndefined (Int)
  , "WidthInPx" :: NullOrUndefined (Int)
  }
derive instance newtypeVideoDetail :: Newtype VideoDetail _


-- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
newtype VideoPreprocessor = VideoPreprocessor 
  { "ColorCorrector" :: NullOrUndefined (ColorCorrector)
  , "Deinterlacer" :: NullOrUndefined (Deinterlacer)
  , "ImageInserter" :: NullOrUndefined (ImageInserter)
  , "NoiseReducer" :: NullOrUndefined (NoiseReducer)
  , "TimecodeBurnin" :: NullOrUndefined (TimecodeBurnin)
  }
derive instance newtypeVideoPreprocessor :: Newtype VideoPreprocessor _


-- | Selector for video.
newtype VideoSelector = VideoSelector 
  { "ColorSpace" :: NullOrUndefined (ColorSpace)
  , "ColorSpaceUsage" :: NullOrUndefined (ColorSpaceUsage)
  , "Hdr10Metadata" :: NullOrUndefined (Hdr10Metadata)
  , "Pid" :: NullOrUndefined (Int)
  , "ProgramNumber" :: NullOrUndefined (Int)
  }
derive instance newtypeVideoSelector :: Newtype VideoSelector _


-- | Enable Timecode insertion to include timecode information in this output. Do this in the API by setting (VideoTimecodeInsertion) to (PIC_TIMING_SEI). To get timecodes to appear correctly in your output, also set up the timecode configuration for your job in the input settings. Only enable Timecode insertion when the input framerate is identical to output framerate. Disable this setting to remove the timecode from the output. Default is disabled.
newtype VideoTimecodeInsertion = VideoTimecodeInsertion String
derive instance newtypeVideoTimecodeInsertion :: Newtype VideoTimecodeInsertion _


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
newtype WavSettings = WavSettings 
  { "BitDepth" :: NullOrUndefined (Int)
  , "Channels" :: NullOrUndefined (Int)
  , "SampleRate" :: NullOrUndefined (Int)
  }
derive instance newtypeWavSettings :: Newtype WavSettings _
