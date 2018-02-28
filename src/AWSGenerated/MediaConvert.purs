

-- | AWS Elemental MediaConvert
module AWS.MediaConvert where

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

serviceName = "MediaConvert" :: String


-- | Permanently remove a job from a queue. Once you have canceled a job, you can't start it again. You can't delete a running job.
cancelJob :: forall eff. CancelJobRequest -> Aff (exception :: EXCEPTION | eff) CancelJobResponse
cancelJob = Request.request serviceName "cancelJob" 


-- | Create a new transcoding job. For information about jobs and job settings, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
createJob :: forall eff. CreateJobRequest -> Aff (exception :: EXCEPTION | eff) CreateJobResponse
createJob = Request.request serviceName "createJob" 


-- | Create a new job template. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
createJobTemplate :: forall eff. CreateJobTemplateRequest -> Aff (exception :: EXCEPTION | eff) CreateJobTemplateResponse
createJobTemplate = Request.request serviceName "createJobTemplate" 


-- | Create a new preset. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
createPreset :: forall eff. CreatePresetRequest -> Aff (exception :: EXCEPTION | eff) CreatePresetResponse
createPreset = Request.request serviceName "createPreset" 


-- | Create a new transcoding queue. For information about job templates see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
createQueue :: forall eff. CreateQueueRequest -> Aff (exception :: EXCEPTION | eff) CreateQueueResponse
createQueue = Request.request serviceName "createQueue" 


-- | Permanently delete a job template you have created.
deleteJobTemplate :: forall eff. DeleteJobTemplateRequest -> Aff (exception :: EXCEPTION | eff) DeleteJobTemplateResponse
deleteJobTemplate = Request.request serviceName "deleteJobTemplate" 


-- | Permanently delete a preset you have created.
deletePreset :: forall eff. DeletePresetRequest -> Aff (exception :: EXCEPTION | eff) DeletePresetResponse
deletePreset = Request.request serviceName "deletePreset" 


-- | Permanently delete a queue you have created.
deleteQueue :: forall eff. DeleteQueueRequest -> Aff (exception :: EXCEPTION | eff) DeleteQueueResponse
deleteQueue = Request.request serviceName "deleteQueue" 


-- | Send an request with an empty body to the regional API endpoint to get your account API endpoint.
describeEndpoints :: forall eff. DescribeEndpointsRequest -> Aff (exception :: EXCEPTION | eff) DescribeEndpointsResponse
describeEndpoints = Request.request serviceName "describeEndpoints" 


-- | Retrieve the JSON for a specific completed transcoding job.
getJob :: forall eff. GetJobRequest -> Aff (exception :: EXCEPTION | eff) GetJobResponse
getJob = Request.request serviceName "getJob" 


-- | Retrieve the JSON for a specific job template.
getJobTemplate :: forall eff. GetJobTemplateRequest -> Aff (exception :: EXCEPTION | eff) GetJobTemplateResponse
getJobTemplate = Request.request serviceName "getJobTemplate" 


-- | Retrieve the JSON for a specific preset.
getPreset :: forall eff. GetPresetRequest -> Aff (exception :: EXCEPTION | eff) GetPresetResponse
getPreset = Request.request serviceName "getPreset" 


-- | Retrieve the JSON for a specific queue.
getQueue :: forall eff. GetQueueRequest -> Aff (exception :: EXCEPTION | eff) GetQueueResponse
getQueue = Request.request serviceName "getQueue" 


-- | Retrieve a JSON array of up to twenty of your job templates. This will return the templates themselves, not just a list of them. To retrieve the next twenty templates, use the nextToken string returned with the array
listJobTemplates :: forall eff. ListJobTemplatesRequest -> Aff (exception :: EXCEPTION | eff) ListJobTemplatesResponse
listJobTemplates = Request.request serviceName "listJobTemplates" 


-- | Retrieve a JSON array of up to twenty of your most recently created jobs. This array includes in-process, completed, and errored jobs. This will return the jobs themselves, not just a list of the jobs. To retrieve the twenty next most recent jobs, use the nextToken string returned with the array.
listJobs :: forall eff. ListJobsRequest -> Aff (exception :: EXCEPTION | eff) ListJobsResponse
listJobs = Request.request serviceName "listJobs" 


-- | Retrieve a JSON array of up to twenty of your presets. This will return the presets themselves, not just a list of them. To retrieve the next twenty presets, use the nextToken string returned with the array.
listPresets :: forall eff. ListPresetsRequest -> Aff (exception :: EXCEPTION | eff) ListPresetsResponse
listPresets = Request.request serviceName "listPresets" 


-- | Retrieve a JSON array of up to twenty of your queues. This will return the queues themselves, not just a list of them. To retrieve the next twenty queues, use the nextToken string returned with the array.
listQueues :: forall eff. ListQueuesRequest -> Aff (exception :: EXCEPTION | eff) ListQueuesResponse
listQueues = Request.request serviceName "listQueues" 


-- | Modify one of your existing job templates.
updateJobTemplate :: forall eff. UpdateJobTemplateRequest -> Aff (exception :: EXCEPTION | eff) UpdateJobTemplateResponse
updateJobTemplate = Request.request serviceName "updateJobTemplate" 


-- | Modify one of your existing presets.
updatePreset :: forall eff. UpdatePresetRequest -> Aff (exception :: EXCEPTION | eff) UpdatePresetResponse
updatePreset = Request.request serviceName "updatePreset" 


-- | Modify one of your existing queues.
updateQueue :: forall eff. UpdateQueueRequest -> Aff (exception :: EXCEPTION | eff) UpdateQueueResponse
updateQueue = Request.request serviceName "updateQueue" 


-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
newtype AacAudioDescriptionBroadcasterMix = AacAudioDescriptionBroadcasterMix String
derive instance newtypeAacAudioDescriptionBroadcasterMix :: Newtype AacAudioDescriptionBroadcasterMix _
derive instance repGenericAacAudioDescriptionBroadcasterMix :: Generic AacAudioDescriptionBroadcasterMix _
instance showAacAudioDescriptionBroadcasterMix :: Show AacAudioDescriptionBroadcasterMix where
  show = genericShow
instance decodeAacAudioDescriptionBroadcasterMix :: Decode AacAudioDescriptionBroadcasterMix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacAudioDescriptionBroadcasterMix :: Encode AacAudioDescriptionBroadcasterMix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | AAC Profile.
newtype AacCodecProfile = AacCodecProfile String
derive instance newtypeAacCodecProfile :: Newtype AacCodecProfile _
derive instance repGenericAacCodecProfile :: Generic AacCodecProfile _
instance showAacCodecProfile :: Show AacCodecProfile where
  show = genericShow
instance decodeAacCodecProfile :: Decode AacCodecProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacCodecProfile :: Encode AacCodecProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Mono (Audio Description), Mono, Stereo, or 5.1 channel layout. Valid values depend on rate control mode and profile. "1.0 - Audio Description (Receiver Mix)" setting receives a stereo description plus control track and emits a mono AAC encode of the description track, with control data emitted in the PES header as per ETSI TS 101 154 Annex E.
newtype AacCodingMode = AacCodingMode String
derive instance newtypeAacCodingMode :: Newtype AacCodingMode _
derive instance repGenericAacCodingMode :: Generic AacCodingMode _
instance showAacCodingMode :: Show AacCodingMode where
  show = genericShow
instance decodeAacCodingMode :: Decode AacCodingMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacCodingMode :: Encode AacCodingMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Rate Control Mode.
newtype AacRateControlMode = AacRateControlMode String
derive instance newtypeAacRateControlMode :: Newtype AacRateControlMode _
derive instance repGenericAacRateControlMode :: Generic AacRateControlMode _
instance showAacRateControlMode :: Show AacRateControlMode where
  show = genericShow
instance decodeAacRateControlMode :: Decode AacRateControlMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacRateControlMode :: Encode AacRateControlMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
newtype AacRawFormat = AacRawFormat String
derive instance newtypeAacRawFormat :: Newtype AacRawFormat _
derive instance repGenericAacRawFormat :: Generic AacRawFormat _
instance showAacRawFormat :: Show AacRawFormat where
  show = genericShow
instance decodeAacRawFormat :: Decode AacRawFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacRawFormat :: Encode AacRawFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AAC.
newtype AacSettings = AacSettings 
  { "AudioDescriptionBroadcasterMix" :: NullOrUndefined.NullOrUndefined (AacAudioDescriptionBroadcasterMix)
  , "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "CodecProfile" :: NullOrUndefined.NullOrUndefined (AacCodecProfile)
  , "CodingMode" :: NullOrUndefined.NullOrUndefined (AacCodingMode)
  , "RateControlMode" :: NullOrUndefined.NullOrUndefined (AacRateControlMode)
  , "RawFormat" :: NullOrUndefined.NullOrUndefined (AacRawFormat)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (Int)
  , "Specification" :: NullOrUndefined.NullOrUndefined (AacSpecification)
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


-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
newtype AacSpecification = AacSpecification String
derive instance newtypeAacSpecification :: Newtype AacSpecification _
derive instance repGenericAacSpecification :: Generic AacSpecification _
instance showAacSpecification :: Show AacSpecification where
  show = genericShow
instance decodeAacSpecification :: Decode AacSpecification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacSpecification :: Encode AacSpecification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | VBR Quality Level - Only used if rate_control_mode is VBR.
newtype AacVbrQuality = AacVbrQuality String
derive instance newtypeAacVbrQuality :: Newtype AacVbrQuality _
derive instance repGenericAacVbrQuality :: Generic AacVbrQuality _
instance showAacVbrQuality :: Show AacVbrQuality where
  show = genericShow
instance decodeAacVbrQuality :: Decode AacVbrQuality where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAacVbrQuality :: Encode AacVbrQuality where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the "Bitstream Mode" (bsmod) for the emitted AC-3 stream. See ATSC A/52-2012 for background on these values.
newtype Ac3BitstreamMode = Ac3BitstreamMode String
derive instance newtypeAc3BitstreamMode :: Newtype Ac3BitstreamMode _
derive instance repGenericAc3BitstreamMode :: Generic Ac3BitstreamMode _
instance showAc3BitstreamMode :: Show Ac3BitstreamMode where
  show = genericShow
instance decodeAc3BitstreamMode :: Decode Ac3BitstreamMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3BitstreamMode :: Encode Ac3BitstreamMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Dolby Digital coding mode. Determines number of channels.
newtype Ac3CodingMode = Ac3CodingMode String
derive instance newtypeAc3CodingMode :: Newtype Ac3CodingMode _
derive instance repGenericAc3CodingMode :: Generic Ac3CodingMode _
instance showAc3CodingMode :: Show Ac3CodingMode where
  show = genericShow
instance decodeAc3CodingMode :: Decode Ac3CodingMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3CodingMode :: Encode Ac3CodingMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
newtype Ac3DynamicRangeCompressionProfile = Ac3DynamicRangeCompressionProfile String
derive instance newtypeAc3DynamicRangeCompressionProfile :: Newtype Ac3DynamicRangeCompressionProfile _
derive instance repGenericAc3DynamicRangeCompressionProfile :: Generic Ac3DynamicRangeCompressionProfile _
instance showAc3DynamicRangeCompressionProfile :: Show Ac3DynamicRangeCompressionProfile where
  show = genericShow
instance decodeAc3DynamicRangeCompressionProfile :: Decode Ac3DynamicRangeCompressionProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3DynamicRangeCompressionProfile :: Encode Ac3DynamicRangeCompressionProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
newtype Ac3LfeFilter = Ac3LfeFilter String
derive instance newtypeAc3LfeFilter :: Newtype Ac3LfeFilter _
derive instance repGenericAc3LfeFilter :: Generic Ac3LfeFilter _
instance showAc3LfeFilter :: Show Ac3LfeFilter where
  show = genericShow
instance decodeAc3LfeFilter :: Decode Ac3LfeFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3LfeFilter :: Encode Ac3LfeFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
newtype Ac3MetadataControl = Ac3MetadataControl String
derive instance newtypeAc3MetadataControl :: Newtype Ac3MetadataControl _
derive instance repGenericAc3MetadataControl :: Generic Ac3MetadataControl _
instance showAc3MetadataControl :: Show Ac3MetadataControl where
  show = genericShow
instance decodeAc3MetadataControl :: Decode Ac3MetadataControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3MetadataControl :: Encode Ac3MetadataControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AC3.
newtype Ac3Settings = Ac3Settings 
  { "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "BitstreamMode" :: NullOrUndefined.NullOrUndefined (Ac3BitstreamMode)
  , "CodingMode" :: NullOrUndefined.NullOrUndefined (Ac3CodingMode)
  , "Dialnorm" :: NullOrUndefined.NullOrUndefined (Int)
  , "DynamicRangeCompressionProfile" :: NullOrUndefined.NullOrUndefined (Ac3DynamicRangeCompressionProfile)
  , "LfeFilter" :: NullOrUndefined.NullOrUndefined (Ac3LfeFilter)
  , "MetadataControl" :: NullOrUndefined.NullOrUndefined (Ac3MetadataControl)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAc3Settings :: Newtype Ac3Settings _
derive instance repGenericAc3Settings :: Generic Ac3Settings _
instance showAc3Settings :: Show Ac3Settings where
  show = genericShow
instance decodeAc3Settings :: Decode Ac3Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAc3Settings :: Encode Ac3Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This setting only applies to H.264 and MPEG2 outputs. Use Insert AFD signaling (AfdSignaling) to whether there are AFD values in the output video data and what those values are. * Choose None to remove all AFD values from this output. * Choose Fixed to ignore input AFD values and instead encode the value specified in the job. * Choose Auto to calculate output AFD values based on the input AFD scaler data.
newtype AfdSignaling = AfdSignaling String
derive instance newtypeAfdSignaling :: Newtype AfdSignaling _
derive instance repGenericAfdSignaling :: Generic AfdSignaling _
instance showAfdSignaling :: Show AfdSignaling where
  show = genericShow
instance decodeAfdSignaling :: Decode AfdSignaling where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAfdSignaling :: Encode AfdSignaling where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value AIFF.
newtype AiffSettings = AiffSettings 
  { "BitDepth" :: NullOrUndefined.NullOrUndefined (Int)
  , "Channels" :: NullOrUndefined.NullOrUndefined (Int)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAiffSettings :: Newtype AiffSettings _
derive instance repGenericAiffSettings :: Generic AiffSettings _
instance showAiffSettings :: Show AiffSettings where
  show = genericShow
instance decodeAiffSettings :: Decode AiffSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAiffSettings :: Encode AiffSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for ancillary captions source.
newtype AncillarySourceSettings = AncillarySourceSettings 
  { "SourceAncillaryChannelNumber" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeAncillarySourceSettings :: Newtype AncillarySourceSettings _
derive instance repGenericAncillarySourceSettings :: Generic AncillarySourceSettings _
instance showAncillarySourceSettings :: Show AncillarySourceSettings where
  show = genericShow
instance decodeAncillarySourceSettings :: Decode AncillarySourceSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAncillarySourceSettings :: Encode AncillarySourceSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable Anti-alias (AntiAlias) to enhance sharp edges in video output when your input resolution is much larger than your output resolution. Default is enabled.
newtype AntiAlias = AntiAlias String
derive instance newtypeAntiAlias :: Newtype AntiAlias _
derive instance repGenericAntiAlias :: Generic AntiAlias _
instance showAntiAlias :: Show AntiAlias where
  show = genericShow
instance decodeAntiAlias :: Decode AntiAlias where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAntiAlias :: Encode AntiAlias where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Type of Audio codec.
newtype AudioCodec = AudioCodec String
derive instance newtypeAudioCodec :: Newtype AudioCodec _
derive instance repGenericAudioCodec :: Generic AudioCodec _
instance showAudioCodec :: Show AudioCodec where
  show = genericShow
instance decodeAudioCodec :: Decode AudioCodec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioCodec :: Encode AudioCodec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Audio codec settings (CodecSettings) under (AudioDescriptions) contains the group of settings related to audio encoding. The settings in this group vary depending on the value you choose for Audio codec (Codec). For each codec enum you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * AAC, AacSettings * MP2, Mp2Settings * WAV, WavSettings * AIFF, AiffSettings * AC3, Ac3Settings * EAC3, Eac3Settings
newtype AudioCodecSettings = AudioCodecSettings 
  { "AacSettings" :: NullOrUndefined.NullOrUndefined (AacSettings)
  , "Ac3Settings" :: NullOrUndefined.NullOrUndefined (Ac3Settings)
  , "AiffSettings" :: NullOrUndefined.NullOrUndefined (AiffSettings)
  , "Codec" :: NullOrUndefined.NullOrUndefined (AudioCodec)
  , "Eac3Settings" :: NullOrUndefined.NullOrUndefined (Eac3Settings)
  , "Mp2Settings" :: NullOrUndefined.NullOrUndefined (Mp2Settings)
  , "WavSettings" :: NullOrUndefined.NullOrUndefined (WavSettings)
  }
derive instance newtypeAudioCodecSettings :: Newtype AudioCodecSettings _
derive instance repGenericAudioCodecSettings :: Generic AudioCodecSettings _
instance showAudioCodecSettings :: Show AudioCodecSettings where
  show = genericShow
instance decodeAudioCodecSettings :: Decode AudioCodecSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioCodecSettings :: Encode AudioCodecSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When an "Audio Description":#audio_description specifies an AudioSelector or AudioSelectorGroup  for which no matching source is found in the input, then the audio selector marked as DEFAULT will be used.  If none are marked as default, silence will be inserted for the duration of the input.
newtype AudioDefaultSelection = AudioDefaultSelection String
derive instance newtypeAudioDefaultSelection :: Newtype AudioDefaultSelection _
derive instance repGenericAudioDefaultSelection :: Generic AudioDefaultSelection _
instance showAudioDefaultSelection :: Show AudioDefaultSelection where
  show = genericShow
instance decodeAudioDefaultSelection :: Decode AudioDefaultSelection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioDefaultSelection :: Encode AudioDefaultSelection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Description of audio output
newtype AudioDescription = AudioDescription 
  { "AudioNormalizationSettings" :: NullOrUndefined.NullOrUndefined (AudioNormalizationSettings)
  , "AudioSourceName" :: NullOrUndefined.NullOrUndefined (String)
  , "AudioType" :: NullOrUndefined.NullOrUndefined (Int)
  , "AudioTypeControl" :: NullOrUndefined.NullOrUndefined (AudioTypeControl)
  , "CodecSettings" :: NullOrUndefined.NullOrUndefined (AudioCodecSettings)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (LanguageCode)
  , "LanguageCodeControl" :: NullOrUndefined.NullOrUndefined (AudioLanguageCodeControl)
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


-- | Choosing FOLLOW_INPUT will cause the ISO 639 language code of the output to follow the ISO 639 language code of the input. The language specified for languageCode' will be used when USE_CONFIGURED is selected or when FOLLOW_INPUT is selected but there is no ISO 639 language code specified by the input.
newtype AudioLanguageCodeControl = AudioLanguageCodeControl String
derive instance newtypeAudioLanguageCodeControl :: Newtype AudioLanguageCodeControl _
derive instance repGenericAudioLanguageCodeControl :: Generic AudioLanguageCodeControl _
instance showAudioLanguageCodeControl :: Show AudioLanguageCodeControl where
  show = genericShow
instance decodeAudioLanguageCodeControl :: Decode AudioLanguageCodeControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioLanguageCodeControl :: Encode AudioLanguageCodeControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Audio normalization algorithm to use. 1770-1 conforms to the CALM Act specification, 1770-2 conforms to the EBU R-128 specification.
newtype AudioNormalizationAlgorithm = AudioNormalizationAlgorithm String
derive instance newtypeAudioNormalizationAlgorithm :: Newtype AudioNormalizationAlgorithm _
derive instance repGenericAudioNormalizationAlgorithm :: Generic AudioNormalizationAlgorithm _
instance showAudioNormalizationAlgorithm :: Show AudioNormalizationAlgorithm where
  show = genericShow
instance decodeAudioNormalizationAlgorithm :: Decode AudioNormalizationAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioNormalizationAlgorithm :: Encode AudioNormalizationAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
newtype AudioNormalizationAlgorithmControl = AudioNormalizationAlgorithmControl String
derive instance newtypeAudioNormalizationAlgorithmControl :: Newtype AudioNormalizationAlgorithmControl _
derive instance repGenericAudioNormalizationAlgorithmControl :: Generic AudioNormalizationAlgorithmControl _
instance showAudioNormalizationAlgorithmControl :: Show AudioNormalizationAlgorithmControl where
  show = genericShow
instance decodeAudioNormalizationAlgorithmControl :: Decode AudioNormalizationAlgorithmControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioNormalizationAlgorithmControl :: Encode AudioNormalizationAlgorithmControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If set to LOG, log each output's audio track loudness to a CSV file.
newtype AudioNormalizationLoudnessLogging = AudioNormalizationLoudnessLogging String
derive instance newtypeAudioNormalizationLoudnessLogging :: Newtype AudioNormalizationLoudnessLogging _
derive instance repGenericAudioNormalizationLoudnessLogging :: Generic AudioNormalizationLoudnessLogging _
instance showAudioNormalizationLoudnessLogging :: Show AudioNormalizationLoudnessLogging where
  show = genericShow
instance decodeAudioNormalizationLoudnessLogging :: Decode AudioNormalizationLoudnessLogging where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioNormalizationLoudnessLogging :: Encode AudioNormalizationLoudnessLogging where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
newtype AudioNormalizationPeakCalculation = AudioNormalizationPeakCalculation String
derive instance newtypeAudioNormalizationPeakCalculation :: Newtype AudioNormalizationPeakCalculation _
derive instance repGenericAudioNormalizationPeakCalculation :: Generic AudioNormalizationPeakCalculation _
instance showAudioNormalizationPeakCalculation :: Show AudioNormalizationPeakCalculation where
  show = genericShow
instance decodeAudioNormalizationPeakCalculation :: Decode AudioNormalizationPeakCalculation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioNormalizationPeakCalculation :: Encode AudioNormalizationPeakCalculation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Advanced audio normalization settings.
newtype AudioNormalizationSettings = AudioNormalizationSettings 
  { "Algorithm" :: NullOrUndefined.NullOrUndefined (AudioNormalizationAlgorithm)
  , "AlgorithmControl" :: NullOrUndefined.NullOrUndefined (AudioNormalizationAlgorithmControl)
  , "CorrectionGateLevel" :: NullOrUndefined.NullOrUndefined (Int)
  , "LoudnessLogging" :: NullOrUndefined.NullOrUndefined (AudioNormalizationLoudnessLogging)
  , "PeakCalculation" :: NullOrUndefined.NullOrUndefined (AudioNormalizationPeakCalculation)
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


-- | Selector for Audio
newtype AudioSelector = AudioSelector 
  { "DefaultSelection" :: NullOrUndefined.NullOrUndefined (AudioDefaultSelection)
  , "ExternalAudioFileInput" :: NullOrUndefined.NullOrUndefined (String)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (LanguageCode)
  , "Offset" :: NullOrUndefined.NullOrUndefined (Int)
  , "Pids" :: NullOrUndefined.NullOrUndefined (ListOf__integer)
  , "ProgramSelection" :: NullOrUndefined.NullOrUndefined (Int)
  , "RemixSettings" :: NullOrUndefined.NullOrUndefined (RemixSettings)
  , "SelectorType" :: NullOrUndefined.NullOrUndefined (AudioSelectorType)
  , "Tracks" :: NullOrUndefined.NullOrUndefined (ListOf__integer)
  }
derive instance newtypeAudioSelector :: Newtype AudioSelector _
derive instance repGenericAudioSelector :: Generic AudioSelector _
instance showAudioSelector :: Show AudioSelector where
  show = genericShow
instance decodeAudioSelector :: Decode AudioSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioSelector :: Encode AudioSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Group of Audio Selectors
newtype AudioSelectorGroup = AudioSelectorGroup 
  { "AudioSelectorNames" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  }
derive instance newtypeAudioSelectorGroup :: Newtype AudioSelectorGroup _
derive instance repGenericAudioSelectorGroup :: Generic AudioSelectorGroup _
instance showAudioSelectorGroup :: Show AudioSelectorGroup where
  show = genericShow
instance decodeAudioSelectorGroup :: Decode AudioSelectorGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioSelectorGroup :: Encode AudioSelectorGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the type of the audio selector.
newtype AudioSelectorType = AudioSelectorType String
derive instance newtypeAudioSelectorType :: Newtype AudioSelectorType _
derive instance repGenericAudioSelectorType :: Generic AudioSelectorType _
instance showAudioSelectorType :: Show AudioSelectorType where
  show = genericShow
instance decodeAudioSelectorType :: Decode AudioSelectorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioSelectorType :: Encode AudioSelectorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
newtype AudioTypeControl = AudioTypeControl String
derive instance newtypeAudioTypeControl :: Newtype AudioTypeControl _
derive instance repGenericAudioTypeControl :: Generic AudioTypeControl _
instance showAudioTypeControl :: Show AudioTypeControl where
  show = genericShow
instance decodeAudioTypeControl :: Decode AudioTypeControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioTypeControl :: Encode AudioTypeControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for Avail Blanking
newtype AvailBlanking = AvailBlanking 
  { "AvailBlankingImage" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAvailBlanking :: Newtype AvailBlanking _
derive instance repGenericAvailBlanking :: Generic AvailBlanking _
instance showAvailBlanking :: Show AvailBlanking where
  show = genericShow
instance decodeAvailBlanking :: Decode AvailBlanking where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailBlanking :: Encode AvailBlanking where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The service can't process your request because of a problem in the request. Please check your request form and syntax.
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


-- | Burn-In Destination Settings.
newtype BurninDestinationSettings = BurninDestinationSettings 
  { "Alignment" :: NullOrUndefined.NullOrUndefined (BurninSubtitleAlignment)
  , "BackgroundColor" :: NullOrUndefined.NullOrUndefined (BurninSubtitleBackgroundColor)
  , "BackgroundOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontColor" :: NullOrUndefined.NullOrUndefined (BurninSubtitleFontColor)
  , "FontOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontResolution" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "OutlineColor" :: NullOrUndefined.NullOrUndefined (BurninSubtitleOutlineColor)
  , "OutlineSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowColor" :: NullOrUndefined.NullOrUndefined (BurninSubtitleShadowColor)
  , "ShadowOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowXOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowYOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "TeletextSpacing" :: NullOrUndefined.NullOrUndefined (BurninSubtitleTeletextSpacing)
  , "XPosition" :: NullOrUndefined.NullOrUndefined (Int)
  , "YPosition" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeBurninDestinationSettings :: Newtype BurninDestinationSettings _
derive instance repGenericBurninDestinationSettings :: Generic BurninDestinationSettings _
instance showBurninDestinationSettings :: Show BurninDestinationSettings where
  show = genericShow
instance decodeBurninDestinationSettings :: Decode BurninDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurninDestinationSettings :: Encode BurninDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleAlignment = BurninSubtitleAlignment String
derive instance newtypeBurninSubtitleAlignment :: Newtype BurninSubtitleAlignment _
derive instance repGenericBurninSubtitleAlignment :: Generic BurninSubtitleAlignment _
instance showBurninSubtitleAlignment :: Show BurninSubtitleAlignment where
  show = genericShow
instance decodeBurninSubtitleAlignment :: Decode BurninSubtitleAlignment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurninSubtitleAlignment :: Encode BurninSubtitleAlignment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the color of the rectangle behind the captions.
-- | All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleBackgroundColor = BurninSubtitleBackgroundColor String
derive instance newtypeBurninSubtitleBackgroundColor :: Newtype BurninSubtitleBackgroundColor _
derive instance repGenericBurninSubtitleBackgroundColor :: Generic BurninSubtitleBackgroundColor _
instance showBurninSubtitleBackgroundColor :: Show BurninSubtitleBackgroundColor where
  show = genericShow
instance decodeBurninSubtitleBackgroundColor :: Decode BurninSubtitleBackgroundColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurninSubtitleBackgroundColor :: Encode BurninSubtitleBackgroundColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleFontColor = BurninSubtitleFontColor String
derive instance newtypeBurninSubtitleFontColor :: Newtype BurninSubtitleFontColor _
derive instance repGenericBurninSubtitleFontColor :: Generic BurninSubtitleFontColor _
instance showBurninSubtitleFontColor :: Show BurninSubtitleFontColor where
  show = genericShow
instance decodeBurninSubtitleFontColor :: Decode BurninSubtitleFontColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurninSubtitleFontColor :: Encode BurninSubtitleFontColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleOutlineColor = BurninSubtitleOutlineColor String
derive instance newtypeBurninSubtitleOutlineColor :: Newtype BurninSubtitleOutlineColor _
derive instance repGenericBurninSubtitleOutlineColor :: Generic BurninSubtitleOutlineColor _
instance showBurninSubtitleOutlineColor :: Show BurninSubtitleOutlineColor where
  show = genericShow
instance decodeBurninSubtitleOutlineColor :: Decode BurninSubtitleOutlineColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurninSubtitleOutlineColor :: Encode BurninSubtitleOutlineColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the color of the shadow cast by the captions.
-- | All burn-in and DVB-Sub font settings must match.
newtype BurninSubtitleShadowColor = BurninSubtitleShadowColor String
derive instance newtypeBurninSubtitleShadowColor :: Newtype BurninSubtitleShadowColor _
derive instance repGenericBurninSubtitleShadowColor :: Generic BurninSubtitleShadowColor _
instance showBurninSubtitleShadowColor :: Show BurninSubtitleShadowColor where
  show = genericShow
instance decodeBurninSubtitleShadowColor :: Decode BurninSubtitleShadowColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurninSubtitleShadowColor :: Encode BurninSubtitleShadowColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Controls whether a fixed grid size or proportional font spacing will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
newtype BurninSubtitleTeletextSpacing = BurninSubtitleTeletextSpacing String
derive instance newtypeBurninSubtitleTeletextSpacing :: Newtype BurninSubtitleTeletextSpacing _
derive instance repGenericBurninSubtitleTeletextSpacing :: Generic BurninSubtitleTeletextSpacing _
instance showBurninSubtitleTeletextSpacing :: Show BurninSubtitleTeletextSpacing where
  show = genericShow
instance decodeBurninSubtitleTeletextSpacing :: Decode BurninSubtitleTeletextSpacing where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBurninSubtitleTeletextSpacing :: Encode BurninSubtitleTeletextSpacing where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelJobRequest = CancelJobRequest 
  { "Id" :: (String)
  }
derive instance newtypeCancelJobRequest :: Newtype CancelJobRequest _
derive instance repGenericCancelJobRequest :: Generic CancelJobRequest _
instance showCancelJobRequest :: Show CancelJobRequest where
  show = genericShow
instance decodeCancelJobRequest :: Decode CancelJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobRequest :: Encode CancelJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelJobResponse = CancelJobResponse Types.NoArguments
derive instance newtypeCancelJobResponse :: Newtype CancelJobResponse _
derive instance repGenericCancelJobResponse :: Generic CancelJobResponse _
instance showCancelJobResponse :: Show CancelJobResponse where
  show = genericShow
instance decodeCancelJobResponse :: Decode CancelJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobResponse :: Encode CancelJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Description of Caption output
newtype CaptionDescription = CaptionDescription 
  { "CaptionSelectorName" :: NullOrUndefined.NullOrUndefined (String)
  , "DestinationSettings" :: NullOrUndefined.NullOrUndefined (CaptionDestinationSettings)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (LanguageCode)
  , "LanguageDescription" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCaptionDescription :: Newtype CaptionDescription _
derive instance repGenericCaptionDescription :: Generic CaptionDescription _
instance showCaptionDescription :: Show CaptionDescription where
  show = genericShow
instance decodeCaptionDescription :: Decode CaptionDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionDescription :: Encode CaptionDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Caption Description for preset
newtype CaptionDescriptionPreset = CaptionDescriptionPreset 
  { "DestinationSettings" :: NullOrUndefined.NullOrUndefined (CaptionDestinationSettings)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (LanguageCode)
  , "LanguageDescription" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCaptionDescriptionPreset :: Newtype CaptionDescriptionPreset _
derive instance repGenericCaptionDescriptionPreset :: Generic CaptionDescriptionPreset _
instance showCaptionDescriptionPreset :: Show CaptionDescriptionPreset where
  show = genericShow
instance decodeCaptionDescriptionPreset :: Decode CaptionDescriptionPreset where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionDescriptionPreset :: Encode CaptionDescriptionPreset where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specific settings required by destination type. Note that burnin_destination_settings are not available if the source of the caption data is Embedded or Teletext.
newtype CaptionDestinationSettings = CaptionDestinationSettings 
  { "BurninDestinationSettings" :: NullOrUndefined.NullOrUndefined (BurninDestinationSettings)
  , "DestinationType" :: NullOrUndefined.NullOrUndefined (CaptionDestinationType)
  , "DvbSubDestinationSettings" :: NullOrUndefined.NullOrUndefined (DvbSubDestinationSettings)
  , "SccDestinationSettings" :: NullOrUndefined.NullOrUndefined (SccDestinationSettings)
  , "TeletextDestinationSettings" :: NullOrUndefined.NullOrUndefined (TeletextDestinationSettings)
  , "TtmlDestinationSettings" :: NullOrUndefined.NullOrUndefined (TtmlDestinationSettings)
  }
derive instance newtypeCaptionDestinationSettings :: Newtype CaptionDestinationSettings _
derive instance repGenericCaptionDestinationSettings :: Generic CaptionDestinationSettings _
instance showCaptionDestinationSettings :: Show CaptionDestinationSettings where
  show = genericShow
instance decodeCaptionDestinationSettings :: Decode CaptionDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionDestinationSettings :: Encode CaptionDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Type of Caption output, including Burn-In, Embedded, SCC, SRT, TTML, WebVTT, DVB-Sub, Teletext.
newtype CaptionDestinationType = CaptionDestinationType String
derive instance newtypeCaptionDestinationType :: Newtype CaptionDestinationType _
derive instance repGenericCaptionDestinationType :: Generic CaptionDestinationType _
instance showCaptionDestinationType :: Show CaptionDestinationType where
  show = genericShow
instance decodeCaptionDestinationType :: Decode CaptionDestinationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionDestinationType :: Encode CaptionDestinationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Caption inputs to be mapped to caption outputs.
newtype CaptionSelector = CaptionSelector 
  { "LanguageCode" :: NullOrUndefined.NullOrUndefined (LanguageCode)
  , "SourceSettings" :: NullOrUndefined.NullOrUndefined (CaptionSourceSettings)
  }
derive instance newtypeCaptionSelector :: Newtype CaptionSelector _
derive instance repGenericCaptionSelector :: Generic CaptionSelector _
instance showCaptionSelector :: Show CaptionSelector where
  show = genericShow
instance decodeCaptionSelector :: Decode CaptionSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionSelector :: Encode CaptionSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Source settings (SourceSettings) contains the group of settings for captions in the input.
newtype CaptionSourceSettings = CaptionSourceSettings 
  { "AncillarySourceSettings" :: NullOrUndefined.NullOrUndefined (AncillarySourceSettings)
  , "DvbSubSourceSettings" :: NullOrUndefined.NullOrUndefined (DvbSubSourceSettings)
  , "EmbeddedSourceSettings" :: NullOrUndefined.NullOrUndefined (EmbeddedSourceSettings)
  , "FileSourceSettings" :: NullOrUndefined.NullOrUndefined (FileSourceSettings)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (CaptionSourceType)
  , "TeletextSourceSettings" :: NullOrUndefined.NullOrUndefined (TeletextSourceSettings)
  }
derive instance newtypeCaptionSourceSettings :: Newtype CaptionSourceSettings _
derive instance repGenericCaptionSourceSettings :: Generic CaptionSourceSettings _
instance showCaptionSourceSettings :: Show CaptionSourceSettings where
  show = genericShow
instance decodeCaptionSourceSettings :: Decode CaptionSourceSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionSourceSettings :: Encode CaptionSourceSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Source (SourceType) to identify the format of your input captions.  The service cannot auto-detect caption format.
newtype CaptionSourceType = CaptionSourceType String
derive instance newtypeCaptionSourceType :: Newtype CaptionSourceType _
derive instance repGenericCaptionSourceType :: Generic CaptionSourceType _
instance showCaptionSourceType :: Show CaptionSourceType where
  show = genericShow
instance decodeCaptionSourceType :: Decode CaptionSourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionSourceType :: Encode CaptionSourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Channel mapping (ChannelMapping) contains the group of fields that hold the remixing value for each channel. Units are in dB. Acceptable values are within the range from -60 (mute) through 6. A setting of 0 passes the input channel unchanged to the output channel (no attenuation or amplification).
newtype ChannelMapping = ChannelMapping 
  { "OutputChannels" :: NullOrUndefined.NullOrUndefined (ListOfOutputChannelMapping)
  }
derive instance newtypeChannelMapping :: Newtype ChannelMapping _
derive instance repGenericChannelMapping :: Generic ChannelMapping _
instance showChannelMapping :: Show ChannelMapping where
  show = genericShow
instance decodeChannelMapping :: Decode ChannelMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelMapping :: Encode ChannelMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for color correction.
newtype ColorCorrector = ColorCorrector 
  { "Brightness" :: NullOrUndefined.NullOrUndefined (Int)
  , "ColorSpaceConversion" :: NullOrUndefined.NullOrUndefined (ColorSpaceConversion)
  , "Contrast" :: NullOrUndefined.NullOrUndefined (Int)
  , "Hdr10Metadata" :: NullOrUndefined.NullOrUndefined (Hdr10Metadata)
  , "Hue" :: NullOrUndefined.NullOrUndefined (Int)
  , "Saturation" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeColorCorrector :: Newtype ColorCorrector _
derive instance repGenericColorCorrector :: Generic ColorCorrector _
instance showColorCorrector :: Show ColorCorrector where
  show = genericShow
instance decodeColorCorrector :: Decode ColorCorrector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColorCorrector :: Encode ColorCorrector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable Insert color metadata (ColorMetadata) to include color metadata in this output. This setting is enabled by default.
newtype ColorMetadata = ColorMetadata String
derive instance newtypeColorMetadata :: Newtype ColorMetadata _
derive instance repGenericColorMetadata :: Generic ColorMetadata _
instance showColorMetadata :: Show ColorMetadata where
  show = genericShow
instance decodeColorMetadata :: Decode ColorMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColorMetadata :: Encode ColorMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the colorspace of an input. This setting works in tandem with "Color Corrector":#color_corrector > color_space_conversion to determine if any conversion will be performed.
newtype ColorSpace = ColorSpace String
derive instance newtypeColorSpace :: Newtype ColorSpace _
derive instance repGenericColorSpace :: Generic ColorSpace _
instance showColorSpace :: Show ColorSpace where
  show = genericShow
instance decodeColorSpace :: Decode ColorSpace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColorSpace :: Encode ColorSpace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Determines if colorspace conversion will be performed. If set to _None_, no conversion will be performed. If _Force 601_ or _Force 709_ are selected, conversion will be performed for inputs with differing colorspaces. An input's colorspace can be specified explicitly in the "Video Selector":#inputs-video_selector if necessary.
newtype ColorSpaceConversion = ColorSpaceConversion String
derive instance newtypeColorSpaceConversion :: Newtype ColorSpaceConversion _
derive instance repGenericColorSpaceConversion :: Generic ColorSpaceConversion _
instance showColorSpaceConversion :: Show ColorSpaceConversion where
  show = genericShow
instance decodeColorSpaceConversion :: Decode ColorSpaceConversion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColorSpaceConversion :: Encode ColorSpaceConversion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | There are two sources for color metadata, the input file and the job configuration. This enum controls which takes precedence. FORCE: System will use color metadata supplied by user, if any. If the user does not supply color metadata the system will use data from the source. FALLBACK: System will use color metadata from the source. If source has no color metadata, the system will use user-supplied color metadata values if available.
newtype ColorSpaceUsage = ColorSpaceUsage String
derive instance newtypeColorSpaceUsage :: Newtype ColorSpaceUsage _
derive instance repGenericColorSpaceUsage :: Generic ColorSpaceUsage _
instance showColorSpaceUsage :: Show ColorSpaceUsage where
  show = genericShow
instance decodeColorSpaceUsage :: Decode ColorSpaceUsage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColorSpaceUsage :: Encode ColorSpaceUsage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The service could not complete your request because there is a conflict with the current state of the resource.
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


-- | Container specific settings.
newtype ContainerSettings = ContainerSettings 
  { "Container" :: NullOrUndefined.NullOrUndefined (ContainerType)
  , "F4vSettings" :: NullOrUndefined.NullOrUndefined (F4vSettings)
  , "M2tsSettings" :: NullOrUndefined.NullOrUndefined (M2tsSettings)
  , "M3u8Settings" :: NullOrUndefined.NullOrUndefined (M3u8Settings)
  , "MovSettings" :: NullOrUndefined.NullOrUndefined (MovSettings)
  , "Mp4Settings" :: NullOrUndefined.NullOrUndefined (Mp4Settings)
  }
derive instance newtypeContainerSettings :: Newtype ContainerSettings _
derive instance repGenericContainerSettings :: Generic ContainerSettings _
instance showContainerSettings :: Show ContainerSettings where
  show = genericShow
instance decodeContainerSettings :: Decode ContainerSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerSettings :: Encode ContainerSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Container for this output. Some containers require a container settings object. If not specified, the default object will be created.
newtype ContainerType = ContainerType String
derive instance newtypeContainerType :: Newtype ContainerType _
derive instance repGenericContainerType :: Generic ContainerType _
instance showContainerType :: Show ContainerType where
  show = genericShow
instance decodeContainerType :: Decode ContainerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerType :: Encode ContainerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobRequest = CreateJobRequest 
  { "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (String)
  , "JobTemplate" :: NullOrUndefined.NullOrUndefined (String)
  , "Queue" :: NullOrUndefined.NullOrUndefined (String)
  , "Role" :: NullOrUndefined.NullOrUndefined (String)
  , "Settings" :: NullOrUndefined.NullOrUndefined (JobSettings)
  , "UserMetadata" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  }
derive instance newtypeCreateJobRequest :: Newtype CreateJobRequest _
derive instance repGenericCreateJobRequest :: Generic CreateJobRequest _
instance showCreateJobRequest :: Show CreateJobRequest where
  show = genericShow
instance decodeCreateJobRequest :: Decode CreateJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobRequest :: Encode CreateJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobResponse = CreateJobResponse 
  { "Job" :: NullOrUndefined.NullOrUndefined (Job)
  }
derive instance newtypeCreateJobResponse :: Newtype CreateJobResponse _
derive instance repGenericCreateJobResponse :: Generic CreateJobResponse _
instance showCreateJobResponse :: Show CreateJobResponse where
  show = genericShow
instance decodeCreateJobResponse :: Decode CreateJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobResponse :: Encode CreateJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobTemplateRequest = CreateJobTemplateRequest 
  { "Category" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Queue" :: NullOrUndefined.NullOrUndefined (String)
  , "Settings" :: NullOrUndefined.NullOrUndefined (JobTemplateSettings)
  }
derive instance newtypeCreateJobTemplateRequest :: Newtype CreateJobTemplateRequest _
derive instance repGenericCreateJobTemplateRequest :: Generic CreateJobTemplateRequest _
instance showCreateJobTemplateRequest :: Show CreateJobTemplateRequest where
  show = genericShow
instance decodeCreateJobTemplateRequest :: Decode CreateJobTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobTemplateRequest :: Encode CreateJobTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobTemplateResponse = CreateJobTemplateResponse 
  { "JobTemplate" :: NullOrUndefined.NullOrUndefined (JobTemplate)
  }
derive instance newtypeCreateJobTemplateResponse :: Newtype CreateJobTemplateResponse _
derive instance repGenericCreateJobTemplateResponse :: Generic CreateJobTemplateResponse _
instance showCreateJobTemplateResponse :: Show CreateJobTemplateResponse where
  show = genericShow
instance decodeCreateJobTemplateResponse :: Decode CreateJobTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobTemplateResponse :: Encode CreateJobTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatePresetRequest = CreatePresetRequest 
  { "Category" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Settings" :: NullOrUndefined.NullOrUndefined (PresetSettings)
  }
derive instance newtypeCreatePresetRequest :: Newtype CreatePresetRequest _
derive instance repGenericCreatePresetRequest :: Generic CreatePresetRequest _
instance showCreatePresetRequest :: Show CreatePresetRequest where
  show = genericShow
instance decodeCreatePresetRequest :: Decode CreatePresetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePresetRequest :: Encode CreatePresetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatePresetResponse = CreatePresetResponse 
  { "Preset" :: NullOrUndefined.NullOrUndefined (Preset)
  }
derive instance newtypeCreatePresetResponse :: Newtype CreatePresetResponse _
derive instance repGenericCreatePresetResponse :: Generic CreatePresetResponse _
instance showCreatePresetResponse :: Show CreatePresetResponse where
  show = genericShow
instance decodeCreatePresetResponse :: Decode CreatePresetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePresetResponse :: Encode CreatePresetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateQueueRequest = CreateQueueRequest 
  { "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateQueueRequest :: Newtype CreateQueueRequest _
derive instance repGenericCreateQueueRequest :: Generic CreateQueueRequest _
instance showCreateQueueRequest :: Show CreateQueueRequest where
  show = genericShow
instance decodeCreateQueueRequest :: Decode CreateQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateQueueRequest :: Encode CreateQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateQueueResponse = CreateQueueResponse 
  { "Queue" :: NullOrUndefined.NullOrUndefined (Queue)
  }
derive instance newtypeCreateQueueResponse :: Newtype CreateQueueResponse _
derive instance repGenericCreateQueueResponse :: Generic CreateQueueResponse _
instance showCreateQueueResponse :: Show CreateQueueResponse where
  show = genericShow
instance decodeCreateQueueResponse :: Decode CreateQueueResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateQueueResponse :: Encode CreateQueueResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies DRM settings for DASH outputs.
newtype DashIsoEncryptionSettings = DashIsoEncryptionSettings 
  { "SpekeKeyProvider" :: NullOrUndefined.NullOrUndefined (SpekeKeyProvider)
  }
derive instance newtypeDashIsoEncryptionSettings :: Newtype DashIsoEncryptionSettings _
derive instance repGenericDashIsoEncryptionSettings :: Generic DashIsoEncryptionSettings _
instance showDashIsoEncryptionSettings :: Show DashIsoEncryptionSettings where
  show = genericShow
instance decodeDashIsoEncryptionSettings :: Decode DashIsoEncryptionSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashIsoEncryptionSettings :: Encode DashIsoEncryptionSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
newtype DashIsoGroupSettings = DashIsoGroupSettings 
  { "BaseUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "Destination" :: NullOrUndefined.NullOrUndefined (String)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (DashIsoEncryptionSettings)
  , "FragmentLength" :: NullOrUndefined.NullOrUndefined (Int)
  , "HbbtvCompliance" :: NullOrUndefined.NullOrUndefined (DashIsoHbbtvCompliance)
  , "MinBufferTime" :: NullOrUndefined.NullOrUndefined (Int)
  , "SegmentControl" :: NullOrUndefined.NullOrUndefined (DashIsoSegmentControl)
  , "SegmentLength" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDashIsoGroupSettings :: Newtype DashIsoGroupSettings _
derive instance repGenericDashIsoGroupSettings :: Generic DashIsoGroupSettings _
instance showDashIsoGroupSettings :: Show DashIsoGroupSettings where
  show = genericShow
instance decodeDashIsoGroupSettings :: Decode DashIsoGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashIsoGroupSettings :: Encode DashIsoGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Supports HbbTV specification as indicated
newtype DashIsoHbbtvCompliance = DashIsoHbbtvCompliance String
derive instance newtypeDashIsoHbbtvCompliance :: Newtype DashIsoHbbtvCompliance _
derive instance repGenericDashIsoHbbtvCompliance :: Generic DashIsoHbbtvCompliance _
instance showDashIsoHbbtvCompliance :: Show DashIsoHbbtvCompliance where
  show = genericShow
instance decodeDashIsoHbbtvCompliance :: Decode DashIsoHbbtvCompliance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashIsoHbbtvCompliance :: Encode DashIsoHbbtvCompliance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
newtype DashIsoSegmentControl = DashIsoSegmentControl String
derive instance newtypeDashIsoSegmentControl :: Newtype DashIsoSegmentControl _
derive instance repGenericDashIsoSegmentControl :: Generic DashIsoSegmentControl _
instance showDashIsoSegmentControl :: Show DashIsoSegmentControl where
  show = genericShow
instance decodeDashIsoSegmentControl :: Decode DashIsoSegmentControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashIsoSegmentControl :: Encode DashIsoSegmentControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your source file includes a ticker, such as a scrolling headline at the bottom of the frame.
newtype DeinterlaceAlgorithm = DeinterlaceAlgorithm String
derive instance newtypeDeinterlaceAlgorithm :: Newtype DeinterlaceAlgorithm _
derive instance repGenericDeinterlaceAlgorithm :: Generic DeinterlaceAlgorithm _
instance showDeinterlaceAlgorithm :: Show DeinterlaceAlgorithm where
  show = genericShow
instance decodeDeinterlaceAlgorithm :: Decode DeinterlaceAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeinterlaceAlgorithm :: Encode DeinterlaceAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for deinterlacer
newtype Deinterlacer = Deinterlacer 
  { "Algorithm" :: NullOrUndefined.NullOrUndefined (DeinterlaceAlgorithm)
  , "Control" :: NullOrUndefined.NullOrUndefined (DeinterlacerControl)
  , "Mode" :: NullOrUndefined.NullOrUndefined (DeinterlacerMode)
  }
derive instance newtypeDeinterlacer :: Newtype Deinterlacer _
derive instance repGenericDeinterlacer :: Generic Deinterlacer _
instance showDeinterlacer :: Show Deinterlacer where
  show = genericShow
instance decodeDeinterlacer :: Decode Deinterlacer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeinterlacer :: Encode Deinterlacer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | - When set to NORMAL (default), the deinterlacer does not convert frames that are tagged  in metadata as progressive. It will only convert those that are tagged as some other type. - When set to FORCE_ALL_FRAMES, the deinterlacer converts every frame to progressive - even those that are already tagged as progressive. Turn Force mode on only if there is  a good chance that the metadata has tagged frames as progressive when they are not  progressive. Do not turn on otherwise; processing frames that are already progressive  into progressive will probably result in lower quality video.
newtype DeinterlacerControl = DeinterlacerControl String
derive instance newtypeDeinterlacerControl :: Newtype DeinterlacerControl _
derive instance repGenericDeinterlacerControl :: Generic DeinterlacerControl _
instance showDeinterlacerControl :: Show DeinterlacerControl where
  show = genericShow
instance decodeDeinterlacerControl :: Decode DeinterlacerControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeinterlacerControl :: Encode DeinterlacerControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Deinterlacer (DeinterlaceMode) to choose how the service will do deinterlacing. Default is Deinterlace. - Deinterlace converts interlaced to progressive. - Inverse telecine converts Hard Telecine 29.97i to progressive 23.976p. - Adaptive auto-detects and converts to progressive.
newtype DeinterlacerMode = DeinterlacerMode String
derive instance newtypeDeinterlacerMode :: Newtype DeinterlacerMode _
derive instance repGenericDeinterlacerMode :: Generic DeinterlacerMode _
instance showDeinterlacerMode :: Show DeinterlacerMode where
  show = genericShow
instance decodeDeinterlacerMode :: Decode DeinterlacerMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeinterlacerMode :: Encode DeinterlacerMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteJobTemplateRequest = DeleteJobTemplateRequest 
  { "Name" :: (String)
  }
derive instance newtypeDeleteJobTemplateRequest :: Newtype DeleteJobTemplateRequest _
derive instance repGenericDeleteJobTemplateRequest :: Generic DeleteJobTemplateRequest _
instance showDeleteJobTemplateRequest :: Show DeleteJobTemplateRequest where
  show = genericShow
instance decodeDeleteJobTemplateRequest :: Decode DeleteJobTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteJobTemplateRequest :: Encode DeleteJobTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteJobTemplateResponse = DeleteJobTemplateResponse Types.NoArguments
derive instance newtypeDeleteJobTemplateResponse :: Newtype DeleteJobTemplateResponse _
derive instance repGenericDeleteJobTemplateResponse :: Generic DeleteJobTemplateResponse _
instance showDeleteJobTemplateResponse :: Show DeleteJobTemplateResponse where
  show = genericShow
instance decodeDeleteJobTemplateResponse :: Decode DeleteJobTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteJobTemplateResponse :: Encode DeleteJobTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePresetRequest = DeletePresetRequest 
  { "Name" :: (String)
  }
derive instance newtypeDeletePresetRequest :: Newtype DeletePresetRequest _
derive instance repGenericDeletePresetRequest :: Generic DeletePresetRequest _
instance showDeletePresetRequest :: Show DeletePresetRequest where
  show = genericShow
instance decodeDeletePresetRequest :: Decode DeletePresetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePresetRequest :: Encode DeletePresetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePresetResponse = DeletePresetResponse Types.NoArguments
derive instance newtypeDeletePresetResponse :: Newtype DeletePresetResponse _
derive instance repGenericDeletePresetResponse :: Generic DeletePresetResponse _
instance showDeletePresetResponse :: Show DeletePresetResponse where
  show = genericShow
instance decodeDeletePresetResponse :: Decode DeletePresetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePresetResponse :: Encode DeletePresetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteQueueRequest = DeleteQueueRequest 
  { "Name" :: (String)
  }
derive instance newtypeDeleteQueueRequest :: Newtype DeleteQueueRequest _
derive instance repGenericDeleteQueueRequest :: Generic DeleteQueueRequest _
instance showDeleteQueueRequest :: Show DeleteQueueRequest where
  show = genericShow
instance decodeDeleteQueueRequest :: Decode DeleteQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteQueueRequest :: Encode DeleteQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteQueueResponse = DeleteQueueResponse Types.NoArguments
derive instance newtypeDeleteQueueResponse :: Newtype DeleteQueueResponse _
derive instance repGenericDeleteQueueResponse :: Generic DeleteQueueResponse _
instance showDeleteQueueResponse :: Show DeleteQueueResponse where
  show = genericShow
instance decodeDeleteQueueResponse :: Decode DeleteQueueResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteQueueResponse :: Encode DeleteQueueResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | DescribeEndpointsRequest
newtype DescribeEndpointsRequest = DescribeEndpointsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEndpointsRequest :: Newtype DescribeEndpointsRequest _
derive instance repGenericDescribeEndpointsRequest :: Generic DescribeEndpointsRequest _
instance showDescribeEndpointsRequest :: Show DescribeEndpointsRequest where
  show = genericShow
instance decodeDescribeEndpointsRequest :: Decode DescribeEndpointsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEndpointsRequest :: Encode DescribeEndpointsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEndpointsResponse = DescribeEndpointsResponse 
  { "Endpoints" :: NullOrUndefined.NullOrUndefined (ListOfEndpoint)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEndpointsResponse :: Newtype DescribeEndpointsResponse _
derive instance repGenericDescribeEndpointsResponse :: Generic DescribeEndpointsResponse _
instance showDescribeEndpointsResponse :: Show DescribeEndpointsResponse where
  show = genericShow
instance decodeDescribeEndpointsResponse :: Decode DescribeEndpointsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEndpointsResponse :: Encode DescribeEndpointsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Applies only to 29.97 fps outputs. When this feature is enabled, the service will use drop-frame timecode on outputs. If it is not possible to use drop-frame timecode, the system will fall back to non-drop-frame. This setting is enabled by default when Timecode insertion (TimecodeInsertion) is enabled.
newtype DropFrameTimecode = DropFrameTimecode String
derive instance newtypeDropFrameTimecode :: Newtype DropFrameTimecode _
derive instance repGenericDropFrameTimecode :: Generic DropFrameTimecode _
instance showDropFrameTimecode :: Show DropFrameTimecode where
  show = genericShow
instance decodeDropFrameTimecode :: Decode DropFrameTimecode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDropFrameTimecode :: Encode DropFrameTimecode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Inserts DVB Network Information Table (NIT) at the specified table repetition interval.
newtype DvbNitSettings = DvbNitSettings 
  { "NetworkId" :: NullOrUndefined.NullOrUndefined (Int)
  , "NetworkName" :: NullOrUndefined.NullOrUndefined (String)
  , "NitInterval" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDvbNitSettings :: Newtype DvbNitSettings _
derive instance repGenericDvbNitSettings :: Generic DvbNitSettings _
instance showDvbNitSettings :: Show DvbNitSettings where
  show = genericShow
instance decodeDvbNitSettings :: Decode DvbNitSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbNitSettings :: Encode DvbNitSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Inserts DVB Service Description Table (NIT) at the specified table repetition interval.
newtype DvbSdtSettings = DvbSdtSettings 
  { "OutputSdt" :: NullOrUndefined.NullOrUndefined (OutputSdt)
  , "SdtInterval" :: NullOrUndefined.NullOrUndefined (Int)
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


-- | DVB-Sub Destination Settings
newtype DvbSubDestinationSettings = DvbSubDestinationSettings 
  { "Alignment" :: NullOrUndefined.NullOrUndefined (DvbSubtitleAlignment)
  , "BackgroundColor" :: NullOrUndefined.NullOrUndefined (DvbSubtitleBackgroundColor)
  , "BackgroundOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontColor" :: NullOrUndefined.NullOrUndefined (DvbSubtitleFontColor)
  , "FontOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontResolution" :: NullOrUndefined.NullOrUndefined (Int)
  , "FontSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "OutlineColor" :: NullOrUndefined.NullOrUndefined (DvbSubtitleOutlineColor)
  , "OutlineSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowColor" :: NullOrUndefined.NullOrUndefined (DvbSubtitleShadowColor)
  , "ShadowOpacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowXOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "ShadowYOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "TeletextSpacing" :: NullOrUndefined.NullOrUndefined (DvbSubtitleTeletextSpacing)
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


-- | DVB Sub Source Settings
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


-- | If no explicit x_position or y_position is provided, setting alignment to centered will place the captions at the bottom center of the output. Similarly, setting a left alignment will align captions to the bottom left of the output. If x and y positions are given in conjunction with the alignment parameter, the font will be justified (either left or centered) relative to those coordinates. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleAlignment = DvbSubtitleAlignment String
derive instance newtypeDvbSubtitleAlignment :: Newtype DvbSubtitleAlignment _
derive instance repGenericDvbSubtitleAlignment :: Generic DvbSubtitleAlignment _
instance showDvbSubtitleAlignment :: Show DvbSubtitleAlignment where
  show = genericShow
instance decodeDvbSubtitleAlignment :: Decode DvbSubtitleAlignment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubtitleAlignment :: Encode DvbSubtitleAlignment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the color of the rectangle behind the captions.
-- | All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleBackgroundColor = DvbSubtitleBackgroundColor String
derive instance newtypeDvbSubtitleBackgroundColor :: Newtype DvbSubtitleBackgroundColor _
derive instance repGenericDvbSubtitleBackgroundColor :: Generic DvbSubtitleBackgroundColor _
instance showDvbSubtitleBackgroundColor :: Show DvbSubtitleBackgroundColor where
  show = genericShow
instance decodeDvbSubtitleBackgroundColor :: Decode DvbSubtitleBackgroundColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubtitleBackgroundColor :: Encode DvbSubtitleBackgroundColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the color of the burned-in captions. This option is not valid for source captions that are STL, 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleFontColor = DvbSubtitleFontColor String
derive instance newtypeDvbSubtitleFontColor :: Newtype DvbSubtitleFontColor _
derive instance repGenericDvbSubtitleFontColor :: Generic DvbSubtitleFontColor _
instance showDvbSubtitleFontColor :: Show DvbSubtitleFontColor where
  show = genericShow
instance decodeDvbSubtitleFontColor :: Decode DvbSubtitleFontColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubtitleFontColor :: Encode DvbSubtitleFontColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies font outline color. This option is not valid for source captions that are either 608/embedded or teletext. These source settings are already pre-defined by the caption stream. All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleOutlineColor = DvbSubtitleOutlineColor String
derive instance newtypeDvbSubtitleOutlineColor :: Newtype DvbSubtitleOutlineColor _
derive instance repGenericDvbSubtitleOutlineColor :: Generic DvbSubtitleOutlineColor _
instance showDvbSubtitleOutlineColor :: Show DvbSubtitleOutlineColor where
  show = genericShow
instance decodeDvbSubtitleOutlineColor :: Decode DvbSubtitleOutlineColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubtitleOutlineColor :: Encode DvbSubtitleOutlineColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the color of the shadow cast by the captions.
-- | All burn-in and DVB-Sub font settings must match.
newtype DvbSubtitleShadowColor = DvbSubtitleShadowColor String
derive instance newtypeDvbSubtitleShadowColor :: Newtype DvbSubtitleShadowColor _
derive instance repGenericDvbSubtitleShadowColor :: Generic DvbSubtitleShadowColor _
instance showDvbSubtitleShadowColor :: Show DvbSubtitleShadowColor where
  show = genericShow
instance decodeDvbSubtitleShadowColor :: Decode DvbSubtitleShadowColor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubtitleShadowColor :: Encode DvbSubtitleShadowColor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Controls whether a fixed grid size or proportional font spacing will be used to generate the output subtitles bitmap. Only applicable for Teletext inputs and DVB-Sub/Burn-in outputs.
newtype DvbSubtitleTeletextSpacing = DvbSubtitleTeletextSpacing String
derive instance newtypeDvbSubtitleTeletextSpacing :: Newtype DvbSubtitleTeletextSpacing _
derive instance repGenericDvbSubtitleTeletextSpacing :: Generic DvbSubtitleTeletextSpacing _
instance showDvbSubtitleTeletextSpacing :: Show DvbSubtitleTeletextSpacing where
  show = genericShow
instance decodeDvbSubtitleTeletextSpacing :: Decode DvbSubtitleTeletextSpacing where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbSubtitleTeletextSpacing :: Encode DvbSubtitleTeletextSpacing where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Inserts DVB Time and Date Table (TDT) at the specified table repetition interval.
newtype DvbTdtSettings = DvbTdtSettings 
  { "TdtInterval" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDvbTdtSettings :: Newtype DvbTdtSettings _
derive instance repGenericDvbTdtSettings :: Generic DvbTdtSettings _
instance showDvbTdtSettings :: Show DvbTdtSettings where
  show = genericShow
instance decodeDvbTdtSettings :: Decode DvbTdtSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDvbTdtSettings :: Encode DvbTdtSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
newtype Eac3AttenuationControl = Eac3AttenuationControl String
derive instance newtypeEac3AttenuationControl :: Newtype Eac3AttenuationControl _
derive instance repGenericEac3AttenuationControl :: Generic Eac3AttenuationControl _
instance showEac3AttenuationControl :: Show Eac3AttenuationControl where
  show = genericShow
instance decodeEac3AttenuationControl :: Decode Eac3AttenuationControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3AttenuationControl :: Encode Eac3AttenuationControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies the "Bitstream Mode" (bsmod) for the emitted E-AC-3 stream. See ATSC A/52-2012 (Annex E) for background on these values.
newtype Eac3BitstreamMode = Eac3BitstreamMode String
derive instance newtypeEac3BitstreamMode :: Newtype Eac3BitstreamMode _
derive instance repGenericEac3BitstreamMode :: Generic Eac3BitstreamMode _
instance showEac3BitstreamMode :: Show Eac3BitstreamMode where
  show = genericShow
instance decodeEac3BitstreamMode :: Decode Eac3BitstreamMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3BitstreamMode :: Encode Eac3BitstreamMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Dolby Digital Plus coding mode. Determines number of channels.
newtype Eac3CodingMode = Eac3CodingMode String
derive instance newtypeEac3CodingMode :: Newtype Eac3CodingMode _
derive instance repGenericEac3CodingMode :: Generic Eac3CodingMode _
instance showEac3CodingMode :: Show Eac3CodingMode where
  show = genericShow
instance decodeEac3CodingMode :: Decode Eac3CodingMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3CodingMode :: Encode Eac3CodingMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Activates a DC highpass filter for all input channels.
newtype Eac3DcFilter = Eac3DcFilter String
derive instance newtypeEac3DcFilter :: Newtype Eac3DcFilter _
derive instance repGenericEac3DcFilter :: Generic Eac3DcFilter _
instance showEac3DcFilter :: Show Eac3DcFilter where
  show = genericShow
instance decodeEac3DcFilter :: Decode Eac3DcFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3DcFilter :: Encode Eac3DcFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables Dynamic Range Compression that restricts the absolute peak level for a signal.
newtype Eac3DynamicRangeCompressionLine = Eac3DynamicRangeCompressionLine String
derive instance newtypeEac3DynamicRangeCompressionLine :: Newtype Eac3DynamicRangeCompressionLine _
derive instance repGenericEac3DynamicRangeCompressionLine :: Generic Eac3DynamicRangeCompressionLine _
instance showEac3DynamicRangeCompressionLine :: Show Eac3DynamicRangeCompressionLine where
  show = genericShow
instance decodeEac3DynamicRangeCompressionLine :: Decode Eac3DynamicRangeCompressionLine where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3DynamicRangeCompressionLine :: Encode Eac3DynamicRangeCompressionLine where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables Heavy Dynamic Range Compression, ensures that the instantaneous signal peaks do not exceed specified levels.
newtype Eac3DynamicRangeCompressionRf = Eac3DynamicRangeCompressionRf String
derive instance newtypeEac3DynamicRangeCompressionRf :: Newtype Eac3DynamicRangeCompressionRf _
derive instance repGenericEac3DynamicRangeCompressionRf :: Generic Eac3DynamicRangeCompressionRf _
instance showEac3DynamicRangeCompressionRf :: Show Eac3DynamicRangeCompressionRf where
  show = genericShow
instance decodeEac3DynamicRangeCompressionRf :: Decode Eac3DynamicRangeCompressionRf where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3DynamicRangeCompressionRf :: Encode Eac3DynamicRangeCompressionRf where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When encoding 3/2 audio, controls whether the LFE channel is enabled
newtype Eac3LfeControl = Eac3LfeControl String
derive instance newtypeEac3LfeControl :: Newtype Eac3LfeControl _
derive instance repGenericEac3LfeControl :: Generic Eac3LfeControl _
instance showEac3LfeControl :: Show Eac3LfeControl where
  show = genericShow
instance decodeEac3LfeControl :: Decode Eac3LfeControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3LfeControl :: Encode Eac3LfeControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
newtype Eac3LfeFilter = Eac3LfeFilter String
derive instance newtypeEac3LfeFilter :: Newtype Eac3LfeFilter _
derive instance repGenericEac3LfeFilter :: Generic Eac3LfeFilter _
instance showEac3LfeFilter :: Show Eac3LfeFilter where
  show = genericShow
instance decodeEac3LfeFilter :: Decode Eac3LfeFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3LfeFilter :: Encode Eac3LfeFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
newtype Eac3MetadataControl = Eac3MetadataControl String
derive instance newtypeEac3MetadataControl :: Newtype Eac3MetadataControl _
derive instance repGenericEac3MetadataControl :: Generic Eac3MetadataControl _
instance showEac3MetadataControl :: Show Eac3MetadataControl where
  show = genericShow
instance decodeEac3MetadataControl :: Decode Eac3MetadataControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3MetadataControl :: Encode Eac3MetadataControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
newtype Eac3PassthroughControl = Eac3PassthroughControl String
derive instance newtypeEac3PassthroughControl :: Newtype Eac3PassthroughControl _
derive instance repGenericEac3PassthroughControl :: Generic Eac3PassthroughControl _
instance showEac3PassthroughControl :: Show Eac3PassthroughControl where
  show = genericShow
instance decodeEac3PassthroughControl :: Decode Eac3PassthroughControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3PassthroughControl :: Encode Eac3PassthroughControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
newtype Eac3PhaseControl = Eac3PhaseControl String
derive instance newtypeEac3PhaseControl :: Newtype Eac3PhaseControl _
derive instance repGenericEac3PhaseControl :: Generic Eac3PhaseControl _
instance showEac3PhaseControl :: Show Eac3PhaseControl where
  show = genericShow
instance decodeEac3PhaseControl :: Decode Eac3PhaseControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3PhaseControl :: Encode Eac3PhaseControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
newtype Eac3Settings = Eac3Settings 
  { "AttenuationControl" :: NullOrUndefined.NullOrUndefined (Eac3AttenuationControl)
  , "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "BitstreamMode" :: NullOrUndefined.NullOrUndefined (Eac3BitstreamMode)
  , "CodingMode" :: NullOrUndefined.NullOrUndefined (Eac3CodingMode)
  , "DcFilter" :: NullOrUndefined.NullOrUndefined (Eac3DcFilter)
  , "Dialnorm" :: NullOrUndefined.NullOrUndefined (Int)
  , "DynamicRangeCompressionLine" :: NullOrUndefined.NullOrUndefined (Eac3DynamicRangeCompressionLine)
  , "DynamicRangeCompressionRf" :: NullOrUndefined.NullOrUndefined (Eac3DynamicRangeCompressionRf)
  , "LfeControl" :: NullOrUndefined.NullOrUndefined (Eac3LfeControl)
  , "LfeFilter" :: NullOrUndefined.NullOrUndefined (Eac3LfeFilter)
  , "LoRoCenterMixLevel" :: NullOrUndefined.NullOrUndefined (Number)
  , "LoRoSurroundMixLevel" :: NullOrUndefined.NullOrUndefined (Number)
  , "LtRtCenterMixLevel" :: NullOrUndefined.NullOrUndefined (Number)
  , "LtRtSurroundMixLevel" :: NullOrUndefined.NullOrUndefined (Number)
  , "MetadataControl" :: NullOrUndefined.NullOrUndefined (Eac3MetadataControl)
  , "PassthroughControl" :: NullOrUndefined.NullOrUndefined (Eac3PassthroughControl)
  , "PhaseControl" :: NullOrUndefined.NullOrUndefined (Eac3PhaseControl)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (Int)
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


-- | Stereo downmix preference. Only used for 3/2 coding mode.
newtype Eac3StereoDownmix = Eac3StereoDownmix String
derive instance newtypeEac3StereoDownmix :: Newtype Eac3StereoDownmix _
derive instance repGenericEac3StereoDownmix :: Generic Eac3StereoDownmix _
instance showEac3StereoDownmix :: Show Eac3StereoDownmix where
  show = genericShow
instance decodeEac3StereoDownmix :: Decode Eac3StereoDownmix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3StereoDownmix :: Encode Eac3StereoDownmix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
newtype Eac3SurroundExMode = Eac3SurroundExMode String
derive instance newtypeEac3SurroundExMode :: Newtype Eac3SurroundExMode _
derive instance repGenericEac3SurroundExMode :: Generic Eac3SurroundExMode _
instance showEac3SurroundExMode :: Show Eac3SurroundExMode where
  show = genericShow
instance decodeEac3SurroundExMode :: Decode Eac3SurroundExMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3SurroundExMode :: Encode Eac3SurroundExMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
newtype Eac3SurroundMode = Eac3SurroundMode String
derive instance newtypeEac3SurroundMode :: Newtype Eac3SurroundMode _
derive instance repGenericEac3SurroundMode :: Generic Eac3SurroundMode _
instance showEac3SurroundMode :: Show Eac3SurroundMode where
  show = genericShow
instance decodeEac3SurroundMode :: Decode Eac3SurroundMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEac3SurroundMode :: Encode Eac3SurroundMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to UPCONVERT, 608 data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
newtype EmbeddedConvert608To708 = EmbeddedConvert608To708 String
derive instance newtypeEmbeddedConvert608To708 :: Newtype EmbeddedConvert608To708 _
derive instance repGenericEmbeddedConvert608To708 :: Generic EmbeddedConvert608To708 _
instance showEmbeddedConvert608To708 :: Show EmbeddedConvert608To708 where
  show = genericShow
instance decodeEmbeddedConvert608To708 :: Decode EmbeddedConvert608To708 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmbeddedConvert608To708 :: Encode EmbeddedConvert608To708 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for embedded captions Source
newtype EmbeddedSourceSettings = EmbeddedSourceSettings 
  { "Convert608To708" :: NullOrUndefined.NullOrUndefined (EmbeddedConvert608To708)
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


-- | Describes account specific API endpoint
newtype Endpoint = Endpoint 
  { "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEndpoint :: Newtype Endpoint _
derive instance repGenericEndpoint :: Generic Endpoint _
instance showEndpoint :: Show Endpoint where
  show = genericShow
instance decodeEndpoint :: Decode Endpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpoint :: Encode Endpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExceptionBody = ExceptionBody 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeExceptionBody :: Newtype ExceptionBody _
derive instance repGenericExceptionBody :: Generic ExceptionBody _
instance showExceptionBody :: Show ExceptionBody where
  show = genericShow
instance decodeExceptionBody :: Decode ExceptionBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExceptionBody :: Encode ExceptionBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
newtype F4vMoovPlacement = F4vMoovPlacement String
derive instance newtypeF4vMoovPlacement :: Newtype F4vMoovPlacement _
derive instance repGenericF4vMoovPlacement :: Generic F4vMoovPlacement _
instance showF4vMoovPlacement :: Show F4vMoovPlacement where
  show = genericShow
instance decodeF4vMoovPlacement :: Decode F4vMoovPlacement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeF4vMoovPlacement :: Encode F4vMoovPlacement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for F4v container
newtype F4vSettings = F4vSettings 
  { "MoovPlacement" :: NullOrUndefined.NullOrUndefined (F4vMoovPlacement)
  }
derive instance newtypeF4vSettings :: Newtype F4vSettings _
derive instance repGenericF4vSettings :: Generic F4vSettings _
instance showF4vSettings :: Show F4vSettings where
  show = genericShow
instance decodeF4vSettings :: Decode F4vSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeF4vSettings :: Encode F4vSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to FILE_GROUP_SETTINGS.
newtype FileGroupSettings = FileGroupSettings 
  { "Destination" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeFileGroupSettings :: Newtype FileGroupSettings _
derive instance repGenericFileGroupSettings :: Generic FileGroupSettings _
instance showFileGroupSettings :: Show FileGroupSettings where
  show = genericShow
instance decodeFileGroupSettings :: Decode FileGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileGroupSettings :: Encode FileGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If set to UPCONVERT, 608 caption data is both passed through via the "608 compatibility bytes" fields of the 708 wrapper as well as translated into 708. 708 data present in the source content will be discarded.
newtype FileSourceConvert608To708 = FileSourceConvert608To708 String
derive instance newtypeFileSourceConvert608To708 :: Newtype FileSourceConvert608To708 _
derive instance repGenericFileSourceConvert608To708 :: Generic FileSourceConvert608To708 _
instance showFileSourceConvert608To708 :: Show FileSourceConvert608To708 where
  show = genericShow
instance decodeFileSourceConvert608To708 :: Decode FileSourceConvert608To708 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSourceConvert608To708 :: Encode FileSourceConvert608To708 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for File-based Captions in Source
newtype FileSourceSettings = FileSourceSettings 
  { "Convert608To708" :: NullOrUndefined.NullOrUndefined (FileSourceConvert608To708)
  , "SourceFile" :: NullOrUndefined.NullOrUndefined (String)
  , "TimeDelta" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeFileSourceSettings :: Newtype FileSourceSettings _
derive instance repGenericFileSourceSettings :: Generic FileSourceSettings _
instance showFileSourceSettings :: Show FileSourceSettings where
  show = genericShow
instance decodeFileSourceSettings :: Decode FileSourceSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileSourceSettings :: Encode FileSourceSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | You don't have permissions for this action with the credentials you sent.
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


-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value FRAME_CAPTURE.
newtype FrameCaptureSettings = FrameCaptureSettings 
  { "FramerateDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "MaxCaptures" :: NullOrUndefined.NullOrUndefined (Int)
  , "Quality" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeFrameCaptureSettings :: Newtype FrameCaptureSettings _
derive instance repGenericFrameCaptureSettings :: Generic FrameCaptureSettings _
instance showFrameCaptureSettings :: Show FrameCaptureSettings where
  show = genericShow
instance decodeFrameCaptureSettings :: Decode FrameCaptureSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFrameCaptureSettings :: Encode FrameCaptureSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobRequest = GetJobRequest 
  { "Id" :: (String)
  }
derive instance newtypeGetJobRequest :: Newtype GetJobRequest _
derive instance repGenericGetJobRequest :: Generic GetJobRequest _
instance showGetJobRequest :: Show GetJobRequest where
  show = genericShow
instance decodeGetJobRequest :: Decode GetJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobRequest :: Encode GetJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobResponse = GetJobResponse 
  { "Job" :: NullOrUndefined.NullOrUndefined (Job)
  }
derive instance newtypeGetJobResponse :: Newtype GetJobResponse _
derive instance repGenericGetJobResponse :: Generic GetJobResponse _
instance showGetJobResponse :: Show GetJobResponse where
  show = genericShow
instance decodeGetJobResponse :: Decode GetJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobResponse :: Encode GetJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobTemplateRequest = GetJobTemplateRequest 
  { "Name" :: (String)
  }
derive instance newtypeGetJobTemplateRequest :: Newtype GetJobTemplateRequest _
derive instance repGenericGetJobTemplateRequest :: Generic GetJobTemplateRequest _
instance showGetJobTemplateRequest :: Show GetJobTemplateRequest where
  show = genericShow
instance decodeGetJobTemplateRequest :: Decode GetJobTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobTemplateRequest :: Encode GetJobTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobTemplateResponse = GetJobTemplateResponse 
  { "JobTemplate" :: NullOrUndefined.NullOrUndefined (JobTemplate)
  }
derive instance newtypeGetJobTemplateResponse :: Newtype GetJobTemplateResponse _
derive instance repGenericGetJobTemplateResponse :: Generic GetJobTemplateResponse _
instance showGetJobTemplateResponse :: Show GetJobTemplateResponse where
  show = genericShow
instance decodeGetJobTemplateResponse :: Decode GetJobTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobTemplateResponse :: Encode GetJobTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPresetRequest = GetPresetRequest 
  { "Name" :: (String)
  }
derive instance newtypeGetPresetRequest :: Newtype GetPresetRequest _
derive instance repGenericGetPresetRequest :: Generic GetPresetRequest _
instance showGetPresetRequest :: Show GetPresetRequest where
  show = genericShow
instance decodeGetPresetRequest :: Decode GetPresetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPresetRequest :: Encode GetPresetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPresetResponse = GetPresetResponse 
  { "Preset" :: NullOrUndefined.NullOrUndefined (Preset)
  }
derive instance newtypeGetPresetResponse :: Newtype GetPresetResponse _
derive instance repGenericGetPresetResponse :: Generic GetPresetResponse _
instance showGetPresetResponse :: Show GetPresetResponse where
  show = genericShow
instance decodeGetPresetResponse :: Decode GetPresetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPresetResponse :: Encode GetPresetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetQueueRequest = GetQueueRequest 
  { "Name" :: (String)
  }
derive instance newtypeGetQueueRequest :: Newtype GetQueueRequest _
derive instance repGenericGetQueueRequest :: Generic GetQueueRequest _
instance showGetQueueRequest :: Show GetQueueRequest where
  show = genericShow
instance decodeGetQueueRequest :: Decode GetQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetQueueRequest :: Encode GetQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetQueueResponse = GetQueueResponse 
  { "Queue" :: NullOrUndefined.NullOrUndefined (Queue)
  }
derive instance newtypeGetQueueResponse :: Newtype GetQueueResponse _
derive instance repGenericGetQueueResponse :: Generic GetQueueResponse _
instance showGetQueueResponse :: Show GetQueueResponse where
  show = genericShow
instance decodeGetQueueResponse :: Decode GetQueueResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetQueueResponse :: Encode GetQueueResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
newtype H264AdaptiveQuantization = H264AdaptiveQuantization String
derive instance newtypeH264AdaptiveQuantization :: Newtype H264AdaptiveQuantization _
derive instance repGenericH264AdaptiveQuantization :: Generic H264AdaptiveQuantization _
instance showH264AdaptiveQuantization :: Show H264AdaptiveQuantization where
  show = genericShow
instance decodeH264AdaptiveQuantization :: Decode H264AdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264AdaptiveQuantization :: Encode H264AdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | H.264 Level.
newtype H264CodecLevel = H264CodecLevel String
derive instance newtypeH264CodecLevel :: Newtype H264CodecLevel _
derive instance repGenericH264CodecLevel :: Generic H264CodecLevel _
instance showH264CodecLevel :: Show H264CodecLevel where
  show = genericShow
instance decodeH264CodecLevel :: Decode H264CodecLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264CodecLevel :: Encode H264CodecLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
newtype H264CodecProfile = H264CodecProfile String
derive instance newtypeH264CodecProfile :: Newtype H264CodecProfile _
derive instance repGenericH264CodecProfile :: Generic H264CodecProfile _
instance showH264CodecProfile :: Show H264CodecProfile where
  show = genericShow
instance decodeH264CodecProfile :: Decode H264CodecProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264CodecProfile :: Encode H264CodecProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or CAVLC.
newtype H264EntropyEncoding = H264EntropyEncoding String
derive instance newtypeH264EntropyEncoding :: Newtype H264EntropyEncoding _
derive instance repGenericH264EntropyEncoding :: Generic H264EntropyEncoding _
instance showH264EntropyEncoding :: Show H264EntropyEncoding where
  show = genericShow
instance decodeH264EntropyEncoding :: Decode H264EntropyEncoding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264EntropyEncoding :: Encode H264EntropyEncoding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Choosing FORCE_FIELD disables PAFF encoding for interlaced outputs.
newtype H264FieldEncoding = H264FieldEncoding String
derive instance newtypeH264FieldEncoding :: Newtype H264FieldEncoding _
derive instance repGenericH264FieldEncoding :: Generic H264FieldEncoding _
instance showH264FieldEncoding :: Show H264FieldEncoding where
  show = genericShow
instance decodeH264FieldEncoding :: Decode H264FieldEncoding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264FieldEncoding :: Encode H264FieldEncoding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
newtype H264FlickerAdaptiveQuantization = H264FlickerAdaptiveQuantization String
derive instance newtypeH264FlickerAdaptiveQuantization :: Newtype H264FlickerAdaptiveQuantization _
derive instance repGenericH264FlickerAdaptiveQuantization :: Generic H264FlickerAdaptiveQuantization _
instance showH264FlickerAdaptiveQuantization :: Show H264FlickerAdaptiveQuantization where
  show = genericShow
instance decodeH264FlickerAdaptiveQuantization :: Decode H264FlickerAdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264FlickerAdaptiveQuantization :: Encode H264FlickerAdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
newtype H264FramerateControl = H264FramerateControl String
derive instance newtypeH264FramerateControl :: Newtype H264FramerateControl _
derive instance repGenericH264FramerateControl :: Generic H264FramerateControl _
instance showH264FramerateControl :: Show H264FramerateControl where
  show = genericShow
instance decodeH264FramerateControl :: Decode H264FramerateControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264FramerateControl :: Encode H264FramerateControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
newtype H264FramerateConversionAlgorithm = H264FramerateConversionAlgorithm String
derive instance newtypeH264FramerateConversionAlgorithm :: Newtype H264FramerateConversionAlgorithm _
derive instance repGenericH264FramerateConversionAlgorithm :: Generic H264FramerateConversionAlgorithm _
instance showH264FramerateConversionAlgorithm :: Show H264FramerateConversionAlgorithm where
  show = genericShow
instance decodeH264FramerateConversionAlgorithm :: Decode H264FramerateConversionAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264FramerateConversionAlgorithm :: Encode H264FramerateConversionAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If enable, use reference B frames for GOP structures that have B frames > 1.
newtype H264GopBReference = H264GopBReference String
derive instance newtypeH264GopBReference :: Newtype H264GopBReference _
derive instance repGenericH264GopBReference :: Generic H264GopBReference _
instance showH264GopBReference :: Show H264GopBReference where
  show = genericShow
instance decodeH264GopBReference :: Decode H264GopBReference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264GopBReference :: Encode H264GopBReference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates if the GOP Size in H264 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
newtype H264GopSizeUnits = H264GopSizeUnits String
derive instance newtypeH264GopSizeUnits :: Newtype H264GopSizeUnits _
derive instance repGenericH264GopSizeUnits :: Generic H264GopSizeUnits _
instance showH264GopSizeUnits :: Show H264GopSizeUnits where
  show = genericShow
instance decodeH264GopSizeUnits :: Decode H264GopSizeUnits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264GopSizeUnits :: Encode H264GopSizeUnits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
newtype H264InterlaceMode = H264InterlaceMode String
derive instance newtypeH264InterlaceMode :: Newtype H264InterlaceMode _
derive instance repGenericH264InterlaceMode :: Generic H264InterlaceMode _
instance showH264InterlaceMode :: Show H264InterlaceMode where
  show = genericShow
instance decodeH264InterlaceMode :: Decode H264InterlaceMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264InterlaceMode :: Encode H264InterlaceMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
newtype H264ParControl = H264ParControl String
derive instance newtypeH264ParControl :: Newtype H264ParControl _
derive instance repGenericH264ParControl :: Generic H264ParControl _
instance showH264ParControl :: Show H264ParControl where
  show = genericShow
instance decodeH264ParControl :: Decode H264ParControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264ParControl :: Encode H264ParControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Quality tuning level (H264QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.
newtype H264QualityTuningLevel = H264QualityTuningLevel String
derive instance newtypeH264QualityTuningLevel :: Newtype H264QualityTuningLevel _
derive instance repGenericH264QualityTuningLevel :: Generic H264QualityTuningLevel _
instance showH264QualityTuningLevel :: Show H264QualityTuningLevel where
  show = genericShow
instance decodeH264QualityTuningLevel :: Decode H264QualityTuningLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264QualityTuningLevel :: Encode H264QualityTuningLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Rate control mode. CQ uses constant quantizer (qp), ABR (average bitrate) does not write HRD parameters.
newtype H264RateControlMode = H264RateControlMode String
derive instance newtypeH264RateControlMode :: Newtype H264RateControlMode _
derive instance repGenericH264RateControlMode :: Generic H264RateControlMode _
instance showH264RateControlMode :: Show H264RateControlMode where
  show = genericShow
instance decodeH264RateControlMode :: Decode H264RateControlMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264RateControlMode :: Encode H264RateControlMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Places a PPS header on each encoded picture, even if repeated.
newtype H264RepeatPps = H264RepeatPps String
derive instance newtypeH264RepeatPps :: Newtype H264RepeatPps _
derive instance repGenericH264RepeatPps :: Generic H264RepeatPps _
instance showH264RepeatPps :: Show H264RepeatPps where
  show = genericShow
instance decodeH264RepeatPps :: Decode H264RepeatPps where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264RepeatPps :: Encode H264RepeatPps where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Scene change detection (inserts I-frames on scene changes).
newtype H264SceneChangeDetect = H264SceneChangeDetect String
derive instance newtypeH264SceneChangeDetect :: Newtype H264SceneChangeDetect _
derive instance repGenericH264SceneChangeDetect :: Generic H264SceneChangeDetect _
instance showH264SceneChangeDetect :: Show H264SceneChangeDetect where
  show = genericShow
instance decodeH264SceneChangeDetect :: Decode H264SceneChangeDetect where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264SceneChangeDetect :: Encode H264SceneChangeDetect where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value H_264.
newtype H264Settings = H264Settings 
  { "AdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (H264AdaptiveQuantization)
  , "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "CodecLevel" :: NullOrUndefined.NullOrUndefined (H264CodecLevel)
  , "CodecProfile" :: NullOrUndefined.NullOrUndefined (H264CodecProfile)
  , "EntropyEncoding" :: NullOrUndefined.NullOrUndefined (H264EntropyEncoding)
  , "FieldEncoding" :: NullOrUndefined.NullOrUndefined (H264FieldEncoding)
  , "FlickerAdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (H264FlickerAdaptiveQuantization)
  , "FramerateControl" :: NullOrUndefined.NullOrUndefined (H264FramerateControl)
  , "FramerateConversionAlgorithm" :: NullOrUndefined.NullOrUndefined (H264FramerateConversionAlgorithm)
  , "FramerateDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "GopBReference" :: NullOrUndefined.NullOrUndefined (H264GopBReference)
  , "GopClosedCadence" :: NullOrUndefined.NullOrUndefined (Int)
  , "GopSize" :: NullOrUndefined.NullOrUndefined (Number)
  , "GopSizeUnits" :: NullOrUndefined.NullOrUndefined (H264GopSizeUnits)
  , "HrdBufferInitialFillPercentage" :: NullOrUndefined.NullOrUndefined (Int)
  , "HrdBufferSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "InterlaceMode" :: NullOrUndefined.NullOrUndefined (H264InterlaceMode)
  , "MaxBitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinIInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "NumberBFramesBetweenReferenceFrames" :: NullOrUndefined.NullOrUndefined (Int)
  , "NumberReferenceFrames" :: NullOrUndefined.NullOrUndefined (Int)
  , "ParControl" :: NullOrUndefined.NullOrUndefined (H264ParControl)
  , "ParDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "QualityTuningLevel" :: NullOrUndefined.NullOrUndefined (H264QualityTuningLevel)
  , "RateControlMode" :: NullOrUndefined.NullOrUndefined (H264RateControlMode)
  , "RepeatPps" :: NullOrUndefined.NullOrUndefined (H264RepeatPps)
  , "SceneChangeDetect" :: NullOrUndefined.NullOrUndefined (H264SceneChangeDetect)
  , "Slices" :: NullOrUndefined.NullOrUndefined (Int)
  , "SlowPal" :: NullOrUndefined.NullOrUndefined (H264SlowPal)
  , "Softness" :: NullOrUndefined.NullOrUndefined (Int)
  , "SpatialAdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (H264SpatialAdaptiveQuantization)
  , "Syntax" :: NullOrUndefined.NullOrUndefined (H264Syntax)
  , "Telecine" :: NullOrUndefined.NullOrUndefined (H264Telecine)
  , "TemporalAdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (H264TemporalAdaptiveQuantization)
  , "UnregisteredSeiTimecode" :: NullOrUndefined.NullOrUndefined (H264UnregisteredSeiTimecode)
  }
derive instance newtypeH264Settings :: Newtype H264Settings _
derive instance repGenericH264Settings :: Generic H264Settings _
instance showH264Settings :: Show H264Settings where
  show = genericShow
instance decodeH264Settings :: Decode H264Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264Settings :: Encode H264Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
newtype H264SlowPal = H264SlowPal String
derive instance newtypeH264SlowPal :: Newtype H264SlowPal _
derive instance repGenericH264SlowPal :: Generic H264SlowPal _
instance showH264SlowPal :: Show H264SlowPal where
  show = genericShow
instance decodeH264SlowPal :: Decode H264SlowPal where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264SlowPal :: Encode H264SlowPal where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adjust quantization within each frame based on spatial variation of content complexity.
newtype H264SpatialAdaptiveQuantization = H264SpatialAdaptiveQuantization String
derive instance newtypeH264SpatialAdaptiveQuantization :: Newtype H264SpatialAdaptiveQuantization _
derive instance repGenericH264SpatialAdaptiveQuantization :: Generic H264SpatialAdaptiveQuantization _
instance showH264SpatialAdaptiveQuantization :: Show H264SpatialAdaptiveQuantization where
  show = genericShow
instance decodeH264SpatialAdaptiveQuantization :: Decode H264SpatialAdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264SpatialAdaptiveQuantization :: Encode H264SpatialAdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Produces a bitstream compliant with SMPTE RP-2027.
newtype H264Syntax = H264Syntax String
derive instance newtypeH264Syntax :: Newtype H264Syntax _
derive instance repGenericH264Syntax :: Generic H264Syntax _
instance showH264Syntax :: Show H264Syntax where
  show = genericShow
instance decodeH264Syntax :: Decode H264Syntax where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264Syntax :: Encode H264Syntax where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
newtype H264Telecine = H264Telecine String
derive instance newtypeH264Telecine :: Newtype H264Telecine _
derive instance repGenericH264Telecine :: Generic H264Telecine _
instance showH264Telecine :: Show H264Telecine where
  show = genericShow
instance decodeH264Telecine :: Decode H264Telecine where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264Telecine :: Encode H264Telecine where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adjust quantization within each frame based on temporal variation of content complexity.
newtype H264TemporalAdaptiveQuantization = H264TemporalAdaptiveQuantization String
derive instance newtypeH264TemporalAdaptiveQuantization :: Newtype H264TemporalAdaptiveQuantization _
derive instance repGenericH264TemporalAdaptiveQuantization :: Generic H264TemporalAdaptiveQuantization _
instance showH264TemporalAdaptiveQuantization :: Show H264TemporalAdaptiveQuantization where
  show = genericShow
instance decodeH264TemporalAdaptiveQuantization :: Decode H264TemporalAdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264TemporalAdaptiveQuantization :: Encode H264TemporalAdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
newtype H264UnregisteredSeiTimecode = H264UnregisteredSeiTimecode String
derive instance newtypeH264UnregisteredSeiTimecode :: Newtype H264UnregisteredSeiTimecode _
derive instance repGenericH264UnregisteredSeiTimecode :: Generic H264UnregisteredSeiTimecode _
instance showH264UnregisteredSeiTimecode :: Show H264UnregisteredSeiTimecode where
  show = genericShow
instance decodeH264UnregisteredSeiTimecode :: Decode H264UnregisteredSeiTimecode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH264UnregisteredSeiTimecode :: Encode H264UnregisteredSeiTimecode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
newtype H265AdaptiveQuantization = H265AdaptiveQuantization String
derive instance newtypeH265AdaptiveQuantization :: Newtype H265AdaptiveQuantization _
derive instance repGenericH265AdaptiveQuantization :: Generic H265AdaptiveQuantization _
instance showH265AdaptiveQuantization :: Show H265AdaptiveQuantization where
  show = genericShow
instance decodeH265AdaptiveQuantization :: Decode H265AdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265AdaptiveQuantization :: Encode H265AdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
newtype H265AlternateTransferFunctionSei = H265AlternateTransferFunctionSei String
derive instance newtypeH265AlternateTransferFunctionSei :: Newtype H265AlternateTransferFunctionSei _
derive instance repGenericH265AlternateTransferFunctionSei :: Generic H265AlternateTransferFunctionSei _
instance showH265AlternateTransferFunctionSei :: Show H265AlternateTransferFunctionSei where
  show = genericShow
instance decodeH265AlternateTransferFunctionSei :: Decode H265AlternateTransferFunctionSei where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265AlternateTransferFunctionSei :: Encode H265AlternateTransferFunctionSei where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | H.265 Level.
newtype H265CodecLevel = H265CodecLevel String
derive instance newtypeH265CodecLevel :: Newtype H265CodecLevel _
derive instance repGenericH265CodecLevel :: Generic H265CodecLevel _
instance showH265CodecLevel :: Show H265CodecLevel where
  show = genericShow
instance decodeH265CodecLevel :: Decode H265CodecLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265CodecLevel :: Encode H265CodecLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Represents the Profile and Tier, per the HEVC (H.265) specification. Selections are grouped as [Profile] / [Tier], so "Main/High" represents Main Profile with High Tier. 4:2:2 profiles are only available with the HEVC 4:2:2 License.
newtype H265CodecProfile = H265CodecProfile String
derive instance newtypeH265CodecProfile :: Newtype H265CodecProfile _
derive instance repGenericH265CodecProfile :: Generic H265CodecProfile _
instance showH265CodecProfile :: Show H265CodecProfile where
  show = genericShow
instance decodeH265CodecProfile :: Decode H265CodecProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265CodecProfile :: Encode H265CodecProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adjust quantization within each frame to reduce flicker or 'pop' on I-frames.
newtype H265FlickerAdaptiveQuantization = H265FlickerAdaptiveQuantization String
derive instance newtypeH265FlickerAdaptiveQuantization :: Newtype H265FlickerAdaptiveQuantization _
derive instance repGenericH265FlickerAdaptiveQuantization :: Generic H265FlickerAdaptiveQuantization _
instance showH265FlickerAdaptiveQuantization :: Show H265FlickerAdaptiveQuantization where
  show = genericShow
instance decodeH265FlickerAdaptiveQuantization :: Decode H265FlickerAdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265FlickerAdaptiveQuantization :: Encode H265FlickerAdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
newtype H265FramerateControl = H265FramerateControl String
derive instance newtypeH265FramerateControl :: Newtype H265FramerateControl _
derive instance repGenericH265FramerateControl :: Generic H265FramerateControl _
instance showH265FramerateControl :: Show H265FramerateControl where
  show = genericShow
instance decodeH265FramerateControl :: Decode H265FramerateControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265FramerateControl :: Encode H265FramerateControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
newtype H265FramerateConversionAlgorithm = H265FramerateConversionAlgorithm String
derive instance newtypeH265FramerateConversionAlgorithm :: Newtype H265FramerateConversionAlgorithm _
derive instance repGenericH265FramerateConversionAlgorithm :: Generic H265FramerateConversionAlgorithm _
instance showH265FramerateConversionAlgorithm :: Show H265FramerateConversionAlgorithm where
  show = genericShow
instance decodeH265FramerateConversionAlgorithm :: Decode H265FramerateConversionAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265FramerateConversionAlgorithm :: Encode H265FramerateConversionAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If enable, use reference B frames for GOP structures that have B frames > 1.
newtype H265GopBReference = H265GopBReference String
derive instance newtypeH265GopBReference :: Newtype H265GopBReference _
derive instance repGenericH265GopBReference :: Generic H265GopBReference _
instance showH265GopBReference :: Show H265GopBReference where
  show = genericShow
instance decodeH265GopBReference :: Decode H265GopBReference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265GopBReference :: Encode H265GopBReference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates if the GOP Size in H265 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
newtype H265GopSizeUnits = H265GopSizeUnits String
derive instance newtypeH265GopSizeUnits :: Newtype H265GopSizeUnits _
derive instance repGenericH265GopSizeUnits :: Generic H265GopSizeUnits _
instance showH265GopSizeUnits :: Show H265GopSizeUnits where
  show = genericShow
instance decodeH265GopSizeUnits :: Decode H265GopSizeUnits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265GopSizeUnits :: Encode H265GopSizeUnits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
newtype H265InterlaceMode = H265InterlaceMode String
derive instance newtypeH265InterlaceMode :: Newtype H265InterlaceMode _
derive instance repGenericH265InterlaceMode :: Generic H265InterlaceMode _
instance showH265InterlaceMode :: Show H265InterlaceMode where
  show = genericShow
instance decodeH265InterlaceMode :: Decode H265InterlaceMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265InterlaceMode :: Encode H265InterlaceMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
newtype H265ParControl = H265ParControl String
derive instance newtypeH265ParControl :: Newtype H265ParControl _
derive instance repGenericH265ParControl :: Generic H265ParControl _
instance showH265ParControl :: Show H265ParControl where
  show = genericShow
instance decodeH265ParControl :: Decode H265ParControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265ParControl :: Encode H265ParControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Quality tuning level (H265QualityTuningLevel) to specifiy whether to use fast single-pass, high-quality singlepass, or high-quality multipass video encoding.
newtype H265QualityTuningLevel = H265QualityTuningLevel String
derive instance newtypeH265QualityTuningLevel :: Newtype H265QualityTuningLevel _
derive instance repGenericH265QualityTuningLevel :: Generic H265QualityTuningLevel _
instance showH265QualityTuningLevel :: Show H265QualityTuningLevel where
  show = genericShow
instance decodeH265QualityTuningLevel :: Decode H265QualityTuningLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265QualityTuningLevel :: Encode H265QualityTuningLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Rate control mode. CQ uses constant quantizer (qp), ABR (average bitrate) does not write HRD parameters.
newtype H265RateControlMode = H265RateControlMode String
derive instance newtypeH265RateControlMode :: Newtype H265RateControlMode _
derive instance repGenericH265RateControlMode :: Generic H265RateControlMode _
instance showH265RateControlMode :: Show H265RateControlMode where
  show = genericShow
instance decodeH265RateControlMode :: Decode H265RateControlMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265RateControlMode :: Encode H265RateControlMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specify Sample Adaptive Offset (SAO) filter strength.  Adaptive mode dynamically selects best strength based on content
newtype H265SampleAdaptiveOffsetFilterMode = H265SampleAdaptiveOffsetFilterMode String
derive instance newtypeH265SampleAdaptiveOffsetFilterMode :: Newtype H265SampleAdaptiveOffsetFilterMode _
derive instance repGenericH265SampleAdaptiveOffsetFilterMode :: Generic H265SampleAdaptiveOffsetFilterMode _
instance showH265SampleAdaptiveOffsetFilterMode :: Show H265SampleAdaptiveOffsetFilterMode where
  show = genericShow
instance decodeH265SampleAdaptiveOffsetFilterMode :: Decode H265SampleAdaptiveOffsetFilterMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265SampleAdaptiveOffsetFilterMode :: Encode H265SampleAdaptiveOffsetFilterMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Scene change detection (inserts I-frames on scene changes).
newtype H265SceneChangeDetect = H265SceneChangeDetect String
derive instance newtypeH265SceneChangeDetect :: Newtype H265SceneChangeDetect _
derive instance repGenericH265SceneChangeDetect :: Generic H265SceneChangeDetect _
instance showH265SceneChangeDetect :: Show H265SceneChangeDetect where
  show = genericShow
instance decodeH265SceneChangeDetect :: Decode H265SceneChangeDetect where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265SceneChangeDetect :: Encode H265SceneChangeDetect where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for H265 codec
newtype H265Settings = H265Settings 
  { "AdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (H265AdaptiveQuantization)
  , "AlternateTransferFunctionSei" :: NullOrUndefined.NullOrUndefined (H265AlternateTransferFunctionSei)
  , "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "CodecLevel" :: NullOrUndefined.NullOrUndefined (H265CodecLevel)
  , "CodecProfile" :: NullOrUndefined.NullOrUndefined (H265CodecProfile)
  , "FlickerAdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (H265FlickerAdaptiveQuantization)
  , "FramerateControl" :: NullOrUndefined.NullOrUndefined (H265FramerateControl)
  , "FramerateConversionAlgorithm" :: NullOrUndefined.NullOrUndefined (H265FramerateConversionAlgorithm)
  , "FramerateDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "GopBReference" :: NullOrUndefined.NullOrUndefined (H265GopBReference)
  , "GopClosedCadence" :: NullOrUndefined.NullOrUndefined (Int)
  , "GopSize" :: NullOrUndefined.NullOrUndefined (Number)
  , "GopSizeUnits" :: NullOrUndefined.NullOrUndefined (H265GopSizeUnits)
  , "HrdBufferInitialFillPercentage" :: NullOrUndefined.NullOrUndefined (Int)
  , "HrdBufferSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "InterlaceMode" :: NullOrUndefined.NullOrUndefined (H265InterlaceMode)
  , "MaxBitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinIInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "NumberBFramesBetweenReferenceFrames" :: NullOrUndefined.NullOrUndefined (Int)
  , "NumberReferenceFrames" :: NullOrUndefined.NullOrUndefined (Int)
  , "ParControl" :: NullOrUndefined.NullOrUndefined (H265ParControl)
  , "ParDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "QualityTuningLevel" :: NullOrUndefined.NullOrUndefined (H265QualityTuningLevel)
  , "RateControlMode" :: NullOrUndefined.NullOrUndefined (H265RateControlMode)
  , "SampleAdaptiveOffsetFilterMode" :: NullOrUndefined.NullOrUndefined (H265SampleAdaptiveOffsetFilterMode)
  , "SceneChangeDetect" :: NullOrUndefined.NullOrUndefined (H265SceneChangeDetect)
  , "Slices" :: NullOrUndefined.NullOrUndefined (Int)
  , "SlowPal" :: NullOrUndefined.NullOrUndefined (H265SlowPal)
  , "SpatialAdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (H265SpatialAdaptiveQuantization)
  , "Telecine" :: NullOrUndefined.NullOrUndefined (H265Telecine)
  , "TemporalAdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (H265TemporalAdaptiveQuantization)
  , "TemporalIds" :: NullOrUndefined.NullOrUndefined (H265TemporalIds)
  , "Tiles" :: NullOrUndefined.NullOrUndefined (H265Tiles)
  , "UnregisteredSeiTimecode" :: NullOrUndefined.NullOrUndefined (H265UnregisteredSeiTimecode)
  }
derive instance newtypeH265Settings :: Newtype H265Settings _
derive instance repGenericH265Settings :: Generic H265Settings _
instance showH265Settings :: Show H265Settings where
  show = genericShow
instance decodeH265Settings :: Decode H265Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265Settings :: Encode H265Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
newtype H265SlowPal = H265SlowPal String
derive instance newtypeH265SlowPal :: Newtype H265SlowPal _
derive instance repGenericH265SlowPal :: Generic H265SlowPal _
instance showH265SlowPal :: Show H265SlowPal where
  show = genericShow
instance decodeH265SlowPal :: Decode H265SlowPal where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265SlowPal :: Encode H265SlowPal where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adjust quantization within each frame based on spatial variation of content complexity.
newtype H265SpatialAdaptiveQuantization = H265SpatialAdaptiveQuantization String
derive instance newtypeH265SpatialAdaptiveQuantization :: Newtype H265SpatialAdaptiveQuantization _
derive instance repGenericH265SpatialAdaptiveQuantization :: Generic H265SpatialAdaptiveQuantization _
instance showH265SpatialAdaptiveQuantization :: Show H265SpatialAdaptiveQuantization where
  show = genericShow
instance decodeH265SpatialAdaptiveQuantization :: Decode H265SpatialAdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265SpatialAdaptiveQuantization :: Encode H265SpatialAdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This field applies only if the Streams > Advanced > Framerate (framerate) field  is set to 29.970. This field works with the Streams > Advanced > Preprocessors > Deinterlacer  field (deinterlace_mode) and the Streams > Advanced > Interlaced Mode field (interlace_mode)  to identify the scan type for the output: Progressive, Interlaced, Hard Telecine or Soft Telecine. - Hard: produces 29.97i output from 23.976 input. - Soft: produces 23.976; the player converts this output to 29.97i.
newtype H265Telecine = H265Telecine String
derive instance newtypeH265Telecine :: Newtype H265Telecine _
derive instance repGenericH265Telecine :: Generic H265Telecine _
instance showH265Telecine :: Show H265Telecine where
  show = genericShow
instance decodeH265Telecine :: Decode H265Telecine where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265Telecine :: Encode H265Telecine where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adjust quantization within each frame based on temporal variation of content complexity.
newtype H265TemporalAdaptiveQuantization = H265TemporalAdaptiveQuantization String
derive instance newtypeH265TemporalAdaptiveQuantization :: Newtype H265TemporalAdaptiveQuantization _
derive instance repGenericH265TemporalAdaptiveQuantization :: Generic H265TemporalAdaptiveQuantization _
instance showH265TemporalAdaptiveQuantization :: Show H265TemporalAdaptiveQuantization where
  show = genericShow
instance decodeH265TemporalAdaptiveQuantization :: Decode H265TemporalAdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265TemporalAdaptiveQuantization :: Encode H265TemporalAdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3 layers are supported depending on GOP structure: I- and P-frames form one layer, reference B-frames can form a second layer and non-reference b-frames can form a third layer. Decoders can optionally decode only the lower temporal layers to generate a lower frame rate output. For example, given a bitstream with temporal IDs and with b-frames = 1 (i.e. IbPbPb display order), a decoder could decode all the frames for full frame rate output or only the I and P frames (lowest temporal layer) for a half frame rate output.
newtype H265TemporalIds = H265TemporalIds String
derive instance newtypeH265TemporalIds :: Newtype H265TemporalIds _
derive instance repGenericH265TemporalIds :: Generic H265TemporalIds _
instance showH265TemporalIds :: Show H265TemporalIds where
  show = genericShow
instance decodeH265TemporalIds :: Decode H265TemporalIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265TemporalIds :: Encode H265TemporalIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable use of tiles, allowing horizontal as well as vertical subdivision of the encoded pictures.
newtype H265Tiles = H265Tiles String
derive instance newtypeH265Tiles :: Newtype H265Tiles _
derive instance repGenericH265Tiles :: Generic H265Tiles _
instance showH265Tiles :: Show H265Tiles where
  show = genericShow
instance decodeH265Tiles :: Decode H265Tiles where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265Tiles :: Encode H265Tiles where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
newtype H265UnregisteredSeiTimecode = H265UnregisteredSeiTimecode String
derive instance newtypeH265UnregisteredSeiTimecode :: Newtype H265UnregisteredSeiTimecode _
derive instance repGenericH265UnregisteredSeiTimecode :: Generic H265UnregisteredSeiTimecode _
instance showH265UnregisteredSeiTimecode :: Show H265UnregisteredSeiTimecode where
  show = genericShow
instance decodeH265UnregisteredSeiTimecode :: Decode H265UnregisteredSeiTimecode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeH265UnregisteredSeiTimecode :: Encode H265UnregisteredSeiTimecode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use the HDR master display (Hdr10Metadata) settings to provide values for HDR color. These values vary depending on the input video and must be provided by a color grader. Range is 0 to 50,000, each increment represents 0.00002 in CIE1931 color coordinate.
newtype Hdr10Metadata = Hdr10Metadata 
  { "BluePrimaryX" :: NullOrUndefined.NullOrUndefined (Int)
  , "BluePrimaryY" :: NullOrUndefined.NullOrUndefined (Int)
  , "GreenPrimaryX" :: NullOrUndefined.NullOrUndefined (Int)
  , "GreenPrimaryY" :: NullOrUndefined.NullOrUndefined (Int)
  , "MaxContentLightLevel" :: NullOrUndefined.NullOrUndefined (Int)
  , "MaxFrameAverageLightLevel" :: NullOrUndefined.NullOrUndefined (Int)
  , "MaxLuminance" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinLuminance" :: NullOrUndefined.NullOrUndefined (Int)
  , "RedPrimaryX" :: NullOrUndefined.NullOrUndefined (Int)
  , "RedPrimaryY" :: NullOrUndefined.NullOrUndefined (Int)
  , "WhitePointX" :: NullOrUndefined.NullOrUndefined (Int)
  , "WhitePointY" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeHdr10Metadata :: Newtype Hdr10Metadata _
derive instance repGenericHdr10Metadata :: Generic Hdr10Metadata _
instance showHdr10Metadata :: Show Hdr10Metadata where
  show = genericShow
instance decodeHdr10Metadata :: Decode Hdr10Metadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHdr10Metadata :: Encode Hdr10Metadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HlsAdMarkers = HlsAdMarkers String
derive instance newtypeHlsAdMarkers :: Newtype HlsAdMarkers _
derive instance repGenericHlsAdMarkers :: Generic HlsAdMarkers _
instance showHlsAdMarkers :: Show HlsAdMarkers where
  show = genericShow
instance decodeHlsAdMarkers :: Decode HlsAdMarkers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsAdMarkers :: Encode HlsAdMarkers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
newtype HlsAudioTrackType = HlsAudioTrackType String
derive instance newtypeHlsAudioTrackType :: Newtype HlsAudioTrackType _
derive instance repGenericHlsAudioTrackType :: Generic HlsAudioTrackType _
instance showHlsAudioTrackType :: Show HlsAudioTrackType where
  show = genericShow
instance decodeHlsAudioTrackType :: Decode HlsAudioTrackType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsAudioTrackType :: Encode HlsAudioTrackType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Caption Language Mapping
newtype HlsCaptionLanguageMapping = HlsCaptionLanguageMapping 
  { "CaptionChannel" :: NullOrUndefined.NullOrUndefined (Int)
  , "LanguageCode" :: NullOrUndefined.NullOrUndefined (LanguageCode)
  , "LanguageDescription" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHlsCaptionLanguageMapping :: Newtype HlsCaptionLanguageMapping _
derive instance repGenericHlsCaptionLanguageMapping :: Generic HlsCaptionLanguageMapping _
instance showHlsCaptionLanguageMapping :: Show HlsCaptionLanguageMapping where
  show = genericShow
instance decodeHlsCaptionLanguageMapping :: Decode HlsCaptionLanguageMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsCaptionLanguageMapping :: Encode HlsCaptionLanguageMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Applies only to 608 Embedded output captions. Insert: Include CLOSED-CAPTIONS lines in the manifest. Specify at least one language in the CC1 Language Code field. One CLOSED-CAPTION line is added for each Language Code you specify. Make sure to specify the languages in the order in which they appear in the original source (if the source is embedded format) or the order of the caption selectors (if the source is other than embedded). Otherwise, languages in the manifest will not match up properly with the output captions. None: Include CLOSED-CAPTIONS=NONE line in the manifest. Omit: Omit any CLOSED-CAPTIONS line from the manifest.
newtype HlsCaptionLanguageSetting = HlsCaptionLanguageSetting String
derive instance newtypeHlsCaptionLanguageSetting :: Newtype HlsCaptionLanguageSetting _
derive instance repGenericHlsCaptionLanguageSetting :: Generic HlsCaptionLanguageSetting _
instance showHlsCaptionLanguageSetting :: Show HlsCaptionLanguageSetting where
  show = genericShow
instance decodeHlsCaptionLanguageSetting :: Decode HlsCaptionLanguageSetting where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsCaptionLanguageSetting :: Encode HlsCaptionLanguageSetting where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to ENABLED, sets #EXT-X-ALLOW-CACHE:no tag, which prevents client from saving media segments for later replay.
newtype HlsClientCache = HlsClientCache String
derive instance newtypeHlsClientCache :: Newtype HlsClientCache _
derive instance repGenericHlsClientCache :: Generic HlsClientCache _
instance showHlsClientCache :: Show HlsClientCache where
  show = genericShow
instance decodeHlsClientCache :: Decode HlsClientCache where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsClientCache :: Encode HlsClientCache where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
newtype HlsCodecSpecification = HlsCodecSpecification String
derive instance newtypeHlsCodecSpecification :: Newtype HlsCodecSpecification _
derive instance repGenericHlsCodecSpecification :: Generic HlsCodecSpecification _
instance showHlsCodecSpecification :: Show HlsCodecSpecification where
  show = genericShow
instance decodeHlsCodecSpecification :: Decode HlsCodecSpecification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsCodecSpecification :: Encode HlsCodecSpecification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates whether segments should be placed in subdirectories.
newtype HlsDirectoryStructure = HlsDirectoryStructure String
derive instance newtypeHlsDirectoryStructure :: Newtype HlsDirectoryStructure _
derive instance repGenericHlsDirectoryStructure :: Generic HlsDirectoryStructure _
instance showHlsDirectoryStructure :: Show HlsDirectoryStructure where
  show = genericShow
instance decodeHlsDirectoryStructure :: Decode HlsDirectoryStructure where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsDirectoryStructure :: Encode HlsDirectoryStructure where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for HLS encryption
newtype HlsEncryptionSettings = HlsEncryptionSettings 
  { "ConstantInitializationVector" :: NullOrUndefined.NullOrUndefined (String)
  , "EncryptionMethod" :: NullOrUndefined.NullOrUndefined (HlsEncryptionType)
  , "InitializationVectorInManifest" :: NullOrUndefined.NullOrUndefined (HlsInitializationVectorInManifest)
  , "SpekeKeyProvider" :: NullOrUndefined.NullOrUndefined (SpekeKeyProvider)
  , "StaticKeyProvider" :: NullOrUndefined.NullOrUndefined (StaticKeyProvider)
  , "Type" :: NullOrUndefined.NullOrUndefined (HlsKeyProviderType)
  }
derive instance newtypeHlsEncryptionSettings :: Newtype HlsEncryptionSettings _
derive instance repGenericHlsEncryptionSettings :: Generic HlsEncryptionSettings _
instance showHlsEncryptionSettings :: Show HlsEncryptionSettings where
  show = genericShow
instance decodeHlsEncryptionSettings :: Decode HlsEncryptionSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsEncryptionSettings :: Encode HlsEncryptionSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Encrypts the segments with the given encryption scheme. Leave blank to disable. Selecting 'Disabled' in the web interface also disables encryption.
newtype HlsEncryptionType = HlsEncryptionType String
derive instance newtypeHlsEncryptionType :: Newtype HlsEncryptionType _
derive instance repGenericHlsEncryptionType :: Generic HlsEncryptionType _
instance showHlsEncryptionType :: Show HlsEncryptionType where
  show = genericShow
instance decodeHlsEncryptionType :: Decode HlsEncryptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsEncryptionType :: Encode HlsEncryptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to HLS_GROUP_SETTINGS.
newtype HlsGroupSettings = HlsGroupSettings 
  { "AdMarkers" :: NullOrUndefined.NullOrUndefined (ListOfHlsAdMarkers)
  , "BaseUrl" :: NullOrUndefined.NullOrUndefined (String)
  , "CaptionLanguageMappings" :: NullOrUndefined.NullOrUndefined (ListOfHlsCaptionLanguageMapping)
  , "CaptionLanguageSetting" :: NullOrUndefined.NullOrUndefined (HlsCaptionLanguageSetting)
  , "ClientCache" :: NullOrUndefined.NullOrUndefined (HlsClientCache)
  , "CodecSpecification" :: NullOrUndefined.NullOrUndefined (HlsCodecSpecification)
  , "Destination" :: NullOrUndefined.NullOrUndefined (String)
  , "DirectoryStructure" :: NullOrUndefined.NullOrUndefined (HlsDirectoryStructure)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (HlsEncryptionSettings)
  , "ManifestCompression" :: NullOrUndefined.NullOrUndefined (HlsManifestCompression)
  , "ManifestDurationFormat" :: NullOrUndefined.NullOrUndefined (HlsManifestDurationFormat)
  , "MinSegmentLength" :: NullOrUndefined.NullOrUndefined (Int)
  , "OutputSelection" :: NullOrUndefined.NullOrUndefined (HlsOutputSelection)
  , "ProgramDateTime" :: NullOrUndefined.NullOrUndefined (HlsProgramDateTime)
  , "ProgramDateTimePeriod" :: NullOrUndefined.NullOrUndefined (Int)
  , "SegmentControl" :: NullOrUndefined.NullOrUndefined (HlsSegmentControl)
  , "SegmentLength" :: NullOrUndefined.NullOrUndefined (Int)
  , "SegmentsPerSubdirectory" :: NullOrUndefined.NullOrUndefined (Int)
  , "StreamInfResolution" :: NullOrUndefined.NullOrUndefined (HlsStreamInfResolution)
  , "TimedMetadataId3Frame" :: NullOrUndefined.NullOrUndefined (HlsTimedMetadataId3Frame)
  , "TimedMetadataId3Period" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimestampDeltaMilliseconds" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeHlsGroupSettings :: Newtype HlsGroupSettings _
derive instance repGenericHlsGroupSettings :: Generic HlsGroupSettings _
instance showHlsGroupSettings :: Show HlsGroupSettings where
  show = genericShow
instance decodeHlsGroupSettings :: Decode HlsGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsGroupSettings :: Encode HlsGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
newtype HlsIFrameOnlyManifest = HlsIFrameOnlyManifest String
derive instance newtypeHlsIFrameOnlyManifest :: Newtype HlsIFrameOnlyManifest _
derive instance repGenericHlsIFrameOnlyManifest :: Generic HlsIFrameOnlyManifest _
instance showHlsIFrameOnlyManifest :: Show HlsIFrameOnlyManifest where
  show = genericShow
instance decodeHlsIFrameOnlyManifest :: Decode HlsIFrameOnlyManifest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsIFrameOnlyManifest :: Encode HlsIFrameOnlyManifest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The Initialization Vector is a 128-bit number used in conjunction with the key for encrypting blocks. If set to INCLUDE, Initialization Vector is listed in the manifest. Otherwise Initialization Vector is not in the manifest.
newtype HlsInitializationVectorInManifest = HlsInitializationVectorInManifest String
derive instance newtypeHlsInitializationVectorInManifest :: Newtype HlsInitializationVectorInManifest _
derive instance repGenericHlsInitializationVectorInManifest :: Generic HlsInitializationVectorInManifest _
instance showHlsInitializationVectorInManifest :: Show HlsInitializationVectorInManifest where
  show = genericShow
instance decodeHlsInitializationVectorInManifest :: Decode HlsInitializationVectorInManifest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsInitializationVectorInManifest :: Encode HlsInitializationVectorInManifest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates which type of key provider is used for encryption.
newtype HlsKeyProviderType = HlsKeyProviderType String
derive instance newtypeHlsKeyProviderType :: Newtype HlsKeyProviderType _
derive instance repGenericHlsKeyProviderType :: Generic HlsKeyProviderType _
instance showHlsKeyProviderType :: Show HlsKeyProviderType where
  show = genericShow
instance decodeHlsKeyProviderType :: Decode HlsKeyProviderType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsKeyProviderType :: Encode HlsKeyProviderType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to GZIP, compresses HLS playlist.
newtype HlsManifestCompression = HlsManifestCompression String
derive instance newtypeHlsManifestCompression :: Newtype HlsManifestCompression _
derive instance repGenericHlsManifestCompression :: Generic HlsManifestCompression _
instance showHlsManifestCompression :: Show HlsManifestCompression where
  show = genericShow
instance decodeHlsManifestCompression :: Decode HlsManifestCompression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsManifestCompression :: Encode HlsManifestCompression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates whether the output manifest should use floating point values for segment duration.
newtype HlsManifestDurationFormat = HlsManifestDurationFormat String
derive instance newtypeHlsManifestDurationFormat :: Newtype HlsManifestDurationFormat _
derive instance repGenericHlsManifestDurationFormat :: Generic HlsManifestDurationFormat _
instance showHlsManifestDurationFormat :: Show HlsManifestDurationFormat where
  show = genericShow
instance decodeHlsManifestDurationFormat :: Decode HlsManifestDurationFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsManifestDurationFormat :: Encode HlsManifestDurationFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates whether the .m3u8 manifest file should be generated for this HLS output group.
newtype HlsOutputSelection = HlsOutputSelection String
derive instance newtypeHlsOutputSelection :: Newtype HlsOutputSelection _
derive instance repGenericHlsOutputSelection :: Generic HlsOutputSelection _
instance showHlsOutputSelection :: Show HlsOutputSelection where
  show = genericShow
instance decodeHlsOutputSelection :: Decode HlsOutputSelection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsOutputSelection :: Encode HlsOutputSelection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Includes or excludes EXT-X-PROGRAM-DATE-TIME tag in .m3u8 manifest files. The value is calculated as follows: either the program date and time are initialized using the input timecode source, or the time is initialized using the input timecode source and the date is initialized using the timestamp_offset.
newtype HlsProgramDateTime = HlsProgramDateTime String
derive instance newtypeHlsProgramDateTime :: Newtype HlsProgramDateTime _
derive instance repGenericHlsProgramDateTime :: Generic HlsProgramDateTime _
instance showHlsProgramDateTime :: Show HlsProgramDateTime where
  show = genericShow
instance decodeHlsProgramDateTime :: Decode HlsProgramDateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsProgramDateTime :: Encode HlsProgramDateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
newtype HlsSegmentControl = HlsSegmentControl String
derive instance newtypeHlsSegmentControl :: Newtype HlsSegmentControl _
derive instance repGenericHlsSegmentControl :: Generic HlsSegmentControl _
instance showHlsSegmentControl :: Show HlsSegmentControl where
  show = genericShow
instance decodeHlsSegmentControl :: Decode HlsSegmentControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsSegmentControl :: Encode HlsSegmentControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for HLS output groups
newtype HlsSettings = HlsSettings 
  { "AudioGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "AudioRenditionSets" :: NullOrUndefined.NullOrUndefined (String)
  , "AudioTrackType" :: NullOrUndefined.NullOrUndefined (HlsAudioTrackType)
  , "IFrameOnlyManifest" :: NullOrUndefined.NullOrUndefined (HlsIFrameOnlyManifest)
  , "SegmentModifier" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHlsSettings :: Newtype HlsSettings _
derive instance repGenericHlsSettings :: Generic HlsSettings _
instance showHlsSettings :: Show HlsSettings where
  show = genericShow
instance decodeHlsSettings :: Decode HlsSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsSettings :: Encode HlsSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
newtype HlsStreamInfResolution = HlsStreamInfResolution String
derive instance newtypeHlsStreamInfResolution :: Newtype HlsStreamInfResolution _
derive instance repGenericHlsStreamInfResolution :: Generic HlsStreamInfResolution _
instance showHlsStreamInfResolution :: Show HlsStreamInfResolution where
  show = genericShow
instance decodeHlsStreamInfResolution :: Decode HlsStreamInfResolution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsStreamInfResolution :: Encode HlsStreamInfResolution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates ID3 frame that has the timecode.
newtype HlsTimedMetadataId3Frame = HlsTimedMetadataId3Frame String
derive instance newtypeHlsTimedMetadataId3Frame :: Newtype HlsTimedMetadataId3Frame _
derive instance repGenericHlsTimedMetadataId3Frame :: Generic HlsTimedMetadataId3Frame _
instance showHlsTimedMetadataId3Frame :: Show HlsTimedMetadataId3Frame where
  show = genericShow
instance decodeHlsTimedMetadataId3Frame :: Decode HlsTimedMetadataId3Frame where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsTimedMetadataId3Frame :: Encode HlsTimedMetadataId3Frame where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3) to specify the base 64 encoded string and use Timecode (TimeCode) to specify the time when the tag should be inserted. To insert multiple ID3 tags in your output, create mulitple instances of ID3 insertion (Id3Insertion).
newtype Id3Insertion = Id3Insertion 
  { "Id3" :: NullOrUndefined.NullOrUndefined (String)
  , "Timecode" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeId3Insertion :: Newtype Id3Insertion _
derive instance repGenericId3Insertion :: Generic Id3Insertion _
instance showId3Insertion :: Show Id3Insertion where
  show = genericShow
instance decodeId3Insertion :: Decode Id3Insertion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeId3Insertion :: Encode Id3Insertion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable the Image inserter (ImageInserter) feature to include a graphic overlay on your video. Enable or disable this feature for each output individually. This setting is disabled by default.
newtype ImageInserter = ImageInserter 
  { "InsertableImages" :: NullOrUndefined.NullOrUndefined (ListOfInsertableImage)
  }
derive instance newtypeImageInserter :: Newtype ImageInserter _
derive instance repGenericImageInserter :: Generic ImageInserter _
instance showImageInserter :: Show ImageInserter where
  show = genericShow
instance decodeImageInserter :: Decode ImageInserter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImageInserter :: Encode ImageInserter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specifies media input
newtype Input = Input 
  { "AudioSelectorGroups" :: NullOrUndefined.NullOrUndefined (MapOfAudioSelectorGroup)
  , "AudioSelectors" :: NullOrUndefined.NullOrUndefined (MapOfAudioSelector)
  , "CaptionSelectors" :: NullOrUndefined.NullOrUndefined (MapOfCaptionSelector)
  , "DeblockFilter" :: NullOrUndefined.NullOrUndefined (InputDeblockFilter)
  , "DenoiseFilter" :: NullOrUndefined.NullOrUndefined (InputDenoiseFilter)
  , "FileInput" :: NullOrUndefined.NullOrUndefined (String)
  , "FilterEnable" :: NullOrUndefined.NullOrUndefined (InputFilterEnable)
  , "FilterStrength" :: NullOrUndefined.NullOrUndefined (Int)
  , "InputClippings" :: NullOrUndefined.NullOrUndefined (ListOfInputClipping)
  , "ProgramNumber" :: NullOrUndefined.NullOrUndefined (Int)
  , "PsiControl" :: NullOrUndefined.NullOrUndefined (InputPsiControl)
  , "TimecodeSource" :: NullOrUndefined.NullOrUndefined (InputTimecodeSource)
  , "VideoSelector" :: NullOrUndefined.NullOrUndefined (VideoSelector)
  }
derive instance newtypeInput :: Newtype Input _
derive instance repGenericInput :: Generic Input _
instance showInput :: Show Input where
  show = genericShow
instance decodeInput :: Decode Input where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInput :: Encode Input where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Include one instance of (InputClipping) for each input clip.
newtype InputClipping = InputClipping 
  { "EndTimecode" :: NullOrUndefined.NullOrUndefined (String)
  , "StartTimecode" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInputClipping :: Newtype InputClipping _
derive instance repGenericInputClipping :: Generic InputClipping _
instance showInputClipping :: Show InputClipping where
  show = genericShow
instance decodeInputClipping :: Decode InputClipping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputClipping :: Encode InputClipping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manaully controllable for MPEG2 and uncompressed video inputs.
newtype InputDeblockFilter = InputDeblockFilter String
derive instance newtypeInputDeblockFilter :: Newtype InputDeblockFilter _
derive instance repGenericInputDeblockFilter :: Generic InputDeblockFilter _
instance showInputDeblockFilter :: Show InputDeblockFilter where
  show = genericShow
instance decodeInputDeblockFilter :: Decode InputDeblockFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputDeblockFilter :: Encode InputDeblockFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
newtype InputDenoiseFilter = InputDenoiseFilter String
derive instance newtypeInputDenoiseFilter :: Newtype InputDenoiseFilter _
derive instance repGenericInputDenoiseFilter :: Generic InputDenoiseFilter _
instance showInputDenoiseFilter :: Show InputDenoiseFilter where
  show = genericShow
instance decodeInputDenoiseFilter :: Decode InputDenoiseFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputDenoiseFilter :: Encode InputDenoiseFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Filter enable (InputFilterEnable) to specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The in put is filtered regardless of input type.
newtype InputFilterEnable = InputFilterEnable String
derive instance newtypeInputFilterEnable :: Newtype InputFilterEnable _
derive instance repGenericInputFilterEnable :: Generic InputFilterEnable _
instance showInputFilterEnable :: Show InputFilterEnable where
  show = genericShow
instance decodeInputFilterEnable :: Decode InputFilterEnable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputFilterEnable :: Encode InputFilterEnable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
newtype InputPsiControl = InputPsiControl String
derive instance newtypeInputPsiControl :: Newtype InputPsiControl _
derive instance repGenericInputPsiControl :: Generic InputPsiControl _
instance showInputPsiControl :: Show InputPsiControl where
  show = genericShow
instance decodeInputPsiControl :: Decode InputPsiControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputPsiControl :: Encode InputPsiControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specified video input in a template.
newtype InputTemplate = InputTemplate 
  { "AudioSelectorGroups" :: NullOrUndefined.NullOrUndefined (MapOfAudioSelectorGroup)
  , "AudioSelectors" :: NullOrUndefined.NullOrUndefined (MapOfAudioSelector)
  , "CaptionSelectors" :: NullOrUndefined.NullOrUndefined (MapOfCaptionSelector)
  , "DeblockFilter" :: NullOrUndefined.NullOrUndefined (InputDeblockFilter)
  , "DenoiseFilter" :: NullOrUndefined.NullOrUndefined (InputDenoiseFilter)
  , "FilterEnable" :: NullOrUndefined.NullOrUndefined (InputFilterEnable)
  , "FilterStrength" :: NullOrUndefined.NullOrUndefined (Int)
  , "InputClippings" :: NullOrUndefined.NullOrUndefined (ListOfInputClipping)
  , "ProgramNumber" :: NullOrUndefined.NullOrUndefined (Int)
  , "PsiControl" :: NullOrUndefined.NullOrUndefined (InputPsiControl)
  , "TimecodeSource" :: NullOrUndefined.NullOrUndefined (InputTimecodeSource)
  , "VideoSelector" :: NullOrUndefined.NullOrUndefined (VideoSelector)
  }
derive instance newtypeInputTemplate :: Newtype InputTemplate _
derive instance repGenericInputTemplate :: Generic InputTemplate _
instance showInputTemplate :: Show InputTemplate where
  show = genericShow
instance decodeInputTemplate :: Decode InputTemplate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputTemplate :: Encode InputTemplate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Timecode source (InputTimecodeSource) to specify how timecode information from your input is adjusted and encoded in all outputs for the job. Default is embedded. Set to Embedded (EMBEDDED) to use the timecode that is in the input video. If no embedded timecode is in the source, will set the timecode for the first frame to 00:00:00:00. Set to Start at 0 (ZEROBASED) to set the timecode of the initial frame to 00:00:00:00. Set to Specified start (SPECIFIEDSTART) to provide the initial timecode yourself the setting (Start).
newtype InputTimecodeSource = InputTimecodeSource String
derive instance newtypeInputTimecodeSource :: Newtype InputTimecodeSource _
derive instance repGenericInputTimecodeSource :: Generic InputTimecodeSource _
instance showInputTimecodeSource :: Show InputTimecodeSource where
  show = genericShow
instance decodeInputTimecodeSource :: Decode InputTimecodeSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputTimecodeSource :: Encode InputTimecodeSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for Insertable Image
newtype InsertableImage = InsertableImage 
  { "Duration" :: NullOrUndefined.NullOrUndefined (Int)
  , "FadeIn" :: NullOrUndefined.NullOrUndefined (Int)
  , "FadeOut" :: NullOrUndefined.NullOrUndefined (Int)
  , "Height" :: NullOrUndefined.NullOrUndefined (Int)
  , "ImageInserterInput" :: NullOrUndefined.NullOrUndefined (String)
  , "ImageX" :: NullOrUndefined.NullOrUndefined (Int)
  , "ImageY" :: NullOrUndefined.NullOrUndefined (Int)
  , "Layer" :: NullOrUndefined.NullOrUndefined (Int)
  , "Opacity" :: NullOrUndefined.NullOrUndefined (Int)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (String)
  , "Width" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeInsertableImage :: Newtype InsertableImage _
derive instance repGenericInsertableImage :: Generic InsertableImage _
instance showInsertableImage :: Show InsertableImage where
  show = genericShow
instance decodeInsertableImage :: Decode InsertableImage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsertableImage :: Encode InsertableImage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The service encountered an unexpected condition and cannot fulfill your request.
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


-- | Each job converts an input file into an output file or files. For more information, see the User Guide at http://docs.aws.amazon.com/mediaconvert/latest/ug/what-is.html
newtype Job = Job 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (Number)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (Int)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "JobTemplate" :: NullOrUndefined.NullOrUndefined (String)
  , "OutputGroupDetails" :: NullOrUndefined.NullOrUndefined (ListOfOutputGroupDetail)
  , "Queue" :: NullOrUndefined.NullOrUndefined (String)
  , "Role" :: NullOrUndefined.NullOrUndefined (String)
  , "Settings" :: NullOrUndefined.NullOrUndefined (JobSettings)
  , "Status" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "Timing" :: NullOrUndefined.NullOrUndefined (Timing)
  , "UserMetadata" :: NullOrUndefined.NullOrUndefined (MapOf__string)
  }
derive instance newtypeJob :: Newtype Job _
derive instance repGenericJob :: Generic Job _
instance showJob :: Show Job where
  show = genericShow
instance decodeJob :: Decode Job where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJob :: Encode Job where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | JobSettings contains all the transcode settings for a job.
newtype JobSettings = JobSettings 
  { "AdAvailOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "AvailBlanking" :: NullOrUndefined.NullOrUndefined (AvailBlanking)
  , "Inputs" :: NullOrUndefined.NullOrUndefined (ListOfInput)
  , "NielsenConfiguration" :: NullOrUndefined.NullOrUndefined (NielsenConfiguration)
  , "OutputGroups" :: NullOrUndefined.NullOrUndefined (ListOfOutputGroup)
  , "TimecodeConfig" :: NullOrUndefined.NullOrUndefined (TimecodeConfig)
  , "TimedMetadataInsertion" :: NullOrUndefined.NullOrUndefined (TimedMetadataInsertion)
  }
derive instance newtypeJobSettings :: Newtype JobSettings _
derive instance repGenericJobSettings :: Generic JobSettings _
instance showJobSettings :: Show JobSettings where
  show = genericShow
instance decodeJobSettings :: Decode JobSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobSettings :: Encode JobSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _
derive instance repGenericJobStatus :: Generic JobStatus _
instance showJobStatus :: Show JobStatus where
  show = genericShow
instance decodeJobStatus :: Decode JobStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobStatus :: Encode JobStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A job template is a pre-made set of encoding instructions that you can use to quickly create a job.
newtype JobTemplate = JobTemplate 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Category" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (Number)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdated" :: NullOrUndefined.NullOrUndefined (Number)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Queue" :: NullOrUndefined.NullOrUndefined (String)
  , "Settings" :: NullOrUndefined.NullOrUndefined (JobTemplateSettings)
  , "Type" :: NullOrUndefined.NullOrUndefined (Type)
  }
derive instance newtypeJobTemplate :: Newtype JobTemplate _
derive instance repGenericJobTemplate :: Generic JobTemplate _
instance showJobTemplate :: Show JobTemplate where
  show = genericShow
instance decodeJobTemplate :: Decode JobTemplate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobTemplate :: Encode JobTemplate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
newtype JobTemplateListBy = JobTemplateListBy String
derive instance newtypeJobTemplateListBy :: Newtype JobTemplateListBy _
derive instance repGenericJobTemplateListBy :: Generic JobTemplateListBy _
instance showJobTemplateListBy :: Show JobTemplateListBy where
  show = genericShow
instance decodeJobTemplateListBy :: Decode JobTemplateListBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobTemplateListBy :: Encode JobTemplateListBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | JobTemplateSettings contains all the transcode settings saved in the template that will be applied to jobs created from it.
newtype JobTemplateSettings = JobTemplateSettings 
  { "AdAvailOffset" :: NullOrUndefined.NullOrUndefined (Int)
  , "AvailBlanking" :: NullOrUndefined.NullOrUndefined (AvailBlanking)
  , "Inputs" :: NullOrUndefined.NullOrUndefined (ListOfInputTemplate)
  , "NielsenConfiguration" :: NullOrUndefined.NullOrUndefined (NielsenConfiguration)
  , "OutputGroups" :: NullOrUndefined.NullOrUndefined (ListOfOutputGroup)
  , "TimecodeConfig" :: NullOrUndefined.NullOrUndefined (TimecodeConfig)
  , "TimedMetadataInsertion" :: NullOrUndefined.NullOrUndefined (TimedMetadataInsertion)
  }
derive instance newtypeJobTemplateSettings :: Newtype JobTemplateSettings _
derive instance repGenericJobTemplateSettings :: Generic JobTemplateSettings _
instance showJobTemplateSettings :: Show JobTemplateSettings where
  show = genericShow
instance decodeJobTemplateSettings :: Decode JobTemplateSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobTemplateSettings :: Encode JobTemplateSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Code to specify the language, following the specification "ISO 639-2 three-digit code":http://www.loc.gov/standards/iso639-2/
newtype LanguageCode = LanguageCode String
derive instance newtypeLanguageCode :: Newtype LanguageCode _
derive instance repGenericLanguageCode :: Generic LanguageCode _
instance showLanguageCode :: Show LanguageCode where
  show = genericShow
instance decodeLanguageCode :: Decode LanguageCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLanguageCode :: Encode LanguageCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobTemplatesRequest = ListJobTemplatesRequest 
  { "Category" :: NullOrUndefined.NullOrUndefined (String)
  , "ListBy" :: NullOrUndefined.NullOrUndefined (JobTemplateListBy)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Order" :: NullOrUndefined.NullOrUndefined (Order)
  }
derive instance newtypeListJobTemplatesRequest :: Newtype ListJobTemplatesRequest _
derive instance repGenericListJobTemplatesRequest :: Generic ListJobTemplatesRequest _
instance showListJobTemplatesRequest :: Show ListJobTemplatesRequest where
  show = genericShow
instance decodeListJobTemplatesRequest :: Decode ListJobTemplatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobTemplatesRequest :: Encode ListJobTemplatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobTemplatesResponse = ListJobTemplatesResponse 
  { "JobTemplates" :: NullOrUndefined.NullOrUndefined (ListOfJobTemplate)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListJobTemplatesResponse :: Newtype ListJobTemplatesResponse _
derive instance repGenericListJobTemplatesResponse :: Generic ListJobTemplatesResponse _
instance showListJobTemplatesResponse :: Show ListJobTemplatesResponse where
  show = genericShow
instance decodeListJobTemplatesResponse :: Decode ListJobTemplatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobTemplatesResponse :: Encode ListJobTemplatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobsRequest = ListJobsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Order" :: NullOrUndefined.NullOrUndefined (Order)
  , "Queue" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (JobStatus)
  }
derive instance newtypeListJobsRequest :: Newtype ListJobsRequest _
derive instance repGenericListJobsRequest :: Generic ListJobsRequest _
instance showListJobsRequest :: Show ListJobsRequest where
  show = genericShow
instance decodeListJobsRequest :: Decode ListJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsRequest :: Encode ListJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobsResponse = ListJobsResponse 
  { "Jobs" :: NullOrUndefined.NullOrUndefined (ListOfJob)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListJobsResponse :: Newtype ListJobsResponse _
derive instance repGenericListJobsResponse :: Generic ListJobsResponse _
instance showListJobsResponse :: Show ListJobsResponse where
  show = genericShow
instance decodeListJobsResponse :: Decode ListJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsResponse :: Encode ListJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfAudioDescription = ListOfAudioDescription (Array AudioDescription)
derive instance newtypeListOfAudioDescription :: Newtype ListOfAudioDescription _
derive instance repGenericListOfAudioDescription :: Generic ListOfAudioDescription _
instance showListOfAudioDescription :: Show ListOfAudioDescription where
  show = genericShow
instance decodeListOfAudioDescription :: Decode ListOfAudioDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfAudioDescription :: Encode ListOfAudioDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfCaptionDescription = ListOfCaptionDescription (Array CaptionDescription)
derive instance newtypeListOfCaptionDescription :: Newtype ListOfCaptionDescription _
derive instance repGenericListOfCaptionDescription :: Generic ListOfCaptionDescription _
instance showListOfCaptionDescription :: Show ListOfCaptionDescription where
  show = genericShow
instance decodeListOfCaptionDescription :: Decode ListOfCaptionDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfCaptionDescription :: Encode ListOfCaptionDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfCaptionDescriptionPreset = ListOfCaptionDescriptionPreset (Array CaptionDescriptionPreset)
derive instance newtypeListOfCaptionDescriptionPreset :: Newtype ListOfCaptionDescriptionPreset _
derive instance repGenericListOfCaptionDescriptionPreset :: Generic ListOfCaptionDescriptionPreset _
instance showListOfCaptionDescriptionPreset :: Show ListOfCaptionDescriptionPreset where
  show = genericShow
instance decodeListOfCaptionDescriptionPreset :: Decode ListOfCaptionDescriptionPreset where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfCaptionDescriptionPreset :: Encode ListOfCaptionDescriptionPreset where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfEndpoint = ListOfEndpoint (Array Endpoint)
derive instance newtypeListOfEndpoint :: Newtype ListOfEndpoint _
derive instance repGenericListOfEndpoint :: Generic ListOfEndpoint _
instance showListOfEndpoint :: Show ListOfEndpoint where
  show = genericShow
instance decodeListOfEndpoint :: Decode ListOfEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfEndpoint :: Encode ListOfEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfHlsAdMarkers = ListOfHlsAdMarkers (Array HlsAdMarkers)
derive instance newtypeListOfHlsAdMarkers :: Newtype ListOfHlsAdMarkers _
derive instance repGenericListOfHlsAdMarkers :: Generic ListOfHlsAdMarkers _
instance showListOfHlsAdMarkers :: Show ListOfHlsAdMarkers where
  show = genericShow
instance decodeListOfHlsAdMarkers :: Decode ListOfHlsAdMarkers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfHlsAdMarkers :: Encode ListOfHlsAdMarkers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfHlsCaptionLanguageMapping = ListOfHlsCaptionLanguageMapping (Array HlsCaptionLanguageMapping)
derive instance newtypeListOfHlsCaptionLanguageMapping :: Newtype ListOfHlsCaptionLanguageMapping _
derive instance repGenericListOfHlsCaptionLanguageMapping :: Generic ListOfHlsCaptionLanguageMapping _
instance showListOfHlsCaptionLanguageMapping :: Show ListOfHlsCaptionLanguageMapping where
  show = genericShow
instance decodeListOfHlsCaptionLanguageMapping :: Decode ListOfHlsCaptionLanguageMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfHlsCaptionLanguageMapping :: Encode ListOfHlsCaptionLanguageMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfId3Insertion = ListOfId3Insertion (Array Id3Insertion)
derive instance newtypeListOfId3Insertion :: Newtype ListOfId3Insertion _
derive instance repGenericListOfId3Insertion :: Generic ListOfId3Insertion _
instance showListOfId3Insertion :: Show ListOfId3Insertion where
  show = genericShow
instance decodeListOfId3Insertion :: Decode ListOfId3Insertion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfId3Insertion :: Encode ListOfId3Insertion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfInput = ListOfInput (Array Input)
derive instance newtypeListOfInput :: Newtype ListOfInput _
derive instance repGenericListOfInput :: Generic ListOfInput _
instance showListOfInput :: Show ListOfInput where
  show = genericShow
instance decodeListOfInput :: Decode ListOfInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInput :: Encode ListOfInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfInputClipping = ListOfInputClipping (Array InputClipping)
derive instance newtypeListOfInputClipping :: Newtype ListOfInputClipping _
derive instance repGenericListOfInputClipping :: Generic ListOfInputClipping _
instance showListOfInputClipping :: Show ListOfInputClipping where
  show = genericShow
instance decodeListOfInputClipping :: Decode ListOfInputClipping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputClipping :: Encode ListOfInputClipping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfInputTemplate = ListOfInputTemplate (Array InputTemplate)
derive instance newtypeListOfInputTemplate :: Newtype ListOfInputTemplate _
derive instance repGenericListOfInputTemplate :: Generic ListOfInputTemplate _
instance showListOfInputTemplate :: Show ListOfInputTemplate where
  show = genericShow
instance decodeListOfInputTemplate :: Decode ListOfInputTemplate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInputTemplate :: Encode ListOfInputTemplate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfInsertableImage = ListOfInsertableImage (Array InsertableImage)
derive instance newtypeListOfInsertableImage :: Newtype ListOfInsertableImage _
derive instance repGenericListOfInsertableImage :: Generic ListOfInsertableImage _
instance showListOfInsertableImage :: Show ListOfInsertableImage where
  show = genericShow
instance decodeListOfInsertableImage :: Decode ListOfInsertableImage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfInsertableImage :: Encode ListOfInsertableImage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfJob = ListOfJob (Array Job)
derive instance newtypeListOfJob :: Newtype ListOfJob _
derive instance repGenericListOfJob :: Generic ListOfJob _
instance showListOfJob :: Show ListOfJob where
  show = genericShow
instance decodeListOfJob :: Decode ListOfJob where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfJob :: Encode ListOfJob where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfJobTemplate = ListOfJobTemplate (Array JobTemplate)
derive instance newtypeListOfJobTemplate :: Newtype ListOfJobTemplate _
derive instance repGenericListOfJobTemplate :: Generic ListOfJobTemplate _
instance showListOfJobTemplate :: Show ListOfJobTemplate where
  show = genericShow
instance decodeListOfJobTemplate :: Decode ListOfJobTemplate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfJobTemplate :: Encode ListOfJobTemplate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfOutput = ListOfOutput (Array Output)
derive instance newtypeListOfOutput :: Newtype ListOfOutput _
derive instance repGenericListOfOutput :: Generic ListOfOutput _
instance showListOfOutput :: Show ListOfOutput where
  show = genericShow
instance decodeListOfOutput :: Decode ListOfOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOutput :: Encode ListOfOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfOutputChannelMapping = ListOfOutputChannelMapping (Array OutputChannelMapping)
derive instance newtypeListOfOutputChannelMapping :: Newtype ListOfOutputChannelMapping _
derive instance repGenericListOfOutputChannelMapping :: Generic ListOfOutputChannelMapping _
instance showListOfOutputChannelMapping :: Show ListOfOutputChannelMapping where
  show = genericShow
instance decodeListOfOutputChannelMapping :: Decode ListOfOutputChannelMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOutputChannelMapping :: Encode ListOfOutputChannelMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfOutputDetail = ListOfOutputDetail (Array OutputDetail)
derive instance newtypeListOfOutputDetail :: Newtype ListOfOutputDetail _
derive instance repGenericListOfOutputDetail :: Generic ListOfOutputDetail _
instance showListOfOutputDetail :: Show ListOfOutputDetail where
  show = genericShow
instance decodeListOfOutputDetail :: Decode ListOfOutputDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOutputDetail :: Encode ListOfOutputDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfOutputGroup = ListOfOutputGroup (Array OutputGroup)
derive instance newtypeListOfOutputGroup :: Newtype ListOfOutputGroup _
derive instance repGenericListOfOutputGroup :: Generic ListOfOutputGroup _
instance showListOfOutputGroup :: Show ListOfOutputGroup where
  show = genericShow
instance decodeListOfOutputGroup :: Decode ListOfOutputGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOutputGroup :: Encode ListOfOutputGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfOutputGroupDetail = ListOfOutputGroupDetail (Array OutputGroupDetail)
derive instance newtypeListOfOutputGroupDetail :: Newtype ListOfOutputGroupDetail _
derive instance repGenericListOfOutputGroupDetail :: Generic ListOfOutputGroupDetail _
instance showListOfOutputGroupDetail :: Show ListOfOutputGroupDetail where
  show = genericShow
instance decodeListOfOutputGroupDetail :: Decode ListOfOutputGroupDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfOutputGroupDetail :: Encode ListOfOutputGroupDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfPreset = ListOfPreset (Array Preset)
derive instance newtypeListOfPreset :: Newtype ListOfPreset _
derive instance repGenericListOfPreset :: Generic ListOfPreset _
instance showListOfPreset :: Show ListOfPreset where
  show = genericShow
instance decodeListOfPreset :: Decode ListOfPreset where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfPreset :: Encode ListOfPreset where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfQueue = ListOfQueue (Array Queue)
derive instance newtypeListOfQueue :: Newtype ListOfQueue _
derive instance repGenericListOfQueue :: Generic ListOfQueue _
instance showListOfQueue :: Show ListOfQueue where
  show = genericShow
instance decodeListOfQueue :: Decode ListOfQueue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfQueue :: Encode ListOfQueue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOf__integer = ListOf__integer (Array Int)
derive instance newtypeListOf__integer :: Newtype ListOf__integer _
derive instance repGenericListOf__integer :: Generic ListOf__integer _
instance showListOf__integer :: Show ListOf__integer where
  show = genericShow
instance decodeListOf__integer :: Decode ListOf__integer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOf__integer :: Encode ListOf__integer where
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


newtype ListPresetsRequest = ListPresetsRequest 
  { "Category" :: NullOrUndefined.NullOrUndefined (String)
  , "ListBy" :: NullOrUndefined.NullOrUndefined (PresetListBy)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Order" :: NullOrUndefined.NullOrUndefined (Order)
  }
derive instance newtypeListPresetsRequest :: Newtype ListPresetsRequest _
derive instance repGenericListPresetsRequest :: Generic ListPresetsRequest _
instance showListPresetsRequest :: Show ListPresetsRequest where
  show = genericShow
instance decodeListPresetsRequest :: Decode ListPresetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPresetsRequest :: Encode ListPresetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPresetsResponse = ListPresetsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Presets" :: NullOrUndefined.NullOrUndefined (ListOfPreset)
  }
derive instance newtypeListPresetsResponse :: Newtype ListPresetsResponse _
derive instance repGenericListPresetsResponse :: Generic ListPresetsResponse _
instance showListPresetsResponse :: Show ListPresetsResponse where
  show = genericShow
instance decodeListPresetsResponse :: Decode ListPresetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPresetsResponse :: Encode ListPresetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListQueuesRequest = ListQueuesRequest 
  { "ListBy" :: NullOrUndefined.NullOrUndefined (QueueListBy)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Order" :: NullOrUndefined.NullOrUndefined (Order)
  }
derive instance newtypeListQueuesRequest :: Newtype ListQueuesRequest _
derive instance repGenericListQueuesRequest :: Generic ListQueuesRequest _
instance showListQueuesRequest :: Show ListQueuesRequest where
  show = genericShow
instance decodeListQueuesRequest :: Decode ListQueuesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListQueuesRequest :: Encode ListQueuesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListQueuesResponse = ListQueuesResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Queues" :: NullOrUndefined.NullOrUndefined (ListOfQueue)
  }
derive instance newtypeListQueuesResponse :: Newtype ListQueuesResponse _
derive instance repGenericListQueuesResponse :: Generic ListQueuesResponse _
instance showListQueuesResponse :: Show ListQueuesResponse where
  show = genericShow
instance decodeListQueuesResponse :: Decode ListQueuesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListQueuesResponse :: Encode ListQueuesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Selects between the DVB and ATSC buffer models for Dolby Digital audio.
newtype M2tsAudioBufferModel = M2tsAudioBufferModel String
derive instance newtypeM2tsAudioBufferModel :: Newtype M2tsAudioBufferModel _
derive instance repGenericM2tsAudioBufferModel :: Generic M2tsAudioBufferModel _
instance showM2tsAudioBufferModel :: Show M2tsAudioBufferModel where
  show = genericShow
instance decodeM2tsAudioBufferModel :: Decode M2tsAudioBufferModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsAudioBufferModel :: Encode M2tsAudioBufferModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Controls what buffer model to use for accurate interleaving. If set to MULTIPLEX, use multiplex  buffer model. If set to NONE, this can lead to lower latency, but low-memory devices may not be able to play back the stream without interruptions.
newtype M2tsBufferModel = M2tsBufferModel String
derive instance newtypeM2tsBufferModel :: Newtype M2tsBufferModel _
derive instance repGenericM2tsBufferModel :: Generic M2tsBufferModel _
instance showM2tsBufferModel :: Show M2tsBufferModel where
  show = genericShow
instance decodeM2tsBufferModel :: Decode M2tsBufferModel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsBufferModel :: Encode M2tsBufferModel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to VIDEO_AND_FIXED_INTERVALS, audio EBP markers will be added to partitions 3 and 4. The interval between these additional markers will be fixed, and will be slightly shorter than the video EBP marker interval. When set to VIDEO_INTERVAL, these additional markers will not be inserted. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
newtype M2tsEbpAudioInterval = M2tsEbpAudioInterval String
derive instance newtypeM2tsEbpAudioInterval :: Newtype M2tsEbpAudioInterval _
derive instance repGenericM2tsEbpAudioInterval :: Generic M2tsEbpAudioInterval _
instance showM2tsEbpAudioInterval :: Show M2tsEbpAudioInterval where
  show = genericShow
instance decodeM2tsEbpAudioInterval :: Decode M2tsEbpAudioInterval where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsEbpAudioInterval :: Encode M2tsEbpAudioInterval where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Selects which PIDs to place EBP markers on. They can either be placed only on the video PID, or on both the video PID and all audio PIDs. Only applicable when EBP segmentation markers are is selected (segmentationMarkers is EBP or EBP_LEGACY).
newtype M2tsEbpPlacement = M2tsEbpPlacement String
derive instance newtypeM2tsEbpPlacement :: Newtype M2tsEbpPlacement _
derive instance repGenericM2tsEbpPlacement :: Generic M2tsEbpPlacement _
instance showM2tsEbpPlacement :: Show M2tsEbpPlacement where
  show = genericShow
instance decodeM2tsEbpPlacement :: Decode M2tsEbpPlacement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsEbpPlacement :: Encode M2tsEbpPlacement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Controls whether to include the ES Rate field in the PES header.
newtype M2tsEsRateInPes = M2tsEsRateInPes String
derive instance newtypeM2tsEsRateInPes :: Newtype M2tsEsRateInPes _
derive instance repGenericM2tsEsRateInPes :: Generic M2tsEsRateInPes _
instance showM2tsEsRateInPes :: Show M2tsEsRateInPes where
  show = genericShow
instance decodeM2tsEsRateInPes :: Decode M2tsEsRateInPes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsEsRateInPes :: Encode M2tsEsRateInPes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
newtype M2tsNielsenId3 = M2tsNielsenId3 String
derive instance newtypeM2tsNielsenId3 :: Newtype M2tsNielsenId3 _
derive instance repGenericM2tsNielsenId3 :: Generic M2tsNielsenId3 _
instance showM2tsNielsenId3 :: Show M2tsNielsenId3 where
  show = genericShow
instance decodeM2tsNielsenId3 :: Decode M2tsNielsenId3 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsNielsenId3 :: Encode M2tsNielsenId3 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to PCR_EVERY_PES_PACKET, a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This is effective only when the PCR PID is the same as the video or audio elementary stream.
newtype M2tsPcrControl = M2tsPcrControl String
derive instance newtypeM2tsPcrControl :: Newtype M2tsPcrControl _
derive instance repGenericM2tsPcrControl :: Generic M2tsPcrControl _
instance showM2tsPcrControl :: Show M2tsPcrControl where
  show = genericShow
instance decodeM2tsPcrControl :: Decode M2tsPcrControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsPcrControl :: Encode M2tsPcrControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to CBR, inserts null packets into transport stream to fill specified bitrate. When set to VBR, the bitrate setting acts as the maximum bitrate, but the output will not be padded up to that bitrate.
newtype M2tsRateMode = M2tsRateMode String
derive instance newtypeM2tsRateMode :: Newtype M2tsRateMode _
derive instance repGenericM2tsRateMode :: Generic M2tsRateMode _
instance showM2tsRateMode :: Show M2tsRateMode where
  show = genericShow
instance decodeM2tsRateMode :: Decode M2tsRateMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsRateMode :: Encode M2tsRateMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables SCTE-35 passthrough (scte35Source) to pass any SCTE-35 signals from input to output.
newtype M2tsScte35Source = M2tsScte35Source String
derive instance newtypeM2tsScte35Source :: Newtype M2tsScte35Source _
derive instance repGenericM2tsScte35Source :: Generic M2tsScte35Source _
instance showM2tsScte35Source :: Show M2tsScte35Source where
  show = genericShow
instance decodeM2tsScte35Source :: Decode M2tsScte35Source where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsScte35Source :: Encode M2tsScte35Source where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Inserts segmentation markers at each segmentation_time period. rai_segstart sets the Random Access Indicator bit in the adaptation field. rai_adapt sets the RAI bit and adds the current timecode in the private data bytes. psi_segstart inserts PAT and PMT tables at the start of segments. ebp adds Encoder Boundary Point information to the adaptation field as per OpenCable specification OC-SP-EBP-I01-130118. ebp_legacy adds Encoder Boundary Point information to the adaptation field using a legacy proprietary format.
newtype M2tsSegmentationMarkers = M2tsSegmentationMarkers String
derive instance newtypeM2tsSegmentationMarkers :: Newtype M2tsSegmentationMarkers _
derive instance repGenericM2tsSegmentationMarkers :: Generic M2tsSegmentationMarkers _
instance showM2tsSegmentationMarkers :: Show M2tsSegmentationMarkers where
  show = genericShow
instance decodeM2tsSegmentationMarkers :: Decode M2tsSegmentationMarkers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsSegmentationMarkers :: Encode M2tsSegmentationMarkers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The segmentation style parameter controls how segmentation markers are inserted into the transport stream. With avails, it is possible that segments may be truncated, which can influence where future segmentation markers are inserted. When a segmentation style of "reset_cadence" is selected and a segment is truncated due to an avail, we will reset the segmentation cadence. This means the subsequent segment will have a duration of of $segmentation_time seconds. When a segmentation style of "maintain_cadence" is selected and a segment is truncated due to an avail, we will not reset the segmentation cadence. This means the subsequent segment will likely be truncated as well. However, all segments after that will have a duration of $segmentation_time seconds. Note that EBP lookahead is a slight exception to this rule.
newtype M2tsSegmentationStyle = M2tsSegmentationStyle String
derive instance newtypeM2tsSegmentationStyle :: Newtype M2tsSegmentationStyle _
derive instance repGenericM2tsSegmentationStyle :: Generic M2tsSegmentationStyle _
instance showM2tsSegmentationStyle :: Show M2tsSegmentationStyle where
  show = genericShow
instance decodeM2tsSegmentationStyle :: Decode M2tsSegmentationStyle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsSegmentationStyle :: Encode M2tsSegmentationStyle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for M2TS Container.
newtype M2tsSettings = M2tsSettings 
  { "AudioBufferModel" :: NullOrUndefined.NullOrUndefined (M2tsAudioBufferModel)
  , "AudioFramesPerPes" :: NullOrUndefined.NullOrUndefined (Int)
  , "AudioPids" :: NullOrUndefined.NullOrUndefined (ListOf__integer)
  , "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "BufferModel" :: NullOrUndefined.NullOrUndefined (M2tsBufferModel)
  , "DvbNitSettings" :: NullOrUndefined.NullOrUndefined (DvbNitSettings)
  , "DvbSdtSettings" :: NullOrUndefined.NullOrUndefined (DvbSdtSettings)
  , "DvbSubPids" :: NullOrUndefined.NullOrUndefined (ListOf__integer)
  , "DvbTdtSettings" :: NullOrUndefined.NullOrUndefined (DvbTdtSettings)
  , "DvbTeletextPid" :: NullOrUndefined.NullOrUndefined (Int)
  , "EbpAudioInterval" :: NullOrUndefined.NullOrUndefined (M2tsEbpAudioInterval)
  , "EbpPlacement" :: NullOrUndefined.NullOrUndefined (M2tsEbpPlacement)
  , "EsRateInPes" :: NullOrUndefined.NullOrUndefined (M2tsEsRateInPes)
  , "FragmentTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "MaxPcrInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinEbpInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "NielsenId3" :: NullOrUndefined.NullOrUndefined (M2tsNielsenId3)
  , "NullPacketBitrate" :: NullOrUndefined.NullOrUndefined (Number)
  , "PatInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "PcrControl" :: NullOrUndefined.NullOrUndefined (M2tsPcrControl)
  , "PcrPid" :: NullOrUndefined.NullOrUndefined (Int)
  , "PmtInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "PmtPid" :: NullOrUndefined.NullOrUndefined (Int)
  , "PrivateMetadataPid" :: NullOrUndefined.NullOrUndefined (Int)
  , "ProgramNumber" :: NullOrUndefined.NullOrUndefined (Int)
  , "RateMode" :: NullOrUndefined.NullOrUndefined (M2tsRateMode)
  , "Scte35Pid" :: NullOrUndefined.NullOrUndefined (Int)
  , "Scte35Source" :: NullOrUndefined.NullOrUndefined (M2tsScte35Source)
  , "SegmentationMarkers" :: NullOrUndefined.NullOrUndefined (M2tsSegmentationMarkers)
  , "SegmentationStyle" :: NullOrUndefined.NullOrUndefined (M2tsSegmentationStyle)
  , "SegmentationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "TimedMetadataPid" :: NullOrUndefined.NullOrUndefined (Int)
  , "TransportStreamId" :: NullOrUndefined.NullOrUndefined (Int)
  , "VideoPid" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeM2tsSettings :: Newtype M2tsSettings _
derive instance repGenericM2tsSettings :: Generic M2tsSettings _
instance showM2tsSettings :: Show M2tsSettings where
  show = genericShow
instance decodeM2tsSettings :: Decode M2tsSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM2tsSettings :: Encode M2tsSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If INSERT, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
newtype M3u8NielsenId3 = M3u8NielsenId3 String
derive instance newtypeM3u8NielsenId3 :: Newtype M3u8NielsenId3 _
derive instance repGenericM3u8NielsenId3 :: Generic M3u8NielsenId3 _
instance showM3u8NielsenId3 :: Show M3u8NielsenId3 where
  show = genericShow
instance decodeM3u8NielsenId3 :: Decode M3u8NielsenId3 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM3u8NielsenId3 :: Encode M3u8NielsenId3 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to PCR_EVERY_PES_PACKET a Program Clock Reference value is inserted for every Packetized Elementary Stream (PES) header. This parameter is effective only when the PCR PID is the same as the video or audio elementary stream.
newtype M3u8PcrControl = M3u8PcrControl String
derive instance newtypeM3u8PcrControl :: Newtype M3u8PcrControl _
derive instance repGenericM3u8PcrControl :: Generic M3u8PcrControl _
instance showM3u8PcrControl :: Show M3u8PcrControl where
  show = genericShow
instance decodeM3u8PcrControl :: Decode M3u8PcrControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM3u8PcrControl :: Encode M3u8PcrControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables SCTE-35 passthrough (scte35Source) to pass any SCTE-35 signals from input to output.
newtype M3u8Scte35Source = M3u8Scte35Source String
derive instance newtypeM3u8Scte35Source :: Newtype M3u8Scte35Source _
derive instance repGenericM3u8Scte35Source :: Generic M3u8Scte35Source _
instance showM3u8Scte35Source :: Show M3u8Scte35Source where
  show = genericShow
instance decodeM3u8Scte35Source :: Decode M3u8Scte35Source where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM3u8Scte35Source :: Encode M3u8Scte35Source where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for TS segments in HLS
newtype M3u8Settings = M3u8Settings 
  { "AudioFramesPerPes" :: NullOrUndefined.NullOrUndefined (Int)
  , "AudioPids" :: NullOrUndefined.NullOrUndefined (ListOf__integer)
  , "NielsenId3" :: NullOrUndefined.NullOrUndefined (M3u8NielsenId3)
  , "PatInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "PcrControl" :: NullOrUndefined.NullOrUndefined (M3u8PcrControl)
  , "PcrPid" :: NullOrUndefined.NullOrUndefined (Int)
  , "PmtInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "PmtPid" :: NullOrUndefined.NullOrUndefined (Int)
  , "PrivateMetadataPid" :: NullOrUndefined.NullOrUndefined (Int)
  , "ProgramNumber" :: NullOrUndefined.NullOrUndefined (Int)
  , "Scte35Pid" :: NullOrUndefined.NullOrUndefined (Int)
  , "Scte35Source" :: NullOrUndefined.NullOrUndefined (M3u8Scte35Source)
  , "TimedMetadata" :: NullOrUndefined.NullOrUndefined (TimedMetadata)
  , "TimedMetadataPid" :: NullOrUndefined.NullOrUndefined (Int)
  , "TransportStreamId" :: NullOrUndefined.NullOrUndefined (Int)
  , "VideoPid" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeM3u8Settings :: Newtype M3u8Settings _
derive instance repGenericM3u8Settings :: Generic M3u8Settings _
instance showM3u8Settings :: Show M3u8Settings where
  show = genericShow
instance decodeM3u8Settings :: Decode M3u8Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeM3u8Settings :: Encode M3u8Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfAudioSelector = MapOfAudioSelector (StrMap.StrMap AudioSelector)
derive instance newtypeMapOfAudioSelector :: Newtype MapOfAudioSelector _
derive instance repGenericMapOfAudioSelector :: Generic MapOfAudioSelector _
instance showMapOfAudioSelector :: Show MapOfAudioSelector where
  show = genericShow
instance decodeMapOfAudioSelector :: Decode MapOfAudioSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfAudioSelector :: Encode MapOfAudioSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfAudioSelectorGroup = MapOfAudioSelectorGroup (StrMap.StrMap AudioSelectorGroup)
derive instance newtypeMapOfAudioSelectorGroup :: Newtype MapOfAudioSelectorGroup _
derive instance repGenericMapOfAudioSelectorGroup :: Generic MapOfAudioSelectorGroup _
instance showMapOfAudioSelectorGroup :: Show MapOfAudioSelectorGroup where
  show = genericShow
instance decodeMapOfAudioSelectorGroup :: Decode MapOfAudioSelectorGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfAudioSelectorGroup :: Encode MapOfAudioSelectorGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfCaptionSelector = MapOfCaptionSelector (StrMap.StrMap CaptionSelector)
derive instance newtypeMapOfCaptionSelector :: Newtype MapOfCaptionSelector _
derive instance repGenericMapOfCaptionSelector :: Generic MapOfCaptionSelector _
instance showMapOfCaptionSelector :: Show MapOfCaptionSelector where
  show = genericShow
instance decodeMapOfCaptionSelector :: Decode MapOfCaptionSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfCaptionSelector :: Encode MapOfCaptionSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOf__string = MapOf__string (StrMap.StrMap String)
derive instance newtypeMapOf__string :: Newtype MapOf__string _
derive instance repGenericMapOf__string :: Generic MapOf__string _
instance showMapOf__string :: Show MapOf__string where
  show = genericShow
instance decodeMapOf__string :: Decode MapOf__string where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOf__string :: Encode MapOf__string where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When enabled, include 'clap' atom if appropriate for the video output settings.
newtype MovClapAtom = MovClapAtom String
derive instance newtypeMovClapAtom :: Newtype MovClapAtom _
derive instance repGenericMovClapAtom :: Generic MovClapAtom _
instance showMovClapAtom :: Show MovClapAtom where
  show = genericShow
instance decodeMovClapAtom :: Decode MovClapAtom where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMovClapAtom :: Encode MovClapAtom where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
newtype MovCslgAtom = MovCslgAtom String
derive instance newtypeMovCslgAtom :: Newtype MovCslgAtom _
derive instance repGenericMovCslgAtom :: Generic MovCslgAtom _
instance showMovCslgAtom :: Show MovCslgAtom where
  show = genericShow
instance decodeMovCslgAtom :: Decode MovCslgAtom where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMovCslgAtom :: Encode MovCslgAtom where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file using XDCAM fourcc codes. This increases compatibility with Apple editors and players, but may decrease compatibility with other players. Only applicable when the video codec is MPEG2.
newtype MovMpeg2FourCCControl = MovMpeg2FourCCControl String
derive instance newtypeMovMpeg2FourCCControl :: Newtype MovMpeg2FourCCControl _
derive instance repGenericMovMpeg2FourCCControl :: Generic MovMpeg2FourCCControl _
instance showMovMpeg2FourCCControl :: Show MovMpeg2FourCCControl where
  show = genericShow
instance decodeMovMpeg2FourCCControl :: Decode MovMpeg2FourCCControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMovMpeg2FourCCControl :: Encode MovMpeg2FourCCControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If set to OMNEON, inserts Omneon-compatible padding
newtype MovPaddingControl = MovPaddingControl String
derive instance newtypeMovPaddingControl :: Newtype MovPaddingControl _
derive instance repGenericMovPaddingControl :: Generic MovPaddingControl _
instance showMovPaddingControl :: Show MovPaddingControl where
  show = genericShow
instance decodeMovPaddingControl :: Decode MovPaddingControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMovPaddingControl :: Encode MovPaddingControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A value of 'external' creates separate media files and the wrapper file (.mov) contains references to these media files. A value of 'self_contained' creates only a wrapper (.mov) file and this file contains all of the media.
newtype MovReference = MovReference String
derive instance newtypeMovReference :: Newtype MovReference _
derive instance repGenericMovReference :: Generic MovReference _
instance showMovReference :: Show MovReference where
  show = genericShow
instance decodeMovReference :: Decode MovReference where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMovReference :: Encode MovReference where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for MOV Container.
newtype MovSettings = MovSettings 
  { "ClapAtom" :: NullOrUndefined.NullOrUndefined (MovClapAtom)
  , "CslgAtom" :: NullOrUndefined.NullOrUndefined (MovCslgAtom)
  , "Mpeg2FourCCControl" :: NullOrUndefined.NullOrUndefined (MovMpeg2FourCCControl)
  , "PaddingControl" :: NullOrUndefined.NullOrUndefined (MovPaddingControl)
  , "Reference" :: NullOrUndefined.NullOrUndefined (MovReference)
  }
derive instance newtypeMovSettings :: Newtype MovSettings _
derive instance repGenericMovSettings :: Generic MovSettings _
instance showMovSettings :: Show MovSettings where
  show = genericShow
instance decodeMovSettings :: Decode MovSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMovSettings :: Encode MovSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value MP2.
newtype Mp2Settings = Mp2Settings 
  { "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "Channels" :: NullOrUndefined.NullOrUndefined (Int)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeMp2Settings :: Newtype Mp2Settings _
derive instance repGenericMp2Settings :: Generic Mp2Settings _
instance showMp2Settings :: Show Mp2Settings where
  show = genericShow
instance decodeMp2Settings :: Decode Mp2Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMp2Settings :: Encode Mp2Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
newtype Mp4CslgAtom = Mp4CslgAtom String
derive instance newtypeMp4CslgAtom :: Newtype Mp4CslgAtom _
derive instance repGenericMp4CslgAtom :: Generic Mp4CslgAtom _
instance showMp4CslgAtom :: Show Mp4CslgAtom where
  show = genericShow
instance decodeMp4CslgAtom :: Decode Mp4CslgAtom where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMp4CslgAtom :: Encode Mp4CslgAtom where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Inserts a free-space box immediately after the moov box.
newtype Mp4FreeSpaceBox = Mp4FreeSpaceBox String
derive instance newtypeMp4FreeSpaceBox :: Newtype Mp4FreeSpaceBox _
derive instance repGenericMp4FreeSpaceBox :: Generic Mp4FreeSpaceBox _
instance showMp4FreeSpaceBox :: Show Mp4FreeSpaceBox where
  show = genericShow
instance decodeMp4FreeSpaceBox :: Decode Mp4FreeSpaceBox where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMp4FreeSpaceBox :: Encode Mp4FreeSpaceBox where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
newtype Mp4MoovPlacement = Mp4MoovPlacement String
derive instance newtypeMp4MoovPlacement :: Newtype Mp4MoovPlacement _
derive instance repGenericMp4MoovPlacement :: Generic Mp4MoovPlacement _
instance showMp4MoovPlacement :: Show Mp4MoovPlacement where
  show = genericShow
instance decodeMp4MoovPlacement :: Decode Mp4MoovPlacement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMp4MoovPlacement :: Encode Mp4MoovPlacement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for MP4 Container
newtype Mp4Settings = Mp4Settings 
  { "CslgAtom" :: NullOrUndefined.NullOrUndefined (Mp4CslgAtom)
  , "FreeSpaceBox" :: NullOrUndefined.NullOrUndefined (Mp4FreeSpaceBox)
  , "MoovPlacement" :: NullOrUndefined.NullOrUndefined (Mp4MoovPlacement)
  , "Mp4MajorBrand" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMp4Settings :: Newtype Mp4Settings _
derive instance repGenericMp4Settings :: Generic Mp4Settings _
instance showMp4Settings :: Show Mp4Settings where
  show = genericShow
instance decodeMp4Settings :: Decode Mp4Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMp4Settings :: Encode Mp4Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adaptive quantization. Allows intra-frame quantizers to vary to improve visual quality.
newtype Mpeg2AdaptiveQuantization = Mpeg2AdaptiveQuantization String
derive instance newtypeMpeg2AdaptiveQuantization :: Newtype Mpeg2AdaptiveQuantization _
derive instance repGenericMpeg2AdaptiveQuantization :: Generic Mpeg2AdaptiveQuantization _
instance showMpeg2AdaptiveQuantization :: Show Mpeg2AdaptiveQuantization where
  show = genericShow
instance decodeMpeg2AdaptiveQuantization :: Decode Mpeg2AdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2AdaptiveQuantization :: Encode Mpeg2AdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
newtype Mpeg2CodecLevel = Mpeg2CodecLevel String
derive instance newtypeMpeg2CodecLevel :: Newtype Mpeg2CodecLevel _
derive instance repGenericMpeg2CodecLevel :: Generic Mpeg2CodecLevel _
instance showMpeg2CodecLevel :: Show Mpeg2CodecLevel where
  show = genericShow
instance decodeMpeg2CodecLevel :: Decode Mpeg2CodecLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2CodecLevel :: Encode Mpeg2CodecLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
newtype Mpeg2CodecProfile = Mpeg2CodecProfile String
derive instance newtypeMpeg2CodecProfile :: Newtype Mpeg2CodecProfile _
derive instance repGenericMpeg2CodecProfile :: Generic Mpeg2CodecProfile _
instance showMpeg2CodecProfile :: Show Mpeg2CodecProfile where
  show = genericShow
instance decodeMpeg2CodecProfile :: Decode Mpeg2CodecProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2CodecProfile :: Encode Mpeg2CodecProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
newtype Mpeg2FramerateControl = Mpeg2FramerateControl String
derive instance newtypeMpeg2FramerateControl :: Newtype Mpeg2FramerateControl _
derive instance repGenericMpeg2FramerateControl :: Generic Mpeg2FramerateControl _
instance showMpeg2FramerateControl :: Show Mpeg2FramerateControl where
  show = genericShow
instance decodeMpeg2FramerateControl :: Decode Mpeg2FramerateControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2FramerateControl :: Encode Mpeg2FramerateControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
newtype Mpeg2FramerateConversionAlgorithm = Mpeg2FramerateConversionAlgorithm String
derive instance newtypeMpeg2FramerateConversionAlgorithm :: Newtype Mpeg2FramerateConversionAlgorithm _
derive instance repGenericMpeg2FramerateConversionAlgorithm :: Generic Mpeg2FramerateConversionAlgorithm _
instance showMpeg2FramerateConversionAlgorithm :: Show Mpeg2FramerateConversionAlgorithm where
  show = genericShow
instance decodeMpeg2FramerateConversionAlgorithm :: Decode Mpeg2FramerateConversionAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2FramerateConversionAlgorithm :: Encode Mpeg2FramerateConversionAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Indicates if the GOP Size in MPEG2 is specified in frames or seconds. If seconds the system will convert the GOP Size into a frame count at run time.
newtype Mpeg2GopSizeUnits = Mpeg2GopSizeUnits String
derive instance newtypeMpeg2GopSizeUnits :: Newtype Mpeg2GopSizeUnits _
derive instance repGenericMpeg2GopSizeUnits :: Generic Mpeg2GopSizeUnits _
instance showMpeg2GopSizeUnits :: Show Mpeg2GopSizeUnits where
  show = genericShow
instance decodeMpeg2GopSizeUnits :: Decode Mpeg2GopSizeUnits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2GopSizeUnits :: Encode Mpeg2GopSizeUnits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
newtype Mpeg2InterlaceMode = Mpeg2InterlaceMode String
derive instance newtypeMpeg2InterlaceMode :: Newtype Mpeg2InterlaceMode _
derive instance repGenericMpeg2InterlaceMode :: Generic Mpeg2InterlaceMode _
instance showMpeg2InterlaceMode :: Show Mpeg2InterlaceMode where
  show = genericShow
instance decodeMpeg2InterlaceMode :: Decode Mpeg2InterlaceMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2InterlaceMode :: Encode Mpeg2InterlaceMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Intra DC precision (Mpeg2IntraDcPrecision) to set quantization precision for intra-block DC coefficients. If you choose the value auto, the service will automatically select the precision based on the per-frame compression ratio.
newtype Mpeg2IntraDcPrecision = Mpeg2IntraDcPrecision String
derive instance newtypeMpeg2IntraDcPrecision :: Newtype Mpeg2IntraDcPrecision _
derive instance repGenericMpeg2IntraDcPrecision :: Generic Mpeg2IntraDcPrecision _
instance showMpeg2IntraDcPrecision :: Show Mpeg2IntraDcPrecision where
  show = genericShow
instance decodeMpeg2IntraDcPrecision :: Decode Mpeg2IntraDcPrecision where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2IntraDcPrecision :: Encode Mpeg2IntraDcPrecision where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Using the API, enable ParFollowSource if you want the service to use the pixel aspect ratio from the input. Using the console, do this by choosing Follow source for Pixel aspect ratio.
newtype Mpeg2ParControl = Mpeg2ParControl String
derive instance newtypeMpeg2ParControl :: Newtype Mpeg2ParControl _
derive instance repGenericMpeg2ParControl :: Generic Mpeg2ParControl _
instance showMpeg2ParControl :: Show Mpeg2ParControl where
  show = genericShow
instance decodeMpeg2ParControl :: Decode Mpeg2ParControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2ParControl :: Encode Mpeg2ParControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Quality tuning level (Mpeg2QualityTuningLevel) to specifiy whether to use single-pass or multipass video encoding.
newtype Mpeg2QualityTuningLevel = Mpeg2QualityTuningLevel String
derive instance newtypeMpeg2QualityTuningLevel :: Newtype Mpeg2QualityTuningLevel _
derive instance repGenericMpeg2QualityTuningLevel :: Generic Mpeg2QualityTuningLevel _
instance showMpeg2QualityTuningLevel :: Show Mpeg2QualityTuningLevel where
  show = genericShow
instance decodeMpeg2QualityTuningLevel :: Decode Mpeg2QualityTuningLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2QualityTuningLevel :: Encode Mpeg2QualityTuningLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
newtype Mpeg2RateControlMode = Mpeg2RateControlMode String
derive instance newtypeMpeg2RateControlMode :: Newtype Mpeg2RateControlMode _
derive instance repGenericMpeg2RateControlMode :: Generic Mpeg2RateControlMode _
instance showMpeg2RateControlMode :: Show Mpeg2RateControlMode where
  show = genericShow
instance decodeMpeg2RateControlMode :: Decode Mpeg2RateControlMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2RateControlMode :: Encode Mpeg2RateControlMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Scene change detection (inserts I-frames on scene changes).
newtype Mpeg2SceneChangeDetect = Mpeg2SceneChangeDetect String
derive instance newtypeMpeg2SceneChangeDetect :: Newtype Mpeg2SceneChangeDetect _
derive instance repGenericMpeg2SceneChangeDetect :: Generic Mpeg2SceneChangeDetect _
instance showMpeg2SceneChangeDetect :: Show Mpeg2SceneChangeDetect where
  show = genericShow
instance decodeMpeg2SceneChangeDetect :: Decode Mpeg2SceneChangeDetect where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2SceneChangeDetect :: Encode Mpeg2SceneChangeDetect where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value MPEG2.
newtype Mpeg2Settings = Mpeg2Settings 
  { "AdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (Mpeg2AdaptiveQuantization)
  , "Bitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "CodecLevel" :: NullOrUndefined.NullOrUndefined (Mpeg2CodecLevel)
  , "CodecProfile" :: NullOrUndefined.NullOrUndefined (Mpeg2CodecProfile)
  , "FramerateControl" :: NullOrUndefined.NullOrUndefined (Mpeg2FramerateControl)
  , "FramerateConversionAlgorithm" :: NullOrUndefined.NullOrUndefined (Mpeg2FramerateConversionAlgorithm)
  , "FramerateDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "GopClosedCadence" :: NullOrUndefined.NullOrUndefined (Int)
  , "GopSize" :: NullOrUndefined.NullOrUndefined (Number)
  , "GopSizeUnits" :: NullOrUndefined.NullOrUndefined (Mpeg2GopSizeUnits)
  , "HrdBufferInitialFillPercentage" :: NullOrUndefined.NullOrUndefined (Int)
  , "HrdBufferSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "InterlaceMode" :: NullOrUndefined.NullOrUndefined (Mpeg2InterlaceMode)
  , "IntraDcPrecision" :: NullOrUndefined.NullOrUndefined (Mpeg2IntraDcPrecision)
  , "MaxBitrate" :: NullOrUndefined.NullOrUndefined (Int)
  , "MinIInterval" :: NullOrUndefined.NullOrUndefined (Int)
  , "NumberBFramesBetweenReferenceFrames" :: NullOrUndefined.NullOrUndefined (Int)
  , "ParControl" :: NullOrUndefined.NullOrUndefined (Mpeg2ParControl)
  , "ParDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "QualityTuningLevel" :: NullOrUndefined.NullOrUndefined (Mpeg2QualityTuningLevel)
  , "RateControlMode" :: NullOrUndefined.NullOrUndefined (Mpeg2RateControlMode)
  , "SceneChangeDetect" :: NullOrUndefined.NullOrUndefined (Mpeg2SceneChangeDetect)
  , "SlowPal" :: NullOrUndefined.NullOrUndefined (Mpeg2SlowPal)
  , "Softness" :: NullOrUndefined.NullOrUndefined (Int)
  , "SpatialAdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (Mpeg2SpatialAdaptiveQuantization)
  , "Syntax" :: NullOrUndefined.NullOrUndefined (Mpeg2Syntax)
  , "Telecine" :: NullOrUndefined.NullOrUndefined (Mpeg2Telecine)
  , "TemporalAdaptiveQuantization" :: NullOrUndefined.NullOrUndefined (Mpeg2TemporalAdaptiveQuantization)
  }
derive instance newtypeMpeg2Settings :: Newtype Mpeg2Settings _
derive instance repGenericMpeg2Settings :: Generic Mpeg2Settings _
instance showMpeg2Settings :: Show Mpeg2Settings where
  show = genericShow
instance decodeMpeg2Settings :: Decode Mpeg2Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2Settings :: Encode Mpeg2Settings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
newtype Mpeg2SlowPal = Mpeg2SlowPal String
derive instance newtypeMpeg2SlowPal :: Newtype Mpeg2SlowPal _
derive instance repGenericMpeg2SlowPal :: Generic Mpeg2SlowPal _
instance showMpeg2SlowPal :: Show Mpeg2SlowPal where
  show = genericShow
instance decodeMpeg2SlowPal :: Decode Mpeg2SlowPal where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2SlowPal :: Encode Mpeg2SlowPal where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adjust quantization within each frame based on spatial variation of content complexity.
newtype Mpeg2SpatialAdaptiveQuantization = Mpeg2SpatialAdaptiveQuantization String
derive instance newtypeMpeg2SpatialAdaptiveQuantization :: Newtype Mpeg2SpatialAdaptiveQuantization _
derive instance repGenericMpeg2SpatialAdaptiveQuantization :: Generic Mpeg2SpatialAdaptiveQuantization _
instance showMpeg2SpatialAdaptiveQuantization :: Show Mpeg2SpatialAdaptiveQuantization where
  show = genericShow
instance decodeMpeg2SpatialAdaptiveQuantization :: Decode Mpeg2SpatialAdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2SpatialAdaptiveQuantization :: Encode Mpeg2SpatialAdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Produces a Type D-10 compatible bitstream (SMPTE 356M-2001).
newtype Mpeg2Syntax = Mpeg2Syntax String
derive instance newtypeMpeg2Syntax :: Newtype Mpeg2Syntax _
derive instance repGenericMpeg2Syntax :: Generic Mpeg2Syntax _
instance showMpeg2Syntax :: Show Mpeg2Syntax where
  show = genericShow
instance decodeMpeg2Syntax :: Decode Mpeg2Syntax where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2Syntax :: Encode Mpeg2Syntax where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Only use Telecine (Mpeg2Telecine) when you set Framerate (Framerate) to 29.970. Set Telecine (Mpeg2Telecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
newtype Mpeg2Telecine = Mpeg2Telecine String
derive instance newtypeMpeg2Telecine :: Newtype Mpeg2Telecine _
derive instance repGenericMpeg2Telecine :: Generic Mpeg2Telecine _
instance showMpeg2Telecine :: Show Mpeg2Telecine where
  show = genericShow
instance decodeMpeg2Telecine :: Decode Mpeg2Telecine where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2Telecine :: Encode Mpeg2Telecine where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Adjust quantization within each frame based on temporal variation of content complexity.
newtype Mpeg2TemporalAdaptiveQuantization = Mpeg2TemporalAdaptiveQuantization String
derive instance newtypeMpeg2TemporalAdaptiveQuantization :: Newtype Mpeg2TemporalAdaptiveQuantization _
derive instance repGenericMpeg2TemporalAdaptiveQuantization :: Generic Mpeg2TemporalAdaptiveQuantization _
instance showMpeg2TemporalAdaptiveQuantization :: Show Mpeg2TemporalAdaptiveQuantization where
  show = genericShow
instance decodeMpeg2TemporalAdaptiveQuantization :: Decode Mpeg2TemporalAdaptiveQuantization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMpeg2TemporalAdaptiveQuantization :: Encode Mpeg2TemporalAdaptiveQuantization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
newtype MsSmoothAudioDeduplication = MsSmoothAudioDeduplication String
derive instance newtypeMsSmoothAudioDeduplication :: Newtype MsSmoothAudioDeduplication _
derive instance repGenericMsSmoothAudioDeduplication :: Generic MsSmoothAudioDeduplication _
instance showMsSmoothAudioDeduplication :: Show MsSmoothAudioDeduplication where
  show = genericShow
instance decodeMsSmoothAudioDeduplication :: Decode MsSmoothAudioDeduplication where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMsSmoothAudioDeduplication :: Encode MsSmoothAudioDeduplication where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If you are using DRM, set DRM System (MsSmoothEncryptionSettings) to specify the value SpekeKeyProvider.
newtype MsSmoothEncryptionSettings = MsSmoothEncryptionSettings 
  { "SpekeKeyProvider" :: NullOrUndefined.NullOrUndefined (SpekeKeyProvider)
  }
derive instance newtypeMsSmoothEncryptionSettings :: Newtype MsSmoothEncryptionSettings _
derive instance repGenericMsSmoothEncryptionSettings :: Generic MsSmoothEncryptionSettings _
instance showMsSmoothEncryptionSettings :: Show MsSmoothEncryptionSettings where
  show = genericShow
instance decodeMsSmoothEncryptionSettings :: Decode MsSmoothEncryptionSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMsSmoothEncryptionSettings :: Encode MsSmoothEncryptionSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to MS_SMOOTH_GROUP_SETTINGS.
newtype MsSmoothGroupSettings = MsSmoothGroupSettings 
  { "AudioDeduplication" :: NullOrUndefined.NullOrUndefined (MsSmoothAudioDeduplication)
  , "Destination" :: NullOrUndefined.NullOrUndefined (String)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (MsSmoothEncryptionSettings)
  , "FragmentLength" :: NullOrUndefined.NullOrUndefined (Int)
  , "ManifestEncoding" :: NullOrUndefined.NullOrUndefined (MsSmoothManifestEncoding)
  }
derive instance newtypeMsSmoothGroupSettings :: Newtype MsSmoothGroupSettings _
derive instance repGenericMsSmoothGroupSettings :: Generic MsSmoothGroupSettings _
instance showMsSmoothGroupSettings :: Show MsSmoothGroupSettings where
  show = genericShow
instance decodeMsSmoothGroupSettings :: Decode MsSmoothGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMsSmoothGroupSettings :: Encode MsSmoothGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
newtype MsSmoothManifestEncoding = MsSmoothManifestEncoding String
derive instance newtypeMsSmoothManifestEncoding :: Newtype MsSmoothManifestEncoding _
derive instance repGenericMsSmoothManifestEncoding :: Generic MsSmoothManifestEncoding _
instance showMsSmoothManifestEncoding :: Show MsSmoothManifestEncoding where
  show = genericShow
instance decodeMsSmoothManifestEncoding :: Decode MsSmoothManifestEncoding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMsSmoothManifestEncoding :: Encode MsSmoothManifestEncoding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for Nielsen Configuration
newtype NielsenConfiguration = NielsenConfiguration 
  { "BreakoutCode" :: NullOrUndefined.NullOrUndefined (Int)
  , "DistributorId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNielsenConfiguration :: Newtype NielsenConfiguration _
derive instance repGenericNielsenConfiguration :: Generic NielsenConfiguration _
instance showNielsenConfiguration :: Show NielsenConfiguration where
  show = genericShow
instance decodeNielsenConfiguration :: Decode NielsenConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNielsenConfiguration :: Encode NielsenConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable the Noise reducer (NoiseReducer) feature to remove noise from your video output if necessary. Enable or disable this feature for each output individually. This setting is disabled by default. When you enable Noise reducer (NoiseReducer), you must also select a value for Noise reducer filter (NoiseReducerFilter).
newtype NoiseReducer = NoiseReducer 
  { "Filter" :: NullOrUndefined.NullOrUndefined (NoiseReducerFilter)
  , "FilterSettings" :: NullOrUndefined.NullOrUndefined (NoiseReducerFilterSettings)
  , "SpatialFilterSettings" :: NullOrUndefined.NullOrUndefined (NoiseReducerSpatialFilterSettings)
  }
derive instance newtypeNoiseReducer :: Newtype NoiseReducer _
derive instance repGenericNoiseReducer :: Generic NoiseReducer _
instance showNoiseReducer :: Show NoiseReducer where
  show = genericShow
instance decodeNoiseReducer :: Decode NoiseReducer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoiseReducer :: Encode NoiseReducer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Noise reducer filter (NoiseReducerFilter) to select one of the following spatial image filtering functions. To use this setting, you must also enable Noise reducer (NoiseReducer). * Bilateral is an edge preserving noise reduction filter * Mean (softest), Gaussian, Lanczos, and Sharpen (sharpest) are convolution filters * Conserve is a min/max noise reduction filter * Spatial is frequency-domain filter based on JND principles.
newtype NoiseReducerFilter = NoiseReducerFilter String
derive instance newtypeNoiseReducerFilter :: Newtype NoiseReducerFilter _
derive instance repGenericNoiseReducerFilter :: Generic NoiseReducerFilter _
instance showNoiseReducerFilter :: Show NoiseReducerFilter where
  show = genericShow
instance decodeNoiseReducerFilter :: Decode NoiseReducerFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoiseReducerFilter :: Encode NoiseReducerFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for a noise reducer filter
newtype NoiseReducerFilterSettings = NoiseReducerFilterSettings 
  { "Strength" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeNoiseReducerFilterSettings :: Newtype NoiseReducerFilterSettings _
derive instance repGenericNoiseReducerFilterSettings :: Generic NoiseReducerFilterSettings _
instance showNoiseReducerFilterSettings :: Show NoiseReducerFilterSettings where
  show = genericShow
instance decodeNoiseReducerFilterSettings :: Decode NoiseReducerFilterSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoiseReducerFilterSettings :: Encode NoiseReducerFilterSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Noise reducer filter settings for spatial filter.
newtype NoiseReducerSpatialFilterSettings = NoiseReducerSpatialFilterSettings 
  { "PostFilterSharpenStrength" :: NullOrUndefined.NullOrUndefined (Int)
  , "Speed" :: NullOrUndefined.NullOrUndefined (Int)
  , "Strength" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeNoiseReducerSpatialFilterSettings :: Newtype NoiseReducerSpatialFilterSettings _
derive instance repGenericNoiseReducerSpatialFilterSettings :: Generic NoiseReducerSpatialFilterSettings _
instance showNoiseReducerSpatialFilterSettings :: Show NoiseReducerSpatialFilterSettings where
  show = genericShow
instance decodeNoiseReducerSpatialFilterSettings :: Decode NoiseReducerSpatialFilterSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoiseReducerSpatialFilterSettings :: Encode NoiseReducerSpatialFilterSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The resource you requested does not exist.
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


-- | When you request lists of resources, you can optionally specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
newtype Order = Order String
derive instance newtypeOrder :: Newtype Order _
derive instance repGenericOrder :: Generic Order _
instance showOrder :: Show Order where
  show = genericShow
instance decodeOrder :: Decode Order where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrder :: Encode Order where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An output object describes the settings for a single output file or stream in an output group.
newtype Output = Output 
  { "AudioDescriptions" :: NullOrUndefined.NullOrUndefined (ListOfAudioDescription)
  , "CaptionDescriptions" :: NullOrUndefined.NullOrUndefined (ListOfCaptionDescription)
  , "ContainerSettings" :: NullOrUndefined.NullOrUndefined (ContainerSettings)
  , "Extension" :: NullOrUndefined.NullOrUndefined (String)
  , "NameModifier" :: NullOrUndefined.NullOrUndefined (String)
  , "OutputSettings" :: NullOrUndefined.NullOrUndefined (OutputSettings)
  , "Preset" :: NullOrUndefined.NullOrUndefined (String)
  , "VideoDescription" :: NullOrUndefined.NullOrUndefined (VideoDescription)
  }
derive instance newtypeOutput :: Newtype Output _
derive instance repGenericOutput :: Generic Output _
instance showOutput :: Show Output where
  show = genericShow
instance decodeOutput :: Decode Output where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutput :: Encode Output where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | OutputChannel mapping settings.
newtype OutputChannelMapping = OutputChannelMapping 
  { "InputChannels" :: NullOrUndefined.NullOrUndefined (ListOf__integer)
  }
derive instance newtypeOutputChannelMapping :: Newtype OutputChannelMapping _
derive instance repGenericOutputChannelMapping :: Generic OutputChannelMapping _
instance showOutputChannelMapping :: Show OutputChannelMapping where
  show = genericShow
instance decodeOutputChannelMapping :: Decode OutputChannelMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputChannelMapping :: Encode OutputChannelMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Details regarding output
newtype OutputDetail = OutputDetail 
  { "DurationInMs" :: NullOrUndefined.NullOrUndefined (Int)
  , "VideoDetails" :: NullOrUndefined.NullOrUndefined (VideoDetail)
  }
derive instance newtypeOutputDetail :: Newtype OutputDetail _
derive instance repGenericOutputDetail :: Generic OutputDetail _
instance showOutputDetail :: Show OutputDetail where
  show = genericShow
instance decodeOutputDetail :: Decode OutputDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputDetail :: Encode OutputDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Group of outputs
newtype OutputGroup = OutputGroup 
  { "CustomName" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
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


-- | Contains details about the output groups specified in the job settings.
newtype OutputGroupDetail = OutputGroupDetail 
  { "OutputDetails" :: NullOrUndefined.NullOrUndefined (ListOfOutputDetail)
  }
derive instance newtypeOutputGroupDetail :: Newtype OutputGroupDetail _
derive instance repGenericOutputGroupDetail :: Generic OutputGroupDetail _
instance showOutputGroupDetail :: Show OutputGroupDetail where
  show = genericShow
instance decodeOutputGroupDetail :: Decode OutputGroupDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputGroupDetail :: Encode OutputGroupDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Output Group settings, including type
newtype OutputGroupSettings = OutputGroupSettings 
  { "DashIsoGroupSettings" :: NullOrUndefined.NullOrUndefined (DashIsoGroupSettings)
  , "FileGroupSettings" :: NullOrUndefined.NullOrUndefined (FileGroupSettings)
  , "HlsGroupSettings" :: NullOrUndefined.NullOrUndefined (HlsGroupSettings)
  , "MsSmoothGroupSettings" :: NullOrUndefined.NullOrUndefined (MsSmoothGroupSettings)
  , "Type" :: NullOrUndefined.NullOrUndefined (OutputGroupType)
  }
derive instance newtypeOutputGroupSettings :: Newtype OutputGroupSettings _
derive instance repGenericOutputGroupSettings :: Generic OutputGroupSettings _
instance showOutputGroupSettings :: Show OutputGroupSettings where
  show = genericShow
instance decodeOutputGroupSettings :: Decode OutputGroupSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputGroupSettings :: Encode OutputGroupSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming)
newtype OutputGroupType = OutputGroupType String
derive instance newtypeOutputGroupType :: Newtype OutputGroupType _
derive instance repGenericOutputGroupType :: Generic OutputGroupType _
instance showOutputGroupType :: Show OutputGroupType where
  show = genericShow
instance decodeOutputGroupType :: Decode OutputGroupType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputGroupType :: Encode OutputGroupType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Selects method of inserting SDT information into output stream.  "Follow input SDT" copies SDT information from input stream to  output stream. "Follow input SDT if present" copies SDT information from  input stream to output stream if SDT information is present in the input, otherwise it will fall back on the user-defined values. Enter "SDT  Manually" means user will enter the SDT information. "No SDT" means output  stream will not contain SDT information.
newtype OutputSdt = OutputSdt String
derive instance newtypeOutputSdt :: Newtype OutputSdt _
derive instance repGenericOutputSdt :: Generic OutputSdt _
instance showOutputSdt :: Show OutputSdt where
  show = genericShow
instance decodeOutputSdt :: Decode OutputSdt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputSdt :: Encode OutputSdt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Specific settings for this type of output.
newtype OutputSettings = OutputSettings 
  { "HlsSettings" :: NullOrUndefined.NullOrUndefined (HlsSettings)
  }
derive instance newtypeOutputSettings :: Newtype OutputSettings _
derive instance repGenericOutputSettings :: Generic OutputSettings _
instance showOutputSettings :: Show OutputSettings where
  show = genericShow
instance decodeOutputSettings :: Decode OutputSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputSettings :: Encode OutputSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A preset is a collection of preconfigured media conversion settings that you want MediaConvert to apply to the output during the conversion process.
newtype Preset = Preset 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Category" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (Number)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdated" :: NullOrUndefined.NullOrUndefined (Number)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Settings" :: NullOrUndefined.NullOrUndefined (PresetSettings)
  , "Type" :: NullOrUndefined.NullOrUndefined (Type)
  }
derive instance newtypePreset :: Newtype Preset _
derive instance repGenericPreset :: Generic Preset _
instance showPreset :: Show Preset where
  show = genericShow
instance decodePreset :: Decode Preset where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePreset :: Encode Preset where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
newtype PresetListBy = PresetListBy String
derive instance newtypePresetListBy :: Newtype PresetListBy _
derive instance repGenericPresetListBy :: Generic PresetListBy _
instance showPresetListBy :: Show PresetListBy where
  show = genericShow
instance decodePresetListBy :: Decode PresetListBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresetListBy :: Encode PresetListBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for preset
newtype PresetSettings = PresetSettings 
  { "AudioDescriptions" :: NullOrUndefined.NullOrUndefined (ListOfAudioDescription)
  , "CaptionDescriptions" :: NullOrUndefined.NullOrUndefined (ListOfCaptionDescriptionPreset)
  , "ContainerSettings" :: NullOrUndefined.NullOrUndefined (ContainerSettings)
  , "VideoDescription" :: NullOrUndefined.NullOrUndefined (VideoDescription)
  }
derive instance newtypePresetSettings :: Newtype PresetSettings _
derive instance repGenericPresetSettings :: Generic PresetSettings _
instance showPresetSettings :: Show PresetSettings where
  show = genericShow
instance decodePresetSettings :: Decode PresetSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresetSettings :: Encode PresetSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Profile (ProResCodecProfile) to specifiy the type of Apple ProRes codec to use for this output.
newtype ProresCodecProfile = ProresCodecProfile String
derive instance newtypeProresCodecProfile :: Newtype ProresCodecProfile _
derive instance repGenericProresCodecProfile :: Generic ProresCodecProfile _
instance showProresCodecProfile :: Show ProresCodecProfile where
  show = genericShow
instance decodeProresCodecProfile :: Decode ProresCodecProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProresCodecProfile :: Encode ProresCodecProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Using the API, set FramerateControl to INITIALIZE_FROM_SOURCE if you want the service to use the framerate from the input. Using the console, do this by choosing INITIALIZE_FROM_SOURCE for Framerate.
newtype ProresFramerateControl = ProresFramerateControl String
derive instance newtypeProresFramerateControl :: Newtype ProresFramerateControl _
derive instance repGenericProresFramerateControl :: Generic ProresFramerateControl _
instance showProresFramerateControl :: Show ProresFramerateControl where
  show = genericShow
instance decodeProresFramerateControl :: Decode ProresFramerateControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProresFramerateControl :: Encode ProresFramerateControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | When set to INTERPOLATE, produces smoother motion during framerate conversion.
newtype ProresFramerateConversionAlgorithm = ProresFramerateConversionAlgorithm String
derive instance newtypeProresFramerateConversionAlgorithm :: Newtype ProresFramerateConversionAlgorithm _
derive instance repGenericProresFramerateConversionAlgorithm :: Generic ProresFramerateConversionAlgorithm _
instance showProresFramerateConversionAlgorithm :: Show ProresFramerateConversionAlgorithm where
  show = genericShow
instance decodeProresFramerateConversionAlgorithm :: Decode ProresFramerateConversionAlgorithm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProresFramerateConversionAlgorithm :: Encode ProresFramerateConversionAlgorithm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Interlace mode (InterlaceMode) to choose the scan line type for the output. * Top Field First (TOP_FIELD) and Bottom Field First (BOTTOM_FIELD) produce interlaced output with the entire output having the same field polarity (top or bottom first). * Follow, Default Top (FOLLOw_TOP_FIELD) and Follow, Default Bottom (FOLLOW_BOTTOM_FIELD) use the same  field polarity as the source. Therefore, behavior depends on the input scan type. - If the source is interlaced, the output will be interlaced with the same polarity as the source (it will follow the source). The output could therefore be a mix of "top field first" and "bottom field first". - If the source is progressive, the output will be interlaced with "top field first" or "bottom field first" polarity, depending on which of the Follow options you chose.
newtype ProresInterlaceMode = ProresInterlaceMode String
derive instance newtypeProresInterlaceMode :: Newtype ProresInterlaceMode _
derive instance repGenericProresInterlaceMode :: Generic ProresInterlaceMode _
instance showProresInterlaceMode :: Show ProresInterlaceMode where
  show = genericShow
instance decodeProresInterlaceMode :: Decode ProresInterlaceMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProresInterlaceMode :: Encode ProresInterlaceMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use (ProresParControl) to specify how the service determines the pixel aspect ratio. Set to Follow source (INITIALIZE_FROM_SOURCE) to use the pixel aspect ratio from the input.  To specify a different pixel aspect ratio: Using the console, choose it from the dropdown menu. Using the API, set ProresParControl to (SPECIFIED) and provide  for (ParNumerator) and (ParDenominator).
newtype ProresParControl = ProresParControl String
derive instance newtypeProresParControl :: Newtype ProresParControl _
derive instance repGenericProresParControl :: Generic ProresParControl _
instance showProresParControl :: Show ProresParControl where
  show = genericShow
instance decodeProresParControl :: Decode ProresParControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProresParControl :: Encode ProresParControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Codec) under (VideoDescription)>(CodecSettings) to the value PRORES.
newtype ProresSettings = ProresSettings 
  { "CodecProfile" :: NullOrUndefined.NullOrUndefined (ProresCodecProfile)
  , "FramerateControl" :: NullOrUndefined.NullOrUndefined (ProresFramerateControl)
  , "FramerateConversionAlgorithm" :: NullOrUndefined.NullOrUndefined (ProresFramerateConversionAlgorithm)
  , "FramerateDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "FramerateNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "InterlaceMode" :: NullOrUndefined.NullOrUndefined (ProresInterlaceMode)
  , "ParControl" :: NullOrUndefined.NullOrUndefined (ProresParControl)
  , "ParDenominator" :: NullOrUndefined.NullOrUndefined (Int)
  , "ParNumerator" :: NullOrUndefined.NullOrUndefined (Int)
  , "SlowPal" :: NullOrUndefined.NullOrUndefined (ProresSlowPal)
  , "Telecine" :: NullOrUndefined.NullOrUndefined (ProresTelecine)
  }
derive instance newtypeProresSettings :: Newtype ProresSettings _
derive instance repGenericProresSettings :: Generic ProresSettings _
instance showProresSettings :: Show ProresSettings where
  show = genericShow
instance decodeProresSettings :: Decode ProresSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProresSettings :: Encode ProresSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enables Slow PAL rate conversion. 23.976fps and 24fps input is relabeled as 25fps, and audio is sped up correspondingly.
newtype ProresSlowPal = ProresSlowPal String
derive instance newtypeProresSlowPal :: Newtype ProresSlowPal _
derive instance repGenericProresSlowPal :: Generic ProresSlowPal _
instance showProresSlowPal :: Show ProresSlowPal where
  show = genericShow
instance decodeProresSlowPal :: Decode ProresSlowPal where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProresSlowPal :: Encode ProresSlowPal where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Only use Telecine (ProresTelecine) when you set Framerate (Framerate) to 29.970. Set Telecine (ProresTelecine) to Hard (hard) to produce a 29.97i output from a 23.976 input. Set it to Soft (soft) to produce 23.976 output and leave converstion to the player.
newtype ProresTelecine = ProresTelecine String
derive instance newtypeProresTelecine :: Newtype ProresTelecine _
derive instance repGenericProresTelecine :: Generic ProresTelecine _
instance showProresTelecine :: Show ProresTelecine where
  show = genericShow
instance decodeProresTelecine :: Decode ProresTelecine where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProresTelecine :: Encode ProresTelecine where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | MediaConvert jobs are submitted to a queue. Unless specified otherwise jobs are submitted to a built-in default queue. User can create additional queues to separate the jobs of different categories or priority.
newtype Queue = Queue 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (Number)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "LastUpdated" :: NullOrUndefined.NullOrUndefined (Number)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (QueueStatus)
  , "Type" :: NullOrUndefined.NullOrUndefined (Type)
  }
derive instance newtypeQueue :: Newtype Queue _
derive instance repGenericQueue :: Generic Queue _
instance showQueue :: Show Queue where
  show = genericShow
instance decodeQueue :: Decode Queue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueue :: Encode Queue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.
newtype QueueListBy = QueueListBy String
derive instance newtypeQueueListBy :: Newtype QueueListBy _
derive instance repGenericQueueListBy :: Generic QueueListBy _
instance showQueueListBy :: Show QueueListBy where
  show = genericShow
instance decodeQueueListBy :: Decode QueueListBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueListBy :: Encode QueueListBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Queues can be ACTIVE or PAUSED. If you pause a queue, jobs in that queue will not begin. Jobs running when a queue is paused continue to run until they finish or error out.
newtype QueueStatus = QueueStatus String
derive instance newtypeQueueStatus :: Newtype QueueStatus _
derive instance repGenericQueueStatus :: Generic QueueStatus _
instance showQueueStatus :: Show QueueStatus where
  show = genericShow
instance decodeQueueStatus :: Decode QueueStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueStatus :: Encode QueueStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Rectangle to identify a specific area of the video frame.
newtype Rectangle = Rectangle 
  { "Height" :: NullOrUndefined.NullOrUndefined (Int)
  , "Width" :: NullOrUndefined.NullOrUndefined (Int)
  , "X" :: NullOrUndefined.NullOrUndefined (Int)
  , "Y" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeRectangle :: Newtype Rectangle _
derive instance repGenericRectangle :: Generic Rectangle _
instance showRectangle :: Show Rectangle where
  show = genericShow
instance decodeRectangle :: Decode Rectangle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRectangle :: Encode Rectangle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Manual audio remixing (RemixSettings) to adjust audio levels for each output channel. With audio remixing, you can output more or fewer audio channels than your input audio source provides.
newtype RemixSettings = RemixSettings 
  { "ChannelMapping" :: NullOrUndefined.NullOrUndefined (ChannelMapping)
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


-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.
newtype RespondToAfd = RespondToAfd String
derive instance newtypeRespondToAfd :: Newtype RespondToAfd _
derive instance repGenericRespondToAfd :: Generic RespondToAfd _
instance showRespondToAfd :: Show RespondToAfd where
  show = genericShow
instance decodeRespondToAfd :: Decode RespondToAfd where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRespondToAfd :: Encode RespondToAfd where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Applies only if your input aspect ratio is different from your output aspect ratio. Enable Stretch to output (StretchToOutput) to have the service stretch your video image to fit. Leave this setting disabled to allow the service to letterbox your video instead. This setting overrides any positioning value you specify elsewhere in the job.
newtype ScalingBehavior = ScalingBehavior String
derive instance newtypeScalingBehavior :: Newtype ScalingBehavior _
derive instance repGenericScalingBehavior :: Generic ScalingBehavior _
instance showScalingBehavior :: Show ScalingBehavior where
  show = genericShow
instance decodeScalingBehavior :: Decode ScalingBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingBehavior :: Encode ScalingBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Set Framerate (SccDestinationFramerate) to make sure that the captions and the video are synchronized in the output. Specify a framerate that matches the framerate of the associated video. If the video framerate is 29.97, choose 29.97 dropframe (FRAMERATE_29_97_DROPFRAME) only if the video has video_insertion=true and drop_frame_timecode=true; otherwise, choose 29.97 non-dropframe (FRAMERATE_29_97_NON_DROPFRAME).
newtype SccDestinationFramerate = SccDestinationFramerate String
derive instance newtypeSccDestinationFramerate :: Newtype SccDestinationFramerate _
derive instance repGenericSccDestinationFramerate :: Generic SccDestinationFramerate _
instance showSccDestinationFramerate :: Show SccDestinationFramerate where
  show = genericShow
instance decodeSccDestinationFramerate :: Decode SccDestinationFramerate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSccDestinationFramerate :: Encode SccDestinationFramerate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for SCC caption output.
newtype SccDestinationSettings = SccDestinationSettings 
  { "Framerate" :: NullOrUndefined.NullOrUndefined (SccDestinationFramerate)
  }
derive instance newtypeSccDestinationSettings :: Newtype SccDestinationSettings _
derive instance repGenericSccDestinationSettings :: Generic SccDestinationSettings _
instance showSccDestinationSettings :: Show SccDestinationSettings where
  show = genericShow
instance decodeSccDestinationSettings :: Decode SccDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSccDestinationSettings :: Encode SccDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for use with a SPEKE key provider
newtype SpekeKeyProvider = SpekeKeyProvider 
  { "ResourceId" :: NullOrUndefined.NullOrUndefined (String)
  , "SystemIds" :: NullOrUndefined.NullOrUndefined (ListOf__string)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSpekeKeyProvider :: Newtype SpekeKeyProvider _
derive instance repGenericSpekeKeyProvider :: Generic SpekeKeyProvider _
instance showSpekeKeyProvider :: Show SpekeKeyProvider where
  show = genericShow
instance decodeSpekeKeyProvider :: Decode SpekeKeyProvider where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSpekeKeyProvider :: Encode SpekeKeyProvider where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for use with a SPEKE key provider.
newtype StaticKeyProvider = StaticKeyProvider 
  { "KeyFormat" :: NullOrUndefined.NullOrUndefined (String)
  , "KeyFormatVersions" :: NullOrUndefined.NullOrUndefined (String)
  , "StaticKeyValue" :: NullOrUndefined.NullOrUndefined (String)
  , "Url" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeStaticKeyProvider :: Newtype StaticKeyProvider _
derive instance repGenericStaticKeyProvider :: Generic StaticKeyProvider _
instance showStaticKeyProvider :: Show StaticKeyProvider where
  show = genericShow
instance decodeStaticKeyProvider :: Decode StaticKeyProvider where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStaticKeyProvider :: Encode StaticKeyProvider where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for Teletext caption output
newtype TeletextDestinationSettings = TeletextDestinationSettings 
  { "PageNumber" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTeletextDestinationSettings :: Newtype TeletextDestinationSettings _
derive instance repGenericTeletextDestinationSettings :: Generic TeletextDestinationSettings _
instance showTeletextDestinationSettings :: Show TeletextDestinationSettings where
  show = genericShow
instance decodeTeletextDestinationSettings :: Decode TeletextDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTeletextDestinationSettings :: Encode TeletextDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings specific to Teletext caption sources, including Page number.
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


-- | Timecode burn-in (TimecodeBurnIn)--Burns the output timecode and specified prefix into the output.
newtype TimecodeBurnin = TimecodeBurnin 
  { "FontSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "Position" :: NullOrUndefined.NullOrUndefined (TimecodeBurninPosition)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTimecodeBurnin :: Newtype TimecodeBurnin _
derive instance repGenericTimecodeBurnin :: Generic TimecodeBurnin _
instance showTimecodeBurnin :: Show TimecodeBurnin where
  show = genericShow
instance decodeTimecodeBurnin :: Decode TimecodeBurnin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimecodeBurnin :: Encode TimecodeBurnin where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to specify the location the burned-in timecode on output video.
newtype TimecodeBurninPosition = TimecodeBurninPosition String
derive instance newtypeTimecodeBurninPosition :: Newtype TimecodeBurninPosition _
derive instance repGenericTimecodeBurninPosition :: Generic TimecodeBurninPosition _
instance showTimecodeBurninPosition :: Show TimecodeBurninPosition where
  show = genericShow
instance decodeTimecodeBurninPosition :: Decode TimecodeBurninPosition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimecodeBurninPosition :: Encode TimecodeBurninPosition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Contains settings used to acquire and adjust timecode information from inputs.
newtype TimecodeConfig = TimecodeConfig 
  { "Anchor" :: NullOrUndefined.NullOrUndefined (String)
  , "Source" :: NullOrUndefined.NullOrUndefined (TimecodeSource)
  , "Start" :: NullOrUndefined.NullOrUndefined (String)
  , "TimestampOffset" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTimecodeConfig :: Newtype TimecodeConfig _
derive instance repGenericTimecodeConfig :: Generic TimecodeConfig _
instance showTimecodeConfig :: Show TimecodeConfig where
  show = genericShow
instance decodeTimecodeConfig :: Decode TimecodeConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimecodeConfig :: Encode TimecodeConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Use Timecode source (TimecodeSource) to set how timecodes are handled within this input. To make sure that your video, audio, captions, and markers are synchronized and that time-based features, such as image inserter, work correctly, choose the Timecode source option that matches your assets. All timecodes are in a 24-hour format with frame number (HH:MM:SS:FF). * Embedded (EMBEDDED) - Use the timecode that is in the input video. If no embedded timecode is in the source, the service will use Start at 0 (ZEROBASED) instead. * Start at 0 (ZEROBASED) - Set the timecode of the initial frame to 00:00:00:00. * Specified Start (SPECIFIEDSTART) - Set the timecode of the initial frame to a value other than zero. You use Start timecode (Start) to provide this value.
newtype TimecodeSource = TimecodeSource String
derive instance newtypeTimecodeSource :: Newtype TimecodeSource _
derive instance repGenericTimecodeSource :: Generic TimecodeSource _
instance showTimecodeSource :: Show TimecodeSource where
  show = genericShow
instance decodeTimecodeSource :: Decode TimecodeSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimecodeSource :: Encode TimecodeSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | If PASSTHROUGH, inserts ID3 timed metadata from the timed_metadata REST command into this output.
newtype TimedMetadata = TimedMetadata String
derive instance newtypeTimedMetadata :: Newtype TimedMetadata _
derive instance repGenericTimedMetadata :: Generic TimedMetadata _
instance showTimedMetadata :: Show TimedMetadata where
  show = genericShow
instance decodeTimedMetadata :: Decode TimedMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimedMetadata :: Encode TimedMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in your job. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
newtype TimedMetadataInsertion = TimedMetadataInsertion 
  { "Id3Insertions" :: NullOrUndefined.NullOrUndefined (ListOfId3Insertion)
  }
derive instance newtypeTimedMetadataInsertion :: Newtype TimedMetadataInsertion _
derive instance repGenericTimedMetadataInsertion :: Generic TimedMetadataInsertion _
instance showTimedMetadataInsertion :: Show TimedMetadataInsertion where
  show = genericShow
instance decodeTimedMetadataInsertion :: Decode TimedMetadataInsertion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimedMetadataInsertion :: Encode TimedMetadataInsertion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information about when jobs are submitted, started, and finished is specified in Unix epoch format in seconds.
newtype Timing = Timing 
  { "FinishTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "SubmitTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeTiming :: Newtype Timing _
derive instance repGenericTiming :: Generic Timing _
instance showTiming :: Show Timing where
  show = genericShow
instance decodeTiming :: Decode Timing where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTiming :: Encode Timing where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Too many requests have been sent in too short of a time. The service limits the rate at which it will accept requests.
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


-- | Settings specific to TTML caption outputs, including Pass style information (TtmlStylePassthrough).
newtype TtmlDestinationSettings = TtmlDestinationSettings 
  { "StylePassthrough" :: NullOrUndefined.NullOrUndefined (TtmlStylePassthrough)
  }
derive instance newtypeTtmlDestinationSettings :: Newtype TtmlDestinationSettings _
derive instance repGenericTtmlDestinationSettings :: Generic TtmlDestinationSettings _
instance showTtmlDestinationSettings :: Show TtmlDestinationSettings where
  show = genericShow
instance decodeTtmlDestinationSettings :: Decode TtmlDestinationSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTtmlDestinationSettings :: Encode TtmlDestinationSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT, CFF-TT) to the CFF-TT output or TTML output.
newtype TtmlStylePassthrough = TtmlStylePassthrough String
derive instance newtypeTtmlStylePassthrough :: Newtype TtmlStylePassthrough _
derive instance repGenericTtmlStylePassthrough :: Generic TtmlStylePassthrough _
instance showTtmlStylePassthrough :: Show TtmlStylePassthrough where
  show = genericShow
instance decodeTtmlStylePassthrough :: Decode TtmlStylePassthrough where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTtmlStylePassthrough :: Encode TtmlStylePassthrough where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Type = Type String
derive instance newtypeType :: Newtype Type _
derive instance repGenericType :: Generic Type _
instance showType :: Show Type where
  show = genericShow
instance decodeType :: Decode Type where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeType :: Encode Type where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateJobTemplateRequest = UpdateJobTemplateRequest 
  { "Category" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: (String)
  , "Queue" :: NullOrUndefined.NullOrUndefined (String)
  , "Settings" :: NullOrUndefined.NullOrUndefined (JobTemplateSettings)
  }
derive instance newtypeUpdateJobTemplateRequest :: Newtype UpdateJobTemplateRequest _
derive instance repGenericUpdateJobTemplateRequest :: Generic UpdateJobTemplateRequest _
instance showUpdateJobTemplateRequest :: Show UpdateJobTemplateRequest where
  show = genericShow
instance decodeUpdateJobTemplateRequest :: Decode UpdateJobTemplateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobTemplateRequest :: Encode UpdateJobTemplateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateJobTemplateResponse = UpdateJobTemplateResponse 
  { "JobTemplate" :: NullOrUndefined.NullOrUndefined (JobTemplate)
  }
derive instance newtypeUpdateJobTemplateResponse :: Newtype UpdateJobTemplateResponse _
derive instance repGenericUpdateJobTemplateResponse :: Generic UpdateJobTemplateResponse _
instance showUpdateJobTemplateResponse :: Show UpdateJobTemplateResponse where
  show = genericShow
instance decodeUpdateJobTemplateResponse :: Decode UpdateJobTemplateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobTemplateResponse :: Encode UpdateJobTemplateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePresetRequest = UpdatePresetRequest 
  { "Category" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: (String)
  , "Settings" :: NullOrUndefined.NullOrUndefined (PresetSettings)
  }
derive instance newtypeUpdatePresetRequest :: Newtype UpdatePresetRequest _
derive instance repGenericUpdatePresetRequest :: Generic UpdatePresetRequest _
instance showUpdatePresetRequest :: Show UpdatePresetRequest where
  show = genericShow
instance decodeUpdatePresetRequest :: Decode UpdatePresetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePresetRequest :: Encode UpdatePresetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePresetResponse = UpdatePresetResponse 
  { "Preset" :: NullOrUndefined.NullOrUndefined (Preset)
  }
derive instance newtypeUpdatePresetResponse :: Newtype UpdatePresetResponse _
derive instance repGenericUpdatePresetResponse :: Generic UpdatePresetResponse _
instance showUpdatePresetResponse :: Show UpdatePresetResponse where
  show = genericShow
instance decodeUpdatePresetResponse :: Decode UpdatePresetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePresetResponse :: Encode UpdatePresetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateQueueRequest = UpdateQueueRequest 
  { "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (QueueStatus)
  }
derive instance newtypeUpdateQueueRequest :: Newtype UpdateQueueRequest _
derive instance repGenericUpdateQueueRequest :: Generic UpdateQueueRequest _
instance showUpdateQueueRequest :: Show UpdateQueueRequest where
  show = genericShow
instance decodeUpdateQueueRequest :: Decode UpdateQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateQueueRequest :: Encode UpdateQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateQueueResponse = UpdateQueueResponse 
  { "Queue" :: NullOrUndefined.NullOrUndefined (Queue)
  }
derive instance newtypeUpdateQueueResponse :: Newtype UpdateQueueResponse _
derive instance repGenericUpdateQueueResponse :: Generic UpdateQueueResponse _
instance showUpdateQueueResponse :: Show UpdateQueueResponse where
  show = genericShow
instance decodeUpdateQueueResponse :: Decode UpdateQueueResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateQueueResponse :: Encode UpdateQueueResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Type of video codec
newtype VideoCodec = VideoCodec String
derive instance newtypeVideoCodec :: Newtype VideoCodec _
derive instance repGenericVideoCodec :: Generic VideoCodec _
instance showVideoCodec :: Show VideoCodec where
  show = genericShow
instance decodeVideoCodec :: Decode VideoCodec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoCodec :: Encode VideoCodec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Video codec settings, (CodecSettings) under (VideoDescription), contains the group of settings related to video encoding. The settings in this group vary depending on the value you choose for Video codec (Codec). For each codec enum you choose, define the corresponding settings object. The following lists the codec enum, settings object pairs. * H_264, H264Settings * H_265, H265Settings * MPEG2, Mpeg2Settings * PRORES, ProresSettings * FRAME_CAPTURE, FrameCaptureSettings
newtype VideoCodecSettings = VideoCodecSettings 
  { "Codec" :: NullOrUndefined.NullOrUndefined (VideoCodec)
  , "FrameCaptureSettings" :: NullOrUndefined.NullOrUndefined (FrameCaptureSettings)
  , "H264Settings" :: NullOrUndefined.NullOrUndefined (H264Settings)
  , "H265Settings" :: NullOrUndefined.NullOrUndefined (H265Settings)
  , "Mpeg2Settings" :: NullOrUndefined.NullOrUndefined (Mpeg2Settings)
  , "ProresSettings" :: NullOrUndefined.NullOrUndefined (ProresSettings)
  }
derive instance newtypeVideoCodecSettings :: Newtype VideoCodecSettings _
derive instance repGenericVideoCodecSettings :: Generic VideoCodecSettings _
instance showVideoCodecSettings :: Show VideoCodecSettings where
  show = genericShow
instance decodeVideoCodecSettings :: Decode VideoCodecSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoCodecSettings :: Encode VideoCodecSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Settings for video outputs
newtype VideoDescription = VideoDescription 
  { "AfdSignaling" :: NullOrUndefined.NullOrUndefined (AfdSignaling)
  , "AntiAlias" :: NullOrUndefined.NullOrUndefined (AntiAlias)
  , "CodecSettings" :: NullOrUndefined.NullOrUndefined (VideoCodecSettings)
  , "ColorMetadata" :: NullOrUndefined.NullOrUndefined (ColorMetadata)
  , "Crop" :: NullOrUndefined.NullOrUndefined (Rectangle)
  , "DropFrameTimecode" :: NullOrUndefined.NullOrUndefined (DropFrameTimecode)
  , "FixedAfd" :: NullOrUndefined.NullOrUndefined (Int)
  , "Height" :: NullOrUndefined.NullOrUndefined (Int)
  , "Position" :: NullOrUndefined.NullOrUndefined (Rectangle)
  , "RespondToAfd" :: NullOrUndefined.NullOrUndefined (RespondToAfd)
  , "ScalingBehavior" :: NullOrUndefined.NullOrUndefined (ScalingBehavior)
  , "Sharpness" :: NullOrUndefined.NullOrUndefined (Int)
  , "TimecodeInsertion" :: NullOrUndefined.NullOrUndefined (VideoTimecodeInsertion)
  , "VideoPreprocessors" :: NullOrUndefined.NullOrUndefined (VideoPreprocessor)
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


-- | Contains details about the output's video stream
newtype VideoDetail = VideoDetail 
  { "HeightInPx" :: NullOrUndefined.NullOrUndefined (Int)
  , "WidthInPx" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeVideoDetail :: Newtype VideoDetail _
derive instance repGenericVideoDetail :: Generic VideoDetail _
instance showVideoDetail :: Show VideoDetail where
  show = genericShow
instance decodeVideoDetail :: Decode VideoDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoDetail :: Encode VideoDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Find additional transcoding features under Preprocessors (VideoPreprocessors). Enable the features at each output individually. These features are disabled by default.
newtype VideoPreprocessor = VideoPreprocessor 
  { "ColorCorrector" :: NullOrUndefined.NullOrUndefined (ColorCorrector)
  , "Deinterlacer" :: NullOrUndefined.NullOrUndefined (Deinterlacer)
  , "ImageInserter" :: NullOrUndefined.NullOrUndefined (ImageInserter)
  , "NoiseReducer" :: NullOrUndefined.NullOrUndefined (NoiseReducer)
  , "TimecodeBurnin" :: NullOrUndefined.NullOrUndefined (TimecodeBurnin)
  }
derive instance newtypeVideoPreprocessor :: Newtype VideoPreprocessor _
derive instance repGenericVideoPreprocessor :: Generic VideoPreprocessor _
instance showVideoPreprocessor :: Show VideoPreprocessor where
  show = genericShow
instance decodeVideoPreprocessor :: Decode VideoPreprocessor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoPreprocessor :: Encode VideoPreprocessor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Selector for video.
newtype VideoSelector = VideoSelector 
  { "ColorSpace" :: NullOrUndefined.NullOrUndefined (ColorSpace)
  , "ColorSpaceUsage" :: NullOrUndefined.NullOrUndefined (ColorSpaceUsage)
  , "Hdr10Metadata" :: NullOrUndefined.NullOrUndefined (Hdr10Metadata)
  , "Pid" :: NullOrUndefined.NullOrUndefined (Int)
  , "ProgramNumber" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeVideoSelector :: Newtype VideoSelector _
derive instance repGenericVideoSelector :: Generic VideoSelector _
instance showVideoSelector :: Show VideoSelector where
  show = genericShow
instance decodeVideoSelector :: Decode VideoSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoSelector :: Encode VideoSelector where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Enable Timecode insertion to include timecode information in this output. Do this in the API by setting (VideoTimecodeInsertion) to (PIC_TIMING_SEI). To get timecodes to appear correctly in your output, also set up the timecode configuration for your job in the input settings. Only enable Timecode insertion when the input framerate is identical to output framerate. Disable this setting to remove the timecode from the output. Default is disabled.
newtype VideoTimecodeInsertion = VideoTimecodeInsertion String
derive instance newtypeVideoTimecodeInsertion :: Newtype VideoTimecodeInsertion _
derive instance repGenericVideoTimecodeInsertion :: Generic VideoTimecodeInsertion _
instance showVideoTimecodeInsertion :: Show VideoTimecodeInsertion where
  show = genericShow
instance decodeVideoTimecodeInsertion :: Decode VideoTimecodeInsertion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoTimecodeInsertion :: Encode VideoTimecodeInsertion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value WAV.
newtype WavSettings = WavSettings 
  { "BitDepth" :: NullOrUndefined.NullOrUndefined (Int)
  , "Channels" :: NullOrUndefined.NullOrUndefined (Int)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeWavSettings :: Newtype WavSettings _
derive instance repGenericWavSettings :: Generic WavSettings _
instance showWavSettings :: Show WavSettings where
  show = genericShow
instance decodeWavSettings :: Decode WavSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWavSettings :: Encode WavSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
