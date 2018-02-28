

-- | <fullname>AWS Elastic Transcoder Service</fullname> <p>The AWS Elastic Transcoder Service.</p>
module AWS.ElasticTranscoder where

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

serviceName = "ElasticTranscoder" :: String


-- | <p>The CancelJob operation cancels an unfinished job.</p> <note> <p>You can only cancel a job that has a status of <code>Submitted</code>. To prevent a pipeline from starting to process a job while you're getting the job identifier, use <a>UpdatePipelineStatus</a> to temporarily pause the pipeline.</p> </note>
cancelJob :: forall eff. CancelJobRequest -> Aff (exception :: EXCEPTION | eff) CancelJobResponse
cancelJob = Request.request serviceName "cancelJob" 


-- | <p>When you create a job, Elastic Transcoder returns JSON data that includes the values that you specified plus information about the job that is created.</p> <p>If you have specified more than one output for your jobs (for example, one output for the Kindle Fire and another output for the Apple iPhone 4s), you currently must use the Elastic Transcoder API to list the jobs (as opposed to the AWS Console).</p>
createJob :: forall eff. CreateJobRequest -> Aff (exception :: EXCEPTION | eff) CreateJobResponse
createJob = Request.request serviceName "createJob" 


-- | <p>The CreatePipeline operation creates a pipeline with settings that you specify.</p>
createPipeline :: forall eff. CreatePipelineRequest -> Aff (exception :: EXCEPTION | eff) CreatePipelineResponse
createPipeline = Request.request serviceName "createPipeline" 


-- | <p>The CreatePreset operation creates a preset with settings that you specify.</p> <important> <p>Elastic Transcoder checks the CreatePreset settings to ensure that they meet Elastic Transcoder requirements and to determine whether they comply with H.264 standards. If your settings are not valid for Elastic Transcoder, Elastic Transcoder returns an HTTP 400 response (<code>ValidationException</code>) and does not create the preset. If the settings are valid for Elastic Transcoder but aren't strictly compliant with the H.264 standard, Elastic Transcoder creates the preset and returns a warning message in the response. This helps you determine whether your settings comply with the H.264 standard while giving you greater flexibility with respect to the video that Elastic Transcoder produces.</p> </important> <p>Elastic Transcoder uses the H.264 video-compression format. For more information, see the International Telecommunication Union publication <i>Recommendation ITU-T H.264: Advanced video coding for generic audiovisual services</i>.</p>
createPreset :: forall eff. CreatePresetRequest -> Aff (exception :: EXCEPTION | eff) CreatePresetResponse
createPreset = Request.request serviceName "createPreset" 


-- | <p>The DeletePipeline operation removes a pipeline.</p> <p> You can only delete a pipeline that has never been used or that is not currently in use (doesn't contain any active jobs). If the pipeline is currently in use, <code>DeletePipeline</code> returns an error. </p>
deletePipeline :: forall eff. DeletePipelineRequest -> Aff (exception :: EXCEPTION | eff) DeletePipelineResponse
deletePipeline = Request.request serviceName "deletePipeline" 


-- | <p>The DeletePreset operation removes a preset that you've added in an AWS region.</p> <note> <p>You can't delete the default presets that are included with Elastic Transcoder.</p> </note>
deletePreset :: forall eff. DeletePresetRequest -> Aff (exception :: EXCEPTION | eff) DeletePresetResponse
deletePreset = Request.request serviceName "deletePreset" 


-- | <p>The ListJobsByPipeline operation gets a list of the jobs currently in a pipeline.</p> <p>Elastic Transcoder returns all of the jobs currently in the specified pipeline. The response body contains one element for each job that satisfies the search criteria.</p>
listJobsByPipeline :: forall eff. ListJobsByPipelineRequest -> Aff (exception :: EXCEPTION | eff) ListJobsByPipelineResponse
listJobsByPipeline = Request.request serviceName "listJobsByPipeline" 


-- | <p>The ListJobsByStatus operation gets a list of jobs that have a specified status. The response body contains one element for each job that satisfies the search criteria.</p>
listJobsByStatus :: forall eff. ListJobsByStatusRequest -> Aff (exception :: EXCEPTION | eff) ListJobsByStatusResponse
listJobsByStatus = Request.request serviceName "listJobsByStatus" 


-- | <p>The ListPipelines operation gets a list of the pipelines associated with the current AWS account.</p>
listPipelines :: forall eff. ListPipelinesRequest -> Aff (exception :: EXCEPTION | eff) ListPipelinesResponse
listPipelines = Request.request serviceName "listPipelines" 


-- | <p>The ListPresets operation gets a list of the default presets included with Elastic Transcoder and the presets that you've added in an AWS region.</p>
listPresets :: forall eff. ListPresetsRequest -> Aff (exception :: EXCEPTION | eff) ListPresetsResponse
listPresets = Request.request serviceName "listPresets" 


-- | <p>The ReadJob operation returns detailed information about a job.</p>
readJob :: forall eff. ReadJobRequest -> Aff (exception :: EXCEPTION | eff) ReadJobResponse
readJob = Request.request serviceName "readJob" 


-- | <p>The ReadPipeline operation gets detailed information about a pipeline.</p>
readPipeline :: forall eff. ReadPipelineRequest -> Aff (exception :: EXCEPTION | eff) ReadPipelineResponse
readPipeline = Request.request serviceName "readPipeline" 


-- | <p>The ReadPreset operation gets detailed information about a preset.</p>
readPreset :: forall eff. ReadPresetRequest -> Aff (exception :: EXCEPTION | eff) ReadPresetResponse
readPreset = Request.request serviceName "readPreset" 


-- | <p>The TestRole operation tests the IAM role used to create the pipeline.</p> <p>The <code>TestRole</code> action lets you determine whether the IAM role you are using has sufficient permissions to let Elastic Transcoder perform tasks associated with the transcoding process. The action attempts to assume the specified IAM role, checks read access to the input and output buckets, and tries to send a test notification to Amazon SNS topics that you specify.</p>
testRole :: forall eff. TestRoleRequest -> Aff (exception :: EXCEPTION | eff) TestRoleResponse
testRole = Request.request serviceName "testRole" 


-- | <p> Use the <code>UpdatePipeline</code> operation to update settings for a pipeline.</p> <important> <p>When you change pipeline settings, your changes take effect immediately. Jobs that you have already submitted and that Elastic Transcoder has not started to process are affected in addition to jobs that you submit after you change settings. </p> </important>
updatePipeline :: forall eff. UpdatePipelineRequest -> Aff (exception :: EXCEPTION | eff) UpdatePipelineResponse
updatePipeline = Request.request serviceName "updatePipeline" 


-- | <p>With the UpdatePipelineNotifications operation, you can update Amazon Simple Notification Service (Amazon SNS) notifications for a pipeline.</p> <p>When you update notifications for a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>
updatePipelineNotifications :: forall eff. UpdatePipelineNotificationsRequest -> Aff (exception :: EXCEPTION | eff) UpdatePipelineNotificationsResponse
updatePipelineNotifications = Request.request serviceName "updatePipelineNotifications" 


-- | <p>The UpdatePipelineStatus operation pauses or reactivates a pipeline, so that the pipeline stops or restarts the processing of jobs.</p> <p>Changing the pipeline status is useful if you want to cancel one or more jobs. You can't cancel jobs after Elastic Transcoder has started processing them; if you pause the pipeline to which you submitted the jobs, you have more time to get the job IDs for the jobs that you want to cancel, and to send a <a>CancelJob</a> request. </p>
updatePipelineStatus :: forall eff. UpdatePipelineStatusRequest -> Aff (exception :: EXCEPTION | eff) UpdatePipelineStatusResponse
updatePipelineStatus = Request.request serviceName "updatePipelineStatus" 


newtype AccessControl = AccessControl String
derive instance newtypeAccessControl :: Newtype AccessControl _
derive instance repGenericAccessControl :: Generic AccessControl _
instance showAccessControl :: Show AccessControl where
  show = genericShow
instance decodeAccessControl :: Decode AccessControl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessControl :: Encode AccessControl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessControls = AccessControls (Array AccessControl)
derive instance newtypeAccessControls :: Newtype AccessControls _
derive instance repGenericAccessControls :: Generic AccessControls _
instance showAccessControls :: Show AccessControls where
  show = genericShow
instance decodeAccessControls :: Decode AccessControls where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessControls :: Encode AccessControls where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>General authentication failure. The request was not signed correctly.</p>
newtype AccessDeniedException = AccessDeniedException Types.NoArguments
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _
derive instance repGenericAccessDeniedException :: Generic AccessDeniedException _
instance showAccessDeniedException :: Show AccessDeniedException where
  show = genericShow
instance decodeAccessDeniedException :: Decode AccessDeniedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDeniedException :: Encode AccessDeniedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20.</p> <p>To remove artwork or leave the artwork empty, you can either set <code>Artwork</code> to null, or set the <code>Merge Policy</code> to "Replace" and use an empty <code>Artwork</code> array.</p> <p>To pass through existing artwork unchanged, set the <code>Merge Policy</code> to "Prepend", "Append", or "Fallback", and use an empty <code>Artwork</code> array.</p>
newtype Artwork = Artwork 
  { "InputKey" :: NullOrUndefined.NullOrUndefined (WatermarkKey)
  , "MaxWidth" :: NullOrUndefined.NullOrUndefined (DigitsOrAuto)
  , "MaxHeight" :: NullOrUndefined.NullOrUndefined (DigitsOrAuto)
  , "SizingPolicy" :: NullOrUndefined.NullOrUndefined (SizingPolicy)
  , "PaddingPolicy" :: NullOrUndefined.NullOrUndefined (PaddingPolicy)
  , "AlbumArtFormat" :: NullOrUndefined.NullOrUndefined (JpgOrPng)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  }
derive instance newtypeArtwork :: Newtype Artwork _
derive instance repGenericArtwork :: Generic Artwork _
instance showArtwork :: Show Artwork where
  show = genericShow
instance decodeArtwork :: Decode Artwork where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtwork :: Encode Artwork where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Artworks = Artworks (Array Artwork)
derive instance newtypeArtworks :: Newtype Artworks _
derive instance repGenericArtworks :: Generic Artworks _
instance showArtworks :: Show Artworks where
  show = genericShow
instance decodeArtworks :: Decode Artworks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtworks :: Encode Artworks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Ascending = Ascending String
derive instance newtypeAscending :: Newtype Ascending _
derive instance repGenericAscending :: Generic Ascending _
instance showAscending :: Show Ascending where
  show = genericShow
instance decodeAscending :: Decode Ascending where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAscending :: Encode Ascending where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AspectRatio = AspectRatio String
derive instance newtypeAspectRatio :: Newtype AspectRatio _
derive instance repGenericAspectRatio :: Generic AspectRatio _
instance showAspectRatio :: Show AspectRatio where
  show = genericShow
instance decodeAspectRatio :: Decode AspectRatio where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAspectRatio :: Encode AspectRatio where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioBitDepth = AudioBitDepth String
derive instance newtypeAudioBitDepth :: Newtype AudioBitDepth _
derive instance repGenericAudioBitDepth :: Generic AudioBitDepth _
instance showAudioBitDepth :: Show AudioBitDepth where
  show = genericShow
instance decodeAudioBitDepth :: Decode AudioBitDepth where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioBitDepth :: Encode AudioBitDepth where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioBitOrder = AudioBitOrder String
derive instance newtypeAudioBitOrder :: Newtype AudioBitOrder _
derive instance repGenericAudioBitOrder :: Generic AudioBitOrder _
instance showAudioBitOrder :: Show AudioBitOrder where
  show = genericShow
instance decodeAudioBitOrder :: Decode AudioBitOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioBitOrder :: Encode AudioBitOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioBitRate = AudioBitRate String
derive instance newtypeAudioBitRate :: Newtype AudioBitRate _
derive instance repGenericAudioBitRate :: Generic AudioBitRate _
instance showAudioBitRate :: Show AudioBitRate where
  show = genericShow
instance decodeAudioBitRate :: Decode AudioBitRate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioBitRate :: Encode AudioBitRate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioChannels = AudioChannels String
derive instance newtypeAudioChannels :: Newtype AudioChannels _
derive instance repGenericAudioChannels :: Generic AudioChannels _
instance showAudioChannels :: Show AudioChannels where
  show = genericShow
instance decodeAudioChannels :: Decode AudioChannels where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioChannels :: Encode AudioChannels where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioCodec = AudioCodec String
derive instance newtypeAudioCodec :: Newtype AudioCodec _
derive instance repGenericAudioCodec :: Generic AudioCodec _
instance showAudioCodec :: Show AudioCodec where
  show = genericShow
instance decodeAudioCodec :: Decode AudioCodec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioCodec :: Encode AudioCodec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Options associated with your audio codec.</p>
newtype AudioCodecOptions = AudioCodecOptions 
  { "Profile" :: NullOrUndefined.NullOrUndefined (AudioCodecProfile)
  , "BitDepth" :: NullOrUndefined.NullOrUndefined (AudioBitDepth)
  , "BitOrder" :: NullOrUndefined.NullOrUndefined (AudioBitOrder)
  , "Signed" :: NullOrUndefined.NullOrUndefined (AudioSigned)
  }
derive instance newtypeAudioCodecOptions :: Newtype AudioCodecOptions _
derive instance repGenericAudioCodecOptions :: Generic AudioCodecOptions _
instance showAudioCodecOptions :: Show AudioCodecOptions where
  show = genericShow
instance decodeAudioCodecOptions :: Decode AudioCodecOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioCodecOptions :: Encode AudioCodecOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioCodecProfile = AudioCodecProfile String
derive instance newtypeAudioCodecProfile :: Newtype AudioCodecProfile _
derive instance repGenericAudioCodecProfile :: Generic AudioCodecProfile _
instance showAudioCodecProfile :: Show AudioCodecProfile where
  show = genericShow
instance decodeAudioCodecProfile :: Decode AudioCodecProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioCodecProfile :: Encode AudioCodecProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioPackingMode = AudioPackingMode String
derive instance newtypeAudioPackingMode :: Newtype AudioPackingMode _
derive instance repGenericAudioPackingMode :: Generic AudioPackingMode _
instance showAudioPackingMode :: Show AudioPackingMode where
  show = genericShow
instance decodeAudioPackingMode :: Decode AudioPackingMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioPackingMode :: Encode AudioPackingMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Parameters required for transcoding audio.</p>
newtype AudioParameters = AudioParameters 
  { "Codec" :: NullOrUndefined.NullOrUndefined (AudioCodec)
  , "SampleRate" :: NullOrUndefined.NullOrUndefined (AudioSampleRate)
  , "BitRate" :: NullOrUndefined.NullOrUndefined (AudioBitRate)
  , "Channels" :: NullOrUndefined.NullOrUndefined (AudioChannels)
  , "AudioPackingMode" :: NullOrUndefined.NullOrUndefined (AudioPackingMode)
  , "CodecOptions" :: NullOrUndefined.NullOrUndefined (AudioCodecOptions)
  }
derive instance newtypeAudioParameters :: Newtype AudioParameters _
derive instance repGenericAudioParameters :: Generic AudioParameters _
instance showAudioParameters :: Show AudioParameters where
  show = genericShow
instance decodeAudioParameters :: Decode AudioParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioParameters :: Encode AudioParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioSampleRate = AudioSampleRate String
derive instance newtypeAudioSampleRate :: Newtype AudioSampleRate _
derive instance repGenericAudioSampleRate :: Generic AudioSampleRate _
instance showAudioSampleRate :: Show AudioSampleRate where
  show = genericShow
instance decodeAudioSampleRate :: Decode AudioSampleRate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioSampleRate :: Encode AudioSampleRate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AudioSigned = AudioSigned String
derive instance newtypeAudioSigned :: Newtype AudioSigned _
derive instance repGenericAudioSigned :: Generic AudioSigned _
instance showAudioSigned :: Show AudioSigned where
  show = genericShow
instance decodeAudioSigned :: Decode AudioSigned where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAudioSigned :: Encode AudioSigned where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Base64EncodedString = Base64EncodedString String
derive instance newtypeBase64EncodedString :: Newtype Base64EncodedString _
derive instance repGenericBase64EncodedString :: Generic Base64EncodedString _
instance showBase64EncodedString :: Show Base64EncodedString where
  show = genericShow
instance decodeBase64EncodedString :: Decode Base64EncodedString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBase64EncodedString :: Encode Base64EncodedString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketName = BucketName String
derive instance newtypeBucketName :: Newtype BucketName _
derive instance repGenericBucketName :: Generic BucketName _
instance showBucketName :: Show BucketName where
  show = genericShow
instance decodeBucketName :: Decode BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketName :: Encode BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>CancelJobRequest</code> structure.</p>
newtype CancelJobRequest = CancelJobRequest 
  { "Id" :: (Id)
  }
derive instance newtypeCancelJobRequest :: Newtype CancelJobRequest _
derive instance repGenericCancelJobRequest :: Generic CancelJobRequest _
instance showCancelJobRequest :: Show CancelJobRequest where
  show = genericShow
instance decodeCancelJobRequest :: Decode CancelJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobRequest :: Encode CancelJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The response body contains a JSON object. If the job is successfully canceled, the value of <code>Success</code> is <code>true</code>.</p>
newtype CancelJobResponse = CancelJobResponse Types.NoArguments
derive instance newtypeCancelJobResponse :: Newtype CancelJobResponse _
derive instance repGenericCancelJobResponse :: Generic CancelJobResponse _
instance showCancelJobResponse :: Show CancelJobResponse where
  show = genericShow
instance decodeCancelJobResponse :: Decode CancelJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobResponse :: Encode CancelJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The file format of the output captions. If you leave this value blank, Elastic Transcoder returns an error.</p>
newtype CaptionFormat = CaptionFormat 
  { "Format" :: NullOrUndefined.NullOrUndefined (CaptionFormatFormat)
  , "Pattern" :: NullOrUndefined.NullOrUndefined (CaptionFormatPattern)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  }
derive instance newtypeCaptionFormat :: Newtype CaptionFormat _
derive instance repGenericCaptionFormat :: Generic CaptionFormat _
instance showCaptionFormat :: Show CaptionFormat where
  show = genericShow
instance decodeCaptionFormat :: Decode CaptionFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionFormat :: Encode CaptionFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CaptionFormatFormat = CaptionFormatFormat String
derive instance newtypeCaptionFormatFormat :: Newtype CaptionFormatFormat _
derive instance repGenericCaptionFormatFormat :: Generic CaptionFormatFormat _
instance showCaptionFormatFormat :: Show CaptionFormatFormat where
  show = genericShow
instance decodeCaptionFormatFormat :: Decode CaptionFormatFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionFormatFormat :: Encode CaptionFormatFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CaptionFormatPattern = CaptionFormatPattern String
derive instance newtypeCaptionFormatPattern :: Newtype CaptionFormatPattern _
derive instance repGenericCaptionFormatPattern :: Generic CaptionFormatPattern _
instance showCaptionFormatPattern :: Show CaptionFormatPattern where
  show = genericShow
instance decodeCaptionFormatPattern :: Decode CaptionFormatPattern where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionFormatPattern :: Encode CaptionFormatPattern where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CaptionFormats = CaptionFormats (Array CaptionFormat)
derive instance newtypeCaptionFormats :: Newtype CaptionFormats _
derive instance repGenericCaptionFormats :: Generic CaptionFormats _
instance showCaptionFormats :: Show CaptionFormats where
  show = genericShow
instance decodeCaptionFormats :: Decode CaptionFormats where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionFormats :: Encode CaptionFormats where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CaptionMergePolicy = CaptionMergePolicy String
derive instance newtypeCaptionMergePolicy :: Newtype CaptionMergePolicy _
derive instance repGenericCaptionMergePolicy :: Generic CaptionMergePolicy _
instance showCaptionMergePolicy :: Show CaptionMergePolicy where
  show = genericShow
instance decodeCaptionMergePolicy :: Decode CaptionMergePolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionMergePolicy :: Encode CaptionMergePolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A source file for the input sidecar captions used during the transcoding process.</p>
newtype CaptionSource = CaptionSource 
  { "Key" :: NullOrUndefined.NullOrUndefined (LongKey)
  , "Language" :: NullOrUndefined.NullOrUndefined (Key)
  , "TimeOffset" :: NullOrUndefined.NullOrUndefined (TimeOffset)
  , "Label" :: NullOrUndefined.NullOrUndefined (Name)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  }
derive instance newtypeCaptionSource :: Newtype CaptionSource _
derive instance repGenericCaptionSource :: Generic CaptionSource _
instance showCaptionSource :: Show CaptionSource where
  show = genericShow
instance decodeCaptionSource :: Decode CaptionSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionSource :: Encode CaptionSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CaptionSources = CaptionSources (Array CaptionSource)
derive instance newtypeCaptionSources :: Newtype CaptionSources _
derive instance repGenericCaptionSources :: Generic CaptionSources _
instance showCaptionSources :: Show CaptionSources where
  show = genericShow
instance decodeCaptionSources :: Decode CaptionSources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptionSources :: Encode CaptionSources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The captions to be created, if any.</p>
newtype Captions = Captions 
  { "MergePolicy" :: NullOrUndefined.NullOrUndefined (CaptionMergePolicy)
  , "CaptionSources" :: NullOrUndefined.NullOrUndefined (CaptionSources)
  , "CaptionFormats" :: NullOrUndefined.NullOrUndefined (CaptionFormats)
  }
derive instance newtypeCaptions :: Newtype Captions _
derive instance repGenericCaptions :: Generic Captions _
instance showCaptions :: Show Captions where
  show = genericShow
instance decodeCaptions :: Decode Captions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCaptions :: Encode Captions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Settings for one clip in a composition. All jobs in a playlist must have the same clip settings.</p>
newtype Clip = Clip 
  { "TimeSpan" :: NullOrUndefined.NullOrUndefined (TimeSpan)
  }
derive instance newtypeClip :: Newtype Clip _
derive instance repGenericClip :: Generic Clip _
instance showClip :: Show Clip where
  show = genericShow
instance decodeClip :: Decode Clip where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClip :: Encode Clip where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CodecOption = CodecOption String
derive instance newtypeCodecOption :: Newtype CodecOption _
derive instance repGenericCodecOption :: Generic CodecOption _
instance showCodecOption :: Show CodecOption where
  show = genericShow
instance decodeCodecOption :: Decode CodecOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodecOption :: Encode CodecOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CodecOptions = CodecOptions (StrMap.StrMap CodecOption)
derive instance newtypeCodecOptions :: Newtype CodecOptions _
derive instance repGenericCodecOptions :: Generic CodecOptions _
instance showCodecOptions :: Show CodecOptions where
  show = genericShow
instance decodeCodecOptions :: Decode CodecOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodecOptions :: Encode CodecOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Composition = Composition (Array Clip)
derive instance newtypeComposition :: Newtype Composition _
derive instance repGenericComposition :: Generic Composition _
instance showComposition :: Show Composition where
  show = genericShow
instance decodeComposition :: Decode Composition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComposition :: Encode Composition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>CreateJobOutput</code> structure.</p>
newtype CreateJobOutput = CreateJobOutput 
  { "Key" :: NullOrUndefined.NullOrUndefined (Key)
  , "ThumbnailPattern" :: NullOrUndefined.NullOrUndefined (ThumbnailPattern)
  , "ThumbnailEncryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  , "Rotate" :: NullOrUndefined.NullOrUndefined (Rotate)
  , "PresetId" :: NullOrUndefined.NullOrUndefined (Id)
  , "SegmentDuration" :: NullOrUndefined.NullOrUndefined (FloatString)
  , "Watermarks" :: NullOrUndefined.NullOrUndefined (JobWatermarks)
  , "AlbumArt" :: NullOrUndefined.NullOrUndefined (JobAlbumArt)
  , "Composition" :: NullOrUndefined.NullOrUndefined (Composition)
  , "Captions" :: NullOrUndefined.NullOrUndefined (Captions)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  }
derive instance newtypeCreateJobOutput :: Newtype CreateJobOutput _
derive instance repGenericCreateJobOutput :: Generic CreateJobOutput _
instance showCreateJobOutput :: Show CreateJobOutput where
  show = genericShow
instance decodeCreateJobOutput :: Decode CreateJobOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobOutput :: Encode CreateJobOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobOutputs = CreateJobOutputs (Array CreateJobOutput)
derive instance newtypeCreateJobOutputs :: Newtype CreateJobOutputs _
derive instance repGenericCreateJobOutputs :: Generic CreateJobOutputs _
instance showCreateJobOutputs :: Show CreateJobOutputs where
  show = genericShow
instance decodeCreateJobOutputs :: Decode CreateJobOutputs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobOutputs :: Encode CreateJobOutputs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the master playlist.</p>
newtype CreateJobPlaylist = CreateJobPlaylist 
  { "Name" :: NullOrUndefined.NullOrUndefined (Filename)
  , "Format" :: NullOrUndefined.NullOrUndefined (PlaylistFormat)
  , "OutputKeys" :: NullOrUndefined.NullOrUndefined (OutputKeys)
  , "HlsContentProtection" :: NullOrUndefined.NullOrUndefined (HlsContentProtection)
  , "PlayReadyDrm" :: NullOrUndefined.NullOrUndefined (PlayReadyDrm)
  }
derive instance newtypeCreateJobPlaylist :: Newtype CreateJobPlaylist _
derive instance repGenericCreateJobPlaylist :: Generic CreateJobPlaylist _
instance showCreateJobPlaylist :: Show CreateJobPlaylist where
  show = genericShow
instance decodeCreateJobPlaylist :: Decode CreateJobPlaylist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobPlaylist :: Encode CreateJobPlaylist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobPlaylists = CreateJobPlaylists (Array CreateJobPlaylist)
derive instance newtypeCreateJobPlaylists :: Newtype CreateJobPlaylists _
derive instance repGenericCreateJobPlaylists :: Generic CreateJobPlaylists _
instance showCreateJobPlaylists :: Show CreateJobPlaylists where
  show = genericShow
instance decodeCreateJobPlaylists :: Decode CreateJobPlaylists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobPlaylists :: Encode CreateJobPlaylists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>CreateJobRequest</code> structure.</p>
newtype CreateJobRequest = CreateJobRequest 
  { "PipelineId" :: (Id)
  , "Input" :: NullOrUndefined.NullOrUndefined (JobInput)
  , "Inputs" :: NullOrUndefined.NullOrUndefined (JobInputs)
  , "Output" :: NullOrUndefined.NullOrUndefined (CreateJobOutput)
  , "Outputs" :: NullOrUndefined.NullOrUndefined (CreateJobOutputs)
  , "OutputKeyPrefix" :: NullOrUndefined.NullOrUndefined (Key)
  , "Playlists" :: NullOrUndefined.NullOrUndefined (CreateJobPlaylists)
  , "UserMetadata" :: NullOrUndefined.NullOrUndefined (UserMetadata)
  }
derive instance newtypeCreateJobRequest :: Newtype CreateJobRequest _
derive instance repGenericCreateJobRequest :: Generic CreateJobRequest _
instance showCreateJobRequest :: Show CreateJobRequest where
  show = genericShow
instance decodeCreateJobRequest :: Decode CreateJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobRequest :: Encode CreateJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The CreateJobResponse structure.</p>
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


-- | <p>The <code>CreatePipelineRequest</code> structure.</p>
newtype CreatePipelineRequest = CreatePipelineRequest 
  { "Name" :: (Name)
  , "InputBucket" :: (BucketName)
  , "OutputBucket" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "Role" :: (Role)
  , "AwsKmsKeyArn" :: NullOrUndefined.NullOrUndefined (KeyArn)
  , "Notifications" :: NullOrUndefined.NullOrUndefined (Notifications)
  , "ContentConfig" :: NullOrUndefined.NullOrUndefined (PipelineOutputConfig)
  , "ThumbnailConfig" :: NullOrUndefined.NullOrUndefined (PipelineOutputConfig)
  }
derive instance newtypeCreatePipelineRequest :: Newtype CreatePipelineRequest _
derive instance repGenericCreatePipelineRequest :: Generic CreatePipelineRequest _
instance showCreatePipelineRequest :: Show CreatePipelineRequest where
  show = genericShow
instance decodeCreatePipelineRequest :: Decode CreatePipelineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePipelineRequest :: Encode CreatePipelineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When you create a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>
newtype CreatePipelineResponse = CreatePipelineResponse 
  { "Pipeline" :: NullOrUndefined.NullOrUndefined (Pipeline)
  , "Warnings" :: NullOrUndefined.NullOrUndefined (Warnings)
  }
derive instance newtypeCreatePipelineResponse :: Newtype CreatePipelineResponse _
derive instance repGenericCreatePipelineResponse :: Generic CreatePipelineResponse _
instance showCreatePipelineResponse :: Show CreatePipelineResponse where
  show = genericShow
instance decodeCreatePipelineResponse :: Decode CreatePipelineResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePipelineResponse :: Encode CreatePipelineResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>CreatePresetRequest</code> structure.</p>
newtype CreatePresetRequest = CreatePresetRequest 
  { "Name" :: (Name)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Container" :: (PresetContainer)
  , "Video" :: NullOrUndefined.NullOrUndefined (VideoParameters)
  , "Audio" :: NullOrUndefined.NullOrUndefined (AudioParameters)
  , "Thumbnails" :: NullOrUndefined.NullOrUndefined (Thumbnails)
  }
derive instance newtypeCreatePresetRequest :: Newtype CreatePresetRequest _
derive instance repGenericCreatePresetRequest :: Generic CreatePresetRequest _
instance showCreatePresetRequest :: Show CreatePresetRequest where
  show = genericShow
instance decodeCreatePresetRequest :: Decode CreatePresetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePresetRequest :: Encode CreatePresetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>CreatePresetResponse</code> structure.</p>
newtype CreatePresetResponse = CreatePresetResponse 
  { "Preset" :: NullOrUndefined.NullOrUndefined (Preset)
  , "Warning" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreatePresetResponse :: Newtype CreatePresetResponse _
derive instance repGenericCreatePresetResponse :: Generic CreatePresetResponse _
instance showCreatePresetResponse :: Show CreatePresetResponse where
  show = genericShow
instance decodeCreatePresetResponse :: Decode CreatePresetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePresetResponse :: Encode CreatePresetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>DeletePipelineRequest</code> structure.</p>
newtype DeletePipelineRequest = DeletePipelineRequest 
  { "Id" :: (Id)
  }
derive instance newtypeDeletePipelineRequest :: Newtype DeletePipelineRequest _
derive instance repGenericDeletePipelineRequest :: Generic DeletePipelineRequest _
instance showDeletePipelineRequest :: Show DeletePipelineRequest where
  show = genericShow
instance decodeDeletePipelineRequest :: Decode DeletePipelineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePipelineRequest :: Encode DeletePipelineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>DeletePipelineResponse</code> structure.</p>
newtype DeletePipelineResponse = DeletePipelineResponse Types.NoArguments
derive instance newtypeDeletePipelineResponse :: Newtype DeletePipelineResponse _
derive instance repGenericDeletePipelineResponse :: Generic DeletePipelineResponse _
instance showDeletePipelineResponse :: Show DeletePipelineResponse where
  show = genericShow
instance decodeDeletePipelineResponse :: Decode DeletePipelineResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePipelineResponse :: Encode DeletePipelineResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>DeletePresetRequest</code> structure.</p>
newtype DeletePresetRequest = DeletePresetRequest 
  { "Id" :: (Id)
  }
derive instance newtypeDeletePresetRequest :: Newtype DeletePresetRequest _
derive instance repGenericDeletePresetRequest :: Generic DeletePresetRequest _
instance showDeletePresetRequest :: Show DeletePresetRequest where
  show = genericShow
instance decodeDeletePresetRequest :: Decode DeletePresetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePresetRequest :: Encode DeletePresetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>DeletePresetResponse</code> structure.</p>
newtype DeletePresetResponse = DeletePresetResponse Types.NoArguments
derive instance newtypeDeletePresetResponse :: Newtype DeletePresetResponse _
derive instance repGenericDeletePresetResponse :: Generic DeletePresetResponse _
instance showDeletePresetResponse :: Show DeletePresetResponse where
  show = genericShow
instance decodeDeletePresetResponse :: Decode DeletePresetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePresetResponse :: Encode DeletePresetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The detected properties of the input file. Elastic Transcoder identifies these values from the input file.</p>
newtype DetectedProperties = DetectedProperties 
  { "Width" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Height" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "FrameRate" :: NullOrUndefined.NullOrUndefined (FloatString)
  , "FileSize" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "DurationMillis" :: NullOrUndefined.NullOrUndefined (NullableLong)
  }
derive instance newtypeDetectedProperties :: Newtype DetectedProperties _
derive instance repGenericDetectedProperties :: Generic DetectedProperties _
instance showDetectedProperties :: Show DetectedProperties where
  show = genericShow
instance decodeDetectedProperties :: Decode DetectedProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectedProperties :: Encode DetectedProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Digits = Digits String
derive instance newtypeDigits :: Newtype Digits _
derive instance repGenericDigits :: Generic Digits _
instance showDigits :: Show Digits where
  show = genericShow
instance decodeDigits :: Decode Digits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDigits :: Encode Digits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DigitsOrAuto = DigitsOrAuto String
derive instance newtypeDigitsOrAuto :: Newtype DigitsOrAuto _
derive instance repGenericDigitsOrAuto :: Generic DigitsOrAuto _
instance showDigitsOrAuto :: Show DigitsOrAuto where
  show = genericShow
instance decodeDigitsOrAuto :: Decode DigitsOrAuto where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDigitsOrAuto :: Encode DigitsOrAuto where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The encryption settings, if any, that are used for decrypting your input files or encrypting your output files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file, otherwise you must specify the mode you want Elastic Transcoder to use to encrypt your output files.</p>
newtype Encryption = Encryption 
  { "Mode" :: NullOrUndefined.NullOrUndefined (EncryptionMode)
  , "Key" :: NullOrUndefined.NullOrUndefined (Base64EncodedString)
  , "KeyMd5" :: NullOrUndefined.NullOrUndefined (Base64EncodedString)
  , "InitializationVector" :: NullOrUndefined.NullOrUndefined (ZeroTo255String)
  }
derive instance newtypeEncryption :: Newtype Encryption _
derive instance repGenericEncryption :: Generic Encryption _
instance showEncryption :: Show Encryption where
  show = genericShow
instance decodeEncryption :: Decode Encryption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryption :: Encode Encryption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptionMode = EncryptionMode String
derive instance newtypeEncryptionMode :: Newtype EncryptionMode _
derive instance repGenericEncryptionMode :: Generic EncryptionMode _
instance showEncryptionMode :: Show EncryptionMode where
  show = genericShow
instance decodeEncryptionMode :: Decode EncryptionMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionMode :: Encode EncryptionMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExceptionMessages = ExceptionMessages (Array String)
derive instance newtypeExceptionMessages :: Newtype ExceptionMessages _
derive instance repGenericExceptionMessages :: Generic ExceptionMessages _
instance showExceptionMessages :: Show ExceptionMessages where
  show = genericShow
instance decodeExceptionMessages :: Decode ExceptionMessages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExceptionMessages :: Encode ExceptionMessages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Filename = Filename String
derive instance newtypeFilename :: Newtype Filename _
derive instance repGenericFilename :: Generic Filename _
instance showFilename :: Show Filename where
  show = genericShow
instance decodeFilename :: Decode Filename where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilename :: Encode Filename where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FixedGOP = FixedGOP String
derive instance newtypeFixedGOP :: Newtype FixedGOP _
derive instance repGenericFixedGOP :: Generic FixedGOP _
instance showFixedGOP :: Show FixedGOP where
  show = genericShow
instance decodeFixedGOP :: Decode FixedGOP where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFixedGOP :: Encode FixedGOP where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FloatString = FloatString String
derive instance newtypeFloatString :: Newtype FloatString _
derive instance repGenericFloatString :: Generic FloatString _
instance showFloatString :: Show FloatString where
  show = genericShow
instance decodeFloatString :: Decode FloatString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFloatString :: Encode FloatString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FrameRate = FrameRate String
derive instance newtypeFrameRate :: Newtype FrameRate _
derive instance repGenericFrameRate :: Generic FrameRate _
instance showFrameRate :: Show FrameRate where
  show = genericShow
instance decodeFrameRate :: Decode FrameRate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFrameRate :: Encode FrameRate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Grantee = Grantee String
derive instance newtypeGrantee :: Newtype Grantee _
derive instance repGenericGrantee :: Generic Grantee _
instance showGrantee :: Show Grantee where
  show = genericShow
instance decodeGrantee :: Decode Grantee where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrantee :: Encode Grantee where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GranteeType = GranteeType String
derive instance newtypeGranteeType :: Newtype GranteeType _
derive instance repGenericGranteeType :: Generic GranteeType _
instance showGranteeType :: Show GranteeType where
  show = genericShow
instance decodeGranteeType :: Decode GranteeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGranteeType :: Encode GranteeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The HLS content protection settings, if any, that you want Elastic Transcoder to apply to your output files.</p>
newtype HlsContentProtection = HlsContentProtection 
  { "Method" :: NullOrUndefined.NullOrUndefined (HlsContentProtectionMethod)
  , "Key" :: NullOrUndefined.NullOrUndefined (Base64EncodedString)
  , "KeyMd5" :: NullOrUndefined.NullOrUndefined (Base64EncodedString)
  , "InitializationVector" :: NullOrUndefined.NullOrUndefined (ZeroTo255String)
  , "LicenseAcquisitionUrl" :: NullOrUndefined.NullOrUndefined (ZeroTo512String)
  , "KeyStoragePolicy" :: NullOrUndefined.NullOrUndefined (KeyStoragePolicy)
  }
derive instance newtypeHlsContentProtection :: Newtype HlsContentProtection _
derive instance repGenericHlsContentProtection :: Generic HlsContentProtection _
instance showHlsContentProtection :: Show HlsContentProtection where
  show = genericShow
instance decodeHlsContentProtection :: Decode HlsContentProtection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsContentProtection :: Encode HlsContentProtection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HlsContentProtectionMethod = HlsContentProtectionMethod String
derive instance newtypeHlsContentProtectionMethod :: Newtype HlsContentProtectionMethod _
derive instance repGenericHlsContentProtectionMethod :: Generic HlsContentProtectionMethod _
instance showHlsContentProtectionMethod :: Show HlsContentProtectionMethod where
  show = genericShow
instance decodeHlsContentProtectionMethod :: Decode HlsContentProtectionMethod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHlsContentProtectionMethod :: Encode HlsContentProtectionMethod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HorizontalAlign = HorizontalAlign String
derive instance newtypeHorizontalAlign :: Newtype HorizontalAlign _
derive instance repGenericHorizontalAlign :: Generic HorizontalAlign _
instance showHorizontalAlign :: Show HorizontalAlign where
  show = genericShow
instance decodeHorizontalAlign :: Decode HorizontalAlign where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHorizontalAlign :: Encode HorizontalAlign where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Id = Id String
derive instance newtypeId :: Newtype Id _
derive instance repGenericId :: Generic Id _
instance showId :: Show Id where
  show = genericShow
instance decodeId :: Decode Id where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeId :: Encode Id where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IncompatibleVersionException = IncompatibleVersionException Types.NoArguments
derive instance newtypeIncompatibleVersionException :: Newtype IncompatibleVersionException _
derive instance repGenericIncompatibleVersionException :: Generic IncompatibleVersionException _
instance showIncompatibleVersionException :: Show IncompatibleVersionException where
  show = genericShow
instance decodeIncompatibleVersionException :: Decode IncompatibleVersionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncompatibleVersionException :: Encode IncompatibleVersionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The captions to be created, if any.</p>
newtype InputCaptions = InputCaptions 
  { "MergePolicy" :: NullOrUndefined.NullOrUndefined (CaptionMergePolicy)
  , "CaptionSources" :: NullOrUndefined.NullOrUndefined (CaptionSources)
  }
derive instance newtypeInputCaptions :: Newtype InputCaptions _
derive instance repGenericInputCaptions :: Generic InputCaptions _
instance showInputCaptions :: Show InputCaptions where
  show = genericShow
instance decodeInputCaptions :: Decode InputCaptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputCaptions :: Encode InputCaptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Interlaced = Interlaced String
derive instance newtypeInterlaced :: Newtype Interlaced _
derive instance repGenericInterlaced :: Generic Interlaced _
instance showInterlaced :: Show Interlaced where
  show = genericShow
instance decodeInterlaced :: Decode Interlaced where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInterlaced :: Encode Interlaced where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Elastic Transcoder encountered an unexpected exception while trying to fulfill the request.</p>
newtype InternalServiceException = InternalServiceException Types.NoArguments
derive instance newtypeInternalServiceException :: Newtype InternalServiceException _
derive instance repGenericInternalServiceException :: Generic InternalServiceException _
instance showInternalServiceException :: Show InternalServiceException where
  show = genericShow
instance decodeInternalServiceException :: Decode InternalServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServiceException :: Encode InternalServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A section of the response body that provides information about the job that is created.</p>
newtype Job = Job 
  { "Id" :: NullOrUndefined.NullOrUndefined (Id)
  , "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "PipelineId" :: NullOrUndefined.NullOrUndefined (Id)
  , "Input" :: NullOrUndefined.NullOrUndefined (JobInput)
  , "Inputs" :: NullOrUndefined.NullOrUndefined (JobInputs)
  , "Output" :: NullOrUndefined.NullOrUndefined (JobOutput)
  , "Outputs" :: NullOrUndefined.NullOrUndefined (JobOutputs)
  , "OutputKeyPrefix" :: NullOrUndefined.NullOrUndefined (Key)
  , "Playlists" :: NullOrUndefined.NullOrUndefined (Playlists)
  , "Status" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "UserMetadata" :: NullOrUndefined.NullOrUndefined (UserMetadata)
  , "Timing" :: NullOrUndefined.NullOrUndefined (Timing)
  }
derive instance newtypeJob :: Newtype Job _
derive instance repGenericJob :: Generic Job _
instance showJob :: Show Job where
  show = genericShow
instance decodeJob :: Decode Job where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJob :: Encode Job where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The .jpg or .png file associated with an audio file.</p>
newtype JobAlbumArt = JobAlbumArt 
  { "MergePolicy" :: NullOrUndefined.NullOrUndefined (MergePolicy)
  , "Artwork" :: NullOrUndefined.NullOrUndefined (Artworks)
  }
derive instance newtypeJobAlbumArt :: Newtype JobAlbumArt _
derive instance repGenericJobAlbumArt :: Generic JobAlbumArt _
instance showJobAlbumArt :: Show JobAlbumArt where
  show = genericShow
instance decodeJobAlbumArt :: Decode JobAlbumArt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobAlbumArt :: Encode JobAlbumArt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobContainer = JobContainer String
derive instance newtypeJobContainer :: Newtype JobContainer _
derive instance repGenericJobContainer :: Generic JobContainer _
instance showJobContainer :: Show JobContainer where
  show = genericShow
instance decodeJobContainer :: Decode JobContainer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobContainer :: Encode JobContainer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the file that you're transcoding.</p>
newtype JobInput = JobInput 
  { "Key" :: NullOrUndefined.NullOrUndefined (LongKey)
  , "FrameRate" :: NullOrUndefined.NullOrUndefined (FrameRate)
  , "Resolution" :: NullOrUndefined.NullOrUndefined (Resolution)
  , "AspectRatio" :: NullOrUndefined.NullOrUndefined (AspectRatio)
  , "Interlaced" :: NullOrUndefined.NullOrUndefined (Interlaced)
  , "Container" :: NullOrUndefined.NullOrUndefined (JobContainer)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  , "TimeSpan" :: NullOrUndefined.NullOrUndefined (TimeSpan)
  , "InputCaptions" :: NullOrUndefined.NullOrUndefined (InputCaptions)
  , "DetectedProperties" :: NullOrUndefined.NullOrUndefined (DetectedProperties)
  }
derive instance newtypeJobInput :: Newtype JobInput _
derive instance repGenericJobInput :: Generic JobInput _
instance showJobInput :: Show JobInput where
  show = genericShow
instance decodeJobInput :: Decode JobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobInput :: Encode JobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobInputs = JobInputs (Array JobInput)
derive instance newtypeJobInputs :: Newtype JobInputs _
derive instance repGenericJobInputs :: Generic JobInputs _
instance showJobInputs :: Show JobInputs where
  show = genericShow
instance decodeJobInputs :: Decode JobInputs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobInputs :: Encode JobInputs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <important> <p>Outputs recommended instead.</p> </important> <p>If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the <code>Output</code> object lists information about the first output. This duplicates the information that is listed for the first output in the <code>Outputs</code> object.</p>
newtype JobOutput = JobOutput 
  { "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Key" :: NullOrUndefined.NullOrUndefined (Key)
  , "ThumbnailPattern" :: NullOrUndefined.NullOrUndefined (ThumbnailPattern)
  , "ThumbnailEncryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  , "Rotate" :: NullOrUndefined.NullOrUndefined (Rotate)
  , "PresetId" :: NullOrUndefined.NullOrUndefined (Id)
  , "SegmentDuration" :: NullOrUndefined.NullOrUndefined (FloatString)
  , "Status" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "StatusDetail" :: NullOrUndefined.NullOrUndefined (Description)
  , "Duration" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "Width" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Height" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "FrameRate" :: NullOrUndefined.NullOrUndefined (FloatString)
  , "FileSize" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "DurationMillis" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "Watermarks" :: NullOrUndefined.NullOrUndefined (JobWatermarks)
  , "AlbumArt" :: NullOrUndefined.NullOrUndefined (JobAlbumArt)
  , "Composition" :: NullOrUndefined.NullOrUndefined (Composition)
  , "Captions" :: NullOrUndefined.NullOrUndefined (Captions)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  , "AppliedColorSpaceConversion" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeJobOutput :: Newtype JobOutput _
derive instance repGenericJobOutput :: Generic JobOutput _
instance showJobOutput :: Show JobOutput where
  show = genericShow
instance decodeJobOutput :: Decode JobOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobOutput :: Encode JobOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobOutputs = JobOutputs (Array JobOutput)
derive instance newtypeJobOutputs :: Newtype JobOutputs _
derive instance repGenericJobOutputs :: Generic JobOutputs _
instance showJobOutputs :: Show JobOutputs where
  show = genericShow
instance decodeJobOutputs :: Decode JobOutputs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobOutputs :: Encode JobOutputs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _
derive instance repGenericJobStatus :: Generic JobStatus _
instance showJobStatus :: Show JobStatus where
  show = genericShow
instance decodeJobStatus :: Decode JobStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobStatus :: Encode JobStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.</p>
newtype JobWatermark = JobWatermark 
  { "PresetWatermarkId" :: NullOrUndefined.NullOrUndefined (PresetWatermarkId)
  , "InputKey" :: NullOrUndefined.NullOrUndefined (WatermarkKey)
  , "Encryption" :: NullOrUndefined.NullOrUndefined (Encryption)
  }
derive instance newtypeJobWatermark :: Newtype JobWatermark _
derive instance repGenericJobWatermark :: Generic JobWatermark _
instance showJobWatermark :: Show JobWatermark where
  show = genericShow
instance decodeJobWatermark :: Decode JobWatermark where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobWatermark :: Encode JobWatermark where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobWatermarks = JobWatermarks (Array JobWatermark)
derive instance newtypeJobWatermarks :: Newtype JobWatermarks _
derive instance repGenericJobWatermarks :: Generic JobWatermarks _
instance showJobWatermarks :: Show JobWatermarks where
  show = genericShow
instance decodeJobWatermarks :: Decode JobWatermarks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobWatermarks :: Encode JobWatermarks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Jobs = Jobs (Array Job)
derive instance newtypeJobs :: Newtype Jobs _
derive instance repGenericJobs :: Generic Jobs _
instance showJobs :: Show Jobs where
  show = genericShow
instance decodeJobs :: Decode Jobs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobs :: Encode Jobs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JpgOrPng = JpgOrPng String
derive instance newtypeJpgOrPng :: Newtype JpgOrPng _
derive instance repGenericJpgOrPng :: Generic JpgOrPng _
instance showJpgOrPng :: Show JpgOrPng where
  show = genericShow
instance decodeJpgOrPng :: Decode JpgOrPng where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJpgOrPng :: Encode JpgOrPng where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Key = Key String
derive instance newtypeKey :: Newtype Key _
derive instance repGenericKey :: Generic Key _
instance showKey :: Show Key where
  show = genericShow
instance decodeKey :: Decode Key where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKey :: Encode Key where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyArn = KeyArn String
derive instance newtypeKeyArn :: Newtype KeyArn _
derive instance repGenericKeyArn :: Generic KeyArn _
instance showKeyArn :: Show KeyArn where
  show = genericShow
instance decodeKeyArn :: Decode KeyArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyArn :: Encode KeyArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyIdGuid = KeyIdGuid String
derive instance newtypeKeyIdGuid :: Newtype KeyIdGuid _
derive instance repGenericKeyIdGuid :: Generic KeyIdGuid _
instance showKeyIdGuid :: Show KeyIdGuid where
  show = genericShow
instance decodeKeyIdGuid :: Decode KeyIdGuid where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyIdGuid :: Encode KeyIdGuid where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyStoragePolicy = KeyStoragePolicy String
derive instance newtypeKeyStoragePolicy :: Newtype KeyStoragePolicy _
derive instance repGenericKeyStoragePolicy :: Generic KeyStoragePolicy _
instance showKeyStoragePolicy :: Show KeyStoragePolicy where
  show = genericShow
instance decodeKeyStoragePolicy :: Decode KeyStoragePolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyStoragePolicy :: Encode KeyStoragePolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyframesMaxDist = KeyframesMaxDist String
derive instance newtypeKeyframesMaxDist :: Newtype KeyframesMaxDist _
derive instance repGenericKeyframesMaxDist :: Generic KeyframesMaxDist _
instance showKeyframesMaxDist :: Show KeyframesMaxDist where
  show = genericShow
instance decodeKeyframesMaxDist :: Decode KeyframesMaxDist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyframesMaxDist :: Encode KeyframesMaxDist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Too many operations for a given AWS account. For example, the number of pipelines exceeds the maximum allowed.</p>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ListJobsByPipelineRequest</code> structure.</p>
newtype ListJobsByPipelineRequest = ListJobsByPipelineRequest 
  { "PipelineId" :: (Id)
  , "Ascending" :: NullOrUndefined.NullOrUndefined (Ascending)
  , "PageToken" :: NullOrUndefined.NullOrUndefined (Id)
  }
derive instance newtypeListJobsByPipelineRequest :: Newtype ListJobsByPipelineRequest _
derive instance repGenericListJobsByPipelineRequest :: Generic ListJobsByPipelineRequest _
instance showListJobsByPipelineRequest :: Show ListJobsByPipelineRequest where
  show = genericShow
instance decodeListJobsByPipelineRequest :: Decode ListJobsByPipelineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsByPipelineRequest :: Encode ListJobsByPipelineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ListJobsByPipelineResponse</code> structure.</p>
newtype ListJobsByPipelineResponse = ListJobsByPipelineResponse 
  { "Jobs" :: NullOrUndefined.NullOrUndefined (Jobs)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (Id)
  }
derive instance newtypeListJobsByPipelineResponse :: Newtype ListJobsByPipelineResponse _
derive instance repGenericListJobsByPipelineResponse :: Generic ListJobsByPipelineResponse _
instance showListJobsByPipelineResponse :: Show ListJobsByPipelineResponse where
  show = genericShow
instance decodeListJobsByPipelineResponse :: Decode ListJobsByPipelineResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsByPipelineResponse :: Encode ListJobsByPipelineResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ListJobsByStatusRequest</code> structure.</p>
newtype ListJobsByStatusRequest = ListJobsByStatusRequest 
  { "Status" :: (JobStatus)
  , "Ascending" :: NullOrUndefined.NullOrUndefined (Ascending)
  , "PageToken" :: NullOrUndefined.NullOrUndefined (Id)
  }
derive instance newtypeListJobsByStatusRequest :: Newtype ListJobsByStatusRequest _
derive instance repGenericListJobsByStatusRequest :: Generic ListJobsByStatusRequest _
instance showListJobsByStatusRequest :: Show ListJobsByStatusRequest where
  show = genericShow
instance decodeListJobsByStatusRequest :: Decode ListJobsByStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsByStatusRequest :: Encode ListJobsByStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The <code>ListJobsByStatusResponse</code> structure. </p>
newtype ListJobsByStatusResponse = ListJobsByStatusResponse 
  { "Jobs" :: NullOrUndefined.NullOrUndefined (Jobs)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (Id)
  }
derive instance newtypeListJobsByStatusResponse :: Newtype ListJobsByStatusResponse _
derive instance repGenericListJobsByStatusResponse :: Generic ListJobsByStatusResponse _
instance showListJobsByStatusResponse :: Show ListJobsByStatusResponse where
  show = genericShow
instance decodeListJobsByStatusResponse :: Decode ListJobsByStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsByStatusResponse :: Encode ListJobsByStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ListPipelineRequest</code> structure.</p>
newtype ListPipelinesRequest = ListPipelinesRequest 
  { "Ascending" :: NullOrUndefined.NullOrUndefined (Ascending)
  , "PageToken" :: NullOrUndefined.NullOrUndefined (Id)
  }
derive instance newtypeListPipelinesRequest :: Newtype ListPipelinesRequest _
derive instance repGenericListPipelinesRequest :: Generic ListPipelinesRequest _
instance showListPipelinesRequest :: Show ListPipelinesRequest where
  show = genericShow
instance decodeListPipelinesRequest :: Decode ListPipelinesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPipelinesRequest :: Encode ListPipelinesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of the pipelines associated with the current AWS account.</p>
newtype ListPipelinesResponse = ListPipelinesResponse 
  { "Pipelines" :: NullOrUndefined.NullOrUndefined (Pipelines)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (Id)
  }
derive instance newtypeListPipelinesResponse :: Newtype ListPipelinesResponse _
derive instance repGenericListPipelinesResponse :: Generic ListPipelinesResponse _
instance showListPipelinesResponse :: Show ListPipelinesResponse where
  show = genericShow
instance decodeListPipelinesResponse :: Decode ListPipelinesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPipelinesResponse :: Encode ListPipelinesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ListPresetsRequest</code> structure.</p>
newtype ListPresetsRequest = ListPresetsRequest 
  { "Ascending" :: NullOrUndefined.NullOrUndefined (Ascending)
  , "PageToken" :: NullOrUndefined.NullOrUndefined (Id)
  }
derive instance newtypeListPresetsRequest :: Newtype ListPresetsRequest _
derive instance repGenericListPresetsRequest :: Generic ListPresetsRequest _
instance showListPresetsRequest :: Show ListPresetsRequest where
  show = genericShow
instance decodeListPresetsRequest :: Decode ListPresetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPresetsRequest :: Encode ListPresetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ListPresetsResponse</code> structure.</p>
newtype ListPresetsResponse = ListPresetsResponse 
  { "Presets" :: NullOrUndefined.NullOrUndefined (Presets)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (Id)
  }
derive instance newtypeListPresetsResponse :: Newtype ListPresetsResponse _
derive instance repGenericListPresetsResponse :: Generic ListPresetsResponse _
instance showListPresetsResponse :: Show ListPresetsResponse where
  show = genericShow
instance decodeListPresetsResponse :: Decode ListPresetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPresetsResponse :: Encode ListPresetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LongKey = LongKey String
derive instance newtypeLongKey :: Newtype LongKey _
derive instance repGenericLongKey :: Generic LongKey _
instance showLongKey :: Show LongKey where
  show = genericShow
instance decodeLongKey :: Decode LongKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLongKey :: Encode LongKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxFrameRate = MaxFrameRate String
derive instance newtypeMaxFrameRate :: Newtype MaxFrameRate _
derive instance repGenericMaxFrameRate :: Generic MaxFrameRate _
instance showMaxFrameRate :: Show MaxFrameRate where
  show = genericShow
instance decodeMaxFrameRate :: Decode MaxFrameRate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxFrameRate :: Encode MaxFrameRate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MergePolicy = MergePolicy String
derive instance newtypeMergePolicy :: Newtype MergePolicy _
derive instance repGenericMergePolicy :: Generic MergePolicy _
instance showMergePolicy :: Show MergePolicy where
  show = genericShow
instance decodeMergePolicy :: Decode MergePolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMergePolicy :: Encode MergePolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive instance repGenericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow
instance decodeName :: Decode Name where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeName :: Encode Name where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonEmptyBase64EncodedString = NonEmptyBase64EncodedString String
derive instance newtypeNonEmptyBase64EncodedString :: Newtype NonEmptyBase64EncodedString _
derive instance repGenericNonEmptyBase64EncodedString :: Generic NonEmptyBase64EncodedString _
instance showNonEmptyBase64EncodedString :: Show NonEmptyBase64EncodedString where
  show = genericShow
instance decodeNonEmptyBase64EncodedString :: Decode NonEmptyBase64EncodedString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonEmptyBase64EncodedString :: Encode NonEmptyBase64EncodedString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon Simple Notification Service (Amazon SNS) topic or topics to notify in order to report job status.</p> <important> <p>To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.</p> </important>
newtype Notifications = Notifications 
  { "Progressing" :: NullOrUndefined.NullOrUndefined (SnsTopic)
  , "Completed" :: NullOrUndefined.NullOrUndefined (SnsTopic)
  , "Warning" :: NullOrUndefined.NullOrUndefined (SnsTopic)
  , "Error" :: NullOrUndefined.NullOrUndefined (SnsTopic)
  }
derive instance newtypeNotifications :: Newtype Notifications _
derive instance repGenericNotifications :: Generic Notifications _
instance showNotifications :: Show Notifications where
  show = genericShow
instance decodeNotifications :: Decode Notifications where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotifications :: Encode Notifications where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableInteger = NullableInteger Int
derive instance newtypeNullableInteger :: Newtype NullableInteger _
derive instance repGenericNullableInteger :: Generic NullableInteger _
instance showNullableInteger :: Show NullableInteger where
  show = genericShow
instance decodeNullableInteger :: Decode NullableInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableInteger :: Encode NullableInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableLong = NullableLong Number
derive instance newtypeNullableLong :: Newtype NullableLong _
derive instance repGenericNullableLong :: Generic NullableLong _
instance showNullableLong :: Show NullableLong where
  show = genericShow
instance decodeNullableLong :: Decode NullableLong where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableLong :: Encode NullableLong where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OneTo512String = OneTo512String String
derive instance newtypeOneTo512String :: Newtype OneTo512String _
derive instance repGenericOneTo512String :: Generic OneTo512String _
instance showOneTo512String :: Show OneTo512String where
  show = genericShow
instance decodeOneTo512String :: Decode OneTo512String where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOneTo512String :: Encode OneTo512String where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Opacity = Opacity String
derive instance newtypeOpacity :: Newtype Opacity _
derive instance repGenericOpacity :: Generic Opacity _
instance showOpacity :: Show Opacity where
  show = genericShow
instance decodeOpacity :: Decode Opacity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOpacity :: Encode Opacity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OutputKeys = OutputKeys (Array Key)
derive instance newtypeOutputKeys :: Newtype OutputKeys _
derive instance repGenericOutputKeys :: Generic OutputKeys _
instance showOutputKeys :: Show OutputKeys where
  show = genericShow
instance decodeOutputKeys :: Decode OutputKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputKeys :: Encode OutputKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaddingPolicy = PaddingPolicy String
derive instance newtypePaddingPolicy :: Newtype PaddingPolicy _
derive instance repGenericPaddingPolicy :: Generic PaddingPolicy _
instance showPaddingPolicy :: Show PaddingPolicy where
  show = genericShow
instance decodePaddingPolicy :: Decode PaddingPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaddingPolicy :: Encode PaddingPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Permission</code> structure.</p>
newtype Permission = Permission 
  { "GranteeType" :: NullOrUndefined.NullOrUndefined (GranteeType)
  , "Grantee" :: NullOrUndefined.NullOrUndefined (Grantee)
  , "Access" :: NullOrUndefined.NullOrUndefined (AccessControls)
  }
derive instance newtypePermission :: Newtype Permission _
derive instance repGenericPermission :: Generic Permission _
instance showPermission :: Show Permission where
  show = genericShow
instance decodePermission :: Decode Permission where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePermission :: Encode Permission where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Permissions = Permissions (Array Permission)
derive instance newtypePermissions :: Newtype Permissions _
derive instance repGenericPermissions :: Generic Permissions _
instance showPermissions :: Show Permissions where
  show = genericShow
instance decodePermissions :: Decode Permissions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePermissions :: Encode Permissions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pipeline (queue) that is used to manage jobs.</p>
newtype Pipeline = Pipeline 
  { "Id" :: NullOrUndefined.NullOrUndefined (Id)
  , "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (Name)
  , "Status" :: NullOrUndefined.NullOrUndefined (PipelineStatus)
  , "InputBucket" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "OutputBucket" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "Role" :: NullOrUndefined.NullOrUndefined (Role)
  , "AwsKmsKeyArn" :: NullOrUndefined.NullOrUndefined (KeyArn)
  , "Notifications" :: NullOrUndefined.NullOrUndefined (Notifications)
  , "ContentConfig" :: NullOrUndefined.NullOrUndefined (PipelineOutputConfig)
  , "ThumbnailConfig" :: NullOrUndefined.NullOrUndefined (PipelineOutputConfig)
  }
derive instance newtypePipeline :: Newtype Pipeline _
derive instance repGenericPipeline :: Generic Pipeline _
instance showPipeline :: Show Pipeline where
  show = genericShow
instance decodePipeline :: Decode Pipeline where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipeline :: Encode Pipeline where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>PipelineOutputConfig</code> structure.</p>
newtype PipelineOutputConfig = PipelineOutputConfig 
  { "Bucket" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "StorageClass" :: NullOrUndefined.NullOrUndefined (StorageClass)
  , "Permissions" :: NullOrUndefined.NullOrUndefined (Permissions)
  }
derive instance newtypePipelineOutputConfig :: Newtype PipelineOutputConfig _
derive instance repGenericPipelineOutputConfig :: Generic PipelineOutputConfig _
instance showPipelineOutputConfig :: Show PipelineOutputConfig where
  show = genericShow
instance decodePipelineOutputConfig :: Decode PipelineOutputConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineOutputConfig :: Encode PipelineOutputConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PipelineStatus = PipelineStatus String
derive instance newtypePipelineStatus :: Newtype PipelineStatus _
derive instance repGenericPipelineStatus :: Generic PipelineStatus _
instance showPipelineStatus :: Show PipelineStatus where
  show = genericShow
instance decodePipelineStatus :: Decode PipelineStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineStatus :: Encode PipelineStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Pipelines = Pipelines (Array Pipeline)
derive instance newtypePipelines :: Newtype Pipelines _
derive instance repGenericPipelines :: Generic Pipelines _
instance showPipelines :: Show Pipelines where
  show = genericShow
instance decodePipelines :: Decode Pipelines where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelines :: Encode Pipelines where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PixelsOrPercent = PixelsOrPercent String
derive instance newtypePixelsOrPercent :: Newtype PixelsOrPercent _
derive instance repGenericPixelsOrPercent :: Generic PixelsOrPercent _
instance showPixelsOrPercent :: Show PixelsOrPercent where
  show = genericShow
instance decodePixelsOrPercent :: Decode PixelsOrPercent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePixelsOrPercent :: Encode PixelsOrPercent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The PlayReady DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.</p> <p>PlayReady DRM encrypts your media files using <code>AES-CTR</code> encryption.</p> <p>If you use DRM for an <code>HLSv3</code> playlist, your outputs must have a master playlist.</p>
newtype PlayReadyDrm = PlayReadyDrm 
  { "Format" :: NullOrUndefined.NullOrUndefined (PlayReadyDrmFormatString)
  , "Key" :: NullOrUndefined.NullOrUndefined (NonEmptyBase64EncodedString)
  , "KeyMd5" :: NullOrUndefined.NullOrUndefined (NonEmptyBase64EncodedString)
  , "KeyId" :: NullOrUndefined.NullOrUndefined (KeyIdGuid)
  , "InitializationVector" :: NullOrUndefined.NullOrUndefined (ZeroTo255String)
  , "LicenseAcquisitionUrl" :: NullOrUndefined.NullOrUndefined (OneTo512String)
  }
derive instance newtypePlayReadyDrm :: Newtype PlayReadyDrm _
derive instance repGenericPlayReadyDrm :: Generic PlayReadyDrm _
instance showPlayReadyDrm :: Show PlayReadyDrm where
  show = genericShow
instance decodePlayReadyDrm :: Decode PlayReadyDrm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayReadyDrm :: Encode PlayReadyDrm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlayReadyDrmFormatString = PlayReadyDrmFormatString String
derive instance newtypePlayReadyDrmFormatString :: Newtype PlayReadyDrmFormatString _
derive instance repGenericPlayReadyDrmFormatString :: Generic PlayReadyDrmFormatString _
instance showPlayReadyDrmFormatString :: Show PlayReadyDrmFormatString where
  show = genericShow
instance decodePlayReadyDrmFormatString :: Decode PlayReadyDrmFormatString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlayReadyDrmFormatString :: Encode PlayReadyDrmFormatString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Use Only for Fragmented MP4 or MPEG-TS Outputs. If you specify a preset for which the value of Container is <code>fmp4</code> (Fragmented MP4) or <code>ts</code> (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create. We recommend that you create only one master playlist per output format. The maximum number of master playlists in a job is 30. </p>
newtype Playlist = Playlist 
  { "Name" :: NullOrUndefined.NullOrUndefined (Filename)
  , "Format" :: NullOrUndefined.NullOrUndefined (PlaylistFormat)
  , "OutputKeys" :: NullOrUndefined.NullOrUndefined (OutputKeys)
  , "HlsContentProtection" :: NullOrUndefined.NullOrUndefined (HlsContentProtection)
  , "PlayReadyDrm" :: NullOrUndefined.NullOrUndefined (PlayReadyDrm)
  , "Status" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "StatusDetail" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypePlaylist :: Newtype Playlist _
derive instance repGenericPlaylist :: Generic Playlist _
instance showPlaylist :: Show Playlist where
  show = genericShow
instance decodePlaylist :: Decode Playlist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlaylist :: Encode Playlist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlaylistFormat = PlaylistFormat String
derive instance newtypePlaylistFormat :: Newtype PlaylistFormat _
derive instance repGenericPlaylistFormat :: Generic PlaylistFormat _
instance showPlaylistFormat :: Show PlaylistFormat where
  show = genericShow
instance decodePlaylistFormat :: Decode PlaylistFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlaylistFormat :: Encode PlaylistFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Playlists = Playlists (Array Playlist)
derive instance newtypePlaylists :: Newtype Playlists _
derive instance repGenericPlaylists :: Generic Playlists _
instance showPlaylists :: Show Playlists where
  show = genericShow
instance decodePlaylists :: Decode Playlists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlaylists :: Encode Playlists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Presets are templates that contain most of the settings for transcoding media files from one format to another. Elastic Transcoder includes some default presets for common formats, for example, several iPod and iPhone versions. You can also create your own presets for formats that aren't included among the default presets. You specify which preset you want to use when you create a job.</p>
newtype Preset = Preset 
  { "Id" :: NullOrUndefined.NullOrUndefined (Id)
  , "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Name" :: NullOrUndefined.NullOrUndefined (Name)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Container" :: NullOrUndefined.NullOrUndefined (PresetContainer)
  , "Audio" :: NullOrUndefined.NullOrUndefined (AudioParameters)
  , "Video" :: NullOrUndefined.NullOrUndefined (VideoParameters)
  , "Thumbnails" :: NullOrUndefined.NullOrUndefined (Thumbnails)
  , "Type" :: NullOrUndefined.NullOrUndefined (PresetType)
  }
derive instance newtypePreset :: Newtype Preset _
derive instance repGenericPreset :: Generic Preset _
instance showPreset :: Show Preset where
  show = genericShow
instance decodePreset :: Decode Preset where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePreset :: Encode Preset where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PresetContainer = PresetContainer String
derive instance newtypePresetContainer :: Newtype PresetContainer _
derive instance repGenericPresetContainer :: Generic PresetContainer _
instance showPresetContainer :: Show PresetContainer where
  show = genericShow
instance decodePresetContainer :: Decode PresetContainer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresetContainer :: Encode PresetContainer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PresetType = PresetType String
derive instance newtypePresetType :: Newtype PresetType _
derive instance repGenericPresetType :: Generic PresetType _
instance showPresetType :: Show PresetType where
  show = genericShow
instance decodePresetType :: Decode PresetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresetType :: Encode PresetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video.</p> <p>Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.</p> <p>When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.</p>
newtype PresetWatermark = PresetWatermark 
  { "Id" :: NullOrUndefined.NullOrUndefined (PresetWatermarkId)
  , "MaxWidth" :: NullOrUndefined.NullOrUndefined (PixelsOrPercent)
  , "MaxHeight" :: NullOrUndefined.NullOrUndefined (PixelsOrPercent)
  , "SizingPolicy" :: NullOrUndefined.NullOrUndefined (WatermarkSizingPolicy)
  , "HorizontalAlign" :: NullOrUndefined.NullOrUndefined (HorizontalAlign)
  , "HorizontalOffset" :: NullOrUndefined.NullOrUndefined (PixelsOrPercent)
  , "VerticalAlign" :: NullOrUndefined.NullOrUndefined (VerticalAlign)
  , "VerticalOffset" :: NullOrUndefined.NullOrUndefined (PixelsOrPercent)
  , "Opacity" :: NullOrUndefined.NullOrUndefined (Opacity)
  , "Target" :: NullOrUndefined.NullOrUndefined (Target)
  }
derive instance newtypePresetWatermark :: Newtype PresetWatermark _
derive instance repGenericPresetWatermark :: Generic PresetWatermark _
instance showPresetWatermark :: Show PresetWatermark where
  show = genericShow
instance decodePresetWatermark :: Decode PresetWatermark where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresetWatermark :: Encode PresetWatermark where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PresetWatermarkId = PresetWatermarkId String
derive instance newtypePresetWatermarkId :: Newtype PresetWatermarkId _
derive instance repGenericPresetWatermarkId :: Generic PresetWatermarkId _
instance showPresetWatermarkId :: Show PresetWatermarkId where
  show = genericShow
instance decodePresetWatermarkId :: Decode PresetWatermarkId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresetWatermarkId :: Encode PresetWatermarkId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PresetWatermarks = PresetWatermarks (Array PresetWatermark)
derive instance newtypePresetWatermarks :: Newtype PresetWatermarks _
derive instance repGenericPresetWatermarks :: Generic PresetWatermarks _
instance showPresetWatermarks :: Show PresetWatermarks where
  show = genericShow
instance decodePresetWatermarks :: Decode PresetWatermarks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresetWatermarks :: Encode PresetWatermarks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Presets = Presets (Array Preset)
derive instance newtypePresets :: Newtype Presets _
derive instance repGenericPresets :: Generic Presets _
instance showPresets :: Show Presets where
  show = genericShow
instance decodePresets :: Decode Presets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePresets :: Encode Presets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ReadJobRequest</code> structure.</p>
newtype ReadJobRequest = ReadJobRequest 
  { "Id" :: (Id)
  }
derive instance newtypeReadJobRequest :: Newtype ReadJobRequest _
derive instance repGenericReadJobRequest :: Generic ReadJobRequest _
instance showReadJobRequest :: Show ReadJobRequest where
  show = genericShow
instance decodeReadJobRequest :: Decode ReadJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReadJobRequest :: Encode ReadJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ReadJobResponse</code> structure.</p>
newtype ReadJobResponse = ReadJobResponse 
  { "Job" :: NullOrUndefined.NullOrUndefined (Job)
  }
derive instance newtypeReadJobResponse :: Newtype ReadJobResponse _
derive instance repGenericReadJobResponse :: Generic ReadJobResponse _
instance showReadJobResponse :: Show ReadJobResponse where
  show = genericShow
instance decodeReadJobResponse :: Decode ReadJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReadJobResponse :: Encode ReadJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ReadPipelineRequest</code> structure.</p>
newtype ReadPipelineRequest = ReadPipelineRequest 
  { "Id" :: (Id)
  }
derive instance newtypeReadPipelineRequest :: Newtype ReadPipelineRequest _
derive instance repGenericReadPipelineRequest :: Generic ReadPipelineRequest _
instance showReadPipelineRequest :: Show ReadPipelineRequest where
  show = genericShow
instance decodeReadPipelineRequest :: Decode ReadPipelineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReadPipelineRequest :: Encode ReadPipelineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ReadPipelineResponse</code> structure.</p>
newtype ReadPipelineResponse = ReadPipelineResponse 
  { "Pipeline" :: NullOrUndefined.NullOrUndefined (Pipeline)
  , "Warnings" :: NullOrUndefined.NullOrUndefined (Warnings)
  }
derive instance newtypeReadPipelineResponse :: Newtype ReadPipelineResponse _
derive instance repGenericReadPipelineResponse :: Generic ReadPipelineResponse _
instance showReadPipelineResponse :: Show ReadPipelineResponse where
  show = genericShow
instance decodeReadPipelineResponse :: Decode ReadPipelineResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReadPipelineResponse :: Encode ReadPipelineResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ReadPresetRequest</code> structure.</p>
newtype ReadPresetRequest = ReadPresetRequest 
  { "Id" :: (Id)
  }
derive instance newtypeReadPresetRequest :: Newtype ReadPresetRequest _
derive instance repGenericReadPresetRequest :: Generic ReadPresetRequest _
instance showReadPresetRequest :: Show ReadPresetRequest where
  show = genericShow
instance decodeReadPresetRequest :: Decode ReadPresetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReadPresetRequest :: Encode ReadPresetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ReadPresetResponse</code> structure.</p>
newtype ReadPresetResponse = ReadPresetResponse 
  { "Preset" :: NullOrUndefined.NullOrUndefined (Preset)
  }
derive instance newtypeReadPresetResponse :: Newtype ReadPresetResponse _
derive instance repGenericReadPresetResponse :: Generic ReadPresetResponse _
instance showReadPresetResponse :: Show ReadPresetResponse where
  show = genericShow
instance decodeReadPresetResponse :: Decode ReadPresetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReadPresetResponse :: Encode ReadPresetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Resolution = Resolution String
derive instance newtypeResolution :: Newtype Resolution _
derive instance repGenericResolution :: Generic Resolution _
instance showResolution :: Show Resolution where
  show = genericShow
instance decodeResolution :: Decode Resolution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResolution :: Encode Resolution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource you are attempting to change is in use. For example, you are attempting to delete a pipeline that is currently in use.</p>
newtype ResourceInUseException = ResourceInUseException Types.NoArguments
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _
derive instance repGenericResourceInUseException :: Generic ResourceInUseException _
instance showResourceInUseException :: Show ResourceInUseException where
  show = genericShow
instance decodeResourceInUseException :: Decode ResourceInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceInUseException :: Encode ResourceInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested resource does not exist or is not available. For example, the pipeline to which you're trying to add a job doesn't exist or is still being created.</p>
newtype ResourceNotFoundException = ResourceNotFoundException Types.NoArguments
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Role = Role String
derive instance newtypeRole :: Newtype Role _
derive instance repGenericRole :: Generic Role _
instance showRole :: Show Role where
  show = genericShow
instance decodeRole :: Decode Role where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRole :: Encode Role where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Rotate = Rotate String
derive instance newtypeRotate :: Newtype Rotate _
derive instance repGenericRotate :: Generic Rotate _
instance showRotate :: Show Rotate where
  show = genericShow
instance decodeRotate :: Decode Rotate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRotate :: Encode Rotate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SizingPolicy = SizingPolicy String
derive instance newtypeSizingPolicy :: Newtype SizingPolicy _
derive instance repGenericSizingPolicy :: Generic SizingPolicy _
instance showSizingPolicy :: Show SizingPolicy where
  show = genericShow
instance decodeSizingPolicy :: Decode SizingPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizingPolicy :: Encode SizingPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnsTopic = SnsTopic String
derive instance newtypeSnsTopic :: Newtype SnsTopic _
derive instance repGenericSnsTopic :: Generic SnsTopic _
instance showSnsTopic :: Show SnsTopic where
  show = genericShow
instance decodeSnsTopic :: Decode SnsTopic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnsTopic :: Encode SnsTopic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnsTopics = SnsTopics (Array SnsTopic)
derive instance newtypeSnsTopics :: Newtype SnsTopics _
derive instance repGenericSnsTopics :: Generic SnsTopics _
instance showSnsTopics :: Show SnsTopics where
  show = genericShow
instance decodeSnsTopics :: Decode SnsTopics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnsTopics :: Encode SnsTopics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorageClass = StorageClass String
derive instance newtypeStorageClass :: Newtype StorageClass _
derive instance repGenericStorageClass :: Generic StorageClass _
instance showStorageClass :: Show StorageClass where
  show = genericShow
instance decodeStorageClass :: Decode StorageClass where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageClass :: Encode StorageClass where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Success = Success String
derive instance newtypeSuccess :: Newtype Success _
derive instance repGenericSuccess :: Generic Success _
instance showSuccess :: Show Success where
  show = genericShow
instance decodeSuccess :: Decode Success where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSuccess :: Encode Success where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Target = Target String
derive instance newtypeTarget :: Newtype Target _
derive instance repGenericTarget :: Generic Target _
instance showTarget :: Show Target where
  show = genericShow
instance decodeTarget :: Decode Target where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTarget :: Encode Target where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The <code>TestRoleRequest</code> structure. </p>
newtype TestRoleRequest = TestRoleRequest 
  { "Role" :: (Role)
  , "InputBucket" :: (BucketName)
  , "OutputBucket" :: (BucketName)
  , "Topics" :: (SnsTopics)
  }
derive instance newtypeTestRoleRequest :: Newtype TestRoleRequest _
derive instance repGenericTestRoleRequest :: Generic TestRoleRequest _
instance showTestRoleRequest :: Show TestRoleRequest where
  show = genericShow
instance decodeTestRoleRequest :: Decode TestRoleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestRoleRequest :: Encode TestRoleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>TestRoleResponse</code> structure.</p>
newtype TestRoleResponse = TestRoleResponse 
  { "Success" :: NullOrUndefined.NullOrUndefined (Success)
  , "Messages" :: NullOrUndefined.NullOrUndefined (ExceptionMessages)
  }
derive instance newtypeTestRoleResponse :: Newtype TestRoleResponse _
derive instance repGenericTestRoleResponse :: Generic TestRoleResponse _
instance showTestRoleResponse :: Show TestRoleResponse where
  show = genericShow
instance decodeTestRoleResponse :: Decode TestRoleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestRoleResponse :: Encode TestRoleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThumbnailPattern = ThumbnailPattern String
derive instance newtypeThumbnailPattern :: Newtype ThumbnailPattern _
derive instance repGenericThumbnailPattern :: Generic ThumbnailPattern _
instance showThumbnailPattern :: Show ThumbnailPattern where
  show = genericShow
instance decodeThumbnailPattern :: Decode ThumbnailPattern where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThumbnailPattern :: Encode ThumbnailPattern where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThumbnailResolution = ThumbnailResolution String
derive instance newtypeThumbnailResolution :: Newtype ThumbnailResolution _
derive instance repGenericThumbnailResolution :: Generic ThumbnailResolution _
instance showThumbnailResolution :: Show ThumbnailResolution where
  show = genericShow
instance decodeThumbnailResolution :: Decode ThumbnailResolution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThumbnailResolution :: Encode ThumbnailResolution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Thumbnails for videos.</p>
newtype Thumbnails = Thumbnails 
  { "Format" :: NullOrUndefined.NullOrUndefined (JpgOrPng)
  , "Interval" :: NullOrUndefined.NullOrUndefined (Digits)
  , "Resolution" :: NullOrUndefined.NullOrUndefined (ThumbnailResolution)
  , "AspectRatio" :: NullOrUndefined.NullOrUndefined (AspectRatio)
  , "MaxWidth" :: NullOrUndefined.NullOrUndefined (DigitsOrAuto)
  , "MaxHeight" :: NullOrUndefined.NullOrUndefined (DigitsOrAuto)
  , "SizingPolicy" :: NullOrUndefined.NullOrUndefined (SizingPolicy)
  , "PaddingPolicy" :: NullOrUndefined.NullOrUndefined (PaddingPolicy)
  }
derive instance newtypeThumbnails :: Newtype Thumbnails _
derive instance repGenericThumbnails :: Generic Thumbnails _
instance showThumbnails :: Show Thumbnails where
  show = genericShow
instance decodeThumbnails :: Decode Thumbnails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThumbnails :: Encode Thumbnails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Time = Time String
derive instance newtypeTime :: Newtype Time _
derive instance repGenericTime :: Generic Time _
instance showTime :: Show Time where
  show = genericShow
instance decodeTime :: Decode Time where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTime :: Encode Time where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeOffset = TimeOffset String
derive instance newtypeTimeOffset :: Newtype TimeOffset _
derive instance repGenericTimeOffset :: Generic TimeOffset _
instance showTimeOffset :: Show TimeOffset where
  show = genericShow
instance decodeTimeOffset :: Decode TimeOffset where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeOffset :: Encode TimeOffset where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Settings that determine when a clip begins and how long it lasts.</p>
newtype TimeSpan = TimeSpan 
  { "StartTime" :: NullOrUndefined.NullOrUndefined (Time)
  , "Duration" :: NullOrUndefined.NullOrUndefined (Time)
  }
derive instance newtypeTimeSpan :: Newtype TimeSpan _
derive instance repGenericTimeSpan :: Generic TimeSpan _
instance showTimeSpan :: Show TimeSpan where
  show = genericShow
instance decodeTimeSpan :: Decode TimeSpan where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeSpan :: Encode TimeSpan where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details about the timing of a job.</p>
newtype Timing = Timing 
  { "SubmitTimeMillis" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "StartTimeMillis" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "FinishTimeMillis" :: NullOrUndefined.NullOrUndefined (NullableLong)
  }
derive instance newtypeTiming :: Newtype Timing _
derive instance repGenericTiming :: Generic Timing _
instance showTiming :: Show Timing where
  show = genericShow
instance decodeTiming :: Decode Timing where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTiming :: Encode Timing where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>UpdatePipelineNotificationsRequest</code> structure.</p>
newtype UpdatePipelineNotificationsRequest = UpdatePipelineNotificationsRequest 
  { "Id" :: (Id)
  , "Notifications" :: (Notifications)
  }
derive instance newtypeUpdatePipelineNotificationsRequest :: Newtype UpdatePipelineNotificationsRequest _
derive instance repGenericUpdatePipelineNotificationsRequest :: Generic UpdatePipelineNotificationsRequest _
instance showUpdatePipelineNotificationsRequest :: Show UpdatePipelineNotificationsRequest where
  show = genericShow
instance decodeUpdatePipelineNotificationsRequest :: Decode UpdatePipelineNotificationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePipelineNotificationsRequest :: Encode UpdatePipelineNotificationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>UpdatePipelineNotificationsResponse</code> structure.</p>
newtype UpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse 
  { "Pipeline" :: NullOrUndefined.NullOrUndefined (Pipeline)
  }
derive instance newtypeUpdatePipelineNotificationsResponse :: Newtype UpdatePipelineNotificationsResponse _
derive instance repGenericUpdatePipelineNotificationsResponse :: Generic UpdatePipelineNotificationsResponse _
instance showUpdatePipelineNotificationsResponse :: Show UpdatePipelineNotificationsResponse where
  show = genericShow
instance decodeUpdatePipelineNotificationsResponse :: Decode UpdatePipelineNotificationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePipelineNotificationsResponse :: Encode UpdatePipelineNotificationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>UpdatePipelineRequest</code> structure.</p>
newtype UpdatePipelineRequest = UpdatePipelineRequest 
  { "Id" :: (Id)
  , "Name" :: NullOrUndefined.NullOrUndefined (Name)
  , "InputBucket" :: NullOrUndefined.NullOrUndefined (BucketName)
  , "Role" :: NullOrUndefined.NullOrUndefined (Role)
  , "AwsKmsKeyArn" :: NullOrUndefined.NullOrUndefined (KeyArn)
  , "Notifications" :: NullOrUndefined.NullOrUndefined (Notifications)
  , "ContentConfig" :: NullOrUndefined.NullOrUndefined (PipelineOutputConfig)
  , "ThumbnailConfig" :: NullOrUndefined.NullOrUndefined (PipelineOutputConfig)
  }
derive instance newtypeUpdatePipelineRequest :: Newtype UpdatePipelineRequest _
derive instance repGenericUpdatePipelineRequest :: Generic UpdatePipelineRequest _
instance showUpdatePipelineRequest :: Show UpdatePipelineRequest where
  show = genericShow
instance decodeUpdatePipelineRequest :: Decode UpdatePipelineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePipelineRequest :: Encode UpdatePipelineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When you update a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>
newtype UpdatePipelineResponse = UpdatePipelineResponse 
  { "Pipeline" :: NullOrUndefined.NullOrUndefined (Pipeline)
  , "Warnings" :: NullOrUndefined.NullOrUndefined (Warnings)
  }
derive instance newtypeUpdatePipelineResponse :: Newtype UpdatePipelineResponse _
derive instance repGenericUpdatePipelineResponse :: Generic UpdatePipelineResponse _
instance showUpdatePipelineResponse :: Show UpdatePipelineResponse where
  show = genericShow
instance decodeUpdatePipelineResponse :: Decode UpdatePipelineResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePipelineResponse :: Encode UpdatePipelineResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>UpdatePipelineStatusRequest</code> structure.</p>
newtype UpdatePipelineStatusRequest = UpdatePipelineStatusRequest 
  { "Id" :: (Id)
  , "Status" :: (PipelineStatus)
  }
derive instance newtypeUpdatePipelineStatusRequest :: Newtype UpdatePipelineStatusRequest _
derive instance repGenericUpdatePipelineStatusRequest :: Generic UpdatePipelineStatusRequest _
instance showUpdatePipelineStatusRequest :: Show UpdatePipelineStatusRequest where
  show = genericShow
instance decodeUpdatePipelineStatusRequest :: Decode UpdatePipelineStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePipelineStatusRequest :: Encode UpdatePipelineStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>When you update status for a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>
newtype UpdatePipelineStatusResponse = UpdatePipelineStatusResponse 
  { "Pipeline" :: NullOrUndefined.NullOrUndefined (Pipeline)
  }
derive instance newtypeUpdatePipelineStatusResponse :: Newtype UpdatePipelineStatusResponse _
derive instance repGenericUpdatePipelineStatusResponse :: Generic UpdatePipelineStatusResponse _
instance showUpdatePipelineStatusResponse :: Show UpdatePipelineStatusResponse where
  show = genericShow
instance decodeUpdatePipelineStatusResponse :: Decode UpdatePipelineStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePipelineStatusResponse :: Encode UpdatePipelineStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserMetadata = UserMetadata (StrMap.StrMap String)
derive instance newtypeUserMetadata :: Newtype UserMetadata _
derive instance repGenericUserMetadata :: Generic UserMetadata _
instance showUserMetadata :: Show UserMetadata where
  show = genericShow
instance decodeUserMetadata :: Decode UserMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserMetadata :: Encode UserMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more required parameter values were not provided in the request.</p>
newtype ValidationException = ValidationException Types.NoArguments
derive instance newtypeValidationException :: Newtype ValidationException _
derive instance repGenericValidationException :: Generic ValidationException _
instance showValidationException :: Show ValidationException where
  show = genericShow
instance decodeValidationException :: Decode ValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationException :: Encode ValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VerticalAlign = VerticalAlign String
derive instance newtypeVerticalAlign :: Newtype VerticalAlign _
derive instance repGenericVerticalAlign :: Generic VerticalAlign _
instance showVerticalAlign :: Show VerticalAlign where
  show = genericShow
instance decodeVerticalAlign :: Decode VerticalAlign where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerticalAlign :: Encode VerticalAlign where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VideoBitRate = VideoBitRate String
derive instance newtypeVideoBitRate :: Newtype VideoBitRate _
derive instance repGenericVideoBitRate :: Generic VideoBitRate _
instance showVideoBitRate :: Show VideoBitRate where
  show = genericShow
instance decodeVideoBitRate :: Decode VideoBitRate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoBitRate :: Encode VideoBitRate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VideoCodec = VideoCodec String
derive instance newtypeVideoCodec :: Newtype VideoCodec _
derive instance repGenericVideoCodec :: Generic VideoCodec _
instance showVideoCodec :: Show VideoCodec where
  show = genericShow
instance decodeVideoCodec :: Decode VideoCodec where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoCodec :: Encode VideoCodec where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>VideoParameters</code> structure.</p>
newtype VideoParameters = VideoParameters 
  { "Codec" :: NullOrUndefined.NullOrUndefined (VideoCodec)
  , "CodecOptions" :: NullOrUndefined.NullOrUndefined (CodecOptions)
  , "KeyframesMaxDist" :: NullOrUndefined.NullOrUndefined (KeyframesMaxDist)
  , "FixedGOP" :: NullOrUndefined.NullOrUndefined (FixedGOP)
  , "BitRate" :: NullOrUndefined.NullOrUndefined (VideoBitRate)
  , "FrameRate" :: NullOrUndefined.NullOrUndefined (FrameRate)
  , "MaxFrameRate" :: NullOrUndefined.NullOrUndefined (MaxFrameRate)
  , "Resolution" :: NullOrUndefined.NullOrUndefined (Resolution)
  , "AspectRatio" :: NullOrUndefined.NullOrUndefined (AspectRatio)
  , "MaxWidth" :: NullOrUndefined.NullOrUndefined (DigitsOrAuto)
  , "MaxHeight" :: NullOrUndefined.NullOrUndefined (DigitsOrAuto)
  , "DisplayAspectRatio" :: NullOrUndefined.NullOrUndefined (AspectRatio)
  , "SizingPolicy" :: NullOrUndefined.NullOrUndefined (SizingPolicy)
  , "PaddingPolicy" :: NullOrUndefined.NullOrUndefined (PaddingPolicy)
  , "Watermarks" :: NullOrUndefined.NullOrUndefined (PresetWatermarks)
  }
derive instance newtypeVideoParameters :: Newtype VideoParameters _
derive instance repGenericVideoParameters :: Generic VideoParameters _
instance showVideoParameters :: Show VideoParameters where
  show = genericShow
instance decodeVideoParameters :: Decode VideoParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVideoParameters :: Encode VideoParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.</p> <p>Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.</p>
newtype Warning = Warning 
  { "Code" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeWarning :: Newtype Warning _
derive instance repGenericWarning :: Generic Warning _
instance showWarning :: Show Warning where
  show = genericShow
instance decodeWarning :: Decode Warning where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWarning :: Encode Warning where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Warnings = Warnings (Array Warning)
derive instance newtypeWarnings :: Newtype Warnings _
derive instance repGenericWarnings :: Generic Warnings _
instance showWarnings :: Show Warnings where
  show = genericShow
instance decodeWarnings :: Decode Warnings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWarnings :: Encode Warnings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WatermarkKey = WatermarkKey String
derive instance newtypeWatermarkKey :: Newtype WatermarkKey _
derive instance repGenericWatermarkKey :: Generic WatermarkKey _
instance showWatermarkKey :: Show WatermarkKey where
  show = genericShow
instance decodeWatermarkKey :: Decode WatermarkKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWatermarkKey :: Encode WatermarkKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WatermarkSizingPolicy = WatermarkSizingPolicy String
derive instance newtypeWatermarkSizingPolicy :: Newtype WatermarkSizingPolicy _
derive instance repGenericWatermarkSizingPolicy :: Generic WatermarkSizingPolicy _
instance showWatermarkSizingPolicy :: Show WatermarkSizingPolicy where
  show = genericShow
instance decodeWatermarkSizingPolicy :: Decode WatermarkSizingPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWatermarkSizingPolicy :: Encode WatermarkSizingPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ZeroTo255String = ZeroTo255String String
derive instance newtypeZeroTo255String :: Newtype ZeroTo255String _
derive instance repGenericZeroTo255String :: Generic ZeroTo255String _
instance showZeroTo255String :: Show ZeroTo255String where
  show = genericShow
instance decodeZeroTo255String :: Decode ZeroTo255String where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeZeroTo255String :: Encode ZeroTo255String where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ZeroTo512String = ZeroTo512String String
derive instance newtypeZeroTo512String :: Newtype ZeroTo512String _
derive instance repGenericZeroTo512String :: Generic ZeroTo512String _
instance showZeroTo512String :: Show ZeroTo512String where
  show = genericShow
instance decodeZeroTo512String :: Decode ZeroTo512String where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeZeroTo512String :: Encode ZeroTo512String where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
