

-- | <fullname>AWS Elastic Transcoder Service</fullname> <p>The AWS Elastic Transcoder Service.</p>
module AWS.ElasticTranscoder where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ElasticTranscoder" :: String


-- | <p>The CancelJob operation cancels an unfinished job.</p> <note> <p>You can only cancel a job that has a status of <code>Submitted</code>. To prevent a pipeline from starting to process a job while you're getting the job identifier, use <a>UpdatePipelineStatus</a> to temporarily pause the pipeline.</p> </note>
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: AWS.RequestError | eff) CancelJobResponse
cancelJob = AWS.request serviceName "CancelJob" 


-- | <p>When you create a job, Elastic Transcoder returns JSON data that includes the values that you specified plus information about the job that is created.</p> <p>If you have specified more than one output for your jobs (for example, one output for the Kindle Fire and another output for the Apple iPhone 4s), you currently must use the Elastic Transcoder API to list the jobs (as opposed to the AWS Console).</p>
createJob :: forall eff. CreateJobRequest -> Aff (err :: AWS.RequestError | eff) CreateJobResponse
createJob = AWS.request serviceName "CreateJob" 


-- | <p>The CreatePipeline operation creates a pipeline with settings that you specify.</p>
createPipeline :: forall eff. CreatePipelineRequest -> Aff (err :: AWS.RequestError | eff) CreatePipelineResponse
createPipeline = AWS.request serviceName "CreatePipeline" 


-- | <p>The CreatePreset operation creates a preset with settings that you specify.</p> <important> <p>Elastic Transcoder checks the CreatePreset settings to ensure that they meet Elastic Transcoder requirements and to determine whether they comply with H.264 standards. If your settings are not valid for Elastic Transcoder, Elastic Transcoder returns an HTTP 400 response (<code>ValidationException</code>) and does not create the preset. If the settings are valid for Elastic Transcoder but aren't strictly compliant with the H.264 standard, Elastic Transcoder creates the preset and returns a warning message in the response. This helps you determine whether your settings comply with the H.264 standard while giving you greater flexibility with respect to the video that Elastic Transcoder produces.</p> </important> <p>Elastic Transcoder uses the H.264 video-compression format. For more information, see the International Telecommunication Union publication <i>Recommendation ITU-T H.264: Advanced video coding for generic audiovisual services</i>.</p>
createPreset :: forall eff. CreatePresetRequest -> Aff (err :: AWS.RequestError | eff) CreatePresetResponse
createPreset = AWS.request serviceName "CreatePreset" 


-- | <p>The DeletePipeline operation removes a pipeline.</p> <p> You can only delete a pipeline that has never been used or that is not currently in use (doesn't contain any active jobs). If the pipeline is currently in use, <code>DeletePipeline</code> returns an error. </p>
deletePipeline :: forall eff. DeletePipelineRequest -> Aff (err :: AWS.RequestError | eff) DeletePipelineResponse
deletePipeline = AWS.request serviceName "DeletePipeline" 


-- | <p>The DeletePreset operation removes a preset that you've added in an AWS region.</p> <note> <p>You can't delete the default presets that are included with Elastic Transcoder.</p> </note>
deletePreset :: forall eff. DeletePresetRequest -> Aff (err :: AWS.RequestError | eff) DeletePresetResponse
deletePreset = AWS.request serviceName "DeletePreset" 


-- | <p>The ListJobsByPipeline operation gets a list of the jobs currently in a pipeline.</p> <p>Elastic Transcoder returns all of the jobs currently in the specified pipeline. The response body contains one element for each job that satisfies the search criteria.</p>
listJobsByPipeline :: forall eff. ListJobsByPipelineRequest -> Aff (err :: AWS.RequestError | eff) ListJobsByPipelineResponse
listJobsByPipeline = AWS.request serviceName "ListJobsByPipeline" 


-- | <p>The ListJobsByStatus operation gets a list of jobs that have a specified status. The response body contains one element for each job that satisfies the search criteria.</p>
listJobsByStatus :: forall eff. ListJobsByStatusRequest -> Aff (err :: AWS.RequestError | eff) ListJobsByStatusResponse
listJobsByStatus = AWS.request serviceName "ListJobsByStatus" 


-- | <p>The ListPipelines operation gets a list of the pipelines associated with the current AWS account.</p>
listPipelines :: forall eff. ListPipelinesRequest -> Aff (err :: AWS.RequestError | eff) ListPipelinesResponse
listPipelines = AWS.request serviceName "ListPipelines" 


-- | <p>The ListPresets operation gets a list of the default presets included with Elastic Transcoder and the presets that you've added in an AWS region.</p>
listPresets :: forall eff. ListPresetsRequest -> Aff (err :: AWS.RequestError | eff) ListPresetsResponse
listPresets = AWS.request serviceName "ListPresets" 


-- | <p>The ReadJob operation returns detailed information about a job.</p>
readJob :: forall eff. ReadJobRequest -> Aff (err :: AWS.RequestError | eff) ReadJobResponse
readJob = AWS.request serviceName "ReadJob" 


-- | <p>The ReadPipeline operation gets detailed information about a pipeline.</p>
readPipeline :: forall eff. ReadPipelineRequest -> Aff (err :: AWS.RequestError | eff) ReadPipelineResponse
readPipeline = AWS.request serviceName "ReadPipeline" 


-- | <p>The ReadPreset operation gets detailed information about a preset.</p>
readPreset :: forall eff. ReadPresetRequest -> Aff (err :: AWS.RequestError | eff) ReadPresetResponse
readPreset = AWS.request serviceName "ReadPreset" 


-- | <p>The TestRole operation tests the IAM role used to create the pipeline.</p> <p>The <code>TestRole</code> action lets you determine whether the IAM role you are using has sufficient permissions to let Elastic Transcoder perform tasks associated with the transcoding process. The action attempts to assume the specified IAM role, checks read access to the input and output buckets, and tries to send a test notification to Amazon SNS topics that you specify.</p>
testRole :: forall eff. TestRoleRequest -> Aff (err :: AWS.RequestError | eff) TestRoleResponse
testRole = AWS.request serviceName "TestRole" 


-- | <p> Use the <code>UpdatePipeline</code> operation to update settings for a pipeline.</p> <important> <p>When you change pipeline settings, your changes take effect immediately. Jobs that you have already submitted and that Elastic Transcoder has not started to process are affected in addition to jobs that you submit after you change settings. </p> </important>
updatePipeline :: forall eff. UpdatePipelineRequest -> Aff (err :: AWS.RequestError | eff) UpdatePipelineResponse
updatePipeline = AWS.request serviceName "UpdatePipeline" 


-- | <p>With the UpdatePipelineNotifications operation, you can update Amazon Simple Notification Service (Amazon SNS) notifications for a pipeline.</p> <p>When you update notifications for a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>
updatePipelineNotifications :: forall eff. UpdatePipelineNotificationsRequest -> Aff (err :: AWS.RequestError | eff) UpdatePipelineNotificationsResponse
updatePipelineNotifications = AWS.request serviceName "UpdatePipelineNotifications" 


-- | <p>The UpdatePipelineStatus operation pauses or reactivates a pipeline, so that the pipeline stops or restarts the processing of jobs.</p> <p>Changing the pipeline status is useful if you want to cancel one or more jobs. You can't cancel jobs after Elastic Transcoder has started processing them; if you pause the pipeline to which you submitted the jobs, you have more time to get the job IDs for the jobs that you want to cancel, and to send a <a>CancelJob</a> request. </p>
updatePipelineStatus :: forall eff. UpdatePipelineStatusRequest -> Aff (err :: AWS.RequestError | eff) UpdatePipelineStatusResponse
updatePipelineStatus = AWS.request serviceName "UpdatePipelineStatus" 


newtype AccessControl = AccessControl String


newtype AccessControls = AccessControls (Array AccessControl)


-- | <p>General authentication failure. The request was not signed correctly.</p>
newtype AccessDeniedException = AccessDeniedException 
  { 
  }


-- | <p>The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20.</p> <p>To remove artwork or leave the artwork empty, you can either set <code>Artwork</code> to null, or set the <code>Merge Policy</code> to "Replace" and use an empty <code>Artwork</code> array.</p> <p>To pass through existing artwork unchanged, set the <code>Merge Policy</code> to "Prepend", "Append", or "Fallback", and use an empty <code>Artwork</code> array.</p>
newtype Artwork = Artwork 
  { "InputKey" :: NullOrUndefined (WatermarkKey)
  , "MaxWidth" :: NullOrUndefined (DigitsOrAuto)
  , "MaxHeight" :: NullOrUndefined (DigitsOrAuto)
  , "SizingPolicy" :: NullOrUndefined (SizingPolicy)
  , "PaddingPolicy" :: NullOrUndefined (PaddingPolicy)
  , "AlbumArtFormat" :: NullOrUndefined (JpgOrPng)
  , "Encryption" :: NullOrUndefined (Encryption)
  }


newtype Artworks = Artworks (Array Artwork)


newtype Ascending = Ascending String


newtype AspectRatio = AspectRatio String


newtype AudioBitDepth = AudioBitDepth String


newtype AudioBitOrder = AudioBitOrder String


newtype AudioBitRate = AudioBitRate String


newtype AudioChannels = AudioChannels String


newtype AudioCodec = AudioCodec String


-- | <p>Options associated with your audio codec.</p>
newtype AudioCodecOptions = AudioCodecOptions 
  { "Profile" :: NullOrUndefined (AudioCodecProfile)
  , "BitDepth" :: NullOrUndefined (AudioBitDepth)
  , "BitOrder" :: NullOrUndefined (AudioBitOrder)
  , "Signed" :: NullOrUndefined (AudioSigned)
  }


newtype AudioCodecProfile = AudioCodecProfile String


newtype AudioPackingMode = AudioPackingMode String


-- | <p>Parameters required for transcoding audio.</p>
newtype AudioParameters = AudioParameters 
  { "Codec" :: NullOrUndefined (AudioCodec)
  , "SampleRate" :: NullOrUndefined (AudioSampleRate)
  , "BitRate" :: NullOrUndefined (AudioBitRate)
  , "Channels" :: NullOrUndefined (AudioChannels)
  , "AudioPackingMode" :: NullOrUndefined (AudioPackingMode)
  , "CodecOptions" :: NullOrUndefined (AudioCodecOptions)
  }


newtype AudioSampleRate = AudioSampleRate String


newtype AudioSigned = AudioSigned String


newtype Base64EncodedString = Base64EncodedString String


newtype BucketName = BucketName String


-- | <p>The <code>CancelJobRequest</code> structure.</p>
newtype CancelJobRequest = CancelJobRequest 
  { "Id" :: (Id)
  }


-- | <p>The response body contains a JSON object. If the job is successfully canceled, the value of <code>Success</code> is <code>true</code>.</p>
newtype CancelJobResponse = CancelJobResponse 
  { 
  }


-- | <p>The file format of the output captions. If you leave this value blank, Elastic Transcoder returns an error.</p>
newtype CaptionFormat = CaptionFormat 
  { "Format" :: NullOrUndefined (CaptionFormatFormat)
  , "Pattern" :: NullOrUndefined (CaptionFormatPattern)
  , "Encryption" :: NullOrUndefined (Encryption)
  }


newtype CaptionFormatFormat = CaptionFormatFormat String


newtype CaptionFormatPattern = CaptionFormatPattern String


newtype CaptionFormats = CaptionFormats (Array CaptionFormat)


newtype CaptionMergePolicy = CaptionMergePolicy String


-- | <p>A source file for the input sidecar captions used during the transcoding process.</p>
newtype CaptionSource = CaptionSource 
  { "Key" :: NullOrUndefined (LongKey)
  , "Language" :: NullOrUndefined (Key)
  , "TimeOffset" :: NullOrUndefined (TimeOffset)
  , "Label" :: NullOrUndefined (Name)
  , "Encryption" :: NullOrUndefined (Encryption)
  }


newtype CaptionSources = CaptionSources (Array CaptionSource)


-- | <p>The captions to be created, if any.</p>
newtype Captions = Captions 
  { "MergePolicy" :: NullOrUndefined (CaptionMergePolicy)
  , "CaptionSources" :: NullOrUndefined (CaptionSources)
  , "CaptionFormats" :: NullOrUndefined (CaptionFormats)
  }


-- | <p>Settings for one clip in a composition. All jobs in a playlist must have the same clip settings.</p>
newtype Clip = Clip 
  { "TimeSpan" :: NullOrUndefined (TimeSpan)
  }


newtype CodecOption = CodecOption String


newtype CodecOptions = CodecOptions (Map CodecOption CodecOption)


newtype Composition = Composition (Array Clip)


-- | <p>The <code>CreateJobOutput</code> structure.</p>
newtype CreateJobOutput = CreateJobOutput 
  { "Key" :: NullOrUndefined (Key)
  , "ThumbnailPattern" :: NullOrUndefined (ThumbnailPattern)
  , "ThumbnailEncryption" :: NullOrUndefined (Encryption)
  , "Rotate" :: NullOrUndefined (Rotate)
  , "PresetId" :: NullOrUndefined (Id)
  , "SegmentDuration" :: NullOrUndefined (FloatString)
  , "Watermarks" :: NullOrUndefined (JobWatermarks)
  , "AlbumArt" :: NullOrUndefined (JobAlbumArt)
  , "Composition" :: NullOrUndefined (Composition)
  , "Captions" :: NullOrUndefined (Captions)
  , "Encryption" :: NullOrUndefined (Encryption)
  }


newtype CreateJobOutputs = CreateJobOutputs (Array CreateJobOutput)


-- | <p>Information about the master playlist.</p>
newtype CreateJobPlaylist = CreateJobPlaylist 
  { "Name" :: NullOrUndefined (Filename)
  , "Format" :: NullOrUndefined (PlaylistFormat)
  , "OutputKeys" :: NullOrUndefined (OutputKeys)
  , "HlsContentProtection" :: NullOrUndefined (HlsContentProtection)
  , "PlayReadyDrm" :: NullOrUndefined (PlayReadyDrm)
  }


newtype CreateJobPlaylists = CreateJobPlaylists (Array CreateJobPlaylist)


-- | <p>The <code>CreateJobRequest</code> structure.</p>
newtype CreateJobRequest = CreateJobRequest 
  { "PipelineId" :: (Id)
  , "Input" :: NullOrUndefined (JobInput)
  , "Inputs" :: NullOrUndefined (JobInputs)
  , "Output" :: NullOrUndefined (CreateJobOutput)
  , "Outputs" :: NullOrUndefined (CreateJobOutputs)
  , "OutputKeyPrefix" :: NullOrUndefined (Key)
  , "Playlists" :: NullOrUndefined (CreateJobPlaylists)
  , "UserMetadata" :: NullOrUndefined (UserMetadata)
  }


-- | <p>The CreateJobResponse structure.</p>
newtype CreateJobResponse = CreateJobResponse 
  { "Job" :: NullOrUndefined (Job)
  }


-- | <p>The <code>CreatePipelineRequest</code> structure.</p>
newtype CreatePipelineRequest = CreatePipelineRequest 
  { "Name" :: (Name)
  , "InputBucket" :: (BucketName)
  , "OutputBucket" :: NullOrUndefined (BucketName)
  , "Role" :: (Role)
  , "AwsKmsKeyArn" :: NullOrUndefined (KeyArn)
  , "Notifications" :: NullOrUndefined (Notifications)
  , "ContentConfig" :: NullOrUndefined (PipelineOutputConfig)
  , "ThumbnailConfig" :: NullOrUndefined (PipelineOutputConfig)
  }


-- | <p>When you create a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>
newtype CreatePipelineResponse = CreatePipelineResponse 
  { "Pipeline" :: NullOrUndefined (Pipeline)
  , "Warnings" :: NullOrUndefined (Warnings)
  }


-- | <p>The <code>CreatePresetRequest</code> structure.</p>
newtype CreatePresetRequest = CreatePresetRequest 
  { "Name" :: (Name)
  , "Description" :: NullOrUndefined (Description)
  , "Container" :: (PresetContainer)
  , "Video" :: NullOrUndefined (VideoParameters)
  , "Audio" :: NullOrUndefined (AudioParameters)
  , "Thumbnails" :: NullOrUndefined (Thumbnails)
  }


-- | <p>The <code>CreatePresetResponse</code> structure.</p>
newtype CreatePresetResponse = CreatePresetResponse 
  { "Preset" :: NullOrUndefined (Preset)
  , "Warning" :: NullOrUndefined (String)
  }


-- | <p>The <code>DeletePipelineRequest</code> structure.</p>
newtype DeletePipelineRequest = DeletePipelineRequest 
  { "Id" :: (Id)
  }


-- | <p>The <code>DeletePipelineResponse</code> structure.</p>
newtype DeletePipelineResponse = DeletePipelineResponse 
  { 
  }


-- | <p>The <code>DeletePresetRequest</code> structure.</p>
newtype DeletePresetRequest = DeletePresetRequest 
  { "Id" :: (Id)
  }


-- | <p>The <code>DeletePresetResponse</code> structure.</p>
newtype DeletePresetResponse = DeletePresetResponse 
  { 
  }


newtype Description = Description String


-- | <p>The detected properties of the input file. Elastic Transcoder identifies these values from the input file.</p>
newtype DetectedProperties = DetectedProperties 
  { "Width" :: NullOrUndefined (NullableInteger)
  , "Height" :: NullOrUndefined (NullableInteger)
  , "FrameRate" :: NullOrUndefined (FloatString)
  , "FileSize" :: NullOrUndefined (NullableLong)
  , "DurationMillis" :: NullOrUndefined (NullableLong)
  }


newtype Digits = Digits String


newtype DigitsOrAuto = DigitsOrAuto String


-- | <p>The encryption settings, if any, that are used for decrypting your input files or encrypting your output files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file, otherwise you must specify the mode you want Elastic Transcoder to use to encrypt your output files.</p>
newtype Encryption = Encryption 
  { "Mode" :: NullOrUndefined (EncryptionMode)
  , "Key" :: NullOrUndefined (Base64EncodedString)
  , "KeyMd5" :: NullOrUndefined (Base64EncodedString)
  , "InitializationVector" :: NullOrUndefined (ZeroTo255String)
  }


newtype EncryptionMode = EncryptionMode String


newtype ExceptionMessages = ExceptionMessages (Array String)


newtype Filename = Filename String


newtype FixedGOP = FixedGOP String


newtype FloatString = FloatString String


newtype FrameRate = FrameRate String


newtype Grantee = Grantee String


newtype GranteeType = GranteeType String


-- | <p>The HLS content protection settings, if any, that you want Elastic Transcoder to apply to your output files.</p>
newtype HlsContentProtection = HlsContentProtection 
  { "Method" :: NullOrUndefined (HlsContentProtectionMethod)
  , "Key" :: NullOrUndefined (Base64EncodedString)
  , "KeyMd5" :: NullOrUndefined (Base64EncodedString)
  , "InitializationVector" :: NullOrUndefined (ZeroTo255String)
  , "LicenseAcquisitionUrl" :: NullOrUndefined (ZeroTo512String)
  , "KeyStoragePolicy" :: NullOrUndefined (KeyStoragePolicy)
  }


newtype HlsContentProtectionMethod = HlsContentProtectionMethod String


newtype HorizontalAlign = HorizontalAlign String


newtype Id = Id String


newtype IncompatibleVersionException = IncompatibleVersionException 
  { 
  }


-- | <p>The captions to be created, if any.</p>
newtype InputCaptions = InputCaptions 
  { "MergePolicy" :: NullOrUndefined (CaptionMergePolicy)
  , "CaptionSources" :: NullOrUndefined (CaptionSources)
  }


newtype Interlaced = Interlaced String


-- | <p>Elastic Transcoder encountered an unexpected exception while trying to fulfill the request.</p>
newtype InternalServiceException = InternalServiceException 
  { 
  }


-- | <p>A section of the response body that provides information about the job that is created.</p>
newtype Job = Job 
  { "Id" :: NullOrUndefined (Id)
  , "Arn" :: NullOrUndefined (String)
  , "PipelineId" :: NullOrUndefined (Id)
  , "Input" :: NullOrUndefined (JobInput)
  , "Inputs" :: NullOrUndefined (JobInputs)
  , "Output" :: NullOrUndefined (JobOutput)
  , "Outputs" :: NullOrUndefined (JobOutputs)
  , "OutputKeyPrefix" :: NullOrUndefined (Key)
  , "Playlists" :: NullOrUndefined (Playlists)
  , "Status" :: NullOrUndefined (JobStatus)
  , "UserMetadata" :: NullOrUndefined (UserMetadata)
  , "Timing" :: NullOrUndefined (Timing)
  }


-- | <p>The .jpg or .png file associated with an audio file.</p>
newtype JobAlbumArt = JobAlbumArt 
  { "MergePolicy" :: NullOrUndefined (MergePolicy)
  , "Artwork" :: NullOrUndefined (Artworks)
  }


newtype JobContainer = JobContainer String


-- | <p>Information about the file that you're transcoding.</p>
newtype JobInput = JobInput 
  { "Key" :: NullOrUndefined (LongKey)
  , "FrameRate" :: NullOrUndefined (FrameRate)
  , "Resolution" :: NullOrUndefined (Resolution)
  , "AspectRatio" :: NullOrUndefined (AspectRatio)
  , "Interlaced" :: NullOrUndefined (Interlaced)
  , "Container" :: NullOrUndefined (JobContainer)
  , "Encryption" :: NullOrUndefined (Encryption)
  , "TimeSpan" :: NullOrUndefined (TimeSpan)
  , "InputCaptions" :: NullOrUndefined (InputCaptions)
  , "DetectedProperties" :: NullOrUndefined (DetectedProperties)
  }


newtype JobInputs = JobInputs (Array JobInput)


-- | <important> <p>Outputs recommended instead.</p> </important> <p>If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the <code>Output</code> object lists information about the first output. This duplicates the information that is listed for the first output in the <code>Outputs</code> object.</p>
newtype JobOutput = JobOutput 
  { "Id" :: NullOrUndefined (String)
  , "Key" :: NullOrUndefined (Key)
  , "ThumbnailPattern" :: NullOrUndefined (ThumbnailPattern)
  , "ThumbnailEncryption" :: NullOrUndefined (Encryption)
  , "Rotate" :: NullOrUndefined (Rotate)
  , "PresetId" :: NullOrUndefined (Id)
  , "SegmentDuration" :: NullOrUndefined (FloatString)
  , "Status" :: NullOrUndefined (JobStatus)
  , "StatusDetail" :: NullOrUndefined (Description)
  , "Duration" :: NullOrUndefined (NullableLong)
  , "Width" :: NullOrUndefined (NullableInteger)
  , "Height" :: NullOrUndefined (NullableInteger)
  , "FrameRate" :: NullOrUndefined (FloatString)
  , "FileSize" :: NullOrUndefined (NullableLong)
  , "DurationMillis" :: NullOrUndefined (NullableLong)
  , "Watermarks" :: NullOrUndefined (JobWatermarks)
  , "AlbumArt" :: NullOrUndefined (JobAlbumArt)
  , "Composition" :: NullOrUndefined (Composition)
  , "Captions" :: NullOrUndefined (Captions)
  , "Encryption" :: NullOrUndefined (Encryption)
  , "AppliedColorSpaceConversion" :: NullOrUndefined (String)
  }


newtype JobOutputs = JobOutputs (Array JobOutput)


newtype JobStatus = JobStatus String


-- | <p>Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.</p>
newtype JobWatermark = JobWatermark 
  { "PresetWatermarkId" :: NullOrUndefined (PresetWatermarkId)
  , "InputKey" :: NullOrUndefined (WatermarkKey)
  , "Encryption" :: NullOrUndefined (Encryption)
  }


newtype JobWatermarks = JobWatermarks (Array JobWatermark)


newtype Jobs = Jobs (Array Job)


newtype JpgOrPng = JpgOrPng String


newtype Key = Key String


newtype KeyArn = KeyArn String


newtype KeyIdGuid = KeyIdGuid String


newtype KeyStoragePolicy = KeyStoragePolicy String


newtype KeyframesMaxDist = KeyframesMaxDist String


-- | <p>Too many operations for a given AWS account. For example, the number of pipelines exceeds the maximum allowed.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }


-- | <p>The <code>ListJobsByPipelineRequest</code> structure.</p>
newtype ListJobsByPipelineRequest = ListJobsByPipelineRequest 
  { "PipelineId" :: (Id)
  , "Ascending" :: NullOrUndefined (Ascending)
  , "PageToken" :: NullOrUndefined (Id)
  }


-- | <p>The <code>ListJobsByPipelineResponse</code> structure.</p>
newtype ListJobsByPipelineResponse = ListJobsByPipelineResponse 
  { "Jobs" :: NullOrUndefined (Jobs)
  , "NextPageToken" :: NullOrUndefined (Id)
  }


-- | <p>The <code>ListJobsByStatusRequest</code> structure.</p>
newtype ListJobsByStatusRequest = ListJobsByStatusRequest 
  { "Status" :: (JobStatus)
  , "Ascending" :: NullOrUndefined (Ascending)
  , "PageToken" :: NullOrUndefined (Id)
  }


-- | <p> The <code>ListJobsByStatusResponse</code> structure. </p>
newtype ListJobsByStatusResponse = ListJobsByStatusResponse 
  { "Jobs" :: NullOrUndefined (Jobs)
  , "NextPageToken" :: NullOrUndefined (Id)
  }


-- | <p>The <code>ListPipelineRequest</code> structure.</p>
newtype ListPipelinesRequest = ListPipelinesRequest 
  { "Ascending" :: NullOrUndefined (Ascending)
  , "PageToken" :: NullOrUndefined (Id)
  }


-- | <p>A list of the pipelines associated with the current AWS account.</p>
newtype ListPipelinesResponse = ListPipelinesResponse 
  { "Pipelines" :: NullOrUndefined (Pipelines)
  , "NextPageToken" :: NullOrUndefined (Id)
  }


-- | <p>The <code>ListPresetsRequest</code> structure.</p>
newtype ListPresetsRequest = ListPresetsRequest 
  { "Ascending" :: NullOrUndefined (Ascending)
  , "PageToken" :: NullOrUndefined (Id)
  }


-- | <p>The <code>ListPresetsResponse</code> structure.</p>
newtype ListPresetsResponse = ListPresetsResponse 
  { "Presets" :: NullOrUndefined (Presets)
  , "NextPageToken" :: NullOrUndefined (Id)
  }


newtype LongKey = LongKey String


newtype MaxFrameRate = MaxFrameRate String


newtype MergePolicy = MergePolicy String


newtype Name = Name String


newtype NonEmptyBase64EncodedString = NonEmptyBase64EncodedString String


-- | <p>The Amazon Simple Notification Service (Amazon SNS) topic or topics to notify in order to report job status.</p> <important> <p>To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.</p> </important>
newtype Notifications = Notifications 
  { "Progressing" :: NullOrUndefined (SnsTopic)
  , "Completed" :: NullOrUndefined (SnsTopic)
  , "Warning" :: NullOrUndefined (SnsTopic)
  , "Error" :: NullOrUndefined (SnsTopic)
  }


newtype NullableInteger = NullableInteger Int


newtype NullableLong = NullableLong Number


newtype OneTo512String = OneTo512String String


newtype Opacity = Opacity String


newtype OutputKeys = OutputKeys (Array Key)


newtype PaddingPolicy = PaddingPolicy String


-- | <p>The <code>Permission</code> structure.</p>
newtype Permission = Permission 
  { "GranteeType" :: NullOrUndefined (GranteeType)
  , "Grantee" :: NullOrUndefined (Grantee)
  , "Access" :: NullOrUndefined (AccessControls)
  }


newtype Permissions = Permissions (Array Permission)


-- | <p>The pipeline (queue) that is used to manage jobs.</p>
newtype Pipeline = Pipeline 
  { "Id" :: NullOrUndefined (Id)
  , "Arn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (Name)
  , "Status" :: NullOrUndefined (PipelineStatus)
  , "InputBucket" :: NullOrUndefined (BucketName)
  , "OutputBucket" :: NullOrUndefined (BucketName)
  , "Role" :: NullOrUndefined (Role)
  , "AwsKmsKeyArn" :: NullOrUndefined (KeyArn)
  , "Notifications" :: NullOrUndefined (Notifications)
  , "ContentConfig" :: NullOrUndefined (PipelineOutputConfig)
  , "ThumbnailConfig" :: NullOrUndefined (PipelineOutputConfig)
  }


-- | <p>The <code>PipelineOutputConfig</code> structure.</p>
newtype PipelineOutputConfig = PipelineOutputConfig 
  { "Bucket" :: NullOrUndefined (BucketName)
  , "StorageClass" :: NullOrUndefined (StorageClass)
  , "Permissions" :: NullOrUndefined (Permissions)
  }


newtype PipelineStatus = PipelineStatus String


newtype Pipelines = Pipelines (Array Pipeline)


newtype PixelsOrPercent = PixelsOrPercent String


-- | <p>The PlayReady DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.</p> <p>PlayReady DRM encrypts your media files using <code>AES-CTR</code> encryption.</p> <p>If you use DRM for an <code>HLSv3</code> playlist, your outputs must have a master playlist.</p>
newtype PlayReadyDrm = PlayReadyDrm 
  { "Format" :: NullOrUndefined (PlayReadyDrmFormatString)
  , "Key" :: NullOrUndefined (NonEmptyBase64EncodedString)
  , "KeyMd5" :: NullOrUndefined (NonEmptyBase64EncodedString)
  , "KeyId" :: NullOrUndefined (KeyIdGuid)
  , "InitializationVector" :: NullOrUndefined (ZeroTo255String)
  , "LicenseAcquisitionUrl" :: NullOrUndefined (OneTo512String)
  }


newtype PlayReadyDrmFormatString = PlayReadyDrmFormatString String


-- | <p> Use Only for Fragmented MP4 or MPEG-TS Outputs. If you specify a preset for which the value of Container is <code>fmp4</code> (Fragmented MP4) or <code>ts</code> (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create. We recommend that you create only one master playlist per output format. The maximum number of master playlists in a job is 30. </p>
newtype Playlist = Playlist 
  { "Name" :: NullOrUndefined (Filename)
  , "Format" :: NullOrUndefined (PlaylistFormat)
  , "OutputKeys" :: NullOrUndefined (OutputKeys)
  , "HlsContentProtection" :: NullOrUndefined (HlsContentProtection)
  , "PlayReadyDrm" :: NullOrUndefined (PlayReadyDrm)
  , "Status" :: NullOrUndefined (JobStatus)
  , "StatusDetail" :: NullOrUndefined (Description)
  }


newtype PlaylistFormat = PlaylistFormat String


newtype Playlists = Playlists (Array Playlist)


-- | <p>Presets are templates that contain most of the settings for transcoding media files from one format to another. Elastic Transcoder includes some default presets for common formats, for example, several iPod and iPhone versions. You can also create your own presets for formats that aren't included among the default presets. You specify which preset you want to use when you create a job.</p>
newtype Preset = Preset 
  { "Id" :: NullOrUndefined (Id)
  , "Arn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (Name)
  , "Description" :: NullOrUndefined (Description)
  , "Container" :: NullOrUndefined (PresetContainer)
  , "Audio" :: NullOrUndefined (AudioParameters)
  , "Video" :: NullOrUndefined (VideoParameters)
  , "Thumbnails" :: NullOrUndefined (Thumbnails)
  , "Type" :: NullOrUndefined (PresetType)
  }


newtype PresetContainer = PresetContainer String


newtype PresetType = PresetType String


-- | <p>Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video.</p> <p>Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.</p> <p>When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.</p>
newtype PresetWatermark = PresetWatermark 
  { "Id" :: NullOrUndefined (PresetWatermarkId)
  , "MaxWidth" :: NullOrUndefined (PixelsOrPercent)
  , "MaxHeight" :: NullOrUndefined (PixelsOrPercent)
  , "SizingPolicy" :: NullOrUndefined (WatermarkSizingPolicy)
  , "HorizontalAlign" :: NullOrUndefined (HorizontalAlign)
  , "HorizontalOffset" :: NullOrUndefined (PixelsOrPercent)
  , "VerticalAlign" :: NullOrUndefined (VerticalAlign)
  , "VerticalOffset" :: NullOrUndefined (PixelsOrPercent)
  , "Opacity" :: NullOrUndefined (Opacity)
  , "Target" :: NullOrUndefined (Target)
  }


newtype PresetWatermarkId = PresetWatermarkId String


newtype PresetWatermarks = PresetWatermarks (Array PresetWatermark)


newtype Presets = Presets (Array Preset)


-- | <p>The <code>ReadJobRequest</code> structure.</p>
newtype ReadJobRequest = ReadJobRequest 
  { "Id" :: (Id)
  }


-- | <p>The <code>ReadJobResponse</code> structure.</p>
newtype ReadJobResponse = ReadJobResponse 
  { "Job" :: NullOrUndefined (Job)
  }


-- | <p>The <code>ReadPipelineRequest</code> structure.</p>
newtype ReadPipelineRequest = ReadPipelineRequest 
  { "Id" :: (Id)
  }


-- | <p>The <code>ReadPipelineResponse</code> structure.</p>
newtype ReadPipelineResponse = ReadPipelineResponse 
  { "Pipeline" :: NullOrUndefined (Pipeline)
  , "Warnings" :: NullOrUndefined (Warnings)
  }


-- | <p>The <code>ReadPresetRequest</code> structure.</p>
newtype ReadPresetRequest = ReadPresetRequest 
  { "Id" :: (Id)
  }


-- | <p>The <code>ReadPresetResponse</code> structure.</p>
newtype ReadPresetResponse = ReadPresetResponse 
  { "Preset" :: NullOrUndefined (Preset)
  }


newtype Resolution = Resolution String


-- | <p>The resource you are attempting to change is in use. For example, you are attempting to delete a pipeline that is currently in use.</p>
newtype ResourceInUseException = ResourceInUseException 
  { 
  }


-- | <p>The requested resource does not exist or is not available. For example, the pipeline to which you're trying to add a job doesn't exist or is still being created.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }


newtype Role = Role String


newtype Rotate = Rotate String


newtype SizingPolicy = SizingPolicy String


newtype SnsTopic = SnsTopic String


newtype SnsTopics = SnsTopics (Array SnsTopic)


newtype StorageClass = StorageClass String


newtype Success = Success String


newtype Target = Target String


-- | <p> The <code>TestRoleRequest</code> structure. </p>
newtype TestRoleRequest = TestRoleRequest 
  { "Role" :: (Role)
  , "InputBucket" :: (BucketName)
  , "OutputBucket" :: (BucketName)
  , "Topics" :: (SnsTopics)
  }


-- | <p>The <code>TestRoleResponse</code> structure.</p>
newtype TestRoleResponse = TestRoleResponse 
  { "Success" :: NullOrUndefined (Success)
  , "Messages" :: NullOrUndefined (ExceptionMessages)
  }


newtype ThumbnailPattern = ThumbnailPattern String


newtype ThumbnailResolution = ThumbnailResolution String


-- | <p>Thumbnails for videos.</p>
newtype Thumbnails = Thumbnails 
  { "Format" :: NullOrUndefined (JpgOrPng)
  , "Interval" :: NullOrUndefined (Digits)
  , "Resolution" :: NullOrUndefined (ThumbnailResolution)
  , "AspectRatio" :: NullOrUndefined (AspectRatio)
  , "MaxWidth" :: NullOrUndefined (DigitsOrAuto)
  , "MaxHeight" :: NullOrUndefined (DigitsOrAuto)
  , "SizingPolicy" :: NullOrUndefined (SizingPolicy)
  , "PaddingPolicy" :: NullOrUndefined (PaddingPolicy)
  }


newtype Time = Time String


newtype TimeOffset = TimeOffset String


-- | <p>Settings that determine when a clip begins and how long it lasts.</p>
newtype TimeSpan = TimeSpan 
  { "StartTime" :: NullOrUndefined (Time)
  , "Duration" :: NullOrUndefined (Time)
  }


-- | <p>Details about the timing of a job.</p>
newtype Timing = Timing 
  { "SubmitTimeMillis" :: NullOrUndefined (NullableLong)
  , "StartTimeMillis" :: NullOrUndefined (NullableLong)
  , "FinishTimeMillis" :: NullOrUndefined (NullableLong)
  }


-- | <p>The <code>UpdatePipelineNotificationsRequest</code> structure.</p>
newtype UpdatePipelineNotificationsRequest = UpdatePipelineNotificationsRequest 
  { "Id" :: (Id)
  , "Notifications" :: (Notifications)
  }


-- | <p>The <code>UpdatePipelineNotificationsResponse</code> structure.</p>
newtype UpdatePipelineNotificationsResponse = UpdatePipelineNotificationsResponse 
  { "Pipeline" :: NullOrUndefined (Pipeline)
  }


-- | <p>The <code>UpdatePipelineRequest</code> structure.</p>
newtype UpdatePipelineRequest = UpdatePipelineRequest 
  { "Id" :: (Id)
  , "Name" :: NullOrUndefined (Name)
  , "InputBucket" :: NullOrUndefined (BucketName)
  , "Role" :: NullOrUndefined (Role)
  , "AwsKmsKeyArn" :: NullOrUndefined (KeyArn)
  , "Notifications" :: NullOrUndefined (Notifications)
  , "ContentConfig" :: NullOrUndefined (PipelineOutputConfig)
  , "ThumbnailConfig" :: NullOrUndefined (PipelineOutputConfig)
  }


-- | <p>When you update a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>
newtype UpdatePipelineResponse = UpdatePipelineResponse 
  { "Pipeline" :: NullOrUndefined (Pipeline)
  , "Warnings" :: NullOrUndefined (Warnings)
  }


-- | <p>The <code>UpdatePipelineStatusRequest</code> structure.</p>
newtype UpdatePipelineStatusRequest = UpdatePipelineStatusRequest 
  { "Id" :: (Id)
  , "Status" :: (PipelineStatus)
  }


-- | <p>When you update status for a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>
newtype UpdatePipelineStatusResponse = UpdatePipelineStatusResponse 
  { "Pipeline" :: NullOrUndefined (Pipeline)
  }


newtype UserMetadata = UserMetadata (Map String String)


-- | <p>One or more required parameter values were not provided in the request.</p>
newtype ValidationException = ValidationException 
  { 
  }


newtype VerticalAlign = VerticalAlign String


newtype VideoBitRate = VideoBitRate String


newtype VideoCodec = VideoCodec String


-- | <p>The <code>VideoParameters</code> structure.</p>
newtype VideoParameters = VideoParameters 
  { "Codec" :: NullOrUndefined (VideoCodec)
  , "CodecOptions" :: NullOrUndefined (CodecOptions)
  , "KeyframesMaxDist" :: NullOrUndefined (KeyframesMaxDist)
  , "FixedGOP" :: NullOrUndefined (FixedGOP)
  , "BitRate" :: NullOrUndefined (VideoBitRate)
  , "FrameRate" :: NullOrUndefined (FrameRate)
  , "MaxFrameRate" :: NullOrUndefined (MaxFrameRate)
  , "Resolution" :: NullOrUndefined (Resolution)
  , "AspectRatio" :: NullOrUndefined (AspectRatio)
  , "MaxWidth" :: NullOrUndefined (DigitsOrAuto)
  , "MaxHeight" :: NullOrUndefined (DigitsOrAuto)
  , "DisplayAspectRatio" :: NullOrUndefined (AspectRatio)
  , "SizingPolicy" :: NullOrUndefined (SizingPolicy)
  , "PaddingPolicy" :: NullOrUndefined (PaddingPolicy)
  , "Watermarks" :: NullOrUndefined (PresetWatermarks)
  }


-- | <p>Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.</p> <p>Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.</p>
newtype Warning = Warning 
  { "Code" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }


newtype Warnings = Warnings (Array Warning)


newtype WatermarkKey = WatermarkKey String


newtype WatermarkSizingPolicy = WatermarkSizingPolicy String


newtype ZeroTo255String = ZeroTo255String String


newtype ZeroTo512String = ZeroTo512String String
