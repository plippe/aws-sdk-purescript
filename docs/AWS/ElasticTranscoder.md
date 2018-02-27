## Module AWS.ElasticTranscoder

<fullname>AWS Elastic Transcoder Service</fullname> <p>The AWS Elastic Transcoder Service.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `cancelJob`

``` purescript
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: RequestError | eff) CancelJobResponse
```

<p>The CancelJob operation cancels an unfinished job.</p> <note> <p>You can only cancel a job that has a status of <code>Submitted</code>. To prevent a pipeline from starting to process a job while you're getting the job identifier, use <a>UpdatePipelineStatus</a> to temporarily pause the pipeline.</p> </note>

#### `createJob`

``` purescript
createJob :: forall eff. CreateJobRequest -> Aff (err :: RequestError | eff) CreateJobResponse
```

<p>When you create a job, Elastic Transcoder returns JSON data that includes the values that you specified plus information about the job that is created.</p> <p>If you have specified more than one output for your jobs (for example, one output for the Kindle Fire and another output for the Apple iPhone 4s), you currently must use the Elastic Transcoder API to list the jobs (as opposed to the AWS Console).</p>

#### `createPipeline`

``` purescript
createPipeline :: forall eff. CreatePipelineRequest -> Aff (err :: RequestError | eff) CreatePipelineResponse
```

<p>The CreatePipeline operation creates a pipeline with settings that you specify.</p>

#### `createPreset`

``` purescript
createPreset :: forall eff. CreatePresetRequest -> Aff (err :: RequestError | eff) CreatePresetResponse
```

<p>The CreatePreset operation creates a preset with settings that you specify.</p> <important> <p>Elastic Transcoder checks the CreatePreset settings to ensure that they meet Elastic Transcoder requirements and to determine whether they comply with H.264 standards. If your settings are not valid for Elastic Transcoder, Elastic Transcoder returns an HTTP 400 response (<code>ValidationException</code>) and does not create the preset. If the settings are valid for Elastic Transcoder but aren't strictly compliant with the H.264 standard, Elastic Transcoder creates the preset and returns a warning message in the response. This helps you determine whether your settings comply with the H.264 standard while giving you greater flexibility with respect to the video that Elastic Transcoder produces.</p> </important> <p>Elastic Transcoder uses the H.264 video-compression format. For more information, see the International Telecommunication Union publication <i>Recommendation ITU-T H.264: Advanced video coding for generic audiovisual services</i>.</p>

#### `deletePipeline`

``` purescript
deletePipeline :: forall eff. DeletePipelineRequest -> Aff (err :: RequestError | eff) DeletePipelineResponse
```

<p>The DeletePipeline operation removes a pipeline.</p> <p> You can only delete a pipeline that has never been used or that is not currently in use (doesn't contain any active jobs). If the pipeline is currently in use, <code>DeletePipeline</code> returns an error. </p>

#### `deletePreset`

``` purescript
deletePreset :: forall eff. DeletePresetRequest -> Aff (err :: RequestError | eff) DeletePresetResponse
```

<p>The DeletePreset operation removes a preset that you've added in an AWS region.</p> <note> <p>You can't delete the default presets that are included with Elastic Transcoder.</p> </note>

#### `listJobsByPipeline`

``` purescript
listJobsByPipeline :: forall eff. ListJobsByPipelineRequest -> Aff (err :: RequestError | eff) ListJobsByPipelineResponse
```

<p>The ListJobsByPipeline operation gets a list of the jobs currently in a pipeline.</p> <p>Elastic Transcoder returns all of the jobs currently in the specified pipeline. The response body contains one element for each job that satisfies the search criteria.</p>

#### `listJobsByStatus`

``` purescript
listJobsByStatus :: forall eff. ListJobsByStatusRequest -> Aff (err :: RequestError | eff) ListJobsByStatusResponse
```

<p>The ListJobsByStatus operation gets a list of jobs that have a specified status. The response body contains one element for each job that satisfies the search criteria.</p>

#### `listPipelines`

``` purescript
listPipelines :: forall eff. ListPipelinesRequest -> Aff (err :: RequestError | eff) ListPipelinesResponse
```

<p>The ListPipelines operation gets a list of the pipelines associated with the current AWS account.</p>

#### `listPresets`

``` purescript
listPresets :: forall eff. ListPresetsRequest -> Aff (err :: RequestError | eff) ListPresetsResponse
```

<p>The ListPresets operation gets a list of the default presets included with Elastic Transcoder and the presets that you've added in an AWS region.</p>

#### `readJob`

``` purescript
readJob :: forall eff. ReadJobRequest -> Aff (err :: RequestError | eff) ReadJobResponse
```

<p>The ReadJob operation returns detailed information about a job.</p>

#### `readPipeline`

``` purescript
readPipeline :: forall eff. ReadPipelineRequest -> Aff (err :: RequestError | eff) ReadPipelineResponse
```

<p>The ReadPipeline operation gets detailed information about a pipeline.</p>

#### `readPreset`

``` purescript
readPreset :: forall eff. ReadPresetRequest -> Aff (err :: RequestError | eff) ReadPresetResponse
```

<p>The ReadPreset operation gets detailed information about a preset.</p>

#### `testRole`

``` purescript
testRole :: forall eff. TestRoleRequest -> Aff (err :: RequestError | eff) TestRoleResponse
```

<p>The TestRole operation tests the IAM role used to create the pipeline.</p> <p>The <code>TestRole</code> action lets you determine whether the IAM role you are using has sufficient permissions to let Elastic Transcoder perform tasks associated with the transcoding process. The action attempts to assume the specified IAM role, checks read access to the input and output buckets, and tries to send a test notification to Amazon SNS topics that you specify.</p>

#### `updatePipeline`

``` purescript
updatePipeline :: forall eff. UpdatePipelineRequest -> Aff (err :: RequestError | eff) UpdatePipelineResponse
```

<p> Use the <code>UpdatePipeline</code> operation to update settings for a pipeline.</p> <important> <p>When you change pipeline settings, your changes take effect immediately. Jobs that you have already submitted and that Elastic Transcoder has not started to process are affected in addition to jobs that you submit after you change settings. </p> </important>

#### `updatePipelineNotifications`

``` purescript
updatePipelineNotifications :: forall eff. UpdatePipelineNotificationsRequest -> Aff (err :: RequestError | eff) UpdatePipelineNotificationsResponse
```

<p>With the UpdatePipelineNotifications operation, you can update Amazon Simple Notification Service (Amazon SNS) notifications for a pipeline.</p> <p>When you update notifications for a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>

#### `updatePipelineStatus`

``` purescript
updatePipelineStatus :: forall eff. UpdatePipelineStatusRequest -> Aff (err :: RequestError | eff) UpdatePipelineStatusResponse
```

<p>The UpdatePipelineStatus operation pauses or reactivates a pipeline, so that the pipeline stops or restarts the processing of jobs.</p> <p>Changing the pipeline status is useful if you want to cancel one or more jobs. You can't cancel jobs after Elastic Transcoder has started processing them; if you pause the pipeline to which you submitted the jobs, you have more time to get the job IDs for the jobs that you want to cancel, and to send a <a>CancelJob</a> request. </p>

#### `AccessControl`

``` purescript
newtype AccessControl
  = AccessControl String
```

##### Instances
``` purescript
Newtype AccessControl _
```

#### `AccessControls`

``` purescript
newtype AccessControls
  = AccessControls (Array AccessControl)
```

##### Instances
``` purescript
Newtype AccessControls _
```

#### `AccessDeniedException`

``` purescript
newtype AccessDeniedException
  = AccessDeniedException {  }
```

<p>General authentication failure. The request was not signed correctly.</p>

##### Instances
``` purescript
Newtype AccessDeniedException _
```

#### `Artwork`

``` purescript
newtype Artwork
  = Artwork { "InputKey" :: NullOrUndefined (WatermarkKey), "MaxWidth" :: NullOrUndefined (DigitsOrAuto), "MaxHeight" :: NullOrUndefined (DigitsOrAuto), "SizingPolicy" :: NullOrUndefined (SizingPolicy), "PaddingPolicy" :: NullOrUndefined (PaddingPolicy), "AlbumArtFormat" :: NullOrUndefined (JpgOrPng), "Encryption" :: NullOrUndefined (Encryption) }
```

<p>The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20.</p> <p>To remove artwork or leave the artwork empty, you can either set <code>Artwork</code> to null, or set the <code>Merge Policy</code> to "Replace" and use an empty <code>Artwork</code> array.</p> <p>To pass through existing artwork unchanged, set the <code>Merge Policy</code> to "Prepend", "Append", or "Fallback", and use an empty <code>Artwork</code> array.</p>

##### Instances
``` purescript
Newtype Artwork _
```

#### `Artworks`

``` purescript
newtype Artworks
  = Artworks (Array Artwork)
```

##### Instances
``` purescript
Newtype Artworks _
```

#### `Ascending`

``` purescript
newtype Ascending
  = Ascending String
```

##### Instances
``` purescript
Newtype Ascending _
```

#### `AspectRatio`

``` purescript
newtype AspectRatio
  = AspectRatio String
```

##### Instances
``` purescript
Newtype AspectRatio _
```

#### `AudioBitDepth`

``` purescript
newtype AudioBitDepth
  = AudioBitDepth String
```

##### Instances
``` purescript
Newtype AudioBitDepth _
```

#### `AudioBitOrder`

``` purescript
newtype AudioBitOrder
  = AudioBitOrder String
```

##### Instances
``` purescript
Newtype AudioBitOrder _
```

#### `AudioBitRate`

``` purescript
newtype AudioBitRate
  = AudioBitRate String
```

##### Instances
``` purescript
Newtype AudioBitRate _
```

#### `AudioChannels`

``` purescript
newtype AudioChannels
  = AudioChannels String
```

##### Instances
``` purescript
Newtype AudioChannels _
```

#### `AudioCodec`

``` purescript
newtype AudioCodec
  = AudioCodec String
```

##### Instances
``` purescript
Newtype AudioCodec _
```

#### `AudioCodecOptions`

``` purescript
newtype AudioCodecOptions
  = AudioCodecOptions { "Profile" :: NullOrUndefined (AudioCodecProfile), "BitDepth" :: NullOrUndefined (AudioBitDepth), "BitOrder" :: NullOrUndefined (AudioBitOrder), "Signed" :: NullOrUndefined (AudioSigned) }
```

<p>Options associated with your audio codec.</p>

##### Instances
``` purescript
Newtype AudioCodecOptions _
```

#### `AudioCodecProfile`

``` purescript
newtype AudioCodecProfile
  = AudioCodecProfile String
```

##### Instances
``` purescript
Newtype AudioCodecProfile _
```

#### `AudioPackingMode`

``` purescript
newtype AudioPackingMode
  = AudioPackingMode String
```

##### Instances
``` purescript
Newtype AudioPackingMode _
```

#### `AudioParameters`

``` purescript
newtype AudioParameters
  = AudioParameters { "Codec" :: NullOrUndefined (AudioCodec), "SampleRate" :: NullOrUndefined (AudioSampleRate), "BitRate" :: NullOrUndefined (AudioBitRate), "Channels" :: NullOrUndefined (AudioChannels), "AudioPackingMode" :: NullOrUndefined (AudioPackingMode), "CodecOptions" :: NullOrUndefined (AudioCodecOptions) }
```

<p>Parameters required for transcoding audio.</p>

##### Instances
``` purescript
Newtype AudioParameters _
```

#### `AudioSampleRate`

``` purescript
newtype AudioSampleRate
  = AudioSampleRate String
```

##### Instances
``` purescript
Newtype AudioSampleRate _
```

#### `AudioSigned`

``` purescript
newtype AudioSigned
  = AudioSigned String
```

##### Instances
``` purescript
Newtype AudioSigned _
```

#### `Base64EncodedString`

``` purescript
newtype Base64EncodedString
  = Base64EncodedString String
```

##### Instances
``` purescript
Newtype Base64EncodedString _
```

#### `BucketName`

``` purescript
newtype BucketName
  = BucketName String
```

##### Instances
``` purescript
Newtype BucketName _
```

#### `CancelJobRequest`

``` purescript
newtype CancelJobRequest
  = CancelJobRequest { "Id" :: Id }
```

<p>The <code>CancelJobRequest</code> structure.</p>

##### Instances
``` purescript
Newtype CancelJobRequest _
```

#### `CancelJobResponse`

``` purescript
newtype CancelJobResponse
  = CancelJobResponse {  }
```

<p>The response body contains a JSON object. If the job is successfully canceled, the value of <code>Success</code> is <code>true</code>.</p>

##### Instances
``` purescript
Newtype CancelJobResponse _
```

#### `CaptionFormat`

``` purescript
newtype CaptionFormat
  = CaptionFormat { "Format" :: NullOrUndefined (CaptionFormatFormat), "Pattern" :: NullOrUndefined (CaptionFormatPattern), "Encryption" :: NullOrUndefined (Encryption) }
```

<p>The file format of the output captions. If you leave this value blank, Elastic Transcoder returns an error.</p>

##### Instances
``` purescript
Newtype CaptionFormat _
```

#### `CaptionFormatFormat`

``` purescript
newtype CaptionFormatFormat
  = CaptionFormatFormat String
```

##### Instances
``` purescript
Newtype CaptionFormatFormat _
```

#### `CaptionFormatPattern`

``` purescript
newtype CaptionFormatPattern
  = CaptionFormatPattern String
```

##### Instances
``` purescript
Newtype CaptionFormatPattern _
```

#### `CaptionFormats`

``` purescript
newtype CaptionFormats
  = CaptionFormats (Array CaptionFormat)
```

##### Instances
``` purescript
Newtype CaptionFormats _
```

#### `CaptionMergePolicy`

``` purescript
newtype CaptionMergePolicy
  = CaptionMergePolicy String
```

##### Instances
``` purescript
Newtype CaptionMergePolicy _
```

#### `CaptionSource`

``` purescript
newtype CaptionSource
  = CaptionSource { "Key" :: NullOrUndefined (LongKey), "Language" :: NullOrUndefined (Key), "TimeOffset" :: NullOrUndefined (TimeOffset), "Label" :: NullOrUndefined (Name), "Encryption" :: NullOrUndefined (Encryption) }
```

<p>A source file for the input sidecar captions used during the transcoding process.</p>

##### Instances
``` purescript
Newtype CaptionSource _
```

#### `CaptionSources`

``` purescript
newtype CaptionSources
  = CaptionSources (Array CaptionSource)
```

##### Instances
``` purescript
Newtype CaptionSources _
```

#### `Captions`

``` purescript
newtype Captions
  = Captions { "MergePolicy" :: NullOrUndefined (CaptionMergePolicy), "CaptionSources" :: NullOrUndefined (CaptionSources), "CaptionFormats" :: NullOrUndefined (CaptionFormats) }
```

<p>The captions to be created, if any.</p>

##### Instances
``` purescript
Newtype Captions _
```

#### `Clip`

``` purescript
newtype Clip
  = Clip { "TimeSpan" :: NullOrUndefined (TimeSpan) }
```

<p>Settings for one clip in a composition. All jobs in a playlist must have the same clip settings.</p>

##### Instances
``` purescript
Newtype Clip _
```

#### `CodecOption`

``` purescript
newtype CodecOption
  = CodecOption String
```

##### Instances
``` purescript
Newtype CodecOption _
```

#### `CodecOptions`

``` purescript
newtype CodecOptions
  = CodecOptions (Map CodecOption CodecOption)
```

##### Instances
``` purescript
Newtype CodecOptions _
```

#### `Composition`

``` purescript
newtype Composition
  = Composition (Array Clip)
```

##### Instances
``` purescript
Newtype Composition _
```

#### `CreateJobOutput`

``` purescript
newtype CreateJobOutput
  = CreateJobOutput { "Key" :: NullOrUndefined (Key), "ThumbnailPattern" :: NullOrUndefined (ThumbnailPattern), "ThumbnailEncryption" :: NullOrUndefined (Encryption), "Rotate" :: NullOrUndefined (Rotate), "PresetId" :: NullOrUndefined (Id), "SegmentDuration" :: NullOrUndefined (FloatString), "Watermarks" :: NullOrUndefined (JobWatermarks), "AlbumArt" :: NullOrUndefined (JobAlbumArt), "Composition" :: NullOrUndefined (Composition), "Captions" :: NullOrUndefined (Captions), "Encryption" :: NullOrUndefined (Encryption) }
```

<p>The <code>CreateJobOutput</code> structure.</p>

##### Instances
``` purescript
Newtype CreateJobOutput _
```

#### `CreateJobOutputs`

``` purescript
newtype CreateJobOutputs
  = CreateJobOutputs (Array CreateJobOutput)
```

##### Instances
``` purescript
Newtype CreateJobOutputs _
```

#### `CreateJobPlaylist`

``` purescript
newtype CreateJobPlaylist
  = CreateJobPlaylist { "Name" :: NullOrUndefined (Filename), "Format" :: NullOrUndefined (PlaylistFormat), "OutputKeys" :: NullOrUndefined (OutputKeys), "HlsContentProtection" :: NullOrUndefined (HlsContentProtection), "PlayReadyDrm" :: NullOrUndefined (PlayReadyDrm) }
```

<p>Information about the master playlist.</p>

##### Instances
``` purescript
Newtype CreateJobPlaylist _
```

#### `CreateJobPlaylists`

``` purescript
newtype CreateJobPlaylists
  = CreateJobPlaylists (Array CreateJobPlaylist)
```

##### Instances
``` purescript
Newtype CreateJobPlaylists _
```

#### `CreateJobRequest`

``` purescript
newtype CreateJobRequest
  = CreateJobRequest { "PipelineId" :: Id, "Input" :: NullOrUndefined (JobInput), "Inputs" :: NullOrUndefined (JobInputs), "Output" :: NullOrUndefined (CreateJobOutput), "Outputs" :: NullOrUndefined (CreateJobOutputs), "OutputKeyPrefix" :: NullOrUndefined (Key), "Playlists" :: NullOrUndefined (CreateJobPlaylists), "UserMetadata" :: NullOrUndefined (UserMetadata) }
```

<p>The <code>CreateJobRequest</code> structure.</p>

##### Instances
``` purescript
Newtype CreateJobRequest _
```

#### `CreateJobResponse`

``` purescript
newtype CreateJobResponse
  = CreateJobResponse { "Job" :: NullOrUndefined (Job) }
```

<p>The CreateJobResponse structure.</p>

##### Instances
``` purescript
Newtype CreateJobResponse _
```

#### `CreatePipelineRequest`

``` purescript
newtype CreatePipelineRequest
  = CreatePipelineRequest { "Name" :: Name, "InputBucket" :: BucketName, "OutputBucket" :: NullOrUndefined (BucketName), "Role" :: Role, "AwsKmsKeyArn" :: NullOrUndefined (KeyArn), "Notifications" :: NullOrUndefined (Notifications), "ContentConfig" :: NullOrUndefined (PipelineOutputConfig), "ThumbnailConfig" :: NullOrUndefined (PipelineOutputConfig) }
```

<p>The <code>CreatePipelineRequest</code> structure.</p>

##### Instances
``` purescript
Newtype CreatePipelineRequest _
```

#### `CreatePipelineResponse`

``` purescript
newtype CreatePipelineResponse
  = CreatePipelineResponse { "Pipeline" :: NullOrUndefined (Pipeline), "Warnings" :: NullOrUndefined (Warnings) }
```

<p>When you create a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>

##### Instances
``` purescript
Newtype CreatePipelineResponse _
```

#### `CreatePresetRequest`

``` purescript
newtype CreatePresetRequest
  = CreatePresetRequest { "Name" :: Name, "Description" :: NullOrUndefined (Description), "Container" :: PresetContainer, "Video" :: NullOrUndefined (VideoParameters), "Audio" :: NullOrUndefined (AudioParameters), "Thumbnails" :: NullOrUndefined (Thumbnails) }
```

<p>The <code>CreatePresetRequest</code> structure.</p>

##### Instances
``` purescript
Newtype CreatePresetRequest _
```

#### `CreatePresetResponse`

``` purescript
newtype CreatePresetResponse
  = CreatePresetResponse { "Preset" :: NullOrUndefined (Preset), "Warning" :: NullOrUndefined (String) }
```

<p>The <code>CreatePresetResponse</code> structure.</p>

##### Instances
``` purescript
Newtype CreatePresetResponse _
```

#### `DeletePipelineRequest`

``` purescript
newtype DeletePipelineRequest
  = DeletePipelineRequest { "Id" :: Id }
```

<p>The <code>DeletePipelineRequest</code> structure.</p>

##### Instances
``` purescript
Newtype DeletePipelineRequest _
```

#### `DeletePipelineResponse`

``` purescript
newtype DeletePipelineResponse
  = DeletePipelineResponse {  }
```

<p>The <code>DeletePipelineResponse</code> structure.</p>

##### Instances
``` purescript
Newtype DeletePipelineResponse _
```

#### `DeletePresetRequest`

``` purescript
newtype DeletePresetRequest
  = DeletePresetRequest { "Id" :: Id }
```

<p>The <code>DeletePresetRequest</code> structure.</p>

##### Instances
``` purescript
Newtype DeletePresetRequest _
```

#### `DeletePresetResponse`

``` purescript
newtype DeletePresetResponse
  = DeletePresetResponse {  }
```

<p>The <code>DeletePresetResponse</code> structure.</p>

##### Instances
``` purescript
Newtype DeletePresetResponse _
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

##### Instances
``` purescript
Newtype Description _
```

#### `DetectedProperties`

``` purescript
newtype DetectedProperties
  = DetectedProperties { "Width" :: NullOrUndefined (NullableInteger), "Height" :: NullOrUndefined (NullableInteger), "FrameRate" :: NullOrUndefined (FloatString), "FileSize" :: NullOrUndefined (NullableLong), "DurationMillis" :: NullOrUndefined (NullableLong) }
```

<p>The detected properties of the input file. Elastic Transcoder identifies these values from the input file.</p>

##### Instances
``` purescript
Newtype DetectedProperties _
```

#### `Digits`

``` purescript
newtype Digits
  = Digits String
```

##### Instances
``` purescript
Newtype Digits _
```

#### `DigitsOrAuto`

``` purescript
newtype DigitsOrAuto
  = DigitsOrAuto String
```

##### Instances
``` purescript
Newtype DigitsOrAuto _
```

#### `Encryption`

``` purescript
newtype Encryption
  = Encryption { "Mode" :: NullOrUndefined (EncryptionMode), "Key" :: NullOrUndefined (Base64EncodedString), "KeyMd5" :: NullOrUndefined (Base64EncodedString), "InitializationVector" :: NullOrUndefined (ZeroTo255String) }
```

<p>The encryption settings, if any, that are used for decrypting your input files or encrypting your output files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file, otherwise you must specify the mode you want Elastic Transcoder to use to encrypt your output files.</p>

##### Instances
``` purescript
Newtype Encryption _
```

#### `EncryptionMode`

``` purescript
newtype EncryptionMode
  = EncryptionMode String
```

##### Instances
``` purescript
Newtype EncryptionMode _
```

#### `ExceptionMessages`

``` purescript
newtype ExceptionMessages
  = ExceptionMessages (Array String)
```

##### Instances
``` purescript
Newtype ExceptionMessages _
```

#### `Filename`

``` purescript
newtype Filename
  = Filename String
```

##### Instances
``` purescript
Newtype Filename _
```

#### `FixedGOP`

``` purescript
newtype FixedGOP
  = FixedGOP String
```

##### Instances
``` purescript
Newtype FixedGOP _
```

#### `FloatString`

``` purescript
newtype FloatString
  = FloatString String
```

##### Instances
``` purescript
Newtype FloatString _
```

#### `FrameRate`

``` purescript
newtype FrameRate
  = FrameRate String
```

##### Instances
``` purescript
Newtype FrameRate _
```

#### `Grantee`

``` purescript
newtype Grantee
  = Grantee String
```

##### Instances
``` purescript
Newtype Grantee _
```

#### `GranteeType`

``` purescript
newtype GranteeType
  = GranteeType String
```

##### Instances
``` purescript
Newtype GranteeType _
```

#### `HlsContentProtection`

``` purescript
newtype HlsContentProtection
  = HlsContentProtection { "Method" :: NullOrUndefined (HlsContentProtectionMethod), "Key" :: NullOrUndefined (Base64EncodedString), "KeyMd5" :: NullOrUndefined (Base64EncodedString), "InitializationVector" :: NullOrUndefined (ZeroTo255String), "LicenseAcquisitionUrl" :: NullOrUndefined (ZeroTo512String), "KeyStoragePolicy" :: NullOrUndefined (KeyStoragePolicy) }
```

<p>The HLS content protection settings, if any, that you want Elastic Transcoder to apply to your output files.</p>

##### Instances
``` purescript
Newtype HlsContentProtection _
```

#### `HlsContentProtectionMethod`

``` purescript
newtype HlsContentProtectionMethod
  = HlsContentProtectionMethod String
```

##### Instances
``` purescript
Newtype HlsContentProtectionMethod _
```

#### `HorizontalAlign`

``` purescript
newtype HorizontalAlign
  = HorizontalAlign String
```

##### Instances
``` purescript
Newtype HorizontalAlign _
```

#### `Id`

``` purescript
newtype Id
  = Id String
```

##### Instances
``` purescript
Newtype Id _
```

#### `IncompatibleVersionException`

``` purescript
newtype IncompatibleVersionException
  = IncompatibleVersionException {  }
```

##### Instances
``` purescript
Newtype IncompatibleVersionException _
```

#### `InputCaptions`

``` purescript
newtype InputCaptions
  = InputCaptions { "MergePolicy" :: NullOrUndefined (CaptionMergePolicy), "CaptionSources" :: NullOrUndefined (CaptionSources) }
```

<p>The captions to be created, if any.</p>

##### Instances
``` purescript
Newtype InputCaptions _
```

#### `Interlaced`

``` purescript
newtype Interlaced
  = Interlaced String
```

##### Instances
``` purescript
Newtype Interlaced _
```

#### `InternalServiceException`

``` purescript
newtype InternalServiceException
  = InternalServiceException {  }
```

<p>Elastic Transcoder encountered an unexpected exception while trying to fulfill the request.</p>

##### Instances
``` purescript
Newtype InternalServiceException _
```

#### `Job`

``` purescript
newtype Job
  = Job { "Id" :: NullOrUndefined (Id), "Arn" :: NullOrUndefined (String), "PipelineId" :: NullOrUndefined (Id), "Input" :: NullOrUndefined (JobInput), "Inputs" :: NullOrUndefined (JobInputs), "Output" :: NullOrUndefined (JobOutput), "Outputs" :: NullOrUndefined (JobOutputs), "OutputKeyPrefix" :: NullOrUndefined (Key), "Playlists" :: NullOrUndefined (Playlists), "Status" :: NullOrUndefined (JobStatus), "UserMetadata" :: NullOrUndefined (UserMetadata), "Timing" :: NullOrUndefined (Timing) }
```

<p>A section of the response body that provides information about the job that is created.</p>

##### Instances
``` purescript
Newtype Job _
```

#### `JobAlbumArt`

``` purescript
newtype JobAlbumArt
  = JobAlbumArt { "MergePolicy" :: NullOrUndefined (MergePolicy), "Artwork" :: NullOrUndefined (Artworks) }
```

<p>The .jpg or .png file associated with an audio file.</p>

##### Instances
``` purescript
Newtype JobAlbumArt _
```

#### `JobContainer`

``` purescript
newtype JobContainer
  = JobContainer String
```

##### Instances
``` purescript
Newtype JobContainer _
```

#### `JobInput`

``` purescript
newtype JobInput
  = JobInput { "Key" :: NullOrUndefined (LongKey), "FrameRate" :: NullOrUndefined (FrameRate), "Resolution" :: NullOrUndefined (Resolution), "AspectRatio" :: NullOrUndefined (AspectRatio), "Interlaced" :: NullOrUndefined (Interlaced), "Container" :: NullOrUndefined (JobContainer), "Encryption" :: NullOrUndefined (Encryption), "TimeSpan" :: NullOrUndefined (TimeSpan), "InputCaptions" :: NullOrUndefined (InputCaptions), "DetectedProperties" :: NullOrUndefined (DetectedProperties) }
```

<p>Information about the file that you're transcoding.</p>

##### Instances
``` purescript
Newtype JobInput _
```

#### `JobInputs`

``` purescript
newtype JobInputs
  = JobInputs (Array JobInput)
```

##### Instances
``` purescript
Newtype JobInputs _
```

#### `JobOutput`

``` purescript
newtype JobOutput
  = JobOutput { "Id" :: NullOrUndefined (String), "Key" :: NullOrUndefined (Key), "ThumbnailPattern" :: NullOrUndefined (ThumbnailPattern), "ThumbnailEncryption" :: NullOrUndefined (Encryption), "Rotate" :: NullOrUndefined (Rotate), "PresetId" :: NullOrUndefined (Id), "SegmentDuration" :: NullOrUndefined (FloatString), "Status" :: NullOrUndefined (JobStatus), "StatusDetail" :: NullOrUndefined (Description), "Duration" :: NullOrUndefined (NullableLong), "Width" :: NullOrUndefined (NullableInteger), "Height" :: NullOrUndefined (NullableInteger), "FrameRate" :: NullOrUndefined (FloatString), "FileSize" :: NullOrUndefined (NullableLong), "DurationMillis" :: NullOrUndefined (NullableLong), "Watermarks" :: NullOrUndefined (JobWatermarks), "AlbumArt" :: NullOrUndefined (JobAlbumArt), "Composition" :: NullOrUndefined (Composition), "Captions" :: NullOrUndefined (Captions), "Encryption" :: NullOrUndefined (Encryption), "AppliedColorSpaceConversion" :: NullOrUndefined (String) }
```

<important> <p>Outputs recommended instead.</p> </important> <p>If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the <code>Output</code> object lists information about the first output. This duplicates the information that is listed for the first output in the <code>Outputs</code> object.</p>

##### Instances
``` purescript
Newtype JobOutput _
```

#### `JobOutputs`

``` purescript
newtype JobOutputs
  = JobOutputs (Array JobOutput)
```

##### Instances
``` purescript
Newtype JobOutputs _
```

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

##### Instances
``` purescript
Newtype JobStatus _
```

#### `JobWatermark`

``` purescript
newtype JobWatermark
  = JobWatermark { "PresetWatermarkId" :: NullOrUndefined (PresetWatermarkId), "InputKey" :: NullOrUndefined (WatermarkKey), "Encryption" :: NullOrUndefined (Encryption) }
```

<p>Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.</p>

##### Instances
``` purescript
Newtype JobWatermark _
```

#### `JobWatermarks`

``` purescript
newtype JobWatermarks
  = JobWatermarks (Array JobWatermark)
```

##### Instances
``` purescript
Newtype JobWatermarks _
```

#### `Jobs`

``` purescript
newtype Jobs
  = Jobs (Array Job)
```

##### Instances
``` purescript
Newtype Jobs _
```

#### `JpgOrPng`

``` purescript
newtype JpgOrPng
  = JpgOrPng String
```

##### Instances
``` purescript
Newtype JpgOrPng _
```

#### `Key`

``` purescript
newtype Key
  = Key String
```

##### Instances
``` purescript
Newtype Key _
```

#### `KeyArn`

``` purescript
newtype KeyArn
  = KeyArn String
```

##### Instances
``` purescript
Newtype KeyArn _
```

#### `KeyIdGuid`

``` purescript
newtype KeyIdGuid
  = KeyIdGuid String
```

##### Instances
``` purescript
Newtype KeyIdGuid _
```

#### `KeyStoragePolicy`

``` purescript
newtype KeyStoragePolicy
  = KeyStoragePolicy String
```

##### Instances
``` purescript
Newtype KeyStoragePolicy _
```

#### `KeyframesMaxDist`

``` purescript
newtype KeyframesMaxDist
  = KeyframesMaxDist String
```

##### Instances
``` purescript
Newtype KeyframesMaxDist _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>Too many operations for a given AWS account. For example, the number of pipelines exceeds the maximum allowed.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListJobsByPipelineRequest`

``` purescript
newtype ListJobsByPipelineRequest
  = ListJobsByPipelineRequest { "PipelineId" :: Id, "Ascending" :: NullOrUndefined (Ascending), "PageToken" :: NullOrUndefined (Id) }
```

<p>The <code>ListJobsByPipelineRequest</code> structure.</p>

##### Instances
``` purescript
Newtype ListJobsByPipelineRequest _
```

#### `ListJobsByPipelineResponse`

``` purescript
newtype ListJobsByPipelineResponse
  = ListJobsByPipelineResponse { "Jobs" :: NullOrUndefined (Jobs), "NextPageToken" :: NullOrUndefined (Id) }
```

<p>The <code>ListJobsByPipelineResponse</code> structure.</p>

##### Instances
``` purescript
Newtype ListJobsByPipelineResponse _
```

#### `ListJobsByStatusRequest`

``` purescript
newtype ListJobsByStatusRequest
  = ListJobsByStatusRequest { "Status" :: JobStatus, "Ascending" :: NullOrUndefined (Ascending), "PageToken" :: NullOrUndefined (Id) }
```

<p>The <code>ListJobsByStatusRequest</code> structure.</p>

##### Instances
``` purescript
Newtype ListJobsByStatusRequest _
```

#### `ListJobsByStatusResponse`

``` purescript
newtype ListJobsByStatusResponse
  = ListJobsByStatusResponse { "Jobs" :: NullOrUndefined (Jobs), "NextPageToken" :: NullOrUndefined (Id) }
```

<p> The <code>ListJobsByStatusResponse</code> structure. </p>

##### Instances
``` purescript
Newtype ListJobsByStatusResponse _
```

#### `ListPipelinesRequest`

``` purescript
newtype ListPipelinesRequest
  = ListPipelinesRequest { "Ascending" :: NullOrUndefined (Ascending), "PageToken" :: NullOrUndefined (Id) }
```

<p>The <code>ListPipelineRequest</code> structure.</p>

##### Instances
``` purescript
Newtype ListPipelinesRequest _
```

#### `ListPipelinesResponse`

``` purescript
newtype ListPipelinesResponse
  = ListPipelinesResponse { "Pipelines" :: NullOrUndefined (Pipelines), "NextPageToken" :: NullOrUndefined (Id) }
```

<p>A list of the pipelines associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype ListPipelinesResponse _
```

#### `ListPresetsRequest`

``` purescript
newtype ListPresetsRequest
  = ListPresetsRequest { "Ascending" :: NullOrUndefined (Ascending), "PageToken" :: NullOrUndefined (Id) }
```

<p>The <code>ListPresetsRequest</code> structure.</p>

##### Instances
``` purescript
Newtype ListPresetsRequest _
```

#### `ListPresetsResponse`

``` purescript
newtype ListPresetsResponse
  = ListPresetsResponse { "Presets" :: NullOrUndefined (Presets), "NextPageToken" :: NullOrUndefined (Id) }
```

<p>The <code>ListPresetsResponse</code> structure.</p>

##### Instances
``` purescript
Newtype ListPresetsResponse _
```

#### `LongKey`

``` purescript
newtype LongKey
  = LongKey String
```

##### Instances
``` purescript
Newtype LongKey _
```

#### `MaxFrameRate`

``` purescript
newtype MaxFrameRate
  = MaxFrameRate String
```

##### Instances
``` purescript
Newtype MaxFrameRate _
```

#### `MergePolicy`

``` purescript
newtype MergePolicy
  = MergePolicy String
```

##### Instances
``` purescript
Newtype MergePolicy _
```

#### `Name`

``` purescript
newtype Name
  = Name String
```

##### Instances
``` purescript
Newtype Name _
```

#### `NonEmptyBase64EncodedString`

``` purescript
newtype NonEmptyBase64EncodedString
  = NonEmptyBase64EncodedString String
```

##### Instances
``` purescript
Newtype NonEmptyBase64EncodedString _
```

#### `Notifications`

``` purescript
newtype Notifications
  = Notifications { "Progressing" :: NullOrUndefined (SnsTopic), "Completed" :: NullOrUndefined (SnsTopic), "Warning" :: NullOrUndefined (SnsTopic), "Error" :: NullOrUndefined (SnsTopic) }
```

<p>The Amazon Simple Notification Service (Amazon SNS) topic or topics to notify in order to report job status.</p> <important> <p>To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.</p> </important>

##### Instances
``` purescript
Newtype Notifications _
```

#### `NullableInteger`

``` purescript
newtype NullableInteger
  = NullableInteger Int
```

##### Instances
``` purescript
Newtype NullableInteger _
```

#### `NullableLong`

``` purescript
newtype NullableLong
  = NullableLong Number
```

##### Instances
``` purescript
Newtype NullableLong _
```

#### `OneTo512String`

``` purescript
newtype OneTo512String
  = OneTo512String String
```

##### Instances
``` purescript
Newtype OneTo512String _
```

#### `Opacity`

``` purescript
newtype Opacity
  = Opacity String
```

##### Instances
``` purescript
Newtype Opacity _
```

#### `OutputKeys`

``` purescript
newtype OutputKeys
  = OutputKeys (Array Key)
```

##### Instances
``` purescript
Newtype OutputKeys _
```

#### `PaddingPolicy`

``` purescript
newtype PaddingPolicy
  = PaddingPolicy String
```

##### Instances
``` purescript
Newtype PaddingPolicy _
```

#### `Permission`

``` purescript
newtype Permission
  = Permission { "GranteeType" :: NullOrUndefined (GranteeType), "Grantee" :: NullOrUndefined (Grantee), "Access" :: NullOrUndefined (AccessControls) }
```

<p>The <code>Permission</code> structure.</p>

##### Instances
``` purescript
Newtype Permission _
```

#### `Permissions`

``` purescript
newtype Permissions
  = Permissions (Array Permission)
```

##### Instances
``` purescript
Newtype Permissions _
```

#### `Pipeline`

``` purescript
newtype Pipeline
  = Pipeline { "Id" :: NullOrUndefined (Id), "Arn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (Name), "Status" :: NullOrUndefined (PipelineStatus), "InputBucket" :: NullOrUndefined (BucketName), "OutputBucket" :: NullOrUndefined (BucketName), "Role" :: NullOrUndefined (Role), "AwsKmsKeyArn" :: NullOrUndefined (KeyArn), "Notifications" :: NullOrUndefined (Notifications), "ContentConfig" :: NullOrUndefined (PipelineOutputConfig), "ThumbnailConfig" :: NullOrUndefined (PipelineOutputConfig) }
```

<p>The pipeline (queue) that is used to manage jobs.</p>

##### Instances
``` purescript
Newtype Pipeline _
```

#### `PipelineOutputConfig`

``` purescript
newtype PipelineOutputConfig
  = PipelineOutputConfig { "Bucket" :: NullOrUndefined (BucketName), "StorageClass" :: NullOrUndefined (StorageClass), "Permissions" :: NullOrUndefined (Permissions) }
```

<p>The <code>PipelineOutputConfig</code> structure.</p>

##### Instances
``` purescript
Newtype PipelineOutputConfig _
```

#### `PipelineStatus`

``` purescript
newtype PipelineStatus
  = PipelineStatus String
```

##### Instances
``` purescript
Newtype PipelineStatus _
```

#### `Pipelines`

``` purescript
newtype Pipelines
  = Pipelines (Array Pipeline)
```

##### Instances
``` purescript
Newtype Pipelines _
```

#### `PixelsOrPercent`

``` purescript
newtype PixelsOrPercent
  = PixelsOrPercent String
```

##### Instances
``` purescript
Newtype PixelsOrPercent _
```

#### `PlayReadyDrm`

``` purescript
newtype PlayReadyDrm
  = PlayReadyDrm { "Format" :: NullOrUndefined (PlayReadyDrmFormatString), "Key" :: NullOrUndefined (NonEmptyBase64EncodedString), "KeyMd5" :: NullOrUndefined (NonEmptyBase64EncodedString), "KeyId" :: NullOrUndefined (KeyIdGuid), "InitializationVector" :: NullOrUndefined (ZeroTo255String), "LicenseAcquisitionUrl" :: NullOrUndefined (OneTo512String) }
```

<p>The PlayReady DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.</p> <p>PlayReady DRM encrypts your media files using <code>AES-CTR</code> encryption.</p> <p>If you use DRM for an <code>HLSv3</code> playlist, your outputs must have a master playlist.</p>

##### Instances
``` purescript
Newtype PlayReadyDrm _
```

#### `PlayReadyDrmFormatString`

``` purescript
newtype PlayReadyDrmFormatString
  = PlayReadyDrmFormatString String
```

##### Instances
``` purescript
Newtype PlayReadyDrmFormatString _
```

#### `Playlist`

``` purescript
newtype Playlist
  = Playlist { "Name" :: NullOrUndefined (Filename), "Format" :: NullOrUndefined (PlaylistFormat), "OutputKeys" :: NullOrUndefined (OutputKeys), "HlsContentProtection" :: NullOrUndefined (HlsContentProtection), "PlayReadyDrm" :: NullOrUndefined (PlayReadyDrm), "Status" :: NullOrUndefined (JobStatus), "StatusDetail" :: NullOrUndefined (Description) }
```

<p> Use Only for Fragmented MP4 or MPEG-TS Outputs. If you specify a preset for which the value of Container is <code>fmp4</code> (Fragmented MP4) or <code>ts</code> (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create. We recommend that you create only one master playlist per output format. The maximum number of master playlists in a job is 30. </p>

##### Instances
``` purescript
Newtype Playlist _
```

#### `PlaylistFormat`

``` purescript
newtype PlaylistFormat
  = PlaylistFormat String
```

##### Instances
``` purescript
Newtype PlaylistFormat _
```

#### `Playlists`

``` purescript
newtype Playlists
  = Playlists (Array Playlist)
```

##### Instances
``` purescript
Newtype Playlists _
```

#### `Preset`

``` purescript
newtype Preset
  = Preset { "Id" :: NullOrUndefined (Id), "Arn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (Name), "Description" :: NullOrUndefined (Description), "Container" :: NullOrUndefined (PresetContainer), "Audio" :: NullOrUndefined (AudioParameters), "Video" :: NullOrUndefined (VideoParameters), "Thumbnails" :: NullOrUndefined (Thumbnails), "Type" :: NullOrUndefined (PresetType) }
```

<p>Presets are templates that contain most of the settings for transcoding media files from one format to another. Elastic Transcoder includes some default presets for common formats, for example, several iPod and iPhone versions. You can also create your own presets for formats that aren't included among the default presets. You specify which preset you want to use when you create a job.</p>

##### Instances
``` purescript
Newtype Preset _
```

#### `PresetContainer`

``` purescript
newtype PresetContainer
  = PresetContainer String
```

##### Instances
``` purescript
Newtype PresetContainer _
```

#### `PresetType`

``` purescript
newtype PresetType
  = PresetType String
```

##### Instances
``` purescript
Newtype PresetType _
```

#### `PresetWatermark`

``` purescript
newtype PresetWatermark
  = PresetWatermark { "Id" :: NullOrUndefined (PresetWatermarkId), "MaxWidth" :: NullOrUndefined (PixelsOrPercent), "MaxHeight" :: NullOrUndefined (PixelsOrPercent), "SizingPolicy" :: NullOrUndefined (WatermarkSizingPolicy), "HorizontalAlign" :: NullOrUndefined (HorizontalAlign), "HorizontalOffset" :: NullOrUndefined (PixelsOrPercent), "VerticalAlign" :: NullOrUndefined (VerticalAlign), "VerticalOffset" :: NullOrUndefined (PixelsOrPercent), "Opacity" :: NullOrUndefined (Opacity), "Target" :: NullOrUndefined (Target) }
```

<p>Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video.</p> <p>Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.</p> <p>When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.</p>

##### Instances
``` purescript
Newtype PresetWatermark _
```

#### `PresetWatermarkId`

``` purescript
newtype PresetWatermarkId
  = PresetWatermarkId String
```

##### Instances
``` purescript
Newtype PresetWatermarkId _
```

#### `PresetWatermarks`

``` purescript
newtype PresetWatermarks
  = PresetWatermarks (Array PresetWatermark)
```

##### Instances
``` purescript
Newtype PresetWatermarks _
```

#### `Presets`

``` purescript
newtype Presets
  = Presets (Array Preset)
```

##### Instances
``` purescript
Newtype Presets _
```

#### `ReadJobRequest`

``` purescript
newtype ReadJobRequest
  = ReadJobRequest { "Id" :: Id }
```

<p>The <code>ReadJobRequest</code> structure.</p>

##### Instances
``` purescript
Newtype ReadJobRequest _
```

#### `ReadJobResponse`

``` purescript
newtype ReadJobResponse
  = ReadJobResponse { "Job" :: NullOrUndefined (Job) }
```

<p>The <code>ReadJobResponse</code> structure.</p>

##### Instances
``` purescript
Newtype ReadJobResponse _
```

#### `ReadPipelineRequest`

``` purescript
newtype ReadPipelineRequest
  = ReadPipelineRequest { "Id" :: Id }
```

<p>The <code>ReadPipelineRequest</code> structure.</p>

##### Instances
``` purescript
Newtype ReadPipelineRequest _
```

#### `ReadPipelineResponse`

``` purescript
newtype ReadPipelineResponse
  = ReadPipelineResponse { "Pipeline" :: NullOrUndefined (Pipeline), "Warnings" :: NullOrUndefined (Warnings) }
```

<p>The <code>ReadPipelineResponse</code> structure.</p>

##### Instances
``` purescript
Newtype ReadPipelineResponse _
```

#### `ReadPresetRequest`

``` purescript
newtype ReadPresetRequest
  = ReadPresetRequest { "Id" :: Id }
```

<p>The <code>ReadPresetRequest</code> structure.</p>

##### Instances
``` purescript
Newtype ReadPresetRequest _
```

#### `ReadPresetResponse`

``` purescript
newtype ReadPresetResponse
  = ReadPresetResponse { "Preset" :: NullOrUndefined (Preset) }
```

<p>The <code>ReadPresetResponse</code> structure.</p>

##### Instances
``` purescript
Newtype ReadPresetResponse _
```

#### `Resolution`

``` purescript
newtype Resolution
  = Resolution String
```

##### Instances
``` purescript
Newtype Resolution _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException {  }
```

<p>The resource you are attempting to change is in use. For example, you are attempting to delete a pipeline that is currently in use.</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>The requested resource does not exist or is not available. For example, the pipeline to which you're trying to add a job doesn't exist or is still being created.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `Role`

``` purescript
newtype Role
  = Role String
```

##### Instances
``` purescript
Newtype Role _
```

#### `Rotate`

``` purescript
newtype Rotate
  = Rotate String
```

##### Instances
``` purescript
Newtype Rotate _
```

#### `SizingPolicy`

``` purescript
newtype SizingPolicy
  = SizingPolicy String
```

##### Instances
``` purescript
Newtype SizingPolicy _
```

#### `SnsTopic`

``` purescript
newtype SnsTopic
  = SnsTopic String
```

##### Instances
``` purescript
Newtype SnsTopic _
```

#### `SnsTopics`

``` purescript
newtype SnsTopics
  = SnsTopics (Array SnsTopic)
```

##### Instances
``` purescript
Newtype SnsTopics _
```

#### `StorageClass`

``` purescript
newtype StorageClass
  = StorageClass String
```

##### Instances
``` purescript
Newtype StorageClass _
```

#### `Success`

``` purescript
newtype Success
  = Success String
```

##### Instances
``` purescript
Newtype Success _
```

#### `Target`

``` purescript
newtype Target
  = Target String
```

##### Instances
``` purescript
Newtype Target _
```

#### `TestRoleRequest`

``` purescript
newtype TestRoleRequest
  = TestRoleRequest { "Role" :: Role, "InputBucket" :: BucketName, "OutputBucket" :: BucketName, "Topics" :: SnsTopics }
```

<p> The <code>TestRoleRequest</code> structure. </p>

##### Instances
``` purescript
Newtype TestRoleRequest _
```

#### `TestRoleResponse`

``` purescript
newtype TestRoleResponse
  = TestRoleResponse { "Success" :: NullOrUndefined (Success), "Messages" :: NullOrUndefined (ExceptionMessages) }
```

<p>The <code>TestRoleResponse</code> structure.</p>

##### Instances
``` purescript
Newtype TestRoleResponse _
```

#### `ThumbnailPattern`

``` purescript
newtype ThumbnailPattern
  = ThumbnailPattern String
```

##### Instances
``` purescript
Newtype ThumbnailPattern _
```

#### `ThumbnailResolution`

``` purescript
newtype ThumbnailResolution
  = ThumbnailResolution String
```

##### Instances
``` purescript
Newtype ThumbnailResolution _
```

#### `Thumbnails`

``` purescript
newtype Thumbnails
  = Thumbnails { "Format" :: NullOrUndefined (JpgOrPng), "Interval" :: NullOrUndefined (Digits), "Resolution" :: NullOrUndefined (ThumbnailResolution), "AspectRatio" :: NullOrUndefined (AspectRatio), "MaxWidth" :: NullOrUndefined (DigitsOrAuto), "MaxHeight" :: NullOrUndefined (DigitsOrAuto), "SizingPolicy" :: NullOrUndefined (SizingPolicy), "PaddingPolicy" :: NullOrUndefined (PaddingPolicy) }
```

<p>Thumbnails for videos.</p>

##### Instances
``` purescript
Newtype Thumbnails _
```

#### `Time`

``` purescript
newtype Time
  = Time String
```

##### Instances
``` purescript
Newtype Time _
```

#### `TimeOffset`

``` purescript
newtype TimeOffset
  = TimeOffset String
```

##### Instances
``` purescript
Newtype TimeOffset _
```

#### `TimeSpan`

``` purescript
newtype TimeSpan
  = TimeSpan { "StartTime" :: NullOrUndefined (Time), "Duration" :: NullOrUndefined (Time) }
```

<p>Settings that determine when a clip begins and how long it lasts.</p>

##### Instances
``` purescript
Newtype TimeSpan _
```

#### `Timing`

``` purescript
newtype Timing
  = Timing { "SubmitTimeMillis" :: NullOrUndefined (NullableLong), "StartTimeMillis" :: NullOrUndefined (NullableLong), "FinishTimeMillis" :: NullOrUndefined (NullableLong) }
```

<p>Details about the timing of a job.</p>

##### Instances
``` purescript
Newtype Timing _
```

#### `UpdatePipelineNotificationsRequest`

``` purescript
newtype UpdatePipelineNotificationsRequest
  = UpdatePipelineNotificationsRequest { "Id" :: Id, "Notifications" :: Notifications }
```

<p>The <code>UpdatePipelineNotificationsRequest</code> structure.</p>

##### Instances
``` purescript
Newtype UpdatePipelineNotificationsRequest _
```

#### `UpdatePipelineNotificationsResponse`

``` purescript
newtype UpdatePipelineNotificationsResponse
  = UpdatePipelineNotificationsResponse { "Pipeline" :: NullOrUndefined (Pipeline) }
```

<p>The <code>UpdatePipelineNotificationsResponse</code> structure.</p>

##### Instances
``` purescript
Newtype UpdatePipelineNotificationsResponse _
```

#### `UpdatePipelineRequest`

``` purescript
newtype UpdatePipelineRequest
  = UpdatePipelineRequest { "Id" :: Id, "Name" :: NullOrUndefined (Name), "InputBucket" :: NullOrUndefined (BucketName), "Role" :: NullOrUndefined (Role), "AwsKmsKeyArn" :: NullOrUndefined (KeyArn), "Notifications" :: NullOrUndefined (Notifications), "ContentConfig" :: NullOrUndefined (PipelineOutputConfig), "ThumbnailConfig" :: NullOrUndefined (PipelineOutputConfig) }
```

<p>The <code>UpdatePipelineRequest</code> structure.</p>

##### Instances
``` purescript
Newtype UpdatePipelineRequest _
```

#### `UpdatePipelineResponse`

``` purescript
newtype UpdatePipelineResponse
  = UpdatePipelineResponse { "Pipeline" :: NullOrUndefined (Pipeline), "Warnings" :: NullOrUndefined (Warnings) }
```

<p>When you update a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>

##### Instances
``` purescript
Newtype UpdatePipelineResponse _
```

#### `UpdatePipelineStatusRequest`

``` purescript
newtype UpdatePipelineStatusRequest
  = UpdatePipelineStatusRequest { "Id" :: Id, "Status" :: PipelineStatus }
```

<p>The <code>UpdatePipelineStatusRequest</code> structure.</p>

##### Instances
``` purescript
Newtype UpdatePipelineStatusRequest _
```

#### `UpdatePipelineStatusResponse`

``` purescript
newtype UpdatePipelineStatusResponse
  = UpdatePipelineStatusResponse { "Pipeline" :: NullOrUndefined (Pipeline) }
```

<p>When you update status for a pipeline, Elastic Transcoder returns the values that you specified in the request.</p>

##### Instances
``` purescript
Newtype UpdatePipelineStatusResponse _
```

#### `UserMetadata`

``` purescript
newtype UserMetadata
  = UserMetadata (Map String String)
```

##### Instances
``` purescript
Newtype UserMetadata _
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException {  }
```

<p>One or more required parameter values were not provided in the request.</p>

##### Instances
``` purescript
Newtype ValidationException _
```

#### `VerticalAlign`

``` purescript
newtype VerticalAlign
  = VerticalAlign String
```

##### Instances
``` purescript
Newtype VerticalAlign _
```

#### `VideoBitRate`

``` purescript
newtype VideoBitRate
  = VideoBitRate String
```

##### Instances
``` purescript
Newtype VideoBitRate _
```

#### `VideoCodec`

``` purescript
newtype VideoCodec
  = VideoCodec String
```

##### Instances
``` purescript
Newtype VideoCodec _
```

#### `VideoParameters`

``` purescript
newtype VideoParameters
  = VideoParameters { "Codec" :: NullOrUndefined (VideoCodec), "CodecOptions" :: NullOrUndefined (CodecOptions), "KeyframesMaxDist" :: NullOrUndefined (KeyframesMaxDist), "FixedGOP" :: NullOrUndefined (FixedGOP), "BitRate" :: NullOrUndefined (VideoBitRate), "FrameRate" :: NullOrUndefined (FrameRate), "MaxFrameRate" :: NullOrUndefined (MaxFrameRate), "Resolution" :: NullOrUndefined (Resolution), "AspectRatio" :: NullOrUndefined (AspectRatio), "MaxWidth" :: NullOrUndefined (DigitsOrAuto), "MaxHeight" :: NullOrUndefined (DigitsOrAuto), "DisplayAspectRatio" :: NullOrUndefined (AspectRatio), "SizingPolicy" :: NullOrUndefined (SizingPolicy), "PaddingPolicy" :: NullOrUndefined (PaddingPolicy), "Watermarks" :: NullOrUndefined (PresetWatermarks) }
```

<p>The <code>VideoParameters</code> structure.</p>

##### Instances
``` purescript
Newtype VideoParameters _
```

#### `Warning`

``` purescript
newtype Warning
  = Warning { "Code" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.</p> <p>Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.</p>

##### Instances
``` purescript
Newtype Warning _
```

#### `Warnings`

``` purescript
newtype Warnings
  = Warnings (Array Warning)
```

##### Instances
``` purescript
Newtype Warnings _
```

#### `WatermarkKey`

``` purescript
newtype WatermarkKey
  = WatermarkKey String
```

##### Instances
``` purescript
Newtype WatermarkKey _
```

#### `WatermarkSizingPolicy`

``` purescript
newtype WatermarkSizingPolicy
  = WatermarkSizingPolicy String
```

##### Instances
``` purescript
Newtype WatermarkSizingPolicy _
```

#### `ZeroTo255String`

``` purescript
newtype ZeroTo255String
  = ZeroTo255String String
```

##### Instances
``` purescript
Newtype ZeroTo255String _
```

#### `ZeroTo512String`

``` purescript
newtype ZeroTo512String
  = ZeroTo512String String
```

##### Instances
``` purescript
Newtype ZeroTo512String _
```


