## Module AWS.Pinpoint

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createApp`

``` purescript
createApp :: forall eff. CreateAppRequest -> Aff (err :: RequestError | eff) CreateAppResponse
```

Creates or updates an app.

#### `createCampaign`

``` purescript
createCampaign :: forall eff. CreateCampaignRequest -> Aff (err :: RequestError | eff) CreateCampaignResponse
```

Creates or updates a campaign.

#### `createImportJob`

``` purescript
createImportJob :: forall eff. CreateImportJobRequest -> Aff (err :: RequestError | eff) CreateImportJobResponse
```

Creates or updates an import job.

#### `createSegment`

``` purescript
createSegment :: forall eff. CreateSegmentRequest -> Aff (err :: RequestError | eff) CreateSegmentResponse
```

Used to create or update a segment.

#### `deleteAdmChannel`

``` purescript
deleteAdmChannel :: forall eff. DeleteAdmChannelRequest -> Aff (err :: RequestError | eff) DeleteAdmChannelResponse
```

Delete an ADM channel

#### `deleteApnsChannel`

``` purescript
deleteApnsChannel :: forall eff. DeleteApnsChannelRequest -> Aff (err :: RequestError | eff) DeleteApnsChannelResponse
```

Deletes the APNs channel for an app.

#### `deleteApnsSandboxChannel`

``` purescript
deleteApnsSandboxChannel :: forall eff. DeleteApnsSandboxChannelRequest -> Aff (err :: RequestError | eff) DeleteApnsSandboxChannelResponse
```

Delete an APNS sandbox channel

#### `deleteApnsVoipChannel`

``` purescript
deleteApnsVoipChannel :: forall eff. DeleteApnsVoipChannelRequest -> Aff (err :: RequestError | eff) DeleteApnsVoipChannelResponse
```

Delete an APNS VoIP channel

#### `deleteApnsVoipSandboxChannel`

``` purescript
deleteApnsVoipSandboxChannel :: forall eff. DeleteApnsVoipSandboxChannelRequest -> Aff (err :: RequestError | eff) DeleteApnsVoipSandboxChannelResponse
```

Delete an APNS VoIP sandbox channel

#### `deleteApp`

``` purescript
deleteApp :: forall eff. DeleteAppRequest -> Aff (err :: RequestError | eff) DeleteAppResponse
```

Deletes an app.

#### `deleteBaiduChannel`

``` purescript
deleteBaiduChannel :: forall eff. DeleteBaiduChannelRequest -> Aff (err :: RequestError | eff) DeleteBaiduChannelResponse
```

Delete a BAIDU GCM channel

#### `deleteCampaign`

``` purescript
deleteCampaign :: forall eff. DeleteCampaignRequest -> Aff (err :: RequestError | eff) DeleteCampaignResponse
```

Deletes a campaign.

#### `deleteEmailChannel`

``` purescript
deleteEmailChannel :: forall eff. DeleteEmailChannelRequest -> Aff (err :: RequestError | eff) DeleteEmailChannelResponse
```

Delete an email channel

#### `deleteEventStream`

``` purescript
deleteEventStream :: forall eff. DeleteEventStreamRequest -> Aff (err :: RequestError | eff) DeleteEventStreamResponse
```

Deletes the event stream for an app.

#### `deleteGcmChannel`

``` purescript
deleteGcmChannel :: forall eff. DeleteGcmChannelRequest -> Aff (err :: RequestError | eff) DeleteGcmChannelResponse
```

Deletes the GCM channel for an app.

#### `deleteSegment`

``` purescript
deleteSegment :: forall eff. DeleteSegmentRequest -> Aff (err :: RequestError | eff) DeleteSegmentResponse
```

Deletes a segment.

#### `deleteSmsChannel`

``` purescript
deleteSmsChannel :: forall eff. DeleteSmsChannelRequest -> Aff (err :: RequestError | eff) DeleteSmsChannelResponse
```

Delete an SMS channel

#### `getAdmChannel`

``` purescript
getAdmChannel :: forall eff. GetAdmChannelRequest -> Aff (err :: RequestError | eff) GetAdmChannelResponse
```

Get an ADM channel

#### `getApnsChannel`

``` purescript
getApnsChannel :: forall eff. GetApnsChannelRequest -> Aff (err :: RequestError | eff) GetApnsChannelResponse
```

Returns information about the APNs channel for an app.

#### `getApnsSandboxChannel`

``` purescript
getApnsSandboxChannel :: forall eff. GetApnsSandboxChannelRequest -> Aff (err :: RequestError | eff) GetApnsSandboxChannelResponse
```

Get an APNS sandbox channel

#### `getApnsVoipChannel`

``` purescript
getApnsVoipChannel :: forall eff. GetApnsVoipChannelRequest -> Aff (err :: RequestError | eff) GetApnsVoipChannelResponse
```

Get an APNS VoIP channel

#### `getApnsVoipSandboxChannel`

``` purescript
getApnsVoipSandboxChannel :: forall eff. GetApnsVoipSandboxChannelRequest -> Aff (err :: RequestError | eff) GetApnsVoipSandboxChannelResponse
```

Get an APNS VoipSandbox channel

#### `getApp`

``` purescript
getApp :: forall eff. GetAppRequest -> Aff (err :: RequestError | eff) GetAppResponse
```

Returns information about an app.

#### `getApplicationSettings`

``` purescript
getApplicationSettings :: forall eff. GetApplicationSettingsRequest -> Aff (err :: RequestError | eff) GetApplicationSettingsResponse
```

Used to request the settings for an app.

#### `getApps`

``` purescript
getApps :: forall eff. GetAppsRequest -> Aff (err :: RequestError | eff) GetAppsResponse
```

Returns information about your apps.

#### `getBaiduChannel`

``` purescript
getBaiduChannel :: forall eff. GetBaiduChannelRequest -> Aff (err :: RequestError | eff) GetBaiduChannelResponse
```

Get a BAIDU GCM channel

#### `getCampaign`

``` purescript
getCampaign :: forall eff. GetCampaignRequest -> Aff (err :: RequestError | eff) GetCampaignResponse
```

Returns information about a campaign.

#### `getCampaignActivities`

``` purescript
getCampaignActivities :: forall eff. GetCampaignActivitiesRequest -> Aff (err :: RequestError | eff) GetCampaignActivitiesResponse
```

Returns information about the activity performed by a campaign.

#### `getCampaignVersion`

``` purescript
getCampaignVersion :: forall eff. GetCampaignVersionRequest -> Aff (err :: RequestError | eff) GetCampaignVersionResponse
```

Returns information about a specific version of a campaign.

#### `getCampaignVersions`

``` purescript
getCampaignVersions :: forall eff. GetCampaignVersionsRequest -> Aff (err :: RequestError | eff) GetCampaignVersionsResponse
```

Returns information about your campaign versions.

#### `getCampaigns`

``` purescript
getCampaigns :: forall eff. GetCampaignsRequest -> Aff (err :: RequestError | eff) GetCampaignsResponse
```

Returns information about your campaigns.

#### `getEmailChannel`

``` purescript
getEmailChannel :: forall eff. GetEmailChannelRequest -> Aff (err :: RequestError | eff) GetEmailChannelResponse
```

Get an email channel

#### `getEndpoint`

``` purescript
getEndpoint :: forall eff. GetEndpointRequest -> Aff (err :: RequestError | eff) GetEndpointResponse
```

Returns information about an endpoint.

#### `getEventStream`

``` purescript
getEventStream :: forall eff. GetEventStreamRequest -> Aff (err :: RequestError | eff) GetEventStreamResponse
```

Returns the event stream for an app.

#### `getGcmChannel`

``` purescript
getGcmChannel :: forall eff. GetGcmChannelRequest -> Aff (err :: RequestError | eff) GetGcmChannelResponse
```

Returns information about the GCM channel for an app.

#### `getImportJob`

``` purescript
getImportJob :: forall eff. GetImportJobRequest -> Aff (err :: RequestError | eff) GetImportJobResponse
```

Returns information about an import job.

#### `getImportJobs`

``` purescript
getImportJobs :: forall eff. GetImportJobsRequest -> Aff (err :: RequestError | eff) GetImportJobsResponse
```

Returns information about your import jobs.

#### `getSegment`

``` purescript
getSegment :: forall eff. GetSegmentRequest -> Aff (err :: RequestError | eff) GetSegmentResponse
```

Returns information about a segment.

#### `getSegmentImportJobs`

``` purescript
getSegmentImportJobs :: forall eff. GetSegmentImportJobsRequest -> Aff (err :: RequestError | eff) GetSegmentImportJobsResponse
```

Returns a list of import jobs for a specific segment.

#### `getSegmentVersion`

``` purescript
getSegmentVersion :: forall eff. GetSegmentVersionRequest -> Aff (err :: RequestError | eff) GetSegmentVersionResponse
```

Returns information about a segment version.

#### `getSegmentVersions`

``` purescript
getSegmentVersions :: forall eff. GetSegmentVersionsRequest -> Aff (err :: RequestError | eff) GetSegmentVersionsResponse
```

Returns information about your segment versions.

#### `getSegments`

``` purescript
getSegments :: forall eff. GetSegmentsRequest -> Aff (err :: RequestError | eff) GetSegmentsResponse
```

Used to get information about your segments.

#### `getSmsChannel`

``` purescript
getSmsChannel :: forall eff. GetSmsChannelRequest -> Aff (err :: RequestError | eff) GetSmsChannelResponse
```

Get an SMS channel

#### `putEventStream`

``` purescript
putEventStream :: forall eff. PutEventStreamRequest -> Aff (err :: RequestError | eff) PutEventStreamResponse
```

Use to create or update the event stream for an app.

#### `sendMessages`

``` purescript
sendMessages :: forall eff. SendMessagesRequest -> Aff (err :: RequestError | eff) SendMessagesResponse
```

Send a batch of messages

#### `sendUsersMessages`

``` purescript
sendUsersMessages :: forall eff. SendUsersMessagesRequest -> Aff (err :: RequestError | eff) SendUsersMessagesResponse
```

Send a batch of messages to users

#### `updateAdmChannel`

``` purescript
updateAdmChannel :: forall eff. UpdateAdmChannelRequest -> Aff (err :: RequestError | eff) UpdateAdmChannelResponse
```

Update an ADM channel

#### `updateApnsChannel`

``` purescript
updateApnsChannel :: forall eff. UpdateApnsChannelRequest -> Aff (err :: RequestError | eff) UpdateApnsChannelResponse
```

Use to update the APNs channel for an app.

#### `updateApnsSandboxChannel`

``` purescript
updateApnsSandboxChannel :: forall eff. UpdateApnsSandboxChannelRequest -> Aff (err :: RequestError | eff) UpdateApnsSandboxChannelResponse
```

Update an APNS sandbox channel

#### `updateApnsVoipChannel`

``` purescript
updateApnsVoipChannel :: forall eff. UpdateApnsVoipChannelRequest -> Aff (err :: RequestError | eff) UpdateApnsVoipChannelResponse
```

Update an APNS VoIP channel

#### `updateApnsVoipSandboxChannel`

``` purescript
updateApnsVoipSandboxChannel :: forall eff. UpdateApnsVoipSandboxChannelRequest -> Aff (err :: RequestError | eff) UpdateApnsVoipSandboxChannelResponse
```

Update an APNS VoIP sandbox channel

#### `updateApplicationSettings`

``` purescript
updateApplicationSettings :: forall eff. UpdateApplicationSettingsRequest -> Aff (err :: RequestError | eff) UpdateApplicationSettingsResponse
```

Used to update the settings for an app.

#### `updateBaiduChannel`

``` purescript
updateBaiduChannel :: forall eff. UpdateBaiduChannelRequest -> Aff (err :: RequestError | eff) UpdateBaiduChannelResponse
```

Update a BAIDU GCM channel

#### `updateCampaign`

``` purescript
updateCampaign :: forall eff. UpdateCampaignRequest -> Aff (err :: RequestError | eff) UpdateCampaignResponse
```

Use to update a campaign.

#### `updateEmailChannel`

``` purescript
updateEmailChannel :: forall eff. UpdateEmailChannelRequest -> Aff (err :: RequestError | eff) UpdateEmailChannelResponse
```

Update an email channel

#### `updateEndpoint`

``` purescript
updateEndpoint :: forall eff. UpdateEndpointRequest -> Aff (err :: RequestError | eff) UpdateEndpointResponse
```

Use to update an endpoint.

#### `updateEndpointsBatch`

``` purescript
updateEndpointsBatch :: forall eff. UpdateEndpointsBatchRequest -> Aff (err :: RequestError | eff) UpdateEndpointsBatchResponse
```

Use to update a batch of endpoints.

#### `updateGcmChannel`

``` purescript
updateGcmChannel :: forall eff. UpdateGcmChannelRequest -> Aff (err :: RequestError | eff) UpdateGcmChannelResponse
```

Use to update the GCM channel for an app.

#### `updateSegment`

``` purescript
updateSegment :: forall eff. UpdateSegmentRequest -> Aff (err :: RequestError | eff) UpdateSegmentResponse
```

Use to update a segment.

#### `updateSmsChannel`

``` purescript
updateSmsChannel :: forall eff. UpdateSmsChannelRequest -> Aff (err :: RequestError | eff) UpdateSmsChannelResponse
```

Update an SMS channel

#### `ADMChannelRequest`

``` purescript
newtype ADMChannelRequest
  = ADMChannelRequest { "ClientId" :: NullOrUndefined (String), "ClientSecret" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean) }
```

Amazon Device Messaging channel definition.

#### `ADMChannelResponse`

``` purescript
newtype ADMChannelResponse
  = ADMChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Amazon Device Messaging channel definition.

#### `ADMMessage`

``` purescript
newtype ADMMessage
  = ADMMessage { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "ConsolidationKey" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "ExpiresAfter" :: NullOrUndefined (String), "IconReference" :: NullOrUndefined (String), "ImageIconUrl" :: NullOrUndefined (String), "ImageUrl" :: NullOrUndefined (String), "MD5" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "SmallImageIconUrl" :: NullOrUndefined (String), "Sound" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

ADM Message.

#### `APNSChannelRequest`

``` purescript
newtype APNSChannelRequest
  = APNSChannelRequest { "BundleId" :: NullOrUndefined (String), "Certificate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "PrivateKey" :: NullOrUndefined (String), "TeamId" :: NullOrUndefined (String), "TokenKey" :: NullOrUndefined (String), "TokenKeyId" :: NullOrUndefined (String) }
```

Apple Push Notification Service channel definition.

#### `APNSChannelResponse`

``` purescript
newtype APNSChannelResponse
  = APNSChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "HasTokenKey" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Apple Distribution Push Notification Service channel definition.

#### `APNSMessage`

``` purescript
newtype APNSMessage
  = APNSMessage { "Action" :: NullOrUndefined (Action), "Badge" :: NullOrUndefined (Int), "Body" :: NullOrUndefined (String), "Category" :: NullOrUndefined (String), "CollapseId" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "MediaUrl" :: NullOrUndefined (String), "PreferredAuthenticationMethod" :: NullOrUndefined (String), "Priority" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "Sound" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "ThreadId" :: NullOrUndefined (String), "TimeToLive" :: NullOrUndefined (Int), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

APNS Message.

#### `APNSSandboxChannelRequest`

``` purescript
newtype APNSSandboxChannelRequest
  = APNSSandboxChannelRequest { "BundleId" :: NullOrUndefined (String), "Certificate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "PrivateKey" :: NullOrUndefined (String), "TeamId" :: NullOrUndefined (String), "TokenKey" :: NullOrUndefined (String), "TokenKeyId" :: NullOrUndefined (String) }
```

Apple Development Push Notification Service channel definition.

#### `APNSSandboxChannelResponse`

``` purescript
newtype APNSSandboxChannelResponse
  = APNSSandboxChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "HasTokenKey" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Apple Development Push Notification Service channel definition.

#### `APNSVoipChannelRequest`

``` purescript
newtype APNSVoipChannelRequest
  = APNSVoipChannelRequest { "BundleId" :: NullOrUndefined (String), "Certificate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "PrivateKey" :: NullOrUndefined (String), "TeamId" :: NullOrUndefined (String), "TokenKey" :: NullOrUndefined (String), "TokenKeyId" :: NullOrUndefined (String) }
```

Apple VoIP Push Notification Service channel definition.

#### `APNSVoipChannelResponse`

``` purescript
newtype APNSVoipChannelResponse
  = APNSVoipChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "HasTokenKey" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Apple VoIP Push Notification Service channel definition.

#### `APNSVoipSandboxChannelRequest`

``` purescript
newtype APNSVoipSandboxChannelRequest
  = APNSVoipSandboxChannelRequest { "BundleId" :: NullOrUndefined (String), "Certificate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "PrivateKey" :: NullOrUndefined (String), "TeamId" :: NullOrUndefined (String), "TokenKey" :: NullOrUndefined (String), "TokenKeyId" :: NullOrUndefined (String) }
```

Apple VoIP Developer Push Notification Service channel definition.

#### `APNSVoipSandboxChannelResponse`

``` purescript
newtype APNSVoipSandboxChannelResponse
  = APNSVoipSandboxChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "HasTokenKey" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Apple VoIP Developer Push Notification Service channel definition.

#### `Action`

``` purescript
newtype Action
  = Action String
```

#### `ActivitiesResponse`

``` purescript
newtype ActivitiesResponse
  = ActivitiesResponse { "Item" :: NullOrUndefined (ListOfActivityResponse) }
```

Activities for campaign.

#### `ActivityResponse`

``` purescript
newtype ActivityResponse
  = ActivityResponse { "ApplicationId" :: NullOrUndefined (String), "CampaignId" :: NullOrUndefined (String), "End" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Result" :: NullOrUndefined (String), "ScheduledStart" :: NullOrUndefined (String), "Start" :: NullOrUndefined (String), "State" :: NullOrUndefined (String), "SuccessfulEndpointCount" :: NullOrUndefined (Int), "TimezonesCompletedCount" :: NullOrUndefined (Int), "TimezonesTotalCount" :: NullOrUndefined (Int), "TotalEndpointCount" :: NullOrUndefined (Int), "TreatmentId" :: NullOrUndefined (String) }
```

Activity definition

#### `AddressConfiguration`

``` purescript
newtype AddressConfiguration
  = AddressConfiguration { "BodyOverride" :: NullOrUndefined (String), "ChannelType" :: NullOrUndefined (ChannelType), "Context" :: NullOrUndefined (MapOf__string), "RawContent" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "TitleOverride" :: NullOrUndefined (String) }
```

Address configuration.

#### `ApplicationResponse`

``` purescript
newtype ApplicationResponse
  = ApplicationResponse { "Id" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Application Response.

#### `ApplicationSettingsResource`

``` purescript
newtype ApplicationSettingsResource
  = ApplicationSettingsResource { "ApplicationId" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Limits" :: NullOrUndefined (CampaignLimits), "QuietTime" :: NullOrUndefined (QuietTime) }
```

Application settings.

#### `ApplicationsResponse`

``` purescript
newtype ApplicationsResponse
  = ApplicationsResponse { "Item" :: NullOrUndefined (ListOfApplicationResponse), "NextToken" :: NullOrUndefined (String) }
```

Get Applications Result.

#### `AttributeDimension`

``` purescript
newtype AttributeDimension
  = AttributeDimension { "AttributeType" :: NullOrUndefined (AttributeType), "Values" :: NullOrUndefined (ListOf__string) }
```

Custom attibute dimension

#### `AttributeType`

``` purescript
newtype AttributeType
  = AttributeType String
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

#### `BaiduChannelRequest`

``` purescript
newtype BaiduChannelRequest
  = BaiduChannelRequest { "ApiKey" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "SecretKey" :: NullOrUndefined (String) }
```

Baidu Cloud Push credentials

#### `BaiduChannelResponse`

``` purescript
newtype BaiduChannelResponse
  = BaiduChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Credential" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Baidu Cloud Messaging channel definition

#### `BaiduMessage`

``` purescript
newtype BaiduMessage
  = BaiduMessage { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "IconReference" :: NullOrUndefined (String), "ImageIconUrl" :: NullOrUndefined (String), "ImageUrl" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "SmallImageIconUrl" :: NullOrUndefined (String), "Sound" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

Baidu Message.

#### `CampaignEmailMessage`

``` purescript
newtype CampaignEmailMessage
  = CampaignEmailMessage { "Body" :: NullOrUndefined (String), "FromAddress" :: NullOrUndefined (String), "HtmlBody" :: NullOrUndefined (String), "Title" :: NullOrUndefined (String) }
```

The email message configuration.

#### `CampaignLimits`

``` purescript
newtype CampaignLimits
  = CampaignLimits { "Daily" :: NullOrUndefined (Int), "MaximumDuration" :: NullOrUndefined (Int), "MessagesPerSecond" :: NullOrUndefined (Int), "Total" :: NullOrUndefined (Int) }
```

Campaign Limits are used to limit the number of messages that can be sent to a user.

#### `CampaignResponse`

``` purescript
newtype CampaignResponse
  = CampaignResponse { "AdditionalTreatments" :: NullOrUndefined (ListOfTreatmentResource), "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultState" :: NullOrUndefined (CampaignState), "Description" :: NullOrUndefined (String), "HoldoutPercent" :: NullOrUndefined (Int), "Id" :: NullOrUndefined (String), "IsPaused" :: NullOrUndefined (Boolean), "LastModifiedDate" :: NullOrUndefined (String), "Limits" :: NullOrUndefined (CampaignLimits), "MessageConfiguration" :: NullOrUndefined (MessageConfiguration), "Name" :: NullOrUndefined (String), "Schedule" :: NullOrUndefined (Schedule), "SegmentId" :: NullOrUndefined (String), "SegmentVersion" :: NullOrUndefined (Int), "State" :: NullOrUndefined (CampaignState), "TreatmentDescription" :: NullOrUndefined (String), "TreatmentName" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Campaign definition

#### `CampaignSmsMessage`

``` purescript
newtype CampaignSmsMessage
  = CampaignSmsMessage { "Body" :: NullOrUndefined (String), "MessageType" :: NullOrUndefined (MessageType), "SenderId" :: NullOrUndefined (String) }
```

SMS message configuration.

#### `CampaignState`

``` purescript
newtype CampaignState
  = CampaignState { "CampaignStatus" :: NullOrUndefined (CampaignStatus) }
```

State of the Campaign

#### `CampaignStatus`

``` purescript
newtype CampaignStatus
  = CampaignStatus String
```

#### `CampaignsResponse`

``` purescript
newtype CampaignsResponse
  = CampaignsResponse { "Item" :: NullOrUndefined (ListOfCampaignResponse), "NextToken" :: NullOrUndefined (String) }
```

List of available campaigns.

#### `ChannelType`

``` purescript
newtype ChannelType
  = ChannelType String
```

#### `CreateAppRequest`

``` purescript
newtype CreateAppRequest
  = CreateAppRequest { "CreateApplicationRequest" :: CreateApplicationRequest }
```

#### `CreateAppResponse`

``` purescript
newtype CreateAppResponse
  = CreateAppResponse { "ApplicationResponse" :: ApplicationResponse }
```

#### `CreateApplicationRequest`

``` purescript
newtype CreateApplicationRequest
  = CreateApplicationRequest { "Name" :: NullOrUndefined (String) }
```

Application Request.

#### `CreateCampaignRequest`

``` purescript
newtype CreateCampaignRequest
  = CreateCampaignRequest { "ApplicationId" :: String, "WriteCampaignRequest" :: WriteCampaignRequest }
```

#### `CreateCampaignResponse`

``` purescript
newtype CreateCampaignResponse
  = CreateCampaignResponse { "CampaignResponse" :: CampaignResponse }
```

#### `CreateImportJobRequest`

``` purescript
newtype CreateImportJobRequest
  = CreateImportJobRequest { "ApplicationId" :: String, "ImportJobRequest" :: ImportJobRequest }
```

#### `CreateImportJobResponse`

``` purescript
newtype CreateImportJobResponse
  = CreateImportJobResponse { "ImportJobResponse" :: ImportJobResponse }
```

#### `CreateSegmentRequest`

``` purescript
newtype CreateSegmentRequest
  = CreateSegmentRequest { "ApplicationId" :: String, "WriteSegmentRequest" :: WriteSegmentRequest }
```

#### `CreateSegmentResponse`

``` purescript
newtype CreateSegmentResponse
  = CreateSegmentResponse { "SegmentResponse" :: SegmentResponse }
```

#### `DefaultMessage`

``` purescript
newtype DefaultMessage
  = DefaultMessage { "Body" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string) }
```

Default Message across push notification, email, and sms.

#### `DefaultPushNotificationMessage`

``` purescript
newtype DefaultPushNotificationMessage
  = DefaultPushNotificationMessage { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "SilentPush" :: NullOrUndefined (Boolean), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

Default Push Notification Message.

#### `DeleteAdmChannelRequest`

``` purescript
newtype DeleteAdmChannelRequest
  = DeleteAdmChannelRequest { "ApplicationId" :: String }
```

#### `DeleteAdmChannelResponse`

``` purescript
newtype DeleteAdmChannelResponse
  = DeleteAdmChannelResponse { "ADMChannelResponse" :: ADMChannelResponse }
```

#### `DeleteApnsChannelRequest`

``` purescript
newtype DeleteApnsChannelRequest
  = DeleteApnsChannelRequest { "ApplicationId" :: String }
```

#### `DeleteApnsChannelResponse`

``` purescript
newtype DeleteApnsChannelResponse
  = DeleteApnsChannelResponse { "APNSChannelResponse" :: APNSChannelResponse }
```

#### `DeleteApnsSandboxChannelRequest`

``` purescript
newtype DeleteApnsSandboxChannelRequest
  = DeleteApnsSandboxChannelRequest { "ApplicationId" :: String }
```

#### `DeleteApnsSandboxChannelResponse`

``` purescript
newtype DeleteApnsSandboxChannelResponse
  = DeleteApnsSandboxChannelResponse { "APNSSandboxChannelResponse" :: APNSSandboxChannelResponse }
```

#### `DeleteApnsVoipChannelRequest`

``` purescript
newtype DeleteApnsVoipChannelRequest
  = DeleteApnsVoipChannelRequest { "ApplicationId" :: String }
```

#### `DeleteApnsVoipChannelResponse`

``` purescript
newtype DeleteApnsVoipChannelResponse
  = DeleteApnsVoipChannelResponse { "APNSVoipChannelResponse" :: APNSVoipChannelResponse }
```

#### `DeleteApnsVoipSandboxChannelRequest`

``` purescript
newtype DeleteApnsVoipSandboxChannelRequest
  = DeleteApnsVoipSandboxChannelRequest { "ApplicationId" :: String }
```

#### `DeleteApnsVoipSandboxChannelResponse`

``` purescript
newtype DeleteApnsVoipSandboxChannelResponse
  = DeleteApnsVoipSandboxChannelResponse { "APNSVoipSandboxChannelResponse" :: APNSVoipSandboxChannelResponse }
```

#### `DeleteAppRequest`

``` purescript
newtype DeleteAppRequest
  = DeleteAppRequest { "ApplicationId" :: String }
```

#### `DeleteAppResponse`

``` purescript
newtype DeleteAppResponse
  = DeleteAppResponse { "ApplicationResponse" :: ApplicationResponse }
```

#### `DeleteBaiduChannelRequest`

``` purescript
newtype DeleteBaiduChannelRequest
  = DeleteBaiduChannelRequest { "ApplicationId" :: String }
```

#### `DeleteBaiduChannelResponse`

``` purescript
newtype DeleteBaiduChannelResponse
  = DeleteBaiduChannelResponse { "BaiduChannelResponse" :: BaiduChannelResponse }
```

#### `DeleteCampaignRequest`

``` purescript
newtype DeleteCampaignRequest
  = DeleteCampaignRequest { "ApplicationId" :: String, "CampaignId" :: String }
```

#### `DeleteCampaignResponse`

``` purescript
newtype DeleteCampaignResponse
  = DeleteCampaignResponse { "CampaignResponse" :: CampaignResponse }
```

#### `DeleteEmailChannelRequest`

``` purescript
newtype DeleteEmailChannelRequest
  = DeleteEmailChannelRequest { "ApplicationId" :: String }
```

#### `DeleteEmailChannelResponse`

``` purescript
newtype DeleteEmailChannelResponse
  = DeleteEmailChannelResponse { "EmailChannelResponse" :: EmailChannelResponse }
```

#### `DeleteEventStreamRequest`

``` purescript
newtype DeleteEventStreamRequest
  = DeleteEventStreamRequest { "ApplicationId" :: String }
```

DeleteEventStream Request

#### `DeleteEventStreamResponse`

``` purescript
newtype DeleteEventStreamResponse
  = DeleteEventStreamResponse { "EventStream" :: EventStream }
```

#### `DeleteGcmChannelRequest`

``` purescript
newtype DeleteGcmChannelRequest
  = DeleteGcmChannelRequest { "ApplicationId" :: String }
```

#### `DeleteGcmChannelResponse`

``` purescript
newtype DeleteGcmChannelResponse
  = DeleteGcmChannelResponse { "GCMChannelResponse" :: GCMChannelResponse }
```

#### `DeleteSegmentRequest`

``` purescript
newtype DeleteSegmentRequest
  = DeleteSegmentRequest { "ApplicationId" :: String, "SegmentId" :: String }
```

#### `DeleteSegmentResponse`

``` purescript
newtype DeleteSegmentResponse
  = DeleteSegmentResponse { "SegmentResponse" :: SegmentResponse }
```

#### `DeleteSmsChannelRequest`

``` purescript
newtype DeleteSmsChannelRequest
  = DeleteSmsChannelRequest { "ApplicationId" :: String }
```

#### `DeleteSmsChannelResponse`

``` purescript
newtype DeleteSmsChannelResponse
  = DeleteSmsChannelResponse { "SMSChannelResponse" :: SMSChannelResponse }
```

#### `DeliveryStatus`

``` purescript
newtype DeliveryStatus
  = DeliveryStatus String
```

#### `DimensionType`

``` purescript
newtype DimensionType
  = DimensionType String
```

#### `DirectMessageConfiguration`

``` purescript
newtype DirectMessageConfiguration
  = DirectMessageConfiguration { "ADMMessage" :: NullOrUndefined (ADMMessage), "APNSMessage" :: NullOrUndefined (APNSMessage), "BaiduMessage" :: NullOrUndefined (BaiduMessage), "DefaultMessage" :: NullOrUndefined (DefaultMessage), "DefaultPushNotificationMessage" :: NullOrUndefined (DefaultPushNotificationMessage), "GCMMessage" :: NullOrUndefined (GCMMessage), "SMSMessage" :: NullOrUndefined (SMSMessage) }
```

The message configuration.

#### `Duration`

``` purescript
newtype Duration
  = Duration String
```

#### `EmailChannelRequest`

``` purescript
newtype EmailChannelRequest
  = EmailChannelRequest { "Enabled" :: NullOrUndefined (Boolean), "FromAddress" :: NullOrUndefined (String), "Identity" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Email Channel Request

#### `EmailChannelResponse`

``` purescript
newtype EmailChannelResponse
  = EmailChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "FromAddress" :: NullOrUndefined (String), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "Identity" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Email Channel Response.

#### `EndpointBatchItem`

``` purescript
newtype EndpointBatchItem
  = EndpointBatchItem { "Address" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (MapOfListOf__string), "ChannelType" :: NullOrUndefined (ChannelType), "Demographic" :: NullOrUndefined (EndpointDemographic), "EffectiveDate" :: NullOrUndefined (String), "EndpointStatus" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Location" :: NullOrUndefined (EndpointLocation), "Metrics" :: NullOrUndefined (MapOf__double), "OptOut" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "User" :: NullOrUndefined (EndpointUser) }
```

Endpoint update request

#### `EndpointBatchRequest`

``` purescript
newtype EndpointBatchRequest
  = EndpointBatchRequest { "Item" :: NullOrUndefined (ListOfEndpointBatchItem) }
```

Endpoint batch update request.

#### `EndpointDemographic`

``` purescript
newtype EndpointDemographic
  = EndpointDemographic { "AppVersion" :: NullOrUndefined (String), "Locale" :: NullOrUndefined (String), "Make" :: NullOrUndefined (String), "Model" :: NullOrUndefined (String), "ModelVersion" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "PlatformVersion" :: NullOrUndefined (String), "Timezone" :: NullOrUndefined (String) }
```

Endpoint demographic data

#### `EndpointLocation`

``` purescript
newtype EndpointLocation
  = EndpointLocation { "City" :: NullOrUndefined (String), "Country" :: NullOrUndefined (String), "Latitude" :: NullOrUndefined (Number), "Longitude" :: NullOrUndefined (Number), "PostalCode" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String) }
```

Endpoint location data

#### `EndpointMessageResult`

``` purescript
newtype EndpointMessageResult
  = EndpointMessageResult { "Address" :: NullOrUndefined (String), "DeliveryStatus" :: NullOrUndefined (DeliveryStatus), "StatusCode" :: NullOrUndefined (Int), "StatusMessage" :: NullOrUndefined (String), "UpdatedToken" :: NullOrUndefined (String) }
```

The result from sending a message to an endpoint.

#### `EndpointRequest`

``` purescript
newtype EndpointRequest
  = EndpointRequest { "Address" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (MapOfListOf__string), "ChannelType" :: NullOrUndefined (ChannelType), "Demographic" :: NullOrUndefined (EndpointDemographic), "EffectiveDate" :: NullOrUndefined (String), "EndpointStatus" :: NullOrUndefined (String), "Location" :: NullOrUndefined (EndpointLocation), "Metrics" :: NullOrUndefined (MapOf__double), "OptOut" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "User" :: NullOrUndefined (EndpointUser) }
```

Endpoint update request

#### `EndpointResponse`

``` purescript
newtype EndpointResponse
  = EndpointResponse { "Address" :: NullOrUndefined (String), "ApplicationId" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (MapOfListOf__string), "ChannelType" :: NullOrUndefined (ChannelType), "CohortId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Demographic" :: NullOrUndefined (EndpointDemographic), "EffectiveDate" :: NullOrUndefined (String), "EndpointStatus" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Location" :: NullOrUndefined (EndpointLocation), "Metrics" :: NullOrUndefined (MapOf__double), "OptOut" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "User" :: NullOrUndefined (EndpointUser) }
```

Endpoint response

#### `EndpointSendConfiguration`

``` purescript
newtype EndpointSendConfiguration
  = EndpointSendConfiguration { "BodyOverride" :: NullOrUndefined (String), "Context" :: NullOrUndefined (MapOf__string), "RawContent" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "TitleOverride" :: NullOrUndefined (String) }
```

Endpoint send configuration.

#### `EndpointUser`

``` purescript
newtype EndpointUser
  = EndpointUser { "UserAttributes" :: NullOrUndefined (MapOfListOf__string), "UserId" :: NullOrUndefined (String) }
```

Endpoint user specific custom userAttributes

#### `EventStream`

``` purescript
newtype EventStream
  = EventStream { "ApplicationId" :: NullOrUndefined (String), "DestinationStreamArn" :: NullOrUndefined (String), "ExternalId" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "LastUpdatedBy" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Model for an event publishing subscription export.

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

#### `Format`

``` purescript
newtype Format
  = Format String
```

#### `Frequency`

``` purescript
newtype Frequency
  = Frequency String
```

#### `GCMChannelRequest`

``` purescript
newtype GCMChannelRequest
  = GCMChannelRequest { "ApiKey" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean) }
```

Google Cloud Messaging credentials

#### `GCMChannelResponse`

``` purescript
newtype GCMChannelResponse
  = GCMChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Credential" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Google Cloud Messaging channel definition

#### `GCMMessage`

``` purescript
newtype GCMMessage
  = GCMMessage { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "CollapseKey" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "IconReference" :: NullOrUndefined (String), "ImageIconUrl" :: NullOrUndefined (String), "ImageUrl" :: NullOrUndefined (String), "Priority" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "RestrictedPackageName" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "SmallImageIconUrl" :: NullOrUndefined (String), "Sound" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "TimeToLive" :: NullOrUndefined (Int), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

GCM Message.

#### `GetAdmChannelRequest`

``` purescript
newtype GetAdmChannelRequest
  = GetAdmChannelRequest { "ApplicationId" :: String }
```

#### `GetAdmChannelResponse`

``` purescript
newtype GetAdmChannelResponse
  = GetAdmChannelResponse { "ADMChannelResponse" :: ADMChannelResponse }
```

#### `GetApnsChannelRequest`

``` purescript
newtype GetApnsChannelRequest
  = GetApnsChannelRequest { "ApplicationId" :: String }
```

#### `GetApnsChannelResponse`

``` purescript
newtype GetApnsChannelResponse
  = GetApnsChannelResponse { "APNSChannelResponse" :: APNSChannelResponse }
```

#### `GetApnsSandboxChannelRequest`

``` purescript
newtype GetApnsSandboxChannelRequest
  = GetApnsSandboxChannelRequest { "ApplicationId" :: String }
```

#### `GetApnsSandboxChannelResponse`

``` purescript
newtype GetApnsSandboxChannelResponse
  = GetApnsSandboxChannelResponse { "APNSSandboxChannelResponse" :: APNSSandboxChannelResponse }
```

#### `GetApnsVoipChannelRequest`

``` purescript
newtype GetApnsVoipChannelRequest
  = GetApnsVoipChannelRequest { "ApplicationId" :: String }
```

#### `GetApnsVoipChannelResponse`

``` purescript
newtype GetApnsVoipChannelResponse
  = GetApnsVoipChannelResponse { "APNSVoipChannelResponse" :: APNSVoipChannelResponse }
```

#### `GetApnsVoipSandboxChannelRequest`

``` purescript
newtype GetApnsVoipSandboxChannelRequest
  = GetApnsVoipSandboxChannelRequest { "ApplicationId" :: String }
```

#### `GetApnsVoipSandboxChannelResponse`

``` purescript
newtype GetApnsVoipSandboxChannelResponse
  = GetApnsVoipSandboxChannelResponse { "APNSVoipSandboxChannelResponse" :: APNSVoipSandboxChannelResponse }
```

#### `GetAppRequest`

``` purescript
newtype GetAppRequest
  = GetAppRequest { "ApplicationId" :: String }
```

#### `GetAppResponse`

``` purescript
newtype GetAppResponse
  = GetAppResponse { "ApplicationResponse" :: ApplicationResponse }
```

#### `GetApplicationSettingsRequest`

``` purescript
newtype GetApplicationSettingsRequest
  = GetApplicationSettingsRequest { "ApplicationId" :: String }
```

#### `GetApplicationSettingsResponse`

``` purescript
newtype GetApplicationSettingsResponse
  = GetApplicationSettingsResponse { "ApplicationSettingsResource" :: ApplicationSettingsResource }
```

#### `GetAppsRequest`

``` purescript
newtype GetAppsRequest
  = GetAppsRequest { "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

#### `GetAppsResponse`

``` purescript
newtype GetAppsResponse
  = GetAppsResponse { "ApplicationsResponse" :: ApplicationsResponse }
```

#### `GetBaiduChannelRequest`

``` purescript
newtype GetBaiduChannelRequest
  = GetBaiduChannelRequest { "ApplicationId" :: String }
```

#### `GetBaiduChannelResponse`

``` purescript
newtype GetBaiduChannelResponse
  = GetBaiduChannelResponse { "BaiduChannelResponse" :: BaiduChannelResponse }
```

#### `GetCampaignActivitiesRequest`

``` purescript
newtype GetCampaignActivitiesRequest
  = GetCampaignActivitiesRequest { "ApplicationId" :: String, "CampaignId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

#### `GetCampaignActivitiesResponse`

``` purescript
newtype GetCampaignActivitiesResponse
  = GetCampaignActivitiesResponse { "ActivitiesResponse" :: ActivitiesResponse }
```

#### `GetCampaignRequest`

``` purescript
newtype GetCampaignRequest
  = GetCampaignRequest { "ApplicationId" :: String, "CampaignId" :: String }
```

#### `GetCampaignResponse`

``` purescript
newtype GetCampaignResponse
  = GetCampaignResponse { "CampaignResponse" :: CampaignResponse }
```

#### `GetCampaignVersionRequest`

``` purescript
newtype GetCampaignVersionRequest
  = GetCampaignVersionRequest { "ApplicationId" :: String, "CampaignId" :: String, "Version" :: String }
```

#### `GetCampaignVersionResponse`

``` purescript
newtype GetCampaignVersionResponse
  = GetCampaignVersionResponse { "CampaignResponse" :: CampaignResponse }
```

#### `GetCampaignVersionsRequest`

``` purescript
newtype GetCampaignVersionsRequest
  = GetCampaignVersionsRequest { "ApplicationId" :: String, "CampaignId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

#### `GetCampaignVersionsResponse`

``` purescript
newtype GetCampaignVersionsResponse
  = GetCampaignVersionsResponse { "CampaignsResponse" :: CampaignsResponse }
```

#### `GetCampaignsRequest`

``` purescript
newtype GetCampaignsRequest
  = GetCampaignsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

#### `GetCampaignsResponse`

``` purescript
newtype GetCampaignsResponse
  = GetCampaignsResponse { "CampaignsResponse" :: CampaignsResponse }
```

#### `GetEmailChannelRequest`

``` purescript
newtype GetEmailChannelRequest
  = GetEmailChannelRequest { "ApplicationId" :: String }
```

#### `GetEmailChannelResponse`

``` purescript
newtype GetEmailChannelResponse
  = GetEmailChannelResponse { "EmailChannelResponse" :: EmailChannelResponse }
```

#### `GetEndpointRequest`

``` purescript
newtype GetEndpointRequest
  = GetEndpointRequest { "ApplicationId" :: String, "EndpointId" :: String }
```

#### `GetEndpointResponse`

``` purescript
newtype GetEndpointResponse
  = GetEndpointResponse { "EndpointResponse" :: EndpointResponse }
```

#### `GetEventStreamRequest`

``` purescript
newtype GetEventStreamRequest
  = GetEventStreamRequest { "ApplicationId" :: String }
```

GetEventStreamRequest

#### `GetEventStreamResponse`

``` purescript
newtype GetEventStreamResponse
  = GetEventStreamResponse { "EventStream" :: EventStream }
```

#### `GetGcmChannelRequest`

``` purescript
newtype GetGcmChannelRequest
  = GetGcmChannelRequest { "ApplicationId" :: String }
```

#### `GetGcmChannelResponse`

``` purescript
newtype GetGcmChannelResponse
  = GetGcmChannelResponse { "GCMChannelResponse" :: GCMChannelResponse }
```

#### `GetImportJobRequest`

``` purescript
newtype GetImportJobRequest
  = GetImportJobRequest { "ApplicationId" :: String, "JobId" :: String }
```

#### `GetImportJobResponse`

``` purescript
newtype GetImportJobResponse
  = GetImportJobResponse { "ImportJobResponse" :: ImportJobResponse }
```

#### `GetImportJobsRequest`

``` purescript
newtype GetImportJobsRequest
  = GetImportJobsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

#### `GetImportJobsResponse`

``` purescript
newtype GetImportJobsResponse
  = GetImportJobsResponse { "ImportJobsResponse" :: ImportJobsResponse }
```

#### `GetSegmentImportJobsRequest`

``` purescript
newtype GetSegmentImportJobsRequest
  = GetSegmentImportJobsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "SegmentId" :: String, "Token" :: NullOrUndefined (String) }
```

#### `GetSegmentImportJobsResponse`

``` purescript
newtype GetSegmentImportJobsResponse
  = GetSegmentImportJobsResponse { "ImportJobsResponse" :: ImportJobsResponse }
```

#### `GetSegmentRequest`

``` purescript
newtype GetSegmentRequest
  = GetSegmentRequest { "ApplicationId" :: String, "SegmentId" :: String }
```

#### `GetSegmentResponse`

``` purescript
newtype GetSegmentResponse
  = GetSegmentResponse { "SegmentResponse" :: SegmentResponse }
```

#### `GetSegmentVersionRequest`

``` purescript
newtype GetSegmentVersionRequest
  = GetSegmentVersionRequest { "ApplicationId" :: String, "SegmentId" :: String, "Version" :: String }
```

#### `GetSegmentVersionResponse`

``` purescript
newtype GetSegmentVersionResponse
  = GetSegmentVersionResponse { "SegmentResponse" :: SegmentResponse }
```

#### `GetSegmentVersionsRequest`

``` purescript
newtype GetSegmentVersionsRequest
  = GetSegmentVersionsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "SegmentId" :: String, "Token" :: NullOrUndefined (String) }
```

#### `GetSegmentVersionsResponse`

``` purescript
newtype GetSegmentVersionsResponse
  = GetSegmentVersionsResponse { "SegmentsResponse" :: SegmentsResponse }
```

#### `GetSegmentsRequest`

``` purescript
newtype GetSegmentsRequest
  = GetSegmentsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

#### `GetSegmentsResponse`

``` purescript
newtype GetSegmentsResponse
  = GetSegmentsResponse { "SegmentsResponse" :: SegmentsResponse }
```

#### `GetSmsChannelRequest`

``` purescript
newtype GetSmsChannelRequest
  = GetSmsChannelRequest { "ApplicationId" :: String }
```

#### `GetSmsChannelResponse`

``` purescript
newtype GetSmsChannelResponse
  = GetSmsChannelResponse { "SMSChannelResponse" :: SMSChannelResponse }
```

#### `ImportJobRequest`

``` purescript
newtype ImportJobRequest
  = ImportJobRequest { "DefineSegment" :: NullOrUndefined (Boolean), "ExternalId" :: NullOrUndefined (String), "Format" :: NullOrUndefined (Format), "RegisterEndpoints" :: NullOrUndefined (Boolean), "RoleArn" :: NullOrUndefined (String), "S3Url" :: NullOrUndefined (String), "SegmentId" :: NullOrUndefined (String), "SegmentName" :: NullOrUndefined (String) }
```

#### `ImportJobResource`

``` purescript
newtype ImportJobResource
  = ImportJobResource { "DefineSegment" :: NullOrUndefined (Boolean), "ExternalId" :: NullOrUndefined (String), "Format" :: NullOrUndefined (Format), "RegisterEndpoints" :: NullOrUndefined (Boolean), "RoleArn" :: NullOrUndefined (String), "S3Url" :: NullOrUndefined (String), "SegmentId" :: NullOrUndefined (String), "SegmentName" :: NullOrUndefined (String) }
```

#### `ImportJobResponse`

``` purescript
newtype ImportJobResponse
  = ImportJobResponse { "ApplicationId" :: NullOrUndefined (String), "CompletedPieces" :: NullOrUndefined (Int), "CompletionDate" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (ImportJobResource), "FailedPieces" :: NullOrUndefined (Int), "Failures" :: NullOrUndefined (ListOf__string), "Id" :: NullOrUndefined (String), "JobStatus" :: NullOrUndefined (JobStatus), "TotalFailures" :: NullOrUndefined (Int), "TotalPieces" :: NullOrUndefined (Int), "TotalProcessed" :: NullOrUndefined (Int), "Type" :: NullOrUndefined (String) }
```

#### `ImportJobsResponse`

``` purescript
newtype ImportJobsResponse
  = ImportJobsResponse { "Item" :: NullOrUndefined (ListOfImportJobResponse), "NextToken" :: NullOrUndefined (String) }
```

Import job list.

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

#### `ListOfActivityResponse`

``` purescript
newtype ListOfActivityResponse
  = ListOfActivityResponse (Array ActivityResponse)
```

#### `ListOfApplicationResponse`

``` purescript
newtype ListOfApplicationResponse
  = ListOfApplicationResponse (Array ApplicationResponse)
```

#### `ListOfCampaignResponse`

``` purescript
newtype ListOfCampaignResponse
  = ListOfCampaignResponse (Array CampaignResponse)
```

#### `ListOfEndpointBatchItem`

``` purescript
newtype ListOfEndpointBatchItem
  = ListOfEndpointBatchItem (Array EndpointBatchItem)
```

#### `ListOfImportJobResponse`

``` purescript
newtype ListOfImportJobResponse
  = ListOfImportJobResponse (Array ImportJobResponse)
```

#### `ListOfSegmentResponse`

``` purescript
newtype ListOfSegmentResponse
  = ListOfSegmentResponse (Array SegmentResponse)
```

#### `ListOfTreatmentResource`

``` purescript
newtype ListOfTreatmentResource
  = ListOfTreatmentResource (Array TreatmentResource)
```

#### `ListOfWriteTreatmentResource`

``` purescript
newtype ListOfWriteTreatmentResource
  = ListOfWriteTreatmentResource (Array WriteTreatmentResource)
```

#### `ListOf__string`

``` purescript
newtype ListOf__string
  = ListOf__string (Array String)
```

#### `MapOfAddressConfiguration`

``` purescript
newtype MapOfAddressConfiguration
  = MapOfAddressConfiguration (Map String AddressConfiguration)
```

#### `MapOfAttributeDimension`

``` purescript
newtype MapOfAttributeDimension
  = MapOfAttributeDimension (Map String AttributeDimension)
```

#### `MapOfEndpointMessageResult`

``` purescript
newtype MapOfEndpointMessageResult
  = MapOfEndpointMessageResult (Map String EndpointMessageResult)
```

#### `MapOfEndpointSendConfiguration`

``` purescript
newtype MapOfEndpointSendConfiguration
  = MapOfEndpointSendConfiguration (Map String EndpointSendConfiguration)
```

#### `MapOfListOf__string`

``` purescript
newtype MapOfListOf__string
  = MapOfListOf__string (Map String ListOf__string)
```

#### `MapOfMapOfEndpointMessageResult`

``` purescript
newtype MapOfMapOfEndpointMessageResult
  = MapOfMapOfEndpointMessageResult (Map String MapOfEndpointMessageResult)
```

#### `MapOfMessageResult`

``` purescript
newtype MapOfMessageResult
  = MapOfMessageResult (Map String MessageResult)
```

#### `MapOf__double`

``` purescript
newtype MapOf__double
  = MapOf__double (Map String Number)
```

#### `MapOf__integer`

``` purescript
newtype MapOf__integer
  = MapOf__integer (Map String Int)
```

#### `MapOf__string`

``` purescript
newtype MapOf__string
  = MapOf__string (Map String String)
```

#### `Message`

``` purescript
newtype Message
  = Message { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "ImageIconUrl" :: NullOrUndefined (String), "ImageSmallIconUrl" :: NullOrUndefined (String), "ImageUrl" :: NullOrUndefined (String), "JsonBody" :: NullOrUndefined (String), "MediaUrl" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

#### `MessageBody`

``` purescript
newtype MessageBody
  = MessageBody { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

#### `MessageConfiguration`

``` purescript
newtype MessageConfiguration
  = MessageConfiguration { "ADMMessage" :: NullOrUndefined (Message), "APNSMessage" :: NullOrUndefined (Message), "BaiduMessage" :: NullOrUndefined (Message), "DefaultMessage" :: NullOrUndefined (Message), "EmailMessage" :: NullOrUndefined (CampaignEmailMessage), "GCMMessage" :: NullOrUndefined (Message), "SMSMessage" :: NullOrUndefined (CampaignSmsMessage) }
```

Message configuration for a campaign.

#### `MessageRequest`

``` purescript
newtype MessageRequest
  = MessageRequest { "Addresses" :: NullOrUndefined (MapOfAddressConfiguration), "Context" :: NullOrUndefined (MapOf__string), "Endpoints" :: NullOrUndefined (MapOfEndpointSendConfiguration), "MessageConfiguration" :: NullOrUndefined (DirectMessageConfiguration) }
```

Send message request.

#### `MessageResponse`

``` purescript
newtype MessageResponse
  = MessageResponse { "ApplicationId" :: NullOrUndefined (String), "EndpointResult" :: NullOrUndefined (MapOfEndpointMessageResult), "RequestId" :: NullOrUndefined (String), "Result" :: NullOrUndefined (MapOfMessageResult) }
```

Send message response.

#### `MessageResult`

``` purescript
newtype MessageResult
  = MessageResult { "DeliveryStatus" :: NullOrUndefined (DeliveryStatus), "StatusCode" :: NullOrUndefined (Int), "StatusMessage" :: NullOrUndefined (String), "UpdatedToken" :: NullOrUndefined (String) }
```

The result from sending a message to an address.

#### `MessageType`

``` purescript
newtype MessageType
  = MessageType String
```

#### `MethodNotAllowedException`

``` purescript
newtype MethodNotAllowedException
  = MethodNotAllowedException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

#### `PutEventStreamRequest`

``` purescript
newtype PutEventStreamRequest
  = PutEventStreamRequest { "ApplicationId" :: String, "WriteEventStream" :: WriteEventStream }
```

#### `PutEventStreamResponse`

``` purescript
newtype PutEventStreamResponse
  = PutEventStreamResponse { "EventStream" :: EventStream }
```

#### `QuietTime`

``` purescript
newtype QuietTime
  = QuietTime { "End" :: NullOrUndefined (String), "Start" :: NullOrUndefined (String) }
```

Quiet Time

#### `RecencyDimension`

``` purescript
newtype RecencyDimension
  = RecencyDimension { "Duration" :: NullOrUndefined (Duration), "RecencyType" :: NullOrUndefined (RecencyType) }
```

Define how a segment based on recency of use.

#### `RecencyType`

``` purescript
newtype RecencyType
  = RecencyType String
```

#### `SMSChannelRequest`

``` purescript
newtype SMSChannelRequest
  = SMSChannelRequest { "Enabled" :: NullOrUndefined (Boolean), "SenderId" :: NullOrUndefined (String), "ShortCode" :: NullOrUndefined (String) }
```

SMS Channel Request

#### `SMSChannelResponse`

``` purescript
newtype SMSChannelResponse
  = SMSChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "SenderId" :: NullOrUndefined (String), "ShortCode" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

SMS Channel Response.

#### `SMSMessage`

``` purescript
newtype SMSMessage
  = SMSMessage { "Body" :: NullOrUndefined (String), "MessageType" :: NullOrUndefined (MessageType), "SenderId" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string) }
```

SMS Message.

#### `Schedule`

``` purescript
newtype Schedule
  = Schedule { "EndTime" :: NullOrUndefined (String), "Frequency" :: NullOrUndefined (Frequency), "IsLocalTime" :: NullOrUndefined (Boolean), "QuietTime" :: NullOrUndefined (QuietTime), "StartTime" :: NullOrUndefined (String), "Timezone" :: NullOrUndefined (String) }
```

Shcedule that defines when a campaign is run.

#### `SegmentBehaviors`

``` purescript
newtype SegmentBehaviors
  = SegmentBehaviors { "Recency" :: NullOrUndefined (RecencyDimension) }
```

Segment behavior dimensions

#### `SegmentDemographics`

``` purescript
newtype SegmentDemographics
  = SegmentDemographics { "AppVersion" :: NullOrUndefined (SetDimension), "Channel" :: NullOrUndefined (SetDimension), "DeviceType" :: NullOrUndefined (SetDimension), "Make" :: NullOrUndefined (SetDimension), "Model" :: NullOrUndefined (SetDimension), "Platform" :: NullOrUndefined (SetDimension) }
```

Segment demographic dimensions

#### `SegmentDimensions`

``` purescript
newtype SegmentDimensions
  = SegmentDimensions { "Attributes" :: NullOrUndefined (MapOfAttributeDimension), "Behavior" :: NullOrUndefined (SegmentBehaviors), "Demographic" :: NullOrUndefined (SegmentDemographics), "Location" :: NullOrUndefined (SegmentLocation), "UserAttributes" :: NullOrUndefined (MapOfAttributeDimension) }
```

Segment dimensions

#### `SegmentImportResource`

``` purescript
newtype SegmentImportResource
  = SegmentImportResource { "ChannelCounts" :: NullOrUndefined (MapOf__integer), "ExternalId" :: NullOrUndefined (String), "Format" :: NullOrUndefined (Format), "RoleArn" :: NullOrUndefined (String), "S3Url" :: NullOrUndefined (String), "Size" :: NullOrUndefined (Int) }
```

Segment import definition.

#### `SegmentLocation`

``` purescript
newtype SegmentLocation
  = SegmentLocation { "Country" :: NullOrUndefined (SetDimension) }
```

Segment location dimensions

#### `SegmentResponse`

``` purescript
newtype SegmentResponse
  = SegmentResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Dimensions" :: NullOrUndefined (SegmentDimensions), "Id" :: NullOrUndefined (String), "ImportDefinition" :: NullOrUndefined (SegmentImportResource), "LastModifiedDate" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SegmentType" :: NullOrUndefined (SegmentType), "Version" :: NullOrUndefined (Int) }
```

Segment definition.

#### `SegmentType`

``` purescript
newtype SegmentType
  = SegmentType String
```

#### `SegmentsResponse`

``` purescript
newtype SegmentsResponse
  = SegmentsResponse { "Item" :: NullOrUndefined (ListOfSegmentResponse), "NextToken" :: NullOrUndefined (String) }
```

Segments in your account.

#### `SendMessagesRequest`

``` purescript
newtype SendMessagesRequest
  = SendMessagesRequest { "ApplicationId" :: String, "MessageRequest" :: MessageRequest }
```

#### `SendMessagesResponse`

``` purescript
newtype SendMessagesResponse
  = SendMessagesResponse { "MessageResponse" :: MessageResponse }
```

#### `SendUsersMessageRequest`

``` purescript
newtype SendUsersMessageRequest
  = SendUsersMessageRequest { "Context" :: NullOrUndefined (MapOf__string), "MessageConfiguration" :: NullOrUndefined (DirectMessageConfiguration), "Users" :: NullOrUndefined (MapOfEndpointSendConfiguration) }
```

Send message request.

#### `SendUsersMessageResponse`

``` purescript
newtype SendUsersMessageResponse
  = SendUsersMessageResponse { "ApplicationId" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Result" :: NullOrUndefined (MapOfMapOfEndpointMessageResult) }
```

User send message response.

#### `SendUsersMessagesRequest`

``` purescript
newtype SendUsersMessagesRequest
  = SendUsersMessagesRequest { "ApplicationId" :: String, "SendUsersMessageRequest" :: SendUsersMessageRequest }
```

#### `SendUsersMessagesResponse`

``` purescript
newtype SendUsersMessagesResponse
  = SendUsersMessagesResponse { "SendUsersMessageResponse" :: SendUsersMessageResponse }
```

#### `SetDimension`

``` purescript
newtype SetDimension
  = SetDimension { "DimensionType" :: NullOrUndefined (DimensionType), "Values" :: NullOrUndefined (ListOf__string) }
```

Dimension specification of a segment.

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

#### `TreatmentResource`

``` purescript
newtype TreatmentResource
  = TreatmentResource { "Id" :: NullOrUndefined (String), "MessageConfiguration" :: NullOrUndefined (MessageConfiguration), "Schedule" :: NullOrUndefined (Schedule), "SizePercent" :: NullOrUndefined (Int), "State" :: NullOrUndefined (CampaignState), "TreatmentDescription" :: NullOrUndefined (String), "TreatmentName" :: NullOrUndefined (String) }
```

Treatment resource

#### `UpdateAdmChannelRequest`

``` purescript
newtype UpdateAdmChannelRequest
  = UpdateAdmChannelRequest { "ADMChannelRequest" :: ADMChannelRequest, "ApplicationId" :: String }
```

#### `UpdateAdmChannelResponse`

``` purescript
newtype UpdateAdmChannelResponse
  = UpdateAdmChannelResponse { "ADMChannelResponse" :: ADMChannelResponse }
```

#### `UpdateApnsChannelRequest`

``` purescript
newtype UpdateApnsChannelRequest
  = UpdateApnsChannelRequest { "APNSChannelRequest" :: APNSChannelRequest, "ApplicationId" :: String }
```

#### `UpdateApnsChannelResponse`

``` purescript
newtype UpdateApnsChannelResponse
  = UpdateApnsChannelResponse { "APNSChannelResponse" :: APNSChannelResponse }
```

#### `UpdateApnsSandboxChannelRequest`

``` purescript
newtype UpdateApnsSandboxChannelRequest
  = UpdateApnsSandboxChannelRequest { "APNSSandboxChannelRequest" :: APNSSandboxChannelRequest, "ApplicationId" :: String }
```

#### `UpdateApnsSandboxChannelResponse`

``` purescript
newtype UpdateApnsSandboxChannelResponse
  = UpdateApnsSandboxChannelResponse { "APNSSandboxChannelResponse" :: APNSSandboxChannelResponse }
```

#### `UpdateApnsVoipChannelRequest`

``` purescript
newtype UpdateApnsVoipChannelRequest
  = UpdateApnsVoipChannelRequest { "APNSVoipChannelRequest" :: APNSVoipChannelRequest, "ApplicationId" :: String }
```

#### `UpdateApnsVoipChannelResponse`

``` purescript
newtype UpdateApnsVoipChannelResponse
  = UpdateApnsVoipChannelResponse { "APNSVoipChannelResponse" :: APNSVoipChannelResponse }
```

#### `UpdateApnsVoipSandboxChannelRequest`

``` purescript
newtype UpdateApnsVoipSandboxChannelRequest
  = UpdateApnsVoipSandboxChannelRequest { "APNSVoipSandboxChannelRequest" :: APNSVoipSandboxChannelRequest, "ApplicationId" :: String }
```

#### `UpdateApnsVoipSandboxChannelResponse`

``` purescript
newtype UpdateApnsVoipSandboxChannelResponse
  = UpdateApnsVoipSandboxChannelResponse { "APNSVoipSandboxChannelResponse" :: APNSVoipSandboxChannelResponse }
```

#### `UpdateApplicationSettingsRequest`

``` purescript
newtype UpdateApplicationSettingsRequest
  = UpdateApplicationSettingsRequest { "ApplicationId" :: String, "WriteApplicationSettingsRequest" :: WriteApplicationSettingsRequest }
```

#### `UpdateApplicationSettingsResponse`

``` purescript
newtype UpdateApplicationSettingsResponse
  = UpdateApplicationSettingsResponse { "ApplicationSettingsResource" :: ApplicationSettingsResource }
```

#### `UpdateBaiduChannelRequest`

``` purescript
newtype UpdateBaiduChannelRequest
  = UpdateBaiduChannelRequest { "ApplicationId" :: String, "BaiduChannelRequest" :: BaiduChannelRequest }
```

#### `UpdateBaiduChannelResponse`

``` purescript
newtype UpdateBaiduChannelResponse
  = UpdateBaiduChannelResponse { "BaiduChannelResponse" :: BaiduChannelResponse }
```

#### `UpdateCampaignRequest`

``` purescript
newtype UpdateCampaignRequest
  = UpdateCampaignRequest { "ApplicationId" :: String, "CampaignId" :: String, "WriteCampaignRequest" :: WriteCampaignRequest }
```

#### `UpdateCampaignResponse`

``` purescript
newtype UpdateCampaignResponse
  = UpdateCampaignResponse { "CampaignResponse" :: CampaignResponse }
```

#### `UpdateEmailChannelRequest`

``` purescript
newtype UpdateEmailChannelRequest
  = UpdateEmailChannelRequest { "ApplicationId" :: String, "EmailChannelRequest" :: EmailChannelRequest }
```

#### `UpdateEmailChannelResponse`

``` purescript
newtype UpdateEmailChannelResponse
  = UpdateEmailChannelResponse { "EmailChannelResponse" :: EmailChannelResponse }
```

#### `UpdateEndpointRequest`

``` purescript
newtype UpdateEndpointRequest
  = UpdateEndpointRequest { "ApplicationId" :: String, "EndpointId" :: String, "EndpointRequest" :: EndpointRequest }
```

#### `UpdateEndpointResponse`

``` purescript
newtype UpdateEndpointResponse
  = UpdateEndpointResponse { "MessageBody" :: MessageBody }
```

#### `UpdateEndpointsBatchRequest`

``` purescript
newtype UpdateEndpointsBatchRequest
  = UpdateEndpointsBatchRequest { "ApplicationId" :: String, "EndpointBatchRequest" :: EndpointBatchRequest }
```

#### `UpdateEndpointsBatchResponse`

``` purescript
newtype UpdateEndpointsBatchResponse
  = UpdateEndpointsBatchResponse { "MessageBody" :: MessageBody }
```

#### `UpdateGcmChannelRequest`

``` purescript
newtype UpdateGcmChannelRequest
  = UpdateGcmChannelRequest { "ApplicationId" :: String, "GCMChannelRequest" :: GCMChannelRequest }
```

#### `UpdateGcmChannelResponse`

``` purescript
newtype UpdateGcmChannelResponse
  = UpdateGcmChannelResponse { "GCMChannelResponse" :: GCMChannelResponse }
```

#### `UpdateSegmentRequest`

``` purescript
newtype UpdateSegmentRequest
  = UpdateSegmentRequest { "ApplicationId" :: String, "SegmentId" :: String, "WriteSegmentRequest" :: WriteSegmentRequest }
```

#### `UpdateSegmentResponse`

``` purescript
newtype UpdateSegmentResponse
  = UpdateSegmentResponse { "SegmentResponse" :: SegmentResponse }
```

#### `UpdateSmsChannelRequest`

``` purescript
newtype UpdateSmsChannelRequest
  = UpdateSmsChannelRequest { "ApplicationId" :: String, "SMSChannelRequest" :: SMSChannelRequest }
```

#### `UpdateSmsChannelResponse`

``` purescript
newtype UpdateSmsChannelResponse
  = UpdateSmsChannelResponse { "SMSChannelResponse" :: SMSChannelResponse }
```

#### `WriteApplicationSettingsRequest`

``` purescript
newtype WriteApplicationSettingsRequest
  = WriteApplicationSettingsRequest { "Limits" :: NullOrUndefined (CampaignLimits), "QuietTime" :: NullOrUndefined (QuietTime) }
```

Creating application setting request

#### `WriteCampaignRequest`

``` purescript
newtype WriteCampaignRequest
  = WriteCampaignRequest { "AdditionalTreatments" :: NullOrUndefined (ListOfWriteTreatmentResource), "Description" :: NullOrUndefined (String), "HoldoutPercent" :: NullOrUndefined (Int), "IsPaused" :: NullOrUndefined (Boolean), "Limits" :: NullOrUndefined (CampaignLimits), "MessageConfiguration" :: NullOrUndefined (MessageConfiguration), "Name" :: NullOrUndefined (String), "Schedule" :: NullOrUndefined (Schedule), "SegmentId" :: NullOrUndefined (String), "SegmentVersion" :: NullOrUndefined (Int), "TreatmentDescription" :: NullOrUndefined (String), "TreatmentName" :: NullOrUndefined (String) }
```

Used to create a campaign.

#### `WriteEventStream`

``` purescript
newtype WriteEventStream
  = WriteEventStream { "DestinationStreamArn" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Request to save an EventStream.

#### `WriteSegmentRequest`

``` purescript
newtype WriteSegmentRequest
  = WriteSegmentRequest { "Dimensions" :: NullOrUndefined (SegmentDimensions), "Name" :: NullOrUndefined (String) }
```

Segment definition.

#### `WriteTreatmentResource`

``` purescript
newtype WriteTreatmentResource
  = WriteTreatmentResource { "MessageConfiguration" :: NullOrUndefined (MessageConfiguration), "Schedule" :: NullOrUndefined (Schedule), "SizePercent" :: NullOrUndefined (Int), "TreatmentDescription" :: NullOrUndefined (String), "TreatmentName" :: NullOrUndefined (String) }
```

Used to create a campaign treatment.


