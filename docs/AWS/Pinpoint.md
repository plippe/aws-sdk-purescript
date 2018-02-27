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

##### Instances
``` purescript
Newtype ADMChannelRequest _
```

#### `ADMChannelResponse`

``` purescript
newtype ADMChannelResponse
  = ADMChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Amazon Device Messaging channel definition.

##### Instances
``` purescript
Newtype ADMChannelResponse _
```

#### `ADMMessage`

``` purescript
newtype ADMMessage
  = ADMMessage { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "ConsolidationKey" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "ExpiresAfter" :: NullOrUndefined (String), "IconReference" :: NullOrUndefined (String), "ImageIconUrl" :: NullOrUndefined (String), "ImageUrl" :: NullOrUndefined (String), "MD5" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "SmallImageIconUrl" :: NullOrUndefined (String), "Sound" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

ADM Message.

##### Instances
``` purescript
Newtype ADMMessage _
```

#### `APNSChannelRequest`

``` purescript
newtype APNSChannelRequest
  = APNSChannelRequest { "BundleId" :: NullOrUndefined (String), "Certificate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "PrivateKey" :: NullOrUndefined (String), "TeamId" :: NullOrUndefined (String), "TokenKey" :: NullOrUndefined (String), "TokenKeyId" :: NullOrUndefined (String) }
```

Apple Push Notification Service channel definition.

##### Instances
``` purescript
Newtype APNSChannelRequest _
```

#### `APNSChannelResponse`

``` purescript
newtype APNSChannelResponse
  = APNSChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "HasTokenKey" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Apple Distribution Push Notification Service channel definition.

##### Instances
``` purescript
Newtype APNSChannelResponse _
```

#### `APNSMessage`

``` purescript
newtype APNSMessage
  = APNSMessage { "Action" :: NullOrUndefined (Action), "Badge" :: NullOrUndefined (Int), "Body" :: NullOrUndefined (String), "Category" :: NullOrUndefined (String), "CollapseId" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "MediaUrl" :: NullOrUndefined (String), "PreferredAuthenticationMethod" :: NullOrUndefined (String), "Priority" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "Sound" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "ThreadId" :: NullOrUndefined (String), "TimeToLive" :: NullOrUndefined (Int), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

APNS Message.

##### Instances
``` purescript
Newtype APNSMessage _
```

#### `APNSSandboxChannelRequest`

``` purescript
newtype APNSSandboxChannelRequest
  = APNSSandboxChannelRequest { "BundleId" :: NullOrUndefined (String), "Certificate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "PrivateKey" :: NullOrUndefined (String), "TeamId" :: NullOrUndefined (String), "TokenKey" :: NullOrUndefined (String), "TokenKeyId" :: NullOrUndefined (String) }
```

Apple Development Push Notification Service channel definition.

##### Instances
``` purescript
Newtype APNSSandboxChannelRequest _
```

#### `APNSSandboxChannelResponse`

``` purescript
newtype APNSSandboxChannelResponse
  = APNSSandboxChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "HasTokenKey" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Apple Development Push Notification Service channel definition.

##### Instances
``` purescript
Newtype APNSSandboxChannelResponse _
```

#### `APNSVoipChannelRequest`

``` purescript
newtype APNSVoipChannelRequest
  = APNSVoipChannelRequest { "BundleId" :: NullOrUndefined (String), "Certificate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "PrivateKey" :: NullOrUndefined (String), "TeamId" :: NullOrUndefined (String), "TokenKey" :: NullOrUndefined (String), "TokenKeyId" :: NullOrUndefined (String) }
```

Apple VoIP Push Notification Service channel definition.

##### Instances
``` purescript
Newtype APNSVoipChannelRequest _
```

#### `APNSVoipChannelResponse`

``` purescript
newtype APNSVoipChannelResponse
  = APNSVoipChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "HasTokenKey" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Apple VoIP Push Notification Service channel definition.

##### Instances
``` purescript
Newtype APNSVoipChannelResponse _
```

#### `APNSVoipSandboxChannelRequest`

``` purescript
newtype APNSVoipSandboxChannelRequest
  = APNSVoipSandboxChannelRequest { "BundleId" :: NullOrUndefined (String), "Certificate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "PrivateKey" :: NullOrUndefined (String), "TeamId" :: NullOrUndefined (String), "TokenKey" :: NullOrUndefined (String), "TokenKeyId" :: NullOrUndefined (String) }
```

Apple VoIP Developer Push Notification Service channel definition.

##### Instances
``` purescript
Newtype APNSVoipSandboxChannelRequest _
```

#### `APNSVoipSandboxChannelResponse`

``` purescript
newtype APNSVoipSandboxChannelResponse
  = APNSVoipSandboxChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultAuthenticationMethod" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "HasTokenKey" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Apple VoIP Developer Push Notification Service channel definition.

##### Instances
``` purescript
Newtype APNSVoipSandboxChannelResponse _
```

#### `Action`

``` purescript
newtype Action
  = Action String
```

##### Instances
``` purescript
Newtype Action _
```

#### `ActivitiesResponse`

``` purescript
newtype ActivitiesResponse
  = ActivitiesResponse { "Item" :: NullOrUndefined (ListOfActivityResponse) }
```

Activities for campaign.

##### Instances
``` purescript
Newtype ActivitiesResponse _
```

#### `ActivityResponse`

``` purescript
newtype ActivityResponse
  = ActivityResponse { "ApplicationId" :: NullOrUndefined (String), "CampaignId" :: NullOrUndefined (String), "End" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Result" :: NullOrUndefined (String), "ScheduledStart" :: NullOrUndefined (String), "Start" :: NullOrUndefined (String), "State" :: NullOrUndefined (String), "SuccessfulEndpointCount" :: NullOrUndefined (Int), "TimezonesCompletedCount" :: NullOrUndefined (Int), "TimezonesTotalCount" :: NullOrUndefined (Int), "TotalEndpointCount" :: NullOrUndefined (Int), "TreatmentId" :: NullOrUndefined (String) }
```

Activity definition

##### Instances
``` purescript
Newtype ActivityResponse _
```

#### `AddressConfiguration`

``` purescript
newtype AddressConfiguration
  = AddressConfiguration { "BodyOverride" :: NullOrUndefined (String), "ChannelType" :: NullOrUndefined (ChannelType), "Context" :: NullOrUndefined (MapOf__string), "RawContent" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "TitleOverride" :: NullOrUndefined (String) }
```

Address configuration.

##### Instances
``` purescript
Newtype AddressConfiguration _
```

#### `ApplicationResponse`

``` purescript
newtype ApplicationResponse
  = ApplicationResponse { "Id" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String) }
```

Application Response.

##### Instances
``` purescript
Newtype ApplicationResponse _
```

#### `ApplicationSettingsResource`

``` purescript
newtype ApplicationSettingsResource
  = ApplicationSettingsResource { "ApplicationId" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Limits" :: NullOrUndefined (CampaignLimits), "QuietTime" :: NullOrUndefined (QuietTime) }
```

Application settings.

##### Instances
``` purescript
Newtype ApplicationSettingsResource _
```

#### `ApplicationsResponse`

``` purescript
newtype ApplicationsResponse
  = ApplicationsResponse { "Item" :: NullOrUndefined (ListOfApplicationResponse), "NextToken" :: NullOrUndefined (String) }
```

Get Applications Result.

##### Instances
``` purescript
Newtype ApplicationsResponse _
```

#### `AttributeDimension`

``` purescript
newtype AttributeDimension
  = AttributeDimension { "AttributeType" :: NullOrUndefined (AttributeType), "Values" :: NullOrUndefined (ListOf__string) }
```

Custom attibute dimension

##### Instances
``` purescript
Newtype AttributeDimension _
```

#### `AttributeType`

``` purescript
newtype AttributeType
  = AttributeType String
```

##### Instances
``` purescript
Newtype AttributeType _
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `BaiduChannelRequest`

``` purescript
newtype BaiduChannelRequest
  = BaiduChannelRequest { "ApiKey" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "SecretKey" :: NullOrUndefined (String) }
```

Baidu Cloud Push credentials

##### Instances
``` purescript
Newtype BaiduChannelRequest _
```

#### `BaiduChannelResponse`

``` purescript
newtype BaiduChannelResponse
  = BaiduChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Credential" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Baidu Cloud Messaging channel definition

##### Instances
``` purescript
Newtype BaiduChannelResponse _
```

#### `BaiduMessage`

``` purescript
newtype BaiduMessage
  = BaiduMessage { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "IconReference" :: NullOrUndefined (String), "ImageIconUrl" :: NullOrUndefined (String), "ImageUrl" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "SmallImageIconUrl" :: NullOrUndefined (String), "Sound" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

Baidu Message.

##### Instances
``` purescript
Newtype BaiduMessage _
```

#### `CampaignEmailMessage`

``` purescript
newtype CampaignEmailMessage
  = CampaignEmailMessage { "Body" :: NullOrUndefined (String), "FromAddress" :: NullOrUndefined (String), "HtmlBody" :: NullOrUndefined (String), "Title" :: NullOrUndefined (String) }
```

The email message configuration.

##### Instances
``` purescript
Newtype CampaignEmailMessage _
```

#### `CampaignLimits`

``` purescript
newtype CampaignLimits
  = CampaignLimits { "Daily" :: NullOrUndefined (Int), "MaximumDuration" :: NullOrUndefined (Int), "MessagesPerSecond" :: NullOrUndefined (Int), "Total" :: NullOrUndefined (Int) }
```

Campaign Limits are used to limit the number of messages that can be sent to a user.

##### Instances
``` purescript
Newtype CampaignLimits _
```

#### `CampaignResponse`

``` purescript
newtype CampaignResponse
  = CampaignResponse { "AdditionalTreatments" :: NullOrUndefined (ListOfTreatmentResource), "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "DefaultState" :: NullOrUndefined (CampaignState), "Description" :: NullOrUndefined (String), "HoldoutPercent" :: NullOrUndefined (Int), "Id" :: NullOrUndefined (String), "IsPaused" :: NullOrUndefined (Boolean), "LastModifiedDate" :: NullOrUndefined (String), "Limits" :: NullOrUndefined (CampaignLimits), "MessageConfiguration" :: NullOrUndefined (MessageConfiguration), "Name" :: NullOrUndefined (String), "Schedule" :: NullOrUndefined (Schedule), "SegmentId" :: NullOrUndefined (String), "SegmentVersion" :: NullOrUndefined (Int), "State" :: NullOrUndefined (CampaignState), "TreatmentDescription" :: NullOrUndefined (String), "TreatmentName" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Campaign definition

##### Instances
``` purescript
Newtype CampaignResponse _
```

#### `CampaignSmsMessage`

``` purescript
newtype CampaignSmsMessage
  = CampaignSmsMessage { "Body" :: NullOrUndefined (String), "MessageType" :: NullOrUndefined (MessageType), "SenderId" :: NullOrUndefined (String) }
```

SMS message configuration.

##### Instances
``` purescript
Newtype CampaignSmsMessage _
```

#### `CampaignState`

``` purescript
newtype CampaignState
  = CampaignState { "CampaignStatus" :: NullOrUndefined (CampaignStatus) }
```

State of the Campaign

##### Instances
``` purescript
Newtype CampaignState _
```

#### `CampaignStatus`

``` purescript
newtype CampaignStatus
  = CampaignStatus String
```

##### Instances
``` purescript
Newtype CampaignStatus _
```

#### `CampaignsResponse`

``` purescript
newtype CampaignsResponse
  = CampaignsResponse { "Item" :: NullOrUndefined (ListOfCampaignResponse), "NextToken" :: NullOrUndefined (String) }
```

List of available campaigns.

##### Instances
``` purescript
Newtype CampaignsResponse _
```

#### `ChannelType`

``` purescript
newtype ChannelType
  = ChannelType String
```

##### Instances
``` purescript
Newtype ChannelType _
```

#### `CreateAppRequest`

``` purescript
newtype CreateAppRequest
  = CreateAppRequest { "CreateApplicationRequest" :: CreateApplicationRequest }
```

##### Instances
``` purescript
Newtype CreateAppRequest _
```

#### `CreateAppResponse`

``` purescript
newtype CreateAppResponse
  = CreateAppResponse { "ApplicationResponse" :: ApplicationResponse }
```

##### Instances
``` purescript
Newtype CreateAppResponse _
```

#### `CreateApplicationRequest`

``` purescript
newtype CreateApplicationRequest
  = CreateApplicationRequest { "Name" :: NullOrUndefined (String) }
```

Application Request.

##### Instances
``` purescript
Newtype CreateApplicationRequest _
```

#### `CreateCampaignRequest`

``` purescript
newtype CreateCampaignRequest
  = CreateCampaignRequest { "ApplicationId" :: String, "WriteCampaignRequest" :: WriteCampaignRequest }
```

##### Instances
``` purescript
Newtype CreateCampaignRequest _
```

#### `CreateCampaignResponse`

``` purescript
newtype CreateCampaignResponse
  = CreateCampaignResponse { "CampaignResponse" :: CampaignResponse }
```

##### Instances
``` purescript
Newtype CreateCampaignResponse _
```

#### `CreateImportJobRequest`

``` purescript
newtype CreateImportJobRequest
  = CreateImportJobRequest { "ApplicationId" :: String, "ImportJobRequest" :: ImportJobRequest }
```

##### Instances
``` purescript
Newtype CreateImportJobRequest _
```

#### `CreateImportJobResponse`

``` purescript
newtype CreateImportJobResponse
  = CreateImportJobResponse { "ImportJobResponse" :: ImportJobResponse }
```

##### Instances
``` purescript
Newtype CreateImportJobResponse _
```

#### `CreateSegmentRequest`

``` purescript
newtype CreateSegmentRequest
  = CreateSegmentRequest { "ApplicationId" :: String, "WriteSegmentRequest" :: WriteSegmentRequest }
```

##### Instances
``` purescript
Newtype CreateSegmentRequest _
```

#### `CreateSegmentResponse`

``` purescript
newtype CreateSegmentResponse
  = CreateSegmentResponse { "SegmentResponse" :: SegmentResponse }
```

##### Instances
``` purescript
Newtype CreateSegmentResponse _
```

#### `DefaultMessage`

``` purescript
newtype DefaultMessage
  = DefaultMessage { "Body" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string) }
```

Default Message across push notification, email, and sms.

##### Instances
``` purescript
Newtype DefaultMessage _
```

#### `DefaultPushNotificationMessage`

``` purescript
newtype DefaultPushNotificationMessage
  = DefaultPushNotificationMessage { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "SilentPush" :: NullOrUndefined (Boolean), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

Default Push Notification Message.

##### Instances
``` purescript
Newtype DefaultPushNotificationMessage _
```

#### `DeleteAdmChannelRequest`

``` purescript
newtype DeleteAdmChannelRequest
  = DeleteAdmChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteAdmChannelRequest _
```

#### `DeleteAdmChannelResponse`

``` purescript
newtype DeleteAdmChannelResponse
  = DeleteAdmChannelResponse { "ADMChannelResponse" :: ADMChannelResponse }
```

##### Instances
``` purescript
Newtype DeleteAdmChannelResponse _
```

#### `DeleteApnsChannelRequest`

``` purescript
newtype DeleteApnsChannelRequest
  = DeleteApnsChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteApnsChannelRequest _
```

#### `DeleteApnsChannelResponse`

``` purescript
newtype DeleteApnsChannelResponse
  = DeleteApnsChannelResponse { "APNSChannelResponse" :: APNSChannelResponse }
```

##### Instances
``` purescript
Newtype DeleteApnsChannelResponse _
```

#### `DeleteApnsSandboxChannelRequest`

``` purescript
newtype DeleteApnsSandboxChannelRequest
  = DeleteApnsSandboxChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteApnsSandboxChannelRequest _
```

#### `DeleteApnsSandboxChannelResponse`

``` purescript
newtype DeleteApnsSandboxChannelResponse
  = DeleteApnsSandboxChannelResponse { "APNSSandboxChannelResponse" :: APNSSandboxChannelResponse }
```

##### Instances
``` purescript
Newtype DeleteApnsSandboxChannelResponse _
```

#### `DeleteApnsVoipChannelRequest`

``` purescript
newtype DeleteApnsVoipChannelRequest
  = DeleteApnsVoipChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteApnsVoipChannelRequest _
```

#### `DeleteApnsVoipChannelResponse`

``` purescript
newtype DeleteApnsVoipChannelResponse
  = DeleteApnsVoipChannelResponse { "APNSVoipChannelResponse" :: APNSVoipChannelResponse }
```

##### Instances
``` purescript
Newtype DeleteApnsVoipChannelResponse _
```

#### `DeleteApnsVoipSandboxChannelRequest`

``` purescript
newtype DeleteApnsVoipSandboxChannelRequest
  = DeleteApnsVoipSandboxChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteApnsVoipSandboxChannelRequest _
```

#### `DeleteApnsVoipSandboxChannelResponse`

``` purescript
newtype DeleteApnsVoipSandboxChannelResponse
  = DeleteApnsVoipSandboxChannelResponse { "APNSVoipSandboxChannelResponse" :: APNSVoipSandboxChannelResponse }
```

##### Instances
``` purescript
Newtype DeleteApnsVoipSandboxChannelResponse _
```

#### `DeleteAppRequest`

``` purescript
newtype DeleteAppRequest
  = DeleteAppRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteAppRequest _
```

#### `DeleteAppResponse`

``` purescript
newtype DeleteAppResponse
  = DeleteAppResponse { "ApplicationResponse" :: ApplicationResponse }
```

##### Instances
``` purescript
Newtype DeleteAppResponse _
```

#### `DeleteBaiduChannelRequest`

``` purescript
newtype DeleteBaiduChannelRequest
  = DeleteBaiduChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteBaiduChannelRequest _
```

#### `DeleteBaiduChannelResponse`

``` purescript
newtype DeleteBaiduChannelResponse
  = DeleteBaiduChannelResponse { "BaiduChannelResponse" :: BaiduChannelResponse }
```

##### Instances
``` purescript
Newtype DeleteBaiduChannelResponse _
```

#### `DeleteCampaignRequest`

``` purescript
newtype DeleteCampaignRequest
  = DeleteCampaignRequest { "ApplicationId" :: String, "CampaignId" :: String }
```

##### Instances
``` purescript
Newtype DeleteCampaignRequest _
```

#### `DeleteCampaignResponse`

``` purescript
newtype DeleteCampaignResponse
  = DeleteCampaignResponse { "CampaignResponse" :: CampaignResponse }
```

##### Instances
``` purescript
Newtype DeleteCampaignResponse _
```

#### `DeleteEmailChannelRequest`

``` purescript
newtype DeleteEmailChannelRequest
  = DeleteEmailChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteEmailChannelRequest _
```

#### `DeleteEmailChannelResponse`

``` purescript
newtype DeleteEmailChannelResponse
  = DeleteEmailChannelResponse { "EmailChannelResponse" :: EmailChannelResponse }
```

##### Instances
``` purescript
Newtype DeleteEmailChannelResponse _
```

#### `DeleteEventStreamRequest`

``` purescript
newtype DeleteEventStreamRequest
  = DeleteEventStreamRequest { "ApplicationId" :: String }
```

DeleteEventStream Request

##### Instances
``` purescript
Newtype DeleteEventStreamRequest _
```

#### `DeleteEventStreamResponse`

``` purescript
newtype DeleteEventStreamResponse
  = DeleteEventStreamResponse { "EventStream" :: EventStream }
```

##### Instances
``` purescript
Newtype DeleteEventStreamResponse _
```

#### `DeleteGcmChannelRequest`

``` purescript
newtype DeleteGcmChannelRequest
  = DeleteGcmChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteGcmChannelRequest _
```

#### `DeleteGcmChannelResponse`

``` purescript
newtype DeleteGcmChannelResponse
  = DeleteGcmChannelResponse { "GCMChannelResponse" :: GCMChannelResponse }
```

##### Instances
``` purescript
Newtype DeleteGcmChannelResponse _
```

#### `DeleteSegmentRequest`

``` purescript
newtype DeleteSegmentRequest
  = DeleteSegmentRequest { "ApplicationId" :: String, "SegmentId" :: String }
```

##### Instances
``` purescript
Newtype DeleteSegmentRequest _
```

#### `DeleteSegmentResponse`

``` purescript
newtype DeleteSegmentResponse
  = DeleteSegmentResponse { "SegmentResponse" :: SegmentResponse }
```

##### Instances
``` purescript
Newtype DeleteSegmentResponse _
```

#### `DeleteSmsChannelRequest`

``` purescript
newtype DeleteSmsChannelRequest
  = DeleteSmsChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype DeleteSmsChannelRequest _
```

#### `DeleteSmsChannelResponse`

``` purescript
newtype DeleteSmsChannelResponse
  = DeleteSmsChannelResponse { "SMSChannelResponse" :: SMSChannelResponse }
```

##### Instances
``` purescript
Newtype DeleteSmsChannelResponse _
```

#### `DeliveryStatus`

``` purescript
newtype DeliveryStatus
  = DeliveryStatus String
```

##### Instances
``` purescript
Newtype DeliveryStatus _
```

#### `DimensionType`

``` purescript
newtype DimensionType
  = DimensionType String
```

##### Instances
``` purescript
Newtype DimensionType _
```

#### `DirectMessageConfiguration`

``` purescript
newtype DirectMessageConfiguration
  = DirectMessageConfiguration { "ADMMessage" :: NullOrUndefined (ADMMessage), "APNSMessage" :: NullOrUndefined (APNSMessage), "BaiduMessage" :: NullOrUndefined (BaiduMessage), "DefaultMessage" :: NullOrUndefined (DefaultMessage), "DefaultPushNotificationMessage" :: NullOrUndefined (DefaultPushNotificationMessage), "GCMMessage" :: NullOrUndefined (GCMMessage), "SMSMessage" :: NullOrUndefined (SMSMessage) }
```

The message configuration.

##### Instances
``` purescript
Newtype DirectMessageConfiguration _
```

#### `Duration`

``` purescript
newtype Duration
  = Duration String
```

##### Instances
``` purescript
Newtype Duration _
```

#### `EmailChannelRequest`

``` purescript
newtype EmailChannelRequest
  = EmailChannelRequest { "Enabled" :: NullOrUndefined (Boolean), "FromAddress" :: NullOrUndefined (String), "Identity" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Email Channel Request

##### Instances
``` purescript
Newtype EmailChannelRequest _
```

#### `EmailChannelResponse`

``` purescript
newtype EmailChannelResponse
  = EmailChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "FromAddress" :: NullOrUndefined (String), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "Identity" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Email Channel Response.

##### Instances
``` purescript
Newtype EmailChannelResponse _
```

#### `EndpointBatchItem`

``` purescript
newtype EndpointBatchItem
  = EndpointBatchItem { "Address" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (MapOfListOf__string), "ChannelType" :: NullOrUndefined (ChannelType), "Demographic" :: NullOrUndefined (EndpointDemographic), "EffectiveDate" :: NullOrUndefined (String), "EndpointStatus" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Location" :: NullOrUndefined (EndpointLocation), "Metrics" :: NullOrUndefined (MapOf__double), "OptOut" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "User" :: NullOrUndefined (EndpointUser) }
```

Endpoint update request

##### Instances
``` purescript
Newtype EndpointBatchItem _
```

#### `EndpointBatchRequest`

``` purescript
newtype EndpointBatchRequest
  = EndpointBatchRequest { "Item" :: NullOrUndefined (ListOfEndpointBatchItem) }
```

Endpoint batch update request.

##### Instances
``` purescript
Newtype EndpointBatchRequest _
```

#### `EndpointDemographic`

``` purescript
newtype EndpointDemographic
  = EndpointDemographic { "AppVersion" :: NullOrUndefined (String), "Locale" :: NullOrUndefined (String), "Make" :: NullOrUndefined (String), "Model" :: NullOrUndefined (String), "ModelVersion" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "PlatformVersion" :: NullOrUndefined (String), "Timezone" :: NullOrUndefined (String) }
```

Endpoint demographic data

##### Instances
``` purescript
Newtype EndpointDemographic _
```

#### `EndpointLocation`

``` purescript
newtype EndpointLocation
  = EndpointLocation { "City" :: NullOrUndefined (String), "Country" :: NullOrUndefined (String), "Latitude" :: NullOrUndefined (Number), "Longitude" :: NullOrUndefined (Number), "PostalCode" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String) }
```

Endpoint location data

##### Instances
``` purescript
Newtype EndpointLocation _
```

#### `EndpointMessageResult`

``` purescript
newtype EndpointMessageResult
  = EndpointMessageResult { "Address" :: NullOrUndefined (String), "DeliveryStatus" :: NullOrUndefined (DeliveryStatus), "StatusCode" :: NullOrUndefined (Int), "StatusMessage" :: NullOrUndefined (String), "UpdatedToken" :: NullOrUndefined (String) }
```

The result from sending a message to an endpoint.

##### Instances
``` purescript
Newtype EndpointMessageResult _
```

#### `EndpointRequest`

``` purescript
newtype EndpointRequest
  = EndpointRequest { "Address" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (MapOfListOf__string), "ChannelType" :: NullOrUndefined (ChannelType), "Demographic" :: NullOrUndefined (EndpointDemographic), "EffectiveDate" :: NullOrUndefined (String), "EndpointStatus" :: NullOrUndefined (String), "Location" :: NullOrUndefined (EndpointLocation), "Metrics" :: NullOrUndefined (MapOf__double), "OptOut" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "User" :: NullOrUndefined (EndpointUser) }
```

Endpoint update request

##### Instances
``` purescript
Newtype EndpointRequest _
```

#### `EndpointResponse`

``` purescript
newtype EndpointResponse
  = EndpointResponse { "Address" :: NullOrUndefined (String), "ApplicationId" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (MapOfListOf__string), "ChannelType" :: NullOrUndefined (ChannelType), "CohortId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Demographic" :: NullOrUndefined (EndpointDemographic), "EffectiveDate" :: NullOrUndefined (String), "EndpointStatus" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Location" :: NullOrUndefined (EndpointLocation), "Metrics" :: NullOrUndefined (MapOf__double), "OptOut" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "User" :: NullOrUndefined (EndpointUser) }
```

Endpoint response

##### Instances
``` purescript
Newtype EndpointResponse _
```

#### `EndpointSendConfiguration`

``` purescript
newtype EndpointSendConfiguration
  = EndpointSendConfiguration { "BodyOverride" :: NullOrUndefined (String), "Context" :: NullOrUndefined (MapOf__string), "RawContent" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "TitleOverride" :: NullOrUndefined (String) }
```

Endpoint send configuration.

##### Instances
``` purescript
Newtype EndpointSendConfiguration _
```

#### `EndpointUser`

``` purescript
newtype EndpointUser
  = EndpointUser { "UserAttributes" :: NullOrUndefined (MapOfListOf__string), "UserId" :: NullOrUndefined (String) }
```

Endpoint user specific custom userAttributes

##### Instances
``` purescript
Newtype EndpointUser _
```

#### `EventStream`

``` purescript
newtype EventStream
  = EventStream { "ApplicationId" :: NullOrUndefined (String), "DestinationStreamArn" :: NullOrUndefined (String), "ExternalId" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "LastUpdatedBy" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Model for an event publishing subscription export.

##### Instances
``` purescript
Newtype EventStream _
```

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

##### Instances
``` purescript
Newtype ForbiddenException _
```

#### `Format`

``` purescript
newtype Format
  = Format String
```

##### Instances
``` purescript
Newtype Format _
```

#### `Frequency`

``` purescript
newtype Frequency
  = Frequency String
```

##### Instances
``` purescript
Newtype Frequency _
```

#### `GCMChannelRequest`

``` purescript
newtype GCMChannelRequest
  = GCMChannelRequest { "ApiKey" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean) }
```

Google Cloud Messaging credentials

##### Instances
``` purescript
Newtype GCMChannelRequest _
```

#### `GCMChannelResponse`

``` purescript
newtype GCMChannelResponse
  = GCMChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Credential" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

Google Cloud Messaging channel definition

##### Instances
``` purescript
Newtype GCMChannelResponse _
```

#### `GCMMessage`

``` purescript
newtype GCMMessage
  = GCMMessage { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "CollapseKey" :: NullOrUndefined (String), "Data" :: NullOrUndefined (MapOf__string), "IconReference" :: NullOrUndefined (String), "ImageIconUrl" :: NullOrUndefined (String), "ImageUrl" :: NullOrUndefined (String), "Priority" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "RestrictedPackageName" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "SmallImageIconUrl" :: NullOrUndefined (String), "Sound" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string), "TimeToLive" :: NullOrUndefined (Int), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

GCM Message.

##### Instances
``` purescript
Newtype GCMMessage _
```

#### `GetAdmChannelRequest`

``` purescript
newtype GetAdmChannelRequest
  = GetAdmChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetAdmChannelRequest _
```

#### `GetAdmChannelResponse`

``` purescript
newtype GetAdmChannelResponse
  = GetAdmChannelResponse { "ADMChannelResponse" :: ADMChannelResponse }
```

##### Instances
``` purescript
Newtype GetAdmChannelResponse _
```

#### `GetApnsChannelRequest`

``` purescript
newtype GetApnsChannelRequest
  = GetApnsChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetApnsChannelRequest _
```

#### `GetApnsChannelResponse`

``` purescript
newtype GetApnsChannelResponse
  = GetApnsChannelResponse { "APNSChannelResponse" :: APNSChannelResponse }
```

##### Instances
``` purescript
Newtype GetApnsChannelResponse _
```

#### `GetApnsSandboxChannelRequest`

``` purescript
newtype GetApnsSandboxChannelRequest
  = GetApnsSandboxChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetApnsSandboxChannelRequest _
```

#### `GetApnsSandboxChannelResponse`

``` purescript
newtype GetApnsSandboxChannelResponse
  = GetApnsSandboxChannelResponse { "APNSSandboxChannelResponse" :: APNSSandboxChannelResponse }
```

##### Instances
``` purescript
Newtype GetApnsSandboxChannelResponse _
```

#### `GetApnsVoipChannelRequest`

``` purescript
newtype GetApnsVoipChannelRequest
  = GetApnsVoipChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetApnsVoipChannelRequest _
```

#### `GetApnsVoipChannelResponse`

``` purescript
newtype GetApnsVoipChannelResponse
  = GetApnsVoipChannelResponse { "APNSVoipChannelResponse" :: APNSVoipChannelResponse }
```

##### Instances
``` purescript
Newtype GetApnsVoipChannelResponse _
```

#### `GetApnsVoipSandboxChannelRequest`

``` purescript
newtype GetApnsVoipSandboxChannelRequest
  = GetApnsVoipSandboxChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetApnsVoipSandboxChannelRequest _
```

#### `GetApnsVoipSandboxChannelResponse`

``` purescript
newtype GetApnsVoipSandboxChannelResponse
  = GetApnsVoipSandboxChannelResponse { "APNSVoipSandboxChannelResponse" :: APNSVoipSandboxChannelResponse }
```

##### Instances
``` purescript
Newtype GetApnsVoipSandboxChannelResponse _
```

#### `GetAppRequest`

``` purescript
newtype GetAppRequest
  = GetAppRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetAppRequest _
```

#### `GetAppResponse`

``` purescript
newtype GetAppResponse
  = GetAppResponse { "ApplicationResponse" :: ApplicationResponse }
```

##### Instances
``` purescript
Newtype GetAppResponse _
```

#### `GetApplicationSettingsRequest`

``` purescript
newtype GetApplicationSettingsRequest
  = GetApplicationSettingsRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetApplicationSettingsRequest _
```

#### `GetApplicationSettingsResponse`

``` purescript
newtype GetApplicationSettingsResponse
  = GetApplicationSettingsResponse { "ApplicationSettingsResource" :: ApplicationSettingsResource }
```

##### Instances
``` purescript
Newtype GetApplicationSettingsResponse _
```

#### `GetAppsRequest`

``` purescript
newtype GetAppsRequest
  = GetAppsRequest { "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetAppsRequest _
```

#### `GetAppsResponse`

``` purescript
newtype GetAppsResponse
  = GetAppsResponse { "ApplicationsResponse" :: ApplicationsResponse }
```

##### Instances
``` purescript
Newtype GetAppsResponse _
```

#### `GetBaiduChannelRequest`

``` purescript
newtype GetBaiduChannelRequest
  = GetBaiduChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetBaiduChannelRequest _
```

#### `GetBaiduChannelResponse`

``` purescript
newtype GetBaiduChannelResponse
  = GetBaiduChannelResponse { "BaiduChannelResponse" :: BaiduChannelResponse }
```

##### Instances
``` purescript
Newtype GetBaiduChannelResponse _
```

#### `GetCampaignActivitiesRequest`

``` purescript
newtype GetCampaignActivitiesRequest
  = GetCampaignActivitiesRequest { "ApplicationId" :: String, "CampaignId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetCampaignActivitiesRequest _
```

#### `GetCampaignActivitiesResponse`

``` purescript
newtype GetCampaignActivitiesResponse
  = GetCampaignActivitiesResponse { "ActivitiesResponse" :: ActivitiesResponse }
```

##### Instances
``` purescript
Newtype GetCampaignActivitiesResponse _
```

#### `GetCampaignRequest`

``` purescript
newtype GetCampaignRequest
  = GetCampaignRequest { "ApplicationId" :: String, "CampaignId" :: String }
```

##### Instances
``` purescript
Newtype GetCampaignRequest _
```

#### `GetCampaignResponse`

``` purescript
newtype GetCampaignResponse
  = GetCampaignResponse { "CampaignResponse" :: CampaignResponse }
```

##### Instances
``` purescript
Newtype GetCampaignResponse _
```

#### `GetCampaignVersionRequest`

``` purescript
newtype GetCampaignVersionRequest
  = GetCampaignVersionRequest { "ApplicationId" :: String, "CampaignId" :: String, "Version" :: String }
```

##### Instances
``` purescript
Newtype GetCampaignVersionRequest _
```

#### `GetCampaignVersionResponse`

``` purescript
newtype GetCampaignVersionResponse
  = GetCampaignVersionResponse { "CampaignResponse" :: CampaignResponse }
```

##### Instances
``` purescript
Newtype GetCampaignVersionResponse _
```

#### `GetCampaignVersionsRequest`

``` purescript
newtype GetCampaignVersionsRequest
  = GetCampaignVersionsRequest { "ApplicationId" :: String, "CampaignId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetCampaignVersionsRequest _
```

#### `GetCampaignVersionsResponse`

``` purescript
newtype GetCampaignVersionsResponse
  = GetCampaignVersionsResponse { "CampaignsResponse" :: CampaignsResponse }
```

##### Instances
``` purescript
Newtype GetCampaignVersionsResponse _
```

#### `GetCampaignsRequest`

``` purescript
newtype GetCampaignsRequest
  = GetCampaignsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetCampaignsRequest _
```

#### `GetCampaignsResponse`

``` purescript
newtype GetCampaignsResponse
  = GetCampaignsResponse { "CampaignsResponse" :: CampaignsResponse }
```

##### Instances
``` purescript
Newtype GetCampaignsResponse _
```

#### `GetEmailChannelRequest`

``` purescript
newtype GetEmailChannelRequest
  = GetEmailChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetEmailChannelRequest _
```

#### `GetEmailChannelResponse`

``` purescript
newtype GetEmailChannelResponse
  = GetEmailChannelResponse { "EmailChannelResponse" :: EmailChannelResponse }
```

##### Instances
``` purescript
Newtype GetEmailChannelResponse _
```

#### `GetEndpointRequest`

``` purescript
newtype GetEndpointRequest
  = GetEndpointRequest { "ApplicationId" :: String, "EndpointId" :: String }
```

##### Instances
``` purescript
Newtype GetEndpointRequest _
```

#### `GetEndpointResponse`

``` purescript
newtype GetEndpointResponse
  = GetEndpointResponse { "EndpointResponse" :: EndpointResponse }
```

##### Instances
``` purescript
Newtype GetEndpointResponse _
```

#### `GetEventStreamRequest`

``` purescript
newtype GetEventStreamRequest
  = GetEventStreamRequest { "ApplicationId" :: String }
```

GetEventStreamRequest

##### Instances
``` purescript
Newtype GetEventStreamRequest _
```

#### `GetEventStreamResponse`

``` purescript
newtype GetEventStreamResponse
  = GetEventStreamResponse { "EventStream" :: EventStream }
```

##### Instances
``` purescript
Newtype GetEventStreamResponse _
```

#### `GetGcmChannelRequest`

``` purescript
newtype GetGcmChannelRequest
  = GetGcmChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetGcmChannelRequest _
```

#### `GetGcmChannelResponse`

``` purescript
newtype GetGcmChannelResponse
  = GetGcmChannelResponse { "GCMChannelResponse" :: GCMChannelResponse }
```

##### Instances
``` purescript
Newtype GetGcmChannelResponse _
```

#### `GetImportJobRequest`

``` purescript
newtype GetImportJobRequest
  = GetImportJobRequest { "ApplicationId" :: String, "JobId" :: String }
```

##### Instances
``` purescript
Newtype GetImportJobRequest _
```

#### `GetImportJobResponse`

``` purescript
newtype GetImportJobResponse
  = GetImportJobResponse { "ImportJobResponse" :: ImportJobResponse }
```

##### Instances
``` purescript
Newtype GetImportJobResponse _
```

#### `GetImportJobsRequest`

``` purescript
newtype GetImportJobsRequest
  = GetImportJobsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetImportJobsRequest _
```

#### `GetImportJobsResponse`

``` purescript
newtype GetImportJobsResponse
  = GetImportJobsResponse { "ImportJobsResponse" :: ImportJobsResponse }
```

##### Instances
``` purescript
Newtype GetImportJobsResponse _
```

#### `GetSegmentImportJobsRequest`

``` purescript
newtype GetSegmentImportJobsRequest
  = GetSegmentImportJobsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "SegmentId" :: String, "Token" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetSegmentImportJobsRequest _
```

#### `GetSegmentImportJobsResponse`

``` purescript
newtype GetSegmentImportJobsResponse
  = GetSegmentImportJobsResponse { "ImportJobsResponse" :: ImportJobsResponse }
```

##### Instances
``` purescript
Newtype GetSegmentImportJobsResponse _
```

#### `GetSegmentRequest`

``` purescript
newtype GetSegmentRequest
  = GetSegmentRequest { "ApplicationId" :: String, "SegmentId" :: String }
```

##### Instances
``` purescript
Newtype GetSegmentRequest _
```

#### `GetSegmentResponse`

``` purescript
newtype GetSegmentResponse
  = GetSegmentResponse { "SegmentResponse" :: SegmentResponse }
```

##### Instances
``` purescript
Newtype GetSegmentResponse _
```

#### `GetSegmentVersionRequest`

``` purescript
newtype GetSegmentVersionRequest
  = GetSegmentVersionRequest { "ApplicationId" :: String, "SegmentId" :: String, "Version" :: String }
```

##### Instances
``` purescript
Newtype GetSegmentVersionRequest _
```

#### `GetSegmentVersionResponse`

``` purescript
newtype GetSegmentVersionResponse
  = GetSegmentVersionResponse { "SegmentResponse" :: SegmentResponse }
```

##### Instances
``` purescript
Newtype GetSegmentVersionResponse _
```

#### `GetSegmentVersionsRequest`

``` purescript
newtype GetSegmentVersionsRequest
  = GetSegmentVersionsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "SegmentId" :: String, "Token" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetSegmentVersionsRequest _
```

#### `GetSegmentVersionsResponse`

``` purescript
newtype GetSegmentVersionsResponse
  = GetSegmentVersionsResponse { "SegmentsResponse" :: SegmentsResponse }
```

##### Instances
``` purescript
Newtype GetSegmentVersionsResponse _
```

#### `GetSegmentsRequest`

``` purescript
newtype GetSegmentsRequest
  = GetSegmentsRequest { "ApplicationId" :: String, "PageSize" :: NullOrUndefined (String), "Token" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype GetSegmentsRequest _
```

#### `GetSegmentsResponse`

``` purescript
newtype GetSegmentsResponse
  = GetSegmentsResponse { "SegmentsResponse" :: SegmentsResponse }
```

##### Instances
``` purescript
Newtype GetSegmentsResponse _
```

#### `GetSmsChannelRequest`

``` purescript
newtype GetSmsChannelRequest
  = GetSmsChannelRequest { "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype GetSmsChannelRequest _
```

#### `GetSmsChannelResponse`

``` purescript
newtype GetSmsChannelResponse
  = GetSmsChannelResponse { "SMSChannelResponse" :: SMSChannelResponse }
```

##### Instances
``` purescript
Newtype GetSmsChannelResponse _
```

#### `ImportJobRequest`

``` purescript
newtype ImportJobRequest
  = ImportJobRequest { "DefineSegment" :: NullOrUndefined (Boolean), "ExternalId" :: NullOrUndefined (String), "Format" :: NullOrUndefined (Format), "RegisterEndpoints" :: NullOrUndefined (Boolean), "RoleArn" :: NullOrUndefined (String), "S3Url" :: NullOrUndefined (String), "SegmentId" :: NullOrUndefined (String), "SegmentName" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ImportJobRequest _
```

#### `ImportJobResource`

``` purescript
newtype ImportJobResource
  = ImportJobResource { "DefineSegment" :: NullOrUndefined (Boolean), "ExternalId" :: NullOrUndefined (String), "Format" :: NullOrUndefined (Format), "RegisterEndpoints" :: NullOrUndefined (Boolean), "RoleArn" :: NullOrUndefined (String), "S3Url" :: NullOrUndefined (String), "SegmentId" :: NullOrUndefined (String), "SegmentName" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ImportJobResource _
```

#### `ImportJobResponse`

``` purescript
newtype ImportJobResponse
  = ImportJobResponse { "ApplicationId" :: NullOrUndefined (String), "CompletedPieces" :: NullOrUndefined (Int), "CompletionDate" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Definition" :: NullOrUndefined (ImportJobResource), "FailedPieces" :: NullOrUndefined (Int), "Failures" :: NullOrUndefined (ListOf__string), "Id" :: NullOrUndefined (String), "JobStatus" :: NullOrUndefined (JobStatus), "TotalFailures" :: NullOrUndefined (Int), "TotalPieces" :: NullOrUndefined (Int), "TotalProcessed" :: NullOrUndefined (Int), "Type" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ImportJobResponse _
```

#### `ImportJobsResponse`

``` purescript
newtype ImportJobsResponse
  = ImportJobsResponse { "Item" :: NullOrUndefined (ListOfImportJobResponse), "NextToken" :: NullOrUndefined (String) }
```

Import job list.

##### Instances
``` purescript
Newtype ImportJobsResponse _
```

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

##### Instances
``` purescript
Newtype InternalServerErrorException _
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

#### `ListOfActivityResponse`

``` purescript
newtype ListOfActivityResponse
  = ListOfActivityResponse (Array ActivityResponse)
```

##### Instances
``` purescript
Newtype ListOfActivityResponse _
```

#### `ListOfApplicationResponse`

``` purescript
newtype ListOfApplicationResponse
  = ListOfApplicationResponse (Array ApplicationResponse)
```

##### Instances
``` purescript
Newtype ListOfApplicationResponse _
```

#### `ListOfCampaignResponse`

``` purescript
newtype ListOfCampaignResponse
  = ListOfCampaignResponse (Array CampaignResponse)
```

##### Instances
``` purescript
Newtype ListOfCampaignResponse _
```

#### `ListOfEndpointBatchItem`

``` purescript
newtype ListOfEndpointBatchItem
  = ListOfEndpointBatchItem (Array EndpointBatchItem)
```

##### Instances
``` purescript
Newtype ListOfEndpointBatchItem _
```

#### `ListOfImportJobResponse`

``` purescript
newtype ListOfImportJobResponse
  = ListOfImportJobResponse (Array ImportJobResponse)
```

##### Instances
``` purescript
Newtype ListOfImportJobResponse _
```

#### `ListOfSegmentResponse`

``` purescript
newtype ListOfSegmentResponse
  = ListOfSegmentResponse (Array SegmentResponse)
```

##### Instances
``` purescript
Newtype ListOfSegmentResponse _
```

#### `ListOfTreatmentResource`

``` purescript
newtype ListOfTreatmentResource
  = ListOfTreatmentResource (Array TreatmentResource)
```

##### Instances
``` purescript
Newtype ListOfTreatmentResource _
```

#### `ListOfWriteTreatmentResource`

``` purescript
newtype ListOfWriteTreatmentResource
  = ListOfWriteTreatmentResource (Array WriteTreatmentResource)
```

##### Instances
``` purescript
Newtype ListOfWriteTreatmentResource _
```

#### `ListOf__string`

``` purescript
newtype ListOf__string
  = ListOf__string (Array String)
```

##### Instances
``` purescript
Newtype ListOf__string _
```

#### `MapOfAddressConfiguration`

``` purescript
newtype MapOfAddressConfiguration
  = MapOfAddressConfiguration (Map String AddressConfiguration)
```

##### Instances
``` purescript
Newtype MapOfAddressConfiguration _
```

#### `MapOfAttributeDimension`

``` purescript
newtype MapOfAttributeDimension
  = MapOfAttributeDimension (Map String AttributeDimension)
```

##### Instances
``` purescript
Newtype MapOfAttributeDimension _
```

#### `MapOfEndpointMessageResult`

``` purescript
newtype MapOfEndpointMessageResult
  = MapOfEndpointMessageResult (Map String EndpointMessageResult)
```

##### Instances
``` purescript
Newtype MapOfEndpointMessageResult _
```

#### `MapOfEndpointSendConfiguration`

``` purescript
newtype MapOfEndpointSendConfiguration
  = MapOfEndpointSendConfiguration (Map String EndpointSendConfiguration)
```

##### Instances
``` purescript
Newtype MapOfEndpointSendConfiguration _
```

#### `MapOfListOf__string`

``` purescript
newtype MapOfListOf__string
  = MapOfListOf__string (Map String ListOf__string)
```

##### Instances
``` purescript
Newtype MapOfListOf__string _
```

#### `MapOfMapOfEndpointMessageResult`

``` purescript
newtype MapOfMapOfEndpointMessageResult
  = MapOfMapOfEndpointMessageResult (Map String MapOfEndpointMessageResult)
```

##### Instances
``` purescript
Newtype MapOfMapOfEndpointMessageResult _
```

#### `MapOfMessageResult`

``` purescript
newtype MapOfMessageResult
  = MapOfMessageResult (Map String MessageResult)
```

##### Instances
``` purescript
Newtype MapOfMessageResult _
```

#### `MapOf__double`

``` purescript
newtype MapOf__double
  = MapOf__double (Map String Number)
```

##### Instances
``` purescript
Newtype MapOf__double _
```

#### `MapOf__integer`

``` purescript
newtype MapOf__integer
  = MapOf__integer (Map String Int)
```

##### Instances
``` purescript
Newtype MapOf__integer _
```

#### `MapOf__string`

``` purescript
newtype MapOf__string
  = MapOf__string (Map String String)
```

##### Instances
``` purescript
Newtype MapOf__string _
```

#### `Message`

``` purescript
newtype Message
  = Message { "Action" :: NullOrUndefined (Action), "Body" :: NullOrUndefined (String), "ImageIconUrl" :: NullOrUndefined (String), "ImageSmallIconUrl" :: NullOrUndefined (String), "ImageUrl" :: NullOrUndefined (String), "JsonBody" :: NullOrUndefined (String), "MediaUrl" :: NullOrUndefined (String), "RawContent" :: NullOrUndefined (String), "SilentPush" :: NullOrUndefined (Boolean), "Title" :: NullOrUndefined (String), "Url" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype Message _
```

#### `MessageBody`

``` purescript
newtype MessageBody
  = MessageBody { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

##### Instances
``` purescript
Newtype MessageBody _
```

#### `MessageConfiguration`

``` purescript
newtype MessageConfiguration
  = MessageConfiguration { "ADMMessage" :: NullOrUndefined (Message), "APNSMessage" :: NullOrUndefined (Message), "BaiduMessage" :: NullOrUndefined (Message), "DefaultMessage" :: NullOrUndefined (Message), "EmailMessage" :: NullOrUndefined (CampaignEmailMessage), "GCMMessage" :: NullOrUndefined (Message), "SMSMessage" :: NullOrUndefined (CampaignSmsMessage) }
```

Message configuration for a campaign.

##### Instances
``` purescript
Newtype MessageConfiguration _
```

#### `MessageRequest`

``` purescript
newtype MessageRequest
  = MessageRequest { "Addresses" :: NullOrUndefined (MapOfAddressConfiguration), "Context" :: NullOrUndefined (MapOf__string), "Endpoints" :: NullOrUndefined (MapOfEndpointSendConfiguration), "MessageConfiguration" :: NullOrUndefined (DirectMessageConfiguration) }
```

Send message request.

##### Instances
``` purescript
Newtype MessageRequest _
```

#### `MessageResponse`

``` purescript
newtype MessageResponse
  = MessageResponse { "ApplicationId" :: NullOrUndefined (String), "EndpointResult" :: NullOrUndefined (MapOfEndpointMessageResult), "RequestId" :: NullOrUndefined (String), "Result" :: NullOrUndefined (MapOfMessageResult) }
```

Send message response.

##### Instances
``` purescript
Newtype MessageResponse _
```

#### `MessageResult`

``` purescript
newtype MessageResult
  = MessageResult { "DeliveryStatus" :: NullOrUndefined (DeliveryStatus), "StatusCode" :: NullOrUndefined (Int), "StatusMessage" :: NullOrUndefined (String), "UpdatedToken" :: NullOrUndefined (String) }
```

The result from sending a message to an address.

##### Instances
``` purescript
Newtype MessageResult _
```

#### `MessageType`

``` purescript
newtype MessageType
  = MessageType String
```

##### Instances
``` purescript
Newtype MessageType _
```

#### `MethodNotAllowedException`

``` purescript
newtype MethodNotAllowedException
  = MethodNotAllowedException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

##### Instances
``` purescript
Newtype MethodNotAllowedException _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `PutEventStreamRequest`

``` purescript
newtype PutEventStreamRequest
  = PutEventStreamRequest { "ApplicationId" :: String, "WriteEventStream" :: WriteEventStream }
```

##### Instances
``` purescript
Newtype PutEventStreamRequest _
```

#### `PutEventStreamResponse`

``` purescript
newtype PutEventStreamResponse
  = PutEventStreamResponse { "EventStream" :: EventStream }
```

##### Instances
``` purescript
Newtype PutEventStreamResponse _
```

#### `QuietTime`

``` purescript
newtype QuietTime
  = QuietTime { "End" :: NullOrUndefined (String), "Start" :: NullOrUndefined (String) }
```

Quiet Time

##### Instances
``` purescript
Newtype QuietTime _
```

#### `RecencyDimension`

``` purescript
newtype RecencyDimension
  = RecencyDimension { "Duration" :: NullOrUndefined (Duration), "RecencyType" :: NullOrUndefined (RecencyType) }
```

Define how a segment based on recency of use.

##### Instances
``` purescript
Newtype RecencyDimension _
```

#### `RecencyType`

``` purescript
newtype RecencyType
  = RecencyType String
```

##### Instances
``` purescript
Newtype RecencyType _
```

#### `SMSChannelRequest`

``` purescript
newtype SMSChannelRequest
  = SMSChannelRequest { "Enabled" :: NullOrUndefined (Boolean), "SenderId" :: NullOrUndefined (String), "ShortCode" :: NullOrUndefined (String) }
```

SMS Channel Request

##### Instances
``` purescript
Newtype SMSChannelRequest _
```

#### `SMSChannelResponse`

``` purescript
newtype SMSChannelResponse
  = SMSChannelResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "HasCredential" :: NullOrUndefined (Boolean), "Id" :: NullOrUndefined (String), "IsArchived" :: NullOrUndefined (Boolean), "LastModifiedBy" :: NullOrUndefined (String), "LastModifiedDate" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "SenderId" :: NullOrUndefined (String), "ShortCode" :: NullOrUndefined (String), "Version" :: NullOrUndefined (Int) }
```

SMS Channel Response.

##### Instances
``` purescript
Newtype SMSChannelResponse _
```

#### `SMSMessage`

``` purescript
newtype SMSMessage
  = SMSMessage { "Body" :: NullOrUndefined (String), "MessageType" :: NullOrUndefined (MessageType), "SenderId" :: NullOrUndefined (String), "Substitutions" :: NullOrUndefined (MapOfListOf__string) }
```

SMS Message.

##### Instances
``` purescript
Newtype SMSMessage _
```

#### `Schedule`

``` purescript
newtype Schedule
  = Schedule { "EndTime" :: NullOrUndefined (String), "Frequency" :: NullOrUndefined (Frequency), "IsLocalTime" :: NullOrUndefined (Boolean), "QuietTime" :: NullOrUndefined (QuietTime), "StartTime" :: NullOrUndefined (String), "Timezone" :: NullOrUndefined (String) }
```

Shcedule that defines when a campaign is run.

##### Instances
``` purescript
Newtype Schedule _
```

#### `SegmentBehaviors`

``` purescript
newtype SegmentBehaviors
  = SegmentBehaviors { "Recency" :: NullOrUndefined (RecencyDimension) }
```

Segment behavior dimensions

##### Instances
``` purescript
Newtype SegmentBehaviors _
```

#### `SegmentDemographics`

``` purescript
newtype SegmentDemographics
  = SegmentDemographics { "AppVersion" :: NullOrUndefined (SetDimension), "Channel" :: NullOrUndefined (SetDimension), "DeviceType" :: NullOrUndefined (SetDimension), "Make" :: NullOrUndefined (SetDimension), "Model" :: NullOrUndefined (SetDimension), "Platform" :: NullOrUndefined (SetDimension) }
```

Segment demographic dimensions

##### Instances
``` purescript
Newtype SegmentDemographics _
```

#### `SegmentDimensions`

``` purescript
newtype SegmentDimensions
  = SegmentDimensions { "Attributes" :: NullOrUndefined (MapOfAttributeDimension), "Behavior" :: NullOrUndefined (SegmentBehaviors), "Demographic" :: NullOrUndefined (SegmentDemographics), "Location" :: NullOrUndefined (SegmentLocation), "UserAttributes" :: NullOrUndefined (MapOfAttributeDimension) }
```

Segment dimensions

##### Instances
``` purescript
Newtype SegmentDimensions _
```

#### `SegmentImportResource`

``` purescript
newtype SegmentImportResource
  = SegmentImportResource { "ChannelCounts" :: NullOrUndefined (MapOf__integer), "ExternalId" :: NullOrUndefined (String), "Format" :: NullOrUndefined (Format), "RoleArn" :: NullOrUndefined (String), "S3Url" :: NullOrUndefined (String), "Size" :: NullOrUndefined (Int) }
```

Segment import definition.

##### Instances
``` purescript
Newtype SegmentImportResource _
```

#### `SegmentLocation`

``` purescript
newtype SegmentLocation
  = SegmentLocation { "Country" :: NullOrUndefined (SetDimension) }
```

Segment location dimensions

##### Instances
``` purescript
Newtype SegmentLocation _
```

#### `SegmentResponse`

``` purescript
newtype SegmentResponse
  = SegmentResponse { "ApplicationId" :: NullOrUndefined (String), "CreationDate" :: NullOrUndefined (String), "Dimensions" :: NullOrUndefined (SegmentDimensions), "Id" :: NullOrUndefined (String), "ImportDefinition" :: NullOrUndefined (SegmentImportResource), "LastModifiedDate" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SegmentType" :: NullOrUndefined (SegmentType), "Version" :: NullOrUndefined (Int) }
```

Segment definition.

##### Instances
``` purescript
Newtype SegmentResponse _
```

#### `SegmentType`

``` purescript
newtype SegmentType
  = SegmentType String
```

##### Instances
``` purescript
Newtype SegmentType _
```

#### `SegmentsResponse`

``` purescript
newtype SegmentsResponse
  = SegmentsResponse { "Item" :: NullOrUndefined (ListOfSegmentResponse), "NextToken" :: NullOrUndefined (String) }
```

Segments in your account.

##### Instances
``` purescript
Newtype SegmentsResponse _
```

#### `SendMessagesRequest`

``` purescript
newtype SendMessagesRequest
  = SendMessagesRequest { "ApplicationId" :: String, "MessageRequest" :: MessageRequest }
```

##### Instances
``` purescript
Newtype SendMessagesRequest _
```

#### `SendMessagesResponse`

``` purescript
newtype SendMessagesResponse
  = SendMessagesResponse { "MessageResponse" :: MessageResponse }
```

##### Instances
``` purescript
Newtype SendMessagesResponse _
```

#### `SendUsersMessageRequest`

``` purescript
newtype SendUsersMessageRequest
  = SendUsersMessageRequest { "Context" :: NullOrUndefined (MapOf__string), "MessageConfiguration" :: NullOrUndefined (DirectMessageConfiguration), "Users" :: NullOrUndefined (MapOfEndpointSendConfiguration) }
```

Send message request.

##### Instances
``` purescript
Newtype SendUsersMessageRequest _
```

#### `SendUsersMessageResponse`

``` purescript
newtype SendUsersMessageResponse
  = SendUsersMessageResponse { "ApplicationId" :: NullOrUndefined (String), "RequestId" :: NullOrUndefined (String), "Result" :: NullOrUndefined (MapOfMapOfEndpointMessageResult) }
```

User send message response.

##### Instances
``` purescript
Newtype SendUsersMessageResponse _
```

#### `SendUsersMessagesRequest`

``` purescript
newtype SendUsersMessagesRequest
  = SendUsersMessagesRequest { "ApplicationId" :: String, "SendUsersMessageRequest" :: SendUsersMessageRequest }
```

##### Instances
``` purescript
Newtype SendUsersMessagesRequest _
```

#### `SendUsersMessagesResponse`

``` purescript
newtype SendUsersMessagesResponse
  = SendUsersMessagesResponse { "SendUsersMessageResponse" :: SendUsersMessageResponse }
```

##### Instances
``` purescript
Newtype SendUsersMessagesResponse _
```

#### `SetDimension`

``` purescript
newtype SetDimension
  = SetDimension { "DimensionType" :: NullOrUndefined (DimensionType), "Values" :: NullOrUndefined (ListOf__string) }
```

Dimension specification of a segment.

##### Instances
``` purescript
Newtype SetDimension _
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String), "RequestID" :: NullOrUndefined (String) }
```

Simple message object.

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `TreatmentResource`

``` purescript
newtype TreatmentResource
  = TreatmentResource { "Id" :: NullOrUndefined (String), "MessageConfiguration" :: NullOrUndefined (MessageConfiguration), "Schedule" :: NullOrUndefined (Schedule), "SizePercent" :: NullOrUndefined (Int), "State" :: NullOrUndefined (CampaignState), "TreatmentDescription" :: NullOrUndefined (String), "TreatmentName" :: NullOrUndefined (String) }
```

Treatment resource

##### Instances
``` purescript
Newtype TreatmentResource _
```

#### `UpdateAdmChannelRequest`

``` purescript
newtype UpdateAdmChannelRequest
  = UpdateAdmChannelRequest { "ADMChannelRequest" :: ADMChannelRequest, "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype UpdateAdmChannelRequest _
```

#### `UpdateAdmChannelResponse`

``` purescript
newtype UpdateAdmChannelResponse
  = UpdateAdmChannelResponse { "ADMChannelResponse" :: ADMChannelResponse }
```

##### Instances
``` purescript
Newtype UpdateAdmChannelResponse _
```

#### `UpdateApnsChannelRequest`

``` purescript
newtype UpdateApnsChannelRequest
  = UpdateApnsChannelRequest { "APNSChannelRequest" :: APNSChannelRequest, "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype UpdateApnsChannelRequest _
```

#### `UpdateApnsChannelResponse`

``` purescript
newtype UpdateApnsChannelResponse
  = UpdateApnsChannelResponse { "APNSChannelResponse" :: APNSChannelResponse }
```

##### Instances
``` purescript
Newtype UpdateApnsChannelResponse _
```

#### `UpdateApnsSandboxChannelRequest`

``` purescript
newtype UpdateApnsSandboxChannelRequest
  = UpdateApnsSandboxChannelRequest { "APNSSandboxChannelRequest" :: APNSSandboxChannelRequest, "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype UpdateApnsSandboxChannelRequest _
```

#### `UpdateApnsSandboxChannelResponse`

``` purescript
newtype UpdateApnsSandboxChannelResponse
  = UpdateApnsSandboxChannelResponse { "APNSSandboxChannelResponse" :: APNSSandboxChannelResponse }
```

##### Instances
``` purescript
Newtype UpdateApnsSandboxChannelResponse _
```

#### `UpdateApnsVoipChannelRequest`

``` purescript
newtype UpdateApnsVoipChannelRequest
  = UpdateApnsVoipChannelRequest { "APNSVoipChannelRequest" :: APNSVoipChannelRequest, "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype UpdateApnsVoipChannelRequest _
```

#### `UpdateApnsVoipChannelResponse`

``` purescript
newtype UpdateApnsVoipChannelResponse
  = UpdateApnsVoipChannelResponse { "APNSVoipChannelResponse" :: APNSVoipChannelResponse }
```

##### Instances
``` purescript
Newtype UpdateApnsVoipChannelResponse _
```

#### `UpdateApnsVoipSandboxChannelRequest`

``` purescript
newtype UpdateApnsVoipSandboxChannelRequest
  = UpdateApnsVoipSandboxChannelRequest { "APNSVoipSandboxChannelRequest" :: APNSVoipSandboxChannelRequest, "ApplicationId" :: String }
```

##### Instances
``` purescript
Newtype UpdateApnsVoipSandboxChannelRequest _
```

#### `UpdateApnsVoipSandboxChannelResponse`

``` purescript
newtype UpdateApnsVoipSandboxChannelResponse
  = UpdateApnsVoipSandboxChannelResponse { "APNSVoipSandboxChannelResponse" :: APNSVoipSandboxChannelResponse }
```

##### Instances
``` purescript
Newtype UpdateApnsVoipSandboxChannelResponse _
```

#### `UpdateApplicationSettingsRequest`

``` purescript
newtype UpdateApplicationSettingsRequest
  = UpdateApplicationSettingsRequest { "ApplicationId" :: String, "WriteApplicationSettingsRequest" :: WriteApplicationSettingsRequest }
```

##### Instances
``` purescript
Newtype UpdateApplicationSettingsRequest _
```

#### `UpdateApplicationSettingsResponse`

``` purescript
newtype UpdateApplicationSettingsResponse
  = UpdateApplicationSettingsResponse { "ApplicationSettingsResource" :: ApplicationSettingsResource }
```

##### Instances
``` purescript
Newtype UpdateApplicationSettingsResponse _
```

#### `UpdateBaiduChannelRequest`

``` purescript
newtype UpdateBaiduChannelRequest
  = UpdateBaiduChannelRequest { "ApplicationId" :: String, "BaiduChannelRequest" :: BaiduChannelRequest }
```

##### Instances
``` purescript
Newtype UpdateBaiduChannelRequest _
```

#### `UpdateBaiduChannelResponse`

``` purescript
newtype UpdateBaiduChannelResponse
  = UpdateBaiduChannelResponse { "BaiduChannelResponse" :: BaiduChannelResponse }
```

##### Instances
``` purescript
Newtype UpdateBaiduChannelResponse _
```

#### `UpdateCampaignRequest`

``` purescript
newtype UpdateCampaignRequest
  = UpdateCampaignRequest { "ApplicationId" :: String, "CampaignId" :: String, "WriteCampaignRequest" :: WriteCampaignRequest }
```

##### Instances
``` purescript
Newtype UpdateCampaignRequest _
```

#### `UpdateCampaignResponse`

``` purescript
newtype UpdateCampaignResponse
  = UpdateCampaignResponse { "CampaignResponse" :: CampaignResponse }
```

##### Instances
``` purescript
Newtype UpdateCampaignResponse _
```

#### `UpdateEmailChannelRequest`

``` purescript
newtype UpdateEmailChannelRequest
  = UpdateEmailChannelRequest { "ApplicationId" :: String, "EmailChannelRequest" :: EmailChannelRequest }
```

##### Instances
``` purescript
Newtype UpdateEmailChannelRequest _
```

#### `UpdateEmailChannelResponse`

``` purescript
newtype UpdateEmailChannelResponse
  = UpdateEmailChannelResponse { "EmailChannelResponse" :: EmailChannelResponse }
```

##### Instances
``` purescript
Newtype UpdateEmailChannelResponse _
```

#### `UpdateEndpointRequest`

``` purescript
newtype UpdateEndpointRequest
  = UpdateEndpointRequest { "ApplicationId" :: String, "EndpointId" :: String, "EndpointRequest" :: EndpointRequest }
```

##### Instances
``` purescript
Newtype UpdateEndpointRequest _
```

#### `UpdateEndpointResponse`

``` purescript
newtype UpdateEndpointResponse
  = UpdateEndpointResponse { "MessageBody" :: MessageBody }
```

##### Instances
``` purescript
Newtype UpdateEndpointResponse _
```

#### `UpdateEndpointsBatchRequest`

``` purescript
newtype UpdateEndpointsBatchRequest
  = UpdateEndpointsBatchRequest { "ApplicationId" :: String, "EndpointBatchRequest" :: EndpointBatchRequest }
```

##### Instances
``` purescript
Newtype UpdateEndpointsBatchRequest _
```

#### `UpdateEndpointsBatchResponse`

``` purescript
newtype UpdateEndpointsBatchResponse
  = UpdateEndpointsBatchResponse { "MessageBody" :: MessageBody }
```

##### Instances
``` purescript
Newtype UpdateEndpointsBatchResponse _
```

#### `UpdateGcmChannelRequest`

``` purescript
newtype UpdateGcmChannelRequest
  = UpdateGcmChannelRequest { "ApplicationId" :: String, "GCMChannelRequest" :: GCMChannelRequest }
```

##### Instances
``` purescript
Newtype UpdateGcmChannelRequest _
```

#### `UpdateGcmChannelResponse`

``` purescript
newtype UpdateGcmChannelResponse
  = UpdateGcmChannelResponse { "GCMChannelResponse" :: GCMChannelResponse }
```

##### Instances
``` purescript
Newtype UpdateGcmChannelResponse _
```

#### `UpdateSegmentRequest`

``` purescript
newtype UpdateSegmentRequest
  = UpdateSegmentRequest { "ApplicationId" :: String, "SegmentId" :: String, "WriteSegmentRequest" :: WriteSegmentRequest }
```

##### Instances
``` purescript
Newtype UpdateSegmentRequest _
```

#### `UpdateSegmentResponse`

``` purescript
newtype UpdateSegmentResponse
  = UpdateSegmentResponse { "SegmentResponse" :: SegmentResponse }
```

##### Instances
``` purescript
Newtype UpdateSegmentResponse _
```

#### `UpdateSmsChannelRequest`

``` purescript
newtype UpdateSmsChannelRequest
  = UpdateSmsChannelRequest { "ApplicationId" :: String, "SMSChannelRequest" :: SMSChannelRequest }
```

##### Instances
``` purescript
Newtype UpdateSmsChannelRequest _
```

#### `UpdateSmsChannelResponse`

``` purescript
newtype UpdateSmsChannelResponse
  = UpdateSmsChannelResponse { "SMSChannelResponse" :: SMSChannelResponse }
```

##### Instances
``` purescript
Newtype UpdateSmsChannelResponse _
```

#### `WriteApplicationSettingsRequest`

``` purescript
newtype WriteApplicationSettingsRequest
  = WriteApplicationSettingsRequest { "Limits" :: NullOrUndefined (CampaignLimits), "QuietTime" :: NullOrUndefined (QuietTime) }
```

Creating application setting request

##### Instances
``` purescript
Newtype WriteApplicationSettingsRequest _
```

#### `WriteCampaignRequest`

``` purescript
newtype WriteCampaignRequest
  = WriteCampaignRequest { "AdditionalTreatments" :: NullOrUndefined (ListOfWriteTreatmentResource), "Description" :: NullOrUndefined (String), "HoldoutPercent" :: NullOrUndefined (Int), "IsPaused" :: NullOrUndefined (Boolean), "Limits" :: NullOrUndefined (CampaignLimits), "MessageConfiguration" :: NullOrUndefined (MessageConfiguration), "Name" :: NullOrUndefined (String), "Schedule" :: NullOrUndefined (Schedule), "SegmentId" :: NullOrUndefined (String), "SegmentVersion" :: NullOrUndefined (Int), "TreatmentDescription" :: NullOrUndefined (String), "TreatmentName" :: NullOrUndefined (String) }
```

Used to create a campaign.

##### Instances
``` purescript
Newtype WriteCampaignRequest _
```

#### `WriteEventStream`

``` purescript
newtype WriteEventStream
  = WriteEventStream { "DestinationStreamArn" :: NullOrUndefined (String), "RoleArn" :: NullOrUndefined (String) }
```

Request to save an EventStream.

##### Instances
``` purescript
Newtype WriteEventStream _
```

#### `WriteSegmentRequest`

``` purescript
newtype WriteSegmentRequest
  = WriteSegmentRequest { "Dimensions" :: NullOrUndefined (SegmentDimensions), "Name" :: NullOrUndefined (String) }
```

Segment definition.

##### Instances
``` purescript
Newtype WriteSegmentRequest _
```

#### `WriteTreatmentResource`

``` purescript
newtype WriteTreatmentResource
  = WriteTreatmentResource { "MessageConfiguration" :: NullOrUndefined (MessageConfiguration), "Schedule" :: NullOrUndefined (Schedule), "SizePercent" :: NullOrUndefined (Int), "TreatmentDescription" :: NullOrUndefined (String), "TreatmentName" :: NullOrUndefined (String) }
```

Used to create a campaign treatment.

##### Instances
``` purescript
Newtype WriteTreatmentResource _
```


