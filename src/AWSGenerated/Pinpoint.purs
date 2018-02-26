

module AWS.Pinpoint where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Pinpoint" :: String


-- | Creates or updates an app.
createApp :: forall eff. CreateAppRequest -> Aff (err :: AWS.RequestError | eff) CreateAppResponse
createApp = AWS.request serviceName "CreateApp" 


-- | Creates or updates a campaign.
createCampaign :: forall eff. CreateCampaignRequest -> Aff (err :: AWS.RequestError | eff) CreateCampaignResponse
createCampaign = AWS.request serviceName "CreateCampaign" 


-- | Creates or updates an import job.
createImportJob :: forall eff. CreateImportJobRequest -> Aff (err :: AWS.RequestError | eff) CreateImportJobResponse
createImportJob = AWS.request serviceName "CreateImportJob" 


-- | Used to create or update a segment.
createSegment :: forall eff. CreateSegmentRequest -> Aff (err :: AWS.RequestError | eff) CreateSegmentResponse
createSegment = AWS.request serviceName "CreateSegment" 


-- | Delete an ADM channel
deleteAdmChannel :: forall eff. DeleteAdmChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteAdmChannelResponse
deleteAdmChannel = AWS.request serviceName "DeleteAdmChannel" 


-- | Deletes the APNs channel for an app.
deleteApnsChannel :: forall eff. DeleteApnsChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteApnsChannelResponse
deleteApnsChannel = AWS.request serviceName "DeleteApnsChannel" 


-- | Delete an APNS sandbox channel
deleteApnsSandboxChannel :: forall eff. DeleteApnsSandboxChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteApnsSandboxChannelResponse
deleteApnsSandboxChannel = AWS.request serviceName "DeleteApnsSandboxChannel" 


-- | Delete an APNS VoIP channel
deleteApnsVoipChannel :: forall eff. DeleteApnsVoipChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteApnsVoipChannelResponse
deleteApnsVoipChannel = AWS.request serviceName "DeleteApnsVoipChannel" 


-- | Delete an APNS VoIP sandbox channel
deleteApnsVoipSandboxChannel :: forall eff. DeleteApnsVoipSandboxChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteApnsVoipSandboxChannelResponse
deleteApnsVoipSandboxChannel = AWS.request serviceName "DeleteApnsVoipSandboxChannel" 


-- | Deletes an app.
deleteApp :: forall eff. DeleteAppRequest -> Aff (err :: AWS.RequestError | eff) DeleteAppResponse
deleteApp = AWS.request serviceName "DeleteApp" 


-- | Delete a BAIDU GCM channel
deleteBaiduChannel :: forall eff. DeleteBaiduChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteBaiduChannelResponse
deleteBaiduChannel = AWS.request serviceName "DeleteBaiduChannel" 


-- | Deletes a campaign.
deleteCampaign :: forall eff. DeleteCampaignRequest -> Aff (err :: AWS.RequestError | eff) DeleteCampaignResponse
deleteCampaign = AWS.request serviceName "DeleteCampaign" 


-- | Delete an email channel
deleteEmailChannel :: forall eff. DeleteEmailChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteEmailChannelResponse
deleteEmailChannel = AWS.request serviceName "DeleteEmailChannel" 


-- | Deletes the event stream for an app.
deleteEventStream :: forall eff. DeleteEventStreamRequest -> Aff (err :: AWS.RequestError | eff) DeleteEventStreamResponse
deleteEventStream = AWS.request serviceName "DeleteEventStream" 


-- | Deletes the GCM channel for an app.
deleteGcmChannel :: forall eff. DeleteGcmChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteGcmChannelResponse
deleteGcmChannel = AWS.request serviceName "DeleteGcmChannel" 


-- | Deletes a segment.
deleteSegment :: forall eff. DeleteSegmentRequest -> Aff (err :: AWS.RequestError | eff) DeleteSegmentResponse
deleteSegment = AWS.request serviceName "DeleteSegment" 


-- | Delete an SMS channel
deleteSmsChannel :: forall eff. DeleteSmsChannelRequest -> Aff (err :: AWS.RequestError | eff) DeleteSmsChannelResponse
deleteSmsChannel = AWS.request serviceName "DeleteSmsChannel" 


-- | Get an ADM channel
getAdmChannel :: forall eff. GetAdmChannelRequest -> Aff (err :: AWS.RequestError | eff) GetAdmChannelResponse
getAdmChannel = AWS.request serviceName "GetAdmChannel" 


-- | Returns information about the APNs channel for an app.
getApnsChannel :: forall eff. GetApnsChannelRequest -> Aff (err :: AWS.RequestError | eff) GetApnsChannelResponse
getApnsChannel = AWS.request serviceName "GetApnsChannel" 


-- | Get an APNS sandbox channel
getApnsSandboxChannel :: forall eff. GetApnsSandboxChannelRequest -> Aff (err :: AWS.RequestError | eff) GetApnsSandboxChannelResponse
getApnsSandboxChannel = AWS.request serviceName "GetApnsSandboxChannel" 


-- | Get an APNS VoIP channel
getApnsVoipChannel :: forall eff. GetApnsVoipChannelRequest -> Aff (err :: AWS.RequestError | eff) GetApnsVoipChannelResponse
getApnsVoipChannel = AWS.request serviceName "GetApnsVoipChannel" 


-- | Get an APNS VoipSandbox channel
getApnsVoipSandboxChannel :: forall eff. GetApnsVoipSandboxChannelRequest -> Aff (err :: AWS.RequestError | eff) GetApnsVoipSandboxChannelResponse
getApnsVoipSandboxChannel = AWS.request serviceName "GetApnsVoipSandboxChannel" 


-- | Returns information about an app.
getApp :: forall eff. GetAppRequest -> Aff (err :: AWS.RequestError | eff) GetAppResponse
getApp = AWS.request serviceName "GetApp" 


-- | Used to request the settings for an app.
getApplicationSettings :: forall eff. GetApplicationSettingsRequest -> Aff (err :: AWS.RequestError | eff) GetApplicationSettingsResponse
getApplicationSettings = AWS.request serviceName "GetApplicationSettings" 


-- | Returns information about your apps.
getApps :: forall eff. GetAppsRequest -> Aff (err :: AWS.RequestError | eff) GetAppsResponse
getApps = AWS.request serviceName "GetApps" 


-- | Get a BAIDU GCM channel
getBaiduChannel :: forall eff. GetBaiduChannelRequest -> Aff (err :: AWS.RequestError | eff) GetBaiduChannelResponse
getBaiduChannel = AWS.request serviceName "GetBaiduChannel" 


-- | Returns information about a campaign.
getCampaign :: forall eff. GetCampaignRequest -> Aff (err :: AWS.RequestError | eff) GetCampaignResponse
getCampaign = AWS.request serviceName "GetCampaign" 


-- | Returns information about the activity performed by a campaign.
getCampaignActivities :: forall eff. GetCampaignActivitiesRequest -> Aff (err :: AWS.RequestError | eff) GetCampaignActivitiesResponse
getCampaignActivities = AWS.request serviceName "GetCampaignActivities" 


-- | Returns information about a specific version of a campaign.
getCampaignVersion :: forall eff. GetCampaignVersionRequest -> Aff (err :: AWS.RequestError | eff) GetCampaignVersionResponse
getCampaignVersion = AWS.request serviceName "GetCampaignVersion" 


-- | Returns information about your campaign versions.
getCampaignVersions :: forall eff. GetCampaignVersionsRequest -> Aff (err :: AWS.RequestError | eff) GetCampaignVersionsResponse
getCampaignVersions = AWS.request serviceName "GetCampaignVersions" 


-- | Returns information about your campaigns.
getCampaigns :: forall eff. GetCampaignsRequest -> Aff (err :: AWS.RequestError | eff) GetCampaignsResponse
getCampaigns = AWS.request serviceName "GetCampaigns" 


-- | Get an email channel
getEmailChannel :: forall eff. GetEmailChannelRequest -> Aff (err :: AWS.RequestError | eff) GetEmailChannelResponse
getEmailChannel = AWS.request serviceName "GetEmailChannel" 


-- | Returns information about an endpoint.
getEndpoint :: forall eff. GetEndpointRequest -> Aff (err :: AWS.RequestError | eff) GetEndpointResponse
getEndpoint = AWS.request serviceName "GetEndpoint" 


-- | Returns the event stream for an app.
getEventStream :: forall eff. GetEventStreamRequest -> Aff (err :: AWS.RequestError | eff) GetEventStreamResponse
getEventStream = AWS.request serviceName "GetEventStream" 


-- | Returns information about the GCM channel for an app.
getGcmChannel :: forall eff. GetGcmChannelRequest -> Aff (err :: AWS.RequestError | eff) GetGcmChannelResponse
getGcmChannel = AWS.request serviceName "GetGcmChannel" 


-- | Returns information about an import job.
getImportJob :: forall eff. GetImportJobRequest -> Aff (err :: AWS.RequestError | eff) GetImportJobResponse
getImportJob = AWS.request serviceName "GetImportJob" 


-- | Returns information about your import jobs.
getImportJobs :: forall eff. GetImportJobsRequest -> Aff (err :: AWS.RequestError | eff) GetImportJobsResponse
getImportJobs = AWS.request serviceName "GetImportJobs" 


-- | Returns information about a segment.
getSegment :: forall eff. GetSegmentRequest -> Aff (err :: AWS.RequestError | eff) GetSegmentResponse
getSegment = AWS.request serviceName "GetSegment" 


-- | Returns a list of import jobs for a specific segment.
getSegmentImportJobs :: forall eff. GetSegmentImportJobsRequest -> Aff (err :: AWS.RequestError | eff) GetSegmentImportJobsResponse
getSegmentImportJobs = AWS.request serviceName "GetSegmentImportJobs" 


-- | Returns information about a segment version.
getSegmentVersion :: forall eff. GetSegmentVersionRequest -> Aff (err :: AWS.RequestError | eff) GetSegmentVersionResponse
getSegmentVersion = AWS.request serviceName "GetSegmentVersion" 


-- | Returns information about your segment versions.
getSegmentVersions :: forall eff. GetSegmentVersionsRequest -> Aff (err :: AWS.RequestError | eff) GetSegmentVersionsResponse
getSegmentVersions = AWS.request serviceName "GetSegmentVersions" 


-- | Used to get information about your segments.
getSegments :: forall eff. GetSegmentsRequest -> Aff (err :: AWS.RequestError | eff) GetSegmentsResponse
getSegments = AWS.request serviceName "GetSegments" 


-- | Get an SMS channel
getSmsChannel :: forall eff. GetSmsChannelRequest -> Aff (err :: AWS.RequestError | eff) GetSmsChannelResponse
getSmsChannel = AWS.request serviceName "GetSmsChannel" 


-- | Use to create or update the event stream for an app.
putEventStream :: forall eff. PutEventStreamRequest -> Aff (err :: AWS.RequestError | eff) PutEventStreamResponse
putEventStream = AWS.request serviceName "PutEventStream" 


-- | Send a batch of messages
sendMessages :: forall eff. SendMessagesRequest -> Aff (err :: AWS.RequestError | eff) SendMessagesResponse
sendMessages = AWS.request serviceName "SendMessages" 


-- | Send a batch of messages to users
sendUsersMessages :: forall eff. SendUsersMessagesRequest -> Aff (err :: AWS.RequestError | eff) SendUsersMessagesResponse
sendUsersMessages = AWS.request serviceName "SendUsersMessages" 


-- | Update an ADM channel
updateAdmChannel :: forall eff. UpdateAdmChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateAdmChannelResponse
updateAdmChannel = AWS.request serviceName "UpdateAdmChannel" 


-- | Use to update the APNs channel for an app.
updateApnsChannel :: forall eff. UpdateApnsChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateApnsChannelResponse
updateApnsChannel = AWS.request serviceName "UpdateApnsChannel" 


-- | Update an APNS sandbox channel
updateApnsSandboxChannel :: forall eff. UpdateApnsSandboxChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateApnsSandboxChannelResponse
updateApnsSandboxChannel = AWS.request serviceName "UpdateApnsSandboxChannel" 


-- | Update an APNS VoIP channel
updateApnsVoipChannel :: forall eff. UpdateApnsVoipChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateApnsVoipChannelResponse
updateApnsVoipChannel = AWS.request serviceName "UpdateApnsVoipChannel" 


-- | Update an APNS VoIP sandbox channel
updateApnsVoipSandboxChannel :: forall eff. UpdateApnsVoipSandboxChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateApnsVoipSandboxChannelResponse
updateApnsVoipSandboxChannel = AWS.request serviceName "UpdateApnsVoipSandboxChannel" 


-- | Used to update the settings for an app.
updateApplicationSettings :: forall eff. UpdateApplicationSettingsRequest -> Aff (err :: AWS.RequestError | eff) UpdateApplicationSettingsResponse
updateApplicationSettings = AWS.request serviceName "UpdateApplicationSettings" 


-- | Update a BAIDU GCM channel
updateBaiduChannel :: forall eff. UpdateBaiduChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateBaiduChannelResponse
updateBaiduChannel = AWS.request serviceName "UpdateBaiduChannel" 


-- | Use to update a campaign.
updateCampaign :: forall eff. UpdateCampaignRequest -> Aff (err :: AWS.RequestError | eff) UpdateCampaignResponse
updateCampaign = AWS.request serviceName "UpdateCampaign" 


-- | Update an email channel
updateEmailChannel :: forall eff. UpdateEmailChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateEmailChannelResponse
updateEmailChannel = AWS.request serviceName "UpdateEmailChannel" 


-- | Use to update an endpoint.
updateEndpoint :: forall eff. UpdateEndpointRequest -> Aff (err :: AWS.RequestError | eff) UpdateEndpointResponse
updateEndpoint = AWS.request serviceName "UpdateEndpoint" 


-- | Use to update a batch of endpoints.
updateEndpointsBatch :: forall eff. UpdateEndpointsBatchRequest -> Aff (err :: AWS.RequestError | eff) UpdateEndpointsBatchResponse
updateEndpointsBatch = AWS.request serviceName "UpdateEndpointsBatch" 


-- | Use to update the GCM channel for an app.
updateGcmChannel :: forall eff. UpdateGcmChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateGcmChannelResponse
updateGcmChannel = AWS.request serviceName "UpdateGcmChannel" 


-- | Use to update a segment.
updateSegment :: forall eff. UpdateSegmentRequest -> Aff (err :: AWS.RequestError | eff) UpdateSegmentResponse
updateSegment = AWS.request serviceName "UpdateSegment" 


-- | Update an SMS channel
updateSmsChannel :: forall eff. UpdateSmsChannelRequest -> Aff (err :: AWS.RequestError | eff) UpdateSmsChannelResponse
updateSmsChannel = AWS.request serviceName "UpdateSmsChannel" 


-- | Amazon Device Messaging channel definition.
newtype ADMChannelRequest = ADMChannelRequest 
  { "ClientId" :: NullOrUndefined (String)
  , "ClientSecret" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  }


-- | Amazon Device Messaging channel definition.
newtype ADMChannelResponse = ADMChannelResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


-- | ADM Message.
newtype ADMMessage = ADMMessage 
  { "Action" :: NullOrUndefined (Action)
  , "Body" :: NullOrUndefined (String)
  , "ConsolidationKey" :: NullOrUndefined (String)
  , "Data" :: NullOrUndefined (MapOf__string)
  , "ExpiresAfter" :: NullOrUndefined (String)
  , "IconReference" :: NullOrUndefined (String)
  , "ImageIconUrl" :: NullOrUndefined (String)
  , "ImageUrl" :: NullOrUndefined (String)
  , "MD5" :: NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined (Boolean)
  , "SmallImageIconUrl" :: NullOrUndefined (String)
  , "Sound" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  , "Title" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  }


-- | Apple Push Notification Service channel definition.
newtype APNSChannelRequest = APNSChannelRequest 
  { "BundleId" :: NullOrUndefined (String)
  , "Certificate" :: NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "PrivateKey" :: NullOrUndefined (String)
  , "TeamId" :: NullOrUndefined (String)
  , "TokenKey" :: NullOrUndefined (String)
  , "TokenKeyId" :: NullOrUndefined (String)
  }


-- | Apple Distribution Push Notification Service channel definition.
newtype APNSChannelResponse = APNSChannelResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined (Boolean)
  , "HasTokenKey" :: NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


-- | APNS Message.
newtype APNSMessage = APNSMessage 
  { "Action" :: NullOrUndefined (Action)
  , "Badge" :: NullOrUndefined (Int)
  , "Body" :: NullOrUndefined (String)
  , "Category" :: NullOrUndefined (String)
  , "CollapseId" :: NullOrUndefined (String)
  , "Data" :: NullOrUndefined (MapOf__string)
  , "MediaUrl" :: NullOrUndefined (String)
  , "PreferredAuthenticationMethod" :: NullOrUndefined (String)
  , "Priority" :: NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined (Boolean)
  , "Sound" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  , "ThreadId" :: NullOrUndefined (String)
  , "TimeToLive" :: NullOrUndefined (Int)
  , "Title" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  }


-- | Apple Development Push Notification Service channel definition.
newtype APNSSandboxChannelRequest = APNSSandboxChannelRequest 
  { "BundleId" :: NullOrUndefined (String)
  , "Certificate" :: NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "PrivateKey" :: NullOrUndefined (String)
  , "TeamId" :: NullOrUndefined (String)
  , "TokenKey" :: NullOrUndefined (String)
  , "TokenKeyId" :: NullOrUndefined (String)
  }


-- | Apple Development Push Notification Service channel definition.
newtype APNSSandboxChannelResponse = APNSSandboxChannelResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined (Boolean)
  , "HasTokenKey" :: NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


-- | Apple VoIP Push Notification Service channel definition.
newtype APNSVoipChannelRequest = APNSVoipChannelRequest 
  { "BundleId" :: NullOrUndefined (String)
  , "Certificate" :: NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "PrivateKey" :: NullOrUndefined (String)
  , "TeamId" :: NullOrUndefined (String)
  , "TokenKey" :: NullOrUndefined (String)
  , "TokenKeyId" :: NullOrUndefined (String)
  }


-- | Apple VoIP Push Notification Service channel definition.
newtype APNSVoipChannelResponse = APNSVoipChannelResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined (Boolean)
  , "HasTokenKey" :: NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


-- | Apple VoIP Developer Push Notification Service channel definition.
newtype APNSVoipSandboxChannelRequest = APNSVoipSandboxChannelRequest 
  { "BundleId" :: NullOrUndefined (String)
  , "Certificate" :: NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "PrivateKey" :: NullOrUndefined (String)
  , "TeamId" :: NullOrUndefined (String)
  , "TokenKey" :: NullOrUndefined (String)
  , "TokenKeyId" :: NullOrUndefined (String)
  }


-- | Apple VoIP Developer Push Notification Service channel definition.
newtype APNSVoipSandboxChannelResponse = APNSVoipSandboxChannelResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "DefaultAuthenticationMethod" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined (Boolean)
  , "HasTokenKey" :: NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


newtype Action = Action String


-- | Activities for campaign.
newtype ActivitiesResponse = ActivitiesResponse 
  { "Item" :: NullOrUndefined (ListOfActivityResponse)
  }


-- | Activity definition
newtype ActivityResponse = ActivityResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CampaignId" :: NullOrUndefined (String)
  , "End" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Result" :: NullOrUndefined (String)
  , "ScheduledStart" :: NullOrUndefined (String)
  , "Start" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (String)
  , "SuccessfulEndpointCount" :: NullOrUndefined (Int)
  , "TimezonesCompletedCount" :: NullOrUndefined (Int)
  , "TimezonesTotalCount" :: NullOrUndefined (Int)
  , "TotalEndpointCount" :: NullOrUndefined (Int)
  , "TreatmentId" :: NullOrUndefined (String)
  }


-- | Address configuration.
newtype AddressConfiguration = AddressConfiguration 
  { "BodyOverride" :: NullOrUndefined (String)
  , "ChannelType" :: NullOrUndefined (ChannelType)
  , "Context" :: NullOrUndefined (MapOf__string)
  , "RawContent" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  , "TitleOverride" :: NullOrUndefined (String)
  }


-- | Application Response.
newtype ApplicationResponse = ApplicationResponse 
  { "Id" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }


-- | Application settings.
newtype ApplicationSettingsResource = ApplicationSettingsResource 
  { "ApplicationId" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Limits" :: NullOrUndefined (CampaignLimits)
  , "QuietTime" :: NullOrUndefined (QuietTime)
  }


-- | Get Applications Result.
newtype ApplicationsResponse = ApplicationsResponse 
  { "Item" :: NullOrUndefined (ListOfApplicationResponse)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Custom attibute dimension
newtype AttributeDimension = AttributeDimension 
  { "AttributeType" :: NullOrUndefined (AttributeType)
  , "Values" :: NullOrUndefined (ListOf__string)
  }


newtype AttributeType = AttributeType String


-- | Simple message object.
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }


-- | Baidu Cloud Push credentials
newtype BaiduChannelRequest = BaiduChannelRequest 
  { "ApiKey" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "SecretKey" :: NullOrUndefined (String)
  }


-- | Baidu Cloud Messaging channel definition
newtype BaiduChannelResponse = BaiduChannelResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "Credential" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


-- | Baidu Message.
newtype BaiduMessage = BaiduMessage 
  { "Action" :: NullOrUndefined (Action)
  , "Body" :: NullOrUndefined (String)
  , "Data" :: NullOrUndefined (MapOf__string)
  , "IconReference" :: NullOrUndefined (String)
  , "ImageIconUrl" :: NullOrUndefined (String)
  , "ImageUrl" :: NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined (Boolean)
  , "SmallImageIconUrl" :: NullOrUndefined (String)
  , "Sound" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  , "Title" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  }


-- | The email message configuration.
newtype CampaignEmailMessage = CampaignEmailMessage 
  { "Body" :: NullOrUndefined (String)
  , "FromAddress" :: NullOrUndefined (String)
  , "HtmlBody" :: NullOrUndefined (String)
  , "Title" :: NullOrUndefined (String)
  }


-- | Campaign Limits are used to limit the number of messages that can be sent to a user.
newtype CampaignLimits = CampaignLimits 
  { "Daily" :: NullOrUndefined (Int)
  , "MaximumDuration" :: NullOrUndefined (Int)
  , "MessagesPerSecond" :: NullOrUndefined (Int)
  , "Total" :: NullOrUndefined (Int)
  }


-- | Campaign definition
newtype CampaignResponse = CampaignResponse 
  { "AdditionalTreatments" :: NullOrUndefined (ListOfTreatmentResource)
  , "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "DefaultState" :: NullOrUndefined (CampaignState)
  , "Description" :: NullOrUndefined (String)
  , "HoldoutPercent" :: NullOrUndefined (Int)
  , "Id" :: NullOrUndefined (String)
  , "IsPaused" :: NullOrUndefined (Boolean)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Limits" :: NullOrUndefined (CampaignLimits)
  , "MessageConfiguration" :: NullOrUndefined (MessageConfiguration)
  , "Name" :: NullOrUndefined (String)
  , "Schedule" :: NullOrUndefined (Schedule)
  , "SegmentId" :: NullOrUndefined (String)
  , "SegmentVersion" :: NullOrUndefined (Int)
  , "State" :: NullOrUndefined (CampaignState)
  , "TreatmentDescription" :: NullOrUndefined (String)
  , "TreatmentName" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


-- | SMS message configuration.
newtype CampaignSmsMessage = CampaignSmsMessage 
  { "Body" :: NullOrUndefined (String)
  , "MessageType" :: NullOrUndefined (MessageType)
  , "SenderId" :: NullOrUndefined (String)
  }


-- | State of the Campaign
newtype CampaignState = CampaignState 
  { "CampaignStatus" :: NullOrUndefined (CampaignStatus)
  }


newtype CampaignStatus = CampaignStatus String


-- | List of available campaigns.
newtype CampaignsResponse = CampaignsResponse 
  { "Item" :: NullOrUndefined (ListOfCampaignResponse)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ChannelType = ChannelType String


newtype CreateAppRequest = CreateAppRequest 
  { "CreateApplicationRequest" :: (CreateApplicationRequest)
  }


newtype CreateAppResponse = CreateAppResponse 
  { "ApplicationResponse" :: (ApplicationResponse)
  }


-- | Application Request.
newtype CreateApplicationRequest = CreateApplicationRequest 
  { "Name" :: NullOrUndefined (String)
  }


newtype CreateCampaignRequest = CreateCampaignRequest 
  { "ApplicationId" :: (String)
  , "WriteCampaignRequest" :: (WriteCampaignRequest)
  }


newtype CreateCampaignResponse = CreateCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }


newtype CreateImportJobRequest = CreateImportJobRequest 
  { "ApplicationId" :: (String)
  , "ImportJobRequest" :: (ImportJobRequest)
  }


newtype CreateImportJobResponse = CreateImportJobResponse 
  { "ImportJobResponse" :: (ImportJobResponse)
  }


newtype CreateSegmentRequest = CreateSegmentRequest 
  { "ApplicationId" :: (String)
  , "WriteSegmentRequest" :: (WriteSegmentRequest)
  }


newtype CreateSegmentResponse = CreateSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }


-- | Default Message across push notification, email, and sms.
newtype DefaultMessage = DefaultMessage 
  { "Body" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  }


-- | Default Push Notification Message.
newtype DefaultPushNotificationMessage = DefaultPushNotificationMessage 
  { "Action" :: NullOrUndefined (Action)
  , "Body" :: NullOrUndefined (String)
  , "Data" :: NullOrUndefined (MapOf__string)
  , "SilentPush" :: NullOrUndefined (Boolean)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  , "Title" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  }


newtype DeleteAdmChannelRequest = DeleteAdmChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteAdmChannelResponse = DeleteAdmChannelResponse 
  { "ADMChannelResponse" :: (ADMChannelResponse)
  }


newtype DeleteApnsChannelRequest = DeleteApnsChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteApnsChannelResponse = DeleteApnsChannelResponse 
  { "APNSChannelResponse" :: (APNSChannelResponse)
  }


newtype DeleteApnsSandboxChannelRequest = DeleteApnsSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteApnsSandboxChannelResponse = DeleteApnsSandboxChannelResponse 
  { "APNSSandboxChannelResponse" :: (APNSSandboxChannelResponse)
  }


newtype DeleteApnsVoipChannelRequest = DeleteApnsVoipChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteApnsVoipChannelResponse = DeleteApnsVoipChannelResponse 
  { "APNSVoipChannelResponse" :: (APNSVoipChannelResponse)
  }


newtype DeleteApnsVoipSandboxChannelRequest = DeleteApnsVoipSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteApnsVoipSandboxChannelResponse = DeleteApnsVoipSandboxChannelResponse 
  { "APNSVoipSandboxChannelResponse" :: (APNSVoipSandboxChannelResponse)
  }


newtype DeleteAppRequest = DeleteAppRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteAppResponse = DeleteAppResponse 
  { "ApplicationResponse" :: (ApplicationResponse)
  }


newtype DeleteBaiduChannelRequest = DeleteBaiduChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteBaiduChannelResponse = DeleteBaiduChannelResponse 
  { "BaiduChannelResponse" :: (BaiduChannelResponse)
  }


newtype DeleteCampaignRequest = DeleteCampaignRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  }


newtype DeleteCampaignResponse = DeleteCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }


newtype DeleteEmailChannelRequest = DeleteEmailChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteEmailChannelResponse = DeleteEmailChannelResponse 
  { "EmailChannelResponse" :: (EmailChannelResponse)
  }


-- | DeleteEventStream Request
newtype DeleteEventStreamRequest = DeleteEventStreamRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteEventStreamResponse = DeleteEventStreamResponse 
  { "EventStream" :: (EventStream)
  }


newtype DeleteGcmChannelRequest = DeleteGcmChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteGcmChannelResponse = DeleteGcmChannelResponse 
  { "GCMChannelResponse" :: (GCMChannelResponse)
  }


newtype DeleteSegmentRequest = DeleteSegmentRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  }


newtype DeleteSegmentResponse = DeleteSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }


newtype DeleteSmsChannelRequest = DeleteSmsChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype DeleteSmsChannelResponse = DeleteSmsChannelResponse 
  { "SMSChannelResponse" :: (SMSChannelResponse)
  }


newtype DeliveryStatus = DeliveryStatus String


newtype DimensionType = DimensionType String


-- | The message configuration.
newtype DirectMessageConfiguration = DirectMessageConfiguration 
  { "ADMMessage" :: NullOrUndefined (ADMMessage)
  , "APNSMessage" :: NullOrUndefined (APNSMessage)
  , "BaiduMessage" :: NullOrUndefined (BaiduMessage)
  , "DefaultMessage" :: NullOrUndefined (DefaultMessage)
  , "DefaultPushNotificationMessage" :: NullOrUndefined (DefaultPushNotificationMessage)
  , "GCMMessage" :: NullOrUndefined (GCMMessage)
  , "SMSMessage" :: NullOrUndefined (SMSMessage)
  }


newtype Duration = Duration String


-- | Email Channel Request
newtype EmailChannelRequest = EmailChannelRequest 
  { "Enabled" :: NullOrUndefined (Boolean)
  , "FromAddress" :: NullOrUndefined (String)
  , "Identity" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }


-- | Email Channel Response.
newtype EmailChannelResponse = EmailChannelResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "FromAddress" :: NullOrUndefined (String)
  , "HasCredential" :: NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined (String)
  , "Identity" :: NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


-- | Endpoint update request
newtype EndpointBatchItem = EndpointBatchItem 
  { "Address" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (MapOfListOf__string)
  , "ChannelType" :: NullOrUndefined (ChannelType)
  , "Demographic" :: NullOrUndefined (EndpointDemographic)
  , "EffectiveDate" :: NullOrUndefined (String)
  , "EndpointStatus" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Location" :: NullOrUndefined (EndpointLocation)
  , "Metrics" :: NullOrUndefined (MapOf__double)
  , "OptOut" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "User" :: NullOrUndefined (EndpointUser)
  }


-- | Endpoint batch update request.
newtype EndpointBatchRequest = EndpointBatchRequest 
  { "Item" :: NullOrUndefined (ListOfEndpointBatchItem)
  }


-- | Endpoint demographic data
newtype EndpointDemographic = EndpointDemographic 
  { "AppVersion" :: NullOrUndefined (String)
  , "Locale" :: NullOrUndefined (String)
  , "Make" :: NullOrUndefined (String)
  , "Model" :: NullOrUndefined (String)
  , "ModelVersion" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "PlatformVersion" :: NullOrUndefined (String)
  , "Timezone" :: NullOrUndefined (String)
  }


-- | Endpoint location data
newtype EndpointLocation = EndpointLocation 
  { "City" :: NullOrUndefined (String)
  , "Country" :: NullOrUndefined (String)
  , "Latitude" :: NullOrUndefined (Number)
  , "Longitude" :: NullOrUndefined (Number)
  , "PostalCode" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  }


-- | The result from sending a message to an endpoint.
newtype EndpointMessageResult = EndpointMessageResult 
  { "Address" :: NullOrUndefined (String)
  , "DeliveryStatus" :: NullOrUndefined (DeliveryStatus)
  , "StatusCode" :: NullOrUndefined (Int)
  , "StatusMessage" :: NullOrUndefined (String)
  , "UpdatedToken" :: NullOrUndefined (String)
  }


-- | Endpoint update request
newtype EndpointRequest = EndpointRequest 
  { "Address" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (MapOfListOf__string)
  , "ChannelType" :: NullOrUndefined (ChannelType)
  , "Demographic" :: NullOrUndefined (EndpointDemographic)
  , "EffectiveDate" :: NullOrUndefined (String)
  , "EndpointStatus" :: NullOrUndefined (String)
  , "Location" :: NullOrUndefined (EndpointLocation)
  , "Metrics" :: NullOrUndefined (MapOf__double)
  , "OptOut" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "User" :: NullOrUndefined (EndpointUser)
  }


-- | Endpoint response
newtype EndpointResponse = EndpointResponse 
  { "Address" :: NullOrUndefined (String)
  , "ApplicationId" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (MapOfListOf__string)
  , "ChannelType" :: NullOrUndefined (ChannelType)
  , "CohortId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "Demographic" :: NullOrUndefined (EndpointDemographic)
  , "EffectiveDate" :: NullOrUndefined (String)
  , "EndpointStatus" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Location" :: NullOrUndefined (EndpointLocation)
  , "Metrics" :: NullOrUndefined (MapOf__double)
  , "OptOut" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "User" :: NullOrUndefined (EndpointUser)
  }


-- | Endpoint send configuration.
newtype EndpointSendConfiguration = EndpointSendConfiguration 
  { "BodyOverride" :: NullOrUndefined (String)
  , "Context" :: NullOrUndefined (MapOf__string)
  , "RawContent" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  , "TitleOverride" :: NullOrUndefined (String)
  }


-- | Endpoint user specific custom userAttributes
newtype EndpointUser = EndpointUser 
  { "UserAttributes" :: NullOrUndefined (MapOfListOf__string)
  , "UserId" :: NullOrUndefined (String)
  }


-- | Model for an event publishing subscription export.
newtype EventStream = EventStream 
  { "ApplicationId" :: NullOrUndefined (String)
  , "DestinationStreamArn" :: NullOrUndefined (String)
  , "ExternalId" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "LastUpdatedBy" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }


-- | Simple message object.
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }


newtype Format = Format String


newtype Frequency = Frequency String


-- | Google Cloud Messaging credentials
newtype GCMChannelRequest = GCMChannelRequest 
  { "ApiKey" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  }


-- | Google Cloud Messaging channel definition
newtype GCMChannelResponse = GCMChannelResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "Credential" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


-- | GCM Message.
newtype GCMMessage = GCMMessage 
  { "Action" :: NullOrUndefined (Action)
  , "Body" :: NullOrUndefined (String)
  , "CollapseKey" :: NullOrUndefined (String)
  , "Data" :: NullOrUndefined (MapOf__string)
  , "IconReference" :: NullOrUndefined (String)
  , "ImageIconUrl" :: NullOrUndefined (String)
  , "ImageUrl" :: NullOrUndefined (String)
  , "Priority" :: NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined (String)
  , "RestrictedPackageName" :: NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined (Boolean)
  , "SmallImageIconUrl" :: NullOrUndefined (String)
  , "Sound" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  , "TimeToLive" :: NullOrUndefined (Int)
  , "Title" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  }


newtype GetAdmChannelRequest = GetAdmChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype GetAdmChannelResponse = GetAdmChannelResponse 
  { "ADMChannelResponse" :: (ADMChannelResponse)
  }


newtype GetApnsChannelRequest = GetApnsChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype GetApnsChannelResponse = GetApnsChannelResponse 
  { "APNSChannelResponse" :: (APNSChannelResponse)
  }


newtype GetApnsSandboxChannelRequest = GetApnsSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype GetApnsSandboxChannelResponse = GetApnsSandboxChannelResponse 
  { "APNSSandboxChannelResponse" :: (APNSSandboxChannelResponse)
  }


newtype GetApnsVoipChannelRequest = GetApnsVoipChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype GetApnsVoipChannelResponse = GetApnsVoipChannelResponse 
  { "APNSVoipChannelResponse" :: (APNSVoipChannelResponse)
  }


newtype GetApnsVoipSandboxChannelRequest = GetApnsVoipSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype GetApnsVoipSandboxChannelResponse = GetApnsVoipSandboxChannelResponse 
  { "APNSVoipSandboxChannelResponse" :: (APNSVoipSandboxChannelResponse)
  }


newtype GetAppRequest = GetAppRequest 
  { "ApplicationId" :: (String)
  }


newtype GetAppResponse = GetAppResponse 
  { "ApplicationResponse" :: (ApplicationResponse)
  }


newtype GetApplicationSettingsRequest = GetApplicationSettingsRequest 
  { "ApplicationId" :: (String)
  }


newtype GetApplicationSettingsResponse = GetApplicationSettingsResponse 
  { "ApplicationSettingsResource" :: (ApplicationSettingsResource)
  }


newtype GetAppsRequest = GetAppsRequest 
  { "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }


newtype GetAppsResponse = GetAppsResponse 
  { "ApplicationsResponse" :: (ApplicationsResponse)
  }


newtype GetBaiduChannelRequest = GetBaiduChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype GetBaiduChannelResponse = GetBaiduChannelResponse 
  { "BaiduChannelResponse" :: (BaiduChannelResponse)
  }


newtype GetCampaignActivitiesRequest = GetCampaignActivitiesRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }


newtype GetCampaignActivitiesResponse = GetCampaignActivitiesResponse 
  { "ActivitiesResponse" :: (ActivitiesResponse)
  }


newtype GetCampaignRequest = GetCampaignRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  }


newtype GetCampaignResponse = GetCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }


newtype GetCampaignVersionRequest = GetCampaignVersionRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "Version" :: (String)
  }


newtype GetCampaignVersionResponse = GetCampaignVersionResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }


newtype GetCampaignVersionsRequest = GetCampaignVersionsRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }


newtype GetCampaignVersionsResponse = GetCampaignVersionsResponse 
  { "CampaignsResponse" :: (CampaignsResponse)
  }


newtype GetCampaignsRequest = GetCampaignsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }


newtype GetCampaignsResponse = GetCampaignsResponse 
  { "CampaignsResponse" :: (CampaignsResponse)
  }


newtype GetEmailChannelRequest = GetEmailChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype GetEmailChannelResponse = GetEmailChannelResponse 
  { "EmailChannelResponse" :: (EmailChannelResponse)
  }


newtype GetEndpointRequest = GetEndpointRequest 
  { "ApplicationId" :: (String)
  , "EndpointId" :: (String)
  }


newtype GetEndpointResponse = GetEndpointResponse 
  { "EndpointResponse" :: (EndpointResponse)
  }


-- | GetEventStreamRequest
newtype GetEventStreamRequest = GetEventStreamRequest 
  { "ApplicationId" :: (String)
  }


newtype GetEventStreamResponse = GetEventStreamResponse 
  { "EventStream" :: (EventStream)
  }


newtype GetGcmChannelRequest = GetGcmChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype GetGcmChannelResponse = GetGcmChannelResponse 
  { "GCMChannelResponse" :: (GCMChannelResponse)
  }


newtype GetImportJobRequest = GetImportJobRequest 
  { "ApplicationId" :: (String)
  , "JobId" :: (String)
  }


newtype GetImportJobResponse = GetImportJobResponse 
  { "ImportJobResponse" :: (ImportJobResponse)
  }


newtype GetImportJobsRequest = GetImportJobsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }


newtype GetImportJobsResponse = GetImportJobsResponse 
  { "ImportJobsResponse" :: (ImportJobsResponse)
  }


newtype GetSegmentImportJobsRequest = GetSegmentImportJobsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "SegmentId" :: (String)
  , "Token" :: NullOrUndefined (String)
  }


newtype GetSegmentImportJobsResponse = GetSegmentImportJobsResponse 
  { "ImportJobsResponse" :: (ImportJobsResponse)
  }


newtype GetSegmentRequest = GetSegmentRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  }


newtype GetSegmentResponse = GetSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }


newtype GetSegmentVersionRequest = GetSegmentVersionRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  , "Version" :: (String)
  }


newtype GetSegmentVersionResponse = GetSegmentVersionResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }


newtype GetSegmentVersionsRequest = GetSegmentVersionsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "SegmentId" :: (String)
  , "Token" :: NullOrUndefined (String)
  }


newtype GetSegmentVersionsResponse = GetSegmentVersionsResponse 
  { "SegmentsResponse" :: (SegmentsResponse)
  }


newtype GetSegmentsRequest = GetSegmentsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }


newtype GetSegmentsResponse = GetSegmentsResponse 
  { "SegmentsResponse" :: (SegmentsResponse)
  }


newtype GetSmsChannelRequest = GetSmsChannelRequest 
  { "ApplicationId" :: (String)
  }


newtype GetSmsChannelResponse = GetSmsChannelResponse 
  { "SMSChannelResponse" :: (SMSChannelResponse)
  }


newtype ImportJobRequest = ImportJobRequest 
  { "DefineSegment" :: NullOrUndefined (Boolean)
  , "ExternalId" :: NullOrUndefined (String)
  , "Format" :: NullOrUndefined (Format)
  , "RegisterEndpoints" :: NullOrUndefined (Boolean)
  , "RoleArn" :: NullOrUndefined (String)
  , "S3Url" :: NullOrUndefined (String)
  , "SegmentId" :: NullOrUndefined (String)
  , "SegmentName" :: NullOrUndefined (String)
  }


newtype ImportJobResource = ImportJobResource 
  { "DefineSegment" :: NullOrUndefined (Boolean)
  , "ExternalId" :: NullOrUndefined (String)
  , "Format" :: NullOrUndefined (Format)
  , "RegisterEndpoints" :: NullOrUndefined (Boolean)
  , "RoleArn" :: NullOrUndefined (String)
  , "S3Url" :: NullOrUndefined (String)
  , "SegmentId" :: NullOrUndefined (String)
  , "SegmentName" :: NullOrUndefined (String)
  }


newtype ImportJobResponse = ImportJobResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CompletedPieces" :: NullOrUndefined (Int)
  , "CompletionDate" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "Definition" :: NullOrUndefined (ImportJobResource)
  , "FailedPieces" :: NullOrUndefined (Int)
  , "Failures" :: NullOrUndefined (ListOf__string)
  , "Id" :: NullOrUndefined (String)
  , "JobStatus" :: NullOrUndefined (JobStatus)
  , "TotalFailures" :: NullOrUndefined (Int)
  , "TotalPieces" :: NullOrUndefined (Int)
  , "TotalProcessed" :: NullOrUndefined (Int)
  , "Type" :: NullOrUndefined (String)
  }


-- | Import job list.
newtype ImportJobsResponse = ImportJobsResponse 
  { "Item" :: NullOrUndefined (ListOfImportJobResponse)
  , "NextToken" :: NullOrUndefined (String)
  }


-- | Simple message object.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }


newtype JobStatus = JobStatus String


newtype ListOfActivityResponse = ListOfActivityResponse (Array ActivityResponse)


newtype ListOfApplicationResponse = ListOfApplicationResponse (Array ApplicationResponse)


newtype ListOfCampaignResponse = ListOfCampaignResponse (Array CampaignResponse)


newtype ListOfEndpointBatchItem = ListOfEndpointBatchItem (Array EndpointBatchItem)


newtype ListOfImportJobResponse = ListOfImportJobResponse (Array ImportJobResponse)


newtype ListOfSegmentResponse = ListOfSegmentResponse (Array SegmentResponse)


newtype ListOfTreatmentResource = ListOfTreatmentResource (Array TreatmentResource)


newtype ListOfWriteTreatmentResource = ListOfWriteTreatmentResource (Array WriteTreatmentResource)


newtype ListOf__string = ListOf__string (Array String)


newtype MapOfAddressConfiguration = MapOfAddressConfiguration (Map String AddressConfiguration)


newtype MapOfAttributeDimension = MapOfAttributeDimension (Map String AttributeDimension)


newtype MapOfEndpointMessageResult = MapOfEndpointMessageResult (Map String EndpointMessageResult)


newtype MapOfEndpointSendConfiguration = MapOfEndpointSendConfiguration (Map String EndpointSendConfiguration)


newtype MapOfListOf__string = MapOfListOf__string (Map String ListOf__string)


newtype MapOfMapOfEndpointMessageResult = MapOfMapOfEndpointMessageResult (Map String MapOfEndpointMessageResult)


newtype MapOfMessageResult = MapOfMessageResult (Map String MessageResult)


newtype MapOf__double = MapOf__double (Map String Number)


newtype MapOf__integer = MapOf__integer (Map String Int)


newtype MapOf__string = MapOf__string (Map String String)


newtype Message = Message 
  { "Action" :: NullOrUndefined (Action)
  , "Body" :: NullOrUndefined (String)
  , "ImageIconUrl" :: NullOrUndefined (String)
  , "ImageSmallIconUrl" :: NullOrUndefined (String)
  , "ImageUrl" :: NullOrUndefined (String)
  , "JsonBody" :: NullOrUndefined (String)
  , "MediaUrl" :: NullOrUndefined (String)
  , "RawContent" :: NullOrUndefined (String)
  , "SilentPush" :: NullOrUndefined (Boolean)
  , "Title" :: NullOrUndefined (String)
  , "Url" :: NullOrUndefined (String)
  }


-- | Simple message object.
newtype MessageBody = MessageBody 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }


-- | Message configuration for a campaign.
newtype MessageConfiguration = MessageConfiguration 
  { "ADMMessage" :: NullOrUndefined (Message)
  , "APNSMessage" :: NullOrUndefined (Message)
  , "BaiduMessage" :: NullOrUndefined (Message)
  , "DefaultMessage" :: NullOrUndefined (Message)
  , "EmailMessage" :: NullOrUndefined (CampaignEmailMessage)
  , "GCMMessage" :: NullOrUndefined (Message)
  , "SMSMessage" :: NullOrUndefined (CampaignSmsMessage)
  }


-- | Send message request.
newtype MessageRequest = MessageRequest 
  { "Addresses" :: NullOrUndefined (MapOfAddressConfiguration)
  , "Context" :: NullOrUndefined (MapOf__string)
  , "Endpoints" :: NullOrUndefined (MapOfEndpointSendConfiguration)
  , "MessageConfiguration" :: NullOrUndefined (DirectMessageConfiguration)
  }


-- | Send message response.
newtype MessageResponse = MessageResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "EndpointResult" :: NullOrUndefined (MapOfEndpointMessageResult)
  , "RequestId" :: NullOrUndefined (String)
  , "Result" :: NullOrUndefined (MapOfMessageResult)
  }


-- | The result from sending a message to an address.
newtype MessageResult = MessageResult 
  { "DeliveryStatus" :: NullOrUndefined (DeliveryStatus)
  , "StatusCode" :: NullOrUndefined (Int)
  , "StatusMessage" :: NullOrUndefined (String)
  , "UpdatedToken" :: NullOrUndefined (String)
  }


newtype MessageType = MessageType String


-- | Simple message object.
newtype MethodNotAllowedException = MethodNotAllowedException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }


-- | Simple message object.
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }


newtype PutEventStreamRequest = PutEventStreamRequest 
  { "ApplicationId" :: (String)
  , "WriteEventStream" :: (WriteEventStream)
  }


newtype PutEventStreamResponse = PutEventStreamResponse 
  { "EventStream" :: (EventStream)
  }


-- | Quiet Time
newtype QuietTime = QuietTime 
  { "End" :: NullOrUndefined (String)
  , "Start" :: NullOrUndefined (String)
  }


-- | Define how a segment based on recency of use.
newtype RecencyDimension = RecencyDimension 
  { "Duration" :: NullOrUndefined (Duration)
  , "RecencyType" :: NullOrUndefined (RecencyType)
  }


newtype RecencyType = RecencyType String


-- | SMS Channel Request
newtype SMSChannelRequest = SMSChannelRequest 
  { "Enabled" :: NullOrUndefined (Boolean)
  , "SenderId" :: NullOrUndefined (String)
  , "ShortCode" :: NullOrUndefined (String)
  }


-- | SMS Channel Response.
newtype SMSChannelResponse = SMSChannelResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "HasCredential" :: NullOrUndefined (Boolean)
  , "Id" :: NullOrUndefined (String)
  , "IsArchived" :: NullOrUndefined (Boolean)
  , "LastModifiedBy" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "SenderId" :: NullOrUndefined (String)
  , "ShortCode" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (Int)
  }


-- | SMS Message.
newtype SMSMessage = SMSMessage 
  { "Body" :: NullOrUndefined (String)
  , "MessageType" :: NullOrUndefined (MessageType)
  , "SenderId" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  }


-- | Shcedule that defines when a campaign is run.
newtype Schedule = Schedule 
  { "EndTime" :: NullOrUndefined (String)
  , "Frequency" :: NullOrUndefined (Frequency)
  , "IsLocalTime" :: NullOrUndefined (Boolean)
  , "QuietTime" :: NullOrUndefined (QuietTime)
  , "StartTime" :: NullOrUndefined (String)
  , "Timezone" :: NullOrUndefined (String)
  }


-- | Segment behavior dimensions
newtype SegmentBehaviors = SegmentBehaviors 
  { "Recency" :: NullOrUndefined (RecencyDimension)
  }


-- | Segment demographic dimensions
newtype SegmentDemographics = SegmentDemographics 
  { "AppVersion" :: NullOrUndefined (SetDimension)
  , "Channel" :: NullOrUndefined (SetDimension)
  , "DeviceType" :: NullOrUndefined (SetDimension)
  , "Make" :: NullOrUndefined (SetDimension)
  , "Model" :: NullOrUndefined (SetDimension)
  , "Platform" :: NullOrUndefined (SetDimension)
  }


-- | Segment dimensions
newtype SegmentDimensions = SegmentDimensions 
  { "Attributes" :: NullOrUndefined (MapOfAttributeDimension)
  , "Behavior" :: NullOrUndefined (SegmentBehaviors)
  , "Demographic" :: NullOrUndefined (SegmentDemographics)
  , "Location" :: NullOrUndefined (SegmentLocation)
  , "UserAttributes" :: NullOrUndefined (MapOfAttributeDimension)
  }


-- | Segment import definition.
newtype SegmentImportResource = SegmentImportResource 
  { "ChannelCounts" :: NullOrUndefined (MapOf__integer)
  , "ExternalId" :: NullOrUndefined (String)
  , "Format" :: NullOrUndefined (Format)
  , "RoleArn" :: NullOrUndefined (String)
  , "S3Url" :: NullOrUndefined (String)
  , "Size" :: NullOrUndefined (Int)
  }


-- | Segment location dimensions
newtype SegmentLocation = SegmentLocation 
  { "Country" :: NullOrUndefined (SetDimension)
  }


-- | Segment definition.
newtype SegmentResponse = SegmentResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "CreationDate" :: NullOrUndefined (String)
  , "Dimensions" :: NullOrUndefined (SegmentDimensions)
  , "Id" :: NullOrUndefined (String)
  , "ImportDefinition" :: NullOrUndefined (SegmentImportResource)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "SegmentType" :: NullOrUndefined (SegmentType)
  , "Version" :: NullOrUndefined (Int)
  }


newtype SegmentType = SegmentType String


-- | Segments in your account.
newtype SegmentsResponse = SegmentsResponse 
  { "Item" :: NullOrUndefined (ListOfSegmentResponse)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype SendMessagesRequest = SendMessagesRequest 
  { "ApplicationId" :: (String)
  , "MessageRequest" :: (MessageRequest)
  }


newtype SendMessagesResponse = SendMessagesResponse 
  { "MessageResponse" :: (MessageResponse)
  }


-- | Send message request.
newtype SendUsersMessageRequest = SendUsersMessageRequest 
  { "Context" :: NullOrUndefined (MapOf__string)
  , "MessageConfiguration" :: NullOrUndefined (DirectMessageConfiguration)
  , "Users" :: NullOrUndefined (MapOfEndpointSendConfiguration)
  }


-- | User send message response.
newtype SendUsersMessageResponse = SendUsersMessageResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "Result" :: NullOrUndefined (MapOfMapOfEndpointMessageResult)
  }


newtype SendUsersMessagesRequest = SendUsersMessagesRequest 
  { "ApplicationId" :: (String)
  , "SendUsersMessageRequest" :: (SendUsersMessageRequest)
  }


newtype SendUsersMessagesResponse = SendUsersMessagesResponse 
  { "SendUsersMessageResponse" :: (SendUsersMessageResponse)
  }


-- | Dimension specification of a segment.
newtype SetDimension = SetDimension 
  { "DimensionType" :: NullOrUndefined (DimensionType)
  , "Values" :: NullOrUndefined (ListOf__string)
  }


-- | Simple message object.
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }


-- | Treatment resource
newtype TreatmentResource = TreatmentResource 
  { "Id" :: NullOrUndefined (String)
  , "MessageConfiguration" :: NullOrUndefined (MessageConfiguration)
  , "Schedule" :: NullOrUndefined (Schedule)
  , "SizePercent" :: NullOrUndefined (Int)
  , "State" :: NullOrUndefined (CampaignState)
  , "TreatmentDescription" :: NullOrUndefined (String)
  , "TreatmentName" :: NullOrUndefined (String)
  }


newtype UpdateAdmChannelRequest = UpdateAdmChannelRequest 
  { "ADMChannelRequest" :: (ADMChannelRequest)
  , "ApplicationId" :: (String)
  }


newtype UpdateAdmChannelResponse = UpdateAdmChannelResponse 
  { "ADMChannelResponse" :: (ADMChannelResponse)
  }


newtype UpdateApnsChannelRequest = UpdateApnsChannelRequest 
  { "APNSChannelRequest" :: (APNSChannelRequest)
  , "ApplicationId" :: (String)
  }


newtype UpdateApnsChannelResponse = UpdateApnsChannelResponse 
  { "APNSChannelResponse" :: (APNSChannelResponse)
  }


newtype UpdateApnsSandboxChannelRequest = UpdateApnsSandboxChannelRequest 
  { "APNSSandboxChannelRequest" :: (APNSSandboxChannelRequest)
  , "ApplicationId" :: (String)
  }


newtype UpdateApnsSandboxChannelResponse = UpdateApnsSandboxChannelResponse 
  { "APNSSandboxChannelResponse" :: (APNSSandboxChannelResponse)
  }


newtype UpdateApnsVoipChannelRequest = UpdateApnsVoipChannelRequest 
  { "APNSVoipChannelRequest" :: (APNSVoipChannelRequest)
  , "ApplicationId" :: (String)
  }


newtype UpdateApnsVoipChannelResponse = UpdateApnsVoipChannelResponse 
  { "APNSVoipChannelResponse" :: (APNSVoipChannelResponse)
  }


newtype UpdateApnsVoipSandboxChannelRequest = UpdateApnsVoipSandboxChannelRequest 
  { "APNSVoipSandboxChannelRequest" :: (APNSVoipSandboxChannelRequest)
  , "ApplicationId" :: (String)
  }


newtype UpdateApnsVoipSandboxChannelResponse = UpdateApnsVoipSandboxChannelResponse 
  { "APNSVoipSandboxChannelResponse" :: (APNSVoipSandboxChannelResponse)
  }


newtype UpdateApplicationSettingsRequest = UpdateApplicationSettingsRequest 
  { "ApplicationId" :: (String)
  , "WriteApplicationSettingsRequest" :: (WriteApplicationSettingsRequest)
  }


newtype UpdateApplicationSettingsResponse = UpdateApplicationSettingsResponse 
  { "ApplicationSettingsResource" :: (ApplicationSettingsResource)
  }


newtype UpdateBaiduChannelRequest = UpdateBaiduChannelRequest 
  { "ApplicationId" :: (String)
  , "BaiduChannelRequest" :: (BaiduChannelRequest)
  }


newtype UpdateBaiduChannelResponse = UpdateBaiduChannelResponse 
  { "BaiduChannelResponse" :: (BaiduChannelResponse)
  }


newtype UpdateCampaignRequest = UpdateCampaignRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "WriteCampaignRequest" :: (WriteCampaignRequest)
  }


newtype UpdateCampaignResponse = UpdateCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }


newtype UpdateEmailChannelRequest = UpdateEmailChannelRequest 
  { "ApplicationId" :: (String)
  , "EmailChannelRequest" :: (EmailChannelRequest)
  }


newtype UpdateEmailChannelResponse = UpdateEmailChannelResponse 
  { "EmailChannelResponse" :: (EmailChannelResponse)
  }


newtype UpdateEndpointRequest = UpdateEndpointRequest 
  { "ApplicationId" :: (String)
  , "EndpointId" :: (String)
  , "EndpointRequest" :: (EndpointRequest)
  }


newtype UpdateEndpointResponse = UpdateEndpointResponse 
  { "MessageBody" :: (MessageBody)
  }


newtype UpdateEndpointsBatchRequest = UpdateEndpointsBatchRequest 
  { "ApplicationId" :: (String)
  , "EndpointBatchRequest" :: (EndpointBatchRequest)
  }


newtype UpdateEndpointsBatchResponse = UpdateEndpointsBatchResponse 
  { "MessageBody" :: (MessageBody)
  }


newtype UpdateGcmChannelRequest = UpdateGcmChannelRequest 
  { "ApplicationId" :: (String)
  , "GCMChannelRequest" :: (GCMChannelRequest)
  }


newtype UpdateGcmChannelResponse = UpdateGcmChannelResponse 
  { "GCMChannelResponse" :: (GCMChannelResponse)
  }


newtype UpdateSegmentRequest = UpdateSegmentRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  , "WriteSegmentRequest" :: (WriteSegmentRequest)
  }


newtype UpdateSegmentResponse = UpdateSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }


newtype UpdateSmsChannelRequest = UpdateSmsChannelRequest 
  { "ApplicationId" :: (String)
  , "SMSChannelRequest" :: (SMSChannelRequest)
  }


newtype UpdateSmsChannelResponse = UpdateSmsChannelResponse 
  { "SMSChannelResponse" :: (SMSChannelResponse)
  }


-- | Creating application setting request
newtype WriteApplicationSettingsRequest = WriteApplicationSettingsRequest 
  { "Limits" :: NullOrUndefined (CampaignLimits)
  , "QuietTime" :: NullOrUndefined (QuietTime)
  }


-- | Used to create a campaign.
newtype WriteCampaignRequest = WriteCampaignRequest 
  { "AdditionalTreatments" :: NullOrUndefined (ListOfWriteTreatmentResource)
  , "Description" :: NullOrUndefined (String)
  , "HoldoutPercent" :: NullOrUndefined (Int)
  , "IsPaused" :: NullOrUndefined (Boolean)
  , "Limits" :: NullOrUndefined (CampaignLimits)
  , "MessageConfiguration" :: NullOrUndefined (MessageConfiguration)
  , "Name" :: NullOrUndefined (String)
  , "Schedule" :: NullOrUndefined (Schedule)
  , "SegmentId" :: NullOrUndefined (String)
  , "SegmentVersion" :: NullOrUndefined (Int)
  , "TreatmentDescription" :: NullOrUndefined (String)
  , "TreatmentName" :: NullOrUndefined (String)
  }


-- | Request to save an EventStream.
newtype WriteEventStream = WriteEventStream 
  { "DestinationStreamArn" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }


-- | Segment definition.
newtype WriteSegmentRequest = WriteSegmentRequest 
  { "Dimensions" :: NullOrUndefined (SegmentDimensions)
  , "Name" :: NullOrUndefined (String)
  }


-- | Used to create a campaign treatment.
newtype WriteTreatmentResource = WriteTreatmentResource 
  { "MessageConfiguration" :: NullOrUndefined (MessageConfiguration)
  , "Schedule" :: NullOrUndefined (Schedule)
  , "SizePercent" :: NullOrUndefined (Int)
  , "TreatmentDescription" :: NullOrUndefined (String)
  , "TreatmentName" :: NullOrUndefined (String)
  }
