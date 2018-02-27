

module AWS.Pinpoint where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeADMChannelRequest :: Newtype ADMChannelRequest _


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
derive instance newtypeADMChannelResponse :: Newtype ADMChannelResponse _


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
derive instance newtypeADMMessage :: Newtype ADMMessage _


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
derive instance newtypeAPNSChannelRequest :: Newtype APNSChannelRequest _


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
derive instance newtypeAPNSChannelResponse :: Newtype APNSChannelResponse _


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
derive instance newtypeAPNSMessage :: Newtype APNSMessage _


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
derive instance newtypeAPNSSandboxChannelRequest :: Newtype APNSSandboxChannelRequest _


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
derive instance newtypeAPNSSandboxChannelResponse :: Newtype APNSSandboxChannelResponse _


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
derive instance newtypeAPNSVoipChannelRequest :: Newtype APNSVoipChannelRequest _


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
derive instance newtypeAPNSVoipChannelResponse :: Newtype APNSVoipChannelResponse _


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
derive instance newtypeAPNSVoipSandboxChannelRequest :: Newtype APNSVoipSandboxChannelRequest _


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
derive instance newtypeAPNSVoipSandboxChannelResponse :: Newtype APNSVoipSandboxChannelResponse _


newtype Action = Action String
derive instance newtypeAction :: Newtype Action _


-- | Activities for campaign.
newtype ActivitiesResponse = ActivitiesResponse 
  { "Item" :: NullOrUndefined (ListOfActivityResponse)
  }
derive instance newtypeActivitiesResponse :: Newtype ActivitiesResponse _


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
derive instance newtypeActivityResponse :: Newtype ActivityResponse _


-- | Address configuration.
newtype AddressConfiguration = AddressConfiguration 
  { "BodyOverride" :: NullOrUndefined (String)
  , "ChannelType" :: NullOrUndefined (ChannelType)
  , "Context" :: NullOrUndefined (MapOf__string)
  , "RawContent" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  , "TitleOverride" :: NullOrUndefined (String)
  }
derive instance newtypeAddressConfiguration :: Newtype AddressConfiguration _


-- | Application Response.
newtype ApplicationResponse = ApplicationResponse 
  { "Id" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeApplicationResponse :: Newtype ApplicationResponse _


-- | Application settings.
newtype ApplicationSettingsResource = ApplicationSettingsResource 
  { "ApplicationId" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "Limits" :: NullOrUndefined (CampaignLimits)
  , "QuietTime" :: NullOrUndefined (QuietTime)
  }
derive instance newtypeApplicationSettingsResource :: Newtype ApplicationSettingsResource _


-- | Get Applications Result.
newtype ApplicationsResponse = ApplicationsResponse 
  { "Item" :: NullOrUndefined (ListOfApplicationResponse)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeApplicationsResponse :: Newtype ApplicationsResponse _


-- | Custom attibute dimension
newtype AttributeDimension = AttributeDimension 
  { "AttributeType" :: NullOrUndefined (AttributeType)
  , "Values" :: NullOrUndefined (ListOf__string)
  }
derive instance newtypeAttributeDimension :: Newtype AttributeDimension _


newtype AttributeType = AttributeType String
derive instance newtypeAttributeType :: Newtype AttributeType _


-- | Simple message object.
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


-- | Baidu Cloud Push credentials
newtype BaiduChannelRequest = BaiduChannelRequest 
  { "ApiKey" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "SecretKey" :: NullOrUndefined (String)
  }
derive instance newtypeBaiduChannelRequest :: Newtype BaiduChannelRequest _


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
derive instance newtypeBaiduChannelResponse :: Newtype BaiduChannelResponse _


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
derive instance newtypeBaiduMessage :: Newtype BaiduMessage _


-- | The email message configuration.
newtype CampaignEmailMessage = CampaignEmailMessage 
  { "Body" :: NullOrUndefined (String)
  , "FromAddress" :: NullOrUndefined (String)
  , "HtmlBody" :: NullOrUndefined (String)
  , "Title" :: NullOrUndefined (String)
  }
derive instance newtypeCampaignEmailMessage :: Newtype CampaignEmailMessage _


-- | Campaign Limits are used to limit the number of messages that can be sent to a user.
newtype CampaignLimits = CampaignLimits 
  { "Daily" :: NullOrUndefined (Int)
  , "MaximumDuration" :: NullOrUndefined (Int)
  , "MessagesPerSecond" :: NullOrUndefined (Int)
  , "Total" :: NullOrUndefined (Int)
  }
derive instance newtypeCampaignLimits :: Newtype CampaignLimits _


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
derive instance newtypeCampaignResponse :: Newtype CampaignResponse _


-- | SMS message configuration.
newtype CampaignSmsMessage = CampaignSmsMessage 
  { "Body" :: NullOrUndefined (String)
  , "MessageType" :: NullOrUndefined (MessageType)
  , "SenderId" :: NullOrUndefined (String)
  }
derive instance newtypeCampaignSmsMessage :: Newtype CampaignSmsMessage _


-- | State of the Campaign
newtype CampaignState = CampaignState 
  { "CampaignStatus" :: NullOrUndefined (CampaignStatus)
  }
derive instance newtypeCampaignState :: Newtype CampaignState _


newtype CampaignStatus = CampaignStatus String
derive instance newtypeCampaignStatus :: Newtype CampaignStatus _


-- | List of available campaigns.
newtype CampaignsResponse = CampaignsResponse 
  { "Item" :: NullOrUndefined (ListOfCampaignResponse)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeCampaignsResponse :: Newtype CampaignsResponse _


newtype ChannelType = ChannelType String
derive instance newtypeChannelType :: Newtype ChannelType _


newtype CreateAppRequest = CreateAppRequest 
  { "CreateApplicationRequest" :: (CreateApplicationRequest)
  }
derive instance newtypeCreateAppRequest :: Newtype CreateAppRequest _


newtype CreateAppResponse = CreateAppResponse 
  { "ApplicationResponse" :: (ApplicationResponse)
  }
derive instance newtypeCreateAppResponse :: Newtype CreateAppResponse _


-- | Application Request.
newtype CreateApplicationRequest = CreateApplicationRequest 
  { "Name" :: NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationRequest :: Newtype CreateApplicationRequest _


newtype CreateCampaignRequest = CreateCampaignRequest 
  { "ApplicationId" :: (String)
  , "WriteCampaignRequest" :: (WriteCampaignRequest)
  }
derive instance newtypeCreateCampaignRequest :: Newtype CreateCampaignRequest _


newtype CreateCampaignResponse = CreateCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeCreateCampaignResponse :: Newtype CreateCampaignResponse _


newtype CreateImportJobRequest = CreateImportJobRequest 
  { "ApplicationId" :: (String)
  , "ImportJobRequest" :: (ImportJobRequest)
  }
derive instance newtypeCreateImportJobRequest :: Newtype CreateImportJobRequest _


newtype CreateImportJobResponse = CreateImportJobResponse 
  { "ImportJobResponse" :: (ImportJobResponse)
  }
derive instance newtypeCreateImportJobResponse :: Newtype CreateImportJobResponse _


newtype CreateSegmentRequest = CreateSegmentRequest 
  { "ApplicationId" :: (String)
  , "WriteSegmentRequest" :: (WriteSegmentRequest)
  }
derive instance newtypeCreateSegmentRequest :: Newtype CreateSegmentRequest _


newtype CreateSegmentResponse = CreateSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeCreateSegmentResponse :: Newtype CreateSegmentResponse _


-- | Default Message across push notification, email, and sms.
newtype DefaultMessage = DefaultMessage 
  { "Body" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  }
derive instance newtypeDefaultMessage :: Newtype DefaultMessage _


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
derive instance newtypeDefaultPushNotificationMessage :: Newtype DefaultPushNotificationMessage _


newtype DeleteAdmChannelRequest = DeleteAdmChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteAdmChannelRequest :: Newtype DeleteAdmChannelRequest _


newtype DeleteAdmChannelResponse = DeleteAdmChannelResponse 
  { "ADMChannelResponse" :: (ADMChannelResponse)
  }
derive instance newtypeDeleteAdmChannelResponse :: Newtype DeleteAdmChannelResponse _


newtype DeleteApnsChannelRequest = DeleteApnsChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteApnsChannelRequest :: Newtype DeleteApnsChannelRequest _


newtype DeleteApnsChannelResponse = DeleteApnsChannelResponse 
  { "APNSChannelResponse" :: (APNSChannelResponse)
  }
derive instance newtypeDeleteApnsChannelResponse :: Newtype DeleteApnsChannelResponse _


newtype DeleteApnsSandboxChannelRequest = DeleteApnsSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteApnsSandboxChannelRequest :: Newtype DeleteApnsSandboxChannelRequest _


newtype DeleteApnsSandboxChannelResponse = DeleteApnsSandboxChannelResponse 
  { "APNSSandboxChannelResponse" :: (APNSSandboxChannelResponse)
  }
derive instance newtypeDeleteApnsSandboxChannelResponse :: Newtype DeleteApnsSandboxChannelResponse _


newtype DeleteApnsVoipChannelRequest = DeleteApnsVoipChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteApnsVoipChannelRequest :: Newtype DeleteApnsVoipChannelRequest _


newtype DeleteApnsVoipChannelResponse = DeleteApnsVoipChannelResponse 
  { "APNSVoipChannelResponse" :: (APNSVoipChannelResponse)
  }
derive instance newtypeDeleteApnsVoipChannelResponse :: Newtype DeleteApnsVoipChannelResponse _


newtype DeleteApnsVoipSandboxChannelRequest = DeleteApnsVoipSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteApnsVoipSandboxChannelRequest :: Newtype DeleteApnsVoipSandboxChannelRequest _


newtype DeleteApnsVoipSandboxChannelResponse = DeleteApnsVoipSandboxChannelResponse 
  { "APNSVoipSandboxChannelResponse" :: (APNSVoipSandboxChannelResponse)
  }
derive instance newtypeDeleteApnsVoipSandboxChannelResponse :: Newtype DeleteApnsVoipSandboxChannelResponse _


newtype DeleteAppRequest = DeleteAppRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteAppRequest :: Newtype DeleteAppRequest _


newtype DeleteAppResponse = DeleteAppResponse 
  { "ApplicationResponse" :: (ApplicationResponse)
  }
derive instance newtypeDeleteAppResponse :: Newtype DeleteAppResponse _


newtype DeleteBaiduChannelRequest = DeleteBaiduChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteBaiduChannelRequest :: Newtype DeleteBaiduChannelRequest _


newtype DeleteBaiduChannelResponse = DeleteBaiduChannelResponse 
  { "BaiduChannelResponse" :: (BaiduChannelResponse)
  }
derive instance newtypeDeleteBaiduChannelResponse :: Newtype DeleteBaiduChannelResponse _


newtype DeleteCampaignRequest = DeleteCampaignRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  }
derive instance newtypeDeleteCampaignRequest :: Newtype DeleteCampaignRequest _


newtype DeleteCampaignResponse = DeleteCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeDeleteCampaignResponse :: Newtype DeleteCampaignResponse _


newtype DeleteEmailChannelRequest = DeleteEmailChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteEmailChannelRequest :: Newtype DeleteEmailChannelRequest _


newtype DeleteEmailChannelResponse = DeleteEmailChannelResponse 
  { "EmailChannelResponse" :: (EmailChannelResponse)
  }
derive instance newtypeDeleteEmailChannelResponse :: Newtype DeleteEmailChannelResponse _


-- | DeleteEventStream Request
newtype DeleteEventStreamRequest = DeleteEventStreamRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteEventStreamRequest :: Newtype DeleteEventStreamRequest _


newtype DeleteEventStreamResponse = DeleteEventStreamResponse 
  { "EventStream" :: (EventStream)
  }
derive instance newtypeDeleteEventStreamResponse :: Newtype DeleteEventStreamResponse _


newtype DeleteGcmChannelRequest = DeleteGcmChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteGcmChannelRequest :: Newtype DeleteGcmChannelRequest _


newtype DeleteGcmChannelResponse = DeleteGcmChannelResponse 
  { "GCMChannelResponse" :: (GCMChannelResponse)
  }
derive instance newtypeDeleteGcmChannelResponse :: Newtype DeleteGcmChannelResponse _


newtype DeleteSegmentRequest = DeleteSegmentRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  }
derive instance newtypeDeleteSegmentRequest :: Newtype DeleteSegmentRequest _


newtype DeleteSegmentResponse = DeleteSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeDeleteSegmentResponse :: Newtype DeleteSegmentResponse _


newtype DeleteSmsChannelRequest = DeleteSmsChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeDeleteSmsChannelRequest :: Newtype DeleteSmsChannelRequest _


newtype DeleteSmsChannelResponse = DeleteSmsChannelResponse 
  { "SMSChannelResponse" :: (SMSChannelResponse)
  }
derive instance newtypeDeleteSmsChannelResponse :: Newtype DeleteSmsChannelResponse _


newtype DeliveryStatus = DeliveryStatus String
derive instance newtypeDeliveryStatus :: Newtype DeliveryStatus _


newtype DimensionType = DimensionType String
derive instance newtypeDimensionType :: Newtype DimensionType _


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
derive instance newtypeDirectMessageConfiguration :: Newtype DirectMessageConfiguration _


newtype Duration = Duration String
derive instance newtypeDuration :: Newtype Duration _


-- | Email Channel Request
newtype EmailChannelRequest = EmailChannelRequest 
  { "Enabled" :: NullOrUndefined (Boolean)
  , "FromAddress" :: NullOrUndefined (String)
  , "Identity" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }
derive instance newtypeEmailChannelRequest :: Newtype EmailChannelRequest _


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
derive instance newtypeEmailChannelResponse :: Newtype EmailChannelResponse _


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
derive instance newtypeEndpointBatchItem :: Newtype EndpointBatchItem _


-- | Endpoint batch update request.
newtype EndpointBatchRequest = EndpointBatchRequest 
  { "Item" :: NullOrUndefined (ListOfEndpointBatchItem)
  }
derive instance newtypeEndpointBatchRequest :: Newtype EndpointBatchRequest _


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
derive instance newtypeEndpointDemographic :: Newtype EndpointDemographic _


-- | Endpoint location data
newtype EndpointLocation = EndpointLocation 
  { "City" :: NullOrUndefined (String)
  , "Country" :: NullOrUndefined (String)
  , "Latitude" :: NullOrUndefined (Number)
  , "Longitude" :: NullOrUndefined (Number)
  , "PostalCode" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  }
derive instance newtypeEndpointLocation :: Newtype EndpointLocation _


-- | The result from sending a message to an endpoint.
newtype EndpointMessageResult = EndpointMessageResult 
  { "Address" :: NullOrUndefined (String)
  , "DeliveryStatus" :: NullOrUndefined (DeliveryStatus)
  , "StatusCode" :: NullOrUndefined (Int)
  , "StatusMessage" :: NullOrUndefined (String)
  , "UpdatedToken" :: NullOrUndefined (String)
  }
derive instance newtypeEndpointMessageResult :: Newtype EndpointMessageResult _


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
derive instance newtypeEndpointRequest :: Newtype EndpointRequest _


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
derive instance newtypeEndpointResponse :: Newtype EndpointResponse _


-- | Endpoint send configuration.
newtype EndpointSendConfiguration = EndpointSendConfiguration 
  { "BodyOverride" :: NullOrUndefined (String)
  , "Context" :: NullOrUndefined (MapOf__string)
  , "RawContent" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  , "TitleOverride" :: NullOrUndefined (String)
  }
derive instance newtypeEndpointSendConfiguration :: Newtype EndpointSendConfiguration _


-- | Endpoint user specific custom userAttributes
newtype EndpointUser = EndpointUser 
  { "UserAttributes" :: NullOrUndefined (MapOfListOf__string)
  , "UserId" :: NullOrUndefined (String)
  }
derive instance newtypeEndpointUser :: Newtype EndpointUser _


-- | Model for an event publishing subscription export.
newtype EventStream = EventStream 
  { "ApplicationId" :: NullOrUndefined (String)
  , "DestinationStreamArn" :: NullOrUndefined (String)
  , "ExternalId" :: NullOrUndefined (String)
  , "LastModifiedDate" :: NullOrUndefined (String)
  , "LastUpdatedBy" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }
derive instance newtypeEventStream :: Newtype EventStream _


-- | Simple message object.
newtype ForbiddenException = ForbiddenException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }
derive instance newtypeForbiddenException :: Newtype ForbiddenException _


newtype Format = Format String
derive instance newtypeFormat :: Newtype Format _


newtype Frequency = Frequency String
derive instance newtypeFrequency :: Newtype Frequency _


-- | Google Cloud Messaging credentials
newtype GCMChannelRequest = GCMChannelRequest 
  { "ApiKey" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  }
derive instance newtypeGCMChannelRequest :: Newtype GCMChannelRequest _


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
derive instance newtypeGCMChannelResponse :: Newtype GCMChannelResponse _


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
derive instance newtypeGCMMessage :: Newtype GCMMessage _


newtype GetAdmChannelRequest = GetAdmChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetAdmChannelRequest :: Newtype GetAdmChannelRequest _


newtype GetAdmChannelResponse = GetAdmChannelResponse 
  { "ADMChannelResponse" :: (ADMChannelResponse)
  }
derive instance newtypeGetAdmChannelResponse :: Newtype GetAdmChannelResponse _


newtype GetApnsChannelRequest = GetApnsChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApnsChannelRequest :: Newtype GetApnsChannelRequest _


newtype GetApnsChannelResponse = GetApnsChannelResponse 
  { "APNSChannelResponse" :: (APNSChannelResponse)
  }
derive instance newtypeGetApnsChannelResponse :: Newtype GetApnsChannelResponse _


newtype GetApnsSandboxChannelRequest = GetApnsSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApnsSandboxChannelRequest :: Newtype GetApnsSandboxChannelRequest _


newtype GetApnsSandboxChannelResponse = GetApnsSandboxChannelResponse 
  { "APNSSandboxChannelResponse" :: (APNSSandboxChannelResponse)
  }
derive instance newtypeGetApnsSandboxChannelResponse :: Newtype GetApnsSandboxChannelResponse _


newtype GetApnsVoipChannelRequest = GetApnsVoipChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApnsVoipChannelRequest :: Newtype GetApnsVoipChannelRequest _


newtype GetApnsVoipChannelResponse = GetApnsVoipChannelResponse 
  { "APNSVoipChannelResponse" :: (APNSVoipChannelResponse)
  }
derive instance newtypeGetApnsVoipChannelResponse :: Newtype GetApnsVoipChannelResponse _


newtype GetApnsVoipSandboxChannelRequest = GetApnsVoipSandboxChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApnsVoipSandboxChannelRequest :: Newtype GetApnsVoipSandboxChannelRequest _


newtype GetApnsVoipSandboxChannelResponse = GetApnsVoipSandboxChannelResponse 
  { "APNSVoipSandboxChannelResponse" :: (APNSVoipSandboxChannelResponse)
  }
derive instance newtypeGetApnsVoipSandboxChannelResponse :: Newtype GetApnsVoipSandboxChannelResponse _


newtype GetAppRequest = GetAppRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetAppRequest :: Newtype GetAppRequest _


newtype GetAppResponse = GetAppResponse 
  { "ApplicationResponse" :: (ApplicationResponse)
  }
derive instance newtypeGetAppResponse :: Newtype GetAppResponse _


newtype GetApplicationSettingsRequest = GetApplicationSettingsRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetApplicationSettingsRequest :: Newtype GetApplicationSettingsRequest _


newtype GetApplicationSettingsResponse = GetApplicationSettingsResponse 
  { "ApplicationSettingsResource" :: (ApplicationSettingsResource)
  }
derive instance newtypeGetApplicationSettingsResponse :: Newtype GetApplicationSettingsResponse _


newtype GetAppsRequest = GetAppsRequest 
  { "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }
derive instance newtypeGetAppsRequest :: Newtype GetAppsRequest _


newtype GetAppsResponse = GetAppsResponse 
  { "ApplicationsResponse" :: (ApplicationsResponse)
  }
derive instance newtypeGetAppsResponse :: Newtype GetAppsResponse _


newtype GetBaiduChannelRequest = GetBaiduChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetBaiduChannelRequest :: Newtype GetBaiduChannelRequest _


newtype GetBaiduChannelResponse = GetBaiduChannelResponse 
  { "BaiduChannelResponse" :: (BaiduChannelResponse)
  }
derive instance newtypeGetBaiduChannelResponse :: Newtype GetBaiduChannelResponse _


newtype GetCampaignActivitiesRequest = GetCampaignActivitiesRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }
derive instance newtypeGetCampaignActivitiesRequest :: Newtype GetCampaignActivitiesRequest _


newtype GetCampaignActivitiesResponse = GetCampaignActivitiesResponse 
  { "ActivitiesResponse" :: (ActivitiesResponse)
  }
derive instance newtypeGetCampaignActivitiesResponse :: Newtype GetCampaignActivitiesResponse _


newtype GetCampaignRequest = GetCampaignRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  }
derive instance newtypeGetCampaignRequest :: Newtype GetCampaignRequest _


newtype GetCampaignResponse = GetCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeGetCampaignResponse :: Newtype GetCampaignResponse _


newtype GetCampaignVersionRequest = GetCampaignVersionRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "Version" :: (String)
  }
derive instance newtypeGetCampaignVersionRequest :: Newtype GetCampaignVersionRequest _


newtype GetCampaignVersionResponse = GetCampaignVersionResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeGetCampaignVersionResponse :: Newtype GetCampaignVersionResponse _


newtype GetCampaignVersionsRequest = GetCampaignVersionsRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }
derive instance newtypeGetCampaignVersionsRequest :: Newtype GetCampaignVersionsRequest _


newtype GetCampaignVersionsResponse = GetCampaignVersionsResponse 
  { "CampaignsResponse" :: (CampaignsResponse)
  }
derive instance newtypeGetCampaignVersionsResponse :: Newtype GetCampaignVersionsResponse _


newtype GetCampaignsRequest = GetCampaignsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }
derive instance newtypeGetCampaignsRequest :: Newtype GetCampaignsRequest _


newtype GetCampaignsResponse = GetCampaignsResponse 
  { "CampaignsResponse" :: (CampaignsResponse)
  }
derive instance newtypeGetCampaignsResponse :: Newtype GetCampaignsResponse _


newtype GetEmailChannelRequest = GetEmailChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetEmailChannelRequest :: Newtype GetEmailChannelRequest _


newtype GetEmailChannelResponse = GetEmailChannelResponse 
  { "EmailChannelResponse" :: (EmailChannelResponse)
  }
derive instance newtypeGetEmailChannelResponse :: Newtype GetEmailChannelResponse _


newtype GetEndpointRequest = GetEndpointRequest 
  { "ApplicationId" :: (String)
  , "EndpointId" :: (String)
  }
derive instance newtypeGetEndpointRequest :: Newtype GetEndpointRequest _


newtype GetEndpointResponse = GetEndpointResponse 
  { "EndpointResponse" :: (EndpointResponse)
  }
derive instance newtypeGetEndpointResponse :: Newtype GetEndpointResponse _


-- | GetEventStreamRequest
newtype GetEventStreamRequest = GetEventStreamRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetEventStreamRequest :: Newtype GetEventStreamRequest _


newtype GetEventStreamResponse = GetEventStreamResponse 
  { "EventStream" :: (EventStream)
  }
derive instance newtypeGetEventStreamResponse :: Newtype GetEventStreamResponse _


newtype GetGcmChannelRequest = GetGcmChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetGcmChannelRequest :: Newtype GetGcmChannelRequest _


newtype GetGcmChannelResponse = GetGcmChannelResponse 
  { "GCMChannelResponse" :: (GCMChannelResponse)
  }
derive instance newtypeGetGcmChannelResponse :: Newtype GetGcmChannelResponse _


newtype GetImportJobRequest = GetImportJobRequest 
  { "ApplicationId" :: (String)
  , "JobId" :: (String)
  }
derive instance newtypeGetImportJobRequest :: Newtype GetImportJobRequest _


newtype GetImportJobResponse = GetImportJobResponse 
  { "ImportJobResponse" :: (ImportJobResponse)
  }
derive instance newtypeGetImportJobResponse :: Newtype GetImportJobResponse _


newtype GetImportJobsRequest = GetImportJobsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }
derive instance newtypeGetImportJobsRequest :: Newtype GetImportJobsRequest _


newtype GetImportJobsResponse = GetImportJobsResponse 
  { "ImportJobsResponse" :: (ImportJobsResponse)
  }
derive instance newtypeGetImportJobsResponse :: Newtype GetImportJobsResponse _


newtype GetSegmentImportJobsRequest = GetSegmentImportJobsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "SegmentId" :: (String)
  , "Token" :: NullOrUndefined (String)
  }
derive instance newtypeGetSegmentImportJobsRequest :: Newtype GetSegmentImportJobsRequest _


newtype GetSegmentImportJobsResponse = GetSegmentImportJobsResponse 
  { "ImportJobsResponse" :: (ImportJobsResponse)
  }
derive instance newtypeGetSegmentImportJobsResponse :: Newtype GetSegmentImportJobsResponse _


newtype GetSegmentRequest = GetSegmentRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  }
derive instance newtypeGetSegmentRequest :: Newtype GetSegmentRequest _


newtype GetSegmentResponse = GetSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeGetSegmentResponse :: Newtype GetSegmentResponse _


newtype GetSegmentVersionRequest = GetSegmentVersionRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  , "Version" :: (String)
  }
derive instance newtypeGetSegmentVersionRequest :: Newtype GetSegmentVersionRequest _


newtype GetSegmentVersionResponse = GetSegmentVersionResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeGetSegmentVersionResponse :: Newtype GetSegmentVersionResponse _


newtype GetSegmentVersionsRequest = GetSegmentVersionsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "SegmentId" :: (String)
  , "Token" :: NullOrUndefined (String)
  }
derive instance newtypeGetSegmentVersionsRequest :: Newtype GetSegmentVersionsRequest _


newtype GetSegmentVersionsResponse = GetSegmentVersionsResponse 
  { "SegmentsResponse" :: (SegmentsResponse)
  }
derive instance newtypeGetSegmentVersionsResponse :: Newtype GetSegmentVersionsResponse _


newtype GetSegmentsRequest = GetSegmentsRequest 
  { "ApplicationId" :: (String)
  , "PageSize" :: NullOrUndefined (String)
  , "Token" :: NullOrUndefined (String)
  }
derive instance newtypeGetSegmentsRequest :: Newtype GetSegmentsRequest _


newtype GetSegmentsResponse = GetSegmentsResponse 
  { "SegmentsResponse" :: (SegmentsResponse)
  }
derive instance newtypeGetSegmentsResponse :: Newtype GetSegmentsResponse _


newtype GetSmsChannelRequest = GetSmsChannelRequest 
  { "ApplicationId" :: (String)
  }
derive instance newtypeGetSmsChannelRequest :: Newtype GetSmsChannelRequest _


newtype GetSmsChannelResponse = GetSmsChannelResponse 
  { "SMSChannelResponse" :: (SMSChannelResponse)
  }
derive instance newtypeGetSmsChannelResponse :: Newtype GetSmsChannelResponse _


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
derive instance newtypeImportJobRequest :: Newtype ImportJobRequest _


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
derive instance newtypeImportJobResource :: Newtype ImportJobResource _


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
derive instance newtypeImportJobResponse :: Newtype ImportJobResponse _


-- | Import job list.
newtype ImportJobsResponse = ImportJobsResponse 
  { "Item" :: NullOrUndefined (ListOfImportJobResponse)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeImportJobsResponse :: Newtype ImportJobsResponse _


-- | Simple message object.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _


newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _


newtype ListOfActivityResponse = ListOfActivityResponse (Array ActivityResponse)
derive instance newtypeListOfActivityResponse :: Newtype ListOfActivityResponse _


newtype ListOfApplicationResponse = ListOfApplicationResponse (Array ApplicationResponse)
derive instance newtypeListOfApplicationResponse :: Newtype ListOfApplicationResponse _


newtype ListOfCampaignResponse = ListOfCampaignResponse (Array CampaignResponse)
derive instance newtypeListOfCampaignResponse :: Newtype ListOfCampaignResponse _


newtype ListOfEndpointBatchItem = ListOfEndpointBatchItem (Array EndpointBatchItem)
derive instance newtypeListOfEndpointBatchItem :: Newtype ListOfEndpointBatchItem _


newtype ListOfImportJobResponse = ListOfImportJobResponse (Array ImportJobResponse)
derive instance newtypeListOfImportJobResponse :: Newtype ListOfImportJobResponse _


newtype ListOfSegmentResponse = ListOfSegmentResponse (Array SegmentResponse)
derive instance newtypeListOfSegmentResponse :: Newtype ListOfSegmentResponse _


newtype ListOfTreatmentResource = ListOfTreatmentResource (Array TreatmentResource)
derive instance newtypeListOfTreatmentResource :: Newtype ListOfTreatmentResource _


newtype ListOfWriteTreatmentResource = ListOfWriteTreatmentResource (Array WriteTreatmentResource)
derive instance newtypeListOfWriteTreatmentResource :: Newtype ListOfWriteTreatmentResource _


newtype ListOf__string = ListOf__string (Array String)
derive instance newtypeListOf__string :: Newtype ListOf__string _


newtype MapOfAddressConfiguration = MapOfAddressConfiguration (Map String AddressConfiguration)
derive instance newtypeMapOfAddressConfiguration :: Newtype MapOfAddressConfiguration _


newtype MapOfAttributeDimension = MapOfAttributeDimension (Map String AttributeDimension)
derive instance newtypeMapOfAttributeDimension :: Newtype MapOfAttributeDimension _


newtype MapOfEndpointMessageResult = MapOfEndpointMessageResult (Map String EndpointMessageResult)
derive instance newtypeMapOfEndpointMessageResult :: Newtype MapOfEndpointMessageResult _


newtype MapOfEndpointSendConfiguration = MapOfEndpointSendConfiguration (Map String EndpointSendConfiguration)
derive instance newtypeMapOfEndpointSendConfiguration :: Newtype MapOfEndpointSendConfiguration _


newtype MapOfListOf__string = MapOfListOf__string (Map String ListOf__string)
derive instance newtypeMapOfListOf__string :: Newtype MapOfListOf__string _


newtype MapOfMapOfEndpointMessageResult = MapOfMapOfEndpointMessageResult (Map String MapOfEndpointMessageResult)
derive instance newtypeMapOfMapOfEndpointMessageResult :: Newtype MapOfMapOfEndpointMessageResult _


newtype MapOfMessageResult = MapOfMessageResult (Map String MessageResult)
derive instance newtypeMapOfMessageResult :: Newtype MapOfMessageResult _


newtype MapOf__double = MapOf__double (Map String Number)
derive instance newtypeMapOf__double :: Newtype MapOf__double _


newtype MapOf__integer = MapOf__integer (Map String Int)
derive instance newtypeMapOf__integer :: Newtype MapOf__integer _


newtype MapOf__string = MapOf__string (Map String String)
derive instance newtypeMapOf__string :: Newtype MapOf__string _


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
derive instance newtypeMessage :: Newtype Message _


-- | Simple message object.
newtype MessageBody = MessageBody 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }
derive instance newtypeMessageBody :: Newtype MessageBody _


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
derive instance newtypeMessageConfiguration :: Newtype MessageConfiguration _


-- | Send message request.
newtype MessageRequest = MessageRequest 
  { "Addresses" :: NullOrUndefined (MapOfAddressConfiguration)
  , "Context" :: NullOrUndefined (MapOf__string)
  , "Endpoints" :: NullOrUndefined (MapOfEndpointSendConfiguration)
  , "MessageConfiguration" :: NullOrUndefined (DirectMessageConfiguration)
  }
derive instance newtypeMessageRequest :: Newtype MessageRequest _


-- | Send message response.
newtype MessageResponse = MessageResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "EndpointResult" :: NullOrUndefined (MapOfEndpointMessageResult)
  , "RequestId" :: NullOrUndefined (String)
  , "Result" :: NullOrUndefined (MapOfMessageResult)
  }
derive instance newtypeMessageResponse :: Newtype MessageResponse _


-- | The result from sending a message to an address.
newtype MessageResult = MessageResult 
  { "DeliveryStatus" :: NullOrUndefined (DeliveryStatus)
  , "StatusCode" :: NullOrUndefined (Int)
  , "StatusMessage" :: NullOrUndefined (String)
  , "UpdatedToken" :: NullOrUndefined (String)
  }
derive instance newtypeMessageResult :: Newtype MessageResult _


newtype MessageType = MessageType String
derive instance newtypeMessageType :: Newtype MessageType _


-- | Simple message object.
newtype MethodNotAllowedException = MethodNotAllowedException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }
derive instance newtypeMethodNotAllowedException :: Newtype MethodNotAllowedException _


-- | Simple message object.
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


newtype PutEventStreamRequest = PutEventStreamRequest 
  { "ApplicationId" :: (String)
  , "WriteEventStream" :: (WriteEventStream)
  }
derive instance newtypePutEventStreamRequest :: Newtype PutEventStreamRequest _


newtype PutEventStreamResponse = PutEventStreamResponse 
  { "EventStream" :: (EventStream)
  }
derive instance newtypePutEventStreamResponse :: Newtype PutEventStreamResponse _


-- | Quiet Time
newtype QuietTime = QuietTime 
  { "End" :: NullOrUndefined (String)
  , "Start" :: NullOrUndefined (String)
  }
derive instance newtypeQuietTime :: Newtype QuietTime _


-- | Define how a segment based on recency of use.
newtype RecencyDimension = RecencyDimension 
  { "Duration" :: NullOrUndefined (Duration)
  , "RecencyType" :: NullOrUndefined (RecencyType)
  }
derive instance newtypeRecencyDimension :: Newtype RecencyDimension _


newtype RecencyType = RecencyType String
derive instance newtypeRecencyType :: Newtype RecencyType _


-- | SMS Channel Request
newtype SMSChannelRequest = SMSChannelRequest 
  { "Enabled" :: NullOrUndefined (Boolean)
  , "SenderId" :: NullOrUndefined (String)
  , "ShortCode" :: NullOrUndefined (String)
  }
derive instance newtypeSMSChannelRequest :: Newtype SMSChannelRequest _


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
derive instance newtypeSMSChannelResponse :: Newtype SMSChannelResponse _


-- | SMS Message.
newtype SMSMessage = SMSMessage 
  { "Body" :: NullOrUndefined (String)
  , "MessageType" :: NullOrUndefined (MessageType)
  , "SenderId" :: NullOrUndefined (String)
  , "Substitutions" :: NullOrUndefined (MapOfListOf__string)
  }
derive instance newtypeSMSMessage :: Newtype SMSMessage _


-- | Shcedule that defines when a campaign is run.
newtype Schedule = Schedule 
  { "EndTime" :: NullOrUndefined (String)
  , "Frequency" :: NullOrUndefined (Frequency)
  , "IsLocalTime" :: NullOrUndefined (Boolean)
  , "QuietTime" :: NullOrUndefined (QuietTime)
  , "StartTime" :: NullOrUndefined (String)
  , "Timezone" :: NullOrUndefined (String)
  }
derive instance newtypeSchedule :: Newtype Schedule _


-- | Segment behavior dimensions
newtype SegmentBehaviors = SegmentBehaviors 
  { "Recency" :: NullOrUndefined (RecencyDimension)
  }
derive instance newtypeSegmentBehaviors :: Newtype SegmentBehaviors _


-- | Segment demographic dimensions
newtype SegmentDemographics = SegmentDemographics 
  { "AppVersion" :: NullOrUndefined (SetDimension)
  , "Channel" :: NullOrUndefined (SetDimension)
  , "DeviceType" :: NullOrUndefined (SetDimension)
  , "Make" :: NullOrUndefined (SetDimension)
  , "Model" :: NullOrUndefined (SetDimension)
  , "Platform" :: NullOrUndefined (SetDimension)
  }
derive instance newtypeSegmentDemographics :: Newtype SegmentDemographics _


-- | Segment dimensions
newtype SegmentDimensions = SegmentDimensions 
  { "Attributes" :: NullOrUndefined (MapOfAttributeDimension)
  , "Behavior" :: NullOrUndefined (SegmentBehaviors)
  , "Demographic" :: NullOrUndefined (SegmentDemographics)
  , "Location" :: NullOrUndefined (SegmentLocation)
  , "UserAttributes" :: NullOrUndefined (MapOfAttributeDimension)
  }
derive instance newtypeSegmentDimensions :: Newtype SegmentDimensions _


-- | Segment import definition.
newtype SegmentImportResource = SegmentImportResource 
  { "ChannelCounts" :: NullOrUndefined (MapOf__integer)
  , "ExternalId" :: NullOrUndefined (String)
  , "Format" :: NullOrUndefined (Format)
  , "RoleArn" :: NullOrUndefined (String)
  , "S3Url" :: NullOrUndefined (String)
  , "Size" :: NullOrUndefined (Int)
  }
derive instance newtypeSegmentImportResource :: Newtype SegmentImportResource _


-- | Segment location dimensions
newtype SegmentLocation = SegmentLocation 
  { "Country" :: NullOrUndefined (SetDimension)
  }
derive instance newtypeSegmentLocation :: Newtype SegmentLocation _


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
derive instance newtypeSegmentResponse :: Newtype SegmentResponse _


newtype SegmentType = SegmentType String
derive instance newtypeSegmentType :: Newtype SegmentType _


-- | Segments in your account.
newtype SegmentsResponse = SegmentsResponse 
  { "Item" :: NullOrUndefined (ListOfSegmentResponse)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeSegmentsResponse :: Newtype SegmentsResponse _


newtype SendMessagesRequest = SendMessagesRequest 
  { "ApplicationId" :: (String)
  , "MessageRequest" :: (MessageRequest)
  }
derive instance newtypeSendMessagesRequest :: Newtype SendMessagesRequest _


newtype SendMessagesResponse = SendMessagesResponse 
  { "MessageResponse" :: (MessageResponse)
  }
derive instance newtypeSendMessagesResponse :: Newtype SendMessagesResponse _


-- | Send message request.
newtype SendUsersMessageRequest = SendUsersMessageRequest 
  { "Context" :: NullOrUndefined (MapOf__string)
  , "MessageConfiguration" :: NullOrUndefined (DirectMessageConfiguration)
  , "Users" :: NullOrUndefined (MapOfEndpointSendConfiguration)
  }
derive instance newtypeSendUsersMessageRequest :: Newtype SendUsersMessageRequest _


-- | User send message response.
newtype SendUsersMessageResponse = SendUsersMessageResponse 
  { "ApplicationId" :: NullOrUndefined (String)
  , "RequestId" :: NullOrUndefined (String)
  , "Result" :: NullOrUndefined (MapOfMapOfEndpointMessageResult)
  }
derive instance newtypeSendUsersMessageResponse :: Newtype SendUsersMessageResponse _


newtype SendUsersMessagesRequest = SendUsersMessagesRequest 
  { "ApplicationId" :: (String)
  , "SendUsersMessageRequest" :: (SendUsersMessageRequest)
  }
derive instance newtypeSendUsersMessagesRequest :: Newtype SendUsersMessagesRequest _


newtype SendUsersMessagesResponse = SendUsersMessagesResponse 
  { "SendUsersMessageResponse" :: (SendUsersMessageResponse)
  }
derive instance newtypeSendUsersMessagesResponse :: Newtype SendUsersMessagesResponse _


-- | Dimension specification of a segment.
newtype SetDimension = SetDimension 
  { "DimensionType" :: NullOrUndefined (DimensionType)
  , "Values" :: NullOrUndefined (ListOf__string)
  }
derive instance newtypeSetDimension :: Newtype SetDimension _


-- | Simple message object.
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  , "RequestID" :: NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


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
derive instance newtypeTreatmentResource :: Newtype TreatmentResource _


newtype UpdateAdmChannelRequest = UpdateAdmChannelRequest 
  { "ADMChannelRequest" :: (ADMChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateAdmChannelRequest :: Newtype UpdateAdmChannelRequest _


newtype UpdateAdmChannelResponse = UpdateAdmChannelResponse 
  { "ADMChannelResponse" :: (ADMChannelResponse)
  }
derive instance newtypeUpdateAdmChannelResponse :: Newtype UpdateAdmChannelResponse _


newtype UpdateApnsChannelRequest = UpdateApnsChannelRequest 
  { "APNSChannelRequest" :: (APNSChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateApnsChannelRequest :: Newtype UpdateApnsChannelRequest _


newtype UpdateApnsChannelResponse = UpdateApnsChannelResponse 
  { "APNSChannelResponse" :: (APNSChannelResponse)
  }
derive instance newtypeUpdateApnsChannelResponse :: Newtype UpdateApnsChannelResponse _


newtype UpdateApnsSandboxChannelRequest = UpdateApnsSandboxChannelRequest 
  { "APNSSandboxChannelRequest" :: (APNSSandboxChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateApnsSandboxChannelRequest :: Newtype UpdateApnsSandboxChannelRequest _


newtype UpdateApnsSandboxChannelResponse = UpdateApnsSandboxChannelResponse 
  { "APNSSandboxChannelResponse" :: (APNSSandboxChannelResponse)
  }
derive instance newtypeUpdateApnsSandboxChannelResponse :: Newtype UpdateApnsSandboxChannelResponse _


newtype UpdateApnsVoipChannelRequest = UpdateApnsVoipChannelRequest 
  { "APNSVoipChannelRequest" :: (APNSVoipChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateApnsVoipChannelRequest :: Newtype UpdateApnsVoipChannelRequest _


newtype UpdateApnsVoipChannelResponse = UpdateApnsVoipChannelResponse 
  { "APNSVoipChannelResponse" :: (APNSVoipChannelResponse)
  }
derive instance newtypeUpdateApnsVoipChannelResponse :: Newtype UpdateApnsVoipChannelResponse _


newtype UpdateApnsVoipSandboxChannelRequest = UpdateApnsVoipSandboxChannelRequest 
  { "APNSVoipSandboxChannelRequest" :: (APNSVoipSandboxChannelRequest)
  , "ApplicationId" :: (String)
  }
derive instance newtypeUpdateApnsVoipSandboxChannelRequest :: Newtype UpdateApnsVoipSandboxChannelRequest _


newtype UpdateApnsVoipSandboxChannelResponse = UpdateApnsVoipSandboxChannelResponse 
  { "APNSVoipSandboxChannelResponse" :: (APNSVoipSandboxChannelResponse)
  }
derive instance newtypeUpdateApnsVoipSandboxChannelResponse :: Newtype UpdateApnsVoipSandboxChannelResponse _


newtype UpdateApplicationSettingsRequest = UpdateApplicationSettingsRequest 
  { "ApplicationId" :: (String)
  , "WriteApplicationSettingsRequest" :: (WriteApplicationSettingsRequest)
  }
derive instance newtypeUpdateApplicationSettingsRequest :: Newtype UpdateApplicationSettingsRequest _


newtype UpdateApplicationSettingsResponse = UpdateApplicationSettingsResponse 
  { "ApplicationSettingsResource" :: (ApplicationSettingsResource)
  }
derive instance newtypeUpdateApplicationSettingsResponse :: Newtype UpdateApplicationSettingsResponse _


newtype UpdateBaiduChannelRequest = UpdateBaiduChannelRequest 
  { "ApplicationId" :: (String)
  , "BaiduChannelRequest" :: (BaiduChannelRequest)
  }
derive instance newtypeUpdateBaiduChannelRequest :: Newtype UpdateBaiduChannelRequest _


newtype UpdateBaiduChannelResponse = UpdateBaiduChannelResponse 
  { "BaiduChannelResponse" :: (BaiduChannelResponse)
  }
derive instance newtypeUpdateBaiduChannelResponse :: Newtype UpdateBaiduChannelResponse _


newtype UpdateCampaignRequest = UpdateCampaignRequest 
  { "ApplicationId" :: (String)
  , "CampaignId" :: (String)
  , "WriteCampaignRequest" :: (WriteCampaignRequest)
  }
derive instance newtypeUpdateCampaignRequest :: Newtype UpdateCampaignRequest _


newtype UpdateCampaignResponse = UpdateCampaignResponse 
  { "CampaignResponse" :: (CampaignResponse)
  }
derive instance newtypeUpdateCampaignResponse :: Newtype UpdateCampaignResponse _


newtype UpdateEmailChannelRequest = UpdateEmailChannelRequest 
  { "ApplicationId" :: (String)
  , "EmailChannelRequest" :: (EmailChannelRequest)
  }
derive instance newtypeUpdateEmailChannelRequest :: Newtype UpdateEmailChannelRequest _


newtype UpdateEmailChannelResponse = UpdateEmailChannelResponse 
  { "EmailChannelResponse" :: (EmailChannelResponse)
  }
derive instance newtypeUpdateEmailChannelResponse :: Newtype UpdateEmailChannelResponse _


newtype UpdateEndpointRequest = UpdateEndpointRequest 
  { "ApplicationId" :: (String)
  , "EndpointId" :: (String)
  , "EndpointRequest" :: (EndpointRequest)
  }
derive instance newtypeUpdateEndpointRequest :: Newtype UpdateEndpointRequest _


newtype UpdateEndpointResponse = UpdateEndpointResponse 
  { "MessageBody" :: (MessageBody)
  }
derive instance newtypeUpdateEndpointResponse :: Newtype UpdateEndpointResponse _


newtype UpdateEndpointsBatchRequest = UpdateEndpointsBatchRequest 
  { "ApplicationId" :: (String)
  , "EndpointBatchRequest" :: (EndpointBatchRequest)
  }
derive instance newtypeUpdateEndpointsBatchRequest :: Newtype UpdateEndpointsBatchRequest _


newtype UpdateEndpointsBatchResponse = UpdateEndpointsBatchResponse 
  { "MessageBody" :: (MessageBody)
  }
derive instance newtypeUpdateEndpointsBatchResponse :: Newtype UpdateEndpointsBatchResponse _


newtype UpdateGcmChannelRequest = UpdateGcmChannelRequest 
  { "ApplicationId" :: (String)
  , "GCMChannelRequest" :: (GCMChannelRequest)
  }
derive instance newtypeUpdateGcmChannelRequest :: Newtype UpdateGcmChannelRequest _


newtype UpdateGcmChannelResponse = UpdateGcmChannelResponse 
  { "GCMChannelResponse" :: (GCMChannelResponse)
  }
derive instance newtypeUpdateGcmChannelResponse :: Newtype UpdateGcmChannelResponse _


newtype UpdateSegmentRequest = UpdateSegmentRequest 
  { "ApplicationId" :: (String)
  , "SegmentId" :: (String)
  , "WriteSegmentRequest" :: (WriteSegmentRequest)
  }
derive instance newtypeUpdateSegmentRequest :: Newtype UpdateSegmentRequest _


newtype UpdateSegmentResponse = UpdateSegmentResponse 
  { "SegmentResponse" :: (SegmentResponse)
  }
derive instance newtypeUpdateSegmentResponse :: Newtype UpdateSegmentResponse _


newtype UpdateSmsChannelRequest = UpdateSmsChannelRequest 
  { "ApplicationId" :: (String)
  , "SMSChannelRequest" :: (SMSChannelRequest)
  }
derive instance newtypeUpdateSmsChannelRequest :: Newtype UpdateSmsChannelRequest _


newtype UpdateSmsChannelResponse = UpdateSmsChannelResponse 
  { "SMSChannelResponse" :: (SMSChannelResponse)
  }
derive instance newtypeUpdateSmsChannelResponse :: Newtype UpdateSmsChannelResponse _


-- | Creating application setting request
newtype WriteApplicationSettingsRequest = WriteApplicationSettingsRequest 
  { "Limits" :: NullOrUndefined (CampaignLimits)
  , "QuietTime" :: NullOrUndefined (QuietTime)
  }
derive instance newtypeWriteApplicationSettingsRequest :: Newtype WriteApplicationSettingsRequest _


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
derive instance newtypeWriteCampaignRequest :: Newtype WriteCampaignRequest _


-- | Request to save an EventStream.
newtype WriteEventStream = WriteEventStream 
  { "DestinationStreamArn" :: NullOrUndefined (String)
  , "RoleArn" :: NullOrUndefined (String)
  }
derive instance newtypeWriteEventStream :: Newtype WriteEventStream _


-- | Segment definition.
newtype WriteSegmentRequest = WriteSegmentRequest 
  { "Dimensions" :: NullOrUndefined (SegmentDimensions)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeWriteSegmentRequest :: Newtype WriteSegmentRequest _


-- | Used to create a campaign treatment.
newtype WriteTreatmentResource = WriteTreatmentResource 
  { "MessageConfiguration" :: NullOrUndefined (MessageConfiguration)
  , "Schedule" :: NullOrUndefined (Schedule)
  , "SizePercent" :: NullOrUndefined (Int)
  , "TreatmentDescription" :: NullOrUndefined (String)
  , "TreatmentName" :: NullOrUndefined (String)
  }
derive instance newtypeWriteTreatmentResource :: Newtype WriteTreatmentResource _
