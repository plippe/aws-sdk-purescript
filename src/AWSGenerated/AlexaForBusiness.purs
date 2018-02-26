

-- | <p>Alexa for Business makes it easy for you to use Alexa in your organization. Alexa for Business gives you the tools you need to manage Alexa devices, enroll your users, and assign skills, at scale. You can build your own context-aware voice skills using the Alexa Skills Kit, and the Alexa for Business APIs, and you can make these available as private skills for your organization. Alexa for Business also makes it easy to voice-enable your products and services, providing context-aware voice experiences for your customers.</p>
module AWS.AlexaForBusiness where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "AlexaForBusiness" :: String


-- | <p>Associates a device to a given room. This applies all the settings from the room profile to the device, and all the skills in any skill groups added to that room. This operation requires the device to be online, or a manual sync is required. </p>
associateDeviceWithRoom :: forall eff. AssociateDeviceWithRoomRequest -> Aff (err :: AWS.RequestError | eff) AssociateDeviceWithRoomResponse
associateDeviceWithRoom = AWS.request serviceName "AssociateDeviceWithRoom" 


-- | <p>Associates a skill group to a given room. This enables all skills in the associated skill group on all devices in the room.</p>
associateSkillGroupWithRoom :: forall eff. AssociateSkillGroupWithRoomRequest -> Aff (err :: AWS.RequestError | eff) AssociateSkillGroupWithRoomResponse
associateSkillGroupWithRoom = AWS.request serviceName "AssociateSkillGroupWithRoom" 


-- | <p>Creates a new room profile with the specified details.</p>
createProfile :: forall eff. CreateProfileRequest -> Aff (err :: AWS.RequestError | eff) CreateProfileResponse
createProfile = AWS.request serviceName "CreateProfile" 


-- | <p>Creates a room with the specified details.</p>
createRoom :: forall eff. CreateRoomRequest -> Aff (err :: AWS.RequestError | eff) CreateRoomResponse
createRoom = AWS.request serviceName "CreateRoom" 


-- | <p>Creates a skill group with a specified name and description.</p>
createSkillGroup :: forall eff. CreateSkillGroupRequest -> Aff (err :: AWS.RequestError | eff) CreateSkillGroupResponse
createSkillGroup = AWS.request serviceName "CreateSkillGroup" 


-- | <p>Creates a user.</p>
createUser :: forall eff. CreateUserRequest -> Aff (err :: AWS.RequestError | eff) CreateUserResponse
createUser = AWS.request serviceName "CreateUser" 


-- | <p>Deletes a room profile by the profile ARN.</p>
deleteProfile :: forall eff. DeleteProfileRequest -> Aff (err :: AWS.RequestError | eff) DeleteProfileResponse
deleteProfile = AWS.request serviceName "DeleteProfile" 


-- | <p>Deletes a room by the room ARN.</p>
deleteRoom :: forall eff. DeleteRoomRequest -> Aff (err :: AWS.RequestError | eff) DeleteRoomResponse
deleteRoom = AWS.request serviceName "DeleteRoom" 


-- | <p>Deletes room skill parameter details by room, skill, and parameter key ID.</p>
deleteRoomSkillParameter :: forall eff. DeleteRoomSkillParameterRequest -> Aff (err :: AWS.RequestError | eff) DeleteRoomSkillParameterResponse
deleteRoomSkillParameter = AWS.request serviceName "DeleteRoomSkillParameter" 


-- | <p>Deletes a skill group by skill group ARN.</p>
deleteSkillGroup :: forall eff. DeleteSkillGroupRequest -> Aff (err :: AWS.RequestError | eff) DeleteSkillGroupResponse
deleteSkillGroup = AWS.request serviceName "DeleteSkillGroup" 


-- | <p>Deletes a specified user by user ARN and enrollment ARN.</p>
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: AWS.RequestError | eff) DeleteUserResponse
deleteUser = AWS.request serviceName "DeleteUser" 


-- | <p>Disassociates a device from its current room. The device continues to be connected to the Wi-Fi network and is still registered to the account. The device settings and skills are removed from the room.</p>
disassociateDeviceFromRoom :: forall eff. DisassociateDeviceFromRoomRequest -> Aff (err :: AWS.RequestError | eff) DisassociateDeviceFromRoomResponse
disassociateDeviceFromRoom = AWS.request serviceName "DisassociateDeviceFromRoom" 


-- | <p>Disassociates a skill group from a specified room. This disables all skills in the skill group on all devices in the room.</p>
disassociateSkillGroupFromRoom :: forall eff. DisassociateSkillGroupFromRoomRequest -> Aff (err :: AWS.RequestError | eff) DisassociateSkillGroupFromRoomResponse
disassociateSkillGroupFromRoom = AWS.request serviceName "DisassociateSkillGroupFromRoom" 


-- | <p>Gets the details of a device by device ARN.</p>
getDevice :: forall eff. GetDeviceRequest -> Aff (err :: AWS.RequestError | eff) GetDeviceResponse
getDevice = AWS.request serviceName "GetDevice" 


-- | <p>Gets the details of a room profile by profile ARN.</p>
getProfile :: forall eff. GetProfileRequest -> Aff (err :: AWS.RequestError | eff) GetProfileResponse
getProfile = AWS.request serviceName "GetProfile" 


-- | <p>Gets room details by room ARN.</p>
getRoom :: forall eff. GetRoomRequest -> Aff (err :: AWS.RequestError | eff) GetRoomResponse
getRoom = AWS.request serviceName "GetRoom" 


-- | <p>Gets room skill parameter details by room, skill, and parameter key ARN.</p>
getRoomSkillParameter :: forall eff. GetRoomSkillParameterRequest -> Aff (err :: AWS.RequestError | eff) GetRoomSkillParameterResponse
getRoomSkillParameter = AWS.request serviceName "GetRoomSkillParameter" 


-- | <p>Gets skill group details by skill group ARN.</p>
getSkillGroup :: forall eff. GetSkillGroupRequest -> Aff (err :: AWS.RequestError | eff) GetSkillGroupResponse
getSkillGroup = AWS.request serviceName "GetSkillGroup" 


-- | <p>Lists all enabled skills in a specific skill group.</p>
listSkills :: forall eff. ListSkillsRequest -> Aff (err :: AWS.RequestError | eff) ListSkillsResponse
listSkills = AWS.request serviceName "ListSkills" 


-- | <p>Lists all tags for a specific resource.</p>
listTags :: forall eff. ListTagsRequest -> Aff (err :: AWS.RequestError | eff) ListTagsResponse
listTags = AWS.request serviceName "ListTags" 


-- | <p>Updates room skill parameter details by room, skill, and parameter key ID. Not all skills have a room skill parameter.</p>
putRoomSkillParameter :: forall eff. PutRoomSkillParameterRequest -> Aff (err :: AWS.RequestError | eff) PutRoomSkillParameterResponse
putRoomSkillParameter = AWS.request serviceName "PutRoomSkillParameter" 


-- | <p>Determines the details for the room from which a skill request was invoked. This operation is used by skill developers.</p>
resolveRoom :: forall eff. ResolveRoomRequest -> Aff (err :: AWS.RequestError | eff) ResolveRoomResponse
resolveRoom = AWS.request serviceName "ResolveRoom" 


-- | <p>Revokes an invitation and invalidates the enrollment URL.</p>
revokeInvitation :: forall eff. RevokeInvitationRequest -> Aff (err :: AWS.RequestError | eff) RevokeInvitationResponse
revokeInvitation = AWS.request serviceName "RevokeInvitation" 


-- | <p>Searches devices and lists the ones that meet a set of filter criteria.</p>
searchDevices :: forall eff. SearchDevicesRequest -> Aff (err :: AWS.RequestError | eff) SearchDevicesResponse
searchDevices = AWS.request serviceName "SearchDevices" 


-- | <p>Searches room profiles and lists the ones that meet a set of filter criteria.</p>
searchProfiles :: forall eff. SearchProfilesRequest -> Aff (err :: AWS.RequestError | eff) SearchProfilesResponse
searchProfiles = AWS.request serviceName "SearchProfiles" 


-- | <p>Searches rooms and lists the ones that meet a set of filter and sort criteria.</p>
searchRooms :: forall eff. SearchRoomsRequest -> Aff (err :: AWS.RequestError | eff) SearchRoomsResponse
searchRooms = AWS.request serviceName "SearchRooms" 


-- | <p>Searches skill groups and lists the ones that meet a set of filter and sort criteria.</p>
searchSkillGroups :: forall eff. SearchSkillGroupsRequest -> Aff (err :: AWS.RequestError | eff) SearchSkillGroupsResponse
searchSkillGroups = AWS.request serviceName "SearchSkillGroups" 


-- | <p>Searches users and lists the ones that meet a set of filter and sort criteria.</p>
searchUsers :: forall eff. SearchUsersRequest -> Aff (err :: AWS.RequestError | eff) SearchUsersResponse
searchUsers = AWS.request serviceName "SearchUsers" 


-- | <p>Sends an enrollment invitation email with a URL to a user. The URL is valid for 72 hours or until you call this operation again, whichever comes first. </p>
sendInvitation :: forall eff. SendInvitationRequest -> Aff (err :: AWS.RequestError | eff) SendInvitationResponse
sendInvitation = AWS.request serviceName "SendInvitation" 


-- | <p>Resets a device and its account to the known default settings by clearing all information and settings set by previous users.</p>
startDeviceSync :: forall eff. StartDeviceSyncRequest -> Aff (err :: AWS.RequestError | eff) StartDeviceSyncResponse
startDeviceSync = AWS.request serviceName "StartDeviceSync" 


-- | <p>Adds metadata tags to a specified resource.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (err :: AWS.RequestError | eff) TagResourceResponse
tagResource = AWS.request serviceName "TagResource" 


-- | <p>Removes metadata tags from a specified resource.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: AWS.RequestError | eff) UntagResourceResponse
untagResource = AWS.request serviceName "UntagResource" 


-- | <p>Updates the device name by device ARN.</p>
updateDevice :: forall eff. UpdateDeviceRequest -> Aff (err :: AWS.RequestError | eff) UpdateDeviceResponse
updateDevice = AWS.request serviceName "UpdateDevice" 


-- | <p>Updates an existing room profile by room profile ARN.</p>
updateProfile :: forall eff. UpdateProfileRequest -> Aff (err :: AWS.RequestError | eff) UpdateProfileResponse
updateProfile = AWS.request serviceName "UpdateProfile" 


-- | <p>Updates room details by room ARN.</p>
updateRoom :: forall eff. UpdateRoomRequest -> Aff (err :: AWS.RequestError | eff) UpdateRoomResponse
updateRoom = AWS.request serviceName "UpdateRoom" 


-- | <p>Updates skill group details by skill group ARN.</p>
updateSkillGroup :: forall eff. UpdateSkillGroupRequest -> Aff (err :: AWS.RequestError | eff) UpdateSkillGroupResponse
updateSkillGroup = AWS.request serviceName "UpdateSkillGroup" 


newtype Address = Address String


-- | <p>The resource being created already exists. HTTP Status Code: 400</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype Arn = Arn String


newtype AssociateDeviceWithRoomRequest = AssociateDeviceWithRoomRequest 
  { "DeviceArn" :: NullOrUndefined (Arn)
  , "RoomArn" :: NullOrUndefined (Arn)
  }


newtype AssociateDeviceWithRoomResponse = AssociateDeviceWithRoomResponse 
  { 
  }


newtype AssociateSkillGroupWithRoomRequest = AssociateSkillGroupWithRoomRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "RoomArn" :: NullOrUndefined (Arn)
  }


newtype AssociateSkillGroupWithRoomResponse = AssociateSkillGroupWithRoomResponse 
  { 
  }


-- | User specified token that is used to support idempotency during Create Resource
newtype ClientRequestToken = ClientRequestToken String


newtype CreateProfileRequest = CreateProfileRequest 
  { "ProfileName" :: (ProfileName)
  , "Timezone" :: (Timezone)
  , "Address" :: (Address)
  , "DistanceUnit" :: (DistanceUnit)
  , "TemperatureUnit" :: (TemperatureUnit)
  , "WakeWord" :: (WakeWord)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "SetupModeDisabled" :: NullOrUndefined (Boolean)
  , "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit)
  , "PSTNEnabled" :: NullOrUndefined (Boolean)
  }


newtype CreateProfileResponse = CreateProfileResponse 
  { "ProfileArn" :: NullOrUndefined (Arn)
  }


newtype CreateRoomRequest = CreateRoomRequest 
  { "RoomName" :: (RoomName)
  , "Description" :: NullOrUndefined (RoomDescription)
  , "ProfileArn" :: NullOrUndefined (Arn)
  , "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "Tags" :: NullOrUndefined (TagList)
  }


newtype CreateRoomResponse = CreateRoomResponse 
  { "RoomArn" :: NullOrUndefined (Arn)
  }


newtype CreateSkillGroupRequest = CreateSkillGroupRequest 
  { "SkillGroupName" :: (SkillGroupName)
  , "Description" :: NullOrUndefined (SkillGroupDescription)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }


newtype CreateSkillGroupResponse = CreateSkillGroupResponse 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  }


newtype CreateUserRequest = CreateUserRequest 
  { "UserId" :: (User_UserId')
  , "FirstName" :: NullOrUndefined (User_FirstName')
  , "LastName" :: NullOrUndefined (User_LastName')
  , "Email" :: NullOrUndefined (Email)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "Tags" :: NullOrUndefined (TagList)
  }


newtype CreateUserResponse = CreateUserResponse 
  { "UserArn" :: NullOrUndefined (Arn)
  }


newtype DeleteProfileRequest = DeleteProfileRequest 
  { "ProfileArn" :: NullOrUndefined (Arn)
  }


newtype DeleteProfileResponse = DeleteProfileResponse 
  { 
  }


newtype DeleteRoomRequest = DeleteRoomRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  }


newtype DeleteRoomResponse = DeleteRoomResponse 
  { 
  }


newtype DeleteRoomSkillParameterRequest = DeleteRoomSkillParameterRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "SkillId" :: (SkillId)
  , "ParameterKey" :: (RoomSkillParameterKey)
  }


newtype DeleteRoomSkillParameterResponse = DeleteRoomSkillParameterResponse 
  { 
  }


newtype DeleteSkillGroupRequest = DeleteSkillGroupRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  }


newtype DeleteSkillGroupResponse = DeleteSkillGroupResponse 
  { 
  }


newtype DeleteUserRequest = DeleteUserRequest 
  { "UserArn" :: NullOrUndefined (Arn)
  , "EnrollmentId" :: (EnrollmentId)
  }


newtype DeleteUserResponse = DeleteUserResponse 
  { 
  }


-- | <p>A device with attributes.</p>
newtype Device = Device 
  { "DeviceArn" :: NullOrUndefined (Arn)
  , "DeviceSerialNumber" :: NullOrUndefined (DeviceSerialNumber)
  , "DeviceType" :: NullOrUndefined (DeviceType)
  , "DeviceName" :: NullOrUndefined (DeviceName)
  , "SoftwareVersion" :: NullOrUndefined (SoftwareVersion)
  , "MacAddress" :: NullOrUndefined (MacAddress)
  , "RoomArn" :: NullOrUndefined (Arn)
  , "DeviceStatus" :: NullOrUndefined (DeviceStatus)
  , "DeviceStatusInfo" :: NullOrUndefined (DeviceStatusInfo)
  }


-- | <p>Device attributes.</p>
newtype DeviceData = DeviceData 
  { "DeviceArn" :: NullOrUndefined (Arn)
  , "DeviceSerialNumber" :: NullOrUndefined (DeviceSerialNumber)
  , "DeviceType" :: NullOrUndefined (DeviceType)
  , "DeviceName" :: NullOrUndefined (DeviceName)
  , "SoftwareVersion" :: NullOrUndefined (SoftwareVersion)
  , "MacAddress" :: NullOrUndefined (MacAddress)
  , "DeviceStatus" :: NullOrUndefined (DeviceStatus)
  , "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "DeviceStatusInfo" :: NullOrUndefined (DeviceStatusInfo)
  }


newtype DeviceDataList = DeviceDataList (Array DeviceData)


newtype DeviceName = DeviceName String


newtype DeviceSerialNumber = DeviceSerialNumber String


newtype DeviceStatus = DeviceStatus String


-- | <p>Details of a device’s status.</p>
newtype DeviceStatusDetail = DeviceStatusDetail 
  { "Code" :: NullOrUndefined (DeviceStatusDetailCode)
  }


newtype DeviceStatusDetailCode = DeviceStatusDetailCode String


newtype DeviceStatusDetails = DeviceStatusDetails (Array DeviceStatusDetail)


-- | <p>Detailed information about a device's status.</p>
newtype DeviceStatusInfo = DeviceStatusInfo 
  { "DeviceStatusDetails" :: NullOrUndefined (DeviceStatusDetails)
  }


newtype DeviceType = DeviceType String


newtype DisassociateDeviceFromRoomRequest = DisassociateDeviceFromRoomRequest 
  { "DeviceArn" :: NullOrUndefined (Arn)
  }


newtype DisassociateDeviceFromRoomResponse = DisassociateDeviceFromRoomResponse 
  { 
  }


newtype DisassociateSkillGroupFromRoomRequest = DisassociateSkillGroupFromRoomRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "RoomArn" :: NullOrUndefined (Arn)
  }


newtype DisassociateSkillGroupFromRoomResponse = DisassociateSkillGroupFromRoomResponse 
  { 
  }


newtype DistanceUnit = DistanceUnit String


newtype Email = Email String


newtype EnrollmentId = EnrollmentId String


newtype EnrollmentStatus = EnrollmentStatus String


newtype ErrorMessage = ErrorMessage String


newtype Feature = Feature String


newtype Features = Features (Array Feature)


-- | <p>A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria.</p>
newtype Filter = Filter 
  { "Key" :: (FilterKey)
  , "Values" :: (FilterValueList)
  }


newtype FilterKey = FilterKey String


newtype FilterList = FilterList (Array Filter)


newtype FilterValue = FilterValue String


newtype FilterValueList = FilterValueList (Array FilterValue)


newtype GetDeviceRequest = GetDeviceRequest 
  { "DeviceArn" :: NullOrUndefined (Arn)
  }


newtype GetDeviceResponse = GetDeviceResponse 
  { "Device" :: NullOrUndefined (Device)
  }


newtype GetProfileRequest = GetProfileRequest 
  { "ProfileArn" :: NullOrUndefined (Arn)
  }


newtype GetProfileResponse = GetProfileResponse 
  { "Profile" :: NullOrUndefined (Profile)
  }


newtype GetRoomRequest = GetRoomRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  }


newtype GetRoomResponse = GetRoomResponse 
  { "Room" :: NullOrUndefined (Room)
  }


newtype GetRoomSkillParameterRequest = GetRoomSkillParameterRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "SkillId" :: (SkillId)
  , "ParameterKey" :: (RoomSkillParameterKey)
  }


newtype GetRoomSkillParameterResponse = GetRoomSkillParameterResponse 
  { "RoomSkillParameter" :: NullOrUndefined (RoomSkillParameter)
  }


newtype GetSkillGroupRequest = GetSkillGroupRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  }


newtype GetSkillGroupResponse = GetSkillGroupResponse 
  { "SkillGroup" :: NullOrUndefined (SkillGroup)
  }


-- | <p>The attempt to update a user is invalid due to the user's current status. HTTP Status Code: 400</p>
newtype InvalidUserStatusException = InvalidUserStatusException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>You are performing an action that would put you beyond your account's limits. HTTP Status Code: 400</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ListSkillsRequest = ListSkillsRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (SkillListMaxResults)
  }


newtype ListSkillsResponse = ListSkillsResponse 
  { "SkillSummaries" :: NullOrUndefined (SkillSummaryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListTagsRequest = ListTagsRequest 
  { "Arn" :: (Arn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListTagsResponse = ListTagsResponse 
  { "Tags" :: NullOrUndefined (TagList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype MacAddress = MacAddress String


newtype MaxResults = MaxResults Int


newtype MaxVolumeLimit = MaxVolumeLimit Int


-- | <p>The name sent in the request is already in use. HTTP Status Code: 400</p>
newtype NameInUseException = NameInUseException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype NextToken = NextToken String


-- | <p>The resource is not found. HTTP Status Code: 400</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>A room profile with attributes.</p>
newtype Profile = Profile 
  { "ProfileArn" :: NullOrUndefined (Arn)
  , "ProfileName" :: NullOrUndefined (ProfileName)
  , "Address" :: NullOrUndefined (Address)
  , "Timezone" :: NullOrUndefined (Timezone)
  , "DistanceUnit" :: NullOrUndefined (DistanceUnit)
  , "TemperatureUnit" :: NullOrUndefined (TemperatureUnit)
  , "WakeWord" :: NullOrUndefined (WakeWord)
  , "SetupModeDisabled" :: NullOrUndefined (Boolean)
  , "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit)
  , "PSTNEnabled" :: NullOrUndefined (Boolean)
  }


-- | <p>The data of a room profile.</p>
newtype ProfileData = ProfileData 
  { "ProfileArn" :: NullOrUndefined (Arn)
  , "ProfileName" :: NullOrUndefined (ProfileName)
  , "Address" :: NullOrUndefined (Address)
  , "Timezone" :: NullOrUndefined (Timezone)
  , "DistanceUnit" :: NullOrUndefined (DistanceUnit)
  , "TemperatureUnit" :: NullOrUndefined (TemperatureUnit)
  , "WakeWord" :: NullOrUndefined (WakeWord)
  }


newtype ProfileDataList = ProfileDataList (Array ProfileData)


newtype ProfileName = ProfileName String


newtype ProviderCalendarId = ProviderCalendarId String


newtype PutRoomSkillParameterRequest = PutRoomSkillParameterRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "SkillId" :: (SkillId)
  , "RoomSkillParameter" :: (RoomSkillParameter)
  }


newtype PutRoomSkillParameterResponse = PutRoomSkillParameterResponse 
  { 
  }


newtype ResolveRoomRequest = ResolveRoomRequest 
  { "UserId" :: (UserId)
  , "SkillId" :: (SkillId)
  }


newtype ResolveRoomResponse = ResolveRoomResponse 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "RoomSkillParameters" :: NullOrUndefined (RoomSkillParameters)
  }


-- | <p>The resource in the request is already in use. HTTP Status Code: 400</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }


newtype RevokeInvitationRequest = RevokeInvitationRequest 
  { "UserArn" :: NullOrUndefined (Arn)
  , "EnrollmentId" :: NullOrUndefined (EnrollmentId)
  }


newtype RevokeInvitationResponse = RevokeInvitationResponse 
  { 
  }


-- | <p>A room with attributes.</p>
newtype Room = Room 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "Description" :: NullOrUndefined (RoomDescription)
  , "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId)
  , "ProfileArn" :: NullOrUndefined (Arn)
  }


-- | <p>The data of a room.</p>
newtype RoomData = RoomData 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "Description" :: NullOrUndefined (RoomDescription)
  , "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId)
  , "ProfileArn" :: NullOrUndefined (Arn)
  , "ProfileName" :: NullOrUndefined (ProfileName)
  }


newtype RoomDataList = RoomDataList (Array RoomData)


newtype RoomDescription = RoomDescription String


newtype RoomName = RoomName String


-- | <p>A skill parameter associated with a room.</p>
newtype RoomSkillParameter = RoomSkillParameter 
  { "ParameterKey" :: (RoomSkillParameterKey)
  , "ParameterValue" :: (RoomSkillParameterValue)
  }


newtype RoomSkillParameterKey = RoomSkillParameterKey String


newtype RoomSkillParameterValue = RoomSkillParameterValue String


newtype RoomSkillParameters = RoomSkillParameters (Array RoomSkillParameter)


newtype SearchDevicesRequest = SearchDevicesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }


newtype SearchDevicesResponse = SearchDevicesResponse 
  { "Devices" :: NullOrUndefined (DeviceDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }


newtype SearchProfilesRequest = SearchProfilesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }


newtype SearchProfilesResponse = SearchProfilesResponse 
  { "Profiles" :: NullOrUndefined (ProfileDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }


newtype SearchRoomsRequest = SearchRoomsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }


newtype SearchRoomsResponse = SearchRoomsResponse 
  { "Rooms" :: NullOrUndefined (RoomDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }


newtype SearchSkillGroupsRequest = SearchSkillGroupsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }


newtype SearchSkillGroupsResponse = SearchSkillGroupsResponse 
  { "SkillGroups" :: NullOrUndefined (SkillGroupDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }


newtype SearchUsersRequest = SearchUsersRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (FilterList)
  , "SortCriteria" :: NullOrUndefined (SortList)
  }


newtype SearchUsersResponse = SearchUsersResponse 
  { "Users" :: NullOrUndefined (UserDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TotalCount" :: NullOrUndefined (TotalCount)
  }


newtype SendInvitationRequest = SendInvitationRequest 
  { "UserArn" :: NullOrUndefined (Arn)
  }


newtype SendInvitationResponse = SendInvitationResponse 
  { 
  }


-- | <p>A skill group with attributes.</p>
newtype SkillGroup = SkillGroup 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "SkillGroupName" :: NullOrUndefined (SkillGroupName)
  , "Description" :: NullOrUndefined (SkillGroupDescription)
  }


-- | <p>The attributes of a skill group.</p>
newtype SkillGroupData = SkillGroupData 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "SkillGroupName" :: NullOrUndefined (SkillGroupName)
  , "Description" :: NullOrUndefined (SkillGroupDescription)
  }


newtype SkillGroupDataList = SkillGroupDataList (Array SkillGroupData)


newtype SkillGroupDescription = SkillGroupDescription String


newtype SkillGroupName = SkillGroupName String


newtype SkillId = SkillId String


newtype SkillListMaxResults = SkillListMaxResults Int


newtype SkillName = SkillName String


-- | <p>The summary of skills.</p>
newtype SkillSummary = SkillSummary 
  { "SkillId" :: NullOrUndefined (SkillId)
  , "SkillName" :: NullOrUndefined (SkillName)
  , "SupportsLinking" :: NullOrUndefined (Boolean)
  }


newtype SkillSummaryList = SkillSummaryList (Array SkillSummary)


newtype SoftwareVersion = SoftwareVersion String


-- | <p>An object representing a sort criteria. </p>
newtype Sort = Sort 
  { "Key" :: (SortKey)
  , "Value" :: (SortValue)
  }


newtype SortKey = SortKey String


newtype SortList = SortList (Array Sort)


newtype SortValue = SortValue String


newtype StartDeviceSyncRequest = StartDeviceSyncRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "DeviceArn" :: NullOrUndefined (Arn)
  , "Features" :: (Features)
  }


newtype StartDeviceSyncResponse = StartDeviceSyncResponse 
  { 
  }


-- | <p>A key-value pair that can be associated with a resource. </p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagList = TagList (Array Tag)


newtype TagResourceRequest = TagResourceRequest 
  { "Arn" :: (Arn)
  , "Tags" :: (TagList)
  }


newtype TagResourceResponse = TagResourceResponse 
  { 
  }


newtype TagValue = TagValue String


newtype TemperatureUnit = TemperatureUnit String


newtype Timezone = Timezone String


newtype TotalCount = TotalCount Int


newtype UntagResourceRequest = UntagResourceRequest 
  { "Arn" :: (Arn)
  , "TagKeys" :: (TagKeyList)
  }


newtype UntagResourceResponse = UntagResourceResponse 
  { 
  }


newtype UpdateDeviceRequest = UpdateDeviceRequest 
  { "DeviceArn" :: NullOrUndefined (Arn)
  , "DeviceName" :: NullOrUndefined (DeviceName)
  }


newtype UpdateDeviceResponse = UpdateDeviceResponse 
  { 
  }


newtype UpdateProfileRequest = UpdateProfileRequest 
  { "ProfileArn" :: NullOrUndefined (Arn)
  , "ProfileName" :: NullOrUndefined (ProfileName)
  , "Timezone" :: NullOrUndefined (Timezone)
  , "Address" :: NullOrUndefined (Address)
  , "DistanceUnit" :: NullOrUndefined (DistanceUnit)
  , "TemperatureUnit" :: NullOrUndefined (TemperatureUnit)
  , "WakeWord" :: NullOrUndefined (WakeWord)
  , "SetupModeDisabled" :: NullOrUndefined (Boolean)
  , "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit)
  , "PSTNEnabled" :: NullOrUndefined (Boolean)
  }


newtype UpdateProfileResponse = UpdateProfileResponse 
  { 
  }


newtype UpdateRoomRequest = UpdateRoomRequest 
  { "RoomArn" :: NullOrUndefined (Arn)
  , "RoomName" :: NullOrUndefined (RoomName)
  , "Description" :: NullOrUndefined (RoomDescription)
  , "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId)
  , "ProfileArn" :: NullOrUndefined (Arn)
  }


newtype UpdateRoomResponse = UpdateRoomResponse 
  { 
  }


newtype UpdateSkillGroupRequest = UpdateSkillGroupRequest 
  { "SkillGroupArn" :: NullOrUndefined (Arn)
  , "SkillGroupName" :: NullOrUndefined (SkillGroupName)
  , "Description" :: NullOrUndefined (SkillGroupDescription)
  }


newtype UpdateSkillGroupResponse = UpdateSkillGroupResponse 
  { 
  }


-- | <p>Information related to a user.</p>
newtype UserData = UserData 
  { "UserArn" :: NullOrUndefined (Arn)
  , "FirstName" :: NullOrUndefined (User_FirstName')
  , "LastName" :: NullOrUndefined (User_LastName')
  , "Email" :: NullOrUndefined (Email)
  , "EnrollmentStatus" :: NullOrUndefined (EnrollmentStatus)
  , "EnrollmentId" :: NullOrUndefined (EnrollmentId)
  }


newtype UserDataList = UserDataList (Array UserData)


newtype UserId = UserId String


newtype WakeWord = WakeWord String


newtype User_FirstName' = User_FirstName' String


newtype User_LastName' = User_LastName' String


newtype User_UserId' = User_UserId' String
