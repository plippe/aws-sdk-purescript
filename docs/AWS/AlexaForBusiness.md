## Module AWS.AlexaForBusiness

<p>Alexa for Business makes it easy for you to use Alexa in your organization. Alexa for Business gives you the tools you need to manage Alexa devices, enroll your users, and assign skills, at scale. You can build your own context-aware voice skills using the Alexa Skills Kit, and the Alexa for Business APIs, and you can make these available as private skills for your organization. Alexa for Business also makes it easy to voice-enable your products and services, providing context-aware voice experiences for your customers.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateDeviceWithRoom`

``` purescript
associateDeviceWithRoom :: forall eff. AssociateDeviceWithRoomRequest -> Aff (err :: RequestError | eff) AssociateDeviceWithRoomResponse
```

<p>Associates a device to a given room. This applies all the settings from the room profile to the device, and all the skills in any skill groups added to that room. This operation requires the device to be online, or a manual sync is required. </p>

#### `associateSkillGroupWithRoom`

``` purescript
associateSkillGroupWithRoom :: forall eff. AssociateSkillGroupWithRoomRequest -> Aff (err :: RequestError | eff) AssociateSkillGroupWithRoomResponse
```

<p>Associates a skill group to a given room. This enables all skills in the associated skill group on all devices in the room.</p>

#### `createProfile`

``` purescript
createProfile :: forall eff. CreateProfileRequest -> Aff (err :: RequestError | eff) CreateProfileResponse
```

<p>Creates a new room profile with the specified details.</p>

#### `createRoom`

``` purescript
createRoom :: forall eff. CreateRoomRequest -> Aff (err :: RequestError | eff) CreateRoomResponse
```

<p>Creates a room with the specified details.</p>

#### `createSkillGroup`

``` purescript
createSkillGroup :: forall eff. CreateSkillGroupRequest -> Aff (err :: RequestError | eff) CreateSkillGroupResponse
```

<p>Creates a skill group with a specified name and description.</p>

#### `createUser`

``` purescript
createUser :: forall eff. CreateUserRequest -> Aff (err :: RequestError | eff) CreateUserResponse
```

<p>Creates a user.</p>

#### `deleteProfile`

``` purescript
deleteProfile :: forall eff. DeleteProfileRequest -> Aff (err :: RequestError | eff) DeleteProfileResponse
```

<p>Deletes a room profile by the profile ARN.</p>

#### `deleteRoom`

``` purescript
deleteRoom :: forall eff. DeleteRoomRequest -> Aff (err :: RequestError | eff) DeleteRoomResponse
```

<p>Deletes a room by the room ARN.</p>

#### `deleteRoomSkillParameter`

``` purescript
deleteRoomSkillParameter :: forall eff. DeleteRoomSkillParameterRequest -> Aff (err :: RequestError | eff) DeleteRoomSkillParameterResponse
```

<p>Deletes room skill parameter details by room, skill, and parameter key ID.</p>

#### `deleteSkillGroup`

``` purescript
deleteSkillGroup :: forall eff. DeleteSkillGroupRequest -> Aff (err :: RequestError | eff) DeleteSkillGroupResponse
```

<p>Deletes a skill group by skill group ARN.</p>

#### `deleteUser`

``` purescript
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: RequestError | eff) DeleteUserResponse
```

<p>Deletes a specified user by user ARN and enrollment ARN.</p>

#### `disassociateDeviceFromRoom`

``` purescript
disassociateDeviceFromRoom :: forall eff. DisassociateDeviceFromRoomRequest -> Aff (err :: RequestError | eff) DisassociateDeviceFromRoomResponse
```

<p>Disassociates a device from its current room. The device continues to be connected to the Wi-Fi network and is still registered to the account. The device settings and skills are removed from the room.</p>

#### `disassociateSkillGroupFromRoom`

``` purescript
disassociateSkillGroupFromRoom :: forall eff. DisassociateSkillGroupFromRoomRequest -> Aff (err :: RequestError | eff) DisassociateSkillGroupFromRoomResponse
```

<p>Disassociates a skill group from a specified room. This disables all skills in the skill group on all devices in the room.</p>

#### `getDevice`

``` purescript
getDevice :: forall eff. GetDeviceRequest -> Aff (err :: RequestError | eff) GetDeviceResponse
```

<p>Gets the details of a device by device ARN.</p>

#### `getProfile`

``` purescript
getProfile :: forall eff. GetProfileRequest -> Aff (err :: RequestError | eff) GetProfileResponse
```

<p>Gets the details of a room profile by profile ARN.</p>

#### `getRoom`

``` purescript
getRoom :: forall eff. GetRoomRequest -> Aff (err :: RequestError | eff) GetRoomResponse
```

<p>Gets room details by room ARN.</p>

#### `getRoomSkillParameter`

``` purescript
getRoomSkillParameter :: forall eff. GetRoomSkillParameterRequest -> Aff (err :: RequestError | eff) GetRoomSkillParameterResponse
```

<p>Gets room skill parameter details by room, skill, and parameter key ARN.</p>

#### `getSkillGroup`

``` purescript
getSkillGroup :: forall eff. GetSkillGroupRequest -> Aff (err :: RequestError | eff) GetSkillGroupResponse
```

<p>Gets skill group details by skill group ARN.</p>

#### `listSkills`

``` purescript
listSkills :: forall eff. ListSkillsRequest -> Aff (err :: RequestError | eff) ListSkillsResponse
```

<p>Lists all enabled skills in a specific skill group.</p>

#### `listTags`

``` purescript
listTags :: forall eff. ListTagsRequest -> Aff (err :: RequestError | eff) ListTagsResponse
```

<p>Lists all tags for a specific resource.</p>

#### `putRoomSkillParameter`

``` purescript
putRoomSkillParameter :: forall eff. PutRoomSkillParameterRequest -> Aff (err :: RequestError | eff) PutRoomSkillParameterResponse
```

<p>Updates room skill parameter details by room, skill, and parameter key ID. Not all skills have a room skill parameter.</p>

#### `resolveRoom`

``` purescript
resolveRoom :: forall eff. ResolveRoomRequest -> Aff (err :: RequestError | eff) ResolveRoomResponse
```

<p>Determines the details for the room from which a skill request was invoked. This operation is used by skill developers.</p>

#### `revokeInvitation`

``` purescript
revokeInvitation :: forall eff. RevokeInvitationRequest -> Aff (err :: RequestError | eff) RevokeInvitationResponse
```

<p>Revokes an invitation and invalidates the enrollment URL.</p>

#### `searchDevices`

``` purescript
searchDevices :: forall eff. SearchDevicesRequest -> Aff (err :: RequestError | eff) SearchDevicesResponse
```

<p>Searches devices and lists the ones that meet a set of filter criteria.</p>

#### `searchProfiles`

``` purescript
searchProfiles :: forall eff. SearchProfilesRequest -> Aff (err :: RequestError | eff) SearchProfilesResponse
```

<p>Searches room profiles and lists the ones that meet a set of filter criteria.</p>

#### `searchRooms`

``` purescript
searchRooms :: forall eff. SearchRoomsRequest -> Aff (err :: RequestError | eff) SearchRoomsResponse
```

<p>Searches rooms and lists the ones that meet a set of filter and sort criteria.</p>

#### `searchSkillGroups`

``` purescript
searchSkillGroups :: forall eff. SearchSkillGroupsRequest -> Aff (err :: RequestError | eff) SearchSkillGroupsResponse
```

<p>Searches skill groups and lists the ones that meet a set of filter and sort criteria.</p>

#### `searchUsers`

``` purescript
searchUsers :: forall eff. SearchUsersRequest -> Aff (err :: RequestError | eff) SearchUsersResponse
```

<p>Searches users and lists the ones that meet a set of filter and sort criteria.</p>

#### `sendInvitation`

``` purescript
sendInvitation :: forall eff. SendInvitationRequest -> Aff (err :: RequestError | eff) SendInvitationResponse
```

<p>Sends an enrollment invitation email with a URL to a user. The URL is valid for 72 hours or until you call this operation again, whichever comes first. </p>

#### `startDeviceSync`

``` purescript
startDeviceSync :: forall eff. StartDeviceSyncRequest -> Aff (err :: RequestError | eff) StartDeviceSyncResponse
```

<p>Resets a device and its account to the known default settings by clearing all information and settings set by previous users.</p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) TagResourceResponse
```

<p>Adds metadata tags to a specified resource.</p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) UntagResourceResponse
```

<p>Removes metadata tags from a specified resource.</p>

#### `updateDevice`

``` purescript
updateDevice :: forall eff. UpdateDeviceRequest -> Aff (err :: RequestError | eff) UpdateDeviceResponse
```

<p>Updates the device name by device ARN.</p>

#### `updateProfile`

``` purescript
updateProfile :: forall eff. UpdateProfileRequest -> Aff (err :: RequestError | eff) UpdateProfileResponse
```

<p>Updates an existing room profile by room profile ARN.</p>

#### `updateRoom`

``` purescript
updateRoom :: forall eff. UpdateRoomRequest -> Aff (err :: RequestError | eff) UpdateRoomResponse
```

<p>Updates room details by room ARN.</p>

#### `updateSkillGroup`

``` purescript
updateSkillGroup :: forall eff. UpdateSkillGroupRequest -> Aff (err :: RequestError | eff) UpdateSkillGroupResponse
```

<p>Updates skill group details by skill group ARN.</p>

#### `Address`

``` purescript
newtype Address
  = Address String
```

#### `AlreadyExistsException`

``` purescript
newtype AlreadyExistsException
  = AlreadyExistsException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The resource being created already exists. HTTP Status Code: 400</p>

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

#### `AssociateDeviceWithRoomRequest`

``` purescript
newtype AssociateDeviceWithRoomRequest
  = AssociateDeviceWithRoomRequest { "DeviceArn" :: NullOrUndefined (Arn), "RoomArn" :: NullOrUndefined (Arn) }
```

#### `AssociateDeviceWithRoomResponse`

``` purescript
newtype AssociateDeviceWithRoomResponse
  = AssociateDeviceWithRoomResponse {  }
```

#### `AssociateSkillGroupWithRoomRequest`

``` purescript
newtype AssociateSkillGroupWithRoomRequest
  = AssociateSkillGroupWithRoomRequest { "SkillGroupArn" :: NullOrUndefined (Arn), "RoomArn" :: NullOrUndefined (Arn) }
```

#### `AssociateSkillGroupWithRoomResponse`

``` purescript
newtype AssociateSkillGroupWithRoomResponse
  = AssociateSkillGroupWithRoomResponse {  }
```

#### `ClientRequestToken`

``` purescript
newtype ClientRequestToken
  = ClientRequestToken String
```

User specified token that is used to support idempotency during Create Resource

#### `CreateProfileRequest`

``` purescript
newtype CreateProfileRequest
  = CreateProfileRequest { "ProfileName" :: ProfileName, "Timezone" :: Timezone, "Address" :: Address, "DistanceUnit" :: DistanceUnit, "TemperatureUnit" :: TemperatureUnit, "WakeWord" :: WakeWord, "ClientRequestToken" :: NullOrUndefined (ClientRequestToken), "SetupModeDisabled" :: NullOrUndefined (Boolean), "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit), "PSTNEnabled" :: NullOrUndefined (Boolean) }
```

#### `CreateProfileResponse`

``` purescript
newtype CreateProfileResponse
  = CreateProfileResponse { "ProfileArn" :: NullOrUndefined (Arn) }
```

#### `CreateRoomRequest`

``` purescript
newtype CreateRoomRequest
  = CreateRoomRequest { "RoomName" :: RoomName, "Description" :: NullOrUndefined (RoomDescription), "ProfileArn" :: NullOrUndefined (Arn), "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken), "Tags" :: NullOrUndefined (TagList) }
```

#### `CreateRoomResponse`

``` purescript
newtype CreateRoomResponse
  = CreateRoomResponse { "RoomArn" :: NullOrUndefined (Arn) }
```

#### `CreateSkillGroupRequest`

``` purescript
newtype CreateSkillGroupRequest
  = CreateSkillGroupRequest { "SkillGroupName" :: SkillGroupName, "Description" :: NullOrUndefined (SkillGroupDescription), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

#### `CreateSkillGroupResponse`

``` purescript
newtype CreateSkillGroupResponse
  = CreateSkillGroupResponse { "SkillGroupArn" :: NullOrUndefined (Arn) }
```

#### `CreateUserRequest`

``` purescript
newtype CreateUserRequest
  = CreateUserRequest { "UserId" :: User_UserId', "FirstName" :: NullOrUndefined (User_FirstName'), "LastName" :: NullOrUndefined (User_LastName'), "Email" :: NullOrUndefined (Email), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken), "Tags" :: NullOrUndefined (TagList) }
```

#### `CreateUserResponse`

``` purescript
newtype CreateUserResponse
  = CreateUserResponse { "UserArn" :: NullOrUndefined (Arn) }
```

#### `DeleteProfileRequest`

``` purescript
newtype DeleteProfileRequest
  = DeleteProfileRequest { "ProfileArn" :: NullOrUndefined (Arn) }
```

#### `DeleteProfileResponse`

``` purescript
newtype DeleteProfileResponse
  = DeleteProfileResponse {  }
```

#### `DeleteRoomRequest`

``` purescript
newtype DeleteRoomRequest
  = DeleteRoomRequest { "RoomArn" :: NullOrUndefined (Arn) }
```

#### `DeleteRoomResponse`

``` purescript
newtype DeleteRoomResponse
  = DeleteRoomResponse {  }
```

#### `DeleteRoomSkillParameterRequest`

``` purescript
newtype DeleteRoomSkillParameterRequest
  = DeleteRoomSkillParameterRequest { "RoomArn" :: NullOrUndefined (Arn), "SkillId" :: SkillId, "ParameterKey" :: RoomSkillParameterKey }
```

#### `DeleteRoomSkillParameterResponse`

``` purescript
newtype DeleteRoomSkillParameterResponse
  = DeleteRoomSkillParameterResponse {  }
```

#### `DeleteSkillGroupRequest`

``` purescript
newtype DeleteSkillGroupRequest
  = DeleteSkillGroupRequest { "SkillGroupArn" :: NullOrUndefined (Arn) }
```

#### `DeleteSkillGroupResponse`

``` purescript
newtype DeleteSkillGroupResponse
  = DeleteSkillGroupResponse {  }
```

#### `DeleteUserRequest`

``` purescript
newtype DeleteUserRequest
  = DeleteUserRequest { "UserArn" :: NullOrUndefined (Arn), "EnrollmentId" :: EnrollmentId }
```

#### `DeleteUserResponse`

``` purescript
newtype DeleteUserResponse
  = DeleteUserResponse {  }
```

#### `Device`

``` purescript
newtype Device
  = Device { "DeviceArn" :: NullOrUndefined (Arn), "DeviceSerialNumber" :: NullOrUndefined (DeviceSerialNumber), "DeviceType" :: NullOrUndefined (DeviceType), "DeviceName" :: NullOrUndefined (DeviceName), "SoftwareVersion" :: NullOrUndefined (SoftwareVersion), "MacAddress" :: NullOrUndefined (MacAddress), "RoomArn" :: NullOrUndefined (Arn), "DeviceStatus" :: NullOrUndefined (DeviceStatus), "DeviceStatusInfo" :: NullOrUndefined (DeviceStatusInfo) }
```

<p>A device with attributes.</p>

#### `DeviceData`

``` purescript
newtype DeviceData
  = DeviceData { "DeviceArn" :: NullOrUndefined (Arn), "DeviceSerialNumber" :: NullOrUndefined (DeviceSerialNumber), "DeviceType" :: NullOrUndefined (DeviceType), "DeviceName" :: NullOrUndefined (DeviceName), "SoftwareVersion" :: NullOrUndefined (SoftwareVersion), "MacAddress" :: NullOrUndefined (MacAddress), "DeviceStatus" :: NullOrUndefined (DeviceStatus), "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "DeviceStatusInfo" :: NullOrUndefined (DeviceStatusInfo) }
```

<p>Device attributes.</p>

#### `DeviceDataList`

``` purescript
newtype DeviceDataList
  = DeviceDataList (Array DeviceData)
```

#### `DeviceName`

``` purescript
newtype DeviceName
  = DeviceName String
```

#### `DeviceSerialNumber`

``` purescript
newtype DeviceSerialNumber
  = DeviceSerialNumber String
```

#### `DeviceStatus`

``` purescript
newtype DeviceStatus
  = DeviceStatus String
```

#### `DeviceStatusDetail`

``` purescript
newtype DeviceStatusDetail
  = DeviceStatusDetail { "Code" :: NullOrUndefined (DeviceStatusDetailCode) }
```

<p>Details of a device’s status.</p>

#### `DeviceStatusDetailCode`

``` purescript
newtype DeviceStatusDetailCode
  = DeviceStatusDetailCode String
```

#### `DeviceStatusDetails`

``` purescript
newtype DeviceStatusDetails
  = DeviceStatusDetails (Array DeviceStatusDetail)
```

#### `DeviceStatusInfo`

``` purescript
newtype DeviceStatusInfo
  = DeviceStatusInfo { "DeviceStatusDetails" :: NullOrUndefined (DeviceStatusDetails) }
```

<p>Detailed information about a device's status.</p>

#### `DeviceType`

``` purescript
newtype DeviceType
  = DeviceType String
```

#### `DisassociateDeviceFromRoomRequest`

``` purescript
newtype DisassociateDeviceFromRoomRequest
  = DisassociateDeviceFromRoomRequest { "DeviceArn" :: NullOrUndefined (Arn) }
```

#### `DisassociateDeviceFromRoomResponse`

``` purescript
newtype DisassociateDeviceFromRoomResponse
  = DisassociateDeviceFromRoomResponse {  }
```

#### `DisassociateSkillGroupFromRoomRequest`

``` purescript
newtype DisassociateSkillGroupFromRoomRequest
  = DisassociateSkillGroupFromRoomRequest { "SkillGroupArn" :: NullOrUndefined (Arn), "RoomArn" :: NullOrUndefined (Arn) }
```

#### `DisassociateSkillGroupFromRoomResponse`

``` purescript
newtype DisassociateSkillGroupFromRoomResponse
  = DisassociateSkillGroupFromRoomResponse {  }
```

#### `DistanceUnit`

``` purescript
newtype DistanceUnit
  = DistanceUnit String
```

#### `Email`

``` purescript
newtype Email
  = Email String
```

#### `EnrollmentId`

``` purescript
newtype EnrollmentId
  = EnrollmentId String
```

#### `EnrollmentStatus`

``` purescript
newtype EnrollmentStatus
  = EnrollmentStatus String
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `Feature`

``` purescript
newtype Feature
  = Feature String
```

#### `Features`

``` purescript
newtype Features
  = Features (Array Feature)
```

#### `Filter`

``` purescript
newtype Filter
  = Filter { "Key" :: FilterKey, "Values" :: FilterValueList }
```

<p>A filter name and value pair that is used to return a more specific list of results. Filters can be used to match a set of resources by various criteria.</p>

#### `FilterKey`

``` purescript
newtype FilterKey
  = FilterKey String
```

#### `FilterList`

``` purescript
newtype FilterList
  = FilterList (Array Filter)
```

#### `FilterValue`

``` purescript
newtype FilterValue
  = FilterValue String
```

#### `FilterValueList`

``` purescript
newtype FilterValueList
  = FilterValueList (Array FilterValue)
```

#### `GetDeviceRequest`

``` purescript
newtype GetDeviceRequest
  = GetDeviceRequest { "DeviceArn" :: NullOrUndefined (Arn) }
```

#### `GetDeviceResponse`

``` purescript
newtype GetDeviceResponse
  = GetDeviceResponse { "Device" :: NullOrUndefined (Device) }
```

#### `GetProfileRequest`

``` purescript
newtype GetProfileRequest
  = GetProfileRequest { "ProfileArn" :: NullOrUndefined (Arn) }
```

#### `GetProfileResponse`

``` purescript
newtype GetProfileResponse
  = GetProfileResponse { "Profile" :: NullOrUndefined (Profile) }
```

#### `GetRoomRequest`

``` purescript
newtype GetRoomRequest
  = GetRoomRequest { "RoomArn" :: NullOrUndefined (Arn) }
```

#### `GetRoomResponse`

``` purescript
newtype GetRoomResponse
  = GetRoomResponse { "Room" :: NullOrUndefined (Room) }
```

#### `GetRoomSkillParameterRequest`

``` purescript
newtype GetRoomSkillParameterRequest
  = GetRoomSkillParameterRequest { "RoomArn" :: NullOrUndefined (Arn), "SkillId" :: SkillId, "ParameterKey" :: RoomSkillParameterKey }
```

#### `GetRoomSkillParameterResponse`

``` purescript
newtype GetRoomSkillParameterResponse
  = GetRoomSkillParameterResponse { "RoomSkillParameter" :: NullOrUndefined (RoomSkillParameter) }
```

#### `GetSkillGroupRequest`

``` purescript
newtype GetSkillGroupRequest
  = GetSkillGroupRequest { "SkillGroupArn" :: NullOrUndefined (Arn) }
```

#### `GetSkillGroupResponse`

``` purescript
newtype GetSkillGroupResponse
  = GetSkillGroupResponse { "SkillGroup" :: NullOrUndefined (SkillGroup) }
```

#### `InvalidUserStatusException`

``` purescript
newtype InvalidUserStatusException
  = InvalidUserStatusException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The attempt to update a user is invalid due to the user's current status. HTTP Status Code: 400</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>You are performing an action that would put you beyond your account's limits. HTTP Status Code: 400</p>

#### `ListSkillsRequest`

``` purescript
newtype ListSkillsRequest
  = ListSkillsRequest { "SkillGroupArn" :: NullOrUndefined (Arn), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (SkillListMaxResults) }
```

#### `ListSkillsResponse`

``` purescript
newtype ListSkillsResponse
  = ListSkillsResponse { "SkillSummaries" :: NullOrUndefined (SkillSummaryList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListTagsRequest`

``` purescript
newtype ListTagsRequest
  = ListTagsRequest { "Arn" :: Arn, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `ListTagsResponse`

``` purescript
newtype ListTagsResponse
  = ListTagsResponse { "Tags" :: NullOrUndefined (TagList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `MacAddress`

``` purescript
newtype MacAddress
  = MacAddress String
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `MaxVolumeLimit`

``` purescript
newtype MaxVolumeLimit
  = MaxVolumeLimit Int
```

#### `NameInUseException`

``` purescript
newtype NameInUseException
  = NameInUseException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The name sent in the request is already in use. HTTP Status Code: 400</p>

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The resource is not found. HTTP Status Code: 400</p>

#### `Profile`

``` purescript
newtype Profile
  = Profile { "ProfileArn" :: NullOrUndefined (Arn), "ProfileName" :: NullOrUndefined (ProfileName), "Address" :: NullOrUndefined (Address), "Timezone" :: NullOrUndefined (Timezone), "DistanceUnit" :: NullOrUndefined (DistanceUnit), "TemperatureUnit" :: NullOrUndefined (TemperatureUnit), "WakeWord" :: NullOrUndefined (WakeWord), "SetupModeDisabled" :: NullOrUndefined (Boolean), "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit), "PSTNEnabled" :: NullOrUndefined (Boolean) }
```

<p>A room profile with attributes.</p>

#### `ProfileData`

``` purescript
newtype ProfileData
  = ProfileData { "ProfileArn" :: NullOrUndefined (Arn), "ProfileName" :: NullOrUndefined (ProfileName), "Address" :: NullOrUndefined (Address), "Timezone" :: NullOrUndefined (Timezone), "DistanceUnit" :: NullOrUndefined (DistanceUnit), "TemperatureUnit" :: NullOrUndefined (TemperatureUnit), "WakeWord" :: NullOrUndefined (WakeWord) }
```

<p>The data of a room profile.</p>

#### `ProfileDataList`

``` purescript
newtype ProfileDataList
  = ProfileDataList (Array ProfileData)
```

#### `ProfileName`

``` purescript
newtype ProfileName
  = ProfileName String
```

#### `ProviderCalendarId`

``` purescript
newtype ProviderCalendarId
  = ProviderCalendarId String
```

#### `PutRoomSkillParameterRequest`

``` purescript
newtype PutRoomSkillParameterRequest
  = PutRoomSkillParameterRequest { "RoomArn" :: NullOrUndefined (Arn), "SkillId" :: SkillId, "RoomSkillParameter" :: RoomSkillParameter }
```

#### `PutRoomSkillParameterResponse`

``` purescript
newtype PutRoomSkillParameterResponse
  = PutRoomSkillParameterResponse {  }
```

#### `ResolveRoomRequest`

``` purescript
newtype ResolveRoomRequest
  = ResolveRoomRequest { "UserId" :: UserId, "SkillId" :: SkillId }
```

#### `ResolveRoomResponse`

``` purescript
newtype ResolveRoomResponse
  = ResolveRoomResponse { "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "RoomSkillParameters" :: NullOrUndefined (RoomSkillParameters) }
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message" :: NullOrUndefined (ErrorMessage), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

<p>The resource in the request is already in use. HTTP Status Code: 400</p>

#### `RevokeInvitationRequest`

``` purescript
newtype RevokeInvitationRequest
  = RevokeInvitationRequest { "UserArn" :: NullOrUndefined (Arn), "EnrollmentId" :: NullOrUndefined (EnrollmentId) }
```

#### `RevokeInvitationResponse`

``` purescript
newtype RevokeInvitationResponse
  = RevokeInvitationResponse {  }
```

#### `Room`

``` purescript
newtype Room
  = Room { "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "Description" :: NullOrUndefined (RoomDescription), "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId), "ProfileArn" :: NullOrUndefined (Arn) }
```

<p>A room with attributes.</p>

#### `RoomData`

``` purescript
newtype RoomData
  = RoomData { "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "Description" :: NullOrUndefined (RoomDescription), "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId), "ProfileArn" :: NullOrUndefined (Arn), "ProfileName" :: NullOrUndefined (ProfileName) }
```

<p>The data of a room.</p>

#### `RoomDataList`

``` purescript
newtype RoomDataList
  = RoomDataList (Array RoomData)
```

#### `RoomDescription`

``` purescript
newtype RoomDescription
  = RoomDescription String
```

#### `RoomName`

``` purescript
newtype RoomName
  = RoomName String
```

#### `RoomSkillParameter`

``` purescript
newtype RoomSkillParameter
  = RoomSkillParameter { "ParameterKey" :: RoomSkillParameterKey, "ParameterValue" :: RoomSkillParameterValue }
```

<p>A skill parameter associated with a room.</p>

#### `RoomSkillParameterKey`

``` purescript
newtype RoomSkillParameterKey
  = RoomSkillParameterKey String
```

#### `RoomSkillParameterValue`

``` purescript
newtype RoomSkillParameterValue
  = RoomSkillParameterValue String
```

#### `RoomSkillParameters`

``` purescript
newtype RoomSkillParameters
  = RoomSkillParameters (Array RoomSkillParameter)
```

#### `SearchDevicesRequest`

``` purescript
newtype SearchDevicesRequest
  = SearchDevicesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

#### `SearchDevicesResponse`

``` purescript
newtype SearchDevicesResponse
  = SearchDevicesResponse { "Devices" :: NullOrUndefined (DeviceDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

#### `SearchProfilesRequest`

``` purescript
newtype SearchProfilesRequest
  = SearchProfilesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

#### `SearchProfilesResponse`

``` purescript
newtype SearchProfilesResponse
  = SearchProfilesResponse { "Profiles" :: NullOrUndefined (ProfileDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

#### `SearchRoomsRequest`

``` purescript
newtype SearchRoomsRequest
  = SearchRoomsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

#### `SearchRoomsResponse`

``` purescript
newtype SearchRoomsResponse
  = SearchRoomsResponse { "Rooms" :: NullOrUndefined (RoomDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

#### `SearchSkillGroupsRequest`

``` purescript
newtype SearchSkillGroupsRequest
  = SearchSkillGroupsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

#### `SearchSkillGroupsResponse`

``` purescript
newtype SearchSkillGroupsResponse
  = SearchSkillGroupsResponse { "SkillGroups" :: NullOrUndefined (SkillGroupDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

#### `SearchUsersRequest`

``` purescript
newtype SearchUsersRequest
  = SearchUsersRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (FilterList), "SortCriteria" :: NullOrUndefined (SortList) }
```

#### `SearchUsersResponse`

``` purescript
newtype SearchUsersResponse
  = SearchUsersResponse { "Users" :: NullOrUndefined (UserDataList), "NextToken" :: NullOrUndefined (NextToken), "TotalCount" :: NullOrUndefined (TotalCount) }
```

#### `SendInvitationRequest`

``` purescript
newtype SendInvitationRequest
  = SendInvitationRequest { "UserArn" :: NullOrUndefined (Arn) }
```

#### `SendInvitationResponse`

``` purescript
newtype SendInvitationResponse
  = SendInvitationResponse {  }
```

#### `SkillGroup`

``` purescript
newtype SkillGroup
  = SkillGroup { "SkillGroupArn" :: NullOrUndefined (Arn), "SkillGroupName" :: NullOrUndefined (SkillGroupName), "Description" :: NullOrUndefined (SkillGroupDescription) }
```

<p>A skill group with attributes.</p>

#### `SkillGroupData`

``` purescript
newtype SkillGroupData
  = SkillGroupData { "SkillGroupArn" :: NullOrUndefined (Arn), "SkillGroupName" :: NullOrUndefined (SkillGroupName), "Description" :: NullOrUndefined (SkillGroupDescription) }
```

<p>The attributes of a skill group.</p>

#### `SkillGroupDataList`

``` purescript
newtype SkillGroupDataList
  = SkillGroupDataList (Array SkillGroupData)
```

#### `SkillGroupDescription`

``` purescript
newtype SkillGroupDescription
  = SkillGroupDescription String
```

#### `SkillGroupName`

``` purescript
newtype SkillGroupName
  = SkillGroupName String
```

#### `SkillId`

``` purescript
newtype SkillId
  = SkillId String
```

#### `SkillListMaxResults`

``` purescript
newtype SkillListMaxResults
  = SkillListMaxResults Int
```

#### `SkillName`

``` purescript
newtype SkillName
  = SkillName String
```

#### `SkillSummary`

``` purescript
newtype SkillSummary
  = SkillSummary { "SkillId" :: NullOrUndefined (SkillId), "SkillName" :: NullOrUndefined (SkillName), "SupportsLinking" :: NullOrUndefined (Boolean) }
```

<p>The summary of skills.</p>

#### `SkillSummaryList`

``` purescript
newtype SkillSummaryList
  = SkillSummaryList (Array SkillSummary)
```

#### `SoftwareVersion`

``` purescript
newtype SoftwareVersion
  = SoftwareVersion String
```

#### `Sort`

``` purescript
newtype Sort
  = Sort { "Key" :: SortKey, "Value" :: SortValue }
```

<p>An object representing a sort criteria. </p>

#### `SortKey`

``` purescript
newtype SortKey
  = SortKey String
```

#### `SortList`

``` purescript
newtype SortList
  = SortList (Array Sort)
```

#### `SortValue`

``` purescript
newtype SortValue
  = SortValue String
```

#### `StartDeviceSyncRequest`

``` purescript
newtype StartDeviceSyncRequest
  = StartDeviceSyncRequest { "RoomArn" :: NullOrUndefined (Arn), "DeviceArn" :: NullOrUndefined (Arn), "Features" :: Features }
```

#### `StartDeviceSyncResponse`

``` purescript
newtype StartDeviceSyncResponse
  = StartDeviceSyncResponse {  }
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (TagKey), "Value" :: NullOrUndefined (TagValue) }
```

<p>A key-value pair that can be associated with a resource. </p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "Arn" :: Arn, "Tags" :: TagList }
```

#### `TagResourceResponse`

``` purescript
newtype TagResourceResponse
  = TagResourceResponse {  }
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `TemperatureUnit`

``` purescript
newtype TemperatureUnit
  = TemperatureUnit String
```

#### `Timezone`

``` purescript
newtype Timezone
  = Timezone String
```

#### `TotalCount`

``` purescript
newtype TotalCount
  = TotalCount Int
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "Arn" :: Arn, "TagKeys" :: TagKeyList }
```

#### `UntagResourceResponse`

``` purescript
newtype UntagResourceResponse
  = UntagResourceResponse {  }
```

#### `UpdateDeviceRequest`

``` purescript
newtype UpdateDeviceRequest
  = UpdateDeviceRequest { "DeviceArn" :: NullOrUndefined (Arn), "DeviceName" :: NullOrUndefined (DeviceName) }
```

#### `UpdateDeviceResponse`

``` purescript
newtype UpdateDeviceResponse
  = UpdateDeviceResponse {  }
```

#### `UpdateProfileRequest`

``` purescript
newtype UpdateProfileRequest
  = UpdateProfileRequest { "ProfileArn" :: NullOrUndefined (Arn), "ProfileName" :: NullOrUndefined (ProfileName), "Timezone" :: NullOrUndefined (Timezone), "Address" :: NullOrUndefined (Address), "DistanceUnit" :: NullOrUndefined (DistanceUnit), "TemperatureUnit" :: NullOrUndefined (TemperatureUnit), "WakeWord" :: NullOrUndefined (WakeWord), "SetupModeDisabled" :: NullOrUndefined (Boolean), "MaxVolumeLimit" :: NullOrUndefined (MaxVolumeLimit), "PSTNEnabled" :: NullOrUndefined (Boolean) }
```

#### `UpdateProfileResponse`

``` purescript
newtype UpdateProfileResponse
  = UpdateProfileResponse {  }
```

#### `UpdateRoomRequest`

``` purescript
newtype UpdateRoomRequest
  = UpdateRoomRequest { "RoomArn" :: NullOrUndefined (Arn), "RoomName" :: NullOrUndefined (RoomName), "Description" :: NullOrUndefined (RoomDescription), "ProviderCalendarId" :: NullOrUndefined (ProviderCalendarId), "ProfileArn" :: NullOrUndefined (Arn) }
```

#### `UpdateRoomResponse`

``` purescript
newtype UpdateRoomResponse
  = UpdateRoomResponse {  }
```

#### `UpdateSkillGroupRequest`

``` purescript
newtype UpdateSkillGroupRequest
  = UpdateSkillGroupRequest { "SkillGroupArn" :: NullOrUndefined (Arn), "SkillGroupName" :: NullOrUndefined (SkillGroupName), "Description" :: NullOrUndefined (SkillGroupDescription) }
```

#### `UpdateSkillGroupResponse`

``` purescript
newtype UpdateSkillGroupResponse
  = UpdateSkillGroupResponse {  }
```

#### `UserData`

``` purescript
newtype UserData
  = UserData { "UserArn" :: NullOrUndefined (Arn), "FirstName" :: NullOrUndefined (User_FirstName'), "LastName" :: NullOrUndefined (User_LastName'), "Email" :: NullOrUndefined (Email), "EnrollmentStatus" :: NullOrUndefined (EnrollmentStatus), "EnrollmentId" :: NullOrUndefined (EnrollmentId) }
```

<p>Information related to a user.</p>

#### `UserDataList`

``` purescript
newtype UserDataList
  = UserDataList (Array UserData)
```

#### `UserId`

``` purescript
newtype UserId
  = UserId String
```

#### `WakeWord`

``` purescript
newtype WakeWord
  = WakeWord String
```

#### `User_FirstName'`

``` purescript
newtype User_FirstName'
  = User_FirstName' String
```

#### `User_LastName'`

``` purescript
newtype User_LastName'
  = User_LastName' String
```

#### `User_UserId'`

``` purescript
newtype User_UserId'
  = User_UserId' String
```


