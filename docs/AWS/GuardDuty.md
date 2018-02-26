## Module AWS.GuardDuty

Assess, monitor, manage, and remediate security issues across your AWS infrastructure, applications, and data.

#### `serviceName`

``` purescript
serviceName :: String
```

#### `acceptInvitation`

``` purescript
acceptInvitation :: forall eff. AcceptInvitationRequest -> Aff (err :: RequestError | eff) AcceptInvitationResponse
```

Accepts the invitation to be monitored by a master GuardDuty account.

#### `archiveFindings`

``` purescript
archiveFindings :: forall eff. ArchiveFindingsRequest -> Aff (err :: RequestError | eff) ArchiveFindingsResponse
```

Archives Amazon GuardDuty findings specified by the list of finding IDs.

#### `createDetector`

``` purescript
createDetector :: forall eff. CreateDetectorRequest -> Aff (err :: RequestError | eff) CreateDetectorResponse
```

Creates a single Amazon GuardDuty detector. A detector is an object that represents the GuardDuty service. A detector must be created in order for GuardDuty to become operational.

#### `createIPSet`

``` purescript
createIPSet :: forall eff. CreateIPSetRequest -> Aff (err :: RequestError | eff) CreateIPSetResponse
```

Creates a new IPSet - a list of trusted IP addresses that have been whitelisted for secure communication with AWS infrastructure and applications.

#### `createMembers`

``` purescript
createMembers :: forall eff. CreateMembersRequest -> Aff (err :: RequestError | eff) CreateMembersResponse
```

Creates member accounts of the current AWS account by specifying a list of AWS account IDs. The current AWS account can then invite these members to manage GuardDuty in their accounts.

#### `createSampleFindings`

``` purescript
createSampleFindings :: forall eff. CreateSampleFindingsRequest -> Aff (err :: RequestError | eff) CreateSampleFindingsResponse
```

Generates example findings of types specified by the list of finding types. If 'NULL' is specified for findingTypes, the API generates example findings of all supported finding types.

#### `createThreatIntelSet`

``` purescript
createThreatIntelSet :: forall eff. CreateThreatIntelSetRequest -> Aff (err :: RequestError | eff) CreateThreatIntelSetResponse
```

Create a new ThreatIntelSet. ThreatIntelSets consist of known malicious IP addresses. GuardDuty generates findings based on ThreatIntelSets.

#### `declineInvitations`

``` purescript
declineInvitations :: forall eff. DeclineInvitationsRequest -> Aff (err :: RequestError | eff) DeclineInvitationsResponse
```

Declines invitations sent to the current member account by AWS account specified by their account IDs.

#### `deleteDetector`

``` purescript
deleteDetector :: forall eff. DeleteDetectorRequest -> Aff (err :: RequestError | eff) DeleteDetectorResponse
```

Deletes a Amazon GuardDuty detector specified by the detector ID.

#### `deleteIPSet`

``` purescript
deleteIPSet :: forall eff. DeleteIPSetRequest -> Aff (err :: RequestError | eff) DeleteIPSetResponse
```

Deletes the IPSet specified by the IPSet ID.

#### `deleteInvitations`

``` purescript
deleteInvitations :: forall eff. DeleteInvitationsRequest -> Aff (err :: RequestError | eff) DeleteInvitationsResponse
```

Deletes invitations sent to the current member account by AWS accounts specified by their account IDs.

#### `deleteMembers`

``` purescript
deleteMembers :: forall eff. DeleteMembersRequest -> Aff (err :: RequestError | eff) DeleteMembersResponse
```

Deletes GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.

#### `deleteThreatIntelSet`

``` purescript
deleteThreatIntelSet :: forall eff. DeleteThreatIntelSetRequest -> Aff (err :: RequestError | eff) DeleteThreatIntelSetResponse
```

Deletes ThreatIntelSet specified by the ThreatIntelSet ID.

#### `disassociateFromMasterAccount`

``` purescript
disassociateFromMasterAccount :: forall eff. DisassociateFromMasterAccountRequest -> Aff (err :: RequestError | eff) DisassociateFromMasterAccountResponse
```

Disassociates the current GuardDuty member account from its master account.

#### `disassociateMembers`

``` purescript
disassociateMembers :: forall eff. DisassociateMembersRequest -> Aff (err :: RequestError | eff) DisassociateMembersResponse
```

Disassociates GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.

#### `getDetector`

``` purescript
getDetector :: forall eff. GetDetectorRequest -> Aff (err :: RequestError | eff) GetDetectorResponse
```

Retrieves an Amazon GuardDuty detector specified by the detectorId.

#### `getFindings`

``` purescript
getFindings :: forall eff. GetFindingsRequest -> Aff (err :: RequestError | eff) GetFindingsResponse
```

Describes Amazon GuardDuty findings specified by finding IDs.

#### `getFindingsStatistics`

``` purescript
getFindingsStatistics :: forall eff. GetFindingsStatisticsRequest -> Aff (err :: RequestError | eff) GetFindingsStatisticsResponse
```

Lists Amazon GuardDuty findings' statistics for the specified detector ID.

#### `getIPSet`

``` purescript
getIPSet :: forall eff. GetIPSetRequest -> Aff (err :: RequestError | eff) GetIPSetResponse
```

Retrieves the IPSet specified by the IPSet ID.

#### `getInvitationsCount`

``` purescript
getInvitationsCount :: forall eff. GetInvitationsCountRequest -> Aff (err :: RequestError | eff) GetInvitationsCountResponse
```

Returns the count of all GuardDuty membership invitations that were sent to the current member account except the currently accepted invitation.

#### `getMasterAccount`

``` purescript
getMasterAccount :: forall eff. GetMasterAccountRequest -> Aff (err :: RequestError | eff) GetMasterAccountResponse
```

Provides the details for the GuardDuty master account to the current GuardDuty member account.

#### `getMembers`

``` purescript
getMembers :: forall eff. GetMembersRequest -> Aff (err :: RequestError | eff) GetMembersResponse
```

Retrieves GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.

#### `getThreatIntelSet`

``` purescript
getThreatIntelSet :: forall eff. GetThreatIntelSetRequest -> Aff (err :: RequestError | eff) GetThreatIntelSetResponse
```

Retrieves the ThreatIntelSet that is specified by the ThreatIntelSet ID.

#### `inviteMembers`

``` purescript
inviteMembers :: forall eff. InviteMembersRequest -> Aff (err :: RequestError | eff) InviteMembersResponse
```

Invites other AWS accounts (created as members of the current AWS account by CreateMembers) to enable GuardDuty and allow the current AWS account to view and manage these accounts' GuardDuty findings on their behalf as the master account.

#### `listDetectors`

``` purescript
listDetectors :: forall eff. ListDetectorsRequest -> Aff (err :: RequestError | eff) ListDetectorsResponse
```

Lists detectorIds of all the existing Amazon GuardDuty detector resources.

#### `listFindings`

``` purescript
listFindings :: forall eff. ListFindingsRequest -> Aff (err :: RequestError | eff) ListFindingsResponse
```

Lists Amazon GuardDuty findings for the specified detector ID.

#### `listIPSets`

``` purescript
listIPSets :: forall eff. ListIPSetsRequest -> Aff (err :: RequestError | eff) ListIPSetsResponse
```

Lists the IPSets of the GuardDuty service specified by the detector ID.

#### `listInvitations`

``` purescript
listInvitations :: forall eff. ListInvitationsRequest -> Aff (err :: RequestError | eff) ListInvitationsResponse
```

Lists all GuardDuty membership invitations that were sent to the current AWS account.

#### `listMembers`

``` purescript
listMembers :: forall eff. ListMembersRequest -> Aff (err :: RequestError | eff) ListMembersResponse
```

Lists details about all member accounts for the current GuardDuty master account.

#### `listThreatIntelSets`

``` purescript
listThreatIntelSets :: forall eff. ListThreatIntelSetsRequest -> Aff (err :: RequestError | eff) ListThreatIntelSetsResponse
```

Lists the ThreatIntelSets of the GuardDuty service specified by the detector ID.

#### `startMonitoringMembers`

``` purescript
startMonitoringMembers :: forall eff. StartMonitoringMembersRequest -> Aff (err :: RequestError | eff) StartMonitoringMembersResponse
```

Re-enables GuardDuty to monitor findings of the member accounts specified by the account IDs. A master GuardDuty account can run this command after disabling GuardDuty from monitoring these members' findings by running StopMonitoringMembers.

#### `stopMonitoringMembers`

``` purescript
stopMonitoringMembers :: forall eff. StopMonitoringMembersRequest -> Aff (err :: RequestError | eff) StopMonitoringMembersResponse
```

Disables GuardDuty from monitoring findings of the member accounts specified by the account IDs. After running this command, a master GuardDuty account can run StartMonitoringMembers to re-enable GuardDuty to monitor these members' findings.

#### `unarchiveFindings`

``` purescript
unarchiveFindings :: forall eff. UnarchiveFindingsRequest -> Aff (err :: RequestError | eff) UnarchiveFindingsResponse
```

Unarchives Amazon GuardDuty findings specified by the list of finding IDs.

#### `updateDetector`

``` purescript
updateDetector :: forall eff. UpdateDetectorRequest -> Aff (err :: RequestError | eff) UpdateDetectorResponse
```

Updates an Amazon GuardDuty detector specified by the detectorId.

#### `updateFindingsFeedback`

``` purescript
updateFindingsFeedback :: forall eff. UpdateFindingsFeedbackRequest -> Aff (err :: RequestError | eff) UpdateFindingsFeedbackResponse
```

Marks specified Amazon GuardDuty findings as useful or not useful.

#### `updateIPSet`

``` purescript
updateIPSet :: forall eff. UpdateIPSetRequest -> Aff (err :: RequestError | eff) UpdateIPSetResponse
```

Updates the IPSet specified by the IPSet ID.

#### `updateThreatIntelSet`

``` purescript
updateThreatIntelSet :: forall eff. UpdateThreatIntelSetRequest -> Aff (err :: RequestError | eff) UpdateThreatIntelSetResponse
```

Updates the ThreatIntelSet specified by ThreatIntelSet ID.

#### `AcceptInvitationRequest`

``` purescript
newtype AcceptInvitationRequest
  = AcceptInvitationRequest { "DetectorId" :: String, "InvitationId" :: NullOrUndefined (InvitationId), "MasterId" :: NullOrUndefined (MasterId) }
```

AcceptInvitation request body.

#### `AcceptInvitationResponse`

``` purescript
newtype AcceptInvitationResponse
  = AcceptInvitationResponse {  }
```

#### `AccessKeyDetails`

``` purescript
newtype AccessKeyDetails
  = AccessKeyDetails { "AccessKeyId" :: NullOrUndefined (String), "PrincipalId" :: NullOrUndefined (String), "UserName" :: NullOrUndefined (String), "UserType" :: NullOrUndefined (String) }
```

The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.

#### `AccountDetail`

``` purescript
newtype AccountDetail
  = AccountDetail { "AccountId" :: NullOrUndefined (AccountId), "Email" :: NullOrUndefined (Email) }
```

An object containing the member's accountId and email address.

#### `AccountDetails`

``` purescript
newtype AccountDetails
  = AccountDetails (Array AccountDetail)
```

A list of account/email pairs.

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

AWS account ID.

#### `AccountIds`

``` purescript
newtype AccountIds
  = AccountIds (Array String)
```

A list of account IDs.

#### `Action`

``` purescript
newtype Action
  = Action { "ActionType" :: NullOrUndefined (String), "AwsApiCallAction" :: NullOrUndefined (AwsApiCallAction), "DnsRequestAction" :: NullOrUndefined (DnsRequestAction), "NetworkConnectionAction" :: NullOrUndefined (NetworkConnectionAction), "PortProbeAction" :: NullOrUndefined (PortProbeAction) }
```

Information about the activity described in a finding.

#### `Activate`

``` purescript
newtype Activate
  = Activate Boolean
```

Whether we should start processing the list immediately or not.

#### `ArchiveFindingsRequest`

``` purescript
newtype ArchiveFindingsRequest
  = ArchiveFindingsRequest { "DetectorId" :: String, "FindingIds" :: NullOrUndefined (FindingIds) }
```

ArchiveFindings request body.

#### `ArchiveFindingsResponse`

``` purescript
newtype ArchiveFindingsResponse
  = ArchiveFindingsResponse {  }
```

#### `AwsApiCallAction`

``` purescript
newtype AwsApiCallAction
  = AwsApiCallAction { "Api" :: NullOrUndefined (String), "CallerType" :: NullOrUndefined (String), "DomainDetails" :: NullOrUndefined (DomainDetails), "RemoteIpDetails" :: NullOrUndefined (RemoteIpDetails), "ServiceName" :: NullOrUndefined (String) }
```

Information about the AWS_API_CALL action described in this finding.

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String) }
```

Error response object.

#### `City`

``` purescript
newtype City
  = City { "CityName" :: NullOrUndefined (String) }
```

City information of the remote IP address.

#### `Comments`

``` purescript
newtype Comments
  = Comments String
```

Additional feedback about the GuardDuty findings.

#### `Condition`

``` purescript
newtype Condition
  = Condition { "Eq" :: NullOrUndefined (Eq), "Gt" :: NullOrUndefined (Int), "Gte" :: NullOrUndefined (Int), "Lt" :: NullOrUndefined (Int), "Lte" :: NullOrUndefined (Int), "Neq" :: NullOrUndefined (Neq) }
```

Finding attribute (for example, accountId) for which conditions and values must be specified when querying findings.

#### `CountBySeverityFindingStatistic`

``` purescript
newtype CountBySeverityFindingStatistic
  = CountBySeverityFindingStatistic Int
```

The count of findings for the given severity.

#### `Country`

``` purescript
newtype Country
  = Country { "CountryCode" :: NullOrUndefined (String), "CountryName" :: NullOrUndefined (String) }
```

Country information of the remote IP address.

#### `CreateDetectorRequest`

``` purescript
newtype CreateDetectorRequest
  = CreateDetectorRequest { "Enable" :: NullOrUndefined (Enable) }
```

CreateDetector request body.

#### `CreateDetectorResponse`

``` purescript
newtype CreateDetectorResponse
  = CreateDetectorResponse { "DetectorId" :: NullOrUndefined (DetectorId) }
```

#### `CreateIPSetRequest`

``` purescript
newtype CreateIPSetRequest
  = CreateIPSetRequest { "Activate" :: NullOrUndefined (Activate), "DetectorId" :: String, "Format" :: NullOrUndefined (IpSetFormat), "Location" :: NullOrUndefined (Location), "Name" :: NullOrUndefined (Name) }
```

CreateIPSet request body.

#### `CreateIPSetResponse`

``` purescript
newtype CreateIPSetResponse
  = CreateIPSetResponse { "IpSetId" :: NullOrUndefined (IpSetId) }
```

#### `CreateMembersRequest`

``` purescript
newtype CreateMembersRequest
  = CreateMembersRequest { "AccountDetails" :: NullOrUndefined (AccountDetails), "DetectorId" :: String }
```

CreateMembers request body.

#### `CreateMembersResponse`

``` purescript
newtype CreateMembersResponse
  = CreateMembersResponse { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts) }
```

#### `CreateSampleFindingsRequest`

``` purescript
newtype CreateSampleFindingsRequest
  = CreateSampleFindingsRequest { "DetectorId" :: String, "FindingTypes" :: NullOrUndefined (FindingTypes) }
```

CreateSampleFindings request body.

#### `CreateSampleFindingsResponse`

``` purescript
newtype CreateSampleFindingsResponse
  = CreateSampleFindingsResponse {  }
```

#### `CreateThreatIntelSetRequest`

``` purescript
newtype CreateThreatIntelSetRequest
  = CreateThreatIntelSetRequest { "Activate" :: NullOrUndefined (Activate), "DetectorId" :: String, "Format" :: NullOrUndefined (ThreatIntelSetFormat), "Location" :: NullOrUndefined (Location), "Name" :: NullOrUndefined (Name) }
```

CreateThreatIntelSet request body.

#### `CreateThreatIntelSetResponse`

``` purescript
newtype CreateThreatIntelSetResponse
  = CreateThreatIntelSetResponse { "ThreatIntelSetId" :: NullOrUndefined (ThreatIntelSetId) }
```

#### `CreatedAt`

``` purescript
newtype CreatedAt
  = CreatedAt String
```

The first time a resource was created. The format will be ISO-8601.

#### `DeclineInvitationsRequest`

``` purescript
newtype DeclineInvitationsRequest
  = DeclineInvitationsRequest { "AccountIds" :: NullOrUndefined (AccountIds) }
```

DeclineInvitations request body.

#### `DeclineInvitationsResponse`

``` purescript
newtype DeclineInvitationsResponse
  = DeclineInvitationsResponse { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts) }
```

#### `DeleteDetectorRequest`

``` purescript
newtype DeleteDetectorRequest
  = DeleteDetectorRequest { "DetectorId" :: String }
```

#### `DeleteDetectorResponse`

``` purescript
newtype DeleteDetectorResponse
  = DeleteDetectorResponse {  }
```

#### `DeleteIPSetRequest`

``` purescript
newtype DeleteIPSetRequest
  = DeleteIPSetRequest { "DetectorId" :: String, "IpSetId" :: String }
```

#### `DeleteIPSetResponse`

``` purescript
newtype DeleteIPSetResponse
  = DeleteIPSetResponse {  }
```

#### `DeleteInvitationsRequest`

``` purescript
newtype DeleteInvitationsRequest
  = DeleteInvitationsRequest { "AccountIds" :: NullOrUndefined (AccountIds) }
```

DeleteInvitations request body.

#### `DeleteInvitationsResponse`

``` purescript
newtype DeleteInvitationsResponse
  = DeleteInvitationsResponse { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts) }
```

#### `DeleteMembersRequest`

``` purescript
newtype DeleteMembersRequest
  = DeleteMembersRequest { "AccountIds" :: NullOrUndefined (AccountIds), "DetectorId" :: String }
```

DeleteMembers request body.

#### `DeleteMembersResponse`

``` purescript
newtype DeleteMembersResponse
  = DeleteMembersResponse { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts) }
```

#### `DeleteThreatIntelSetRequest`

``` purescript
newtype DeleteThreatIntelSetRequest
  = DeleteThreatIntelSetRequest { "DetectorId" :: String, "ThreatIntelSetId" :: String }
```

#### `DeleteThreatIntelSetResponse`

``` purescript
newtype DeleteThreatIntelSetResponse
  = DeleteThreatIntelSetResponse {  }
```

#### `DetectorId`

``` purescript
newtype DetectorId
  = DetectorId String
```

The unique identifier for a detector.

#### `DetectorIds`

``` purescript
newtype DetectorIds
  = DetectorIds (Array DetectorId)
```

A list of detector Ids.

#### `DetectorStatus`

``` purescript
newtype DetectorStatus
  = DetectorStatus String
```

The status of detector.

#### `DisassociateFromMasterAccountRequest`

``` purescript
newtype DisassociateFromMasterAccountRequest
  = DisassociateFromMasterAccountRequest { "DetectorId" :: String }
```

#### `DisassociateFromMasterAccountResponse`

``` purescript
newtype DisassociateFromMasterAccountResponse
  = DisassociateFromMasterAccountResponse {  }
```

#### `DisassociateMembersRequest`

``` purescript
newtype DisassociateMembersRequest
  = DisassociateMembersRequest { "AccountIds" :: NullOrUndefined (AccountIds), "DetectorId" :: String }
```

DisassociateMembers request body.

#### `DisassociateMembersResponse`

``` purescript
newtype DisassociateMembersResponse
  = DisassociateMembersResponse { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts) }
```

#### `DnsRequestAction`

``` purescript
newtype DnsRequestAction
  = DnsRequestAction { "Domain" :: NullOrUndefined (Domain) }
```

Information about the DNS_REQUEST action described in this finding.

#### `Domain`

``` purescript
newtype Domain
  = Domain String
```

A domain name.

#### `DomainDetails`

``` purescript
newtype DomainDetails
  = DomainDetails {  }
```

Domain information for the AWS API call.

#### `Email`

``` purescript
newtype Email
  = Email String
```

Member account's email address.

#### `Enable`

``` purescript
newtype Enable
  = Enable Boolean
```

A boolean value that specifies whether the detector is to be enabled.

#### `Eq`

``` purescript
newtype Eq
  = Eq (Array String)
```

Represents the equal condition to be applied to a single field when querying for findings.

#### `ErrorResponse`

``` purescript
newtype ErrorResponse
  = ErrorResponse { "Message" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String) }
```

Error response object.

#### `Feedback`

``` purescript
newtype Feedback
  = Feedback String
```

Finding Feedback Value

#### `Finding`

``` purescript
newtype Finding
  = Finding { "AccountId" :: NullOrUndefined (String), "Arn" :: NullOrUndefined (String), "Confidence" :: NullOrUndefined (Number), "CreatedAt" :: NullOrUndefined (CreatedAt), "Description" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Partition" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String), "Resource" :: NullOrUndefined (Resource), "SchemaVersion" :: NullOrUndefined (String), "Service" :: NullOrUndefined (Service), "Severity" :: NullOrUndefined (Number), "Title" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String), "UpdatedAt" :: NullOrUndefined (UpdatedAt) }
```

Representation of a abnormal or suspicious activity.

#### `FindingCriteria`

``` purescript
newtype FindingCriteria
  = FindingCriteria { "Criterion" :: NullOrUndefined (MapOfCondition) }
```

Represents the criteria used for querying findings.

#### `FindingId`

``` purescript
newtype FindingId
  = FindingId String
```

The unique identifier for the Finding

#### `FindingIds`

``` purescript
newtype FindingIds
  = FindingIds (Array FindingId)
```

The list of the Findings.

#### `FindingStatisticType`

``` purescript
newtype FindingStatisticType
  = FindingStatisticType String
```

The types of finding statistics.

#### `FindingStatisticTypes`

``` purescript
newtype FindingStatisticTypes
  = FindingStatisticTypes (Array FindingStatisticType)
```

The list of the finding statistics.

#### `FindingStatistics`

``` purescript
newtype FindingStatistics
  = FindingStatistics { "CountBySeverity" :: NullOrUndefined (MapOfCountBySeverityFindingStatistic) }
```

Finding statistics object.

#### `FindingType`

``` purescript
newtype FindingType
  = FindingType String
```

The finding type for the finding

#### `FindingTypes`

``` purescript
newtype FindingTypes
  = FindingTypes (Array FindingType)
```

The list of the finding types.

#### `Findings`

``` purescript
newtype Findings
  = Findings (Array Finding)
```

A list of findings.

#### `GeoLocation`

``` purescript
newtype GeoLocation
  = GeoLocation { "Lat" :: NullOrUndefined (Number), "Lon" :: NullOrUndefined (Number) }
```

Location information of the remote IP address.

#### `GetDetectorRequest`

``` purescript
newtype GetDetectorRequest
  = GetDetectorRequest { "DetectorId" :: String }
```

#### `GetDetectorResponse`

``` purescript
newtype GetDetectorResponse
  = GetDetectorResponse { "CreatedAt" :: NullOrUndefined (CreatedAt), "ServiceRole" :: NullOrUndefined (ServiceRole), "Status" :: NullOrUndefined (DetectorStatus), "UpdatedAt" :: NullOrUndefined (UpdatedAt) }
```

#### `GetFindingsRequest`

``` purescript
newtype GetFindingsRequest
  = GetFindingsRequest { "DetectorId" :: String, "FindingIds" :: NullOrUndefined (FindingIds), "SortCriteria" :: NullOrUndefined (SortCriteria) }
```

GetFindings request body.

#### `GetFindingsResponse`

``` purescript
newtype GetFindingsResponse
  = GetFindingsResponse { "Findings" :: NullOrUndefined (Findings) }
```

#### `GetFindingsStatisticsRequest`

``` purescript
newtype GetFindingsStatisticsRequest
  = GetFindingsStatisticsRequest { "DetectorId" :: String, "FindingCriteria" :: NullOrUndefined (FindingCriteria), "FindingStatisticTypes" :: NullOrUndefined (FindingStatisticTypes) }
```

GetFindingsStatistics request body.

#### `GetFindingsStatisticsResponse`

``` purescript
newtype GetFindingsStatisticsResponse
  = GetFindingsStatisticsResponse { "FindingStatistics" :: NullOrUndefined (FindingStatistics) }
```

#### `GetIPSetRequest`

``` purescript
newtype GetIPSetRequest
  = GetIPSetRequest { "DetectorId" :: String, "IpSetId" :: String }
```

#### `GetIPSetResponse`

``` purescript
newtype GetIPSetResponse
  = GetIPSetResponse { "Format" :: NullOrUndefined (IpSetFormat), "Location" :: NullOrUndefined (Location), "Name" :: NullOrUndefined (Name), "Status" :: NullOrUndefined (IpSetStatus) }
```

#### `GetInvitationsCountRequest`

``` purescript
newtype GetInvitationsCountRequest
  = GetInvitationsCountRequest {  }
```

#### `GetInvitationsCountResponse`

``` purescript
newtype GetInvitationsCountResponse
  = GetInvitationsCountResponse { "InvitationsCount" :: NullOrUndefined (Int) }
```

#### `GetMasterAccountRequest`

``` purescript
newtype GetMasterAccountRequest
  = GetMasterAccountRequest { "DetectorId" :: String }
```

#### `GetMasterAccountResponse`

``` purescript
newtype GetMasterAccountResponse
  = GetMasterAccountResponse { "Master" :: NullOrUndefined (Master) }
```

#### `GetMembersRequest`

``` purescript
newtype GetMembersRequest
  = GetMembersRequest { "AccountIds" :: NullOrUndefined (AccountIds), "DetectorId" :: String }
```

GetMembers request body.

#### `GetMembersResponse`

``` purescript
newtype GetMembersResponse
  = GetMembersResponse { "Members" :: NullOrUndefined (Members), "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts) }
```

#### `GetThreatIntelSetRequest`

``` purescript
newtype GetThreatIntelSetRequest
  = GetThreatIntelSetRequest { "DetectorId" :: String, "ThreatIntelSetId" :: String }
```

#### `GetThreatIntelSetResponse`

``` purescript
newtype GetThreatIntelSetResponse
  = GetThreatIntelSetResponse { "Format" :: NullOrUndefined (ThreatIntelSetFormat), "Location" :: NullOrUndefined (Location), "Name" :: NullOrUndefined (Name), "Status" :: NullOrUndefined (ThreatIntelSetStatus) }
```

#### `IamInstanceProfile`

``` purescript
newtype IamInstanceProfile
  = IamInstanceProfile { "Arn" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String) }
```

The profile information of the EC2 instance.

#### `InstanceDetails`

``` purescript
newtype InstanceDetails
  = InstanceDetails { "AvailabilityZone" :: NullOrUndefined (String), "IamInstanceProfile" :: NullOrUndefined (IamInstanceProfile), "ImageId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "InstanceState" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (String), "LaunchTime" :: NullOrUndefined (String), "NetworkInterfaces" :: NullOrUndefined (NetworkInterfaces), "Platform" :: NullOrUndefined (String), "ProductCodes" :: NullOrUndefined (ProductCodes), "Tags" :: NullOrUndefined (Tags) }
```

The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException { "Message" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String) }
```

Error response object.

#### `Invitation`

``` purescript
newtype Invitation
  = Invitation { "AccountId" :: NullOrUndefined (String), "InvitationId" :: NullOrUndefined (InvitationId), "InvitedAt" :: NullOrUndefined (InvitedAt), "RelationshipStatus" :: NullOrUndefined (String) }
```

Invitation from an AWS account to become the current account's master.

#### `InvitationId`

``` purescript
newtype InvitationId
  = InvitationId String
```

This value is used to validate the master account to the member account.

#### `Invitations`

``` purescript
newtype Invitations
  = Invitations (Array Invitation)
```

A list of invitation descriptions.

#### `InviteMembersRequest`

``` purescript
newtype InviteMembersRequest
  = InviteMembersRequest { "AccountIds" :: NullOrUndefined (AccountIds), "DetectorId" :: String, "Message" :: NullOrUndefined (Message) }
```

InviteMembers request body.

#### `InviteMembersResponse`

``` purescript
newtype InviteMembersResponse
  = InviteMembersResponse { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts) }
```

#### `InvitedAt`

``` purescript
newtype InvitedAt
  = InvitedAt String
```

Timestamp at which a member has been invited. The format will be ISO-8601.

#### `IpSetFormat`

``` purescript
newtype IpSetFormat
  = IpSetFormat String
```

The format of the ipSet.

#### `IpSetId`

``` purescript
newtype IpSetId
  = IpSetId String
```

The unique identifier for an IP Set

#### `IpSetIds`

``` purescript
newtype IpSetIds
  = IpSetIds (Array IpSetId)
```

A list of the IP set IDs

#### `IpSetStatus`

``` purescript
newtype IpSetStatus
  = IpSetStatus String
```

The status of ipSet file uploaded.

#### `Ipv6Address`

``` purescript
newtype Ipv6Address
  = Ipv6Address String
```

IpV6 address of the EC2 instance.

#### `Ipv6Addresses`

``` purescript
newtype Ipv6Addresses
  = Ipv6Addresses (Array Ipv6Address)
```

A list of EC2 instance IPv6 address information.

#### `ListDetectorsRequest`

``` purescript
newtype ListDetectorsRequest
  = ListDetectorsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListDetectorsResponse`

``` purescript
newtype ListDetectorsResponse
  = ListDetectorsResponse { "DetectorIds" :: NullOrUndefined (DetectorIds), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListFindingsRequest`

``` purescript
newtype ListFindingsRequest
  = ListFindingsRequest { "DetectorId" :: String, "FindingCriteria" :: NullOrUndefined (FindingCriteria), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken), "SortCriteria" :: NullOrUndefined (SortCriteria) }
```

ListFindings request body.

#### `ListFindingsResponse`

``` purescript
newtype ListFindingsResponse
  = ListFindingsResponse { "FindingIds" :: NullOrUndefined (FindingIds), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListIPSetsRequest`

``` purescript
newtype ListIPSetsRequest
  = ListIPSetsRequest { "DetectorId" :: String, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListIPSetsResponse`

``` purescript
newtype ListIPSetsResponse
  = ListIPSetsResponse { "IpSetIds" :: NullOrUndefined (IpSetIds), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListInvitationsRequest`

``` purescript
newtype ListInvitationsRequest
  = ListInvitationsRequest { "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListInvitationsResponse`

``` purescript
newtype ListInvitationsResponse
  = ListInvitationsResponse { "Invitations" :: NullOrUndefined (Invitations), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListMembersRequest`

``` purescript
newtype ListMembersRequest
  = ListMembersRequest { "DetectorId" :: String, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String), "OnlyAssociated" :: NullOrUndefined (String) }
```

#### `ListMembersResponse`

``` purescript
newtype ListMembersResponse
  = ListMembersResponse { "Members" :: NullOrUndefined (Members), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListOfPortProbeDetail`

``` purescript
newtype ListOfPortProbeDetail
  = ListOfPortProbeDetail (Array PortProbeDetail)
```

#### `ListThreatIntelSetsRequest`

``` purescript
newtype ListThreatIntelSetsRequest
  = ListThreatIntelSetsRequest { "DetectorId" :: String, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (String) }
```

#### `ListThreatIntelSetsResponse`

``` purescript
newtype ListThreatIntelSetsResponse
  = ListThreatIntelSetsResponse { "NextToken" :: NullOrUndefined (NextToken), "ThreatIntelSetIds" :: NullOrUndefined (ThreatIntelSetIds) }
```

#### `LocalPortDetails`

``` purescript
newtype LocalPortDetails
  = LocalPortDetails { "Port" :: NullOrUndefined (Int), "PortName" :: NullOrUndefined (String) }
```

Local port information of the connection.

#### `Location`

``` purescript
newtype Location
  = Location String
```

The location of the S3 bucket where the list resides. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)

#### `MapOfCondition`

``` purescript
newtype MapOfCondition
  = MapOfCondition (Map String Condition)
```

#### `MapOfCountBySeverityFindingStatistic`

``` purescript
newtype MapOfCountBySeverityFindingStatistic
  = MapOfCountBySeverityFindingStatistic (Map String CountBySeverityFindingStatistic)
```

#### `Master`

``` purescript
newtype Master
  = Master { "AccountId" :: NullOrUndefined (String), "InvitationId" :: NullOrUndefined (InvitationId), "InvitedAt" :: NullOrUndefined (InvitedAt), "RelationshipStatus" :: NullOrUndefined (String) }
```

Contains details about the master account.

#### `MasterId`

``` purescript
newtype MasterId
  = MasterId String
```

The master account ID.

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `Member`

``` purescript
newtype Member
  = Member { "AccountId" :: NullOrUndefined (AccountId), "DetectorId" :: NullOrUndefined (DetectorId), "Email" :: NullOrUndefined (Email), "InvitedAt" :: NullOrUndefined (InvitedAt), "MasterId" :: NullOrUndefined (MasterId), "RelationshipStatus" :: NullOrUndefined (String), "UpdatedAt" :: NullOrUndefined (UpdatedAt) }
```

Contains details about the member account.

#### `Members`

``` purescript
newtype Members
  = Members (Array Member)
```

A list of member descriptions.

#### `Message`

``` purescript
newtype Message
  = Message String
```

The invitation message that you want to send to the accounts that you're inviting to GuardDuty as members.

#### `Name`

``` purescript
newtype Name
  = Name String
```

The user-friendly name to identify the list.

#### `Neq`

``` purescript
newtype Neq
  = Neq (Array String)
```

Represents the not equal condition to be applied to a single field when querying for findings.

#### `NetworkConnectionAction`

``` purescript
newtype NetworkConnectionAction
  = NetworkConnectionAction { "Blocked" :: NullOrUndefined (Boolean), "ConnectionDirection" :: NullOrUndefined (String), "LocalPortDetails" :: NullOrUndefined (LocalPortDetails), "Protocol" :: NullOrUndefined (String), "RemoteIpDetails" :: NullOrUndefined (RemoteIpDetails), "RemotePortDetails" :: NullOrUndefined (RemotePortDetails) }
```

Information about the NETWORK_CONNECTION action described in this finding.

#### `NetworkInterface`

``` purescript
newtype NetworkInterface
  = NetworkInterface { "Ipv6Addresses" :: NullOrUndefined (Ipv6Addresses), "PrivateDnsName" :: NullOrUndefined (PrivateDnsName), "PrivateIpAddress" :: NullOrUndefined (PrivateIpAddress), "PrivateIpAddresses" :: NullOrUndefined (PrivateIpAddresses), "PublicDnsName" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String), "SecurityGroups" :: NullOrUndefined (SecurityGroups), "SubnetId" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String) }
```

The network interface information of the EC2 instance.

#### `NetworkInterfaces`

``` purescript
newtype NetworkInterfaces
  = NetworkInterfaces (Array NetworkInterface)
```

The network interface information of the EC2 instance.

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action fill nextToken in the request with the value of NextToken from the previous response to continue listing data.

#### `OrderBy`

``` purescript
newtype OrderBy
  = OrderBy String
```

#### `Organization`

``` purescript
newtype Organization
  = Organization { "Asn" :: NullOrUndefined (String), "AsnOrg" :: NullOrUndefined (String), "Isp" :: NullOrUndefined (String), "Org" :: NullOrUndefined (String) }
```

ISP Organization information of the remote IP address.

#### `PortProbeAction`

``` purescript
newtype PortProbeAction
  = PortProbeAction { "Blocked" :: NullOrUndefined (Boolean), "PortProbeDetails" :: NullOrUndefined (ListOfPortProbeDetail) }
```

Information about the PORT_PROBE action described in this finding.

#### `PortProbeDetail`

``` purescript
newtype PortProbeDetail
  = PortProbeDetail { "LocalPortDetails" :: NullOrUndefined (LocalPortDetails), "RemoteIpDetails" :: NullOrUndefined (RemoteIpDetails) }
```

Details about the port probe finding.

#### `PrivateDnsName`

``` purescript
newtype PrivateDnsName
  = PrivateDnsName String
```

Private DNS name of the EC2 instance.

#### `PrivateIpAddress`

``` purescript
newtype PrivateIpAddress
  = PrivateIpAddress String
```

Private IP address of the EC2 instance.

#### `PrivateIpAddressDetails`

``` purescript
newtype PrivateIpAddressDetails
  = PrivateIpAddressDetails { "PrivateDnsName" :: NullOrUndefined (PrivateDnsName), "PrivateIpAddress" :: NullOrUndefined (PrivateIpAddress) }
```

Other private IP address information of the EC2 instance.

#### `PrivateIpAddresses`

``` purescript
newtype PrivateIpAddresses
  = PrivateIpAddresses (Array PrivateIpAddressDetails)
```

Other private IP address information of the EC2 instance.

#### `ProductCode`

``` purescript
newtype ProductCode
  = ProductCode { "Code" :: NullOrUndefined (String), "ProductType" :: NullOrUndefined (String) }
```

The product code of the EC2 instance.

#### `ProductCodes`

``` purescript
newtype ProductCodes
  = ProductCodes (Array ProductCode)
```

The product code of the EC2 instance.

#### `RemoteIpDetails`

``` purescript
newtype RemoteIpDetails
  = RemoteIpDetails { "City" :: NullOrUndefined (City), "Country" :: NullOrUndefined (Country), "GeoLocation" :: NullOrUndefined (GeoLocation), "IpAddressV4" :: NullOrUndefined (String), "Organization" :: NullOrUndefined (Organization) }
```

Remote IP information of the connection.

#### `RemotePortDetails`

``` purescript
newtype RemotePortDetails
  = RemotePortDetails { "Port" :: NullOrUndefined (Int), "PortName" :: NullOrUndefined (String) }
```

Remote port information of the connection.

#### `Resource`

``` purescript
newtype Resource
  = Resource { "AccessKeyDetails" :: NullOrUndefined (AccessKeyDetails), "InstanceDetails" :: NullOrUndefined (InstanceDetails), "ResourceType" :: NullOrUndefined (String) }
```

The AWS resource associated with the activity that prompted GuardDuty to generate a finding.

#### `SecurityGroup`

``` purescript
newtype SecurityGroup
  = SecurityGroup { "GroupId" :: NullOrUndefined (String), "GroupName" :: NullOrUndefined (String) }
```

Security groups associated with the EC2 instance.

#### `SecurityGroups`

``` purescript
newtype SecurityGroups
  = SecurityGroups (Array SecurityGroup)
```

Security groups associated with the EC2 instance.

#### `Service`

``` purescript
newtype Service
  = Service { "Action" :: NullOrUndefined (Action), "Archived" :: NullOrUndefined (Boolean), "Count" :: NullOrUndefined (Int), "DetectorId" :: NullOrUndefined (DetectorId), "EventFirstSeen" :: NullOrUndefined (String), "EventLastSeen" :: NullOrUndefined (String), "ResourceRole" :: NullOrUndefined (String), "ServiceName" :: NullOrUndefined (String), "UserFeedback" :: NullOrUndefined (String) }
```

Additional information assigned to the generated finding by GuardDuty.

#### `ServiceRole`

``` purescript
newtype ServiceRole
  = ServiceRole String
```

Customer serviceRole name or ARN for accessing customer resources

#### `SortCriteria`

``` purescript
newtype SortCriteria
  = SortCriteria { "AttributeName" :: NullOrUndefined (String), "OrderBy" :: NullOrUndefined (OrderBy) }
```

Represents the criteria used for sorting findings.

#### `StartMonitoringMembersRequest`

``` purescript
newtype StartMonitoringMembersRequest
  = StartMonitoringMembersRequest { "AccountIds" :: NullOrUndefined (AccountIds), "DetectorId" :: String }
```

StartMonitoringMembers request body.

#### `StartMonitoringMembersResponse`

``` purescript
newtype StartMonitoringMembersResponse
  = StartMonitoringMembersResponse { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts) }
```

#### `StopMonitoringMembersRequest`

``` purescript
newtype StopMonitoringMembersRequest
  = StopMonitoringMembersRequest { "AccountIds" :: NullOrUndefined (AccountIds), "DetectorId" :: String }
```

StopMonitoringMembers request body.

#### `StopMonitoringMembersResponse`

``` purescript
newtype StopMonitoringMembersResponse
  = StopMonitoringMembersResponse { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts) }
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String) }
```

A tag of the EC2 instance.

#### `Tags`

``` purescript
newtype Tags
  = Tags (Array Tag)
```

The tags of the EC2 instance.

#### `ThreatIntelSetFormat`

``` purescript
newtype ThreatIntelSetFormat
  = ThreatIntelSetFormat String
```

The format of the threatIntelSet.

#### `ThreatIntelSetId`

``` purescript
newtype ThreatIntelSetId
  = ThreatIntelSetId String
```

The unique identifier for an threat intel set

#### `ThreatIntelSetIds`

``` purescript
newtype ThreatIntelSetIds
  = ThreatIntelSetIds (Array ThreatIntelSetId)
```

The list of the threat intel set IDs

#### `ThreatIntelSetStatus`

``` purescript
newtype ThreatIntelSetStatus
  = ThreatIntelSetStatus String
```

The status of threatIntelSet file uploaded.

#### `UnarchiveFindingsRequest`

``` purescript
newtype UnarchiveFindingsRequest
  = UnarchiveFindingsRequest { "DetectorId" :: String, "FindingIds" :: NullOrUndefined (FindingIds) }
```

UnarchiveFindings request body.

#### `UnarchiveFindingsResponse`

``` purescript
newtype UnarchiveFindingsResponse
  = UnarchiveFindingsResponse {  }
```

#### `UnprocessedAccount`

``` purescript
newtype UnprocessedAccount
  = UnprocessedAccount { "AccountId" :: NullOrUndefined (String), "Result" :: NullOrUndefined (String) }
```

An object containing the unprocessed account and a result string explaining why it was unprocessed.

#### `UnprocessedAccounts`

``` purescript
newtype UnprocessedAccounts
  = UnprocessedAccounts (Array UnprocessedAccount)
```

A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.

#### `UpdateDetectorRequest`

``` purescript
newtype UpdateDetectorRequest
  = UpdateDetectorRequest { "DetectorId" :: String, "Enable" :: NullOrUndefined (Enable) }
```

UpdateDetector request body.

#### `UpdateDetectorResponse`

``` purescript
newtype UpdateDetectorResponse
  = UpdateDetectorResponse {  }
```

#### `UpdateFindingsFeedbackRequest`

``` purescript
newtype UpdateFindingsFeedbackRequest
  = UpdateFindingsFeedbackRequest { "Comments" :: NullOrUndefined (Comments), "DetectorId" :: String, "Feedback" :: NullOrUndefined (Feedback), "FindingIds" :: NullOrUndefined (FindingIds) }
```

UpdateFindingsFeedback request body.

#### `UpdateFindingsFeedbackResponse`

``` purescript
newtype UpdateFindingsFeedbackResponse
  = UpdateFindingsFeedbackResponse {  }
```

#### `UpdateIPSetRequest`

``` purescript
newtype UpdateIPSetRequest
  = UpdateIPSetRequest { "Activate" :: NullOrUndefined (Activate), "DetectorId" :: String, "IpSetId" :: String, "Location" :: NullOrUndefined (Location), "Name" :: NullOrUndefined (Name) }
```

UpdateIPSet request body.

#### `UpdateIPSetResponse`

``` purescript
newtype UpdateIPSetResponse
  = UpdateIPSetResponse {  }
```

#### `UpdateThreatIntelSetRequest`

``` purescript
newtype UpdateThreatIntelSetRequest
  = UpdateThreatIntelSetRequest { "Activate" :: NullOrUndefined (Activate), "DetectorId" :: String, "Location" :: NullOrUndefined (Location), "Name" :: NullOrUndefined (Name), "ThreatIntelSetId" :: String }
```

UpdateThreatIntelSet request body.

#### `UpdateThreatIntelSetResponse`

``` purescript
newtype UpdateThreatIntelSetResponse
  = UpdateThreatIntelSetResponse {  }
```

#### `UpdatedAt`

``` purescript
newtype UpdatedAt
  = UpdatedAt String
```

The first time a resource was created. The format will be ISO-8601.


