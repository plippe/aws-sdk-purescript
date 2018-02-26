

-- | Assess, monitor, manage, and remediate security issues across your AWS infrastructure, applications, and data.
module AWS.GuardDuty where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "GuardDuty" :: String


-- | Accepts the invitation to be monitored by a master GuardDuty account.
acceptInvitation :: forall eff. AcceptInvitationRequest -> Aff (err :: AWS.RequestError | eff) AcceptInvitationResponse
acceptInvitation = AWS.request serviceName "AcceptInvitation" 


-- | Archives Amazon GuardDuty findings specified by the list of finding IDs.
archiveFindings :: forall eff. ArchiveFindingsRequest -> Aff (err :: AWS.RequestError | eff) ArchiveFindingsResponse
archiveFindings = AWS.request serviceName "ArchiveFindings" 


-- | Creates a single Amazon GuardDuty detector. A detector is an object that represents the GuardDuty service. A detector must be created in order for GuardDuty to become operational.
createDetector :: forall eff. CreateDetectorRequest -> Aff (err :: AWS.RequestError | eff) CreateDetectorResponse
createDetector = AWS.request serviceName "CreateDetector" 


-- | Creates a new IPSet - a list of trusted IP addresses that have been whitelisted for secure communication with AWS infrastructure and applications.
createIPSet :: forall eff. CreateIPSetRequest -> Aff (err :: AWS.RequestError | eff) CreateIPSetResponse
createIPSet = AWS.request serviceName "CreateIPSet" 


-- | Creates member accounts of the current AWS account by specifying a list of AWS account IDs. The current AWS account can then invite these members to manage GuardDuty in their accounts.
createMembers :: forall eff. CreateMembersRequest -> Aff (err :: AWS.RequestError | eff) CreateMembersResponse
createMembers = AWS.request serviceName "CreateMembers" 


-- | Generates example findings of types specified by the list of finding types. If 'NULL' is specified for findingTypes, the API generates example findings of all supported finding types.
createSampleFindings :: forall eff. CreateSampleFindingsRequest -> Aff (err :: AWS.RequestError | eff) CreateSampleFindingsResponse
createSampleFindings = AWS.request serviceName "CreateSampleFindings" 


-- | Create a new ThreatIntelSet. ThreatIntelSets consist of known malicious IP addresses. GuardDuty generates findings based on ThreatIntelSets.
createThreatIntelSet :: forall eff. CreateThreatIntelSetRequest -> Aff (err :: AWS.RequestError | eff) CreateThreatIntelSetResponse
createThreatIntelSet = AWS.request serviceName "CreateThreatIntelSet" 


-- | Declines invitations sent to the current member account by AWS account specified by their account IDs.
declineInvitations :: forall eff. DeclineInvitationsRequest -> Aff (err :: AWS.RequestError | eff) DeclineInvitationsResponse
declineInvitations = AWS.request serviceName "DeclineInvitations" 


-- | Deletes a Amazon GuardDuty detector specified by the detector ID.
deleteDetector :: forall eff. DeleteDetectorRequest -> Aff (err :: AWS.RequestError | eff) DeleteDetectorResponse
deleteDetector = AWS.request serviceName "DeleteDetector" 


-- | Deletes the IPSet specified by the IPSet ID.
deleteIPSet :: forall eff. DeleteIPSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteIPSetResponse
deleteIPSet = AWS.request serviceName "DeleteIPSet" 


-- | Deletes invitations sent to the current member account by AWS accounts specified by their account IDs.
deleteInvitations :: forall eff. DeleteInvitationsRequest -> Aff (err :: AWS.RequestError | eff) DeleteInvitationsResponse
deleteInvitations = AWS.request serviceName "DeleteInvitations" 


-- | Deletes GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
deleteMembers :: forall eff. DeleteMembersRequest -> Aff (err :: AWS.RequestError | eff) DeleteMembersResponse
deleteMembers = AWS.request serviceName "DeleteMembers" 


-- | Deletes ThreatIntelSet specified by the ThreatIntelSet ID.
deleteThreatIntelSet :: forall eff. DeleteThreatIntelSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteThreatIntelSetResponse
deleteThreatIntelSet = AWS.request serviceName "DeleteThreatIntelSet" 


-- | Disassociates the current GuardDuty member account from its master account.
disassociateFromMasterAccount :: forall eff. DisassociateFromMasterAccountRequest -> Aff (err :: AWS.RequestError | eff) DisassociateFromMasterAccountResponse
disassociateFromMasterAccount = AWS.request serviceName "DisassociateFromMasterAccount" 


-- | Disassociates GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
disassociateMembers :: forall eff. DisassociateMembersRequest -> Aff (err :: AWS.RequestError | eff) DisassociateMembersResponse
disassociateMembers = AWS.request serviceName "DisassociateMembers" 


-- | Retrieves an Amazon GuardDuty detector specified by the detectorId.
getDetector :: forall eff. GetDetectorRequest -> Aff (err :: AWS.RequestError | eff) GetDetectorResponse
getDetector = AWS.request serviceName "GetDetector" 


-- | Describes Amazon GuardDuty findings specified by finding IDs.
getFindings :: forall eff. GetFindingsRequest -> Aff (err :: AWS.RequestError | eff) GetFindingsResponse
getFindings = AWS.request serviceName "GetFindings" 


-- | Lists Amazon GuardDuty findings' statistics for the specified detector ID.
getFindingsStatistics :: forall eff. GetFindingsStatisticsRequest -> Aff (err :: AWS.RequestError | eff) GetFindingsStatisticsResponse
getFindingsStatistics = AWS.request serviceName "GetFindingsStatistics" 


-- | Retrieves the IPSet specified by the IPSet ID.
getIPSet :: forall eff. GetIPSetRequest -> Aff (err :: AWS.RequestError | eff) GetIPSetResponse
getIPSet = AWS.request serviceName "GetIPSet" 


-- | Returns the count of all GuardDuty membership invitations that were sent to the current member account except the currently accepted invitation.
getInvitationsCount :: forall eff. GetInvitationsCountRequest -> Aff (err :: AWS.RequestError | eff) GetInvitationsCountResponse
getInvitationsCount = AWS.request serviceName "GetInvitationsCount" 


-- | Provides the details for the GuardDuty master account to the current GuardDuty member account.
getMasterAccount :: forall eff. GetMasterAccountRequest -> Aff (err :: AWS.RequestError | eff) GetMasterAccountResponse
getMasterAccount = AWS.request serviceName "GetMasterAccount" 


-- | Retrieves GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
getMembers :: forall eff. GetMembersRequest -> Aff (err :: AWS.RequestError | eff) GetMembersResponse
getMembers = AWS.request serviceName "GetMembers" 


-- | Retrieves the ThreatIntelSet that is specified by the ThreatIntelSet ID.
getThreatIntelSet :: forall eff. GetThreatIntelSetRequest -> Aff (err :: AWS.RequestError | eff) GetThreatIntelSetResponse
getThreatIntelSet = AWS.request serviceName "GetThreatIntelSet" 


-- | Invites other AWS accounts (created as members of the current AWS account by CreateMembers) to enable GuardDuty and allow the current AWS account to view and manage these accounts' GuardDuty findings on their behalf as the master account.
inviteMembers :: forall eff. InviteMembersRequest -> Aff (err :: AWS.RequestError | eff) InviteMembersResponse
inviteMembers = AWS.request serviceName "InviteMembers" 


-- | Lists detectorIds of all the existing Amazon GuardDuty detector resources.
listDetectors :: forall eff. ListDetectorsRequest -> Aff (err :: AWS.RequestError | eff) ListDetectorsResponse
listDetectors = AWS.request serviceName "ListDetectors" 


-- | Lists Amazon GuardDuty findings for the specified detector ID.
listFindings :: forall eff. ListFindingsRequest -> Aff (err :: AWS.RequestError | eff) ListFindingsResponse
listFindings = AWS.request serviceName "ListFindings" 


-- | Lists the IPSets of the GuardDuty service specified by the detector ID.
listIPSets :: forall eff. ListIPSetsRequest -> Aff (err :: AWS.RequestError | eff) ListIPSetsResponse
listIPSets = AWS.request serviceName "ListIPSets" 


-- | Lists all GuardDuty membership invitations that were sent to the current AWS account.
listInvitations :: forall eff. ListInvitationsRequest -> Aff (err :: AWS.RequestError | eff) ListInvitationsResponse
listInvitations = AWS.request serviceName "ListInvitations" 


-- | Lists details about all member accounts for the current GuardDuty master account.
listMembers :: forall eff. ListMembersRequest -> Aff (err :: AWS.RequestError | eff) ListMembersResponse
listMembers = AWS.request serviceName "ListMembers" 


-- | Lists the ThreatIntelSets of the GuardDuty service specified by the detector ID.
listThreatIntelSets :: forall eff. ListThreatIntelSetsRequest -> Aff (err :: AWS.RequestError | eff) ListThreatIntelSetsResponse
listThreatIntelSets = AWS.request serviceName "ListThreatIntelSets" 


-- | Re-enables GuardDuty to monitor findings of the member accounts specified by the account IDs. A master GuardDuty account can run this command after disabling GuardDuty from monitoring these members' findings by running StopMonitoringMembers.
startMonitoringMembers :: forall eff. StartMonitoringMembersRequest -> Aff (err :: AWS.RequestError | eff) StartMonitoringMembersResponse
startMonitoringMembers = AWS.request serviceName "StartMonitoringMembers" 


-- | Disables GuardDuty from monitoring findings of the member accounts specified by the account IDs. After running this command, a master GuardDuty account can run StartMonitoringMembers to re-enable GuardDuty to monitor these members' findings.
stopMonitoringMembers :: forall eff. StopMonitoringMembersRequest -> Aff (err :: AWS.RequestError | eff) StopMonitoringMembersResponse
stopMonitoringMembers = AWS.request serviceName "StopMonitoringMembers" 


-- | Unarchives Amazon GuardDuty findings specified by the list of finding IDs.
unarchiveFindings :: forall eff. UnarchiveFindingsRequest -> Aff (err :: AWS.RequestError | eff) UnarchiveFindingsResponse
unarchiveFindings = AWS.request serviceName "UnarchiveFindings" 


-- | Updates an Amazon GuardDuty detector specified by the detectorId.
updateDetector :: forall eff. UpdateDetectorRequest -> Aff (err :: AWS.RequestError | eff) UpdateDetectorResponse
updateDetector = AWS.request serviceName "UpdateDetector" 


-- | Marks specified Amazon GuardDuty findings as useful or not useful.
updateFindingsFeedback :: forall eff. UpdateFindingsFeedbackRequest -> Aff (err :: AWS.RequestError | eff) UpdateFindingsFeedbackResponse
updateFindingsFeedback = AWS.request serviceName "UpdateFindingsFeedback" 


-- | Updates the IPSet specified by the IPSet ID.
updateIPSet :: forall eff. UpdateIPSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateIPSetResponse
updateIPSet = AWS.request serviceName "UpdateIPSet" 


-- | Updates the ThreatIntelSet specified by ThreatIntelSet ID.
updateThreatIntelSet :: forall eff. UpdateThreatIntelSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateThreatIntelSetResponse
updateThreatIntelSet = AWS.request serviceName "UpdateThreatIntelSet" 


-- | AcceptInvitation request body.
newtype AcceptInvitationRequest = AcceptInvitationRequest 
  { "DetectorId" :: (String)
  , "InvitationId" :: NullOrUndefined (InvitationId)
  , "MasterId" :: NullOrUndefined (MasterId)
  }


newtype AcceptInvitationResponse = AcceptInvitationResponse 
  { 
  }


-- | The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
newtype AccessKeyDetails = AccessKeyDetails 
  { "AccessKeyId" :: NullOrUndefined (String)
  , "PrincipalId" :: NullOrUndefined (String)
  , "UserName" :: NullOrUndefined (String)
  , "UserType" :: NullOrUndefined (String)
  }


-- | An object containing the member's accountId and email address.
newtype AccountDetail = AccountDetail 
  { "AccountId" :: NullOrUndefined (AccountId)
  , "Email" :: NullOrUndefined (Email)
  }


-- | A list of account/email pairs.
newtype AccountDetails = AccountDetails (Array AccountDetail)


-- | AWS account ID.
newtype AccountId = AccountId String


-- | A list of account IDs.
newtype AccountIds = AccountIds (Array String)


-- | Information about the activity described in a finding.
newtype Action = Action 
  { "ActionType" :: NullOrUndefined (String)
  , "AwsApiCallAction" :: NullOrUndefined (AwsApiCallAction)
  , "DnsRequestAction" :: NullOrUndefined (DnsRequestAction)
  , "NetworkConnectionAction" :: NullOrUndefined (NetworkConnectionAction)
  , "PortProbeAction" :: NullOrUndefined (PortProbeAction)
  }


-- | Whether we should start processing the list immediately or not.
newtype Activate = Activate Boolean


-- | ArchiveFindings request body.
newtype ArchiveFindingsRequest = ArchiveFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingIds" :: NullOrUndefined (FindingIds)
  }


newtype ArchiveFindingsResponse = ArchiveFindingsResponse 
  { 
  }


-- | Information about the AWS_API_CALL action described in this finding.
newtype AwsApiCallAction = AwsApiCallAction 
  { "Api" :: NullOrUndefined (String)
  , "CallerType" :: NullOrUndefined (String)
  , "DomainDetails" :: NullOrUndefined (DomainDetails)
  , "RemoteIpDetails" :: NullOrUndefined (RemoteIpDetails)
  , "ServiceName" :: NullOrUndefined (String)
  }


-- | Error response object.
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  }


-- | City information of the remote IP address.
newtype City = City 
  { "CityName" :: NullOrUndefined (String)
  }


-- | Additional feedback about the GuardDuty findings.
newtype Comments = Comments String


-- | Finding attribute (for example, accountId) for which conditions and values must be specified when querying findings.
newtype Condition = Condition 
  { "Eq" :: NullOrUndefined (Eq)
  , "Gt" :: NullOrUndefined (Int)
  , "Gte" :: NullOrUndefined (Int)
  , "Lt" :: NullOrUndefined (Int)
  , "Lte" :: NullOrUndefined (Int)
  , "Neq" :: NullOrUndefined (Neq)
  }


-- | The count of findings for the given severity.
newtype CountBySeverityFindingStatistic = CountBySeverityFindingStatistic Int


-- | Country information of the remote IP address.
newtype Country = Country 
  { "CountryCode" :: NullOrUndefined (String)
  , "CountryName" :: NullOrUndefined (String)
  }


-- | CreateDetector request body.
newtype CreateDetectorRequest = CreateDetectorRequest 
  { "Enable" :: NullOrUndefined (Enable)
  }


newtype CreateDetectorResponse = CreateDetectorResponse 
  { "DetectorId" :: NullOrUndefined (DetectorId)
  }


-- | CreateIPSet request body.
newtype CreateIPSetRequest = CreateIPSetRequest 
  { "Activate" :: NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "Format" :: NullOrUndefined (IpSetFormat)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  }


newtype CreateIPSetResponse = CreateIPSetResponse 
  { "IpSetId" :: NullOrUndefined (IpSetId)
  }


-- | CreateMembers request body.
newtype CreateMembersRequest = CreateMembersRequest 
  { "AccountDetails" :: NullOrUndefined (AccountDetails)
  , "DetectorId" :: (String)
  }


newtype CreateMembersResponse = CreateMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }


-- | CreateSampleFindings request body.
newtype CreateSampleFindingsRequest = CreateSampleFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingTypes" :: NullOrUndefined (FindingTypes)
  }


newtype CreateSampleFindingsResponse = CreateSampleFindingsResponse 
  { 
  }


-- | CreateThreatIntelSet request body.
newtype CreateThreatIntelSetRequest = CreateThreatIntelSetRequest 
  { "Activate" :: NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "Format" :: NullOrUndefined (ThreatIntelSetFormat)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  }


newtype CreateThreatIntelSetResponse = CreateThreatIntelSetResponse 
  { "ThreatIntelSetId" :: NullOrUndefined (ThreatIntelSetId)
  }


-- | The first time a resource was created. The format will be ISO-8601.
newtype CreatedAt = CreatedAt String


-- | DeclineInvitations request body.
newtype DeclineInvitationsRequest = DeclineInvitationsRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  }


newtype DeclineInvitationsResponse = DeclineInvitationsResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }


newtype DeleteDetectorRequest = DeleteDetectorRequest 
  { "DetectorId" :: (String)
  }


newtype DeleteDetectorResponse = DeleteDetectorResponse 
  { 
  }


newtype DeleteIPSetRequest = DeleteIPSetRequest 
  { "DetectorId" :: (String)
  , "IpSetId" :: (String)
  }


newtype DeleteIPSetResponse = DeleteIPSetResponse 
  { 
  }


-- | DeleteInvitations request body.
newtype DeleteInvitationsRequest = DeleteInvitationsRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  }


newtype DeleteInvitationsResponse = DeleteInvitationsResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }


-- | DeleteMembers request body.
newtype DeleteMembersRequest = DeleteMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }


newtype DeleteMembersResponse = DeleteMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }


newtype DeleteThreatIntelSetRequest = DeleteThreatIntelSetRequest 
  { "DetectorId" :: (String)
  , "ThreatIntelSetId" :: (String)
  }


newtype DeleteThreatIntelSetResponse = DeleteThreatIntelSetResponse 
  { 
  }


-- | The unique identifier for a detector.
newtype DetectorId = DetectorId String


-- | A list of detector Ids.
newtype DetectorIds = DetectorIds (Array DetectorId)


-- | The status of detector.
newtype DetectorStatus = DetectorStatus String


newtype DisassociateFromMasterAccountRequest = DisassociateFromMasterAccountRequest 
  { "DetectorId" :: (String)
  }


newtype DisassociateFromMasterAccountResponse = DisassociateFromMasterAccountResponse 
  { 
  }


-- | DisassociateMembers request body.
newtype DisassociateMembersRequest = DisassociateMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }


newtype DisassociateMembersResponse = DisassociateMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }


-- | Information about the DNS_REQUEST action described in this finding.
newtype DnsRequestAction = DnsRequestAction 
  { "Domain" :: NullOrUndefined (Domain)
  }


-- | A domain name.
newtype Domain = Domain String


-- | Domain information for the AWS API call.
newtype DomainDetails = DomainDetails 
  { 
  }


-- | Member account's email address.
newtype Email = Email String


-- | A boolean value that specifies whether the detector is to be enabled.
newtype Enable = Enable Boolean


-- | Represents the equal condition to be applied to a single field when querying for findings.
newtype Eq = Eq (Array String)


-- | Error response object.
newtype ErrorResponse = ErrorResponse 
  { "Message" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  }


-- | Finding Feedback Value
newtype Feedback = Feedback String


-- | Representation of a abnormal or suspicious activity.
newtype Finding = Finding 
  { "AccountId" :: NullOrUndefined (String)
  , "Arn" :: NullOrUndefined (String)
  , "Confidence" :: NullOrUndefined (Number)
  , "CreatedAt" :: NullOrUndefined (CreatedAt)
  , "Description" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Partition" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  , "Resource" :: NullOrUndefined (Resource)
  , "SchemaVersion" :: NullOrUndefined (String)
  , "Service" :: NullOrUndefined (Service)
  , "Severity" :: NullOrUndefined (Number)
  , "Title" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  , "UpdatedAt" :: NullOrUndefined (UpdatedAt)
  }


-- | Represents the criteria used for querying findings.
newtype FindingCriteria = FindingCriteria 
  { "Criterion" :: NullOrUndefined (MapOfCondition)
  }


-- | The unique identifier for the Finding
newtype FindingId = FindingId String


-- | The list of the Findings.
newtype FindingIds = FindingIds (Array FindingId)


-- | The types of finding statistics.
newtype FindingStatisticType = FindingStatisticType String


-- | The list of the finding statistics.
newtype FindingStatisticTypes = FindingStatisticTypes (Array FindingStatisticType)


-- | Finding statistics object.
newtype FindingStatistics = FindingStatistics 
  { "CountBySeverity" :: NullOrUndefined (MapOfCountBySeverityFindingStatistic)
  }


-- | The finding type for the finding
newtype FindingType = FindingType String


-- | The list of the finding types.
newtype FindingTypes = FindingTypes (Array FindingType)


-- | A list of findings.
newtype Findings = Findings (Array Finding)


-- | Location information of the remote IP address.
newtype GeoLocation = GeoLocation 
  { "Lat" :: NullOrUndefined (Number)
  , "Lon" :: NullOrUndefined (Number)
  }


newtype GetDetectorRequest = GetDetectorRequest 
  { "DetectorId" :: (String)
  }


newtype GetDetectorResponse = GetDetectorResponse 
  { "CreatedAt" :: NullOrUndefined (CreatedAt)
  , "ServiceRole" :: NullOrUndefined (ServiceRole)
  , "Status" :: NullOrUndefined (DetectorStatus)
  , "UpdatedAt" :: NullOrUndefined (UpdatedAt)
  }


-- | GetFindings request body.
newtype GetFindingsRequest = GetFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingIds" :: NullOrUndefined (FindingIds)
  , "SortCriteria" :: NullOrUndefined (SortCriteria)
  }


newtype GetFindingsResponse = GetFindingsResponse 
  { "Findings" :: NullOrUndefined (Findings)
  }


-- | GetFindingsStatistics request body.
newtype GetFindingsStatisticsRequest = GetFindingsStatisticsRequest 
  { "DetectorId" :: (String)
  , "FindingCriteria" :: NullOrUndefined (FindingCriteria)
  , "FindingStatisticTypes" :: NullOrUndefined (FindingStatisticTypes)
  }


newtype GetFindingsStatisticsResponse = GetFindingsStatisticsResponse 
  { "FindingStatistics" :: NullOrUndefined (FindingStatistics)
  }


newtype GetIPSetRequest = GetIPSetRequest 
  { "DetectorId" :: (String)
  , "IpSetId" :: (String)
  }


newtype GetIPSetResponse = GetIPSetResponse 
  { "Format" :: NullOrUndefined (IpSetFormat)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  , "Status" :: NullOrUndefined (IpSetStatus)
  }


newtype GetInvitationsCountRequest = GetInvitationsCountRequest 
  { 
  }


newtype GetInvitationsCountResponse = GetInvitationsCountResponse 
  { "InvitationsCount" :: NullOrUndefined (Int)
  }


newtype GetMasterAccountRequest = GetMasterAccountRequest 
  { "DetectorId" :: (String)
  }


newtype GetMasterAccountResponse = GetMasterAccountResponse 
  { "Master" :: NullOrUndefined (Master)
  }


-- | GetMembers request body.
newtype GetMembersRequest = GetMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }


newtype GetMembersResponse = GetMembersResponse 
  { "Members" :: NullOrUndefined (Members)
  , "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }


newtype GetThreatIntelSetRequest = GetThreatIntelSetRequest 
  { "DetectorId" :: (String)
  , "ThreatIntelSetId" :: (String)
  }


newtype GetThreatIntelSetResponse = GetThreatIntelSetResponse 
  { "Format" :: NullOrUndefined (ThreatIntelSetFormat)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  , "Status" :: NullOrUndefined (ThreatIntelSetStatus)
  }


-- | The profile information of the EC2 instance.
newtype IamInstanceProfile = IamInstanceProfile 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  }


-- | The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.
newtype InstanceDetails = InstanceDetails 
  { "AvailabilityZone" :: NullOrUndefined (String)
  , "IamInstanceProfile" :: NullOrUndefined (IamInstanceProfile)
  , "ImageId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "InstanceState" :: NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined (String)
  , "LaunchTime" :: NullOrUndefined (String)
  , "NetworkInterfaces" :: NullOrUndefined (NetworkInterfaces)
  , "Platform" :: NullOrUndefined (String)
  , "ProductCodes" :: NullOrUndefined (ProductCodes)
  , "Tags" :: NullOrUndefined (Tags)
  }


-- | Error response object.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  }


-- | Invitation from an AWS account to become the current account's master.
newtype Invitation = Invitation 
  { "AccountId" :: NullOrUndefined (String)
  , "InvitationId" :: NullOrUndefined (InvitationId)
  , "InvitedAt" :: NullOrUndefined (InvitedAt)
  , "RelationshipStatus" :: NullOrUndefined (String)
  }


-- | This value is used to validate the master account to the member account.
newtype InvitationId = InvitationId String


-- | A list of invitation descriptions.
newtype Invitations = Invitations (Array Invitation)


-- | InviteMembers request body.
newtype InviteMembersRequest = InviteMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  , "Message" :: NullOrUndefined (Message)
  }


newtype InviteMembersResponse = InviteMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }


-- | Timestamp at which a member has been invited. The format will be ISO-8601.
newtype InvitedAt = InvitedAt String


-- | The format of the ipSet.
newtype IpSetFormat = IpSetFormat String


-- | The unique identifier for an IP Set
newtype IpSetId = IpSetId String


-- | A list of the IP set IDs
newtype IpSetIds = IpSetIds (Array IpSetId)


-- | The status of ipSet file uploaded.
newtype IpSetStatus = IpSetStatus String


-- | IpV6 address of the EC2 instance.
newtype Ipv6Address = Ipv6Address String


-- | A list of EC2 instance IPv6 address information.
newtype Ipv6Addresses = Ipv6Addresses (Array Ipv6Address)


newtype ListDetectorsRequest = ListDetectorsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListDetectorsResponse = ListDetectorsResponse 
  { "DetectorIds" :: NullOrUndefined (DetectorIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | ListFindings request body.
newtype ListFindingsRequest = ListFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingCriteria" :: NullOrUndefined (FindingCriteria)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "SortCriteria" :: NullOrUndefined (SortCriteria)
  }


newtype ListFindingsResponse = ListFindingsResponse 
  { "FindingIds" :: NullOrUndefined (FindingIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListIPSetsRequest = ListIPSetsRequest 
  { "DetectorId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListIPSetsResponse = ListIPSetsResponse 
  { "IpSetIds" :: NullOrUndefined (IpSetIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListInvitationsRequest = ListInvitationsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListInvitationsResponse = ListInvitationsResponse 
  { "Invitations" :: NullOrUndefined (Invitations)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListMembersRequest = ListMembersRequest 
  { "DetectorId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  , "OnlyAssociated" :: NullOrUndefined (String)
  }


newtype ListMembersResponse = ListMembersResponse 
  { "Members" :: NullOrUndefined (Members)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListOfPortProbeDetail = ListOfPortProbeDetail (Array PortProbeDetail)


newtype ListThreatIntelSetsRequest = ListThreatIntelSetsRequest 
  { "DetectorId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListThreatIntelSetsResponse = ListThreatIntelSetsResponse 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "ThreatIntelSetIds" :: NullOrUndefined (ThreatIntelSetIds)
  }


-- | Local port information of the connection.
newtype LocalPortDetails = LocalPortDetails 
  { "Port" :: NullOrUndefined (Int)
  , "PortName" :: NullOrUndefined (String)
  }


-- | The location of the S3 bucket where the list resides. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)
newtype Location = Location String


newtype MapOfCondition = MapOfCondition (Map String Condition)


newtype MapOfCountBySeverityFindingStatistic = MapOfCountBySeverityFindingStatistic (Map String CountBySeverityFindingStatistic)


-- | Contains details about the master account.
newtype Master = Master 
  { "AccountId" :: NullOrUndefined (String)
  , "InvitationId" :: NullOrUndefined (InvitationId)
  , "InvitedAt" :: NullOrUndefined (InvitedAt)
  , "RelationshipStatus" :: NullOrUndefined (String)
  }


-- | The master account ID.
newtype MasterId = MasterId String


newtype MaxResults = MaxResults Int


-- | Contains details about the member account.
newtype Member = Member 
  { "AccountId" :: NullOrUndefined (AccountId)
  , "DetectorId" :: NullOrUndefined (DetectorId)
  , "Email" :: NullOrUndefined (Email)
  , "InvitedAt" :: NullOrUndefined (InvitedAt)
  , "MasterId" :: NullOrUndefined (MasterId)
  , "RelationshipStatus" :: NullOrUndefined (String)
  , "UpdatedAt" :: NullOrUndefined (UpdatedAt)
  }


-- | A list of member descriptions.
newtype Members = Members (Array Member)


-- | The invitation message that you want to send to the accounts that you're inviting to GuardDuty as members.
newtype Message = Message String


-- | The user-friendly name to identify the list.
newtype Name = Name String


-- | Represents the not equal condition to be applied to a single field when querying for findings.
newtype Neq = Neq (Array String)


-- | Information about the NETWORK_CONNECTION action described in this finding.
newtype NetworkConnectionAction = NetworkConnectionAction 
  { "Blocked" :: NullOrUndefined (Boolean)
  , "ConnectionDirection" :: NullOrUndefined (String)
  , "LocalPortDetails" :: NullOrUndefined (LocalPortDetails)
  , "Protocol" :: NullOrUndefined (String)
  , "RemoteIpDetails" :: NullOrUndefined (RemoteIpDetails)
  , "RemotePortDetails" :: NullOrUndefined (RemotePortDetails)
  }


-- | The network interface information of the EC2 instance.
newtype NetworkInterface = NetworkInterface 
  { "Ipv6Addresses" :: NullOrUndefined (Ipv6Addresses)
  , "PrivateDnsName" :: NullOrUndefined (PrivateDnsName)
  , "PrivateIpAddress" :: NullOrUndefined (PrivateIpAddress)
  , "PrivateIpAddresses" :: NullOrUndefined (PrivateIpAddresses)
  , "PublicDnsName" :: NullOrUndefined (String)
  , "PublicIp" :: NullOrUndefined (String)
  , "SecurityGroups" :: NullOrUndefined (SecurityGroups)
  , "SubnetId" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  }


-- | The network interface information of the EC2 instance.
newtype NetworkInterfaces = NetworkInterfaces (Array NetworkInterface)


-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
newtype NextToken = NextToken String


newtype OrderBy = OrderBy String


-- | ISP Organization information of the remote IP address.
newtype Organization = Organization 
  { "Asn" :: NullOrUndefined (String)
  , "AsnOrg" :: NullOrUndefined (String)
  , "Isp" :: NullOrUndefined (String)
  , "Org" :: NullOrUndefined (String)
  }


-- | Information about the PORT_PROBE action described in this finding.
newtype PortProbeAction = PortProbeAction 
  { "Blocked" :: NullOrUndefined (Boolean)
  , "PortProbeDetails" :: NullOrUndefined (ListOfPortProbeDetail)
  }


-- | Details about the port probe finding.
newtype PortProbeDetail = PortProbeDetail 
  { "LocalPortDetails" :: NullOrUndefined (LocalPortDetails)
  , "RemoteIpDetails" :: NullOrUndefined (RemoteIpDetails)
  }


-- | Private DNS name of the EC2 instance.
newtype PrivateDnsName = PrivateDnsName String


-- | Private IP address of the EC2 instance.
newtype PrivateIpAddress = PrivateIpAddress String


-- | Other private IP address information of the EC2 instance.
newtype PrivateIpAddressDetails = PrivateIpAddressDetails 
  { "PrivateDnsName" :: NullOrUndefined (PrivateDnsName)
  , "PrivateIpAddress" :: NullOrUndefined (PrivateIpAddress)
  }


-- | Other private IP address information of the EC2 instance.
newtype PrivateIpAddresses = PrivateIpAddresses (Array PrivateIpAddressDetails)


-- | The product code of the EC2 instance.
newtype ProductCode = ProductCode 
  { "Code" :: NullOrUndefined (String)
  , "ProductType" :: NullOrUndefined (String)
  }


-- | The product code of the EC2 instance.
newtype ProductCodes = ProductCodes (Array ProductCode)


-- | Remote IP information of the connection.
newtype RemoteIpDetails = RemoteIpDetails 
  { "City" :: NullOrUndefined (City)
  , "Country" :: NullOrUndefined (Country)
  , "GeoLocation" :: NullOrUndefined (GeoLocation)
  , "IpAddressV4" :: NullOrUndefined (String)
  , "Organization" :: NullOrUndefined (Organization)
  }


-- | Remote port information of the connection.
newtype RemotePortDetails = RemotePortDetails 
  { "Port" :: NullOrUndefined (Int)
  , "PortName" :: NullOrUndefined (String)
  }


-- | The AWS resource associated with the activity that prompted GuardDuty to generate a finding.
newtype Resource = Resource 
  { "AccessKeyDetails" :: NullOrUndefined (AccessKeyDetails)
  , "InstanceDetails" :: NullOrUndefined (InstanceDetails)
  , "ResourceType" :: NullOrUndefined (String)
  }


-- | Security groups associated with the EC2 instance.
newtype SecurityGroup = SecurityGroup 
  { "GroupId" :: NullOrUndefined (String)
  , "GroupName" :: NullOrUndefined (String)
  }


-- | Security groups associated with the EC2 instance.
newtype SecurityGroups = SecurityGroups (Array SecurityGroup)


-- | Additional information assigned to the generated finding by GuardDuty.
newtype Service = Service 
  { "Action" :: NullOrUndefined (Action)
  , "Archived" :: NullOrUndefined (Boolean)
  , "Count" :: NullOrUndefined (Int)
  , "DetectorId" :: NullOrUndefined (DetectorId)
  , "EventFirstSeen" :: NullOrUndefined (String)
  , "EventLastSeen" :: NullOrUndefined (String)
  , "ResourceRole" :: NullOrUndefined (String)
  , "ServiceName" :: NullOrUndefined (String)
  , "UserFeedback" :: NullOrUndefined (String)
  }


-- | Customer serviceRole name or ARN for accessing customer resources
newtype ServiceRole = ServiceRole String


-- | Represents the criteria used for sorting findings.
newtype SortCriteria = SortCriteria 
  { "AttributeName" :: NullOrUndefined (String)
  , "OrderBy" :: NullOrUndefined (OrderBy)
  }


-- | StartMonitoringMembers request body.
newtype StartMonitoringMembersRequest = StartMonitoringMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }


newtype StartMonitoringMembersResponse = StartMonitoringMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }


-- | StopMonitoringMembers request body.
newtype StopMonitoringMembersRequest = StopMonitoringMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }


newtype StopMonitoringMembersResponse = StopMonitoringMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }


-- | A tag of the EC2 instance.
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }


-- | The tags of the EC2 instance.
newtype Tags = Tags (Array Tag)


-- | The format of the threatIntelSet.
newtype ThreatIntelSetFormat = ThreatIntelSetFormat String


-- | The unique identifier for an threat intel set
newtype ThreatIntelSetId = ThreatIntelSetId String


-- | The list of the threat intel set IDs
newtype ThreatIntelSetIds = ThreatIntelSetIds (Array ThreatIntelSetId)


-- | The status of threatIntelSet file uploaded.
newtype ThreatIntelSetStatus = ThreatIntelSetStatus String


-- | UnarchiveFindings request body.
newtype UnarchiveFindingsRequest = UnarchiveFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingIds" :: NullOrUndefined (FindingIds)
  }


newtype UnarchiveFindingsResponse = UnarchiveFindingsResponse 
  { 
  }


-- | An object containing the unprocessed account and a result string explaining why it was unprocessed.
newtype UnprocessedAccount = UnprocessedAccount 
  { "AccountId" :: NullOrUndefined (String)
  , "Result" :: NullOrUndefined (String)
  }


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
newtype UnprocessedAccounts = UnprocessedAccounts (Array UnprocessedAccount)


-- | UpdateDetector request body.
newtype UpdateDetectorRequest = UpdateDetectorRequest 
  { "DetectorId" :: (String)
  , "Enable" :: NullOrUndefined (Enable)
  }


newtype UpdateDetectorResponse = UpdateDetectorResponse 
  { 
  }


-- | UpdateFindingsFeedback request body.
newtype UpdateFindingsFeedbackRequest = UpdateFindingsFeedbackRequest 
  { "Comments" :: NullOrUndefined (Comments)
  , "DetectorId" :: (String)
  , "Feedback" :: NullOrUndefined (Feedback)
  , "FindingIds" :: NullOrUndefined (FindingIds)
  }


newtype UpdateFindingsFeedbackResponse = UpdateFindingsFeedbackResponse 
  { 
  }


-- | UpdateIPSet request body.
newtype UpdateIPSetRequest = UpdateIPSetRequest 
  { "Activate" :: NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "IpSetId" :: (String)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  }


newtype UpdateIPSetResponse = UpdateIPSetResponse 
  { 
  }


-- | UpdateThreatIntelSet request body.
newtype UpdateThreatIntelSetRequest = UpdateThreatIntelSetRequest 
  { "Activate" :: NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  , "ThreatIntelSetId" :: (String)
  }


newtype UpdateThreatIntelSetResponse = UpdateThreatIntelSetResponse 
  { 
  }


-- | The first time a resource was created. The format will be ISO-8601.
newtype UpdatedAt = UpdatedAt String
