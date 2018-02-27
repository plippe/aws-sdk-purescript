

-- | Assess, monitor, manage, and remediate security issues across your AWS infrastructure, applications, and data.
module AWS.GuardDuty where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "GuardDuty" :: String


-- | Accepts the invitation to be monitored by a master GuardDuty account.
acceptInvitation :: forall eff. AcceptInvitationRequest -> Aff (err :: AWS.RequestError | eff) AcceptInvitationResponse
acceptInvitation = AWS.request serviceName "acceptInvitation" 


-- | Archives Amazon GuardDuty findings specified by the list of finding IDs.
archiveFindings :: forall eff. ArchiveFindingsRequest -> Aff (err :: AWS.RequestError | eff) ArchiveFindingsResponse
archiveFindings = AWS.request serviceName "archiveFindings" 


-- | Creates a single Amazon GuardDuty detector. A detector is an object that represents the GuardDuty service. A detector must be created in order for GuardDuty to become operational.
createDetector :: forall eff. CreateDetectorRequest -> Aff (err :: AWS.RequestError | eff) CreateDetectorResponse
createDetector = AWS.request serviceName "createDetector" 


-- | Creates a new IPSet - a list of trusted IP addresses that have been whitelisted for secure communication with AWS infrastructure and applications.
createIPSet :: forall eff. CreateIPSetRequest -> Aff (err :: AWS.RequestError | eff) CreateIPSetResponse
createIPSet = AWS.request serviceName "createIPSet" 


-- | Creates member accounts of the current AWS account by specifying a list of AWS account IDs. The current AWS account can then invite these members to manage GuardDuty in their accounts.
createMembers :: forall eff. CreateMembersRequest -> Aff (err :: AWS.RequestError | eff) CreateMembersResponse
createMembers = AWS.request serviceName "createMembers" 


-- | Generates example findings of types specified by the list of finding types. If 'NULL' is specified for findingTypes, the API generates example findings of all supported finding types.
createSampleFindings :: forall eff. CreateSampleFindingsRequest -> Aff (err :: AWS.RequestError | eff) CreateSampleFindingsResponse
createSampleFindings = AWS.request serviceName "createSampleFindings" 


-- | Create a new ThreatIntelSet. ThreatIntelSets consist of known malicious IP addresses. GuardDuty generates findings based on ThreatIntelSets.
createThreatIntelSet :: forall eff. CreateThreatIntelSetRequest -> Aff (err :: AWS.RequestError | eff) CreateThreatIntelSetResponse
createThreatIntelSet = AWS.request serviceName "createThreatIntelSet" 


-- | Declines invitations sent to the current member account by AWS account specified by their account IDs.
declineInvitations :: forall eff. DeclineInvitationsRequest -> Aff (err :: AWS.RequestError | eff) DeclineInvitationsResponse
declineInvitations = AWS.request serviceName "declineInvitations" 


-- | Deletes a Amazon GuardDuty detector specified by the detector ID.
deleteDetector :: forall eff. DeleteDetectorRequest -> Aff (err :: AWS.RequestError | eff) DeleteDetectorResponse
deleteDetector = AWS.request serviceName "deleteDetector" 


-- | Deletes the IPSet specified by the IPSet ID.
deleteIPSet :: forall eff. DeleteIPSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteIPSetResponse
deleteIPSet = AWS.request serviceName "deleteIPSet" 


-- | Deletes invitations sent to the current member account by AWS accounts specified by their account IDs.
deleteInvitations :: forall eff. DeleteInvitationsRequest -> Aff (err :: AWS.RequestError | eff) DeleteInvitationsResponse
deleteInvitations = AWS.request serviceName "deleteInvitations" 


-- | Deletes GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
deleteMembers :: forall eff. DeleteMembersRequest -> Aff (err :: AWS.RequestError | eff) DeleteMembersResponse
deleteMembers = AWS.request serviceName "deleteMembers" 


-- | Deletes ThreatIntelSet specified by the ThreatIntelSet ID.
deleteThreatIntelSet :: forall eff. DeleteThreatIntelSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteThreatIntelSetResponse
deleteThreatIntelSet = AWS.request serviceName "deleteThreatIntelSet" 


-- | Disassociates the current GuardDuty member account from its master account.
disassociateFromMasterAccount :: forall eff. DisassociateFromMasterAccountRequest -> Aff (err :: AWS.RequestError | eff) DisassociateFromMasterAccountResponse
disassociateFromMasterAccount = AWS.request serviceName "disassociateFromMasterAccount" 


-- | Disassociates GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
disassociateMembers :: forall eff. DisassociateMembersRequest -> Aff (err :: AWS.RequestError | eff) DisassociateMembersResponse
disassociateMembers = AWS.request serviceName "disassociateMembers" 


-- | Retrieves an Amazon GuardDuty detector specified by the detectorId.
getDetector :: forall eff. GetDetectorRequest -> Aff (err :: AWS.RequestError | eff) GetDetectorResponse
getDetector = AWS.request serviceName "getDetector" 


-- | Describes Amazon GuardDuty findings specified by finding IDs.
getFindings :: forall eff. GetFindingsRequest -> Aff (err :: AWS.RequestError | eff) GetFindingsResponse
getFindings = AWS.request serviceName "getFindings" 


-- | Lists Amazon GuardDuty findings' statistics for the specified detector ID.
getFindingsStatistics :: forall eff. GetFindingsStatisticsRequest -> Aff (err :: AWS.RequestError | eff) GetFindingsStatisticsResponse
getFindingsStatistics = AWS.request serviceName "getFindingsStatistics" 


-- | Retrieves the IPSet specified by the IPSet ID.
getIPSet :: forall eff. GetIPSetRequest -> Aff (err :: AWS.RequestError | eff) GetIPSetResponse
getIPSet = AWS.request serviceName "getIPSet" 


-- | Returns the count of all GuardDuty membership invitations that were sent to the current member account except the currently accepted invitation.
getInvitationsCount :: forall eff. GetInvitationsCountRequest -> Aff (err :: AWS.RequestError | eff) GetInvitationsCountResponse
getInvitationsCount = AWS.request serviceName "getInvitationsCount" 


-- | Provides the details for the GuardDuty master account to the current GuardDuty member account.
getMasterAccount :: forall eff. GetMasterAccountRequest -> Aff (err :: AWS.RequestError | eff) GetMasterAccountResponse
getMasterAccount = AWS.request serviceName "getMasterAccount" 


-- | Retrieves GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
getMembers :: forall eff. GetMembersRequest -> Aff (err :: AWS.RequestError | eff) GetMembersResponse
getMembers = AWS.request serviceName "getMembers" 


-- | Retrieves the ThreatIntelSet that is specified by the ThreatIntelSet ID.
getThreatIntelSet :: forall eff. GetThreatIntelSetRequest -> Aff (err :: AWS.RequestError | eff) GetThreatIntelSetResponse
getThreatIntelSet = AWS.request serviceName "getThreatIntelSet" 


-- | Invites other AWS accounts (created as members of the current AWS account by CreateMembers) to enable GuardDuty and allow the current AWS account to view and manage these accounts' GuardDuty findings on their behalf as the master account.
inviteMembers :: forall eff. InviteMembersRequest -> Aff (err :: AWS.RequestError | eff) InviteMembersResponse
inviteMembers = AWS.request serviceName "inviteMembers" 


-- | Lists detectorIds of all the existing Amazon GuardDuty detector resources.
listDetectors :: forall eff. ListDetectorsRequest -> Aff (err :: AWS.RequestError | eff) ListDetectorsResponse
listDetectors = AWS.request serviceName "listDetectors" 


-- | Lists Amazon GuardDuty findings for the specified detector ID.
listFindings :: forall eff. ListFindingsRequest -> Aff (err :: AWS.RequestError | eff) ListFindingsResponse
listFindings = AWS.request serviceName "listFindings" 


-- | Lists the IPSets of the GuardDuty service specified by the detector ID.
listIPSets :: forall eff. ListIPSetsRequest -> Aff (err :: AWS.RequestError | eff) ListIPSetsResponse
listIPSets = AWS.request serviceName "listIPSets" 


-- | Lists all GuardDuty membership invitations that were sent to the current AWS account.
listInvitations :: forall eff. ListInvitationsRequest -> Aff (err :: AWS.RequestError | eff) ListInvitationsResponse
listInvitations = AWS.request serviceName "listInvitations" 


-- | Lists details about all member accounts for the current GuardDuty master account.
listMembers :: forall eff. ListMembersRequest -> Aff (err :: AWS.RequestError | eff) ListMembersResponse
listMembers = AWS.request serviceName "listMembers" 


-- | Lists the ThreatIntelSets of the GuardDuty service specified by the detector ID.
listThreatIntelSets :: forall eff. ListThreatIntelSetsRequest -> Aff (err :: AWS.RequestError | eff) ListThreatIntelSetsResponse
listThreatIntelSets = AWS.request serviceName "listThreatIntelSets" 


-- | Re-enables GuardDuty to monitor findings of the member accounts specified by the account IDs. A master GuardDuty account can run this command after disabling GuardDuty from monitoring these members' findings by running StopMonitoringMembers.
startMonitoringMembers :: forall eff. StartMonitoringMembersRequest -> Aff (err :: AWS.RequestError | eff) StartMonitoringMembersResponse
startMonitoringMembers = AWS.request serviceName "startMonitoringMembers" 


-- | Disables GuardDuty from monitoring findings of the member accounts specified by the account IDs. After running this command, a master GuardDuty account can run StartMonitoringMembers to re-enable GuardDuty to monitor these members' findings.
stopMonitoringMembers :: forall eff. StopMonitoringMembersRequest -> Aff (err :: AWS.RequestError | eff) StopMonitoringMembersResponse
stopMonitoringMembers = AWS.request serviceName "stopMonitoringMembers" 


-- | Unarchives Amazon GuardDuty findings specified by the list of finding IDs.
unarchiveFindings :: forall eff. UnarchiveFindingsRequest -> Aff (err :: AWS.RequestError | eff) UnarchiveFindingsResponse
unarchiveFindings = AWS.request serviceName "unarchiveFindings" 


-- | Updates an Amazon GuardDuty detector specified by the detectorId.
updateDetector :: forall eff. UpdateDetectorRequest -> Aff (err :: AWS.RequestError | eff) UpdateDetectorResponse
updateDetector = AWS.request serviceName "updateDetector" 


-- | Marks specified Amazon GuardDuty findings as useful or not useful.
updateFindingsFeedback :: forall eff. UpdateFindingsFeedbackRequest -> Aff (err :: AWS.RequestError | eff) UpdateFindingsFeedbackResponse
updateFindingsFeedback = AWS.request serviceName "updateFindingsFeedback" 


-- | Updates the IPSet specified by the IPSet ID.
updateIPSet :: forall eff. UpdateIPSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateIPSetResponse
updateIPSet = AWS.request serviceName "updateIPSet" 


-- | Updates the ThreatIntelSet specified by ThreatIntelSet ID.
updateThreatIntelSet :: forall eff. UpdateThreatIntelSetRequest -> Aff (err :: AWS.RequestError | eff) UpdateThreatIntelSetResponse
updateThreatIntelSet = AWS.request serviceName "updateThreatIntelSet" 


-- | AcceptInvitation request body.
newtype AcceptInvitationRequest = AcceptInvitationRequest 
  { "DetectorId" :: (String)
  , "InvitationId" :: NullOrUndefined (InvitationId)
  , "MasterId" :: NullOrUndefined (MasterId)
  }
derive instance newtypeAcceptInvitationRequest :: Newtype AcceptInvitationRequest _


newtype AcceptInvitationResponse = AcceptInvitationResponse 
  { 
  }
derive instance newtypeAcceptInvitationResponse :: Newtype AcceptInvitationResponse _


-- | The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
newtype AccessKeyDetails = AccessKeyDetails 
  { "AccessKeyId" :: NullOrUndefined (String)
  , "PrincipalId" :: NullOrUndefined (String)
  , "UserName" :: NullOrUndefined (String)
  , "UserType" :: NullOrUndefined (String)
  }
derive instance newtypeAccessKeyDetails :: Newtype AccessKeyDetails _


-- | An object containing the member's accountId and email address.
newtype AccountDetail = AccountDetail 
  { "AccountId" :: NullOrUndefined (AccountId)
  , "Email" :: NullOrUndefined (Email)
  }
derive instance newtypeAccountDetail :: Newtype AccountDetail _


-- | A list of account/email pairs.
newtype AccountDetails = AccountDetails (Array AccountDetail)
derive instance newtypeAccountDetails :: Newtype AccountDetails _


-- | AWS account ID.
newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _


-- | A list of account IDs.
newtype AccountIds = AccountIds (Array String)
derive instance newtypeAccountIds :: Newtype AccountIds _


-- | Information about the activity described in a finding.
newtype Action = Action 
  { "ActionType" :: NullOrUndefined (String)
  , "AwsApiCallAction" :: NullOrUndefined (AwsApiCallAction)
  , "DnsRequestAction" :: NullOrUndefined (DnsRequestAction)
  , "NetworkConnectionAction" :: NullOrUndefined (NetworkConnectionAction)
  , "PortProbeAction" :: NullOrUndefined (PortProbeAction)
  }
derive instance newtypeAction :: Newtype Action _


-- | Whether we should start processing the list immediately or not.
newtype Activate = Activate Boolean
derive instance newtypeActivate :: Newtype Activate _


-- | ArchiveFindings request body.
newtype ArchiveFindingsRequest = ArchiveFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingIds" :: NullOrUndefined (FindingIds)
  }
derive instance newtypeArchiveFindingsRequest :: Newtype ArchiveFindingsRequest _


newtype ArchiveFindingsResponse = ArchiveFindingsResponse 
  { 
  }
derive instance newtypeArchiveFindingsResponse :: Newtype ArchiveFindingsResponse _


-- | Information about the AWS_API_CALL action described in this finding.
newtype AwsApiCallAction = AwsApiCallAction 
  { "Api" :: NullOrUndefined (String)
  , "CallerType" :: NullOrUndefined (String)
  , "DomainDetails" :: NullOrUndefined (DomainDetails)
  , "RemoteIpDetails" :: NullOrUndefined (RemoteIpDetails)
  , "ServiceName" :: NullOrUndefined (String)
  }
derive instance newtypeAwsApiCallAction :: Newtype AwsApiCallAction _


-- | Error response object.
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


-- | City information of the remote IP address.
newtype City = City 
  { "CityName" :: NullOrUndefined (String)
  }
derive instance newtypeCity :: Newtype City _


-- | Additional feedback about the GuardDuty findings.
newtype Comments = Comments String
derive instance newtypeComments :: Newtype Comments _


-- | Finding attribute (for example, accountId) for which conditions and values must be specified when querying findings.
newtype Condition = Condition 
  { "Eq" :: NullOrUndefined (Eq)
  , "Gt" :: NullOrUndefined (Int)
  , "Gte" :: NullOrUndefined (Int)
  , "Lt" :: NullOrUndefined (Int)
  , "Lte" :: NullOrUndefined (Int)
  , "Neq" :: NullOrUndefined (Neq)
  }
derive instance newtypeCondition :: Newtype Condition _


-- | The count of findings for the given severity.
newtype CountBySeverityFindingStatistic = CountBySeverityFindingStatistic Int
derive instance newtypeCountBySeverityFindingStatistic :: Newtype CountBySeverityFindingStatistic _


-- | Country information of the remote IP address.
newtype Country = Country 
  { "CountryCode" :: NullOrUndefined (String)
  , "CountryName" :: NullOrUndefined (String)
  }
derive instance newtypeCountry :: Newtype Country _


-- | CreateDetector request body.
newtype CreateDetectorRequest = CreateDetectorRequest 
  { "Enable" :: NullOrUndefined (Enable)
  }
derive instance newtypeCreateDetectorRequest :: Newtype CreateDetectorRequest _


newtype CreateDetectorResponse = CreateDetectorResponse 
  { "DetectorId" :: NullOrUndefined (DetectorId)
  }
derive instance newtypeCreateDetectorResponse :: Newtype CreateDetectorResponse _


-- | CreateIPSet request body.
newtype CreateIPSetRequest = CreateIPSetRequest 
  { "Activate" :: NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "Format" :: NullOrUndefined (IpSetFormat)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  }
derive instance newtypeCreateIPSetRequest :: Newtype CreateIPSetRequest _


newtype CreateIPSetResponse = CreateIPSetResponse 
  { "IpSetId" :: NullOrUndefined (IpSetId)
  }
derive instance newtypeCreateIPSetResponse :: Newtype CreateIPSetResponse _


-- | CreateMembers request body.
newtype CreateMembersRequest = CreateMembersRequest 
  { "AccountDetails" :: NullOrUndefined (AccountDetails)
  , "DetectorId" :: (String)
  }
derive instance newtypeCreateMembersRequest :: Newtype CreateMembersRequest _


newtype CreateMembersResponse = CreateMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeCreateMembersResponse :: Newtype CreateMembersResponse _


-- | CreateSampleFindings request body.
newtype CreateSampleFindingsRequest = CreateSampleFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingTypes" :: NullOrUndefined (FindingTypes)
  }
derive instance newtypeCreateSampleFindingsRequest :: Newtype CreateSampleFindingsRequest _


newtype CreateSampleFindingsResponse = CreateSampleFindingsResponse 
  { 
  }
derive instance newtypeCreateSampleFindingsResponse :: Newtype CreateSampleFindingsResponse _


-- | CreateThreatIntelSet request body.
newtype CreateThreatIntelSetRequest = CreateThreatIntelSetRequest 
  { "Activate" :: NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "Format" :: NullOrUndefined (ThreatIntelSetFormat)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  }
derive instance newtypeCreateThreatIntelSetRequest :: Newtype CreateThreatIntelSetRequest _


newtype CreateThreatIntelSetResponse = CreateThreatIntelSetResponse 
  { "ThreatIntelSetId" :: NullOrUndefined (ThreatIntelSetId)
  }
derive instance newtypeCreateThreatIntelSetResponse :: Newtype CreateThreatIntelSetResponse _


-- | The first time a resource was created. The format will be ISO-8601.
newtype CreatedAt = CreatedAt String
derive instance newtypeCreatedAt :: Newtype CreatedAt _


-- | DeclineInvitations request body.
newtype DeclineInvitationsRequest = DeclineInvitationsRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  }
derive instance newtypeDeclineInvitationsRequest :: Newtype DeclineInvitationsRequest _


newtype DeclineInvitationsResponse = DeclineInvitationsResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeDeclineInvitationsResponse :: Newtype DeclineInvitationsResponse _


newtype DeleteDetectorRequest = DeleteDetectorRequest 
  { "DetectorId" :: (String)
  }
derive instance newtypeDeleteDetectorRequest :: Newtype DeleteDetectorRequest _


newtype DeleteDetectorResponse = DeleteDetectorResponse 
  { 
  }
derive instance newtypeDeleteDetectorResponse :: Newtype DeleteDetectorResponse _


newtype DeleteIPSetRequest = DeleteIPSetRequest 
  { "DetectorId" :: (String)
  , "IpSetId" :: (String)
  }
derive instance newtypeDeleteIPSetRequest :: Newtype DeleteIPSetRequest _


newtype DeleteIPSetResponse = DeleteIPSetResponse 
  { 
  }
derive instance newtypeDeleteIPSetResponse :: Newtype DeleteIPSetResponse _


-- | DeleteInvitations request body.
newtype DeleteInvitationsRequest = DeleteInvitationsRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  }
derive instance newtypeDeleteInvitationsRequest :: Newtype DeleteInvitationsRequest _


newtype DeleteInvitationsResponse = DeleteInvitationsResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeDeleteInvitationsResponse :: Newtype DeleteInvitationsResponse _


-- | DeleteMembers request body.
newtype DeleteMembersRequest = DeleteMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeDeleteMembersRequest :: Newtype DeleteMembersRequest _


newtype DeleteMembersResponse = DeleteMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeDeleteMembersResponse :: Newtype DeleteMembersResponse _


newtype DeleteThreatIntelSetRequest = DeleteThreatIntelSetRequest 
  { "DetectorId" :: (String)
  , "ThreatIntelSetId" :: (String)
  }
derive instance newtypeDeleteThreatIntelSetRequest :: Newtype DeleteThreatIntelSetRequest _


newtype DeleteThreatIntelSetResponse = DeleteThreatIntelSetResponse 
  { 
  }
derive instance newtypeDeleteThreatIntelSetResponse :: Newtype DeleteThreatIntelSetResponse _


-- | The unique identifier for a detector.
newtype DetectorId = DetectorId String
derive instance newtypeDetectorId :: Newtype DetectorId _


-- | A list of detector Ids.
newtype DetectorIds = DetectorIds (Array DetectorId)
derive instance newtypeDetectorIds :: Newtype DetectorIds _


-- | The status of detector.
newtype DetectorStatus = DetectorStatus String
derive instance newtypeDetectorStatus :: Newtype DetectorStatus _


newtype DisassociateFromMasterAccountRequest = DisassociateFromMasterAccountRequest 
  { "DetectorId" :: (String)
  }
derive instance newtypeDisassociateFromMasterAccountRequest :: Newtype DisassociateFromMasterAccountRequest _


newtype DisassociateFromMasterAccountResponse = DisassociateFromMasterAccountResponse 
  { 
  }
derive instance newtypeDisassociateFromMasterAccountResponse :: Newtype DisassociateFromMasterAccountResponse _


-- | DisassociateMembers request body.
newtype DisassociateMembersRequest = DisassociateMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeDisassociateMembersRequest :: Newtype DisassociateMembersRequest _


newtype DisassociateMembersResponse = DisassociateMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeDisassociateMembersResponse :: Newtype DisassociateMembersResponse _


-- | Information about the DNS_REQUEST action described in this finding.
newtype DnsRequestAction = DnsRequestAction 
  { "Domain" :: NullOrUndefined (Domain)
  }
derive instance newtypeDnsRequestAction :: Newtype DnsRequestAction _


-- | A domain name.
newtype Domain = Domain String
derive instance newtypeDomain :: Newtype Domain _


-- | Domain information for the AWS API call.
newtype DomainDetails = DomainDetails 
  { 
  }
derive instance newtypeDomainDetails :: Newtype DomainDetails _


-- | Member account's email address.
newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _


-- | A boolean value that specifies whether the detector is to be enabled.
newtype Enable = Enable Boolean
derive instance newtypeEnable :: Newtype Enable _


-- | Represents the equal condition to be applied to a single field when querying for findings.
newtype Eq = Eq (Array String)
derive instance newtypeEq :: Newtype Eq _


-- | Error response object.
newtype ErrorResponse = ErrorResponse 
  { "Message" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  }
derive instance newtypeErrorResponse :: Newtype ErrorResponse _


-- | Finding Feedback Value
newtype Feedback = Feedback String
derive instance newtypeFeedback :: Newtype Feedback _


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
derive instance newtypeFinding :: Newtype Finding _


-- | Represents the criteria used for querying findings.
newtype FindingCriteria = FindingCriteria 
  { "Criterion" :: NullOrUndefined (MapOfCondition)
  }
derive instance newtypeFindingCriteria :: Newtype FindingCriteria _


-- | The unique identifier for the Finding
newtype FindingId = FindingId String
derive instance newtypeFindingId :: Newtype FindingId _


-- | The list of the Findings.
newtype FindingIds = FindingIds (Array FindingId)
derive instance newtypeFindingIds :: Newtype FindingIds _


-- | The types of finding statistics.
newtype FindingStatisticType = FindingStatisticType String
derive instance newtypeFindingStatisticType :: Newtype FindingStatisticType _


-- | The list of the finding statistics.
newtype FindingStatisticTypes = FindingStatisticTypes (Array FindingStatisticType)
derive instance newtypeFindingStatisticTypes :: Newtype FindingStatisticTypes _


-- | Finding statistics object.
newtype FindingStatistics = FindingStatistics 
  { "CountBySeverity" :: NullOrUndefined (MapOfCountBySeverityFindingStatistic)
  }
derive instance newtypeFindingStatistics :: Newtype FindingStatistics _


-- | The finding type for the finding
newtype FindingType = FindingType String
derive instance newtypeFindingType :: Newtype FindingType _


-- | The list of the finding types.
newtype FindingTypes = FindingTypes (Array FindingType)
derive instance newtypeFindingTypes :: Newtype FindingTypes _


-- | A list of findings.
newtype Findings = Findings (Array Finding)
derive instance newtypeFindings :: Newtype Findings _


-- | Location information of the remote IP address.
newtype GeoLocation = GeoLocation 
  { "Lat" :: NullOrUndefined (Number)
  , "Lon" :: NullOrUndefined (Number)
  }
derive instance newtypeGeoLocation :: Newtype GeoLocation _


newtype GetDetectorRequest = GetDetectorRequest 
  { "DetectorId" :: (String)
  }
derive instance newtypeGetDetectorRequest :: Newtype GetDetectorRequest _


newtype GetDetectorResponse = GetDetectorResponse 
  { "CreatedAt" :: NullOrUndefined (CreatedAt)
  , "ServiceRole" :: NullOrUndefined (ServiceRole)
  , "Status" :: NullOrUndefined (DetectorStatus)
  , "UpdatedAt" :: NullOrUndefined (UpdatedAt)
  }
derive instance newtypeGetDetectorResponse :: Newtype GetDetectorResponse _


-- | GetFindings request body.
newtype GetFindingsRequest = GetFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingIds" :: NullOrUndefined (FindingIds)
  , "SortCriteria" :: NullOrUndefined (SortCriteria)
  }
derive instance newtypeGetFindingsRequest :: Newtype GetFindingsRequest _


newtype GetFindingsResponse = GetFindingsResponse 
  { "Findings" :: NullOrUndefined (Findings)
  }
derive instance newtypeGetFindingsResponse :: Newtype GetFindingsResponse _


-- | GetFindingsStatistics request body.
newtype GetFindingsStatisticsRequest = GetFindingsStatisticsRequest 
  { "DetectorId" :: (String)
  , "FindingCriteria" :: NullOrUndefined (FindingCriteria)
  , "FindingStatisticTypes" :: NullOrUndefined (FindingStatisticTypes)
  }
derive instance newtypeGetFindingsStatisticsRequest :: Newtype GetFindingsStatisticsRequest _


newtype GetFindingsStatisticsResponse = GetFindingsStatisticsResponse 
  { "FindingStatistics" :: NullOrUndefined (FindingStatistics)
  }
derive instance newtypeGetFindingsStatisticsResponse :: Newtype GetFindingsStatisticsResponse _


newtype GetIPSetRequest = GetIPSetRequest 
  { "DetectorId" :: (String)
  , "IpSetId" :: (String)
  }
derive instance newtypeGetIPSetRequest :: Newtype GetIPSetRequest _


newtype GetIPSetResponse = GetIPSetResponse 
  { "Format" :: NullOrUndefined (IpSetFormat)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  , "Status" :: NullOrUndefined (IpSetStatus)
  }
derive instance newtypeGetIPSetResponse :: Newtype GetIPSetResponse _


newtype GetInvitationsCountRequest = GetInvitationsCountRequest 
  { 
  }
derive instance newtypeGetInvitationsCountRequest :: Newtype GetInvitationsCountRequest _


newtype GetInvitationsCountResponse = GetInvitationsCountResponse 
  { "InvitationsCount" :: NullOrUndefined (Int)
  }
derive instance newtypeGetInvitationsCountResponse :: Newtype GetInvitationsCountResponse _


newtype GetMasterAccountRequest = GetMasterAccountRequest 
  { "DetectorId" :: (String)
  }
derive instance newtypeGetMasterAccountRequest :: Newtype GetMasterAccountRequest _


newtype GetMasterAccountResponse = GetMasterAccountResponse 
  { "Master" :: NullOrUndefined (Master)
  }
derive instance newtypeGetMasterAccountResponse :: Newtype GetMasterAccountResponse _


-- | GetMembers request body.
newtype GetMembersRequest = GetMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeGetMembersRequest :: Newtype GetMembersRequest _


newtype GetMembersResponse = GetMembersResponse 
  { "Members" :: NullOrUndefined (Members)
  , "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeGetMembersResponse :: Newtype GetMembersResponse _


newtype GetThreatIntelSetRequest = GetThreatIntelSetRequest 
  { "DetectorId" :: (String)
  , "ThreatIntelSetId" :: (String)
  }
derive instance newtypeGetThreatIntelSetRequest :: Newtype GetThreatIntelSetRequest _


newtype GetThreatIntelSetResponse = GetThreatIntelSetResponse 
  { "Format" :: NullOrUndefined (ThreatIntelSetFormat)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  , "Status" :: NullOrUndefined (ThreatIntelSetStatus)
  }
derive instance newtypeGetThreatIntelSetResponse :: Newtype GetThreatIntelSetResponse _


-- | The profile information of the EC2 instance.
newtype IamInstanceProfile = IamInstanceProfile 
  { "Arn" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  }
derive instance newtypeIamInstanceProfile :: Newtype IamInstanceProfile _


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
derive instance newtypeInstanceDetails :: Newtype InstanceDetails _


-- | Error response object.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _


-- | Invitation from an AWS account to become the current account's master.
newtype Invitation = Invitation 
  { "AccountId" :: NullOrUndefined (String)
  , "InvitationId" :: NullOrUndefined (InvitationId)
  , "InvitedAt" :: NullOrUndefined (InvitedAt)
  , "RelationshipStatus" :: NullOrUndefined (String)
  }
derive instance newtypeInvitation :: Newtype Invitation _


-- | This value is used to validate the master account to the member account.
newtype InvitationId = InvitationId String
derive instance newtypeInvitationId :: Newtype InvitationId _


-- | A list of invitation descriptions.
newtype Invitations = Invitations (Array Invitation)
derive instance newtypeInvitations :: Newtype Invitations _


-- | InviteMembers request body.
newtype InviteMembersRequest = InviteMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  , "Message" :: NullOrUndefined (Message)
  }
derive instance newtypeInviteMembersRequest :: Newtype InviteMembersRequest _


newtype InviteMembersResponse = InviteMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeInviteMembersResponse :: Newtype InviteMembersResponse _


-- | Timestamp at which a member has been invited. The format will be ISO-8601.
newtype InvitedAt = InvitedAt String
derive instance newtypeInvitedAt :: Newtype InvitedAt _


-- | The format of the ipSet.
newtype IpSetFormat = IpSetFormat String
derive instance newtypeIpSetFormat :: Newtype IpSetFormat _


-- | The unique identifier for an IP Set
newtype IpSetId = IpSetId String
derive instance newtypeIpSetId :: Newtype IpSetId _


-- | A list of the IP set IDs
newtype IpSetIds = IpSetIds (Array IpSetId)
derive instance newtypeIpSetIds :: Newtype IpSetIds _


-- | The status of ipSet file uploaded.
newtype IpSetStatus = IpSetStatus String
derive instance newtypeIpSetStatus :: Newtype IpSetStatus _


-- | IpV6 address of the EC2 instance.
newtype Ipv6Address = Ipv6Address String
derive instance newtypeIpv6Address :: Newtype Ipv6Address _


-- | A list of EC2 instance IPv6 address information.
newtype Ipv6Addresses = Ipv6Addresses (Array Ipv6Address)
derive instance newtypeIpv6Addresses :: Newtype Ipv6Addresses _


newtype ListDetectorsRequest = ListDetectorsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListDetectorsRequest :: Newtype ListDetectorsRequest _


newtype ListDetectorsResponse = ListDetectorsResponse 
  { "DetectorIds" :: NullOrUndefined (DetectorIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDetectorsResponse :: Newtype ListDetectorsResponse _


-- | ListFindings request body.
newtype ListFindingsRequest = ListFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingCriteria" :: NullOrUndefined (FindingCriteria)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "SortCriteria" :: NullOrUndefined (SortCriteria)
  }
derive instance newtypeListFindingsRequest :: Newtype ListFindingsRequest _


newtype ListFindingsResponse = ListFindingsResponse 
  { "FindingIds" :: NullOrUndefined (FindingIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListFindingsResponse :: Newtype ListFindingsResponse _


newtype ListIPSetsRequest = ListIPSetsRequest 
  { "DetectorId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListIPSetsRequest :: Newtype ListIPSetsRequest _


newtype ListIPSetsResponse = ListIPSetsResponse 
  { "IpSetIds" :: NullOrUndefined (IpSetIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListIPSetsResponse :: Newtype ListIPSetsResponse _


newtype ListInvitationsRequest = ListInvitationsRequest 
  { "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListInvitationsRequest :: Newtype ListInvitationsRequest _


newtype ListInvitationsResponse = ListInvitationsResponse 
  { "Invitations" :: NullOrUndefined (Invitations)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListInvitationsResponse :: Newtype ListInvitationsResponse _


newtype ListMembersRequest = ListMembersRequest 
  { "DetectorId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  , "OnlyAssociated" :: NullOrUndefined (String)
  }
derive instance newtypeListMembersRequest :: Newtype ListMembersRequest _


newtype ListMembersResponse = ListMembersResponse 
  { "Members" :: NullOrUndefined (Members)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListMembersResponse :: Newtype ListMembersResponse _


newtype ListOfPortProbeDetail = ListOfPortProbeDetail (Array PortProbeDetail)
derive instance newtypeListOfPortProbeDetail :: Newtype ListOfPortProbeDetail _


newtype ListThreatIntelSetsRequest = ListThreatIntelSetsRequest 
  { "DetectorId" :: (String)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListThreatIntelSetsRequest :: Newtype ListThreatIntelSetsRequest _


newtype ListThreatIntelSetsResponse = ListThreatIntelSetsResponse 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "ThreatIntelSetIds" :: NullOrUndefined (ThreatIntelSetIds)
  }
derive instance newtypeListThreatIntelSetsResponse :: Newtype ListThreatIntelSetsResponse _


-- | Local port information of the connection.
newtype LocalPortDetails = LocalPortDetails 
  { "Port" :: NullOrUndefined (Int)
  , "PortName" :: NullOrUndefined (String)
  }
derive instance newtypeLocalPortDetails :: Newtype LocalPortDetails _


-- | The location of the S3 bucket where the list resides. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)
newtype Location = Location String
derive instance newtypeLocation :: Newtype Location _


newtype MapOfCondition = MapOfCondition (Map String Condition)
derive instance newtypeMapOfCondition :: Newtype MapOfCondition _


newtype MapOfCountBySeverityFindingStatistic = MapOfCountBySeverityFindingStatistic (Map String CountBySeverityFindingStatistic)
derive instance newtypeMapOfCountBySeverityFindingStatistic :: Newtype MapOfCountBySeverityFindingStatistic _


-- | Contains details about the master account.
newtype Master = Master 
  { "AccountId" :: NullOrUndefined (String)
  , "InvitationId" :: NullOrUndefined (InvitationId)
  , "InvitedAt" :: NullOrUndefined (InvitedAt)
  , "RelationshipStatus" :: NullOrUndefined (String)
  }
derive instance newtypeMaster :: Newtype Master _


-- | The master account ID.
newtype MasterId = MasterId String
derive instance newtypeMasterId :: Newtype MasterId _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


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
derive instance newtypeMember :: Newtype Member _


-- | A list of member descriptions.
newtype Members = Members (Array Member)
derive instance newtypeMembers :: Newtype Members _


-- | The invitation message that you want to send to the accounts that you're inviting to GuardDuty as members.
newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


-- | The user-friendly name to identify the list.
newtype Name = Name String
derive instance newtypeName :: Newtype Name _


-- | Represents the not equal condition to be applied to a single field when querying for findings.
newtype Neq = Neq (Array String)
derive instance newtypeNeq :: Newtype Neq _


-- | Information about the NETWORK_CONNECTION action described in this finding.
newtype NetworkConnectionAction = NetworkConnectionAction 
  { "Blocked" :: NullOrUndefined (Boolean)
  , "ConnectionDirection" :: NullOrUndefined (String)
  , "LocalPortDetails" :: NullOrUndefined (LocalPortDetails)
  , "Protocol" :: NullOrUndefined (String)
  , "RemoteIpDetails" :: NullOrUndefined (RemoteIpDetails)
  , "RemotePortDetails" :: NullOrUndefined (RemotePortDetails)
  }
derive instance newtypeNetworkConnectionAction :: Newtype NetworkConnectionAction _


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
derive instance newtypeNetworkInterface :: Newtype NetworkInterface _


-- | The network interface information of the EC2 instance.
newtype NetworkInterfaces = NetworkInterfaces (Array NetworkInterface)
derive instance newtypeNetworkInterfaces :: Newtype NetworkInterfaces _


-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype OrderBy = OrderBy String
derive instance newtypeOrderBy :: Newtype OrderBy _


-- | ISP Organization information of the remote IP address.
newtype Organization = Organization 
  { "Asn" :: NullOrUndefined (String)
  , "AsnOrg" :: NullOrUndefined (String)
  , "Isp" :: NullOrUndefined (String)
  , "Org" :: NullOrUndefined (String)
  }
derive instance newtypeOrganization :: Newtype Organization _


-- | Information about the PORT_PROBE action described in this finding.
newtype PortProbeAction = PortProbeAction 
  { "Blocked" :: NullOrUndefined (Boolean)
  , "PortProbeDetails" :: NullOrUndefined (ListOfPortProbeDetail)
  }
derive instance newtypePortProbeAction :: Newtype PortProbeAction _


-- | Details about the port probe finding.
newtype PortProbeDetail = PortProbeDetail 
  { "LocalPortDetails" :: NullOrUndefined (LocalPortDetails)
  , "RemoteIpDetails" :: NullOrUndefined (RemoteIpDetails)
  }
derive instance newtypePortProbeDetail :: Newtype PortProbeDetail _


-- | Private DNS name of the EC2 instance.
newtype PrivateDnsName = PrivateDnsName String
derive instance newtypePrivateDnsName :: Newtype PrivateDnsName _


-- | Private IP address of the EC2 instance.
newtype PrivateIpAddress = PrivateIpAddress String
derive instance newtypePrivateIpAddress :: Newtype PrivateIpAddress _


-- | Other private IP address information of the EC2 instance.
newtype PrivateIpAddressDetails = PrivateIpAddressDetails 
  { "PrivateDnsName" :: NullOrUndefined (PrivateDnsName)
  , "PrivateIpAddress" :: NullOrUndefined (PrivateIpAddress)
  }
derive instance newtypePrivateIpAddressDetails :: Newtype PrivateIpAddressDetails _


-- | Other private IP address information of the EC2 instance.
newtype PrivateIpAddresses = PrivateIpAddresses (Array PrivateIpAddressDetails)
derive instance newtypePrivateIpAddresses :: Newtype PrivateIpAddresses _


-- | The product code of the EC2 instance.
newtype ProductCode = ProductCode 
  { "Code" :: NullOrUndefined (String)
  , "ProductType" :: NullOrUndefined (String)
  }
derive instance newtypeProductCode :: Newtype ProductCode _


-- | The product code of the EC2 instance.
newtype ProductCodes = ProductCodes (Array ProductCode)
derive instance newtypeProductCodes :: Newtype ProductCodes _


-- | Remote IP information of the connection.
newtype RemoteIpDetails = RemoteIpDetails 
  { "City" :: NullOrUndefined (City)
  , "Country" :: NullOrUndefined (Country)
  , "GeoLocation" :: NullOrUndefined (GeoLocation)
  , "IpAddressV4" :: NullOrUndefined (String)
  , "Organization" :: NullOrUndefined (Organization)
  }
derive instance newtypeRemoteIpDetails :: Newtype RemoteIpDetails _


-- | Remote port information of the connection.
newtype RemotePortDetails = RemotePortDetails 
  { "Port" :: NullOrUndefined (Int)
  , "PortName" :: NullOrUndefined (String)
  }
derive instance newtypeRemotePortDetails :: Newtype RemotePortDetails _


-- | The AWS resource associated with the activity that prompted GuardDuty to generate a finding.
newtype Resource = Resource 
  { "AccessKeyDetails" :: NullOrUndefined (AccessKeyDetails)
  , "InstanceDetails" :: NullOrUndefined (InstanceDetails)
  , "ResourceType" :: NullOrUndefined (String)
  }
derive instance newtypeResource :: Newtype Resource _


-- | Security groups associated with the EC2 instance.
newtype SecurityGroup = SecurityGroup 
  { "GroupId" :: NullOrUndefined (String)
  , "GroupName" :: NullOrUndefined (String)
  }
derive instance newtypeSecurityGroup :: Newtype SecurityGroup _


-- | Security groups associated with the EC2 instance.
newtype SecurityGroups = SecurityGroups (Array SecurityGroup)
derive instance newtypeSecurityGroups :: Newtype SecurityGroups _


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
derive instance newtypeService :: Newtype Service _


-- | Customer serviceRole name or ARN for accessing customer resources
newtype ServiceRole = ServiceRole String
derive instance newtypeServiceRole :: Newtype ServiceRole _


-- | Represents the criteria used for sorting findings.
newtype SortCriteria = SortCriteria 
  { "AttributeName" :: NullOrUndefined (String)
  , "OrderBy" :: NullOrUndefined (OrderBy)
  }
derive instance newtypeSortCriteria :: Newtype SortCriteria _


-- | StartMonitoringMembers request body.
newtype StartMonitoringMembersRequest = StartMonitoringMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeStartMonitoringMembersRequest :: Newtype StartMonitoringMembersRequest _


newtype StartMonitoringMembersResponse = StartMonitoringMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeStartMonitoringMembersResponse :: Newtype StartMonitoringMembersResponse _


-- | StopMonitoringMembers request body.
newtype StopMonitoringMembersRequest = StopMonitoringMembersRequest 
  { "AccountIds" :: NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeStopMonitoringMembersRequest :: Newtype StopMonitoringMembersRequest _


newtype StopMonitoringMembersResponse = StopMonitoringMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeStopMonitoringMembersResponse :: Newtype StopMonitoringMembersResponse _


-- | A tag of the EC2 instance.
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _


-- | The tags of the EC2 instance.
newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _


-- | The format of the threatIntelSet.
newtype ThreatIntelSetFormat = ThreatIntelSetFormat String
derive instance newtypeThreatIntelSetFormat :: Newtype ThreatIntelSetFormat _


-- | The unique identifier for an threat intel set
newtype ThreatIntelSetId = ThreatIntelSetId String
derive instance newtypeThreatIntelSetId :: Newtype ThreatIntelSetId _


-- | The list of the threat intel set IDs
newtype ThreatIntelSetIds = ThreatIntelSetIds (Array ThreatIntelSetId)
derive instance newtypeThreatIntelSetIds :: Newtype ThreatIntelSetIds _


-- | The status of threatIntelSet file uploaded.
newtype ThreatIntelSetStatus = ThreatIntelSetStatus String
derive instance newtypeThreatIntelSetStatus :: Newtype ThreatIntelSetStatus _


-- | UnarchiveFindings request body.
newtype UnarchiveFindingsRequest = UnarchiveFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingIds" :: NullOrUndefined (FindingIds)
  }
derive instance newtypeUnarchiveFindingsRequest :: Newtype UnarchiveFindingsRequest _


newtype UnarchiveFindingsResponse = UnarchiveFindingsResponse 
  { 
  }
derive instance newtypeUnarchiveFindingsResponse :: Newtype UnarchiveFindingsResponse _


-- | An object containing the unprocessed account and a result string explaining why it was unprocessed.
newtype UnprocessedAccount = UnprocessedAccount 
  { "AccountId" :: NullOrUndefined (String)
  , "Result" :: NullOrUndefined (String)
  }
derive instance newtypeUnprocessedAccount :: Newtype UnprocessedAccount _


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
newtype UnprocessedAccounts = UnprocessedAccounts (Array UnprocessedAccount)
derive instance newtypeUnprocessedAccounts :: Newtype UnprocessedAccounts _


-- | UpdateDetector request body.
newtype UpdateDetectorRequest = UpdateDetectorRequest 
  { "DetectorId" :: (String)
  , "Enable" :: NullOrUndefined (Enable)
  }
derive instance newtypeUpdateDetectorRequest :: Newtype UpdateDetectorRequest _


newtype UpdateDetectorResponse = UpdateDetectorResponse 
  { 
  }
derive instance newtypeUpdateDetectorResponse :: Newtype UpdateDetectorResponse _


-- | UpdateFindingsFeedback request body.
newtype UpdateFindingsFeedbackRequest = UpdateFindingsFeedbackRequest 
  { "Comments" :: NullOrUndefined (Comments)
  , "DetectorId" :: (String)
  , "Feedback" :: NullOrUndefined (Feedback)
  , "FindingIds" :: NullOrUndefined (FindingIds)
  }
derive instance newtypeUpdateFindingsFeedbackRequest :: Newtype UpdateFindingsFeedbackRequest _


newtype UpdateFindingsFeedbackResponse = UpdateFindingsFeedbackResponse 
  { 
  }
derive instance newtypeUpdateFindingsFeedbackResponse :: Newtype UpdateFindingsFeedbackResponse _


-- | UpdateIPSet request body.
newtype UpdateIPSetRequest = UpdateIPSetRequest 
  { "Activate" :: NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "IpSetId" :: (String)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  }
derive instance newtypeUpdateIPSetRequest :: Newtype UpdateIPSetRequest _


newtype UpdateIPSetResponse = UpdateIPSetResponse 
  { 
  }
derive instance newtypeUpdateIPSetResponse :: Newtype UpdateIPSetResponse _


-- | UpdateThreatIntelSet request body.
newtype UpdateThreatIntelSetRequest = UpdateThreatIntelSetRequest 
  { "Activate" :: NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "Location" :: NullOrUndefined (Location)
  , "Name" :: NullOrUndefined (Name)
  , "ThreatIntelSetId" :: (String)
  }
derive instance newtypeUpdateThreatIntelSetRequest :: Newtype UpdateThreatIntelSetRequest _


newtype UpdateThreatIntelSetResponse = UpdateThreatIntelSetResponse 
  { 
  }
derive instance newtypeUpdateThreatIntelSetResponse :: Newtype UpdateThreatIntelSetResponse _


-- | The first time a resource was created. The format will be ISO-8601.
newtype UpdatedAt = UpdatedAt String
derive instance newtypeUpdatedAt :: Newtype UpdatedAt _
