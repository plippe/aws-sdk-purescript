

-- | Assess, monitor, manage, and remediate security issues across your AWS infrastructure, applications, and data.
module AWS.GuardDuty where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "GuardDuty" :: String


-- | Accepts the invitation to be monitored by a master GuardDuty account.
acceptInvitation :: forall eff. AcceptInvitationRequest -> Aff (exception :: EXCEPTION | eff) AcceptInvitationResponse
acceptInvitation = Request.request serviceName "acceptInvitation" 


-- | Archives Amazon GuardDuty findings specified by the list of finding IDs.
archiveFindings :: forall eff. ArchiveFindingsRequest -> Aff (exception :: EXCEPTION | eff) ArchiveFindingsResponse
archiveFindings = Request.request serviceName "archiveFindings" 


-- | Creates a single Amazon GuardDuty detector. A detector is an object that represents the GuardDuty service. A detector must be created in order for GuardDuty to become operational.
createDetector :: forall eff. CreateDetectorRequest -> Aff (exception :: EXCEPTION | eff) CreateDetectorResponse
createDetector = Request.request serviceName "createDetector" 


-- | Creates a new IPSet - a list of trusted IP addresses that have been whitelisted for secure communication with AWS infrastructure and applications.
createIPSet :: forall eff. CreateIPSetRequest -> Aff (exception :: EXCEPTION | eff) CreateIPSetResponse
createIPSet = Request.request serviceName "createIPSet" 


-- | Creates member accounts of the current AWS account by specifying a list of AWS account IDs. The current AWS account can then invite these members to manage GuardDuty in their accounts.
createMembers :: forall eff. CreateMembersRequest -> Aff (exception :: EXCEPTION | eff) CreateMembersResponse
createMembers = Request.request serviceName "createMembers" 


-- | Generates example findings of types specified by the list of finding types. If 'NULL' is specified for findingTypes, the API generates example findings of all supported finding types.
createSampleFindings :: forall eff. CreateSampleFindingsRequest -> Aff (exception :: EXCEPTION | eff) CreateSampleFindingsResponse
createSampleFindings = Request.request serviceName "createSampleFindings" 


-- | Create a new ThreatIntelSet. ThreatIntelSets consist of known malicious IP addresses. GuardDuty generates findings based on ThreatIntelSets.
createThreatIntelSet :: forall eff. CreateThreatIntelSetRequest -> Aff (exception :: EXCEPTION | eff) CreateThreatIntelSetResponse
createThreatIntelSet = Request.request serviceName "createThreatIntelSet" 


-- | Declines invitations sent to the current member account by AWS account specified by their account IDs.
declineInvitations :: forall eff. DeclineInvitationsRequest -> Aff (exception :: EXCEPTION | eff) DeclineInvitationsResponse
declineInvitations = Request.request serviceName "declineInvitations" 


-- | Deletes a Amazon GuardDuty detector specified by the detector ID.
deleteDetector :: forall eff. DeleteDetectorRequest -> Aff (exception :: EXCEPTION | eff) DeleteDetectorResponse
deleteDetector = Request.request serviceName "deleteDetector" 


-- | Deletes the IPSet specified by the IPSet ID.
deleteIPSet :: forall eff. DeleteIPSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteIPSetResponse
deleteIPSet = Request.request serviceName "deleteIPSet" 


-- | Deletes invitations sent to the current member account by AWS accounts specified by their account IDs.
deleteInvitations :: forall eff. DeleteInvitationsRequest -> Aff (exception :: EXCEPTION | eff) DeleteInvitationsResponse
deleteInvitations = Request.request serviceName "deleteInvitations" 


-- | Deletes GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
deleteMembers :: forall eff. DeleteMembersRequest -> Aff (exception :: EXCEPTION | eff) DeleteMembersResponse
deleteMembers = Request.request serviceName "deleteMembers" 


-- | Deletes ThreatIntelSet specified by the ThreatIntelSet ID.
deleteThreatIntelSet :: forall eff. DeleteThreatIntelSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteThreatIntelSetResponse
deleteThreatIntelSet = Request.request serviceName "deleteThreatIntelSet" 


-- | Disassociates the current GuardDuty member account from its master account.
disassociateFromMasterAccount :: forall eff. DisassociateFromMasterAccountRequest -> Aff (exception :: EXCEPTION | eff) DisassociateFromMasterAccountResponse
disassociateFromMasterAccount = Request.request serviceName "disassociateFromMasterAccount" 


-- | Disassociates GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
disassociateMembers :: forall eff. DisassociateMembersRequest -> Aff (exception :: EXCEPTION | eff) DisassociateMembersResponse
disassociateMembers = Request.request serviceName "disassociateMembers" 


-- | Retrieves an Amazon GuardDuty detector specified by the detectorId.
getDetector :: forall eff. GetDetectorRequest -> Aff (exception :: EXCEPTION | eff) GetDetectorResponse
getDetector = Request.request serviceName "getDetector" 


-- | Describes Amazon GuardDuty findings specified by finding IDs.
getFindings :: forall eff. GetFindingsRequest -> Aff (exception :: EXCEPTION | eff) GetFindingsResponse
getFindings = Request.request serviceName "getFindings" 


-- | Lists Amazon GuardDuty findings' statistics for the specified detector ID.
getFindingsStatistics :: forall eff. GetFindingsStatisticsRequest -> Aff (exception :: EXCEPTION | eff) GetFindingsStatisticsResponse
getFindingsStatistics = Request.request serviceName "getFindingsStatistics" 


-- | Retrieves the IPSet specified by the IPSet ID.
getIPSet :: forall eff. GetIPSetRequest -> Aff (exception :: EXCEPTION | eff) GetIPSetResponse
getIPSet = Request.request serviceName "getIPSet" 


-- | Returns the count of all GuardDuty membership invitations that were sent to the current member account except the currently accepted invitation.
getInvitationsCount :: forall eff. GetInvitationsCountRequest -> Aff (exception :: EXCEPTION | eff) GetInvitationsCountResponse
getInvitationsCount = Request.request serviceName "getInvitationsCount" 


-- | Provides the details for the GuardDuty master account to the current GuardDuty member account.
getMasterAccount :: forall eff. GetMasterAccountRequest -> Aff (exception :: EXCEPTION | eff) GetMasterAccountResponse
getMasterAccount = Request.request serviceName "getMasterAccount" 


-- | Retrieves GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
getMembers :: forall eff. GetMembersRequest -> Aff (exception :: EXCEPTION | eff) GetMembersResponse
getMembers = Request.request serviceName "getMembers" 


-- | Retrieves the ThreatIntelSet that is specified by the ThreatIntelSet ID.
getThreatIntelSet :: forall eff. GetThreatIntelSetRequest -> Aff (exception :: EXCEPTION | eff) GetThreatIntelSetResponse
getThreatIntelSet = Request.request serviceName "getThreatIntelSet" 


-- | Invites other AWS accounts (created as members of the current AWS account by CreateMembers) to enable GuardDuty and allow the current AWS account to view and manage these accounts' GuardDuty findings on their behalf as the master account.
inviteMembers :: forall eff. InviteMembersRequest -> Aff (exception :: EXCEPTION | eff) InviteMembersResponse
inviteMembers = Request.request serviceName "inviteMembers" 


-- | Lists detectorIds of all the existing Amazon GuardDuty detector resources.
listDetectors :: forall eff. ListDetectorsRequest -> Aff (exception :: EXCEPTION | eff) ListDetectorsResponse
listDetectors = Request.request serviceName "listDetectors" 


-- | Lists Amazon GuardDuty findings for the specified detector ID.
listFindings :: forall eff. ListFindingsRequest -> Aff (exception :: EXCEPTION | eff) ListFindingsResponse
listFindings = Request.request serviceName "listFindings" 


-- | Lists the IPSets of the GuardDuty service specified by the detector ID.
listIPSets :: forall eff. ListIPSetsRequest -> Aff (exception :: EXCEPTION | eff) ListIPSetsResponse
listIPSets = Request.request serviceName "listIPSets" 


-- | Lists all GuardDuty membership invitations that were sent to the current AWS account.
listInvitations :: forall eff. ListInvitationsRequest -> Aff (exception :: EXCEPTION | eff) ListInvitationsResponse
listInvitations = Request.request serviceName "listInvitations" 


-- | Lists details about all member accounts for the current GuardDuty master account.
listMembers :: forall eff. ListMembersRequest -> Aff (exception :: EXCEPTION | eff) ListMembersResponse
listMembers = Request.request serviceName "listMembers" 


-- | Lists the ThreatIntelSets of the GuardDuty service specified by the detector ID.
listThreatIntelSets :: forall eff. ListThreatIntelSetsRequest -> Aff (exception :: EXCEPTION | eff) ListThreatIntelSetsResponse
listThreatIntelSets = Request.request serviceName "listThreatIntelSets" 


-- | Re-enables GuardDuty to monitor findings of the member accounts specified by the account IDs. A master GuardDuty account can run this command after disabling GuardDuty from monitoring these members' findings by running StopMonitoringMembers.
startMonitoringMembers :: forall eff. StartMonitoringMembersRequest -> Aff (exception :: EXCEPTION | eff) StartMonitoringMembersResponse
startMonitoringMembers = Request.request serviceName "startMonitoringMembers" 


-- | Disables GuardDuty from monitoring findings of the member accounts specified by the account IDs. After running this command, a master GuardDuty account can run StartMonitoringMembers to re-enable GuardDuty to monitor these members' findings.
stopMonitoringMembers :: forall eff. StopMonitoringMembersRequest -> Aff (exception :: EXCEPTION | eff) StopMonitoringMembersResponse
stopMonitoringMembers = Request.request serviceName "stopMonitoringMembers" 


-- | Unarchives Amazon GuardDuty findings specified by the list of finding IDs.
unarchiveFindings :: forall eff. UnarchiveFindingsRequest -> Aff (exception :: EXCEPTION | eff) UnarchiveFindingsResponse
unarchiveFindings = Request.request serviceName "unarchiveFindings" 


-- | Updates an Amazon GuardDuty detector specified by the detectorId.
updateDetector :: forall eff. UpdateDetectorRequest -> Aff (exception :: EXCEPTION | eff) UpdateDetectorResponse
updateDetector = Request.request serviceName "updateDetector" 


-- | Marks specified Amazon GuardDuty findings as useful or not useful.
updateFindingsFeedback :: forall eff. UpdateFindingsFeedbackRequest -> Aff (exception :: EXCEPTION | eff) UpdateFindingsFeedbackResponse
updateFindingsFeedback = Request.request serviceName "updateFindingsFeedback" 


-- | Updates the IPSet specified by the IPSet ID.
updateIPSet :: forall eff. UpdateIPSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateIPSetResponse
updateIPSet = Request.request serviceName "updateIPSet" 


-- | Updates the ThreatIntelSet specified by ThreatIntelSet ID.
updateThreatIntelSet :: forall eff. UpdateThreatIntelSetRequest -> Aff (exception :: EXCEPTION | eff) UpdateThreatIntelSetResponse
updateThreatIntelSet = Request.request serviceName "updateThreatIntelSet" 


-- | AcceptInvitation request body.
newtype AcceptInvitationRequest = AcceptInvitationRequest 
  { "DetectorId" :: (String)
  , "InvitationId" :: NullOrUndefined.NullOrUndefined (InvitationId)
  , "MasterId" :: NullOrUndefined.NullOrUndefined (MasterId)
  }
derive instance newtypeAcceptInvitationRequest :: Newtype AcceptInvitationRequest _
derive instance repGenericAcceptInvitationRequest :: Generic AcceptInvitationRequest _
instance showAcceptInvitationRequest :: Show AcceptInvitationRequest where
  show = genericShow
instance decodeAcceptInvitationRequest :: Decode AcceptInvitationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcceptInvitationRequest :: Encode AcceptInvitationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AcceptInvitationResponse = AcceptInvitationResponse Types.NoArguments
derive instance newtypeAcceptInvitationResponse :: Newtype AcceptInvitationResponse _
derive instance repGenericAcceptInvitationResponse :: Generic AcceptInvitationResponse _
instance showAcceptInvitationResponse :: Show AcceptInvitationResponse where
  show = genericShow
instance decodeAcceptInvitationResponse :: Decode AcceptInvitationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcceptInvitationResponse :: Encode AcceptInvitationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The IAM access key details (IAM user information) of a user that engaged in the activity that prompted GuardDuty to generate a finding.
newtype AccessKeyDetails = AccessKeyDetails 
  { "AccessKeyId" :: NullOrUndefined.NullOrUndefined (String)
  , "PrincipalId" :: NullOrUndefined.NullOrUndefined (String)
  , "UserName" :: NullOrUndefined.NullOrUndefined (String)
  , "UserType" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAccessKeyDetails :: Newtype AccessKeyDetails _
derive instance repGenericAccessKeyDetails :: Generic AccessKeyDetails _
instance showAccessKeyDetails :: Show AccessKeyDetails where
  show = genericShow
instance decodeAccessKeyDetails :: Decode AccessKeyDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessKeyDetails :: Encode AccessKeyDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An object containing the member's accountId and email address.
newtype AccountDetail = AccountDetail 
  { "AccountId" :: NullOrUndefined.NullOrUndefined (AccountId)
  , "Email" :: NullOrUndefined.NullOrUndefined (Email)
  }
derive instance newtypeAccountDetail :: Newtype AccountDetail _
derive instance repGenericAccountDetail :: Generic AccountDetail _
instance showAccountDetail :: Show AccountDetail where
  show = genericShow
instance decodeAccountDetail :: Decode AccountDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountDetail :: Encode AccountDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of account/email pairs.
newtype AccountDetails = AccountDetails (Array AccountDetail)
derive instance newtypeAccountDetails :: Newtype AccountDetails _
derive instance repGenericAccountDetails :: Generic AccountDetails _
instance showAccountDetails :: Show AccountDetails where
  show = genericShow
instance decodeAccountDetails :: Decode AccountDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountDetails :: Encode AccountDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | AWS account ID.
newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _
derive instance repGenericAccountId :: Generic AccountId _
instance showAccountId :: Show AccountId where
  show = genericShow
instance decodeAccountId :: Decode AccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountId :: Encode AccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of account IDs.
newtype AccountIds = AccountIds (Array String)
derive instance newtypeAccountIds :: Newtype AccountIds _
derive instance repGenericAccountIds :: Generic AccountIds _
instance showAccountIds :: Show AccountIds where
  show = genericShow
instance decodeAccountIds :: Decode AccountIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountIds :: Encode AccountIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information about the activity described in a finding.
newtype Action = Action 
  { "ActionType" :: NullOrUndefined.NullOrUndefined (String)
  , "AwsApiCallAction" :: NullOrUndefined.NullOrUndefined (AwsApiCallAction)
  , "DnsRequestAction" :: NullOrUndefined.NullOrUndefined (DnsRequestAction)
  , "NetworkConnectionAction" :: NullOrUndefined.NullOrUndefined (NetworkConnectionAction)
  , "PortProbeAction" :: NullOrUndefined.NullOrUndefined (PortProbeAction)
  }
derive instance newtypeAction :: Newtype Action _
derive instance repGenericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
instance decodeAction :: Decode Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAction :: Encode Action where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Whether we should start processing the list immediately or not.
newtype Activate = Activate Boolean
derive instance newtypeActivate :: Newtype Activate _
derive instance repGenericActivate :: Generic Activate _
instance showActivate :: Show Activate where
  show = genericShow
instance decodeActivate :: Decode Activate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivate :: Encode Activate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | ArchiveFindings request body.
newtype ArchiveFindingsRequest = ArchiveFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingIds" :: NullOrUndefined.NullOrUndefined (FindingIds)
  }
derive instance newtypeArchiveFindingsRequest :: Newtype ArchiveFindingsRequest _
derive instance repGenericArchiveFindingsRequest :: Generic ArchiveFindingsRequest _
instance showArchiveFindingsRequest :: Show ArchiveFindingsRequest where
  show = genericShow
instance decodeArchiveFindingsRequest :: Decode ArchiveFindingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArchiveFindingsRequest :: Encode ArchiveFindingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArchiveFindingsResponse = ArchiveFindingsResponse Types.NoArguments
derive instance newtypeArchiveFindingsResponse :: Newtype ArchiveFindingsResponse _
derive instance repGenericArchiveFindingsResponse :: Generic ArchiveFindingsResponse _
instance showArchiveFindingsResponse :: Show ArchiveFindingsResponse where
  show = genericShow
instance decodeArchiveFindingsResponse :: Decode ArchiveFindingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArchiveFindingsResponse :: Encode ArchiveFindingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information about the AWS_API_CALL action described in this finding.
newtype AwsApiCallAction = AwsApiCallAction 
  { "Api" :: NullOrUndefined.NullOrUndefined (String)
  , "CallerType" :: NullOrUndefined.NullOrUndefined (String)
  , "DomainDetails" :: NullOrUndefined.NullOrUndefined (DomainDetails)
  , "RemoteIpDetails" :: NullOrUndefined.NullOrUndefined (RemoteIpDetails)
  , "ServiceName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAwsApiCallAction :: Newtype AwsApiCallAction _
derive instance repGenericAwsApiCallAction :: Generic AwsApiCallAction _
instance showAwsApiCallAction :: Show AwsApiCallAction where
  show = genericShow
instance decodeAwsApiCallAction :: Decode AwsApiCallAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsApiCallAction :: Encode AwsApiCallAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Error response object.
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | City information of the remote IP address.
newtype City = City 
  { "CityName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCity :: Newtype City _
derive instance repGenericCity :: Generic City _
instance showCity :: Show City where
  show = genericShow
instance decodeCity :: Decode City where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCity :: Encode City where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Additional feedback about the GuardDuty findings.
newtype Comments = Comments String
derive instance newtypeComments :: Newtype Comments _
derive instance repGenericComments :: Generic Comments _
instance showComments :: Show Comments where
  show = genericShow
instance decodeComments :: Decode Comments where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComments :: Encode Comments where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Finding attribute (for example, accountId) for which conditions and values must be specified when querying findings.
newtype Condition = Condition 
  { "Eq" :: NullOrUndefined.NullOrUndefined (Eq)
  , "Gt" :: NullOrUndefined.NullOrUndefined (Int)
  , "Gte" :: NullOrUndefined.NullOrUndefined (Int)
  , "Lt" :: NullOrUndefined.NullOrUndefined (Int)
  , "Lte" :: NullOrUndefined.NullOrUndefined (Int)
  , "Neq" :: NullOrUndefined.NullOrUndefined (Neq)
  }
derive instance newtypeCondition :: Newtype Condition _
derive instance repGenericCondition :: Generic Condition _
instance showCondition :: Show Condition where
  show = genericShow
instance decodeCondition :: Decode Condition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCondition :: Encode Condition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The count of findings for the given severity.
newtype CountBySeverityFindingStatistic = CountBySeverityFindingStatistic Int
derive instance newtypeCountBySeverityFindingStatistic :: Newtype CountBySeverityFindingStatistic _
derive instance repGenericCountBySeverityFindingStatistic :: Generic CountBySeverityFindingStatistic _
instance showCountBySeverityFindingStatistic :: Show CountBySeverityFindingStatistic where
  show = genericShow
instance decodeCountBySeverityFindingStatistic :: Decode CountBySeverityFindingStatistic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCountBySeverityFindingStatistic :: Encode CountBySeverityFindingStatistic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Country information of the remote IP address.
newtype Country = Country 
  { "CountryCode" :: NullOrUndefined.NullOrUndefined (String)
  , "CountryName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCountry :: Newtype Country _
derive instance repGenericCountry :: Generic Country _
instance showCountry :: Show Country where
  show = genericShow
instance decodeCountry :: Decode Country where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCountry :: Encode Country where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | CreateDetector request body.
newtype CreateDetectorRequest = CreateDetectorRequest 
  { "Enable" :: NullOrUndefined.NullOrUndefined (Enable)
  }
derive instance newtypeCreateDetectorRequest :: Newtype CreateDetectorRequest _
derive instance repGenericCreateDetectorRequest :: Generic CreateDetectorRequest _
instance showCreateDetectorRequest :: Show CreateDetectorRequest where
  show = genericShow
instance decodeCreateDetectorRequest :: Decode CreateDetectorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDetectorRequest :: Encode CreateDetectorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDetectorResponse = CreateDetectorResponse 
  { "DetectorId" :: NullOrUndefined.NullOrUndefined (DetectorId)
  }
derive instance newtypeCreateDetectorResponse :: Newtype CreateDetectorResponse _
derive instance repGenericCreateDetectorResponse :: Generic CreateDetectorResponse _
instance showCreateDetectorResponse :: Show CreateDetectorResponse where
  show = genericShow
instance decodeCreateDetectorResponse :: Decode CreateDetectorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDetectorResponse :: Encode CreateDetectorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | CreateIPSet request body.
newtype CreateIPSetRequest = CreateIPSetRequest 
  { "Activate" :: NullOrUndefined.NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "Format" :: NullOrUndefined.NullOrUndefined (IpSetFormat)
  , "Location" :: NullOrUndefined.NullOrUndefined (Location)
  , "Name" :: NullOrUndefined.NullOrUndefined (Name)
  }
derive instance newtypeCreateIPSetRequest :: Newtype CreateIPSetRequest _
derive instance repGenericCreateIPSetRequest :: Generic CreateIPSetRequest _
instance showCreateIPSetRequest :: Show CreateIPSetRequest where
  show = genericShow
instance decodeCreateIPSetRequest :: Decode CreateIPSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIPSetRequest :: Encode CreateIPSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateIPSetResponse = CreateIPSetResponse 
  { "IpSetId" :: NullOrUndefined.NullOrUndefined (IpSetId)
  }
derive instance newtypeCreateIPSetResponse :: Newtype CreateIPSetResponse _
derive instance repGenericCreateIPSetResponse :: Generic CreateIPSetResponse _
instance showCreateIPSetResponse :: Show CreateIPSetResponse where
  show = genericShow
instance decodeCreateIPSetResponse :: Decode CreateIPSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateIPSetResponse :: Encode CreateIPSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | CreateMembers request body.
newtype CreateMembersRequest = CreateMembersRequest 
  { "AccountDetails" :: NullOrUndefined.NullOrUndefined (AccountDetails)
  , "DetectorId" :: (String)
  }
derive instance newtypeCreateMembersRequest :: Newtype CreateMembersRequest _
derive instance repGenericCreateMembersRequest :: Generic CreateMembersRequest _
instance showCreateMembersRequest :: Show CreateMembersRequest where
  show = genericShow
instance decodeCreateMembersRequest :: Decode CreateMembersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMembersRequest :: Encode CreateMembersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateMembersResponse = CreateMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined.NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeCreateMembersResponse :: Newtype CreateMembersResponse _
derive instance repGenericCreateMembersResponse :: Generic CreateMembersResponse _
instance showCreateMembersResponse :: Show CreateMembersResponse where
  show = genericShow
instance decodeCreateMembersResponse :: Decode CreateMembersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMembersResponse :: Encode CreateMembersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | CreateSampleFindings request body.
newtype CreateSampleFindingsRequest = CreateSampleFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingTypes" :: NullOrUndefined.NullOrUndefined (FindingTypes)
  }
derive instance newtypeCreateSampleFindingsRequest :: Newtype CreateSampleFindingsRequest _
derive instance repGenericCreateSampleFindingsRequest :: Generic CreateSampleFindingsRequest _
instance showCreateSampleFindingsRequest :: Show CreateSampleFindingsRequest where
  show = genericShow
instance decodeCreateSampleFindingsRequest :: Decode CreateSampleFindingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSampleFindingsRequest :: Encode CreateSampleFindingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSampleFindingsResponse = CreateSampleFindingsResponse Types.NoArguments
derive instance newtypeCreateSampleFindingsResponse :: Newtype CreateSampleFindingsResponse _
derive instance repGenericCreateSampleFindingsResponse :: Generic CreateSampleFindingsResponse _
instance showCreateSampleFindingsResponse :: Show CreateSampleFindingsResponse where
  show = genericShow
instance decodeCreateSampleFindingsResponse :: Decode CreateSampleFindingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSampleFindingsResponse :: Encode CreateSampleFindingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | CreateThreatIntelSet request body.
newtype CreateThreatIntelSetRequest = CreateThreatIntelSetRequest 
  { "Activate" :: NullOrUndefined.NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "Format" :: NullOrUndefined.NullOrUndefined (ThreatIntelSetFormat)
  , "Location" :: NullOrUndefined.NullOrUndefined (Location)
  , "Name" :: NullOrUndefined.NullOrUndefined (Name)
  }
derive instance newtypeCreateThreatIntelSetRequest :: Newtype CreateThreatIntelSetRequest _
derive instance repGenericCreateThreatIntelSetRequest :: Generic CreateThreatIntelSetRequest _
instance showCreateThreatIntelSetRequest :: Show CreateThreatIntelSetRequest where
  show = genericShow
instance decodeCreateThreatIntelSetRequest :: Decode CreateThreatIntelSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateThreatIntelSetRequest :: Encode CreateThreatIntelSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateThreatIntelSetResponse = CreateThreatIntelSetResponse 
  { "ThreatIntelSetId" :: NullOrUndefined.NullOrUndefined (ThreatIntelSetId)
  }
derive instance newtypeCreateThreatIntelSetResponse :: Newtype CreateThreatIntelSetResponse _
derive instance repGenericCreateThreatIntelSetResponse :: Generic CreateThreatIntelSetResponse _
instance showCreateThreatIntelSetResponse :: Show CreateThreatIntelSetResponse where
  show = genericShow
instance decodeCreateThreatIntelSetResponse :: Decode CreateThreatIntelSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateThreatIntelSetResponse :: Encode CreateThreatIntelSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The first time a resource was created. The format will be ISO-8601.
newtype CreatedAt = CreatedAt String
derive instance newtypeCreatedAt :: Newtype CreatedAt _
derive instance repGenericCreatedAt :: Generic CreatedAt _
instance showCreatedAt :: Show CreatedAt where
  show = genericShow
instance decodeCreatedAt :: Decode CreatedAt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatedAt :: Encode CreatedAt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | DeclineInvitations request body.
newtype DeclineInvitationsRequest = DeclineInvitationsRequest 
  { "AccountIds" :: NullOrUndefined.NullOrUndefined (AccountIds)
  }
derive instance newtypeDeclineInvitationsRequest :: Newtype DeclineInvitationsRequest _
derive instance repGenericDeclineInvitationsRequest :: Generic DeclineInvitationsRequest _
instance showDeclineInvitationsRequest :: Show DeclineInvitationsRequest where
  show = genericShow
instance decodeDeclineInvitationsRequest :: Decode DeclineInvitationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeclineInvitationsRequest :: Encode DeclineInvitationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeclineInvitationsResponse = DeclineInvitationsResponse 
  { "UnprocessedAccounts" :: NullOrUndefined.NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeDeclineInvitationsResponse :: Newtype DeclineInvitationsResponse _
derive instance repGenericDeclineInvitationsResponse :: Generic DeclineInvitationsResponse _
instance showDeclineInvitationsResponse :: Show DeclineInvitationsResponse where
  show = genericShow
instance decodeDeclineInvitationsResponse :: Decode DeclineInvitationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeclineInvitationsResponse :: Encode DeclineInvitationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDetectorRequest = DeleteDetectorRequest 
  { "DetectorId" :: (String)
  }
derive instance newtypeDeleteDetectorRequest :: Newtype DeleteDetectorRequest _
derive instance repGenericDeleteDetectorRequest :: Generic DeleteDetectorRequest _
instance showDeleteDetectorRequest :: Show DeleteDetectorRequest where
  show = genericShow
instance decodeDeleteDetectorRequest :: Decode DeleteDetectorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDetectorRequest :: Encode DeleteDetectorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDetectorResponse = DeleteDetectorResponse Types.NoArguments
derive instance newtypeDeleteDetectorResponse :: Newtype DeleteDetectorResponse _
derive instance repGenericDeleteDetectorResponse :: Generic DeleteDetectorResponse _
instance showDeleteDetectorResponse :: Show DeleteDetectorResponse where
  show = genericShow
instance decodeDeleteDetectorResponse :: Decode DeleteDetectorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDetectorResponse :: Encode DeleteDetectorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteIPSetRequest = DeleteIPSetRequest 
  { "DetectorId" :: (String)
  , "IpSetId" :: (String)
  }
derive instance newtypeDeleteIPSetRequest :: Newtype DeleteIPSetRequest _
derive instance repGenericDeleteIPSetRequest :: Generic DeleteIPSetRequest _
instance showDeleteIPSetRequest :: Show DeleteIPSetRequest where
  show = genericShow
instance decodeDeleteIPSetRequest :: Decode DeleteIPSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIPSetRequest :: Encode DeleteIPSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteIPSetResponse = DeleteIPSetResponse Types.NoArguments
derive instance newtypeDeleteIPSetResponse :: Newtype DeleteIPSetResponse _
derive instance repGenericDeleteIPSetResponse :: Generic DeleteIPSetResponse _
instance showDeleteIPSetResponse :: Show DeleteIPSetResponse where
  show = genericShow
instance decodeDeleteIPSetResponse :: Decode DeleteIPSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteIPSetResponse :: Encode DeleteIPSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | DeleteInvitations request body.
newtype DeleteInvitationsRequest = DeleteInvitationsRequest 
  { "AccountIds" :: NullOrUndefined.NullOrUndefined (AccountIds)
  }
derive instance newtypeDeleteInvitationsRequest :: Newtype DeleteInvitationsRequest _
derive instance repGenericDeleteInvitationsRequest :: Generic DeleteInvitationsRequest _
instance showDeleteInvitationsRequest :: Show DeleteInvitationsRequest where
  show = genericShow
instance decodeDeleteInvitationsRequest :: Decode DeleteInvitationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInvitationsRequest :: Encode DeleteInvitationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteInvitationsResponse = DeleteInvitationsResponse 
  { "UnprocessedAccounts" :: NullOrUndefined.NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeDeleteInvitationsResponse :: Newtype DeleteInvitationsResponse _
derive instance repGenericDeleteInvitationsResponse :: Generic DeleteInvitationsResponse _
instance showDeleteInvitationsResponse :: Show DeleteInvitationsResponse where
  show = genericShow
instance decodeDeleteInvitationsResponse :: Decode DeleteInvitationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteInvitationsResponse :: Encode DeleteInvitationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | DeleteMembers request body.
newtype DeleteMembersRequest = DeleteMembersRequest 
  { "AccountIds" :: NullOrUndefined.NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeDeleteMembersRequest :: Newtype DeleteMembersRequest _
derive instance repGenericDeleteMembersRequest :: Generic DeleteMembersRequest _
instance showDeleteMembersRequest :: Show DeleteMembersRequest where
  show = genericShow
instance decodeDeleteMembersRequest :: Decode DeleteMembersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMembersRequest :: Encode DeleteMembersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMembersResponse = DeleteMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined.NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeDeleteMembersResponse :: Newtype DeleteMembersResponse _
derive instance repGenericDeleteMembersResponse :: Generic DeleteMembersResponse _
instance showDeleteMembersResponse :: Show DeleteMembersResponse where
  show = genericShow
instance decodeDeleteMembersResponse :: Decode DeleteMembersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMembersResponse :: Encode DeleteMembersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteThreatIntelSetRequest = DeleteThreatIntelSetRequest 
  { "DetectorId" :: (String)
  , "ThreatIntelSetId" :: (String)
  }
derive instance newtypeDeleteThreatIntelSetRequest :: Newtype DeleteThreatIntelSetRequest _
derive instance repGenericDeleteThreatIntelSetRequest :: Generic DeleteThreatIntelSetRequest _
instance showDeleteThreatIntelSetRequest :: Show DeleteThreatIntelSetRequest where
  show = genericShow
instance decodeDeleteThreatIntelSetRequest :: Decode DeleteThreatIntelSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteThreatIntelSetRequest :: Encode DeleteThreatIntelSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteThreatIntelSetResponse = DeleteThreatIntelSetResponse Types.NoArguments
derive instance newtypeDeleteThreatIntelSetResponse :: Newtype DeleteThreatIntelSetResponse _
derive instance repGenericDeleteThreatIntelSetResponse :: Generic DeleteThreatIntelSetResponse _
instance showDeleteThreatIntelSetResponse :: Show DeleteThreatIntelSetResponse where
  show = genericShow
instance decodeDeleteThreatIntelSetResponse :: Decode DeleteThreatIntelSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteThreatIntelSetResponse :: Encode DeleteThreatIntelSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The unique identifier for a detector.
newtype DetectorId = DetectorId String
derive instance newtypeDetectorId :: Newtype DetectorId _
derive instance repGenericDetectorId :: Generic DetectorId _
instance showDetectorId :: Show DetectorId where
  show = genericShow
instance decodeDetectorId :: Decode DetectorId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectorId :: Encode DetectorId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of detector Ids.
newtype DetectorIds = DetectorIds (Array DetectorId)
derive instance newtypeDetectorIds :: Newtype DetectorIds _
derive instance repGenericDetectorIds :: Generic DetectorIds _
instance showDetectorIds :: Show DetectorIds where
  show = genericShow
instance decodeDetectorIds :: Decode DetectorIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectorIds :: Encode DetectorIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The status of detector.
newtype DetectorStatus = DetectorStatus String
derive instance newtypeDetectorStatus :: Newtype DetectorStatus _
derive instance repGenericDetectorStatus :: Generic DetectorStatus _
instance showDetectorStatus :: Show DetectorStatus where
  show = genericShow
instance decodeDetectorStatus :: Decode DetectorStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetectorStatus :: Encode DetectorStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateFromMasterAccountRequest = DisassociateFromMasterAccountRequest 
  { "DetectorId" :: (String)
  }
derive instance newtypeDisassociateFromMasterAccountRequest :: Newtype DisassociateFromMasterAccountRequest _
derive instance repGenericDisassociateFromMasterAccountRequest :: Generic DisassociateFromMasterAccountRequest _
instance showDisassociateFromMasterAccountRequest :: Show DisassociateFromMasterAccountRequest where
  show = genericShow
instance decodeDisassociateFromMasterAccountRequest :: Decode DisassociateFromMasterAccountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateFromMasterAccountRequest :: Encode DisassociateFromMasterAccountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateFromMasterAccountResponse = DisassociateFromMasterAccountResponse Types.NoArguments
derive instance newtypeDisassociateFromMasterAccountResponse :: Newtype DisassociateFromMasterAccountResponse _
derive instance repGenericDisassociateFromMasterAccountResponse :: Generic DisassociateFromMasterAccountResponse _
instance showDisassociateFromMasterAccountResponse :: Show DisassociateFromMasterAccountResponse where
  show = genericShow
instance decodeDisassociateFromMasterAccountResponse :: Decode DisassociateFromMasterAccountResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateFromMasterAccountResponse :: Encode DisassociateFromMasterAccountResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | DisassociateMembers request body.
newtype DisassociateMembersRequest = DisassociateMembersRequest 
  { "AccountIds" :: NullOrUndefined.NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeDisassociateMembersRequest :: Newtype DisassociateMembersRequest _
derive instance repGenericDisassociateMembersRequest :: Generic DisassociateMembersRequest _
instance showDisassociateMembersRequest :: Show DisassociateMembersRequest where
  show = genericShow
instance decodeDisassociateMembersRequest :: Decode DisassociateMembersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateMembersRequest :: Encode DisassociateMembersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateMembersResponse = DisassociateMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined.NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeDisassociateMembersResponse :: Newtype DisassociateMembersResponse _
derive instance repGenericDisassociateMembersResponse :: Generic DisassociateMembersResponse _
instance showDisassociateMembersResponse :: Show DisassociateMembersResponse where
  show = genericShow
instance decodeDisassociateMembersResponse :: Decode DisassociateMembersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateMembersResponse :: Encode DisassociateMembersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information about the DNS_REQUEST action described in this finding.
newtype DnsRequestAction = DnsRequestAction 
  { "Domain" :: NullOrUndefined.NullOrUndefined (Domain)
  }
derive instance newtypeDnsRequestAction :: Newtype DnsRequestAction _
derive instance repGenericDnsRequestAction :: Generic DnsRequestAction _
instance showDnsRequestAction :: Show DnsRequestAction where
  show = genericShow
instance decodeDnsRequestAction :: Decode DnsRequestAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDnsRequestAction :: Encode DnsRequestAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A domain name.
newtype Domain = Domain String
derive instance newtypeDomain :: Newtype Domain _
derive instance repGenericDomain :: Generic Domain _
instance showDomain :: Show Domain where
  show = genericShow
instance decodeDomain :: Decode Domain where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomain :: Encode Domain where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Domain information for the AWS API call.
newtype DomainDetails = DomainDetails Types.NoArguments
derive instance newtypeDomainDetails :: Newtype DomainDetails _
derive instance repGenericDomainDetails :: Generic DomainDetails _
instance showDomainDetails :: Show DomainDetails where
  show = genericShow
instance decodeDomainDetails :: Decode DomainDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainDetails :: Encode DomainDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Member account's email address.
newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _
derive instance repGenericEmail :: Generic Email _
instance showEmail :: Show Email where
  show = genericShow
instance decodeEmail :: Decode Email where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmail :: Encode Email where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A boolean value that specifies whether the detector is to be enabled.
newtype Enable = Enable Boolean
derive instance newtypeEnable :: Newtype Enable _
derive instance repGenericEnable :: Generic Enable _
instance showEnable :: Show Enable where
  show = genericShow
instance decodeEnable :: Decode Enable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnable :: Encode Enable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Represents the equal condition to be applied to a single field when querying for findings.
newtype Eq = Eq (Array String)
derive instance newtypeEq :: Newtype Eq _
derive instance repGenericEq :: Generic Eq _
instance showEq :: Show Eq where
  show = genericShow
instance decodeEq :: Decode Eq where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEq :: Encode Eq where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Error response object.
newtype ErrorResponse = ErrorResponse 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeErrorResponse :: Newtype ErrorResponse _
derive instance repGenericErrorResponse :: Generic ErrorResponse _
instance showErrorResponse :: Show ErrorResponse where
  show = genericShow
instance decodeErrorResponse :: Decode ErrorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorResponse :: Encode ErrorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Finding Feedback Value
newtype Feedback = Feedback String
derive instance newtypeFeedback :: Newtype Feedback _
derive instance repGenericFeedback :: Generic Feedback _
instance showFeedback :: Show Feedback where
  show = genericShow
instance decodeFeedback :: Decode Feedback where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFeedback :: Encode Feedback where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Representation of a abnormal or suspicious activity.
newtype Finding = Finding 
  { "AccountId" :: NullOrUndefined.NullOrUndefined (String)
  , "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Confidence" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedAt" :: NullOrUndefined.NullOrUndefined (CreatedAt)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "Partition" :: NullOrUndefined.NullOrUndefined (String)
  , "Region" :: NullOrUndefined.NullOrUndefined (String)
  , "Resource" :: NullOrUndefined.NullOrUndefined (Resource)
  , "SchemaVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "Service" :: NullOrUndefined.NullOrUndefined (Service)
  , "Severity" :: NullOrUndefined.NullOrUndefined (Number)
  , "Title" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  , "UpdatedAt" :: NullOrUndefined.NullOrUndefined (UpdatedAt)
  }
derive instance newtypeFinding :: Newtype Finding _
derive instance repGenericFinding :: Generic Finding _
instance showFinding :: Show Finding where
  show = genericShow
instance decodeFinding :: Decode Finding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFinding :: Encode Finding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Represents the criteria used for querying findings.
newtype FindingCriteria = FindingCriteria 
  { "Criterion" :: NullOrUndefined.NullOrUndefined (MapOfCondition)
  }
derive instance newtypeFindingCriteria :: Newtype FindingCriteria _
derive instance repGenericFindingCriteria :: Generic FindingCriteria _
instance showFindingCriteria :: Show FindingCriteria where
  show = genericShow
instance decodeFindingCriteria :: Decode FindingCriteria where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFindingCriteria :: Encode FindingCriteria where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The unique identifier for the Finding
newtype FindingId = FindingId String
derive instance newtypeFindingId :: Newtype FindingId _
derive instance repGenericFindingId :: Generic FindingId _
instance showFindingId :: Show FindingId where
  show = genericShow
instance decodeFindingId :: Decode FindingId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFindingId :: Encode FindingId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The list of the Findings.
newtype FindingIds = FindingIds (Array FindingId)
derive instance newtypeFindingIds :: Newtype FindingIds _
derive instance repGenericFindingIds :: Generic FindingIds _
instance showFindingIds :: Show FindingIds where
  show = genericShow
instance decodeFindingIds :: Decode FindingIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFindingIds :: Encode FindingIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The types of finding statistics.
newtype FindingStatisticType = FindingStatisticType String
derive instance newtypeFindingStatisticType :: Newtype FindingStatisticType _
derive instance repGenericFindingStatisticType :: Generic FindingStatisticType _
instance showFindingStatisticType :: Show FindingStatisticType where
  show = genericShow
instance decodeFindingStatisticType :: Decode FindingStatisticType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFindingStatisticType :: Encode FindingStatisticType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The list of the finding statistics.
newtype FindingStatisticTypes = FindingStatisticTypes (Array FindingStatisticType)
derive instance newtypeFindingStatisticTypes :: Newtype FindingStatisticTypes _
derive instance repGenericFindingStatisticTypes :: Generic FindingStatisticTypes _
instance showFindingStatisticTypes :: Show FindingStatisticTypes where
  show = genericShow
instance decodeFindingStatisticTypes :: Decode FindingStatisticTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFindingStatisticTypes :: Encode FindingStatisticTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Finding statistics object.
newtype FindingStatistics = FindingStatistics 
  { "CountBySeverity" :: NullOrUndefined.NullOrUndefined (MapOfCountBySeverityFindingStatistic)
  }
derive instance newtypeFindingStatistics :: Newtype FindingStatistics _
derive instance repGenericFindingStatistics :: Generic FindingStatistics _
instance showFindingStatistics :: Show FindingStatistics where
  show = genericShow
instance decodeFindingStatistics :: Decode FindingStatistics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFindingStatistics :: Encode FindingStatistics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The finding type for the finding
newtype FindingType = FindingType String
derive instance newtypeFindingType :: Newtype FindingType _
derive instance repGenericFindingType :: Generic FindingType _
instance showFindingType :: Show FindingType where
  show = genericShow
instance decodeFindingType :: Decode FindingType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFindingType :: Encode FindingType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The list of the finding types.
newtype FindingTypes = FindingTypes (Array FindingType)
derive instance newtypeFindingTypes :: Newtype FindingTypes _
derive instance repGenericFindingTypes :: Generic FindingTypes _
instance showFindingTypes :: Show FindingTypes where
  show = genericShow
instance decodeFindingTypes :: Decode FindingTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFindingTypes :: Encode FindingTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of findings.
newtype Findings = Findings (Array Finding)
derive instance newtypeFindings :: Newtype Findings _
derive instance repGenericFindings :: Generic Findings _
instance showFindings :: Show Findings where
  show = genericShow
instance decodeFindings :: Decode Findings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFindings :: Encode Findings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Location information of the remote IP address.
newtype GeoLocation = GeoLocation 
  { "Lat" :: NullOrUndefined.NullOrUndefined (Number)
  , "Lon" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeGeoLocation :: Newtype GeoLocation _
derive instance repGenericGeoLocation :: Generic GeoLocation _
instance showGeoLocation :: Show GeoLocation where
  show = genericShow
instance decodeGeoLocation :: Decode GeoLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocation :: Encode GeoLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDetectorRequest = GetDetectorRequest 
  { "DetectorId" :: (String)
  }
derive instance newtypeGetDetectorRequest :: Newtype GetDetectorRequest _
derive instance repGenericGetDetectorRequest :: Generic GetDetectorRequest _
instance showGetDetectorRequest :: Show GetDetectorRequest where
  show = genericShow
instance decodeGetDetectorRequest :: Decode GetDetectorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDetectorRequest :: Encode GetDetectorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDetectorResponse = GetDetectorResponse 
  { "CreatedAt" :: NullOrUndefined.NullOrUndefined (CreatedAt)
  , "ServiceRole" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "Status" :: NullOrUndefined.NullOrUndefined (DetectorStatus)
  , "UpdatedAt" :: NullOrUndefined.NullOrUndefined (UpdatedAt)
  }
derive instance newtypeGetDetectorResponse :: Newtype GetDetectorResponse _
derive instance repGenericGetDetectorResponse :: Generic GetDetectorResponse _
instance showGetDetectorResponse :: Show GetDetectorResponse where
  show = genericShow
instance decodeGetDetectorResponse :: Decode GetDetectorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDetectorResponse :: Encode GetDetectorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | GetFindings request body.
newtype GetFindingsRequest = GetFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingIds" :: NullOrUndefined.NullOrUndefined (FindingIds)
  , "SortCriteria" :: NullOrUndefined.NullOrUndefined (SortCriteria)
  }
derive instance newtypeGetFindingsRequest :: Newtype GetFindingsRequest _
derive instance repGenericGetFindingsRequest :: Generic GetFindingsRequest _
instance showGetFindingsRequest :: Show GetFindingsRequest where
  show = genericShow
instance decodeGetFindingsRequest :: Decode GetFindingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFindingsRequest :: Encode GetFindingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFindingsResponse = GetFindingsResponse 
  { "Findings" :: NullOrUndefined.NullOrUndefined (Findings)
  }
derive instance newtypeGetFindingsResponse :: Newtype GetFindingsResponse _
derive instance repGenericGetFindingsResponse :: Generic GetFindingsResponse _
instance showGetFindingsResponse :: Show GetFindingsResponse where
  show = genericShow
instance decodeGetFindingsResponse :: Decode GetFindingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFindingsResponse :: Encode GetFindingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | GetFindingsStatistics request body.
newtype GetFindingsStatisticsRequest = GetFindingsStatisticsRequest 
  { "DetectorId" :: (String)
  , "FindingCriteria" :: NullOrUndefined.NullOrUndefined (FindingCriteria)
  , "FindingStatisticTypes" :: NullOrUndefined.NullOrUndefined (FindingStatisticTypes)
  }
derive instance newtypeGetFindingsStatisticsRequest :: Newtype GetFindingsStatisticsRequest _
derive instance repGenericGetFindingsStatisticsRequest :: Generic GetFindingsStatisticsRequest _
instance showGetFindingsStatisticsRequest :: Show GetFindingsStatisticsRequest where
  show = genericShow
instance decodeGetFindingsStatisticsRequest :: Decode GetFindingsStatisticsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFindingsStatisticsRequest :: Encode GetFindingsStatisticsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFindingsStatisticsResponse = GetFindingsStatisticsResponse 
  { "FindingStatistics" :: NullOrUndefined.NullOrUndefined (FindingStatistics)
  }
derive instance newtypeGetFindingsStatisticsResponse :: Newtype GetFindingsStatisticsResponse _
derive instance repGenericGetFindingsStatisticsResponse :: Generic GetFindingsStatisticsResponse _
instance showGetFindingsStatisticsResponse :: Show GetFindingsStatisticsResponse where
  show = genericShow
instance decodeGetFindingsStatisticsResponse :: Decode GetFindingsStatisticsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFindingsStatisticsResponse :: Encode GetFindingsStatisticsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIPSetRequest = GetIPSetRequest 
  { "DetectorId" :: (String)
  , "IpSetId" :: (String)
  }
derive instance newtypeGetIPSetRequest :: Newtype GetIPSetRequest _
derive instance repGenericGetIPSetRequest :: Generic GetIPSetRequest _
instance showGetIPSetRequest :: Show GetIPSetRequest where
  show = genericShow
instance decodeGetIPSetRequest :: Decode GetIPSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIPSetRequest :: Encode GetIPSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetIPSetResponse = GetIPSetResponse 
  { "Format" :: NullOrUndefined.NullOrUndefined (IpSetFormat)
  , "Location" :: NullOrUndefined.NullOrUndefined (Location)
  , "Name" :: NullOrUndefined.NullOrUndefined (Name)
  , "Status" :: NullOrUndefined.NullOrUndefined (IpSetStatus)
  }
derive instance newtypeGetIPSetResponse :: Newtype GetIPSetResponse _
derive instance repGenericGetIPSetResponse :: Generic GetIPSetResponse _
instance showGetIPSetResponse :: Show GetIPSetResponse where
  show = genericShow
instance decodeGetIPSetResponse :: Decode GetIPSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetIPSetResponse :: Encode GetIPSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInvitationsCountRequest = GetInvitationsCountRequest Types.NoArguments
derive instance newtypeGetInvitationsCountRequest :: Newtype GetInvitationsCountRequest _
derive instance repGenericGetInvitationsCountRequest :: Generic GetInvitationsCountRequest _
instance showGetInvitationsCountRequest :: Show GetInvitationsCountRequest where
  show = genericShow
instance decodeGetInvitationsCountRequest :: Decode GetInvitationsCountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInvitationsCountRequest :: Encode GetInvitationsCountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInvitationsCountResponse = GetInvitationsCountResponse 
  { "InvitationsCount" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeGetInvitationsCountResponse :: Newtype GetInvitationsCountResponse _
derive instance repGenericGetInvitationsCountResponse :: Generic GetInvitationsCountResponse _
instance showGetInvitationsCountResponse :: Show GetInvitationsCountResponse where
  show = genericShow
instance decodeGetInvitationsCountResponse :: Decode GetInvitationsCountResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInvitationsCountResponse :: Encode GetInvitationsCountResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMasterAccountRequest = GetMasterAccountRequest 
  { "DetectorId" :: (String)
  }
derive instance newtypeGetMasterAccountRequest :: Newtype GetMasterAccountRequest _
derive instance repGenericGetMasterAccountRequest :: Generic GetMasterAccountRequest _
instance showGetMasterAccountRequest :: Show GetMasterAccountRequest where
  show = genericShow
instance decodeGetMasterAccountRequest :: Decode GetMasterAccountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMasterAccountRequest :: Encode GetMasterAccountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMasterAccountResponse = GetMasterAccountResponse 
  { "Master" :: NullOrUndefined.NullOrUndefined (Master)
  }
derive instance newtypeGetMasterAccountResponse :: Newtype GetMasterAccountResponse _
derive instance repGenericGetMasterAccountResponse :: Generic GetMasterAccountResponse _
instance showGetMasterAccountResponse :: Show GetMasterAccountResponse where
  show = genericShow
instance decodeGetMasterAccountResponse :: Decode GetMasterAccountResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMasterAccountResponse :: Encode GetMasterAccountResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | GetMembers request body.
newtype GetMembersRequest = GetMembersRequest 
  { "AccountIds" :: NullOrUndefined.NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeGetMembersRequest :: Newtype GetMembersRequest _
derive instance repGenericGetMembersRequest :: Generic GetMembersRequest _
instance showGetMembersRequest :: Show GetMembersRequest where
  show = genericShow
instance decodeGetMembersRequest :: Decode GetMembersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMembersRequest :: Encode GetMembersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMembersResponse = GetMembersResponse 
  { "Members" :: NullOrUndefined.NullOrUndefined (Members)
  , "UnprocessedAccounts" :: NullOrUndefined.NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeGetMembersResponse :: Newtype GetMembersResponse _
derive instance repGenericGetMembersResponse :: Generic GetMembersResponse _
instance showGetMembersResponse :: Show GetMembersResponse where
  show = genericShow
instance decodeGetMembersResponse :: Decode GetMembersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMembersResponse :: Encode GetMembersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetThreatIntelSetRequest = GetThreatIntelSetRequest 
  { "DetectorId" :: (String)
  , "ThreatIntelSetId" :: (String)
  }
derive instance newtypeGetThreatIntelSetRequest :: Newtype GetThreatIntelSetRequest _
derive instance repGenericGetThreatIntelSetRequest :: Generic GetThreatIntelSetRequest _
instance showGetThreatIntelSetRequest :: Show GetThreatIntelSetRequest where
  show = genericShow
instance decodeGetThreatIntelSetRequest :: Decode GetThreatIntelSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetThreatIntelSetRequest :: Encode GetThreatIntelSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetThreatIntelSetResponse = GetThreatIntelSetResponse 
  { "Format" :: NullOrUndefined.NullOrUndefined (ThreatIntelSetFormat)
  , "Location" :: NullOrUndefined.NullOrUndefined (Location)
  , "Name" :: NullOrUndefined.NullOrUndefined (Name)
  , "Status" :: NullOrUndefined.NullOrUndefined (ThreatIntelSetStatus)
  }
derive instance newtypeGetThreatIntelSetResponse :: Newtype GetThreatIntelSetResponse _
derive instance repGenericGetThreatIntelSetResponse :: Generic GetThreatIntelSetResponse _
instance showGetThreatIntelSetResponse :: Show GetThreatIntelSetResponse where
  show = genericShow
instance decodeGetThreatIntelSetResponse :: Decode GetThreatIntelSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetThreatIntelSetResponse :: Encode GetThreatIntelSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The profile information of the EC2 instance.
newtype IamInstanceProfile = IamInstanceProfile 
  { "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Id" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeIamInstanceProfile :: Newtype IamInstanceProfile _
derive instance repGenericIamInstanceProfile :: Generic IamInstanceProfile _
instance showIamInstanceProfile :: Show IamInstanceProfile where
  show = genericShow
instance decodeIamInstanceProfile :: Decode IamInstanceProfile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIamInstanceProfile :: Encode IamInstanceProfile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The information about the EC2 instance associated with the activity that prompted GuardDuty to generate a finding.
newtype InstanceDetails = InstanceDetails 
  { "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "IamInstanceProfile" :: NullOrUndefined.NullOrUndefined (IamInstanceProfile)
  , "ImageId" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceState" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined.NullOrUndefined (String)
  , "LaunchTime" :: NullOrUndefined.NullOrUndefined (String)
  , "NetworkInterfaces" :: NullOrUndefined.NullOrUndefined (NetworkInterfaces)
  , "Platform" :: NullOrUndefined.NullOrUndefined (String)
  , "ProductCodes" :: NullOrUndefined.NullOrUndefined (ProductCodes)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  }
derive instance newtypeInstanceDetails :: Newtype InstanceDetails _
derive instance repGenericInstanceDetails :: Generic InstanceDetails _
instance showInstanceDetails :: Show InstanceDetails where
  show = genericShow
instance decodeInstanceDetails :: Decode InstanceDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceDetails :: Encode InstanceDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Error response object.
newtype InternalServerErrorException = InternalServerErrorException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalServerErrorException :: Newtype InternalServerErrorException _
derive instance repGenericInternalServerErrorException :: Generic InternalServerErrorException _
instance showInternalServerErrorException :: Show InternalServerErrorException where
  show = genericShow
instance decodeInternalServerErrorException :: Decode InternalServerErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerErrorException :: Encode InternalServerErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Invitation from an AWS account to become the current account's master.
newtype Invitation = Invitation 
  { "AccountId" :: NullOrUndefined.NullOrUndefined (String)
  , "InvitationId" :: NullOrUndefined.NullOrUndefined (InvitationId)
  , "InvitedAt" :: NullOrUndefined.NullOrUndefined (InvitedAt)
  , "RelationshipStatus" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvitation :: Newtype Invitation _
derive instance repGenericInvitation :: Generic Invitation _
instance showInvitation :: Show Invitation where
  show = genericShow
instance decodeInvitation :: Decode Invitation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvitation :: Encode Invitation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | This value is used to validate the master account to the member account.
newtype InvitationId = InvitationId String
derive instance newtypeInvitationId :: Newtype InvitationId _
derive instance repGenericInvitationId :: Generic InvitationId _
instance showInvitationId :: Show InvitationId where
  show = genericShow
instance decodeInvitationId :: Decode InvitationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvitationId :: Encode InvitationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of invitation descriptions.
newtype Invitations = Invitations (Array Invitation)
derive instance newtypeInvitations :: Newtype Invitations _
derive instance repGenericInvitations :: Generic Invitations _
instance showInvitations :: Show Invitations where
  show = genericShow
instance decodeInvitations :: Decode Invitations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvitations :: Encode Invitations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | InviteMembers request body.
newtype InviteMembersRequest = InviteMembersRequest 
  { "AccountIds" :: NullOrUndefined.NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeInviteMembersRequest :: Newtype InviteMembersRequest _
derive instance repGenericInviteMembersRequest :: Generic InviteMembersRequest _
instance showInviteMembersRequest :: Show InviteMembersRequest where
  show = genericShow
instance decodeInviteMembersRequest :: Decode InviteMembersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInviteMembersRequest :: Encode InviteMembersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InviteMembersResponse = InviteMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined.NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeInviteMembersResponse :: Newtype InviteMembersResponse _
derive instance repGenericInviteMembersResponse :: Generic InviteMembersResponse _
instance showInviteMembersResponse :: Show InviteMembersResponse where
  show = genericShow
instance decodeInviteMembersResponse :: Decode InviteMembersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInviteMembersResponse :: Encode InviteMembersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Timestamp at which a member has been invited. The format will be ISO-8601.
newtype InvitedAt = InvitedAt String
derive instance newtypeInvitedAt :: Newtype InvitedAt _
derive instance repGenericInvitedAt :: Generic InvitedAt _
instance showInvitedAt :: Show InvitedAt where
  show = genericShow
instance decodeInvitedAt :: Decode InvitedAt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvitedAt :: Encode InvitedAt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The format of the ipSet.
newtype IpSetFormat = IpSetFormat String
derive instance newtypeIpSetFormat :: Newtype IpSetFormat _
derive instance repGenericIpSetFormat :: Generic IpSetFormat _
instance showIpSetFormat :: Show IpSetFormat where
  show = genericShow
instance decodeIpSetFormat :: Decode IpSetFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpSetFormat :: Encode IpSetFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The unique identifier for an IP Set
newtype IpSetId = IpSetId String
derive instance newtypeIpSetId :: Newtype IpSetId _
derive instance repGenericIpSetId :: Generic IpSetId _
instance showIpSetId :: Show IpSetId where
  show = genericShow
instance decodeIpSetId :: Decode IpSetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpSetId :: Encode IpSetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of the IP set IDs
newtype IpSetIds = IpSetIds (Array IpSetId)
derive instance newtypeIpSetIds :: Newtype IpSetIds _
derive instance repGenericIpSetIds :: Generic IpSetIds _
instance showIpSetIds :: Show IpSetIds where
  show = genericShow
instance decodeIpSetIds :: Decode IpSetIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpSetIds :: Encode IpSetIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The status of ipSet file uploaded.
newtype IpSetStatus = IpSetStatus String
derive instance newtypeIpSetStatus :: Newtype IpSetStatus _
derive instance repGenericIpSetStatus :: Generic IpSetStatus _
instance showIpSetStatus :: Show IpSetStatus where
  show = genericShow
instance decodeIpSetStatus :: Decode IpSetStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpSetStatus :: Encode IpSetStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | IpV6 address of the EC2 instance.
newtype Ipv6Address = Ipv6Address String
derive instance newtypeIpv6Address :: Newtype Ipv6Address _
derive instance repGenericIpv6Address :: Generic Ipv6Address _
instance showIpv6Address :: Show Ipv6Address where
  show = genericShow
instance decodeIpv6Address :: Decode Ipv6Address where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpv6Address :: Encode Ipv6Address where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of EC2 instance IPv6 address information.
newtype Ipv6Addresses = Ipv6Addresses (Array Ipv6Address)
derive instance newtypeIpv6Addresses :: Newtype Ipv6Addresses _
derive instance repGenericIpv6Addresses :: Generic Ipv6Addresses _
instance showIpv6Addresses :: Show Ipv6Addresses where
  show = genericShow
instance decodeIpv6Addresses :: Decode Ipv6Addresses where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpv6Addresses :: Encode Ipv6Addresses where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDetectorsRequest = ListDetectorsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListDetectorsRequest :: Newtype ListDetectorsRequest _
derive instance repGenericListDetectorsRequest :: Generic ListDetectorsRequest _
instance showListDetectorsRequest :: Show ListDetectorsRequest where
  show = genericShow
instance decodeListDetectorsRequest :: Decode ListDetectorsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDetectorsRequest :: Encode ListDetectorsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDetectorsResponse = ListDetectorsResponse 
  { "DetectorIds" :: NullOrUndefined.NullOrUndefined (DetectorIds)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDetectorsResponse :: Newtype ListDetectorsResponse _
derive instance repGenericListDetectorsResponse :: Generic ListDetectorsResponse _
instance showListDetectorsResponse :: Show ListDetectorsResponse where
  show = genericShow
instance decodeListDetectorsResponse :: Decode ListDetectorsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDetectorsResponse :: Encode ListDetectorsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | ListFindings request body.
newtype ListFindingsRequest = ListFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingCriteria" :: NullOrUndefined.NullOrUndefined (FindingCriteria)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "SortCriteria" :: NullOrUndefined.NullOrUndefined (SortCriteria)
  }
derive instance newtypeListFindingsRequest :: Newtype ListFindingsRequest _
derive instance repGenericListFindingsRequest :: Generic ListFindingsRequest _
instance showListFindingsRequest :: Show ListFindingsRequest where
  show = genericShow
instance decodeListFindingsRequest :: Decode ListFindingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFindingsRequest :: Encode ListFindingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListFindingsResponse = ListFindingsResponse 
  { "FindingIds" :: NullOrUndefined.NullOrUndefined (FindingIds)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListFindingsResponse :: Newtype ListFindingsResponse _
derive instance repGenericListFindingsResponse :: Generic ListFindingsResponse _
instance showListFindingsResponse :: Show ListFindingsResponse where
  show = genericShow
instance decodeListFindingsResponse :: Decode ListFindingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListFindingsResponse :: Encode ListFindingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIPSetsRequest = ListIPSetsRequest 
  { "DetectorId" :: (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListIPSetsRequest :: Newtype ListIPSetsRequest _
derive instance repGenericListIPSetsRequest :: Generic ListIPSetsRequest _
instance showListIPSetsRequest :: Show ListIPSetsRequest where
  show = genericShow
instance decodeListIPSetsRequest :: Decode ListIPSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIPSetsRequest :: Encode ListIPSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIPSetsResponse = ListIPSetsResponse 
  { "IpSetIds" :: NullOrUndefined.NullOrUndefined (IpSetIds)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListIPSetsResponse :: Newtype ListIPSetsResponse _
derive instance repGenericListIPSetsResponse :: Generic ListIPSetsResponse _
instance showListIPSetsResponse :: Show ListIPSetsResponse where
  show = genericShow
instance decodeListIPSetsResponse :: Decode ListIPSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIPSetsResponse :: Encode ListIPSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListInvitationsRequest = ListInvitationsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListInvitationsRequest :: Newtype ListInvitationsRequest _
derive instance repGenericListInvitationsRequest :: Generic ListInvitationsRequest _
instance showListInvitationsRequest :: Show ListInvitationsRequest where
  show = genericShow
instance decodeListInvitationsRequest :: Decode ListInvitationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInvitationsRequest :: Encode ListInvitationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListInvitationsResponse = ListInvitationsResponse 
  { "Invitations" :: NullOrUndefined.NullOrUndefined (Invitations)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListInvitationsResponse :: Newtype ListInvitationsResponse _
derive instance repGenericListInvitationsResponse :: Generic ListInvitationsResponse _
instance showListInvitationsResponse :: Show ListInvitationsResponse where
  show = genericShow
instance decodeListInvitationsResponse :: Decode ListInvitationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInvitationsResponse :: Encode ListInvitationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListMembersRequest = ListMembersRequest 
  { "DetectorId" :: (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "OnlyAssociated" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListMembersRequest :: Newtype ListMembersRequest _
derive instance repGenericListMembersRequest :: Generic ListMembersRequest _
instance showListMembersRequest :: Show ListMembersRequest where
  show = genericShow
instance decodeListMembersRequest :: Decode ListMembersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListMembersRequest :: Encode ListMembersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListMembersResponse = ListMembersResponse 
  { "Members" :: NullOrUndefined.NullOrUndefined (Members)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListMembersResponse :: Newtype ListMembersResponse _
derive instance repGenericListMembersResponse :: Generic ListMembersResponse _
instance showListMembersResponse :: Show ListMembersResponse where
  show = genericShow
instance decodeListMembersResponse :: Decode ListMembersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListMembersResponse :: Encode ListMembersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfPortProbeDetail = ListOfPortProbeDetail (Array PortProbeDetail)
derive instance newtypeListOfPortProbeDetail :: Newtype ListOfPortProbeDetail _
derive instance repGenericListOfPortProbeDetail :: Generic ListOfPortProbeDetail _
instance showListOfPortProbeDetail :: Show ListOfPortProbeDetail where
  show = genericShow
instance decodeListOfPortProbeDetail :: Decode ListOfPortProbeDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfPortProbeDetail :: Encode ListOfPortProbeDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThreatIntelSetsRequest = ListThreatIntelSetsRequest 
  { "DetectorId" :: (String)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListThreatIntelSetsRequest :: Newtype ListThreatIntelSetsRequest _
derive instance repGenericListThreatIntelSetsRequest :: Generic ListThreatIntelSetsRequest _
instance showListThreatIntelSetsRequest :: Show ListThreatIntelSetsRequest where
  show = genericShow
instance decodeListThreatIntelSetsRequest :: Decode ListThreatIntelSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThreatIntelSetsRequest :: Encode ListThreatIntelSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListThreatIntelSetsResponse = ListThreatIntelSetsResponse 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "ThreatIntelSetIds" :: NullOrUndefined.NullOrUndefined (ThreatIntelSetIds)
  }
derive instance newtypeListThreatIntelSetsResponse :: Newtype ListThreatIntelSetsResponse _
derive instance repGenericListThreatIntelSetsResponse :: Generic ListThreatIntelSetsResponse _
instance showListThreatIntelSetsResponse :: Show ListThreatIntelSetsResponse where
  show = genericShow
instance decodeListThreatIntelSetsResponse :: Decode ListThreatIntelSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListThreatIntelSetsResponse :: Encode ListThreatIntelSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Local port information of the connection.
newtype LocalPortDetails = LocalPortDetails 
  { "Port" :: NullOrUndefined.NullOrUndefined (Int)
  , "PortName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLocalPortDetails :: Newtype LocalPortDetails _
derive instance repGenericLocalPortDetails :: Generic LocalPortDetails _
instance showLocalPortDetails :: Show LocalPortDetails where
  show = genericShow
instance decodeLocalPortDetails :: Decode LocalPortDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocalPortDetails :: Encode LocalPortDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The location of the S3 bucket where the list resides. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key)
newtype Location = Location String
derive instance newtypeLocation :: Newtype Location _
derive instance repGenericLocation :: Generic Location _
instance showLocation :: Show Location where
  show = genericShow
instance decodeLocation :: Decode Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocation :: Encode Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfCondition = MapOfCondition (StrMap.StrMap Condition)
derive instance newtypeMapOfCondition :: Newtype MapOfCondition _
derive instance repGenericMapOfCondition :: Generic MapOfCondition _
instance showMapOfCondition :: Show MapOfCondition where
  show = genericShow
instance decodeMapOfCondition :: Decode MapOfCondition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfCondition :: Encode MapOfCondition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfCountBySeverityFindingStatistic = MapOfCountBySeverityFindingStatistic (StrMap.StrMap CountBySeverityFindingStatistic)
derive instance newtypeMapOfCountBySeverityFindingStatistic :: Newtype MapOfCountBySeverityFindingStatistic _
derive instance repGenericMapOfCountBySeverityFindingStatistic :: Generic MapOfCountBySeverityFindingStatistic _
instance showMapOfCountBySeverityFindingStatistic :: Show MapOfCountBySeverityFindingStatistic where
  show = genericShow
instance decodeMapOfCountBySeverityFindingStatistic :: Decode MapOfCountBySeverityFindingStatistic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfCountBySeverityFindingStatistic :: Encode MapOfCountBySeverityFindingStatistic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Contains details about the master account.
newtype Master = Master 
  { "AccountId" :: NullOrUndefined.NullOrUndefined (String)
  , "InvitationId" :: NullOrUndefined.NullOrUndefined (InvitationId)
  , "InvitedAt" :: NullOrUndefined.NullOrUndefined (InvitedAt)
  , "RelationshipStatus" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMaster :: Newtype Master _
derive instance repGenericMaster :: Generic Master _
instance showMaster :: Show Master where
  show = genericShow
instance decodeMaster :: Decode Master where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaster :: Encode Master where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The master account ID.
newtype MasterId = MasterId String
derive instance newtypeMasterId :: Newtype MasterId _
derive instance repGenericMasterId :: Generic MasterId _
instance showMasterId :: Show MasterId where
  show = genericShow
instance decodeMasterId :: Decode MasterId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMasterId :: Encode MasterId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Contains details about the member account.
newtype Member = Member 
  { "AccountId" :: NullOrUndefined.NullOrUndefined (AccountId)
  , "DetectorId" :: NullOrUndefined.NullOrUndefined (DetectorId)
  , "Email" :: NullOrUndefined.NullOrUndefined (Email)
  , "InvitedAt" :: NullOrUndefined.NullOrUndefined (InvitedAt)
  , "MasterId" :: NullOrUndefined.NullOrUndefined (MasterId)
  , "RelationshipStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "UpdatedAt" :: NullOrUndefined.NullOrUndefined (UpdatedAt)
  }
derive instance newtypeMember :: Newtype Member _
derive instance repGenericMember :: Generic Member _
instance showMember :: Show Member where
  show = genericShow
instance decodeMember :: Decode Member where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMember :: Encode Member where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of member descriptions.
newtype Members = Members (Array Member)
derive instance newtypeMembers :: Newtype Members _
derive instance repGenericMembers :: Generic Members _
instance showMembers :: Show Members where
  show = genericShow
instance decodeMembers :: Decode Members where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMembers :: Encode Members where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The invitation message that you want to send to the accounts that you're inviting to GuardDuty as members.
newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The user-friendly name to identify the list.
newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive instance repGenericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow
instance decodeName :: Decode Name where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeName :: Encode Name where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Represents the not equal condition to be applied to a single field when querying for findings.
newtype Neq = Neq (Array String)
derive instance newtypeNeq :: Newtype Neq _
derive instance repGenericNeq :: Generic Neq _
instance showNeq :: Show Neq where
  show = genericShow
instance decodeNeq :: Decode Neq where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNeq :: Encode Neq where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information about the NETWORK_CONNECTION action described in this finding.
newtype NetworkConnectionAction = NetworkConnectionAction 
  { "Blocked" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ConnectionDirection" :: NullOrUndefined.NullOrUndefined (String)
  , "LocalPortDetails" :: NullOrUndefined.NullOrUndefined (LocalPortDetails)
  , "Protocol" :: NullOrUndefined.NullOrUndefined (String)
  , "RemoteIpDetails" :: NullOrUndefined.NullOrUndefined (RemoteIpDetails)
  , "RemotePortDetails" :: NullOrUndefined.NullOrUndefined (RemotePortDetails)
  }
derive instance newtypeNetworkConnectionAction :: Newtype NetworkConnectionAction _
derive instance repGenericNetworkConnectionAction :: Generic NetworkConnectionAction _
instance showNetworkConnectionAction :: Show NetworkConnectionAction where
  show = genericShow
instance decodeNetworkConnectionAction :: Decode NetworkConnectionAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkConnectionAction :: Encode NetworkConnectionAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The network interface information of the EC2 instance.
newtype NetworkInterface = NetworkInterface 
  { "Ipv6Addresses" :: NullOrUndefined.NullOrUndefined (Ipv6Addresses)
  , "PrivateDnsName" :: NullOrUndefined.NullOrUndefined (PrivateDnsName)
  , "PrivateIpAddress" :: NullOrUndefined.NullOrUndefined (PrivateIpAddress)
  , "PrivateIpAddresses" :: NullOrUndefined.NullOrUndefined (PrivateIpAddresses)
  , "PublicDnsName" :: NullOrUndefined.NullOrUndefined (String)
  , "PublicIp" :: NullOrUndefined.NullOrUndefined (String)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroups)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNetworkInterface :: Newtype NetworkInterface _
derive instance repGenericNetworkInterface :: Generic NetworkInterface _
instance showNetworkInterface :: Show NetworkInterface where
  show = genericShow
instance decodeNetworkInterface :: Decode NetworkInterface where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInterface :: Encode NetworkInterface where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The network interface information of the EC2 instance.
newtype NetworkInterfaces = NetworkInterfaces (Array NetworkInterface)
derive instance newtypeNetworkInterfaces :: Newtype NetworkInterfaces _
derive instance repGenericNetworkInterfaces :: Generic NetworkInterfaces _
instance showNetworkInterfaces :: Show NetworkInterfaces where
  show = genericShow
instance decodeNetworkInterfaces :: Decode NetworkInterfaces where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInterfaces :: Encode NetworkInterfaces where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderBy = OrderBy String
derive instance newtypeOrderBy :: Newtype OrderBy _
derive instance repGenericOrderBy :: Generic OrderBy _
instance showOrderBy :: Show OrderBy where
  show = genericShow
instance decodeOrderBy :: Decode OrderBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderBy :: Encode OrderBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | ISP Organization information of the remote IP address.
newtype Organization = Organization 
  { "Asn" :: NullOrUndefined.NullOrUndefined (String)
  , "AsnOrg" :: NullOrUndefined.NullOrUndefined (String)
  , "Isp" :: NullOrUndefined.NullOrUndefined (String)
  , "Org" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeOrganization :: Newtype Organization _
derive instance repGenericOrganization :: Generic Organization _
instance showOrganization :: Show Organization where
  show = genericShow
instance decodeOrganization :: Decode Organization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrganization :: Encode Organization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Information about the PORT_PROBE action described in this finding.
newtype PortProbeAction = PortProbeAction 
  { "Blocked" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PortProbeDetails" :: NullOrUndefined.NullOrUndefined (ListOfPortProbeDetail)
  }
derive instance newtypePortProbeAction :: Newtype PortProbeAction _
derive instance repGenericPortProbeAction :: Generic PortProbeAction _
instance showPortProbeAction :: Show PortProbeAction where
  show = genericShow
instance decodePortProbeAction :: Decode PortProbeAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortProbeAction :: Encode PortProbeAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Details about the port probe finding.
newtype PortProbeDetail = PortProbeDetail 
  { "LocalPortDetails" :: NullOrUndefined.NullOrUndefined (LocalPortDetails)
  , "RemoteIpDetails" :: NullOrUndefined.NullOrUndefined (RemoteIpDetails)
  }
derive instance newtypePortProbeDetail :: Newtype PortProbeDetail _
derive instance repGenericPortProbeDetail :: Generic PortProbeDetail _
instance showPortProbeDetail :: Show PortProbeDetail where
  show = genericShow
instance decodePortProbeDetail :: Decode PortProbeDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortProbeDetail :: Encode PortProbeDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Private DNS name of the EC2 instance.
newtype PrivateDnsName = PrivateDnsName String
derive instance newtypePrivateDnsName :: Newtype PrivateDnsName _
derive instance repGenericPrivateDnsName :: Generic PrivateDnsName _
instance showPrivateDnsName :: Show PrivateDnsName where
  show = genericShow
instance decodePrivateDnsName :: Decode PrivateDnsName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrivateDnsName :: Encode PrivateDnsName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Private IP address of the EC2 instance.
newtype PrivateIpAddress = PrivateIpAddress String
derive instance newtypePrivateIpAddress :: Newtype PrivateIpAddress _
derive instance repGenericPrivateIpAddress :: Generic PrivateIpAddress _
instance showPrivateIpAddress :: Show PrivateIpAddress where
  show = genericShow
instance decodePrivateIpAddress :: Decode PrivateIpAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrivateIpAddress :: Encode PrivateIpAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Other private IP address information of the EC2 instance.
newtype PrivateIpAddressDetails = PrivateIpAddressDetails 
  { "PrivateDnsName" :: NullOrUndefined.NullOrUndefined (PrivateDnsName)
  , "PrivateIpAddress" :: NullOrUndefined.NullOrUndefined (PrivateIpAddress)
  }
derive instance newtypePrivateIpAddressDetails :: Newtype PrivateIpAddressDetails _
derive instance repGenericPrivateIpAddressDetails :: Generic PrivateIpAddressDetails _
instance showPrivateIpAddressDetails :: Show PrivateIpAddressDetails where
  show = genericShow
instance decodePrivateIpAddressDetails :: Decode PrivateIpAddressDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrivateIpAddressDetails :: Encode PrivateIpAddressDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Other private IP address information of the EC2 instance.
newtype PrivateIpAddresses = PrivateIpAddresses (Array PrivateIpAddressDetails)
derive instance newtypePrivateIpAddresses :: Newtype PrivateIpAddresses _
derive instance repGenericPrivateIpAddresses :: Generic PrivateIpAddresses _
instance showPrivateIpAddresses :: Show PrivateIpAddresses where
  show = genericShow
instance decodePrivateIpAddresses :: Decode PrivateIpAddresses where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrivateIpAddresses :: Encode PrivateIpAddresses where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The product code of the EC2 instance.
newtype ProductCode = ProductCode 
  { "Code" :: NullOrUndefined.NullOrUndefined (String)
  , "ProductType" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeProductCode :: Newtype ProductCode _
derive instance repGenericProductCode :: Generic ProductCode _
instance showProductCode :: Show ProductCode where
  show = genericShow
instance decodeProductCode :: Decode ProductCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProductCode :: Encode ProductCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The product code of the EC2 instance.
newtype ProductCodes = ProductCodes (Array ProductCode)
derive instance newtypeProductCodes :: Newtype ProductCodes _
derive instance repGenericProductCodes :: Generic ProductCodes _
instance showProductCodes :: Show ProductCodes where
  show = genericShow
instance decodeProductCodes :: Decode ProductCodes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProductCodes :: Encode ProductCodes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Remote IP information of the connection.
newtype RemoteIpDetails = RemoteIpDetails 
  { "City" :: NullOrUndefined.NullOrUndefined (City)
  , "Country" :: NullOrUndefined.NullOrUndefined (Country)
  , "GeoLocation" :: NullOrUndefined.NullOrUndefined (GeoLocation)
  , "IpAddressV4" :: NullOrUndefined.NullOrUndefined (String)
  , "Organization" :: NullOrUndefined.NullOrUndefined (Organization)
  }
derive instance newtypeRemoteIpDetails :: Newtype RemoteIpDetails _
derive instance repGenericRemoteIpDetails :: Generic RemoteIpDetails _
instance showRemoteIpDetails :: Show RemoteIpDetails where
  show = genericShow
instance decodeRemoteIpDetails :: Decode RemoteIpDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoteIpDetails :: Encode RemoteIpDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Remote port information of the connection.
newtype RemotePortDetails = RemotePortDetails 
  { "Port" :: NullOrUndefined.NullOrUndefined (Int)
  , "PortName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRemotePortDetails :: Newtype RemotePortDetails _
derive instance repGenericRemotePortDetails :: Generic RemotePortDetails _
instance showRemotePortDetails :: Show RemotePortDetails where
  show = genericShow
instance decodeRemotePortDetails :: Decode RemotePortDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemotePortDetails :: Encode RemotePortDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The AWS resource associated with the activity that prompted GuardDuty to generate a finding.
newtype Resource = Resource 
  { "AccessKeyDetails" :: NullOrUndefined.NullOrUndefined (AccessKeyDetails)
  , "InstanceDetails" :: NullOrUndefined.NullOrUndefined (InstanceDetails)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResource :: Newtype Resource _
derive instance repGenericResource :: Generic Resource _
instance showResource :: Show Resource where
  show = genericShow
instance decodeResource :: Decode Resource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResource :: Encode Resource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Security groups associated with the EC2 instance.
newtype SecurityGroup = SecurityGroup 
  { "GroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "GroupName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSecurityGroup :: Newtype SecurityGroup _
derive instance repGenericSecurityGroup :: Generic SecurityGroup _
instance showSecurityGroup :: Show SecurityGroup where
  show = genericShow
instance decodeSecurityGroup :: Decode SecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroup :: Encode SecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Security groups associated with the EC2 instance.
newtype SecurityGroups = SecurityGroups (Array SecurityGroup)
derive instance newtypeSecurityGroups :: Newtype SecurityGroups _
derive instance repGenericSecurityGroups :: Generic SecurityGroups _
instance showSecurityGroups :: Show SecurityGroups where
  show = genericShow
instance decodeSecurityGroups :: Decode SecurityGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroups :: Encode SecurityGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Additional information assigned to the generated finding by GuardDuty.
newtype Service = Service 
  { "Action" :: NullOrUndefined.NullOrUndefined (Action)
  , "Archived" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Count" :: NullOrUndefined.NullOrUndefined (Int)
  , "DetectorId" :: NullOrUndefined.NullOrUndefined (DetectorId)
  , "EventFirstSeen" :: NullOrUndefined.NullOrUndefined (String)
  , "EventLastSeen" :: NullOrUndefined.NullOrUndefined (String)
  , "ResourceRole" :: NullOrUndefined.NullOrUndefined (String)
  , "ServiceName" :: NullOrUndefined.NullOrUndefined (String)
  , "UserFeedback" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeService :: Newtype Service _
derive instance repGenericService :: Generic Service _
instance showService :: Show Service where
  show = genericShow
instance decodeService :: Decode Service where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeService :: Encode Service where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Customer serviceRole name or ARN for accessing customer resources
newtype ServiceRole = ServiceRole String
derive instance newtypeServiceRole :: Newtype ServiceRole _
derive instance repGenericServiceRole :: Generic ServiceRole _
instance showServiceRole :: Show ServiceRole where
  show = genericShow
instance decodeServiceRole :: Decode ServiceRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceRole :: Encode ServiceRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | Represents the criteria used for sorting findings.
newtype SortCriteria = SortCriteria 
  { "AttributeName" :: NullOrUndefined.NullOrUndefined (String)
  , "OrderBy" :: NullOrUndefined.NullOrUndefined (OrderBy)
  }
derive instance newtypeSortCriteria :: Newtype SortCriteria _
derive instance repGenericSortCriteria :: Generic SortCriteria _
instance showSortCriteria :: Show SortCriteria where
  show = genericShow
instance decodeSortCriteria :: Decode SortCriteria where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSortCriteria :: Encode SortCriteria where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | StartMonitoringMembers request body.
newtype StartMonitoringMembersRequest = StartMonitoringMembersRequest 
  { "AccountIds" :: NullOrUndefined.NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeStartMonitoringMembersRequest :: Newtype StartMonitoringMembersRequest _
derive instance repGenericStartMonitoringMembersRequest :: Generic StartMonitoringMembersRequest _
instance showStartMonitoringMembersRequest :: Show StartMonitoringMembersRequest where
  show = genericShow
instance decodeStartMonitoringMembersRequest :: Decode StartMonitoringMembersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartMonitoringMembersRequest :: Encode StartMonitoringMembersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartMonitoringMembersResponse = StartMonitoringMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined.NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeStartMonitoringMembersResponse :: Newtype StartMonitoringMembersResponse _
derive instance repGenericStartMonitoringMembersResponse :: Generic StartMonitoringMembersResponse _
instance showStartMonitoringMembersResponse :: Show StartMonitoringMembersResponse where
  show = genericShow
instance decodeStartMonitoringMembersResponse :: Decode StartMonitoringMembersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartMonitoringMembersResponse :: Encode StartMonitoringMembersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | StopMonitoringMembers request body.
newtype StopMonitoringMembersRequest = StopMonitoringMembersRequest 
  { "AccountIds" :: NullOrUndefined.NullOrUndefined (AccountIds)
  , "DetectorId" :: (String)
  }
derive instance newtypeStopMonitoringMembersRequest :: Newtype StopMonitoringMembersRequest _
derive instance repGenericStopMonitoringMembersRequest :: Generic StopMonitoringMembersRequest _
instance showStopMonitoringMembersRequest :: Show StopMonitoringMembersRequest where
  show = genericShow
instance decodeStopMonitoringMembersRequest :: Decode StopMonitoringMembersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopMonitoringMembersRequest :: Encode StopMonitoringMembersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopMonitoringMembersResponse = StopMonitoringMembersResponse 
  { "UnprocessedAccounts" :: NullOrUndefined.NullOrUndefined (UnprocessedAccounts)
  }
derive instance newtypeStopMonitoringMembersResponse :: Newtype StopMonitoringMembersResponse _
derive instance repGenericStopMonitoringMembersResponse :: Generic StopMonitoringMembersResponse _
instance showStopMonitoringMembersResponse :: Show StopMonitoringMembersResponse where
  show = genericShow
instance decodeStopMonitoringMembersResponse :: Decode StopMonitoringMembersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopMonitoringMembersResponse :: Encode StopMonitoringMembersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A tag of the EC2 instance.
newtype Tag = Tag 
  { "Key" :: NullOrUndefined.NullOrUndefined (String)
  , "Value" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The tags of the EC2 instance.
newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The format of the threatIntelSet.
newtype ThreatIntelSetFormat = ThreatIntelSetFormat String
derive instance newtypeThreatIntelSetFormat :: Newtype ThreatIntelSetFormat _
derive instance repGenericThreatIntelSetFormat :: Generic ThreatIntelSetFormat _
instance showThreatIntelSetFormat :: Show ThreatIntelSetFormat where
  show = genericShow
instance decodeThreatIntelSetFormat :: Decode ThreatIntelSetFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThreatIntelSetFormat :: Encode ThreatIntelSetFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The unique identifier for an threat intel set
newtype ThreatIntelSetId = ThreatIntelSetId String
derive instance newtypeThreatIntelSetId :: Newtype ThreatIntelSetId _
derive instance repGenericThreatIntelSetId :: Generic ThreatIntelSetId _
instance showThreatIntelSetId :: Show ThreatIntelSetId where
  show = genericShow
instance decodeThreatIntelSetId :: Decode ThreatIntelSetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThreatIntelSetId :: Encode ThreatIntelSetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The list of the threat intel set IDs
newtype ThreatIntelSetIds = ThreatIntelSetIds (Array ThreatIntelSetId)
derive instance newtypeThreatIntelSetIds :: Newtype ThreatIntelSetIds _
derive instance repGenericThreatIntelSetIds :: Generic ThreatIntelSetIds _
instance showThreatIntelSetIds :: Show ThreatIntelSetIds where
  show = genericShow
instance decodeThreatIntelSetIds :: Decode ThreatIntelSetIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThreatIntelSetIds :: Encode ThreatIntelSetIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The status of threatIntelSet file uploaded.
newtype ThreatIntelSetStatus = ThreatIntelSetStatus String
derive instance newtypeThreatIntelSetStatus :: Newtype ThreatIntelSetStatus _
derive instance repGenericThreatIntelSetStatus :: Generic ThreatIntelSetStatus _
instance showThreatIntelSetStatus :: Show ThreatIntelSetStatus where
  show = genericShow
instance decodeThreatIntelSetStatus :: Decode ThreatIntelSetStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThreatIntelSetStatus :: Encode ThreatIntelSetStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | UnarchiveFindings request body.
newtype UnarchiveFindingsRequest = UnarchiveFindingsRequest 
  { "DetectorId" :: (String)
  , "FindingIds" :: NullOrUndefined.NullOrUndefined (FindingIds)
  }
derive instance newtypeUnarchiveFindingsRequest :: Newtype UnarchiveFindingsRequest _
derive instance repGenericUnarchiveFindingsRequest :: Generic UnarchiveFindingsRequest _
instance showUnarchiveFindingsRequest :: Show UnarchiveFindingsRequest where
  show = genericShow
instance decodeUnarchiveFindingsRequest :: Decode UnarchiveFindingsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnarchiveFindingsRequest :: Encode UnarchiveFindingsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UnarchiveFindingsResponse = UnarchiveFindingsResponse Types.NoArguments
derive instance newtypeUnarchiveFindingsResponse :: Newtype UnarchiveFindingsResponse _
derive instance repGenericUnarchiveFindingsResponse :: Generic UnarchiveFindingsResponse _
instance showUnarchiveFindingsResponse :: Show UnarchiveFindingsResponse where
  show = genericShow
instance decodeUnarchiveFindingsResponse :: Decode UnarchiveFindingsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnarchiveFindingsResponse :: Encode UnarchiveFindingsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | An object containing the unprocessed account and a result string explaining why it was unprocessed.
newtype UnprocessedAccount = UnprocessedAccount 
  { "AccountId" :: NullOrUndefined.NullOrUndefined (String)
  , "Result" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnprocessedAccount :: Newtype UnprocessedAccount _
derive instance repGenericUnprocessedAccount :: Generic UnprocessedAccount _
instance showUnprocessedAccount :: Show UnprocessedAccount where
  show = genericShow
instance decodeUnprocessedAccount :: Decode UnprocessedAccount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnprocessedAccount :: Encode UnprocessedAccount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
newtype UnprocessedAccounts = UnprocessedAccounts (Array UnprocessedAccount)
derive instance newtypeUnprocessedAccounts :: Newtype UnprocessedAccounts _
derive instance repGenericUnprocessedAccounts :: Generic UnprocessedAccounts _
instance showUnprocessedAccounts :: Show UnprocessedAccounts where
  show = genericShow
instance decodeUnprocessedAccounts :: Decode UnprocessedAccounts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnprocessedAccounts :: Encode UnprocessedAccounts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | UpdateDetector request body.
newtype UpdateDetectorRequest = UpdateDetectorRequest 
  { "DetectorId" :: (String)
  , "Enable" :: NullOrUndefined.NullOrUndefined (Enable)
  }
derive instance newtypeUpdateDetectorRequest :: Newtype UpdateDetectorRequest _
derive instance repGenericUpdateDetectorRequest :: Generic UpdateDetectorRequest _
instance showUpdateDetectorRequest :: Show UpdateDetectorRequest where
  show = genericShow
instance decodeUpdateDetectorRequest :: Decode UpdateDetectorRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDetectorRequest :: Encode UpdateDetectorRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDetectorResponse = UpdateDetectorResponse Types.NoArguments
derive instance newtypeUpdateDetectorResponse :: Newtype UpdateDetectorResponse _
derive instance repGenericUpdateDetectorResponse :: Generic UpdateDetectorResponse _
instance showUpdateDetectorResponse :: Show UpdateDetectorResponse where
  show = genericShow
instance decodeUpdateDetectorResponse :: Decode UpdateDetectorResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDetectorResponse :: Encode UpdateDetectorResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | UpdateFindingsFeedback request body.
newtype UpdateFindingsFeedbackRequest = UpdateFindingsFeedbackRequest 
  { "Comments" :: NullOrUndefined.NullOrUndefined (Comments)
  , "DetectorId" :: (String)
  , "Feedback" :: NullOrUndefined.NullOrUndefined (Feedback)
  , "FindingIds" :: NullOrUndefined.NullOrUndefined (FindingIds)
  }
derive instance newtypeUpdateFindingsFeedbackRequest :: Newtype UpdateFindingsFeedbackRequest _
derive instance repGenericUpdateFindingsFeedbackRequest :: Generic UpdateFindingsFeedbackRequest _
instance showUpdateFindingsFeedbackRequest :: Show UpdateFindingsFeedbackRequest where
  show = genericShow
instance decodeUpdateFindingsFeedbackRequest :: Decode UpdateFindingsFeedbackRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFindingsFeedbackRequest :: Encode UpdateFindingsFeedbackRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateFindingsFeedbackResponse = UpdateFindingsFeedbackResponse Types.NoArguments
derive instance newtypeUpdateFindingsFeedbackResponse :: Newtype UpdateFindingsFeedbackResponse _
derive instance repGenericUpdateFindingsFeedbackResponse :: Generic UpdateFindingsFeedbackResponse _
instance showUpdateFindingsFeedbackResponse :: Show UpdateFindingsFeedbackResponse where
  show = genericShow
instance decodeUpdateFindingsFeedbackResponse :: Decode UpdateFindingsFeedbackResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFindingsFeedbackResponse :: Encode UpdateFindingsFeedbackResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | UpdateIPSet request body.
newtype UpdateIPSetRequest = UpdateIPSetRequest 
  { "Activate" :: NullOrUndefined.NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "IpSetId" :: (String)
  , "Location" :: NullOrUndefined.NullOrUndefined (Location)
  , "Name" :: NullOrUndefined.NullOrUndefined (Name)
  }
derive instance newtypeUpdateIPSetRequest :: Newtype UpdateIPSetRequest _
derive instance repGenericUpdateIPSetRequest :: Generic UpdateIPSetRequest _
instance showUpdateIPSetRequest :: Show UpdateIPSetRequest where
  show = genericShow
instance decodeUpdateIPSetRequest :: Decode UpdateIPSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIPSetRequest :: Encode UpdateIPSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateIPSetResponse = UpdateIPSetResponse Types.NoArguments
derive instance newtypeUpdateIPSetResponse :: Newtype UpdateIPSetResponse _
derive instance repGenericUpdateIPSetResponse :: Generic UpdateIPSetResponse _
instance showUpdateIPSetResponse :: Show UpdateIPSetResponse where
  show = genericShow
instance decodeUpdateIPSetResponse :: Decode UpdateIPSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateIPSetResponse :: Encode UpdateIPSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | UpdateThreatIntelSet request body.
newtype UpdateThreatIntelSetRequest = UpdateThreatIntelSetRequest 
  { "Activate" :: NullOrUndefined.NullOrUndefined (Activate)
  , "DetectorId" :: (String)
  , "Location" :: NullOrUndefined.NullOrUndefined (Location)
  , "Name" :: NullOrUndefined.NullOrUndefined (Name)
  , "ThreatIntelSetId" :: (String)
  }
derive instance newtypeUpdateThreatIntelSetRequest :: Newtype UpdateThreatIntelSetRequest _
derive instance repGenericUpdateThreatIntelSetRequest :: Generic UpdateThreatIntelSetRequest _
instance showUpdateThreatIntelSetRequest :: Show UpdateThreatIntelSetRequest where
  show = genericShow
instance decodeUpdateThreatIntelSetRequest :: Decode UpdateThreatIntelSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateThreatIntelSetRequest :: Encode UpdateThreatIntelSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateThreatIntelSetResponse = UpdateThreatIntelSetResponse Types.NoArguments
derive instance newtypeUpdateThreatIntelSetResponse :: Newtype UpdateThreatIntelSetResponse _
derive instance repGenericUpdateThreatIntelSetResponse :: Generic UpdateThreatIntelSetResponse _
instance showUpdateThreatIntelSetResponse :: Show UpdateThreatIntelSetResponse where
  show = genericShow
instance decodeUpdateThreatIntelSetResponse :: Decode UpdateThreatIntelSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateThreatIntelSetResponse :: Encode UpdateThreatIntelSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | The first time a resource was created. The format will be ISO-8601.
newtype UpdatedAt = UpdatedAt String
derive instance newtypeUpdatedAt :: Newtype UpdatedAt _
derive instance repGenericUpdatedAt :: Generic UpdatedAt _
instance showUpdatedAt :: Show UpdatedAt where
  show = genericShow
instance decodeUpdatedAt :: Decode UpdatedAt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatedAt :: Encode UpdatedAt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
