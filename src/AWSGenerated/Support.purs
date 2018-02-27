

-- | <fullname>AWS Support</fullname> <p>The AWS Support API reference is intended for programmers who need detailed information about the AWS Support operations and data types. This service enables you to manage your AWS Support cases programmatically. It uses HTTP methods that return results in JSON format.</p> <p>The AWS Support service also exposes a set of <a href="http://aws.amazon.com/premiumsupport/trustedadvisor/">Trusted Advisor</a> features. You can retrieve a list of checks and their descriptions, get check results, specify checks to refresh, and get the refresh status of checks. </p> <p>The following list describes the AWS Support case management operations:</p> <ul> <li> <p> <b>Service names, issue categories, and available severity levels. </b>The <a>DescribeServices</a> and <a>DescribeSeverityLevels</a> operations return AWS service names, service codes, service categories, and problem severity levels. You use these values when you call the <a>CreateCase</a> operation. </p> </li> <li> <p> <b>Case creation, case details, and case resolution.</b> The <a>CreateCase</a>, <a>DescribeCases</a>, <a>DescribeAttachment</a>, and <a>ResolveCase</a> operations create AWS Support cases, retrieve information about cases, and resolve cases.</p> </li> <li> <p> <b>Case communication.</b> The <a>DescribeCommunications</a>, <a>AddCommunicationToCase</a>, and <a>AddAttachmentsToSet</a> operations retrieve and add communications and attachments to AWS Support cases. </p> </li> </ul> <p>The following list describes the operations available from the AWS Support service for Trusted Advisor:</p> <ul> <li> <p> <a>DescribeTrustedAdvisorChecks</a> returns the list of checks that run against your AWS resources.</p> </li> <li> <p>Using the <code>checkId</code> for a specific check returned by <a>DescribeTrustedAdvisorChecks</a>, you can call <a>DescribeTrustedAdvisorCheckResult</a> to obtain the results for the check you specified.</p> </li> <li> <p> <a>DescribeTrustedAdvisorCheckSummaries</a> returns summarized results for one or more Trusted Advisor checks.</p> </li> <li> <p> <a>RefreshTrustedAdvisorCheck</a> requests that Trusted Advisor rerun a specified check. </p> </li> <li> <p> <a>DescribeTrustedAdvisorCheckRefreshStatuses</a> reports the refresh status of one or more checks. </p> </li> </ul> <p>For authentication of requests, AWS Support uses <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a>.</p> <p>See <a href="http://docs.aws.amazon.com/awssupport/latest/user/Welcome.html">About the AWS Support API</a> in the <i>AWS Support User Guide</i> for information about how to use this service to create and manage your support cases, and how to call Trusted Advisor for results of checks on your resources. </p>
module AWS.Support where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Support" :: String


-- | <p>Adds one or more attachments to an attachment set. If an <code>attachmentSetId</code> is not specified, a new attachment set is created, and the ID of the set is returned in the response. If an <code>attachmentSetId</code> is specified, the attachments are added to the specified set, if it exists.</p> <p>An attachment set is a temporary container for attachments that are to be added to a case or case communication. The set is available for one hour after it is created; the <code>expiryTime</code> returned in the response indicates when the set expires. The maximum number of attachments in a set is 3, and the maximum size of any attachment in the set is 5 MB.</p>
addAttachmentsToSet :: forall eff. AddAttachmentsToSetRequest -> Aff (err :: AWS.RequestError | eff) AddAttachmentsToSetResponse
addAttachmentsToSet = AWS.request serviceName "addAttachmentsToSet" 


-- | <p>Adds additional customer communication to an AWS Support case. You use the <code>caseId</code> value to identify the case to add communication to. You can list a set of email addresses to copy on the communication using the <code>ccEmailAddresses</code> value. The <code>communicationBody</code> value contains the text of the communication.</p> <p>The response indicates the success or failure of the request.</p> <p>This operation implements a subset of the features of the AWS Support Center.</p>
addCommunicationToCase :: forall eff. AddCommunicationToCaseRequest -> Aff (err :: AWS.RequestError | eff) AddCommunicationToCaseResponse
addCommunicationToCase = AWS.request serviceName "addCommunicationToCase" 


-- | <p>Creates a new case in the AWS Support Center. This operation is modeled on the behavior of the AWS Support Center <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page. Its parameters require you to specify the following information: </p> <ul> <li> <p> <b>issueType.</b> The type of issue for the case. You can specify either "customer-service" or "technical." If you do not indicate a value, the default is "technical." </p> </li> <li> <p> <b>serviceCode.</b> The code for an AWS service. You obtain the <code>serviceCode</code> by calling <a>DescribeServices</a>. </p> </li> <li> <p> <b>categoryCode.</b> The category for the service defined for the <code>serviceCode</code> value. You also obtain the category code for a service by calling <a>DescribeServices</a>. Each AWS service defines its own set of category codes. </p> </li> <li> <p> <b>severityCode.</b> A value that indicates the urgency of the case, which in turn determines the response time according to your service level agreement with AWS Support. You obtain the SeverityCode by calling <a>DescribeSeverityLevels</a>.</p> </li> <li> <p> <b>subject.</b> The <b>Subject</b> field on the AWS Support Center <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page.</p> </li> <li> <p> <b>communicationBody.</b> The <b>Description</b> field on the AWS Support Center <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page.</p> </li> <li> <p> <b>attachmentSetId.</b> The ID of a set of attachments that has been created by using <a>AddAttachmentsToSet</a>.</p> </li> <li> <p> <b>language.</b> The human language in which AWS Support handles the case. English and Japanese are currently supported.</p> </li> <li> <p> <b>ccEmailAddresses.</b> The AWS Support Center <b>CC</b> field on the <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page. You can list email addresses to be copied on any correspondence about the case. The account that opens the case is already identified by passing the AWS Credentials in the HTTP POST method or in a method or function call from one of the programming languages supported by an <a href="http://aws.amazon.com/tools/">AWS SDK</a>. </p> </li> </ul> <note> <p>To add additional communication or attachments to an existing case, use <a>AddCommunicationToCase</a>.</p> </note> <p>A successful <a>CreateCase</a> request returns an AWS Support case number. Case numbers are used by the <a>DescribeCases</a> operation to retrieve existing AWS Support cases. </p>
createCase :: forall eff. CreateCaseRequest -> Aff (err :: AWS.RequestError | eff) CreateCaseResponse
createCase = AWS.request serviceName "createCase" 


-- | <p>Returns the attachment that has the specified ID. Attachment IDs are generated by the case management system when you add an attachment to a case or case communication. Attachment IDs are returned in the <a>AttachmentDetails</a> objects that are returned by the <a>DescribeCommunications</a> operation.</p>
describeAttachment :: forall eff. DescribeAttachmentRequest -> Aff (err :: AWS.RequestError | eff) DescribeAttachmentResponse
describeAttachment = AWS.request serviceName "describeAttachment" 


-- | <p>Returns a list of cases that you specify by passing one or more case IDs. In addition, you can filter the cases by date by setting values for the <code>afterTime</code> and <code>beforeTime</code> request parameters. You can set values for the <code>includeResolvedCases</code> and <code>includeCommunications</code> request parameters to control how much information is returned. </p> <p>Case data is available for 12 months after creation. If a case was created more than 12 months ago, a request for data might cause an error.</p> <p>The response returns the following in JSON format:</p> <ul> <li> <p>One or more <a>CaseDetails</a> data types. </p> </li> <li> <p>One or more <code>nextToken</code> values, which specify where to paginate the returned records represented by the <code>CaseDetails</code> objects.</p> </li> </ul>
describeCases :: forall eff. DescribeCasesRequest -> Aff (err :: AWS.RequestError | eff) DescribeCasesResponse
describeCases = AWS.request serviceName "describeCases" 


-- | <p>Returns communications (and attachments) for one or more support cases. You can use the <code>afterTime</code> and <code>beforeTime</code> parameters to filter by date. You can use the <code>caseId</code> parameter to restrict the results to a particular case.</p> <p>Case data is available for 12 months after creation. If a case was created more than 12 months ago, a request for data might cause an error.</p> <p>You can use the <code>maxResults</code> and <code>nextToken</code> parameters to control the pagination of the result set. Set <code>maxResults</code> to the number of cases you want displayed on each page, and use <code>nextToken</code> to specify the resumption of pagination.</p>
describeCommunications :: forall eff. DescribeCommunicationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeCommunicationsResponse
describeCommunications = AWS.request serviceName "describeCommunications" 


-- | <p>Returns the current list of AWS services and a list of service categories that applies to each one. You then use service names and categories in your <a>CreateCase</a> requests. Each AWS service has its own set of categories.</p> <p>The service codes and category codes correspond to the values that are displayed in the <b>Service</b> and <b>Category</b> drop-down lists on the AWS Support Center <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page. The values in those fields, however, do not necessarily match the service codes and categories returned by the <code>DescribeServices</code> request. Always use the service codes and categories obtained programmatically. This practice ensures that you always have the most recent set of service and category codes.</p>
describeServices :: forall eff. DescribeServicesRequest -> Aff (err :: AWS.RequestError | eff) DescribeServicesResponse
describeServices = AWS.request serviceName "describeServices" 


-- | <p>Returns the list of severity levels that you can assign to an AWS Support case. The severity level for a case is also a field in the <a>CaseDetails</a> data type included in any <a>CreateCase</a> request. </p>
describeSeverityLevels :: forall eff. DescribeSeverityLevelsRequest -> Aff (err :: AWS.RequestError | eff) DescribeSeverityLevelsResponse
describeSeverityLevels = AWS.request serviceName "describeSeverityLevels" 


-- | <p>Returns the refresh status of the Trusted Advisor checks that have the specified check IDs. Check IDs can be obtained by calling <a>DescribeTrustedAdvisorChecks</a>.</p> <note> <p>Some checks are refreshed automatically, and their refresh statuses cannot be retrieved by using this operation. Use of the <code>DescribeTrustedAdvisorCheckRefreshStatuses</code> operation for these checks causes an <code>InvalidParameterValue</code> error.</p> </note>
describeTrustedAdvisorCheckRefreshStatuses :: forall eff. DescribeTrustedAdvisorCheckRefreshStatusesRequest -> Aff (err :: AWS.RequestError | eff) DescribeTrustedAdvisorCheckRefreshStatusesResponse
describeTrustedAdvisorCheckRefreshStatuses = AWS.request serviceName "describeTrustedAdvisorCheckRefreshStatuses" 


-- | <p>Returns the results of the Trusted Advisor check that has the specified check ID. Check IDs can be obtained by calling <a>DescribeTrustedAdvisorChecks</a>.</p> <p>The response contains a <a>TrustedAdvisorCheckResult</a> object, which contains these three objects:</p> <ul> <li> <p> <a>TrustedAdvisorCategorySpecificSummary</a> </p> </li> <li> <p> <a>TrustedAdvisorResourceDetail</a> </p> </li> <li> <p> <a>TrustedAdvisorResourcesSummary</a> </p> </li> </ul> <p>In addition, the response contains these fields:</p> <ul> <li> <p> <b>status.</b> The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".</p> </li> <li> <p> <b>timestamp.</b> The time of the last refresh of the check.</p> </li> <li> <p> <b>checkId.</b> The unique identifier for the check.</p> </li> </ul>
describeTrustedAdvisorCheckResult :: forall eff. DescribeTrustedAdvisorCheckResultRequest -> Aff (err :: AWS.RequestError | eff) DescribeTrustedAdvisorCheckResultResponse
describeTrustedAdvisorCheckResult = AWS.request serviceName "describeTrustedAdvisorCheckResult" 


-- | <p>Returns the summaries of the results of the Trusted Advisor checks that have the specified check IDs. Check IDs can be obtained by calling <a>DescribeTrustedAdvisorChecks</a>.</p> <p>The response contains an array of <a>TrustedAdvisorCheckSummary</a> objects.</p>
describeTrustedAdvisorCheckSummaries :: forall eff. DescribeTrustedAdvisorCheckSummariesRequest -> Aff (err :: AWS.RequestError | eff) DescribeTrustedAdvisorCheckSummariesResponse
describeTrustedAdvisorCheckSummaries = AWS.request serviceName "describeTrustedAdvisorCheckSummaries" 


-- | <p>Returns information about all available Trusted Advisor checks, including name, ID, category, description, and metadata. You must specify a language code; English ("en") and Japanese ("ja") are currently supported. The response contains a <a>TrustedAdvisorCheckDescription</a> for each check.</p>
describeTrustedAdvisorChecks :: forall eff. DescribeTrustedAdvisorChecksRequest -> Aff (err :: AWS.RequestError | eff) DescribeTrustedAdvisorChecksResponse
describeTrustedAdvisorChecks = AWS.request serviceName "describeTrustedAdvisorChecks" 


-- | <p>Requests a refresh of the Trusted Advisor check that has the specified check ID. Check IDs can be obtained by calling <a>DescribeTrustedAdvisorChecks</a>.</p> <note> <p>Some checks are refreshed automatically, and they cannot be refreshed by using this operation. Use of the <code>RefreshTrustedAdvisorCheck</code> operation for these checks causes an <code>InvalidParameterValue</code> error.</p> </note> <p>The response contains a <a>TrustedAdvisorCheckRefreshStatus</a> object, which contains these fields:</p> <ul> <li> <p> <b>status.</b> The refresh status of the check: "none", "enqueued", "processing", "success", or "abandoned".</p> </li> <li> <p> <b>millisUntilNextRefreshable.</b> The amount of time, in milliseconds, until the check is eligible for refresh.</p> </li> <li> <p> <b>checkId.</b> The unique identifier for the check.</p> </li> </ul>
refreshTrustedAdvisorCheck :: forall eff. RefreshTrustedAdvisorCheckRequest -> Aff (err :: AWS.RequestError | eff) RefreshTrustedAdvisorCheckResponse
refreshTrustedAdvisorCheck = AWS.request serviceName "refreshTrustedAdvisorCheck" 


-- | <p>Takes a <code>caseId</code> and returns the initial state of the case along with the state of the case after the call to <a>ResolveCase</a> completed.</p>
resolveCase :: forall eff. ResolveCaseRequest -> Aff (err :: AWS.RequestError | eff) ResolveCaseResponse
resolveCase = AWS.request serviceName "resolveCase" 


-- | <p/>
newtype AddAttachmentsToSetRequest = AddAttachmentsToSetRequest 
  { "AttachmentSetId'" :: NullOrUndefined (AttachmentSetId)
  , "Attachments'" :: (Attachments)
  }
derive instance newtypeAddAttachmentsToSetRequest :: Newtype AddAttachmentsToSetRequest _


-- | <p>The ID and expiry time of the attachment set returned by the <a>AddAttachmentsToSet</a> operation.</p>
newtype AddAttachmentsToSetResponse = AddAttachmentsToSetResponse 
  { "AttachmentSetId'" :: NullOrUndefined (AttachmentSetId)
  , "ExpiryTime'" :: NullOrUndefined (ExpiryTime)
  }
derive instance newtypeAddAttachmentsToSetResponse :: Newtype AddAttachmentsToSetResponse _


-- | <p>To be written.</p>
newtype AddCommunicationToCaseRequest = AddCommunicationToCaseRequest 
  { "CaseId'" :: NullOrUndefined (CaseId)
  , "CommunicationBody'" :: (CommunicationBody)
  , "CcEmailAddresses'" :: NullOrUndefined (CcEmailAddressList)
  , "AttachmentSetId'" :: NullOrUndefined (AttachmentSetId)
  }
derive instance newtypeAddCommunicationToCaseRequest :: Newtype AddCommunicationToCaseRequest _


-- | <p>The result of the <a>AddCommunicationToCase</a> operation.</p>
newtype AddCommunicationToCaseResponse = AddCommunicationToCaseResponse 
  { "Result'" :: NullOrUndefined (Result)
  }
derive instance newtypeAddCommunicationToCaseResponse :: Newtype AddCommunicationToCaseResponse _


newtype AfterTime = AfterTime String
derive instance newtypeAfterTime :: Newtype AfterTime _


-- | <p>An attachment to a case communication. The attachment consists of the file name and the content of the file.</p>
newtype Attachment = Attachment 
  { "FileName'" :: NullOrUndefined (FileName)
  , "Data'" :: NullOrUndefined (Data)
  }
derive instance newtypeAttachment :: Newtype Attachment _


-- | <p>The file name and ID of an attachment to a case communication. You can use the ID to retrieve the attachment with the <a>DescribeAttachment</a> operation.</p>
newtype AttachmentDetails = AttachmentDetails 
  { "AttachmentId'" :: NullOrUndefined (AttachmentId)
  , "FileName'" :: NullOrUndefined (FileName)
  }
derive instance newtypeAttachmentDetails :: Newtype AttachmentDetails _


newtype AttachmentId = AttachmentId String
derive instance newtypeAttachmentId :: Newtype AttachmentId _


-- | <p>An attachment with the specified ID could not be found.</p>
newtype AttachmentIdNotFound = AttachmentIdNotFound 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAttachmentIdNotFound :: Newtype AttachmentIdNotFound _


-- | <p>The limit for the number of attachment sets created in a short period of time has been exceeded.</p>
newtype AttachmentLimitExceeded = AttachmentLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAttachmentLimitExceeded :: Newtype AttachmentLimitExceeded _


newtype AttachmentSet = AttachmentSet (Array AttachmentDetails)
derive instance newtypeAttachmentSet :: Newtype AttachmentSet _


-- | <p>The expiration time of the attachment set has passed. The set expires 1 hour after it is created.</p>
newtype AttachmentSetExpired = AttachmentSetExpired 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAttachmentSetExpired :: Newtype AttachmentSetExpired _


newtype AttachmentSetId = AttachmentSetId String
derive instance newtypeAttachmentSetId :: Newtype AttachmentSetId _


-- | <p>An attachment set with the specified ID could not be found.</p>
newtype AttachmentSetIdNotFound = AttachmentSetIdNotFound 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAttachmentSetIdNotFound :: Newtype AttachmentSetIdNotFound _


-- | <p>A limit for the size of an attachment set has been exceeded. The limits are 3 attachments and 5 MB per attachment.</p>
newtype AttachmentSetSizeLimitExceeded = AttachmentSetSizeLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAttachmentSetSizeLimitExceeded :: Newtype AttachmentSetSizeLimitExceeded _


newtype Attachments = Attachments (Array Attachment)
derive instance newtypeAttachments :: Newtype Attachments _


newtype BeforeTime = BeforeTime String
derive instance newtypeBeforeTime :: Newtype BeforeTime _


-- | <p>The case creation limit for the account has been exceeded.</p>
newtype CaseCreationLimitExceeded = CaseCreationLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeCaseCreationLimitExceeded :: Newtype CaseCreationLimitExceeded _


-- | <p>A JSON-formatted object that contains the metadata for a support case. It is contained the response from a <a>DescribeCases</a> request. <b>CaseDetails</b> contains the following fields:</p> <ul> <li> <p> <b>caseId.</b> The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-<i>12345678910-2013-c4c1d2bf33c5cf47</i>.</p> </li> <li> <p> <b>categoryCode.</b> The category of problem for the AWS Support case. Corresponds to the CategoryCode values returned by a call to <a>DescribeServices</a>.</p> </li> <li> <p> <b>displayId.</b> The identifier for the case on pages in the AWS Support Center.</p> </li> <li> <p> <b>language.</b> The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.</p> </li> <li> <p> <b>recentCommunications.</b> One or more <a>Communication</a> objects. Fields of these objects are <code>attachments</code>, <code>body</code>, <code>caseId</code>, <code>submittedBy</code>, and <code>timeCreated</code>.</p> </li> <li> <p> <b>nextToken.</b> A resumption point for pagination.</p> </li> <li> <p> <b>serviceCode.</b> The identifier for the AWS service that corresponds to the service code defined in the call to <a>DescribeServices</a>.</p> </li> <li> <p> <b>severityCode. </b>The severity code assigned to the case. Contains one of the values returned by the call to <a>DescribeSeverityLevels</a>.</p> </li> <li> <p> <b>status.</b> The status of the case in the AWS Support Center.</p> </li> <li> <p> <b>subject.</b> The subject line of the case.</p> </li> <li> <p> <b>submittedBy.</b> The email address of the account that submitted the case.</p> </li> <li> <p> <b>timeCreated.</b> The time the case was created, in ISO-8601 format.</p> </li> </ul>
newtype CaseDetails = CaseDetails 
  { "CaseId'" :: NullOrUndefined (CaseId)
  , "DisplayId'" :: NullOrUndefined (DisplayId)
  , "Subject'" :: NullOrUndefined (Subject)
  , "Status'" :: NullOrUndefined (Status)
  , "ServiceCode'" :: NullOrUndefined (ServiceCode)
  , "CategoryCode'" :: NullOrUndefined (CategoryCode)
  , "SeverityCode'" :: NullOrUndefined (SeverityCode)
  , "SubmittedBy'" :: NullOrUndefined (SubmittedBy)
  , "TimeCreated'" :: NullOrUndefined (TimeCreated)
  , "RecentCommunications'" :: NullOrUndefined (RecentCaseCommunications)
  , "CcEmailAddresses'" :: NullOrUndefined (CcEmailAddressList)
  , "Language'" :: NullOrUndefined (Language)
  }
derive instance newtypeCaseDetails :: Newtype CaseDetails _


newtype CaseId = CaseId String
derive instance newtypeCaseId :: Newtype CaseId _


newtype CaseIdList = CaseIdList (Array CaseId)
derive instance newtypeCaseIdList :: Newtype CaseIdList _


-- | <p>The requested <code>caseId</code> could not be located.</p>
newtype CaseIdNotFound = CaseIdNotFound 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeCaseIdNotFound :: Newtype CaseIdNotFound _


newtype CaseList = CaseList (Array CaseDetails)
derive instance newtypeCaseList :: Newtype CaseList _


newtype CaseStatus = CaseStatus String
derive instance newtypeCaseStatus :: Newtype CaseStatus _


-- | <p>A JSON-formatted name/value pair that represents the category name and category code of the problem, selected from the <a>DescribeServices</a> response for each AWS service.</p>
newtype Category = Category 
  { "Code'" :: NullOrUndefined (CategoryCode)
  , "Name'" :: NullOrUndefined (CategoryName)
  }
derive instance newtypeCategory :: Newtype Category _


newtype CategoryCode = CategoryCode String
derive instance newtypeCategoryCode :: Newtype CategoryCode _


newtype CategoryList = CategoryList (Array Category)
derive instance newtypeCategoryList :: Newtype CategoryList _


newtype CategoryName = CategoryName String
derive instance newtypeCategoryName :: Newtype CategoryName _


newtype CcEmailAddress = CcEmailAddress String
derive instance newtypeCcEmailAddress :: Newtype CcEmailAddress _


newtype CcEmailAddressList = CcEmailAddressList (Array CcEmailAddress)
derive instance newtypeCcEmailAddressList :: Newtype CcEmailAddressList _


-- | <p>A communication associated with an AWS Support case. The communication consists of the case ID, the message body, attachment information, the account email address, and the date and time of the communication.</p>
newtype Communication = Communication 
  { "CaseId'" :: NullOrUndefined (CaseId)
  , "Body'" :: NullOrUndefined (CommunicationBody)
  , "SubmittedBy'" :: NullOrUndefined (SubmittedBy)
  , "TimeCreated'" :: NullOrUndefined (TimeCreated)
  , "AttachmentSet'" :: NullOrUndefined (AttachmentSet)
  }
derive instance newtypeCommunication :: Newtype Communication _


newtype CommunicationBody = CommunicationBody String
derive instance newtypeCommunicationBody :: Newtype CommunicationBody _


newtype CommunicationList = CommunicationList (Array Communication)
derive instance newtypeCommunicationList :: Newtype CommunicationList _


-- | <p/>
newtype CreateCaseRequest = CreateCaseRequest 
  { "Subject'" :: (Subject)
  , "ServiceCode'" :: NullOrUndefined (ServiceCode)
  , "SeverityCode'" :: NullOrUndefined (SeverityCode)
  , "CategoryCode'" :: NullOrUndefined (CategoryCode)
  , "CommunicationBody'" :: (CommunicationBody)
  , "CcEmailAddresses'" :: NullOrUndefined (CcEmailAddressList)
  , "Language'" :: NullOrUndefined (Language)
  , "IssueType'" :: NullOrUndefined (IssueType)
  , "AttachmentSetId'" :: NullOrUndefined (AttachmentSetId)
  }
derive instance newtypeCreateCaseRequest :: Newtype CreateCaseRequest _


-- | <p>The AWS Support case ID returned by a successful completion of the <a>CreateCase</a> operation. </p>
newtype CreateCaseResponse = CreateCaseResponse 
  { "CaseId'" :: NullOrUndefined (CaseId)
  }
derive instance newtypeCreateCaseResponse :: Newtype CreateCaseResponse _


newtype Data = Data String
derive instance newtypeData :: Newtype Data _


-- | <p>The limit for the number of <a>DescribeAttachment</a> requests in a short period of time has been exceeded.</p>
newtype DescribeAttachmentLimitExceeded = DescribeAttachmentLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDescribeAttachmentLimitExceeded :: Newtype DescribeAttachmentLimitExceeded _


newtype DescribeAttachmentRequest = DescribeAttachmentRequest 
  { "AttachmentId'" :: (AttachmentId)
  }
derive instance newtypeDescribeAttachmentRequest :: Newtype DescribeAttachmentRequest _


-- | <p>The content and file name of the attachment returned by the <a>DescribeAttachment</a> operation.</p>
newtype DescribeAttachmentResponse = DescribeAttachmentResponse 
  { "Attachment'" :: NullOrUndefined (Attachment)
  }
derive instance newtypeDescribeAttachmentResponse :: Newtype DescribeAttachmentResponse _


-- | <p/>
newtype DescribeCasesRequest = DescribeCasesRequest 
  { "CaseIdList'" :: NullOrUndefined (CaseIdList)
  , "DisplayId'" :: NullOrUndefined (DisplayId)
  , "AfterTime'" :: NullOrUndefined (AfterTime)
  , "BeforeTime'" :: NullOrUndefined (BeforeTime)
  , "IncludeResolvedCases'" :: NullOrUndefined (IncludeResolvedCases)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  , "Language'" :: NullOrUndefined (Language)
  , "IncludeCommunications'" :: NullOrUndefined (IncludeCommunications)
  }
derive instance newtypeDescribeCasesRequest :: Newtype DescribeCasesRequest _


-- | <p>Returns an array of <a>CaseDetails</a> objects and a <code>nextToken</code> that defines a point for pagination in the result set.</p>
newtype DescribeCasesResponse = DescribeCasesResponse 
  { "Cases'" :: NullOrUndefined (CaseList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeCasesResponse :: Newtype DescribeCasesResponse _


-- | <p/>
newtype DescribeCommunicationsRequest = DescribeCommunicationsRequest 
  { "CaseId'" :: (CaseId)
  , "BeforeTime'" :: NullOrUndefined (BeforeTime)
  , "AfterTime'" :: NullOrUndefined (AfterTime)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeDescribeCommunicationsRequest :: Newtype DescribeCommunicationsRequest _


-- | <p>The communications returned by the <a>DescribeCommunications</a> operation.</p>
newtype DescribeCommunicationsResponse = DescribeCommunicationsResponse 
  { "Communications'" :: NullOrUndefined (CommunicationList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeCommunicationsResponse :: Newtype DescribeCommunicationsResponse _


-- | <p/>
newtype DescribeServicesRequest = DescribeServicesRequest 
  { "ServiceCodeList'" :: NullOrUndefined (ServiceCodeList)
  , "Language'" :: NullOrUndefined (Language)
  }
derive instance newtypeDescribeServicesRequest :: Newtype DescribeServicesRequest _


-- | <p>The list of AWS services returned by the <a>DescribeServices</a> operation.</p>
newtype DescribeServicesResponse = DescribeServicesResponse 
  { "Services'" :: NullOrUndefined (ServiceList)
  }
derive instance newtypeDescribeServicesResponse :: Newtype DescribeServicesResponse _


-- | <p/>
newtype DescribeSeverityLevelsRequest = DescribeSeverityLevelsRequest 
  { "Language'" :: NullOrUndefined (Language)
  }
derive instance newtypeDescribeSeverityLevelsRequest :: Newtype DescribeSeverityLevelsRequest _


-- | <p>The list of severity levels returned by the <a>DescribeSeverityLevels</a> operation.</p>
newtype DescribeSeverityLevelsResponse = DescribeSeverityLevelsResponse 
  { "SeverityLevels'" :: NullOrUndefined (SeverityLevelsList)
  }
derive instance newtypeDescribeSeverityLevelsResponse :: Newtype DescribeSeverityLevelsResponse _


-- | <p/>
newtype DescribeTrustedAdvisorCheckRefreshStatusesRequest = DescribeTrustedAdvisorCheckRefreshStatusesRequest 
  { "CheckIds'" :: (StringList)
  }
derive instance newtypeDescribeTrustedAdvisorCheckRefreshStatusesRequest :: Newtype DescribeTrustedAdvisorCheckRefreshStatusesRequest _


-- | <p>The statuses of the Trusted Advisor checks returned by the <a>DescribeTrustedAdvisorCheckRefreshStatuses</a> operation.</p>
newtype DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse 
  { "Statuses'" :: (TrustedAdvisorCheckRefreshStatusList)
  }
derive instance newtypeDescribeTrustedAdvisorCheckRefreshStatusesResponse :: Newtype DescribeTrustedAdvisorCheckRefreshStatusesResponse _


-- | <p/>
newtype DescribeTrustedAdvisorCheckResultRequest = DescribeTrustedAdvisorCheckResultRequest 
  { "CheckId'" :: (String)
  , "Language'" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeTrustedAdvisorCheckResultRequest :: Newtype DescribeTrustedAdvisorCheckResultRequest _


-- | <p>The result of the Trusted Advisor check returned by the <a>DescribeTrustedAdvisorCheckResult</a> operation.</p>
newtype DescribeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse 
  { "Result'" :: NullOrUndefined (TrustedAdvisorCheckResult)
  }
derive instance newtypeDescribeTrustedAdvisorCheckResultResponse :: Newtype DescribeTrustedAdvisorCheckResultResponse _


-- | <p/>
newtype DescribeTrustedAdvisorCheckSummariesRequest = DescribeTrustedAdvisorCheckSummariesRequest 
  { "CheckIds'" :: (StringList)
  }
derive instance newtypeDescribeTrustedAdvisorCheckSummariesRequest :: Newtype DescribeTrustedAdvisorCheckSummariesRequest _


-- | <p>The summaries of the Trusted Advisor checks returned by the <a>DescribeTrustedAdvisorCheckSummaries</a> operation.</p>
newtype DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse 
  { "Summaries'" :: (TrustedAdvisorCheckSummaryList)
  }
derive instance newtypeDescribeTrustedAdvisorCheckSummariesResponse :: Newtype DescribeTrustedAdvisorCheckSummariesResponse _


-- | <p/>
newtype DescribeTrustedAdvisorChecksRequest = DescribeTrustedAdvisorChecksRequest 
  { "Language'" :: (String)
  }
derive instance newtypeDescribeTrustedAdvisorChecksRequest :: Newtype DescribeTrustedAdvisorChecksRequest _


-- | <p>Information about the Trusted Advisor checks returned by the <a>DescribeTrustedAdvisorChecks</a> operation.</p>
newtype DescribeTrustedAdvisorChecksResponse = DescribeTrustedAdvisorChecksResponse 
  { "Checks'" :: (TrustedAdvisorCheckList)
  }
derive instance newtypeDescribeTrustedAdvisorChecksResponse :: Newtype DescribeTrustedAdvisorChecksResponse _


newtype DisplayId = DisplayId String
derive instance newtypeDisplayId :: Newtype DisplayId _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype ExpiryTime = ExpiryTime String
derive instance newtypeExpiryTime :: Newtype ExpiryTime _


newtype FileName = FileName String
derive instance newtypeFileName :: Newtype FileName _


newtype IncludeCommunications = IncludeCommunications Boolean
derive instance newtypeIncludeCommunications :: Newtype IncludeCommunications _


newtype IncludeResolvedCases = IncludeResolvedCases Boolean
derive instance newtypeIncludeResolvedCases :: Newtype IncludeResolvedCases _


-- | <p>An internal server error occurred.</p>
newtype InternalServerError = InternalServerError 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _


newtype IssueType = IssueType String
derive instance newtypeIssueType :: Newtype IssueType _


newtype Language = Language String
derive instance newtypeLanguage :: Newtype Language _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>The five most recent communications associated with the case.</p>
newtype RecentCaseCommunications = RecentCaseCommunications 
  { "Communications'" :: NullOrUndefined (CommunicationList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeRecentCaseCommunications :: Newtype RecentCaseCommunications _


-- | <p/>
newtype RefreshTrustedAdvisorCheckRequest = RefreshTrustedAdvisorCheckRequest 
  { "CheckId'" :: (String)
  }
derive instance newtypeRefreshTrustedAdvisorCheckRequest :: Newtype RefreshTrustedAdvisorCheckRequest _


-- | <p>The current refresh status of a Trusted Advisor check.</p>
newtype RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse 
  { "Status'" :: (TrustedAdvisorCheckRefreshStatus)
  }
derive instance newtypeRefreshTrustedAdvisorCheckResponse :: Newtype RefreshTrustedAdvisorCheckResponse _


-- | <p/>
newtype ResolveCaseRequest = ResolveCaseRequest 
  { "CaseId'" :: NullOrUndefined (CaseId)
  }
derive instance newtypeResolveCaseRequest :: Newtype ResolveCaseRequest _


-- | <p>The status of the case returned by the <a>ResolveCase</a> operation.</p>
newtype ResolveCaseResponse = ResolveCaseResponse 
  { "InitialCaseStatus'" :: NullOrUndefined (CaseStatus)
  , "FinalCaseStatus'" :: NullOrUndefined (CaseStatus)
  }
derive instance newtypeResolveCaseResponse :: Newtype ResolveCaseResponse _


newtype Result = Result Boolean
derive instance newtypeResult :: Newtype Result _


-- | <p>Information about an AWS service returned by the <a>DescribeServices</a> operation. </p>
newtype Service = Service 
  { "Code'" :: NullOrUndefined (ServiceCode)
  , "Name'" :: NullOrUndefined (ServiceName)
  , "Categories'" :: NullOrUndefined (CategoryList)
  }
derive instance newtypeService :: Newtype Service _


newtype ServiceCode = ServiceCode String
derive instance newtypeServiceCode :: Newtype ServiceCode _


newtype ServiceCodeList = ServiceCodeList (Array ServiceCode)
derive instance newtypeServiceCodeList :: Newtype ServiceCodeList _


newtype ServiceList = ServiceList (Array Service)
derive instance newtypeServiceList :: Newtype ServiceList _


newtype ServiceName = ServiceName String
derive instance newtypeServiceName :: Newtype ServiceName _


newtype SeverityCode = SeverityCode String
derive instance newtypeSeverityCode :: Newtype SeverityCode _


-- | <p>A code and name pair that represent a severity level that can be applied to a support case.</p>
newtype SeverityLevel = SeverityLevel 
  { "Code'" :: NullOrUndefined (SeverityLevelCode)
  , "Name'" :: NullOrUndefined (SeverityLevelName)
  }
derive instance newtypeSeverityLevel :: Newtype SeverityLevel _


newtype SeverityLevelCode = SeverityLevelCode String
derive instance newtypeSeverityLevelCode :: Newtype SeverityLevelCode _


newtype SeverityLevelName = SeverityLevelName String
derive instance newtypeSeverityLevelName :: Newtype SeverityLevelName _


newtype SeverityLevelsList = SeverityLevelsList (Array SeverityLevel)
derive instance newtypeSeverityLevelsList :: Newtype SeverityLevelsList _


newtype Status = Status String
derive instance newtypeStatus :: Newtype Status _


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _


newtype Subject = Subject String
derive instance newtypeSubject :: Newtype Subject _


newtype SubmittedBy = SubmittedBy String
derive instance newtypeSubmittedBy :: Newtype SubmittedBy _


newtype TimeCreated = TimeCreated String
derive instance newtypeTimeCreated :: Newtype TimeCreated _


-- | <p>The container for summary information that relates to the category of the Trusted Advisor check.</p>
newtype TrustedAdvisorCategorySpecificSummary = TrustedAdvisorCategorySpecificSummary 
  { "CostOptimizing'" :: NullOrUndefined (TrustedAdvisorCostOptimizingSummary)
  }
derive instance newtypeTrustedAdvisorCategorySpecificSummary :: Newtype TrustedAdvisorCategorySpecificSummary _


-- | <p>The description and metadata for a Trusted Advisor check.</p>
newtype TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription 
  { "Id'" :: (String)
  , "Name'" :: (String)
  , "Description'" :: (String)
  , "Category'" :: (String)
  , "Metadata'" :: (StringList)
  }
derive instance newtypeTrustedAdvisorCheckDescription :: Newtype TrustedAdvisorCheckDescription _


newtype TrustedAdvisorCheckList = TrustedAdvisorCheckList (Array TrustedAdvisorCheckDescription)
derive instance newtypeTrustedAdvisorCheckList :: Newtype TrustedAdvisorCheckList _


-- | <p>The refresh status of a Trusted Advisor check.</p>
newtype TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus 
  { "CheckId'" :: (String)
  , "Status'" :: (String)
  , "MillisUntilNextRefreshable'" :: (Number)
  }
derive instance newtypeTrustedAdvisorCheckRefreshStatus :: Newtype TrustedAdvisorCheckRefreshStatus _


newtype TrustedAdvisorCheckRefreshStatusList = TrustedAdvisorCheckRefreshStatusList (Array TrustedAdvisorCheckRefreshStatus)
derive instance newtypeTrustedAdvisorCheckRefreshStatusList :: Newtype TrustedAdvisorCheckRefreshStatusList _


-- | <p>The results of a Trusted Advisor check returned by <a>DescribeTrustedAdvisorCheckResult</a>.</p>
newtype TrustedAdvisorCheckResult = TrustedAdvisorCheckResult 
  { "CheckId'" :: (String)
  , "Number" :: (String)
  , "Status'" :: (String)
  , "ResourcesSummary'" :: (TrustedAdvisorResourcesSummary)
  , "CategorySpecificSummary'" :: (TrustedAdvisorCategorySpecificSummary)
  , "FlaggedResources'" :: (TrustedAdvisorResourceDetailList)
  }
derive instance newtypeTrustedAdvisorCheckResult :: Newtype TrustedAdvisorCheckResult _


-- | <p>A summary of a Trusted Advisor check result, including the alert status, last refresh, and number of resources examined.</p>
newtype TrustedAdvisorCheckSummary = TrustedAdvisorCheckSummary 
  { "CheckId'" :: (String)
  , "Number" :: (String)
  , "Status'" :: (String)
  , "HasFlaggedResources'" :: NullOrUndefined (Boolean)
  , "ResourcesSummary'" :: (TrustedAdvisorResourcesSummary)
  , "CategorySpecificSummary'" :: (TrustedAdvisorCategorySpecificSummary)
  }
derive instance newtypeTrustedAdvisorCheckSummary :: Newtype TrustedAdvisorCheckSummary _


newtype TrustedAdvisorCheckSummaryList = TrustedAdvisorCheckSummaryList (Array TrustedAdvisorCheckSummary)
derive instance newtypeTrustedAdvisorCheckSummaryList :: Newtype TrustedAdvisorCheckSummaryList _


-- | <p>The estimated cost savings that might be realized if the recommended actions are taken.</p>
newtype TrustedAdvisorCostOptimizingSummary = TrustedAdvisorCostOptimizingSummary 
  { "EstimatedMonthlySavings'" :: (Number)
  , "EstimatedPercentMonthlySavings'" :: (Number)
  }
derive instance newtypeTrustedAdvisorCostOptimizingSummary :: Newtype TrustedAdvisorCostOptimizingSummary _


-- | <p>Contains information about a resource identified by a Trusted Advisor check.</p>
newtype TrustedAdvisorResourceDetail = TrustedAdvisorResourceDetail 
  { "Status'" :: (String)
  , "Region'" :: NullOrUndefined (String)
  , "ResourceId'" :: (String)
  , "IsSuppressed'" :: NullOrUndefined (Boolean)
  , "Metadata'" :: (StringList)
  }
derive instance newtypeTrustedAdvisorResourceDetail :: Newtype TrustedAdvisorResourceDetail _


newtype TrustedAdvisorResourceDetailList = TrustedAdvisorResourceDetailList (Array TrustedAdvisorResourceDetail)
derive instance newtypeTrustedAdvisorResourceDetailList :: Newtype TrustedAdvisorResourceDetailList _


-- | <p>Details about AWS resources that were analyzed in a call to Trusted Advisor <a>DescribeTrustedAdvisorCheckSummaries</a>. </p>
newtype TrustedAdvisorResourcesSummary = TrustedAdvisorResourcesSummary 
  { "ResourcesProcessed'" :: (Number)
  , "ResourcesFlagged'" :: (Number)
  , "ResourcesIgnored'" :: (Number)
  , "ResourcesSuppressed'" :: (Number)
  }
derive instance newtypeTrustedAdvisorResourcesSummary :: Newtype TrustedAdvisorResourcesSummary _
