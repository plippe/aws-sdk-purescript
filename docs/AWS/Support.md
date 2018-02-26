## Module AWS.Support

<fullname>AWS Support</fullname> <p>The AWS Support API reference is intended for programmers who need detailed information about the AWS Support operations and data types. This service enables you to manage your AWS Support cases programmatically. It uses HTTP methods that return results in JSON format.</p> <p>The AWS Support service also exposes a set of <a href="http://aws.amazon.com/premiumsupport/trustedadvisor/">Trusted Advisor</a> features. You can retrieve a list of checks and their descriptions, get check results, specify checks to refresh, and get the refresh status of checks. </p> <p>The following list describes the AWS Support case management operations:</p> <ul> <li> <p> <b>Service names, issue categories, and available severity levels. </b>The <a>DescribeServices</a> and <a>DescribeSeverityLevels</a> operations return AWS service names, service codes, service categories, and problem severity levels. You use these values when you call the <a>CreateCase</a> operation. </p> </li> <li> <p> <b>Case creation, case details, and case resolution.</b> The <a>CreateCase</a>, <a>DescribeCases</a>, <a>DescribeAttachment</a>, and <a>ResolveCase</a> operations create AWS Support cases, retrieve information about cases, and resolve cases.</p> </li> <li> <p> <b>Case communication.</b> The <a>DescribeCommunications</a>, <a>AddCommunicationToCase</a>, and <a>AddAttachmentsToSet</a> operations retrieve and add communications and attachments to AWS Support cases. </p> </li> </ul> <p>The following list describes the operations available from the AWS Support service for Trusted Advisor:</p> <ul> <li> <p> <a>DescribeTrustedAdvisorChecks</a> returns the list of checks that run against your AWS resources.</p> </li> <li> <p>Using the <code>checkId</code> for a specific check returned by <a>DescribeTrustedAdvisorChecks</a>, you can call <a>DescribeTrustedAdvisorCheckResult</a> to obtain the results for the check you specified.</p> </li> <li> <p> <a>DescribeTrustedAdvisorCheckSummaries</a> returns summarized results for one or more Trusted Advisor checks.</p> </li> <li> <p> <a>RefreshTrustedAdvisorCheck</a> requests that Trusted Advisor rerun a specified check. </p> </li> <li> <p> <a>DescribeTrustedAdvisorCheckRefreshStatuses</a> reports the refresh status of one or more checks. </p> </li> </ul> <p>For authentication of requests, AWS Support uses <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a>.</p> <p>See <a href="http://docs.aws.amazon.com/awssupport/latest/user/Welcome.html">About the AWS Support API</a> in the <i>AWS Support User Guide</i> for information about how to use this service to create and manage your support cases, and how to call Trusted Advisor for results of checks on your resources. </p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addAttachmentsToSet`

``` purescript
addAttachmentsToSet :: forall eff. AddAttachmentsToSetRequest -> Aff (err :: RequestError | eff) AddAttachmentsToSetResponse
```

<p>Adds one or more attachments to an attachment set. If an <code>attachmentSetId</code> is not specified, a new attachment set is created, and the ID of the set is returned in the response. If an <code>attachmentSetId</code> is specified, the attachments are added to the specified set, if it exists.</p> <p>An attachment set is a temporary container for attachments that are to be added to a case or case communication. The set is available for one hour after it is created; the <code>expiryTime</code> returned in the response indicates when the set expires. The maximum number of attachments in a set is 3, and the maximum size of any attachment in the set is 5 MB.</p>

#### `addCommunicationToCase`

``` purescript
addCommunicationToCase :: forall eff. AddCommunicationToCaseRequest -> Aff (err :: RequestError | eff) AddCommunicationToCaseResponse
```

<p>Adds additional customer communication to an AWS Support case. You use the <code>caseId</code> value to identify the case to add communication to. You can list a set of email addresses to copy on the communication using the <code>ccEmailAddresses</code> value. The <code>communicationBody</code> value contains the text of the communication.</p> <p>The response indicates the success or failure of the request.</p> <p>This operation implements a subset of the features of the AWS Support Center.</p>

#### `createCase`

``` purescript
createCase :: forall eff. CreateCaseRequest -> Aff (err :: RequestError | eff) CreateCaseResponse
```

<p>Creates a new case in the AWS Support Center. This operation is modeled on the behavior of the AWS Support Center <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page. Its parameters require you to specify the following information: </p> <ul> <li> <p> <b>issueType.</b> The type of issue for the case. You can specify either "customer-service" or "technical." If you do not indicate a value, the default is "technical." </p> </li> <li> <p> <b>serviceCode.</b> The code for an AWS service. You obtain the <code>serviceCode</code> by calling <a>DescribeServices</a>. </p> </li> <li> <p> <b>categoryCode.</b> The category for the service defined for the <code>serviceCode</code> value. You also obtain the category code for a service by calling <a>DescribeServices</a>. Each AWS service defines its own set of category codes. </p> </li> <li> <p> <b>severityCode.</b> A value that indicates the urgency of the case, which in turn determines the response time according to your service level agreement with AWS Support. You obtain the SeverityCode by calling <a>DescribeSeverityLevels</a>.</p> </li> <li> <p> <b>subject.</b> The <b>Subject</b> field on the AWS Support Center <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page.</p> </li> <li> <p> <b>communicationBody.</b> The <b>Description</b> field on the AWS Support Center <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page.</p> </li> <li> <p> <b>attachmentSetId.</b> The ID of a set of attachments that has been created by using <a>AddAttachmentsToSet</a>.</p> </li> <li> <p> <b>language.</b> The human language in which AWS Support handles the case. English and Japanese are currently supported.</p> </li> <li> <p> <b>ccEmailAddresses.</b> The AWS Support Center <b>CC</b> field on the <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page. You can list email addresses to be copied on any correspondence about the case. The account that opens the case is already identified by passing the AWS Credentials in the HTTP POST method or in a method or function call from one of the programming languages supported by an <a href="http://aws.amazon.com/tools/">AWS SDK</a>. </p> </li> </ul> <note> <p>To add additional communication or attachments to an existing case, use <a>AddCommunicationToCase</a>.</p> </note> <p>A successful <a>CreateCase</a> request returns an AWS Support case number. Case numbers are used by the <a>DescribeCases</a> operation to retrieve existing AWS Support cases. </p>

#### `describeAttachment`

``` purescript
describeAttachment :: forall eff. DescribeAttachmentRequest -> Aff (err :: RequestError | eff) DescribeAttachmentResponse
```

<p>Returns the attachment that has the specified ID. Attachment IDs are generated by the case management system when you add an attachment to a case or case communication. Attachment IDs are returned in the <a>AttachmentDetails</a> objects that are returned by the <a>DescribeCommunications</a> operation.</p>

#### `describeCases`

``` purescript
describeCases :: forall eff. DescribeCasesRequest -> Aff (err :: RequestError | eff) DescribeCasesResponse
```

<p>Returns a list of cases that you specify by passing one or more case IDs. In addition, you can filter the cases by date by setting values for the <code>afterTime</code> and <code>beforeTime</code> request parameters. You can set values for the <code>includeResolvedCases</code> and <code>includeCommunications</code> request parameters to control how much information is returned. </p> <p>Case data is available for 12 months after creation. If a case was created more than 12 months ago, a request for data might cause an error.</p> <p>The response returns the following in JSON format:</p> <ul> <li> <p>One or more <a>CaseDetails</a> data types. </p> </li> <li> <p>One or more <code>nextToken</code> values, which specify where to paginate the returned records represented by the <code>CaseDetails</code> objects.</p> </li> </ul>

#### `describeCommunications`

``` purescript
describeCommunications :: forall eff. DescribeCommunicationsRequest -> Aff (err :: RequestError | eff) DescribeCommunicationsResponse
```

<p>Returns communications (and attachments) for one or more support cases. You can use the <code>afterTime</code> and <code>beforeTime</code> parameters to filter by date. You can use the <code>caseId</code> parameter to restrict the results to a particular case.</p> <p>Case data is available for 12 months after creation. If a case was created more than 12 months ago, a request for data might cause an error.</p> <p>You can use the <code>maxResults</code> and <code>nextToken</code> parameters to control the pagination of the result set. Set <code>maxResults</code> to the number of cases you want displayed on each page, and use <code>nextToken</code> to specify the resumption of pagination.</p>

#### `describeServices`

``` purescript
describeServices :: forall eff. DescribeServicesRequest -> Aff (err :: RequestError | eff) DescribeServicesResponse
```

<p>Returns the current list of AWS services and a list of service categories that applies to each one. You then use service names and categories in your <a>CreateCase</a> requests. Each AWS service has its own set of categories.</p> <p>The service codes and category codes correspond to the values that are displayed in the <b>Service</b> and <b>Category</b> drop-down lists on the AWS Support Center <a href="https://console.aws.amazon.com/support/home#/case/create">Create Case</a> page. The values in those fields, however, do not necessarily match the service codes and categories returned by the <code>DescribeServices</code> request. Always use the service codes and categories obtained programmatically. This practice ensures that you always have the most recent set of service and category codes.</p>

#### `describeSeverityLevels`

``` purescript
describeSeverityLevels :: forall eff. DescribeSeverityLevelsRequest -> Aff (err :: RequestError | eff) DescribeSeverityLevelsResponse
```

<p>Returns the list of severity levels that you can assign to an AWS Support case. The severity level for a case is also a field in the <a>CaseDetails</a> data type included in any <a>CreateCase</a> request. </p>

#### `describeTrustedAdvisorCheckRefreshStatuses`

``` purescript
describeTrustedAdvisorCheckRefreshStatuses :: forall eff. DescribeTrustedAdvisorCheckRefreshStatusesRequest -> Aff (err :: RequestError | eff) DescribeTrustedAdvisorCheckRefreshStatusesResponse
```

<p>Returns the refresh status of the Trusted Advisor checks that have the specified check IDs. Check IDs can be obtained by calling <a>DescribeTrustedAdvisorChecks</a>.</p> <note> <p>Some checks are refreshed automatically, and their refresh statuses cannot be retrieved by using this operation. Use of the <code>DescribeTrustedAdvisorCheckRefreshStatuses</code> operation for these checks causes an <code>InvalidParameterValue</code> error.</p> </note>

#### `describeTrustedAdvisorCheckResult`

``` purescript
describeTrustedAdvisorCheckResult :: forall eff. DescribeTrustedAdvisorCheckResultRequest -> Aff (err :: RequestError | eff) DescribeTrustedAdvisorCheckResultResponse
```

<p>Returns the results of the Trusted Advisor check that has the specified check ID. Check IDs can be obtained by calling <a>DescribeTrustedAdvisorChecks</a>.</p> <p>The response contains a <a>TrustedAdvisorCheckResult</a> object, which contains these three objects:</p> <ul> <li> <p> <a>TrustedAdvisorCategorySpecificSummary</a> </p> </li> <li> <p> <a>TrustedAdvisorResourceDetail</a> </p> </li> <li> <p> <a>TrustedAdvisorResourcesSummary</a> </p> </li> </ul> <p>In addition, the response contains these fields:</p> <ul> <li> <p> <b>status.</b> The alert status of the check: "ok" (green), "warning" (yellow), "error" (red), or "not_available".</p> </li> <li> <p> <b>timestamp.</b> The time of the last refresh of the check.</p> </li> <li> <p> <b>checkId.</b> The unique identifier for the check.</p> </li> </ul>

#### `describeTrustedAdvisorCheckSummaries`

``` purescript
describeTrustedAdvisorCheckSummaries :: forall eff. DescribeTrustedAdvisorCheckSummariesRequest -> Aff (err :: RequestError | eff) DescribeTrustedAdvisorCheckSummariesResponse
```

<p>Returns the summaries of the results of the Trusted Advisor checks that have the specified check IDs. Check IDs can be obtained by calling <a>DescribeTrustedAdvisorChecks</a>.</p> <p>The response contains an array of <a>TrustedAdvisorCheckSummary</a> objects.</p>

#### `describeTrustedAdvisorChecks`

``` purescript
describeTrustedAdvisorChecks :: forall eff. DescribeTrustedAdvisorChecksRequest -> Aff (err :: RequestError | eff) DescribeTrustedAdvisorChecksResponse
```

<p>Returns information about all available Trusted Advisor checks, including name, ID, category, description, and metadata. You must specify a language code; English ("en") and Japanese ("ja") are currently supported. The response contains a <a>TrustedAdvisorCheckDescription</a> for each check.</p>

#### `refreshTrustedAdvisorCheck`

``` purescript
refreshTrustedAdvisorCheck :: forall eff. RefreshTrustedAdvisorCheckRequest -> Aff (err :: RequestError | eff) RefreshTrustedAdvisorCheckResponse
```

<p>Requests a refresh of the Trusted Advisor check that has the specified check ID. Check IDs can be obtained by calling <a>DescribeTrustedAdvisorChecks</a>.</p> <note> <p>Some checks are refreshed automatically, and they cannot be refreshed by using this operation. Use of the <code>RefreshTrustedAdvisorCheck</code> operation for these checks causes an <code>InvalidParameterValue</code> error.</p> </note> <p>The response contains a <a>TrustedAdvisorCheckRefreshStatus</a> object, which contains these fields:</p> <ul> <li> <p> <b>status.</b> The refresh status of the check: "none", "enqueued", "processing", "success", or "abandoned".</p> </li> <li> <p> <b>millisUntilNextRefreshable.</b> The amount of time, in milliseconds, until the check is eligible for refresh.</p> </li> <li> <p> <b>checkId.</b> The unique identifier for the check.</p> </li> </ul>

#### `resolveCase`

``` purescript
resolveCase :: forall eff. ResolveCaseRequest -> Aff (err :: RequestError | eff) ResolveCaseResponse
```

<p>Takes a <code>caseId</code> and returns the initial state of the case along with the state of the case after the call to <a>ResolveCase</a> completed.</p>

#### `AddAttachmentsToSetRequest`

``` purescript
newtype AddAttachmentsToSetRequest
  = AddAttachmentsToSetRequest { "AttachmentSetId'" :: NullOrUndefined (AttachmentSetId), "Attachments'" :: Attachments }
```

<p/>

#### `AddAttachmentsToSetResponse`

``` purescript
newtype AddAttachmentsToSetResponse
  = AddAttachmentsToSetResponse { "AttachmentSetId'" :: NullOrUndefined (AttachmentSetId), "ExpiryTime'" :: NullOrUndefined (ExpiryTime) }
```

<p>The ID and expiry time of the attachment set returned by the <a>AddAttachmentsToSet</a> operation.</p>

#### `AddCommunicationToCaseRequest`

``` purescript
newtype AddCommunicationToCaseRequest
  = AddCommunicationToCaseRequest { "CaseId'" :: NullOrUndefined (CaseId), "CommunicationBody'" :: CommunicationBody, "CcEmailAddresses'" :: NullOrUndefined (CcEmailAddressList), "AttachmentSetId'" :: NullOrUndefined (AttachmentSetId) }
```

<p>To be written.</p>

#### `AddCommunicationToCaseResponse`

``` purescript
newtype AddCommunicationToCaseResponse
  = AddCommunicationToCaseResponse { "Result'" :: NullOrUndefined (Result) }
```

<p>The result of the <a>AddCommunicationToCase</a> operation.</p>

#### `AfterTime`

``` purescript
newtype AfterTime
  = AfterTime String
```

#### `Attachment`

``` purescript
newtype Attachment
  = Attachment { "FileName'" :: NullOrUndefined (FileName), "Data'" :: NullOrUndefined (Data) }
```

<p>An attachment to a case communication. The attachment consists of the file name and the content of the file.</p>

#### `AttachmentDetails`

``` purescript
newtype AttachmentDetails
  = AttachmentDetails { "AttachmentId'" :: NullOrUndefined (AttachmentId), "FileName'" :: NullOrUndefined (FileName) }
```

<p>The file name and ID of an attachment to a case communication. You can use the ID to retrieve the attachment with the <a>DescribeAttachment</a> operation.</p>

#### `AttachmentId`

``` purescript
newtype AttachmentId
  = AttachmentId String
```

#### `AttachmentIdNotFound`

``` purescript
newtype AttachmentIdNotFound
  = AttachmentIdNotFound { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>An attachment with the specified ID could not be found.</p>

#### `AttachmentLimitExceeded`

``` purescript
newtype AttachmentLimitExceeded
  = AttachmentLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The limit for the number of attachment sets created in a short period of time has been exceeded.</p>

#### `AttachmentSet`

``` purescript
newtype AttachmentSet
  = AttachmentSet (Array AttachmentDetails)
```

#### `AttachmentSetExpired`

``` purescript
newtype AttachmentSetExpired
  = AttachmentSetExpired { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The expiration time of the attachment set has passed. The set expires 1 hour after it is created.</p>

#### `AttachmentSetId`

``` purescript
newtype AttachmentSetId
  = AttachmentSetId String
```

#### `AttachmentSetIdNotFound`

``` purescript
newtype AttachmentSetIdNotFound
  = AttachmentSetIdNotFound { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>An attachment set with the specified ID could not be found.</p>

#### `AttachmentSetSizeLimitExceeded`

``` purescript
newtype AttachmentSetSizeLimitExceeded
  = AttachmentSetSizeLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>A limit for the size of an attachment set has been exceeded. The limits are 3 attachments and 5 MB per attachment.</p>

#### `Attachments`

``` purescript
newtype Attachments
  = Attachments (Array Attachment)
```

#### `BeforeTime`

``` purescript
newtype BeforeTime
  = BeforeTime String
```

#### `CaseCreationLimitExceeded`

``` purescript
newtype CaseCreationLimitExceeded
  = CaseCreationLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The case creation limit for the account has been exceeded.</p>

#### `CaseDetails`

``` purescript
newtype CaseDetails
  = CaseDetails { "CaseId'" :: NullOrUndefined (CaseId), "DisplayId'" :: NullOrUndefined (DisplayId), "Subject'" :: NullOrUndefined (Subject), "Status'" :: NullOrUndefined (Status), "ServiceCode'" :: NullOrUndefined (ServiceCode), "CategoryCode'" :: NullOrUndefined (CategoryCode), "SeverityCode'" :: NullOrUndefined (SeverityCode), "SubmittedBy'" :: NullOrUndefined (SubmittedBy), "TimeCreated'" :: NullOrUndefined (TimeCreated), "RecentCommunications'" :: NullOrUndefined (RecentCaseCommunications), "CcEmailAddresses'" :: NullOrUndefined (CcEmailAddressList), "Language'" :: NullOrUndefined (Language) }
```

<p>A JSON-formatted object that contains the metadata for a support case. It is contained the response from a <a>DescribeCases</a> request. <b>CaseDetails</b> contains the following fields:</p> <ul> <li> <p> <b>caseId.</b> The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-<i>12345678910-2013-c4c1d2bf33c5cf47</i>.</p> </li> <li> <p> <b>categoryCode.</b> The category of problem for the AWS Support case. Corresponds to the CategoryCode values returned by a call to <a>DescribeServices</a>.</p> </li> <li> <p> <b>displayId.</b> The identifier for the case on pages in the AWS Support Center.</p> </li> <li> <p> <b>language.</b> The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.</p> </li> <li> <p> <b>recentCommunications.</b> One or more <a>Communication</a> objects. Fields of these objects are <code>attachments</code>, <code>body</code>, <code>caseId</code>, <code>submittedBy</code>, and <code>timeCreated</code>.</p> </li> <li> <p> <b>nextToken.</b> A resumption point for pagination.</p> </li> <li> <p> <b>serviceCode.</b> The identifier for the AWS service that corresponds to the service code defined in the call to <a>DescribeServices</a>.</p> </li> <li> <p> <b>severityCode. </b>The severity code assigned to the case. Contains one of the values returned by the call to <a>DescribeSeverityLevels</a>.</p> </li> <li> <p> <b>status.</b> The status of the case in the AWS Support Center.</p> </li> <li> <p> <b>subject.</b> The subject line of the case.</p> </li> <li> <p> <b>submittedBy.</b> The email address of the account that submitted the case.</p> </li> <li> <p> <b>timeCreated.</b> The time the case was created, in ISO-8601 format.</p> </li> </ul>

#### `CaseId`

``` purescript
newtype CaseId
  = CaseId String
```

#### `CaseIdList`

``` purescript
newtype CaseIdList
  = CaseIdList (Array CaseId)
```

#### `CaseIdNotFound`

``` purescript
newtype CaseIdNotFound
  = CaseIdNotFound { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The requested <code>caseId</code> could not be located.</p>

#### `CaseList`

``` purescript
newtype CaseList
  = CaseList (Array CaseDetails)
```

#### `CaseStatus`

``` purescript
newtype CaseStatus
  = CaseStatus String
```

#### `Category`

``` purescript
newtype Category
  = Category { "Code'" :: NullOrUndefined (CategoryCode), "Name'" :: NullOrUndefined (CategoryName) }
```

<p>A JSON-formatted name/value pair that represents the category name and category code of the problem, selected from the <a>DescribeServices</a> response for each AWS service.</p>

#### `CategoryCode`

``` purescript
newtype CategoryCode
  = CategoryCode String
```

#### `CategoryList`

``` purescript
newtype CategoryList
  = CategoryList (Array Category)
```

#### `CategoryName`

``` purescript
newtype CategoryName
  = CategoryName String
```

#### `CcEmailAddress`

``` purescript
newtype CcEmailAddress
  = CcEmailAddress String
```

#### `CcEmailAddressList`

``` purescript
newtype CcEmailAddressList
  = CcEmailAddressList (Array CcEmailAddress)
```

#### `Communication`

``` purescript
newtype Communication
  = Communication { "CaseId'" :: NullOrUndefined (CaseId), "Body'" :: NullOrUndefined (CommunicationBody), "SubmittedBy'" :: NullOrUndefined (SubmittedBy), "TimeCreated'" :: NullOrUndefined (TimeCreated), "AttachmentSet'" :: NullOrUndefined (AttachmentSet) }
```

<p>A communication associated with an AWS Support case. The communication consists of the case ID, the message body, attachment information, the account email address, and the date and time of the communication.</p>

#### `CommunicationBody`

``` purescript
newtype CommunicationBody
  = CommunicationBody String
```

#### `CommunicationList`

``` purescript
newtype CommunicationList
  = CommunicationList (Array Communication)
```

#### `CreateCaseRequest`

``` purescript
newtype CreateCaseRequest
  = CreateCaseRequest { "Subject'" :: Subject, "ServiceCode'" :: NullOrUndefined (ServiceCode), "SeverityCode'" :: NullOrUndefined (SeverityCode), "CategoryCode'" :: NullOrUndefined (CategoryCode), "CommunicationBody'" :: CommunicationBody, "CcEmailAddresses'" :: NullOrUndefined (CcEmailAddressList), "Language'" :: NullOrUndefined (Language), "IssueType'" :: NullOrUndefined (IssueType), "AttachmentSetId'" :: NullOrUndefined (AttachmentSetId) }
```

<p/>

#### `CreateCaseResponse`

``` purescript
newtype CreateCaseResponse
  = CreateCaseResponse { "CaseId'" :: NullOrUndefined (CaseId) }
```

<p>The AWS Support case ID returned by a successful completion of the <a>CreateCase</a> operation. </p>

#### `Data`

``` purescript
newtype Data
  = Data String
```

#### `DescribeAttachmentLimitExceeded`

``` purescript
newtype DescribeAttachmentLimitExceeded
  = DescribeAttachmentLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The limit for the number of <a>DescribeAttachment</a> requests in a short period of time has been exceeded.</p>

#### `DescribeAttachmentRequest`

``` purescript
newtype DescribeAttachmentRequest
  = DescribeAttachmentRequest { "AttachmentId'" :: AttachmentId }
```

#### `DescribeAttachmentResponse`

``` purescript
newtype DescribeAttachmentResponse
  = DescribeAttachmentResponse { "Attachment'" :: NullOrUndefined (Attachment) }
```

<p>The content and file name of the attachment returned by the <a>DescribeAttachment</a> operation.</p>

#### `DescribeCasesRequest`

``` purescript
newtype DescribeCasesRequest
  = DescribeCasesRequest { "CaseIdList'" :: NullOrUndefined (CaseIdList), "DisplayId'" :: NullOrUndefined (DisplayId), "AfterTime'" :: NullOrUndefined (AfterTime), "BeforeTime'" :: NullOrUndefined (BeforeTime), "IncludeResolvedCases'" :: NullOrUndefined (IncludeResolvedCases), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults), "Language'" :: NullOrUndefined (Language), "IncludeCommunications'" :: NullOrUndefined (IncludeCommunications) }
```

<p/>

#### `DescribeCasesResponse`

``` purescript
newtype DescribeCasesResponse
  = DescribeCasesResponse { "Cases'" :: NullOrUndefined (CaseList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Returns an array of <a>CaseDetails</a> objects and a <code>nextToken</code> that defines a point for pagination in the result set.</p>

#### `DescribeCommunicationsRequest`

``` purescript
newtype DescribeCommunicationsRequest
  = DescribeCommunicationsRequest { "CaseId'" :: CaseId, "BeforeTime'" :: NullOrUndefined (BeforeTime), "AfterTime'" :: NullOrUndefined (AfterTime), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

<p/>

#### `DescribeCommunicationsResponse`

``` purescript
newtype DescribeCommunicationsResponse
  = DescribeCommunicationsResponse { "Communications'" :: NullOrUndefined (CommunicationList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The communications returned by the <a>DescribeCommunications</a> operation.</p>

#### `DescribeServicesRequest`

``` purescript
newtype DescribeServicesRequest
  = DescribeServicesRequest { "ServiceCodeList'" :: NullOrUndefined (ServiceCodeList), "Language'" :: NullOrUndefined (Language) }
```

<p/>

#### `DescribeServicesResponse`

``` purescript
newtype DescribeServicesResponse
  = DescribeServicesResponse { "Services'" :: NullOrUndefined (ServiceList) }
```

<p>The list of AWS services returned by the <a>DescribeServices</a> operation.</p>

#### `DescribeSeverityLevelsRequest`

``` purescript
newtype DescribeSeverityLevelsRequest
  = DescribeSeverityLevelsRequest { "Language'" :: NullOrUndefined (Language) }
```

<p/>

#### `DescribeSeverityLevelsResponse`

``` purescript
newtype DescribeSeverityLevelsResponse
  = DescribeSeverityLevelsResponse { "SeverityLevels'" :: NullOrUndefined (SeverityLevelsList) }
```

<p>The list of severity levels returned by the <a>DescribeSeverityLevels</a> operation.</p>

#### `DescribeTrustedAdvisorCheckRefreshStatusesRequest`

``` purescript
newtype DescribeTrustedAdvisorCheckRefreshStatusesRequest
  = DescribeTrustedAdvisorCheckRefreshStatusesRequest { "CheckIds'" :: StringList }
```

<p/>

#### `DescribeTrustedAdvisorCheckRefreshStatusesResponse`

``` purescript
newtype DescribeTrustedAdvisorCheckRefreshStatusesResponse
  = DescribeTrustedAdvisorCheckRefreshStatusesResponse { "Statuses'" :: TrustedAdvisorCheckRefreshStatusList }
```

<p>The statuses of the Trusted Advisor checks returned by the <a>DescribeTrustedAdvisorCheckRefreshStatuses</a> operation.</p>

#### `DescribeTrustedAdvisorCheckResultRequest`

``` purescript
newtype DescribeTrustedAdvisorCheckResultRequest
  = DescribeTrustedAdvisorCheckResultRequest { "CheckId'" :: String, "Language'" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeTrustedAdvisorCheckResultResponse`

``` purescript
newtype DescribeTrustedAdvisorCheckResultResponse
  = DescribeTrustedAdvisorCheckResultResponse { "Result'" :: NullOrUndefined (TrustedAdvisorCheckResult) }
```

<p>The result of the Trusted Advisor check returned by the <a>DescribeTrustedAdvisorCheckResult</a> operation.</p>

#### `DescribeTrustedAdvisorCheckSummariesRequest`

``` purescript
newtype DescribeTrustedAdvisorCheckSummariesRequest
  = DescribeTrustedAdvisorCheckSummariesRequest { "CheckIds'" :: StringList }
```

<p/>

#### `DescribeTrustedAdvisorCheckSummariesResponse`

``` purescript
newtype DescribeTrustedAdvisorCheckSummariesResponse
  = DescribeTrustedAdvisorCheckSummariesResponse { "Summaries'" :: TrustedAdvisorCheckSummaryList }
```

<p>The summaries of the Trusted Advisor checks returned by the <a>DescribeTrustedAdvisorCheckSummaries</a> operation.</p>

#### `DescribeTrustedAdvisorChecksRequest`

``` purescript
newtype DescribeTrustedAdvisorChecksRequest
  = DescribeTrustedAdvisorChecksRequest { "Language'" :: String }
```

<p/>

#### `DescribeTrustedAdvisorChecksResponse`

``` purescript
newtype DescribeTrustedAdvisorChecksResponse
  = DescribeTrustedAdvisorChecksResponse { "Checks'" :: TrustedAdvisorCheckList }
```

<p>Information about the Trusted Advisor checks returned by the <a>DescribeTrustedAdvisorChecks</a> operation.</p>

#### `DisplayId`

``` purescript
newtype DisplayId
  = DisplayId String
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `ExpiryTime`

``` purescript
newtype ExpiryTime
  = ExpiryTime String
```

#### `FileName`

``` purescript
newtype FileName
  = FileName String
```

#### `IncludeCommunications`

``` purescript
newtype IncludeCommunications
  = IncludeCommunications Boolean
```

#### `IncludeResolvedCases`

``` purescript
newtype IncludeResolvedCases
  = IncludeResolvedCases Boolean
```

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>An internal server error occurred.</p>

#### `IssueType`

``` purescript
newtype IssueType
  = IssueType String
```

#### `Language`

``` purescript
newtype Language
  = Language String
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `RecentCaseCommunications`

``` purescript
newtype RecentCaseCommunications
  = RecentCaseCommunications { "Communications'" :: NullOrUndefined (CommunicationList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The five most recent communications associated with the case.</p>

#### `RefreshTrustedAdvisorCheckRequest`

``` purescript
newtype RefreshTrustedAdvisorCheckRequest
  = RefreshTrustedAdvisorCheckRequest { "CheckId'" :: String }
```

<p/>

#### `RefreshTrustedAdvisorCheckResponse`

``` purescript
newtype RefreshTrustedAdvisorCheckResponse
  = RefreshTrustedAdvisorCheckResponse { "Status'" :: TrustedAdvisorCheckRefreshStatus }
```

<p>The current refresh status of a Trusted Advisor check.</p>

#### `ResolveCaseRequest`

``` purescript
newtype ResolveCaseRequest
  = ResolveCaseRequest { "CaseId'" :: NullOrUndefined (CaseId) }
```

<p/>

#### `ResolveCaseResponse`

``` purescript
newtype ResolveCaseResponse
  = ResolveCaseResponse { "InitialCaseStatus'" :: NullOrUndefined (CaseStatus), "FinalCaseStatus'" :: NullOrUndefined (CaseStatus) }
```

<p>The status of the case returned by the <a>ResolveCase</a> operation.</p>

#### `Result`

``` purescript
newtype Result
  = Result Boolean
```

#### `Service`

``` purescript
newtype Service
  = Service { "Code'" :: NullOrUndefined (ServiceCode), "Name'" :: NullOrUndefined (ServiceName), "Categories'" :: NullOrUndefined (CategoryList) }
```

<p>Information about an AWS service returned by the <a>DescribeServices</a> operation. </p>

#### `ServiceCode`

``` purescript
newtype ServiceCode
  = ServiceCode String
```

#### `ServiceCodeList`

``` purescript
newtype ServiceCodeList
  = ServiceCodeList (Array ServiceCode)
```

#### `ServiceList`

``` purescript
newtype ServiceList
  = ServiceList (Array Service)
```

#### `ServiceName`

``` purescript
newtype ServiceName
  = ServiceName String
```

#### `SeverityCode`

``` purescript
newtype SeverityCode
  = SeverityCode String
```

#### `SeverityLevel`

``` purescript
newtype SeverityLevel
  = SeverityLevel { "Code'" :: NullOrUndefined (SeverityLevelCode), "Name'" :: NullOrUndefined (SeverityLevelName) }
```

<p>A code and name pair that represent a severity level that can be applied to a support case.</p>

#### `SeverityLevelCode`

``` purescript
newtype SeverityLevelCode
  = SeverityLevelCode String
```

#### `SeverityLevelName`

``` purescript
newtype SeverityLevelName
  = SeverityLevelName String
```

#### `SeverityLevelsList`

``` purescript
newtype SeverityLevelsList
  = SeverityLevelsList (Array SeverityLevel)
```

#### `Status`

``` purescript
newtype Status
  = Status String
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

#### `Subject`

``` purescript
newtype Subject
  = Subject String
```

#### `SubmittedBy`

``` purescript
newtype SubmittedBy
  = SubmittedBy String
```

#### `TimeCreated`

``` purescript
newtype TimeCreated
  = TimeCreated String
```

#### `TrustedAdvisorCategorySpecificSummary`

``` purescript
newtype TrustedAdvisorCategorySpecificSummary
  = TrustedAdvisorCategorySpecificSummary { "CostOptimizing'" :: NullOrUndefined (TrustedAdvisorCostOptimizingSummary) }
```

<p>The container for summary information that relates to the category of the Trusted Advisor check.</p>

#### `TrustedAdvisorCheckDescription`

``` purescript
newtype TrustedAdvisorCheckDescription
  = TrustedAdvisorCheckDescription { "Id'" :: String, "Name'" :: String, "Description'" :: String, "Category'" :: String, "Metadata'" :: StringList }
```

<p>The description and metadata for a Trusted Advisor check.</p>

#### `TrustedAdvisorCheckList`

``` purescript
newtype TrustedAdvisorCheckList
  = TrustedAdvisorCheckList (Array TrustedAdvisorCheckDescription)
```

#### `TrustedAdvisorCheckRefreshStatus`

``` purescript
newtype TrustedAdvisorCheckRefreshStatus
  = TrustedAdvisorCheckRefreshStatus { "CheckId'" :: String, "Status'" :: String, "MillisUntilNextRefreshable'" :: Number }
```

<p>The refresh status of a Trusted Advisor check.</p>

#### `TrustedAdvisorCheckRefreshStatusList`

``` purescript
newtype TrustedAdvisorCheckRefreshStatusList
  = TrustedAdvisorCheckRefreshStatusList (Array TrustedAdvisorCheckRefreshStatus)
```

#### `TrustedAdvisorCheckResult`

``` purescript
newtype TrustedAdvisorCheckResult
  = TrustedAdvisorCheckResult { "CheckId'" :: String, "Number" :: String, "Status'" :: String, "ResourcesSummary'" :: TrustedAdvisorResourcesSummary, "CategorySpecificSummary'" :: TrustedAdvisorCategorySpecificSummary, "FlaggedResources'" :: TrustedAdvisorResourceDetailList }
```

<p>The results of a Trusted Advisor check returned by <a>DescribeTrustedAdvisorCheckResult</a>.</p>

#### `TrustedAdvisorCheckSummary`

``` purescript
newtype TrustedAdvisorCheckSummary
  = TrustedAdvisorCheckSummary { "CheckId'" :: String, "Number" :: String, "Status'" :: String, "HasFlaggedResources'" :: NullOrUndefined (Boolean), "ResourcesSummary'" :: TrustedAdvisorResourcesSummary, "CategorySpecificSummary'" :: TrustedAdvisorCategorySpecificSummary }
```

<p>A summary of a Trusted Advisor check result, including the alert status, last refresh, and number of resources examined.</p>

#### `TrustedAdvisorCheckSummaryList`

``` purescript
newtype TrustedAdvisorCheckSummaryList
  = TrustedAdvisorCheckSummaryList (Array TrustedAdvisorCheckSummary)
```

#### `TrustedAdvisorCostOptimizingSummary`

``` purescript
newtype TrustedAdvisorCostOptimizingSummary
  = TrustedAdvisorCostOptimizingSummary { "EstimatedMonthlySavings'" :: Number, "EstimatedPercentMonthlySavings'" :: Number }
```

<p>The estimated cost savings that might be realized if the recommended actions are taken.</p>

#### `TrustedAdvisorResourceDetail`

``` purescript
newtype TrustedAdvisorResourceDetail
  = TrustedAdvisorResourceDetail { "Status'" :: String, "Region'" :: NullOrUndefined (String), "ResourceId'" :: String, "IsSuppressed'" :: NullOrUndefined (Boolean), "Metadata'" :: StringList }
```

<p>Contains information about a resource identified by a Trusted Advisor check.</p>

#### `TrustedAdvisorResourceDetailList`

``` purescript
newtype TrustedAdvisorResourceDetailList
  = TrustedAdvisorResourceDetailList (Array TrustedAdvisorResourceDetail)
```

#### `TrustedAdvisorResourcesSummary`

``` purescript
newtype TrustedAdvisorResourcesSummary
  = TrustedAdvisorResourcesSummary { "ResourcesProcessed'" :: Number, "ResourcesFlagged'" :: Number, "ResourcesIgnored'" :: Number, "ResourcesSuppressed'" :: Number }
```

<p>Details about AWS resources that were analyzed in a call to Trusted Advisor <a>DescribeTrustedAdvisorCheckSummaries</a>. </p>


