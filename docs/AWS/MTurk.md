## Module AWS.MTurk

<fullname>Amazon Mechanical Turk API Reference</fullname>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `acceptQualificationRequest`

``` purescript
acceptQualificationRequest :: forall eff. AcceptQualificationRequestRequest -> Aff (err :: RequestError | eff) AcceptQualificationRequestResponse
```

<p> The <code>AcceptQualificationRequest</code> operation approves a Worker's request for a Qualification. </p> <p> Only the owner of the Qualification type can grant a Qualification request for that type. </p> <p> A successful request for the <code>AcceptQualificationRequest</code> operation returns with no errors and an empty body. </p>

#### `approveAssignment`

``` purescript
approveAssignment :: forall eff. ApproveAssignmentRequest -> Aff (err :: RequestError | eff) ApproveAssignmentResponse
```

<p> The <code>ApproveAssignment</code> operation approves the results of a completed assignment. </p> <p> Approving an assignment initiates two payments from the Requester's Amazon.com account </p> <ul> <li> <p> The Worker who submitted the results is paid the reward specified in the HIT. </p> </li> <li> <p> Amazon Mechanical Turk fees are debited. </p> </li> </ul> <p> If the Requester's account does not have adequate funds for these payments, the call to ApproveAssignment returns an exception, and the approval is not processed. You can include an optional feedback message with the approval, which the Worker can see in the Status section of the web site. </p> <p> You can also call this operation for assignments that were previous rejected and approve them by explicitly overriding the previous rejection. This only works on rejected assignments that were submitted within the previous 30 days and only if the assignment's related HIT has not been deleted. </p>

#### `associateQualificationWithWorker`

``` purescript
associateQualificationWithWorker :: forall eff. AssociateQualificationWithWorkerRequest -> Aff (err :: RequestError | eff) AssociateQualificationWithWorkerResponse
```

<p> The <code>AssociateQualificationWithWorker</code> operation gives a Worker a Qualification. <code>AssociateQualificationWithWorker</code> does not require that the Worker submit a Qualification request. It gives the Qualification directly to the Worker. </p> <p> You can only assign a Qualification of a Qualification type that you created (using the <code>CreateQualificationType</code> operation). </p> <note> <p> Note: <code>AssociateQualificationWithWorker</code> does not affect any pending Qualification requests for the Qualification by the Worker. If you assign a Qualification to a Worker, then later grant a Qualification request made by the Worker, the granting of the request may modify the Qualification score. To resolve a pending Qualification request without affecting the Qualification the Worker already has, reject the request with the <code>RejectQualificationRequest</code> operation. </p> </note>

#### `createAdditionalAssignmentsForHIT`

``` purescript
createAdditionalAssignmentsForHIT :: forall eff. CreateAdditionalAssignmentsForHITRequest -> Aff (err :: RequestError | eff) CreateAdditionalAssignmentsForHITResponse
```

<p> The <code>CreateAdditionalAssignmentsForHIT</code> operation increases the maximum number of assignments of an existing HIT. </p> <p> To extend the maximum number of assignments, specify the number of additional assignments.</p> <note> <ul> <li> <p>HITs created with fewer than 10 assignments cannot be extended to have 10 or more assignments. Attempting to add assignments in a way that brings the total number of assignments for a HIT from fewer than 10 assignments to 10 or more assignments will result in an <code>AWS.MechanicalTurk.InvalidMaximumAssignmentsIncrease</code> exception.</p> </li> <li> <p>HITs that were created before July 22, 2015 cannot be extended. Attempting to extend HITs that were created before July 22, 2015 will result in an <code>AWS.MechanicalTurk.HITTooOldForExtension</code> exception. </p> </li> </ul> </note>

#### `createHIT`

``` purescript
createHIT :: forall eff. CreateHITRequest -> Aff (err :: RequestError | eff) CreateHITResponse
```

<p>The <code>CreateHIT</code> operation creates a new Human Intelligence Task (HIT). The new HIT is made available for Workers to find and accept on the Amazon Mechanical Turk website. </p> <p> This operation allows you to specify a new HIT by passing in values for the properties of the HIT, such as its title, reward amount and number of assignments. When you pass these values to <code>CreateHIT</code>, a new HIT is created for you, with a new <code>HITTypeID</code>. The HITTypeID can be used to create additional HITs in the future without needing to specify common parameters such as the title, description and reward amount each time.</p> <p> An alternative way to create HITs is to first generate a HITTypeID using the <code>CreateHITType</code> operation and then call the <code>CreateHITWithHITType</code> operation. This is the recommended best practice for Requesters who are creating large numbers of HITs. </p> <p>CreateHIT also supports several ways to provide question data: by providing a value for the <code>Question</code> parameter that fully specifies the contents of the HIT, or by providing a <code>HitLayoutId</code> and associated <code>HitLayoutParameters</code>. </p> <note> <p> If a HIT is created with 10 or more maximum assignments, there is an additional fee. For more information, see <a href="https://requester.mturk.com/pricing">Amazon Mechanical Turk Pricing</a>.</p> </note>

#### `createHITType`

``` purescript
createHITType :: forall eff. CreateHITTypeRequest -> Aff (err :: RequestError | eff) CreateHITTypeResponse
```

<p> The <code>CreateHITType</code> operation creates a new HIT type. This operation allows you to define a standard set of HIT properties to use when creating HITs. If you register a HIT type with values that match an existing HIT type, the HIT type ID of the existing type will be returned. </p>

#### `createHITWithHITType`

``` purescript
createHITWithHITType :: forall eff. CreateHITWithHITTypeRequest -> Aff (err :: RequestError | eff) CreateHITWithHITTypeResponse
```

<p> The <code>CreateHITWithHITType</code> operation creates a new Human Intelligence Task (HIT) using an existing HITTypeID generated by the <code>CreateHITType</code> operation. </p> <p> This is an alternative way to create HITs from the <code>CreateHIT</code> operation. This is the recommended best practice for Requesters who are creating large numbers of HITs. </p> <p>CreateHITWithHITType also supports several ways to provide question data: by providing a value for the <code>Question</code> parameter that fully specifies the contents of the HIT, or by providing a <code>HitLayoutId</code> and associated <code>HitLayoutParameters</code>. </p> <note> <p> If a HIT is created with 10 or more maximum assignments, there is an additional fee. For more information, see <a href="https://requester.mturk.com/pricing">Amazon Mechanical Turk Pricing</a>. </p> </note>

#### `createQualificationType`

``` purescript
createQualificationType :: forall eff. CreateQualificationTypeRequest -> Aff (err :: RequestError | eff) CreateQualificationTypeResponse
```

<p> The <code>CreateQualificationType</code> operation creates a new Qualification type, which is represented by a <code>QualificationType</code> data structure. </p>

#### `createWorkerBlock`

``` purescript
createWorkerBlock :: forall eff. CreateWorkerBlockRequest -> Aff (err :: RequestError | eff) CreateWorkerBlockResponse
```

<p>The <code>CreateWorkerBlock</code> operation allows you to prevent a Worker from working on your HITs. For example, you can block a Worker who is producing poor quality work. You can block up to 100,000 Workers.</p>

#### `deleteHIT`

``` purescript
deleteHIT :: forall eff. DeleteHITRequest -> Aff (err :: RequestError | eff) DeleteHITResponse
```

<p> The <code>DeleteHIT</code> operation is used to delete HIT that is no longer needed. Only the Requester who created the HIT can delete it. </p> <p> You can only dispose of HITs that are in the <code>Reviewable</code> state, with all of their submitted assignments already either approved or rejected. If you call the DeleteHIT operation on a HIT that is not in the <code>Reviewable</code> state (for example, that has not expired, or still has active assignments), or on a HIT that is Reviewable but without all of its submitted assignments already approved or rejected, the service will return an error. </p> <note> <ul> <li> <p> HITs are automatically disposed of after 120 days. </p> </li> <li> <p> After you dispose of a HIT, you can no longer approve the HIT's rejected assignments. </p> </li> <li> <p> Disposed HITs are not returned in results for the ListHITs operation. </p> </li> <li> <p> Disposing HITs can improve the performance of operations such as ListReviewableHITs and ListHITs. </p> </li> </ul> </note>

#### `deleteQualificationType`

``` purescript
deleteQualificationType :: forall eff. DeleteQualificationTypeRequest -> Aff (err :: RequestError | eff) DeleteQualificationTypeResponse
```

<p> The <code>DeleteQualificationType</code> deletes a Qualification type and deletes any HIT types that are associated with the Qualification type. </p> <p>This operation does not revoke Qualifications already assigned to Workers because the Qualifications might be needed for active HITs. If there are any pending requests for the Qualification type, Amazon Mechanical Turk rejects those requests. After you delete a Qualification type, you can no longer use it to create HITs or HIT types.</p> <note> <p>DeleteQualificationType must wait for all the HITs that use the deleted Qualification type to be deleted before completing. It may take up to 48 hours before DeleteQualificationType completes and the unique name of the Qualification type is available for reuse with CreateQualificationType.</p> </note>

#### `deleteWorkerBlock`

``` purescript
deleteWorkerBlock :: forall eff. DeleteWorkerBlockRequest -> Aff (err :: RequestError | eff) DeleteWorkerBlockResponse
```

<p>The <code>DeleteWorkerBlock</code> operation allows you to reinstate a blocked Worker to work on your HITs. This operation reverses the effects of the CreateWorkerBlock operation. You need the Worker ID to use this operation. If the Worker ID is missing or invalid, this operation fails and returns the message “WorkerId is invalid.” If the specified Worker is not blocked, this operation returns successfully.</p>

#### `disassociateQualificationFromWorker`

``` purescript
disassociateQualificationFromWorker :: forall eff. DisassociateQualificationFromWorkerRequest -> Aff (err :: RequestError | eff) DisassociateQualificationFromWorkerResponse
```

<p> The <code>DisassociateQualificationFromWorker</code> revokes a previously granted Qualification from a user. </p> <p> You can provide a text message explaining why the Qualification was revoked. The user who had the Qualification can see this message. </p>

#### `getAccountBalance`

``` purescript
getAccountBalance :: forall eff. GetAccountBalanceRequest -> Aff (err :: RequestError | eff) GetAccountBalanceResponse
```

<p>The <code>GetAccountBalance</code> operation retrieves the amount of money in your Amazon Mechanical Turk account.</p>

#### `getAssignment`

``` purescript
getAssignment :: forall eff. GetAssignmentRequest -> Aff (err :: RequestError | eff) GetAssignmentResponse
```

<p> The <code>GetAssignment</code> operation retrieves the details of the specified Assignment. </p>

#### `getFileUploadURL`

``` purescript
getFileUploadURL :: forall eff. GetFileUploadURLRequest -> Aff (err :: RequestError | eff) GetFileUploadURLResponse
```

<p> The <code>GetFileUploadURL</code> operation generates and returns a temporary URL. You use the temporary URL to retrieve a file uploaded by a Worker as an answer to a FileUploadAnswer question for a HIT. The temporary URL is generated the instant the GetFileUploadURL operation is called, and is valid for 60 seconds. You can get a temporary file upload URL any time until the HIT is disposed. After the HIT is disposed, any uploaded files are deleted, and cannot be retrieved. Pending Deprecation on December 12, 2017. The Answer Specification structure will no longer support the <code>FileUploadAnswer</code> element to be used for the QuestionForm data structure. Instead, we recommend that Requesters who want to create HITs asking Workers to upload files to use Amazon S3. </p>

#### `getHIT`

``` purescript
getHIT :: forall eff. GetHITRequest -> Aff (err :: RequestError | eff) GetHITResponse
```

<p> The <code>GetHIT</code> operation retrieves the details of the specified HIT. </p>

#### `getQualificationScore`

``` purescript
getQualificationScore :: forall eff. GetQualificationScoreRequest -> Aff (err :: RequestError | eff) GetQualificationScoreResponse
```

<p> The <code>GetQualificationScore</code> operation returns the value of a Worker's Qualification for a given Qualification type. </p> <p> To get a Worker's Qualification, you must know the Worker's ID. The Worker's ID is included in the assignment data returned by the <code>ListAssignmentsForHIT</code> operation. </p> <p>Only the owner of a Qualification type can query the value of a Worker's Qualification of that type.</p>

#### `getQualificationType`

``` purescript
getQualificationType :: forall eff. GetQualificationTypeRequest -> Aff (err :: RequestError | eff) GetQualificationTypeResponse
```

<p> The <code>GetQualificationType</code>operation retrieves information about a Qualification type using its ID. </p>

#### `listAssignmentsForHIT`

``` purescript
listAssignmentsForHIT :: forall eff. ListAssignmentsForHITRequest -> Aff (err :: RequestError | eff) ListAssignmentsForHITResponse
```

<p> The <code>ListAssignmentsForHIT</code> operation retrieves completed assignments for a HIT. You can use this operation to retrieve the results for a HIT. </p> <p> You can get assignments for a HIT at any time, even if the HIT is not yet Reviewable. If a HIT requested multiple assignments, and has received some results but has not yet become Reviewable, you can still retrieve the partial results with this operation. </p> <p> Use the AssignmentStatus parameter to control which set of assignments for a HIT are returned. The ListAssignmentsForHIT operation can return submitted assignments awaiting approval, or it can return assignments that have already been approved or rejected. You can set AssignmentStatus=Approved,Rejected to get assignments that have already been approved and rejected together in one result set. </p> <p> Only the Requester who created the HIT can retrieve the assignments for that HIT. </p> <p> Results are sorted and divided into numbered pages and the operation returns a single page of results. You can use the parameters of the operation to control sorting and pagination. </p>

#### `listBonusPayments`

``` purescript
listBonusPayments :: forall eff. ListBonusPaymentsRequest -> Aff (err :: RequestError | eff) ListBonusPaymentsResponse
```

<p> The <code>ListBonusPayments</code> operation retrieves the amounts of bonuses you have paid to Workers for a given HIT or assignment. </p>

#### `listHITs`

``` purescript
listHITs :: forall eff. ListHITsRequest -> Aff (err :: RequestError | eff) ListHITsResponse
```

<p> The <code>ListHITs</code> operation returns all of a Requester's HITs. The operation returns HITs of any status, except for HITs that have been deleted of with the DeleteHIT operation or that have been auto-deleted. </p>

#### `listHITsForQualificationType`

``` purescript
listHITsForQualificationType :: forall eff. ListHITsForQualificationTypeRequest -> Aff (err :: RequestError | eff) ListHITsForQualificationTypeResponse
```

<p> The <code>ListHITsForQualificationType</code> operation returns the HITs that use the given Qualification type for a Qualification requirement. The operation returns HITs of any status, except for HITs that have been deleted with the <code>DeleteHIT</code> operation or that have been auto-deleted. </p>

#### `listQualificationRequests`

``` purescript
listQualificationRequests :: forall eff. ListQualificationRequestsRequest -> Aff (err :: RequestError | eff) ListQualificationRequestsResponse
```

<p> The <code>ListQualificationRequests</code> operation retrieves requests for Qualifications of a particular Qualification type. The owner of the Qualification type calls this operation to poll for pending requests, and accepts them using the AcceptQualification operation. </p>

#### `listQualificationTypes`

``` purescript
listQualificationTypes :: forall eff. ListQualificationTypesRequest -> Aff (err :: RequestError | eff) ListQualificationTypesResponse
```

<p> The <code>ListQualificationRequests</code> operation retrieves requests for Qualifications of a particular Qualification type. The owner of the Qualification type calls this operation to poll for pending requests, and accepts them using the AcceptQualification operation. </p>

#### `listReviewPolicyResultsForHIT`

``` purescript
listReviewPolicyResultsForHIT :: forall eff. ListReviewPolicyResultsForHITRequest -> Aff (err :: RequestError | eff) ListReviewPolicyResultsForHITResponse
```

<p> The <code>ListReviewPolicyResultsForHIT</code> operation retrieves the computed results and the actions taken in the course of executing your Review Policies for a given HIT. For information about how to specify Review Policies when you call CreateHIT, see Review Policies. The ListReviewPolicyResultsForHIT operation can return results for both Assignment-level and HIT-level review results. </p>

#### `listReviewableHITs`

``` purescript
listReviewableHITs :: forall eff. ListReviewableHITsRequest -> Aff (err :: RequestError | eff) ListReviewableHITsResponse
```

<p> The <code>ListReviewableHITs</code> operation retrieves the HITs with Status equal to Reviewable or Status equal to Reviewing that belong to the Requester calling the operation. </p>

#### `listWorkerBlocks`

``` purescript
listWorkerBlocks :: forall eff. ListWorkerBlocksRequest -> Aff (err :: RequestError | eff) ListWorkerBlocksResponse
```

<p>The <code>ListWorkersBlocks</code> operation retrieves a list of Workers who are blocked from working on your HITs.</p>

#### `listWorkersWithQualificationType`

``` purescript
listWorkersWithQualificationType :: forall eff. ListWorkersWithQualificationTypeRequest -> Aff (err :: RequestError | eff) ListWorkersWithQualificationTypeResponse
```

<p> The <code>ListWorkersWithQualificationType</code> operation returns all of the Workers that have been associated with a given Qualification type. </p>

#### `notifyWorkers`

``` purescript
notifyWorkers :: forall eff. NotifyWorkersRequest -> Aff (err :: RequestError | eff) NotifyWorkersResponse
```

<p> The <code>NotifyWorkers</code> operation sends an email to one or more Workers that you specify with the Worker ID. You can specify up to 100 Worker IDs to send the same message with a single call to the NotifyWorkers operation. The NotifyWorkers operation will send a notification email to a Worker only if you have previously approved or rejected work from the Worker. </p>

#### `rejectAssignment`

``` purescript
rejectAssignment :: forall eff. RejectAssignmentRequest -> Aff (err :: RequestError | eff) RejectAssignmentResponse
```

<p> The <code>RejectAssignment</code> operation rejects the results of a completed assignment. </p> <p> You can include an optional feedback message with the rejection, which the Worker can see in the Status section of the web site. When you include a feedback message with the rejection, it helps the Worker understand why the assignment was rejected, and can improve the quality of the results the Worker submits in the future. </p> <p> Only the Requester who created the HIT can reject an assignment for the HIT. </p>

#### `rejectQualificationRequest`

``` purescript
rejectQualificationRequest :: forall eff. RejectQualificationRequestRequest -> Aff (err :: RequestError | eff) RejectQualificationRequestResponse
```

<p> The <code>RejectQualificationRequest</code> operation rejects a user's request for a Qualification. </p> <p> You can provide a text message explaining why the request was rejected. The Worker who made the request can see this message.</p>

#### `sendBonus`

``` purescript
sendBonus :: forall eff. SendBonusRequest -> Aff (err :: RequestError | eff) SendBonusResponse
```

<p> The <code>SendBonus</code> operation issues a payment of money from your account to a Worker. This payment happens separately from the reward you pay to the Worker when you approve the Worker's assignment. The SendBonus operation requires the Worker's ID and the assignment ID as parameters to initiate payment of the bonus. You must include a message that explains the reason for the bonus payment, as the Worker may not be expecting the payment. Amazon Mechanical Turk collects a fee for bonus payments, similar to the HIT listing fee. This operation fails if your account does not have enough funds to pay for both the bonus and the fees. </p>

#### `sendTestEventNotification`

``` purescript
sendTestEventNotification :: forall eff. SendTestEventNotificationRequest -> Aff (err :: RequestError | eff) SendTestEventNotificationResponse
```

<p> The <code>SendTestEventNotification</code> operation causes Amazon Mechanical Turk to send a notification message as if a HIT event occurred, according to the provided notification specification. This allows you to test notifications without setting up notifications for a real HIT type and trying to trigger them using the website. When you call this operation, the service attempts to send the test notification immediately. </p>

#### `updateExpirationForHIT`

``` purescript
updateExpirationForHIT :: forall eff. UpdateExpirationForHITRequest -> Aff (err :: RequestError | eff) UpdateExpirationForHITResponse
```

<p> The <code>UpdateExpirationForHIT</code> operation allows you update the expiration time of a HIT. If you update it to a time in the past, the HIT will be immediately expired. </p>

#### `updateHITReviewStatus`

``` purescript
updateHITReviewStatus :: forall eff. UpdateHITReviewStatusRequest -> Aff (err :: RequestError | eff) UpdateHITReviewStatusResponse
```

<p> The <code>UpdateHITReviewStatus</code> operation updates the status of a HIT. If the status is Reviewable, this operation can update the status to Reviewing, or it can revert a Reviewing HIT back to the Reviewable status. </p>

#### `updateHITTypeOfHIT`

``` purescript
updateHITTypeOfHIT :: forall eff. UpdateHITTypeOfHITRequest -> Aff (err :: RequestError | eff) UpdateHITTypeOfHITResponse
```

<p> The <code>UpdateHITTypeOfHIT</code> operation allows you to change the HITType properties of a HIT. This operation disassociates the HIT from its old HITType properties and associates it with the new HITType properties. The HIT takes on the properties of the new HITType in place of the old ones. </p>

#### `updateNotificationSettings`

``` purescript
updateNotificationSettings :: forall eff. UpdateNotificationSettingsRequest -> Aff (err :: RequestError | eff) UpdateNotificationSettingsResponse
```

<p> The <code>UpdateNotificationSettings</code> operation creates, updates, disables or re-enables notifications for a HIT type. If you call the UpdateNotificationSettings operation for a HIT type that already has a notification specification, the operation replaces the old specification with a new one. You can call the UpdateNotificationSettings operation to enable or disable notifications for the HIT type, without having to modify the notification specification itself by providing updates to the Active status without specifying a new notification specification. To change the Active status of a HIT type's notifications, the HIT type must already have a notification specification, or one must be provided in the same call to <code>UpdateNotificationSettings</code>. </p>

#### `updateQualificationType`

``` purescript
updateQualificationType :: forall eff. UpdateQualificationTypeRequest -> Aff (err :: RequestError | eff) UpdateQualificationTypeResponse
```

<p> The <code>UpdateQualificationType</code> operation modifies the attributes of an existing Qualification type, which is represented by a QualificationType data structure. Only the owner of a Qualification type can modify its attributes. </p> <p> Most attributes of a Qualification type can be changed after the type has been created. However, the Name and Keywords fields cannot be modified. The RetryDelayInSeconds parameter can be modified or added to change the delay or to enable retries, but RetryDelayInSeconds cannot be used to disable retries. </p> <p> You can use this operation to update the test for a Qualification type. The test is updated based on the values specified for the Test, TestDurationInSeconds and AnswerKey parameters. All three parameters specify the updated test. If you are updating the test for a type, you must specify the Test and TestDurationInSeconds parameters. The AnswerKey parameter is optional; omitting it specifies that the updated test does not have an answer key. </p> <p> If you omit the Test parameter, the test for the Qualification type is unchanged. There is no way to remove a test from a Qualification type that has one. If the type already has a test, you cannot update it to be AutoGranted. If the Qualification type does not have a test and one is provided by an update, the type will henceforth have a test. </p> <p> If you want to update the test duration or answer key for an existing test without changing the questions, you must specify a Test parameter with the original questions, along with the updated values. </p> <p> If you provide an updated Test but no AnswerKey, the new test will not have an answer key. Requests for such Qualifications must be granted manually. </p> <p> You can also update the AutoGranted and AutoGrantedValue attributes of the Qualification type.</p>

#### `AcceptQualificationRequestRequest`

``` purescript
newtype AcceptQualificationRequestRequest
  = AcceptQualificationRequestRequest { "QualificationRequestId" :: String, "IntegerValue" :: NullOrUndefined (Int) }
```

#### `AcceptQualificationRequestResponse`

``` purescript
newtype AcceptQualificationRequestResponse
  = AcceptQualificationRequestResponse {  }
```

#### `ApproveAssignmentRequest`

``` purescript
newtype ApproveAssignmentRequest
  = ApproveAssignmentRequest { "AssignmentId" :: EntityId, "RequesterFeedback" :: NullOrUndefined (String), "OverrideRejection" :: NullOrUndefined (Boolean) }
```

#### `ApproveAssignmentResponse`

``` purescript
newtype ApproveAssignmentResponse
  = ApproveAssignmentResponse {  }
```

#### `Assignment`

``` purescript
newtype Assignment
  = Assignment { "AssignmentId" :: NullOrUndefined (EntityId), "WorkerId" :: NullOrUndefined (CustomerId), "HITId" :: NullOrUndefined (EntityId), "AssignmentStatus" :: NullOrUndefined (AssignmentStatus), "AutoApprovalTime" :: NullOrUndefined (Number), "AcceptTime" :: NullOrUndefined (Number), "SubmitTime" :: NullOrUndefined (Number), "ApprovalTime" :: NullOrUndefined (Number), "RejectionTime" :: NullOrUndefined (Number), "Deadline" :: NullOrUndefined (Number), "Answer" :: NullOrUndefined (String), "RequesterFeedback" :: NullOrUndefined (String) }
```

<p> The Assignment data structure represents a single assignment of a HIT to a Worker. The assignment tracks the Worker's efforts to complete the HIT, and contains the results for later retrieval. </p>

#### `AssignmentList`

``` purescript
newtype AssignmentList
  = AssignmentList (Array Assignment)
```

#### `AssignmentStatus`

``` purescript
newtype AssignmentStatus
  = AssignmentStatus String
```

#### `AssignmentStatusList`

``` purescript
newtype AssignmentStatusList
  = AssignmentStatusList (Array AssignmentStatus)
```

#### `AssociateQualificationWithWorkerRequest`

``` purescript
newtype AssociateQualificationWithWorkerRequest
  = AssociateQualificationWithWorkerRequest { "QualificationTypeId" :: EntityId, "WorkerId" :: CustomerId, "IntegerValue" :: NullOrUndefined (Int), "SendNotification" :: NullOrUndefined (Boolean) }
```

#### `AssociateQualificationWithWorkerResponse`

``` purescript
newtype AssociateQualificationWithWorkerResponse
  = AssociateQualificationWithWorkerResponse {  }
```

#### `BonusPayment`

``` purescript
newtype BonusPayment
  = BonusPayment { "WorkerId" :: NullOrUndefined (CustomerId), "BonusAmount" :: NullOrUndefined (CurrencyAmount), "AssignmentId" :: NullOrUndefined (EntityId), "Reason" :: NullOrUndefined (String), "GrantTime" :: NullOrUndefined (Number) }
```

<p>An object representing a Bonus payment paid to a Worker.</p>

#### `BonusPaymentList`

``` purescript
newtype BonusPaymentList
  = BonusPaymentList (Array BonusPayment)
```

#### `Comparator`

``` purescript
newtype Comparator
  = Comparator String
```

#### `CountryParameters`

``` purescript
newtype CountryParameters
  = CountryParameters String
```

#### `CreateAdditionalAssignmentsForHITRequest`

``` purescript
newtype CreateAdditionalAssignmentsForHITRequest
  = CreateAdditionalAssignmentsForHITRequest { "HITId" :: EntityId, "NumberOfAdditionalAssignments" :: Int, "UniqueRequestToken" :: NullOrUndefined (IdempotencyToken) }
```

#### `CreateAdditionalAssignmentsForHITResponse`

``` purescript
newtype CreateAdditionalAssignmentsForHITResponse
  = CreateAdditionalAssignmentsForHITResponse {  }
```

#### `CreateHITRequest`

``` purescript
newtype CreateHITRequest
  = CreateHITRequest { "MaxAssignments" :: NullOrUndefined (Int), "AutoApprovalDelayInSeconds" :: NullOrUndefined (Number), "LifetimeInSeconds" :: Number, "AssignmentDurationInSeconds" :: Number, "Reward" :: CurrencyAmount, "Title" :: String, "Keywords" :: NullOrUndefined (String), "Description" :: String, "Question" :: NullOrUndefined (String), "RequesterAnnotation" :: NullOrUndefined (String), "QualificationRequirements" :: NullOrUndefined (QualificationRequirementList), "UniqueRequestToken" :: NullOrUndefined (IdempotencyToken), "AssignmentReviewPolicy" :: NullOrUndefined (ReviewPolicy), "HITReviewPolicy" :: NullOrUndefined (ReviewPolicy), "HITLayoutId" :: NullOrUndefined (EntityId), "HITLayoutParameters" :: NullOrUndefined (HITLayoutParameterList) }
```

#### `CreateHITResponse`

``` purescript
newtype CreateHITResponse
  = CreateHITResponse { "HIT" :: NullOrUndefined (HIT) }
```

#### `CreateHITTypeRequest`

``` purescript
newtype CreateHITTypeRequest
  = CreateHITTypeRequest { "AutoApprovalDelayInSeconds" :: NullOrUndefined (Number), "AssignmentDurationInSeconds" :: Number, "Reward" :: CurrencyAmount, "Title" :: String, "Keywords" :: NullOrUndefined (String), "Description" :: String, "QualificationRequirements" :: NullOrUndefined (QualificationRequirementList) }
```

#### `CreateHITTypeResponse`

``` purescript
newtype CreateHITTypeResponse
  = CreateHITTypeResponse { "HITTypeId" :: NullOrUndefined (EntityId) }
```

#### `CreateHITWithHITTypeRequest`

``` purescript
newtype CreateHITWithHITTypeRequest
  = CreateHITWithHITTypeRequest { "HITTypeId" :: EntityId, "MaxAssignments" :: NullOrUndefined (Int), "LifetimeInSeconds" :: Number, "Question" :: NullOrUndefined (String), "RequesterAnnotation" :: NullOrUndefined (String), "UniqueRequestToken" :: NullOrUndefined (IdempotencyToken), "AssignmentReviewPolicy" :: NullOrUndefined (ReviewPolicy), "HITReviewPolicy" :: NullOrUndefined (ReviewPolicy), "HITLayoutId" :: NullOrUndefined (EntityId), "HITLayoutParameters" :: NullOrUndefined (HITLayoutParameterList) }
```

#### `CreateHITWithHITTypeResponse`

``` purescript
newtype CreateHITWithHITTypeResponse
  = CreateHITWithHITTypeResponse { "HIT" :: NullOrUndefined (HIT) }
```

#### `CreateQualificationTypeRequest`

``` purescript
newtype CreateQualificationTypeRequest
  = CreateQualificationTypeRequest { "Name" :: String, "Keywords" :: NullOrUndefined (String), "Description" :: String, "QualificationTypeStatus" :: QualificationTypeStatus, "RetryDelayInSeconds" :: NullOrUndefined (Number), "Test" :: NullOrUndefined (String), "AnswerKey" :: NullOrUndefined (String), "TestDurationInSeconds" :: NullOrUndefined (Number), "AutoGranted" :: NullOrUndefined (Boolean), "AutoGrantedValue" :: NullOrUndefined (Int) }
```

#### `CreateQualificationTypeResponse`

``` purescript
newtype CreateQualificationTypeResponse
  = CreateQualificationTypeResponse { "QualificationType" :: NullOrUndefined (QualificationType) }
```

#### `CreateWorkerBlockRequest`

``` purescript
newtype CreateWorkerBlockRequest
  = CreateWorkerBlockRequest { "WorkerId" :: CustomerId, "Reason" :: String }
```

#### `CreateWorkerBlockResponse`

``` purescript
newtype CreateWorkerBlockResponse
  = CreateWorkerBlockResponse {  }
```

#### `CurrencyAmount`

``` purescript
newtype CurrencyAmount
  = CurrencyAmount String
```

<p>A string representing a currency amount.</p>

#### `CustomerId`

``` purescript
newtype CustomerId
  = CustomerId String
```

#### `CustomerIdList`

``` purescript
newtype CustomerIdList
  = CustomerIdList (Array CustomerId)
```

#### `DeleteHITRequest`

``` purescript
newtype DeleteHITRequest
  = DeleteHITRequest { "HITId" :: EntityId }
```

#### `DeleteHITResponse`

``` purescript
newtype DeleteHITResponse
  = DeleteHITResponse {  }
```

#### `DeleteQualificationTypeRequest`

``` purescript
newtype DeleteQualificationTypeRequest
  = DeleteQualificationTypeRequest { "QualificationTypeId" :: EntityId }
```

#### `DeleteQualificationTypeResponse`

``` purescript
newtype DeleteQualificationTypeResponse
  = DeleteQualificationTypeResponse {  }
```

#### `DeleteWorkerBlockRequest`

``` purescript
newtype DeleteWorkerBlockRequest
  = DeleteWorkerBlockRequest { "WorkerId" :: CustomerId, "Reason" :: NullOrUndefined (String) }
```

#### `DeleteWorkerBlockResponse`

``` purescript
newtype DeleteWorkerBlockResponse
  = DeleteWorkerBlockResponse {  }
```

#### `DisassociateQualificationFromWorkerRequest`

``` purescript
newtype DisassociateQualificationFromWorkerRequest
  = DisassociateQualificationFromWorkerRequest { "WorkerId" :: CustomerId, "QualificationTypeId" :: EntityId, "Reason" :: NullOrUndefined (String) }
```

#### `DisassociateQualificationFromWorkerResponse`

``` purescript
newtype DisassociateQualificationFromWorkerResponse
  = DisassociateQualificationFromWorkerResponse {  }
```

#### `EntityId`

``` purescript
newtype EntityId
  = EntityId String
```

#### `EventType`

``` purescript
newtype EventType
  = EventType String
```

#### `EventTypeList`

``` purescript
newtype EventTypeList
  = EventTypeList (Array EventType)
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

#### `GetAccountBalanceRequest`

``` purescript
newtype GetAccountBalanceRequest
  = GetAccountBalanceRequest {  }
```

#### `GetAccountBalanceResponse`

``` purescript
newtype GetAccountBalanceResponse
  = GetAccountBalanceResponse { "AvailableBalance" :: NullOrUndefined (CurrencyAmount), "OnHoldBalance" :: NullOrUndefined (CurrencyAmount) }
```

#### `GetAssignmentRequest`

``` purescript
newtype GetAssignmentRequest
  = GetAssignmentRequest { "AssignmentId" :: EntityId }
```

#### `GetAssignmentResponse`

``` purescript
newtype GetAssignmentResponse
  = GetAssignmentResponse { "Assignment" :: NullOrUndefined (Assignment), "HIT" :: NullOrUndefined (HIT) }
```

#### `GetFileUploadURLRequest`

``` purescript
newtype GetFileUploadURLRequest
  = GetFileUploadURLRequest { "AssignmentId" :: EntityId, "QuestionIdentifier" :: String }
```

#### `GetFileUploadURLResponse`

``` purescript
newtype GetFileUploadURLResponse
  = GetFileUploadURLResponse { "FileUploadURL" :: NullOrUndefined (String) }
```

#### `GetHITRequest`

``` purescript
newtype GetHITRequest
  = GetHITRequest { "HITId" :: EntityId }
```

#### `GetHITResponse`

``` purescript
newtype GetHITResponse
  = GetHITResponse { "HIT" :: NullOrUndefined (HIT) }
```

#### `GetQualificationScoreRequest`

``` purescript
newtype GetQualificationScoreRequest
  = GetQualificationScoreRequest { "QualificationTypeId" :: EntityId, "WorkerId" :: CustomerId }
```

#### `GetQualificationScoreResponse`

``` purescript
newtype GetQualificationScoreResponse
  = GetQualificationScoreResponse { "Qualification" :: NullOrUndefined (Qualification) }
```

#### `GetQualificationTypeRequest`

``` purescript
newtype GetQualificationTypeRequest
  = GetQualificationTypeRequest { "QualificationTypeId" :: EntityId }
```

#### `GetQualificationTypeResponse`

``` purescript
newtype GetQualificationTypeResponse
  = GetQualificationTypeResponse { "QualificationType" :: NullOrUndefined (QualificationType) }
```

#### `HIT`

``` purescript
newtype HIT
  = HIT { "HITId" :: NullOrUndefined (EntityId), "HITTypeId" :: NullOrUndefined (EntityId), "HITGroupId" :: NullOrUndefined (EntityId), "HITLayoutId" :: NullOrUndefined (EntityId), "CreationTime" :: NullOrUndefined (Number), "Title" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Question" :: NullOrUndefined (String), "Keywords" :: NullOrUndefined (String), "HITStatus" :: NullOrUndefined (HITStatus), "MaxAssignments" :: NullOrUndefined (Int), "Reward" :: NullOrUndefined (CurrencyAmount), "AutoApprovalDelayInSeconds" :: NullOrUndefined (Number), "Expiration" :: NullOrUndefined (Number), "AssignmentDurationInSeconds" :: NullOrUndefined (Number), "RequesterAnnotation" :: NullOrUndefined (String), "QualificationRequirements" :: NullOrUndefined (QualificationRequirementList), "HITReviewStatus" :: NullOrUndefined (HITReviewStatus), "NumberOfAssignmentsPending" :: NullOrUndefined (Int), "NumberOfAssignmentsAvailable" :: NullOrUndefined (Int), "NumberOfAssignmentsCompleted" :: NullOrUndefined (Int) }
```

<p> The HIT data structure represents a single HIT, including all the information necessary for a Worker to accept and complete the HIT.</p>

#### `HITLayoutParameter`

``` purescript
newtype HITLayoutParameter
  = HITLayoutParameter { "Name" :: String, "Value" :: String }
```

<p> The HITLayoutParameter data structure defines parameter values used with a HITLayout. A HITLayout is a reusable Amazon Mechanical Turk project template used to provide Human Intelligence Task (HIT) question data for CreateHIT. </p>

#### `HITLayoutParameterList`

``` purescript
newtype HITLayoutParameterList
  = HITLayoutParameterList (Array HITLayoutParameter)
```

#### `HITList`

``` purescript
newtype HITList
  = HITList (Array HIT)
```

#### `HITReviewStatus`

``` purescript
newtype HITReviewStatus
  = HITReviewStatus String
```

#### `HITStatus`

``` purescript
newtype HITStatus
  = HITStatus String
```

#### `IdempotencyToken`

``` purescript
newtype IdempotencyToken
  = IdempotencyToken String
```

#### `IntegerList`

``` purescript
newtype IntegerList
  = IntegerList (Array Int)
```

#### `ListAssignmentsForHITRequest`

``` purescript
newtype ListAssignmentsForHITRequest
  = ListAssignmentsForHITRequest { "HITId" :: EntityId, "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize), "AssignmentStatuses" :: NullOrUndefined (AssignmentStatusList) }
```

#### `ListAssignmentsForHITResponse`

``` purescript
newtype ListAssignmentsForHITResponse
  = ListAssignmentsForHITResponse { "NextToken" :: NullOrUndefined (PaginationToken), "NumResults" :: NullOrUndefined (Int), "Assignments" :: NullOrUndefined (AssignmentList) }
```

#### `ListBonusPaymentsRequest`

``` purescript
newtype ListBonusPaymentsRequest
  = ListBonusPaymentsRequest { "HITId" :: NullOrUndefined (EntityId), "AssignmentId" :: NullOrUndefined (EntityId), "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize) }
```

#### `ListBonusPaymentsResponse`

``` purescript
newtype ListBonusPaymentsResponse
  = ListBonusPaymentsResponse { "NumResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (PaginationToken), "BonusPayments" :: NullOrUndefined (BonusPaymentList) }
```

#### `ListHITsForQualificationTypeRequest`

``` purescript
newtype ListHITsForQualificationTypeRequest
  = ListHITsForQualificationTypeRequest { "QualificationTypeId" :: EntityId, "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize) }
```

#### `ListHITsForQualificationTypeResponse`

``` purescript
newtype ListHITsForQualificationTypeResponse
  = ListHITsForQualificationTypeResponse { "NextToken" :: NullOrUndefined (PaginationToken), "NumResults" :: NullOrUndefined (Int), "HITs" :: NullOrUndefined (HITList) }
```

#### `ListHITsRequest`

``` purescript
newtype ListHITsRequest
  = ListHITsRequest { "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize) }
```

#### `ListHITsResponse`

``` purescript
newtype ListHITsResponse
  = ListHITsResponse { "NextToken" :: NullOrUndefined (PaginationToken), "NumResults" :: NullOrUndefined (Int), "HITs" :: NullOrUndefined (HITList) }
```

#### `ListQualificationRequestsRequest`

``` purescript
newtype ListQualificationRequestsRequest
  = ListQualificationRequestsRequest { "QualificationTypeId" :: NullOrUndefined (EntityId), "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize) }
```

#### `ListQualificationRequestsResponse`

``` purescript
newtype ListQualificationRequestsResponse
  = ListQualificationRequestsResponse { "NumResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (PaginationToken), "QualificationRequests" :: NullOrUndefined (QualificationRequestList) }
```

#### `ListQualificationTypesRequest`

``` purescript
newtype ListQualificationTypesRequest
  = ListQualificationTypesRequest { "Query" :: NullOrUndefined (String), "MustBeRequestable" :: Boolean, "MustBeOwnedByCaller" :: NullOrUndefined (Boolean), "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize) }
```

#### `ListQualificationTypesResponse`

``` purescript
newtype ListQualificationTypesResponse
  = ListQualificationTypesResponse { "NumResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (PaginationToken), "QualificationTypes" :: NullOrUndefined (QualificationTypeList) }
```

#### `ListReviewPolicyResultsForHITRequest`

``` purescript
newtype ListReviewPolicyResultsForHITRequest
  = ListReviewPolicyResultsForHITRequest { "HITId" :: EntityId, "PolicyLevels" :: NullOrUndefined (ReviewPolicyLevelList), "RetrieveActions" :: NullOrUndefined (Boolean), "RetrieveResults" :: NullOrUndefined (Boolean), "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize) }
```

#### `ListReviewPolicyResultsForHITResponse`

``` purescript
newtype ListReviewPolicyResultsForHITResponse
  = ListReviewPolicyResultsForHITResponse { "HITId" :: NullOrUndefined (EntityId), "AssignmentReviewPolicy" :: NullOrUndefined (ReviewPolicy), "HITReviewPolicy" :: NullOrUndefined (ReviewPolicy), "AssignmentReviewReport" :: NullOrUndefined (ReviewReport), "HITReviewReport" :: NullOrUndefined (ReviewReport), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `ListReviewableHITsRequest`

``` purescript
newtype ListReviewableHITsRequest
  = ListReviewableHITsRequest { "HITTypeId" :: NullOrUndefined (EntityId), "Status" :: NullOrUndefined (ReviewableHITStatus), "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize) }
```

#### `ListReviewableHITsResponse`

``` purescript
newtype ListReviewableHITsResponse
  = ListReviewableHITsResponse { "NextToken" :: NullOrUndefined (PaginationToken), "NumResults" :: NullOrUndefined (Int), "HITs" :: NullOrUndefined (HITList) }
```

#### `ListWorkerBlocksRequest`

``` purescript
newtype ListWorkerBlocksRequest
  = ListWorkerBlocksRequest { "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize) }
```

#### `ListWorkerBlocksResponse`

``` purescript
newtype ListWorkerBlocksResponse
  = ListWorkerBlocksResponse { "NextToken" :: NullOrUndefined (PaginationToken), "NumResults" :: NullOrUndefined (Int), "WorkerBlocks" :: NullOrUndefined (WorkerBlockList) }
```

#### `ListWorkersWithQualificationTypeRequest`

``` purescript
newtype ListWorkersWithQualificationTypeRequest
  = ListWorkersWithQualificationTypeRequest { "QualificationTypeId" :: EntityId, "Status" :: NullOrUndefined (QualificationStatus), "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (ResultSize) }
```

#### `ListWorkersWithQualificationTypeResponse`

``` purescript
newtype ListWorkersWithQualificationTypeResponse
  = ListWorkersWithQualificationTypeResponse { "NextToken" :: NullOrUndefined (PaginationToken), "NumResults" :: NullOrUndefined (Int), "Qualifications" :: NullOrUndefined (QualificationList) }
```

#### `Locale`

``` purescript
newtype Locale
  = Locale { "Country" :: CountryParameters, "Subdivision" :: NullOrUndefined (CountryParameters) }
```

<p>The Locale data structure represents a geographical region or location.</p>

#### `LocaleList`

``` purescript
newtype LocaleList
  = LocaleList (Array Locale)
```

#### `NotificationSpecification`

``` purescript
newtype NotificationSpecification
  = NotificationSpecification { "Destination" :: String, "Transport" :: NotificationTransport, "Version" :: String, "EventTypes" :: EventTypeList }
```

<p>The NotificationSpecification data structure describes a HIT event notification for a HIT type.</p>

#### `NotificationTransport`

``` purescript
newtype NotificationTransport
  = NotificationTransport String
```

#### `NotifyWorkersFailureCode`

``` purescript
newtype NotifyWorkersFailureCode
  = NotifyWorkersFailureCode String
```

#### `NotifyWorkersFailureStatus`

``` purescript
newtype NotifyWorkersFailureStatus
  = NotifyWorkersFailureStatus { "NotifyWorkersFailureCode" :: NullOrUndefined (NotifyWorkersFailureCode), "NotifyWorkersFailureMessage" :: NullOrUndefined (String), "WorkerId" :: NullOrUndefined (CustomerId) }
```

<p> When MTurk encounters an issue with notifying the Workers you specified, it returns back this object with failure details. </p>

#### `NotifyWorkersFailureStatusList`

``` purescript
newtype NotifyWorkersFailureStatusList
  = NotifyWorkersFailureStatusList (Array NotifyWorkersFailureStatus)
```

#### `NotifyWorkersRequest`

``` purescript
newtype NotifyWorkersRequest
  = NotifyWorkersRequest { "Subject" :: String, "MessageText" :: String, "WorkerIds" :: CustomerIdList }
```

#### `NotifyWorkersResponse`

``` purescript
newtype NotifyWorkersResponse
  = NotifyWorkersResponse { "NotifyWorkersFailureStatuses" :: NullOrUndefined (NotifyWorkersFailureStatusList) }
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

<p>If the previous response was incomplete (because there is more data to retrieve), Amazon Mechanical Turk returns a pagination token in the response. You can use this pagination token to retrieve the next set of results. </p>

#### `ParameterMapEntry`

``` purescript
newtype ParameterMapEntry
  = ParameterMapEntry { "Key" :: NullOrUndefined (String), "Values" :: NullOrUndefined (StringList) }
```

<p> This data structure is the data type for the AnswerKey parameter of the ScoreMyKnownAnswers/2011-09-01 Review Policy. </p>

#### `ParameterMapEntryList`

``` purescript
newtype ParameterMapEntryList
  = ParameterMapEntryList (Array ParameterMapEntry)
```

#### `PolicyParameter`

``` purescript
newtype PolicyParameter
  = PolicyParameter { "Key" :: NullOrUndefined (String), "Values" :: NullOrUndefined (StringList), "MapEntries" :: NullOrUndefined (ParameterMapEntryList) }
```

<p> Name of the parameter from the Review policy. </p>

#### `PolicyParameterList`

``` purescript
newtype PolicyParameterList
  = PolicyParameterList (Array PolicyParameter)
```

#### `Qualification`

``` purescript
newtype Qualification
  = Qualification { "QualificationTypeId" :: NullOrUndefined (EntityId), "WorkerId" :: NullOrUndefined (CustomerId), "GrantTime" :: NullOrUndefined (Number), "IntegerValue" :: NullOrUndefined (Int), "LocaleValue" :: NullOrUndefined (Locale), "Status" :: NullOrUndefined (QualificationStatus) }
```

<p>The Qualification data structure represents a Qualification assigned to a user, including the Qualification type and the value (score).</p>

#### `QualificationList`

``` purescript
newtype QualificationList
  = QualificationList (Array Qualification)
```

#### `QualificationRequest`

``` purescript
newtype QualificationRequest
  = QualificationRequest { "QualificationRequestId" :: NullOrUndefined (String), "QualificationTypeId" :: NullOrUndefined (EntityId), "WorkerId" :: NullOrUndefined (CustomerId), "Test" :: NullOrUndefined (String), "Answer" :: NullOrUndefined (String), "SubmitTime" :: NullOrUndefined (Number) }
```

<p> The QualificationRequest data structure represents a request a Worker has made for a Qualification. </p>

#### `QualificationRequestList`

``` purescript
newtype QualificationRequestList
  = QualificationRequestList (Array QualificationRequest)
```

#### `QualificationRequirement`

``` purescript
newtype QualificationRequirement
  = QualificationRequirement { "QualificationTypeId" :: String, "Comparator" :: Comparator, "IntegerValues" :: NullOrUndefined (IntegerList), "LocaleValues" :: NullOrUndefined (LocaleList), "RequiredToPreview" :: NullOrUndefined (Boolean) }
```

<p> The QualificationRequirement data structure describes a Qualification that a Worker must have before the Worker is allowed to accept a HIT. A requirement may optionally state that a Worker must have the Qualification in order to preview the HIT. </p>

#### `QualificationRequirementList`

``` purescript
newtype QualificationRequirementList
  = QualificationRequirementList (Array QualificationRequirement)
```

#### `QualificationStatus`

``` purescript
newtype QualificationStatus
  = QualificationStatus String
```

#### `QualificationType`

``` purescript
newtype QualificationType
  = QualificationType { "QualificationTypeId" :: NullOrUndefined (EntityId), "CreationTime" :: NullOrUndefined (Number), "Name" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "Keywords" :: NullOrUndefined (String), "QualificationTypeStatus" :: NullOrUndefined (QualificationTypeStatus), "Test" :: NullOrUndefined (String), "TestDurationInSeconds" :: NullOrUndefined (Number), "AnswerKey" :: NullOrUndefined (String), "RetryDelayInSeconds" :: NullOrUndefined (Number), "IsRequestable" :: NullOrUndefined (Boolean), "AutoGranted" :: NullOrUndefined (Boolean), "AutoGrantedValue" :: NullOrUndefined (Int) }
```

<p> The QualificationType data structure represents a Qualification type, a description of a property of a Worker that must match the requirements of a HIT for the Worker to be able to accept the HIT. The type also describes how a Worker can obtain a Qualification of that type, such as through a Qualification test. </p>

#### `QualificationTypeList`

``` purescript
newtype QualificationTypeList
  = QualificationTypeList (Array QualificationType)
```

#### `QualificationTypeStatus`

``` purescript
newtype QualificationTypeStatus
  = QualificationTypeStatus String
```

#### `RejectAssignmentRequest`

``` purescript
newtype RejectAssignmentRequest
  = RejectAssignmentRequest { "AssignmentId" :: EntityId, "RequesterFeedback" :: String }
```

#### `RejectAssignmentResponse`

``` purescript
newtype RejectAssignmentResponse
  = RejectAssignmentResponse {  }
```

#### `RejectQualificationRequestRequest`

``` purescript
newtype RejectQualificationRequestRequest
  = RejectQualificationRequestRequest { "QualificationRequestId" :: String, "Reason" :: NullOrUndefined (String) }
```

#### `RejectQualificationRequestResponse`

``` purescript
newtype RejectQualificationRequestResponse
  = RejectQualificationRequestResponse {  }
```

#### `RequestError`

``` purescript
newtype RequestError
  = RequestError { "Message" :: NullOrUndefined (ExceptionMessage), "TurkErrorCode" :: NullOrUndefined (TurkErrorCode) }
```

<p>Your request is invalid.</p>

#### `ResultSize`

``` purescript
newtype ResultSize
  = ResultSize Int
```

#### `ReviewActionDetail`

``` purescript
newtype ReviewActionDetail
  = ReviewActionDetail { "ActionId" :: NullOrUndefined (EntityId), "ActionName" :: NullOrUndefined (String), "TargetId" :: NullOrUndefined (EntityId), "TargetType" :: NullOrUndefined (String), "Status" :: NullOrUndefined (ReviewActionStatus), "CompleteTime" :: NullOrUndefined (Number), "Result" :: NullOrUndefined (String), "ErrorCode" :: NullOrUndefined (String) }
```

<p> Both the AssignmentReviewReport and the HITReviewReport elements contains the ReviewActionDetail data structure. This structure is returned multiple times for each action specified in the Review Policy. </p>

#### `ReviewActionDetailList`

``` purescript
newtype ReviewActionDetailList
  = ReviewActionDetailList (Array ReviewActionDetail)
```

#### `ReviewActionStatus`

``` purescript
newtype ReviewActionStatus
  = ReviewActionStatus String
```

#### `ReviewPolicy`

``` purescript
newtype ReviewPolicy
  = ReviewPolicy { "PolicyName" :: String, "Parameters" :: NullOrUndefined (PolicyParameterList) }
```

<p> HIT Review Policy data structures represent HIT review policies, which you specify when you create a HIT. </p>

#### `ReviewPolicyLevel`

``` purescript
newtype ReviewPolicyLevel
  = ReviewPolicyLevel String
```

#### `ReviewPolicyLevelList`

``` purescript
newtype ReviewPolicyLevelList
  = ReviewPolicyLevelList (Array ReviewPolicyLevel)
```

#### `ReviewReport`

``` purescript
newtype ReviewReport
  = ReviewReport { "ReviewResults" :: NullOrUndefined (ReviewResultDetailList), "ReviewActions" :: NullOrUndefined (ReviewActionDetailList) }
```

<p> Contains both ReviewResult and ReviewAction elements for a particular HIT. </p>

#### `ReviewResultDetail`

``` purescript
newtype ReviewResultDetail
  = ReviewResultDetail { "ActionId" :: NullOrUndefined (EntityId), "SubjectId" :: NullOrUndefined (EntityId), "SubjectType" :: NullOrUndefined (String), "QuestionId" :: NullOrUndefined (EntityId), "Key" :: NullOrUndefined (String), "Value" :: NullOrUndefined (String) }
```

<p> This data structure is returned multiple times for each result specified in the Review Policy. </p>

#### `ReviewResultDetailList`

``` purescript
newtype ReviewResultDetailList
  = ReviewResultDetailList (Array ReviewResultDetail)
```

#### `ReviewableHITStatus`

``` purescript
newtype ReviewableHITStatus
  = ReviewableHITStatus String
```

#### `SendBonusRequest`

``` purescript
newtype SendBonusRequest
  = SendBonusRequest { "WorkerId" :: CustomerId, "BonusAmount" :: CurrencyAmount, "AssignmentId" :: EntityId, "Reason" :: String, "UniqueRequestToken" :: NullOrUndefined (IdempotencyToken) }
```

#### `SendBonusResponse`

``` purescript
newtype SendBonusResponse
  = SendBonusResponse {  }
```

#### `SendTestEventNotificationRequest`

``` purescript
newtype SendTestEventNotificationRequest
  = SendTestEventNotificationRequest { "Notification" :: NotificationSpecification, "TestEventType" :: EventType }
```

#### `SendTestEventNotificationResponse`

``` purescript
newtype SendTestEventNotificationResponse
  = SendTestEventNotificationResponse {  }
```

#### `ServiceFault`

``` purescript
newtype ServiceFault
  = ServiceFault { "Message" :: NullOrUndefined (ExceptionMessage), "TurkErrorCode" :: NullOrUndefined (TurkErrorCode) }
```

<p>Amazon Mechanical Turk is temporarily unable to process your request. Try your call again.</p>

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

#### `TurkErrorCode`

``` purescript
newtype TurkErrorCode
  = TurkErrorCode String
```

#### `UpdateExpirationForHITRequest`

``` purescript
newtype UpdateExpirationForHITRequest
  = UpdateExpirationForHITRequest { "HITId" :: EntityId, "ExpireAt" :: Number }
```

#### `UpdateExpirationForHITResponse`

``` purescript
newtype UpdateExpirationForHITResponse
  = UpdateExpirationForHITResponse {  }
```

#### `UpdateHITReviewStatusRequest`

``` purescript
newtype UpdateHITReviewStatusRequest
  = UpdateHITReviewStatusRequest { "HITId" :: EntityId, "Revert" :: NullOrUndefined (Boolean) }
```

#### `UpdateHITReviewStatusResponse`

``` purescript
newtype UpdateHITReviewStatusResponse
  = UpdateHITReviewStatusResponse {  }
```

#### `UpdateHITTypeOfHITRequest`

``` purescript
newtype UpdateHITTypeOfHITRequest
  = UpdateHITTypeOfHITRequest { "HITId" :: EntityId, "HITTypeId" :: EntityId }
```

#### `UpdateHITTypeOfHITResponse`

``` purescript
newtype UpdateHITTypeOfHITResponse
  = UpdateHITTypeOfHITResponse {  }
```

#### `UpdateNotificationSettingsRequest`

``` purescript
newtype UpdateNotificationSettingsRequest
  = UpdateNotificationSettingsRequest { "HITTypeId" :: EntityId, "Notification" :: NullOrUndefined (NotificationSpecification), "Active" :: NullOrUndefined (Boolean) }
```

#### `UpdateNotificationSettingsResponse`

``` purescript
newtype UpdateNotificationSettingsResponse
  = UpdateNotificationSettingsResponse {  }
```

#### `UpdateQualificationTypeRequest`

``` purescript
newtype UpdateQualificationTypeRequest
  = UpdateQualificationTypeRequest { "QualificationTypeId" :: EntityId, "Description" :: NullOrUndefined (String), "QualificationTypeStatus" :: NullOrUndefined (QualificationTypeStatus), "Test" :: NullOrUndefined (String), "AnswerKey" :: NullOrUndefined (String), "TestDurationInSeconds" :: NullOrUndefined (Number), "RetryDelayInSeconds" :: NullOrUndefined (Number), "AutoGranted" :: NullOrUndefined (Boolean), "AutoGrantedValue" :: NullOrUndefined (Int) }
```

#### `UpdateQualificationTypeResponse`

``` purescript
newtype UpdateQualificationTypeResponse
  = UpdateQualificationTypeResponse { "QualificationType" :: NullOrUndefined (QualificationType) }
```

#### `WorkerBlock`

``` purescript
newtype WorkerBlock
  = WorkerBlock { "WorkerId" :: NullOrUndefined (CustomerId), "Reason" :: NullOrUndefined (String) }
```

<p> The WorkerBlock data structure represents a Worker who has been blocked. It has two elements: the WorkerId and the Reason for the block. </p>

#### `WorkerBlockList`

``` purescript
newtype WorkerBlockList
  = WorkerBlockList (Array WorkerBlock)
```


