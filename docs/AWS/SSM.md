## Module AWS.SSM

<fullname>AWS Systems Manager</fullname> <p>AWS Systems Manager is a collection of capabilities that helps you automate management tasks such as collecting system inventory, applying operating system (OS) patches, automating the creation of Amazon Machine Images (AMIs), and configuring operating systems (OSs) and applications at scale. Systems Manager lets you remotely and securely manage the configuration of your managed instances. A <i>managed instance</i> is any Amazon EC2 instance or on-premises machine in your hybrid environment that has been configured for Systems Manager.</p> <p>This reference is intended to be used with the <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/">AWS Systems Manager User Guide</a>.</p> <p>To get started, verify prerequisites and configure managed instances. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-setting-up.html">Systems Manager Prerequisites</a>.</p> <p>For information about other API actions you can perform on Amazon EC2 instances, see the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/">Amazon EC2 API Reference</a>. For information about how to use a Query API, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/making-api-requests.html">Making API Requests</a>. </p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addTagsToResource`

``` purescript
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (err :: RequestError | eff) AddTagsToResourceResult
```

<p>Adds or overwrites one or more tags for the specified resource. Tags are metadata that you can assign to your documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. Each tag consists of a key and an optional value, both of which you define. For example, you could define a set of tags for your account's managed instances that helps you track each instance's owner and stack level. For example: Key=Owner and Value=DbAdmin, SysAdmin, or Dev. Or Key=Stack and Value=Production, Pre-Production, or Test.</p> <p>Each resource can have a maximum of 50 tags. </p> <p>We recommend that you devise a set of tag keys that meets your needs for each resource type. Using a consistent set of tag keys makes it easier for you to manage your resources. You can search and filter the resources based on the tags you add. Tags don't have any semantic meaning to Amazon EC2 and are interpreted strictly as a string of characters. </p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html">Tagging Your Amazon EC2 Resources</a> in the <i>Amazon EC2 User Guide</i>.</p>

#### `cancelCommand`

``` purescript
cancelCommand :: forall eff. CancelCommandRequest -> Aff (err :: RequestError | eff) CancelCommandResult
```

<p>Attempts to cancel the command specified by the Command ID. There is no guarantee that the command will be terminated and the underlying process stopped.</p>

#### `createActivation`

``` purescript
createActivation :: forall eff. CreateActivationRequest -> Aff (err :: RequestError | eff) CreateActivationResult
```

<p>Registers your on-premises server or virtual machine with Amazon EC2 so that you can manage these resources using Run Command. An on-premises server or virtual machine that has been registered with EC2 is called a managed instance. For more information about activations, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances.html">Setting Up Systems Manager in Hybrid Environments</a>.</p>

#### `createAssociation`

``` purescript
createAssociation :: forall eff. CreateAssociationRequest -> Aff (err :: RequestError | eff) CreateAssociationResult
```

<p>Associates the specified Systems Manager document with the specified instances or targets.</p> <p>When you associate a document with one or more instances using instance IDs or tags, the SSM Agent running on the instance processes the document and configures the instance as specified.</p> <p>If you associate a document with an instance that already has an associated document, the system throws the AssociationAlreadyExists exception.</p>

#### `createAssociationBatch`

``` purescript
createAssociationBatch :: forall eff. CreateAssociationBatchRequest -> Aff (err :: RequestError | eff) CreateAssociationBatchResult
```

<p>Associates the specified Systems Manager document with the specified instances or targets.</p> <p>When you associate a document with one or more instances using instance IDs or tags, the SSM Agent running on the instance processes the document and configures the instance as specified.</p> <p>If you associate a document with an instance that already has an associated document, the system throws the AssociationAlreadyExists exception.</p>

#### `createDocument`

``` purescript
createDocument :: forall eff. CreateDocumentRequest -> Aff (err :: RequestError | eff) CreateDocumentResult
```

<p>Creates a Systems Manager document.</p> <p>After you create a document, you can use CreateAssociation to associate it with one or more running instances.</p>

#### `createMaintenanceWindow`

``` purescript
createMaintenanceWindow :: forall eff. CreateMaintenanceWindowRequest -> Aff (err :: RequestError | eff) CreateMaintenanceWindowResult
```

<p>Creates a new Maintenance Window.</p>

#### `createPatchBaseline`

``` purescript
createPatchBaseline :: forall eff. CreatePatchBaselineRequest -> Aff (err :: RequestError | eff) CreatePatchBaselineResult
```

<p>Creates a patch baseline.</p> <note> <p>For information about valid key and value pairs in <code>PatchFilters</code> for each supported operating system type, see <a href="http://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PatchFilter.html">PatchFilter</a>.</p> </note>

#### `createResourceDataSync`

``` purescript
createResourceDataSync :: forall eff. CreateResourceDataSyncRequest -> Aff (err :: RequestError | eff) CreateResourceDataSyncResult
```

<p>Creates a resource data sync configuration to a single bucket in Amazon S3. This is an asynchronous operation that returns immediately. After a successful initial sync is completed, the system continuously syncs data to the Amazon S3 bucket. To check the status of the sync, use the <a>ListResourceDataSync</a>.</p> <p>By default, data is not encrypted in Amazon S3. We strongly recommend that you enable encryption in Amazon S3 to ensure secure data storage. We also recommend that you secure access to the Amazon S3 bucket by creating a restrictive bucket policy. To view an example of a restrictive Amazon S3 bucket policy for Resource Data Sync, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-configuring.html#sysman-inventory-datasync">Configuring Resource Data Sync for Inventory</a>.</p>

#### `deleteActivation`

``` purescript
deleteActivation :: forall eff. DeleteActivationRequest -> Aff (err :: RequestError | eff) DeleteActivationResult
```

<p>Deletes an activation. You are not required to delete an activation. If you delete an activation, you can no longer use it to register additional managed instances. Deleting an activation does not de-register managed instances. You must manually de-register managed instances.</p>

#### `deleteAssociation`

``` purescript
deleteAssociation :: forall eff. DeleteAssociationRequest -> Aff (err :: RequestError | eff) DeleteAssociationResult
```

<p>Disassociates the specified Systems Manager document from the specified instance.</p> <p>When you disassociate a document from an instance, it does not change the configuration of the instance. To change the configuration state of an instance after you disassociate a document, you must create a new document with the desired configuration and associate it with the instance.</p>

#### `deleteDocument`

``` purescript
deleteDocument :: forall eff. DeleteDocumentRequest -> Aff (err :: RequestError | eff) DeleteDocumentResult
```

<p>Deletes the Systems Manager document and all instance associations to the document.</p> <p>Before you delete the document, we recommend that you use <a>DeleteAssociation</a> to disassociate all instances that are associated with the document.</p>

#### `deleteMaintenanceWindow`

``` purescript
deleteMaintenanceWindow :: forall eff. DeleteMaintenanceWindowRequest -> Aff (err :: RequestError | eff) DeleteMaintenanceWindowResult
```

<p>Deletes a Maintenance Window.</p>

#### `deleteParameter`

``` purescript
deleteParameter :: forall eff. DeleteParameterRequest -> Aff (err :: RequestError | eff) DeleteParameterResult
```

<p>Delete a parameter from the system.</p>

#### `deleteParameters`

``` purescript
deleteParameters :: forall eff. DeleteParametersRequest -> Aff (err :: RequestError | eff) DeleteParametersResult
```

<p>Delete a list of parameters. This API is used to delete parameters by using the Amazon EC2 console.</p>

#### `deletePatchBaseline`

``` purescript
deletePatchBaseline :: forall eff. DeletePatchBaselineRequest -> Aff (err :: RequestError | eff) DeletePatchBaselineResult
```

<p>Deletes a patch baseline.</p>

#### `deleteResourceDataSync`

``` purescript
deleteResourceDataSync :: forall eff. DeleteResourceDataSyncRequest -> Aff (err :: RequestError | eff) DeleteResourceDataSyncResult
```

<p>Deletes a Resource Data Sync configuration. After the configuration is deleted, changes to inventory data on managed instances are no longer synced with the target Amazon S3 bucket. Deleting a sync configuration does not delete data in the target Amazon S3 bucket.</p>

#### `deregisterManagedInstance`

``` purescript
deregisterManagedInstance :: forall eff. DeregisterManagedInstanceRequest -> Aff (err :: RequestError | eff) DeregisterManagedInstanceResult
```

<p>Removes the server or virtual machine from the list of registered servers. You can reregister the instance again at any time. If you don't plan to use Run Command on the server, we suggest uninstalling the SSM Agent first.</p>

#### `deregisterPatchBaselineForPatchGroup`

``` purescript
deregisterPatchBaselineForPatchGroup :: forall eff. DeregisterPatchBaselineForPatchGroupRequest -> Aff (err :: RequestError | eff) DeregisterPatchBaselineForPatchGroupResult
```

<p>Removes a patch group from a patch baseline.</p>

#### `deregisterTargetFromMaintenanceWindow`

``` purescript
deregisterTargetFromMaintenanceWindow :: forall eff. DeregisterTargetFromMaintenanceWindowRequest -> Aff (err :: RequestError | eff) DeregisterTargetFromMaintenanceWindowResult
```

<p>Removes a target from a Maintenance Window.</p>

#### `deregisterTaskFromMaintenanceWindow`

``` purescript
deregisterTaskFromMaintenanceWindow :: forall eff. DeregisterTaskFromMaintenanceWindowRequest -> Aff (err :: RequestError | eff) DeregisterTaskFromMaintenanceWindowResult
```

<p>Removes a task from a Maintenance Window.</p>

#### `describeActivations`

``` purescript
describeActivations :: forall eff. DescribeActivationsRequest -> Aff (err :: RequestError | eff) DescribeActivationsResult
```

<p>Details about the activation, including: the date and time the activation was created, the expiration date, the IAM role assigned to the instances in the activation, and the number of instances activated by this registration.</p>

#### `describeAssociation`

``` purescript
describeAssociation :: forall eff. DescribeAssociationRequest -> Aff (err :: RequestError | eff) DescribeAssociationResult
```

<p>Describes the association for the specified target or instance. If you created the association by using the <code>Targets</code> parameter, then you must retrieve the association by using the association ID. If you created the association by specifying an instance ID and a Systems Manager document, then you retrieve the association by specifying the document name and the instance ID. </p>

#### `describeAutomationExecutions`

``` purescript
describeAutomationExecutions :: forall eff. DescribeAutomationExecutionsRequest -> Aff (err :: RequestError | eff) DescribeAutomationExecutionsResult
```

<p>Provides details about all active and terminated Automation executions.</p>

#### `describeAutomationStepExecutions`

``` purescript
describeAutomationStepExecutions :: forall eff. DescribeAutomationStepExecutionsRequest -> Aff (err :: RequestError | eff) DescribeAutomationStepExecutionsResult
```

<p>Information about all active and terminated step executions in an Automation workflow.</p>

#### `describeAvailablePatches`

``` purescript
describeAvailablePatches :: forall eff. DescribeAvailablePatchesRequest -> Aff (err :: RequestError | eff) DescribeAvailablePatchesResult
```

<p>Lists all patches that could possibly be included in a patch baseline.</p>

#### `describeDocument`

``` purescript
describeDocument :: forall eff. DescribeDocumentRequest -> Aff (err :: RequestError | eff) DescribeDocumentResult
```

<p>Describes the specified Systems Manager document.</p>

#### `describeDocumentPermission`

``` purescript
describeDocumentPermission :: forall eff. DescribeDocumentPermissionRequest -> Aff (err :: RequestError | eff) DescribeDocumentPermissionResponse
```

<p>Describes the permissions for a Systems Manager document. If you created the document, you are the owner. If a document is shared, it can either be shared privately (by specifying a user's AWS account ID) or publicly (<i>All</i>). </p>

#### `describeEffectiveInstanceAssociations`

``` purescript
describeEffectiveInstanceAssociations :: forall eff. DescribeEffectiveInstanceAssociationsRequest -> Aff (err :: RequestError | eff) DescribeEffectiveInstanceAssociationsResult
```

<p>All associations for the instance(s).</p>

#### `describeEffectivePatchesForPatchBaseline`

``` purescript
describeEffectivePatchesForPatchBaseline :: forall eff. DescribeEffectivePatchesForPatchBaselineRequest -> Aff (err :: RequestError | eff) DescribeEffectivePatchesForPatchBaselineResult
```

<p>Retrieves the current effective patches (the patch and the approval state) for the specified patch baseline. Note that this API applies only to Windows patch baselines.</p>

#### `describeInstanceAssociationsStatus`

``` purescript
describeInstanceAssociationsStatus :: forall eff. DescribeInstanceAssociationsStatusRequest -> Aff (err :: RequestError | eff) DescribeInstanceAssociationsStatusResult
```

<p>The status of the associations for the instance(s).</p>

#### `describeInstanceInformation`

``` purescript
describeInstanceInformation :: forall eff. DescribeInstanceInformationRequest -> Aff (err :: RequestError | eff) DescribeInstanceInformationResult
```

<p>Describes one or more of your instances. You can use this to get information about instances like the operating system platform, the SSM Agent version (Linux), status etc. If you specify one or more instance IDs, it returns information for those instances. If you do not specify instance IDs, it returns information for all your instances. If you specify an instance ID that is not valid or an instance that you do not own, you receive an error. </p>

#### `describeInstancePatchStates`

``` purescript
describeInstancePatchStates :: forall eff. DescribeInstancePatchStatesRequest -> Aff (err :: RequestError | eff) DescribeInstancePatchStatesResult
```

<p>Retrieves the high-level patch state of one or more instances.</p>

#### `describeInstancePatchStatesForPatchGroup`

``` purescript
describeInstancePatchStatesForPatchGroup :: forall eff. DescribeInstancePatchStatesForPatchGroupRequest -> Aff (err :: RequestError | eff) DescribeInstancePatchStatesForPatchGroupResult
```

<p>Retrieves the high-level patch state for the instances in the specified patch group.</p>

#### `describeInstancePatches`

``` purescript
describeInstancePatches :: forall eff. DescribeInstancePatchesRequest -> Aff (err :: RequestError | eff) DescribeInstancePatchesResult
```

<p>Retrieves information about the patches on the specified instance and their state relative to the patch baseline being used for the instance.</p>

#### `describeMaintenanceWindowExecutionTaskInvocations`

``` purescript
describeMaintenanceWindowExecutionTaskInvocations :: forall eff. DescribeMaintenanceWindowExecutionTaskInvocationsRequest -> Aff (err :: RequestError | eff) DescribeMaintenanceWindowExecutionTaskInvocationsResult
```

<p>Retrieves the individual task executions (one per target) for a particular task executed as part of a Maintenance Window execution.</p>

#### `describeMaintenanceWindowExecutionTasks`

``` purescript
describeMaintenanceWindowExecutionTasks :: forall eff. DescribeMaintenanceWindowExecutionTasksRequest -> Aff (err :: RequestError | eff) DescribeMaintenanceWindowExecutionTasksResult
```

<p>For a given Maintenance Window execution, lists the tasks that were executed.</p>

#### `describeMaintenanceWindowExecutions`

``` purescript
describeMaintenanceWindowExecutions :: forall eff. DescribeMaintenanceWindowExecutionsRequest -> Aff (err :: RequestError | eff) DescribeMaintenanceWindowExecutionsResult
```

<p>Lists the executions of a Maintenance Window. This includes information about when the Maintenance Window was scheduled to be active, and information about tasks registered and run with the Maintenance Window.</p>

#### `describeMaintenanceWindowTargets`

``` purescript
describeMaintenanceWindowTargets :: forall eff. DescribeMaintenanceWindowTargetsRequest -> Aff (err :: RequestError | eff) DescribeMaintenanceWindowTargetsResult
```

<p>Lists the targets registered with the Maintenance Window.</p>

#### `describeMaintenanceWindowTasks`

``` purescript
describeMaintenanceWindowTasks :: forall eff. DescribeMaintenanceWindowTasksRequest -> Aff (err :: RequestError | eff) DescribeMaintenanceWindowTasksResult
```

<p>Lists the tasks in a Maintenance Window.</p>

#### `describeMaintenanceWindows`

``` purescript
describeMaintenanceWindows :: forall eff. DescribeMaintenanceWindowsRequest -> Aff (err :: RequestError | eff) DescribeMaintenanceWindowsResult
```

<p>Retrieves the Maintenance Windows in an AWS account.</p>

#### `describeParameters`

``` purescript
describeParameters :: forall eff. DescribeParametersRequest -> Aff (err :: RequestError | eff) DescribeParametersResult
```

<p>Get information about a parameter.</p> <p>Request results are returned on a best-effort basis. If you specify <code>MaxResults</code> in the request, the response includes information up to the limit specified. The number of items returned, however, can be between zero and the value of <code>MaxResults</code>. If the service reaches an internal limit while processing the results, it stops the operation and returns the matching values up to that point and a <code>NextToken</code>. You can specify the <code>NextToken</code> in a subsequent call to get the next set of results.</p>

#### `describePatchBaselines`

``` purescript
describePatchBaselines :: forall eff. DescribePatchBaselinesRequest -> Aff (err :: RequestError | eff) DescribePatchBaselinesResult
```

<p>Lists the patch baselines in your AWS account.</p>

#### `describePatchGroupState`

``` purescript
describePatchGroupState :: forall eff. DescribePatchGroupStateRequest -> Aff (err :: RequestError | eff) DescribePatchGroupStateResult
```

<p>Returns high-level aggregated patch compliance state for a patch group.</p>

#### `describePatchGroups`

``` purescript
describePatchGroups :: forall eff. DescribePatchGroupsRequest -> Aff (err :: RequestError | eff) DescribePatchGroupsResult
```

<p>Lists all patch groups that have been registered with patch baselines.</p>

#### `getAutomationExecution`

``` purescript
getAutomationExecution :: forall eff. GetAutomationExecutionRequest -> Aff (err :: RequestError | eff) GetAutomationExecutionResult
```

<p>Get detailed information about a particular Automation execution.</p>

#### `getCommandInvocation`

``` purescript
getCommandInvocation :: forall eff. GetCommandInvocationRequest -> Aff (err :: RequestError | eff) GetCommandInvocationResult
```

<p>Returns detailed information about command execution for an invocation or plugin. </p>

#### `getDefaultPatchBaseline`

``` purescript
getDefaultPatchBaseline :: forall eff. GetDefaultPatchBaselineRequest -> Aff (err :: RequestError | eff) GetDefaultPatchBaselineResult
```

<p>Retrieves the default patch baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.</p>

#### `getDeployablePatchSnapshotForInstance`

``` purescript
getDeployablePatchSnapshotForInstance :: forall eff. GetDeployablePatchSnapshotForInstanceRequest -> Aff (err :: RequestError | eff) GetDeployablePatchSnapshotForInstanceResult
```

<p>Retrieves the current snapshot for the patch baseline the instance uses. This API is primarily used by the AWS-RunPatchBaseline Systems Manager document. </p>

#### `getDocument`

``` purescript
getDocument :: forall eff. GetDocumentRequest -> Aff (err :: RequestError | eff) GetDocumentResult
```

<p>Gets the contents of the specified Systems Manager document.</p>

#### `getInventory`

``` purescript
getInventory :: forall eff. GetInventoryRequest -> Aff (err :: RequestError | eff) GetInventoryResult
```

<p>Query inventory information.</p>

#### `getInventorySchema`

``` purescript
getInventorySchema :: forall eff. GetInventorySchemaRequest -> Aff (err :: RequestError | eff) GetInventorySchemaResult
```

<p>Return a list of inventory type names for the account, or return a list of attribute names for a specific Inventory item type. </p>

#### `getMaintenanceWindow`

``` purescript
getMaintenanceWindow :: forall eff. GetMaintenanceWindowRequest -> Aff (err :: RequestError | eff) GetMaintenanceWindowResult
```

<p>Retrieves a Maintenance Window.</p>

#### `getMaintenanceWindowExecution`

``` purescript
getMaintenanceWindowExecution :: forall eff. GetMaintenanceWindowExecutionRequest -> Aff (err :: RequestError | eff) GetMaintenanceWindowExecutionResult
```

<p>Retrieves details about a specific task executed as part of a Maintenance Window execution.</p>

#### `getMaintenanceWindowExecutionTask`

``` purescript
getMaintenanceWindowExecutionTask :: forall eff. GetMaintenanceWindowExecutionTaskRequest -> Aff (err :: RequestError | eff) GetMaintenanceWindowExecutionTaskResult
```

<p>Retrieves the details about a specific task executed as part of a Maintenance Window execution.</p>

#### `getMaintenanceWindowExecutionTaskInvocation`

``` purescript
getMaintenanceWindowExecutionTaskInvocation :: forall eff. GetMaintenanceWindowExecutionTaskInvocationRequest -> Aff (err :: RequestError | eff) GetMaintenanceWindowExecutionTaskInvocationResult
```

<p>Retrieves a task invocation. A task invocation is a specific task executing on a specific target. Maintenance Windows report status for all invocations. </p>

#### `getMaintenanceWindowTask`

``` purescript
getMaintenanceWindowTask :: forall eff. GetMaintenanceWindowTaskRequest -> Aff (err :: RequestError | eff) GetMaintenanceWindowTaskResult
```

<p>Lists the tasks in a Maintenance Window.</p>

#### `getParameter`

``` purescript
getParameter :: forall eff. GetParameterRequest -> Aff (err :: RequestError | eff) GetParameterResult
```

<p>Get information about a parameter by using the parameter name. </p>

#### `getParameterHistory`

``` purescript
getParameterHistory :: forall eff. GetParameterHistoryRequest -> Aff (err :: RequestError | eff) GetParameterHistoryResult
```

<p>Query a list of all parameters used by the AWS account.</p>

#### `getParameters`

``` purescript
getParameters :: forall eff. GetParametersRequest -> Aff (err :: RequestError | eff) GetParametersResult
```

<p>Get details of a parameter.</p>

#### `getParametersByPath`

``` purescript
getParametersByPath :: forall eff. GetParametersByPathRequest -> Aff (err :: RequestError | eff) GetParametersByPathResult
```

<p>Retrieve parameters in a specific hierarchy. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html">Working with Systems Manager Parameters</a>. </p> <p>Request results are returned on a best-effort basis. If you specify <code>MaxResults</code> in the request, the response includes information up to the limit specified. The number of items returned, however, can be between zero and the value of <code>MaxResults</code>. If the service reaches an internal limit while processing the results, it stops the operation and returns the matching values up to that point and a <code>NextToken</code>. You can specify the <code>NextToken</code> in a subsequent call to get the next set of results.</p> <note> <p>This API action doesn't support filtering by tags. </p> </note>

#### `getPatchBaseline`

``` purescript
getPatchBaseline :: forall eff. GetPatchBaselineRequest -> Aff (err :: RequestError | eff) GetPatchBaselineResult
```

<p>Retrieves information about a patch baseline.</p>

#### `getPatchBaselineForPatchGroup`

``` purescript
getPatchBaselineForPatchGroup :: forall eff. GetPatchBaselineForPatchGroupRequest -> Aff (err :: RequestError | eff) GetPatchBaselineForPatchGroupResult
```

<p>Retrieves the patch baseline that should be used for the specified patch group.</p>

#### `listAssociationVersions`

``` purescript
listAssociationVersions :: forall eff. ListAssociationVersionsRequest -> Aff (err :: RequestError | eff) ListAssociationVersionsResult
```

<p>Retrieves all versions of an association for a specific association ID.</p>

#### `listAssociations`

``` purescript
listAssociations :: forall eff. ListAssociationsRequest -> Aff (err :: RequestError | eff) ListAssociationsResult
```

<p>Lists the associations for the specified Systems Manager document or instance.</p>

#### `listCommandInvocations`

``` purescript
listCommandInvocations :: forall eff. ListCommandInvocationsRequest -> Aff (err :: RequestError | eff) ListCommandInvocationsResult
```

<p>An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. ListCommandInvocations provide status about command execution.</p>

#### `listCommands`

``` purescript
listCommands :: forall eff. ListCommandsRequest -> Aff (err :: RequestError | eff) ListCommandsResult
```

<p>Lists the commands requested by users of the AWS account.</p>

#### `listComplianceItems`

``` purescript
listComplianceItems :: forall eff. ListComplianceItemsRequest -> Aff (err :: RequestError | eff) ListComplianceItemsResult
```

<p>For a specified resource ID, this API action returns a list of compliance statuses for different resource types. Currently, you can only specify one resource ID per call. List results depend on the criteria specified in the filter. </p>

#### `listComplianceSummaries`

``` purescript
listComplianceSummaries :: forall eff. ListComplianceSummariesRequest -> Aff (err :: RequestError | eff) ListComplianceSummariesResult
```

<p>Returns a summary count of compliant and non-compliant resources for a compliance type. For example, this call can return State Manager associations, patches, or custom compliance types according to the filter criteria that you specify. </p>

#### `listDocumentVersions`

``` purescript
listDocumentVersions :: forall eff. ListDocumentVersionsRequest -> Aff (err :: RequestError | eff) ListDocumentVersionsResult
```

<p>List all versions for a document.</p>

#### `listDocuments`

``` purescript
listDocuments :: forall eff. ListDocumentsRequest -> Aff (err :: RequestError | eff) ListDocumentsResult
```

<p>Describes one or more of your Systems Manager documents.</p>

#### `listInventoryEntries`

``` purescript
listInventoryEntries :: forall eff. ListInventoryEntriesRequest -> Aff (err :: RequestError | eff) ListInventoryEntriesResult
```

<p>A list of inventory items returned by the request.</p>

#### `listResourceComplianceSummaries`

``` purescript
listResourceComplianceSummaries :: forall eff. ListResourceComplianceSummariesRequest -> Aff (err :: RequestError | eff) ListResourceComplianceSummariesResult
```

<p>Returns a resource-level summary count. The summary includes information about compliant and non-compliant statuses and detailed compliance-item severity counts, according to the filter criteria you specify.</p>

#### `listResourceDataSync`

``` purescript
listResourceDataSync :: forall eff. ListResourceDataSyncRequest -> Aff (err :: RequestError | eff) ListResourceDataSyncResult
```

<p>Lists your resource data sync configurations. Includes information about the last time a sync attempted to start, the last sync status, and the last time a sync successfully completed.</p> <p>The number of sync configurations might be too large to return using a single call to <code>ListResourceDataSync</code>. You can limit the number of sync configurations returned by using the <code>MaxResults</code> parameter. To determine whether there are more sync configurations to list, check the value of <code>NextToken</code> in the output. If there are more sync configurations to list, you can request them by specifying the <code>NextToken</code> returned in the call to the parameter of a subsequent call. </p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: RequestError | eff) ListTagsForResourceResult
```

<p>Returns a list of the tags assigned to the specified resource.</p>

#### `modifyDocumentPermission`

``` purescript
modifyDocumentPermission :: forall eff. ModifyDocumentPermissionRequest -> Aff (err :: RequestError | eff) ModifyDocumentPermissionResponse
```

<p>Shares a Systems Manager document publicly or privately. If you share a document privately, you must specify the AWS user account IDs for those people who can use the document. If you share a document publicly, you must specify <i>All</i> as the account ID.</p>

#### `putComplianceItems`

``` purescript
putComplianceItems :: forall eff. PutComplianceItemsRequest -> Aff (err :: RequestError | eff) PutComplianceItemsResult
```

<p>Registers a compliance type and other compliance details on a designated resource. This action lets you register custom compliance details with a resource. This call overwrites existing compliance information on the resource, so you must provide a full list of compliance items each time that you send the request.</p> <p>ComplianceType can be one of the following:</p> <ul> <li> <p>ExecutionId: The execution ID when the patch, association, or custom compliance item was applied.</p> </li> <li> <p>ExecutionType: Specify patch, association, or Custom:<code>string</code>.</p> </li> <li> <p>ExecutionTime. The time the patch, association, or custom compliance item was applied to the instance.</p> </li> <li> <p>Id: The patch, association, or custom compliance ID.</p> </li> <li> <p>Title: A title.</p> </li> <li> <p>Status: The status of the compliance item. For example, <code>approved</code> for patches, or <code>Failed</code> for associations.</p> </li> <li> <p>Severity: A patch severity. For example, <code>critical</code>.</p> </li> <li> <p>DocumentName: A SSM document name. For example, AWS-RunPatchBaseline.</p> </li> <li> <p>DocumentVersion: An SSM document version number. For example, 4.</p> </li> <li> <p>Classification: A patch classification. For example, <code>security updates</code>.</p> </li> <li> <p>PatchBaselineId: A patch baseline ID.</p> </li> <li> <p>PatchSeverity: A patch severity. For example, <code>Critical</code>.</p> </li> <li> <p>PatchState: A patch state. For example, <code>InstancesWithFailedPatches</code>.</p> </li> <li> <p>PatchGroup: The name of a patch group.</p> </li> <li> <p>InstalledTime: The time the association, patch, or custom compliance item was applied to the resource. Specify the time by using the following format: yyyy-MM-dd'T'HH:mm:ss'Z'</p> </li> </ul>

#### `putInventory`

``` purescript
putInventory :: forall eff. PutInventoryRequest -> Aff (err :: RequestError | eff) PutInventoryResult
```

<p>Bulk update custom inventory items on one more instance. The request adds an inventory item, if it doesn't already exist, or updates an inventory item, if it does exist.</p>

#### `putParameter`

``` purescript
putParameter :: forall eff. PutParameterRequest -> Aff (err :: RequestError | eff) PutParameterResult
```

<p>Add one or more parameters to the system.</p>

#### `registerDefaultPatchBaseline`

``` purescript
registerDefaultPatchBaseline :: forall eff. RegisterDefaultPatchBaselineRequest -> Aff (err :: RequestError | eff) RegisterDefaultPatchBaselineResult
```

<p>Defines the default patch baseline.</p>

#### `registerPatchBaselineForPatchGroup`

``` purescript
registerPatchBaselineForPatchGroup :: forall eff. RegisterPatchBaselineForPatchGroupRequest -> Aff (err :: RequestError | eff) RegisterPatchBaselineForPatchGroupResult
```

<p>Registers a patch baseline for a patch group.</p>

#### `registerTargetWithMaintenanceWindow`

``` purescript
registerTargetWithMaintenanceWindow :: forall eff. RegisterTargetWithMaintenanceWindowRequest -> Aff (err :: RequestError | eff) RegisterTargetWithMaintenanceWindowResult
```

<p>Registers a target with a Maintenance Window.</p>

#### `registerTaskWithMaintenanceWindow`

``` purescript
registerTaskWithMaintenanceWindow :: forall eff. RegisterTaskWithMaintenanceWindowRequest -> Aff (err :: RequestError | eff) RegisterTaskWithMaintenanceWindowResult
```

<p>Adds a new task to a Maintenance Window.</p>

#### `removeTagsFromResource`

``` purescript
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (err :: RequestError | eff) RemoveTagsFromResourceResult
```

<p>Removes all tags from the specified resource.</p>

#### `sendAutomationSignal`

``` purescript
sendAutomationSignal :: forall eff. SendAutomationSignalRequest -> Aff (err :: RequestError | eff) SendAutomationSignalResult
```

<p>Sends a signal to an Automation execution to change the current behavior or status of the execution. </p>

#### `sendCommand`

``` purescript
sendCommand :: forall eff. SendCommandRequest -> Aff (err :: RequestError | eff) SendCommandResult
```

<p>Executes commands on one or more managed instances.</p>

#### `startAutomationExecution`

``` purescript
startAutomationExecution :: forall eff. StartAutomationExecutionRequest -> Aff (err :: RequestError | eff) StartAutomationExecutionResult
```

<p>Initiates execution of an Automation document.</p>

#### `stopAutomationExecution`

``` purescript
stopAutomationExecution :: forall eff. StopAutomationExecutionRequest -> Aff (err :: RequestError | eff) StopAutomationExecutionResult
```

<p>Stop an Automation that is currently executing.</p>

#### `updateAssociation`

``` purescript
updateAssociation :: forall eff. UpdateAssociationRequest -> Aff (err :: RequestError | eff) UpdateAssociationResult
```

<p>Updates an association. You can update the association name and version, the document version, schedule, parameters, and Amazon S3 output.</p>

#### `updateAssociationStatus`

``` purescript
updateAssociationStatus :: forall eff. UpdateAssociationStatusRequest -> Aff (err :: RequestError | eff) UpdateAssociationStatusResult
```

<p>Updates the status of the Systems Manager document associated with the specified instance.</p>

#### `updateDocument`

``` purescript
updateDocument :: forall eff. UpdateDocumentRequest -> Aff (err :: RequestError | eff) UpdateDocumentResult
```

<p>The document you want to update.</p>

#### `updateDocumentDefaultVersion`

``` purescript
updateDocumentDefaultVersion :: forall eff. UpdateDocumentDefaultVersionRequest -> Aff (err :: RequestError | eff) UpdateDocumentDefaultVersionResult
```

<p>Set the default version of a document. </p>

#### `updateMaintenanceWindow`

``` purescript
updateMaintenanceWindow :: forall eff. UpdateMaintenanceWindowRequest -> Aff (err :: RequestError | eff) UpdateMaintenanceWindowResult
```

<p>Updates an existing Maintenance Window. Only specified parameters are modified.</p>

#### `updateMaintenanceWindowTarget`

``` purescript
updateMaintenanceWindowTarget :: forall eff. UpdateMaintenanceWindowTargetRequest -> Aff (err :: RequestError | eff) UpdateMaintenanceWindowTargetResult
```

<p>Modifies the target of an existing Maintenance Window. You can't change the target type, but you can change the following:</p> <p>The target from being an ID target to a Tag target, or a Tag target to an ID target.</p> <p>IDs for an ID target.</p> <p>Tags for a Tag target.</p> <p>Owner.</p> <p>Name.</p> <p>Description.</p> <p>If a parameter is null, then the corresponding field is not modified.</p>

#### `updateMaintenanceWindowTask`

``` purescript
updateMaintenanceWindowTask :: forall eff. UpdateMaintenanceWindowTaskRequest -> Aff (err :: RequestError | eff) UpdateMaintenanceWindowTaskResult
```

<p>Modifies a task assigned to a Maintenance Window. You can't change the task type, but you can change the following values:</p> <p>Task ARN. For example, you can change a RUN_COMMAND task from AWS-RunPowerShellScript to AWS-RunShellScript.</p> <p>Service role ARN.</p> <p>Task parameters.</p> <p>Task priority.</p> <p>Task MaxConcurrency and MaxErrors.</p> <p>Log location.</p> <p>If a parameter is null, then the corresponding field is not modified. Also, if you set Replace to true, then all fields required by the RegisterTaskWithMaintenanceWindow action are required for this request. Optional fields that aren't specified are set to null.</p>

#### `updateManagedInstanceRole`

``` purescript
updateManagedInstanceRole :: forall eff. UpdateManagedInstanceRoleRequest -> Aff (err :: RequestError | eff) UpdateManagedInstanceRoleResult
```

<p>Assigns or changes an Amazon Identity and Access Management (IAM) role to the managed instance.</p>

#### `updatePatchBaseline`

``` purescript
updatePatchBaseline :: forall eff. UpdatePatchBaselineRequest -> Aff (err :: RequestError | eff) UpdatePatchBaselineResult
```

<p>Modifies an existing patch baseline. Fields not specified in the request are left unchanged.</p> <note> <p>For information about valid key and value pairs in <code>PatchFilters</code> for each supported operating system type, see <a href="http://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PatchFilter.html">PatchFilter</a>.</p> </note>

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

##### Instances
``` purescript
Newtype AccountId _
```

#### `AccountIdList`

``` purescript
newtype AccountIdList
  = AccountIdList (Array AccountId)
```

##### Instances
``` purescript
Newtype AccountIdList _
```

#### `Activation`

``` purescript
newtype Activation
  = Activation { "ActivationId" :: NullOrUndefined (ActivationId), "Description" :: NullOrUndefined (ActivationDescription), "DefaultInstanceName" :: NullOrUndefined (DefaultInstanceName), "IamRole" :: NullOrUndefined (IamRole), "RegistrationLimit" :: NullOrUndefined (RegistrationLimit), "RegistrationsCount" :: NullOrUndefined (RegistrationsCount), "ExpirationDate" :: NullOrUndefined (ExpirationDate), "Expired" :: NullOrUndefined (Boolean), "CreatedDate" :: NullOrUndefined (CreatedDate) }
```

<p>An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.</p>

##### Instances
``` purescript
Newtype Activation _
```

#### `ActivationCode`

``` purescript
newtype ActivationCode
  = ActivationCode String
```

##### Instances
``` purescript
Newtype ActivationCode _
```

#### `ActivationDescription`

``` purescript
newtype ActivationDescription
  = ActivationDescription String
```

##### Instances
``` purescript
Newtype ActivationDescription _
```

#### `ActivationId`

``` purescript
newtype ActivationId
  = ActivationId String
```

##### Instances
``` purescript
Newtype ActivationId _
```

#### `ActivationList`

``` purescript
newtype ActivationList
  = ActivationList (Array Activation)
```

##### Instances
``` purescript
Newtype ActivationList _
```

#### `AddTagsToResourceRequest`

``` purescript
newtype AddTagsToResourceRequest
  = AddTagsToResourceRequest { "ResourceType" :: ResourceTypeForTagging, "ResourceId" :: ResourceId, "Tags" :: TagList }
```

##### Instances
``` purescript
Newtype AddTagsToResourceRequest _
```

#### `AddTagsToResourceResult`

``` purescript
newtype AddTagsToResourceResult
  = AddTagsToResourceResult {  }
```

##### Instances
``` purescript
Newtype AddTagsToResourceResult _
```

#### `AgentErrorCode`

``` purescript
newtype AgentErrorCode
  = AgentErrorCode String
```

##### Instances
``` purescript
Newtype AgentErrorCode _
```

#### `AggregatorSchemaOnly`

``` purescript
newtype AggregatorSchemaOnly
  = AggregatorSchemaOnly Boolean
```

##### Instances
``` purescript
Newtype AggregatorSchemaOnly _
```

#### `AllowedPattern`

``` purescript
newtype AllowedPattern
  = AllowedPattern String
```

##### Instances
``` purescript
Newtype AllowedPattern _
```

#### `AlreadyExistsException`

``` purescript
newtype AlreadyExistsException
  = AlreadyExistsException { "Message" :: NullOrUndefined (String) }
```

<p>Error returned if an attempt is made to register a patch group with a patch baseline that is already registered with a different patch baseline.</p>

##### Instances
``` purescript
Newtype AlreadyExistsException _
```

#### `ApproveAfterDays`

``` purescript
newtype ApproveAfterDays
  = ApproveAfterDays Int
```

##### Instances
``` purescript
Newtype ApproveAfterDays _
```

#### `AssociatedInstances`

``` purescript
newtype AssociatedInstances
  = AssociatedInstances {  }
```

<p>You must disassociate a document from all instances before you can delete it.</p>

##### Instances
``` purescript
Newtype AssociatedInstances _
```

#### `Association`

``` purescript
newtype Association
  = Association { "Name" :: NullOrUndefined (DocumentName), "InstanceId" :: NullOrUndefined (InstanceId), "AssociationId" :: NullOrUndefined (AssociationId), "AssociationVersion" :: NullOrUndefined (AssociationVersion), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Targets" :: NullOrUndefined (Targets), "LastExecutionDate" :: NullOrUndefined (DateTime), "Overview" :: NullOrUndefined (AssociationOverview), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Describes an association of a Systems Manager document and an instance.</p>

##### Instances
``` purescript
Newtype Association _
```

#### `AssociationAlreadyExists`

``` purescript
newtype AssociationAlreadyExists
  = AssociationAlreadyExists {  }
```

<p>The specified association already exists.</p>

##### Instances
``` purescript
Newtype AssociationAlreadyExists _
```

#### `AssociationDescription`

``` purescript
newtype AssociationDescription
  = AssociationDescription { "Name" :: NullOrUndefined (DocumentName), "InstanceId" :: NullOrUndefined (InstanceId), "AssociationVersion" :: NullOrUndefined (AssociationVersion), "Date" :: NullOrUndefined (DateTime), "LastUpdateAssociationDate" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (AssociationStatus), "Overview" :: NullOrUndefined (AssociationOverview), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Parameters" :: NullOrUndefined (Parameters), "AssociationId" :: NullOrUndefined (AssociationId), "Targets" :: NullOrUndefined (Targets), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "LastExecutionDate" :: NullOrUndefined (DateTime), "LastSuccessfulExecutionDate" :: NullOrUndefined (DateTime), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Describes the parameters for a document.</p>

##### Instances
``` purescript
Newtype AssociationDescription _
```

#### `AssociationDescriptionList`

``` purescript
newtype AssociationDescriptionList
  = AssociationDescriptionList (Array AssociationDescription)
```

##### Instances
``` purescript
Newtype AssociationDescriptionList _
```

#### `AssociationDoesNotExist`

``` purescript
newtype AssociationDoesNotExist
  = AssociationDoesNotExist { "Message" :: NullOrUndefined (String) }
```

<p>The specified association does not exist.</p>

##### Instances
``` purescript
Newtype AssociationDoesNotExist _
```

#### `AssociationFilter`

``` purescript
newtype AssociationFilter
  = AssociationFilter { "Key'" :: AssociationFilterKey, "Value'" :: AssociationFilterValue }
```

<p>Describes a filter.</p>

##### Instances
``` purescript
Newtype AssociationFilter _
```

#### `AssociationFilterKey`

``` purescript
newtype AssociationFilterKey
  = AssociationFilterKey String
```

##### Instances
``` purescript
Newtype AssociationFilterKey _
```

#### `AssociationFilterList`

``` purescript
newtype AssociationFilterList
  = AssociationFilterList (Array AssociationFilter)
```

##### Instances
``` purescript
Newtype AssociationFilterList _
```

#### `AssociationFilterValue`

``` purescript
newtype AssociationFilterValue
  = AssociationFilterValue String
```

##### Instances
``` purescript
Newtype AssociationFilterValue _
```

#### `AssociationId`

``` purescript
newtype AssociationId
  = AssociationId String
```

##### Instances
``` purescript
Newtype AssociationId _
```

#### `AssociationLimitExceeded`

``` purescript
newtype AssociationLimitExceeded
  = AssociationLimitExceeded {  }
```

<p>You can have at most 2,000 active associations.</p>

##### Instances
``` purescript
Newtype AssociationLimitExceeded _
```

#### `AssociationList`

``` purescript
newtype AssociationList
  = AssociationList (Array Association)
```

##### Instances
``` purescript
Newtype AssociationList _
```

#### `AssociationName`

``` purescript
newtype AssociationName
  = AssociationName String
```

##### Instances
``` purescript
Newtype AssociationName _
```

#### `AssociationOverview`

``` purescript
newtype AssociationOverview
  = AssociationOverview { "Status" :: NullOrUndefined (StatusName), "DetailedStatus" :: NullOrUndefined (StatusName), "AssociationStatusAggregatedCount" :: NullOrUndefined (AssociationStatusAggregatedCount) }
```

<p>Information about the association.</p>

##### Instances
``` purescript
Newtype AssociationOverview _
```

#### `AssociationStatus`

``` purescript
newtype AssociationStatus
  = AssociationStatus { "Date" :: DateTime, "Name" :: AssociationStatusName, "Message" :: StatusMessage, "AdditionalInfo" :: NullOrUndefined (StatusAdditionalInfo) }
```

<p>Describes an association status.</p>

##### Instances
``` purescript
Newtype AssociationStatus _
```

#### `AssociationStatusAggregatedCount`

``` purescript
newtype AssociationStatusAggregatedCount
  = AssociationStatusAggregatedCount (Map StatusName InstanceCount)
```

##### Instances
``` purescript
Newtype AssociationStatusAggregatedCount _
```

#### `AssociationStatusName`

``` purescript
newtype AssociationStatusName
  = AssociationStatusName String
```

##### Instances
``` purescript
Newtype AssociationStatusName _
```

#### `AssociationVersion`

``` purescript
newtype AssociationVersion
  = AssociationVersion String
```

##### Instances
``` purescript
Newtype AssociationVersion _
```

#### `AssociationVersionInfo`

``` purescript
newtype AssociationVersionInfo
  = AssociationVersionInfo { "AssociationId" :: NullOrUndefined (AssociationId), "AssociationVersion" :: NullOrUndefined (AssociationVersion), "CreatedDate" :: NullOrUndefined (DateTime), "Name" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Parameters" :: NullOrUndefined (Parameters), "Targets" :: NullOrUndefined (Targets), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Information about the association version.</p>

##### Instances
``` purescript
Newtype AssociationVersionInfo _
```

#### `AssociationVersionLimitExceeded`

``` purescript
newtype AssociationVersionLimitExceeded
  = AssociationVersionLimitExceeded { "Message" :: NullOrUndefined (String) }
```

<p>You have reached the maximum number versions allowed for an association. Each association has a limit of 1,000 versions. </p>

##### Instances
``` purescript
Newtype AssociationVersionLimitExceeded _
```

#### `AssociationVersionList`

``` purescript
newtype AssociationVersionList
  = AssociationVersionList (Array AssociationVersionInfo)
```

##### Instances
``` purescript
Newtype AssociationVersionList _
```

#### `AttributeName`

``` purescript
newtype AttributeName
  = AttributeName String
```

##### Instances
``` purescript
Newtype AttributeName _
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

##### Instances
``` purescript
Newtype AttributeValue _
```

#### `AutomationActionName`

``` purescript
newtype AutomationActionName
  = AutomationActionName String
```

##### Instances
``` purescript
Newtype AutomationActionName _
```

#### `AutomationDefinitionNotFoundException`

``` purescript
newtype AutomationDefinitionNotFoundException
  = AutomationDefinitionNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>An Automation document with the specified name could not be found.</p>

##### Instances
``` purescript
Newtype AutomationDefinitionNotFoundException _
```

#### `AutomationDefinitionVersionNotFoundException`

``` purescript
newtype AutomationDefinitionVersionNotFoundException
  = AutomationDefinitionVersionNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>An Automation document with the specified name and version could not be found.</p>

##### Instances
``` purescript
Newtype AutomationDefinitionVersionNotFoundException _
```

#### `AutomationExecution`

``` purescript
newtype AutomationExecution
  = AutomationExecution { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId), "DocumentName" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "ExecutionStartTime" :: NullOrUndefined (DateTime), "ExecutionEndTime" :: NullOrUndefined (DateTime), "AutomationExecutionStatus" :: NullOrUndefined (AutomationExecutionStatus), "StepExecutions" :: NullOrUndefined (StepExecutionList), "StepExecutionsTruncated" :: NullOrUndefined (Boolean), "Parameters" :: NullOrUndefined (AutomationParameterMap), "Outputs" :: NullOrUndefined (AutomationParameterMap), "FailureMessage" :: NullOrUndefined (String), "Mode" :: NullOrUndefined (ExecutionMode), "ParentAutomationExecutionId" :: NullOrUndefined (AutomationExecutionId), "ExecutedBy" :: NullOrUndefined (String), "CurrentStepName" :: NullOrUndefined (String), "CurrentAction" :: NullOrUndefined (String), "TargetParameterName" :: NullOrUndefined (AutomationParameterKey), "Targets" :: NullOrUndefined (Targets), "ResolvedTargets" :: NullOrUndefined (ResolvedTargets), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "Target" :: NullOrUndefined (String) }
```

<p>Detailed information about the current state of an individual Automation execution.</p>

##### Instances
``` purescript
Newtype AutomationExecution _
```

#### `AutomationExecutionFilter`

``` purescript
newtype AutomationExecutionFilter
  = AutomationExecutionFilter { "Key" :: AutomationExecutionFilterKey, "Values" :: AutomationExecutionFilterValueList }
```

<p>A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.</p>

##### Instances
``` purescript
Newtype AutomationExecutionFilter _
```

#### `AutomationExecutionFilterKey`

``` purescript
newtype AutomationExecutionFilterKey
  = AutomationExecutionFilterKey String
```

##### Instances
``` purescript
Newtype AutomationExecutionFilterKey _
```

#### `AutomationExecutionFilterList`

``` purescript
newtype AutomationExecutionFilterList
  = AutomationExecutionFilterList (Array AutomationExecutionFilter)
```

##### Instances
``` purescript
Newtype AutomationExecutionFilterList _
```

#### `AutomationExecutionFilterValue`

``` purescript
newtype AutomationExecutionFilterValue
  = AutomationExecutionFilterValue String
```

##### Instances
``` purescript
Newtype AutomationExecutionFilterValue _
```

#### `AutomationExecutionFilterValueList`

``` purescript
newtype AutomationExecutionFilterValueList
  = AutomationExecutionFilterValueList (Array AutomationExecutionFilterValue)
```

##### Instances
``` purescript
Newtype AutomationExecutionFilterValueList _
```

#### `AutomationExecutionId`

``` purescript
newtype AutomationExecutionId
  = AutomationExecutionId String
```

##### Instances
``` purescript
Newtype AutomationExecutionId _
```

#### `AutomationExecutionLimitExceededException`

``` purescript
newtype AutomationExecutionLimitExceededException
  = AutomationExecutionLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The number of simultaneously running Automation executions exceeded the allowable limit.</p>

##### Instances
``` purescript
Newtype AutomationExecutionLimitExceededException _
```

#### `AutomationExecutionMetadata`

``` purescript
newtype AutomationExecutionMetadata
  = AutomationExecutionMetadata { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId), "DocumentName" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "AutomationExecutionStatus" :: NullOrUndefined (AutomationExecutionStatus), "ExecutionStartTime" :: NullOrUndefined (DateTime), "ExecutionEndTime" :: NullOrUndefined (DateTime), "ExecutedBy" :: NullOrUndefined (String), "LogFile" :: NullOrUndefined (String), "Outputs" :: NullOrUndefined (AutomationParameterMap), "Mode" :: NullOrUndefined (ExecutionMode), "ParentAutomationExecutionId" :: NullOrUndefined (AutomationExecutionId), "CurrentStepName" :: NullOrUndefined (String), "CurrentAction" :: NullOrUndefined (String), "FailureMessage" :: NullOrUndefined (String), "TargetParameterName" :: NullOrUndefined (AutomationParameterKey), "Targets" :: NullOrUndefined (Targets), "ResolvedTargets" :: NullOrUndefined (ResolvedTargets), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "Target" :: NullOrUndefined (String) }
```

<p>Details about a specific Automation execution.</p>

##### Instances
``` purescript
Newtype AutomationExecutionMetadata _
```

#### `AutomationExecutionMetadataList`

``` purescript
newtype AutomationExecutionMetadataList
  = AutomationExecutionMetadataList (Array AutomationExecutionMetadata)
```

##### Instances
``` purescript
Newtype AutomationExecutionMetadataList _
```

#### `AutomationExecutionNotFoundException`

``` purescript
newtype AutomationExecutionNotFoundException
  = AutomationExecutionNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>There is no automation execution information for the requested automation execution ID.</p>

##### Instances
``` purescript
Newtype AutomationExecutionNotFoundException _
```

#### `AutomationExecutionStatus`

``` purescript
newtype AutomationExecutionStatus
  = AutomationExecutionStatus String
```

##### Instances
``` purescript
Newtype AutomationExecutionStatus _
```

#### `AutomationParameterKey`

``` purescript
newtype AutomationParameterKey
  = AutomationParameterKey String
```

##### Instances
``` purescript
Newtype AutomationParameterKey _
```

#### `AutomationParameterMap`

``` purescript
newtype AutomationParameterMap
  = AutomationParameterMap (Map AutomationParameterKey AutomationParameterValueList)
```

##### Instances
``` purescript
Newtype AutomationParameterMap _
```

#### `AutomationParameterValue`

``` purescript
newtype AutomationParameterValue
  = AutomationParameterValue String
```

##### Instances
``` purescript
Newtype AutomationParameterValue _
```

#### `AutomationParameterValueList`

``` purescript
newtype AutomationParameterValueList
  = AutomationParameterValueList (Array AutomationParameterValue)
```

##### Instances
``` purescript
Newtype AutomationParameterValueList _
```

#### `AutomationStepNotFoundException`

``` purescript
newtype AutomationStepNotFoundException
  = AutomationStepNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>The specified step name and execution ID don't exist. Verify the information and try again.</p>

##### Instances
``` purescript
Newtype AutomationStepNotFoundException _
```

#### `BaselineDescription`

``` purescript
newtype BaselineDescription
  = BaselineDescription String
```

##### Instances
``` purescript
Newtype BaselineDescription _
```

#### `BaselineId`

``` purescript
newtype BaselineId
  = BaselineId String
```

##### Instances
``` purescript
Newtype BaselineId _
```

#### `BaselineName`

``` purescript
newtype BaselineName
  = BaselineName String
```

##### Instances
``` purescript
Newtype BaselineName _
```

#### `BatchErrorMessage`

``` purescript
newtype BatchErrorMessage
  = BatchErrorMessage String
```

##### Instances
``` purescript
Newtype BatchErrorMessage _
```

#### `CancelCommandRequest`

``` purescript
newtype CancelCommandRequest
  = CancelCommandRequest { "CommandId" :: CommandId, "InstanceIds" :: NullOrUndefined (InstanceIdList) }
```

<p/>

##### Instances
``` purescript
Newtype CancelCommandRequest _
```

#### `CancelCommandResult`

``` purescript
newtype CancelCommandResult
  = CancelCommandResult {  }
```

<p>Whether or not the command was successfully canceled. There is no guarantee that a request can be canceled.</p>

##### Instances
``` purescript
Newtype CancelCommandResult _
```

#### `ClientToken`

``` purescript
newtype ClientToken
  = ClientToken String
```

##### Instances
``` purescript
Newtype ClientToken _
```

#### `Command`

``` purescript
newtype Command
  = Command { "CommandId" :: NullOrUndefined (CommandId), "DocumentName" :: NullOrUndefined (DocumentName), "Comment" :: NullOrUndefined (Comment), "ExpiresAfter" :: NullOrUndefined (DateTime), "Parameters" :: NullOrUndefined (Parameters), "InstanceIds" :: NullOrUndefined (InstanceIdList), "Targets" :: NullOrUndefined (Targets), "RequestedDateTime" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (CommandStatus), "StatusDetails" :: NullOrUndefined (StatusDetails), "OutputS3Region" :: NullOrUndefined (S3Region), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "TargetCount" :: NullOrUndefined (TargetCount), "CompletedCount" :: NullOrUndefined (CompletedCount), "ErrorCount" :: NullOrUndefined (ErrorCount), "ServiceRole" :: NullOrUndefined (ServiceRole), "NotificationConfig" :: NullOrUndefined (NotificationConfig) }
```

<p>Describes a command request.</p>

##### Instances
``` purescript
Newtype Command _
```

#### `CommandFilter`

``` purescript
newtype CommandFilter
  = CommandFilter { "Key'" :: CommandFilterKey, "Value'" :: CommandFilterValue }
```

<p>Describes a command filter.</p>

##### Instances
``` purescript
Newtype CommandFilter _
```

#### `CommandFilterKey`

``` purescript
newtype CommandFilterKey
  = CommandFilterKey String
```

##### Instances
``` purescript
Newtype CommandFilterKey _
```

#### `CommandFilterList`

``` purescript
newtype CommandFilterList
  = CommandFilterList (Array CommandFilter)
```

##### Instances
``` purescript
Newtype CommandFilterList _
```

#### `CommandFilterValue`

``` purescript
newtype CommandFilterValue
  = CommandFilterValue String
```

##### Instances
``` purescript
Newtype CommandFilterValue _
```

#### `CommandId`

``` purescript
newtype CommandId
  = CommandId String
```

##### Instances
``` purescript
Newtype CommandId _
```

#### `CommandInvocation`

``` purescript
newtype CommandInvocation
  = CommandInvocation { "CommandId" :: NullOrUndefined (CommandId), "InstanceId" :: NullOrUndefined (InstanceId), "InstanceName" :: NullOrUndefined (InstanceTagName), "Comment" :: NullOrUndefined (Comment), "DocumentName" :: NullOrUndefined (DocumentName), "RequestedDateTime" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (CommandInvocationStatus), "StatusDetails" :: NullOrUndefined (StatusDetails), "TraceOutput" :: NullOrUndefined (InvocationTraceOutput), "StandardOutputUrl" :: NullOrUndefined (Url), "StandardErrorUrl" :: NullOrUndefined (Url), "CommandPlugins" :: NullOrUndefined (CommandPluginList), "ServiceRole" :: NullOrUndefined (ServiceRole), "NotificationConfig" :: NullOrUndefined (NotificationConfig) }
```

<p>An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. A command invocation returns status and detail information about a command you executed. </p>

##### Instances
``` purescript
Newtype CommandInvocation _
```

#### `CommandInvocationList`

``` purescript
newtype CommandInvocationList
  = CommandInvocationList (Array CommandInvocation)
```

##### Instances
``` purescript
Newtype CommandInvocationList _
```

#### `CommandInvocationStatus`

``` purescript
newtype CommandInvocationStatus
  = CommandInvocationStatus String
```

##### Instances
``` purescript
Newtype CommandInvocationStatus _
```

#### `CommandList`

``` purescript
newtype CommandList
  = CommandList (Array Command)
```

##### Instances
``` purescript
Newtype CommandList _
```

#### `CommandMaxResults`

``` purescript
newtype CommandMaxResults
  = CommandMaxResults Int
```

##### Instances
``` purescript
Newtype CommandMaxResults _
```

#### `CommandPlugin`

``` purescript
newtype CommandPlugin
  = CommandPlugin { "Name" :: NullOrUndefined (CommandPluginName), "Status" :: NullOrUndefined (CommandPluginStatus), "StatusDetails" :: NullOrUndefined (StatusDetails), "ResponseCode" :: NullOrUndefined (ResponseCode), "ResponseStartDateTime" :: NullOrUndefined (DateTime), "ResponseFinishDateTime" :: NullOrUndefined (DateTime), "Output" :: NullOrUndefined (CommandPluginOutput), "StandardOutputUrl" :: NullOrUndefined (Url), "StandardErrorUrl" :: NullOrUndefined (Url), "OutputS3Region" :: NullOrUndefined (S3Region), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix) }
```

<p>Describes plugin details.</p>

##### Instances
``` purescript
Newtype CommandPlugin _
```

#### `CommandPluginList`

``` purescript
newtype CommandPluginList
  = CommandPluginList (Array CommandPlugin)
```

##### Instances
``` purescript
Newtype CommandPluginList _
```

#### `CommandPluginName`

``` purescript
newtype CommandPluginName
  = CommandPluginName String
```

##### Instances
``` purescript
Newtype CommandPluginName _
```

#### `CommandPluginOutput`

``` purescript
newtype CommandPluginOutput
  = CommandPluginOutput String
```

##### Instances
``` purescript
Newtype CommandPluginOutput _
```

#### `CommandPluginStatus`

``` purescript
newtype CommandPluginStatus
  = CommandPluginStatus String
```

##### Instances
``` purescript
Newtype CommandPluginStatus _
```

#### `CommandStatus`

``` purescript
newtype CommandStatus
  = CommandStatus String
```

##### Instances
``` purescript
Newtype CommandStatus _
```

#### `Comment`

``` purescript
newtype Comment
  = Comment String
```

##### Instances
``` purescript
Newtype Comment _
```

#### `CompletedCount`

``` purescript
newtype CompletedCount
  = CompletedCount Int
```

##### Instances
``` purescript
Newtype CompletedCount _
```

#### `ComplianceExecutionId`

``` purescript
newtype ComplianceExecutionId
  = ComplianceExecutionId String
```

##### Instances
``` purescript
Newtype ComplianceExecutionId _
```

#### `ComplianceExecutionSummary`

``` purescript
newtype ComplianceExecutionSummary
  = ComplianceExecutionSummary { "ExecutionTime" :: DateTime, "ExecutionId" :: NullOrUndefined (ComplianceExecutionId), "ExecutionType" :: NullOrUndefined (ComplianceExecutionType) }
```

<p>A summary of the call execution that includes an execution ID, the type of execution (for example, <code>Command</code>), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.</p>

##### Instances
``` purescript
Newtype ComplianceExecutionSummary _
```

#### `ComplianceExecutionType`

``` purescript
newtype ComplianceExecutionType
  = ComplianceExecutionType String
```

##### Instances
``` purescript
Newtype ComplianceExecutionType _
```

#### `ComplianceFilterValue`

``` purescript
newtype ComplianceFilterValue
  = ComplianceFilterValue String
```

##### Instances
``` purescript
Newtype ComplianceFilterValue _
```

#### `ComplianceItem`

``` purescript
newtype ComplianceItem
  = ComplianceItem { "ComplianceType" :: NullOrUndefined (ComplianceTypeName), "ResourceType" :: NullOrUndefined (ComplianceResourceType), "ResourceId" :: NullOrUndefined (ComplianceResourceId), "Id" :: NullOrUndefined (ComplianceItemId), "Title" :: NullOrUndefined (ComplianceItemTitle), "Status" :: NullOrUndefined (ComplianceStatus), "Severity" :: NullOrUndefined (ComplianceSeverity), "ExecutionSummary" :: NullOrUndefined (ComplianceExecutionSummary), "Details" :: NullOrUndefined (ComplianceItemDetails) }
```

<p>Information about the compliance as defined by the resource type. For example, for a patch resource type, <code>Items</code> includes information about the PatchSeverity, Classification, etc.</p>

##### Instances
``` purescript
Newtype ComplianceItem _
```

#### `ComplianceItemContentHash`

``` purescript
newtype ComplianceItemContentHash
  = ComplianceItemContentHash String
```

##### Instances
``` purescript
Newtype ComplianceItemContentHash _
```

#### `ComplianceItemDetails`

``` purescript
newtype ComplianceItemDetails
  = ComplianceItemDetails (Map AttributeName AttributeValue)
```

##### Instances
``` purescript
Newtype ComplianceItemDetails _
```

#### `ComplianceItemEntry`

``` purescript
newtype ComplianceItemEntry
  = ComplianceItemEntry { "Id" :: NullOrUndefined (ComplianceItemId), "Title" :: NullOrUndefined (ComplianceItemTitle), "Severity" :: ComplianceSeverity, "Status" :: ComplianceStatus, "Details" :: NullOrUndefined (ComplianceItemDetails) }
```

<p>Information about a compliance item.</p>

##### Instances
``` purescript
Newtype ComplianceItemEntry _
```

#### `ComplianceItemEntryList`

``` purescript
newtype ComplianceItemEntryList
  = ComplianceItemEntryList (Array ComplianceItemEntry)
```

##### Instances
``` purescript
Newtype ComplianceItemEntryList _
```

#### `ComplianceItemId`

``` purescript
newtype ComplianceItemId
  = ComplianceItemId String
```

##### Instances
``` purescript
Newtype ComplianceItemId _
```

#### `ComplianceItemList`

``` purescript
newtype ComplianceItemList
  = ComplianceItemList (Array ComplianceItem)
```

##### Instances
``` purescript
Newtype ComplianceItemList _
```

#### `ComplianceItemTitle`

``` purescript
newtype ComplianceItemTitle
  = ComplianceItemTitle String
```

##### Instances
``` purescript
Newtype ComplianceItemTitle _
```

#### `ComplianceQueryOperatorType`

``` purescript
newtype ComplianceQueryOperatorType
  = ComplianceQueryOperatorType String
```

##### Instances
``` purescript
Newtype ComplianceQueryOperatorType _
```

#### `ComplianceResourceId`

``` purescript
newtype ComplianceResourceId
  = ComplianceResourceId String
```

##### Instances
``` purescript
Newtype ComplianceResourceId _
```

#### `ComplianceResourceIdList`

``` purescript
newtype ComplianceResourceIdList
  = ComplianceResourceIdList (Array ComplianceResourceId)
```

##### Instances
``` purescript
Newtype ComplianceResourceIdList _
```

#### `ComplianceResourceType`

``` purescript
newtype ComplianceResourceType
  = ComplianceResourceType String
```

##### Instances
``` purescript
Newtype ComplianceResourceType _
```

#### `ComplianceResourceTypeList`

``` purescript
newtype ComplianceResourceTypeList
  = ComplianceResourceTypeList (Array ComplianceResourceType)
```

##### Instances
``` purescript
Newtype ComplianceResourceTypeList _
```

#### `ComplianceSeverity`

``` purescript
newtype ComplianceSeverity
  = ComplianceSeverity String
```

##### Instances
``` purescript
Newtype ComplianceSeverity _
```

#### `ComplianceStatus`

``` purescript
newtype ComplianceStatus
  = ComplianceStatus String
```

##### Instances
``` purescript
Newtype ComplianceStatus _
```

#### `ComplianceStringFilter`

``` purescript
newtype ComplianceStringFilter
  = ComplianceStringFilter { "Key" :: NullOrUndefined (ComplianceStringFilterKey), "Values" :: NullOrUndefined (ComplianceStringFilterValueList), "Type" :: NullOrUndefined (ComplianceQueryOperatorType) }
```

<p>One or more filters. Use a filter to return a more specific list of results.</p>

##### Instances
``` purescript
Newtype ComplianceStringFilter _
```

#### `ComplianceStringFilterKey`

``` purescript
newtype ComplianceStringFilterKey
  = ComplianceStringFilterKey String
```

##### Instances
``` purescript
Newtype ComplianceStringFilterKey _
```

#### `ComplianceStringFilterList`

``` purescript
newtype ComplianceStringFilterList
  = ComplianceStringFilterList (Array ComplianceStringFilter)
```

##### Instances
``` purescript
Newtype ComplianceStringFilterList _
```

#### `ComplianceStringFilterValueList`

``` purescript
newtype ComplianceStringFilterValueList
  = ComplianceStringFilterValueList (Array ComplianceFilterValue)
```

##### Instances
``` purescript
Newtype ComplianceStringFilterValueList _
```

#### `ComplianceSummaryCount`

``` purescript
newtype ComplianceSummaryCount
  = ComplianceSummaryCount Int
```

##### Instances
``` purescript
Newtype ComplianceSummaryCount _
```

#### `ComplianceSummaryItem`

``` purescript
newtype ComplianceSummaryItem
  = ComplianceSummaryItem { "ComplianceType" :: NullOrUndefined (ComplianceTypeName), "CompliantSummary" :: NullOrUndefined (CompliantSummary), "NonCompliantSummary" :: NullOrUndefined (NonCompliantSummary) }
```

<p>A summary of compliance information by compliance type.</p>

##### Instances
``` purescript
Newtype ComplianceSummaryItem _
```

#### `ComplianceSummaryItemList`

``` purescript
newtype ComplianceSummaryItemList
  = ComplianceSummaryItemList (Array ComplianceSummaryItem)
```

##### Instances
``` purescript
Newtype ComplianceSummaryItemList _
```

#### `ComplianceTypeCountLimitExceededException`

``` purescript
newtype ComplianceTypeCountLimitExceededException
  = ComplianceTypeCountLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>You specified too many custom compliance types. You can specify a maximum of 10 different types. </p>

##### Instances
``` purescript
Newtype ComplianceTypeCountLimitExceededException _
```

#### `ComplianceTypeName`

``` purescript
newtype ComplianceTypeName
  = ComplianceTypeName String
```

##### Instances
``` purescript
Newtype ComplianceTypeName _
```

#### `CompliantSummary`

``` purescript
newtype CompliantSummary
  = CompliantSummary { "CompliantCount" :: NullOrUndefined (ComplianceSummaryCount), "SeveritySummary" :: NullOrUndefined (SeveritySummary) }
```

<p>A summary of resources that are compliant. The summary is organized according to the resource count for each compliance type.</p>

##### Instances
``` purescript
Newtype CompliantSummary _
```

#### `ComputerName`

``` purescript
newtype ComputerName
  = ComputerName String
```

##### Instances
``` purescript
Newtype ComputerName _
```

#### `CreateActivationRequest`

``` purescript
newtype CreateActivationRequest
  = CreateActivationRequest { "Description" :: NullOrUndefined (ActivationDescription), "DefaultInstanceName" :: NullOrUndefined (DefaultInstanceName), "IamRole" :: IamRole, "RegistrationLimit" :: NullOrUndefined (RegistrationLimit), "ExpirationDate" :: NullOrUndefined (ExpirationDate) }
```

##### Instances
``` purescript
Newtype CreateActivationRequest _
```

#### `CreateActivationResult`

``` purescript
newtype CreateActivationResult
  = CreateActivationResult { "ActivationId" :: NullOrUndefined (ActivationId), "ActivationCode" :: NullOrUndefined (ActivationCode) }
```

##### Instances
``` purescript
Newtype CreateActivationResult _
```

#### `CreateAssociationBatchRequest`

``` purescript
newtype CreateAssociationBatchRequest
  = CreateAssociationBatchRequest { "Entries" :: CreateAssociationBatchRequestEntries }
```

##### Instances
``` purescript
Newtype CreateAssociationBatchRequest _
```

#### `CreateAssociationBatchRequestEntries`

``` purescript
newtype CreateAssociationBatchRequestEntries
  = CreateAssociationBatchRequestEntries (Array CreateAssociationBatchRequestEntry)
```

##### Instances
``` purescript
Newtype CreateAssociationBatchRequestEntries _
```

#### `CreateAssociationBatchRequestEntry`

``` purescript
newtype CreateAssociationBatchRequestEntry
  = CreateAssociationBatchRequestEntry { "Name" :: DocumentName, "InstanceId" :: NullOrUndefined (InstanceId), "Parameters" :: NullOrUndefined (Parameters), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Targets" :: NullOrUndefined (Targets), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Describes the association of a Systems Manager document and an instance.</p>

##### Instances
``` purescript
Newtype CreateAssociationBatchRequestEntry _
```

#### `CreateAssociationBatchResult`

``` purescript
newtype CreateAssociationBatchResult
  = CreateAssociationBatchResult { "Successful" :: NullOrUndefined (AssociationDescriptionList), "Failed" :: NullOrUndefined (FailedCreateAssociationList) }
```

##### Instances
``` purescript
Newtype CreateAssociationBatchResult _
```

#### `CreateAssociationRequest`

``` purescript
newtype CreateAssociationRequest
  = CreateAssociationRequest { "Name" :: DocumentName, "DocumentVersion" :: NullOrUndefined (DocumentVersion), "InstanceId" :: NullOrUndefined (InstanceId), "Parameters" :: NullOrUndefined (Parameters), "Targets" :: NullOrUndefined (Targets), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "AssociationName" :: NullOrUndefined (AssociationName) }
```

##### Instances
``` purescript
Newtype CreateAssociationRequest _
```

#### `CreateAssociationResult`

``` purescript
newtype CreateAssociationResult
  = CreateAssociationResult { "AssociationDescription" :: NullOrUndefined (AssociationDescription) }
```

##### Instances
``` purescript
Newtype CreateAssociationResult _
```

#### `CreateDocumentRequest`

``` purescript
newtype CreateDocumentRequest
  = CreateDocumentRequest { "Content" :: DocumentContent, "Name" :: DocumentName, "DocumentType" :: NullOrUndefined (DocumentType), "DocumentFormat" :: NullOrUndefined (DocumentFormat), "TargetType" :: NullOrUndefined (TargetType) }
```

##### Instances
``` purescript
Newtype CreateDocumentRequest _
```

#### `CreateDocumentResult`

``` purescript
newtype CreateDocumentResult
  = CreateDocumentResult { "DocumentDescription" :: NullOrUndefined (DocumentDescription) }
```

##### Instances
``` purescript
Newtype CreateDocumentResult _
```

#### `CreateMaintenanceWindowRequest`

``` purescript
newtype CreateMaintenanceWindowRequest
  = CreateMaintenanceWindowRequest { "Name" :: MaintenanceWindowName, "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Schedule" :: MaintenanceWindowSchedule, "Duration" :: MaintenanceWindowDurationHours, "Cutoff" :: MaintenanceWindowCutoff, "AllowUnassociatedTargets" :: MaintenanceWindowAllowUnassociatedTargets, "ClientToken" :: NullOrUndefined (ClientToken) }
```

##### Instances
``` purescript
Newtype CreateMaintenanceWindowRequest _
```

#### `CreateMaintenanceWindowResult`

``` purescript
newtype CreateMaintenanceWindowResult
  = CreateMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId) }
```

##### Instances
``` purescript
Newtype CreateMaintenanceWindowResult _
```

#### `CreatePatchBaselineRequest`

``` purescript
newtype CreatePatchBaselineRequest
  = CreatePatchBaselineRequest { "OperatingSystem" :: NullOrUndefined (OperatingSystem), "Name" :: BaselineName, "GlobalFilters" :: NullOrUndefined (PatchFilterGroup), "ApprovalRules" :: NullOrUndefined (PatchRuleGroup), "ApprovedPatches" :: NullOrUndefined (PatchIdList), "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean), "RejectedPatches" :: NullOrUndefined (PatchIdList), "Description" :: NullOrUndefined (BaselineDescription), "Sources" :: NullOrUndefined (PatchSourceList), "ClientToken" :: NullOrUndefined (ClientToken) }
```

##### Instances
``` purescript
Newtype CreatePatchBaselineRequest _
```

#### `CreatePatchBaselineResult`

``` purescript
newtype CreatePatchBaselineResult
  = CreatePatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId) }
```

##### Instances
``` purescript
Newtype CreatePatchBaselineResult _
```

#### `CreateResourceDataSyncRequest`

``` purescript
newtype CreateResourceDataSyncRequest
  = CreateResourceDataSyncRequest { "SyncName" :: ResourceDataSyncName, "S3Destination" :: ResourceDataSyncS3Destination }
```

##### Instances
``` purescript
Newtype CreateResourceDataSyncRequest _
```

#### `CreateResourceDataSyncResult`

``` purescript
newtype CreateResourceDataSyncResult
  = CreateResourceDataSyncResult {  }
```

##### Instances
``` purescript
Newtype CreateResourceDataSyncResult _
```

#### `CreatedDate`

``` purescript
newtype CreatedDate
  = CreatedDate Number
```

##### Instances
``` purescript
Newtype CreatedDate _
```

#### `CustomSchemaCountLimitExceededException`

``` purescript
newtype CustomSchemaCountLimitExceededException
  = CustomSchemaCountLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>You have exceeded the limit for custom schemas. Delete one or more custom schemas and try again.</p>

##### Instances
``` purescript
Newtype CustomSchemaCountLimitExceededException _
```

#### `DateTime`

``` purescript
newtype DateTime
  = DateTime Number
```

##### Instances
``` purescript
Newtype DateTime _
```

#### `DefaultBaseline`

``` purescript
newtype DefaultBaseline
  = DefaultBaseline Boolean
```

##### Instances
``` purescript
Newtype DefaultBaseline _
```

#### `DefaultInstanceName`

``` purescript
newtype DefaultInstanceName
  = DefaultInstanceName String
```

##### Instances
``` purescript
Newtype DefaultInstanceName _
```

#### `DeleteActivationRequest`

``` purescript
newtype DeleteActivationRequest
  = DeleteActivationRequest { "ActivationId" :: ActivationId }
```

##### Instances
``` purescript
Newtype DeleteActivationRequest _
```

#### `DeleteActivationResult`

``` purescript
newtype DeleteActivationResult
  = DeleteActivationResult {  }
```

##### Instances
``` purescript
Newtype DeleteActivationResult _
```

#### `DeleteAssociationRequest`

``` purescript
newtype DeleteAssociationRequest
  = DeleteAssociationRequest { "Name" :: NullOrUndefined (DocumentName), "InstanceId" :: NullOrUndefined (InstanceId), "AssociationId" :: NullOrUndefined (AssociationId) }
```

##### Instances
``` purescript
Newtype DeleteAssociationRequest _
```

#### `DeleteAssociationResult`

``` purescript
newtype DeleteAssociationResult
  = DeleteAssociationResult {  }
```

##### Instances
``` purescript
Newtype DeleteAssociationResult _
```

#### `DeleteDocumentRequest`

``` purescript
newtype DeleteDocumentRequest
  = DeleteDocumentRequest { "Name" :: DocumentName }
```

##### Instances
``` purescript
Newtype DeleteDocumentRequest _
```

#### `DeleteDocumentResult`

``` purescript
newtype DeleteDocumentResult
  = DeleteDocumentResult {  }
```

##### Instances
``` purescript
Newtype DeleteDocumentResult _
```

#### `DeleteMaintenanceWindowRequest`

``` purescript
newtype DeleteMaintenanceWindowRequest
  = DeleteMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId }
```

##### Instances
``` purescript
Newtype DeleteMaintenanceWindowRequest _
```

#### `DeleteMaintenanceWindowResult`

``` purescript
newtype DeleteMaintenanceWindowResult
  = DeleteMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId) }
```

##### Instances
``` purescript
Newtype DeleteMaintenanceWindowResult _
```

#### `DeleteParameterRequest`

``` purescript
newtype DeleteParameterRequest
  = DeleteParameterRequest { "Name" :: PSParameterName }
```

##### Instances
``` purescript
Newtype DeleteParameterRequest _
```

#### `DeleteParameterResult`

``` purescript
newtype DeleteParameterResult
  = DeleteParameterResult {  }
```

##### Instances
``` purescript
Newtype DeleteParameterResult _
```

#### `DeleteParametersRequest`

``` purescript
newtype DeleteParametersRequest
  = DeleteParametersRequest { "Names" :: ParameterNameList }
```

##### Instances
``` purescript
Newtype DeleteParametersRequest _
```

#### `DeleteParametersResult`

``` purescript
newtype DeleteParametersResult
  = DeleteParametersResult { "DeletedParameters" :: NullOrUndefined (ParameterNameList), "InvalidParameters" :: NullOrUndefined (ParameterNameList) }
```

##### Instances
``` purescript
Newtype DeleteParametersResult _
```

#### `DeletePatchBaselineRequest`

``` purescript
newtype DeletePatchBaselineRequest
  = DeletePatchBaselineRequest { "BaselineId" :: BaselineId }
```

##### Instances
``` purescript
Newtype DeletePatchBaselineRequest _
```

#### `DeletePatchBaselineResult`

``` purescript
newtype DeletePatchBaselineResult
  = DeletePatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId) }
```

##### Instances
``` purescript
Newtype DeletePatchBaselineResult _
```

#### `DeleteResourceDataSyncRequest`

``` purescript
newtype DeleteResourceDataSyncRequest
  = DeleteResourceDataSyncRequest { "SyncName" :: ResourceDataSyncName }
```

##### Instances
``` purescript
Newtype DeleteResourceDataSyncRequest _
```

#### `DeleteResourceDataSyncResult`

``` purescript
newtype DeleteResourceDataSyncResult
  = DeleteResourceDataSyncResult {  }
```

##### Instances
``` purescript
Newtype DeleteResourceDataSyncResult _
```

#### `DeregisterManagedInstanceRequest`

``` purescript
newtype DeregisterManagedInstanceRequest
  = DeregisterManagedInstanceRequest { "InstanceId" :: ManagedInstanceId }
```

##### Instances
``` purescript
Newtype DeregisterManagedInstanceRequest _
```

#### `DeregisterManagedInstanceResult`

``` purescript
newtype DeregisterManagedInstanceResult
  = DeregisterManagedInstanceResult {  }
```

##### Instances
``` purescript
Newtype DeregisterManagedInstanceResult _
```

#### `DeregisterPatchBaselineForPatchGroupRequest`

``` purescript
newtype DeregisterPatchBaselineForPatchGroupRequest
  = DeregisterPatchBaselineForPatchGroupRequest { "BaselineId" :: BaselineId, "PatchGroup" :: PatchGroup }
```

##### Instances
``` purescript
Newtype DeregisterPatchBaselineForPatchGroupRequest _
```

#### `DeregisterPatchBaselineForPatchGroupResult`

``` purescript
newtype DeregisterPatchBaselineForPatchGroupResult
  = DeregisterPatchBaselineForPatchGroupResult { "BaselineId" :: NullOrUndefined (BaselineId), "PatchGroup" :: NullOrUndefined (PatchGroup) }
```

##### Instances
``` purescript
Newtype DeregisterPatchBaselineForPatchGroupResult _
```

#### `DeregisterTargetFromMaintenanceWindowRequest`

``` purescript
newtype DeregisterTargetFromMaintenanceWindowRequest
  = DeregisterTargetFromMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "WindowTargetId" :: MaintenanceWindowTargetId, "Safe" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype DeregisterTargetFromMaintenanceWindowRequest _
```

#### `DeregisterTargetFromMaintenanceWindowResult`

``` purescript
newtype DeregisterTargetFromMaintenanceWindowResult
  = DeregisterTargetFromMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId) }
```

##### Instances
``` purescript
Newtype DeregisterTargetFromMaintenanceWindowResult _
```

#### `DeregisterTaskFromMaintenanceWindowRequest`

``` purescript
newtype DeregisterTaskFromMaintenanceWindowRequest
  = DeregisterTaskFromMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "WindowTaskId" :: MaintenanceWindowTaskId }
```

##### Instances
``` purescript
Newtype DeregisterTaskFromMaintenanceWindowRequest _
```

#### `DeregisterTaskFromMaintenanceWindowResult`

``` purescript
newtype DeregisterTaskFromMaintenanceWindowResult
  = DeregisterTaskFromMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId) }
```

##### Instances
``` purescript
Newtype DeregisterTaskFromMaintenanceWindowResult _
```

#### `DescribeActivationsFilter`

``` purescript
newtype DescribeActivationsFilter
  = DescribeActivationsFilter { "FilterKey" :: NullOrUndefined (DescribeActivationsFilterKeys), "FilterValues" :: NullOrUndefined (StringList) }
```

<p>Filter for the DescribeActivation API.</p>

##### Instances
``` purescript
Newtype DescribeActivationsFilter _
```

#### `DescribeActivationsFilterKeys`

``` purescript
newtype DescribeActivationsFilterKeys
  = DescribeActivationsFilterKeys String
```

##### Instances
``` purescript
Newtype DescribeActivationsFilterKeys _
```

#### `DescribeActivationsFilterList`

``` purescript
newtype DescribeActivationsFilterList
  = DescribeActivationsFilterList (Array DescribeActivationsFilter)
```

##### Instances
``` purescript
Newtype DescribeActivationsFilterList _
```

#### `DescribeActivationsRequest`

``` purescript
newtype DescribeActivationsRequest
  = DescribeActivationsRequest { "Filters" :: NullOrUndefined (DescribeActivationsFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeActivationsRequest _
```

#### `DescribeActivationsResult`

``` purescript
newtype DescribeActivationsResult
  = DescribeActivationsResult { "ActivationList" :: NullOrUndefined (ActivationList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeActivationsResult _
```

#### `DescribeAssociationRequest`

``` purescript
newtype DescribeAssociationRequest
  = DescribeAssociationRequest { "Name" :: NullOrUndefined (DocumentName), "InstanceId" :: NullOrUndefined (InstanceId), "AssociationId" :: NullOrUndefined (AssociationId), "AssociationVersion" :: NullOrUndefined (AssociationVersion) }
```

##### Instances
``` purescript
Newtype DescribeAssociationRequest _
```

#### `DescribeAssociationResult`

``` purescript
newtype DescribeAssociationResult
  = DescribeAssociationResult { "AssociationDescription" :: NullOrUndefined (AssociationDescription) }
```

##### Instances
``` purescript
Newtype DescribeAssociationResult _
```

#### `DescribeAutomationExecutionsRequest`

``` purescript
newtype DescribeAutomationExecutionsRequest
  = DescribeAutomationExecutionsRequest { "Filters" :: NullOrUndefined (AutomationExecutionFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeAutomationExecutionsRequest _
```

#### `DescribeAutomationExecutionsResult`

``` purescript
newtype DescribeAutomationExecutionsResult
  = DescribeAutomationExecutionsResult { "AutomationExecutionMetadataList" :: NullOrUndefined (AutomationExecutionMetadataList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeAutomationExecutionsResult _
```

#### `DescribeAutomationStepExecutionsRequest`

``` purescript
newtype DescribeAutomationStepExecutionsRequest
  = DescribeAutomationStepExecutionsRequest { "AutomationExecutionId" :: AutomationExecutionId, "Filters" :: NullOrUndefined (StepExecutionFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "ReverseOrder" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype DescribeAutomationStepExecutionsRequest _
```

#### `DescribeAutomationStepExecutionsResult`

``` purescript
newtype DescribeAutomationStepExecutionsResult
  = DescribeAutomationStepExecutionsResult { "StepExecutions" :: NullOrUndefined (StepExecutionList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeAutomationStepExecutionsResult _
```

#### `DescribeAvailablePatchesRequest`

``` purescript
newtype DescribeAvailablePatchesRequest
  = DescribeAvailablePatchesRequest { "Filters" :: NullOrUndefined (PatchOrchestratorFilterList), "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeAvailablePatchesRequest _
```

#### `DescribeAvailablePatchesResult`

``` purescript
newtype DescribeAvailablePatchesResult
  = DescribeAvailablePatchesResult { "Patches" :: NullOrUndefined (PatchList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeAvailablePatchesResult _
```

#### `DescribeDocumentPermissionRequest`

``` purescript
newtype DescribeDocumentPermissionRequest
  = DescribeDocumentPermissionRequest { "Name" :: DocumentName, "PermissionType" :: DocumentPermissionType }
```

##### Instances
``` purescript
Newtype DescribeDocumentPermissionRequest _
```

#### `DescribeDocumentPermissionResponse`

``` purescript
newtype DescribeDocumentPermissionResponse
  = DescribeDocumentPermissionResponse { "AccountIds" :: NullOrUndefined (AccountIdList) }
```

##### Instances
``` purescript
Newtype DescribeDocumentPermissionResponse _
```

#### `DescribeDocumentRequest`

``` purescript
newtype DescribeDocumentRequest
  = DescribeDocumentRequest { "Name" :: DocumentARN, "DocumentVersion" :: NullOrUndefined (DocumentVersion) }
```

##### Instances
``` purescript
Newtype DescribeDocumentRequest _
```

#### `DescribeDocumentResult`

``` purescript
newtype DescribeDocumentResult
  = DescribeDocumentResult { "Document" :: NullOrUndefined (DocumentDescription) }
```

##### Instances
``` purescript
Newtype DescribeDocumentResult _
```

#### `DescribeEffectiveInstanceAssociationsRequest`

``` purescript
newtype DescribeEffectiveInstanceAssociationsRequest
  = DescribeEffectiveInstanceAssociationsRequest { "InstanceId" :: InstanceId, "MaxResults" :: NullOrUndefined (EffectiveInstanceAssociationMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeEffectiveInstanceAssociationsRequest _
```

#### `DescribeEffectiveInstanceAssociationsResult`

``` purescript
newtype DescribeEffectiveInstanceAssociationsResult
  = DescribeEffectiveInstanceAssociationsResult { "Associations" :: NullOrUndefined (InstanceAssociationList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeEffectiveInstanceAssociationsResult _
```

#### `DescribeEffectivePatchesForPatchBaselineRequest`

``` purescript
newtype DescribeEffectivePatchesForPatchBaselineRequest
  = DescribeEffectivePatchesForPatchBaselineRequest { "BaselineId" :: BaselineId, "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeEffectivePatchesForPatchBaselineRequest _
```

#### `DescribeEffectivePatchesForPatchBaselineResult`

``` purescript
newtype DescribeEffectivePatchesForPatchBaselineResult
  = DescribeEffectivePatchesForPatchBaselineResult { "EffectivePatches" :: NullOrUndefined (EffectivePatchList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeEffectivePatchesForPatchBaselineResult _
```

#### `DescribeInstanceAssociationsStatusRequest`

``` purescript
newtype DescribeInstanceAssociationsStatusRequest
  = DescribeInstanceAssociationsStatusRequest { "InstanceId" :: InstanceId, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeInstanceAssociationsStatusRequest _
```

#### `DescribeInstanceAssociationsStatusResult`

``` purescript
newtype DescribeInstanceAssociationsStatusResult
  = DescribeInstanceAssociationsStatusResult { "InstanceAssociationStatusInfos" :: NullOrUndefined (InstanceAssociationStatusInfos), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeInstanceAssociationsStatusResult _
```

#### `DescribeInstanceInformationRequest`

``` purescript
newtype DescribeInstanceInformationRequest
  = DescribeInstanceInformationRequest { "InstanceInformationFilterList" :: NullOrUndefined (InstanceInformationFilterList), "Filters" :: NullOrUndefined (InstanceInformationStringFilterList), "MaxResults" :: NullOrUndefined (MaxResultsEC2Compatible), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeInstanceInformationRequest _
```

#### `DescribeInstanceInformationResult`

``` purescript
newtype DescribeInstanceInformationResult
  = DescribeInstanceInformationResult { "InstanceInformationList" :: NullOrUndefined (InstanceInformationList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeInstanceInformationResult _
```

#### `DescribeInstancePatchStatesForPatchGroupRequest`

``` purescript
newtype DescribeInstancePatchStatesForPatchGroupRequest
  = DescribeInstancePatchStatesForPatchGroupRequest { "PatchGroup" :: PatchGroup, "Filters" :: NullOrUndefined (InstancePatchStateFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults) }
```

##### Instances
``` purescript
Newtype DescribeInstancePatchStatesForPatchGroupRequest _
```

#### `DescribeInstancePatchStatesForPatchGroupResult`

``` purescript
newtype DescribeInstancePatchStatesForPatchGroupResult
  = DescribeInstancePatchStatesForPatchGroupResult { "InstancePatchStates" :: NullOrUndefined (InstancePatchStatesList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeInstancePatchStatesForPatchGroupResult _
```

#### `DescribeInstancePatchStatesRequest`

``` purescript
newtype DescribeInstancePatchStatesRequest
  = DescribeInstancePatchStatesRequest { "InstanceIds" :: InstanceIdList, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults) }
```

##### Instances
``` purescript
Newtype DescribeInstancePatchStatesRequest _
```

#### `DescribeInstancePatchStatesResult`

``` purescript
newtype DescribeInstancePatchStatesResult
  = DescribeInstancePatchStatesResult { "InstancePatchStates" :: NullOrUndefined (InstancePatchStateList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeInstancePatchStatesResult _
```

#### `DescribeInstancePatchesRequest`

``` purescript
newtype DescribeInstancePatchesRequest
  = DescribeInstancePatchesRequest { "InstanceId" :: InstanceId, "Filters" :: NullOrUndefined (PatchOrchestratorFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults) }
```

##### Instances
``` purescript
Newtype DescribeInstancePatchesRequest _
```

#### `DescribeInstancePatchesResult`

``` purescript
newtype DescribeInstancePatchesResult
  = DescribeInstancePatchesResult { "Patches" :: NullOrUndefined (PatchComplianceDataList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeInstancePatchesResult _
```

#### `DescribeMaintenanceWindowExecutionTaskInvocationsRequest`

``` purescript
newtype DescribeMaintenanceWindowExecutionTaskInvocationsRequest
  = DescribeMaintenanceWindowExecutionTaskInvocationsRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId, "TaskId" :: MaintenanceWindowExecutionTaskId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowExecutionTaskInvocationsRequest _
```

#### `DescribeMaintenanceWindowExecutionTaskInvocationsResult`

``` purescript
newtype DescribeMaintenanceWindowExecutionTaskInvocationsResult
  = DescribeMaintenanceWindowExecutionTaskInvocationsResult { "WindowExecutionTaskInvocationIdentities" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationIdentityList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowExecutionTaskInvocationsResult _
```

#### `DescribeMaintenanceWindowExecutionTasksRequest`

``` purescript
newtype DescribeMaintenanceWindowExecutionTasksRequest
  = DescribeMaintenanceWindowExecutionTasksRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowExecutionTasksRequest _
```

#### `DescribeMaintenanceWindowExecutionTasksResult`

``` purescript
newtype DescribeMaintenanceWindowExecutionTasksResult
  = DescribeMaintenanceWindowExecutionTasksResult { "WindowExecutionTaskIdentities" :: NullOrUndefined (MaintenanceWindowExecutionTaskIdentityList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowExecutionTasksResult _
```

#### `DescribeMaintenanceWindowExecutionsRequest`

``` purescript
newtype DescribeMaintenanceWindowExecutionsRequest
  = DescribeMaintenanceWindowExecutionsRequest { "WindowId" :: MaintenanceWindowId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowExecutionsRequest _
```

#### `DescribeMaintenanceWindowExecutionsResult`

``` purescript
newtype DescribeMaintenanceWindowExecutionsResult
  = DescribeMaintenanceWindowExecutionsResult { "WindowExecutions" :: NullOrUndefined (MaintenanceWindowExecutionList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowExecutionsResult _
```

#### `DescribeMaintenanceWindowTargetsRequest`

``` purescript
newtype DescribeMaintenanceWindowTargetsRequest
  = DescribeMaintenanceWindowTargetsRequest { "WindowId" :: MaintenanceWindowId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowTargetsRequest _
```

#### `DescribeMaintenanceWindowTargetsResult`

``` purescript
newtype DescribeMaintenanceWindowTargetsResult
  = DescribeMaintenanceWindowTargetsResult { "Targets" :: NullOrUndefined (MaintenanceWindowTargetList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowTargetsResult _
```

#### `DescribeMaintenanceWindowTasksRequest`

``` purescript
newtype DescribeMaintenanceWindowTasksRequest
  = DescribeMaintenanceWindowTasksRequest { "WindowId" :: MaintenanceWindowId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowTasksRequest _
```

#### `DescribeMaintenanceWindowTasksResult`

``` purescript
newtype DescribeMaintenanceWindowTasksResult
  = DescribeMaintenanceWindowTasksResult { "Tasks" :: NullOrUndefined (MaintenanceWindowTaskList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowTasksResult _
```

#### `DescribeMaintenanceWindowsRequest`

``` purescript
newtype DescribeMaintenanceWindowsRequest
  = DescribeMaintenanceWindowsRequest { "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowsRequest _
```

#### `DescribeMaintenanceWindowsResult`

``` purescript
newtype DescribeMaintenanceWindowsResult
  = DescribeMaintenanceWindowsResult { "WindowIdentities" :: NullOrUndefined (MaintenanceWindowIdentityList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMaintenanceWindowsResult _
```

#### `DescribeParametersRequest`

``` purescript
newtype DescribeParametersRequest
  = DescribeParametersRequest { "Filters" :: NullOrUndefined (ParametersFilterList), "ParameterFilters" :: NullOrUndefined (ParameterStringFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeParametersRequest _
```

#### `DescribeParametersResult`

``` purescript
newtype DescribeParametersResult
  = DescribeParametersResult { "Parameters" :: NullOrUndefined (ParameterMetadataList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeParametersResult _
```

#### `DescribePatchBaselinesRequest`

``` purescript
newtype DescribePatchBaselinesRequest
  = DescribePatchBaselinesRequest { "Filters" :: NullOrUndefined (PatchOrchestratorFilterList), "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribePatchBaselinesRequest _
```

#### `DescribePatchBaselinesResult`

``` purescript
newtype DescribePatchBaselinesResult
  = DescribePatchBaselinesResult { "BaselineIdentities" :: NullOrUndefined (PatchBaselineIdentityList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribePatchBaselinesResult _
```

#### `DescribePatchGroupStateRequest`

``` purescript
newtype DescribePatchGroupStateRequest
  = DescribePatchGroupStateRequest { "PatchGroup" :: PatchGroup }
```

##### Instances
``` purescript
Newtype DescribePatchGroupStateRequest _
```

#### `DescribePatchGroupStateResult`

``` purescript
newtype DescribePatchGroupStateResult
  = DescribePatchGroupStateResult { "Instances" :: NullOrUndefined (Int), "InstancesWithInstalledPatches" :: NullOrUndefined (Int), "InstancesWithInstalledOtherPatches" :: NullOrUndefined (Int), "InstancesWithMissingPatches" :: NullOrUndefined (Int), "InstancesWithFailedPatches" :: NullOrUndefined (Int), "InstancesWithNotApplicablePatches" :: NullOrUndefined (Int) }
```

##### Instances
``` purescript
Newtype DescribePatchGroupStateResult _
```

#### `DescribePatchGroupsRequest`

``` purescript
newtype DescribePatchGroupsRequest
  = DescribePatchGroupsRequest { "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults), "Filters" :: NullOrUndefined (PatchOrchestratorFilterList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribePatchGroupsRequest _
```

#### `DescribePatchGroupsResult`

``` purescript
newtype DescribePatchGroupsResult
  = DescribePatchGroupsResult { "Mappings" :: NullOrUndefined (PatchGroupPatchBaselineMappingList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribePatchGroupsResult _
```

#### `DescriptionInDocument`

``` purescript
newtype DescriptionInDocument
  = DescriptionInDocument String
```

##### Instances
``` purescript
Newtype DescriptionInDocument _
```

#### `DocumentARN`

``` purescript
newtype DocumentARN
  = DocumentARN String
```

##### Instances
``` purescript
Newtype DocumentARN _
```

#### `DocumentAlreadyExists`

``` purescript
newtype DocumentAlreadyExists
  = DocumentAlreadyExists { "Message" :: NullOrUndefined (String) }
```

<p>The specified document already exists.</p>

##### Instances
``` purescript
Newtype DocumentAlreadyExists _
```

#### `DocumentContent`

``` purescript
newtype DocumentContent
  = DocumentContent String
```

##### Instances
``` purescript
Newtype DocumentContent _
```

#### `DocumentDefaultVersionDescription`

``` purescript
newtype DocumentDefaultVersionDescription
  = DocumentDefaultVersionDescription { "Name" :: NullOrUndefined (DocumentName), "DefaultVersion" :: NullOrUndefined (DocumentVersion) }
```

<p>A default version of a document.</p>

##### Instances
``` purescript
Newtype DocumentDefaultVersionDescription _
```

#### `DocumentDescription`

``` purescript
newtype DocumentDescription
  = DocumentDescription { "Sha1" :: NullOrUndefined (DocumentSha1), "Hash" :: NullOrUndefined (DocumentHash), "HashType" :: NullOrUndefined (DocumentHashType), "Name" :: NullOrUndefined (DocumentARN), "Owner" :: NullOrUndefined (DocumentOwner), "CreatedDate" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (DocumentStatus), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Description" :: NullOrUndefined (DescriptionInDocument), "Parameters" :: NullOrUndefined (DocumentParameterList), "PlatformTypes" :: NullOrUndefined (PlatformTypeList), "DocumentType" :: NullOrUndefined (DocumentType), "SchemaVersion" :: NullOrUndefined (DocumentSchemaVersion), "LatestVersion" :: NullOrUndefined (DocumentVersion), "DefaultVersion" :: NullOrUndefined (DocumentVersion), "DocumentFormat" :: NullOrUndefined (DocumentFormat), "TargetType" :: NullOrUndefined (TargetType), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a Systems Manager document. </p>

##### Instances
``` purescript
Newtype DocumentDescription _
```

#### `DocumentFilter`

``` purescript
newtype DocumentFilter
  = DocumentFilter { "Key'" :: DocumentFilterKey, "Value'" :: DocumentFilterValue }
```

<p>Describes a filter.</p>

##### Instances
``` purescript
Newtype DocumentFilter _
```

#### `DocumentFilterKey`

``` purescript
newtype DocumentFilterKey
  = DocumentFilterKey String
```

##### Instances
``` purescript
Newtype DocumentFilterKey _
```

#### `DocumentFilterList`

``` purescript
newtype DocumentFilterList
  = DocumentFilterList (Array DocumentFilter)
```

##### Instances
``` purescript
Newtype DocumentFilterList _
```

#### `DocumentFilterValue`

``` purescript
newtype DocumentFilterValue
  = DocumentFilterValue String
```

##### Instances
``` purescript
Newtype DocumentFilterValue _
```

#### `DocumentFormat`

``` purescript
newtype DocumentFormat
  = DocumentFormat String
```

##### Instances
``` purescript
Newtype DocumentFormat _
```

#### `DocumentHash`

``` purescript
newtype DocumentHash
  = DocumentHash String
```

##### Instances
``` purescript
Newtype DocumentHash _
```

#### `DocumentHashType`

``` purescript
newtype DocumentHashType
  = DocumentHashType String
```

##### Instances
``` purescript
Newtype DocumentHashType _
```

#### `DocumentIdentifier`

``` purescript
newtype DocumentIdentifier
  = DocumentIdentifier { "Name" :: NullOrUndefined (DocumentARN), "Owner" :: NullOrUndefined (DocumentOwner), "PlatformTypes" :: NullOrUndefined (PlatformTypeList), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "DocumentType" :: NullOrUndefined (DocumentType), "SchemaVersion" :: NullOrUndefined (DocumentSchemaVersion), "DocumentFormat" :: NullOrUndefined (DocumentFormat), "TargetType" :: NullOrUndefined (TargetType), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes the name of a Systems Manager document.</p>

##### Instances
``` purescript
Newtype DocumentIdentifier _
```

#### `DocumentIdentifierList`

``` purescript
newtype DocumentIdentifierList
  = DocumentIdentifierList (Array DocumentIdentifier)
```

##### Instances
``` purescript
Newtype DocumentIdentifierList _
```

#### `DocumentKeyValuesFilter`

``` purescript
newtype DocumentKeyValuesFilter
  = DocumentKeyValuesFilter { "Key" :: NullOrUndefined (DocumentKeyValuesFilterKey), "Values" :: NullOrUndefined (DocumentKeyValuesFilterValues) }
```

<p>One or more filters. Use a filter to return a more specific list of documents.</p> <p>For keys, you can specify one or more tags that have been applied to a document. </p> <p>Other valid values include Owner, Name, PlatformTypes, and DocumentType.</p> <p>Note that only one Owner can be specified in a request. For example: <code>Key=Owner,Values=Self</code>.</p> <p>If you use Name as a key, you can use a name prefix to return a list of documents. For example, in the AWS CLI, to return a list of all documents that begin with <code>Te</code>, run the following command:</p> <p> <code>aws ssm list-documents --filters Key=Name,Values=Te</code> </p> <p>If you specify more than two keys, only documents that are identified by all the tags are returned in the results. If you specify more than two values for a key, documents that are identified by any of the values are returned in the results.</p> <p>To specify a custom key and value pair, use the format <code>Key=tag:[tagName],Values=[valueName]</code>.</p> <p>For example, if you created a Key called region and are using the AWS CLI to call the <code>list-documents</code> command: </p> <p> <code>aws ssm list-documents --filters Key=tag:region,Values=east,west Key=Owner,Values=Self</code> </p>

##### Instances
``` purescript
Newtype DocumentKeyValuesFilter _
```

#### `DocumentKeyValuesFilterKey`

``` purescript
newtype DocumentKeyValuesFilterKey
  = DocumentKeyValuesFilterKey String
```

##### Instances
``` purescript
Newtype DocumentKeyValuesFilterKey _
```

#### `DocumentKeyValuesFilterList`

``` purescript
newtype DocumentKeyValuesFilterList
  = DocumentKeyValuesFilterList (Array DocumentKeyValuesFilter)
```

##### Instances
``` purescript
Newtype DocumentKeyValuesFilterList _
```

#### `DocumentKeyValuesFilterValue`

``` purescript
newtype DocumentKeyValuesFilterValue
  = DocumentKeyValuesFilterValue String
```

##### Instances
``` purescript
Newtype DocumentKeyValuesFilterValue _
```

#### `DocumentKeyValuesFilterValues`

``` purescript
newtype DocumentKeyValuesFilterValues
  = DocumentKeyValuesFilterValues (Array DocumentKeyValuesFilterValue)
```

##### Instances
``` purescript
Newtype DocumentKeyValuesFilterValues _
```

#### `DocumentLimitExceeded`

``` purescript
newtype DocumentLimitExceeded
  = DocumentLimitExceeded { "Message" :: NullOrUndefined (String) }
```

<p>You can have at most 200 active Systems Manager documents.</p>

##### Instances
``` purescript
Newtype DocumentLimitExceeded _
```

#### `DocumentName`

``` purescript
newtype DocumentName
  = DocumentName String
```

##### Instances
``` purescript
Newtype DocumentName _
```

#### `DocumentOwner`

``` purescript
newtype DocumentOwner
  = DocumentOwner String
```

##### Instances
``` purescript
Newtype DocumentOwner _
```

#### `DocumentParameter`

``` purescript
newtype DocumentParameter
  = DocumentParameter { "Name" :: NullOrUndefined (DocumentParameterName), "Type" :: NullOrUndefined (DocumentParameterType), "Description" :: NullOrUndefined (DocumentParameterDescrption), "DefaultValue" :: NullOrUndefined (DocumentParameterDefaultValue) }
```

<p>Parameters specified in a System Manager document that execute on the server when the command is run. </p>

##### Instances
``` purescript
Newtype DocumentParameter _
```

#### `DocumentParameterDefaultValue`

``` purescript
newtype DocumentParameterDefaultValue
  = DocumentParameterDefaultValue String
```

##### Instances
``` purescript
Newtype DocumentParameterDefaultValue _
```

#### `DocumentParameterDescrption`

``` purescript
newtype DocumentParameterDescrption
  = DocumentParameterDescrption String
```

##### Instances
``` purescript
Newtype DocumentParameterDescrption _
```

#### `DocumentParameterList`

``` purescript
newtype DocumentParameterList
  = DocumentParameterList (Array DocumentParameter)
```

##### Instances
``` purescript
Newtype DocumentParameterList _
```

#### `DocumentParameterName`

``` purescript
newtype DocumentParameterName
  = DocumentParameterName String
```

##### Instances
``` purescript
Newtype DocumentParameterName _
```

#### `DocumentParameterType`

``` purescript
newtype DocumentParameterType
  = DocumentParameterType String
```

##### Instances
``` purescript
Newtype DocumentParameterType _
```

#### `DocumentPermissionLimit`

``` purescript
newtype DocumentPermissionLimit
  = DocumentPermissionLimit { "Message" :: NullOrUndefined (String) }
```

<p>The document cannot be shared with more AWS user accounts. You can share a document with a maximum of 20 accounts. You can publicly share up to five documents. If you need to increase this limit, contact AWS Support.</p>

##### Instances
``` purescript
Newtype DocumentPermissionLimit _
```

#### `DocumentPermissionType`

``` purescript
newtype DocumentPermissionType
  = DocumentPermissionType String
```

##### Instances
``` purescript
Newtype DocumentPermissionType _
```

#### `DocumentSchemaVersion`

``` purescript
newtype DocumentSchemaVersion
  = DocumentSchemaVersion String
```

##### Instances
``` purescript
Newtype DocumentSchemaVersion _
```

#### `DocumentSha1`

``` purescript
newtype DocumentSha1
  = DocumentSha1 String
```

##### Instances
``` purescript
Newtype DocumentSha1 _
```

#### `DocumentStatus`

``` purescript
newtype DocumentStatus
  = DocumentStatus String
```

##### Instances
``` purescript
Newtype DocumentStatus _
```

#### `DocumentType`

``` purescript
newtype DocumentType
  = DocumentType String
```

##### Instances
``` purescript
Newtype DocumentType _
```

#### `DocumentVersion`

``` purescript
newtype DocumentVersion
  = DocumentVersion String
```

##### Instances
``` purescript
Newtype DocumentVersion _
```

#### `DocumentVersionInfo`

``` purescript
newtype DocumentVersionInfo
  = DocumentVersionInfo { "Name" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "CreatedDate" :: NullOrUndefined (DateTime), "IsDefaultVersion" :: NullOrUndefined (Boolean), "DocumentFormat" :: NullOrUndefined (DocumentFormat) }
```

<p>Version information about the document.</p>

##### Instances
``` purescript
Newtype DocumentVersionInfo _
```

#### `DocumentVersionLimitExceeded`

``` purescript
newtype DocumentVersionLimitExceeded
  = DocumentVersionLimitExceeded { "Message" :: NullOrUndefined (String) }
```

<p>The document has too many versions. Delete one or more document versions and try again.</p>

##### Instances
``` purescript
Newtype DocumentVersionLimitExceeded _
```

#### `DocumentVersionList`

``` purescript
newtype DocumentVersionList
  = DocumentVersionList (Array DocumentVersionInfo)
```

##### Instances
``` purescript
Newtype DocumentVersionList _
```

#### `DocumentVersionNumber`

``` purescript
newtype DocumentVersionNumber
  = DocumentVersionNumber String
```

##### Instances
``` purescript
Newtype DocumentVersionNumber _
```

#### `DoesNotExistException`

``` purescript
newtype DoesNotExistException
  = DoesNotExistException { "Message" :: NullOrUndefined (String) }
```

<p>Error returned when the ID specified for a resource, such as a Maintenance Window or Patch baseline, doesn't exist.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>

##### Instances
``` purescript
Newtype DoesNotExistException _
```

#### `DuplicateDocumentContent`

``` purescript
newtype DuplicateDocumentContent
  = DuplicateDocumentContent { "Message" :: NullOrUndefined (String) }
```

<p>The content of the association document matches another document. Change the content of the document and try again.</p>

##### Instances
``` purescript
Newtype DuplicateDocumentContent _
```

#### `DuplicateInstanceId`

``` purescript
newtype DuplicateInstanceId
  = DuplicateInstanceId {  }
```

<p>You cannot specify an instance ID in more than one association.</p>

##### Instances
``` purescript
Newtype DuplicateInstanceId _
```

#### `EffectiveInstanceAssociationMaxResults`

``` purescript
newtype EffectiveInstanceAssociationMaxResults
  = EffectiveInstanceAssociationMaxResults Int
```

##### Instances
``` purescript
Newtype EffectiveInstanceAssociationMaxResults _
```

#### `EffectivePatch`

``` purescript
newtype EffectivePatch
  = EffectivePatch { "Patch" :: NullOrUndefined (Patch), "PatchStatus" :: NullOrUndefined (PatchStatus) }
```

<p>The EffectivePatch structure defines metadata about a patch along with the approval state of the patch in a particular patch baseline. The approval state includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.</p>

##### Instances
``` purescript
Newtype EffectivePatch _
```

#### `EffectivePatchList`

``` purescript
newtype EffectivePatchList
  = EffectivePatchList (Array EffectivePatch)
```

##### Instances
``` purescript
Newtype EffectivePatchList _
```

#### `ErrorCount`

``` purescript
newtype ErrorCount
  = ErrorCount Int
```

##### Instances
``` purescript
Newtype ErrorCount _
```

#### `ExecutionMode`

``` purescript
newtype ExecutionMode
  = ExecutionMode String
```

##### Instances
``` purescript
Newtype ExecutionMode _
```

#### `ExpirationDate`

``` purescript
newtype ExpirationDate
  = ExpirationDate Number
```

##### Instances
``` purescript
Newtype ExpirationDate _
```

#### `FailedCreateAssociation`

``` purescript
newtype FailedCreateAssociation
  = FailedCreateAssociation { "Entry" :: NullOrUndefined (CreateAssociationBatchRequestEntry), "Message" :: NullOrUndefined (BatchErrorMessage), "Fault" :: NullOrUndefined (Fault) }
```

<p>Describes a failed association.</p>

##### Instances
``` purescript
Newtype FailedCreateAssociation _
```

#### `FailedCreateAssociationList`

``` purescript
newtype FailedCreateAssociationList
  = FailedCreateAssociationList (Array FailedCreateAssociation)
```

##### Instances
``` purescript
Newtype FailedCreateAssociationList _
```

#### `FailureDetails`

``` purescript
newtype FailureDetails
  = FailureDetails { "FailureStage" :: NullOrUndefined (String), "FailureType" :: NullOrUndefined (String), "Details" :: NullOrUndefined (AutomationParameterMap) }
```

<p>Information about an Automation failure.</p>

##### Instances
``` purescript
Newtype FailureDetails _
```

#### `Fault`

``` purescript
newtype Fault
  = Fault String
```

##### Instances
``` purescript
Newtype Fault _
```

#### `FeatureNotAvailableException`

``` purescript
newtype FeatureNotAvailableException
  = FeatureNotAvailableException { "Message" :: NullOrUndefined (String) }
```

<p>You attempted to register a LAMBDA or STEP_FUNCTION task in a region where the corresponding service is not available. </p>

##### Instances
``` purescript
Newtype FeatureNotAvailableException _
```

#### `GetAutomationExecutionRequest`

``` purescript
newtype GetAutomationExecutionRequest
  = GetAutomationExecutionRequest { "AutomationExecutionId" :: AutomationExecutionId }
```

##### Instances
``` purescript
Newtype GetAutomationExecutionRequest _
```

#### `GetAutomationExecutionResult`

``` purescript
newtype GetAutomationExecutionResult
  = GetAutomationExecutionResult { "AutomationExecution" :: NullOrUndefined (AutomationExecution) }
```

##### Instances
``` purescript
Newtype GetAutomationExecutionResult _
```

#### `GetCommandInvocationRequest`

``` purescript
newtype GetCommandInvocationRequest
  = GetCommandInvocationRequest { "CommandId" :: CommandId, "InstanceId" :: InstanceId, "PluginName" :: NullOrUndefined (CommandPluginName) }
```

##### Instances
``` purescript
Newtype GetCommandInvocationRequest _
```

#### `GetCommandInvocationResult`

``` purescript
newtype GetCommandInvocationResult
  = GetCommandInvocationResult { "CommandId" :: NullOrUndefined (CommandId), "InstanceId" :: NullOrUndefined (InstanceId), "Comment" :: NullOrUndefined (Comment), "DocumentName" :: NullOrUndefined (DocumentName), "PluginName" :: NullOrUndefined (CommandPluginName), "ResponseCode" :: NullOrUndefined (ResponseCode), "ExecutionStartDateTime" :: NullOrUndefined (StringDateTime), "ExecutionElapsedTime" :: NullOrUndefined (StringDateTime), "ExecutionEndDateTime" :: NullOrUndefined (StringDateTime), "Status" :: NullOrUndefined (CommandInvocationStatus), "StatusDetails" :: NullOrUndefined (StatusDetails), "StandardOutputContent" :: NullOrUndefined (StandardOutputContent), "StandardOutputUrl" :: NullOrUndefined (Url), "StandardErrorContent" :: NullOrUndefined (StandardErrorContent), "StandardErrorUrl" :: NullOrUndefined (Url) }
```

##### Instances
``` purescript
Newtype GetCommandInvocationResult _
```

#### `GetDefaultPatchBaselineRequest`

``` purescript
newtype GetDefaultPatchBaselineRequest
  = GetDefaultPatchBaselineRequest { "OperatingSystem" :: NullOrUndefined (OperatingSystem) }
```

##### Instances
``` purescript
Newtype GetDefaultPatchBaselineRequest _
```

#### `GetDefaultPatchBaselineResult`

``` purescript
newtype GetDefaultPatchBaselineResult
  = GetDefaultPatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId), "OperatingSystem" :: NullOrUndefined (OperatingSystem) }
```

##### Instances
``` purescript
Newtype GetDefaultPatchBaselineResult _
```

#### `GetDeployablePatchSnapshotForInstanceRequest`

``` purescript
newtype GetDeployablePatchSnapshotForInstanceRequest
  = GetDeployablePatchSnapshotForInstanceRequest { "InstanceId" :: InstanceId, "SnapshotId" :: SnapshotId }
```

##### Instances
``` purescript
Newtype GetDeployablePatchSnapshotForInstanceRequest _
```

#### `GetDeployablePatchSnapshotForInstanceResult`

``` purescript
newtype GetDeployablePatchSnapshotForInstanceResult
  = GetDeployablePatchSnapshotForInstanceResult { "InstanceId" :: NullOrUndefined (InstanceId), "SnapshotId" :: NullOrUndefined (SnapshotId), "SnapshotDownloadUrl" :: NullOrUndefined (SnapshotDownloadUrl), "Product" :: NullOrUndefined (Product) }
```

##### Instances
``` purescript
Newtype GetDeployablePatchSnapshotForInstanceResult _
```

#### `GetDocumentRequest`

``` purescript
newtype GetDocumentRequest
  = GetDocumentRequest { "Name" :: DocumentARN, "DocumentVersion" :: NullOrUndefined (DocumentVersion), "DocumentFormat" :: NullOrUndefined (DocumentFormat) }
```

##### Instances
``` purescript
Newtype GetDocumentRequest _
```

#### `GetDocumentResult`

``` purescript
newtype GetDocumentResult
  = GetDocumentResult { "Name" :: NullOrUndefined (DocumentARN), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Content" :: NullOrUndefined (DocumentContent), "DocumentType" :: NullOrUndefined (DocumentType), "DocumentFormat" :: NullOrUndefined (DocumentFormat) }
```

##### Instances
``` purescript
Newtype GetDocumentResult _
```

#### `GetInventoryRequest`

``` purescript
newtype GetInventoryRequest
  = GetInventoryRequest { "Filters" :: NullOrUndefined (InventoryFilterList), "Aggregators" :: NullOrUndefined (InventoryAggregatorList), "ResultAttributes" :: NullOrUndefined (ResultAttributeList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype GetInventoryRequest _
```

#### `GetInventoryResult`

``` purescript
newtype GetInventoryResult
  = GetInventoryResult { "Entities" :: NullOrUndefined (InventoryResultEntityList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetInventoryResult _
```

#### `GetInventorySchemaMaxResults`

``` purescript
newtype GetInventorySchemaMaxResults
  = GetInventorySchemaMaxResults Int
```

##### Instances
``` purescript
Newtype GetInventorySchemaMaxResults _
```

#### `GetInventorySchemaRequest`

``` purescript
newtype GetInventorySchemaRequest
  = GetInventorySchemaRequest { "TypeName" :: NullOrUndefined (InventoryItemTypeNameFilter), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (GetInventorySchemaMaxResults), "Aggregator" :: NullOrUndefined (AggregatorSchemaOnly), "SubType" :: NullOrUndefined (IsSubTypeSchema) }
```

##### Instances
``` purescript
Newtype GetInventorySchemaRequest _
```

#### `GetInventorySchemaResult`

``` purescript
newtype GetInventorySchemaResult
  = GetInventorySchemaResult { "Schemas" :: NullOrUndefined (InventoryItemSchemaResultList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetInventorySchemaResult _
```

#### `GetMaintenanceWindowExecutionRequest`

``` purescript
newtype GetMaintenanceWindowExecutionRequest
  = GetMaintenanceWindowExecutionRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowExecutionRequest _
```

#### `GetMaintenanceWindowExecutionResult`

``` purescript
newtype GetMaintenanceWindowExecutionResult
  = GetMaintenanceWindowExecutionResult { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskIds" :: NullOrUndefined (MaintenanceWindowExecutionTaskIdList), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime) }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowExecutionResult _
```

#### `GetMaintenanceWindowExecutionTaskInvocationRequest`

``` purescript
newtype GetMaintenanceWindowExecutionTaskInvocationRequest
  = GetMaintenanceWindowExecutionTaskInvocationRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId, "TaskId" :: MaintenanceWindowExecutionTaskId, "InvocationId" :: MaintenanceWindowExecutionTaskInvocationId }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowExecutionTaskInvocationRequest _
```

#### `GetMaintenanceWindowExecutionTaskInvocationResult`

``` purescript
newtype GetMaintenanceWindowExecutionTaskInvocationResult
  = GetMaintenanceWindowExecutionTaskInvocationResult { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId), "InvocationId" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationId), "ExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskExecutionId), "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType), "Parameters" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationParameters), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTaskTargetId) }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowExecutionTaskInvocationResult _
```

#### `GetMaintenanceWindowExecutionTaskRequest`

``` purescript
newtype GetMaintenanceWindowExecutionTaskRequest
  = GetMaintenanceWindowExecutionTaskRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId, "TaskId" :: MaintenanceWindowExecutionTaskId }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowExecutionTaskRequest _
```

#### `GetMaintenanceWindowExecutionTaskResult`

``` purescript
newtype GetMaintenanceWindowExecutionTaskResult
  = GetMaintenanceWindowExecutionTaskResult { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "ServiceRole" :: NullOrUndefined (ServiceRole), "Type" :: NullOrUndefined (MaintenanceWindowTaskType), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParametersList), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime) }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowExecutionTaskResult _
```

#### `GetMaintenanceWindowRequest`

``` purescript
newtype GetMaintenanceWindowRequest
  = GetMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowRequest _
```

#### `GetMaintenanceWindowResult`

``` purescript
newtype GetMaintenanceWindowResult
  = GetMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Schedule" :: NullOrUndefined (MaintenanceWindowSchedule), "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours), "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff), "AllowUnassociatedTargets" :: NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets), "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled), "CreatedDate" :: NullOrUndefined (DateTime), "ModifiedDate" :: NullOrUndefined (DateTime) }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowResult _
```

#### `GetMaintenanceWindowTaskRequest`

``` purescript
newtype GetMaintenanceWindowTaskRequest
  = GetMaintenanceWindowTaskRequest { "WindowId" :: MaintenanceWindowId, "WindowTaskId" :: MaintenanceWindowTaskId }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowTaskRequest _
```

#### `GetMaintenanceWindowTaskResult`

``` purescript
newtype GetMaintenanceWindowTaskResult
  = GetMaintenanceWindowTaskResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId), "Targets" :: NullOrUndefined (Targets), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "LoggingInfo" :: NullOrUndefined (LoggingInfo), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

##### Instances
``` purescript
Newtype GetMaintenanceWindowTaskResult _
```

#### `GetParameterHistoryRequest`

``` purescript
newtype GetParameterHistoryRequest
  = GetParameterHistoryRequest { "Name" :: PSParameterName, "WithDecryption" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetParameterHistoryRequest _
```

#### `GetParameterHistoryResult`

``` purescript
newtype GetParameterHistoryResult
  = GetParameterHistoryResult { "Parameters" :: NullOrUndefined (ParameterHistoryList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetParameterHistoryResult _
```

#### `GetParameterRequest`

``` purescript
newtype GetParameterRequest
  = GetParameterRequest { "Name" :: PSParameterName, "WithDecryption" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype GetParameterRequest _
```

#### `GetParameterResult`

``` purescript
newtype GetParameterResult
  = GetParameterResult { "Parameter" :: NullOrUndefined (Parameter) }
```

##### Instances
``` purescript
Newtype GetParameterResult _
```

#### `GetParametersByPathMaxResults`

``` purescript
newtype GetParametersByPathMaxResults
  = GetParametersByPathMaxResults Int
```

##### Instances
``` purescript
Newtype GetParametersByPathMaxResults _
```

#### `GetParametersByPathRequest`

``` purescript
newtype GetParametersByPathRequest
  = GetParametersByPathRequest { "Path" :: PSParameterName, "Recursive" :: NullOrUndefined (Boolean), "ParameterFilters" :: NullOrUndefined (ParameterStringFilterList), "WithDecryption" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (GetParametersByPathMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetParametersByPathRequest _
```

#### `GetParametersByPathResult`

``` purescript
newtype GetParametersByPathResult
  = GetParametersByPathResult { "Parameters" :: NullOrUndefined (ParameterList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetParametersByPathResult _
```

#### `GetParametersRequest`

``` purescript
newtype GetParametersRequest
  = GetParametersRequest { "Names" :: ParameterNameList, "WithDecryption" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype GetParametersRequest _
```

#### `GetParametersResult`

``` purescript
newtype GetParametersResult
  = GetParametersResult { "Parameters" :: NullOrUndefined (ParameterList), "InvalidParameters" :: NullOrUndefined (ParameterNameList) }
```

##### Instances
``` purescript
Newtype GetParametersResult _
```

#### `GetPatchBaselineForPatchGroupRequest`

``` purescript
newtype GetPatchBaselineForPatchGroupRequest
  = GetPatchBaselineForPatchGroupRequest { "PatchGroup" :: PatchGroup, "OperatingSystem" :: NullOrUndefined (OperatingSystem) }
```

##### Instances
``` purescript
Newtype GetPatchBaselineForPatchGroupRequest _
```

#### `GetPatchBaselineForPatchGroupResult`

``` purescript
newtype GetPatchBaselineForPatchGroupResult
  = GetPatchBaselineForPatchGroupResult { "BaselineId" :: NullOrUndefined (BaselineId), "PatchGroup" :: NullOrUndefined (PatchGroup), "OperatingSystem" :: NullOrUndefined (OperatingSystem) }
```

##### Instances
``` purescript
Newtype GetPatchBaselineForPatchGroupResult _
```

#### `GetPatchBaselineRequest`

``` purescript
newtype GetPatchBaselineRequest
  = GetPatchBaselineRequest { "BaselineId" :: BaselineId }
```

##### Instances
``` purescript
Newtype GetPatchBaselineRequest _
```

#### `GetPatchBaselineResult`

``` purescript
newtype GetPatchBaselineResult
  = GetPatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId), "Name" :: NullOrUndefined (BaselineName), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "GlobalFilters" :: NullOrUndefined (PatchFilterGroup), "ApprovalRules" :: NullOrUndefined (PatchRuleGroup), "ApprovedPatches" :: NullOrUndefined (PatchIdList), "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean), "RejectedPatches" :: NullOrUndefined (PatchIdList), "PatchGroups" :: NullOrUndefined (PatchGroupList), "CreatedDate" :: NullOrUndefined (DateTime), "ModifiedDate" :: NullOrUndefined (DateTime), "Description" :: NullOrUndefined (BaselineDescription), "Sources" :: NullOrUndefined (PatchSourceList) }
```

##### Instances
``` purescript
Newtype GetPatchBaselineResult _
```

#### `HierarchyLevelLimitExceededException`

``` purescript
newtype HierarchyLevelLimitExceededException
  = HierarchyLevelLimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>A hierarchy can have a maximum of 15 levels. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html">Working with Systems Manager Parameters</a>. </p>

##### Instances
``` purescript
Newtype HierarchyLevelLimitExceededException _
```

#### `HierarchyTypeMismatchException`

``` purescript
newtype HierarchyTypeMismatchException
  = HierarchyTypeMismatchException { "Message'" :: NullOrUndefined (String) }
```

<p>Parameter Store does not support changing a parameter type in a hierarchy. For example, you can't change a parameter from a String type to a SecureString type. You must create a new, unique parameter.</p>

##### Instances
``` purescript
Newtype HierarchyTypeMismatchException _
```

#### `IPAddress`

``` purescript
newtype IPAddress
  = IPAddress String
```

##### Instances
``` purescript
Newtype IPAddress _
```

#### `IamRole`

``` purescript
newtype IamRole
  = IamRole String
```

##### Instances
``` purescript
Newtype IamRole _
```

#### `IdempotencyToken`

``` purescript
newtype IdempotencyToken
  = IdempotencyToken String
```

##### Instances
``` purescript
Newtype IdempotencyToken _
```

#### `IdempotentParameterMismatch`

``` purescript
newtype IdempotentParameterMismatch
  = IdempotentParameterMismatch { "Message" :: NullOrUndefined (String) }
```

<p>Error returned when an idempotent operation is retried and the parameters don't match the original call to the API with the same idempotency token. </p>

##### Instances
``` purescript
Newtype IdempotentParameterMismatch _
```

#### `InstanceAggregatedAssociationOverview`

``` purescript
newtype InstanceAggregatedAssociationOverview
  = InstanceAggregatedAssociationOverview { "DetailedStatus" :: NullOrUndefined (StatusName), "InstanceAssociationStatusAggregatedCount" :: NullOrUndefined (InstanceAssociationStatusAggregatedCount) }
```

<p>Status information about the aggregated associations.</p>

##### Instances
``` purescript
Newtype InstanceAggregatedAssociationOverview _
```

#### `InstanceAssociation`

``` purescript
newtype InstanceAssociation
  = InstanceAssociation { "AssociationId" :: NullOrUndefined (AssociationId), "InstanceId" :: NullOrUndefined (InstanceId), "Content" :: NullOrUndefined (DocumentContent), "AssociationVersion" :: NullOrUndefined (AssociationVersion) }
```

<p>One or more association documents on the instance. </p>

##### Instances
``` purescript
Newtype InstanceAssociation _
```

#### `InstanceAssociationExecutionSummary`

``` purescript
newtype InstanceAssociationExecutionSummary
  = InstanceAssociationExecutionSummary String
```

##### Instances
``` purescript
Newtype InstanceAssociationExecutionSummary _
```

#### `InstanceAssociationList`

``` purescript
newtype InstanceAssociationList
  = InstanceAssociationList (Array InstanceAssociation)
```

##### Instances
``` purescript
Newtype InstanceAssociationList _
```

#### `InstanceAssociationOutputLocation`

``` purescript
newtype InstanceAssociationOutputLocation
  = InstanceAssociationOutputLocation { "S3Location" :: NullOrUndefined (S3OutputLocation) }
```

<p>An Amazon S3 bucket where you want to store the results of this request.</p>

##### Instances
``` purescript
Newtype InstanceAssociationOutputLocation _
```

#### `InstanceAssociationOutputUrl`

``` purescript
newtype InstanceAssociationOutputUrl
  = InstanceAssociationOutputUrl { "S3OutputUrl" :: NullOrUndefined (S3OutputUrl) }
```

<p>The URL of Amazon S3 bucket where you want to store the results of this request.</p>

##### Instances
``` purescript
Newtype InstanceAssociationOutputUrl _
```

#### `InstanceAssociationStatusAggregatedCount`

``` purescript
newtype InstanceAssociationStatusAggregatedCount
  = InstanceAssociationStatusAggregatedCount (Map StatusName InstanceCount)
```

##### Instances
``` purescript
Newtype InstanceAssociationStatusAggregatedCount _
```

#### `InstanceAssociationStatusInfo`

``` purescript
newtype InstanceAssociationStatusInfo
  = InstanceAssociationStatusInfo { "AssociationId" :: NullOrUndefined (AssociationId), "Name" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "AssociationVersion" :: NullOrUndefined (AssociationVersion), "InstanceId" :: NullOrUndefined (InstanceId), "ExecutionDate" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (StatusName), "DetailedStatus" :: NullOrUndefined (StatusName), "ExecutionSummary" :: NullOrUndefined (InstanceAssociationExecutionSummary), "ErrorCode" :: NullOrUndefined (AgentErrorCode), "OutputUrl" :: NullOrUndefined (InstanceAssociationOutputUrl), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Status information about the instance association.</p>

##### Instances
``` purescript
Newtype InstanceAssociationStatusInfo _
```

#### `InstanceAssociationStatusInfos`

``` purescript
newtype InstanceAssociationStatusInfos
  = InstanceAssociationStatusInfos (Array InstanceAssociationStatusInfo)
```

##### Instances
``` purescript
Newtype InstanceAssociationStatusInfos _
```

#### `InstanceCount`

``` purescript
newtype InstanceCount
  = InstanceCount Int
```

##### Instances
``` purescript
Newtype InstanceCount _
```

#### `InstanceId`

``` purescript
newtype InstanceId
  = InstanceId String
```

##### Instances
``` purescript
Newtype InstanceId _
```

#### `InstanceIdList`

``` purescript
newtype InstanceIdList
  = InstanceIdList (Array InstanceId)
```

##### Instances
``` purescript
Newtype InstanceIdList _
```

#### `InstanceInformation`

``` purescript
newtype InstanceInformation
  = InstanceInformation { "InstanceId" :: NullOrUndefined (InstanceId), "PingStatus" :: NullOrUndefined (PingStatus), "LastPingDateTime" :: NullOrUndefined (DateTime), "AgentVersion" :: NullOrUndefined (Version), "IsLatestVersion" :: NullOrUndefined (Boolean), "PlatformType" :: NullOrUndefined (PlatformType), "PlatformName" :: NullOrUndefined (String), "PlatformVersion" :: NullOrUndefined (String), "ActivationId" :: NullOrUndefined (ActivationId), "IamRole" :: NullOrUndefined (IamRole), "RegistrationDate" :: NullOrUndefined (DateTime), "ResourceType" :: NullOrUndefined (ResourceType), "Name" :: NullOrUndefined (String), "IPAddress" :: NullOrUndefined (IPAddress), "ComputerName" :: NullOrUndefined (ComputerName), "AssociationStatus" :: NullOrUndefined (StatusName), "LastAssociationExecutionDate" :: NullOrUndefined (DateTime), "LastSuccessfulAssociationExecutionDate" :: NullOrUndefined (DateTime), "AssociationOverview" :: NullOrUndefined (InstanceAggregatedAssociationOverview) }
```

<p>Describes a filter for a specific list of instances. </p>

##### Instances
``` purescript
Newtype InstanceInformation _
```

#### `InstanceInformationFilter`

``` purescript
newtype InstanceInformationFilter
  = InstanceInformationFilter { "Key'" :: InstanceInformationFilterKey, "ValueSet'" :: InstanceInformationFilterValueSet }
```

<p>Describes a filter for a specific list of instances. </p>

##### Instances
``` purescript
Newtype InstanceInformationFilter _
```

#### `InstanceInformationFilterKey`

``` purescript
newtype InstanceInformationFilterKey
  = InstanceInformationFilterKey String
```

##### Instances
``` purescript
Newtype InstanceInformationFilterKey _
```

#### `InstanceInformationFilterList`

``` purescript
newtype InstanceInformationFilterList
  = InstanceInformationFilterList (Array InstanceInformationFilter)
```

##### Instances
``` purescript
Newtype InstanceInformationFilterList _
```

#### `InstanceInformationFilterValue`

``` purescript
newtype InstanceInformationFilterValue
  = InstanceInformationFilterValue String
```

##### Instances
``` purescript
Newtype InstanceInformationFilterValue _
```

#### `InstanceInformationFilterValueSet`

``` purescript
newtype InstanceInformationFilterValueSet
  = InstanceInformationFilterValueSet (Array InstanceInformationFilterValue)
```

##### Instances
``` purescript
Newtype InstanceInformationFilterValueSet _
```

#### `InstanceInformationList`

``` purescript
newtype InstanceInformationList
  = InstanceInformationList (Array InstanceInformation)
```

##### Instances
``` purescript
Newtype InstanceInformationList _
```

#### `InstanceInformationStringFilter`

``` purescript
newtype InstanceInformationStringFilter
  = InstanceInformationStringFilter { "Key" :: InstanceInformationStringFilterKey, "Values" :: InstanceInformationFilterValueSet }
```

<p>The filters to describe or get information about your managed instances.</p>

##### Instances
``` purescript
Newtype InstanceInformationStringFilter _
```

#### `InstanceInformationStringFilterKey`

``` purescript
newtype InstanceInformationStringFilterKey
  = InstanceInformationStringFilterKey String
```

##### Instances
``` purescript
Newtype InstanceInformationStringFilterKey _
```

#### `InstanceInformationStringFilterList`

``` purescript
newtype InstanceInformationStringFilterList
  = InstanceInformationStringFilterList (Array InstanceInformationStringFilter)
```

##### Instances
``` purescript
Newtype InstanceInformationStringFilterList _
```

#### `InstancePatchState`

``` purescript
newtype InstancePatchState
  = InstancePatchState { "InstanceId" :: InstanceId, "PatchGroup" :: PatchGroup, "BaselineId" :: BaselineId, "SnapshotId" :: NullOrUndefined (SnapshotId), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "InstalledCount" :: NullOrUndefined (PatchInstalledCount), "InstalledOtherCount" :: NullOrUndefined (PatchInstalledOtherCount), "MissingCount" :: NullOrUndefined (PatchMissingCount), "FailedCount" :: NullOrUndefined (PatchFailedCount), "NotApplicableCount" :: NullOrUndefined (PatchNotApplicableCount), "OperationStartTime" :: DateTime, "OperationEndTime" :: DateTime, "Operation" :: PatchOperationType }
```

<p>Defines the high-level patch compliance state for a managed instance, providing information about the number of installed, missing, not applicable, and failed patches along with metadata about the operation when this information was gathered for the instance.</p>

##### Instances
``` purescript
Newtype InstancePatchState _
```

#### `InstancePatchStateFilter`

``` purescript
newtype InstancePatchStateFilter
  = InstancePatchStateFilter { "Key" :: InstancePatchStateFilterKey, "Values" :: InstancePatchStateFilterValues, "Type" :: InstancePatchStateOperatorType }
```

<p>Defines a filter used in DescribeInstancePatchStatesForPatchGroup used to scope down the information returned by the API.</p>

##### Instances
``` purescript
Newtype InstancePatchStateFilter _
```

#### `InstancePatchStateFilterKey`

``` purescript
newtype InstancePatchStateFilterKey
  = InstancePatchStateFilterKey String
```

##### Instances
``` purescript
Newtype InstancePatchStateFilterKey _
```

#### `InstancePatchStateFilterList`

``` purescript
newtype InstancePatchStateFilterList
  = InstancePatchStateFilterList (Array InstancePatchStateFilter)
```

##### Instances
``` purescript
Newtype InstancePatchStateFilterList _
```

#### `InstancePatchStateFilterValue`

``` purescript
newtype InstancePatchStateFilterValue
  = InstancePatchStateFilterValue String
```

##### Instances
``` purescript
Newtype InstancePatchStateFilterValue _
```

#### `InstancePatchStateFilterValues`

``` purescript
newtype InstancePatchStateFilterValues
  = InstancePatchStateFilterValues (Array InstancePatchStateFilterValue)
```

##### Instances
``` purescript
Newtype InstancePatchStateFilterValues _
```

#### `InstancePatchStateList`

``` purescript
newtype InstancePatchStateList
  = InstancePatchStateList (Array InstancePatchState)
```

##### Instances
``` purescript
Newtype InstancePatchStateList _
```

#### `InstancePatchStateOperatorType`

``` purescript
newtype InstancePatchStateOperatorType
  = InstancePatchStateOperatorType String
```

##### Instances
``` purescript
Newtype InstancePatchStateOperatorType _
```

#### `InstancePatchStatesList`

``` purescript
newtype InstancePatchStatesList
  = InstancePatchStatesList (Array InstancePatchState)
```

##### Instances
``` purescript
Newtype InstancePatchStatesList _
```

#### `InstanceTagName`

``` purescript
newtype InstanceTagName
  = InstanceTagName String
```

##### Instances
``` purescript
Newtype InstanceTagName _
```

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message" :: NullOrUndefined (String) }
```

<p>An error occurred on the server side.</p>

##### Instances
``` purescript
Newtype InternalServerError _
```

#### `InvalidActivation`

``` purescript
newtype InvalidActivation
  = InvalidActivation { "Message" :: NullOrUndefined (String) }
```

<p>The activation is not valid. The activation might have been deleted, or the ActivationId and the ActivationCode do not match.</p>

##### Instances
``` purescript
Newtype InvalidActivation _
```

#### `InvalidActivationId`

``` purescript
newtype InvalidActivationId
  = InvalidActivationId { "Message" :: NullOrUndefined (String) }
```

<p>The activation ID is not valid. Verify the you entered the correct ActivationId or ActivationCode and try again.</p>

##### Instances
``` purescript
Newtype InvalidActivationId _
```

#### `InvalidAllowedPatternException`

``` purescript
newtype InvalidAllowedPatternException
  = InvalidAllowedPatternException { "Message'" :: NullOrUndefined (String) }
```

<p>The request does not meet the regular expression requirement.</p>

##### Instances
``` purescript
Newtype InvalidAllowedPatternException _
```

#### `InvalidAssociationVersion`

``` purescript
newtype InvalidAssociationVersion
  = InvalidAssociationVersion { "Message" :: NullOrUndefined (String) }
```

<p>The version you specified is not valid. Use ListAssociationVersions to view all versions of an association according to the association ID. Or, use the <code>$LATEST</code> parameter to view the latest version of the association.</p>

##### Instances
``` purescript
Newtype InvalidAssociationVersion _
```

#### `InvalidAutomationExecutionParametersException`

``` purescript
newtype InvalidAutomationExecutionParametersException
  = InvalidAutomationExecutionParametersException { "Message" :: NullOrUndefined (String) }
```

<p>The supplied parameters for invoking the specified Automation document are incorrect. For example, they may not match the set of parameters permitted for the specified Automation document.</p>

##### Instances
``` purescript
Newtype InvalidAutomationExecutionParametersException _
```

#### `InvalidAutomationSignalException`

``` purescript
newtype InvalidAutomationSignalException
  = InvalidAutomationSignalException { "Message" :: NullOrUndefined (String) }
```

<p>The signal is not valid for the current Automation execution.</p>

##### Instances
``` purescript
Newtype InvalidAutomationSignalException _
```

#### `InvalidAutomationStatusUpdateException`

``` purescript
newtype InvalidAutomationStatusUpdateException
  = InvalidAutomationStatusUpdateException { "Message" :: NullOrUndefined (String) }
```

<p>The specified update status operation is not valid.</p>

##### Instances
``` purescript
Newtype InvalidAutomationStatusUpdateException _
```

#### `InvalidCommandId`

``` purescript
newtype InvalidCommandId
  = InvalidCommandId {  }
```

##### Instances
``` purescript
Newtype InvalidCommandId _
```

#### `InvalidDocument`

``` purescript
newtype InvalidDocument
  = InvalidDocument { "Message" :: NullOrUndefined (String) }
```

<p>The specified document does not exist.</p>

##### Instances
``` purescript
Newtype InvalidDocument _
```

#### `InvalidDocumentContent`

``` purescript
newtype InvalidDocumentContent
  = InvalidDocumentContent { "Message" :: NullOrUndefined (String) }
```

<p>The content for the document is not valid.</p>

##### Instances
``` purescript
Newtype InvalidDocumentContent _
```

#### `InvalidDocumentOperation`

``` purescript
newtype InvalidDocumentOperation
  = InvalidDocumentOperation { "Message" :: NullOrUndefined (String) }
```

<p>You attempted to delete a document while it is still shared. You must stop sharing the document before you can delete it.</p>

##### Instances
``` purescript
Newtype InvalidDocumentOperation _
```

#### `InvalidDocumentSchemaVersion`

``` purescript
newtype InvalidDocumentSchemaVersion
  = InvalidDocumentSchemaVersion { "Message" :: NullOrUndefined (String) }
```

<p>The version of the document schema is not supported.</p>

##### Instances
``` purescript
Newtype InvalidDocumentSchemaVersion _
```

#### `InvalidDocumentVersion`

``` purescript
newtype InvalidDocumentVersion
  = InvalidDocumentVersion { "Message" :: NullOrUndefined (String) }
```

<p>The document version is not valid or does not exist.</p>

##### Instances
``` purescript
Newtype InvalidDocumentVersion _
```

#### `InvalidFilter`

``` purescript
newtype InvalidFilter
  = InvalidFilter { "Message" :: NullOrUndefined (String) }
```

<p>The filter name is not valid. Verify the you entered the correct name and try again.</p>

##### Instances
``` purescript
Newtype InvalidFilter _
```

#### `InvalidFilterKey`

``` purescript
newtype InvalidFilterKey
  = InvalidFilterKey {  }
```

<p>The specified key is not valid.</p>

##### Instances
``` purescript
Newtype InvalidFilterKey _
```

#### `InvalidFilterOption`

``` purescript
newtype InvalidFilterOption
  = InvalidFilterOption { "Message'" :: NullOrUndefined (String) }
```

<p>The specified filter option is not valid. Valid options are Equals and BeginsWith. For Path filter, valid options are Recursive and OneLevel.</p>

##### Instances
``` purescript
Newtype InvalidFilterOption _
```

#### `InvalidFilterValue`

``` purescript
newtype InvalidFilterValue
  = InvalidFilterValue { "Message" :: NullOrUndefined (String) }
```

<p>The filter value is not valid. Verify the value and try again.</p>

##### Instances
``` purescript
Newtype InvalidFilterValue _
```

#### `InvalidInstanceId`

``` purescript
newtype InvalidInstanceId
  = InvalidInstanceId { "Message" :: NullOrUndefined (String) }
```

<p>The following problems can cause this exception:</p> <p>You do not have permission to access the instance.</p> <p>The SSM Agent is not running. On managed instances and Linux instances, verify that the SSM Agent is running. On EC2 Windows instances, verify that the EC2Config service is running.</p> <p>The SSM Agent or EC2Config service is not registered to the SSM endpoint. Try reinstalling the SSM Agent or EC2Config service.</p> <p>The instance is not in valid state. Valid states are: Running, Pending, Stopped, Stopping. Invalid states are: Shutting-down and Terminated.</p>

##### Instances
``` purescript
Newtype InvalidInstanceId _
```

#### `InvalidInstanceInformationFilterValue`

``` purescript
newtype InvalidInstanceInformationFilterValue
  = InvalidInstanceInformationFilterValue { "Message'" :: NullOrUndefined (String) }
```

<p>The specified filter value is not valid.</p>

##### Instances
``` purescript
Newtype InvalidInstanceInformationFilterValue _
```

#### `InvalidInventoryItemContextException`

``` purescript
newtype InvalidInventoryItemContextException
  = InvalidInventoryItemContextException { "Message" :: NullOrUndefined (String) }
```

<p>You specified invalid keys or values in the <code>Context</code> attribute for <code>InventoryItem</code>. Verify the keys and values, and try again.</p>

##### Instances
``` purescript
Newtype InvalidInventoryItemContextException _
```

#### `InvalidItemContentException`

``` purescript
newtype InvalidItemContentException
  = InvalidItemContentException { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "Message" :: NullOrUndefined (String) }
```

<p>One or more content items is not valid.</p>

##### Instances
``` purescript
Newtype InvalidItemContentException _
```

#### `InvalidKeyId`

``` purescript
newtype InvalidKeyId
  = InvalidKeyId { "Message'" :: NullOrUndefined (String) }
```

<p>The query key ID is not valid.</p>

##### Instances
``` purescript
Newtype InvalidKeyId _
```

#### `InvalidNextToken`

``` purescript
newtype InvalidNextToken
  = InvalidNextToken { "Message" :: NullOrUndefined (String) }
```

<p>The specified token is not valid.</p>

##### Instances
``` purescript
Newtype InvalidNextToken _
```

#### `InvalidNotificationConfig`

``` purescript
newtype InvalidNotificationConfig
  = InvalidNotificationConfig { "Message" :: NullOrUndefined (String) }
```

<p>One or more configuration items is not valid. Verify that a valid Amazon Resource Name (ARN) was provided for an Amazon SNS topic.</p>

##### Instances
``` purescript
Newtype InvalidNotificationConfig _
```

#### `InvalidOutputFolder`

``` purescript
newtype InvalidOutputFolder
  = InvalidOutputFolder {  }
```

<p>The S3 bucket does not exist.</p>

##### Instances
``` purescript
Newtype InvalidOutputFolder _
```

#### `InvalidOutputLocation`

``` purescript
newtype InvalidOutputLocation
  = InvalidOutputLocation {  }
```

<p>The output location is not valid or does not exist.</p>

##### Instances
``` purescript
Newtype InvalidOutputLocation _
```

#### `InvalidParameters`

``` purescript
newtype InvalidParameters
  = InvalidParameters { "Message" :: NullOrUndefined (String) }
```

<p>You must specify values for all required parameters in the Systems Manager document. You can only supply values to parameters defined in the Systems Manager document.</p>

##### Instances
``` purescript
Newtype InvalidParameters _
```

#### `InvalidPermissionType`

``` purescript
newtype InvalidPermissionType
  = InvalidPermissionType { "Message" :: NullOrUndefined (String) }
```

<p>The permission type is not supported. <i>Share</i> is the only supported permission type.</p>

##### Instances
``` purescript
Newtype InvalidPermissionType _
```

#### `InvalidPluginName`

``` purescript
newtype InvalidPluginName
  = InvalidPluginName {  }
```

<p>The plugin name is not valid.</p>

##### Instances
``` purescript
Newtype InvalidPluginName _
```

#### `InvalidResourceId`

``` purescript
newtype InvalidResourceId
  = InvalidResourceId {  }
```

<p>The resource ID is not valid. Verify that you entered the correct ID and try again.</p>

##### Instances
``` purescript
Newtype InvalidResourceId _
```

#### `InvalidResourceType`

``` purescript
newtype InvalidResourceType
  = InvalidResourceType {  }
```

<p>The resource type is not valid. For example, if you are attempting to tag an instance, the instance must be a registered, managed instance.</p>

##### Instances
``` purescript
Newtype InvalidResourceType _
```

#### `InvalidResultAttributeException`

``` purescript
newtype InvalidResultAttributeException
  = InvalidResultAttributeException { "Message" :: NullOrUndefined (String) }
```

<p>The specified inventory item result attribute is not valid.</p>

##### Instances
``` purescript
Newtype InvalidResultAttributeException _
```

#### `InvalidRole`

``` purescript
newtype InvalidRole
  = InvalidRole { "Message" :: NullOrUndefined (String) }
```

<p>The role name can't contain invalid characters. Also verify that you specified an IAM role for notifications that includes the required trust policy. For information about configuring the IAM role for Run Command notifications, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/rc-sns-notifications.html">Configuring Amazon SNS Notifications for Run Command</a> in the <i>AWS Systems Manager User Guide</i>.</p>

##### Instances
``` purescript
Newtype InvalidRole _
```

#### `InvalidSchedule`

``` purescript
newtype InvalidSchedule
  = InvalidSchedule { "Message" :: NullOrUndefined (String) }
```

<p>The schedule is invalid. Verify your cron or rate expression and try again.</p>

##### Instances
``` purescript
Newtype InvalidSchedule _
```

#### `InvalidTarget`

``` purescript
newtype InvalidTarget
  = InvalidTarget { "Message" :: NullOrUndefined (String) }
```

<p>The target is not valid or does not exist. It might not be configured for EC2 Systems Manager or you might not have permission to perform the operation.</p>

##### Instances
``` purescript
Newtype InvalidTarget _
```

#### `InvalidTypeNameException`

``` purescript
newtype InvalidTypeNameException
  = InvalidTypeNameException { "Message" :: NullOrUndefined (String) }
```

<p>The parameter type name is not valid.</p>

##### Instances
``` purescript
Newtype InvalidTypeNameException _
```

#### `InvalidUpdate`

``` purescript
newtype InvalidUpdate
  = InvalidUpdate { "Message" :: NullOrUndefined (String) }
```

<p>The update is not valid.</p>

##### Instances
``` purescript
Newtype InvalidUpdate _
```

#### `InventoryAggregator`

``` purescript
newtype InventoryAggregator
  = InventoryAggregator { "Expression" :: NullOrUndefined (InventoryAggregatorExpression), "Aggregators" :: NullOrUndefined (InventoryAggregatorList) }
```

<p>Specifies the inventory type and attribute for the aggregation execution.</p>

##### Instances
``` purescript
Newtype InventoryAggregator _
```

#### `InventoryAggregatorExpression`

``` purescript
newtype InventoryAggregatorExpression
  = InventoryAggregatorExpression String
```

##### Instances
``` purescript
Newtype InventoryAggregatorExpression _
```

#### `InventoryAggregatorList`

``` purescript
newtype InventoryAggregatorList
  = InventoryAggregatorList (Array InventoryAggregator)
```

##### Instances
``` purescript
Newtype InventoryAggregatorList _
```

#### `InventoryAttributeDataType`

``` purescript
newtype InventoryAttributeDataType
  = InventoryAttributeDataType String
```

##### Instances
``` purescript
Newtype InventoryAttributeDataType _
```

#### `InventoryFilter`

``` purescript
newtype InventoryFilter
  = InventoryFilter { "Key" :: InventoryFilterKey, "Values" :: InventoryFilterValueList, "Type" :: NullOrUndefined (InventoryQueryOperatorType) }
```

<p>One or more filters. Use a filter to return a more specific list of results.</p>

##### Instances
``` purescript
Newtype InventoryFilter _
```

#### `InventoryFilterKey`

``` purescript
newtype InventoryFilterKey
  = InventoryFilterKey String
```

##### Instances
``` purescript
Newtype InventoryFilterKey _
```

#### `InventoryFilterList`

``` purescript
newtype InventoryFilterList
  = InventoryFilterList (Array InventoryFilter)
```

##### Instances
``` purescript
Newtype InventoryFilterList _
```

#### `InventoryFilterValue`

``` purescript
newtype InventoryFilterValue
  = InventoryFilterValue String
```

##### Instances
``` purescript
Newtype InventoryFilterValue _
```

#### `InventoryFilterValueList`

``` purescript
newtype InventoryFilterValueList
  = InventoryFilterValueList (Array InventoryFilterValue)
```

##### Instances
``` purescript
Newtype InventoryFilterValueList _
```

#### `InventoryItem`

``` purescript
newtype InventoryItem
  = InventoryItem { "TypeName" :: InventoryItemTypeName, "SchemaVersion" :: InventoryItemSchemaVersion, "CaptureTime" :: InventoryItemCaptureTime, "ContentHash" :: NullOrUndefined (InventoryItemContentHash), "Content" :: NullOrUndefined (InventoryItemEntryList), "Context" :: NullOrUndefined (InventoryItemContentContext) }
```

<p>Information collected from managed instances based on your inventory policy document</p>

##### Instances
``` purescript
Newtype InventoryItem _
```

#### `InventoryItemAttribute`

``` purescript
newtype InventoryItemAttribute
  = InventoryItemAttribute { "Name" :: InventoryItemAttributeName, "DataType" :: InventoryAttributeDataType }
```

<p>Attributes are the entries within the inventory item content. It contains name and value.</p>

##### Instances
``` purescript
Newtype InventoryItemAttribute _
```

#### `InventoryItemAttributeList`

``` purescript
newtype InventoryItemAttributeList
  = InventoryItemAttributeList (Array InventoryItemAttribute)
```

##### Instances
``` purescript
Newtype InventoryItemAttributeList _
```

#### `InventoryItemAttributeName`

``` purescript
newtype InventoryItemAttributeName
  = InventoryItemAttributeName String
```

##### Instances
``` purescript
Newtype InventoryItemAttributeName _
```

#### `InventoryItemCaptureTime`

``` purescript
newtype InventoryItemCaptureTime
  = InventoryItemCaptureTime String
```

##### Instances
``` purescript
Newtype InventoryItemCaptureTime _
```

#### `InventoryItemContentContext`

``` purescript
newtype InventoryItemContentContext
  = InventoryItemContentContext (Map AttributeName AttributeValue)
```

##### Instances
``` purescript
Newtype InventoryItemContentContext _
```

#### `InventoryItemContentHash`

``` purescript
newtype InventoryItemContentHash
  = InventoryItemContentHash String
```

##### Instances
``` purescript
Newtype InventoryItemContentHash _
```

#### `InventoryItemEntry`

``` purescript
newtype InventoryItemEntry
  = InventoryItemEntry (Map AttributeName AttributeValue)
```

##### Instances
``` purescript
Newtype InventoryItemEntry _
```

#### `InventoryItemEntryList`

``` purescript
newtype InventoryItemEntryList
  = InventoryItemEntryList (Array InventoryItemEntry)
```

##### Instances
``` purescript
Newtype InventoryItemEntryList _
```

#### `InventoryItemList`

``` purescript
newtype InventoryItemList
  = InventoryItemList (Array InventoryItem)
```

##### Instances
``` purescript
Newtype InventoryItemList _
```

#### `InventoryItemSchema`

``` purescript
newtype InventoryItemSchema
  = InventoryItemSchema { "TypeName" :: InventoryItemTypeName, "Version" :: NullOrUndefined (InventoryItemSchemaVersion), "Attributes" :: InventoryItemAttributeList, "DisplayName" :: NullOrUndefined (InventoryTypeDisplayName) }
```

<p>The inventory item schema definition. Users can use this to compose inventory query filters.</p>

##### Instances
``` purescript
Newtype InventoryItemSchema _
```

#### `InventoryItemSchemaResultList`

``` purescript
newtype InventoryItemSchemaResultList
  = InventoryItemSchemaResultList (Array InventoryItemSchema)
```

##### Instances
``` purescript
Newtype InventoryItemSchemaResultList _
```

#### `InventoryItemSchemaVersion`

``` purescript
newtype InventoryItemSchemaVersion
  = InventoryItemSchemaVersion String
```

##### Instances
``` purescript
Newtype InventoryItemSchemaVersion _
```

#### `InventoryItemTypeName`

``` purescript
newtype InventoryItemTypeName
  = InventoryItemTypeName String
```

##### Instances
``` purescript
Newtype InventoryItemTypeName _
```

#### `InventoryItemTypeNameFilter`

``` purescript
newtype InventoryItemTypeNameFilter
  = InventoryItemTypeNameFilter String
```

##### Instances
``` purescript
Newtype InventoryItemTypeNameFilter _
```

#### `InventoryQueryOperatorType`

``` purescript
newtype InventoryQueryOperatorType
  = InventoryQueryOperatorType String
```

##### Instances
``` purescript
Newtype InventoryQueryOperatorType _
```

#### `InventoryResultEntity`

``` purescript
newtype InventoryResultEntity
  = InventoryResultEntity { "Id" :: NullOrUndefined (InventoryResultEntityId), "Data" :: NullOrUndefined (InventoryResultItemMap) }
```

<p>Inventory query results.</p>

##### Instances
``` purescript
Newtype InventoryResultEntity _
```

#### `InventoryResultEntityId`

``` purescript
newtype InventoryResultEntityId
  = InventoryResultEntityId String
```

##### Instances
``` purescript
Newtype InventoryResultEntityId _
```

#### `InventoryResultEntityList`

``` purescript
newtype InventoryResultEntityList
  = InventoryResultEntityList (Array InventoryResultEntity)
```

##### Instances
``` purescript
Newtype InventoryResultEntityList _
```

#### `InventoryResultItem`

``` purescript
newtype InventoryResultItem
  = InventoryResultItem { "TypeName" :: InventoryItemTypeName, "SchemaVersion" :: InventoryItemSchemaVersion, "CaptureTime" :: NullOrUndefined (InventoryItemCaptureTime), "ContentHash" :: NullOrUndefined (InventoryItemContentHash), "Content" :: InventoryItemEntryList }
```

<p>The inventory result item.</p>

##### Instances
``` purescript
Newtype InventoryResultItem _
```

#### `InventoryResultItemKey`

``` purescript
newtype InventoryResultItemKey
  = InventoryResultItemKey String
```

##### Instances
``` purescript
Newtype InventoryResultItemKey _
```

#### `InventoryResultItemMap`

``` purescript
newtype InventoryResultItemMap
  = InventoryResultItemMap (Map InventoryResultItemKey InventoryResultItem)
```

##### Instances
``` purescript
Newtype InventoryResultItemMap _
```

#### `InventoryTypeDisplayName`

``` purescript
newtype InventoryTypeDisplayName
  = InventoryTypeDisplayName String
```

##### Instances
``` purescript
Newtype InventoryTypeDisplayName _
```

#### `InvocationDoesNotExist`

``` purescript
newtype InvocationDoesNotExist
  = InvocationDoesNotExist {  }
```

<p>The command ID and instance ID you specified did not match any invocations. Verify the command ID adn the instance ID and try again. </p>

##### Instances
``` purescript
Newtype InvocationDoesNotExist _
```

#### `InvocationTraceOutput`

``` purescript
newtype InvocationTraceOutput
  = InvocationTraceOutput String
```

##### Instances
``` purescript
Newtype InvocationTraceOutput _
```

#### `IsSubTypeSchema`

``` purescript
newtype IsSubTypeSchema
  = IsSubTypeSchema Boolean
```

##### Instances
``` purescript
Newtype IsSubTypeSchema _
```

#### `ItemContentMismatchException`

``` purescript
newtype ItemContentMismatchException
  = ItemContentMismatchException { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "Message" :: NullOrUndefined (String) }
```

<p>The inventory item has invalid content. </p>

##### Instances
``` purescript
Newtype ItemContentMismatchException _
```

#### `ItemSizeLimitExceededException`

``` purescript
newtype ItemSizeLimitExceededException
  = ItemSizeLimitExceededException { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "Message" :: NullOrUndefined (String) }
```

<p>The inventory item size has exceeded the size limit.</p>

##### Instances
``` purescript
Newtype ItemSizeLimitExceededException _
```

#### `KeyList`

``` purescript
newtype KeyList
  = KeyList (Array TagKey)
```

##### Instances
``` purescript
Newtype KeyList _
```

#### `LastResourceDataSyncStatus`

``` purescript
newtype LastResourceDataSyncStatus
  = LastResourceDataSyncStatus String
```

##### Instances
``` purescript
Newtype LastResourceDataSyncStatus _
```

#### `LastResourceDataSyncTime`

``` purescript
newtype LastResourceDataSyncTime
  = LastResourceDataSyncTime Number
```

##### Instances
``` purescript
Newtype LastResourceDataSyncTime _
```

#### `LastSuccessfulResourceDataSyncTime`

``` purescript
newtype LastSuccessfulResourceDataSyncTime
  = LastSuccessfulResourceDataSyncTime Number
```

##### Instances
``` purescript
Newtype LastSuccessfulResourceDataSyncTime _
```

#### `ListAssociationVersionsRequest`

``` purescript
newtype ListAssociationVersionsRequest
  = ListAssociationVersionsRequest { "AssociationId" :: AssociationId, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAssociationVersionsRequest _
```

#### `ListAssociationVersionsResult`

``` purescript
newtype ListAssociationVersionsResult
  = ListAssociationVersionsResult { "AssociationVersions" :: NullOrUndefined (AssociationVersionList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAssociationVersionsResult _
```

#### `ListAssociationsRequest`

``` purescript
newtype ListAssociationsRequest
  = ListAssociationsRequest { "AssociationFilterList" :: NullOrUndefined (AssociationFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAssociationsRequest _
```

#### `ListAssociationsResult`

``` purescript
newtype ListAssociationsResult
  = ListAssociationsResult { "Associations" :: NullOrUndefined (AssociationList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListAssociationsResult _
```

#### `ListCommandInvocationsRequest`

``` purescript
newtype ListCommandInvocationsRequest
  = ListCommandInvocationsRequest { "CommandId" :: NullOrUndefined (CommandId), "InstanceId" :: NullOrUndefined (InstanceId), "MaxResults" :: NullOrUndefined (CommandMaxResults), "NextToken" :: NullOrUndefined (NextToken), "Filters" :: NullOrUndefined (CommandFilterList), "Details" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype ListCommandInvocationsRequest _
```

#### `ListCommandInvocationsResult`

``` purescript
newtype ListCommandInvocationsResult
  = ListCommandInvocationsResult { "CommandInvocations" :: NullOrUndefined (CommandInvocationList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListCommandInvocationsResult _
```

#### `ListCommandsRequest`

``` purescript
newtype ListCommandsRequest
  = ListCommandsRequest { "CommandId" :: NullOrUndefined (CommandId), "InstanceId" :: NullOrUndefined (InstanceId), "MaxResults" :: NullOrUndefined (CommandMaxResults), "NextToken" :: NullOrUndefined (NextToken), "Filters" :: NullOrUndefined (CommandFilterList) }
```

##### Instances
``` purescript
Newtype ListCommandsRequest _
```

#### `ListCommandsResult`

``` purescript
newtype ListCommandsResult
  = ListCommandsResult { "Commands" :: NullOrUndefined (CommandList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListCommandsResult _
```

#### `ListComplianceItemsRequest`

``` purescript
newtype ListComplianceItemsRequest
  = ListComplianceItemsRequest { "Filters" :: NullOrUndefined (ComplianceStringFilterList), "ResourceIds" :: NullOrUndefined (ComplianceResourceIdList), "ResourceTypes" :: NullOrUndefined (ComplianceResourceTypeList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListComplianceItemsRequest _
```

#### `ListComplianceItemsResult`

``` purescript
newtype ListComplianceItemsResult
  = ListComplianceItemsResult { "ComplianceItems" :: NullOrUndefined (ComplianceItemList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListComplianceItemsResult _
```

#### `ListComplianceSummariesRequest`

``` purescript
newtype ListComplianceSummariesRequest
  = ListComplianceSummariesRequest { "Filters" :: NullOrUndefined (ComplianceStringFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListComplianceSummariesRequest _
```

#### `ListComplianceSummariesResult`

``` purescript
newtype ListComplianceSummariesResult
  = ListComplianceSummariesResult { "ComplianceSummaryItems" :: NullOrUndefined (ComplianceSummaryItemList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListComplianceSummariesResult _
```

#### `ListDocumentVersionsRequest`

``` purescript
newtype ListDocumentVersionsRequest
  = ListDocumentVersionsRequest { "Name" :: DocumentName, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListDocumentVersionsRequest _
```

#### `ListDocumentVersionsResult`

``` purescript
newtype ListDocumentVersionsResult
  = ListDocumentVersionsResult { "DocumentVersions" :: NullOrUndefined (DocumentVersionList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListDocumentVersionsResult _
```

#### `ListDocumentsRequest`

``` purescript
newtype ListDocumentsRequest
  = ListDocumentsRequest { "DocumentFilterList" :: NullOrUndefined (DocumentFilterList), "Filters" :: NullOrUndefined (DocumentKeyValuesFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListDocumentsRequest _
```

#### `ListDocumentsResult`

``` purescript
newtype ListDocumentsResult
  = ListDocumentsResult { "DocumentIdentifiers" :: NullOrUndefined (DocumentIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListDocumentsResult _
```

#### `ListInventoryEntriesRequest`

``` purescript
newtype ListInventoryEntriesRequest
  = ListInventoryEntriesRequest { "InstanceId" :: InstanceId, "TypeName" :: InventoryItemTypeName, "Filters" :: NullOrUndefined (InventoryFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListInventoryEntriesRequest _
```

#### `ListInventoryEntriesResult`

``` purescript
newtype ListInventoryEntriesResult
  = ListInventoryEntriesResult { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "InstanceId" :: NullOrUndefined (InstanceId), "SchemaVersion" :: NullOrUndefined (InventoryItemSchemaVersion), "CaptureTime" :: NullOrUndefined (InventoryItemCaptureTime), "Entries" :: NullOrUndefined (InventoryItemEntryList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListInventoryEntriesResult _
```

#### `ListResourceComplianceSummariesRequest`

``` purescript
newtype ListResourceComplianceSummariesRequest
  = ListResourceComplianceSummariesRequest { "Filters" :: NullOrUndefined (ComplianceStringFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListResourceComplianceSummariesRequest _
```

#### `ListResourceComplianceSummariesResult`

``` purescript
newtype ListResourceComplianceSummariesResult
  = ListResourceComplianceSummariesResult { "ResourceComplianceSummaryItems" :: NullOrUndefined (ResourceComplianceSummaryItemList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListResourceComplianceSummariesResult _
```

#### `ListResourceDataSyncRequest`

``` purescript
newtype ListResourceDataSyncRequest
  = ListResourceDataSyncRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListResourceDataSyncRequest _
```

#### `ListResourceDataSyncResult`

``` purescript
newtype ListResourceDataSyncResult
  = ListResourceDataSyncResult { "ResourceDataSyncItems" :: NullOrUndefined (ResourceDataSyncItemList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListResourceDataSyncResult _
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "ResourceType" :: ResourceTypeForTagging, "ResourceId" :: ResourceId }
```

##### Instances
``` purescript
Newtype ListTagsForResourceRequest _
```

#### `ListTagsForResourceResult`

``` purescript
newtype ListTagsForResourceResult
  = ListTagsForResourceResult { "TagList" :: NullOrUndefined (TagList) }
```

##### Instances
``` purescript
Newtype ListTagsForResourceResult _
```

#### `LoggingInfo`

``` purescript
newtype LoggingInfo
  = LoggingInfo { "S3BucketName" :: S3BucketName, "S3KeyPrefix" :: NullOrUndefined (S3KeyPrefix), "S3Region" :: S3Region }
```

<p>Information about an Amazon S3 bucket to write instance-level logs to.</p>

##### Instances
``` purescript
Newtype LoggingInfo _
```

#### `MaintenanceWindowAllowUnassociatedTargets`

``` purescript
newtype MaintenanceWindowAllowUnassociatedTargets
  = MaintenanceWindowAllowUnassociatedTargets Boolean
```

##### Instances
``` purescript
Newtype MaintenanceWindowAllowUnassociatedTargets _
```

#### `MaintenanceWindowAutomationParameters`

``` purescript
newtype MaintenanceWindowAutomationParameters
  = MaintenanceWindowAutomationParameters { "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Parameters" :: NullOrUndefined (AutomationParameterMap) }
```

<p>The parameters for an AUTOMATION task type.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowAutomationParameters _
```

#### `MaintenanceWindowCutoff`

``` purescript
newtype MaintenanceWindowCutoff
  = MaintenanceWindowCutoff Int
```

##### Instances
``` purescript
Newtype MaintenanceWindowCutoff _
```

#### `MaintenanceWindowDescription`

``` purescript
newtype MaintenanceWindowDescription
  = MaintenanceWindowDescription String
```

##### Instances
``` purescript
Newtype MaintenanceWindowDescription _
```

#### `MaintenanceWindowDurationHours`

``` purescript
newtype MaintenanceWindowDurationHours
  = MaintenanceWindowDurationHours Int
```

##### Instances
``` purescript
Newtype MaintenanceWindowDurationHours _
```

#### `MaintenanceWindowEnabled`

``` purescript
newtype MaintenanceWindowEnabled
  = MaintenanceWindowEnabled Boolean
```

##### Instances
``` purescript
Newtype MaintenanceWindowEnabled _
```

#### `MaintenanceWindowExecution`

``` purescript
newtype MaintenanceWindowExecution
  = MaintenanceWindowExecution { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime) }
```

<p>Describes the information about an execution of a Maintenance Window. </p>

##### Instances
``` purescript
Newtype MaintenanceWindowExecution _
```

#### `MaintenanceWindowExecutionId`

``` purescript
newtype MaintenanceWindowExecutionId
  = MaintenanceWindowExecutionId String
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionId _
```

#### `MaintenanceWindowExecutionList`

``` purescript
newtype MaintenanceWindowExecutionList
  = MaintenanceWindowExecutionList (Array MaintenanceWindowExecution)
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionList _
```

#### `MaintenanceWindowExecutionStatus`

``` purescript
newtype MaintenanceWindowExecutionStatus
  = MaintenanceWindowExecutionStatus String
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionStatus _
```

#### `MaintenanceWindowExecutionStatusDetails`

``` purescript
newtype MaintenanceWindowExecutionStatusDetails
  = MaintenanceWindowExecutionStatusDetails String
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionStatusDetails _
```

#### `MaintenanceWindowExecutionTaskExecutionId`

``` purescript
newtype MaintenanceWindowExecutionTaskExecutionId
  = MaintenanceWindowExecutionTaskExecutionId String
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionTaskExecutionId _
```

#### `MaintenanceWindowExecutionTaskId`

``` purescript
newtype MaintenanceWindowExecutionTaskId
  = MaintenanceWindowExecutionTaskId String
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionTaskId _
```

#### `MaintenanceWindowExecutionTaskIdList`

``` purescript
newtype MaintenanceWindowExecutionTaskIdList
  = MaintenanceWindowExecutionTaskIdList (Array MaintenanceWindowExecutionTaskId)
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionTaskIdList _
```

#### `MaintenanceWindowExecutionTaskIdentity`

``` purescript
newtype MaintenanceWindowExecutionTaskIdentity
  = MaintenanceWindowExecutionTaskIdentity { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType) }
```

<p>Information about a task execution performed as part of a Maintenance Window execution.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionTaskIdentity _
```

#### `MaintenanceWindowExecutionTaskIdentityList`

``` purescript
newtype MaintenanceWindowExecutionTaskIdentityList
  = MaintenanceWindowExecutionTaskIdentityList (Array MaintenanceWindowExecutionTaskIdentity)
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionTaskIdentityList _
```

#### `MaintenanceWindowExecutionTaskInvocationId`

``` purescript
newtype MaintenanceWindowExecutionTaskInvocationId
  = MaintenanceWindowExecutionTaskInvocationId String
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionTaskInvocationId _
```

#### `MaintenanceWindowExecutionTaskInvocationIdentity`

``` purescript
newtype MaintenanceWindowExecutionTaskInvocationIdentity
  = MaintenanceWindowExecutionTaskInvocationIdentity { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId), "InvocationId" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationId), "ExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskExecutionId), "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType), "Parameters" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationParameters), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTaskTargetId) }
```

<p>Describes the information about a task invocation for a particular target as part of a task execution performed as part of a Maintenance Window execution.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionTaskInvocationIdentity _
```

#### `MaintenanceWindowExecutionTaskInvocationIdentityList`

``` purescript
newtype MaintenanceWindowExecutionTaskInvocationIdentityList
  = MaintenanceWindowExecutionTaskInvocationIdentityList (Array MaintenanceWindowExecutionTaskInvocationIdentity)
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionTaskInvocationIdentityList _
```

#### `MaintenanceWindowExecutionTaskInvocationParameters`

``` purescript
newtype MaintenanceWindowExecutionTaskInvocationParameters
  = MaintenanceWindowExecutionTaskInvocationParameters String
```

##### Instances
``` purescript
Newtype MaintenanceWindowExecutionTaskInvocationParameters _
```

#### `MaintenanceWindowFilter`

``` purescript
newtype MaintenanceWindowFilter
  = MaintenanceWindowFilter { "Key" :: NullOrUndefined (MaintenanceWindowFilterKey), "Values" :: NullOrUndefined (MaintenanceWindowFilterValues) }
```

<p>Filter used in the request.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowFilter _
```

#### `MaintenanceWindowFilterKey`

``` purescript
newtype MaintenanceWindowFilterKey
  = MaintenanceWindowFilterKey String
```

##### Instances
``` purescript
Newtype MaintenanceWindowFilterKey _
```

#### `MaintenanceWindowFilterList`

``` purescript
newtype MaintenanceWindowFilterList
  = MaintenanceWindowFilterList (Array MaintenanceWindowFilter)
```

##### Instances
``` purescript
Newtype MaintenanceWindowFilterList _
```

#### `MaintenanceWindowFilterValue`

``` purescript
newtype MaintenanceWindowFilterValue
  = MaintenanceWindowFilterValue String
```

##### Instances
``` purescript
Newtype MaintenanceWindowFilterValue _
```

#### `MaintenanceWindowFilterValues`

``` purescript
newtype MaintenanceWindowFilterValues
  = MaintenanceWindowFilterValues (Array MaintenanceWindowFilterValue)
```

##### Instances
``` purescript
Newtype MaintenanceWindowFilterValues _
```

#### `MaintenanceWindowId`

``` purescript
newtype MaintenanceWindowId
  = MaintenanceWindowId String
```

##### Instances
``` purescript
Newtype MaintenanceWindowId _
```

#### `MaintenanceWindowIdentity`

``` purescript
newtype MaintenanceWindowIdentity
  = MaintenanceWindowIdentity { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled), "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours), "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff) }
```

<p>Information about the Maintenance Window.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowIdentity _
```

#### `MaintenanceWindowIdentityList`

``` purescript
newtype MaintenanceWindowIdentityList
  = MaintenanceWindowIdentityList (Array MaintenanceWindowIdentity)
```

##### Instances
``` purescript
Newtype MaintenanceWindowIdentityList _
```

#### `MaintenanceWindowLambdaClientContext`

``` purescript
newtype MaintenanceWindowLambdaClientContext
  = MaintenanceWindowLambdaClientContext String
```

##### Instances
``` purescript
Newtype MaintenanceWindowLambdaClientContext _
```

#### `MaintenanceWindowLambdaParameters`

``` purescript
newtype MaintenanceWindowLambdaParameters
  = MaintenanceWindowLambdaParameters { "ClientContext" :: NullOrUndefined (MaintenanceWindowLambdaClientContext), "Qualifier" :: NullOrUndefined (MaintenanceWindowLambdaQualifier), "Payload" :: NullOrUndefined (MaintenanceWindowLambdaPayload) }
```

<p>The parameters for a LAMBDA task type.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowLambdaParameters _
```

#### `MaintenanceWindowLambdaPayload`

``` purescript
newtype MaintenanceWindowLambdaPayload
  = MaintenanceWindowLambdaPayload String
```

##### Instances
``` purescript
Newtype MaintenanceWindowLambdaPayload _
```

#### `MaintenanceWindowLambdaQualifier`

``` purescript
newtype MaintenanceWindowLambdaQualifier
  = MaintenanceWindowLambdaQualifier String
```

##### Instances
``` purescript
Newtype MaintenanceWindowLambdaQualifier _
```

#### `MaintenanceWindowMaxResults`

``` purescript
newtype MaintenanceWindowMaxResults
  = MaintenanceWindowMaxResults Int
```

##### Instances
``` purescript
Newtype MaintenanceWindowMaxResults _
```

#### `MaintenanceWindowName`

``` purescript
newtype MaintenanceWindowName
  = MaintenanceWindowName String
```

##### Instances
``` purescript
Newtype MaintenanceWindowName _
```

#### `MaintenanceWindowResourceType`

``` purescript
newtype MaintenanceWindowResourceType
  = MaintenanceWindowResourceType String
```

##### Instances
``` purescript
Newtype MaintenanceWindowResourceType _
```

#### `MaintenanceWindowRunCommandParameters`

``` purescript
newtype MaintenanceWindowRunCommandParameters
  = MaintenanceWindowRunCommandParameters { "Comment" :: NullOrUndefined (Comment), "DocumentHash" :: NullOrUndefined (DocumentHash), "DocumentHashType" :: NullOrUndefined (DocumentHashType), "NotificationConfig" :: NullOrUndefined (NotificationConfig), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix), "Parameters" :: NullOrUndefined (Parameters), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "TimeoutSeconds" :: NullOrUndefined (TimeoutSeconds) }
```

<p>The parameters for a RUN_COMMAND task type.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowRunCommandParameters _
```

#### `MaintenanceWindowSchedule`

``` purescript
newtype MaintenanceWindowSchedule
  = MaintenanceWindowSchedule String
```

##### Instances
``` purescript
Newtype MaintenanceWindowSchedule _
```

#### `MaintenanceWindowStepFunctionsInput`

``` purescript
newtype MaintenanceWindowStepFunctionsInput
  = MaintenanceWindowStepFunctionsInput String
```

##### Instances
``` purescript
Newtype MaintenanceWindowStepFunctionsInput _
```

#### `MaintenanceWindowStepFunctionsName`

``` purescript
newtype MaintenanceWindowStepFunctionsName
  = MaintenanceWindowStepFunctionsName String
```

##### Instances
``` purescript
Newtype MaintenanceWindowStepFunctionsName _
```

#### `MaintenanceWindowStepFunctionsParameters`

``` purescript
newtype MaintenanceWindowStepFunctionsParameters
  = MaintenanceWindowStepFunctionsParameters { "Input" :: NullOrUndefined (MaintenanceWindowStepFunctionsInput), "Name" :: NullOrUndefined (MaintenanceWindowStepFunctionsName) }
```

<p>The parameters for the STEP_FUNCTION execution.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowStepFunctionsParameters _
```

#### `MaintenanceWindowTarget`

``` purescript
newtype MaintenanceWindowTarget
  = MaintenanceWindowTarget { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId), "ResourceType" :: NullOrUndefined (MaintenanceWindowResourceType), "Targets" :: NullOrUndefined (Targets), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

<p>The target registered with the Maintenance Window.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowTarget _
```

#### `MaintenanceWindowTargetId`

``` purescript
newtype MaintenanceWindowTargetId
  = MaintenanceWindowTargetId String
```

##### Instances
``` purescript
Newtype MaintenanceWindowTargetId _
```

#### `MaintenanceWindowTargetList`

``` purescript
newtype MaintenanceWindowTargetList
  = MaintenanceWindowTargetList (Array MaintenanceWindowTarget)
```

##### Instances
``` purescript
Newtype MaintenanceWindowTargetList _
```

#### `MaintenanceWindowTask`

``` purescript
newtype MaintenanceWindowTask
  = MaintenanceWindowTask { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "Type" :: NullOrUndefined (MaintenanceWindowTaskType), "Targets" :: NullOrUndefined (Targets), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "LoggingInfo" :: NullOrUndefined (LoggingInfo), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

<p>Information about a task defined for a Maintenance Window.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowTask _
```

#### `MaintenanceWindowTaskArn`

``` purescript
newtype MaintenanceWindowTaskArn
  = MaintenanceWindowTaskArn String
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskArn _
```

#### `MaintenanceWindowTaskId`

``` purescript
newtype MaintenanceWindowTaskId
  = MaintenanceWindowTaskId String
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskId _
```

#### `MaintenanceWindowTaskInvocationParameters`

``` purescript
newtype MaintenanceWindowTaskInvocationParameters
  = MaintenanceWindowTaskInvocationParameters { "RunCommand" :: NullOrUndefined (MaintenanceWindowRunCommandParameters), "Automation" :: NullOrUndefined (MaintenanceWindowAutomationParameters), "StepFunctions" :: NullOrUndefined (MaintenanceWindowStepFunctionsParameters), "Lambda" :: NullOrUndefined (MaintenanceWindowLambdaParameters) }
```

<p>The parameters for task execution.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowTaskInvocationParameters _
```

#### `MaintenanceWindowTaskList`

``` purescript
newtype MaintenanceWindowTaskList
  = MaintenanceWindowTaskList (Array MaintenanceWindowTask)
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskList _
```

#### `MaintenanceWindowTaskParameterName`

``` purescript
newtype MaintenanceWindowTaskParameterName
  = MaintenanceWindowTaskParameterName String
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskParameterName _
```

#### `MaintenanceWindowTaskParameterValue`

``` purescript
newtype MaintenanceWindowTaskParameterValue
  = MaintenanceWindowTaskParameterValue String
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskParameterValue _
```

#### `MaintenanceWindowTaskParameterValueExpression`

``` purescript
newtype MaintenanceWindowTaskParameterValueExpression
  = MaintenanceWindowTaskParameterValueExpression { "Values" :: NullOrUndefined (MaintenanceWindowTaskParameterValueList) }
```

<p>Defines the values for a task parameter.</p>

##### Instances
``` purescript
Newtype MaintenanceWindowTaskParameterValueExpression _
```

#### `MaintenanceWindowTaskParameterValueList`

``` purescript
newtype MaintenanceWindowTaskParameterValueList
  = MaintenanceWindowTaskParameterValueList (Array MaintenanceWindowTaskParameterValue)
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskParameterValueList _
```

#### `MaintenanceWindowTaskParameters`

``` purescript
newtype MaintenanceWindowTaskParameters
  = MaintenanceWindowTaskParameters (Map MaintenanceWindowTaskParameterName MaintenanceWindowTaskParameterValueExpression)
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskParameters _
```

#### `MaintenanceWindowTaskParametersList`

``` purescript
newtype MaintenanceWindowTaskParametersList
  = MaintenanceWindowTaskParametersList (Array MaintenanceWindowTaskParameters)
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskParametersList _
```

#### `MaintenanceWindowTaskPriority`

``` purescript
newtype MaintenanceWindowTaskPriority
  = MaintenanceWindowTaskPriority Int
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskPriority _
```

#### `MaintenanceWindowTaskTargetId`

``` purescript
newtype MaintenanceWindowTaskTargetId
  = MaintenanceWindowTaskTargetId String
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskTargetId _
```

#### `MaintenanceWindowTaskType`

``` purescript
newtype MaintenanceWindowTaskType
  = MaintenanceWindowTaskType String
```

##### Instances
``` purescript
Newtype MaintenanceWindowTaskType _
```

#### `ManagedInstanceId`

``` purescript
newtype ManagedInstanceId
  = ManagedInstanceId String
```

##### Instances
``` purescript
Newtype ManagedInstanceId _
```

#### `MaxConcurrency`

``` purescript
newtype MaxConcurrency
  = MaxConcurrency String
```

##### Instances
``` purescript
Newtype MaxConcurrency _
```

#### `MaxDocumentSizeExceeded`

``` purescript
newtype MaxDocumentSizeExceeded
  = MaxDocumentSizeExceeded { "Message" :: NullOrUndefined (String) }
```

<p>The size limit of a document is 64 KB.</p>

##### Instances
``` purescript
Newtype MaxDocumentSizeExceeded _
```

#### `MaxErrors`

``` purescript
newtype MaxErrors
  = MaxErrors String
```

##### Instances
``` purescript
Newtype MaxErrors _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `MaxResultsEC2Compatible`

``` purescript
newtype MaxResultsEC2Compatible
  = MaxResultsEC2Compatible Int
```

##### Instances
``` purescript
Newtype MaxResultsEC2Compatible _
```

#### `ModifyDocumentPermissionRequest`

``` purescript
newtype ModifyDocumentPermissionRequest
  = ModifyDocumentPermissionRequest { "Name" :: DocumentName, "PermissionType" :: DocumentPermissionType, "AccountIdsToAdd" :: NullOrUndefined (AccountIdList), "AccountIdsToRemove" :: NullOrUndefined (AccountIdList) }
```

##### Instances
``` purescript
Newtype ModifyDocumentPermissionRequest _
```

#### `ModifyDocumentPermissionResponse`

``` purescript
newtype ModifyDocumentPermissionResponse
  = ModifyDocumentPermissionResponse {  }
```

##### Instances
``` purescript
Newtype ModifyDocumentPermissionResponse _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `NonCompliantSummary`

``` purescript
newtype NonCompliantSummary
  = NonCompliantSummary { "NonCompliantCount" :: NullOrUndefined (ComplianceSummaryCount), "SeveritySummary" :: NullOrUndefined (SeveritySummary) }
```

<p>A summary of resources that are not compliant. The summary is organized according to resource type.</p>

##### Instances
``` purescript
Newtype NonCompliantSummary _
```

#### `NormalStringMap`

``` purescript
newtype NormalStringMap
  = NormalStringMap (Map String String)
```

##### Instances
``` purescript
Newtype NormalStringMap _
```

#### `NotificationArn`

``` purescript
newtype NotificationArn
  = NotificationArn String
```

##### Instances
``` purescript
Newtype NotificationArn _
```

#### `NotificationConfig`

``` purescript
newtype NotificationConfig
  = NotificationConfig { "NotificationArn" :: NullOrUndefined (NotificationArn), "NotificationEvents" :: NullOrUndefined (NotificationEventList), "NotificationType" :: NullOrUndefined (NotificationType) }
```

<p>Configurations for sending notifications.</p>

##### Instances
``` purescript
Newtype NotificationConfig _
```

#### `NotificationEvent`

``` purescript
newtype NotificationEvent
  = NotificationEvent String
```

##### Instances
``` purescript
Newtype NotificationEvent _
```

#### `NotificationEventList`

``` purescript
newtype NotificationEventList
  = NotificationEventList (Array NotificationEvent)
```

##### Instances
``` purescript
Newtype NotificationEventList _
```

#### `NotificationType`

``` purescript
newtype NotificationType
  = NotificationType String
```

##### Instances
``` purescript
Newtype NotificationType _
```

#### `OperatingSystem`

``` purescript
newtype OperatingSystem
  = OperatingSystem String
```

##### Instances
``` purescript
Newtype OperatingSystem _
```

#### `OwnerInformation`

``` purescript
newtype OwnerInformation
  = OwnerInformation String
```

##### Instances
``` purescript
Newtype OwnerInformation _
```

#### `PSParameterName`

``` purescript
newtype PSParameterName
  = PSParameterName String
```

##### Instances
``` purescript
Newtype PSParameterName _
```

#### `PSParameterValue`

``` purescript
newtype PSParameterValue
  = PSParameterValue String
```

##### Instances
``` purescript
Newtype PSParameterValue _
```

#### `PSParameterVersion`

``` purescript
newtype PSParameterVersion
  = PSParameterVersion Number
```

##### Instances
``` purescript
Newtype PSParameterVersion _
```

#### `Parameter`

``` purescript
newtype Parameter
  = Parameter { "Name" :: NullOrUndefined (PSParameterName), "Type" :: NullOrUndefined (ParameterType), "Value" :: NullOrUndefined (PSParameterValue), "Version" :: NullOrUndefined (PSParameterVersion) }
```

<p>An Amazon EC2 Systems Manager parameter in Parameter Store.</p>

##### Instances
``` purescript
Newtype Parameter _
```

#### `ParameterAlreadyExists`

``` purescript
newtype ParameterAlreadyExists
  = ParameterAlreadyExists { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter already exists. You can't create duplicate parameters.</p>

##### Instances
``` purescript
Newtype ParameterAlreadyExists _
```

#### `ParameterDescription`

``` purescript
newtype ParameterDescription
  = ParameterDescription String
```

##### Instances
``` purescript
Newtype ParameterDescription _
```

#### `ParameterHistory`

``` purescript
newtype ParameterHistory
  = ParameterHistory { "Name" :: NullOrUndefined (PSParameterName), "Type" :: NullOrUndefined (ParameterType), "KeyId" :: NullOrUndefined (ParameterKeyId), "LastModifiedDate" :: NullOrUndefined (DateTime), "LastModifiedUser" :: NullOrUndefined (String), "Description" :: NullOrUndefined (ParameterDescription), "Value" :: NullOrUndefined (PSParameterValue), "AllowedPattern" :: NullOrUndefined (AllowedPattern), "Version" :: NullOrUndefined (PSParameterVersion) }
```

<p>Information about parameter usage.</p>

##### Instances
``` purescript
Newtype ParameterHistory _
```

#### `ParameterHistoryList`

``` purescript
newtype ParameterHistoryList
  = ParameterHistoryList (Array ParameterHistory)
```

##### Instances
``` purescript
Newtype ParameterHistoryList _
```

#### `ParameterKeyId`

``` purescript
newtype ParameterKeyId
  = ParameterKeyId String
```

##### Instances
``` purescript
Newtype ParameterKeyId _
```

#### `ParameterLimitExceeded`

``` purescript
newtype ParameterLimitExceeded
  = ParameterLimitExceeded { "Message'" :: NullOrUndefined (String) }
```

<p>You have exceeded the number of parameters for this AWS account. Delete one or more parameters and try again.</p>

##### Instances
``` purescript
Newtype ParameterLimitExceeded _
```

#### `ParameterList`

``` purescript
newtype ParameterList
  = ParameterList (Array Parameter)
```

##### Instances
``` purescript
Newtype ParameterList _
```

#### `ParameterMaxVersionLimitExceeded`

``` purescript
newtype ParameterMaxVersionLimitExceeded
  = ParameterMaxVersionLimitExceeded { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter exceeded the maximum number of allowed versions.</p>

##### Instances
``` purescript
Newtype ParameterMaxVersionLimitExceeded _
```

#### `ParameterMetadata`

``` purescript
newtype ParameterMetadata
  = ParameterMetadata { "Name" :: NullOrUndefined (PSParameterName), "Type" :: NullOrUndefined (ParameterType), "KeyId" :: NullOrUndefined (ParameterKeyId), "LastModifiedDate" :: NullOrUndefined (DateTime), "LastModifiedUser" :: NullOrUndefined (String), "Description" :: NullOrUndefined (ParameterDescription), "AllowedPattern" :: NullOrUndefined (AllowedPattern), "Version" :: NullOrUndefined (PSParameterVersion) }
```

<p>Metada includes information like the ARN of the last user and the date/time the parameter was last used.</p>

##### Instances
``` purescript
Newtype ParameterMetadata _
```

#### `ParameterMetadataList`

``` purescript
newtype ParameterMetadataList
  = ParameterMetadataList (Array ParameterMetadata)
```

##### Instances
``` purescript
Newtype ParameterMetadataList _
```

#### `ParameterName`

``` purescript
newtype ParameterName
  = ParameterName String
```

##### Instances
``` purescript
Newtype ParameterName _
```

#### `ParameterNameList`

``` purescript
newtype ParameterNameList
  = ParameterNameList (Array PSParameterName)
```

##### Instances
``` purescript
Newtype ParameterNameList _
```

#### `ParameterNotFound`

``` purescript
newtype ParameterNotFound
  = ParameterNotFound { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter could not be found. Verify the name and try again.</p>

##### Instances
``` purescript
Newtype ParameterNotFound _
```

#### `ParameterPatternMismatchException`

``` purescript
newtype ParameterPatternMismatchException
  = ParameterPatternMismatchException { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter name is not valid.</p>

##### Instances
``` purescript
Newtype ParameterPatternMismatchException _
```

#### `ParameterStringFilter`

``` purescript
newtype ParameterStringFilter
  = ParameterStringFilter { "Key" :: ParameterStringFilterKey, "Option" :: NullOrUndefined (ParameterStringQueryOption), "Values" :: NullOrUndefined (ParameterStringFilterValueList) }
```

<p>One or more filters. Use a filter to return a more specific list of results.</p>

##### Instances
``` purescript
Newtype ParameterStringFilter _
```

#### `ParameterStringFilterKey`

``` purescript
newtype ParameterStringFilterKey
  = ParameterStringFilterKey String
```

##### Instances
``` purescript
Newtype ParameterStringFilterKey _
```

#### `ParameterStringFilterList`

``` purescript
newtype ParameterStringFilterList
  = ParameterStringFilterList (Array ParameterStringFilter)
```

##### Instances
``` purescript
Newtype ParameterStringFilterList _
```

#### `ParameterStringFilterValue`

``` purescript
newtype ParameterStringFilterValue
  = ParameterStringFilterValue String
```

##### Instances
``` purescript
Newtype ParameterStringFilterValue _
```

#### `ParameterStringFilterValueList`

``` purescript
newtype ParameterStringFilterValueList
  = ParameterStringFilterValueList (Array ParameterStringFilterValue)
```

##### Instances
``` purescript
Newtype ParameterStringFilterValueList _
```

#### `ParameterStringQueryOption`

``` purescript
newtype ParameterStringQueryOption
  = ParameterStringQueryOption String
```

##### Instances
``` purescript
Newtype ParameterStringQueryOption _
```

#### `ParameterType`

``` purescript
newtype ParameterType
  = ParameterType String
```

##### Instances
``` purescript
Newtype ParameterType _
```

#### `ParameterValue`

``` purescript
newtype ParameterValue
  = ParameterValue String
```

##### Instances
``` purescript
Newtype ParameterValue _
```

#### `ParameterValueList`

``` purescript
newtype ParameterValueList
  = ParameterValueList (Array ParameterValue)
```

##### Instances
``` purescript
Newtype ParameterValueList _
```

#### `ParameterVersionNotFound`

``` purescript
newtype ParameterVersionNotFound
  = ParameterVersionNotFound { "Message'" :: NullOrUndefined (String) }
```

<p>The specified parameter version was not found. Verify the parameter name and version, and try again.</p>

##### Instances
``` purescript
Newtype ParameterVersionNotFound _
```

#### `Parameters`

``` purescript
newtype Parameters
  = Parameters (Map ParameterName ParameterValueList)
```

##### Instances
``` purescript
Newtype Parameters _
```

#### `ParametersFilter`

``` purescript
newtype ParametersFilter
  = ParametersFilter { "Key" :: ParametersFilterKey, "Values" :: ParametersFilterValueList }
```

<p>This data type is deprecated. Instead, use <a>ParameterStringFilter</a>.</p>

##### Instances
``` purescript
Newtype ParametersFilter _
```

#### `ParametersFilterKey`

``` purescript
newtype ParametersFilterKey
  = ParametersFilterKey String
```

##### Instances
``` purescript
Newtype ParametersFilterKey _
```

#### `ParametersFilterList`

``` purescript
newtype ParametersFilterList
  = ParametersFilterList (Array ParametersFilter)
```

##### Instances
``` purescript
Newtype ParametersFilterList _
```

#### `ParametersFilterValue`

``` purescript
newtype ParametersFilterValue
  = ParametersFilterValue String
```

##### Instances
``` purescript
Newtype ParametersFilterValue _
```

#### `ParametersFilterValueList`

``` purescript
newtype ParametersFilterValueList
  = ParametersFilterValueList (Array ParametersFilterValue)
```

##### Instances
``` purescript
Newtype ParametersFilterValueList _
```

#### `Patch`

``` purescript
newtype Patch
  = Patch { "Id" :: NullOrUndefined (PatchId), "ReleaseDate" :: NullOrUndefined (DateTime), "Title" :: NullOrUndefined (PatchTitle), "Description" :: NullOrUndefined (PatchDescription), "ContentUrl" :: NullOrUndefined (PatchContentUrl), "Vendor" :: NullOrUndefined (PatchVendor), "ProductFamily" :: NullOrUndefined (PatchProductFamily), "Product" :: NullOrUndefined (PatchProduct), "Classification" :: NullOrUndefined (PatchClassification), "MsrcSeverity" :: NullOrUndefined (PatchMsrcSeverity), "KbNumber" :: NullOrUndefined (PatchKbNumber), "MsrcNumber" :: NullOrUndefined (PatchMsrcNumber), "Language" :: NullOrUndefined (PatchLanguage) }
```

<p>Represents metadata about a patch.</p>

##### Instances
``` purescript
Newtype Patch _
```

#### `PatchBaselineIdentity`

``` purescript
newtype PatchBaselineIdentity
  = PatchBaselineIdentity { "BaselineId" :: NullOrUndefined (BaselineId), "BaselineName" :: NullOrUndefined (BaselineName), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "BaselineDescription" :: NullOrUndefined (BaselineDescription), "DefaultBaseline" :: NullOrUndefined (DefaultBaseline) }
```

<p>Defines the basic information about a patch baseline.</p>

##### Instances
``` purescript
Newtype PatchBaselineIdentity _
```

#### `PatchBaselineIdentityList`

``` purescript
newtype PatchBaselineIdentityList
  = PatchBaselineIdentityList (Array PatchBaselineIdentity)
```

##### Instances
``` purescript
Newtype PatchBaselineIdentityList _
```

#### `PatchBaselineMaxResults`

``` purescript
newtype PatchBaselineMaxResults
  = PatchBaselineMaxResults Int
```

##### Instances
``` purescript
Newtype PatchBaselineMaxResults _
```

#### `PatchClassification`

``` purescript
newtype PatchClassification
  = PatchClassification String
```

##### Instances
``` purescript
Newtype PatchClassification _
```

#### `PatchComplianceData`

``` purescript
newtype PatchComplianceData
  = PatchComplianceData { "Title" :: PatchTitle, "KBId" :: PatchKbNumber, "Classification" :: PatchClassification, "Severity" :: PatchSeverity, "State" :: PatchComplianceDataState, "InstalledTime" :: DateTime }
```

<p>Information about the state of a patch on a particular instance as it relates to the patch baseline used to patch the instance.</p>

##### Instances
``` purescript
Newtype PatchComplianceData _
```

#### `PatchComplianceDataList`

``` purescript
newtype PatchComplianceDataList
  = PatchComplianceDataList (Array PatchComplianceData)
```

##### Instances
``` purescript
Newtype PatchComplianceDataList _
```

#### `PatchComplianceDataState`

``` purescript
newtype PatchComplianceDataState
  = PatchComplianceDataState String
```

##### Instances
``` purescript
Newtype PatchComplianceDataState _
```

#### `PatchComplianceLevel`

``` purescript
newtype PatchComplianceLevel
  = PatchComplianceLevel String
```

##### Instances
``` purescript
Newtype PatchComplianceLevel _
```

#### `PatchComplianceMaxResults`

``` purescript
newtype PatchComplianceMaxResults
  = PatchComplianceMaxResults Int
```

##### Instances
``` purescript
Newtype PatchComplianceMaxResults _
```

#### `PatchContentUrl`

``` purescript
newtype PatchContentUrl
  = PatchContentUrl String
```

##### Instances
``` purescript
Newtype PatchContentUrl _
```

#### `PatchDeploymentStatus`

``` purescript
newtype PatchDeploymentStatus
  = PatchDeploymentStatus String
```

##### Instances
``` purescript
Newtype PatchDeploymentStatus _
```

#### `PatchDescription`

``` purescript
newtype PatchDescription
  = PatchDescription String
```

##### Instances
``` purescript
Newtype PatchDescription _
```

#### `PatchFailedCount`

``` purescript
newtype PatchFailedCount
  = PatchFailedCount Int
```

##### Instances
``` purescript
Newtype PatchFailedCount _
```

#### `PatchFilter`

``` purescript
newtype PatchFilter
  = PatchFilter { "Key" :: PatchFilterKey, "Values" :: PatchFilterValueList }
```

<p>Defines a patch filter.</p> <p>A patch filter consists of key/value pairs, but not all keys are valid for all operating system types. For example, the key <code>PRODUCT</code> is valid for all supported operating system types. The key <code>MSRC_SEVERITY</code>, however, is valid only for Windows operating systems, and the key <code>SECTION</code> is valid only for Ubuntu operating systems.</p> <p>Refer to the following sections for information about which keys may be used with each major operating system, and which values are valid for each key.</p> <p> <b>Windows Operating Systems</b> </p> <p>The supported keys for Windows operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>MSRC_SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Windows7</code> </p> </li> <li> <p> <code>Windows8</code> </p> </li> <li> <p> <code>Windows8.1</code> </p> </li> <li> <p> <code>Windows8Embedded</code> </p> </li> <li> <p> <code>Windows10</code> </p> </li> <li> <p> <code>Windows10LTSB</code> </p> </li> <li> <p> <code>WindowsServer2008</code> </p> </li> <li> <p> <code>WindowsServer2008R2</code> </p> </li> <li> <p> <code>WindowsServer2012</code> </p> </li> <li> <p> <code>WindowsServer2012R2</code> </p> </li> <li> <p> <code>WindowsServer2016</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>CriticalUpdates</code> </p> </li> <li> <p> <code>DefinitionUpdates</code> </p> </li> <li> <p> <code>Drivers</code> </p> </li> <li> <p> <code>FeaturePacks</code> </p> </li> <li> <p> <code>SecurityUpdates</code> </p> </li> <li> <p> <code>ServicePacks</code> </p> </li> <li> <p> <code>Tools</code> </p> </li> <li> <p> <code>UpdateRollups</code> </p> </li> <li> <p> <code>Updates</code> </p> </li> <li> <p> <code>Upgrades</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>MSRC_SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> <li> <p> <code>Unspecified</code> </p> </li> </ul> <p> <b>Ubuntu Operating Systems</b> </p> <p>The supported keys for Ubuntu operating systems are <code>PRODUCT</code>, <code>PRIORITY</code>, and <code>SECTION</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Ubuntu14.04</code> </p> </li> <li> <p> <code>Ubuntu16.04</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>PRIORITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Required</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Standard</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Extra</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SECTION</code> </p> <p>Only the length of the key value is validated. Minimum length is 1. Maximum length is 64.</p> <p> <b>Amazon Linux Operating Systems</b> </p> <p>The supported keys for Amazon Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>AmazonLinux2012.03</code> </p> </li> <li> <p> <code>AmazonLinux2012.09</code> </p> </li> <li> <p> <code>AmazonLinux2013.03</code> </p> </li> <li> <p> <code>AmazonLinux2013.09</code> </p> </li> <li> <p> <code>AmazonLinux2014.03</code> </p> </li> <li> <p> <code>AmazonLinux2014.09</code> </p> </li> <li> <p> <code>AmazonLinux2015.03</code> </p> </li> <li> <p> <code>AmazonLinux2015.09</code> </p> </li> <li> <p> <code>AmazonLinux2016.03</code> </p> </li> <li> <p> <code>AmazonLinux2016.09</code> </p> </li> <li> <p> <code>AmazonLinux2017.03</code> </p> </li> <li> <p> <code>AmazonLinux2017.09</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>RedHat Enterprise Linux (RHEL) Operating Systems</b> </p> <p>The supported keys for RedHat Enterprise Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>RedhatEnterpriseLinux6.5</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.6</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.7</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.8</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.9</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.0</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.1</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.2</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.3</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.4</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>SUSE Linux Enterprise Server (SUSE) Operating Systems</b> </p> <p>The supported keys for SUSE operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Suse12.0</code> </p> </li> <li> <p> <code>Suse12.1</code> </p> </li> <li> <p> <code>Suse12.2</code> </p> </li> <li> <p> <code>Suse12.3</code> </p> </li> <li> <p> <code>Suse12.4</code> </p> </li> <li> <p> <code>Suse12.5</code> </p> </li> <li> <p> <code>Suse12.6</code> </p> </li> <li> <p> <code>Suse12.7</code> </p> </li> <li> <p> <code>Suse12.8</code> </p> </li> <li> <p> <code>Suse12.9</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Feature</code> </p> </li> <li> <p> <code>Document</code> </p> </li> <li> <p> <code>Yast</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul>

##### Instances
``` purescript
Newtype PatchFilter _
```

#### `PatchFilterGroup`

``` purescript
newtype PatchFilterGroup
  = PatchFilterGroup { "PatchFilters" :: PatchFilterList }
```

<p>A set of patch filters, typically used for approval rules.</p>

##### Instances
``` purescript
Newtype PatchFilterGroup _
```

#### `PatchFilterKey`

``` purescript
newtype PatchFilterKey
  = PatchFilterKey String
```

##### Instances
``` purescript
Newtype PatchFilterKey _
```

#### `PatchFilterList`

``` purescript
newtype PatchFilterList
  = PatchFilterList (Array PatchFilter)
```

##### Instances
``` purescript
Newtype PatchFilterList _
```

#### `PatchFilterValue`

``` purescript
newtype PatchFilterValue
  = PatchFilterValue String
```

##### Instances
``` purescript
Newtype PatchFilterValue _
```

#### `PatchFilterValueList`

``` purescript
newtype PatchFilterValueList
  = PatchFilterValueList (Array PatchFilterValue)
```

##### Instances
``` purescript
Newtype PatchFilterValueList _
```

#### `PatchGroup`

``` purescript
newtype PatchGroup
  = PatchGroup String
```

##### Instances
``` purescript
Newtype PatchGroup _
```

#### `PatchGroupList`

``` purescript
newtype PatchGroupList
  = PatchGroupList (Array PatchGroup)
```

##### Instances
``` purescript
Newtype PatchGroupList _
```

#### `PatchGroupPatchBaselineMapping`

``` purescript
newtype PatchGroupPatchBaselineMapping
  = PatchGroupPatchBaselineMapping { "PatchGroup" :: NullOrUndefined (PatchGroup), "BaselineIdentity" :: NullOrUndefined (PatchBaselineIdentity) }
```

<p>The mapping between a patch group and the patch baseline the patch group is registered with.</p>

##### Instances
``` purescript
Newtype PatchGroupPatchBaselineMapping _
```

#### `PatchGroupPatchBaselineMappingList`

``` purescript
newtype PatchGroupPatchBaselineMappingList
  = PatchGroupPatchBaselineMappingList (Array PatchGroupPatchBaselineMapping)
```

##### Instances
``` purescript
Newtype PatchGroupPatchBaselineMappingList _
```

#### `PatchId`

``` purescript
newtype PatchId
  = PatchId String
```

##### Instances
``` purescript
Newtype PatchId _
```

#### `PatchIdList`

``` purescript
newtype PatchIdList
  = PatchIdList (Array PatchId)
```

##### Instances
``` purescript
Newtype PatchIdList _
```

#### `PatchInstalledCount`

``` purescript
newtype PatchInstalledCount
  = PatchInstalledCount Int
```

##### Instances
``` purescript
Newtype PatchInstalledCount _
```

#### `PatchInstalledOtherCount`

``` purescript
newtype PatchInstalledOtherCount
  = PatchInstalledOtherCount Int
```

##### Instances
``` purescript
Newtype PatchInstalledOtherCount _
```

#### `PatchKbNumber`

``` purescript
newtype PatchKbNumber
  = PatchKbNumber String
```

##### Instances
``` purescript
Newtype PatchKbNumber _
```

#### `PatchLanguage`

``` purescript
newtype PatchLanguage
  = PatchLanguage String
```

##### Instances
``` purescript
Newtype PatchLanguage _
```

#### `PatchList`

``` purescript
newtype PatchList
  = PatchList (Array Patch)
```

##### Instances
``` purescript
Newtype PatchList _
```

#### `PatchMissingCount`

``` purescript
newtype PatchMissingCount
  = PatchMissingCount Int
```

##### Instances
``` purescript
Newtype PatchMissingCount _
```

#### `PatchMsrcNumber`

``` purescript
newtype PatchMsrcNumber
  = PatchMsrcNumber String
```

##### Instances
``` purescript
Newtype PatchMsrcNumber _
```

#### `PatchMsrcSeverity`

``` purescript
newtype PatchMsrcSeverity
  = PatchMsrcSeverity String
```

##### Instances
``` purescript
Newtype PatchMsrcSeverity _
```

#### `PatchNotApplicableCount`

``` purescript
newtype PatchNotApplicableCount
  = PatchNotApplicableCount Int
```

##### Instances
``` purescript
Newtype PatchNotApplicableCount _
```

#### `PatchOperationType`

``` purescript
newtype PatchOperationType
  = PatchOperationType String
```

##### Instances
``` purescript
Newtype PatchOperationType _
```

#### `PatchOrchestratorFilter`

``` purescript
newtype PatchOrchestratorFilter
  = PatchOrchestratorFilter { "Key" :: NullOrUndefined (PatchOrchestratorFilterKey), "Values" :: NullOrUndefined (PatchOrchestratorFilterValues) }
```

<p>Defines a filter used in Patch Manager APIs.</p>

##### Instances
``` purescript
Newtype PatchOrchestratorFilter _
```

#### `PatchOrchestratorFilterKey`

``` purescript
newtype PatchOrchestratorFilterKey
  = PatchOrchestratorFilterKey String
```

##### Instances
``` purescript
Newtype PatchOrchestratorFilterKey _
```

#### `PatchOrchestratorFilterList`

``` purescript
newtype PatchOrchestratorFilterList
  = PatchOrchestratorFilterList (Array PatchOrchestratorFilter)
```

##### Instances
``` purescript
Newtype PatchOrchestratorFilterList _
```

#### `PatchOrchestratorFilterValue`

``` purescript
newtype PatchOrchestratorFilterValue
  = PatchOrchestratorFilterValue String
```

##### Instances
``` purescript
Newtype PatchOrchestratorFilterValue _
```

#### `PatchOrchestratorFilterValues`

``` purescript
newtype PatchOrchestratorFilterValues
  = PatchOrchestratorFilterValues (Array PatchOrchestratorFilterValue)
```

##### Instances
``` purescript
Newtype PatchOrchestratorFilterValues _
```

#### `PatchProduct`

``` purescript
newtype PatchProduct
  = PatchProduct String
```

##### Instances
``` purescript
Newtype PatchProduct _
```

#### `PatchProductFamily`

``` purescript
newtype PatchProductFamily
  = PatchProductFamily String
```

##### Instances
``` purescript
Newtype PatchProductFamily _
```

#### `PatchRule`

``` purescript
newtype PatchRule
  = PatchRule { "PatchFilterGroup" :: PatchFilterGroup, "ComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApproveAfterDays" :: ApproveAfterDays, "EnableNonSecurity" :: NullOrUndefined (Boolean) }
```

<p>Defines an approval rule for a patch baseline.</p>

##### Instances
``` purescript
Newtype PatchRule _
```

#### `PatchRuleGroup`

``` purescript
newtype PatchRuleGroup
  = PatchRuleGroup { "PatchRules" :: PatchRuleList }
```

<p>A set of rules defining the approval rules for a patch baseline.</p>

##### Instances
``` purescript
Newtype PatchRuleGroup _
```

#### `PatchRuleList`

``` purescript
newtype PatchRuleList
  = PatchRuleList (Array PatchRule)
```

##### Instances
``` purescript
Newtype PatchRuleList _
```

#### `PatchSeverity`

``` purescript
newtype PatchSeverity
  = PatchSeverity String
```

##### Instances
``` purescript
Newtype PatchSeverity _
```

#### `PatchSource`

``` purescript
newtype PatchSource
  = PatchSource { "Name" :: PatchSourceName, "Products" :: PatchSourceProductList, "Configuration" :: PatchSourceConfiguration }
```

<p>Information about the patches to use to update the instances, including target operating systems and source repository. Applies to Linux instances only.</p>

##### Instances
``` purescript
Newtype PatchSource _
```

#### `PatchSourceConfiguration`

``` purescript
newtype PatchSourceConfiguration
  = PatchSourceConfiguration String
```

##### Instances
``` purescript
Newtype PatchSourceConfiguration _
```

#### `PatchSourceList`

``` purescript
newtype PatchSourceList
  = PatchSourceList (Array PatchSource)
```

##### Instances
``` purescript
Newtype PatchSourceList _
```

#### `PatchSourceName`

``` purescript
newtype PatchSourceName
  = PatchSourceName String
```

##### Instances
``` purescript
Newtype PatchSourceName _
```

#### `PatchSourceProduct`

``` purescript
newtype PatchSourceProduct
  = PatchSourceProduct String
```

##### Instances
``` purescript
Newtype PatchSourceProduct _
```

#### `PatchSourceProductList`

``` purescript
newtype PatchSourceProductList
  = PatchSourceProductList (Array PatchSourceProduct)
```

##### Instances
``` purescript
Newtype PatchSourceProductList _
```

#### `PatchStatus`

``` purescript
newtype PatchStatus
  = PatchStatus { "DeploymentStatus" :: NullOrUndefined (PatchDeploymentStatus), "ComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovalDate" :: NullOrUndefined (DateTime) }
```

<p>Information about the approval status of a patch.</p>

##### Instances
``` purescript
Newtype PatchStatus _
```

#### `PatchTitle`

``` purescript
newtype PatchTitle
  = PatchTitle String
```

##### Instances
``` purescript
Newtype PatchTitle _
```

#### `PatchVendor`

``` purescript
newtype PatchVendor
  = PatchVendor String
```

##### Instances
``` purescript
Newtype PatchVendor _
```

#### `PingStatus`

``` purescript
newtype PingStatus
  = PingStatus String
```

##### Instances
``` purescript
Newtype PingStatus _
```

#### `PlatformType`

``` purescript
newtype PlatformType
  = PlatformType String
```

##### Instances
``` purescript
Newtype PlatformType _
```

#### `PlatformTypeList`

``` purescript
newtype PlatformTypeList
  = PlatformTypeList (Array PlatformType)
```

##### Instances
``` purescript
Newtype PlatformTypeList _
```

#### `Product`

``` purescript
newtype Product
  = Product String
```

##### Instances
``` purescript
Newtype Product _
```

#### `PutComplianceItemsRequest`

``` purescript
newtype PutComplianceItemsRequest
  = PutComplianceItemsRequest { "ResourceId" :: ComplianceResourceId, "ResourceType" :: ComplianceResourceType, "ComplianceType" :: ComplianceTypeName, "ExecutionSummary" :: ComplianceExecutionSummary, "Items" :: ComplianceItemEntryList, "ItemContentHash" :: NullOrUndefined (ComplianceItemContentHash) }
```

##### Instances
``` purescript
Newtype PutComplianceItemsRequest _
```

#### `PutComplianceItemsResult`

``` purescript
newtype PutComplianceItemsResult
  = PutComplianceItemsResult {  }
```

##### Instances
``` purescript
Newtype PutComplianceItemsResult _
```

#### `PutInventoryRequest`

``` purescript
newtype PutInventoryRequest
  = PutInventoryRequest { "InstanceId" :: InstanceId, "Items" :: InventoryItemList }
```

##### Instances
``` purescript
Newtype PutInventoryRequest _
```

#### `PutInventoryResult`

``` purescript
newtype PutInventoryResult
  = PutInventoryResult {  }
```

##### Instances
``` purescript
Newtype PutInventoryResult _
```

#### `PutParameterRequest`

``` purescript
newtype PutParameterRequest
  = PutParameterRequest { "Name" :: PSParameterName, "Description" :: NullOrUndefined (ParameterDescription), "Value" :: PSParameterValue, "Type" :: ParameterType, "KeyId" :: NullOrUndefined (ParameterKeyId), "Overwrite" :: NullOrUndefined (Boolean), "AllowedPattern" :: NullOrUndefined (AllowedPattern) }
```

##### Instances
``` purescript
Newtype PutParameterRequest _
```

#### `PutParameterResult`

``` purescript
newtype PutParameterResult
  = PutParameterResult { "Version" :: NullOrUndefined (PSParameterVersion) }
```

##### Instances
``` purescript
Newtype PutParameterResult _
```

#### `RegisterDefaultPatchBaselineRequest`

``` purescript
newtype RegisterDefaultPatchBaselineRequest
  = RegisterDefaultPatchBaselineRequest { "BaselineId" :: BaselineId }
```

##### Instances
``` purescript
Newtype RegisterDefaultPatchBaselineRequest _
```

#### `RegisterDefaultPatchBaselineResult`

``` purescript
newtype RegisterDefaultPatchBaselineResult
  = RegisterDefaultPatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId) }
```

##### Instances
``` purescript
Newtype RegisterDefaultPatchBaselineResult _
```

#### `RegisterPatchBaselineForPatchGroupRequest`

``` purescript
newtype RegisterPatchBaselineForPatchGroupRequest
  = RegisterPatchBaselineForPatchGroupRequest { "BaselineId" :: BaselineId, "PatchGroup" :: PatchGroup }
```

##### Instances
``` purescript
Newtype RegisterPatchBaselineForPatchGroupRequest _
```

#### `RegisterPatchBaselineForPatchGroupResult`

``` purescript
newtype RegisterPatchBaselineForPatchGroupResult
  = RegisterPatchBaselineForPatchGroupResult { "BaselineId" :: NullOrUndefined (BaselineId), "PatchGroup" :: NullOrUndefined (PatchGroup) }
```

##### Instances
``` purescript
Newtype RegisterPatchBaselineForPatchGroupResult _
```

#### `RegisterTargetWithMaintenanceWindowRequest`

``` purescript
newtype RegisterTargetWithMaintenanceWindowRequest
  = RegisterTargetWithMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "ResourceType" :: MaintenanceWindowResourceType, "Targets" :: Targets, "OwnerInformation" :: NullOrUndefined (OwnerInformation), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "ClientToken" :: NullOrUndefined (ClientToken) }
```

##### Instances
``` purescript
Newtype RegisterTargetWithMaintenanceWindowRequest _
```

#### `RegisterTargetWithMaintenanceWindowResult`

``` purescript
newtype RegisterTargetWithMaintenanceWindowResult
  = RegisterTargetWithMaintenanceWindowResult { "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId) }
```

##### Instances
``` purescript
Newtype RegisterTargetWithMaintenanceWindowResult _
```

#### `RegisterTaskWithMaintenanceWindowRequest`

``` purescript
newtype RegisterTaskWithMaintenanceWindowRequest
  = RegisterTaskWithMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "Targets" :: Targets, "TaskArn" :: MaintenanceWindowTaskArn, "ServiceRoleArn" :: ServiceRole, "TaskType" :: MaintenanceWindowTaskType, "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: MaxConcurrency, "MaxErrors" :: MaxErrors, "LoggingInfo" :: NullOrUndefined (LoggingInfo), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "ClientToken" :: NullOrUndefined (ClientToken) }
```

##### Instances
``` purescript
Newtype RegisterTaskWithMaintenanceWindowRequest _
```

#### `RegisterTaskWithMaintenanceWindowResult`

``` purescript
newtype RegisterTaskWithMaintenanceWindowResult
  = RegisterTaskWithMaintenanceWindowResult { "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId) }
```

##### Instances
``` purescript
Newtype RegisterTaskWithMaintenanceWindowResult _
```

#### `RegistrationLimit`

``` purescript
newtype RegistrationLimit
  = RegistrationLimit Int
```

##### Instances
``` purescript
Newtype RegistrationLimit _
```

#### `RegistrationsCount`

``` purescript
newtype RegistrationsCount
  = RegistrationsCount Int
```

##### Instances
``` purescript
Newtype RegistrationsCount _
```

#### `RemoveTagsFromResourceRequest`

``` purescript
newtype RemoveTagsFromResourceRequest
  = RemoveTagsFromResourceRequest { "ResourceType" :: ResourceTypeForTagging, "ResourceId" :: ResourceId, "TagKeys" :: KeyList }
```

##### Instances
``` purescript
Newtype RemoveTagsFromResourceRequest _
```

#### `RemoveTagsFromResourceResult`

``` purescript
newtype RemoveTagsFromResourceResult
  = RemoveTagsFromResourceResult {  }
```

##### Instances
``` purescript
Newtype RemoveTagsFromResourceResult _
```

#### `ResolvedTargets`

``` purescript
newtype ResolvedTargets
  = ResolvedTargets { "ParameterValues" :: NullOrUndefined (TargetParameterList), "Truncated" :: NullOrUndefined (Boolean) }
```

<p>Information about targets that resolved during the Automation execution.</p>

##### Instances
``` purescript
Newtype ResolvedTargets _
```

#### `ResourceComplianceSummaryItem`

``` purescript
newtype ResourceComplianceSummaryItem
  = ResourceComplianceSummaryItem { "ComplianceType" :: NullOrUndefined (ComplianceTypeName), "ResourceType" :: NullOrUndefined (ComplianceResourceType), "ResourceId" :: NullOrUndefined (ComplianceResourceId), "Status" :: NullOrUndefined (ComplianceStatus), "OverallSeverity" :: NullOrUndefined (ComplianceSeverity), "ExecutionSummary" :: NullOrUndefined (ComplianceExecutionSummary), "CompliantSummary" :: NullOrUndefined (CompliantSummary), "NonCompliantSummary" :: NullOrUndefined (NonCompliantSummary) }
```

<p>Compliance summary information for a specific resource. </p>

##### Instances
``` purescript
Newtype ResourceComplianceSummaryItem _
```

#### `ResourceComplianceSummaryItemList`

``` purescript
newtype ResourceComplianceSummaryItemList
  = ResourceComplianceSummaryItemList (Array ResourceComplianceSummaryItem)
```

##### Instances
``` purescript
Newtype ResourceComplianceSummaryItemList _
```

#### `ResourceDataSyncAWSKMSKeyARN`

``` purescript
newtype ResourceDataSyncAWSKMSKeyARN
  = ResourceDataSyncAWSKMSKeyARN String
```

##### Instances
``` purescript
Newtype ResourceDataSyncAWSKMSKeyARN _
```

#### `ResourceDataSyncAlreadyExistsException`

``` purescript
newtype ResourceDataSyncAlreadyExistsException
  = ResourceDataSyncAlreadyExistsException { "SyncName" :: NullOrUndefined (ResourceDataSyncName) }
```

<p>A sync configuration with the same name already exists.</p>

##### Instances
``` purescript
Newtype ResourceDataSyncAlreadyExistsException _
```

#### `ResourceDataSyncCountExceededException`

``` purescript
newtype ResourceDataSyncCountExceededException
  = ResourceDataSyncCountExceededException { "Message" :: NullOrUndefined (String) }
```

<p>You have exceeded the allowed maximum sync configurations.</p>

##### Instances
``` purescript
Newtype ResourceDataSyncCountExceededException _
```

#### `ResourceDataSyncCreatedTime`

``` purescript
newtype ResourceDataSyncCreatedTime
  = ResourceDataSyncCreatedTime Number
```

##### Instances
``` purescript
Newtype ResourceDataSyncCreatedTime _
```

#### `ResourceDataSyncInvalidConfigurationException`

``` purescript
newtype ResourceDataSyncInvalidConfigurationException
  = ResourceDataSyncInvalidConfigurationException { "Message" :: NullOrUndefined (String) }
```

<p>The specified sync configuration is invalid.</p>

##### Instances
``` purescript
Newtype ResourceDataSyncInvalidConfigurationException _
```

#### `ResourceDataSyncItem`

``` purescript
newtype ResourceDataSyncItem
  = ResourceDataSyncItem { "SyncName" :: NullOrUndefined (ResourceDataSyncName), "S3Destination" :: NullOrUndefined (ResourceDataSyncS3Destination), "LastSyncTime" :: NullOrUndefined (LastResourceDataSyncTime), "LastSuccessfulSyncTime" :: NullOrUndefined (LastSuccessfulResourceDataSyncTime), "LastStatus" :: NullOrUndefined (LastResourceDataSyncStatus), "SyncCreatedTime" :: NullOrUndefined (ResourceDataSyncCreatedTime) }
```

<p>Information about a Resource Data Sync configuration, including its current status and last successful sync.</p>

##### Instances
``` purescript
Newtype ResourceDataSyncItem _
```

#### `ResourceDataSyncItemList`

``` purescript
newtype ResourceDataSyncItemList
  = ResourceDataSyncItemList (Array ResourceDataSyncItem)
```

##### Instances
``` purescript
Newtype ResourceDataSyncItemList _
```

#### `ResourceDataSyncName`

``` purescript
newtype ResourceDataSyncName
  = ResourceDataSyncName String
```

##### Instances
``` purescript
Newtype ResourceDataSyncName _
```

#### `ResourceDataSyncNotFoundException`

``` purescript
newtype ResourceDataSyncNotFoundException
  = ResourceDataSyncNotFoundException { "SyncName" :: NullOrUndefined (ResourceDataSyncName) }
```

<p>The specified sync name was not found.</p>

##### Instances
``` purescript
Newtype ResourceDataSyncNotFoundException _
```

#### `ResourceDataSyncS3BucketName`

``` purescript
newtype ResourceDataSyncS3BucketName
  = ResourceDataSyncS3BucketName String
```

##### Instances
``` purescript
Newtype ResourceDataSyncS3BucketName _
```

#### `ResourceDataSyncS3Destination`

``` purescript
newtype ResourceDataSyncS3Destination
  = ResourceDataSyncS3Destination { "BucketName" :: ResourceDataSyncS3BucketName, "Prefix" :: NullOrUndefined (ResourceDataSyncS3Prefix), "SyncFormat" :: ResourceDataSyncS3Format, "Region" :: ResourceDataSyncS3Region, "AWSKMSKeyARN" :: NullOrUndefined (ResourceDataSyncAWSKMSKeyARN) }
```

<p>Information about the target Amazon S3 bucket for the Resource Data Sync.</p>

##### Instances
``` purescript
Newtype ResourceDataSyncS3Destination _
```

#### `ResourceDataSyncS3Format`

``` purescript
newtype ResourceDataSyncS3Format
  = ResourceDataSyncS3Format String
```

##### Instances
``` purescript
Newtype ResourceDataSyncS3Format _
```

#### `ResourceDataSyncS3Prefix`

``` purescript
newtype ResourceDataSyncS3Prefix
  = ResourceDataSyncS3Prefix String
```

##### Instances
``` purescript
Newtype ResourceDataSyncS3Prefix _
```

#### `ResourceDataSyncS3Region`

``` purescript
newtype ResourceDataSyncS3Region
  = ResourceDataSyncS3Region String
```

##### Instances
``` purescript
Newtype ResourceDataSyncS3Region _
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

##### Instances
``` purescript
Newtype ResourceId _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message" :: NullOrUndefined (String) }
```

<p>Error returned if an attempt is made to delete a patch baseline that is registered for a patch group.</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `ResourceLimitExceededException`

``` purescript
newtype ResourceLimitExceededException
  = ResourceLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>Error returned when the caller has exceeded the default resource limits. For example, too many Maintenance Windows or Patch baselines have been created.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>

##### Instances
``` purescript
Newtype ResourceLimitExceededException _
```

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

##### Instances
``` purescript
Newtype ResourceType _
```

#### `ResourceTypeForTagging`

``` purescript
newtype ResourceTypeForTagging
  = ResourceTypeForTagging String
```

##### Instances
``` purescript
Newtype ResourceTypeForTagging _
```

#### `ResponseCode`

``` purescript
newtype ResponseCode
  = ResponseCode Int
```

##### Instances
``` purescript
Newtype ResponseCode _
```

#### `ResultAttribute`

``` purescript
newtype ResultAttribute
  = ResultAttribute { "TypeName" :: InventoryItemTypeName }
```

<p>The inventory item result attribute.</p>

##### Instances
``` purescript
Newtype ResultAttribute _
```

#### `ResultAttributeList`

``` purescript
newtype ResultAttributeList
  = ResultAttributeList (Array ResultAttribute)
```

##### Instances
``` purescript
Newtype ResultAttributeList _
```

#### `S3BucketName`

``` purescript
newtype S3BucketName
  = S3BucketName String
```

##### Instances
``` purescript
Newtype S3BucketName _
```

#### `S3KeyPrefix`

``` purescript
newtype S3KeyPrefix
  = S3KeyPrefix String
```

##### Instances
``` purescript
Newtype S3KeyPrefix _
```

#### `S3OutputLocation`

``` purescript
newtype S3OutputLocation
  = S3OutputLocation { "OutputS3Region" :: NullOrUndefined (S3Region), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix) }
```

<p>An Amazon S3 bucket where you want to store the results of this request.</p>

##### Instances
``` purescript
Newtype S3OutputLocation _
```

#### `S3OutputUrl`

``` purescript
newtype S3OutputUrl
  = S3OutputUrl { "OutputUrl" :: NullOrUndefined (Url) }
```

<p>A URL for the Amazon S3 bucket where you want to store the results of this request.</p>

##### Instances
``` purescript
Newtype S3OutputUrl _
```

#### `S3Region`

``` purescript
newtype S3Region
  = S3Region String
```

##### Instances
``` purescript
Newtype S3Region _
```

#### `ScheduleExpression`

``` purescript
newtype ScheduleExpression
  = ScheduleExpression String
```

##### Instances
``` purescript
Newtype ScheduleExpression _
```

#### `SendAutomationSignalRequest`

``` purescript
newtype SendAutomationSignalRequest
  = SendAutomationSignalRequest { "AutomationExecutionId" :: AutomationExecutionId, "SignalType" :: SignalType, "Payload" :: NullOrUndefined (AutomationParameterMap) }
```

##### Instances
``` purescript
Newtype SendAutomationSignalRequest _
```

#### `SendAutomationSignalResult`

``` purescript
newtype SendAutomationSignalResult
  = SendAutomationSignalResult {  }
```

##### Instances
``` purescript
Newtype SendAutomationSignalResult _
```

#### `SendCommandRequest`

``` purescript
newtype SendCommandRequest
  = SendCommandRequest { "InstanceIds" :: NullOrUndefined (InstanceIdList), "Targets" :: NullOrUndefined (Targets), "DocumentName" :: DocumentARN, "DocumentHash" :: NullOrUndefined (DocumentHash), "DocumentHashType" :: NullOrUndefined (DocumentHashType), "TimeoutSeconds" :: NullOrUndefined (TimeoutSeconds), "Comment" :: NullOrUndefined (Comment), "Parameters" :: NullOrUndefined (Parameters), "OutputS3Region" :: NullOrUndefined (S3Region), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "NotificationConfig" :: NullOrUndefined (NotificationConfig) }
```

##### Instances
``` purescript
Newtype SendCommandRequest _
```

#### `SendCommandResult`

``` purescript
newtype SendCommandResult
  = SendCommandResult { "Command" :: NullOrUndefined (Command) }
```

##### Instances
``` purescript
Newtype SendCommandResult _
```

#### `ServiceRole`

``` purescript
newtype ServiceRole
  = ServiceRole String
```

##### Instances
``` purescript
Newtype ServiceRole _
```

#### `SeveritySummary`

``` purescript
newtype SeveritySummary
  = SeveritySummary { "CriticalCount" :: NullOrUndefined (ComplianceSummaryCount), "HighCount" :: NullOrUndefined (ComplianceSummaryCount), "MediumCount" :: NullOrUndefined (ComplianceSummaryCount), "LowCount" :: NullOrUndefined (ComplianceSummaryCount), "InformationalCount" :: NullOrUndefined (ComplianceSummaryCount), "UnspecifiedCount" :: NullOrUndefined (ComplianceSummaryCount) }
```

<p>The number of managed instances found for each patch severity level defined in the request filter.</p>

##### Instances
``` purescript
Newtype SeveritySummary _
```

#### `SignalType`

``` purescript
newtype SignalType
  = SignalType String
```

##### Instances
``` purescript
Newtype SignalType _
```

#### `SnapshotDownloadUrl`

``` purescript
newtype SnapshotDownloadUrl
  = SnapshotDownloadUrl String
```

##### Instances
``` purescript
Newtype SnapshotDownloadUrl _
```

#### `SnapshotId`

``` purescript
newtype SnapshotId
  = SnapshotId String
```

##### Instances
``` purescript
Newtype SnapshotId _
```

#### `StandardErrorContent`

``` purescript
newtype StandardErrorContent
  = StandardErrorContent String
```

##### Instances
``` purescript
Newtype StandardErrorContent _
```

#### `StandardOutputContent`

``` purescript
newtype StandardOutputContent
  = StandardOutputContent String
```

##### Instances
``` purescript
Newtype StandardOutputContent _
```

#### `StartAutomationExecutionRequest`

``` purescript
newtype StartAutomationExecutionRequest
  = StartAutomationExecutionRequest { "DocumentName" :: DocumentARN, "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Parameters" :: NullOrUndefined (AutomationParameterMap), "ClientToken" :: NullOrUndefined (IdempotencyToken), "Mode" :: NullOrUndefined (ExecutionMode), "TargetParameterName" :: NullOrUndefined (AutomationParameterKey), "Targets" :: NullOrUndefined (Targets), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors) }
```

##### Instances
``` purescript
Newtype StartAutomationExecutionRequest _
```

#### `StartAutomationExecutionResult`

``` purescript
newtype StartAutomationExecutionResult
  = StartAutomationExecutionResult { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId) }
```

##### Instances
``` purescript
Newtype StartAutomationExecutionResult _
```

#### `StatusAdditionalInfo`

``` purescript
newtype StatusAdditionalInfo
  = StatusAdditionalInfo String
```

##### Instances
``` purescript
Newtype StatusAdditionalInfo _
```

#### `StatusDetails`

``` purescript
newtype StatusDetails
  = StatusDetails String
```

##### Instances
``` purescript
Newtype StatusDetails _
```

#### `StatusMessage`

``` purescript
newtype StatusMessage
  = StatusMessage String
```

##### Instances
``` purescript
Newtype StatusMessage _
```

#### `StatusName`

``` purescript
newtype StatusName
  = StatusName String
```

##### Instances
``` purescript
Newtype StatusName _
```

#### `StatusUnchanged`

``` purescript
newtype StatusUnchanged
  = StatusUnchanged {  }
```

<p>The updated status is the same as the current status.</p>

##### Instances
``` purescript
Newtype StatusUnchanged _
```

#### `StepExecution`

``` purescript
newtype StepExecution
  = StepExecution { "StepName" :: NullOrUndefined (String), "Action" :: NullOrUndefined (AutomationActionName), "TimeoutSeconds" :: NullOrUndefined (Number), "OnFailure" :: NullOrUndefined (String), "MaxAttempts" :: NullOrUndefined (Int), "ExecutionStartTime" :: NullOrUndefined (DateTime), "ExecutionEndTime" :: NullOrUndefined (DateTime), "StepStatus" :: NullOrUndefined (AutomationExecutionStatus), "ResponseCode" :: NullOrUndefined (String), "Inputs" :: NullOrUndefined (NormalStringMap), "Outputs" :: NullOrUndefined (AutomationParameterMap), "Response" :: NullOrUndefined (String), "FailureMessage" :: NullOrUndefined (String), "FailureDetails" :: NullOrUndefined (FailureDetails), "StepExecutionId" :: NullOrUndefined (String), "OverriddenParameters" :: NullOrUndefined (AutomationParameterMap) }
```

<p>Detailed information about an the execution state of an Automation step.</p>

##### Instances
``` purescript
Newtype StepExecution _
```

#### `StepExecutionFilter`

``` purescript
newtype StepExecutionFilter
  = StepExecutionFilter { "Key" :: StepExecutionFilterKey, "Values" :: StepExecutionFilterValueList }
```

<p>A filter to limit the amount of step execution information returned by the call.</p>

##### Instances
``` purescript
Newtype StepExecutionFilter _
```

#### `StepExecutionFilterKey`

``` purescript
newtype StepExecutionFilterKey
  = StepExecutionFilterKey String
```

##### Instances
``` purescript
Newtype StepExecutionFilterKey _
```

#### `StepExecutionFilterList`

``` purescript
newtype StepExecutionFilterList
  = StepExecutionFilterList (Array StepExecutionFilter)
```

##### Instances
``` purescript
Newtype StepExecutionFilterList _
```

#### `StepExecutionFilterValue`

``` purescript
newtype StepExecutionFilterValue
  = StepExecutionFilterValue String
```

##### Instances
``` purescript
Newtype StepExecutionFilterValue _
```

#### `StepExecutionFilterValueList`

``` purescript
newtype StepExecutionFilterValueList
  = StepExecutionFilterValueList (Array StepExecutionFilterValue)
```

##### Instances
``` purescript
Newtype StepExecutionFilterValueList _
```

#### `StepExecutionList`

``` purescript
newtype StepExecutionList
  = StepExecutionList (Array StepExecution)
```

##### Instances
``` purescript
Newtype StepExecutionList _
```

#### `StopAutomationExecutionRequest`

``` purescript
newtype StopAutomationExecutionRequest
  = StopAutomationExecutionRequest { "AutomationExecutionId" :: AutomationExecutionId, "Type" :: NullOrUndefined (StopType) }
```

##### Instances
``` purescript
Newtype StopAutomationExecutionRequest _
```

#### `StopAutomationExecutionResult`

``` purescript
newtype StopAutomationExecutionResult
  = StopAutomationExecutionResult {  }
```

##### Instances
``` purescript
Newtype StopAutomationExecutionResult _
```

#### `StopType`

``` purescript
newtype StopType
  = StopType String
```

##### Instances
``` purescript
Newtype StopType _
```

#### `StringDateTime`

``` purescript
newtype StringDateTime
  = StringDateTime String
```

##### Instances
``` purescript
Newtype StringDateTime _
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

##### Instances
``` purescript
Newtype StringList _
```

#### `SubTypeCountLimitExceededException`

``` purescript
newtype SubTypeCountLimitExceededException
  = SubTypeCountLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The sub-type count exceeded the limit for the inventory type.</p>

##### Instances
``` purescript
Newtype SubTypeCountLimitExceededException _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>Metadata that you assign to your AWS resources. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. In Systems Manager, you can apply tags to documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `Target`

``` purescript
newtype Target
  = Target { "Key" :: NullOrUndefined (TargetKey), "Values" :: NullOrUndefined (TargetValues) }
```

<p>An array of search criteria that targets instances using a Key,Value combination that you specify. <code>Targets</code> is required if you don't provide one or more instance IDs in the call.</p> <p/>

##### Instances
``` purescript
Newtype Target _
```

#### `TargetCount`

``` purescript
newtype TargetCount
  = TargetCount Int
```

##### Instances
``` purescript
Newtype TargetCount _
```

#### `TargetInUseException`

``` purescript
newtype TargetInUseException
  = TargetInUseException { "Message" :: NullOrUndefined (String) }
```

<p>You specified the <code>Safe</code> option for the DeregisterTargetFromMaintenanceWindow operation, but the target is still referenced in a task.</p>

##### Instances
``` purescript
Newtype TargetInUseException _
```

#### `TargetKey`

``` purescript
newtype TargetKey
  = TargetKey String
```

##### Instances
``` purescript
Newtype TargetKey _
```

#### `TargetParameterList`

``` purescript
newtype TargetParameterList
  = TargetParameterList (Array ParameterValue)
```

##### Instances
``` purescript
Newtype TargetParameterList _
```

#### `TargetType`

``` purescript
newtype TargetType
  = TargetType String
```

##### Instances
``` purescript
Newtype TargetType _
```

#### `TargetValue`

``` purescript
newtype TargetValue
  = TargetValue String
```

##### Instances
``` purescript
Newtype TargetValue _
```

#### `TargetValues`

``` purescript
newtype TargetValues
  = TargetValues (Array TargetValue)
```

##### Instances
``` purescript
Newtype TargetValues _
```

#### `Targets`

``` purescript
newtype Targets
  = Targets (Array Target)
```

##### Instances
``` purescript
Newtype Targets _
```

#### `TimeoutSeconds`

``` purescript
newtype TimeoutSeconds
  = TimeoutSeconds Int
```

##### Instances
``` purescript
Newtype TimeoutSeconds _
```

#### `TooManyTagsError`

``` purescript
newtype TooManyTagsError
  = TooManyTagsError {  }
```

<p>The Targets parameter includes too many tags. Remove one or more tags and try the command again.</p>

##### Instances
``` purescript
Newtype TooManyTagsError _
```

#### `TooManyUpdates`

``` purescript
newtype TooManyUpdates
  = TooManyUpdates { "Message" :: NullOrUndefined (String) }
```

<p>There are concurrent updates for a resource that supports one update at a time.</p>

##### Instances
``` purescript
Newtype TooManyUpdates _
```

#### `TotalSizeLimitExceededException`

``` purescript
newtype TotalSizeLimitExceededException
  = TotalSizeLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The size of inventory data has exceeded the total size limit for the resource.</p>

##### Instances
``` purescript
Newtype TotalSizeLimitExceededException _
```

#### `UnsupportedInventoryItemContextException`

``` purescript
newtype UnsupportedInventoryItemContextException
  = UnsupportedInventoryItemContextException { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "Message" :: NullOrUndefined (String) }
```

<p>The <code>Context</code> attribute that you specified for the <code>InventoryItem</code> is not allowed for this inventory type. You can only use the <code>Context</code> attribute with inventory types like <code>AWS:ComplianceItem</code>.</p>

##### Instances
``` purescript
Newtype UnsupportedInventoryItemContextException _
```

#### `UnsupportedInventorySchemaVersionException`

``` purescript
newtype UnsupportedInventorySchemaVersionException
  = UnsupportedInventorySchemaVersionException { "Message" :: NullOrUndefined (String) }
```

<p>Inventory item type schema version has to match supported versions in the service. Check output of GetInventorySchema to see the available schema version for each type.</p>

##### Instances
``` purescript
Newtype UnsupportedInventorySchemaVersionException _
```

#### `UnsupportedOperatingSystem`

``` purescript
newtype UnsupportedOperatingSystem
  = UnsupportedOperatingSystem { "Message" :: NullOrUndefined (String) }
```

<p>The operating systems you specified is not supported, or the operation is not supported for the operating system. Valid operating systems include: Windows, AmazonLinux, RedhatEnterpriseLinux, and Ubuntu.</p>

##### Instances
``` purescript
Newtype UnsupportedOperatingSystem _
```

#### `UnsupportedParameterType`

``` purescript
newtype UnsupportedParameterType
  = UnsupportedParameterType { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter type is not supported.</p>

##### Instances
``` purescript
Newtype UnsupportedParameterType _
```

#### `UnsupportedPlatformType`

``` purescript
newtype UnsupportedPlatformType
  = UnsupportedPlatformType { "Message" :: NullOrUndefined (String) }
```

<p>The document does not support the platform type of the given instance ID(s). For example, you sent an document for a Windows instance to a Linux instance.</p>

##### Instances
``` purescript
Newtype UnsupportedPlatformType _
```

#### `UpdateAssociationRequest`

``` purescript
newtype UpdateAssociationRequest
  = UpdateAssociationRequest { "AssociationId" :: AssociationId, "Parameters" :: NullOrUndefined (Parameters), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "Name" :: NullOrUndefined (DocumentName), "Targets" :: NullOrUndefined (Targets), "AssociationName" :: NullOrUndefined (AssociationName), "AssociationVersion" :: NullOrUndefined (AssociationVersion) }
```

##### Instances
``` purescript
Newtype UpdateAssociationRequest _
```

#### `UpdateAssociationResult`

``` purescript
newtype UpdateAssociationResult
  = UpdateAssociationResult { "AssociationDescription" :: NullOrUndefined (AssociationDescription) }
```

##### Instances
``` purescript
Newtype UpdateAssociationResult _
```

#### `UpdateAssociationStatusRequest`

``` purescript
newtype UpdateAssociationStatusRequest
  = UpdateAssociationStatusRequest { "Name" :: DocumentName, "InstanceId" :: InstanceId, "AssociationStatus" :: AssociationStatus }
```

##### Instances
``` purescript
Newtype UpdateAssociationStatusRequest _
```

#### `UpdateAssociationStatusResult`

``` purescript
newtype UpdateAssociationStatusResult
  = UpdateAssociationStatusResult { "AssociationDescription" :: NullOrUndefined (AssociationDescription) }
```

##### Instances
``` purescript
Newtype UpdateAssociationStatusResult _
```

#### `UpdateDocumentDefaultVersionRequest`

``` purescript
newtype UpdateDocumentDefaultVersionRequest
  = UpdateDocumentDefaultVersionRequest { "Name" :: DocumentName, "DocumentVersion" :: DocumentVersionNumber }
```

##### Instances
``` purescript
Newtype UpdateDocumentDefaultVersionRequest _
```

#### `UpdateDocumentDefaultVersionResult`

``` purescript
newtype UpdateDocumentDefaultVersionResult
  = UpdateDocumentDefaultVersionResult { "Description" :: NullOrUndefined (DocumentDefaultVersionDescription) }
```

##### Instances
``` purescript
Newtype UpdateDocumentDefaultVersionResult _
```

#### `UpdateDocumentRequest`

``` purescript
newtype UpdateDocumentRequest
  = UpdateDocumentRequest { "Content" :: DocumentContent, "Name" :: DocumentName, "DocumentVersion" :: NullOrUndefined (DocumentVersion), "DocumentFormat" :: NullOrUndefined (DocumentFormat), "TargetType" :: NullOrUndefined (TargetType) }
```

##### Instances
``` purescript
Newtype UpdateDocumentRequest _
```

#### `UpdateDocumentResult`

``` purescript
newtype UpdateDocumentResult
  = UpdateDocumentResult { "DocumentDescription" :: NullOrUndefined (DocumentDescription) }
```

##### Instances
``` purescript
Newtype UpdateDocumentResult _
```

#### `UpdateMaintenanceWindowRequest`

``` purescript
newtype UpdateMaintenanceWindowRequest
  = UpdateMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Schedule" :: NullOrUndefined (MaintenanceWindowSchedule), "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours), "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff), "AllowUnassociatedTargets" :: NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets), "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled), "Replace" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype UpdateMaintenanceWindowRequest _
```

#### `UpdateMaintenanceWindowResult`

``` purescript
newtype UpdateMaintenanceWindowResult
  = UpdateMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Schedule" :: NullOrUndefined (MaintenanceWindowSchedule), "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours), "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff), "AllowUnassociatedTargets" :: NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets), "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled) }
```

##### Instances
``` purescript
Newtype UpdateMaintenanceWindowResult _
```

#### `UpdateMaintenanceWindowTargetRequest`

``` purescript
newtype UpdateMaintenanceWindowTargetRequest
  = UpdateMaintenanceWindowTargetRequest { "WindowId" :: MaintenanceWindowId, "WindowTargetId" :: MaintenanceWindowTargetId, "Targets" :: NullOrUndefined (Targets), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Replace" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype UpdateMaintenanceWindowTargetRequest _
```

#### `UpdateMaintenanceWindowTargetResult`

``` purescript
newtype UpdateMaintenanceWindowTargetResult
  = UpdateMaintenanceWindowTargetResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId), "Targets" :: NullOrUndefined (Targets), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

##### Instances
``` purescript
Newtype UpdateMaintenanceWindowTargetResult _
```

#### `UpdateMaintenanceWindowTaskRequest`

``` purescript
newtype UpdateMaintenanceWindowTaskRequest
  = UpdateMaintenanceWindowTaskRequest { "WindowId" :: MaintenanceWindowId, "WindowTaskId" :: MaintenanceWindowTaskId, "Targets" :: NullOrUndefined (Targets), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "LoggingInfo" :: NullOrUndefined (LoggingInfo), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Replace" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype UpdateMaintenanceWindowTaskRequest _
```

#### `UpdateMaintenanceWindowTaskResult`

``` purescript
newtype UpdateMaintenanceWindowTaskResult
  = UpdateMaintenanceWindowTaskResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId), "Targets" :: NullOrUndefined (Targets), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "LoggingInfo" :: NullOrUndefined (LoggingInfo), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

##### Instances
``` purescript
Newtype UpdateMaintenanceWindowTaskResult _
```

#### `UpdateManagedInstanceRoleRequest`

``` purescript
newtype UpdateManagedInstanceRoleRequest
  = UpdateManagedInstanceRoleRequest { "InstanceId" :: ManagedInstanceId, "IamRole" :: IamRole }
```

##### Instances
``` purescript
Newtype UpdateManagedInstanceRoleRequest _
```

#### `UpdateManagedInstanceRoleResult`

``` purescript
newtype UpdateManagedInstanceRoleResult
  = UpdateManagedInstanceRoleResult {  }
```

##### Instances
``` purescript
Newtype UpdateManagedInstanceRoleResult _
```

#### `UpdatePatchBaselineRequest`

``` purescript
newtype UpdatePatchBaselineRequest
  = UpdatePatchBaselineRequest { "BaselineId" :: BaselineId, "Name" :: NullOrUndefined (BaselineName), "GlobalFilters" :: NullOrUndefined (PatchFilterGroup), "ApprovalRules" :: NullOrUndefined (PatchRuleGroup), "ApprovedPatches" :: NullOrUndefined (PatchIdList), "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean), "RejectedPatches" :: NullOrUndefined (PatchIdList), "Description" :: NullOrUndefined (BaselineDescription), "Sources" :: NullOrUndefined (PatchSourceList), "Replace" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype UpdatePatchBaselineRequest _
```

#### `UpdatePatchBaselineResult`

``` purescript
newtype UpdatePatchBaselineResult
  = UpdatePatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId), "Name" :: NullOrUndefined (BaselineName), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "GlobalFilters" :: NullOrUndefined (PatchFilterGroup), "ApprovalRules" :: NullOrUndefined (PatchRuleGroup), "ApprovedPatches" :: NullOrUndefined (PatchIdList), "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean), "RejectedPatches" :: NullOrUndefined (PatchIdList), "CreatedDate" :: NullOrUndefined (DateTime), "ModifiedDate" :: NullOrUndefined (DateTime), "Description" :: NullOrUndefined (BaselineDescription), "Sources" :: NullOrUndefined (PatchSourceList) }
```

##### Instances
``` purescript
Newtype UpdatePatchBaselineResult _
```

#### `Url`

``` purescript
newtype Url
  = Url String
```

##### Instances
``` purescript
Newtype Url _
```

#### `Version`

``` purescript
newtype Version
  = Version String
```

##### Instances
``` purescript
Newtype Version _
```


