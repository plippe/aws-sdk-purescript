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

#### `AccountIdList`

``` purescript
newtype AccountIdList
  = AccountIdList (Array AccountId)
```

#### `Activation`

``` purescript
newtype Activation
  = Activation { "ActivationId" :: NullOrUndefined (ActivationId), "Description" :: NullOrUndefined (ActivationDescription), "DefaultInstanceName" :: NullOrUndefined (DefaultInstanceName), "IamRole" :: NullOrUndefined (IamRole), "RegistrationLimit" :: NullOrUndefined (RegistrationLimit), "RegistrationsCount" :: NullOrUndefined (RegistrationsCount), "ExpirationDate" :: NullOrUndefined (ExpirationDate), "Expired" :: NullOrUndefined (Boolean), "CreatedDate" :: NullOrUndefined (CreatedDate) }
```

<p>An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.</p>

#### `ActivationCode`

``` purescript
newtype ActivationCode
  = ActivationCode String
```

#### `ActivationDescription`

``` purescript
newtype ActivationDescription
  = ActivationDescription String
```

#### `ActivationId`

``` purescript
newtype ActivationId
  = ActivationId String
```

#### `ActivationList`

``` purescript
newtype ActivationList
  = ActivationList (Array Activation)
```

#### `AddTagsToResourceRequest`

``` purescript
newtype AddTagsToResourceRequest
  = AddTagsToResourceRequest { "ResourceType" :: ResourceTypeForTagging, "ResourceId" :: ResourceId, "Tags" :: TagList }
```

#### `AddTagsToResourceResult`

``` purescript
newtype AddTagsToResourceResult
  = AddTagsToResourceResult {  }
```

#### `AgentErrorCode`

``` purescript
newtype AgentErrorCode
  = AgentErrorCode String
```

#### `AggregatorSchemaOnly`

``` purescript
newtype AggregatorSchemaOnly
  = AggregatorSchemaOnly Boolean
```

#### `AllowedPattern`

``` purescript
newtype AllowedPattern
  = AllowedPattern String
```

#### `AlreadyExistsException`

``` purescript
newtype AlreadyExistsException
  = AlreadyExistsException { "Message" :: NullOrUndefined (String) }
```

<p>Error returned if an attempt is made to register a patch group with a patch baseline that is already registered with a different patch baseline.</p>

#### `ApproveAfterDays`

``` purescript
newtype ApproveAfterDays
  = ApproveAfterDays Int
```

#### `AssociatedInstances`

``` purescript
newtype AssociatedInstances
  = AssociatedInstances {  }
```

<p>You must disassociate a document from all instances before you can delete it.</p>

#### `Association`

``` purescript
newtype Association
  = Association { "Name" :: NullOrUndefined (DocumentName), "InstanceId" :: NullOrUndefined (InstanceId), "AssociationId" :: NullOrUndefined (AssociationId), "AssociationVersion" :: NullOrUndefined (AssociationVersion), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Targets" :: NullOrUndefined (Targets), "LastExecutionDate" :: NullOrUndefined (DateTime), "Overview" :: NullOrUndefined (AssociationOverview), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Describes an association of a Systems Manager document and an instance.</p>

#### `AssociationAlreadyExists`

``` purescript
newtype AssociationAlreadyExists
  = AssociationAlreadyExists {  }
```

<p>The specified association already exists.</p>

#### `AssociationDescription`

``` purescript
newtype AssociationDescription
  = AssociationDescription { "Name" :: NullOrUndefined (DocumentName), "InstanceId" :: NullOrUndefined (InstanceId), "AssociationVersion" :: NullOrUndefined (AssociationVersion), "Date" :: NullOrUndefined (DateTime), "LastUpdateAssociationDate" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (AssociationStatus), "Overview" :: NullOrUndefined (AssociationOverview), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Parameters" :: NullOrUndefined (Parameters), "AssociationId" :: NullOrUndefined (AssociationId), "Targets" :: NullOrUndefined (Targets), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "LastExecutionDate" :: NullOrUndefined (DateTime), "LastSuccessfulExecutionDate" :: NullOrUndefined (DateTime), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Describes the parameters for a document.</p>

#### `AssociationDescriptionList`

``` purescript
newtype AssociationDescriptionList
  = AssociationDescriptionList (Array AssociationDescription)
```

#### `AssociationDoesNotExist`

``` purescript
newtype AssociationDoesNotExist
  = AssociationDoesNotExist { "Message" :: NullOrUndefined (String) }
```

<p>The specified association does not exist.</p>

#### `AssociationFilter`

``` purescript
newtype AssociationFilter
  = AssociationFilter { "Key'" :: AssociationFilterKey, "Value'" :: AssociationFilterValue }
```

<p>Describes a filter.</p>

#### `AssociationFilterKey`

``` purescript
newtype AssociationFilterKey
  = AssociationFilterKey String
```

#### `AssociationFilterList`

``` purescript
newtype AssociationFilterList
  = AssociationFilterList (Array AssociationFilter)
```

#### `AssociationFilterValue`

``` purescript
newtype AssociationFilterValue
  = AssociationFilterValue String
```

#### `AssociationId`

``` purescript
newtype AssociationId
  = AssociationId String
```

#### `AssociationLimitExceeded`

``` purescript
newtype AssociationLimitExceeded
  = AssociationLimitExceeded {  }
```

<p>You can have at most 2,000 active associations.</p>

#### `AssociationList`

``` purescript
newtype AssociationList
  = AssociationList (Array Association)
```

#### `AssociationName`

``` purescript
newtype AssociationName
  = AssociationName String
```

#### `AssociationOverview`

``` purescript
newtype AssociationOverview
  = AssociationOverview { "Status" :: NullOrUndefined (StatusName), "DetailedStatus" :: NullOrUndefined (StatusName), "AssociationStatusAggregatedCount" :: NullOrUndefined (AssociationStatusAggregatedCount) }
```

<p>Information about the association.</p>

#### `AssociationStatus`

``` purescript
newtype AssociationStatus
  = AssociationStatus { "Date" :: DateTime, "Name" :: AssociationStatusName, "Message" :: StatusMessage, "AdditionalInfo" :: NullOrUndefined (StatusAdditionalInfo) }
```

<p>Describes an association status.</p>

#### `AssociationStatusAggregatedCount`

``` purescript
newtype AssociationStatusAggregatedCount
  = AssociationStatusAggregatedCount (Map StatusName InstanceCount)
```

#### `AssociationStatusName`

``` purescript
newtype AssociationStatusName
  = AssociationStatusName String
```

#### `AssociationVersion`

``` purescript
newtype AssociationVersion
  = AssociationVersion String
```

#### `AssociationVersionInfo`

``` purescript
newtype AssociationVersionInfo
  = AssociationVersionInfo { "AssociationId" :: NullOrUndefined (AssociationId), "AssociationVersion" :: NullOrUndefined (AssociationVersion), "CreatedDate" :: NullOrUndefined (DateTime), "Name" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Parameters" :: NullOrUndefined (Parameters), "Targets" :: NullOrUndefined (Targets), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Information about the association version.</p>

#### `AssociationVersionLimitExceeded`

``` purescript
newtype AssociationVersionLimitExceeded
  = AssociationVersionLimitExceeded { "Message" :: NullOrUndefined (String) }
```

<p>You have reached the maximum number versions allowed for an association. Each association has a limit of 1,000 versions. </p>

#### `AssociationVersionList`

``` purescript
newtype AssociationVersionList
  = AssociationVersionList (Array AssociationVersionInfo)
```

#### `AttributeName`

``` purescript
newtype AttributeName
  = AttributeName String
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

#### `AutomationActionName`

``` purescript
newtype AutomationActionName
  = AutomationActionName String
```

#### `AutomationDefinitionNotFoundException`

``` purescript
newtype AutomationDefinitionNotFoundException
  = AutomationDefinitionNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>An Automation document with the specified name could not be found.</p>

#### `AutomationDefinitionVersionNotFoundException`

``` purescript
newtype AutomationDefinitionVersionNotFoundException
  = AutomationDefinitionVersionNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>An Automation document with the specified name and version could not be found.</p>

#### `AutomationExecution`

``` purescript
newtype AutomationExecution
  = AutomationExecution { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId), "DocumentName" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "ExecutionStartTime" :: NullOrUndefined (DateTime), "ExecutionEndTime" :: NullOrUndefined (DateTime), "AutomationExecutionStatus" :: NullOrUndefined (AutomationExecutionStatus), "StepExecutions" :: NullOrUndefined (StepExecutionList), "StepExecutionsTruncated" :: NullOrUndefined (Boolean), "Parameters" :: NullOrUndefined (AutomationParameterMap), "Outputs" :: NullOrUndefined (AutomationParameterMap), "FailureMessage" :: NullOrUndefined (String), "Mode" :: NullOrUndefined (ExecutionMode), "ParentAutomationExecutionId" :: NullOrUndefined (AutomationExecutionId), "ExecutedBy" :: NullOrUndefined (String), "CurrentStepName" :: NullOrUndefined (String), "CurrentAction" :: NullOrUndefined (String), "TargetParameterName" :: NullOrUndefined (AutomationParameterKey), "Targets" :: NullOrUndefined (Targets), "ResolvedTargets" :: NullOrUndefined (ResolvedTargets), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "Target" :: NullOrUndefined (String) }
```

<p>Detailed information about the current state of an individual Automation execution.</p>

#### `AutomationExecutionFilter`

``` purescript
newtype AutomationExecutionFilter
  = AutomationExecutionFilter { "Key" :: AutomationExecutionFilterKey, "Values" :: AutomationExecutionFilterValueList }
```

<p>A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.</p>

#### `AutomationExecutionFilterKey`

``` purescript
newtype AutomationExecutionFilterKey
  = AutomationExecutionFilterKey String
```

#### `AutomationExecutionFilterList`

``` purescript
newtype AutomationExecutionFilterList
  = AutomationExecutionFilterList (Array AutomationExecutionFilter)
```

#### `AutomationExecutionFilterValue`

``` purescript
newtype AutomationExecutionFilterValue
  = AutomationExecutionFilterValue String
```

#### `AutomationExecutionFilterValueList`

``` purescript
newtype AutomationExecutionFilterValueList
  = AutomationExecutionFilterValueList (Array AutomationExecutionFilterValue)
```

#### `AutomationExecutionId`

``` purescript
newtype AutomationExecutionId
  = AutomationExecutionId String
```

#### `AutomationExecutionLimitExceededException`

``` purescript
newtype AutomationExecutionLimitExceededException
  = AutomationExecutionLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The number of simultaneously running Automation executions exceeded the allowable limit.</p>

#### `AutomationExecutionMetadata`

``` purescript
newtype AutomationExecutionMetadata
  = AutomationExecutionMetadata { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId), "DocumentName" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "AutomationExecutionStatus" :: NullOrUndefined (AutomationExecutionStatus), "ExecutionStartTime" :: NullOrUndefined (DateTime), "ExecutionEndTime" :: NullOrUndefined (DateTime), "ExecutedBy" :: NullOrUndefined (String), "LogFile" :: NullOrUndefined (String), "Outputs" :: NullOrUndefined (AutomationParameterMap), "Mode" :: NullOrUndefined (ExecutionMode), "ParentAutomationExecutionId" :: NullOrUndefined (AutomationExecutionId), "CurrentStepName" :: NullOrUndefined (String), "CurrentAction" :: NullOrUndefined (String), "FailureMessage" :: NullOrUndefined (String), "TargetParameterName" :: NullOrUndefined (AutomationParameterKey), "Targets" :: NullOrUndefined (Targets), "ResolvedTargets" :: NullOrUndefined (ResolvedTargets), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "Target" :: NullOrUndefined (String) }
```

<p>Details about a specific Automation execution.</p>

#### `AutomationExecutionMetadataList`

``` purescript
newtype AutomationExecutionMetadataList
  = AutomationExecutionMetadataList (Array AutomationExecutionMetadata)
```

#### `AutomationExecutionNotFoundException`

``` purescript
newtype AutomationExecutionNotFoundException
  = AutomationExecutionNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>There is no automation execution information for the requested automation execution ID.</p>

#### `AutomationExecutionStatus`

``` purescript
newtype AutomationExecutionStatus
  = AutomationExecutionStatus String
```

#### `AutomationParameterKey`

``` purescript
newtype AutomationParameterKey
  = AutomationParameterKey String
```

#### `AutomationParameterMap`

``` purescript
newtype AutomationParameterMap
  = AutomationParameterMap (Map AutomationParameterKey AutomationParameterValueList)
```

#### `AutomationParameterValue`

``` purescript
newtype AutomationParameterValue
  = AutomationParameterValue String
```

#### `AutomationParameterValueList`

``` purescript
newtype AutomationParameterValueList
  = AutomationParameterValueList (Array AutomationParameterValue)
```

#### `AutomationStepNotFoundException`

``` purescript
newtype AutomationStepNotFoundException
  = AutomationStepNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>The specified step name and execution ID don't exist. Verify the information and try again.</p>

#### `BaselineDescription`

``` purescript
newtype BaselineDescription
  = BaselineDescription String
```

#### `BaselineId`

``` purescript
newtype BaselineId
  = BaselineId String
```

#### `BaselineName`

``` purescript
newtype BaselineName
  = BaselineName String
```

#### `BatchErrorMessage`

``` purescript
newtype BatchErrorMessage
  = BatchErrorMessage String
```

#### `CancelCommandRequest`

``` purescript
newtype CancelCommandRequest
  = CancelCommandRequest { "CommandId" :: CommandId, "InstanceIds" :: NullOrUndefined (InstanceIdList) }
```

<p/>

#### `CancelCommandResult`

``` purescript
newtype CancelCommandResult
  = CancelCommandResult {  }
```

<p>Whether or not the command was successfully canceled. There is no guarantee that a request can be canceled.</p>

#### `ClientToken`

``` purescript
newtype ClientToken
  = ClientToken String
```

#### `Command`

``` purescript
newtype Command
  = Command { "CommandId" :: NullOrUndefined (CommandId), "DocumentName" :: NullOrUndefined (DocumentName), "Comment" :: NullOrUndefined (Comment), "ExpiresAfter" :: NullOrUndefined (DateTime), "Parameters" :: NullOrUndefined (Parameters), "InstanceIds" :: NullOrUndefined (InstanceIdList), "Targets" :: NullOrUndefined (Targets), "RequestedDateTime" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (CommandStatus), "StatusDetails" :: NullOrUndefined (StatusDetails), "OutputS3Region" :: NullOrUndefined (S3Region), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "TargetCount" :: NullOrUndefined (TargetCount), "CompletedCount" :: NullOrUndefined (CompletedCount), "ErrorCount" :: NullOrUndefined (ErrorCount), "ServiceRole" :: NullOrUndefined (ServiceRole), "NotificationConfig" :: NullOrUndefined (NotificationConfig) }
```

<p>Describes a command request.</p>

#### `CommandFilter`

``` purescript
newtype CommandFilter
  = CommandFilter { "Key'" :: CommandFilterKey, "Value'" :: CommandFilterValue }
```

<p>Describes a command filter.</p>

#### `CommandFilterKey`

``` purescript
newtype CommandFilterKey
  = CommandFilterKey String
```

#### `CommandFilterList`

``` purescript
newtype CommandFilterList
  = CommandFilterList (Array CommandFilter)
```

#### `CommandFilterValue`

``` purescript
newtype CommandFilterValue
  = CommandFilterValue String
```

#### `CommandId`

``` purescript
newtype CommandId
  = CommandId String
```

#### `CommandInvocation`

``` purescript
newtype CommandInvocation
  = CommandInvocation { "CommandId" :: NullOrUndefined (CommandId), "InstanceId" :: NullOrUndefined (InstanceId), "InstanceName" :: NullOrUndefined (InstanceTagName), "Comment" :: NullOrUndefined (Comment), "DocumentName" :: NullOrUndefined (DocumentName), "RequestedDateTime" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (CommandInvocationStatus), "StatusDetails" :: NullOrUndefined (StatusDetails), "TraceOutput" :: NullOrUndefined (InvocationTraceOutput), "StandardOutputUrl" :: NullOrUndefined (Url), "StandardErrorUrl" :: NullOrUndefined (Url), "CommandPlugins" :: NullOrUndefined (CommandPluginList), "ServiceRole" :: NullOrUndefined (ServiceRole), "NotificationConfig" :: NullOrUndefined (NotificationConfig) }
```

<p>An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. A command invocation returns status and detail information about a command you executed. </p>

#### `CommandInvocationList`

``` purescript
newtype CommandInvocationList
  = CommandInvocationList (Array CommandInvocation)
```

#### `CommandInvocationStatus`

``` purescript
newtype CommandInvocationStatus
  = CommandInvocationStatus String
```

#### `CommandList`

``` purescript
newtype CommandList
  = CommandList (Array Command)
```

#### `CommandMaxResults`

``` purescript
newtype CommandMaxResults
  = CommandMaxResults Int
```

#### `CommandPlugin`

``` purescript
newtype CommandPlugin
  = CommandPlugin { "Name" :: NullOrUndefined (CommandPluginName), "Status" :: NullOrUndefined (CommandPluginStatus), "StatusDetails" :: NullOrUndefined (StatusDetails), "ResponseCode" :: NullOrUndefined (ResponseCode), "ResponseStartDateTime" :: NullOrUndefined (DateTime), "ResponseFinishDateTime" :: NullOrUndefined (DateTime), "Output" :: NullOrUndefined (CommandPluginOutput), "StandardOutputUrl" :: NullOrUndefined (Url), "StandardErrorUrl" :: NullOrUndefined (Url), "OutputS3Region" :: NullOrUndefined (S3Region), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix) }
```

<p>Describes plugin details.</p>

#### `CommandPluginList`

``` purescript
newtype CommandPluginList
  = CommandPluginList (Array CommandPlugin)
```

#### `CommandPluginName`

``` purescript
newtype CommandPluginName
  = CommandPluginName String
```

#### `CommandPluginOutput`

``` purescript
newtype CommandPluginOutput
  = CommandPluginOutput String
```

#### `CommandPluginStatus`

``` purescript
newtype CommandPluginStatus
  = CommandPluginStatus String
```

#### `CommandStatus`

``` purescript
newtype CommandStatus
  = CommandStatus String
```

#### `Comment`

``` purescript
newtype Comment
  = Comment String
```

#### `CompletedCount`

``` purescript
newtype CompletedCount
  = CompletedCount Int
```

#### `ComplianceExecutionId`

``` purescript
newtype ComplianceExecutionId
  = ComplianceExecutionId String
```

#### `ComplianceExecutionSummary`

``` purescript
newtype ComplianceExecutionSummary
  = ComplianceExecutionSummary { "ExecutionTime" :: DateTime, "ExecutionId" :: NullOrUndefined (ComplianceExecutionId), "ExecutionType" :: NullOrUndefined (ComplianceExecutionType) }
```

<p>A summary of the call execution that includes an execution ID, the type of execution (for example, <code>Command</code>), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.</p>

#### `ComplianceExecutionType`

``` purescript
newtype ComplianceExecutionType
  = ComplianceExecutionType String
```

#### `ComplianceFilterValue`

``` purescript
newtype ComplianceFilterValue
  = ComplianceFilterValue String
```

#### `ComplianceItem`

``` purescript
newtype ComplianceItem
  = ComplianceItem { "ComplianceType" :: NullOrUndefined (ComplianceTypeName), "ResourceType" :: NullOrUndefined (ComplianceResourceType), "ResourceId" :: NullOrUndefined (ComplianceResourceId), "Id" :: NullOrUndefined (ComplianceItemId), "Title" :: NullOrUndefined (ComplianceItemTitle), "Status" :: NullOrUndefined (ComplianceStatus), "Severity" :: NullOrUndefined (ComplianceSeverity), "ExecutionSummary" :: NullOrUndefined (ComplianceExecutionSummary), "Details" :: NullOrUndefined (ComplianceItemDetails) }
```

<p>Information about the compliance as defined by the resource type. For example, for a patch resource type, <code>Items</code> includes information about the PatchSeverity, Classification, etc.</p>

#### `ComplianceItemContentHash`

``` purescript
newtype ComplianceItemContentHash
  = ComplianceItemContentHash String
```

#### `ComplianceItemDetails`

``` purescript
newtype ComplianceItemDetails
  = ComplianceItemDetails (Map AttributeName AttributeValue)
```

#### `ComplianceItemEntry`

``` purescript
newtype ComplianceItemEntry
  = ComplianceItemEntry { "Id" :: NullOrUndefined (ComplianceItemId), "Title" :: NullOrUndefined (ComplianceItemTitle), "Severity" :: ComplianceSeverity, "Status" :: ComplianceStatus, "Details" :: NullOrUndefined (ComplianceItemDetails) }
```

<p>Information about a compliance item.</p>

#### `ComplianceItemEntryList`

``` purescript
newtype ComplianceItemEntryList
  = ComplianceItemEntryList (Array ComplianceItemEntry)
```

#### `ComplianceItemId`

``` purescript
newtype ComplianceItemId
  = ComplianceItemId String
```

#### `ComplianceItemList`

``` purescript
newtype ComplianceItemList
  = ComplianceItemList (Array ComplianceItem)
```

#### `ComplianceItemTitle`

``` purescript
newtype ComplianceItemTitle
  = ComplianceItemTitle String
```

#### `ComplianceQueryOperatorType`

``` purescript
newtype ComplianceQueryOperatorType
  = ComplianceQueryOperatorType String
```

#### `ComplianceResourceId`

``` purescript
newtype ComplianceResourceId
  = ComplianceResourceId String
```

#### `ComplianceResourceIdList`

``` purescript
newtype ComplianceResourceIdList
  = ComplianceResourceIdList (Array ComplianceResourceId)
```

#### `ComplianceResourceType`

``` purescript
newtype ComplianceResourceType
  = ComplianceResourceType String
```

#### `ComplianceResourceTypeList`

``` purescript
newtype ComplianceResourceTypeList
  = ComplianceResourceTypeList (Array ComplianceResourceType)
```

#### `ComplianceSeverity`

``` purescript
newtype ComplianceSeverity
  = ComplianceSeverity String
```

#### `ComplianceStatus`

``` purescript
newtype ComplianceStatus
  = ComplianceStatus String
```

#### `ComplianceStringFilter`

``` purescript
newtype ComplianceStringFilter
  = ComplianceStringFilter { "Key" :: NullOrUndefined (ComplianceStringFilterKey), "Values" :: NullOrUndefined (ComplianceStringFilterValueList), "Type" :: NullOrUndefined (ComplianceQueryOperatorType) }
```

<p>One or more filters. Use a filter to return a more specific list of results.</p>

#### `ComplianceStringFilterKey`

``` purescript
newtype ComplianceStringFilterKey
  = ComplianceStringFilterKey String
```

#### `ComplianceStringFilterList`

``` purescript
newtype ComplianceStringFilterList
  = ComplianceStringFilterList (Array ComplianceStringFilter)
```

#### `ComplianceStringFilterValueList`

``` purescript
newtype ComplianceStringFilterValueList
  = ComplianceStringFilterValueList (Array ComplianceFilterValue)
```

#### `ComplianceSummaryCount`

``` purescript
newtype ComplianceSummaryCount
  = ComplianceSummaryCount Int
```

#### `ComplianceSummaryItem`

``` purescript
newtype ComplianceSummaryItem
  = ComplianceSummaryItem { "ComplianceType" :: NullOrUndefined (ComplianceTypeName), "CompliantSummary" :: NullOrUndefined (CompliantSummary), "NonCompliantSummary" :: NullOrUndefined (NonCompliantSummary) }
```

<p>A summary of compliance information by compliance type.</p>

#### `ComplianceSummaryItemList`

``` purescript
newtype ComplianceSummaryItemList
  = ComplianceSummaryItemList (Array ComplianceSummaryItem)
```

#### `ComplianceTypeCountLimitExceededException`

``` purescript
newtype ComplianceTypeCountLimitExceededException
  = ComplianceTypeCountLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>You specified too many custom compliance types. You can specify a maximum of 10 different types. </p>

#### `ComplianceTypeName`

``` purescript
newtype ComplianceTypeName
  = ComplianceTypeName String
```

#### `CompliantSummary`

``` purescript
newtype CompliantSummary
  = CompliantSummary { "CompliantCount" :: NullOrUndefined (ComplianceSummaryCount), "SeveritySummary" :: NullOrUndefined (SeveritySummary) }
```

<p>A summary of resources that are compliant. The summary is organized according to the resource count for each compliance type.</p>

#### `ComputerName`

``` purescript
newtype ComputerName
  = ComputerName String
```

#### `CreateActivationRequest`

``` purescript
newtype CreateActivationRequest
  = CreateActivationRequest { "Description" :: NullOrUndefined (ActivationDescription), "DefaultInstanceName" :: NullOrUndefined (DefaultInstanceName), "IamRole" :: IamRole, "RegistrationLimit" :: NullOrUndefined (RegistrationLimit), "ExpirationDate" :: NullOrUndefined (ExpirationDate) }
```

#### `CreateActivationResult`

``` purescript
newtype CreateActivationResult
  = CreateActivationResult { "ActivationId" :: NullOrUndefined (ActivationId), "ActivationCode" :: NullOrUndefined (ActivationCode) }
```

#### `CreateAssociationBatchRequest`

``` purescript
newtype CreateAssociationBatchRequest
  = CreateAssociationBatchRequest { "Entries" :: CreateAssociationBatchRequestEntries }
```

#### `CreateAssociationBatchRequestEntries`

``` purescript
newtype CreateAssociationBatchRequestEntries
  = CreateAssociationBatchRequestEntries (Array CreateAssociationBatchRequestEntry)
```

#### `CreateAssociationBatchRequestEntry`

``` purescript
newtype CreateAssociationBatchRequestEntry
  = CreateAssociationBatchRequestEntry { "Name" :: DocumentName, "InstanceId" :: NullOrUndefined (InstanceId), "Parameters" :: NullOrUndefined (Parameters), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Targets" :: NullOrUndefined (Targets), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Describes the association of a Systems Manager document and an instance.</p>

#### `CreateAssociationBatchResult`

``` purescript
newtype CreateAssociationBatchResult
  = CreateAssociationBatchResult { "Successful" :: NullOrUndefined (AssociationDescriptionList), "Failed" :: NullOrUndefined (FailedCreateAssociationList) }
```

#### `CreateAssociationRequest`

``` purescript
newtype CreateAssociationRequest
  = CreateAssociationRequest { "Name" :: DocumentName, "DocumentVersion" :: NullOrUndefined (DocumentVersion), "InstanceId" :: NullOrUndefined (InstanceId), "Parameters" :: NullOrUndefined (Parameters), "Targets" :: NullOrUndefined (Targets), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "AssociationName" :: NullOrUndefined (AssociationName) }
```

#### `CreateAssociationResult`

``` purescript
newtype CreateAssociationResult
  = CreateAssociationResult { "AssociationDescription" :: NullOrUndefined (AssociationDescription) }
```

#### `CreateDocumentRequest`

``` purescript
newtype CreateDocumentRequest
  = CreateDocumentRequest { "Content" :: DocumentContent, "Name" :: DocumentName, "DocumentType" :: NullOrUndefined (DocumentType), "DocumentFormat" :: NullOrUndefined (DocumentFormat), "TargetType" :: NullOrUndefined (TargetType) }
```

#### `CreateDocumentResult`

``` purescript
newtype CreateDocumentResult
  = CreateDocumentResult { "DocumentDescription" :: NullOrUndefined (DocumentDescription) }
```

#### `CreateMaintenanceWindowRequest`

``` purescript
newtype CreateMaintenanceWindowRequest
  = CreateMaintenanceWindowRequest { "Name" :: MaintenanceWindowName, "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Schedule" :: MaintenanceWindowSchedule, "Duration" :: MaintenanceWindowDurationHours, "Cutoff" :: MaintenanceWindowCutoff, "AllowUnassociatedTargets" :: MaintenanceWindowAllowUnassociatedTargets, "ClientToken" :: NullOrUndefined (ClientToken) }
```

#### `CreateMaintenanceWindowResult`

``` purescript
newtype CreateMaintenanceWindowResult
  = CreateMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId) }
```

#### `CreatePatchBaselineRequest`

``` purescript
newtype CreatePatchBaselineRequest
  = CreatePatchBaselineRequest { "OperatingSystem" :: NullOrUndefined (OperatingSystem), "Name" :: BaselineName, "GlobalFilters" :: NullOrUndefined (PatchFilterGroup), "ApprovalRules" :: NullOrUndefined (PatchRuleGroup), "ApprovedPatches" :: NullOrUndefined (PatchIdList), "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean), "RejectedPatches" :: NullOrUndefined (PatchIdList), "Description" :: NullOrUndefined (BaselineDescription), "Sources" :: NullOrUndefined (PatchSourceList), "ClientToken" :: NullOrUndefined (ClientToken) }
```

#### `CreatePatchBaselineResult`

``` purescript
newtype CreatePatchBaselineResult
  = CreatePatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId) }
```

#### `CreateResourceDataSyncRequest`

``` purescript
newtype CreateResourceDataSyncRequest
  = CreateResourceDataSyncRequest { "SyncName" :: ResourceDataSyncName, "S3Destination" :: ResourceDataSyncS3Destination }
```

#### `CreateResourceDataSyncResult`

``` purescript
newtype CreateResourceDataSyncResult
  = CreateResourceDataSyncResult {  }
```

#### `CreatedDate`

``` purescript
newtype CreatedDate
  = CreatedDate Number
```

#### `CustomSchemaCountLimitExceededException`

``` purescript
newtype CustomSchemaCountLimitExceededException
  = CustomSchemaCountLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>You have exceeded the limit for custom schemas. Delete one or more custom schemas and try again.</p>

#### `DateTime`

``` purescript
newtype DateTime
  = DateTime Number
```

#### `DefaultBaseline`

``` purescript
newtype DefaultBaseline
  = DefaultBaseline Boolean
```

#### `DefaultInstanceName`

``` purescript
newtype DefaultInstanceName
  = DefaultInstanceName String
```

#### `DeleteActivationRequest`

``` purescript
newtype DeleteActivationRequest
  = DeleteActivationRequest { "ActivationId" :: ActivationId }
```

#### `DeleteActivationResult`

``` purescript
newtype DeleteActivationResult
  = DeleteActivationResult {  }
```

#### `DeleteAssociationRequest`

``` purescript
newtype DeleteAssociationRequest
  = DeleteAssociationRequest { "Name" :: NullOrUndefined (DocumentName), "InstanceId" :: NullOrUndefined (InstanceId), "AssociationId" :: NullOrUndefined (AssociationId) }
```

#### `DeleteAssociationResult`

``` purescript
newtype DeleteAssociationResult
  = DeleteAssociationResult {  }
```

#### `DeleteDocumentRequest`

``` purescript
newtype DeleteDocumentRequest
  = DeleteDocumentRequest { "Name" :: DocumentName }
```

#### `DeleteDocumentResult`

``` purescript
newtype DeleteDocumentResult
  = DeleteDocumentResult {  }
```

#### `DeleteMaintenanceWindowRequest`

``` purescript
newtype DeleteMaintenanceWindowRequest
  = DeleteMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId }
```

#### `DeleteMaintenanceWindowResult`

``` purescript
newtype DeleteMaintenanceWindowResult
  = DeleteMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId) }
```

#### `DeleteParameterRequest`

``` purescript
newtype DeleteParameterRequest
  = DeleteParameterRequest { "Name" :: PSParameterName }
```

#### `DeleteParameterResult`

``` purescript
newtype DeleteParameterResult
  = DeleteParameterResult {  }
```

#### `DeleteParametersRequest`

``` purescript
newtype DeleteParametersRequest
  = DeleteParametersRequest { "Names" :: ParameterNameList }
```

#### `DeleteParametersResult`

``` purescript
newtype DeleteParametersResult
  = DeleteParametersResult { "DeletedParameters" :: NullOrUndefined (ParameterNameList), "InvalidParameters" :: NullOrUndefined (ParameterNameList) }
```

#### `DeletePatchBaselineRequest`

``` purescript
newtype DeletePatchBaselineRequest
  = DeletePatchBaselineRequest { "BaselineId" :: BaselineId }
```

#### `DeletePatchBaselineResult`

``` purescript
newtype DeletePatchBaselineResult
  = DeletePatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId) }
```

#### `DeleteResourceDataSyncRequest`

``` purescript
newtype DeleteResourceDataSyncRequest
  = DeleteResourceDataSyncRequest { "SyncName" :: ResourceDataSyncName }
```

#### `DeleteResourceDataSyncResult`

``` purescript
newtype DeleteResourceDataSyncResult
  = DeleteResourceDataSyncResult {  }
```

#### `DeregisterManagedInstanceRequest`

``` purescript
newtype DeregisterManagedInstanceRequest
  = DeregisterManagedInstanceRequest { "InstanceId" :: ManagedInstanceId }
```

#### `DeregisterManagedInstanceResult`

``` purescript
newtype DeregisterManagedInstanceResult
  = DeregisterManagedInstanceResult {  }
```

#### `DeregisterPatchBaselineForPatchGroupRequest`

``` purescript
newtype DeregisterPatchBaselineForPatchGroupRequest
  = DeregisterPatchBaselineForPatchGroupRequest { "BaselineId" :: BaselineId, "PatchGroup" :: PatchGroup }
```

#### `DeregisterPatchBaselineForPatchGroupResult`

``` purescript
newtype DeregisterPatchBaselineForPatchGroupResult
  = DeregisterPatchBaselineForPatchGroupResult { "BaselineId" :: NullOrUndefined (BaselineId), "PatchGroup" :: NullOrUndefined (PatchGroup) }
```

#### `DeregisterTargetFromMaintenanceWindowRequest`

``` purescript
newtype DeregisterTargetFromMaintenanceWindowRequest
  = DeregisterTargetFromMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "WindowTargetId" :: MaintenanceWindowTargetId, "Safe" :: NullOrUndefined (Boolean) }
```

#### `DeregisterTargetFromMaintenanceWindowResult`

``` purescript
newtype DeregisterTargetFromMaintenanceWindowResult
  = DeregisterTargetFromMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId) }
```

#### `DeregisterTaskFromMaintenanceWindowRequest`

``` purescript
newtype DeregisterTaskFromMaintenanceWindowRequest
  = DeregisterTaskFromMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "WindowTaskId" :: MaintenanceWindowTaskId }
```

#### `DeregisterTaskFromMaintenanceWindowResult`

``` purescript
newtype DeregisterTaskFromMaintenanceWindowResult
  = DeregisterTaskFromMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId) }
```

#### `DescribeActivationsFilter`

``` purescript
newtype DescribeActivationsFilter
  = DescribeActivationsFilter { "FilterKey" :: NullOrUndefined (DescribeActivationsFilterKeys), "FilterValues" :: NullOrUndefined (StringList) }
```

<p>Filter for the DescribeActivation API.</p>

#### `DescribeActivationsFilterKeys`

``` purescript
newtype DescribeActivationsFilterKeys
  = DescribeActivationsFilterKeys String
```

#### `DescribeActivationsFilterList`

``` purescript
newtype DescribeActivationsFilterList
  = DescribeActivationsFilterList (Array DescribeActivationsFilter)
```

#### `DescribeActivationsRequest`

``` purescript
newtype DescribeActivationsRequest
  = DescribeActivationsRequest { "Filters" :: NullOrUndefined (DescribeActivationsFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeActivationsResult`

``` purescript
newtype DescribeActivationsResult
  = DescribeActivationsResult { "ActivationList" :: NullOrUndefined (ActivationList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeAssociationRequest`

``` purescript
newtype DescribeAssociationRequest
  = DescribeAssociationRequest { "Name" :: NullOrUndefined (DocumentName), "InstanceId" :: NullOrUndefined (InstanceId), "AssociationId" :: NullOrUndefined (AssociationId), "AssociationVersion" :: NullOrUndefined (AssociationVersion) }
```

#### `DescribeAssociationResult`

``` purescript
newtype DescribeAssociationResult
  = DescribeAssociationResult { "AssociationDescription" :: NullOrUndefined (AssociationDescription) }
```

#### `DescribeAutomationExecutionsRequest`

``` purescript
newtype DescribeAutomationExecutionsRequest
  = DescribeAutomationExecutionsRequest { "Filters" :: NullOrUndefined (AutomationExecutionFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeAutomationExecutionsResult`

``` purescript
newtype DescribeAutomationExecutionsResult
  = DescribeAutomationExecutionsResult { "AutomationExecutionMetadataList" :: NullOrUndefined (AutomationExecutionMetadataList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeAutomationStepExecutionsRequest`

``` purescript
newtype DescribeAutomationStepExecutionsRequest
  = DescribeAutomationStepExecutionsRequest { "AutomationExecutionId" :: AutomationExecutionId, "Filters" :: NullOrUndefined (StepExecutionFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "ReverseOrder" :: NullOrUndefined (Boolean) }
```

#### `DescribeAutomationStepExecutionsResult`

``` purescript
newtype DescribeAutomationStepExecutionsResult
  = DescribeAutomationStepExecutionsResult { "StepExecutions" :: NullOrUndefined (StepExecutionList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeAvailablePatchesRequest`

``` purescript
newtype DescribeAvailablePatchesRequest
  = DescribeAvailablePatchesRequest { "Filters" :: NullOrUndefined (PatchOrchestratorFilterList), "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeAvailablePatchesResult`

``` purescript
newtype DescribeAvailablePatchesResult
  = DescribeAvailablePatchesResult { "Patches" :: NullOrUndefined (PatchList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeDocumentPermissionRequest`

``` purescript
newtype DescribeDocumentPermissionRequest
  = DescribeDocumentPermissionRequest { "Name" :: DocumentName, "PermissionType" :: DocumentPermissionType }
```

#### `DescribeDocumentPermissionResponse`

``` purescript
newtype DescribeDocumentPermissionResponse
  = DescribeDocumentPermissionResponse { "AccountIds" :: NullOrUndefined (AccountIdList) }
```

#### `DescribeDocumentRequest`

``` purescript
newtype DescribeDocumentRequest
  = DescribeDocumentRequest { "Name" :: DocumentARN, "DocumentVersion" :: NullOrUndefined (DocumentVersion) }
```

#### `DescribeDocumentResult`

``` purescript
newtype DescribeDocumentResult
  = DescribeDocumentResult { "Document" :: NullOrUndefined (DocumentDescription) }
```

#### `DescribeEffectiveInstanceAssociationsRequest`

``` purescript
newtype DescribeEffectiveInstanceAssociationsRequest
  = DescribeEffectiveInstanceAssociationsRequest { "InstanceId" :: InstanceId, "MaxResults" :: NullOrUndefined (EffectiveInstanceAssociationMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeEffectiveInstanceAssociationsResult`

``` purescript
newtype DescribeEffectiveInstanceAssociationsResult
  = DescribeEffectiveInstanceAssociationsResult { "Associations" :: NullOrUndefined (InstanceAssociationList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeEffectivePatchesForPatchBaselineRequest`

``` purescript
newtype DescribeEffectivePatchesForPatchBaselineRequest
  = DescribeEffectivePatchesForPatchBaselineRequest { "BaselineId" :: BaselineId, "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeEffectivePatchesForPatchBaselineResult`

``` purescript
newtype DescribeEffectivePatchesForPatchBaselineResult
  = DescribeEffectivePatchesForPatchBaselineResult { "EffectivePatches" :: NullOrUndefined (EffectivePatchList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeInstanceAssociationsStatusRequest`

``` purescript
newtype DescribeInstanceAssociationsStatusRequest
  = DescribeInstanceAssociationsStatusRequest { "InstanceId" :: InstanceId, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeInstanceAssociationsStatusResult`

``` purescript
newtype DescribeInstanceAssociationsStatusResult
  = DescribeInstanceAssociationsStatusResult { "InstanceAssociationStatusInfos" :: NullOrUndefined (InstanceAssociationStatusInfos), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeInstanceInformationRequest`

``` purescript
newtype DescribeInstanceInformationRequest
  = DescribeInstanceInformationRequest { "InstanceInformationFilterList" :: NullOrUndefined (InstanceInformationFilterList), "Filters" :: NullOrUndefined (InstanceInformationStringFilterList), "MaxResults" :: NullOrUndefined (MaxResultsEC2Compatible), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeInstanceInformationResult`

``` purescript
newtype DescribeInstanceInformationResult
  = DescribeInstanceInformationResult { "InstanceInformationList" :: NullOrUndefined (InstanceInformationList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeInstancePatchStatesForPatchGroupRequest`

``` purescript
newtype DescribeInstancePatchStatesForPatchGroupRequest
  = DescribeInstancePatchStatesForPatchGroupRequest { "PatchGroup" :: PatchGroup, "Filters" :: NullOrUndefined (InstancePatchStateFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults) }
```

#### `DescribeInstancePatchStatesForPatchGroupResult`

``` purescript
newtype DescribeInstancePatchStatesForPatchGroupResult
  = DescribeInstancePatchStatesForPatchGroupResult { "InstancePatchStates" :: NullOrUndefined (InstancePatchStatesList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeInstancePatchStatesRequest`

``` purescript
newtype DescribeInstancePatchStatesRequest
  = DescribeInstancePatchStatesRequest { "InstanceIds" :: InstanceIdList, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults) }
```

#### `DescribeInstancePatchStatesResult`

``` purescript
newtype DescribeInstancePatchStatesResult
  = DescribeInstancePatchStatesResult { "InstancePatchStates" :: NullOrUndefined (InstancePatchStateList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeInstancePatchesRequest`

``` purescript
newtype DescribeInstancePatchesRequest
  = DescribeInstancePatchesRequest { "InstanceId" :: InstanceId, "Filters" :: NullOrUndefined (PatchOrchestratorFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults) }
```

#### `DescribeInstancePatchesResult`

``` purescript
newtype DescribeInstancePatchesResult
  = DescribeInstancePatchesResult { "Patches" :: NullOrUndefined (PatchComplianceDataList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowExecutionTaskInvocationsRequest`

``` purescript
newtype DescribeMaintenanceWindowExecutionTaskInvocationsRequest
  = DescribeMaintenanceWindowExecutionTaskInvocationsRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId, "TaskId" :: MaintenanceWindowExecutionTaskId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowExecutionTaskInvocationsResult`

``` purescript
newtype DescribeMaintenanceWindowExecutionTaskInvocationsResult
  = DescribeMaintenanceWindowExecutionTaskInvocationsResult { "WindowExecutionTaskInvocationIdentities" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationIdentityList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowExecutionTasksRequest`

``` purescript
newtype DescribeMaintenanceWindowExecutionTasksRequest
  = DescribeMaintenanceWindowExecutionTasksRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowExecutionTasksResult`

``` purescript
newtype DescribeMaintenanceWindowExecutionTasksResult
  = DescribeMaintenanceWindowExecutionTasksResult { "WindowExecutionTaskIdentities" :: NullOrUndefined (MaintenanceWindowExecutionTaskIdentityList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowExecutionsRequest`

``` purescript
newtype DescribeMaintenanceWindowExecutionsRequest
  = DescribeMaintenanceWindowExecutionsRequest { "WindowId" :: MaintenanceWindowId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowExecutionsResult`

``` purescript
newtype DescribeMaintenanceWindowExecutionsResult
  = DescribeMaintenanceWindowExecutionsResult { "WindowExecutions" :: NullOrUndefined (MaintenanceWindowExecutionList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowTargetsRequest`

``` purescript
newtype DescribeMaintenanceWindowTargetsRequest
  = DescribeMaintenanceWindowTargetsRequest { "WindowId" :: MaintenanceWindowId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowTargetsResult`

``` purescript
newtype DescribeMaintenanceWindowTargetsResult
  = DescribeMaintenanceWindowTargetsResult { "Targets" :: NullOrUndefined (MaintenanceWindowTargetList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowTasksRequest`

``` purescript
newtype DescribeMaintenanceWindowTasksRequest
  = DescribeMaintenanceWindowTasksRequest { "WindowId" :: MaintenanceWindowId, "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowTasksResult`

``` purescript
newtype DescribeMaintenanceWindowTasksResult
  = DescribeMaintenanceWindowTasksResult { "Tasks" :: NullOrUndefined (MaintenanceWindowTaskList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowsRequest`

``` purescript
newtype DescribeMaintenanceWindowsRequest
  = DescribeMaintenanceWindowsRequest { "Filters" :: NullOrUndefined (MaintenanceWindowFilterList), "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeMaintenanceWindowsResult`

``` purescript
newtype DescribeMaintenanceWindowsResult
  = DescribeMaintenanceWindowsResult { "WindowIdentities" :: NullOrUndefined (MaintenanceWindowIdentityList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeParametersRequest`

``` purescript
newtype DescribeParametersRequest
  = DescribeParametersRequest { "Filters" :: NullOrUndefined (ParametersFilterList), "ParameterFilters" :: NullOrUndefined (ParameterStringFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeParametersResult`

``` purescript
newtype DescribeParametersResult
  = DescribeParametersResult { "Parameters" :: NullOrUndefined (ParameterMetadataList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribePatchBaselinesRequest`

``` purescript
newtype DescribePatchBaselinesRequest
  = DescribePatchBaselinesRequest { "Filters" :: NullOrUndefined (PatchOrchestratorFilterList), "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribePatchBaselinesResult`

``` purescript
newtype DescribePatchBaselinesResult
  = DescribePatchBaselinesResult { "BaselineIdentities" :: NullOrUndefined (PatchBaselineIdentityList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribePatchGroupStateRequest`

``` purescript
newtype DescribePatchGroupStateRequest
  = DescribePatchGroupStateRequest { "PatchGroup" :: PatchGroup }
```

#### `DescribePatchGroupStateResult`

``` purescript
newtype DescribePatchGroupStateResult
  = DescribePatchGroupStateResult { "Instances" :: NullOrUndefined (Int), "InstancesWithInstalledPatches" :: NullOrUndefined (Int), "InstancesWithInstalledOtherPatches" :: NullOrUndefined (Int), "InstancesWithMissingPatches" :: NullOrUndefined (Int), "InstancesWithFailedPatches" :: NullOrUndefined (Int), "InstancesWithNotApplicablePatches" :: NullOrUndefined (Int) }
```

#### `DescribePatchGroupsRequest`

``` purescript
newtype DescribePatchGroupsRequest
  = DescribePatchGroupsRequest { "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults), "Filters" :: NullOrUndefined (PatchOrchestratorFilterList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribePatchGroupsResult`

``` purescript
newtype DescribePatchGroupsResult
  = DescribePatchGroupsResult { "Mappings" :: NullOrUndefined (PatchGroupPatchBaselineMappingList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescriptionInDocument`

``` purescript
newtype DescriptionInDocument
  = DescriptionInDocument String
```

#### `DocumentARN`

``` purescript
newtype DocumentARN
  = DocumentARN String
```

#### `DocumentAlreadyExists`

``` purescript
newtype DocumentAlreadyExists
  = DocumentAlreadyExists { "Message" :: NullOrUndefined (String) }
```

<p>The specified document already exists.</p>

#### `DocumentContent`

``` purescript
newtype DocumentContent
  = DocumentContent String
```

#### `DocumentDefaultVersionDescription`

``` purescript
newtype DocumentDefaultVersionDescription
  = DocumentDefaultVersionDescription { "Name" :: NullOrUndefined (DocumentName), "DefaultVersion" :: NullOrUndefined (DocumentVersion) }
```

<p>A default version of a document.</p>

#### `DocumentDescription`

``` purescript
newtype DocumentDescription
  = DocumentDescription { "Sha1" :: NullOrUndefined (DocumentSha1), "Hash" :: NullOrUndefined (DocumentHash), "HashType" :: NullOrUndefined (DocumentHashType), "Name" :: NullOrUndefined (DocumentARN), "Owner" :: NullOrUndefined (DocumentOwner), "CreatedDate" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (DocumentStatus), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Description" :: NullOrUndefined (DescriptionInDocument), "Parameters" :: NullOrUndefined (DocumentParameterList), "PlatformTypes" :: NullOrUndefined (PlatformTypeList), "DocumentType" :: NullOrUndefined (DocumentType), "SchemaVersion" :: NullOrUndefined (DocumentSchemaVersion), "LatestVersion" :: NullOrUndefined (DocumentVersion), "DefaultVersion" :: NullOrUndefined (DocumentVersion), "DocumentFormat" :: NullOrUndefined (DocumentFormat), "TargetType" :: NullOrUndefined (TargetType), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes a Systems Manager document. </p>

#### `DocumentFilter`

``` purescript
newtype DocumentFilter
  = DocumentFilter { "Key'" :: DocumentFilterKey, "Value'" :: DocumentFilterValue }
```

<p>Describes a filter.</p>

#### `DocumentFilterKey`

``` purescript
newtype DocumentFilterKey
  = DocumentFilterKey String
```

#### `DocumentFilterList`

``` purescript
newtype DocumentFilterList
  = DocumentFilterList (Array DocumentFilter)
```

#### `DocumentFilterValue`

``` purescript
newtype DocumentFilterValue
  = DocumentFilterValue String
```

#### `DocumentFormat`

``` purescript
newtype DocumentFormat
  = DocumentFormat String
```

#### `DocumentHash`

``` purescript
newtype DocumentHash
  = DocumentHash String
```

#### `DocumentHashType`

``` purescript
newtype DocumentHashType
  = DocumentHashType String
```

#### `DocumentIdentifier`

``` purescript
newtype DocumentIdentifier
  = DocumentIdentifier { "Name" :: NullOrUndefined (DocumentARN), "Owner" :: NullOrUndefined (DocumentOwner), "PlatformTypes" :: NullOrUndefined (PlatformTypeList), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "DocumentType" :: NullOrUndefined (DocumentType), "SchemaVersion" :: NullOrUndefined (DocumentSchemaVersion), "DocumentFormat" :: NullOrUndefined (DocumentFormat), "TargetType" :: NullOrUndefined (TargetType), "Tags" :: NullOrUndefined (TagList) }
```

<p>Describes the name of a Systems Manager document.</p>

#### `DocumentIdentifierList`

``` purescript
newtype DocumentIdentifierList
  = DocumentIdentifierList (Array DocumentIdentifier)
```

#### `DocumentKeyValuesFilter`

``` purescript
newtype DocumentKeyValuesFilter
  = DocumentKeyValuesFilter { "Key" :: NullOrUndefined (DocumentKeyValuesFilterKey), "Values" :: NullOrUndefined (DocumentKeyValuesFilterValues) }
```

<p>One or more filters. Use a filter to return a more specific list of documents.</p> <p>For keys, you can specify one or more tags that have been applied to a document. </p> <p>Other valid values include Owner, Name, PlatformTypes, and DocumentType.</p> <p>Note that only one Owner can be specified in a request. For example: <code>Key=Owner,Values=Self</code>.</p> <p>If you use Name as a key, you can use a name prefix to return a list of documents. For example, in the AWS CLI, to return a list of all documents that begin with <code>Te</code>, run the following command:</p> <p> <code>aws ssm list-documents --filters Key=Name,Values=Te</code> </p> <p>If you specify more than two keys, only documents that are identified by all the tags are returned in the results. If you specify more than two values for a key, documents that are identified by any of the values are returned in the results.</p> <p>To specify a custom key and value pair, use the format <code>Key=tag:[tagName],Values=[valueName]</code>.</p> <p>For example, if you created a Key called region and are using the AWS CLI to call the <code>list-documents</code> command: </p> <p> <code>aws ssm list-documents --filters Key=tag:region,Values=east,west Key=Owner,Values=Self</code> </p>

#### `DocumentKeyValuesFilterKey`

``` purescript
newtype DocumentKeyValuesFilterKey
  = DocumentKeyValuesFilterKey String
```

#### `DocumentKeyValuesFilterList`

``` purescript
newtype DocumentKeyValuesFilterList
  = DocumentKeyValuesFilterList (Array DocumentKeyValuesFilter)
```

#### `DocumentKeyValuesFilterValue`

``` purescript
newtype DocumentKeyValuesFilterValue
  = DocumentKeyValuesFilterValue String
```

#### `DocumentKeyValuesFilterValues`

``` purescript
newtype DocumentKeyValuesFilterValues
  = DocumentKeyValuesFilterValues (Array DocumentKeyValuesFilterValue)
```

#### `DocumentLimitExceeded`

``` purescript
newtype DocumentLimitExceeded
  = DocumentLimitExceeded { "Message" :: NullOrUndefined (String) }
```

<p>You can have at most 200 active Systems Manager documents.</p>

#### `DocumentName`

``` purescript
newtype DocumentName
  = DocumentName String
```

#### `DocumentOwner`

``` purescript
newtype DocumentOwner
  = DocumentOwner String
```

#### `DocumentParameter`

``` purescript
newtype DocumentParameter
  = DocumentParameter { "Name" :: NullOrUndefined (DocumentParameterName), "Type" :: NullOrUndefined (DocumentParameterType), "Description" :: NullOrUndefined (DocumentParameterDescrption), "DefaultValue" :: NullOrUndefined (DocumentParameterDefaultValue) }
```

<p>Parameters specified in a System Manager document that execute on the server when the command is run. </p>

#### `DocumentParameterDefaultValue`

``` purescript
newtype DocumentParameterDefaultValue
  = DocumentParameterDefaultValue String
```

#### `DocumentParameterDescrption`

``` purescript
newtype DocumentParameterDescrption
  = DocumentParameterDescrption String
```

#### `DocumentParameterList`

``` purescript
newtype DocumentParameterList
  = DocumentParameterList (Array DocumentParameter)
```

#### `DocumentParameterName`

``` purescript
newtype DocumentParameterName
  = DocumentParameterName String
```

#### `DocumentParameterType`

``` purescript
newtype DocumentParameterType
  = DocumentParameterType String
```

#### `DocumentPermissionLimit`

``` purescript
newtype DocumentPermissionLimit
  = DocumentPermissionLimit { "Message" :: NullOrUndefined (String) }
```

<p>The document cannot be shared with more AWS user accounts. You can share a document with a maximum of 20 accounts. You can publicly share up to five documents. If you need to increase this limit, contact AWS Support.</p>

#### `DocumentPermissionType`

``` purescript
newtype DocumentPermissionType
  = DocumentPermissionType String
```

#### `DocumentSchemaVersion`

``` purescript
newtype DocumentSchemaVersion
  = DocumentSchemaVersion String
```

#### `DocumentSha1`

``` purescript
newtype DocumentSha1
  = DocumentSha1 String
```

#### `DocumentStatus`

``` purescript
newtype DocumentStatus
  = DocumentStatus String
```

#### `DocumentType`

``` purescript
newtype DocumentType
  = DocumentType String
```

#### `DocumentVersion`

``` purescript
newtype DocumentVersion
  = DocumentVersion String
```

#### `DocumentVersionInfo`

``` purescript
newtype DocumentVersionInfo
  = DocumentVersionInfo { "Name" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "CreatedDate" :: NullOrUndefined (DateTime), "IsDefaultVersion" :: NullOrUndefined (Boolean), "DocumentFormat" :: NullOrUndefined (DocumentFormat) }
```

<p>Version information about the document.</p>

#### `DocumentVersionLimitExceeded`

``` purescript
newtype DocumentVersionLimitExceeded
  = DocumentVersionLimitExceeded { "Message" :: NullOrUndefined (String) }
```

<p>The document has too many versions. Delete one or more document versions and try again.</p>

#### `DocumentVersionList`

``` purescript
newtype DocumentVersionList
  = DocumentVersionList (Array DocumentVersionInfo)
```

#### `DocumentVersionNumber`

``` purescript
newtype DocumentVersionNumber
  = DocumentVersionNumber String
```

#### `DoesNotExistException`

``` purescript
newtype DoesNotExistException
  = DoesNotExistException { "Message" :: NullOrUndefined (String) }
```

<p>Error returned when the ID specified for a resource, such as a Maintenance Window or Patch baseline, doesn't exist.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>

#### `DuplicateDocumentContent`

``` purescript
newtype DuplicateDocumentContent
  = DuplicateDocumentContent { "Message" :: NullOrUndefined (String) }
```

<p>The content of the association document matches another document. Change the content of the document and try again.</p>

#### `DuplicateInstanceId`

``` purescript
newtype DuplicateInstanceId
  = DuplicateInstanceId {  }
```

<p>You cannot specify an instance ID in more than one association.</p>

#### `EffectiveInstanceAssociationMaxResults`

``` purescript
newtype EffectiveInstanceAssociationMaxResults
  = EffectiveInstanceAssociationMaxResults Int
```

#### `EffectivePatch`

``` purescript
newtype EffectivePatch
  = EffectivePatch { "Patch" :: NullOrUndefined (Patch), "PatchStatus" :: NullOrUndefined (PatchStatus) }
```

<p>The EffectivePatch structure defines metadata about a patch along with the approval state of the patch in a particular patch baseline. The approval state includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.</p>

#### `EffectivePatchList`

``` purescript
newtype EffectivePatchList
  = EffectivePatchList (Array EffectivePatch)
```

#### `ErrorCount`

``` purescript
newtype ErrorCount
  = ErrorCount Int
```

#### `ExecutionMode`

``` purescript
newtype ExecutionMode
  = ExecutionMode String
```

#### `ExpirationDate`

``` purescript
newtype ExpirationDate
  = ExpirationDate Number
```

#### `FailedCreateAssociation`

``` purescript
newtype FailedCreateAssociation
  = FailedCreateAssociation { "Entry" :: NullOrUndefined (CreateAssociationBatchRequestEntry), "Message" :: NullOrUndefined (BatchErrorMessage), "Fault" :: NullOrUndefined (Fault) }
```

<p>Describes a failed association.</p>

#### `FailedCreateAssociationList`

``` purescript
newtype FailedCreateAssociationList
  = FailedCreateAssociationList (Array FailedCreateAssociation)
```

#### `FailureDetails`

``` purescript
newtype FailureDetails
  = FailureDetails { "FailureStage" :: NullOrUndefined (String), "FailureType" :: NullOrUndefined (String), "Details" :: NullOrUndefined (AutomationParameterMap) }
```

<p>Information about an Automation failure.</p>

#### `Fault`

``` purescript
newtype Fault
  = Fault String
```

#### `FeatureNotAvailableException`

``` purescript
newtype FeatureNotAvailableException
  = FeatureNotAvailableException { "Message" :: NullOrUndefined (String) }
```

<p>You attempted to register a LAMBDA or STEP_FUNCTION task in a region where the corresponding service is not available. </p>

#### `GetAutomationExecutionRequest`

``` purescript
newtype GetAutomationExecutionRequest
  = GetAutomationExecutionRequest { "AutomationExecutionId" :: AutomationExecutionId }
```

#### `GetAutomationExecutionResult`

``` purescript
newtype GetAutomationExecutionResult
  = GetAutomationExecutionResult { "AutomationExecution" :: NullOrUndefined (AutomationExecution) }
```

#### `GetCommandInvocationRequest`

``` purescript
newtype GetCommandInvocationRequest
  = GetCommandInvocationRequest { "CommandId" :: CommandId, "InstanceId" :: InstanceId, "PluginName" :: NullOrUndefined (CommandPluginName) }
```

#### `GetCommandInvocationResult`

``` purescript
newtype GetCommandInvocationResult
  = GetCommandInvocationResult { "CommandId" :: NullOrUndefined (CommandId), "InstanceId" :: NullOrUndefined (InstanceId), "Comment" :: NullOrUndefined (Comment), "DocumentName" :: NullOrUndefined (DocumentName), "PluginName" :: NullOrUndefined (CommandPluginName), "ResponseCode" :: NullOrUndefined (ResponseCode), "ExecutionStartDateTime" :: NullOrUndefined (StringDateTime), "ExecutionElapsedTime" :: NullOrUndefined (StringDateTime), "ExecutionEndDateTime" :: NullOrUndefined (StringDateTime), "Status" :: NullOrUndefined (CommandInvocationStatus), "StatusDetails" :: NullOrUndefined (StatusDetails), "StandardOutputContent" :: NullOrUndefined (StandardOutputContent), "StandardOutputUrl" :: NullOrUndefined (Url), "StandardErrorContent" :: NullOrUndefined (StandardErrorContent), "StandardErrorUrl" :: NullOrUndefined (Url) }
```

#### `GetDefaultPatchBaselineRequest`

``` purescript
newtype GetDefaultPatchBaselineRequest
  = GetDefaultPatchBaselineRequest { "OperatingSystem" :: NullOrUndefined (OperatingSystem) }
```

#### `GetDefaultPatchBaselineResult`

``` purescript
newtype GetDefaultPatchBaselineResult
  = GetDefaultPatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId), "OperatingSystem" :: NullOrUndefined (OperatingSystem) }
```

#### `GetDeployablePatchSnapshotForInstanceRequest`

``` purescript
newtype GetDeployablePatchSnapshotForInstanceRequest
  = GetDeployablePatchSnapshotForInstanceRequest { "InstanceId" :: InstanceId, "SnapshotId" :: SnapshotId }
```

#### `GetDeployablePatchSnapshotForInstanceResult`

``` purescript
newtype GetDeployablePatchSnapshotForInstanceResult
  = GetDeployablePatchSnapshotForInstanceResult { "InstanceId" :: NullOrUndefined (InstanceId), "SnapshotId" :: NullOrUndefined (SnapshotId), "SnapshotDownloadUrl" :: NullOrUndefined (SnapshotDownloadUrl), "Product" :: NullOrUndefined (Product) }
```

#### `GetDocumentRequest`

``` purescript
newtype GetDocumentRequest
  = GetDocumentRequest { "Name" :: DocumentARN, "DocumentVersion" :: NullOrUndefined (DocumentVersion), "DocumentFormat" :: NullOrUndefined (DocumentFormat) }
```

#### `GetDocumentResult`

``` purescript
newtype GetDocumentResult
  = GetDocumentResult { "Name" :: NullOrUndefined (DocumentARN), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Content" :: NullOrUndefined (DocumentContent), "DocumentType" :: NullOrUndefined (DocumentType), "DocumentFormat" :: NullOrUndefined (DocumentFormat) }
```

#### `GetInventoryRequest`

``` purescript
newtype GetInventoryRequest
  = GetInventoryRequest { "Filters" :: NullOrUndefined (InventoryFilterList), "Aggregators" :: NullOrUndefined (InventoryAggregatorList), "ResultAttributes" :: NullOrUndefined (ResultAttributeList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `GetInventoryResult`

``` purescript
newtype GetInventoryResult
  = GetInventoryResult { "Entities" :: NullOrUndefined (InventoryResultEntityList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetInventorySchemaMaxResults`

``` purescript
newtype GetInventorySchemaMaxResults
  = GetInventorySchemaMaxResults Int
```

#### `GetInventorySchemaRequest`

``` purescript
newtype GetInventorySchemaRequest
  = GetInventorySchemaRequest { "TypeName" :: NullOrUndefined (InventoryItemTypeNameFilter), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (GetInventorySchemaMaxResults), "Aggregator" :: NullOrUndefined (AggregatorSchemaOnly), "SubType" :: NullOrUndefined (IsSubTypeSchema) }
```

#### `GetInventorySchemaResult`

``` purescript
newtype GetInventorySchemaResult
  = GetInventorySchemaResult { "Schemas" :: NullOrUndefined (InventoryItemSchemaResultList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetMaintenanceWindowExecutionRequest`

``` purescript
newtype GetMaintenanceWindowExecutionRequest
  = GetMaintenanceWindowExecutionRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId }
```

#### `GetMaintenanceWindowExecutionResult`

``` purescript
newtype GetMaintenanceWindowExecutionResult
  = GetMaintenanceWindowExecutionResult { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskIds" :: NullOrUndefined (MaintenanceWindowExecutionTaskIdList), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime) }
```

#### `GetMaintenanceWindowExecutionTaskInvocationRequest`

``` purescript
newtype GetMaintenanceWindowExecutionTaskInvocationRequest
  = GetMaintenanceWindowExecutionTaskInvocationRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId, "TaskId" :: MaintenanceWindowExecutionTaskId, "InvocationId" :: MaintenanceWindowExecutionTaskInvocationId }
```

#### `GetMaintenanceWindowExecutionTaskInvocationResult`

``` purescript
newtype GetMaintenanceWindowExecutionTaskInvocationResult
  = GetMaintenanceWindowExecutionTaskInvocationResult { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId), "InvocationId" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationId), "ExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskExecutionId), "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType), "Parameters" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationParameters), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTaskTargetId) }
```

#### `GetMaintenanceWindowExecutionTaskRequest`

``` purescript
newtype GetMaintenanceWindowExecutionTaskRequest
  = GetMaintenanceWindowExecutionTaskRequest { "WindowExecutionId" :: MaintenanceWindowExecutionId, "TaskId" :: MaintenanceWindowExecutionTaskId }
```

#### `GetMaintenanceWindowExecutionTaskResult`

``` purescript
newtype GetMaintenanceWindowExecutionTaskResult
  = GetMaintenanceWindowExecutionTaskResult { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "ServiceRole" :: NullOrUndefined (ServiceRole), "Type" :: NullOrUndefined (MaintenanceWindowTaskType), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParametersList), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime) }
```

#### `GetMaintenanceWindowRequest`

``` purescript
newtype GetMaintenanceWindowRequest
  = GetMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId }
```

#### `GetMaintenanceWindowResult`

``` purescript
newtype GetMaintenanceWindowResult
  = GetMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Schedule" :: NullOrUndefined (MaintenanceWindowSchedule), "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours), "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff), "AllowUnassociatedTargets" :: NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets), "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled), "CreatedDate" :: NullOrUndefined (DateTime), "ModifiedDate" :: NullOrUndefined (DateTime) }
```

#### `GetMaintenanceWindowTaskRequest`

``` purescript
newtype GetMaintenanceWindowTaskRequest
  = GetMaintenanceWindowTaskRequest { "WindowId" :: MaintenanceWindowId, "WindowTaskId" :: MaintenanceWindowTaskId }
```

#### `GetMaintenanceWindowTaskResult`

``` purescript
newtype GetMaintenanceWindowTaskResult
  = GetMaintenanceWindowTaskResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId), "Targets" :: NullOrUndefined (Targets), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "LoggingInfo" :: NullOrUndefined (LoggingInfo), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

#### `GetParameterHistoryRequest`

``` purescript
newtype GetParameterHistoryRequest
  = GetParameterHistoryRequest { "Name" :: PSParameterName, "WithDecryption" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetParameterHistoryResult`

``` purescript
newtype GetParameterHistoryResult
  = GetParameterHistoryResult { "Parameters" :: NullOrUndefined (ParameterHistoryList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetParameterRequest`

``` purescript
newtype GetParameterRequest
  = GetParameterRequest { "Name" :: PSParameterName, "WithDecryption" :: NullOrUndefined (Boolean) }
```

#### `GetParameterResult`

``` purescript
newtype GetParameterResult
  = GetParameterResult { "Parameter" :: NullOrUndefined (Parameter) }
```

#### `GetParametersByPathMaxResults`

``` purescript
newtype GetParametersByPathMaxResults
  = GetParametersByPathMaxResults Int
```

#### `GetParametersByPathRequest`

``` purescript
newtype GetParametersByPathRequest
  = GetParametersByPathRequest { "Path" :: PSParameterName, "Recursive" :: NullOrUndefined (Boolean), "ParameterFilters" :: NullOrUndefined (ParameterStringFilterList), "WithDecryption" :: NullOrUndefined (Boolean), "MaxResults" :: NullOrUndefined (GetParametersByPathMaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetParametersByPathResult`

``` purescript
newtype GetParametersByPathResult
  = GetParametersByPathResult { "Parameters" :: NullOrUndefined (ParameterList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetParametersRequest`

``` purescript
newtype GetParametersRequest
  = GetParametersRequest { "Names" :: ParameterNameList, "WithDecryption" :: NullOrUndefined (Boolean) }
```

#### `GetParametersResult`

``` purescript
newtype GetParametersResult
  = GetParametersResult { "Parameters" :: NullOrUndefined (ParameterList), "InvalidParameters" :: NullOrUndefined (ParameterNameList) }
```

#### `GetPatchBaselineForPatchGroupRequest`

``` purescript
newtype GetPatchBaselineForPatchGroupRequest
  = GetPatchBaselineForPatchGroupRequest { "PatchGroup" :: PatchGroup, "OperatingSystem" :: NullOrUndefined (OperatingSystem) }
```

#### `GetPatchBaselineForPatchGroupResult`

``` purescript
newtype GetPatchBaselineForPatchGroupResult
  = GetPatchBaselineForPatchGroupResult { "BaselineId" :: NullOrUndefined (BaselineId), "PatchGroup" :: NullOrUndefined (PatchGroup), "OperatingSystem" :: NullOrUndefined (OperatingSystem) }
```

#### `GetPatchBaselineRequest`

``` purescript
newtype GetPatchBaselineRequest
  = GetPatchBaselineRequest { "BaselineId" :: BaselineId }
```

#### `GetPatchBaselineResult`

``` purescript
newtype GetPatchBaselineResult
  = GetPatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId), "Name" :: NullOrUndefined (BaselineName), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "GlobalFilters" :: NullOrUndefined (PatchFilterGroup), "ApprovalRules" :: NullOrUndefined (PatchRuleGroup), "ApprovedPatches" :: NullOrUndefined (PatchIdList), "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean), "RejectedPatches" :: NullOrUndefined (PatchIdList), "PatchGroups" :: NullOrUndefined (PatchGroupList), "CreatedDate" :: NullOrUndefined (DateTime), "ModifiedDate" :: NullOrUndefined (DateTime), "Description" :: NullOrUndefined (BaselineDescription), "Sources" :: NullOrUndefined (PatchSourceList) }
```

#### `HierarchyLevelLimitExceededException`

``` purescript
newtype HierarchyLevelLimitExceededException
  = HierarchyLevelLimitExceededException { "Message'" :: NullOrUndefined (String) }
```

<p>A hierarchy can have a maximum of 15 levels. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html">Working with Systems Manager Parameters</a>. </p>

#### `HierarchyTypeMismatchException`

``` purescript
newtype HierarchyTypeMismatchException
  = HierarchyTypeMismatchException { "Message'" :: NullOrUndefined (String) }
```

<p>Parameter Store does not support changing a parameter type in a hierarchy. For example, you can't change a parameter from a String type to a SecureString type. You must create a new, unique parameter.</p>

#### `IPAddress`

``` purescript
newtype IPAddress
  = IPAddress String
```

#### `IamRole`

``` purescript
newtype IamRole
  = IamRole String
```

#### `IdempotencyToken`

``` purescript
newtype IdempotencyToken
  = IdempotencyToken String
```

#### `IdempotentParameterMismatch`

``` purescript
newtype IdempotentParameterMismatch
  = IdempotentParameterMismatch { "Message" :: NullOrUndefined (String) }
```

<p>Error returned when an idempotent operation is retried and the parameters don't match the original call to the API with the same idempotency token. </p>

#### `InstanceAggregatedAssociationOverview`

``` purescript
newtype InstanceAggregatedAssociationOverview
  = InstanceAggregatedAssociationOverview { "DetailedStatus" :: NullOrUndefined (StatusName), "InstanceAssociationStatusAggregatedCount" :: NullOrUndefined (InstanceAssociationStatusAggregatedCount) }
```

<p>Status information about the aggregated associations.</p>

#### `InstanceAssociation`

``` purescript
newtype InstanceAssociation
  = InstanceAssociation { "AssociationId" :: NullOrUndefined (AssociationId), "InstanceId" :: NullOrUndefined (InstanceId), "Content" :: NullOrUndefined (DocumentContent), "AssociationVersion" :: NullOrUndefined (AssociationVersion) }
```

<p>One or more association documents on the instance. </p>

#### `InstanceAssociationExecutionSummary`

``` purescript
newtype InstanceAssociationExecutionSummary
  = InstanceAssociationExecutionSummary String
```

#### `InstanceAssociationList`

``` purescript
newtype InstanceAssociationList
  = InstanceAssociationList (Array InstanceAssociation)
```

#### `InstanceAssociationOutputLocation`

``` purescript
newtype InstanceAssociationOutputLocation
  = InstanceAssociationOutputLocation { "S3Location" :: NullOrUndefined (S3OutputLocation) }
```

<p>An Amazon S3 bucket where you want to store the results of this request.</p>

#### `InstanceAssociationOutputUrl`

``` purescript
newtype InstanceAssociationOutputUrl
  = InstanceAssociationOutputUrl { "S3OutputUrl" :: NullOrUndefined (S3OutputUrl) }
```

<p>The URL of Amazon S3 bucket where you want to store the results of this request.</p>

#### `InstanceAssociationStatusAggregatedCount`

``` purescript
newtype InstanceAssociationStatusAggregatedCount
  = InstanceAssociationStatusAggregatedCount (Map StatusName InstanceCount)
```

#### `InstanceAssociationStatusInfo`

``` purescript
newtype InstanceAssociationStatusInfo
  = InstanceAssociationStatusInfo { "AssociationId" :: NullOrUndefined (AssociationId), "Name" :: NullOrUndefined (DocumentName), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "AssociationVersion" :: NullOrUndefined (AssociationVersion), "InstanceId" :: NullOrUndefined (InstanceId), "ExecutionDate" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (StatusName), "DetailedStatus" :: NullOrUndefined (StatusName), "ExecutionSummary" :: NullOrUndefined (InstanceAssociationExecutionSummary), "ErrorCode" :: NullOrUndefined (AgentErrorCode), "OutputUrl" :: NullOrUndefined (InstanceAssociationOutputUrl), "AssociationName" :: NullOrUndefined (AssociationName) }
```

<p>Status information about the instance association.</p>

#### `InstanceAssociationStatusInfos`

``` purescript
newtype InstanceAssociationStatusInfos
  = InstanceAssociationStatusInfos (Array InstanceAssociationStatusInfo)
```

#### `InstanceCount`

``` purescript
newtype InstanceCount
  = InstanceCount Int
```

#### `InstanceId`

``` purescript
newtype InstanceId
  = InstanceId String
```

#### `InstanceIdList`

``` purescript
newtype InstanceIdList
  = InstanceIdList (Array InstanceId)
```

#### `InstanceInformation`

``` purescript
newtype InstanceInformation
  = InstanceInformation { "InstanceId" :: NullOrUndefined (InstanceId), "PingStatus" :: NullOrUndefined (PingStatus), "LastPingDateTime" :: NullOrUndefined (DateTime), "AgentVersion" :: NullOrUndefined (Version), "IsLatestVersion" :: NullOrUndefined (Boolean), "PlatformType" :: NullOrUndefined (PlatformType), "PlatformName" :: NullOrUndefined (String), "PlatformVersion" :: NullOrUndefined (String), "ActivationId" :: NullOrUndefined (ActivationId), "IamRole" :: NullOrUndefined (IamRole), "RegistrationDate" :: NullOrUndefined (DateTime), "ResourceType" :: NullOrUndefined (ResourceType), "Name" :: NullOrUndefined (String), "IPAddress" :: NullOrUndefined (IPAddress), "ComputerName" :: NullOrUndefined (ComputerName), "AssociationStatus" :: NullOrUndefined (StatusName), "LastAssociationExecutionDate" :: NullOrUndefined (DateTime), "LastSuccessfulAssociationExecutionDate" :: NullOrUndefined (DateTime), "AssociationOverview" :: NullOrUndefined (InstanceAggregatedAssociationOverview) }
```

<p>Describes a filter for a specific list of instances. </p>

#### `InstanceInformationFilter`

``` purescript
newtype InstanceInformationFilter
  = InstanceInformationFilter { "Key'" :: InstanceInformationFilterKey, "ValueSet'" :: InstanceInformationFilterValueSet }
```

<p>Describes a filter for a specific list of instances. </p>

#### `InstanceInformationFilterKey`

``` purescript
newtype InstanceInformationFilterKey
  = InstanceInformationFilterKey String
```

#### `InstanceInformationFilterList`

``` purescript
newtype InstanceInformationFilterList
  = InstanceInformationFilterList (Array InstanceInformationFilter)
```

#### `InstanceInformationFilterValue`

``` purescript
newtype InstanceInformationFilterValue
  = InstanceInformationFilterValue String
```

#### `InstanceInformationFilterValueSet`

``` purescript
newtype InstanceInformationFilterValueSet
  = InstanceInformationFilterValueSet (Array InstanceInformationFilterValue)
```

#### `InstanceInformationList`

``` purescript
newtype InstanceInformationList
  = InstanceInformationList (Array InstanceInformation)
```

#### `InstanceInformationStringFilter`

``` purescript
newtype InstanceInformationStringFilter
  = InstanceInformationStringFilter { "Key" :: InstanceInformationStringFilterKey, "Values" :: InstanceInformationFilterValueSet }
```

<p>The filters to describe or get information about your managed instances.</p>

#### `InstanceInformationStringFilterKey`

``` purescript
newtype InstanceInformationStringFilterKey
  = InstanceInformationStringFilterKey String
```

#### `InstanceInformationStringFilterList`

``` purescript
newtype InstanceInformationStringFilterList
  = InstanceInformationStringFilterList (Array InstanceInformationStringFilter)
```

#### `InstancePatchState`

``` purescript
newtype InstancePatchState
  = InstancePatchState { "InstanceId" :: InstanceId, "PatchGroup" :: PatchGroup, "BaselineId" :: BaselineId, "SnapshotId" :: NullOrUndefined (SnapshotId), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "InstalledCount" :: NullOrUndefined (PatchInstalledCount), "InstalledOtherCount" :: NullOrUndefined (PatchInstalledOtherCount), "MissingCount" :: NullOrUndefined (PatchMissingCount), "FailedCount" :: NullOrUndefined (PatchFailedCount), "NotApplicableCount" :: NullOrUndefined (PatchNotApplicableCount), "OperationStartTime" :: DateTime, "OperationEndTime" :: DateTime, "Operation" :: PatchOperationType }
```

<p>Defines the high-level patch compliance state for a managed instance, providing information about the number of installed, missing, not applicable, and failed patches along with metadata about the operation when this information was gathered for the instance.</p>

#### `InstancePatchStateFilter`

``` purescript
newtype InstancePatchStateFilter
  = InstancePatchStateFilter { "Key" :: InstancePatchStateFilterKey, "Values" :: InstancePatchStateFilterValues, "Type" :: InstancePatchStateOperatorType }
```

<p>Defines a filter used in DescribeInstancePatchStatesForPatchGroup used to scope down the information returned by the API.</p>

#### `InstancePatchStateFilterKey`

``` purescript
newtype InstancePatchStateFilterKey
  = InstancePatchStateFilterKey String
```

#### `InstancePatchStateFilterList`

``` purescript
newtype InstancePatchStateFilterList
  = InstancePatchStateFilterList (Array InstancePatchStateFilter)
```

#### `InstancePatchStateFilterValue`

``` purescript
newtype InstancePatchStateFilterValue
  = InstancePatchStateFilterValue String
```

#### `InstancePatchStateFilterValues`

``` purescript
newtype InstancePatchStateFilterValues
  = InstancePatchStateFilterValues (Array InstancePatchStateFilterValue)
```

#### `InstancePatchStateList`

``` purescript
newtype InstancePatchStateList
  = InstancePatchStateList (Array InstancePatchState)
```

#### `InstancePatchStateOperatorType`

``` purescript
newtype InstancePatchStateOperatorType
  = InstancePatchStateOperatorType String
```

#### `InstancePatchStatesList`

``` purescript
newtype InstancePatchStatesList
  = InstancePatchStatesList (Array InstancePatchState)
```

#### `InstanceTagName`

``` purescript
newtype InstanceTagName
  = InstanceTagName String
```

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message" :: NullOrUndefined (String) }
```

<p>An error occurred on the server side.</p>

#### `InvalidActivation`

``` purescript
newtype InvalidActivation
  = InvalidActivation { "Message" :: NullOrUndefined (String) }
```

<p>The activation is not valid. The activation might have been deleted, or the ActivationId and the ActivationCode do not match.</p>

#### `InvalidActivationId`

``` purescript
newtype InvalidActivationId
  = InvalidActivationId { "Message" :: NullOrUndefined (String) }
```

<p>The activation ID is not valid. Verify the you entered the correct ActivationId or ActivationCode and try again.</p>

#### `InvalidAllowedPatternException`

``` purescript
newtype InvalidAllowedPatternException
  = InvalidAllowedPatternException { "Message'" :: NullOrUndefined (String) }
```

<p>The request does not meet the regular expression requirement.</p>

#### `InvalidAssociationVersion`

``` purescript
newtype InvalidAssociationVersion
  = InvalidAssociationVersion { "Message" :: NullOrUndefined (String) }
```

<p>The version you specified is not valid. Use ListAssociationVersions to view all versions of an association according to the association ID. Or, use the <code>$LATEST</code> parameter to view the latest version of the association.</p>

#### `InvalidAutomationExecutionParametersException`

``` purescript
newtype InvalidAutomationExecutionParametersException
  = InvalidAutomationExecutionParametersException { "Message" :: NullOrUndefined (String) }
```

<p>The supplied parameters for invoking the specified Automation document are incorrect. For example, they may not match the set of parameters permitted for the specified Automation document.</p>

#### `InvalidAutomationSignalException`

``` purescript
newtype InvalidAutomationSignalException
  = InvalidAutomationSignalException { "Message" :: NullOrUndefined (String) }
```

<p>The signal is not valid for the current Automation execution.</p>

#### `InvalidAutomationStatusUpdateException`

``` purescript
newtype InvalidAutomationStatusUpdateException
  = InvalidAutomationStatusUpdateException { "Message" :: NullOrUndefined (String) }
```

<p>The specified update status operation is not valid.</p>

#### `InvalidCommandId`

``` purescript
newtype InvalidCommandId
  = InvalidCommandId {  }
```

#### `InvalidDocument`

``` purescript
newtype InvalidDocument
  = InvalidDocument { "Message" :: NullOrUndefined (String) }
```

<p>The specified document does not exist.</p>

#### `InvalidDocumentContent`

``` purescript
newtype InvalidDocumentContent
  = InvalidDocumentContent { "Message" :: NullOrUndefined (String) }
```

<p>The content for the document is not valid.</p>

#### `InvalidDocumentOperation`

``` purescript
newtype InvalidDocumentOperation
  = InvalidDocumentOperation { "Message" :: NullOrUndefined (String) }
```

<p>You attempted to delete a document while it is still shared. You must stop sharing the document before you can delete it.</p>

#### `InvalidDocumentSchemaVersion`

``` purescript
newtype InvalidDocumentSchemaVersion
  = InvalidDocumentSchemaVersion { "Message" :: NullOrUndefined (String) }
```

<p>The version of the document schema is not supported.</p>

#### `InvalidDocumentVersion`

``` purescript
newtype InvalidDocumentVersion
  = InvalidDocumentVersion { "Message" :: NullOrUndefined (String) }
```

<p>The document version is not valid or does not exist.</p>

#### `InvalidFilter`

``` purescript
newtype InvalidFilter
  = InvalidFilter { "Message" :: NullOrUndefined (String) }
```

<p>The filter name is not valid. Verify the you entered the correct name and try again.</p>

#### `InvalidFilterKey`

``` purescript
newtype InvalidFilterKey
  = InvalidFilterKey {  }
```

<p>The specified key is not valid.</p>

#### `InvalidFilterOption`

``` purescript
newtype InvalidFilterOption
  = InvalidFilterOption { "Message'" :: NullOrUndefined (String) }
```

<p>The specified filter option is not valid. Valid options are Equals and BeginsWith. For Path filter, valid options are Recursive and OneLevel.</p>

#### `InvalidFilterValue`

``` purescript
newtype InvalidFilterValue
  = InvalidFilterValue { "Message" :: NullOrUndefined (String) }
```

<p>The filter value is not valid. Verify the value and try again.</p>

#### `InvalidInstanceId`

``` purescript
newtype InvalidInstanceId
  = InvalidInstanceId { "Message" :: NullOrUndefined (String) }
```

<p>The following problems can cause this exception:</p> <p>You do not have permission to access the instance.</p> <p>The SSM Agent is not running. On managed instances and Linux instances, verify that the SSM Agent is running. On EC2 Windows instances, verify that the EC2Config service is running.</p> <p>The SSM Agent or EC2Config service is not registered to the SSM endpoint. Try reinstalling the SSM Agent or EC2Config service.</p> <p>The instance is not in valid state. Valid states are: Running, Pending, Stopped, Stopping. Invalid states are: Shutting-down and Terminated.</p>

#### `InvalidInstanceInformationFilterValue`

``` purescript
newtype InvalidInstanceInformationFilterValue
  = InvalidInstanceInformationFilterValue { "Message'" :: NullOrUndefined (String) }
```

<p>The specified filter value is not valid.</p>

#### `InvalidInventoryItemContextException`

``` purescript
newtype InvalidInventoryItemContextException
  = InvalidInventoryItemContextException { "Message" :: NullOrUndefined (String) }
```

<p>You specified invalid keys or values in the <code>Context</code> attribute for <code>InventoryItem</code>. Verify the keys and values, and try again.</p>

#### `InvalidItemContentException`

``` purescript
newtype InvalidItemContentException
  = InvalidItemContentException { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "Message" :: NullOrUndefined (String) }
```

<p>One or more content items is not valid.</p>

#### `InvalidKeyId`

``` purescript
newtype InvalidKeyId
  = InvalidKeyId { "Message'" :: NullOrUndefined (String) }
```

<p>The query key ID is not valid.</p>

#### `InvalidNextToken`

``` purescript
newtype InvalidNextToken
  = InvalidNextToken { "Message" :: NullOrUndefined (String) }
```

<p>The specified token is not valid.</p>

#### `InvalidNotificationConfig`

``` purescript
newtype InvalidNotificationConfig
  = InvalidNotificationConfig { "Message" :: NullOrUndefined (String) }
```

<p>One or more configuration items is not valid. Verify that a valid Amazon Resource Name (ARN) was provided for an Amazon SNS topic.</p>

#### `InvalidOutputFolder`

``` purescript
newtype InvalidOutputFolder
  = InvalidOutputFolder {  }
```

<p>The S3 bucket does not exist.</p>

#### `InvalidOutputLocation`

``` purescript
newtype InvalidOutputLocation
  = InvalidOutputLocation {  }
```

<p>The output location is not valid or does not exist.</p>

#### `InvalidParameters`

``` purescript
newtype InvalidParameters
  = InvalidParameters { "Message" :: NullOrUndefined (String) }
```

<p>You must specify values for all required parameters in the Systems Manager document. You can only supply values to parameters defined in the Systems Manager document.</p>

#### `InvalidPermissionType`

``` purescript
newtype InvalidPermissionType
  = InvalidPermissionType { "Message" :: NullOrUndefined (String) }
```

<p>The permission type is not supported. <i>Share</i> is the only supported permission type.</p>

#### `InvalidPluginName`

``` purescript
newtype InvalidPluginName
  = InvalidPluginName {  }
```

<p>The plugin name is not valid.</p>

#### `InvalidResourceId`

``` purescript
newtype InvalidResourceId
  = InvalidResourceId {  }
```

<p>The resource ID is not valid. Verify that you entered the correct ID and try again.</p>

#### `InvalidResourceType`

``` purescript
newtype InvalidResourceType
  = InvalidResourceType {  }
```

<p>The resource type is not valid. For example, if you are attempting to tag an instance, the instance must be a registered, managed instance.</p>

#### `InvalidResultAttributeException`

``` purescript
newtype InvalidResultAttributeException
  = InvalidResultAttributeException { "Message" :: NullOrUndefined (String) }
```

<p>The specified inventory item result attribute is not valid.</p>

#### `InvalidRole`

``` purescript
newtype InvalidRole
  = InvalidRole { "Message" :: NullOrUndefined (String) }
```

<p>The role name can't contain invalid characters. Also verify that you specified an IAM role for notifications that includes the required trust policy. For information about configuring the IAM role for Run Command notifications, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/rc-sns-notifications.html">Configuring Amazon SNS Notifications for Run Command</a> in the <i>AWS Systems Manager User Guide</i>.</p>

#### `InvalidSchedule`

``` purescript
newtype InvalidSchedule
  = InvalidSchedule { "Message" :: NullOrUndefined (String) }
```

<p>The schedule is invalid. Verify your cron or rate expression and try again.</p>

#### `InvalidTarget`

``` purescript
newtype InvalidTarget
  = InvalidTarget { "Message" :: NullOrUndefined (String) }
```

<p>The target is not valid or does not exist. It might not be configured for EC2 Systems Manager or you might not have permission to perform the operation.</p>

#### `InvalidTypeNameException`

``` purescript
newtype InvalidTypeNameException
  = InvalidTypeNameException { "Message" :: NullOrUndefined (String) }
```

<p>The parameter type name is not valid.</p>

#### `InvalidUpdate`

``` purescript
newtype InvalidUpdate
  = InvalidUpdate { "Message" :: NullOrUndefined (String) }
```

<p>The update is not valid.</p>

#### `InventoryAggregator`

``` purescript
newtype InventoryAggregator
  = InventoryAggregator { "Expression" :: NullOrUndefined (InventoryAggregatorExpression), "Aggregators" :: NullOrUndefined (InventoryAggregatorList) }
```

<p>Specifies the inventory type and attribute for the aggregation execution.</p>

#### `InventoryAggregatorExpression`

``` purescript
newtype InventoryAggregatorExpression
  = InventoryAggregatorExpression String
```

#### `InventoryAggregatorList`

``` purescript
newtype InventoryAggregatorList
  = InventoryAggregatorList (Array InventoryAggregator)
```

#### `InventoryAttributeDataType`

``` purescript
newtype InventoryAttributeDataType
  = InventoryAttributeDataType String
```

#### `InventoryFilter`

``` purescript
newtype InventoryFilter
  = InventoryFilter { "Key" :: InventoryFilterKey, "Values" :: InventoryFilterValueList, "Type" :: NullOrUndefined (InventoryQueryOperatorType) }
```

<p>One or more filters. Use a filter to return a more specific list of results.</p>

#### `InventoryFilterKey`

``` purescript
newtype InventoryFilterKey
  = InventoryFilterKey String
```

#### `InventoryFilterList`

``` purescript
newtype InventoryFilterList
  = InventoryFilterList (Array InventoryFilter)
```

#### `InventoryFilterValue`

``` purescript
newtype InventoryFilterValue
  = InventoryFilterValue String
```

#### `InventoryFilterValueList`

``` purescript
newtype InventoryFilterValueList
  = InventoryFilterValueList (Array InventoryFilterValue)
```

#### `InventoryItem`

``` purescript
newtype InventoryItem
  = InventoryItem { "TypeName" :: InventoryItemTypeName, "SchemaVersion" :: InventoryItemSchemaVersion, "CaptureTime" :: InventoryItemCaptureTime, "ContentHash" :: NullOrUndefined (InventoryItemContentHash), "Content" :: NullOrUndefined (InventoryItemEntryList), "Context" :: NullOrUndefined (InventoryItemContentContext) }
```

<p>Information collected from managed instances based on your inventory policy document</p>

#### `InventoryItemAttribute`

``` purescript
newtype InventoryItemAttribute
  = InventoryItemAttribute { "Name" :: InventoryItemAttributeName, "DataType" :: InventoryAttributeDataType }
```

<p>Attributes are the entries within the inventory item content. It contains name and value.</p>

#### `InventoryItemAttributeList`

``` purescript
newtype InventoryItemAttributeList
  = InventoryItemAttributeList (Array InventoryItemAttribute)
```

#### `InventoryItemAttributeName`

``` purescript
newtype InventoryItemAttributeName
  = InventoryItemAttributeName String
```

#### `InventoryItemCaptureTime`

``` purescript
newtype InventoryItemCaptureTime
  = InventoryItemCaptureTime String
```

#### `InventoryItemContentContext`

``` purescript
newtype InventoryItemContentContext
  = InventoryItemContentContext (Map AttributeName AttributeValue)
```

#### `InventoryItemContentHash`

``` purescript
newtype InventoryItemContentHash
  = InventoryItemContentHash String
```

#### `InventoryItemEntry`

``` purescript
newtype InventoryItemEntry
  = InventoryItemEntry (Map AttributeName AttributeValue)
```

#### `InventoryItemEntryList`

``` purescript
newtype InventoryItemEntryList
  = InventoryItemEntryList (Array InventoryItemEntry)
```

#### `InventoryItemList`

``` purescript
newtype InventoryItemList
  = InventoryItemList (Array InventoryItem)
```

#### `InventoryItemSchema`

``` purescript
newtype InventoryItemSchema
  = InventoryItemSchema { "TypeName" :: InventoryItemTypeName, "Version" :: NullOrUndefined (InventoryItemSchemaVersion), "Attributes" :: InventoryItemAttributeList, "DisplayName" :: NullOrUndefined (InventoryTypeDisplayName) }
```

<p>The inventory item schema definition. Users can use this to compose inventory query filters.</p>

#### `InventoryItemSchemaResultList`

``` purescript
newtype InventoryItemSchemaResultList
  = InventoryItemSchemaResultList (Array InventoryItemSchema)
```

#### `InventoryItemSchemaVersion`

``` purescript
newtype InventoryItemSchemaVersion
  = InventoryItemSchemaVersion String
```

#### `InventoryItemTypeName`

``` purescript
newtype InventoryItemTypeName
  = InventoryItemTypeName String
```

#### `InventoryItemTypeNameFilter`

``` purescript
newtype InventoryItemTypeNameFilter
  = InventoryItemTypeNameFilter String
```

#### `InventoryQueryOperatorType`

``` purescript
newtype InventoryQueryOperatorType
  = InventoryQueryOperatorType String
```

#### `InventoryResultEntity`

``` purescript
newtype InventoryResultEntity
  = InventoryResultEntity { "Id" :: NullOrUndefined (InventoryResultEntityId), "Data" :: NullOrUndefined (InventoryResultItemMap) }
```

<p>Inventory query results.</p>

#### `InventoryResultEntityId`

``` purescript
newtype InventoryResultEntityId
  = InventoryResultEntityId String
```

#### `InventoryResultEntityList`

``` purescript
newtype InventoryResultEntityList
  = InventoryResultEntityList (Array InventoryResultEntity)
```

#### `InventoryResultItem`

``` purescript
newtype InventoryResultItem
  = InventoryResultItem { "TypeName" :: InventoryItemTypeName, "SchemaVersion" :: InventoryItemSchemaVersion, "CaptureTime" :: NullOrUndefined (InventoryItemCaptureTime), "ContentHash" :: NullOrUndefined (InventoryItemContentHash), "Content" :: InventoryItemEntryList }
```

<p>The inventory result item.</p>

#### `InventoryResultItemKey`

``` purescript
newtype InventoryResultItemKey
  = InventoryResultItemKey String
```

#### `InventoryResultItemMap`

``` purescript
newtype InventoryResultItemMap
  = InventoryResultItemMap (Map InventoryResultItemKey InventoryResultItem)
```

#### `InventoryTypeDisplayName`

``` purescript
newtype InventoryTypeDisplayName
  = InventoryTypeDisplayName String
```

#### `InvocationDoesNotExist`

``` purescript
newtype InvocationDoesNotExist
  = InvocationDoesNotExist {  }
```

<p>The command ID and instance ID you specified did not match any invocations. Verify the command ID adn the instance ID and try again. </p>

#### `InvocationTraceOutput`

``` purescript
newtype InvocationTraceOutput
  = InvocationTraceOutput String
```

#### `IsSubTypeSchema`

``` purescript
newtype IsSubTypeSchema
  = IsSubTypeSchema Boolean
```

#### `ItemContentMismatchException`

``` purescript
newtype ItemContentMismatchException
  = ItemContentMismatchException { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "Message" :: NullOrUndefined (String) }
```

<p>The inventory item has invalid content. </p>

#### `ItemSizeLimitExceededException`

``` purescript
newtype ItemSizeLimitExceededException
  = ItemSizeLimitExceededException { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "Message" :: NullOrUndefined (String) }
```

<p>The inventory item size has exceeded the size limit.</p>

#### `KeyList`

``` purescript
newtype KeyList
  = KeyList (Array TagKey)
```

#### `LastResourceDataSyncStatus`

``` purescript
newtype LastResourceDataSyncStatus
  = LastResourceDataSyncStatus String
```

#### `LastResourceDataSyncTime`

``` purescript
newtype LastResourceDataSyncTime
  = LastResourceDataSyncTime Number
```

#### `LastSuccessfulResourceDataSyncTime`

``` purescript
newtype LastSuccessfulResourceDataSyncTime
  = LastSuccessfulResourceDataSyncTime Number
```

#### `ListAssociationVersionsRequest`

``` purescript
newtype ListAssociationVersionsRequest
  = ListAssociationVersionsRequest { "AssociationId" :: AssociationId, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListAssociationVersionsResult`

``` purescript
newtype ListAssociationVersionsResult
  = ListAssociationVersionsResult { "AssociationVersions" :: NullOrUndefined (AssociationVersionList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListAssociationsRequest`

``` purescript
newtype ListAssociationsRequest
  = ListAssociationsRequest { "AssociationFilterList" :: NullOrUndefined (AssociationFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListAssociationsResult`

``` purescript
newtype ListAssociationsResult
  = ListAssociationsResult { "Associations" :: NullOrUndefined (AssociationList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListCommandInvocationsRequest`

``` purescript
newtype ListCommandInvocationsRequest
  = ListCommandInvocationsRequest { "CommandId" :: NullOrUndefined (CommandId), "InstanceId" :: NullOrUndefined (InstanceId), "MaxResults" :: NullOrUndefined (CommandMaxResults), "NextToken" :: NullOrUndefined (NextToken), "Filters" :: NullOrUndefined (CommandFilterList), "Details" :: NullOrUndefined (Boolean) }
```

#### `ListCommandInvocationsResult`

``` purescript
newtype ListCommandInvocationsResult
  = ListCommandInvocationsResult { "CommandInvocations" :: NullOrUndefined (CommandInvocationList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListCommandsRequest`

``` purescript
newtype ListCommandsRequest
  = ListCommandsRequest { "CommandId" :: NullOrUndefined (CommandId), "InstanceId" :: NullOrUndefined (InstanceId), "MaxResults" :: NullOrUndefined (CommandMaxResults), "NextToken" :: NullOrUndefined (NextToken), "Filters" :: NullOrUndefined (CommandFilterList) }
```

#### `ListCommandsResult`

``` purescript
newtype ListCommandsResult
  = ListCommandsResult { "Commands" :: NullOrUndefined (CommandList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListComplianceItemsRequest`

``` purescript
newtype ListComplianceItemsRequest
  = ListComplianceItemsRequest { "Filters" :: NullOrUndefined (ComplianceStringFilterList), "ResourceIds" :: NullOrUndefined (ComplianceResourceIdList), "ResourceTypes" :: NullOrUndefined (ComplianceResourceTypeList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `ListComplianceItemsResult`

``` purescript
newtype ListComplianceItemsResult
  = ListComplianceItemsResult { "ComplianceItems" :: NullOrUndefined (ComplianceItemList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListComplianceSummariesRequest`

``` purescript
newtype ListComplianceSummariesRequest
  = ListComplianceSummariesRequest { "Filters" :: NullOrUndefined (ComplianceStringFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `ListComplianceSummariesResult`

``` purescript
newtype ListComplianceSummariesResult
  = ListComplianceSummariesResult { "ComplianceSummaryItems" :: NullOrUndefined (ComplianceSummaryItemList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListDocumentVersionsRequest`

``` purescript
newtype ListDocumentVersionsRequest
  = ListDocumentVersionsRequest { "Name" :: DocumentName, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListDocumentVersionsResult`

``` purescript
newtype ListDocumentVersionsResult
  = ListDocumentVersionsResult { "DocumentVersions" :: NullOrUndefined (DocumentVersionList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListDocumentsRequest`

``` purescript
newtype ListDocumentsRequest
  = ListDocumentsRequest { "DocumentFilterList" :: NullOrUndefined (DocumentFilterList), "Filters" :: NullOrUndefined (DocumentKeyValuesFilterList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListDocumentsResult`

``` purescript
newtype ListDocumentsResult
  = ListDocumentsResult { "DocumentIdentifiers" :: NullOrUndefined (DocumentIdentifierList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListInventoryEntriesRequest`

``` purescript
newtype ListInventoryEntriesRequest
  = ListInventoryEntriesRequest { "InstanceId" :: InstanceId, "TypeName" :: InventoryItemTypeName, "Filters" :: NullOrUndefined (InventoryFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `ListInventoryEntriesResult`

``` purescript
newtype ListInventoryEntriesResult
  = ListInventoryEntriesResult { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "InstanceId" :: NullOrUndefined (InstanceId), "SchemaVersion" :: NullOrUndefined (InventoryItemSchemaVersion), "CaptureTime" :: NullOrUndefined (InventoryItemCaptureTime), "Entries" :: NullOrUndefined (InventoryItemEntryList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListResourceComplianceSummariesRequest`

``` purescript
newtype ListResourceComplianceSummariesRequest
  = ListResourceComplianceSummariesRequest { "Filters" :: NullOrUndefined (ComplianceStringFilterList), "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `ListResourceComplianceSummariesResult`

``` purescript
newtype ListResourceComplianceSummariesResult
  = ListResourceComplianceSummariesResult { "ResourceComplianceSummaryItems" :: NullOrUndefined (ResourceComplianceSummaryItemList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListResourceDataSyncRequest`

``` purescript
newtype ListResourceDataSyncRequest
  = ListResourceDataSyncRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `ListResourceDataSyncResult`

``` purescript
newtype ListResourceDataSyncResult
  = ListResourceDataSyncResult { "ResourceDataSyncItems" :: NullOrUndefined (ResourceDataSyncItemList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "ResourceType" :: ResourceTypeForTagging, "ResourceId" :: ResourceId }
```

#### `ListTagsForResourceResult`

``` purescript
newtype ListTagsForResourceResult
  = ListTagsForResourceResult { "TagList" :: NullOrUndefined (TagList) }
```

#### `LoggingInfo`

``` purescript
newtype LoggingInfo
  = LoggingInfo { "S3BucketName" :: S3BucketName, "S3KeyPrefix" :: NullOrUndefined (S3KeyPrefix), "S3Region" :: S3Region }
```

<p>Information about an Amazon S3 bucket to write instance-level logs to.</p>

#### `MaintenanceWindowAllowUnassociatedTargets`

``` purescript
newtype MaintenanceWindowAllowUnassociatedTargets
  = MaintenanceWindowAllowUnassociatedTargets Boolean
```

#### `MaintenanceWindowAutomationParameters`

``` purescript
newtype MaintenanceWindowAutomationParameters
  = MaintenanceWindowAutomationParameters { "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Parameters" :: NullOrUndefined (AutomationParameterMap) }
```

<p>The parameters for an AUTOMATION task type.</p>

#### `MaintenanceWindowCutoff`

``` purescript
newtype MaintenanceWindowCutoff
  = MaintenanceWindowCutoff Int
```

#### `MaintenanceWindowDescription`

``` purescript
newtype MaintenanceWindowDescription
  = MaintenanceWindowDescription String
```

#### `MaintenanceWindowDurationHours`

``` purescript
newtype MaintenanceWindowDurationHours
  = MaintenanceWindowDurationHours Int
```

#### `MaintenanceWindowEnabled`

``` purescript
newtype MaintenanceWindowEnabled
  = MaintenanceWindowEnabled Boolean
```

#### `MaintenanceWindowExecution`

``` purescript
newtype MaintenanceWindowExecution
  = MaintenanceWindowExecution { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime) }
```

<p>Describes the information about an execution of a Maintenance Window. </p>

#### `MaintenanceWindowExecutionId`

``` purescript
newtype MaintenanceWindowExecutionId
  = MaintenanceWindowExecutionId String
```

#### `MaintenanceWindowExecutionList`

``` purescript
newtype MaintenanceWindowExecutionList
  = MaintenanceWindowExecutionList (Array MaintenanceWindowExecution)
```

#### `MaintenanceWindowExecutionStatus`

``` purescript
newtype MaintenanceWindowExecutionStatus
  = MaintenanceWindowExecutionStatus String
```

#### `MaintenanceWindowExecutionStatusDetails`

``` purescript
newtype MaintenanceWindowExecutionStatusDetails
  = MaintenanceWindowExecutionStatusDetails String
```

#### `MaintenanceWindowExecutionTaskExecutionId`

``` purescript
newtype MaintenanceWindowExecutionTaskExecutionId
  = MaintenanceWindowExecutionTaskExecutionId String
```

#### `MaintenanceWindowExecutionTaskId`

``` purescript
newtype MaintenanceWindowExecutionTaskId
  = MaintenanceWindowExecutionTaskId String
```

#### `MaintenanceWindowExecutionTaskIdList`

``` purescript
newtype MaintenanceWindowExecutionTaskIdList
  = MaintenanceWindowExecutionTaskIdList (Array MaintenanceWindowExecutionTaskId)
```

#### `MaintenanceWindowExecutionTaskIdentity`

``` purescript
newtype MaintenanceWindowExecutionTaskIdentity
  = MaintenanceWindowExecutionTaskIdentity { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType) }
```

<p>Information about a task execution performed as part of a Maintenance Window execution.</p>

#### `MaintenanceWindowExecutionTaskIdentityList`

``` purescript
newtype MaintenanceWindowExecutionTaskIdentityList
  = MaintenanceWindowExecutionTaskIdentityList (Array MaintenanceWindowExecutionTaskIdentity)
```

#### `MaintenanceWindowExecutionTaskInvocationId`

``` purescript
newtype MaintenanceWindowExecutionTaskInvocationId
  = MaintenanceWindowExecutionTaskInvocationId String
```

#### `MaintenanceWindowExecutionTaskInvocationIdentity`

``` purescript
newtype MaintenanceWindowExecutionTaskInvocationIdentity
  = MaintenanceWindowExecutionTaskInvocationIdentity { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId), "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId), "InvocationId" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationId), "ExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskExecutionId), "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType), "Parameters" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationParameters), "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus), "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails), "StartTime" :: NullOrUndefined (DateTime), "EndTime" :: NullOrUndefined (DateTime), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTaskTargetId) }
```

<p>Describes the information about a task invocation for a particular target as part of a task execution performed as part of a Maintenance Window execution.</p>

#### `MaintenanceWindowExecutionTaskInvocationIdentityList`

``` purescript
newtype MaintenanceWindowExecutionTaskInvocationIdentityList
  = MaintenanceWindowExecutionTaskInvocationIdentityList (Array MaintenanceWindowExecutionTaskInvocationIdentity)
```

#### `MaintenanceWindowExecutionTaskInvocationParameters`

``` purescript
newtype MaintenanceWindowExecutionTaskInvocationParameters
  = MaintenanceWindowExecutionTaskInvocationParameters String
```

#### `MaintenanceWindowFilter`

``` purescript
newtype MaintenanceWindowFilter
  = MaintenanceWindowFilter { "Key" :: NullOrUndefined (MaintenanceWindowFilterKey), "Values" :: NullOrUndefined (MaintenanceWindowFilterValues) }
```

<p>Filter used in the request.</p>

#### `MaintenanceWindowFilterKey`

``` purescript
newtype MaintenanceWindowFilterKey
  = MaintenanceWindowFilterKey String
```

#### `MaintenanceWindowFilterList`

``` purescript
newtype MaintenanceWindowFilterList
  = MaintenanceWindowFilterList (Array MaintenanceWindowFilter)
```

#### `MaintenanceWindowFilterValue`

``` purescript
newtype MaintenanceWindowFilterValue
  = MaintenanceWindowFilterValue String
```

#### `MaintenanceWindowFilterValues`

``` purescript
newtype MaintenanceWindowFilterValues
  = MaintenanceWindowFilterValues (Array MaintenanceWindowFilterValue)
```

#### `MaintenanceWindowId`

``` purescript
newtype MaintenanceWindowId
  = MaintenanceWindowId String
```

#### `MaintenanceWindowIdentity`

``` purescript
newtype MaintenanceWindowIdentity
  = MaintenanceWindowIdentity { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled), "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours), "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff) }
```

<p>Information about the Maintenance Window.</p>

#### `MaintenanceWindowIdentityList`

``` purescript
newtype MaintenanceWindowIdentityList
  = MaintenanceWindowIdentityList (Array MaintenanceWindowIdentity)
```

#### `MaintenanceWindowLambdaClientContext`

``` purescript
newtype MaintenanceWindowLambdaClientContext
  = MaintenanceWindowLambdaClientContext String
```

#### `MaintenanceWindowLambdaParameters`

``` purescript
newtype MaintenanceWindowLambdaParameters
  = MaintenanceWindowLambdaParameters { "ClientContext" :: NullOrUndefined (MaintenanceWindowLambdaClientContext), "Qualifier" :: NullOrUndefined (MaintenanceWindowLambdaQualifier), "Payload" :: NullOrUndefined (MaintenanceWindowLambdaPayload) }
```

<p>The parameters for a LAMBDA task type.</p>

#### `MaintenanceWindowLambdaPayload`

``` purescript
newtype MaintenanceWindowLambdaPayload
  = MaintenanceWindowLambdaPayload String
```

#### `MaintenanceWindowLambdaQualifier`

``` purescript
newtype MaintenanceWindowLambdaQualifier
  = MaintenanceWindowLambdaQualifier String
```

#### `MaintenanceWindowMaxResults`

``` purescript
newtype MaintenanceWindowMaxResults
  = MaintenanceWindowMaxResults Int
```

#### `MaintenanceWindowName`

``` purescript
newtype MaintenanceWindowName
  = MaintenanceWindowName String
```

#### `MaintenanceWindowResourceType`

``` purescript
newtype MaintenanceWindowResourceType
  = MaintenanceWindowResourceType String
```

#### `MaintenanceWindowRunCommandParameters`

``` purescript
newtype MaintenanceWindowRunCommandParameters
  = MaintenanceWindowRunCommandParameters { "Comment" :: NullOrUndefined (Comment), "DocumentHash" :: NullOrUndefined (DocumentHash), "DocumentHashType" :: NullOrUndefined (DocumentHashType), "NotificationConfig" :: NullOrUndefined (NotificationConfig), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix), "Parameters" :: NullOrUndefined (Parameters), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "TimeoutSeconds" :: NullOrUndefined (TimeoutSeconds) }
```

<p>The parameters for a RUN_COMMAND task type.</p>

#### `MaintenanceWindowSchedule`

``` purescript
newtype MaintenanceWindowSchedule
  = MaintenanceWindowSchedule String
```

#### `MaintenanceWindowStepFunctionsInput`

``` purescript
newtype MaintenanceWindowStepFunctionsInput
  = MaintenanceWindowStepFunctionsInput String
```

#### `MaintenanceWindowStepFunctionsName`

``` purescript
newtype MaintenanceWindowStepFunctionsName
  = MaintenanceWindowStepFunctionsName String
```

#### `MaintenanceWindowStepFunctionsParameters`

``` purescript
newtype MaintenanceWindowStepFunctionsParameters
  = MaintenanceWindowStepFunctionsParameters { "Input" :: NullOrUndefined (MaintenanceWindowStepFunctionsInput), "Name" :: NullOrUndefined (MaintenanceWindowStepFunctionsName) }
```

<p>The parameters for the STEP_FUNCTION execution.</p>

#### `MaintenanceWindowTarget`

``` purescript
newtype MaintenanceWindowTarget
  = MaintenanceWindowTarget { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId), "ResourceType" :: NullOrUndefined (MaintenanceWindowResourceType), "Targets" :: NullOrUndefined (Targets), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

<p>The target registered with the Maintenance Window.</p>

#### `MaintenanceWindowTargetId`

``` purescript
newtype MaintenanceWindowTargetId
  = MaintenanceWindowTargetId String
```

#### `MaintenanceWindowTargetList`

``` purescript
newtype MaintenanceWindowTargetList
  = MaintenanceWindowTargetList (Array MaintenanceWindowTarget)
```

#### `MaintenanceWindowTask`

``` purescript
newtype MaintenanceWindowTask
  = MaintenanceWindowTask { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "Type" :: NullOrUndefined (MaintenanceWindowTaskType), "Targets" :: NullOrUndefined (Targets), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "LoggingInfo" :: NullOrUndefined (LoggingInfo), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

<p>Information about a task defined for a Maintenance Window.</p>

#### `MaintenanceWindowTaskArn`

``` purescript
newtype MaintenanceWindowTaskArn
  = MaintenanceWindowTaskArn String
```

#### `MaintenanceWindowTaskId`

``` purescript
newtype MaintenanceWindowTaskId
  = MaintenanceWindowTaskId String
```

#### `MaintenanceWindowTaskInvocationParameters`

``` purescript
newtype MaintenanceWindowTaskInvocationParameters
  = MaintenanceWindowTaskInvocationParameters { "RunCommand" :: NullOrUndefined (MaintenanceWindowRunCommandParameters), "Automation" :: NullOrUndefined (MaintenanceWindowAutomationParameters), "StepFunctions" :: NullOrUndefined (MaintenanceWindowStepFunctionsParameters), "Lambda" :: NullOrUndefined (MaintenanceWindowLambdaParameters) }
```

<p>The parameters for task execution.</p>

#### `MaintenanceWindowTaskList`

``` purescript
newtype MaintenanceWindowTaskList
  = MaintenanceWindowTaskList (Array MaintenanceWindowTask)
```

#### `MaintenanceWindowTaskParameterName`

``` purescript
newtype MaintenanceWindowTaskParameterName
  = MaintenanceWindowTaskParameterName String
```

#### `MaintenanceWindowTaskParameterValue`

``` purescript
newtype MaintenanceWindowTaskParameterValue
  = MaintenanceWindowTaskParameterValue String
```

#### `MaintenanceWindowTaskParameterValueExpression`

``` purescript
newtype MaintenanceWindowTaskParameterValueExpression
  = MaintenanceWindowTaskParameterValueExpression { "Values" :: NullOrUndefined (MaintenanceWindowTaskParameterValueList) }
```

<p>Defines the values for a task parameter.</p>

#### `MaintenanceWindowTaskParameterValueList`

``` purescript
newtype MaintenanceWindowTaskParameterValueList
  = MaintenanceWindowTaskParameterValueList (Array MaintenanceWindowTaskParameterValue)
```

#### `MaintenanceWindowTaskParameters`

``` purescript
newtype MaintenanceWindowTaskParameters
  = MaintenanceWindowTaskParameters (Map MaintenanceWindowTaskParameterName MaintenanceWindowTaskParameterValueExpression)
```

#### `MaintenanceWindowTaskParametersList`

``` purescript
newtype MaintenanceWindowTaskParametersList
  = MaintenanceWindowTaskParametersList (Array MaintenanceWindowTaskParameters)
```

#### `MaintenanceWindowTaskPriority`

``` purescript
newtype MaintenanceWindowTaskPriority
  = MaintenanceWindowTaskPriority Int
```

#### `MaintenanceWindowTaskTargetId`

``` purescript
newtype MaintenanceWindowTaskTargetId
  = MaintenanceWindowTaskTargetId String
```

#### `MaintenanceWindowTaskType`

``` purescript
newtype MaintenanceWindowTaskType
  = MaintenanceWindowTaskType String
```

#### `ManagedInstanceId`

``` purescript
newtype ManagedInstanceId
  = ManagedInstanceId String
```

#### `MaxConcurrency`

``` purescript
newtype MaxConcurrency
  = MaxConcurrency String
```

#### `MaxDocumentSizeExceeded`

``` purescript
newtype MaxDocumentSizeExceeded
  = MaxDocumentSizeExceeded { "Message" :: NullOrUndefined (String) }
```

<p>The size limit of a document is 64 KB.</p>

#### `MaxErrors`

``` purescript
newtype MaxErrors
  = MaxErrors String
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `MaxResultsEC2Compatible`

``` purescript
newtype MaxResultsEC2Compatible
  = MaxResultsEC2Compatible Int
```

#### `ModifyDocumentPermissionRequest`

``` purescript
newtype ModifyDocumentPermissionRequest
  = ModifyDocumentPermissionRequest { "Name" :: DocumentName, "PermissionType" :: DocumentPermissionType, "AccountIdsToAdd" :: NullOrUndefined (AccountIdList), "AccountIdsToRemove" :: NullOrUndefined (AccountIdList) }
```

#### `ModifyDocumentPermissionResponse`

``` purescript
newtype ModifyDocumentPermissionResponse
  = ModifyDocumentPermissionResponse {  }
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `NonCompliantSummary`

``` purescript
newtype NonCompliantSummary
  = NonCompliantSummary { "NonCompliantCount" :: NullOrUndefined (ComplianceSummaryCount), "SeveritySummary" :: NullOrUndefined (SeveritySummary) }
```

<p>A summary of resources that are not compliant. The summary is organized according to resource type.</p>

#### `NormalStringMap`

``` purescript
newtype NormalStringMap
  = NormalStringMap (Map String String)
```

#### `NotificationArn`

``` purescript
newtype NotificationArn
  = NotificationArn String
```

#### `NotificationConfig`

``` purescript
newtype NotificationConfig
  = NotificationConfig { "NotificationArn" :: NullOrUndefined (NotificationArn), "NotificationEvents" :: NullOrUndefined (NotificationEventList), "NotificationType" :: NullOrUndefined (NotificationType) }
```

<p>Configurations for sending notifications.</p>

#### `NotificationEvent`

``` purescript
newtype NotificationEvent
  = NotificationEvent String
```

#### `NotificationEventList`

``` purescript
newtype NotificationEventList
  = NotificationEventList (Array NotificationEvent)
```

#### `NotificationType`

``` purescript
newtype NotificationType
  = NotificationType String
```

#### `OperatingSystem`

``` purescript
newtype OperatingSystem
  = OperatingSystem String
```

#### `OwnerInformation`

``` purescript
newtype OwnerInformation
  = OwnerInformation String
```

#### `PSParameterName`

``` purescript
newtype PSParameterName
  = PSParameterName String
```

#### `PSParameterValue`

``` purescript
newtype PSParameterValue
  = PSParameterValue String
```

#### `PSParameterVersion`

``` purescript
newtype PSParameterVersion
  = PSParameterVersion Number
```

#### `Parameter`

``` purescript
newtype Parameter
  = Parameter { "Name" :: NullOrUndefined (PSParameterName), "Type" :: NullOrUndefined (ParameterType), "Value" :: NullOrUndefined (PSParameterValue), "Version" :: NullOrUndefined (PSParameterVersion) }
```

<p>An Amazon EC2 Systems Manager parameter in Parameter Store.</p>

#### `ParameterAlreadyExists`

``` purescript
newtype ParameterAlreadyExists
  = ParameterAlreadyExists { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter already exists. You can't create duplicate parameters.</p>

#### `ParameterDescription`

``` purescript
newtype ParameterDescription
  = ParameterDescription String
```

#### `ParameterHistory`

``` purescript
newtype ParameterHistory
  = ParameterHistory { "Name" :: NullOrUndefined (PSParameterName), "Type" :: NullOrUndefined (ParameterType), "KeyId" :: NullOrUndefined (ParameterKeyId), "LastModifiedDate" :: NullOrUndefined (DateTime), "LastModifiedUser" :: NullOrUndefined (String), "Description" :: NullOrUndefined (ParameterDescription), "Value" :: NullOrUndefined (PSParameterValue), "AllowedPattern" :: NullOrUndefined (AllowedPattern), "Version" :: NullOrUndefined (PSParameterVersion) }
```

<p>Information about parameter usage.</p>

#### `ParameterHistoryList`

``` purescript
newtype ParameterHistoryList
  = ParameterHistoryList (Array ParameterHistory)
```

#### `ParameterKeyId`

``` purescript
newtype ParameterKeyId
  = ParameterKeyId String
```

#### `ParameterLimitExceeded`

``` purescript
newtype ParameterLimitExceeded
  = ParameterLimitExceeded { "Message'" :: NullOrUndefined (String) }
```

<p>You have exceeded the number of parameters for this AWS account. Delete one or more parameters and try again.</p>

#### `ParameterList`

``` purescript
newtype ParameterList
  = ParameterList (Array Parameter)
```

#### `ParameterMaxVersionLimitExceeded`

``` purescript
newtype ParameterMaxVersionLimitExceeded
  = ParameterMaxVersionLimitExceeded { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter exceeded the maximum number of allowed versions.</p>

#### `ParameterMetadata`

``` purescript
newtype ParameterMetadata
  = ParameterMetadata { "Name" :: NullOrUndefined (PSParameterName), "Type" :: NullOrUndefined (ParameterType), "KeyId" :: NullOrUndefined (ParameterKeyId), "LastModifiedDate" :: NullOrUndefined (DateTime), "LastModifiedUser" :: NullOrUndefined (String), "Description" :: NullOrUndefined (ParameterDescription), "AllowedPattern" :: NullOrUndefined (AllowedPattern), "Version" :: NullOrUndefined (PSParameterVersion) }
```

<p>Metada includes information like the ARN of the last user and the date/time the parameter was last used.</p>

#### `ParameterMetadataList`

``` purescript
newtype ParameterMetadataList
  = ParameterMetadataList (Array ParameterMetadata)
```

#### `ParameterName`

``` purescript
newtype ParameterName
  = ParameterName String
```

#### `ParameterNameList`

``` purescript
newtype ParameterNameList
  = ParameterNameList (Array PSParameterName)
```

#### `ParameterNotFound`

``` purescript
newtype ParameterNotFound
  = ParameterNotFound { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter could not be found. Verify the name and try again.</p>

#### `ParameterPatternMismatchException`

``` purescript
newtype ParameterPatternMismatchException
  = ParameterPatternMismatchException { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter name is not valid.</p>

#### `ParameterStringFilter`

``` purescript
newtype ParameterStringFilter
  = ParameterStringFilter { "Key" :: ParameterStringFilterKey, "Option" :: NullOrUndefined (ParameterStringQueryOption), "Values" :: NullOrUndefined (ParameterStringFilterValueList) }
```

<p>One or more filters. Use a filter to return a more specific list of results.</p>

#### `ParameterStringFilterKey`

``` purescript
newtype ParameterStringFilterKey
  = ParameterStringFilterKey String
```

#### `ParameterStringFilterList`

``` purescript
newtype ParameterStringFilterList
  = ParameterStringFilterList (Array ParameterStringFilter)
```

#### `ParameterStringFilterValue`

``` purescript
newtype ParameterStringFilterValue
  = ParameterStringFilterValue String
```

#### `ParameterStringFilterValueList`

``` purescript
newtype ParameterStringFilterValueList
  = ParameterStringFilterValueList (Array ParameterStringFilterValue)
```

#### `ParameterStringQueryOption`

``` purescript
newtype ParameterStringQueryOption
  = ParameterStringQueryOption String
```

#### `ParameterType`

``` purescript
newtype ParameterType
  = ParameterType String
```

#### `ParameterValue`

``` purescript
newtype ParameterValue
  = ParameterValue String
```

#### `ParameterValueList`

``` purescript
newtype ParameterValueList
  = ParameterValueList (Array ParameterValue)
```

#### `ParameterVersionNotFound`

``` purescript
newtype ParameterVersionNotFound
  = ParameterVersionNotFound { "Message'" :: NullOrUndefined (String) }
```

<p>The specified parameter version was not found. Verify the parameter name and version, and try again.</p>

#### `Parameters`

``` purescript
newtype Parameters
  = Parameters (Map ParameterName ParameterValueList)
```

#### `ParametersFilter`

``` purescript
newtype ParametersFilter
  = ParametersFilter { "Key" :: ParametersFilterKey, "Values" :: ParametersFilterValueList }
```

<p>This data type is deprecated. Instead, use <a>ParameterStringFilter</a>.</p>

#### `ParametersFilterKey`

``` purescript
newtype ParametersFilterKey
  = ParametersFilterKey String
```

#### `ParametersFilterList`

``` purescript
newtype ParametersFilterList
  = ParametersFilterList (Array ParametersFilter)
```

#### `ParametersFilterValue`

``` purescript
newtype ParametersFilterValue
  = ParametersFilterValue String
```

#### `ParametersFilterValueList`

``` purescript
newtype ParametersFilterValueList
  = ParametersFilterValueList (Array ParametersFilterValue)
```

#### `Patch`

``` purescript
newtype Patch
  = Patch { "Id" :: NullOrUndefined (PatchId), "ReleaseDate" :: NullOrUndefined (DateTime), "Title" :: NullOrUndefined (PatchTitle), "Description" :: NullOrUndefined (PatchDescription), "ContentUrl" :: NullOrUndefined (PatchContentUrl), "Vendor" :: NullOrUndefined (PatchVendor), "ProductFamily" :: NullOrUndefined (PatchProductFamily), "Product" :: NullOrUndefined (PatchProduct), "Classification" :: NullOrUndefined (PatchClassification), "MsrcSeverity" :: NullOrUndefined (PatchMsrcSeverity), "KbNumber" :: NullOrUndefined (PatchKbNumber), "MsrcNumber" :: NullOrUndefined (PatchMsrcNumber), "Language" :: NullOrUndefined (PatchLanguage) }
```

<p>Represents metadata about a patch.</p>

#### `PatchBaselineIdentity`

``` purescript
newtype PatchBaselineIdentity
  = PatchBaselineIdentity { "BaselineId" :: NullOrUndefined (BaselineId), "BaselineName" :: NullOrUndefined (BaselineName), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "BaselineDescription" :: NullOrUndefined (BaselineDescription), "DefaultBaseline" :: NullOrUndefined (DefaultBaseline) }
```

<p>Defines the basic information about a patch baseline.</p>

#### `PatchBaselineIdentityList`

``` purescript
newtype PatchBaselineIdentityList
  = PatchBaselineIdentityList (Array PatchBaselineIdentity)
```

#### `PatchBaselineMaxResults`

``` purescript
newtype PatchBaselineMaxResults
  = PatchBaselineMaxResults Int
```

#### `PatchClassification`

``` purescript
newtype PatchClassification
  = PatchClassification String
```

#### `PatchComplianceData`

``` purescript
newtype PatchComplianceData
  = PatchComplianceData { "Title" :: PatchTitle, "KBId" :: PatchKbNumber, "Classification" :: PatchClassification, "Severity" :: PatchSeverity, "State" :: PatchComplianceDataState, "InstalledTime" :: DateTime }
```

<p>Information about the state of a patch on a particular instance as it relates to the patch baseline used to patch the instance.</p>

#### `PatchComplianceDataList`

``` purescript
newtype PatchComplianceDataList
  = PatchComplianceDataList (Array PatchComplianceData)
```

#### `PatchComplianceDataState`

``` purescript
newtype PatchComplianceDataState
  = PatchComplianceDataState String
```

#### `PatchComplianceLevel`

``` purescript
newtype PatchComplianceLevel
  = PatchComplianceLevel String
```

#### `PatchComplianceMaxResults`

``` purescript
newtype PatchComplianceMaxResults
  = PatchComplianceMaxResults Int
```

#### `PatchContentUrl`

``` purescript
newtype PatchContentUrl
  = PatchContentUrl String
```

#### `PatchDeploymentStatus`

``` purescript
newtype PatchDeploymentStatus
  = PatchDeploymentStatus String
```

#### `PatchDescription`

``` purescript
newtype PatchDescription
  = PatchDescription String
```

#### `PatchFailedCount`

``` purescript
newtype PatchFailedCount
  = PatchFailedCount Int
```

#### `PatchFilter`

``` purescript
newtype PatchFilter
  = PatchFilter { "Key" :: PatchFilterKey, "Values" :: PatchFilterValueList }
```

<p>Defines a patch filter.</p> <p>A patch filter consists of key/value pairs, but not all keys are valid for all operating system types. For example, the key <code>PRODUCT</code> is valid for all supported operating system types. The key <code>MSRC_SEVERITY</code>, however, is valid only for Windows operating systems, and the key <code>SECTION</code> is valid only for Ubuntu operating systems.</p> <p>Refer to the following sections for information about which keys may be used with each major operating system, and which values are valid for each key.</p> <p> <b>Windows Operating Systems</b> </p> <p>The supported keys for Windows operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>MSRC_SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Windows7</code> </p> </li> <li> <p> <code>Windows8</code> </p> </li> <li> <p> <code>Windows8.1</code> </p> </li> <li> <p> <code>Windows8Embedded</code> </p> </li> <li> <p> <code>Windows10</code> </p> </li> <li> <p> <code>Windows10LTSB</code> </p> </li> <li> <p> <code>WindowsServer2008</code> </p> </li> <li> <p> <code>WindowsServer2008R2</code> </p> </li> <li> <p> <code>WindowsServer2012</code> </p> </li> <li> <p> <code>WindowsServer2012R2</code> </p> </li> <li> <p> <code>WindowsServer2016</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>CriticalUpdates</code> </p> </li> <li> <p> <code>DefinitionUpdates</code> </p> </li> <li> <p> <code>Drivers</code> </p> </li> <li> <p> <code>FeaturePacks</code> </p> </li> <li> <p> <code>SecurityUpdates</code> </p> </li> <li> <p> <code>ServicePacks</code> </p> </li> <li> <p> <code>Tools</code> </p> </li> <li> <p> <code>UpdateRollups</code> </p> </li> <li> <p> <code>Updates</code> </p> </li> <li> <p> <code>Upgrades</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>MSRC_SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> <li> <p> <code>Unspecified</code> </p> </li> </ul> <p> <b>Ubuntu Operating Systems</b> </p> <p>The supported keys for Ubuntu operating systems are <code>PRODUCT</code>, <code>PRIORITY</code>, and <code>SECTION</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Ubuntu14.04</code> </p> </li> <li> <p> <code>Ubuntu16.04</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>PRIORITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Required</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Standard</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Extra</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SECTION</code> </p> <p>Only the length of the key value is validated. Minimum length is 1. Maximum length is 64.</p> <p> <b>Amazon Linux Operating Systems</b> </p> <p>The supported keys for Amazon Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>AmazonLinux2012.03</code> </p> </li> <li> <p> <code>AmazonLinux2012.09</code> </p> </li> <li> <p> <code>AmazonLinux2013.03</code> </p> </li> <li> <p> <code>AmazonLinux2013.09</code> </p> </li> <li> <p> <code>AmazonLinux2014.03</code> </p> </li> <li> <p> <code>AmazonLinux2014.09</code> </p> </li> <li> <p> <code>AmazonLinux2015.03</code> </p> </li> <li> <p> <code>AmazonLinux2015.09</code> </p> </li> <li> <p> <code>AmazonLinux2016.03</code> </p> </li> <li> <p> <code>AmazonLinux2016.09</code> </p> </li> <li> <p> <code>AmazonLinux2017.03</code> </p> </li> <li> <p> <code>AmazonLinux2017.09</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>RedHat Enterprise Linux (RHEL) Operating Systems</b> </p> <p>The supported keys for RedHat Enterprise Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>RedhatEnterpriseLinux6.5</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.6</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.7</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.8</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.9</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.0</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.1</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.2</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.3</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.4</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>SUSE Linux Enterprise Server (SUSE) Operating Systems</b> </p> <p>The supported keys for SUSE operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Suse12.0</code> </p> </li> <li> <p> <code>Suse12.1</code> </p> </li> <li> <p> <code>Suse12.2</code> </p> </li> <li> <p> <code>Suse12.3</code> </p> </li> <li> <p> <code>Suse12.4</code> </p> </li> <li> <p> <code>Suse12.5</code> </p> </li> <li> <p> <code>Suse12.6</code> </p> </li> <li> <p> <code>Suse12.7</code> </p> </li> <li> <p> <code>Suse12.8</code> </p> </li> <li> <p> <code>Suse12.9</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Feature</code> </p> </li> <li> <p> <code>Document</code> </p> </li> <li> <p> <code>Yast</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul>

#### `PatchFilterGroup`

``` purescript
newtype PatchFilterGroup
  = PatchFilterGroup { "PatchFilters" :: PatchFilterList }
```

<p>A set of patch filters, typically used for approval rules.</p>

#### `PatchFilterKey`

``` purescript
newtype PatchFilterKey
  = PatchFilterKey String
```

#### `PatchFilterList`

``` purescript
newtype PatchFilterList
  = PatchFilterList (Array PatchFilter)
```

#### `PatchFilterValue`

``` purescript
newtype PatchFilterValue
  = PatchFilterValue String
```

#### `PatchFilterValueList`

``` purescript
newtype PatchFilterValueList
  = PatchFilterValueList (Array PatchFilterValue)
```

#### `PatchGroup`

``` purescript
newtype PatchGroup
  = PatchGroup String
```

#### `PatchGroupList`

``` purescript
newtype PatchGroupList
  = PatchGroupList (Array PatchGroup)
```

#### `PatchGroupPatchBaselineMapping`

``` purescript
newtype PatchGroupPatchBaselineMapping
  = PatchGroupPatchBaselineMapping { "PatchGroup" :: NullOrUndefined (PatchGroup), "BaselineIdentity" :: NullOrUndefined (PatchBaselineIdentity) }
```

<p>The mapping between a patch group and the patch baseline the patch group is registered with.</p>

#### `PatchGroupPatchBaselineMappingList`

``` purescript
newtype PatchGroupPatchBaselineMappingList
  = PatchGroupPatchBaselineMappingList (Array PatchGroupPatchBaselineMapping)
```

#### `PatchId`

``` purescript
newtype PatchId
  = PatchId String
```

#### `PatchIdList`

``` purescript
newtype PatchIdList
  = PatchIdList (Array PatchId)
```

#### `PatchInstalledCount`

``` purescript
newtype PatchInstalledCount
  = PatchInstalledCount Int
```

#### `PatchInstalledOtherCount`

``` purescript
newtype PatchInstalledOtherCount
  = PatchInstalledOtherCount Int
```

#### `PatchKbNumber`

``` purescript
newtype PatchKbNumber
  = PatchKbNumber String
```

#### `PatchLanguage`

``` purescript
newtype PatchLanguage
  = PatchLanguage String
```

#### `PatchList`

``` purescript
newtype PatchList
  = PatchList (Array Patch)
```

#### `PatchMissingCount`

``` purescript
newtype PatchMissingCount
  = PatchMissingCount Int
```

#### `PatchMsrcNumber`

``` purescript
newtype PatchMsrcNumber
  = PatchMsrcNumber String
```

#### `PatchMsrcSeverity`

``` purescript
newtype PatchMsrcSeverity
  = PatchMsrcSeverity String
```

#### `PatchNotApplicableCount`

``` purescript
newtype PatchNotApplicableCount
  = PatchNotApplicableCount Int
```

#### `PatchOperationType`

``` purescript
newtype PatchOperationType
  = PatchOperationType String
```

#### `PatchOrchestratorFilter`

``` purescript
newtype PatchOrchestratorFilter
  = PatchOrchestratorFilter { "Key" :: NullOrUndefined (PatchOrchestratorFilterKey), "Values" :: NullOrUndefined (PatchOrchestratorFilterValues) }
```

<p>Defines a filter used in Patch Manager APIs.</p>

#### `PatchOrchestratorFilterKey`

``` purescript
newtype PatchOrchestratorFilterKey
  = PatchOrchestratorFilterKey String
```

#### `PatchOrchestratorFilterList`

``` purescript
newtype PatchOrchestratorFilterList
  = PatchOrchestratorFilterList (Array PatchOrchestratorFilter)
```

#### `PatchOrchestratorFilterValue`

``` purescript
newtype PatchOrchestratorFilterValue
  = PatchOrchestratorFilterValue String
```

#### `PatchOrchestratorFilterValues`

``` purescript
newtype PatchOrchestratorFilterValues
  = PatchOrchestratorFilterValues (Array PatchOrchestratorFilterValue)
```

#### `PatchProduct`

``` purescript
newtype PatchProduct
  = PatchProduct String
```

#### `PatchProductFamily`

``` purescript
newtype PatchProductFamily
  = PatchProductFamily String
```

#### `PatchRule`

``` purescript
newtype PatchRule
  = PatchRule { "PatchFilterGroup" :: PatchFilterGroup, "ComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApproveAfterDays" :: ApproveAfterDays, "EnableNonSecurity" :: NullOrUndefined (Boolean) }
```

<p>Defines an approval rule for a patch baseline.</p>

#### `PatchRuleGroup`

``` purescript
newtype PatchRuleGroup
  = PatchRuleGroup { "PatchRules" :: PatchRuleList }
```

<p>A set of rules defining the approval rules for a patch baseline.</p>

#### `PatchRuleList`

``` purescript
newtype PatchRuleList
  = PatchRuleList (Array PatchRule)
```

#### `PatchSeverity`

``` purescript
newtype PatchSeverity
  = PatchSeverity String
```

#### `PatchSource`

``` purescript
newtype PatchSource
  = PatchSource { "Name" :: PatchSourceName, "Products" :: PatchSourceProductList, "Configuration" :: PatchSourceConfiguration }
```

<p>Information about the patches to use to update the instances, including target operating systems and source repository. Applies to Linux instances only.</p>

#### `PatchSourceConfiguration`

``` purescript
newtype PatchSourceConfiguration
  = PatchSourceConfiguration String
```

#### `PatchSourceList`

``` purescript
newtype PatchSourceList
  = PatchSourceList (Array PatchSource)
```

#### `PatchSourceName`

``` purescript
newtype PatchSourceName
  = PatchSourceName String
```

#### `PatchSourceProduct`

``` purescript
newtype PatchSourceProduct
  = PatchSourceProduct String
```

#### `PatchSourceProductList`

``` purescript
newtype PatchSourceProductList
  = PatchSourceProductList (Array PatchSourceProduct)
```

#### `PatchStatus`

``` purescript
newtype PatchStatus
  = PatchStatus { "DeploymentStatus" :: NullOrUndefined (PatchDeploymentStatus), "ComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovalDate" :: NullOrUndefined (DateTime) }
```

<p>Information about the approval status of a patch.</p>

#### `PatchTitle`

``` purescript
newtype PatchTitle
  = PatchTitle String
```

#### `PatchVendor`

``` purescript
newtype PatchVendor
  = PatchVendor String
```

#### `PingStatus`

``` purescript
newtype PingStatus
  = PingStatus String
```

#### `PlatformType`

``` purescript
newtype PlatformType
  = PlatformType String
```

#### `PlatformTypeList`

``` purescript
newtype PlatformTypeList
  = PlatformTypeList (Array PlatformType)
```

#### `Product`

``` purescript
newtype Product
  = Product String
```

#### `PutComplianceItemsRequest`

``` purescript
newtype PutComplianceItemsRequest
  = PutComplianceItemsRequest { "ResourceId" :: ComplianceResourceId, "ResourceType" :: ComplianceResourceType, "ComplianceType" :: ComplianceTypeName, "ExecutionSummary" :: ComplianceExecutionSummary, "Items" :: ComplianceItemEntryList, "ItemContentHash" :: NullOrUndefined (ComplianceItemContentHash) }
```

#### `PutComplianceItemsResult`

``` purescript
newtype PutComplianceItemsResult
  = PutComplianceItemsResult {  }
```

#### `PutInventoryRequest`

``` purescript
newtype PutInventoryRequest
  = PutInventoryRequest { "InstanceId" :: InstanceId, "Items" :: InventoryItemList }
```

#### `PutInventoryResult`

``` purescript
newtype PutInventoryResult
  = PutInventoryResult {  }
```

#### `PutParameterRequest`

``` purescript
newtype PutParameterRequest
  = PutParameterRequest { "Name" :: PSParameterName, "Description" :: NullOrUndefined (ParameterDescription), "Value" :: PSParameterValue, "Type" :: ParameterType, "KeyId" :: NullOrUndefined (ParameterKeyId), "Overwrite" :: NullOrUndefined (Boolean), "AllowedPattern" :: NullOrUndefined (AllowedPattern) }
```

#### `PutParameterResult`

``` purescript
newtype PutParameterResult
  = PutParameterResult { "Version" :: NullOrUndefined (PSParameterVersion) }
```

#### `RegisterDefaultPatchBaselineRequest`

``` purescript
newtype RegisterDefaultPatchBaselineRequest
  = RegisterDefaultPatchBaselineRequest { "BaselineId" :: BaselineId }
```

#### `RegisterDefaultPatchBaselineResult`

``` purescript
newtype RegisterDefaultPatchBaselineResult
  = RegisterDefaultPatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId) }
```

#### `RegisterPatchBaselineForPatchGroupRequest`

``` purescript
newtype RegisterPatchBaselineForPatchGroupRequest
  = RegisterPatchBaselineForPatchGroupRequest { "BaselineId" :: BaselineId, "PatchGroup" :: PatchGroup }
```

#### `RegisterPatchBaselineForPatchGroupResult`

``` purescript
newtype RegisterPatchBaselineForPatchGroupResult
  = RegisterPatchBaselineForPatchGroupResult { "BaselineId" :: NullOrUndefined (BaselineId), "PatchGroup" :: NullOrUndefined (PatchGroup) }
```

#### `RegisterTargetWithMaintenanceWindowRequest`

``` purescript
newtype RegisterTargetWithMaintenanceWindowRequest
  = RegisterTargetWithMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "ResourceType" :: MaintenanceWindowResourceType, "Targets" :: Targets, "OwnerInformation" :: NullOrUndefined (OwnerInformation), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "ClientToken" :: NullOrUndefined (ClientToken) }
```

#### `RegisterTargetWithMaintenanceWindowResult`

``` purescript
newtype RegisterTargetWithMaintenanceWindowResult
  = RegisterTargetWithMaintenanceWindowResult { "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId) }
```

#### `RegisterTaskWithMaintenanceWindowRequest`

``` purescript
newtype RegisterTaskWithMaintenanceWindowRequest
  = RegisterTaskWithMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "Targets" :: Targets, "TaskArn" :: MaintenanceWindowTaskArn, "ServiceRoleArn" :: ServiceRole, "TaskType" :: MaintenanceWindowTaskType, "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: MaxConcurrency, "MaxErrors" :: MaxErrors, "LoggingInfo" :: NullOrUndefined (LoggingInfo), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "ClientToken" :: NullOrUndefined (ClientToken) }
```

#### `RegisterTaskWithMaintenanceWindowResult`

``` purescript
newtype RegisterTaskWithMaintenanceWindowResult
  = RegisterTaskWithMaintenanceWindowResult { "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId) }
```

#### `RegistrationLimit`

``` purescript
newtype RegistrationLimit
  = RegistrationLimit Int
```

#### `RegistrationsCount`

``` purescript
newtype RegistrationsCount
  = RegistrationsCount Int
```

#### `RemoveTagsFromResourceRequest`

``` purescript
newtype RemoveTagsFromResourceRequest
  = RemoveTagsFromResourceRequest { "ResourceType" :: ResourceTypeForTagging, "ResourceId" :: ResourceId, "TagKeys" :: KeyList }
```

#### `RemoveTagsFromResourceResult`

``` purescript
newtype RemoveTagsFromResourceResult
  = RemoveTagsFromResourceResult {  }
```

#### `ResolvedTargets`

``` purescript
newtype ResolvedTargets
  = ResolvedTargets { "ParameterValues" :: NullOrUndefined (TargetParameterList), "Truncated" :: NullOrUndefined (Boolean) }
```

<p>Information about targets that resolved during the Automation execution.</p>

#### `ResourceComplianceSummaryItem`

``` purescript
newtype ResourceComplianceSummaryItem
  = ResourceComplianceSummaryItem { "ComplianceType" :: NullOrUndefined (ComplianceTypeName), "ResourceType" :: NullOrUndefined (ComplianceResourceType), "ResourceId" :: NullOrUndefined (ComplianceResourceId), "Status" :: NullOrUndefined (ComplianceStatus), "OverallSeverity" :: NullOrUndefined (ComplianceSeverity), "ExecutionSummary" :: NullOrUndefined (ComplianceExecutionSummary), "CompliantSummary" :: NullOrUndefined (CompliantSummary), "NonCompliantSummary" :: NullOrUndefined (NonCompliantSummary) }
```

<p>Compliance summary information for a specific resource. </p>

#### `ResourceComplianceSummaryItemList`

``` purescript
newtype ResourceComplianceSummaryItemList
  = ResourceComplianceSummaryItemList (Array ResourceComplianceSummaryItem)
```

#### `ResourceDataSyncAWSKMSKeyARN`

``` purescript
newtype ResourceDataSyncAWSKMSKeyARN
  = ResourceDataSyncAWSKMSKeyARN String
```

#### `ResourceDataSyncAlreadyExistsException`

``` purescript
newtype ResourceDataSyncAlreadyExistsException
  = ResourceDataSyncAlreadyExistsException { "SyncName" :: NullOrUndefined (ResourceDataSyncName) }
```

<p>A sync configuration with the same name already exists.</p>

#### `ResourceDataSyncCountExceededException`

``` purescript
newtype ResourceDataSyncCountExceededException
  = ResourceDataSyncCountExceededException { "Message" :: NullOrUndefined (String) }
```

<p>You have exceeded the allowed maximum sync configurations.</p>

#### `ResourceDataSyncCreatedTime`

``` purescript
newtype ResourceDataSyncCreatedTime
  = ResourceDataSyncCreatedTime Number
```

#### `ResourceDataSyncInvalidConfigurationException`

``` purescript
newtype ResourceDataSyncInvalidConfigurationException
  = ResourceDataSyncInvalidConfigurationException { "Message" :: NullOrUndefined (String) }
```

<p>The specified sync configuration is invalid.</p>

#### `ResourceDataSyncItem`

``` purescript
newtype ResourceDataSyncItem
  = ResourceDataSyncItem { "SyncName" :: NullOrUndefined (ResourceDataSyncName), "S3Destination" :: NullOrUndefined (ResourceDataSyncS3Destination), "LastSyncTime" :: NullOrUndefined (LastResourceDataSyncTime), "LastSuccessfulSyncTime" :: NullOrUndefined (LastSuccessfulResourceDataSyncTime), "LastStatus" :: NullOrUndefined (LastResourceDataSyncStatus), "SyncCreatedTime" :: NullOrUndefined (ResourceDataSyncCreatedTime) }
```

<p>Information about a Resource Data Sync configuration, including its current status and last successful sync.</p>

#### `ResourceDataSyncItemList`

``` purescript
newtype ResourceDataSyncItemList
  = ResourceDataSyncItemList (Array ResourceDataSyncItem)
```

#### `ResourceDataSyncName`

``` purescript
newtype ResourceDataSyncName
  = ResourceDataSyncName String
```

#### `ResourceDataSyncNotFoundException`

``` purescript
newtype ResourceDataSyncNotFoundException
  = ResourceDataSyncNotFoundException { "SyncName" :: NullOrUndefined (ResourceDataSyncName) }
```

<p>The specified sync name was not found.</p>

#### `ResourceDataSyncS3BucketName`

``` purescript
newtype ResourceDataSyncS3BucketName
  = ResourceDataSyncS3BucketName String
```

#### `ResourceDataSyncS3Destination`

``` purescript
newtype ResourceDataSyncS3Destination
  = ResourceDataSyncS3Destination { "BucketName" :: ResourceDataSyncS3BucketName, "Prefix" :: NullOrUndefined (ResourceDataSyncS3Prefix), "SyncFormat" :: ResourceDataSyncS3Format, "Region" :: ResourceDataSyncS3Region, "AWSKMSKeyARN" :: NullOrUndefined (ResourceDataSyncAWSKMSKeyARN) }
```

<p>Information about the target Amazon S3 bucket for the Resource Data Sync.</p>

#### `ResourceDataSyncS3Format`

``` purescript
newtype ResourceDataSyncS3Format
  = ResourceDataSyncS3Format String
```

#### `ResourceDataSyncS3Prefix`

``` purescript
newtype ResourceDataSyncS3Prefix
  = ResourceDataSyncS3Prefix String
```

#### `ResourceDataSyncS3Region`

``` purescript
newtype ResourceDataSyncS3Region
  = ResourceDataSyncS3Region String
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message" :: NullOrUndefined (String) }
```

<p>Error returned if an attempt is made to delete a patch baseline that is registered for a patch group.</p>

#### `ResourceLimitExceededException`

``` purescript
newtype ResourceLimitExceededException
  = ResourceLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>Error returned when the caller has exceeded the default resource limits. For example, too many Maintenance Windows or Patch baselines have been created.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

#### `ResourceTypeForTagging`

``` purescript
newtype ResourceTypeForTagging
  = ResourceTypeForTagging String
```

#### `ResponseCode`

``` purescript
newtype ResponseCode
  = ResponseCode Int
```

#### `ResultAttribute`

``` purescript
newtype ResultAttribute
  = ResultAttribute { "TypeName" :: InventoryItemTypeName }
```

<p>The inventory item result attribute.</p>

#### `ResultAttributeList`

``` purescript
newtype ResultAttributeList
  = ResultAttributeList (Array ResultAttribute)
```

#### `S3BucketName`

``` purescript
newtype S3BucketName
  = S3BucketName String
```

#### `S3KeyPrefix`

``` purescript
newtype S3KeyPrefix
  = S3KeyPrefix String
```

#### `S3OutputLocation`

``` purescript
newtype S3OutputLocation
  = S3OutputLocation { "OutputS3Region" :: NullOrUndefined (S3Region), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix) }
```

<p>An Amazon S3 bucket where you want to store the results of this request.</p>

#### `S3OutputUrl`

``` purescript
newtype S3OutputUrl
  = S3OutputUrl { "OutputUrl" :: NullOrUndefined (Url) }
```

<p>A URL for the Amazon S3 bucket where you want to store the results of this request.</p>

#### `S3Region`

``` purescript
newtype S3Region
  = S3Region String
```

#### `ScheduleExpression`

``` purescript
newtype ScheduleExpression
  = ScheduleExpression String
```

#### `SendAutomationSignalRequest`

``` purescript
newtype SendAutomationSignalRequest
  = SendAutomationSignalRequest { "AutomationExecutionId" :: AutomationExecutionId, "SignalType" :: SignalType, "Payload" :: NullOrUndefined (AutomationParameterMap) }
```

#### `SendAutomationSignalResult`

``` purescript
newtype SendAutomationSignalResult
  = SendAutomationSignalResult {  }
```

#### `SendCommandRequest`

``` purescript
newtype SendCommandRequest
  = SendCommandRequest { "InstanceIds" :: NullOrUndefined (InstanceIdList), "Targets" :: NullOrUndefined (Targets), "DocumentName" :: DocumentARN, "DocumentHash" :: NullOrUndefined (DocumentHash), "DocumentHashType" :: NullOrUndefined (DocumentHashType), "TimeoutSeconds" :: NullOrUndefined (TimeoutSeconds), "Comment" :: NullOrUndefined (Comment), "Parameters" :: NullOrUndefined (Parameters), "OutputS3Region" :: NullOrUndefined (S3Region), "OutputS3BucketName" :: NullOrUndefined (S3BucketName), "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "NotificationConfig" :: NullOrUndefined (NotificationConfig) }
```

#### `SendCommandResult`

``` purescript
newtype SendCommandResult
  = SendCommandResult { "Command" :: NullOrUndefined (Command) }
```

#### `ServiceRole`

``` purescript
newtype ServiceRole
  = ServiceRole String
```

#### `SeveritySummary`

``` purescript
newtype SeveritySummary
  = SeveritySummary { "CriticalCount" :: NullOrUndefined (ComplianceSummaryCount), "HighCount" :: NullOrUndefined (ComplianceSummaryCount), "MediumCount" :: NullOrUndefined (ComplianceSummaryCount), "LowCount" :: NullOrUndefined (ComplianceSummaryCount), "InformationalCount" :: NullOrUndefined (ComplianceSummaryCount), "UnspecifiedCount" :: NullOrUndefined (ComplianceSummaryCount) }
```

<p>The number of managed instances found for each patch severity level defined in the request filter.</p>

#### `SignalType`

``` purescript
newtype SignalType
  = SignalType String
```

#### `SnapshotDownloadUrl`

``` purescript
newtype SnapshotDownloadUrl
  = SnapshotDownloadUrl String
```

#### `SnapshotId`

``` purescript
newtype SnapshotId
  = SnapshotId String
```

#### `StandardErrorContent`

``` purescript
newtype StandardErrorContent
  = StandardErrorContent String
```

#### `StandardOutputContent`

``` purescript
newtype StandardOutputContent
  = StandardOutputContent String
```

#### `StartAutomationExecutionRequest`

``` purescript
newtype StartAutomationExecutionRequest
  = StartAutomationExecutionRequest { "DocumentName" :: DocumentARN, "DocumentVersion" :: NullOrUndefined (DocumentVersion), "Parameters" :: NullOrUndefined (AutomationParameterMap), "ClientToken" :: NullOrUndefined (IdempotencyToken), "Mode" :: NullOrUndefined (ExecutionMode), "TargetParameterName" :: NullOrUndefined (AutomationParameterKey), "Targets" :: NullOrUndefined (Targets), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors) }
```

#### `StartAutomationExecutionResult`

``` purescript
newtype StartAutomationExecutionResult
  = StartAutomationExecutionResult { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId) }
```

#### `StatusAdditionalInfo`

``` purescript
newtype StatusAdditionalInfo
  = StatusAdditionalInfo String
```

#### `StatusDetails`

``` purescript
newtype StatusDetails
  = StatusDetails String
```

#### `StatusMessage`

``` purescript
newtype StatusMessage
  = StatusMessage String
```

#### `StatusName`

``` purescript
newtype StatusName
  = StatusName String
```

#### `StatusUnchanged`

``` purescript
newtype StatusUnchanged
  = StatusUnchanged {  }
```

<p>The updated status is the same as the current status.</p>

#### `StepExecution`

``` purescript
newtype StepExecution
  = StepExecution { "StepName" :: NullOrUndefined (String), "Action" :: NullOrUndefined (AutomationActionName), "TimeoutSeconds" :: NullOrUndefined (Number), "OnFailure" :: NullOrUndefined (String), "MaxAttempts" :: NullOrUndefined (Int), "ExecutionStartTime" :: NullOrUndefined (DateTime), "ExecutionEndTime" :: NullOrUndefined (DateTime), "StepStatus" :: NullOrUndefined (AutomationExecutionStatus), "ResponseCode" :: NullOrUndefined (String), "Inputs" :: NullOrUndefined (NormalStringMap), "Outputs" :: NullOrUndefined (AutomationParameterMap), "Response" :: NullOrUndefined (String), "FailureMessage" :: NullOrUndefined (String), "FailureDetails" :: NullOrUndefined (FailureDetails), "StepExecutionId" :: NullOrUndefined (String), "OverriddenParameters" :: NullOrUndefined (AutomationParameterMap) }
```

<p>Detailed information about an the execution state of an Automation step.</p>

#### `StepExecutionFilter`

``` purescript
newtype StepExecutionFilter
  = StepExecutionFilter { "Key" :: StepExecutionFilterKey, "Values" :: StepExecutionFilterValueList }
```

<p>A filter to limit the amount of step execution information returned by the call.</p>

#### `StepExecutionFilterKey`

``` purescript
newtype StepExecutionFilterKey
  = StepExecutionFilterKey String
```

#### `StepExecutionFilterList`

``` purescript
newtype StepExecutionFilterList
  = StepExecutionFilterList (Array StepExecutionFilter)
```

#### `StepExecutionFilterValue`

``` purescript
newtype StepExecutionFilterValue
  = StepExecutionFilterValue String
```

#### `StepExecutionFilterValueList`

``` purescript
newtype StepExecutionFilterValueList
  = StepExecutionFilterValueList (Array StepExecutionFilterValue)
```

#### `StepExecutionList`

``` purescript
newtype StepExecutionList
  = StepExecutionList (Array StepExecution)
```

#### `StopAutomationExecutionRequest`

``` purescript
newtype StopAutomationExecutionRequest
  = StopAutomationExecutionRequest { "AutomationExecutionId" :: AutomationExecutionId, "Type" :: NullOrUndefined (StopType) }
```

#### `StopAutomationExecutionResult`

``` purescript
newtype StopAutomationExecutionResult
  = StopAutomationExecutionResult {  }
```

#### `StopType`

``` purescript
newtype StopType
  = StopType String
```

#### `StringDateTime`

``` purescript
newtype StringDateTime
  = StringDateTime String
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

#### `SubTypeCountLimitExceededException`

``` purescript
newtype SubTypeCountLimitExceededException
  = SubTypeCountLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The sub-type count exceeded the limit for the inventory type.</p>

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>Metadata that you assign to your AWS resources. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. In Systems Manager, you can apply tags to documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines.</p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `Target`

``` purescript
newtype Target
  = Target { "Key" :: NullOrUndefined (TargetKey), "Values" :: NullOrUndefined (TargetValues) }
```

<p>An array of search criteria that targets instances using a Key,Value combination that you specify. <code>Targets</code> is required if you don't provide one or more instance IDs in the call.</p> <p/>

#### `TargetCount`

``` purescript
newtype TargetCount
  = TargetCount Int
```

#### `TargetInUseException`

``` purescript
newtype TargetInUseException
  = TargetInUseException { "Message" :: NullOrUndefined (String) }
```

<p>You specified the <code>Safe</code> option for the DeregisterTargetFromMaintenanceWindow operation, but the target is still referenced in a task.</p>

#### `TargetKey`

``` purescript
newtype TargetKey
  = TargetKey String
```

#### `TargetParameterList`

``` purescript
newtype TargetParameterList
  = TargetParameterList (Array ParameterValue)
```

#### `TargetType`

``` purescript
newtype TargetType
  = TargetType String
```

#### `TargetValue`

``` purescript
newtype TargetValue
  = TargetValue String
```

#### `TargetValues`

``` purescript
newtype TargetValues
  = TargetValues (Array TargetValue)
```

#### `Targets`

``` purescript
newtype Targets
  = Targets (Array Target)
```

#### `TimeoutSeconds`

``` purescript
newtype TimeoutSeconds
  = TimeoutSeconds Int
```

#### `TooManyTagsError`

``` purescript
newtype TooManyTagsError
  = TooManyTagsError {  }
```

<p>The Targets parameter includes too many tags. Remove one or more tags and try the command again.</p>

#### `TooManyUpdates`

``` purescript
newtype TooManyUpdates
  = TooManyUpdates { "Message" :: NullOrUndefined (String) }
```

<p>There are concurrent updates for a resource that supports one update at a time.</p>

#### `TotalSizeLimitExceededException`

``` purescript
newtype TotalSizeLimitExceededException
  = TotalSizeLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The size of inventory data has exceeded the total size limit for the resource.</p>

#### `UnsupportedInventoryItemContextException`

``` purescript
newtype UnsupportedInventoryItemContextException
  = UnsupportedInventoryItemContextException { "TypeName" :: NullOrUndefined (InventoryItemTypeName), "Message" :: NullOrUndefined (String) }
```

<p>The <code>Context</code> attribute that you specified for the <code>InventoryItem</code> is not allowed for this inventory type. You can only use the <code>Context</code> attribute with inventory types like <code>AWS:ComplianceItem</code>.</p>

#### `UnsupportedInventorySchemaVersionException`

``` purescript
newtype UnsupportedInventorySchemaVersionException
  = UnsupportedInventorySchemaVersionException { "Message" :: NullOrUndefined (String) }
```

<p>Inventory item type schema version has to match supported versions in the service. Check output of GetInventorySchema to see the available schema version for each type.</p>

#### `UnsupportedOperatingSystem`

``` purescript
newtype UnsupportedOperatingSystem
  = UnsupportedOperatingSystem { "Message" :: NullOrUndefined (String) }
```

<p>The operating systems you specified is not supported, or the operation is not supported for the operating system. Valid operating systems include: Windows, AmazonLinux, RedhatEnterpriseLinux, and Ubuntu.</p>

#### `UnsupportedParameterType`

``` purescript
newtype UnsupportedParameterType
  = UnsupportedParameterType { "Message'" :: NullOrUndefined (String) }
```

<p>The parameter type is not supported.</p>

#### `UnsupportedPlatformType`

``` purescript
newtype UnsupportedPlatformType
  = UnsupportedPlatformType { "Message" :: NullOrUndefined (String) }
```

<p>The document does not support the platform type of the given instance ID(s). For example, you sent an document for a Windows instance to a Linux instance.</p>

#### `UpdateAssociationRequest`

``` purescript
newtype UpdateAssociationRequest
  = UpdateAssociationRequest { "AssociationId" :: AssociationId, "Parameters" :: NullOrUndefined (Parameters), "DocumentVersion" :: NullOrUndefined (DocumentVersion), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation), "Name" :: NullOrUndefined (DocumentName), "Targets" :: NullOrUndefined (Targets), "AssociationName" :: NullOrUndefined (AssociationName), "AssociationVersion" :: NullOrUndefined (AssociationVersion) }
```

#### `UpdateAssociationResult`

``` purescript
newtype UpdateAssociationResult
  = UpdateAssociationResult { "AssociationDescription" :: NullOrUndefined (AssociationDescription) }
```

#### `UpdateAssociationStatusRequest`

``` purescript
newtype UpdateAssociationStatusRequest
  = UpdateAssociationStatusRequest { "Name" :: DocumentName, "InstanceId" :: InstanceId, "AssociationStatus" :: AssociationStatus }
```

#### `UpdateAssociationStatusResult`

``` purescript
newtype UpdateAssociationStatusResult
  = UpdateAssociationStatusResult { "AssociationDescription" :: NullOrUndefined (AssociationDescription) }
```

#### `UpdateDocumentDefaultVersionRequest`

``` purescript
newtype UpdateDocumentDefaultVersionRequest
  = UpdateDocumentDefaultVersionRequest { "Name" :: DocumentName, "DocumentVersion" :: DocumentVersionNumber }
```

#### `UpdateDocumentDefaultVersionResult`

``` purescript
newtype UpdateDocumentDefaultVersionResult
  = UpdateDocumentDefaultVersionResult { "Description" :: NullOrUndefined (DocumentDefaultVersionDescription) }
```

#### `UpdateDocumentRequest`

``` purescript
newtype UpdateDocumentRequest
  = UpdateDocumentRequest { "Content" :: DocumentContent, "Name" :: DocumentName, "DocumentVersion" :: NullOrUndefined (DocumentVersion), "DocumentFormat" :: NullOrUndefined (DocumentFormat), "TargetType" :: NullOrUndefined (TargetType) }
```

#### `UpdateDocumentResult`

``` purescript
newtype UpdateDocumentResult
  = UpdateDocumentResult { "DocumentDescription" :: NullOrUndefined (DocumentDescription) }
```

#### `UpdateMaintenanceWindowRequest`

``` purescript
newtype UpdateMaintenanceWindowRequest
  = UpdateMaintenanceWindowRequest { "WindowId" :: MaintenanceWindowId, "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Schedule" :: NullOrUndefined (MaintenanceWindowSchedule), "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours), "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff), "AllowUnassociatedTargets" :: NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets), "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled), "Replace" :: NullOrUndefined (Boolean) }
```

#### `UpdateMaintenanceWindowResult`

``` purescript
newtype UpdateMaintenanceWindowResult
  = UpdateMaintenanceWindowResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Schedule" :: NullOrUndefined (MaintenanceWindowSchedule), "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours), "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff), "AllowUnassociatedTargets" :: NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets), "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled) }
```

#### `UpdateMaintenanceWindowTargetRequest`

``` purescript
newtype UpdateMaintenanceWindowTargetRequest
  = UpdateMaintenanceWindowTargetRequest { "WindowId" :: MaintenanceWindowId, "WindowTargetId" :: MaintenanceWindowTargetId, "Targets" :: NullOrUndefined (Targets), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Replace" :: NullOrUndefined (Boolean) }
```

#### `UpdateMaintenanceWindowTargetResult`

``` purescript
newtype UpdateMaintenanceWindowTargetResult
  = UpdateMaintenanceWindowTargetResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId), "Targets" :: NullOrUndefined (Targets), "OwnerInformation" :: NullOrUndefined (OwnerInformation), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

#### `UpdateMaintenanceWindowTaskRequest`

``` purescript
newtype UpdateMaintenanceWindowTaskRequest
  = UpdateMaintenanceWindowTaskRequest { "WindowId" :: MaintenanceWindowId, "WindowTaskId" :: MaintenanceWindowTaskId, "Targets" :: NullOrUndefined (Targets), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "LoggingInfo" :: NullOrUndefined (LoggingInfo), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription), "Replace" :: NullOrUndefined (Boolean) }
```

#### `UpdateMaintenanceWindowTaskResult`

``` purescript
newtype UpdateMaintenanceWindowTaskResult
  = UpdateMaintenanceWindowTaskResult { "WindowId" :: NullOrUndefined (MaintenanceWindowId), "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId), "Targets" :: NullOrUndefined (Targets), "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn), "ServiceRoleArn" :: NullOrUndefined (ServiceRole), "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters), "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters), "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority), "MaxConcurrency" :: NullOrUndefined (MaxConcurrency), "MaxErrors" :: NullOrUndefined (MaxErrors), "LoggingInfo" :: NullOrUndefined (LoggingInfo), "Name" :: NullOrUndefined (MaintenanceWindowName), "Description" :: NullOrUndefined (MaintenanceWindowDescription) }
```

#### `UpdateManagedInstanceRoleRequest`

``` purescript
newtype UpdateManagedInstanceRoleRequest
  = UpdateManagedInstanceRoleRequest { "InstanceId" :: ManagedInstanceId, "IamRole" :: IamRole }
```

#### `UpdateManagedInstanceRoleResult`

``` purescript
newtype UpdateManagedInstanceRoleResult
  = UpdateManagedInstanceRoleResult {  }
```

#### `UpdatePatchBaselineRequest`

``` purescript
newtype UpdatePatchBaselineRequest
  = UpdatePatchBaselineRequest { "BaselineId" :: BaselineId, "Name" :: NullOrUndefined (BaselineName), "GlobalFilters" :: NullOrUndefined (PatchFilterGroup), "ApprovalRules" :: NullOrUndefined (PatchRuleGroup), "ApprovedPatches" :: NullOrUndefined (PatchIdList), "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean), "RejectedPatches" :: NullOrUndefined (PatchIdList), "Description" :: NullOrUndefined (BaselineDescription), "Sources" :: NullOrUndefined (PatchSourceList), "Replace" :: NullOrUndefined (Boolean) }
```

#### `UpdatePatchBaselineResult`

``` purescript
newtype UpdatePatchBaselineResult
  = UpdatePatchBaselineResult { "BaselineId" :: NullOrUndefined (BaselineId), "Name" :: NullOrUndefined (BaselineName), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "GlobalFilters" :: NullOrUndefined (PatchFilterGroup), "ApprovalRules" :: NullOrUndefined (PatchRuleGroup), "ApprovedPatches" :: NullOrUndefined (PatchIdList), "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel), "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean), "RejectedPatches" :: NullOrUndefined (PatchIdList), "CreatedDate" :: NullOrUndefined (DateTime), "ModifiedDate" :: NullOrUndefined (DateTime), "Description" :: NullOrUndefined (BaselineDescription), "Sources" :: NullOrUndefined (PatchSourceList) }
```

#### `Url`

``` purescript
newtype Url
  = Url String
```

#### `Version`

``` purescript
newtype Version
  = Version String
```


