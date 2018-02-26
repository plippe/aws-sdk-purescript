

-- | <fullname>AWS Systems Manager</fullname> <p>AWS Systems Manager is a collection of capabilities that helps you automate management tasks such as collecting system inventory, applying operating system (OS) patches, automating the creation of Amazon Machine Images (AMIs), and configuring operating systems (OSs) and applications at scale. Systems Manager lets you remotely and securely manage the configuration of your managed instances. A <i>managed instance</i> is any Amazon EC2 instance or on-premises machine in your hybrid environment that has been configured for Systems Manager.</p> <p>This reference is intended to be used with the <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/">AWS Systems Manager User Guide</a>.</p> <p>To get started, verify prerequisites and configure managed instances. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-setting-up.html">Systems Manager Prerequisites</a>.</p> <p>For information about other API actions you can perform on Amazon EC2 instances, see the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/">Amazon EC2 API Reference</a>. For information about how to use a Query API, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/making-api-requests.html">Making API Requests</a>. </p>
module AWS.SSM where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "SSM" :: String


-- | <p>Adds or overwrites one or more tags for the specified resource. Tags are metadata that you can assign to your documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. Each tag consists of a key and an optional value, both of which you define. For example, you could define a set of tags for your account's managed instances that helps you track each instance's owner and stack level. For example: Key=Owner and Value=DbAdmin, SysAdmin, or Dev. Or Key=Stack and Value=Production, Pre-Production, or Test.</p> <p>Each resource can have a maximum of 50 tags. </p> <p>We recommend that you devise a set of tag keys that meets your needs for each resource type. Using a consistent set of tag keys makes it easier for you to manage your resources. You can search and filter the resources based on the tags you add. Tags don't have any semantic meaning to Amazon EC2 and are interpreted strictly as a string of characters. </p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html">Tagging Your Amazon EC2 Resources</a> in the <i>Amazon EC2 User Guide</i>.</p>
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (err :: AWS.RequestError | eff) AddTagsToResourceResult
addTagsToResource = AWS.request serviceName "AddTagsToResource" 


-- | <p>Attempts to cancel the command specified by the Command ID. There is no guarantee that the command will be terminated and the underlying process stopped.</p>
cancelCommand :: forall eff. CancelCommandRequest -> Aff (err :: AWS.RequestError | eff) CancelCommandResult
cancelCommand = AWS.request serviceName "CancelCommand" 


-- | <p>Registers your on-premises server or virtual machine with Amazon EC2 so that you can manage these resources using Run Command. An on-premises server or virtual machine that has been registered with EC2 is called a managed instance. For more information about activations, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances.html">Setting Up Systems Manager in Hybrid Environments</a>.</p>
createActivation :: forall eff. CreateActivationRequest -> Aff (err :: AWS.RequestError | eff) CreateActivationResult
createActivation = AWS.request serviceName "CreateActivation" 


-- | <p>Associates the specified Systems Manager document with the specified instances or targets.</p> <p>When you associate a document with one or more instances using instance IDs or tags, the SSM Agent running on the instance processes the document and configures the instance as specified.</p> <p>If you associate a document with an instance that already has an associated document, the system throws the AssociationAlreadyExists exception.</p>
createAssociation :: forall eff. CreateAssociationRequest -> Aff (err :: AWS.RequestError | eff) CreateAssociationResult
createAssociation = AWS.request serviceName "CreateAssociation" 


-- | <p>Associates the specified Systems Manager document with the specified instances or targets.</p> <p>When you associate a document with one or more instances using instance IDs or tags, the SSM Agent running on the instance processes the document and configures the instance as specified.</p> <p>If you associate a document with an instance that already has an associated document, the system throws the AssociationAlreadyExists exception.</p>
createAssociationBatch :: forall eff. CreateAssociationBatchRequest -> Aff (err :: AWS.RequestError | eff) CreateAssociationBatchResult
createAssociationBatch = AWS.request serviceName "CreateAssociationBatch" 


-- | <p>Creates a Systems Manager document.</p> <p>After you create a document, you can use CreateAssociation to associate it with one or more running instances.</p>
createDocument :: forall eff. CreateDocumentRequest -> Aff (err :: AWS.RequestError | eff) CreateDocumentResult
createDocument = AWS.request serviceName "CreateDocument" 


-- | <p>Creates a new Maintenance Window.</p>
createMaintenanceWindow :: forall eff. CreateMaintenanceWindowRequest -> Aff (err :: AWS.RequestError | eff) CreateMaintenanceWindowResult
createMaintenanceWindow = AWS.request serviceName "CreateMaintenanceWindow" 


-- | <p>Creates a patch baseline.</p> <note> <p>For information about valid key and value pairs in <code>PatchFilters</code> for each supported operating system type, see <a href="http://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PatchFilter.html">PatchFilter</a>.</p> </note>
createPatchBaseline :: forall eff. CreatePatchBaselineRequest -> Aff (err :: AWS.RequestError | eff) CreatePatchBaselineResult
createPatchBaseline = AWS.request serviceName "CreatePatchBaseline" 


-- | <p>Creates a resource data sync configuration to a single bucket in Amazon S3. This is an asynchronous operation that returns immediately. After a successful initial sync is completed, the system continuously syncs data to the Amazon S3 bucket. To check the status of the sync, use the <a>ListResourceDataSync</a>.</p> <p>By default, data is not encrypted in Amazon S3. We strongly recommend that you enable encryption in Amazon S3 to ensure secure data storage. We also recommend that you secure access to the Amazon S3 bucket by creating a restrictive bucket policy. To view an example of a restrictive Amazon S3 bucket policy for Resource Data Sync, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-configuring.html#sysman-inventory-datasync">Configuring Resource Data Sync for Inventory</a>.</p>
createResourceDataSync :: forall eff. CreateResourceDataSyncRequest -> Aff (err :: AWS.RequestError | eff) CreateResourceDataSyncResult
createResourceDataSync = AWS.request serviceName "CreateResourceDataSync" 


-- | <p>Deletes an activation. You are not required to delete an activation. If you delete an activation, you can no longer use it to register additional managed instances. Deleting an activation does not de-register managed instances. You must manually de-register managed instances.</p>
deleteActivation :: forall eff. DeleteActivationRequest -> Aff (err :: AWS.RequestError | eff) DeleteActivationResult
deleteActivation = AWS.request serviceName "DeleteActivation" 


-- | <p>Disassociates the specified Systems Manager document from the specified instance.</p> <p>When you disassociate a document from an instance, it does not change the configuration of the instance. To change the configuration state of an instance after you disassociate a document, you must create a new document with the desired configuration and associate it with the instance.</p>
deleteAssociation :: forall eff. DeleteAssociationRequest -> Aff (err :: AWS.RequestError | eff) DeleteAssociationResult
deleteAssociation = AWS.request serviceName "DeleteAssociation" 


-- | <p>Deletes the Systems Manager document and all instance associations to the document.</p> <p>Before you delete the document, we recommend that you use <a>DeleteAssociation</a> to disassociate all instances that are associated with the document.</p>
deleteDocument :: forall eff. DeleteDocumentRequest -> Aff (err :: AWS.RequestError | eff) DeleteDocumentResult
deleteDocument = AWS.request serviceName "DeleteDocument" 


-- | <p>Deletes a Maintenance Window.</p>
deleteMaintenanceWindow :: forall eff. DeleteMaintenanceWindowRequest -> Aff (err :: AWS.RequestError | eff) DeleteMaintenanceWindowResult
deleteMaintenanceWindow = AWS.request serviceName "DeleteMaintenanceWindow" 


-- | <p>Delete a parameter from the system.</p>
deleteParameter :: forall eff. DeleteParameterRequest -> Aff (err :: AWS.RequestError | eff) DeleteParameterResult
deleteParameter = AWS.request serviceName "DeleteParameter" 


-- | <p>Delete a list of parameters. This API is used to delete parameters by using the Amazon EC2 console.</p>
deleteParameters :: forall eff. DeleteParametersRequest -> Aff (err :: AWS.RequestError | eff) DeleteParametersResult
deleteParameters = AWS.request serviceName "DeleteParameters" 


-- | <p>Deletes a patch baseline.</p>
deletePatchBaseline :: forall eff. DeletePatchBaselineRequest -> Aff (err :: AWS.RequestError | eff) DeletePatchBaselineResult
deletePatchBaseline = AWS.request serviceName "DeletePatchBaseline" 


-- | <p>Deletes a Resource Data Sync configuration. After the configuration is deleted, changes to inventory data on managed instances are no longer synced with the target Amazon S3 bucket. Deleting a sync configuration does not delete data in the target Amazon S3 bucket.</p>
deleteResourceDataSync :: forall eff. DeleteResourceDataSyncRequest -> Aff (err :: AWS.RequestError | eff) DeleteResourceDataSyncResult
deleteResourceDataSync = AWS.request serviceName "DeleteResourceDataSync" 


-- | <p>Removes the server or virtual machine from the list of registered servers. You can reregister the instance again at any time. If you don't plan to use Run Command on the server, we suggest uninstalling the SSM Agent first.</p>
deregisterManagedInstance :: forall eff. DeregisterManagedInstanceRequest -> Aff (err :: AWS.RequestError | eff) DeregisterManagedInstanceResult
deregisterManagedInstance = AWS.request serviceName "DeregisterManagedInstance" 


-- | <p>Removes a patch group from a patch baseline.</p>
deregisterPatchBaselineForPatchGroup :: forall eff. DeregisterPatchBaselineForPatchGroupRequest -> Aff (err :: AWS.RequestError | eff) DeregisterPatchBaselineForPatchGroupResult
deregisterPatchBaselineForPatchGroup = AWS.request serviceName "DeregisterPatchBaselineForPatchGroup" 


-- | <p>Removes a target from a Maintenance Window.</p>
deregisterTargetFromMaintenanceWindow :: forall eff. DeregisterTargetFromMaintenanceWindowRequest -> Aff (err :: AWS.RequestError | eff) DeregisterTargetFromMaintenanceWindowResult
deregisterTargetFromMaintenanceWindow = AWS.request serviceName "DeregisterTargetFromMaintenanceWindow" 


-- | <p>Removes a task from a Maintenance Window.</p>
deregisterTaskFromMaintenanceWindow :: forall eff. DeregisterTaskFromMaintenanceWindowRequest -> Aff (err :: AWS.RequestError | eff) DeregisterTaskFromMaintenanceWindowResult
deregisterTaskFromMaintenanceWindow = AWS.request serviceName "DeregisterTaskFromMaintenanceWindow" 


-- | <p>Details about the activation, including: the date and time the activation was created, the expiration date, the IAM role assigned to the instances in the activation, and the number of instances activated by this registration.</p>
describeActivations :: forall eff. DescribeActivationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeActivationsResult
describeActivations = AWS.request serviceName "DescribeActivations" 


-- | <p>Describes the association for the specified target or instance. If you created the association by using the <code>Targets</code> parameter, then you must retrieve the association by using the association ID. If you created the association by specifying an instance ID and a Systems Manager document, then you retrieve the association by specifying the document name and the instance ID. </p>
describeAssociation :: forall eff. DescribeAssociationRequest -> Aff (err :: AWS.RequestError | eff) DescribeAssociationResult
describeAssociation = AWS.request serviceName "DescribeAssociation" 


-- | <p>Provides details about all active and terminated Automation executions.</p>
describeAutomationExecutions :: forall eff. DescribeAutomationExecutionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeAutomationExecutionsResult
describeAutomationExecutions = AWS.request serviceName "DescribeAutomationExecutions" 


-- | <p>Information about all active and terminated step executions in an Automation workflow.</p>
describeAutomationStepExecutions :: forall eff. DescribeAutomationStepExecutionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeAutomationStepExecutionsResult
describeAutomationStepExecutions = AWS.request serviceName "DescribeAutomationStepExecutions" 


-- | <p>Lists all patches that could possibly be included in a patch baseline.</p>
describeAvailablePatches :: forall eff. DescribeAvailablePatchesRequest -> Aff (err :: AWS.RequestError | eff) DescribeAvailablePatchesResult
describeAvailablePatches = AWS.request serviceName "DescribeAvailablePatches" 


-- | <p>Describes the specified Systems Manager document.</p>
describeDocument :: forall eff. DescribeDocumentRequest -> Aff (err :: AWS.RequestError | eff) DescribeDocumentResult
describeDocument = AWS.request serviceName "DescribeDocument" 


-- | <p>Describes the permissions for a Systems Manager document. If you created the document, you are the owner. If a document is shared, it can either be shared privately (by specifying a user's AWS account ID) or publicly (<i>All</i>). </p>
describeDocumentPermission :: forall eff. DescribeDocumentPermissionRequest -> Aff (err :: AWS.RequestError | eff) DescribeDocumentPermissionResponse
describeDocumentPermission = AWS.request serviceName "DescribeDocumentPermission" 


-- | <p>All associations for the instance(s).</p>
describeEffectiveInstanceAssociations :: forall eff. DescribeEffectiveInstanceAssociationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEffectiveInstanceAssociationsResult
describeEffectiveInstanceAssociations = AWS.request serviceName "DescribeEffectiveInstanceAssociations" 


-- | <p>Retrieves the current effective patches (the patch and the approval state) for the specified patch baseline. Note that this API applies only to Windows patch baselines.</p>
describeEffectivePatchesForPatchBaseline :: forall eff. DescribeEffectivePatchesForPatchBaselineRequest -> Aff (err :: AWS.RequestError | eff) DescribeEffectivePatchesForPatchBaselineResult
describeEffectivePatchesForPatchBaseline = AWS.request serviceName "DescribeEffectivePatchesForPatchBaseline" 


-- | <p>The status of the associations for the instance(s).</p>
describeInstanceAssociationsStatus :: forall eff. DescribeInstanceAssociationsStatusRequest -> Aff (err :: AWS.RequestError | eff) DescribeInstanceAssociationsStatusResult
describeInstanceAssociationsStatus = AWS.request serviceName "DescribeInstanceAssociationsStatus" 


-- | <p>Describes one or more of your instances. You can use this to get information about instances like the operating system platform, the SSM Agent version (Linux), status etc. If you specify one or more instance IDs, it returns information for those instances. If you do not specify instance IDs, it returns information for all your instances. If you specify an instance ID that is not valid or an instance that you do not own, you receive an error. </p>
describeInstanceInformation :: forall eff. DescribeInstanceInformationRequest -> Aff (err :: AWS.RequestError | eff) DescribeInstanceInformationResult
describeInstanceInformation = AWS.request serviceName "DescribeInstanceInformation" 


-- | <p>Retrieves the high-level patch state of one or more instances.</p>
describeInstancePatchStates :: forall eff. DescribeInstancePatchStatesRequest -> Aff (err :: AWS.RequestError | eff) DescribeInstancePatchStatesResult
describeInstancePatchStates = AWS.request serviceName "DescribeInstancePatchStates" 


-- | <p>Retrieves the high-level patch state for the instances in the specified patch group.</p>
describeInstancePatchStatesForPatchGroup :: forall eff. DescribeInstancePatchStatesForPatchGroupRequest -> Aff (err :: AWS.RequestError | eff) DescribeInstancePatchStatesForPatchGroupResult
describeInstancePatchStatesForPatchGroup = AWS.request serviceName "DescribeInstancePatchStatesForPatchGroup" 


-- | <p>Retrieves information about the patches on the specified instance and their state relative to the patch baseline being used for the instance.</p>
describeInstancePatches :: forall eff. DescribeInstancePatchesRequest -> Aff (err :: AWS.RequestError | eff) DescribeInstancePatchesResult
describeInstancePatches = AWS.request serviceName "DescribeInstancePatches" 


-- | <p>Retrieves the individual task executions (one per target) for a particular task executed as part of a Maintenance Window execution.</p>
describeMaintenanceWindowExecutionTaskInvocations :: forall eff. DescribeMaintenanceWindowExecutionTaskInvocationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeMaintenanceWindowExecutionTaskInvocationsResult
describeMaintenanceWindowExecutionTaskInvocations = AWS.request serviceName "DescribeMaintenanceWindowExecutionTaskInvocations" 


-- | <p>For a given Maintenance Window execution, lists the tasks that were executed.</p>
describeMaintenanceWindowExecutionTasks :: forall eff. DescribeMaintenanceWindowExecutionTasksRequest -> Aff (err :: AWS.RequestError | eff) DescribeMaintenanceWindowExecutionTasksResult
describeMaintenanceWindowExecutionTasks = AWS.request serviceName "DescribeMaintenanceWindowExecutionTasks" 


-- | <p>Lists the executions of a Maintenance Window. This includes information about when the Maintenance Window was scheduled to be active, and information about tasks registered and run with the Maintenance Window.</p>
describeMaintenanceWindowExecutions :: forall eff. DescribeMaintenanceWindowExecutionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeMaintenanceWindowExecutionsResult
describeMaintenanceWindowExecutions = AWS.request serviceName "DescribeMaintenanceWindowExecutions" 


-- | <p>Lists the targets registered with the Maintenance Window.</p>
describeMaintenanceWindowTargets :: forall eff. DescribeMaintenanceWindowTargetsRequest -> Aff (err :: AWS.RequestError | eff) DescribeMaintenanceWindowTargetsResult
describeMaintenanceWindowTargets = AWS.request serviceName "DescribeMaintenanceWindowTargets" 


-- | <p>Lists the tasks in a Maintenance Window.</p>
describeMaintenanceWindowTasks :: forall eff. DescribeMaintenanceWindowTasksRequest -> Aff (err :: AWS.RequestError | eff) DescribeMaintenanceWindowTasksResult
describeMaintenanceWindowTasks = AWS.request serviceName "DescribeMaintenanceWindowTasks" 


-- | <p>Retrieves the Maintenance Windows in an AWS account.</p>
describeMaintenanceWindows :: forall eff. DescribeMaintenanceWindowsRequest -> Aff (err :: AWS.RequestError | eff) DescribeMaintenanceWindowsResult
describeMaintenanceWindows = AWS.request serviceName "DescribeMaintenanceWindows" 


-- | <p>Get information about a parameter.</p> <p>Request results are returned on a best-effort basis. If you specify <code>MaxResults</code> in the request, the response includes information up to the limit specified. The number of items returned, however, can be between zero and the value of <code>MaxResults</code>. If the service reaches an internal limit while processing the results, it stops the operation and returns the matching values up to that point and a <code>NextToken</code>. You can specify the <code>NextToken</code> in a subsequent call to get the next set of results.</p>
describeParameters :: forall eff. DescribeParametersRequest -> Aff (err :: AWS.RequestError | eff) DescribeParametersResult
describeParameters = AWS.request serviceName "DescribeParameters" 


-- | <p>Lists the patch baselines in your AWS account.</p>
describePatchBaselines :: forall eff. DescribePatchBaselinesRequest -> Aff (err :: AWS.RequestError | eff) DescribePatchBaselinesResult
describePatchBaselines = AWS.request serviceName "DescribePatchBaselines" 


-- | <p>Returns high-level aggregated patch compliance state for a patch group.</p>
describePatchGroupState :: forall eff. DescribePatchGroupStateRequest -> Aff (err :: AWS.RequestError | eff) DescribePatchGroupStateResult
describePatchGroupState = AWS.request serviceName "DescribePatchGroupState" 


-- | <p>Lists all patch groups that have been registered with patch baselines.</p>
describePatchGroups :: forall eff. DescribePatchGroupsRequest -> Aff (err :: AWS.RequestError | eff) DescribePatchGroupsResult
describePatchGroups = AWS.request serviceName "DescribePatchGroups" 


-- | <p>Get detailed information about a particular Automation execution.</p>
getAutomationExecution :: forall eff. GetAutomationExecutionRequest -> Aff (err :: AWS.RequestError | eff) GetAutomationExecutionResult
getAutomationExecution = AWS.request serviceName "GetAutomationExecution" 


-- | <p>Returns detailed information about command execution for an invocation or plugin. </p>
getCommandInvocation :: forall eff. GetCommandInvocationRequest -> Aff (err :: AWS.RequestError | eff) GetCommandInvocationResult
getCommandInvocation = AWS.request serviceName "GetCommandInvocation" 


-- | <p>Retrieves the default patch baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.</p>
getDefaultPatchBaseline :: forall eff. GetDefaultPatchBaselineRequest -> Aff (err :: AWS.RequestError | eff) GetDefaultPatchBaselineResult
getDefaultPatchBaseline = AWS.request serviceName "GetDefaultPatchBaseline" 


-- | <p>Retrieves the current snapshot for the patch baseline the instance uses. This API is primarily used by the AWS-RunPatchBaseline Systems Manager document. </p>
getDeployablePatchSnapshotForInstance :: forall eff. GetDeployablePatchSnapshotForInstanceRequest -> Aff (err :: AWS.RequestError | eff) GetDeployablePatchSnapshotForInstanceResult
getDeployablePatchSnapshotForInstance = AWS.request serviceName "GetDeployablePatchSnapshotForInstance" 


-- | <p>Gets the contents of the specified Systems Manager document.</p>
getDocument :: forall eff. GetDocumentRequest -> Aff (err :: AWS.RequestError | eff) GetDocumentResult
getDocument = AWS.request serviceName "GetDocument" 


-- | <p>Query inventory information.</p>
getInventory :: forall eff. GetInventoryRequest -> Aff (err :: AWS.RequestError | eff) GetInventoryResult
getInventory = AWS.request serviceName "GetInventory" 


-- | <p>Return a list of inventory type names for the account, or return a list of attribute names for a specific Inventory item type. </p>
getInventorySchema :: forall eff. GetInventorySchemaRequest -> Aff (err :: AWS.RequestError | eff) GetInventorySchemaResult
getInventorySchema = AWS.request serviceName "GetInventorySchema" 


-- | <p>Retrieves a Maintenance Window.</p>
getMaintenanceWindow :: forall eff. GetMaintenanceWindowRequest -> Aff (err :: AWS.RequestError | eff) GetMaintenanceWindowResult
getMaintenanceWindow = AWS.request serviceName "GetMaintenanceWindow" 


-- | <p>Retrieves details about a specific task executed as part of a Maintenance Window execution.</p>
getMaintenanceWindowExecution :: forall eff. GetMaintenanceWindowExecutionRequest -> Aff (err :: AWS.RequestError | eff) GetMaintenanceWindowExecutionResult
getMaintenanceWindowExecution = AWS.request serviceName "GetMaintenanceWindowExecution" 


-- | <p>Retrieves the details about a specific task executed as part of a Maintenance Window execution.</p>
getMaintenanceWindowExecutionTask :: forall eff. GetMaintenanceWindowExecutionTaskRequest -> Aff (err :: AWS.RequestError | eff) GetMaintenanceWindowExecutionTaskResult
getMaintenanceWindowExecutionTask = AWS.request serviceName "GetMaintenanceWindowExecutionTask" 


-- | <p>Retrieves a task invocation. A task invocation is a specific task executing on a specific target. Maintenance Windows report status for all invocations. </p>
getMaintenanceWindowExecutionTaskInvocation :: forall eff. GetMaintenanceWindowExecutionTaskInvocationRequest -> Aff (err :: AWS.RequestError | eff) GetMaintenanceWindowExecutionTaskInvocationResult
getMaintenanceWindowExecutionTaskInvocation = AWS.request serviceName "GetMaintenanceWindowExecutionTaskInvocation" 


-- | <p>Lists the tasks in a Maintenance Window.</p>
getMaintenanceWindowTask :: forall eff. GetMaintenanceWindowTaskRequest -> Aff (err :: AWS.RequestError | eff) GetMaintenanceWindowTaskResult
getMaintenanceWindowTask = AWS.request serviceName "GetMaintenanceWindowTask" 


-- | <p>Get information about a parameter by using the parameter name. </p>
getParameter :: forall eff. GetParameterRequest -> Aff (err :: AWS.RequestError | eff) GetParameterResult
getParameter = AWS.request serviceName "GetParameter" 


-- | <p>Query a list of all parameters used by the AWS account.</p>
getParameterHistory :: forall eff. GetParameterHistoryRequest -> Aff (err :: AWS.RequestError | eff) GetParameterHistoryResult
getParameterHistory = AWS.request serviceName "GetParameterHistory" 


-- | <p>Get details of a parameter.</p>
getParameters :: forall eff. GetParametersRequest -> Aff (err :: AWS.RequestError | eff) GetParametersResult
getParameters = AWS.request serviceName "GetParameters" 


-- | <p>Retrieve parameters in a specific hierarchy. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html">Working with Systems Manager Parameters</a>. </p> <p>Request results are returned on a best-effort basis. If you specify <code>MaxResults</code> in the request, the response includes information up to the limit specified. The number of items returned, however, can be between zero and the value of <code>MaxResults</code>. If the service reaches an internal limit while processing the results, it stops the operation and returns the matching values up to that point and a <code>NextToken</code>. You can specify the <code>NextToken</code> in a subsequent call to get the next set of results.</p> <note> <p>This API action doesn't support filtering by tags. </p> </note>
getParametersByPath :: forall eff. GetParametersByPathRequest -> Aff (err :: AWS.RequestError | eff) GetParametersByPathResult
getParametersByPath = AWS.request serviceName "GetParametersByPath" 


-- | <p>Retrieves information about a patch baseline.</p>
getPatchBaseline :: forall eff. GetPatchBaselineRequest -> Aff (err :: AWS.RequestError | eff) GetPatchBaselineResult
getPatchBaseline = AWS.request serviceName "GetPatchBaseline" 


-- | <p>Retrieves the patch baseline that should be used for the specified patch group.</p>
getPatchBaselineForPatchGroup :: forall eff. GetPatchBaselineForPatchGroupRequest -> Aff (err :: AWS.RequestError | eff) GetPatchBaselineForPatchGroupResult
getPatchBaselineForPatchGroup = AWS.request serviceName "GetPatchBaselineForPatchGroup" 


-- | <p>Retrieves all versions of an association for a specific association ID.</p>
listAssociationVersions :: forall eff. ListAssociationVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListAssociationVersionsResult
listAssociationVersions = AWS.request serviceName "ListAssociationVersions" 


-- | <p>Lists the associations for the specified Systems Manager document or instance.</p>
listAssociations :: forall eff. ListAssociationsRequest -> Aff (err :: AWS.RequestError | eff) ListAssociationsResult
listAssociations = AWS.request serviceName "ListAssociations" 


-- | <p>An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. ListCommandInvocations provide status about command execution.</p>
listCommandInvocations :: forall eff. ListCommandInvocationsRequest -> Aff (err :: AWS.RequestError | eff) ListCommandInvocationsResult
listCommandInvocations = AWS.request serviceName "ListCommandInvocations" 


-- | <p>Lists the commands requested by users of the AWS account.</p>
listCommands :: forall eff. ListCommandsRequest -> Aff (err :: AWS.RequestError | eff) ListCommandsResult
listCommands = AWS.request serviceName "ListCommands" 


-- | <p>For a specified resource ID, this API action returns a list of compliance statuses for different resource types. Currently, you can only specify one resource ID per call. List results depend on the criteria specified in the filter. </p>
listComplianceItems :: forall eff. ListComplianceItemsRequest -> Aff (err :: AWS.RequestError | eff) ListComplianceItemsResult
listComplianceItems = AWS.request serviceName "ListComplianceItems" 


-- | <p>Returns a summary count of compliant and non-compliant resources for a compliance type. For example, this call can return State Manager associations, patches, or custom compliance types according to the filter criteria that you specify. </p>
listComplianceSummaries :: forall eff. ListComplianceSummariesRequest -> Aff (err :: AWS.RequestError | eff) ListComplianceSummariesResult
listComplianceSummaries = AWS.request serviceName "ListComplianceSummaries" 


-- | <p>List all versions for a document.</p>
listDocumentVersions :: forall eff. ListDocumentVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListDocumentVersionsResult
listDocumentVersions = AWS.request serviceName "ListDocumentVersions" 


-- | <p>Describes one or more of your Systems Manager documents.</p>
listDocuments :: forall eff. ListDocumentsRequest -> Aff (err :: AWS.RequestError | eff) ListDocumentsResult
listDocuments = AWS.request serviceName "ListDocuments" 


-- | <p>A list of inventory items returned by the request.</p>
listInventoryEntries :: forall eff. ListInventoryEntriesRequest -> Aff (err :: AWS.RequestError | eff) ListInventoryEntriesResult
listInventoryEntries = AWS.request serviceName "ListInventoryEntries" 


-- | <p>Returns a resource-level summary count. The summary includes information about compliant and non-compliant statuses and detailed compliance-item severity counts, according to the filter criteria you specify.</p>
listResourceComplianceSummaries :: forall eff. ListResourceComplianceSummariesRequest -> Aff (err :: AWS.RequestError | eff) ListResourceComplianceSummariesResult
listResourceComplianceSummaries = AWS.request serviceName "ListResourceComplianceSummaries" 


-- | <p>Lists your resource data sync configurations. Includes information about the last time a sync attempted to start, the last sync status, and the last time a sync successfully completed.</p> <p>The number of sync configurations might be too large to return using a single call to <code>ListResourceDataSync</code>. You can limit the number of sync configurations returned by using the <code>MaxResults</code> parameter. To determine whether there are more sync configurations to list, check the value of <code>NextToken</code> in the output. If there are more sync configurations to list, you can request them by specifying the <code>NextToken</code> returned in the call to the parameter of a subsequent call. </p>
listResourceDataSync :: forall eff. ListResourceDataSyncRequest -> Aff (err :: AWS.RequestError | eff) ListResourceDataSyncResult
listResourceDataSync = AWS.request serviceName "ListResourceDataSync" 


-- | <p>Returns a list of the tags assigned to the specified resource.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResult
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


-- | <p>Shares a Systems Manager document publicly or privately. If you share a document privately, you must specify the AWS user account IDs for those people who can use the document. If you share a document publicly, you must specify <i>All</i> as the account ID.</p>
modifyDocumentPermission :: forall eff. ModifyDocumentPermissionRequest -> Aff (err :: AWS.RequestError | eff) ModifyDocumentPermissionResponse
modifyDocumentPermission = AWS.request serviceName "ModifyDocumentPermission" 


-- | <p>Registers a compliance type and other compliance details on a designated resource. This action lets you register custom compliance details with a resource. This call overwrites existing compliance information on the resource, so you must provide a full list of compliance items each time that you send the request.</p> <p>ComplianceType can be one of the following:</p> <ul> <li> <p>ExecutionId: The execution ID when the patch, association, or custom compliance item was applied.</p> </li> <li> <p>ExecutionType: Specify patch, association, or Custom:<code>string</code>.</p> </li> <li> <p>ExecutionTime. The time the patch, association, or custom compliance item was applied to the instance.</p> </li> <li> <p>Id: The patch, association, or custom compliance ID.</p> </li> <li> <p>Title: A title.</p> </li> <li> <p>Status: The status of the compliance item. For example, <code>approved</code> for patches, or <code>Failed</code> for associations.</p> </li> <li> <p>Severity: A patch severity. For example, <code>critical</code>.</p> </li> <li> <p>DocumentName: A SSM document name. For example, AWS-RunPatchBaseline.</p> </li> <li> <p>DocumentVersion: An SSM document version number. For example, 4.</p> </li> <li> <p>Classification: A patch classification. For example, <code>security updates</code>.</p> </li> <li> <p>PatchBaselineId: A patch baseline ID.</p> </li> <li> <p>PatchSeverity: A patch severity. For example, <code>Critical</code>.</p> </li> <li> <p>PatchState: A patch state. For example, <code>InstancesWithFailedPatches</code>.</p> </li> <li> <p>PatchGroup: The name of a patch group.</p> </li> <li> <p>InstalledTime: The time the association, patch, or custom compliance item was applied to the resource. Specify the time by using the following format: yyyy-MM-dd'T'HH:mm:ss'Z'</p> </li> </ul>
putComplianceItems :: forall eff. PutComplianceItemsRequest -> Aff (err :: AWS.RequestError | eff) PutComplianceItemsResult
putComplianceItems = AWS.request serviceName "PutComplianceItems" 


-- | <p>Bulk update custom inventory items on one more instance. The request adds an inventory item, if it doesn't already exist, or updates an inventory item, if it does exist.</p>
putInventory :: forall eff. PutInventoryRequest -> Aff (err :: AWS.RequestError | eff) PutInventoryResult
putInventory = AWS.request serviceName "PutInventory" 


-- | <p>Add one or more parameters to the system.</p>
putParameter :: forall eff. PutParameterRequest -> Aff (err :: AWS.RequestError | eff) PutParameterResult
putParameter = AWS.request serviceName "PutParameter" 


-- | <p>Defines the default patch baseline.</p>
registerDefaultPatchBaseline :: forall eff. RegisterDefaultPatchBaselineRequest -> Aff (err :: AWS.RequestError | eff) RegisterDefaultPatchBaselineResult
registerDefaultPatchBaseline = AWS.request serviceName "RegisterDefaultPatchBaseline" 


-- | <p>Registers a patch baseline for a patch group.</p>
registerPatchBaselineForPatchGroup :: forall eff. RegisterPatchBaselineForPatchGroupRequest -> Aff (err :: AWS.RequestError | eff) RegisterPatchBaselineForPatchGroupResult
registerPatchBaselineForPatchGroup = AWS.request serviceName "RegisterPatchBaselineForPatchGroup" 


-- | <p>Registers a target with a Maintenance Window.</p>
registerTargetWithMaintenanceWindow :: forall eff. RegisterTargetWithMaintenanceWindowRequest -> Aff (err :: AWS.RequestError | eff) RegisterTargetWithMaintenanceWindowResult
registerTargetWithMaintenanceWindow = AWS.request serviceName "RegisterTargetWithMaintenanceWindow" 


-- | <p>Adds a new task to a Maintenance Window.</p>
registerTaskWithMaintenanceWindow :: forall eff. RegisterTaskWithMaintenanceWindowRequest -> Aff (err :: AWS.RequestError | eff) RegisterTaskWithMaintenanceWindowResult
registerTaskWithMaintenanceWindow = AWS.request serviceName "RegisterTaskWithMaintenanceWindow" 


-- | <p>Removes all tags from the specified resource.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (err :: AWS.RequestError | eff) RemoveTagsFromResourceResult
removeTagsFromResource = AWS.request serviceName "RemoveTagsFromResource" 


-- | <p>Sends a signal to an Automation execution to change the current behavior or status of the execution. </p>
sendAutomationSignal :: forall eff. SendAutomationSignalRequest -> Aff (err :: AWS.RequestError | eff) SendAutomationSignalResult
sendAutomationSignal = AWS.request serviceName "SendAutomationSignal" 


-- | <p>Executes commands on one or more managed instances.</p>
sendCommand :: forall eff. SendCommandRequest -> Aff (err :: AWS.RequestError | eff) SendCommandResult
sendCommand = AWS.request serviceName "SendCommand" 


-- | <p>Initiates execution of an Automation document.</p>
startAutomationExecution :: forall eff. StartAutomationExecutionRequest -> Aff (err :: AWS.RequestError | eff) StartAutomationExecutionResult
startAutomationExecution = AWS.request serviceName "StartAutomationExecution" 


-- | <p>Stop an Automation that is currently executing.</p>
stopAutomationExecution :: forall eff. StopAutomationExecutionRequest -> Aff (err :: AWS.RequestError | eff) StopAutomationExecutionResult
stopAutomationExecution = AWS.request serviceName "StopAutomationExecution" 


-- | <p>Updates an association. You can update the association name and version, the document version, schedule, parameters, and Amazon S3 output.</p>
updateAssociation :: forall eff. UpdateAssociationRequest -> Aff (err :: AWS.RequestError | eff) UpdateAssociationResult
updateAssociation = AWS.request serviceName "UpdateAssociation" 


-- | <p>Updates the status of the Systems Manager document associated with the specified instance.</p>
updateAssociationStatus :: forall eff. UpdateAssociationStatusRequest -> Aff (err :: AWS.RequestError | eff) UpdateAssociationStatusResult
updateAssociationStatus = AWS.request serviceName "UpdateAssociationStatus" 


-- | <p>The document you want to update.</p>
updateDocument :: forall eff. UpdateDocumentRequest -> Aff (err :: AWS.RequestError | eff) UpdateDocumentResult
updateDocument = AWS.request serviceName "UpdateDocument" 


-- | <p>Set the default version of a document. </p>
updateDocumentDefaultVersion :: forall eff. UpdateDocumentDefaultVersionRequest -> Aff (err :: AWS.RequestError | eff) UpdateDocumentDefaultVersionResult
updateDocumentDefaultVersion = AWS.request serviceName "UpdateDocumentDefaultVersion" 


-- | <p>Updates an existing Maintenance Window. Only specified parameters are modified.</p>
updateMaintenanceWindow :: forall eff. UpdateMaintenanceWindowRequest -> Aff (err :: AWS.RequestError | eff) UpdateMaintenanceWindowResult
updateMaintenanceWindow = AWS.request serviceName "UpdateMaintenanceWindow" 


-- | <p>Modifies the target of an existing Maintenance Window. You can't change the target type, but you can change the following:</p> <p>The target from being an ID target to a Tag target, or a Tag target to an ID target.</p> <p>IDs for an ID target.</p> <p>Tags for a Tag target.</p> <p>Owner.</p> <p>Name.</p> <p>Description.</p> <p>If a parameter is null, then the corresponding field is not modified.</p>
updateMaintenanceWindowTarget :: forall eff. UpdateMaintenanceWindowTargetRequest -> Aff (err :: AWS.RequestError | eff) UpdateMaintenanceWindowTargetResult
updateMaintenanceWindowTarget = AWS.request serviceName "UpdateMaintenanceWindowTarget" 


-- | <p>Modifies a task assigned to a Maintenance Window. You can't change the task type, but you can change the following values:</p> <p>Task ARN. For example, you can change a RUN_COMMAND task from AWS-RunPowerShellScript to AWS-RunShellScript.</p> <p>Service role ARN.</p> <p>Task parameters.</p> <p>Task priority.</p> <p>Task MaxConcurrency and MaxErrors.</p> <p>Log location.</p> <p>If a parameter is null, then the corresponding field is not modified. Also, if you set Replace to true, then all fields required by the RegisterTaskWithMaintenanceWindow action are required for this request. Optional fields that aren't specified are set to null.</p>
updateMaintenanceWindowTask :: forall eff. UpdateMaintenanceWindowTaskRequest -> Aff (err :: AWS.RequestError | eff) UpdateMaintenanceWindowTaskResult
updateMaintenanceWindowTask = AWS.request serviceName "UpdateMaintenanceWindowTask" 


-- | <p>Assigns or changes an Amazon Identity and Access Management (IAM) role to the managed instance.</p>
updateManagedInstanceRole :: forall eff. UpdateManagedInstanceRoleRequest -> Aff (err :: AWS.RequestError | eff) UpdateManagedInstanceRoleResult
updateManagedInstanceRole = AWS.request serviceName "UpdateManagedInstanceRole" 


-- | <p>Modifies an existing patch baseline. Fields not specified in the request are left unchanged.</p> <note> <p>For information about valid key and value pairs in <code>PatchFilters</code> for each supported operating system type, see <a href="http://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PatchFilter.html">PatchFilter</a>.</p> </note>
updatePatchBaseline :: forall eff. UpdatePatchBaselineRequest -> Aff (err :: AWS.RequestError | eff) UpdatePatchBaselineResult
updatePatchBaseline = AWS.request serviceName "UpdatePatchBaseline" 


newtype AccountId = AccountId String


newtype AccountIdList = AccountIdList (Array AccountId)


-- | <p>An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.</p>
newtype Activation = Activation 
  { "ActivationId" :: NullOrUndefined (ActivationId)
  , "Description" :: NullOrUndefined (ActivationDescription)
  , "DefaultInstanceName" :: NullOrUndefined (DefaultInstanceName)
  , "IamRole" :: NullOrUndefined (IamRole)
  , "RegistrationLimit" :: NullOrUndefined (RegistrationLimit)
  , "RegistrationsCount" :: NullOrUndefined (RegistrationsCount)
  , "ExpirationDate" :: NullOrUndefined (ExpirationDate)
  , "Expired" :: NullOrUndefined (Boolean)
  , "CreatedDate" :: NullOrUndefined (CreatedDate)
  }


newtype ActivationCode = ActivationCode String


newtype ActivationDescription = ActivationDescription String


newtype ActivationId = ActivationId String


newtype ActivationList = ActivationList (Array Activation)


newtype AddTagsToResourceRequest = AddTagsToResourceRequest 
  { "ResourceType" :: (ResourceTypeForTagging)
  , "ResourceId" :: (ResourceId)
  , "Tags" :: (TagList)
  }


newtype AddTagsToResourceResult = AddTagsToResourceResult 
  { 
  }


newtype AgentErrorCode = AgentErrorCode String


newtype AggregatorSchemaOnly = AggregatorSchemaOnly Boolean


newtype AllowedPattern = AllowedPattern String


-- | <p>Error returned if an attempt is made to register a patch group with a patch baseline that is already registered with a different patch baseline.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Message" :: NullOrUndefined (String)
  }


newtype ApproveAfterDays = ApproveAfterDays Int


-- | <p>You must disassociate a document from all instances before you can delete it.</p>
newtype AssociatedInstances = AssociatedInstances 
  { 
  }


-- | <p>Describes an association of a Systems Manager document and an instance.</p>
newtype Association = Association 
  { "Name" :: NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "AssociationId" :: NullOrUndefined (AssociationId)
  , "AssociationVersion" :: NullOrUndefined (AssociationVersion)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Targets" :: NullOrUndefined (Targets)
  , "LastExecutionDate" :: NullOrUndefined (DateTime)
  , "Overview" :: NullOrUndefined (AssociationOverview)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "AssociationName" :: NullOrUndefined (AssociationName)
  }


-- | <p>The specified association already exists.</p>
newtype AssociationAlreadyExists = AssociationAlreadyExists 
  { 
  }


-- | <p>Describes the parameters for a document.</p>
newtype AssociationDescription = AssociationDescription 
  { "Name" :: NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "AssociationVersion" :: NullOrUndefined (AssociationVersion)
  , "Date" :: NullOrUndefined (DateTime)
  , "LastUpdateAssociationDate" :: NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined (AssociationStatus)
  , "Overview" :: NullOrUndefined (AssociationOverview)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "AssociationId" :: NullOrUndefined (AssociationId)
  , "Targets" :: NullOrUndefined (Targets)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation)
  , "LastExecutionDate" :: NullOrUndefined (DateTime)
  , "LastSuccessfulExecutionDate" :: NullOrUndefined (DateTime)
  , "AssociationName" :: NullOrUndefined (AssociationName)
  }


newtype AssociationDescriptionList = AssociationDescriptionList (Array AssociationDescription)


-- | <p>The specified association does not exist.</p>
newtype AssociationDoesNotExist = AssociationDoesNotExist 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Describes a filter.</p>
newtype AssociationFilter = AssociationFilter 
  { "Key'" :: (AssociationFilterKey)
  , "Value'" :: (AssociationFilterValue)
  }


newtype AssociationFilterKey = AssociationFilterKey String


newtype AssociationFilterList = AssociationFilterList (Array AssociationFilter)


newtype AssociationFilterValue = AssociationFilterValue String


newtype AssociationId = AssociationId String


-- | <p>You can have at most 2,000 active associations.</p>
newtype AssociationLimitExceeded = AssociationLimitExceeded 
  { 
  }


newtype AssociationList = AssociationList (Array Association)


newtype AssociationName = AssociationName String


-- | <p>Information about the association.</p>
newtype AssociationOverview = AssociationOverview 
  { "Status" :: NullOrUndefined (StatusName)
  , "DetailedStatus" :: NullOrUndefined (StatusName)
  , "AssociationStatusAggregatedCount" :: NullOrUndefined (AssociationStatusAggregatedCount)
  }


-- | <p>Describes an association status.</p>
newtype AssociationStatus = AssociationStatus 
  { "Date" :: (DateTime)
  , "Name" :: (AssociationStatusName)
  , "Message" :: (StatusMessage)
  , "AdditionalInfo" :: NullOrUndefined (StatusAdditionalInfo)
  }


newtype AssociationStatusAggregatedCount = AssociationStatusAggregatedCount (Map StatusName InstanceCount)


newtype AssociationStatusName = AssociationStatusName String


newtype AssociationVersion = AssociationVersion String


-- | <p>Information about the association version.</p>
newtype AssociationVersionInfo = AssociationVersionInfo 
  { "AssociationId" :: NullOrUndefined (AssociationId)
  , "AssociationVersion" :: NullOrUndefined (AssociationVersion)
  , "CreatedDate" :: NullOrUndefined (DateTime)
  , "Name" :: NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "Targets" :: NullOrUndefined (Targets)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation)
  , "AssociationName" :: NullOrUndefined (AssociationName)
  }


-- | <p>You have reached the maximum number versions allowed for an association. Each association has a limit of 1,000 versions. </p>
newtype AssociationVersionLimitExceeded = AssociationVersionLimitExceeded 
  { "Message" :: NullOrUndefined (String)
  }


newtype AssociationVersionList = AssociationVersionList (Array AssociationVersionInfo)


newtype AttributeName = AttributeName String


newtype AttributeValue = AttributeValue String


newtype AutomationActionName = AutomationActionName String


-- | <p>An Automation document with the specified name could not be found.</p>
newtype AutomationDefinitionNotFoundException = AutomationDefinitionNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>An Automation document with the specified name and version could not be found.</p>
newtype AutomationDefinitionVersionNotFoundException = AutomationDefinitionVersionNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Detailed information about the current state of an individual Automation execution.</p>
newtype AutomationExecution = AutomationExecution 
  { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId)
  , "DocumentName" :: NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "ExecutionStartTime" :: NullOrUndefined (DateTime)
  , "ExecutionEndTime" :: NullOrUndefined (DateTime)
  , "AutomationExecutionStatus" :: NullOrUndefined (AutomationExecutionStatus)
  , "StepExecutions" :: NullOrUndefined (StepExecutionList)
  , "StepExecutionsTruncated" :: NullOrUndefined (Boolean)
  , "Parameters" :: NullOrUndefined (AutomationParameterMap)
  , "Outputs" :: NullOrUndefined (AutomationParameterMap)
  , "FailureMessage" :: NullOrUndefined (String)
  , "Mode" :: NullOrUndefined (ExecutionMode)
  , "ParentAutomationExecutionId" :: NullOrUndefined (AutomationExecutionId)
  , "ExecutedBy" :: NullOrUndefined (String)
  , "CurrentStepName" :: NullOrUndefined (String)
  , "CurrentAction" :: NullOrUndefined (String)
  , "TargetParameterName" :: NullOrUndefined (AutomationParameterKey)
  , "Targets" :: NullOrUndefined (Targets)
  , "ResolvedTargets" :: NullOrUndefined (ResolvedTargets)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  , "Target" :: NullOrUndefined (String)
  }


-- | <p>A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.</p>
newtype AutomationExecutionFilter = AutomationExecutionFilter 
  { "Key" :: (AutomationExecutionFilterKey)
  , "Values" :: (AutomationExecutionFilterValueList)
  }


newtype AutomationExecutionFilterKey = AutomationExecutionFilterKey String


newtype AutomationExecutionFilterList = AutomationExecutionFilterList (Array AutomationExecutionFilter)


newtype AutomationExecutionFilterValue = AutomationExecutionFilterValue String


newtype AutomationExecutionFilterValueList = AutomationExecutionFilterValueList (Array AutomationExecutionFilterValue)


newtype AutomationExecutionId = AutomationExecutionId String


-- | <p>The number of simultaneously running Automation executions exceeded the allowable limit.</p>
newtype AutomationExecutionLimitExceededException = AutomationExecutionLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Details about a specific Automation execution.</p>
newtype AutomationExecutionMetadata = AutomationExecutionMetadata 
  { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId)
  , "DocumentName" :: NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "AutomationExecutionStatus" :: NullOrUndefined (AutomationExecutionStatus)
  , "ExecutionStartTime" :: NullOrUndefined (DateTime)
  , "ExecutionEndTime" :: NullOrUndefined (DateTime)
  , "ExecutedBy" :: NullOrUndefined (String)
  , "LogFile" :: NullOrUndefined (String)
  , "Outputs" :: NullOrUndefined (AutomationParameterMap)
  , "Mode" :: NullOrUndefined (ExecutionMode)
  , "ParentAutomationExecutionId" :: NullOrUndefined (AutomationExecutionId)
  , "CurrentStepName" :: NullOrUndefined (String)
  , "CurrentAction" :: NullOrUndefined (String)
  , "FailureMessage" :: NullOrUndefined (String)
  , "TargetParameterName" :: NullOrUndefined (AutomationParameterKey)
  , "Targets" :: NullOrUndefined (Targets)
  , "ResolvedTargets" :: NullOrUndefined (ResolvedTargets)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  , "Target" :: NullOrUndefined (String)
  }


newtype AutomationExecutionMetadataList = AutomationExecutionMetadataList (Array AutomationExecutionMetadata)


-- | <p>There is no automation execution information for the requested automation execution ID.</p>
newtype AutomationExecutionNotFoundException = AutomationExecutionNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }


newtype AutomationExecutionStatus = AutomationExecutionStatus String


newtype AutomationParameterKey = AutomationParameterKey String


newtype AutomationParameterMap = AutomationParameterMap (Map AutomationParameterKey AutomationParameterValueList)


newtype AutomationParameterValue = AutomationParameterValue String


newtype AutomationParameterValueList = AutomationParameterValueList (Array AutomationParameterValue)


-- | <p>The specified step name and execution ID don't exist. Verify the information and try again.</p>
newtype AutomationStepNotFoundException = AutomationStepNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }


newtype BaselineDescription = BaselineDescription String


newtype BaselineId = BaselineId String


newtype BaselineName = BaselineName String


newtype BatchErrorMessage = BatchErrorMessage String


-- | <p/>
newtype CancelCommandRequest = CancelCommandRequest 
  { "CommandId" :: (CommandId)
  , "InstanceIds" :: NullOrUndefined (InstanceIdList)
  }


-- | <p>Whether or not the command was successfully canceled. There is no guarantee that a request can be canceled.</p>
newtype CancelCommandResult = CancelCommandResult 
  { 
  }


newtype ClientToken = ClientToken String


-- | <p>Describes a command request.</p>
newtype Command = Command 
  { "CommandId" :: NullOrUndefined (CommandId)
  , "DocumentName" :: NullOrUndefined (DocumentName)
  , "Comment" :: NullOrUndefined (Comment)
  , "ExpiresAfter" :: NullOrUndefined (DateTime)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "InstanceIds" :: NullOrUndefined (InstanceIdList)
  , "Targets" :: NullOrUndefined (Targets)
  , "RequestedDateTime" :: NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined (CommandStatus)
  , "StatusDetails" :: NullOrUndefined (StatusDetails)
  , "OutputS3Region" :: NullOrUndefined (S3Region)
  , "OutputS3BucketName" :: NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  , "TargetCount" :: NullOrUndefined (TargetCount)
  , "CompletedCount" :: NullOrUndefined (CompletedCount)
  , "ErrorCount" :: NullOrUndefined (ErrorCount)
  , "ServiceRole" :: NullOrUndefined (ServiceRole)
  , "NotificationConfig" :: NullOrUndefined (NotificationConfig)
  }


-- | <p>Describes a command filter.</p>
newtype CommandFilter = CommandFilter 
  { "Key'" :: (CommandFilterKey)
  , "Value'" :: (CommandFilterValue)
  }


newtype CommandFilterKey = CommandFilterKey String


newtype CommandFilterList = CommandFilterList (Array CommandFilter)


newtype CommandFilterValue = CommandFilterValue String


newtype CommandId = CommandId String


-- | <p>An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. A command invocation returns status and detail information about a command you executed. </p>
newtype CommandInvocation = CommandInvocation 
  { "CommandId" :: NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "InstanceName" :: NullOrUndefined (InstanceTagName)
  , "Comment" :: NullOrUndefined (Comment)
  , "DocumentName" :: NullOrUndefined (DocumentName)
  , "RequestedDateTime" :: NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined (CommandInvocationStatus)
  , "StatusDetails" :: NullOrUndefined (StatusDetails)
  , "TraceOutput" :: NullOrUndefined (InvocationTraceOutput)
  , "StandardOutputUrl" :: NullOrUndefined (Url)
  , "StandardErrorUrl" :: NullOrUndefined (Url)
  , "CommandPlugins" :: NullOrUndefined (CommandPluginList)
  , "ServiceRole" :: NullOrUndefined (ServiceRole)
  , "NotificationConfig" :: NullOrUndefined (NotificationConfig)
  }


newtype CommandInvocationList = CommandInvocationList (Array CommandInvocation)


newtype CommandInvocationStatus = CommandInvocationStatus String


newtype CommandList = CommandList (Array Command)


newtype CommandMaxResults = CommandMaxResults Int


-- | <p>Describes plugin details.</p>
newtype CommandPlugin = CommandPlugin 
  { "Name" :: NullOrUndefined (CommandPluginName)
  , "Status" :: NullOrUndefined (CommandPluginStatus)
  , "StatusDetails" :: NullOrUndefined (StatusDetails)
  , "ResponseCode" :: NullOrUndefined (ResponseCode)
  , "ResponseStartDateTime" :: NullOrUndefined (DateTime)
  , "ResponseFinishDateTime" :: NullOrUndefined (DateTime)
  , "Output" :: NullOrUndefined (CommandPluginOutput)
  , "StandardOutputUrl" :: NullOrUndefined (Url)
  , "StandardErrorUrl" :: NullOrUndefined (Url)
  , "OutputS3Region" :: NullOrUndefined (S3Region)
  , "OutputS3BucketName" :: NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix)
  }


newtype CommandPluginList = CommandPluginList (Array CommandPlugin)


newtype CommandPluginName = CommandPluginName String


newtype CommandPluginOutput = CommandPluginOutput String


newtype CommandPluginStatus = CommandPluginStatus String


newtype CommandStatus = CommandStatus String


newtype Comment = Comment String


newtype CompletedCount = CompletedCount Int


newtype ComplianceExecutionId = ComplianceExecutionId String


-- | <p>A summary of the call execution that includes an execution ID, the type of execution (for example, <code>Command</code>), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.</p>
newtype ComplianceExecutionSummary = ComplianceExecutionSummary 
  { "ExecutionTime" :: (DateTime)
  , "ExecutionId" :: NullOrUndefined (ComplianceExecutionId)
  , "ExecutionType" :: NullOrUndefined (ComplianceExecutionType)
  }


newtype ComplianceExecutionType = ComplianceExecutionType String


newtype ComplianceFilterValue = ComplianceFilterValue String


-- | <p>Information about the compliance as defined by the resource type. For example, for a patch resource type, <code>Items</code> includes information about the PatchSeverity, Classification, etc.</p>
newtype ComplianceItem = ComplianceItem 
  { "ComplianceType" :: NullOrUndefined (ComplianceTypeName)
  , "ResourceType" :: NullOrUndefined (ComplianceResourceType)
  , "ResourceId" :: NullOrUndefined (ComplianceResourceId)
  , "Id" :: NullOrUndefined (ComplianceItemId)
  , "Title" :: NullOrUndefined (ComplianceItemTitle)
  , "Status" :: NullOrUndefined (ComplianceStatus)
  , "Severity" :: NullOrUndefined (ComplianceSeverity)
  , "ExecutionSummary" :: NullOrUndefined (ComplianceExecutionSummary)
  , "Details" :: NullOrUndefined (ComplianceItemDetails)
  }


newtype ComplianceItemContentHash = ComplianceItemContentHash String


newtype ComplianceItemDetails = ComplianceItemDetails (Map AttributeName AttributeValue)


-- | <p>Information about a compliance item.</p>
newtype ComplianceItemEntry = ComplianceItemEntry 
  { "Id" :: NullOrUndefined (ComplianceItemId)
  , "Title" :: NullOrUndefined (ComplianceItemTitle)
  , "Severity" :: (ComplianceSeverity)
  , "Status" :: (ComplianceStatus)
  , "Details" :: NullOrUndefined (ComplianceItemDetails)
  }


newtype ComplianceItemEntryList = ComplianceItemEntryList (Array ComplianceItemEntry)


newtype ComplianceItemId = ComplianceItemId String


newtype ComplianceItemList = ComplianceItemList (Array ComplianceItem)


newtype ComplianceItemTitle = ComplianceItemTitle String


newtype ComplianceQueryOperatorType = ComplianceQueryOperatorType String


newtype ComplianceResourceId = ComplianceResourceId String


newtype ComplianceResourceIdList = ComplianceResourceIdList (Array ComplianceResourceId)


newtype ComplianceResourceType = ComplianceResourceType String


newtype ComplianceResourceTypeList = ComplianceResourceTypeList (Array ComplianceResourceType)


newtype ComplianceSeverity = ComplianceSeverity String


newtype ComplianceStatus = ComplianceStatus String


-- | <p>One or more filters. Use a filter to return a more specific list of results.</p>
newtype ComplianceStringFilter = ComplianceStringFilter 
  { "Key" :: NullOrUndefined (ComplianceStringFilterKey)
  , "Values" :: NullOrUndefined (ComplianceStringFilterValueList)
  , "Type" :: NullOrUndefined (ComplianceQueryOperatorType)
  }


newtype ComplianceStringFilterKey = ComplianceStringFilterKey String


newtype ComplianceStringFilterList = ComplianceStringFilterList (Array ComplianceStringFilter)


newtype ComplianceStringFilterValueList = ComplianceStringFilterValueList (Array ComplianceFilterValue)


newtype ComplianceSummaryCount = ComplianceSummaryCount Int


-- | <p>A summary of compliance information by compliance type.</p>
newtype ComplianceSummaryItem = ComplianceSummaryItem 
  { "ComplianceType" :: NullOrUndefined (ComplianceTypeName)
  , "CompliantSummary" :: NullOrUndefined (CompliantSummary)
  , "NonCompliantSummary" :: NullOrUndefined (NonCompliantSummary)
  }


newtype ComplianceSummaryItemList = ComplianceSummaryItemList (Array ComplianceSummaryItem)


-- | <p>You specified too many custom compliance types. You can specify a maximum of 10 different types. </p>
newtype ComplianceTypeCountLimitExceededException = ComplianceTypeCountLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


newtype ComplianceTypeName = ComplianceTypeName String


-- | <p>A summary of resources that are compliant. The summary is organized according to the resource count for each compliance type.</p>
newtype CompliantSummary = CompliantSummary 
  { "CompliantCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "SeveritySummary" :: NullOrUndefined (SeveritySummary)
  }


newtype ComputerName = ComputerName String


newtype CreateActivationRequest = CreateActivationRequest 
  { "Description" :: NullOrUndefined (ActivationDescription)
  , "DefaultInstanceName" :: NullOrUndefined (DefaultInstanceName)
  , "IamRole" :: (IamRole)
  , "RegistrationLimit" :: NullOrUndefined (RegistrationLimit)
  , "ExpirationDate" :: NullOrUndefined (ExpirationDate)
  }


newtype CreateActivationResult = CreateActivationResult 
  { "ActivationId" :: NullOrUndefined (ActivationId)
  , "ActivationCode" :: NullOrUndefined (ActivationCode)
  }


newtype CreateAssociationBatchRequest = CreateAssociationBatchRequest 
  { "Entries" :: (CreateAssociationBatchRequestEntries)
  }


newtype CreateAssociationBatchRequestEntries = CreateAssociationBatchRequestEntries (Array CreateAssociationBatchRequestEntry)


-- | <p>Describes the association of a Systems Manager document and an instance.</p>
newtype CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry 
  { "Name" :: (DocumentName)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Targets" :: NullOrUndefined (Targets)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation)
  , "AssociationName" :: NullOrUndefined (AssociationName)
  }


newtype CreateAssociationBatchResult = CreateAssociationBatchResult 
  { "Successful" :: NullOrUndefined (AssociationDescriptionList)
  , "Failed" :: NullOrUndefined (FailedCreateAssociationList)
  }


newtype CreateAssociationRequest = CreateAssociationRequest 
  { "Name" :: (DocumentName)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "Targets" :: NullOrUndefined (Targets)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation)
  , "AssociationName" :: NullOrUndefined (AssociationName)
  }


newtype CreateAssociationResult = CreateAssociationResult 
  { "AssociationDescription" :: NullOrUndefined (AssociationDescription)
  }


newtype CreateDocumentRequest = CreateDocumentRequest 
  { "Content" :: (DocumentContent)
  , "Name" :: (DocumentName)
  , "DocumentType" :: NullOrUndefined (DocumentType)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined (TargetType)
  }


newtype CreateDocumentResult = CreateDocumentResult 
  { "DocumentDescription" :: NullOrUndefined (DocumentDescription)
  }


newtype CreateMaintenanceWindowRequest = CreateMaintenanceWindowRequest 
  { "Name" :: (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Schedule" :: (MaintenanceWindowSchedule)
  , "Duration" :: (MaintenanceWindowDurationHours)
  , "Cutoff" :: (MaintenanceWindowCutoff)
  , "AllowUnassociatedTargets" :: (MaintenanceWindowAllowUnassociatedTargets)
  , "ClientToken" :: NullOrUndefined (ClientToken)
  }


newtype CreateMaintenanceWindowResult = CreateMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  }


newtype CreatePatchBaselineRequest = CreatePatchBaselineRequest 
  { "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  , "Name" :: (BaselineName)
  , "GlobalFilters" :: NullOrUndefined (PatchFilterGroup)
  , "ApprovalRules" :: NullOrUndefined (PatchRuleGroup)
  , "ApprovedPatches" :: NullOrUndefined (PatchIdList)
  , "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel)
  , "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean)
  , "RejectedPatches" :: NullOrUndefined (PatchIdList)
  , "Description" :: NullOrUndefined (BaselineDescription)
  , "Sources" :: NullOrUndefined (PatchSourceList)
  , "ClientToken" :: NullOrUndefined (ClientToken)
  }


newtype CreatePatchBaselineResult = CreatePatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  }


newtype CreateResourceDataSyncRequest = CreateResourceDataSyncRequest 
  { "SyncName" :: (ResourceDataSyncName)
  , "S3Destination" :: (ResourceDataSyncS3Destination)
  }


newtype CreateResourceDataSyncResult = CreateResourceDataSyncResult 
  { 
  }


newtype CreatedDate = CreatedDate Number


-- | <p>You have exceeded the limit for custom schemas. Delete one or more custom schemas and try again.</p>
newtype CustomSchemaCountLimitExceededException = CustomSchemaCountLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


newtype DateTime = DateTime Number


newtype DefaultBaseline = DefaultBaseline Boolean


newtype DefaultInstanceName = DefaultInstanceName String


newtype DeleteActivationRequest = DeleteActivationRequest 
  { "ActivationId" :: (ActivationId)
  }


newtype DeleteActivationResult = DeleteActivationResult 
  { 
  }


newtype DeleteAssociationRequest = DeleteAssociationRequest 
  { "Name" :: NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "AssociationId" :: NullOrUndefined (AssociationId)
  }


newtype DeleteAssociationResult = DeleteAssociationResult 
  { 
  }


newtype DeleteDocumentRequest = DeleteDocumentRequest 
  { "Name" :: (DocumentName)
  }


newtype DeleteDocumentResult = DeleteDocumentResult 
  { 
  }


newtype DeleteMaintenanceWindowRequest = DeleteMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  }


newtype DeleteMaintenanceWindowResult = DeleteMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  }


newtype DeleteParameterRequest = DeleteParameterRequest 
  { "Name" :: (PSParameterName)
  }


newtype DeleteParameterResult = DeleteParameterResult 
  { 
  }


newtype DeleteParametersRequest = DeleteParametersRequest 
  { "Names" :: (ParameterNameList)
  }


newtype DeleteParametersResult = DeleteParametersResult 
  { "DeletedParameters" :: NullOrUndefined (ParameterNameList)
  , "InvalidParameters" :: NullOrUndefined (ParameterNameList)
  }


newtype DeletePatchBaselineRequest = DeletePatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  }


newtype DeletePatchBaselineResult = DeletePatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  }


newtype DeleteResourceDataSyncRequest = DeleteResourceDataSyncRequest 
  { "SyncName" :: (ResourceDataSyncName)
  }


newtype DeleteResourceDataSyncResult = DeleteResourceDataSyncResult 
  { 
  }


newtype DeregisterManagedInstanceRequest = DeregisterManagedInstanceRequest 
  { "InstanceId" :: (ManagedInstanceId)
  }


newtype DeregisterManagedInstanceResult = DeregisterManagedInstanceResult 
  { 
  }


newtype DeregisterPatchBaselineForPatchGroupRequest = DeregisterPatchBaselineForPatchGroupRequest 
  { "BaselineId" :: (BaselineId)
  , "PatchGroup" :: (PatchGroup)
  }


newtype DeregisterPatchBaselineForPatchGroupResult = DeregisterPatchBaselineForPatchGroupResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "PatchGroup" :: NullOrUndefined (PatchGroup)
  }


newtype DeregisterTargetFromMaintenanceWindowRequest = DeregisterTargetFromMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTargetId" :: (MaintenanceWindowTargetId)
  , "Safe" :: NullOrUndefined (Boolean)
  }


newtype DeregisterTargetFromMaintenanceWindowResult = DeregisterTargetFromMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId)
  }


newtype DeregisterTaskFromMaintenanceWindowRequest = DeregisterTaskFromMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTaskId" :: (MaintenanceWindowTaskId)
  }


newtype DeregisterTaskFromMaintenanceWindowResult = DeregisterTaskFromMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId)
  }


-- | <p>Filter for the DescribeActivation API.</p>
newtype DescribeActivationsFilter = DescribeActivationsFilter 
  { "FilterKey" :: NullOrUndefined (DescribeActivationsFilterKeys)
  , "FilterValues" :: NullOrUndefined (StringList)
  }


newtype DescribeActivationsFilterKeys = DescribeActivationsFilterKeys String


newtype DescribeActivationsFilterList = DescribeActivationsFilterList (Array DescribeActivationsFilter)


newtype DescribeActivationsRequest = DescribeActivationsRequest 
  { "Filters" :: NullOrUndefined (DescribeActivationsFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeActivationsResult = DescribeActivationsResult 
  { "ActivationList" :: NullOrUndefined (ActivationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeAssociationRequest = DescribeAssociationRequest 
  { "Name" :: NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "AssociationId" :: NullOrUndefined (AssociationId)
  , "AssociationVersion" :: NullOrUndefined (AssociationVersion)
  }


newtype DescribeAssociationResult = DescribeAssociationResult 
  { "AssociationDescription" :: NullOrUndefined (AssociationDescription)
  }


newtype DescribeAutomationExecutionsRequest = DescribeAutomationExecutionsRequest 
  { "Filters" :: NullOrUndefined (AutomationExecutionFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeAutomationExecutionsResult = DescribeAutomationExecutionsResult 
  { "AutomationExecutionMetadataList" :: NullOrUndefined (AutomationExecutionMetadataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeAutomationStepExecutionsRequest = DescribeAutomationStepExecutionsRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  , "Filters" :: NullOrUndefined (StepExecutionFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "ReverseOrder" :: NullOrUndefined (Boolean)
  }


newtype DescribeAutomationStepExecutionsResult = DescribeAutomationStepExecutionsResult 
  { "StepExecutions" :: NullOrUndefined (StepExecutionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeAvailablePatchesRequest = DescribeAvailablePatchesRequest 
  { "Filters" :: NullOrUndefined (PatchOrchestratorFilterList)
  , "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeAvailablePatchesResult = DescribeAvailablePatchesResult 
  { "Patches" :: NullOrUndefined (PatchList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeDocumentPermissionRequest = DescribeDocumentPermissionRequest 
  { "Name" :: (DocumentName)
  , "PermissionType" :: (DocumentPermissionType)
  }


newtype DescribeDocumentPermissionResponse = DescribeDocumentPermissionResponse 
  { "AccountIds" :: NullOrUndefined (AccountIdList)
  }


newtype DescribeDocumentRequest = DescribeDocumentRequest 
  { "Name" :: (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  }


newtype DescribeDocumentResult = DescribeDocumentResult 
  { "Document" :: NullOrUndefined (DocumentDescription)
  }


newtype DescribeEffectiveInstanceAssociationsRequest = DescribeEffectiveInstanceAssociationsRequest 
  { "InstanceId" :: (InstanceId)
  , "MaxResults" :: NullOrUndefined (EffectiveInstanceAssociationMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeEffectiveInstanceAssociationsResult = DescribeEffectiveInstanceAssociationsResult 
  { "Associations" :: NullOrUndefined (InstanceAssociationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeEffectivePatchesForPatchBaselineRequest = DescribeEffectivePatchesForPatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  , "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeEffectivePatchesForPatchBaselineResult = DescribeEffectivePatchesForPatchBaselineResult 
  { "EffectivePatches" :: NullOrUndefined (EffectivePatchList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeInstanceAssociationsStatusRequest = DescribeInstanceAssociationsStatusRequest 
  { "InstanceId" :: (InstanceId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeInstanceAssociationsStatusResult = DescribeInstanceAssociationsStatusResult 
  { "InstanceAssociationStatusInfos" :: NullOrUndefined (InstanceAssociationStatusInfos)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeInstanceInformationRequest = DescribeInstanceInformationRequest 
  { "InstanceInformationFilterList" :: NullOrUndefined (InstanceInformationFilterList)
  , "Filters" :: NullOrUndefined (InstanceInformationStringFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResultsEC2Compatible)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeInstanceInformationResult = DescribeInstanceInformationResult 
  { "InstanceInformationList" :: NullOrUndefined (InstanceInformationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeInstancePatchStatesForPatchGroupRequest = DescribeInstancePatchStatesForPatchGroupRequest 
  { "PatchGroup" :: (PatchGroup)
  , "Filters" :: NullOrUndefined (InstancePatchStateFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults)
  }


newtype DescribeInstancePatchStatesForPatchGroupResult = DescribeInstancePatchStatesForPatchGroupResult 
  { "InstancePatchStates" :: NullOrUndefined (InstancePatchStatesList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeInstancePatchStatesRequest = DescribeInstancePatchStatesRequest 
  { "InstanceIds" :: (InstanceIdList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults)
  }


newtype DescribeInstancePatchStatesResult = DescribeInstancePatchStatesResult 
  { "InstancePatchStates" :: NullOrUndefined (InstancePatchStateList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeInstancePatchesRequest = DescribeInstancePatchesRequest 
  { "InstanceId" :: (InstanceId)
  , "Filters" :: NullOrUndefined (PatchOrchestratorFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults)
  }


newtype DescribeInstancePatchesResult = DescribeInstancePatchesResult 
  { "Patches" :: NullOrUndefined (PatchComplianceDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowExecutionTaskInvocationsRequest = DescribeMaintenanceWindowExecutionTaskInvocationsRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "TaskId" :: (MaintenanceWindowExecutionTaskId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowExecutionTaskInvocationsResult = DescribeMaintenanceWindowExecutionTaskInvocationsResult 
  { "WindowExecutionTaskInvocationIdentities" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationIdentityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowExecutionTasksRequest = DescribeMaintenanceWindowExecutionTasksRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowExecutionTasksResult = DescribeMaintenanceWindowExecutionTasksResult 
  { "WindowExecutionTaskIdentities" :: NullOrUndefined (MaintenanceWindowExecutionTaskIdentityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowExecutionsRequest = DescribeMaintenanceWindowExecutionsRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowExecutionsResult = DescribeMaintenanceWindowExecutionsResult 
  { "WindowExecutions" :: NullOrUndefined (MaintenanceWindowExecutionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowTargetsRequest = DescribeMaintenanceWindowTargetsRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowTargetsResult = DescribeMaintenanceWindowTargetsResult 
  { "Targets" :: NullOrUndefined (MaintenanceWindowTargetList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowTasksRequest = DescribeMaintenanceWindowTasksRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowTasksResult = DescribeMaintenanceWindowTasksResult 
  { "Tasks" :: NullOrUndefined (MaintenanceWindowTaskList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowsRequest = DescribeMaintenanceWindowsRequest 
  { "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeMaintenanceWindowsResult = DescribeMaintenanceWindowsResult 
  { "WindowIdentities" :: NullOrUndefined (MaintenanceWindowIdentityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeParametersRequest = DescribeParametersRequest 
  { "Filters" :: NullOrUndefined (ParametersFilterList)
  , "ParameterFilters" :: NullOrUndefined (ParameterStringFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeParametersResult = DescribeParametersResult 
  { "Parameters" :: NullOrUndefined (ParameterMetadataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribePatchBaselinesRequest = DescribePatchBaselinesRequest 
  { "Filters" :: NullOrUndefined (PatchOrchestratorFilterList)
  , "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribePatchBaselinesResult = DescribePatchBaselinesResult 
  { "BaselineIdentities" :: NullOrUndefined (PatchBaselineIdentityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribePatchGroupStateRequest = DescribePatchGroupStateRequest 
  { "PatchGroup" :: (PatchGroup)
  }


newtype DescribePatchGroupStateResult = DescribePatchGroupStateResult 
  { "Instances" :: NullOrUndefined (Int)
  , "InstancesWithInstalledPatches" :: NullOrUndefined (Int)
  , "InstancesWithInstalledOtherPatches" :: NullOrUndefined (Int)
  , "InstancesWithMissingPatches" :: NullOrUndefined (Int)
  , "InstancesWithFailedPatches" :: NullOrUndefined (Int)
  , "InstancesWithNotApplicablePatches" :: NullOrUndefined (Int)
  }


newtype DescribePatchGroupsRequest = DescribePatchGroupsRequest 
  { "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults)
  , "Filters" :: NullOrUndefined (PatchOrchestratorFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribePatchGroupsResult = DescribePatchGroupsResult 
  { "Mappings" :: NullOrUndefined (PatchGroupPatchBaselineMappingList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescriptionInDocument = DescriptionInDocument String


newtype DocumentARN = DocumentARN String


-- | <p>The specified document already exists.</p>
newtype DocumentAlreadyExists = DocumentAlreadyExists 
  { "Message" :: NullOrUndefined (String)
  }


newtype DocumentContent = DocumentContent String


-- | <p>A default version of a document.</p>
newtype DocumentDefaultVersionDescription = DocumentDefaultVersionDescription 
  { "Name" :: NullOrUndefined (DocumentName)
  , "DefaultVersion" :: NullOrUndefined (DocumentVersion)
  }


-- | <p>Describes a Systems Manager document. </p>
newtype DocumentDescription = DocumentDescription 
  { "Sha1" :: NullOrUndefined (DocumentSha1)
  , "Hash" :: NullOrUndefined (DocumentHash)
  , "HashType" :: NullOrUndefined (DocumentHashType)
  , "Name" :: NullOrUndefined (DocumentARN)
  , "Owner" :: NullOrUndefined (DocumentOwner)
  , "CreatedDate" :: NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined (DocumentStatus)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Description" :: NullOrUndefined (DescriptionInDocument)
  , "Parameters" :: NullOrUndefined (DocumentParameterList)
  , "PlatformTypes" :: NullOrUndefined (PlatformTypeList)
  , "DocumentType" :: NullOrUndefined (DocumentType)
  , "SchemaVersion" :: NullOrUndefined (DocumentSchemaVersion)
  , "LatestVersion" :: NullOrUndefined (DocumentVersion)
  , "DefaultVersion" :: NullOrUndefined (DocumentVersion)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined (TargetType)
  , "Tags" :: NullOrUndefined (TagList)
  }


-- | <p>Describes a filter.</p>
newtype DocumentFilter = DocumentFilter 
  { "Key'" :: (DocumentFilterKey)
  , "Value'" :: (DocumentFilterValue)
  }


newtype DocumentFilterKey = DocumentFilterKey String


newtype DocumentFilterList = DocumentFilterList (Array DocumentFilter)


newtype DocumentFilterValue = DocumentFilterValue String


newtype DocumentFormat = DocumentFormat String


newtype DocumentHash = DocumentHash String


newtype DocumentHashType = DocumentHashType String


-- | <p>Describes the name of a Systems Manager document.</p>
newtype DocumentIdentifier = DocumentIdentifier 
  { "Name" :: NullOrUndefined (DocumentARN)
  , "Owner" :: NullOrUndefined (DocumentOwner)
  , "PlatformTypes" :: NullOrUndefined (PlatformTypeList)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "DocumentType" :: NullOrUndefined (DocumentType)
  , "SchemaVersion" :: NullOrUndefined (DocumentSchemaVersion)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined (TargetType)
  , "Tags" :: NullOrUndefined (TagList)
  }


newtype DocumentIdentifierList = DocumentIdentifierList (Array DocumentIdentifier)


-- | <p>One or more filters. Use a filter to return a more specific list of documents.</p> <p>For keys, you can specify one or more tags that have been applied to a document. </p> <p>Other valid values include Owner, Name, PlatformTypes, and DocumentType.</p> <p>Note that only one Owner can be specified in a request. For example: <code>Key=Owner,Values=Self</code>.</p> <p>If you use Name as a key, you can use a name prefix to return a list of documents. For example, in the AWS CLI, to return a list of all documents that begin with <code>Te</code>, run the following command:</p> <p> <code>aws ssm list-documents --filters Key=Name,Values=Te</code> </p> <p>If you specify more than two keys, only documents that are identified by all the tags are returned in the results. If you specify more than two values for a key, documents that are identified by any of the values are returned in the results.</p> <p>To specify a custom key and value pair, use the format <code>Key=tag:[tagName],Values=[valueName]</code>.</p> <p>For example, if you created a Key called region and are using the AWS CLI to call the <code>list-documents</code> command: </p> <p> <code>aws ssm list-documents --filters Key=tag:region,Values=east,west Key=Owner,Values=Self</code> </p>
newtype DocumentKeyValuesFilter = DocumentKeyValuesFilter 
  { "Key" :: NullOrUndefined (DocumentKeyValuesFilterKey)
  , "Values" :: NullOrUndefined (DocumentKeyValuesFilterValues)
  }


newtype DocumentKeyValuesFilterKey = DocumentKeyValuesFilterKey String


newtype DocumentKeyValuesFilterList = DocumentKeyValuesFilterList (Array DocumentKeyValuesFilter)


newtype DocumentKeyValuesFilterValue = DocumentKeyValuesFilterValue String


newtype DocumentKeyValuesFilterValues = DocumentKeyValuesFilterValues (Array DocumentKeyValuesFilterValue)


-- | <p>You can have at most 200 active Systems Manager documents.</p>
newtype DocumentLimitExceeded = DocumentLimitExceeded 
  { "Message" :: NullOrUndefined (String)
  }


newtype DocumentName = DocumentName String


newtype DocumentOwner = DocumentOwner String


-- | <p>Parameters specified in a System Manager document that execute on the server when the command is run. </p>
newtype DocumentParameter = DocumentParameter 
  { "Name" :: NullOrUndefined (DocumentParameterName)
  , "Type" :: NullOrUndefined (DocumentParameterType)
  , "Description" :: NullOrUndefined (DocumentParameterDescrption)
  , "DefaultValue" :: NullOrUndefined (DocumentParameterDefaultValue)
  }


newtype DocumentParameterDefaultValue = DocumentParameterDefaultValue String


newtype DocumentParameterDescrption = DocumentParameterDescrption String


newtype DocumentParameterList = DocumentParameterList (Array DocumentParameter)


newtype DocumentParameterName = DocumentParameterName String


newtype DocumentParameterType = DocumentParameterType String


-- | <p>The document cannot be shared with more AWS user accounts. You can share a document with a maximum of 20 accounts. You can publicly share up to five documents. If you need to increase this limit, contact AWS Support.</p>
newtype DocumentPermissionLimit = DocumentPermissionLimit 
  { "Message" :: NullOrUndefined (String)
  }


newtype DocumentPermissionType = DocumentPermissionType String


newtype DocumentSchemaVersion = DocumentSchemaVersion String


newtype DocumentSha1 = DocumentSha1 String


newtype DocumentStatus = DocumentStatus String


newtype DocumentType = DocumentType String


newtype DocumentVersion = DocumentVersion String


-- | <p>Version information about the document.</p>
newtype DocumentVersionInfo = DocumentVersionInfo 
  { "Name" :: NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "CreatedDate" :: NullOrUndefined (DateTime)
  , "IsDefaultVersion" :: NullOrUndefined (Boolean)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  }


-- | <p>The document has too many versions. Delete one or more document versions and try again.</p>
newtype DocumentVersionLimitExceeded = DocumentVersionLimitExceeded 
  { "Message" :: NullOrUndefined (String)
  }


newtype DocumentVersionList = DocumentVersionList (Array DocumentVersionInfo)


newtype DocumentVersionNumber = DocumentVersionNumber String


-- | <p>Error returned when the ID specified for a resource, such as a Maintenance Window or Patch baseline, doesn't exist.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>
newtype DoesNotExistException = DoesNotExistException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The content of the association document matches another document. Change the content of the document and try again.</p>
newtype DuplicateDocumentContent = DuplicateDocumentContent 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>You cannot specify an instance ID in more than one association.</p>
newtype DuplicateInstanceId = DuplicateInstanceId 
  { 
  }


newtype EffectiveInstanceAssociationMaxResults = EffectiveInstanceAssociationMaxResults Int


-- | <p>The EffectivePatch structure defines metadata about a patch along with the approval state of the patch in a particular patch baseline. The approval state includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.</p>
newtype EffectivePatch = EffectivePatch 
  { "Patch" :: NullOrUndefined (Patch)
  , "PatchStatus" :: NullOrUndefined (PatchStatus)
  }


newtype EffectivePatchList = EffectivePatchList (Array EffectivePatch)


newtype ErrorCount = ErrorCount Int


newtype ExecutionMode = ExecutionMode String


newtype ExpirationDate = ExpirationDate Number


-- | <p>Describes a failed association.</p>
newtype FailedCreateAssociation = FailedCreateAssociation 
  { "Entry" :: NullOrUndefined (CreateAssociationBatchRequestEntry)
  , "Message" :: NullOrUndefined (BatchErrorMessage)
  , "Fault" :: NullOrUndefined (Fault)
  }


newtype FailedCreateAssociationList = FailedCreateAssociationList (Array FailedCreateAssociation)


-- | <p>Information about an Automation failure.</p>
newtype FailureDetails = FailureDetails 
  { "FailureStage" :: NullOrUndefined (String)
  , "FailureType" :: NullOrUndefined (String)
  , "Details" :: NullOrUndefined (AutomationParameterMap)
  }


newtype Fault = Fault String


-- | <p>You attempted to register a LAMBDA or STEP_FUNCTION task in a region where the corresponding service is not available. </p>
newtype FeatureNotAvailableException = FeatureNotAvailableException 
  { "Message" :: NullOrUndefined (String)
  }


newtype GetAutomationExecutionRequest = GetAutomationExecutionRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  }


newtype GetAutomationExecutionResult = GetAutomationExecutionResult 
  { "AutomationExecution" :: NullOrUndefined (AutomationExecution)
  }


newtype GetCommandInvocationRequest = GetCommandInvocationRequest 
  { "CommandId" :: (CommandId)
  , "InstanceId" :: (InstanceId)
  , "PluginName" :: NullOrUndefined (CommandPluginName)
  }


newtype GetCommandInvocationResult = GetCommandInvocationResult 
  { "CommandId" :: NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "Comment" :: NullOrUndefined (Comment)
  , "DocumentName" :: NullOrUndefined (DocumentName)
  , "PluginName" :: NullOrUndefined (CommandPluginName)
  , "ResponseCode" :: NullOrUndefined (ResponseCode)
  , "ExecutionStartDateTime" :: NullOrUndefined (StringDateTime)
  , "ExecutionElapsedTime" :: NullOrUndefined (StringDateTime)
  , "ExecutionEndDateTime" :: NullOrUndefined (StringDateTime)
  , "Status" :: NullOrUndefined (CommandInvocationStatus)
  , "StatusDetails" :: NullOrUndefined (StatusDetails)
  , "StandardOutputContent" :: NullOrUndefined (StandardOutputContent)
  , "StandardOutputUrl" :: NullOrUndefined (Url)
  , "StandardErrorContent" :: NullOrUndefined (StandardErrorContent)
  , "StandardErrorUrl" :: NullOrUndefined (Url)
  }


newtype GetDefaultPatchBaselineRequest = GetDefaultPatchBaselineRequest 
  { "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  }


newtype GetDefaultPatchBaselineResult = GetDefaultPatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  }


newtype GetDeployablePatchSnapshotForInstanceRequest = GetDeployablePatchSnapshotForInstanceRequest 
  { "InstanceId" :: (InstanceId)
  , "SnapshotId" :: (SnapshotId)
  }


newtype GetDeployablePatchSnapshotForInstanceResult = GetDeployablePatchSnapshotForInstanceResult 
  { "InstanceId" :: NullOrUndefined (InstanceId)
  , "SnapshotId" :: NullOrUndefined (SnapshotId)
  , "SnapshotDownloadUrl" :: NullOrUndefined (SnapshotDownloadUrl)
  , "Product" :: NullOrUndefined (Product)
  }


newtype GetDocumentRequest = GetDocumentRequest 
  { "Name" :: (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  }


newtype GetDocumentResult = GetDocumentResult 
  { "Name" :: NullOrUndefined (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Content" :: NullOrUndefined (DocumentContent)
  , "DocumentType" :: NullOrUndefined (DocumentType)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  }


newtype GetInventoryRequest = GetInventoryRequest 
  { "Filters" :: NullOrUndefined (InventoryFilterList)
  , "Aggregators" :: NullOrUndefined (InventoryAggregatorList)
  , "ResultAttributes" :: NullOrUndefined (ResultAttributeList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype GetInventoryResult = GetInventoryResult 
  { "Entities" :: NullOrUndefined (InventoryResultEntityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetInventorySchemaMaxResults = GetInventorySchemaMaxResults Int


newtype GetInventorySchemaRequest = GetInventorySchemaRequest 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeNameFilter)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (GetInventorySchemaMaxResults)
  , "Aggregator" :: NullOrUndefined (AggregatorSchemaOnly)
  , "SubType" :: NullOrUndefined (IsSubTypeSchema)
  }


newtype GetInventorySchemaResult = GetInventorySchemaResult 
  { "Schemas" :: NullOrUndefined (InventoryItemSchemaResultList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetMaintenanceWindowExecutionRequest = GetMaintenanceWindowExecutionRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  }


newtype GetMaintenanceWindowExecutionResult = GetMaintenanceWindowExecutionResult 
  { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskIds" :: NullOrUndefined (MaintenanceWindowExecutionTaskIdList)
  , "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined (DateTime)
  }


newtype GetMaintenanceWindowExecutionTaskInvocationRequest = GetMaintenanceWindowExecutionTaskInvocationRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "TaskId" :: (MaintenanceWindowExecutionTaskId)
  , "InvocationId" :: (MaintenanceWindowExecutionTaskInvocationId)
  }


newtype GetMaintenanceWindowExecutionTaskInvocationResult = GetMaintenanceWindowExecutionTaskInvocationResult 
  { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId)
  , "InvocationId" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationId)
  , "ExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskExecutionId)
  , "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType)
  , "Parameters" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationParameters)
  , "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined (DateTime)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTaskTargetId)
  }


newtype GetMaintenanceWindowExecutionTaskRequest = GetMaintenanceWindowExecutionTaskRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "TaskId" :: (MaintenanceWindowExecutionTaskId)
  }


newtype GetMaintenanceWindowExecutionTaskResult = GetMaintenanceWindowExecutionTaskResult 
  { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId)
  , "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn)
  , "ServiceRole" :: NullOrUndefined (ServiceRole)
  , "Type" :: NullOrUndefined (MaintenanceWindowTaskType)
  , "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParametersList)
  , "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  , "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined (DateTime)
  }


newtype GetMaintenanceWindowRequest = GetMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  }


newtype GetMaintenanceWindowResult = GetMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Schedule" :: NullOrUndefined (MaintenanceWindowSchedule)
  , "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours)
  , "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff)
  , "AllowUnassociatedTargets" :: NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets)
  , "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled)
  , "CreatedDate" :: NullOrUndefined (DateTime)
  , "ModifiedDate" :: NullOrUndefined (DateTime)
  }


newtype GetMaintenanceWindowTaskRequest = GetMaintenanceWindowTaskRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTaskId" :: (MaintenanceWindowTaskId)
  }


newtype GetMaintenanceWindowTaskResult = GetMaintenanceWindowTaskResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId)
  , "Targets" :: NullOrUndefined (Targets)
  , "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn)
  , "ServiceRoleArn" :: NullOrUndefined (ServiceRole)
  , "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType)
  , "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters)
  , "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters)
  , "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  , "LoggingInfo" :: NullOrUndefined (LoggingInfo)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  }


newtype GetParameterHistoryRequest = GetParameterHistoryRequest 
  { "Name" :: (PSParameterName)
  , "WithDecryption" :: NullOrUndefined (Boolean)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetParameterHistoryResult = GetParameterHistoryResult 
  { "Parameters" :: NullOrUndefined (ParameterHistoryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetParameterRequest = GetParameterRequest 
  { "Name" :: (PSParameterName)
  , "WithDecryption" :: NullOrUndefined (Boolean)
  }


newtype GetParameterResult = GetParameterResult 
  { "Parameter" :: NullOrUndefined (Parameter)
  }


newtype GetParametersByPathMaxResults = GetParametersByPathMaxResults Int


newtype GetParametersByPathRequest = GetParametersByPathRequest 
  { "Path" :: (PSParameterName)
  , "Recursive" :: NullOrUndefined (Boolean)
  , "ParameterFilters" :: NullOrUndefined (ParameterStringFilterList)
  , "WithDecryption" :: NullOrUndefined (Boolean)
  , "MaxResults" :: NullOrUndefined (GetParametersByPathMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetParametersByPathResult = GetParametersByPathResult 
  { "Parameters" :: NullOrUndefined (ParameterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetParametersRequest = GetParametersRequest 
  { "Names" :: (ParameterNameList)
  , "WithDecryption" :: NullOrUndefined (Boolean)
  }


newtype GetParametersResult = GetParametersResult 
  { "Parameters" :: NullOrUndefined (ParameterList)
  , "InvalidParameters" :: NullOrUndefined (ParameterNameList)
  }


newtype GetPatchBaselineForPatchGroupRequest = GetPatchBaselineForPatchGroupRequest 
  { "PatchGroup" :: (PatchGroup)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  }


newtype GetPatchBaselineForPatchGroupResult = GetPatchBaselineForPatchGroupResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "PatchGroup" :: NullOrUndefined (PatchGroup)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  }


newtype GetPatchBaselineRequest = GetPatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  }


newtype GetPatchBaselineResult = GetPatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "Name" :: NullOrUndefined (BaselineName)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  , "GlobalFilters" :: NullOrUndefined (PatchFilterGroup)
  , "ApprovalRules" :: NullOrUndefined (PatchRuleGroup)
  , "ApprovedPatches" :: NullOrUndefined (PatchIdList)
  , "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel)
  , "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean)
  , "RejectedPatches" :: NullOrUndefined (PatchIdList)
  , "PatchGroups" :: NullOrUndefined (PatchGroupList)
  , "CreatedDate" :: NullOrUndefined (DateTime)
  , "ModifiedDate" :: NullOrUndefined (DateTime)
  , "Description" :: NullOrUndefined (BaselineDescription)
  , "Sources" :: NullOrUndefined (PatchSourceList)
  }


-- | <p>A hierarchy can have a maximum of 15 levels. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html">Working with Systems Manager Parameters</a>. </p>
newtype HierarchyLevelLimitExceededException = HierarchyLevelLimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Parameter Store does not support changing a parameter type in a hierarchy. For example, you can't change a parameter from a String type to a SecureString type. You must create a new, unique parameter.</p>
newtype HierarchyTypeMismatchException = HierarchyTypeMismatchException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype IPAddress = IPAddress String


newtype IamRole = IamRole String


newtype IdempotencyToken = IdempotencyToken String


-- | <p>Error returned when an idempotent operation is retried and the parameters don't match the original call to the API with the same idempotency token. </p>
newtype IdempotentParameterMismatch = IdempotentParameterMismatch 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Status information about the aggregated associations.</p>
newtype InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview 
  { "DetailedStatus" :: NullOrUndefined (StatusName)
  , "InstanceAssociationStatusAggregatedCount" :: NullOrUndefined (InstanceAssociationStatusAggregatedCount)
  }


-- | <p>One or more association documents on the instance. </p>
newtype InstanceAssociation = InstanceAssociation 
  { "AssociationId" :: NullOrUndefined (AssociationId)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "Content" :: NullOrUndefined (DocumentContent)
  , "AssociationVersion" :: NullOrUndefined (AssociationVersion)
  }


newtype InstanceAssociationExecutionSummary = InstanceAssociationExecutionSummary String


newtype InstanceAssociationList = InstanceAssociationList (Array InstanceAssociation)


-- | <p>An Amazon S3 bucket where you want to store the results of this request.</p>
newtype InstanceAssociationOutputLocation = InstanceAssociationOutputLocation 
  { "S3Location" :: NullOrUndefined (S3OutputLocation)
  }


-- | <p>The URL of Amazon S3 bucket where you want to store the results of this request.</p>
newtype InstanceAssociationOutputUrl = InstanceAssociationOutputUrl 
  { "S3OutputUrl" :: NullOrUndefined (S3OutputUrl)
  }


newtype InstanceAssociationStatusAggregatedCount = InstanceAssociationStatusAggregatedCount (Map StatusName InstanceCount)


-- | <p>Status information about the instance association.</p>
newtype InstanceAssociationStatusInfo = InstanceAssociationStatusInfo 
  { "AssociationId" :: NullOrUndefined (AssociationId)
  , "Name" :: NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "AssociationVersion" :: NullOrUndefined (AssociationVersion)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "ExecutionDate" :: NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined (StatusName)
  , "DetailedStatus" :: NullOrUndefined (StatusName)
  , "ExecutionSummary" :: NullOrUndefined (InstanceAssociationExecutionSummary)
  , "ErrorCode" :: NullOrUndefined (AgentErrorCode)
  , "OutputUrl" :: NullOrUndefined (InstanceAssociationOutputUrl)
  , "AssociationName" :: NullOrUndefined (AssociationName)
  }


newtype InstanceAssociationStatusInfos = InstanceAssociationStatusInfos (Array InstanceAssociationStatusInfo)


newtype InstanceCount = InstanceCount Int


newtype InstanceId = InstanceId String


newtype InstanceIdList = InstanceIdList (Array InstanceId)


-- | <p>Describes a filter for a specific list of instances. </p>
newtype InstanceInformation = InstanceInformation 
  { "InstanceId" :: NullOrUndefined (InstanceId)
  , "PingStatus" :: NullOrUndefined (PingStatus)
  , "LastPingDateTime" :: NullOrUndefined (DateTime)
  , "AgentVersion" :: NullOrUndefined (Version)
  , "IsLatestVersion" :: NullOrUndefined (Boolean)
  , "PlatformType" :: NullOrUndefined (PlatformType)
  , "PlatformName" :: NullOrUndefined (String)
  , "PlatformVersion" :: NullOrUndefined (String)
  , "ActivationId" :: NullOrUndefined (ActivationId)
  , "IamRole" :: NullOrUndefined (IamRole)
  , "RegistrationDate" :: NullOrUndefined (DateTime)
  , "ResourceType" :: NullOrUndefined (ResourceType)
  , "Name" :: NullOrUndefined (String)
  , "IPAddress" :: NullOrUndefined (IPAddress)
  , "ComputerName" :: NullOrUndefined (ComputerName)
  , "AssociationStatus" :: NullOrUndefined (StatusName)
  , "LastAssociationExecutionDate" :: NullOrUndefined (DateTime)
  , "LastSuccessfulAssociationExecutionDate" :: NullOrUndefined (DateTime)
  , "AssociationOverview" :: NullOrUndefined (InstanceAggregatedAssociationOverview)
  }


-- | <p>Describes a filter for a specific list of instances. </p>
newtype InstanceInformationFilter = InstanceInformationFilter 
  { "Key'" :: (InstanceInformationFilterKey)
  , "ValueSet'" :: (InstanceInformationFilterValueSet)
  }


newtype InstanceInformationFilterKey = InstanceInformationFilterKey String


newtype InstanceInformationFilterList = InstanceInformationFilterList (Array InstanceInformationFilter)


newtype InstanceInformationFilterValue = InstanceInformationFilterValue String


newtype InstanceInformationFilterValueSet = InstanceInformationFilterValueSet (Array InstanceInformationFilterValue)


newtype InstanceInformationList = InstanceInformationList (Array InstanceInformation)


-- | <p>The filters to describe or get information about your managed instances.</p>
newtype InstanceInformationStringFilter = InstanceInformationStringFilter 
  { "Key" :: (InstanceInformationStringFilterKey)
  , "Values" :: (InstanceInformationFilterValueSet)
  }


newtype InstanceInformationStringFilterKey = InstanceInformationStringFilterKey String


newtype InstanceInformationStringFilterList = InstanceInformationStringFilterList (Array InstanceInformationStringFilter)


-- | <p>Defines the high-level patch compliance state for a managed instance, providing information about the number of installed, missing, not applicable, and failed patches along with metadata about the operation when this information was gathered for the instance.</p>
newtype InstancePatchState = InstancePatchState 
  { "InstanceId" :: (InstanceId)
  , "PatchGroup" :: (PatchGroup)
  , "BaselineId" :: (BaselineId)
  , "SnapshotId" :: NullOrUndefined (SnapshotId)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "InstalledCount" :: NullOrUndefined (PatchInstalledCount)
  , "InstalledOtherCount" :: NullOrUndefined (PatchInstalledOtherCount)
  , "MissingCount" :: NullOrUndefined (PatchMissingCount)
  , "FailedCount" :: NullOrUndefined (PatchFailedCount)
  , "NotApplicableCount" :: NullOrUndefined (PatchNotApplicableCount)
  , "OperationStartTime" :: (DateTime)
  , "OperationEndTime" :: (DateTime)
  , "Operation" :: (PatchOperationType)
  }


-- | <p>Defines a filter used in DescribeInstancePatchStatesForPatchGroup used to scope down the information returned by the API.</p>
newtype InstancePatchStateFilter = InstancePatchStateFilter 
  { "Key" :: (InstancePatchStateFilterKey)
  , "Values" :: (InstancePatchStateFilterValues)
  , "Type" :: (InstancePatchStateOperatorType)
  }


newtype InstancePatchStateFilterKey = InstancePatchStateFilterKey String


newtype InstancePatchStateFilterList = InstancePatchStateFilterList (Array InstancePatchStateFilter)


newtype InstancePatchStateFilterValue = InstancePatchStateFilterValue String


newtype InstancePatchStateFilterValues = InstancePatchStateFilterValues (Array InstancePatchStateFilterValue)


newtype InstancePatchStateList = InstancePatchStateList (Array InstancePatchState)


newtype InstancePatchStateOperatorType = InstancePatchStateOperatorType String


newtype InstancePatchStatesList = InstancePatchStatesList (Array InstancePatchState)


newtype InstanceTagName = InstanceTagName String


-- | <p>An error occurred on the server side.</p>
newtype InternalServerError = InternalServerError 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The activation is not valid. The activation might have been deleted, or the ActivationId and the ActivationCode do not match.</p>
newtype InvalidActivation = InvalidActivation 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The activation ID is not valid. Verify the you entered the correct ActivationId or ActivationCode and try again.</p>
newtype InvalidActivationId = InvalidActivationId 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The request does not meet the regular expression requirement.</p>
newtype InvalidAllowedPatternException = InvalidAllowedPatternException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>The version you specified is not valid. Use ListAssociationVersions to view all versions of an association according to the association ID. Or, use the <code>$LATEST</code> parameter to view the latest version of the association.</p>
newtype InvalidAssociationVersion = InvalidAssociationVersion 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The supplied parameters for invoking the specified Automation document are incorrect. For example, they may not match the set of parameters permitted for the specified Automation document.</p>
newtype InvalidAutomationExecutionParametersException = InvalidAutomationExecutionParametersException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The signal is not valid for the current Automation execution.</p>
newtype InvalidAutomationSignalException = InvalidAutomationSignalException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The specified update status operation is not valid.</p>
newtype InvalidAutomationStatusUpdateException = InvalidAutomationStatusUpdateException 
  { "Message" :: NullOrUndefined (String)
  }


newtype InvalidCommandId = InvalidCommandId 
  { 
  }


-- | <p>The specified document does not exist.</p>
newtype InvalidDocument = InvalidDocument 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The content for the document is not valid.</p>
newtype InvalidDocumentContent = InvalidDocumentContent 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>You attempted to delete a document while it is still shared. You must stop sharing the document before you can delete it.</p>
newtype InvalidDocumentOperation = InvalidDocumentOperation 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The version of the document schema is not supported.</p>
newtype InvalidDocumentSchemaVersion = InvalidDocumentSchemaVersion 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The document version is not valid or does not exist.</p>
newtype InvalidDocumentVersion = InvalidDocumentVersion 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The filter name is not valid. Verify the you entered the correct name and try again.</p>
newtype InvalidFilter = InvalidFilter 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The specified key is not valid.</p>
newtype InvalidFilterKey = InvalidFilterKey 
  { 
  }


-- | <p>The specified filter option is not valid. Valid options are Equals and BeginsWith. For Path filter, valid options are Recursive and OneLevel.</p>
newtype InvalidFilterOption = InvalidFilterOption 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>The filter value is not valid. Verify the value and try again.</p>
newtype InvalidFilterValue = InvalidFilterValue 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The following problems can cause this exception:</p> <p>You do not have permission to access the instance.</p> <p>The SSM Agent is not running. On managed instances and Linux instances, verify that the SSM Agent is running. On EC2 Windows instances, verify that the EC2Config service is running.</p> <p>The SSM Agent or EC2Config service is not registered to the SSM endpoint. Try reinstalling the SSM Agent or EC2Config service.</p> <p>The instance is not in valid state. Valid states are: Running, Pending, Stopped, Stopping. Invalid states are: Shutting-down and Terminated.</p>
newtype InvalidInstanceId = InvalidInstanceId 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The specified filter value is not valid.</p>
newtype InvalidInstanceInformationFilterValue = InvalidInstanceInformationFilterValue 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>You specified invalid keys or values in the <code>Context</code> attribute for <code>InventoryItem</code>. Verify the keys and values, and try again.</p>
newtype InvalidInventoryItemContextException = InvalidInventoryItemContextException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>One or more content items is not valid.</p>
newtype InvalidItemContentException = InvalidItemContentException 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined (String)
  }


-- | <p>The query key ID is not valid.</p>
newtype InvalidKeyId = InvalidKeyId 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>The specified token is not valid.</p>
newtype InvalidNextToken = InvalidNextToken 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>One or more configuration items is not valid. Verify that a valid Amazon Resource Name (ARN) was provided for an Amazon SNS topic.</p>
newtype InvalidNotificationConfig = InvalidNotificationConfig 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The S3 bucket does not exist.</p>
newtype InvalidOutputFolder = InvalidOutputFolder 
  { 
  }


-- | <p>The output location is not valid or does not exist.</p>
newtype InvalidOutputLocation = InvalidOutputLocation 
  { 
  }


-- | <p>You must specify values for all required parameters in the Systems Manager document. You can only supply values to parameters defined in the Systems Manager document.</p>
newtype InvalidParameters = InvalidParameters 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The permission type is not supported. <i>Share</i> is the only supported permission type.</p>
newtype InvalidPermissionType = InvalidPermissionType 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The plugin name is not valid.</p>
newtype InvalidPluginName = InvalidPluginName 
  { 
  }


-- | <p>The resource ID is not valid. Verify that you entered the correct ID and try again.</p>
newtype InvalidResourceId = InvalidResourceId 
  { 
  }


-- | <p>The resource type is not valid. For example, if you are attempting to tag an instance, the instance must be a registered, managed instance.</p>
newtype InvalidResourceType = InvalidResourceType 
  { 
  }


-- | <p>The specified inventory item result attribute is not valid.</p>
newtype InvalidResultAttributeException = InvalidResultAttributeException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The role name can't contain invalid characters. Also verify that you specified an IAM role for notifications that includes the required trust policy. For information about configuring the IAM role for Run Command notifications, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/rc-sns-notifications.html">Configuring Amazon SNS Notifications for Run Command</a> in the <i>AWS Systems Manager User Guide</i>.</p>
newtype InvalidRole = InvalidRole 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The schedule is invalid. Verify your cron or rate expression and try again.</p>
newtype InvalidSchedule = InvalidSchedule 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The target is not valid or does not exist. It might not be configured for EC2 Systems Manager or you might not have permission to perform the operation.</p>
newtype InvalidTarget = InvalidTarget 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The parameter type name is not valid.</p>
newtype InvalidTypeNameException = InvalidTypeNameException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The update is not valid.</p>
newtype InvalidUpdate = InvalidUpdate 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Specifies the inventory type and attribute for the aggregation execution.</p>
newtype InventoryAggregator = InventoryAggregator 
  { "Expression" :: NullOrUndefined (InventoryAggregatorExpression)
  , "Aggregators" :: NullOrUndefined (InventoryAggregatorList)
  }


newtype InventoryAggregatorExpression = InventoryAggregatorExpression String


newtype InventoryAggregatorList = InventoryAggregatorList (Array InventoryAggregator)


newtype InventoryAttributeDataType = InventoryAttributeDataType String


-- | <p>One or more filters. Use a filter to return a more specific list of results.</p>
newtype InventoryFilter = InventoryFilter 
  { "Key" :: (InventoryFilterKey)
  , "Values" :: (InventoryFilterValueList)
  , "Type" :: NullOrUndefined (InventoryQueryOperatorType)
  }


newtype InventoryFilterKey = InventoryFilterKey String


newtype InventoryFilterList = InventoryFilterList (Array InventoryFilter)


newtype InventoryFilterValue = InventoryFilterValue String


newtype InventoryFilterValueList = InventoryFilterValueList (Array InventoryFilterValue)


-- | <p>Information collected from managed instances based on your inventory policy document</p>
newtype InventoryItem = InventoryItem 
  { "TypeName" :: (InventoryItemTypeName)
  , "SchemaVersion" :: (InventoryItemSchemaVersion)
  , "CaptureTime" :: (InventoryItemCaptureTime)
  , "ContentHash" :: NullOrUndefined (InventoryItemContentHash)
  , "Content" :: NullOrUndefined (InventoryItemEntryList)
  , "Context" :: NullOrUndefined (InventoryItemContentContext)
  }


-- | <p>Attributes are the entries within the inventory item content. It contains name and value.</p>
newtype InventoryItemAttribute = InventoryItemAttribute 
  { "Name" :: (InventoryItemAttributeName)
  , "DataType" :: (InventoryAttributeDataType)
  }


newtype InventoryItemAttributeList = InventoryItemAttributeList (Array InventoryItemAttribute)


newtype InventoryItemAttributeName = InventoryItemAttributeName String


newtype InventoryItemCaptureTime = InventoryItemCaptureTime String


newtype InventoryItemContentContext = InventoryItemContentContext (Map AttributeName AttributeValue)


newtype InventoryItemContentHash = InventoryItemContentHash String


newtype InventoryItemEntry = InventoryItemEntry (Map AttributeName AttributeValue)


newtype InventoryItemEntryList = InventoryItemEntryList (Array InventoryItemEntry)


newtype InventoryItemList = InventoryItemList (Array InventoryItem)


-- | <p>The inventory item schema definition. Users can use this to compose inventory query filters.</p>
newtype InventoryItemSchema = InventoryItemSchema 
  { "TypeName" :: (InventoryItemTypeName)
  , "Version" :: NullOrUndefined (InventoryItemSchemaVersion)
  , "Attributes" :: (InventoryItemAttributeList)
  , "DisplayName" :: NullOrUndefined (InventoryTypeDisplayName)
  }


newtype InventoryItemSchemaResultList = InventoryItemSchemaResultList (Array InventoryItemSchema)


newtype InventoryItemSchemaVersion = InventoryItemSchemaVersion String


newtype InventoryItemTypeName = InventoryItemTypeName String


newtype InventoryItemTypeNameFilter = InventoryItemTypeNameFilter String


newtype InventoryQueryOperatorType = InventoryQueryOperatorType String


-- | <p>Inventory query results.</p>
newtype InventoryResultEntity = InventoryResultEntity 
  { "Id" :: NullOrUndefined (InventoryResultEntityId)
  , "Data" :: NullOrUndefined (InventoryResultItemMap)
  }


newtype InventoryResultEntityId = InventoryResultEntityId String


newtype InventoryResultEntityList = InventoryResultEntityList (Array InventoryResultEntity)


-- | <p>The inventory result item.</p>
newtype InventoryResultItem = InventoryResultItem 
  { "TypeName" :: (InventoryItemTypeName)
  , "SchemaVersion" :: (InventoryItemSchemaVersion)
  , "CaptureTime" :: NullOrUndefined (InventoryItemCaptureTime)
  , "ContentHash" :: NullOrUndefined (InventoryItemContentHash)
  , "Content" :: (InventoryItemEntryList)
  }


newtype InventoryResultItemKey = InventoryResultItemKey String


newtype InventoryResultItemMap = InventoryResultItemMap (Map InventoryResultItemKey InventoryResultItem)


newtype InventoryTypeDisplayName = InventoryTypeDisplayName String


-- | <p>The command ID and instance ID you specified did not match any invocations. Verify the command ID adn the instance ID and try again. </p>
newtype InvocationDoesNotExist = InvocationDoesNotExist 
  { 
  }


newtype InvocationTraceOutput = InvocationTraceOutput String


newtype IsSubTypeSchema = IsSubTypeSchema Boolean


-- | <p>The inventory item has invalid content. </p>
newtype ItemContentMismatchException = ItemContentMismatchException 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined (String)
  }


-- | <p>The inventory item size has exceeded the size limit.</p>
newtype ItemSizeLimitExceededException = ItemSizeLimitExceededException 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined (String)
  }


newtype KeyList = KeyList (Array TagKey)


newtype LastResourceDataSyncStatus = LastResourceDataSyncStatus String


newtype LastResourceDataSyncTime = LastResourceDataSyncTime Number


newtype LastSuccessfulResourceDataSyncTime = LastSuccessfulResourceDataSyncTime Number


newtype ListAssociationVersionsRequest = ListAssociationVersionsRequest 
  { "AssociationId" :: (AssociationId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListAssociationVersionsResult = ListAssociationVersionsResult 
  { "AssociationVersions" :: NullOrUndefined (AssociationVersionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListAssociationsRequest = ListAssociationsRequest 
  { "AssociationFilterList" :: NullOrUndefined (AssociationFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListAssociationsResult = ListAssociationsResult 
  { "Associations" :: NullOrUndefined (AssociationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListCommandInvocationsRequest = ListCommandInvocationsRequest 
  { "CommandId" :: NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "MaxResults" :: NullOrUndefined (CommandMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Filters" :: NullOrUndefined (CommandFilterList)
  , "Details" :: NullOrUndefined (Boolean)
  }


newtype ListCommandInvocationsResult = ListCommandInvocationsResult 
  { "CommandInvocations" :: NullOrUndefined (CommandInvocationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListCommandsRequest = ListCommandsRequest 
  { "CommandId" :: NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "MaxResults" :: NullOrUndefined (CommandMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Filters" :: NullOrUndefined (CommandFilterList)
  }


newtype ListCommandsResult = ListCommandsResult 
  { "Commands" :: NullOrUndefined (CommandList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListComplianceItemsRequest = ListComplianceItemsRequest 
  { "Filters" :: NullOrUndefined (ComplianceStringFilterList)
  , "ResourceIds" :: NullOrUndefined (ComplianceResourceIdList)
  , "ResourceTypes" :: NullOrUndefined (ComplianceResourceTypeList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListComplianceItemsResult = ListComplianceItemsResult 
  { "ComplianceItems" :: NullOrUndefined (ComplianceItemList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListComplianceSummariesRequest = ListComplianceSummariesRequest 
  { "Filters" :: NullOrUndefined (ComplianceStringFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListComplianceSummariesResult = ListComplianceSummariesResult 
  { "ComplianceSummaryItems" :: NullOrUndefined (ComplianceSummaryItemList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListDocumentVersionsRequest = ListDocumentVersionsRequest 
  { "Name" :: (DocumentName)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListDocumentVersionsResult = ListDocumentVersionsResult 
  { "DocumentVersions" :: NullOrUndefined (DocumentVersionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListDocumentsRequest = ListDocumentsRequest 
  { "DocumentFilterList" :: NullOrUndefined (DocumentFilterList)
  , "Filters" :: NullOrUndefined (DocumentKeyValuesFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListDocumentsResult = ListDocumentsResult 
  { "DocumentIdentifiers" :: NullOrUndefined (DocumentIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListInventoryEntriesRequest = ListInventoryEntriesRequest 
  { "InstanceId" :: (InstanceId)
  , "TypeName" :: (InventoryItemTypeName)
  , "Filters" :: NullOrUndefined (InventoryFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListInventoryEntriesResult = ListInventoryEntriesResult 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "SchemaVersion" :: NullOrUndefined (InventoryItemSchemaVersion)
  , "CaptureTime" :: NullOrUndefined (InventoryItemCaptureTime)
  , "Entries" :: NullOrUndefined (InventoryItemEntryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListResourceComplianceSummariesRequest = ListResourceComplianceSummariesRequest 
  { "Filters" :: NullOrUndefined (ComplianceStringFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListResourceComplianceSummariesResult = ListResourceComplianceSummariesResult 
  { "ResourceComplianceSummaryItems" :: NullOrUndefined (ResourceComplianceSummaryItemList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListResourceDataSyncRequest = ListResourceDataSyncRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListResourceDataSyncResult = ListResourceDataSyncResult 
  { "ResourceDataSyncItems" :: NullOrUndefined (ResourceDataSyncItemList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceType" :: (ResourceTypeForTagging)
  , "ResourceId" :: (ResourceId)
  }


newtype ListTagsForResourceResult = ListTagsForResourceResult 
  { "TagList" :: NullOrUndefined (TagList)
  }


-- | <p>Information about an Amazon S3 bucket to write instance-level logs to.</p>
newtype LoggingInfo = LoggingInfo 
  { "S3BucketName" :: (S3BucketName)
  , "S3KeyPrefix" :: NullOrUndefined (S3KeyPrefix)
  , "S3Region" :: (S3Region)
  }


newtype MaintenanceWindowAllowUnassociatedTargets = MaintenanceWindowAllowUnassociatedTargets Boolean


-- | <p>The parameters for an AUTOMATION task type.</p>
newtype MaintenanceWindowAutomationParameters = MaintenanceWindowAutomationParameters 
  { "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Parameters" :: NullOrUndefined (AutomationParameterMap)
  }


newtype MaintenanceWindowCutoff = MaintenanceWindowCutoff Int


newtype MaintenanceWindowDescription = MaintenanceWindowDescription String


newtype MaintenanceWindowDurationHours = MaintenanceWindowDurationHours Int


newtype MaintenanceWindowEnabled = MaintenanceWindowEnabled Boolean


-- | <p>Describes the information about an execution of a Maintenance Window. </p>
newtype MaintenanceWindowExecution = MaintenanceWindowExecution 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId)
  , "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined (DateTime)
  }


newtype MaintenanceWindowExecutionId = MaintenanceWindowExecutionId String


newtype MaintenanceWindowExecutionList = MaintenanceWindowExecutionList (Array MaintenanceWindowExecution)


newtype MaintenanceWindowExecutionStatus = MaintenanceWindowExecutionStatus String


newtype MaintenanceWindowExecutionStatusDetails = MaintenanceWindowExecutionStatusDetails String


newtype MaintenanceWindowExecutionTaskExecutionId = MaintenanceWindowExecutionTaskExecutionId String


newtype MaintenanceWindowExecutionTaskId = MaintenanceWindowExecutionTaskId String


newtype MaintenanceWindowExecutionTaskIdList = MaintenanceWindowExecutionTaskIdList (Array MaintenanceWindowExecutionTaskId)


-- | <p>Information about a task execution performed as part of a Maintenance Window execution.</p>
newtype MaintenanceWindowExecutionTaskIdentity = MaintenanceWindowExecutionTaskIdentity 
  { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId)
  , "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined (DateTime)
  , "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn)
  , "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType)
  }


newtype MaintenanceWindowExecutionTaskIdentityList = MaintenanceWindowExecutionTaskIdentityList (Array MaintenanceWindowExecutionTaskIdentity)


newtype MaintenanceWindowExecutionTaskInvocationId = MaintenanceWindowExecutionTaskInvocationId String


-- | <p>Describes the information about a task invocation for a particular target as part of a task execution performed as part of a Maintenance Window execution.</p>
newtype MaintenanceWindowExecutionTaskInvocationIdentity = MaintenanceWindowExecutionTaskInvocationIdentity 
  { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskId)
  , "InvocationId" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationId)
  , "ExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionTaskExecutionId)
  , "TaskType" :: NullOrUndefined (MaintenanceWindowTaskType)
  , "Parameters" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationParameters)
  , "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined (DateTime)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTaskTargetId)
  }


newtype MaintenanceWindowExecutionTaskInvocationIdentityList = MaintenanceWindowExecutionTaskInvocationIdentityList (Array MaintenanceWindowExecutionTaskInvocationIdentity)


newtype MaintenanceWindowExecutionTaskInvocationParameters = MaintenanceWindowExecutionTaskInvocationParameters String


-- | <p>Filter used in the request.</p>
newtype MaintenanceWindowFilter = MaintenanceWindowFilter 
  { "Key" :: NullOrUndefined (MaintenanceWindowFilterKey)
  , "Values" :: NullOrUndefined (MaintenanceWindowFilterValues)
  }


newtype MaintenanceWindowFilterKey = MaintenanceWindowFilterKey String


newtype MaintenanceWindowFilterList = MaintenanceWindowFilterList (Array MaintenanceWindowFilter)


newtype MaintenanceWindowFilterValue = MaintenanceWindowFilterValue String


newtype MaintenanceWindowFilterValues = MaintenanceWindowFilterValues (Array MaintenanceWindowFilterValue)


newtype MaintenanceWindowId = MaintenanceWindowId String


-- | <p>Information about the Maintenance Window.</p>
newtype MaintenanceWindowIdentity = MaintenanceWindowIdentity 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled)
  , "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours)
  , "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff)
  }


newtype MaintenanceWindowIdentityList = MaintenanceWindowIdentityList (Array MaintenanceWindowIdentity)


newtype MaintenanceWindowLambdaClientContext = MaintenanceWindowLambdaClientContext String


-- | <p>The parameters for a LAMBDA task type.</p>
newtype MaintenanceWindowLambdaParameters = MaintenanceWindowLambdaParameters 
  { "ClientContext" :: NullOrUndefined (MaintenanceWindowLambdaClientContext)
  , "Qualifier" :: NullOrUndefined (MaintenanceWindowLambdaQualifier)
  , "Payload" :: NullOrUndefined (MaintenanceWindowLambdaPayload)
  }


newtype MaintenanceWindowLambdaPayload = MaintenanceWindowLambdaPayload String


newtype MaintenanceWindowLambdaQualifier = MaintenanceWindowLambdaQualifier String


newtype MaintenanceWindowMaxResults = MaintenanceWindowMaxResults Int


newtype MaintenanceWindowName = MaintenanceWindowName String


newtype MaintenanceWindowResourceType = MaintenanceWindowResourceType String


-- | <p>The parameters for a RUN_COMMAND task type.</p>
newtype MaintenanceWindowRunCommandParameters = MaintenanceWindowRunCommandParameters 
  { "Comment" :: NullOrUndefined (Comment)
  , "DocumentHash" :: NullOrUndefined (DocumentHash)
  , "DocumentHashType" :: NullOrUndefined (DocumentHashType)
  , "NotificationConfig" :: NullOrUndefined (NotificationConfig)
  , "OutputS3BucketName" :: NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "ServiceRoleArn" :: NullOrUndefined (ServiceRole)
  , "TimeoutSeconds" :: NullOrUndefined (TimeoutSeconds)
  }


newtype MaintenanceWindowSchedule = MaintenanceWindowSchedule String


newtype MaintenanceWindowStepFunctionsInput = MaintenanceWindowStepFunctionsInput String


newtype MaintenanceWindowStepFunctionsName = MaintenanceWindowStepFunctionsName String


-- | <p>The parameters for the STEP_FUNCTION execution.</p>
newtype MaintenanceWindowStepFunctionsParameters = MaintenanceWindowStepFunctionsParameters 
  { "Input" :: NullOrUndefined (MaintenanceWindowStepFunctionsInput)
  , "Name" :: NullOrUndefined (MaintenanceWindowStepFunctionsName)
  }


-- | <p>The target registered with the Maintenance Window.</p>
newtype MaintenanceWindowTarget = MaintenanceWindowTarget 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId)
  , "ResourceType" :: NullOrUndefined (MaintenanceWindowResourceType)
  , "Targets" :: NullOrUndefined (Targets)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  }


newtype MaintenanceWindowTargetId = MaintenanceWindowTargetId String


newtype MaintenanceWindowTargetList = MaintenanceWindowTargetList (Array MaintenanceWindowTarget)


-- | <p>Information about a task defined for a Maintenance Window.</p>
newtype MaintenanceWindowTask = MaintenanceWindowTask 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId)
  , "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn)
  , "Type" :: NullOrUndefined (MaintenanceWindowTaskType)
  , "Targets" :: NullOrUndefined (Targets)
  , "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters)
  , "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority)
  , "LoggingInfo" :: NullOrUndefined (LoggingInfo)
  , "ServiceRoleArn" :: NullOrUndefined (ServiceRole)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  }


newtype MaintenanceWindowTaskArn = MaintenanceWindowTaskArn String


newtype MaintenanceWindowTaskId = MaintenanceWindowTaskId String


-- | <p>The parameters for task execution.</p>
newtype MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters 
  { "RunCommand" :: NullOrUndefined (MaintenanceWindowRunCommandParameters)
  , "Automation" :: NullOrUndefined (MaintenanceWindowAutomationParameters)
  , "StepFunctions" :: NullOrUndefined (MaintenanceWindowStepFunctionsParameters)
  , "Lambda" :: NullOrUndefined (MaintenanceWindowLambdaParameters)
  }


newtype MaintenanceWindowTaskList = MaintenanceWindowTaskList (Array MaintenanceWindowTask)


newtype MaintenanceWindowTaskParameterName = MaintenanceWindowTaskParameterName String


newtype MaintenanceWindowTaskParameterValue = MaintenanceWindowTaskParameterValue String


-- | <p>Defines the values for a task parameter.</p>
newtype MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression 
  { "Values" :: NullOrUndefined (MaintenanceWindowTaskParameterValueList)
  }


newtype MaintenanceWindowTaskParameterValueList = MaintenanceWindowTaskParameterValueList (Array MaintenanceWindowTaskParameterValue)


newtype MaintenanceWindowTaskParameters = MaintenanceWindowTaskParameters (Map MaintenanceWindowTaskParameterName MaintenanceWindowTaskParameterValueExpression)


newtype MaintenanceWindowTaskParametersList = MaintenanceWindowTaskParametersList (Array MaintenanceWindowTaskParameters)


newtype MaintenanceWindowTaskPriority = MaintenanceWindowTaskPriority Int


newtype MaintenanceWindowTaskTargetId = MaintenanceWindowTaskTargetId String


newtype MaintenanceWindowTaskType = MaintenanceWindowTaskType String


newtype ManagedInstanceId = ManagedInstanceId String


newtype MaxConcurrency = MaxConcurrency String


-- | <p>The size limit of a document is 64 KB.</p>
newtype MaxDocumentSizeExceeded = MaxDocumentSizeExceeded 
  { "Message" :: NullOrUndefined (String)
  }


newtype MaxErrors = MaxErrors String


newtype MaxResults = MaxResults Int


newtype MaxResultsEC2Compatible = MaxResultsEC2Compatible Int


newtype ModifyDocumentPermissionRequest = ModifyDocumentPermissionRequest 
  { "Name" :: (DocumentName)
  , "PermissionType" :: (DocumentPermissionType)
  , "AccountIdsToAdd" :: NullOrUndefined (AccountIdList)
  , "AccountIdsToRemove" :: NullOrUndefined (AccountIdList)
  }


newtype ModifyDocumentPermissionResponse = ModifyDocumentPermissionResponse 
  { 
  }


newtype NextToken = NextToken String


-- | <p>A summary of resources that are not compliant. The summary is organized according to resource type.</p>
newtype NonCompliantSummary = NonCompliantSummary 
  { "NonCompliantCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "SeveritySummary" :: NullOrUndefined (SeveritySummary)
  }


newtype NormalStringMap = NormalStringMap (Map String String)


newtype NotificationArn = NotificationArn String


-- | <p>Configurations for sending notifications.</p>
newtype NotificationConfig = NotificationConfig 
  { "NotificationArn" :: NullOrUndefined (NotificationArn)
  , "NotificationEvents" :: NullOrUndefined (NotificationEventList)
  , "NotificationType" :: NullOrUndefined (NotificationType)
  }


newtype NotificationEvent = NotificationEvent String


newtype NotificationEventList = NotificationEventList (Array NotificationEvent)


newtype NotificationType = NotificationType String


newtype OperatingSystem = OperatingSystem String


newtype OwnerInformation = OwnerInformation String


newtype PSParameterName = PSParameterName String


newtype PSParameterValue = PSParameterValue String


newtype PSParameterVersion = PSParameterVersion Number


-- | <p>An Amazon EC2 Systems Manager parameter in Parameter Store.</p>
newtype Parameter = Parameter 
  { "Name" :: NullOrUndefined (PSParameterName)
  , "Type" :: NullOrUndefined (ParameterType)
  , "Value" :: NullOrUndefined (PSParameterValue)
  , "Version" :: NullOrUndefined (PSParameterVersion)
  }


-- | <p>The parameter already exists. You can't create duplicate parameters.</p>
newtype ParameterAlreadyExists = ParameterAlreadyExists 
  { "Message'" :: NullOrUndefined (String)
  }


newtype ParameterDescription = ParameterDescription String


-- | <p>Information about parameter usage.</p>
newtype ParameterHistory = ParameterHistory 
  { "Name" :: NullOrUndefined (PSParameterName)
  , "Type" :: NullOrUndefined (ParameterType)
  , "KeyId" :: NullOrUndefined (ParameterKeyId)
  , "LastModifiedDate" :: NullOrUndefined (DateTime)
  , "LastModifiedUser" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (ParameterDescription)
  , "Value" :: NullOrUndefined (PSParameterValue)
  , "AllowedPattern" :: NullOrUndefined (AllowedPattern)
  , "Version" :: NullOrUndefined (PSParameterVersion)
  }


newtype ParameterHistoryList = ParameterHistoryList (Array ParameterHistory)


newtype ParameterKeyId = ParameterKeyId String


-- | <p>You have exceeded the number of parameters for this AWS account. Delete one or more parameters and try again.</p>
newtype ParameterLimitExceeded = ParameterLimitExceeded 
  { "Message'" :: NullOrUndefined (String)
  }


newtype ParameterList = ParameterList (Array Parameter)


-- | <p>The parameter exceeded the maximum number of allowed versions.</p>
newtype ParameterMaxVersionLimitExceeded = ParameterMaxVersionLimitExceeded 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>Metada includes information like the ARN of the last user and the date/time the parameter was last used.</p>
newtype ParameterMetadata = ParameterMetadata 
  { "Name" :: NullOrUndefined (PSParameterName)
  , "Type" :: NullOrUndefined (ParameterType)
  , "KeyId" :: NullOrUndefined (ParameterKeyId)
  , "LastModifiedDate" :: NullOrUndefined (DateTime)
  , "LastModifiedUser" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (ParameterDescription)
  , "AllowedPattern" :: NullOrUndefined (AllowedPattern)
  , "Version" :: NullOrUndefined (PSParameterVersion)
  }


newtype ParameterMetadataList = ParameterMetadataList (Array ParameterMetadata)


newtype ParameterName = ParameterName String


newtype ParameterNameList = ParameterNameList (Array PSParameterName)


-- | <p>The parameter could not be found. Verify the name and try again.</p>
newtype ParameterNotFound = ParameterNotFound 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>The parameter name is not valid.</p>
newtype ParameterPatternMismatchException = ParameterPatternMismatchException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>One or more filters. Use a filter to return a more specific list of results.</p>
newtype ParameterStringFilter = ParameterStringFilter 
  { "Key" :: (ParameterStringFilterKey)
  , "Option" :: NullOrUndefined (ParameterStringQueryOption)
  , "Values" :: NullOrUndefined (ParameterStringFilterValueList)
  }


newtype ParameterStringFilterKey = ParameterStringFilterKey String


newtype ParameterStringFilterList = ParameterStringFilterList (Array ParameterStringFilter)


newtype ParameterStringFilterValue = ParameterStringFilterValue String


newtype ParameterStringFilterValueList = ParameterStringFilterValueList (Array ParameterStringFilterValue)


newtype ParameterStringQueryOption = ParameterStringQueryOption String


newtype ParameterType = ParameterType String


newtype ParameterValue = ParameterValue String


newtype ParameterValueList = ParameterValueList (Array ParameterValue)


-- | <p>The specified parameter version was not found. Verify the parameter name and version, and try again.</p>
newtype ParameterVersionNotFound = ParameterVersionNotFound 
  { "Message'" :: NullOrUndefined (String)
  }


newtype Parameters = Parameters (Map ParameterName ParameterValueList)


-- | <p>This data type is deprecated. Instead, use <a>ParameterStringFilter</a>.</p>
newtype ParametersFilter = ParametersFilter 
  { "Key" :: (ParametersFilterKey)
  , "Values" :: (ParametersFilterValueList)
  }


newtype ParametersFilterKey = ParametersFilterKey String


newtype ParametersFilterList = ParametersFilterList (Array ParametersFilter)


newtype ParametersFilterValue = ParametersFilterValue String


newtype ParametersFilterValueList = ParametersFilterValueList (Array ParametersFilterValue)


-- | <p>Represents metadata about a patch.</p>
newtype Patch = Patch 
  { "Id" :: NullOrUndefined (PatchId)
  , "ReleaseDate" :: NullOrUndefined (DateTime)
  , "Title" :: NullOrUndefined (PatchTitle)
  , "Description" :: NullOrUndefined (PatchDescription)
  , "ContentUrl" :: NullOrUndefined (PatchContentUrl)
  , "Vendor" :: NullOrUndefined (PatchVendor)
  , "ProductFamily" :: NullOrUndefined (PatchProductFamily)
  , "Product" :: NullOrUndefined (PatchProduct)
  , "Classification" :: NullOrUndefined (PatchClassification)
  , "MsrcSeverity" :: NullOrUndefined (PatchMsrcSeverity)
  , "KbNumber" :: NullOrUndefined (PatchKbNumber)
  , "MsrcNumber" :: NullOrUndefined (PatchMsrcNumber)
  , "Language" :: NullOrUndefined (PatchLanguage)
  }


-- | <p>Defines the basic information about a patch baseline.</p>
newtype PatchBaselineIdentity = PatchBaselineIdentity 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "BaselineName" :: NullOrUndefined (BaselineName)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  , "BaselineDescription" :: NullOrUndefined (BaselineDescription)
  , "DefaultBaseline" :: NullOrUndefined (DefaultBaseline)
  }


newtype PatchBaselineIdentityList = PatchBaselineIdentityList (Array PatchBaselineIdentity)


newtype PatchBaselineMaxResults = PatchBaselineMaxResults Int


newtype PatchClassification = PatchClassification String


-- | <p>Information about the state of a patch on a particular instance as it relates to the patch baseline used to patch the instance.</p>
newtype PatchComplianceData = PatchComplianceData 
  { "Title" :: (PatchTitle)
  , "KBId" :: (PatchKbNumber)
  , "Classification" :: (PatchClassification)
  , "Severity" :: (PatchSeverity)
  , "State" :: (PatchComplianceDataState)
  , "InstalledTime" :: (DateTime)
  }


newtype PatchComplianceDataList = PatchComplianceDataList (Array PatchComplianceData)


newtype PatchComplianceDataState = PatchComplianceDataState String


newtype PatchComplianceLevel = PatchComplianceLevel String


newtype PatchComplianceMaxResults = PatchComplianceMaxResults Int


newtype PatchContentUrl = PatchContentUrl String


newtype PatchDeploymentStatus = PatchDeploymentStatus String


newtype PatchDescription = PatchDescription String


newtype PatchFailedCount = PatchFailedCount Int


-- | <p>Defines a patch filter.</p> <p>A patch filter consists of key/value pairs, but not all keys are valid for all operating system types. For example, the key <code>PRODUCT</code> is valid for all supported operating system types. The key <code>MSRC_SEVERITY</code>, however, is valid only for Windows operating systems, and the key <code>SECTION</code> is valid only for Ubuntu operating systems.</p> <p>Refer to the following sections for information about which keys may be used with each major operating system, and which values are valid for each key.</p> <p> <b>Windows Operating Systems</b> </p> <p>The supported keys for Windows operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>MSRC_SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Windows7</code> </p> </li> <li> <p> <code>Windows8</code> </p> </li> <li> <p> <code>Windows8.1</code> </p> </li> <li> <p> <code>Windows8Embedded</code> </p> </li> <li> <p> <code>Windows10</code> </p> </li> <li> <p> <code>Windows10LTSB</code> </p> </li> <li> <p> <code>WindowsServer2008</code> </p> </li> <li> <p> <code>WindowsServer2008R2</code> </p> </li> <li> <p> <code>WindowsServer2012</code> </p> </li> <li> <p> <code>WindowsServer2012R2</code> </p> </li> <li> <p> <code>WindowsServer2016</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>CriticalUpdates</code> </p> </li> <li> <p> <code>DefinitionUpdates</code> </p> </li> <li> <p> <code>Drivers</code> </p> </li> <li> <p> <code>FeaturePacks</code> </p> </li> <li> <p> <code>SecurityUpdates</code> </p> </li> <li> <p> <code>ServicePacks</code> </p> </li> <li> <p> <code>Tools</code> </p> </li> <li> <p> <code>UpdateRollups</code> </p> </li> <li> <p> <code>Updates</code> </p> </li> <li> <p> <code>Upgrades</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>MSRC_SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> <li> <p> <code>Unspecified</code> </p> </li> </ul> <p> <b>Ubuntu Operating Systems</b> </p> <p>The supported keys for Ubuntu operating systems are <code>PRODUCT</code>, <code>PRIORITY</code>, and <code>SECTION</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Ubuntu14.04</code> </p> </li> <li> <p> <code>Ubuntu16.04</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>PRIORITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Required</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Standard</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Extra</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SECTION</code> </p> <p>Only the length of the key value is validated. Minimum length is 1. Maximum length is 64.</p> <p> <b>Amazon Linux Operating Systems</b> </p> <p>The supported keys for Amazon Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>AmazonLinux2012.03</code> </p> </li> <li> <p> <code>AmazonLinux2012.09</code> </p> </li> <li> <p> <code>AmazonLinux2013.03</code> </p> </li> <li> <p> <code>AmazonLinux2013.09</code> </p> </li> <li> <p> <code>AmazonLinux2014.03</code> </p> </li> <li> <p> <code>AmazonLinux2014.09</code> </p> </li> <li> <p> <code>AmazonLinux2015.03</code> </p> </li> <li> <p> <code>AmazonLinux2015.09</code> </p> </li> <li> <p> <code>AmazonLinux2016.03</code> </p> </li> <li> <p> <code>AmazonLinux2016.09</code> </p> </li> <li> <p> <code>AmazonLinux2017.03</code> </p> </li> <li> <p> <code>AmazonLinux2017.09</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>RedHat Enterprise Linux (RHEL) Operating Systems</b> </p> <p>The supported keys for RedHat Enterprise Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>RedhatEnterpriseLinux6.5</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.6</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.7</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.8</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.9</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.0</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.1</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.2</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.3</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.4</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>SUSE Linux Enterprise Server (SUSE) Operating Systems</b> </p> <p>The supported keys for SUSE operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Suse12.0</code> </p> </li> <li> <p> <code>Suse12.1</code> </p> </li> <li> <p> <code>Suse12.2</code> </p> </li> <li> <p> <code>Suse12.3</code> </p> </li> <li> <p> <code>Suse12.4</code> </p> </li> <li> <p> <code>Suse12.5</code> </p> </li> <li> <p> <code>Suse12.6</code> </p> </li> <li> <p> <code>Suse12.7</code> </p> </li> <li> <p> <code>Suse12.8</code> </p> </li> <li> <p> <code>Suse12.9</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Feature</code> </p> </li> <li> <p> <code>Document</code> </p> </li> <li> <p> <code>Yast</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul>
newtype PatchFilter = PatchFilter 
  { "Key" :: (PatchFilterKey)
  , "Values" :: (PatchFilterValueList)
  }


-- | <p>A set of patch filters, typically used for approval rules.</p>
newtype PatchFilterGroup = PatchFilterGroup 
  { "PatchFilters" :: (PatchFilterList)
  }


newtype PatchFilterKey = PatchFilterKey String


newtype PatchFilterList = PatchFilterList (Array PatchFilter)


newtype PatchFilterValue = PatchFilterValue String


newtype PatchFilterValueList = PatchFilterValueList (Array PatchFilterValue)


newtype PatchGroup = PatchGroup String


newtype PatchGroupList = PatchGroupList (Array PatchGroup)


-- | <p>The mapping between a patch group and the patch baseline the patch group is registered with.</p>
newtype PatchGroupPatchBaselineMapping = PatchGroupPatchBaselineMapping 
  { "PatchGroup" :: NullOrUndefined (PatchGroup)
  , "BaselineIdentity" :: NullOrUndefined (PatchBaselineIdentity)
  }


newtype PatchGroupPatchBaselineMappingList = PatchGroupPatchBaselineMappingList (Array PatchGroupPatchBaselineMapping)


newtype PatchId = PatchId String


newtype PatchIdList = PatchIdList (Array PatchId)


newtype PatchInstalledCount = PatchInstalledCount Int


newtype PatchInstalledOtherCount = PatchInstalledOtherCount Int


newtype PatchKbNumber = PatchKbNumber String


newtype PatchLanguage = PatchLanguage String


newtype PatchList = PatchList (Array Patch)


newtype PatchMissingCount = PatchMissingCount Int


newtype PatchMsrcNumber = PatchMsrcNumber String


newtype PatchMsrcSeverity = PatchMsrcSeverity String


newtype PatchNotApplicableCount = PatchNotApplicableCount Int


newtype PatchOperationType = PatchOperationType String


-- | <p>Defines a filter used in Patch Manager APIs.</p>
newtype PatchOrchestratorFilter = PatchOrchestratorFilter 
  { "Key" :: NullOrUndefined (PatchOrchestratorFilterKey)
  , "Values" :: NullOrUndefined (PatchOrchestratorFilterValues)
  }


newtype PatchOrchestratorFilterKey = PatchOrchestratorFilterKey String


newtype PatchOrchestratorFilterList = PatchOrchestratorFilterList (Array PatchOrchestratorFilter)


newtype PatchOrchestratorFilterValue = PatchOrchestratorFilterValue String


newtype PatchOrchestratorFilterValues = PatchOrchestratorFilterValues (Array PatchOrchestratorFilterValue)


newtype PatchProduct = PatchProduct String


newtype PatchProductFamily = PatchProductFamily String


-- | <p>Defines an approval rule for a patch baseline.</p>
newtype PatchRule = PatchRule 
  { "PatchFilterGroup" :: (PatchFilterGroup)
  , "ComplianceLevel" :: NullOrUndefined (PatchComplianceLevel)
  , "ApproveAfterDays" :: (ApproveAfterDays)
  , "EnableNonSecurity" :: NullOrUndefined (Boolean)
  }


-- | <p>A set of rules defining the approval rules for a patch baseline.</p>
newtype PatchRuleGroup = PatchRuleGroup 
  { "PatchRules" :: (PatchRuleList)
  }


newtype PatchRuleList = PatchRuleList (Array PatchRule)


newtype PatchSeverity = PatchSeverity String


-- | <p>Information about the patches to use to update the instances, including target operating systems and source repository. Applies to Linux instances only.</p>
newtype PatchSource = PatchSource 
  { "Name" :: (PatchSourceName)
  , "Products" :: (PatchSourceProductList)
  , "Configuration" :: (PatchSourceConfiguration)
  }


newtype PatchSourceConfiguration = PatchSourceConfiguration String


newtype PatchSourceList = PatchSourceList (Array PatchSource)


newtype PatchSourceName = PatchSourceName String


newtype PatchSourceProduct = PatchSourceProduct String


newtype PatchSourceProductList = PatchSourceProductList (Array PatchSourceProduct)


-- | <p>Information about the approval status of a patch.</p>
newtype PatchStatus = PatchStatus 
  { "DeploymentStatus" :: NullOrUndefined (PatchDeploymentStatus)
  , "ComplianceLevel" :: NullOrUndefined (PatchComplianceLevel)
  , "ApprovalDate" :: NullOrUndefined (DateTime)
  }


newtype PatchTitle = PatchTitle String


newtype PatchVendor = PatchVendor String


newtype PingStatus = PingStatus String


newtype PlatformType = PlatformType String


newtype PlatformTypeList = PlatformTypeList (Array PlatformType)


newtype Product = Product String


newtype PutComplianceItemsRequest = PutComplianceItemsRequest 
  { "ResourceId" :: (ComplianceResourceId)
  , "ResourceType" :: (ComplianceResourceType)
  , "ComplianceType" :: (ComplianceTypeName)
  , "ExecutionSummary" :: (ComplianceExecutionSummary)
  , "Items" :: (ComplianceItemEntryList)
  , "ItemContentHash" :: NullOrUndefined (ComplianceItemContentHash)
  }


newtype PutComplianceItemsResult = PutComplianceItemsResult 
  { 
  }


newtype PutInventoryRequest = PutInventoryRequest 
  { "InstanceId" :: (InstanceId)
  , "Items" :: (InventoryItemList)
  }


newtype PutInventoryResult = PutInventoryResult 
  { 
  }


newtype PutParameterRequest = PutParameterRequest 
  { "Name" :: (PSParameterName)
  , "Description" :: NullOrUndefined (ParameterDescription)
  , "Value" :: (PSParameterValue)
  , "Type" :: (ParameterType)
  , "KeyId" :: NullOrUndefined (ParameterKeyId)
  , "Overwrite" :: NullOrUndefined (Boolean)
  , "AllowedPattern" :: NullOrUndefined (AllowedPattern)
  }


newtype PutParameterResult = PutParameterResult 
  { "Version" :: NullOrUndefined (PSParameterVersion)
  }


newtype RegisterDefaultPatchBaselineRequest = RegisterDefaultPatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  }


newtype RegisterDefaultPatchBaselineResult = RegisterDefaultPatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  }


newtype RegisterPatchBaselineForPatchGroupRequest = RegisterPatchBaselineForPatchGroupRequest 
  { "BaselineId" :: (BaselineId)
  , "PatchGroup" :: (PatchGroup)
  }


newtype RegisterPatchBaselineForPatchGroupResult = RegisterPatchBaselineForPatchGroupResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "PatchGroup" :: NullOrUndefined (PatchGroup)
  }


newtype RegisterTargetWithMaintenanceWindowRequest = RegisterTargetWithMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "ResourceType" :: (MaintenanceWindowResourceType)
  , "Targets" :: (Targets)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "ClientToken" :: NullOrUndefined (ClientToken)
  }


newtype RegisterTargetWithMaintenanceWindowResult = RegisterTargetWithMaintenanceWindowResult 
  { "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId)
  }


newtype RegisterTaskWithMaintenanceWindowRequest = RegisterTaskWithMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Targets" :: (Targets)
  , "TaskArn" :: (MaintenanceWindowTaskArn)
  , "ServiceRoleArn" :: (ServiceRole)
  , "TaskType" :: (MaintenanceWindowTaskType)
  , "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters)
  , "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters)
  , "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: (MaxConcurrency)
  , "MaxErrors" :: (MaxErrors)
  , "LoggingInfo" :: NullOrUndefined (LoggingInfo)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "ClientToken" :: NullOrUndefined (ClientToken)
  }


newtype RegisterTaskWithMaintenanceWindowResult = RegisterTaskWithMaintenanceWindowResult 
  { "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId)
  }


newtype RegistrationLimit = RegistrationLimit Int


newtype RegistrationsCount = RegistrationsCount Int


newtype RemoveTagsFromResourceRequest = RemoveTagsFromResourceRequest 
  { "ResourceType" :: (ResourceTypeForTagging)
  , "ResourceId" :: (ResourceId)
  , "TagKeys" :: (KeyList)
  }


newtype RemoveTagsFromResourceResult = RemoveTagsFromResourceResult 
  { 
  }


-- | <p>Information about targets that resolved during the Automation execution.</p>
newtype ResolvedTargets = ResolvedTargets 
  { "ParameterValues" :: NullOrUndefined (TargetParameterList)
  , "Truncated" :: NullOrUndefined (Boolean)
  }


-- | <p>Compliance summary information for a specific resource. </p>
newtype ResourceComplianceSummaryItem = ResourceComplianceSummaryItem 
  { "ComplianceType" :: NullOrUndefined (ComplianceTypeName)
  , "ResourceType" :: NullOrUndefined (ComplianceResourceType)
  , "ResourceId" :: NullOrUndefined (ComplianceResourceId)
  , "Status" :: NullOrUndefined (ComplianceStatus)
  , "OverallSeverity" :: NullOrUndefined (ComplianceSeverity)
  , "ExecutionSummary" :: NullOrUndefined (ComplianceExecutionSummary)
  , "CompliantSummary" :: NullOrUndefined (CompliantSummary)
  , "NonCompliantSummary" :: NullOrUndefined (NonCompliantSummary)
  }


newtype ResourceComplianceSummaryItemList = ResourceComplianceSummaryItemList (Array ResourceComplianceSummaryItem)


newtype ResourceDataSyncAWSKMSKeyARN = ResourceDataSyncAWSKMSKeyARN String


-- | <p>A sync configuration with the same name already exists.</p>
newtype ResourceDataSyncAlreadyExistsException = ResourceDataSyncAlreadyExistsException 
  { "SyncName" :: NullOrUndefined (ResourceDataSyncName)
  }


-- | <p>You have exceeded the allowed maximum sync configurations.</p>
newtype ResourceDataSyncCountExceededException = ResourceDataSyncCountExceededException 
  { "Message" :: NullOrUndefined (String)
  }


newtype ResourceDataSyncCreatedTime = ResourceDataSyncCreatedTime Number


-- | <p>The specified sync configuration is invalid.</p>
newtype ResourceDataSyncInvalidConfigurationException = ResourceDataSyncInvalidConfigurationException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Information about a Resource Data Sync configuration, including its current status and last successful sync.</p>
newtype ResourceDataSyncItem = ResourceDataSyncItem 
  { "SyncName" :: NullOrUndefined (ResourceDataSyncName)
  , "S3Destination" :: NullOrUndefined (ResourceDataSyncS3Destination)
  , "LastSyncTime" :: NullOrUndefined (LastResourceDataSyncTime)
  , "LastSuccessfulSyncTime" :: NullOrUndefined (LastSuccessfulResourceDataSyncTime)
  , "LastStatus" :: NullOrUndefined (LastResourceDataSyncStatus)
  , "SyncCreatedTime" :: NullOrUndefined (ResourceDataSyncCreatedTime)
  }


newtype ResourceDataSyncItemList = ResourceDataSyncItemList (Array ResourceDataSyncItem)


newtype ResourceDataSyncName = ResourceDataSyncName String


-- | <p>The specified sync name was not found.</p>
newtype ResourceDataSyncNotFoundException = ResourceDataSyncNotFoundException 
  { "SyncName" :: NullOrUndefined (ResourceDataSyncName)
  }


newtype ResourceDataSyncS3BucketName = ResourceDataSyncS3BucketName String


-- | <p>Information about the target Amazon S3 bucket for the Resource Data Sync.</p>
newtype ResourceDataSyncS3Destination = ResourceDataSyncS3Destination 
  { "BucketName" :: (ResourceDataSyncS3BucketName)
  , "Prefix" :: NullOrUndefined (ResourceDataSyncS3Prefix)
  , "SyncFormat" :: (ResourceDataSyncS3Format)
  , "Region" :: (ResourceDataSyncS3Region)
  , "AWSKMSKeyARN" :: NullOrUndefined (ResourceDataSyncAWSKMSKeyARN)
  }


newtype ResourceDataSyncS3Format = ResourceDataSyncS3Format String


newtype ResourceDataSyncS3Prefix = ResourceDataSyncS3Prefix String


newtype ResourceDataSyncS3Region = ResourceDataSyncS3Region String


newtype ResourceId = ResourceId String


-- | <p>Error returned if an attempt is made to delete a patch baseline that is registered for a patch group.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Error returned when the caller has exceeded the default resource limits. For example, too many Maintenance Windows or Patch baselines have been created.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>
newtype ResourceLimitExceededException = ResourceLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


newtype ResourceType = ResourceType String


newtype ResourceTypeForTagging = ResourceTypeForTagging String


newtype ResponseCode = ResponseCode Int


-- | <p>The inventory item result attribute.</p>
newtype ResultAttribute = ResultAttribute 
  { "TypeName" :: (InventoryItemTypeName)
  }


newtype ResultAttributeList = ResultAttributeList (Array ResultAttribute)


newtype S3BucketName = S3BucketName String


newtype S3KeyPrefix = S3KeyPrefix String


-- | <p>An Amazon S3 bucket where you want to store the results of this request.</p>
newtype S3OutputLocation = S3OutputLocation 
  { "OutputS3Region" :: NullOrUndefined (S3Region)
  , "OutputS3BucketName" :: NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix)
  }


-- | <p>A URL for the Amazon S3 bucket where you want to store the results of this request.</p>
newtype S3OutputUrl = S3OutputUrl 
  { "OutputUrl" :: NullOrUndefined (Url)
  }


newtype S3Region = S3Region String


newtype ScheduleExpression = ScheduleExpression String


newtype SendAutomationSignalRequest = SendAutomationSignalRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  , "SignalType" :: (SignalType)
  , "Payload" :: NullOrUndefined (AutomationParameterMap)
  }


newtype SendAutomationSignalResult = SendAutomationSignalResult 
  { 
  }


newtype SendCommandRequest = SendCommandRequest 
  { "InstanceIds" :: NullOrUndefined (InstanceIdList)
  , "Targets" :: NullOrUndefined (Targets)
  , "DocumentName" :: (DocumentARN)
  , "DocumentHash" :: NullOrUndefined (DocumentHash)
  , "DocumentHashType" :: NullOrUndefined (DocumentHashType)
  , "TimeoutSeconds" :: NullOrUndefined (TimeoutSeconds)
  , "Comment" :: NullOrUndefined (Comment)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "OutputS3Region" :: NullOrUndefined (S3Region)
  , "OutputS3BucketName" :: NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  , "ServiceRoleArn" :: NullOrUndefined (ServiceRole)
  , "NotificationConfig" :: NullOrUndefined (NotificationConfig)
  }


newtype SendCommandResult = SendCommandResult 
  { "Command" :: NullOrUndefined (Command)
  }


newtype ServiceRole = ServiceRole String


-- | <p>The number of managed instances found for each patch severity level defined in the request filter.</p>
newtype SeveritySummary = SeveritySummary 
  { "CriticalCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "HighCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "MediumCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "LowCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "InformationalCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "UnspecifiedCount" :: NullOrUndefined (ComplianceSummaryCount)
  }


newtype SignalType = SignalType String


newtype SnapshotDownloadUrl = SnapshotDownloadUrl String


newtype SnapshotId = SnapshotId String


newtype StandardErrorContent = StandardErrorContent String


newtype StandardOutputContent = StandardOutputContent String


newtype StartAutomationExecutionRequest = StartAutomationExecutionRequest 
  { "DocumentName" :: (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Parameters" :: NullOrUndefined (AutomationParameterMap)
  , "ClientToken" :: NullOrUndefined (IdempotencyToken)
  , "Mode" :: NullOrUndefined (ExecutionMode)
  , "TargetParameterName" :: NullOrUndefined (AutomationParameterKey)
  , "Targets" :: NullOrUndefined (Targets)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  }


newtype StartAutomationExecutionResult = StartAutomationExecutionResult 
  { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId)
  }


newtype StatusAdditionalInfo = StatusAdditionalInfo String


newtype StatusDetails = StatusDetails String


newtype StatusMessage = StatusMessage String


newtype StatusName = StatusName String


-- | <p>The updated status is the same as the current status.</p>
newtype StatusUnchanged = StatusUnchanged 
  { 
  }


-- | <p>Detailed information about an the execution state of an Automation step.</p>
newtype StepExecution = StepExecution 
  { "StepName" :: NullOrUndefined (String)
  , "Action" :: NullOrUndefined (AutomationActionName)
  , "TimeoutSeconds" :: NullOrUndefined (Number)
  , "OnFailure" :: NullOrUndefined (String)
  , "MaxAttempts" :: NullOrUndefined (Int)
  , "ExecutionStartTime" :: NullOrUndefined (DateTime)
  , "ExecutionEndTime" :: NullOrUndefined (DateTime)
  , "StepStatus" :: NullOrUndefined (AutomationExecutionStatus)
  , "ResponseCode" :: NullOrUndefined (String)
  , "Inputs" :: NullOrUndefined (NormalStringMap)
  , "Outputs" :: NullOrUndefined (AutomationParameterMap)
  , "Response" :: NullOrUndefined (String)
  , "FailureMessage" :: NullOrUndefined (String)
  , "FailureDetails" :: NullOrUndefined (FailureDetails)
  , "StepExecutionId" :: NullOrUndefined (String)
  , "OverriddenParameters" :: NullOrUndefined (AutomationParameterMap)
  }


-- | <p>A filter to limit the amount of step execution information returned by the call.</p>
newtype StepExecutionFilter = StepExecutionFilter 
  { "Key" :: (StepExecutionFilterKey)
  , "Values" :: (StepExecutionFilterValueList)
  }


newtype StepExecutionFilterKey = StepExecutionFilterKey String


newtype StepExecutionFilterList = StepExecutionFilterList (Array StepExecutionFilter)


newtype StepExecutionFilterValue = StepExecutionFilterValue String


newtype StepExecutionFilterValueList = StepExecutionFilterValueList (Array StepExecutionFilterValue)


newtype StepExecutionList = StepExecutionList (Array StepExecution)


newtype StopAutomationExecutionRequest = StopAutomationExecutionRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  , "Type" :: NullOrUndefined (StopType)
  }


newtype StopAutomationExecutionResult = StopAutomationExecutionResult 
  { 
  }


newtype StopType = StopType String


newtype StringDateTime = StringDateTime String


newtype StringList = StringList (Array String)


-- | <p>The sub-type count exceeded the limit for the inventory type.</p>
newtype SubTypeCountLimitExceededException = SubTypeCountLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Metadata that you assign to your AWS resources. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. In Systems Manager, you can apply tags to documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }


newtype TagKey = TagKey String


newtype TagList = TagList (Array Tag)


newtype TagValue = TagValue String


-- | <p>An array of search criteria that targets instances using a Key,Value combination that you specify. <code>Targets</code> is required if you don't provide one or more instance IDs in the call.</p> <p/>
newtype Target = Target 
  { "Key" :: NullOrUndefined (TargetKey)
  , "Values" :: NullOrUndefined (TargetValues)
  }


newtype TargetCount = TargetCount Int


-- | <p>You specified the <code>Safe</code> option for the DeregisterTargetFromMaintenanceWindow operation, but the target is still referenced in a task.</p>
newtype TargetInUseException = TargetInUseException 
  { "Message" :: NullOrUndefined (String)
  }


newtype TargetKey = TargetKey String


newtype TargetParameterList = TargetParameterList (Array ParameterValue)


newtype TargetType = TargetType String


newtype TargetValue = TargetValue String


newtype TargetValues = TargetValues (Array TargetValue)


newtype Targets = Targets (Array Target)


newtype TimeoutSeconds = TimeoutSeconds Int


-- | <p>The Targets parameter includes too many tags. Remove one or more tags and try the command again.</p>
newtype TooManyTagsError = TooManyTagsError 
  { 
  }


-- | <p>There are concurrent updates for a resource that supports one update at a time.</p>
newtype TooManyUpdates = TooManyUpdates 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The size of inventory data has exceeded the total size limit for the resource.</p>
newtype TotalSizeLimitExceededException = TotalSizeLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The <code>Context</code> attribute that you specified for the <code>InventoryItem</code> is not allowed for this inventory type. You can only use the <code>Context</code> attribute with inventory types like <code>AWS:ComplianceItem</code>.</p>
newtype UnsupportedInventoryItemContextException = UnsupportedInventoryItemContextException 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined (String)
  }


-- | <p>Inventory item type schema version has to match supported versions in the service. Check output of GetInventorySchema to see the available schema version for each type.</p>
newtype UnsupportedInventorySchemaVersionException = UnsupportedInventorySchemaVersionException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The operating systems you specified is not supported, or the operation is not supported for the operating system. Valid operating systems include: Windows, AmazonLinux, RedhatEnterpriseLinux, and Ubuntu.</p>
newtype UnsupportedOperatingSystem = UnsupportedOperatingSystem 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The parameter type is not supported.</p>
newtype UnsupportedParameterType = UnsupportedParameterType 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>The document does not support the platform type of the given instance ID(s). For example, you sent an document for a Windows instance to a Linux instance.</p>
newtype UnsupportedPlatformType = UnsupportedPlatformType 
  { "Message" :: NullOrUndefined (String)
  }


newtype UpdateAssociationRequest = UpdateAssociationRequest 
  { "AssociationId" :: (AssociationId)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined (InstanceAssociationOutputLocation)
  , "Name" :: NullOrUndefined (DocumentName)
  , "Targets" :: NullOrUndefined (Targets)
  , "AssociationName" :: NullOrUndefined (AssociationName)
  , "AssociationVersion" :: NullOrUndefined (AssociationVersion)
  }


newtype UpdateAssociationResult = UpdateAssociationResult 
  { "AssociationDescription" :: NullOrUndefined (AssociationDescription)
  }


newtype UpdateAssociationStatusRequest = UpdateAssociationStatusRequest 
  { "Name" :: (DocumentName)
  , "InstanceId" :: (InstanceId)
  , "AssociationStatus" :: (AssociationStatus)
  }


newtype UpdateAssociationStatusResult = UpdateAssociationStatusResult 
  { "AssociationDescription" :: NullOrUndefined (AssociationDescription)
  }


newtype UpdateDocumentDefaultVersionRequest = UpdateDocumentDefaultVersionRequest 
  { "Name" :: (DocumentName)
  , "DocumentVersion" :: (DocumentVersionNumber)
  }


newtype UpdateDocumentDefaultVersionResult = UpdateDocumentDefaultVersionResult 
  { "Description" :: NullOrUndefined (DocumentDefaultVersionDescription)
  }


newtype UpdateDocumentRequest = UpdateDocumentRequest 
  { "Content" :: (DocumentContent)
  , "Name" :: (DocumentName)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined (TargetType)
  }


newtype UpdateDocumentResult = UpdateDocumentResult 
  { "DocumentDescription" :: NullOrUndefined (DocumentDescription)
  }


newtype UpdateMaintenanceWindowRequest = UpdateMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Schedule" :: NullOrUndefined (MaintenanceWindowSchedule)
  , "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours)
  , "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff)
  , "AllowUnassociatedTargets" :: NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets)
  , "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled)
  , "Replace" :: NullOrUndefined (Boolean)
  }


newtype UpdateMaintenanceWindowResult = UpdateMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Schedule" :: NullOrUndefined (MaintenanceWindowSchedule)
  , "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours)
  , "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff)
  , "AllowUnassociatedTargets" :: NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets)
  , "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled)
  }


newtype UpdateMaintenanceWindowTargetRequest = UpdateMaintenanceWindowTargetRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTargetId" :: (MaintenanceWindowTargetId)
  , "Targets" :: NullOrUndefined (Targets)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Replace" :: NullOrUndefined (Boolean)
  }


newtype UpdateMaintenanceWindowTargetResult = UpdateMaintenanceWindowTargetResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId)
  , "Targets" :: NullOrUndefined (Targets)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  }


newtype UpdateMaintenanceWindowTaskRequest = UpdateMaintenanceWindowTaskRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTaskId" :: (MaintenanceWindowTaskId)
  , "Targets" :: NullOrUndefined (Targets)
  , "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn)
  , "ServiceRoleArn" :: NullOrUndefined (ServiceRole)
  , "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters)
  , "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters)
  , "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  , "LoggingInfo" :: NullOrUndefined (LoggingInfo)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Replace" :: NullOrUndefined (Boolean)
  }


newtype UpdateMaintenanceWindowTaskResult = UpdateMaintenanceWindowTaskResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId)
  , "Targets" :: NullOrUndefined (Targets)
  , "TaskArn" :: NullOrUndefined (MaintenanceWindowTaskArn)
  , "ServiceRoleArn" :: NullOrUndefined (ServiceRole)
  , "TaskParameters" :: NullOrUndefined (MaintenanceWindowTaskParameters)
  , "TaskInvocationParameters" :: NullOrUndefined (MaintenanceWindowTaskInvocationParameters)
  , "Priority" :: NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined (MaxErrors)
  , "LoggingInfo" :: NullOrUndefined (LoggingInfo)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  }


newtype UpdateManagedInstanceRoleRequest = UpdateManagedInstanceRoleRequest 
  { "InstanceId" :: (ManagedInstanceId)
  , "IamRole" :: (IamRole)
  }


newtype UpdateManagedInstanceRoleResult = UpdateManagedInstanceRoleResult 
  { 
  }


newtype UpdatePatchBaselineRequest = UpdatePatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  , "Name" :: NullOrUndefined (BaselineName)
  , "GlobalFilters" :: NullOrUndefined (PatchFilterGroup)
  , "ApprovalRules" :: NullOrUndefined (PatchRuleGroup)
  , "ApprovedPatches" :: NullOrUndefined (PatchIdList)
  , "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel)
  , "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean)
  , "RejectedPatches" :: NullOrUndefined (PatchIdList)
  , "Description" :: NullOrUndefined (BaselineDescription)
  , "Sources" :: NullOrUndefined (PatchSourceList)
  , "Replace" :: NullOrUndefined (Boolean)
  }


newtype UpdatePatchBaselineResult = UpdatePatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "Name" :: NullOrUndefined (BaselineName)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  , "GlobalFilters" :: NullOrUndefined (PatchFilterGroup)
  , "ApprovalRules" :: NullOrUndefined (PatchRuleGroup)
  , "ApprovedPatches" :: NullOrUndefined (PatchIdList)
  , "ApprovedPatchesComplianceLevel" :: NullOrUndefined (PatchComplianceLevel)
  , "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined (Boolean)
  , "RejectedPatches" :: NullOrUndefined (PatchIdList)
  , "CreatedDate" :: NullOrUndefined (DateTime)
  , "ModifiedDate" :: NullOrUndefined (DateTime)
  , "Description" :: NullOrUndefined (BaselineDescription)
  , "Sources" :: NullOrUndefined (PatchSourceList)
  }


newtype Url = Url String


newtype Version = Version String
