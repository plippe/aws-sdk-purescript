

-- | <fullname>AWS Systems Manager</fullname> <p>AWS Systems Manager is a collection of capabilities that helps you automate management tasks such as collecting system inventory, applying operating system (OS) patches, automating the creation of Amazon Machine Images (AMIs), and configuring operating systems (OSs) and applications at scale. Systems Manager lets you remotely and securely manage the configuration of your managed instances. A <i>managed instance</i> is any Amazon EC2 instance or on-premises machine in your hybrid environment that has been configured for Systems Manager.</p> <p>This reference is intended to be used with the <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/">AWS Systems Manager User Guide</a>.</p> <p>To get started, verify prerequisites and configure managed instances. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-setting-up.html">Systems Manager Prerequisites</a>.</p> <p>For information about other API actions you can perform on Amazon EC2 instances, see the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/">Amazon EC2 API Reference</a>. For information about how to use a Query API, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/making-api-requests.html">Making API Requests</a>. </p>
module AWS.SSM where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeAccountId :: Newtype AccountId _


newtype AccountIdList = AccountIdList (Array AccountId)
derive instance newtypeAccountIdList :: Newtype AccountIdList _


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
derive instance newtypeActivation :: Newtype Activation _


newtype ActivationCode = ActivationCode String
derive instance newtypeActivationCode :: Newtype ActivationCode _


newtype ActivationDescription = ActivationDescription String
derive instance newtypeActivationDescription :: Newtype ActivationDescription _


newtype ActivationId = ActivationId String
derive instance newtypeActivationId :: Newtype ActivationId _


newtype ActivationList = ActivationList (Array Activation)
derive instance newtypeActivationList :: Newtype ActivationList _


newtype AddTagsToResourceRequest = AddTagsToResourceRequest 
  { "ResourceType" :: (ResourceTypeForTagging)
  , "ResourceId" :: (ResourceId)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsToResourceRequest :: Newtype AddTagsToResourceRequest _


newtype AddTagsToResourceResult = AddTagsToResourceResult 
  { 
  }
derive instance newtypeAddTagsToResourceResult :: Newtype AddTagsToResourceResult _


newtype AgentErrorCode = AgentErrorCode String
derive instance newtypeAgentErrorCode :: Newtype AgentErrorCode _


newtype AggregatorSchemaOnly = AggregatorSchemaOnly Boolean
derive instance newtypeAggregatorSchemaOnly :: Newtype AggregatorSchemaOnly _


newtype AllowedPattern = AllowedPattern String
derive instance newtypeAllowedPattern :: Newtype AllowedPattern _


-- | <p>Error returned if an attempt is made to register a patch group with a patch baseline that is already registered with a different patch baseline.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAlreadyExistsException :: Newtype AlreadyExistsException _


newtype ApproveAfterDays = ApproveAfterDays Int
derive instance newtypeApproveAfterDays :: Newtype ApproveAfterDays _


-- | <p>You must disassociate a document from all instances before you can delete it.</p>
newtype AssociatedInstances = AssociatedInstances 
  { 
  }
derive instance newtypeAssociatedInstances :: Newtype AssociatedInstances _


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
derive instance newtypeAssociation :: Newtype Association _


-- | <p>The specified association already exists.</p>
newtype AssociationAlreadyExists = AssociationAlreadyExists 
  { 
  }
derive instance newtypeAssociationAlreadyExists :: Newtype AssociationAlreadyExists _


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
derive instance newtypeAssociationDescription :: Newtype AssociationDescription _


newtype AssociationDescriptionList = AssociationDescriptionList (Array AssociationDescription)
derive instance newtypeAssociationDescriptionList :: Newtype AssociationDescriptionList _


-- | <p>The specified association does not exist.</p>
newtype AssociationDoesNotExist = AssociationDoesNotExist 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAssociationDoesNotExist :: Newtype AssociationDoesNotExist _


-- | <p>Describes a filter.</p>
newtype AssociationFilter = AssociationFilter 
  { "Key'" :: (AssociationFilterKey)
  , "Value'" :: (AssociationFilterValue)
  }
derive instance newtypeAssociationFilter :: Newtype AssociationFilter _


newtype AssociationFilterKey = AssociationFilterKey String
derive instance newtypeAssociationFilterKey :: Newtype AssociationFilterKey _


newtype AssociationFilterList = AssociationFilterList (Array AssociationFilter)
derive instance newtypeAssociationFilterList :: Newtype AssociationFilterList _


newtype AssociationFilterValue = AssociationFilterValue String
derive instance newtypeAssociationFilterValue :: Newtype AssociationFilterValue _


newtype AssociationId = AssociationId String
derive instance newtypeAssociationId :: Newtype AssociationId _


-- | <p>You can have at most 2,000 active associations.</p>
newtype AssociationLimitExceeded = AssociationLimitExceeded 
  { 
  }
derive instance newtypeAssociationLimitExceeded :: Newtype AssociationLimitExceeded _


newtype AssociationList = AssociationList (Array Association)
derive instance newtypeAssociationList :: Newtype AssociationList _


newtype AssociationName = AssociationName String
derive instance newtypeAssociationName :: Newtype AssociationName _


-- | <p>Information about the association.</p>
newtype AssociationOverview = AssociationOverview 
  { "Status" :: NullOrUndefined (StatusName)
  , "DetailedStatus" :: NullOrUndefined (StatusName)
  , "AssociationStatusAggregatedCount" :: NullOrUndefined (AssociationStatusAggregatedCount)
  }
derive instance newtypeAssociationOverview :: Newtype AssociationOverview _


-- | <p>Describes an association status.</p>
newtype AssociationStatus = AssociationStatus 
  { "Date" :: (DateTime)
  , "Name" :: (AssociationStatusName)
  , "Message" :: (StatusMessage)
  , "AdditionalInfo" :: NullOrUndefined (StatusAdditionalInfo)
  }
derive instance newtypeAssociationStatus :: Newtype AssociationStatus _


newtype AssociationStatusAggregatedCount = AssociationStatusAggregatedCount (Map StatusName InstanceCount)
derive instance newtypeAssociationStatusAggregatedCount :: Newtype AssociationStatusAggregatedCount _


newtype AssociationStatusName = AssociationStatusName String
derive instance newtypeAssociationStatusName :: Newtype AssociationStatusName _


newtype AssociationVersion = AssociationVersion String
derive instance newtypeAssociationVersion :: Newtype AssociationVersion _


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
derive instance newtypeAssociationVersionInfo :: Newtype AssociationVersionInfo _


-- | <p>You have reached the maximum number versions allowed for an association. Each association has a limit of 1,000 versions. </p>
newtype AssociationVersionLimitExceeded = AssociationVersionLimitExceeded 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAssociationVersionLimitExceeded :: Newtype AssociationVersionLimitExceeded _


newtype AssociationVersionList = AssociationVersionList (Array AssociationVersionInfo)
derive instance newtypeAssociationVersionList :: Newtype AssociationVersionList _


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _


newtype AutomationActionName = AutomationActionName String
derive instance newtypeAutomationActionName :: Newtype AutomationActionName _


-- | <p>An Automation document with the specified name could not be found.</p>
newtype AutomationDefinitionNotFoundException = AutomationDefinitionNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAutomationDefinitionNotFoundException :: Newtype AutomationDefinitionNotFoundException _


-- | <p>An Automation document with the specified name and version could not be found.</p>
newtype AutomationDefinitionVersionNotFoundException = AutomationDefinitionVersionNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAutomationDefinitionVersionNotFoundException :: Newtype AutomationDefinitionVersionNotFoundException _


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
derive instance newtypeAutomationExecution :: Newtype AutomationExecution _


-- | <p>A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.</p>
newtype AutomationExecutionFilter = AutomationExecutionFilter 
  { "Key" :: (AutomationExecutionFilterKey)
  , "Values" :: (AutomationExecutionFilterValueList)
  }
derive instance newtypeAutomationExecutionFilter :: Newtype AutomationExecutionFilter _


newtype AutomationExecutionFilterKey = AutomationExecutionFilterKey String
derive instance newtypeAutomationExecutionFilterKey :: Newtype AutomationExecutionFilterKey _


newtype AutomationExecutionFilterList = AutomationExecutionFilterList (Array AutomationExecutionFilter)
derive instance newtypeAutomationExecutionFilterList :: Newtype AutomationExecutionFilterList _


newtype AutomationExecutionFilterValue = AutomationExecutionFilterValue String
derive instance newtypeAutomationExecutionFilterValue :: Newtype AutomationExecutionFilterValue _


newtype AutomationExecutionFilterValueList = AutomationExecutionFilterValueList (Array AutomationExecutionFilterValue)
derive instance newtypeAutomationExecutionFilterValueList :: Newtype AutomationExecutionFilterValueList _


newtype AutomationExecutionId = AutomationExecutionId String
derive instance newtypeAutomationExecutionId :: Newtype AutomationExecutionId _


-- | <p>The number of simultaneously running Automation executions exceeded the allowable limit.</p>
newtype AutomationExecutionLimitExceededException = AutomationExecutionLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAutomationExecutionLimitExceededException :: Newtype AutomationExecutionLimitExceededException _


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
derive instance newtypeAutomationExecutionMetadata :: Newtype AutomationExecutionMetadata _


newtype AutomationExecutionMetadataList = AutomationExecutionMetadataList (Array AutomationExecutionMetadata)
derive instance newtypeAutomationExecutionMetadataList :: Newtype AutomationExecutionMetadataList _


-- | <p>There is no automation execution information for the requested automation execution ID.</p>
newtype AutomationExecutionNotFoundException = AutomationExecutionNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAutomationExecutionNotFoundException :: Newtype AutomationExecutionNotFoundException _


newtype AutomationExecutionStatus = AutomationExecutionStatus String
derive instance newtypeAutomationExecutionStatus :: Newtype AutomationExecutionStatus _


newtype AutomationParameterKey = AutomationParameterKey String
derive instance newtypeAutomationParameterKey :: Newtype AutomationParameterKey _


newtype AutomationParameterMap = AutomationParameterMap (Map AutomationParameterKey AutomationParameterValueList)
derive instance newtypeAutomationParameterMap :: Newtype AutomationParameterMap _


newtype AutomationParameterValue = AutomationParameterValue String
derive instance newtypeAutomationParameterValue :: Newtype AutomationParameterValue _


newtype AutomationParameterValueList = AutomationParameterValueList (Array AutomationParameterValue)
derive instance newtypeAutomationParameterValueList :: Newtype AutomationParameterValueList _


-- | <p>The specified step name and execution ID don't exist. Verify the information and try again.</p>
newtype AutomationStepNotFoundException = AutomationStepNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAutomationStepNotFoundException :: Newtype AutomationStepNotFoundException _


newtype BaselineDescription = BaselineDescription String
derive instance newtypeBaselineDescription :: Newtype BaselineDescription _


newtype BaselineId = BaselineId String
derive instance newtypeBaselineId :: Newtype BaselineId _


newtype BaselineName = BaselineName String
derive instance newtypeBaselineName :: Newtype BaselineName _


newtype BatchErrorMessage = BatchErrorMessage String
derive instance newtypeBatchErrorMessage :: Newtype BatchErrorMessage _


-- | <p/>
newtype CancelCommandRequest = CancelCommandRequest 
  { "CommandId" :: (CommandId)
  , "InstanceIds" :: NullOrUndefined (InstanceIdList)
  }
derive instance newtypeCancelCommandRequest :: Newtype CancelCommandRequest _


-- | <p>Whether or not the command was successfully canceled. There is no guarantee that a request can be canceled.</p>
newtype CancelCommandResult = CancelCommandResult 
  { 
  }
derive instance newtypeCancelCommandResult :: Newtype CancelCommandResult _


newtype ClientToken = ClientToken String
derive instance newtypeClientToken :: Newtype ClientToken _


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
derive instance newtypeCommand :: Newtype Command _


-- | <p>Describes a command filter.</p>
newtype CommandFilter = CommandFilter 
  { "Key'" :: (CommandFilterKey)
  , "Value'" :: (CommandFilterValue)
  }
derive instance newtypeCommandFilter :: Newtype CommandFilter _


newtype CommandFilterKey = CommandFilterKey String
derive instance newtypeCommandFilterKey :: Newtype CommandFilterKey _


newtype CommandFilterList = CommandFilterList (Array CommandFilter)
derive instance newtypeCommandFilterList :: Newtype CommandFilterList _


newtype CommandFilterValue = CommandFilterValue String
derive instance newtypeCommandFilterValue :: Newtype CommandFilterValue _


newtype CommandId = CommandId String
derive instance newtypeCommandId :: Newtype CommandId _


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
derive instance newtypeCommandInvocation :: Newtype CommandInvocation _


newtype CommandInvocationList = CommandInvocationList (Array CommandInvocation)
derive instance newtypeCommandInvocationList :: Newtype CommandInvocationList _


newtype CommandInvocationStatus = CommandInvocationStatus String
derive instance newtypeCommandInvocationStatus :: Newtype CommandInvocationStatus _


newtype CommandList = CommandList (Array Command)
derive instance newtypeCommandList :: Newtype CommandList _


newtype CommandMaxResults = CommandMaxResults Int
derive instance newtypeCommandMaxResults :: Newtype CommandMaxResults _


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
derive instance newtypeCommandPlugin :: Newtype CommandPlugin _


newtype CommandPluginList = CommandPluginList (Array CommandPlugin)
derive instance newtypeCommandPluginList :: Newtype CommandPluginList _


newtype CommandPluginName = CommandPluginName String
derive instance newtypeCommandPluginName :: Newtype CommandPluginName _


newtype CommandPluginOutput = CommandPluginOutput String
derive instance newtypeCommandPluginOutput :: Newtype CommandPluginOutput _


newtype CommandPluginStatus = CommandPluginStatus String
derive instance newtypeCommandPluginStatus :: Newtype CommandPluginStatus _


newtype CommandStatus = CommandStatus String
derive instance newtypeCommandStatus :: Newtype CommandStatus _


newtype Comment = Comment String
derive instance newtypeComment :: Newtype Comment _


newtype CompletedCount = CompletedCount Int
derive instance newtypeCompletedCount :: Newtype CompletedCount _


newtype ComplianceExecutionId = ComplianceExecutionId String
derive instance newtypeComplianceExecutionId :: Newtype ComplianceExecutionId _


-- | <p>A summary of the call execution that includes an execution ID, the type of execution (for example, <code>Command</code>), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.</p>
newtype ComplianceExecutionSummary = ComplianceExecutionSummary 
  { "ExecutionTime" :: (DateTime)
  , "ExecutionId" :: NullOrUndefined (ComplianceExecutionId)
  , "ExecutionType" :: NullOrUndefined (ComplianceExecutionType)
  }
derive instance newtypeComplianceExecutionSummary :: Newtype ComplianceExecutionSummary _


newtype ComplianceExecutionType = ComplianceExecutionType String
derive instance newtypeComplianceExecutionType :: Newtype ComplianceExecutionType _


newtype ComplianceFilterValue = ComplianceFilterValue String
derive instance newtypeComplianceFilterValue :: Newtype ComplianceFilterValue _


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
derive instance newtypeComplianceItem :: Newtype ComplianceItem _


newtype ComplianceItemContentHash = ComplianceItemContentHash String
derive instance newtypeComplianceItemContentHash :: Newtype ComplianceItemContentHash _


newtype ComplianceItemDetails = ComplianceItemDetails (Map AttributeName AttributeValue)
derive instance newtypeComplianceItemDetails :: Newtype ComplianceItemDetails _


-- | <p>Information about a compliance item.</p>
newtype ComplianceItemEntry = ComplianceItemEntry 
  { "Id" :: NullOrUndefined (ComplianceItemId)
  , "Title" :: NullOrUndefined (ComplianceItemTitle)
  , "Severity" :: (ComplianceSeverity)
  , "Status" :: (ComplianceStatus)
  , "Details" :: NullOrUndefined (ComplianceItemDetails)
  }
derive instance newtypeComplianceItemEntry :: Newtype ComplianceItemEntry _


newtype ComplianceItemEntryList = ComplianceItemEntryList (Array ComplianceItemEntry)
derive instance newtypeComplianceItemEntryList :: Newtype ComplianceItemEntryList _


newtype ComplianceItemId = ComplianceItemId String
derive instance newtypeComplianceItemId :: Newtype ComplianceItemId _


newtype ComplianceItemList = ComplianceItemList (Array ComplianceItem)
derive instance newtypeComplianceItemList :: Newtype ComplianceItemList _


newtype ComplianceItemTitle = ComplianceItemTitle String
derive instance newtypeComplianceItemTitle :: Newtype ComplianceItemTitle _


newtype ComplianceQueryOperatorType = ComplianceQueryOperatorType String
derive instance newtypeComplianceQueryOperatorType :: Newtype ComplianceQueryOperatorType _


newtype ComplianceResourceId = ComplianceResourceId String
derive instance newtypeComplianceResourceId :: Newtype ComplianceResourceId _


newtype ComplianceResourceIdList = ComplianceResourceIdList (Array ComplianceResourceId)
derive instance newtypeComplianceResourceIdList :: Newtype ComplianceResourceIdList _


newtype ComplianceResourceType = ComplianceResourceType String
derive instance newtypeComplianceResourceType :: Newtype ComplianceResourceType _


newtype ComplianceResourceTypeList = ComplianceResourceTypeList (Array ComplianceResourceType)
derive instance newtypeComplianceResourceTypeList :: Newtype ComplianceResourceTypeList _


newtype ComplianceSeverity = ComplianceSeverity String
derive instance newtypeComplianceSeverity :: Newtype ComplianceSeverity _


newtype ComplianceStatus = ComplianceStatus String
derive instance newtypeComplianceStatus :: Newtype ComplianceStatus _


-- | <p>One or more filters. Use a filter to return a more specific list of results.</p>
newtype ComplianceStringFilter = ComplianceStringFilter 
  { "Key" :: NullOrUndefined (ComplianceStringFilterKey)
  , "Values" :: NullOrUndefined (ComplianceStringFilterValueList)
  , "Type" :: NullOrUndefined (ComplianceQueryOperatorType)
  }
derive instance newtypeComplianceStringFilter :: Newtype ComplianceStringFilter _


newtype ComplianceStringFilterKey = ComplianceStringFilterKey String
derive instance newtypeComplianceStringFilterKey :: Newtype ComplianceStringFilterKey _


newtype ComplianceStringFilterList = ComplianceStringFilterList (Array ComplianceStringFilter)
derive instance newtypeComplianceStringFilterList :: Newtype ComplianceStringFilterList _


newtype ComplianceStringFilterValueList = ComplianceStringFilterValueList (Array ComplianceFilterValue)
derive instance newtypeComplianceStringFilterValueList :: Newtype ComplianceStringFilterValueList _


newtype ComplianceSummaryCount = ComplianceSummaryCount Int
derive instance newtypeComplianceSummaryCount :: Newtype ComplianceSummaryCount _


-- | <p>A summary of compliance information by compliance type.</p>
newtype ComplianceSummaryItem = ComplianceSummaryItem 
  { "ComplianceType" :: NullOrUndefined (ComplianceTypeName)
  , "CompliantSummary" :: NullOrUndefined (CompliantSummary)
  , "NonCompliantSummary" :: NullOrUndefined (NonCompliantSummary)
  }
derive instance newtypeComplianceSummaryItem :: Newtype ComplianceSummaryItem _


newtype ComplianceSummaryItemList = ComplianceSummaryItemList (Array ComplianceSummaryItem)
derive instance newtypeComplianceSummaryItemList :: Newtype ComplianceSummaryItemList _


-- | <p>You specified too many custom compliance types. You can specify a maximum of 10 different types. </p>
newtype ComplianceTypeCountLimitExceededException = ComplianceTypeCountLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeComplianceTypeCountLimitExceededException :: Newtype ComplianceTypeCountLimitExceededException _


newtype ComplianceTypeName = ComplianceTypeName String
derive instance newtypeComplianceTypeName :: Newtype ComplianceTypeName _


-- | <p>A summary of resources that are compliant. The summary is organized according to the resource count for each compliance type.</p>
newtype CompliantSummary = CompliantSummary 
  { "CompliantCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "SeveritySummary" :: NullOrUndefined (SeveritySummary)
  }
derive instance newtypeCompliantSummary :: Newtype CompliantSummary _


newtype ComputerName = ComputerName String
derive instance newtypeComputerName :: Newtype ComputerName _


newtype CreateActivationRequest = CreateActivationRequest 
  { "Description" :: NullOrUndefined (ActivationDescription)
  , "DefaultInstanceName" :: NullOrUndefined (DefaultInstanceName)
  , "IamRole" :: (IamRole)
  , "RegistrationLimit" :: NullOrUndefined (RegistrationLimit)
  , "ExpirationDate" :: NullOrUndefined (ExpirationDate)
  }
derive instance newtypeCreateActivationRequest :: Newtype CreateActivationRequest _


newtype CreateActivationResult = CreateActivationResult 
  { "ActivationId" :: NullOrUndefined (ActivationId)
  , "ActivationCode" :: NullOrUndefined (ActivationCode)
  }
derive instance newtypeCreateActivationResult :: Newtype CreateActivationResult _


newtype CreateAssociationBatchRequest = CreateAssociationBatchRequest 
  { "Entries" :: (CreateAssociationBatchRequestEntries)
  }
derive instance newtypeCreateAssociationBatchRequest :: Newtype CreateAssociationBatchRequest _


newtype CreateAssociationBatchRequestEntries = CreateAssociationBatchRequestEntries (Array CreateAssociationBatchRequestEntry)
derive instance newtypeCreateAssociationBatchRequestEntries :: Newtype CreateAssociationBatchRequestEntries _


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
derive instance newtypeCreateAssociationBatchRequestEntry :: Newtype CreateAssociationBatchRequestEntry _


newtype CreateAssociationBatchResult = CreateAssociationBatchResult 
  { "Successful" :: NullOrUndefined (AssociationDescriptionList)
  , "Failed" :: NullOrUndefined (FailedCreateAssociationList)
  }
derive instance newtypeCreateAssociationBatchResult :: Newtype CreateAssociationBatchResult _


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
derive instance newtypeCreateAssociationRequest :: Newtype CreateAssociationRequest _


newtype CreateAssociationResult = CreateAssociationResult 
  { "AssociationDescription" :: NullOrUndefined (AssociationDescription)
  }
derive instance newtypeCreateAssociationResult :: Newtype CreateAssociationResult _


newtype CreateDocumentRequest = CreateDocumentRequest 
  { "Content" :: (DocumentContent)
  , "Name" :: (DocumentName)
  , "DocumentType" :: NullOrUndefined (DocumentType)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined (TargetType)
  }
derive instance newtypeCreateDocumentRequest :: Newtype CreateDocumentRequest _


newtype CreateDocumentResult = CreateDocumentResult 
  { "DocumentDescription" :: NullOrUndefined (DocumentDescription)
  }
derive instance newtypeCreateDocumentResult :: Newtype CreateDocumentResult _


newtype CreateMaintenanceWindowRequest = CreateMaintenanceWindowRequest 
  { "Name" :: (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Schedule" :: (MaintenanceWindowSchedule)
  , "Duration" :: (MaintenanceWindowDurationHours)
  , "Cutoff" :: (MaintenanceWindowCutoff)
  , "AllowUnassociatedTargets" :: (MaintenanceWindowAllowUnassociatedTargets)
  , "ClientToken" :: NullOrUndefined (ClientToken)
  }
derive instance newtypeCreateMaintenanceWindowRequest :: Newtype CreateMaintenanceWindowRequest _


newtype CreateMaintenanceWindowResult = CreateMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  }
derive instance newtypeCreateMaintenanceWindowResult :: Newtype CreateMaintenanceWindowResult _


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
derive instance newtypeCreatePatchBaselineRequest :: Newtype CreatePatchBaselineRequest _


newtype CreatePatchBaselineResult = CreatePatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  }
derive instance newtypeCreatePatchBaselineResult :: Newtype CreatePatchBaselineResult _


newtype CreateResourceDataSyncRequest = CreateResourceDataSyncRequest 
  { "SyncName" :: (ResourceDataSyncName)
  , "S3Destination" :: (ResourceDataSyncS3Destination)
  }
derive instance newtypeCreateResourceDataSyncRequest :: Newtype CreateResourceDataSyncRequest _


newtype CreateResourceDataSyncResult = CreateResourceDataSyncResult 
  { 
  }
derive instance newtypeCreateResourceDataSyncResult :: Newtype CreateResourceDataSyncResult _


newtype CreatedDate = CreatedDate Number
derive instance newtypeCreatedDate :: Newtype CreatedDate _


-- | <p>You have exceeded the limit for custom schemas. Delete one or more custom schemas and try again.</p>
newtype CustomSchemaCountLimitExceededException = CustomSchemaCountLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeCustomSchemaCountLimitExceededException :: Newtype CustomSchemaCountLimitExceededException _


newtype DateTime = DateTime Number
derive instance newtypeDateTime :: Newtype DateTime _


newtype DefaultBaseline = DefaultBaseline Boolean
derive instance newtypeDefaultBaseline :: Newtype DefaultBaseline _


newtype DefaultInstanceName = DefaultInstanceName String
derive instance newtypeDefaultInstanceName :: Newtype DefaultInstanceName _


newtype DeleteActivationRequest = DeleteActivationRequest 
  { "ActivationId" :: (ActivationId)
  }
derive instance newtypeDeleteActivationRequest :: Newtype DeleteActivationRequest _


newtype DeleteActivationResult = DeleteActivationResult 
  { 
  }
derive instance newtypeDeleteActivationResult :: Newtype DeleteActivationResult _


newtype DeleteAssociationRequest = DeleteAssociationRequest 
  { "Name" :: NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "AssociationId" :: NullOrUndefined (AssociationId)
  }
derive instance newtypeDeleteAssociationRequest :: Newtype DeleteAssociationRequest _


newtype DeleteAssociationResult = DeleteAssociationResult 
  { 
  }
derive instance newtypeDeleteAssociationResult :: Newtype DeleteAssociationResult _


newtype DeleteDocumentRequest = DeleteDocumentRequest 
  { "Name" :: (DocumentName)
  }
derive instance newtypeDeleteDocumentRequest :: Newtype DeleteDocumentRequest _


newtype DeleteDocumentResult = DeleteDocumentResult 
  { 
  }
derive instance newtypeDeleteDocumentResult :: Newtype DeleteDocumentResult _


newtype DeleteMaintenanceWindowRequest = DeleteMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  }
derive instance newtypeDeleteMaintenanceWindowRequest :: Newtype DeleteMaintenanceWindowRequest _


newtype DeleteMaintenanceWindowResult = DeleteMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  }
derive instance newtypeDeleteMaintenanceWindowResult :: Newtype DeleteMaintenanceWindowResult _


newtype DeleteParameterRequest = DeleteParameterRequest 
  { "Name" :: (PSParameterName)
  }
derive instance newtypeDeleteParameterRequest :: Newtype DeleteParameterRequest _


newtype DeleteParameterResult = DeleteParameterResult 
  { 
  }
derive instance newtypeDeleteParameterResult :: Newtype DeleteParameterResult _


newtype DeleteParametersRequest = DeleteParametersRequest 
  { "Names" :: (ParameterNameList)
  }
derive instance newtypeDeleteParametersRequest :: Newtype DeleteParametersRequest _


newtype DeleteParametersResult = DeleteParametersResult 
  { "DeletedParameters" :: NullOrUndefined (ParameterNameList)
  , "InvalidParameters" :: NullOrUndefined (ParameterNameList)
  }
derive instance newtypeDeleteParametersResult :: Newtype DeleteParametersResult _


newtype DeletePatchBaselineRequest = DeletePatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  }
derive instance newtypeDeletePatchBaselineRequest :: Newtype DeletePatchBaselineRequest _


newtype DeletePatchBaselineResult = DeletePatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  }
derive instance newtypeDeletePatchBaselineResult :: Newtype DeletePatchBaselineResult _


newtype DeleteResourceDataSyncRequest = DeleteResourceDataSyncRequest 
  { "SyncName" :: (ResourceDataSyncName)
  }
derive instance newtypeDeleteResourceDataSyncRequest :: Newtype DeleteResourceDataSyncRequest _


newtype DeleteResourceDataSyncResult = DeleteResourceDataSyncResult 
  { 
  }
derive instance newtypeDeleteResourceDataSyncResult :: Newtype DeleteResourceDataSyncResult _


newtype DeregisterManagedInstanceRequest = DeregisterManagedInstanceRequest 
  { "InstanceId" :: (ManagedInstanceId)
  }
derive instance newtypeDeregisterManagedInstanceRequest :: Newtype DeregisterManagedInstanceRequest _


newtype DeregisterManagedInstanceResult = DeregisterManagedInstanceResult 
  { 
  }
derive instance newtypeDeregisterManagedInstanceResult :: Newtype DeregisterManagedInstanceResult _


newtype DeregisterPatchBaselineForPatchGroupRequest = DeregisterPatchBaselineForPatchGroupRequest 
  { "BaselineId" :: (BaselineId)
  , "PatchGroup" :: (PatchGroup)
  }
derive instance newtypeDeregisterPatchBaselineForPatchGroupRequest :: Newtype DeregisterPatchBaselineForPatchGroupRequest _


newtype DeregisterPatchBaselineForPatchGroupResult = DeregisterPatchBaselineForPatchGroupResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "PatchGroup" :: NullOrUndefined (PatchGroup)
  }
derive instance newtypeDeregisterPatchBaselineForPatchGroupResult :: Newtype DeregisterPatchBaselineForPatchGroupResult _


newtype DeregisterTargetFromMaintenanceWindowRequest = DeregisterTargetFromMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTargetId" :: (MaintenanceWindowTargetId)
  , "Safe" :: NullOrUndefined (Boolean)
  }
derive instance newtypeDeregisterTargetFromMaintenanceWindowRequest :: Newtype DeregisterTargetFromMaintenanceWindowRequest _


newtype DeregisterTargetFromMaintenanceWindowResult = DeregisterTargetFromMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId)
  }
derive instance newtypeDeregisterTargetFromMaintenanceWindowResult :: Newtype DeregisterTargetFromMaintenanceWindowResult _


newtype DeregisterTaskFromMaintenanceWindowRequest = DeregisterTaskFromMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTaskId" :: (MaintenanceWindowTaskId)
  }
derive instance newtypeDeregisterTaskFromMaintenanceWindowRequest :: Newtype DeregisterTaskFromMaintenanceWindowRequest _


newtype DeregisterTaskFromMaintenanceWindowResult = DeregisterTaskFromMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId)
  }
derive instance newtypeDeregisterTaskFromMaintenanceWindowResult :: Newtype DeregisterTaskFromMaintenanceWindowResult _


-- | <p>Filter for the DescribeActivation API.</p>
newtype DescribeActivationsFilter = DescribeActivationsFilter 
  { "FilterKey" :: NullOrUndefined (DescribeActivationsFilterKeys)
  , "FilterValues" :: NullOrUndefined (StringList)
  }
derive instance newtypeDescribeActivationsFilter :: Newtype DescribeActivationsFilter _


newtype DescribeActivationsFilterKeys = DescribeActivationsFilterKeys String
derive instance newtypeDescribeActivationsFilterKeys :: Newtype DescribeActivationsFilterKeys _


newtype DescribeActivationsFilterList = DescribeActivationsFilterList (Array DescribeActivationsFilter)
derive instance newtypeDescribeActivationsFilterList :: Newtype DescribeActivationsFilterList _


newtype DescribeActivationsRequest = DescribeActivationsRequest 
  { "Filters" :: NullOrUndefined (DescribeActivationsFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeActivationsRequest :: Newtype DescribeActivationsRequest _


newtype DescribeActivationsResult = DescribeActivationsResult 
  { "ActivationList" :: NullOrUndefined (ActivationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeActivationsResult :: Newtype DescribeActivationsResult _


newtype DescribeAssociationRequest = DescribeAssociationRequest 
  { "Name" :: NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "AssociationId" :: NullOrUndefined (AssociationId)
  , "AssociationVersion" :: NullOrUndefined (AssociationVersion)
  }
derive instance newtypeDescribeAssociationRequest :: Newtype DescribeAssociationRequest _


newtype DescribeAssociationResult = DescribeAssociationResult 
  { "AssociationDescription" :: NullOrUndefined (AssociationDescription)
  }
derive instance newtypeDescribeAssociationResult :: Newtype DescribeAssociationResult _


newtype DescribeAutomationExecutionsRequest = DescribeAutomationExecutionsRequest 
  { "Filters" :: NullOrUndefined (AutomationExecutionFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAutomationExecutionsRequest :: Newtype DescribeAutomationExecutionsRequest _


newtype DescribeAutomationExecutionsResult = DescribeAutomationExecutionsResult 
  { "AutomationExecutionMetadataList" :: NullOrUndefined (AutomationExecutionMetadataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAutomationExecutionsResult :: Newtype DescribeAutomationExecutionsResult _


newtype DescribeAutomationStepExecutionsRequest = DescribeAutomationStepExecutionsRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  , "Filters" :: NullOrUndefined (StepExecutionFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "ReverseOrder" :: NullOrUndefined (Boolean)
  }
derive instance newtypeDescribeAutomationStepExecutionsRequest :: Newtype DescribeAutomationStepExecutionsRequest _


newtype DescribeAutomationStepExecutionsResult = DescribeAutomationStepExecutionsResult 
  { "StepExecutions" :: NullOrUndefined (StepExecutionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAutomationStepExecutionsResult :: Newtype DescribeAutomationStepExecutionsResult _


newtype DescribeAvailablePatchesRequest = DescribeAvailablePatchesRequest 
  { "Filters" :: NullOrUndefined (PatchOrchestratorFilterList)
  , "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAvailablePatchesRequest :: Newtype DescribeAvailablePatchesRequest _


newtype DescribeAvailablePatchesResult = DescribeAvailablePatchesResult 
  { "Patches" :: NullOrUndefined (PatchList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAvailablePatchesResult :: Newtype DescribeAvailablePatchesResult _


newtype DescribeDocumentPermissionRequest = DescribeDocumentPermissionRequest 
  { "Name" :: (DocumentName)
  , "PermissionType" :: (DocumentPermissionType)
  }
derive instance newtypeDescribeDocumentPermissionRequest :: Newtype DescribeDocumentPermissionRequest _


newtype DescribeDocumentPermissionResponse = DescribeDocumentPermissionResponse 
  { "AccountIds" :: NullOrUndefined (AccountIdList)
  }
derive instance newtypeDescribeDocumentPermissionResponse :: Newtype DescribeDocumentPermissionResponse _


newtype DescribeDocumentRequest = DescribeDocumentRequest 
  { "Name" :: (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  }
derive instance newtypeDescribeDocumentRequest :: Newtype DescribeDocumentRequest _


newtype DescribeDocumentResult = DescribeDocumentResult 
  { "Document" :: NullOrUndefined (DocumentDescription)
  }
derive instance newtypeDescribeDocumentResult :: Newtype DescribeDocumentResult _


newtype DescribeEffectiveInstanceAssociationsRequest = DescribeEffectiveInstanceAssociationsRequest 
  { "InstanceId" :: (InstanceId)
  , "MaxResults" :: NullOrUndefined (EffectiveInstanceAssociationMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeEffectiveInstanceAssociationsRequest :: Newtype DescribeEffectiveInstanceAssociationsRequest _


newtype DescribeEffectiveInstanceAssociationsResult = DescribeEffectiveInstanceAssociationsResult 
  { "Associations" :: NullOrUndefined (InstanceAssociationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeEffectiveInstanceAssociationsResult :: Newtype DescribeEffectiveInstanceAssociationsResult _


newtype DescribeEffectivePatchesForPatchBaselineRequest = DescribeEffectivePatchesForPatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  , "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeEffectivePatchesForPatchBaselineRequest :: Newtype DescribeEffectivePatchesForPatchBaselineRequest _


newtype DescribeEffectivePatchesForPatchBaselineResult = DescribeEffectivePatchesForPatchBaselineResult 
  { "EffectivePatches" :: NullOrUndefined (EffectivePatchList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeEffectivePatchesForPatchBaselineResult :: Newtype DescribeEffectivePatchesForPatchBaselineResult _


newtype DescribeInstanceAssociationsStatusRequest = DescribeInstanceAssociationsStatusRequest 
  { "InstanceId" :: (InstanceId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstanceAssociationsStatusRequest :: Newtype DescribeInstanceAssociationsStatusRequest _


newtype DescribeInstanceAssociationsStatusResult = DescribeInstanceAssociationsStatusResult 
  { "InstanceAssociationStatusInfos" :: NullOrUndefined (InstanceAssociationStatusInfos)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstanceAssociationsStatusResult :: Newtype DescribeInstanceAssociationsStatusResult _


newtype DescribeInstanceInformationRequest = DescribeInstanceInformationRequest 
  { "InstanceInformationFilterList" :: NullOrUndefined (InstanceInformationFilterList)
  , "Filters" :: NullOrUndefined (InstanceInformationStringFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResultsEC2Compatible)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstanceInformationRequest :: Newtype DescribeInstanceInformationRequest _


newtype DescribeInstanceInformationResult = DescribeInstanceInformationResult 
  { "InstanceInformationList" :: NullOrUndefined (InstanceInformationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstanceInformationResult :: Newtype DescribeInstanceInformationResult _


newtype DescribeInstancePatchStatesForPatchGroupRequest = DescribeInstancePatchStatesForPatchGroupRequest 
  { "PatchGroup" :: (PatchGroup)
  , "Filters" :: NullOrUndefined (InstancePatchStateFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults)
  }
derive instance newtypeDescribeInstancePatchStatesForPatchGroupRequest :: Newtype DescribeInstancePatchStatesForPatchGroupRequest _


newtype DescribeInstancePatchStatesForPatchGroupResult = DescribeInstancePatchStatesForPatchGroupResult 
  { "InstancePatchStates" :: NullOrUndefined (InstancePatchStatesList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancePatchStatesForPatchGroupResult :: Newtype DescribeInstancePatchStatesForPatchGroupResult _


newtype DescribeInstancePatchStatesRequest = DescribeInstancePatchStatesRequest 
  { "InstanceIds" :: (InstanceIdList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults)
  }
derive instance newtypeDescribeInstancePatchStatesRequest :: Newtype DescribeInstancePatchStatesRequest _


newtype DescribeInstancePatchStatesResult = DescribeInstancePatchStatesResult 
  { "InstancePatchStates" :: NullOrUndefined (InstancePatchStateList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancePatchStatesResult :: Newtype DescribeInstancePatchStatesResult _


newtype DescribeInstancePatchesRequest = DescribeInstancePatchesRequest 
  { "InstanceId" :: (InstanceId)
  , "Filters" :: NullOrUndefined (PatchOrchestratorFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (PatchComplianceMaxResults)
  }
derive instance newtypeDescribeInstancePatchesRequest :: Newtype DescribeInstancePatchesRequest _


newtype DescribeInstancePatchesResult = DescribeInstancePatchesResult 
  { "Patches" :: NullOrUndefined (PatchComplianceDataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancePatchesResult :: Newtype DescribeInstancePatchesResult _


newtype DescribeMaintenanceWindowExecutionTaskInvocationsRequest = DescribeMaintenanceWindowExecutionTaskInvocationsRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "TaskId" :: (MaintenanceWindowExecutionTaskId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionTaskInvocationsRequest :: Newtype DescribeMaintenanceWindowExecutionTaskInvocationsRequest _


newtype DescribeMaintenanceWindowExecutionTaskInvocationsResult = DescribeMaintenanceWindowExecutionTaskInvocationsResult 
  { "WindowExecutionTaskInvocationIdentities" :: NullOrUndefined (MaintenanceWindowExecutionTaskInvocationIdentityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionTaskInvocationsResult :: Newtype DescribeMaintenanceWindowExecutionTaskInvocationsResult _


newtype DescribeMaintenanceWindowExecutionTasksRequest = DescribeMaintenanceWindowExecutionTasksRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionTasksRequest :: Newtype DescribeMaintenanceWindowExecutionTasksRequest _


newtype DescribeMaintenanceWindowExecutionTasksResult = DescribeMaintenanceWindowExecutionTasksResult 
  { "WindowExecutionTaskIdentities" :: NullOrUndefined (MaintenanceWindowExecutionTaskIdentityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionTasksResult :: Newtype DescribeMaintenanceWindowExecutionTasksResult _


newtype DescribeMaintenanceWindowExecutionsRequest = DescribeMaintenanceWindowExecutionsRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionsRequest :: Newtype DescribeMaintenanceWindowExecutionsRequest _


newtype DescribeMaintenanceWindowExecutionsResult = DescribeMaintenanceWindowExecutionsResult 
  { "WindowExecutions" :: NullOrUndefined (MaintenanceWindowExecutionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionsResult :: Newtype DescribeMaintenanceWindowExecutionsResult _


newtype DescribeMaintenanceWindowTargetsRequest = DescribeMaintenanceWindowTargetsRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowTargetsRequest :: Newtype DescribeMaintenanceWindowTargetsRequest _


newtype DescribeMaintenanceWindowTargetsResult = DescribeMaintenanceWindowTargetsResult 
  { "Targets" :: NullOrUndefined (MaintenanceWindowTargetList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowTargetsResult :: Newtype DescribeMaintenanceWindowTargetsResult _


newtype DescribeMaintenanceWindowTasksRequest = DescribeMaintenanceWindowTasksRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowTasksRequest :: Newtype DescribeMaintenanceWindowTasksRequest _


newtype DescribeMaintenanceWindowTasksResult = DescribeMaintenanceWindowTasksResult 
  { "Tasks" :: NullOrUndefined (MaintenanceWindowTaskList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowTasksResult :: Newtype DescribeMaintenanceWindowTasksResult _


newtype DescribeMaintenanceWindowsRequest = DescribeMaintenanceWindowsRequest 
  { "Filters" :: NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowsRequest :: Newtype DescribeMaintenanceWindowsRequest _


newtype DescribeMaintenanceWindowsResult = DescribeMaintenanceWindowsResult 
  { "WindowIdentities" :: NullOrUndefined (MaintenanceWindowIdentityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowsResult :: Newtype DescribeMaintenanceWindowsResult _


newtype DescribeParametersRequest = DescribeParametersRequest 
  { "Filters" :: NullOrUndefined (ParametersFilterList)
  , "ParameterFilters" :: NullOrUndefined (ParameterStringFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeParametersRequest :: Newtype DescribeParametersRequest _


newtype DescribeParametersResult = DescribeParametersResult 
  { "Parameters" :: NullOrUndefined (ParameterMetadataList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeParametersResult :: Newtype DescribeParametersResult _


newtype DescribePatchBaselinesRequest = DescribePatchBaselinesRequest 
  { "Filters" :: NullOrUndefined (PatchOrchestratorFilterList)
  , "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePatchBaselinesRequest :: Newtype DescribePatchBaselinesRequest _


newtype DescribePatchBaselinesResult = DescribePatchBaselinesResult 
  { "BaselineIdentities" :: NullOrUndefined (PatchBaselineIdentityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePatchBaselinesResult :: Newtype DescribePatchBaselinesResult _


newtype DescribePatchGroupStateRequest = DescribePatchGroupStateRequest 
  { "PatchGroup" :: (PatchGroup)
  }
derive instance newtypeDescribePatchGroupStateRequest :: Newtype DescribePatchGroupStateRequest _


newtype DescribePatchGroupStateResult = DescribePatchGroupStateResult 
  { "Instances" :: NullOrUndefined (Int)
  , "InstancesWithInstalledPatches" :: NullOrUndefined (Int)
  , "InstancesWithInstalledOtherPatches" :: NullOrUndefined (Int)
  , "InstancesWithMissingPatches" :: NullOrUndefined (Int)
  , "InstancesWithFailedPatches" :: NullOrUndefined (Int)
  , "InstancesWithNotApplicablePatches" :: NullOrUndefined (Int)
  }
derive instance newtypeDescribePatchGroupStateResult :: Newtype DescribePatchGroupStateResult _


newtype DescribePatchGroupsRequest = DescribePatchGroupsRequest 
  { "MaxResults" :: NullOrUndefined (PatchBaselineMaxResults)
  , "Filters" :: NullOrUndefined (PatchOrchestratorFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePatchGroupsRequest :: Newtype DescribePatchGroupsRequest _


newtype DescribePatchGroupsResult = DescribePatchGroupsResult 
  { "Mappings" :: NullOrUndefined (PatchGroupPatchBaselineMappingList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePatchGroupsResult :: Newtype DescribePatchGroupsResult _


newtype DescriptionInDocument = DescriptionInDocument String
derive instance newtypeDescriptionInDocument :: Newtype DescriptionInDocument _


newtype DocumentARN = DocumentARN String
derive instance newtypeDocumentARN :: Newtype DocumentARN _


-- | <p>The specified document already exists.</p>
newtype DocumentAlreadyExists = DocumentAlreadyExists 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeDocumentAlreadyExists :: Newtype DocumentAlreadyExists _


newtype DocumentContent = DocumentContent String
derive instance newtypeDocumentContent :: Newtype DocumentContent _


-- | <p>A default version of a document.</p>
newtype DocumentDefaultVersionDescription = DocumentDefaultVersionDescription 
  { "Name" :: NullOrUndefined (DocumentName)
  , "DefaultVersion" :: NullOrUndefined (DocumentVersion)
  }
derive instance newtypeDocumentDefaultVersionDescription :: Newtype DocumentDefaultVersionDescription _


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
derive instance newtypeDocumentDescription :: Newtype DocumentDescription _


-- | <p>Describes a filter.</p>
newtype DocumentFilter = DocumentFilter 
  { "Key'" :: (DocumentFilterKey)
  , "Value'" :: (DocumentFilterValue)
  }
derive instance newtypeDocumentFilter :: Newtype DocumentFilter _


newtype DocumentFilterKey = DocumentFilterKey String
derive instance newtypeDocumentFilterKey :: Newtype DocumentFilterKey _


newtype DocumentFilterList = DocumentFilterList (Array DocumentFilter)
derive instance newtypeDocumentFilterList :: Newtype DocumentFilterList _


newtype DocumentFilterValue = DocumentFilterValue String
derive instance newtypeDocumentFilterValue :: Newtype DocumentFilterValue _


newtype DocumentFormat = DocumentFormat String
derive instance newtypeDocumentFormat :: Newtype DocumentFormat _


newtype DocumentHash = DocumentHash String
derive instance newtypeDocumentHash :: Newtype DocumentHash _


newtype DocumentHashType = DocumentHashType String
derive instance newtypeDocumentHashType :: Newtype DocumentHashType _


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
derive instance newtypeDocumentIdentifier :: Newtype DocumentIdentifier _


newtype DocumentIdentifierList = DocumentIdentifierList (Array DocumentIdentifier)
derive instance newtypeDocumentIdentifierList :: Newtype DocumentIdentifierList _


-- | <p>One or more filters. Use a filter to return a more specific list of documents.</p> <p>For keys, you can specify one or more tags that have been applied to a document. </p> <p>Other valid values include Owner, Name, PlatformTypes, and DocumentType.</p> <p>Note that only one Owner can be specified in a request. For example: <code>Key=Owner,Values=Self</code>.</p> <p>If you use Name as a key, you can use a name prefix to return a list of documents. For example, in the AWS CLI, to return a list of all documents that begin with <code>Te</code>, run the following command:</p> <p> <code>aws ssm list-documents --filters Key=Name,Values=Te</code> </p> <p>If you specify more than two keys, only documents that are identified by all the tags are returned in the results. If you specify more than two values for a key, documents that are identified by any of the values are returned in the results.</p> <p>To specify a custom key and value pair, use the format <code>Key=tag:[tagName],Values=[valueName]</code>.</p> <p>For example, if you created a Key called region and are using the AWS CLI to call the <code>list-documents</code> command: </p> <p> <code>aws ssm list-documents --filters Key=tag:region,Values=east,west Key=Owner,Values=Self</code> </p>
newtype DocumentKeyValuesFilter = DocumentKeyValuesFilter 
  { "Key" :: NullOrUndefined (DocumentKeyValuesFilterKey)
  , "Values" :: NullOrUndefined (DocumentKeyValuesFilterValues)
  }
derive instance newtypeDocumentKeyValuesFilter :: Newtype DocumentKeyValuesFilter _


newtype DocumentKeyValuesFilterKey = DocumentKeyValuesFilterKey String
derive instance newtypeDocumentKeyValuesFilterKey :: Newtype DocumentKeyValuesFilterKey _


newtype DocumentKeyValuesFilterList = DocumentKeyValuesFilterList (Array DocumentKeyValuesFilter)
derive instance newtypeDocumentKeyValuesFilterList :: Newtype DocumentKeyValuesFilterList _


newtype DocumentKeyValuesFilterValue = DocumentKeyValuesFilterValue String
derive instance newtypeDocumentKeyValuesFilterValue :: Newtype DocumentKeyValuesFilterValue _


newtype DocumentKeyValuesFilterValues = DocumentKeyValuesFilterValues (Array DocumentKeyValuesFilterValue)
derive instance newtypeDocumentKeyValuesFilterValues :: Newtype DocumentKeyValuesFilterValues _


-- | <p>You can have at most 200 active Systems Manager documents.</p>
newtype DocumentLimitExceeded = DocumentLimitExceeded 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeDocumentLimitExceeded :: Newtype DocumentLimitExceeded _


newtype DocumentName = DocumentName String
derive instance newtypeDocumentName :: Newtype DocumentName _


newtype DocumentOwner = DocumentOwner String
derive instance newtypeDocumentOwner :: Newtype DocumentOwner _


-- | <p>Parameters specified in a System Manager document that execute on the server when the command is run. </p>
newtype DocumentParameter = DocumentParameter 
  { "Name" :: NullOrUndefined (DocumentParameterName)
  , "Type" :: NullOrUndefined (DocumentParameterType)
  , "Description" :: NullOrUndefined (DocumentParameterDescrption)
  , "DefaultValue" :: NullOrUndefined (DocumentParameterDefaultValue)
  }
derive instance newtypeDocumentParameter :: Newtype DocumentParameter _


newtype DocumentParameterDefaultValue = DocumentParameterDefaultValue String
derive instance newtypeDocumentParameterDefaultValue :: Newtype DocumentParameterDefaultValue _


newtype DocumentParameterDescrption = DocumentParameterDescrption String
derive instance newtypeDocumentParameterDescrption :: Newtype DocumentParameterDescrption _


newtype DocumentParameterList = DocumentParameterList (Array DocumentParameter)
derive instance newtypeDocumentParameterList :: Newtype DocumentParameterList _


newtype DocumentParameterName = DocumentParameterName String
derive instance newtypeDocumentParameterName :: Newtype DocumentParameterName _


newtype DocumentParameterType = DocumentParameterType String
derive instance newtypeDocumentParameterType :: Newtype DocumentParameterType _


-- | <p>The document cannot be shared with more AWS user accounts. You can share a document with a maximum of 20 accounts. You can publicly share up to five documents. If you need to increase this limit, contact AWS Support.</p>
newtype DocumentPermissionLimit = DocumentPermissionLimit 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeDocumentPermissionLimit :: Newtype DocumentPermissionLimit _


newtype DocumentPermissionType = DocumentPermissionType String
derive instance newtypeDocumentPermissionType :: Newtype DocumentPermissionType _


newtype DocumentSchemaVersion = DocumentSchemaVersion String
derive instance newtypeDocumentSchemaVersion :: Newtype DocumentSchemaVersion _


newtype DocumentSha1 = DocumentSha1 String
derive instance newtypeDocumentSha1 :: Newtype DocumentSha1 _


newtype DocumentStatus = DocumentStatus String
derive instance newtypeDocumentStatus :: Newtype DocumentStatus _


newtype DocumentType = DocumentType String
derive instance newtypeDocumentType :: Newtype DocumentType _


newtype DocumentVersion = DocumentVersion String
derive instance newtypeDocumentVersion :: Newtype DocumentVersion _


-- | <p>Version information about the document.</p>
newtype DocumentVersionInfo = DocumentVersionInfo 
  { "Name" :: NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "CreatedDate" :: NullOrUndefined (DateTime)
  , "IsDefaultVersion" :: NullOrUndefined (Boolean)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  }
derive instance newtypeDocumentVersionInfo :: Newtype DocumentVersionInfo _


-- | <p>The document has too many versions. Delete one or more document versions and try again.</p>
newtype DocumentVersionLimitExceeded = DocumentVersionLimitExceeded 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeDocumentVersionLimitExceeded :: Newtype DocumentVersionLimitExceeded _


newtype DocumentVersionList = DocumentVersionList (Array DocumentVersionInfo)
derive instance newtypeDocumentVersionList :: Newtype DocumentVersionList _


newtype DocumentVersionNumber = DocumentVersionNumber String
derive instance newtypeDocumentVersionNumber :: Newtype DocumentVersionNumber _


-- | <p>Error returned when the ID specified for a resource, such as a Maintenance Window or Patch baseline, doesn't exist.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>
newtype DoesNotExistException = DoesNotExistException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeDoesNotExistException :: Newtype DoesNotExistException _


-- | <p>The content of the association document matches another document. Change the content of the document and try again.</p>
newtype DuplicateDocumentContent = DuplicateDocumentContent 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeDuplicateDocumentContent :: Newtype DuplicateDocumentContent _


-- | <p>You cannot specify an instance ID in more than one association.</p>
newtype DuplicateInstanceId = DuplicateInstanceId 
  { 
  }
derive instance newtypeDuplicateInstanceId :: Newtype DuplicateInstanceId _


newtype EffectiveInstanceAssociationMaxResults = EffectiveInstanceAssociationMaxResults Int
derive instance newtypeEffectiveInstanceAssociationMaxResults :: Newtype EffectiveInstanceAssociationMaxResults _


-- | <p>The EffectivePatch structure defines metadata about a patch along with the approval state of the patch in a particular patch baseline. The approval state includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.</p>
newtype EffectivePatch = EffectivePatch 
  { "Patch" :: NullOrUndefined (Patch)
  , "PatchStatus" :: NullOrUndefined (PatchStatus)
  }
derive instance newtypeEffectivePatch :: Newtype EffectivePatch _


newtype EffectivePatchList = EffectivePatchList (Array EffectivePatch)
derive instance newtypeEffectivePatchList :: Newtype EffectivePatchList _


newtype ErrorCount = ErrorCount Int
derive instance newtypeErrorCount :: Newtype ErrorCount _


newtype ExecutionMode = ExecutionMode String
derive instance newtypeExecutionMode :: Newtype ExecutionMode _


newtype ExpirationDate = ExpirationDate Number
derive instance newtypeExpirationDate :: Newtype ExpirationDate _


-- | <p>Describes a failed association.</p>
newtype FailedCreateAssociation = FailedCreateAssociation 
  { "Entry" :: NullOrUndefined (CreateAssociationBatchRequestEntry)
  , "Message" :: NullOrUndefined (BatchErrorMessage)
  , "Fault" :: NullOrUndefined (Fault)
  }
derive instance newtypeFailedCreateAssociation :: Newtype FailedCreateAssociation _


newtype FailedCreateAssociationList = FailedCreateAssociationList (Array FailedCreateAssociation)
derive instance newtypeFailedCreateAssociationList :: Newtype FailedCreateAssociationList _


-- | <p>Information about an Automation failure.</p>
newtype FailureDetails = FailureDetails 
  { "FailureStage" :: NullOrUndefined (String)
  , "FailureType" :: NullOrUndefined (String)
  , "Details" :: NullOrUndefined (AutomationParameterMap)
  }
derive instance newtypeFailureDetails :: Newtype FailureDetails _


newtype Fault = Fault String
derive instance newtypeFault :: Newtype Fault _


-- | <p>You attempted to register a LAMBDA or STEP_FUNCTION task in a region where the corresponding service is not available. </p>
newtype FeatureNotAvailableException = FeatureNotAvailableException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeFeatureNotAvailableException :: Newtype FeatureNotAvailableException _


newtype GetAutomationExecutionRequest = GetAutomationExecutionRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  }
derive instance newtypeGetAutomationExecutionRequest :: Newtype GetAutomationExecutionRequest _


newtype GetAutomationExecutionResult = GetAutomationExecutionResult 
  { "AutomationExecution" :: NullOrUndefined (AutomationExecution)
  }
derive instance newtypeGetAutomationExecutionResult :: Newtype GetAutomationExecutionResult _


newtype GetCommandInvocationRequest = GetCommandInvocationRequest 
  { "CommandId" :: (CommandId)
  , "InstanceId" :: (InstanceId)
  , "PluginName" :: NullOrUndefined (CommandPluginName)
  }
derive instance newtypeGetCommandInvocationRequest :: Newtype GetCommandInvocationRequest _


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
derive instance newtypeGetCommandInvocationResult :: Newtype GetCommandInvocationResult _


newtype GetDefaultPatchBaselineRequest = GetDefaultPatchBaselineRequest 
  { "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  }
derive instance newtypeGetDefaultPatchBaselineRequest :: Newtype GetDefaultPatchBaselineRequest _


newtype GetDefaultPatchBaselineResult = GetDefaultPatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  }
derive instance newtypeGetDefaultPatchBaselineResult :: Newtype GetDefaultPatchBaselineResult _


newtype GetDeployablePatchSnapshotForInstanceRequest = GetDeployablePatchSnapshotForInstanceRequest 
  { "InstanceId" :: (InstanceId)
  , "SnapshotId" :: (SnapshotId)
  }
derive instance newtypeGetDeployablePatchSnapshotForInstanceRequest :: Newtype GetDeployablePatchSnapshotForInstanceRequest _


newtype GetDeployablePatchSnapshotForInstanceResult = GetDeployablePatchSnapshotForInstanceResult 
  { "InstanceId" :: NullOrUndefined (InstanceId)
  , "SnapshotId" :: NullOrUndefined (SnapshotId)
  , "SnapshotDownloadUrl" :: NullOrUndefined (SnapshotDownloadUrl)
  , "Product" :: NullOrUndefined (Product)
  }
derive instance newtypeGetDeployablePatchSnapshotForInstanceResult :: Newtype GetDeployablePatchSnapshotForInstanceResult _


newtype GetDocumentRequest = GetDocumentRequest 
  { "Name" :: (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  }
derive instance newtypeGetDocumentRequest :: Newtype GetDocumentRequest _


newtype GetDocumentResult = GetDocumentResult 
  { "Name" :: NullOrUndefined (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Content" :: NullOrUndefined (DocumentContent)
  , "DocumentType" :: NullOrUndefined (DocumentType)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  }
derive instance newtypeGetDocumentResult :: Newtype GetDocumentResult _


newtype GetInventoryRequest = GetInventoryRequest 
  { "Filters" :: NullOrUndefined (InventoryFilterList)
  , "Aggregators" :: NullOrUndefined (InventoryAggregatorList)
  , "ResultAttributes" :: NullOrUndefined (ResultAttributeList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeGetInventoryRequest :: Newtype GetInventoryRequest _


newtype GetInventoryResult = GetInventoryResult 
  { "Entities" :: NullOrUndefined (InventoryResultEntityList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetInventoryResult :: Newtype GetInventoryResult _


newtype GetInventorySchemaMaxResults = GetInventorySchemaMaxResults Int
derive instance newtypeGetInventorySchemaMaxResults :: Newtype GetInventorySchemaMaxResults _


newtype GetInventorySchemaRequest = GetInventorySchemaRequest 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeNameFilter)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (GetInventorySchemaMaxResults)
  , "Aggregator" :: NullOrUndefined (AggregatorSchemaOnly)
  , "SubType" :: NullOrUndefined (IsSubTypeSchema)
  }
derive instance newtypeGetInventorySchemaRequest :: Newtype GetInventorySchemaRequest _


newtype GetInventorySchemaResult = GetInventorySchemaResult 
  { "Schemas" :: NullOrUndefined (InventoryItemSchemaResultList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetInventorySchemaResult :: Newtype GetInventorySchemaResult _


newtype GetMaintenanceWindowExecutionRequest = GetMaintenanceWindowExecutionRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  }
derive instance newtypeGetMaintenanceWindowExecutionRequest :: Newtype GetMaintenanceWindowExecutionRequest _


newtype GetMaintenanceWindowExecutionResult = GetMaintenanceWindowExecutionResult 
  { "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskIds" :: NullOrUndefined (MaintenanceWindowExecutionTaskIdList)
  , "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined (DateTime)
  }
derive instance newtypeGetMaintenanceWindowExecutionResult :: Newtype GetMaintenanceWindowExecutionResult _


newtype GetMaintenanceWindowExecutionTaskInvocationRequest = GetMaintenanceWindowExecutionTaskInvocationRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "TaskId" :: (MaintenanceWindowExecutionTaskId)
  , "InvocationId" :: (MaintenanceWindowExecutionTaskInvocationId)
  }
derive instance newtypeGetMaintenanceWindowExecutionTaskInvocationRequest :: Newtype GetMaintenanceWindowExecutionTaskInvocationRequest _


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
derive instance newtypeGetMaintenanceWindowExecutionTaskInvocationResult :: Newtype GetMaintenanceWindowExecutionTaskInvocationResult _


newtype GetMaintenanceWindowExecutionTaskRequest = GetMaintenanceWindowExecutionTaskRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "TaskId" :: (MaintenanceWindowExecutionTaskId)
  }
derive instance newtypeGetMaintenanceWindowExecutionTaskRequest :: Newtype GetMaintenanceWindowExecutionTaskRequest _


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
derive instance newtypeGetMaintenanceWindowExecutionTaskResult :: Newtype GetMaintenanceWindowExecutionTaskResult _


newtype GetMaintenanceWindowRequest = GetMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  }
derive instance newtypeGetMaintenanceWindowRequest :: Newtype GetMaintenanceWindowRequest _


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
derive instance newtypeGetMaintenanceWindowResult :: Newtype GetMaintenanceWindowResult _


newtype GetMaintenanceWindowTaskRequest = GetMaintenanceWindowTaskRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTaskId" :: (MaintenanceWindowTaskId)
  }
derive instance newtypeGetMaintenanceWindowTaskRequest :: Newtype GetMaintenanceWindowTaskRequest _


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
derive instance newtypeGetMaintenanceWindowTaskResult :: Newtype GetMaintenanceWindowTaskResult _


newtype GetParameterHistoryRequest = GetParameterHistoryRequest 
  { "Name" :: (PSParameterName)
  , "WithDecryption" :: NullOrUndefined (Boolean)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetParameterHistoryRequest :: Newtype GetParameterHistoryRequest _


newtype GetParameterHistoryResult = GetParameterHistoryResult 
  { "Parameters" :: NullOrUndefined (ParameterHistoryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetParameterHistoryResult :: Newtype GetParameterHistoryResult _


newtype GetParameterRequest = GetParameterRequest 
  { "Name" :: (PSParameterName)
  , "WithDecryption" :: NullOrUndefined (Boolean)
  }
derive instance newtypeGetParameterRequest :: Newtype GetParameterRequest _


newtype GetParameterResult = GetParameterResult 
  { "Parameter" :: NullOrUndefined (Parameter)
  }
derive instance newtypeGetParameterResult :: Newtype GetParameterResult _


newtype GetParametersByPathMaxResults = GetParametersByPathMaxResults Int
derive instance newtypeGetParametersByPathMaxResults :: Newtype GetParametersByPathMaxResults _


newtype GetParametersByPathRequest = GetParametersByPathRequest 
  { "Path" :: (PSParameterName)
  , "Recursive" :: NullOrUndefined (Boolean)
  , "ParameterFilters" :: NullOrUndefined (ParameterStringFilterList)
  , "WithDecryption" :: NullOrUndefined (Boolean)
  , "MaxResults" :: NullOrUndefined (GetParametersByPathMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetParametersByPathRequest :: Newtype GetParametersByPathRequest _


newtype GetParametersByPathResult = GetParametersByPathResult 
  { "Parameters" :: NullOrUndefined (ParameterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetParametersByPathResult :: Newtype GetParametersByPathResult _


newtype GetParametersRequest = GetParametersRequest 
  { "Names" :: (ParameterNameList)
  , "WithDecryption" :: NullOrUndefined (Boolean)
  }
derive instance newtypeGetParametersRequest :: Newtype GetParametersRequest _


newtype GetParametersResult = GetParametersResult 
  { "Parameters" :: NullOrUndefined (ParameterList)
  , "InvalidParameters" :: NullOrUndefined (ParameterNameList)
  }
derive instance newtypeGetParametersResult :: Newtype GetParametersResult _


newtype GetPatchBaselineForPatchGroupRequest = GetPatchBaselineForPatchGroupRequest 
  { "PatchGroup" :: (PatchGroup)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  }
derive instance newtypeGetPatchBaselineForPatchGroupRequest :: Newtype GetPatchBaselineForPatchGroupRequest _


newtype GetPatchBaselineForPatchGroupResult = GetPatchBaselineForPatchGroupResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "PatchGroup" :: NullOrUndefined (PatchGroup)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  }
derive instance newtypeGetPatchBaselineForPatchGroupResult :: Newtype GetPatchBaselineForPatchGroupResult _


newtype GetPatchBaselineRequest = GetPatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  }
derive instance newtypeGetPatchBaselineRequest :: Newtype GetPatchBaselineRequest _


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
derive instance newtypeGetPatchBaselineResult :: Newtype GetPatchBaselineResult _


-- | <p>A hierarchy can have a maximum of 15 levels. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html">Working with Systems Manager Parameters</a>. </p>
newtype HierarchyLevelLimitExceededException = HierarchyLevelLimitExceededException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeHierarchyLevelLimitExceededException :: Newtype HierarchyLevelLimitExceededException _


-- | <p>Parameter Store does not support changing a parameter type in a hierarchy. For example, you can't change a parameter from a String type to a SecureString type. You must create a new, unique parameter.</p>
newtype HierarchyTypeMismatchException = HierarchyTypeMismatchException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeHierarchyTypeMismatchException :: Newtype HierarchyTypeMismatchException _


newtype IPAddress = IPAddress String
derive instance newtypeIPAddress :: Newtype IPAddress _


newtype IamRole = IamRole String
derive instance newtypeIamRole :: Newtype IamRole _


newtype IdempotencyToken = IdempotencyToken String
derive instance newtypeIdempotencyToken :: Newtype IdempotencyToken _


-- | <p>Error returned when an idempotent operation is retried and the parameters don't match the original call to the API with the same idempotency token. </p>
newtype IdempotentParameterMismatch = IdempotentParameterMismatch 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeIdempotentParameterMismatch :: Newtype IdempotentParameterMismatch _


-- | <p>Status information about the aggregated associations.</p>
newtype InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview 
  { "DetailedStatus" :: NullOrUndefined (StatusName)
  , "InstanceAssociationStatusAggregatedCount" :: NullOrUndefined (InstanceAssociationStatusAggregatedCount)
  }
derive instance newtypeInstanceAggregatedAssociationOverview :: Newtype InstanceAggregatedAssociationOverview _


-- | <p>One or more association documents on the instance. </p>
newtype InstanceAssociation = InstanceAssociation 
  { "AssociationId" :: NullOrUndefined (AssociationId)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "Content" :: NullOrUndefined (DocumentContent)
  , "AssociationVersion" :: NullOrUndefined (AssociationVersion)
  }
derive instance newtypeInstanceAssociation :: Newtype InstanceAssociation _


newtype InstanceAssociationExecutionSummary = InstanceAssociationExecutionSummary String
derive instance newtypeInstanceAssociationExecutionSummary :: Newtype InstanceAssociationExecutionSummary _


newtype InstanceAssociationList = InstanceAssociationList (Array InstanceAssociation)
derive instance newtypeInstanceAssociationList :: Newtype InstanceAssociationList _


-- | <p>An Amazon S3 bucket where you want to store the results of this request.</p>
newtype InstanceAssociationOutputLocation = InstanceAssociationOutputLocation 
  { "S3Location" :: NullOrUndefined (S3OutputLocation)
  }
derive instance newtypeInstanceAssociationOutputLocation :: Newtype InstanceAssociationOutputLocation _


-- | <p>The URL of Amazon S3 bucket where you want to store the results of this request.</p>
newtype InstanceAssociationOutputUrl = InstanceAssociationOutputUrl 
  { "S3OutputUrl" :: NullOrUndefined (S3OutputUrl)
  }
derive instance newtypeInstanceAssociationOutputUrl :: Newtype InstanceAssociationOutputUrl _


newtype InstanceAssociationStatusAggregatedCount = InstanceAssociationStatusAggregatedCount (Map StatusName InstanceCount)
derive instance newtypeInstanceAssociationStatusAggregatedCount :: Newtype InstanceAssociationStatusAggregatedCount _


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
derive instance newtypeInstanceAssociationStatusInfo :: Newtype InstanceAssociationStatusInfo _


newtype InstanceAssociationStatusInfos = InstanceAssociationStatusInfos (Array InstanceAssociationStatusInfo)
derive instance newtypeInstanceAssociationStatusInfos :: Newtype InstanceAssociationStatusInfos _


newtype InstanceCount = InstanceCount Int
derive instance newtypeInstanceCount :: Newtype InstanceCount _


newtype InstanceId = InstanceId String
derive instance newtypeInstanceId :: Newtype InstanceId _


newtype InstanceIdList = InstanceIdList (Array InstanceId)
derive instance newtypeInstanceIdList :: Newtype InstanceIdList _


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
derive instance newtypeInstanceInformation :: Newtype InstanceInformation _


-- | <p>Describes a filter for a specific list of instances. </p>
newtype InstanceInformationFilter = InstanceInformationFilter 
  { "Key'" :: (InstanceInformationFilterKey)
  , "ValueSet'" :: (InstanceInformationFilterValueSet)
  }
derive instance newtypeInstanceInformationFilter :: Newtype InstanceInformationFilter _


newtype InstanceInformationFilterKey = InstanceInformationFilterKey String
derive instance newtypeInstanceInformationFilterKey :: Newtype InstanceInformationFilterKey _


newtype InstanceInformationFilterList = InstanceInformationFilterList (Array InstanceInformationFilter)
derive instance newtypeInstanceInformationFilterList :: Newtype InstanceInformationFilterList _


newtype InstanceInformationFilterValue = InstanceInformationFilterValue String
derive instance newtypeInstanceInformationFilterValue :: Newtype InstanceInformationFilterValue _


newtype InstanceInformationFilterValueSet = InstanceInformationFilterValueSet (Array InstanceInformationFilterValue)
derive instance newtypeInstanceInformationFilterValueSet :: Newtype InstanceInformationFilterValueSet _


newtype InstanceInformationList = InstanceInformationList (Array InstanceInformation)
derive instance newtypeInstanceInformationList :: Newtype InstanceInformationList _


-- | <p>The filters to describe or get information about your managed instances.</p>
newtype InstanceInformationStringFilter = InstanceInformationStringFilter 
  { "Key" :: (InstanceInformationStringFilterKey)
  , "Values" :: (InstanceInformationFilterValueSet)
  }
derive instance newtypeInstanceInformationStringFilter :: Newtype InstanceInformationStringFilter _


newtype InstanceInformationStringFilterKey = InstanceInformationStringFilterKey String
derive instance newtypeInstanceInformationStringFilterKey :: Newtype InstanceInformationStringFilterKey _


newtype InstanceInformationStringFilterList = InstanceInformationStringFilterList (Array InstanceInformationStringFilter)
derive instance newtypeInstanceInformationStringFilterList :: Newtype InstanceInformationStringFilterList _


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
derive instance newtypeInstancePatchState :: Newtype InstancePatchState _


-- | <p>Defines a filter used in DescribeInstancePatchStatesForPatchGroup used to scope down the information returned by the API.</p>
newtype InstancePatchStateFilter = InstancePatchStateFilter 
  { "Key" :: (InstancePatchStateFilterKey)
  , "Values" :: (InstancePatchStateFilterValues)
  , "Type" :: (InstancePatchStateOperatorType)
  }
derive instance newtypeInstancePatchStateFilter :: Newtype InstancePatchStateFilter _


newtype InstancePatchStateFilterKey = InstancePatchStateFilterKey String
derive instance newtypeInstancePatchStateFilterKey :: Newtype InstancePatchStateFilterKey _


newtype InstancePatchStateFilterList = InstancePatchStateFilterList (Array InstancePatchStateFilter)
derive instance newtypeInstancePatchStateFilterList :: Newtype InstancePatchStateFilterList _


newtype InstancePatchStateFilterValue = InstancePatchStateFilterValue String
derive instance newtypeInstancePatchStateFilterValue :: Newtype InstancePatchStateFilterValue _


newtype InstancePatchStateFilterValues = InstancePatchStateFilterValues (Array InstancePatchStateFilterValue)
derive instance newtypeInstancePatchStateFilterValues :: Newtype InstancePatchStateFilterValues _


newtype InstancePatchStateList = InstancePatchStateList (Array InstancePatchState)
derive instance newtypeInstancePatchStateList :: Newtype InstancePatchStateList _


newtype InstancePatchStateOperatorType = InstancePatchStateOperatorType String
derive instance newtypeInstancePatchStateOperatorType :: Newtype InstancePatchStateOperatorType _


newtype InstancePatchStatesList = InstancePatchStatesList (Array InstancePatchState)
derive instance newtypeInstancePatchStatesList :: Newtype InstancePatchStatesList _


newtype InstanceTagName = InstanceTagName String
derive instance newtypeInstanceTagName :: Newtype InstanceTagName _


-- | <p>An error occurred on the server side.</p>
newtype InternalServerError = InternalServerError 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _


-- | <p>The activation is not valid. The activation might have been deleted, or the ActivationId and the ActivationCode do not match.</p>
newtype InvalidActivation = InvalidActivation 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidActivation :: Newtype InvalidActivation _


-- | <p>The activation ID is not valid. Verify the you entered the correct ActivationId or ActivationCode and try again.</p>
newtype InvalidActivationId = InvalidActivationId 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidActivationId :: Newtype InvalidActivationId _


-- | <p>The request does not meet the regular expression requirement.</p>
newtype InvalidAllowedPatternException = InvalidAllowedPatternException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidAllowedPatternException :: Newtype InvalidAllowedPatternException _


-- | <p>The version you specified is not valid. Use ListAssociationVersions to view all versions of an association according to the association ID. Or, use the <code>$LATEST</code> parameter to view the latest version of the association.</p>
newtype InvalidAssociationVersion = InvalidAssociationVersion 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidAssociationVersion :: Newtype InvalidAssociationVersion _


-- | <p>The supplied parameters for invoking the specified Automation document are incorrect. For example, they may not match the set of parameters permitted for the specified Automation document.</p>
newtype InvalidAutomationExecutionParametersException = InvalidAutomationExecutionParametersException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidAutomationExecutionParametersException :: Newtype InvalidAutomationExecutionParametersException _


-- | <p>The signal is not valid for the current Automation execution.</p>
newtype InvalidAutomationSignalException = InvalidAutomationSignalException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidAutomationSignalException :: Newtype InvalidAutomationSignalException _


-- | <p>The specified update status operation is not valid.</p>
newtype InvalidAutomationStatusUpdateException = InvalidAutomationStatusUpdateException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidAutomationStatusUpdateException :: Newtype InvalidAutomationStatusUpdateException _


newtype InvalidCommandId = InvalidCommandId 
  { 
  }
derive instance newtypeInvalidCommandId :: Newtype InvalidCommandId _


-- | <p>The specified document does not exist.</p>
newtype InvalidDocument = InvalidDocument 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidDocument :: Newtype InvalidDocument _


-- | <p>The content for the document is not valid.</p>
newtype InvalidDocumentContent = InvalidDocumentContent 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidDocumentContent :: Newtype InvalidDocumentContent _


-- | <p>You attempted to delete a document while it is still shared. You must stop sharing the document before you can delete it.</p>
newtype InvalidDocumentOperation = InvalidDocumentOperation 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidDocumentOperation :: Newtype InvalidDocumentOperation _


-- | <p>The version of the document schema is not supported.</p>
newtype InvalidDocumentSchemaVersion = InvalidDocumentSchemaVersion 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidDocumentSchemaVersion :: Newtype InvalidDocumentSchemaVersion _


-- | <p>The document version is not valid or does not exist.</p>
newtype InvalidDocumentVersion = InvalidDocumentVersion 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidDocumentVersion :: Newtype InvalidDocumentVersion _


-- | <p>The filter name is not valid. Verify the you entered the correct name and try again.</p>
newtype InvalidFilter = InvalidFilter 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidFilter :: Newtype InvalidFilter _


-- | <p>The specified key is not valid.</p>
newtype InvalidFilterKey = InvalidFilterKey 
  { 
  }
derive instance newtypeInvalidFilterKey :: Newtype InvalidFilterKey _


-- | <p>The specified filter option is not valid. Valid options are Equals and BeginsWith. For Path filter, valid options are Recursive and OneLevel.</p>
newtype InvalidFilterOption = InvalidFilterOption 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidFilterOption :: Newtype InvalidFilterOption _


-- | <p>The filter value is not valid. Verify the value and try again.</p>
newtype InvalidFilterValue = InvalidFilterValue 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidFilterValue :: Newtype InvalidFilterValue _


-- | <p>The following problems can cause this exception:</p> <p>You do not have permission to access the instance.</p> <p>The SSM Agent is not running. On managed instances and Linux instances, verify that the SSM Agent is running. On EC2 Windows instances, verify that the EC2Config service is running.</p> <p>The SSM Agent or EC2Config service is not registered to the SSM endpoint. Try reinstalling the SSM Agent or EC2Config service.</p> <p>The instance is not in valid state. Valid states are: Running, Pending, Stopped, Stopping. Invalid states are: Shutting-down and Terminated.</p>
newtype InvalidInstanceId = InvalidInstanceId 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidInstanceId :: Newtype InvalidInstanceId _


-- | <p>The specified filter value is not valid.</p>
newtype InvalidInstanceInformationFilterValue = InvalidInstanceInformationFilterValue 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidInstanceInformationFilterValue :: Newtype InvalidInstanceInformationFilterValue _


-- | <p>You specified invalid keys or values in the <code>Context</code> attribute for <code>InventoryItem</code>. Verify the keys and values, and try again.</p>
newtype InvalidInventoryItemContextException = InvalidInventoryItemContextException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidInventoryItemContextException :: Newtype InvalidInventoryItemContextException _


-- | <p>One or more content items is not valid.</p>
newtype InvalidItemContentException = InvalidItemContentException 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidItemContentException :: Newtype InvalidItemContentException _


-- | <p>The query key ID is not valid.</p>
newtype InvalidKeyId = InvalidKeyId 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidKeyId :: Newtype InvalidKeyId _


-- | <p>The specified token is not valid.</p>
newtype InvalidNextToken = InvalidNextToken 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidNextToken :: Newtype InvalidNextToken _


-- | <p>One or more configuration items is not valid. Verify that a valid Amazon Resource Name (ARN) was provided for an Amazon SNS topic.</p>
newtype InvalidNotificationConfig = InvalidNotificationConfig 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidNotificationConfig :: Newtype InvalidNotificationConfig _


-- | <p>The S3 bucket does not exist.</p>
newtype InvalidOutputFolder = InvalidOutputFolder 
  { 
  }
derive instance newtypeInvalidOutputFolder :: Newtype InvalidOutputFolder _


-- | <p>The output location is not valid or does not exist.</p>
newtype InvalidOutputLocation = InvalidOutputLocation 
  { 
  }
derive instance newtypeInvalidOutputLocation :: Newtype InvalidOutputLocation _


-- | <p>You must specify values for all required parameters in the Systems Manager document. You can only supply values to parameters defined in the Systems Manager document.</p>
newtype InvalidParameters = InvalidParameters 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidParameters :: Newtype InvalidParameters _


-- | <p>The permission type is not supported. <i>Share</i> is the only supported permission type.</p>
newtype InvalidPermissionType = InvalidPermissionType 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidPermissionType :: Newtype InvalidPermissionType _


-- | <p>The plugin name is not valid.</p>
newtype InvalidPluginName = InvalidPluginName 
  { 
  }
derive instance newtypeInvalidPluginName :: Newtype InvalidPluginName _


-- | <p>The resource ID is not valid. Verify that you entered the correct ID and try again.</p>
newtype InvalidResourceId = InvalidResourceId 
  { 
  }
derive instance newtypeInvalidResourceId :: Newtype InvalidResourceId _


-- | <p>The resource type is not valid. For example, if you are attempting to tag an instance, the instance must be a registered, managed instance.</p>
newtype InvalidResourceType = InvalidResourceType 
  { 
  }
derive instance newtypeInvalidResourceType :: Newtype InvalidResourceType _


-- | <p>The specified inventory item result attribute is not valid.</p>
newtype InvalidResultAttributeException = InvalidResultAttributeException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidResultAttributeException :: Newtype InvalidResultAttributeException _


-- | <p>The role name can't contain invalid characters. Also verify that you specified an IAM role for notifications that includes the required trust policy. For information about configuring the IAM role for Run Command notifications, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/rc-sns-notifications.html">Configuring Amazon SNS Notifications for Run Command</a> in the <i>AWS Systems Manager User Guide</i>.</p>
newtype InvalidRole = InvalidRole 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidRole :: Newtype InvalidRole _


-- | <p>The schedule is invalid. Verify your cron or rate expression and try again.</p>
newtype InvalidSchedule = InvalidSchedule 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidSchedule :: Newtype InvalidSchedule _


-- | <p>The target is not valid or does not exist. It might not be configured for EC2 Systems Manager or you might not have permission to perform the operation.</p>
newtype InvalidTarget = InvalidTarget 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidTarget :: Newtype InvalidTarget _


-- | <p>The parameter type name is not valid.</p>
newtype InvalidTypeNameException = InvalidTypeNameException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidTypeNameException :: Newtype InvalidTypeNameException _


-- | <p>The update is not valid.</p>
newtype InvalidUpdate = InvalidUpdate 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidUpdate :: Newtype InvalidUpdate _


-- | <p>Specifies the inventory type and attribute for the aggregation execution.</p>
newtype InventoryAggregator = InventoryAggregator 
  { "Expression" :: NullOrUndefined (InventoryAggregatorExpression)
  , "Aggregators" :: NullOrUndefined (InventoryAggregatorList)
  }
derive instance newtypeInventoryAggregator :: Newtype InventoryAggregator _


newtype InventoryAggregatorExpression = InventoryAggregatorExpression String
derive instance newtypeInventoryAggregatorExpression :: Newtype InventoryAggregatorExpression _


newtype InventoryAggregatorList = InventoryAggregatorList (Array InventoryAggregator)
derive instance newtypeInventoryAggregatorList :: Newtype InventoryAggregatorList _


newtype InventoryAttributeDataType = InventoryAttributeDataType String
derive instance newtypeInventoryAttributeDataType :: Newtype InventoryAttributeDataType _


-- | <p>One or more filters. Use a filter to return a more specific list of results.</p>
newtype InventoryFilter = InventoryFilter 
  { "Key" :: (InventoryFilterKey)
  , "Values" :: (InventoryFilterValueList)
  , "Type" :: NullOrUndefined (InventoryQueryOperatorType)
  }
derive instance newtypeInventoryFilter :: Newtype InventoryFilter _


newtype InventoryFilterKey = InventoryFilterKey String
derive instance newtypeInventoryFilterKey :: Newtype InventoryFilterKey _


newtype InventoryFilterList = InventoryFilterList (Array InventoryFilter)
derive instance newtypeInventoryFilterList :: Newtype InventoryFilterList _


newtype InventoryFilterValue = InventoryFilterValue String
derive instance newtypeInventoryFilterValue :: Newtype InventoryFilterValue _


newtype InventoryFilterValueList = InventoryFilterValueList (Array InventoryFilterValue)
derive instance newtypeInventoryFilterValueList :: Newtype InventoryFilterValueList _


-- | <p>Information collected from managed instances based on your inventory policy document</p>
newtype InventoryItem = InventoryItem 
  { "TypeName" :: (InventoryItemTypeName)
  , "SchemaVersion" :: (InventoryItemSchemaVersion)
  , "CaptureTime" :: (InventoryItemCaptureTime)
  , "ContentHash" :: NullOrUndefined (InventoryItemContentHash)
  , "Content" :: NullOrUndefined (InventoryItemEntryList)
  , "Context" :: NullOrUndefined (InventoryItemContentContext)
  }
derive instance newtypeInventoryItem :: Newtype InventoryItem _


-- | <p>Attributes are the entries within the inventory item content. It contains name and value.</p>
newtype InventoryItemAttribute = InventoryItemAttribute 
  { "Name" :: (InventoryItemAttributeName)
  , "DataType" :: (InventoryAttributeDataType)
  }
derive instance newtypeInventoryItemAttribute :: Newtype InventoryItemAttribute _


newtype InventoryItemAttributeList = InventoryItemAttributeList (Array InventoryItemAttribute)
derive instance newtypeInventoryItemAttributeList :: Newtype InventoryItemAttributeList _


newtype InventoryItemAttributeName = InventoryItemAttributeName String
derive instance newtypeInventoryItemAttributeName :: Newtype InventoryItemAttributeName _


newtype InventoryItemCaptureTime = InventoryItemCaptureTime String
derive instance newtypeInventoryItemCaptureTime :: Newtype InventoryItemCaptureTime _


newtype InventoryItemContentContext = InventoryItemContentContext (Map AttributeName AttributeValue)
derive instance newtypeInventoryItemContentContext :: Newtype InventoryItemContentContext _


newtype InventoryItemContentHash = InventoryItemContentHash String
derive instance newtypeInventoryItemContentHash :: Newtype InventoryItemContentHash _


newtype InventoryItemEntry = InventoryItemEntry (Map AttributeName AttributeValue)
derive instance newtypeInventoryItemEntry :: Newtype InventoryItemEntry _


newtype InventoryItemEntryList = InventoryItemEntryList (Array InventoryItemEntry)
derive instance newtypeInventoryItemEntryList :: Newtype InventoryItemEntryList _


newtype InventoryItemList = InventoryItemList (Array InventoryItem)
derive instance newtypeInventoryItemList :: Newtype InventoryItemList _


-- | <p>The inventory item schema definition. Users can use this to compose inventory query filters.</p>
newtype InventoryItemSchema = InventoryItemSchema 
  { "TypeName" :: (InventoryItemTypeName)
  , "Version" :: NullOrUndefined (InventoryItemSchemaVersion)
  , "Attributes" :: (InventoryItemAttributeList)
  , "DisplayName" :: NullOrUndefined (InventoryTypeDisplayName)
  }
derive instance newtypeInventoryItemSchema :: Newtype InventoryItemSchema _


newtype InventoryItemSchemaResultList = InventoryItemSchemaResultList (Array InventoryItemSchema)
derive instance newtypeInventoryItemSchemaResultList :: Newtype InventoryItemSchemaResultList _


newtype InventoryItemSchemaVersion = InventoryItemSchemaVersion String
derive instance newtypeInventoryItemSchemaVersion :: Newtype InventoryItemSchemaVersion _


newtype InventoryItemTypeName = InventoryItemTypeName String
derive instance newtypeInventoryItemTypeName :: Newtype InventoryItemTypeName _


newtype InventoryItemTypeNameFilter = InventoryItemTypeNameFilter String
derive instance newtypeInventoryItemTypeNameFilter :: Newtype InventoryItemTypeNameFilter _


newtype InventoryQueryOperatorType = InventoryQueryOperatorType String
derive instance newtypeInventoryQueryOperatorType :: Newtype InventoryQueryOperatorType _


-- | <p>Inventory query results.</p>
newtype InventoryResultEntity = InventoryResultEntity 
  { "Id" :: NullOrUndefined (InventoryResultEntityId)
  , "Data" :: NullOrUndefined (InventoryResultItemMap)
  }
derive instance newtypeInventoryResultEntity :: Newtype InventoryResultEntity _


newtype InventoryResultEntityId = InventoryResultEntityId String
derive instance newtypeInventoryResultEntityId :: Newtype InventoryResultEntityId _


newtype InventoryResultEntityList = InventoryResultEntityList (Array InventoryResultEntity)
derive instance newtypeInventoryResultEntityList :: Newtype InventoryResultEntityList _


-- | <p>The inventory result item.</p>
newtype InventoryResultItem = InventoryResultItem 
  { "TypeName" :: (InventoryItemTypeName)
  , "SchemaVersion" :: (InventoryItemSchemaVersion)
  , "CaptureTime" :: NullOrUndefined (InventoryItemCaptureTime)
  , "ContentHash" :: NullOrUndefined (InventoryItemContentHash)
  , "Content" :: (InventoryItemEntryList)
  }
derive instance newtypeInventoryResultItem :: Newtype InventoryResultItem _


newtype InventoryResultItemKey = InventoryResultItemKey String
derive instance newtypeInventoryResultItemKey :: Newtype InventoryResultItemKey _


newtype InventoryResultItemMap = InventoryResultItemMap (Map InventoryResultItemKey InventoryResultItem)
derive instance newtypeInventoryResultItemMap :: Newtype InventoryResultItemMap _


newtype InventoryTypeDisplayName = InventoryTypeDisplayName String
derive instance newtypeInventoryTypeDisplayName :: Newtype InventoryTypeDisplayName _


-- | <p>The command ID and instance ID you specified did not match any invocations. Verify the command ID adn the instance ID and try again. </p>
newtype InvocationDoesNotExist = InvocationDoesNotExist 
  { 
  }
derive instance newtypeInvocationDoesNotExist :: Newtype InvocationDoesNotExist _


newtype InvocationTraceOutput = InvocationTraceOutput String
derive instance newtypeInvocationTraceOutput :: Newtype InvocationTraceOutput _


newtype IsSubTypeSchema = IsSubTypeSchema Boolean
derive instance newtypeIsSubTypeSchema :: Newtype IsSubTypeSchema _


-- | <p>The inventory item has invalid content. </p>
newtype ItemContentMismatchException = ItemContentMismatchException 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeItemContentMismatchException :: Newtype ItemContentMismatchException _


-- | <p>The inventory item size has exceeded the size limit.</p>
newtype ItemSizeLimitExceededException = ItemSizeLimitExceededException 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeItemSizeLimitExceededException :: Newtype ItemSizeLimitExceededException _


newtype KeyList = KeyList (Array TagKey)
derive instance newtypeKeyList :: Newtype KeyList _


newtype LastResourceDataSyncStatus = LastResourceDataSyncStatus String
derive instance newtypeLastResourceDataSyncStatus :: Newtype LastResourceDataSyncStatus _


newtype LastResourceDataSyncTime = LastResourceDataSyncTime Number
derive instance newtypeLastResourceDataSyncTime :: Newtype LastResourceDataSyncTime _


newtype LastSuccessfulResourceDataSyncTime = LastSuccessfulResourceDataSyncTime Number
derive instance newtypeLastSuccessfulResourceDataSyncTime :: Newtype LastSuccessfulResourceDataSyncTime _


newtype ListAssociationVersionsRequest = ListAssociationVersionsRequest 
  { "AssociationId" :: (AssociationId)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListAssociationVersionsRequest :: Newtype ListAssociationVersionsRequest _


newtype ListAssociationVersionsResult = ListAssociationVersionsResult 
  { "AssociationVersions" :: NullOrUndefined (AssociationVersionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListAssociationVersionsResult :: Newtype ListAssociationVersionsResult _


newtype ListAssociationsRequest = ListAssociationsRequest 
  { "AssociationFilterList" :: NullOrUndefined (AssociationFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListAssociationsRequest :: Newtype ListAssociationsRequest _


newtype ListAssociationsResult = ListAssociationsResult 
  { "Associations" :: NullOrUndefined (AssociationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListAssociationsResult :: Newtype ListAssociationsResult _


newtype ListCommandInvocationsRequest = ListCommandInvocationsRequest 
  { "CommandId" :: NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "MaxResults" :: NullOrUndefined (CommandMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Filters" :: NullOrUndefined (CommandFilterList)
  , "Details" :: NullOrUndefined (Boolean)
  }
derive instance newtypeListCommandInvocationsRequest :: Newtype ListCommandInvocationsRequest _


newtype ListCommandInvocationsResult = ListCommandInvocationsResult 
  { "CommandInvocations" :: NullOrUndefined (CommandInvocationList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListCommandInvocationsResult :: Newtype ListCommandInvocationsResult _


newtype ListCommandsRequest = ListCommandsRequest 
  { "CommandId" :: NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "MaxResults" :: NullOrUndefined (CommandMaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Filters" :: NullOrUndefined (CommandFilterList)
  }
derive instance newtypeListCommandsRequest :: Newtype ListCommandsRequest _


newtype ListCommandsResult = ListCommandsResult 
  { "Commands" :: NullOrUndefined (CommandList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListCommandsResult :: Newtype ListCommandsResult _


newtype ListComplianceItemsRequest = ListComplianceItemsRequest 
  { "Filters" :: NullOrUndefined (ComplianceStringFilterList)
  , "ResourceIds" :: NullOrUndefined (ComplianceResourceIdList)
  , "ResourceTypes" :: NullOrUndefined (ComplianceResourceTypeList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListComplianceItemsRequest :: Newtype ListComplianceItemsRequest _


newtype ListComplianceItemsResult = ListComplianceItemsResult 
  { "ComplianceItems" :: NullOrUndefined (ComplianceItemList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListComplianceItemsResult :: Newtype ListComplianceItemsResult _


newtype ListComplianceSummariesRequest = ListComplianceSummariesRequest 
  { "Filters" :: NullOrUndefined (ComplianceStringFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListComplianceSummariesRequest :: Newtype ListComplianceSummariesRequest _


newtype ListComplianceSummariesResult = ListComplianceSummariesResult 
  { "ComplianceSummaryItems" :: NullOrUndefined (ComplianceSummaryItemList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListComplianceSummariesResult :: Newtype ListComplianceSummariesResult _


newtype ListDocumentVersionsRequest = ListDocumentVersionsRequest 
  { "Name" :: (DocumentName)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDocumentVersionsRequest :: Newtype ListDocumentVersionsRequest _


newtype ListDocumentVersionsResult = ListDocumentVersionsResult 
  { "DocumentVersions" :: NullOrUndefined (DocumentVersionList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDocumentVersionsResult :: Newtype ListDocumentVersionsResult _


newtype ListDocumentsRequest = ListDocumentsRequest 
  { "DocumentFilterList" :: NullOrUndefined (DocumentFilterList)
  , "Filters" :: NullOrUndefined (DocumentKeyValuesFilterList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDocumentsRequest :: Newtype ListDocumentsRequest _


newtype ListDocumentsResult = ListDocumentsResult 
  { "DocumentIdentifiers" :: NullOrUndefined (DocumentIdentifierList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDocumentsResult :: Newtype ListDocumentsResult _


newtype ListInventoryEntriesRequest = ListInventoryEntriesRequest 
  { "InstanceId" :: (InstanceId)
  , "TypeName" :: (InventoryItemTypeName)
  , "Filters" :: NullOrUndefined (InventoryFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListInventoryEntriesRequest :: Newtype ListInventoryEntriesRequest _


newtype ListInventoryEntriesResult = ListInventoryEntriesResult 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "InstanceId" :: NullOrUndefined (InstanceId)
  , "SchemaVersion" :: NullOrUndefined (InventoryItemSchemaVersion)
  , "CaptureTime" :: NullOrUndefined (InventoryItemCaptureTime)
  , "Entries" :: NullOrUndefined (InventoryItemEntryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListInventoryEntriesResult :: Newtype ListInventoryEntriesResult _


newtype ListResourceComplianceSummariesRequest = ListResourceComplianceSummariesRequest 
  { "Filters" :: NullOrUndefined (ComplianceStringFilterList)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListResourceComplianceSummariesRequest :: Newtype ListResourceComplianceSummariesRequest _


newtype ListResourceComplianceSummariesResult = ListResourceComplianceSummariesResult 
  { "ResourceComplianceSummaryItems" :: NullOrUndefined (ResourceComplianceSummaryItemList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListResourceComplianceSummariesResult :: Newtype ListResourceComplianceSummariesResult _


newtype ListResourceDataSyncRequest = ListResourceDataSyncRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListResourceDataSyncRequest :: Newtype ListResourceDataSyncRequest _


newtype ListResourceDataSyncResult = ListResourceDataSyncResult 
  { "ResourceDataSyncItems" :: NullOrUndefined (ResourceDataSyncItemList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListResourceDataSyncResult :: Newtype ListResourceDataSyncResult _


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceType" :: (ResourceTypeForTagging)
  , "ResourceId" :: (ResourceId)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _


newtype ListTagsForResourceResult = ListTagsForResourceResult 
  { "TagList" :: NullOrUndefined (TagList)
  }
derive instance newtypeListTagsForResourceResult :: Newtype ListTagsForResourceResult _


-- | <p>Information about an Amazon S3 bucket to write instance-level logs to.</p>
newtype LoggingInfo = LoggingInfo 
  { "S3BucketName" :: (S3BucketName)
  , "S3KeyPrefix" :: NullOrUndefined (S3KeyPrefix)
  , "S3Region" :: (S3Region)
  }
derive instance newtypeLoggingInfo :: Newtype LoggingInfo _


newtype MaintenanceWindowAllowUnassociatedTargets = MaintenanceWindowAllowUnassociatedTargets Boolean
derive instance newtypeMaintenanceWindowAllowUnassociatedTargets :: Newtype MaintenanceWindowAllowUnassociatedTargets _


-- | <p>The parameters for an AUTOMATION task type.</p>
newtype MaintenanceWindowAutomationParameters = MaintenanceWindowAutomationParameters 
  { "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "Parameters" :: NullOrUndefined (AutomationParameterMap)
  }
derive instance newtypeMaintenanceWindowAutomationParameters :: Newtype MaintenanceWindowAutomationParameters _


newtype MaintenanceWindowCutoff = MaintenanceWindowCutoff Int
derive instance newtypeMaintenanceWindowCutoff :: Newtype MaintenanceWindowCutoff _


newtype MaintenanceWindowDescription = MaintenanceWindowDescription String
derive instance newtypeMaintenanceWindowDescription :: Newtype MaintenanceWindowDescription _


newtype MaintenanceWindowDurationHours = MaintenanceWindowDurationHours Int
derive instance newtypeMaintenanceWindowDurationHours :: Newtype MaintenanceWindowDurationHours _


newtype MaintenanceWindowEnabled = MaintenanceWindowEnabled Boolean
derive instance newtypeMaintenanceWindowEnabled :: Newtype MaintenanceWindowEnabled _


-- | <p>Describes the information about an execution of a Maintenance Window. </p>
newtype MaintenanceWindowExecution = MaintenanceWindowExecution 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowExecutionId" :: NullOrUndefined (MaintenanceWindowExecutionId)
  , "Status" :: NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined (DateTime)
  }
derive instance newtypeMaintenanceWindowExecution :: Newtype MaintenanceWindowExecution _


newtype MaintenanceWindowExecutionId = MaintenanceWindowExecutionId String
derive instance newtypeMaintenanceWindowExecutionId :: Newtype MaintenanceWindowExecutionId _


newtype MaintenanceWindowExecutionList = MaintenanceWindowExecutionList (Array MaintenanceWindowExecution)
derive instance newtypeMaintenanceWindowExecutionList :: Newtype MaintenanceWindowExecutionList _


newtype MaintenanceWindowExecutionStatus = MaintenanceWindowExecutionStatus String
derive instance newtypeMaintenanceWindowExecutionStatus :: Newtype MaintenanceWindowExecutionStatus _


newtype MaintenanceWindowExecutionStatusDetails = MaintenanceWindowExecutionStatusDetails String
derive instance newtypeMaintenanceWindowExecutionStatusDetails :: Newtype MaintenanceWindowExecutionStatusDetails _


newtype MaintenanceWindowExecutionTaskExecutionId = MaintenanceWindowExecutionTaskExecutionId String
derive instance newtypeMaintenanceWindowExecutionTaskExecutionId :: Newtype MaintenanceWindowExecutionTaskExecutionId _


newtype MaintenanceWindowExecutionTaskId = MaintenanceWindowExecutionTaskId String
derive instance newtypeMaintenanceWindowExecutionTaskId :: Newtype MaintenanceWindowExecutionTaskId _


newtype MaintenanceWindowExecutionTaskIdList = MaintenanceWindowExecutionTaskIdList (Array MaintenanceWindowExecutionTaskId)
derive instance newtypeMaintenanceWindowExecutionTaskIdList :: Newtype MaintenanceWindowExecutionTaskIdList _


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
derive instance newtypeMaintenanceWindowExecutionTaskIdentity :: Newtype MaintenanceWindowExecutionTaskIdentity _


newtype MaintenanceWindowExecutionTaskIdentityList = MaintenanceWindowExecutionTaskIdentityList (Array MaintenanceWindowExecutionTaskIdentity)
derive instance newtypeMaintenanceWindowExecutionTaskIdentityList :: Newtype MaintenanceWindowExecutionTaskIdentityList _


newtype MaintenanceWindowExecutionTaskInvocationId = MaintenanceWindowExecutionTaskInvocationId String
derive instance newtypeMaintenanceWindowExecutionTaskInvocationId :: Newtype MaintenanceWindowExecutionTaskInvocationId _


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
derive instance newtypeMaintenanceWindowExecutionTaskInvocationIdentity :: Newtype MaintenanceWindowExecutionTaskInvocationIdentity _


newtype MaintenanceWindowExecutionTaskInvocationIdentityList = MaintenanceWindowExecutionTaskInvocationIdentityList (Array MaintenanceWindowExecutionTaskInvocationIdentity)
derive instance newtypeMaintenanceWindowExecutionTaskInvocationIdentityList :: Newtype MaintenanceWindowExecutionTaskInvocationIdentityList _


newtype MaintenanceWindowExecutionTaskInvocationParameters = MaintenanceWindowExecutionTaskInvocationParameters String
derive instance newtypeMaintenanceWindowExecutionTaskInvocationParameters :: Newtype MaintenanceWindowExecutionTaskInvocationParameters _


-- | <p>Filter used in the request.</p>
newtype MaintenanceWindowFilter = MaintenanceWindowFilter 
  { "Key" :: NullOrUndefined (MaintenanceWindowFilterKey)
  , "Values" :: NullOrUndefined (MaintenanceWindowFilterValues)
  }
derive instance newtypeMaintenanceWindowFilter :: Newtype MaintenanceWindowFilter _


newtype MaintenanceWindowFilterKey = MaintenanceWindowFilterKey String
derive instance newtypeMaintenanceWindowFilterKey :: Newtype MaintenanceWindowFilterKey _


newtype MaintenanceWindowFilterList = MaintenanceWindowFilterList (Array MaintenanceWindowFilter)
derive instance newtypeMaintenanceWindowFilterList :: Newtype MaintenanceWindowFilterList _


newtype MaintenanceWindowFilterValue = MaintenanceWindowFilterValue String
derive instance newtypeMaintenanceWindowFilterValue :: Newtype MaintenanceWindowFilterValue _


newtype MaintenanceWindowFilterValues = MaintenanceWindowFilterValues (Array MaintenanceWindowFilterValue)
derive instance newtypeMaintenanceWindowFilterValues :: Newtype MaintenanceWindowFilterValues _


newtype MaintenanceWindowId = MaintenanceWindowId String
derive instance newtypeMaintenanceWindowId :: Newtype MaintenanceWindowId _


-- | <p>Information about the Maintenance Window.</p>
newtype MaintenanceWindowIdentity = MaintenanceWindowIdentity 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Enabled" :: NullOrUndefined (MaintenanceWindowEnabled)
  , "Duration" :: NullOrUndefined (MaintenanceWindowDurationHours)
  , "Cutoff" :: NullOrUndefined (MaintenanceWindowCutoff)
  }
derive instance newtypeMaintenanceWindowIdentity :: Newtype MaintenanceWindowIdentity _


newtype MaintenanceWindowIdentityList = MaintenanceWindowIdentityList (Array MaintenanceWindowIdentity)
derive instance newtypeMaintenanceWindowIdentityList :: Newtype MaintenanceWindowIdentityList _


newtype MaintenanceWindowLambdaClientContext = MaintenanceWindowLambdaClientContext String
derive instance newtypeMaintenanceWindowLambdaClientContext :: Newtype MaintenanceWindowLambdaClientContext _


-- | <p>The parameters for a LAMBDA task type.</p>
newtype MaintenanceWindowLambdaParameters = MaintenanceWindowLambdaParameters 
  { "ClientContext" :: NullOrUndefined (MaintenanceWindowLambdaClientContext)
  , "Qualifier" :: NullOrUndefined (MaintenanceWindowLambdaQualifier)
  , "Payload" :: NullOrUndefined (MaintenanceWindowLambdaPayload)
  }
derive instance newtypeMaintenanceWindowLambdaParameters :: Newtype MaintenanceWindowLambdaParameters _


newtype MaintenanceWindowLambdaPayload = MaintenanceWindowLambdaPayload String
derive instance newtypeMaintenanceWindowLambdaPayload :: Newtype MaintenanceWindowLambdaPayload _


newtype MaintenanceWindowLambdaQualifier = MaintenanceWindowLambdaQualifier String
derive instance newtypeMaintenanceWindowLambdaQualifier :: Newtype MaintenanceWindowLambdaQualifier _


newtype MaintenanceWindowMaxResults = MaintenanceWindowMaxResults Int
derive instance newtypeMaintenanceWindowMaxResults :: Newtype MaintenanceWindowMaxResults _


newtype MaintenanceWindowName = MaintenanceWindowName String
derive instance newtypeMaintenanceWindowName :: Newtype MaintenanceWindowName _


newtype MaintenanceWindowResourceType = MaintenanceWindowResourceType String
derive instance newtypeMaintenanceWindowResourceType :: Newtype MaintenanceWindowResourceType _


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
derive instance newtypeMaintenanceWindowRunCommandParameters :: Newtype MaintenanceWindowRunCommandParameters _


newtype MaintenanceWindowSchedule = MaintenanceWindowSchedule String
derive instance newtypeMaintenanceWindowSchedule :: Newtype MaintenanceWindowSchedule _


newtype MaintenanceWindowStepFunctionsInput = MaintenanceWindowStepFunctionsInput String
derive instance newtypeMaintenanceWindowStepFunctionsInput :: Newtype MaintenanceWindowStepFunctionsInput _


newtype MaintenanceWindowStepFunctionsName = MaintenanceWindowStepFunctionsName String
derive instance newtypeMaintenanceWindowStepFunctionsName :: Newtype MaintenanceWindowStepFunctionsName _


-- | <p>The parameters for the STEP_FUNCTION execution.</p>
newtype MaintenanceWindowStepFunctionsParameters = MaintenanceWindowStepFunctionsParameters 
  { "Input" :: NullOrUndefined (MaintenanceWindowStepFunctionsInput)
  , "Name" :: NullOrUndefined (MaintenanceWindowStepFunctionsName)
  }
derive instance newtypeMaintenanceWindowStepFunctionsParameters :: Newtype MaintenanceWindowStepFunctionsParameters _


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
derive instance newtypeMaintenanceWindowTarget :: Newtype MaintenanceWindowTarget _


newtype MaintenanceWindowTargetId = MaintenanceWindowTargetId String
derive instance newtypeMaintenanceWindowTargetId :: Newtype MaintenanceWindowTargetId _


newtype MaintenanceWindowTargetList = MaintenanceWindowTargetList (Array MaintenanceWindowTarget)
derive instance newtypeMaintenanceWindowTargetList :: Newtype MaintenanceWindowTargetList _


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
derive instance newtypeMaintenanceWindowTask :: Newtype MaintenanceWindowTask _


newtype MaintenanceWindowTaskArn = MaintenanceWindowTaskArn String
derive instance newtypeMaintenanceWindowTaskArn :: Newtype MaintenanceWindowTaskArn _


newtype MaintenanceWindowTaskId = MaintenanceWindowTaskId String
derive instance newtypeMaintenanceWindowTaskId :: Newtype MaintenanceWindowTaskId _


-- | <p>The parameters for task execution.</p>
newtype MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters 
  { "RunCommand" :: NullOrUndefined (MaintenanceWindowRunCommandParameters)
  , "Automation" :: NullOrUndefined (MaintenanceWindowAutomationParameters)
  , "StepFunctions" :: NullOrUndefined (MaintenanceWindowStepFunctionsParameters)
  , "Lambda" :: NullOrUndefined (MaintenanceWindowLambdaParameters)
  }
derive instance newtypeMaintenanceWindowTaskInvocationParameters :: Newtype MaintenanceWindowTaskInvocationParameters _


newtype MaintenanceWindowTaskList = MaintenanceWindowTaskList (Array MaintenanceWindowTask)
derive instance newtypeMaintenanceWindowTaskList :: Newtype MaintenanceWindowTaskList _


newtype MaintenanceWindowTaskParameterName = MaintenanceWindowTaskParameterName String
derive instance newtypeMaintenanceWindowTaskParameterName :: Newtype MaintenanceWindowTaskParameterName _


newtype MaintenanceWindowTaskParameterValue = MaintenanceWindowTaskParameterValue String
derive instance newtypeMaintenanceWindowTaskParameterValue :: Newtype MaintenanceWindowTaskParameterValue _


-- | <p>Defines the values for a task parameter.</p>
newtype MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression 
  { "Values" :: NullOrUndefined (MaintenanceWindowTaskParameterValueList)
  }
derive instance newtypeMaintenanceWindowTaskParameterValueExpression :: Newtype MaintenanceWindowTaskParameterValueExpression _


newtype MaintenanceWindowTaskParameterValueList = MaintenanceWindowTaskParameterValueList (Array MaintenanceWindowTaskParameterValue)
derive instance newtypeMaintenanceWindowTaskParameterValueList :: Newtype MaintenanceWindowTaskParameterValueList _


newtype MaintenanceWindowTaskParameters = MaintenanceWindowTaskParameters (Map MaintenanceWindowTaskParameterName MaintenanceWindowTaskParameterValueExpression)
derive instance newtypeMaintenanceWindowTaskParameters :: Newtype MaintenanceWindowTaskParameters _


newtype MaintenanceWindowTaskParametersList = MaintenanceWindowTaskParametersList (Array MaintenanceWindowTaskParameters)
derive instance newtypeMaintenanceWindowTaskParametersList :: Newtype MaintenanceWindowTaskParametersList _


newtype MaintenanceWindowTaskPriority = MaintenanceWindowTaskPriority Int
derive instance newtypeMaintenanceWindowTaskPriority :: Newtype MaintenanceWindowTaskPriority _


newtype MaintenanceWindowTaskTargetId = MaintenanceWindowTaskTargetId String
derive instance newtypeMaintenanceWindowTaskTargetId :: Newtype MaintenanceWindowTaskTargetId _


newtype MaintenanceWindowTaskType = MaintenanceWindowTaskType String
derive instance newtypeMaintenanceWindowTaskType :: Newtype MaintenanceWindowTaskType _


newtype ManagedInstanceId = ManagedInstanceId String
derive instance newtypeManagedInstanceId :: Newtype ManagedInstanceId _


newtype MaxConcurrency = MaxConcurrency String
derive instance newtypeMaxConcurrency :: Newtype MaxConcurrency _


-- | <p>The size limit of a document is 64 KB.</p>
newtype MaxDocumentSizeExceeded = MaxDocumentSizeExceeded 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeMaxDocumentSizeExceeded :: Newtype MaxDocumentSizeExceeded _


newtype MaxErrors = MaxErrors String
derive instance newtypeMaxErrors :: Newtype MaxErrors _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype MaxResultsEC2Compatible = MaxResultsEC2Compatible Int
derive instance newtypeMaxResultsEC2Compatible :: Newtype MaxResultsEC2Compatible _


newtype ModifyDocumentPermissionRequest = ModifyDocumentPermissionRequest 
  { "Name" :: (DocumentName)
  , "PermissionType" :: (DocumentPermissionType)
  , "AccountIdsToAdd" :: NullOrUndefined (AccountIdList)
  , "AccountIdsToRemove" :: NullOrUndefined (AccountIdList)
  }
derive instance newtypeModifyDocumentPermissionRequest :: Newtype ModifyDocumentPermissionRequest _


newtype ModifyDocumentPermissionResponse = ModifyDocumentPermissionResponse 
  { 
  }
derive instance newtypeModifyDocumentPermissionResponse :: Newtype ModifyDocumentPermissionResponse _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>A summary of resources that are not compliant. The summary is organized according to resource type.</p>
newtype NonCompliantSummary = NonCompliantSummary 
  { "NonCompliantCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "SeveritySummary" :: NullOrUndefined (SeveritySummary)
  }
derive instance newtypeNonCompliantSummary :: Newtype NonCompliantSummary _


newtype NormalStringMap = NormalStringMap (Map String String)
derive instance newtypeNormalStringMap :: Newtype NormalStringMap _


newtype NotificationArn = NotificationArn String
derive instance newtypeNotificationArn :: Newtype NotificationArn _


-- | <p>Configurations for sending notifications.</p>
newtype NotificationConfig = NotificationConfig 
  { "NotificationArn" :: NullOrUndefined (NotificationArn)
  , "NotificationEvents" :: NullOrUndefined (NotificationEventList)
  , "NotificationType" :: NullOrUndefined (NotificationType)
  }
derive instance newtypeNotificationConfig :: Newtype NotificationConfig _


newtype NotificationEvent = NotificationEvent String
derive instance newtypeNotificationEvent :: Newtype NotificationEvent _


newtype NotificationEventList = NotificationEventList (Array NotificationEvent)
derive instance newtypeNotificationEventList :: Newtype NotificationEventList _


newtype NotificationType = NotificationType String
derive instance newtypeNotificationType :: Newtype NotificationType _


newtype OperatingSystem = OperatingSystem String
derive instance newtypeOperatingSystem :: Newtype OperatingSystem _


newtype OwnerInformation = OwnerInformation String
derive instance newtypeOwnerInformation :: Newtype OwnerInformation _


newtype PSParameterName = PSParameterName String
derive instance newtypePSParameterName :: Newtype PSParameterName _


newtype PSParameterValue = PSParameterValue String
derive instance newtypePSParameterValue :: Newtype PSParameterValue _


newtype PSParameterVersion = PSParameterVersion Number
derive instance newtypePSParameterVersion :: Newtype PSParameterVersion _


-- | <p>An Amazon EC2 Systems Manager parameter in Parameter Store.</p>
newtype Parameter = Parameter 
  { "Name" :: NullOrUndefined (PSParameterName)
  , "Type" :: NullOrUndefined (ParameterType)
  , "Value" :: NullOrUndefined (PSParameterValue)
  , "Version" :: NullOrUndefined (PSParameterVersion)
  }
derive instance newtypeParameter :: Newtype Parameter _


-- | <p>The parameter already exists. You can't create duplicate parameters.</p>
newtype ParameterAlreadyExists = ParameterAlreadyExists 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeParameterAlreadyExists :: Newtype ParameterAlreadyExists _


newtype ParameterDescription = ParameterDescription String
derive instance newtypeParameterDescription :: Newtype ParameterDescription _


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
derive instance newtypeParameterHistory :: Newtype ParameterHistory _


newtype ParameterHistoryList = ParameterHistoryList (Array ParameterHistory)
derive instance newtypeParameterHistoryList :: Newtype ParameterHistoryList _


newtype ParameterKeyId = ParameterKeyId String
derive instance newtypeParameterKeyId :: Newtype ParameterKeyId _


-- | <p>You have exceeded the number of parameters for this AWS account. Delete one or more parameters and try again.</p>
newtype ParameterLimitExceeded = ParameterLimitExceeded 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeParameterLimitExceeded :: Newtype ParameterLimitExceeded _


newtype ParameterList = ParameterList (Array Parameter)
derive instance newtypeParameterList :: Newtype ParameterList _


-- | <p>The parameter exceeded the maximum number of allowed versions.</p>
newtype ParameterMaxVersionLimitExceeded = ParameterMaxVersionLimitExceeded 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeParameterMaxVersionLimitExceeded :: Newtype ParameterMaxVersionLimitExceeded _


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
derive instance newtypeParameterMetadata :: Newtype ParameterMetadata _


newtype ParameterMetadataList = ParameterMetadataList (Array ParameterMetadata)
derive instance newtypeParameterMetadataList :: Newtype ParameterMetadataList _


newtype ParameterName = ParameterName String
derive instance newtypeParameterName :: Newtype ParameterName _


newtype ParameterNameList = ParameterNameList (Array PSParameterName)
derive instance newtypeParameterNameList :: Newtype ParameterNameList _


-- | <p>The parameter could not be found. Verify the name and try again.</p>
newtype ParameterNotFound = ParameterNotFound 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeParameterNotFound :: Newtype ParameterNotFound _


-- | <p>The parameter name is not valid.</p>
newtype ParameterPatternMismatchException = ParameterPatternMismatchException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeParameterPatternMismatchException :: Newtype ParameterPatternMismatchException _


-- | <p>One or more filters. Use a filter to return a more specific list of results.</p>
newtype ParameterStringFilter = ParameterStringFilter 
  { "Key" :: (ParameterStringFilterKey)
  , "Option" :: NullOrUndefined (ParameterStringQueryOption)
  , "Values" :: NullOrUndefined (ParameterStringFilterValueList)
  }
derive instance newtypeParameterStringFilter :: Newtype ParameterStringFilter _


newtype ParameterStringFilterKey = ParameterStringFilterKey String
derive instance newtypeParameterStringFilterKey :: Newtype ParameterStringFilterKey _


newtype ParameterStringFilterList = ParameterStringFilterList (Array ParameterStringFilter)
derive instance newtypeParameterStringFilterList :: Newtype ParameterStringFilterList _


newtype ParameterStringFilterValue = ParameterStringFilterValue String
derive instance newtypeParameterStringFilterValue :: Newtype ParameterStringFilterValue _


newtype ParameterStringFilterValueList = ParameterStringFilterValueList (Array ParameterStringFilterValue)
derive instance newtypeParameterStringFilterValueList :: Newtype ParameterStringFilterValueList _


newtype ParameterStringQueryOption = ParameterStringQueryOption String
derive instance newtypeParameterStringQueryOption :: Newtype ParameterStringQueryOption _


newtype ParameterType = ParameterType String
derive instance newtypeParameterType :: Newtype ParameterType _


newtype ParameterValue = ParameterValue String
derive instance newtypeParameterValue :: Newtype ParameterValue _


newtype ParameterValueList = ParameterValueList (Array ParameterValue)
derive instance newtypeParameterValueList :: Newtype ParameterValueList _


-- | <p>The specified parameter version was not found. Verify the parameter name and version, and try again.</p>
newtype ParameterVersionNotFound = ParameterVersionNotFound 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeParameterVersionNotFound :: Newtype ParameterVersionNotFound _


newtype Parameters = Parameters (Map ParameterName ParameterValueList)
derive instance newtypeParameters :: Newtype Parameters _


-- | <p>This data type is deprecated. Instead, use <a>ParameterStringFilter</a>.</p>
newtype ParametersFilter = ParametersFilter 
  { "Key" :: (ParametersFilterKey)
  , "Values" :: (ParametersFilterValueList)
  }
derive instance newtypeParametersFilter :: Newtype ParametersFilter _


newtype ParametersFilterKey = ParametersFilterKey String
derive instance newtypeParametersFilterKey :: Newtype ParametersFilterKey _


newtype ParametersFilterList = ParametersFilterList (Array ParametersFilter)
derive instance newtypeParametersFilterList :: Newtype ParametersFilterList _


newtype ParametersFilterValue = ParametersFilterValue String
derive instance newtypeParametersFilterValue :: Newtype ParametersFilterValue _


newtype ParametersFilterValueList = ParametersFilterValueList (Array ParametersFilterValue)
derive instance newtypeParametersFilterValueList :: Newtype ParametersFilterValueList _


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
derive instance newtypePatch :: Newtype Patch _


-- | <p>Defines the basic information about a patch baseline.</p>
newtype PatchBaselineIdentity = PatchBaselineIdentity 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "BaselineName" :: NullOrUndefined (BaselineName)
  , "OperatingSystem" :: NullOrUndefined (OperatingSystem)
  , "BaselineDescription" :: NullOrUndefined (BaselineDescription)
  , "DefaultBaseline" :: NullOrUndefined (DefaultBaseline)
  }
derive instance newtypePatchBaselineIdentity :: Newtype PatchBaselineIdentity _


newtype PatchBaselineIdentityList = PatchBaselineIdentityList (Array PatchBaselineIdentity)
derive instance newtypePatchBaselineIdentityList :: Newtype PatchBaselineIdentityList _


newtype PatchBaselineMaxResults = PatchBaselineMaxResults Int
derive instance newtypePatchBaselineMaxResults :: Newtype PatchBaselineMaxResults _


newtype PatchClassification = PatchClassification String
derive instance newtypePatchClassification :: Newtype PatchClassification _


-- | <p>Information about the state of a patch on a particular instance as it relates to the patch baseline used to patch the instance.</p>
newtype PatchComplianceData = PatchComplianceData 
  { "Title" :: (PatchTitle)
  , "KBId" :: (PatchKbNumber)
  , "Classification" :: (PatchClassification)
  , "Severity" :: (PatchSeverity)
  , "State" :: (PatchComplianceDataState)
  , "InstalledTime" :: (DateTime)
  }
derive instance newtypePatchComplianceData :: Newtype PatchComplianceData _


newtype PatchComplianceDataList = PatchComplianceDataList (Array PatchComplianceData)
derive instance newtypePatchComplianceDataList :: Newtype PatchComplianceDataList _


newtype PatchComplianceDataState = PatchComplianceDataState String
derive instance newtypePatchComplianceDataState :: Newtype PatchComplianceDataState _


newtype PatchComplianceLevel = PatchComplianceLevel String
derive instance newtypePatchComplianceLevel :: Newtype PatchComplianceLevel _


newtype PatchComplianceMaxResults = PatchComplianceMaxResults Int
derive instance newtypePatchComplianceMaxResults :: Newtype PatchComplianceMaxResults _


newtype PatchContentUrl = PatchContentUrl String
derive instance newtypePatchContentUrl :: Newtype PatchContentUrl _


newtype PatchDeploymentStatus = PatchDeploymentStatus String
derive instance newtypePatchDeploymentStatus :: Newtype PatchDeploymentStatus _


newtype PatchDescription = PatchDescription String
derive instance newtypePatchDescription :: Newtype PatchDescription _


newtype PatchFailedCount = PatchFailedCount Int
derive instance newtypePatchFailedCount :: Newtype PatchFailedCount _


-- | <p>Defines a patch filter.</p> <p>A patch filter consists of key/value pairs, but not all keys are valid for all operating system types. For example, the key <code>PRODUCT</code> is valid for all supported operating system types. The key <code>MSRC_SEVERITY</code>, however, is valid only for Windows operating systems, and the key <code>SECTION</code> is valid only for Ubuntu operating systems.</p> <p>Refer to the following sections for information about which keys may be used with each major operating system, and which values are valid for each key.</p> <p> <b>Windows Operating Systems</b> </p> <p>The supported keys for Windows operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>MSRC_SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Windows7</code> </p> </li> <li> <p> <code>Windows8</code> </p> </li> <li> <p> <code>Windows8.1</code> </p> </li> <li> <p> <code>Windows8Embedded</code> </p> </li> <li> <p> <code>Windows10</code> </p> </li> <li> <p> <code>Windows10LTSB</code> </p> </li> <li> <p> <code>WindowsServer2008</code> </p> </li> <li> <p> <code>WindowsServer2008R2</code> </p> </li> <li> <p> <code>WindowsServer2012</code> </p> </li> <li> <p> <code>WindowsServer2012R2</code> </p> </li> <li> <p> <code>WindowsServer2016</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>CriticalUpdates</code> </p> </li> <li> <p> <code>DefinitionUpdates</code> </p> </li> <li> <p> <code>Drivers</code> </p> </li> <li> <p> <code>FeaturePacks</code> </p> </li> <li> <p> <code>SecurityUpdates</code> </p> </li> <li> <p> <code>ServicePacks</code> </p> </li> <li> <p> <code>Tools</code> </p> </li> <li> <p> <code>UpdateRollups</code> </p> </li> <li> <p> <code>Updates</code> </p> </li> <li> <p> <code>Upgrades</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>MSRC_SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> <li> <p> <code>Unspecified</code> </p> </li> </ul> <p> <b>Ubuntu Operating Systems</b> </p> <p>The supported keys for Ubuntu operating systems are <code>PRODUCT</code>, <code>PRIORITY</code>, and <code>SECTION</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Ubuntu14.04</code> </p> </li> <li> <p> <code>Ubuntu16.04</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>PRIORITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Required</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Standard</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Extra</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SECTION</code> </p> <p>Only the length of the key value is validated. Minimum length is 1. Maximum length is 64.</p> <p> <b>Amazon Linux Operating Systems</b> </p> <p>The supported keys for Amazon Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>AmazonLinux2012.03</code> </p> </li> <li> <p> <code>AmazonLinux2012.09</code> </p> </li> <li> <p> <code>AmazonLinux2013.03</code> </p> </li> <li> <p> <code>AmazonLinux2013.09</code> </p> </li> <li> <p> <code>AmazonLinux2014.03</code> </p> </li> <li> <p> <code>AmazonLinux2014.09</code> </p> </li> <li> <p> <code>AmazonLinux2015.03</code> </p> </li> <li> <p> <code>AmazonLinux2015.09</code> </p> </li> <li> <p> <code>AmazonLinux2016.03</code> </p> </li> <li> <p> <code>AmazonLinux2016.09</code> </p> </li> <li> <p> <code>AmazonLinux2017.03</code> </p> </li> <li> <p> <code>AmazonLinux2017.09</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>RedHat Enterprise Linux (RHEL) Operating Systems</b> </p> <p>The supported keys for RedHat Enterprise Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>RedhatEnterpriseLinux6.5</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.6</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.7</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.8</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.9</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.0</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.1</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.2</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.3</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.4</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>SUSE Linux Enterprise Server (SUSE) Operating Systems</b> </p> <p>The supported keys for SUSE operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Suse12.0</code> </p> </li> <li> <p> <code>Suse12.1</code> </p> </li> <li> <p> <code>Suse12.2</code> </p> </li> <li> <p> <code>Suse12.3</code> </p> </li> <li> <p> <code>Suse12.4</code> </p> </li> <li> <p> <code>Suse12.5</code> </p> </li> <li> <p> <code>Suse12.6</code> </p> </li> <li> <p> <code>Suse12.7</code> </p> </li> <li> <p> <code>Suse12.8</code> </p> </li> <li> <p> <code>Suse12.9</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Feature</code> </p> </li> <li> <p> <code>Document</code> </p> </li> <li> <p> <code>Yast</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul>
newtype PatchFilter = PatchFilter 
  { "Key" :: (PatchFilterKey)
  , "Values" :: (PatchFilterValueList)
  }
derive instance newtypePatchFilter :: Newtype PatchFilter _


-- | <p>A set of patch filters, typically used for approval rules.</p>
newtype PatchFilterGroup = PatchFilterGroup 
  { "PatchFilters" :: (PatchFilterList)
  }
derive instance newtypePatchFilterGroup :: Newtype PatchFilterGroup _


newtype PatchFilterKey = PatchFilterKey String
derive instance newtypePatchFilterKey :: Newtype PatchFilterKey _


newtype PatchFilterList = PatchFilterList (Array PatchFilter)
derive instance newtypePatchFilterList :: Newtype PatchFilterList _


newtype PatchFilterValue = PatchFilterValue String
derive instance newtypePatchFilterValue :: Newtype PatchFilterValue _


newtype PatchFilterValueList = PatchFilterValueList (Array PatchFilterValue)
derive instance newtypePatchFilterValueList :: Newtype PatchFilterValueList _


newtype PatchGroup = PatchGroup String
derive instance newtypePatchGroup :: Newtype PatchGroup _


newtype PatchGroupList = PatchGroupList (Array PatchGroup)
derive instance newtypePatchGroupList :: Newtype PatchGroupList _


-- | <p>The mapping between a patch group and the patch baseline the patch group is registered with.</p>
newtype PatchGroupPatchBaselineMapping = PatchGroupPatchBaselineMapping 
  { "PatchGroup" :: NullOrUndefined (PatchGroup)
  , "BaselineIdentity" :: NullOrUndefined (PatchBaselineIdentity)
  }
derive instance newtypePatchGroupPatchBaselineMapping :: Newtype PatchGroupPatchBaselineMapping _


newtype PatchGroupPatchBaselineMappingList = PatchGroupPatchBaselineMappingList (Array PatchGroupPatchBaselineMapping)
derive instance newtypePatchGroupPatchBaselineMappingList :: Newtype PatchGroupPatchBaselineMappingList _


newtype PatchId = PatchId String
derive instance newtypePatchId :: Newtype PatchId _


newtype PatchIdList = PatchIdList (Array PatchId)
derive instance newtypePatchIdList :: Newtype PatchIdList _


newtype PatchInstalledCount = PatchInstalledCount Int
derive instance newtypePatchInstalledCount :: Newtype PatchInstalledCount _


newtype PatchInstalledOtherCount = PatchInstalledOtherCount Int
derive instance newtypePatchInstalledOtherCount :: Newtype PatchInstalledOtherCount _


newtype PatchKbNumber = PatchKbNumber String
derive instance newtypePatchKbNumber :: Newtype PatchKbNumber _


newtype PatchLanguage = PatchLanguage String
derive instance newtypePatchLanguage :: Newtype PatchLanguage _


newtype PatchList = PatchList (Array Patch)
derive instance newtypePatchList :: Newtype PatchList _


newtype PatchMissingCount = PatchMissingCount Int
derive instance newtypePatchMissingCount :: Newtype PatchMissingCount _


newtype PatchMsrcNumber = PatchMsrcNumber String
derive instance newtypePatchMsrcNumber :: Newtype PatchMsrcNumber _


newtype PatchMsrcSeverity = PatchMsrcSeverity String
derive instance newtypePatchMsrcSeverity :: Newtype PatchMsrcSeverity _


newtype PatchNotApplicableCount = PatchNotApplicableCount Int
derive instance newtypePatchNotApplicableCount :: Newtype PatchNotApplicableCount _


newtype PatchOperationType = PatchOperationType String
derive instance newtypePatchOperationType :: Newtype PatchOperationType _


-- | <p>Defines a filter used in Patch Manager APIs.</p>
newtype PatchOrchestratorFilter = PatchOrchestratorFilter 
  { "Key" :: NullOrUndefined (PatchOrchestratorFilterKey)
  , "Values" :: NullOrUndefined (PatchOrchestratorFilterValues)
  }
derive instance newtypePatchOrchestratorFilter :: Newtype PatchOrchestratorFilter _


newtype PatchOrchestratorFilterKey = PatchOrchestratorFilterKey String
derive instance newtypePatchOrchestratorFilterKey :: Newtype PatchOrchestratorFilterKey _


newtype PatchOrchestratorFilterList = PatchOrchestratorFilterList (Array PatchOrchestratorFilter)
derive instance newtypePatchOrchestratorFilterList :: Newtype PatchOrchestratorFilterList _


newtype PatchOrchestratorFilterValue = PatchOrchestratorFilterValue String
derive instance newtypePatchOrchestratorFilterValue :: Newtype PatchOrchestratorFilterValue _


newtype PatchOrchestratorFilterValues = PatchOrchestratorFilterValues (Array PatchOrchestratorFilterValue)
derive instance newtypePatchOrchestratorFilterValues :: Newtype PatchOrchestratorFilterValues _


newtype PatchProduct = PatchProduct String
derive instance newtypePatchProduct :: Newtype PatchProduct _


newtype PatchProductFamily = PatchProductFamily String
derive instance newtypePatchProductFamily :: Newtype PatchProductFamily _


-- | <p>Defines an approval rule for a patch baseline.</p>
newtype PatchRule = PatchRule 
  { "PatchFilterGroup" :: (PatchFilterGroup)
  , "ComplianceLevel" :: NullOrUndefined (PatchComplianceLevel)
  , "ApproveAfterDays" :: (ApproveAfterDays)
  , "EnableNonSecurity" :: NullOrUndefined (Boolean)
  }
derive instance newtypePatchRule :: Newtype PatchRule _


-- | <p>A set of rules defining the approval rules for a patch baseline.</p>
newtype PatchRuleGroup = PatchRuleGroup 
  { "PatchRules" :: (PatchRuleList)
  }
derive instance newtypePatchRuleGroup :: Newtype PatchRuleGroup _


newtype PatchRuleList = PatchRuleList (Array PatchRule)
derive instance newtypePatchRuleList :: Newtype PatchRuleList _


newtype PatchSeverity = PatchSeverity String
derive instance newtypePatchSeverity :: Newtype PatchSeverity _


-- | <p>Information about the patches to use to update the instances, including target operating systems and source repository. Applies to Linux instances only.</p>
newtype PatchSource = PatchSource 
  { "Name" :: (PatchSourceName)
  , "Products" :: (PatchSourceProductList)
  , "Configuration" :: (PatchSourceConfiguration)
  }
derive instance newtypePatchSource :: Newtype PatchSource _


newtype PatchSourceConfiguration = PatchSourceConfiguration String
derive instance newtypePatchSourceConfiguration :: Newtype PatchSourceConfiguration _


newtype PatchSourceList = PatchSourceList (Array PatchSource)
derive instance newtypePatchSourceList :: Newtype PatchSourceList _


newtype PatchSourceName = PatchSourceName String
derive instance newtypePatchSourceName :: Newtype PatchSourceName _


newtype PatchSourceProduct = PatchSourceProduct String
derive instance newtypePatchSourceProduct :: Newtype PatchSourceProduct _


newtype PatchSourceProductList = PatchSourceProductList (Array PatchSourceProduct)
derive instance newtypePatchSourceProductList :: Newtype PatchSourceProductList _


-- | <p>Information about the approval status of a patch.</p>
newtype PatchStatus = PatchStatus 
  { "DeploymentStatus" :: NullOrUndefined (PatchDeploymentStatus)
  , "ComplianceLevel" :: NullOrUndefined (PatchComplianceLevel)
  , "ApprovalDate" :: NullOrUndefined (DateTime)
  }
derive instance newtypePatchStatus :: Newtype PatchStatus _


newtype PatchTitle = PatchTitle String
derive instance newtypePatchTitle :: Newtype PatchTitle _


newtype PatchVendor = PatchVendor String
derive instance newtypePatchVendor :: Newtype PatchVendor _


newtype PingStatus = PingStatus String
derive instance newtypePingStatus :: Newtype PingStatus _


newtype PlatformType = PlatformType String
derive instance newtypePlatformType :: Newtype PlatformType _


newtype PlatformTypeList = PlatformTypeList (Array PlatformType)
derive instance newtypePlatformTypeList :: Newtype PlatformTypeList _


newtype Product = Product String
derive instance newtypeProduct :: Newtype Product _


newtype PutComplianceItemsRequest = PutComplianceItemsRequest 
  { "ResourceId" :: (ComplianceResourceId)
  , "ResourceType" :: (ComplianceResourceType)
  , "ComplianceType" :: (ComplianceTypeName)
  , "ExecutionSummary" :: (ComplianceExecutionSummary)
  , "Items" :: (ComplianceItemEntryList)
  , "ItemContentHash" :: NullOrUndefined (ComplianceItemContentHash)
  }
derive instance newtypePutComplianceItemsRequest :: Newtype PutComplianceItemsRequest _


newtype PutComplianceItemsResult = PutComplianceItemsResult 
  { 
  }
derive instance newtypePutComplianceItemsResult :: Newtype PutComplianceItemsResult _


newtype PutInventoryRequest = PutInventoryRequest 
  { "InstanceId" :: (InstanceId)
  , "Items" :: (InventoryItemList)
  }
derive instance newtypePutInventoryRequest :: Newtype PutInventoryRequest _


newtype PutInventoryResult = PutInventoryResult 
  { 
  }
derive instance newtypePutInventoryResult :: Newtype PutInventoryResult _


newtype PutParameterRequest = PutParameterRequest 
  { "Name" :: (PSParameterName)
  , "Description" :: NullOrUndefined (ParameterDescription)
  , "Value" :: (PSParameterValue)
  , "Type" :: (ParameterType)
  , "KeyId" :: NullOrUndefined (ParameterKeyId)
  , "Overwrite" :: NullOrUndefined (Boolean)
  , "AllowedPattern" :: NullOrUndefined (AllowedPattern)
  }
derive instance newtypePutParameterRequest :: Newtype PutParameterRequest _


newtype PutParameterResult = PutParameterResult 
  { "Version" :: NullOrUndefined (PSParameterVersion)
  }
derive instance newtypePutParameterResult :: Newtype PutParameterResult _


newtype RegisterDefaultPatchBaselineRequest = RegisterDefaultPatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  }
derive instance newtypeRegisterDefaultPatchBaselineRequest :: Newtype RegisterDefaultPatchBaselineRequest _


newtype RegisterDefaultPatchBaselineResult = RegisterDefaultPatchBaselineResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  }
derive instance newtypeRegisterDefaultPatchBaselineResult :: Newtype RegisterDefaultPatchBaselineResult _


newtype RegisterPatchBaselineForPatchGroupRequest = RegisterPatchBaselineForPatchGroupRequest 
  { "BaselineId" :: (BaselineId)
  , "PatchGroup" :: (PatchGroup)
  }
derive instance newtypeRegisterPatchBaselineForPatchGroupRequest :: Newtype RegisterPatchBaselineForPatchGroupRequest _


newtype RegisterPatchBaselineForPatchGroupResult = RegisterPatchBaselineForPatchGroupResult 
  { "BaselineId" :: NullOrUndefined (BaselineId)
  , "PatchGroup" :: NullOrUndefined (PatchGroup)
  }
derive instance newtypeRegisterPatchBaselineForPatchGroupResult :: Newtype RegisterPatchBaselineForPatchGroupResult _


newtype RegisterTargetWithMaintenanceWindowRequest = RegisterTargetWithMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "ResourceType" :: (MaintenanceWindowResourceType)
  , "Targets" :: (Targets)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "ClientToken" :: NullOrUndefined (ClientToken)
  }
derive instance newtypeRegisterTargetWithMaintenanceWindowRequest :: Newtype RegisterTargetWithMaintenanceWindowRequest _


newtype RegisterTargetWithMaintenanceWindowResult = RegisterTargetWithMaintenanceWindowResult 
  { "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId)
  }
derive instance newtypeRegisterTargetWithMaintenanceWindowResult :: Newtype RegisterTargetWithMaintenanceWindowResult _


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
derive instance newtypeRegisterTaskWithMaintenanceWindowRequest :: Newtype RegisterTaskWithMaintenanceWindowRequest _


newtype RegisterTaskWithMaintenanceWindowResult = RegisterTaskWithMaintenanceWindowResult 
  { "WindowTaskId" :: NullOrUndefined (MaintenanceWindowTaskId)
  }
derive instance newtypeRegisterTaskWithMaintenanceWindowResult :: Newtype RegisterTaskWithMaintenanceWindowResult _


newtype RegistrationLimit = RegistrationLimit Int
derive instance newtypeRegistrationLimit :: Newtype RegistrationLimit _


newtype RegistrationsCount = RegistrationsCount Int
derive instance newtypeRegistrationsCount :: Newtype RegistrationsCount _


newtype RemoveTagsFromResourceRequest = RemoveTagsFromResourceRequest 
  { "ResourceType" :: (ResourceTypeForTagging)
  , "ResourceId" :: (ResourceId)
  , "TagKeys" :: (KeyList)
  }
derive instance newtypeRemoveTagsFromResourceRequest :: Newtype RemoveTagsFromResourceRequest _


newtype RemoveTagsFromResourceResult = RemoveTagsFromResourceResult 
  { 
  }
derive instance newtypeRemoveTagsFromResourceResult :: Newtype RemoveTagsFromResourceResult _


-- | <p>Information about targets that resolved during the Automation execution.</p>
newtype ResolvedTargets = ResolvedTargets 
  { "ParameterValues" :: NullOrUndefined (TargetParameterList)
  , "Truncated" :: NullOrUndefined (Boolean)
  }
derive instance newtypeResolvedTargets :: Newtype ResolvedTargets _


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
derive instance newtypeResourceComplianceSummaryItem :: Newtype ResourceComplianceSummaryItem _


newtype ResourceComplianceSummaryItemList = ResourceComplianceSummaryItemList (Array ResourceComplianceSummaryItem)
derive instance newtypeResourceComplianceSummaryItemList :: Newtype ResourceComplianceSummaryItemList _


newtype ResourceDataSyncAWSKMSKeyARN = ResourceDataSyncAWSKMSKeyARN String
derive instance newtypeResourceDataSyncAWSKMSKeyARN :: Newtype ResourceDataSyncAWSKMSKeyARN _


-- | <p>A sync configuration with the same name already exists.</p>
newtype ResourceDataSyncAlreadyExistsException = ResourceDataSyncAlreadyExistsException 
  { "SyncName" :: NullOrUndefined (ResourceDataSyncName)
  }
derive instance newtypeResourceDataSyncAlreadyExistsException :: Newtype ResourceDataSyncAlreadyExistsException _


-- | <p>You have exceeded the allowed maximum sync configurations.</p>
newtype ResourceDataSyncCountExceededException = ResourceDataSyncCountExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeResourceDataSyncCountExceededException :: Newtype ResourceDataSyncCountExceededException _


newtype ResourceDataSyncCreatedTime = ResourceDataSyncCreatedTime Number
derive instance newtypeResourceDataSyncCreatedTime :: Newtype ResourceDataSyncCreatedTime _


-- | <p>The specified sync configuration is invalid.</p>
newtype ResourceDataSyncInvalidConfigurationException = ResourceDataSyncInvalidConfigurationException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeResourceDataSyncInvalidConfigurationException :: Newtype ResourceDataSyncInvalidConfigurationException _


-- | <p>Information about a Resource Data Sync configuration, including its current status and last successful sync.</p>
newtype ResourceDataSyncItem = ResourceDataSyncItem 
  { "SyncName" :: NullOrUndefined (ResourceDataSyncName)
  , "S3Destination" :: NullOrUndefined (ResourceDataSyncS3Destination)
  , "LastSyncTime" :: NullOrUndefined (LastResourceDataSyncTime)
  , "LastSuccessfulSyncTime" :: NullOrUndefined (LastSuccessfulResourceDataSyncTime)
  , "LastStatus" :: NullOrUndefined (LastResourceDataSyncStatus)
  , "SyncCreatedTime" :: NullOrUndefined (ResourceDataSyncCreatedTime)
  }
derive instance newtypeResourceDataSyncItem :: Newtype ResourceDataSyncItem _


newtype ResourceDataSyncItemList = ResourceDataSyncItemList (Array ResourceDataSyncItem)
derive instance newtypeResourceDataSyncItemList :: Newtype ResourceDataSyncItemList _


newtype ResourceDataSyncName = ResourceDataSyncName String
derive instance newtypeResourceDataSyncName :: Newtype ResourceDataSyncName _


-- | <p>The specified sync name was not found.</p>
newtype ResourceDataSyncNotFoundException = ResourceDataSyncNotFoundException 
  { "SyncName" :: NullOrUndefined (ResourceDataSyncName)
  }
derive instance newtypeResourceDataSyncNotFoundException :: Newtype ResourceDataSyncNotFoundException _


newtype ResourceDataSyncS3BucketName = ResourceDataSyncS3BucketName String
derive instance newtypeResourceDataSyncS3BucketName :: Newtype ResourceDataSyncS3BucketName _


-- | <p>Information about the target Amazon S3 bucket for the Resource Data Sync.</p>
newtype ResourceDataSyncS3Destination = ResourceDataSyncS3Destination 
  { "BucketName" :: (ResourceDataSyncS3BucketName)
  , "Prefix" :: NullOrUndefined (ResourceDataSyncS3Prefix)
  , "SyncFormat" :: (ResourceDataSyncS3Format)
  , "Region" :: (ResourceDataSyncS3Region)
  , "AWSKMSKeyARN" :: NullOrUndefined (ResourceDataSyncAWSKMSKeyARN)
  }
derive instance newtypeResourceDataSyncS3Destination :: Newtype ResourceDataSyncS3Destination _


newtype ResourceDataSyncS3Format = ResourceDataSyncS3Format String
derive instance newtypeResourceDataSyncS3Format :: Newtype ResourceDataSyncS3Format _


newtype ResourceDataSyncS3Prefix = ResourceDataSyncS3Prefix String
derive instance newtypeResourceDataSyncS3Prefix :: Newtype ResourceDataSyncS3Prefix _


newtype ResourceDataSyncS3Region = ResourceDataSyncS3Region String
derive instance newtypeResourceDataSyncS3Region :: Newtype ResourceDataSyncS3Region _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


-- | <p>Error returned if an attempt is made to delete a patch baseline that is registered for a patch group.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>Error returned when the caller has exceeded the default resource limits. For example, too many Maintenance Windows or Patch baselines have been created.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>
newtype ResourceLimitExceededException = ResourceLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeResourceLimitExceededException :: Newtype ResourceLimitExceededException _


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _


newtype ResourceTypeForTagging = ResourceTypeForTagging String
derive instance newtypeResourceTypeForTagging :: Newtype ResourceTypeForTagging _


newtype ResponseCode = ResponseCode Int
derive instance newtypeResponseCode :: Newtype ResponseCode _


-- | <p>The inventory item result attribute.</p>
newtype ResultAttribute = ResultAttribute 
  { "TypeName" :: (InventoryItemTypeName)
  }
derive instance newtypeResultAttribute :: Newtype ResultAttribute _


newtype ResultAttributeList = ResultAttributeList (Array ResultAttribute)
derive instance newtypeResultAttributeList :: Newtype ResultAttributeList _


newtype S3BucketName = S3BucketName String
derive instance newtypeS3BucketName :: Newtype S3BucketName _


newtype S3KeyPrefix = S3KeyPrefix String
derive instance newtypeS3KeyPrefix :: Newtype S3KeyPrefix _


-- | <p>An Amazon S3 bucket where you want to store the results of this request.</p>
newtype S3OutputLocation = S3OutputLocation 
  { "OutputS3Region" :: NullOrUndefined (S3Region)
  , "OutputS3BucketName" :: NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined (S3KeyPrefix)
  }
derive instance newtypeS3OutputLocation :: Newtype S3OutputLocation _


-- | <p>A URL for the Amazon S3 bucket where you want to store the results of this request.</p>
newtype S3OutputUrl = S3OutputUrl 
  { "OutputUrl" :: NullOrUndefined (Url)
  }
derive instance newtypeS3OutputUrl :: Newtype S3OutputUrl _


newtype S3Region = S3Region String
derive instance newtypeS3Region :: Newtype S3Region _


newtype ScheduleExpression = ScheduleExpression String
derive instance newtypeScheduleExpression :: Newtype ScheduleExpression _


newtype SendAutomationSignalRequest = SendAutomationSignalRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  , "SignalType" :: (SignalType)
  , "Payload" :: NullOrUndefined (AutomationParameterMap)
  }
derive instance newtypeSendAutomationSignalRequest :: Newtype SendAutomationSignalRequest _


newtype SendAutomationSignalResult = SendAutomationSignalResult 
  { 
  }
derive instance newtypeSendAutomationSignalResult :: Newtype SendAutomationSignalResult _


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
derive instance newtypeSendCommandRequest :: Newtype SendCommandRequest _


newtype SendCommandResult = SendCommandResult 
  { "Command" :: NullOrUndefined (Command)
  }
derive instance newtypeSendCommandResult :: Newtype SendCommandResult _


newtype ServiceRole = ServiceRole String
derive instance newtypeServiceRole :: Newtype ServiceRole _


-- | <p>The number of managed instances found for each patch severity level defined in the request filter.</p>
newtype SeveritySummary = SeveritySummary 
  { "CriticalCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "HighCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "MediumCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "LowCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "InformationalCount" :: NullOrUndefined (ComplianceSummaryCount)
  , "UnspecifiedCount" :: NullOrUndefined (ComplianceSummaryCount)
  }
derive instance newtypeSeveritySummary :: Newtype SeveritySummary _


newtype SignalType = SignalType String
derive instance newtypeSignalType :: Newtype SignalType _


newtype SnapshotDownloadUrl = SnapshotDownloadUrl String
derive instance newtypeSnapshotDownloadUrl :: Newtype SnapshotDownloadUrl _


newtype SnapshotId = SnapshotId String
derive instance newtypeSnapshotId :: Newtype SnapshotId _


newtype StandardErrorContent = StandardErrorContent String
derive instance newtypeStandardErrorContent :: Newtype StandardErrorContent _


newtype StandardOutputContent = StandardOutputContent String
derive instance newtypeStandardOutputContent :: Newtype StandardOutputContent _


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
derive instance newtypeStartAutomationExecutionRequest :: Newtype StartAutomationExecutionRequest _


newtype StartAutomationExecutionResult = StartAutomationExecutionResult 
  { "AutomationExecutionId" :: NullOrUndefined (AutomationExecutionId)
  }
derive instance newtypeStartAutomationExecutionResult :: Newtype StartAutomationExecutionResult _


newtype StatusAdditionalInfo = StatusAdditionalInfo String
derive instance newtypeStatusAdditionalInfo :: Newtype StatusAdditionalInfo _


newtype StatusDetails = StatusDetails String
derive instance newtypeStatusDetails :: Newtype StatusDetails _


newtype StatusMessage = StatusMessage String
derive instance newtypeStatusMessage :: Newtype StatusMessage _


newtype StatusName = StatusName String
derive instance newtypeStatusName :: Newtype StatusName _


-- | <p>The updated status is the same as the current status.</p>
newtype StatusUnchanged = StatusUnchanged 
  { 
  }
derive instance newtypeStatusUnchanged :: Newtype StatusUnchanged _


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
derive instance newtypeStepExecution :: Newtype StepExecution _


-- | <p>A filter to limit the amount of step execution information returned by the call.</p>
newtype StepExecutionFilter = StepExecutionFilter 
  { "Key" :: (StepExecutionFilterKey)
  , "Values" :: (StepExecutionFilterValueList)
  }
derive instance newtypeStepExecutionFilter :: Newtype StepExecutionFilter _


newtype StepExecutionFilterKey = StepExecutionFilterKey String
derive instance newtypeStepExecutionFilterKey :: Newtype StepExecutionFilterKey _


newtype StepExecutionFilterList = StepExecutionFilterList (Array StepExecutionFilter)
derive instance newtypeStepExecutionFilterList :: Newtype StepExecutionFilterList _


newtype StepExecutionFilterValue = StepExecutionFilterValue String
derive instance newtypeStepExecutionFilterValue :: Newtype StepExecutionFilterValue _


newtype StepExecutionFilterValueList = StepExecutionFilterValueList (Array StepExecutionFilterValue)
derive instance newtypeStepExecutionFilterValueList :: Newtype StepExecutionFilterValueList _


newtype StepExecutionList = StepExecutionList (Array StepExecution)
derive instance newtypeStepExecutionList :: Newtype StepExecutionList _


newtype StopAutomationExecutionRequest = StopAutomationExecutionRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  , "Type" :: NullOrUndefined (StopType)
  }
derive instance newtypeStopAutomationExecutionRequest :: Newtype StopAutomationExecutionRequest _


newtype StopAutomationExecutionResult = StopAutomationExecutionResult 
  { 
  }
derive instance newtypeStopAutomationExecutionResult :: Newtype StopAutomationExecutionResult _


newtype StopType = StopType String
derive instance newtypeStopType :: Newtype StopType _


newtype StringDateTime = StringDateTime String
derive instance newtypeStringDateTime :: Newtype StringDateTime _


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _


-- | <p>The sub-type count exceeded the limit for the inventory type.</p>
newtype SubTypeCountLimitExceededException = SubTypeCountLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeSubTypeCountLimitExceededException :: Newtype SubTypeCountLimitExceededException _


-- | <p>Metadata that you assign to your AWS resources. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. In Systems Manager, you can apply tags to documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


-- | <p>An array of search criteria that targets instances using a Key,Value combination that you specify. <code>Targets</code> is required if you don't provide one or more instance IDs in the call.</p> <p/>
newtype Target = Target 
  { "Key" :: NullOrUndefined (TargetKey)
  , "Values" :: NullOrUndefined (TargetValues)
  }
derive instance newtypeTarget :: Newtype Target _


newtype TargetCount = TargetCount Int
derive instance newtypeTargetCount :: Newtype TargetCount _


-- | <p>You specified the <code>Safe</code> option for the DeregisterTargetFromMaintenanceWindow operation, but the target is still referenced in a task.</p>
newtype TargetInUseException = TargetInUseException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeTargetInUseException :: Newtype TargetInUseException _


newtype TargetKey = TargetKey String
derive instance newtypeTargetKey :: Newtype TargetKey _


newtype TargetParameterList = TargetParameterList (Array ParameterValue)
derive instance newtypeTargetParameterList :: Newtype TargetParameterList _


newtype TargetType = TargetType String
derive instance newtypeTargetType :: Newtype TargetType _


newtype TargetValue = TargetValue String
derive instance newtypeTargetValue :: Newtype TargetValue _


newtype TargetValues = TargetValues (Array TargetValue)
derive instance newtypeTargetValues :: Newtype TargetValues _


newtype Targets = Targets (Array Target)
derive instance newtypeTargets :: Newtype Targets _


newtype TimeoutSeconds = TimeoutSeconds Int
derive instance newtypeTimeoutSeconds :: Newtype TimeoutSeconds _


-- | <p>The Targets parameter includes too many tags. Remove one or more tags and try the command again.</p>
newtype TooManyTagsError = TooManyTagsError 
  { 
  }
derive instance newtypeTooManyTagsError :: Newtype TooManyTagsError _


-- | <p>There are concurrent updates for a resource that supports one update at a time.</p>
newtype TooManyUpdates = TooManyUpdates 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeTooManyUpdates :: Newtype TooManyUpdates _


-- | <p>The size of inventory data has exceeded the total size limit for the resource.</p>
newtype TotalSizeLimitExceededException = TotalSizeLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeTotalSizeLimitExceededException :: Newtype TotalSizeLimitExceededException _


-- | <p>The <code>Context</code> attribute that you specified for the <code>InventoryItem</code> is not allowed for this inventory type. You can only use the <code>Context</code> attribute with inventory types like <code>AWS:ComplianceItem</code>.</p>
newtype UnsupportedInventoryItemContextException = UnsupportedInventoryItemContextException 
  { "TypeName" :: NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeUnsupportedInventoryItemContextException :: Newtype UnsupportedInventoryItemContextException _


-- | <p>Inventory item type schema version has to match supported versions in the service. Check output of GetInventorySchema to see the available schema version for each type.</p>
newtype UnsupportedInventorySchemaVersionException = UnsupportedInventorySchemaVersionException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeUnsupportedInventorySchemaVersionException :: Newtype UnsupportedInventorySchemaVersionException _


-- | <p>The operating systems you specified is not supported, or the operation is not supported for the operating system. Valid operating systems include: Windows, AmazonLinux, RedhatEnterpriseLinux, and Ubuntu.</p>
newtype UnsupportedOperatingSystem = UnsupportedOperatingSystem 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeUnsupportedOperatingSystem :: Newtype UnsupportedOperatingSystem _


-- | <p>The parameter type is not supported.</p>
newtype UnsupportedParameterType = UnsupportedParameterType 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeUnsupportedParameterType :: Newtype UnsupportedParameterType _


-- | <p>The document does not support the platform type of the given instance ID(s). For example, you sent an document for a Windows instance to a Linux instance.</p>
newtype UnsupportedPlatformType = UnsupportedPlatformType 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeUnsupportedPlatformType :: Newtype UnsupportedPlatformType _


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
derive instance newtypeUpdateAssociationRequest :: Newtype UpdateAssociationRequest _


newtype UpdateAssociationResult = UpdateAssociationResult 
  { "AssociationDescription" :: NullOrUndefined (AssociationDescription)
  }
derive instance newtypeUpdateAssociationResult :: Newtype UpdateAssociationResult _


newtype UpdateAssociationStatusRequest = UpdateAssociationStatusRequest 
  { "Name" :: (DocumentName)
  , "InstanceId" :: (InstanceId)
  , "AssociationStatus" :: (AssociationStatus)
  }
derive instance newtypeUpdateAssociationStatusRequest :: Newtype UpdateAssociationStatusRequest _


newtype UpdateAssociationStatusResult = UpdateAssociationStatusResult 
  { "AssociationDescription" :: NullOrUndefined (AssociationDescription)
  }
derive instance newtypeUpdateAssociationStatusResult :: Newtype UpdateAssociationStatusResult _


newtype UpdateDocumentDefaultVersionRequest = UpdateDocumentDefaultVersionRequest 
  { "Name" :: (DocumentName)
  , "DocumentVersion" :: (DocumentVersionNumber)
  }
derive instance newtypeUpdateDocumentDefaultVersionRequest :: Newtype UpdateDocumentDefaultVersionRequest _


newtype UpdateDocumentDefaultVersionResult = UpdateDocumentDefaultVersionResult 
  { "Description" :: NullOrUndefined (DocumentDefaultVersionDescription)
  }
derive instance newtypeUpdateDocumentDefaultVersionResult :: Newtype UpdateDocumentDefaultVersionResult _


newtype UpdateDocumentRequest = UpdateDocumentRequest 
  { "Content" :: (DocumentContent)
  , "Name" :: (DocumentName)
  , "DocumentVersion" :: NullOrUndefined (DocumentVersion)
  , "DocumentFormat" :: NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined (TargetType)
  }
derive instance newtypeUpdateDocumentRequest :: Newtype UpdateDocumentRequest _


newtype UpdateDocumentResult = UpdateDocumentResult 
  { "DocumentDescription" :: NullOrUndefined (DocumentDescription)
  }
derive instance newtypeUpdateDocumentResult :: Newtype UpdateDocumentResult _


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
derive instance newtypeUpdateMaintenanceWindowRequest :: Newtype UpdateMaintenanceWindowRequest _


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
derive instance newtypeUpdateMaintenanceWindowResult :: Newtype UpdateMaintenanceWindowResult _


newtype UpdateMaintenanceWindowTargetRequest = UpdateMaintenanceWindowTargetRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTargetId" :: (MaintenanceWindowTargetId)
  , "Targets" :: NullOrUndefined (Targets)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  , "Replace" :: NullOrUndefined (Boolean)
  }
derive instance newtypeUpdateMaintenanceWindowTargetRequest :: Newtype UpdateMaintenanceWindowTargetRequest _


newtype UpdateMaintenanceWindowTargetResult = UpdateMaintenanceWindowTargetResult 
  { "WindowId" :: NullOrUndefined (MaintenanceWindowId)
  , "WindowTargetId" :: NullOrUndefined (MaintenanceWindowTargetId)
  , "Targets" :: NullOrUndefined (Targets)
  , "OwnerInformation" :: NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined (MaintenanceWindowDescription)
  }
derive instance newtypeUpdateMaintenanceWindowTargetResult :: Newtype UpdateMaintenanceWindowTargetResult _


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
derive instance newtypeUpdateMaintenanceWindowTaskRequest :: Newtype UpdateMaintenanceWindowTaskRequest _


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
derive instance newtypeUpdateMaintenanceWindowTaskResult :: Newtype UpdateMaintenanceWindowTaskResult _


newtype UpdateManagedInstanceRoleRequest = UpdateManagedInstanceRoleRequest 
  { "InstanceId" :: (ManagedInstanceId)
  , "IamRole" :: (IamRole)
  }
derive instance newtypeUpdateManagedInstanceRoleRequest :: Newtype UpdateManagedInstanceRoleRequest _


newtype UpdateManagedInstanceRoleResult = UpdateManagedInstanceRoleResult 
  { 
  }
derive instance newtypeUpdateManagedInstanceRoleResult :: Newtype UpdateManagedInstanceRoleResult _


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
derive instance newtypeUpdatePatchBaselineRequest :: Newtype UpdatePatchBaselineRequest _


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
derive instance newtypeUpdatePatchBaselineResult :: Newtype UpdatePatchBaselineResult _


newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _
