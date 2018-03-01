

-- | <fullname>AWS Systems Manager</fullname> <p>AWS Systems Manager is a collection of capabilities that helps you automate management tasks such as collecting system inventory, applying operating system (OS) patches, automating the creation of Amazon Machine Images (AMIs), and configuring operating systems (OSs) and applications at scale. Systems Manager lets you remotely and securely manage the configuration of your managed instances. A <i>managed instance</i> is any Amazon EC2 instance or on-premises machine in your hybrid environment that has been configured for Systems Manager.</p> <p>This reference is intended to be used with the <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/">AWS Systems Manager User Guide</a>.</p> <p>To get started, verify prerequisites and configure managed instances. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-setting-up.html">Systems Manager Prerequisites</a>.</p> <p>For information about other API actions you can perform on Amazon EC2 instances, see the <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/">Amazon EC2 API Reference</a>. For information about how to use a Query API, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/making-api-requests.html">Making API Requests</a>. </p>
module AWS.SSM where

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

serviceName = "SSM" :: String


-- | <p>Adds or overwrites one or more tags for the specified resource. Tags are metadata that you can assign to your documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. Each tag consists of a key and an optional value, both of which you define. For example, you could define a set of tags for your account's managed instances that helps you track each instance's owner and stack level. For example: Key=Owner and Value=DbAdmin, SysAdmin, or Dev. Or Key=Stack and Value=Production, Pre-Production, or Test.</p> <p>Each resource can have a maximum of 50 tags. </p> <p>We recommend that you devise a set of tag keys that meets your needs for each resource type. Using a consistent set of tag keys makes it easier for you to manage your resources. You can search and filter the resources based on the tags you add. Tags don't have any semantic meaning to Amazon EC2 and are interpreted strictly as a string of characters. </p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html">Tagging Your Amazon EC2 Resources</a> in the <i>Amazon EC2 User Guide</i>.</p>
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (exception :: EXCEPTION | eff) AddTagsToResourceResult
addTagsToResource = Request.request serviceName "addTagsToResource" 


-- | <p>Attempts to cancel the command specified by the Command ID. There is no guarantee that the command will be terminated and the underlying process stopped.</p>
cancelCommand :: forall eff. CancelCommandRequest -> Aff (exception :: EXCEPTION | eff) CancelCommandResult
cancelCommand = Request.request serviceName "cancelCommand" 


-- | <p>Registers your on-premises server or virtual machine with Amazon EC2 so that you can manage these resources using Run Command. An on-premises server or virtual machine that has been registered with EC2 is called a managed instance. For more information about activations, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-managedinstances.html">Setting Up Systems Manager in Hybrid Environments</a>.</p>
createActivation :: forall eff. CreateActivationRequest -> Aff (exception :: EXCEPTION | eff) CreateActivationResult
createActivation = Request.request serviceName "createActivation" 


-- | <p>Associates the specified Systems Manager document with the specified instances or targets.</p> <p>When you associate a document with one or more instances using instance IDs or tags, the SSM Agent running on the instance processes the document and configures the instance as specified.</p> <p>If you associate a document with an instance that already has an associated document, the system throws the AssociationAlreadyExists exception.</p>
createAssociation :: forall eff. CreateAssociationRequest -> Aff (exception :: EXCEPTION | eff) CreateAssociationResult
createAssociation = Request.request serviceName "createAssociation" 


-- | <p>Associates the specified Systems Manager document with the specified instances or targets.</p> <p>When you associate a document with one or more instances using instance IDs or tags, the SSM Agent running on the instance processes the document and configures the instance as specified.</p> <p>If you associate a document with an instance that already has an associated document, the system throws the AssociationAlreadyExists exception.</p>
createAssociationBatch :: forall eff. CreateAssociationBatchRequest -> Aff (exception :: EXCEPTION | eff) CreateAssociationBatchResult
createAssociationBatch = Request.request serviceName "createAssociationBatch" 


-- | <p>Creates a Systems Manager document.</p> <p>After you create a document, you can use CreateAssociation to associate it with one or more running instances.</p>
createDocument :: forall eff. CreateDocumentRequest -> Aff (exception :: EXCEPTION | eff) CreateDocumentResult
createDocument = Request.request serviceName "createDocument" 


-- | <p>Creates a new Maintenance Window.</p>
createMaintenanceWindow :: forall eff. CreateMaintenanceWindowRequest -> Aff (exception :: EXCEPTION | eff) CreateMaintenanceWindowResult
createMaintenanceWindow = Request.request serviceName "createMaintenanceWindow" 


-- | <p>Creates a patch baseline.</p> <note> <p>For information about valid key and value pairs in <code>PatchFilters</code> for each supported operating system type, see <a href="http://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PatchFilter.html">PatchFilter</a>.</p> </note>
createPatchBaseline :: forall eff. CreatePatchBaselineRequest -> Aff (exception :: EXCEPTION | eff) CreatePatchBaselineResult
createPatchBaseline = Request.request serviceName "createPatchBaseline" 


-- | <p>Creates a resource data sync configuration to a single bucket in Amazon S3. This is an asynchronous operation that returns immediately. After a successful initial sync is completed, the system continuously syncs data to the Amazon S3 bucket. To check the status of the sync, use the <a>ListResourceDataSync</a>.</p> <p>By default, data is not encrypted in Amazon S3. We strongly recommend that you enable encryption in Amazon S3 to ensure secure data storage. We also recommend that you secure access to the Amazon S3 bucket by creating a restrictive bucket policy. To view an example of a restrictive Amazon S3 bucket policy for Resource Data Sync, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-configuring.html#sysman-inventory-datasync">Configuring Resource Data Sync for Inventory</a>.</p>
createResourceDataSync :: forall eff. CreateResourceDataSyncRequest -> Aff (exception :: EXCEPTION | eff) CreateResourceDataSyncResult
createResourceDataSync = Request.request serviceName "createResourceDataSync" 


-- | <p>Deletes an activation. You are not required to delete an activation. If you delete an activation, you can no longer use it to register additional managed instances. Deleting an activation does not de-register managed instances. You must manually de-register managed instances.</p>
deleteActivation :: forall eff. DeleteActivationRequest -> Aff (exception :: EXCEPTION | eff) DeleteActivationResult
deleteActivation = Request.request serviceName "deleteActivation" 


-- | <p>Disassociates the specified Systems Manager document from the specified instance.</p> <p>When you disassociate a document from an instance, it does not change the configuration of the instance. To change the configuration state of an instance after you disassociate a document, you must create a new document with the desired configuration and associate it with the instance.</p>
deleteAssociation :: forall eff. DeleteAssociationRequest -> Aff (exception :: EXCEPTION | eff) DeleteAssociationResult
deleteAssociation = Request.request serviceName "deleteAssociation" 


-- | <p>Deletes the Systems Manager document and all instance associations to the document.</p> <p>Before you delete the document, we recommend that you use <a>DeleteAssociation</a> to disassociate all instances that are associated with the document.</p>
deleteDocument :: forall eff. DeleteDocumentRequest -> Aff (exception :: EXCEPTION | eff) DeleteDocumentResult
deleteDocument = Request.request serviceName "deleteDocument" 


-- | <p>Deletes a Maintenance Window.</p>
deleteMaintenanceWindow :: forall eff. DeleteMaintenanceWindowRequest -> Aff (exception :: EXCEPTION | eff) DeleteMaintenanceWindowResult
deleteMaintenanceWindow = Request.request serviceName "deleteMaintenanceWindow" 


-- | <p>Delete a parameter from the system.</p>
deleteParameter :: forall eff. DeleteParameterRequest -> Aff (exception :: EXCEPTION | eff) DeleteParameterResult
deleteParameter = Request.request serviceName "deleteParameter" 


-- | <p>Delete a list of parameters. This API is used to delete parameters by using the Amazon EC2 console.</p>
deleteParameters :: forall eff. DeleteParametersRequest -> Aff (exception :: EXCEPTION | eff) DeleteParametersResult
deleteParameters = Request.request serviceName "deleteParameters" 


-- | <p>Deletes a patch baseline.</p>
deletePatchBaseline :: forall eff. DeletePatchBaselineRequest -> Aff (exception :: EXCEPTION | eff) DeletePatchBaselineResult
deletePatchBaseline = Request.request serviceName "deletePatchBaseline" 


-- | <p>Deletes a Resource Data Sync configuration. After the configuration is deleted, changes to inventory data on managed instances are no longer synced with the target Amazon S3 bucket. Deleting a sync configuration does not delete data in the target Amazon S3 bucket.</p>
deleteResourceDataSync :: forall eff. DeleteResourceDataSyncRequest -> Aff (exception :: EXCEPTION | eff) DeleteResourceDataSyncResult
deleteResourceDataSync = Request.request serviceName "deleteResourceDataSync" 


-- | <p>Removes the server or virtual machine from the list of registered servers. You can reregister the instance again at any time. If you don't plan to use Run Command on the server, we suggest uninstalling the SSM Agent first.</p>
deregisterManagedInstance :: forall eff. DeregisterManagedInstanceRequest -> Aff (exception :: EXCEPTION | eff) DeregisterManagedInstanceResult
deregisterManagedInstance = Request.request serviceName "deregisterManagedInstance" 


-- | <p>Removes a patch group from a patch baseline.</p>
deregisterPatchBaselineForPatchGroup :: forall eff. DeregisterPatchBaselineForPatchGroupRequest -> Aff (exception :: EXCEPTION | eff) DeregisterPatchBaselineForPatchGroupResult
deregisterPatchBaselineForPatchGroup = Request.request serviceName "deregisterPatchBaselineForPatchGroup" 


-- | <p>Removes a target from a Maintenance Window.</p>
deregisterTargetFromMaintenanceWindow :: forall eff. DeregisterTargetFromMaintenanceWindowRequest -> Aff (exception :: EXCEPTION | eff) DeregisterTargetFromMaintenanceWindowResult
deregisterTargetFromMaintenanceWindow = Request.request serviceName "deregisterTargetFromMaintenanceWindow" 


-- | <p>Removes a task from a Maintenance Window.</p>
deregisterTaskFromMaintenanceWindow :: forall eff. DeregisterTaskFromMaintenanceWindowRequest -> Aff (exception :: EXCEPTION | eff) DeregisterTaskFromMaintenanceWindowResult
deregisterTaskFromMaintenanceWindow = Request.request serviceName "deregisterTaskFromMaintenanceWindow" 


-- | <p>Details about the activation, including: the date and time the activation was created, the expiration date, the IAM role assigned to the instances in the activation, and the number of instances activated by this registration.</p>
describeActivations :: forall eff. DescribeActivationsRequest -> Aff (exception :: EXCEPTION | eff) DescribeActivationsResult
describeActivations = Request.request serviceName "describeActivations" 


-- | <p>Describes the association for the specified target or instance. If you created the association by using the <code>Targets</code> parameter, then you must retrieve the association by using the association ID. If you created the association by specifying an instance ID and a Systems Manager document, then you retrieve the association by specifying the document name and the instance ID. </p>
describeAssociation :: forall eff. DescribeAssociationRequest -> Aff (exception :: EXCEPTION | eff) DescribeAssociationResult
describeAssociation = Request.request serviceName "describeAssociation" 


-- | <p>Provides details about all active and terminated Automation executions.</p>
describeAutomationExecutions :: forall eff. DescribeAutomationExecutionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeAutomationExecutionsResult
describeAutomationExecutions = Request.request serviceName "describeAutomationExecutions" 


-- | <p>Information about all active and terminated step executions in an Automation workflow.</p>
describeAutomationStepExecutions :: forall eff. DescribeAutomationStepExecutionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeAutomationStepExecutionsResult
describeAutomationStepExecutions = Request.request serviceName "describeAutomationStepExecutions" 


-- | <p>Lists all patches that could possibly be included in a patch baseline.</p>
describeAvailablePatches :: forall eff. DescribeAvailablePatchesRequest -> Aff (exception :: EXCEPTION | eff) DescribeAvailablePatchesResult
describeAvailablePatches = Request.request serviceName "describeAvailablePatches" 


-- | <p>Describes the specified Systems Manager document.</p>
describeDocument :: forall eff. DescribeDocumentRequest -> Aff (exception :: EXCEPTION | eff) DescribeDocumentResult
describeDocument = Request.request serviceName "describeDocument" 


-- | <p>Describes the permissions for a Systems Manager document. If you created the document, you are the owner. If a document is shared, it can either be shared privately (by specifying a user's AWS account ID) or publicly (<i>All</i>). </p>
describeDocumentPermission :: forall eff. DescribeDocumentPermissionRequest -> Aff (exception :: EXCEPTION | eff) DescribeDocumentPermissionResponse
describeDocumentPermission = Request.request serviceName "describeDocumentPermission" 


-- | <p>All associations for the instance(s).</p>
describeEffectiveInstanceAssociations :: forall eff. DescribeEffectiveInstanceAssociationsRequest -> Aff (exception :: EXCEPTION | eff) DescribeEffectiveInstanceAssociationsResult
describeEffectiveInstanceAssociations = Request.request serviceName "describeEffectiveInstanceAssociations" 


-- | <p>Retrieves the current effective patches (the patch and the approval state) for the specified patch baseline. Note that this API applies only to Windows patch baselines.</p>
describeEffectivePatchesForPatchBaseline :: forall eff. DescribeEffectivePatchesForPatchBaselineRequest -> Aff (exception :: EXCEPTION | eff) DescribeEffectivePatchesForPatchBaselineResult
describeEffectivePatchesForPatchBaseline = Request.request serviceName "describeEffectivePatchesForPatchBaseline" 


-- | <p>The status of the associations for the instance(s).</p>
describeInstanceAssociationsStatus :: forall eff. DescribeInstanceAssociationsStatusRequest -> Aff (exception :: EXCEPTION | eff) DescribeInstanceAssociationsStatusResult
describeInstanceAssociationsStatus = Request.request serviceName "describeInstanceAssociationsStatus" 


-- | <p>Describes one or more of your instances. You can use this to get information about instances like the operating system platform, the SSM Agent version (Linux), status etc. If you specify one or more instance IDs, it returns information for those instances. If you do not specify instance IDs, it returns information for all your instances. If you specify an instance ID that is not valid or an instance that you do not own, you receive an error. </p>
describeInstanceInformation :: forall eff. DescribeInstanceInformationRequest -> Aff (exception :: EXCEPTION | eff) DescribeInstanceInformationResult
describeInstanceInformation = Request.request serviceName "describeInstanceInformation" 


-- | <p>Retrieves the high-level patch state of one or more instances.</p>
describeInstancePatchStates :: forall eff. DescribeInstancePatchStatesRequest -> Aff (exception :: EXCEPTION | eff) DescribeInstancePatchStatesResult
describeInstancePatchStates = Request.request serviceName "describeInstancePatchStates" 


-- | <p>Retrieves the high-level patch state for the instances in the specified patch group.</p>
describeInstancePatchStatesForPatchGroup :: forall eff. DescribeInstancePatchStatesForPatchGroupRequest -> Aff (exception :: EXCEPTION | eff) DescribeInstancePatchStatesForPatchGroupResult
describeInstancePatchStatesForPatchGroup = Request.request serviceName "describeInstancePatchStatesForPatchGroup" 


-- | <p>Retrieves information about the patches on the specified instance and their state relative to the patch baseline being used for the instance.</p>
describeInstancePatches :: forall eff. DescribeInstancePatchesRequest -> Aff (exception :: EXCEPTION | eff) DescribeInstancePatchesResult
describeInstancePatches = Request.request serviceName "describeInstancePatches" 


-- | <p>Retrieves the individual task executions (one per target) for a particular task executed as part of a Maintenance Window execution.</p>
describeMaintenanceWindowExecutionTaskInvocations :: forall eff. DescribeMaintenanceWindowExecutionTaskInvocationsRequest -> Aff (exception :: EXCEPTION | eff) DescribeMaintenanceWindowExecutionTaskInvocationsResult
describeMaintenanceWindowExecutionTaskInvocations = Request.request serviceName "describeMaintenanceWindowExecutionTaskInvocations" 


-- | <p>For a given Maintenance Window execution, lists the tasks that were executed.</p>
describeMaintenanceWindowExecutionTasks :: forall eff. DescribeMaintenanceWindowExecutionTasksRequest -> Aff (exception :: EXCEPTION | eff) DescribeMaintenanceWindowExecutionTasksResult
describeMaintenanceWindowExecutionTasks = Request.request serviceName "describeMaintenanceWindowExecutionTasks" 


-- | <p>Lists the executions of a Maintenance Window. This includes information about when the Maintenance Window was scheduled to be active, and information about tasks registered and run with the Maintenance Window.</p>
describeMaintenanceWindowExecutions :: forall eff. DescribeMaintenanceWindowExecutionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeMaintenanceWindowExecutionsResult
describeMaintenanceWindowExecutions = Request.request serviceName "describeMaintenanceWindowExecutions" 


-- | <p>Lists the targets registered with the Maintenance Window.</p>
describeMaintenanceWindowTargets :: forall eff. DescribeMaintenanceWindowTargetsRequest -> Aff (exception :: EXCEPTION | eff) DescribeMaintenanceWindowTargetsResult
describeMaintenanceWindowTargets = Request.request serviceName "describeMaintenanceWindowTargets" 


-- | <p>Lists the tasks in a Maintenance Window.</p>
describeMaintenanceWindowTasks :: forall eff. DescribeMaintenanceWindowTasksRequest -> Aff (exception :: EXCEPTION | eff) DescribeMaintenanceWindowTasksResult
describeMaintenanceWindowTasks = Request.request serviceName "describeMaintenanceWindowTasks" 


-- | <p>Retrieves the Maintenance Windows in an AWS account.</p>
describeMaintenanceWindows :: forall eff. DescribeMaintenanceWindowsRequest -> Aff (exception :: EXCEPTION | eff) DescribeMaintenanceWindowsResult
describeMaintenanceWindows = Request.request serviceName "describeMaintenanceWindows" 


-- | <p>Get information about a parameter.</p> <p>Request results are returned on a best-effort basis. If you specify <code>MaxResults</code> in the request, the response includes information up to the limit specified. The number of items returned, however, can be between zero and the value of <code>MaxResults</code>. If the service reaches an internal limit while processing the results, it stops the operation and returns the matching values up to that point and a <code>NextToken</code>. You can specify the <code>NextToken</code> in a subsequent call to get the next set of results.</p>
describeParameters :: forall eff. DescribeParametersRequest -> Aff (exception :: EXCEPTION | eff) DescribeParametersResult
describeParameters = Request.request serviceName "describeParameters" 


-- | <p>Lists the patch baselines in your AWS account.</p>
describePatchBaselines :: forall eff. DescribePatchBaselinesRequest -> Aff (exception :: EXCEPTION | eff) DescribePatchBaselinesResult
describePatchBaselines = Request.request serviceName "describePatchBaselines" 


-- | <p>Returns high-level aggregated patch compliance state for a patch group.</p>
describePatchGroupState :: forall eff. DescribePatchGroupStateRequest -> Aff (exception :: EXCEPTION | eff) DescribePatchGroupStateResult
describePatchGroupState = Request.request serviceName "describePatchGroupState" 


-- | <p>Lists all patch groups that have been registered with patch baselines.</p>
describePatchGroups :: forall eff. DescribePatchGroupsRequest -> Aff (exception :: EXCEPTION | eff) DescribePatchGroupsResult
describePatchGroups = Request.request serviceName "describePatchGroups" 


-- | <p>Get detailed information about a particular Automation execution.</p>
getAutomationExecution :: forall eff. GetAutomationExecutionRequest -> Aff (exception :: EXCEPTION | eff) GetAutomationExecutionResult
getAutomationExecution = Request.request serviceName "getAutomationExecution" 


-- | <p>Returns detailed information about command execution for an invocation or plugin. </p>
getCommandInvocation :: forall eff. GetCommandInvocationRequest -> Aff (exception :: EXCEPTION | eff) GetCommandInvocationResult
getCommandInvocation = Request.request serviceName "getCommandInvocation" 


-- | <p>Retrieves the default patch baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.</p>
getDefaultPatchBaseline :: forall eff. GetDefaultPatchBaselineRequest -> Aff (exception :: EXCEPTION | eff) GetDefaultPatchBaselineResult
getDefaultPatchBaseline = Request.request serviceName "getDefaultPatchBaseline" 


-- | <p>Retrieves the current snapshot for the patch baseline the instance uses. This API is primarily used by the AWS-RunPatchBaseline Systems Manager document. </p>
getDeployablePatchSnapshotForInstance :: forall eff. GetDeployablePatchSnapshotForInstanceRequest -> Aff (exception :: EXCEPTION | eff) GetDeployablePatchSnapshotForInstanceResult
getDeployablePatchSnapshotForInstance = Request.request serviceName "getDeployablePatchSnapshotForInstance" 


-- | <p>Gets the contents of the specified Systems Manager document.</p>
getDocument :: forall eff. GetDocumentRequest -> Aff (exception :: EXCEPTION | eff) GetDocumentResult
getDocument = Request.request serviceName "getDocument" 


-- | <p>Query inventory information.</p>
getInventory :: forall eff. GetInventoryRequest -> Aff (exception :: EXCEPTION | eff) GetInventoryResult
getInventory = Request.request serviceName "getInventory" 


-- | <p>Return a list of inventory type names for the account, or return a list of attribute names for a specific Inventory item type. </p>
getInventorySchema :: forall eff. GetInventorySchemaRequest -> Aff (exception :: EXCEPTION | eff) GetInventorySchemaResult
getInventorySchema = Request.request serviceName "getInventorySchema" 


-- | <p>Retrieves a Maintenance Window.</p>
getMaintenanceWindow :: forall eff. GetMaintenanceWindowRequest -> Aff (exception :: EXCEPTION | eff) GetMaintenanceWindowResult
getMaintenanceWindow = Request.request serviceName "getMaintenanceWindow" 


-- | <p>Retrieves details about a specific task executed as part of a Maintenance Window execution.</p>
getMaintenanceWindowExecution :: forall eff. GetMaintenanceWindowExecutionRequest -> Aff (exception :: EXCEPTION | eff) GetMaintenanceWindowExecutionResult
getMaintenanceWindowExecution = Request.request serviceName "getMaintenanceWindowExecution" 


-- | <p>Retrieves the details about a specific task executed as part of a Maintenance Window execution.</p>
getMaintenanceWindowExecutionTask :: forall eff. GetMaintenanceWindowExecutionTaskRequest -> Aff (exception :: EXCEPTION | eff) GetMaintenanceWindowExecutionTaskResult
getMaintenanceWindowExecutionTask = Request.request serviceName "getMaintenanceWindowExecutionTask" 


-- | <p>Retrieves a task invocation. A task invocation is a specific task executing on a specific target. Maintenance Windows report status for all invocations. </p>
getMaintenanceWindowExecutionTaskInvocation :: forall eff. GetMaintenanceWindowExecutionTaskInvocationRequest -> Aff (exception :: EXCEPTION | eff) GetMaintenanceWindowExecutionTaskInvocationResult
getMaintenanceWindowExecutionTaskInvocation = Request.request serviceName "getMaintenanceWindowExecutionTaskInvocation" 


-- | <p>Lists the tasks in a Maintenance Window.</p>
getMaintenanceWindowTask :: forall eff. GetMaintenanceWindowTaskRequest -> Aff (exception :: EXCEPTION | eff) GetMaintenanceWindowTaskResult
getMaintenanceWindowTask = Request.request serviceName "getMaintenanceWindowTask" 


-- | <p>Get information about a parameter by using the parameter name. </p>
getParameter :: forall eff. GetParameterRequest -> Aff (exception :: EXCEPTION | eff) GetParameterResult
getParameter = Request.request serviceName "getParameter" 


-- | <p>Query a list of all parameters used by the AWS account.</p>
getParameterHistory :: forall eff. GetParameterHistoryRequest -> Aff (exception :: EXCEPTION | eff) GetParameterHistoryResult
getParameterHistory = Request.request serviceName "getParameterHistory" 


-- | <p>Get details of a parameter.</p>
getParameters :: forall eff. GetParametersRequest -> Aff (exception :: EXCEPTION | eff) GetParametersResult
getParameters = Request.request serviceName "getParameters" 


-- | <p>Retrieve parameters in a specific hierarchy. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html">Working with Systems Manager Parameters</a>. </p> <p>Request results are returned on a best-effort basis. If you specify <code>MaxResults</code> in the request, the response includes information up to the limit specified. The number of items returned, however, can be between zero and the value of <code>MaxResults</code>. If the service reaches an internal limit while processing the results, it stops the operation and returns the matching values up to that point and a <code>NextToken</code>. You can specify the <code>NextToken</code> in a subsequent call to get the next set of results.</p> <note> <p>This API action doesn't support filtering by tags. </p> </note>
getParametersByPath :: forall eff. GetParametersByPathRequest -> Aff (exception :: EXCEPTION | eff) GetParametersByPathResult
getParametersByPath = Request.request serviceName "getParametersByPath" 


-- | <p>Retrieves information about a patch baseline.</p>
getPatchBaseline :: forall eff. GetPatchBaselineRequest -> Aff (exception :: EXCEPTION | eff) GetPatchBaselineResult
getPatchBaseline = Request.request serviceName "getPatchBaseline" 


-- | <p>Retrieves the patch baseline that should be used for the specified patch group.</p>
getPatchBaselineForPatchGroup :: forall eff. GetPatchBaselineForPatchGroupRequest -> Aff (exception :: EXCEPTION | eff) GetPatchBaselineForPatchGroupResult
getPatchBaselineForPatchGroup = Request.request serviceName "getPatchBaselineForPatchGroup" 


-- | <p>Retrieves all versions of an association for a specific association ID.</p>
listAssociationVersions :: forall eff. ListAssociationVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListAssociationVersionsResult
listAssociationVersions = Request.request serviceName "listAssociationVersions" 


-- | <p>Lists the associations for the specified Systems Manager document or instance.</p>
listAssociations :: forall eff. ListAssociationsRequest -> Aff (exception :: EXCEPTION | eff) ListAssociationsResult
listAssociations = Request.request serviceName "listAssociations" 


-- | <p>An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. ListCommandInvocations provide status about command execution.</p>
listCommandInvocations :: forall eff. ListCommandInvocationsRequest -> Aff (exception :: EXCEPTION | eff) ListCommandInvocationsResult
listCommandInvocations = Request.request serviceName "listCommandInvocations" 


-- | <p>Lists the commands requested by users of the AWS account.</p>
listCommands :: forall eff. ListCommandsRequest -> Aff (exception :: EXCEPTION | eff) ListCommandsResult
listCommands = Request.request serviceName "listCommands" 


-- | <p>For a specified resource ID, this API action returns a list of compliance statuses for different resource types. Currently, you can only specify one resource ID per call. List results depend on the criteria specified in the filter. </p>
listComplianceItems :: forall eff. ListComplianceItemsRequest -> Aff (exception :: EXCEPTION | eff) ListComplianceItemsResult
listComplianceItems = Request.request serviceName "listComplianceItems" 


-- | <p>Returns a summary count of compliant and non-compliant resources for a compliance type. For example, this call can return State Manager associations, patches, or custom compliance types according to the filter criteria that you specify. </p>
listComplianceSummaries :: forall eff. ListComplianceSummariesRequest -> Aff (exception :: EXCEPTION | eff) ListComplianceSummariesResult
listComplianceSummaries = Request.request serviceName "listComplianceSummaries" 


-- | <p>List all versions for a document.</p>
listDocumentVersions :: forall eff. ListDocumentVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListDocumentVersionsResult
listDocumentVersions = Request.request serviceName "listDocumentVersions" 


-- | <p>Describes one or more of your Systems Manager documents.</p>
listDocuments :: forall eff. ListDocumentsRequest -> Aff (exception :: EXCEPTION | eff) ListDocumentsResult
listDocuments = Request.request serviceName "listDocuments" 


-- | <p>A list of inventory items returned by the request.</p>
listInventoryEntries :: forall eff. ListInventoryEntriesRequest -> Aff (exception :: EXCEPTION | eff) ListInventoryEntriesResult
listInventoryEntries = Request.request serviceName "listInventoryEntries" 


-- | <p>Returns a resource-level summary count. The summary includes information about compliant and non-compliant statuses and detailed compliance-item severity counts, according to the filter criteria you specify.</p>
listResourceComplianceSummaries :: forall eff. ListResourceComplianceSummariesRequest -> Aff (exception :: EXCEPTION | eff) ListResourceComplianceSummariesResult
listResourceComplianceSummaries = Request.request serviceName "listResourceComplianceSummaries" 


-- | <p>Lists your resource data sync configurations. Includes information about the last time a sync attempted to start, the last sync status, and the last time a sync successfully completed.</p> <p>The number of sync configurations might be too large to return using a single call to <code>ListResourceDataSync</code>. You can limit the number of sync configurations returned by using the <code>MaxResults</code> parameter. To determine whether there are more sync configurations to list, check the value of <code>NextToken</code> in the output. If there are more sync configurations to list, you can request them by specifying the <code>NextToken</code> returned in the call to the parameter of a subsequent call. </p>
listResourceDataSync :: forall eff. ListResourceDataSyncRequest -> Aff (exception :: EXCEPTION | eff) ListResourceDataSyncResult
listResourceDataSync = Request.request serviceName "listResourceDataSync" 


-- | <p>Returns a list of the tags assigned to the specified resource.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (exception :: EXCEPTION | eff) ListTagsForResourceResult
listTagsForResource = Request.request serviceName "listTagsForResource" 


-- | <p>Shares a Systems Manager document publicly or privately. If you share a document privately, you must specify the AWS user account IDs for those people who can use the document. If you share a document publicly, you must specify <i>All</i> as the account ID.</p>
modifyDocumentPermission :: forall eff. ModifyDocumentPermissionRequest -> Aff (exception :: EXCEPTION | eff) ModifyDocumentPermissionResponse
modifyDocumentPermission = Request.request serviceName "modifyDocumentPermission" 


-- | <p>Registers a compliance type and other compliance details on a designated resource. This action lets you register custom compliance details with a resource. This call overwrites existing compliance information on the resource, so you must provide a full list of compliance items each time that you send the request.</p> <p>ComplianceType can be one of the following:</p> <ul> <li> <p>ExecutionId: The execution ID when the patch, association, or custom compliance item was applied.</p> </li> <li> <p>ExecutionType: Specify patch, association, or Custom:<code>string</code>.</p> </li> <li> <p>ExecutionTime. The time the patch, association, or custom compliance item was applied to the instance.</p> </li> <li> <p>Id: The patch, association, or custom compliance ID.</p> </li> <li> <p>Title: A title.</p> </li> <li> <p>Status: The status of the compliance item. For example, <code>approved</code> for patches, or <code>Failed</code> for associations.</p> </li> <li> <p>Severity: A patch severity. For example, <code>critical</code>.</p> </li> <li> <p>DocumentName: A SSM document name. For example, AWS-RunPatchBaseline.</p> </li> <li> <p>DocumentVersion: An SSM document version number. For example, 4.</p> </li> <li> <p>Classification: A patch classification. For example, <code>security updates</code>.</p> </li> <li> <p>PatchBaselineId: A patch baseline ID.</p> </li> <li> <p>PatchSeverity: A patch severity. For example, <code>Critical</code>.</p> </li> <li> <p>PatchState: A patch state. For example, <code>InstancesWithFailedPatches</code>.</p> </li> <li> <p>PatchGroup: The name of a patch group.</p> </li> <li> <p>InstalledTime: The time the association, patch, or custom compliance item was applied to the resource. Specify the time by using the following format: yyyy-MM-dd'T'HH:mm:ss'Z'</p> </li> </ul>
putComplianceItems :: forall eff. PutComplianceItemsRequest -> Aff (exception :: EXCEPTION | eff) PutComplianceItemsResult
putComplianceItems = Request.request serviceName "putComplianceItems" 


-- | <p>Bulk update custom inventory items on one more instance. The request adds an inventory item, if it doesn't already exist, or updates an inventory item, if it does exist.</p>
putInventory :: forall eff. PutInventoryRequest -> Aff (exception :: EXCEPTION | eff) PutInventoryResult
putInventory = Request.request serviceName "putInventory" 


-- | <p>Add one or more parameters to the system.</p>
putParameter :: forall eff. PutParameterRequest -> Aff (exception :: EXCEPTION | eff) PutParameterResult
putParameter = Request.request serviceName "putParameter" 


-- | <p>Defines the default patch baseline.</p>
registerDefaultPatchBaseline :: forall eff. RegisterDefaultPatchBaselineRequest -> Aff (exception :: EXCEPTION | eff) RegisterDefaultPatchBaselineResult
registerDefaultPatchBaseline = Request.request serviceName "registerDefaultPatchBaseline" 


-- | <p>Registers a patch baseline for a patch group.</p>
registerPatchBaselineForPatchGroup :: forall eff. RegisterPatchBaselineForPatchGroupRequest -> Aff (exception :: EXCEPTION | eff) RegisterPatchBaselineForPatchGroupResult
registerPatchBaselineForPatchGroup = Request.request serviceName "registerPatchBaselineForPatchGroup" 


-- | <p>Registers a target with a Maintenance Window.</p>
registerTargetWithMaintenanceWindow :: forall eff. RegisterTargetWithMaintenanceWindowRequest -> Aff (exception :: EXCEPTION | eff) RegisterTargetWithMaintenanceWindowResult
registerTargetWithMaintenanceWindow = Request.request serviceName "registerTargetWithMaintenanceWindow" 


-- | <p>Adds a new task to a Maintenance Window.</p>
registerTaskWithMaintenanceWindow :: forall eff. RegisterTaskWithMaintenanceWindowRequest -> Aff (exception :: EXCEPTION | eff) RegisterTaskWithMaintenanceWindowResult
registerTaskWithMaintenanceWindow = Request.request serviceName "registerTaskWithMaintenanceWindow" 


-- | <p>Removes all tags from the specified resource.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (exception :: EXCEPTION | eff) RemoveTagsFromResourceResult
removeTagsFromResource = Request.request serviceName "removeTagsFromResource" 


-- | <p>Sends a signal to an Automation execution to change the current behavior or status of the execution. </p>
sendAutomationSignal :: forall eff. SendAutomationSignalRequest -> Aff (exception :: EXCEPTION | eff) SendAutomationSignalResult
sendAutomationSignal = Request.request serviceName "sendAutomationSignal" 


-- | <p>Executes commands on one or more managed instances.</p>
sendCommand :: forall eff. SendCommandRequest -> Aff (exception :: EXCEPTION | eff) SendCommandResult
sendCommand = Request.request serviceName "sendCommand" 


-- | <p>Initiates execution of an Automation document.</p>
startAutomationExecution :: forall eff. StartAutomationExecutionRequest -> Aff (exception :: EXCEPTION | eff) StartAutomationExecutionResult
startAutomationExecution = Request.request serviceName "startAutomationExecution" 


-- | <p>Stop an Automation that is currently executing.</p>
stopAutomationExecution :: forall eff. StopAutomationExecutionRequest -> Aff (exception :: EXCEPTION | eff) StopAutomationExecutionResult
stopAutomationExecution = Request.request serviceName "stopAutomationExecution" 


-- | <p>Updates an association. You can update the association name and version, the document version, schedule, parameters, and Amazon S3 output.</p>
updateAssociation :: forall eff. UpdateAssociationRequest -> Aff (exception :: EXCEPTION | eff) UpdateAssociationResult
updateAssociation = Request.request serviceName "updateAssociation" 


-- | <p>Updates the status of the Systems Manager document associated with the specified instance.</p>
updateAssociationStatus :: forall eff. UpdateAssociationStatusRequest -> Aff (exception :: EXCEPTION | eff) UpdateAssociationStatusResult
updateAssociationStatus = Request.request serviceName "updateAssociationStatus" 


-- | <p>The document you want to update.</p>
updateDocument :: forall eff. UpdateDocumentRequest -> Aff (exception :: EXCEPTION | eff) UpdateDocumentResult
updateDocument = Request.request serviceName "updateDocument" 


-- | <p>Set the default version of a document. </p>
updateDocumentDefaultVersion :: forall eff. UpdateDocumentDefaultVersionRequest -> Aff (exception :: EXCEPTION | eff) UpdateDocumentDefaultVersionResult
updateDocumentDefaultVersion = Request.request serviceName "updateDocumentDefaultVersion" 


-- | <p>Updates an existing Maintenance Window. Only specified parameters are modified.</p>
updateMaintenanceWindow :: forall eff. UpdateMaintenanceWindowRequest -> Aff (exception :: EXCEPTION | eff) UpdateMaintenanceWindowResult
updateMaintenanceWindow = Request.request serviceName "updateMaintenanceWindow" 


-- | <p>Modifies the target of an existing Maintenance Window. You can't change the target type, but you can change the following:</p> <p>The target from being an ID target to a Tag target, or a Tag target to an ID target.</p> <p>IDs for an ID target.</p> <p>Tags for a Tag target.</p> <p>Owner.</p> <p>Name.</p> <p>Description.</p> <p>If a parameter is null, then the corresponding field is not modified.</p>
updateMaintenanceWindowTarget :: forall eff. UpdateMaintenanceWindowTargetRequest -> Aff (exception :: EXCEPTION | eff) UpdateMaintenanceWindowTargetResult
updateMaintenanceWindowTarget = Request.request serviceName "updateMaintenanceWindowTarget" 


-- | <p>Modifies a task assigned to a Maintenance Window. You can't change the task type, but you can change the following values:</p> <p>Task ARN. For example, you can change a RUN_COMMAND task from AWS-RunPowerShellScript to AWS-RunShellScript.</p> <p>Service role ARN.</p> <p>Task parameters.</p> <p>Task priority.</p> <p>Task MaxConcurrency and MaxErrors.</p> <p>Log location.</p> <p>If a parameter is null, then the corresponding field is not modified. Also, if you set Replace to true, then all fields required by the RegisterTaskWithMaintenanceWindow action are required for this request. Optional fields that aren't specified are set to null.</p>
updateMaintenanceWindowTask :: forall eff. UpdateMaintenanceWindowTaskRequest -> Aff (exception :: EXCEPTION | eff) UpdateMaintenanceWindowTaskResult
updateMaintenanceWindowTask = Request.request serviceName "updateMaintenanceWindowTask" 


-- | <p>Assigns or changes an Amazon Identity and Access Management (IAM) role to the managed instance.</p>
updateManagedInstanceRole :: forall eff. UpdateManagedInstanceRoleRequest -> Aff (exception :: EXCEPTION | eff) UpdateManagedInstanceRoleResult
updateManagedInstanceRole = Request.request serviceName "updateManagedInstanceRole" 


-- | <p>Modifies an existing patch baseline. Fields not specified in the request are left unchanged.</p> <note> <p>For information about valid key and value pairs in <code>PatchFilters</code> for each supported operating system type, see <a href="http://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PatchFilter.html">PatchFilter</a>.</p> </note>
updatePatchBaseline :: forall eff. UpdatePatchBaselineRequest -> Aff (exception :: EXCEPTION | eff) UpdatePatchBaselineResult
updatePatchBaseline = Request.request serviceName "updatePatchBaseline" 


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _
derive instance repGenericAccountId :: Generic AccountId _
instance showAccountId :: Show AccountId where
  show = genericShow
instance decodeAccountId :: Decode AccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountId :: Encode AccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountIdList = AccountIdList (Array AccountId)
derive instance newtypeAccountIdList :: Newtype AccountIdList _
derive instance repGenericAccountIdList :: Generic AccountIdList _
instance showAccountIdList :: Show AccountIdList where
  show = genericShow
instance decodeAccountIdList :: Decode AccountIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountIdList :: Encode AccountIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An activation registers one or more on-premises servers or virtual machines (VMs) with AWS so that you can configure those servers or VMs using Run Command. A server or VM that has been registered with AWS is called a managed instance.</p>
newtype Activation = Activation 
  { "ActivationId" :: NullOrUndefined.NullOrUndefined (ActivationId)
  , "Description" :: NullOrUndefined.NullOrUndefined (ActivationDescription)
  , "DefaultInstanceName" :: NullOrUndefined.NullOrUndefined (DefaultInstanceName)
  , "IamRole" :: NullOrUndefined.NullOrUndefined (IamRole)
  , "RegistrationLimit" :: NullOrUndefined.NullOrUndefined (RegistrationLimit)
  , "RegistrationsCount" :: NullOrUndefined.NullOrUndefined (RegistrationsCount)
  , "ExpirationDate" :: NullOrUndefined.NullOrUndefined (ExpirationDate)
  , "Expired" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "CreatedDate" :: NullOrUndefined.NullOrUndefined (CreatedDate)
  }
derive instance newtypeActivation :: Newtype Activation _
derive instance repGenericActivation :: Generic Activation _
instance showActivation :: Show Activation where
  show = genericShow
instance decodeActivation :: Decode Activation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivation :: Encode Activation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivationCode = ActivationCode String
derive instance newtypeActivationCode :: Newtype ActivationCode _
derive instance repGenericActivationCode :: Generic ActivationCode _
instance showActivationCode :: Show ActivationCode where
  show = genericShow
instance decodeActivationCode :: Decode ActivationCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivationCode :: Encode ActivationCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivationDescription = ActivationDescription String
derive instance newtypeActivationDescription :: Newtype ActivationDescription _
derive instance repGenericActivationDescription :: Generic ActivationDescription _
instance showActivationDescription :: Show ActivationDescription where
  show = genericShow
instance decodeActivationDescription :: Decode ActivationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivationDescription :: Encode ActivationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivationId = ActivationId String
derive instance newtypeActivationId :: Newtype ActivationId _
derive instance repGenericActivationId :: Generic ActivationId _
instance showActivationId :: Show ActivationId where
  show = genericShow
instance decodeActivationId :: Decode ActivationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivationId :: Encode ActivationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivationList = ActivationList (Array Activation)
derive instance newtypeActivationList :: Newtype ActivationList _
derive instance repGenericActivationList :: Generic ActivationList _
instance showActivationList :: Show ActivationList where
  show = genericShow
instance decodeActivationList :: Decode ActivationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivationList :: Encode ActivationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddTagsToResourceRequest = AddTagsToResourceRequest 
  { "ResourceType" :: (ResourceTypeForTagging)
  , "ResourceId" :: (ResourceId)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsToResourceRequest :: Newtype AddTagsToResourceRequest _
derive instance repGenericAddTagsToResourceRequest :: Generic AddTagsToResourceRequest _
instance showAddTagsToResourceRequest :: Show AddTagsToResourceRequest where
  show = genericShow
instance decodeAddTagsToResourceRequest :: Decode AddTagsToResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceRequest :: Encode AddTagsToResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddTagsToResourceResult = AddTagsToResourceResult Types.NoArguments
derive instance newtypeAddTagsToResourceResult :: Newtype AddTagsToResourceResult _
derive instance repGenericAddTagsToResourceResult :: Generic AddTagsToResourceResult _
instance showAddTagsToResourceResult :: Show AddTagsToResourceResult where
  show = genericShow
instance decodeAddTagsToResourceResult :: Decode AddTagsToResourceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceResult :: Encode AddTagsToResourceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AgentErrorCode = AgentErrorCode String
derive instance newtypeAgentErrorCode :: Newtype AgentErrorCode _
derive instance repGenericAgentErrorCode :: Generic AgentErrorCode _
instance showAgentErrorCode :: Show AgentErrorCode where
  show = genericShow
instance decodeAgentErrorCode :: Decode AgentErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentErrorCode :: Encode AgentErrorCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AggregatorSchemaOnly = AggregatorSchemaOnly Boolean
derive instance newtypeAggregatorSchemaOnly :: Newtype AggregatorSchemaOnly _
derive instance repGenericAggregatorSchemaOnly :: Generic AggregatorSchemaOnly _
instance showAggregatorSchemaOnly :: Show AggregatorSchemaOnly where
  show = genericShow
instance decodeAggregatorSchemaOnly :: Decode AggregatorSchemaOnly where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAggregatorSchemaOnly :: Encode AggregatorSchemaOnly where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedPattern = AllowedPattern String
derive instance newtypeAllowedPattern :: Newtype AllowedPattern _
derive instance repGenericAllowedPattern :: Generic AllowedPattern _
instance showAllowedPattern :: Show AllowedPattern where
  show = genericShow
instance decodeAllowedPattern :: Decode AllowedPattern where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedPattern :: Encode AllowedPattern where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Error returned if an attempt is made to register a patch group with a patch baseline that is already registered with a different patch baseline.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAlreadyExistsException :: Newtype AlreadyExistsException _
derive instance repGenericAlreadyExistsException :: Generic AlreadyExistsException _
instance showAlreadyExistsException :: Show AlreadyExistsException where
  show = genericShow
instance decodeAlreadyExistsException :: Decode AlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlreadyExistsException :: Encode AlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApproveAfterDays = ApproveAfterDays Int
derive instance newtypeApproveAfterDays :: Newtype ApproveAfterDays _
derive instance repGenericApproveAfterDays :: Generic ApproveAfterDays _
instance showApproveAfterDays :: Show ApproveAfterDays where
  show = genericShow
instance decodeApproveAfterDays :: Decode ApproveAfterDays where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApproveAfterDays :: Encode ApproveAfterDays where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You must disassociate a document from all instances before you can delete it.</p>
newtype AssociatedInstances = AssociatedInstances Types.NoArguments
derive instance newtypeAssociatedInstances :: Newtype AssociatedInstances _
derive instance repGenericAssociatedInstances :: Generic AssociatedInstances _
instance showAssociatedInstances :: Show AssociatedInstances where
  show = genericShow
instance decodeAssociatedInstances :: Decode AssociatedInstances where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociatedInstances :: Encode AssociatedInstances where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an association of a Systems Manager document and an instance.</p>
newtype Association = Association 
  { "Name" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "AssociationId" :: NullOrUndefined.NullOrUndefined (AssociationId)
  , "AssociationVersion" :: NullOrUndefined.NullOrUndefined (AssociationVersion)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "LastExecutionDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Overview" :: NullOrUndefined.NullOrUndefined (AssociationOverview)
  , "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (ScheduleExpression)
  , "AssociationName" :: NullOrUndefined.NullOrUndefined (AssociationName)
  }
derive instance newtypeAssociation :: Newtype Association _
derive instance repGenericAssociation :: Generic Association _
instance showAssociation :: Show Association where
  show = genericShow
instance decodeAssociation :: Decode Association where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociation :: Encode Association where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified association already exists.</p>
newtype AssociationAlreadyExists = AssociationAlreadyExists Types.NoArguments
derive instance newtypeAssociationAlreadyExists :: Newtype AssociationAlreadyExists _
derive instance repGenericAssociationAlreadyExists :: Generic AssociationAlreadyExists _
instance showAssociationAlreadyExists :: Show AssociationAlreadyExists where
  show = genericShow
instance decodeAssociationAlreadyExists :: Decode AssociationAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationAlreadyExists :: Encode AssociationAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the parameters for a document.</p>
newtype AssociationDescription = AssociationDescription 
  { "Name" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "AssociationVersion" :: NullOrUndefined.NullOrUndefined (AssociationVersion)
  , "Date" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "LastUpdateAssociationDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined.NullOrUndefined (AssociationStatus)
  , "Overview" :: NullOrUndefined.NullOrUndefined (AssociationOverview)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "AssociationId" :: NullOrUndefined.NullOrUndefined (AssociationId)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined.NullOrUndefined (InstanceAssociationOutputLocation)
  , "LastExecutionDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "LastSuccessfulExecutionDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "AssociationName" :: NullOrUndefined.NullOrUndefined (AssociationName)
  }
derive instance newtypeAssociationDescription :: Newtype AssociationDescription _
derive instance repGenericAssociationDescription :: Generic AssociationDescription _
instance showAssociationDescription :: Show AssociationDescription where
  show = genericShow
instance decodeAssociationDescription :: Decode AssociationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationDescription :: Encode AssociationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationDescriptionList = AssociationDescriptionList (Array AssociationDescription)
derive instance newtypeAssociationDescriptionList :: Newtype AssociationDescriptionList _
derive instance repGenericAssociationDescriptionList :: Generic AssociationDescriptionList _
instance showAssociationDescriptionList :: Show AssociationDescriptionList where
  show = genericShow
instance decodeAssociationDescriptionList :: Decode AssociationDescriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationDescriptionList :: Encode AssociationDescriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified association does not exist.</p>
newtype AssociationDoesNotExist = AssociationDoesNotExist 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAssociationDoesNotExist :: Newtype AssociationDoesNotExist _
derive instance repGenericAssociationDoesNotExist :: Generic AssociationDoesNotExist _
instance showAssociationDoesNotExist :: Show AssociationDoesNotExist where
  show = genericShow
instance decodeAssociationDoesNotExist :: Decode AssociationDoesNotExist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationDoesNotExist :: Encode AssociationDoesNotExist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a filter.</p>
newtype AssociationFilter = AssociationFilter 
  { "Key'" :: (AssociationFilterKey)
  , "Value'" :: (AssociationFilterValue)
  }
derive instance newtypeAssociationFilter :: Newtype AssociationFilter _
derive instance repGenericAssociationFilter :: Generic AssociationFilter _
instance showAssociationFilter :: Show AssociationFilter where
  show = genericShow
instance decodeAssociationFilter :: Decode AssociationFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationFilter :: Encode AssociationFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationFilterKey = AssociationFilterKey String
derive instance newtypeAssociationFilterKey :: Newtype AssociationFilterKey _
derive instance repGenericAssociationFilterKey :: Generic AssociationFilterKey _
instance showAssociationFilterKey :: Show AssociationFilterKey where
  show = genericShow
instance decodeAssociationFilterKey :: Decode AssociationFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationFilterKey :: Encode AssociationFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationFilterList = AssociationFilterList (Array AssociationFilter)
derive instance newtypeAssociationFilterList :: Newtype AssociationFilterList _
derive instance repGenericAssociationFilterList :: Generic AssociationFilterList _
instance showAssociationFilterList :: Show AssociationFilterList where
  show = genericShow
instance decodeAssociationFilterList :: Decode AssociationFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationFilterList :: Encode AssociationFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationFilterValue = AssociationFilterValue String
derive instance newtypeAssociationFilterValue :: Newtype AssociationFilterValue _
derive instance repGenericAssociationFilterValue :: Generic AssociationFilterValue _
instance showAssociationFilterValue :: Show AssociationFilterValue where
  show = genericShow
instance decodeAssociationFilterValue :: Decode AssociationFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationFilterValue :: Encode AssociationFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationId = AssociationId String
derive instance newtypeAssociationId :: Newtype AssociationId _
derive instance repGenericAssociationId :: Generic AssociationId _
instance showAssociationId :: Show AssociationId where
  show = genericShow
instance decodeAssociationId :: Decode AssociationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationId :: Encode AssociationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can have at most 2,000 active associations.</p>
newtype AssociationLimitExceeded = AssociationLimitExceeded Types.NoArguments
derive instance newtypeAssociationLimitExceeded :: Newtype AssociationLimitExceeded _
derive instance repGenericAssociationLimitExceeded :: Generic AssociationLimitExceeded _
instance showAssociationLimitExceeded :: Show AssociationLimitExceeded where
  show = genericShow
instance decodeAssociationLimitExceeded :: Decode AssociationLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationLimitExceeded :: Encode AssociationLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationList = AssociationList (Array Association)
derive instance newtypeAssociationList :: Newtype AssociationList _
derive instance repGenericAssociationList :: Generic AssociationList _
instance showAssociationList :: Show AssociationList where
  show = genericShow
instance decodeAssociationList :: Decode AssociationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationList :: Encode AssociationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationName = AssociationName String
derive instance newtypeAssociationName :: Newtype AssociationName _
derive instance repGenericAssociationName :: Generic AssociationName _
instance showAssociationName :: Show AssociationName where
  show = genericShow
instance decodeAssociationName :: Decode AssociationName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationName :: Encode AssociationName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the association.</p>
newtype AssociationOverview = AssociationOverview 
  { "Status" :: NullOrUndefined.NullOrUndefined (StatusName)
  , "DetailedStatus" :: NullOrUndefined.NullOrUndefined (StatusName)
  , "AssociationStatusAggregatedCount" :: NullOrUndefined.NullOrUndefined (AssociationStatusAggregatedCount)
  }
derive instance newtypeAssociationOverview :: Newtype AssociationOverview _
derive instance repGenericAssociationOverview :: Generic AssociationOverview _
instance showAssociationOverview :: Show AssociationOverview where
  show = genericShow
instance decodeAssociationOverview :: Decode AssociationOverview where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationOverview :: Encode AssociationOverview where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an association status.</p>
newtype AssociationStatus = AssociationStatus 
  { "Date" :: (DateTime)
  , "Name" :: (AssociationStatusName)
  , "Message" :: (StatusMessage)
  , "AdditionalInfo" :: NullOrUndefined.NullOrUndefined (StatusAdditionalInfo)
  }
derive instance newtypeAssociationStatus :: Newtype AssociationStatus _
derive instance repGenericAssociationStatus :: Generic AssociationStatus _
instance showAssociationStatus :: Show AssociationStatus where
  show = genericShow
instance decodeAssociationStatus :: Decode AssociationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationStatus :: Encode AssociationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationStatusAggregatedCount = AssociationStatusAggregatedCount (StrMap.StrMap InstanceCount)
derive instance newtypeAssociationStatusAggregatedCount :: Newtype AssociationStatusAggregatedCount _
derive instance repGenericAssociationStatusAggregatedCount :: Generic AssociationStatusAggregatedCount _
instance showAssociationStatusAggregatedCount :: Show AssociationStatusAggregatedCount where
  show = genericShow
instance decodeAssociationStatusAggregatedCount :: Decode AssociationStatusAggregatedCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationStatusAggregatedCount :: Encode AssociationStatusAggregatedCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationStatusName = AssociationStatusName String
derive instance newtypeAssociationStatusName :: Newtype AssociationStatusName _
derive instance repGenericAssociationStatusName :: Generic AssociationStatusName _
instance showAssociationStatusName :: Show AssociationStatusName where
  show = genericShow
instance decodeAssociationStatusName :: Decode AssociationStatusName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationStatusName :: Encode AssociationStatusName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationVersion = AssociationVersion String
derive instance newtypeAssociationVersion :: Newtype AssociationVersion _
derive instance repGenericAssociationVersion :: Generic AssociationVersion _
instance showAssociationVersion :: Show AssociationVersion where
  show = genericShow
instance decodeAssociationVersion :: Decode AssociationVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationVersion :: Encode AssociationVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the association version.</p>
newtype AssociationVersionInfo = AssociationVersionInfo 
  { "AssociationId" :: NullOrUndefined.NullOrUndefined (AssociationId)
  , "AssociationVersion" :: NullOrUndefined.NullOrUndefined (AssociationVersion)
  , "CreatedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Name" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined.NullOrUndefined (InstanceAssociationOutputLocation)
  , "AssociationName" :: NullOrUndefined.NullOrUndefined (AssociationName)
  }
derive instance newtypeAssociationVersionInfo :: Newtype AssociationVersionInfo _
derive instance repGenericAssociationVersionInfo :: Generic AssociationVersionInfo _
instance showAssociationVersionInfo :: Show AssociationVersionInfo where
  show = genericShow
instance decodeAssociationVersionInfo :: Decode AssociationVersionInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationVersionInfo :: Encode AssociationVersionInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have reached the maximum number versions allowed for an association. Each association has a limit of 1,000 versions. </p>
newtype AssociationVersionLimitExceeded = AssociationVersionLimitExceeded 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAssociationVersionLimitExceeded :: Newtype AssociationVersionLimitExceeded _
derive instance repGenericAssociationVersionLimitExceeded :: Generic AssociationVersionLimitExceeded _
instance showAssociationVersionLimitExceeded :: Show AssociationVersionLimitExceeded where
  show = genericShow
instance decodeAssociationVersionLimitExceeded :: Decode AssociationVersionLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationVersionLimitExceeded :: Encode AssociationVersionLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociationVersionList = AssociationVersionList (Array AssociationVersionInfo)
derive instance newtypeAssociationVersionList :: Newtype AssociationVersionList _
derive instance repGenericAssociationVersionList :: Generic AssociationVersionList _
instance showAssociationVersionList :: Show AssociationVersionList where
  show = genericShow
instance decodeAssociationVersionList :: Decode AssociationVersionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociationVersionList :: Encode AssociationVersionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _
derive instance repGenericAttributeName :: Generic AttributeName _
instance showAttributeName :: Show AttributeName where
  show = genericShow
instance decodeAttributeName :: Decode AttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeName :: Encode AttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationActionName = AutomationActionName String
derive instance newtypeAutomationActionName :: Newtype AutomationActionName _
derive instance repGenericAutomationActionName :: Generic AutomationActionName _
instance showAutomationActionName :: Show AutomationActionName where
  show = genericShow
instance decodeAutomationActionName :: Decode AutomationActionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationActionName :: Encode AutomationActionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Automation document with the specified name could not be found.</p>
newtype AutomationDefinitionNotFoundException = AutomationDefinitionNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAutomationDefinitionNotFoundException :: Newtype AutomationDefinitionNotFoundException _
derive instance repGenericAutomationDefinitionNotFoundException :: Generic AutomationDefinitionNotFoundException _
instance showAutomationDefinitionNotFoundException :: Show AutomationDefinitionNotFoundException where
  show = genericShow
instance decodeAutomationDefinitionNotFoundException :: Decode AutomationDefinitionNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationDefinitionNotFoundException :: Encode AutomationDefinitionNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Automation document with the specified name and version could not be found.</p>
newtype AutomationDefinitionVersionNotFoundException = AutomationDefinitionVersionNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAutomationDefinitionVersionNotFoundException :: Newtype AutomationDefinitionVersionNotFoundException _
derive instance repGenericAutomationDefinitionVersionNotFoundException :: Generic AutomationDefinitionVersionNotFoundException _
instance showAutomationDefinitionVersionNotFoundException :: Show AutomationDefinitionVersionNotFoundException where
  show = genericShow
instance decodeAutomationDefinitionVersionNotFoundException :: Decode AutomationDefinitionVersionNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationDefinitionVersionNotFoundException :: Encode AutomationDefinitionVersionNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed information about the current state of an individual Automation execution.</p>
newtype AutomationExecution = AutomationExecution 
  { "AutomationExecutionId" :: NullOrUndefined.NullOrUndefined (AutomationExecutionId)
  , "DocumentName" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "ExecutionStartTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "ExecutionEndTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "AutomationExecutionStatus" :: NullOrUndefined.NullOrUndefined (AutomationExecutionStatus)
  , "StepExecutions" :: NullOrUndefined.NullOrUndefined (StepExecutionList)
  , "StepExecutionsTruncated" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (AutomationParameterMap)
  , "Outputs" :: NullOrUndefined.NullOrUndefined (AutomationParameterMap)
  , "FailureMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "Mode" :: NullOrUndefined.NullOrUndefined (ExecutionMode)
  , "ParentAutomationExecutionId" :: NullOrUndefined.NullOrUndefined (AutomationExecutionId)
  , "ExecutedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "CurrentStepName" :: NullOrUndefined.NullOrUndefined (String)
  , "CurrentAction" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetParameterName" :: NullOrUndefined.NullOrUndefined (AutomationParameterKey)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "ResolvedTargets" :: NullOrUndefined.NullOrUndefined (ResolvedTargets)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  , "Target" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAutomationExecution :: Newtype AutomationExecution _
derive instance repGenericAutomationExecution :: Generic AutomationExecution _
instance showAutomationExecution :: Show AutomationExecution where
  show = genericShow
instance decodeAutomationExecution :: Decode AutomationExecution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecution :: Encode AutomationExecution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A filter used to match specific automation executions. This is used to limit the scope of Automation execution information returned.</p>
newtype AutomationExecutionFilter = AutomationExecutionFilter 
  { "Key" :: (AutomationExecutionFilterKey)
  , "Values" :: (AutomationExecutionFilterValueList)
  }
derive instance newtypeAutomationExecutionFilter :: Newtype AutomationExecutionFilter _
derive instance repGenericAutomationExecutionFilter :: Generic AutomationExecutionFilter _
instance showAutomationExecutionFilter :: Show AutomationExecutionFilter where
  show = genericShow
instance decodeAutomationExecutionFilter :: Decode AutomationExecutionFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionFilter :: Encode AutomationExecutionFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationExecutionFilterKey = AutomationExecutionFilterKey String
derive instance newtypeAutomationExecutionFilterKey :: Newtype AutomationExecutionFilterKey _
derive instance repGenericAutomationExecutionFilterKey :: Generic AutomationExecutionFilterKey _
instance showAutomationExecutionFilterKey :: Show AutomationExecutionFilterKey where
  show = genericShow
instance decodeAutomationExecutionFilterKey :: Decode AutomationExecutionFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionFilterKey :: Encode AutomationExecutionFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationExecutionFilterList = AutomationExecutionFilterList (Array AutomationExecutionFilter)
derive instance newtypeAutomationExecutionFilterList :: Newtype AutomationExecutionFilterList _
derive instance repGenericAutomationExecutionFilterList :: Generic AutomationExecutionFilterList _
instance showAutomationExecutionFilterList :: Show AutomationExecutionFilterList where
  show = genericShow
instance decodeAutomationExecutionFilterList :: Decode AutomationExecutionFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionFilterList :: Encode AutomationExecutionFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationExecutionFilterValue = AutomationExecutionFilterValue String
derive instance newtypeAutomationExecutionFilterValue :: Newtype AutomationExecutionFilterValue _
derive instance repGenericAutomationExecutionFilterValue :: Generic AutomationExecutionFilterValue _
instance showAutomationExecutionFilterValue :: Show AutomationExecutionFilterValue where
  show = genericShow
instance decodeAutomationExecutionFilterValue :: Decode AutomationExecutionFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionFilterValue :: Encode AutomationExecutionFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationExecutionFilterValueList = AutomationExecutionFilterValueList (Array AutomationExecutionFilterValue)
derive instance newtypeAutomationExecutionFilterValueList :: Newtype AutomationExecutionFilterValueList _
derive instance repGenericAutomationExecutionFilterValueList :: Generic AutomationExecutionFilterValueList _
instance showAutomationExecutionFilterValueList :: Show AutomationExecutionFilterValueList where
  show = genericShow
instance decodeAutomationExecutionFilterValueList :: Decode AutomationExecutionFilterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionFilterValueList :: Encode AutomationExecutionFilterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationExecutionId = AutomationExecutionId String
derive instance newtypeAutomationExecutionId :: Newtype AutomationExecutionId _
derive instance repGenericAutomationExecutionId :: Generic AutomationExecutionId _
instance showAutomationExecutionId :: Show AutomationExecutionId where
  show = genericShow
instance decodeAutomationExecutionId :: Decode AutomationExecutionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionId :: Encode AutomationExecutionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of simultaneously running Automation executions exceeded the allowable limit.</p>
newtype AutomationExecutionLimitExceededException = AutomationExecutionLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAutomationExecutionLimitExceededException :: Newtype AutomationExecutionLimitExceededException _
derive instance repGenericAutomationExecutionLimitExceededException :: Generic AutomationExecutionLimitExceededException _
instance showAutomationExecutionLimitExceededException :: Show AutomationExecutionLimitExceededException where
  show = genericShow
instance decodeAutomationExecutionLimitExceededException :: Decode AutomationExecutionLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionLimitExceededException :: Encode AutomationExecutionLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details about a specific Automation execution.</p>
newtype AutomationExecutionMetadata = AutomationExecutionMetadata 
  { "AutomationExecutionId" :: NullOrUndefined.NullOrUndefined (AutomationExecutionId)
  , "DocumentName" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "AutomationExecutionStatus" :: NullOrUndefined.NullOrUndefined (AutomationExecutionStatus)
  , "ExecutionStartTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "ExecutionEndTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "ExecutedBy" :: NullOrUndefined.NullOrUndefined (String)
  , "LogFile" :: NullOrUndefined.NullOrUndefined (String)
  , "Outputs" :: NullOrUndefined.NullOrUndefined (AutomationParameterMap)
  , "Mode" :: NullOrUndefined.NullOrUndefined (ExecutionMode)
  , "ParentAutomationExecutionId" :: NullOrUndefined.NullOrUndefined (AutomationExecutionId)
  , "CurrentStepName" :: NullOrUndefined.NullOrUndefined (String)
  , "CurrentAction" :: NullOrUndefined.NullOrUndefined (String)
  , "FailureMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetParameterName" :: NullOrUndefined.NullOrUndefined (AutomationParameterKey)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "ResolvedTargets" :: NullOrUndefined.NullOrUndefined (ResolvedTargets)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  , "Target" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAutomationExecutionMetadata :: Newtype AutomationExecutionMetadata _
derive instance repGenericAutomationExecutionMetadata :: Generic AutomationExecutionMetadata _
instance showAutomationExecutionMetadata :: Show AutomationExecutionMetadata where
  show = genericShow
instance decodeAutomationExecutionMetadata :: Decode AutomationExecutionMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionMetadata :: Encode AutomationExecutionMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationExecutionMetadataList = AutomationExecutionMetadataList (Array AutomationExecutionMetadata)
derive instance newtypeAutomationExecutionMetadataList :: Newtype AutomationExecutionMetadataList _
derive instance repGenericAutomationExecutionMetadataList :: Generic AutomationExecutionMetadataList _
instance showAutomationExecutionMetadataList :: Show AutomationExecutionMetadataList where
  show = genericShow
instance decodeAutomationExecutionMetadataList :: Decode AutomationExecutionMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionMetadataList :: Encode AutomationExecutionMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is no automation execution information for the requested automation execution ID.</p>
newtype AutomationExecutionNotFoundException = AutomationExecutionNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAutomationExecutionNotFoundException :: Newtype AutomationExecutionNotFoundException _
derive instance repGenericAutomationExecutionNotFoundException :: Generic AutomationExecutionNotFoundException _
instance showAutomationExecutionNotFoundException :: Show AutomationExecutionNotFoundException where
  show = genericShow
instance decodeAutomationExecutionNotFoundException :: Decode AutomationExecutionNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionNotFoundException :: Encode AutomationExecutionNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationExecutionStatus = AutomationExecutionStatus String
derive instance newtypeAutomationExecutionStatus :: Newtype AutomationExecutionStatus _
derive instance repGenericAutomationExecutionStatus :: Generic AutomationExecutionStatus _
instance showAutomationExecutionStatus :: Show AutomationExecutionStatus where
  show = genericShow
instance decodeAutomationExecutionStatus :: Decode AutomationExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationExecutionStatus :: Encode AutomationExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationParameterKey = AutomationParameterKey String
derive instance newtypeAutomationParameterKey :: Newtype AutomationParameterKey _
derive instance repGenericAutomationParameterKey :: Generic AutomationParameterKey _
instance showAutomationParameterKey :: Show AutomationParameterKey where
  show = genericShow
instance decodeAutomationParameterKey :: Decode AutomationParameterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationParameterKey :: Encode AutomationParameterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationParameterMap = AutomationParameterMap (StrMap.StrMap AutomationParameterValueList)
derive instance newtypeAutomationParameterMap :: Newtype AutomationParameterMap _
derive instance repGenericAutomationParameterMap :: Generic AutomationParameterMap _
instance showAutomationParameterMap :: Show AutomationParameterMap where
  show = genericShow
instance decodeAutomationParameterMap :: Decode AutomationParameterMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationParameterMap :: Encode AutomationParameterMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationParameterValue = AutomationParameterValue String
derive instance newtypeAutomationParameterValue :: Newtype AutomationParameterValue _
derive instance repGenericAutomationParameterValue :: Generic AutomationParameterValue _
instance showAutomationParameterValue :: Show AutomationParameterValue where
  show = genericShow
instance decodeAutomationParameterValue :: Decode AutomationParameterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationParameterValue :: Encode AutomationParameterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutomationParameterValueList = AutomationParameterValueList (Array AutomationParameterValue)
derive instance newtypeAutomationParameterValueList :: Newtype AutomationParameterValueList _
derive instance repGenericAutomationParameterValueList :: Generic AutomationParameterValueList _
instance showAutomationParameterValueList :: Show AutomationParameterValueList where
  show = genericShow
instance decodeAutomationParameterValueList :: Decode AutomationParameterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationParameterValueList :: Encode AutomationParameterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified step name and execution ID don't exist. Verify the information and try again.</p>
newtype AutomationStepNotFoundException = AutomationStepNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAutomationStepNotFoundException :: Newtype AutomationStepNotFoundException _
derive instance repGenericAutomationStepNotFoundException :: Generic AutomationStepNotFoundException _
instance showAutomationStepNotFoundException :: Show AutomationStepNotFoundException where
  show = genericShow
instance decodeAutomationStepNotFoundException :: Decode AutomationStepNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutomationStepNotFoundException :: Encode AutomationStepNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BaselineDescription = BaselineDescription String
derive instance newtypeBaselineDescription :: Newtype BaselineDescription _
derive instance repGenericBaselineDescription :: Generic BaselineDescription _
instance showBaselineDescription :: Show BaselineDescription where
  show = genericShow
instance decodeBaselineDescription :: Decode BaselineDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBaselineDescription :: Encode BaselineDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BaselineId = BaselineId String
derive instance newtypeBaselineId :: Newtype BaselineId _
derive instance repGenericBaselineId :: Generic BaselineId _
instance showBaselineId :: Show BaselineId where
  show = genericShow
instance decodeBaselineId :: Decode BaselineId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBaselineId :: Encode BaselineId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BaselineName = BaselineName String
derive instance newtypeBaselineName :: Newtype BaselineName _
derive instance repGenericBaselineName :: Generic BaselineName _
instance showBaselineName :: Show BaselineName where
  show = genericShow
instance decodeBaselineName :: Decode BaselineName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBaselineName :: Encode BaselineName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchErrorMessage = BatchErrorMessage String
derive instance newtypeBatchErrorMessage :: Newtype BatchErrorMessage _
derive instance repGenericBatchErrorMessage :: Generic BatchErrorMessage _
instance showBatchErrorMessage :: Show BatchErrorMessage where
  show = genericShow
instance decodeBatchErrorMessage :: Decode BatchErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchErrorMessage :: Encode BatchErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CancelCommandRequest = CancelCommandRequest 
  { "CommandId" :: (CommandId)
  , "InstanceIds" :: NullOrUndefined.NullOrUndefined (InstanceIdList)
  }
derive instance newtypeCancelCommandRequest :: Newtype CancelCommandRequest _
derive instance repGenericCancelCommandRequest :: Generic CancelCommandRequest _
instance showCancelCommandRequest :: Show CancelCommandRequest where
  show = genericShow
instance decodeCancelCommandRequest :: Decode CancelCommandRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelCommandRequest :: Encode CancelCommandRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Whether or not the command was successfully canceled. There is no guarantee that a request can be canceled.</p>
newtype CancelCommandResult = CancelCommandResult Types.NoArguments
derive instance newtypeCancelCommandResult :: Newtype CancelCommandResult _
derive instance repGenericCancelCommandResult :: Generic CancelCommandResult _
instance showCancelCommandResult :: Show CancelCommandResult where
  show = genericShow
instance decodeCancelCommandResult :: Decode CancelCommandResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelCommandResult :: Encode CancelCommandResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientToken = ClientToken String
derive instance newtypeClientToken :: Newtype ClientToken _
derive instance repGenericClientToken :: Generic ClientToken _
instance showClientToken :: Show ClientToken where
  show = genericShow
instance decodeClientToken :: Decode ClientToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientToken :: Encode ClientToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a command request.</p>
newtype Command = Command 
  { "CommandId" :: NullOrUndefined.NullOrUndefined (CommandId)
  , "DocumentName" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "Comment" :: NullOrUndefined.NullOrUndefined (Comment)
  , "ExpiresAfter" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "InstanceIds" :: NullOrUndefined.NullOrUndefined (InstanceIdList)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "RequestedDateTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined.NullOrUndefined (CommandStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (StatusDetails)
  , "OutputS3Region" :: NullOrUndefined.NullOrUndefined (S3Region)
  , "OutputS3BucketName" :: NullOrUndefined.NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined.NullOrUndefined (S3KeyPrefix)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  , "TargetCount" :: NullOrUndefined.NullOrUndefined (TargetCount)
  , "CompletedCount" :: NullOrUndefined.NullOrUndefined (CompletedCount)
  , "ErrorCount" :: NullOrUndefined.NullOrUndefined (ErrorCount)
  , "ServiceRole" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "NotificationConfig" :: NullOrUndefined.NullOrUndefined (NotificationConfig)
  }
derive instance newtypeCommand :: Newtype Command _
derive instance repGenericCommand :: Generic Command _
instance showCommand :: Show Command where
  show = genericShow
instance decodeCommand :: Decode Command where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommand :: Encode Command where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a command filter.</p>
newtype CommandFilter = CommandFilter 
  { "Key'" :: (CommandFilterKey)
  , "Value'" :: (CommandFilterValue)
  }
derive instance newtypeCommandFilter :: Newtype CommandFilter _
derive instance repGenericCommandFilter :: Generic CommandFilter _
instance showCommandFilter :: Show CommandFilter where
  show = genericShow
instance decodeCommandFilter :: Decode CommandFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandFilter :: Encode CommandFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandFilterKey = CommandFilterKey String
derive instance newtypeCommandFilterKey :: Newtype CommandFilterKey _
derive instance repGenericCommandFilterKey :: Generic CommandFilterKey _
instance showCommandFilterKey :: Show CommandFilterKey where
  show = genericShow
instance decodeCommandFilterKey :: Decode CommandFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandFilterKey :: Encode CommandFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandFilterList = CommandFilterList (Array CommandFilter)
derive instance newtypeCommandFilterList :: Newtype CommandFilterList _
derive instance repGenericCommandFilterList :: Generic CommandFilterList _
instance showCommandFilterList :: Show CommandFilterList where
  show = genericShow
instance decodeCommandFilterList :: Decode CommandFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandFilterList :: Encode CommandFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandFilterValue = CommandFilterValue String
derive instance newtypeCommandFilterValue :: Newtype CommandFilterValue _
derive instance repGenericCommandFilterValue :: Generic CommandFilterValue _
instance showCommandFilterValue :: Show CommandFilterValue where
  show = genericShow
instance decodeCommandFilterValue :: Decode CommandFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandFilterValue :: Encode CommandFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandId = CommandId String
derive instance newtypeCommandId :: Newtype CommandId _
derive instance repGenericCommandId :: Generic CommandId _
instance showCommandId :: Show CommandId where
  show = genericShow
instance decodeCommandId :: Decode CommandId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandId :: Encode CommandId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. A command invocation returns status and detail information about a command you executed. </p>
newtype CommandInvocation = CommandInvocation 
  { "CommandId" :: NullOrUndefined.NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "InstanceName" :: NullOrUndefined.NullOrUndefined (InstanceTagName)
  , "Comment" :: NullOrUndefined.NullOrUndefined (Comment)
  , "DocumentName" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "RequestedDateTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined.NullOrUndefined (CommandInvocationStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (StatusDetails)
  , "TraceOutput" :: NullOrUndefined.NullOrUndefined (InvocationTraceOutput)
  , "StandardOutputUrl" :: NullOrUndefined.NullOrUndefined (Url)
  , "StandardErrorUrl" :: NullOrUndefined.NullOrUndefined (Url)
  , "CommandPlugins" :: NullOrUndefined.NullOrUndefined (CommandPluginList)
  , "ServiceRole" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "NotificationConfig" :: NullOrUndefined.NullOrUndefined (NotificationConfig)
  }
derive instance newtypeCommandInvocation :: Newtype CommandInvocation _
derive instance repGenericCommandInvocation :: Generic CommandInvocation _
instance showCommandInvocation :: Show CommandInvocation where
  show = genericShow
instance decodeCommandInvocation :: Decode CommandInvocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandInvocation :: Encode CommandInvocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandInvocationList = CommandInvocationList (Array CommandInvocation)
derive instance newtypeCommandInvocationList :: Newtype CommandInvocationList _
derive instance repGenericCommandInvocationList :: Generic CommandInvocationList _
instance showCommandInvocationList :: Show CommandInvocationList where
  show = genericShow
instance decodeCommandInvocationList :: Decode CommandInvocationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandInvocationList :: Encode CommandInvocationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandInvocationStatus = CommandInvocationStatus String
derive instance newtypeCommandInvocationStatus :: Newtype CommandInvocationStatus _
derive instance repGenericCommandInvocationStatus :: Generic CommandInvocationStatus _
instance showCommandInvocationStatus :: Show CommandInvocationStatus where
  show = genericShow
instance decodeCommandInvocationStatus :: Decode CommandInvocationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandInvocationStatus :: Encode CommandInvocationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandList = CommandList (Array Command)
derive instance newtypeCommandList :: Newtype CommandList _
derive instance repGenericCommandList :: Generic CommandList _
instance showCommandList :: Show CommandList where
  show = genericShow
instance decodeCommandList :: Decode CommandList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandList :: Encode CommandList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandMaxResults = CommandMaxResults Int
derive instance newtypeCommandMaxResults :: Newtype CommandMaxResults _
derive instance repGenericCommandMaxResults :: Generic CommandMaxResults _
instance showCommandMaxResults :: Show CommandMaxResults where
  show = genericShow
instance decodeCommandMaxResults :: Decode CommandMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandMaxResults :: Encode CommandMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes plugin details.</p>
newtype CommandPlugin = CommandPlugin 
  { "Name" :: NullOrUndefined.NullOrUndefined (CommandPluginName)
  , "Status" :: NullOrUndefined.NullOrUndefined (CommandPluginStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (StatusDetails)
  , "ResponseCode" :: NullOrUndefined.NullOrUndefined (ResponseCode)
  , "ResponseStartDateTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "ResponseFinishDateTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Output" :: NullOrUndefined.NullOrUndefined (CommandPluginOutput)
  , "StandardOutputUrl" :: NullOrUndefined.NullOrUndefined (Url)
  , "StandardErrorUrl" :: NullOrUndefined.NullOrUndefined (Url)
  , "OutputS3Region" :: NullOrUndefined.NullOrUndefined (S3Region)
  , "OutputS3BucketName" :: NullOrUndefined.NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined.NullOrUndefined (S3KeyPrefix)
  }
derive instance newtypeCommandPlugin :: Newtype CommandPlugin _
derive instance repGenericCommandPlugin :: Generic CommandPlugin _
instance showCommandPlugin :: Show CommandPlugin where
  show = genericShow
instance decodeCommandPlugin :: Decode CommandPlugin where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandPlugin :: Encode CommandPlugin where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandPluginList = CommandPluginList (Array CommandPlugin)
derive instance newtypeCommandPluginList :: Newtype CommandPluginList _
derive instance repGenericCommandPluginList :: Generic CommandPluginList _
instance showCommandPluginList :: Show CommandPluginList where
  show = genericShow
instance decodeCommandPluginList :: Decode CommandPluginList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandPluginList :: Encode CommandPluginList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandPluginName = CommandPluginName String
derive instance newtypeCommandPluginName :: Newtype CommandPluginName _
derive instance repGenericCommandPluginName :: Generic CommandPluginName _
instance showCommandPluginName :: Show CommandPluginName where
  show = genericShow
instance decodeCommandPluginName :: Decode CommandPluginName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandPluginName :: Encode CommandPluginName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandPluginOutput = CommandPluginOutput String
derive instance newtypeCommandPluginOutput :: Newtype CommandPluginOutput _
derive instance repGenericCommandPluginOutput :: Generic CommandPluginOutput _
instance showCommandPluginOutput :: Show CommandPluginOutput where
  show = genericShow
instance decodeCommandPluginOutput :: Decode CommandPluginOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandPluginOutput :: Encode CommandPluginOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandPluginStatus = CommandPluginStatus String
derive instance newtypeCommandPluginStatus :: Newtype CommandPluginStatus _
derive instance repGenericCommandPluginStatus :: Generic CommandPluginStatus _
instance showCommandPluginStatus :: Show CommandPluginStatus where
  show = genericShow
instance decodeCommandPluginStatus :: Decode CommandPluginStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandPluginStatus :: Encode CommandPluginStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommandStatus = CommandStatus String
derive instance newtypeCommandStatus :: Newtype CommandStatus _
derive instance repGenericCommandStatus :: Generic CommandStatus _
instance showCommandStatus :: Show CommandStatus where
  show = genericShow
instance decodeCommandStatus :: Decode CommandStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommandStatus :: Encode CommandStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Comment = Comment String
derive instance newtypeComment :: Newtype Comment _
derive instance repGenericComment :: Generic Comment _
instance showComment :: Show Comment where
  show = genericShow
instance decodeComment :: Decode Comment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComment :: Encode Comment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompletedCount = CompletedCount Int
derive instance newtypeCompletedCount :: Newtype CompletedCount _
derive instance repGenericCompletedCount :: Generic CompletedCount _
instance showCompletedCount :: Show CompletedCount where
  show = genericShow
instance decodeCompletedCount :: Decode CompletedCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompletedCount :: Encode CompletedCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceExecutionId = ComplianceExecutionId String
derive instance newtypeComplianceExecutionId :: Newtype ComplianceExecutionId _
derive instance repGenericComplianceExecutionId :: Generic ComplianceExecutionId _
instance showComplianceExecutionId :: Show ComplianceExecutionId where
  show = genericShow
instance decodeComplianceExecutionId :: Decode ComplianceExecutionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceExecutionId :: Encode ComplianceExecutionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A summary of the call execution that includes an execution ID, the type of execution (for example, <code>Command</code>), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.</p>
newtype ComplianceExecutionSummary = ComplianceExecutionSummary 
  { "ExecutionTime" :: (DateTime)
  , "ExecutionId" :: NullOrUndefined.NullOrUndefined (ComplianceExecutionId)
  , "ExecutionType" :: NullOrUndefined.NullOrUndefined (ComplianceExecutionType)
  }
derive instance newtypeComplianceExecutionSummary :: Newtype ComplianceExecutionSummary _
derive instance repGenericComplianceExecutionSummary :: Generic ComplianceExecutionSummary _
instance showComplianceExecutionSummary :: Show ComplianceExecutionSummary where
  show = genericShow
instance decodeComplianceExecutionSummary :: Decode ComplianceExecutionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceExecutionSummary :: Encode ComplianceExecutionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceExecutionType = ComplianceExecutionType String
derive instance newtypeComplianceExecutionType :: Newtype ComplianceExecutionType _
derive instance repGenericComplianceExecutionType :: Generic ComplianceExecutionType _
instance showComplianceExecutionType :: Show ComplianceExecutionType where
  show = genericShow
instance decodeComplianceExecutionType :: Decode ComplianceExecutionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceExecutionType :: Encode ComplianceExecutionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceFilterValue = ComplianceFilterValue String
derive instance newtypeComplianceFilterValue :: Newtype ComplianceFilterValue _
derive instance repGenericComplianceFilterValue :: Generic ComplianceFilterValue _
instance showComplianceFilterValue :: Show ComplianceFilterValue where
  show = genericShow
instance decodeComplianceFilterValue :: Decode ComplianceFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceFilterValue :: Encode ComplianceFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the compliance as defined by the resource type. For example, for a patch resource type, <code>Items</code> includes information about the PatchSeverity, Classification, etc.</p>
newtype ComplianceItem = ComplianceItem 
  { "ComplianceType" :: NullOrUndefined.NullOrUndefined (ComplianceTypeName)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (ComplianceResourceType)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (ComplianceResourceId)
  , "Id" :: NullOrUndefined.NullOrUndefined (ComplianceItemId)
  , "Title" :: NullOrUndefined.NullOrUndefined (ComplianceItemTitle)
  , "Status" :: NullOrUndefined.NullOrUndefined (ComplianceStatus)
  , "Severity" :: NullOrUndefined.NullOrUndefined (ComplianceSeverity)
  , "ExecutionSummary" :: NullOrUndefined.NullOrUndefined (ComplianceExecutionSummary)
  , "Details" :: NullOrUndefined.NullOrUndefined (ComplianceItemDetails)
  }
derive instance newtypeComplianceItem :: Newtype ComplianceItem _
derive instance repGenericComplianceItem :: Generic ComplianceItem _
instance showComplianceItem :: Show ComplianceItem where
  show = genericShow
instance decodeComplianceItem :: Decode ComplianceItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceItem :: Encode ComplianceItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceItemContentHash = ComplianceItemContentHash String
derive instance newtypeComplianceItemContentHash :: Newtype ComplianceItemContentHash _
derive instance repGenericComplianceItemContentHash :: Generic ComplianceItemContentHash _
instance showComplianceItemContentHash :: Show ComplianceItemContentHash where
  show = genericShow
instance decodeComplianceItemContentHash :: Decode ComplianceItemContentHash where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceItemContentHash :: Encode ComplianceItemContentHash where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceItemDetails = ComplianceItemDetails (StrMap.StrMap AttributeValue)
derive instance newtypeComplianceItemDetails :: Newtype ComplianceItemDetails _
derive instance repGenericComplianceItemDetails :: Generic ComplianceItemDetails _
instance showComplianceItemDetails :: Show ComplianceItemDetails where
  show = genericShow
instance decodeComplianceItemDetails :: Decode ComplianceItemDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceItemDetails :: Encode ComplianceItemDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a compliance item.</p>
newtype ComplianceItemEntry = ComplianceItemEntry 
  { "Id" :: NullOrUndefined.NullOrUndefined (ComplianceItemId)
  , "Title" :: NullOrUndefined.NullOrUndefined (ComplianceItemTitle)
  , "Severity" :: (ComplianceSeverity)
  , "Status" :: (ComplianceStatus)
  , "Details" :: NullOrUndefined.NullOrUndefined (ComplianceItemDetails)
  }
derive instance newtypeComplianceItemEntry :: Newtype ComplianceItemEntry _
derive instance repGenericComplianceItemEntry :: Generic ComplianceItemEntry _
instance showComplianceItemEntry :: Show ComplianceItemEntry where
  show = genericShow
instance decodeComplianceItemEntry :: Decode ComplianceItemEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceItemEntry :: Encode ComplianceItemEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceItemEntryList = ComplianceItemEntryList (Array ComplianceItemEntry)
derive instance newtypeComplianceItemEntryList :: Newtype ComplianceItemEntryList _
derive instance repGenericComplianceItemEntryList :: Generic ComplianceItemEntryList _
instance showComplianceItemEntryList :: Show ComplianceItemEntryList where
  show = genericShow
instance decodeComplianceItemEntryList :: Decode ComplianceItemEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceItemEntryList :: Encode ComplianceItemEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceItemId = ComplianceItemId String
derive instance newtypeComplianceItemId :: Newtype ComplianceItemId _
derive instance repGenericComplianceItemId :: Generic ComplianceItemId _
instance showComplianceItemId :: Show ComplianceItemId where
  show = genericShow
instance decodeComplianceItemId :: Decode ComplianceItemId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceItemId :: Encode ComplianceItemId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceItemList = ComplianceItemList (Array ComplianceItem)
derive instance newtypeComplianceItemList :: Newtype ComplianceItemList _
derive instance repGenericComplianceItemList :: Generic ComplianceItemList _
instance showComplianceItemList :: Show ComplianceItemList where
  show = genericShow
instance decodeComplianceItemList :: Decode ComplianceItemList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceItemList :: Encode ComplianceItemList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceItemTitle = ComplianceItemTitle String
derive instance newtypeComplianceItemTitle :: Newtype ComplianceItemTitle _
derive instance repGenericComplianceItemTitle :: Generic ComplianceItemTitle _
instance showComplianceItemTitle :: Show ComplianceItemTitle where
  show = genericShow
instance decodeComplianceItemTitle :: Decode ComplianceItemTitle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceItemTitle :: Encode ComplianceItemTitle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceQueryOperatorType = ComplianceQueryOperatorType String
derive instance newtypeComplianceQueryOperatorType :: Newtype ComplianceQueryOperatorType _
derive instance repGenericComplianceQueryOperatorType :: Generic ComplianceQueryOperatorType _
instance showComplianceQueryOperatorType :: Show ComplianceQueryOperatorType where
  show = genericShow
instance decodeComplianceQueryOperatorType :: Decode ComplianceQueryOperatorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceQueryOperatorType :: Encode ComplianceQueryOperatorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceResourceId = ComplianceResourceId String
derive instance newtypeComplianceResourceId :: Newtype ComplianceResourceId _
derive instance repGenericComplianceResourceId :: Generic ComplianceResourceId _
instance showComplianceResourceId :: Show ComplianceResourceId where
  show = genericShow
instance decodeComplianceResourceId :: Decode ComplianceResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceResourceId :: Encode ComplianceResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceResourceIdList = ComplianceResourceIdList (Array ComplianceResourceId)
derive instance newtypeComplianceResourceIdList :: Newtype ComplianceResourceIdList _
derive instance repGenericComplianceResourceIdList :: Generic ComplianceResourceIdList _
instance showComplianceResourceIdList :: Show ComplianceResourceIdList where
  show = genericShow
instance decodeComplianceResourceIdList :: Decode ComplianceResourceIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceResourceIdList :: Encode ComplianceResourceIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceResourceType = ComplianceResourceType String
derive instance newtypeComplianceResourceType :: Newtype ComplianceResourceType _
derive instance repGenericComplianceResourceType :: Generic ComplianceResourceType _
instance showComplianceResourceType :: Show ComplianceResourceType where
  show = genericShow
instance decodeComplianceResourceType :: Decode ComplianceResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceResourceType :: Encode ComplianceResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceResourceTypeList = ComplianceResourceTypeList (Array ComplianceResourceType)
derive instance newtypeComplianceResourceTypeList :: Newtype ComplianceResourceTypeList _
derive instance repGenericComplianceResourceTypeList :: Generic ComplianceResourceTypeList _
instance showComplianceResourceTypeList :: Show ComplianceResourceTypeList where
  show = genericShow
instance decodeComplianceResourceTypeList :: Decode ComplianceResourceTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceResourceTypeList :: Encode ComplianceResourceTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceSeverity = ComplianceSeverity String
derive instance newtypeComplianceSeverity :: Newtype ComplianceSeverity _
derive instance repGenericComplianceSeverity :: Generic ComplianceSeverity _
instance showComplianceSeverity :: Show ComplianceSeverity where
  show = genericShow
instance decodeComplianceSeverity :: Decode ComplianceSeverity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceSeverity :: Encode ComplianceSeverity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceStatus = ComplianceStatus String
derive instance newtypeComplianceStatus :: Newtype ComplianceStatus _
derive instance repGenericComplianceStatus :: Generic ComplianceStatus _
instance showComplianceStatus :: Show ComplianceStatus where
  show = genericShow
instance decodeComplianceStatus :: Decode ComplianceStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceStatus :: Encode ComplianceStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more filters. Use a filter to return a more specific list of results.</p>
newtype ComplianceStringFilter = ComplianceStringFilter 
  { "Key" :: NullOrUndefined.NullOrUndefined (ComplianceStringFilterKey)
  , "Values" :: NullOrUndefined.NullOrUndefined (ComplianceStringFilterValueList)
  , "Type" :: NullOrUndefined.NullOrUndefined (ComplianceQueryOperatorType)
  }
derive instance newtypeComplianceStringFilter :: Newtype ComplianceStringFilter _
derive instance repGenericComplianceStringFilter :: Generic ComplianceStringFilter _
instance showComplianceStringFilter :: Show ComplianceStringFilter where
  show = genericShow
instance decodeComplianceStringFilter :: Decode ComplianceStringFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceStringFilter :: Encode ComplianceStringFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceStringFilterKey = ComplianceStringFilterKey String
derive instance newtypeComplianceStringFilterKey :: Newtype ComplianceStringFilterKey _
derive instance repGenericComplianceStringFilterKey :: Generic ComplianceStringFilterKey _
instance showComplianceStringFilterKey :: Show ComplianceStringFilterKey where
  show = genericShow
instance decodeComplianceStringFilterKey :: Decode ComplianceStringFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceStringFilterKey :: Encode ComplianceStringFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceStringFilterList = ComplianceStringFilterList (Array ComplianceStringFilter)
derive instance newtypeComplianceStringFilterList :: Newtype ComplianceStringFilterList _
derive instance repGenericComplianceStringFilterList :: Generic ComplianceStringFilterList _
instance showComplianceStringFilterList :: Show ComplianceStringFilterList where
  show = genericShow
instance decodeComplianceStringFilterList :: Decode ComplianceStringFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceStringFilterList :: Encode ComplianceStringFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceStringFilterValueList = ComplianceStringFilterValueList (Array ComplianceFilterValue)
derive instance newtypeComplianceStringFilterValueList :: Newtype ComplianceStringFilterValueList _
derive instance repGenericComplianceStringFilterValueList :: Generic ComplianceStringFilterValueList _
instance showComplianceStringFilterValueList :: Show ComplianceStringFilterValueList where
  show = genericShow
instance decodeComplianceStringFilterValueList :: Decode ComplianceStringFilterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceStringFilterValueList :: Encode ComplianceStringFilterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceSummaryCount = ComplianceSummaryCount Int
derive instance newtypeComplianceSummaryCount :: Newtype ComplianceSummaryCount _
derive instance repGenericComplianceSummaryCount :: Generic ComplianceSummaryCount _
instance showComplianceSummaryCount :: Show ComplianceSummaryCount where
  show = genericShow
instance decodeComplianceSummaryCount :: Decode ComplianceSummaryCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceSummaryCount :: Encode ComplianceSummaryCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A summary of compliance information by compliance type.</p>
newtype ComplianceSummaryItem = ComplianceSummaryItem 
  { "ComplianceType" :: NullOrUndefined.NullOrUndefined (ComplianceTypeName)
  , "CompliantSummary" :: NullOrUndefined.NullOrUndefined (CompliantSummary)
  , "NonCompliantSummary" :: NullOrUndefined.NullOrUndefined (NonCompliantSummary)
  }
derive instance newtypeComplianceSummaryItem :: Newtype ComplianceSummaryItem _
derive instance repGenericComplianceSummaryItem :: Generic ComplianceSummaryItem _
instance showComplianceSummaryItem :: Show ComplianceSummaryItem where
  show = genericShow
instance decodeComplianceSummaryItem :: Decode ComplianceSummaryItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceSummaryItem :: Encode ComplianceSummaryItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceSummaryItemList = ComplianceSummaryItemList (Array ComplianceSummaryItem)
derive instance newtypeComplianceSummaryItemList :: Newtype ComplianceSummaryItemList _
derive instance repGenericComplianceSummaryItemList :: Generic ComplianceSummaryItemList _
instance showComplianceSummaryItemList :: Show ComplianceSummaryItemList where
  show = genericShow
instance decodeComplianceSummaryItemList :: Decode ComplianceSummaryItemList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceSummaryItemList :: Encode ComplianceSummaryItemList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You specified too many custom compliance types. You can specify a maximum of 10 different types. </p>
newtype ComplianceTypeCountLimitExceededException = ComplianceTypeCountLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeComplianceTypeCountLimitExceededException :: Newtype ComplianceTypeCountLimitExceededException _
derive instance repGenericComplianceTypeCountLimitExceededException :: Generic ComplianceTypeCountLimitExceededException _
instance showComplianceTypeCountLimitExceededException :: Show ComplianceTypeCountLimitExceededException where
  show = genericShow
instance decodeComplianceTypeCountLimitExceededException :: Decode ComplianceTypeCountLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceTypeCountLimitExceededException :: Encode ComplianceTypeCountLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceTypeName = ComplianceTypeName String
derive instance newtypeComplianceTypeName :: Newtype ComplianceTypeName _
derive instance repGenericComplianceTypeName :: Generic ComplianceTypeName _
instance showComplianceTypeName :: Show ComplianceTypeName where
  show = genericShow
instance decodeComplianceTypeName :: Decode ComplianceTypeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceTypeName :: Encode ComplianceTypeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A summary of resources that are compliant. The summary is organized according to the resource count for each compliance type.</p>
newtype CompliantSummary = CompliantSummary 
  { "CompliantCount" :: NullOrUndefined.NullOrUndefined (ComplianceSummaryCount)
  , "SeveritySummary" :: NullOrUndefined.NullOrUndefined (SeveritySummary)
  }
derive instance newtypeCompliantSummary :: Newtype CompliantSummary _
derive instance repGenericCompliantSummary :: Generic CompliantSummary _
instance showCompliantSummary :: Show CompliantSummary where
  show = genericShow
instance decodeCompliantSummary :: Decode CompliantSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompliantSummary :: Encode CompliantSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComputerName = ComputerName String
derive instance newtypeComputerName :: Newtype ComputerName _
derive instance repGenericComputerName :: Generic ComputerName _
instance showComputerName :: Show ComputerName where
  show = genericShow
instance decodeComputerName :: Decode ComputerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputerName :: Encode ComputerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateActivationRequest = CreateActivationRequest 
  { "Description" :: NullOrUndefined.NullOrUndefined (ActivationDescription)
  , "DefaultInstanceName" :: NullOrUndefined.NullOrUndefined (DefaultInstanceName)
  , "IamRole" :: (IamRole)
  , "RegistrationLimit" :: NullOrUndefined.NullOrUndefined (RegistrationLimit)
  , "ExpirationDate" :: NullOrUndefined.NullOrUndefined (ExpirationDate)
  }
derive instance newtypeCreateActivationRequest :: Newtype CreateActivationRequest _
derive instance repGenericCreateActivationRequest :: Generic CreateActivationRequest _
instance showCreateActivationRequest :: Show CreateActivationRequest where
  show = genericShow
instance decodeCreateActivationRequest :: Decode CreateActivationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateActivationRequest :: Encode CreateActivationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateActivationResult = CreateActivationResult 
  { "ActivationId" :: NullOrUndefined.NullOrUndefined (ActivationId)
  , "ActivationCode" :: NullOrUndefined.NullOrUndefined (ActivationCode)
  }
derive instance newtypeCreateActivationResult :: Newtype CreateActivationResult _
derive instance repGenericCreateActivationResult :: Generic CreateActivationResult _
instance showCreateActivationResult :: Show CreateActivationResult where
  show = genericShow
instance decodeCreateActivationResult :: Decode CreateActivationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateActivationResult :: Encode CreateActivationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAssociationBatchRequest = CreateAssociationBatchRequest 
  { "Entries" :: (CreateAssociationBatchRequestEntries)
  }
derive instance newtypeCreateAssociationBatchRequest :: Newtype CreateAssociationBatchRequest _
derive instance repGenericCreateAssociationBatchRequest :: Generic CreateAssociationBatchRequest _
instance showCreateAssociationBatchRequest :: Show CreateAssociationBatchRequest where
  show = genericShow
instance decodeCreateAssociationBatchRequest :: Decode CreateAssociationBatchRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAssociationBatchRequest :: Encode CreateAssociationBatchRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAssociationBatchRequestEntries = CreateAssociationBatchRequestEntries (Array CreateAssociationBatchRequestEntry)
derive instance newtypeCreateAssociationBatchRequestEntries :: Newtype CreateAssociationBatchRequestEntries _
derive instance repGenericCreateAssociationBatchRequestEntries :: Generic CreateAssociationBatchRequestEntries _
instance showCreateAssociationBatchRequestEntries :: Show CreateAssociationBatchRequestEntries where
  show = genericShow
instance decodeCreateAssociationBatchRequestEntries :: Decode CreateAssociationBatchRequestEntries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAssociationBatchRequestEntries :: Encode CreateAssociationBatchRequestEntries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the association of a Systems Manager document and an instance.</p>
newtype CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry 
  { "Name" :: (DocumentName)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined.NullOrUndefined (InstanceAssociationOutputLocation)
  , "AssociationName" :: NullOrUndefined.NullOrUndefined (AssociationName)
  }
derive instance newtypeCreateAssociationBatchRequestEntry :: Newtype CreateAssociationBatchRequestEntry _
derive instance repGenericCreateAssociationBatchRequestEntry :: Generic CreateAssociationBatchRequestEntry _
instance showCreateAssociationBatchRequestEntry :: Show CreateAssociationBatchRequestEntry where
  show = genericShow
instance decodeCreateAssociationBatchRequestEntry :: Decode CreateAssociationBatchRequestEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAssociationBatchRequestEntry :: Encode CreateAssociationBatchRequestEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAssociationBatchResult = CreateAssociationBatchResult 
  { "Successful" :: NullOrUndefined.NullOrUndefined (AssociationDescriptionList)
  , "Failed" :: NullOrUndefined.NullOrUndefined (FailedCreateAssociationList)
  }
derive instance newtypeCreateAssociationBatchResult :: Newtype CreateAssociationBatchResult _
derive instance repGenericCreateAssociationBatchResult :: Generic CreateAssociationBatchResult _
instance showCreateAssociationBatchResult :: Show CreateAssociationBatchResult where
  show = genericShow
instance decodeCreateAssociationBatchResult :: Decode CreateAssociationBatchResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAssociationBatchResult :: Encode CreateAssociationBatchResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAssociationRequest = CreateAssociationRequest 
  { "Name" :: (DocumentName)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined.NullOrUndefined (InstanceAssociationOutputLocation)
  , "AssociationName" :: NullOrUndefined.NullOrUndefined (AssociationName)
  }
derive instance newtypeCreateAssociationRequest :: Newtype CreateAssociationRequest _
derive instance repGenericCreateAssociationRequest :: Generic CreateAssociationRequest _
instance showCreateAssociationRequest :: Show CreateAssociationRequest where
  show = genericShow
instance decodeCreateAssociationRequest :: Decode CreateAssociationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAssociationRequest :: Encode CreateAssociationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateAssociationResult = CreateAssociationResult 
  { "AssociationDescription" :: NullOrUndefined.NullOrUndefined (AssociationDescription)
  }
derive instance newtypeCreateAssociationResult :: Newtype CreateAssociationResult _
derive instance repGenericCreateAssociationResult :: Generic CreateAssociationResult _
instance showCreateAssociationResult :: Show CreateAssociationResult where
  show = genericShow
instance decodeCreateAssociationResult :: Decode CreateAssociationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAssociationResult :: Encode CreateAssociationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDocumentRequest = CreateDocumentRequest 
  { "Content" :: (DocumentContent)
  , "Name" :: (DocumentName)
  , "DocumentType" :: NullOrUndefined.NullOrUndefined (DocumentType)
  , "DocumentFormat" :: NullOrUndefined.NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined.NullOrUndefined (TargetType)
  }
derive instance newtypeCreateDocumentRequest :: Newtype CreateDocumentRequest _
derive instance repGenericCreateDocumentRequest :: Generic CreateDocumentRequest _
instance showCreateDocumentRequest :: Show CreateDocumentRequest where
  show = genericShow
instance decodeCreateDocumentRequest :: Decode CreateDocumentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDocumentRequest :: Encode CreateDocumentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDocumentResult = CreateDocumentResult 
  { "DocumentDescription" :: NullOrUndefined.NullOrUndefined (DocumentDescription)
  }
derive instance newtypeCreateDocumentResult :: Newtype CreateDocumentResult _
derive instance repGenericCreateDocumentResult :: Generic CreateDocumentResult _
instance showCreateDocumentResult :: Show CreateDocumentResult where
  show = genericShow
instance decodeCreateDocumentResult :: Decode CreateDocumentResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDocumentResult :: Encode CreateDocumentResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateMaintenanceWindowRequest = CreateMaintenanceWindowRequest 
  { "Name" :: (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  , "Schedule" :: (MaintenanceWindowSchedule)
  , "Duration" :: (MaintenanceWindowDurationHours)
  , "Cutoff" :: (MaintenanceWindowCutoff)
  , "AllowUnassociatedTargets" :: (MaintenanceWindowAllowUnassociatedTargets)
  , "ClientToken" :: NullOrUndefined.NullOrUndefined (ClientToken)
  }
derive instance newtypeCreateMaintenanceWindowRequest :: Newtype CreateMaintenanceWindowRequest _
derive instance repGenericCreateMaintenanceWindowRequest :: Generic CreateMaintenanceWindowRequest _
instance showCreateMaintenanceWindowRequest :: Show CreateMaintenanceWindowRequest where
  show = genericShow
instance decodeCreateMaintenanceWindowRequest :: Decode CreateMaintenanceWindowRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMaintenanceWindowRequest :: Encode CreateMaintenanceWindowRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateMaintenanceWindowResult = CreateMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  }
derive instance newtypeCreateMaintenanceWindowResult :: Newtype CreateMaintenanceWindowResult _
derive instance repGenericCreateMaintenanceWindowResult :: Generic CreateMaintenanceWindowResult _
instance showCreateMaintenanceWindowResult :: Show CreateMaintenanceWindowResult where
  show = genericShow
instance decodeCreateMaintenanceWindowResult :: Decode CreateMaintenanceWindowResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMaintenanceWindowResult :: Encode CreateMaintenanceWindowResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatePatchBaselineRequest = CreatePatchBaselineRequest 
  { "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  , "Name" :: (BaselineName)
  , "GlobalFilters" :: NullOrUndefined.NullOrUndefined (PatchFilterGroup)
  , "ApprovalRules" :: NullOrUndefined.NullOrUndefined (PatchRuleGroup)
  , "ApprovedPatches" :: NullOrUndefined.NullOrUndefined (PatchIdList)
  , "ApprovedPatchesComplianceLevel" :: NullOrUndefined.NullOrUndefined (PatchComplianceLevel)
  , "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RejectedPatches" :: NullOrUndefined.NullOrUndefined (PatchIdList)
  , "Description" :: NullOrUndefined.NullOrUndefined (BaselineDescription)
  , "Sources" :: NullOrUndefined.NullOrUndefined (PatchSourceList)
  , "ClientToken" :: NullOrUndefined.NullOrUndefined (ClientToken)
  }
derive instance newtypeCreatePatchBaselineRequest :: Newtype CreatePatchBaselineRequest _
derive instance repGenericCreatePatchBaselineRequest :: Generic CreatePatchBaselineRequest _
instance showCreatePatchBaselineRequest :: Show CreatePatchBaselineRequest where
  show = genericShow
instance decodeCreatePatchBaselineRequest :: Decode CreatePatchBaselineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePatchBaselineRequest :: Encode CreatePatchBaselineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatePatchBaselineResult = CreatePatchBaselineResult 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  }
derive instance newtypeCreatePatchBaselineResult :: Newtype CreatePatchBaselineResult _
derive instance repGenericCreatePatchBaselineResult :: Generic CreatePatchBaselineResult _
instance showCreatePatchBaselineResult :: Show CreatePatchBaselineResult where
  show = genericShow
instance decodeCreatePatchBaselineResult :: Decode CreatePatchBaselineResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePatchBaselineResult :: Encode CreatePatchBaselineResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceDataSyncRequest = CreateResourceDataSyncRequest 
  { "SyncName" :: (ResourceDataSyncName)
  , "S3Destination" :: (ResourceDataSyncS3Destination)
  }
derive instance newtypeCreateResourceDataSyncRequest :: Newtype CreateResourceDataSyncRequest _
derive instance repGenericCreateResourceDataSyncRequest :: Generic CreateResourceDataSyncRequest _
instance showCreateResourceDataSyncRequest :: Show CreateResourceDataSyncRequest where
  show = genericShow
instance decodeCreateResourceDataSyncRequest :: Decode CreateResourceDataSyncRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceDataSyncRequest :: Encode CreateResourceDataSyncRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateResourceDataSyncResult = CreateResourceDataSyncResult Types.NoArguments
derive instance newtypeCreateResourceDataSyncResult :: Newtype CreateResourceDataSyncResult _
derive instance repGenericCreateResourceDataSyncResult :: Generic CreateResourceDataSyncResult _
instance showCreateResourceDataSyncResult :: Show CreateResourceDataSyncResult where
  show = genericShow
instance decodeCreateResourceDataSyncResult :: Decode CreateResourceDataSyncResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateResourceDataSyncResult :: Encode CreateResourceDataSyncResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatedDate = CreatedDate Number
derive instance newtypeCreatedDate :: Newtype CreatedDate _
derive instance repGenericCreatedDate :: Generic CreatedDate _
instance showCreatedDate :: Show CreatedDate where
  show = genericShow
instance decodeCreatedDate :: Decode CreatedDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatedDate :: Encode CreatedDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have exceeded the limit for custom schemas. Delete one or more custom schemas and try again.</p>
newtype CustomSchemaCountLimitExceededException = CustomSchemaCountLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCustomSchemaCountLimitExceededException :: Newtype CustomSchemaCountLimitExceededException _
derive instance repGenericCustomSchemaCountLimitExceededException :: Generic CustomSchemaCountLimitExceededException _
instance showCustomSchemaCountLimitExceededException :: Show CustomSchemaCountLimitExceededException where
  show = genericShow
instance decodeCustomSchemaCountLimitExceededException :: Decode CustomSchemaCountLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomSchemaCountLimitExceededException :: Encode CustomSchemaCountLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DateTime = DateTime Number
derive instance newtypeDateTime :: Newtype DateTime _
derive instance repGenericDateTime :: Generic DateTime _
instance showDateTime :: Show DateTime where
  show = genericShow
instance decodeDateTime :: Decode DateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDateTime :: Encode DateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DefaultBaseline = DefaultBaseline Boolean
derive instance newtypeDefaultBaseline :: Newtype DefaultBaseline _
derive instance repGenericDefaultBaseline :: Generic DefaultBaseline _
instance showDefaultBaseline :: Show DefaultBaseline where
  show = genericShow
instance decodeDefaultBaseline :: Decode DefaultBaseline where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultBaseline :: Encode DefaultBaseline where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DefaultInstanceName = DefaultInstanceName String
derive instance newtypeDefaultInstanceName :: Newtype DefaultInstanceName _
derive instance repGenericDefaultInstanceName :: Generic DefaultInstanceName _
instance showDefaultInstanceName :: Show DefaultInstanceName where
  show = genericShow
instance decodeDefaultInstanceName :: Decode DefaultInstanceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultInstanceName :: Encode DefaultInstanceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteActivationRequest = DeleteActivationRequest 
  { "ActivationId" :: (ActivationId)
  }
derive instance newtypeDeleteActivationRequest :: Newtype DeleteActivationRequest _
derive instance repGenericDeleteActivationRequest :: Generic DeleteActivationRequest _
instance showDeleteActivationRequest :: Show DeleteActivationRequest where
  show = genericShow
instance decodeDeleteActivationRequest :: Decode DeleteActivationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteActivationRequest :: Encode DeleteActivationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteActivationResult = DeleteActivationResult Types.NoArguments
derive instance newtypeDeleteActivationResult :: Newtype DeleteActivationResult _
derive instance repGenericDeleteActivationResult :: Generic DeleteActivationResult _
instance showDeleteActivationResult :: Show DeleteActivationResult where
  show = genericShow
instance decodeDeleteActivationResult :: Decode DeleteActivationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteActivationResult :: Encode DeleteActivationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAssociationRequest = DeleteAssociationRequest 
  { "Name" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "AssociationId" :: NullOrUndefined.NullOrUndefined (AssociationId)
  }
derive instance newtypeDeleteAssociationRequest :: Newtype DeleteAssociationRequest _
derive instance repGenericDeleteAssociationRequest :: Generic DeleteAssociationRequest _
instance showDeleteAssociationRequest :: Show DeleteAssociationRequest where
  show = genericShow
instance decodeDeleteAssociationRequest :: Decode DeleteAssociationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAssociationRequest :: Encode DeleteAssociationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAssociationResult = DeleteAssociationResult Types.NoArguments
derive instance newtypeDeleteAssociationResult :: Newtype DeleteAssociationResult _
derive instance repGenericDeleteAssociationResult :: Generic DeleteAssociationResult _
instance showDeleteAssociationResult :: Show DeleteAssociationResult where
  show = genericShow
instance decodeDeleteAssociationResult :: Decode DeleteAssociationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAssociationResult :: Encode DeleteAssociationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDocumentRequest = DeleteDocumentRequest 
  { "Name" :: (DocumentName)
  }
derive instance newtypeDeleteDocumentRequest :: Newtype DeleteDocumentRequest _
derive instance repGenericDeleteDocumentRequest :: Generic DeleteDocumentRequest _
instance showDeleteDocumentRequest :: Show DeleteDocumentRequest where
  show = genericShow
instance decodeDeleteDocumentRequest :: Decode DeleteDocumentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDocumentRequest :: Encode DeleteDocumentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDocumentResult = DeleteDocumentResult Types.NoArguments
derive instance newtypeDeleteDocumentResult :: Newtype DeleteDocumentResult _
derive instance repGenericDeleteDocumentResult :: Generic DeleteDocumentResult _
instance showDeleteDocumentResult :: Show DeleteDocumentResult where
  show = genericShow
instance decodeDeleteDocumentResult :: Decode DeleteDocumentResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDocumentResult :: Encode DeleteDocumentResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMaintenanceWindowRequest = DeleteMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  }
derive instance newtypeDeleteMaintenanceWindowRequest :: Newtype DeleteMaintenanceWindowRequest _
derive instance repGenericDeleteMaintenanceWindowRequest :: Generic DeleteMaintenanceWindowRequest _
instance showDeleteMaintenanceWindowRequest :: Show DeleteMaintenanceWindowRequest where
  show = genericShow
instance decodeDeleteMaintenanceWindowRequest :: Decode DeleteMaintenanceWindowRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMaintenanceWindowRequest :: Encode DeleteMaintenanceWindowRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMaintenanceWindowResult = DeleteMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  }
derive instance newtypeDeleteMaintenanceWindowResult :: Newtype DeleteMaintenanceWindowResult _
derive instance repGenericDeleteMaintenanceWindowResult :: Generic DeleteMaintenanceWindowResult _
instance showDeleteMaintenanceWindowResult :: Show DeleteMaintenanceWindowResult where
  show = genericShow
instance decodeDeleteMaintenanceWindowResult :: Decode DeleteMaintenanceWindowResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMaintenanceWindowResult :: Encode DeleteMaintenanceWindowResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteParameterRequest = DeleteParameterRequest 
  { "Name" :: (PSParameterName)
  }
derive instance newtypeDeleteParameterRequest :: Newtype DeleteParameterRequest _
derive instance repGenericDeleteParameterRequest :: Generic DeleteParameterRequest _
instance showDeleteParameterRequest :: Show DeleteParameterRequest where
  show = genericShow
instance decodeDeleteParameterRequest :: Decode DeleteParameterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteParameterRequest :: Encode DeleteParameterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteParameterResult = DeleteParameterResult Types.NoArguments
derive instance newtypeDeleteParameterResult :: Newtype DeleteParameterResult _
derive instance repGenericDeleteParameterResult :: Generic DeleteParameterResult _
instance showDeleteParameterResult :: Show DeleteParameterResult where
  show = genericShow
instance decodeDeleteParameterResult :: Decode DeleteParameterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteParameterResult :: Encode DeleteParameterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteParametersRequest = DeleteParametersRequest 
  { "Names" :: (ParameterNameList)
  }
derive instance newtypeDeleteParametersRequest :: Newtype DeleteParametersRequest _
derive instance repGenericDeleteParametersRequest :: Generic DeleteParametersRequest _
instance showDeleteParametersRequest :: Show DeleteParametersRequest where
  show = genericShow
instance decodeDeleteParametersRequest :: Decode DeleteParametersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteParametersRequest :: Encode DeleteParametersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteParametersResult = DeleteParametersResult 
  { "DeletedParameters" :: NullOrUndefined.NullOrUndefined (ParameterNameList)
  , "InvalidParameters" :: NullOrUndefined.NullOrUndefined (ParameterNameList)
  }
derive instance newtypeDeleteParametersResult :: Newtype DeleteParametersResult _
derive instance repGenericDeleteParametersResult :: Generic DeleteParametersResult _
instance showDeleteParametersResult :: Show DeleteParametersResult where
  show = genericShow
instance decodeDeleteParametersResult :: Decode DeleteParametersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteParametersResult :: Encode DeleteParametersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePatchBaselineRequest = DeletePatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  }
derive instance newtypeDeletePatchBaselineRequest :: Newtype DeletePatchBaselineRequest _
derive instance repGenericDeletePatchBaselineRequest :: Generic DeletePatchBaselineRequest _
instance showDeletePatchBaselineRequest :: Show DeletePatchBaselineRequest where
  show = genericShow
instance decodeDeletePatchBaselineRequest :: Decode DeletePatchBaselineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePatchBaselineRequest :: Encode DeletePatchBaselineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePatchBaselineResult = DeletePatchBaselineResult 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  }
derive instance newtypeDeletePatchBaselineResult :: Newtype DeletePatchBaselineResult _
derive instance repGenericDeletePatchBaselineResult :: Generic DeletePatchBaselineResult _
instance showDeletePatchBaselineResult :: Show DeletePatchBaselineResult where
  show = genericShow
instance decodeDeletePatchBaselineResult :: Decode DeletePatchBaselineResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePatchBaselineResult :: Encode DeletePatchBaselineResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResourceDataSyncRequest = DeleteResourceDataSyncRequest 
  { "SyncName" :: (ResourceDataSyncName)
  }
derive instance newtypeDeleteResourceDataSyncRequest :: Newtype DeleteResourceDataSyncRequest _
derive instance repGenericDeleteResourceDataSyncRequest :: Generic DeleteResourceDataSyncRequest _
instance showDeleteResourceDataSyncRequest :: Show DeleteResourceDataSyncRequest where
  show = genericShow
instance decodeDeleteResourceDataSyncRequest :: Decode DeleteResourceDataSyncRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResourceDataSyncRequest :: Encode DeleteResourceDataSyncRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResourceDataSyncResult = DeleteResourceDataSyncResult Types.NoArguments
derive instance newtypeDeleteResourceDataSyncResult :: Newtype DeleteResourceDataSyncResult _
derive instance repGenericDeleteResourceDataSyncResult :: Generic DeleteResourceDataSyncResult _
instance showDeleteResourceDataSyncResult :: Show DeleteResourceDataSyncResult where
  show = genericShow
instance decodeDeleteResourceDataSyncResult :: Decode DeleteResourceDataSyncResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResourceDataSyncResult :: Encode DeleteResourceDataSyncResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterManagedInstanceRequest = DeregisterManagedInstanceRequest 
  { "InstanceId" :: (ManagedInstanceId)
  }
derive instance newtypeDeregisterManagedInstanceRequest :: Newtype DeregisterManagedInstanceRequest _
derive instance repGenericDeregisterManagedInstanceRequest :: Generic DeregisterManagedInstanceRequest _
instance showDeregisterManagedInstanceRequest :: Show DeregisterManagedInstanceRequest where
  show = genericShow
instance decodeDeregisterManagedInstanceRequest :: Decode DeregisterManagedInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterManagedInstanceRequest :: Encode DeregisterManagedInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterManagedInstanceResult = DeregisterManagedInstanceResult Types.NoArguments
derive instance newtypeDeregisterManagedInstanceResult :: Newtype DeregisterManagedInstanceResult _
derive instance repGenericDeregisterManagedInstanceResult :: Generic DeregisterManagedInstanceResult _
instance showDeregisterManagedInstanceResult :: Show DeregisterManagedInstanceResult where
  show = genericShow
instance decodeDeregisterManagedInstanceResult :: Decode DeregisterManagedInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterManagedInstanceResult :: Encode DeregisterManagedInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterPatchBaselineForPatchGroupRequest = DeregisterPatchBaselineForPatchGroupRequest 
  { "BaselineId" :: (BaselineId)
  , "PatchGroup" :: (PatchGroup)
  }
derive instance newtypeDeregisterPatchBaselineForPatchGroupRequest :: Newtype DeregisterPatchBaselineForPatchGroupRequest _
derive instance repGenericDeregisterPatchBaselineForPatchGroupRequest :: Generic DeregisterPatchBaselineForPatchGroupRequest _
instance showDeregisterPatchBaselineForPatchGroupRequest :: Show DeregisterPatchBaselineForPatchGroupRequest where
  show = genericShow
instance decodeDeregisterPatchBaselineForPatchGroupRequest :: Decode DeregisterPatchBaselineForPatchGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterPatchBaselineForPatchGroupRequest :: Encode DeregisterPatchBaselineForPatchGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterPatchBaselineForPatchGroupResult = DeregisterPatchBaselineForPatchGroupResult 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  , "PatchGroup" :: NullOrUndefined.NullOrUndefined (PatchGroup)
  }
derive instance newtypeDeregisterPatchBaselineForPatchGroupResult :: Newtype DeregisterPatchBaselineForPatchGroupResult _
derive instance repGenericDeregisterPatchBaselineForPatchGroupResult :: Generic DeregisterPatchBaselineForPatchGroupResult _
instance showDeregisterPatchBaselineForPatchGroupResult :: Show DeregisterPatchBaselineForPatchGroupResult where
  show = genericShow
instance decodeDeregisterPatchBaselineForPatchGroupResult :: Decode DeregisterPatchBaselineForPatchGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterPatchBaselineForPatchGroupResult :: Encode DeregisterPatchBaselineForPatchGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterTargetFromMaintenanceWindowRequest = DeregisterTargetFromMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTargetId" :: (MaintenanceWindowTargetId)
  , "Safe" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDeregisterTargetFromMaintenanceWindowRequest :: Newtype DeregisterTargetFromMaintenanceWindowRequest _
derive instance repGenericDeregisterTargetFromMaintenanceWindowRequest :: Generic DeregisterTargetFromMaintenanceWindowRequest _
instance showDeregisterTargetFromMaintenanceWindowRequest :: Show DeregisterTargetFromMaintenanceWindowRequest where
  show = genericShow
instance decodeDeregisterTargetFromMaintenanceWindowRequest :: Decode DeregisterTargetFromMaintenanceWindowRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterTargetFromMaintenanceWindowRequest :: Encode DeregisterTargetFromMaintenanceWindowRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterTargetFromMaintenanceWindowResult = DeregisterTargetFromMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "WindowTargetId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTargetId)
  }
derive instance newtypeDeregisterTargetFromMaintenanceWindowResult :: Newtype DeregisterTargetFromMaintenanceWindowResult _
derive instance repGenericDeregisterTargetFromMaintenanceWindowResult :: Generic DeregisterTargetFromMaintenanceWindowResult _
instance showDeregisterTargetFromMaintenanceWindowResult :: Show DeregisterTargetFromMaintenanceWindowResult where
  show = genericShow
instance decodeDeregisterTargetFromMaintenanceWindowResult :: Decode DeregisterTargetFromMaintenanceWindowResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterTargetFromMaintenanceWindowResult :: Encode DeregisterTargetFromMaintenanceWindowResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterTaskFromMaintenanceWindowRequest = DeregisterTaskFromMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTaskId" :: (MaintenanceWindowTaskId)
  }
derive instance newtypeDeregisterTaskFromMaintenanceWindowRequest :: Newtype DeregisterTaskFromMaintenanceWindowRequest _
derive instance repGenericDeregisterTaskFromMaintenanceWindowRequest :: Generic DeregisterTaskFromMaintenanceWindowRequest _
instance showDeregisterTaskFromMaintenanceWindowRequest :: Show DeregisterTaskFromMaintenanceWindowRequest where
  show = genericShow
instance decodeDeregisterTaskFromMaintenanceWindowRequest :: Decode DeregisterTaskFromMaintenanceWindowRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterTaskFromMaintenanceWindowRequest :: Encode DeregisterTaskFromMaintenanceWindowRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterTaskFromMaintenanceWindowResult = DeregisterTaskFromMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "WindowTaskId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskId)
  }
derive instance newtypeDeregisterTaskFromMaintenanceWindowResult :: Newtype DeregisterTaskFromMaintenanceWindowResult _
derive instance repGenericDeregisterTaskFromMaintenanceWindowResult :: Generic DeregisterTaskFromMaintenanceWindowResult _
instance showDeregisterTaskFromMaintenanceWindowResult :: Show DeregisterTaskFromMaintenanceWindowResult where
  show = genericShow
instance decodeDeregisterTaskFromMaintenanceWindowResult :: Decode DeregisterTaskFromMaintenanceWindowResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterTaskFromMaintenanceWindowResult :: Encode DeregisterTaskFromMaintenanceWindowResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Filter for the DescribeActivation API.</p>
newtype DescribeActivationsFilter = DescribeActivationsFilter 
  { "FilterKey" :: NullOrUndefined.NullOrUndefined (DescribeActivationsFilterKeys)
  , "FilterValues" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeDescribeActivationsFilter :: Newtype DescribeActivationsFilter _
derive instance repGenericDescribeActivationsFilter :: Generic DescribeActivationsFilter _
instance showDescribeActivationsFilter :: Show DescribeActivationsFilter where
  show = genericShow
instance decodeDescribeActivationsFilter :: Decode DescribeActivationsFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivationsFilter :: Encode DescribeActivationsFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeActivationsFilterKeys = DescribeActivationsFilterKeys String
derive instance newtypeDescribeActivationsFilterKeys :: Newtype DescribeActivationsFilterKeys _
derive instance repGenericDescribeActivationsFilterKeys :: Generic DescribeActivationsFilterKeys _
instance showDescribeActivationsFilterKeys :: Show DescribeActivationsFilterKeys where
  show = genericShow
instance decodeDescribeActivationsFilterKeys :: Decode DescribeActivationsFilterKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivationsFilterKeys :: Encode DescribeActivationsFilterKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeActivationsFilterList = DescribeActivationsFilterList (Array DescribeActivationsFilter)
derive instance newtypeDescribeActivationsFilterList :: Newtype DescribeActivationsFilterList _
derive instance repGenericDescribeActivationsFilterList :: Generic DescribeActivationsFilterList _
instance showDescribeActivationsFilterList :: Show DescribeActivationsFilterList where
  show = genericShow
instance decodeDescribeActivationsFilterList :: Decode DescribeActivationsFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivationsFilterList :: Encode DescribeActivationsFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeActivationsRequest = DescribeActivationsRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (DescribeActivationsFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeActivationsRequest :: Newtype DescribeActivationsRequest _
derive instance repGenericDescribeActivationsRequest :: Generic DescribeActivationsRequest _
instance showDescribeActivationsRequest :: Show DescribeActivationsRequest where
  show = genericShow
instance decodeDescribeActivationsRequest :: Decode DescribeActivationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivationsRequest :: Encode DescribeActivationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeActivationsResult = DescribeActivationsResult 
  { "ActivationList" :: NullOrUndefined.NullOrUndefined (ActivationList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeActivationsResult :: Newtype DescribeActivationsResult _
derive instance repGenericDescribeActivationsResult :: Generic DescribeActivationsResult _
instance showDescribeActivationsResult :: Show DescribeActivationsResult where
  show = genericShow
instance decodeDescribeActivationsResult :: Decode DescribeActivationsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivationsResult :: Encode DescribeActivationsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAssociationRequest = DescribeAssociationRequest 
  { "Name" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "AssociationId" :: NullOrUndefined.NullOrUndefined (AssociationId)
  , "AssociationVersion" :: NullOrUndefined.NullOrUndefined (AssociationVersion)
  }
derive instance newtypeDescribeAssociationRequest :: Newtype DescribeAssociationRequest _
derive instance repGenericDescribeAssociationRequest :: Generic DescribeAssociationRequest _
instance showDescribeAssociationRequest :: Show DescribeAssociationRequest where
  show = genericShow
instance decodeDescribeAssociationRequest :: Decode DescribeAssociationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAssociationRequest :: Encode DescribeAssociationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAssociationResult = DescribeAssociationResult 
  { "AssociationDescription" :: NullOrUndefined.NullOrUndefined (AssociationDescription)
  }
derive instance newtypeDescribeAssociationResult :: Newtype DescribeAssociationResult _
derive instance repGenericDescribeAssociationResult :: Generic DescribeAssociationResult _
instance showDescribeAssociationResult :: Show DescribeAssociationResult where
  show = genericShow
instance decodeDescribeAssociationResult :: Decode DescribeAssociationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAssociationResult :: Encode DescribeAssociationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAutomationExecutionsRequest = DescribeAutomationExecutionsRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (AutomationExecutionFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAutomationExecutionsRequest :: Newtype DescribeAutomationExecutionsRequest _
derive instance repGenericDescribeAutomationExecutionsRequest :: Generic DescribeAutomationExecutionsRequest _
instance showDescribeAutomationExecutionsRequest :: Show DescribeAutomationExecutionsRequest where
  show = genericShow
instance decodeDescribeAutomationExecutionsRequest :: Decode DescribeAutomationExecutionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAutomationExecutionsRequest :: Encode DescribeAutomationExecutionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAutomationExecutionsResult = DescribeAutomationExecutionsResult 
  { "AutomationExecutionMetadataList" :: NullOrUndefined.NullOrUndefined (AutomationExecutionMetadataList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAutomationExecutionsResult :: Newtype DescribeAutomationExecutionsResult _
derive instance repGenericDescribeAutomationExecutionsResult :: Generic DescribeAutomationExecutionsResult _
instance showDescribeAutomationExecutionsResult :: Show DescribeAutomationExecutionsResult where
  show = genericShow
instance decodeDescribeAutomationExecutionsResult :: Decode DescribeAutomationExecutionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAutomationExecutionsResult :: Encode DescribeAutomationExecutionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAutomationStepExecutionsRequest = DescribeAutomationStepExecutionsRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  , "Filters" :: NullOrUndefined.NullOrUndefined (StepExecutionFilterList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "ReverseOrder" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDescribeAutomationStepExecutionsRequest :: Newtype DescribeAutomationStepExecutionsRequest _
derive instance repGenericDescribeAutomationStepExecutionsRequest :: Generic DescribeAutomationStepExecutionsRequest _
instance showDescribeAutomationStepExecutionsRequest :: Show DescribeAutomationStepExecutionsRequest where
  show = genericShow
instance decodeDescribeAutomationStepExecutionsRequest :: Decode DescribeAutomationStepExecutionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAutomationStepExecutionsRequest :: Encode DescribeAutomationStepExecutionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAutomationStepExecutionsResult = DescribeAutomationStepExecutionsResult 
  { "StepExecutions" :: NullOrUndefined.NullOrUndefined (StepExecutionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAutomationStepExecutionsResult :: Newtype DescribeAutomationStepExecutionsResult _
derive instance repGenericDescribeAutomationStepExecutionsResult :: Generic DescribeAutomationStepExecutionsResult _
instance showDescribeAutomationStepExecutionsResult :: Show DescribeAutomationStepExecutionsResult where
  show = genericShow
instance decodeDescribeAutomationStepExecutionsResult :: Decode DescribeAutomationStepExecutionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAutomationStepExecutionsResult :: Encode DescribeAutomationStepExecutionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAvailablePatchesRequest = DescribeAvailablePatchesRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (PatchOrchestratorFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PatchBaselineMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAvailablePatchesRequest :: Newtype DescribeAvailablePatchesRequest _
derive instance repGenericDescribeAvailablePatchesRequest :: Generic DescribeAvailablePatchesRequest _
instance showDescribeAvailablePatchesRequest :: Show DescribeAvailablePatchesRequest where
  show = genericShow
instance decodeDescribeAvailablePatchesRequest :: Decode DescribeAvailablePatchesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAvailablePatchesRequest :: Encode DescribeAvailablePatchesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAvailablePatchesResult = DescribeAvailablePatchesResult 
  { "Patches" :: NullOrUndefined.NullOrUndefined (PatchList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAvailablePatchesResult :: Newtype DescribeAvailablePatchesResult _
derive instance repGenericDescribeAvailablePatchesResult :: Generic DescribeAvailablePatchesResult _
instance showDescribeAvailablePatchesResult :: Show DescribeAvailablePatchesResult where
  show = genericShow
instance decodeDescribeAvailablePatchesResult :: Decode DescribeAvailablePatchesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAvailablePatchesResult :: Encode DescribeAvailablePatchesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDocumentPermissionRequest = DescribeDocumentPermissionRequest 
  { "Name" :: (DocumentName)
  , "PermissionType" :: (DocumentPermissionType)
  }
derive instance newtypeDescribeDocumentPermissionRequest :: Newtype DescribeDocumentPermissionRequest _
derive instance repGenericDescribeDocumentPermissionRequest :: Generic DescribeDocumentPermissionRequest _
instance showDescribeDocumentPermissionRequest :: Show DescribeDocumentPermissionRequest where
  show = genericShow
instance decodeDescribeDocumentPermissionRequest :: Decode DescribeDocumentPermissionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDocumentPermissionRequest :: Encode DescribeDocumentPermissionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDocumentPermissionResponse = DescribeDocumentPermissionResponse 
  { "AccountIds" :: NullOrUndefined.NullOrUndefined (AccountIdList)
  }
derive instance newtypeDescribeDocumentPermissionResponse :: Newtype DescribeDocumentPermissionResponse _
derive instance repGenericDescribeDocumentPermissionResponse :: Generic DescribeDocumentPermissionResponse _
instance showDescribeDocumentPermissionResponse :: Show DescribeDocumentPermissionResponse where
  show = genericShow
instance decodeDescribeDocumentPermissionResponse :: Decode DescribeDocumentPermissionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDocumentPermissionResponse :: Encode DescribeDocumentPermissionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDocumentRequest = DescribeDocumentRequest 
  { "Name" :: (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  }
derive instance newtypeDescribeDocumentRequest :: Newtype DescribeDocumentRequest _
derive instance repGenericDescribeDocumentRequest :: Generic DescribeDocumentRequest _
instance showDescribeDocumentRequest :: Show DescribeDocumentRequest where
  show = genericShow
instance decodeDescribeDocumentRequest :: Decode DescribeDocumentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDocumentRequest :: Encode DescribeDocumentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDocumentResult = DescribeDocumentResult 
  { "Document" :: NullOrUndefined.NullOrUndefined (DocumentDescription)
  }
derive instance newtypeDescribeDocumentResult :: Newtype DescribeDocumentResult _
derive instance repGenericDescribeDocumentResult :: Generic DescribeDocumentResult _
instance showDescribeDocumentResult :: Show DescribeDocumentResult where
  show = genericShow
instance decodeDescribeDocumentResult :: Decode DescribeDocumentResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDocumentResult :: Encode DescribeDocumentResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEffectiveInstanceAssociationsRequest = DescribeEffectiveInstanceAssociationsRequest 
  { "InstanceId" :: (InstanceId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (EffectiveInstanceAssociationMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeEffectiveInstanceAssociationsRequest :: Newtype DescribeEffectiveInstanceAssociationsRequest _
derive instance repGenericDescribeEffectiveInstanceAssociationsRequest :: Generic DescribeEffectiveInstanceAssociationsRequest _
instance showDescribeEffectiveInstanceAssociationsRequest :: Show DescribeEffectiveInstanceAssociationsRequest where
  show = genericShow
instance decodeDescribeEffectiveInstanceAssociationsRequest :: Decode DescribeEffectiveInstanceAssociationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEffectiveInstanceAssociationsRequest :: Encode DescribeEffectiveInstanceAssociationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEffectiveInstanceAssociationsResult = DescribeEffectiveInstanceAssociationsResult 
  { "Associations" :: NullOrUndefined.NullOrUndefined (InstanceAssociationList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeEffectiveInstanceAssociationsResult :: Newtype DescribeEffectiveInstanceAssociationsResult _
derive instance repGenericDescribeEffectiveInstanceAssociationsResult :: Generic DescribeEffectiveInstanceAssociationsResult _
instance showDescribeEffectiveInstanceAssociationsResult :: Show DescribeEffectiveInstanceAssociationsResult where
  show = genericShow
instance decodeDescribeEffectiveInstanceAssociationsResult :: Decode DescribeEffectiveInstanceAssociationsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEffectiveInstanceAssociationsResult :: Encode DescribeEffectiveInstanceAssociationsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEffectivePatchesForPatchBaselineRequest = DescribeEffectivePatchesForPatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PatchBaselineMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeEffectivePatchesForPatchBaselineRequest :: Newtype DescribeEffectivePatchesForPatchBaselineRequest _
derive instance repGenericDescribeEffectivePatchesForPatchBaselineRequest :: Generic DescribeEffectivePatchesForPatchBaselineRequest _
instance showDescribeEffectivePatchesForPatchBaselineRequest :: Show DescribeEffectivePatchesForPatchBaselineRequest where
  show = genericShow
instance decodeDescribeEffectivePatchesForPatchBaselineRequest :: Decode DescribeEffectivePatchesForPatchBaselineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEffectivePatchesForPatchBaselineRequest :: Encode DescribeEffectivePatchesForPatchBaselineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEffectivePatchesForPatchBaselineResult = DescribeEffectivePatchesForPatchBaselineResult 
  { "EffectivePatches" :: NullOrUndefined.NullOrUndefined (EffectivePatchList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeEffectivePatchesForPatchBaselineResult :: Newtype DescribeEffectivePatchesForPatchBaselineResult _
derive instance repGenericDescribeEffectivePatchesForPatchBaselineResult :: Generic DescribeEffectivePatchesForPatchBaselineResult _
instance showDescribeEffectivePatchesForPatchBaselineResult :: Show DescribeEffectivePatchesForPatchBaselineResult where
  show = genericShow
instance decodeDescribeEffectivePatchesForPatchBaselineResult :: Decode DescribeEffectivePatchesForPatchBaselineResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEffectivePatchesForPatchBaselineResult :: Encode DescribeEffectivePatchesForPatchBaselineResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstanceAssociationsStatusRequest = DescribeInstanceAssociationsStatusRequest 
  { "InstanceId" :: (InstanceId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstanceAssociationsStatusRequest :: Newtype DescribeInstanceAssociationsStatusRequest _
derive instance repGenericDescribeInstanceAssociationsStatusRequest :: Generic DescribeInstanceAssociationsStatusRequest _
instance showDescribeInstanceAssociationsStatusRequest :: Show DescribeInstanceAssociationsStatusRequest where
  show = genericShow
instance decodeDescribeInstanceAssociationsStatusRequest :: Decode DescribeInstanceAssociationsStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstanceAssociationsStatusRequest :: Encode DescribeInstanceAssociationsStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstanceAssociationsStatusResult = DescribeInstanceAssociationsStatusResult 
  { "InstanceAssociationStatusInfos" :: NullOrUndefined.NullOrUndefined (InstanceAssociationStatusInfos)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstanceAssociationsStatusResult :: Newtype DescribeInstanceAssociationsStatusResult _
derive instance repGenericDescribeInstanceAssociationsStatusResult :: Generic DescribeInstanceAssociationsStatusResult _
instance showDescribeInstanceAssociationsStatusResult :: Show DescribeInstanceAssociationsStatusResult where
  show = genericShow
instance decodeDescribeInstanceAssociationsStatusResult :: Decode DescribeInstanceAssociationsStatusResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstanceAssociationsStatusResult :: Encode DescribeInstanceAssociationsStatusResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstanceInformationRequest = DescribeInstanceInformationRequest 
  { "InstanceInformationFilterList" :: NullOrUndefined.NullOrUndefined (InstanceInformationFilterList)
  , "Filters" :: NullOrUndefined.NullOrUndefined (InstanceInformationStringFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResultsEC2Compatible)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstanceInformationRequest :: Newtype DescribeInstanceInformationRequest _
derive instance repGenericDescribeInstanceInformationRequest :: Generic DescribeInstanceInformationRequest _
instance showDescribeInstanceInformationRequest :: Show DescribeInstanceInformationRequest where
  show = genericShow
instance decodeDescribeInstanceInformationRequest :: Decode DescribeInstanceInformationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstanceInformationRequest :: Encode DescribeInstanceInformationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstanceInformationResult = DescribeInstanceInformationResult 
  { "InstanceInformationList" :: NullOrUndefined.NullOrUndefined (InstanceInformationList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstanceInformationResult :: Newtype DescribeInstanceInformationResult _
derive instance repGenericDescribeInstanceInformationResult :: Generic DescribeInstanceInformationResult _
instance showDescribeInstanceInformationResult :: Show DescribeInstanceInformationResult where
  show = genericShow
instance decodeDescribeInstanceInformationResult :: Decode DescribeInstanceInformationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstanceInformationResult :: Encode DescribeInstanceInformationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstancePatchStatesForPatchGroupRequest = DescribeInstancePatchStatesForPatchGroupRequest 
  { "PatchGroup" :: (PatchGroup)
  , "Filters" :: NullOrUndefined.NullOrUndefined (InstancePatchStateFilterList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PatchComplianceMaxResults)
  }
derive instance newtypeDescribeInstancePatchStatesForPatchGroupRequest :: Newtype DescribeInstancePatchStatesForPatchGroupRequest _
derive instance repGenericDescribeInstancePatchStatesForPatchGroupRequest :: Generic DescribeInstancePatchStatesForPatchGroupRequest _
instance showDescribeInstancePatchStatesForPatchGroupRequest :: Show DescribeInstancePatchStatesForPatchGroupRequest where
  show = genericShow
instance decodeDescribeInstancePatchStatesForPatchGroupRequest :: Decode DescribeInstancePatchStatesForPatchGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancePatchStatesForPatchGroupRequest :: Encode DescribeInstancePatchStatesForPatchGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstancePatchStatesForPatchGroupResult = DescribeInstancePatchStatesForPatchGroupResult 
  { "InstancePatchStates" :: NullOrUndefined.NullOrUndefined (InstancePatchStatesList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancePatchStatesForPatchGroupResult :: Newtype DescribeInstancePatchStatesForPatchGroupResult _
derive instance repGenericDescribeInstancePatchStatesForPatchGroupResult :: Generic DescribeInstancePatchStatesForPatchGroupResult _
instance showDescribeInstancePatchStatesForPatchGroupResult :: Show DescribeInstancePatchStatesForPatchGroupResult where
  show = genericShow
instance decodeDescribeInstancePatchStatesForPatchGroupResult :: Decode DescribeInstancePatchStatesForPatchGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancePatchStatesForPatchGroupResult :: Encode DescribeInstancePatchStatesForPatchGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstancePatchStatesRequest = DescribeInstancePatchStatesRequest 
  { "InstanceIds" :: (InstanceIdList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PatchComplianceMaxResults)
  }
derive instance newtypeDescribeInstancePatchStatesRequest :: Newtype DescribeInstancePatchStatesRequest _
derive instance repGenericDescribeInstancePatchStatesRequest :: Generic DescribeInstancePatchStatesRequest _
instance showDescribeInstancePatchStatesRequest :: Show DescribeInstancePatchStatesRequest where
  show = genericShow
instance decodeDescribeInstancePatchStatesRequest :: Decode DescribeInstancePatchStatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancePatchStatesRequest :: Encode DescribeInstancePatchStatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstancePatchStatesResult = DescribeInstancePatchStatesResult 
  { "InstancePatchStates" :: NullOrUndefined.NullOrUndefined (InstancePatchStateList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancePatchStatesResult :: Newtype DescribeInstancePatchStatesResult _
derive instance repGenericDescribeInstancePatchStatesResult :: Generic DescribeInstancePatchStatesResult _
instance showDescribeInstancePatchStatesResult :: Show DescribeInstancePatchStatesResult where
  show = genericShow
instance decodeDescribeInstancePatchStatesResult :: Decode DescribeInstancePatchStatesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancePatchStatesResult :: Encode DescribeInstancePatchStatesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstancePatchesRequest = DescribeInstancePatchesRequest 
  { "InstanceId" :: (InstanceId)
  , "Filters" :: NullOrUndefined.NullOrUndefined (PatchOrchestratorFilterList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PatchComplianceMaxResults)
  }
derive instance newtypeDescribeInstancePatchesRequest :: Newtype DescribeInstancePatchesRequest _
derive instance repGenericDescribeInstancePatchesRequest :: Generic DescribeInstancePatchesRequest _
instance showDescribeInstancePatchesRequest :: Show DescribeInstancePatchesRequest where
  show = genericShow
instance decodeDescribeInstancePatchesRequest :: Decode DescribeInstancePatchesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancePatchesRequest :: Encode DescribeInstancePatchesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeInstancePatchesResult = DescribeInstancePatchesResult 
  { "Patches" :: NullOrUndefined.NullOrUndefined (PatchComplianceDataList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancePatchesResult :: Newtype DescribeInstancePatchesResult _
derive instance repGenericDescribeInstancePatchesResult :: Generic DescribeInstancePatchesResult _
instance showDescribeInstancePatchesResult :: Show DescribeInstancePatchesResult where
  show = genericShow
instance decodeDescribeInstancePatchesResult :: Decode DescribeInstancePatchesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancePatchesResult :: Encode DescribeInstancePatchesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowExecutionTaskInvocationsRequest = DescribeMaintenanceWindowExecutionTaskInvocationsRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "TaskId" :: (MaintenanceWindowExecutionTaskId)
  , "Filters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionTaskInvocationsRequest :: Newtype DescribeMaintenanceWindowExecutionTaskInvocationsRequest _
derive instance repGenericDescribeMaintenanceWindowExecutionTaskInvocationsRequest :: Generic DescribeMaintenanceWindowExecutionTaskInvocationsRequest _
instance showDescribeMaintenanceWindowExecutionTaskInvocationsRequest :: Show DescribeMaintenanceWindowExecutionTaskInvocationsRequest where
  show = genericShow
instance decodeDescribeMaintenanceWindowExecutionTaskInvocationsRequest :: Decode DescribeMaintenanceWindowExecutionTaskInvocationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowExecutionTaskInvocationsRequest :: Encode DescribeMaintenanceWindowExecutionTaskInvocationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowExecutionTaskInvocationsResult = DescribeMaintenanceWindowExecutionTaskInvocationsResult 
  { "WindowExecutionTaskInvocationIdentities" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskInvocationIdentityList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionTaskInvocationsResult :: Newtype DescribeMaintenanceWindowExecutionTaskInvocationsResult _
derive instance repGenericDescribeMaintenanceWindowExecutionTaskInvocationsResult :: Generic DescribeMaintenanceWindowExecutionTaskInvocationsResult _
instance showDescribeMaintenanceWindowExecutionTaskInvocationsResult :: Show DescribeMaintenanceWindowExecutionTaskInvocationsResult where
  show = genericShow
instance decodeDescribeMaintenanceWindowExecutionTaskInvocationsResult :: Decode DescribeMaintenanceWindowExecutionTaskInvocationsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowExecutionTaskInvocationsResult :: Encode DescribeMaintenanceWindowExecutionTaskInvocationsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowExecutionTasksRequest = DescribeMaintenanceWindowExecutionTasksRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "Filters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionTasksRequest :: Newtype DescribeMaintenanceWindowExecutionTasksRequest _
derive instance repGenericDescribeMaintenanceWindowExecutionTasksRequest :: Generic DescribeMaintenanceWindowExecutionTasksRequest _
instance showDescribeMaintenanceWindowExecutionTasksRequest :: Show DescribeMaintenanceWindowExecutionTasksRequest where
  show = genericShow
instance decodeDescribeMaintenanceWindowExecutionTasksRequest :: Decode DescribeMaintenanceWindowExecutionTasksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowExecutionTasksRequest :: Encode DescribeMaintenanceWindowExecutionTasksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowExecutionTasksResult = DescribeMaintenanceWindowExecutionTasksResult 
  { "WindowExecutionTaskIdentities" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskIdentityList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionTasksResult :: Newtype DescribeMaintenanceWindowExecutionTasksResult _
derive instance repGenericDescribeMaintenanceWindowExecutionTasksResult :: Generic DescribeMaintenanceWindowExecutionTasksResult _
instance showDescribeMaintenanceWindowExecutionTasksResult :: Show DescribeMaintenanceWindowExecutionTasksResult where
  show = genericShow
instance decodeDescribeMaintenanceWindowExecutionTasksResult :: Decode DescribeMaintenanceWindowExecutionTasksResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowExecutionTasksResult :: Encode DescribeMaintenanceWindowExecutionTasksResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowExecutionsRequest = DescribeMaintenanceWindowExecutionsRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Filters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionsRequest :: Newtype DescribeMaintenanceWindowExecutionsRequest _
derive instance repGenericDescribeMaintenanceWindowExecutionsRequest :: Generic DescribeMaintenanceWindowExecutionsRequest _
instance showDescribeMaintenanceWindowExecutionsRequest :: Show DescribeMaintenanceWindowExecutionsRequest where
  show = genericShow
instance decodeDescribeMaintenanceWindowExecutionsRequest :: Decode DescribeMaintenanceWindowExecutionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowExecutionsRequest :: Encode DescribeMaintenanceWindowExecutionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowExecutionsResult = DescribeMaintenanceWindowExecutionsResult 
  { "WindowExecutions" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowExecutionsResult :: Newtype DescribeMaintenanceWindowExecutionsResult _
derive instance repGenericDescribeMaintenanceWindowExecutionsResult :: Generic DescribeMaintenanceWindowExecutionsResult _
instance showDescribeMaintenanceWindowExecutionsResult :: Show DescribeMaintenanceWindowExecutionsResult where
  show = genericShow
instance decodeDescribeMaintenanceWindowExecutionsResult :: Decode DescribeMaintenanceWindowExecutionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowExecutionsResult :: Encode DescribeMaintenanceWindowExecutionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowTargetsRequest = DescribeMaintenanceWindowTargetsRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Filters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowTargetsRequest :: Newtype DescribeMaintenanceWindowTargetsRequest _
derive instance repGenericDescribeMaintenanceWindowTargetsRequest :: Generic DescribeMaintenanceWindowTargetsRequest _
instance showDescribeMaintenanceWindowTargetsRequest :: Show DescribeMaintenanceWindowTargetsRequest where
  show = genericShow
instance decodeDescribeMaintenanceWindowTargetsRequest :: Decode DescribeMaintenanceWindowTargetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowTargetsRequest :: Encode DescribeMaintenanceWindowTargetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowTargetsResult = DescribeMaintenanceWindowTargetsResult 
  { "Targets" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTargetList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowTargetsResult :: Newtype DescribeMaintenanceWindowTargetsResult _
derive instance repGenericDescribeMaintenanceWindowTargetsResult :: Generic DescribeMaintenanceWindowTargetsResult _
instance showDescribeMaintenanceWindowTargetsResult :: Show DescribeMaintenanceWindowTargetsResult where
  show = genericShow
instance decodeDescribeMaintenanceWindowTargetsResult :: Decode DescribeMaintenanceWindowTargetsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowTargetsResult :: Encode DescribeMaintenanceWindowTargetsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowTasksRequest = DescribeMaintenanceWindowTasksRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Filters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowTasksRequest :: Newtype DescribeMaintenanceWindowTasksRequest _
derive instance repGenericDescribeMaintenanceWindowTasksRequest :: Generic DescribeMaintenanceWindowTasksRequest _
instance showDescribeMaintenanceWindowTasksRequest :: Show DescribeMaintenanceWindowTasksRequest where
  show = genericShow
instance decodeDescribeMaintenanceWindowTasksRequest :: Decode DescribeMaintenanceWindowTasksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowTasksRequest :: Encode DescribeMaintenanceWindowTasksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowTasksResult = DescribeMaintenanceWindowTasksResult 
  { "Tasks" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowTasksResult :: Newtype DescribeMaintenanceWindowTasksResult _
derive instance repGenericDescribeMaintenanceWindowTasksResult :: Generic DescribeMaintenanceWindowTasksResult _
instance showDescribeMaintenanceWindowTasksResult :: Show DescribeMaintenanceWindowTasksResult where
  show = genericShow
instance decodeDescribeMaintenanceWindowTasksResult :: Decode DescribeMaintenanceWindowTasksResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowTasksResult :: Encode DescribeMaintenanceWindowTasksResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowsRequest = DescribeMaintenanceWindowsRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowsRequest :: Newtype DescribeMaintenanceWindowsRequest _
derive instance repGenericDescribeMaintenanceWindowsRequest :: Generic DescribeMaintenanceWindowsRequest _
instance showDescribeMaintenanceWindowsRequest :: Show DescribeMaintenanceWindowsRequest where
  show = genericShow
instance decodeDescribeMaintenanceWindowsRequest :: Decode DescribeMaintenanceWindowsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowsRequest :: Encode DescribeMaintenanceWindowsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMaintenanceWindowsResult = DescribeMaintenanceWindowsResult 
  { "WindowIdentities" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowIdentityList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMaintenanceWindowsResult :: Newtype DescribeMaintenanceWindowsResult _
derive instance repGenericDescribeMaintenanceWindowsResult :: Generic DescribeMaintenanceWindowsResult _
instance showDescribeMaintenanceWindowsResult :: Show DescribeMaintenanceWindowsResult where
  show = genericShow
instance decodeDescribeMaintenanceWindowsResult :: Decode DescribeMaintenanceWindowsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMaintenanceWindowsResult :: Encode DescribeMaintenanceWindowsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeParametersRequest = DescribeParametersRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (ParametersFilterList)
  , "ParameterFilters" :: NullOrUndefined.NullOrUndefined (ParameterStringFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeParametersRequest :: Newtype DescribeParametersRequest _
derive instance repGenericDescribeParametersRequest :: Generic DescribeParametersRequest _
instance showDescribeParametersRequest :: Show DescribeParametersRequest where
  show = genericShow
instance decodeDescribeParametersRequest :: Decode DescribeParametersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeParametersRequest :: Encode DescribeParametersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeParametersResult = DescribeParametersResult 
  { "Parameters" :: NullOrUndefined.NullOrUndefined (ParameterMetadataList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeParametersResult :: Newtype DescribeParametersResult _
derive instance repGenericDescribeParametersResult :: Generic DescribeParametersResult _
instance showDescribeParametersResult :: Show DescribeParametersResult where
  show = genericShow
instance decodeDescribeParametersResult :: Decode DescribeParametersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeParametersResult :: Encode DescribeParametersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePatchBaselinesRequest = DescribePatchBaselinesRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (PatchOrchestratorFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PatchBaselineMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePatchBaselinesRequest :: Newtype DescribePatchBaselinesRequest _
derive instance repGenericDescribePatchBaselinesRequest :: Generic DescribePatchBaselinesRequest _
instance showDescribePatchBaselinesRequest :: Show DescribePatchBaselinesRequest where
  show = genericShow
instance decodeDescribePatchBaselinesRequest :: Decode DescribePatchBaselinesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePatchBaselinesRequest :: Encode DescribePatchBaselinesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePatchBaselinesResult = DescribePatchBaselinesResult 
  { "BaselineIdentities" :: NullOrUndefined.NullOrUndefined (PatchBaselineIdentityList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePatchBaselinesResult :: Newtype DescribePatchBaselinesResult _
derive instance repGenericDescribePatchBaselinesResult :: Generic DescribePatchBaselinesResult _
instance showDescribePatchBaselinesResult :: Show DescribePatchBaselinesResult where
  show = genericShow
instance decodeDescribePatchBaselinesResult :: Decode DescribePatchBaselinesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePatchBaselinesResult :: Encode DescribePatchBaselinesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePatchGroupStateRequest = DescribePatchGroupStateRequest 
  { "PatchGroup" :: (PatchGroup)
  }
derive instance newtypeDescribePatchGroupStateRequest :: Newtype DescribePatchGroupStateRequest _
derive instance repGenericDescribePatchGroupStateRequest :: Generic DescribePatchGroupStateRequest _
instance showDescribePatchGroupStateRequest :: Show DescribePatchGroupStateRequest where
  show = genericShow
instance decodeDescribePatchGroupStateRequest :: Decode DescribePatchGroupStateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePatchGroupStateRequest :: Encode DescribePatchGroupStateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePatchGroupStateResult = DescribePatchGroupStateResult 
  { "Instances" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstancesWithInstalledPatches" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstancesWithInstalledOtherPatches" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstancesWithMissingPatches" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstancesWithFailedPatches" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstancesWithNotApplicablePatches" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDescribePatchGroupStateResult :: Newtype DescribePatchGroupStateResult _
derive instance repGenericDescribePatchGroupStateResult :: Generic DescribePatchGroupStateResult _
instance showDescribePatchGroupStateResult :: Show DescribePatchGroupStateResult where
  show = genericShow
instance decodeDescribePatchGroupStateResult :: Decode DescribePatchGroupStateResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePatchGroupStateResult :: Encode DescribePatchGroupStateResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePatchGroupsRequest = DescribePatchGroupsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (PatchBaselineMaxResults)
  , "Filters" :: NullOrUndefined.NullOrUndefined (PatchOrchestratorFilterList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePatchGroupsRequest :: Newtype DescribePatchGroupsRequest _
derive instance repGenericDescribePatchGroupsRequest :: Generic DescribePatchGroupsRequest _
instance showDescribePatchGroupsRequest :: Show DescribePatchGroupsRequest where
  show = genericShow
instance decodeDescribePatchGroupsRequest :: Decode DescribePatchGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePatchGroupsRequest :: Encode DescribePatchGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePatchGroupsResult = DescribePatchGroupsResult 
  { "Mappings" :: NullOrUndefined.NullOrUndefined (PatchGroupPatchBaselineMappingList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribePatchGroupsResult :: Newtype DescribePatchGroupsResult _
derive instance repGenericDescribePatchGroupsResult :: Generic DescribePatchGroupsResult _
instance showDescribePatchGroupsResult :: Show DescribePatchGroupsResult where
  show = genericShow
instance decodeDescribePatchGroupsResult :: Decode DescribePatchGroupsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePatchGroupsResult :: Encode DescribePatchGroupsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescriptionInDocument = DescriptionInDocument String
derive instance newtypeDescriptionInDocument :: Newtype DescriptionInDocument _
derive instance repGenericDescriptionInDocument :: Generic DescriptionInDocument _
instance showDescriptionInDocument :: Show DescriptionInDocument where
  show = genericShow
instance decodeDescriptionInDocument :: Decode DescriptionInDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescriptionInDocument :: Encode DescriptionInDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentARN = DocumentARN String
derive instance newtypeDocumentARN :: Newtype DocumentARN _
derive instance repGenericDocumentARN :: Generic DocumentARN _
instance showDocumentARN :: Show DocumentARN where
  show = genericShow
instance decodeDocumentARN :: Decode DocumentARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentARN :: Encode DocumentARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified document already exists.</p>
newtype DocumentAlreadyExists = DocumentAlreadyExists 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDocumentAlreadyExists :: Newtype DocumentAlreadyExists _
derive instance repGenericDocumentAlreadyExists :: Generic DocumentAlreadyExists _
instance showDocumentAlreadyExists :: Show DocumentAlreadyExists where
  show = genericShow
instance decodeDocumentAlreadyExists :: Decode DocumentAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentAlreadyExists :: Encode DocumentAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentContent = DocumentContent String
derive instance newtypeDocumentContent :: Newtype DocumentContent _
derive instance repGenericDocumentContent :: Generic DocumentContent _
instance showDocumentContent :: Show DocumentContent where
  show = genericShow
instance decodeDocumentContent :: Decode DocumentContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentContent :: Encode DocumentContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A default version of a document.</p>
newtype DocumentDefaultVersionDescription = DocumentDefaultVersionDescription 
  { "Name" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "DefaultVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  }
derive instance newtypeDocumentDefaultVersionDescription :: Newtype DocumentDefaultVersionDescription _
derive instance repGenericDocumentDefaultVersionDescription :: Generic DocumentDefaultVersionDescription _
instance showDocumentDefaultVersionDescription :: Show DocumentDefaultVersionDescription where
  show = genericShow
instance decodeDocumentDefaultVersionDescription :: Decode DocumentDefaultVersionDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentDefaultVersionDescription :: Encode DocumentDefaultVersionDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a Systems Manager document. </p>
newtype DocumentDescription = DocumentDescription 
  { "Sha1" :: NullOrUndefined.NullOrUndefined (DocumentSha1)
  , "Hash" :: NullOrUndefined.NullOrUndefined (DocumentHash)
  , "HashType" :: NullOrUndefined.NullOrUndefined (DocumentHashType)
  , "Name" :: NullOrUndefined.NullOrUndefined (DocumentARN)
  , "Owner" :: NullOrUndefined.NullOrUndefined (DocumentOwner)
  , "CreatedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined.NullOrUndefined (DocumentStatus)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionInDocument)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (DocumentParameterList)
  , "PlatformTypes" :: NullOrUndefined.NullOrUndefined (PlatformTypeList)
  , "DocumentType" :: NullOrUndefined.NullOrUndefined (DocumentType)
  , "SchemaVersion" :: NullOrUndefined.NullOrUndefined (DocumentSchemaVersion)
  , "LatestVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "DefaultVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "DocumentFormat" :: NullOrUndefined.NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined.NullOrUndefined (TargetType)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeDocumentDescription :: Newtype DocumentDescription _
derive instance repGenericDocumentDescription :: Generic DocumentDescription _
instance showDocumentDescription :: Show DocumentDescription where
  show = genericShow
instance decodeDocumentDescription :: Decode DocumentDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentDescription :: Encode DocumentDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a filter.</p>
newtype DocumentFilter = DocumentFilter 
  { "Key'" :: (DocumentFilterKey)
  , "Value'" :: (DocumentFilterValue)
  }
derive instance newtypeDocumentFilter :: Newtype DocumentFilter _
derive instance repGenericDocumentFilter :: Generic DocumentFilter _
instance showDocumentFilter :: Show DocumentFilter where
  show = genericShow
instance decodeDocumentFilter :: Decode DocumentFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentFilter :: Encode DocumentFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentFilterKey = DocumentFilterKey String
derive instance newtypeDocumentFilterKey :: Newtype DocumentFilterKey _
derive instance repGenericDocumentFilterKey :: Generic DocumentFilterKey _
instance showDocumentFilterKey :: Show DocumentFilterKey where
  show = genericShow
instance decodeDocumentFilterKey :: Decode DocumentFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentFilterKey :: Encode DocumentFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentFilterList = DocumentFilterList (Array DocumentFilter)
derive instance newtypeDocumentFilterList :: Newtype DocumentFilterList _
derive instance repGenericDocumentFilterList :: Generic DocumentFilterList _
instance showDocumentFilterList :: Show DocumentFilterList where
  show = genericShow
instance decodeDocumentFilterList :: Decode DocumentFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentFilterList :: Encode DocumentFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentFilterValue = DocumentFilterValue String
derive instance newtypeDocumentFilterValue :: Newtype DocumentFilterValue _
derive instance repGenericDocumentFilterValue :: Generic DocumentFilterValue _
instance showDocumentFilterValue :: Show DocumentFilterValue where
  show = genericShow
instance decodeDocumentFilterValue :: Decode DocumentFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentFilterValue :: Encode DocumentFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentFormat = DocumentFormat String
derive instance newtypeDocumentFormat :: Newtype DocumentFormat _
derive instance repGenericDocumentFormat :: Generic DocumentFormat _
instance showDocumentFormat :: Show DocumentFormat where
  show = genericShow
instance decodeDocumentFormat :: Decode DocumentFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentFormat :: Encode DocumentFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentHash = DocumentHash String
derive instance newtypeDocumentHash :: Newtype DocumentHash _
derive instance repGenericDocumentHash :: Generic DocumentHash _
instance showDocumentHash :: Show DocumentHash where
  show = genericShow
instance decodeDocumentHash :: Decode DocumentHash where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentHash :: Encode DocumentHash where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentHashType = DocumentHashType String
derive instance newtypeDocumentHashType :: Newtype DocumentHashType _
derive instance repGenericDocumentHashType :: Generic DocumentHashType _
instance showDocumentHashType :: Show DocumentHashType where
  show = genericShow
instance decodeDocumentHashType :: Decode DocumentHashType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentHashType :: Encode DocumentHashType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the name of a Systems Manager document.</p>
newtype DocumentIdentifier = DocumentIdentifier 
  { "Name" :: NullOrUndefined.NullOrUndefined (DocumentARN)
  , "Owner" :: NullOrUndefined.NullOrUndefined (DocumentOwner)
  , "PlatformTypes" :: NullOrUndefined.NullOrUndefined (PlatformTypeList)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "DocumentType" :: NullOrUndefined.NullOrUndefined (DocumentType)
  , "SchemaVersion" :: NullOrUndefined.NullOrUndefined (DocumentSchemaVersion)
  , "DocumentFormat" :: NullOrUndefined.NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined.NullOrUndefined (TargetType)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeDocumentIdentifier :: Newtype DocumentIdentifier _
derive instance repGenericDocumentIdentifier :: Generic DocumentIdentifier _
instance showDocumentIdentifier :: Show DocumentIdentifier where
  show = genericShow
instance decodeDocumentIdentifier :: Decode DocumentIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentIdentifier :: Encode DocumentIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentIdentifierList = DocumentIdentifierList (Array DocumentIdentifier)
derive instance newtypeDocumentIdentifierList :: Newtype DocumentIdentifierList _
derive instance repGenericDocumentIdentifierList :: Generic DocumentIdentifierList _
instance showDocumentIdentifierList :: Show DocumentIdentifierList where
  show = genericShow
instance decodeDocumentIdentifierList :: Decode DocumentIdentifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentIdentifierList :: Encode DocumentIdentifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more filters. Use a filter to return a more specific list of documents.</p> <p>For keys, you can specify one or more tags that have been applied to a document. </p> <p>Other valid values include Owner, Name, PlatformTypes, and DocumentType.</p> <p>Note that only one Owner can be specified in a request. For example: <code>Key=Owner,Values=Self</code>.</p> <p>If you use Name as a key, you can use a name prefix to return a list of documents. For example, in the AWS CLI, to return a list of all documents that begin with <code>Te</code>, run the following command:</p> <p> <code>aws ssm list-documents --filters Key=Name,Values=Te</code> </p> <p>If you specify more than two keys, only documents that are identified by all the tags are returned in the results. If you specify more than two values for a key, documents that are identified by any of the values are returned in the results.</p> <p>To specify a custom key and value pair, use the format <code>Key=tag:[tagName],Values=[valueName]</code>.</p> <p>For example, if you created a Key called region and are using the AWS CLI to call the <code>list-documents</code> command: </p> <p> <code>aws ssm list-documents --filters Key=tag:region,Values=east,west Key=Owner,Values=Self</code> </p>
newtype DocumentKeyValuesFilter = DocumentKeyValuesFilter 
  { "Key" :: NullOrUndefined.NullOrUndefined (DocumentKeyValuesFilterKey)
  , "Values" :: NullOrUndefined.NullOrUndefined (DocumentKeyValuesFilterValues)
  }
derive instance newtypeDocumentKeyValuesFilter :: Newtype DocumentKeyValuesFilter _
derive instance repGenericDocumentKeyValuesFilter :: Generic DocumentKeyValuesFilter _
instance showDocumentKeyValuesFilter :: Show DocumentKeyValuesFilter where
  show = genericShow
instance decodeDocumentKeyValuesFilter :: Decode DocumentKeyValuesFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentKeyValuesFilter :: Encode DocumentKeyValuesFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentKeyValuesFilterKey = DocumentKeyValuesFilterKey String
derive instance newtypeDocumentKeyValuesFilterKey :: Newtype DocumentKeyValuesFilterKey _
derive instance repGenericDocumentKeyValuesFilterKey :: Generic DocumentKeyValuesFilterKey _
instance showDocumentKeyValuesFilterKey :: Show DocumentKeyValuesFilterKey where
  show = genericShow
instance decodeDocumentKeyValuesFilterKey :: Decode DocumentKeyValuesFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentKeyValuesFilterKey :: Encode DocumentKeyValuesFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentKeyValuesFilterList = DocumentKeyValuesFilterList (Array DocumentKeyValuesFilter)
derive instance newtypeDocumentKeyValuesFilterList :: Newtype DocumentKeyValuesFilterList _
derive instance repGenericDocumentKeyValuesFilterList :: Generic DocumentKeyValuesFilterList _
instance showDocumentKeyValuesFilterList :: Show DocumentKeyValuesFilterList where
  show = genericShow
instance decodeDocumentKeyValuesFilterList :: Decode DocumentKeyValuesFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentKeyValuesFilterList :: Encode DocumentKeyValuesFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentKeyValuesFilterValue = DocumentKeyValuesFilterValue String
derive instance newtypeDocumentKeyValuesFilterValue :: Newtype DocumentKeyValuesFilterValue _
derive instance repGenericDocumentKeyValuesFilterValue :: Generic DocumentKeyValuesFilterValue _
instance showDocumentKeyValuesFilterValue :: Show DocumentKeyValuesFilterValue where
  show = genericShow
instance decodeDocumentKeyValuesFilterValue :: Decode DocumentKeyValuesFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentKeyValuesFilterValue :: Encode DocumentKeyValuesFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentKeyValuesFilterValues = DocumentKeyValuesFilterValues (Array DocumentKeyValuesFilterValue)
derive instance newtypeDocumentKeyValuesFilterValues :: Newtype DocumentKeyValuesFilterValues _
derive instance repGenericDocumentKeyValuesFilterValues :: Generic DocumentKeyValuesFilterValues _
instance showDocumentKeyValuesFilterValues :: Show DocumentKeyValuesFilterValues where
  show = genericShow
instance decodeDocumentKeyValuesFilterValues :: Decode DocumentKeyValuesFilterValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentKeyValuesFilterValues :: Encode DocumentKeyValuesFilterValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can have at most 200 active Systems Manager documents.</p>
newtype DocumentLimitExceeded = DocumentLimitExceeded 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDocumentLimitExceeded :: Newtype DocumentLimitExceeded _
derive instance repGenericDocumentLimitExceeded :: Generic DocumentLimitExceeded _
instance showDocumentLimitExceeded :: Show DocumentLimitExceeded where
  show = genericShow
instance decodeDocumentLimitExceeded :: Decode DocumentLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentLimitExceeded :: Encode DocumentLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentName = DocumentName String
derive instance newtypeDocumentName :: Newtype DocumentName _
derive instance repGenericDocumentName :: Generic DocumentName _
instance showDocumentName :: Show DocumentName where
  show = genericShow
instance decodeDocumentName :: Decode DocumentName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentName :: Encode DocumentName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentOwner = DocumentOwner String
derive instance newtypeDocumentOwner :: Newtype DocumentOwner _
derive instance repGenericDocumentOwner :: Generic DocumentOwner _
instance showDocumentOwner :: Show DocumentOwner where
  show = genericShow
instance decodeDocumentOwner :: Decode DocumentOwner where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentOwner :: Encode DocumentOwner where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Parameters specified in a System Manager document that execute on the server when the command is run. </p>
newtype DocumentParameter = DocumentParameter 
  { "Name" :: NullOrUndefined.NullOrUndefined (DocumentParameterName)
  , "Type" :: NullOrUndefined.NullOrUndefined (DocumentParameterType)
  , "Description" :: NullOrUndefined.NullOrUndefined (DocumentParameterDescrption)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (DocumentParameterDefaultValue)
  }
derive instance newtypeDocumentParameter :: Newtype DocumentParameter _
derive instance repGenericDocumentParameter :: Generic DocumentParameter _
instance showDocumentParameter :: Show DocumentParameter where
  show = genericShow
instance decodeDocumentParameter :: Decode DocumentParameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentParameter :: Encode DocumentParameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentParameterDefaultValue = DocumentParameterDefaultValue String
derive instance newtypeDocumentParameterDefaultValue :: Newtype DocumentParameterDefaultValue _
derive instance repGenericDocumentParameterDefaultValue :: Generic DocumentParameterDefaultValue _
instance showDocumentParameterDefaultValue :: Show DocumentParameterDefaultValue where
  show = genericShow
instance decodeDocumentParameterDefaultValue :: Decode DocumentParameterDefaultValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentParameterDefaultValue :: Encode DocumentParameterDefaultValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentParameterDescrption = DocumentParameterDescrption String
derive instance newtypeDocumentParameterDescrption :: Newtype DocumentParameterDescrption _
derive instance repGenericDocumentParameterDescrption :: Generic DocumentParameterDescrption _
instance showDocumentParameterDescrption :: Show DocumentParameterDescrption where
  show = genericShow
instance decodeDocumentParameterDescrption :: Decode DocumentParameterDescrption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentParameterDescrption :: Encode DocumentParameterDescrption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentParameterList = DocumentParameterList (Array DocumentParameter)
derive instance newtypeDocumentParameterList :: Newtype DocumentParameterList _
derive instance repGenericDocumentParameterList :: Generic DocumentParameterList _
instance showDocumentParameterList :: Show DocumentParameterList where
  show = genericShow
instance decodeDocumentParameterList :: Decode DocumentParameterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentParameterList :: Encode DocumentParameterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentParameterName = DocumentParameterName String
derive instance newtypeDocumentParameterName :: Newtype DocumentParameterName _
derive instance repGenericDocumentParameterName :: Generic DocumentParameterName _
instance showDocumentParameterName :: Show DocumentParameterName where
  show = genericShow
instance decodeDocumentParameterName :: Decode DocumentParameterName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentParameterName :: Encode DocumentParameterName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentParameterType = DocumentParameterType String
derive instance newtypeDocumentParameterType :: Newtype DocumentParameterType _
derive instance repGenericDocumentParameterType :: Generic DocumentParameterType _
instance showDocumentParameterType :: Show DocumentParameterType where
  show = genericShow
instance decodeDocumentParameterType :: Decode DocumentParameterType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentParameterType :: Encode DocumentParameterType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The document cannot be shared with more AWS user accounts. You can share a document with a maximum of 20 accounts. You can publicly share up to five documents. If you need to increase this limit, contact AWS Support.</p>
newtype DocumentPermissionLimit = DocumentPermissionLimit 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDocumentPermissionLimit :: Newtype DocumentPermissionLimit _
derive instance repGenericDocumentPermissionLimit :: Generic DocumentPermissionLimit _
instance showDocumentPermissionLimit :: Show DocumentPermissionLimit where
  show = genericShow
instance decodeDocumentPermissionLimit :: Decode DocumentPermissionLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentPermissionLimit :: Encode DocumentPermissionLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentPermissionType = DocumentPermissionType String
derive instance newtypeDocumentPermissionType :: Newtype DocumentPermissionType _
derive instance repGenericDocumentPermissionType :: Generic DocumentPermissionType _
instance showDocumentPermissionType :: Show DocumentPermissionType where
  show = genericShow
instance decodeDocumentPermissionType :: Decode DocumentPermissionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentPermissionType :: Encode DocumentPermissionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentSchemaVersion = DocumentSchemaVersion String
derive instance newtypeDocumentSchemaVersion :: Newtype DocumentSchemaVersion _
derive instance repGenericDocumentSchemaVersion :: Generic DocumentSchemaVersion _
instance showDocumentSchemaVersion :: Show DocumentSchemaVersion where
  show = genericShow
instance decodeDocumentSchemaVersion :: Decode DocumentSchemaVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentSchemaVersion :: Encode DocumentSchemaVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentSha1 = DocumentSha1 String
derive instance newtypeDocumentSha1 :: Newtype DocumentSha1 _
derive instance repGenericDocumentSha1 :: Generic DocumentSha1 _
instance showDocumentSha1 :: Show DocumentSha1 where
  show = genericShow
instance decodeDocumentSha1 :: Decode DocumentSha1 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentSha1 :: Encode DocumentSha1 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentStatus = DocumentStatus String
derive instance newtypeDocumentStatus :: Newtype DocumentStatus _
derive instance repGenericDocumentStatus :: Generic DocumentStatus _
instance showDocumentStatus :: Show DocumentStatus where
  show = genericShow
instance decodeDocumentStatus :: Decode DocumentStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentStatus :: Encode DocumentStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentType = DocumentType String
derive instance newtypeDocumentType :: Newtype DocumentType _
derive instance repGenericDocumentType :: Generic DocumentType _
instance showDocumentType :: Show DocumentType where
  show = genericShow
instance decodeDocumentType :: Decode DocumentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentType :: Encode DocumentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentVersion = DocumentVersion String
derive instance newtypeDocumentVersion :: Newtype DocumentVersion _
derive instance repGenericDocumentVersion :: Generic DocumentVersion _
instance showDocumentVersion :: Show DocumentVersion where
  show = genericShow
instance decodeDocumentVersion :: Decode DocumentVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentVersion :: Encode DocumentVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Version information about the document.</p>
newtype DocumentVersionInfo = DocumentVersionInfo 
  { "Name" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "CreatedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "IsDefaultVersion" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "DocumentFormat" :: NullOrUndefined.NullOrUndefined (DocumentFormat)
  }
derive instance newtypeDocumentVersionInfo :: Newtype DocumentVersionInfo _
derive instance repGenericDocumentVersionInfo :: Generic DocumentVersionInfo _
instance showDocumentVersionInfo :: Show DocumentVersionInfo where
  show = genericShow
instance decodeDocumentVersionInfo :: Decode DocumentVersionInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentVersionInfo :: Encode DocumentVersionInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The document has too many versions. Delete one or more document versions and try again.</p>
newtype DocumentVersionLimitExceeded = DocumentVersionLimitExceeded 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDocumentVersionLimitExceeded :: Newtype DocumentVersionLimitExceeded _
derive instance repGenericDocumentVersionLimitExceeded :: Generic DocumentVersionLimitExceeded _
instance showDocumentVersionLimitExceeded :: Show DocumentVersionLimitExceeded where
  show = genericShow
instance decodeDocumentVersionLimitExceeded :: Decode DocumentVersionLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentVersionLimitExceeded :: Encode DocumentVersionLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentVersionList = DocumentVersionList (Array DocumentVersionInfo)
derive instance newtypeDocumentVersionList :: Newtype DocumentVersionList _
derive instance repGenericDocumentVersionList :: Generic DocumentVersionList _
instance showDocumentVersionList :: Show DocumentVersionList where
  show = genericShow
instance decodeDocumentVersionList :: Decode DocumentVersionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentVersionList :: Encode DocumentVersionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentVersionNumber = DocumentVersionNumber String
derive instance newtypeDocumentVersionNumber :: Newtype DocumentVersionNumber _
derive instance repGenericDocumentVersionNumber :: Generic DocumentVersionNumber _
instance showDocumentVersionNumber :: Show DocumentVersionNumber where
  show = genericShow
instance decodeDocumentVersionNumber :: Decode DocumentVersionNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentVersionNumber :: Encode DocumentVersionNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Error returned when the ID specified for a resource, such as a Maintenance Window or Patch baseline, doesn't exist.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>
newtype DoesNotExistException = DoesNotExistException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDoesNotExistException :: Newtype DoesNotExistException _
derive instance repGenericDoesNotExistException :: Generic DoesNotExistException _
instance showDoesNotExistException :: Show DoesNotExistException where
  show = genericShow
instance decodeDoesNotExistException :: Decode DoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDoesNotExistException :: Encode DoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The content of the association document matches another document. Change the content of the document and try again.</p>
newtype DuplicateDocumentContent = DuplicateDocumentContent 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDuplicateDocumentContent :: Newtype DuplicateDocumentContent _
derive instance repGenericDuplicateDocumentContent :: Generic DuplicateDocumentContent _
instance showDuplicateDocumentContent :: Show DuplicateDocumentContent where
  show = genericShow
instance decodeDuplicateDocumentContent :: Decode DuplicateDocumentContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateDocumentContent :: Encode DuplicateDocumentContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot specify an instance ID in more than one association.</p>
newtype DuplicateInstanceId = DuplicateInstanceId Types.NoArguments
derive instance newtypeDuplicateInstanceId :: Newtype DuplicateInstanceId _
derive instance repGenericDuplicateInstanceId :: Generic DuplicateInstanceId _
instance showDuplicateInstanceId :: Show DuplicateInstanceId where
  show = genericShow
instance decodeDuplicateInstanceId :: Decode DuplicateInstanceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateInstanceId :: Encode DuplicateInstanceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EffectiveInstanceAssociationMaxResults = EffectiveInstanceAssociationMaxResults Int
derive instance newtypeEffectiveInstanceAssociationMaxResults :: Newtype EffectiveInstanceAssociationMaxResults _
derive instance repGenericEffectiveInstanceAssociationMaxResults :: Generic EffectiveInstanceAssociationMaxResults _
instance showEffectiveInstanceAssociationMaxResults :: Show EffectiveInstanceAssociationMaxResults where
  show = genericShow
instance decodeEffectiveInstanceAssociationMaxResults :: Decode EffectiveInstanceAssociationMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEffectiveInstanceAssociationMaxResults :: Encode EffectiveInstanceAssociationMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The EffectivePatch structure defines metadata about a patch along with the approval state of the patch in a particular patch baseline. The approval state includes information about whether the patch is currently approved, due to be approved by a rule, explicitly approved, or explicitly rejected and the date the patch was or will be approved.</p>
newtype EffectivePatch = EffectivePatch 
  { "Patch" :: NullOrUndefined.NullOrUndefined (Patch)
  , "PatchStatus" :: NullOrUndefined.NullOrUndefined (PatchStatus)
  }
derive instance newtypeEffectivePatch :: Newtype EffectivePatch _
derive instance repGenericEffectivePatch :: Generic EffectivePatch _
instance showEffectivePatch :: Show EffectivePatch where
  show = genericShow
instance decodeEffectivePatch :: Decode EffectivePatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEffectivePatch :: Encode EffectivePatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EffectivePatchList = EffectivePatchList (Array EffectivePatch)
derive instance newtypeEffectivePatchList :: Newtype EffectivePatchList _
derive instance repGenericEffectivePatchList :: Generic EffectivePatchList _
instance showEffectivePatchList :: Show EffectivePatchList where
  show = genericShow
instance decodeEffectivePatchList :: Decode EffectivePatchList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEffectivePatchList :: Encode EffectivePatchList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorCount = ErrorCount Int
derive instance newtypeErrorCount :: Newtype ErrorCount _
derive instance repGenericErrorCount :: Generic ErrorCount _
instance showErrorCount :: Show ErrorCount where
  show = genericShow
instance decodeErrorCount :: Decode ErrorCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCount :: Encode ErrorCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionMode = ExecutionMode String
derive instance newtypeExecutionMode :: Newtype ExecutionMode _
derive instance repGenericExecutionMode :: Generic ExecutionMode _
instance showExecutionMode :: Show ExecutionMode where
  show = genericShow
instance decodeExecutionMode :: Decode ExecutionMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionMode :: Encode ExecutionMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExpirationDate = ExpirationDate Number
derive instance newtypeExpirationDate :: Newtype ExpirationDate _
derive instance repGenericExpirationDate :: Generic ExpirationDate _
instance showExpirationDate :: Show ExpirationDate where
  show = genericShow
instance decodeExpirationDate :: Decode ExpirationDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpirationDate :: Encode ExpirationDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a failed association.</p>
newtype FailedCreateAssociation = FailedCreateAssociation 
  { "Entry" :: NullOrUndefined.NullOrUndefined (CreateAssociationBatchRequestEntry)
  , "Message" :: NullOrUndefined.NullOrUndefined (BatchErrorMessage)
  , "Fault" :: NullOrUndefined.NullOrUndefined (Fault)
  }
derive instance newtypeFailedCreateAssociation :: Newtype FailedCreateAssociation _
derive instance repGenericFailedCreateAssociation :: Generic FailedCreateAssociation _
instance showFailedCreateAssociation :: Show FailedCreateAssociation where
  show = genericShow
instance decodeFailedCreateAssociation :: Decode FailedCreateAssociation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedCreateAssociation :: Encode FailedCreateAssociation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailedCreateAssociationList = FailedCreateAssociationList (Array FailedCreateAssociation)
derive instance newtypeFailedCreateAssociationList :: Newtype FailedCreateAssociationList _
derive instance repGenericFailedCreateAssociationList :: Generic FailedCreateAssociationList _
instance showFailedCreateAssociationList :: Show FailedCreateAssociationList where
  show = genericShow
instance decodeFailedCreateAssociationList :: Decode FailedCreateAssociationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedCreateAssociationList :: Encode FailedCreateAssociationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an Automation failure.</p>
newtype FailureDetails = FailureDetails 
  { "FailureStage" :: NullOrUndefined.NullOrUndefined (String)
  , "FailureType" :: NullOrUndefined.NullOrUndefined (String)
  , "Details" :: NullOrUndefined.NullOrUndefined (AutomationParameterMap)
  }
derive instance newtypeFailureDetails :: Newtype FailureDetails _
derive instance repGenericFailureDetails :: Generic FailureDetails _
instance showFailureDetails :: Show FailureDetails where
  show = genericShow
instance decodeFailureDetails :: Decode FailureDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureDetails :: Encode FailureDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Fault = Fault String
derive instance newtypeFault :: Newtype Fault _
derive instance repGenericFault :: Generic Fault _
instance showFault :: Show Fault where
  show = genericShow
instance decodeFault :: Decode Fault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFault :: Encode Fault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You attempted to register a LAMBDA or STEP_FUNCTION task in a region where the corresponding service is not available. </p>
newtype FeatureNotAvailableException = FeatureNotAvailableException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeFeatureNotAvailableException :: Newtype FeatureNotAvailableException _
derive instance repGenericFeatureNotAvailableException :: Generic FeatureNotAvailableException _
instance showFeatureNotAvailableException :: Show FeatureNotAvailableException where
  show = genericShow
instance decodeFeatureNotAvailableException :: Decode FeatureNotAvailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFeatureNotAvailableException :: Encode FeatureNotAvailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAutomationExecutionRequest = GetAutomationExecutionRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  }
derive instance newtypeGetAutomationExecutionRequest :: Newtype GetAutomationExecutionRequest _
derive instance repGenericGetAutomationExecutionRequest :: Generic GetAutomationExecutionRequest _
instance showGetAutomationExecutionRequest :: Show GetAutomationExecutionRequest where
  show = genericShow
instance decodeGetAutomationExecutionRequest :: Decode GetAutomationExecutionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAutomationExecutionRequest :: Encode GetAutomationExecutionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetAutomationExecutionResult = GetAutomationExecutionResult 
  { "AutomationExecution" :: NullOrUndefined.NullOrUndefined (AutomationExecution)
  }
derive instance newtypeGetAutomationExecutionResult :: Newtype GetAutomationExecutionResult _
derive instance repGenericGetAutomationExecutionResult :: Generic GetAutomationExecutionResult _
instance showGetAutomationExecutionResult :: Show GetAutomationExecutionResult where
  show = genericShow
instance decodeGetAutomationExecutionResult :: Decode GetAutomationExecutionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAutomationExecutionResult :: Encode GetAutomationExecutionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCommandInvocationRequest = GetCommandInvocationRequest 
  { "CommandId" :: (CommandId)
  , "InstanceId" :: (InstanceId)
  , "PluginName" :: NullOrUndefined.NullOrUndefined (CommandPluginName)
  }
derive instance newtypeGetCommandInvocationRequest :: Newtype GetCommandInvocationRequest _
derive instance repGenericGetCommandInvocationRequest :: Generic GetCommandInvocationRequest _
instance showGetCommandInvocationRequest :: Show GetCommandInvocationRequest where
  show = genericShow
instance decodeGetCommandInvocationRequest :: Decode GetCommandInvocationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommandInvocationRequest :: Encode GetCommandInvocationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCommandInvocationResult = GetCommandInvocationResult 
  { "CommandId" :: NullOrUndefined.NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "Comment" :: NullOrUndefined.NullOrUndefined (Comment)
  , "DocumentName" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "PluginName" :: NullOrUndefined.NullOrUndefined (CommandPluginName)
  , "ResponseCode" :: NullOrUndefined.NullOrUndefined (ResponseCode)
  , "ExecutionStartDateTime" :: NullOrUndefined.NullOrUndefined (StringDateTime)
  , "ExecutionElapsedTime" :: NullOrUndefined.NullOrUndefined (StringDateTime)
  , "ExecutionEndDateTime" :: NullOrUndefined.NullOrUndefined (StringDateTime)
  , "Status" :: NullOrUndefined.NullOrUndefined (CommandInvocationStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (StatusDetails)
  , "StandardOutputContent" :: NullOrUndefined.NullOrUndefined (StandardOutputContent)
  , "StandardOutputUrl" :: NullOrUndefined.NullOrUndefined (Url)
  , "StandardErrorContent" :: NullOrUndefined.NullOrUndefined (StandardErrorContent)
  , "StandardErrorUrl" :: NullOrUndefined.NullOrUndefined (Url)
  }
derive instance newtypeGetCommandInvocationResult :: Newtype GetCommandInvocationResult _
derive instance repGenericGetCommandInvocationResult :: Generic GetCommandInvocationResult _
instance showGetCommandInvocationResult :: Show GetCommandInvocationResult where
  show = genericShow
instance decodeGetCommandInvocationResult :: Decode GetCommandInvocationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCommandInvocationResult :: Encode GetCommandInvocationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDefaultPatchBaselineRequest = GetDefaultPatchBaselineRequest 
  { "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  }
derive instance newtypeGetDefaultPatchBaselineRequest :: Newtype GetDefaultPatchBaselineRequest _
derive instance repGenericGetDefaultPatchBaselineRequest :: Generic GetDefaultPatchBaselineRequest _
instance showGetDefaultPatchBaselineRequest :: Show GetDefaultPatchBaselineRequest where
  show = genericShow
instance decodeGetDefaultPatchBaselineRequest :: Decode GetDefaultPatchBaselineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDefaultPatchBaselineRequest :: Encode GetDefaultPatchBaselineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDefaultPatchBaselineResult = GetDefaultPatchBaselineResult 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  }
derive instance newtypeGetDefaultPatchBaselineResult :: Newtype GetDefaultPatchBaselineResult _
derive instance repGenericGetDefaultPatchBaselineResult :: Generic GetDefaultPatchBaselineResult _
instance showGetDefaultPatchBaselineResult :: Show GetDefaultPatchBaselineResult where
  show = genericShow
instance decodeGetDefaultPatchBaselineResult :: Decode GetDefaultPatchBaselineResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDefaultPatchBaselineResult :: Encode GetDefaultPatchBaselineResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDeployablePatchSnapshotForInstanceRequest = GetDeployablePatchSnapshotForInstanceRequest 
  { "InstanceId" :: (InstanceId)
  , "SnapshotId" :: (SnapshotId)
  }
derive instance newtypeGetDeployablePatchSnapshotForInstanceRequest :: Newtype GetDeployablePatchSnapshotForInstanceRequest _
derive instance repGenericGetDeployablePatchSnapshotForInstanceRequest :: Generic GetDeployablePatchSnapshotForInstanceRequest _
instance showGetDeployablePatchSnapshotForInstanceRequest :: Show GetDeployablePatchSnapshotForInstanceRequest where
  show = genericShow
instance decodeGetDeployablePatchSnapshotForInstanceRequest :: Decode GetDeployablePatchSnapshotForInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeployablePatchSnapshotForInstanceRequest :: Encode GetDeployablePatchSnapshotForInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDeployablePatchSnapshotForInstanceResult = GetDeployablePatchSnapshotForInstanceResult 
  { "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "SnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  , "SnapshotDownloadUrl" :: NullOrUndefined.NullOrUndefined (SnapshotDownloadUrl)
  , "Product" :: NullOrUndefined.NullOrUndefined (Product)
  }
derive instance newtypeGetDeployablePatchSnapshotForInstanceResult :: Newtype GetDeployablePatchSnapshotForInstanceResult _
derive instance repGenericGetDeployablePatchSnapshotForInstanceResult :: Generic GetDeployablePatchSnapshotForInstanceResult _
instance showGetDeployablePatchSnapshotForInstanceResult :: Show GetDeployablePatchSnapshotForInstanceResult where
  show = genericShow
instance decodeGetDeployablePatchSnapshotForInstanceResult :: Decode GetDeployablePatchSnapshotForInstanceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDeployablePatchSnapshotForInstanceResult :: Encode GetDeployablePatchSnapshotForInstanceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDocumentRequest = GetDocumentRequest 
  { "Name" :: (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "DocumentFormat" :: NullOrUndefined.NullOrUndefined (DocumentFormat)
  }
derive instance newtypeGetDocumentRequest :: Newtype GetDocumentRequest _
derive instance repGenericGetDocumentRequest :: Generic GetDocumentRequest _
instance showGetDocumentRequest :: Show GetDocumentRequest where
  show = genericShow
instance decodeGetDocumentRequest :: Decode GetDocumentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentRequest :: Encode GetDocumentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDocumentResult = GetDocumentResult 
  { "Name" :: NullOrUndefined.NullOrUndefined (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "Content" :: NullOrUndefined.NullOrUndefined (DocumentContent)
  , "DocumentType" :: NullOrUndefined.NullOrUndefined (DocumentType)
  , "DocumentFormat" :: NullOrUndefined.NullOrUndefined (DocumentFormat)
  }
derive instance newtypeGetDocumentResult :: Newtype GetDocumentResult _
derive instance repGenericGetDocumentResult :: Generic GetDocumentResult _
instance showGetDocumentResult :: Show GetDocumentResult where
  show = genericShow
instance decodeGetDocumentResult :: Decode GetDocumentResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentResult :: Encode GetDocumentResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInventoryRequest = GetInventoryRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (InventoryFilterList)
  , "Aggregators" :: NullOrUndefined.NullOrUndefined (InventoryAggregatorList)
  , "ResultAttributes" :: NullOrUndefined.NullOrUndefined (ResultAttributeList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeGetInventoryRequest :: Newtype GetInventoryRequest _
derive instance repGenericGetInventoryRequest :: Generic GetInventoryRequest _
instance showGetInventoryRequest :: Show GetInventoryRequest where
  show = genericShow
instance decodeGetInventoryRequest :: Decode GetInventoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInventoryRequest :: Encode GetInventoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInventoryResult = GetInventoryResult 
  { "Entities" :: NullOrUndefined.NullOrUndefined (InventoryResultEntityList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetInventoryResult :: Newtype GetInventoryResult _
derive instance repGenericGetInventoryResult :: Generic GetInventoryResult _
instance showGetInventoryResult :: Show GetInventoryResult where
  show = genericShow
instance decodeGetInventoryResult :: Decode GetInventoryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInventoryResult :: Encode GetInventoryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInventorySchemaMaxResults = GetInventorySchemaMaxResults Int
derive instance newtypeGetInventorySchemaMaxResults :: Newtype GetInventorySchemaMaxResults _
derive instance repGenericGetInventorySchemaMaxResults :: Generic GetInventorySchemaMaxResults _
instance showGetInventorySchemaMaxResults :: Show GetInventorySchemaMaxResults where
  show = genericShow
instance decodeGetInventorySchemaMaxResults :: Decode GetInventorySchemaMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInventorySchemaMaxResults :: Encode GetInventorySchemaMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInventorySchemaRequest = GetInventorySchemaRequest 
  { "TypeName" :: NullOrUndefined.NullOrUndefined (InventoryItemTypeNameFilter)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (GetInventorySchemaMaxResults)
  , "Aggregator" :: NullOrUndefined.NullOrUndefined (AggregatorSchemaOnly)
  , "SubType" :: NullOrUndefined.NullOrUndefined (IsSubTypeSchema)
  }
derive instance newtypeGetInventorySchemaRequest :: Newtype GetInventorySchemaRequest _
derive instance repGenericGetInventorySchemaRequest :: Generic GetInventorySchemaRequest _
instance showGetInventorySchemaRequest :: Show GetInventorySchemaRequest where
  show = genericShow
instance decodeGetInventorySchemaRequest :: Decode GetInventorySchemaRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInventorySchemaRequest :: Encode GetInventorySchemaRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetInventorySchemaResult = GetInventorySchemaResult 
  { "Schemas" :: NullOrUndefined.NullOrUndefined (InventoryItemSchemaResultList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetInventorySchemaResult :: Newtype GetInventorySchemaResult _
derive instance repGenericGetInventorySchemaResult :: Generic GetInventorySchemaResult _
instance showGetInventorySchemaResult :: Show GetInventorySchemaResult where
  show = genericShow
instance decodeGetInventorySchemaResult :: Decode GetInventorySchemaResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetInventorySchemaResult :: Encode GetInventorySchemaResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowExecutionRequest = GetMaintenanceWindowExecutionRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  }
derive instance newtypeGetMaintenanceWindowExecutionRequest :: Newtype GetMaintenanceWindowExecutionRequest _
derive instance repGenericGetMaintenanceWindowExecutionRequest :: Generic GetMaintenanceWindowExecutionRequest _
instance showGetMaintenanceWindowExecutionRequest :: Show GetMaintenanceWindowExecutionRequest where
  show = genericShow
instance decodeGetMaintenanceWindowExecutionRequest :: Decode GetMaintenanceWindowExecutionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowExecutionRequest :: Encode GetMaintenanceWindowExecutionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowExecutionResult = GetMaintenanceWindowExecutionResult 
  { "WindowExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskIds" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskIdList)
  , "Status" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  }
derive instance newtypeGetMaintenanceWindowExecutionResult :: Newtype GetMaintenanceWindowExecutionResult _
derive instance repGenericGetMaintenanceWindowExecutionResult :: Generic GetMaintenanceWindowExecutionResult _
instance showGetMaintenanceWindowExecutionResult :: Show GetMaintenanceWindowExecutionResult where
  show = genericShow
instance decodeGetMaintenanceWindowExecutionResult :: Decode GetMaintenanceWindowExecutionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowExecutionResult :: Encode GetMaintenanceWindowExecutionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowExecutionTaskInvocationRequest = GetMaintenanceWindowExecutionTaskInvocationRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "TaskId" :: (MaintenanceWindowExecutionTaskId)
  , "InvocationId" :: (MaintenanceWindowExecutionTaskInvocationId)
  }
derive instance newtypeGetMaintenanceWindowExecutionTaskInvocationRequest :: Newtype GetMaintenanceWindowExecutionTaskInvocationRequest _
derive instance repGenericGetMaintenanceWindowExecutionTaskInvocationRequest :: Generic GetMaintenanceWindowExecutionTaskInvocationRequest _
instance showGetMaintenanceWindowExecutionTaskInvocationRequest :: Show GetMaintenanceWindowExecutionTaskInvocationRequest where
  show = genericShow
instance decodeGetMaintenanceWindowExecutionTaskInvocationRequest :: Decode GetMaintenanceWindowExecutionTaskInvocationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowExecutionTaskInvocationRequest :: Encode GetMaintenanceWindowExecutionTaskInvocationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowExecutionTaskInvocationResult = GetMaintenanceWindowExecutionTaskInvocationResult 
  { "WindowExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskId)
  , "InvocationId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskInvocationId)
  , "ExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskExecutionId)
  , "TaskType" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskType)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskInvocationParameters)
  , "Status" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "OwnerInformation" :: NullOrUndefined.NullOrUndefined (OwnerInformation)
  , "WindowTargetId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskTargetId)
  }
derive instance newtypeGetMaintenanceWindowExecutionTaskInvocationResult :: Newtype GetMaintenanceWindowExecutionTaskInvocationResult _
derive instance repGenericGetMaintenanceWindowExecutionTaskInvocationResult :: Generic GetMaintenanceWindowExecutionTaskInvocationResult _
instance showGetMaintenanceWindowExecutionTaskInvocationResult :: Show GetMaintenanceWindowExecutionTaskInvocationResult where
  show = genericShow
instance decodeGetMaintenanceWindowExecutionTaskInvocationResult :: Decode GetMaintenanceWindowExecutionTaskInvocationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowExecutionTaskInvocationResult :: Encode GetMaintenanceWindowExecutionTaskInvocationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowExecutionTaskRequest = GetMaintenanceWindowExecutionTaskRequest 
  { "WindowExecutionId" :: (MaintenanceWindowExecutionId)
  , "TaskId" :: (MaintenanceWindowExecutionTaskId)
  }
derive instance newtypeGetMaintenanceWindowExecutionTaskRequest :: Newtype GetMaintenanceWindowExecutionTaskRequest _
derive instance repGenericGetMaintenanceWindowExecutionTaskRequest :: Generic GetMaintenanceWindowExecutionTaskRequest _
instance showGetMaintenanceWindowExecutionTaskRequest :: Show GetMaintenanceWindowExecutionTaskRequest where
  show = genericShow
instance decodeGetMaintenanceWindowExecutionTaskRequest :: Decode GetMaintenanceWindowExecutionTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowExecutionTaskRequest :: Encode GetMaintenanceWindowExecutionTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowExecutionTaskResult = GetMaintenanceWindowExecutionTaskResult 
  { "WindowExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskId)
  , "TaskArn" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskArn)
  , "ServiceRole" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "Type" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskType)
  , "TaskParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskParametersList)
  , "Priority" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  , "Status" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  }
derive instance newtypeGetMaintenanceWindowExecutionTaskResult :: Newtype GetMaintenanceWindowExecutionTaskResult _
derive instance repGenericGetMaintenanceWindowExecutionTaskResult :: Generic GetMaintenanceWindowExecutionTaskResult _
instance showGetMaintenanceWindowExecutionTaskResult :: Show GetMaintenanceWindowExecutionTaskResult where
  show = genericShow
instance decodeGetMaintenanceWindowExecutionTaskResult :: Decode GetMaintenanceWindowExecutionTaskResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowExecutionTaskResult :: Encode GetMaintenanceWindowExecutionTaskResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowRequest = GetMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  }
derive instance newtypeGetMaintenanceWindowRequest :: Newtype GetMaintenanceWindowRequest _
derive instance repGenericGetMaintenanceWindowRequest :: Generic GetMaintenanceWindowRequest _
instance showGetMaintenanceWindowRequest :: Show GetMaintenanceWindowRequest where
  show = genericShow
instance decodeGetMaintenanceWindowRequest :: Decode GetMaintenanceWindowRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowRequest :: Encode GetMaintenanceWindowRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowResult = GetMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowSchedule)
  , "Duration" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDurationHours)
  , "Cutoff" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowCutoff)
  , "AllowUnassociatedTargets" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowEnabled)
  , "CreatedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "ModifiedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  }
derive instance newtypeGetMaintenanceWindowResult :: Newtype GetMaintenanceWindowResult _
derive instance repGenericGetMaintenanceWindowResult :: Generic GetMaintenanceWindowResult _
instance showGetMaintenanceWindowResult :: Show GetMaintenanceWindowResult where
  show = genericShow
instance decodeGetMaintenanceWindowResult :: Decode GetMaintenanceWindowResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowResult :: Encode GetMaintenanceWindowResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowTaskRequest = GetMaintenanceWindowTaskRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTaskId" :: (MaintenanceWindowTaskId)
  }
derive instance newtypeGetMaintenanceWindowTaskRequest :: Newtype GetMaintenanceWindowTaskRequest _
derive instance repGenericGetMaintenanceWindowTaskRequest :: Generic GetMaintenanceWindowTaskRequest _
instance showGetMaintenanceWindowTaskRequest :: Show GetMaintenanceWindowTaskRequest where
  show = genericShow
instance decodeGetMaintenanceWindowTaskRequest :: Decode GetMaintenanceWindowTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowTaskRequest :: Encode GetMaintenanceWindowTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMaintenanceWindowTaskResult = GetMaintenanceWindowTaskResult 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "WindowTaskId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskId)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "TaskArn" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskArn)
  , "ServiceRoleArn" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "TaskType" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskType)
  , "TaskParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskParameters)
  , "TaskInvocationParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskInvocationParameters)
  , "Priority" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  , "LoggingInfo" :: NullOrUndefined.NullOrUndefined (LoggingInfo)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  }
derive instance newtypeGetMaintenanceWindowTaskResult :: Newtype GetMaintenanceWindowTaskResult _
derive instance repGenericGetMaintenanceWindowTaskResult :: Generic GetMaintenanceWindowTaskResult _
instance showGetMaintenanceWindowTaskResult :: Show GetMaintenanceWindowTaskResult where
  show = genericShow
instance decodeGetMaintenanceWindowTaskResult :: Decode GetMaintenanceWindowTaskResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMaintenanceWindowTaskResult :: Encode GetMaintenanceWindowTaskResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParameterHistoryRequest = GetParameterHistoryRequest 
  { "Name" :: (PSParameterName)
  , "WithDecryption" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetParameterHistoryRequest :: Newtype GetParameterHistoryRequest _
derive instance repGenericGetParameterHistoryRequest :: Generic GetParameterHistoryRequest _
instance showGetParameterHistoryRequest :: Show GetParameterHistoryRequest where
  show = genericShow
instance decodeGetParameterHistoryRequest :: Decode GetParameterHistoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParameterHistoryRequest :: Encode GetParameterHistoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParameterHistoryResult = GetParameterHistoryResult 
  { "Parameters" :: NullOrUndefined.NullOrUndefined (ParameterHistoryList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetParameterHistoryResult :: Newtype GetParameterHistoryResult _
derive instance repGenericGetParameterHistoryResult :: Generic GetParameterHistoryResult _
instance showGetParameterHistoryResult :: Show GetParameterHistoryResult where
  show = genericShow
instance decodeGetParameterHistoryResult :: Decode GetParameterHistoryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParameterHistoryResult :: Encode GetParameterHistoryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParameterRequest = GetParameterRequest 
  { "Name" :: (PSParameterName)
  , "WithDecryption" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeGetParameterRequest :: Newtype GetParameterRequest _
derive instance repGenericGetParameterRequest :: Generic GetParameterRequest _
instance showGetParameterRequest :: Show GetParameterRequest where
  show = genericShow
instance decodeGetParameterRequest :: Decode GetParameterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParameterRequest :: Encode GetParameterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParameterResult = GetParameterResult 
  { "Parameter" :: NullOrUndefined.NullOrUndefined (Parameter)
  }
derive instance newtypeGetParameterResult :: Newtype GetParameterResult _
derive instance repGenericGetParameterResult :: Generic GetParameterResult _
instance showGetParameterResult :: Show GetParameterResult where
  show = genericShow
instance decodeGetParameterResult :: Decode GetParameterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParameterResult :: Encode GetParameterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParametersByPathMaxResults = GetParametersByPathMaxResults Int
derive instance newtypeGetParametersByPathMaxResults :: Newtype GetParametersByPathMaxResults _
derive instance repGenericGetParametersByPathMaxResults :: Generic GetParametersByPathMaxResults _
instance showGetParametersByPathMaxResults :: Show GetParametersByPathMaxResults where
  show = genericShow
instance decodeGetParametersByPathMaxResults :: Decode GetParametersByPathMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParametersByPathMaxResults :: Encode GetParametersByPathMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParametersByPathRequest = GetParametersByPathRequest 
  { "Path" :: (PSParameterName)
  , "Recursive" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ParameterFilters" :: NullOrUndefined.NullOrUndefined (ParameterStringFilterList)
  , "WithDecryption" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (GetParametersByPathMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetParametersByPathRequest :: Newtype GetParametersByPathRequest _
derive instance repGenericGetParametersByPathRequest :: Generic GetParametersByPathRequest _
instance showGetParametersByPathRequest :: Show GetParametersByPathRequest where
  show = genericShow
instance decodeGetParametersByPathRequest :: Decode GetParametersByPathRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParametersByPathRequest :: Encode GetParametersByPathRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParametersByPathResult = GetParametersByPathResult 
  { "Parameters" :: NullOrUndefined.NullOrUndefined (ParameterList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetParametersByPathResult :: Newtype GetParametersByPathResult _
derive instance repGenericGetParametersByPathResult :: Generic GetParametersByPathResult _
instance showGetParametersByPathResult :: Show GetParametersByPathResult where
  show = genericShow
instance decodeGetParametersByPathResult :: Decode GetParametersByPathResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParametersByPathResult :: Encode GetParametersByPathResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParametersRequest = GetParametersRequest 
  { "Names" :: (ParameterNameList)
  , "WithDecryption" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeGetParametersRequest :: Newtype GetParametersRequest _
derive instance repGenericGetParametersRequest :: Generic GetParametersRequest _
instance showGetParametersRequest :: Show GetParametersRequest where
  show = genericShow
instance decodeGetParametersRequest :: Decode GetParametersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParametersRequest :: Encode GetParametersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetParametersResult = GetParametersResult 
  { "Parameters" :: NullOrUndefined.NullOrUndefined (ParameterList)
  , "InvalidParameters" :: NullOrUndefined.NullOrUndefined (ParameterNameList)
  }
derive instance newtypeGetParametersResult :: Newtype GetParametersResult _
derive instance repGenericGetParametersResult :: Generic GetParametersResult _
instance showGetParametersResult :: Show GetParametersResult where
  show = genericShow
instance decodeGetParametersResult :: Decode GetParametersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetParametersResult :: Encode GetParametersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPatchBaselineForPatchGroupRequest = GetPatchBaselineForPatchGroupRequest 
  { "PatchGroup" :: (PatchGroup)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  }
derive instance newtypeGetPatchBaselineForPatchGroupRequest :: Newtype GetPatchBaselineForPatchGroupRequest _
derive instance repGenericGetPatchBaselineForPatchGroupRequest :: Generic GetPatchBaselineForPatchGroupRequest _
instance showGetPatchBaselineForPatchGroupRequest :: Show GetPatchBaselineForPatchGroupRequest where
  show = genericShow
instance decodeGetPatchBaselineForPatchGroupRequest :: Decode GetPatchBaselineForPatchGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPatchBaselineForPatchGroupRequest :: Encode GetPatchBaselineForPatchGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPatchBaselineForPatchGroupResult = GetPatchBaselineForPatchGroupResult 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  , "PatchGroup" :: NullOrUndefined.NullOrUndefined (PatchGroup)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  }
derive instance newtypeGetPatchBaselineForPatchGroupResult :: Newtype GetPatchBaselineForPatchGroupResult _
derive instance repGenericGetPatchBaselineForPatchGroupResult :: Generic GetPatchBaselineForPatchGroupResult _
instance showGetPatchBaselineForPatchGroupResult :: Show GetPatchBaselineForPatchGroupResult where
  show = genericShow
instance decodeGetPatchBaselineForPatchGroupResult :: Decode GetPatchBaselineForPatchGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPatchBaselineForPatchGroupResult :: Encode GetPatchBaselineForPatchGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPatchBaselineRequest = GetPatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  }
derive instance newtypeGetPatchBaselineRequest :: Newtype GetPatchBaselineRequest _
derive instance repGenericGetPatchBaselineRequest :: Generic GetPatchBaselineRequest _
instance showGetPatchBaselineRequest :: Show GetPatchBaselineRequest where
  show = genericShow
instance decodeGetPatchBaselineRequest :: Decode GetPatchBaselineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPatchBaselineRequest :: Encode GetPatchBaselineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPatchBaselineResult = GetPatchBaselineResult 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  , "Name" :: NullOrUndefined.NullOrUndefined (BaselineName)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  , "GlobalFilters" :: NullOrUndefined.NullOrUndefined (PatchFilterGroup)
  , "ApprovalRules" :: NullOrUndefined.NullOrUndefined (PatchRuleGroup)
  , "ApprovedPatches" :: NullOrUndefined.NullOrUndefined (PatchIdList)
  , "ApprovedPatchesComplianceLevel" :: NullOrUndefined.NullOrUndefined (PatchComplianceLevel)
  , "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RejectedPatches" :: NullOrUndefined.NullOrUndefined (PatchIdList)
  , "PatchGroups" :: NullOrUndefined.NullOrUndefined (PatchGroupList)
  , "CreatedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "ModifiedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Description" :: NullOrUndefined.NullOrUndefined (BaselineDescription)
  , "Sources" :: NullOrUndefined.NullOrUndefined (PatchSourceList)
  }
derive instance newtypeGetPatchBaselineResult :: Newtype GetPatchBaselineResult _
derive instance repGenericGetPatchBaselineResult :: Generic GetPatchBaselineResult _
instance showGetPatchBaselineResult :: Show GetPatchBaselineResult where
  show = genericShow
instance decodeGetPatchBaselineResult :: Decode GetPatchBaselineResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPatchBaselineResult :: Encode GetPatchBaselineResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A hierarchy can have a maximum of 15 levels. For more information, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html">Working with Systems Manager Parameters</a>. </p>
newtype HierarchyLevelLimitExceededException = HierarchyLevelLimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHierarchyLevelLimitExceededException :: Newtype HierarchyLevelLimitExceededException _
derive instance repGenericHierarchyLevelLimitExceededException :: Generic HierarchyLevelLimitExceededException _
instance showHierarchyLevelLimitExceededException :: Show HierarchyLevelLimitExceededException where
  show = genericShow
instance decodeHierarchyLevelLimitExceededException :: Decode HierarchyLevelLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHierarchyLevelLimitExceededException :: Encode HierarchyLevelLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Parameter Store does not support changing a parameter type in a hierarchy. For example, you can't change a parameter from a String type to a SecureString type. You must create a new, unique parameter.</p>
newtype HierarchyTypeMismatchException = HierarchyTypeMismatchException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHierarchyTypeMismatchException :: Newtype HierarchyTypeMismatchException _
derive instance repGenericHierarchyTypeMismatchException :: Generic HierarchyTypeMismatchException _
instance showHierarchyTypeMismatchException :: Show HierarchyTypeMismatchException where
  show = genericShow
instance decodeHierarchyTypeMismatchException :: Decode HierarchyTypeMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHierarchyTypeMismatchException :: Encode HierarchyTypeMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPAddress = IPAddress String
derive instance newtypeIPAddress :: Newtype IPAddress _
derive instance repGenericIPAddress :: Generic IPAddress _
instance showIPAddress :: Show IPAddress where
  show = genericShow
instance decodeIPAddress :: Decode IPAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPAddress :: Encode IPAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IamRole = IamRole String
derive instance newtypeIamRole :: Newtype IamRole _
derive instance repGenericIamRole :: Generic IamRole _
instance showIamRole :: Show IamRole where
  show = genericShow
instance decodeIamRole :: Decode IamRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIamRole :: Encode IamRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdempotencyToken = IdempotencyToken String
derive instance newtypeIdempotencyToken :: Newtype IdempotencyToken _
derive instance repGenericIdempotencyToken :: Generic IdempotencyToken _
instance showIdempotencyToken :: Show IdempotencyToken where
  show = genericShow
instance decodeIdempotencyToken :: Decode IdempotencyToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdempotencyToken :: Encode IdempotencyToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Error returned when an idempotent operation is retried and the parameters don't match the original call to the API with the same idempotency token. </p>
newtype IdempotentParameterMismatch = IdempotentParameterMismatch 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeIdempotentParameterMismatch :: Newtype IdempotentParameterMismatch _
derive instance repGenericIdempotentParameterMismatch :: Generic IdempotentParameterMismatch _
instance showIdempotentParameterMismatch :: Show IdempotentParameterMismatch where
  show = genericShow
instance decodeIdempotentParameterMismatch :: Decode IdempotentParameterMismatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdempotentParameterMismatch :: Encode IdempotentParameterMismatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Status information about the aggregated associations.</p>
newtype InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview 
  { "DetailedStatus" :: NullOrUndefined.NullOrUndefined (StatusName)
  , "InstanceAssociationStatusAggregatedCount" :: NullOrUndefined.NullOrUndefined (InstanceAssociationStatusAggregatedCount)
  }
derive instance newtypeInstanceAggregatedAssociationOverview :: Newtype InstanceAggregatedAssociationOverview _
derive instance repGenericInstanceAggregatedAssociationOverview :: Generic InstanceAggregatedAssociationOverview _
instance showInstanceAggregatedAssociationOverview :: Show InstanceAggregatedAssociationOverview where
  show = genericShow
instance decodeInstanceAggregatedAssociationOverview :: Decode InstanceAggregatedAssociationOverview where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAggregatedAssociationOverview :: Encode InstanceAggregatedAssociationOverview where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more association documents on the instance. </p>
newtype InstanceAssociation = InstanceAssociation 
  { "AssociationId" :: NullOrUndefined.NullOrUndefined (AssociationId)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "Content" :: NullOrUndefined.NullOrUndefined (DocumentContent)
  , "AssociationVersion" :: NullOrUndefined.NullOrUndefined (AssociationVersion)
  }
derive instance newtypeInstanceAssociation :: Newtype InstanceAssociation _
derive instance repGenericInstanceAssociation :: Generic InstanceAssociation _
instance showInstanceAssociation :: Show InstanceAssociation where
  show = genericShow
instance decodeInstanceAssociation :: Decode InstanceAssociation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAssociation :: Encode InstanceAssociation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceAssociationExecutionSummary = InstanceAssociationExecutionSummary String
derive instance newtypeInstanceAssociationExecutionSummary :: Newtype InstanceAssociationExecutionSummary _
derive instance repGenericInstanceAssociationExecutionSummary :: Generic InstanceAssociationExecutionSummary _
instance showInstanceAssociationExecutionSummary :: Show InstanceAssociationExecutionSummary where
  show = genericShow
instance decodeInstanceAssociationExecutionSummary :: Decode InstanceAssociationExecutionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAssociationExecutionSummary :: Encode InstanceAssociationExecutionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceAssociationList = InstanceAssociationList (Array InstanceAssociation)
derive instance newtypeInstanceAssociationList :: Newtype InstanceAssociationList _
derive instance repGenericInstanceAssociationList :: Generic InstanceAssociationList _
instance showInstanceAssociationList :: Show InstanceAssociationList where
  show = genericShow
instance decodeInstanceAssociationList :: Decode InstanceAssociationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAssociationList :: Encode InstanceAssociationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Amazon S3 bucket where you want to store the results of this request.</p>
newtype InstanceAssociationOutputLocation = InstanceAssociationOutputLocation 
  { "S3Location" :: NullOrUndefined.NullOrUndefined (S3OutputLocation)
  }
derive instance newtypeInstanceAssociationOutputLocation :: Newtype InstanceAssociationOutputLocation _
derive instance repGenericInstanceAssociationOutputLocation :: Generic InstanceAssociationOutputLocation _
instance showInstanceAssociationOutputLocation :: Show InstanceAssociationOutputLocation where
  show = genericShow
instance decodeInstanceAssociationOutputLocation :: Decode InstanceAssociationOutputLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAssociationOutputLocation :: Encode InstanceAssociationOutputLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The URL of Amazon S3 bucket where you want to store the results of this request.</p>
newtype InstanceAssociationOutputUrl = InstanceAssociationOutputUrl 
  { "S3OutputUrl" :: NullOrUndefined.NullOrUndefined (S3OutputUrl)
  }
derive instance newtypeInstanceAssociationOutputUrl :: Newtype InstanceAssociationOutputUrl _
derive instance repGenericInstanceAssociationOutputUrl :: Generic InstanceAssociationOutputUrl _
instance showInstanceAssociationOutputUrl :: Show InstanceAssociationOutputUrl where
  show = genericShow
instance decodeInstanceAssociationOutputUrl :: Decode InstanceAssociationOutputUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAssociationOutputUrl :: Encode InstanceAssociationOutputUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceAssociationStatusAggregatedCount = InstanceAssociationStatusAggregatedCount (StrMap.StrMap InstanceCount)
derive instance newtypeInstanceAssociationStatusAggregatedCount :: Newtype InstanceAssociationStatusAggregatedCount _
derive instance repGenericInstanceAssociationStatusAggregatedCount :: Generic InstanceAssociationStatusAggregatedCount _
instance showInstanceAssociationStatusAggregatedCount :: Show InstanceAssociationStatusAggregatedCount where
  show = genericShow
instance decodeInstanceAssociationStatusAggregatedCount :: Decode InstanceAssociationStatusAggregatedCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAssociationStatusAggregatedCount :: Encode InstanceAssociationStatusAggregatedCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Status information about the instance association.</p>
newtype InstanceAssociationStatusInfo = InstanceAssociationStatusInfo 
  { "AssociationId" :: NullOrUndefined.NullOrUndefined (AssociationId)
  , "Name" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "AssociationVersion" :: NullOrUndefined.NullOrUndefined (AssociationVersion)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "ExecutionDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined.NullOrUndefined (StatusName)
  , "DetailedStatus" :: NullOrUndefined.NullOrUndefined (StatusName)
  , "ExecutionSummary" :: NullOrUndefined.NullOrUndefined (InstanceAssociationExecutionSummary)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (AgentErrorCode)
  , "OutputUrl" :: NullOrUndefined.NullOrUndefined (InstanceAssociationOutputUrl)
  , "AssociationName" :: NullOrUndefined.NullOrUndefined (AssociationName)
  }
derive instance newtypeInstanceAssociationStatusInfo :: Newtype InstanceAssociationStatusInfo _
derive instance repGenericInstanceAssociationStatusInfo :: Generic InstanceAssociationStatusInfo _
instance showInstanceAssociationStatusInfo :: Show InstanceAssociationStatusInfo where
  show = genericShow
instance decodeInstanceAssociationStatusInfo :: Decode InstanceAssociationStatusInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAssociationStatusInfo :: Encode InstanceAssociationStatusInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceAssociationStatusInfos = InstanceAssociationStatusInfos (Array InstanceAssociationStatusInfo)
derive instance newtypeInstanceAssociationStatusInfos :: Newtype InstanceAssociationStatusInfos _
derive instance repGenericInstanceAssociationStatusInfos :: Generic InstanceAssociationStatusInfos _
instance showInstanceAssociationStatusInfos :: Show InstanceAssociationStatusInfos where
  show = genericShow
instance decodeInstanceAssociationStatusInfos :: Decode InstanceAssociationStatusInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceAssociationStatusInfos :: Encode InstanceAssociationStatusInfos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceCount = InstanceCount Int
derive instance newtypeInstanceCount :: Newtype InstanceCount _
derive instance repGenericInstanceCount :: Generic InstanceCount _
instance showInstanceCount :: Show InstanceCount where
  show = genericShow
instance decodeInstanceCount :: Decode InstanceCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceCount :: Encode InstanceCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceId = InstanceId String
derive instance newtypeInstanceId :: Newtype InstanceId _
derive instance repGenericInstanceId :: Generic InstanceId _
instance showInstanceId :: Show InstanceId where
  show = genericShow
instance decodeInstanceId :: Decode InstanceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceId :: Encode InstanceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceIdList = InstanceIdList (Array InstanceId)
derive instance newtypeInstanceIdList :: Newtype InstanceIdList _
derive instance repGenericInstanceIdList :: Generic InstanceIdList _
instance showInstanceIdList :: Show InstanceIdList where
  show = genericShow
instance decodeInstanceIdList :: Decode InstanceIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceIdList :: Encode InstanceIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a filter for a specific list of instances. </p>
newtype InstanceInformation = InstanceInformation 
  { "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "PingStatus" :: NullOrUndefined.NullOrUndefined (PingStatus)
  , "LastPingDateTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "AgentVersion" :: NullOrUndefined.NullOrUndefined (Version)
  , "IsLatestVersion" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PlatformType" :: NullOrUndefined.NullOrUndefined (PlatformType)
  , "PlatformName" :: NullOrUndefined.NullOrUndefined (String)
  , "PlatformVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "ActivationId" :: NullOrUndefined.NullOrUndefined (ActivationId)
  , "IamRole" :: NullOrUndefined.NullOrUndefined (IamRole)
  , "RegistrationDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "IPAddress" :: NullOrUndefined.NullOrUndefined (IPAddress)
  , "ComputerName" :: NullOrUndefined.NullOrUndefined (ComputerName)
  , "AssociationStatus" :: NullOrUndefined.NullOrUndefined (StatusName)
  , "LastAssociationExecutionDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "LastSuccessfulAssociationExecutionDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "AssociationOverview" :: NullOrUndefined.NullOrUndefined (InstanceAggregatedAssociationOverview)
  }
derive instance newtypeInstanceInformation :: Newtype InstanceInformation _
derive instance repGenericInstanceInformation :: Generic InstanceInformation _
instance showInstanceInformation :: Show InstanceInformation where
  show = genericShow
instance decodeInstanceInformation :: Decode InstanceInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformation :: Encode InstanceInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a filter for a specific list of instances. </p>
newtype InstanceInformationFilter = InstanceInformationFilter 
  { "Key'" :: (InstanceInformationFilterKey)
  , "ValueSet'" :: (InstanceInformationFilterValueSet)
  }
derive instance newtypeInstanceInformationFilter :: Newtype InstanceInformationFilter _
derive instance repGenericInstanceInformationFilter :: Generic InstanceInformationFilter _
instance showInstanceInformationFilter :: Show InstanceInformationFilter where
  show = genericShow
instance decodeInstanceInformationFilter :: Decode InstanceInformationFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformationFilter :: Encode InstanceInformationFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceInformationFilterKey = InstanceInformationFilterKey String
derive instance newtypeInstanceInformationFilterKey :: Newtype InstanceInformationFilterKey _
derive instance repGenericInstanceInformationFilterKey :: Generic InstanceInformationFilterKey _
instance showInstanceInformationFilterKey :: Show InstanceInformationFilterKey where
  show = genericShow
instance decodeInstanceInformationFilterKey :: Decode InstanceInformationFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformationFilterKey :: Encode InstanceInformationFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceInformationFilterList = InstanceInformationFilterList (Array InstanceInformationFilter)
derive instance newtypeInstanceInformationFilterList :: Newtype InstanceInformationFilterList _
derive instance repGenericInstanceInformationFilterList :: Generic InstanceInformationFilterList _
instance showInstanceInformationFilterList :: Show InstanceInformationFilterList where
  show = genericShow
instance decodeInstanceInformationFilterList :: Decode InstanceInformationFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformationFilterList :: Encode InstanceInformationFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceInformationFilterValue = InstanceInformationFilterValue String
derive instance newtypeInstanceInformationFilterValue :: Newtype InstanceInformationFilterValue _
derive instance repGenericInstanceInformationFilterValue :: Generic InstanceInformationFilterValue _
instance showInstanceInformationFilterValue :: Show InstanceInformationFilterValue where
  show = genericShow
instance decodeInstanceInformationFilterValue :: Decode InstanceInformationFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformationFilterValue :: Encode InstanceInformationFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceInformationFilterValueSet = InstanceInformationFilterValueSet (Array InstanceInformationFilterValue)
derive instance newtypeInstanceInformationFilterValueSet :: Newtype InstanceInformationFilterValueSet _
derive instance repGenericInstanceInformationFilterValueSet :: Generic InstanceInformationFilterValueSet _
instance showInstanceInformationFilterValueSet :: Show InstanceInformationFilterValueSet where
  show = genericShow
instance decodeInstanceInformationFilterValueSet :: Decode InstanceInformationFilterValueSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformationFilterValueSet :: Encode InstanceInformationFilterValueSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceInformationList = InstanceInformationList (Array InstanceInformation)
derive instance newtypeInstanceInformationList :: Newtype InstanceInformationList _
derive instance repGenericInstanceInformationList :: Generic InstanceInformationList _
instance showInstanceInformationList :: Show InstanceInformationList where
  show = genericShow
instance decodeInstanceInformationList :: Decode InstanceInformationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformationList :: Encode InstanceInformationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The filters to describe or get information about your managed instances.</p>
newtype InstanceInformationStringFilter = InstanceInformationStringFilter 
  { "Key" :: (InstanceInformationStringFilterKey)
  , "Values" :: (InstanceInformationFilterValueSet)
  }
derive instance newtypeInstanceInformationStringFilter :: Newtype InstanceInformationStringFilter _
derive instance repGenericInstanceInformationStringFilter :: Generic InstanceInformationStringFilter _
instance showInstanceInformationStringFilter :: Show InstanceInformationStringFilter where
  show = genericShow
instance decodeInstanceInformationStringFilter :: Decode InstanceInformationStringFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformationStringFilter :: Encode InstanceInformationStringFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceInformationStringFilterKey = InstanceInformationStringFilterKey String
derive instance newtypeInstanceInformationStringFilterKey :: Newtype InstanceInformationStringFilterKey _
derive instance repGenericInstanceInformationStringFilterKey :: Generic InstanceInformationStringFilterKey _
instance showInstanceInformationStringFilterKey :: Show InstanceInformationStringFilterKey where
  show = genericShow
instance decodeInstanceInformationStringFilterKey :: Decode InstanceInformationStringFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformationStringFilterKey :: Encode InstanceInformationStringFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceInformationStringFilterList = InstanceInformationStringFilterList (Array InstanceInformationStringFilter)
derive instance newtypeInstanceInformationStringFilterList :: Newtype InstanceInformationStringFilterList _
derive instance repGenericInstanceInformationStringFilterList :: Generic InstanceInformationStringFilterList _
instance showInstanceInformationStringFilterList :: Show InstanceInformationStringFilterList where
  show = genericShow
instance decodeInstanceInformationStringFilterList :: Decode InstanceInformationStringFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceInformationStringFilterList :: Encode InstanceInformationStringFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines the high-level patch compliance state for a managed instance, providing information about the number of installed, missing, not applicable, and failed patches along with metadata about the operation when this information was gathered for the instance.</p>
newtype InstancePatchState = InstancePatchState 
  { "InstanceId" :: (InstanceId)
  , "PatchGroup" :: (PatchGroup)
  , "BaselineId" :: (BaselineId)
  , "SnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  , "OwnerInformation" :: NullOrUndefined.NullOrUndefined (OwnerInformation)
  , "InstalledCount" :: NullOrUndefined.NullOrUndefined (PatchInstalledCount)
  , "InstalledOtherCount" :: NullOrUndefined.NullOrUndefined (PatchInstalledOtherCount)
  , "MissingCount" :: NullOrUndefined.NullOrUndefined (PatchMissingCount)
  , "FailedCount" :: NullOrUndefined.NullOrUndefined (PatchFailedCount)
  , "NotApplicableCount" :: NullOrUndefined.NullOrUndefined (PatchNotApplicableCount)
  , "OperationStartTime" :: (DateTime)
  , "OperationEndTime" :: (DateTime)
  , "Operation" :: (PatchOperationType)
  }
derive instance newtypeInstancePatchState :: Newtype InstancePatchState _
derive instance repGenericInstancePatchState :: Generic InstancePatchState _
instance showInstancePatchState :: Show InstancePatchState where
  show = genericShow
instance decodeInstancePatchState :: Decode InstancePatchState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePatchState :: Encode InstancePatchState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines a filter used in DescribeInstancePatchStatesForPatchGroup used to scope down the information returned by the API.</p>
newtype InstancePatchStateFilter = InstancePatchStateFilter 
  { "Key" :: (InstancePatchStateFilterKey)
  , "Values" :: (InstancePatchStateFilterValues)
  , "Type" :: (InstancePatchStateOperatorType)
  }
derive instance newtypeInstancePatchStateFilter :: Newtype InstancePatchStateFilter _
derive instance repGenericInstancePatchStateFilter :: Generic InstancePatchStateFilter _
instance showInstancePatchStateFilter :: Show InstancePatchStateFilter where
  show = genericShow
instance decodeInstancePatchStateFilter :: Decode InstancePatchStateFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePatchStateFilter :: Encode InstancePatchStateFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePatchStateFilterKey = InstancePatchStateFilterKey String
derive instance newtypeInstancePatchStateFilterKey :: Newtype InstancePatchStateFilterKey _
derive instance repGenericInstancePatchStateFilterKey :: Generic InstancePatchStateFilterKey _
instance showInstancePatchStateFilterKey :: Show InstancePatchStateFilterKey where
  show = genericShow
instance decodeInstancePatchStateFilterKey :: Decode InstancePatchStateFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePatchStateFilterKey :: Encode InstancePatchStateFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePatchStateFilterList = InstancePatchStateFilterList (Array InstancePatchStateFilter)
derive instance newtypeInstancePatchStateFilterList :: Newtype InstancePatchStateFilterList _
derive instance repGenericInstancePatchStateFilterList :: Generic InstancePatchStateFilterList _
instance showInstancePatchStateFilterList :: Show InstancePatchStateFilterList where
  show = genericShow
instance decodeInstancePatchStateFilterList :: Decode InstancePatchStateFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePatchStateFilterList :: Encode InstancePatchStateFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePatchStateFilterValue = InstancePatchStateFilterValue String
derive instance newtypeInstancePatchStateFilterValue :: Newtype InstancePatchStateFilterValue _
derive instance repGenericInstancePatchStateFilterValue :: Generic InstancePatchStateFilterValue _
instance showInstancePatchStateFilterValue :: Show InstancePatchStateFilterValue where
  show = genericShow
instance decodeInstancePatchStateFilterValue :: Decode InstancePatchStateFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePatchStateFilterValue :: Encode InstancePatchStateFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePatchStateFilterValues = InstancePatchStateFilterValues (Array InstancePatchStateFilterValue)
derive instance newtypeInstancePatchStateFilterValues :: Newtype InstancePatchStateFilterValues _
derive instance repGenericInstancePatchStateFilterValues :: Generic InstancePatchStateFilterValues _
instance showInstancePatchStateFilterValues :: Show InstancePatchStateFilterValues where
  show = genericShow
instance decodeInstancePatchStateFilterValues :: Decode InstancePatchStateFilterValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePatchStateFilterValues :: Encode InstancePatchStateFilterValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePatchStateList = InstancePatchStateList (Array InstancePatchState)
derive instance newtypeInstancePatchStateList :: Newtype InstancePatchStateList _
derive instance repGenericInstancePatchStateList :: Generic InstancePatchStateList _
instance showInstancePatchStateList :: Show InstancePatchStateList where
  show = genericShow
instance decodeInstancePatchStateList :: Decode InstancePatchStateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePatchStateList :: Encode InstancePatchStateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePatchStateOperatorType = InstancePatchStateOperatorType String
derive instance newtypeInstancePatchStateOperatorType :: Newtype InstancePatchStateOperatorType _
derive instance repGenericInstancePatchStateOperatorType :: Generic InstancePatchStateOperatorType _
instance showInstancePatchStateOperatorType :: Show InstancePatchStateOperatorType where
  show = genericShow
instance decodeInstancePatchStateOperatorType :: Decode InstancePatchStateOperatorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePatchStateOperatorType :: Encode InstancePatchStateOperatorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePatchStatesList = InstancePatchStatesList (Array InstancePatchState)
derive instance newtypeInstancePatchStatesList :: Newtype InstancePatchStatesList _
derive instance repGenericInstancePatchStatesList :: Generic InstancePatchStatesList _
instance showInstancePatchStatesList :: Show InstancePatchStatesList where
  show = genericShow
instance decodeInstancePatchStatesList :: Decode InstancePatchStatesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePatchStatesList :: Encode InstancePatchStatesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceTagName = InstanceTagName String
derive instance newtypeInstanceTagName :: Newtype InstanceTagName _
derive instance repGenericInstanceTagName :: Generic InstanceTagName _
instance showInstanceTagName :: Show InstanceTagName where
  show = genericShow
instance decodeInstanceTagName :: Decode InstanceTagName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceTagName :: Encode InstanceTagName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error occurred on the server side.</p>
newtype InternalServerError = InternalServerError 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _
derive instance repGenericInternalServerError :: Generic InternalServerError _
instance showInternalServerError :: Show InternalServerError where
  show = genericShow
instance decodeInternalServerError :: Decode InternalServerError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerError :: Encode InternalServerError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The activation is not valid. The activation might have been deleted, or the ActivationId and the ActivationCode do not match.</p>
newtype InvalidActivation = InvalidActivation 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidActivation :: Newtype InvalidActivation _
derive instance repGenericInvalidActivation :: Generic InvalidActivation _
instance showInvalidActivation :: Show InvalidActivation where
  show = genericShow
instance decodeInvalidActivation :: Decode InvalidActivation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidActivation :: Encode InvalidActivation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The activation ID is not valid. Verify the you entered the correct ActivationId or ActivationCode and try again.</p>
newtype InvalidActivationId = InvalidActivationId 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidActivationId :: Newtype InvalidActivationId _
derive instance repGenericInvalidActivationId :: Generic InvalidActivationId _
instance showInvalidActivationId :: Show InvalidActivationId where
  show = genericShow
instance decodeInvalidActivationId :: Decode InvalidActivationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidActivationId :: Encode InvalidActivationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request does not meet the regular expression requirement.</p>
newtype InvalidAllowedPatternException = InvalidAllowedPatternException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidAllowedPatternException :: Newtype InvalidAllowedPatternException _
derive instance repGenericInvalidAllowedPatternException :: Generic InvalidAllowedPatternException _
instance showInvalidAllowedPatternException :: Show InvalidAllowedPatternException where
  show = genericShow
instance decodeInvalidAllowedPatternException :: Decode InvalidAllowedPatternException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAllowedPatternException :: Encode InvalidAllowedPatternException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The version you specified is not valid. Use ListAssociationVersions to view all versions of an association according to the association ID. Or, use the <code>$LATEST</code> parameter to view the latest version of the association.</p>
newtype InvalidAssociationVersion = InvalidAssociationVersion 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidAssociationVersion :: Newtype InvalidAssociationVersion _
derive instance repGenericInvalidAssociationVersion :: Generic InvalidAssociationVersion _
instance showInvalidAssociationVersion :: Show InvalidAssociationVersion where
  show = genericShow
instance decodeInvalidAssociationVersion :: Decode InvalidAssociationVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAssociationVersion :: Encode InvalidAssociationVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The supplied parameters for invoking the specified Automation document are incorrect. For example, they may not match the set of parameters permitted for the specified Automation document.</p>
newtype InvalidAutomationExecutionParametersException = InvalidAutomationExecutionParametersException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidAutomationExecutionParametersException :: Newtype InvalidAutomationExecutionParametersException _
derive instance repGenericInvalidAutomationExecutionParametersException :: Generic InvalidAutomationExecutionParametersException _
instance showInvalidAutomationExecutionParametersException :: Show InvalidAutomationExecutionParametersException where
  show = genericShow
instance decodeInvalidAutomationExecutionParametersException :: Decode InvalidAutomationExecutionParametersException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAutomationExecutionParametersException :: Encode InvalidAutomationExecutionParametersException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The signal is not valid for the current Automation execution.</p>
newtype InvalidAutomationSignalException = InvalidAutomationSignalException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidAutomationSignalException :: Newtype InvalidAutomationSignalException _
derive instance repGenericInvalidAutomationSignalException :: Generic InvalidAutomationSignalException _
instance showInvalidAutomationSignalException :: Show InvalidAutomationSignalException where
  show = genericShow
instance decodeInvalidAutomationSignalException :: Decode InvalidAutomationSignalException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAutomationSignalException :: Encode InvalidAutomationSignalException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified update status operation is not valid.</p>
newtype InvalidAutomationStatusUpdateException = InvalidAutomationStatusUpdateException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidAutomationStatusUpdateException :: Newtype InvalidAutomationStatusUpdateException _
derive instance repGenericInvalidAutomationStatusUpdateException :: Generic InvalidAutomationStatusUpdateException _
instance showInvalidAutomationStatusUpdateException :: Show InvalidAutomationStatusUpdateException where
  show = genericShow
instance decodeInvalidAutomationStatusUpdateException :: Decode InvalidAutomationStatusUpdateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidAutomationStatusUpdateException :: Encode InvalidAutomationStatusUpdateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidCommandId = InvalidCommandId Types.NoArguments
derive instance newtypeInvalidCommandId :: Newtype InvalidCommandId _
derive instance repGenericInvalidCommandId :: Generic InvalidCommandId _
instance showInvalidCommandId :: Show InvalidCommandId where
  show = genericShow
instance decodeInvalidCommandId :: Decode InvalidCommandId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidCommandId :: Encode InvalidCommandId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified document does not exist.</p>
newtype InvalidDocument = InvalidDocument 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidDocument :: Newtype InvalidDocument _
derive instance repGenericInvalidDocument :: Generic InvalidDocument _
instance showInvalidDocument :: Show InvalidDocument where
  show = genericShow
instance decodeInvalidDocument :: Decode InvalidDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDocument :: Encode InvalidDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The content for the document is not valid.</p>
newtype InvalidDocumentContent = InvalidDocumentContent 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidDocumentContent :: Newtype InvalidDocumentContent _
derive instance repGenericInvalidDocumentContent :: Generic InvalidDocumentContent _
instance showInvalidDocumentContent :: Show InvalidDocumentContent where
  show = genericShow
instance decodeInvalidDocumentContent :: Decode InvalidDocumentContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDocumentContent :: Encode InvalidDocumentContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You attempted to delete a document while it is still shared. You must stop sharing the document before you can delete it.</p>
newtype InvalidDocumentOperation = InvalidDocumentOperation 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidDocumentOperation :: Newtype InvalidDocumentOperation _
derive instance repGenericInvalidDocumentOperation :: Generic InvalidDocumentOperation _
instance showInvalidDocumentOperation :: Show InvalidDocumentOperation where
  show = genericShow
instance decodeInvalidDocumentOperation :: Decode InvalidDocumentOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDocumentOperation :: Encode InvalidDocumentOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The version of the document schema is not supported.</p>
newtype InvalidDocumentSchemaVersion = InvalidDocumentSchemaVersion 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidDocumentSchemaVersion :: Newtype InvalidDocumentSchemaVersion _
derive instance repGenericInvalidDocumentSchemaVersion :: Generic InvalidDocumentSchemaVersion _
instance showInvalidDocumentSchemaVersion :: Show InvalidDocumentSchemaVersion where
  show = genericShow
instance decodeInvalidDocumentSchemaVersion :: Decode InvalidDocumentSchemaVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDocumentSchemaVersion :: Encode InvalidDocumentSchemaVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The document version is not valid or does not exist.</p>
newtype InvalidDocumentVersion = InvalidDocumentVersion 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidDocumentVersion :: Newtype InvalidDocumentVersion _
derive instance repGenericInvalidDocumentVersion :: Generic InvalidDocumentVersion _
instance showInvalidDocumentVersion :: Show InvalidDocumentVersion where
  show = genericShow
instance decodeInvalidDocumentVersion :: Decode InvalidDocumentVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDocumentVersion :: Encode InvalidDocumentVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The filter name is not valid. Verify the you entered the correct name and try again.</p>
newtype InvalidFilter = InvalidFilter 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidFilter :: Newtype InvalidFilter _
derive instance repGenericInvalidFilter :: Generic InvalidFilter _
instance showInvalidFilter :: Show InvalidFilter where
  show = genericShow
instance decodeInvalidFilter :: Decode InvalidFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFilter :: Encode InvalidFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified key is not valid.</p>
newtype InvalidFilterKey = InvalidFilterKey Types.NoArguments
derive instance newtypeInvalidFilterKey :: Newtype InvalidFilterKey _
derive instance repGenericInvalidFilterKey :: Generic InvalidFilterKey _
instance showInvalidFilterKey :: Show InvalidFilterKey where
  show = genericShow
instance decodeInvalidFilterKey :: Decode InvalidFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFilterKey :: Encode InvalidFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified filter option is not valid. Valid options are Equals and BeginsWith. For Path filter, valid options are Recursive and OneLevel.</p>
newtype InvalidFilterOption = InvalidFilterOption 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidFilterOption :: Newtype InvalidFilterOption _
derive instance repGenericInvalidFilterOption :: Generic InvalidFilterOption _
instance showInvalidFilterOption :: Show InvalidFilterOption where
  show = genericShow
instance decodeInvalidFilterOption :: Decode InvalidFilterOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFilterOption :: Encode InvalidFilterOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The filter value is not valid. Verify the value and try again.</p>
newtype InvalidFilterValue = InvalidFilterValue 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidFilterValue :: Newtype InvalidFilterValue _
derive instance repGenericInvalidFilterValue :: Generic InvalidFilterValue _
instance showInvalidFilterValue :: Show InvalidFilterValue where
  show = genericShow
instance decodeInvalidFilterValue :: Decode InvalidFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFilterValue :: Encode InvalidFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The following problems can cause this exception:</p> <p>You do not have permission to access the instance.</p> <p>The SSM Agent is not running. On managed instances and Linux instances, verify that the SSM Agent is running. On EC2 Windows instances, verify that the EC2Config service is running.</p> <p>The SSM Agent or EC2Config service is not registered to the SSM endpoint. Try reinstalling the SSM Agent or EC2Config service.</p> <p>The instance is not in valid state. Valid states are: Running, Pending, Stopped, Stopping. Invalid states are: Shutting-down and Terminated.</p>
newtype InvalidInstanceId = InvalidInstanceId 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidInstanceId :: Newtype InvalidInstanceId _
derive instance repGenericInvalidInstanceId :: Generic InvalidInstanceId _
instance showInvalidInstanceId :: Show InvalidInstanceId where
  show = genericShow
instance decodeInvalidInstanceId :: Decode InvalidInstanceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidInstanceId :: Encode InvalidInstanceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified filter value is not valid.</p>
newtype InvalidInstanceInformationFilterValue = InvalidInstanceInformationFilterValue 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidInstanceInformationFilterValue :: Newtype InvalidInstanceInformationFilterValue _
derive instance repGenericInvalidInstanceInformationFilterValue :: Generic InvalidInstanceInformationFilterValue _
instance showInvalidInstanceInformationFilterValue :: Show InvalidInstanceInformationFilterValue where
  show = genericShow
instance decodeInvalidInstanceInformationFilterValue :: Decode InvalidInstanceInformationFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidInstanceInformationFilterValue :: Encode InvalidInstanceInformationFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You specified invalid keys or values in the <code>Context</code> attribute for <code>InventoryItem</code>. Verify the keys and values, and try again.</p>
newtype InvalidInventoryItemContextException = InvalidInventoryItemContextException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidInventoryItemContextException :: Newtype InvalidInventoryItemContextException _
derive instance repGenericInvalidInventoryItemContextException :: Generic InvalidInventoryItemContextException _
instance showInvalidInventoryItemContextException :: Show InvalidInventoryItemContextException where
  show = genericShow
instance decodeInvalidInventoryItemContextException :: Decode InvalidInventoryItemContextException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidInventoryItemContextException :: Encode InvalidInventoryItemContextException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more content items is not valid.</p>
newtype InvalidItemContentException = InvalidItemContentException 
  { "TypeName" :: NullOrUndefined.NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidItemContentException :: Newtype InvalidItemContentException _
derive instance repGenericInvalidItemContentException :: Generic InvalidItemContentException _
instance showInvalidItemContentException :: Show InvalidItemContentException where
  show = genericShow
instance decodeInvalidItemContentException :: Decode InvalidItemContentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidItemContentException :: Encode InvalidItemContentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The query key ID is not valid.</p>
newtype InvalidKeyId = InvalidKeyId 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidKeyId :: Newtype InvalidKeyId _
derive instance repGenericInvalidKeyId :: Generic InvalidKeyId _
instance showInvalidKeyId :: Show InvalidKeyId where
  show = genericShow
instance decodeInvalidKeyId :: Decode InvalidKeyId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidKeyId :: Encode InvalidKeyId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified token is not valid.</p>
newtype InvalidNextToken = InvalidNextToken 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidNextToken :: Newtype InvalidNextToken _
derive instance repGenericInvalidNextToken :: Generic InvalidNextToken _
instance showInvalidNextToken :: Show InvalidNextToken where
  show = genericShow
instance decodeInvalidNextToken :: Decode InvalidNextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextToken :: Encode InvalidNextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more configuration items is not valid. Verify that a valid Amazon Resource Name (ARN) was provided for an Amazon SNS topic.</p>
newtype InvalidNotificationConfig = InvalidNotificationConfig 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidNotificationConfig :: Newtype InvalidNotificationConfig _
derive instance repGenericInvalidNotificationConfig :: Generic InvalidNotificationConfig _
instance showInvalidNotificationConfig :: Show InvalidNotificationConfig where
  show = genericShow
instance decodeInvalidNotificationConfig :: Decode InvalidNotificationConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNotificationConfig :: Encode InvalidNotificationConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The S3 bucket does not exist.</p>
newtype InvalidOutputFolder = InvalidOutputFolder Types.NoArguments
derive instance newtypeInvalidOutputFolder :: Newtype InvalidOutputFolder _
derive instance repGenericInvalidOutputFolder :: Generic InvalidOutputFolder _
instance showInvalidOutputFolder :: Show InvalidOutputFolder where
  show = genericShow
instance decodeInvalidOutputFolder :: Decode InvalidOutputFolder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOutputFolder :: Encode InvalidOutputFolder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output location is not valid or does not exist.</p>
newtype InvalidOutputLocation = InvalidOutputLocation Types.NoArguments
derive instance newtypeInvalidOutputLocation :: Newtype InvalidOutputLocation _
derive instance repGenericInvalidOutputLocation :: Generic InvalidOutputLocation _
instance showInvalidOutputLocation :: Show InvalidOutputLocation where
  show = genericShow
instance decodeInvalidOutputLocation :: Decode InvalidOutputLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOutputLocation :: Encode InvalidOutputLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You must specify values for all required parameters in the Systems Manager document. You can only supply values to parameters defined in the Systems Manager document.</p>
newtype InvalidParameters = InvalidParameters 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidParameters :: Newtype InvalidParameters _
derive instance repGenericInvalidParameters :: Generic InvalidParameters _
instance showInvalidParameters :: Show InvalidParameters where
  show = genericShow
instance decodeInvalidParameters :: Decode InvalidParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameters :: Encode InvalidParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The permission type is not supported. <i>Share</i> is the only supported permission type.</p>
newtype InvalidPermissionType = InvalidPermissionType 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidPermissionType :: Newtype InvalidPermissionType _
derive instance repGenericInvalidPermissionType :: Generic InvalidPermissionType _
instance showInvalidPermissionType :: Show InvalidPermissionType where
  show = genericShow
instance decodeInvalidPermissionType :: Decode InvalidPermissionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPermissionType :: Encode InvalidPermissionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The plugin name is not valid.</p>
newtype InvalidPluginName = InvalidPluginName Types.NoArguments
derive instance newtypeInvalidPluginName :: Newtype InvalidPluginName _
derive instance repGenericInvalidPluginName :: Generic InvalidPluginName _
instance showInvalidPluginName :: Show InvalidPluginName where
  show = genericShow
instance decodeInvalidPluginName :: Decode InvalidPluginName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPluginName :: Encode InvalidPluginName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource ID is not valid. Verify that you entered the correct ID and try again.</p>
newtype InvalidResourceId = InvalidResourceId Types.NoArguments
derive instance newtypeInvalidResourceId :: Newtype InvalidResourceId _
derive instance repGenericInvalidResourceId :: Generic InvalidResourceId _
instance showInvalidResourceId :: Show InvalidResourceId where
  show = genericShow
instance decodeInvalidResourceId :: Decode InvalidResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidResourceId :: Encode InvalidResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource type is not valid. For example, if you are attempting to tag an instance, the instance must be a registered, managed instance.</p>
newtype InvalidResourceType = InvalidResourceType Types.NoArguments
derive instance newtypeInvalidResourceType :: Newtype InvalidResourceType _
derive instance repGenericInvalidResourceType :: Generic InvalidResourceType _
instance showInvalidResourceType :: Show InvalidResourceType where
  show = genericShow
instance decodeInvalidResourceType :: Decode InvalidResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidResourceType :: Encode InvalidResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified inventory item result attribute is not valid.</p>
newtype InvalidResultAttributeException = InvalidResultAttributeException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidResultAttributeException :: Newtype InvalidResultAttributeException _
derive instance repGenericInvalidResultAttributeException :: Generic InvalidResultAttributeException _
instance showInvalidResultAttributeException :: Show InvalidResultAttributeException where
  show = genericShow
instance decodeInvalidResultAttributeException :: Decode InvalidResultAttributeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidResultAttributeException :: Encode InvalidResultAttributeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The role name can't contain invalid characters. Also verify that you specified an IAM role for notifications that includes the required trust policy. For information about configuring the IAM role for Run Command notifications, see <a href="http://docs.aws.amazon.com/systems-manager/latest/userguide/rc-sns-notifications.html">Configuring Amazon SNS Notifications for Run Command</a> in the <i>AWS Systems Manager User Guide</i>.</p>
newtype InvalidRole = InvalidRole 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidRole :: Newtype InvalidRole _
derive instance repGenericInvalidRole :: Generic InvalidRole _
instance showInvalidRole :: Show InvalidRole where
  show = genericShow
instance decodeInvalidRole :: Decode InvalidRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRole :: Encode InvalidRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The schedule is invalid. Verify your cron or rate expression and try again.</p>
newtype InvalidSchedule = InvalidSchedule 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidSchedule :: Newtype InvalidSchedule _
derive instance repGenericInvalidSchedule :: Generic InvalidSchedule _
instance showInvalidSchedule :: Show InvalidSchedule where
  show = genericShow
instance decodeInvalidSchedule :: Decode InvalidSchedule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSchedule :: Encode InvalidSchedule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The target is not valid or does not exist. It might not be configured for EC2 Systems Manager or you might not have permission to perform the operation.</p>
newtype InvalidTarget = InvalidTarget 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidTarget :: Newtype InvalidTarget _
derive instance repGenericInvalidTarget :: Generic InvalidTarget _
instance showInvalidTarget :: Show InvalidTarget where
  show = genericShow
instance decodeInvalidTarget :: Decode InvalidTarget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTarget :: Encode InvalidTarget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameter type name is not valid.</p>
newtype InvalidTypeNameException = InvalidTypeNameException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidTypeNameException :: Newtype InvalidTypeNameException _
derive instance repGenericInvalidTypeNameException :: Generic InvalidTypeNameException _
instance showInvalidTypeNameException :: Show InvalidTypeNameException where
  show = genericShow
instance decodeInvalidTypeNameException :: Decode InvalidTypeNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTypeNameException :: Encode InvalidTypeNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The update is not valid.</p>
newtype InvalidUpdate = InvalidUpdate 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidUpdate :: Newtype InvalidUpdate _
derive instance repGenericInvalidUpdate :: Generic InvalidUpdate _
instance showInvalidUpdate :: Show InvalidUpdate where
  show = genericShow
instance decodeInvalidUpdate :: Decode InvalidUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidUpdate :: Encode InvalidUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the inventory type and attribute for the aggregation execution.</p>
newtype InventoryAggregator = InventoryAggregator 
  { "Expression" :: NullOrUndefined.NullOrUndefined (InventoryAggregatorExpression)
  }
derive instance newtypeInventoryAggregator :: Newtype InventoryAggregator _
derive instance repGenericInventoryAggregator :: Generic InventoryAggregator _
instance showInventoryAggregator :: Show InventoryAggregator where
  show = genericShow
instance decodeInventoryAggregator :: Decode InventoryAggregator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryAggregator :: Encode InventoryAggregator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryAggregatorExpression = InventoryAggregatorExpression String
derive instance newtypeInventoryAggregatorExpression :: Newtype InventoryAggregatorExpression _
derive instance repGenericInventoryAggregatorExpression :: Generic InventoryAggregatorExpression _
instance showInventoryAggregatorExpression :: Show InventoryAggregatorExpression where
  show = genericShow
instance decodeInventoryAggregatorExpression :: Decode InventoryAggregatorExpression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryAggregatorExpression :: Encode InventoryAggregatorExpression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryAggregatorList = InventoryAggregatorList (Array InventoryAggregator)
derive instance newtypeInventoryAggregatorList :: Newtype InventoryAggregatorList _
derive instance repGenericInventoryAggregatorList :: Generic InventoryAggregatorList _
instance showInventoryAggregatorList :: Show InventoryAggregatorList where
  show = genericShow
instance decodeInventoryAggregatorList :: Decode InventoryAggregatorList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryAggregatorList :: Encode InventoryAggregatorList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryAttributeDataType = InventoryAttributeDataType String
derive instance newtypeInventoryAttributeDataType :: Newtype InventoryAttributeDataType _
derive instance repGenericInventoryAttributeDataType :: Generic InventoryAttributeDataType _
instance showInventoryAttributeDataType :: Show InventoryAttributeDataType where
  show = genericShow
instance decodeInventoryAttributeDataType :: Decode InventoryAttributeDataType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryAttributeDataType :: Encode InventoryAttributeDataType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more filters. Use a filter to return a more specific list of results.</p>
newtype InventoryFilter = InventoryFilter 
  { "Key" :: (InventoryFilterKey)
  , "Values" :: (InventoryFilterValueList)
  , "Type" :: NullOrUndefined.NullOrUndefined (InventoryQueryOperatorType)
  }
derive instance newtypeInventoryFilter :: Newtype InventoryFilter _
derive instance repGenericInventoryFilter :: Generic InventoryFilter _
instance showInventoryFilter :: Show InventoryFilter where
  show = genericShow
instance decodeInventoryFilter :: Decode InventoryFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryFilter :: Encode InventoryFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryFilterKey = InventoryFilterKey String
derive instance newtypeInventoryFilterKey :: Newtype InventoryFilterKey _
derive instance repGenericInventoryFilterKey :: Generic InventoryFilterKey _
instance showInventoryFilterKey :: Show InventoryFilterKey where
  show = genericShow
instance decodeInventoryFilterKey :: Decode InventoryFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryFilterKey :: Encode InventoryFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryFilterList = InventoryFilterList (Array InventoryFilter)
derive instance newtypeInventoryFilterList :: Newtype InventoryFilterList _
derive instance repGenericInventoryFilterList :: Generic InventoryFilterList _
instance showInventoryFilterList :: Show InventoryFilterList where
  show = genericShow
instance decodeInventoryFilterList :: Decode InventoryFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryFilterList :: Encode InventoryFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryFilterValue = InventoryFilterValue String
derive instance newtypeInventoryFilterValue :: Newtype InventoryFilterValue _
derive instance repGenericInventoryFilterValue :: Generic InventoryFilterValue _
instance showInventoryFilterValue :: Show InventoryFilterValue where
  show = genericShow
instance decodeInventoryFilterValue :: Decode InventoryFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryFilterValue :: Encode InventoryFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryFilterValueList = InventoryFilterValueList (Array InventoryFilterValue)
derive instance newtypeInventoryFilterValueList :: Newtype InventoryFilterValueList _
derive instance repGenericInventoryFilterValueList :: Generic InventoryFilterValueList _
instance showInventoryFilterValueList :: Show InventoryFilterValueList where
  show = genericShow
instance decodeInventoryFilterValueList :: Decode InventoryFilterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryFilterValueList :: Encode InventoryFilterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information collected from managed instances based on your inventory policy document</p>
newtype InventoryItem = InventoryItem 
  { "TypeName" :: (InventoryItemTypeName)
  , "SchemaVersion" :: (InventoryItemSchemaVersion)
  , "CaptureTime" :: (InventoryItemCaptureTime)
  , "ContentHash" :: NullOrUndefined.NullOrUndefined (InventoryItemContentHash)
  , "Content" :: NullOrUndefined.NullOrUndefined (InventoryItemEntryList)
  , "Context" :: NullOrUndefined.NullOrUndefined (InventoryItemContentContext)
  }
derive instance newtypeInventoryItem :: Newtype InventoryItem _
derive instance repGenericInventoryItem :: Generic InventoryItem _
instance showInventoryItem :: Show InventoryItem where
  show = genericShow
instance decodeInventoryItem :: Decode InventoryItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItem :: Encode InventoryItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Attributes are the entries within the inventory item content. It contains name and value.</p>
newtype InventoryItemAttribute = InventoryItemAttribute 
  { "Name" :: (InventoryItemAttributeName)
  , "DataType" :: (InventoryAttributeDataType)
  }
derive instance newtypeInventoryItemAttribute :: Newtype InventoryItemAttribute _
derive instance repGenericInventoryItemAttribute :: Generic InventoryItemAttribute _
instance showInventoryItemAttribute :: Show InventoryItemAttribute where
  show = genericShow
instance decodeInventoryItemAttribute :: Decode InventoryItemAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemAttribute :: Encode InventoryItemAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemAttributeList = InventoryItemAttributeList (Array InventoryItemAttribute)
derive instance newtypeInventoryItemAttributeList :: Newtype InventoryItemAttributeList _
derive instance repGenericInventoryItemAttributeList :: Generic InventoryItemAttributeList _
instance showInventoryItemAttributeList :: Show InventoryItemAttributeList where
  show = genericShow
instance decodeInventoryItemAttributeList :: Decode InventoryItemAttributeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemAttributeList :: Encode InventoryItemAttributeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemAttributeName = InventoryItemAttributeName String
derive instance newtypeInventoryItemAttributeName :: Newtype InventoryItemAttributeName _
derive instance repGenericInventoryItemAttributeName :: Generic InventoryItemAttributeName _
instance showInventoryItemAttributeName :: Show InventoryItemAttributeName where
  show = genericShow
instance decodeInventoryItemAttributeName :: Decode InventoryItemAttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemAttributeName :: Encode InventoryItemAttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemCaptureTime = InventoryItemCaptureTime String
derive instance newtypeInventoryItemCaptureTime :: Newtype InventoryItemCaptureTime _
derive instance repGenericInventoryItemCaptureTime :: Generic InventoryItemCaptureTime _
instance showInventoryItemCaptureTime :: Show InventoryItemCaptureTime where
  show = genericShow
instance decodeInventoryItemCaptureTime :: Decode InventoryItemCaptureTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemCaptureTime :: Encode InventoryItemCaptureTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemContentContext = InventoryItemContentContext (StrMap.StrMap AttributeValue)
derive instance newtypeInventoryItemContentContext :: Newtype InventoryItemContentContext _
derive instance repGenericInventoryItemContentContext :: Generic InventoryItemContentContext _
instance showInventoryItemContentContext :: Show InventoryItemContentContext where
  show = genericShow
instance decodeInventoryItemContentContext :: Decode InventoryItemContentContext where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemContentContext :: Encode InventoryItemContentContext where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemContentHash = InventoryItemContentHash String
derive instance newtypeInventoryItemContentHash :: Newtype InventoryItemContentHash _
derive instance repGenericInventoryItemContentHash :: Generic InventoryItemContentHash _
instance showInventoryItemContentHash :: Show InventoryItemContentHash where
  show = genericShow
instance decodeInventoryItemContentHash :: Decode InventoryItemContentHash where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemContentHash :: Encode InventoryItemContentHash where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemEntry = InventoryItemEntry (StrMap.StrMap AttributeValue)
derive instance newtypeInventoryItemEntry :: Newtype InventoryItemEntry _
derive instance repGenericInventoryItemEntry :: Generic InventoryItemEntry _
instance showInventoryItemEntry :: Show InventoryItemEntry where
  show = genericShow
instance decodeInventoryItemEntry :: Decode InventoryItemEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemEntry :: Encode InventoryItemEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemEntryList = InventoryItemEntryList (Array InventoryItemEntry)
derive instance newtypeInventoryItemEntryList :: Newtype InventoryItemEntryList _
derive instance repGenericInventoryItemEntryList :: Generic InventoryItemEntryList _
instance showInventoryItemEntryList :: Show InventoryItemEntryList where
  show = genericShow
instance decodeInventoryItemEntryList :: Decode InventoryItemEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemEntryList :: Encode InventoryItemEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemList = InventoryItemList (Array InventoryItem)
derive instance newtypeInventoryItemList :: Newtype InventoryItemList _
derive instance repGenericInventoryItemList :: Generic InventoryItemList _
instance showInventoryItemList :: Show InventoryItemList where
  show = genericShow
instance decodeInventoryItemList :: Decode InventoryItemList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemList :: Encode InventoryItemList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The inventory item schema definition. Users can use this to compose inventory query filters.</p>
newtype InventoryItemSchema = InventoryItemSchema 
  { "TypeName" :: (InventoryItemTypeName)
  , "Version" :: NullOrUndefined.NullOrUndefined (InventoryItemSchemaVersion)
  , "Attributes" :: (InventoryItemAttributeList)
  , "DisplayName" :: NullOrUndefined.NullOrUndefined (InventoryTypeDisplayName)
  }
derive instance newtypeInventoryItemSchema :: Newtype InventoryItemSchema _
derive instance repGenericInventoryItemSchema :: Generic InventoryItemSchema _
instance showInventoryItemSchema :: Show InventoryItemSchema where
  show = genericShow
instance decodeInventoryItemSchema :: Decode InventoryItemSchema where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemSchema :: Encode InventoryItemSchema where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemSchemaResultList = InventoryItemSchemaResultList (Array InventoryItemSchema)
derive instance newtypeInventoryItemSchemaResultList :: Newtype InventoryItemSchemaResultList _
derive instance repGenericInventoryItemSchemaResultList :: Generic InventoryItemSchemaResultList _
instance showInventoryItemSchemaResultList :: Show InventoryItemSchemaResultList where
  show = genericShow
instance decodeInventoryItemSchemaResultList :: Decode InventoryItemSchemaResultList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemSchemaResultList :: Encode InventoryItemSchemaResultList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemSchemaVersion = InventoryItemSchemaVersion String
derive instance newtypeInventoryItemSchemaVersion :: Newtype InventoryItemSchemaVersion _
derive instance repGenericInventoryItemSchemaVersion :: Generic InventoryItemSchemaVersion _
instance showInventoryItemSchemaVersion :: Show InventoryItemSchemaVersion where
  show = genericShow
instance decodeInventoryItemSchemaVersion :: Decode InventoryItemSchemaVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemSchemaVersion :: Encode InventoryItemSchemaVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemTypeName = InventoryItemTypeName String
derive instance newtypeInventoryItemTypeName :: Newtype InventoryItemTypeName _
derive instance repGenericInventoryItemTypeName :: Generic InventoryItemTypeName _
instance showInventoryItemTypeName :: Show InventoryItemTypeName where
  show = genericShow
instance decodeInventoryItemTypeName :: Decode InventoryItemTypeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemTypeName :: Encode InventoryItemTypeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryItemTypeNameFilter = InventoryItemTypeNameFilter String
derive instance newtypeInventoryItemTypeNameFilter :: Newtype InventoryItemTypeNameFilter _
derive instance repGenericInventoryItemTypeNameFilter :: Generic InventoryItemTypeNameFilter _
instance showInventoryItemTypeNameFilter :: Show InventoryItemTypeNameFilter where
  show = genericShow
instance decodeInventoryItemTypeNameFilter :: Decode InventoryItemTypeNameFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryItemTypeNameFilter :: Encode InventoryItemTypeNameFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryQueryOperatorType = InventoryQueryOperatorType String
derive instance newtypeInventoryQueryOperatorType :: Newtype InventoryQueryOperatorType _
derive instance repGenericInventoryQueryOperatorType :: Generic InventoryQueryOperatorType _
instance showInventoryQueryOperatorType :: Show InventoryQueryOperatorType where
  show = genericShow
instance decodeInventoryQueryOperatorType :: Decode InventoryQueryOperatorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryQueryOperatorType :: Encode InventoryQueryOperatorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Inventory query results.</p>
newtype InventoryResultEntity = InventoryResultEntity 
  { "Id" :: NullOrUndefined.NullOrUndefined (InventoryResultEntityId)
  , "Data" :: NullOrUndefined.NullOrUndefined (InventoryResultItemMap)
  }
derive instance newtypeInventoryResultEntity :: Newtype InventoryResultEntity _
derive instance repGenericInventoryResultEntity :: Generic InventoryResultEntity _
instance showInventoryResultEntity :: Show InventoryResultEntity where
  show = genericShow
instance decodeInventoryResultEntity :: Decode InventoryResultEntity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryResultEntity :: Encode InventoryResultEntity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryResultEntityId = InventoryResultEntityId String
derive instance newtypeInventoryResultEntityId :: Newtype InventoryResultEntityId _
derive instance repGenericInventoryResultEntityId :: Generic InventoryResultEntityId _
instance showInventoryResultEntityId :: Show InventoryResultEntityId where
  show = genericShow
instance decodeInventoryResultEntityId :: Decode InventoryResultEntityId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryResultEntityId :: Encode InventoryResultEntityId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryResultEntityList = InventoryResultEntityList (Array InventoryResultEntity)
derive instance newtypeInventoryResultEntityList :: Newtype InventoryResultEntityList _
derive instance repGenericInventoryResultEntityList :: Generic InventoryResultEntityList _
instance showInventoryResultEntityList :: Show InventoryResultEntityList where
  show = genericShow
instance decodeInventoryResultEntityList :: Decode InventoryResultEntityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryResultEntityList :: Encode InventoryResultEntityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The inventory result item.</p>
newtype InventoryResultItem = InventoryResultItem 
  { "TypeName" :: (InventoryItemTypeName)
  , "SchemaVersion" :: (InventoryItemSchemaVersion)
  , "CaptureTime" :: NullOrUndefined.NullOrUndefined (InventoryItemCaptureTime)
  , "ContentHash" :: NullOrUndefined.NullOrUndefined (InventoryItemContentHash)
  , "Content" :: (InventoryItemEntryList)
  }
derive instance newtypeInventoryResultItem :: Newtype InventoryResultItem _
derive instance repGenericInventoryResultItem :: Generic InventoryResultItem _
instance showInventoryResultItem :: Show InventoryResultItem where
  show = genericShow
instance decodeInventoryResultItem :: Decode InventoryResultItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryResultItem :: Encode InventoryResultItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryResultItemKey = InventoryResultItemKey String
derive instance newtypeInventoryResultItemKey :: Newtype InventoryResultItemKey _
derive instance repGenericInventoryResultItemKey :: Generic InventoryResultItemKey _
instance showInventoryResultItemKey :: Show InventoryResultItemKey where
  show = genericShow
instance decodeInventoryResultItemKey :: Decode InventoryResultItemKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryResultItemKey :: Encode InventoryResultItemKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryResultItemMap = InventoryResultItemMap (StrMap.StrMap InventoryResultItem)
derive instance newtypeInventoryResultItemMap :: Newtype InventoryResultItemMap _
derive instance repGenericInventoryResultItemMap :: Generic InventoryResultItemMap _
instance showInventoryResultItemMap :: Show InventoryResultItemMap where
  show = genericShow
instance decodeInventoryResultItemMap :: Decode InventoryResultItemMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryResultItemMap :: Encode InventoryResultItemMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InventoryTypeDisplayName = InventoryTypeDisplayName String
derive instance newtypeInventoryTypeDisplayName :: Newtype InventoryTypeDisplayName _
derive instance repGenericInventoryTypeDisplayName :: Generic InventoryTypeDisplayName _
instance showInventoryTypeDisplayName :: Show InventoryTypeDisplayName where
  show = genericShow
instance decodeInventoryTypeDisplayName :: Decode InventoryTypeDisplayName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInventoryTypeDisplayName :: Encode InventoryTypeDisplayName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The command ID and instance ID you specified did not match any invocations. Verify the command ID adn the instance ID and try again. </p>
newtype InvocationDoesNotExist = InvocationDoesNotExist Types.NoArguments
derive instance newtypeInvocationDoesNotExist :: Newtype InvocationDoesNotExist _
derive instance repGenericInvocationDoesNotExist :: Generic InvocationDoesNotExist _
instance showInvocationDoesNotExist :: Show InvocationDoesNotExist where
  show = genericShow
instance decodeInvocationDoesNotExist :: Decode InvocationDoesNotExist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvocationDoesNotExist :: Encode InvocationDoesNotExist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvocationTraceOutput = InvocationTraceOutput String
derive instance newtypeInvocationTraceOutput :: Newtype InvocationTraceOutput _
derive instance repGenericInvocationTraceOutput :: Generic InvocationTraceOutput _
instance showInvocationTraceOutput :: Show InvocationTraceOutput where
  show = genericShow
instance decodeInvocationTraceOutput :: Decode InvocationTraceOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvocationTraceOutput :: Encode InvocationTraceOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsSubTypeSchema = IsSubTypeSchema Boolean
derive instance newtypeIsSubTypeSchema :: Newtype IsSubTypeSchema _
derive instance repGenericIsSubTypeSchema :: Generic IsSubTypeSchema _
instance showIsSubTypeSchema :: Show IsSubTypeSchema where
  show = genericShow
instance decodeIsSubTypeSchema :: Decode IsSubTypeSchema where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsSubTypeSchema :: Encode IsSubTypeSchema where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The inventory item has invalid content. </p>
newtype ItemContentMismatchException = ItemContentMismatchException 
  { "TypeName" :: NullOrUndefined.NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeItemContentMismatchException :: Newtype ItemContentMismatchException _
derive instance repGenericItemContentMismatchException :: Generic ItemContentMismatchException _
instance showItemContentMismatchException :: Show ItemContentMismatchException where
  show = genericShow
instance decodeItemContentMismatchException :: Decode ItemContentMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeItemContentMismatchException :: Encode ItemContentMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The inventory item size has exceeded the size limit.</p>
newtype ItemSizeLimitExceededException = ItemSizeLimitExceededException 
  { "TypeName" :: NullOrUndefined.NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeItemSizeLimitExceededException :: Newtype ItemSizeLimitExceededException _
derive instance repGenericItemSizeLimitExceededException :: Generic ItemSizeLimitExceededException _
instance showItemSizeLimitExceededException :: Show ItemSizeLimitExceededException where
  show = genericShow
instance decodeItemSizeLimitExceededException :: Decode ItemSizeLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeItemSizeLimitExceededException :: Encode ItemSizeLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyList = KeyList (Array TagKey)
derive instance newtypeKeyList :: Newtype KeyList _
derive instance repGenericKeyList :: Generic KeyList _
instance showKeyList :: Show KeyList where
  show = genericShow
instance decodeKeyList :: Decode KeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyList :: Encode KeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastResourceDataSyncStatus = LastResourceDataSyncStatus String
derive instance newtypeLastResourceDataSyncStatus :: Newtype LastResourceDataSyncStatus _
derive instance repGenericLastResourceDataSyncStatus :: Generic LastResourceDataSyncStatus _
instance showLastResourceDataSyncStatus :: Show LastResourceDataSyncStatus where
  show = genericShow
instance decodeLastResourceDataSyncStatus :: Decode LastResourceDataSyncStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastResourceDataSyncStatus :: Encode LastResourceDataSyncStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastResourceDataSyncTime = LastResourceDataSyncTime Number
derive instance newtypeLastResourceDataSyncTime :: Newtype LastResourceDataSyncTime _
derive instance repGenericLastResourceDataSyncTime :: Generic LastResourceDataSyncTime _
instance showLastResourceDataSyncTime :: Show LastResourceDataSyncTime where
  show = genericShow
instance decodeLastResourceDataSyncTime :: Decode LastResourceDataSyncTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastResourceDataSyncTime :: Encode LastResourceDataSyncTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastSuccessfulResourceDataSyncTime = LastSuccessfulResourceDataSyncTime Number
derive instance newtypeLastSuccessfulResourceDataSyncTime :: Newtype LastSuccessfulResourceDataSyncTime _
derive instance repGenericLastSuccessfulResourceDataSyncTime :: Generic LastSuccessfulResourceDataSyncTime _
instance showLastSuccessfulResourceDataSyncTime :: Show LastSuccessfulResourceDataSyncTime where
  show = genericShow
instance decodeLastSuccessfulResourceDataSyncTime :: Decode LastSuccessfulResourceDataSyncTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastSuccessfulResourceDataSyncTime :: Encode LastSuccessfulResourceDataSyncTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAssociationVersionsRequest = ListAssociationVersionsRequest 
  { "AssociationId" :: (AssociationId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListAssociationVersionsRequest :: Newtype ListAssociationVersionsRequest _
derive instance repGenericListAssociationVersionsRequest :: Generic ListAssociationVersionsRequest _
instance showListAssociationVersionsRequest :: Show ListAssociationVersionsRequest where
  show = genericShow
instance decodeListAssociationVersionsRequest :: Decode ListAssociationVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAssociationVersionsRequest :: Encode ListAssociationVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAssociationVersionsResult = ListAssociationVersionsResult 
  { "AssociationVersions" :: NullOrUndefined.NullOrUndefined (AssociationVersionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListAssociationVersionsResult :: Newtype ListAssociationVersionsResult _
derive instance repGenericListAssociationVersionsResult :: Generic ListAssociationVersionsResult _
instance showListAssociationVersionsResult :: Show ListAssociationVersionsResult where
  show = genericShow
instance decodeListAssociationVersionsResult :: Decode ListAssociationVersionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAssociationVersionsResult :: Encode ListAssociationVersionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAssociationsRequest = ListAssociationsRequest 
  { "AssociationFilterList" :: NullOrUndefined.NullOrUndefined (AssociationFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListAssociationsRequest :: Newtype ListAssociationsRequest _
derive instance repGenericListAssociationsRequest :: Generic ListAssociationsRequest _
instance showListAssociationsRequest :: Show ListAssociationsRequest where
  show = genericShow
instance decodeListAssociationsRequest :: Decode ListAssociationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAssociationsRequest :: Encode ListAssociationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAssociationsResult = ListAssociationsResult 
  { "Associations" :: NullOrUndefined.NullOrUndefined (AssociationList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListAssociationsResult :: Newtype ListAssociationsResult _
derive instance repGenericListAssociationsResult :: Generic ListAssociationsResult _
instance showListAssociationsResult :: Show ListAssociationsResult where
  show = genericShow
instance decodeListAssociationsResult :: Decode ListAssociationsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAssociationsResult :: Encode ListAssociationsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCommandInvocationsRequest = ListCommandInvocationsRequest 
  { "CommandId" :: NullOrUndefined.NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (CommandMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Filters" :: NullOrUndefined.NullOrUndefined (CommandFilterList)
  , "Details" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeListCommandInvocationsRequest :: Newtype ListCommandInvocationsRequest _
derive instance repGenericListCommandInvocationsRequest :: Generic ListCommandInvocationsRequest _
instance showListCommandInvocationsRequest :: Show ListCommandInvocationsRequest where
  show = genericShow
instance decodeListCommandInvocationsRequest :: Decode ListCommandInvocationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCommandInvocationsRequest :: Encode ListCommandInvocationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCommandInvocationsResult = ListCommandInvocationsResult 
  { "CommandInvocations" :: NullOrUndefined.NullOrUndefined (CommandInvocationList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListCommandInvocationsResult :: Newtype ListCommandInvocationsResult _
derive instance repGenericListCommandInvocationsResult :: Generic ListCommandInvocationsResult _
instance showListCommandInvocationsResult :: Show ListCommandInvocationsResult where
  show = genericShow
instance decodeListCommandInvocationsResult :: Decode ListCommandInvocationsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCommandInvocationsResult :: Encode ListCommandInvocationsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCommandsRequest = ListCommandsRequest 
  { "CommandId" :: NullOrUndefined.NullOrUndefined (CommandId)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (CommandMaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Filters" :: NullOrUndefined.NullOrUndefined (CommandFilterList)
  }
derive instance newtypeListCommandsRequest :: Newtype ListCommandsRequest _
derive instance repGenericListCommandsRequest :: Generic ListCommandsRequest _
instance showListCommandsRequest :: Show ListCommandsRequest where
  show = genericShow
instance decodeListCommandsRequest :: Decode ListCommandsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCommandsRequest :: Encode ListCommandsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCommandsResult = ListCommandsResult 
  { "Commands" :: NullOrUndefined.NullOrUndefined (CommandList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListCommandsResult :: Newtype ListCommandsResult _
derive instance repGenericListCommandsResult :: Generic ListCommandsResult _
instance showListCommandsResult :: Show ListCommandsResult where
  show = genericShow
instance decodeListCommandsResult :: Decode ListCommandsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCommandsResult :: Encode ListCommandsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListComplianceItemsRequest = ListComplianceItemsRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (ComplianceStringFilterList)
  , "ResourceIds" :: NullOrUndefined.NullOrUndefined (ComplianceResourceIdList)
  , "ResourceTypes" :: NullOrUndefined.NullOrUndefined (ComplianceResourceTypeList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListComplianceItemsRequest :: Newtype ListComplianceItemsRequest _
derive instance repGenericListComplianceItemsRequest :: Generic ListComplianceItemsRequest _
instance showListComplianceItemsRequest :: Show ListComplianceItemsRequest where
  show = genericShow
instance decodeListComplianceItemsRequest :: Decode ListComplianceItemsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListComplianceItemsRequest :: Encode ListComplianceItemsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListComplianceItemsResult = ListComplianceItemsResult 
  { "ComplianceItems" :: NullOrUndefined.NullOrUndefined (ComplianceItemList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListComplianceItemsResult :: Newtype ListComplianceItemsResult _
derive instance repGenericListComplianceItemsResult :: Generic ListComplianceItemsResult _
instance showListComplianceItemsResult :: Show ListComplianceItemsResult where
  show = genericShow
instance decodeListComplianceItemsResult :: Decode ListComplianceItemsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListComplianceItemsResult :: Encode ListComplianceItemsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListComplianceSummariesRequest = ListComplianceSummariesRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (ComplianceStringFilterList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListComplianceSummariesRequest :: Newtype ListComplianceSummariesRequest _
derive instance repGenericListComplianceSummariesRequest :: Generic ListComplianceSummariesRequest _
instance showListComplianceSummariesRequest :: Show ListComplianceSummariesRequest where
  show = genericShow
instance decodeListComplianceSummariesRequest :: Decode ListComplianceSummariesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListComplianceSummariesRequest :: Encode ListComplianceSummariesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListComplianceSummariesResult = ListComplianceSummariesResult 
  { "ComplianceSummaryItems" :: NullOrUndefined.NullOrUndefined (ComplianceSummaryItemList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListComplianceSummariesResult :: Newtype ListComplianceSummariesResult _
derive instance repGenericListComplianceSummariesResult :: Generic ListComplianceSummariesResult _
instance showListComplianceSummariesResult :: Show ListComplianceSummariesResult where
  show = genericShow
instance decodeListComplianceSummariesResult :: Decode ListComplianceSummariesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListComplianceSummariesResult :: Encode ListComplianceSummariesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDocumentVersionsRequest = ListDocumentVersionsRequest 
  { "Name" :: (DocumentName)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDocumentVersionsRequest :: Newtype ListDocumentVersionsRequest _
derive instance repGenericListDocumentVersionsRequest :: Generic ListDocumentVersionsRequest _
instance showListDocumentVersionsRequest :: Show ListDocumentVersionsRequest where
  show = genericShow
instance decodeListDocumentVersionsRequest :: Decode ListDocumentVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDocumentVersionsRequest :: Encode ListDocumentVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDocumentVersionsResult = ListDocumentVersionsResult 
  { "DocumentVersions" :: NullOrUndefined.NullOrUndefined (DocumentVersionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDocumentVersionsResult :: Newtype ListDocumentVersionsResult _
derive instance repGenericListDocumentVersionsResult :: Generic ListDocumentVersionsResult _
instance showListDocumentVersionsResult :: Show ListDocumentVersionsResult where
  show = genericShow
instance decodeListDocumentVersionsResult :: Decode ListDocumentVersionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDocumentVersionsResult :: Encode ListDocumentVersionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDocumentsRequest = ListDocumentsRequest 
  { "DocumentFilterList" :: NullOrUndefined.NullOrUndefined (DocumentFilterList)
  , "Filters" :: NullOrUndefined.NullOrUndefined (DocumentKeyValuesFilterList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDocumentsRequest :: Newtype ListDocumentsRequest _
derive instance repGenericListDocumentsRequest :: Generic ListDocumentsRequest _
instance showListDocumentsRequest :: Show ListDocumentsRequest where
  show = genericShow
instance decodeListDocumentsRequest :: Decode ListDocumentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDocumentsRequest :: Encode ListDocumentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDocumentsResult = ListDocumentsResult 
  { "DocumentIdentifiers" :: NullOrUndefined.NullOrUndefined (DocumentIdentifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDocumentsResult :: Newtype ListDocumentsResult _
derive instance repGenericListDocumentsResult :: Generic ListDocumentsResult _
instance showListDocumentsResult :: Show ListDocumentsResult where
  show = genericShow
instance decodeListDocumentsResult :: Decode ListDocumentsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDocumentsResult :: Encode ListDocumentsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListInventoryEntriesRequest = ListInventoryEntriesRequest 
  { "InstanceId" :: (InstanceId)
  , "TypeName" :: (InventoryItemTypeName)
  , "Filters" :: NullOrUndefined.NullOrUndefined (InventoryFilterList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListInventoryEntriesRequest :: Newtype ListInventoryEntriesRequest _
derive instance repGenericListInventoryEntriesRequest :: Generic ListInventoryEntriesRequest _
instance showListInventoryEntriesRequest :: Show ListInventoryEntriesRequest where
  show = genericShow
instance decodeListInventoryEntriesRequest :: Decode ListInventoryEntriesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInventoryEntriesRequest :: Encode ListInventoryEntriesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListInventoryEntriesResult = ListInventoryEntriesResult 
  { "TypeName" :: NullOrUndefined.NullOrUndefined (InventoryItemTypeName)
  , "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "SchemaVersion" :: NullOrUndefined.NullOrUndefined (InventoryItemSchemaVersion)
  , "CaptureTime" :: NullOrUndefined.NullOrUndefined (InventoryItemCaptureTime)
  , "Entries" :: NullOrUndefined.NullOrUndefined (InventoryItemEntryList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListInventoryEntriesResult :: Newtype ListInventoryEntriesResult _
derive instance repGenericListInventoryEntriesResult :: Generic ListInventoryEntriesResult _
instance showListInventoryEntriesResult :: Show ListInventoryEntriesResult where
  show = genericShow
instance decodeListInventoryEntriesResult :: Decode ListInventoryEntriesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInventoryEntriesResult :: Encode ListInventoryEntriesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceComplianceSummariesRequest = ListResourceComplianceSummariesRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (ComplianceStringFilterList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListResourceComplianceSummariesRequest :: Newtype ListResourceComplianceSummariesRequest _
derive instance repGenericListResourceComplianceSummariesRequest :: Generic ListResourceComplianceSummariesRequest _
instance showListResourceComplianceSummariesRequest :: Show ListResourceComplianceSummariesRequest where
  show = genericShow
instance decodeListResourceComplianceSummariesRequest :: Decode ListResourceComplianceSummariesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceComplianceSummariesRequest :: Encode ListResourceComplianceSummariesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceComplianceSummariesResult = ListResourceComplianceSummariesResult 
  { "ResourceComplianceSummaryItems" :: NullOrUndefined.NullOrUndefined (ResourceComplianceSummaryItemList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListResourceComplianceSummariesResult :: Newtype ListResourceComplianceSummariesResult _
derive instance repGenericListResourceComplianceSummariesResult :: Generic ListResourceComplianceSummariesResult _
instance showListResourceComplianceSummariesResult :: Show ListResourceComplianceSummariesResult where
  show = genericShow
instance decodeListResourceComplianceSummariesResult :: Decode ListResourceComplianceSummariesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceComplianceSummariesResult :: Encode ListResourceComplianceSummariesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceDataSyncRequest = ListResourceDataSyncRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListResourceDataSyncRequest :: Newtype ListResourceDataSyncRequest _
derive instance repGenericListResourceDataSyncRequest :: Generic ListResourceDataSyncRequest _
instance showListResourceDataSyncRequest :: Show ListResourceDataSyncRequest where
  show = genericShow
instance decodeListResourceDataSyncRequest :: Decode ListResourceDataSyncRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceDataSyncRequest :: Encode ListResourceDataSyncRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListResourceDataSyncResult = ListResourceDataSyncResult 
  { "ResourceDataSyncItems" :: NullOrUndefined.NullOrUndefined (ResourceDataSyncItemList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListResourceDataSyncResult :: Newtype ListResourceDataSyncResult _
derive instance repGenericListResourceDataSyncResult :: Generic ListResourceDataSyncResult _
instance showListResourceDataSyncResult :: Show ListResourceDataSyncResult where
  show = genericShow
instance decodeListResourceDataSyncResult :: Decode ListResourceDataSyncResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceDataSyncResult :: Encode ListResourceDataSyncResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceType" :: (ResourceTypeForTagging)
  , "ResourceId" :: (ResourceId)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _
derive instance repGenericListTagsForResourceRequest :: Generic ListTagsForResourceRequest _
instance showListTagsForResourceRequest :: Show ListTagsForResourceRequest where
  show = genericShow
instance decodeListTagsForResourceRequest :: Decode ListTagsForResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceRequest :: Encode ListTagsForResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceResult = ListTagsForResourceResult 
  { "TagList" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeListTagsForResourceResult :: Newtype ListTagsForResourceResult _
derive instance repGenericListTagsForResourceResult :: Generic ListTagsForResourceResult _
instance showListTagsForResourceResult :: Show ListTagsForResourceResult where
  show = genericShow
instance decodeListTagsForResourceResult :: Decode ListTagsForResourceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceResult :: Encode ListTagsForResourceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an Amazon S3 bucket to write instance-level logs to.</p>
newtype LoggingInfo = LoggingInfo 
  { "S3BucketName" :: (S3BucketName)
  , "S3KeyPrefix" :: NullOrUndefined.NullOrUndefined (S3KeyPrefix)
  , "S3Region" :: (S3Region)
  }
derive instance newtypeLoggingInfo :: Newtype LoggingInfo _
derive instance repGenericLoggingInfo :: Generic LoggingInfo _
instance showLoggingInfo :: Show LoggingInfo where
  show = genericShow
instance decodeLoggingInfo :: Decode LoggingInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoggingInfo :: Encode LoggingInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowAllowUnassociatedTargets = MaintenanceWindowAllowUnassociatedTargets Boolean
derive instance newtypeMaintenanceWindowAllowUnassociatedTargets :: Newtype MaintenanceWindowAllowUnassociatedTargets _
derive instance repGenericMaintenanceWindowAllowUnassociatedTargets :: Generic MaintenanceWindowAllowUnassociatedTargets _
instance showMaintenanceWindowAllowUnassociatedTargets :: Show MaintenanceWindowAllowUnassociatedTargets where
  show = genericShow
instance decodeMaintenanceWindowAllowUnassociatedTargets :: Decode MaintenanceWindowAllowUnassociatedTargets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowAllowUnassociatedTargets :: Encode MaintenanceWindowAllowUnassociatedTargets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameters for an AUTOMATION task type.</p>
newtype MaintenanceWindowAutomationParameters = MaintenanceWindowAutomationParameters 
  { "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (AutomationParameterMap)
  }
derive instance newtypeMaintenanceWindowAutomationParameters :: Newtype MaintenanceWindowAutomationParameters _
derive instance repGenericMaintenanceWindowAutomationParameters :: Generic MaintenanceWindowAutomationParameters _
instance showMaintenanceWindowAutomationParameters :: Show MaintenanceWindowAutomationParameters where
  show = genericShow
instance decodeMaintenanceWindowAutomationParameters :: Decode MaintenanceWindowAutomationParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowAutomationParameters :: Encode MaintenanceWindowAutomationParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowCutoff = MaintenanceWindowCutoff Int
derive instance newtypeMaintenanceWindowCutoff :: Newtype MaintenanceWindowCutoff _
derive instance repGenericMaintenanceWindowCutoff :: Generic MaintenanceWindowCutoff _
instance showMaintenanceWindowCutoff :: Show MaintenanceWindowCutoff where
  show = genericShow
instance decodeMaintenanceWindowCutoff :: Decode MaintenanceWindowCutoff where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowCutoff :: Encode MaintenanceWindowCutoff where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowDescription = MaintenanceWindowDescription String
derive instance newtypeMaintenanceWindowDescription :: Newtype MaintenanceWindowDescription _
derive instance repGenericMaintenanceWindowDescription :: Generic MaintenanceWindowDescription _
instance showMaintenanceWindowDescription :: Show MaintenanceWindowDescription where
  show = genericShow
instance decodeMaintenanceWindowDescription :: Decode MaintenanceWindowDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowDescription :: Encode MaintenanceWindowDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowDurationHours = MaintenanceWindowDurationHours Int
derive instance newtypeMaintenanceWindowDurationHours :: Newtype MaintenanceWindowDurationHours _
derive instance repGenericMaintenanceWindowDurationHours :: Generic MaintenanceWindowDurationHours _
instance showMaintenanceWindowDurationHours :: Show MaintenanceWindowDurationHours where
  show = genericShow
instance decodeMaintenanceWindowDurationHours :: Decode MaintenanceWindowDurationHours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowDurationHours :: Encode MaintenanceWindowDurationHours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowEnabled = MaintenanceWindowEnabled Boolean
derive instance newtypeMaintenanceWindowEnabled :: Newtype MaintenanceWindowEnabled _
derive instance repGenericMaintenanceWindowEnabled :: Generic MaintenanceWindowEnabled _
instance showMaintenanceWindowEnabled :: Show MaintenanceWindowEnabled where
  show = genericShow
instance decodeMaintenanceWindowEnabled :: Decode MaintenanceWindowEnabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowEnabled :: Encode MaintenanceWindowEnabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the information about an execution of a Maintenance Window. </p>
newtype MaintenanceWindowExecution = MaintenanceWindowExecution 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "WindowExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionId)
  , "Status" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  }
derive instance newtypeMaintenanceWindowExecution :: Newtype MaintenanceWindowExecution _
derive instance repGenericMaintenanceWindowExecution :: Generic MaintenanceWindowExecution _
instance showMaintenanceWindowExecution :: Show MaintenanceWindowExecution where
  show = genericShow
instance decodeMaintenanceWindowExecution :: Decode MaintenanceWindowExecution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecution :: Encode MaintenanceWindowExecution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionId = MaintenanceWindowExecutionId String
derive instance newtypeMaintenanceWindowExecutionId :: Newtype MaintenanceWindowExecutionId _
derive instance repGenericMaintenanceWindowExecutionId :: Generic MaintenanceWindowExecutionId _
instance showMaintenanceWindowExecutionId :: Show MaintenanceWindowExecutionId where
  show = genericShow
instance decodeMaintenanceWindowExecutionId :: Decode MaintenanceWindowExecutionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionId :: Encode MaintenanceWindowExecutionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionList = MaintenanceWindowExecutionList (Array MaintenanceWindowExecution)
derive instance newtypeMaintenanceWindowExecutionList :: Newtype MaintenanceWindowExecutionList _
derive instance repGenericMaintenanceWindowExecutionList :: Generic MaintenanceWindowExecutionList _
instance showMaintenanceWindowExecutionList :: Show MaintenanceWindowExecutionList where
  show = genericShow
instance decodeMaintenanceWindowExecutionList :: Decode MaintenanceWindowExecutionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionList :: Encode MaintenanceWindowExecutionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionStatus = MaintenanceWindowExecutionStatus String
derive instance newtypeMaintenanceWindowExecutionStatus :: Newtype MaintenanceWindowExecutionStatus _
derive instance repGenericMaintenanceWindowExecutionStatus :: Generic MaintenanceWindowExecutionStatus _
instance showMaintenanceWindowExecutionStatus :: Show MaintenanceWindowExecutionStatus where
  show = genericShow
instance decodeMaintenanceWindowExecutionStatus :: Decode MaintenanceWindowExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionStatus :: Encode MaintenanceWindowExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionStatusDetails = MaintenanceWindowExecutionStatusDetails String
derive instance newtypeMaintenanceWindowExecutionStatusDetails :: Newtype MaintenanceWindowExecutionStatusDetails _
derive instance repGenericMaintenanceWindowExecutionStatusDetails :: Generic MaintenanceWindowExecutionStatusDetails _
instance showMaintenanceWindowExecutionStatusDetails :: Show MaintenanceWindowExecutionStatusDetails where
  show = genericShow
instance decodeMaintenanceWindowExecutionStatusDetails :: Decode MaintenanceWindowExecutionStatusDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionStatusDetails :: Encode MaintenanceWindowExecutionStatusDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionTaskExecutionId = MaintenanceWindowExecutionTaskExecutionId String
derive instance newtypeMaintenanceWindowExecutionTaskExecutionId :: Newtype MaintenanceWindowExecutionTaskExecutionId _
derive instance repGenericMaintenanceWindowExecutionTaskExecutionId :: Generic MaintenanceWindowExecutionTaskExecutionId _
instance showMaintenanceWindowExecutionTaskExecutionId :: Show MaintenanceWindowExecutionTaskExecutionId where
  show = genericShow
instance decodeMaintenanceWindowExecutionTaskExecutionId :: Decode MaintenanceWindowExecutionTaskExecutionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionTaskExecutionId :: Encode MaintenanceWindowExecutionTaskExecutionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionTaskId = MaintenanceWindowExecutionTaskId String
derive instance newtypeMaintenanceWindowExecutionTaskId :: Newtype MaintenanceWindowExecutionTaskId _
derive instance repGenericMaintenanceWindowExecutionTaskId :: Generic MaintenanceWindowExecutionTaskId _
instance showMaintenanceWindowExecutionTaskId :: Show MaintenanceWindowExecutionTaskId where
  show = genericShow
instance decodeMaintenanceWindowExecutionTaskId :: Decode MaintenanceWindowExecutionTaskId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionTaskId :: Encode MaintenanceWindowExecutionTaskId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionTaskIdList = MaintenanceWindowExecutionTaskIdList (Array MaintenanceWindowExecutionTaskId)
derive instance newtypeMaintenanceWindowExecutionTaskIdList :: Newtype MaintenanceWindowExecutionTaskIdList _
derive instance repGenericMaintenanceWindowExecutionTaskIdList :: Generic MaintenanceWindowExecutionTaskIdList _
instance showMaintenanceWindowExecutionTaskIdList :: Show MaintenanceWindowExecutionTaskIdList where
  show = genericShow
instance decodeMaintenanceWindowExecutionTaskIdList :: Decode MaintenanceWindowExecutionTaskIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionTaskIdList :: Encode MaintenanceWindowExecutionTaskIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a task execution performed as part of a Maintenance Window execution.</p>
newtype MaintenanceWindowExecutionTaskIdentity = MaintenanceWindowExecutionTaskIdentity 
  { "WindowExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskId)
  , "Status" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "TaskArn" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskArn)
  , "TaskType" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskType)
  }
derive instance newtypeMaintenanceWindowExecutionTaskIdentity :: Newtype MaintenanceWindowExecutionTaskIdentity _
derive instance repGenericMaintenanceWindowExecutionTaskIdentity :: Generic MaintenanceWindowExecutionTaskIdentity _
instance showMaintenanceWindowExecutionTaskIdentity :: Show MaintenanceWindowExecutionTaskIdentity where
  show = genericShow
instance decodeMaintenanceWindowExecutionTaskIdentity :: Decode MaintenanceWindowExecutionTaskIdentity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionTaskIdentity :: Encode MaintenanceWindowExecutionTaskIdentity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionTaskIdentityList = MaintenanceWindowExecutionTaskIdentityList (Array MaintenanceWindowExecutionTaskIdentity)
derive instance newtypeMaintenanceWindowExecutionTaskIdentityList :: Newtype MaintenanceWindowExecutionTaskIdentityList _
derive instance repGenericMaintenanceWindowExecutionTaskIdentityList :: Generic MaintenanceWindowExecutionTaskIdentityList _
instance showMaintenanceWindowExecutionTaskIdentityList :: Show MaintenanceWindowExecutionTaskIdentityList where
  show = genericShow
instance decodeMaintenanceWindowExecutionTaskIdentityList :: Decode MaintenanceWindowExecutionTaskIdentityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionTaskIdentityList :: Encode MaintenanceWindowExecutionTaskIdentityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionTaskInvocationId = MaintenanceWindowExecutionTaskInvocationId String
derive instance newtypeMaintenanceWindowExecutionTaskInvocationId :: Newtype MaintenanceWindowExecutionTaskInvocationId _
derive instance repGenericMaintenanceWindowExecutionTaskInvocationId :: Generic MaintenanceWindowExecutionTaskInvocationId _
instance showMaintenanceWindowExecutionTaskInvocationId :: Show MaintenanceWindowExecutionTaskInvocationId where
  show = genericShow
instance decodeMaintenanceWindowExecutionTaskInvocationId :: Decode MaintenanceWindowExecutionTaskInvocationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionTaskInvocationId :: Encode MaintenanceWindowExecutionTaskInvocationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the information about a task invocation for a particular target as part of a task execution performed as part of a Maintenance Window execution.</p>
newtype MaintenanceWindowExecutionTaskInvocationIdentity = MaintenanceWindowExecutionTaskInvocationIdentity 
  { "WindowExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionId)
  , "TaskExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskId)
  , "InvocationId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskInvocationId)
  , "ExecutionId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskExecutionId)
  , "TaskType" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskType)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionTaskInvocationParameters)
  , "Status" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatus)
  , "StatusDetails" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowExecutionStatusDetails)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "OwnerInformation" :: NullOrUndefined.NullOrUndefined (OwnerInformation)
  , "WindowTargetId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskTargetId)
  }
derive instance newtypeMaintenanceWindowExecutionTaskInvocationIdentity :: Newtype MaintenanceWindowExecutionTaskInvocationIdentity _
derive instance repGenericMaintenanceWindowExecutionTaskInvocationIdentity :: Generic MaintenanceWindowExecutionTaskInvocationIdentity _
instance showMaintenanceWindowExecutionTaskInvocationIdentity :: Show MaintenanceWindowExecutionTaskInvocationIdentity where
  show = genericShow
instance decodeMaintenanceWindowExecutionTaskInvocationIdentity :: Decode MaintenanceWindowExecutionTaskInvocationIdentity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionTaskInvocationIdentity :: Encode MaintenanceWindowExecutionTaskInvocationIdentity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionTaskInvocationIdentityList = MaintenanceWindowExecutionTaskInvocationIdentityList (Array MaintenanceWindowExecutionTaskInvocationIdentity)
derive instance newtypeMaintenanceWindowExecutionTaskInvocationIdentityList :: Newtype MaintenanceWindowExecutionTaskInvocationIdentityList _
derive instance repGenericMaintenanceWindowExecutionTaskInvocationIdentityList :: Generic MaintenanceWindowExecutionTaskInvocationIdentityList _
instance showMaintenanceWindowExecutionTaskInvocationIdentityList :: Show MaintenanceWindowExecutionTaskInvocationIdentityList where
  show = genericShow
instance decodeMaintenanceWindowExecutionTaskInvocationIdentityList :: Decode MaintenanceWindowExecutionTaskInvocationIdentityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionTaskInvocationIdentityList :: Encode MaintenanceWindowExecutionTaskInvocationIdentityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowExecutionTaskInvocationParameters = MaintenanceWindowExecutionTaskInvocationParameters String
derive instance newtypeMaintenanceWindowExecutionTaskInvocationParameters :: Newtype MaintenanceWindowExecutionTaskInvocationParameters _
derive instance repGenericMaintenanceWindowExecutionTaskInvocationParameters :: Generic MaintenanceWindowExecutionTaskInvocationParameters _
instance showMaintenanceWindowExecutionTaskInvocationParameters :: Show MaintenanceWindowExecutionTaskInvocationParameters where
  show = genericShow
instance decodeMaintenanceWindowExecutionTaskInvocationParameters :: Decode MaintenanceWindowExecutionTaskInvocationParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowExecutionTaskInvocationParameters :: Encode MaintenanceWindowExecutionTaskInvocationParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Filter used in the request.</p>
newtype MaintenanceWindowFilter = MaintenanceWindowFilter 
  { "Key" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowFilterKey)
  , "Values" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowFilterValues)
  }
derive instance newtypeMaintenanceWindowFilter :: Newtype MaintenanceWindowFilter _
derive instance repGenericMaintenanceWindowFilter :: Generic MaintenanceWindowFilter _
instance showMaintenanceWindowFilter :: Show MaintenanceWindowFilter where
  show = genericShow
instance decodeMaintenanceWindowFilter :: Decode MaintenanceWindowFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowFilter :: Encode MaintenanceWindowFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowFilterKey = MaintenanceWindowFilterKey String
derive instance newtypeMaintenanceWindowFilterKey :: Newtype MaintenanceWindowFilterKey _
derive instance repGenericMaintenanceWindowFilterKey :: Generic MaintenanceWindowFilterKey _
instance showMaintenanceWindowFilterKey :: Show MaintenanceWindowFilterKey where
  show = genericShow
instance decodeMaintenanceWindowFilterKey :: Decode MaintenanceWindowFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowFilterKey :: Encode MaintenanceWindowFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowFilterList = MaintenanceWindowFilterList (Array MaintenanceWindowFilter)
derive instance newtypeMaintenanceWindowFilterList :: Newtype MaintenanceWindowFilterList _
derive instance repGenericMaintenanceWindowFilterList :: Generic MaintenanceWindowFilterList _
instance showMaintenanceWindowFilterList :: Show MaintenanceWindowFilterList where
  show = genericShow
instance decodeMaintenanceWindowFilterList :: Decode MaintenanceWindowFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowFilterList :: Encode MaintenanceWindowFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowFilterValue = MaintenanceWindowFilterValue String
derive instance newtypeMaintenanceWindowFilterValue :: Newtype MaintenanceWindowFilterValue _
derive instance repGenericMaintenanceWindowFilterValue :: Generic MaintenanceWindowFilterValue _
instance showMaintenanceWindowFilterValue :: Show MaintenanceWindowFilterValue where
  show = genericShow
instance decodeMaintenanceWindowFilterValue :: Decode MaintenanceWindowFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowFilterValue :: Encode MaintenanceWindowFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowFilterValues = MaintenanceWindowFilterValues (Array MaintenanceWindowFilterValue)
derive instance newtypeMaintenanceWindowFilterValues :: Newtype MaintenanceWindowFilterValues _
derive instance repGenericMaintenanceWindowFilterValues :: Generic MaintenanceWindowFilterValues _
instance showMaintenanceWindowFilterValues :: Show MaintenanceWindowFilterValues where
  show = genericShow
instance decodeMaintenanceWindowFilterValues :: Decode MaintenanceWindowFilterValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowFilterValues :: Encode MaintenanceWindowFilterValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowId = MaintenanceWindowId String
derive instance newtypeMaintenanceWindowId :: Newtype MaintenanceWindowId _
derive instance repGenericMaintenanceWindowId :: Generic MaintenanceWindowId _
instance showMaintenanceWindowId :: Show MaintenanceWindowId where
  show = genericShow
instance decodeMaintenanceWindowId :: Decode MaintenanceWindowId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowId :: Encode MaintenanceWindowId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the Maintenance Window.</p>
newtype MaintenanceWindowIdentity = MaintenanceWindowIdentity 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowEnabled)
  , "Duration" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDurationHours)
  , "Cutoff" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowCutoff)
  }
derive instance newtypeMaintenanceWindowIdentity :: Newtype MaintenanceWindowIdentity _
derive instance repGenericMaintenanceWindowIdentity :: Generic MaintenanceWindowIdentity _
instance showMaintenanceWindowIdentity :: Show MaintenanceWindowIdentity where
  show = genericShow
instance decodeMaintenanceWindowIdentity :: Decode MaintenanceWindowIdentity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowIdentity :: Encode MaintenanceWindowIdentity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowIdentityList = MaintenanceWindowIdentityList (Array MaintenanceWindowIdentity)
derive instance newtypeMaintenanceWindowIdentityList :: Newtype MaintenanceWindowIdentityList _
derive instance repGenericMaintenanceWindowIdentityList :: Generic MaintenanceWindowIdentityList _
instance showMaintenanceWindowIdentityList :: Show MaintenanceWindowIdentityList where
  show = genericShow
instance decodeMaintenanceWindowIdentityList :: Decode MaintenanceWindowIdentityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowIdentityList :: Encode MaintenanceWindowIdentityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowLambdaClientContext = MaintenanceWindowLambdaClientContext String
derive instance newtypeMaintenanceWindowLambdaClientContext :: Newtype MaintenanceWindowLambdaClientContext _
derive instance repGenericMaintenanceWindowLambdaClientContext :: Generic MaintenanceWindowLambdaClientContext _
instance showMaintenanceWindowLambdaClientContext :: Show MaintenanceWindowLambdaClientContext where
  show = genericShow
instance decodeMaintenanceWindowLambdaClientContext :: Decode MaintenanceWindowLambdaClientContext where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowLambdaClientContext :: Encode MaintenanceWindowLambdaClientContext where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameters for a LAMBDA task type.</p>
newtype MaintenanceWindowLambdaParameters = MaintenanceWindowLambdaParameters 
  { "ClientContext" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowLambdaClientContext)
  , "Qualifier" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowLambdaQualifier)
  , "Payload" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowLambdaPayload)
  }
derive instance newtypeMaintenanceWindowLambdaParameters :: Newtype MaintenanceWindowLambdaParameters _
derive instance repGenericMaintenanceWindowLambdaParameters :: Generic MaintenanceWindowLambdaParameters _
instance showMaintenanceWindowLambdaParameters :: Show MaintenanceWindowLambdaParameters where
  show = genericShow
instance decodeMaintenanceWindowLambdaParameters :: Decode MaintenanceWindowLambdaParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowLambdaParameters :: Encode MaintenanceWindowLambdaParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowLambdaPayload = MaintenanceWindowLambdaPayload String
derive instance newtypeMaintenanceWindowLambdaPayload :: Newtype MaintenanceWindowLambdaPayload _
derive instance repGenericMaintenanceWindowLambdaPayload :: Generic MaintenanceWindowLambdaPayload _
instance showMaintenanceWindowLambdaPayload :: Show MaintenanceWindowLambdaPayload where
  show = genericShow
instance decodeMaintenanceWindowLambdaPayload :: Decode MaintenanceWindowLambdaPayload where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowLambdaPayload :: Encode MaintenanceWindowLambdaPayload where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowLambdaQualifier = MaintenanceWindowLambdaQualifier String
derive instance newtypeMaintenanceWindowLambdaQualifier :: Newtype MaintenanceWindowLambdaQualifier _
derive instance repGenericMaintenanceWindowLambdaQualifier :: Generic MaintenanceWindowLambdaQualifier _
instance showMaintenanceWindowLambdaQualifier :: Show MaintenanceWindowLambdaQualifier where
  show = genericShow
instance decodeMaintenanceWindowLambdaQualifier :: Decode MaintenanceWindowLambdaQualifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowLambdaQualifier :: Encode MaintenanceWindowLambdaQualifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowMaxResults = MaintenanceWindowMaxResults Int
derive instance newtypeMaintenanceWindowMaxResults :: Newtype MaintenanceWindowMaxResults _
derive instance repGenericMaintenanceWindowMaxResults :: Generic MaintenanceWindowMaxResults _
instance showMaintenanceWindowMaxResults :: Show MaintenanceWindowMaxResults where
  show = genericShow
instance decodeMaintenanceWindowMaxResults :: Decode MaintenanceWindowMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowMaxResults :: Encode MaintenanceWindowMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowName = MaintenanceWindowName String
derive instance newtypeMaintenanceWindowName :: Newtype MaintenanceWindowName _
derive instance repGenericMaintenanceWindowName :: Generic MaintenanceWindowName _
instance showMaintenanceWindowName :: Show MaintenanceWindowName where
  show = genericShow
instance decodeMaintenanceWindowName :: Decode MaintenanceWindowName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowName :: Encode MaintenanceWindowName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowResourceType = MaintenanceWindowResourceType String
derive instance newtypeMaintenanceWindowResourceType :: Newtype MaintenanceWindowResourceType _
derive instance repGenericMaintenanceWindowResourceType :: Generic MaintenanceWindowResourceType _
instance showMaintenanceWindowResourceType :: Show MaintenanceWindowResourceType where
  show = genericShow
instance decodeMaintenanceWindowResourceType :: Decode MaintenanceWindowResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowResourceType :: Encode MaintenanceWindowResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameters for a RUN_COMMAND task type.</p>
newtype MaintenanceWindowRunCommandParameters = MaintenanceWindowRunCommandParameters 
  { "Comment" :: NullOrUndefined.NullOrUndefined (Comment)
  , "DocumentHash" :: NullOrUndefined.NullOrUndefined (DocumentHash)
  , "DocumentHashType" :: NullOrUndefined.NullOrUndefined (DocumentHashType)
  , "NotificationConfig" :: NullOrUndefined.NullOrUndefined (NotificationConfig)
  , "OutputS3BucketName" :: NullOrUndefined.NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined.NullOrUndefined (S3KeyPrefix)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "ServiceRoleArn" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "TimeoutSeconds" :: NullOrUndefined.NullOrUndefined (TimeoutSeconds)
  }
derive instance newtypeMaintenanceWindowRunCommandParameters :: Newtype MaintenanceWindowRunCommandParameters _
derive instance repGenericMaintenanceWindowRunCommandParameters :: Generic MaintenanceWindowRunCommandParameters _
instance showMaintenanceWindowRunCommandParameters :: Show MaintenanceWindowRunCommandParameters where
  show = genericShow
instance decodeMaintenanceWindowRunCommandParameters :: Decode MaintenanceWindowRunCommandParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowRunCommandParameters :: Encode MaintenanceWindowRunCommandParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowSchedule = MaintenanceWindowSchedule String
derive instance newtypeMaintenanceWindowSchedule :: Newtype MaintenanceWindowSchedule _
derive instance repGenericMaintenanceWindowSchedule :: Generic MaintenanceWindowSchedule _
instance showMaintenanceWindowSchedule :: Show MaintenanceWindowSchedule where
  show = genericShow
instance decodeMaintenanceWindowSchedule :: Decode MaintenanceWindowSchedule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowSchedule :: Encode MaintenanceWindowSchedule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowStepFunctionsInput = MaintenanceWindowStepFunctionsInput String
derive instance newtypeMaintenanceWindowStepFunctionsInput :: Newtype MaintenanceWindowStepFunctionsInput _
derive instance repGenericMaintenanceWindowStepFunctionsInput :: Generic MaintenanceWindowStepFunctionsInput _
instance showMaintenanceWindowStepFunctionsInput :: Show MaintenanceWindowStepFunctionsInput where
  show = genericShow
instance decodeMaintenanceWindowStepFunctionsInput :: Decode MaintenanceWindowStepFunctionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowStepFunctionsInput :: Encode MaintenanceWindowStepFunctionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowStepFunctionsName = MaintenanceWindowStepFunctionsName String
derive instance newtypeMaintenanceWindowStepFunctionsName :: Newtype MaintenanceWindowStepFunctionsName _
derive instance repGenericMaintenanceWindowStepFunctionsName :: Generic MaintenanceWindowStepFunctionsName _
instance showMaintenanceWindowStepFunctionsName :: Show MaintenanceWindowStepFunctionsName where
  show = genericShow
instance decodeMaintenanceWindowStepFunctionsName :: Decode MaintenanceWindowStepFunctionsName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowStepFunctionsName :: Encode MaintenanceWindowStepFunctionsName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameters for the STEP_FUNCTION execution.</p>
newtype MaintenanceWindowStepFunctionsParameters = MaintenanceWindowStepFunctionsParameters 
  { "Input" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowStepFunctionsInput)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowStepFunctionsName)
  }
derive instance newtypeMaintenanceWindowStepFunctionsParameters :: Newtype MaintenanceWindowStepFunctionsParameters _
derive instance repGenericMaintenanceWindowStepFunctionsParameters :: Generic MaintenanceWindowStepFunctionsParameters _
instance showMaintenanceWindowStepFunctionsParameters :: Show MaintenanceWindowStepFunctionsParameters where
  show = genericShow
instance decodeMaintenanceWindowStepFunctionsParameters :: Decode MaintenanceWindowStepFunctionsParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowStepFunctionsParameters :: Encode MaintenanceWindowStepFunctionsParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The target registered with the Maintenance Window.</p>
newtype MaintenanceWindowTarget = MaintenanceWindowTarget 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "WindowTargetId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTargetId)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowResourceType)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "OwnerInformation" :: NullOrUndefined.NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  }
derive instance newtypeMaintenanceWindowTarget :: Newtype MaintenanceWindowTarget _
derive instance repGenericMaintenanceWindowTarget :: Generic MaintenanceWindowTarget _
instance showMaintenanceWindowTarget :: Show MaintenanceWindowTarget where
  show = genericShow
instance decodeMaintenanceWindowTarget :: Decode MaintenanceWindowTarget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTarget :: Encode MaintenanceWindowTarget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTargetId = MaintenanceWindowTargetId String
derive instance newtypeMaintenanceWindowTargetId :: Newtype MaintenanceWindowTargetId _
derive instance repGenericMaintenanceWindowTargetId :: Generic MaintenanceWindowTargetId _
instance showMaintenanceWindowTargetId :: Show MaintenanceWindowTargetId where
  show = genericShow
instance decodeMaintenanceWindowTargetId :: Decode MaintenanceWindowTargetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTargetId :: Encode MaintenanceWindowTargetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTargetList = MaintenanceWindowTargetList (Array MaintenanceWindowTarget)
derive instance newtypeMaintenanceWindowTargetList :: Newtype MaintenanceWindowTargetList _
derive instance repGenericMaintenanceWindowTargetList :: Generic MaintenanceWindowTargetList _
instance showMaintenanceWindowTargetList :: Show MaintenanceWindowTargetList where
  show = genericShow
instance decodeMaintenanceWindowTargetList :: Decode MaintenanceWindowTargetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTargetList :: Encode MaintenanceWindowTargetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a task defined for a Maintenance Window.</p>
newtype MaintenanceWindowTask = MaintenanceWindowTask 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "WindowTaskId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskId)
  , "TaskArn" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskArn)
  , "Type" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskType)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "TaskParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskParameters)
  , "Priority" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskPriority)
  , "LoggingInfo" :: NullOrUndefined.NullOrUndefined (LoggingInfo)
  , "ServiceRoleArn" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  }
derive instance newtypeMaintenanceWindowTask :: Newtype MaintenanceWindowTask _
derive instance repGenericMaintenanceWindowTask :: Generic MaintenanceWindowTask _
instance showMaintenanceWindowTask :: Show MaintenanceWindowTask where
  show = genericShow
instance decodeMaintenanceWindowTask :: Decode MaintenanceWindowTask where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTask :: Encode MaintenanceWindowTask where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskArn = MaintenanceWindowTaskArn String
derive instance newtypeMaintenanceWindowTaskArn :: Newtype MaintenanceWindowTaskArn _
derive instance repGenericMaintenanceWindowTaskArn :: Generic MaintenanceWindowTaskArn _
instance showMaintenanceWindowTaskArn :: Show MaintenanceWindowTaskArn where
  show = genericShow
instance decodeMaintenanceWindowTaskArn :: Decode MaintenanceWindowTaskArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskArn :: Encode MaintenanceWindowTaskArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskId = MaintenanceWindowTaskId String
derive instance newtypeMaintenanceWindowTaskId :: Newtype MaintenanceWindowTaskId _
derive instance repGenericMaintenanceWindowTaskId :: Generic MaintenanceWindowTaskId _
instance showMaintenanceWindowTaskId :: Show MaintenanceWindowTaskId where
  show = genericShow
instance decodeMaintenanceWindowTaskId :: Decode MaintenanceWindowTaskId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskId :: Encode MaintenanceWindowTaskId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameters for task execution.</p>
newtype MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters 
  { "RunCommand" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowRunCommandParameters)
  , "Automation" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowAutomationParameters)
  , "StepFunctions" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowStepFunctionsParameters)
  , "Lambda" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowLambdaParameters)
  }
derive instance newtypeMaintenanceWindowTaskInvocationParameters :: Newtype MaintenanceWindowTaskInvocationParameters _
derive instance repGenericMaintenanceWindowTaskInvocationParameters :: Generic MaintenanceWindowTaskInvocationParameters _
instance showMaintenanceWindowTaskInvocationParameters :: Show MaintenanceWindowTaskInvocationParameters where
  show = genericShow
instance decodeMaintenanceWindowTaskInvocationParameters :: Decode MaintenanceWindowTaskInvocationParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskInvocationParameters :: Encode MaintenanceWindowTaskInvocationParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskList = MaintenanceWindowTaskList (Array MaintenanceWindowTask)
derive instance newtypeMaintenanceWindowTaskList :: Newtype MaintenanceWindowTaskList _
derive instance repGenericMaintenanceWindowTaskList :: Generic MaintenanceWindowTaskList _
instance showMaintenanceWindowTaskList :: Show MaintenanceWindowTaskList where
  show = genericShow
instance decodeMaintenanceWindowTaskList :: Decode MaintenanceWindowTaskList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskList :: Encode MaintenanceWindowTaskList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskParameterName = MaintenanceWindowTaskParameterName String
derive instance newtypeMaintenanceWindowTaskParameterName :: Newtype MaintenanceWindowTaskParameterName _
derive instance repGenericMaintenanceWindowTaskParameterName :: Generic MaintenanceWindowTaskParameterName _
instance showMaintenanceWindowTaskParameterName :: Show MaintenanceWindowTaskParameterName where
  show = genericShow
instance decodeMaintenanceWindowTaskParameterName :: Decode MaintenanceWindowTaskParameterName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskParameterName :: Encode MaintenanceWindowTaskParameterName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskParameterValue = MaintenanceWindowTaskParameterValue String
derive instance newtypeMaintenanceWindowTaskParameterValue :: Newtype MaintenanceWindowTaskParameterValue _
derive instance repGenericMaintenanceWindowTaskParameterValue :: Generic MaintenanceWindowTaskParameterValue _
instance showMaintenanceWindowTaskParameterValue :: Show MaintenanceWindowTaskParameterValue where
  show = genericShow
instance decodeMaintenanceWindowTaskParameterValue :: Decode MaintenanceWindowTaskParameterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskParameterValue :: Encode MaintenanceWindowTaskParameterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines the values for a task parameter.</p>
newtype MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression 
  { "Values" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskParameterValueList)
  }
derive instance newtypeMaintenanceWindowTaskParameterValueExpression :: Newtype MaintenanceWindowTaskParameterValueExpression _
derive instance repGenericMaintenanceWindowTaskParameterValueExpression :: Generic MaintenanceWindowTaskParameterValueExpression _
instance showMaintenanceWindowTaskParameterValueExpression :: Show MaintenanceWindowTaskParameterValueExpression where
  show = genericShow
instance decodeMaintenanceWindowTaskParameterValueExpression :: Decode MaintenanceWindowTaskParameterValueExpression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskParameterValueExpression :: Encode MaintenanceWindowTaskParameterValueExpression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskParameterValueList = MaintenanceWindowTaskParameterValueList (Array MaintenanceWindowTaskParameterValue)
derive instance newtypeMaintenanceWindowTaskParameterValueList :: Newtype MaintenanceWindowTaskParameterValueList _
derive instance repGenericMaintenanceWindowTaskParameterValueList :: Generic MaintenanceWindowTaskParameterValueList _
instance showMaintenanceWindowTaskParameterValueList :: Show MaintenanceWindowTaskParameterValueList where
  show = genericShow
instance decodeMaintenanceWindowTaskParameterValueList :: Decode MaintenanceWindowTaskParameterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskParameterValueList :: Encode MaintenanceWindowTaskParameterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskParameters = MaintenanceWindowTaskParameters (StrMap.StrMap MaintenanceWindowTaskParameterValueExpression)
derive instance newtypeMaintenanceWindowTaskParameters :: Newtype MaintenanceWindowTaskParameters _
derive instance repGenericMaintenanceWindowTaskParameters :: Generic MaintenanceWindowTaskParameters _
instance showMaintenanceWindowTaskParameters :: Show MaintenanceWindowTaskParameters where
  show = genericShow
instance decodeMaintenanceWindowTaskParameters :: Decode MaintenanceWindowTaskParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskParameters :: Encode MaintenanceWindowTaskParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskParametersList = MaintenanceWindowTaskParametersList (Array MaintenanceWindowTaskParameters)
derive instance newtypeMaintenanceWindowTaskParametersList :: Newtype MaintenanceWindowTaskParametersList _
derive instance repGenericMaintenanceWindowTaskParametersList :: Generic MaintenanceWindowTaskParametersList _
instance showMaintenanceWindowTaskParametersList :: Show MaintenanceWindowTaskParametersList where
  show = genericShow
instance decodeMaintenanceWindowTaskParametersList :: Decode MaintenanceWindowTaskParametersList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskParametersList :: Encode MaintenanceWindowTaskParametersList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskPriority = MaintenanceWindowTaskPriority Int
derive instance newtypeMaintenanceWindowTaskPriority :: Newtype MaintenanceWindowTaskPriority _
derive instance repGenericMaintenanceWindowTaskPriority :: Generic MaintenanceWindowTaskPriority _
instance showMaintenanceWindowTaskPriority :: Show MaintenanceWindowTaskPriority where
  show = genericShow
instance decodeMaintenanceWindowTaskPriority :: Decode MaintenanceWindowTaskPriority where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskPriority :: Encode MaintenanceWindowTaskPriority where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskTargetId = MaintenanceWindowTaskTargetId String
derive instance newtypeMaintenanceWindowTaskTargetId :: Newtype MaintenanceWindowTaskTargetId _
derive instance repGenericMaintenanceWindowTaskTargetId :: Generic MaintenanceWindowTaskTargetId _
instance showMaintenanceWindowTaskTargetId :: Show MaintenanceWindowTaskTargetId where
  show = genericShow
instance decodeMaintenanceWindowTaskTargetId :: Decode MaintenanceWindowTaskTargetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskTargetId :: Encode MaintenanceWindowTaskTargetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaintenanceWindowTaskType = MaintenanceWindowTaskType String
derive instance newtypeMaintenanceWindowTaskType :: Newtype MaintenanceWindowTaskType _
derive instance repGenericMaintenanceWindowTaskType :: Generic MaintenanceWindowTaskType _
instance showMaintenanceWindowTaskType :: Show MaintenanceWindowTaskType where
  show = genericShow
instance decodeMaintenanceWindowTaskType :: Decode MaintenanceWindowTaskType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintenanceWindowTaskType :: Encode MaintenanceWindowTaskType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ManagedInstanceId = ManagedInstanceId String
derive instance newtypeManagedInstanceId :: Newtype ManagedInstanceId _
derive instance repGenericManagedInstanceId :: Generic ManagedInstanceId _
instance showManagedInstanceId :: Show ManagedInstanceId where
  show = genericShow
instance decodeManagedInstanceId :: Decode ManagedInstanceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManagedInstanceId :: Encode ManagedInstanceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxConcurrency = MaxConcurrency String
derive instance newtypeMaxConcurrency :: Newtype MaxConcurrency _
derive instance repGenericMaxConcurrency :: Generic MaxConcurrency _
instance showMaxConcurrency :: Show MaxConcurrency where
  show = genericShow
instance decodeMaxConcurrency :: Decode MaxConcurrency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxConcurrency :: Encode MaxConcurrency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The size limit of a document is 64 KB.</p>
newtype MaxDocumentSizeExceeded = MaxDocumentSizeExceeded 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMaxDocumentSizeExceeded :: Newtype MaxDocumentSizeExceeded _
derive instance repGenericMaxDocumentSizeExceeded :: Generic MaxDocumentSizeExceeded _
instance showMaxDocumentSizeExceeded :: Show MaxDocumentSizeExceeded where
  show = genericShow
instance decodeMaxDocumentSizeExceeded :: Decode MaxDocumentSizeExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxDocumentSizeExceeded :: Encode MaxDocumentSizeExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxErrors = MaxErrors String
derive instance newtypeMaxErrors :: Newtype MaxErrors _
derive instance repGenericMaxErrors :: Generic MaxErrors _
instance showMaxErrors :: Show MaxErrors where
  show = genericShow
instance decodeMaxErrors :: Decode MaxErrors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxErrors :: Encode MaxErrors where
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


newtype MaxResultsEC2Compatible = MaxResultsEC2Compatible Int
derive instance newtypeMaxResultsEC2Compatible :: Newtype MaxResultsEC2Compatible _
derive instance repGenericMaxResultsEC2Compatible :: Generic MaxResultsEC2Compatible _
instance showMaxResultsEC2Compatible :: Show MaxResultsEC2Compatible where
  show = genericShow
instance decodeMaxResultsEC2Compatible :: Decode MaxResultsEC2Compatible where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResultsEC2Compatible :: Encode MaxResultsEC2Compatible where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyDocumentPermissionRequest = ModifyDocumentPermissionRequest 
  { "Name" :: (DocumentName)
  , "PermissionType" :: (DocumentPermissionType)
  , "AccountIdsToAdd" :: NullOrUndefined.NullOrUndefined (AccountIdList)
  , "AccountIdsToRemove" :: NullOrUndefined.NullOrUndefined (AccountIdList)
  }
derive instance newtypeModifyDocumentPermissionRequest :: Newtype ModifyDocumentPermissionRequest _
derive instance repGenericModifyDocumentPermissionRequest :: Generic ModifyDocumentPermissionRequest _
instance showModifyDocumentPermissionRequest :: Show ModifyDocumentPermissionRequest where
  show = genericShow
instance decodeModifyDocumentPermissionRequest :: Decode ModifyDocumentPermissionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyDocumentPermissionRequest :: Encode ModifyDocumentPermissionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyDocumentPermissionResponse = ModifyDocumentPermissionResponse Types.NoArguments
derive instance newtypeModifyDocumentPermissionResponse :: Newtype ModifyDocumentPermissionResponse _
derive instance repGenericModifyDocumentPermissionResponse :: Generic ModifyDocumentPermissionResponse _
instance showModifyDocumentPermissionResponse :: Show ModifyDocumentPermissionResponse where
  show = genericShow
instance decodeModifyDocumentPermissionResponse :: Decode ModifyDocumentPermissionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyDocumentPermissionResponse :: Encode ModifyDocumentPermissionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A summary of resources that are not compliant. The summary is organized according to resource type.</p>
newtype NonCompliantSummary = NonCompliantSummary 
  { "NonCompliantCount" :: NullOrUndefined.NullOrUndefined (ComplianceSummaryCount)
  , "SeveritySummary" :: NullOrUndefined.NullOrUndefined (SeveritySummary)
  }
derive instance newtypeNonCompliantSummary :: Newtype NonCompliantSummary _
derive instance repGenericNonCompliantSummary :: Generic NonCompliantSummary _
instance showNonCompliantSummary :: Show NonCompliantSummary where
  show = genericShow
instance decodeNonCompliantSummary :: Decode NonCompliantSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonCompliantSummary :: Encode NonCompliantSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NormalStringMap = NormalStringMap (StrMap.StrMap String)
derive instance newtypeNormalStringMap :: Newtype NormalStringMap _
derive instance repGenericNormalStringMap :: Generic NormalStringMap _
instance showNormalStringMap :: Show NormalStringMap where
  show = genericShow
instance decodeNormalStringMap :: Decode NormalStringMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNormalStringMap :: Encode NormalStringMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationArn = NotificationArn String
derive instance newtypeNotificationArn :: Newtype NotificationArn _
derive instance repGenericNotificationArn :: Generic NotificationArn _
instance showNotificationArn :: Show NotificationArn where
  show = genericShow
instance decodeNotificationArn :: Decode NotificationArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationArn :: Encode NotificationArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configurations for sending notifications.</p>
newtype NotificationConfig = NotificationConfig 
  { "NotificationArn" :: NullOrUndefined.NullOrUndefined (NotificationArn)
  , "NotificationEvents" :: NullOrUndefined.NullOrUndefined (NotificationEventList)
  , "NotificationType" :: NullOrUndefined.NullOrUndefined (NotificationType)
  }
derive instance newtypeNotificationConfig :: Newtype NotificationConfig _
derive instance repGenericNotificationConfig :: Generic NotificationConfig _
instance showNotificationConfig :: Show NotificationConfig where
  show = genericShow
instance decodeNotificationConfig :: Decode NotificationConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationConfig :: Encode NotificationConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationEvent = NotificationEvent String
derive instance newtypeNotificationEvent :: Newtype NotificationEvent _
derive instance repGenericNotificationEvent :: Generic NotificationEvent _
instance showNotificationEvent :: Show NotificationEvent where
  show = genericShow
instance decodeNotificationEvent :: Decode NotificationEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationEvent :: Encode NotificationEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationEventList = NotificationEventList (Array NotificationEvent)
derive instance newtypeNotificationEventList :: Newtype NotificationEventList _
derive instance repGenericNotificationEventList :: Generic NotificationEventList _
instance showNotificationEventList :: Show NotificationEventList where
  show = genericShow
instance decodeNotificationEventList :: Decode NotificationEventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationEventList :: Encode NotificationEventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationType = NotificationType String
derive instance newtypeNotificationType :: Newtype NotificationType _
derive instance repGenericNotificationType :: Generic NotificationType _
instance showNotificationType :: Show NotificationType where
  show = genericShow
instance decodeNotificationType :: Decode NotificationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationType :: Encode NotificationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OperatingSystem = OperatingSystem String
derive instance newtypeOperatingSystem :: Newtype OperatingSystem _
derive instance repGenericOperatingSystem :: Generic OperatingSystem _
instance showOperatingSystem :: Show OperatingSystem where
  show = genericShow
instance decodeOperatingSystem :: Decode OperatingSystem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperatingSystem :: Encode OperatingSystem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OwnerInformation = OwnerInformation String
derive instance newtypeOwnerInformation :: Newtype OwnerInformation _
derive instance repGenericOwnerInformation :: Generic OwnerInformation _
instance showOwnerInformation :: Show OwnerInformation where
  show = genericShow
instance decodeOwnerInformation :: Decode OwnerInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOwnerInformation :: Encode OwnerInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PSParameterName = PSParameterName String
derive instance newtypePSParameterName :: Newtype PSParameterName _
derive instance repGenericPSParameterName :: Generic PSParameterName _
instance showPSParameterName :: Show PSParameterName where
  show = genericShow
instance decodePSParameterName :: Decode PSParameterName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePSParameterName :: Encode PSParameterName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PSParameterValue = PSParameterValue String
derive instance newtypePSParameterValue :: Newtype PSParameterValue _
derive instance repGenericPSParameterValue :: Generic PSParameterValue _
instance showPSParameterValue :: Show PSParameterValue where
  show = genericShow
instance decodePSParameterValue :: Decode PSParameterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePSParameterValue :: Encode PSParameterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PSParameterVersion = PSParameterVersion Number
derive instance newtypePSParameterVersion :: Newtype PSParameterVersion _
derive instance repGenericPSParameterVersion :: Generic PSParameterVersion _
instance showPSParameterVersion :: Show PSParameterVersion where
  show = genericShow
instance decodePSParameterVersion :: Decode PSParameterVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePSParameterVersion :: Encode PSParameterVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Amazon EC2 Systems Manager parameter in Parameter Store.</p>
newtype Parameter = Parameter 
  { "Name" :: NullOrUndefined.NullOrUndefined (PSParameterName)
  , "Type" :: NullOrUndefined.NullOrUndefined (ParameterType)
  , "Value" :: NullOrUndefined.NullOrUndefined (PSParameterValue)
  , "Version" :: NullOrUndefined.NullOrUndefined (PSParameterVersion)
  }
derive instance newtypeParameter :: Newtype Parameter _
derive instance repGenericParameter :: Generic Parameter _
instance showParameter :: Show Parameter where
  show = genericShow
instance decodeParameter :: Decode Parameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameter :: Encode Parameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameter already exists. You can't create duplicate parameters.</p>
newtype ParameterAlreadyExists = ParameterAlreadyExists 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeParameterAlreadyExists :: Newtype ParameterAlreadyExists _
derive instance repGenericParameterAlreadyExists :: Generic ParameterAlreadyExists _
instance showParameterAlreadyExists :: Show ParameterAlreadyExists where
  show = genericShow
instance decodeParameterAlreadyExists :: Decode ParameterAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterAlreadyExists :: Encode ParameterAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterDescription = ParameterDescription String
derive instance newtypeParameterDescription :: Newtype ParameterDescription _
derive instance repGenericParameterDescription :: Generic ParameterDescription _
instance showParameterDescription :: Show ParameterDescription where
  show = genericShow
instance decodeParameterDescription :: Decode ParameterDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterDescription :: Encode ParameterDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about parameter usage.</p>
newtype ParameterHistory = ParameterHistory 
  { "Name" :: NullOrUndefined.NullOrUndefined (PSParameterName)
  , "Type" :: NullOrUndefined.NullOrUndefined (ParameterType)
  , "KeyId" :: NullOrUndefined.NullOrUndefined (ParameterKeyId)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "LastModifiedUser" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (ParameterDescription)
  , "Value" :: NullOrUndefined.NullOrUndefined (PSParameterValue)
  , "AllowedPattern" :: NullOrUndefined.NullOrUndefined (AllowedPattern)
  , "Version" :: NullOrUndefined.NullOrUndefined (PSParameterVersion)
  }
derive instance newtypeParameterHistory :: Newtype ParameterHistory _
derive instance repGenericParameterHistory :: Generic ParameterHistory _
instance showParameterHistory :: Show ParameterHistory where
  show = genericShow
instance decodeParameterHistory :: Decode ParameterHistory where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterHistory :: Encode ParameterHistory where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterHistoryList = ParameterHistoryList (Array ParameterHistory)
derive instance newtypeParameterHistoryList :: Newtype ParameterHistoryList _
derive instance repGenericParameterHistoryList :: Generic ParameterHistoryList _
instance showParameterHistoryList :: Show ParameterHistoryList where
  show = genericShow
instance decodeParameterHistoryList :: Decode ParameterHistoryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterHistoryList :: Encode ParameterHistoryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterKeyId = ParameterKeyId String
derive instance newtypeParameterKeyId :: Newtype ParameterKeyId _
derive instance repGenericParameterKeyId :: Generic ParameterKeyId _
instance showParameterKeyId :: Show ParameterKeyId where
  show = genericShow
instance decodeParameterKeyId :: Decode ParameterKeyId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterKeyId :: Encode ParameterKeyId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have exceeded the number of parameters for this AWS account. Delete one or more parameters and try again.</p>
newtype ParameterLimitExceeded = ParameterLimitExceeded 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeParameterLimitExceeded :: Newtype ParameterLimitExceeded _
derive instance repGenericParameterLimitExceeded :: Generic ParameterLimitExceeded _
instance showParameterLimitExceeded :: Show ParameterLimitExceeded where
  show = genericShow
instance decodeParameterLimitExceeded :: Decode ParameterLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterLimitExceeded :: Encode ParameterLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterList = ParameterList (Array Parameter)
derive instance newtypeParameterList :: Newtype ParameterList _
derive instance repGenericParameterList :: Generic ParameterList _
instance showParameterList :: Show ParameterList where
  show = genericShow
instance decodeParameterList :: Decode ParameterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterList :: Encode ParameterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameter exceeded the maximum number of allowed versions.</p>
newtype ParameterMaxVersionLimitExceeded = ParameterMaxVersionLimitExceeded 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeParameterMaxVersionLimitExceeded :: Newtype ParameterMaxVersionLimitExceeded _
derive instance repGenericParameterMaxVersionLimitExceeded :: Generic ParameterMaxVersionLimitExceeded _
instance showParameterMaxVersionLimitExceeded :: Show ParameterMaxVersionLimitExceeded where
  show = genericShow
instance decodeParameterMaxVersionLimitExceeded :: Decode ParameterMaxVersionLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterMaxVersionLimitExceeded :: Encode ParameterMaxVersionLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Metada includes information like the ARN of the last user and the date/time the parameter was last used.</p>
newtype ParameterMetadata = ParameterMetadata 
  { "Name" :: NullOrUndefined.NullOrUndefined (PSParameterName)
  , "Type" :: NullOrUndefined.NullOrUndefined (ParameterType)
  , "KeyId" :: NullOrUndefined.NullOrUndefined (ParameterKeyId)
  , "LastModifiedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "LastModifiedUser" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (ParameterDescription)
  , "AllowedPattern" :: NullOrUndefined.NullOrUndefined (AllowedPattern)
  , "Version" :: NullOrUndefined.NullOrUndefined (PSParameterVersion)
  }
derive instance newtypeParameterMetadata :: Newtype ParameterMetadata _
derive instance repGenericParameterMetadata :: Generic ParameterMetadata _
instance showParameterMetadata :: Show ParameterMetadata where
  show = genericShow
instance decodeParameterMetadata :: Decode ParameterMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterMetadata :: Encode ParameterMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterMetadataList = ParameterMetadataList (Array ParameterMetadata)
derive instance newtypeParameterMetadataList :: Newtype ParameterMetadataList _
derive instance repGenericParameterMetadataList :: Generic ParameterMetadataList _
instance showParameterMetadataList :: Show ParameterMetadataList where
  show = genericShow
instance decodeParameterMetadataList :: Decode ParameterMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterMetadataList :: Encode ParameterMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterName = ParameterName String
derive instance newtypeParameterName :: Newtype ParameterName _
derive instance repGenericParameterName :: Generic ParameterName _
instance showParameterName :: Show ParameterName where
  show = genericShow
instance decodeParameterName :: Decode ParameterName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterName :: Encode ParameterName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterNameList = ParameterNameList (Array PSParameterName)
derive instance newtypeParameterNameList :: Newtype ParameterNameList _
derive instance repGenericParameterNameList :: Generic ParameterNameList _
instance showParameterNameList :: Show ParameterNameList where
  show = genericShow
instance decodeParameterNameList :: Decode ParameterNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterNameList :: Encode ParameterNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameter could not be found. Verify the name and try again.</p>
newtype ParameterNotFound = ParameterNotFound 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeParameterNotFound :: Newtype ParameterNotFound _
derive instance repGenericParameterNotFound :: Generic ParameterNotFound _
instance showParameterNotFound :: Show ParameterNotFound where
  show = genericShow
instance decodeParameterNotFound :: Decode ParameterNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterNotFound :: Encode ParameterNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameter name is not valid.</p>
newtype ParameterPatternMismatchException = ParameterPatternMismatchException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeParameterPatternMismatchException :: Newtype ParameterPatternMismatchException _
derive instance repGenericParameterPatternMismatchException :: Generic ParameterPatternMismatchException _
instance showParameterPatternMismatchException :: Show ParameterPatternMismatchException where
  show = genericShow
instance decodeParameterPatternMismatchException :: Decode ParameterPatternMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterPatternMismatchException :: Encode ParameterPatternMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more filters. Use a filter to return a more specific list of results.</p>
newtype ParameterStringFilter = ParameterStringFilter 
  { "Key" :: (ParameterStringFilterKey)
  , "Option" :: NullOrUndefined.NullOrUndefined (ParameterStringQueryOption)
  , "Values" :: NullOrUndefined.NullOrUndefined (ParameterStringFilterValueList)
  }
derive instance newtypeParameterStringFilter :: Newtype ParameterStringFilter _
derive instance repGenericParameterStringFilter :: Generic ParameterStringFilter _
instance showParameterStringFilter :: Show ParameterStringFilter where
  show = genericShow
instance decodeParameterStringFilter :: Decode ParameterStringFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterStringFilter :: Encode ParameterStringFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterStringFilterKey = ParameterStringFilterKey String
derive instance newtypeParameterStringFilterKey :: Newtype ParameterStringFilterKey _
derive instance repGenericParameterStringFilterKey :: Generic ParameterStringFilterKey _
instance showParameterStringFilterKey :: Show ParameterStringFilterKey where
  show = genericShow
instance decodeParameterStringFilterKey :: Decode ParameterStringFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterStringFilterKey :: Encode ParameterStringFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterStringFilterList = ParameterStringFilterList (Array ParameterStringFilter)
derive instance newtypeParameterStringFilterList :: Newtype ParameterStringFilterList _
derive instance repGenericParameterStringFilterList :: Generic ParameterStringFilterList _
instance showParameterStringFilterList :: Show ParameterStringFilterList where
  show = genericShow
instance decodeParameterStringFilterList :: Decode ParameterStringFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterStringFilterList :: Encode ParameterStringFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterStringFilterValue = ParameterStringFilterValue String
derive instance newtypeParameterStringFilterValue :: Newtype ParameterStringFilterValue _
derive instance repGenericParameterStringFilterValue :: Generic ParameterStringFilterValue _
instance showParameterStringFilterValue :: Show ParameterStringFilterValue where
  show = genericShow
instance decodeParameterStringFilterValue :: Decode ParameterStringFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterStringFilterValue :: Encode ParameterStringFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterStringFilterValueList = ParameterStringFilterValueList (Array ParameterStringFilterValue)
derive instance newtypeParameterStringFilterValueList :: Newtype ParameterStringFilterValueList _
derive instance repGenericParameterStringFilterValueList :: Generic ParameterStringFilterValueList _
instance showParameterStringFilterValueList :: Show ParameterStringFilterValueList where
  show = genericShow
instance decodeParameterStringFilterValueList :: Decode ParameterStringFilterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterStringFilterValueList :: Encode ParameterStringFilterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterStringQueryOption = ParameterStringQueryOption String
derive instance newtypeParameterStringQueryOption :: Newtype ParameterStringQueryOption _
derive instance repGenericParameterStringQueryOption :: Generic ParameterStringQueryOption _
instance showParameterStringQueryOption :: Show ParameterStringQueryOption where
  show = genericShow
instance decodeParameterStringQueryOption :: Decode ParameterStringQueryOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterStringQueryOption :: Encode ParameterStringQueryOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterType = ParameterType String
derive instance newtypeParameterType :: Newtype ParameterType _
derive instance repGenericParameterType :: Generic ParameterType _
instance showParameterType :: Show ParameterType where
  show = genericShow
instance decodeParameterType :: Decode ParameterType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterType :: Encode ParameterType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterValue = ParameterValue String
derive instance newtypeParameterValue :: Newtype ParameterValue _
derive instance repGenericParameterValue :: Generic ParameterValue _
instance showParameterValue :: Show ParameterValue where
  show = genericShow
instance decodeParameterValue :: Decode ParameterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterValue :: Encode ParameterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterValueList = ParameterValueList (Array ParameterValue)
derive instance newtypeParameterValueList :: Newtype ParameterValueList _
derive instance repGenericParameterValueList :: Generic ParameterValueList _
instance showParameterValueList :: Show ParameterValueList where
  show = genericShow
instance decodeParameterValueList :: Decode ParameterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterValueList :: Encode ParameterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified parameter version was not found. Verify the parameter name and version, and try again.</p>
newtype ParameterVersionNotFound = ParameterVersionNotFound 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeParameterVersionNotFound :: Newtype ParameterVersionNotFound _
derive instance repGenericParameterVersionNotFound :: Generic ParameterVersionNotFound _
instance showParameterVersionNotFound :: Show ParameterVersionNotFound where
  show = genericShow
instance decodeParameterVersionNotFound :: Decode ParameterVersionNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterVersionNotFound :: Encode ParameterVersionNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Parameters = Parameters (StrMap.StrMap ParameterValueList)
derive instance newtypeParameters :: Newtype Parameters _
derive instance repGenericParameters :: Generic Parameters _
instance showParameters :: Show Parameters where
  show = genericShow
instance decodeParameters :: Decode Parameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameters :: Encode Parameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This data type is deprecated. Instead, use <a>ParameterStringFilter</a>.</p>
newtype ParametersFilter = ParametersFilter 
  { "Key" :: (ParametersFilterKey)
  , "Values" :: (ParametersFilterValueList)
  }
derive instance newtypeParametersFilter :: Newtype ParametersFilter _
derive instance repGenericParametersFilter :: Generic ParametersFilter _
instance showParametersFilter :: Show ParametersFilter where
  show = genericShow
instance decodeParametersFilter :: Decode ParametersFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersFilter :: Encode ParametersFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParametersFilterKey = ParametersFilterKey String
derive instance newtypeParametersFilterKey :: Newtype ParametersFilterKey _
derive instance repGenericParametersFilterKey :: Generic ParametersFilterKey _
instance showParametersFilterKey :: Show ParametersFilterKey where
  show = genericShow
instance decodeParametersFilterKey :: Decode ParametersFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersFilterKey :: Encode ParametersFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParametersFilterList = ParametersFilterList (Array ParametersFilter)
derive instance newtypeParametersFilterList :: Newtype ParametersFilterList _
derive instance repGenericParametersFilterList :: Generic ParametersFilterList _
instance showParametersFilterList :: Show ParametersFilterList where
  show = genericShow
instance decodeParametersFilterList :: Decode ParametersFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersFilterList :: Encode ParametersFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParametersFilterValue = ParametersFilterValue String
derive instance newtypeParametersFilterValue :: Newtype ParametersFilterValue _
derive instance repGenericParametersFilterValue :: Generic ParametersFilterValue _
instance showParametersFilterValue :: Show ParametersFilterValue where
  show = genericShow
instance decodeParametersFilterValue :: Decode ParametersFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersFilterValue :: Encode ParametersFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParametersFilterValueList = ParametersFilterValueList (Array ParametersFilterValue)
derive instance newtypeParametersFilterValueList :: Newtype ParametersFilterValueList _
derive instance repGenericParametersFilterValueList :: Generic ParametersFilterValueList _
instance showParametersFilterValueList :: Show ParametersFilterValueList where
  show = genericShow
instance decodeParametersFilterValueList :: Decode ParametersFilterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersFilterValueList :: Encode ParametersFilterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents metadata about a patch.</p>
newtype Patch = Patch 
  { "Id" :: NullOrUndefined.NullOrUndefined (PatchId)
  , "ReleaseDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Title" :: NullOrUndefined.NullOrUndefined (PatchTitle)
  , "Description" :: NullOrUndefined.NullOrUndefined (PatchDescription)
  , "ContentUrl" :: NullOrUndefined.NullOrUndefined (PatchContentUrl)
  , "Vendor" :: NullOrUndefined.NullOrUndefined (PatchVendor)
  , "ProductFamily" :: NullOrUndefined.NullOrUndefined (PatchProductFamily)
  , "Product" :: NullOrUndefined.NullOrUndefined (PatchProduct)
  , "Classification" :: NullOrUndefined.NullOrUndefined (PatchClassification)
  , "MsrcSeverity" :: NullOrUndefined.NullOrUndefined (PatchMsrcSeverity)
  , "KbNumber" :: NullOrUndefined.NullOrUndefined (PatchKbNumber)
  , "MsrcNumber" :: NullOrUndefined.NullOrUndefined (PatchMsrcNumber)
  , "Language" :: NullOrUndefined.NullOrUndefined (PatchLanguage)
  }
derive instance newtypePatch :: Newtype Patch _
derive instance repGenericPatch :: Generic Patch _
instance showPatch :: Show Patch where
  show = genericShow
instance decodePatch :: Decode Patch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatch :: Encode Patch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines the basic information about a patch baseline.</p>
newtype PatchBaselineIdentity = PatchBaselineIdentity 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  , "BaselineName" :: NullOrUndefined.NullOrUndefined (BaselineName)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  , "BaselineDescription" :: NullOrUndefined.NullOrUndefined (BaselineDescription)
  , "DefaultBaseline" :: NullOrUndefined.NullOrUndefined (DefaultBaseline)
  }
derive instance newtypePatchBaselineIdentity :: Newtype PatchBaselineIdentity _
derive instance repGenericPatchBaselineIdentity :: Generic PatchBaselineIdentity _
instance showPatchBaselineIdentity :: Show PatchBaselineIdentity where
  show = genericShow
instance decodePatchBaselineIdentity :: Decode PatchBaselineIdentity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchBaselineIdentity :: Encode PatchBaselineIdentity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchBaselineIdentityList = PatchBaselineIdentityList (Array PatchBaselineIdentity)
derive instance newtypePatchBaselineIdentityList :: Newtype PatchBaselineIdentityList _
derive instance repGenericPatchBaselineIdentityList :: Generic PatchBaselineIdentityList _
instance showPatchBaselineIdentityList :: Show PatchBaselineIdentityList where
  show = genericShow
instance decodePatchBaselineIdentityList :: Decode PatchBaselineIdentityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchBaselineIdentityList :: Encode PatchBaselineIdentityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchBaselineMaxResults = PatchBaselineMaxResults Int
derive instance newtypePatchBaselineMaxResults :: Newtype PatchBaselineMaxResults _
derive instance repGenericPatchBaselineMaxResults :: Generic PatchBaselineMaxResults _
instance showPatchBaselineMaxResults :: Show PatchBaselineMaxResults where
  show = genericShow
instance decodePatchBaselineMaxResults :: Decode PatchBaselineMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchBaselineMaxResults :: Encode PatchBaselineMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchClassification = PatchClassification String
derive instance newtypePatchClassification :: Newtype PatchClassification _
derive instance repGenericPatchClassification :: Generic PatchClassification _
instance showPatchClassification :: Show PatchClassification where
  show = genericShow
instance decodePatchClassification :: Decode PatchClassification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchClassification :: Encode PatchClassification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


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
derive instance repGenericPatchComplianceData :: Generic PatchComplianceData _
instance showPatchComplianceData :: Show PatchComplianceData where
  show = genericShow
instance decodePatchComplianceData :: Decode PatchComplianceData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchComplianceData :: Encode PatchComplianceData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchComplianceDataList = PatchComplianceDataList (Array PatchComplianceData)
derive instance newtypePatchComplianceDataList :: Newtype PatchComplianceDataList _
derive instance repGenericPatchComplianceDataList :: Generic PatchComplianceDataList _
instance showPatchComplianceDataList :: Show PatchComplianceDataList where
  show = genericShow
instance decodePatchComplianceDataList :: Decode PatchComplianceDataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchComplianceDataList :: Encode PatchComplianceDataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchComplianceDataState = PatchComplianceDataState String
derive instance newtypePatchComplianceDataState :: Newtype PatchComplianceDataState _
derive instance repGenericPatchComplianceDataState :: Generic PatchComplianceDataState _
instance showPatchComplianceDataState :: Show PatchComplianceDataState where
  show = genericShow
instance decodePatchComplianceDataState :: Decode PatchComplianceDataState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchComplianceDataState :: Encode PatchComplianceDataState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchComplianceLevel = PatchComplianceLevel String
derive instance newtypePatchComplianceLevel :: Newtype PatchComplianceLevel _
derive instance repGenericPatchComplianceLevel :: Generic PatchComplianceLevel _
instance showPatchComplianceLevel :: Show PatchComplianceLevel where
  show = genericShow
instance decodePatchComplianceLevel :: Decode PatchComplianceLevel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchComplianceLevel :: Encode PatchComplianceLevel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchComplianceMaxResults = PatchComplianceMaxResults Int
derive instance newtypePatchComplianceMaxResults :: Newtype PatchComplianceMaxResults _
derive instance repGenericPatchComplianceMaxResults :: Generic PatchComplianceMaxResults _
instance showPatchComplianceMaxResults :: Show PatchComplianceMaxResults where
  show = genericShow
instance decodePatchComplianceMaxResults :: Decode PatchComplianceMaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchComplianceMaxResults :: Encode PatchComplianceMaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchContentUrl = PatchContentUrl String
derive instance newtypePatchContentUrl :: Newtype PatchContentUrl _
derive instance repGenericPatchContentUrl :: Generic PatchContentUrl _
instance showPatchContentUrl :: Show PatchContentUrl where
  show = genericShow
instance decodePatchContentUrl :: Decode PatchContentUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchContentUrl :: Encode PatchContentUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchDeploymentStatus = PatchDeploymentStatus String
derive instance newtypePatchDeploymentStatus :: Newtype PatchDeploymentStatus _
derive instance repGenericPatchDeploymentStatus :: Generic PatchDeploymentStatus _
instance showPatchDeploymentStatus :: Show PatchDeploymentStatus where
  show = genericShow
instance decodePatchDeploymentStatus :: Decode PatchDeploymentStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchDeploymentStatus :: Encode PatchDeploymentStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchDescription = PatchDescription String
derive instance newtypePatchDescription :: Newtype PatchDescription _
derive instance repGenericPatchDescription :: Generic PatchDescription _
instance showPatchDescription :: Show PatchDescription where
  show = genericShow
instance decodePatchDescription :: Decode PatchDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchDescription :: Encode PatchDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchFailedCount = PatchFailedCount Int
derive instance newtypePatchFailedCount :: Newtype PatchFailedCount _
derive instance repGenericPatchFailedCount :: Generic PatchFailedCount _
instance showPatchFailedCount :: Show PatchFailedCount where
  show = genericShow
instance decodePatchFailedCount :: Decode PatchFailedCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchFailedCount :: Encode PatchFailedCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines a patch filter.</p> <p>A patch filter consists of key/value pairs, but not all keys are valid for all operating system types. For example, the key <code>PRODUCT</code> is valid for all supported operating system types. The key <code>MSRC_SEVERITY</code>, however, is valid only for Windows operating systems, and the key <code>SECTION</code> is valid only for Ubuntu operating systems.</p> <p>Refer to the following sections for information about which keys may be used with each major operating system, and which values are valid for each key.</p> <p> <b>Windows Operating Systems</b> </p> <p>The supported keys for Windows operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>MSRC_SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Windows7</code> </p> </li> <li> <p> <code>Windows8</code> </p> </li> <li> <p> <code>Windows8.1</code> </p> </li> <li> <p> <code>Windows8Embedded</code> </p> </li> <li> <p> <code>Windows10</code> </p> </li> <li> <p> <code>Windows10LTSB</code> </p> </li> <li> <p> <code>WindowsServer2008</code> </p> </li> <li> <p> <code>WindowsServer2008R2</code> </p> </li> <li> <p> <code>WindowsServer2012</code> </p> </li> <li> <p> <code>WindowsServer2012R2</code> </p> </li> <li> <p> <code>WindowsServer2016</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>CriticalUpdates</code> </p> </li> <li> <p> <code>DefinitionUpdates</code> </p> </li> <li> <p> <code>Drivers</code> </p> </li> <li> <p> <code>FeaturePacks</code> </p> </li> <li> <p> <code>SecurityUpdates</code> </p> </li> <li> <p> <code>ServicePacks</code> </p> </li> <li> <p> <code>Tools</code> </p> </li> <li> <p> <code>UpdateRollups</code> </p> </li> <li> <p> <code>Updates</code> </p> </li> <li> <p> <code>Upgrades</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>MSRC_SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> <li> <p> <code>Unspecified</code> </p> </li> </ul> <p> <b>Ubuntu Operating Systems</b> </p> <p>The supported keys for Ubuntu operating systems are <code>PRODUCT</code>, <code>PRIORITY</code>, and <code>SECTION</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Ubuntu14.04</code> </p> </li> <li> <p> <code>Ubuntu16.04</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>PRIORITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Required</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Standard</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Extra</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SECTION</code> </p> <p>Only the length of the key value is validated. Minimum length is 1. Maximum length is 64.</p> <p> <b>Amazon Linux Operating Systems</b> </p> <p>The supported keys for Amazon Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>AmazonLinux2012.03</code> </p> </li> <li> <p> <code>AmazonLinux2012.09</code> </p> </li> <li> <p> <code>AmazonLinux2013.03</code> </p> </li> <li> <p> <code>AmazonLinux2013.09</code> </p> </li> <li> <p> <code>AmazonLinux2014.03</code> </p> </li> <li> <p> <code>AmazonLinux2014.09</code> </p> </li> <li> <p> <code>AmazonLinux2015.03</code> </p> </li> <li> <p> <code>AmazonLinux2015.09</code> </p> </li> <li> <p> <code>AmazonLinux2016.03</code> </p> </li> <li> <p> <code>AmazonLinux2016.09</code> </p> </li> <li> <p> <code>AmazonLinux2017.03</code> </p> </li> <li> <p> <code>AmazonLinux2017.09</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>RedHat Enterprise Linux (RHEL) Operating Systems</b> </p> <p>The supported keys for RedHat Enterprise Linux operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>RedhatEnterpriseLinux6.5</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.6</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.7</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.8</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux6.9</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.0</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.1</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.2</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.3</code> </p> </li> <li> <p> <code>RedhatEnterpriseLinux7.4</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Bugfix</code> </p> </li> <li> <p> <code>Enhancement</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Newpackage</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Medium</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul> <p> <b>SUSE Linux Enterprise Server (SUSE) Operating Systems</b> </p> <p>The supported keys for SUSE operating systems are <code>PRODUCT</code>, <code>CLASSIFICATION</code>, and <code>SEVERITY</code>. See the following lists for valid values for each of these keys.</p> <p> <i>Supported key:</i> <code>PRODUCT</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Suse12.0</code> </p> </li> <li> <p> <code>Suse12.1</code> </p> </li> <li> <p> <code>Suse12.2</code> </p> </li> <li> <p> <code>Suse12.3</code> </p> </li> <li> <p> <code>Suse12.4</code> </p> </li> <li> <p> <code>Suse12.5</code> </p> </li> <li> <p> <code>Suse12.6</code> </p> </li> <li> <p> <code>Suse12.7</code> </p> </li> <li> <p> <code>Suse12.8</code> </p> </li> <li> <p> <code>Suse12.9</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>CLASSIFICATION</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Security</code> </p> </li> <li> <p> <code>Recommended</code> </p> </li> <li> <p> <code>Optional</code> </p> </li> <li> <p> <code>Feature</code> </p> </li> <li> <p> <code>Document</code> </p> </li> <li> <p> <code>Yast</code> </p> </li> </ul> <p> <i>Supported key:</i> <code>SEVERITY</code> </p> <p> <i>Supported values:</i> </p> <ul> <li> <p> <code>Critical</code> </p> </li> <li> <p> <code>Important</code> </p> </li> <li> <p> <code>Moderate</code> </p> </li> <li> <p> <code>Low</code> </p> </li> </ul>
newtype PatchFilter = PatchFilter 
  { "Key" :: (PatchFilterKey)
  , "Values" :: (PatchFilterValueList)
  }
derive instance newtypePatchFilter :: Newtype PatchFilter _
derive instance repGenericPatchFilter :: Generic PatchFilter _
instance showPatchFilter :: Show PatchFilter where
  show = genericShow
instance decodePatchFilter :: Decode PatchFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchFilter :: Encode PatchFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A set of patch filters, typically used for approval rules.</p>
newtype PatchFilterGroup = PatchFilterGroup 
  { "PatchFilters" :: (PatchFilterList)
  }
derive instance newtypePatchFilterGroup :: Newtype PatchFilterGroup _
derive instance repGenericPatchFilterGroup :: Generic PatchFilterGroup _
instance showPatchFilterGroup :: Show PatchFilterGroup where
  show = genericShow
instance decodePatchFilterGroup :: Decode PatchFilterGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchFilterGroup :: Encode PatchFilterGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchFilterKey = PatchFilterKey String
derive instance newtypePatchFilterKey :: Newtype PatchFilterKey _
derive instance repGenericPatchFilterKey :: Generic PatchFilterKey _
instance showPatchFilterKey :: Show PatchFilterKey where
  show = genericShow
instance decodePatchFilterKey :: Decode PatchFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchFilterKey :: Encode PatchFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchFilterList = PatchFilterList (Array PatchFilter)
derive instance newtypePatchFilterList :: Newtype PatchFilterList _
derive instance repGenericPatchFilterList :: Generic PatchFilterList _
instance showPatchFilterList :: Show PatchFilterList where
  show = genericShow
instance decodePatchFilterList :: Decode PatchFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchFilterList :: Encode PatchFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchFilterValue = PatchFilterValue String
derive instance newtypePatchFilterValue :: Newtype PatchFilterValue _
derive instance repGenericPatchFilterValue :: Generic PatchFilterValue _
instance showPatchFilterValue :: Show PatchFilterValue where
  show = genericShow
instance decodePatchFilterValue :: Decode PatchFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchFilterValue :: Encode PatchFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchFilterValueList = PatchFilterValueList (Array PatchFilterValue)
derive instance newtypePatchFilterValueList :: Newtype PatchFilterValueList _
derive instance repGenericPatchFilterValueList :: Generic PatchFilterValueList _
instance showPatchFilterValueList :: Show PatchFilterValueList where
  show = genericShow
instance decodePatchFilterValueList :: Decode PatchFilterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchFilterValueList :: Encode PatchFilterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchGroup = PatchGroup String
derive instance newtypePatchGroup :: Newtype PatchGroup _
derive instance repGenericPatchGroup :: Generic PatchGroup _
instance showPatchGroup :: Show PatchGroup where
  show = genericShow
instance decodePatchGroup :: Decode PatchGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchGroup :: Encode PatchGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchGroupList = PatchGroupList (Array PatchGroup)
derive instance newtypePatchGroupList :: Newtype PatchGroupList _
derive instance repGenericPatchGroupList :: Generic PatchGroupList _
instance showPatchGroupList :: Show PatchGroupList where
  show = genericShow
instance decodePatchGroupList :: Decode PatchGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchGroupList :: Encode PatchGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The mapping between a patch group and the patch baseline the patch group is registered with.</p>
newtype PatchGroupPatchBaselineMapping = PatchGroupPatchBaselineMapping 
  { "PatchGroup" :: NullOrUndefined.NullOrUndefined (PatchGroup)
  , "BaselineIdentity" :: NullOrUndefined.NullOrUndefined (PatchBaselineIdentity)
  }
derive instance newtypePatchGroupPatchBaselineMapping :: Newtype PatchGroupPatchBaselineMapping _
derive instance repGenericPatchGroupPatchBaselineMapping :: Generic PatchGroupPatchBaselineMapping _
instance showPatchGroupPatchBaselineMapping :: Show PatchGroupPatchBaselineMapping where
  show = genericShow
instance decodePatchGroupPatchBaselineMapping :: Decode PatchGroupPatchBaselineMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchGroupPatchBaselineMapping :: Encode PatchGroupPatchBaselineMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchGroupPatchBaselineMappingList = PatchGroupPatchBaselineMappingList (Array PatchGroupPatchBaselineMapping)
derive instance newtypePatchGroupPatchBaselineMappingList :: Newtype PatchGroupPatchBaselineMappingList _
derive instance repGenericPatchGroupPatchBaselineMappingList :: Generic PatchGroupPatchBaselineMappingList _
instance showPatchGroupPatchBaselineMappingList :: Show PatchGroupPatchBaselineMappingList where
  show = genericShow
instance decodePatchGroupPatchBaselineMappingList :: Decode PatchGroupPatchBaselineMappingList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchGroupPatchBaselineMappingList :: Encode PatchGroupPatchBaselineMappingList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchId = PatchId String
derive instance newtypePatchId :: Newtype PatchId _
derive instance repGenericPatchId :: Generic PatchId _
instance showPatchId :: Show PatchId where
  show = genericShow
instance decodePatchId :: Decode PatchId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchId :: Encode PatchId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchIdList = PatchIdList (Array PatchId)
derive instance newtypePatchIdList :: Newtype PatchIdList _
derive instance repGenericPatchIdList :: Generic PatchIdList _
instance showPatchIdList :: Show PatchIdList where
  show = genericShow
instance decodePatchIdList :: Decode PatchIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchIdList :: Encode PatchIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchInstalledCount = PatchInstalledCount Int
derive instance newtypePatchInstalledCount :: Newtype PatchInstalledCount _
derive instance repGenericPatchInstalledCount :: Generic PatchInstalledCount _
instance showPatchInstalledCount :: Show PatchInstalledCount where
  show = genericShow
instance decodePatchInstalledCount :: Decode PatchInstalledCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchInstalledCount :: Encode PatchInstalledCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchInstalledOtherCount = PatchInstalledOtherCount Int
derive instance newtypePatchInstalledOtherCount :: Newtype PatchInstalledOtherCount _
derive instance repGenericPatchInstalledOtherCount :: Generic PatchInstalledOtherCount _
instance showPatchInstalledOtherCount :: Show PatchInstalledOtherCount where
  show = genericShow
instance decodePatchInstalledOtherCount :: Decode PatchInstalledOtherCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchInstalledOtherCount :: Encode PatchInstalledOtherCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchKbNumber = PatchKbNumber String
derive instance newtypePatchKbNumber :: Newtype PatchKbNumber _
derive instance repGenericPatchKbNumber :: Generic PatchKbNumber _
instance showPatchKbNumber :: Show PatchKbNumber where
  show = genericShow
instance decodePatchKbNumber :: Decode PatchKbNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchKbNumber :: Encode PatchKbNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchLanguage = PatchLanguage String
derive instance newtypePatchLanguage :: Newtype PatchLanguage _
derive instance repGenericPatchLanguage :: Generic PatchLanguage _
instance showPatchLanguage :: Show PatchLanguage where
  show = genericShow
instance decodePatchLanguage :: Decode PatchLanguage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchLanguage :: Encode PatchLanguage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchList = PatchList (Array Patch)
derive instance newtypePatchList :: Newtype PatchList _
derive instance repGenericPatchList :: Generic PatchList _
instance showPatchList :: Show PatchList where
  show = genericShow
instance decodePatchList :: Decode PatchList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchList :: Encode PatchList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchMissingCount = PatchMissingCount Int
derive instance newtypePatchMissingCount :: Newtype PatchMissingCount _
derive instance repGenericPatchMissingCount :: Generic PatchMissingCount _
instance showPatchMissingCount :: Show PatchMissingCount where
  show = genericShow
instance decodePatchMissingCount :: Decode PatchMissingCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchMissingCount :: Encode PatchMissingCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchMsrcNumber = PatchMsrcNumber String
derive instance newtypePatchMsrcNumber :: Newtype PatchMsrcNumber _
derive instance repGenericPatchMsrcNumber :: Generic PatchMsrcNumber _
instance showPatchMsrcNumber :: Show PatchMsrcNumber where
  show = genericShow
instance decodePatchMsrcNumber :: Decode PatchMsrcNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchMsrcNumber :: Encode PatchMsrcNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchMsrcSeverity = PatchMsrcSeverity String
derive instance newtypePatchMsrcSeverity :: Newtype PatchMsrcSeverity _
derive instance repGenericPatchMsrcSeverity :: Generic PatchMsrcSeverity _
instance showPatchMsrcSeverity :: Show PatchMsrcSeverity where
  show = genericShow
instance decodePatchMsrcSeverity :: Decode PatchMsrcSeverity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchMsrcSeverity :: Encode PatchMsrcSeverity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchNotApplicableCount = PatchNotApplicableCount Int
derive instance newtypePatchNotApplicableCount :: Newtype PatchNotApplicableCount _
derive instance repGenericPatchNotApplicableCount :: Generic PatchNotApplicableCount _
instance showPatchNotApplicableCount :: Show PatchNotApplicableCount where
  show = genericShow
instance decodePatchNotApplicableCount :: Decode PatchNotApplicableCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchNotApplicableCount :: Encode PatchNotApplicableCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchOperationType = PatchOperationType String
derive instance newtypePatchOperationType :: Newtype PatchOperationType _
derive instance repGenericPatchOperationType :: Generic PatchOperationType _
instance showPatchOperationType :: Show PatchOperationType where
  show = genericShow
instance decodePatchOperationType :: Decode PatchOperationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchOperationType :: Encode PatchOperationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines a filter used in Patch Manager APIs.</p>
newtype PatchOrchestratorFilter = PatchOrchestratorFilter 
  { "Key" :: NullOrUndefined.NullOrUndefined (PatchOrchestratorFilterKey)
  , "Values" :: NullOrUndefined.NullOrUndefined (PatchOrchestratorFilterValues)
  }
derive instance newtypePatchOrchestratorFilter :: Newtype PatchOrchestratorFilter _
derive instance repGenericPatchOrchestratorFilter :: Generic PatchOrchestratorFilter _
instance showPatchOrchestratorFilter :: Show PatchOrchestratorFilter where
  show = genericShow
instance decodePatchOrchestratorFilter :: Decode PatchOrchestratorFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchOrchestratorFilter :: Encode PatchOrchestratorFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchOrchestratorFilterKey = PatchOrchestratorFilterKey String
derive instance newtypePatchOrchestratorFilterKey :: Newtype PatchOrchestratorFilterKey _
derive instance repGenericPatchOrchestratorFilterKey :: Generic PatchOrchestratorFilterKey _
instance showPatchOrchestratorFilterKey :: Show PatchOrchestratorFilterKey where
  show = genericShow
instance decodePatchOrchestratorFilterKey :: Decode PatchOrchestratorFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchOrchestratorFilterKey :: Encode PatchOrchestratorFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchOrchestratorFilterList = PatchOrchestratorFilterList (Array PatchOrchestratorFilter)
derive instance newtypePatchOrchestratorFilterList :: Newtype PatchOrchestratorFilterList _
derive instance repGenericPatchOrchestratorFilterList :: Generic PatchOrchestratorFilterList _
instance showPatchOrchestratorFilterList :: Show PatchOrchestratorFilterList where
  show = genericShow
instance decodePatchOrchestratorFilterList :: Decode PatchOrchestratorFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchOrchestratorFilterList :: Encode PatchOrchestratorFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchOrchestratorFilterValue = PatchOrchestratorFilterValue String
derive instance newtypePatchOrchestratorFilterValue :: Newtype PatchOrchestratorFilterValue _
derive instance repGenericPatchOrchestratorFilterValue :: Generic PatchOrchestratorFilterValue _
instance showPatchOrchestratorFilterValue :: Show PatchOrchestratorFilterValue where
  show = genericShow
instance decodePatchOrchestratorFilterValue :: Decode PatchOrchestratorFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchOrchestratorFilterValue :: Encode PatchOrchestratorFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchOrchestratorFilterValues = PatchOrchestratorFilterValues (Array PatchOrchestratorFilterValue)
derive instance newtypePatchOrchestratorFilterValues :: Newtype PatchOrchestratorFilterValues _
derive instance repGenericPatchOrchestratorFilterValues :: Generic PatchOrchestratorFilterValues _
instance showPatchOrchestratorFilterValues :: Show PatchOrchestratorFilterValues where
  show = genericShow
instance decodePatchOrchestratorFilterValues :: Decode PatchOrchestratorFilterValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchOrchestratorFilterValues :: Encode PatchOrchestratorFilterValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchProduct = PatchProduct String
derive instance newtypePatchProduct :: Newtype PatchProduct _
derive instance repGenericPatchProduct :: Generic PatchProduct _
instance showPatchProduct :: Show PatchProduct where
  show = genericShow
instance decodePatchProduct :: Decode PatchProduct where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchProduct :: Encode PatchProduct where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchProductFamily = PatchProductFamily String
derive instance newtypePatchProductFamily :: Newtype PatchProductFamily _
derive instance repGenericPatchProductFamily :: Generic PatchProductFamily _
instance showPatchProductFamily :: Show PatchProductFamily where
  show = genericShow
instance decodePatchProductFamily :: Decode PatchProductFamily where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchProductFamily :: Encode PatchProductFamily where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines an approval rule for a patch baseline.</p>
newtype PatchRule = PatchRule 
  { "PatchFilterGroup" :: (PatchFilterGroup)
  , "ComplianceLevel" :: NullOrUndefined.NullOrUndefined (PatchComplianceLevel)
  , "ApproveAfterDays" :: (ApproveAfterDays)
  , "EnableNonSecurity" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypePatchRule :: Newtype PatchRule _
derive instance repGenericPatchRule :: Generic PatchRule _
instance showPatchRule :: Show PatchRule where
  show = genericShow
instance decodePatchRule :: Decode PatchRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchRule :: Encode PatchRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A set of rules defining the approval rules for a patch baseline.</p>
newtype PatchRuleGroup = PatchRuleGroup 
  { "PatchRules" :: (PatchRuleList)
  }
derive instance newtypePatchRuleGroup :: Newtype PatchRuleGroup _
derive instance repGenericPatchRuleGroup :: Generic PatchRuleGroup _
instance showPatchRuleGroup :: Show PatchRuleGroup where
  show = genericShow
instance decodePatchRuleGroup :: Decode PatchRuleGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchRuleGroup :: Encode PatchRuleGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchRuleList = PatchRuleList (Array PatchRule)
derive instance newtypePatchRuleList :: Newtype PatchRuleList _
derive instance repGenericPatchRuleList :: Generic PatchRuleList _
instance showPatchRuleList :: Show PatchRuleList where
  show = genericShow
instance decodePatchRuleList :: Decode PatchRuleList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchRuleList :: Encode PatchRuleList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchSeverity = PatchSeverity String
derive instance newtypePatchSeverity :: Newtype PatchSeverity _
derive instance repGenericPatchSeverity :: Generic PatchSeverity _
instance showPatchSeverity :: Show PatchSeverity where
  show = genericShow
instance decodePatchSeverity :: Decode PatchSeverity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchSeverity :: Encode PatchSeverity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the patches to use to update the instances, including target operating systems and source repository. Applies to Linux instances only.</p>
newtype PatchSource = PatchSource 
  { "Name" :: (PatchSourceName)
  , "Products" :: (PatchSourceProductList)
  , "Configuration" :: (PatchSourceConfiguration)
  }
derive instance newtypePatchSource :: Newtype PatchSource _
derive instance repGenericPatchSource :: Generic PatchSource _
instance showPatchSource :: Show PatchSource where
  show = genericShow
instance decodePatchSource :: Decode PatchSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchSource :: Encode PatchSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchSourceConfiguration = PatchSourceConfiguration String
derive instance newtypePatchSourceConfiguration :: Newtype PatchSourceConfiguration _
derive instance repGenericPatchSourceConfiguration :: Generic PatchSourceConfiguration _
instance showPatchSourceConfiguration :: Show PatchSourceConfiguration where
  show = genericShow
instance decodePatchSourceConfiguration :: Decode PatchSourceConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchSourceConfiguration :: Encode PatchSourceConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchSourceList = PatchSourceList (Array PatchSource)
derive instance newtypePatchSourceList :: Newtype PatchSourceList _
derive instance repGenericPatchSourceList :: Generic PatchSourceList _
instance showPatchSourceList :: Show PatchSourceList where
  show = genericShow
instance decodePatchSourceList :: Decode PatchSourceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchSourceList :: Encode PatchSourceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchSourceName = PatchSourceName String
derive instance newtypePatchSourceName :: Newtype PatchSourceName _
derive instance repGenericPatchSourceName :: Generic PatchSourceName _
instance showPatchSourceName :: Show PatchSourceName where
  show = genericShow
instance decodePatchSourceName :: Decode PatchSourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchSourceName :: Encode PatchSourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchSourceProduct = PatchSourceProduct String
derive instance newtypePatchSourceProduct :: Newtype PatchSourceProduct _
derive instance repGenericPatchSourceProduct :: Generic PatchSourceProduct _
instance showPatchSourceProduct :: Show PatchSourceProduct where
  show = genericShow
instance decodePatchSourceProduct :: Decode PatchSourceProduct where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchSourceProduct :: Encode PatchSourceProduct where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchSourceProductList = PatchSourceProductList (Array PatchSourceProduct)
derive instance newtypePatchSourceProductList :: Newtype PatchSourceProductList _
derive instance repGenericPatchSourceProductList :: Generic PatchSourceProductList _
instance showPatchSourceProductList :: Show PatchSourceProductList where
  show = genericShow
instance decodePatchSourceProductList :: Decode PatchSourceProductList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchSourceProductList :: Encode PatchSourceProductList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the approval status of a patch.</p>
newtype PatchStatus = PatchStatus 
  { "DeploymentStatus" :: NullOrUndefined.NullOrUndefined (PatchDeploymentStatus)
  , "ComplianceLevel" :: NullOrUndefined.NullOrUndefined (PatchComplianceLevel)
  , "ApprovalDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  }
derive instance newtypePatchStatus :: Newtype PatchStatus _
derive instance repGenericPatchStatus :: Generic PatchStatus _
instance showPatchStatus :: Show PatchStatus where
  show = genericShow
instance decodePatchStatus :: Decode PatchStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchStatus :: Encode PatchStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchTitle = PatchTitle String
derive instance newtypePatchTitle :: Newtype PatchTitle _
derive instance repGenericPatchTitle :: Generic PatchTitle _
instance showPatchTitle :: Show PatchTitle where
  show = genericShow
instance decodePatchTitle :: Decode PatchTitle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchTitle :: Encode PatchTitle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PatchVendor = PatchVendor String
derive instance newtypePatchVendor :: Newtype PatchVendor _
derive instance repGenericPatchVendor :: Generic PatchVendor _
instance showPatchVendor :: Show PatchVendor where
  show = genericShow
instance decodePatchVendor :: Decode PatchVendor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePatchVendor :: Encode PatchVendor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PingStatus = PingStatus String
derive instance newtypePingStatus :: Newtype PingStatus _
derive instance repGenericPingStatus :: Generic PingStatus _
instance showPingStatus :: Show PingStatus where
  show = genericShow
instance decodePingStatus :: Decode PingStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePingStatus :: Encode PingStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformType = PlatformType String
derive instance newtypePlatformType :: Newtype PlatformType _
derive instance repGenericPlatformType :: Generic PlatformType _
instance showPlatformType :: Show PlatformType where
  show = genericShow
instance decodePlatformType :: Decode PlatformType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformType :: Encode PlatformType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformTypeList = PlatformTypeList (Array PlatformType)
derive instance newtypePlatformTypeList :: Newtype PlatformTypeList _
derive instance repGenericPlatformTypeList :: Generic PlatformTypeList _
instance showPlatformTypeList :: Show PlatformTypeList where
  show = genericShow
instance decodePlatformTypeList :: Decode PlatformTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformTypeList :: Encode PlatformTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Product = Product String
derive instance newtypeProduct :: Newtype Product _
derive instance repGenericProduct :: Generic Product _
instance showProduct :: Show Product where
  show = genericShow
instance decodeProduct :: Decode Product where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProduct :: Encode Product where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutComplianceItemsRequest = PutComplianceItemsRequest 
  { "ResourceId" :: (ComplianceResourceId)
  , "ResourceType" :: (ComplianceResourceType)
  , "ComplianceType" :: (ComplianceTypeName)
  , "ExecutionSummary" :: (ComplianceExecutionSummary)
  , "Items" :: (ComplianceItemEntryList)
  , "ItemContentHash" :: NullOrUndefined.NullOrUndefined (ComplianceItemContentHash)
  }
derive instance newtypePutComplianceItemsRequest :: Newtype PutComplianceItemsRequest _
derive instance repGenericPutComplianceItemsRequest :: Generic PutComplianceItemsRequest _
instance showPutComplianceItemsRequest :: Show PutComplianceItemsRequest where
  show = genericShow
instance decodePutComplianceItemsRequest :: Decode PutComplianceItemsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutComplianceItemsRequest :: Encode PutComplianceItemsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutComplianceItemsResult = PutComplianceItemsResult Types.NoArguments
derive instance newtypePutComplianceItemsResult :: Newtype PutComplianceItemsResult _
derive instance repGenericPutComplianceItemsResult :: Generic PutComplianceItemsResult _
instance showPutComplianceItemsResult :: Show PutComplianceItemsResult where
  show = genericShow
instance decodePutComplianceItemsResult :: Decode PutComplianceItemsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutComplianceItemsResult :: Encode PutComplianceItemsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutInventoryRequest = PutInventoryRequest 
  { "InstanceId" :: (InstanceId)
  , "Items" :: (InventoryItemList)
  }
derive instance newtypePutInventoryRequest :: Newtype PutInventoryRequest _
derive instance repGenericPutInventoryRequest :: Generic PutInventoryRequest _
instance showPutInventoryRequest :: Show PutInventoryRequest where
  show = genericShow
instance decodePutInventoryRequest :: Decode PutInventoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutInventoryRequest :: Encode PutInventoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutInventoryResult = PutInventoryResult Types.NoArguments
derive instance newtypePutInventoryResult :: Newtype PutInventoryResult _
derive instance repGenericPutInventoryResult :: Generic PutInventoryResult _
instance showPutInventoryResult :: Show PutInventoryResult where
  show = genericShow
instance decodePutInventoryResult :: Decode PutInventoryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutInventoryResult :: Encode PutInventoryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutParameterRequest = PutParameterRequest 
  { "Name" :: (PSParameterName)
  , "Description" :: NullOrUndefined.NullOrUndefined (ParameterDescription)
  , "Value" :: (PSParameterValue)
  , "Type" :: (ParameterType)
  , "KeyId" :: NullOrUndefined.NullOrUndefined (ParameterKeyId)
  , "Overwrite" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "AllowedPattern" :: NullOrUndefined.NullOrUndefined (AllowedPattern)
  }
derive instance newtypePutParameterRequest :: Newtype PutParameterRequest _
derive instance repGenericPutParameterRequest :: Generic PutParameterRequest _
instance showPutParameterRequest :: Show PutParameterRequest where
  show = genericShow
instance decodePutParameterRequest :: Decode PutParameterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutParameterRequest :: Encode PutParameterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutParameterResult = PutParameterResult 
  { "Version" :: NullOrUndefined.NullOrUndefined (PSParameterVersion)
  }
derive instance newtypePutParameterResult :: Newtype PutParameterResult _
derive instance repGenericPutParameterResult :: Generic PutParameterResult _
instance showPutParameterResult :: Show PutParameterResult where
  show = genericShow
instance decodePutParameterResult :: Decode PutParameterResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutParameterResult :: Encode PutParameterResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterDefaultPatchBaselineRequest = RegisterDefaultPatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  }
derive instance newtypeRegisterDefaultPatchBaselineRequest :: Newtype RegisterDefaultPatchBaselineRequest _
derive instance repGenericRegisterDefaultPatchBaselineRequest :: Generic RegisterDefaultPatchBaselineRequest _
instance showRegisterDefaultPatchBaselineRequest :: Show RegisterDefaultPatchBaselineRequest where
  show = genericShow
instance decodeRegisterDefaultPatchBaselineRequest :: Decode RegisterDefaultPatchBaselineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterDefaultPatchBaselineRequest :: Encode RegisterDefaultPatchBaselineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterDefaultPatchBaselineResult = RegisterDefaultPatchBaselineResult 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  }
derive instance newtypeRegisterDefaultPatchBaselineResult :: Newtype RegisterDefaultPatchBaselineResult _
derive instance repGenericRegisterDefaultPatchBaselineResult :: Generic RegisterDefaultPatchBaselineResult _
instance showRegisterDefaultPatchBaselineResult :: Show RegisterDefaultPatchBaselineResult where
  show = genericShow
instance decodeRegisterDefaultPatchBaselineResult :: Decode RegisterDefaultPatchBaselineResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterDefaultPatchBaselineResult :: Encode RegisterDefaultPatchBaselineResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterPatchBaselineForPatchGroupRequest = RegisterPatchBaselineForPatchGroupRequest 
  { "BaselineId" :: (BaselineId)
  , "PatchGroup" :: (PatchGroup)
  }
derive instance newtypeRegisterPatchBaselineForPatchGroupRequest :: Newtype RegisterPatchBaselineForPatchGroupRequest _
derive instance repGenericRegisterPatchBaselineForPatchGroupRequest :: Generic RegisterPatchBaselineForPatchGroupRequest _
instance showRegisterPatchBaselineForPatchGroupRequest :: Show RegisterPatchBaselineForPatchGroupRequest where
  show = genericShow
instance decodeRegisterPatchBaselineForPatchGroupRequest :: Decode RegisterPatchBaselineForPatchGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterPatchBaselineForPatchGroupRequest :: Encode RegisterPatchBaselineForPatchGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterPatchBaselineForPatchGroupResult = RegisterPatchBaselineForPatchGroupResult 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  , "PatchGroup" :: NullOrUndefined.NullOrUndefined (PatchGroup)
  }
derive instance newtypeRegisterPatchBaselineForPatchGroupResult :: Newtype RegisterPatchBaselineForPatchGroupResult _
derive instance repGenericRegisterPatchBaselineForPatchGroupResult :: Generic RegisterPatchBaselineForPatchGroupResult _
instance showRegisterPatchBaselineForPatchGroupResult :: Show RegisterPatchBaselineForPatchGroupResult where
  show = genericShow
instance decodeRegisterPatchBaselineForPatchGroupResult :: Decode RegisterPatchBaselineForPatchGroupResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterPatchBaselineForPatchGroupResult :: Encode RegisterPatchBaselineForPatchGroupResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterTargetWithMaintenanceWindowRequest = RegisterTargetWithMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "ResourceType" :: (MaintenanceWindowResourceType)
  , "Targets" :: (Targets)
  , "OwnerInformation" :: NullOrUndefined.NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  , "ClientToken" :: NullOrUndefined.NullOrUndefined (ClientToken)
  }
derive instance newtypeRegisterTargetWithMaintenanceWindowRequest :: Newtype RegisterTargetWithMaintenanceWindowRequest _
derive instance repGenericRegisterTargetWithMaintenanceWindowRequest :: Generic RegisterTargetWithMaintenanceWindowRequest _
instance showRegisterTargetWithMaintenanceWindowRequest :: Show RegisterTargetWithMaintenanceWindowRequest where
  show = genericShow
instance decodeRegisterTargetWithMaintenanceWindowRequest :: Decode RegisterTargetWithMaintenanceWindowRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterTargetWithMaintenanceWindowRequest :: Encode RegisterTargetWithMaintenanceWindowRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterTargetWithMaintenanceWindowResult = RegisterTargetWithMaintenanceWindowResult 
  { "WindowTargetId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTargetId)
  }
derive instance newtypeRegisterTargetWithMaintenanceWindowResult :: Newtype RegisterTargetWithMaintenanceWindowResult _
derive instance repGenericRegisterTargetWithMaintenanceWindowResult :: Generic RegisterTargetWithMaintenanceWindowResult _
instance showRegisterTargetWithMaintenanceWindowResult :: Show RegisterTargetWithMaintenanceWindowResult where
  show = genericShow
instance decodeRegisterTargetWithMaintenanceWindowResult :: Decode RegisterTargetWithMaintenanceWindowResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterTargetWithMaintenanceWindowResult :: Encode RegisterTargetWithMaintenanceWindowResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterTaskWithMaintenanceWindowRequest = RegisterTaskWithMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Targets" :: (Targets)
  , "TaskArn" :: (MaintenanceWindowTaskArn)
  , "ServiceRoleArn" :: (ServiceRole)
  , "TaskType" :: (MaintenanceWindowTaskType)
  , "TaskParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskParameters)
  , "TaskInvocationParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskInvocationParameters)
  , "Priority" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: (MaxConcurrency)
  , "MaxErrors" :: (MaxErrors)
  , "LoggingInfo" :: NullOrUndefined.NullOrUndefined (LoggingInfo)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  , "ClientToken" :: NullOrUndefined.NullOrUndefined (ClientToken)
  }
derive instance newtypeRegisterTaskWithMaintenanceWindowRequest :: Newtype RegisterTaskWithMaintenanceWindowRequest _
derive instance repGenericRegisterTaskWithMaintenanceWindowRequest :: Generic RegisterTaskWithMaintenanceWindowRequest _
instance showRegisterTaskWithMaintenanceWindowRequest :: Show RegisterTaskWithMaintenanceWindowRequest where
  show = genericShow
instance decodeRegisterTaskWithMaintenanceWindowRequest :: Decode RegisterTaskWithMaintenanceWindowRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterTaskWithMaintenanceWindowRequest :: Encode RegisterTaskWithMaintenanceWindowRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterTaskWithMaintenanceWindowResult = RegisterTaskWithMaintenanceWindowResult 
  { "WindowTaskId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskId)
  }
derive instance newtypeRegisterTaskWithMaintenanceWindowResult :: Newtype RegisterTaskWithMaintenanceWindowResult _
derive instance repGenericRegisterTaskWithMaintenanceWindowResult :: Generic RegisterTaskWithMaintenanceWindowResult _
instance showRegisterTaskWithMaintenanceWindowResult :: Show RegisterTaskWithMaintenanceWindowResult where
  show = genericShow
instance decodeRegisterTaskWithMaintenanceWindowResult :: Decode RegisterTaskWithMaintenanceWindowResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterTaskWithMaintenanceWindowResult :: Encode RegisterTaskWithMaintenanceWindowResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegistrationLimit = RegistrationLimit Int
derive instance newtypeRegistrationLimit :: Newtype RegistrationLimit _
derive instance repGenericRegistrationLimit :: Generic RegistrationLimit _
instance showRegistrationLimit :: Show RegistrationLimit where
  show = genericShow
instance decodeRegistrationLimit :: Decode RegistrationLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistrationLimit :: Encode RegistrationLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegistrationsCount = RegistrationsCount Int
derive instance newtypeRegistrationsCount :: Newtype RegistrationsCount _
derive instance repGenericRegistrationsCount :: Generic RegistrationsCount _
instance showRegistrationsCount :: Show RegistrationsCount where
  show = genericShow
instance decodeRegistrationsCount :: Decode RegistrationsCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistrationsCount :: Encode RegistrationsCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsFromResourceRequest = RemoveTagsFromResourceRequest 
  { "ResourceType" :: (ResourceTypeForTagging)
  , "ResourceId" :: (ResourceId)
  , "TagKeys" :: (KeyList)
  }
derive instance newtypeRemoveTagsFromResourceRequest :: Newtype RemoveTagsFromResourceRequest _
derive instance repGenericRemoveTagsFromResourceRequest :: Generic RemoveTagsFromResourceRequest _
instance showRemoveTagsFromResourceRequest :: Show RemoveTagsFromResourceRequest where
  show = genericShow
instance decodeRemoveTagsFromResourceRequest :: Decode RemoveTagsFromResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceRequest :: Encode RemoveTagsFromResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsFromResourceResult = RemoveTagsFromResourceResult Types.NoArguments
derive instance newtypeRemoveTagsFromResourceResult :: Newtype RemoveTagsFromResourceResult _
derive instance repGenericRemoveTagsFromResourceResult :: Generic RemoveTagsFromResourceResult _
instance showRemoveTagsFromResourceResult :: Show RemoveTagsFromResourceResult where
  show = genericShow
instance decodeRemoveTagsFromResourceResult :: Decode RemoveTagsFromResourceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceResult :: Encode RemoveTagsFromResourceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about targets that resolved during the Automation execution.</p>
newtype ResolvedTargets = ResolvedTargets 
  { "ParameterValues" :: NullOrUndefined.NullOrUndefined (TargetParameterList)
  , "Truncated" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeResolvedTargets :: Newtype ResolvedTargets _
derive instance repGenericResolvedTargets :: Generic ResolvedTargets _
instance showResolvedTargets :: Show ResolvedTargets where
  show = genericShow
instance decodeResolvedTargets :: Decode ResolvedTargets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResolvedTargets :: Encode ResolvedTargets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Compliance summary information for a specific resource. </p>
newtype ResourceComplianceSummaryItem = ResourceComplianceSummaryItem 
  { "ComplianceType" :: NullOrUndefined.NullOrUndefined (ComplianceTypeName)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (ComplianceResourceType)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (ComplianceResourceId)
  , "Status" :: NullOrUndefined.NullOrUndefined (ComplianceStatus)
  , "OverallSeverity" :: NullOrUndefined.NullOrUndefined (ComplianceSeverity)
  , "ExecutionSummary" :: NullOrUndefined.NullOrUndefined (ComplianceExecutionSummary)
  , "CompliantSummary" :: NullOrUndefined.NullOrUndefined (CompliantSummary)
  , "NonCompliantSummary" :: NullOrUndefined.NullOrUndefined (NonCompliantSummary)
  }
derive instance newtypeResourceComplianceSummaryItem :: Newtype ResourceComplianceSummaryItem _
derive instance repGenericResourceComplianceSummaryItem :: Generic ResourceComplianceSummaryItem _
instance showResourceComplianceSummaryItem :: Show ResourceComplianceSummaryItem where
  show = genericShow
instance decodeResourceComplianceSummaryItem :: Decode ResourceComplianceSummaryItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceComplianceSummaryItem :: Encode ResourceComplianceSummaryItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceComplianceSummaryItemList = ResourceComplianceSummaryItemList (Array ResourceComplianceSummaryItem)
derive instance newtypeResourceComplianceSummaryItemList :: Newtype ResourceComplianceSummaryItemList _
derive instance repGenericResourceComplianceSummaryItemList :: Generic ResourceComplianceSummaryItemList _
instance showResourceComplianceSummaryItemList :: Show ResourceComplianceSummaryItemList where
  show = genericShow
instance decodeResourceComplianceSummaryItemList :: Decode ResourceComplianceSummaryItemList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceComplianceSummaryItemList :: Encode ResourceComplianceSummaryItemList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDataSyncAWSKMSKeyARN = ResourceDataSyncAWSKMSKeyARN String
derive instance newtypeResourceDataSyncAWSKMSKeyARN :: Newtype ResourceDataSyncAWSKMSKeyARN _
derive instance repGenericResourceDataSyncAWSKMSKeyARN :: Generic ResourceDataSyncAWSKMSKeyARN _
instance showResourceDataSyncAWSKMSKeyARN :: Show ResourceDataSyncAWSKMSKeyARN where
  show = genericShow
instance decodeResourceDataSyncAWSKMSKeyARN :: Decode ResourceDataSyncAWSKMSKeyARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncAWSKMSKeyARN :: Encode ResourceDataSyncAWSKMSKeyARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A sync configuration with the same name already exists.</p>
newtype ResourceDataSyncAlreadyExistsException = ResourceDataSyncAlreadyExistsException 
  { "SyncName" :: NullOrUndefined.NullOrUndefined (ResourceDataSyncName)
  }
derive instance newtypeResourceDataSyncAlreadyExistsException :: Newtype ResourceDataSyncAlreadyExistsException _
derive instance repGenericResourceDataSyncAlreadyExistsException :: Generic ResourceDataSyncAlreadyExistsException _
instance showResourceDataSyncAlreadyExistsException :: Show ResourceDataSyncAlreadyExistsException where
  show = genericShow
instance decodeResourceDataSyncAlreadyExistsException :: Decode ResourceDataSyncAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncAlreadyExistsException :: Encode ResourceDataSyncAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have exceeded the allowed maximum sync configurations.</p>
newtype ResourceDataSyncCountExceededException = ResourceDataSyncCountExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceDataSyncCountExceededException :: Newtype ResourceDataSyncCountExceededException _
derive instance repGenericResourceDataSyncCountExceededException :: Generic ResourceDataSyncCountExceededException _
instance showResourceDataSyncCountExceededException :: Show ResourceDataSyncCountExceededException where
  show = genericShow
instance decodeResourceDataSyncCountExceededException :: Decode ResourceDataSyncCountExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncCountExceededException :: Encode ResourceDataSyncCountExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDataSyncCreatedTime = ResourceDataSyncCreatedTime Number
derive instance newtypeResourceDataSyncCreatedTime :: Newtype ResourceDataSyncCreatedTime _
derive instance repGenericResourceDataSyncCreatedTime :: Generic ResourceDataSyncCreatedTime _
instance showResourceDataSyncCreatedTime :: Show ResourceDataSyncCreatedTime where
  show = genericShow
instance decodeResourceDataSyncCreatedTime :: Decode ResourceDataSyncCreatedTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncCreatedTime :: Encode ResourceDataSyncCreatedTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified sync configuration is invalid.</p>
newtype ResourceDataSyncInvalidConfigurationException = ResourceDataSyncInvalidConfigurationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceDataSyncInvalidConfigurationException :: Newtype ResourceDataSyncInvalidConfigurationException _
derive instance repGenericResourceDataSyncInvalidConfigurationException :: Generic ResourceDataSyncInvalidConfigurationException _
instance showResourceDataSyncInvalidConfigurationException :: Show ResourceDataSyncInvalidConfigurationException where
  show = genericShow
instance decodeResourceDataSyncInvalidConfigurationException :: Decode ResourceDataSyncInvalidConfigurationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncInvalidConfigurationException :: Encode ResourceDataSyncInvalidConfigurationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a Resource Data Sync configuration, including its current status and last successful sync.</p>
newtype ResourceDataSyncItem = ResourceDataSyncItem 
  { "SyncName" :: NullOrUndefined.NullOrUndefined (ResourceDataSyncName)
  , "S3Destination" :: NullOrUndefined.NullOrUndefined (ResourceDataSyncS3Destination)
  , "LastSyncTime" :: NullOrUndefined.NullOrUndefined (LastResourceDataSyncTime)
  , "LastSuccessfulSyncTime" :: NullOrUndefined.NullOrUndefined (LastSuccessfulResourceDataSyncTime)
  , "LastStatus" :: NullOrUndefined.NullOrUndefined (LastResourceDataSyncStatus)
  , "SyncCreatedTime" :: NullOrUndefined.NullOrUndefined (ResourceDataSyncCreatedTime)
  }
derive instance newtypeResourceDataSyncItem :: Newtype ResourceDataSyncItem _
derive instance repGenericResourceDataSyncItem :: Generic ResourceDataSyncItem _
instance showResourceDataSyncItem :: Show ResourceDataSyncItem where
  show = genericShow
instance decodeResourceDataSyncItem :: Decode ResourceDataSyncItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncItem :: Encode ResourceDataSyncItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDataSyncItemList = ResourceDataSyncItemList (Array ResourceDataSyncItem)
derive instance newtypeResourceDataSyncItemList :: Newtype ResourceDataSyncItemList _
derive instance repGenericResourceDataSyncItemList :: Generic ResourceDataSyncItemList _
instance showResourceDataSyncItemList :: Show ResourceDataSyncItemList where
  show = genericShow
instance decodeResourceDataSyncItemList :: Decode ResourceDataSyncItemList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncItemList :: Encode ResourceDataSyncItemList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDataSyncName = ResourceDataSyncName String
derive instance newtypeResourceDataSyncName :: Newtype ResourceDataSyncName _
derive instance repGenericResourceDataSyncName :: Generic ResourceDataSyncName _
instance showResourceDataSyncName :: Show ResourceDataSyncName where
  show = genericShow
instance decodeResourceDataSyncName :: Decode ResourceDataSyncName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncName :: Encode ResourceDataSyncName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified sync name was not found.</p>
newtype ResourceDataSyncNotFoundException = ResourceDataSyncNotFoundException 
  { "SyncName" :: NullOrUndefined.NullOrUndefined (ResourceDataSyncName)
  }
derive instance newtypeResourceDataSyncNotFoundException :: Newtype ResourceDataSyncNotFoundException _
derive instance repGenericResourceDataSyncNotFoundException :: Generic ResourceDataSyncNotFoundException _
instance showResourceDataSyncNotFoundException :: Show ResourceDataSyncNotFoundException where
  show = genericShow
instance decodeResourceDataSyncNotFoundException :: Decode ResourceDataSyncNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncNotFoundException :: Encode ResourceDataSyncNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDataSyncS3BucketName = ResourceDataSyncS3BucketName String
derive instance newtypeResourceDataSyncS3BucketName :: Newtype ResourceDataSyncS3BucketName _
derive instance repGenericResourceDataSyncS3BucketName :: Generic ResourceDataSyncS3BucketName _
instance showResourceDataSyncS3BucketName :: Show ResourceDataSyncS3BucketName where
  show = genericShow
instance decodeResourceDataSyncS3BucketName :: Decode ResourceDataSyncS3BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncS3BucketName :: Encode ResourceDataSyncS3BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the target Amazon S3 bucket for the Resource Data Sync.</p>
newtype ResourceDataSyncS3Destination = ResourceDataSyncS3Destination 
  { "BucketName" :: (ResourceDataSyncS3BucketName)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (ResourceDataSyncS3Prefix)
  , "SyncFormat" :: (ResourceDataSyncS3Format)
  , "Region" :: (ResourceDataSyncS3Region)
  , "AWSKMSKeyARN" :: NullOrUndefined.NullOrUndefined (ResourceDataSyncAWSKMSKeyARN)
  }
derive instance newtypeResourceDataSyncS3Destination :: Newtype ResourceDataSyncS3Destination _
derive instance repGenericResourceDataSyncS3Destination :: Generic ResourceDataSyncS3Destination _
instance showResourceDataSyncS3Destination :: Show ResourceDataSyncS3Destination where
  show = genericShow
instance decodeResourceDataSyncS3Destination :: Decode ResourceDataSyncS3Destination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncS3Destination :: Encode ResourceDataSyncS3Destination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDataSyncS3Format = ResourceDataSyncS3Format String
derive instance newtypeResourceDataSyncS3Format :: Newtype ResourceDataSyncS3Format _
derive instance repGenericResourceDataSyncS3Format :: Generic ResourceDataSyncS3Format _
instance showResourceDataSyncS3Format :: Show ResourceDataSyncS3Format where
  show = genericShow
instance decodeResourceDataSyncS3Format :: Decode ResourceDataSyncS3Format where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncS3Format :: Encode ResourceDataSyncS3Format where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDataSyncS3Prefix = ResourceDataSyncS3Prefix String
derive instance newtypeResourceDataSyncS3Prefix :: Newtype ResourceDataSyncS3Prefix _
derive instance repGenericResourceDataSyncS3Prefix :: Generic ResourceDataSyncS3Prefix _
instance showResourceDataSyncS3Prefix :: Show ResourceDataSyncS3Prefix where
  show = genericShow
instance decodeResourceDataSyncS3Prefix :: Decode ResourceDataSyncS3Prefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncS3Prefix :: Encode ResourceDataSyncS3Prefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDataSyncS3Region = ResourceDataSyncS3Region String
derive instance newtypeResourceDataSyncS3Region :: Newtype ResourceDataSyncS3Region _
derive instance repGenericResourceDataSyncS3Region :: Generic ResourceDataSyncS3Region _
instance showResourceDataSyncS3Region :: Show ResourceDataSyncS3Region where
  show = genericShow
instance decodeResourceDataSyncS3Region :: Decode ResourceDataSyncS3Region where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDataSyncS3Region :: Encode ResourceDataSyncS3Region where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _
derive instance repGenericResourceId :: Generic ResourceId _
instance showResourceId :: Show ResourceId where
  show = genericShow
instance decodeResourceId :: Decode ResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceId :: Encode ResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Error returned if an attempt is made to delete a patch baseline that is registered for a patch group.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _
derive instance repGenericResourceInUseException :: Generic ResourceInUseException _
instance showResourceInUseException :: Show ResourceInUseException where
  show = genericShow
instance decodeResourceInUseException :: Decode ResourceInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceInUseException :: Encode ResourceInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Error returned when the caller has exceeded the default resource limits. For example, too many Maintenance Windows or Patch baselines have been created.</p> <p>For information about resource limits in Systems Manager, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_ssm">AWS Systems Manager Limits</a>.</p>
newtype ResourceLimitExceededException = ResourceLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeResourceLimitExceededException :: Newtype ResourceLimitExceededException _
derive instance repGenericResourceLimitExceededException :: Generic ResourceLimitExceededException _
instance showResourceLimitExceededException :: Show ResourceLimitExceededException where
  show = genericShow
instance decodeResourceLimitExceededException :: Decode ResourceLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceLimitExceededException :: Encode ResourceLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _
derive instance repGenericResourceType :: Generic ResourceType _
instance showResourceType :: Show ResourceType where
  show = genericShow
instance decodeResourceType :: Decode ResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceType :: Encode ResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceTypeForTagging = ResourceTypeForTagging String
derive instance newtypeResourceTypeForTagging :: Newtype ResourceTypeForTagging _
derive instance repGenericResourceTypeForTagging :: Generic ResourceTypeForTagging _
instance showResourceTypeForTagging :: Show ResourceTypeForTagging where
  show = genericShow
instance decodeResourceTypeForTagging :: Decode ResourceTypeForTagging where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTypeForTagging :: Encode ResourceTypeForTagging where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResponseCode = ResponseCode Int
derive instance newtypeResponseCode :: Newtype ResponseCode _
derive instance repGenericResponseCode :: Generic ResponseCode _
instance showResponseCode :: Show ResponseCode where
  show = genericShow
instance decodeResponseCode :: Decode ResponseCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResponseCode :: Encode ResponseCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The inventory item result attribute.</p>
newtype ResultAttribute = ResultAttribute 
  { "TypeName" :: (InventoryItemTypeName)
  }
derive instance newtypeResultAttribute :: Newtype ResultAttribute _
derive instance repGenericResultAttribute :: Generic ResultAttribute _
instance showResultAttribute :: Show ResultAttribute where
  show = genericShow
instance decodeResultAttribute :: Decode ResultAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResultAttribute :: Encode ResultAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResultAttributeList = ResultAttributeList (Array ResultAttribute)
derive instance newtypeResultAttributeList :: Newtype ResultAttributeList _
derive instance repGenericResultAttributeList :: Generic ResultAttributeList _
instance showResultAttributeList :: Show ResultAttributeList where
  show = genericShow
instance decodeResultAttributeList :: Decode ResultAttributeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResultAttributeList :: Encode ResultAttributeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3BucketName = S3BucketName String
derive instance newtypeS3BucketName :: Newtype S3BucketName _
derive instance repGenericS3BucketName :: Generic S3BucketName _
instance showS3BucketName :: Show S3BucketName where
  show = genericShow
instance decodeS3BucketName :: Decode S3BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3BucketName :: Encode S3BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3KeyPrefix = S3KeyPrefix String
derive instance newtypeS3KeyPrefix :: Newtype S3KeyPrefix _
derive instance repGenericS3KeyPrefix :: Generic S3KeyPrefix _
instance showS3KeyPrefix :: Show S3KeyPrefix where
  show = genericShow
instance decodeS3KeyPrefix :: Decode S3KeyPrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3KeyPrefix :: Encode S3KeyPrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An Amazon S3 bucket where you want to store the results of this request.</p>
newtype S3OutputLocation = S3OutputLocation 
  { "OutputS3Region" :: NullOrUndefined.NullOrUndefined (S3Region)
  , "OutputS3BucketName" :: NullOrUndefined.NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined.NullOrUndefined (S3KeyPrefix)
  }
derive instance newtypeS3OutputLocation :: Newtype S3OutputLocation _
derive instance repGenericS3OutputLocation :: Generic S3OutputLocation _
instance showS3OutputLocation :: Show S3OutputLocation where
  show = genericShow
instance decodeS3OutputLocation :: Decode S3OutputLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3OutputLocation :: Encode S3OutputLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A URL for the Amazon S3 bucket where you want to store the results of this request.</p>
newtype S3OutputUrl = S3OutputUrl 
  { "OutputUrl" :: NullOrUndefined.NullOrUndefined (Url)
  }
derive instance newtypeS3OutputUrl :: Newtype S3OutputUrl _
derive instance repGenericS3OutputUrl :: Generic S3OutputUrl _
instance showS3OutputUrl :: Show S3OutputUrl where
  show = genericShow
instance decodeS3OutputUrl :: Decode S3OutputUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3OutputUrl :: Encode S3OutputUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3Region = S3Region String
derive instance newtypeS3Region :: Newtype S3Region _
derive instance repGenericS3Region :: Generic S3Region _
instance showS3Region :: Show S3Region where
  show = genericShow
instance decodeS3Region :: Decode S3Region where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Region :: Encode S3Region where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScheduleExpression = ScheduleExpression String
derive instance newtypeScheduleExpression :: Newtype ScheduleExpression _
derive instance repGenericScheduleExpression :: Generic ScheduleExpression _
instance showScheduleExpression :: Show ScheduleExpression where
  show = genericShow
instance decodeScheduleExpression :: Decode ScheduleExpression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleExpression :: Encode ScheduleExpression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendAutomationSignalRequest = SendAutomationSignalRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  , "SignalType" :: (SignalType)
  , "Payload" :: NullOrUndefined.NullOrUndefined (AutomationParameterMap)
  }
derive instance newtypeSendAutomationSignalRequest :: Newtype SendAutomationSignalRequest _
derive instance repGenericSendAutomationSignalRequest :: Generic SendAutomationSignalRequest _
instance showSendAutomationSignalRequest :: Show SendAutomationSignalRequest where
  show = genericShow
instance decodeSendAutomationSignalRequest :: Decode SendAutomationSignalRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendAutomationSignalRequest :: Encode SendAutomationSignalRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendAutomationSignalResult = SendAutomationSignalResult Types.NoArguments
derive instance newtypeSendAutomationSignalResult :: Newtype SendAutomationSignalResult _
derive instance repGenericSendAutomationSignalResult :: Generic SendAutomationSignalResult _
instance showSendAutomationSignalResult :: Show SendAutomationSignalResult where
  show = genericShow
instance decodeSendAutomationSignalResult :: Decode SendAutomationSignalResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendAutomationSignalResult :: Encode SendAutomationSignalResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendCommandRequest = SendCommandRequest 
  { "InstanceIds" :: NullOrUndefined.NullOrUndefined (InstanceIdList)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "DocumentName" :: (DocumentARN)
  , "DocumentHash" :: NullOrUndefined.NullOrUndefined (DocumentHash)
  , "DocumentHashType" :: NullOrUndefined.NullOrUndefined (DocumentHashType)
  , "TimeoutSeconds" :: NullOrUndefined.NullOrUndefined (TimeoutSeconds)
  , "Comment" :: NullOrUndefined.NullOrUndefined (Comment)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "OutputS3Region" :: NullOrUndefined.NullOrUndefined (S3Region)
  , "OutputS3BucketName" :: NullOrUndefined.NullOrUndefined (S3BucketName)
  , "OutputS3KeyPrefix" :: NullOrUndefined.NullOrUndefined (S3KeyPrefix)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  , "ServiceRoleArn" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "NotificationConfig" :: NullOrUndefined.NullOrUndefined (NotificationConfig)
  }
derive instance newtypeSendCommandRequest :: Newtype SendCommandRequest _
derive instance repGenericSendCommandRequest :: Generic SendCommandRequest _
instance showSendCommandRequest :: Show SendCommandRequest where
  show = genericShow
instance decodeSendCommandRequest :: Decode SendCommandRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendCommandRequest :: Encode SendCommandRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendCommandResult = SendCommandResult 
  { "Command" :: NullOrUndefined.NullOrUndefined (Command)
  }
derive instance newtypeSendCommandResult :: Newtype SendCommandResult _
derive instance repGenericSendCommandResult :: Generic SendCommandResult _
instance showSendCommandResult :: Show SendCommandResult where
  show = genericShow
instance decodeSendCommandResult :: Decode SendCommandResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendCommandResult :: Encode SendCommandResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServiceRole = ServiceRole String
derive instance newtypeServiceRole :: Newtype ServiceRole _
derive instance repGenericServiceRole :: Generic ServiceRole _
instance showServiceRole :: Show ServiceRole where
  show = genericShow
instance decodeServiceRole :: Decode ServiceRole where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceRole :: Encode ServiceRole where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of managed instances found for each patch severity level defined in the request filter.</p>
newtype SeveritySummary = SeveritySummary 
  { "CriticalCount" :: NullOrUndefined.NullOrUndefined (ComplianceSummaryCount)
  , "HighCount" :: NullOrUndefined.NullOrUndefined (ComplianceSummaryCount)
  , "MediumCount" :: NullOrUndefined.NullOrUndefined (ComplianceSummaryCount)
  , "LowCount" :: NullOrUndefined.NullOrUndefined (ComplianceSummaryCount)
  , "InformationalCount" :: NullOrUndefined.NullOrUndefined (ComplianceSummaryCount)
  , "UnspecifiedCount" :: NullOrUndefined.NullOrUndefined (ComplianceSummaryCount)
  }
derive instance newtypeSeveritySummary :: Newtype SeveritySummary _
derive instance repGenericSeveritySummary :: Generic SeveritySummary _
instance showSeveritySummary :: Show SeveritySummary where
  show = genericShow
instance decodeSeveritySummary :: Decode SeveritySummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSeveritySummary :: Encode SeveritySummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SignalType = SignalType String
derive instance newtypeSignalType :: Newtype SignalType _
derive instance repGenericSignalType :: Generic SignalType _
instance showSignalType :: Show SignalType where
  show = genericShow
instance decodeSignalType :: Decode SignalType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignalType :: Encode SignalType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotDownloadUrl = SnapshotDownloadUrl String
derive instance newtypeSnapshotDownloadUrl :: Newtype SnapshotDownloadUrl _
derive instance repGenericSnapshotDownloadUrl :: Generic SnapshotDownloadUrl _
instance showSnapshotDownloadUrl :: Show SnapshotDownloadUrl where
  show = genericShow
instance decodeSnapshotDownloadUrl :: Decode SnapshotDownloadUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotDownloadUrl :: Encode SnapshotDownloadUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotId = SnapshotId String
derive instance newtypeSnapshotId :: Newtype SnapshotId _
derive instance repGenericSnapshotId :: Generic SnapshotId _
instance showSnapshotId :: Show SnapshotId where
  show = genericShow
instance decodeSnapshotId :: Decode SnapshotId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotId :: Encode SnapshotId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StandardErrorContent = StandardErrorContent String
derive instance newtypeStandardErrorContent :: Newtype StandardErrorContent _
derive instance repGenericStandardErrorContent :: Generic StandardErrorContent _
instance showStandardErrorContent :: Show StandardErrorContent where
  show = genericShow
instance decodeStandardErrorContent :: Decode StandardErrorContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStandardErrorContent :: Encode StandardErrorContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StandardOutputContent = StandardOutputContent String
derive instance newtypeStandardOutputContent :: Newtype StandardOutputContent _
derive instance repGenericStandardOutputContent :: Generic StandardOutputContent _
instance showStandardOutputContent :: Show StandardOutputContent where
  show = genericShow
instance decodeStandardOutputContent :: Decode StandardOutputContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStandardOutputContent :: Encode StandardOutputContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartAutomationExecutionRequest = StartAutomationExecutionRequest 
  { "DocumentName" :: (DocumentARN)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (AutomationParameterMap)
  , "ClientToken" :: NullOrUndefined.NullOrUndefined (IdempotencyToken)
  , "Mode" :: NullOrUndefined.NullOrUndefined (ExecutionMode)
  , "TargetParameterName" :: NullOrUndefined.NullOrUndefined (AutomationParameterKey)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  }
derive instance newtypeStartAutomationExecutionRequest :: Newtype StartAutomationExecutionRequest _
derive instance repGenericStartAutomationExecutionRequest :: Generic StartAutomationExecutionRequest _
instance showStartAutomationExecutionRequest :: Show StartAutomationExecutionRequest where
  show = genericShow
instance decodeStartAutomationExecutionRequest :: Decode StartAutomationExecutionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartAutomationExecutionRequest :: Encode StartAutomationExecutionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartAutomationExecutionResult = StartAutomationExecutionResult 
  { "AutomationExecutionId" :: NullOrUndefined.NullOrUndefined (AutomationExecutionId)
  }
derive instance newtypeStartAutomationExecutionResult :: Newtype StartAutomationExecutionResult _
derive instance repGenericStartAutomationExecutionResult :: Generic StartAutomationExecutionResult _
instance showStartAutomationExecutionResult :: Show StartAutomationExecutionResult where
  show = genericShow
instance decodeStartAutomationExecutionResult :: Decode StartAutomationExecutionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartAutomationExecutionResult :: Encode StartAutomationExecutionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusAdditionalInfo = StatusAdditionalInfo String
derive instance newtypeStatusAdditionalInfo :: Newtype StatusAdditionalInfo _
derive instance repGenericStatusAdditionalInfo :: Generic StatusAdditionalInfo _
instance showStatusAdditionalInfo :: Show StatusAdditionalInfo where
  show = genericShow
instance decodeStatusAdditionalInfo :: Decode StatusAdditionalInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusAdditionalInfo :: Encode StatusAdditionalInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusDetails = StatusDetails String
derive instance newtypeStatusDetails :: Newtype StatusDetails _
derive instance repGenericStatusDetails :: Generic StatusDetails _
instance showStatusDetails :: Show StatusDetails where
  show = genericShow
instance decodeStatusDetails :: Decode StatusDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusDetails :: Encode StatusDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusMessage = StatusMessage String
derive instance newtypeStatusMessage :: Newtype StatusMessage _
derive instance repGenericStatusMessage :: Generic StatusMessage _
instance showStatusMessage :: Show StatusMessage where
  show = genericShow
instance decodeStatusMessage :: Decode StatusMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusMessage :: Encode StatusMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusName = StatusName String
derive instance newtypeStatusName :: Newtype StatusName _
derive instance repGenericStatusName :: Generic StatusName _
instance showStatusName :: Show StatusName where
  show = genericShow
instance decodeStatusName :: Decode StatusName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusName :: Encode StatusName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The updated status is the same as the current status.</p>
newtype StatusUnchanged = StatusUnchanged Types.NoArguments
derive instance newtypeStatusUnchanged :: Newtype StatusUnchanged _
derive instance repGenericStatusUnchanged :: Generic StatusUnchanged _
instance showStatusUnchanged :: Show StatusUnchanged where
  show = genericShow
instance decodeStatusUnchanged :: Decode StatusUnchanged where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusUnchanged :: Encode StatusUnchanged where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed information about an the execution state of an Automation step.</p>
newtype StepExecution = StepExecution 
  { "StepName" :: NullOrUndefined.NullOrUndefined (String)
  , "Action" :: NullOrUndefined.NullOrUndefined (AutomationActionName)
  , "TimeoutSeconds" :: NullOrUndefined.NullOrUndefined (Number)
  , "OnFailure" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxAttempts" :: NullOrUndefined.NullOrUndefined (Int)
  , "ExecutionStartTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "ExecutionEndTime" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "StepStatus" :: NullOrUndefined.NullOrUndefined (AutomationExecutionStatus)
  , "ResponseCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Inputs" :: NullOrUndefined.NullOrUndefined (NormalStringMap)
  , "Outputs" :: NullOrUndefined.NullOrUndefined (AutomationParameterMap)
  , "Response" :: NullOrUndefined.NullOrUndefined (String)
  , "FailureMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "FailureDetails" :: NullOrUndefined.NullOrUndefined (FailureDetails)
  , "StepExecutionId" :: NullOrUndefined.NullOrUndefined (String)
  , "OverriddenParameters" :: NullOrUndefined.NullOrUndefined (AutomationParameterMap)
  }
derive instance newtypeStepExecution :: Newtype StepExecution _
derive instance repGenericStepExecution :: Generic StepExecution _
instance showStepExecution :: Show StepExecution where
  show = genericShow
instance decodeStepExecution :: Decode StepExecution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepExecution :: Encode StepExecution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A filter to limit the amount of step execution information returned by the call.</p>
newtype StepExecutionFilter = StepExecutionFilter 
  { "Key" :: (StepExecutionFilterKey)
  , "Values" :: (StepExecutionFilterValueList)
  }
derive instance newtypeStepExecutionFilter :: Newtype StepExecutionFilter _
derive instance repGenericStepExecutionFilter :: Generic StepExecutionFilter _
instance showStepExecutionFilter :: Show StepExecutionFilter where
  show = genericShow
instance decodeStepExecutionFilter :: Decode StepExecutionFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepExecutionFilter :: Encode StepExecutionFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepExecutionFilterKey = StepExecutionFilterKey String
derive instance newtypeStepExecutionFilterKey :: Newtype StepExecutionFilterKey _
derive instance repGenericStepExecutionFilterKey :: Generic StepExecutionFilterKey _
instance showStepExecutionFilterKey :: Show StepExecutionFilterKey where
  show = genericShow
instance decodeStepExecutionFilterKey :: Decode StepExecutionFilterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepExecutionFilterKey :: Encode StepExecutionFilterKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepExecutionFilterList = StepExecutionFilterList (Array StepExecutionFilter)
derive instance newtypeStepExecutionFilterList :: Newtype StepExecutionFilterList _
derive instance repGenericStepExecutionFilterList :: Generic StepExecutionFilterList _
instance showStepExecutionFilterList :: Show StepExecutionFilterList where
  show = genericShow
instance decodeStepExecutionFilterList :: Decode StepExecutionFilterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepExecutionFilterList :: Encode StepExecutionFilterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepExecutionFilterValue = StepExecutionFilterValue String
derive instance newtypeStepExecutionFilterValue :: Newtype StepExecutionFilterValue _
derive instance repGenericStepExecutionFilterValue :: Generic StepExecutionFilterValue _
instance showStepExecutionFilterValue :: Show StepExecutionFilterValue where
  show = genericShow
instance decodeStepExecutionFilterValue :: Decode StepExecutionFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepExecutionFilterValue :: Encode StepExecutionFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepExecutionFilterValueList = StepExecutionFilterValueList (Array StepExecutionFilterValue)
derive instance newtypeStepExecutionFilterValueList :: Newtype StepExecutionFilterValueList _
derive instance repGenericStepExecutionFilterValueList :: Generic StepExecutionFilterValueList _
instance showStepExecutionFilterValueList :: Show StepExecutionFilterValueList where
  show = genericShow
instance decodeStepExecutionFilterValueList :: Decode StepExecutionFilterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepExecutionFilterValueList :: Encode StepExecutionFilterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepExecutionList = StepExecutionList (Array StepExecution)
derive instance newtypeStepExecutionList :: Newtype StepExecutionList _
derive instance repGenericStepExecutionList :: Generic StepExecutionList _
instance showStepExecutionList :: Show StepExecutionList where
  show = genericShow
instance decodeStepExecutionList :: Decode StepExecutionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepExecutionList :: Encode StepExecutionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopAutomationExecutionRequest = StopAutomationExecutionRequest 
  { "AutomationExecutionId" :: (AutomationExecutionId)
  , "Type" :: NullOrUndefined.NullOrUndefined (StopType)
  }
derive instance newtypeStopAutomationExecutionRequest :: Newtype StopAutomationExecutionRequest _
derive instance repGenericStopAutomationExecutionRequest :: Generic StopAutomationExecutionRequest _
instance showStopAutomationExecutionRequest :: Show StopAutomationExecutionRequest where
  show = genericShow
instance decodeStopAutomationExecutionRequest :: Decode StopAutomationExecutionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopAutomationExecutionRequest :: Encode StopAutomationExecutionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopAutomationExecutionResult = StopAutomationExecutionResult Types.NoArguments
derive instance newtypeStopAutomationExecutionResult :: Newtype StopAutomationExecutionResult _
derive instance repGenericStopAutomationExecutionResult :: Generic StopAutomationExecutionResult _
instance showStopAutomationExecutionResult :: Show StopAutomationExecutionResult where
  show = genericShow
instance decodeStopAutomationExecutionResult :: Decode StopAutomationExecutionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopAutomationExecutionResult :: Encode StopAutomationExecutionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopType = StopType String
derive instance newtypeStopType :: Newtype StopType _
derive instance repGenericStopType :: Generic StopType _
instance showStopType :: Show StopType where
  show = genericShow
instance decodeStopType :: Decode StopType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopType :: Encode StopType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringDateTime = StringDateTime String
derive instance newtypeStringDateTime :: Newtype StringDateTime _
derive instance repGenericStringDateTime :: Generic StringDateTime _
instance showStringDateTime :: Show StringDateTime where
  show = genericShow
instance decodeStringDateTime :: Decode StringDateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringDateTime :: Encode StringDateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _
derive instance repGenericStringList :: Generic StringList _
instance showStringList :: Show StringList where
  show = genericShow
instance decodeStringList :: Decode StringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringList :: Encode StringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The sub-type count exceeded the limit for the inventory type.</p>
newtype SubTypeCountLimitExceededException = SubTypeCountLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSubTypeCountLimitExceededException :: Newtype SubTypeCountLimitExceededException _
derive instance repGenericSubTypeCountLimitExceededException :: Generic SubTypeCountLimitExceededException _
instance showSubTypeCountLimitExceededException :: Show SubTypeCountLimitExceededException where
  show = genericShow
instance decodeSubTypeCountLimitExceededException :: Decode SubTypeCountLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubTypeCountLimitExceededException :: Encode SubTypeCountLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Metadata that you assign to your AWS resources. Tags enable you to categorize your resources in different ways, for example, by purpose, owner, or environment. In Systems Manager, you can apply tags to documents, managed instances, Maintenance Windows, Parameter Store parameters, and patch baselines.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An array of search criteria that targets instances using a Key,Value combination that you specify. <code>Targets</code> is required if you don't provide one or more instance IDs in the call.</p> <p/>
newtype Target = Target 
  { "Key" :: NullOrUndefined.NullOrUndefined (TargetKey)
  , "Values" :: NullOrUndefined.NullOrUndefined (TargetValues)
  }
derive instance newtypeTarget :: Newtype Target _
derive instance repGenericTarget :: Generic Target _
instance showTarget :: Show Target where
  show = genericShow
instance decodeTarget :: Decode Target where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTarget :: Encode Target where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetCount = TargetCount Int
derive instance newtypeTargetCount :: Newtype TargetCount _
derive instance repGenericTargetCount :: Generic TargetCount _
instance showTargetCount :: Show TargetCount where
  show = genericShow
instance decodeTargetCount :: Decode TargetCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetCount :: Encode TargetCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You specified the <code>Safe</code> option for the DeregisterTargetFromMaintenanceWindow operation, but the target is still referenced in a task.</p>
newtype TargetInUseException = TargetInUseException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTargetInUseException :: Newtype TargetInUseException _
derive instance repGenericTargetInUseException :: Generic TargetInUseException _
instance showTargetInUseException :: Show TargetInUseException where
  show = genericShow
instance decodeTargetInUseException :: Decode TargetInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetInUseException :: Encode TargetInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetKey = TargetKey String
derive instance newtypeTargetKey :: Newtype TargetKey _
derive instance repGenericTargetKey :: Generic TargetKey _
instance showTargetKey :: Show TargetKey where
  show = genericShow
instance decodeTargetKey :: Decode TargetKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetKey :: Encode TargetKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetParameterList = TargetParameterList (Array ParameterValue)
derive instance newtypeTargetParameterList :: Newtype TargetParameterList _
derive instance repGenericTargetParameterList :: Generic TargetParameterList _
instance showTargetParameterList :: Show TargetParameterList where
  show = genericShow
instance decodeTargetParameterList :: Decode TargetParameterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetParameterList :: Encode TargetParameterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetType = TargetType String
derive instance newtypeTargetType :: Newtype TargetType _
derive instance repGenericTargetType :: Generic TargetType _
instance showTargetType :: Show TargetType where
  show = genericShow
instance decodeTargetType :: Decode TargetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetType :: Encode TargetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetValue = TargetValue String
derive instance newtypeTargetValue :: Newtype TargetValue _
derive instance repGenericTargetValue :: Generic TargetValue _
instance showTargetValue :: Show TargetValue where
  show = genericShow
instance decodeTargetValue :: Decode TargetValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetValue :: Encode TargetValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetValues = TargetValues (Array TargetValue)
derive instance newtypeTargetValues :: Newtype TargetValues _
derive instance repGenericTargetValues :: Generic TargetValues _
instance showTargetValues :: Show TargetValues where
  show = genericShow
instance decodeTargetValues :: Decode TargetValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetValues :: Encode TargetValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Targets = Targets (Array Target)
derive instance newtypeTargets :: Newtype Targets _
derive instance repGenericTargets :: Generic Targets _
instance showTargets :: Show Targets where
  show = genericShow
instance decodeTargets :: Decode Targets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargets :: Encode Targets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeoutSeconds = TimeoutSeconds Int
derive instance newtypeTimeoutSeconds :: Newtype TimeoutSeconds _
derive instance repGenericTimeoutSeconds :: Generic TimeoutSeconds _
instance showTimeoutSeconds :: Show TimeoutSeconds where
  show = genericShow
instance decodeTimeoutSeconds :: Decode TimeoutSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeoutSeconds :: Encode TimeoutSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Targets parameter includes too many tags. Remove one or more tags and try the command again.</p>
newtype TooManyTagsError = TooManyTagsError Types.NoArguments
derive instance newtypeTooManyTagsError :: Newtype TooManyTagsError _
derive instance repGenericTooManyTagsError :: Generic TooManyTagsError _
instance showTooManyTagsError :: Show TooManyTagsError where
  show = genericShow
instance decodeTooManyTagsError :: Decode TooManyTagsError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTagsError :: Encode TooManyTagsError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There are concurrent updates for a resource that supports one update at a time.</p>
newtype TooManyUpdates = TooManyUpdates 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTooManyUpdates :: Newtype TooManyUpdates _
derive instance repGenericTooManyUpdates :: Generic TooManyUpdates _
instance showTooManyUpdates :: Show TooManyUpdates where
  show = genericShow
instance decodeTooManyUpdates :: Decode TooManyUpdates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyUpdates :: Encode TooManyUpdates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The size of inventory data has exceeded the total size limit for the resource.</p>
newtype TotalSizeLimitExceededException = TotalSizeLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTotalSizeLimitExceededException :: Newtype TotalSizeLimitExceededException _
derive instance repGenericTotalSizeLimitExceededException :: Generic TotalSizeLimitExceededException _
instance showTotalSizeLimitExceededException :: Show TotalSizeLimitExceededException where
  show = genericShow
instance decodeTotalSizeLimitExceededException :: Decode TotalSizeLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTotalSizeLimitExceededException :: Encode TotalSizeLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Context</code> attribute that you specified for the <code>InventoryItem</code> is not allowed for this inventory type. You can only use the <code>Context</code> attribute with inventory types like <code>AWS:ComplianceItem</code>.</p>
newtype UnsupportedInventoryItemContextException = UnsupportedInventoryItemContextException 
  { "TypeName" :: NullOrUndefined.NullOrUndefined (InventoryItemTypeName)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnsupportedInventoryItemContextException :: Newtype UnsupportedInventoryItemContextException _
derive instance repGenericUnsupportedInventoryItemContextException :: Generic UnsupportedInventoryItemContextException _
instance showUnsupportedInventoryItemContextException :: Show UnsupportedInventoryItemContextException where
  show = genericShow
instance decodeUnsupportedInventoryItemContextException :: Decode UnsupportedInventoryItemContextException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedInventoryItemContextException :: Encode UnsupportedInventoryItemContextException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Inventory item type schema version has to match supported versions in the service. Check output of GetInventorySchema to see the available schema version for each type.</p>
newtype UnsupportedInventorySchemaVersionException = UnsupportedInventorySchemaVersionException 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnsupportedInventorySchemaVersionException :: Newtype UnsupportedInventorySchemaVersionException _
derive instance repGenericUnsupportedInventorySchemaVersionException :: Generic UnsupportedInventorySchemaVersionException _
instance showUnsupportedInventorySchemaVersionException :: Show UnsupportedInventorySchemaVersionException where
  show = genericShow
instance decodeUnsupportedInventorySchemaVersionException :: Decode UnsupportedInventorySchemaVersionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedInventorySchemaVersionException :: Encode UnsupportedInventorySchemaVersionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operating systems you specified is not supported, or the operation is not supported for the operating system. Valid operating systems include: Windows, AmazonLinux, RedhatEnterpriseLinux, and Ubuntu.</p>
newtype UnsupportedOperatingSystem = UnsupportedOperatingSystem 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnsupportedOperatingSystem :: Newtype UnsupportedOperatingSystem _
derive instance repGenericUnsupportedOperatingSystem :: Generic UnsupportedOperatingSystem _
instance showUnsupportedOperatingSystem :: Show UnsupportedOperatingSystem where
  show = genericShow
instance decodeUnsupportedOperatingSystem :: Decode UnsupportedOperatingSystem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedOperatingSystem :: Encode UnsupportedOperatingSystem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The parameter type is not supported.</p>
newtype UnsupportedParameterType = UnsupportedParameterType 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnsupportedParameterType :: Newtype UnsupportedParameterType _
derive instance repGenericUnsupportedParameterType :: Generic UnsupportedParameterType _
instance showUnsupportedParameterType :: Show UnsupportedParameterType where
  show = genericShow
instance decodeUnsupportedParameterType :: Decode UnsupportedParameterType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedParameterType :: Encode UnsupportedParameterType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The document does not support the platform type of the given instance ID(s). For example, you sent an document for a Windows instance to a Linux instance.</p>
newtype UnsupportedPlatformType = UnsupportedPlatformType 
  { "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnsupportedPlatformType :: Newtype UnsupportedPlatformType _
derive instance repGenericUnsupportedPlatformType :: Generic UnsupportedPlatformType _
instance showUnsupportedPlatformType :: Show UnsupportedPlatformType where
  show = genericShow
instance decodeUnsupportedPlatformType :: Decode UnsupportedPlatformType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedPlatformType :: Encode UnsupportedPlatformType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAssociationRequest = UpdateAssociationRequest 
  { "AssociationId" :: (AssociationId)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (ScheduleExpression)
  , "OutputLocation" :: NullOrUndefined.NullOrUndefined (InstanceAssociationOutputLocation)
  , "Name" :: NullOrUndefined.NullOrUndefined (DocumentName)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "AssociationName" :: NullOrUndefined.NullOrUndefined (AssociationName)
  , "AssociationVersion" :: NullOrUndefined.NullOrUndefined (AssociationVersion)
  }
derive instance newtypeUpdateAssociationRequest :: Newtype UpdateAssociationRequest _
derive instance repGenericUpdateAssociationRequest :: Generic UpdateAssociationRequest _
instance showUpdateAssociationRequest :: Show UpdateAssociationRequest where
  show = genericShow
instance decodeUpdateAssociationRequest :: Decode UpdateAssociationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAssociationRequest :: Encode UpdateAssociationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAssociationResult = UpdateAssociationResult 
  { "AssociationDescription" :: NullOrUndefined.NullOrUndefined (AssociationDescription)
  }
derive instance newtypeUpdateAssociationResult :: Newtype UpdateAssociationResult _
derive instance repGenericUpdateAssociationResult :: Generic UpdateAssociationResult _
instance showUpdateAssociationResult :: Show UpdateAssociationResult where
  show = genericShow
instance decodeUpdateAssociationResult :: Decode UpdateAssociationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAssociationResult :: Encode UpdateAssociationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAssociationStatusRequest = UpdateAssociationStatusRequest 
  { "Name" :: (DocumentName)
  , "InstanceId" :: (InstanceId)
  , "AssociationStatus" :: (AssociationStatus)
  }
derive instance newtypeUpdateAssociationStatusRequest :: Newtype UpdateAssociationStatusRequest _
derive instance repGenericUpdateAssociationStatusRequest :: Generic UpdateAssociationStatusRequest _
instance showUpdateAssociationStatusRequest :: Show UpdateAssociationStatusRequest where
  show = genericShow
instance decodeUpdateAssociationStatusRequest :: Decode UpdateAssociationStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAssociationStatusRequest :: Encode UpdateAssociationStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateAssociationStatusResult = UpdateAssociationStatusResult 
  { "AssociationDescription" :: NullOrUndefined.NullOrUndefined (AssociationDescription)
  }
derive instance newtypeUpdateAssociationStatusResult :: Newtype UpdateAssociationStatusResult _
derive instance repGenericUpdateAssociationStatusResult :: Generic UpdateAssociationStatusResult _
instance showUpdateAssociationStatusResult :: Show UpdateAssociationStatusResult where
  show = genericShow
instance decodeUpdateAssociationStatusResult :: Decode UpdateAssociationStatusResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateAssociationStatusResult :: Encode UpdateAssociationStatusResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDocumentDefaultVersionRequest = UpdateDocumentDefaultVersionRequest 
  { "Name" :: (DocumentName)
  , "DocumentVersion" :: (DocumentVersionNumber)
  }
derive instance newtypeUpdateDocumentDefaultVersionRequest :: Newtype UpdateDocumentDefaultVersionRequest _
derive instance repGenericUpdateDocumentDefaultVersionRequest :: Generic UpdateDocumentDefaultVersionRequest _
instance showUpdateDocumentDefaultVersionRequest :: Show UpdateDocumentDefaultVersionRequest where
  show = genericShow
instance decodeUpdateDocumentDefaultVersionRequest :: Decode UpdateDocumentDefaultVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDocumentDefaultVersionRequest :: Encode UpdateDocumentDefaultVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDocumentDefaultVersionResult = UpdateDocumentDefaultVersionResult 
  { "Description" :: NullOrUndefined.NullOrUndefined (DocumentDefaultVersionDescription)
  }
derive instance newtypeUpdateDocumentDefaultVersionResult :: Newtype UpdateDocumentDefaultVersionResult _
derive instance repGenericUpdateDocumentDefaultVersionResult :: Generic UpdateDocumentDefaultVersionResult _
instance showUpdateDocumentDefaultVersionResult :: Show UpdateDocumentDefaultVersionResult where
  show = genericShow
instance decodeUpdateDocumentDefaultVersionResult :: Decode UpdateDocumentDefaultVersionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDocumentDefaultVersionResult :: Encode UpdateDocumentDefaultVersionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDocumentRequest = UpdateDocumentRequest 
  { "Content" :: (DocumentContent)
  , "Name" :: (DocumentName)
  , "DocumentVersion" :: NullOrUndefined.NullOrUndefined (DocumentVersion)
  , "DocumentFormat" :: NullOrUndefined.NullOrUndefined (DocumentFormat)
  , "TargetType" :: NullOrUndefined.NullOrUndefined (TargetType)
  }
derive instance newtypeUpdateDocumentRequest :: Newtype UpdateDocumentRequest _
derive instance repGenericUpdateDocumentRequest :: Generic UpdateDocumentRequest _
instance showUpdateDocumentRequest :: Show UpdateDocumentRequest where
  show = genericShow
instance decodeUpdateDocumentRequest :: Decode UpdateDocumentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDocumentRequest :: Encode UpdateDocumentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDocumentResult = UpdateDocumentResult 
  { "DocumentDescription" :: NullOrUndefined.NullOrUndefined (DocumentDescription)
  }
derive instance newtypeUpdateDocumentResult :: Newtype UpdateDocumentResult _
derive instance repGenericUpdateDocumentResult :: Generic UpdateDocumentResult _
instance showUpdateDocumentResult :: Show UpdateDocumentResult where
  show = genericShow
instance decodeUpdateDocumentResult :: Decode UpdateDocumentResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDocumentResult :: Encode UpdateDocumentResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateMaintenanceWindowRequest = UpdateMaintenanceWindowRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowSchedule)
  , "Duration" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDurationHours)
  , "Cutoff" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowCutoff)
  , "AllowUnassociatedTargets" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowEnabled)
  , "Replace" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeUpdateMaintenanceWindowRequest :: Newtype UpdateMaintenanceWindowRequest _
derive instance repGenericUpdateMaintenanceWindowRequest :: Generic UpdateMaintenanceWindowRequest _
instance showUpdateMaintenanceWindowRequest :: Show UpdateMaintenanceWindowRequest where
  show = genericShow
instance decodeUpdateMaintenanceWindowRequest :: Decode UpdateMaintenanceWindowRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMaintenanceWindowRequest :: Encode UpdateMaintenanceWindowRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateMaintenanceWindowResult = UpdateMaintenanceWindowResult 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowSchedule)
  , "Duration" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDurationHours)
  , "Cutoff" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowCutoff)
  , "AllowUnassociatedTargets" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowAllowUnassociatedTargets)
  , "Enabled" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowEnabled)
  }
derive instance newtypeUpdateMaintenanceWindowResult :: Newtype UpdateMaintenanceWindowResult _
derive instance repGenericUpdateMaintenanceWindowResult :: Generic UpdateMaintenanceWindowResult _
instance showUpdateMaintenanceWindowResult :: Show UpdateMaintenanceWindowResult where
  show = genericShow
instance decodeUpdateMaintenanceWindowResult :: Decode UpdateMaintenanceWindowResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMaintenanceWindowResult :: Encode UpdateMaintenanceWindowResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateMaintenanceWindowTargetRequest = UpdateMaintenanceWindowTargetRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTargetId" :: (MaintenanceWindowTargetId)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "OwnerInformation" :: NullOrUndefined.NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  , "Replace" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeUpdateMaintenanceWindowTargetRequest :: Newtype UpdateMaintenanceWindowTargetRequest _
derive instance repGenericUpdateMaintenanceWindowTargetRequest :: Generic UpdateMaintenanceWindowTargetRequest _
instance showUpdateMaintenanceWindowTargetRequest :: Show UpdateMaintenanceWindowTargetRequest where
  show = genericShow
instance decodeUpdateMaintenanceWindowTargetRequest :: Decode UpdateMaintenanceWindowTargetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMaintenanceWindowTargetRequest :: Encode UpdateMaintenanceWindowTargetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateMaintenanceWindowTargetResult = UpdateMaintenanceWindowTargetResult 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "WindowTargetId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTargetId)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "OwnerInformation" :: NullOrUndefined.NullOrUndefined (OwnerInformation)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  }
derive instance newtypeUpdateMaintenanceWindowTargetResult :: Newtype UpdateMaintenanceWindowTargetResult _
derive instance repGenericUpdateMaintenanceWindowTargetResult :: Generic UpdateMaintenanceWindowTargetResult _
instance showUpdateMaintenanceWindowTargetResult :: Show UpdateMaintenanceWindowTargetResult where
  show = genericShow
instance decodeUpdateMaintenanceWindowTargetResult :: Decode UpdateMaintenanceWindowTargetResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMaintenanceWindowTargetResult :: Encode UpdateMaintenanceWindowTargetResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateMaintenanceWindowTaskRequest = UpdateMaintenanceWindowTaskRequest 
  { "WindowId" :: (MaintenanceWindowId)
  , "WindowTaskId" :: (MaintenanceWindowTaskId)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "TaskArn" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskArn)
  , "ServiceRoleArn" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "TaskParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskParameters)
  , "TaskInvocationParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskInvocationParameters)
  , "Priority" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  , "LoggingInfo" :: NullOrUndefined.NullOrUndefined (LoggingInfo)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  , "Replace" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeUpdateMaintenanceWindowTaskRequest :: Newtype UpdateMaintenanceWindowTaskRequest _
derive instance repGenericUpdateMaintenanceWindowTaskRequest :: Generic UpdateMaintenanceWindowTaskRequest _
instance showUpdateMaintenanceWindowTaskRequest :: Show UpdateMaintenanceWindowTaskRequest where
  show = genericShow
instance decodeUpdateMaintenanceWindowTaskRequest :: Decode UpdateMaintenanceWindowTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMaintenanceWindowTaskRequest :: Encode UpdateMaintenanceWindowTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateMaintenanceWindowTaskResult = UpdateMaintenanceWindowTaskResult 
  { "WindowId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowId)
  , "WindowTaskId" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskId)
  , "Targets" :: NullOrUndefined.NullOrUndefined (Targets)
  , "TaskArn" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskArn)
  , "ServiceRoleArn" :: NullOrUndefined.NullOrUndefined (ServiceRole)
  , "TaskParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskParameters)
  , "TaskInvocationParameters" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskInvocationParameters)
  , "Priority" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowTaskPriority)
  , "MaxConcurrency" :: NullOrUndefined.NullOrUndefined (MaxConcurrency)
  , "MaxErrors" :: NullOrUndefined.NullOrUndefined (MaxErrors)
  , "LoggingInfo" :: NullOrUndefined.NullOrUndefined (LoggingInfo)
  , "Name" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowName)
  , "Description" :: NullOrUndefined.NullOrUndefined (MaintenanceWindowDescription)
  }
derive instance newtypeUpdateMaintenanceWindowTaskResult :: Newtype UpdateMaintenanceWindowTaskResult _
derive instance repGenericUpdateMaintenanceWindowTaskResult :: Generic UpdateMaintenanceWindowTaskResult _
instance showUpdateMaintenanceWindowTaskResult :: Show UpdateMaintenanceWindowTaskResult where
  show = genericShow
instance decodeUpdateMaintenanceWindowTaskResult :: Decode UpdateMaintenanceWindowTaskResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateMaintenanceWindowTaskResult :: Encode UpdateMaintenanceWindowTaskResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateManagedInstanceRoleRequest = UpdateManagedInstanceRoleRequest 
  { "InstanceId" :: (ManagedInstanceId)
  , "IamRole" :: (IamRole)
  }
derive instance newtypeUpdateManagedInstanceRoleRequest :: Newtype UpdateManagedInstanceRoleRequest _
derive instance repGenericUpdateManagedInstanceRoleRequest :: Generic UpdateManagedInstanceRoleRequest _
instance showUpdateManagedInstanceRoleRequest :: Show UpdateManagedInstanceRoleRequest where
  show = genericShow
instance decodeUpdateManagedInstanceRoleRequest :: Decode UpdateManagedInstanceRoleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateManagedInstanceRoleRequest :: Encode UpdateManagedInstanceRoleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateManagedInstanceRoleResult = UpdateManagedInstanceRoleResult Types.NoArguments
derive instance newtypeUpdateManagedInstanceRoleResult :: Newtype UpdateManagedInstanceRoleResult _
derive instance repGenericUpdateManagedInstanceRoleResult :: Generic UpdateManagedInstanceRoleResult _
instance showUpdateManagedInstanceRoleResult :: Show UpdateManagedInstanceRoleResult where
  show = genericShow
instance decodeUpdateManagedInstanceRoleResult :: Decode UpdateManagedInstanceRoleResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateManagedInstanceRoleResult :: Encode UpdateManagedInstanceRoleResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePatchBaselineRequest = UpdatePatchBaselineRequest 
  { "BaselineId" :: (BaselineId)
  , "Name" :: NullOrUndefined.NullOrUndefined (BaselineName)
  , "GlobalFilters" :: NullOrUndefined.NullOrUndefined (PatchFilterGroup)
  , "ApprovalRules" :: NullOrUndefined.NullOrUndefined (PatchRuleGroup)
  , "ApprovedPatches" :: NullOrUndefined.NullOrUndefined (PatchIdList)
  , "ApprovedPatchesComplianceLevel" :: NullOrUndefined.NullOrUndefined (PatchComplianceLevel)
  , "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RejectedPatches" :: NullOrUndefined.NullOrUndefined (PatchIdList)
  , "Description" :: NullOrUndefined.NullOrUndefined (BaselineDescription)
  , "Sources" :: NullOrUndefined.NullOrUndefined (PatchSourceList)
  , "Replace" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeUpdatePatchBaselineRequest :: Newtype UpdatePatchBaselineRequest _
derive instance repGenericUpdatePatchBaselineRequest :: Generic UpdatePatchBaselineRequest _
instance showUpdatePatchBaselineRequest :: Show UpdatePatchBaselineRequest where
  show = genericShow
instance decodeUpdatePatchBaselineRequest :: Decode UpdatePatchBaselineRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePatchBaselineRequest :: Encode UpdatePatchBaselineRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePatchBaselineResult = UpdatePatchBaselineResult 
  { "BaselineId" :: NullOrUndefined.NullOrUndefined (BaselineId)
  , "Name" :: NullOrUndefined.NullOrUndefined (BaselineName)
  , "OperatingSystem" :: NullOrUndefined.NullOrUndefined (OperatingSystem)
  , "GlobalFilters" :: NullOrUndefined.NullOrUndefined (PatchFilterGroup)
  , "ApprovalRules" :: NullOrUndefined.NullOrUndefined (PatchRuleGroup)
  , "ApprovedPatches" :: NullOrUndefined.NullOrUndefined (PatchIdList)
  , "ApprovedPatchesComplianceLevel" :: NullOrUndefined.NullOrUndefined (PatchComplianceLevel)
  , "ApprovedPatchesEnableNonSecurity" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RejectedPatches" :: NullOrUndefined.NullOrUndefined (PatchIdList)
  , "CreatedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "ModifiedDate" :: NullOrUndefined.NullOrUndefined (DateTime)
  , "Description" :: NullOrUndefined.NullOrUndefined (BaselineDescription)
  , "Sources" :: NullOrUndefined.NullOrUndefined (PatchSourceList)
  }
derive instance newtypeUpdatePatchBaselineResult :: Newtype UpdatePatchBaselineResult _
derive instance repGenericUpdatePatchBaselineResult :: Generic UpdatePatchBaselineResult _
instance showUpdatePatchBaselineResult :: Show UpdatePatchBaselineResult where
  show = genericShow
instance decodeUpdatePatchBaselineResult :: Decode UpdatePatchBaselineResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePatchBaselineResult :: Encode UpdatePatchBaselineResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _
derive instance repGenericUrl :: Generic Url _
instance showUrl :: Show Url where
  show = genericShow
instance decodeUrl :: Decode Url where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUrl :: Encode Url where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _
derive instance repGenericVersion :: Generic Version _
instance showVersion :: Show Version where
  show = genericShow
instance decodeVersion :: Decode Version where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersion :: Encode Version where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
