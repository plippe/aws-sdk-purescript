

-- | <fullname>AWS CloudFormation</fullname> <p>AWS CloudFormation allows you to create and manage AWS infrastructure deployments predictably and repeatedly. You can use AWS CloudFormation to leverage AWS products, such as Amazon Elastic Compute Cloud, Amazon Elastic Block Store, Amazon Simple Notification Service, Elastic Load Balancing, and Auto Scaling to build highly-reliable, highly scalable, cost-effective applications without creating or configuring the underlying AWS infrastructure.</p> <p>With AWS CloudFormation, you declare all of your resources and dependencies in a template file. The template defines a collection of resources as a single unit called a stack. AWS CloudFormation creates and deletes all member resources of the stack together and manages all dependencies between the resources for you.</p> <p>For more information about AWS CloudFormation, see the <a href="http://aws.amazon.com/cloudformation/">AWS CloudFormation Product Page</a>.</p> <p>Amazon CloudFormation makes use of other AWS products. If you need additional technical information about a specific AWS product, you can find the product's technical documentation at <a href="http://docs.aws.amazon.com/">docs.aws.amazon.com</a>.</p>
module AWS.CloudFormation where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudFormation" :: String


-- | <p>Cancels an update on the specified stack. If the call completes successfully, the stack rolls back the update and reverts to the previous stack configuration.</p> <note> <p>You can cancel only stacks that are in the UPDATE_IN_PROGRESS state.</p> </note>
cancelUpdateStack :: forall eff. CancelUpdateStackInput -> Aff (err :: AWS.RequestError | eff) Unit
cancelUpdateStack = AWS.request serviceName "CancelUpdateStack" 


-- | <p>For a specified stack that is in the <code>UPDATE_ROLLBACK_FAILED</code> state, continues rolling it back to the <code>UPDATE_ROLLBACK_COMPLETE</code> state. Depending on the cause of the failure, you can manually <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed"> fix the error</a> and continue the rollback. By continuing the rollback, you can return your stack to a working state (the <code>UPDATE_ROLLBACK_COMPLETE</code> state), and then try to update the stack again.</p> <p>A stack goes into the <code>UPDATE_ROLLBACK_FAILED</code> state when AWS CloudFormation cannot roll back all changes after a failed stack update. For example, you might have a stack that is rolling back to an old database instance that was deleted outside of AWS CloudFormation. Because AWS CloudFormation doesn't know the database was deleted, it assumes that the database instance still exists and attempts to roll back to it, causing the update rollback to fail.</p>
continueUpdateRollback :: forall eff. ContinueUpdateRollbackInput -> Aff (err :: AWS.RequestError | eff) ContinueUpdateRollbackOutput
continueUpdateRollback = AWS.request serviceName "ContinueUpdateRollback" 


-- | <p>Creates a list of changes that will be applied to a stack so that you can review the changes before executing them. You can create a change set for a stack that doesn't exist or an existing stack. If you create a change set for a stack that doesn't exist, the change set shows all of the resources that AWS CloudFormation will create. If you create a change set for an existing stack, AWS CloudFormation compares the stack's information with the information that you submit in the change set and lists the differences. Use change sets to understand which resources AWS CloudFormation will create or change, and how it will change resources in an existing stack, before you create or update a stack.</p> <p>To create a change set for a stack that doesn't exist, for the <code>ChangeSetType</code> parameter, specify <code>CREATE</code>. To create a change set for an existing stack, specify <code>UPDATE</code> for the <code>ChangeSetType</code> parameter. After the <code>CreateChangeSet</code> call successfully completes, AWS CloudFormation starts creating the change set. To check the status of the change set or to review it, use the <a>DescribeChangeSet</a> action.</p> <p>When you are satisfied with the changes the change set will make, execute the change set by using the <a>ExecuteChangeSet</a> action. AWS CloudFormation doesn't make changes until you execute the change set.</p>
createChangeSet :: forall eff. CreateChangeSetInput -> Aff (err :: AWS.RequestError | eff) CreateChangeSetOutput
createChangeSet = AWS.request serviceName "CreateChangeSet" 


-- | <p>Creates a stack as specified in the template. After the call completes successfully, the stack creation starts. You can check the status of the stack via the <a>DescribeStacks</a> API.</p>
createStack :: forall eff. CreateStackInput -> Aff (err :: AWS.RequestError | eff) CreateStackOutput
createStack = AWS.request serviceName "CreateStack" 


-- | <p>Creates stack instances for the specified accounts, within the specified regions. A stack instance refers to a stack in a specific account and region. <code>Accounts</code> and <code>Regions</code> are required parameters—you must specify at least one account and one region. </p>
createStackInstances :: forall eff. CreateStackInstancesInput -> Aff (err :: AWS.RequestError | eff) CreateStackInstancesOutput
createStackInstances = AWS.request serviceName "CreateStackInstances" 


-- | <p>Creates a stack set.</p>
createStackSet :: forall eff. CreateStackSetInput -> Aff (err :: AWS.RequestError | eff) CreateStackSetOutput
createStackSet = AWS.request serviceName "CreateStackSet" 


-- | <p>Deletes the specified change set. Deleting change sets ensures that no one executes the wrong change set.</p> <p>If the call successfully completes, AWS CloudFormation successfully deleted the change set.</p>
deleteChangeSet :: forall eff. DeleteChangeSetInput -> Aff (err :: AWS.RequestError | eff) DeleteChangeSetOutput
deleteChangeSet = AWS.request serviceName "DeleteChangeSet" 


-- | <p>Deletes a specified stack. Once the call completes successfully, stack deletion starts. Deleted stacks do not show up in the <a>DescribeStacks</a> API if the deletion has been completed successfully.</p>
deleteStack :: forall eff. DeleteStackInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteStack = AWS.request serviceName "DeleteStack" 


-- | <p>Deletes stack instances for the specified accounts, in the specified regions. </p>
deleteStackInstances :: forall eff. DeleteStackInstancesInput -> Aff (err :: AWS.RequestError | eff) DeleteStackInstancesOutput
deleteStackInstances = AWS.request serviceName "DeleteStackInstances" 


-- | <p>Deletes a stack set. Before you can delete a stack set, all of its member stack instances must be deleted. For more information about how to do this, see <a>DeleteStackInstances</a>. </p>
deleteStackSet :: forall eff. DeleteStackSetInput -> Aff (err :: AWS.RequestError | eff) DeleteStackSetOutput
deleteStackSet = AWS.request serviceName "DeleteStackSet" 


-- | <p>Retrieves your account's AWS CloudFormation limits, such as the maximum number of stacks that you can create in your account.</p>
describeAccountLimits :: forall eff. DescribeAccountLimitsInput -> Aff (err :: AWS.RequestError | eff) DescribeAccountLimitsOutput
describeAccountLimits = AWS.request serviceName "DescribeAccountLimits" 


-- | <p>Returns the inputs for the change set and a list of changes that AWS CloudFormation will make if you execute the change set. For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html">Updating Stacks Using Change Sets</a> in the AWS CloudFormation User Guide.</p>
describeChangeSet :: forall eff. DescribeChangeSetInput -> Aff (err :: AWS.RequestError | eff) DescribeChangeSetOutput
describeChangeSet = AWS.request serviceName "DescribeChangeSet" 


-- | <p>Returns all stack related events for a specified stack in reverse chronological order. For more information about a stack's event history, go to <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/concept-stack.html">Stacks</a> in the AWS CloudFormation User Guide.</p> <note> <p>You can list events for stacks that have failed to create or have been deleted by specifying the unique stack identifier (stack ID).</p> </note>
describeStackEvents :: forall eff. DescribeStackEventsInput -> Aff (err :: AWS.RequestError | eff) DescribeStackEventsOutput
describeStackEvents = AWS.request serviceName "DescribeStackEvents" 


-- | <p>Returns the stack instance that's associated with the specified stack set, AWS account, and region.</p> <p>For a list of stack instances that are associated with a specific stack set, use <a>ListStackInstances</a>.</p>
describeStackInstance :: forall eff. DescribeStackInstanceInput -> Aff (err :: AWS.RequestError | eff) DescribeStackInstanceOutput
describeStackInstance = AWS.request serviceName "DescribeStackInstance" 


-- | <p>Returns a description of the specified resource in the specified stack.</p> <p>For deleted stacks, DescribeStackResource returns resource information for up to 90 days after the stack has been deleted.</p>
describeStackResource :: forall eff. DescribeStackResourceInput -> Aff (err :: AWS.RequestError | eff) DescribeStackResourceOutput
describeStackResource = AWS.request serviceName "DescribeStackResource" 


-- | <p>Returns AWS resource descriptions for running and deleted stacks. If <code>StackName</code> is specified, all the associated resources that are part of the stack are returned. If <code>PhysicalResourceId</code> is specified, the associated resources of the stack that the resource belongs to are returned.</p> <note> <p>Only the first 100 resources will be returned. If your stack has more resources than this, you should use <code>ListStackResources</code> instead.</p> </note> <p>For deleted stacks, <code>DescribeStackResources</code> returns resource information for up to 90 days after the stack has been deleted.</p> <p>You must specify either <code>StackName</code> or <code>PhysicalResourceId</code>, but not both. In addition, you can specify <code>LogicalResourceId</code> to filter the returned result. For more information about resources, the <code>LogicalResourceId</code> and <code>PhysicalResourceId</code>, go to the <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/">AWS CloudFormation User Guide</a>.</p> <note> <p>A <code>ValidationError</code> is returned if you specify both <code>StackName</code> and <code>PhysicalResourceId</code> in the same request.</p> </note>
describeStackResources :: forall eff. DescribeStackResourcesInput -> Aff (err :: AWS.RequestError | eff) DescribeStackResourcesOutput
describeStackResources = AWS.request serviceName "DescribeStackResources" 


-- | <p>Returns the description of the specified stack set. </p>
describeStackSet :: forall eff. DescribeStackSetInput -> Aff (err :: AWS.RequestError | eff) DescribeStackSetOutput
describeStackSet = AWS.request serviceName "DescribeStackSet" 


-- | <p>Returns the description of the specified stack set operation. </p>
describeStackSetOperation :: forall eff. DescribeStackSetOperationInput -> Aff (err :: AWS.RequestError | eff) DescribeStackSetOperationOutput
describeStackSetOperation = AWS.request serviceName "DescribeStackSetOperation" 


-- | <p>Returns the description for the specified stack; if no stack name was specified, then it returns the description for all the stacks created.</p> <note> <p>If the stack does not exist, an <code>AmazonCloudFormationException</code> is returned.</p> </note>
describeStacks :: forall eff. DescribeStacksInput -> Aff (err :: AWS.RequestError | eff) DescribeStacksOutput
describeStacks = AWS.request serviceName "DescribeStacks" 


-- | <p>Returns the estimated monthly cost of a template. The return value is an AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.</p>
estimateTemplateCost :: forall eff. EstimateTemplateCostInput -> Aff (err :: AWS.RequestError | eff) EstimateTemplateCostOutput
estimateTemplateCost = AWS.request serviceName "EstimateTemplateCost" 


-- | <p>Updates a stack using the input information that was provided when the specified change set was created. After the call successfully completes, AWS CloudFormation starts updating the stack. Use the <a>DescribeStacks</a> action to view the status of the update.</p> <p>When you execute a change set, AWS CloudFormation deletes all other change sets associated with the stack because they aren't valid for the updated stack.</p> <p>If a stack policy is associated with the stack, AWS CloudFormation enforces the policy during the update. You can't specify a temporary stack policy that overrides the current policy.</p>
executeChangeSet :: forall eff. ExecuteChangeSetInput -> Aff (err :: AWS.RequestError | eff) ExecuteChangeSetOutput
executeChangeSet = AWS.request serviceName "ExecuteChangeSet" 


-- | <p>Returns the stack policy for a specified stack. If a stack doesn't have a policy, a null value is returned.</p>
getStackPolicy :: forall eff. GetStackPolicyInput -> Aff (err :: AWS.RequestError | eff) GetStackPolicyOutput
getStackPolicy = AWS.request serviceName "GetStackPolicy" 


-- | <p>Returns the template body for a specified stack. You can get the template for running or deleted stacks.</p> <p>For deleted stacks, GetTemplate returns the template for up to 90 days after the stack has been deleted.</p> <note> <p> If the template does not exist, a <code>ValidationError</code> is returned. </p> </note>
getTemplate :: forall eff. GetTemplateInput -> Aff (err :: AWS.RequestError | eff) GetTemplateOutput
getTemplate = AWS.request serviceName "GetTemplate" 


-- | <p>Returns information about a new or existing template. The <code>GetTemplateSummary</code> action is useful for viewing parameter information, such as default parameter values and parameter types, before you create or update a stack or stack set.</p> <p>You can use the <code>GetTemplateSummary</code> action when you submit a template, or you can get template information for a stack set, or a running or deleted stack.</p> <p>For deleted stacks, <code>GetTemplateSummary</code> returns the template information for up to 90 days after the stack has been deleted. If the template does not exist, a <code>ValidationError</code> is returned.</p>
getTemplateSummary :: forall eff. GetTemplateSummaryInput -> Aff (err :: AWS.RequestError | eff) GetTemplateSummaryOutput
getTemplateSummary = AWS.request serviceName "GetTemplateSummary" 


-- | <p>Returns the ID and status of each active change set for a stack. For example, AWS CloudFormation lists change sets that are in the <code>CREATE_IN_PROGRESS</code> or <code>CREATE_PENDING</code> state.</p>
listChangeSets :: forall eff. ListChangeSetsInput -> Aff (err :: AWS.RequestError | eff) ListChangeSetsOutput
listChangeSets = AWS.request serviceName "ListChangeSets" 


-- | <p>Lists all exported output values in the account and region in which you call this action. Use this action to see the exported output values that you can import into other stacks. To import values, use the <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html"> <code>Fn::ImportValue</code> </a> function. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-exports.html"> AWS CloudFormation Export Stack Output Values</a>.</p>
listExports :: forall eff. ListExportsInput -> Aff (err :: AWS.RequestError | eff) ListExportsOutput
listExports = AWS.request serviceName "ListExports" 


-- | <p>Lists all stacks that are importing an exported output value. To modify or remove an exported output value, first use this action to see which stacks are using it. To see the exported output values in your account, see <a>ListExports</a>. </p> <p>For more information about importing an exported output value, see the <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html"> <code>Fn::ImportValue</code> </a> function. </p>
listImports :: forall eff. ListImportsInput -> Aff (err :: AWS.RequestError | eff) ListImportsOutput
listImports = AWS.request serviceName "ListImports" 


-- | <p>Returns summary information about stack instances that are associated with the specified stack set. You can filter for stack instances that are associated with a specific AWS account name or region.</p>
listStackInstances :: forall eff. ListStackInstancesInput -> Aff (err :: AWS.RequestError | eff) ListStackInstancesOutput
listStackInstances = AWS.request serviceName "ListStackInstances" 


-- | <p>Returns descriptions of all resources of the specified stack.</p> <p>For deleted stacks, ListStackResources returns resource information for up to 90 days after the stack has been deleted.</p>
listStackResources :: forall eff. ListStackResourcesInput -> Aff (err :: AWS.RequestError | eff) ListStackResourcesOutput
listStackResources = AWS.request serviceName "ListStackResources" 


-- | <p>Returns summary information about the results of a stack set operation. </p>
listStackSetOperationResults :: forall eff. ListStackSetOperationResultsInput -> Aff (err :: AWS.RequestError | eff) ListStackSetOperationResultsOutput
listStackSetOperationResults = AWS.request serviceName "ListStackSetOperationResults" 


-- | <p>Returns summary information about operations performed on a stack set. </p>
listStackSetOperations :: forall eff. ListStackSetOperationsInput -> Aff (err :: AWS.RequestError | eff) ListStackSetOperationsOutput
listStackSetOperations = AWS.request serviceName "ListStackSetOperations" 


-- | <p>Returns summary information about stack sets that are associated with the user.</p>
listStackSets :: forall eff. ListStackSetsInput -> Aff (err :: AWS.RequestError | eff) ListStackSetsOutput
listStackSets = AWS.request serviceName "ListStackSets" 


-- | <p>Returns the summary information for stacks whose status matches the specified StackStatusFilter. Summary information for stacks that have been deleted is kept for 90 days after the stack is deleted. If no StackStatusFilter is specified, summary information for all stacks is returned (including existing stacks and stacks that have been deleted).</p>
listStacks :: forall eff. ListStacksInput -> Aff (err :: AWS.RequestError | eff) ListStacksOutput
listStacks = AWS.request serviceName "ListStacks" 


-- | <p>Sets a stack policy for a specified stack.</p>
setStackPolicy :: forall eff. SetStackPolicyInput -> Aff (err :: AWS.RequestError | eff) Unit
setStackPolicy = AWS.request serviceName "SetStackPolicy" 


-- | <p>Sends a signal to the specified resource with a success or failure status. You can use the SignalResource API in conjunction with a creation policy or update policy. AWS CloudFormation doesn't proceed with a stack creation or update until resources receive the required number of signals or the timeout period is exceeded. The SignalResource API is useful in cases where you want to send signals from anywhere other than an Amazon EC2 instance.</p>
signalResource :: forall eff. SignalResourceInput -> Aff (err :: AWS.RequestError | eff) Unit
signalResource = AWS.request serviceName "SignalResource" 


-- | <p>Stops an in-progress operation on a stack set and its associated stack instances. </p>
stopStackSetOperation :: forall eff. StopStackSetOperationInput -> Aff (err :: AWS.RequestError | eff) StopStackSetOperationOutput
stopStackSetOperation = AWS.request serviceName "StopStackSetOperation" 


-- | <p>Updates a stack as specified in the template. After the call completes successfully, the stack update starts. You can check the status of the stack via the <a>DescribeStacks</a> action.</p> <p>To get a copy of the template for an existing stack, you can use the <a>GetTemplate</a> action.</p> <p>For more information about creating an update template, updating a stack, and monitoring the progress of the update, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks.html">Updating a Stack</a>.</p>
updateStack :: forall eff. UpdateStackInput -> Aff (err :: AWS.RequestError | eff) UpdateStackOutput
updateStack = AWS.request serviceName "UpdateStack" 


-- | <p>Updates the parameter values for stack instances for the specified accounts, within the specified regions. A stack instance refers to a stack in a specific account and region. </p> <p>You can only update stack instances in regions and accounts where they already exist; to create additional stack instances, use <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackInstances.html">CreateStackInstances</a>. </p> <p>During stack set updates, any parameters overridden for a stack instance are not updated, but retain their overridden value.</p> <p>You can only update the parameter <i>values</i> that are specified in the stack set; to add or delete a parameter itself, use <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html">UpdateStackSet</a> to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html">UpdateStackSet</a> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using <code>UpdateStackInstances</code>.</p>
updateStackInstances :: forall eff. UpdateStackInstancesInput -> Aff (err :: AWS.RequestError | eff) UpdateStackInstancesOutput
updateStackInstances = AWS.request serviceName "UpdateStackInstances" 


-- | <p>Updates the stack set and <i>all</i> associated stack instances.</p> <p>Even if the stack set operation created by updating the stack set fails (completely or partially, below or above a specified failure tolerance), the stack set is updated with your changes. Subsequent <a>CreateStackInstances</a> calls on the specified stack set use the updated stack set.</p>
updateStackSet :: forall eff. UpdateStackSetInput -> Aff (err :: AWS.RequestError | eff) UpdateStackSetOutput
updateStackSet = AWS.request serviceName "UpdateStackSet" 


-- | <p>Updates termination protection for the specified stack. If a user attempts to delete a stack with termination protection enabled, the operation fails and the stack remains unchanged. For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html">Protecting a Stack From Being Deleted</a> in the <i>AWS CloudFormation User Guide</i>.</p> <p> For <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html">nested stacks</a>, termination protection is set on the root stack and cannot be changed directly on the nested stack.</p>
updateTerminationProtection :: forall eff. UpdateTerminationProtectionInput -> Aff (err :: AWS.RequestError | eff) UpdateTerminationProtectionOutput
updateTerminationProtection = AWS.request serviceName "UpdateTerminationProtection" 


-- | <p>Validates a specified template. AWS CloudFormation first checks if the template is valid JSON. If it isn't, AWS CloudFormation checks if the template is valid YAML. If both these checks fail, AWS CloudFormation returns a template validation error.</p>
validateTemplate :: forall eff. ValidateTemplateInput -> Aff (err :: AWS.RequestError | eff) ValidateTemplateOutput
validateTemplate = AWS.request serviceName "ValidateTemplate" 


newtype Account = Account String
derive instance newtypeAccount :: Newtype Account _


-- | <p>Structure that contains the results of the account gate function which AWS CloudFormation invokes, if present, before proceeding with a stack set operation in an account and region.</p> <p>For each account and region, AWS CloudFormation lets you specify a Lamdba function that encapsulates any requirements that must be met before CloudFormation can proceed with a stack set operation in that account and region. CloudFormation invokes the function each time a stack set operation is requested for that account and region; if the function returns <code>FAILED</code>, CloudFormation cancels the operation in that account and region, and sets the stack set operation result status for that account and region to <code>FAILED</code>. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-account-gating.html">Configuring a target account gate</a>.</p>
newtype AccountGateResult = AccountGateResult 
  { "Status" :: NullOrUndefined (AccountGateStatus)
  , "StatusReason" :: NullOrUndefined (AccountGateStatusReason)
  }
derive instance newtypeAccountGateResult :: Newtype AccountGateResult _


newtype AccountGateStatus = AccountGateStatus String
derive instance newtypeAccountGateStatus :: Newtype AccountGateStatus _


newtype AccountGateStatusReason = AccountGateStatusReason String
derive instance newtypeAccountGateStatusReason :: Newtype AccountGateStatusReason _


-- | <p>The AccountLimit data type.</p>
newtype AccountLimit = AccountLimit 
  { "Name" :: NullOrUndefined (LimitName)
  , "Value" :: NullOrUndefined (LimitValue)
  }
derive instance newtypeAccountLimit :: Newtype AccountLimit _


newtype AccountLimitList = AccountLimitList (Array AccountLimit)
derive instance newtypeAccountLimitList :: Newtype AccountLimitList _


newtype AccountList = AccountList (Array Account)
derive instance newtypeAccountList :: Newtype AccountList _


newtype AllowedValue = AllowedValue String
derive instance newtypeAllowedValue :: Newtype AllowedValue _


newtype AllowedValues = AllowedValues (Array AllowedValue)
derive instance newtypeAllowedValues :: Newtype AllowedValues _


-- | <p>The resource with the name requested already exists.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { 
  }
derive instance newtypeAlreadyExistsException :: Newtype AlreadyExistsException _


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


-- | <p>The input for the <a>CancelUpdateStack</a> action.</p>
newtype CancelUpdateStackInput = CancelUpdateStackInput 
  { "StackName" :: (StackName)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCancelUpdateStackInput :: Newtype CancelUpdateStackInput _


newtype Capabilities = Capabilities (Array Capability)
derive instance newtypeCapabilities :: Newtype Capabilities _


newtype CapabilitiesReason = CapabilitiesReason String
derive instance newtypeCapabilitiesReason :: Newtype CapabilitiesReason _


newtype Capability = Capability String
derive instance newtypeCapability :: Newtype Capability _


newtype CausingEntity = CausingEntity String
derive instance newtypeCausingEntity :: Newtype CausingEntity _


-- | <p>The <code>Change</code> structure describes the changes AWS CloudFormation will perform if you execute the change set.</p>
newtype Change = Change 
  { "Type" :: NullOrUndefined (ChangeType)
  , "ResourceChange" :: NullOrUndefined (ResourceChange)
  }
derive instance newtypeChange :: Newtype Change _


newtype ChangeAction = ChangeAction String
derive instance newtypeChangeAction :: Newtype ChangeAction _


newtype ChangeSetId = ChangeSetId String
derive instance newtypeChangeSetId :: Newtype ChangeSetId _


newtype ChangeSetName = ChangeSetName String
derive instance newtypeChangeSetName :: Newtype ChangeSetName _


newtype ChangeSetNameOrId = ChangeSetNameOrId String
derive instance newtypeChangeSetNameOrId :: Newtype ChangeSetNameOrId _


-- | <p>The specified change set name or ID doesn't exit. To view valid change sets for a stack, use the <code>ListChangeSets</code> action.</p>
newtype ChangeSetNotFoundException = ChangeSetNotFoundException 
  { 
  }
derive instance newtypeChangeSetNotFoundException :: Newtype ChangeSetNotFoundException _


newtype ChangeSetStatus = ChangeSetStatus String
derive instance newtypeChangeSetStatus :: Newtype ChangeSetStatus _


newtype ChangeSetStatusReason = ChangeSetStatusReason String
derive instance newtypeChangeSetStatusReason :: Newtype ChangeSetStatusReason _


newtype ChangeSetSummaries = ChangeSetSummaries (Array ChangeSetSummary)
derive instance newtypeChangeSetSummaries :: Newtype ChangeSetSummaries _


-- | <p>The <code>ChangeSetSummary</code> structure describes a change set, its status, and the stack with which it's associated.</p>
newtype ChangeSetSummary = ChangeSetSummary 
  { "StackId" :: NullOrUndefined (StackId)
  , "StackName" :: NullOrUndefined (StackName)
  , "ChangeSetId" :: NullOrUndefined (ChangeSetId)
  , "ChangeSetName" :: NullOrUndefined (ChangeSetName)
  , "ExecutionStatus" :: NullOrUndefined (ExecutionStatus)
  , "Status" :: NullOrUndefined (ChangeSetStatus)
  , "StatusReason" :: NullOrUndefined (ChangeSetStatusReason)
  , "CreationTime" :: NullOrUndefined (CreationTime)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeChangeSetSummary :: Newtype ChangeSetSummary _


newtype ChangeSetType = ChangeSetType String
derive instance newtypeChangeSetType :: Newtype ChangeSetType _


newtype ChangeSource = ChangeSource String
derive instance newtypeChangeSource :: Newtype ChangeSource _


newtype ChangeType = ChangeType String
derive instance newtypeChangeType :: Newtype ChangeType _


newtype Changes = Changes (Array Change)
derive instance newtypeChanges :: Newtype Changes _


newtype ClientRequestToken = ClientRequestToken String
derive instance newtypeClientRequestToken :: Newtype ClientRequestToken _


newtype ClientToken = ClientToken String
derive instance newtypeClientToken :: Newtype ClientToken _


-- | <p>The input for the <a>ContinueUpdateRollback</a> action.</p>
newtype ContinueUpdateRollbackInput = ContinueUpdateRollbackInput 
  { "StackName" :: (StackNameOrId)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "ResourcesToSkip" :: NullOrUndefined (ResourcesToSkip)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeContinueUpdateRollbackInput :: Newtype ContinueUpdateRollbackInput _


-- | <p>The output for a <a>ContinueUpdateRollback</a> action.</p>
newtype ContinueUpdateRollbackOutput = ContinueUpdateRollbackOutput 
  { 
  }
derive instance newtypeContinueUpdateRollbackOutput :: Newtype ContinueUpdateRollbackOutput _


-- | <p>The input for the <a>CreateChangeSet</a> action.</p>
newtype CreateChangeSetInput = CreateChangeSetInput 
  { "StackName" :: (StackNameOrId)
  , "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined (TemplateURL)
  , "UsePreviousTemplate" :: NullOrUndefined (UsePreviousTemplate)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "ResourceTypes" :: NullOrUndefined (ResourceTypes)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration)
  , "NotificationARNs" :: NullOrUndefined (NotificationARNs)
  , "Tags" :: NullOrUndefined (Tags)
  , "ChangeSetName" :: (ChangeSetName)
  , "ClientToken" :: NullOrUndefined (ClientToken)
  , "Description" :: NullOrUndefined (Description)
  , "ChangeSetType" :: NullOrUndefined (ChangeSetType)
  }
derive instance newtypeCreateChangeSetInput :: Newtype CreateChangeSetInput _


-- | <p>The output for the <a>CreateChangeSet</a> action.</p>
newtype CreateChangeSetOutput = CreateChangeSetOutput 
  { "Id" :: NullOrUndefined (ChangeSetId)
  , "StackId" :: NullOrUndefined (StackId)
  }
derive instance newtypeCreateChangeSetOutput :: Newtype CreateChangeSetOutput _


-- | <p>The input for <a>CreateStack</a> action.</p>
newtype CreateStackInput = CreateStackInput 
  { "StackName" :: (StackName)
  , "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined (TemplateURL)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "DisableRollback" :: NullOrUndefined (DisableRollback)
  , "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration)
  , "TimeoutInMinutes" :: NullOrUndefined (TimeoutMinutes)
  , "NotificationARNs" :: NullOrUndefined (NotificationARNs)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "ResourceTypes" :: NullOrUndefined (ResourceTypes)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "OnFailure" :: NullOrUndefined (OnFailure)
  , "StackPolicyBody" :: NullOrUndefined (StackPolicyBody)
  , "StackPolicyURL" :: NullOrUndefined (StackPolicyURL)
  , "Tags" :: NullOrUndefined (Tags)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  , "EnableTerminationProtection" :: NullOrUndefined (EnableTerminationProtection)
  }
derive instance newtypeCreateStackInput :: Newtype CreateStackInput _


newtype CreateStackInstancesInput = CreateStackInstancesInput 
  { "StackSetName" :: (StackSetName)
  , "Accounts" :: (AccountList)
  , "Regions" :: (RegionList)
  , "ParameterOverrides" :: NullOrUndefined (Parameters)
  , "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences)
  , "OperationId" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreateStackInstancesInput :: Newtype CreateStackInstancesInput _


newtype CreateStackInstancesOutput = CreateStackInstancesOutput 
  { "OperationId" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreateStackInstancesOutput :: Newtype CreateStackInstancesOutput _


-- | <p>The output for a <a>CreateStack</a> action.</p>
newtype CreateStackOutput = CreateStackOutput 
  { "StackId" :: NullOrUndefined (StackId)
  }
derive instance newtypeCreateStackOutput :: Newtype CreateStackOutput _


newtype CreateStackSetInput = CreateStackSetInput 
  { "StackSetName" :: (StackSetName)
  , "Description" :: NullOrUndefined (Description)
  , "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined (TemplateURL)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "Tags" :: NullOrUndefined (Tags)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreateStackSetInput :: Newtype CreateStackSetInput _


newtype CreateStackSetOutput = CreateStackSetOutput 
  { "StackSetId" :: NullOrUndefined (StackSetId)
  }
derive instance newtypeCreateStackSetOutput :: Newtype CreateStackSetOutput _


-- | <p>The specified resource exists, but has been changed.</p>
newtype CreatedButModifiedException = CreatedButModifiedException 
  { 
  }
derive instance newtypeCreatedButModifiedException :: Newtype CreatedButModifiedException _


newtype CreationTime = CreationTime Number
derive instance newtypeCreationTime :: Newtype CreationTime _


-- | <p>The input for the <a>DeleteChangeSet</a> action.</p>
newtype DeleteChangeSetInput = DeleteChangeSetInput 
  { "ChangeSetName" :: (ChangeSetNameOrId)
  , "StackName" :: NullOrUndefined (StackNameOrId)
  }
derive instance newtypeDeleteChangeSetInput :: Newtype DeleteChangeSetInput _


-- | <p>The output for the <a>DeleteChangeSet</a> action.</p>
newtype DeleteChangeSetOutput = DeleteChangeSetOutput 
  { 
  }
derive instance newtypeDeleteChangeSetOutput :: Newtype DeleteChangeSetOutput _


-- | <p>The input for <a>DeleteStack</a> action.</p>
newtype DeleteStackInput = DeleteStackInput 
  { "StackName" :: (StackName)
  , "RetainResources" :: NullOrUndefined (RetainResources)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeDeleteStackInput :: Newtype DeleteStackInput _


newtype DeleteStackInstancesInput = DeleteStackInstancesInput 
  { "StackSetName" :: (StackSetName)
  , "Accounts" :: (AccountList)
  , "Regions" :: (RegionList)
  , "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences)
  , "RetainStacks" :: (RetainStacks)
  , "OperationId" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeDeleteStackInstancesInput :: Newtype DeleteStackInstancesInput _


newtype DeleteStackInstancesOutput = DeleteStackInstancesOutput 
  { "OperationId" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeDeleteStackInstancesOutput :: Newtype DeleteStackInstancesOutput _


newtype DeleteStackSetInput = DeleteStackSetInput 
  { "StackSetName" :: (StackSetName)
  }
derive instance newtypeDeleteStackSetInput :: Newtype DeleteStackSetInput _


newtype DeleteStackSetOutput = DeleteStackSetOutput 
  { 
  }
derive instance newtypeDeleteStackSetOutput :: Newtype DeleteStackSetOutput _


newtype DeletionTime = DeletionTime Number
derive instance newtypeDeletionTime :: Newtype DeletionTime _


-- | <p>The input for the <a>DescribeAccountLimits</a> action.</p>
newtype DescribeAccountLimitsInput = DescribeAccountLimitsInput 
  { "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAccountLimitsInput :: Newtype DescribeAccountLimitsInput _


-- | <p>The output for the <a>DescribeAccountLimits</a> action.</p>
newtype DescribeAccountLimitsOutput = DescribeAccountLimitsOutput 
  { "AccountLimits" :: NullOrUndefined (AccountLimitList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAccountLimitsOutput :: Newtype DescribeAccountLimitsOutput _


-- | <p>The input for the <a>DescribeChangeSet</a> action.</p>
newtype DescribeChangeSetInput = DescribeChangeSetInput 
  { "ChangeSetName" :: (ChangeSetNameOrId)
  , "StackName" :: NullOrUndefined (StackNameOrId)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeChangeSetInput :: Newtype DescribeChangeSetInput _


-- | <p>The output for the <a>DescribeChangeSet</a> action.</p>
newtype DescribeChangeSetOutput = DescribeChangeSetOutput 
  { "ChangeSetName" :: NullOrUndefined (ChangeSetName)
  , "ChangeSetId" :: NullOrUndefined (ChangeSetId)
  , "StackId" :: NullOrUndefined (StackId)
  , "StackName" :: NullOrUndefined (StackName)
  , "Description" :: NullOrUndefined (Description)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "CreationTime" :: NullOrUndefined (CreationTime)
  , "ExecutionStatus" :: NullOrUndefined (ExecutionStatus)
  , "Status" :: NullOrUndefined (ChangeSetStatus)
  , "StatusReason" :: NullOrUndefined (ChangeSetStatusReason)
  , "NotificationARNs" :: NullOrUndefined (NotificationARNs)
  , "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "Tags" :: NullOrUndefined (Tags)
  , "Changes" :: NullOrUndefined (Changes)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeChangeSetOutput :: Newtype DescribeChangeSetOutput _


-- | <p>The input for <a>DescribeStackEvents</a> action.</p>
newtype DescribeStackEventsInput = DescribeStackEventsInput 
  { "StackName" :: NullOrUndefined (StackName)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeStackEventsInput :: Newtype DescribeStackEventsInput _


-- | <p>The output for a <a>DescribeStackEvents</a> action.</p>
newtype DescribeStackEventsOutput = DescribeStackEventsOutput 
  { "StackEvents" :: NullOrUndefined (StackEvents)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeStackEventsOutput :: Newtype DescribeStackEventsOutput _


newtype DescribeStackInstanceInput = DescribeStackInstanceInput 
  { "StackSetName" :: (StackSetName)
  , "StackInstanceAccount" :: (Account)
  , "StackInstanceRegion" :: (Region)
  }
derive instance newtypeDescribeStackInstanceInput :: Newtype DescribeStackInstanceInput _


newtype DescribeStackInstanceOutput = DescribeStackInstanceOutput 
  { "StackInstance" :: NullOrUndefined (StackInstance)
  }
derive instance newtypeDescribeStackInstanceOutput :: Newtype DescribeStackInstanceOutput _


-- | <p>The input for <a>DescribeStackResource</a> action.</p>
newtype DescribeStackResourceInput = DescribeStackResourceInput 
  { "StackName" :: (StackName)
  , "LogicalResourceId" :: (LogicalResourceId)
  }
derive instance newtypeDescribeStackResourceInput :: Newtype DescribeStackResourceInput _


-- | <p>The output for a <a>DescribeStackResource</a> action.</p>
newtype DescribeStackResourceOutput = DescribeStackResourceOutput 
  { "StackResourceDetail" :: NullOrUndefined (StackResourceDetail)
  }
derive instance newtypeDescribeStackResourceOutput :: Newtype DescribeStackResourceOutput _


-- | <p>The input for <a>DescribeStackResources</a> action.</p>
newtype DescribeStackResourcesInput = DescribeStackResourcesInput 
  { "StackName" :: NullOrUndefined (StackName)
  , "LogicalResourceId" :: NullOrUndefined (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId)
  }
derive instance newtypeDescribeStackResourcesInput :: Newtype DescribeStackResourcesInput _


-- | <p>The output for a <a>DescribeStackResources</a> action.</p>
newtype DescribeStackResourcesOutput = DescribeStackResourcesOutput 
  { "StackResources" :: NullOrUndefined (StackResources)
  }
derive instance newtypeDescribeStackResourcesOutput :: Newtype DescribeStackResourcesOutput _


newtype DescribeStackSetInput = DescribeStackSetInput 
  { "StackSetName" :: (StackSetName)
  }
derive instance newtypeDescribeStackSetInput :: Newtype DescribeStackSetInput _


newtype DescribeStackSetOperationInput = DescribeStackSetOperationInput 
  { "StackSetName" :: (StackSetName)
  , "OperationId" :: (ClientRequestToken)
  }
derive instance newtypeDescribeStackSetOperationInput :: Newtype DescribeStackSetOperationInput _


newtype DescribeStackSetOperationOutput = DescribeStackSetOperationOutput 
  { "StackSetOperation" :: NullOrUndefined (StackSetOperation)
  }
derive instance newtypeDescribeStackSetOperationOutput :: Newtype DescribeStackSetOperationOutput _


newtype DescribeStackSetOutput = DescribeStackSetOutput 
  { "StackSet" :: NullOrUndefined (StackSet)
  }
derive instance newtypeDescribeStackSetOutput :: Newtype DescribeStackSetOutput _


-- | <p>The input for <a>DescribeStacks</a> action.</p>
newtype DescribeStacksInput = DescribeStacksInput 
  { "StackName" :: NullOrUndefined (StackName)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeStacksInput :: Newtype DescribeStacksInput _


-- | <p>The output for a <a>DescribeStacks</a> action.</p>
newtype DescribeStacksOutput = DescribeStacksOutput 
  { "Stacks" :: NullOrUndefined (Stacks)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeStacksOutput :: Newtype DescribeStacksOutput _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


newtype DisableRollback = DisableRollback Boolean
derive instance newtypeDisableRollback :: Newtype DisableRollback _


newtype EnableTerminationProtection = EnableTerminationProtection Boolean
derive instance newtypeEnableTerminationProtection :: Newtype EnableTerminationProtection _


-- | <p>The input for an <a>EstimateTemplateCost</a> action.</p>
newtype EstimateTemplateCostInput = EstimateTemplateCostInput 
  { "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined (TemplateURL)
  , "Parameters" :: NullOrUndefined (Parameters)
  }
derive instance newtypeEstimateTemplateCostInput :: Newtype EstimateTemplateCostInput _


-- | <p>The output for a <a>EstimateTemplateCost</a> action.</p>
newtype EstimateTemplateCostOutput = EstimateTemplateCostOutput 
  { "Url" :: NullOrUndefined (Url)
  }
derive instance newtypeEstimateTemplateCostOutput :: Newtype EstimateTemplateCostOutput _


newtype EvaluationType = EvaluationType String
derive instance newtypeEvaluationType :: Newtype EvaluationType _


newtype EventId = EventId String
derive instance newtypeEventId :: Newtype EventId _


-- | <p>The input for the <a>ExecuteChangeSet</a> action.</p>
newtype ExecuteChangeSetInput = ExecuteChangeSetInput 
  { "ChangeSetName" :: (ChangeSetNameOrId)
  , "StackName" :: NullOrUndefined (StackNameOrId)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeExecuteChangeSetInput :: Newtype ExecuteChangeSetInput _


-- | <p>The output for the <a>ExecuteChangeSet</a> action.</p>
newtype ExecuteChangeSetOutput = ExecuteChangeSetOutput 
  { 
  }
derive instance newtypeExecuteChangeSetOutput :: Newtype ExecuteChangeSetOutput _


newtype ExecutionStatus = ExecutionStatus String
derive instance newtypeExecutionStatus :: Newtype ExecutionStatus _


-- | <p>The <code>Export</code> structure describes the exported output values for a stack.</p>
newtype Export = Export 
  { "ExportingStackId" :: NullOrUndefined (StackId)
  , "Name" :: NullOrUndefined (ExportName)
  , "Value" :: NullOrUndefined (ExportValue)
  }
derive instance newtypeExport :: Newtype Export _


newtype ExportName = ExportName String
derive instance newtypeExportName :: Newtype ExportName _


newtype ExportValue = ExportValue String
derive instance newtypeExportValue :: Newtype ExportValue _


newtype Exports = Exports (Array Export)
derive instance newtypeExports :: Newtype Exports _


newtype FailureToleranceCount = FailureToleranceCount Int
derive instance newtypeFailureToleranceCount :: Newtype FailureToleranceCount _


newtype FailureTolerancePercentage = FailureTolerancePercentage Int
derive instance newtypeFailureTolerancePercentage :: Newtype FailureTolerancePercentage _


-- | <p>The input for the <a>GetStackPolicy</a> action.</p>
newtype GetStackPolicyInput = GetStackPolicyInput 
  { "StackName" :: (StackName)
  }
derive instance newtypeGetStackPolicyInput :: Newtype GetStackPolicyInput _


-- | <p>The output for the <a>GetStackPolicy</a> action.</p>
newtype GetStackPolicyOutput = GetStackPolicyOutput 
  { "StackPolicyBody" :: NullOrUndefined (StackPolicyBody)
  }
derive instance newtypeGetStackPolicyOutput :: Newtype GetStackPolicyOutput _


-- | <p>The input for a <a>GetTemplate</a> action.</p>
newtype GetTemplateInput = GetTemplateInput 
  { "StackName" :: NullOrUndefined (StackName)
  , "ChangeSetName" :: NullOrUndefined (ChangeSetNameOrId)
  , "TemplateStage" :: NullOrUndefined (TemplateStage)
  }
derive instance newtypeGetTemplateInput :: Newtype GetTemplateInput _


-- | <p>The output for <a>GetTemplate</a> action.</p>
newtype GetTemplateOutput = GetTemplateOutput 
  { "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "StagesAvailable" :: NullOrUndefined (StageList)
  }
derive instance newtypeGetTemplateOutput :: Newtype GetTemplateOutput _


-- | <p>The input for the <a>GetTemplateSummary</a> action.</p>
newtype GetTemplateSummaryInput = GetTemplateSummaryInput 
  { "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined (TemplateURL)
  , "StackName" :: NullOrUndefined (StackNameOrId)
  , "StackSetName" :: NullOrUndefined (StackSetNameOrId)
  }
derive instance newtypeGetTemplateSummaryInput :: Newtype GetTemplateSummaryInput _


-- | <p>The output for the <a>GetTemplateSummary</a> action.</p>
newtype GetTemplateSummaryOutput = GetTemplateSummaryOutput 
  { "Parameters" :: NullOrUndefined (ParameterDeclarations)
  , "Description" :: NullOrUndefined (Description)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "CapabilitiesReason" :: NullOrUndefined (CapabilitiesReason)
  , "ResourceTypes" :: NullOrUndefined (ResourceTypes)
  , "Version" :: NullOrUndefined (Version)
  , "Metadata" :: NullOrUndefined (Metadata)
  , "DeclaredTransforms" :: NullOrUndefined (TransformsList)
  }
derive instance newtypeGetTemplateSummaryOutput :: Newtype GetTemplateSummaryOutput _


newtype Imports = Imports (Array StackName)
derive instance newtypeImports :: Newtype Imports _


-- | <p>The template contains resources with capabilities that weren't specified in the Capabilities parameter.</p>
newtype InsufficientCapabilitiesException = InsufficientCapabilitiesException 
  { 
  }
derive instance newtypeInsufficientCapabilitiesException :: Newtype InsufficientCapabilitiesException _


-- | <p>The specified change set can't be used to update the stack. For example, the change set status might be <code>CREATE_IN_PROGRESS</code>, or the stack status might be <code>UPDATE_IN_PROGRESS</code>.</p>
newtype InvalidChangeSetStatusException = InvalidChangeSetStatusException 
  { 
  }
derive instance newtypeInvalidChangeSetStatusException :: Newtype InvalidChangeSetStatusException _


-- | <p>The specified operation isn't valid.</p>
newtype InvalidOperationException = InvalidOperationException 
  { 
  }
derive instance newtypeInvalidOperationException :: Newtype InvalidOperationException _


newtype LastUpdatedTime = LastUpdatedTime Number
derive instance newtypeLastUpdatedTime :: Newtype LastUpdatedTime _


-- | <p>The quota for the resource has already been reached.</p> <p>For information on stack set limitations, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-limitations.html">Limitations of StackSets</a>.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype LimitName = LimitName String
derive instance newtypeLimitName :: Newtype LimitName _


newtype LimitValue = LimitValue Int
derive instance newtypeLimitValue :: Newtype LimitValue _


-- | <p>The input for the <a>ListChangeSets</a> action.</p>
newtype ListChangeSetsInput = ListChangeSetsInput 
  { "StackName" :: (StackNameOrId)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListChangeSetsInput :: Newtype ListChangeSetsInput _


-- | <p>The output for the <a>ListChangeSets</a> action.</p>
newtype ListChangeSetsOutput = ListChangeSetsOutput 
  { "Summaries" :: NullOrUndefined (ChangeSetSummaries)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListChangeSetsOutput :: Newtype ListChangeSetsOutput _


newtype ListExportsInput = ListExportsInput 
  { "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListExportsInput :: Newtype ListExportsInput _


newtype ListExportsOutput = ListExportsOutput 
  { "Exports" :: NullOrUndefined (Exports)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListExportsOutput :: Newtype ListExportsOutput _


newtype ListImportsInput = ListImportsInput 
  { "ExportName" :: (ExportName)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListImportsInput :: Newtype ListImportsInput _


newtype ListImportsOutput = ListImportsOutput 
  { "Imports" :: NullOrUndefined (Imports)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListImportsOutput :: Newtype ListImportsOutput _


newtype ListStackInstancesInput = ListStackInstancesInput 
  { "StackSetName" :: (StackSetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "StackInstanceAccount" :: NullOrUndefined (Account)
  , "StackInstanceRegion" :: NullOrUndefined (Region)
  }
derive instance newtypeListStackInstancesInput :: Newtype ListStackInstancesInput _


newtype ListStackInstancesOutput = ListStackInstancesOutput 
  { "Summaries" :: NullOrUndefined (StackInstanceSummaries)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListStackInstancesOutput :: Newtype ListStackInstancesOutput _


-- | <p>The input for the <a>ListStackResource</a> action.</p>
newtype ListStackResourcesInput = ListStackResourcesInput 
  { "StackName" :: (StackName)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListStackResourcesInput :: Newtype ListStackResourcesInput _


-- | <p>The output for a <a>ListStackResources</a> action.</p>
newtype ListStackResourcesOutput = ListStackResourcesOutput 
  { "StackResourceSummaries" :: NullOrUndefined (StackResourceSummaries)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListStackResourcesOutput :: Newtype ListStackResourcesOutput _


newtype ListStackSetOperationResultsInput = ListStackSetOperationResultsInput 
  { "StackSetName" :: (StackSetName)
  , "OperationId" :: (ClientRequestToken)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListStackSetOperationResultsInput :: Newtype ListStackSetOperationResultsInput _


newtype ListStackSetOperationResultsOutput = ListStackSetOperationResultsOutput 
  { "Summaries" :: NullOrUndefined (StackSetOperationResultSummaries)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListStackSetOperationResultsOutput :: Newtype ListStackSetOperationResultsOutput _


newtype ListStackSetOperationsInput = ListStackSetOperationsInput 
  { "StackSetName" :: (StackSetName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListStackSetOperationsInput :: Newtype ListStackSetOperationsInput _


newtype ListStackSetOperationsOutput = ListStackSetOperationsOutput 
  { "Summaries" :: NullOrUndefined (StackSetOperationSummaries)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListStackSetOperationsOutput :: Newtype ListStackSetOperationsOutput _


newtype ListStackSetsInput = ListStackSetsInput 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Status" :: NullOrUndefined (StackSetStatus)
  }
derive instance newtypeListStackSetsInput :: Newtype ListStackSetsInput _


newtype ListStackSetsOutput = ListStackSetsOutput 
  { "Summaries" :: NullOrUndefined (StackSetSummaries)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListStackSetsOutput :: Newtype ListStackSetsOutput _


-- | <p>The input for <a>ListStacks</a> action.</p>
newtype ListStacksInput = ListStacksInput 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "StackStatusFilter" :: NullOrUndefined (StackStatusFilter)
  }
derive instance newtypeListStacksInput :: Newtype ListStacksInput _


-- | <p>The output for <a>ListStacks</a> action.</p>
newtype ListStacksOutput = ListStacksOutput 
  { "StackSummaries" :: NullOrUndefined (StackSummaries)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListStacksOutput :: Newtype ListStacksOutput _


newtype LogicalResourceId = LogicalResourceId String
derive instance newtypeLogicalResourceId :: Newtype LogicalResourceId _


newtype MaxConcurrentCount = MaxConcurrentCount Int
derive instance newtypeMaxConcurrentCount :: Newtype MaxConcurrentCount _


newtype MaxConcurrentPercentage = MaxConcurrentPercentage Int
derive instance newtypeMaxConcurrentPercentage :: Newtype MaxConcurrentPercentage _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype Metadata = Metadata String
derive instance newtypeMetadata :: Newtype Metadata _


newtype MonitoringTimeInMinutes = MonitoringTimeInMinutes Int
derive instance newtypeMonitoringTimeInMinutes :: Newtype MonitoringTimeInMinutes _


-- | <p>The specified name is already in use.</p>
newtype NameAlreadyExistsException = NameAlreadyExistsException 
  { 
  }
derive instance newtypeNameAlreadyExistsException :: Newtype NameAlreadyExistsException _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype NoEcho = NoEcho Boolean
derive instance newtypeNoEcho :: Newtype NoEcho _


newtype NotificationARN = NotificationARN String
derive instance newtypeNotificationARN :: Newtype NotificationARN _


newtype NotificationARNs = NotificationARNs (Array NotificationARN)
derive instance newtypeNotificationARNs :: Newtype NotificationARNs _


newtype OnFailure = OnFailure String
derive instance newtypeOnFailure :: Newtype OnFailure _


-- | <p>The specified operation ID already exists.</p>
newtype OperationIdAlreadyExistsException = OperationIdAlreadyExistsException 
  { 
  }
derive instance newtypeOperationIdAlreadyExistsException :: Newtype OperationIdAlreadyExistsException _


-- | <p>Another operation is currently in progress for this stack set. Only one operation can be performed for a stack set at a given time.</p>
newtype OperationInProgressException = OperationInProgressException 
  { 
  }
derive instance newtypeOperationInProgressException :: Newtype OperationInProgressException _


-- | <p>The specified ID refers to an operation that doesn't exist.</p>
newtype OperationNotFoundException = OperationNotFoundException 
  { 
  }
derive instance newtypeOperationNotFoundException :: Newtype OperationNotFoundException _


-- | <p>The Output data type.</p>
newtype Output = Output 
  { "OutputKey" :: NullOrUndefined (OutputKey)
  , "OutputValue" :: NullOrUndefined (OutputValue)
  , "Description" :: NullOrUndefined (Description)
  , "ExportName" :: NullOrUndefined (ExportName)
  }
derive instance newtypeOutput :: Newtype Output _


newtype OutputKey = OutputKey String
derive instance newtypeOutputKey :: Newtype OutputKey _


newtype OutputValue = OutputValue String
derive instance newtypeOutputValue :: Newtype OutputValue _


newtype Outputs = Outputs (Array Output)
derive instance newtypeOutputs :: Newtype Outputs _


-- | <p>The Parameter data type.</p>
newtype Parameter = Parameter 
  { "ParameterKey" :: NullOrUndefined (ParameterKey)
  , "ParameterValue" :: NullOrUndefined (ParameterValue)
  , "UsePreviousValue" :: NullOrUndefined (UsePreviousValue)
  , "ResolvedValue" :: NullOrUndefined (ParameterValue)
  }
derive instance newtypeParameter :: Newtype Parameter _


-- | <p>A set of criteria that AWS CloudFormation uses to validate parameter values. Although other constraints might be defined in the stack template, AWS CloudFormation returns only the <code>AllowedValues</code> property.</p>
newtype ParameterConstraints = ParameterConstraints 
  { "AllowedValues" :: NullOrUndefined (AllowedValues)
  }
derive instance newtypeParameterConstraints :: Newtype ParameterConstraints _


-- | <p>The ParameterDeclaration data type.</p>
newtype ParameterDeclaration = ParameterDeclaration 
  { "ParameterKey" :: NullOrUndefined (ParameterKey)
  , "DefaultValue" :: NullOrUndefined (ParameterValue)
  , "ParameterType" :: NullOrUndefined (ParameterType)
  , "NoEcho" :: NullOrUndefined (NoEcho)
  , "Description" :: NullOrUndefined (Description)
  , "ParameterConstraints" :: NullOrUndefined (ParameterConstraints)
  }
derive instance newtypeParameterDeclaration :: Newtype ParameterDeclaration _


newtype ParameterDeclarations = ParameterDeclarations (Array ParameterDeclaration)
derive instance newtypeParameterDeclarations :: Newtype ParameterDeclarations _


newtype ParameterKey = ParameterKey String
derive instance newtypeParameterKey :: Newtype ParameterKey _


newtype ParameterType = ParameterType String
derive instance newtypeParameterType :: Newtype ParameterType _


newtype ParameterValue = ParameterValue String
derive instance newtypeParameterValue :: Newtype ParameterValue _


newtype Parameters = Parameters (Array Parameter)
derive instance newtypeParameters :: Newtype Parameters _


newtype PhysicalResourceId = PhysicalResourceId String
derive instance newtypePhysicalResourceId :: Newtype PhysicalResourceId _


newtype PropertyName = PropertyName String
derive instance newtypePropertyName :: Newtype PropertyName _


newtype Reason = Reason String
derive instance newtypeReason :: Newtype Reason _


newtype Region = Region String
derive instance newtypeRegion :: Newtype Region _


newtype RegionList = RegionList (Array Region)
derive instance newtypeRegionList :: Newtype RegionList _


newtype Replacement = Replacement String
derive instance newtypeReplacement :: Newtype Replacement _


newtype RequiresRecreation = RequiresRecreation String
derive instance newtypeRequiresRecreation :: Newtype RequiresRecreation _


newtype ResourceAttribute = ResourceAttribute String
derive instance newtypeResourceAttribute :: Newtype ResourceAttribute _


-- | <p>The <code>ResourceChange</code> structure describes the resource and the action that AWS CloudFormation will perform on it if you execute this change set.</p>
newtype ResourceChange = ResourceChange 
  { "Action" :: NullOrUndefined (ChangeAction)
  , "LogicalResourceId" :: NullOrUndefined (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: NullOrUndefined (ResourceType)
  , "Replacement" :: NullOrUndefined (Replacement)
  , "Scope" :: NullOrUndefined (Scope)
  , "Details" :: NullOrUndefined (ResourceChangeDetails)
  }
derive instance newtypeResourceChange :: Newtype ResourceChange _


-- | <p>For a resource with <code>Modify</code> as the action, the <code>ResourceChange</code> structure describes the changes AWS CloudFormation will make to that resource.</p>
newtype ResourceChangeDetail = ResourceChangeDetail 
  { "Target" :: NullOrUndefined (ResourceTargetDefinition)
  , "Evaluation" :: NullOrUndefined (EvaluationType)
  , "ChangeSource" :: NullOrUndefined (ChangeSource)
  , "CausingEntity" :: NullOrUndefined (CausingEntity)
  }
derive instance newtypeResourceChangeDetail :: Newtype ResourceChangeDetail _


newtype ResourceChangeDetails = ResourceChangeDetails (Array ResourceChangeDetail)
derive instance newtypeResourceChangeDetails :: Newtype ResourceChangeDetails _


newtype ResourceProperties = ResourceProperties String
derive instance newtypeResourceProperties :: Newtype ResourceProperties _


newtype ResourceSignalStatus = ResourceSignalStatus String
derive instance newtypeResourceSignalStatus :: Newtype ResourceSignalStatus _


newtype ResourceSignalUniqueId = ResourceSignalUniqueId String
derive instance newtypeResourceSignalUniqueId :: Newtype ResourceSignalUniqueId _


newtype ResourceStatus = ResourceStatus String
derive instance newtypeResourceStatus :: Newtype ResourceStatus _


newtype ResourceStatusReason = ResourceStatusReason String
derive instance newtypeResourceStatusReason :: Newtype ResourceStatusReason _


-- | <p>The field that AWS CloudFormation will change, such as the name of a resource's property, and whether the resource will be recreated.</p>
newtype ResourceTargetDefinition = ResourceTargetDefinition 
  { "Attribute" :: NullOrUndefined (ResourceAttribute)
  , "Name" :: NullOrUndefined (PropertyName)
  , "RequiresRecreation" :: NullOrUndefined (RequiresRecreation)
  }
derive instance newtypeResourceTargetDefinition :: Newtype ResourceTargetDefinition _


newtype ResourceToSkip = ResourceToSkip String
derive instance newtypeResourceToSkip :: Newtype ResourceToSkip _


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _


newtype ResourceTypes = ResourceTypes (Array ResourceType)
derive instance newtypeResourceTypes :: Newtype ResourceTypes _


newtype ResourcesToSkip = ResourcesToSkip (Array ResourceToSkip)
derive instance newtypeResourcesToSkip :: Newtype ResourcesToSkip _


newtype RetainResources = RetainResources (Array LogicalResourceId)
derive instance newtypeRetainResources :: Newtype RetainResources _


newtype RetainStacks = RetainStacks Boolean
derive instance newtypeRetainStacks :: Newtype RetainStacks _


newtype RetainStacksNullable = RetainStacksNullable Boolean
derive instance newtypeRetainStacksNullable :: Newtype RetainStacksNullable _


newtype RoleARN = RoleARN String
derive instance newtypeRoleARN :: Newtype RoleARN _


-- | <p>Structure containing the rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.</p> <p>Rollback triggers enable you to have AWS CloudFormation monitor the state of your application during stack creation and updating, and to roll back that operation if the application breaches the threshold of any of the alarms you've specified. For each rollback trigger you create, you specify the Cloudwatch alarm that CloudFormation should monitor. CloudFormation monitors the specified alarms during the stack create or update operation, and for the specified amount of time after all resources have been deployed. If any of the alarms goes to ALERT state during the stack operation or the monitoring period, CloudFormation rolls back the entire stack operation. If the monitoring period expires without any alarms going to ALERT state, CloudFormation proceeds to dispose of old resources as usual.</p> <p>By default, CloudFormation only rolls back stack operations if an alarm goes to ALERT state, not INSUFFICIENT_DATA state. To have CloudFormation roll back the stack operation if an alarm goes to INSUFFICIENT_DATA state as well, edit the CloudWatch alarm to treat missing data as <code>breaching</code>. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html">Configuring How CloudWatch Alarms Treats Missing Data</a>.</p> <p>AWS CloudFormation does not monitor rollback triggers when it rolls back a stack during an update operation.</p>
newtype RollbackConfiguration = RollbackConfiguration 
  { "RollbackTriggers" :: NullOrUndefined (RollbackTriggers)
  , "MonitoringTimeInMinutes" :: NullOrUndefined (MonitoringTimeInMinutes)
  }
derive instance newtypeRollbackConfiguration :: Newtype RollbackConfiguration _


-- | <p>A rollback trigger AWS CloudFormation monitors during creation and updating of stacks. If any of the alarms you specify goes to ALERT state during the stack operation or within the specified monitoring period afterwards, CloudFormation rolls back the entire stack operation. </p>
newtype RollbackTrigger = RollbackTrigger 
  { "Arn" :: (Arn)
  , "Type" :: (Type)
  }
derive instance newtypeRollbackTrigger :: Newtype RollbackTrigger _


newtype RollbackTriggers = RollbackTriggers (Array RollbackTrigger)
derive instance newtypeRollbackTriggers :: Newtype RollbackTriggers _


newtype Scope = Scope (Array ResourceAttribute)
derive instance newtypeScope :: Newtype Scope _


-- | <p>The input for the <a>SetStackPolicy</a> action.</p>
newtype SetStackPolicyInput = SetStackPolicyInput 
  { "StackName" :: (StackName)
  , "StackPolicyBody" :: NullOrUndefined (StackPolicyBody)
  , "StackPolicyURL" :: NullOrUndefined (StackPolicyURL)
  }
derive instance newtypeSetStackPolicyInput :: Newtype SetStackPolicyInput _


-- | <p>The input for the <a>SignalResource</a> action.</p>
newtype SignalResourceInput = SignalResourceInput 
  { "StackName" :: (StackNameOrId)
  , "LogicalResourceId" :: (LogicalResourceId)
  , "UniqueId" :: (ResourceSignalUniqueId)
  , "Status" :: (ResourceSignalStatus)
  }
derive instance newtypeSignalResourceInput :: Newtype SignalResourceInput _


-- | <p>The Stack data type.</p>
newtype Stack = Stack 
  { "StackId" :: NullOrUndefined (StackId)
  , "StackName" :: (StackName)
  , "ChangeSetId" :: NullOrUndefined (ChangeSetId)
  , "Description" :: NullOrUndefined (Description)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "CreationTime" :: (CreationTime)
  , "DeletionTime" :: NullOrUndefined (DeletionTime)
  , "LastUpdatedTime" :: NullOrUndefined (LastUpdatedTime)
  , "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration)
  , "StackStatus" :: (StackStatus)
  , "StackStatusReason" :: NullOrUndefined (StackStatusReason)
  , "DisableRollback" :: NullOrUndefined (DisableRollback)
  , "NotificationARNs" :: NullOrUndefined (NotificationARNs)
  , "TimeoutInMinutes" :: NullOrUndefined (TimeoutMinutes)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "Outputs" :: NullOrUndefined (Outputs)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "Tags" :: NullOrUndefined (Tags)
  , "EnableTerminationProtection" :: NullOrUndefined (EnableTerminationProtection)
  , "ParentId" :: NullOrUndefined (StackId)
  , "RootId" :: NullOrUndefined (StackId)
  }
derive instance newtypeStack :: Newtype Stack _


-- | <p>The StackEvent data type.</p>
newtype StackEvent = StackEvent 
  { "StackId" :: (StackId)
  , "EventId" :: (EventId)
  , "StackName" :: (StackName)
  , "LogicalResourceId" :: NullOrUndefined (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: NullOrUndefined (ResourceType)
  , "Number" :: (Number)
  , "ResourceStatus" :: NullOrUndefined (ResourceStatus)
  , "ResourceStatusReason" :: NullOrUndefined (ResourceStatusReason)
  , "ResourceProperties" :: NullOrUndefined (ResourceProperties)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeStackEvent :: Newtype StackEvent _


newtype StackEvents = StackEvents (Array StackEvent)
derive instance newtypeStackEvents :: Newtype StackEvents _


newtype StackId = StackId String
derive instance newtypeStackId :: Newtype StackId _


-- | <p>An AWS CloudFormation stack, in a specific account and region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.</p>
newtype StackInstance = StackInstance 
  { "StackSetId" :: NullOrUndefined (StackSetId)
  , "Region" :: NullOrUndefined (Region)
  , "Account" :: NullOrUndefined (Account)
  , "StackId" :: NullOrUndefined (StackId)
  , "ParameterOverrides" :: NullOrUndefined (Parameters)
  , "Status" :: NullOrUndefined (StackInstanceStatus)
  , "StatusReason" :: NullOrUndefined (Reason)
  }
derive instance newtypeStackInstance :: Newtype StackInstance _


-- | <p>The specified stack instance doesn't exist.</p>
newtype StackInstanceNotFoundException = StackInstanceNotFoundException 
  { 
  }
derive instance newtypeStackInstanceNotFoundException :: Newtype StackInstanceNotFoundException _


newtype StackInstanceStatus = StackInstanceStatus String
derive instance newtypeStackInstanceStatus :: Newtype StackInstanceStatus _


newtype StackInstanceSummaries = StackInstanceSummaries (Array StackInstanceSummary)
derive instance newtypeStackInstanceSummaries :: Newtype StackInstanceSummaries _


-- | <p>The structure that contains summary information about a stack instance.</p>
newtype StackInstanceSummary = StackInstanceSummary 
  { "StackSetId" :: NullOrUndefined (StackSetId)
  , "Region" :: NullOrUndefined (Region)
  , "Account" :: NullOrUndefined (Account)
  , "StackId" :: NullOrUndefined (StackId)
  , "Status" :: NullOrUndefined (StackInstanceStatus)
  , "StatusReason" :: NullOrUndefined (Reason)
  }
derive instance newtypeStackInstanceSummary :: Newtype StackInstanceSummary _


newtype StackName = StackName String
derive instance newtypeStackName :: Newtype StackName _


newtype StackNameOrId = StackNameOrId String
derive instance newtypeStackNameOrId :: Newtype StackNameOrId _


newtype StackPolicyBody = StackPolicyBody String
derive instance newtypeStackPolicyBody :: Newtype StackPolicyBody _


newtype StackPolicyDuringUpdateBody = StackPolicyDuringUpdateBody String
derive instance newtypeStackPolicyDuringUpdateBody :: Newtype StackPolicyDuringUpdateBody _


newtype StackPolicyDuringUpdateURL = StackPolicyDuringUpdateURL String
derive instance newtypeStackPolicyDuringUpdateURL :: Newtype StackPolicyDuringUpdateURL _


newtype StackPolicyURL = StackPolicyURL String
derive instance newtypeStackPolicyURL :: Newtype StackPolicyURL _


-- | <p>The StackResource data type.</p>
newtype StackResource = StackResource 
  { "StackName" :: NullOrUndefined (StackName)
  , "StackId" :: NullOrUndefined (StackId)
  , "LogicalResourceId" :: (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: (ResourceType)
  , "Number" :: (Number)
  , "ResourceStatus" :: (ResourceStatus)
  , "ResourceStatusReason" :: NullOrUndefined (ResourceStatusReason)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeStackResource :: Newtype StackResource _


-- | <p>Contains detailed information about the specified stack resource.</p>
newtype StackResourceDetail = StackResourceDetail 
  { "StackName" :: NullOrUndefined (StackName)
  , "StackId" :: NullOrUndefined (StackId)
  , "LogicalResourceId" :: (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: (ResourceType)
  , "LastUpdatedTimestamp" :: (Number)
  , "ResourceStatus" :: (ResourceStatus)
  , "ResourceStatusReason" :: NullOrUndefined (ResourceStatusReason)
  , "Description" :: NullOrUndefined (Description)
  , "Metadata" :: NullOrUndefined (Metadata)
  }
derive instance newtypeStackResourceDetail :: Newtype StackResourceDetail _


newtype StackResourceSummaries = StackResourceSummaries (Array StackResourceSummary)
derive instance newtypeStackResourceSummaries :: Newtype StackResourceSummaries _


-- | <p>Contains high-level information about the specified stack resource.</p>
newtype StackResourceSummary = StackResourceSummary 
  { "LogicalResourceId" :: (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: (ResourceType)
  , "LastUpdatedTimestamp" :: (Number)
  , "ResourceStatus" :: (ResourceStatus)
  , "ResourceStatusReason" :: NullOrUndefined (ResourceStatusReason)
  }
derive instance newtypeStackResourceSummary :: Newtype StackResourceSummary _


newtype StackResources = StackResources (Array StackResource)
derive instance newtypeStackResources :: Newtype StackResources _


-- | <p>A structure that contains information about a stack set. A stack set enables you to provision stacks into AWS accounts and across regions by using a single CloudFormation template. In the stack set, you specify the template to use, as well as any parameters and capabilities that the template requires. </p>
newtype StackSet = StackSet 
  { "StackSetName" :: NullOrUndefined (StackSetName)
  , "StackSetId" :: NullOrUndefined (StackSetId)
  , "Description" :: NullOrUndefined (Description)
  , "Status" :: NullOrUndefined (StackSetStatus)
  , "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeStackSet :: Newtype StackSet _


newtype StackSetId = StackSetId String
derive instance newtypeStackSetId :: Newtype StackSetId _


newtype StackSetName = StackSetName String
derive instance newtypeStackSetName :: Newtype StackSetName _


newtype StackSetNameOrId = StackSetNameOrId String
derive instance newtypeStackSetNameOrId :: Newtype StackSetNameOrId _


-- | <p>You can't yet delete this stack set, because it still contains one or more stack instances. Delete all stack instances from the stack set before deleting the stack set.</p>
newtype StackSetNotEmptyException = StackSetNotEmptyException 
  { 
  }
derive instance newtypeStackSetNotEmptyException :: Newtype StackSetNotEmptyException _


-- | <p>The specified stack set doesn't exist.</p>
newtype StackSetNotFoundException = StackSetNotFoundException 
  { 
  }
derive instance newtypeStackSetNotFoundException :: Newtype StackSetNotFoundException _


-- | <p>The structure that contains information about a stack set operation. </p>
newtype StackSetOperation = StackSetOperation 
  { "OperationId" :: NullOrUndefined (ClientRequestToken)
  , "StackSetId" :: NullOrUndefined (StackSetId)
  , "Action" :: NullOrUndefined (StackSetOperationAction)
  , "Status" :: NullOrUndefined (StackSetOperationStatus)
  , "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences)
  , "RetainStacks" :: NullOrUndefined (RetainStacksNullable)
  , "CreationTimestamp" :: NullOrUndefined (Number)
  , "EndTimestamp" :: NullOrUndefined (Number)
  }
derive instance newtypeStackSetOperation :: Newtype StackSetOperation _


newtype StackSetOperationAction = StackSetOperationAction String
derive instance newtypeStackSetOperationAction :: Newtype StackSetOperationAction _


-- | <p>The user-specified preferences for how AWS CloudFormation performs a stack set operation. </p> <p>For more information on maximum concurrent accounts and failure tolerance, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options">Stack set operation options</a>.</p>
newtype StackSetOperationPreferences = StackSetOperationPreferences 
  { "RegionOrder" :: NullOrUndefined (RegionList)
  , "FailureToleranceCount" :: NullOrUndefined (FailureToleranceCount)
  , "FailureTolerancePercentage" :: NullOrUndefined (FailureTolerancePercentage)
  , "MaxConcurrentCount" :: NullOrUndefined (MaxConcurrentCount)
  , "MaxConcurrentPercentage" :: NullOrUndefined (MaxConcurrentPercentage)
  }
derive instance newtypeStackSetOperationPreferences :: Newtype StackSetOperationPreferences _


newtype StackSetOperationResultStatus = StackSetOperationResultStatus String
derive instance newtypeStackSetOperationResultStatus :: Newtype StackSetOperationResultStatus _


newtype StackSetOperationResultSummaries = StackSetOperationResultSummaries (Array StackSetOperationResultSummary)
derive instance newtypeStackSetOperationResultSummaries :: Newtype StackSetOperationResultSummaries _


-- | <p>The structure that contains information about a specified operation's results for a given account in a given region.</p>
newtype StackSetOperationResultSummary = StackSetOperationResultSummary 
  { "Account" :: NullOrUndefined (Account)
  , "Region" :: NullOrUndefined (Region)
  , "Status" :: NullOrUndefined (StackSetOperationResultStatus)
  , "StatusReason" :: NullOrUndefined (Reason)
  , "AccountGateResult" :: NullOrUndefined (AccountGateResult)
  }
derive instance newtypeStackSetOperationResultSummary :: Newtype StackSetOperationResultSummary _


newtype StackSetOperationStatus = StackSetOperationStatus String
derive instance newtypeStackSetOperationStatus :: Newtype StackSetOperationStatus _


newtype StackSetOperationSummaries = StackSetOperationSummaries (Array StackSetOperationSummary)
derive instance newtypeStackSetOperationSummaries :: Newtype StackSetOperationSummaries _


-- | <p>The structures that contain summary information about the specified operation.</p>
newtype StackSetOperationSummary = StackSetOperationSummary 
  { "OperationId" :: NullOrUndefined (ClientRequestToken)
  , "Action" :: NullOrUndefined (StackSetOperationAction)
  , "Status" :: NullOrUndefined (StackSetOperationStatus)
  , "CreationTimestamp" :: NullOrUndefined (Number)
  , "EndTimestamp" :: NullOrUndefined (Number)
  }
derive instance newtypeStackSetOperationSummary :: Newtype StackSetOperationSummary _


newtype StackSetStatus = StackSetStatus String
derive instance newtypeStackSetStatus :: Newtype StackSetStatus _


newtype StackSetSummaries = StackSetSummaries (Array StackSetSummary)
derive instance newtypeStackSetSummaries :: Newtype StackSetSummaries _


-- | <p>The structures that contain summary information about the specified stack set.</p>
newtype StackSetSummary = StackSetSummary 
  { "StackSetName" :: NullOrUndefined (StackSetName)
  , "StackSetId" :: NullOrUndefined (StackSetId)
  , "Description" :: NullOrUndefined (Description)
  , "Status" :: NullOrUndefined (StackSetStatus)
  }
derive instance newtypeStackSetSummary :: Newtype StackSetSummary _


newtype StackStatus = StackStatus String
derive instance newtypeStackStatus :: Newtype StackStatus _


newtype StackStatusFilter = StackStatusFilter (Array StackStatus)
derive instance newtypeStackStatusFilter :: Newtype StackStatusFilter _


newtype StackStatusReason = StackStatusReason String
derive instance newtypeStackStatusReason :: Newtype StackStatusReason _


newtype StackSummaries = StackSummaries (Array StackSummary)
derive instance newtypeStackSummaries :: Newtype StackSummaries _


-- | <p>The StackSummary Data Type</p>
newtype StackSummary = StackSummary 
  { "StackId" :: NullOrUndefined (StackId)
  , "StackName" :: (StackName)
  , "TemplateDescription" :: NullOrUndefined (TemplateDescription)
  , "CreationTime" :: (CreationTime)
  , "LastUpdatedTime" :: NullOrUndefined (LastUpdatedTime)
  , "DeletionTime" :: NullOrUndefined (DeletionTime)
  , "StackStatus" :: (StackStatus)
  , "StackStatusReason" :: NullOrUndefined (StackStatusReason)
  , "ParentId" :: NullOrUndefined (StackId)
  , "RootId" :: NullOrUndefined (StackId)
  }
derive instance newtypeStackSummary :: Newtype StackSummary _


newtype Stacks = Stacks (Array Stack)
derive instance newtypeStacks :: Newtype Stacks _


newtype StageList = StageList (Array TemplateStage)
derive instance newtypeStageList :: Newtype StageList _


-- | <p>Another operation has been performed on this stack set since the specified operation was performed. </p>
newtype StaleRequestException = StaleRequestException 
  { 
  }
derive instance newtypeStaleRequestException :: Newtype StaleRequestException _


newtype StopStackSetOperationInput = StopStackSetOperationInput 
  { "StackSetName" :: (StackSetName)
  , "OperationId" :: (ClientRequestToken)
  }
derive instance newtypeStopStackSetOperationInput :: Newtype StopStackSetOperationInput _


newtype StopStackSetOperationOutput = StopStackSetOperationOutput 
  { 
  }
derive instance newtypeStopStackSetOperationOutput :: Newtype StopStackSetOperationOutput _


-- | <p>The Tag type enables you to specify a key-value pair that can be used to store information about an AWS CloudFormation stack.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _


newtype TemplateBody = TemplateBody String
derive instance newtypeTemplateBody :: Newtype TemplateBody _


newtype TemplateDescription = TemplateDescription String
derive instance newtypeTemplateDescription :: Newtype TemplateDescription _


-- | <p>The TemplateParameter data type.</p>
newtype TemplateParameter = TemplateParameter 
  { "ParameterKey" :: NullOrUndefined (ParameterKey)
  , "DefaultValue" :: NullOrUndefined (ParameterValue)
  , "NoEcho" :: NullOrUndefined (NoEcho)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeTemplateParameter :: Newtype TemplateParameter _


newtype TemplateParameters = TemplateParameters (Array TemplateParameter)
derive instance newtypeTemplateParameters :: Newtype TemplateParameters _


newtype TemplateStage = TemplateStage String
derive instance newtypeTemplateStage :: Newtype TemplateStage _


newtype TemplateURL = TemplateURL String
derive instance newtypeTemplateURL :: Newtype TemplateURL _


newtype TimeoutMinutes = TimeoutMinutes Int
derive instance newtypeTimeoutMinutes :: Newtype TimeoutMinutes _


-- | <p>A client request token already exists.</p>
newtype TokenAlreadyExistsException = TokenAlreadyExistsException 
  { 
  }
derive instance newtypeTokenAlreadyExistsException :: Newtype TokenAlreadyExistsException _


newtype TransformName = TransformName String
derive instance newtypeTransformName :: Newtype TransformName _


newtype TransformsList = TransformsList (Array TransformName)
derive instance newtypeTransformsList :: Newtype TransformsList _


newtype Type = Type String
derive instance newtypeType :: Newtype Type _


-- | <p>The input for an <a>UpdateStack</a> action.</p>
newtype UpdateStackInput = UpdateStackInput 
  { "StackName" :: (StackName)
  , "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined (TemplateURL)
  , "UsePreviousTemplate" :: NullOrUndefined (UsePreviousTemplate)
  , "StackPolicyDuringUpdateBody" :: NullOrUndefined (StackPolicyDuringUpdateBody)
  , "StackPolicyDuringUpdateURL" :: NullOrUndefined (StackPolicyDuringUpdateURL)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "ResourceTypes" :: NullOrUndefined (ResourceTypes)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration)
  , "StackPolicyBody" :: NullOrUndefined (StackPolicyBody)
  , "StackPolicyURL" :: NullOrUndefined (StackPolicyURL)
  , "NotificationARNs" :: NullOrUndefined (NotificationARNs)
  , "Tags" :: NullOrUndefined (Tags)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackInput :: Newtype UpdateStackInput _


newtype UpdateStackInstancesInput = UpdateStackInstancesInput 
  { "StackSetName" :: (StackSetName)
  , "Accounts" :: (AccountList)
  , "Regions" :: (RegionList)
  , "ParameterOverrides" :: NullOrUndefined (Parameters)
  , "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences)
  , "OperationId" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackInstancesInput :: Newtype UpdateStackInstancesInput _


newtype UpdateStackInstancesOutput = UpdateStackInstancesOutput 
  { "OperationId" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackInstancesOutput :: Newtype UpdateStackInstancesOutput _


-- | <p>The output for an <a>UpdateStack</a> action.</p>
newtype UpdateStackOutput = UpdateStackOutput 
  { "StackId" :: NullOrUndefined (StackId)
  }
derive instance newtypeUpdateStackOutput :: Newtype UpdateStackOutput _


newtype UpdateStackSetInput = UpdateStackSetInput 
  { "StackSetName" :: (StackSetName)
  , "Description" :: NullOrUndefined (Description)
  , "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined (TemplateURL)
  , "UsePreviousTemplate" :: NullOrUndefined (UsePreviousTemplate)
  , "Parameters" :: NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "Tags" :: NullOrUndefined (Tags)
  , "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences)
  , "OperationId" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackSetInput :: Newtype UpdateStackSetInput _


newtype UpdateStackSetOutput = UpdateStackSetOutput 
  { "OperationId" :: NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackSetOutput :: Newtype UpdateStackSetOutput _


newtype UpdateTerminationProtectionInput = UpdateTerminationProtectionInput 
  { "EnableTerminationProtection" :: (EnableTerminationProtection)
  , "StackName" :: (StackNameOrId)
  }
derive instance newtypeUpdateTerminationProtectionInput :: Newtype UpdateTerminationProtectionInput _


newtype UpdateTerminationProtectionOutput = UpdateTerminationProtectionOutput 
  { "StackId" :: NullOrUndefined (StackId)
  }
derive instance newtypeUpdateTerminationProtectionOutput :: Newtype UpdateTerminationProtectionOutput _


newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _


newtype UsePreviousTemplate = UsePreviousTemplate Boolean
derive instance newtypeUsePreviousTemplate :: Newtype UsePreviousTemplate _


newtype UsePreviousValue = UsePreviousValue Boolean
derive instance newtypeUsePreviousValue :: Newtype UsePreviousValue _


-- | <p>The input for <a>ValidateTemplate</a> action.</p>
newtype ValidateTemplateInput = ValidateTemplateInput 
  { "TemplateBody" :: NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined (TemplateURL)
  }
derive instance newtypeValidateTemplateInput :: Newtype ValidateTemplateInput _


-- | <p>The output for <a>ValidateTemplate</a> action.</p>
newtype ValidateTemplateOutput = ValidateTemplateOutput 
  { "Parameters" :: NullOrUndefined (TemplateParameters)
  , "Description" :: NullOrUndefined (Description)
  , "Capabilities" :: NullOrUndefined (Capabilities)
  , "CapabilitiesReason" :: NullOrUndefined (CapabilitiesReason)
  , "DeclaredTransforms" :: NullOrUndefined (TransformsList)
  }
derive instance newtypeValidateTemplateOutput :: Newtype ValidateTemplateOutput _


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _
