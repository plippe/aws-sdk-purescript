

-- | <fullname>AWS CloudFormation</fullname> <p>AWS CloudFormation allows you to create and manage AWS infrastructure deployments predictably and repeatedly. You can use AWS CloudFormation to leverage AWS products, such as Amazon Elastic Compute Cloud, Amazon Elastic Block Store, Amazon Simple Notification Service, Elastic Load Balancing, and Auto Scaling to build highly-reliable, highly scalable, cost-effective applications without creating or configuring the underlying AWS infrastructure.</p> <p>With AWS CloudFormation, you declare all of your resources and dependencies in a template file. The template defines a collection of resources as a single unit called a stack. AWS CloudFormation creates and deletes all member resources of the stack together and manages all dependencies between the resources for you.</p> <p>For more information about AWS CloudFormation, see the <a href="http://aws.amazon.com/cloudformation/">AWS CloudFormation Product Page</a>.</p> <p>Amazon CloudFormation makes use of other AWS products. If you need additional technical information about a specific AWS product, you can find the product's technical documentation at <a href="http://docs.aws.amazon.com/">docs.aws.amazon.com</a>.</p>
module AWS.CloudFormation where

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

serviceName = "CloudFormation" :: String


-- | <p>Cancels an update on the specified stack. If the call completes successfully, the stack rolls back the update and reverts to the previous stack configuration.</p> <note> <p>You can cancel only stacks that are in the UPDATE_IN_PROGRESS state.</p> </note>
cancelUpdateStack :: forall eff. CancelUpdateStackInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
cancelUpdateStack = Request.request serviceName "cancelUpdateStack" 


-- | <p>For a specified stack that is in the <code>UPDATE_ROLLBACK_FAILED</code> state, continues rolling it back to the <code>UPDATE_ROLLBACK_COMPLETE</code> state. Depending on the cause of the failure, you can manually <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed"> fix the error</a> and continue the rollback. By continuing the rollback, you can return your stack to a working state (the <code>UPDATE_ROLLBACK_COMPLETE</code> state), and then try to update the stack again.</p> <p>A stack goes into the <code>UPDATE_ROLLBACK_FAILED</code> state when AWS CloudFormation cannot roll back all changes after a failed stack update. For example, you might have a stack that is rolling back to an old database instance that was deleted outside of AWS CloudFormation. Because AWS CloudFormation doesn't know the database was deleted, it assumes that the database instance still exists and attempts to roll back to it, causing the update rollback to fail.</p>
continueUpdateRollback :: forall eff. ContinueUpdateRollbackInput -> Aff (exception :: EXCEPTION | eff) ContinueUpdateRollbackOutput
continueUpdateRollback = Request.request serviceName "continueUpdateRollback" 


-- | <p>Creates a list of changes that will be applied to a stack so that you can review the changes before executing them. You can create a change set for a stack that doesn't exist or an existing stack. If you create a change set for a stack that doesn't exist, the change set shows all of the resources that AWS CloudFormation will create. If you create a change set for an existing stack, AWS CloudFormation compares the stack's information with the information that you submit in the change set and lists the differences. Use change sets to understand which resources AWS CloudFormation will create or change, and how it will change resources in an existing stack, before you create or update a stack.</p> <p>To create a change set for a stack that doesn't exist, for the <code>ChangeSetType</code> parameter, specify <code>CREATE</code>. To create a change set for an existing stack, specify <code>UPDATE</code> for the <code>ChangeSetType</code> parameter. After the <code>CreateChangeSet</code> call successfully completes, AWS CloudFormation starts creating the change set. To check the status of the change set or to review it, use the <a>DescribeChangeSet</a> action.</p> <p>When you are satisfied with the changes the change set will make, execute the change set by using the <a>ExecuteChangeSet</a> action. AWS CloudFormation doesn't make changes until you execute the change set.</p>
createChangeSet :: forall eff. CreateChangeSetInput -> Aff (exception :: EXCEPTION | eff) CreateChangeSetOutput
createChangeSet = Request.request serviceName "createChangeSet" 


-- | <p>Creates a stack as specified in the template. After the call completes successfully, the stack creation starts. You can check the status of the stack via the <a>DescribeStacks</a> API.</p>
createStack :: forall eff. CreateStackInput -> Aff (exception :: EXCEPTION | eff) CreateStackOutput
createStack = Request.request serviceName "createStack" 


-- | <p>Creates stack instances for the specified accounts, within the specified regions. A stack instance refers to a stack in a specific account and region. <code>Accounts</code> and <code>Regions</code> are required parameters—you must specify at least one account and one region. </p>
createStackInstances :: forall eff. CreateStackInstancesInput -> Aff (exception :: EXCEPTION | eff) CreateStackInstancesOutput
createStackInstances = Request.request serviceName "createStackInstances" 


-- | <p>Creates a stack set.</p>
createStackSet :: forall eff. CreateStackSetInput -> Aff (exception :: EXCEPTION | eff) CreateStackSetOutput
createStackSet = Request.request serviceName "createStackSet" 


-- | <p>Deletes the specified change set. Deleting change sets ensures that no one executes the wrong change set.</p> <p>If the call successfully completes, AWS CloudFormation successfully deleted the change set.</p>
deleteChangeSet :: forall eff. DeleteChangeSetInput -> Aff (exception :: EXCEPTION | eff) DeleteChangeSetOutput
deleteChangeSet = Request.request serviceName "deleteChangeSet" 


-- | <p>Deletes a specified stack. Once the call completes successfully, stack deletion starts. Deleted stacks do not show up in the <a>DescribeStacks</a> API if the deletion has been completed successfully.</p>
deleteStack :: forall eff. DeleteStackInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteStack = Request.request serviceName "deleteStack" 


-- | <p>Deletes stack instances for the specified accounts, in the specified regions. </p>
deleteStackInstances :: forall eff. DeleteStackInstancesInput -> Aff (exception :: EXCEPTION | eff) DeleteStackInstancesOutput
deleteStackInstances = Request.request serviceName "deleteStackInstances" 


-- | <p>Deletes a stack set. Before you can delete a stack set, all of its member stack instances must be deleted. For more information about how to do this, see <a>DeleteStackInstances</a>. </p>
deleteStackSet :: forall eff. DeleteStackSetInput -> Aff (exception :: EXCEPTION | eff) DeleteStackSetOutput
deleteStackSet = Request.request serviceName "deleteStackSet" 


-- | <p>Retrieves your account's AWS CloudFormation limits, such as the maximum number of stacks that you can create in your account.</p>
describeAccountLimits :: forall eff. DescribeAccountLimitsInput -> Aff (exception :: EXCEPTION | eff) DescribeAccountLimitsOutput
describeAccountLimits = Request.request serviceName "describeAccountLimits" 


-- | <p>Returns the inputs for the change set and a list of changes that AWS CloudFormation will make if you execute the change set. For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html">Updating Stacks Using Change Sets</a> in the AWS CloudFormation User Guide.</p>
describeChangeSet :: forall eff. DescribeChangeSetInput -> Aff (exception :: EXCEPTION | eff) DescribeChangeSetOutput
describeChangeSet = Request.request serviceName "describeChangeSet" 


-- | <p>Returns all stack related events for a specified stack in reverse chronological order. For more information about a stack's event history, go to <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/concept-stack.html">Stacks</a> in the AWS CloudFormation User Guide.</p> <note> <p>You can list events for stacks that have failed to create or have been deleted by specifying the unique stack identifier (stack ID).</p> </note>
describeStackEvents :: forall eff. DescribeStackEventsInput -> Aff (exception :: EXCEPTION | eff) DescribeStackEventsOutput
describeStackEvents = Request.request serviceName "describeStackEvents" 


-- | <p>Returns the stack instance that's associated with the specified stack set, AWS account, and region.</p> <p>For a list of stack instances that are associated with a specific stack set, use <a>ListStackInstances</a>.</p>
describeStackInstance :: forall eff. DescribeStackInstanceInput -> Aff (exception :: EXCEPTION | eff) DescribeStackInstanceOutput
describeStackInstance = Request.request serviceName "describeStackInstance" 


-- | <p>Returns a description of the specified resource in the specified stack.</p> <p>For deleted stacks, DescribeStackResource returns resource information for up to 90 days after the stack has been deleted.</p>
describeStackResource :: forall eff. DescribeStackResourceInput -> Aff (exception :: EXCEPTION | eff) DescribeStackResourceOutput
describeStackResource = Request.request serviceName "describeStackResource" 


-- | <p>Returns AWS resource descriptions for running and deleted stacks. If <code>StackName</code> is specified, all the associated resources that are part of the stack are returned. If <code>PhysicalResourceId</code> is specified, the associated resources of the stack that the resource belongs to are returned.</p> <note> <p>Only the first 100 resources will be returned. If your stack has more resources than this, you should use <code>ListStackResources</code> instead.</p> </note> <p>For deleted stacks, <code>DescribeStackResources</code> returns resource information for up to 90 days after the stack has been deleted.</p> <p>You must specify either <code>StackName</code> or <code>PhysicalResourceId</code>, but not both. In addition, you can specify <code>LogicalResourceId</code> to filter the returned result. For more information about resources, the <code>LogicalResourceId</code> and <code>PhysicalResourceId</code>, go to the <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/">AWS CloudFormation User Guide</a>.</p> <note> <p>A <code>ValidationError</code> is returned if you specify both <code>StackName</code> and <code>PhysicalResourceId</code> in the same request.</p> </note>
describeStackResources :: forall eff. DescribeStackResourcesInput -> Aff (exception :: EXCEPTION | eff) DescribeStackResourcesOutput
describeStackResources = Request.request serviceName "describeStackResources" 


-- | <p>Returns the description of the specified stack set. </p>
describeStackSet :: forall eff. DescribeStackSetInput -> Aff (exception :: EXCEPTION | eff) DescribeStackSetOutput
describeStackSet = Request.request serviceName "describeStackSet" 


-- | <p>Returns the description of the specified stack set operation. </p>
describeStackSetOperation :: forall eff. DescribeStackSetOperationInput -> Aff (exception :: EXCEPTION | eff) DescribeStackSetOperationOutput
describeStackSetOperation = Request.request serviceName "describeStackSetOperation" 


-- | <p>Returns the description for the specified stack; if no stack name was specified, then it returns the description for all the stacks created.</p> <note> <p>If the stack does not exist, an <code>AmazonCloudFormationException</code> is returned.</p> </note>
describeStacks :: forall eff. DescribeStacksInput -> Aff (exception :: EXCEPTION | eff) DescribeStacksOutput
describeStacks = Request.request serviceName "describeStacks" 


-- | <p>Returns the estimated monthly cost of a template. The return value is an AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.</p>
estimateTemplateCost :: forall eff. EstimateTemplateCostInput -> Aff (exception :: EXCEPTION | eff) EstimateTemplateCostOutput
estimateTemplateCost = Request.request serviceName "estimateTemplateCost" 


-- | <p>Updates a stack using the input information that was provided when the specified change set was created. After the call successfully completes, AWS CloudFormation starts updating the stack. Use the <a>DescribeStacks</a> action to view the status of the update.</p> <p>When you execute a change set, AWS CloudFormation deletes all other change sets associated with the stack because they aren't valid for the updated stack.</p> <p>If a stack policy is associated with the stack, AWS CloudFormation enforces the policy during the update. You can't specify a temporary stack policy that overrides the current policy.</p>
executeChangeSet :: forall eff. ExecuteChangeSetInput -> Aff (exception :: EXCEPTION | eff) ExecuteChangeSetOutput
executeChangeSet = Request.request serviceName "executeChangeSet" 


-- | <p>Returns the stack policy for a specified stack. If a stack doesn't have a policy, a null value is returned.</p>
getStackPolicy :: forall eff. GetStackPolicyInput -> Aff (exception :: EXCEPTION | eff) GetStackPolicyOutput
getStackPolicy = Request.request serviceName "getStackPolicy" 


-- | <p>Returns the template body for a specified stack. You can get the template for running or deleted stacks.</p> <p>For deleted stacks, GetTemplate returns the template for up to 90 days after the stack has been deleted.</p> <note> <p> If the template does not exist, a <code>ValidationError</code> is returned. </p> </note>
getTemplate :: forall eff. GetTemplateInput -> Aff (exception :: EXCEPTION | eff) GetTemplateOutput
getTemplate = Request.request serviceName "getTemplate" 


-- | <p>Returns information about a new or existing template. The <code>GetTemplateSummary</code> action is useful for viewing parameter information, such as default parameter values and parameter types, before you create or update a stack or stack set.</p> <p>You can use the <code>GetTemplateSummary</code> action when you submit a template, or you can get template information for a stack set, or a running or deleted stack.</p> <p>For deleted stacks, <code>GetTemplateSummary</code> returns the template information for up to 90 days after the stack has been deleted. If the template does not exist, a <code>ValidationError</code> is returned.</p>
getTemplateSummary :: forall eff. GetTemplateSummaryInput -> Aff (exception :: EXCEPTION | eff) GetTemplateSummaryOutput
getTemplateSummary = Request.request serviceName "getTemplateSummary" 


-- | <p>Returns the ID and status of each active change set for a stack. For example, AWS CloudFormation lists change sets that are in the <code>CREATE_IN_PROGRESS</code> or <code>CREATE_PENDING</code> state.</p>
listChangeSets :: forall eff. ListChangeSetsInput -> Aff (exception :: EXCEPTION | eff) ListChangeSetsOutput
listChangeSets = Request.request serviceName "listChangeSets" 


-- | <p>Lists all exported output values in the account and region in which you call this action. Use this action to see the exported output values that you can import into other stacks. To import values, use the <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html"> <code>Fn::ImportValue</code> </a> function. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-exports.html"> AWS CloudFormation Export Stack Output Values</a>.</p>
listExports :: forall eff. ListExportsInput -> Aff (exception :: EXCEPTION | eff) ListExportsOutput
listExports = Request.request serviceName "listExports" 


-- | <p>Lists all stacks that are importing an exported output value. To modify or remove an exported output value, first use this action to see which stacks are using it. To see the exported output values in your account, see <a>ListExports</a>. </p> <p>For more information about importing an exported output value, see the <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html"> <code>Fn::ImportValue</code> </a> function. </p>
listImports :: forall eff. ListImportsInput -> Aff (exception :: EXCEPTION | eff) ListImportsOutput
listImports = Request.request serviceName "listImports" 


-- | <p>Returns summary information about stack instances that are associated with the specified stack set. You can filter for stack instances that are associated with a specific AWS account name or region.</p>
listStackInstances :: forall eff. ListStackInstancesInput -> Aff (exception :: EXCEPTION | eff) ListStackInstancesOutput
listStackInstances = Request.request serviceName "listStackInstances" 


-- | <p>Returns descriptions of all resources of the specified stack.</p> <p>For deleted stacks, ListStackResources returns resource information for up to 90 days after the stack has been deleted.</p>
listStackResources :: forall eff. ListStackResourcesInput -> Aff (exception :: EXCEPTION | eff) ListStackResourcesOutput
listStackResources = Request.request serviceName "listStackResources" 


-- | <p>Returns summary information about the results of a stack set operation. </p>
listStackSetOperationResults :: forall eff. ListStackSetOperationResultsInput -> Aff (exception :: EXCEPTION | eff) ListStackSetOperationResultsOutput
listStackSetOperationResults = Request.request serviceName "listStackSetOperationResults" 


-- | <p>Returns summary information about operations performed on a stack set. </p>
listStackSetOperations :: forall eff. ListStackSetOperationsInput -> Aff (exception :: EXCEPTION | eff) ListStackSetOperationsOutput
listStackSetOperations = Request.request serviceName "listStackSetOperations" 


-- | <p>Returns summary information about stack sets that are associated with the user.</p>
listStackSets :: forall eff. ListStackSetsInput -> Aff (exception :: EXCEPTION | eff) ListStackSetsOutput
listStackSets = Request.request serviceName "listStackSets" 


-- | <p>Returns the summary information for stacks whose status matches the specified StackStatusFilter. Summary information for stacks that have been deleted is kept for 90 days after the stack is deleted. If no StackStatusFilter is specified, summary information for all stacks is returned (including existing stacks and stacks that have been deleted).</p>
listStacks :: forall eff. ListStacksInput -> Aff (exception :: EXCEPTION | eff) ListStacksOutput
listStacks = Request.request serviceName "listStacks" 


-- | <p>Sets a stack policy for a specified stack.</p>
setStackPolicy :: forall eff. SetStackPolicyInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setStackPolicy = Request.request serviceName "setStackPolicy" 


-- | <p>Sends a signal to the specified resource with a success or failure status. You can use the SignalResource API in conjunction with a creation policy or update policy. AWS CloudFormation doesn't proceed with a stack creation or update until resources receive the required number of signals or the timeout period is exceeded. The SignalResource API is useful in cases where you want to send signals from anywhere other than an Amazon EC2 instance.</p>
signalResource :: forall eff. SignalResourceInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
signalResource = Request.request serviceName "signalResource" 


-- | <p>Stops an in-progress operation on a stack set and its associated stack instances. </p>
stopStackSetOperation :: forall eff. StopStackSetOperationInput -> Aff (exception :: EXCEPTION | eff) StopStackSetOperationOutput
stopStackSetOperation = Request.request serviceName "stopStackSetOperation" 


-- | <p>Updates a stack as specified in the template. After the call completes successfully, the stack update starts. You can check the status of the stack via the <a>DescribeStacks</a> action.</p> <p>To get a copy of the template for an existing stack, you can use the <a>GetTemplate</a> action.</p> <p>For more information about creating an update template, updating a stack, and monitoring the progress of the update, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks.html">Updating a Stack</a>.</p>
updateStack :: forall eff. UpdateStackInput -> Aff (exception :: EXCEPTION | eff) UpdateStackOutput
updateStack = Request.request serviceName "updateStack" 


-- | <p>Updates the parameter values for stack instances for the specified accounts, within the specified regions. A stack instance refers to a stack in a specific account and region. </p> <p>You can only update stack instances in regions and accounts where they already exist; to create additional stack instances, use <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackInstances.html">CreateStackInstances</a>. </p> <p>During stack set updates, any parameters overridden for a stack instance are not updated, but retain their overridden value.</p> <p>You can only update the parameter <i>values</i> that are specified in the stack set; to add or delete a parameter itself, use <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html">UpdateStackSet</a> to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html">UpdateStackSet</a> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using <code>UpdateStackInstances</code>.</p>
updateStackInstances :: forall eff. UpdateStackInstancesInput -> Aff (exception :: EXCEPTION | eff) UpdateStackInstancesOutput
updateStackInstances = Request.request serviceName "updateStackInstances" 


-- | <p>Updates the stack set and <i>all</i> associated stack instances.</p> <p>Even if the stack set operation created by updating the stack set fails (completely or partially, below or above a specified failure tolerance), the stack set is updated with your changes. Subsequent <a>CreateStackInstances</a> calls on the specified stack set use the updated stack set.</p>
updateStackSet :: forall eff. UpdateStackSetInput -> Aff (exception :: EXCEPTION | eff) UpdateStackSetOutput
updateStackSet = Request.request serviceName "updateStackSet" 


-- | <p>Updates termination protection for the specified stack. If a user attempts to delete a stack with termination protection enabled, the operation fails and the stack remains unchanged. For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html">Protecting a Stack From Being Deleted</a> in the <i>AWS CloudFormation User Guide</i>.</p> <p> For <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html">nested stacks</a>, termination protection is set on the root stack and cannot be changed directly on the nested stack.</p>
updateTerminationProtection :: forall eff. UpdateTerminationProtectionInput -> Aff (exception :: EXCEPTION | eff) UpdateTerminationProtectionOutput
updateTerminationProtection = Request.request serviceName "updateTerminationProtection" 


-- | <p>Validates a specified template. AWS CloudFormation first checks if the template is valid JSON. If it isn't, AWS CloudFormation checks if the template is valid YAML. If both these checks fail, AWS CloudFormation returns a template validation error.</p>
validateTemplate :: forall eff. ValidateTemplateInput -> Aff (exception :: EXCEPTION | eff) ValidateTemplateOutput
validateTemplate = Request.request serviceName "validateTemplate" 


newtype Account = Account String
derive instance newtypeAccount :: Newtype Account _
derive instance repGenericAccount :: Generic Account _
instance showAccount :: Show Account where
  show = genericShow
instance decodeAccount :: Decode Account where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccount :: Encode Account where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Structure that contains the results of the account gate function which AWS CloudFormation invokes, if present, before proceeding with a stack set operation in an account and region.</p> <p>For each account and region, AWS CloudFormation lets you specify a Lamdba function that encapsulates any requirements that must be met before CloudFormation can proceed with a stack set operation in that account and region. CloudFormation invokes the function each time a stack set operation is requested for that account and region; if the function returns <code>FAILED</code>, CloudFormation cancels the operation in that account and region, and sets the stack set operation result status for that account and region to <code>FAILED</code>. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-account-gating.html">Configuring a target account gate</a>.</p>
newtype AccountGateResult = AccountGateResult 
  { "Status" :: NullOrUndefined.NullOrUndefined (AccountGateStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (AccountGateStatusReason)
  }
derive instance newtypeAccountGateResult :: Newtype AccountGateResult _
derive instance repGenericAccountGateResult :: Generic AccountGateResult _
instance showAccountGateResult :: Show AccountGateResult where
  show = genericShow
instance decodeAccountGateResult :: Decode AccountGateResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountGateResult :: Encode AccountGateResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountGateStatus = AccountGateStatus String
derive instance newtypeAccountGateStatus :: Newtype AccountGateStatus _
derive instance repGenericAccountGateStatus :: Generic AccountGateStatus _
instance showAccountGateStatus :: Show AccountGateStatus where
  show = genericShow
instance decodeAccountGateStatus :: Decode AccountGateStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountGateStatus :: Encode AccountGateStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountGateStatusReason = AccountGateStatusReason String
derive instance newtypeAccountGateStatusReason :: Newtype AccountGateStatusReason _
derive instance repGenericAccountGateStatusReason :: Generic AccountGateStatusReason _
instance showAccountGateStatusReason :: Show AccountGateStatusReason where
  show = genericShow
instance decodeAccountGateStatusReason :: Decode AccountGateStatusReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountGateStatusReason :: Encode AccountGateStatusReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The AccountLimit data type.</p>
newtype AccountLimit = AccountLimit 
  { "Name" :: NullOrUndefined.NullOrUndefined (LimitName)
  , "Value" :: NullOrUndefined.NullOrUndefined (LimitValue)
  }
derive instance newtypeAccountLimit :: Newtype AccountLimit _
derive instance repGenericAccountLimit :: Generic AccountLimit _
instance showAccountLimit :: Show AccountLimit where
  show = genericShow
instance decodeAccountLimit :: Decode AccountLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountLimit :: Encode AccountLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountLimitList = AccountLimitList (Array AccountLimit)
derive instance newtypeAccountLimitList :: Newtype AccountLimitList _
derive instance repGenericAccountLimitList :: Generic AccountLimitList _
instance showAccountLimitList :: Show AccountLimitList where
  show = genericShow
instance decodeAccountLimitList :: Decode AccountLimitList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountLimitList :: Encode AccountLimitList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountList = AccountList (Array Account)
derive instance newtypeAccountList :: Newtype AccountList _
derive instance repGenericAccountList :: Generic AccountList _
instance showAccountList :: Show AccountList where
  show = genericShow
instance decodeAccountList :: Decode AccountList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountList :: Encode AccountList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedValue = AllowedValue String
derive instance newtypeAllowedValue :: Newtype AllowedValue _
derive instance repGenericAllowedValue :: Generic AllowedValue _
instance showAllowedValue :: Show AllowedValue where
  show = genericShow
instance decodeAllowedValue :: Decode AllowedValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedValue :: Encode AllowedValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllowedValues = AllowedValues (Array AllowedValue)
derive instance newtypeAllowedValues :: Newtype AllowedValues _
derive instance repGenericAllowedValues :: Generic AllowedValues _
instance showAllowedValues :: Show AllowedValues where
  show = genericShow
instance decodeAllowedValues :: Decode AllowedValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllowedValues :: Encode AllowedValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource with the name requested already exists.</p>
newtype AlreadyExistsException = AlreadyExistsException Types.NoArguments
derive instance newtypeAlreadyExistsException :: Newtype AlreadyExistsException _
derive instance repGenericAlreadyExistsException :: Generic AlreadyExistsException _
instance showAlreadyExistsException :: Show AlreadyExistsException where
  show = genericShow
instance decodeAlreadyExistsException :: Decode AlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlreadyExistsException :: Encode AlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _
derive instance repGenericArn :: Generic Arn _
instance showArn :: Show Arn where
  show = genericShow
instance decodeArn :: Decode Arn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArn :: Encode Arn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>CancelUpdateStack</a> action.</p>
newtype CancelUpdateStackInput = CancelUpdateStackInput 
  { "StackName" :: (StackName)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCancelUpdateStackInput :: Newtype CancelUpdateStackInput _
derive instance repGenericCancelUpdateStackInput :: Generic CancelUpdateStackInput _
instance showCancelUpdateStackInput :: Show CancelUpdateStackInput where
  show = genericShow
instance decodeCancelUpdateStackInput :: Decode CancelUpdateStackInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelUpdateStackInput :: Encode CancelUpdateStackInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Capabilities = Capabilities (Array Capability)
derive instance newtypeCapabilities :: Newtype Capabilities _
derive instance repGenericCapabilities :: Generic Capabilities _
instance showCapabilities :: Show Capabilities where
  show = genericShow
instance decodeCapabilities :: Decode Capabilities where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCapabilities :: Encode Capabilities where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CapabilitiesReason = CapabilitiesReason String
derive instance newtypeCapabilitiesReason :: Newtype CapabilitiesReason _
derive instance repGenericCapabilitiesReason :: Generic CapabilitiesReason _
instance showCapabilitiesReason :: Show CapabilitiesReason where
  show = genericShow
instance decodeCapabilitiesReason :: Decode CapabilitiesReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCapabilitiesReason :: Encode CapabilitiesReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Capability = Capability String
derive instance newtypeCapability :: Newtype Capability _
derive instance repGenericCapability :: Generic Capability _
instance showCapability :: Show Capability where
  show = genericShow
instance decodeCapability :: Decode Capability where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCapability :: Encode Capability where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CausingEntity = CausingEntity String
derive instance newtypeCausingEntity :: Newtype CausingEntity _
derive instance repGenericCausingEntity :: Generic CausingEntity _
instance showCausingEntity :: Show CausingEntity where
  show = genericShow
instance decodeCausingEntity :: Decode CausingEntity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCausingEntity :: Encode CausingEntity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Change</code> structure describes the changes AWS CloudFormation will perform if you execute the change set.</p>
newtype Change = Change 
  { "Type" :: NullOrUndefined.NullOrUndefined (ChangeType)
  , "ResourceChange" :: NullOrUndefined.NullOrUndefined (ResourceChange)
  }
derive instance newtypeChange :: Newtype Change _
derive instance repGenericChange :: Generic Change _
instance showChange :: Show Change where
  show = genericShow
instance decodeChange :: Decode Change where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChange :: Encode Change where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeAction = ChangeAction String
derive instance newtypeChangeAction :: Newtype ChangeAction _
derive instance repGenericChangeAction :: Generic ChangeAction _
instance showChangeAction :: Show ChangeAction where
  show = genericShow
instance decodeChangeAction :: Decode ChangeAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeAction :: Encode ChangeAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeSetId = ChangeSetId String
derive instance newtypeChangeSetId :: Newtype ChangeSetId _
derive instance repGenericChangeSetId :: Generic ChangeSetId _
instance showChangeSetId :: Show ChangeSetId where
  show = genericShow
instance decodeChangeSetId :: Decode ChangeSetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetId :: Encode ChangeSetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeSetName = ChangeSetName String
derive instance newtypeChangeSetName :: Newtype ChangeSetName _
derive instance repGenericChangeSetName :: Generic ChangeSetName _
instance showChangeSetName :: Show ChangeSetName where
  show = genericShow
instance decodeChangeSetName :: Decode ChangeSetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetName :: Encode ChangeSetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeSetNameOrId = ChangeSetNameOrId String
derive instance newtypeChangeSetNameOrId :: Newtype ChangeSetNameOrId _
derive instance repGenericChangeSetNameOrId :: Generic ChangeSetNameOrId _
instance showChangeSetNameOrId :: Show ChangeSetNameOrId where
  show = genericShow
instance decodeChangeSetNameOrId :: Decode ChangeSetNameOrId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetNameOrId :: Encode ChangeSetNameOrId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified change set name or ID doesn't exit. To view valid change sets for a stack, use the <code>ListChangeSets</code> action.</p>
newtype ChangeSetNotFoundException = ChangeSetNotFoundException Types.NoArguments
derive instance newtypeChangeSetNotFoundException :: Newtype ChangeSetNotFoundException _
derive instance repGenericChangeSetNotFoundException :: Generic ChangeSetNotFoundException _
instance showChangeSetNotFoundException :: Show ChangeSetNotFoundException where
  show = genericShow
instance decodeChangeSetNotFoundException :: Decode ChangeSetNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetNotFoundException :: Encode ChangeSetNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeSetStatus = ChangeSetStatus String
derive instance newtypeChangeSetStatus :: Newtype ChangeSetStatus _
derive instance repGenericChangeSetStatus :: Generic ChangeSetStatus _
instance showChangeSetStatus :: Show ChangeSetStatus where
  show = genericShow
instance decodeChangeSetStatus :: Decode ChangeSetStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetStatus :: Encode ChangeSetStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeSetStatusReason = ChangeSetStatusReason String
derive instance newtypeChangeSetStatusReason :: Newtype ChangeSetStatusReason _
derive instance repGenericChangeSetStatusReason :: Generic ChangeSetStatusReason _
instance showChangeSetStatusReason :: Show ChangeSetStatusReason where
  show = genericShow
instance decodeChangeSetStatusReason :: Decode ChangeSetStatusReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetStatusReason :: Encode ChangeSetStatusReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeSetSummaries = ChangeSetSummaries (Array ChangeSetSummary)
derive instance newtypeChangeSetSummaries :: Newtype ChangeSetSummaries _
derive instance repGenericChangeSetSummaries :: Generic ChangeSetSummaries _
instance showChangeSetSummaries :: Show ChangeSetSummaries where
  show = genericShow
instance decodeChangeSetSummaries :: Decode ChangeSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetSummaries :: Encode ChangeSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ChangeSetSummary</code> structure describes a change set, its status, and the stack with which it's associated.</p>
newtype ChangeSetSummary = ChangeSetSummary 
  { "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "StackName" :: NullOrUndefined.NullOrUndefined (StackName)
  , "ChangeSetId" :: NullOrUndefined.NullOrUndefined (ChangeSetId)
  , "ChangeSetName" :: NullOrUndefined.NullOrUndefined (ChangeSetName)
  , "ExecutionStatus" :: NullOrUndefined.NullOrUndefined (ExecutionStatus)
  , "Status" :: NullOrUndefined.NullOrUndefined (ChangeSetStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (ChangeSetStatusReason)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (CreationTime)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeChangeSetSummary :: Newtype ChangeSetSummary _
derive instance repGenericChangeSetSummary :: Generic ChangeSetSummary _
instance showChangeSetSummary :: Show ChangeSetSummary where
  show = genericShow
instance decodeChangeSetSummary :: Decode ChangeSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetSummary :: Encode ChangeSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeSetType = ChangeSetType String
derive instance newtypeChangeSetType :: Newtype ChangeSetType _
derive instance repGenericChangeSetType :: Generic ChangeSetType _
instance showChangeSetType :: Show ChangeSetType where
  show = genericShow
instance decodeChangeSetType :: Decode ChangeSetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSetType :: Encode ChangeSetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeSource = ChangeSource String
derive instance newtypeChangeSource :: Newtype ChangeSource _
derive instance repGenericChangeSource :: Generic ChangeSource _
instance showChangeSource :: Show ChangeSource where
  show = genericShow
instance decodeChangeSource :: Decode ChangeSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeSource :: Encode ChangeSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeType = ChangeType String
derive instance newtypeChangeType :: Newtype ChangeType _
derive instance repGenericChangeType :: Generic ChangeType _
instance showChangeType :: Show ChangeType where
  show = genericShow
instance decodeChangeType :: Decode ChangeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeType :: Encode ChangeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Changes = Changes (Array Change)
derive instance newtypeChanges :: Newtype Changes _
derive instance repGenericChanges :: Generic Changes _
instance showChanges :: Show Changes where
  show = genericShow
instance decodeChanges :: Decode Changes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChanges :: Encode Changes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientRequestToken = ClientRequestToken String
derive instance newtypeClientRequestToken :: Newtype ClientRequestToken _
derive instance repGenericClientRequestToken :: Generic ClientRequestToken _
instance showClientRequestToken :: Show ClientRequestToken where
  show = genericShow
instance decodeClientRequestToken :: Decode ClientRequestToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientRequestToken :: Encode ClientRequestToken where
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


-- | <p>The input for the <a>ContinueUpdateRollback</a> action.</p>
newtype ContinueUpdateRollbackInput = ContinueUpdateRollbackInput 
  { "StackName" :: (StackNameOrId)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "ResourcesToSkip" :: NullOrUndefined.NullOrUndefined (ResourcesToSkip)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeContinueUpdateRollbackInput :: Newtype ContinueUpdateRollbackInput _
derive instance repGenericContinueUpdateRollbackInput :: Generic ContinueUpdateRollbackInput _
instance showContinueUpdateRollbackInput :: Show ContinueUpdateRollbackInput where
  show = genericShow
instance decodeContinueUpdateRollbackInput :: Decode ContinueUpdateRollbackInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContinueUpdateRollbackInput :: Encode ContinueUpdateRollbackInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for a <a>ContinueUpdateRollback</a> action.</p>
newtype ContinueUpdateRollbackOutput = ContinueUpdateRollbackOutput Types.NoArguments
derive instance newtypeContinueUpdateRollbackOutput :: Newtype ContinueUpdateRollbackOutput _
derive instance repGenericContinueUpdateRollbackOutput :: Generic ContinueUpdateRollbackOutput _
instance showContinueUpdateRollbackOutput :: Show ContinueUpdateRollbackOutput where
  show = genericShow
instance decodeContinueUpdateRollbackOutput :: Decode ContinueUpdateRollbackOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContinueUpdateRollbackOutput :: Encode ContinueUpdateRollbackOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>CreateChangeSet</a> action.</p>
newtype CreateChangeSetInput = CreateChangeSetInput 
  { "StackName" :: (StackNameOrId)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined.NullOrUndefined (TemplateURL)
  , "UsePreviousTemplate" :: NullOrUndefined.NullOrUndefined (UsePreviousTemplate)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "ResourceTypes" :: NullOrUndefined.NullOrUndefined (ResourceTypes)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "RollbackConfiguration" :: NullOrUndefined.NullOrUndefined (RollbackConfiguration)
  , "NotificationARNs" :: NullOrUndefined.NullOrUndefined (NotificationARNs)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  , "ChangeSetName" :: (ChangeSetName)
  , "ClientToken" :: NullOrUndefined.NullOrUndefined (ClientToken)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "ChangeSetType" :: NullOrUndefined.NullOrUndefined (ChangeSetType)
  }
derive instance newtypeCreateChangeSetInput :: Newtype CreateChangeSetInput _
derive instance repGenericCreateChangeSetInput :: Generic CreateChangeSetInput _
instance showCreateChangeSetInput :: Show CreateChangeSetInput where
  show = genericShow
instance decodeCreateChangeSetInput :: Decode CreateChangeSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateChangeSetInput :: Encode CreateChangeSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>CreateChangeSet</a> action.</p>
newtype CreateChangeSetOutput = CreateChangeSetOutput 
  { "Id" :: NullOrUndefined.NullOrUndefined (ChangeSetId)
  , "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  }
derive instance newtypeCreateChangeSetOutput :: Newtype CreateChangeSetOutput _
derive instance repGenericCreateChangeSetOutput :: Generic CreateChangeSetOutput _
instance showCreateChangeSetOutput :: Show CreateChangeSetOutput where
  show = genericShow
instance decodeCreateChangeSetOutput :: Decode CreateChangeSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateChangeSetOutput :: Encode CreateChangeSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for <a>CreateStack</a> action.</p>
newtype CreateStackInput = CreateStackInput 
  { "StackName" :: (StackName)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined.NullOrUndefined (TemplateURL)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "DisableRollback" :: NullOrUndefined.NullOrUndefined (DisableRollback)
  , "RollbackConfiguration" :: NullOrUndefined.NullOrUndefined (RollbackConfiguration)
  , "TimeoutInMinutes" :: NullOrUndefined.NullOrUndefined (TimeoutMinutes)
  , "NotificationARNs" :: NullOrUndefined.NullOrUndefined (NotificationARNs)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "ResourceTypes" :: NullOrUndefined.NullOrUndefined (ResourceTypes)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "OnFailure" :: NullOrUndefined.NullOrUndefined (OnFailure)
  , "StackPolicyBody" :: NullOrUndefined.NullOrUndefined (StackPolicyBody)
  , "StackPolicyURL" :: NullOrUndefined.NullOrUndefined (StackPolicyURL)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "EnableTerminationProtection" :: NullOrUndefined.NullOrUndefined (EnableTerminationProtection)
  }
derive instance newtypeCreateStackInput :: Newtype CreateStackInput _
derive instance repGenericCreateStackInput :: Generic CreateStackInput _
instance showCreateStackInput :: Show CreateStackInput where
  show = genericShow
instance decodeCreateStackInput :: Decode CreateStackInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStackInput :: Encode CreateStackInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStackInstancesInput = CreateStackInstancesInput 
  { "StackSetName" :: (StackSetName)
  , "Accounts" :: (AccountList)
  , "Regions" :: (RegionList)
  , "ParameterOverrides" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "OperationPreferences" :: NullOrUndefined.NullOrUndefined (StackSetOperationPreferences)
  , "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreateStackInstancesInput :: Newtype CreateStackInstancesInput _
derive instance repGenericCreateStackInstancesInput :: Generic CreateStackInstancesInput _
instance showCreateStackInstancesInput :: Show CreateStackInstancesInput where
  show = genericShow
instance decodeCreateStackInstancesInput :: Decode CreateStackInstancesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStackInstancesInput :: Encode CreateStackInstancesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStackInstancesOutput = CreateStackInstancesOutput 
  { "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreateStackInstancesOutput :: Newtype CreateStackInstancesOutput _
derive instance repGenericCreateStackInstancesOutput :: Generic CreateStackInstancesOutput _
instance showCreateStackInstancesOutput :: Show CreateStackInstancesOutput where
  show = genericShow
instance decodeCreateStackInstancesOutput :: Decode CreateStackInstancesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStackInstancesOutput :: Encode CreateStackInstancesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for a <a>CreateStack</a> action.</p>
newtype CreateStackOutput = CreateStackOutput 
  { "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  }
derive instance newtypeCreateStackOutput :: Newtype CreateStackOutput _
derive instance repGenericCreateStackOutput :: Generic CreateStackOutput _
instance showCreateStackOutput :: Show CreateStackOutput where
  show = genericShow
instance decodeCreateStackOutput :: Decode CreateStackOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStackOutput :: Encode CreateStackOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStackSetInput = CreateStackSetInput 
  { "StackSetName" :: (StackSetName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined.NullOrUndefined (TemplateURL)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeCreateStackSetInput :: Newtype CreateStackSetInput _
derive instance repGenericCreateStackSetInput :: Generic CreateStackSetInput _
instance showCreateStackSetInput :: Show CreateStackSetInput where
  show = genericShow
instance decodeCreateStackSetInput :: Decode CreateStackSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStackSetInput :: Encode CreateStackSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStackSetOutput = CreateStackSetOutput 
  { "StackSetId" :: NullOrUndefined.NullOrUndefined (StackSetId)
  }
derive instance newtypeCreateStackSetOutput :: Newtype CreateStackSetOutput _
derive instance repGenericCreateStackSetOutput :: Generic CreateStackSetOutput _
instance showCreateStackSetOutput :: Show CreateStackSetOutput where
  show = genericShow
instance decodeCreateStackSetOutput :: Decode CreateStackSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStackSetOutput :: Encode CreateStackSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified resource exists, but has been changed.</p>
newtype CreatedButModifiedException = CreatedButModifiedException Types.NoArguments
derive instance newtypeCreatedButModifiedException :: Newtype CreatedButModifiedException _
derive instance repGenericCreatedButModifiedException :: Generic CreatedButModifiedException _
instance showCreatedButModifiedException :: Show CreatedButModifiedException where
  show = genericShow
instance decodeCreatedButModifiedException :: Decode CreatedButModifiedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatedButModifiedException :: Encode CreatedButModifiedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreationTime = CreationTime Number
derive instance newtypeCreationTime :: Newtype CreationTime _
derive instance repGenericCreationTime :: Generic CreationTime _
instance showCreationTime :: Show CreationTime where
  show = genericShow
instance decodeCreationTime :: Decode CreationTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreationTime :: Encode CreationTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>DeleteChangeSet</a> action.</p>
newtype DeleteChangeSetInput = DeleteChangeSetInput 
  { "ChangeSetName" :: (ChangeSetNameOrId)
  , "StackName" :: NullOrUndefined.NullOrUndefined (StackNameOrId)
  }
derive instance newtypeDeleteChangeSetInput :: Newtype DeleteChangeSetInput _
derive instance repGenericDeleteChangeSetInput :: Generic DeleteChangeSetInput _
instance showDeleteChangeSetInput :: Show DeleteChangeSetInput where
  show = genericShow
instance decodeDeleteChangeSetInput :: Decode DeleteChangeSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteChangeSetInput :: Encode DeleteChangeSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>DeleteChangeSet</a> action.</p>
newtype DeleteChangeSetOutput = DeleteChangeSetOutput Types.NoArguments
derive instance newtypeDeleteChangeSetOutput :: Newtype DeleteChangeSetOutput _
derive instance repGenericDeleteChangeSetOutput :: Generic DeleteChangeSetOutput _
instance showDeleteChangeSetOutput :: Show DeleteChangeSetOutput where
  show = genericShow
instance decodeDeleteChangeSetOutput :: Decode DeleteChangeSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteChangeSetOutput :: Encode DeleteChangeSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for <a>DeleteStack</a> action.</p>
newtype DeleteStackInput = DeleteStackInput 
  { "StackName" :: (StackName)
  , "RetainResources" :: NullOrUndefined.NullOrUndefined (RetainResources)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeDeleteStackInput :: Newtype DeleteStackInput _
derive instance repGenericDeleteStackInput :: Generic DeleteStackInput _
instance showDeleteStackInput :: Show DeleteStackInput where
  show = genericShow
instance decodeDeleteStackInput :: Decode DeleteStackInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStackInput :: Encode DeleteStackInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStackInstancesInput = DeleteStackInstancesInput 
  { "StackSetName" :: (StackSetName)
  , "Accounts" :: (AccountList)
  , "Regions" :: (RegionList)
  , "OperationPreferences" :: NullOrUndefined.NullOrUndefined (StackSetOperationPreferences)
  , "RetainStacks" :: (RetainStacks)
  , "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeDeleteStackInstancesInput :: Newtype DeleteStackInstancesInput _
derive instance repGenericDeleteStackInstancesInput :: Generic DeleteStackInstancesInput _
instance showDeleteStackInstancesInput :: Show DeleteStackInstancesInput where
  show = genericShow
instance decodeDeleteStackInstancesInput :: Decode DeleteStackInstancesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStackInstancesInput :: Encode DeleteStackInstancesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStackInstancesOutput = DeleteStackInstancesOutput 
  { "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeDeleteStackInstancesOutput :: Newtype DeleteStackInstancesOutput _
derive instance repGenericDeleteStackInstancesOutput :: Generic DeleteStackInstancesOutput _
instance showDeleteStackInstancesOutput :: Show DeleteStackInstancesOutput where
  show = genericShow
instance decodeDeleteStackInstancesOutput :: Decode DeleteStackInstancesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStackInstancesOutput :: Encode DeleteStackInstancesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStackSetInput = DeleteStackSetInput 
  { "StackSetName" :: (StackSetName)
  }
derive instance newtypeDeleteStackSetInput :: Newtype DeleteStackSetInput _
derive instance repGenericDeleteStackSetInput :: Generic DeleteStackSetInput _
instance showDeleteStackSetInput :: Show DeleteStackSetInput where
  show = genericShow
instance decodeDeleteStackSetInput :: Decode DeleteStackSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStackSetInput :: Encode DeleteStackSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStackSetOutput = DeleteStackSetOutput Types.NoArguments
derive instance newtypeDeleteStackSetOutput :: Newtype DeleteStackSetOutput _
derive instance repGenericDeleteStackSetOutput :: Generic DeleteStackSetOutput _
instance showDeleteStackSetOutput :: Show DeleteStackSetOutput where
  show = genericShow
instance decodeDeleteStackSetOutput :: Decode DeleteStackSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStackSetOutput :: Encode DeleteStackSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletionTime = DeletionTime Number
derive instance newtypeDeletionTime :: Newtype DeletionTime _
derive instance repGenericDeletionTime :: Generic DeletionTime _
instance showDeletionTime :: Show DeletionTime where
  show = genericShow
instance decodeDeletionTime :: Decode DeletionTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletionTime :: Encode DeletionTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>DescribeAccountLimits</a> action.</p>
newtype DescribeAccountLimitsInput = DescribeAccountLimitsInput 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAccountLimitsInput :: Newtype DescribeAccountLimitsInput _
derive instance repGenericDescribeAccountLimitsInput :: Generic DescribeAccountLimitsInput _
instance showDescribeAccountLimitsInput :: Show DescribeAccountLimitsInput where
  show = genericShow
instance decodeDescribeAccountLimitsInput :: Decode DescribeAccountLimitsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAccountLimitsInput :: Encode DescribeAccountLimitsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>DescribeAccountLimits</a> action.</p>
newtype DescribeAccountLimitsOutput = DescribeAccountLimitsOutput 
  { "AccountLimits" :: NullOrUndefined.NullOrUndefined (AccountLimitList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAccountLimitsOutput :: Newtype DescribeAccountLimitsOutput _
derive instance repGenericDescribeAccountLimitsOutput :: Generic DescribeAccountLimitsOutput _
instance showDescribeAccountLimitsOutput :: Show DescribeAccountLimitsOutput where
  show = genericShow
instance decodeDescribeAccountLimitsOutput :: Decode DescribeAccountLimitsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAccountLimitsOutput :: Encode DescribeAccountLimitsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>DescribeChangeSet</a> action.</p>
newtype DescribeChangeSetInput = DescribeChangeSetInput 
  { "ChangeSetName" :: (ChangeSetNameOrId)
  , "StackName" :: NullOrUndefined.NullOrUndefined (StackNameOrId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeChangeSetInput :: Newtype DescribeChangeSetInput _
derive instance repGenericDescribeChangeSetInput :: Generic DescribeChangeSetInput _
instance showDescribeChangeSetInput :: Show DescribeChangeSetInput where
  show = genericShow
instance decodeDescribeChangeSetInput :: Decode DescribeChangeSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeChangeSetInput :: Encode DescribeChangeSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>DescribeChangeSet</a> action.</p>
newtype DescribeChangeSetOutput = DescribeChangeSetOutput 
  { "ChangeSetName" :: NullOrUndefined.NullOrUndefined (ChangeSetName)
  , "ChangeSetId" :: NullOrUndefined.NullOrUndefined (ChangeSetId)
  , "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "StackName" :: NullOrUndefined.NullOrUndefined (StackName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (CreationTime)
  , "ExecutionStatus" :: NullOrUndefined.NullOrUndefined (ExecutionStatus)
  , "Status" :: NullOrUndefined.NullOrUndefined (ChangeSetStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (ChangeSetStatusReason)
  , "NotificationARNs" :: NullOrUndefined.NullOrUndefined (NotificationARNs)
  , "RollbackConfiguration" :: NullOrUndefined.NullOrUndefined (RollbackConfiguration)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  , "Changes" :: NullOrUndefined.NullOrUndefined (Changes)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeChangeSetOutput :: Newtype DescribeChangeSetOutput _
derive instance repGenericDescribeChangeSetOutput :: Generic DescribeChangeSetOutput _
instance showDescribeChangeSetOutput :: Show DescribeChangeSetOutput where
  show = genericShow
instance decodeDescribeChangeSetOutput :: Decode DescribeChangeSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeChangeSetOutput :: Encode DescribeChangeSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for <a>DescribeStackEvents</a> action.</p>
newtype DescribeStackEventsInput = DescribeStackEventsInput 
  { "StackName" :: NullOrUndefined.NullOrUndefined (StackName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeStackEventsInput :: Newtype DescribeStackEventsInput _
derive instance repGenericDescribeStackEventsInput :: Generic DescribeStackEventsInput _
instance showDescribeStackEventsInput :: Show DescribeStackEventsInput where
  show = genericShow
instance decodeDescribeStackEventsInput :: Decode DescribeStackEventsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackEventsInput :: Encode DescribeStackEventsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for a <a>DescribeStackEvents</a> action.</p>
newtype DescribeStackEventsOutput = DescribeStackEventsOutput 
  { "StackEvents" :: NullOrUndefined.NullOrUndefined (StackEvents)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeStackEventsOutput :: Newtype DescribeStackEventsOutput _
derive instance repGenericDescribeStackEventsOutput :: Generic DescribeStackEventsOutput _
instance showDescribeStackEventsOutput :: Show DescribeStackEventsOutput where
  show = genericShow
instance decodeDescribeStackEventsOutput :: Decode DescribeStackEventsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackEventsOutput :: Encode DescribeStackEventsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStackInstanceInput = DescribeStackInstanceInput 
  { "StackSetName" :: (StackSetName)
  , "StackInstanceAccount" :: (Account)
  , "StackInstanceRegion" :: (Region)
  }
derive instance newtypeDescribeStackInstanceInput :: Newtype DescribeStackInstanceInput _
derive instance repGenericDescribeStackInstanceInput :: Generic DescribeStackInstanceInput _
instance showDescribeStackInstanceInput :: Show DescribeStackInstanceInput where
  show = genericShow
instance decodeDescribeStackInstanceInput :: Decode DescribeStackInstanceInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackInstanceInput :: Encode DescribeStackInstanceInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStackInstanceOutput = DescribeStackInstanceOutput 
  { "StackInstance" :: NullOrUndefined.NullOrUndefined (StackInstance)
  }
derive instance newtypeDescribeStackInstanceOutput :: Newtype DescribeStackInstanceOutput _
derive instance repGenericDescribeStackInstanceOutput :: Generic DescribeStackInstanceOutput _
instance showDescribeStackInstanceOutput :: Show DescribeStackInstanceOutput where
  show = genericShow
instance decodeDescribeStackInstanceOutput :: Decode DescribeStackInstanceOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackInstanceOutput :: Encode DescribeStackInstanceOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for <a>DescribeStackResource</a> action.</p>
newtype DescribeStackResourceInput = DescribeStackResourceInput 
  { "StackName" :: (StackName)
  , "LogicalResourceId" :: (LogicalResourceId)
  }
derive instance newtypeDescribeStackResourceInput :: Newtype DescribeStackResourceInput _
derive instance repGenericDescribeStackResourceInput :: Generic DescribeStackResourceInput _
instance showDescribeStackResourceInput :: Show DescribeStackResourceInput where
  show = genericShow
instance decodeDescribeStackResourceInput :: Decode DescribeStackResourceInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackResourceInput :: Encode DescribeStackResourceInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for a <a>DescribeStackResource</a> action.</p>
newtype DescribeStackResourceOutput = DescribeStackResourceOutput 
  { "StackResourceDetail" :: NullOrUndefined.NullOrUndefined (StackResourceDetail)
  }
derive instance newtypeDescribeStackResourceOutput :: Newtype DescribeStackResourceOutput _
derive instance repGenericDescribeStackResourceOutput :: Generic DescribeStackResourceOutput _
instance showDescribeStackResourceOutput :: Show DescribeStackResourceOutput where
  show = genericShow
instance decodeDescribeStackResourceOutput :: Decode DescribeStackResourceOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackResourceOutput :: Encode DescribeStackResourceOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for <a>DescribeStackResources</a> action.</p>
newtype DescribeStackResourcesInput = DescribeStackResourcesInput 
  { "StackName" :: NullOrUndefined.NullOrUndefined (StackName)
  , "LogicalResourceId" :: NullOrUndefined.NullOrUndefined (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined.NullOrUndefined (PhysicalResourceId)
  }
derive instance newtypeDescribeStackResourcesInput :: Newtype DescribeStackResourcesInput _
derive instance repGenericDescribeStackResourcesInput :: Generic DescribeStackResourcesInput _
instance showDescribeStackResourcesInput :: Show DescribeStackResourcesInput where
  show = genericShow
instance decodeDescribeStackResourcesInput :: Decode DescribeStackResourcesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackResourcesInput :: Encode DescribeStackResourcesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for a <a>DescribeStackResources</a> action.</p>
newtype DescribeStackResourcesOutput = DescribeStackResourcesOutput 
  { "StackResources" :: NullOrUndefined.NullOrUndefined (StackResources)
  }
derive instance newtypeDescribeStackResourcesOutput :: Newtype DescribeStackResourcesOutput _
derive instance repGenericDescribeStackResourcesOutput :: Generic DescribeStackResourcesOutput _
instance showDescribeStackResourcesOutput :: Show DescribeStackResourcesOutput where
  show = genericShow
instance decodeDescribeStackResourcesOutput :: Decode DescribeStackResourcesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackResourcesOutput :: Encode DescribeStackResourcesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStackSetInput = DescribeStackSetInput 
  { "StackSetName" :: (StackSetName)
  }
derive instance newtypeDescribeStackSetInput :: Newtype DescribeStackSetInput _
derive instance repGenericDescribeStackSetInput :: Generic DescribeStackSetInput _
instance showDescribeStackSetInput :: Show DescribeStackSetInput where
  show = genericShow
instance decodeDescribeStackSetInput :: Decode DescribeStackSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackSetInput :: Encode DescribeStackSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStackSetOperationInput = DescribeStackSetOperationInput 
  { "StackSetName" :: (StackSetName)
  , "OperationId" :: (ClientRequestToken)
  }
derive instance newtypeDescribeStackSetOperationInput :: Newtype DescribeStackSetOperationInput _
derive instance repGenericDescribeStackSetOperationInput :: Generic DescribeStackSetOperationInput _
instance showDescribeStackSetOperationInput :: Show DescribeStackSetOperationInput where
  show = genericShow
instance decodeDescribeStackSetOperationInput :: Decode DescribeStackSetOperationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackSetOperationInput :: Encode DescribeStackSetOperationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStackSetOperationOutput = DescribeStackSetOperationOutput 
  { "StackSetOperation" :: NullOrUndefined.NullOrUndefined (StackSetOperation)
  }
derive instance newtypeDescribeStackSetOperationOutput :: Newtype DescribeStackSetOperationOutput _
derive instance repGenericDescribeStackSetOperationOutput :: Generic DescribeStackSetOperationOutput _
instance showDescribeStackSetOperationOutput :: Show DescribeStackSetOperationOutput where
  show = genericShow
instance decodeDescribeStackSetOperationOutput :: Decode DescribeStackSetOperationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackSetOperationOutput :: Encode DescribeStackSetOperationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStackSetOutput = DescribeStackSetOutput 
  { "StackSet" :: NullOrUndefined.NullOrUndefined (StackSet)
  }
derive instance newtypeDescribeStackSetOutput :: Newtype DescribeStackSetOutput _
derive instance repGenericDescribeStackSetOutput :: Generic DescribeStackSetOutput _
instance showDescribeStackSetOutput :: Show DescribeStackSetOutput where
  show = genericShow
instance decodeDescribeStackSetOutput :: Decode DescribeStackSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStackSetOutput :: Encode DescribeStackSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for <a>DescribeStacks</a> action.</p>
newtype DescribeStacksInput = DescribeStacksInput 
  { "StackName" :: NullOrUndefined.NullOrUndefined (StackName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeStacksInput :: Newtype DescribeStacksInput _
derive instance repGenericDescribeStacksInput :: Generic DescribeStacksInput _
instance showDescribeStacksInput :: Show DescribeStacksInput where
  show = genericShow
instance decodeDescribeStacksInput :: Decode DescribeStacksInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStacksInput :: Encode DescribeStacksInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for a <a>DescribeStacks</a> action.</p>
newtype DescribeStacksOutput = DescribeStacksOutput 
  { "Stacks" :: NullOrUndefined.NullOrUndefined (Stacks)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeStacksOutput :: Newtype DescribeStacksOutput _
derive instance repGenericDescribeStacksOutput :: Generic DescribeStacksOutput _
instance showDescribeStacksOutput :: Show DescribeStacksOutput where
  show = genericShow
instance decodeDescribeStacksOutput :: Decode DescribeStacksOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStacksOutput :: Encode DescribeStacksOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisableRollback = DisableRollback Boolean
derive instance newtypeDisableRollback :: Newtype DisableRollback _
derive instance repGenericDisableRollback :: Generic DisableRollback _
instance showDisableRollback :: Show DisableRollback where
  show = genericShow
instance decodeDisableRollback :: Decode DisableRollback where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableRollback :: Encode DisableRollback where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnableTerminationProtection = EnableTerminationProtection Boolean
derive instance newtypeEnableTerminationProtection :: Newtype EnableTerminationProtection _
derive instance repGenericEnableTerminationProtection :: Generic EnableTerminationProtection _
instance showEnableTerminationProtection :: Show EnableTerminationProtection where
  show = genericShow
instance decodeEnableTerminationProtection :: Decode EnableTerminationProtection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableTerminationProtection :: Encode EnableTerminationProtection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for an <a>EstimateTemplateCost</a> action.</p>
newtype EstimateTemplateCostInput = EstimateTemplateCostInput 
  { "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined.NullOrUndefined (TemplateURL)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  }
derive instance newtypeEstimateTemplateCostInput :: Newtype EstimateTemplateCostInput _
derive instance repGenericEstimateTemplateCostInput :: Generic EstimateTemplateCostInput _
instance showEstimateTemplateCostInput :: Show EstimateTemplateCostInput where
  show = genericShow
instance decodeEstimateTemplateCostInput :: Decode EstimateTemplateCostInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEstimateTemplateCostInput :: Encode EstimateTemplateCostInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for a <a>EstimateTemplateCost</a> action.</p>
newtype EstimateTemplateCostOutput = EstimateTemplateCostOutput 
  { "Url" :: NullOrUndefined.NullOrUndefined (Url)
  }
derive instance newtypeEstimateTemplateCostOutput :: Newtype EstimateTemplateCostOutput _
derive instance repGenericEstimateTemplateCostOutput :: Generic EstimateTemplateCostOutput _
instance showEstimateTemplateCostOutput :: Show EstimateTemplateCostOutput where
  show = genericShow
instance decodeEstimateTemplateCostOutput :: Decode EstimateTemplateCostOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEstimateTemplateCostOutput :: Encode EstimateTemplateCostOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EvaluationType = EvaluationType String
derive instance newtypeEvaluationType :: Newtype EvaluationType _
derive instance repGenericEvaluationType :: Generic EvaluationType _
instance showEvaluationType :: Show EvaluationType where
  show = genericShow
instance decodeEvaluationType :: Decode EvaluationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluationType :: Encode EvaluationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventId = EventId String
derive instance newtypeEventId :: Newtype EventId _
derive instance repGenericEventId :: Generic EventId _
instance showEventId :: Show EventId where
  show = genericShow
instance decodeEventId :: Decode EventId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventId :: Encode EventId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>ExecuteChangeSet</a> action.</p>
newtype ExecuteChangeSetInput = ExecuteChangeSetInput 
  { "ChangeSetName" :: (ChangeSetNameOrId)
  , "StackName" :: NullOrUndefined.NullOrUndefined (StackNameOrId)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeExecuteChangeSetInput :: Newtype ExecuteChangeSetInput _
derive instance repGenericExecuteChangeSetInput :: Generic ExecuteChangeSetInput _
instance showExecuteChangeSetInput :: Show ExecuteChangeSetInput where
  show = genericShow
instance decodeExecuteChangeSetInput :: Decode ExecuteChangeSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecuteChangeSetInput :: Encode ExecuteChangeSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>ExecuteChangeSet</a> action.</p>
newtype ExecuteChangeSetOutput = ExecuteChangeSetOutput Types.NoArguments
derive instance newtypeExecuteChangeSetOutput :: Newtype ExecuteChangeSetOutput _
derive instance repGenericExecuteChangeSetOutput :: Generic ExecuteChangeSetOutput _
instance showExecuteChangeSetOutput :: Show ExecuteChangeSetOutput where
  show = genericShow
instance decodeExecuteChangeSetOutput :: Decode ExecuteChangeSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecuteChangeSetOutput :: Encode ExecuteChangeSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionStatus = ExecutionStatus String
derive instance newtypeExecutionStatus :: Newtype ExecutionStatus _
derive instance repGenericExecutionStatus :: Generic ExecutionStatus _
instance showExecutionStatus :: Show ExecutionStatus where
  show = genericShow
instance decodeExecutionStatus :: Decode ExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionStatus :: Encode ExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Export</code> structure describes the exported output values for a stack.</p>
newtype Export = Export 
  { "ExportingStackId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "Name" :: NullOrUndefined.NullOrUndefined (ExportName)
  , "Value" :: NullOrUndefined.NullOrUndefined (ExportValue)
  }
derive instance newtypeExport :: Newtype Export _
derive instance repGenericExport :: Generic Export _
instance showExport :: Show Export where
  show = genericShow
instance decodeExport :: Decode Export where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExport :: Encode Export where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportName = ExportName String
derive instance newtypeExportName :: Newtype ExportName _
derive instance repGenericExportName :: Generic ExportName _
instance showExportName :: Show ExportName where
  show = genericShow
instance decodeExportName :: Decode ExportName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportName :: Encode ExportName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportValue = ExportValue String
derive instance newtypeExportValue :: Newtype ExportValue _
derive instance repGenericExportValue :: Generic ExportValue _
instance showExportValue :: Show ExportValue where
  show = genericShow
instance decodeExportValue :: Decode ExportValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportValue :: Encode ExportValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Exports = Exports (Array Export)
derive instance newtypeExports :: Newtype Exports _
derive instance repGenericExports :: Generic Exports _
instance showExports :: Show Exports where
  show = genericShow
instance decodeExports :: Decode Exports where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExports :: Encode Exports where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailureToleranceCount = FailureToleranceCount Int
derive instance newtypeFailureToleranceCount :: Newtype FailureToleranceCount _
derive instance repGenericFailureToleranceCount :: Generic FailureToleranceCount _
instance showFailureToleranceCount :: Show FailureToleranceCount where
  show = genericShow
instance decodeFailureToleranceCount :: Decode FailureToleranceCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureToleranceCount :: Encode FailureToleranceCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailureTolerancePercentage = FailureTolerancePercentage Int
derive instance newtypeFailureTolerancePercentage :: Newtype FailureTolerancePercentage _
derive instance repGenericFailureTolerancePercentage :: Generic FailureTolerancePercentage _
instance showFailureTolerancePercentage :: Show FailureTolerancePercentage where
  show = genericShow
instance decodeFailureTolerancePercentage :: Decode FailureTolerancePercentage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureTolerancePercentage :: Encode FailureTolerancePercentage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>GetStackPolicy</a> action.</p>
newtype GetStackPolicyInput = GetStackPolicyInput 
  { "StackName" :: (StackName)
  }
derive instance newtypeGetStackPolicyInput :: Newtype GetStackPolicyInput _
derive instance repGenericGetStackPolicyInput :: Generic GetStackPolicyInput _
instance showGetStackPolicyInput :: Show GetStackPolicyInput where
  show = genericShow
instance decodeGetStackPolicyInput :: Decode GetStackPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStackPolicyInput :: Encode GetStackPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>GetStackPolicy</a> action.</p>
newtype GetStackPolicyOutput = GetStackPolicyOutput 
  { "StackPolicyBody" :: NullOrUndefined.NullOrUndefined (StackPolicyBody)
  }
derive instance newtypeGetStackPolicyOutput :: Newtype GetStackPolicyOutput _
derive instance repGenericGetStackPolicyOutput :: Generic GetStackPolicyOutput _
instance showGetStackPolicyOutput :: Show GetStackPolicyOutput where
  show = genericShow
instance decodeGetStackPolicyOutput :: Decode GetStackPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetStackPolicyOutput :: Encode GetStackPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for a <a>GetTemplate</a> action.</p>
newtype GetTemplateInput = GetTemplateInput 
  { "StackName" :: NullOrUndefined.NullOrUndefined (StackName)
  , "ChangeSetName" :: NullOrUndefined.NullOrUndefined (ChangeSetNameOrId)
  , "TemplateStage" :: NullOrUndefined.NullOrUndefined (TemplateStage)
  }
derive instance newtypeGetTemplateInput :: Newtype GetTemplateInput _
derive instance repGenericGetTemplateInput :: Generic GetTemplateInput _
instance showGetTemplateInput :: Show GetTemplateInput where
  show = genericShow
instance decodeGetTemplateInput :: Decode GetTemplateInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTemplateInput :: Encode GetTemplateInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for <a>GetTemplate</a> action.</p>
newtype GetTemplateOutput = GetTemplateOutput 
  { "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "StagesAvailable" :: NullOrUndefined.NullOrUndefined (StageList)
  }
derive instance newtypeGetTemplateOutput :: Newtype GetTemplateOutput _
derive instance repGenericGetTemplateOutput :: Generic GetTemplateOutput _
instance showGetTemplateOutput :: Show GetTemplateOutput where
  show = genericShow
instance decodeGetTemplateOutput :: Decode GetTemplateOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTemplateOutput :: Encode GetTemplateOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>GetTemplateSummary</a> action.</p>
newtype GetTemplateSummaryInput = GetTemplateSummaryInput 
  { "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined.NullOrUndefined (TemplateURL)
  , "StackName" :: NullOrUndefined.NullOrUndefined (StackNameOrId)
  , "StackSetName" :: NullOrUndefined.NullOrUndefined (StackSetNameOrId)
  }
derive instance newtypeGetTemplateSummaryInput :: Newtype GetTemplateSummaryInput _
derive instance repGenericGetTemplateSummaryInput :: Generic GetTemplateSummaryInput _
instance showGetTemplateSummaryInput :: Show GetTemplateSummaryInput where
  show = genericShow
instance decodeGetTemplateSummaryInput :: Decode GetTemplateSummaryInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTemplateSummaryInput :: Encode GetTemplateSummaryInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>GetTemplateSummary</a> action.</p>
newtype GetTemplateSummaryOutput = GetTemplateSummaryOutput 
  { "Parameters" :: NullOrUndefined.NullOrUndefined (ParameterDeclarations)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "CapabilitiesReason" :: NullOrUndefined.NullOrUndefined (CapabilitiesReason)
  , "ResourceTypes" :: NullOrUndefined.NullOrUndefined (ResourceTypes)
  , "Version" :: NullOrUndefined.NullOrUndefined (Version)
  , "Metadata" :: NullOrUndefined.NullOrUndefined (Metadata)
  , "DeclaredTransforms" :: NullOrUndefined.NullOrUndefined (TransformsList)
  }
derive instance newtypeGetTemplateSummaryOutput :: Newtype GetTemplateSummaryOutput _
derive instance repGenericGetTemplateSummaryOutput :: Generic GetTemplateSummaryOutput _
instance showGetTemplateSummaryOutput :: Show GetTemplateSummaryOutput where
  show = genericShow
instance decodeGetTemplateSummaryOutput :: Decode GetTemplateSummaryOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTemplateSummaryOutput :: Encode GetTemplateSummaryOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Imports = Imports (Array StackName)
derive instance newtypeImports :: Newtype Imports _
derive instance repGenericImports :: Generic Imports _
instance showImports :: Show Imports where
  show = genericShow
instance decodeImports :: Decode Imports where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImports :: Encode Imports where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The template contains resources with capabilities that weren't specified in the Capabilities parameter.</p>
newtype InsufficientCapabilitiesException = InsufficientCapabilitiesException Types.NoArguments
derive instance newtypeInsufficientCapabilitiesException :: Newtype InsufficientCapabilitiesException _
derive instance repGenericInsufficientCapabilitiesException :: Generic InsufficientCapabilitiesException _
instance showInsufficientCapabilitiesException :: Show InsufficientCapabilitiesException where
  show = genericShow
instance decodeInsufficientCapabilitiesException :: Decode InsufficientCapabilitiesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientCapabilitiesException :: Encode InsufficientCapabilitiesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified change set can't be used to update the stack. For example, the change set status might be <code>CREATE_IN_PROGRESS</code>, or the stack status might be <code>UPDATE_IN_PROGRESS</code>.</p>
newtype InvalidChangeSetStatusException = InvalidChangeSetStatusException Types.NoArguments
derive instance newtypeInvalidChangeSetStatusException :: Newtype InvalidChangeSetStatusException _
derive instance repGenericInvalidChangeSetStatusException :: Generic InvalidChangeSetStatusException _
instance showInvalidChangeSetStatusException :: Show InvalidChangeSetStatusException where
  show = genericShow
instance decodeInvalidChangeSetStatusException :: Decode InvalidChangeSetStatusException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidChangeSetStatusException :: Encode InvalidChangeSetStatusException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified operation isn't valid.</p>
newtype InvalidOperationException = InvalidOperationException Types.NoArguments
derive instance newtypeInvalidOperationException :: Newtype InvalidOperationException _
derive instance repGenericInvalidOperationException :: Generic InvalidOperationException _
instance showInvalidOperationException :: Show InvalidOperationException where
  show = genericShow
instance decodeInvalidOperationException :: Decode InvalidOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOperationException :: Encode InvalidOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastUpdatedTime = LastUpdatedTime Number
derive instance newtypeLastUpdatedTime :: Newtype LastUpdatedTime _
derive instance repGenericLastUpdatedTime :: Generic LastUpdatedTime _
instance showLastUpdatedTime :: Show LastUpdatedTime where
  show = genericShow
instance decodeLastUpdatedTime :: Decode LastUpdatedTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastUpdatedTime :: Encode LastUpdatedTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The quota for the resource has already been reached.</p> <p>For information on stack set limitations, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-limitations.html">Limitations of StackSets</a>.</p>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitName = LimitName String
derive instance newtypeLimitName :: Newtype LimitName _
derive instance repGenericLimitName :: Generic LimitName _
instance showLimitName :: Show LimitName where
  show = genericShow
instance decodeLimitName :: Decode LimitName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitName :: Encode LimitName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitValue = LimitValue Int
derive instance newtypeLimitValue :: Newtype LimitValue _
derive instance repGenericLimitValue :: Generic LimitValue _
instance showLimitValue :: Show LimitValue where
  show = genericShow
instance decodeLimitValue :: Decode LimitValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitValue :: Encode LimitValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>ListChangeSets</a> action.</p>
newtype ListChangeSetsInput = ListChangeSetsInput 
  { "StackName" :: (StackNameOrId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListChangeSetsInput :: Newtype ListChangeSetsInput _
derive instance repGenericListChangeSetsInput :: Generic ListChangeSetsInput _
instance showListChangeSetsInput :: Show ListChangeSetsInput where
  show = genericShow
instance decodeListChangeSetsInput :: Decode ListChangeSetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListChangeSetsInput :: Encode ListChangeSetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>ListChangeSets</a> action.</p>
newtype ListChangeSetsOutput = ListChangeSetsOutput 
  { "Summaries" :: NullOrUndefined.NullOrUndefined (ChangeSetSummaries)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListChangeSetsOutput :: Newtype ListChangeSetsOutput _
derive instance repGenericListChangeSetsOutput :: Generic ListChangeSetsOutput _
instance showListChangeSetsOutput :: Show ListChangeSetsOutput where
  show = genericShow
instance decodeListChangeSetsOutput :: Decode ListChangeSetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListChangeSetsOutput :: Encode ListChangeSetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListExportsInput = ListExportsInput 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListExportsInput :: Newtype ListExportsInput _
derive instance repGenericListExportsInput :: Generic ListExportsInput _
instance showListExportsInput :: Show ListExportsInput where
  show = genericShow
instance decodeListExportsInput :: Decode ListExportsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListExportsInput :: Encode ListExportsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListExportsOutput = ListExportsOutput 
  { "Exports" :: NullOrUndefined.NullOrUndefined (Exports)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListExportsOutput :: Newtype ListExportsOutput _
derive instance repGenericListExportsOutput :: Generic ListExportsOutput _
instance showListExportsOutput :: Show ListExportsOutput where
  show = genericShow
instance decodeListExportsOutput :: Decode ListExportsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListExportsOutput :: Encode ListExportsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListImportsInput = ListImportsInput 
  { "ExportName" :: (ExportName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListImportsInput :: Newtype ListImportsInput _
derive instance repGenericListImportsInput :: Generic ListImportsInput _
instance showListImportsInput :: Show ListImportsInput where
  show = genericShow
instance decodeListImportsInput :: Decode ListImportsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListImportsInput :: Encode ListImportsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListImportsOutput = ListImportsOutput 
  { "Imports" :: NullOrUndefined.NullOrUndefined (Imports)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListImportsOutput :: Newtype ListImportsOutput _
derive instance repGenericListImportsOutput :: Generic ListImportsOutput _
instance showListImportsOutput :: Show ListImportsOutput where
  show = genericShow
instance decodeListImportsOutput :: Decode ListImportsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListImportsOutput :: Encode ListImportsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStackInstancesInput = ListStackInstancesInput 
  { "StackSetName" :: (StackSetName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "StackInstanceAccount" :: NullOrUndefined.NullOrUndefined (Account)
  , "StackInstanceRegion" :: NullOrUndefined.NullOrUndefined (Region)
  }
derive instance newtypeListStackInstancesInput :: Newtype ListStackInstancesInput _
derive instance repGenericListStackInstancesInput :: Generic ListStackInstancesInput _
instance showListStackInstancesInput :: Show ListStackInstancesInput where
  show = genericShow
instance decodeListStackInstancesInput :: Decode ListStackInstancesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackInstancesInput :: Encode ListStackInstancesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStackInstancesOutput = ListStackInstancesOutput 
  { "Summaries" :: NullOrUndefined.NullOrUndefined (StackInstanceSummaries)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListStackInstancesOutput :: Newtype ListStackInstancesOutput _
derive instance repGenericListStackInstancesOutput :: Generic ListStackInstancesOutput _
instance showListStackInstancesOutput :: Show ListStackInstancesOutput where
  show = genericShow
instance decodeListStackInstancesOutput :: Decode ListStackInstancesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackInstancesOutput :: Encode ListStackInstancesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>ListStackResource</a> action.</p>
newtype ListStackResourcesInput = ListStackResourcesInput 
  { "StackName" :: (StackName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListStackResourcesInput :: Newtype ListStackResourcesInput _
derive instance repGenericListStackResourcesInput :: Generic ListStackResourcesInput _
instance showListStackResourcesInput :: Show ListStackResourcesInput where
  show = genericShow
instance decodeListStackResourcesInput :: Decode ListStackResourcesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackResourcesInput :: Encode ListStackResourcesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for a <a>ListStackResources</a> action.</p>
newtype ListStackResourcesOutput = ListStackResourcesOutput 
  { "StackResourceSummaries" :: NullOrUndefined.NullOrUndefined (StackResourceSummaries)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListStackResourcesOutput :: Newtype ListStackResourcesOutput _
derive instance repGenericListStackResourcesOutput :: Generic ListStackResourcesOutput _
instance showListStackResourcesOutput :: Show ListStackResourcesOutput where
  show = genericShow
instance decodeListStackResourcesOutput :: Decode ListStackResourcesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackResourcesOutput :: Encode ListStackResourcesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStackSetOperationResultsInput = ListStackSetOperationResultsInput 
  { "StackSetName" :: (StackSetName)
  , "OperationId" :: (ClientRequestToken)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListStackSetOperationResultsInput :: Newtype ListStackSetOperationResultsInput _
derive instance repGenericListStackSetOperationResultsInput :: Generic ListStackSetOperationResultsInput _
instance showListStackSetOperationResultsInput :: Show ListStackSetOperationResultsInput where
  show = genericShow
instance decodeListStackSetOperationResultsInput :: Decode ListStackSetOperationResultsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackSetOperationResultsInput :: Encode ListStackSetOperationResultsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStackSetOperationResultsOutput = ListStackSetOperationResultsOutput 
  { "Summaries" :: NullOrUndefined.NullOrUndefined (StackSetOperationResultSummaries)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListStackSetOperationResultsOutput :: Newtype ListStackSetOperationResultsOutput _
derive instance repGenericListStackSetOperationResultsOutput :: Generic ListStackSetOperationResultsOutput _
instance showListStackSetOperationResultsOutput :: Show ListStackSetOperationResultsOutput where
  show = genericShow
instance decodeListStackSetOperationResultsOutput :: Decode ListStackSetOperationResultsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackSetOperationResultsOutput :: Encode ListStackSetOperationResultsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStackSetOperationsInput = ListStackSetOperationsInput 
  { "StackSetName" :: (StackSetName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListStackSetOperationsInput :: Newtype ListStackSetOperationsInput _
derive instance repGenericListStackSetOperationsInput :: Generic ListStackSetOperationsInput _
instance showListStackSetOperationsInput :: Show ListStackSetOperationsInput where
  show = genericShow
instance decodeListStackSetOperationsInput :: Decode ListStackSetOperationsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackSetOperationsInput :: Encode ListStackSetOperationsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStackSetOperationsOutput = ListStackSetOperationsOutput 
  { "Summaries" :: NullOrUndefined.NullOrUndefined (StackSetOperationSummaries)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListStackSetOperationsOutput :: Newtype ListStackSetOperationsOutput _
derive instance repGenericListStackSetOperationsOutput :: Generic ListStackSetOperationsOutput _
instance showListStackSetOperationsOutput :: Show ListStackSetOperationsOutput where
  show = genericShow
instance decodeListStackSetOperationsOutput :: Decode ListStackSetOperationsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackSetOperationsOutput :: Encode ListStackSetOperationsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStackSetsInput = ListStackSetsInput 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "Status" :: NullOrUndefined.NullOrUndefined (StackSetStatus)
  }
derive instance newtypeListStackSetsInput :: Newtype ListStackSetsInput _
derive instance repGenericListStackSetsInput :: Generic ListStackSetsInput _
instance showListStackSetsInput :: Show ListStackSetsInput where
  show = genericShow
instance decodeListStackSetsInput :: Decode ListStackSetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackSetsInput :: Encode ListStackSetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStackSetsOutput = ListStackSetsOutput 
  { "Summaries" :: NullOrUndefined.NullOrUndefined (StackSetSummaries)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListStackSetsOutput :: Newtype ListStackSetsOutput _
derive instance repGenericListStackSetsOutput :: Generic ListStackSetsOutput _
instance showListStackSetsOutput :: Show ListStackSetsOutput where
  show = genericShow
instance decodeListStackSetsOutput :: Decode ListStackSetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStackSetsOutput :: Encode ListStackSetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for <a>ListStacks</a> action.</p>
newtype ListStacksInput = ListStacksInput 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "StackStatusFilter" :: NullOrUndefined.NullOrUndefined (StackStatusFilter)
  }
derive instance newtypeListStacksInput :: Newtype ListStacksInput _
derive instance repGenericListStacksInput :: Generic ListStacksInput _
instance showListStacksInput :: Show ListStacksInput where
  show = genericShow
instance decodeListStacksInput :: Decode ListStacksInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStacksInput :: Encode ListStacksInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for <a>ListStacks</a> action.</p>
newtype ListStacksOutput = ListStacksOutput 
  { "StackSummaries" :: NullOrUndefined.NullOrUndefined (StackSummaries)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListStacksOutput :: Newtype ListStacksOutput _
derive instance repGenericListStacksOutput :: Generic ListStacksOutput _
instance showListStacksOutput :: Show ListStacksOutput where
  show = genericShow
instance decodeListStacksOutput :: Decode ListStacksOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStacksOutput :: Encode ListStacksOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogicalResourceId = LogicalResourceId String
derive instance newtypeLogicalResourceId :: Newtype LogicalResourceId _
derive instance repGenericLogicalResourceId :: Generic LogicalResourceId _
instance showLogicalResourceId :: Show LogicalResourceId where
  show = genericShow
instance decodeLogicalResourceId :: Decode LogicalResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogicalResourceId :: Encode LogicalResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxConcurrentCount = MaxConcurrentCount Int
derive instance newtypeMaxConcurrentCount :: Newtype MaxConcurrentCount _
derive instance repGenericMaxConcurrentCount :: Generic MaxConcurrentCount _
instance showMaxConcurrentCount :: Show MaxConcurrentCount where
  show = genericShow
instance decodeMaxConcurrentCount :: Decode MaxConcurrentCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxConcurrentCount :: Encode MaxConcurrentCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxConcurrentPercentage = MaxConcurrentPercentage Int
derive instance newtypeMaxConcurrentPercentage :: Newtype MaxConcurrentPercentage _
derive instance repGenericMaxConcurrentPercentage :: Generic MaxConcurrentPercentage _
instance showMaxConcurrentPercentage :: Show MaxConcurrentPercentage where
  show = genericShow
instance decodeMaxConcurrentPercentage :: Decode MaxConcurrentPercentage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxConcurrentPercentage :: Encode MaxConcurrentPercentage where
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


newtype Metadata = Metadata String
derive instance newtypeMetadata :: Newtype Metadata _
derive instance repGenericMetadata :: Generic Metadata _
instance showMetadata :: Show Metadata where
  show = genericShow
instance decodeMetadata :: Decode Metadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetadata :: Encode Metadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MonitoringTimeInMinutes = MonitoringTimeInMinutes Int
derive instance newtypeMonitoringTimeInMinutes :: Newtype MonitoringTimeInMinutes _
derive instance repGenericMonitoringTimeInMinutes :: Generic MonitoringTimeInMinutes _
instance showMonitoringTimeInMinutes :: Show MonitoringTimeInMinutes where
  show = genericShow
instance decodeMonitoringTimeInMinutes :: Decode MonitoringTimeInMinutes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMonitoringTimeInMinutes :: Encode MonitoringTimeInMinutes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified name is already in use.</p>
newtype NameAlreadyExistsException = NameAlreadyExistsException Types.NoArguments
derive instance newtypeNameAlreadyExistsException :: Newtype NameAlreadyExistsException _
derive instance repGenericNameAlreadyExistsException :: Generic NameAlreadyExistsException _
instance showNameAlreadyExistsException :: Show NameAlreadyExistsException where
  show = genericShow
instance decodeNameAlreadyExistsException :: Decode NameAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNameAlreadyExistsException :: Encode NameAlreadyExistsException where
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


newtype NoEcho = NoEcho Boolean
derive instance newtypeNoEcho :: Newtype NoEcho _
derive instance repGenericNoEcho :: Generic NoEcho _
instance showNoEcho :: Show NoEcho where
  show = genericShow
instance decodeNoEcho :: Decode NoEcho where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoEcho :: Encode NoEcho where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationARN = NotificationARN String
derive instance newtypeNotificationARN :: Newtype NotificationARN _
derive instance repGenericNotificationARN :: Generic NotificationARN _
instance showNotificationARN :: Show NotificationARN where
  show = genericShow
instance decodeNotificationARN :: Decode NotificationARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationARN :: Encode NotificationARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NotificationARNs = NotificationARNs (Array NotificationARN)
derive instance newtypeNotificationARNs :: Newtype NotificationARNs _
derive instance repGenericNotificationARNs :: Generic NotificationARNs _
instance showNotificationARNs :: Show NotificationARNs where
  show = genericShow
instance decodeNotificationARNs :: Decode NotificationARNs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationARNs :: Encode NotificationARNs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OnFailure = OnFailure String
derive instance newtypeOnFailure :: Newtype OnFailure _
derive instance repGenericOnFailure :: Generic OnFailure _
instance showOnFailure :: Show OnFailure where
  show = genericShow
instance decodeOnFailure :: Decode OnFailure where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOnFailure :: Encode OnFailure where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified operation ID already exists.</p>
newtype OperationIdAlreadyExistsException = OperationIdAlreadyExistsException Types.NoArguments
derive instance newtypeOperationIdAlreadyExistsException :: Newtype OperationIdAlreadyExistsException _
derive instance repGenericOperationIdAlreadyExistsException :: Generic OperationIdAlreadyExistsException _
instance showOperationIdAlreadyExistsException :: Show OperationIdAlreadyExistsException where
  show = genericShow
instance decodeOperationIdAlreadyExistsException :: Decode OperationIdAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationIdAlreadyExistsException :: Encode OperationIdAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Another operation is currently in progress for this stack set. Only one operation can be performed for a stack set at a given time.</p>
newtype OperationInProgressException = OperationInProgressException Types.NoArguments
derive instance newtypeOperationInProgressException :: Newtype OperationInProgressException _
derive instance repGenericOperationInProgressException :: Generic OperationInProgressException _
instance showOperationInProgressException :: Show OperationInProgressException where
  show = genericShow
instance decodeOperationInProgressException :: Decode OperationInProgressException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationInProgressException :: Encode OperationInProgressException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified ID refers to an operation that doesn't exist.</p>
newtype OperationNotFoundException = OperationNotFoundException Types.NoArguments
derive instance newtypeOperationNotFoundException :: Newtype OperationNotFoundException _
derive instance repGenericOperationNotFoundException :: Generic OperationNotFoundException _
instance showOperationNotFoundException :: Show OperationNotFoundException where
  show = genericShow
instance decodeOperationNotFoundException :: Decode OperationNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationNotFoundException :: Encode OperationNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Output data type.</p>
newtype Output = Output 
  { "OutputKey" :: NullOrUndefined.NullOrUndefined (OutputKey)
  , "OutputValue" :: NullOrUndefined.NullOrUndefined (OutputValue)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "ExportName" :: NullOrUndefined.NullOrUndefined (ExportName)
  }
derive instance newtypeOutput :: Newtype Output _
derive instance repGenericOutput :: Generic Output _
instance showOutput :: Show Output where
  show = genericShow
instance decodeOutput :: Decode Output where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutput :: Encode Output where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OutputKey = OutputKey String
derive instance newtypeOutputKey :: Newtype OutputKey _
derive instance repGenericOutputKey :: Generic OutputKey _
instance showOutputKey :: Show OutputKey where
  show = genericShow
instance decodeOutputKey :: Decode OutputKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputKey :: Encode OutputKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OutputValue = OutputValue String
derive instance newtypeOutputValue :: Newtype OutputValue _
derive instance repGenericOutputValue :: Generic OutputValue _
instance showOutputValue :: Show OutputValue where
  show = genericShow
instance decodeOutputValue :: Decode OutputValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputValue :: Encode OutputValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Outputs = Outputs (Array Output)
derive instance newtypeOutputs :: Newtype Outputs _
derive instance repGenericOutputs :: Generic Outputs _
instance showOutputs :: Show Outputs where
  show = genericShow
instance decodeOutputs :: Decode Outputs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputs :: Encode Outputs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Parameter data type.</p>
newtype Parameter = Parameter 
  { "ParameterKey" :: NullOrUndefined.NullOrUndefined (ParameterKey)
  , "ParameterValue" :: NullOrUndefined.NullOrUndefined (ParameterValue)
  , "UsePreviousValue" :: NullOrUndefined.NullOrUndefined (UsePreviousValue)
  , "ResolvedValue" :: NullOrUndefined.NullOrUndefined (ParameterValue)
  }
derive instance newtypeParameter :: Newtype Parameter _
derive instance repGenericParameter :: Generic Parameter _
instance showParameter :: Show Parameter where
  show = genericShow
instance decodeParameter :: Decode Parameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameter :: Encode Parameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A set of criteria that AWS CloudFormation uses to validate parameter values. Although other constraints might be defined in the stack template, AWS CloudFormation returns only the <code>AllowedValues</code> property.</p>
newtype ParameterConstraints = ParameterConstraints 
  { "AllowedValues" :: NullOrUndefined.NullOrUndefined (AllowedValues)
  }
derive instance newtypeParameterConstraints :: Newtype ParameterConstraints _
derive instance repGenericParameterConstraints :: Generic ParameterConstraints _
instance showParameterConstraints :: Show ParameterConstraints where
  show = genericShow
instance decodeParameterConstraints :: Decode ParameterConstraints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterConstraints :: Encode ParameterConstraints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ParameterDeclaration data type.</p>
newtype ParameterDeclaration = ParameterDeclaration 
  { "ParameterKey" :: NullOrUndefined.NullOrUndefined (ParameterKey)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (ParameterValue)
  , "ParameterType" :: NullOrUndefined.NullOrUndefined (ParameterType)
  , "NoEcho" :: NullOrUndefined.NullOrUndefined (NoEcho)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "ParameterConstraints" :: NullOrUndefined.NullOrUndefined (ParameterConstraints)
  }
derive instance newtypeParameterDeclaration :: Newtype ParameterDeclaration _
derive instance repGenericParameterDeclaration :: Generic ParameterDeclaration _
instance showParameterDeclaration :: Show ParameterDeclaration where
  show = genericShow
instance decodeParameterDeclaration :: Decode ParameterDeclaration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterDeclaration :: Encode ParameterDeclaration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterDeclarations = ParameterDeclarations (Array ParameterDeclaration)
derive instance newtypeParameterDeclarations :: Newtype ParameterDeclarations _
derive instance repGenericParameterDeclarations :: Generic ParameterDeclarations _
instance showParameterDeclarations :: Show ParameterDeclarations where
  show = genericShow
instance decodeParameterDeclarations :: Decode ParameterDeclarations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterDeclarations :: Encode ParameterDeclarations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParameterKey = ParameterKey String
derive instance newtypeParameterKey :: Newtype ParameterKey _
derive instance repGenericParameterKey :: Generic ParameterKey _
instance showParameterKey :: Show ParameterKey where
  show = genericShow
instance decodeParameterKey :: Decode ParameterKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameterKey :: Encode ParameterKey where
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


newtype Parameters = Parameters (Array Parameter)
derive instance newtypeParameters :: Newtype Parameters _
derive instance repGenericParameters :: Generic Parameters _
instance showParameters :: Show Parameters where
  show = genericShow
instance decodeParameters :: Decode Parameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParameters :: Encode Parameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PhysicalResourceId = PhysicalResourceId String
derive instance newtypePhysicalResourceId :: Newtype PhysicalResourceId _
derive instance repGenericPhysicalResourceId :: Generic PhysicalResourceId _
instance showPhysicalResourceId :: Show PhysicalResourceId where
  show = genericShow
instance decodePhysicalResourceId :: Decode PhysicalResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePhysicalResourceId :: Encode PhysicalResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PropertyName = PropertyName String
derive instance newtypePropertyName :: Newtype PropertyName _
derive instance repGenericPropertyName :: Generic PropertyName _
instance showPropertyName :: Show PropertyName where
  show = genericShow
instance decodePropertyName :: Decode PropertyName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePropertyName :: Encode PropertyName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Reason = Reason String
derive instance newtypeReason :: Newtype Reason _
derive instance repGenericReason :: Generic Reason _
instance showReason :: Show Reason where
  show = genericShow
instance decodeReason :: Decode Reason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReason :: Encode Reason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Region = Region String
derive instance newtypeRegion :: Newtype Region _
derive instance repGenericRegion :: Generic Region _
instance showRegion :: Show Region where
  show = genericShow
instance decodeRegion :: Decode Region where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegion :: Encode Region where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegionList = RegionList (Array Region)
derive instance newtypeRegionList :: Newtype RegionList _
derive instance repGenericRegionList :: Generic RegionList _
instance showRegionList :: Show RegionList where
  show = genericShow
instance decodeRegionList :: Decode RegionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegionList :: Encode RegionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Replacement = Replacement String
derive instance newtypeReplacement :: Newtype Replacement _
derive instance repGenericReplacement :: Generic Replacement _
instance showReplacement :: Show Replacement where
  show = genericShow
instance decodeReplacement :: Decode Replacement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplacement :: Encode Replacement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequiresRecreation = RequiresRecreation String
derive instance newtypeRequiresRecreation :: Newtype RequiresRecreation _
derive instance repGenericRequiresRecreation :: Generic RequiresRecreation _
instance showRequiresRecreation :: Show RequiresRecreation where
  show = genericShow
instance decodeRequiresRecreation :: Decode RequiresRecreation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequiresRecreation :: Encode RequiresRecreation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceAttribute = ResourceAttribute String
derive instance newtypeResourceAttribute :: Newtype ResourceAttribute _
derive instance repGenericResourceAttribute :: Generic ResourceAttribute _
instance showResourceAttribute :: Show ResourceAttribute where
  show = genericShow
instance decodeResourceAttribute :: Decode ResourceAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceAttribute :: Encode ResourceAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ResourceChange</code> structure describes the resource and the action that AWS CloudFormation will perform on it if you execute this change set.</p>
newtype ResourceChange = ResourceChange 
  { "Action" :: NullOrUndefined.NullOrUndefined (ChangeAction)
  , "LogicalResourceId" :: NullOrUndefined.NullOrUndefined (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined.NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "Replacement" :: NullOrUndefined.NullOrUndefined (Replacement)
  , "Scope" :: NullOrUndefined.NullOrUndefined (Scope)
  , "Details" :: NullOrUndefined.NullOrUndefined (ResourceChangeDetails)
  }
derive instance newtypeResourceChange :: Newtype ResourceChange _
derive instance repGenericResourceChange :: Generic ResourceChange _
instance showResourceChange :: Show ResourceChange where
  show = genericShow
instance decodeResourceChange :: Decode ResourceChange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceChange :: Encode ResourceChange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>For a resource with <code>Modify</code> as the action, the <code>ResourceChange</code> structure describes the changes AWS CloudFormation will make to that resource.</p>
newtype ResourceChangeDetail = ResourceChangeDetail 
  { "Target" :: NullOrUndefined.NullOrUndefined (ResourceTargetDefinition)
  , "Evaluation" :: NullOrUndefined.NullOrUndefined (EvaluationType)
  , "ChangeSource" :: NullOrUndefined.NullOrUndefined (ChangeSource)
  , "CausingEntity" :: NullOrUndefined.NullOrUndefined (CausingEntity)
  }
derive instance newtypeResourceChangeDetail :: Newtype ResourceChangeDetail _
derive instance repGenericResourceChangeDetail :: Generic ResourceChangeDetail _
instance showResourceChangeDetail :: Show ResourceChangeDetail where
  show = genericShow
instance decodeResourceChangeDetail :: Decode ResourceChangeDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceChangeDetail :: Encode ResourceChangeDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceChangeDetails = ResourceChangeDetails (Array ResourceChangeDetail)
derive instance newtypeResourceChangeDetails :: Newtype ResourceChangeDetails _
derive instance repGenericResourceChangeDetails :: Generic ResourceChangeDetails _
instance showResourceChangeDetails :: Show ResourceChangeDetails where
  show = genericShow
instance decodeResourceChangeDetails :: Decode ResourceChangeDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceChangeDetails :: Encode ResourceChangeDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceProperties = ResourceProperties String
derive instance newtypeResourceProperties :: Newtype ResourceProperties _
derive instance repGenericResourceProperties :: Generic ResourceProperties _
instance showResourceProperties :: Show ResourceProperties where
  show = genericShow
instance decodeResourceProperties :: Decode ResourceProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceProperties :: Encode ResourceProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceSignalStatus = ResourceSignalStatus String
derive instance newtypeResourceSignalStatus :: Newtype ResourceSignalStatus _
derive instance repGenericResourceSignalStatus :: Generic ResourceSignalStatus _
instance showResourceSignalStatus :: Show ResourceSignalStatus where
  show = genericShow
instance decodeResourceSignalStatus :: Decode ResourceSignalStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceSignalStatus :: Encode ResourceSignalStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceSignalUniqueId = ResourceSignalUniqueId String
derive instance newtypeResourceSignalUniqueId :: Newtype ResourceSignalUniqueId _
derive instance repGenericResourceSignalUniqueId :: Generic ResourceSignalUniqueId _
instance showResourceSignalUniqueId :: Show ResourceSignalUniqueId where
  show = genericShow
instance decodeResourceSignalUniqueId :: Decode ResourceSignalUniqueId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceSignalUniqueId :: Encode ResourceSignalUniqueId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceStatus = ResourceStatus String
derive instance newtypeResourceStatus :: Newtype ResourceStatus _
derive instance repGenericResourceStatus :: Generic ResourceStatus _
instance showResourceStatus :: Show ResourceStatus where
  show = genericShow
instance decodeResourceStatus :: Decode ResourceStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceStatus :: Encode ResourceStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceStatusReason = ResourceStatusReason String
derive instance newtypeResourceStatusReason :: Newtype ResourceStatusReason _
derive instance repGenericResourceStatusReason :: Generic ResourceStatusReason _
instance showResourceStatusReason :: Show ResourceStatusReason where
  show = genericShow
instance decodeResourceStatusReason :: Decode ResourceStatusReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceStatusReason :: Encode ResourceStatusReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The field that AWS CloudFormation will change, such as the name of a resource's property, and whether the resource will be recreated.</p>
newtype ResourceTargetDefinition = ResourceTargetDefinition 
  { "Attribute" :: NullOrUndefined.NullOrUndefined (ResourceAttribute)
  , "Name" :: NullOrUndefined.NullOrUndefined (PropertyName)
  , "RequiresRecreation" :: NullOrUndefined.NullOrUndefined (RequiresRecreation)
  }
derive instance newtypeResourceTargetDefinition :: Newtype ResourceTargetDefinition _
derive instance repGenericResourceTargetDefinition :: Generic ResourceTargetDefinition _
instance showResourceTargetDefinition :: Show ResourceTargetDefinition where
  show = genericShow
instance decodeResourceTargetDefinition :: Decode ResourceTargetDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTargetDefinition :: Encode ResourceTargetDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceToSkip = ResourceToSkip String
derive instance newtypeResourceToSkip :: Newtype ResourceToSkip _
derive instance repGenericResourceToSkip :: Generic ResourceToSkip _
instance showResourceToSkip :: Show ResourceToSkip where
  show = genericShow
instance decodeResourceToSkip :: Decode ResourceToSkip where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceToSkip :: Encode ResourceToSkip where
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


newtype ResourceTypes = ResourceTypes (Array ResourceType)
derive instance newtypeResourceTypes :: Newtype ResourceTypes _
derive instance repGenericResourceTypes :: Generic ResourceTypes _
instance showResourceTypes :: Show ResourceTypes where
  show = genericShow
instance decodeResourceTypes :: Decode ResourceTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTypes :: Encode ResourceTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourcesToSkip = ResourcesToSkip (Array ResourceToSkip)
derive instance newtypeResourcesToSkip :: Newtype ResourcesToSkip _
derive instance repGenericResourcesToSkip :: Generic ResourcesToSkip _
instance showResourcesToSkip :: Show ResourcesToSkip where
  show = genericShow
instance decodeResourcesToSkip :: Decode ResourcesToSkip where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourcesToSkip :: Encode ResourcesToSkip where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RetainResources = RetainResources (Array LogicalResourceId)
derive instance newtypeRetainResources :: Newtype RetainResources _
derive instance repGenericRetainResources :: Generic RetainResources _
instance showRetainResources :: Show RetainResources where
  show = genericShow
instance decodeRetainResources :: Decode RetainResources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetainResources :: Encode RetainResources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RetainStacks = RetainStacks Boolean
derive instance newtypeRetainStacks :: Newtype RetainStacks _
derive instance repGenericRetainStacks :: Generic RetainStacks _
instance showRetainStacks :: Show RetainStacks where
  show = genericShow
instance decodeRetainStacks :: Decode RetainStacks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetainStacks :: Encode RetainStacks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RetainStacksNullable = RetainStacksNullable Boolean
derive instance newtypeRetainStacksNullable :: Newtype RetainStacksNullable _
derive instance repGenericRetainStacksNullable :: Generic RetainStacksNullable _
instance showRetainStacksNullable :: Show RetainStacksNullable where
  show = genericShow
instance decodeRetainStacksNullable :: Decode RetainStacksNullable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetainStacksNullable :: Encode RetainStacksNullable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleARN = RoleARN String
derive instance newtypeRoleARN :: Newtype RoleARN _
derive instance repGenericRoleARN :: Generic RoleARN _
instance showRoleARN :: Show RoleARN where
  show = genericShow
instance decodeRoleARN :: Decode RoleARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleARN :: Encode RoleARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Structure containing the rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.</p> <p>Rollback triggers enable you to have AWS CloudFormation monitor the state of your application during stack creation and updating, and to roll back that operation if the application breaches the threshold of any of the alarms you've specified. For each rollback trigger you create, you specify the Cloudwatch alarm that CloudFormation should monitor. CloudFormation monitors the specified alarms during the stack create or update operation, and for the specified amount of time after all resources have been deployed. If any of the alarms goes to ALERT state during the stack operation or the monitoring period, CloudFormation rolls back the entire stack operation. If the monitoring period expires without any alarms going to ALERT state, CloudFormation proceeds to dispose of old resources as usual.</p> <p>By default, CloudFormation only rolls back stack operations if an alarm goes to ALERT state, not INSUFFICIENT_DATA state. To have CloudFormation roll back the stack operation if an alarm goes to INSUFFICIENT_DATA state as well, edit the CloudWatch alarm to treat missing data as <code>breaching</code>. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html">Configuring How CloudWatch Alarms Treats Missing Data</a>.</p> <p>AWS CloudFormation does not monitor rollback triggers when it rolls back a stack during an update operation.</p>
newtype RollbackConfiguration = RollbackConfiguration 
  { "RollbackTriggers" :: NullOrUndefined.NullOrUndefined (RollbackTriggers)
  , "MonitoringTimeInMinutes" :: NullOrUndefined.NullOrUndefined (MonitoringTimeInMinutes)
  }
derive instance newtypeRollbackConfiguration :: Newtype RollbackConfiguration _
derive instance repGenericRollbackConfiguration :: Generic RollbackConfiguration _
instance showRollbackConfiguration :: Show RollbackConfiguration where
  show = genericShow
instance decodeRollbackConfiguration :: Decode RollbackConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRollbackConfiguration :: Encode RollbackConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A rollback trigger AWS CloudFormation monitors during creation and updating of stacks. If any of the alarms you specify goes to ALERT state during the stack operation or within the specified monitoring period afterwards, CloudFormation rolls back the entire stack operation. </p>
newtype RollbackTrigger = RollbackTrigger 
  { "Arn" :: (Arn)
  , "Type" :: (Type)
  }
derive instance newtypeRollbackTrigger :: Newtype RollbackTrigger _
derive instance repGenericRollbackTrigger :: Generic RollbackTrigger _
instance showRollbackTrigger :: Show RollbackTrigger where
  show = genericShow
instance decodeRollbackTrigger :: Decode RollbackTrigger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRollbackTrigger :: Encode RollbackTrigger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RollbackTriggers = RollbackTriggers (Array RollbackTrigger)
derive instance newtypeRollbackTriggers :: Newtype RollbackTriggers _
derive instance repGenericRollbackTriggers :: Generic RollbackTriggers _
instance showRollbackTriggers :: Show RollbackTriggers where
  show = genericShow
instance decodeRollbackTriggers :: Decode RollbackTriggers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRollbackTriggers :: Encode RollbackTriggers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Scope = Scope (Array ResourceAttribute)
derive instance newtypeScope :: Newtype Scope _
derive instance repGenericScope :: Generic Scope _
instance showScope :: Show Scope where
  show = genericShow
instance decodeScope :: Decode Scope where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScope :: Encode Scope where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>SetStackPolicy</a> action.</p>
newtype SetStackPolicyInput = SetStackPolicyInput 
  { "StackName" :: (StackName)
  , "StackPolicyBody" :: NullOrUndefined.NullOrUndefined (StackPolicyBody)
  , "StackPolicyURL" :: NullOrUndefined.NullOrUndefined (StackPolicyURL)
  }
derive instance newtypeSetStackPolicyInput :: Newtype SetStackPolicyInput _
derive instance repGenericSetStackPolicyInput :: Generic SetStackPolicyInput _
instance showSetStackPolicyInput :: Show SetStackPolicyInput where
  show = genericShow
instance decodeSetStackPolicyInput :: Decode SetStackPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetStackPolicyInput :: Encode SetStackPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>SignalResource</a> action.</p>
newtype SignalResourceInput = SignalResourceInput 
  { "StackName" :: (StackNameOrId)
  , "LogicalResourceId" :: (LogicalResourceId)
  , "UniqueId" :: (ResourceSignalUniqueId)
  , "Status" :: (ResourceSignalStatus)
  }
derive instance newtypeSignalResourceInput :: Newtype SignalResourceInput _
derive instance repGenericSignalResourceInput :: Generic SignalResourceInput _
instance showSignalResourceInput :: Show SignalResourceInput where
  show = genericShow
instance decodeSignalResourceInput :: Decode SignalResourceInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignalResourceInput :: Encode SignalResourceInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Stack data type.</p>
newtype Stack = Stack 
  { "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "StackName" :: (StackName)
  , "ChangeSetId" :: NullOrUndefined.NullOrUndefined (ChangeSetId)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "CreationTime" :: (CreationTime)
  , "DeletionTime" :: NullOrUndefined.NullOrUndefined (DeletionTime)
  , "LastUpdatedTime" :: NullOrUndefined.NullOrUndefined (LastUpdatedTime)
  , "RollbackConfiguration" :: NullOrUndefined.NullOrUndefined (RollbackConfiguration)
  , "StackStatus" :: (StackStatus)
  , "StackStatusReason" :: NullOrUndefined.NullOrUndefined (StackStatusReason)
  , "DisableRollback" :: NullOrUndefined.NullOrUndefined (DisableRollback)
  , "NotificationARNs" :: NullOrUndefined.NullOrUndefined (NotificationARNs)
  , "TimeoutInMinutes" :: NullOrUndefined.NullOrUndefined (TimeoutMinutes)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "Outputs" :: NullOrUndefined.NullOrUndefined (Outputs)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  , "EnableTerminationProtection" :: NullOrUndefined.NullOrUndefined (EnableTerminationProtection)
  , "ParentId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "RootId" :: NullOrUndefined.NullOrUndefined (StackId)
  }
derive instance newtypeStack :: Newtype Stack _
derive instance repGenericStack :: Generic Stack _
instance showStack :: Show Stack where
  show = genericShow
instance decodeStack :: Decode Stack where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStack :: Encode Stack where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The StackEvent data type.</p>
newtype StackEvent = StackEvent 
  { "StackId" :: (StackId)
  , "EventId" :: (EventId)
  , "StackName" :: (StackName)
  , "LogicalResourceId" :: NullOrUndefined.NullOrUndefined (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined.NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "Number" :: (Number)
  , "ResourceStatus" :: NullOrUndefined.NullOrUndefined (ResourceStatus)
  , "ResourceStatusReason" :: NullOrUndefined.NullOrUndefined (ResourceStatusReason)
  , "ResourceProperties" :: NullOrUndefined.NullOrUndefined (ResourceProperties)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeStackEvent :: Newtype StackEvent _
derive instance repGenericStackEvent :: Generic StackEvent _
instance showStackEvent :: Show StackEvent where
  show = genericShow
instance decodeStackEvent :: Decode StackEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackEvent :: Encode StackEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackEvents = StackEvents (Array StackEvent)
derive instance newtypeStackEvents :: Newtype StackEvents _
derive instance repGenericStackEvents :: Generic StackEvents _
instance showStackEvents :: Show StackEvents where
  show = genericShow
instance decodeStackEvents :: Decode StackEvents where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackEvents :: Encode StackEvents where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackId = StackId String
derive instance newtypeStackId :: Newtype StackId _
derive instance repGenericStackId :: Generic StackId _
instance showStackId :: Show StackId where
  show = genericShow
instance decodeStackId :: Decode StackId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackId :: Encode StackId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An AWS CloudFormation stack, in a specific account and region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.</p>
newtype StackInstance = StackInstance 
  { "StackSetId" :: NullOrUndefined.NullOrUndefined (StackSetId)
  , "Region" :: NullOrUndefined.NullOrUndefined (Region)
  , "Account" :: NullOrUndefined.NullOrUndefined (Account)
  , "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "ParameterOverrides" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "Status" :: NullOrUndefined.NullOrUndefined (StackInstanceStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (Reason)
  }
derive instance newtypeStackInstance :: Newtype StackInstance _
derive instance repGenericStackInstance :: Generic StackInstance _
instance showStackInstance :: Show StackInstance where
  show = genericShow
instance decodeStackInstance :: Decode StackInstance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackInstance :: Encode StackInstance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified stack instance doesn't exist.</p>
newtype StackInstanceNotFoundException = StackInstanceNotFoundException Types.NoArguments
derive instance newtypeStackInstanceNotFoundException :: Newtype StackInstanceNotFoundException _
derive instance repGenericStackInstanceNotFoundException :: Generic StackInstanceNotFoundException _
instance showStackInstanceNotFoundException :: Show StackInstanceNotFoundException where
  show = genericShow
instance decodeStackInstanceNotFoundException :: Decode StackInstanceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackInstanceNotFoundException :: Encode StackInstanceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackInstanceStatus = StackInstanceStatus String
derive instance newtypeStackInstanceStatus :: Newtype StackInstanceStatus _
derive instance repGenericStackInstanceStatus :: Generic StackInstanceStatus _
instance showStackInstanceStatus :: Show StackInstanceStatus where
  show = genericShow
instance decodeStackInstanceStatus :: Decode StackInstanceStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackInstanceStatus :: Encode StackInstanceStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackInstanceSummaries = StackInstanceSummaries (Array StackInstanceSummary)
derive instance newtypeStackInstanceSummaries :: Newtype StackInstanceSummaries _
derive instance repGenericStackInstanceSummaries :: Generic StackInstanceSummaries _
instance showStackInstanceSummaries :: Show StackInstanceSummaries where
  show = genericShow
instance decodeStackInstanceSummaries :: Decode StackInstanceSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackInstanceSummaries :: Encode StackInstanceSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The structure that contains summary information about a stack instance.</p>
newtype StackInstanceSummary = StackInstanceSummary 
  { "StackSetId" :: NullOrUndefined.NullOrUndefined (StackSetId)
  , "Region" :: NullOrUndefined.NullOrUndefined (Region)
  , "Account" :: NullOrUndefined.NullOrUndefined (Account)
  , "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "Status" :: NullOrUndefined.NullOrUndefined (StackInstanceStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (Reason)
  }
derive instance newtypeStackInstanceSummary :: Newtype StackInstanceSummary _
derive instance repGenericStackInstanceSummary :: Generic StackInstanceSummary _
instance showStackInstanceSummary :: Show StackInstanceSummary where
  show = genericShow
instance decodeStackInstanceSummary :: Decode StackInstanceSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackInstanceSummary :: Encode StackInstanceSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackName = StackName String
derive instance newtypeStackName :: Newtype StackName _
derive instance repGenericStackName :: Generic StackName _
instance showStackName :: Show StackName where
  show = genericShow
instance decodeStackName :: Decode StackName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackName :: Encode StackName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackNameOrId = StackNameOrId String
derive instance newtypeStackNameOrId :: Newtype StackNameOrId _
derive instance repGenericStackNameOrId :: Generic StackNameOrId _
instance showStackNameOrId :: Show StackNameOrId where
  show = genericShow
instance decodeStackNameOrId :: Decode StackNameOrId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackNameOrId :: Encode StackNameOrId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackPolicyBody = StackPolicyBody String
derive instance newtypeStackPolicyBody :: Newtype StackPolicyBody _
derive instance repGenericStackPolicyBody :: Generic StackPolicyBody _
instance showStackPolicyBody :: Show StackPolicyBody where
  show = genericShow
instance decodeStackPolicyBody :: Decode StackPolicyBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackPolicyBody :: Encode StackPolicyBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackPolicyDuringUpdateBody = StackPolicyDuringUpdateBody String
derive instance newtypeStackPolicyDuringUpdateBody :: Newtype StackPolicyDuringUpdateBody _
derive instance repGenericStackPolicyDuringUpdateBody :: Generic StackPolicyDuringUpdateBody _
instance showStackPolicyDuringUpdateBody :: Show StackPolicyDuringUpdateBody where
  show = genericShow
instance decodeStackPolicyDuringUpdateBody :: Decode StackPolicyDuringUpdateBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackPolicyDuringUpdateBody :: Encode StackPolicyDuringUpdateBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackPolicyDuringUpdateURL = StackPolicyDuringUpdateURL String
derive instance newtypeStackPolicyDuringUpdateURL :: Newtype StackPolicyDuringUpdateURL _
derive instance repGenericStackPolicyDuringUpdateURL :: Generic StackPolicyDuringUpdateURL _
instance showStackPolicyDuringUpdateURL :: Show StackPolicyDuringUpdateURL where
  show = genericShow
instance decodeStackPolicyDuringUpdateURL :: Decode StackPolicyDuringUpdateURL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackPolicyDuringUpdateURL :: Encode StackPolicyDuringUpdateURL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackPolicyURL = StackPolicyURL String
derive instance newtypeStackPolicyURL :: Newtype StackPolicyURL _
derive instance repGenericStackPolicyURL :: Generic StackPolicyURL _
instance showStackPolicyURL :: Show StackPolicyURL where
  show = genericShow
instance decodeStackPolicyURL :: Decode StackPolicyURL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackPolicyURL :: Encode StackPolicyURL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The StackResource data type.</p>
newtype StackResource = StackResource 
  { "StackName" :: NullOrUndefined.NullOrUndefined (StackName)
  , "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "LogicalResourceId" :: (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined.NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: (ResourceType)
  , "Number" :: (Number)
  , "ResourceStatus" :: (ResourceStatus)
  , "ResourceStatusReason" :: NullOrUndefined.NullOrUndefined (ResourceStatusReason)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeStackResource :: Newtype StackResource _
derive instance repGenericStackResource :: Generic StackResource _
instance showStackResource :: Show StackResource where
  show = genericShow
instance decodeStackResource :: Decode StackResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackResource :: Encode StackResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains detailed information about the specified stack resource.</p>
newtype StackResourceDetail = StackResourceDetail 
  { "StackName" :: NullOrUndefined.NullOrUndefined (StackName)
  , "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "LogicalResourceId" :: (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined.NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: (ResourceType)
  , "LastUpdatedTimestamp" :: (Number)
  , "ResourceStatus" :: (ResourceStatus)
  , "ResourceStatusReason" :: NullOrUndefined.NullOrUndefined (ResourceStatusReason)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Metadata" :: NullOrUndefined.NullOrUndefined (Metadata)
  }
derive instance newtypeStackResourceDetail :: Newtype StackResourceDetail _
derive instance repGenericStackResourceDetail :: Generic StackResourceDetail _
instance showStackResourceDetail :: Show StackResourceDetail where
  show = genericShow
instance decodeStackResourceDetail :: Decode StackResourceDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackResourceDetail :: Encode StackResourceDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackResourceSummaries = StackResourceSummaries (Array StackResourceSummary)
derive instance newtypeStackResourceSummaries :: Newtype StackResourceSummaries _
derive instance repGenericStackResourceSummaries :: Generic StackResourceSummaries _
instance showStackResourceSummaries :: Show StackResourceSummaries where
  show = genericShow
instance decodeStackResourceSummaries :: Decode StackResourceSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackResourceSummaries :: Encode StackResourceSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains high-level information about the specified stack resource.</p>
newtype StackResourceSummary = StackResourceSummary 
  { "LogicalResourceId" :: (LogicalResourceId)
  , "PhysicalResourceId" :: NullOrUndefined.NullOrUndefined (PhysicalResourceId)
  , "ResourceType" :: (ResourceType)
  , "LastUpdatedTimestamp" :: (Number)
  , "ResourceStatus" :: (ResourceStatus)
  , "ResourceStatusReason" :: NullOrUndefined.NullOrUndefined (ResourceStatusReason)
  }
derive instance newtypeStackResourceSummary :: Newtype StackResourceSummary _
derive instance repGenericStackResourceSummary :: Generic StackResourceSummary _
instance showStackResourceSummary :: Show StackResourceSummary where
  show = genericShow
instance decodeStackResourceSummary :: Decode StackResourceSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackResourceSummary :: Encode StackResourceSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackResources = StackResources (Array StackResource)
derive instance newtypeStackResources :: Newtype StackResources _
derive instance repGenericStackResources :: Generic StackResources _
instance showStackResources :: Show StackResources where
  show = genericShow
instance decodeStackResources :: Decode StackResources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackResources :: Encode StackResources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure that contains information about a stack set. A stack set enables you to provision stacks into AWS accounts and across regions by using a single CloudFormation template. In the stack set, you specify the template to use, as well as any parameters and capabilities that the template requires. </p>
newtype StackSet = StackSet 
  { "StackSetName" :: NullOrUndefined.NullOrUndefined (StackSetName)
  , "StackSetId" :: NullOrUndefined.NullOrUndefined (StackSetId)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Status" :: NullOrUndefined.NullOrUndefined (StackSetStatus)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  }
derive instance newtypeStackSet :: Newtype StackSet _
derive instance repGenericStackSet :: Generic StackSet _
instance showStackSet :: Show StackSet where
  show = genericShow
instance decodeStackSet :: Decode StackSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSet :: Encode StackSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetId = StackSetId String
derive instance newtypeStackSetId :: Newtype StackSetId _
derive instance repGenericStackSetId :: Generic StackSetId _
instance showStackSetId :: Show StackSetId where
  show = genericShow
instance decodeStackSetId :: Decode StackSetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetId :: Encode StackSetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetName = StackSetName String
derive instance newtypeStackSetName :: Newtype StackSetName _
derive instance repGenericStackSetName :: Generic StackSetName _
instance showStackSetName :: Show StackSetName where
  show = genericShow
instance decodeStackSetName :: Decode StackSetName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetName :: Encode StackSetName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetNameOrId = StackSetNameOrId String
derive instance newtypeStackSetNameOrId :: Newtype StackSetNameOrId _
derive instance repGenericStackSetNameOrId :: Generic StackSetNameOrId _
instance showStackSetNameOrId :: Show StackSetNameOrId where
  show = genericShow
instance decodeStackSetNameOrId :: Decode StackSetNameOrId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetNameOrId :: Encode StackSetNameOrId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can't yet delete this stack set, because it still contains one or more stack instances. Delete all stack instances from the stack set before deleting the stack set.</p>
newtype StackSetNotEmptyException = StackSetNotEmptyException Types.NoArguments
derive instance newtypeStackSetNotEmptyException :: Newtype StackSetNotEmptyException _
derive instance repGenericStackSetNotEmptyException :: Generic StackSetNotEmptyException _
instance showStackSetNotEmptyException :: Show StackSetNotEmptyException where
  show = genericShow
instance decodeStackSetNotEmptyException :: Decode StackSetNotEmptyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetNotEmptyException :: Encode StackSetNotEmptyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified stack set doesn't exist.</p>
newtype StackSetNotFoundException = StackSetNotFoundException Types.NoArguments
derive instance newtypeStackSetNotFoundException :: Newtype StackSetNotFoundException _
derive instance repGenericStackSetNotFoundException :: Generic StackSetNotFoundException _
instance showStackSetNotFoundException :: Show StackSetNotFoundException where
  show = genericShow
instance decodeStackSetNotFoundException :: Decode StackSetNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetNotFoundException :: Encode StackSetNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The structure that contains information about a stack set operation. </p>
newtype StackSetOperation = StackSetOperation 
  { "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "StackSetId" :: NullOrUndefined.NullOrUndefined (StackSetId)
  , "Action" :: NullOrUndefined.NullOrUndefined (StackSetOperationAction)
  , "Status" :: NullOrUndefined.NullOrUndefined (StackSetOperationStatus)
  , "OperationPreferences" :: NullOrUndefined.NullOrUndefined (StackSetOperationPreferences)
  , "RetainStacks" :: NullOrUndefined.NullOrUndefined (RetainStacksNullable)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeStackSetOperation :: Newtype StackSetOperation _
derive instance repGenericStackSetOperation :: Generic StackSetOperation _
instance showStackSetOperation :: Show StackSetOperation where
  show = genericShow
instance decodeStackSetOperation :: Decode StackSetOperation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetOperation :: Encode StackSetOperation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetOperationAction = StackSetOperationAction String
derive instance newtypeStackSetOperationAction :: Newtype StackSetOperationAction _
derive instance repGenericStackSetOperationAction :: Generic StackSetOperationAction _
instance showStackSetOperationAction :: Show StackSetOperationAction where
  show = genericShow
instance decodeStackSetOperationAction :: Decode StackSetOperationAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetOperationAction :: Encode StackSetOperationAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user-specified preferences for how AWS CloudFormation performs a stack set operation. </p> <p>For more information on maximum concurrent accounts and failure tolerance, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options">Stack set operation options</a>.</p>
newtype StackSetOperationPreferences = StackSetOperationPreferences 
  { "RegionOrder" :: NullOrUndefined.NullOrUndefined (RegionList)
  , "FailureToleranceCount" :: NullOrUndefined.NullOrUndefined (FailureToleranceCount)
  , "FailureTolerancePercentage" :: NullOrUndefined.NullOrUndefined (FailureTolerancePercentage)
  , "MaxConcurrentCount" :: NullOrUndefined.NullOrUndefined (MaxConcurrentCount)
  , "MaxConcurrentPercentage" :: NullOrUndefined.NullOrUndefined (MaxConcurrentPercentage)
  }
derive instance newtypeStackSetOperationPreferences :: Newtype StackSetOperationPreferences _
derive instance repGenericStackSetOperationPreferences :: Generic StackSetOperationPreferences _
instance showStackSetOperationPreferences :: Show StackSetOperationPreferences where
  show = genericShow
instance decodeStackSetOperationPreferences :: Decode StackSetOperationPreferences where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetOperationPreferences :: Encode StackSetOperationPreferences where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetOperationResultStatus = StackSetOperationResultStatus String
derive instance newtypeStackSetOperationResultStatus :: Newtype StackSetOperationResultStatus _
derive instance repGenericStackSetOperationResultStatus :: Generic StackSetOperationResultStatus _
instance showStackSetOperationResultStatus :: Show StackSetOperationResultStatus where
  show = genericShow
instance decodeStackSetOperationResultStatus :: Decode StackSetOperationResultStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetOperationResultStatus :: Encode StackSetOperationResultStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetOperationResultSummaries = StackSetOperationResultSummaries (Array StackSetOperationResultSummary)
derive instance newtypeStackSetOperationResultSummaries :: Newtype StackSetOperationResultSummaries _
derive instance repGenericStackSetOperationResultSummaries :: Generic StackSetOperationResultSummaries _
instance showStackSetOperationResultSummaries :: Show StackSetOperationResultSummaries where
  show = genericShow
instance decodeStackSetOperationResultSummaries :: Decode StackSetOperationResultSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetOperationResultSummaries :: Encode StackSetOperationResultSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The structure that contains information about a specified operation's results for a given account in a given region.</p>
newtype StackSetOperationResultSummary = StackSetOperationResultSummary 
  { "Account" :: NullOrUndefined.NullOrUndefined (Account)
  , "Region" :: NullOrUndefined.NullOrUndefined (Region)
  , "Status" :: NullOrUndefined.NullOrUndefined (StackSetOperationResultStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (Reason)
  , "AccountGateResult" :: NullOrUndefined.NullOrUndefined (AccountGateResult)
  }
derive instance newtypeStackSetOperationResultSummary :: Newtype StackSetOperationResultSummary _
derive instance repGenericStackSetOperationResultSummary :: Generic StackSetOperationResultSummary _
instance showStackSetOperationResultSummary :: Show StackSetOperationResultSummary where
  show = genericShow
instance decodeStackSetOperationResultSummary :: Decode StackSetOperationResultSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetOperationResultSummary :: Encode StackSetOperationResultSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetOperationStatus = StackSetOperationStatus String
derive instance newtypeStackSetOperationStatus :: Newtype StackSetOperationStatus _
derive instance repGenericStackSetOperationStatus :: Generic StackSetOperationStatus _
instance showStackSetOperationStatus :: Show StackSetOperationStatus where
  show = genericShow
instance decodeStackSetOperationStatus :: Decode StackSetOperationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetOperationStatus :: Encode StackSetOperationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetOperationSummaries = StackSetOperationSummaries (Array StackSetOperationSummary)
derive instance newtypeStackSetOperationSummaries :: Newtype StackSetOperationSummaries _
derive instance repGenericStackSetOperationSummaries :: Generic StackSetOperationSummaries _
instance showStackSetOperationSummaries :: Show StackSetOperationSummaries where
  show = genericShow
instance decodeStackSetOperationSummaries :: Decode StackSetOperationSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetOperationSummaries :: Encode StackSetOperationSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The structures that contain summary information about the specified operation.</p>
newtype StackSetOperationSummary = StackSetOperationSummary 
  { "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  , "Action" :: NullOrUndefined.NullOrUndefined (StackSetOperationAction)
  , "Status" :: NullOrUndefined.NullOrUndefined (StackSetOperationStatus)
  , "CreationTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeStackSetOperationSummary :: Newtype StackSetOperationSummary _
derive instance repGenericStackSetOperationSummary :: Generic StackSetOperationSummary _
instance showStackSetOperationSummary :: Show StackSetOperationSummary where
  show = genericShow
instance decodeStackSetOperationSummary :: Decode StackSetOperationSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetOperationSummary :: Encode StackSetOperationSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetStatus = StackSetStatus String
derive instance newtypeStackSetStatus :: Newtype StackSetStatus _
derive instance repGenericStackSetStatus :: Generic StackSetStatus _
instance showStackSetStatus :: Show StackSetStatus where
  show = genericShow
instance decodeStackSetStatus :: Decode StackSetStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetStatus :: Encode StackSetStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSetSummaries = StackSetSummaries (Array StackSetSummary)
derive instance newtypeStackSetSummaries :: Newtype StackSetSummaries _
derive instance repGenericStackSetSummaries :: Generic StackSetSummaries _
instance showStackSetSummaries :: Show StackSetSummaries where
  show = genericShow
instance decodeStackSetSummaries :: Decode StackSetSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetSummaries :: Encode StackSetSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The structures that contain summary information about the specified stack set.</p>
newtype StackSetSummary = StackSetSummary 
  { "StackSetName" :: NullOrUndefined.NullOrUndefined (StackSetName)
  , "StackSetId" :: NullOrUndefined.NullOrUndefined (StackSetId)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Status" :: NullOrUndefined.NullOrUndefined (StackSetStatus)
  }
derive instance newtypeStackSetSummary :: Newtype StackSetSummary _
derive instance repGenericStackSetSummary :: Generic StackSetSummary _
instance showStackSetSummary :: Show StackSetSummary where
  show = genericShow
instance decodeStackSetSummary :: Decode StackSetSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSetSummary :: Encode StackSetSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackStatus = StackStatus String
derive instance newtypeStackStatus :: Newtype StackStatus _
derive instance repGenericStackStatus :: Generic StackStatus _
instance showStackStatus :: Show StackStatus where
  show = genericShow
instance decodeStackStatus :: Decode StackStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackStatus :: Encode StackStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackStatusFilter = StackStatusFilter (Array StackStatus)
derive instance newtypeStackStatusFilter :: Newtype StackStatusFilter _
derive instance repGenericStackStatusFilter :: Generic StackStatusFilter _
instance showStackStatusFilter :: Show StackStatusFilter where
  show = genericShow
instance decodeStackStatusFilter :: Decode StackStatusFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackStatusFilter :: Encode StackStatusFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackStatusReason = StackStatusReason String
derive instance newtypeStackStatusReason :: Newtype StackStatusReason _
derive instance repGenericStackStatusReason :: Generic StackStatusReason _
instance showStackStatusReason :: Show StackStatusReason where
  show = genericShow
instance decodeStackStatusReason :: Decode StackStatusReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackStatusReason :: Encode StackStatusReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StackSummaries = StackSummaries (Array StackSummary)
derive instance newtypeStackSummaries :: Newtype StackSummaries _
derive instance repGenericStackSummaries :: Generic StackSummaries _
instance showStackSummaries :: Show StackSummaries where
  show = genericShow
instance decodeStackSummaries :: Decode StackSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSummaries :: Encode StackSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The StackSummary Data Type</p>
newtype StackSummary = StackSummary 
  { "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "StackName" :: (StackName)
  , "TemplateDescription" :: NullOrUndefined.NullOrUndefined (TemplateDescription)
  , "CreationTime" :: (CreationTime)
  , "LastUpdatedTime" :: NullOrUndefined.NullOrUndefined (LastUpdatedTime)
  , "DeletionTime" :: NullOrUndefined.NullOrUndefined (DeletionTime)
  , "StackStatus" :: (StackStatus)
  , "StackStatusReason" :: NullOrUndefined.NullOrUndefined (StackStatusReason)
  , "ParentId" :: NullOrUndefined.NullOrUndefined (StackId)
  , "RootId" :: NullOrUndefined.NullOrUndefined (StackId)
  }
derive instance newtypeStackSummary :: Newtype StackSummary _
derive instance repGenericStackSummary :: Generic StackSummary _
instance showStackSummary :: Show StackSummary where
  show = genericShow
instance decodeStackSummary :: Decode StackSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStackSummary :: Encode StackSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Stacks = Stacks (Array Stack)
derive instance newtypeStacks :: Newtype Stacks _
derive instance repGenericStacks :: Generic Stacks _
instance showStacks :: Show Stacks where
  show = genericShow
instance decodeStacks :: Decode Stacks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStacks :: Encode Stacks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StageList = StageList (Array TemplateStage)
derive instance newtypeStageList :: Newtype StageList _
derive instance repGenericStageList :: Generic StageList _
instance showStageList :: Show StageList where
  show = genericShow
instance decodeStageList :: Decode StageList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageList :: Encode StageList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Another operation has been performed on this stack set since the specified operation was performed. </p>
newtype StaleRequestException = StaleRequestException Types.NoArguments
derive instance newtypeStaleRequestException :: Newtype StaleRequestException _
derive instance repGenericStaleRequestException :: Generic StaleRequestException _
instance showStaleRequestException :: Show StaleRequestException where
  show = genericShow
instance decodeStaleRequestException :: Decode StaleRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStaleRequestException :: Encode StaleRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopStackSetOperationInput = StopStackSetOperationInput 
  { "StackSetName" :: (StackSetName)
  , "OperationId" :: (ClientRequestToken)
  }
derive instance newtypeStopStackSetOperationInput :: Newtype StopStackSetOperationInput _
derive instance repGenericStopStackSetOperationInput :: Generic StopStackSetOperationInput _
instance showStopStackSetOperationInput :: Show StopStackSetOperationInput where
  show = genericShow
instance decodeStopStackSetOperationInput :: Decode StopStackSetOperationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopStackSetOperationInput :: Encode StopStackSetOperationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopStackSetOperationOutput = StopStackSetOperationOutput Types.NoArguments
derive instance newtypeStopStackSetOperationOutput :: Newtype StopStackSetOperationOutput _
derive instance repGenericStopStackSetOperationOutput :: Generic StopStackSetOperationOutput _
instance showStopStackSetOperationOutput :: Show StopStackSetOperationOutput where
  show = genericShow
instance decodeStopStackSetOperationOutput :: Decode StopStackSetOperationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopStackSetOperationOutput :: Encode StopStackSetOperationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Tag type enables you to specify a key-value pair that can be used to store information about an AWS CloudFormation stack.</p>
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


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateBody = TemplateBody String
derive instance newtypeTemplateBody :: Newtype TemplateBody _
derive instance repGenericTemplateBody :: Generic TemplateBody _
instance showTemplateBody :: Show TemplateBody where
  show = genericShow
instance decodeTemplateBody :: Decode TemplateBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateBody :: Encode TemplateBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateDescription = TemplateDescription String
derive instance newtypeTemplateDescription :: Newtype TemplateDescription _
derive instance repGenericTemplateDescription :: Generic TemplateDescription _
instance showTemplateDescription :: Show TemplateDescription where
  show = genericShow
instance decodeTemplateDescription :: Decode TemplateDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateDescription :: Encode TemplateDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The TemplateParameter data type.</p>
newtype TemplateParameter = TemplateParameter 
  { "ParameterKey" :: NullOrUndefined.NullOrUndefined (ParameterKey)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (ParameterValue)
  , "NoEcho" :: NullOrUndefined.NullOrUndefined (NoEcho)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeTemplateParameter :: Newtype TemplateParameter _
derive instance repGenericTemplateParameter :: Generic TemplateParameter _
instance showTemplateParameter :: Show TemplateParameter where
  show = genericShow
instance decodeTemplateParameter :: Decode TemplateParameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateParameter :: Encode TemplateParameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateParameters = TemplateParameters (Array TemplateParameter)
derive instance newtypeTemplateParameters :: Newtype TemplateParameters _
derive instance repGenericTemplateParameters :: Generic TemplateParameters _
instance showTemplateParameters :: Show TemplateParameters where
  show = genericShow
instance decodeTemplateParameters :: Decode TemplateParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateParameters :: Encode TemplateParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateStage = TemplateStage String
derive instance newtypeTemplateStage :: Newtype TemplateStage _
derive instance repGenericTemplateStage :: Generic TemplateStage _
instance showTemplateStage :: Show TemplateStage where
  show = genericShow
instance decodeTemplateStage :: Decode TemplateStage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateStage :: Encode TemplateStage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TemplateURL = TemplateURL String
derive instance newtypeTemplateURL :: Newtype TemplateURL _
derive instance repGenericTemplateURL :: Generic TemplateURL _
instance showTemplateURL :: Show TemplateURL where
  show = genericShow
instance decodeTemplateURL :: Decode TemplateURL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTemplateURL :: Encode TemplateURL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeoutMinutes = TimeoutMinutes Int
derive instance newtypeTimeoutMinutes :: Newtype TimeoutMinutes _
derive instance repGenericTimeoutMinutes :: Generic TimeoutMinutes _
instance showTimeoutMinutes :: Show TimeoutMinutes where
  show = genericShow
instance decodeTimeoutMinutes :: Decode TimeoutMinutes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeoutMinutes :: Encode TimeoutMinutes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A client request token already exists.</p>
newtype TokenAlreadyExistsException = TokenAlreadyExistsException Types.NoArguments
derive instance newtypeTokenAlreadyExistsException :: Newtype TokenAlreadyExistsException _
derive instance repGenericTokenAlreadyExistsException :: Generic TokenAlreadyExistsException _
instance showTokenAlreadyExistsException :: Show TokenAlreadyExistsException where
  show = genericShow
instance decodeTokenAlreadyExistsException :: Decode TokenAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTokenAlreadyExistsException :: Encode TokenAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TransformName = TransformName String
derive instance newtypeTransformName :: Newtype TransformName _
derive instance repGenericTransformName :: Generic TransformName _
instance showTransformName :: Show TransformName where
  show = genericShow
instance decodeTransformName :: Decode TransformName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransformName :: Encode TransformName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TransformsList = TransformsList (Array TransformName)
derive instance newtypeTransformsList :: Newtype TransformsList _
derive instance repGenericTransformsList :: Generic TransformsList _
instance showTransformsList :: Show TransformsList where
  show = genericShow
instance decodeTransformsList :: Decode TransformsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransformsList :: Encode TransformsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Type = Type String
derive instance newtypeType :: Newtype Type _
derive instance repGenericType :: Generic Type _
instance showType :: Show Type where
  show = genericShow
instance decodeType :: Decode Type where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeType :: Encode Type where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for an <a>UpdateStack</a> action.</p>
newtype UpdateStackInput = UpdateStackInput 
  { "StackName" :: (StackName)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined.NullOrUndefined (TemplateURL)
  , "UsePreviousTemplate" :: NullOrUndefined.NullOrUndefined (UsePreviousTemplate)
  , "StackPolicyDuringUpdateBody" :: NullOrUndefined.NullOrUndefined (StackPolicyDuringUpdateBody)
  , "StackPolicyDuringUpdateURL" :: NullOrUndefined.NullOrUndefined (StackPolicyDuringUpdateURL)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "ResourceTypes" :: NullOrUndefined.NullOrUndefined (ResourceTypes)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "RollbackConfiguration" :: NullOrUndefined.NullOrUndefined (RollbackConfiguration)
  , "StackPolicyBody" :: NullOrUndefined.NullOrUndefined (StackPolicyBody)
  , "StackPolicyURL" :: NullOrUndefined.NullOrUndefined (StackPolicyURL)
  , "NotificationARNs" :: NullOrUndefined.NullOrUndefined (NotificationARNs)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  , "ClientRequestToken" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackInput :: Newtype UpdateStackInput _
derive instance repGenericUpdateStackInput :: Generic UpdateStackInput _
instance showUpdateStackInput :: Show UpdateStackInput where
  show = genericShow
instance decodeUpdateStackInput :: Decode UpdateStackInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStackInput :: Encode UpdateStackInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStackInstancesInput = UpdateStackInstancesInput 
  { "StackSetName" :: (StackSetName)
  , "Accounts" :: (AccountList)
  , "Regions" :: (RegionList)
  , "ParameterOverrides" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "OperationPreferences" :: NullOrUndefined.NullOrUndefined (StackSetOperationPreferences)
  , "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackInstancesInput :: Newtype UpdateStackInstancesInput _
derive instance repGenericUpdateStackInstancesInput :: Generic UpdateStackInstancesInput _
instance showUpdateStackInstancesInput :: Show UpdateStackInstancesInput where
  show = genericShow
instance decodeUpdateStackInstancesInput :: Decode UpdateStackInstancesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStackInstancesInput :: Encode UpdateStackInstancesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStackInstancesOutput = UpdateStackInstancesOutput 
  { "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackInstancesOutput :: Newtype UpdateStackInstancesOutput _
derive instance repGenericUpdateStackInstancesOutput :: Generic UpdateStackInstancesOutput _
instance showUpdateStackInstancesOutput :: Show UpdateStackInstancesOutput where
  show = genericShow
instance decodeUpdateStackInstancesOutput :: Decode UpdateStackInstancesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStackInstancesOutput :: Encode UpdateStackInstancesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for an <a>UpdateStack</a> action.</p>
newtype UpdateStackOutput = UpdateStackOutput 
  { "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  }
derive instance newtypeUpdateStackOutput :: Newtype UpdateStackOutput _
derive instance repGenericUpdateStackOutput :: Generic UpdateStackOutput _
instance showUpdateStackOutput :: Show UpdateStackOutput where
  show = genericShow
instance decodeUpdateStackOutput :: Decode UpdateStackOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStackOutput :: Encode UpdateStackOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStackSetInput = UpdateStackSetInput 
  { "StackSetName" :: (StackSetName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined.NullOrUndefined (TemplateURL)
  , "UsePreviousTemplate" :: NullOrUndefined.NullOrUndefined (UsePreviousTemplate)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (Parameters)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  , "OperationPreferences" :: NullOrUndefined.NullOrUndefined (StackSetOperationPreferences)
  , "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackSetInput :: Newtype UpdateStackSetInput _
derive instance repGenericUpdateStackSetInput :: Generic UpdateStackSetInput _
instance showUpdateStackSetInput :: Show UpdateStackSetInput where
  show = genericShow
instance decodeUpdateStackSetInput :: Decode UpdateStackSetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStackSetInput :: Encode UpdateStackSetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStackSetOutput = UpdateStackSetOutput 
  { "OperationId" :: NullOrUndefined.NullOrUndefined (ClientRequestToken)
  }
derive instance newtypeUpdateStackSetOutput :: Newtype UpdateStackSetOutput _
derive instance repGenericUpdateStackSetOutput :: Generic UpdateStackSetOutput _
instance showUpdateStackSetOutput :: Show UpdateStackSetOutput where
  show = genericShow
instance decodeUpdateStackSetOutput :: Decode UpdateStackSetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStackSetOutput :: Encode UpdateStackSetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTerminationProtectionInput = UpdateTerminationProtectionInput 
  { "EnableTerminationProtection" :: (EnableTerminationProtection)
  , "StackName" :: (StackNameOrId)
  }
derive instance newtypeUpdateTerminationProtectionInput :: Newtype UpdateTerminationProtectionInput _
derive instance repGenericUpdateTerminationProtectionInput :: Generic UpdateTerminationProtectionInput _
instance showUpdateTerminationProtectionInput :: Show UpdateTerminationProtectionInput where
  show = genericShow
instance decodeUpdateTerminationProtectionInput :: Decode UpdateTerminationProtectionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTerminationProtectionInput :: Encode UpdateTerminationProtectionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTerminationProtectionOutput = UpdateTerminationProtectionOutput 
  { "StackId" :: NullOrUndefined.NullOrUndefined (StackId)
  }
derive instance newtypeUpdateTerminationProtectionOutput :: Newtype UpdateTerminationProtectionOutput _
derive instance repGenericUpdateTerminationProtectionOutput :: Generic UpdateTerminationProtectionOutput _
instance showUpdateTerminationProtectionOutput :: Show UpdateTerminationProtectionOutput where
  show = genericShow
instance decodeUpdateTerminationProtectionOutput :: Decode UpdateTerminationProtectionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTerminationProtectionOutput :: Encode UpdateTerminationProtectionOutput where
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


newtype UsePreviousTemplate = UsePreviousTemplate Boolean
derive instance newtypeUsePreviousTemplate :: Newtype UsePreviousTemplate _
derive instance repGenericUsePreviousTemplate :: Generic UsePreviousTemplate _
instance showUsePreviousTemplate :: Show UsePreviousTemplate where
  show = genericShow
instance decodeUsePreviousTemplate :: Decode UsePreviousTemplate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsePreviousTemplate :: Encode UsePreviousTemplate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsePreviousValue = UsePreviousValue Boolean
derive instance newtypeUsePreviousValue :: Newtype UsePreviousValue _
derive instance repGenericUsePreviousValue :: Generic UsePreviousValue _
instance showUsePreviousValue :: Show UsePreviousValue where
  show = genericShow
instance decodeUsePreviousValue :: Decode UsePreviousValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsePreviousValue :: Encode UsePreviousValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for <a>ValidateTemplate</a> action.</p>
newtype ValidateTemplateInput = ValidateTemplateInput 
  { "TemplateBody" :: NullOrUndefined.NullOrUndefined (TemplateBody)
  , "TemplateURL" :: NullOrUndefined.NullOrUndefined (TemplateURL)
  }
derive instance newtypeValidateTemplateInput :: Newtype ValidateTemplateInput _
derive instance repGenericValidateTemplateInput :: Generic ValidateTemplateInput _
instance showValidateTemplateInput :: Show ValidateTemplateInput where
  show = genericShow
instance decodeValidateTemplateInput :: Decode ValidateTemplateInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidateTemplateInput :: Encode ValidateTemplateInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for <a>ValidateTemplate</a> action.</p>
newtype ValidateTemplateOutput = ValidateTemplateOutput 
  { "Parameters" :: NullOrUndefined.NullOrUndefined (TemplateParameters)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Capabilities" :: NullOrUndefined.NullOrUndefined (Capabilities)
  , "CapabilitiesReason" :: NullOrUndefined.NullOrUndefined (CapabilitiesReason)
  , "DeclaredTransforms" :: NullOrUndefined.NullOrUndefined (TransformsList)
  }
derive instance newtypeValidateTemplateOutput :: Newtype ValidateTemplateOutput _
derive instance repGenericValidateTemplateOutput :: Generic ValidateTemplateOutput _
instance showValidateTemplateOutput :: Show ValidateTemplateOutput where
  show = genericShow
instance decodeValidateTemplateOutput :: Decode ValidateTemplateOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidateTemplateOutput :: Encode ValidateTemplateOutput where
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
