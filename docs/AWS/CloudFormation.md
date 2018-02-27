## Module AWS.CloudFormation

<fullname>AWS CloudFormation</fullname> <p>AWS CloudFormation allows you to create and manage AWS infrastructure deployments predictably and repeatedly. You can use AWS CloudFormation to leverage AWS products, such as Amazon Elastic Compute Cloud, Amazon Elastic Block Store, Amazon Simple Notification Service, Elastic Load Balancing, and Auto Scaling to build highly-reliable, highly scalable, cost-effective applications without creating or configuring the underlying AWS infrastructure.</p> <p>With AWS CloudFormation, you declare all of your resources and dependencies in a template file. The template defines a collection of resources as a single unit called a stack. AWS CloudFormation creates and deletes all member resources of the stack together and manages all dependencies between the resources for you.</p> <p>For more information about AWS CloudFormation, see the <a href="http://aws.amazon.com/cloudformation/">AWS CloudFormation Product Page</a>.</p> <p>Amazon CloudFormation makes use of other AWS products. If you need additional technical information about a specific AWS product, you can find the product's technical documentation at <a href="http://docs.aws.amazon.com/">docs.aws.amazon.com</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `cancelUpdateStack`

``` purescript
cancelUpdateStack :: forall eff. CancelUpdateStackInput -> Aff (err :: RequestError | eff) Unit
```

<p>Cancels an update on the specified stack. If the call completes successfully, the stack rolls back the update and reverts to the previous stack configuration.</p> <note> <p>You can cancel only stacks that are in the UPDATE_IN_PROGRESS state.</p> </note>

#### `continueUpdateRollback`

``` purescript
continueUpdateRollback :: forall eff. ContinueUpdateRollbackInput -> Aff (err :: RequestError | eff) ContinueUpdateRollbackOutput
```

<p>For a specified stack that is in the <code>UPDATE_ROLLBACK_FAILED</code> state, continues rolling it back to the <code>UPDATE_ROLLBACK_COMPLETE</code> state. Depending on the cause of the failure, you can manually <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/troubleshooting.html#troubleshooting-errors-update-rollback-failed"> fix the error</a> and continue the rollback. By continuing the rollback, you can return your stack to a working state (the <code>UPDATE_ROLLBACK_COMPLETE</code> state), and then try to update the stack again.</p> <p>A stack goes into the <code>UPDATE_ROLLBACK_FAILED</code> state when AWS CloudFormation cannot roll back all changes after a failed stack update. For example, you might have a stack that is rolling back to an old database instance that was deleted outside of AWS CloudFormation. Because AWS CloudFormation doesn't know the database was deleted, it assumes that the database instance still exists and attempts to roll back to it, causing the update rollback to fail.</p>

#### `createChangeSet`

``` purescript
createChangeSet :: forall eff. CreateChangeSetInput -> Aff (err :: RequestError | eff) CreateChangeSetOutput
```

<p>Creates a list of changes that will be applied to a stack so that you can review the changes before executing them. You can create a change set for a stack that doesn't exist or an existing stack. If you create a change set for a stack that doesn't exist, the change set shows all of the resources that AWS CloudFormation will create. If you create a change set for an existing stack, AWS CloudFormation compares the stack's information with the information that you submit in the change set and lists the differences. Use change sets to understand which resources AWS CloudFormation will create or change, and how it will change resources in an existing stack, before you create or update a stack.</p> <p>To create a change set for a stack that doesn't exist, for the <code>ChangeSetType</code> parameter, specify <code>CREATE</code>. To create a change set for an existing stack, specify <code>UPDATE</code> for the <code>ChangeSetType</code> parameter. After the <code>CreateChangeSet</code> call successfully completes, AWS CloudFormation starts creating the change set. To check the status of the change set or to review it, use the <a>DescribeChangeSet</a> action.</p> <p>When you are satisfied with the changes the change set will make, execute the change set by using the <a>ExecuteChangeSet</a> action. AWS CloudFormation doesn't make changes until you execute the change set.</p>

#### `createStack`

``` purescript
createStack :: forall eff. CreateStackInput -> Aff (err :: RequestError | eff) CreateStackOutput
```

<p>Creates a stack as specified in the template. After the call completes successfully, the stack creation starts. You can check the status of the stack via the <a>DescribeStacks</a> API.</p>

#### `createStackInstances`

``` purescript
createStackInstances :: forall eff. CreateStackInstancesInput -> Aff (err :: RequestError | eff) CreateStackInstancesOutput
```

<p>Creates stack instances for the specified accounts, within the specified regions. A stack instance refers to a stack in a specific account and region. <code>Accounts</code> and <code>Regions</code> are required parameters—you must specify at least one account and one region. </p>

#### `createStackSet`

``` purescript
createStackSet :: forall eff. CreateStackSetInput -> Aff (err :: RequestError | eff) CreateStackSetOutput
```

<p>Creates a stack set.</p>

#### `deleteChangeSet`

``` purescript
deleteChangeSet :: forall eff. DeleteChangeSetInput -> Aff (err :: RequestError | eff) DeleteChangeSetOutput
```

<p>Deletes the specified change set. Deleting change sets ensures that no one executes the wrong change set.</p> <p>If the call successfully completes, AWS CloudFormation successfully deleted the change set.</p>

#### `deleteStack`

``` purescript
deleteStack :: forall eff. DeleteStackInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a specified stack. Once the call completes successfully, stack deletion starts. Deleted stacks do not show up in the <a>DescribeStacks</a> API if the deletion has been completed successfully.</p>

#### `deleteStackInstances`

``` purescript
deleteStackInstances :: forall eff. DeleteStackInstancesInput -> Aff (err :: RequestError | eff) DeleteStackInstancesOutput
```

<p>Deletes stack instances for the specified accounts, in the specified regions. </p>

#### `deleteStackSet`

``` purescript
deleteStackSet :: forall eff. DeleteStackSetInput -> Aff (err :: RequestError | eff) DeleteStackSetOutput
```

<p>Deletes a stack set. Before you can delete a stack set, all of its member stack instances must be deleted. For more information about how to do this, see <a>DeleteStackInstances</a>. </p>

#### `describeAccountLimits`

``` purescript
describeAccountLimits :: forall eff. DescribeAccountLimitsInput -> Aff (err :: RequestError | eff) DescribeAccountLimitsOutput
```

<p>Retrieves your account's AWS CloudFormation limits, such as the maximum number of stacks that you can create in your account.</p>

#### `describeChangeSet`

``` purescript
describeChangeSet :: forall eff. DescribeChangeSetInput -> Aff (err :: RequestError | eff) DescribeChangeSetOutput
```

<p>Returns the inputs for the change set and a list of changes that AWS CloudFormation will make if you execute the change set. For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-changesets.html">Updating Stacks Using Change Sets</a> in the AWS CloudFormation User Guide.</p>

#### `describeStackEvents`

``` purescript
describeStackEvents :: forall eff. DescribeStackEventsInput -> Aff (err :: RequestError | eff) DescribeStackEventsOutput
```

<p>Returns all stack related events for a specified stack in reverse chronological order. For more information about a stack's event history, go to <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/concept-stack.html">Stacks</a> in the AWS CloudFormation User Guide.</p> <note> <p>You can list events for stacks that have failed to create or have been deleted by specifying the unique stack identifier (stack ID).</p> </note>

#### `describeStackInstance`

``` purescript
describeStackInstance :: forall eff. DescribeStackInstanceInput -> Aff (err :: RequestError | eff) DescribeStackInstanceOutput
```

<p>Returns the stack instance that's associated with the specified stack set, AWS account, and region.</p> <p>For a list of stack instances that are associated with a specific stack set, use <a>ListStackInstances</a>.</p>

#### `describeStackResource`

``` purescript
describeStackResource :: forall eff. DescribeStackResourceInput -> Aff (err :: RequestError | eff) DescribeStackResourceOutput
```

<p>Returns a description of the specified resource in the specified stack.</p> <p>For deleted stacks, DescribeStackResource returns resource information for up to 90 days after the stack has been deleted.</p>

#### `describeStackResources`

``` purescript
describeStackResources :: forall eff. DescribeStackResourcesInput -> Aff (err :: RequestError | eff) DescribeStackResourcesOutput
```

<p>Returns AWS resource descriptions for running and deleted stacks. If <code>StackName</code> is specified, all the associated resources that are part of the stack are returned. If <code>PhysicalResourceId</code> is specified, the associated resources of the stack that the resource belongs to are returned.</p> <note> <p>Only the first 100 resources will be returned. If your stack has more resources than this, you should use <code>ListStackResources</code> instead.</p> </note> <p>For deleted stacks, <code>DescribeStackResources</code> returns resource information for up to 90 days after the stack has been deleted.</p> <p>You must specify either <code>StackName</code> or <code>PhysicalResourceId</code>, but not both. In addition, you can specify <code>LogicalResourceId</code> to filter the returned result. For more information about resources, the <code>LogicalResourceId</code> and <code>PhysicalResourceId</code>, go to the <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/">AWS CloudFormation User Guide</a>.</p> <note> <p>A <code>ValidationError</code> is returned if you specify both <code>StackName</code> and <code>PhysicalResourceId</code> in the same request.</p> </note>

#### `describeStackSet`

``` purescript
describeStackSet :: forall eff. DescribeStackSetInput -> Aff (err :: RequestError | eff) DescribeStackSetOutput
```

<p>Returns the description of the specified stack set. </p>

#### `describeStackSetOperation`

``` purescript
describeStackSetOperation :: forall eff. DescribeStackSetOperationInput -> Aff (err :: RequestError | eff) DescribeStackSetOperationOutput
```

<p>Returns the description of the specified stack set operation. </p>

#### `describeStacks`

``` purescript
describeStacks :: forall eff. DescribeStacksInput -> Aff (err :: RequestError | eff) DescribeStacksOutput
```

<p>Returns the description for the specified stack; if no stack name was specified, then it returns the description for all the stacks created.</p> <note> <p>If the stack does not exist, an <code>AmazonCloudFormationException</code> is returned.</p> </note>

#### `estimateTemplateCost`

``` purescript
estimateTemplateCost :: forall eff. EstimateTemplateCostInput -> Aff (err :: RequestError | eff) EstimateTemplateCostOutput
```

<p>Returns the estimated monthly cost of a template. The return value is an AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.</p>

#### `executeChangeSet`

``` purescript
executeChangeSet :: forall eff. ExecuteChangeSetInput -> Aff (err :: RequestError | eff) ExecuteChangeSetOutput
```

<p>Updates a stack using the input information that was provided when the specified change set was created. After the call successfully completes, AWS CloudFormation starts updating the stack. Use the <a>DescribeStacks</a> action to view the status of the update.</p> <p>When you execute a change set, AWS CloudFormation deletes all other change sets associated with the stack because they aren't valid for the updated stack.</p> <p>If a stack policy is associated with the stack, AWS CloudFormation enforces the policy during the update. You can't specify a temporary stack policy that overrides the current policy.</p>

#### `getStackPolicy`

``` purescript
getStackPolicy :: forall eff. GetStackPolicyInput -> Aff (err :: RequestError | eff) GetStackPolicyOutput
```

<p>Returns the stack policy for a specified stack. If a stack doesn't have a policy, a null value is returned.</p>

#### `getTemplate`

``` purescript
getTemplate :: forall eff. GetTemplateInput -> Aff (err :: RequestError | eff) GetTemplateOutput
```

<p>Returns the template body for a specified stack. You can get the template for running or deleted stacks.</p> <p>For deleted stacks, GetTemplate returns the template for up to 90 days after the stack has been deleted.</p> <note> <p> If the template does not exist, a <code>ValidationError</code> is returned. </p> </note>

#### `getTemplateSummary`

``` purescript
getTemplateSummary :: forall eff. GetTemplateSummaryInput -> Aff (err :: RequestError | eff) GetTemplateSummaryOutput
```

<p>Returns information about a new or existing template. The <code>GetTemplateSummary</code> action is useful for viewing parameter information, such as default parameter values and parameter types, before you create or update a stack or stack set.</p> <p>You can use the <code>GetTemplateSummary</code> action when you submit a template, or you can get template information for a stack set, or a running or deleted stack.</p> <p>For deleted stacks, <code>GetTemplateSummary</code> returns the template information for up to 90 days after the stack has been deleted. If the template does not exist, a <code>ValidationError</code> is returned.</p>

#### `listChangeSets`

``` purescript
listChangeSets :: forall eff. ListChangeSetsInput -> Aff (err :: RequestError | eff) ListChangeSetsOutput
```

<p>Returns the ID and status of each active change set for a stack. For example, AWS CloudFormation lists change sets that are in the <code>CREATE_IN_PROGRESS</code> or <code>CREATE_PENDING</code> state.</p>

#### `listExports`

``` purescript
listExports :: forall eff. ListExportsInput -> Aff (err :: RequestError | eff) ListExportsOutput
```

<p>Lists all exported output values in the account and region in which you call this action. Use this action to see the exported output values that you can import into other stacks. To import values, use the <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html"> <code>Fn::ImportValue</code> </a> function. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-exports.html"> AWS CloudFormation Export Stack Output Values</a>.</p>

#### `listImports`

``` purescript
listImports :: forall eff. ListImportsInput -> Aff (err :: RequestError | eff) ListImportsOutput
```

<p>Lists all stacks that are importing an exported output value. To modify or remove an exported output value, first use this action to see which stacks are using it. To see the exported output values in your account, see <a>ListExports</a>. </p> <p>For more information about importing an exported output value, see the <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html"> <code>Fn::ImportValue</code> </a> function. </p>

#### `listStackInstances`

``` purescript
listStackInstances :: forall eff. ListStackInstancesInput -> Aff (err :: RequestError | eff) ListStackInstancesOutput
```

<p>Returns summary information about stack instances that are associated with the specified stack set. You can filter for stack instances that are associated with a specific AWS account name or region.</p>

#### `listStackResources`

``` purescript
listStackResources :: forall eff. ListStackResourcesInput -> Aff (err :: RequestError | eff) ListStackResourcesOutput
```

<p>Returns descriptions of all resources of the specified stack.</p> <p>For deleted stacks, ListStackResources returns resource information for up to 90 days after the stack has been deleted.</p>

#### `listStackSetOperationResults`

``` purescript
listStackSetOperationResults :: forall eff. ListStackSetOperationResultsInput -> Aff (err :: RequestError | eff) ListStackSetOperationResultsOutput
```

<p>Returns summary information about the results of a stack set operation. </p>

#### `listStackSetOperations`

``` purescript
listStackSetOperations :: forall eff. ListStackSetOperationsInput -> Aff (err :: RequestError | eff) ListStackSetOperationsOutput
```

<p>Returns summary information about operations performed on a stack set. </p>

#### `listStackSets`

``` purescript
listStackSets :: forall eff. ListStackSetsInput -> Aff (err :: RequestError | eff) ListStackSetsOutput
```

<p>Returns summary information about stack sets that are associated with the user.</p>

#### `listStacks`

``` purescript
listStacks :: forall eff. ListStacksInput -> Aff (err :: RequestError | eff) ListStacksOutput
```

<p>Returns the summary information for stacks whose status matches the specified StackStatusFilter. Summary information for stacks that have been deleted is kept for 90 days after the stack is deleted. If no StackStatusFilter is specified, summary information for all stacks is returned (including existing stacks and stacks that have been deleted).</p>

#### `setStackPolicy`

``` purescript
setStackPolicy :: forall eff. SetStackPolicyInput -> Aff (err :: RequestError | eff) Unit
```

<p>Sets a stack policy for a specified stack.</p>

#### `signalResource`

``` purescript
signalResource :: forall eff. SignalResourceInput -> Aff (err :: RequestError | eff) Unit
```

<p>Sends a signal to the specified resource with a success or failure status. You can use the SignalResource API in conjunction with a creation policy or update policy. AWS CloudFormation doesn't proceed with a stack creation or update until resources receive the required number of signals or the timeout period is exceeded. The SignalResource API is useful in cases where you want to send signals from anywhere other than an Amazon EC2 instance.</p>

#### `stopStackSetOperation`

``` purescript
stopStackSetOperation :: forall eff. StopStackSetOperationInput -> Aff (err :: RequestError | eff) StopStackSetOperationOutput
```

<p>Stops an in-progress operation on a stack set and its associated stack instances. </p>

#### `updateStack`

``` purescript
updateStack :: forall eff. UpdateStackInput -> Aff (err :: RequestError | eff) UpdateStackOutput
```

<p>Updates a stack as specified in the template. After the call completes successfully, the stack update starts. You can check the status of the stack via the <a>DescribeStacks</a> action.</p> <p>To get a copy of the template for an existing stack, you can use the <a>GetTemplate</a> action.</p> <p>For more information about creating an update template, updating a stack, and monitoring the progress of the update, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks.html">Updating a Stack</a>.</p>

#### `updateStackInstances`

``` purescript
updateStackInstances :: forall eff. UpdateStackInstancesInput -> Aff (err :: RequestError | eff) UpdateStackInstancesOutput
```

<p>Updates the parameter values for stack instances for the specified accounts, within the specified regions. A stack instance refers to a stack in a specific account and region. </p> <p>You can only update stack instances in regions and accounts where they already exist; to create additional stack instances, use <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CreateStackInstances.html">CreateStackInstances</a>. </p> <p>During stack set updates, any parameters overridden for a stack instance are not updated, but retain their overridden value.</p> <p>You can only update the parameter <i>values</i> that are specified in the stack set; to add or delete a parameter itself, use <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html">UpdateStackSet</a> to update the stack set template. If you add a parameter to a template, before you can override the parameter value specified in the stack set you must first use <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_UpdateStackSet.html">UpdateStackSet</a> to update all stack instances with the updated template and parameter value specified in the stack set. Once a stack instance has been updated with the new parameter, you can then override the parameter value using <code>UpdateStackInstances</code>.</p>

#### `updateStackSet`

``` purescript
updateStackSet :: forall eff. UpdateStackSetInput -> Aff (err :: RequestError | eff) UpdateStackSetOutput
```

<p>Updates the stack set and <i>all</i> associated stack instances.</p> <p>Even if the stack set operation created by updating the stack set fails (completely or partially, below or above a specified failure tolerance), the stack set is updated with your changes. Subsequent <a>CreateStackInstances</a> calls on the specified stack set use the updated stack set.</p>

#### `updateTerminationProtection`

``` purescript
updateTerminationProtection :: forall eff. UpdateTerminationProtectionInput -> Aff (err :: RequestError | eff) UpdateTerminationProtectionOutput
```

<p>Updates termination protection for the specified stack. If a user attempts to delete a stack with termination protection enabled, the operation fails and the stack remains unchanged. For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-protect-stacks.html">Protecting a Stack From Being Deleted</a> in the <i>AWS CloudFormation User Guide</i>.</p> <p> For <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-nested-stacks.html">nested stacks</a>, termination protection is set on the root stack and cannot be changed directly on the nested stack.</p>

#### `validateTemplate`

``` purescript
validateTemplate :: forall eff. ValidateTemplateInput -> Aff (err :: RequestError | eff) ValidateTemplateOutput
```

<p>Validates a specified template. AWS CloudFormation first checks if the template is valid JSON. If it isn't, AWS CloudFormation checks if the template is valid YAML. If both these checks fail, AWS CloudFormation returns a template validation error.</p>

#### `Account`

``` purescript
newtype Account
  = Account String
```

##### Instances
``` purescript
Newtype Account _
```

#### `AccountGateResult`

``` purescript
newtype AccountGateResult
  = AccountGateResult { "Status" :: NullOrUndefined (AccountGateStatus), "StatusReason" :: NullOrUndefined (AccountGateStatusReason) }
```

<p>Structure that contains the results of the account gate function which AWS CloudFormation invokes, if present, before proceeding with a stack set operation in an account and region.</p> <p>For each account and region, AWS CloudFormation lets you specify a Lamdba function that encapsulates any requirements that must be met before CloudFormation can proceed with a stack set operation in that account and region. CloudFormation invokes the function each time a stack set operation is requested for that account and region; if the function returns <code>FAILED</code>, CloudFormation cancels the operation in that account and region, and sets the stack set operation result status for that account and region to <code>FAILED</code>. </p> <p>For more information, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-account-gating.html">Configuring a target account gate</a>.</p>

##### Instances
``` purescript
Newtype AccountGateResult _
```

#### `AccountGateStatus`

``` purescript
newtype AccountGateStatus
  = AccountGateStatus String
```

##### Instances
``` purescript
Newtype AccountGateStatus _
```

#### `AccountGateStatusReason`

``` purescript
newtype AccountGateStatusReason
  = AccountGateStatusReason String
```

##### Instances
``` purescript
Newtype AccountGateStatusReason _
```

#### `AccountLimit`

``` purescript
newtype AccountLimit
  = AccountLimit { "Name" :: NullOrUndefined (LimitName), "Value" :: NullOrUndefined (LimitValue) }
```

<p>The AccountLimit data type.</p>

##### Instances
``` purescript
Newtype AccountLimit _
```

#### `AccountLimitList`

``` purescript
newtype AccountLimitList
  = AccountLimitList (Array AccountLimit)
```

##### Instances
``` purescript
Newtype AccountLimitList _
```

#### `AccountList`

``` purescript
newtype AccountList
  = AccountList (Array Account)
```

##### Instances
``` purescript
Newtype AccountList _
```

#### `AllowedValue`

``` purescript
newtype AllowedValue
  = AllowedValue String
```

##### Instances
``` purescript
Newtype AllowedValue _
```

#### `AllowedValues`

``` purescript
newtype AllowedValues
  = AllowedValues (Array AllowedValue)
```

##### Instances
``` purescript
Newtype AllowedValues _
```

#### `AlreadyExistsException`

``` purescript
newtype AlreadyExistsException
  = AlreadyExistsException {  }
```

<p>The resource with the name requested already exists.</p>

##### Instances
``` purescript
Newtype AlreadyExistsException _
```

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

##### Instances
``` purescript
Newtype Arn _
```

#### `CancelUpdateStackInput`

``` purescript
newtype CancelUpdateStackInput
  = CancelUpdateStackInput { "StackName" :: StackName, "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

<p>The input for the <a>CancelUpdateStack</a> action.</p>

##### Instances
``` purescript
Newtype CancelUpdateStackInput _
```

#### `Capabilities`

``` purescript
newtype Capabilities
  = Capabilities (Array Capability)
```

##### Instances
``` purescript
Newtype Capabilities _
```

#### `CapabilitiesReason`

``` purescript
newtype CapabilitiesReason
  = CapabilitiesReason String
```

##### Instances
``` purescript
Newtype CapabilitiesReason _
```

#### `Capability`

``` purescript
newtype Capability
  = Capability String
```

##### Instances
``` purescript
Newtype Capability _
```

#### `CausingEntity`

``` purescript
newtype CausingEntity
  = CausingEntity String
```

##### Instances
``` purescript
Newtype CausingEntity _
```

#### `Change`

``` purescript
newtype Change
  = Change { "Type" :: NullOrUndefined (ChangeType), "ResourceChange" :: NullOrUndefined (ResourceChange) }
```

<p>The <code>Change</code> structure describes the changes AWS CloudFormation will perform if you execute the change set.</p>

##### Instances
``` purescript
Newtype Change _
```

#### `ChangeAction`

``` purescript
newtype ChangeAction
  = ChangeAction String
```

##### Instances
``` purescript
Newtype ChangeAction _
```

#### `ChangeSetId`

``` purescript
newtype ChangeSetId
  = ChangeSetId String
```

##### Instances
``` purescript
Newtype ChangeSetId _
```

#### `ChangeSetName`

``` purescript
newtype ChangeSetName
  = ChangeSetName String
```

##### Instances
``` purescript
Newtype ChangeSetName _
```

#### `ChangeSetNameOrId`

``` purescript
newtype ChangeSetNameOrId
  = ChangeSetNameOrId String
```

##### Instances
``` purescript
Newtype ChangeSetNameOrId _
```

#### `ChangeSetNotFoundException`

``` purescript
newtype ChangeSetNotFoundException
  = ChangeSetNotFoundException {  }
```

<p>The specified change set name or ID doesn't exit. To view valid change sets for a stack, use the <code>ListChangeSets</code> action.</p>

##### Instances
``` purescript
Newtype ChangeSetNotFoundException _
```

#### `ChangeSetStatus`

``` purescript
newtype ChangeSetStatus
  = ChangeSetStatus String
```

##### Instances
``` purescript
Newtype ChangeSetStatus _
```

#### `ChangeSetStatusReason`

``` purescript
newtype ChangeSetStatusReason
  = ChangeSetStatusReason String
```

##### Instances
``` purescript
Newtype ChangeSetStatusReason _
```

#### `ChangeSetSummaries`

``` purescript
newtype ChangeSetSummaries
  = ChangeSetSummaries (Array ChangeSetSummary)
```

##### Instances
``` purescript
Newtype ChangeSetSummaries _
```

#### `ChangeSetSummary`

``` purescript
newtype ChangeSetSummary
  = ChangeSetSummary { "StackId" :: NullOrUndefined (StackId), "StackName" :: NullOrUndefined (StackName), "ChangeSetId" :: NullOrUndefined (ChangeSetId), "ChangeSetName" :: NullOrUndefined (ChangeSetName), "ExecutionStatus" :: NullOrUndefined (ExecutionStatus), "Status" :: NullOrUndefined (ChangeSetStatus), "StatusReason" :: NullOrUndefined (ChangeSetStatusReason), "CreationTime" :: NullOrUndefined (CreationTime), "Description" :: NullOrUndefined (Description) }
```

<p>The <code>ChangeSetSummary</code> structure describes a change set, its status, and the stack with which it's associated.</p>

##### Instances
``` purescript
Newtype ChangeSetSummary _
```

#### `ChangeSetType`

``` purescript
newtype ChangeSetType
  = ChangeSetType String
```

##### Instances
``` purescript
Newtype ChangeSetType _
```

#### `ChangeSource`

``` purescript
newtype ChangeSource
  = ChangeSource String
```

##### Instances
``` purescript
Newtype ChangeSource _
```

#### `ChangeType`

``` purescript
newtype ChangeType
  = ChangeType String
```

##### Instances
``` purescript
Newtype ChangeType _
```

#### `Changes`

``` purescript
newtype Changes
  = Changes (Array Change)
```

##### Instances
``` purescript
Newtype Changes _
```

#### `ClientRequestToken`

``` purescript
newtype ClientRequestToken
  = ClientRequestToken String
```

##### Instances
``` purescript
Newtype ClientRequestToken _
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

#### `ContinueUpdateRollbackInput`

``` purescript
newtype ContinueUpdateRollbackInput
  = ContinueUpdateRollbackInput { "StackName" :: StackNameOrId, "RoleARN" :: NullOrUndefined (RoleARN), "ResourcesToSkip" :: NullOrUndefined (ResourcesToSkip), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

<p>The input for the <a>ContinueUpdateRollback</a> action.</p>

##### Instances
``` purescript
Newtype ContinueUpdateRollbackInput _
```

#### `ContinueUpdateRollbackOutput`

``` purescript
newtype ContinueUpdateRollbackOutput
  = ContinueUpdateRollbackOutput {  }
```

<p>The output for a <a>ContinueUpdateRollback</a> action.</p>

##### Instances
``` purescript
Newtype ContinueUpdateRollbackOutput _
```

#### `CreateChangeSetInput`

``` purescript
newtype CreateChangeSetInput
  = CreateChangeSetInput { "StackName" :: StackNameOrId, "TemplateBody" :: NullOrUndefined (TemplateBody), "TemplateURL" :: NullOrUndefined (TemplateURL), "UsePreviousTemplate" :: NullOrUndefined (UsePreviousTemplate), "Parameters" :: NullOrUndefined (Parameters), "Capabilities" :: NullOrUndefined (Capabilities), "ResourceTypes" :: NullOrUndefined (ResourceTypes), "RoleARN" :: NullOrUndefined (RoleARN), "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration), "NotificationARNs" :: NullOrUndefined (NotificationARNs), "Tags" :: NullOrUndefined (Tags), "ChangeSetName" :: ChangeSetName, "ClientToken" :: NullOrUndefined (ClientToken), "Description" :: NullOrUndefined (Description), "ChangeSetType" :: NullOrUndefined (ChangeSetType) }
```

<p>The input for the <a>CreateChangeSet</a> action.</p>

##### Instances
``` purescript
Newtype CreateChangeSetInput _
```

#### `CreateChangeSetOutput`

``` purescript
newtype CreateChangeSetOutput
  = CreateChangeSetOutput { "Id" :: NullOrUndefined (ChangeSetId), "StackId" :: NullOrUndefined (StackId) }
```

<p>The output for the <a>CreateChangeSet</a> action.</p>

##### Instances
``` purescript
Newtype CreateChangeSetOutput _
```

#### `CreateStackInput`

``` purescript
newtype CreateStackInput
  = CreateStackInput { "StackName" :: StackName, "TemplateBody" :: NullOrUndefined (TemplateBody), "TemplateURL" :: NullOrUndefined (TemplateURL), "Parameters" :: NullOrUndefined (Parameters), "DisableRollback" :: NullOrUndefined (DisableRollback), "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration), "TimeoutInMinutes" :: NullOrUndefined (TimeoutMinutes), "NotificationARNs" :: NullOrUndefined (NotificationARNs), "Capabilities" :: NullOrUndefined (Capabilities), "ResourceTypes" :: NullOrUndefined (ResourceTypes), "RoleARN" :: NullOrUndefined (RoleARN), "OnFailure" :: NullOrUndefined (OnFailure), "StackPolicyBody" :: NullOrUndefined (StackPolicyBody), "StackPolicyURL" :: NullOrUndefined (StackPolicyURL), "Tags" :: NullOrUndefined (Tags), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken), "EnableTerminationProtection" :: NullOrUndefined (EnableTerminationProtection) }
```

<p>The input for <a>CreateStack</a> action.</p>

##### Instances
``` purescript
Newtype CreateStackInput _
```

#### `CreateStackInstancesInput`

``` purescript
newtype CreateStackInstancesInput
  = CreateStackInstancesInput { "StackSetName" :: StackSetName, "Accounts" :: AccountList, "Regions" :: RegionList, "ParameterOverrides" :: NullOrUndefined (Parameters), "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences), "OperationId" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype CreateStackInstancesInput _
```

#### `CreateStackInstancesOutput`

``` purescript
newtype CreateStackInstancesOutput
  = CreateStackInstancesOutput { "OperationId" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype CreateStackInstancesOutput _
```

#### `CreateStackOutput`

``` purescript
newtype CreateStackOutput
  = CreateStackOutput { "StackId" :: NullOrUndefined (StackId) }
```

<p>The output for a <a>CreateStack</a> action.</p>

##### Instances
``` purescript
Newtype CreateStackOutput _
```

#### `CreateStackSetInput`

``` purescript
newtype CreateStackSetInput
  = CreateStackSetInput { "StackSetName" :: StackSetName, "Description" :: NullOrUndefined (Description), "TemplateBody" :: NullOrUndefined (TemplateBody), "TemplateURL" :: NullOrUndefined (TemplateURL), "Parameters" :: NullOrUndefined (Parameters), "Capabilities" :: NullOrUndefined (Capabilities), "Tags" :: NullOrUndefined (Tags), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype CreateStackSetInput _
```

#### `CreateStackSetOutput`

``` purescript
newtype CreateStackSetOutput
  = CreateStackSetOutput { "StackSetId" :: NullOrUndefined (StackSetId) }
```

##### Instances
``` purescript
Newtype CreateStackSetOutput _
```

#### `CreatedButModifiedException`

``` purescript
newtype CreatedButModifiedException
  = CreatedButModifiedException {  }
```

<p>The specified resource exists, but has been changed.</p>

##### Instances
``` purescript
Newtype CreatedButModifiedException _
```

#### `CreationTime`

``` purescript
newtype CreationTime
  = CreationTime Number
```

##### Instances
``` purescript
Newtype CreationTime _
```

#### `DeleteChangeSetInput`

``` purescript
newtype DeleteChangeSetInput
  = DeleteChangeSetInput { "ChangeSetName" :: ChangeSetNameOrId, "StackName" :: NullOrUndefined (StackNameOrId) }
```

<p>The input for the <a>DeleteChangeSet</a> action.</p>

##### Instances
``` purescript
Newtype DeleteChangeSetInput _
```

#### `DeleteChangeSetOutput`

``` purescript
newtype DeleteChangeSetOutput
  = DeleteChangeSetOutput {  }
```

<p>The output for the <a>DeleteChangeSet</a> action.</p>

##### Instances
``` purescript
Newtype DeleteChangeSetOutput _
```

#### `DeleteStackInput`

``` purescript
newtype DeleteStackInput
  = DeleteStackInput { "StackName" :: StackName, "RetainResources" :: NullOrUndefined (RetainResources), "RoleARN" :: NullOrUndefined (RoleARN), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

<p>The input for <a>DeleteStack</a> action.</p>

##### Instances
``` purescript
Newtype DeleteStackInput _
```

#### `DeleteStackInstancesInput`

``` purescript
newtype DeleteStackInstancesInput
  = DeleteStackInstancesInput { "StackSetName" :: StackSetName, "Accounts" :: AccountList, "Regions" :: RegionList, "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences), "RetainStacks" :: RetainStacks, "OperationId" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype DeleteStackInstancesInput _
```

#### `DeleteStackInstancesOutput`

``` purescript
newtype DeleteStackInstancesOutput
  = DeleteStackInstancesOutput { "OperationId" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype DeleteStackInstancesOutput _
```

#### `DeleteStackSetInput`

``` purescript
newtype DeleteStackSetInput
  = DeleteStackSetInput { "StackSetName" :: StackSetName }
```

##### Instances
``` purescript
Newtype DeleteStackSetInput _
```

#### `DeleteStackSetOutput`

``` purescript
newtype DeleteStackSetOutput
  = DeleteStackSetOutput {  }
```

##### Instances
``` purescript
Newtype DeleteStackSetOutput _
```

#### `DeletionTime`

``` purescript
newtype DeletionTime
  = DeletionTime Number
```

##### Instances
``` purescript
Newtype DeletionTime _
```

#### `DescribeAccountLimitsInput`

``` purescript
newtype DescribeAccountLimitsInput
  = DescribeAccountLimitsInput { "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The input for the <a>DescribeAccountLimits</a> action.</p>

##### Instances
``` purescript
Newtype DescribeAccountLimitsInput _
```

#### `DescribeAccountLimitsOutput`

``` purescript
newtype DescribeAccountLimitsOutput
  = DescribeAccountLimitsOutput { "AccountLimits" :: NullOrUndefined (AccountLimitList), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The output for the <a>DescribeAccountLimits</a> action.</p>

##### Instances
``` purescript
Newtype DescribeAccountLimitsOutput _
```

#### `DescribeChangeSetInput`

``` purescript
newtype DescribeChangeSetInput
  = DescribeChangeSetInput { "ChangeSetName" :: ChangeSetNameOrId, "StackName" :: NullOrUndefined (StackNameOrId), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The input for the <a>DescribeChangeSet</a> action.</p>

##### Instances
``` purescript
Newtype DescribeChangeSetInput _
```

#### `DescribeChangeSetOutput`

``` purescript
newtype DescribeChangeSetOutput
  = DescribeChangeSetOutput { "ChangeSetName" :: NullOrUndefined (ChangeSetName), "ChangeSetId" :: NullOrUndefined (ChangeSetId), "StackId" :: NullOrUndefined (StackId), "StackName" :: NullOrUndefined (StackName), "Description" :: NullOrUndefined (Description), "Parameters" :: NullOrUndefined (Parameters), "CreationTime" :: NullOrUndefined (CreationTime), "ExecutionStatus" :: NullOrUndefined (ExecutionStatus), "Status" :: NullOrUndefined (ChangeSetStatus), "StatusReason" :: NullOrUndefined (ChangeSetStatusReason), "NotificationARNs" :: NullOrUndefined (NotificationARNs), "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration), "Capabilities" :: NullOrUndefined (Capabilities), "Tags" :: NullOrUndefined (Tags), "Changes" :: NullOrUndefined (Changes), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The output for the <a>DescribeChangeSet</a> action.</p>

##### Instances
``` purescript
Newtype DescribeChangeSetOutput _
```

#### `DescribeStackEventsInput`

``` purescript
newtype DescribeStackEventsInput
  = DescribeStackEventsInput { "StackName" :: NullOrUndefined (StackName), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The input for <a>DescribeStackEvents</a> action.</p>

##### Instances
``` purescript
Newtype DescribeStackEventsInput _
```

#### `DescribeStackEventsOutput`

``` purescript
newtype DescribeStackEventsOutput
  = DescribeStackEventsOutput { "StackEvents" :: NullOrUndefined (StackEvents), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The output for a <a>DescribeStackEvents</a> action.</p>

##### Instances
``` purescript
Newtype DescribeStackEventsOutput _
```

#### `DescribeStackInstanceInput`

``` purescript
newtype DescribeStackInstanceInput
  = DescribeStackInstanceInput { "StackSetName" :: StackSetName, "StackInstanceAccount" :: Account, "StackInstanceRegion" :: Region }
```

##### Instances
``` purescript
Newtype DescribeStackInstanceInput _
```

#### `DescribeStackInstanceOutput`

``` purescript
newtype DescribeStackInstanceOutput
  = DescribeStackInstanceOutput { "StackInstance" :: NullOrUndefined (StackInstance) }
```

##### Instances
``` purescript
Newtype DescribeStackInstanceOutput _
```

#### `DescribeStackResourceInput`

``` purescript
newtype DescribeStackResourceInput
  = DescribeStackResourceInput { "StackName" :: StackName, "LogicalResourceId" :: LogicalResourceId }
```

<p>The input for <a>DescribeStackResource</a> action.</p>

##### Instances
``` purescript
Newtype DescribeStackResourceInput _
```

#### `DescribeStackResourceOutput`

``` purescript
newtype DescribeStackResourceOutput
  = DescribeStackResourceOutput { "StackResourceDetail" :: NullOrUndefined (StackResourceDetail) }
```

<p>The output for a <a>DescribeStackResource</a> action.</p>

##### Instances
``` purescript
Newtype DescribeStackResourceOutput _
```

#### `DescribeStackResourcesInput`

``` purescript
newtype DescribeStackResourcesInput
  = DescribeStackResourcesInput { "StackName" :: NullOrUndefined (StackName), "LogicalResourceId" :: NullOrUndefined (LogicalResourceId), "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId) }
```

<p>The input for <a>DescribeStackResources</a> action.</p>

##### Instances
``` purescript
Newtype DescribeStackResourcesInput _
```

#### `DescribeStackResourcesOutput`

``` purescript
newtype DescribeStackResourcesOutput
  = DescribeStackResourcesOutput { "StackResources" :: NullOrUndefined (StackResources) }
```

<p>The output for a <a>DescribeStackResources</a> action.</p>

##### Instances
``` purescript
Newtype DescribeStackResourcesOutput _
```

#### `DescribeStackSetInput`

``` purescript
newtype DescribeStackSetInput
  = DescribeStackSetInput { "StackSetName" :: StackSetName }
```

##### Instances
``` purescript
Newtype DescribeStackSetInput _
```

#### `DescribeStackSetOperationInput`

``` purescript
newtype DescribeStackSetOperationInput
  = DescribeStackSetOperationInput { "StackSetName" :: StackSetName, "OperationId" :: ClientRequestToken }
```

##### Instances
``` purescript
Newtype DescribeStackSetOperationInput _
```

#### `DescribeStackSetOperationOutput`

``` purescript
newtype DescribeStackSetOperationOutput
  = DescribeStackSetOperationOutput { "StackSetOperation" :: NullOrUndefined (StackSetOperation) }
```

##### Instances
``` purescript
Newtype DescribeStackSetOperationOutput _
```

#### `DescribeStackSetOutput`

``` purescript
newtype DescribeStackSetOutput
  = DescribeStackSetOutput { "StackSet" :: NullOrUndefined (StackSet) }
```

##### Instances
``` purescript
Newtype DescribeStackSetOutput _
```

#### `DescribeStacksInput`

``` purescript
newtype DescribeStacksInput
  = DescribeStacksInput { "StackName" :: NullOrUndefined (StackName), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The input for <a>DescribeStacks</a> action.</p>

##### Instances
``` purescript
Newtype DescribeStacksInput _
```

#### `DescribeStacksOutput`

``` purescript
newtype DescribeStacksOutput
  = DescribeStacksOutput { "Stacks" :: NullOrUndefined (Stacks), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The output for a <a>DescribeStacks</a> action.</p>

##### Instances
``` purescript
Newtype DescribeStacksOutput _
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

##### Instances
``` purescript
Newtype Description _
```

#### `DisableRollback`

``` purescript
newtype DisableRollback
  = DisableRollback Boolean
```

##### Instances
``` purescript
Newtype DisableRollback _
```

#### `EnableTerminationProtection`

``` purescript
newtype EnableTerminationProtection
  = EnableTerminationProtection Boolean
```

##### Instances
``` purescript
Newtype EnableTerminationProtection _
```

#### `EstimateTemplateCostInput`

``` purescript
newtype EstimateTemplateCostInput
  = EstimateTemplateCostInput { "TemplateBody" :: NullOrUndefined (TemplateBody), "TemplateURL" :: NullOrUndefined (TemplateURL), "Parameters" :: NullOrUndefined (Parameters) }
```

<p>The input for an <a>EstimateTemplateCost</a> action.</p>

##### Instances
``` purescript
Newtype EstimateTemplateCostInput _
```

#### `EstimateTemplateCostOutput`

``` purescript
newtype EstimateTemplateCostOutput
  = EstimateTemplateCostOutput { "Url" :: NullOrUndefined (Url) }
```

<p>The output for a <a>EstimateTemplateCost</a> action.</p>

##### Instances
``` purescript
Newtype EstimateTemplateCostOutput _
```

#### `EvaluationType`

``` purescript
newtype EvaluationType
  = EvaluationType String
```

##### Instances
``` purescript
Newtype EvaluationType _
```

#### `EventId`

``` purescript
newtype EventId
  = EventId String
```

##### Instances
``` purescript
Newtype EventId _
```

#### `ExecuteChangeSetInput`

``` purescript
newtype ExecuteChangeSetInput
  = ExecuteChangeSetInput { "ChangeSetName" :: ChangeSetNameOrId, "StackName" :: NullOrUndefined (StackNameOrId), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

<p>The input for the <a>ExecuteChangeSet</a> action.</p>

##### Instances
``` purescript
Newtype ExecuteChangeSetInput _
```

#### `ExecuteChangeSetOutput`

``` purescript
newtype ExecuteChangeSetOutput
  = ExecuteChangeSetOutput {  }
```

<p>The output for the <a>ExecuteChangeSet</a> action.</p>

##### Instances
``` purescript
Newtype ExecuteChangeSetOutput _
```

#### `ExecutionStatus`

``` purescript
newtype ExecutionStatus
  = ExecutionStatus String
```

##### Instances
``` purescript
Newtype ExecutionStatus _
```

#### `Export`

``` purescript
newtype Export
  = Export { "ExportingStackId" :: NullOrUndefined (StackId), "Name" :: NullOrUndefined (ExportName), "Value" :: NullOrUndefined (ExportValue) }
```

<p>The <code>Export</code> structure describes the exported output values for a stack.</p>

##### Instances
``` purescript
Newtype Export _
```

#### `ExportName`

``` purescript
newtype ExportName
  = ExportName String
```

##### Instances
``` purescript
Newtype ExportName _
```

#### `ExportValue`

``` purescript
newtype ExportValue
  = ExportValue String
```

##### Instances
``` purescript
Newtype ExportValue _
```

#### `Exports`

``` purescript
newtype Exports
  = Exports (Array Export)
```

##### Instances
``` purescript
Newtype Exports _
```

#### `FailureToleranceCount`

``` purescript
newtype FailureToleranceCount
  = FailureToleranceCount Int
```

##### Instances
``` purescript
Newtype FailureToleranceCount _
```

#### `FailureTolerancePercentage`

``` purescript
newtype FailureTolerancePercentage
  = FailureTolerancePercentage Int
```

##### Instances
``` purescript
Newtype FailureTolerancePercentage _
```

#### `GetStackPolicyInput`

``` purescript
newtype GetStackPolicyInput
  = GetStackPolicyInput { "StackName" :: StackName }
```

<p>The input for the <a>GetStackPolicy</a> action.</p>

##### Instances
``` purescript
Newtype GetStackPolicyInput _
```

#### `GetStackPolicyOutput`

``` purescript
newtype GetStackPolicyOutput
  = GetStackPolicyOutput { "StackPolicyBody" :: NullOrUndefined (StackPolicyBody) }
```

<p>The output for the <a>GetStackPolicy</a> action.</p>

##### Instances
``` purescript
Newtype GetStackPolicyOutput _
```

#### `GetTemplateInput`

``` purescript
newtype GetTemplateInput
  = GetTemplateInput { "StackName" :: NullOrUndefined (StackName), "ChangeSetName" :: NullOrUndefined (ChangeSetNameOrId), "TemplateStage" :: NullOrUndefined (TemplateStage) }
```

<p>The input for a <a>GetTemplate</a> action.</p>

##### Instances
``` purescript
Newtype GetTemplateInput _
```

#### `GetTemplateOutput`

``` purescript
newtype GetTemplateOutput
  = GetTemplateOutput { "TemplateBody" :: NullOrUndefined (TemplateBody), "StagesAvailable" :: NullOrUndefined (StageList) }
```

<p>The output for <a>GetTemplate</a> action.</p>

##### Instances
``` purescript
Newtype GetTemplateOutput _
```

#### `GetTemplateSummaryInput`

``` purescript
newtype GetTemplateSummaryInput
  = GetTemplateSummaryInput { "TemplateBody" :: NullOrUndefined (TemplateBody), "TemplateURL" :: NullOrUndefined (TemplateURL), "StackName" :: NullOrUndefined (StackNameOrId), "StackSetName" :: NullOrUndefined (StackSetNameOrId) }
```

<p>The input for the <a>GetTemplateSummary</a> action.</p>

##### Instances
``` purescript
Newtype GetTemplateSummaryInput _
```

#### `GetTemplateSummaryOutput`

``` purescript
newtype GetTemplateSummaryOutput
  = GetTemplateSummaryOutput { "Parameters" :: NullOrUndefined (ParameterDeclarations), "Description" :: NullOrUndefined (Description), "Capabilities" :: NullOrUndefined (Capabilities), "CapabilitiesReason" :: NullOrUndefined (CapabilitiesReason), "ResourceTypes" :: NullOrUndefined (ResourceTypes), "Version" :: NullOrUndefined (Version), "Metadata" :: NullOrUndefined (Metadata), "DeclaredTransforms" :: NullOrUndefined (TransformsList) }
```

<p>The output for the <a>GetTemplateSummary</a> action.</p>

##### Instances
``` purescript
Newtype GetTemplateSummaryOutput _
```

#### `Imports`

``` purescript
newtype Imports
  = Imports (Array StackName)
```

##### Instances
``` purescript
Newtype Imports _
```

#### `InsufficientCapabilitiesException`

``` purescript
newtype InsufficientCapabilitiesException
  = InsufficientCapabilitiesException {  }
```

<p>The template contains resources with capabilities that weren't specified in the Capabilities parameter.</p>

##### Instances
``` purescript
Newtype InsufficientCapabilitiesException _
```

#### `InvalidChangeSetStatusException`

``` purescript
newtype InvalidChangeSetStatusException
  = InvalidChangeSetStatusException {  }
```

<p>The specified change set can't be used to update the stack. For example, the change set status might be <code>CREATE_IN_PROGRESS</code>, or the stack status might be <code>UPDATE_IN_PROGRESS</code>.</p>

##### Instances
``` purescript
Newtype InvalidChangeSetStatusException _
```

#### `InvalidOperationException`

``` purescript
newtype InvalidOperationException
  = InvalidOperationException {  }
```

<p>The specified operation isn't valid.</p>

##### Instances
``` purescript
Newtype InvalidOperationException _
```

#### `LastUpdatedTime`

``` purescript
newtype LastUpdatedTime
  = LastUpdatedTime Number
```

##### Instances
``` purescript
Newtype LastUpdatedTime _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>The quota for the resource has already been reached.</p> <p>For information on stack set limitations, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-limitations.html">Limitations of StackSets</a>.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `LimitName`

``` purescript
newtype LimitName
  = LimitName String
```

##### Instances
``` purescript
Newtype LimitName _
```

#### `LimitValue`

``` purescript
newtype LimitValue
  = LimitValue Int
```

##### Instances
``` purescript
Newtype LimitValue _
```

#### `ListChangeSetsInput`

``` purescript
newtype ListChangeSetsInput
  = ListChangeSetsInput { "StackName" :: StackNameOrId, "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The input for the <a>ListChangeSets</a> action.</p>

##### Instances
``` purescript
Newtype ListChangeSetsInput _
```

#### `ListChangeSetsOutput`

``` purescript
newtype ListChangeSetsOutput
  = ListChangeSetsOutput { "Summaries" :: NullOrUndefined (ChangeSetSummaries), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The output for the <a>ListChangeSets</a> action.</p>

##### Instances
``` purescript
Newtype ListChangeSetsOutput _
```

#### `ListExportsInput`

``` purescript
newtype ListExportsInput
  = ListExportsInput { "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListExportsInput _
```

#### `ListExportsOutput`

``` purescript
newtype ListExportsOutput
  = ListExportsOutput { "Exports" :: NullOrUndefined (Exports), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListExportsOutput _
```

#### `ListImportsInput`

``` purescript
newtype ListImportsInput
  = ListImportsInput { "ExportName" :: ExportName, "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListImportsInput _
```

#### `ListImportsOutput`

``` purescript
newtype ListImportsOutput
  = ListImportsOutput { "Imports" :: NullOrUndefined (Imports), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListImportsOutput _
```

#### `ListStackInstancesInput`

``` purescript
newtype ListStackInstancesInput
  = ListStackInstancesInput { "StackSetName" :: StackSetName, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "StackInstanceAccount" :: NullOrUndefined (Account), "StackInstanceRegion" :: NullOrUndefined (Region) }
```

##### Instances
``` purescript
Newtype ListStackInstancesInput _
```

#### `ListStackInstancesOutput`

``` purescript
newtype ListStackInstancesOutput
  = ListStackInstancesOutput { "Summaries" :: NullOrUndefined (StackInstanceSummaries), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListStackInstancesOutput _
```

#### `ListStackResourcesInput`

``` purescript
newtype ListStackResourcesInput
  = ListStackResourcesInput { "StackName" :: StackName, "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The input for the <a>ListStackResource</a> action.</p>

##### Instances
``` purescript
Newtype ListStackResourcesInput _
```

#### `ListStackResourcesOutput`

``` purescript
newtype ListStackResourcesOutput
  = ListStackResourcesOutput { "StackResourceSummaries" :: NullOrUndefined (StackResourceSummaries), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The output for a <a>ListStackResources</a> action.</p>

##### Instances
``` purescript
Newtype ListStackResourcesOutput _
```

#### `ListStackSetOperationResultsInput`

``` purescript
newtype ListStackSetOperationResultsInput
  = ListStackSetOperationResultsInput { "StackSetName" :: StackSetName, "OperationId" :: ClientRequestToken, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListStackSetOperationResultsInput _
```

#### `ListStackSetOperationResultsOutput`

``` purescript
newtype ListStackSetOperationResultsOutput
  = ListStackSetOperationResultsOutput { "Summaries" :: NullOrUndefined (StackSetOperationResultSummaries), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListStackSetOperationResultsOutput _
```

#### `ListStackSetOperationsInput`

``` purescript
newtype ListStackSetOperationsInput
  = ListStackSetOperationsInput { "StackSetName" :: StackSetName, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListStackSetOperationsInput _
```

#### `ListStackSetOperationsOutput`

``` purescript
newtype ListStackSetOperationsOutput
  = ListStackSetOperationsOutput { "Summaries" :: NullOrUndefined (StackSetOperationSummaries), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListStackSetOperationsOutput _
```

#### `ListStackSetsInput`

``` purescript
newtype ListStackSetsInput
  = ListStackSetsInput { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Status" :: NullOrUndefined (StackSetStatus) }
```

##### Instances
``` purescript
Newtype ListStackSetsInput _
```

#### `ListStackSetsOutput`

``` purescript
newtype ListStackSetsOutput
  = ListStackSetsOutput { "Summaries" :: NullOrUndefined (StackSetSummaries), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListStackSetsOutput _
```

#### `ListStacksInput`

``` purescript
newtype ListStacksInput
  = ListStacksInput { "NextToken" :: NullOrUndefined (NextToken), "StackStatusFilter" :: NullOrUndefined (StackStatusFilter) }
```

<p>The input for <a>ListStacks</a> action.</p>

##### Instances
``` purescript
Newtype ListStacksInput _
```

#### `ListStacksOutput`

``` purescript
newtype ListStacksOutput
  = ListStacksOutput { "StackSummaries" :: NullOrUndefined (StackSummaries), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The output for <a>ListStacks</a> action.</p>

##### Instances
``` purescript
Newtype ListStacksOutput _
```

#### `LogicalResourceId`

``` purescript
newtype LogicalResourceId
  = LogicalResourceId String
```

##### Instances
``` purescript
Newtype LogicalResourceId _
```

#### `MaxConcurrentCount`

``` purescript
newtype MaxConcurrentCount
  = MaxConcurrentCount Int
```

##### Instances
``` purescript
Newtype MaxConcurrentCount _
```

#### `MaxConcurrentPercentage`

``` purescript
newtype MaxConcurrentPercentage
  = MaxConcurrentPercentage Int
```

##### Instances
``` purescript
Newtype MaxConcurrentPercentage _
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

#### `Metadata`

``` purescript
newtype Metadata
  = Metadata String
```

##### Instances
``` purescript
Newtype Metadata _
```

#### `MonitoringTimeInMinutes`

``` purescript
newtype MonitoringTimeInMinutes
  = MonitoringTimeInMinutes Int
```

##### Instances
``` purescript
Newtype MonitoringTimeInMinutes _
```

#### `NameAlreadyExistsException`

``` purescript
newtype NameAlreadyExistsException
  = NameAlreadyExistsException {  }
```

<p>The specified name is already in use.</p>

##### Instances
``` purescript
Newtype NameAlreadyExistsException _
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

#### `NoEcho`

``` purescript
newtype NoEcho
  = NoEcho Boolean
```

##### Instances
``` purescript
Newtype NoEcho _
```

#### `NotificationARN`

``` purescript
newtype NotificationARN
  = NotificationARN String
```

##### Instances
``` purescript
Newtype NotificationARN _
```

#### `NotificationARNs`

``` purescript
newtype NotificationARNs
  = NotificationARNs (Array NotificationARN)
```

##### Instances
``` purescript
Newtype NotificationARNs _
```

#### `OnFailure`

``` purescript
newtype OnFailure
  = OnFailure String
```

##### Instances
``` purescript
Newtype OnFailure _
```

#### `OperationIdAlreadyExistsException`

``` purescript
newtype OperationIdAlreadyExistsException
  = OperationIdAlreadyExistsException {  }
```

<p>The specified operation ID already exists.</p>

##### Instances
``` purescript
Newtype OperationIdAlreadyExistsException _
```

#### `OperationInProgressException`

``` purescript
newtype OperationInProgressException
  = OperationInProgressException {  }
```

<p>Another operation is currently in progress for this stack set. Only one operation can be performed for a stack set at a given time.</p>

##### Instances
``` purescript
Newtype OperationInProgressException _
```

#### `OperationNotFoundException`

``` purescript
newtype OperationNotFoundException
  = OperationNotFoundException {  }
```

<p>The specified ID refers to an operation that doesn't exist.</p>

##### Instances
``` purescript
Newtype OperationNotFoundException _
```

#### `Output`

``` purescript
newtype Output
  = Output { "OutputKey" :: NullOrUndefined (OutputKey), "OutputValue" :: NullOrUndefined (OutputValue), "Description" :: NullOrUndefined (Description), "ExportName" :: NullOrUndefined (ExportName) }
```

<p>The Output data type.</p>

##### Instances
``` purescript
Newtype Output _
```

#### `OutputKey`

``` purescript
newtype OutputKey
  = OutputKey String
```

##### Instances
``` purescript
Newtype OutputKey _
```

#### `OutputValue`

``` purescript
newtype OutputValue
  = OutputValue String
```

##### Instances
``` purescript
Newtype OutputValue _
```

#### `Outputs`

``` purescript
newtype Outputs
  = Outputs (Array Output)
```

##### Instances
``` purescript
Newtype Outputs _
```

#### `Parameter`

``` purescript
newtype Parameter
  = Parameter { "ParameterKey" :: NullOrUndefined (ParameterKey), "ParameterValue" :: NullOrUndefined (ParameterValue), "UsePreviousValue" :: NullOrUndefined (UsePreviousValue), "ResolvedValue" :: NullOrUndefined (ParameterValue) }
```

<p>The Parameter data type.</p>

##### Instances
``` purescript
Newtype Parameter _
```

#### `ParameterConstraints`

``` purescript
newtype ParameterConstraints
  = ParameterConstraints { "AllowedValues" :: NullOrUndefined (AllowedValues) }
```

<p>A set of criteria that AWS CloudFormation uses to validate parameter values. Although other constraints might be defined in the stack template, AWS CloudFormation returns only the <code>AllowedValues</code> property.</p>

##### Instances
``` purescript
Newtype ParameterConstraints _
```

#### `ParameterDeclaration`

``` purescript
newtype ParameterDeclaration
  = ParameterDeclaration { "ParameterKey" :: NullOrUndefined (ParameterKey), "DefaultValue" :: NullOrUndefined (ParameterValue), "ParameterType" :: NullOrUndefined (ParameterType), "NoEcho" :: NullOrUndefined (NoEcho), "Description" :: NullOrUndefined (Description), "ParameterConstraints" :: NullOrUndefined (ParameterConstraints) }
```

<p>The ParameterDeclaration data type.</p>

##### Instances
``` purescript
Newtype ParameterDeclaration _
```

#### `ParameterDeclarations`

``` purescript
newtype ParameterDeclarations
  = ParameterDeclarations (Array ParameterDeclaration)
```

##### Instances
``` purescript
Newtype ParameterDeclarations _
```

#### `ParameterKey`

``` purescript
newtype ParameterKey
  = ParameterKey String
```

##### Instances
``` purescript
Newtype ParameterKey _
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

#### `Parameters`

``` purescript
newtype Parameters
  = Parameters (Array Parameter)
```

##### Instances
``` purescript
Newtype Parameters _
```

#### `PhysicalResourceId`

``` purescript
newtype PhysicalResourceId
  = PhysicalResourceId String
```

##### Instances
``` purescript
Newtype PhysicalResourceId _
```

#### `PropertyName`

``` purescript
newtype PropertyName
  = PropertyName String
```

##### Instances
``` purescript
Newtype PropertyName _
```

#### `Reason`

``` purescript
newtype Reason
  = Reason String
```

##### Instances
``` purescript
Newtype Reason _
```

#### `Region`

``` purescript
newtype Region
  = Region String
```

##### Instances
``` purescript
Newtype Region _
```

#### `RegionList`

``` purescript
newtype RegionList
  = RegionList (Array Region)
```

##### Instances
``` purescript
Newtype RegionList _
```

#### `Replacement`

``` purescript
newtype Replacement
  = Replacement String
```

##### Instances
``` purescript
Newtype Replacement _
```

#### `RequiresRecreation`

``` purescript
newtype RequiresRecreation
  = RequiresRecreation String
```

##### Instances
``` purescript
Newtype RequiresRecreation _
```

#### `ResourceAttribute`

``` purescript
newtype ResourceAttribute
  = ResourceAttribute String
```

##### Instances
``` purescript
Newtype ResourceAttribute _
```

#### `ResourceChange`

``` purescript
newtype ResourceChange
  = ResourceChange { "Action" :: NullOrUndefined (ChangeAction), "LogicalResourceId" :: NullOrUndefined (LogicalResourceId), "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId), "ResourceType" :: NullOrUndefined (ResourceType), "Replacement" :: NullOrUndefined (Replacement), "Scope" :: NullOrUndefined (Scope), "Details" :: NullOrUndefined (ResourceChangeDetails) }
```

<p>The <code>ResourceChange</code> structure describes the resource and the action that AWS CloudFormation will perform on it if you execute this change set.</p>

##### Instances
``` purescript
Newtype ResourceChange _
```

#### `ResourceChangeDetail`

``` purescript
newtype ResourceChangeDetail
  = ResourceChangeDetail { "Target" :: NullOrUndefined (ResourceTargetDefinition), "Evaluation" :: NullOrUndefined (EvaluationType), "ChangeSource" :: NullOrUndefined (ChangeSource), "CausingEntity" :: NullOrUndefined (CausingEntity) }
```

<p>For a resource with <code>Modify</code> as the action, the <code>ResourceChange</code> structure describes the changes AWS CloudFormation will make to that resource.</p>

##### Instances
``` purescript
Newtype ResourceChangeDetail _
```

#### `ResourceChangeDetails`

``` purescript
newtype ResourceChangeDetails
  = ResourceChangeDetails (Array ResourceChangeDetail)
```

##### Instances
``` purescript
Newtype ResourceChangeDetails _
```

#### `ResourceProperties`

``` purescript
newtype ResourceProperties
  = ResourceProperties String
```

##### Instances
``` purescript
Newtype ResourceProperties _
```

#### `ResourceSignalStatus`

``` purescript
newtype ResourceSignalStatus
  = ResourceSignalStatus String
```

##### Instances
``` purescript
Newtype ResourceSignalStatus _
```

#### `ResourceSignalUniqueId`

``` purescript
newtype ResourceSignalUniqueId
  = ResourceSignalUniqueId String
```

##### Instances
``` purescript
Newtype ResourceSignalUniqueId _
```

#### `ResourceStatus`

``` purescript
newtype ResourceStatus
  = ResourceStatus String
```

##### Instances
``` purescript
Newtype ResourceStatus _
```

#### `ResourceStatusReason`

``` purescript
newtype ResourceStatusReason
  = ResourceStatusReason String
```

##### Instances
``` purescript
Newtype ResourceStatusReason _
```

#### `ResourceTargetDefinition`

``` purescript
newtype ResourceTargetDefinition
  = ResourceTargetDefinition { "Attribute" :: NullOrUndefined (ResourceAttribute), "Name" :: NullOrUndefined (PropertyName), "RequiresRecreation" :: NullOrUndefined (RequiresRecreation) }
```

<p>The field that AWS CloudFormation will change, such as the name of a resource's property, and whether the resource will be recreated.</p>

##### Instances
``` purescript
Newtype ResourceTargetDefinition _
```

#### `ResourceToSkip`

``` purescript
newtype ResourceToSkip
  = ResourceToSkip String
```

##### Instances
``` purescript
Newtype ResourceToSkip _
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

#### `ResourceTypes`

``` purescript
newtype ResourceTypes
  = ResourceTypes (Array ResourceType)
```

##### Instances
``` purescript
Newtype ResourceTypes _
```

#### `ResourcesToSkip`

``` purescript
newtype ResourcesToSkip
  = ResourcesToSkip (Array ResourceToSkip)
```

##### Instances
``` purescript
Newtype ResourcesToSkip _
```

#### `RetainResources`

``` purescript
newtype RetainResources
  = RetainResources (Array LogicalResourceId)
```

##### Instances
``` purescript
Newtype RetainResources _
```

#### `RetainStacks`

``` purescript
newtype RetainStacks
  = RetainStacks Boolean
```

##### Instances
``` purescript
Newtype RetainStacks _
```

#### `RetainStacksNullable`

``` purescript
newtype RetainStacksNullable
  = RetainStacksNullable Boolean
```

##### Instances
``` purescript
Newtype RetainStacksNullable _
```

#### `RoleARN`

``` purescript
newtype RoleARN
  = RoleARN String
```

##### Instances
``` purescript
Newtype RoleARN _
```

#### `RollbackConfiguration`

``` purescript
newtype RollbackConfiguration
  = RollbackConfiguration { "RollbackTriggers" :: NullOrUndefined (RollbackTriggers), "MonitoringTimeInMinutes" :: NullOrUndefined (MonitoringTimeInMinutes) }
```

<p>Structure containing the rollback triggers for AWS CloudFormation to monitor during stack creation and updating operations, and for the specified monitoring period afterwards.</p> <p>Rollback triggers enable you to have AWS CloudFormation monitor the state of your application during stack creation and updating, and to roll back that operation if the application breaches the threshold of any of the alarms you've specified. For each rollback trigger you create, you specify the Cloudwatch alarm that CloudFormation should monitor. CloudFormation monitors the specified alarms during the stack create or update operation, and for the specified amount of time after all resources have been deployed. If any of the alarms goes to ALERT state during the stack operation or the monitoring period, CloudFormation rolls back the entire stack operation. If the monitoring period expires without any alarms going to ALERT state, CloudFormation proceeds to dispose of old resources as usual.</p> <p>By default, CloudFormation only rolls back stack operations if an alarm goes to ALERT state, not INSUFFICIENT_DATA state. To have CloudFormation roll back the stack operation if an alarm goes to INSUFFICIENT_DATA state as well, edit the CloudWatch alarm to treat missing data as <code>breaching</code>. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/AlarmThatSendsEmail.html">Configuring How CloudWatch Alarms Treats Missing Data</a>.</p> <p>AWS CloudFormation does not monitor rollback triggers when it rolls back a stack during an update operation.</p>

##### Instances
``` purescript
Newtype RollbackConfiguration _
```

#### `RollbackTrigger`

``` purescript
newtype RollbackTrigger
  = RollbackTrigger { "Arn" :: Arn, "Type" :: Type }
```

<p>A rollback trigger AWS CloudFormation monitors during creation and updating of stacks. If any of the alarms you specify goes to ALERT state during the stack operation or within the specified monitoring period afterwards, CloudFormation rolls back the entire stack operation. </p>

##### Instances
``` purescript
Newtype RollbackTrigger _
```

#### `RollbackTriggers`

``` purescript
newtype RollbackTriggers
  = RollbackTriggers (Array RollbackTrigger)
```

##### Instances
``` purescript
Newtype RollbackTriggers _
```

#### `Scope`

``` purescript
newtype Scope
  = Scope (Array ResourceAttribute)
```

##### Instances
``` purescript
Newtype Scope _
```

#### `SetStackPolicyInput`

``` purescript
newtype SetStackPolicyInput
  = SetStackPolicyInput { "StackName" :: StackName, "StackPolicyBody" :: NullOrUndefined (StackPolicyBody), "StackPolicyURL" :: NullOrUndefined (StackPolicyURL) }
```

<p>The input for the <a>SetStackPolicy</a> action.</p>

##### Instances
``` purescript
Newtype SetStackPolicyInput _
```

#### `SignalResourceInput`

``` purescript
newtype SignalResourceInput
  = SignalResourceInput { "StackName" :: StackNameOrId, "LogicalResourceId" :: LogicalResourceId, "UniqueId" :: ResourceSignalUniqueId, "Status" :: ResourceSignalStatus }
```

<p>The input for the <a>SignalResource</a> action.</p>

##### Instances
``` purescript
Newtype SignalResourceInput _
```

#### `Stack`

``` purescript
newtype Stack
  = Stack { "StackId" :: NullOrUndefined (StackId), "StackName" :: StackName, "ChangeSetId" :: NullOrUndefined (ChangeSetId), "Description" :: NullOrUndefined (Description), "Parameters" :: NullOrUndefined (Parameters), "CreationTime" :: CreationTime, "DeletionTime" :: NullOrUndefined (DeletionTime), "LastUpdatedTime" :: NullOrUndefined (LastUpdatedTime), "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration), "StackStatus" :: StackStatus, "StackStatusReason" :: NullOrUndefined (StackStatusReason), "DisableRollback" :: NullOrUndefined (DisableRollback), "NotificationARNs" :: NullOrUndefined (NotificationARNs), "TimeoutInMinutes" :: NullOrUndefined (TimeoutMinutes), "Capabilities" :: NullOrUndefined (Capabilities), "Outputs" :: NullOrUndefined (Outputs), "RoleARN" :: NullOrUndefined (RoleARN), "Tags" :: NullOrUndefined (Tags), "EnableTerminationProtection" :: NullOrUndefined (EnableTerminationProtection), "ParentId" :: NullOrUndefined (StackId), "RootId" :: NullOrUndefined (StackId) }
```

<p>The Stack data type.</p>

##### Instances
``` purescript
Newtype Stack _
```

#### `StackEvent`

``` purescript
newtype StackEvent
  = StackEvent { "StackId" :: StackId, "EventId" :: EventId, "StackName" :: StackName, "LogicalResourceId" :: NullOrUndefined (LogicalResourceId), "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId), "ResourceType" :: NullOrUndefined (ResourceType), "Number" :: Number, "ResourceStatus" :: NullOrUndefined (ResourceStatus), "ResourceStatusReason" :: NullOrUndefined (ResourceStatusReason), "ResourceProperties" :: NullOrUndefined (ResourceProperties), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

<p>The StackEvent data type.</p>

##### Instances
``` purescript
Newtype StackEvent _
```

#### `StackEvents`

``` purescript
newtype StackEvents
  = StackEvents (Array StackEvent)
```

##### Instances
``` purescript
Newtype StackEvents _
```

#### `StackId`

``` purescript
newtype StackId
  = StackId String
```

##### Instances
``` purescript
Newtype StackId _
```

#### `StackInstance`

``` purescript
newtype StackInstance
  = StackInstance { "StackSetId" :: NullOrUndefined (StackSetId), "Region" :: NullOrUndefined (Region), "Account" :: NullOrUndefined (Account), "StackId" :: NullOrUndefined (StackId), "ParameterOverrides" :: NullOrUndefined (Parameters), "Status" :: NullOrUndefined (StackInstanceStatus), "StatusReason" :: NullOrUndefined (Reason) }
```

<p>An AWS CloudFormation stack, in a specific account and region, that's part of a stack set operation. A stack instance is a reference to an attempted or actual stack in a given account within a given region. A stack instance can exist without a stack—for example, if the stack couldn't be created for some reason. A stack instance is associated with only one stack set. Each stack instance contains the ID of its associated stack set, as well as the ID of the actual stack and the stack status.</p>

##### Instances
``` purescript
Newtype StackInstance _
```

#### `StackInstanceNotFoundException`

``` purescript
newtype StackInstanceNotFoundException
  = StackInstanceNotFoundException {  }
```

<p>The specified stack instance doesn't exist.</p>

##### Instances
``` purescript
Newtype StackInstanceNotFoundException _
```

#### `StackInstanceStatus`

``` purescript
newtype StackInstanceStatus
  = StackInstanceStatus String
```

##### Instances
``` purescript
Newtype StackInstanceStatus _
```

#### `StackInstanceSummaries`

``` purescript
newtype StackInstanceSummaries
  = StackInstanceSummaries (Array StackInstanceSummary)
```

##### Instances
``` purescript
Newtype StackInstanceSummaries _
```

#### `StackInstanceSummary`

``` purescript
newtype StackInstanceSummary
  = StackInstanceSummary { "StackSetId" :: NullOrUndefined (StackSetId), "Region" :: NullOrUndefined (Region), "Account" :: NullOrUndefined (Account), "StackId" :: NullOrUndefined (StackId), "Status" :: NullOrUndefined (StackInstanceStatus), "StatusReason" :: NullOrUndefined (Reason) }
```

<p>The structure that contains summary information about a stack instance.</p>

##### Instances
``` purescript
Newtype StackInstanceSummary _
```

#### `StackName`

``` purescript
newtype StackName
  = StackName String
```

##### Instances
``` purescript
Newtype StackName _
```

#### `StackNameOrId`

``` purescript
newtype StackNameOrId
  = StackNameOrId String
```

##### Instances
``` purescript
Newtype StackNameOrId _
```

#### `StackPolicyBody`

``` purescript
newtype StackPolicyBody
  = StackPolicyBody String
```

##### Instances
``` purescript
Newtype StackPolicyBody _
```

#### `StackPolicyDuringUpdateBody`

``` purescript
newtype StackPolicyDuringUpdateBody
  = StackPolicyDuringUpdateBody String
```

##### Instances
``` purescript
Newtype StackPolicyDuringUpdateBody _
```

#### `StackPolicyDuringUpdateURL`

``` purescript
newtype StackPolicyDuringUpdateURL
  = StackPolicyDuringUpdateURL String
```

##### Instances
``` purescript
Newtype StackPolicyDuringUpdateURL _
```

#### `StackPolicyURL`

``` purescript
newtype StackPolicyURL
  = StackPolicyURL String
```

##### Instances
``` purescript
Newtype StackPolicyURL _
```

#### `StackResource`

``` purescript
newtype StackResource
  = StackResource { "StackName" :: NullOrUndefined (StackName), "StackId" :: NullOrUndefined (StackId), "LogicalResourceId" :: LogicalResourceId, "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId), "ResourceType" :: ResourceType, "Number" :: Number, "ResourceStatus" :: ResourceStatus, "ResourceStatusReason" :: NullOrUndefined (ResourceStatusReason), "Description" :: NullOrUndefined (Description) }
```

<p>The StackResource data type.</p>

##### Instances
``` purescript
Newtype StackResource _
```

#### `StackResourceDetail`

``` purescript
newtype StackResourceDetail
  = StackResourceDetail { "StackName" :: NullOrUndefined (StackName), "StackId" :: NullOrUndefined (StackId), "LogicalResourceId" :: LogicalResourceId, "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId), "ResourceType" :: ResourceType, "LastUpdatedTimestamp" :: Number, "ResourceStatus" :: ResourceStatus, "ResourceStatusReason" :: NullOrUndefined (ResourceStatusReason), "Description" :: NullOrUndefined (Description), "Metadata" :: NullOrUndefined (Metadata) }
```

<p>Contains detailed information about the specified stack resource.</p>

##### Instances
``` purescript
Newtype StackResourceDetail _
```

#### `StackResourceSummaries`

``` purescript
newtype StackResourceSummaries
  = StackResourceSummaries (Array StackResourceSummary)
```

##### Instances
``` purescript
Newtype StackResourceSummaries _
```

#### `StackResourceSummary`

``` purescript
newtype StackResourceSummary
  = StackResourceSummary { "LogicalResourceId" :: LogicalResourceId, "PhysicalResourceId" :: NullOrUndefined (PhysicalResourceId), "ResourceType" :: ResourceType, "LastUpdatedTimestamp" :: Number, "ResourceStatus" :: ResourceStatus, "ResourceStatusReason" :: NullOrUndefined (ResourceStatusReason) }
```

<p>Contains high-level information about the specified stack resource.</p>

##### Instances
``` purescript
Newtype StackResourceSummary _
```

#### `StackResources`

``` purescript
newtype StackResources
  = StackResources (Array StackResource)
```

##### Instances
``` purescript
Newtype StackResources _
```

#### `StackSet`

``` purescript
newtype StackSet
  = StackSet { "StackSetName" :: NullOrUndefined (StackSetName), "StackSetId" :: NullOrUndefined (StackSetId), "Description" :: NullOrUndefined (Description), "Status" :: NullOrUndefined (StackSetStatus), "TemplateBody" :: NullOrUndefined (TemplateBody), "Parameters" :: NullOrUndefined (Parameters), "Capabilities" :: NullOrUndefined (Capabilities), "Tags" :: NullOrUndefined (Tags) }
```

<p>A structure that contains information about a stack set. A stack set enables you to provision stacks into AWS accounts and across regions by using a single CloudFormation template. In the stack set, you specify the template to use, as well as any parameters and capabilities that the template requires. </p>

##### Instances
``` purescript
Newtype StackSet _
```

#### `StackSetId`

``` purescript
newtype StackSetId
  = StackSetId String
```

##### Instances
``` purescript
Newtype StackSetId _
```

#### `StackSetName`

``` purescript
newtype StackSetName
  = StackSetName String
```

##### Instances
``` purescript
Newtype StackSetName _
```

#### `StackSetNameOrId`

``` purescript
newtype StackSetNameOrId
  = StackSetNameOrId String
```

##### Instances
``` purescript
Newtype StackSetNameOrId _
```

#### `StackSetNotEmptyException`

``` purescript
newtype StackSetNotEmptyException
  = StackSetNotEmptyException {  }
```

<p>You can't yet delete this stack set, because it still contains one or more stack instances. Delete all stack instances from the stack set before deleting the stack set.</p>

##### Instances
``` purescript
Newtype StackSetNotEmptyException _
```

#### `StackSetNotFoundException`

``` purescript
newtype StackSetNotFoundException
  = StackSetNotFoundException {  }
```

<p>The specified stack set doesn't exist.</p>

##### Instances
``` purescript
Newtype StackSetNotFoundException _
```

#### `StackSetOperation`

``` purescript
newtype StackSetOperation
  = StackSetOperation { "OperationId" :: NullOrUndefined (ClientRequestToken), "StackSetId" :: NullOrUndefined (StackSetId), "Action" :: NullOrUndefined (StackSetOperationAction), "Status" :: NullOrUndefined (StackSetOperationStatus), "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences), "RetainStacks" :: NullOrUndefined (RetainStacksNullable), "CreationTimestamp" :: NullOrUndefined (Number), "EndTimestamp" :: NullOrUndefined (Number) }
```

<p>The structure that contains information about a stack set operation. </p>

##### Instances
``` purescript
Newtype StackSetOperation _
```

#### `StackSetOperationAction`

``` purescript
newtype StackSetOperationAction
  = StackSetOperationAction String
```

##### Instances
``` purescript
Newtype StackSetOperationAction _
```

#### `StackSetOperationPreferences`

``` purescript
newtype StackSetOperationPreferences
  = StackSetOperationPreferences { "RegionOrder" :: NullOrUndefined (RegionList), "FailureToleranceCount" :: NullOrUndefined (FailureToleranceCount), "FailureTolerancePercentage" :: NullOrUndefined (FailureTolerancePercentage), "MaxConcurrentCount" :: NullOrUndefined (MaxConcurrentCount), "MaxConcurrentPercentage" :: NullOrUndefined (MaxConcurrentPercentage) }
```

<p>The user-specified preferences for how AWS CloudFormation performs a stack set operation. </p> <p>For more information on maximum concurrent accounts and failure tolerance, see <a href="http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-ops-options">Stack set operation options</a>.</p>

##### Instances
``` purescript
Newtype StackSetOperationPreferences _
```

#### `StackSetOperationResultStatus`

``` purescript
newtype StackSetOperationResultStatus
  = StackSetOperationResultStatus String
```

##### Instances
``` purescript
Newtype StackSetOperationResultStatus _
```

#### `StackSetOperationResultSummaries`

``` purescript
newtype StackSetOperationResultSummaries
  = StackSetOperationResultSummaries (Array StackSetOperationResultSummary)
```

##### Instances
``` purescript
Newtype StackSetOperationResultSummaries _
```

#### `StackSetOperationResultSummary`

``` purescript
newtype StackSetOperationResultSummary
  = StackSetOperationResultSummary { "Account" :: NullOrUndefined (Account), "Region" :: NullOrUndefined (Region), "Status" :: NullOrUndefined (StackSetOperationResultStatus), "StatusReason" :: NullOrUndefined (Reason), "AccountGateResult" :: NullOrUndefined (AccountGateResult) }
```

<p>The structure that contains information about a specified operation's results for a given account in a given region.</p>

##### Instances
``` purescript
Newtype StackSetOperationResultSummary _
```

#### `StackSetOperationStatus`

``` purescript
newtype StackSetOperationStatus
  = StackSetOperationStatus String
```

##### Instances
``` purescript
Newtype StackSetOperationStatus _
```

#### `StackSetOperationSummaries`

``` purescript
newtype StackSetOperationSummaries
  = StackSetOperationSummaries (Array StackSetOperationSummary)
```

##### Instances
``` purescript
Newtype StackSetOperationSummaries _
```

#### `StackSetOperationSummary`

``` purescript
newtype StackSetOperationSummary
  = StackSetOperationSummary { "OperationId" :: NullOrUndefined (ClientRequestToken), "Action" :: NullOrUndefined (StackSetOperationAction), "Status" :: NullOrUndefined (StackSetOperationStatus), "CreationTimestamp" :: NullOrUndefined (Number), "EndTimestamp" :: NullOrUndefined (Number) }
```

<p>The structures that contain summary information about the specified operation.</p>

##### Instances
``` purescript
Newtype StackSetOperationSummary _
```

#### `StackSetStatus`

``` purescript
newtype StackSetStatus
  = StackSetStatus String
```

##### Instances
``` purescript
Newtype StackSetStatus _
```

#### `StackSetSummaries`

``` purescript
newtype StackSetSummaries
  = StackSetSummaries (Array StackSetSummary)
```

##### Instances
``` purescript
Newtype StackSetSummaries _
```

#### `StackSetSummary`

``` purescript
newtype StackSetSummary
  = StackSetSummary { "StackSetName" :: NullOrUndefined (StackSetName), "StackSetId" :: NullOrUndefined (StackSetId), "Description" :: NullOrUndefined (Description), "Status" :: NullOrUndefined (StackSetStatus) }
```

<p>The structures that contain summary information about the specified stack set.</p>

##### Instances
``` purescript
Newtype StackSetSummary _
```

#### `StackStatus`

``` purescript
newtype StackStatus
  = StackStatus String
```

##### Instances
``` purescript
Newtype StackStatus _
```

#### `StackStatusFilter`

``` purescript
newtype StackStatusFilter
  = StackStatusFilter (Array StackStatus)
```

##### Instances
``` purescript
Newtype StackStatusFilter _
```

#### `StackStatusReason`

``` purescript
newtype StackStatusReason
  = StackStatusReason String
```

##### Instances
``` purescript
Newtype StackStatusReason _
```

#### `StackSummaries`

``` purescript
newtype StackSummaries
  = StackSummaries (Array StackSummary)
```

##### Instances
``` purescript
Newtype StackSummaries _
```

#### `StackSummary`

``` purescript
newtype StackSummary
  = StackSummary { "StackId" :: NullOrUndefined (StackId), "StackName" :: StackName, "TemplateDescription" :: NullOrUndefined (TemplateDescription), "CreationTime" :: CreationTime, "LastUpdatedTime" :: NullOrUndefined (LastUpdatedTime), "DeletionTime" :: NullOrUndefined (DeletionTime), "StackStatus" :: StackStatus, "StackStatusReason" :: NullOrUndefined (StackStatusReason), "ParentId" :: NullOrUndefined (StackId), "RootId" :: NullOrUndefined (StackId) }
```

<p>The StackSummary Data Type</p>

##### Instances
``` purescript
Newtype StackSummary _
```

#### `Stacks`

``` purescript
newtype Stacks
  = Stacks (Array Stack)
```

##### Instances
``` purescript
Newtype Stacks _
```

#### `StageList`

``` purescript
newtype StageList
  = StageList (Array TemplateStage)
```

##### Instances
``` purescript
Newtype StageList _
```

#### `StaleRequestException`

``` purescript
newtype StaleRequestException
  = StaleRequestException {  }
```

<p>Another operation has been performed on this stack set since the specified operation was performed. </p>

##### Instances
``` purescript
Newtype StaleRequestException _
```

#### `StopStackSetOperationInput`

``` purescript
newtype StopStackSetOperationInput
  = StopStackSetOperationInput { "StackSetName" :: StackSetName, "OperationId" :: ClientRequestToken }
```

##### Instances
``` purescript
Newtype StopStackSetOperationInput _
```

#### `StopStackSetOperationOutput`

``` purescript
newtype StopStackSetOperationOutput
  = StopStackSetOperationOutput {  }
```

##### Instances
``` purescript
Newtype StopStackSetOperationOutput _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>The Tag type enables you to specify a key-value pair that can be used to store information about an AWS CloudFormation stack.</p>

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

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Array Tag)
```

##### Instances
``` purescript
Newtype Tags _
```

#### `TemplateBody`

``` purescript
newtype TemplateBody
  = TemplateBody String
```

##### Instances
``` purescript
Newtype TemplateBody _
```

#### `TemplateDescription`

``` purescript
newtype TemplateDescription
  = TemplateDescription String
```

##### Instances
``` purescript
Newtype TemplateDescription _
```

#### `TemplateParameter`

``` purescript
newtype TemplateParameter
  = TemplateParameter { "ParameterKey" :: NullOrUndefined (ParameterKey), "DefaultValue" :: NullOrUndefined (ParameterValue), "NoEcho" :: NullOrUndefined (NoEcho), "Description" :: NullOrUndefined (Description) }
```

<p>The TemplateParameter data type.</p>

##### Instances
``` purescript
Newtype TemplateParameter _
```

#### `TemplateParameters`

``` purescript
newtype TemplateParameters
  = TemplateParameters (Array TemplateParameter)
```

##### Instances
``` purescript
Newtype TemplateParameters _
```

#### `TemplateStage`

``` purescript
newtype TemplateStage
  = TemplateStage String
```

##### Instances
``` purescript
Newtype TemplateStage _
```

#### `TemplateURL`

``` purescript
newtype TemplateURL
  = TemplateURL String
```

##### Instances
``` purescript
Newtype TemplateURL _
```

#### `TimeoutMinutes`

``` purescript
newtype TimeoutMinutes
  = TimeoutMinutes Int
```

##### Instances
``` purescript
Newtype TimeoutMinutes _
```

#### `TokenAlreadyExistsException`

``` purescript
newtype TokenAlreadyExistsException
  = TokenAlreadyExistsException {  }
```

<p>A client request token already exists.</p>

##### Instances
``` purescript
Newtype TokenAlreadyExistsException _
```

#### `TransformName`

``` purescript
newtype TransformName
  = TransformName String
```

##### Instances
``` purescript
Newtype TransformName _
```

#### `TransformsList`

``` purescript
newtype TransformsList
  = TransformsList (Array TransformName)
```

##### Instances
``` purescript
Newtype TransformsList _
```

#### `Type`

``` purescript
newtype Type
  = Type String
```

##### Instances
``` purescript
Newtype Type _
```

#### `UpdateStackInput`

``` purescript
newtype UpdateStackInput
  = UpdateStackInput { "StackName" :: StackName, "TemplateBody" :: NullOrUndefined (TemplateBody), "TemplateURL" :: NullOrUndefined (TemplateURL), "UsePreviousTemplate" :: NullOrUndefined (UsePreviousTemplate), "StackPolicyDuringUpdateBody" :: NullOrUndefined (StackPolicyDuringUpdateBody), "StackPolicyDuringUpdateURL" :: NullOrUndefined (StackPolicyDuringUpdateURL), "Parameters" :: NullOrUndefined (Parameters), "Capabilities" :: NullOrUndefined (Capabilities), "ResourceTypes" :: NullOrUndefined (ResourceTypes), "RoleARN" :: NullOrUndefined (RoleARN), "RollbackConfiguration" :: NullOrUndefined (RollbackConfiguration), "StackPolicyBody" :: NullOrUndefined (StackPolicyBody), "StackPolicyURL" :: NullOrUndefined (StackPolicyURL), "NotificationARNs" :: NullOrUndefined (NotificationARNs), "Tags" :: NullOrUndefined (Tags), "ClientRequestToken" :: NullOrUndefined (ClientRequestToken) }
```

<p>The input for an <a>UpdateStack</a> action.</p>

##### Instances
``` purescript
Newtype UpdateStackInput _
```

#### `UpdateStackInstancesInput`

``` purescript
newtype UpdateStackInstancesInput
  = UpdateStackInstancesInput { "StackSetName" :: StackSetName, "Accounts" :: AccountList, "Regions" :: RegionList, "ParameterOverrides" :: NullOrUndefined (Parameters), "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences), "OperationId" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype UpdateStackInstancesInput _
```

#### `UpdateStackInstancesOutput`

``` purescript
newtype UpdateStackInstancesOutput
  = UpdateStackInstancesOutput { "OperationId" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype UpdateStackInstancesOutput _
```

#### `UpdateStackOutput`

``` purescript
newtype UpdateStackOutput
  = UpdateStackOutput { "StackId" :: NullOrUndefined (StackId) }
```

<p>The output for an <a>UpdateStack</a> action.</p>

##### Instances
``` purescript
Newtype UpdateStackOutput _
```

#### `UpdateStackSetInput`

``` purescript
newtype UpdateStackSetInput
  = UpdateStackSetInput { "StackSetName" :: StackSetName, "Description" :: NullOrUndefined (Description), "TemplateBody" :: NullOrUndefined (TemplateBody), "TemplateURL" :: NullOrUndefined (TemplateURL), "UsePreviousTemplate" :: NullOrUndefined (UsePreviousTemplate), "Parameters" :: NullOrUndefined (Parameters), "Capabilities" :: NullOrUndefined (Capabilities), "Tags" :: NullOrUndefined (Tags), "OperationPreferences" :: NullOrUndefined (StackSetOperationPreferences), "OperationId" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype UpdateStackSetInput _
```

#### `UpdateStackSetOutput`

``` purescript
newtype UpdateStackSetOutput
  = UpdateStackSetOutput { "OperationId" :: NullOrUndefined (ClientRequestToken) }
```

##### Instances
``` purescript
Newtype UpdateStackSetOutput _
```

#### `UpdateTerminationProtectionInput`

``` purescript
newtype UpdateTerminationProtectionInput
  = UpdateTerminationProtectionInput { "EnableTerminationProtection" :: EnableTerminationProtection, "StackName" :: StackNameOrId }
```

##### Instances
``` purescript
Newtype UpdateTerminationProtectionInput _
```

#### `UpdateTerminationProtectionOutput`

``` purescript
newtype UpdateTerminationProtectionOutput
  = UpdateTerminationProtectionOutput { "StackId" :: NullOrUndefined (StackId) }
```

##### Instances
``` purescript
Newtype UpdateTerminationProtectionOutput _
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

#### `UsePreviousTemplate`

``` purescript
newtype UsePreviousTemplate
  = UsePreviousTemplate Boolean
```

##### Instances
``` purescript
Newtype UsePreviousTemplate _
```

#### `UsePreviousValue`

``` purescript
newtype UsePreviousValue
  = UsePreviousValue Boolean
```

##### Instances
``` purescript
Newtype UsePreviousValue _
```

#### `ValidateTemplateInput`

``` purescript
newtype ValidateTemplateInput
  = ValidateTemplateInput { "TemplateBody" :: NullOrUndefined (TemplateBody), "TemplateURL" :: NullOrUndefined (TemplateURL) }
```

<p>The input for <a>ValidateTemplate</a> action.</p>

##### Instances
``` purescript
Newtype ValidateTemplateInput _
```

#### `ValidateTemplateOutput`

``` purescript
newtype ValidateTemplateOutput
  = ValidateTemplateOutput { "Parameters" :: NullOrUndefined (TemplateParameters), "Description" :: NullOrUndefined (Description), "Capabilities" :: NullOrUndefined (Capabilities), "CapabilitiesReason" :: NullOrUndefined (CapabilitiesReason), "DeclaredTransforms" :: NullOrUndefined (TransformsList) }
```

<p>The output for <a>ValidateTemplate</a> action.</p>

##### Instances
``` purescript
Newtype ValidateTemplateOutput _
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


