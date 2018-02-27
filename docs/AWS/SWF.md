## Module AWS.SWF

<fullname>Amazon Simple Workflow Service</fullname> <p>The Amazon Simple Workflow Service (Amazon SWF) makes it easy to build applications that use Amazon's cloud to coordinate work across distributed components. In Amazon SWF, a <i>task</i> represents a logical unit of work that is performed by a component of your workflow. Coordinating tasks in a workflow involves managing intertask dependencies, scheduling, and concurrency in accordance with the logical flow of the application.</p> <p>Amazon SWF gives you full control over implementing tasks and coordinating them without worrying about underlying complexities such as tracking their progress and maintaining their state.</p> <p>This documentation serves as reference only. For a broader overview of the Amazon SWF programming model, see the <i> <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/">Amazon SWF Developer Guide</a> </i>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `countClosedWorkflowExecutions`

``` purescript
countClosedWorkflowExecutions :: forall eff. CountClosedWorkflowExecutionsInput -> Aff (err :: RequestError | eff) WorkflowExecutionCount
```

<p>Returns the number of closed workflow executions within the given domain that meet the specified filtering criteria.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `countOpenWorkflowExecutions`

``` purescript
countOpenWorkflowExecutions :: forall eff. CountOpenWorkflowExecutionsInput -> Aff (err :: RequestError | eff) WorkflowExecutionCount
```

<p>Returns the number of open workflow executions within the given domain that meet the specified filtering criteria.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `countPendingActivityTasks`

``` purescript
countPendingActivityTasks :: forall eff. CountPendingActivityTasksInput -> Aff (err :: RequestError | eff) PendingTaskCount
```

<p>Returns the estimated number of activity tasks in the specified task list. The count returned is an approximation and isn't guaranteed to be exact. If you specify a task list that no activity task was ever scheduled in then <code>0</code> is returned.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `countPendingDecisionTasks`

``` purescript
countPendingDecisionTasks :: forall eff. CountPendingDecisionTasksInput -> Aff (err :: RequestError | eff) PendingTaskCount
```

<p>Returns the estimated number of decision tasks in the specified task list. The count returned is an approximation and isn't guaranteed to be exact. If you specify a task list that no decision task was ever scheduled in then <code>0</code> is returned.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `deprecateActivityType`

``` purescript
deprecateActivityType :: forall eff. DeprecateActivityTypeInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deprecates the specified <i>activity type</i>. After an activity type has been deprecated, you cannot create new tasks of that activity type. Tasks of this type that were scheduled before the type was deprecated continue to run.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>activityType.name</code>: String constraint. The key is <code>swf:activityType.name</code>.</p> </li> <li> <p> <code>activityType.version</code>: String constraint. The key is <code>swf:activityType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `deprecateDomain`

``` purescript
deprecateDomain :: forall eff. DeprecateDomainInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deprecates the specified domain. After a domain has been deprecated it cannot be used to create new workflow executions or register new types. However, you can still use visibility actions on this domain. Deprecating a domain also deprecates all activity and workflow types registered in the domain. Executions that were started before the domain was deprecated continues to run.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `deprecateWorkflowType`

``` purescript
deprecateWorkflowType :: forall eff. DeprecateWorkflowTypeInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deprecates the specified <i>workflow type</i>. After a workflow type has been deprecated, you cannot create new executions of that type. Executions that were started before the type was deprecated continues to run. A deprecated workflow type may still be used when calling visibility actions.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>workflowType.name</code>: String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code>: String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `describeActivityType`

``` purescript
describeActivityType :: forall eff. DescribeActivityTypeInput -> Aff (err :: RequestError | eff) ActivityTypeDetail
```

<p>Returns information about the specified activity type. This includes configuration settings provided when the type was registered and other general information about the type.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>activityType.name</code>: String constraint. The key is <code>swf:activityType.name</code>.</p> </li> <li> <p> <code>activityType.version</code>: String constraint. The key is <code>swf:activityType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `describeDomain`

``` purescript
describeDomain :: forall eff. DescribeDomainInput -> Aff (err :: RequestError | eff) DomainDetail
```

<p>Returns information about the specified domain, including description and status.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `describeWorkflowExecution`

``` purescript
describeWorkflowExecution :: forall eff. DescribeWorkflowExecutionInput -> Aff (err :: RequestError | eff) WorkflowExecutionDetail
```

<p>Returns information about the specified workflow execution including its type and some statistics.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `describeWorkflowType`

``` purescript
describeWorkflowType :: forall eff. DescribeWorkflowTypeInput -> Aff (err :: RequestError | eff) WorkflowTypeDetail
```

<p>Returns information about the specified <i>workflow type</i>. This includes configuration settings specified when the type was registered and other information such as creation date, current status, etc.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>workflowType.name</code>: String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code>: String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `getWorkflowExecutionHistory`

``` purescript
getWorkflowExecutionHistory :: forall eff. GetWorkflowExecutionHistoryInput -> Aff (err :: RequestError | eff) History
```

<p>Returns the history of the specified workflow execution. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the <code>nextPageToken</code> returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `listActivityTypes`

``` purescript
listActivityTypes :: forall eff. ListActivityTypesInput -> Aff (err :: RequestError | eff) ActivityTypeInfos
```

<p>Returns information about all activities registered in the specified domain that match the specified name and registration status. The result includes information like creation date, current status of the activity, etc. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the <code>nextPageToken</code> returned by the initial call.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `listClosedWorkflowExecutions`

``` purescript
listClosedWorkflowExecutions :: forall eff. ListClosedWorkflowExecutionsInput -> Aff (err :: RequestError | eff) WorkflowExecutionInfos
```

<p>Returns a list of closed workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `listDomains`

``` purescript
listDomains :: forall eff. ListDomainsInput -> Aff (err :: RequestError | eff) DomainInfos
```

<p>Returns the list of domains registered in the account. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains. The element must be set to <code>arn:aws:swf::AccountID:domain/*</code>, where <i>AccountID</i> is the account ID, with no dashes.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `listOpenWorkflowExecutions`

``` purescript
listOpenWorkflowExecutions :: forall eff. ListOpenWorkflowExecutionsInput -> Aff (err :: RequestError | eff) WorkflowExecutionInfos
```

<p>Returns a list of open workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `listWorkflowTypes`

``` purescript
listWorkflowTypes :: forall eff. ListWorkflowTypesInput -> Aff (err :: RequestError | eff) WorkflowTypeInfos
```

<p>Returns information about workflow types in the specified domain. The results may be split into multiple pages that can be retrieved by making the call repeatedly.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `pollForActivityTask`

``` purescript
pollForActivityTask :: forall eff. PollForActivityTaskInput -> Aff (err :: RequestError | eff) ActivityTask
```

<p>Used by workers to get an <a>ActivityTask</a> from the specified activity <code>taskList</code>. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available. The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns an empty result. An empty result, in this context, means that an ActivityTask is returned, but that the value of taskToken is an empty string. If a task is returned, the worker should use its type to identify and process it correctly.</p> <important> <p>Workers should set their client side socket timeout to at least 70 seconds (10 seconds higher than the maximum time service may hold the poll request).</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `pollForDecisionTask`

``` purescript
pollForDecisionTask :: forall eff. PollForDecisionTaskInput -> Aff (err :: RequestError | eff) DecisionTask
```

<p>Used by deciders to get a <a>DecisionTask</a> from the specified decision <code>taskList</code>. A decision task may be returned for any open workflow execution that is using the specified task list. The task includes a paginated view of the history of the workflow execution. The decider should use the workflow type and the history to determine how to properly handle the task.</p> <p>This action initiates a long poll, where the service holds the HTTP connection open and responds as soon a task becomes available. If no decision task is available in the specified task list before the timeout of 60 seconds expires, an empty result is returned. An empty result, in this context, means that a DecisionTask is returned, but that the value of taskToken is an empty string.</p> <important> <p>Deciders should set their client side socket timeout to at least 70 seconds (10 seconds higher than the timeout).</p> </important> <important> <p>Because the number of workflow history events for a single workflow execution might be very large, the result returned might be split up across a number of pages. To retrieve subsequent pages, make additional calls to <code>PollForDecisionTask</code> using the <code>nextPageToken</code> returned by the initial call. Note that you do <i>not</i> call <code>GetWorkflowExecutionHistory</code> with this <code>nextPageToken</code>. Instead, call <code>PollForDecisionTask</code> again.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `recordActivityTaskHeartbeat`

``` purescript
recordActivityTaskHeartbeat :: forall eff. RecordActivityTaskHeartbeatInput -> Aff (err :: RequestError | eff) ActivityTaskStatus
```

<p>Used by activity workers to report to the service that the <a>ActivityTask</a> represented by the specified <code>taskToken</code> is still making progress. The worker can also specify details of the progress, for example percent complete, using the <code>details</code> parameter. This action can also be used by the worker as a mechanism to check if cancellation is being requested for the activity task. If a cancellation is being attempted for the specified task, then the boolean <code>cancelRequested</code> flag returned by the service is set to <code>true</code>.</p> <p>This action resets the <code>taskHeartbeatTimeout</code> clock. The <code>taskHeartbeatTimeout</code> is specified in <a>RegisterActivityType</a>.</p> <p>This action doesn't in itself create an event in the workflow execution history. However, if the task times out, the workflow execution history contains a <code>ActivityTaskTimedOut</code> event that contains the information from the last heartbeat generated by the activity worker.</p> <note> <p>The <code>taskStartToCloseTimeout</code> of an activity type is the maximum duration of an activity task, regardless of the number of <a>RecordActivityTaskHeartbeat</a> requests received. The <code>taskStartToCloseTimeout</code> is also specified in <a>RegisterActivityType</a>.</p> </note> <note> <p>This operation is only useful for long-lived activities to report liveliness of the task and to determine if a cancellation is being attempted.</p> </note> <important> <p>If the <code>cancelRequested</code> flag returns <code>true</code>, a cancellation is being attempted. If the worker can cancel the activity, it should respond with <a>RespondActivityTaskCanceled</a>. Otherwise, it should ignore the cancellation request.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `registerActivityType`

``` purescript
registerActivityType :: forall eff. RegisterActivityTypeInput -> Aff (err :: RequestError | eff) Unit
```

<p>Registers a new <i>activity type</i> along with its configuration settings in the specified domain.</p> <important> <p>A <code>TypeAlreadyExists</code> fault is returned if the type already exists in the domain. You cannot change any configuration settings of the type after its registration, and it must be registered as a new version.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>defaultTaskList.name</code>: String constraint. The key is <code>swf:defaultTaskList.name</code>.</p> </li> <li> <p> <code>name</code>: String constraint. The key is <code>swf:name</code>.</p> </li> <li> <p> <code>version</code>: String constraint. The key is <code>swf:version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `registerDomain`

``` purescript
registerDomain :: forall eff. RegisterDomainInput -> Aff (err :: RequestError | eff) Unit
```

<p>Registers a new domain.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>You cannot use an IAM policy to control domain access for this action. The name of the domain being registered is available as the resource of this action.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `registerWorkflowType`

``` purescript
registerWorkflowType :: forall eff. RegisterWorkflowTypeInput -> Aff (err :: RequestError | eff) Unit
```

<p>Registers a new <i>workflow type</i> and its configuration settings in the specified domain.</p> <p>The retention period for the workflow history is set by the <a>RegisterDomain</a> action.</p> <important> <p>If the type already exists, then a <code>TypeAlreadyExists</code> fault is returned. You cannot change the configuration settings of a workflow type once it is registered and it must be registered as a new version.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>defaultTaskList.name</code>: String constraint. The key is <code>swf:defaultTaskList.name</code>.</p> </li> <li> <p> <code>name</code>: String constraint. The key is <code>swf:name</code>.</p> </li> <li> <p> <code>version</code>: String constraint. The key is <code>swf:version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `requestCancelWorkflowExecution`

``` purescript
requestCancelWorkflowExecution :: forall eff. RequestCancelWorkflowExecutionInput -> Aff (err :: RequestError | eff) Unit
```

<p>Records a <code>WorkflowExecutionCancelRequested</code> event in the currently running workflow execution identified by the given domain, workflowId, and runId. This logically requests the cancellation of the workflow execution as a whole. It is up to the decider to take appropriate actions when it receives an execution history with this event.</p> <note> <p>If the runId isn't specified, the <code>WorkflowExecutionCancelRequested</code> event is recorded in the history of the current open workflow execution with the specified workflowId in the domain.</p> </note> <note> <p>Because this action allows the workflow to properly clean up and gracefully close, it should be used instead of <a>TerminateWorkflowExecution</a> when possible.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `respondActivityTaskCanceled`

``` purescript
respondActivityTaskCanceled :: forall eff. RespondActivityTaskCanceledInput -> Aff (err :: RequestError | eff) Unit
```

<p>Used by workers to tell the service that the <a>ActivityTask</a> identified by the <code>taskToken</code> was successfully canceled. Additional <code>details</code> can be provided using the <code>details</code> argument.</p> <p>These <code>details</code> (if provided) appear in the <code>ActivityTaskCanceled</code> event added to the workflow history.</p> <important> <p>Only use this operation if the <code>canceled</code> flag of a <a>RecordActivityTaskHeartbeat</a> request returns <code>true</code> and if the activity can be safely undone or abandoned.</p> </important> <p>A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to <a>RespondActivityTaskCompleted</a>, RespondActivityTaskCanceled, <a>RespondActivityTaskFailed</a>, or the task has <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types">timed out</a>.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `respondActivityTaskCompleted`

``` purescript
respondActivityTaskCompleted :: forall eff. RespondActivityTaskCompletedInput -> Aff (err :: RequestError | eff) Unit
```

<p>Used by workers to tell the service that the <a>ActivityTask</a> identified by the <code>taskToken</code> completed successfully with a <code>result</code> (if provided). The <code>result</code> appears in the <code>ActivityTaskCompleted</code> event in the workflow history.</p> <important> <p>If the requested task doesn't complete successfully, use <a>RespondActivityTaskFailed</a> instead. If the worker finds that the task is canceled through the <code>canceled</code> flag returned by <a>RecordActivityTaskHeartbeat</a>, it should cancel the task, clean up and then call <a>RespondActivityTaskCanceled</a>.</p> </important> <p>A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to RespondActivityTaskCompleted, <a>RespondActivityTaskCanceled</a>, <a>RespondActivityTaskFailed</a>, or the task has <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types">timed out</a>.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `respondActivityTaskFailed`

``` purescript
respondActivityTaskFailed :: forall eff. RespondActivityTaskFailedInput -> Aff (err :: RequestError | eff) Unit
```

<p>Used by workers to tell the service that the <a>ActivityTask</a> identified by the <code>taskToken</code> has failed with <code>reason</code> (if specified). The <code>reason</code> and <code>details</code> appear in the <code>ActivityTaskFailed</code> event added to the workflow history.</p> <p>A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to <a>RespondActivityTaskCompleted</a>, <a>RespondActivityTaskCanceled</a>, RespondActivityTaskFailed, or the task has <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types">timed out</a>.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `respondDecisionTaskCompleted`

``` purescript
respondDecisionTaskCompleted :: forall eff. RespondDecisionTaskCompletedInput -> Aff (err :: RequestError | eff) Unit
```

<p>Used by deciders to tell the service that the <a>DecisionTask</a> identified by the <code>taskToken</code> has successfully completed. The <code>decisions</code> argument specifies the list of decisions made while processing the task.</p> <p>A <code>DecisionTaskCompleted</code> event is added to the workflow history. The <code>executionContext</code> specified is attached to the event in the workflow execution history.</p> <p> <b>Access Control</b> </p> <p>If an IAM policy grants permission to use <code>RespondDecisionTaskCompleted</code>, it can express permissions for the list of decisions in the <code>decisions</code> parameter. Each of the decisions has one or more parameters, much like a regular API call. To allow for policies to be as readable as possible, you can express permissions on decisions as if they were actual API calls, including applying conditions to some parameters. For more information, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `signalWorkflowExecution`

``` purescript
signalWorkflowExecution :: forall eff. SignalWorkflowExecutionInput -> Aff (err :: RequestError | eff) Unit
```

<p>Records a <code>WorkflowExecutionSignaled</code> event in the workflow execution history and creates a decision task for the workflow execution identified by the given domain, workflowId and runId. The event is recorded with the specified user defined signalName and input (if provided).</p> <note> <p>If a runId isn't specified, then the <code>WorkflowExecutionSignaled</code> event is recorded in the history of the current open workflow with the matching workflowId in the domain.</p> </note> <note> <p>If the specified workflow execution isn't open, this method fails with <code>UnknownResource</code>.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `startWorkflowExecution`

``` purescript
startWorkflowExecution :: forall eff. StartWorkflowExecutionInput -> Aff (err :: RequestError | eff) Run
```

<p>Starts an execution of the workflow type in the specified domain using the provided <code>workflowId</code> and input data.</p> <p>This action returns the newly started workflow execution.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagList.member.0</code>: The key is <code>swf:tagList.member.0</code>.</p> </li> <li> <p> <code>tagList.member.1</code>: The key is <code>swf:tagList.member.1</code>.</p> </li> <li> <p> <code>tagList.member.2</code>: The key is <code>swf:tagList.member.2</code>.</p> </li> <li> <p> <code>tagList.member.3</code>: The key is <code>swf:tagList.member.3</code>.</p> </li> <li> <p> <code>tagList.member.4</code>: The key is <code>swf:tagList.member.4</code>.</p> </li> <li> <p> <code>taskList</code>: String constraint. The key is <code>swf:taskList.name</code>.</p> </li> <li> <p> <code>workflowType.name</code>: String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code>: String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `terminateWorkflowExecution`

``` purescript
terminateWorkflowExecution :: forall eff. TerminateWorkflowExecutionInput -> Aff (err :: RequestError | eff) Unit
```

<p>Records a <code>WorkflowExecutionTerminated</code> event and forces closure of the workflow execution identified by the given domain, runId, and workflowId. The child policy, registered with the workflow type or specified when starting this execution, is applied to any open child workflow executions of this workflow execution.</p> <important> <p>If the identified workflow execution was in progress, it is terminated immediately.</p> </important> <note> <p>If a runId isn't specified, then the <code>WorkflowExecutionTerminated</code> event is recorded in the history of the current open workflow with the matching workflowId in the domain.</p> </note> <note> <p>You should consider using <a>RequestCancelWorkflowExecution</a> action instead because it allows the workflow to gracefully close while <a>TerminateWorkflowExecution</a> doesn't.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

#### `ActivityId`

``` purescript
newtype ActivityId
  = ActivityId String
```

##### Instances
``` purescript
Newtype ActivityId _
```

#### `ActivityTask`

``` purescript
newtype ActivityTask
  = ActivityTask { "TaskToken'" :: TaskToken, "ActivityId'" :: ActivityId, "StartedEventId'" :: EventId, "WorkflowExecution'" :: WorkflowExecution, "ActivityType'" :: ActivityType, "Input'" :: NullOrUndefined (Data) }
```

<p>Unit of work sent to an activity worker.</p>

##### Instances
``` purescript
Newtype ActivityTask _
```

#### `ActivityTaskCancelRequestedEventAttributes`

``` purescript
newtype ActivityTaskCancelRequestedEventAttributes
  = ActivityTaskCancelRequestedEventAttributes { "DecisionTaskCompletedEventId'" :: EventId, "ActivityId'" :: ActivityId }
```

<p>Provides the details of the <code>ActivityTaskCancelRequested</code> event.</p>

##### Instances
``` purescript
Newtype ActivityTaskCancelRequestedEventAttributes _
```

#### `ActivityTaskCanceledEventAttributes`

``` purescript
newtype ActivityTaskCanceledEventAttributes
  = ActivityTaskCanceledEventAttributes { "Details'" :: NullOrUndefined (Data), "ScheduledEventId'" :: EventId, "StartedEventId'" :: EventId, "LatestCancelRequestedEventId'" :: NullOrUndefined (EventId) }
```

<p>Provides the details of the <code>ActivityTaskCanceled</code> event.</p>

##### Instances
``` purescript
Newtype ActivityTaskCanceledEventAttributes _
```

#### `ActivityTaskCompletedEventAttributes`

``` purescript
newtype ActivityTaskCompletedEventAttributes
  = ActivityTaskCompletedEventAttributes { "Result'" :: NullOrUndefined (Data), "ScheduledEventId'" :: EventId, "StartedEventId'" :: EventId }
```

<p>Provides the details of the <code>ActivityTaskCompleted</code> event.</p>

##### Instances
``` purescript
Newtype ActivityTaskCompletedEventAttributes _
```

#### `ActivityTaskFailedEventAttributes`

``` purescript
newtype ActivityTaskFailedEventAttributes
  = ActivityTaskFailedEventAttributes { "Reason'" :: NullOrUndefined (FailureReason), "Details'" :: NullOrUndefined (Data), "ScheduledEventId'" :: EventId, "StartedEventId'" :: EventId }
```

<p>Provides the details of the <code>ActivityTaskFailed</code> event.</p>

##### Instances
``` purescript
Newtype ActivityTaskFailedEventAttributes _
```

#### `ActivityTaskScheduledEventAttributes`

``` purescript
newtype ActivityTaskScheduledEventAttributes
  = ActivityTaskScheduledEventAttributes { "ActivityType'" :: ActivityType, "ActivityId'" :: ActivityId, "Input'" :: NullOrUndefined (Data), "Control'" :: NullOrUndefined (Data), "ScheduleToStartTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "ScheduleToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "TaskList'" :: TaskList, "TaskPriority'" :: NullOrUndefined (TaskPriority), "DecisionTaskCompletedEventId'" :: EventId, "HeartbeatTimeout'" :: NullOrUndefined (DurationInSecondsOptional) }
```

<p>Provides the details of the <code>ActivityTaskScheduled</code> event.</p>

##### Instances
``` purescript
Newtype ActivityTaskScheduledEventAttributes _
```

#### `ActivityTaskStartedEventAttributes`

``` purescript
newtype ActivityTaskStartedEventAttributes
  = ActivityTaskStartedEventAttributes { "Identity'" :: NullOrUndefined (Identity), "ScheduledEventId'" :: EventId }
```

<p>Provides the details of the <code>ActivityTaskStarted</code> event.</p>

##### Instances
``` purescript
Newtype ActivityTaskStartedEventAttributes _
```

#### `ActivityTaskStatus`

``` purescript
newtype ActivityTaskStatus
  = ActivityTaskStatus { "CancelRequested'" :: Canceled }
```

<p>Status information about an activity task.</p>

##### Instances
``` purescript
Newtype ActivityTaskStatus _
```

#### `ActivityTaskTimedOutEventAttributes`

``` purescript
newtype ActivityTaskTimedOutEventAttributes
  = ActivityTaskTimedOutEventAttributes { "TimeoutType'" :: ActivityTaskTimeoutType, "ScheduledEventId'" :: EventId, "StartedEventId'" :: EventId, "Details'" :: NullOrUndefined (LimitedData) }
```

<p>Provides the details of the <code>ActivityTaskTimedOut</code> event.</p>

##### Instances
``` purescript
Newtype ActivityTaskTimedOutEventAttributes _
```

#### `ActivityTaskTimeoutType`

``` purescript
newtype ActivityTaskTimeoutType
  = ActivityTaskTimeoutType String
```

##### Instances
``` purescript
Newtype ActivityTaskTimeoutType _
```

#### `ActivityType`

``` purescript
newtype ActivityType
  = ActivityType { "Name'" :: Name, "Version'" :: Version }
```

<p>Represents an activity type.</p>

##### Instances
``` purescript
Newtype ActivityType _
```

#### `ActivityTypeConfiguration`

``` purescript
newtype ActivityTypeConfiguration
  = ActivityTypeConfiguration { "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultTaskHeartbeatTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultTaskList'" :: NullOrUndefined (TaskList), "DefaultTaskPriority'" :: NullOrUndefined (TaskPriority), "DefaultTaskScheduleToStartTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultTaskScheduleToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional) }
```

<p>Configuration settings registered with the activity type.</p>

##### Instances
``` purescript
Newtype ActivityTypeConfiguration _
```

#### `ActivityTypeDetail`

``` purescript
newtype ActivityTypeDetail
  = ActivityTypeDetail { "TypeInfo'" :: ActivityTypeInfo, "Configuration'" :: ActivityTypeConfiguration }
```

<p>Detailed information about an activity type.</p>

##### Instances
``` purescript
Newtype ActivityTypeDetail _
```

#### `ActivityTypeInfo`

``` purescript
newtype ActivityTypeInfo
  = ActivityTypeInfo { "ActivityType'" :: ActivityType, "Status'" :: RegistrationStatus, "Description'" :: NullOrUndefined (Description), "CreationDate'" :: Number, "DeprecationDate'" :: NullOrUndefined (Number) }
```

<p>Detailed information about an activity type.</p>

##### Instances
``` purescript
Newtype ActivityTypeInfo _
```

#### `ActivityTypeInfoList`

``` purescript
newtype ActivityTypeInfoList
  = ActivityTypeInfoList (Array ActivityTypeInfo)
```

##### Instances
``` purescript
Newtype ActivityTypeInfoList _
```

#### `ActivityTypeInfos`

``` purescript
newtype ActivityTypeInfos
  = ActivityTypeInfos { "TypeInfos'" :: ActivityTypeInfoList, "NextPageToken'" :: NullOrUndefined (PageToken) }
```

<p>Contains a paginated list of activity type information structures.</p>

##### Instances
``` purescript
Newtype ActivityTypeInfos _
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

#### `CancelTimerDecisionAttributes`

``` purescript
newtype CancelTimerDecisionAttributes
  = CancelTimerDecisionAttributes { "TimerId'" :: TimerId }
```

<p>Provides the details of the <code>CancelTimer</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype CancelTimerDecisionAttributes _
```

#### `CancelTimerFailedCause`

``` purescript
newtype CancelTimerFailedCause
  = CancelTimerFailedCause String
```

##### Instances
``` purescript
Newtype CancelTimerFailedCause _
```

#### `CancelTimerFailedEventAttributes`

``` purescript
newtype CancelTimerFailedEventAttributes
  = CancelTimerFailedEventAttributes { "TimerId'" :: TimerId, "Cause'" :: CancelTimerFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>CancelTimerFailed</code> event.</p>

##### Instances
``` purescript
Newtype CancelTimerFailedEventAttributes _
```

#### `CancelWorkflowExecutionDecisionAttributes`

``` purescript
newtype CancelWorkflowExecutionDecisionAttributes
  = CancelWorkflowExecutionDecisionAttributes { "Details'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>CancelWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype CancelWorkflowExecutionDecisionAttributes _
```

#### `CancelWorkflowExecutionFailedCause`

``` purescript
newtype CancelWorkflowExecutionFailedCause
  = CancelWorkflowExecutionFailedCause String
```

##### Instances
``` purescript
Newtype CancelWorkflowExecutionFailedCause _
```

#### `CancelWorkflowExecutionFailedEventAttributes`

``` purescript
newtype CancelWorkflowExecutionFailedEventAttributes
  = CancelWorkflowExecutionFailedEventAttributes { "Cause'" :: CancelWorkflowExecutionFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>CancelWorkflowExecutionFailed</code> event.</p>

##### Instances
``` purescript
Newtype CancelWorkflowExecutionFailedEventAttributes _
```

#### `Canceled`

``` purescript
newtype Canceled
  = Canceled Boolean
```

##### Instances
``` purescript
Newtype Canceled _
```

#### `CauseMessage`

``` purescript
newtype CauseMessage
  = CauseMessage String
```

##### Instances
``` purescript
Newtype CauseMessage _
```

#### `ChildPolicy`

``` purescript
newtype ChildPolicy
  = ChildPolicy String
```

##### Instances
``` purescript
Newtype ChildPolicy _
```

#### `ChildWorkflowExecutionCanceledEventAttributes`

``` purescript
newtype ChildWorkflowExecutionCanceledEventAttributes
  = ChildWorkflowExecutionCanceledEventAttributes { "WorkflowExecution'" :: WorkflowExecution, "WorkflowType'" :: WorkflowType, "Details'" :: NullOrUndefined (Data), "InitiatedEventId'" :: EventId, "StartedEventId'" :: EventId }
```

<p>Provide details of the <code>ChildWorkflowExecutionCanceled</code> event.</p>

##### Instances
``` purescript
Newtype ChildWorkflowExecutionCanceledEventAttributes _
```

#### `ChildWorkflowExecutionCompletedEventAttributes`

``` purescript
newtype ChildWorkflowExecutionCompletedEventAttributes
  = ChildWorkflowExecutionCompletedEventAttributes { "WorkflowExecution'" :: WorkflowExecution, "WorkflowType'" :: WorkflowType, "Result'" :: NullOrUndefined (Data), "InitiatedEventId'" :: EventId, "StartedEventId'" :: EventId }
```

<p>Provides the details of the <code>ChildWorkflowExecutionCompleted</code> event.</p>

##### Instances
``` purescript
Newtype ChildWorkflowExecutionCompletedEventAttributes _
```

#### `ChildWorkflowExecutionFailedEventAttributes`

``` purescript
newtype ChildWorkflowExecutionFailedEventAttributes
  = ChildWorkflowExecutionFailedEventAttributes { "WorkflowExecution'" :: WorkflowExecution, "WorkflowType'" :: WorkflowType, "Reason'" :: NullOrUndefined (FailureReason), "Details'" :: NullOrUndefined (Data), "InitiatedEventId'" :: EventId, "StartedEventId'" :: EventId }
```

<p>Provides the details of the <code>ChildWorkflowExecutionFailed</code> event.</p>

##### Instances
``` purescript
Newtype ChildWorkflowExecutionFailedEventAttributes _
```

#### `ChildWorkflowExecutionStartedEventAttributes`

``` purescript
newtype ChildWorkflowExecutionStartedEventAttributes
  = ChildWorkflowExecutionStartedEventAttributes { "WorkflowExecution'" :: WorkflowExecution, "WorkflowType'" :: WorkflowType, "InitiatedEventId'" :: EventId }
```

<p>Provides the details of the <code>ChildWorkflowExecutionStarted</code> event.</p>

##### Instances
``` purescript
Newtype ChildWorkflowExecutionStartedEventAttributes _
```

#### `ChildWorkflowExecutionTerminatedEventAttributes`

``` purescript
newtype ChildWorkflowExecutionTerminatedEventAttributes
  = ChildWorkflowExecutionTerminatedEventAttributes { "WorkflowExecution'" :: WorkflowExecution, "WorkflowType'" :: WorkflowType, "InitiatedEventId'" :: EventId, "StartedEventId'" :: EventId }
```

<p>Provides the details of the <code>ChildWorkflowExecutionTerminated</code> event.</p>

##### Instances
``` purescript
Newtype ChildWorkflowExecutionTerminatedEventAttributes _
```

#### `ChildWorkflowExecutionTimedOutEventAttributes`

``` purescript
newtype ChildWorkflowExecutionTimedOutEventAttributes
  = ChildWorkflowExecutionTimedOutEventAttributes { "WorkflowExecution'" :: WorkflowExecution, "WorkflowType'" :: WorkflowType, "TimeoutType'" :: WorkflowExecutionTimeoutType, "InitiatedEventId'" :: EventId, "StartedEventId'" :: EventId }
```

<p>Provides the details of the <code>ChildWorkflowExecutionTimedOut</code> event.</p>

##### Instances
``` purescript
Newtype ChildWorkflowExecutionTimedOutEventAttributes _
```

#### `CloseStatus`

``` purescript
newtype CloseStatus
  = CloseStatus String
```

##### Instances
``` purescript
Newtype CloseStatus _
```

#### `CloseStatusFilter`

``` purescript
newtype CloseStatusFilter
  = CloseStatusFilter { "Status'" :: CloseStatus }
```

<p>Used to filter the closed workflow executions in visibility APIs by their close status.</p>

##### Instances
``` purescript
Newtype CloseStatusFilter _
```

#### `CompleteWorkflowExecutionDecisionAttributes`

``` purescript
newtype CompleteWorkflowExecutionDecisionAttributes
  = CompleteWorkflowExecutionDecisionAttributes { "Result'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>CompleteWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype CompleteWorkflowExecutionDecisionAttributes _
```

#### `CompleteWorkflowExecutionFailedCause`

``` purescript
newtype CompleteWorkflowExecutionFailedCause
  = CompleteWorkflowExecutionFailedCause String
```

##### Instances
``` purescript
Newtype CompleteWorkflowExecutionFailedCause _
```

#### `CompleteWorkflowExecutionFailedEventAttributes`

``` purescript
newtype CompleteWorkflowExecutionFailedEventAttributes
  = CompleteWorkflowExecutionFailedEventAttributes { "Cause'" :: CompleteWorkflowExecutionFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>CompleteWorkflowExecutionFailed</code> event.</p>

##### Instances
``` purescript
Newtype CompleteWorkflowExecutionFailedEventAttributes _
```

#### `ContinueAsNewWorkflowExecutionDecisionAttributes`

``` purescript
newtype ContinueAsNewWorkflowExecutionDecisionAttributes
  = ContinueAsNewWorkflowExecutionDecisionAttributes { "Input'" :: NullOrUndefined (Data), "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "TaskList'" :: NullOrUndefined (TaskList), "TaskPriority'" :: NullOrUndefined (TaskPriority), "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "ChildPolicy'" :: NullOrUndefined (ChildPolicy), "TagList'" :: NullOrUndefined (TagList), "WorkflowTypeVersion'" :: NullOrUndefined (Version), "LambdaRole'" :: NullOrUndefined (Arn) }
```

<p>Provides the details of the <code>ContinueAsNewWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tag</code> – A tag used to identify the workflow execution</p> </li> <li> <p> <code>taskList</code> – String constraint. The key is <code>swf:taskList.name</code>.</p> </li> <li> <p> <code>workflowType.version</code> – String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype ContinueAsNewWorkflowExecutionDecisionAttributes _
```

#### `ContinueAsNewWorkflowExecutionFailedCause`

``` purescript
newtype ContinueAsNewWorkflowExecutionFailedCause
  = ContinueAsNewWorkflowExecutionFailedCause String
```

##### Instances
``` purescript
Newtype ContinueAsNewWorkflowExecutionFailedCause _
```

#### `ContinueAsNewWorkflowExecutionFailedEventAttributes`

``` purescript
newtype ContinueAsNewWorkflowExecutionFailedEventAttributes
  = ContinueAsNewWorkflowExecutionFailedEventAttributes { "Cause'" :: ContinueAsNewWorkflowExecutionFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>ContinueAsNewWorkflowExecutionFailed</code> event.</p>

##### Instances
``` purescript
Newtype ContinueAsNewWorkflowExecutionFailedEventAttributes _
```

#### `Count`

``` purescript
newtype Count
  = Count Int
```

##### Instances
``` purescript
Newtype Count _
```

#### `CountClosedWorkflowExecutionsInput`

``` purescript
newtype CountClosedWorkflowExecutionsInput
  = CountClosedWorkflowExecutionsInput { "Domain'" :: DomainName, "StartTimeFilter'" :: NullOrUndefined (ExecutionTimeFilter), "CloseTimeFilter'" :: NullOrUndefined (ExecutionTimeFilter), "ExecutionFilter'" :: NullOrUndefined (WorkflowExecutionFilter), "TypeFilter'" :: NullOrUndefined (WorkflowTypeFilter), "TagFilter'" :: NullOrUndefined (TagFilter), "CloseStatusFilter'" :: NullOrUndefined (CloseStatusFilter) }
```

##### Instances
``` purescript
Newtype CountClosedWorkflowExecutionsInput _
```

#### `CountOpenWorkflowExecutionsInput`

``` purescript
newtype CountOpenWorkflowExecutionsInput
  = CountOpenWorkflowExecutionsInput { "Domain'" :: DomainName, "StartTimeFilter'" :: ExecutionTimeFilter, "TypeFilter'" :: NullOrUndefined (WorkflowTypeFilter), "TagFilter'" :: NullOrUndefined (TagFilter), "ExecutionFilter'" :: NullOrUndefined (WorkflowExecutionFilter) }
```

##### Instances
``` purescript
Newtype CountOpenWorkflowExecutionsInput _
```

#### `CountPendingActivityTasksInput`

``` purescript
newtype CountPendingActivityTasksInput
  = CountPendingActivityTasksInput { "Domain'" :: DomainName, "TaskList'" :: TaskList }
```

##### Instances
``` purescript
Newtype CountPendingActivityTasksInput _
```

#### `CountPendingDecisionTasksInput`

``` purescript
newtype CountPendingDecisionTasksInput
  = CountPendingDecisionTasksInput { "Domain'" :: DomainName, "TaskList'" :: TaskList }
```

##### Instances
``` purescript
Newtype CountPendingDecisionTasksInput _
```

#### `Data`

``` purescript
newtype Data
  = Data String
```

##### Instances
``` purescript
Newtype Data _
```

#### `Decision`

``` purescript
newtype Decision
  = Decision { "DecisionType'" :: DecisionType, "ScheduleActivityTaskDecisionAttributes'" :: NullOrUndefined (ScheduleActivityTaskDecisionAttributes), "RequestCancelActivityTaskDecisionAttributes'" :: NullOrUndefined (RequestCancelActivityTaskDecisionAttributes), "CompleteWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (CompleteWorkflowExecutionDecisionAttributes), "FailWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (FailWorkflowExecutionDecisionAttributes), "CancelWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (CancelWorkflowExecutionDecisionAttributes), "ContinueAsNewWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (ContinueAsNewWorkflowExecutionDecisionAttributes), "RecordMarkerDecisionAttributes'" :: NullOrUndefined (RecordMarkerDecisionAttributes), "StartTimerDecisionAttributes'" :: NullOrUndefined (StartTimerDecisionAttributes), "CancelTimerDecisionAttributes'" :: NullOrUndefined (CancelTimerDecisionAttributes), "SignalExternalWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (SignalExternalWorkflowExecutionDecisionAttributes), "RequestCancelExternalWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (RequestCancelExternalWorkflowExecutionDecisionAttributes), "StartChildWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (StartChildWorkflowExecutionDecisionAttributes), "ScheduleLambdaFunctionDecisionAttributes'" :: NullOrUndefined (ScheduleLambdaFunctionDecisionAttributes) }
```

<p>Specifies a decision made by the decider. A decision can be one of these types:</p> <ul> <li> <p> <code>CancelTimer</code> – Cancels a previously started timer and records a <code>TimerCanceled</code> event in the history.</p> </li> <li> <p> <code>CancelWorkflowExecution</code> – Closes the workflow execution and records a <code>WorkflowExecutionCanceled</code> event in the history.</p> </li> <li> <p> <code>CompleteWorkflowExecution</code> – Closes the workflow execution and records a <code>WorkflowExecutionCompleted</code> event in the history .</p> </li> <li> <p> <code>ContinueAsNewWorkflowExecution</code> – Closes the workflow execution and starts a new workflow execution of the same type using the same workflow ID and a unique run Id. A <code>WorkflowExecutionContinuedAsNew</code> event is recorded in the history.</p> </li> <li> <p> <code>FailWorkflowExecution</code> – Closes the workflow execution and records a <code>WorkflowExecutionFailed</code> event in the history.</p> </li> <li> <p> <code>RecordMarker</code> – Records a <code>MarkerRecorded</code> event in the history. Markers can be used for adding custom information in the history for instance to let deciders know that they don't need to look at the history beyond the marker event.</p> </li> <li> <p> <code>RequestCancelActivityTask</code> – Attempts to cancel a previously scheduled activity task. If the activity task was scheduled but has not been assigned to a worker, then it is canceled. If the activity task was already assigned to a worker, then the worker is informed that cancellation has been requested in the response to <a>RecordActivityTaskHeartbeat</a>.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecution</code> – Requests that a request be made to cancel the specified external workflow execution and records a <code>RequestCancelExternalWorkflowExecutionInitiated</code> event in the history.</p> </li> <li> <p> <code>ScheduleActivityTask</code> – Schedules an activity task.</p> </li> <li> <p> <code>SignalExternalWorkflowExecution</code> – Requests a signal to be delivered to the specified external workflow execution and records a <code>SignalExternalWorkflowExecutionInitiated</code> event in the history.</p> </li> <li> <p> <code>StartChildWorkflowExecution</code> – Requests that a child workflow execution be started and records a <code>StartChildWorkflowExecutionInitiated</code> event in the history. The child workflow execution is a separate workflow execution with its own history.</p> </li> <li> <p> <code>StartTimer</code> – Starts a timer for this workflow execution and records a <code>TimerStarted</code> event in the history. This timer fires after the specified delay and record a <code>TimerFired</code> event.</p> </li> </ul> <p> <b>Access Control</b> </p> <p>If you grant permission to use <code>RespondDecisionTaskCompleted</code>, you can use IAM policies to express permissions for the list of decisions returned by this action as if they were members of the API. Treating decisions as a pseudo API maintains a uniform conceptual model and helps keep policies readable. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p> <p> <b>Decision Failure</b> </p> <p>Decisions can fail for several reasons</p> <ul> <li> <p>The ordering of decisions should follow a logical flow. Some decisions might not make sense in the current context of the workflow execution and therefore fails.</p> </li> <li> <p>A limit on your account was reached.</p> </li> <li> <p>The decision lacks sufficient permissions.</p> </li> </ul> <p>One of the following events might be added to the history to indicate an error. The event attribute's <code>cause</code> parameter indicates the cause. If <code>cause</code> is set to <code>OPERATION_NOT_PERMITTED</code>, the decision failed because it lacked sufficient permissions. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p> <ul> <li> <p> <code>ScheduleActivityTaskFailed</code> – A <code>ScheduleActivityTask</code> decision failed. This could happen if the activity type specified in the decision isn't registered, is in a deprecated state, or the decision isn't properly configured.</p> </li> <li> <p> <code>RequestCancelActivityTaskFailed</code> – A <code>RequestCancelActivityTask</code> decision failed. This could happen if there is no open activity task with the specified activityId.</p> </li> <li> <p> <code>StartTimerFailed</code> – A <code>StartTimer</code> decision failed. This could happen if there is another open timer with the same timerId.</p> </li> <li> <p> <code>CancelTimerFailed</code> – A <code>CancelTimer</code> decision failed. This could happen if there is no open timer with the specified timerId.</p> </li> <li> <p> <code>StartChildWorkflowExecutionFailed</code> – A <code>StartChildWorkflowExecution</code> decision failed. This could happen if the workflow type specified isn't registered, is deprecated, or the decision isn't properly configured.</p> </li> <li> <p> <code>SignalExternalWorkflowExecutionFailed</code> – A <code>SignalExternalWorkflowExecution</code> decision failed. This could happen if the <code>workflowID</code> specified in the decision was incorrect.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecutionFailed</code> – A <code>RequestCancelExternalWorkflowExecution</code> decision failed. This could happen if the <code>workflowID</code> specified in the decision was incorrect.</p> </li> <li> <p> <code>CancelWorkflowExecutionFailed</code> – A <code>CancelWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.</p> </li> <li> <p> <code>CompleteWorkflowExecutionFailed</code> – A <code>CompleteWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.</p> </li> <li> <p> <code>ContinueAsNewWorkflowExecutionFailed</code> – A <code>ContinueAsNewWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution or the ContinueAsNewWorkflowExecution decision was not configured correctly.</p> </li> <li> <p> <code>FailWorkflowExecutionFailed</code> – A <code>FailWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.</p> </li> </ul> <p>The preceding error events might occur due to an error in the decider logic, which might put the workflow execution in an unstable state The cause field in the event structure for the error event indicates the cause of the error.</p> <note> <p>A workflow execution may be closed by the decider by returning one of the following decisions when completing a decision task: <code>CompleteWorkflowExecution</code>, <code>FailWorkflowExecution</code>, <code>CancelWorkflowExecution</code> and <code>ContinueAsNewWorkflowExecution</code>. An <code>UnhandledDecision</code> fault is returned if a workflow closing decision is specified and a signal or activity event had been added to the history while the decision task was being performed by the decider. Unlike the above situations which are logic issues, this fault is always possible because of race conditions in a distributed system. The right action here is to call <a>RespondDecisionTaskCompleted</a> without any decisions. This would result in another decision task with these new events included in the history. The decider should handle the new events and may decide to close the workflow execution.</p> </note> <p> <b>How to Code a Decision</b> </p> <p>You code a decision by first setting the decision type field to one of the above decision values, and then set the corresponding attributes field shown below:</p> <ul> <li> <p> <code> <a>ScheduleActivityTaskDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>RequestCancelActivityTaskDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>CompleteWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>FailWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>CancelWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>ContinueAsNewWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>RecordMarkerDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>StartTimerDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>CancelTimerDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>SignalExternalWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>RequestCancelExternalWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>StartChildWorkflowExecutionDecisionAttributes</a> </code> </p> </li> </ul>

##### Instances
``` purescript
Newtype Decision _
```

#### `DecisionList`

``` purescript
newtype DecisionList
  = DecisionList (Array Decision)
```

##### Instances
``` purescript
Newtype DecisionList _
```

#### `DecisionTask`

``` purescript
newtype DecisionTask
  = DecisionTask { "TaskToken'" :: TaskToken, "StartedEventId'" :: EventId, "WorkflowExecution'" :: WorkflowExecution, "WorkflowType'" :: WorkflowType, "Events'" :: HistoryEventList, "NextPageToken'" :: NullOrUndefined (PageToken), "PreviousStartedEventId'" :: NullOrUndefined (EventId) }
```

<p>A structure that represents a decision task. Decision tasks are sent to deciders in order for them to make decisions.</p>

##### Instances
``` purescript
Newtype DecisionTask _
```

#### `DecisionTaskCompletedEventAttributes`

``` purescript
newtype DecisionTaskCompletedEventAttributes
  = DecisionTaskCompletedEventAttributes { "ExecutionContext'" :: NullOrUndefined (Data), "ScheduledEventId'" :: EventId, "StartedEventId'" :: EventId }
```

<p>Provides the details of the <code>DecisionTaskCompleted</code> event.</p>

##### Instances
``` purescript
Newtype DecisionTaskCompletedEventAttributes _
```

#### `DecisionTaskScheduledEventAttributes`

``` purescript
newtype DecisionTaskScheduledEventAttributes
  = DecisionTaskScheduledEventAttributes { "TaskList'" :: TaskList, "TaskPriority'" :: NullOrUndefined (TaskPriority), "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional) }
```

<p>Provides details about the <code>DecisionTaskScheduled</code> event.</p>

##### Instances
``` purescript
Newtype DecisionTaskScheduledEventAttributes _
```

#### `DecisionTaskStartedEventAttributes`

``` purescript
newtype DecisionTaskStartedEventAttributes
  = DecisionTaskStartedEventAttributes { "Identity'" :: NullOrUndefined (Identity), "ScheduledEventId'" :: EventId }
```

<p>Provides the details of the <code>DecisionTaskStarted</code> event.</p>

##### Instances
``` purescript
Newtype DecisionTaskStartedEventAttributes _
```

#### `DecisionTaskTimedOutEventAttributes`

``` purescript
newtype DecisionTaskTimedOutEventAttributes
  = DecisionTaskTimedOutEventAttributes { "TimeoutType'" :: DecisionTaskTimeoutType, "ScheduledEventId'" :: EventId, "StartedEventId'" :: EventId }
```

<p>Provides the details of the <code>DecisionTaskTimedOut</code> event.</p>

##### Instances
``` purescript
Newtype DecisionTaskTimedOutEventAttributes _
```

#### `DecisionTaskTimeoutType`

``` purescript
newtype DecisionTaskTimeoutType
  = DecisionTaskTimeoutType String
```

##### Instances
``` purescript
Newtype DecisionTaskTimeoutType _
```

#### `DecisionType`

``` purescript
newtype DecisionType
  = DecisionType String
```

##### Instances
``` purescript
Newtype DecisionType _
```

#### `DefaultUndefinedFault`

``` purescript
newtype DefaultUndefinedFault
  = DefaultUndefinedFault { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The <code>StartWorkflowExecution</code> API action was called without the required parameters set.</p> <p>Some workflow execution parameters, such as the decision <code>taskList</code>, must be set to start the execution. However, these parameters might have been set as defaults when the workflow type was registered. In this case, you can omit these parameters from the <code>StartWorkflowExecution</code> call and Amazon SWF uses the values defined in the workflow type.</p> <note> <p>If these parameters aren't set and no default parameters were defined in the workflow type, this error is displayed.</p> </note>

##### Instances
``` purescript
Newtype DefaultUndefinedFault _
```

#### `DeprecateActivityTypeInput`

``` purescript
newtype DeprecateActivityTypeInput
  = DeprecateActivityTypeInput { "Domain'" :: DomainName, "ActivityType'" :: ActivityType }
```

##### Instances
``` purescript
Newtype DeprecateActivityTypeInput _
```

#### `DeprecateDomainInput`

``` purescript
newtype DeprecateDomainInput
  = DeprecateDomainInput { "Name'" :: DomainName }
```

##### Instances
``` purescript
Newtype DeprecateDomainInput _
```

#### `DeprecateWorkflowTypeInput`

``` purescript
newtype DeprecateWorkflowTypeInput
  = DeprecateWorkflowTypeInput { "Domain'" :: DomainName, "WorkflowType'" :: WorkflowType }
```

##### Instances
``` purescript
Newtype DeprecateWorkflowTypeInput _
```

#### `DescribeActivityTypeInput`

``` purescript
newtype DescribeActivityTypeInput
  = DescribeActivityTypeInput { "Domain'" :: DomainName, "ActivityType'" :: ActivityType }
```

##### Instances
``` purescript
Newtype DescribeActivityTypeInput _
```

#### `DescribeDomainInput`

``` purescript
newtype DescribeDomainInput
  = DescribeDomainInput { "Name'" :: DomainName }
```

##### Instances
``` purescript
Newtype DescribeDomainInput _
```

#### `DescribeWorkflowExecutionInput`

``` purescript
newtype DescribeWorkflowExecutionInput
  = DescribeWorkflowExecutionInput { "Domain'" :: DomainName, "Execution'" :: WorkflowExecution }
```

##### Instances
``` purescript
Newtype DescribeWorkflowExecutionInput _
```

#### `DescribeWorkflowTypeInput`

``` purescript
newtype DescribeWorkflowTypeInput
  = DescribeWorkflowTypeInput { "Domain'" :: DomainName, "WorkflowType'" :: WorkflowType }
```

##### Instances
``` purescript
Newtype DescribeWorkflowTypeInput _
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

#### `DomainAlreadyExistsFault`

``` purescript
newtype DomainAlreadyExistsFault
  = DomainAlreadyExistsFault { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Returned if the specified domain already exists. You get this fault even if the existing domain is in deprecated status.</p>

##### Instances
``` purescript
Newtype DomainAlreadyExistsFault _
```

#### `DomainConfiguration`

``` purescript
newtype DomainConfiguration
  = DomainConfiguration { "WorkflowExecutionRetentionPeriodInDays'" :: DurationInDays }
```

<p>Contains the configuration settings of a domain.</p>

##### Instances
``` purescript
Newtype DomainConfiguration _
```

#### `DomainDeprecatedFault`

``` purescript
newtype DomainDeprecatedFault
  = DomainDeprecatedFault { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Returned when the specified domain has been deprecated.</p>

##### Instances
``` purescript
Newtype DomainDeprecatedFault _
```

#### `DomainDetail`

``` purescript
newtype DomainDetail
  = DomainDetail { "DomainInfo'" :: DomainInfo, "Configuration'" :: DomainConfiguration }
```

<p>Contains details of a domain.</p>

##### Instances
``` purescript
Newtype DomainDetail _
```

#### `DomainInfo`

``` purescript
newtype DomainInfo
  = DomainInfo { "Name'" :: DomainName, "Status'" :: RegistrationStatus, "Description'" :: NullOrUndefined (Description) }
```

<p>Contains general information about a domain.</p>

##### Instances
``` purescript
Newtype DomainInfo _
```

#### `DomainInfoList`

``` purescript
newtype DomainInfoList
  = DomainInfoList (Array DomainInfo)
```

##### Instances
``` purescript
Newtype DomainInfoList _
```

#### `DomainInfos`

``` purescript
newtype DomainInfos
  = DomainInfos { "DomainInfos'" :: DomainInfoList, "NextPageToken'" :: NullOrUndefined (PageToken) }
```

<p>Contains a paginated collection of DomainInfo structures.</p>

##### Instances
``` purescript
Newtype DomainInfos _
```

#### `DomainName`

``` purescript
newtype DomainName
  = DomainName String
```

##### Instances
``` purescript
Newtype DomainName _
```

#### `DurationInDays`

``` purescript
newtype DurationInDays
  = DurationInDays String
```

##### Instances
``` purescript
Newtype DurationInDays _
```

#### `DurationInSeconds`

``` purescript
newtype DurationInSeconds
  = DurationInSeconds String
```

##### Instances
``` purescript
Newtype DurationInSeconds _
```

#### `DurationInSecondsOptional`

``` purescript
newtype DurationInSecondsOptional
  = DurationInSecondsOptional String
```

##### Instances
``` purescript
Newtype DurationInSecondsOptional _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `EventId`

``` purescript
newtype EventId
  = EventId Number
```

##### Instances
``` purescript
Newtype EventId _
```

#### `EventType`

``` purescript
newtype EventType
  = EventType String
```

##### Instances
``` purescript
Newtype EventType _
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

#### `ExecutionTimeFilter`

``` purescript
newtype ExecutionTimeFilter
  = ExecutionTimeFilter { "OldestDate'" :: Number, "LatestDate'" :: NullOrUndefined (Number) }
```

<p>Used to filter the workflow executions in visibility APIs by various time-based rules. Each parameter, if specified, defines a rule that must be satisfied by each returned query result. The parameter values are in the <a href="https://en.wikipedia.org/wiki/Unix_time">Unix Time format</a>. For example: <code>"oldestDate": 1325376070.</code> </p>

##### Instances
``` purescript
Newtype ExecutionTimeFilter _
```

#### `ExternalWorkflowExecutionCancelRequestedEventAttributes`

``` purescript
newtype ExternalWorkflowExecutionCancelRequestedEventAttributes
  = ExternalWorkflowExecutionCancelRequestedEventAttributes { "WorkflowExecution'" :: WorkflowExecution, "InitiatedEventId'" :: EventId }
```

<p>Provides the details of the <code>ExternalWorkflowExecutionCancelRequested</code> event.</p>

##### Instances
``` purescript
Newtype ExternalWorkflowExecutionCancelRequestedEventAttributes _
```

#### `ExternalWorkflowExecutionSignaledEventAttributes`

``` purescript
newtype ExternalWorkflowExecutionSignaledEventAttributes
  = ExternalWorkflowExecutionSignaledEventAttributes { "WorkflowExecution'" :: WorkflowExecution, "InitiatedEventId'" :: EventId }
```

<p>Provides the details of the <code>ExternalWorkflowExecutionSignaled</code> event.</p>

##### Instances
``` purescript
Newtype ExternalWorkflowExecutionSignaledEventAttributes _
```

#### `FailWorkflowExecutionDecisionAttributes`

``` purescript
newtype FailWorkflowExecutionDecisionAttributes
  = FailWorkflowExecutionDecisionAttributes { "Reason'" :: NullOrUndefined (FailureReason), "Details'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>FailWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype FailWorkflowExecutionDecisionAttributes _
```

#### `FailWorkflowExecutionFailedCause`

``` purescript
newtype FailWorkflowExecutionFailedCause
  = FailWorkflowExecutionFailedCause String
```

##### Instances
``` purescript
Newtype FailWorkflowExecutionFailedCause _
```

#### `FailWorkflowExecutionFailedEventAttributes`

``` purescript
newtype FailWorkflowExecutionFailedEventAttributes
  = FailWorkflowExecutionFailedEventAttributes { "Cause'" :: FailWorkflowExecutionFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>FailWorkflowExecutionFailed</code> event.</p>

##### Instances
``` purescript
Newtype FailWorkflowExecutionFailedEventAttributes _
```

#### `FailureReason`

``` purescript
newtype FailureReason
  = FailureReason String
```

##### Instances
``` purescript
Newtype FailureReason _
```

#### `FunctionId`

``` purescript
newtype FunctionId
  = FunctionId String
```

##### Instances
``` purescript
Newtype FunctionId _
```

#### `FunctionInput`

``` purescript
newtype FunctionInput
  = FunctionInput String
```

##### Instances
``` purescript
Newtype FunctionInput _
```

#### `FunctionName`

``` purescript
newtype FunctionName
  = FunctionName String
```

##### Instances
``` purescript
Newtype FunctionName _
```

#### `GetWorkflowExecutionHistoryInput`

``` purescript
newtype GetWorkflowExecutionHistoryInput
  = GetWorkflowExecutionHistoryInput { "Domain'" :: DomainName, "Execution'" :: WorkflowExecution, "NextPageToken'" :: NullOrUndefined (PageToken), "MaximumPageSize'" :: NullOrUndefined (PageSize), "ReverseOrder'" :: NullOrUndefined (ReverseOrder) }
```

##### Instances
``` purescript
Newtype GetWorkflowExecutionHistoryInput _
```

#### `History`

``` purescript
newtype History
  = History { "Events'" :: HistoryEventList, "NextPageToken'" :: NullOrUndefined (PageToken) }
```

<p>Paginated representation of a workflow history for a workflow execution. This is the up to date, complete and authoritative record of the events related to all tasks and events in the life of the workflow execution.</p>

##### Instances
``` purescript
Newtype History _
```

#### `HistoryEvent`

``` purescript
newtype HistoryEvent
  = HistoryEvent { "EventTimestamp'" :: Number, "EventType'" :: EventType, "EventId'" :: EventId, "WorkflowExecutionStartedEventAttributes'" :: NullOrUndefined (WorkflowExecutionStartedEventAttributes), "WorkflowExecutionCompletedEventAttributes'" :: NullOrUndefined (WorkflowExecutionCompletedEventAttributes), "CompleteWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (CompleteWorkflowExecutionFailedEventAttributes), "WorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (WorkflowExecutionFailedEventAttributes), "FailWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (FailWorkflowExecutionFailedEventAttributes), "WorkflowExecutionTimedOutEventAttributes'" :: NullOrUndefined (WorkflowExecutionTimedOutEventAttributes), "WorkflowExecutionCanceledEventAttributes'" :: NullOrUndefined (WorkflowExecutionCanceledEventAttributes), "CancelWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (CancelWorkflowExecutionFailedEventAttributes), "WorkflowExecutionContinuedAsNewEventAttributes'" :: NullOrUndefined (WorkflowExecutionContinuedAsNewEventAttributes), "ContinueAsNewWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (ContinueAsNewWorkflowExecutionFailedEventAttributes), "WorkflowExecutionTerminatedEventAttributes'" :: NullOrUndefined (WorkflowExecutionTerminatedEventAttributes), "WorkflowExecutionCancelRequestedEventAttributes'" :: NullOrUndefined (WorkflowExecutionCancelRequestedEventAttributes), "DecisionTaskScheduledEventAttributes'" :: NullOrUndefined (DecisionTaskScheduledEventAttributes), "DecisionTaskStartedEventAttributes'" :: NullOrUndefined (DecisionTaskStartedEventAttributes), "DecisionTaskCompletedEventAttributes'" :: NullOrUndefined (DecisionTaskCompletedEventAttributes), "DecisionTaskTimedOutEventAttributes'" :: NullOrUndefined (DecisionTaskTimedOutEventAttributes), "ActivityTaskScheduledEventAttributes'" :: NullOrUndefined (ActivityTaskScheduledEventAttributes), "ActivityTaskStartedEventAttributes'" :: NullOrUndefined (ActivityTaskStartedEventAttributes), "ActivityTaskCompletedEventAttributes'" :: NullOrUndefined (ActivityTaskCompletedEventAttributes), "ActivityTaskFailedEventAttributes'" :: NullOrUndefined (ActivityTaskFailedEventAttributes), "ActivityTaskTimedOutEventAttributes'" :: NullOrUndefined (ActivityTaskTimedOutEventAttributes), "ActivityTaskCanceledEventAttributes'" :: NullOrUndefined (ActivityTaskCanceledEventAttributes), "ActivityTaskCancelRequestedEventAttributes'" :: NullOrUndefined (ActivityTaskCancelRequestedEventAttributes), "WorkflowExecutionSignaledEventAttributes'" :: NullOrUndefined (WorkflowExecutionSignaledEventAttributes), "MarkerRecordedEventAttributes'" :: NullOrUndefined (MarkerRecordedEventAttributes), "RecordMarkerFailedEventAttributes'" :: NullOrUndefined (RecordMarkerFailedEventAttributes), "TimerStartedEventAttributes'" :: NullOrUndefined (TimerStartedEventAttributes), "TimerFiredEventAttributes'" :: NullOrUndefined (TimerFiredEventAttributes), "TimerCanceledEventAttributes'" :: NullOrUndefined (TimerCanceledEventAttributes), "StartChildWorkflowExecutionInitiatedEventAttributes'" :: NullOrUndefined (StartChildWorkflowExecutionInitiatedEventAttributes), "ChildWorkflowExecutionStartedEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionStartedEventAttributes), "ChildWorkflowExecutionCompletedEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionCompletedEventAttributes), "ChildWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionFailedEventAttributes), "ChildWorkflowExecutionTimedOutEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionTimedOutEventAttributes), "ChildWorkflowExecutionCanceledEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionCanceledEventAttributes), "ChildWorkflowExecutionTerminatedEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionTerminatedEventAttributes), "SignalExternalWorkflowExecutionInitiatedEventAttributes'" :: NullOrUndefined (SignalExternalWorkflowExecutionInitiatedEventAttributes), "ExternalWorkflowExecutionSignaledEventAttributes'" :: NullOrUndefined (ExternalWorkflowExecutionSignaledEventAttributes), "SignalExternalWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (SignalExternalWorkflowExecutionFailedEventAttributes), "ExternalWorkflowExecutionCancelRequestedEventAttributes'" :: NullOrUndefined (ExternalWorkflowExecutionCancelRequestedEventAttributes), "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'" :: NullOrUndefined (RequestCancelExternalWorkflowExecutionInitiatedEventAttributes), "RequestCancelExternalWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (RequestCancelExternalWorkflowExecutionFailedEventAttributes), "ScheduleActivityTaskFailedEventAttributes'" :: NullOrUndefined (ScheduleActivityTaskFailedEventAttributes), "RequestCancelActivityTaskFailedEventAttributes'" :: NullOrUndefined (RequestCancelActivityTaskFailedEventAttributes), "StartTimerFailedEventAttributes'" :: NullOrUndefined (StartTimerFailedEventAttributes), "CancelTimerFailedEventAttributes'" :: NullOrUndefined (CancelTimerFailedEventAttributes), "StartChildWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (StartChildWorkflowExecutionFailedEventAttributes), "LambdaFunctionScheduledEventAttributes'" :: NullOrUndefined (LambdaFunctionScheduledEventAttributes), "LambdaFunctionStartedEventAttributes'" :: NullOrUndefined (LambdaFunctionStartedEventAttributes), "LambdaFunctionCompletedEventAttributes'" :: NullOrUndefined (LambdaFunctionCompletedEventAttributes), "LambdaFunctionFailedEventAttributes'" :: NullOrUndefined (LambdaFunctionFailedEventAttributes), "LambdaFunctionTimedOutEventAttributes'" :: NullOrUndefined (LambdaFunctionTimedOutEventAttributes), "ScheduleLambdaFunctionFailedEventAttributes'" :: NullOrUndefined (ScheduleLambdaFunctionFailedEventAttributes), "StartLambdaFunctionFailedEventAttributes'" :: NullOrUndefined (StartLambdaFunctionFailedEventAttributes) }
```

<p>Event within a workflow execution. A history event can be one of these types:</p> <ul> <li> <p> <code>ActivityTaskCancelRequested</code> – A <code>RequestCancelActivityTask</code> decision was received by the system.</p> </li> <li> <p> <code>ActivityTaskCanceled</code> – The activity task was successfully canceled.</p> </li> <li> <p> <code>ActivityTaskCompleted</code> – An activity worker successfully completed an activity task by calling <a>RespondActivityTaskCompleted</a>.</p> </li> <li> <p> <code>ActivityTaskFailed</code> – An activity worker failed an activity task by calling <a>RespondActivityTaskFailed</a>.</p> </li> <li> <p> <code>ActivityTaskScheduled</code> – An activity task was scheduled for execution.</p> </li> <li> <p> <code>ActivityTaskStarted</code> – The scheduled activity task was dispatched to a worker.</p> </li> <li> <p> <code>ActivityTaskTimedOut</code> – The activity task timed out.</p> </li> <li> <p> <code>CancelTimerFailed</code> – Failed to process CancelTimer decision. This happens when the decision isn't configured properly, for example no timer exists with the specified timer Id.</p> </li> <li> <p> <code>CancelWorkflowExecutionFailed</code> – A request to cancel a workflow execution failed.</p> </li> <li> <p> <code>ChildWorkflowExecutionCanceled</code> – A child workflow execution, started by this workflow execution, was canceled and closed.</p> </li> <li> <p> <code>ChildWorkflowExecutionCompleted</code> – A child workflow execution, started by this workflow execution, completed successfully and was closed.</p> </li> <li> <p> <code>ChildWorkflowExecutionFailed</code> – A child workflow execution, started by this workflow execution, failed to complete successfully and was closed.</p> </li> <li> <p> <code>ChildWorkflowExecutionStarted</code> – A child workflow execution was successfully started.</p> </li> <li> <p> <code>ChildWorkflowExecutionTerminated</code> – A child workflow execution, started by this workflow execution, was terminated.</p> </li> <li> <p> <code>ChildWorkflowExecutionTimedOut</code> – A child workflow execution, started by this workflow execution, timed out and was closed.</p> </li> <li> <p> <code>CompleteWorkflowExecutionFailed</code> – The workflow execution failed to complete.</p> </li> <li> <p> <code>ContinueAsNewWorkflowExecutionFailed</code> – The workflow execution failed to complete after being continued as a new workflow execution.</p> </li> <li> <p> <code>DecisionTaskCompleted</code> – The decider successfully completed a decision task by calling <a>RespondDecisionTaskCompleted</a>.</p> </li> <li> <p> <code>DecisionTaskScheduled</code> – A decision task was scheduled for the workflow execution.</p> </li> <li> <p> <code>DecisionTaskStarted</code> – The decision task was dispatched to a decider.</p> </li> <li> <p> <code>DecisionTaskTimedOut</code> – The decision task timed out.</p> </li> <li> <p> <code>ExternalWorkflowExecutionCancelRequested</code> – Request to cancel an external workflow execution was successfully delivered to the target execution.</p> </li> <li> <p> <code>ExternalWorkflowExecutionSignaled</code> – A signal, requested by this workflow execution, was successfully delivered to the target external workflow execution.</p> </li> <li> <p> <code>FailWorkflowExecutionFailed</code> – A request to mark a workflow execution as failed, itself failed.</p> </li> <li> <p> <code>MarkerRecorded</code> – A marker was recorded in the workflow history as the result of a <code>RecordMarker</code> decision.</p> </li> <li> <p> <code>RecordMarkerFailed</code> – A <code>RecordMarker</code> decision was returned as failed.</p> </li> <li> <p> <code>RequestCancelActivityTaskFailed</code> – Failed to process RequestCancelActivityTask decision. This happens when the decision isn't configured properly.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecutionFailed</code> – Request to cancel an external workflow execution failed.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecutionInitiated</code> – A request was made to request the cancellation of an external workflow execution.</p> </li> <li> <p> <code>ScheduleActivityTaskFailed</code> – Failed to process ScheduleActivityTask decision. This happens when the decision isn't configured properly, for example the activity type specified isn't registered.</p> </li> <li> <p> <code>SignalExternalWorkflowExecutionFailed</code> – The request to signal an external workflow execution failed.</p> </li> <li> <p> <code>SignalExternalWorkflowExecutionInitiated</code> – A request to signal an external workflow was made.</p> </li> <li> <p> <code>StartActivityTaskFailed</code> – A scheduled activity task failed to start.</p> </li> <li> <p> <code>StartChildWorkflowExecutionFailed</code> – Failed to process StartChildWorkflowExecution decision. This happens when the decision isn't configured properly, for example the workflow type specified isn't registered.</p> </li> <li> <p> <code>StartChildWorkflowExecutionInitiated</code> – A request was made to start a child workflow execution.</p> </li> <li> <p> <code>StartTimerFailed</code> – Failed to process StartTimer decision. This happens when the decision isn't configured properly, for example a timer already exists with the specified timer Id.</p> </li> <li> <p> <code>TimerCanceled</code> – A timer, previously started for this workflow execution, was successfully canceled.</p> </li> <li> <p> <code>TimerFired</code> – A timer, previously started for this workflow execution, fired.</p> </li> <li> <p> <code>TimerStarted</code> – A timer was started for the workflow execution due to a <code>StartTimer</code> decision.</p> </li> <li> <p> <code>WorkflowExecutionCancelRequested</code> – A request to cancel this workflow execution was made.</p> </li> <li> <p> <code>WorkflowExecutionCanceled</code> – The workflow execution was successfully canceled and closed.</p> </li> <li> <p> <code>WorkflowExecutionCompleted</code> – The workflow execution was closed due to successful completion.</p> </li> <li> <p> <code>WorkflowExecutionContinuedAsNew</code> – The workflow execution was closed and a new execution of the same type was created with the same workflowId.</p> </li> <li> <p> <code>WorkflowExecutionFailed</code> – The workflow execution closed due to a failure.</p> </li> <li> <p> <code>WorkflowExecutionSignaled</code> – An external signal was received for the workflow execution.</p> </li> <li> <p> <code>WorkflowExecutionStarted</code> – The workflow execution was started.</p> </li> <li> <p> <code>WorkflowExecutionTerminated</code> – The workflow execution was terminated.</p> </li> <li> <p> <code>WorkflowExecutionTimedOut</code> – The workflow execution was closed because a time out was exceeded.</p> </li> </ul>

##### Instances
``` purescript
Newtype HistoryEvent _
```

#### `HistoryEventList`

``` purescript
newtype HistoryEventList
  = HistoryEventList (Array HistoryEvent)
```

##### Instances
``` purescript
Newtype HistoryEventList _
```

#### `Identity`

``` purescript
newtype Identity
  = Identity String
```

##### Instances
``` purescript
Newtype Identity _
```

#### `LambdaFunctionCompletedEventAttributes`

``` purescript
newtype LambdaFunctionCompletedEventAttributes
  = LambdaFunctionCompletedEventAttributes { "ScheduledEventId'" :: EventId, "StartedEventId'" :: EventId, "Result'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>LambdaFunctionCompleted</code> event. It isn't set for other event types.</p>

##### Instances
``` purescript
Newtype LambdaFunctionCompletedEventAttributes _
```

#### `LambdaFunctionFailedEventAttributes`

``` purescript
newtype LambdaFunctionFailedEventAttributes
  = LambdaFunctionFailedEventAttributes { "ScheduledEventId'" :: EventId, "StartedEventId'" :: EventId, "Reason'" :: NullOrUndefined (FailureReason), "Details'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>LambdaFunctionFailed</code> event. It isn't set for other event types.</p>

##### Instances
``` purescript
Newtype LambdaFunctionFailedEventAttributes _
```

#### `LambdaFunctionScheduledEventAttributes`

``` purescript
newtype LambdaFunctionScheduledEventAttributes
  = LambdaFunctionScheduledEventAttributes { "Id'" :: FunctionId, "Name'" :: FunctionName, "Control'" :: NullOrUndefined (Data), "Input'" :: NullOrUndefined (FunctionInput), "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>LambdaFunctionScheduled</code> event. It isn't set for other event types.</p>

##### Instances
``` purescript
Newtype LambdaFunctionScheduledEventAttributes _
```

#### `LambdaFunctionStartedEventAttributes`

``` purescript
newtype LambdaFunctionStartedEventAttributes
  = LambdaFunctionStartedEventAttributes { "ScheduledEventId'" :: EventId }
```

<p>Provides the details of the <code>LambdaFunctionStarted</code> event. It isn't set for other event types.</p>

##### Instances
``` purescript
Newtype LambdaFunctionStartedEventAttributes _
```

#### `LambdaFunctionTimedOutEventAttributes`

``` purescript
newtype LambdaFunctionTimedOutEventAttributes
  = LambdaFunctionTimedOutEventAttributes { "ScheduledEventId'" :: EventId, "StartedEventId'" :: EventId, "TimeoutType'" :: NullOrUndefined (LambdaFunctionTimeoutType) }
```

<p>Provides details of the <code>LambdaFunctionTimedOut</code> event.</p>

##### Instances
``` purescript
Newtype LambdaFunctionTimedOutEventAttributes _
```

#### `LambdaFunctionTimeoutType`

``` purescript
newtype LambdaFunctionTimeoutType
  = LambdaFunctionTimeoutType String
```

##### Instances
``` purescript
Newtype LambdaFunctionTimeoutType _
```

#### `LimitExceededFault`

``` purescript
newtype LimitExceededFault
  = LimitExceededFault { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Returned by any operation if a system imposed limitation has been reached. To address this fault you should either clean up unused resources or increase the limit by contacting AWS.</p>

##### Instances
``` purescript
Newtype LimitExceededFault _
```

#### `LimitedData`

``` purescript
newtype LimitedData
  = LimitedData String
```

##### Instances
``` purescript
Newtype LimitedData _
```

#### `ListActivityTypesInput`

``` purescript
newtype ListActivityTypesInput
  = ListActivityTypesInput { "Domain'" :: DomainName, "Name'" :: NullOrUndefined (Name), "RegistrationStatus'" :: RegistrationStatus, "NextPageToken'" :: NullOrUndefined (PageToken), "MaximumPageSize'" :: NullOrUndefined (PageSize), "ReverseOrder'" :: NullOrUndefined (ReverseOrder) }
```

##### Instances
``` purescript
Newtype ListActivityTypesInput _
```

#### `ListClosedWorkflowExecutionsInput`

``` purescript
newtype ListClosedWorkflowExecutionsInput
  = ListClosedWorkflowExecutionsInput { "Domain'" :: DomainName, "StartTimeFilter'" :: NullOrUndefined (ExecutionTimeFilter), "CloseTimeFilter'" :: NullOrUndefined (ExecutionTimeFilter), "ExecutionFilter'" :: NullOrUndefined (WorkflowExecutionFilter), "CloseStatusFilter'" :: NullOrUndefined (CloseStatusFilter), "TypeFilter'" :: NullOrUndefined (WorkflowTypeFilter), "TagFilter'" :: NullOrUndefined (TagFilter), "NextPageToken'" :: NullOrUndefined (PageToken), "MaximumPageSize'" :: NullOrUndefined (PageSize), "ReverseOrder'" :: NullOrUndefined (ReverseOrder) }
```

##### Instances
``` purescript
Newtype ListClosedWorkflowExecutionsInput _
```

#### `ListDomainsInput`

``` purescript
newtype ListDomainsInput
  = ListDomainsInput { "NextPageToken'" :: NullOrUndefined (PageToken), "RegistrationStatus'" :: RegistrationStatus, "MaximumPageSize'" :: NullOrUndefined (PageSize), "ReverseOrder'" :: NullOrUndefined (ReverseOrder) }
```

##### Instances
``` purescript
Newtype ListDomainsInput _
```

#### `ListOpenWorkflowExecutionsInput`

``` purescript
newtype ListOpenWorkflowExecutionsInput
  = ListOpenWorkflowExecutionsInput { "Domain'" :: DomainName, "StartTimeFilter'" :: ExecutionTimeFilter, "TypeFilter'" :: NullOrUndefined (WorkflowTypeFilter), "TagFilter'" :: NullOrUndefined (TagFilter), "NextPageToken'" :: NullOrUndefined (PageToken), "MaximumPageSize'" :: NullOrUndefined (PageSize), "ReverseOrder'" :: NullOrUndefined (ReverseOrder), "ExecutionFilter'" :: NullOrUndefined (WorkflowExecutionFilter) }
```

##### Instances
``` purescript
Newtype ListOpenWorkflowExecutionsInput _
```

#### `ListWorkflowTypesInput`

``` purescript
newtype ListWorkflowTypesInput
  = ListWorkflowTypesInput { "Domain'" :: DomainName, "Name'" :: NullOrUndefined (Name), "RegistrationStatus'" :: RegistrationStatus, "NextPageToken'" :: NullOrUndefined (PageToken), "MaximumPageSize'" :: NullOrUndefined (PageSize), "ReverseOrder'" :: NullOrUndefined (ReverseOrder) }
```

##### Instances
``` purescript
Newtype ListWorkflowTypesInput _
```

#### `MarkerName`

``` purescript
newtype MarkerName
  = MarkerName String
```

##### Instances
``` purescript
Newtype MarkerName _
```

#### `MarkerRecordedEventAttributes`

``` purescript
newtype MarkerRecordedEventAttributes
  = MarkerRecordedEventAttributes { "MarkerName'" :: MarkerName, "Details'" :: NullOrUndefined (Data), "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>MarkerRecorded</code> event.</p>

##### Instances
``` purescript
Newtype MarkerRecordedEventAttributes _
```

#### `Name`

``` purescript
newtype Name
  = Name String
```

##### Instances
``` purescript
Newtype Name _
```

#### `OpenDecisionTasksCount`

``` purescript
newtype OpenDecisionTasksCount
  = OpenDecisionTasksCount Int
```

##### Instances
``` purescript
Newtype OpenDecisionTasksCount _
```

#### `OperationNotPermittedFault`

``` purescript
newtype OperationNotPermittedFault
  = OperationNotPermittedFault { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Returned when the caller doesn't have sufficient permissions to invoke the action.</p>

##### Instances
``` purescript
Newtype OperationNotPermittedFault _
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

##### Instances
``` purescript
Newtype PageSize _
```

#### `PageToken`

``` purescript
newtype PageToken
  = PageToken String
```

##### Instances
``` purescript
Newtype PageToken _
```

#### `PendingTaskCount`

``` purescript
newtype PendingTaskCount
  = PendingTaskCount { "Count'" :: Count, "Truncated'" :: NullOrUndefined (Truncated) }
```

<p>Contains the count of tasks in a task list.</p>

##### Instances
``` purescript
Newtype PendingTaskCount _
```

#### `PollForActivityTaskInput`

``` purescript
newtype PollForActivityTaskInput
  = PollForActivityTaskInput { "Domain'" :: DomainName, "TaskList'" :: TaskList, "Identity'" :: NullOrUndefined (Identity) }
```

##### Instances
``` purescript
Newtype PollForActivityTaskInput _
```

#### `PollForDecisionTaskInput`

``` purescript
newtype PollForDecisionTaskInput
  = PollForDecisionTaskInput { "Domain'" :: DomainName, "TaskList'" :: TaskList, "Identity'" :: NullOrUndefined (Identity), "NextPageToken'" :: NullOrUndefined (PageToken), "MaximumPageSize'" :: NullOrUndefined (PageSize), "ReverseOrder'" :: NullOrUndefined (ReverseOrder) }
```

##### Instances
``` purescript
Newtype PollForDecisionTaskInput _
```

#### `RecordActivityTaskHeartbeatInput`

``` purescript
newtype RecordActivityTaskHeartbeatInput
  = RecordActivityTaskHeartbeatInput { "TaskToken'" :: TaskToken, "Details'" :: NullOrUndefined (LimitedData) }
```

##### Instances
``` purescript
Newtype RecordActivityTaskHeartbeatInput _
```

#### `RecordMarkerDecisionAttributes`

``` purescript
newtype RecordMarkerDecisionAttributes
  = RecordMarkerDecisionAttributes { "MarkerName'" :: MarkerName, "Details'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>RecordMarker</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype RecordMarkerDecisionAttributes _
```

#### `RecordMarkerFailedCause`

``` purescript
newtype RecordMarkerFailedCause
  = RecordMarkerFailedCause String
```

##### Instances
``` purescript
Newtype RecordMarkerFailedCause _
```

#### `RecordMarkerFailedEventAttributes`

``` purescript
newtype RecordMarkerFailedEventAttributes
  = RecordMarkerFailedEventAttributes { "MarkerName'" :: MarkerName, "Cause'" :: RecordMarkerFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>RecordMarkerFailed</code> event.</p>

##### Instances
``` purescript
Newtype RecordMarkerFailedEventAttributes _
```

#### `RegisterActivityTypeInput`

``` purescript
newtype RegisterActivityTypeInput
  = RegisterActivityTypeInput { "Domain'" :: DomainName, "Name'" :: Name, "Version'" :: Version, "Description'" :: NullOrUndefined (Description), "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultTaskHeartbeatTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultTaskList'" :: NullOrUndefined (TaskList), "DefaultTaskPriority'" :: NullOrUndefined (TaskPriority), "DefaultTaskScheduleToStartTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultTaskScheduleToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional) }
```

##### Instances
``` purescript
Newtype RegisterActivityTypeInput _
```

#### `RegisterDomainInput`

``` purescript
newtype RegisterDomainInput
  = RegisterDomainInput { "Name'" :: DomainName, "Description'" :: NullOrUndefined (Description), "WorkflowExecutionRetentionPeriodInDays'" :: DurationInDays }
```

##### Instances
``` purescript
Newtype RegisterDomainInput _
```

#### `RegisterWorkflowTypeInput`

``` purescript
newtype RegisterWorkflowTypeInput
  = RegisterWorkflowTypeInput { "Domain'" :: DomainName, "Name'" :: Name, "Version'" :: Version, "Description'" :: NullOrUndefined (Description), "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultTaskList'" :: NullOrUndefined (TaskList), "DefaultTaskPriority'" :: NullOrUndefined (TaskPriority), "DefaultChildPolicy'" :: NullOrUndefined (ChildPolicy), "DefaultLambdaRole'" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype RegisterWorkflowTypeInput _
```

#### `RegistrationStatus`

``` purescript
newtype RegistrationStatus
  = RegistrationStatus String
```

##### Instances
``` purescript
Newtype RegistrationStatus _
```

#### `RequestCancelActivityTaskDecisionAttributes`

``` purescript
newtype RequestCancelActivityTaskDecisionAttributes
  = RequestCancelActivityTaskDecisionAttributes { "ActivityId'" :: ActivityId }
```

<p>Provides the details of the <code>RequestCancelActivityTask</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype RequestCancelActivityTaskDecisionAttributes _
```

#### `RequestCancelActivityTaskFailedCause`

``` purescript
newtype RequestCancelActivityTaskFailedCause
  = RequestCancelActivityTaskFailedCause String
```

##### Instances
``` purescript
Newtype RequestCancelActivityTaskFailedCause _
```

#### `RequestCancelActivityTaskFailedEventAttributes`

``` purescript
newtype RequestCancelActivityTaskFailedEventAttributes
  = RequestCancelActivityTaskFailedEventAttributes { "ActivityId'" :: ActivityId, "Cause'" :: RequestCancelActivityTaskFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>RequestCancelActivityTaskFailed</code> event.</p>

##### Instances
``` purescript
Newtype RequestCancelActivityTaskFailedEventAttributes _
```

#### `RequestCancelExternalWorkflowExecutionDecisionAttributes`

``` purescript
newtype RequestCancelExternalWorkflowExecutionDecisionAttributes
  = RequestCancelExternalWorkflowExecutionDecisionAttributes { "WorkflowId'" :: WorkflowId, "RunId'" :: NullOrUndefined (WorkflowRunIdOptional), "Control'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>RequestCancelExternalWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype RequestCancelExternalWorkflowExecutionDecisionAttributes _
```

#### `RequestCancelExternalWorkflowExecutionFailedCause`

``` purescript
newtype RequestCancelExternalWorkflowExecutionFailedCause
  = RequestCancelExternalWorkflowExecutionFailedCause String
```

##### Instances
``` purescript
Newtype RequestCancelExternalWorkflowExecutionFailedCause _
```

#### `RequestCancelExternalWorkflowExecutionFailedEventAttributes`

``` purescript
newtype RequestCancelExternalWorkflowExecutionFailedEventAttributes
  = RequestCancelExternalWorkflowExecutionFailedEventAttributes { "WorkflowId'" :: WorkflowId, "RunId'" :: NullOrUndefined (WorkflowRunIdOptional), "Cause'" :: RequestCancelExternalWorkflowExecutionFailedCause, "InitiatedEventId'" :: EventId, "DecisionTaskCompletedEventId'" :: EventId, "Control'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>RequestCancelExternalWorkflowExecutionFailed</code> event.</p>

##### Instances
``` purescript
Newtype RequestCancelExternalWorkflowExecutionFailedEventAttributes _
```

#### `RequestCancelExternalWorkflowExecutionInitiatedEventAttributes`

``` purescript
newtype RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes { "WorkflowId'" :: WorkflowId, "RunId'" :: NullOrUndefined (WorkflowRunIdOptional), "DecisionTaskCompletedEventId'" :: EventId, "Control'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>RequestCancelExternalWorkflowExecutionInitiated</code> event.</p>

##### Instances
``` purescript
Newtype RequestCancelExternalWorkflowExecutionInitiatedEventAttributes _
```

#### `RequestCancelWorkflowExecutionInput`

``` purescript
newtype RequestCancelWorkflowExecutionInput
  = RequestCancelWorkflowExecutionInput { "Domain'" :: DomainName, "WorkflowId'" :: WorkflowId, "RunId'" :: NullOrUndefined (WorkflowRunIdOptional) }
```

##### Instances
``` purescript
Newtype RequestCancelWorkflowExecutionInput _
```

#### `RespondActivityTaskCanceledInput`

``` purescript
newtype RespondActivityTaskCanceledInput
  = RespondActivityTaskCanceledInput { "TaskToken'" :: TaskToken, "Details'" :: NullOrUndefined (Data) }
```

##### Instances
``` purescript
Newtype RespondActivityTaskCanceledInput _
```

#### `RespondActivityTaskCompletedInput`

``` purescript
newtype RespondActivityTaskCompletedInput
  = RespondActivityTaskCompletedInput { "TaskToken'" :: TaskToken, "Result'" :: NullOrUndefined (Data) }
```

##### Instances
``` purescript
Newtype RespondActivityTaskCompletedInput _
```

#### `RespondActivityTaskFailedInput`

``` purescript
newtype RespondActivityTaskFailedInput
  = RespondActivityTaskFailedInput { "TaskToken'" :: TaskToken, "Reason'" :: NullOrUndefined (FailureReason), "Details'" :: NullOrUndefined (Data) }
```

##### Instances
``` purescript
Newtype RespondActivityTaskFailedInput _
```

#### `RespondDecisionTaskCompletedInput`

``` purescript
newtype RespondDecisionTaskCompletedInput
  = RespondDecisionTaskCompletedInput { "TaskToken'" :: TaskToken, "Decisions'" :: NullOrUndefined (DecisionList), "ExecutionContext'" :: NullOrUndefined (Data) }
```

<p>Input data for a TaskCompleted response to a decision task.</p>

##### Instances
``` purescript
Newtype RespondDecisionTaskCompletedInput _
```

#### `ReverseOrder`

``` purescript
newtype ReverseOrder
  = ReverseOrder Boolean
```

##### Instances
``` purescript
Newtype ReverseOrder _
```

#### `Run`

``` purescript
newtype Run
  = Run { "RunId'" :: NullOrUndefined (WorkflowRunId) }
```

<p>Specifies the <code>runId</code> of a workflow execution.</p>

##### Instances
``` purescript
Newtype Run _
```

#### `ScheduleActivityTaskDecisionAttributes`

``` purescript
newtype ScheduleActivityTaskDecisionAttributes
  = ScheduleActivityTaskDecisionAttributes { "ActivityType'" :: ActivityType, "ActivityId'" :: ActivityId, "Control'" :: NullOrUndefined (Data), "Input'" :: NullOrUndefined (Data), "ScheduleToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "TaskList'" :: NullOrUndefined (TaskList), "TaskPriority'" :: NullOrUndefined (TaskPriority), "ScheduleToStartTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "HeartbeatTimeout'" :: NullOrUndefined (DurationInSecondsOptional) }
```

<p>Provides the details of the <code>ScheduleActivityTask</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>activityType.name</code> – String constraint. The key is <code>swf:activityType.name</code>.</p> </li> <li> <p> <code>activityType.version</code> – String constraint. The key is <code>swf:activityType.version</code>.</p> </li> <li> <p> <code>taskList</code> – String constraint. The key is <code>swf:taskList.name</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype ScheduleActivityTaskDecisionAttributes _
```

#### `ScheduleActivityTaskFailedCause`

``` purescript
newtype ScheduleActivityTaskFailedCause
  = ScheduleActivityTaskFailedCause String
```

##### Instances
``` purescript
Newtype ScheduleActivityTaskFailedCause _
```

#### `ScheduleActivityTaskFailedEventAttributes`

``` purescript
newtype ScheduleActivityTaskFailedEventAttributes
  = ScheduleActivityTaskFailedEventAttributes { "ActivityType'" :: ActivityType, "ActivityId'" :: ActivityId, "Cause'" :: ScheduleActivityTaskFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>ScheduleActivityTaskFailed</code> event.</p>

##### Instances
``` purescript
Newtype ScheduleActivityTaskFailedEventAttributes _
```

#### `ScheduleLambdaFunctionDecisionAttributes`

``` purescript
newtype ScheduleLambdaFunctionDecisionAttributes
  = ScheduleLambdaFunctionDecisionAttributes { "Id'" :: FunctionId, "Name'" :: FunctionName, "Control'" :: NullOrUndefined (Data), "Input'" :: NullOrUndefined (FunctionInput), "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional) }
```

<p>Decision attributes specified in <code>scheduleLambdaFunctionDecisionAttributes</code> within the list of decisions <code>decisions</code> passed to <a>RespondDecisionTaskCompleted</a>.</p>

##### Instances
``` purescript
Newtype ScheduleLambdaFunctionDecisionAttributes _
```

#### `ScheduleLambdaFunctionFailedCause`

``` purescript
newtype ScheduleLambdaFunctionFailedCause
  = ScheduleLambdaFunctionFailedCause String
```

##### Instances
``` purescript
Newtype ScheduleLambdaFunctionFailedCause _
```

#### `ScheduleLambdaFunctionFailedEventAttributes`

``` purescript
newtype ScheduleLambdaFunctionFailedEventAttributes
  = ScheduleLambdaFunctionFailedEventAttributes { "Id'" :: FunctionId, "Name'" :: FunctionName, "Cause'" :: ScheduleLambdaFunctionFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>ScheduleLambdaFunctionFailed</code> event. It isn't set for other event types.</p>

##### Instances
``` purescript
Newtype ScheduleLambdaFunctionFailedEventAttributes _
```

#### `SignalExternalWorkflowExecutionDecisionAttributes`

``` purescript
newtype SignalExternalWorkflowExecutionDecisionAttributes
  = SignalExternalWorkflowExecutionDecisionAttributes { "WorkflowId'" :: WorkflowId, "RunId'" :: NullOrUndefined (WorkflowRunIdOptional), "SignalName'" :: SignalName, "Input'" :: NullOrUndefined (Data), "Control'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>SignalExternalWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype SignalExternalWorkflowExecutionDecisionAttributes _
```

#### `SignalExternalWorkflowExecutionFailedCause`

``` purescript
newtype SignalExternalWorkflowExecutionFailedCause
  = SignalExternalWorkflowExecutionFailedCause String
```

##### Instances
``` purescript
Newtype SignalExternalWorkflowExecutionFailedCause _
```

#### `SignalExternalWorkflowExecutionFailedEventAttributes`

``` purescript
newtype SignalExternalWorkflowExecutionFailedEventAttributes
  = SignalExternalWorkflowExecutionFailedEventAttributes { "WorkflowId'" :: WorkflowId, "RunId'" :: NullOrUndefined (WorkflowRunIdOptional), "Cause'" :: SignalExternalWorkflowExecutionFailedCause, "InitiatedEventId'" :: EventId, "DecisionTaskCompletedEventId'" :: EventId, "Control'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>SignalExternalWorkflowExecutionFailed</code> event.</p>

##### Instances
``` purescript
Newtype SignalExternalWorkflowExecutionFailedEventAttributes _
```

#### `SignalExternalWorkflowExecutionInitiatedEventAttributes`

``` purescript
newtype SignalExternalWorkflowExecutionInitiatedEventAttributes
  = SignalExternalWorkflowExecutionInitiatedEventAttributes { "WorkflowId'" :: WorkflowId, "RunId'" :: NullOrUndefined (WorkflowRunIdOptional), "SignalName'" :: SignalName, "Input'" :: NullOrUndefined (Data), "DecisionTaskCompletedEventId'" :: EventId, "Control'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>SignalExternalWorkflowExecutionInitiated</code> event.</p>

##### Instances
``` purescript
Newtype SignalExternalWorkflowExecutionInitiatedEventAttributes _
```

#### `SignalName`

``` purescript
newtype SignalName
  = SignalName String
```

##### Instances
``` purescript
Newtype SignalName _
```

#### `SignalWorkflowExecutionInput`

``` purescript
newtype SignalWorkflowExecutionInput
  = SignalWorkflowExecutionInput { "Domain'" :: DomainName, "WorkflowId'" :: WorkflowId, "RunId'" :: NullOrUndefined (WorkflowRunIdOptional), "SignalName'" :: SignalName, "Input'" :: NullOrUndefined (Data) }
```

##### Instances
``` purescript
Newtype SignalWorkflowExecutionInput _
```

#### `StartChildWorkflowExecutionDecisionAttributes`

``` purescript
newtype StartChildWorkflowExecutionDecisionAttributes
  = StartChildWorkflowExecutionDecisionAttributes { "WorkflowType'" :: WorkflowType, "WorkflowId'" :: WorkflowId, "Control'" :: NullOrUndefined (Data), "Input'" :: NullOrUndefined (Data), "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "TaskList'" :: NullOrUndefined (TaskList), "TaskPriority'" :: NullOrUndefined (TaskPriority), "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "ChildPolicy'" :: NullOrUndefined (ChildPolicy), "TagList'" :: NullOrUndefined (TagList), "LambdaRole'" :: NullOrUndefined (Arn) }
```

<p>Provides the details of the <code>StartChildWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagList.member.N</code> – The key is "swf:tagList.N" where N is the tag number from 0 to 4, inclusive.</p> </li> <li> <p> <code>taskList</code> – String constraint. The key is <code>swf:taskList.name</code>.</p> </li> <li> <p> <code>workflowType.name</code> – String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code> – String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype StartChildWorkflowExecutionDecisionAttributes _
```

#### `StartChildWorkflowExecutionFailedCause`

``` purescript
newtype StartChildWorkflowExecutionFailedCause
  = StartChildWorkflowExecutionFailedCause String
```

##### Instances
``` purescript
Newtype StartChildWorkflowExecutionFailedCause _
```

#### `StartChildWorkflowExecutionFailedEventAttributes`

``` purescript
newtype StartChildWorkflowExecutionFailedEventAttributes
  = StartChildWorkflowExecutionFailedEventAttributes { "WorkflowType'" :: WorkflowType, "Cause'" :: StartChildWorkflowExecutionFailedCause, "WorkflowId'" :: WorkflowId, "InitiatedEventId'" :: EventId, "DecisionTaskCompletedEventId'" :: EventId, "Control'" :: NullOrUndefined (Data) }
```

<p>Provides the details of the <code>StartChildWorkflowExecutionFailed</code> event.</p>

##### Instances
``` purescript
Newtype StartChildWorkflowExecutionFailedEventAttributes _
```

#### `StartChildWorkflowExecutionInitiatedEventAttributes`

``` purescript
newtype StartChildWorkflowExecutionInitiatedEventAttributes
  = StartChildWorkflowExecutionInitiatedEventAttributes { "WorkflowId'" :: WorkflowId, "WorkflowType'" :: WorkflowType, "Control'" :: NullOrUndefined (Data), "Input'" :: NullOrUndefined (Data), "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "TaskList'" :: TaskList, "TaskPriority'" :: NullOrUndefined (TaskPriority), "DecisionTaskCompletedEventId'" :: EventId, "ChildPolicy'" :: ChildPolicy, "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "TagList'" :: NullOrUndefined (TagList), "LambdaRole'" :: NullOrUndefined (Arn) }
```

<p>Provides the details of the <code>StartChildWorkflowExecutionInitiated</code> event.</p>

##### Instances
``` purescript
Newtype StartChildWorkflowExecutionInitiatedEventAttributes _
```

#### `StartLambdaFunctionFailedCause`

``` purescript
newtype StartLambdaFunctionFailedCause
  = StartLambdaFunctionFailedCause String
```

##### Instances
``` purescript
Newtype StartLambdaFunctionFailedCause _
```

#### `StartLambdaFunctionFailedEventAttributes`

``` purescript
newtype StartLambdaFunctionFailedEventAttributes
  = StartLambdaFunctionFailedEventAttributes { "ScheduledEventId'" :: NullOrUndefined (EventId), "Cause'" :: NullOrUndefined (StartLambdaFunctionFailedCause), "Message'" :: NullOrUndefined (CauseMessage) }
```

<p>Provides the details of the <code>StartLambdaFunctionFailed</code> event. It isn't set for other event types.</p>

##### Instances
``` purescript
Newtype StartLambdaFunctionFailedEventAttributes _
```

#### `StartTimerDecisionAttributes`

``` purescript
newtype StartTimerDecisionAttributes
  = StartTimerDecisionAttributes { "TimerId'" :: TimerId, "Control'" :: NullOrUndefined (Data), "StartToFireTimeout'" :: DurationInSeconds }
```

<p>Provides the details of the <code>StartTimer</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>

##### Instances
``` purescript
Newtype StartTimerDecisionAttributes _
```

#### `StartTimerFailedCause`

``` purescript
newtype StartTimerFailedCause
  = StartTimerFailedCause String
```

##### Instances
``` purescript
Newtype StartTimerFailedCause _
```

#### `StartTimerFailedEventAttributes`

``` purescript
newtype StartTimerFailedEventAttributes
  = StartTimerFailedEventAttributes { "TimerId'" :: TimerId, "Cause'" :: StartTimerFailedCause, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>StartTimerFailed</code> event.</p>

##### Instances
``` purescript
Newtype StartTimerFailedEventAttributes _
```

#### `StartWorkflowExecutionInput`

``` purescript
newtype StartWorkflowExecutionInput
  = StartWorkflowExecutionInput { "Domain'" :: DomainName, "WorkflowId'" :: WorkflowId, "WorkflowType'" :: WorkflowType, "TaskList'" :: NullOrUndefined (TaskList), "TaskPriority'" :: NullOrUndefined (TaskPriority), "Input'" :: NullOrUndefined (Data), "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "TagList'" :: NullOrUndefined (TagList), "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "ChildPolicy'" :: NullOrUndefined (ChildPolicy), "LambdaRole'" :: NullOrUndefined (Arn) }
```

##### Instances
``` purescript
Newtype StartWorkflowExecutionInput _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag String
```

##### Instances
``` purescript
Newtype Tag _
```

#### `TagFilter`

``` purescript
newtype TagFilter
  = TagFilter { "Tag'" :: Tag }
```

<p>Used to filter the workflow executions in visibility APIs based on a tag.</p>

##### Instances
``` purescript
Newtype TagFilter _
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

#### `TaskList`

``` purescript
newtype TaskList
  = TaskList { "Name'" :: Name }
```

<p>Represents a task list.</p>

##### Instances
``` purescript
Newtype TaskList _
```

#### `TaskPriority`

``` purescript
newtype TaskPriority
  = TaskPriority String
```

##### Instances
``` purescript
Newtype TaskPriority _
```

#### `TaskToken`

``` purescript
newtype TaskToken
  = TaskToken String
```

##### Instances
``` purescript
Newtype TaskToken _
```

#### `TerminateReason`

``` purescript
newtype TerminateReason
  = TerminateReason String
```

##### Instances
``` purescript
Newtype TerminateReason _
```

#### `TerminateWorkflowExecutionInput`

``` purescript
newtype TerminateWorkflowExecutionInput
  = TerminateWorkflowExecutionInput { "Domain'" :: DomainName, "WorkflowId'" :: WorkflowId, "RunId'" :: NullOrUndefined (WorkflowRunIdOptional), "Reason'" :: NullOrUndefined (TerminateReason), "Details'" :: NullOrUndefined (Data), "ChildPolicy'" :: NullOrUndefined (ChildPolicy) }
```

##### Instances
``` purescript
Newtype TerminateWorkflowExecutionInput _
```

#### `TimerCanceledEventAttributes`

``` purescript
newtype TimerCanceledEventAttributes
  = TimerCanceledEventAttributes { "TimerId'" :: TimerId, "StartedEventId'" :: EventId, "DecisionTaskCompletedEventId'" :: EventId }
```

<p> Provides the details of the <code>TimerCanceled</code> event. </p>

##### Instances
``` purescript
Newtype TimerCanceledEventAttributes _
```

#### `TimerFiredEventAttributes`

``` purescript
newtype TimerFiredEventAttributes
  = TimerFiredEventAttributes { "TimerId'" :: TimerId, "StartedEventId'" :: EventId }
```

<p>Provides the details of the <code>TimerFired</code> event.</p>

##### Instances
``` purescript
Newtype TimerFiredEventAttributes _
```

#### `TimerId`

``` purescript
newtype TimerId
  = TimerId String
```

##### Instances
``` purescript
Newtype TimerId _
```

#### `TimerStartedEventAttributes`

``` purescript
newtype TimerStartedEventAttributes
  = TimerStartedEventAttributes { "TimerId'" :: TimerId, "Control'" :: NullOrUndefined (Data), "StartToFireTimeout'" :: DurationInSeconds, "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>TimerStarted</code> event.</p>

##### Instances
``` purescript
Newtype TimerStartedEventAttributes _
```

#### `Truncated`

``` purescript
newtype Truncated
  = Truncated Boolean
```

##### Instances
``` purescript
Newtype Truncated _
```

#### `TypeAlreadyExistsFault`

``` purescript
newtype TypeAlreadyExistsFault
  = TypeAlreadyExistsFault { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Returned if the type already exists in the specified domain. You get this fault even if the existing type is in deprecated status. You can specify another version if the intent is to create a new distinct version of the type.</p>

##### Instances
``` purescript
Newtype TypeAlreadyExistsFault _
```

#### `TypeDeprecatedFault`

``` purescript
newtype TypeDeprecatedFault
  = TypeDeprecatedFault { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Returned when the specified activity or workflow type was already deprecated.</p>

##### Instances
``` purescript
Newtype TypeDeprecatedFault _
```

#### `UnknownResourceFault`

``` purescript
newtype UnknownResourceFault
  = UnknownResourceFault { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Returned when the named resource cannot be found with in the scope of this operation (region or domain). This could happen if the named resource was never created or is no longer available for this operation.</p>

##### Instances
``` purescript
Newtype UnknownResourceFault _
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

#### `VersionOptional`

``` purescript
newtype VersionOptional
  = VersionOptional String
```

##### Instances
``` purescript
Newtype VersionOptional _
```

#### `WorkflowExecution`

``` purescript
newtype WorkflowExecution
  = WorkflowExecution { "WorkflowId'" :: WorkflowId, "RunId'" :: WorkflowRunId }
```

<p>Represents a workflow execution.</p>

##### Instances
``` purescript
Newtype WorkflowExecution _
```

#### `WorkflowExecutionAlreadyStartedFault`

``` purescript
newtype WorkflowExecutionAlreadyStartedFault
  = WorkflowExecutionAlreadyStartedFault { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Returned by <a>StartWorkflowExecution</a> when an open execution with the same workflowId is already running in the specified domain.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionAlreadyStartedFault _
```

#### `WorkflowExecutionCancelRequestedCause`

``` purescript
newtype WorkflowExecutionCancelRequestedCause
  = WorkflowExecutionCancelRequestedCause String
```

##### Instances
``` purescript
Newtype WorkflowExecutionCancelRequestedCause _
```

#### `WorkflowExecutionCancelRequestedEventAttributes`

``` purescript
newtype WorkflowExecutionCancelRequestedEventAttributes
  = WorkflowExecutionCancelRequestedEventAttributes { "ExternalWorkflowExecution'" :: NullOrUndefined (WorkflowExecution), "ExternalInitiatedEventId'" :: NullOrUndefined (EventId), "Cause'" :: NullOrUndefined (WorkflowExecutionCancelRequestedCause) }
```

<p>Provides the details of the <code>WorkflowExecutionCancelRequested</code> event.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionCancelRequestedEventAttributes _
```

#### `WorkflowExecutionCanceledEventAttributes`

``` purescript
newtype WorkflowExecutionCanceledEventAttributes
  = WorkflowExecutionCanceledEventAttributes { "Details'" :: NullOrUndefined (Data), "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>WorkflowExecutionCanceled</code> event.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionCanceledEventAttributes _
```

#### `WorkflowExecutionCompletedEventAttributes`

``` purescript
newtype WorkflowExecutionCompletedEventAttributes
  = WorkflowExecutionCompletedEventAttributes { "Result'" :: NullOrUndefined (Data), "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>WorkflowExecutionCompleted</code> event.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionCompletedEventAttributes _
```

#### `WorkflowExecutionConfiguration`

``` purescript
newtype WorkflowExecutionConfiguration
  = WorkflowExecutionConfiguration { "TaskStartToCloseTimeout'" :: DurationInSeconds, "ExecutionStartToCloseTimeout'" :: DurationInSeconds, "TaskList'" :: TaskList, "TaskPriority'" :: NullOrUndefined (TaskPriority), "ChildPolicy'" :: ChildPolicy, "LambdaRole'" :: NullOrUndefined (Arn) }
```

<p>The configuration settings for a workflow execution including timeout values, tasklist etc. These configuration settings are determined from the defaults specified when registering the workflow type and those specified when starting the workflow execution.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionConfiguration _
```

#### `WorkflowExecutionContinuedAsNewEventAttributes`

``` purescript
newtype WorkflowExecutionContinuedAsNewEventAttributes
  = WorkflowExecutionContinuedAsNewEventAttributes { "Input'" :: NullOrUndefined (Data), "DecisionTaskCompletedEventId'" :: EventId, "NewExecutionRunId'" :: WorkflowRunId, "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "TaskList'" :: TaskList, "TaskPriority'" :: NullOrUndefined (TaskPriority), "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "ChildPolicy'" :: ChildPolicy, "TagList'" :: NullOrUndefined (TagList), "WorkflowType'" :: WorkflowType, "LambdaRole'" :: NullOrUndefined (Arn) }
```

<p>Provides the details of the <code>WorkflowExecutionContinuedAsNew</code> event.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionContinuedAsNewEventAttributes _
```

#### `WorkflowExecutionCount`

``` purescript
newtype WorkflowExecutionCount
  = WorkflowExecutionCount { "Count'" :: Count, "Truncated'" :: NullOrUndefined (Truncated) }
```

<p>Contains the count of workflow executions returned from <a>CountOpenWorkflowExecutions</a> or <a>CountClosedWorkflowExecutions</a> </p>

##### Instances
``` purescript
Newtype WorkflowExecutionCount _
```

#### `WorkflowExecutionDetail`

``` purescript
newtype WorkflowExecutionDetail
  = WorkflowExecutionDetail { "ExecutionInfo'" :: WorkflowExecutionInfo, "ExecutionConfiguration'" :: WorkflowExecutionConfiguration, "OpenCounts'" :: WorkflowExecutionOpenCounts, "LatestActivityTaskTimestamp'" :: NullOrUndefined (Number), "LatestExecutionContext'" :: NullOrUndefined (Data) }
```

<p>Contains details about a workflow execution.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionDetail _
```

#### `WorkflowExecutionFailedEventAttributes`

``` purescript
newtype WorkflowExecutionFailedEventAttributes
  = WorkflowExecutionFailedEventAttributes { "Reason'" :: NullOrUndefined (FailureReason), "Details'" :: NullOrUndefined (Data), "DecisionTaskCompletedEventId'" :: EventId }
```

<p>Provides the details of the <code>WorkflowExecutionFailed</code> event.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionFailedEventAttributes _
```

#### `WorkflowExecutionFilter`

``` purescript
newtype WorkflowExecutionFilter
  = WorkflowExecutionFilter { "WorkflowId'" :: WorkflowId }
```

<p>Used to filter the workflow executions in visibility APIs by their <code>workflowId</code>.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionFilter _
```

#### `WorkflowExecutionInfo`

``` purescript
newtype WorkflowExecutionInfo
  = WorkflowExecutionInfo { "Execution'" :: WorkflowExecution, "WorkflowType'" :: WorkflowType, "StartTimestamp'" :: Number, "CloseTimestamp'" :: NullOrUndefined (Number), "ExecutionStatus'" :: ExecutionStatus, "CloseStatus'" :: NullOrUndefined (CloseStatus), "Parent'" :: NullOrUndefined (WorkflowExecution), "TagList'" :: NullOrUndefined (TagList), "CancelRequested'" :: NullOrUndefined (Canceled) }
```

<p>Contains information about a workflow execution.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionInfo _
```

#### `WorkflowExecutionInfoList`

``` purescript
newtype WorkflowExecutionInfoList
  = WorkflowExecutionInfoList (Array WorkflowExecutionInfo)
```

##### Instances
``` purescript
Newtype WorkflowExecutionInfoList _
```

#### `WorkflowExecutionInfos`

``` purescript
newtype WorkflowExecutionInfos
  = WorkflowExecutionInfos { "ExecutionInfos'" :: WorkflowExecutionInfoList, "NextPageToken'" :: NullOrUndefined (PageToken) }
```

<p>Contains a paginated list of information about workflow executions.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionInfos _
```

#### `WorkflowExecutionOpenCounts`

``` purescript
newtype WorkflowExecutionOpenCounts
  = WorkflowExecutionOpenCounts { "OpenActivityTasks'" :: Count, "OpenDecisionTasks'" :: OpenDecisionTasksCount, "OpenTimers'" :: Count, "OpenChildWorkflowExecutions'" :: Count, "OpenLambdaFunctions'" :: NullOrUndefined (Count) }
```

<p>Contains the counts of open tasks, child workflow executions and timers for a workflow execution.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionOpenCounts _
```

#### `WorkflowExecutionSignaledEventAttributes`

``` purescript
newtype WorkflowExecutionSignaledEventAttributes
  = WorkflowExecutionSignaledEventAttributes { "SignalName'" :: SignalName, "Input'" :: NullOrUndefined (Data), "ExternalWorkflowExecution'" :: NullOrUndefined (WorkflowExecution), "ExternalInitiatedEventId'" :: NullOrUndefined (EventId) }
```

<p>Provides the details of the <code>WorkflowExecutionSignaled</code> event.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionSignaledEventAttributes _
```

#### `WorkflowExecutionStartedEventAttributes`

``` purescript
newtype WorkflowExecutionStartedEventAttributes
  = WorkflowExecutionStartedEventAttributes { "Input'" :: NullOrUndefined (Data), "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "ChildPolicy'" :: ChildPolicy, "TaskList'" :: TaskList, "TaskPriority'" :: NullOrUndefined (TaskPriority), "WorkflowType'" :: WorkflowType, "TagList'" :: NullOrUndefined (TagList), "ContinuedExecutionRunId'" :: NullOrUndefined (WorkflowRunIdOptional), "ParentWorkflowExecution'" :: NullOrUndefined (WorkflowExecution), "ParentInitiatedEventId'" :: NullOrUndefined (EventId), "LambdaRole'" :: NullOrUndefined (Arn) }
```

<p>Provides details of <code>WorkflowExecutionStarted</code> event.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionStartedEventAttributes _
```

#### `WorkflowExecutionTerminatedCause`

``` purescript
newtype WorkflowExecutionTerminatedCause
  = WorkflowExecutionTerminatedCause String
```

##### Instances
``` purescript
Newtype WorkflowExecutionTerminatedCause _
```

#### `WorkflowExecutionTerminatedEventAttributes`

``` purescript
newtype WorkflowExecutionTerminatedEventAttributes
  = WorkflowExecutionTerminatedEventAttributes { "Reason'" :: NullOrUndefined (TerminateReason), "Details'" :: NullOrUndefined (Data), "ChildPolicy'" :: ChildPolicy, "Cause'" :: NullOrUndefined (WorkflowExecutionTerminatedCause) }
```

<p>Provides the details of the <code>WorkflowExecutionTerminated</code> event.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionTerminatedEventAttributes _
```

#### `WorkflowExecutionTimedOutEventAttributes`

``` purescript
newtype WorkflowExecutionTimedOutEventAttributes
  = WorkflowExecutionTimedOutEventAttributes { "TimeoutType'" :: WorkflowExecutionTimeoutType, "ChildPolicy'" :: ChildPolicy }
```

<p>Provides the details of the <code>WorkflowExecutionTimedOut</code> event.</p>

##### Instances
``` purescript
Newtype WorkflowExecutionTimedOutEventAttributes _
```

#### `WorkflowExecutionTimeoutType`

``` purescript
newtype WorkflowExecutionTimeoutType
  = WorkflowExecutionTimeoutType String
```

##### Instances
``` purescript
Newtype WorkflowExecutionTimeoutType _
```

#### `WorkflowId`

``` purescript
newtype WorkflowId
  = WorkflowId String
```

##### Instances
``` purescript
Newtype WorkflowId _
```

#### `WorkflowRunId`

``` purescript
newtype WorkflowRunId
  = WorkflowRunId String
```

##### Instances
``` purescript
Newtype WorkflowRunId _
```

#### `WorkflowRunIdOptional`

``` purescript
newtype WorkflowRunIdOptional
  = WorkflowRunIdOptional String
```

##### Instances
``` purescript
Newtype WorkflowRunIdOptional _
```

#### `WorkflowType`

``` purescript
newtype WorkflowType
  = WorkflowType { "Name'" :: Name, "Version'" :: Version }
```

<p>Represents a workflow type.</p>

##### Instances
``` purescript
Newtype WorkflowType _
```

#### `WorkflowTypeConfiguration`

``` purescript
newtype WorkflowTypeConfiguration
  = WorkflowTypeConfiguration { "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional), "DefaultTaskList'" :: NullOrUndefined (TaskList), "DefaultTaskPriority'" :: NullOrUndefined (TaskPriority), "DefaultChildPolicy'" :: NullOrUndefined (ChildPolicy), "DefaultLambdaRole'" :: NullOrUndefined (Arn) }
```

<p>The configuration settings of a workflow type.</p>

##### Instances
``` purescript
Newtype WorkflowTypeConfiguration _
```

#### `WorkflowTypeDetail`

``` purescript
newtype WorkflowTypeDetail
  = WorkflowTypeDetail { "TypeInfo'" :: WorkflowTypeInfo, "Configuration'" :: WorkflowTypeConfiguration }
```

<p>Contains details about a workflow type.</p>

##### Instances
``` purescript
Newtype WorkflowTypeDetail _
```

#### `WorkflowTypeFilter`

``` purescript
newtype WorkflowTypeFilter
  = WorkflowTypeFilter { "Name'" :: Name, "Version'" :: NullOrUndefined (VersionOptional) }
```

<p>Used to filter workflow execution query results by type. Each parameter, if specified, defines a rule that must be satisfied by each returned result.</p>

##### Instances
``` purescript
Newtype WorkflowTypeFilter _
```

#### `WorkflowTypeInfo`

``` purescript
newtype WorkflowTypeInfo
  = WorkflowTypeInfo { "WorkflowType'" :: WorkflowType, "Status'" :: RegistrationStatus, "Description'" :: NullOrUndefined (Description), "CreationDate'" :: Number, "DeprecationDate'" :: NullOrUndefined (Number) }
```

<p>Contains information about a workflow type.</p>

##### Instances
``` purescript
Newtype WorkflowTypeInfo _
```

#### `WorkflowTypeInfoList`

``` purescript
newtype WorkflowTypeInfoList
  = WorkflowTypeInfoList (Array WorkflowTypeInfo)
```

##### Instances
``` purescript
Newtype WorkflowTypeInfoList _
```

#### `WorkflowTypeInfos`

``` purescript
newtype WorkflowTypeInfos
  = WorkflowTypeInfos { "TypeInfos'" :: WorkflowTypeInfoList, "NextPageToken'" :: NullOrUndefined (PageToken) }
```

<p>Contains a paginated list of information structures about workflow types.</p>

##### Instances
``` purescript
Newtype WorkflowTypeInfos _
```


