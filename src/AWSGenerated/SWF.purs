

-- | <fullname>Amazon Simple Workflow Service</fullname> <p>The Amazon Simple Workflow Service (Amazon SWF) makes it easy to build applications that use Amazon's cloud to coordinate work across distributed components. In Amazon SWF, a <i>task</i> represents a logical unit of work that is performed by a component of your workflow. Coordinating tasks in a workflow involves managing intertask dependencies, scheduling, and concurrency in accordance with the logical flow of the application.</p> <p>Amazon SWF gives you full control over implementing tasks and coordinating them without worrying about underlying complexities such as tracking their progress and maintaining their state.</p> <p>This documentation serves as reference only. For a broader overview of the Amazon SWF programming model, see the <i> <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/">Amazon SWF Developer Guide</a> </i>.</p>
module AWS.SWF where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "SWF" :: String


-- | <p>Returns the number of closed workflow executions within the given domain that meet the specified filtering criteria.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
countClosedWorkflowExecutions :: forall eff. CountClosedWorkflowExecutionsInput -> Aff (err :: AWS.RequestError | eff) WorkflowExecutionCount
countClosedWorkflowExecutions = AWS.request serviceName "countClosedWorkflowExecutions" 


-- | <p>Returns the number of open workflow executions within the given domain that meet the specified filtering criteria.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
countOpenWorkflowExecutions :: forall eff. CountOpenWorkflowExecutionsInput -> Aff (err :: AWS.RequestError | eff) WorkflowExecutionCount
countOpenWorkflowExecutions = AWS.request serviceName "countOpenWorkflowExecutions" 


-- | <p>Returns the estimated number of activity tasks in the specified task list. The count returned is an approximation and isn't guaranteed to be exact. If you specify a task list that no activity task was ever scheduled in then <code>0</code> is returned.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
countPendingActivityTasks :: forall eff. CountPendingActivityTasksInput -> Aff (err :: AWS.RequestError | eff) PendingTaskCount
countPendingActivityTasks = AWS.request serviceName "countPendingActivityTasks" 


-- | <p>Returns the estimated number of decision tasks in the specified task list. The count returned is an approximation and isn't guaranteed to be exact. If you specify a task list that no decision task was ever scheduled in then <code>0</code> is returned.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
countPendingDecisionTasks :: forall eff. CountPendingDecisionTasksInput -> Aff (err :: AWS.RequestError | eff) PendingTaskCount
countPendingDecisionTasks = AWS.request serviceName "countPendingDecisionTasks" 


-- | <p>Deprecates the specified <i>activity type</i>. After an activity type has been deprecated, you cannot create new tasks of that activity type. Tasks of this type that were scheduled before the type was deprecated continue to run.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>activityType.name</code>: String constraint. The key is <code>swf:activityType.name</code>.</p> </li> <li> <p> <code>activityType.version</code>: String constraint. The key is <code>swf:activityType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
deprecateActivityType :: forall eff. DeprecateActivityTypeInput -> Aff (err :: AWS.RequestError | eff) Unit
deprecateActivityType = AWS.request serviceName "deprecateActivityType" 


-- | <p>Deprecates the specified domain. After a domain has been deprecated it cannot be used to create new workflow executions or register new types. However, you can still use visibility actions on this domain. Deprecating a domain also deprecates all activity and workflow types registered in the domain. Executions that were started before the domain was deprecated continues to run.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
deprecateDomain :: forall eff. DeprecateDomainInput -> Aff (err :: AWS.RequestError | eff) Unit
deprecateDomain = AWS.request serviceName "deprecateDomain" 


-- | <p>Deprecates the specified <i>workflow type</i>. After a workflow type has been deprecated, you cannot create new executions of that type. Executions that were started before the type was deprecated continues to run. A deprecated workflow type may still be used when calling visibility actions.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>workflowType.name</code>: String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code>: String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
deprecateWorkflowType :: forall eff. DeprecateWorkflowTypeInput -> Aff (err :: AWS.RequestError | eff) Unit
deprecateWorkflowType = AWS.request serviceName "deprecateWorkflowType" 


-- | <p>Returns information about the specified activity type. This includes configuration settings provided when the type was registered and other general information about the type.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>activityType.name</code>: String constraint. The key is <code>swf:activityType.name</code>.</p> </li> <li> <p> <code>activityType.version</code>: String constraint. The key is <code>swf:activityType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
describeActivityType :: forall eff. DescribeActivityTypeInput -> Aff (err :: AWS.RequestError | eff) ActivityTypeDetail
describeActivityType = AWS.request serviceName "describeActivityType" 


-- | <p>Returns information about the specified domain, including description and status.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
describeDomain :: forall eff. DescribeDomainInput -> Aff (err :: AWS.RequestError | eff) DomainDetail
describeDomain = AWS.request serviceName "describeDomain" 


-- | <p>Returns information about the specified workflow execution including its type and some statistics.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
describeWorkflowExecution :: forall eff. DescribeWorkflowExecutionInput -> Aff (err :: AWS.RequestError | eff) WorkflowExecutionDetail
describeWorkflowExecution = AWS.request serviceName "describeWorkflowExecution" 


-- | <p>Returns information about the specified <i>workflow type</i>. This includes configuration settings specified when the type was registered and other information such as creation date, current status, etc.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>workflowType.name</code>: String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code>: String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
describeWorkflowType :: forall eff. DescribeWorkflowTypeInput -> Aff (err :: AWS.RequestError | eff) WorkflowTypeDetail
describeWorkflowType = AWS.request serviceName "describeWorkflowType" 


-- | <p>Returns the history of the specified workflow execution. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the <code>nextPageToken</code> returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
getWorkflowExecutionHistory :: forall eff. GetWorkflowExecutionHistoryInput -> Aff (err :: AWS.RequestError | eff) History
getWorkflowExecutionHistory = AWS.request serviceName "getWorkflowExecutionHistory" 


-- | <p>Returns information about all activities registered in the specified domain that match the specified name and registration status. The result includes information like creation date, current status of the activity, etc. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the <code>nextPageToken</code> returned by the initial call.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listActivityTypes :: forall eff. ListActivityTypesInput -> Aff (err :: AWS.RequestError | eff) ActivityTypeInfos
listActivityTypes = AWS.request serviceName "listActivityTypes" 


-- | <p>Returns a list of closed workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listClosedWorkflowExecutions :: forall eff. ListClosedWorkflowExecutionsInput -> Aff (err :: AWS.RequestError | eff) WorkflowExecutionInfos
listClosedWorkflowExecutions = AWS.request serviceName "listClosedWorkflowExecutions" 


-- | <p>Returns the list of domains registered in the account. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains. The element must be set to <code>arn:aws:swf::AccountID:domain/*</code>, where <i>AccountID</i> is the account ID, with no dashes.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listDomains :: forall eff. ListDomainsInput -> Aff (err :: AWS.RequestError | eff) DomainInfos
listDomains = AWS.request serviceName "listDomains" 


-- | <p>Returns a list of open workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listOpenWorkflowExecutions :: forall eff. ListOpenWorkflowExecutionsInput -> Aff (err :: AWS.RequestError | eff) WorkflowExecutionInfos
listOpenWorkflowExecutions = AWS.request serviceName "listOpenWorkflowExecutions" 


-- | <p>Returns information about workflow types in the specified domain. The results may be split into multiple pages that can be retrieved by making the call repeatedly.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listWorkflowTypes :: forall eff. ListWorkflowTypesInput -> Aff (err :: AWS.RequestError | eff) WorkflowTypeInfos
listWorkflowTypes = AWS.request serviceName "listWorkflowTypes" 


-- | <p>Used by workers to get an <a>ActivityTask</a> from the specified activity <code>taskList</code>. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available. The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns an empty result. An empty result, in this context, means that an ActivityTask is returned, but that the value of taskToken is an empty string. If a task is returned, the worker should use its type to identify and process it correctly.</p> <important> <p>Workers should set their client side socket timeout to at least 70 seconds (10 seconds higher than the maximum time service may hold the poll request).</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
pollForActivityTask :: forall eff. PollForActivityTaskInput -> Aff (err :: AWS.RequestError | eff) ActivityTask
pollForActivityTask = AWS.request serviceName "pollForActivityTask" 


-- | <p>Used by deciders to get a <a>DecisionTask</a> from the specified decision <code>taskList</code>. A decision task may be returned for any open workflow execution that is using the specified task list. The task includes a paginated view of the history of the workflow execution. The decider should use the workflow type and the history to determine how to properly handle the task.</p> <p>This action initiates a long poll, where the service holds the HTTP connection open and responds as soon a task becomes available. If no decision task is available in the specified task list before the timeout of 60 seconds expires, an empty result is returned. An empty result, in this context, means that a DecisionTask is returned, but that the value of taskToken is an empty string.</p> <important> <p>Deciders should set their client side socket timeout to at least 70 seconds (10 seconds higher than the timeout).</p> </important> <important> <p>Because the number of workflow history events for a single workflow execution might be very large, the result returned might be split up across a number of pages. To retrieve subsequent pages, make additional calls to <code>PollForDecisionTask</code> using the <code>nextPageToken</code> returned by the initial call. Note that you do <i>not</i> call <code>GetWorkflowExecutionHistory</code> with this <code>nextPageToken</code>. Instead, call <code>PollForDecisionTask</code> again.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
pollForDecisionTask :: forall eff. PollForDecisionTaskInput -> Aff (err :: AWS.RequestError | eff) DecisionTask
pollForDecisionTask = AWS.request serviceName "pollForDecisionTask" 


-- | <p>Used by activity workers to report to the service that the <a>ActivityTask</a> represented by the specified <code>taskToken</code> is still making progress. The worker can also specify details of the progress, for example percent complete, using the <code>details</code> parameter. This action can also be used by the worker as a mechanism to check if cancellation is being requested for the activity task. If a cancellation is being attempted for the specified task, then the boolean <code>cancelRequested</code> flag returned by the service is set to <code>true</code>.</p> <p>This action resets the <code>taskHeartbeatTimeout</code> clock. The <code>taskHeartbeatTimeout</code> is specified in <a>RegisterActivityType</a>.</p> <p>This action doesn't in itself create an event in the workflow execution history. However, if the task times out, the workflow execution history contains a <code>ActivityTaskTimedOut</code> event that contains the information from the last heartbeat generated by the activity worker.</p> <note> <p>The <code>taskStartToCloseTimeout</code> of an activity type is the maximum duration of an activity task, regardless of the number of <a>RecordActivityTaskHeartbeat</a> requests received. The <code>taskStartToCloseTimeout</code> is also specified in <a>RegisterActivityType</a>.</p> </note> <note> <p>This operation is only useful for long-lived activities to report liveliness of the task and to determine if a cancellation is being attempted.</p> </note> <important> <p>If the <code>cancelRequested</code> flag returns <code>true</code>, a cancellation is being attempted. If the worker can cancel the activity, it should respond with <a>RespondActivityTaskCanceled</a>. Otherwise, it should ignore the cancellation request.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
recordActivityTaskHeartbeat :: forall eff. RecordActivityTaskHeartbeatInput -> Aff (err :: AWS.RequestError | eff) ActivityTaskStatus
recordActivityTaskHeartbeat = AWS.request serviceName "recordActivityTaskHeartbeat" 


-- | <p>Registers a new <i>activity type</i> along with its configuration settings in the specified domain.</p> <important> <p>A <code>TypeAlreadyExists</code> fault is returned if the type already exists in the domain. You cannot change any configuration settings of the type after its registration, and it must be registered as a new version.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>defaultTaskList.name</code>: String constraint. The key is <code>swf:defaultTaskList.name</code>.</p> </li> <li> <p> <code>name</code>: String constraint. The key is <code>swf:name</code>.</p> </li> <li> <p> <code>version</code>: String constraint. The key is <code>swf:version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
registerActivityType :: forall eff. RegisterActivityTypeInput -> Aff (err :: AWS.RequestError | eff) Unit
registerActivityType = AWS.request serviceName "registerActivityType" 


-- | <p>Registers a new domain.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>You cannot use an IAM policy to control domain access for this action. The name of the domain being registered is available as the resource of this action.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
registerDomain :: forall eff. RegisterDomainInput -> Aff (err :: AWS.RequestError | eff) Unit
registerDomain = AWS.request serviceName "registerDomain" 


-- | <p>Registers a new <i>workflow type</i> and its configuration settings in the specified domain.</p> <p>The retention period for the workflow history is set by the <a>RegisterDomain</a> action.</p> <important> <p>If the type already exists, then a <code>TypeAlreadyExists</code> fault is returned. You cannot change the configuration settings of a workflow type once it is registered and it must be registered as a new version.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>defaultTaskList.name</code>: String constraint. The key is <code>swf:defaultTaskList.name</code>.</p> </li> <li> <p> <code>name</code>: String constraint. The key is <code>swf:name</code>.</p> </li> <li> <p> <code>version</code>: String constraint. The key is <code>swf:version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
registerWorkflowType :: forall eff. RegisterWorkflowTypeInput -> Aff (err :: AWS.RequestError | eff) Unit
registerWorkflowType = AWS.request serviceName "registerWorkflowType" 


-- | <p>Records a <code>WorkflowExecutionCancelRequested</code> event in the currently running workflow execution identified by the given domain, workflowId, and runId. This logically requests the cancellation of the workflow execution as a whole. It is up to the decider to take appropriate actions when it receives an execution history with this event.</p> <note> <p>If the runId isn't specified, the <code>WorkflowExecutionCancelRequested</code> event is recorded in the history of the current open workflow execution with the specified workflowId in the domain.</p> </note> <note> <p>Because this action allows the workflow to properly clean up and gracefully close, it should be used instead of <a>TerminateWorkflowExecution</a> when possible.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
requestCancelWorkflowExecution :: forall eff. RequestCancelWorkflowExecutionInput -> Aff (err :: AWS.RequestError | eff) Unit
requestCancelWorkflowExecution = AWS.request serviceName "requestCancelWorkflowExecution" 


-- | <p>Used by workers to tell the service that the <a>ActivityTask</a> identified by the <code>taskToken</code> was successfully canceled. Additional <code>details</code> can be provided using the <code>details</code> argument.</p> <p>These <code>details</code> (if provided) appear in the <code>ActivityTaskCanceled</code> event added to the workflow history.</p> <important> <p>Only use this operation if the <code>canceled</code> flag of a <a>RecordActivityTaskHeartbeat</a> request returns <code>true</code> and if the activity can be safely undone or abandoned.</p> </important> <p>A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to <a>RespondActivityTaskCompleted</a>, RespondActivityTaskCanceled, <a>RespondActivityTaskFailed</a>, or the task has <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types">timed out</a>.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
respondActivityTaskCanceled :: forall eff. RespondActivityTaskCanceledInput -> Aff (err :: AWS.RequestError | eff) Unit
respondActivityTaskCanceled = AWS.request serviceName "respondActivityTaskCanceled" 


-- | <p>Used by workers to tell the service that the <a>ActivityTask</a> identified by the <code>taskToken</code> completed successfully with a <code>result</code> (if provided). The <code>result</code> appears in the <code>ActivityTaskCompleted</code> event in the workflow history.</p> <important> <p>If the requested task doesn't complete successfully, use <a>RespondActivityTaskFailed</a> instead. If the worker finds that the task is canceled through the <code>canceled</code> flag returned by <a>RecordActivityTaskHeartbeat</a>, it should cancel the task, clean up and then call <a>RespondActivityTaskCanceled</a>.</p> </important> <p>A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to RespondActivityTaskCompleted, <a>RespondActivityTaskCanceled</a>, <a>RespondActivityTaskFailed</a>, or the task has <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types">timed out</a>.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
respondActivityTaskCompleted :: forall eff. RespondActivityTaskCompletedInput -> Aff (err :: AWS.RequestError | eff) Unit
respondActivityTaskCompleted = AWS.request serviceName "respondActivityTaskCompleted" 


-- | <p>Used by workers to tell the service that the <a>ActivityTask</a> identified by the <code>taskToken</code> has failed with <code>reason</code> (if specified). The <code>reason</code> and <code>details</code> appear in the <code>ActivityTaskFailed</code> event added to the workflow history.</p> <p>A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to <a>RespondActivityTaskCompleted</a>, <a>RespondActivityTaskCanceled</a>, RespondActivityTaskFailed, or the task has <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types">timed out</a>.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
respondActivityTaskFailed :: forall eff. RespondActivityTaskFailedInput -> Aff (err :: AWS.RequestError | eff) Unit
respondActivityTaskFailed = AWS.request serviceName "respondActivityTaskFailed" 


-- | <p>Used by deciders to tell the service that the <a>DecisionTask</a> identified by the <code>taskToken</code> has successfully completed. The <code>decisions</code> argument specifies the list of decisions made while processing the task.</p> <p>A <code>DecisionTaskCompleted</code> event is added to the workflow history. The <code>executionContext</code> specified is attached to the event in the workflow execution history.</p> <p> <b>Access Control</b> </p> <p>If an IAM policy grants permission to use <code>RespondDecisionTaskCompleted</code>, it can express permissions for the list of decisions in the <code>decisions</code> parameter. Each of the decisions has one or more parameters, much like a regular API call. To allow for policies to be as readable as possible, you can express permissions on decisions as if they were actual API calls, including applying conditions to some parameters. For more information, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
respondDecisionTaskCompleted :: forall eff. RespondDecisionTaskCompletedInput -> Aff (err :: AWS.RequestError | eff) Unit
respondDecisionTaskCompleted = AWS.request serviceName "respondDecisionTaskCompleted" 


-- | <p>Records a <code>WorkflowExecutionSignaled</code> event in the workflow execution history and creates a decision task for the workflow execution identified by the given domain, workflowId and runId. The event is recorded with the specified user defined signalName and input (if provided).</p> <note> <p>If a runId isn't specified, then the <code>WorkflowExecutionSignaled</code> event is recorded in the history of the current open workflow with the matching workflowId in the domain.</p> </note> <note> <p>If the specified workflow execution isn't open, this method fails with <code>UnknownResource</code>.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
signalWorkflowExecution :: forall eff. SignalWorkflowExecutionInput -> Aff (err :: AWS.RequestError | eff) Unit
signalWorkflowExecution = AWS.request serviceName "signalWorkflowExecution" 


-- | <p>Starts an execution of the workflow type in the specified domain using the provided <code>workflowId</code> and input data.</p> <p>This action returns the newly started workflow execution.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagList.member.0</code>: The key is <code>swf:tagList.member.0</code>.</p> </li> <li> <p> <code>tagList.member.1</code>: The key is <code>swf:tagList.member.1</code>.</p> </li> <li> <p> <code>tagList.member.2</code>: The key is <code>swf:tagList.member.2</code>.</p> </li> <li> <p> <code>tagList.member.3</code>: The key is <code>swf:tagList.member.3</code>.</p> </li> <li> <p> <code>tagList.member.4</code>: The key is <code>swf:tagList.member.4</code>.</p> </li> <li> <p> <code>taskList</code>: String constraint. The key is <code>swf:taskList.name</code>.</p> </li> <li> <p> <code>workflowType.name</code>: String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code>: String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
startWorkflowExecution :: forall eff. StartWorkflowExecutionInput -> Aff (err :: AWS.RequestError | eff) Run
startWorkflowExecution = AWS.request serviceName "startWorkflowExecution" 


-- | <p>Records a <code>WorkflowExecutionTerminated</code> event and forces closure of the workflow execution identified by the given domain, runId, and workflowId. The child policy, registered with the workflow type or specified when starting this execution, is applied to any open child workflow executions of this workflow execution.</p> <important> <p>If the identified workflow execution was in progress, it is terminated immediately.</p> </important> <note> <p>If a runId isn't specified, then the <code>WorkflowExecutionTerminated</code> event is recorded in the history of the current open workflow with the matching workflowId in the domain.</p> </note> <note> <p>You should consider using <a>RequestCancelWorkflowExecution</a> action instead because it allows the workflow to gracefully close while <a>TerminateWorkflowExecution</a> doesn't.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
terminateWorkflowExecution :: forall eff. TerminateWorkflowExecutionInput -> Aff (err :: AWS.RequestError | eff) Unit
terminateWorkflowExecution = AWS.request serviceName "terminateWorkflowExecution" 


newtype ActivityId = ActivityId String
derive instance newtypeActivityId :: Newtype ActivityId _


-- | <p>Unit of work sent to an activity worker.</p>
newtype ActivityTask = ActivityTask 
  { "TaskToken'" :: (TaskToken)
  , "ActivityId'" :: (ActivityId)
  , "StartedEventId'" :: (EventId)
  , "WorkflowExecution'" :: (WorkflowExecution)
  , "ActivityType'" :: (ActivityType)
  , "Input'" :: NullOrUndefined (Data)
  }
derive instance newtypeActivityTask :: Newtype ActivityTask _


-- | <p>Provides the details of the <code>ActivityTaskCancelRequested</code> event.</p>
newtype ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes 
  { "DecisionTaskCompletedEventId'" :: (EventId)
  , "ActivityId'" :: (ActivityId)
  }
derive instance newtypeActivityTaskCancelRequestedEventAttributes :: Newtype ActivityTaskCancelRequestedEventAttributes _


-- | <p>Provides the details of the <code>ActivityTaskCanceled</code> event.</p>
newtype ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes 
  { "Details'" :: NullOrUndefined (Data)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "LatestCancelRequestedEventId'" :: NullOrUndefined (EventId)
  }
derive instance newtypeActivityTaskCanceledEventAttributes :: Newtype ActivityTaskCanceledEventAttributes _


-- | <p>Provides the details of the <code>ActivityTaskCompleted</code> event.</p>
newtype ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes 
  { "Result'" :: NullOrUndefined (Data)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeActivityTaskCompletedEventAttributes :: Newtype ActivityTaskCompletedEventAttributes _


-- | <p>Provides the details of the <code>ActivityTaskFailed</code> event.</p>
newtype ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes 
  { "Reason'" :: NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined (Data)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeActivityTaskFailedEventAttributes :: Newtype ActivityTaskFailedEventAttributes _


-- | <p>Provides the details of the <code>ActivityTaskScheduled</code> event.</p>
newtype ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes 
  { "ActivityType'" :: (ActivityType)
  , "ActivityId'" :: (ActivityId)
  , "Input'" :: NullOrUndefined (Data)
  , "Control'" :: NullOrUndefined (Data)
  , "ScheduleToStartTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "ScheduleToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "HeartbeatTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeActivityTaskScheduledEventAttributes :: Newtype ActivityTaskScheduledEventAttributes _


-- | <p>Provides the details of the <code>ActivityTaskStarted</code> event.</p>
newtype ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes 
  { "Identity'" :: NullOrUndefined (Identity)
  , "ScheduledEventId'" :: (EventId)
  }
derive instance newtypeActivityTaskStartedEventAttributes :: Newtype ActivityTaskStartedEventAttributes _


-- | <p>Status information about an activity task.</p>
newtype ActivityTaskStatus = ActivityTaskStatus 
  { "CancelRequested'" :: (Canceled)
  }
derive instance newtypeActivityTaskStatus :: Newtype ActivityTaskStatus _


-- | <p>Provides the details of the <code>ActivityTaskTimedOut</code> event.</p>
newtype ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes 
  { "TimeoutType'" :: (ActivityTaskTimeoutType)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "Details'" :: NullOrUndefined (LimitedData)
  }
derive instance newtypeActivityTaskTimedOutEventAttributes :: Newtype ActivityTaskTimedOutEventAttributes _


newtype ActivityTaskTimeoutType = ActivityTaskTimeoutType String
derive instance newtypeActivityTaskTimeoutType :: Newtype ActivityTaskTimeoutType _


-- | <p>Represents an activity type.</p>
newtype ActivityType = ActivityType 
  { "Name'" :: (Name)
  , "Version'" :: (Version)
  }
derive instance newtypeActivityType :: Newtype ActivityType _


-- | <p>Configuration settings registered with the activity type.</p>
newtype ActivityTypeConfiguration = ActivityTypeConfiguration 
  { "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskHeartbeatTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskList'" :: NullOrUndefined (TaskList)
  , "DefaultTaskPriority'" :: NullOrUndefined (TaskPriority)
  , "DefaultTaskScheduleToStartTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskScheduleToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeActivityTypeConfiguration :: Newtype ActivityTypeConfiguration _


-- | <p>Detailed information about an activity type.</p>
newtype ActivityTypeDetail = ActivityTypeDetail 
  { "TypeInfo'" :: (ActivityTypeInfo)
  , "Configuration'" :: (ActivityTypeConfiguration)
  }
derive instance newtypeActivityTypeDetail :: Newtype ActivityTypeDetail _


-- | <p>Detailed information about an activity type.</p>
newtype ActivityTypeInfo = ActivityTypeInfo 
  { "ActivityType'" :: (ActivityType)
  , "Status'" :: (RegistrationStatus)
  , "Description'" :: NullOrUndefined (Description)
  , "CreationDate'" :: (Number)
  , "DeprecationDate'" :: NullOrUndefined (Number)
  }
derive instance newtypeActivityTypeInfo :: Newtype ActivityTypeInfo _


newtype ActivityTypeInfoList = ActivityTypeInfoList (Array ActivityTypeInfo)
derive instance newtypeActivityTypeInfoList :: Newtype ActivityTypeInfoList _


-- | <p>Contains a paginated list of activity type information structures.</p>
newtype ActivityTypeInfos = ActivityTypeInfos 
  { "TypeInfos'" :: (ActivityTypeInfoList)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeActivityTypeInfos :: Newtype ActivityTypeInfos _


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


-- | <p>Provides the details of the <code>CancelTimer</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes 
  { "TimerId'" :: (TimerId)
  }
derive instance newtypeCancelTimerDecisionAttributes :: Newtype CancelTimerDecisionAttributes _


newtype CancelTimerFailedCause = CancelTimerFailedCause String
derive instance newtypeCancelTimerFailedCause :: Newtype CancelTimerFailedCause _


-- | <p>Provides the details of the <code>CancelTimerFailed</code> event.</p>
newtype CancelTimerFailedEventAttributes = CancelTimerFailedEventAttributes 
  { "TimerId'" :: (TimerId)
  , "Cause'" :: (CancelTimerFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeCancelTimerFailedEventAttributes :: Newtype CancelTimerFailedEventAttributes _


-- | <p>Provides the details of the <code>CancelWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes 
  { "Details'" :: NullOrUndefined (Data)
  }
derive instance newtypeCancelWorkflowExecutionDecisionAttributes :: Newtype CancelWorkflowExecutionDecisionAttributes _


newtype CancelWorkflowExecutionFailedCause = CancelWorkflowExecutionFailedCause String
derive instance newtypeCancelWorkflowExecutionFailedCause :: Newtype CancelWorkflowExecutionFailedCause _


-- | <p>Provides the details of the <code>CancelWorkflowExecutionFailed</code> event.</p>
newtype CancelWorkflowExecutionFailedEventAttributes = CancelWorkflowExecutionFailedEventAttributes 
  { "Cause'" :: (CancelWorkflowExecutionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeCancelWorkflowExecutionFailedEventAttributes :: Newtype CancelWorkflowExecutionFailedEventAttributes _


newtype Canceled = Canceled Boolean
derive instance newtypeCanceled :: Newtype Canceled _


newtype CauseMessage = CauseMessage String
derive instance newtypeCauseMessage :: Newtype CauseMessage _


newtype ChildPolicy = ChildPolicy String
derive instance newtypeChildPolicy :: Newtype ChildPolicy _


-- | <p>Provide details of the <code>ChildWorkflowExecutionCanceled</code> event.</p>
newtype ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "Details'" :: NullOrUndefined (Data)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionCanceledEventAttributes :: Newtype ChildWorkflowExecutionCanceledEventAttributes _


-- | <p>Provides the details of the <code>ChildWorkflowExecutionCompleted</code> event.</p>
newtype ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "Result'" :: NullOrUndefined (Data)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionCompletedEventAttributes :: Newtype ChildWorkflowExecutionCompletedEventAttributes _


-- | <p>Provides the details of the <code>ChildWorkflowExecutionFailed</code> event.</p>
newtype ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "Reason'" :: NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined (Data)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionFailedEventAttributes :: Newtype ChildWorkflowExecutionFailedEventAttributes _


-- | <p>Provides the details of the <code>ChildWorkflowExecutionStarted</code> event.</p>
newtype ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "InitiatedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionStartedEventAttributes :: Newtype ChildWorkflowExecutionStartedEventAttributes _


-- | <p>Provides the details of the <code>ChildWorkflowExecutionTerminated</code> event.</p>
newtype ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionTerminatedEventAttributes :: Newtype ChildWorkflowExecutionTerminatedEventAttributes _


-- | <p>Provides the details of the <code>ChildWorkflowExecutionTimedOut</code> event.</p>
newtype ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "TimeoutType'" :: (WorkflowExecutionTimeoutType)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionTimedOutEventAttributes :: Newtype ChildWorkflowExecutionTimedOutEventAttributes _


newtype CloseStatus = CloseStatus String
derive instance newtypeCloseStatus :: Newtype CloseStatus _


-- | <p>Used to filter the closed workflow executions in visibility APIs by their close status.</p>
newtype CloseStatusFilter = CloseStatusFilter 
  { "Status'" :: (CloseStatus)
  }
derive instance newtypeCloseStatusFilter :: Newtype CloseStatusFilter _


-- | <p>Provides the details of the <code>CompleteWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes 
  { "Result'" :: NullOrUndefined (Data)
  }
derive instance newtypeCompleteWorkflowExecutionDecisionAttributes :: Newtype CompleteWorkflowExecutionDecisionAttributes _


newtype CompleteWorkflowExecutionFailedCause = CompleteWorkflowExecutionFailedCause String
derive instance newtypeCompleteWorkflowExecutionFailedCause :: Newtype CompleteWorkflowExecutionFailedCause _


-- | <p>Provides the details of the <code>CompleteWorkflowExecutionFailed</code> event.</p>
newtype CompleteWorkflowExecutionFailedEventAttributes = CompleteWorkflowExecutionFailedEventAttributes 
  { "Cause'" :: (CompleteWorkflowExecutionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeCompleteWorkflowExecutionFailedEventAttributes :: Newtype CompleteWorkflowExecutionFailedEventAttributes _


-- | <p>Provides the details of the <code>ContinueAsNewWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tag</code> – A tag used to identify the workflow execution</p> </li> <li> <p> <code>taskList</code> – String constraint. The key is <code>swf:taskList.name</code>.</p> </li> <li> <p> <code>workflowType.version</code> – String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes 
  { "Input'" :: NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: NullOrUndefined (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: NullOrUndefined (ChildPolicy)
  , "TagList'" :: NullOrUndefined (TagList)
  , "WorkflowTypeVersion'" :: NullOrUndefined (Version)
  , "LambdaRole'" :: NullOrUndefined (Arn)
  }
derive instance newtypeContinueAsNewWorkflowExecutionDecisionAttributes :: Newtype ContinueAsNewWorkflowExecutionDecisionAttributes _


newtype ContinueAsNewWorkflowExecutionFailedCause = ContinueAsNewWorkflowExecutionFailedCause String
derive instance newtypeContinueAsNewWorkflowExecutionFailedCause :: Newtype ContinueAsNewWorkflowExecutionFailedCause _


-- | <p>Provides the details of the <code>ContinueAsNewWorkflowExecutionFailed</code> event.</p>
newtype ContinueAsNewWorkflowExecutionFailedEventAttributes = ContinueAsNewWorkflowExecutionFailedEventAttributes 
  { "Cause'" :: (ContinueAsNewWorkflowExecutionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeContinueAsNewWorkflowExecutionFailedEventAttributes :: Newtype ContinueAsNewWorkflowExecutionFailedEventAttributes _


newtype Count = Count Int
derive instance newtypeCount :: Newtype Count _


newtype CountClosedWorkflowExecutionsInput = CountClosedWorkflowExecutionsInput 
  { "Domain'" :: (DomainName)
  , "StartTimeFilter'" :: NullOrUndefined (ExecutionTimeFilter)
  , "CloseTimeFilter'" :: NullOrUndefined (ExecutionTimeFilter)
  , "ExecutionFilter'" :: NullOrUndefined (WorkflowExecutionFilter)
  , "TypeFilter'" :: NullOrUndefined (WorkflowTypeFilter)
  , "TagFilter'" :: NullOrUndefined (TagFilter)
  , "CloseStatusFilter'" :: NullOrUndefined (CloseStatusFilter)
  }
derive instance newtypeCountClosedWorkflowExecutionsInput :: Newtype CountClosedWorkflowExecutionsInput _


newtype CountOpenWorkflowExecutionsInput = CountOpenWorkflowExecutionsInput 
  { "Domain'" :: (DomainName)
  , "StartTimeFilter'" :: (ExecutionTimeFilter)
  , "TypeFilter'" :: NullOrUndefined (WorkflowTypeFilter)
  , "TagFilter'" :: NullOrUndefined (TagFilter)
  , "ExecutionFilter'" :: NullOrUndefined (WorkflowExecutionFilter)
  }
derive instance newtypeCountOpenWorkflowExecutionsInput :: Newtype CountOpenWorkflowExecutionsInput _


newtype CountPendingActivityTasksInput = CountPendingActivityTasksInput 
  { "Domain'" :: (DomainName)
  , "TaskList'" :: (TaskList)
  }
derive instance newtypeCountPendingActivityTasksInput :: Newtype CountPendingActivityTasksInput _


newtype CountPendingDecisionTasksInput = CountPendingDecisionTasksInput 
  { "Domain'" :: (DomainName)
  , "TaskList'" :: (TaskList)
  }
derive instance newtypeCountPendingDecisionTasksInput :: Newtype CountPendingDecisionTasksInput _


newtype Data = Data String
derive instance newtypeData :: Newtype Data _


-- | <p>Specifies a decision made by the decider. A decision can be one of these types:</p> <ul> <li> <p> <code>CancelTimer</code> – Cancels a previously started timer and records a <code>TimerCanceled</code> event in the history.</p> </li> <li> <p> <code>CancelWorkflowExecution</code> – Closes the workflow execution and records a <code>WorkflowExecutionCanceled</code> event in the history.</p> </li> <li> <p> <code>CompleteWorkflowExecution</code> – Closes the workflow execution and records a <code>WorkflowExecutionCompleted</code> event in the history .</p> </li> <li> <p> <code>ContinueAsNewWorkflowExecution</code> – Closes the workflow execution and starts a new workflow execution of the same type using the same workflow ID and a unique run Id. A <code>WorkflowExecutionContinuedAsNew</code> event is recorded in the history.</p> </li> <li> <p> <code>FailWorkflowExecution</code> – Closes the workflow execution and records a <code>WorkflowExecutionFailed</code> event in the history.</p> </li> <li> <p> <code>RecordMarker</code> – Records a <code>MarkerRecorded</code> event in the history. Markers can be used for adding custom information in the history for instance to let deciders know that they don't need to look at the history beyond the marker event.</p> </li> <li> <p> <code>RequestCancelActivityTask</code> – Attempts to cancel a previously scheduled activity task. If the activity task was scheduled but has not been assigned to a worker, then it is canceled. If the activity task was already assigned to a worker, then the worker is informed that cancellation has been requested in the response to <a>RecordActivityTaskHeartbeat</a>.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecution</code> – Requests that a request be made to cancel the specified external workflow execution and records a <code>RequestCancelExternalWorkflowExecutionInitiated</code> event in the history.</p> </li> <li> <p> <code>ScheduleActivityTask</code> – Schedules an activity task.</p> </li> <li> <p> <code>SignalExternalWorkflowExecution</code> – Requests a signal to be delivered to the specified external workflow execution and records a <code>SignalExternalWorkflowExecutionInitiated</code> event in the history.</p> </li> <li> <p> <code>StartChildWorkflowExecution</code> – Requests that a child workflow execution be started and records a <code>StartChildWorkflowExecutionInitiated</code> event in the history. The child workflow execution is a separate workflow execution with its own history.</p> </li> <li> <p> <code>StartTimer</code> – Starts a timer for this workflow execution and records a <code>TimerStarted</code> event in the history. This timer fires after the specified delay and record a <code>TimerFired</code> event.</p> </li> </ul> <p> <b>Access Control</b> </p> <p>If you grant permission to use <code>RespondDecisionTaskCompleted</code>, you can use IAM policies to express permissions for the list of decisions returned by this action as if they were members of the API. Treating decisions as a pseudo API maintains a uniform conceptual model and helps keep policies readable. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p> <p> <b>Decision Failure</b> </p> <p>Decisions can fail for several reasons</p> <ul> <li> <p>The ordering of decisions should follow a logical flow. Some decisions might not make sense in the current context of the workflow execution and therefore fails.</p> </li> <li> <p>A limit on your account was reached.</p> </li> <li> <p>The decision lacks sufficient permissions.</p> </li> </ul> <p>One of the following events might be added to the history to indicate an error. The event attribute's <code>cause</code> parameter indicates the cause. If <code>cause</code> is set to <code>OPERATION_NOT_PERMITTED</code>, the decision failed because it lacked sufficient permissions. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p> <ul> <li> <p> <code>ScheduleActivityTaskFailed</code> – A <code>ScheduleActivityTask</code> decision failed. This could happen if the activity type specified in the decision isn't registered, is in a deprecated state, or the decision isn't properly configured.</p> </li> <li> <p> <code>RequestCancelActivityTaskFailed</code> – A <code>RequestCancelActivityTask</code> decision failed. This could happen if there is no open activity task with the specified activityId.</p> </li> <li> <p> <code>StartTimerFailed</code> – A <code>StartTimer</code> decision failed. This could happen if there is another open timer with the same timerId.</p> </li> <li> <p> <code>CancelTimerFailed</code> – A <code>CancelTimer</code> decision failed. This could happen if there is no open timer with the specified timerId.</p> </li> <li> <p> <code>StartChildWorkflowExecutionFailed</code> – A <code>StartChildWorkflowExecution</code> decision failed. This could happen if the workflow type specified isn't registered, is deprecated, or the decision isn't properly configured.</p> </li> <li> <p> <code>SignalExternalWorkflowExecutionFailed</code> – A <code>SignalExternalWorkflowExecution</code> decision failed. This could happen if the <code>workflowID</code> specified in the decision was incorrect.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecutionFailed</code> – A <code>RequestCancelExternalWorkflowExecution</code> decision failed. This could happen if the <code>workflowID</code> specified in the decision was incorrect.</p> </li> <li> <p> <code>CancelWorkflowExecutionFailed</code> – A <code>CancelWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.</p> </li> <li> <p> <code>CompleteWorkflowExecutionFailed</code> – A <code>CompleteWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.</p> </li> <li> <p> <code>ContinueAsNewWorkflowExecutionFailed</code> – A <code>ContinueAsNewWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution or the ContinueAsNewWorkflowExecution decision was not configured correctly.</p> </li> <li> <p> <code>FailWorkflowExecutionFailed</code> – A <code>FailWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.</p> </li> </ul> <p>The preceding error events might occur due to an error in the decider logic, which might put the workflow execution in an unstable state The cause field in the event structure for the error event indicates the cause of the error.</p> <note> <p>A workflow execution may be closed by the decider by returning one of the following decisions when completing a decision task: <code>CompleteWorkflowExecution</code>, <code>FailWorkflowExecution</code>, <code>CancelWorkflowExecution</code> and <code>ContinueAsNewWorkflowExecution</code>. An <code>UnhandledDecision</code> fault is returned if a workflow closing decision is specified and a signal or activity event had been added to the history while the decision task was being performed by the decider. Unlike the above situations which are logic issues, this fault is always possible because of race conditions in a distributed system. The right action here is to call <a>RespondDecisionTaskCompleted</a> without any decisions. This would result in another decision task with these new events included in the history. The decider should handle the new events and may decide to close the workflow execution.</p> </note> <p> <b>How to Code a Decision</b> </p> <p>You code a decision by first setting the decision type field to one of the above decision values, and then set the corresponding attributes field shown below:</p> <ul> <li> <p> <code> <a>ScheduleActivityTaskDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>RequestCancelActivityTaskDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>CompleteWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>FailWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>CancelWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>ContinueAsNewWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>RecordMarkerDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>StartTimerDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>CancelTimerDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>SignalExternalWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>RequestCancelExternalWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>StartChildWorkflowExecutionDecisionAttributes</a> </code> </p> </li> </ul>
newtype Decision = Decision 
  { "DecisionType'" :: (DecisionType)
  , "ScheduleActivityTaskDecisionAttributes'" :: NullOrUndefined (ScheduleActivityTaskDecisionAttributes)
  , "RequestCancelActivityTaskDecisionAttributes'" :: NullOrUndefined (RequestCancelActivityTaskDecisionAttributes)
  , "CompleteWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (CompleteWorkflowExecutionDecisionAttributes)
  , "FailWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (FailWorkflowExecutionDecisionAttributes)
  , "CancelWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (CancelWorkflowExecutionDecisionAttributes)
  , "ContinueAsNewWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (ContinueAsNewWorkflowExecutionDecisionAttributes)
  , "RecordMarkerDecisionAttributes'" :: NullOrUndefined (RecordMarkerDecisionAttributes)
  , "StartTimerDecisionAttributes'" :: NullOrUndefined (StartTimerDecisionAttributes)
  , "CancelTimerDecisionAttributes'" :: NullOrUndefined (CancelTimerDecisionAttributes)
  , "SignalExternalWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (SignalExternalWorkflowExecutionDecisionAttributes)
  , "RequestCancelExternalWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (RequestCancelExternalWorkflowExecutionDecisionAttributes)
  , "StartChildWorkflowExecutionDecisionAttributes'" :: NullOrUndefined (StartChildWorkflowExecutionDecisionAttributes)
  , "ScheduleLambdaFunctionDecisionAttributes'" :: NullOrUndefined (ScheduleLambdaFunctionDecisionAttributes)
  }
derive instance newtypeDecision :: Newtype Decision _


newtype DecisionList = DecisionList (Array Decision)
derive instance newtypeDecisionList :: Newtype DecisionList _


-- | <p>A structure that represents a decision task. Decision tasks are sent to deciders in order for them to make decisions.</p>
newtype DecisionTask = DecisionTask 
  { "TaskToken'" :: (TaskToken)
  , "StartedEventId'" :: (EventId)
  , "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "Events'" :: (HistoryEventList)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  , "PreviousStartedEventId'" :: NullOrUndefined (EventId)
  }
derive instance newtypeDecisionTask :: Newtype DecisionTask _


-- | <p>Provides the details of the <code>DecisionTaskCompleted</code> event.</p>
newtype DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes 
  { "ExecutionContext'" :: NullOrUndefined (Data)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeDecisionTaskCompletedEventAttributes :: Newtype DecisionTaskCompletedEventAttributes _


-- | <p>Provides details about the <code>DecisionTaskScheduled</code> event.</p>
newtype DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes 
  { "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeDecisionTaskScheduledEventAttributes :: Newtype DecisionTaskScheduledEventAttributes _


-- | <p>Provides the details of the <code>DecisionTaskStarted</code> event.</p>
newtype DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes 
  { "Identity'" :: NullOrUndefined (Identity)
  , "ScheduledEventId'" :: (EventId)
  }
derive instance newtypeDecisionTaskStartedEventAttributes :: Newtype DecisionTaskStartedEventAttributes _


-- | <p>Provides the details of the <code>DecisionTaskTimedOut</code> event.</p>
newtype DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes 
  { "TimeoutType'" :: (DecisionTaskTimeoutType)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeDecisionTaskTimedOutEventAttributes :: Newtype DecisionTaskTimedOutEventAttributes _


newtype DecisionTaskTimeoutType = DecisionTaskTimeoutType String
derive instance newtypeDecisionTaskTimeoutType :: Newtype DecisionTaskTimeoutType _


newtype DecisionType = DecisionType String
derive instance newtypeDecisionType :: Newtype DecisionType _


-- | <p>The <code>StartWorkflowExecution</code> API action was called without the required parameters set.</p> <p>Some workflow execution parameters, such as the decision <code>taskList</code>, must be set to start the execution. However, these parameters might have been set as defaults when the workflow type was registered. In this case, you can omit these parameters from the <code>StartWorkflowExecution</code> call and Amazon SWF uses the values defined in the workflow type.</p> <note> <p>If these parameters aren't set and no default parameters were defined in the workflow type, this error is displayed.</p> </note>
newtype DefaultUndefinedFault = DefaultUndefinedFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDefaultUndefinedFault :: Newtype DefaultUndefinedFault _


newtype DeprecateActivityTypeInput = DeprecateActivityTypeInput 
  { "Domain'" :: (DomainName)
  , "ActivityType'" :: (ActivityType)
  }
derive instance newtypeDeprecateActivityTypeInput :: Newtype DeprecateActivityTypeInput _


newtype DeprecateDomainInput = DeprecateDomainInput 
  { "Name'" :: (DomainName)
  }
derive instance newtypeDeprecateDomainInput :: Newtype DeprecateDomainInput _


newtype DeprecateWorkflowTypeInput = DeprecateWorkflowTypeInput 
  { "Domain'" :: (DomainName)
  , "WorkflowType'" :: (WorkflowType)
  }
derive instance newtypeDeprecateWorkflowTypeInput :: Newtype DeprecateWorkflowTypeInput _


newtype DescribeActivityTypeInput = DescribeActivityTypeInput 
  { "Domain'" :: (DomainName)
  , "ActivityType'" :: (ActivityType)
  }
derive instance newtypeDescribeActivityTypeInput :: Newtype DescribeActivityTypeInput _


newtype DescribeDomainInput = DescribeDomainInput 
  { "Name'" :: (DomainName)
  }
derive instance newtypeDescribeDomainInput :: Newtype DescribeDomainInput _


newtype DescribeWorkflowExecutionInput = DescribeWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "Execution'" :: (WorkflowExecution)
  }
derive instance newtypeDescribeWorkflowExecutionInput :: Newtype DescribeWorkflowExecutionInput _


newtype DescribeWorkflowTypeInput = DescribeWorkflowTypeInput 
  { "Domain'" :: (DomainName)
  , "WorkflowType'" :: (WorkflowType)
  }
derive instance newtypeDescribeWorkflowTypeInput :: Newtype DescribeWorkflowTypeInput _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


-- | <p>Returned if the specified domain already exists. You get this fault even if the existing domain is in deprecated status.</p>
newtype DomainAlreadyExistsFault = DomainAlreadyExistsFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDomainAlreadyExistsFault :: Newtype DomainAlreadyExistsFault _


-- | <p>Contains the configuration settings of a domain.</p>
newtype DomainConfiguration = DomainConfiguration 
  { "WorkflowExecutionRetentionPeriodInDays'" :: (DurationInDays)
  }
derive instance newtypeDomainConfiguration :: Newtype DomainConfiguration _


-- | <p>Returned when the specified domain has been deprecated.</p>
newtype DomainDeprecatedFault = DomainDeprecatedFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDomainDeprecatedFault :: Newtype DomainDeprecatedFault _


-- | <p>Contains details of a domain.</p>
newtype DomainDetail = DomainDetail 
  { "DomainInfo'" :: (DomainInfo)
  , "Configuration'" :: (DomainConfiguration)
  }
derive instance newtypeDomainDetail :: Newtype DomainDetail _


-- | <p>Contains general information about a domain.</p>
newtype DomainInfo = DomainInfo 
  { "Name'" :: (DomainName)
  , "Status'" :: (RegistrationStatus)
  , "Description'" :: NullOrUndefined (Description)
  }
derive instance newtypeDomainInfo :: Newtype DomainInfo _


newtype DomainInfoList = DomainInfoList (Array DomainInfo)
derive instance newtypeDomainInfoList :: Newtype DomainInfoList _


-- | <p>Contains a paginated collection of DomainInfo structures.</p>
newtype DomainInfos = DomainInfos 
  { "DomainInfos'" :: (DomainInfoList)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeDomainInfos :: Newtype DomainInfos _


newtype DomainName = DomainName String
derive instance newtypeDomainName :: Newtype DomainName _


newtype DurationInDays = DurationInDays String
derive instance newtypeDurationInDays :: Newtype DurationInDays _


newtype DurationInSeconds = DurationInSeconds String
derive instance newtypeDurationInSeconds :: Newtype DurationInSeconds _


newtype DurationInSecondsOptional = DurationInSecondsOptional String
derive instance newtypeDurationInSecondsOptional :: Newtype DurationInSecondsOptional _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype EventId = EventId Number
derive instance newtypeEventId :: Newtype EventId _


newtype EventType = EventType String
derive instance newtypeEventType :: Newtype EventType _


newtype ExecutionStatus = ExecutionStatus String
derive instance newtypeExecutionStatus :: Newtype ExecutionStatus _


-- | <p>Used to filter the workflow executions in visibility APIs by various time-based rules. Each parameter, if specified, defines a rule that must be satisfied by each returned query result. The parameter values are in the <a href="https://en.wikipedia.org/wiki/Unix_time">Unix Time format</a>. For example: <code>"oldestDate": 1325376070.</code> </p>
newtype ExecutionTimeFilter = ExecutionTimeFilter 
  { "OldestDate'" :: (Number)
  , "LatestDate'" :: NullOrUndefined (Number)
  }
derive instance newtypeExecutionTimeFilter :: Newtype ExecutionTimeFilter _


-- | <p>Provides the details of the <code>ExternalWorkflowExecutionCancelRequested</code> event.</p>
newtype ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "InitiatedEventId'" :: (EventId)
  }
derive instance newtypeExternalWorkflowExecutionCancelRequestedEventAttributes :: Newtype ExternalWorkflowExecutionCancelRequestedEventAttributes _


-- | <p>Provides the details of the <code>ExternalWorkflowExecutionSignaled</code> event.</p>
newtype ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "InitiatedEventId'" :: (EventId)
  }
derive instance newtypeExternalWorkflowExecutionSignaledEventAttributes :: Newtype ExternalWorkflowExecutionSignaledEventAttributes _


-- | <p>Provides the details of the <code>FailWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes 
  { "Reason'" :: NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined (Data)
  }
derive instance newtypeFailWorkflowExecutionDecisionAttributes :: Newtype FailWorkflowExecutionDecisionAttributes _


newtype FailWorkflowExecutionFailedCause = FailWorkflowExecutionFailedCause String
derive instance newtypeFailWorkflowExecutionFailedCause :: Newtype FailWorkflowExecutionFailedCause _


-- | <p>Provides the details of the <code>FailWorkflowExecutionFailed</code> event.</p>
newtype FailWorkflowExecutionFailedEventAttributes = FailWorkflowExecutionFailedEventAttributes 
  { "Cause'" :: (FailWorkflowExecutionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeFailWorkflowExecutionFailedEventAttributes :: Newtype FailWorkflowExecutionFailedEventAttributes _


newtype FailureReason = FailureReason String
derive instance newtypeFailureReason :: Newtype FailureReason _


newtype FunctionId = FunctionId String
derive instance newtypeFunctionId :: Newtype FunctionId _


newtype FunctionInput = FunctionInput String
derive instance newtypeFunctionInput :: Newtype FunctionInput _


newtype FunctionName = FunctionName String
derive instance newtypeFunctionName :: Newtype FunctionName _


newtype GetWorkflowExecutionHistoryInput = GetWorkflowExecutionHistoryInput 
  { "Domain'" :: (DomainName)
  , "Execution'" :: (WorkflowExecution)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined (ReverseOrder)
  }
derive instance newtypeGetWorkflowExecutionHistoryInput :: Newtype GetWorkflowExecutionHistoryInput _


-- | <p>Paginated representation of a workflow history for a workflow execution. This is the up to date, complete and authoritative record of the events related to all tasks and events in the life of the workflow execution.</p>
newtype History = History 
  { "Events'" :: (HistoryEventList)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeHistory :: Newtype History _


-- | <p>Event within a workflow execution. A history event can be one of these types:</p> <ul> <li> <p> <code>ActivityTaskCancelRequested</code> – A <code>RequestCancelActivityTask</code> decision was received by the system.</p> </li> <li> <p> <code>ActivityTaskCanceled</code> – The activity task was successfully canceled.</p> </li> <li> <p> <code>ActivityTaskCompleted</code> – An activity worker successfully completed an activity task by calling <a>RespondActivityTaskCompleted</a>.</p> </li> <li> <p> <code>ActivityTaskFailed</code> – An activity worker failed an activity task by calling <a>RespondActivityTaskFailed</a>.</p> </li> <li> <p> <code>ActivityTaskScheduled</code> – An activity task was scheduled for execution.</p> </li> <li> <p> <code>ActivityTaskStarted</code> – The scheduled activity task was dispatched to a worker.</p> </li> <li> <p> <code>ActivityTaskTimedOut</code> – The activity task timed out.</p> </li> <li> <p> <code>CancelTimerFailed</code> – Failed to process CancelTimer decision. This happens when the decision isn't configured properly, for example no timer exists with the specified timer Id.</p> </li> <li> <p> <code>CancelWorkflowExecutionFailed</code> – A request to cancel a workflow execution failed.</p> </li> <li> <p> <code>ChildWorkflowExecutionCanceled</code> – A child workflow execution, started by this workflow execution, was canceled and closed.</p> </li> <li> <p> <code>ChildWorkflowExecutionCompleted</code> – A child workflow execution, started by this workflow execution, completed successfully and was closed.</p> </li> <li> <p> <code>ChildWorkflowExecutionFailed</code> – A child workflow execution, started by this workflow execution, failed to complete successfully and was closed.</p> </li> <li> <p> <code>ChildWorkflowExecutionStarted</code> – A child workflow execution was successfully started.</p> </li> <li> <p> <code>ChildWorkflowExecutionTerminated</code> – A child workflow execution, started by this workflow execution, was terminated.</p> </li> <li> <p> <code>ChildWorkflowExecutionTimedOut</code> – A child workflow execution, started by this workflow execution, timed out and was closed.</p> </li> <li> <p> <code>CompleteWorkflowExecutionFailed</code> – The workflow execution failed to complete.</p> </li> <li> <p> <code>ContinueAsNewWorkflowExecutionFailed</code> – The workflow execution failed to complete after being continued as a new workflow execution.</p> </li> <li> <p> <code>DecisionTaskCompleted</code> – The decider successfully completed a decision task by calling <a>RespondDecisionTaskCompleted</a>.</p> </li> <li> <p> <code>DecisionTaskScheduled</code> – A decision task was scheduled for the workflow execution.</p> </li> <li> <p> <code>DecisionTaskStarted</code> – The decision task was dispatched to a decider.</p> </li> <li> <p> <code>DecisionTaskTimedOut</code> – The decision task timed out.</p> </li> <li> <p> <code>ExternalWorkflowExecutionCancelRequested</code> – Request to cancel an external workflow execution was successfully delivered to the target execution.</p> </li> <li> <p> <code>ExternalWorkflowExecutionSignaled</code> – A signal, requested by this workflow execution, was successfully delivered to the target external workflow execution.</p> </li> <li> <p> <code>FailWorkflowExecutionFailed</code> – A request to mark a workflow execution as failed, itself failed.</p> </li> <li> <p> <code>MarkerRecorded</code> – A marker was recorded in the workflow history as the result of a <code>RecordMarker</code> decision.</p> </li> <li> <p> <code>RecordMarkerFailed</code> – A <code>RecordMarker</code> decision was returned as failed.</p> </li> <li> <p> <code>RequestCancelActivityTaskFailed</code> – Failed to process RequestCancelActivityTask decision. This happens when the decision isn't configured properly.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecutionFailed</code> – Request to cancel an external workflow execution failed.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecutionInitiated</code> – A request was made to request the cancellation of an external workflow execution.</p> </li> <li> <p> <code>ScheduleActivityTaskFailed</code> – Failed to process ScheduleActivityTask decision. This happens when the decision isn't configured properly, for example the activity type specified isn't registered.</p> </li> <li> <p> <code>SignalExternalWorkflowExecutionFailed</code> – The request to signal an external workflow execution failed.</p> </li> <li> <p> <code>SignalExternalWorkflowExecutionInitiated</code> – A request to signal an external workflow was made.</p> </li> <li> <p> <code>StartActivityTaskFailed</code> – A scheduled activity task failed to start.</p> </li> <li> <p> <code>StartChildWorkflowExecutionFailed</code> – Failed to process StartChildWorkflowExecution decision. This happens when the decision isn't configured properly, for example the workflow type specified isn't registered.</p> </li> <li> <p> <code>StartChildWorkflowExecutionInitiated</code> – A request was made to start a child workflow execution.</p> </li> <li> <p> <code>StartTimerFailed</code> – Failed to process StartTimer decision. This happens when the decision isn't configured properly, for example a timer already exists with the specified timer Id.</p> </li> <li> <p> <code>TimerCanceled</code> – A timer, previously started for this workflow execution, was successfully canceled.</p> </li> <li> <p> <code>TimerFired</code> – A timer, previously started for this workflow execution, fired.</p> </li> <li> <p> <code>TimerStarted</code> – A timer was started for the workflow execution due to a <code>StartTimer</code> decision.</p> </li> <li> <p> <code>WorkflowExecutionCancelRequested</code> – A request to cancel this workflow execution was made.</p> </li> <li> <p> <code>WorkflowExecutionCanceled</code> – The workflow execution was successfully canceled and closed.</p> </li> <li> <p> <code>WorkflowExecutionCompleted</code> – The workflow execution was closed due to successful completion.</p> </li> <li> <p> <code>WorkflowExecutionContinuedAsNew</code> – The workflow execution was closed and a new execution of the same type was created with the same workflowId.</p> </li> <li> <p> <code>WorkflowExecutionFailed</code> – The workflow execution closed due to a failure.</p> </li> <li> <p> <code>WorkflowExecutionSignaled</code> – An external signal was received for the workflow execution.</p> </li> <li> <p> <code>WorkflowExecutionStarted</code> – The workflow execution was started.</p> </li> <li> <p> <code>WorkflowExecutionTerminated</code> – The workflow execution was terminated.</p> </li> <li> <p> <code>WorkflowExecutionTimedOut</code> – The workflow execution was closed because a time out was exceeded.</p> </li> </ul>
newtype HistoryEvent = HistoryEvent 
  { "EventTimestamp'" :: (Number)
  , "EventType'" :: (EventType)
  , "EventId'" :: (EventId)
  , "WorkflowExecutionStartedEventAttributes'" :: NullOrUndefined (WorkflowExecutionStartedEventAttributes)
  , "WorkflowExecutionCompletedEventAttributes'" :: NullOrUndefined (WorkflowExecutionCompletedEventAttributes)
  , "CompleteWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (CompleteWorkflowExecutionFailedEventAttributes)
  , "WorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (WorkflowExecutionFailedEventAttributes)
  , "FailWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (FailWorkflowExecutionFailedEventAttributes)
  , "WorkflowExecutionTimedOutEventAttributes'" :: NullOrUndefined (WorkflowExecutionTimedOutEventAttributes)
  , "WorkflowExecutionCanceledEventAttributes'" :: NullOrUndefined (WorkflowExecutionCanceledEventAttributes)
  , "CancelWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (CancelWorkflowExecutionFailedEventAttributes)
  , "WorkflowExecutionContinuedAsNewEventAttributes'" :: NullOrUndefined (WorkflowExecutionContinuedAsNewEventAttributes)
  , "ContinueAsNewWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (ContinueAsNewWorkflowExecutionFailedEventAttributes)
  , "WorkflowExecutionTerminatedEventAttributes'" :: NullOrUndefined (WorkflowExecutionTerminatedEventAttributes)
  , "WorkflowExecutionCancelRequestedEventAttributes'" :: NullOrUndefined (WorkflowExecutionCancelRequestedEventAttributes)
  , "DecisionTaskScheduledEventAttributes'" :: NullOrUndefined (DecisionTaskScheduledEventAttributes)
  , "DecisionTaskStartedEventAttributes'" :: NullOrUndefined (DecisionTaskStartedEventAttributes)
  , "DecisionTaskCompletedEventAttributes'" :: NullOrUndefined (DecisionTaskCompletedEventAttributes)
  , "DecisionTaskTimedOutEventAttributes'" :: NullOrUndefined (DecisionTaskTimedOutEventAttributes)
  , "ActivityTaskScheduledEventAttributes'" :: NullOrUndefined (ActivityTaskScheduledEventAttributes)
  , "ActivityTaskStartedEventAttributes'" :: NullOrUndefined (ActivityTaskStartedEventAttributes)
  , "ActivityTaskCompletedEventAttributes'" :: NullOrUndefined (ActivityTaskCompletedEventAttributes)
  , "ActivityTaskFailedEventAttributes'" :: NullOrUndefined (ActivityTaskFailedEventAttributes)
  , "ActivityTaskTimedOutEventAttributes'" :: NullOrUndefined (ActivityTaskTimedOutEventAttributes)
  , "ActivityTaskCanceledEventAttributes'" :: NullOrUndefined (ActivityTaskCanceledEventAttributes)
  , "ActivityTaskCancelRequestedEventAttributes'" :: NullOrUndefined (ActivityTaskCancelRequestedEventAttributes)
  , "WorkflowExecutionSignaledEventAttributes'" :: NullOrUndefined (WorkflowExecutionSignaledEventAttributes)
  , "MarkerRecordedEventAttributes'" :: NullOrUndefined (MarkerRecordedEventAttributes)
  , "RecordMarkerFailedEventAttributes'" :: NullOrUndefined (RecordMarkerFailedEventAttributes)
  , "TimerStartedEventAttributes'" :: NullOrUndefined (TimerStartedEventAttributes)
  , "TimerFiredEventAttributes'" :: NullOrUndefined (TimerFiredEventAttributes)
  , "TimerCanceledEventAttributes'" :: NullOrUndefined (TimerCanceledEventAttributes)
  , "StartChildWorkflowExecutionInitiatedEventAttributes'" :: NullOrUndefined (StartChildWorkflowExecutionInitiatedEventAttributes)
  , "ChildWorkflowExecutionStartedEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionStartedEventAttributes)
  , "ChildWorkflowExecutionCompletedEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionCompletedEventAttributes)
  , "ChildWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionFailedEventAttributes)
  , "ChildWorkflowExecutionTimedOutEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionTimedOutEventAttributes)
  , "ChildWorkflowExecutionCanceledEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionCanceledEventAttributes)
  , "ChildWorkflowExecutionTerminatedEventAttributes'" :: NullOrUndefined (ChildWorkflowExecutionTerminatedEventAttributes)
  , "SignalExternalWorkflowExecutionInitiatedEventAttributes'" :: NullOrUndefined (SignalExternalWorkflowExecutionInitiatedEventAttributes)
  , "ExternalWorkflowExecutionSignaledEventAttributes'" :: NullOrUndefined (ExternalWorkflowExecutionSignaledEventAttributes)
  , "SignalExternalWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (SignalExternalWorkflowExecutionFailedEventAttributes)
  , "ExternalWorkflowExecutionCancelRequestedEventAttributes'" :: NullOrUndefined (ExternalWorkflowExecutionCancelRequestedEventAttributes)
  , "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'" :: NullOrUndefined (RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
  , "RequestCancelExternalWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (RequestCancelExternalWorkflowExecutionFailedEventAttributes)
  , "ScheduleActivityTaskFailedEventAttributes'" :: NullOrUndefined (ScheduleActivityTaskFailedEventAttributes)
  , "RequestCancelActivityTaskFailedEventAttributes'" :: NullOrUndefined (RequestCancelActivityTaskFailedEventAttributes)
  , "StartTimerFailedEventAttributes'" :: NullOrUndefined (StartTimerFailedEventAttributes)
  , "CancelTimerFailedEventAttributes'" :: NullOrUndefined (CancelTimerFailedEventAttributes)
  , "StartChildWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined (StartChildWorkflowExecutionFailedEventAttributes)
  , "LambdaFunctionScheduledEventAttributes'" :: NullOrUndefined (LambdaFunctionScheduledEventAttributes)
  , "LambdaFunctionStartedEventAttributes'" :: NullOrUndefined (LambdaFunctionStartedEventAttributes)
  , "LambdaFunctionCompletedEventAttributes'" :: NullOrUndefined (LambdaFunctionCompletedEventAttributes)
  , "LambdaFunctionFailedEventAttributes'" :: NullOrUndefined (LambdaFunctionFailedEventAttributes)
  , "LambdaFunctionTimedOutEventAttributes'" :: NullOrUndefined (LambdaFunctionTimedOutEventAttributes)
  , "ScheduleLambdaFunctionFailedEventAttributes'" :: NullOrUndefined (ScheduleLambdaFunctionFailedEventAttributes)
  , "StartLambdaFunctionFailedEventAttributes'" :: NullOrUndefined (StartLambdaFunctionFailedEventAttributes)
  }
derive instance newtypeHistoryEvent :: Newtype HistoryEvent _


newtype HistoryEventList = HistoryEventList (Array HistoryEvent)
derive instance newtypeHistoryEventList :: Newtype HistoryEventList _


newtype Identity = Identity String
derive instance newtypeIdentity :: Newtype Identity _


-- | <p>Provides the details of the <code>LambdaFunctionCompleted</code> event. It isn't set for other event types.</p>
newtype LambdaFunctionCompletedEventAttributes = LambdaFunctionCompletedEventAttributes 
  { "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "Result'" :: NullOrUndefined (Data)
  }
derive instance newtypeLambdaFunctionCompletedEventAttributes :: Newtype LambdaFunctionCompletedEventAttributes _


-- | <p>Provides the details of the <code>LambdaFunctionFailed</code> event. It isn't set for other event types.</p>
newtype LambdaFunctionFailedEventAttributes = LambdaFunctionFailedEventAttributes 
  { "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "Reason'" :: NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined (Data)
  }
derive instance newtypeLambdaFunctionFailedEventAttributes :: Newtype LambdaFunctionFailedEventAttributes _


-- | <p>Provides the details of the <code>LambdaFunctionScheduled</code> event. It isn't set for other event types.</p>
newtype LambdaFunctionScheduledEventAttributes = LambdaFunctionScheduledEventAttributes 
  { "Id'" :: (FunctionId)
  , "Name'" :: (FunctionName)
  , "Control'" :: NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined (FunctionInput)
  , "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeLambdaFunctionScheduledEventAttributes :: Newtype LambdaFunctionScheduledEventAttributes _


-- | <p>Provides the details of the <code>LambdaFunctionStarted</code> event. It isn't set for other event types.</p>
newtype LambdaFunctionStartedEventAttributes = LambdaFunctionStartedEventAttributes 
  { "ScheduledEventId'" :: (EventId)
  }
derive instance newtypeLambdaFunctionStartedEventAttributes :: Newtype LambdaFunctionStartedEventAttributes _


-- | <p>Provides details of the <code>LambdaFunctionTimedOut</code> event.</p>
newtype LambdaFunctionTimedOutEventAttributes = LambdaFunctionTimedOutEventAttributes 
  { "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "TimeoutType'" :: NullOrUndefined (LambdaFunctionTimeoutType)
  }
derive instance newtypeLambdaFunctionTimedOutEventAttributes :: Newtype LambdaFunctionTimedOutEventAttributes _


newtype LambdaFunctionTimeoutType = LambdaFunctionTimeoutType String
derive instance newtypeLambdaFunctionTimeoutType :: Newtype LambdaFunctionTimeoutType _


-- | <p>Returned by any operation if a system imposed limitation has been reached. To address this fault you should either clean up unused resources or increase the limit by contacting AWS.</p>
newtype LimitExceededFault = LimitExceededFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededFault :: Newtype LimitExceededFault _


newtype LimitedData = LimitedData String
derive instance newtypeLimitedData :: Newtype LimitedData _


newtype ListActivityTypesInput = ListActivityTypesInput 
  { "Domain'" :: (DomainName)
  , "Name'" :: NullOrUndefined (Name)
  , "RegistrationStatus'" :: (RegistrationStatus)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined (ReverseOrder)
  }
derive instance newtypeListActivityTypesInput :: Newtype ListActivityTypesInput _


newtype ListClosedWorkflowExecutionsInput = ListClosedWorkflowExecutionsInput 
  { "Domain'" :: (DomainName)
  , "StartTimeFilter'" :: NullOrUndefined (ExecutionTimeFilter)
  , "CloseTimeFilter'" :: NullOrUndefined (ExecutionTimeFilter)
  , "ExecutionFilter'" :: NullOrUndefined (WorkflowExecutionFilter)
  , "CloseStatusFilter'" :: NullOrUndefined (CloseStatusFilter)
  , "TypeFilter'" :: NullOrUndefined (WorkflowTypeFilter)
  , "TagFilter'" :: NullOrUndefined (TagFilter)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined (ReverseOrder)
  }
derive instance newtypeListClosedWorkflowExecutionsInput :: Newtype ListClosedWorkflowExecutionsInput _


newtype ListDomainsInput = ListDomainsInput 
  { "NextPageToken'" :: NullOrUndefined (PageToken)
  , "RegistrationStatus'" :: (RegistrationStatus)
  , "MaximumPageSize'" :: NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined (ReverseOrder)
  }
derive instance newtypeListDomainsInput :: Newtype ListDomainsInput _


newtype ListOpenWorkflowExecutionsInput = ListOpenWorkflowExecutionsInput 
  { "Domain'" :: (DomainName)
  , "StartTimeFilter'" :: (ExecutionTimeFilter)
  , "TypeFilter'" :: NullOrUndefined (WorkflowTypeFilter)
  , "TagFilter'" :: NullOrUndefined (TagFilter)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined (ReverseOrder)
  , "ExecutionFilter'" :: NullOrUndefined (WorkflowExecutionFilter)
  }
derive instance newtypeListOpenWorkflowExecutionsInput :: Newtype ListOpenWorkflowExecutionsInput _


newtype ListWorkflowTypesInput = ListWorkflowTypesInput 
  { "Domain'" :: (DomainName)
  , "Name'" :: NullOrUndefined (Name)
  , "RegistrationStatus'" :: (RegistrationStatus)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined (ReverseOrder)
  }
derive instance newtypeListWorkflowTypesInput :: Newtype ListWorkflowTypesInput _


newtype MarkerName = MarkerName String
derive instance newtypeMarkerName :: Newtype MarkerName _


-- | <p>Provides the details of the <code>MarkerRecorded</code> event.</p>
newtype MarkerRecordedEventAttributes = MarkerRecordedEventAttributes 
  { "MarkerName'" :: (MarkerName)
  , "Details'" :: NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeMarkerRecordedEventAttributes :: Newtype MarkerRecordedEventAttributes _


newtype Name = Name String
derive instance newtypeName :: Newtype Name _


newtype OpenDecisionTasksCount = OpenDecisionTasksCount Int
derive instance newtypeOpenDecisionTasksCount :: Newtype OpenDecisionTasksCount _


-- | <p>Returned when the caller doesn't have sufficient permissions to invoke the action.</p>
newtype OperationNotPermittedFault = OperationNotPermittedFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeOperationNotPermittedFault :: Newtype OperationNotPermittedFault _


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _


newtype PageToken = PageToken String
derive instance newtypePageToken :: Newtype PageToken _


-- | <p>Contains the count of tasks in a task list.</p>
newtype PendingTaskCount = PendingTaskCount 
  { "Count'" :: (Count)
  , "Truncated'" :: NullOrUndefined (Truncated)
  }
derive instance newtypePendingTaskCount :: Newtype PendingTaskCount _


newtype PollForActivityTaskInput = PollForActivityTaskInput 
  { "Domain'" :: (DomainName)
  , "TaskList'" :: (TaskList)
  , "Identity'" :: NullOrUndefined (Identity)
  }
derive instance newtypePollForActivityTaskInput :: Newtype PollForActivityTaskInput _


newtype PollForDecisionTaskInput = PollForDecisionTaskInput 
  { "Domain'" :: (DomainName)
  , "TaskList'" :: (TaskList)
  , "Identity'" :: NullOrUndefined (Identity)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined (ReverseOrder)
  }
derive instance newtypePollForDecisionTaskInput :: Newtype PollForDecisionTaskInput _


newtype RecordActivityTaskHeartbeatInput = RecordActivityTaskHeartbeatInput 
  { "TaskToken'" :: (TaskToken)
  , "Details'" :: NullOrUndefined (LimitedData)
  }
derive instance newtypeRecordActivityTaskHeartbeatInput :: Newtype RecordActivityTaskHeartbeatInput _


-- | <p>Provides the details of the <code>RecordMarker</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes 
  { "MarkerName'" :: (MarkerName)
  , "Details'" :: NullOrUndefined (Data)
  }
derive instance newtypeRecordMarkerDecisionAttributes :: Newtype RecordMarkerDecisionAttributes _


newtype RecordMarkerFailedCause = RecordMarkerFailedCause String
derive instance newtypeRecordMarkerFailedCause :: Newtype RecordMarkerFailedCause _


-- | <p>Provides the details of the <code>RecordMarkerFailed</code> event.</p>
newtype RecordMarkerFailedEventAttributes = RecordMarkerFailedEventAttributes 
  { "MarkerName'" :: (MarkerName)
  , "Cause'" :: (RecordMarkerFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeRecordMarkerFailedEventAttributes :: Newtype RecordMarkerFailedEventAttributes _


newtype RegisterActivityTypeInput = RegisterActivityTypeInput 
  { "Domain'" :: (DomainName)
  , "Name'" :: (Name)
  , "Version'" :: (Version)
  , "Description'" :: NullOrUndefined (Description)
  , "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskHeartbeatTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskList'" :: NullOrUndefined (TaskList)
  , "DefaultTaskPriority'" :: NullOrUndefined (TaskPriority)
  , "DefaultTaskScheduleToStartTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskScheduleToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeRegisterActivityTypeInput :: Newtype RegisterActivityTypeInput _


newtype RegisterDomainInput = RegisterDomainInput 
  { "Name'" :: (DomainName)
  , "Description'" :: NullOrUndefined (Description)
  , "WorkflowExecutionRetentionPeriodInDays'" :: (DurationInDays)
  }
derive instance newtypeRegisterDomainInput :: Newtype RegisterDomainInput _


newtype RegisterWorkflowTypeInput = RegisterWorkflowTypeInput 
  { "Domain'" :: (DomainName)
  , "Name'" :: (Name)
  , "Version'" :: (Version)
  , "Description'" :: NullOrUndefined (Description)
  , "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskList'" :: NullOrUndefined (TaskList)
  , "DefaultTaskPriority'" :: NullOrUndefined (TaskPriority)
  , "DefaultChildPolicy'" :: NullOrUndefined (ChildPolicy)
  , "DefaultLambdaRole'" :: NullOrUndefined (Arn)
  }
derive instance newtypeRegisterWorkflowTypeInput :: Newtype RegisterWorkflowTypeInput _


newtype RegistrationStatus = RegistrationStatus String
derive instance newtypeRegistrationStatus :: Newtype RegistrationStatus _


-- | <p>Provides the details of the <code>RequestCancelActivityTask</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes 
  { "ActivityId'" :: (ActivityId)
  }
derive instance newtypeRequestCancelActivityTaskDecisionAttributes :: Newtype RequestCancelActivityTaskDecisionAttributes _


newtype RequestCancelActivityTaskFailedCause = RequestCancelActivityTaskFailedCause String
derive instance newtypeRequestCancelActivityTaskFailedCause :: Newtype RequestCancelActivityTaskFailedCause _


-- | <p>Provides the details of the <code>RequestCancelActivityTaskFailed</code> event.</p>
newtype RequestCancelActivityTaskFailedEventAttributes = RequestCancelActivityTaskFailedEventAttributes 
  { "ActivityId'" :: (ActivityId)
  , "Cause'" :: (RequestCancelActivityTaskFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeRequestCancelActivityTaskFailedEventAttributes :: Newtype RequestCancelActivityTaskFailedEventAttributes _


-- | <p>Provides the details of the <code>RequestCancelExternalWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  , "Control'" :: NullOrUndefined (Data)
  }
derive instance newtypeRequestCancelExternalWorkflowExecutionDecisionAttributes :: Newtype RequestCancelExternalWorkflowExecutionDecisionAttributes _


newtype RequestCancelExternalWorkflowExecutionFailedCause = RequestCancelExternalWorkflowExecutionFailedCause String
derive instance newtypeRequestCancelExternalWorkflowExecutionFailedCause :: Newtype RequestCancelExternalWorkflowExecutionFailedCause _


-- | <p>Provides the details of the <code>RequestCancelExternalWorkflowExecutionFailed</code> event.</p>
newtype RequestCancelExternalWorkflowExecutionFailedEventAttributes = RequestCancelExternalWorkflowExecutionFailedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  , "Cause'" :: (RequestCancelExternalWorkflowExecutionFailedCause)
  , "InitiatedEventId'" :: (EventId)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined (Data)
  }
derive instance newtypeRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Newtype RequestCancelExternalWorkflowExecutionFailedEventAttributes _


-- | <p>Provides the details of the <code>RequestCancelExternalWorkflowExecutionInitiated</code> event.</p>
newtype RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined (Data)
  }
derive instance newtypeRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Newtype RequestCancelExternalWorkflowExecutionInitiatedEventAttributes _


newtype RequestCancelWorkflowExecutionInput = RequestCancelWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  }
derive instance newtypeRequestCancelWorkflowExecutionInput :: Newtype RequestCancelWorkflowExecutionInput _


newtype RespondActivityTaskCanceledInput = RespondActivityTaskCanceledInput 
  { "TaskToken'" :: (TaskToken)
  , "Details'" :: NullOrUndefined (Data)
  }
derive instance newtypeRespondActivityTaskCanceledInput :: Newtype RespondActivityTaskCanceledInput _


newtype RespondActivityTaskCompletedInput = RespondActivityTaskCompletedInput 
  { "TaskToken'" :: (TaskToken)
  , "Result'" :: NullOrUndefined (Data)
  }
derive instance newtypeRespondActivityTaskCompletedInput :: Newtype RespondActivityTaskCompletedInput _


newtype RespondActivityTaskFailedInput = RespondActivityTaskFailedInput 
  { "TaskToken'" :: (TaskToken)
  , "Reason'" :: NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined (Data)
  }
derive instance newtypeRespondActivityTaskFailedInput :: Newtype RespondActivityTaskFailedInput _


-- | <p>Input data for a TaskCompleted response to a decision task.</p>
newtype RespondDecisionTaskCompletedInput = RespondDecisionTaskCompletedInput 
  { "TaskToken'" :: (TaskToken)
  , "Decisions'" :: NullOrUndefined (DecisionList)
  , "ExecutionContext'" :: NullOrUndefined (Data)
  }
derive instance newtypeRespondDecisionTaskCompletedInput :: Newtype RespondDecisionTaskCompletedInput _


newtype ReverseOrder = ReverseOrder Boolean
derive instance newtypeReverseOrder :: Newtype ReverseOrder _


-- | <p>Specifies the <code>runId</code> of a workflow execution.</p>
newtype Run = Run 
  { "RunId'" :: NullOrUndefined (WorkflowRunId)
  }
derive instance newtypeRun :: Newtype Run _


-- | <p>Provides the details of the <code>ScheduleActivityTask</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>activityType.name</code> – String constraint. The key is <code>swf:activityType.name</code>.</p> </li> <li> <p> <code>activityType.version</code> – String constraint. The key is <code>swf:activityType.version</code>.</p> </li> <li> <p> <code>taskList</code> – String constraint. The key is <code>swf:taskList.name</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes 
  { "ActivityType'" :: (ActivityType)
  , "ActivityId'" :: (ActivityId)
  , "Control'" :: NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined (Data)
  , "ScheduleToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: NullOrUndefined (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "ScheduleToStartTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "HeartbeatTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeScheduleActivityTaskDecisionAttributes :: Newtype ScheduleActivityTaskDecisionAttributes _


newtype ScheduleActivityTaskFailedCause = ScheduleActivityTaskFailedCause String
derive instance newtypeScheduleActivityTaskFailedCause :: Newtype ScheduleActivityTaskFailedCause _


-- | <p>Provides the details of the <code>ScheduleActivityTaskFailed</code> event.</p>
newtype ScheduleActivityTaskFailedEventAttributes = ScheduleActivityTaskFailedEventAttributes 
  { "ActivityType'" :: (ActivityType)
  , "ActivityId'" :: (ActivityId)
  , "Cause'" :: (ScheduleActivityTaskFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeScheduleActivityTaskFailedEventAttributes :: Newtype ScheduleActivityTaskFailedEventAttributes _


-- | <p>Decision attributes specified in <code>scheduleLambdaFunctionDecisionAttributes</code> within the list of decisions <code>decisions</code> passed to <a>RespondDecisionTaskCompleted</a>.</p>
newtype ScheduleLambdaFunctionDecisionAttributes = ScheduleLambdaFunctionDecisionAttributes 
  { "Id'" :: (FunctionId)
  , "Name'" :: (FunctionName)
  , "Control'" :: NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined (FunctionInput)
  , "StartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeScheduleLambdaFunctionDecisionAttributes :: Newtype ScheduleLambdaFunctionDecisionAttributes _


newtype ScheduleLambdaFunctionFailedCause = ScheduleLambdaFunctionFailedCause String
derive instance newtypeScheduleLambdaFunctionFailedCause :: Newtype ScheduleLambdaFunctionFailedCause _


-- | <p>Provides the details of the <code>ScheduleLambdaFunctionFailed</code> event. It isn't set for other event types.</p>
newtype ScheduleLambdaFunctionFailedEventAttributes = ScheduleLambdaFunctionFailedEventAttributes 
  { "Id'" :: (FunctionId)
  , "Name'" :: (FunctionName)
  , "Cause'" :: (ScheduleLambdaFunctionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeScheduleLambdaFunctionFailedEventAttributes :: Newtype ScheduleLambdaFunctionFailedEventAttributes _


-- | <p>Provides the details of the <code>SignalExternalWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  , "SignalName'" :: (SignalName)
  , "Input'" :: NullOrUndefined (Data)
  , "Control'" :: NullOrUndefined (Data)
  }
derive instance newtypeSignalExternalWorkflowExecutionDecisionAttributes :: Newtype SignalExternalWorkflowExecutionDecisionAttributes _


newtype SignalExternalWorkflowExecutionFailedCause = SignalExternalWorkflowExecutionFailedCause String
derive instance newtypeSignalExternalWorkflowExecutionFailedCause :: Newtype SignalExternalWorkflowExecutionFailedCause _


-- | <p>Provides the details of the <code>SignalExternalWorkflowExecutionFailed</code> event.</p>
newtype SignalExternalWorkflowExecutionFailedEventAttributes = SignalExternalWorkflowExecutionFailedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  , "Cause'" :: (SignalExternalWorkflowExecutionFailedCause)
  , "InitiatedEventId'" :: (EventId)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined (Data)
  }
derive instance newtypeSignalExternalWorkflowExecutionFailedEventAttributes :: Newtype SignalExternalWorkflowExecutionFailedEventAttributes _


-- | <p>Provides the details of the <code>SignalExternalWorkflowExecutionInitiated</code> event.</p>
newtype SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  , "SignalName'" :: (SignalName)
  , "Input'" :: NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined (Data)
  }
derive instance newtypeSignalExternalWorkflowExecutionInitiatedEventAttributes :: Newtype SignalExternalWorkflowExecutionInitiatedEventAttributes _


newtype SignalName = SignalName String
derive instance newtypeSignalName :: Newtype SignalName _


newtype SignalWorkflowExecutionInput = SignalWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  , "SignalName'" :: (SignalName)
  , "Input'" :: NullOrUndefined (Data)
  }
derive instance newtypeSignalWorkflowExecutionInput :: Newtype SignalWorkflowExecutionInput _


-- | <p>Provides the details of the <code>StartChildWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagList.member.N</code> – The key is "swf:tagList.N" where N is the tag number from 0 to 4, inclusive.</p> </li> <li> <p> <code>taskList</code> – String constraint. The key is <code>swf:taskList.name</code>.</p> </li> <li> <p> <code>workflowType.name</code> – String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code> – String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes 
  { "WorkflowType'" :: (WorkflowType)
  , "WorkflowId'" :: (WorkflowId)
  , "Control'" :: NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: NullOrUndefined (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: NullOrUndefined (ChildPolicy)
  , "TagList'" :: NullOrUndefined (TagList)
  , "LambdaRole'" :: NullOrUndefined (Arn)
  }
derive instance newtypeStartChildWorkflowExecutionDecisionAttributes :: Newtype StartChildWorkflowExecutionDecisionAttributes _


newtype StartChildWorkflowExecutionFailedCause = StartChildWorkflowExecutionFailedCause String
derive instance newtypeStartChildWorkflowExecutionFailedCause :: Newtype StartChildWorkflowExecutionFailedCause _


-- | <p>Provides the details of the <code>StartChildWorkflowExecutionFailed</code> event.</p>
newtype StartChildWorkflowExecutionFailedEventAttributes = StartChildWorkflowExecutionFailedEventAttributes 
  { "WorkflowType'" :: (WorkflowType)
  , "Cause'" :: (StartChildWorkflowExecutionFailedCause)
  , "WorkflowId'" :: (WorkflowId)
  , "InitiatedEventId'" :: (EventId)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined (Data)
  }
derive instance newtypeStartChildWorkflowExecutionFailedEventAttributes :: Newtype StartChildWorkflowExecutionFailedEventAttributes _


-- | <p>Provides the details of the <code>StartChildWorkflowExecutionInitiated</code> event.</p>
newtype StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "WorkflowType'" :: (WorkflowType)
  , "Control'" :: NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "ChildPolicy'" :: (ChildPolicy)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "TagList'" :: NullOrUndefined (TagList)
  , "LambdaRole'" :: NullOrUndefined (Arn)
  }
derive instance newtypeStartChildWorkflowExecutionInitiatedEventAttributes :: Newtype StartChildWorkflowExecutionInitiatedEventAttributes _


newtype StartLambdaFunctionFailedCause = StartLambdaFunctionFailedCause String
derive instance newtypeStartLambdaFunctionFailedCause :: Newtype StartLambdaFunctionFailedCause _


-- | <p>Provides the details of the <code>StartLambdaFunctionFailed</code> event. It isn't set for other event types.</p>
newtype StartLambdaFunctionFailedEventAttributes = StartLambdaFunctionFailedEventAttributes 
  { "ScheduledEventId'" :: NullOrUndefined (EventId)
  , "Cause'" :: NullOrUndefined (StartLambdaFunctionFailedCause)
  , "Message'" :: NullOrUndefined (CauseMessage)
  }
derive instance newtypeStartLambdaFunctionFailedEventAttributes :: Newtype StartLambdaFunctionFailedEventAttributes _


-- | <p>Provides the details of the <code>StartTimer</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype StartTimerDecisionAttributes = StartTimerDecisionAttributes 
  { "TimerId'" :: (TimerId)
  , "Control'" :: NullOrUndefined (Data)
  , "StartToFireTimeout'" :: (DurationInSeconds)
  }
derive instance newtypeStartTimerDecisionAttributes :: Newtype StartTimerDecisionAttributes _


newtype StartTimerFailedCause = StartTimerFailedCause String
derive instance newtypeStartTimerFailedCause :: Newtype StartTimerFailedCause _


-- | <p>Provides the details of the <code>StartTimerFailed</code> event.</p>
newtype StartTimerFailedEventAttributes = StartTimerFailedEventAttributes 
  { "TimerId'" :: (TimerId)
  , "Cause'" :: (StartTimerFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeStartTimerFailedEventAttributes :: Newtype StartTimerFailedEventAttributes _


newtype StartWorkflowExecutionInput = StartWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "WorkflowId'" :: (WorkflowId)
  , "WorkflowType'" :: (WorkflowType)
  , "TaskList'" :: NullOrUndefined (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "Input'" :: NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "TagList'" :: NullOrUndefined (TagList)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: NullOrUndefined (ChildPolicy)
  , "LambdaRole'" :: NullOrUndefined (Arn)
  }
derive instance newtypeStartWorkflowExecutionInput :: Newtype StartWorkflowExecutionInput _


newtype Tag = Tag String
derive instance newtypeTag :: Newtype Tag _


-- | <p>Used to filter the workflow executions in visibility APIs based on a tag.</p>
newtype TagFilter = TagFilter 
  { "Tag'" :: (Tag)
  }
derive instance newtypeTagFilter :: Newtype TagFilter _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


-- | <p>Represents a task list.</p>
newtype TaskList = TaskList 
  { "Name'" :: (Name)
  }
derive instance newtypeTaskList :: Newtype TaskList _


newtype TaskPriority = TaskPriority String
derive instance newtypeTaskPriority :: Newtype TaskPriority _


newtype TaskToken = TaskToken String
derive instance newtypeTaskToken :: Newtype TaskToken _


newtype TerminateReason = TerminateReason String
derive instance newtypeTerminateReason :: Newtype TerminateReason _


newtype TerminateWorkflowExecutionInput = TerminateWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  , "Reason'" :: NullOrUndefined (TerminateReason)
  , "Details'" :: NullOrUndefined (Data)
  , "ChildPolicy'" :: NullOrUndefined (ChildPolicy)
  }
derive instance newtypeTerminateWorkflowExecutionInput :: Newtype TerminateWorkflowExecutionInput _


-- | <p> Provides the details of the <code>TimerCanceled</code> event. </p>
newtype TimerCanceledEventAttributes = TimerCanceledEventAttributes 
  { "TimerId'" :: (TimerId)
  , "StartedEventId'" :: (EventId)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeTimerCanceledEventAttributes :: Newtype TimerCanceledEventAttributes _


-- | <p>Provides the details of the <code>TimerFired</code> event.</p>
newtype TimerFiredEventAttributes = TimerFiredEventAttributes 
  { "TimerId'" :: (TimerId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeTimerFiredEventAttributes :: Newtype TimerFiredEventAttributes _


newtype TimerId = TimerId String
derive instance newtypeTimerId :: Newtype TimerId _


-- | <p>Provides the details of the <code>TimerStarted</code> event.</p>
newtype TimerStartedEventAttributes = TimerStartedEventAttributes 
  { "TimerId'" :: (TimerId)
  , "Control'" :: NullOrUndefined (Data)
  , "StartToFireTimeout'" :: (DurationInSeconds)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeTimerStartedEventAttributes :: Newtype TimerStartedEventAttributes _


newtype Truncated = Truncated Boolean
derive instance newtypeTruncated :: Newtype Truncated _


-- | <p>Returned if the type already exists in the specified domain. You get this fault even if the existing type is in deprecated status. You can specify another version if the intent is to create a new distinct version of the type.</p>
newtype TypeAlreadyExistsFault = TypeAlreadyExistsFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTypeAlreadyExistsFault :: Newtype TypeAlreadyExistsFault _


-- | <p>Returned when the specified activity or workflow type was already deprecated.</p>
newtype TypeDeprecatedFault = TypeDeprecatedFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTypeDeprecatedFault :: Newtype TypeDeprecatedFault _


-- | <p>Returned when the named resource cannot be found with in the scope of this operation (region or domain). This could happen if the named resource was never created or is no longer available for this operation.</p>
newtype UnknownResourceFault = UnknownResourceFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnknownResourceFault :: Newtype UnknownResourceFault _


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _


newtype VersionOptional = VersionOptional String
derive instance newtypeVersionOptional :: Newtype VersionOptional _


-- | <p>Represents a workflow execution.</p>
newtype WorkflowExecution = WorkflowExecution 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: (WorkflowRunId)
  }
derive instance newtypeWorkflowExecution :: Newtype WorkflowExecution _


-- | <p>Returned by <a>StartWorkflowExecution</a> when an open execution with the same workflowId is already running in the specified domain.</p>
newtype WorkflowExecutionAlreadyStartedFault = WorkflowExecutionAlreadyStartedFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeWorkflowExecutionAlreadyStartedFault :: Newtype WorkflowExecutionAlreadyStartedFault _


newtype WorkflowExecutionCancelRequestedCause = WorkflowExecutionCancelRequestedCause String
derive instance newtypeWorkflowExecutionCancelRequestedCause :: Newtype WorkflowExecutionCancelRequestedCause _


-- | <p>Provides the details of the <code>WorkflowExecutionCancelRequested</code> event.</p>
newtype WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes 
  { "ExternalWorkflowExecution'" :: NullOrUndefined (WorkflowExecution)
  , "ExternalInitiatedEventId'" :: NullOrUndefined (EventId)
  , "Cause'" :: NullOrUndefined (WorkflowExecutionCancelRequestedCause)
  }
derive instance newtypeWorkflowExecutionCancelRequestedEventAttributes :: Newtype WorkflowExecutionCancelRequestedEventAttributes _


-- | <p>Provides the details of the <code>WorkflowExecutionCanceled</code> event.</p>
newtype WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes 
  { "Details'" :: NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeWorkflowExecutionCanceledEventAttributes :: Newtype WorkflowExecutionCanceledEventAttributes _


-- | <p>Provides the details of the <code>WorkflowExecutionCompleted</code> event.</p>
newtype WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes 
  { "Result'" :: NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeWorkflowExecutionCompletedEventAttributes :: Newtype WorkflowExecutionCompletedEventAttributes _


-- | <p>The configuration settings for a workflow execution including timeout values, tasklist etc. These configuration settings are determined from the defaults specified when registering the workflow type and those specified when starting the workflow execution.</p>
newtype WorkflowExecutionConfiguration = WorkflowExecutionConfiguration 
  { "TaskStartToCloseTimeout'" :: (DurationInSeconds)
  , "ExecutionStartToCloseTimeout'" :: (DurationInSeconds)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "ChildPolicy'" :: (ChildPolicy)
  , "LambdaRole'" :: NullOrUndefined (Arn)
  }
derive instance newtypeWorkflowExecutionConfiguration :: Newtype WorkflowExecutionConfiguration _


-- | <p>Provides the details of the <code>WorkflowExecutionContinuedAsNew</code> event.</p>
newtype WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes 
  { "Input'" :: NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "NewExecutionRunId'" :: (WorkflowRunId)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: (ChildPolicy)
  , "TagList'" :: NullOrUndefined (TagList)
  , "WorkflowType'" :: (WorkflowType)
  , "LambdaRole'" :: NullOrUndefined (Arn)
  }
derive instance newtypeWorkflowExecutionContinuedAsNewEventAttributes :: Newtype WorkflowExecutionContinuedAsNewEventAttributes _


-- | <p>Contains the count of workflow executions returned from <a>CountOpenWorkflowExecutions</a> or <a>CountClosedWorkflowExecutions</a> </p>
newtype WorkflowExecutionCount = WorkflowExecutionCount 
  { "Count'" :: (Count)
  , "Truncated'" :: NullOrUndefined (Truncated)
  }
derive instance newtypeWorkflowExecutionCount :: Newtype WorkflowExecutionCount _


-- | <p>Contains details about a workflow execution.</p>
newtype WorkflowExecutionDetail = WorkflowExecutionDetail 
  { "ExecutionInfo'" :: (WorkflowExecutionInfo)
  , "ExecutionConfiguration'" :: (WorkflowExecutionConfiguration)
  , "OpenCounts'" :: (WorkflowExecutionOpenCounts)
  , "LatestActivityTaskTimestamp'" :: NullOrUndefined (Number)
  , "LatestExecutionContext'" :: NullOrUndefined (Data)
  }
derive instance newtypeWorkflowExecutionDetail :: Newtype WorkflowExecutionDetail _


-- | <p>Provides the details of the <code>WorkflowExecutionFailed</code> event.</p>
newtype WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes 
  { "Reason'" :: NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeWorkflowExecutionFailedEventAttributes :: Newtype WorkflowExecutionFailedEventAttributes _


-- | <p>Used to filter the workflow executions in visibility APIs by their <code>workflowId</code>.</p>
newtype WorkflowExecutionFilter = WorkflowExecutionFilter 
  { "WorkflowId'" :: (WorkflowId)
  }
derive instance newtypeWorkflowExecutionFilter :: Newtype WorkflowExecutionFilter _


-- | <p>Contains information about a workflow execution.</p>
newtype WorkflowExecutionInfo = WorkflowExecutionInfo 
  { "Execution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "StartTimestamp'" :: (Number)
  , "CloseTimestamp'" :: NullOrUndefined (Number)
  , "ExecutionStatus'" :: (ExecutionStatus)
  , "CloseStatus'" :: NullOrUndefined (CloseStatus)
  , "Parent'" :: NullOrUndefined (WorkflowExecution)
  , "TagList'" :: NullOrUndefined (TagList)
  , "CancelRequested'" :: NullOrUndefined (Canceled)
  }
derive instance newtypeWorkflowExecutionInfo :: Newtype WorkflowExecutionInfo _


newtype WorkflowExecutionInfoList = WorkflowExecutionInfoList (Array WorkflowExecutionInfo)
derive instance newtypeWorkflowExecutionInfoList :: Newtype WorkflowExecutionInfoList _


-- | <p>Contains a paginated list of information about workflow executions.</p>
newtype WorkflowExecutionInfos = WorkflowExecutionInfos 
  { "ExecutionInfos'" :: (WorkflowExecutionInfoList)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeWorkflowExecutionInfos :: Newtype WorkflowExecutionInfos _


-- | <p>Contains the counts of open tasks, child workflow executions and timers for a workflow execution.</p>
newtype WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts 
  { "OpenActivityTasks'" :: (Count)
  , "OpenDecisionTasks'" :: (OpenDecisionTasksCount)
  , "OpenTimers'" :: (Count)
  , "OpenChildWorkflowExecutions'" :: (Count)
  , "OpenLambdaFunctions'" :: NullOrUndefined (Count)
  }
derive instance newtypeWorkflowExecutionOpenCounts :: Newtype WorkflowExecutionOpenCounts _


-- | <p>Provides the details of the <code>WorkflowExecutionSignaled</code> event.</p>
newtype WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes 
  { "SignalName'" :: (SignalName)
  , "Input'" :: NullOrUndefined (Data)
  , "ExternalWorkflowExecution'" :: NullOrUndefined (WorkflowExecution)
  , "ExternalInitiatedEventId'" :: NullOrUndefined (EventId)
  }
derive instance newtypeWorkflowExecutionSignaledEventAttributes :: Newtype WorkflowExecutionSignaledEventAttributes _


-- | <p>Provides details of <code>WorkflowExecutionStarted</code> event.</p>
newtype WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes 
  { "Input'" :: NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: (ChildPolicy)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined (TaskPriority)
  , "WorkflowType'" :: (WorkflowType)
  , "TagList'" :: NullOrUndefined (TagList)
  , "ContinuedExecutionRunId'" :: NullOrUndefined (WorkflowRunIdOptional)
  , "ParentWorkflowExecution'" :: NullOrUndefined (WorkflowExecution)
  , "ParentInitiatedEventId'" :: NullOrUndefined (EventId)
  , "LambdaRole'" :: NullOrUndefined (Arn)
  }
derive instance newtypeWorkflowExecutionStartedEventAttributes :: Newtype WorkflowExecutionStartedEventAttributes _


newtype WorkflowExecutionTerminatedCause = WorkflowExecutionTerminatedCause String
derive instance newtypeWorkflowExecutionTerminatedCause :: Newtype WorkflowExecutionTerminatedCause _


-- | <p>Provides the details of the <code>WorkflowExecutionTerminated</code> event.</p>
newtype WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes 
  { "Reason'" :: NullOrUndefined (TerminateReason)
  , "Details'" :: NullOrUndefined (Data)
  , "ChildPolicy'" :: (ChildPolicy)
  , "Cause'" :: NullOrUndefined (WorkflowExecutionTerminatedCause)
  }
derive instance newtypeWorkflowExecutionTerminatedEventAttributes :: Newtype WorkflowExecutionTerminatedEventAttributes _


-- | <p>Provides the details of the <code>WorkflowExecutionTimedOut</code> event.</p>
newtype WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes 
  { "TimeoutType'" :: (WorkflowExecutionTimeoutType)
  , "ChildPolicy'" :: (ChildPolicy)
  }
derive instance newtypeWorkflowExecutionTimedOutEventAttributes :: Newtype WorkflowExecutionTimedOutEventAttributes _


newtype WorkflowExecutionTimeoutType = WorkflowExecutionTimeoutType String
derive instance newtypeWorkflowExecutionTimeoutType :: Newtype WorkflowExecutionTimeoutType _


newtype WorkflowId = WorkflowId String
derive instance newtypeWorkflowId :: Newtype WorkflowId _


newtype WorkflowRunId = WorkflowRunId String
derive instance newtypeWorkflowRunId :: Newtype WorkflowRunId _


newtype WorkflowRunIdOptional = WorkflowRunIdOptional String
derive instance newtypeWorkflowRunIdOptional :: Newtype WorkflowRunIdOptional _


-- | <p>Represents a workflow type.</p>
newtype WorkflowType = WorkflowType 
  { "Name'" :: (Name)
  , "Version'" :: (Version)
  }
derive instance newtypeWorkflowType :: Newtype WorkflowType _


-- | <p>The configuration settings of a workflow type.</p>
newtype WorkflowTypeConfiguration = WorkflowTypeConfiguration 
  { "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultExecutionStartToCloseTimeout'" :: NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskList'" :: NullOrUndefined (TaskList)
  , "DefaultTaskPriority'" :: NullOrUndefined (TaskPriority)
  , "DefaultChildPolicy'" :: NullOrUndefined (ChildPolicy)
  , "DefaultLambdaRole'" :: NullOrUndefined (Arn)
  }
derive instance newtypeWorkflowTypeConfiguration :: Newtype WorkflowTypeConfiguration _


-- | <p>Contains details about a workflow type.</p>
newtype WorkflowTypeDetail = WorkflowTypeDetail 
  { "TypeInfo'" :: (WorkflowTypeInfo)
  , "Configuration'" :: (WorkflowTypeConfiguration)
  }
derive instance newtypeWorkflowTypeDetail :: Newtype WorkflowTypeDetail _


-- | <p>Used to filter workflow execution query results by type. Each parameter, if specified, defines a rule that must be satisfied by each returned result.</p>
newtype WorkflowTypeFilter = WorkflowTypeFilter 
  { "Name'" :: (Name)
  , "Version'" :: NullOrUndefined (VersionOptional)
  }
derive instance newtypeWorkflowTypeFilter :: Newtype WorkflowTypeFilter _


-- | <p>Contains information about a workflow type.</p>
newtype WorkflowTypeInfo = WorkflowTypeInfo 
  { "WorkflowType'" :: (WorkflowType)
  , "Status'" :: (RegistrationStatus)
  , "Description'" :: NullOrUndefined (Description)
  , "CreationDate'" :: (Number)
  , "DeprecationDate'" :: NullOrUndefined (Number)
  }
derive instance newtypeWorkflowTypeInfo :: Newtype WorkflowTypeInfo _


newtype WorkflowTypeInfoList = WorkflowTypeInfoList (Array WorkflowTypeInfo)
derive instance newtypeWorkflowTypeInfoList :: Newtype WorkflowTypeInfoList _


-- | <p>Contains a paginated list of information structures about workflow types.</p>
newtype WorkflowTypeInfos = WorkflowTypeInfos 
  { "TypeInfos'" :: (WorkflowTypeInfoList)
  , "NextPageToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeWorkflowTypeInfos :: Newtype WorkflowTypeInfos _
