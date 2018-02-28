

-- | <fullname>Amazon Simple Workflow Service</fullname> <p>The Amazon Simple Workflow Service (Amazon SWF) makes it easy to build applications that use Amazon's cloud to coordinate work across distributed components. In Amazon SWF, a <i>task</i> represents a logical unit of work that is performed by a component of your workflow. Coordinating tasks in a workflow involves managing intertask dependencies, scheduling, and concurrency in accordance with the logical flow of the application.</p> <p>Amazon SWF gives you full control over implementing tasks and coordinating them without worrying about underlying complexities such as tracking their progress and maintaining their state.</p> <p>This documentation serves as reference only. For a broader overview of the Amazon SWF programming model, see the <i> <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/">Amazon SWF Developer Guide</a> </i>.</p>
module AWS.SWF where

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

serviceName = "SWF" :: String


-- | <p>Returns the number of closed workflow executions within the given domain that meet the specified filtering criteria.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
countClosedWorkflowExecutions :: forall eff. CountClosedWorkflowExecutionsInput -> Aff (exception :: EXCEPTION | eff) WorkflowExecutionCount
countClosedWorkflowExecutions = Request.request serviceName "countClosedWorkflowExecutions" 


-- | <p>Returns the number of open workflow executions within the given domain that meet the specified filtering criteria.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
countOpenWorkflowExecutions :: forall eff. CountOpenWorkflowExecutionsInput -> Aff (exception :: EXCEPTION | eff) WorkflowExecutionCount
countOpenWorkflowExecutions = Request.request serviceName "countOpenWorkflowExecutions" 


-- | <p>Returns the estimated number of activity tasks in the specified task list. The count returned is an approximation and isn't guaranteed to be exact. If you specify a task list that no activity task was ever scheduled in then <code>0</code> is returned.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
countPendingActivityTasks :: forall eff. CountPendingActivityTasksInput -> Aff (exception :: EXCEPTION | eff) PendingTaskCount
countPendingActivityTasks = Request.request serviceName "countPendingActivityTasks" 


-- | <p>Returns the estimated number of decision tasks in the specified task list. The count returned is an approximation and isn't guaranteed to be exact. If you specify a task list that no decision task was ever scheduled in then <code>0</code> is returned.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
countPendingDecisionTasks :: forall eff. CountPendingDecisionTasksInput -> Aff (exception :: EXCEPTION | eff) PendingTaskCount
countPendingDecisionTasks = Request.request serviceName "countPendingDecisionTasks" 


-- | <p>Deprecates the specified <i>activity type</i>. After an activity type has been deprecated, you cannot create new tasks of that activity type. Tasks of this type that were scheduled before the type was deprecated continue to run.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>activityType.name</code>: String constraint. The key is <code>swf:activityType.name</code>.</p> </li> <li> <p> <code>activityType.version</code>: String constraint. The key is <code>swf:activityType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
deprecateActivityType :: forall eff. DeprecateActivityTypeInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deprecateActivityType = Request.request serviceName "deprecateActivityType" 


-- | <p>Deprecates the specified domain. After a domain has been deprecated it cannot be used to create new workflow executions or register new types. However, you can still use visibility actions on this domain. Deprecating a domain also deprecates all activity and workflow types registered in the domain. Executions that were started before the domain was deprecated continues to run.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
deprecateDomain :: forall eff. DeprecateDomainInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deprecateDomain = Request.request serviceName "deprecateDomain" 


-- | <p>Deprecates the specified <i>workflow type</i>. After a workflow type has been deprecated, you cannot create new executions of that type. Executions that were started before the type was deprecated continues to run. A deprecated workflow type may still be used when calling visibility actions.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>workflowType.name</code>: String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code>: String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
deprecateWorkflowType :: forall eff. DeprecateWorkflowTypeInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deprecateWorkflowType = Request.request serviceName "deprecateWorkflowType" 


-- | <p>Returns information about the specified activity type. This includes configuration settings provided when the type was registered and other general information about the type.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>activityType.name</code>: String constraint. The key is <code>swf:activityType.name</code>.</p> </li> <li> <p> <code>activityType.version</code>: String constraint. The key is <code>swf:activityType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
describeActivityType :: forall eff. DescribeActivityTypeInput -> Aff (exception :: EXCEPTION | eff) ActivityTypeDetail
describeActivityType = Request.request serviceName "describeActivityType" 


-- | <p>Returns information about the specified domain, including description and status.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
describeDomain :: forall eff. DescribeDomainInput -> Aff (exception :: EXCEPTION | eff) DomainDetail
describeDomain = Request.request serviceName "describeDomain" 


-- | <p>Returns information about the specified workflow execution including its type and some statistics.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
describeWorkflowExecution :: forall eff. DescribeWorkflowExecutionInput -> Aff (exception :: EXCEPTION | eff) WorkflowExecutionDetail
describeWorkflowExecution = Request.request serviceName "describeWorkflowExecution" 


-- | <p>Returns information about the specified <i>workflow type</i>. This includes configuration settings specified when the type was registered and other information such as creation date, current status, etc.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>workflowType.name</code>: String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code>: String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
describeWorkflowType :: forall eff. DescribeWorkflowTypeInput -> Aff (exception :: EXCEPTION | eff) WorkflowTypeDetail
describeWorkflowType = Request.request serviceName "describeWorkflowType" 


-- | <p>Returns the history of the specified workflow execution. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the <code>nextPageToken</code> returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
getWorkflowExecutionHistory :: forall eff. GetWorkflowExecutionHistoryInput -> Aff (exception :: EXCEPTION | eff) History
getWorkflowExecutionHistory = Request.request serviceName "getWorkflowExecutionHistory" 


-- | <p>Returns information about all activities registered in the specified domain that match the specified name and registration status. The result includes information like creation date, current status of the activity, etc. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the <code>nextPageToken</code> returned by the initial call.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listActivityTypes :: forall eff. ListActivityTypesInput -> Aff (exception :: EXCEPTION | eff) ActivityTypeInfos
listActivityTypes = Request.request serviceName "listActivityTypes" 


-- | <p>Returns a list of closed workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listClosedWorkflowExecutions :: forall eff. ListClosedWorkflowExecutionsInput -> Aff (exception :: EXCEPTION | eff) WorkflowExecutionInfos
listClosedWorkflowExecutions = Request.request serviceName "listClosedWorkflowExecutions" 


-- | <p>Returns the list of domains registered in the account. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains. The element must be set to <code>arn:aws:swf::AccountID:domain/*</code>, where <i>AccountID</i> is the account ID, with no dashes.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listDomains :: forall eff. ListDomainsInput -> Aff (exception :: EXCEPTION | eff) DomainInfos
listDomains = Request.request serviceName "listDomains" 


-- | <p>Returns a list of open workflow executions in the specified domain that meet the filtering criteria. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.</p> <note> <p>This operation is eventually consistent. The results are best effort and may not exactly reflect recent updates and changes.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagFilter.tag</code>: String constraint. The key is <code>swf:tagFilter.tag</code>.</p> </li> <li> <p> <code>typeFilter.name</code>: String constraint. The key is <code>swf:typeFilter.name</code>.</p> </li> <li> <p> <code>typeFilter.version</code>: String constraint. The key is <code>swf:typeFilter.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listOpenWorkflowExecutions :: forall eff. ListOpenWorkflowExecutionsInput -> Aff (exception :: EXCEPTION | eff) WorkflowExecutionInfos
listOpenWorkflowExecutions = Request.request serviceName "listOpenWorkflowExecutions" 


-- | <p>Returns information about workflow types in the specified domain. The results may be split into multiple pages that can be retrieved by making the call repeatedly.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
listWorkflowTypes :: forall eff. ListWorkflowTypesInput -> Aff (exception :: EXCEPTION | eff) WorkflowTypeInfos
listWorkflowTypes = Request.request serviceName "listWorkflowTypes" 


-- | <p>Used by workers to get an <a>ActivityTask</a> from the specified activity <code>taskList</code>. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available. The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns an empty result. An empty result, in this context, means that an ActivityTask is returned, but that the value of taskToken is an empty string. If a task is returned, the worker should use its type to identify and process it correctly.</p> <important> <p>Workers should set their client side socket timeout to at least 70 seconds (10 seconds higher than the maximum time service may hold the poll request).</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
pollForActivityTask :: forall eff. PollForActivityTaskInput -> Aff (exception :: EXCEPTION | eff) ActivityTask
pollForActivityTask = Request.request serviceName "pollForActivityTask" 


-- | <p>Used by deciders to get a <a>DecisionTask</a> from the specified decision <code>taskList</code>. A decision task may be returned for any open workflow execution that is using the specified task list. The task includes a paginated view of the history of the workflow execution. The decider should use the workflow type and the history to determine how to properly handle the task.</p> <p>This action initiates a long poll, where the service holds the HTTP connection open and responds as soon a task becomes available. If no decision task is available in the specified task list before the timeout of 60 seconds expires, an empty result is returned. An empty result, in this context, means that a DecisionTask is returned, but that the value of taskToken is an empty string.</p> <important> <p>Deciders should set their client side socket timeout to at least 70 seconds (10 seconds higher than the timeout).</p> </important> <important> <p>Because the number of workflow history events for a single workflow execution might be very large, the result returned might be split up across a number of pages. To retrieve subsequent pages, make additional calls to <code>PollForDecisionTask</code> using the <code>nextPageToken</code> returned by the initial call. Note that you do <i>not</i> call <code>GetWorkflowExecutionHistory</code> with this <code>nextPageToken</code>. Instead, call <code>PollForDecisionTask</code> again.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the <code>taskList.name</code> parameter by using a <code>Condition</code> element with the <code>swf:taskList.name</code> key to allow the action to access only certain task lists.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
pollForDecisionTask :: forall eff. PollForDecisionTaskInput -> Aff (exception :: EXCEPTION | eff) DecisionTask
pollForDecisionTask = Request.request serviceName "pollForDecisionTask" 


-- | <p>Used by activity workers to report to the service that the <a>ActivityTask</a> represented by the specified <code>taskToken</code> is still making progress. The worker can also specify details of the progress, for example percent complete, using the <code>details</code> parameter. This action can also be used by the worker as a mechanism to check if cancellation is being requested for the activity task. If a cancellation is being attempted for the specified task, then the boolean <code>cancelRequested</code> flag returned by the service is set to <code>true</code>.</p> <p>This action resets the <code>taskHeartbeatTimeout</code> clock. The <code>taskHeartbeatTimeout</code> is specified in <a>RegisterActivityType</a>.</p> <p>This action doesn't in itself create an event in the workflow execution history. However, if the task times out, the workflow execution history contains a <code>ActivityTaskTimedOut</code> event that contains the information from the last heartbeat generated by the activity worker.</p> <note> <p>The <code>taskStartToCloseTimeout</code> of an activity type is the maximum duration of an activity task, regardless of the number of <a>RecordActivityTaskHeartbeat</a> requests received. The <code>taskStartToCloseTimeout</code> is also specified in <a>RegisterActivityType</a>.</p> </note> <note> <p>This operation is only useful for long-lived activities to report liveliness of the task and to determine if a cancellation is being attempted.</p> </note> <important> <p>If the <code>cancelRequested</code> flag returns <code>true</code>, a cancellation is being attempted. If the worker can cancel the activity, it should respond with <a>RespondActivityTaskCanceled</a>. Otherwise, it should ignore the cancellation request.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
recordActivityTaskHeartbeat :: forall eff. RecordActivityTaskHeartbeatInput -> Aff (exception :: EXCEPTION | eff) ActivityTaskStatus
recordActivityTaskHeartbeat = Request.request serviceName "recordActivityTaskHeartbeat" 


-- | <p>Registers a new <i>activity type</i> along with its configuration settings in the specified domain.</p> <important> <p>A <code>TypeAlreadyExists</code> fault is returned if the type already exists in the domain. You cannot change any configuration settings of the type after its registration, and it must be registered as a new version.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>defaultTaskList.name</code>: String constraint. The key is <code>swf:defaultTaskList.name</code>.</p> </li> <li> <p> <code>name</code>: String constraint. The key is <code>swf:name</code>.</p> </li> <li> <p> <code>version</code>: String constraint. The key is <code>swf:version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
registerActivityType :: forall eff. RegisterActivityTypeInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
registerActivityType = Request.request serviceName "registerActivityType" 


-- | <p>Registers a new domain.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>You cannot use an IAM policy to control domain access for this action. The name of the domain being registered is available as the resource of this action.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
registerDomain :: forall eff. RegisterDomainInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
registerDomain = Request.request serviceName "registerDomain" 


-- | <p>Registers a new <i>workflow type</i> and its configuration settings in the specified domain.</p> <p>The retention period for the workflow history is set by the <a>RegisterDomain</a> action.</p> <important> <p>If the type already exists, then a <code>TypeAlreadyExists</code> fault is returned. You cannot change the configuration settings of a workflow type once it is registered and it must be registered as a new version.</p> </important> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>defaultTaskList.name</code>: String constraint. The key is <code>swf:defaultTaskList.name</code>.</p> </li> <li> <p> <code>name</code>: String constraint. The key is <code>swf:name</code>.</p> </li> <li> <p> <code>version</code>: String constraint. The key is <code>swf:version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
registerWorkflowType :: forall eff. RegisterWorkflowTypeInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
registerWorkflowType = Request.request serviceName "registerWorkflowType" 


-- | <p>Records a <code>WorkflowExecutionCancelRequested</code> event in the currently running workflow execution identified by the given domain, workflowId, and runId. This logically requests the cancellation of the workflow execution as a whole. It is up to the decider to take appropriate actions when it receives an execution history with this event.</p> <note> <p>If the runId isn't specified, the <code>WorkflowExecutionCancelRequested</code> event is recorded in the history of the current open workflow execution with the specified workflowId in the domain.</p> </note> <note> <p>Because this action allows the workflow to properly clean up and gracefully close, it should be used instead of <a>TerminateWorkflowExecution</a> when possible.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
requestCancelWorkflowExecution :: forall eff. RequestCancelWorkflowExecutionInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
requestCancelWorkflowExecution = Request.request serviceName "requestCancelWorkflowExecution" 


-- | <p>Used by workers to tell the service that the <a>ActivityTask</a> identified by the <code>taskToken</code> was successfully canceled. Additional <code>details</code> can be provided using the <code>details</code> argument.</p> <p>These <code>details</code> (if provided) appear in the <code>ActivityTaskCanceled</code> event added to the workflow history.</p> <important> <p>Only use this operation if the <code>canceled</code> flag of a <a>RecordActivityTaskHeartbeat</a> request returns <code>true</code> and if the activity can be safely undone or abandoned.</p> </important> <p>A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to <a>RespondActivityTaskCompleted</a>, RespondActivityTaskCanceled, <a>RespondActivityTaskFailed</a>, or the task has <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types">timed out</a>.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
respondActivityTaskCanceled :: forall eff. RespondActivityTaskCanceledInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
respondActivityTaskCanceled = Request.request serviceName "respondActivityTaskCanceled" 


-- | <p>Used by workers to tell the service that the <a>ActivityTask</a> identified by the <code>taskToken</code> completed successfully with a <code>result</code> (if provided). The <code>result</code> appears in the <code>ActivityTaskCompleted</code> event in the workflow history.</p> <important> <p>If the requested task doesn't complete successfully, use <a>RespondActivityTaskFailed</a> instead. If the worker finds that the task is canceled through the <code>canceled</code> flag returned by <a>RecordActivityTaskHeartbeat</a>, it should cancel the task, clean up and then call <a>RespondActivityTaskCanceled</a>.</p> </important> <p>A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to RespondActivityTaskCompleted, <a>RespondActivityTaskCanceled</a>, <a>RespondActivityTaskFailed</a>, or the task has <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types">timed out</a>.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
respondActivityTaskCompleted :: forall eff. RespondActivityTaskCompletedInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
respondActivityTaskCompleted = Request.request serviceName "respondActivityTaskCompleted" 


-- | <p>Used by workers to tell the service that the <a>ActivityTask</a> identified by the <code>taskToken</code> has failed with <code>reason</code> (if specified). The <code>reason</code> and <code>details</code> appear in the <code>ActivityTaskFailed</code> event added to the workflow history.</p> <p>A task is considered open from the time that it is scheduled until it is closed. Therefore a task is reported as open while a worker is processing it. A task is closed after it has been specified in a call to <a>RespondActivityTaskCompleted</a>, <a>RespondActivityTaskCanceled</a>, RespondActivityTaskFailed, or the task has <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dg-basic.html#swf-dev-timeout-types">timed out</a>.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
respondActivityTaskFailed :: forall eff. RespondActivityTaskFailedInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
respondActivityTaskFailed = Request.request serviceName "respondActivityTaskFailed" 


-- | <p>Used by deciders to tell the service that the <a>DecisionTask</a> identified by the <code>taskToken</code> has successfully completed. The <code>decisions</code> argument specifies the list of decisions made while processing the task.</p> <p>A <code>DecisionTaskCompleted</code> event is added to the workflow history. The <code>executionContext</code> specified is attached to the event in the workflow execution history.</p> <p> <b>Access Control</b> </p> <p>If an IAM policy grants permission to use <code>RespondDecisionTaskCompleted</code>, it can express permissions for the list of decisions in the <code>decisions</code> parameter. Each of the decisions has one or more parameters, much like a regular API call. To allow for policies to be as readable as possible, you can express permissions on decisions as if they were actual API calls, including applying conditions to some parameters. For more information, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
respondDecisionTaskCompleted :: forall eff. RespondDecisionTaskCompletedInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
respondDecisionTaskCompleted = Request.request serviceName "respondDecisionTaskCompleted" 


-- | <p>Records a <code>WorkflowExecutionSignaled</code> event in the workflow execution history and creates a decision task for the workflow execution identified by the given domain, workflowId and runId. The event is recorded with the specified user defined signalName and input (if provided).</p> <note> <p>If a runId isn't specified, then the <code>WorkflowExecutionSignaled</code> event is recorded in the history of the current open workflow with the matching workflowId in the domain.</p> </note> <note> <p>If the specified workflow execution isn't open, this method fails with <code>UnknownResource</code>.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
signalWorkflowExecution :: forall eff. SignalWorkflowExecutionInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
signalWorkflowExecution = Request.request serviceName "signalWorkflowExecution" 


-- | <p>Starts an execution of the workflow type in the specified domain using the provided <code>workflowId</code> and input data.</p> <p>This action returns the newly started workflow execution.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagList.member.0</code>: The key is <code>swf:tagList.member.0</code>.</p> </li> <li> <p> <code>tagList.member.1</code>: The key is <code>swf:tagList.member.1</code>.</p> </li> <li> <p> <code>tagList.member.2</code>: The key is <code>swf:tagList.member.2</code>.</p> </li> <li> <p> <code>tagList.member.3</code>: The key is <code>swf:tagList.member.3</code>.</p> </li> <li> <p> <code>tagList.member.4</code>: The key is <code>swf:tagList.member.4</code>.</p> </li> <li> <p> <code>taskList</code>: String constraint. The key is <code>swf:taskList.name</code>.</p> </li> <li> <p> <code>workflowType.name</code>: String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code>: String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
startWorkflowExecution :: forall eff. StartWorkflowExecutionInput -> Aff (exception :: EXCEPTION | eff) Run
startWorkflowExecution = Request.request serviceName "startWorkflowExecution" 


-- | <p>Records a <code>WorkflowExecutionTerminated</code> event and forces closure of the workflow execution identified by the given domain, runId, and workflowId. The child policy, registered with the workflow type or specified when starting this execution, is applied to any open child workflow executions of this workflow execution.</p> <important> <p>If the identified workflow execution was in progress, it is terminated immediately.</p> </important> <note> <p>If a runId isn't specified, then the <code>WorkflowExecutionTerminated</code> event is recorded in the history of the current open workflow with the matching workflowId in the domain.</p> </note> <note> <p>You should consider using <a>RequestCancelWorkflowExecution</a> action instead because it allows the workflow to gracefully close while <a>TerminateWorkflowExecution</a> doesn't.</p> </note> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this action's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
terminateWorkflowExecution :: forall eff. TerminateWorkflowExecutionInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
terminateWorkflowExecution = Request.request serviceName "terminateWorkflowExecution" 


newtype ActivityId = ActivityId String
derive instance newtypeActivityId :: Newtype ActivityId _
derive instance repGenericActivityId :: Generic ActivityId _
instance showActivityId :: Show ActivityId where
  show = genericShow
instance decodeActivityId :: Decode ActivityId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityId :: Encode ActivityId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Unit of work sent to an activity worker.</p>
newtype ActivityTask = ActivityTask 
  { "TaskToken'" :: (TaskToken)
  , "ActivityId'" :: (ActivityId)
  , "StartedEventId'" :: (EventId)
  , "WorkflowExecution'" :: (WorkflowExecution)
  , "ActivityType'" :: (ActivityType)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeActivityTask :: Newtype ActivityTask _
derive instance repGenericActivityTask :: Generic ActivityTask _
instance showActivityTask :: Show ActivityTask where
  show = genericShow
instance decodeActivityTask :: Decode ActivityTask where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTask :: Encode ActivityTask where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ActivityTaskCancelRequested</code> event.</p>
newtype ActivityTaskCancelRequestedEventAttributes = ActivityTaskCancelRequestedEventAttributes 
  { "DecisionTaskCompletedEventId'" :: (EventId)
  , "ActivityId'" :: (ActivityId)
  }
derive instance newtypeActivityTaskCancelRequestedEventAttributes :: Newtype ActivityTaskCancelRequestedEventAttributes _
derive instance repGenericActivityTaskCancelRequestedEventAttributes :: Generic ActivityTaskCancelRequestedEventAttributes _
instance showActivityTaskCancelRequestedEventAttributes :: Show ActivityTaskCancelRequestedEventAttributes where
  show = genericShow
instance decodeActivityTaskCancelRequestedEventAttributes :: Decode ActivityTaskCancelRequestedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTaskCancelRequestedEventAttributes :: Encode ActivityTaskCancelRequestedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ActivityTaskCanceled</code> event.</p>
newtype ActivityTaskCanceledEventAttributes = ActivityTaskCanceledEventAttributes 
  { "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "LatestCancelRequestedEventId'" :: NullOrUndefined.NullOrUndefined (EventId)
  }
derive instance newtypeActivityTaskCanceledEventAttributes :: Newtype ActivityTaskCanceledEventAttributes _
derive instance repGenericActivityTaskCanceledEventAttributes :: Generic ActivityTaskCanceledEventAttributes _
instance showActivityTaskCanceledEventAttributes :: Show ActivityTaskCanceledEventAttributes where
  show = genericShow
instance decodeActivityTaskCanceledEventAttributes :: Decode ActivityTaskCanceledEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTaskCanceledEventAttributes :: Encode ActivityTaskCanceledEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ActivityTaskCompleted</code> event.</p>
newtype ActivityTaskCompletedEventAttributes = ActivityTaskCompletedEventAttributes 
  { "Result'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeActivityTaskCompletedEventAttributes :: Newtype ActivityTaskCompletedEventAttributes _
derive instance repGenericActivityTaskCompletedEventAttributes :: Generic ActivityTaskCompletedEventAttributes _
instance showActivityTaskCompletedEventAttributes :: Show ActivityTaskCompletedEventAttributes where
  show = genericShow
instance decodeActivityTaskCompletedEventAttributes :: Decode ActivityTaskCompletedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTaskCompletedEventAttributes :: Encode ActivityTaskCompletedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ActivityTaskFailed</code> event.</p>
newtype ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes 
  { "Reason'" :: NullOrUndefined.NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeActivityTaskFailedEventAttributes :: Newtype ActivityTaskFailedEventAttributes _
derive instance repGenericActivityTaskFailedEventAttributes :: Generic ActivityTaskFailedEventAttributes _
instance showActivityTaskFailedEventAttributes :: Show ActivityTaskFailedEventAttributes where
  show = genericShow
instance decodeActivityTaskFailedEventAttributes :: Decode ActivityTaskFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTaskFailedEventAttributes :: Encode ActivityTaskFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ActivityTaskScheduled</code> event.</p>
newtype ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes 
  { "ActivityType'" :: (ActivityType)
  , "ActivityId'" :: (ActivityId)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ScheduleToStartTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "ScheduleToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "StartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "HeartbeatTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeActivityTaskScheduledEventAttributes :: Newtype ActivityTaskScheduledEventAttributes _
derive instance repGenericActivityTaskScheduledEventAttributes :: Generic ActivityTaskScheduledEventAttributes _
instance showActivityTaskScheduledEventAttributes :: Show ActivityTaskScheduledEventAttributes where
  show = genericShow
instance decodeActivityTaskScheduledEventAttributes :: Decode ActivityTaskScheduledEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTaskScheduledEventAttributes :: Encode ActivityTaskScheduledEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ActivityTaskStarted</code> event.</p>
newtype ActivityTaskStartedEventAttributes = ActivityTaskStartedEventAttributes 
  { "Identity'" :: NullOrUndefined.NullOrUndefined (Identity)
  , "ScheduledEventId'" :: (EventId)
  }
derive instance newtypeActivityTaskStartedEventAttributes :: Newtype ActivityTaskStartedEventAttributes _
derive instance repGenericActivityTaskStartedEventAttributes :: Generic ActivityTaskStartedEventAttributes _
instance showActivityTaskStartedEventAttributes :: Show ActivityTaskStartedEventAttributes where
  show = genericShow
instance decodeActivityTaskStartedEventAttributes :: Decode ActivityTaskStartedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTaskStartedEventAttributes :: Encode ActivityTaskStartedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Status information about an activity task.</p>
newtype ActivityTaskStatus = ActivityTaskStatus 
  { "CancelRequested'" :: (Canceled)
  }
derive instance newtypeActivityTaskStatus :: Newtype ActivityTaskStatus _
derive instance repGenericActivityTaskStatus :: Generic ActivityTaskStatus _
instance showActivityTaskStatus :: Show ActivityTaskStatus where
  show = genericShow
instance decodeActivityTaskStatus :: Decode ActivityTaskStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTaskStatus :: Encode ActivityTaskStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ActivityTaskTimedOut</code> event.</p>
newtype ActivityTaskTimedOutEventAttributes = ActivityTaskTimedOutEventAttributes 
  { "TimeoutType'" :: (ActivityTaskTimeoutType)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "Details'" :: NullOrUndefined.NullOrUndefined (LimitedData)
  }
derive instance newtypeActivityTaskTimedOutEventAttributes :: Newtype ActivityTaskTimedOutEventAttributes _
derive instance repGenericActivityTaskTimedOutEventAttributes :: Generic ActivityTaskTimedOutEventAttributes _
instance showActivityTaskTimedOutEventAttributes :: Show ActivityTaskTimedOutEventAttributes where
  show = genericShow
instance decodeActivityTaskTimedOutEventAttributes :: Decode ActivityTaskTimedOutEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTaskTimedOutEventAttributes :: Encode ActivityTaskTimedOutEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivityTaskTimeoutType = ActivityTaskTimeoutType String
derive instance newtypeActivityTaskTimeoutType :: Newtype ActivityTaskTimeoutType _
derive instance repGenericActivityTaskTimeoutType :: Generic ActivityTaskTimeoutType _
instance showActivityTaskTimeoutType :: Show ActivityTaskTimeoutType where
  show = genericShow
instance decodeActivityTaskTimeoutType :: Decode ActivityTaskTimeoutType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTaskTimeoutType :: Encode ActivityTaskTimeoutType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an activity type.</p>
newtype ActivityType = ActivityType 
  { "Name'" :: (Name)
  , "Version'" :: (Version)
  }
derive instance newtypeActivityType :: Newtype ActivityType _
derive instance repGenericActivityType :: Generic ActivityType _
instance showActivityType :: Show ActivityType where
  show = genericShow
instance decodeActivityType :: Decode ActivityType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityType :: Encode ActivityType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration settings registered with the activity type.</p>
newtype ActivityTypeConfiguration = ActivityTypeConfiguration 
  { "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskHeartbeatTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskList'" :: NullOrUndefined.NullOrUndefined (TaskList)
  , "DefaultTaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "DefaultTaskScheduleToStartTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskScheduleToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeActivityTypeConfiguration :: Newtype ActivityTypeConfiguration _
derive instance repGenericActivityTypeConfiguration :: Generic ActivityTypeConfiguration _
instance showActivityTypeConfiguration :: Show ActivityTypeConfiguration where
  show = genericShow
instance decodeActivityTypeConfiguration :: Decode ActivityTypeConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTypeConfiguration :: Encode ActivityTypeConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed information about an activity type.</p>
newtype ActivityTypeDetail = ActivityTypeDetail 
  { "TypeInfo'" :: (ActivityTypeInfo)
  , "Configuration'" :: (ActivityTypeConfiguration)
  }
derive instance newtypeActivityTypeDetail :: Newtype ActivityTypeDetail _
derive instance repGenericActivityTypeDetail :: Generic ActivityTypeDetail _
instance showActivityTypeDetail :: Show ActivityTypeDetail where
  show = genericShow
instance decodeActivityTypeDetail :: Decode ActivityTypeDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTypeDetail :: Encode ActivityTypeDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed information about an activity type.</p>
newtype ActivityTypeInfo = ActivityTypeInfo 
  { "ActivityType'" :: (ActivityType)
  , "Status'" :: (RegistrationStatus)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "CreationDate'" :: (Number)
  , "DeprecationDate'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeActivityTypeInfo :: Newtype ActivityTypeInfo _
derive instance repGenericActivityTypeInfo :: Generic ActivityTypeInfo _
instance showActivityTypeInfo :: Show ActivityTypeInfo where
  show = genericShow
instance decodeActivityTypeInfo :: Decode ActivityTypeInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTypeInfo :: Encode ActivityTypeInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivityTypeInfoList = ActivityTypeInfoList (Array ActivityTypeInfo)
derive instance newtypeActivityTypeInfoList :: Newtype ActivityTypeInfoList _
derive instance repGenericActivityTypeInfoList :: Generic ActivityTypeInfoList _
instance showActivityTypeInfoList :: Show ActivityTypeInfoList where
  show = genericShow
instance decodeActivityTypeInfoList :: Decode ActivityTypeInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTypeInfoList :: Encode ActivityTypeInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a paginated list of activity type information structures.</p>
newtype ActivityTypeInfos = ActivityTypeInfos 
  { "TypeInfos'" :: (ActivityTypeInfoList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeActivityTypeInfos :: Newtype ActivityTypeInfos _
derive instance repGenericActivityTypeInfos :: Generic ActivityTypeInfos _
instance showActivityTypeInfos :: Show ActivityTypeInfos where
  show = genericShow
instance decodeActivityTypeInfos :: Decode ActivityTypeInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTypeInfos :: Encode ActivityTypeInfos where
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


-- | <p>Provides the details of the <code>CancelTimer</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype CancelTimerDecisionAttributes = CancelTimerDecisionAttributes 
  { "TimerId'" :: (TimerId)
  }
derive instance newtypeCancelTimerDecisionAttributes :: Newtype CancelTimerDecisionAttributes _
derive instance repGenericCancelTimerDecisionAttributes :: Generic CancelTimerDecisionAttributes _
instance showCancelTimerDecisionAttributes :: Show CancelTimerDecisionAttributes where
  show = genericShow
instance decodeCancelTimerDecisionAttributes :: Decode CancelTimerDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelTimerDecisionAttributes :: Encode CancelTimerDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelTimerFailedCause = CancelTimerFailedCause String
derive instance newtypeCancelTimerFailedCause :: Newtype CancelTimerFailedCause _
derive instance repGenericCancelTimerFailedCause :: Generic CancelTimerFailedCause _
instance showCancelTimerFailedCause :: Show CancelTimerFailedCause where
  show = genericShow
instance decodeCancelTimerFailedCause :: Decode CancelTimerFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelTimerFailedCause :: Encode CancelTimerFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>CancelTimerFailed</code> event.</p>
newtype CancelTimerFailedEventAttributes = CancelTimerFailedEventAttributes 
  { "TimerId'" :: (TimerId)
  , "Cause'" :: (CancelTimerFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeCancelTimerFailedEventAttributes :: Newtype CancelTimerFailedEventAttributes _
derive instance repGenericCancelTimerFailedEventAttributes :: Generic CancelTimerFailedEventAttributes _
instance showCancelTimerFailedEventAttributes :: Show CancelTimerFailedEventAttributes where
  show = genericShow
instance decodeCancelTimerFailedEventAttributes :: Decode CancelTimerFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelTimerFailedEventAttributes :: Encode CancelTimerFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>CancelWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype CancelWorkflowExecutionDecisionAttributes = CancelWorkflowExecutionDecisionAttributes 
  { "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeCancelWorkflowExecutionDecisionAttributes :: Newtype CancelWorkflowExecutionDecisionAttributes _
derive instance repGenericCancelWorkflowExecutionDecisionAttributes :: Generic CancelWorkflowExecutionDecisionAttributes _
instance showCancelWorkflowExecutionDecisionAttributes :: Show CancelWorkflowExecutionDecisionAttributes where
  show = genericShow
instance decodeCancelWorkflowExecutionDecisionAttributes :: Decode CancelWorkflowExecutionDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelWorkflowExecutionDecisionAttributes :: Encode CancelWorkflowExecutionDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelWorkflowExecutionFailedCause = CancelWorkflowExecutionFailedCause String
derive instance newtypeCancelWorkflowExecutionFailedCause :: Newtype CancelWorkflowExecutionFailedCause _
derive instance repGenericCancelWorkflowExecutionFailedCause :: Generic CancelWorkflowExecutionFailedCause _
instance showCancelWorkflowExecutionFailedCause :: Show CancelWorkflowExecutionFailedCause where
  show = genericShow
instance decodeCancelWorkflowExecutionFailedCause :: Decode CancelWorkflowExecutionFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelWorkflowExecutionFailedCause :: Encode CancelWorkflowExecutionFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>CancelWorkflowExecutionFailed</code> event.</p>
newtype CancelWorkflowExecutionFailedEventAttributes = CancelWorkflowExecutionFailedEventAttributes 
  { "Cause'" :: (CancelWorkflowExecutionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeCancelWorkflowExecutionFailedEventAttributes :: Newtype CancelWorkflowExecutionFailedEventAttributes _
derive instance repGenericCancelWorkflowExecutionFailedEventAttributes :: Generic CancelWorkflowExecutionFailedEventAttributes _
instance showCancelWorkflowExecutionFailedEventAttributes :: Show CancelWorkflowExecutionFailedEventAttributes where
  show = genericShow
instance decodeCancelWorkflowExecutionFailedEventAttributes :: Decode CancelWorkflowExecutionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelWorkflowExecutionFailedEventAttributes :: Encode CancelWorkflowExecutionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Canceled = Canceled Boolean
derive instance newtypeCanceled :: Newtype Canceled _
derive instance repGenericCanceled :: Generic Canceled _
instance showCanceled :: Show Canceled where
  show = genericShow
instance decodeCanceled :: Decode Canceled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCanceled :: Encode Canceled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CauseMessage = CauseMessage String
derive instance newtypeCauseMessage :: Newtype CauseMessage _
derive instance repGenericCauseMessage :: Generic CauseMessage _
instance showCauseMessage :: Show CauseMessage where
  show = genericShow
instance decodeCauseMessage :: Decode CauseMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCauseMessage :: Encode CauseMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChildPolicy = ChildPolicy String
derive instance newtypeChildPolicy :: Newtype ChildPolicy _
derive instance repGenericChildPolicy :: Generic ChildPolicy _
instance showChildPolicy :: Show ChildPolicy where
  show = genericShow
instance decodeChildPolicy :: Decode ChildPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChildPolicy :: Encode ChildPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provide details of the <code>ChildWorkflowExecutionCanceled</code> event.</p>
newtype ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionCanceledEventAttributes :: Newtype ChildWorkflowExecutionCanceledEventAttributes _
derive instance repGenericChildWorkflowExecutionCanceledEventAttributes :: Generic ChildWorkflowExecutionCanceledEventAttributes _
instance showChildWorkflowExecutionCanceledEventAttributes :: Show ChildWorkflowExecutionCanceledEventAttributes where
  show = genericShow
instance decodeChildWorkflowExecutionCanceledEventAttributes :: Decode ChildWorkflowExecutionCanceledEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChildWorkflowExecutionCanceledEventAttributes :: Encode ChildWorkflowExecutionCanceledEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ChildWorkflowExecutionCompleted</code> event.</p>
newtype ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "Result'" :: NullOrUndefined.NullOrUndefined (Data)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionCompletedEventAttributes :: Newtype ChildWorkflowExecutionCompletedEventAttributes _
derive instance repGenericChildWorkflowExecutionCompletedEventAttributes :: Generic ChildWorkflowExecutionCompletedEventAttributes _
instance showChildWorkflowExecutionCompletedEventAttributes :: Show ChildWorkflowExecutionCompletedEventAttributes where
  show = genericShow
instance decodeChildWorkflowExecutionCompletedEventAttributes :: Decode ChildWorkflowExecutionCompletedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChildWorkflowExecutionCompletedEventAttributes :: Encode ChildWorkflowExecutionCompletedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ChildWorkflowExecutionFailed</code> event.</p>
newtype ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionFailedEventAttributes :: Newtype ChildWorkflowExecutionFailedEventAttributes _
derive instance repGenericChildWorkflowExecutionFailedEventAttributes :: Generic ChildWorkflowExecutionFailedEventAttributes _
instance showChildWorkflowExecutionFailedEventAttributes :: Show ChildWorkflowExecutionFailedEventAttributes where
  show = genericShow
instance decodeChildWorkflowExecutionFailedEventAttributes :: Decode ChildWorkflowExecutionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChildWorkflowExecutionFailedEventAttributes :: Encode ChildWorkflowExecutionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ChildWorkflowExecutionStarted</code> event.</p>
newtype ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "InitiatedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionStartedEventAttributes :: Newtype ChildWorkflowExecutionStartedEventAttributes _
derive instance repGenericChildWorkflowExecutionStartedEventAttributes :: Generic ChildWorkflowExecutionStartedEventAttributes _
instance showChildWorkflowExecutionStartedEventAttributes :: Show ChildWorkflowExecutionStartedEventAttributes where
  show = genericShow
instance decodeChildWorkflowExecutionStartedEventAttributes :: Decode ChildWorkflowExecutionStartedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChildWorkflowExecutionStartedEventAttributes :: Encode ChildWorkflowExecutionStartedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ChildWorkflowExecutionTerminated</code> event.</p>
newtype ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionTerminatedEventAttributes :: Newtype ChildWorkflowExecutionTerminatedEventAttributes _
derive instance repGenericChildWorkflowExecutionTerminatedEventAttributes :: Generic ChildWorkflowExecutionTerminatedEventAttributes _
instance showChildWorkflowExecutionTerminatedEventAttributes :: Show ChildWorkflowExecutionTerminatedEventAttributes where
  show = genericShow
instance decodeChildWorkflowExecutionTerminatedEventAttributes :: Decode ChildWorkflowExecutionTerminatedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChildWorkflowExecutionTerminatedEventAttributes :: Encode ChildWorkflowExecutionTerminatedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ChildWorkflowExecutionTimedOut</code> event.</p>
newtype ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "TimeoutType'" :: (WorkflowExecutionTimeoutType)
  , "InitiatedEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeChildWorkflowExecutionTimedOutEventAttributes :: Newtype ChildWorkflowExecutionTimedOutEventAttributes _
derive instance repGenericChildWorkflowExecutionTimedOutEventAttributes :: Generic ChildWorkflowExecutionTimedOutEventAttributes _
instance showChildWorkflowExecutionTimedOutEventAttributes :: Show ChildWorkflowExecutionTimedOutEventAttributes where
  show = genericShow
instance decodeChildWorkflowExecutionTimedOutEventAttributes :: Decode ChildWorkflowExecutionTimedOutEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChildWorkflowExecutionTimedOutEventAttributes :: Encode ChildWorkflowExecutionTimedOutEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloseStatus = CloseStatus String
derive instance newtypeCloseStatus :: Newtype CloseStatus _
derive instance repGenericCloseStatus :: Generic CloseStatus _
instance showCloseStatus :: Show CloseStatus where
  show = genericShow
instance decodeCloseStatus :: Decode CloseStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloseStatus :: Encode CloseStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Used to filter the closed workflow executions in visibility APIs by their close status.</p>
newtype CloseStatusFilter = CloseStatusFilter 
  { "Status'" :: (CloseStatus)
  }
derive instance newtypeCloseStatusFilter :: Newtype CloseStatusFilter _
derive instance repGenericCloseStatusFilter :: Generic CloseStatusFilter _
instance showCloseStatusFilter :: Show CloseStatusFilter where
  show = genericShow
instance decodeCloseStatusFilter :: Decode CloseStatusFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloseStatusFilter :: Encode CloseStatusFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>CompleteWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype CompleteWorkflowExecutionDecisionAttributes = CompleteWorkflowExecutionDecisionAttributes 
  { "Result'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeCompleteWorkflowExecutionDecisionAttributes :: Newtype CompleteWorkflowExecutionDecisionAttributes _
derive instance repGenericCompleteWorkflowExecutionDecisionAttributes :: Generic CompleteWorkflowExecutionDecisionAttributes _
instance showCompleteWorkflowExecutionDecisionAttributes :: Show CompleteWorkflowExecutionDecisionAttributes where
  show = genericShow
instance decodeCompleteWorkflowExecutionDecisionAttributes :: Decode CompleteWorkflowExecutionDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompleteWorkflowExecutionDecisionAttributes :: Encode CompleteWorkflowExecutionDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompleteWorkflowExecutionFailedCause = CompleteWorkflowExecutionFailedCause String
derive instance newtypeCompleteWorkflowExecutionFailedCause :: Newtype CompleteWorkflowExecutionFailedCause _
derive instance repGenericCompleteWorkflowExecutionFailedCause :: Generic CompleteWorkflowExecutionFailedCause _
instance showCompleteWorkflowExecutionFailedCause :: Show CompleteWorkflowExecutionFailedCause where
  show = genericShow
instance decodeCompleteWorkflowExecutionFailedCause :: Decode CompleteWorkflowExecutionFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompleteWorkflowExecutionFailedCause :: Encode CompleteWorkflowExecutionFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>CompleteWorkflowExecutionFailed</code> event.</p>
newtype CompleteWorkflowExecutionFailedEventAttributes = CompleteWorkflowExecutionFailedEventAttributes 
  { "Cause'" :: (CompleteWorkflowExecutionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeCompleteWorkflowExecutionFailedEventAttributes :: Newtype CompleteWorkflowExecutionFailedEventAttributes _
derive instance repGenericCompleteWorkflowExecutionFailedEventAttributes :: Generic CompleteWorkflowExecutionFailedEventAttributes _
instance showCompleteWorkflowExecutionFailedEventAttributes :: Show CompleteWorkflowExecutionFailedEventAttributes where
  show = genericShow
instance decodeCompleteWorkflowExecutionFailedEventAttributes :: Decode CompleteWorkflowExecutionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompleteWorkflowExecutionFailedEventAttributes :: Encode CompleteWorkflowExecutionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ContinueAsNewWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tag</code> – A tag used to identify the workflow execution</p> </li> <li> <p> <code>taskList</code> – String constraint. The key is <code>swf:taskList.name</code>.</p> </li> <li> <p> <code>workflowType.version</code> – String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype ContinueAsNewWorkflowExecutionDecisionAttributes = ContinueAsNewWorkflowExecutionDecisionAttributes 
  { "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: NullOrUndefined.NullOrUndefined (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: NullOrUndefined.NullOrUndefined (ChildPolicy)
  , "TagList'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "WorkflowTypeVersion'" :: NullOrUndefined.NullOrUndefined (Version)
  , "LambdaRole'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeContinueAsNewWorkflowExecutionDecisionAttributes :: Newtype ContinueAsNewWorkflowExecutionDecisionAttributes _
derive instance repGenericContinueAsNewWorkflowExecutionDecisionAttributes :: Generic ContinueAsNewWorkflowExecutionDecisionAttributes _
instance showContinueAsNewWorkflowExecutionDecisionAttributes :: Show ContinueAsNewWorkflowExecutionDecisionAttributes where
  show = genericShow
instance decodeContinueAsNewWorkflowExecutionDecisionAttributes :: Decode ContinueAsNewWorkflowExecutionDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContinueAsNewWorkflowExecutionDecisionAttributes :: Encode ContinueAsNewWorkflowExecutionDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContinueAsNewWorkflowExecutionFailedCause = ContinueAsNewWorkflowExecutionFailedCause String
derive instance newtypeContinueAsNewWorkflowExecutionFailedCause :: Newtype ContinueAsNewWorkflowExecutionFailedCause _
derive instance repGenericContinueAsNewWorkflowExecutionFailedCause :: Generic ContinueAsNewWorkflowExecutionFailedCause _
instance showContinueAsNewWorkflowExecutionFailedCause :: Show ContinueAsNewWorkflowExecutionFailedCause where
  show = genericShow
instance decodeContinueAsNewWorkflowExecutionFailedCause :: Decode ContinueAsNewWorkflowExecutionFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContinueAsNewWorkflowExecutionFailedCause :: Encode ContinueAsNewWorkflowExecutionFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ContinueAsNewWorkflowExecutionFailed</code> event.</p>
newtype ContinueAsNewWorkflowExecutionFailedEventAttributes = ContinueAsNewWorkflowExecutionFailedEventAttributes 
  { "Cause'" :: (ContinueAsNewWorkflowExecutionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeContinueAsNewWorkflowExecutionFailedEventAttributes :: Newtype ContinueAsNewWorkflowExecutionFailedEventAttributes _
derive instance repGenericContinueAsNewWorkflowExecutionFailedEventAttributes :: Generic ContinueAsNewWorkflowExecutionFailedEventAttributes _
instance showContinueAsNewWorkflowExecutionFailedEventAttributes :: Show ContinueAsNewWorkflowExecutionFailedEventAttributes where
  show = genericShow
instance decodeContinueAsNewWorkflowExecutionFailedEventAttributes :: Decode ContinueAsNewWorkflowExecutionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContinueAsNewWorkflowExecutionFailedEventAttributes :: Encode ContinueAsNewWorkflowExecutionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Count = Count Int
derive instance newtypeCount :: Newtype Count _
derive instance repGenericCount :: Generic Count _
instance showCount :: Show Count where
  show = genericShow
instance decodeCount :: Decode Count where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCount :: Encode Count where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CountClosedWorkflowExecutionsInput = CountClosedWorkflowExecutionsInput 
  { "Domain'" :: (DomainName)
  , "StartTimeFilter'" :: NullOrUndefined.NullOrUndefined (ExecutionTimeFilter)
  , "CloseTimeFilter'" :: NullOrUndefined.NullOrUndefined (ExecutionTimeFilter)
  , "ExecutionFilter'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionFilter)
  , "TypeFilter'" :: NullOrUndefined.NullOrUndefined (WorkflowTypeFilter)
  , "TagFilter'" :: NullOrUndefined.NullOrUndefined (TagFilter)
  , "CloseStatusFilter'" :: NullOrUndefined.NullOrUndefined (CloseStatusFilter)
  }
derive instance newtypeCountClosedWorkflowExecutionsInput :: Newtype CountClosedWorkflowExecutionsInput _
derive instance repGenericCountClosedWorkflowExecutionsInput :: Generic CountClosedWorkflowExecutionsInput _
instance showCountClosedWorkflowExecutionsInput :: Show CountClosedWorkflowExecutionsInput where
  show = genericShow
instance decodeCountClosedWorkflowExecutionsInput :: Decode CountClosedWorkflowExecutionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCountClosedWorkflowExecutionsInput :: Encode CountClosedWorkflowExecutionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CountOpenWorkflowExecutionsInput = CountOpenWorkflowExecutionsInput 
  { "Domain'" :: (DomainName)
  , "StartTimeFilter'" :: (ExecutionTimeFilter)
  , "TypeFilter'" :: NullOrUndefined.NullOrUndefined (WorkflowTypeFilter)
  , "TagFilter'" :: NullOrUndefined.NullOrUndefined (TagFilter)
  , "ExecutionFilter'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionFilter)
  }
derive instance newtypeCountOpenWorkflowExecutionsInput :: Newtype CountOpenWorkflowExecutionsInput _
derive instance repGenericCountOpenWorkflowExecutionsInput :: Generic CountOpenWorkflowExecutionsInput _
instance showCountOpenWorkflowExecutionsInput :: Show CountOpenWorkflowExecutionsInput where
  show = genericShow
instance decodeCountOpenWorkflowExecutionsInput :: Decode CountOpenWorkflowExecutionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCountOpenWorkflowExecutionsInput :: Encode CountOpenWorkflowExecutionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CountPendingActivityTasksInput = CountPendingActivityTasksInput 
  { "Domain'" :: (DomainName)
  , "TaskList'" :: (TaskList)
  }
derive instance newtypeCountPendingActivityTasksInput :: Newtype CountPendingActivityTasksInput _
derive instance repGenericCountPendingActivityTasksInput :: Generic CountPendingActivityTasksInput _
instance showCountPendingActivityTasksInput :: Show CountPendingActivityTasksInput where
  show = genericShow
instance decodeCountPendingActivityTasksInput :: Decode CountPendingActivityTasksInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCountPendingActivityTasksInput :: Encode CountPendingActivityTasksInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CountPendingDecisionTasksInput = CountPendingDecisionTasksInput 
  { "Domain'" :: (DomainName)
  , "TaskList'" :: (TaskList)
  }
derive instance newtypeCountPendingDecisionTasksInput :: Newtype CountPendingDecisionTasksInput _
derive instance repGenericCountPendingDecisionTasksInput :: Generic CountPendingDecisionTasksInput _
instance showCountPendingDecisionTasksInput :: Show CountPendingDecisionTasksInput where
  show = genericShow
instance decodeCountPendingDecisionTasksInput :: Decode CountPendingDecisionTasksInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCountPendingDecisionTasksInput :: Encode CountPendingDecisionTasksInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Data = Data String
derive instance newtypeData :: Newtype Data _
derive instance repGenericData :: Generic Data _
instance showData :: Show Data where
  show = genericShow
instance decodeData :: Decode Data where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeData :: Encode Data where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a decision made by the decider. A decision can be one of these types:</p> <ul> <li> <p> <code>CancelTimer</code> – Cancels a previously started timer and records a <code>TimerCanceled</code> event in the history.</p> </li> <li> <p> <code>CancelWorkflowExecution</code> – Closes the workflow execution and records a <code>WorkflowExecutionCanceled</code> event in the history.</p> </li> <li> <p> <code>CompleteWorkflowExecution</code> – Closes the workflow execution and records a <code>WorkflowExecutionCompleted</code> event in the history .</p> </li> <li> <p> <code>ContinueAsNewWorkflowExecution</code> – Closes the workflow execution and starts a new workflow execution of the same type using the same workflow ID and a unique run Id. A <code>WorkflowExecutionContinuedAsNew</code> event is recorded in the history.</p> </li> <li> <p> <code>FailWorkflowExecution</code> – Closes the workflow execution and records a <code>WorkflowExecutionFailed</code> event in the history.</p> </li> <li> <p> <code>RecordMarker</code> – Records a <code>MarkerRecorded</code> event in the history. Markers can be used for adding custom information in the history for instance to let deciders know that they don't need to look at the history beyond the marker event.</p> </li> <li> <p> <code>RequestCancelActivityTask</code> – Attempts to cancel a previously scheduled activity task. If the activity task was scheduled but has not been assigned to a worker, then it is canceled. If the activity task was already assigned to a worker, then the worker is informed that cancellation has been requested in the response to <a>RecordActivityTaskHeartbeat</a>.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecution</code> – Requests that a request be made to cancel the specified external workflow execution and records a <code>RequestCancelExternalWorkflowExecutionInitiated</code> event in the history.</p> </li> <li> <p> <code>ScheduleActivityTask</code> – Schedules an activity task.</p> </li> <li> <p> <code>SignalExternalWorkflowExecution</code> – Requests a signal to be delivered to the specified external workflow execution and records a <code>SignalExternalWorkflowExecutionInitiated</code> event in the history.</p> </li> <li> <p> <code>StartChildWorkflowExecution</code> – Requests that a child workflow execution be started and records a <code>StartChildWorkflowExecutionInitiated</code> event in the history. The child workflow execution is a separate workflow execution with its own history.</p> </li> <li> <p> <code>StartTimer</code> – Starts a timer for this workflow execution and records a <code>TimerStarted</code> event in the history. This timer fires after the specified delay and record a <code>TimerFired</code> event.</p> </li> </ul> <p> <b>Access Control</b> </p> <p>If you grant permission to use <code>RespondDecisionTaskCompleted</code>, you can use IAM policies to express permissions for the list of decisions returned by this action as if they were members of the API. Treating decisions as a pseudo API maintains a uniform conceptual model and helps keep policies readable. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p> <p> <b>Decision Failure</b> </p> <p>Decisions can fail for several reasons</p> <ul> <li> <p>The ordering of decisions should follow a logical flow. Some decisions might not make sense in the current context of the workflow execution and therefore fails.</p> </li> <li> <p>A limit on your account was reached.</p> </li> <li> <p>The decision lacks sufficient permissions.</p> </li> </ul> <p>One of the following events might be added to the history to indicate an error. The event attribute's <code>cause</code> parameter indicates the cause. If <code>cause</code> is set to <code>OPERATION_NOT_PERMITTED</code>, the decision failed because it lacked sufficient permissions. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p> <ul> <li> <p> <code>ScheduleActivityTaskFailed</code> – A <code>ScheduleActivityTask</code> decision failed. This could happen if the activity type specified in the decision isn't registered, is in a deprecated state, or the decision isn't properly configured.</p> </li> <li> <p> <code>RequestCancelActivityTaskFailed</code> – A <code>RequestCancelActivityTask</code> decision failed. This could happen if there is no open activity task with the specified activityId.</p> </li> <li> <p> <code>StartTimerFailed</code> – A <code>StartTimer</code> decision failed. This could happen if there is another open timer with the same timerId.</p> </li> <li> <p> <code>CancelTimerFailed</code> – A <code>CancelTimer</code> decision failed. This could happen if there is no open timer with the specified timerId.</p> </li> <li> <p> <code>StartChildWorkflowExecutionFailed</code> – A <code>StartChildWorkflowExecution</code> decision failed. This could happen if the workflow type specified isn't registered, is deprecated, or the decision isn't properly configured.</p> </li> <li> <p> <code>SignalExternalWorkflowExecutionFailed</code> – A <code>SignalExternalWorkflowExecution</code> decision failed. This could happen if the <code>workflowID</code> specified in the decision was incorrect.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecutionFailed</code> – A <code>RequestCancelExternalWorkflowExecution</code> decision failed. This could happen if the <code>workflowID</code> specified in the decision was incorrect.</p> </li> <li> <p> <code>CancelWorkflowExecutionFailed</code> – A <code>CancelWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.</p> </li> <li> <p> <code>CompleteWorkflowExecutionFailed</code> – A <code>CompleteWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.</p> </li> <li> <p> <code>ContinueAsNewWorkflowExecutionFailed</code> – A <code>ContinueAsNewWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution or the ContinueAsNewWorkflowExecution decision was not configured correctly.</p> </li> <li> <p> <code>FailWorkflowExecutionFailed</code> – A <code>FailWorkflowExecution</code> decision failed. This could happen if there is an unhandled decision task pending in the workflow execution.</p> </li> </ul> <p>The preceding error events might occur due to an error in the decider logic, which might put the workflow execution in an unstable state The cause field in the event structure for the error event indicates the cause of the error.</p> <note> <p>A workflow execution may be closed by the decider by returning one of the following decisions when completing a decision task: <code>CompleteWorkflowExecution</code>, <code>FailWorkflowExecution</code>, <code>CancelWorkflowExecution</code> and <code>ContinueAsNewWorkflowExecution</code>. An <code>UnhandledDecision</code> fault is returned if a workflow closing decision is specified and a signal or activity event had been added to the history while the decision task was being performed by the decider. Unlike the above situations which are logic issues, this fault is always possible because of race conditions in a distributed system. The right action here is to call <a>RespondDecisionTaskCompleted</a> without any decisions. This would result in another decision task with these new events included in the history. The decider should handle the new events and may decide to close the workflow execution.</p> </note> <p> <b>How to Code a Decision</b> </p> <p>You code a decision by first setting the decision type field to one of the above decision values, and then set the corresponding attributes field shown below:</p> <ul> <li> <p> <code> <a>ScheduleActivityTaskDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>RequestCancelActivityTaskDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>CompleteWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>FailWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>CancelWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>ContinueAsNewWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>RecordMarkerDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>StartTimerDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>CancelTimerDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>SignalExternalWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>RequestCancelExternalWorkflowExecutionDecisionAttributes</a> </code> </p> </li> <li> <p> <code> <a>StartChildWorkflowExecutionDecisionAttributes</a> </code> </p> </li> </ul>
newtype Decision = Decision 
  { "DecisionType'" :: (DecisionType)
  , "ScheduleActivityTaskDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (ScheduleActivityTaskDecisionAttributes)
  , "RequestCancelActivityTaskDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (RequestCancelActivityTaskDecisionAttributes)
  , "CompleteWorkflowExecutionDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (CompleteWorkflowExecutionDecisionAttributes)
  , "FailWorkflowExecutionDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (FailWorkflowExecutionDecisionAttributes)
  , "CancelWorkflowExecutionDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (CancelWorkflowExecutionDecisionAttributes)
  , "ContinueAsNewWorkflowExecutionDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (ContinueAsNewWorkflowExecutionDecisionAttributes)
  , "RecordMarkerDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (RecordMarkerDecisionAttributes)
  , "StartTimerDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (StartTimerDecisionAttributes)
  , "CancelTimerDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (CancelTimerDecisionAttributes)
  , "SignalExternalWorkflowExecutionDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (SignalExternalWorkflowExecutionDecisionAttributes)
  , "RequestCancelExternalWorkflowExecutionDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (RequestCancelExternalWorkflowExecutionDecisionAttributes)
  , "StartChildWorkflowExecutionDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (StartChildWorkflowExecutionDecisionAttributes)
  , "ScheduleLambdaFunctionDecisionAttributes'" :: NullOrUndefined.NullOrUndefined (ScheduleLambdaFunctionDecisionAttributes)
  }
derive instance newtypeDecision :: Newtype Decision _
derive instance repGenericDecision :: Generic Decision _
instance showDecision :: Show Decision where
  show = genericShow
instance decodeDecision :: Decode Decision where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecision :: Encode Decision where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DecisionList = DecisionList (Array Decision)
derive instance newtypeDecisionList :: Newtype DecisionList _
derive instance repGenericDecisionList :: Generic DecisionList _
instance showDecisionList :: Show DecisionList where
  show = genericShow
instance decodeDecisionList :: Decode DecisionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecisionList :: Encode DecisionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure that represents a decision task. Decision tasks are sent to deciders in order for them to make decisions.</p>
newtype DecisionTask = DecisionTask 
  { "TaskToken'" :: (TaskToken)
  , "StartedEventId'" :: (EventId)
  , "WorkflowExecution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "Events'" :: (HistoryEventList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  , "PreviousStartedEventId'" :: NullOrUndefined.NullOrUndefined (EventId)
  }
derive instance newtypeDecisionTask :: Newtype DecisionTask _
derive instance repGenericDecisionTask :: Generic DecisionTask _
instance showDecisionTask :: Show DecisionTask where
  show = genericShow
instance decodeDecisionTask :: Decode DecisionTask where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecisionTask :: Encode DecisionTask where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>DecisionTaskCompleted</code> event.</p>
newtype DecisionTaskCompletedEventAttributes = DecisionTaskCompletedEventAttributes 
  { "ExecutionContext'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeDecisionTaskCompletedEventAttributes :: Newtype DecisionTaskCompletedEventAttributes _
derive instance repGenericDecisionTaskCompletedEventAttributes :: Generic DecisionTaskCompletedEventAttributes _
instance showDecisionTaskCompletedEventAttributes :: Show DecisionTaskCompletedEventAttributes where
  show = genericShow
instance decodeDecisionTaskCompletedEventAttributes :: Decode DecisionTaskCompletedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecisionTaskCompletedEventAttributes :: Encode DecisionTaskCompletedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides details about the <code>DecisionTaskScheduled</code> event.</p>
newtype DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes 
  { "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "StartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeDecisionTaskScheduledEventAttributes :: Newtype DecisionTaskScheduledEventAttributes _
derive instance repGenericDecisionTaskScheduledEventAttributes :: Generic DecisionTaskScheduledEventAttributes _
instance showDecisionTaskScheduledEventAttributes :: Show DecisionTaskScheduledEventAttributes where
  show = genericShow
instance decodeDecisionTaskScheduledEventAttributes :: Decode DecisionTaskScheduledEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecisionTaskScheduledEventAttributes :: Encode DecisionTaskScheduledEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>DecisionTaskStarted</code> event.</p>
newtype DecisionTaskStartedEventAttributes = DecisionTaskStartedEventAttributes 
  { "Identity'" :: NullOrUndefined.NullOrUndefined (Identity)
  , "ScheduledEventId'" :: (EventId)
  }
derive instance newtypeDecisionTaskStartedEventAttributes :: Newtype DecisionTaskStartedEventAttributes _
derive instance repGenericDecisionTaskStartedEventAttributes :: Generic DecisionTaskStartedEventAttributes _
instance showDecisionTaskStartedEventAttributes :: Show DecisionTaskStartedEventAttributes where
  show = genericShow
instance decodeDecisionTaskStartedEventAttributes :: Decode DecisionTaskStartedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecisionTaskStartedEventAttributes :: Encode DecisionTaskStartedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>DecisionTaskTimedOut</code> event.</p>
newtype DecisionTaskTimedOutEventAttributes = DecisionTaskTimedOutEventAttributes 
  { "TimeoutType'" :: (DecisionTaskTimeoutType)
  , "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeDecisionTaskTimedOutEventAttributes :: Newtype DecisionTaskTimedOutEventAttributes _
derive instance repGenericDecisionTaskTimedOutEventAttributes :: Generic DecisionTaskTimedOutEventAttributes _
instance showDecisionTaskTimedOutEventAttributes :: Show DecisionTaskTimedOutEventAttributes where
  show = genericShow
instance decodeDecisionTaskTimedOutEventAttributes :: Decode DecisionTaskTimedOutEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecisionTaskTimedOutEventAttributes :: Encode DecisionTaskTimedOutEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DecisionTaskTimeoutType = DecisionTaskTimeoutType String
derive instance newtypeDecisionTaskTimeoutType :: Newtype DecisionTaskTimeoutType _
derive instance repGenericDecisionTaskTimeoutType :: Generic DecisionTaskTimeoutType _
instance showDecisionTaskTimeoutType :: Show DecisionTaskTimeoutType where
  show = genericShow
instance decodeDecisionTaskTimeoutType :: Decode DecisionTaskTimeoutType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecisionTaskTimeoutType :: Encode DecisionTaskTimeoutType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DecisionType = DecisionType String
derive instance newtypeDecisionType :: Newtype DecisionType _
derive instance repGenericDecisionType :: Generic DecisionType _
instance showDecisionType :: Show DecisionType where
  show = genericShow
instance decodeDecisionType :: Decode DecisionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDecisionType :: Encode DecisionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>StartWorkflowExecution</code> API action was called without the required parameters set.</p> <p>Some workflow execution parameters, such as the decision <code>taskList</code>, must be set to start the execution. However, these parameters might have been set as defaults when the workflow type was registered. In this case, you can omit these parameters from the <code>StartWorkflowExecution</code> call and Amazon SWF uses the values defined in the workflow type.</p> <note> <p>If these parameters aren't set and no default parameters were defined in the workflow type, this error is displayed.</p> </note>
newtype DefaultUndefinedFault = DefaultUndefinedFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDefaultUndefinedFault :: Newtype DefaultUndefinedFault _
derive instance repGenericDefaultUndefinedFault :: Generic DefaultUndefinedFault _
instance showDefaultUndefinedFault :: Show DefaultUndefinedFault where
  show = genericShow
instance decodeDefaultUndefinedFault :: Decode DefaultUndefinedFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultUndefinedFault :: Encode DefaultUndefinedFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeprecateActivityTypeInput = DeprecateActivityTypeInput 
  { "Domain'" :: (DomainName)
  , "ActivityType'" :: (ActivityType)
  }
derive instance newtypeDeprecateActivityTypeInput :: Newtype DeprecateActivityTypeInput _
derive instance repGenericDeprecateActivityTypeInput :: Generic DeprecateActivityTypeInput _
instance showDeprecateActivityTypeInput :: Show DeprecateActivityTypeInput where
  show = genericShow
instance decodeDeprecateActivityTypeInput :: Decode DeprecateActivityTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeprecateActivityTypeInput :: Encode DeprecateActivityTypeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeprecateDomainInput = DeprecateDomainInput 
  { "Name'" :: (DomainName)
  }
derive instance newtypeDeprecateDomainInput :: Newtype DeprecateDomainInput _
derive instance repGenericDeprecateDomainInput :: Generic DeprecateDomainInput _
instance showDeprecateDomainInput :: Show DeprecateDomainInput where
  show = genericShow
instance decodeDeprecateDomainInput :: Decode DeprecateDomainInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeprecateDomainInput :: Encode DeprecateDomainInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeprecateWorkflowTypeInput = DeprecateWorkflowTypeInput 
  { "Domain'" :: (DomainName)
  , "WorkflowType'" :: (WorkflowType)
  }
derive instance newtypeDeprecateWorkflowTypeInput :: Newtype DeprecateWorkflowTypeInput _
derive instance repGenericDeprecateWorkflowTypeInput :: Generic DeprecateWorkflowTypeInput _
instance showDeprecateWorkflowTypeInput :: Show DeprecateWorkflowTypeInput where
  show = genericShow
instance decodeDeprecateWorkflowTypeInput :: Decode DeprecateWorkflowTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeprecateWorkflowTypeInput :: Encode DeprecateWorkflowTypeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeActivityTypeInput = DescribeActivityTypeInput 
  { "Domain'" :: (DomainName)
  , "ActivityType'" :: (ActivityType)
  }
derive instance newtypeDescribeActivityTypeInput :: Newtype DescribeActivityTypeInput _
derive instance repGenericDescribeActivityTypeInput :: Generic DescribeActivityTypeInput _
instance showDescribeActivityTypeInput :: Show DescribeActivityTypeInput where
  show = genericShow
instance decodeDescribeActivityTypeInput :: Decode DescribeActivityTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivityTypeInput :: Encode DescribeActivityTypeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDomainInput = DescribeDomainInput 
  { "Name'" :: (DomainName)
  }
derive instance newtypeDescribeDomainInput :: Newtype DescribeDomainInput _
derive instance repGenericDescribeDomainInput :: Generic DescribeDomainInput _
instance showDescribeDomainInput :: Show DescribeDomainInput where
  show = genericShow
instance decodeDescribeDomainInput :: Decode DescribeDomainInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDomainInput :: Encode DescribeDomainInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkflowExecutionInput = DescribeWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "Execution'" :: (WorkflowExecution)
  }
derive instance newtypeDescribeWorkflowExecutionInput :: Newtype DescribeWorkflowExecutionInput _
derive instance repGenericDescribeWorkflowExecutionInput :: Generic DescribeWorkflowExecutionInput _
instance showDescribeWorkflowExecutionInput :: Show DescribeWorkflowExecutionInput where
  show = genericShow
instance decodeDescribeWorkflowExecutionInput :: Decode DescribeWorkflowExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkflowExecutionInput :: Encode DescribeWorkflowExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkflowTypeInput = DescribeWorkflowTypeInput 
  { "Domain'" :: (DomainName)
  , "WorkflowType'" :: (WorkflowType)
  }
derive instance newtypeDescribeWorkflowTypeInput :: Newtype DescribeWorkflowTypeInput _
derive instance repGenericDescribeWorkflowTypeInput :: Generic DescribeWorkflowTypeInput _
instance showDescribeWorkflowTypeInput :: Show DescribeWorkflowTypeInput where
  show = genericShow
instance decodeDescribeWorkflowTypeInput :: Decode DescribeWorkflowTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkflowTypeInput :: Encode DescribeWorkflowTypeInput where
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


-- | <p>Returned if the specified domain already exists. You get this fault even if the existing domain is in deprecated status.</p>
newtype DomainAlreadyExistsFault = DomainAlreadyExistsFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDomainAlreadyExistsFault :: Newtype DomainAlreadyExistsFault _
derive instance repGenericDomainAlreadyExistsFault :: Generic DomainAlreadyExistsFault _
instance showDomainAlreadyExistsFault :: Show DomainAlreadyExistsFault where
  show = genericShow
instance decodeDomainAlreadyExistsFault :: Decode DomainAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainAlreadyExistsFault :: Encode DomainAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the configuration settings of a domain.</p>
newtype DomainConfiguration = DomainConfiguration 
  { "WorkflowExecutionRetentionPeriodInDays'" :: (DurationInDays)
  }
derive instance newtypeDomainConfiguration :: Newtype DomainConfiguration _
derive instance repGenericDomainConfiguration :: Generic DomainConfiguration _
instance showDomainConfiguration :: Show DomainConfiguration where
  show = genericShow
instance decodeDomainConfiguration :: Decode DomainConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainConfiguration :: Encode DomainConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned when the specified domain has been deprecated.</p>
newtype DomainDeprecatedFault = DomainDeprecatedFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDomainDeprecatedFault :: Newtype DomainDeprecatedFault _
derive instance repGenericDomainDeprecatedFault :: Generic DomainDeprecatedFault _
instance showDomainDeprecatedFault :: Show DomainDeprecatedFault where
  show = genericShow
instance decodeDomainDeprecatedFault :: Decode DomainDeprecatedFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainDeprecatedFault :: Encode DomainDeprecatedFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details of a domain.</p>
newtype DomainDetail = DomainDetail 
  { "DomainInfo'" :: (DomainInfo)
  , "Configuration'" :: (DomainConfiguration)
  }
derive instance newtypeDomainDetail :: Newtype DomainDetail _
derive instance repGenericDomainDetail :: Generic DomainDetail _
instance showDomainDetail :: Show DomainDetail where
  show = genericShow
instance decodeDomainDetail :: Decode DomainDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainDetail :: Encode DomainDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains general information about a domain.</p>
newtype DomainInfo = DomainInfo 
  { "Name'" :: (DomainName)
  , "Status'" :: (RegistrationStatus)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeDomainInfo :: Newtype DomainInfo _
derive instance repGenericDomainInfo :: Generic DomainInfo _
instance showDomainInfo :: Show DomainInfo where
  show = genericShow
instance decodeDomainInfo :: Decode DomainInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainInfo :: Encode DomainInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainInfoList = DomainInfoList (Array DomainInfo)
derive instance newtypeDomainInfoList :: Newtype DomainInfoList _
derive instance repGenericDomainInfoList :: Generic DomainInfoList _
instance showDomainInfoList :: Show DomainInfoList where
  show = genericShow
instance decodeDomainInfoList :: Decode DomainInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainInfoList :: Encode DomainInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a paginated collection of DomainInfo structures.</p>
newtype DomainInfos = DomainInfos 
  { "DomainInfos'" :: (DomainInfoList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeDomainInfos :: Newtype DomainInfos _
derive instance repGenericDomainInfos :: Generic DomainInfos _
instance showDomainInfos :: Show DomainInfos where
  show = genericShow
instance decodeDomainInfos :: Decode DomainInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainInfos :: Encode DomainInfos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainName = DomainName String
derive instance newtypeDomainName :: Newtype DomainName _
derive instance repGenericDomainName :: Generic DomainName _
instance showDomainName :: Show DomainName where
  show = genericShow
instance decodeDomainName :: Decode DomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainName :: Encode DomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DurationInDays = DurationInDays String
derive instance newtypeDurationInDays :: Newtype DurationInDays _
derive instance repGenericDurationInDays :: Generic DurationInDays _
instance showDurationInDays :: Show DurationInDays where
  show = genericShow
instance decodeDurationInDays :: Decode DurationInDays where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDurationInDays :: Encode DurationInDays where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DurationInSeconds = DurationInSeconds String
derive instance newtypeDurationInSeconds :: Newtype DurationInSeconds _
derive instance repGenericDurationInSeconds :: Generic DurationInSeconds _
instance showDurationInSeconds :: Show DurationInSeconds where
  show = genericShow
instance decodeDurationInSeconds :: Decode DurationInSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDurationInSeconds :: Encode DurationInSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DurationInSecondsOptional = DurationInSecondsOptional String
derive instance newtypeDurationInSecondsOptional :: Newtype DurationInSecondsOptional _
derive instance repGenericDurationInSecondsOptional :: Generic DurationInSecondsOptional _
instance showDurationInSecondsOptional :: Show DurationInSecondsOptional where
  show = genericShow
instance decodeDurationInSecondsOptional :: Decode DurationInSecondsOptional where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDurationInSecondsOptional :: Encode DurationInSecondsOptional where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventId = EventId Number
derive instance newtypeEventId :: Newtype EventId _
derive instance repGenericEventId :: Generic EventId _
instance showEventId :: Show EventId where
  show = genericShow
instance decodeEventId :: Decode EventId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventId :: Encode EventId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventType = EventType String
derive instance newtypeEventType :: Newtype EventType _
derive instance repGenericEventType :: Generic EventType _
instance showEventType :: Show EventType where
  show = genericShow
instance decodeEventType :: Decode EventType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventType :: Encode EventType where
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


-- | <p>Used to filter the workflow executions in visibility APIs by various time-based rules. Each parameter, if specified, defines a rule that must be satisfied by each returned query result. The parameter values are in the <a href="https://en.wikipedia.org/wiki/Unix_time">Unix Time format</a>. For example: <code>"oldestDate": 1325376070.</code> </p>
newtype ExecutionTimeFilter = ExecutionTimeFilter 
  { "OldestDate'" :: (Number)
  , "LatestDate'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeExecutionTimeFilter :: Newtype ExecutionTimeFilter _
derive instance repGenericExecutionTimeFilter :: Generic ExecutionTimeFilter _
instance showExecutionTimeFilter :: Show ExecutionTimeFilter where
  show = genericShow
instance decodeExecutionTimeFilter :: Decode ExecutionTimeFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionTimeFilter :: Encode ExecutionTimeFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ExternalWorkflowExecutionCancelRequested</code> event.</p>
newtype ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "InitiatedEventId'" :: (EventId)
  }
derive instance newtypeExternalWorkflowExecutionCancelRequestedEventAttributes :: Newtype ExternalWorkflowExecutionCancelRequestedEventAttributes _
derive instance repGenericExternalWorkflowExecutionCancelRequestedEventAttributes :: Generic ExternalWorkflowExecutionCancelRequestedEventAttributes _
instance showExternalWorkflowExecutionCancelRequestedEventAttributes :: Show ExternalWorkflowExecutionCancelRequestedEventAttributes where
  show = genericShow
instance decodeExternalWorkflowExecutionCancelRequestedEventAttributes :: Decode ExternalWorkflowExecutionCancelRequestedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExternalWorkflowExecutionCancelRequestedEventAttributes :: Encode ExternalWorkflowExecutionCancelRequestedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ExternalWorkflowExecutionSignaled</code> event.</p>
newtype ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes 
  { "WorkflowExecution'" :: (WorkflowExecution)
  , "InitiatedEventId'" :: (EventId)
  }
derive instance newtypeExternalWorkflowExecutionSignaledEventAttributes :: Newtype ExternalWorkflowExecutionSignaledEventAttributes _
derive instance repGenericExternalWorkflowExecutionSignaledEventAttributes :: Generic ExternalWorkflowExecutionSignaledEventAttributes _
instance showExternalWorkflowExecutionSignaledEventAttributes :: Show ExternalWorkflowExecutionSignaledEventAttributes where
  show = genericShow
instance decodeExternalWorkflowExecutionSignaledEventAttributes :: Decode ExternalWorkflowExecutionSignaledEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExternalWorkflowExecutionSignaledEventAttributes :: Encode ExternalWorkflowExecutionSignaledEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>FailWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype FailWorkflowExecutionDecisionAttributes = FailWorkflowExecutionDecisionAttributes 
  { "Reason'" :: NullOrUndefined.NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeFailWorkflowExecutionDecisionAttributes :: Newtype FailWorkflowExecutionDecisionAttributes _
derive instance repGenericFailWorkflowExecutionDecisionAttributes :: Generic FailWorkflowExecutionDecisionAttributes _
instance showFailWorkflowExecutionDecisionAttributes :: Show FailWorkflowExecutionDecisionAttributes where
  show = genericShow
instance decodeFailWorkflowExecutionDecisionAttributes :: Decode FailWorkflowExecutionDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailWorkflowExecutionDecisionAttributes :: Encode FailWorkflowExecutionDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailWorkflowExecutionFailedCause = FailWorkflowExecutionFailedCause String
derive instance newtypeFailWorkflowExecutionFailedCause :: Newtype FailWorkflowExecutionFailedCause _
derive instance repGenericFailWorkflowExecutionFailedCause :: Generic FailWorkflowExecutionFailedCause _
instance showFailWorkflowExecutionFailedCause :: Show FailWorkflowExecutionFailedCause where
  show = genericShow
instance decodeFailWorkflowExecutionFailedCause :: Decode FailWorkflowExecutionFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailWorkflowExecutionFailedCause :: Encode FailWorkflowExecutionFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>FailWorkflowExecutionFailed</code> event.</p>
newtype FailWorkflowExecutionFailedEventAttributes = FailWorkflowExecutionFailedEventAttributes 
  { "Cause'" :: (FailWorkflowExecutionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeFailWorkflowExecutionFailedEventAttributes :: Newtype FailWorkflowExecutionFailedEventAttributes _
derive instance repGenericFailWorkflowExecutionFailedEventAttributes :: Generic FailWorkflowExecutionFailedEventAttributes _
instance showFailWorkflowExecutionFailedEventAttributes :: Show FailWorkflowExecutionFailedEventAttributes where
  show = genericShow
instance decodeFailWorkflowExecutionFailedEventAttributes :: Decode FailWorkflowExecutionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailWorkflowExecutionFailedEventAttributes :: Encode FailWorkflowExecutionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailureReason = FailureReason String
derive instance newtypeFailureReason :: Newtype FailureReason _
derive instance repGenericFailureReason :: Generic FailureReason _
instance showFailureReason :: Show FailureReason where
  show = genericShow
instance decodeFailureReason :: Decode FailureReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureReason :: Encode FailureReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FunctionId = FunctionId String
derive instance newtypeFunctionId :: Newtype FunctionId _
derive instance repGenericFunctionId :: Generic FunctionId _
instance showFunctionId :: Show FunctionId where
  show = genericShow
instance decodeFunctionId :: Decode FunctionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFunctionId :: Encode FunctionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FunctionInput = FunctionInput String
derive instance newtypeFunctionInput :: Newtype FunctionInput _
derive instance repGenericFunctionInput :: Generic FunctionInput _
instance showFunctionInput :: Show FunctionInput where
  show = genericShow
instance decodeFunctionInput :: Decode FunctionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFunctionInput :: Encode FunctionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FunctionName = FunctionName String
derive instance newtypeFunctionName :: Newtype FunctionName _
derive instance repGenericFunctionName :: Generic FunctionName _
instance showFunctionName :: Show FunctionName where
  show = genericShow
instance decodeFunctionName :: Decode FunctionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFunctionName :: Encode FunctionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetWorkflowExecutionHistoryInput = GetWorkflowExecutionHistoryInput 
  { "Domain'" :: (DomainName)
  , "Execution'" :: (WorkflowExecution)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined.NullOrUndefined (ReverseOrder)
  }
derive instance newtypeGetWorkflowExecutionHistoryInput :: Newtype GetWorkflowExecutionHistoryInput _
derive instance repGenericGetWorkflowExecutionHistoryInput :: Generic GetWorkflowExecutionHistoryInput _
instance showGetWorkflowExecutionHistoryInput :: Show GetWorkflowExecutionHistoryInput where
  show = genericShow
instance decodeGetWorkflowExecutionHistoryInput :: Decode GetWorkflowExecutionHistoryInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetWorkflowExecutionHistoryInput :: Encode GetWorkflowExecutionHistoryInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Paginated representation of a workflow history for a workflow execution. This is the up to date, complete and authoritative record of the events related to all tasks and events in the life of the workflow execution.</p>
newtype History = History 
  { "Events'" :: (HistoryEventList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeHistory :: Newtype History _
derive instance repGenericHistory :: Generic History _
instance showHistory :: Show History where
  show = genericShow
instance decodeHistory :: Decode History where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistory :: Encode History where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Event within a workflow execution. A history event can be one of these types:</p> <ul> <li> <p> <code>ActivityTaskCancelRequested</code> – A <code>RequestCancelActivityTask</code> decision was received by the system.</p> </li> <li> <p> <code>ActivityTaskCanceled</code> – The activity task was successfully canceled.</p> </li> <li> <p> <code>ActivityTaskCompleted</code> – An activity worker successfully completed an activity task by calling <a>RespondActivityTaskCompleted</a>.</p> </li> <li> <p> <code>ActivityTaskFailed</code> – An activity worker failed an activity task by calling <a>RespondActivityTaskFailed</a>.</p> </li> <li> <p> <code>ActivityTaskScheduled</code> – An activity task was scheduled for execution.</p> </li> <li> <p> <code>ActivityTaskStarted</code> – The scheduled activity task was dispatched to a worker.</p> </li> <li> <p> <code>ActivityTaskTimedOut</code> – The activity task timed out.</p> </li> <li> <p> <code>CancelTimerFailed</code> – Failed to process CancelTimer decision. This happens when the decision isn't configured properly, for example no timer exists with the specified timer Id.</p> </li> <li> <p> <code>CancelWorkflowExecutionFailed</code> – A request to cancel a workflow execution failed.</p> </li> <li> <p> <code>ChildWorkflowExecutionCanceled</code> – A child workflow execution, started by this workflow execution, was canceled and closed.</p> </li> <li> <p> <code>ChildWorkflowExecutionCompleted</code> – A child workflow execution, started by this workflow execution, completed successfully and was closed.</p> </li> <li> <p> <code>ChildWorkflowExecutionFailed</code> – A child workflow execution, started by this workflow execution, failed to complete successfully and was closed.</p> </li> <li> <p> <code>ChildWorkflowExecutionStarted</code> – A child workflow execution was successfully started.</p> </li> <li> <p> <code>ChildWorkflowExecutionTerminated</code> – A child workflow execution, started by this workflow execution, was terminated.</p> </li> <li> <p> <code>ChildWorkflowExecutionTimedOut</code> – A child workflow execution, started by this workflow execution, timed out and was closed.</p> </li> <li> <p> <code>CompleteWorkflowExecutionFailed</code> – The workflow execution failed to complete.</p> </li> <li> <p> <code>ContinueAsNewWorkflowExecutionFailed</code> – The workflow execution failed to complete after being continued as a new workflow execution.</p> </li> <li> <p> <code>DecisionTaskCompleted</code> – The decider successfully completed a decision task by calling <a>RespondDecisionTaskCompleted</a>.</p> </li> <li> <p> <code>DecisionTaskScheduled</code> – A decision task was scheduled for the workflow execution.</p> </li> <li> <p> <code>DecisionTaskStarted</code> – The decision task was dispatched to a decider.</p> </li> <li> <p> <code>DecisionTaskTimedOut</code> – The decision task timed out.</p> </li> <li> <p> <code>ExternalWorkflowExecutionCancelRequested</code> – Request to cancel an external workflow execution was successfully delivered to the target execution.</p> </li> <li> <p> <code>ExternalWorkflowExecutionSignaled</code> – A signal, requested by this workflow execution, was successfully delivered to the target external workflow execution.</p> </li> <li> <p> <code>FailWorkflowExecutionFailed</code> – A request to mark a workflow execution as failed, itself failed.</p> </li> <li> <p> <code>MarkerRecorded</code> – A marker was recorded in the workflow history as the result of a <code>RecordMarker</code> decision.</p> </li> <li> <p> <code>RecordMarkerFailed</code> – A <code>RecordMarker</code> decision was returned as failed.</p> </li> <li> <p> <code>RequestCancelActivityTaskFailed</code> – Failed to process RequestCancelActivityTask decision. This happens when the decision isn't configured properly.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecutionFailed</code> – Request to cancel an external workflow execution failed.</p> </li> <li> <p> <code>RequestCancelExternalWorkflowExecutionInitiated</code> – A request was made to request the cancellation of an external workflow execution.</p> </li> <li> <p> <code>ScheduleActivityTaskFailed</code> – Failed to process ScheduleActivityTask decision. This happens when the decision isn't configured properly, for example the activity type specified isn't registered.</p> </li> <li> <p> <code>SignalExternalWorkflowExecutionFailed</code> – The request to signal an external workflow execution failed.</p> </li> <li> <p> <code>SignalExternalWorkflowExecutionInitiated</code> – A request to signal an external workflow was made.</p> </li> <li> <p> <code>StartActivityTaskFailed</code> – A scheduled activity task failed to start.</p> </li> <li> <p> <code>StartChildWorkflowExecutionFailed</code> – Failed to process StartChildWorkflowExecution decision. This happens when the decision isn't configured properly, for example the workflow type specified isn't registered.</p> </li> <li> <p> <code>StartChildWorkflowExecutionInitiated</code> – A request was made to start a child workflow execution.</p> </li> <li> <p> <code>StartTimerFailed</code> – Failed to process StartTimer decision. This happens when the decision isn't configured properly, for example a timer already exists with the specified timer Id.</p> </li> <li> <p> <code>TimerCanceled</code> – A timer, previously started for this workflow execution, was successfully canceled.</p> </li> <li> <p> <code>TimerFired</code> – A timer, previously started for this workflow execution, fired.</p> </li> <li> <p> <code>TimerStarted</code> – A timer was started for the workflow execution due to a <code>StartTimer</code> decision.</p> </li> <li> <p> <code>WorkflowExecutionCancelRequested</code> – A request to cancel this workflow execution was made.</p> </li> <li> <p> <code>WorkflowExecutionCanceled</code> – The workflow execution was successfully canceled and closed.</p> </li> <li> <p> <code>WorkflowExecutionCompleted</code> – The workflow execution was closed due to successful completion.</p> </li> <li> <p> <code>WorkflowExecutionContinuedAsNew</code> – The workflow execution was closed and a new execution of the same type was created with the same workflowId.</p> </li> <li> <p> <code>WorkflowExecutionFailed</code> – The workflow execution closed due to a failure.</p> </li> <li> <p> <code>WorkflowExecutionSignaled</code> – An external signal was received for the workflow execution.</p> </li> <li> <p> <code>WorkflowExecutionStarted</code> – The workflow execution was started.</p> </li> <li> <p> <code>WorkflowExecutionTerminated</code> – The workflow execution was terminated.</p> </li> <li> <p> <code>WorkflowExecutionTimedOut</code> – The workflow execution was closed because a time out was exceeded.</p> </li> </ul>
newtype HistoryEvent = HistoryEvent 
  { "EventTimestamp'" :: (Number)
  , "EventType'" :: (EventType)
  , "EventId'" :: (EventId)
  , "WorkflowExecutionStartedEventAttributes'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionStartedEventAttributes)
  , "WorkflowExecutionCompletedEventAttributes'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionCompletedEventAttributes)
  , "CompleteWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (CompleteWorkflowExecutionFailedEventAttributes)
  , "WorkflowExecutionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionFailedEventAttributes)
  , "FailWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (FailWorkflowExecutionFailedEventAttributes)
  , "WorkflowExecutionTimedOutEventAttributes'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionTimedOutEventAttributes)
  , "WorkflowExecutionCanceledEventAttributes'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionCanceledEventAttributes)
  , "CancelWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (CancelWorkflowExecutionFailedEventAttributes)
  , "WorkflowExecutionContinuedAsNewEventAttributes'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionContinuedAsNewEventAttributes)
  , "ContinueAsNewWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ContinueAsNewWorkflowExecutionFailedEventAttributes)
  , "WorkflowExecutionTerminatedEventAttributes'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionTerminatedEventAttributes)
  , "WorkflowExecutionCancelRequestedEventAttributes'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionCancelRequestedEventAttributes)
  , "DecisionTaskScheduledEventAttributes'" :: NullOrUndefined.NullOrUndefined (DecisionTaskScheduledEventAttributes)
  , "DecisionTaskStartedEventAttributes'" :: NullOrUndefined.NullOrUndefined (DecisionTaskStartedEventAttributes)
  , "DecisionTaskCompletedEventAttributes'" :: NullOrUndefined.NullOrUndefined (DecisionTaskCompletedEventAttributes)
  , "DecisionTaskTimedOutEventAttributes'" :: NullOrUndefined.NullOrUndefined (DecisionTaskTimedOutEventAttributes)
  , "ActivityTaskScheduledEventAttributes'" :: NullOrUndefined.NullOrUndefined (ActivityTaskScheduledEventAttributes)
  , "ActivityTaskStartedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ActivityTaskStartedEventAttributes)
  , "ActivityTaskCompletedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ActivityTaskCompletedEventAttributes)
  , "ActivityTaskFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ActivityTaskFailedEventAttributes)
  , "ActivityTaskTimedOutEventAttributes'" :: NullOrUndefined.NullOrUndefined (ActivityTaskTimedOutEventAttributes)
  , "ActivityTaskCanceledEventAttributes'" :: NullOrUndefined.NullOrUndefined (ActivityTaskCanceledEventAttributes)
  , "ActivityTaskCancelRequestedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ActivityTaskCancelRequestedEventAttributes)
  , "WorkflowExecutionSignaledEventAttributes'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionSignaledEventAttributes)
  , "MarkerRecordedEventAttributes'" :: NullOrUndefined.NullOrUndefined (MarkerRecordedEventAttributes)
  , "RecordMarkerFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (RecordMarkerFailedEventAttributes)
  , "TimerStartedEventAttributes'" :: NullOrUndefined.NullOrUndefined (TimerStartedEventAttributes)
  , "TimerFiredEventAttributes'" :: NullOrUndefined.NullOrUndefined (TimerFiredEventAttributes)
  , "TimerCanceledEventAttributes'" :: NullOrUndefined.NullOrUndefined (TimerCanceledEventAttributes)
  , "StartChildWorkflowExecutionInitiatedEventAttributes'" :: NullOrUndefined.NullOrUndefined (StartChildWorkflowExecutionInitiatedEventAttributes)
  , "ChildWorkflowExecutionStartedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ChildWorkflowExecutionStartedEventAttributes)
  , "ChildWorkflowExecutionCompletedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ChildWorkflowExecutionCompletedEventAttributes)
  , "ChildWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ChildWorkflowExecutionFailedEventAttributes)
  , "ChildWorkflowExecutionTimedOutEventAttributes'" :: NullOrUndefined.NullOrUndefined (ChildWorkflowExecutionTimedOutEventAttributes)
  , "ChildWorkflowExecutionCanceledEventAttributes'" :: NullOrUndefined.NullOrUndefined (ChildWorkflowExecutionCanceledEventAttributes)
  , "ChildWorkflowExecutionTerminatedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ChildWorkflowExecutionTerminatedEventAttributes)
  , "SignalExternalWorkflowExecutionInitiatedEventAttributes'" :: NullOrUndefined.NullOrUndefined (SignalExternalWorkflowExecutionInitiatedEventAttributes)
  , "ExternalWorkflowExecutionSignaledEventAttributes'" :: NullOrUndefined.NullOrUndefined (ExternalWorkflowExecutionSignaledEventAttributes)
  , "SignalExternalWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (SignalExternalWorkflowExecutionFailedEventAttributes)
  , "ExternalWorkflowExecutionCancelRequestedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ExternalWorkflowExecutionCancelRequestedEventAttributes)
  , "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'" :: NullOrUndefined.NullOrUndefined (RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
  , "RequestCancelExternalWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (RequestCancelExternalWorkflowExecutionFailedEventAttributes)
  , "ScheduleActivityTaskFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ScheduleActivityTaskFailedEventAttributes)
  , "RequestCancelActivityTaskFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (RequestCancelActivityTaskFailedEventAttributes)
  , "StartTimerFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (StartTimerFailedEventAttributes)
  , "CancelTimerFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (CancelTimerFailedEventAttributes)
  , "StartChildWorkflowExecutionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (StartChildWorkflowExecutionFailedEventAttributes)
  , "LambdaFunctionScheduledEventAttributes'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionScheduledEventAttributes)
  , "LambdaFunctionStartedEventAttributes'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionStartedEventAttributes)
  , "LambdaFunctionCompletedEventAttributes'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionCompletedEventAttributes)
  , "LambdaFunctionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionFailedEventAttributes)
  , "LambdaFunctionTimedOutEventAttributes'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionTimedOutEventAttributes)
  , "ScheduleLambdaFunctionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (ScheduleLambdaFunctionFailedEventAttributes)
  , "StartLambdaFunctionFailedEventAttributes'" :: NullOrUndefined.NullOrUndefined (StartLambdaFunctionFailedEventAttributes)
  }
derive instance newtypeHistoryEvent :: Newtype HistoryEvent _
derive instance repGenericHistoryEvent :: Generic HistoryEvent _
instance showHistoryEvent :: Show HistoryEvent where
  show = genericShow
instance decodeHistoryEvent :: Decode HistoryEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistoryEvent :: Encode HistoryEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HistoryEventList = HistoryEventList (Array HistoryEvent)
derive instance newtypeHistoryEventList :: Newtype HistoryEventList _
derive instance repGenericHistoryEventList :: Generic HistoryEventList _
instance showHistoryEventList :: Show HistoryEventList where
  show = genericShow
instance decodeHistoryEventList :: Decode HistoryEventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistoryEventList :: Encode HistoryEventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Identity = Identity String
derive instance newtypeIdentity :: Newtype Identity _
derive instance repGenericIdentity :: Generic Identity _
instance showIdentity :: Show Identity where
  show = genericShow
instance decodeIdentity :: Decode Identity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentity :: Encode Identity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>LambdaFunctionCompleted</code> event. It isn't set for other event types.</p>
newtype LambdaFunctionCompletedEventAttributes = LambdaFunctionCompletedEventAttributes 
  { "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "Result'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeLambdaFunctionCompletedEventAttributes :: Newtype LambdaFunctionCompletedEventAttributes _
derive instance repGenericLambdaFunctionCompletedEventAttributes :: Generic LambdaFunctionCompletedEventAttributes _
instance showLambdaFunctionCompletedEventAttributes :: Show LambdaFunctionCompletedEventAttributes where
  show = genericShow
instance decodeLambdaFunctionCompletedEventAttributes :: Decode LambdaFunctionCompletedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionCompletedEventAttributes :: Encode LambdaFunctionCompletedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>LambdaFunctionFailed</code> event. It isn't set for other event types.</p>
newtype LambdaFunctionFailedEventAttributes = LambdaFunctionFailedEventAttributes 
  { "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeLambdaFunctionFailedEventAttributes :: Newtype LambdaFunctionFailedEventAttributes _
derive instance repGenericLambdaFunctionFailedEventAttributes :: Generic LambdaFunctionFailedEventAttributes _
instance showLambdaFunctionFailedEventAttributes :: Show LambdaFunctionFailedEventAttributes where
  show = genericShow
instance decodeLambdaFunctionFailedEventAttributes :: Decode LambdaFunctionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionFailedEventAttributes :: Encode LambdaFunctionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>LambdaFunctionScheduled</code> event. It isn't set for other event types.</p>
newtype LambdaFunctionScheduledEventAttributes = LambdaFunctionScheduledEventAttributes 
  { "Id'" :: (FunctionId)
  , "Name'" :: (FunctionName)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined.NullOrUndefined (FunctionInput)
  , "StartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeLambdaFunctionScheduledEventAttributes :: Newtype LambdaFunctionScheduledEventAttributes _
derive instance repGenericLambdaFunctionScheduledEventAttributes :: Generic LambdaFunctionScheduledEventAttributes _
instance showLambdaFunctionScheduledEventAttributes :: Show LambdaFunctionScheduledEventAttributes where
  show = genericShow
instance decodeLambdaFunctionScheduledEventAttributes :: Decode LambdaFunctionScheduledEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionScheduledEventAttributes :: Encode LambdaFunctionScheduledEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>LambdaFunctionStarted</code> event. It isn't set for other event types.</p>
newtype LambdaFunctionStartedEventAttributes = LambdaFunctionStartedEventAttributes 
  { "ScheduledEventId'" :: (EventId)
  }
derive instance newtypeLambdaFunctionStartedEventAttributes :: Newtype LambdaFunctionStartedEventAttributes _
derive instance repGenericLambdaFunctionStartedEventAttributes :: Generic LambdaFunctionStartedEventAttributes _
instance showLambdaFunctionStartedEventAttributes :: Show LambdaFunctionStartedEventAttributes where
  show = genericShow
instance decodeLambdaFunctionStartedEventAttributes :: Decode LambdaFunctionStartedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionStartedEventAttributes :: Encode LambdaFunctionStartedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides details of the <code>LambdaFunctionTimedOut</code> event.</p>
newtype LambdaFunctionTimedOutEventAttributes = LambdaFunctionTimedOutEventAttributes 
  { "ScheduledEventId'" :: (EventId)
  , "StartedEventId'" :: (EventId)
  , "TimeoutType'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionTimeoutType)
  }
derive instance newtypeLambdaFunctionTimedOutEventAttributes :: Newtype LambdaFunctionTimedOutEventAttributes _
derive instance repGenericLambdaFunctionTimedOutEventAttributes :: Generic LambdaFunctionTimedOutEventAttributes _
instance showLambdaFunctionTimedOutEventAttributes :: Show LambdaFunctionTimedOutEventAttributes where
  show = genericShow
instance decodeLambdaFunctionTimedOutEventAttributes :: Decode LambdaFunctionTimedOutEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionTimedOutEventAttributes :: Encode LambdaFunctionTimedOutEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LambdaFunctionTimeoutType = LambdaFunctionTimeoutType String
derive instance newtypeLambdaFunctionTimeoutType :: Newtype LambdaFunctionTimeoutType _
derive instance repGenericLambdaFunctionTimeoutType :: Generic LambdaFunctionTimeoutType _
instance showLambdaFunctionTimeoutType :: Show LambdaFunctionTimeoutType where
  show = genericShow
instance decodeLambdaFunctionTimeoutType :: Decode LambdaFunctionTimeoutType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionTimeoutType :: Encode LambdaFunctionTimeoutType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned by any operation if a system imposed limitation has been reached. To address this fault you should either clean up unused resources or increase the limit by contacting AWS.</p>
newtype LimitExceededFault = LimitExceededFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededFault :: Newtype LimitExceededFault _
derive instance repGenericLimitExceededFault :: Generic LimitExceededFault _
instance showLimitExceededFault :: Show LimitExceededFault where
  show = genericShow
instance decodeLimitExceededFault :: Decode LimitExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededFault :: Encode LimitExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitedData = LimitedData String
derive instance newtypeLimitedData :: Newtype LimitedData _
derive instance repGenericLimitedData :: Generic LimitedData _
instance showLimitedData :: Show LimitedData where
  show = genericShow
instance decodeLimitedData :: Decode LimitedData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitedData :: Encode LimitedData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListActivityTypesInput = ListActivityTypesInput 
  { "Domain'" :: (DomainName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "RegistrationStatus'" :: (RegistrationStatus)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined.NullOrUndefined (ReverseOrder)
  }
derive instance newtypeListActivityTypesInput :: Newtype ListActivityTypesInput _
derive instance repGenericListActivityTypesInput :: Generic ListActivityTypesInput _
instance showListActivityTypesInput :: Show ListActivityTypesInput where
  show = genericShow
instance decodeListActivityTypesInput :: Decode ListActivityTypesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListActivityTypesInput :: Encode ListActivityTypesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListClosedWorkflowExecutionsInput = ListClosedWorkflowExecutionsInput 
  { "Domain'" :: (DomainName)
  , "StartTimeFilter'" :: NullOrUndefined.NullOrUndefined (ExecutionTimeFilter)
  , "CloseTimeFilter'" :: NullOrUndefined.NullOrUndefined (ExecutionTimeFilter)
  , "ExecutionFilter'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionFilter)
  , "CloseStatusFilter'" :: NullOrUndefined.NullOrUndefined (CloseStatusFilter)
  , "TypeFilter'" :: NullOrUndefined.NullOrUndefined (WorkflowTypeFilter)
  , "TagFilter'" :: NullOrUndefined.NullOrUndefined (TagFilter)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined.NullOrUndefined (ReverseOrder)
  }
derive instance newtypeListClosedWorkflowExecutionsInput :: Newtype ListClosedWorkflowExecutionsInput _
derive instance repGenericListClosedWorkflowExecutionsInput :: Generic ListClosedWorkflowExecutionsInput _
instance showListClosedWorkflowExecutionsInput :: Show ListClosedWorkflowExecutionsInput where
  show = genericShow
instance decodeListClosedWorkflowExecutionsInput :: Decode ListClosedWorkflowExecutionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListClosedWorkflowExecutionsInput :: Encode ListClosedWorkflowExecutionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDomainsInput = ListDomainsInput 
  { "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  , "RegistrationStatus'" :: (RegistrationStatus)
  , "MaximumPageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined.NullOrUndefined (ReverseOrder)
  }
derive instance newtypeListDomainsInput :: Newtype ListDomainsInput _
derive instance repGenericListDomainsInput :: Generic ListDomainsInput _
instance showListDomainsInput :: Show ListDomainsInput where
  show = genericShow
instance decodeListDomainsInput :: Decode ListDomainsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDomainsInput :: Encode ListDomainsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOpenWorkflowExecutionsInput = ListOpenWorkflowExecutionsInput 
  { "Domain'" :: (DomainName)
  , "StartTimeFilter'" :: (ExecutionTimeFilter)
  , "TypeFilter'" :: NullOrUndefined.NullOrUndefined (WorkflowTypeFilter)
  , "TagFilter'" :: NullOrUndefined.NullOrUndefined (TagFilter)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined.NullOrUndefined (ReverseOrder)
  , "ExecutionFilter'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionFilter)
  }
derive instance newtypeListOpenWorkflowExecutionsInput :: Newtype ListOpenWorkflowExecutionsInput _
derive instance repGenericListOpenWorkflowExecutionsInput :: Generic ListOpenWorkflowExecutionsInput _
instance showListOpenWorkflowExecutionsInput :: Show ListOpenWorkflowExecutionsInput where
  show = genericShow
instance decodeListOpenWorkflowExecutionsInput :: Decode ListOpenWorkflowExecutionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOpenWorkflowExecutionsInput :: Encode ListOpenWorkflowExecutionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListWorkflowTypesInput = ListWorkflowTypesInput 
  { "Domain'" :: (DomainName)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "RegistrationStatus'" :: (RegistrationStatus)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined.NullOrUndefined (ReverseOrder)
  }
derive instance newtypeListWorkflowTypesInput :: Newtype ListWorkflowTypesInput _
derive instance repGenericListWorkflowTypesInput :: Generic ListWorkflowTypesInput _
instance showListWorkflowTypesInput :: Show ListWorkflowTypesInput where
  show = genericShow
instance decodeListWorkflowTypesInput :: Decode ListWorkflowTypesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListWorkflowTypesInput :: Encode ListWorkflowTypesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MarkerName = MarkerName String
derive instance newtypeMarkerName :: Newtype MarkerName _
derive instance repGenericMarkerName :: Generic MarkerName _
instance showMarkerName :: Show MarkerName where
  show = genericShow
instance decodeMarkerName :: Decode MarkerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarkerName :: Encode MarkerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>MarkerRecorded</code> event.</p>
newtype MarkerRecordedEventAttributes = MarkerRecordedEventAttributes 
  { "MarkerName'" :: (MarkerName)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeMarkerRecordedEventAttributes :: Newtype MarkerRecordedEventAttributes _
derive instance repGenericMarkerRecordedEventAttributes :: Generic MarkerRecordedEventAttributes _
instance showMarkerRecordedEventAttributes :: Show MarkerRecordedEventAttributes where
  show = genericShow
instance decodeMarkerRecordedEventAttributes :: Decode MarkerRecordedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarkerRecordedEventAttributes :: Encode MarkerRecordedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive instance repGenericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow
instance decodeName :: Decode Name where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeName :: Encode Name where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OpenDecisionTasksCount = OpenDecisionTasksCount Int
derive instance newtypeOpenDecisionTasksCount :: Newtype OpenDecisionTasksCount _
derive instance repGenericOpenDecisionTasksCount :: Generic OpenDecisionTasksCount _
instance showOpenDecisionTasksCount :: Show OpenDecisionTasksCount where
  show = genericShow
instance decodeOpenDecisionTasksCount :: Decode OpenDecisionTasksCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOpenDecisionTasksCount :: Encode OpenDecisionTasksCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned when the caller doesn't have sufficient permissions to invoke the action.</p>
newtype OperationNotPermittedFault = OperationNotPermittedFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeOperationNotPermittedFault :: Newtype OperationNotPermittedFault _
derive instance repGenericOperationNotPermittedFault :: Generic OperationNotPermittedFault _
instance showOperationNotPermittedFault :: Show OperationNotPermittedFault where
  show = genericShow
instance decodeOperationNotPermittedFault :: Decode OperationNotPermittedFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationNotPermittedFault :: Encode OperationNotPermittedFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _
derive instance repGenericPageSize :: Generic PageSize _
instance showPageSize :: Show PageSize where
  show = genericShow
instance decodePageSize :: Decode PageSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageSize :: Encode PageSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageToken = PageToken String
derive instance newtypePageToken :: Newtype PageToken _
derive instance repGenericPageToken :: Generic PageToken _
instance showPageToken :: Show PageToken where
  show = genericShow
instance decodePageToken :: Decode PageToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageToken :: Encode PageToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the count of tasks in a task list.</p>
newtype PendingTaskCount = PendingTaskCount 
  { "Count'" :: (Count)
  , "Truncated'" :: NullOrUndefined.NullOrUndefined (Truncated)
  }
derive instance newtypePendingTaskCount :: Newtype PendingTaskCount _
derive instance repGenericPendingTaskCount :: Generic PendingTaskCount _
instance showPendingTaskCount :: Show PendingTaskCount where
  show = genericShow
instance decodePendingTaskCount :: Decode PendingTaskCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePendingTaskCount :: Encode PendingTaskCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PollForActivityTaskInput = PollForActivityTaskInput 
  { "Domain'" :: (DomainName)
  , "TaskList'" :: (TaskList)
  , "Identity'" :: NullOrUndefined.NullOrUndefined (Identity)
  }
derive instance newtypePollForActivityTaskInput :: Newtype PollForActivityTaskInput _
derive instance repGenericPollForActivityTaskInput :: Generic PollForActivityTaskInput _
instance showPollForActivityTaskInput :: Show PollForActivityTaskInput where
  show = genericShow
instance decodePollForActivityTaskInput :: Decode PollForActivityTaskInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePollForActivityTaskInput :: Encode PollForActivityTaskInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PollForDecisionTaskInput = PollForDecisionTaskInput 
  { "Domain'" :: (DomainName)
  , "TaskList'" :: (TaskList)
  , "Identity'" :: NullOrUndefined.NullOrUndefined (Identity)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  , "MaximumPageSize'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined.NullOrUndefined (ReverseOrder)
  }
derive instance newtypePollForDecisionTaskInput :: Newtype PollForDecisionTaskInput _
derive instance repGenericPollForDecisionTaskInput :: Generic PollForDecisionTaskInput _
instance showPollForDecisionTaskInput :: Show PollForDecisionTaskInput where
  show = genericShow
instance decodePollForDecisionTaskInput :: Decode PollForDecisionTaskInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePollForDecisionTaskInput :: Encode PollForDecisionTaskInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordActivityTaskHeartbeatInput = RecordActivityTaskHeartbeatInput 
  { "TaskToken'" :: (TaskToken)
  , "Details'" :: NullOrUndefined.NullOrUndefined (LimitedData)
  }
derive instance newtypeRecordActivityTaskHeartbeatInput :: Newtype RecordActivityTaskHeartbeatInput _
derive instance repGenericRecordActivityTaskHeartbeatInput :: Generic RecordActivityTaskHeartbeatInput _
instance showRecordActivityTaskHeartbeatInput :: Show RecordActivityTaskHeartbeatInput where
  show = genericShow
instance decodeRecordActivityTaskHeartbeatInput :: Decode RecordActivityTaskHeartbeatInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordActivityTaskHeartbeatInput :: Encode RecordActivityTaskHeartbeatInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>RecordMarker</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype RecordMarkerDecisionAttributes = RecordMarkerDecisionAttributes 
  { "MarkerName'" :: (MarkerName)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeRecordMarkerDecisionAttributes :: Newtype RecordMarkerDecisionAttributes _
derive instance repGenericRecordMarkerDecisionAttributes :: Generic RecordMarkerDecisionAttributes _
instance showRecordMarkerDecisionAttributes :: Show RecordMarkerDecisionAttributes where
  show = genericShow
instance decodeRecordMarkerDecisionAttributes :: Decode RecordMarkerDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordMarkerDecisionAttributes :: Encode RecordMarkerDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordMarkerFailedCause = RecordMarkerFailedCause String
derive instance newtypeRecordMarkerFailedCause :: Newtype RecordMarkerFailedCause _
derive instance repGenericRecordMarkerFailedCause :: Generic RecordMarkerFailedCause _
instance showRecordMarkerFailedCause :: Show RecordMarkerFailedCause where
  show = genericShow
instance decodeRecordMarkerFailedCause :: Decode RecordMarkerFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordMarkerFailedCause :: Encode RecordMarkerFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>RecordMarkerFailed</code> event.</p>
newtype RecordMarkerFailedEventAttributes = RecordMarkerFailedEventAttributes 
  { "MarkerName'" :: (MarkerName)
  , "Cause'" :: (RecordMarkerFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeRecordMarkerFailedEventAttributes :: Newtype RecordMarkerFailedEventAttributes _
derive instance repGenericRecordMarkerFailedEventAttributes :: Generic RecordMarkerFailedEventAttributes _
instance showRecordMarkerFailedEventAttributes :: Show RecordMarkerFailedEventAttributes where
  show = genericShow
instance decodeRecordMarkerFailedEventAttributes :: Decode RecordMarkerFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordMarkerFailedEventAttributes :: Encode RecordMarkerFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterActivityTypeInput = RegisterActivityTypeInput 
  { "Domain'" :: (DomainName)
  , "Name'" :: (Name)
  , "Version'" :: (Version)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskHeartbeatTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskList'" :: NullOrUndefined.NullOrUndefined (TaskList)
  , "DefaultTaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "DefaultTaskScheduleToStartTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskScheduleToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeRegisterActivityTypeInput :: Newtype RegisterActivityTypeInput _
derive instance repGenericRegisterActivityTypeInput :: Generic RegisterActivityTypeInput _
instance showRegisterActivityTypeInput :: Show RegisterActivityTypeInput where
  show = genericShow
instance decodeRegisterActivityTypeInput :: Decode RegisterActivityTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterActivityTypeInput :: Encode RegisterActivityTypeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterDomainInput = RegisterDomainInput 
  { "Name'" :: (DomainName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "WorkflowExecutionRetentionPeriodInDays'" :: (DurationInDays)
  }
derive instance newtypeRegisterDomainInput :: Newtype RegisterDomainInput _
derive instance repGenericRegisterDomainInput :: Generic RegisterDomainInput _
instance showRegisterDomainInput :: Show RegisterDomainInput where
  show = genericShow
instance decodeRegisterDomainInput :: Decode RegisterDomainInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterDomainInput :: Encode RegisterDomainInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterWorkflowTypeInput = RegisterWorkflowTypeInput 
  { "Domain'" :: (DomainName)
  , "Name'" :: (Name)
  , "Version'" :: (Version)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultExecutionStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskList'" :: NullOrUndefined.NullOrUndefined (TaskList)
  , "DefaultTaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "DefaultChildPolicy'" :: NullOrUndefined.NullOrUndefined (ChildPolicy)
  , "DefaultLambdaRole'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeRegisterWorkflowTypeInput :: Newtype RegisterWorkflowTypeInput _
derive instance repGenericRegisterWorkflowTypeInput :: Generic RegisterWorkflowTypeInput _
instance showRegisterWorkflowTypeInput :: Show RegisterWorkflowTypeInput where
  show = genericShow
instance decodeRegisterWorkflowTypeInput :: Decode RegisterWorkflowTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterWorkflowTypeInput :: Encode RegisterWorkflowTypeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegistrationStatus = RegistrationStatus String
derive instance newtypeRegistrationStatus :: Newtype RegistrationStatus _
derive instance repGenericRegistrationStatus :: Generic RegistrationStatus _
instance showRegistrationStatus :: Show RegistrationStatus where
  show = genericShow
instance decodeRegistrationStatus :: Decode RegistrationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistrationStatus :: Encode RegistrationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>RequestCancelActivityTask</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype RequestCancelActivityTaskDecisionAttributes = RequestCancelActivityTaskDecisionAttributes 
  { "ActivityId'" :: (ActivityId)
  }
derive instance newtypeRequestCancelActivityTaskDecisionAttributes :: Newtype RequestCancelActivityTaskDecisionAttributes _
derive instance repGenericRequestCancelActivityTaskDecisionAttributes :: Generic RequestCancelActivityTaskDecisionAttributes _
instance showRequestCancelActivityTaskDecisionAttributes :: Show RequestCancelActivityTaskDecisionAttributes where
  show = genericShow
instance decodeRequestCancelActivityTaskDecisionAttributes :: Decode RequestCancelActivityTaskDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCancelActivityTaskDecisionAttributes :: Encode RequestCancelActivityTaskDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestCancelActivityTaskFailedCause = RequestCancelActivityTaskFailedCause String
derive instance newtypeRequestCancelActivityTaskFailedCause :: Newtype RequestCancelActivityTaskFailedCause _
derive instance repGenericRequestCancelActivityTaskFailedCause :: Generic RequestCancelActivityTaskFailedCause _
instance showRequestCancelActivityTaskFailedCause :: Show RequestCancelActivityTaskFailedCause where
  show = genericShow
instance decodeRequestCancelActivityTaskFailedCause :: Decode RequestCancelActivityTaskFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCancelActivityTaskFailedCause :: Encode RequestCancelActivityTaskFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>RequestCancelActivityTaskFailed</code> event.</p>
newtype RequestCancelActivityTaskFailedEventAttributes = RequestCancelActivityTaskFailedEventAttributes 
  { "ActivityId'" :: (ActivityId)
  , "Cause'" :: (RequestCancelActivityTaskFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeRequestCancelActivityTaskFailedEventAttributes :: Newtype RequestCancelActivityTaskFailedEventAttributes _
derive instance repGenericRequestCancelActivityTaskFailedEventAttributes :: Generic RequestCancelActivityTaskFailedEventAttributes _
instance showRequestCancelActivityTaskFailedEventAttributes :: Show RequestCancelActivityTaskFailedEventAttributes where
  show = genericShow
instance decodeRequestCancelActivityTaskFailedEventAttributes :: Decode RequestCancelActivityTaskFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCancelActivityTaskFailedEventAttributes :: Encode RequestCancelActivityTaskFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>RequestCancelExternalWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeRequestCancelExternalWorkflowExecutionDecisionAttributes :: Newtype RequestCancelExternalWorkflowExecutionDecisionAttributes _
derive instance repGenericRequestCancelExternalWorkflowExecutionDecisionAttributes :: Generic RequestCancelExternalWorkflowExecutionDecisionAttributes _
instance showRequestCancelExternalWorkflowExecutionDecisionAttributes :: Show RequestCancelExternalWorkflowExecutionDecisionAttributes where
  show = genericShow
instance decodeRequestCancelExternalWorkflowExecutionDecisionAttributes :: Decode RequestCancelExternalWorkflowExecutionDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCancelExternalWorkflowExecutionDecisionAttributes :: Encode RequestCancelExternalWorkflowExecutionDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestCancelExternalWorkflowExecutionFailedCause = RequestCancelExternalWorkflowExecutionFailedCause String
derive instance newtypeRequestCancelExternalWorkflowExecutionFailedCause :: Newtype RequestCancelExternalWorkflowExecutionFailedCause _
derive instance repGenericRequestCancelExternalWorkflowExecutionFailedCause :: Generic RequestCancelExternalWorkflowExecutionFailedCause _
instance showRequestCancelExternalWorkflowExecutionFailedCause :: Show RequestCancelExternalWorkflowExecutionFailedCause where
  show = genericShow
instance decodeRequestCancelExternalWorkflowExecutionFailedCause :: Decode RequestCancelExternalWorkflowExecutionFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCancelExternalWorkflowExecutionFailedCause :: Encode RequestCancelExternalWorkflowExecutionFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>RequestCancelExternalWorkflowExecutionFailed</code> event.</p>
newtype RequestCancelExternalWorkflowExecutionFailedEventAttributes = RequestCancelExternalWorkflowExecutionFailedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  , "Cause'" :: (RequestCancelExternalWorkflowExecutionFailedCause)
  , "InitiatedEventId'" :: (EventId)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Newtype RequestCancelExternalWorkflowExecutionFailedEventAttributes _
derive instance repGenericRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Generic RequestCancelExternalWorkflowExecutionFailedEventAttributes _
instance showRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Show RequestCancelExternalWorkflowExecutionFailedEventAttributes where
  show = genericShow
instance decodeRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Decode RequestCancelExternalWorkflowExecutionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCancelExternalWorkflowExecutionFailedEventAttributes :: Encode RequestCancelExternalWorkflowExecutionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>RequestCancelExternalWorkflowExecutionInitiated</code> event.</p>
newtype RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Newtype RequestCancelExternalWorkflowExecutionInitiatedEventAttributes _
derive instance repGenericRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Generic RequestCancelExternalWorkflowExecutionInitiatedEventAttributes _
instance showRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Show RequestCancelExternalWorkflowExecutionInitiatedEventAttributes where
  show = genericShow
instance decodeRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Decode RequestCancelExternalWorkflowExecutionInitiatedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Encode RequestCancelExternalWorkflowExecutionInitiatedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestCancelWorkflowExecutionInput = RequestCancelWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  }
derive instance newtypeRequestCancelWorkflowExecutionInput :: Newtype RequestCancelWorkflowExecutionInput _
derive instance repGenericRequestCancelWorkflowExecutionInput :: Generic RequestCancelWorkflowExecutionInput _
instance showRequestCancelWorkflowExecutionInput :: Show RequestCancelWorkflowExecutionInput where
  show = genericShow
instance decodeRequestCancelWorkflowExecutionInput :: Decode RequestCancelWorkflowExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCancelWorkflowExecutionInput :: Encode RequestCancelWorkflowExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RespondActivityTaskCanceledInput = RespondActivityTaskCanceledInput 
  { "TaskToken'" :: (TaskToken)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeRespondActivityTaskCanceledInput :: Newtype RespondActivityTaskCanceledInput _
derive instance repGenericRespondActivityTaskCanceledInput :: Generic RespondActivityTaskCanceledInput _
instance showRespondActivityTaskCanceledInput :: Show RespondActivityTaskCanceledInput where
  show = genericShow
instance decodeRespondActivityTaskCanceledInput :: Decode RespondActivityTaskCanceledInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRespondActivityTaskCanceledInput :: Encode RespondActivityTaskCanceledInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RespondActivityTaskCompletedInput = RespondActivityTaskCompletedInput 
  { "TaskToken'" :: (TaskToken)
  , "Result'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeRespondActivityTaskCompletedInput :: Newtype RespondActivityTaskCompletedInput _
derive instance repGenericRespondActivityTaskCompletedInput :: Generic RespondActivityTaskCompletedInput _
instance showRespondActivityTaskCompletedInput :: Show RespondActivityTaskCompletedInput where
  show = genericShow
instance decodeRespondActivityTaskCompletedInput :: Decode RespondActivityTaskCompletedInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRespondActivityTaskCompletedInput :: Encode RespondActivityTaskCompletedInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RespondActivityTaskFailedInput = RespondActivityTaskFailedInput 
  { "TaskToken'" :: (TaskToken)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeRespondActivityTaskFailedInput :: Newtype RespondActivityTaskFailedInput _
derive instance repGenericRespondActivityTaskFailedInput :: Generic RespondActivityTaskFailedInput _
instance showRespondActivityTaskFailedInput :: Show RespondActivityTaskFailedInput where
  show = genericShow
instance decodeRespondActivityTaskFailedInput :: Decode RespondActivityTaskFailedInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRespondActivityTaskFailedInput :: Encode RespondActivityTaskFailedInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input data for a TaskCompleted response to a decision task.</p>
newtype RespondDecisionTaskCompletedInput = RespondDecisionTaskCompletedInput 
  { "TaskToken'" :: (TaskToken)
  , "Decisions'" :: NullOrUndefined.NullOrUndefined (DecisionList)
  , "ExecutionContext'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeRespondDecisionTaskCompletedInput :: Newtype RespondDecisionTaskCompletedInput _
derive instance repGenericRespondDecisionTaskCompletedInput :: Generic RespondDecisionTaskCompletedInput _
instance showRespondDecisionTaskCompletedInput :: Show RespondDecisionTaskCompletedInput where
  show = genericShow
instance decodeRespondDecisionTaskCompletedInput :: Decode RespondDecisionTaskCompletedInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRespondDecisionTaskCompletedInput :: Encode RespondDecisionTaskCompletedInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReverseOrder = ReverseOrder Boolean
derive instance newtypeReverseOrder :: Newtype ReverseOrder _
derive instance repGenericReverseOrder :: Generic ReverseOrder _
instance showReverseOrder :: Show ReverseOrder where
  show = genericShow
instance decodeReverseOrder :: Decode ReverseOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReverseOrder :: Encode ReverseOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the <code>runId</code> of a workflow execution.</p>
newtype Run = Run 
  { "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunId)
  }
derive instance newtypeRun :: Newtype Run _
derive instance repGenericRun :: Generic Run _
instance showRun :: Show Run where
  show = genericShow
instance decodeRun :: Decode Run where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRun :: Encode Run where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ScheduleActivityTask</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>activityType.name</code> – String constraint. The key is <code>swf:activityType.name</code>.</p> </li> <li> <p> <code>activityType.version</code> – String constraint. The key is <code>swf:activityType.version</code>.</p> </li> <li> <p> <code>taskList</code> – String constraint. The key is <code>swf:taskList.name</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes 
  { "ActivityType'" :: (ActivityType)
  , "ActivityId'" :: (ActivityId)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ScheduleToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: NullOrUndefined.NullOrUndefined (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "ScheduleToStartTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "StartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "HeartbeatTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeScheduleActivityTaskDecisionAttributes :: Newtype ScheduleActivityTaskDecisionAttributes _
derive instance repGenericScheduleActivityTaskDecisionAttributes :: Generic ScheduleActivityTaskDecisionAttributes _
instance showScheduleActivityTaskDecisionAttributes :: Show ScheduleActivityTaskDecisionAttributes where
  show = genericShow
instance decodeScheduleActivityTaskDecisionAttributes :: Decode ScheduleActivityTaskDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleActivityTaskDecisionAttributes :: Encode ScheduleActivityTaskDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScheduleActivityTaskFailedCause = ScheduleActivityTaskFailedCause String
derive instance newtypeScheduleActivityTaskFailedCause :: Newtype ScheduleActivityTaskFailedCause _
derive instance repGenericScheduleActivityTaskFailedCause :: Generic ScheduleActivityTaskFailedCause _
instance showScheduleActivityTaskFailedCause :: Show ScheduleActivityTaskFailedCause where
  show = genericShow
instance decodeScheduleActivityTaskFailedCause :: Decode ScheduleActivityTaskFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleActivityTaskFailedCause :: Encode ScheduleActivityTaskFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ScheduleActivityTaskFailed</code> event.</p>
newtype ScheduleActivityTaskFailedEventAttributes = ScheduleActivityTaskFailedEventAttributes 
  { "ActivityType'" :: (ActivityType)
  , "ActivityId'" :: (ActivityId)
  , "Cause'" :: (ScheduleActivityTaskFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeScheduleActivityTaskFailedEventAttributes :: Newtype ScheduleActivityTaskFailedEventAttributes _
derive instance repGenericScheduleActivityTaskFailedEventAttributes :: Generic ScheduleActivityTaskFailedEventAttributes _
instance showScheduleActivityTaskFailedEventAttributes :: Show ScheduleActivityTaskFailedEventAttributes where
  show = genericShow
instance decodeScheduleActivityTaskFailedEventAttributes :: Decode ScheduleActivityTaskFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleActivityTaskFailedEventAttributes :: Encode ScheduleActivityTaskFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Decision attributes specified in <code>scheduleLambdaFunctionDecisionAttributes</code> within the list of decisions <code>decisions</code> passed to <a>RespondDecisionTaskCompleted</a>.</p>
newtype ScheduleLambdaFunctionDecisionAttributes = ScheduleLambdaFunctionDecisionAttributes 
  { "Id'" :: (FunctionId)
  , "Name'" :: (FunctionName)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined.NullOrUndefined (FunctionInput)
  , "StartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  }
derive instance newtypeScheduleLambdaFunctionDecisionAttributes :: Newtype ScheduleLambdaFunctionDecisionAttributes _
derive instance repGenericScheduleLambdaFunctionDecisionAttributes :: Generic ScheduleLambdaFunctionDecisionAttributes _
instance showScheduleLambdaFunctionDecisionAttributes :: Show ScheduleLambdaFunctionDecisionAttributes where
  show = genericShow
instance decodeScheduleLambdaFunctionDecisionAttributes :: Decode ScheduleLambdaFunctionDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleLambdaFunctionDecisionAttributes :: Encode ScheduleLambdaFunctionDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScheduleLambdaFunctionFailedCause = ScheduleLambdaFunctionFailedCause String
derive instance newtypeScheduleLambdaFunctionFailedCause :: Newtype ScheduleLambdaFunctionFailedCause _
derive instance repGenericScheduleLambdaFunctionFailedCause :: Generic ScheduleLambdaFunctionFailedCause _
instance showScheduleLambdaFunctionFailedCause :: Show ScheduleLambdaFunctionFailedCause where
  show = genericShow
instance decodeScheduleLambdaFunctionFailedCause :: Decode ScheduleLambdaFunctionFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleLambdaFunctionFailedCause :: Encode ScheduleLambdaFunctionFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>ScheduleLambdaFunctionFailed</code> event. It isn't set for other event types.</p>
newtype ScheduleLambdaFunctionFailedEventAttributes = ScheduleLambdaFunctionFailedEventAttributes 
  { "Id'" :: (FunctionId)
  , "Name'" :: (FunctionName)
  , "Cause'" :: (ScheduleLambdaFunctionFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeScheduleLambdaFunctionFailedEventAttributes :: Newtype ScheduleLambdaFunctionFailedEventAttributes _
derive instance repGenericScheduleLambdaFunctionFailedEventAttributes :: Generic ScheduleLambdaFunctionFailedEventAttributes _
instance showScheduleLambdaFunctionFailedEventAttributes :: Show ScheduleLambdaFunctionFailedEventAttributes where
  show = genericShow
instance decodeScheduleLambdaFunctionFailedEventAttributes :: Decode ScheduleLambdaFunctionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleLambdaFunctionFailedEventAttributes :: Encode ScheduleLambdaFunctionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>SignalExternalWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  , "SignalName'" :: (SignalName)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeSignalExternalWorkflowExecutionDecisionAttributes :: Newtype SignalExternalWorkflowExecutionDecisionAttributes _
derive instance repGenericSignalExternalWorkflowExecutionDecisionAttributes :: Generic SignalExternalWorkflowExecutionDecisionAttributes _
instance showSignalExternalWorkflowExecutionDecisionAttributes :: Show SignalExternalWorkflowExecutionDecisionAttributes where
  show = genericShow
instance decodeSignalExternalWorkflowExecutionDecisionAttributes :: Decode SignalExternalWorkflowExecutionDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignalExternalWorkflowExecutionDecisionAttributes :: Encode SignalExternalWorkflowExecutionDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SignalExternalWorkflowExecutionFailedCause = SignalExternalWorkflowExecutionFailedCause String
derive instance newtypeSignalExternalWorkflowExecutionFailedCause :: Newtype SignalExternalWorkflowExecutionFailedCause _
derive instance repGenericSignalExternalWorkflowExecutionFailedCause :: Generic SignalExternalWorkflowExecutionFailedCause _
instance showSignalExternalWorkflowExecutionFailedCause :: Show SignalExternalWorkflowExecutionFailedCause where
  show = genericShow
instance decodeSignalExternalWorkflowExecutionFailedCause :: Decode SignalExternalWorkflowExecutionFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignalExternalWorkflowExecutionFailedCause :: Encode SignalExternalWorkflowExecutionFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>SignalExternalWorkflowExecutionFailed</code> event.</p>
newtype SignalExternalWorkflowExecutionFailedEventAttributes = SignalExternalWorkflowExecutionFailedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  , "Cause'" :: (SignalExternalWorkflowExecutionFailedCause)
  , "InitiatedEventId'" :: (EventId)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeSignalExternalWorkflowExecutionFailedEventAttributes :: Newtype SignalExternalWorkflowExecutionFailedEventAttributes _
derive instance repGenericSignalExternalWorkflowExecutionFailedEventAttributes :: Generic SignalExternalWorkflowExecutionFailedEventAttributes _
instance showSignalExternalWorkflowExecutionFailedEventAttributes :: Show SignalExternalWorkflowExecutionFailedEventAttributes where
  show = genericShow
instance decodeSignalExternalWorkflowExecutionFailedEventAttributes :: Decode SignalExternalWorkflowExecutionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignalExternalWorkflowExecutionFailedEventAttributes :: Encode SignalExternalWorkflowExecutionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>SignalExternalWorkflowExecutionInitiated</code> event.</p>
newtype SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  , "SignalName'" :: (SignalName)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeSignalExternalWorkflowExecutionInitiatedEventAttributes :: Newtype SignalExternalWorkflowExecutionInitiatedEventAttributes _
derive instance repGenericSignalExternalWorkflowExecutionInitiatedEventAttributes :: Generic SignalExternalWorkflowExecutionInitiatedEventAttributes _
instance showSignalExternalWorkflowExecutionInitiatedEventAttributes :: Show SignalExternalWorkflowExecutionInitiatedEventAttributes where
  show = genericShow
instance decodeSignalExternalWorkflowExecutionInitiatedEventAttributes :: Decode SignalExternalWorkflowExecutionInitiatedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignalExternalWorkflowExecutionInitiatedEventAttributes :: Encode SignalExternalWorkflowExecutionInitiatedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SignalName = SignalName String
derive instance newtypeSignalName :: Newtype SignalName _
derive instance repGenericSignalName :: Generic SignalName _
instance showSignalName :: Show SignalName where
  show = genericShow
instance decodeSignalName :: Decode SignalName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignalName :: Encode SignalName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SignalWorkflowExecutionInput = SignalWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  , "SignalName'" :: (SignalName)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeSignalWorkflowExecutionInput :: Newtype SignalWorkflowExecutionInput _
derive instance repGenericSignalWorkflowExecutionInput :: Generic SignalWorkflowExecutionInput _
instance showSignalWorkflowExecutionInput :: Show SignalWorkflowExecutionInput where
  show = genericShow
instance decodeSignalWorkflowExecutionInput :: Decode SignalWorkflowExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignalWorkflowExecutionInput :: Encode SignalWorkflowExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>StartChildWorkflowExecution</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>Constrain the following parameters by using a <code>Condition</code> element with the appropriate keys.</p> <ul> <li> <p> <code>tagList.member.N</code> – The key is "swf:tagList.N" where N is the tag number from 0 to 4, inclusive.</p> </li> <li> <p> <code>taskList</code> – String constraint. The key is <code>swf:taskList.name</code>.</p> </li> <li> <p> <code>workflowType.name</code> – String constraint. The key is <code>swf:workflowType.name</code>.</p> </li> <li> <p> <code>workflowType.version</code> – String constraint. The key is <code>swf:workflowType.version</code>.</p> </li> </ul> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype StartChildWorkflowExecutionDecisionAttributes = StartChildWorkflowExecutionDecisionAttributes 
  { "WorkflowType'" :: (WorkflowType)
  , "WorkflowId'" :: (WorkflowId)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: NullOrUndefined.NullOrUndefined (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: NullOrUndefined.NullOrUndefined (ChildPolicy)
  , "TagList'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "LambdaRole'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeStartChildWorkflowExecutionDecisionAttributes :: Newtype StartChildWorkflowExecutionDecisionAttributes _
derive instance repGenericStartChildWorkflowExecutionDecisionAttributes :: Generic StartChildWorkflowExecutionDecisionAttributes _
instance showStartChildWorkflowExecutionDecisionAttributes :: Show StartChildWorkflowExecutionDecisionAttributes where
  show = genericShow
instance decodeStartChildWorkflowExecutionDecisionAttributes :: Decode StartChildWorkflowExecutionDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartChildWorkflowExecutionDecisionAttributes :: Encode StartChildWorkflowExecutionDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartChildWorkflowExecutionFailedCause = StartChildWorkflowExecutionFailedCause String
derive instance newtypeStartChildWorkflowExecutionFailedCause :: Newtype StartChildWorkflowExecutionFailedCause _
derive instance repGenericStartChildWorkflowExecutionFailedCause :: Generic StartChildWorkflowExecutionFailedCause _
instance showStartChildWorkflowExecutionFailedCause :: Show StartChildWorkflowExecutionFailedCause where
  show = genericShow
instance decodeStartChildWorkflowExecutionFailedCause :: Decode StartChildWorkflowExecutionFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartChildWorkflowExecutionFailedCause :: Encode StartChildWorkflowExecutionFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>StartChildWorkflowExecutionFailed</code> event.</p>
newtype StartChildWorkflowExecutionFailedEventAttributes = StartChildWorkflowExecutionFailedEventAttributes 
  { "WorkflowType'" :: (WorkflowType)
  , "Cause'" :: (StartChildWorkflowExecutionFailedCause)
  , "WorkflowId'" :: (WorkflowId)
  , "InitiatedEventId'" :: (EventId)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeStartChildWorkflowExecutionFailedEventAttributes :: Newtype StartChildWorkflowExecutionFailedEventAttributes _
derive instance repGenericStartChildWorkflowExecutionFailedEventAttributes :: Generic StartChildWorkflowExecutionFailedEventAttributes _
instance showStartChildWorkflowExecutionFailedEventAttributes :: Show StartChildWorkflowExecutionFailedEventAttributes where
  show = genericShow
instance decodeStartChildWorkflowExecutionFailedEventAttributes :: Decode StartChildWorkflowExecutionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartChildWorkflowExecutionFailedEventAttributes :: Encode StartChildWorkflowExecutionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>StartChildWorkflowExecutionInitiated</code> event.</p>
newtype StartChildWorkflowExecutionInitiatedEventAttributes = StartChildWorkflowExecutionInitiatedEventAttributes 
  { "WorkflowId'" :: (WorkflowId)
  , "WorkflowType'" :: (WorkflowType)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "ChildPolicy'" :: (ChildPolicy)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "TagList'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "LambdaRole'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeStartChildWorkflowExecutionInitiatedEventAttributes :: Newtype StartChildWorkflowExecutionInitiatedEventAttributes _
derive instance repGenericStartChildWorkflowExecutionInitiatedEventAttributes :: Generic StartChildWorkflowExecutionInitiatedEventAttributes _
instance showStartChildWorkflowExecutionInitiatedEventAttributes :: Show StartChildWorkflowExecutionInitiatedEventAttributes where
  show = genericShow
instance decodeStartChildWorkflowExecutionInitiatedEventAttributes :: Decode StartChildWorkflowExecutionInitiatedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartChildWorkflowExecutionInitiatedEventAttributes :: Encode StartChildWorkflowExecutionInitiatedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartLambdaFunctionFailedCause = StartLambdaFunctionFailedCause String
derive instance newtypeStartLambdaFunctionFailedCause :: Newtype StartLambdaFunctionFailedCause _
derive instance repGenericStartLambdaFunctionFailedCause :: Generic StartLambdaFunctionFailedCause _
instance showStartLambdaFunctionFailedCause :: Show StartLambdaFunctionFailedCause where
  show = genericShow
instance decodeStartLambdaFunctionFailedCause :: Decode StartLambdaFunctionFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartLambdaFunctionFailedCause :: Encode StartLambdaFunctionFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>StartLambdaFunctionFailed</code> event. It isn't set for other event types.</p>
newtype StartLambdaFunctionFailedEventAttributes = StartLambdaFunctionFailedEventAttributes 
  { "ScheduledEventId'" :: NullOrUndefined.NullOrUndefined (EventId)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (StartLambdaFunctionFailedCause)
  , "Message'" :: NullOrUndefined.NullOrUndefined (CauseMessage)
  }
derive instance newtypeStartLambdaFunctionFailedEventAttributes :: Newtype StartLambdaFunctionFailedEventAttributes _
derive instance repGenericStartLambdaFunctionFailedEventAttributes :: Generic StartLambdaFunctionFailedEventAttributes _
instance showStartLambdaFunctionFailedEventAttributes :: Show StartLambdaFunctionFailedEventAttributes where
  show = genericShow
instance decodeStartLambdaFunctionFailedEventAttributes :: Decode StartLambdaFunctionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartLambdaFunctionFailedEventAttributes :: Encode StartLambdaFunctionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>StartTimer</code> decision.</p> <p> <b>Access Control</b> </p> <p>You can use IAM policies to control this decision's access to Amazon SWF resources as follows:</p> <ul> <li> <p>Use a <code>Resource</code> element with the domain name to limit the action to only specified domains.</p> </li> <li> <p>Use an <code>Action</code> element to allow or deny permission to call this action.</p> </li> <li> <p>You cannot use an IAM policy to constrain this action's parameters.</p> </li> </ul> <p>If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's <code>cause</code> parameter is set to <code>OPERATION_NOT_PERMITTED</code>. For details and example IAM policies, see <a href="http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html">Using IAM to Manage Access to Amazon SWF Workflows</a> in the <i>Amazon SWF Developer Guide</i>.</p>
newtype StartTimerDecisionAttributes = StartTimerDecisionAttributes 
  { "TimerId'" :: (TimerId)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  , "StartToFireTimeout'" :: (DurationInSeconds)
  }
derive instance newtypeStartTimerDecisionAttributes :: Newtype StartTimerDecisionAttributes _
derive instance repGenericStartTimerDecisionAttributes :: Generic StartTimerDecisionAttributes _
instance showStartTimerDecisionAttributes :: Show StartTimerDecisionAttributes where
  show = genericShow
instance decodeStartTimerDecisionAttributes :: Decode StartTimerDecisionAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartTimerDecisionAttributes :: Encode StartTimerDecisionAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartTimerFailedCause = StartTimerFailedCause String
derive instance newtypeStartTimerFailedCause :: Newtype StartTimerFailedCause _
derive instance repGenericStartTimerFailedCause :: Generic StartTimerFailedCause _
instance showStartTimerFailedCause :: Show StartTimerFailedCause where
  show = genericShow
instance decodeStartTimerFailedCause :: Decode StartTimerFailedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartTimerFailedCause :: Encode StartTimerFailedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>StartTimerFailed</code> event.</p>
newtype StartTimerFailedEventAttributes = StartTimerFailedEventAttributes 
  { "TimerId'" :: (TimerId)
  , "Cause'" :: (StartTimerFailedCause)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeStartTimerFailedEventAttributes :: Newtype StartTimerFailedEventAttributes _
derive instance repGenericStartTimerFailedEventAttributes :: Generic StartTimerFailedEventAttributes _
instance showStartTimerFailedEventAttributes :: Show StartTimerFailedEventAttributes where
  show = genericShow
instance decodeStartTimerFailedEventAttributes :: Decode StartTimerFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartTimerFailedEventAttributes :: Encode StartTimerFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartWorkflowExecutionInput = StartWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "WorkflowId'" :: (WorkflowId)
  , "WorkflowType'" :: (WorkflowType)
  , "TaskList'" :: NullOrUndefined.NullOrUndefined (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "TagList'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: NullOrUndefined.NullOrUndefined (ChildPolicy)
  , "LambdaRole'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeStartWorkflowExecutionInput :: Newtype StartWorkflowExecutionInput _
derive instance repGenericStartWorkflowExecutionInput :: Generic StartWorkflowExecutionInput _
instance showStartWorkflowExecutionInput :: Show StartWorkflowExecutionInput where
  show = genericShow
instance decodeStartWorkflowExecutionInput :: Decode StartWorkflowExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartWorkflowExecutionInput :: Encode StartWorkflowExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tag = Tag String
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Used to filter the workflow executions in visibility APIs based on a tag.</p>
newtype TagFilter = TagFilter 
  { "Tag'" :: (Tag)
  }
derive instance newtypeTagFilter :: Newtype TagFilter _
derive instance repGenericTagFilter :: Generic TagFilter _
instance showTagFilter :: Show TagFilter where
  show = genericShow
instance decodeTagFilter :: Decode TagFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagFilter :: Encode TagFilter where
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


-- | <p>Represents a task list.</p>
newtype TaskList = TaskList 
  { "Name'" :: (Name)
  }
derive instance newtypeTaskList :: Newtype TaskList _
derive instance repGenericTaskList :: Generic TaskList _
instance showTaskList :: Show TaskList where
  show = genericShow
instance decodeTaskList :: Decode TaskList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskList :: Encode TaskList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskPriority = TaskPriority String
derive instance newtypeTaskPriority :: Newtype TaskPriority _
derive instance repGenericTaskPriority :: Generic TaskPriority _
instance showTaskPriority :: Show TaskPriority where
  show = genericShow
instance decodeTaskPriority :: Decode TaskPriority where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskPriority :: Encode TaskPriority where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskToken = TaskToken String
derive instance newtypeTaskToken :: Newtype TaskToken _
derive instance repGenericTaskToken :: Generic TaskToken _
instance showTaskToken :: Show TaskToken where
  show = genericShow
instance decodeTaskToken :: Decode TaskToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskToken :: Encode TaskToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TerminateReason = TerminateReason String
derive instance newtypeTerminateReason :: Newtype TerminateReason _
derive instance repGenericTerminateReason :: Generic TerminateReason _
instance showTerminateReason :: Show TerminateReason where
  show = genericShow
instance decodeTerminateReason :: Decode TerminateReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateReason :: Encode TerminateReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TerminateWorkflowExecutionInput = TerminateWorkflowExecutionInput 
  { "Domain'" :: (DomainName)
  , "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (TerminateReason)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ChildPolicy'" :: NullOrUndefined.NullOrUndefined (ChildPolicy)
  }
derive instance newtypeTerminateWorkflowExecutionInput :: Newtype TerminateWorkflowExecutionInput _
derive instance repGenericTerminateWorkflowExecutionInput :: Generic TerminateWorkflowExecutionInput _
instance showTerminateWorkflowExecutionInput :: Show TerminateWorkflowExecutionInput where
  show = genericShow
instance decodeTerminateWorkflowExecutionInput :: Decode TerminateWorkflowExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateWorkflowExecutionInput :: Encode TerminateWorkflowExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Provides the details of the <code>TimerCanceled</code> event. </p>
newtype TimerCanceledEventAttributes = TimerCanceledEventAttributes 
  { "TimerId'" :: (TimerId)
  , "StartedEventId'" :: (EventId)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeTimerCanceledEventAttributes :: Newtype TimerCanceledEventAttributes _
derive instance repGenericTimerCanceledEventAttributes :: Generic TimerCanceledEventAttributes _
instance showTimerCanceledEventAttributes :: Show TimerCanceledEventAttributes where
  show = genericShow
instance decodeTimerCanceledEventAttributes :: Decode TimerCanceledEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimerCanceledEventAttributes :: Encode TimerCanceledEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>TimerFired</code> event.</p>
newtype TimerFiredEventAttributes = TimerFiredEventAttributes 
  { "TimerId'" :: (TimerId)
  , "StartedEventId'" :: (EventId)
  }
derive instance newtypeTimerFiredEventAttributes :: Newtype TimerFiredEventAttributes _
derive instance repGenericTimerFiredEventAttributes :: Generic TimerFiredEventAttributes _
instance showTimerFiredEventAttributes :: Show TimerFiredEventAttributes where
  show = genericShow
instance decodeTimerFiredEventAttributes :: Decode TimerFiredEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimerFiredEventAttributes :: Encode TimerFiredEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimerId = TimerId String
derive instance newtypeTimerId :: Newtype TimerId _
derive instance repGenericTimerId :: Generic TimerId _
instance showTimerId :: Show TimerId where
  show = genericShow
instance decodeTimerId :: Decode TimerId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimerId :: Encode TimerId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>TimerStarted</code> event.</p>
newtype TimerStartedEventAttributes = TimerStartedEventAttributes 
  { "TimerId'" :: (TimerId)
  , "Control'" :: NullOrUndefined.NullOrUndefined (Data)
  , "StartToFireTimeout'" :: (DurationInSeconds)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeTimerStartedEventAttributes :: Newtype TimerStartedEventAttributes _
derive instance repGenericTimerStartedEventAttributes :: Generic TimerStartedEventAttributes _
instance showTimerStartedEventAttributes :: Show TimerStartedEventAttributes where
  show = genericShow
instance decodeTimerStartedEventAttributes :: Decode TimerStartedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimerStartedEventAttributes :: Encode TimerStartedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Truncated = Truncated Boolean
derive instance newtypeTruncated :: Newtype Truncated _
derive instance repGenericTruncated :: Generic Truncated _
instance showTruncated :: Show Truncated where
  show = genericShow
instance decodeTruncated :: Decode Truncated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTruncated :: Encode Truncated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned if the type already exists in the specified domain. You get this fault even if the existing type is in deprecated status. You can specify another version if the intent is to create a new distinct version of the type.</p>
newtype TypeAlreadyExistsFault = TypeAlreadyExistsFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTypeAlreadyExistsFault :: Newtype TypeAlreadyExistsFault _
derive instance repGenericTypeAlreadyExistsFault :: Generic TypeAlreadyExistsFault _
instance showTypeAlreadyExistsFault :: Show TypeAlreadyExistsFault where
  show = genericShow
instance decodeTypeAlreadyExistsFault :: Decode TypeAlreadyExistsFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypeAlreadyExistsFault :: Encode TypeAlreadyExistsFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned when the specified activity or workflow type was already deprecated.</p>
newtype TypeDeprecatedFault = TypeDeprecatedFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTypeDeprecatedFault :: Newtype TypeDeprecatedFault _
derive instance repGenericTypeDeprecatedFault :: Generic TypeDeprecatedFault _
instance showTypeDeprecatedFault :: Show TypeDeprecatedFault where
  show = genericShow
instance decodeTypeDeprecatedFault :: Decode TypeDeprecatedFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTypeDeprecatedFault :: Encode TypeDeprecatedFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned when the named resource cannot be found with in the scope of this operation (region or domain). This could happen if the named resource was never created or is no longer available for this operation.</p>
newtype UnknownResourceFault = UnknownResourceFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnknownResourceFault :: Newtype UnknownResourceFault _
derive instance repGenericUnknownResourceFault :: Generic UnknownResourceFault _
instance showUnknownResourceFault :: Show UnknownResourceFault where
  show = genericShow
instance decodeUnknownResourceFault :: Decode UnknownResourceFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnknownResourceFault :: Encode UnknownResourceFault where
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


newtype VersionOptional = VersionOptional String
derive instance newtypeVersionOptional :: Newtype VersionOptional _
derive instance repGenericVersionOptional :: Generic VersionOptional _
instance showVersionOptional :: Show VersionOptional where
  show = genericShow
instance decodeVersionOptional :: Decode VersionOptional where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionOptional :: Encode VersionOptional where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a workflow execution.</p>
newtype WorkflowExecution = WorkflowExecution 
  { "WorkflowId'" :: (WorkflowId)
  , "RunId'" :: (WorkflowRunId)
  }
derive instance newtypeWorkflowExecution :: Newtype WorkflowExecution _
derive instance repGenericWorkflowExecution :: Generic WorkflowExecution _
instance showWorkflowExecution :: Show WorkflowExecution where
  show = genericShow
instance decodeWorkflowExecution :: Decode WorkflowExecution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecution :: Encode WorkflowExecution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returned by <a>StartWorkflowExecution</a> when an open execution with the same workflowId is already running in the specified domain.</p>
newtype WorkflowExecutionAlreadyStartedFault = WorkflowExecutionAlreadyStartedFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeWorkflowExecutionAlreadyStartedFault :: Newtype WorkflowExecutionAlreadyStartedFault _
derive instance repGenericWorkflowExecutionAlreadyStartedFault :: Generic WorkflowExecutionAlreadyStartedFault _
instance showWorkflowExecutionAlreadyStartedFault :: Show WorkflowExecutionAlreadyStartedFault where
  show = genericShow
instance decodeWorkflowExecutionAlreadyStartedFault :: Decode WorkflowExecutionAlreadyStartedFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionAlreadyStartedFault :: Encode WorkflowExecutionAlreadyStartedFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkflowExecutionCancelRequestedCause = WorkflowExecutionCancelRequestedCause String
derive instance newtypeWorkflowExecutionCancelRequestedCause :: Newtype WorkflowExecutionCancelRequestedCause _
derive instance repGenericWorkflowExecutionCancelRequestedCause :: Generic WorkflowExecutionCancelRequestedCause _
instance showWorkflowExecutionCancelRequestedCause :: Show WorkflowExecutionCancelRequestedCause where
  show = genericShow
instance decodeWorkflowExecutionCancelRequestedCause :: Decode WorkflowExecutionCancelRequestedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionCancelRequestedCause :: Encode WorkflowExecutionCancelRequestedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>WorkflowExecutionCancelRequested</code> event.</p>
newtype WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes 
  { "ExternalWorkflowExecution'" :: NullOrUndefined.NullOrUndefined (WorkflowExecution)
  , "ExternalInitiatedEventId'" :: NullOrUndefined.NullOrUndefined (EventId)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionCancelRequestedCause)
  }
derive instance newtypeWorkflowExecutionCancelRequestedEventAttributes :: Newtype WorkflowExecutionCancelRequestedEventAttributes _
derive instance repGenericWorkflowExecutionCancelRequestedEventAttributes :: Generic WorkflowExecutionCancelRequestedEventAttributes _
instance showWorkflowExecutionCancelRequestedEventAttributes :: Show WorkflowExecutionCancelRequestedEventAttributes where
  show = genericShow
instance decodeWorkflowExecutionCancelRequestedEventAttributes :: Decode WorkflowExecutionCancelRequestedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionCancelRequestedEventAttributes :: Encode WorkflowExecutionCancelRequestedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>WorkflowExecutionCanceled</code> event.</p>
newtype WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes 
  { "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeWorkflowExecutionCanceledEventAttributes :: Newtype WorkflowExecutionCanceledEventAttributes _
derive instance repGenericWorkflowExecutionCanceledEventAttributes :: Generic WorkflowExecutionCanceledEventAttributes _
instance showWorkflowExecutionCanceledEventAttributes :: Show WorkflowExecutionCanceledEventAttributes where
  show = genericShow
instance decodeWorkflowExecutionCanceledEventAttributes :: Decode WorkflowExecutionCanceledEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionCanceledEventAttributes :: Encode WorkflowExecutionCanceledEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>WorkflowExecutionCompleted</code> event.</p>
newtype WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes 
  { "Result'" :: NullOrUndefined.NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeWorkflowExecutionCompletedEventAttributes :: Newtype WorkflowExecutionCompletedEventAttributes _
derive instance repGenericWorkflowExecutionCompletedEventAttributes :: Generic WorkflowExecutionCompletedEventAttributes _
instance showWorkflowExecutionCompletedEventAttributes :: Show WorkflowExecutionCompletedEventAttributes where
  show = genericShow
instance decodeWorkflowExecutionCompletedEventAttributes :: Decode WorkflowExecutionCompletedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionCompletedEventAttributes :: Encode WorkflowExecutionCompletedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configuration settings for a workflow execution including timeout values, tasklist etc. These configuration settings are determined from the defaults specified when registering the workflow type and those specified when starting the workflow execution.</p>
newtype WorkflowExecutionConfiguration = WorkflowExecutionConfiguration 
  { "TaskStartToCloseTimeout'" :: (DurationInSeconds)
  , "ExecutionStartToCloseTimeout'" :: (DurationInSeconds)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "ChildPolicy'" :: (ChildPolicy)
  , "LambdaRole'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeWorkflowExecutionConfiguration :: Newtype WorkflowExecutionConfiguration _
derive instance repGenericWorkflowExecutionConfiguration :: Generic WorkflowExecutionConfiguration _
instance showWorkflowExecutionConfiguration :: Show WorkflowExecutionConfiguration where
  show = genericShow
instance decodeWorkflowExecutionConfiguration :: Decode WorkflowExecutionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionConfiguration :: Encode WorkflowExecutionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>WorkflowExecutionContinuedAsNew</code> event.</p>
newtype WorkflowExecutionContinuedAsNewEventAttributes = WorkflowExecutionContinuedAsNewEventAttributes 
  { "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  , "NewExecutionRunId'" :: (WorkflowRunId)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: (ChildPolicy)
  , "TagList'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "WorkflowType'" :: (WorkflowType)
  , "LambdaRole'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeWorkflowExecutionContinuedAsNewEventAttributes :: Newtype WorkflowExecutionContinuedAsNewEventAttributes _
derive instance repGenericWorkflowExecutionContinuedAsNewEventAttributes :: Generic WorkflowExecutionContinuedAsNewEventAttributes _
instance showWorkflowExecutionContinuedAsNewEventAttributes :: Show WorkflowExecutionContinuedAsNewEventAttributes where
  show = genericShow
instance decodeWorkflowExecutionContinuedAsNewEventAttributes :: Decode WorkflowExecutionContinuedAsNewEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionContinuedAsNewEventAttributes :: Encode WorkflowExecutionContinuedAsNewEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the count of workflow executions returned from <a>CountOpenWorkflowExecutions</a> or <a>CountClosedWorkflowExecutions</a> </p>
newtype WorkflowExecutionCount = WorkflowExecutionCount 
  { "Count'" :: (Count)
  , "Truncated'" :: NullOrUndefined.NullOrUndefined (Truncated)
  }
derive instance newtypeWorkflowExecutionCount :: Newtype WorkflowExecutionCount _
derive instance repGenericWorkflowExecutionCount :: Generic WorkflowExecutionCount _
instance showWorkflowExecutionCount :: Show WorkflowExecutionCount where
  show = genericShow
instance decodeWorkflowExecutionCount :: Decode WorkflowExecutionCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionCount :: Encode WorkflowExecutionCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about a workflow execution.</p>
newtype WorkflowExecutionDetail = WorkflowExecutionDetail 
  { "ExecutionInfo'" :: (WorkflowExecutionInfo)
  , "ExecutionConfiguration'" :: (WorkflowExecutionConfiguration)
  , "OpenCounts'" :: (WorkflowExecutionOpenCounts)
  , "LatestActivityTaskTimestamp'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LatestExecutionContext'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeWorkflowExecutionDetail :: Newtype WorkflowExecutionDetail _
derive instance repGenericWorkflowExecutionDetail :: Generic WorkflowExecutionDetail _
instance showWorkflowExecutionDetail :: Show WorkflowExecutionDetail where
  show = genericShow
instance decodeWorkflowExecutionDetail :: Decode WorkflowExecutionDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionDetail :: Encode WorkflowExecutionDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>WorkflowExecutionFailed</code> event.</p>
newtype WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes 
  { "Reason'" :: NullOrUndefined.NullOrUndefined (FailureReason)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  , "DecisionTaskCompletedEventId'" :: (EventId)
  }
derive instance newtypeWorkflowExecutionFailedEventAttributes :: Newtype WorkflowExecutionFailedEventAttributes _
derive instance repGenericWorkflowExecutionFailedEventAttributes :: Generic WorkflowExecutionFailedEventAttributes _
instance showWorkflowExecutionFailedEventAttributes :: Show WorkflowExecutionFailedEventAttributes where
  show = genericShow
instance decodeWorkflowExecutionFailedEventAttributes :: Decode WorkflowExecutionFailedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionFailedEventAttributes :: Encode WorkflowExecutionFailedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Used to filter the workflow executions in visibility APIs by their <code>workflowId</code>.</p>
newtype WorkflowExecutionFilter = WorkflowExecutionFilter 
  { "WorkflowId'" :: (WorkflowId)
  }
derive instance newtypeWorkflowExecutionFilter :: Newtype WorkflowExecutionFilter _
derive instance repGenericWorkflowExecutionFilter :: Generic WorkflowExecutionFilter _
instance showWorkflowExecutionFilter :: Show WorkflowExecutionFilter where
  show = genericShow
instance decodeWorkflowExecutionFilter :: Decode WorkflowExecutionFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionFilter :: Encode WorkflowExecutionFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a workflow execution.</p>
newtype WorkflowExecutionInfo = WorkflowExecutionInfo 
  { "Execution'" :: (WorkflowExecution)
  , "WorkflowType'" :: (WorkflowType)
  , "StartTimestamp'" :: (Number)
  , "CloseTimestamp'" :: NullOrUndefined.NullOrUndefined (Number)
  , "ExecutionStatus'" :: (ExecutionStatus)
  , "CloseStatus'" :: NullOrUndefined.NullOrUndefined (CloseStatus)
  , "Parent'" :: NullOrUndefined.NullOrUndefined (WorkflowExecution)
  , "TagList'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "CancelRequested'" :: NullOrUndefined.NullOrUndefined (Canceled)
  }
derive instance newtypeWorkflowExecutionInfo :: Newtype WorkflowExecutionInfo _
derive instance repGenericWorkflowExecutionInfo :: Generic WorkflowExecutionInfo _
instance showWorkflowExecutionInfo :: Show WorkflowExecutionInfo where
  show = genericShow
instance decodeWorkflowExecutionInfo :: Decode WorkflowExecutionInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionInfo :: Encode WorkflowExecutionInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkflowExecutionInfoList = WorkflowExecutionInfoList (Array WorkflowExecutionInfo)
derive instance newtypeWorkflowExecutionInfoList :: Newtype WorkflowExecutionInfoList _
derive instance repGenericWorkflowExecutionInfoList :: Generic WorkflowExecutionInfoList _
instance showWorkflowExecutionInfoList :: Show WorkflowExecutionInfoList where
  show = genericShow
instance decodeWorkflowExecutionInfoList :: Decode WorkflowExecutionInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionInfoList :: Encode WorkflowExecutionInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a paginated list of information about workflow executions.</p>
newtype WorkflowExecutionInfos = WorkflowExecutionInfos 
  { "ExecutionInfos'" :: (WorkflowExecutionInfoList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeWorkflowExecutionInfos :: Newtype WorkflowExecutionInfos _
derive instance repGenericWorkflowExecutionInfos :: Generic WorkflowExecutionInfos _
instance showWorkflowExecutionInfos :: Show WorkflowExecutionInfos where
  show = genericShow
instance decodeWorkflowExecutionInfos :: Decode WorkflowExecutionInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionInfos :: Encode WorkflowExecutionInfos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the counts of open tasks, child workflow executions and timers for a workflow execution.</p>
newtype WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts 
  { "OpenActivityTasks'" :: (Count)
  , "OpenDecisionTasks'" :: (OpenDecisionTasksCount)
  , "OpenTimers'" :: (Count)
  , "OpenChildWorkflowExecutions'" :: (Count)
  , "OpenLambdaFunctions'" :: NullOrUndefined.NullOrUndefined (Count)
  }
derive instance newtypeWorkflowExecutionOpenCounts :: Newtype WorkflowExecutionOpenCounts _
derive instance repGenericWorkflowExecutionOpenCounts :: Generic WorkflowExecutionOpenCounts _
instance showWorkflowExecutionOpenCounts :: Show WorkflowExecutionOpenCounts where
  show = genericShow
instance decodeWorkflowExecutionOpenCounts :: Decode WorkflowExecutionOpenCounts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionOpenCounts :: Encode WorkflowExecutionOpenCounts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>WorkflowExecutionSignaled</code> event.</p>
newtype WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes 
  { "SignalName'" :: (SignalName)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ExternalWorkflowExecution'" :: NullOrUndefined.NullOrUndefined (WorkflowExecution)
  , "ExternalInitiatedEventId'" :: NullOrUndefined.NullOrUndefined (EventId)
  }
derive instance newtypeWorkflowExecutionSignaledEventAttributes :: Newtype WorkflowExecutionSignaledEventAttributes _
derive instance repGenericWorkflowExecutionSignaledEventAttributes :: Generic WorkflowExecutionSignaledEventAttributes _
instance showWorkflowExecutionSignaledEventAttributes :: Show WorkflowExecutionSignaledEventAttributes where
  show = genericShow
instance decodeWorkflowExecutionSignaledEventAttributes :: Decode WorkflowExecutionSignaledEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionSignaledEventAttributes :: Encode WorkflowExecutionSignaledEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides details of <code>WorkflowExecutionStarted</code> event.</p>
newtype WorkflowExecutionStartedEventAttributes = WorkflowExecutionStartedEventAttributes 
  { "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ExecutionStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "TaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "ChildPolicy'" :: (ChildPolicy)
  , "TaskList'" :: (TaskList)
  , "TaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "WorkflowType'" :: (WorkflowType)
  , "TagList'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "ContinuedExecutionRunId'" :: NullOrUndefined.NullOrUndefined (WorkflowRunIdOptional)
  , "ParentWorkflowExecution'" :: NullOrUndefined.NullOrUndefined (WorkflowExecution)
  , "ParentInitiatedEventId'" :: NullOrUndefined.NullOrUndefined (EventId)
  , "LambdaRole'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeWorkflowExecutionStartedEventAttributes :: Newtype WorkflowExecutionStartedEventAttributes _
derive instance repGenericWorkflowExecutionStartedEventAttributes :: Generic WorkflowExecutionStartedEventAttributes _
instance showWorkflowExecutionStartedEventAttributes :: Show WorkflowExecutionStartedEventAttributes where
  show = genericShow
instance decodeWorkflowExecutionStartedEventAttributes :: Decode WorkflowExecutionStartedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionStartedEventAttributes :: Encode WorkflowExecutionStartedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkflowExecutionTerminatedCause = WorkflowExecutionTerminatedCause String
derive instance newtypeWorkflowExecutionTerminatedCause :: Newtype WorkflowExecutionTerminatedCause _
derive instance repGenericWorkflowExecutionTerminatedCause :: Generic WorkflowExecutionTerminatedCause _
instance showWorkflowExecutionTerminatedCause :: Show WorkflowExecutionTerminatedCause where
  show = genericShow
instance decodeWorkflowExecutionTerminatedCause :: Decode WorkflowExecutionTerminatedCause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionTerminatedCause :: Encode WorkflowExecutionTerminatedCause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>WorkflowExecutionTerminated</code> event.</p>
newtype WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes 
  { "Reason'" :: NullOrUndefined.NullOrUndefined (TerminateReason)
  , "Details'" :: NullOrUndefined.NullOrUndefined (Data)
  , "ChildPolicy'" :: (ChildPolicy)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (WorkflowExecutionTerminatedCause)
  }
derive instance newtypeWorkflowExecutionTerminatedEventAttributes :: Newtype WorkflowExecutionTerminatedEventAttributes _
derive instance repGenericWorkflowExecutionTerminatedEventAttributes :: Generic WorkflowExecutionTerminatedEventAttributes _
instance showWorkflowExecutionTerminatedEventAttributes :: Show WorkflowExecutionTerminatedEventAttributes where
  show = genericShow
instance decodeWorkflowExecutionTerminatedEventAttributes :: Decode WorkflowExecutionTerminatedEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionTerminatedEventAttributes :: Encode WorkflowExecutionTerminatedEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the details of the <code>WorkflowExecutionTimedOut</code> event.</p>
newtype WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes 
  { "TimeoutType'" :: (WorkflowExecutionTimeoutType)
  , "ChildPolicy'" :: (ChildPolicy)
  }
derive instance newtypeWorkflowExecutionTimedOutEventAttributes :: Newtype WorkflowExecutionTimedOutEventAttributes _
derive instance repGenericWorkflowExecutionTimedOutEventAttributes :: Generic WorkflowExecutionTimedOutEventAttributes _
instance showWorkflowExecutionTimedOutEventAttributes :: Show WorkflowExecutionTimedOutEventAttributes where
  show = genericShow
instance decodeWorkflowExecutionTimedOutEventAttributes :: Decode WorkflowExecutionTimedOutEventAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionTimedOutEventAttributes :: Encode WorkflowExecutionTimedOutEventAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkflowExecutionTimeoutType = WorkflowExecutionTimeoutType String
derive instance newtypeWorkflowExecutionTimeoutType :: Newtype WorkflowExecutionTimeoutType _
derive instance repGenericWorkflowExecutionTimeoutType :: Generic WorkflowExecutionTimeoutType _
instance showWorkflowExecutionTimeoutType :: Show WorkflowExecutionTimeoutType where
  show = genericShow
instance decodeWorkflowExecutionTimeoutType :: Decode WorkflowExecutionTimeoutType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowExecutionTimeoutType :: Encode WorkflowExecutionTimeoutType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkflowId = WorkflowId String
derive instance newtypeWorkflowId :: Newtype WorkflowId _
derive instance repGenericWorkflowId :: Generic WorkflowId _
instance showWorkflowId :: Show WorkflowId where
  show = genericShow
instance decodeWorkflowId :: Decode WorkflowId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowId :: Encode WorkflowId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkflowRunId = WorkflowRunId String
derive instance newtypeWorkflowRunId :: Newtype WorkflowRunId _
derive instance repGenericWorkflowRunId :: Generic WorkflowRunId _
instance showWorkflowRunId :: Show WorkflowRunId where
  show = genericShow
instance decodeWorkflowRunId :: Decode WorkflowRunId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowRunId :: Encode WorkflowRunId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkflowRunIdOptional = WorkflowRunIdOptional String
derive instance newtypeWorkflowRunIdOptional :: Newtype WorkflowRunIdOptional _
derive instance repGenericWorkflowRunIdOptional :: Generic WorkflowRunIdOptional _
instance showWorkflowRunIdOptional :: Show WorkflowRunIdOptional where
  show = genericShow
instance decodeWorkflowRunIdOptional :: Decode WorkflowRunIdOptional where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowRunIdOptional :: Encode WorkflowRunIdOptional where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a workflow type.</p>
newtype WorkflowType = WorkflowType 
  { "Name'" :: (Name)
  , "Version'" :: (Version)
  }
derive instance newtypeWorkflowType :: Newtype WorkflowType _
derive instance repGenericWorkflowType :: Generic WorkflowType _
instance showWorkflowType :: Show WorkflowType where
  show = genericShow
instance decodeWorkflowType :: Decode WorkflowType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowType :: Encode WorkflowType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configuration settings of a workflow type.</p>
newtype WorkflowTypeConfiguration = WorkflowTypeConfiguration 
  { "DefaultTaskStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultExecutionStartToCloseTimeout'" :: NullOrUndefined.NullOrUndefined (DurationInSecondsOptional)
  , "DefaultTaskList'" :: NullOrUndefined.NullOrUndefined (TaskList)
  , "DefaultTaskPriority'" :: NullOrUndefined.NullOrUndefined (TaskPriority)
  , "DefaultChildPolicy'" :: NullOrUndefined.NullOrUndefined (ChildPolicy)
  , "DefaultLambdaRole'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeWorkflowTypeConfiguration :: Newtype WorkflowTypeConfiguration _
derive instance repGenericWorkflowTypeConfiguration :: Generic WorkflowTypeConfiguration _
instance showWorkflowTypeConfiguration :: Show WorkflowTypeConfiguration where
  show = genericShow
instance decodeWorkflowTypeConfiguration :: Decode WorkflowTypeConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowTypeConfiguration :: Encode WorkflowTypeConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about a workflow type.</p>
newtype WorkflowTypeDetail = WorkflowTypeDetail 
  { "TypeInfo'" :: (WorkflowTypeInfo)
  , "Configuration'" :: (WorkflowTypeConfiguration)
  }
derive instance newtypeWorkflowTypeDetail :: Newtype WorkflowTypeDetail _
derive instance repGenericWorkflowTypeDetail :: Generic WorkflowTypeDetail _
instance showWorkflowTypeDetail :: Show WorkflowTypeDetail where
  show = genericShow
instance decodeWorkflowTypeDetail :: Decode WorkflowTypeDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowTypeDetail :: Encode WorkflowTypeDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Used to filter workflow execution query results by type. Each parameter, if specified, defines a rule that must be satisfied by each returned result.</p>
newtype WorkflowTypeFilter = WorkflowTypeFilter 
  { "Name'" :: (Name)
  , "Version'" :: NullOrUndefined.NullOrUndefined (VersionOptional)
  }
derive instance newtypeWorkflowTypeFilter :: Newtype WorkflowTypeFilter _
derive instance repGenericWorkflowTypeFilter :: Generic WorkflowTypeFilter _
instance showWorkflowTypeFilter :: Show WorkflowTypeFilter where
  show = genericShow
instance decodeWorkflowTypeFilter :: Decode WorkflowTypeFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowTypeFilter :: Encode WorkflowTypeFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a workflow type.</p>
newtype WorkflowTypeInfo = WorkflowTypeInfo 
  { "WorkflowType'" :: (WorkflowType)
  , "Status'" :: (RegistrationStatus)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "CreationDate'" :: (Number)
  , "DeprecationDate'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeWorkflowTypeInfo :: Newtype WorkflowTypeInfo _
derive instance repGenericWorkflowTypeInfo :: Generic WorkflowTypeInfo _
instance showWorkflowTypeInfo :: Show WorkflowTypeInfo where
  show = genericShow
instance decodeWorkflowTypeInfo :: Decode WorkflowTypeInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowTypeInfo :: Encode WorkflowTypeInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkflowTypeInfoList = WorkflowTypeInfoList (Array WorkflowTypeInfo)
derive instance newtypeWorkflowTypeInfoList :: Newtype WorkflowTypeInfoList _
derive instance repGenericWorkflowTypeInfoList :: Generic WorkflowTypeInfoList _
instance showWorkflowTypeInfoList :: Show WorkflowTypeInfoList where
  show = genericShow
instance decodeWorkflowTypeInfoList :: Decode WorkflowTypeInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowTypeInfoList :: Encode WorkflowTypeInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a paginated list of information structures about workflow types.</p>
newtype WorkflowTypeInfos = WorkflowTypeInfos 
  { "TypeInfos'" :: (WorkflowTypeInfoList)
  , "NextPageToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeWorkflowTypeInfos :: Newtype WorkflowTypeInfos _
derive instance repGenericWorkflowTypeInfos :: Generic WorkflowTypeInfos _
instance showWorkflowTypeInfos :: Show WorkflowTypeInfos where
  show = genericShow
instance decodeWorkflowTypeInfos :: Decode WorkflowTypeInfos where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkflowTypeInfos :: Encode WorkflowTypeInfos where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
