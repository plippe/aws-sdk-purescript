

-- | <p>Amazon EMR is a web service that makes it easy to process large amounts of data efficiently. Amazon EMR uses Hadoop processing combined with several AWS products to do tasks such as web indexing, data mining, log file analysis, machine learning, scientific simulation, and data warehousing.</p>
module AWS.EMR where

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

serviceName = "EMR" :: String


-- | <p>Adds an instance fleet to a running cluster.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x.</p> </note>
addInstanceFleet :: forall eff. AddInstanceFleetInput -> Aff (exception :: EXCEPTION | eff) AddInstanceFleetOutput
addInstanceFleet = Request.request serviceName "addInstanceFleet" 


-- | <p>Adds one or more instance groups to a running cluster.</p>
addInstanceGroups :: forall eff. AddInstanceGroupsInput -> Aff (exception :: EXCEPTION | eff) AddInstanceGroupsOutput
addInstanceGroups = Request.request serviceName "addInstanceGroups" 


-- | <p>AddJobFlowSteps adds new steps to a running cluster. A maximum of 256 steps are allowed in each job flow.</p> <p>If your cluster is long-running (such as a Hive data warehouse) or complex, you may require more than 256 steps to process your data. You can bypass the 256-step limitation in various ways, including using SSH to connect to the master node and submitting queries directly to the software running on the master node, such as Hive and Hadoop. For more information on how to do this, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html">Add More than 256 Steps to a Cluster</a> in the <i>Amazon EMR Management Guide</i>.</p> <p>A step specifies the location of a JAR file stored either on the master node of the cluster or in Amazon S3. Each step is performed by the main function of the main class of the JAR file. The main class can be specified either in the manifest of the JAR or by using the MainFunction parameter of the step.</p> <p>Amazon EMR executes each step in the order listed. For a step to be considered complete, the main function must exit with a zero exit code and all Hadoop jobs started while the step was running must have completed and run successfully.</p> <p>You can only add steps to a cluster that is in one of the following states: STARTING, BOOTSTRAPPING, RUNNING, or WAITING.</p>
addJobFlowSteps :: forall eff. AddJobFlowStepsInput -> Aff (exception :: EXCEPTION | eff) AddJobFlowStepsOutput
addJobFlowSteps = Request.request serviceName "addJobFlowSteps" 


-- | <p>Adds tags to an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html">Tag Clusters</a>. </p>
addTags :: forall eff. AddTagsInput -> Aff (exception :: EXCEPTION | eff) AddTagsOutput
addTags = Request.request serviceName "addTags" 


-- | <p>Cancels a pending step or steps in a running cluster. Available only in Amazon EMR versions 4.8.0 and later, excluding version 5.0.0. A maximum of 256 steps are allowed in each CancelSteps request. CancelSteps is idempotent but asynchronous; it does not guarantee a step will be canceled, even if the request is successfully submitted. You can only cancel steps that are in a <code>PENDING</code> state.</p>
cancelSteps :: forall eff. CancelStepsInput -> Aff (exception :: EXCEPTION | eff) CancelStepsOutput
cancelSteps = Request.request serviceName "cancelSteps" 


-- | <p>Creates a security configuration, which is stored in the service and can be specified when a cluster is created.</p>
createSecurityConfiguration :: forall eff. CreateSecurityConfigurationInput -> Aff (exception :: EXCEPTION | eff) CreateSecurityConfigurationOutput
createSecurityConfiguration = Request.request serviceName "createSecurityConfiguration" 


-- | <p>Deletes a security configuration.</p>
deleteSecurityConfiguration :: forall eff. DeleteSecurityConfigurationInput -> Aff (exception :: EXCEPTION | eff) DeleteSecurityConfigurationOutput
deleteSecurityConfiguration = Request.request serviceName "deleteSecurityConfiguration" 


-- | <p>Provides cluster-level details including status, hardware and software configuration, VPC settings, and so on. For information about the cluster steps, see <a>ListSteps</a>.</p>
describeCluster :: forall eff. DescribeClusterInput -> Aff (exception :: EXCEPTION | eff) DescribeClusterOutput
describeCluster = Request.request serviceName "describeCluster" 


-- | <p>This API is deprecated and will eventually be removed. We recommend you use <a>ListClusters</a>, <a>DescribeCluster</a>, <a>ListSteps</a>, <a>ListInstanceGroups</a> and <a>ListBootstrapActions</a> instead.</p> <p>DescribeJobFlows returns a list of job flows that match all of the supplied parameters. The parameters can include a list of job flow IDs, job flow states, and restrictions on job flow creation date and time.</p> <p>Regardless of supplied parameters, only job flows created within the last two months are returned.</p> <p>If no parameters are supplied, then job flows matching either of the following criteria are returned:</p> <ul> <li> <p>Job flows created and completed in the last two weeks</p> </li> <li> <p> Job flows created within the last two months that are in one of the following states: <code>RUNNING</code>, <code>WAITING</code>, <code>SHUTTING_DOWN</code>, <code>STARTING</code> </p> </li> </ul> <p>Amazon EMR can return a maximum of 512 job flow descriptions.</p>
describeJobFlows :: forall eff. DescribeJobFlowsInput -> Aff (exception :: EXCEPTION | eff) DescribeJobFlowsOutput
describeJobFlows = Request.request serviceName "describeJobFlows" 


-- | <p>Provides the details of a security configuration by returning the configuration JSON.</p>
describeSecurityConfiguration :: forall eff. DescribeSecurityConfigurationInput -> Aff (exception :: EXCEPTION | eff) DescribeSecurityConfigurationOutput
describeSecurityConfiguration = Request.request serviceName "describeSecurityConfiguration" 


-- | <p>Provides more detail about the cluster step.</p>
describeStep :: forall eff. DescribeStepInput -> Aff (exception :: EXCEPTION | eff) DescribeStepOutput
describeStep = Request.request serviceName "describeStep" 


-- | <p>Provides information about the bootstrap actions associated with a cluster.</p>
listBootstrapActions :: forall eff. ListBootstrapActionsInput -> Aff (exception :: EXCEPTION | eff) ListBootstrapActionsOutput
listBootstrapActions = Request.request serviceName "listBootstrapActions" 


-- | <p>Provides the status of all clusters visible to this AWS account. Allows you to filter the list of clusters based on certain criteria; for example, filtering by cluster creation date and time or by status. This call returns a maximum of 50 clusters per call, but returns a marker to track the paging of the cluster list across multiple ListClusters calls.</p>
listClusters :: forall eff. ListClustersInput -> Aff (exception :: EXCEPTION | eff) ListClustersOutput
listClusters = Request.request serviceName "listClusters" 


-- | <p>Lists all available details about the instance fleets in a cluster.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
listInstanceFleets :: forall eff. ListInstanceFleetsInput -> Aff (exception :: EXCEPTION | eff) ListInstanceFleetsOutput
listInstanceFleets = Request.request serviceName "listInstanceFleets" 


-- | <p>Provides all available details about the instance groups in a cluster.</p>
listInstanceGroups :: forall eff. ListInstanceGroupsInput -> Aff (exception :: EXCEPTION | eff) ListInstanceGroupsOutput
listInstanceGroups = Request.request serviceName "listInstanceGroups" 


-- | <p>Provides information for all active EC2 instances and EC2 instances terminated in the last 30 days, up to a maximum of 2,000. EC2 instances in any of the following states are considered active: AWAITING_FULFILLMENT, PROVISIONING, BOOTSTRAPPING, RUNNING.</p>
listInstances :: forall eff. ListInstancesInput -> Aff (exception :: EXCEPTION | eff) ListInstancesOutput
listInstances = Request.request serviceName "listInstances" 


-- | <p>Lists all the security configurations visible to this account, providing their creation dates and times, and their names. This call returns a maximum of 50 clusters per call, but returns a marker to track the paging of the cluster list across multiple ListSecurityConfigurations calls.</p>
listSecurityConfigurations :: forall eff. ListSecurityConfigurationsInput -> Aff (exception :: EXCEPTION | eff) ListSecurityConfigurationsOutput
listSecurityConfigurations = Request.request serviceName "listSecurityConfigurations" 


-- | <p>Provides a list of steps for the cluster in reverse order unless you specify stepIds with the request.</p>
listSteps :: forall eff. ListStepsInput -> Aff (exception :: EXCEPTION | eff) ListStepsOutput
listSteps = Request.request serviceName "listSteps" 


-- | <p>Modifies the target On-Demand and target Spot capacities for the instance fleet with the specified InstanceFleetID within the cluster specified using ClusterID. The call either succeeds or fails atomically.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
modifyInstanceFleet :: forall eff. ModifyInstanceFleetInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
modifyInstanceFleet = Request.request serviceName "modifyInstanceFleet" 


-- | <p>ModifyInstanceGroups modifies the number of nodes and configuration settings of an instance group. The input parameters include the new target instance count for the group and the instance group ID. The call will either succeed or fail atomically.</p>
modifyInstanceGroups :: forall eff. ModifyInstanceGroupsInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
modifyInstanceGroups = Request.request serviceName "modifyInstanceGroups" 


-- | <p>Creates or updates an automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric.</p>
putAutoScalingPolicy :: forall eff. PutAutoScalingPolicyInput -> Aff (exception :: EXCEPTION | eff) PutAutoScalingPolicyOutput
putAutoScalingPolicy = Request.request serviceName "putAutoScalingPolicy" 


-- | <p>Removes an automatic scaling policy from a specified instance group within an EMR cluster.</p>
removeAutoScalingPolicy :: forall eff. RemoveAutoScalingPolicyInput -> Aff (exception :: EXCEPTION | eff) RemoveAutoScalingPolicyOutput
removeAutoScalingPolicy = Request.request serviceName "removeAutoScalingPolicy" 


-- | <p>Removes tags from an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html">Tag Clusters</a>. </p> <p>The following example removes the stack tag with value Prod from a cluster:</p>
removeTags :: forall eff. RemoveTagsInput -> Aff (exception :: EXCEPTION | eff) RemoveTagsOutput
removeTags = Request.request serviceName "removeTags" 


-- | <p>RunJobFlow creates and starts running a new cluster (job flow). The cluster runs the steps specified. After the steps complete, the cluster stops and the HDFS partition is lost. To prevent loss of data, configure the last step of the job flow to store results in Amazon S3. If the <a>JobFlowInstancesConfig</a> <code>KeepJobFlowAliveWhenNoSteps</code> parameter is set to <code>TRUE</code>, the cluster transitions to the WAITING state rather than shutting down after the steps have completed. </p> <p>For additional protection, you can set the <a>JobFlowInstancesConfig</a> <code>TerminationProtected</code> parameter to <code>TRUE</code> to lock the cluster and prevent it from being terminated by API call, user intervention, or in the event of a job flow error.</p> <p>A maximum of 256 steps are allowed in each job flow.</p> <p>If your cluster is long-running (such as a Hive data warehouse) or complex, you may require more than 256 steps to process your data. You can bypass the 256-step limitation in various ways, including using the SSH shell to connect to the master node and submitting queries directly to the software running on the master node, such as Hive and Hadoop. For more information on how to do this, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html">Add More than 256 Steps to a Cluster</a> in the <i>Amazon EMR Management Guide</i>.</p> <p>For long running clusters, we recommend that you periodically store your results.</p> <note> <p>The instance fleets configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions. The RunJobFlow request can contain InstanceFleets parameters or InstanceGroups parameters, but not both.</p> </note>
runJobFlow :: forall eff. RunJobFlowInput -> Aff (exception :: EXCEPTION | eff) RunJobFlowOutput
runJobFlow = Request.request serviceName "runJobFlow" 


-- | <p>SetTerminationProtection locks a cluster (job flow) so the EC2 instances in the cluster cannot be terminated by user intervention, an API call, or in the event of a job-flow error. The cluster still terminates upon successful completion of the job flow. Calling <code>SetTerminationProtection</code> on a cluster is similar to calling the Amazon EC2 <code>DisableAPITermination</code> API on all EC2 instances in a cluster.</p> <p> <code>SetTerminationProtection</code> is used to prevent accidental termination of a cluster and to ensure that in the event of an error, the instances persist so that you can recover any data stored in their ephemeral instance storage.</p> <p> To terminate a cluster that has been locked by setting <code>SetTerminationProtection</code> to <code>true</code>, you must first unlock the job flow by a subsequent call to <code>SetTerminationProtection</code> in which you set the value to <code>false</code>. </p> <p> For more information, see<a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/UsingEMR_TerminationProtection.html">Managing Cluster Termination</a> in the <i>Amazon EMR Management Guide</i>. </p>
setTerminationProtection :: forall eff. SetTerminationProtectionInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setTerminationProtection = Request.request serviceName "setTerminationProtection" 


-- | <p>Sets whether all AWS Identity and Access Management (IAM) users under your account can access the specified clusters (job flows). This action works on running clusters. You can also set the visibility of a cluster when you launch it using the <code>VisibleToAllUsers</code> parameter of <a>RunJobFlow</a>. The SetVisibleToAllUsers action can be called only by an IAM user who created the cluster or the AWS account that owns the cluster.</p>
setVisibleToAllUsers :: forall eff. SetVisibleToAllUsersInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setVisibleToAllUsers = Request.request serviceName "setVisibleToAllUsers" 


-- | <p>TerminateJobFlows shuts a list of clusters (job flows) down. When a job flow is shut down, any step not yet completed is canceled and the EC2 instances on which the cluster is running are stopped. Any log files not already saved are uploaded to Amazon S3 if a LogUri was specified when the cluster was created.</p> <p>The maximum number of clusters allowed is 10. The call to <code>TerminateJobFlows</code> is asynchronous. Depending on the configuration of the cluster, it may take up to 1-5 minutes for the cluster to completely terminate and release allocated resources, such as Amazon EC2 instances.</p>
terminateJobFlows :: forall eff. TerminateJobFlowsInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
terminateJobFlows = Request.request serviceName "terminateJobFlows" 


newtype ActionOnFailure = ActionOnFailure String
derive instance newtypeActionOnFailure :: Newtype ActionOnFailure _
derive instance repGenericActionOnFailure :: Generic ActionOnFailure _
instance showActionOnFailure :: Show ActionOnFailure where
  show = genericShow
instance decodeActionOnFailure :: Decode ActionOnFailure where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionOnFailure :: Encode ActionOnFailure where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddInstanceFleetInput = AddInstanceFleetInput 
  { "ClusterId" :: (XmlStringMaxLen256)
  , "InstanceFleet" :: (InstanceFleetConfig)
  }
derive instance newtypeAddInstanceFleetInput :: Newtype AddInstanceFleetInput _
derive instance repGenericAddInstanceFleetInput :: Generic AddInstanceFleetInput _
instance showAddInstanceFleetInput :: Show AddInstanceFleetInput where
  show = genericShow
instance decodeAddInstanceFleetInput :: Decode AddInstanceFleetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddInstanceFleetInput :: Encode AddInstanceFleetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddInstanceFleetOutput = AddInstanceFleetOutput 
  { "ClusterId" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "InstanceFleetId" :: NullOrUndefined.NullOrUndefined (InstanceFleetId)
  }
derive instance newtypeAddInstanceFleetOutput :: Newtype AddInstanceFleetOutput _
derive instance repGenericAddInstanceFleetOutput :: Generic AddInstanceFleetOutput _
instance showAddInstanceFleetOutput :: Show AddInstanceFleetOutput where
  show = genericShow
instance decodeAddInstanceFleetOutput :: Decode AddInstanceFleetOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddInstanceFleetOutput :: Encode AddInstanceFleetOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Input to an AddInstanceGroups call.</p>
newtype AddInstanceGroupsInput = AddInstanceGroupsInput 
  { "InstanceGroups" :: (InstanceGroupConfigList)
  , "JobFlowId" :: (XmlStringMaxLen256)
  }
derive instance newtypeAddInstanceGroupsInput :: Newtype AddInstanceGroupsInput _
derive instance repGenericAddInstanceGroupsInput :: Generic AddInstanceGroupsInput _
instance showAddInstanceGroupsInput :: Show AddInstanceGroupsInput where
  show = genericShow
instance decodeAddInstanceGroupsInput :: Decode AddInstanceGroupsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddInstanceGroupsInput :: Encode AddInstanceGroupsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Output from an AddInstanceGroups call.</p>
newtype AddInstanceGroupsOutput = AddInstanceGroupsOutput 
  { "JobFlowId" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "InstanceGroupIds" :: NullOrUndefined.NullOrUndefined (InstanceGroupIdsList)
  }
derive instance newtypeAddInstanceGroupsOutput :: Newtype AddInstanceGroupsOutput _
derive instance repGenericAddInstanceGroupsOutput :: Generic AddInstanceGroupsOutput _
instance showAddInstanceGroupsOutput :: Show AddInstanceGroupsOutput where
  show = genericShow
instance decodeAddInstanceGroupsOutput :: Decode AddInstanceGroupsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddInstanceGroupsOutput :: Encode AddInstanceGroupsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The input argument to the <a>AddJobFlowSteps</a> operation. </p>
newtype AddJobFlowStepsInput = AddJobFlowStepsInput 
  { "JobFlowId" :: (XmlStringMaxLen256)
  , "Steps" :: (StepConfigList)
  }
derive instance newtypeAddJobFlowStepsInput :: Newtype AddJobFlowStepsInput _
derive instance repGenericAddJobFlowStepsInput :: Generic AddJobFlowStepsInput _
instance showAddJobFlowStepsInput :: Show AddJobFlowStepsInput where
  show = genericShow
instance decodeAddJobFlowStepsInput :: Decode AddJobFlowStepsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddJobFlowStepsInput :: Encode AddJobFlowStepsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The output for the <a>AddJobFlowSteps</a> operation. </p>
newtype AddJobFlowStepsOutput = AddJobFlowStepsOutput 
  { "StepIds" :: NullOrUndefined.NullOrUndefined (StepIdsList)
  }
derive instance newtypeAddJobFlowStepsOutput :: Newtype AddJobFlowStepsOutput _
derive instance repGenericAddJobFlowStepsOutput :: Generic AddJobFlowStepsOutput _
instance showAddJobFlowStepsOutput :: Show AddJobFlowStepsOutput where
  show = genericShow
instance decodeAddJobFlowStepsOutput :: Decode AddJobFlowStepsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddJobFlowStepsOutput :: Encode AddJobFlowStepsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input identifies a cluster and a list of tags to attach.</p>
newtype AddTagsInput = AddTagsInput 
  { "ResourceId" :: (ResourceId)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsInput :: Newtype AddTagsInput _
derive instance repGenericAddTagsInput :: Generic AddTagsInput _
instance showAddTagsInput :: Show AddTagsInput where
  show = genericShow
instance decodeAddTagsInput :: Decode AddTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsInput :: Encode AddTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This output indicates the result of adding tags to a resource.</p>
newtype AddTagsOutput = AddTagsOutput Types.NoArguments
derive instance newtypeAddTagsOutput :: Newtype AddTagsOutput _
derive instance repGenericAddTagsOutput :: Generic AddTagsOutput _
instance showAddTagsOutput :: Show AddTagsOutput where
  show = genericShow
instance decodeAddTagsOutput :: Decode AddTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsOutput :: Encode AddTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdjustmentType = AdjustmentType String
derive instance newtypeAdjustmentType :: Newtype AdjustmentType _
derive instance repGenericAdjustmentType :: Generic AdjustmentType _
instance showAdjustmentType :: Show AdjustmentType where
  show = genericShow
instance decodeAdjustmentType :: Decode AdjustmentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdjustmentType :: Encode AdjustmentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An application is any Amazon or third-party software that you can add to the cluster. This structure contains a list of strings that indicates the software to use with the cluster and accepts a user argument list. Amazon EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action argument. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-mapr.html">Using the MapR Distribution for Hadoop</a>. Currently supported values are:</p> <ul> <li> <p>"mapr-m3" - launch the cluster using MapR M3 Edition.</p> </li> <li> <p>"mapr-m5" - launch the cluster using MapR M5 Edition.</p> </li> <li> <p>"mapr" with the user arguments specifying "--edition,m3" or "--edition,m5" - launch the cluster using MapR M3 or M5 Edition, respectively.</p> </li> </ul> <note> <p>In Amazon EMR releases 4.x and later, the only accepted parameter is the application name. To pass arguments to applications, you supply a configuration for each application.</p> </note>
newtype Application = Application 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  , "Args" :: NullOrUndefined.NullOrUndefined (StringList)
  , "AdditionalInfo" :: NullOrUndefined.NullOrUndefined (StringMap)
  }
derive instance newtypeApplication :: Newtype Application _
derive instance repGenericApplication :: Generic Application _
instance showApplication :: Show Application where
  show = genericShow
instance decodeApplication :: Decode Application where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplication :: Encode Application where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationList = ApplicationList (Array Application)
derive instance newtypeApplicationList :: Newtype ApplicationList _
derive instance repGenericApplicationList :: Generic ApplicationList _
instance showApplicationList :: Show ApplicationList where
  show = genericShow
instance decodeApplicationList :: Decode ApplicationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationList :: Encode ApplicationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. An automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See <a>PutAutoScalingPolicy</a>.</p>
newtype AutoScalingPolicy = AutoScalingPolicy 
  { "Constraints" :: (ScalingConstraints)
  , "Rules" :: (ScalingRuleList)
  }
derive instance newtypeAutoScalingPolicy :: Newtype AutoScalingPolicy _
derive instance repGenericAutoScalingPolicy :: Generic AutoScalingPolicy _
instance showAutoScalingPolicy :: Show AutoScalingPolicy where
  show = genericShow
instance decodeAutoScalingPolicy :: Decode AutoScalingPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoScalingPolicy :: Encode AutoScalingPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See <a>PutAutoScalingPolicy</a>.</p>
newtype AutoScalingPolicyDescription = AutoScalingPolicyDescription 
  { "Status" :: NullOrUndefined.NullOrUndefined (AutoScalingPolicyStatus)
  , "Constraints" :: NullOrUndefined.NullOrUndefined (ScalingConstraints)
  , "Rules" :: NullOrUndefined.NullOrUndefined (ScalingRuleList)
  }
derive instance newtypeAutoScalingPolicyDescription :: Newtype AutoScalingPolicyDescription _
derive instance repGenericAutoScalingPolicyDescription :: Generic AutoScalingPolicyDescription _
instance showAutoScalingPolicyDescription :: Show AutoScalingPolicyDescription where
  show = genericShow
instance decodeAutoScalingPolicyDescription :: Decode AutoScalingPolicyDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoScalingPolicyDescription :: Encode AutoScalingPolicyDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutoScalingPolicyState = AutoScalingPolicyState String
derive instance newtypeAutoScalingPolicyState :: Newtype AutoScalingPolicyState _
derive instance repGenericAutoScalingPolicyState :: Generic AutoScalingPolicyState _
instance showAutoScalingPolicyState :: Show AutoScalingPolicyState where
  show = genericShow
instance decodeAutoScalingPolicyState :: Decode AutoScalingPolicyState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoScalingPolicyState :: Encode AutoScalingPolicyState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The reason for an <a>AutoScalingPolicyStatus</a> change.</p>
newtype AutoScalingPolicyStateChangeReason = AutoScalingPolicyStateChangeReason 
  { "Code" :: NullOrUndefined.NullOrUndefined (AutoScalingPolicyStateChangeReasonCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAutoScalingPolicyStateChangeReason :: Newtype AutoScalingPolicyStateChangeReason _
derive instance repGenericAutoScalingPolicyStateChangeReason :: Generic AutoScalingPolicyStateChangeReason _
instance showAutoScalingPolicyStateChangeReason :: Show AutoScalingPolicyStateChangeReason where
  show = genericShow
instance decodeAutoScalingPolicyStateChangeReason :: Decode AutoScalingPolicyStateChangeReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoScalingPolicyStateChangeReason :: Encode AutoScalingPolicyStateChangeReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutoScalingPolicyStateChangeReasonCode = AutoScalingPolicyStateChangeReasonCode String
derive instance newtypeAutoScalingPolicyStateChangeReasonCode :: Newtype AutoScalingPolicyStateChangeReasonCode _
derive instance repGenericAutoScalingPolicyStateChangeReasonCode :: Generic AutoScalingPolicyStateChangeReasonCode _
instance showAutoScalingPolicyStateChangeReasonCode :: Show AutoScalingPolicyStateChangeReasonCode where
  show = genericShow
instance decodeAutoScalingPolicyStateChangeReasonCode :: Decode AutoScalingPolicyStateChangeReasonCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoScalingPolicyStateChangeReasonCode :: Encode AutoScalingPolicyStateChangeReasonCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status of an automatic scaling policy. </p>
newtype AutoScalingPolicyStatus = AutoScalingPolicyStatus 
  { "State" :: NullOrUndefined.NullOrUndefined (AutoScalingPolicyState)
  , "StateChangeReason" :: NullOrUndefined.NullOrUndefined (AutoScalingPolicyStateChangeReason)
  }
derive instance newtypeAutoScalingPolicyStatus :: Newtype AutoScalingPolicyStatus _
derive instance repGenericAutoScalingPolicyStatus :: Generic AutoScalingPolicyStatus _
instance showAutoScalingPolicyStatus :: Show AutoScalingPolicyStatus where
  show = genericShow
instance decodeAutoScalingPolicyStatus :: Decode AutoScalingPolicyStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoScalingPolicyStatus :: Encode AutoScalingPolicyStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _
derive instance repGenericBooleanObject :: Generic BooleanObject _
instance showBooleanObject :: Show BooleanObject where
  show = genericShow
instance decodeBooleanObject :: Decode BooleanObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanObject :: Encode BooleanObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration of a bootstrap action.</p>
newtype BootstrapActionConfig = BootstrapActionConfig 
  { "Name" :: (XmlStringMaxLen256)
  , "ScriptBootstrapAction" :: (ScriptBootstrapActionConfig)
  }
derive instance newtypeBootstrapActionConfig :: Newtype BootstrapActionConfig _
derive instance repGenericBootstrapActionConfig :: Generic BootstrapActionConfig _
instance showBootstrapActionConfig :: Show BootstrapActionConfig where
  show = genericShow
instance decodeBootstrapActionConfig :: Decode BootstrapActionConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBootstrapActionConfig :: Encode BootstrapActionConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BootstrapActionConfigList = BootstrapActionConfigList (Array BootstrapActionConfig)
derive instance newtypeBootstrapActionConfigList :: Newtype BootstrapActionConfigList _
derive instance repGenericBootstrapActionConfigList :: Generic BootstrapActionConfigList _
instance showBootstrapActionConfigList :: Show BootstrapActionConfigList where
  show = genericShow
instance decodeBootstrapActionConfigList :: Decode BootstrapActionConfigList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBootstrapActionConfigList :: Encode BootstrapActionConfigList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Reports the configuration of a bootstrap action in a cluster (job flow).</p>
newtype BootstrapActionDetail = BootstrapActionDetail 
  { "BootstrapActionConfig" :: NullOrUndefined.NullOrUndefined (BootstrapActionConfig)
  }
derive instance newtypeBootstrapActionDetail :: Newtype BootstrapActionDetail _
derive instance repGenericBootstrapActionDetail :: Generic BootstrapActionDetail _
instance showBootstrapActionDetail :: Show BootstrapActionDetail where
  show = genericShow
instance decodeBootstrapActionDetail :: Decode BootstrapActionDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBootstrapActionDetail :: Encode BootstrapActionDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BootstrapActionDetailList = BootstrapActionDetailList (Array BootstrapActionDetail)
derive instance newtypeBootstrapActionDetailList :: Newtype BootstrapActionDetailList _
derive instance repGenericBootstrapActionDetailList :: Generic BootstrapActionDetailList _
instance showBootstrapActionDetailList :: Show BootstrapActionDetailList where
  show = genericShow
instance decodeBootstrapActionDetailList :: Decode BootstrapActionDetailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBootstrapActionDetailList :: Encode BootstrapActionDetailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specification of the status of a CancelSteps request. Available only in Amazon EMR version 4.8.0 and later, excluding version 5.0.0.</p>
newtype CancelStepsInfo = CancelStepsInfo 
  { "StepId" :: NullOrUndefined.NullOrUndefined (StepId)
  , "Status" :: NullOrUndefined.NullOrUndefined (CancelStepsRequestStatus)
  , "Reason" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCancelStepsInfo :: Newtype CancelStepsInfo _
derive instance repGenericCancelStepsInfo :: Generic CancelStepsInfo _
instance showCancelStepsInfo :: Show CancelStepsInfo where
  show = genericShow
instance decodeCancelStepsInfo :: Decode CancelStepsInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelStepsInfo :: Encode CancelStepsInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelStepsInfoList = CancelStepsInfoList (Array CancelStepsInfo)
derive instance newtypeCancelStepsInfoList :: Newtype CancelStepsInfoList _
derive instance repGenericCancelStepsInfoList :: Generic CancelStepsInfoList _
instance showCancelStepsInfoList :: Show CancelStepsInfoList where
  show = genericShow
instance decodeCancelStepsInfoList :: Decode CancelStepsInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelStepsInfoList :: Encode CancelStepsInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input argument to the <a>CancelSteps</a> operation.</p>
newtype CancelStepsInput = CancelStepsInput 
  { "ClusterId" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "StepIds" :: NullOrUndefined.NullOrUndefined (StepIdsList)
  }
derive instance newtypeCancelStepsInput :: Newtype CancelStepsInput _
derive instance repGenericCancelStepsInput :: Generic CancelStepsInput _
instance showCancelStepsInput :: Show CancelStepsInput where
  show = genericShow
instance decodeCancelStepsInput :: Decode CancelStepsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelStepsInput :: Encode CancelStepsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The output for the <a>CancelSteps</a> operation. </p>
newtype CancelStepsOutput = CancelStepsOutput 
  { "CancelStepsInfoList" :: NullOrUndefined.NullOrUndefined (CancelStepsInfoList)
  }
derive instance newtypeCancelStepsOutput :: Newtype CancelStepsOutput _
derive instance repGenericCancelStepsOutput :: Generic CancelStepsOutput _
instance showCancelStepsOutput :: Show CancelStepsOutput where
  show = genericShow
instance decodeCancelStepsOutput :: Decode CancelStepsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelStepsOutput :: Encode CancelStepsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelStepsRequestStatus = CancelStepsRequestStatus String
derive instance newtypeCancelStepsRequestStatus :: Newtype CancelStepsRequestStatus _
derive instance repGenericCancelStepsRequestStatus :: Generic CancelStepsRequestStatus _
instance showCancelStepsRequestStatus :: Show CancelStepsRequestStatus where
  show = genericShow
instance decodeCancelStepsRequestStatus :: Decode CancelStepsRequestStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelStepsRequestStatus :: Encode CancelStepsRequestStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The definition of a CloudWatch metric alarm, which determines when an automatic scaling activity is triggered. When the defined alarm conditions are satisfied, scaling activity begins.</p>
newtype CloudWatchAlarmDefinition = CloudWatchAlarmDefinition 
  { "ComparisonOperator" :: (ComparisonOperator)
  , "EvaluationPeriods" :: NullOrUndefined.NullOrUndefined (Int)
  , "MetricName" :: (String)
  , "Namespace" :: NullOrUndefined.NullOrUndefined (String)
  , "Period" :: (Int)
  , "Statistic" :: NullOrUndefined.NullOrUndefined (Statistic)
  , "Threshold" :: (NonNegativeDouble)
  , "Unit''" :: NullOrUndefined.NullOrUndefined (Unit'')
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (MetricDimensionList)
  }
derive instance newtypeCloudWatchAlarmDefinition :: Newtype CloudWatchAlarmDefinition _
derive instance repGenericCloudWatchAlarmDefinition :: Generic CloudWatchAlarmDefinition _
instance showCloudWatchAlarmDefinition :: Show CloudWatchAlarmDefinition where
  show = genericShow
instance decodeCloudWatchAlarmDefinition :: Decode CloudWatchAlarmDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchAlarmDefinition :: Encode CloudWatchAlarmDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The detailed description of the cluster.</p>
newtype Cluster = Cluster 
  { "Id" :: NullOrUndefined.NullOrUndefined (ClusterId)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (ClusterStatus)
  , "Ec2InstanceAttributes" :: NullOrUndefined.NullOrUndefined (Ec2InstanceAttributes)
  , "InstanceCollectionType" :: NullOrUndefined.NullOrUndefined (InstanceCollectionType)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestedAmiVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "RunningAmiVersion" :: NullOrUndefined.NullOrUndefined (String)
  , "ReleaseLabel" :: NullOrUndefined.NullOrUndefined (String)
  , "AutoTerminate" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "TerminationProtected" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "VisibleToAllUsers" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Applications" :: NullOrUndefined.NullOrUndefined (ApplicationList)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "ServiceRole" :: NullOrUndefined.NullOrUndefined (String)
  , "NormalizedInstanceHours" :: NullOrUndefined.NullOrUndefined (Int)
  , "MasterPublicDnsName" :: NullOrUndefined.NullOrUndefined (String)
  , "Configurations" :: NullOrUndefined.NullOrUndefined (ConfigurationList)
  , "SecurityConfiguration" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "AutoScalingRole" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "ScaleDownBehavior" :: NullOrUndefined.NullOrUndefined (ScaleDownBehavior)
  , "CustomAmiId" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "EbsRootVolumeSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "RepoUpgradeOnBoot" :: NullOrUndefined.NullOrUndefined (RepoUpgradeOnBoot)
  , "KerberosAttributes" :: NullOrUndefined.NullOrUndefined (KerberosAttributes)
  }
derive instance newtypeCluster :: Newtype Cluster _
derive instance repGenericCluster :: Generic Cluster _
instance showCluster :: Show Cluster where
  show = genericShow
instance decodeCluster :: Decode Cluster where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCluster :: Encode Cluster where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterId = ClusterId String
derive instance newtypeClusterId :: Newtype ClusterId _
derive instance repGenericClusterId :: Generic ClusterId _
instance showClusterId :: Show ClusterId where
  show = genericShow
instance decodeClusterId :: Decode ClusterId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterId :: Encode ClusterId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterState = ClusterState String
derive instance newtypeClusterState :: Newtype ClusterState _
derive instance repGenericClusterState :: Generic ClusterState _
instance showClusterState :: Show ClusterState where
  show = genericShow
instance decodeClusterState :: Decode ClusterState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterState :: Encode ClusterState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The reason that the cluster changed to its current state.</p>
newtype ClusterStateChangeReason = ClusterStateChangeReason 
  { "Code" :: NullOrUndefined.NullOrUndefined (ClusterStateChangeReasonCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClusterStateChangeReason :: Newtype ClusterStateChangeReason _
derive instance repGenericClusterStateChangeReason :: Generic ClusterStateChangeReason _
instance showClusterStateChangeReason :: Show ClusterStateChangeReason where
  show = genericShow
instance decodeClusterStateChangeReason :: Decode ClusterStateChangeReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterStateChangeReason :: Encode ClusterStateChangeReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterStateChangeReasonCode = ClusterStateChangeReasonCode String
derive instance newtypeClusterStateChangeReasonCode :: Newtype ClusterStateChangeReasonCode _
derive instance repGenericClusterStateChangeReasonCode :: Generic ClusterStateChangeReasonCode _
instance showClusterStateChangeReasonCode :: Show ClusterStateChangeReasonCode where
  show = genericShow
instance decodeClusterStateChangeReasonCode :: Decode ClusterStateChangeReasonCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterStateChangeReasonCode :: Encode ClusterStateChangeReasonCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterStateList = ClusterStateList (Array ClusterState)
derive instance newtypeClusterStateList :: Newtype ClusterStateList _
derive instance repGenericClusterStateList :: Generic ClusterStateList _
instance showClusterStateList :: Show ClusterStateList where
  show = genericShow
instance decodeClusterStateList :: Decode ClusterStateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterStateList :: Encode ClusterStateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The detailed status of the cluster.</p>
newtype ClusterStatus = ClusterStatus 
  { "State" :: NullOrUndefined.NullOrUndefined (ClusterState)
  , "StateChangeReason" :: NullOrUndefined.NullOrUndefined (ClusterStateChangeReason)
  , "Timeline" :: NullOrUndefined.NullOrUndefined (ClusterTimeline)
  }
derive instance newtypeClusterStatus :: Newtype ClusterStatus _
derive instance repGenericClusterStatus :: Generic ClusterStatus _
instance showClusterStatus :: Show ClusterStatus where
  show = genericShow
instance decodeClusterStatus :: Decode ClusterStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterStatus :: Encode ClusterStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The summary description of the cluster.</p>
newtype ClusterSummary = ClusterSummary 
  { "Id" :: NullOrUndefined.NullOrUndefined (ClusterId)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (ClusterStatus)
  , "NormalizedInstanceHours" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeClusterSummary :: Newtype ClusterSummary _
derive instance repGenericClusterSummary :: Generic ClusterSummary _
instance showClusterSummary :: Show ClusterSummary where
  show = genericShow
instance decodeClusterSummary :: Decode ClusterSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSummary :: Encode ClusterSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterSummaryList = ClusterSummaryList (Array ClusterSummary)
derive instance newtypeClusterSummaryList :: Newtype ClusterSummaryList _
derive instance repGenericClusterSummaryList :: Generic ClusterSummaryList _
instance showClusterSummaryList :: Show ClusterSummaryList where
  show = genericShow
instance decodeClusterSummaryList :: Decode ClusterSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterSummaryList :: Encode ClusterSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the timeline of the cluster's lifecycle.</p>
newtype ClusterTimeline = ClusterTimeline 
  { "CreationDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeClusterTimeline :: Newtype ClusterTimeline _
derive instance repGenericClusterTimeline :: Generic ClusterTimeline _
instance showClusterTimeline :: Show ClusterTimeline where
  show = genericShow
instance decodeClusterTimeline :: Decode ClusterTimeline where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterTimeline :: Encode ClusterTimeline where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An entity describing an executable that runs on a cluster.</p>
newtype Command = Command 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "ScriptPath" :: NullOrUndefined.NullOrUndefined (String)
  , "Args" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeCommand :: Newtype Command _
derive instance repGenericCommand :: Generic Command _
instance showCommand :: Show Command where
  show = genericShow
instance decodeCommand :: Decode Command where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommand :: Encode Command where
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


newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _
derive instance repGenericComparisonOperator :: Generic ComparisonOperator _
instance showComparisonOperator :: Show ComparisonOperator where
  show = genericShow
instance decodeComparisonOperator :: Decode ComparisonOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparisonOperator :: Encode ComparisonOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <note> <p>Amazon EMR releases 4.x or later.</p> </note> <p>An optional configuration specification to be used when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR. A configuration consists of a classification, properties, and optional nested configurations. A classification refers to an application-specific configuration file. Properties are the settings you want to change in that file. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html">Configuring Applications</a>.</p>
newtype Configuration = Configuration 
  { "Classification" :: NullOrUndefined.NullOrUndefined (String)
  , "Properties" :: NullOrUndefined.NullOrUndefined (StringMap)
  }
derive instance newtypeConfiguration :: Newtype Configuration _
derive instance repGenericConfiguration :: Generic Configuration _
instance showConfiguration :: Show Configuration where
  show = genericShow
instance decodeConfiguration :: Decode Configuration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfiguration :: Encode Configuration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationList = ConfigurationList (Array Configuration)
derive instance newtypeConfigurationList :: Newtype ConfigurationList _
derive instance repGenericConfigurationList :: Generic ConfigurationList _
instance showConfigurationList :: Show ConfigurationList where
  show = genericShow
instance decodeConfigurationList :: Decode ConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationList :: Encode ConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSecurityConfigurationInput = CreateSecurityConfigurationInput 
  { "Name" :: (XmlString)
  , "SecurityConfiguration" :: (String)
  }
derive instance newtypeCreateSecurityConfigurationInput :: Newtype CreateSecurityConfigurationInput _
derive instance repGenericCreateSecurityConfigurationInput :: Generic CreateSecurityConfigurationInput _
instance showCreateSecurityConfigurationInput :: Show CreateSecurityConfigurationInput where
  show = genericShow
instance decodeCreateSecurityConfigurationInput :: Decode CreateSecurityConfigurationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSecurityConfigurationInput :: Encode CreateSecurityConfigurationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSecurityConfigurationOutput = CreateSecurityConfigurationOutput 
  { "Name" :: (XmlString)
  , "CreationDateTime" :: (Date)
  }
derive instance newtypeCreateSecurityConfigurationOutput :: Newtype CreateSecurityConfigurationOutput _
derive instance repGenericCreateSecurityConfigurationOutput :: Generic CreateSecurityConfigurationOutput _
instance showCreateSecurityConfigurationOutput :: Show CreateSecurityConfigurationOutput where
  show = genericShow
instance decodeCreateSecurityConfigurationOutput :: Decode CreateSecurityConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSecurityConfigurationOutput :: Encode CreateSecurityConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where
  show = genericShow
instance decodeDate :: Decode Date where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDate :: Encode Date where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSecurityConfigurationInput = DeleteSecurityConfigurationInput 
  { "Name" :: (XmlString)
  }
derive instance newtypeDeleteSecurityConfigurationInput :: Newtype DeleteSecurityConfigurationInput _
derive instance repGenericDeleteSecurityConfigurationInput :: Generic DeleteSecurityConfigurationInput _
instance showDeleteSecurityConfigurationInput :: Show DeleteSecurityConfigurationInput where
  show = genericShow
instance decodeDeleteSecurityConfigurationInput :: Decode DeleteSecurityConfigurationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSecurityConfigurationInput :: Encode DeleteSecurityConfigurationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSecurityConfigurationOutput = DeleteSecurityConfigurationOutput Types.NoArguments
derive instance newtypeDeleteSecurityConfigurationOutput :: Newtype DeleteSecurityConfigurationOutput _
derive instance repGenericDeleteSecurityConfigurationOutput :: Generic DeleteSecurityConfigurationOutput _
instance showDeleteSecurityConfigurationOutput :: Show DeleteSecurityConfigurationOutput where
  show = genericShow
instance decodeDeleteSecurityConfigurationOutput :: Decode DeleteSecurityConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSecurityConfigurationOutput :: Encode DeleteSecurityConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input determines which cluster to describe.</p>
newtype DescribeClusterInput = DescribeClusterInput 
  { "ClusterId" :: (ClusterId)
  }
derive instance newtypeDescribeClusterInput :: Newtype DescribeClusterInput _
derive instance repGenericDescribeClusterInput :: Generic DescribeClusterInput _
instance showDescribeClusterInput :: Show DescribeClusterInput where
  show = genericShow
instance decodeDescribeClusterInput :: Decode DescribeClusterInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClusterInput :: Encode DescribeClusterInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This output contains the description of the cluster.</p>
newtype DescribeClusterOutput = DescribeClusterOutput 
  { "Cluster" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeDescribeClusterOutput :: Newtype DescribeClusterOutput _
derive instance repGenericDescribeClusterOutput :: Generic DescribeClusterOutput _
instance showDescribeClusterOutput :: Show DescribeClusterOutput where
  show = genericShow
instance decodeDescribeClusterOutput :: Decode DescribeClusterOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClusterOutput :: Encode DescribeClusterOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The input for the <a>DescribeJobFlows</a> operation. </p>
newtype DescribeJobFlowsInput = DescribeJobFlowsInput 
  { "CreatedAfter" :: NullOrUndefined.NullOrUndefined (Date)
  , "CreatedBefore" :: NullOrUndefined.NullOrUndefined (Date)
  , "JobFlowIds" :: NullOrUndefined.NullOrUndefined (XmlStringList)
  , "JobFlowStates" :: NullOrUndefined.NullOrUndefined (JobFlowExecutionStateList)
  }
derive instance newtypeDescribeJobFlowsInput :: Newtype DescribeJobFlowsInput _
derive instance repGenericDescribeJobFlowsInput :: Generic DescribeJobFlowsInput _
instance showDescribeJobFlowsInput :: Show DescribeJobFlowsInput where
  show = genericShow
instance decodeDescribeJobFlowsInput :: Decode DescribeJobFlowsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobFlowsInput :: Encode DescribeJobFlowsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The output for the <a>DescribeJobFlows</a> operation. </p>
newtype DescribeJobFlowsOutput = DescribeJobFlowsOutput 
  { "JobFlows" :: NullOrUndefined.NullOrUndefined (JobFlowDetailList)
  }
derive instance newtypeDescribeJobFlowsOutput :: Newtype DescribeJobFlowsOutput _
derive instance repGenericDescribeJobFlowsOutput :: Generic DescribeJobFlowsOutput _
instance showDescribeJobFlowsOutput :: Show DescribeJobFlowsOutput where
  show = genericShow
instance decodeDescribeJobFlowsOutput :: Decode DescribeJobFlowsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobFlowsOutput :: Encode DescribeJobFlowsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeSecurityConfigurationInput = DescribeSecurityConfigurationInput 
  { "Name" :: (XmlString)
  }
derive instance newtypeDescribeSecurityConfigurationInput :: Newtype DescribeSecurityConfigurationInput _
derive instance repGenericDescribeSecurityConfigurationInput :: Generic DescribeSecurityConfigurationInput _
instance showDescribeSecurityConfigurationInput :: Show DescribeSecurityConfigurationInput where
  show = genericShow
instance decodeDescribeSecurityConfigurationInput :: Decode DescribeSecurityConfigurationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSecurityConfigurationInput :: Encode DescribeSecurityConfigurationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeSecurityConfigurationOutput = DescribeSecurityConfigurationOutput 
  { "Name" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "SecurityConfiguration" :: NullOrUndefined.NullOrUndefined (String)
  , "CreationDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeDescribeSecurityConfigurationOutput :: Newtype DescribeSecurityConfigurationOutput _
derive instance repGenericDescribeSecurityConfigurationOutput :: Generic DescribeSecurityConfigurationOutput _
instance showDescribeSecurityConfigurationOutput :: Show DescribeSecurityConfigurationOutput where
  show = genericShow
instance decodeDescribeSecurityConfigurationOutput :: Decode DescribeSecurityConfigurationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSecurityConfigurationOutput :: Encode DescribeSecurityConfigurationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input determines which step to describe.</p>
newtype DescribeStepInput = DescribeStepInput 
  { "ClusterId" :: (ClusterId)
  , "StepId" :: (StepId)
  }
derive instance newtypeDescribeStepInput :: Newtype DescribeStepInput _
derive instance repGenericDescribeStepInput :: Generic DescribeStepInput _
instance showDescribeStepInput :: Show DescribeStepInput where
  show = genericShow
instance decodeDescribeStepInput :: Decode DescribeStepInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStepInput :: Encode DescribeStepInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This output contains the description of the cluster step.</p>
newtype DescribeStepOutput = DescribeStepOutput 
  { "Step" :: NullOrUndefined.NullOrUndefined (Step)
  }
derive instance newtypeDescribeStepOutput :: Newtype DescribeStepOutput _
derive instance repGenericDescribeStepOutput :: Generic DescribeStepOutput _
instance showDescribeStepOutput :: Show DescribeStepOutput where
  show = genericShow
instance decodeDescribeStepOutput :: Decode DescribeStepOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStepOutput :: Encode DescribeStepOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EC2InstanceIdsList = EC2InstanceIdsList (Array InstanceId)
derive instance newtypeEC2InstanceIdsList :: Newtype EC2InstanceIdsList _
derive instance repGenericEC2InstanceIdsList :: Generic EC2InstanceIdsList _
instance showEC2InstanceIdsList :: Show EC2InstanceIdsList where
  show = genericShow
instance decodeEC2InstanceIdsList :: Decode EC2InstanceIdsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2InstanceIdsList :: Encode EC2InstanceIdsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EC2InstanceIdsToTerminateList = EC2InstanceIdsToTerminateList (Array InstanceId)
derive instance newtypeEC2InstanceIdsToTerminateList :: Newtype EC2InstanceIdsToTerminateList _
derive instance repGenericEC2InstanceIdsToTerminateList :: Generic EC2InstanceIdsToTerminateList _
instance showEC2InstanceIdsToTerminateList :: Show EC2InstanceIdsToTerminateList where
  show = genericShow
instance decodeEC2InstanceIdsToTerminateList :: Decode EC2InstanceIdsToTerminateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2InstanceIdsToTerminateList :: Encode EC2InstanceIdsToTerminateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration of requested EBS block device associated with the instance group.</p>
newtype EbsBlockDevice = EbsBlockDevice 
  { "VolumeSpecification" :: NullOrUndefined.NullOrUndefined (VolumeSpecification)
  , "Device" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEbsBlockDevice :: Newtype EbsBlockDevice _
derive instance repGenericEbsBlockDevice :: Generic EbsBlockDevice _
instance showEbsBlockDevice :: Show EbsBlockDevice where
  show = genericShow
instance decodeEbsBlockDevice :: Decode EbsBlockDevice where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEbsBlockDevice :: Encode EbsBlockDevice where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration of requested EBS block device associated with the instance group with count of volumes that will be associated to every instance.</p>
newtype EbsBlockDeviceConfig = EbsBlockDeviceConfig 
  { "VolumeSpecification" :: (VolumeSpecification)
  , "VolumesPerInstance" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeEbsBlockDeviceConfig :: Newtype EbsBlockDeviceConfig _
derive instance repGenericEbsBlockDeviceConfig :: Generic EbsBlockDeviceConfig _
instance showEbsBlockDeviceConfig :: Show EbsBlockDeviceConfig where
  show = genericShow
instance decodeEbsBlockDeviceConfig :: Decode EbsBlockDeviceConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEbsBlockDeviceConfig :: Encode EbsBlockDeviceConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EbsBlockDeviceConfigList = EbsBlockDeviceConfigList (Array EbsBlockDeviceConfig)
derive instance newtypeEbsBlockDeviceConfigList :: Newtype EbsBlockDeviceConfigList _
derive instance repGenericEbsBlockDeviceConfigList :: Generic EbsBlockDeviceConfigList _
instance showEbsBlockDeviceConfigList :: Show EbsBlockDeviceConfigList where
  show = genericShow
instance decodeEbsBlockDeviceConfigList :: Decode EbsBlockDeviceConfigList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEbsBlockDeviceConfigList :: Encode EbsBlockDeviceConfigList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EbsBlockDeviceList = EbsBlockDeviceList (Array EbsBlockDevice)
derive instance newtypeEbsBlockDeviceList :: Newtype EbsBlockDeviceList _
derive instance repGenericEbsBlockDeviceList :: Generic EbsBlockDeviceList _
instance showEbsBlockDeviceList :: Show EbsBlockDeviceList where
  show = genericShow
instance decodeEbsBlockDeviceList :: Decode EbsBlockDeviceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEbsBlockDeviceList :: Encode EbsBlockDeviceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon EBS configuration of a cluster instance.</p>
newtype EbsConfiguration = EbsConfiguration 
  { "EbsBlockDeviceConfigs" :: NullOrUndefined.NullOrUndefined (EbsBlockDeviceConfigList)
  , "EbsOptimized" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  }
derive instance newtypeEbsConfiguration :: Newtype EbsConfiguration _
derive instance repGenericEbsConfiguration :: Generic EbsConfiguration _
instance showEbsConfiguration :: Show EbsConfiguration where
  show = genericShow
instance decodeEbsConfiguration :: Decode EbsConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEbsConfiguration :: Encode EbsConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>EBS block device that's attached to an EC2 instance.</p>
newtype EbsVolume = EbsVolume 
  { "Device" :: NullOrUndefined.NullOrUndefined (String)
  , "VolumeId" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEbsVolume :: Newtype EbsVolume _
derive instance repGenericEbsVolume :: Generic EbsVolume _
instance showEbsVolume :: Show EbsVolume where
  show = genericShow
instance decodeEbsVolume :: Decode EbsVolume where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEbsVolume :: Encode EbsVolume where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EbsVolumeList = EbsVolumeList (Array EbsVolume)
derive instance newtypeEbsVolumeList :: Newtype EbsVolumeList _
derive instance repGenericEbsVolumeList :: Generic EbsVolumeList _
instance showEbsVolumeList :: Show EbsVolumeList where
  show = genericShow
instance decodeEbsVolumeList :: Decode EbsVolumeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEbsVolumeList :: Encode EbsVolumeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.</p>
newtype Ec2InstanceAttributes = Ec2InstanceAttributes 
  { "Ec2KeyName" :: NullOrUndefined.NullOrUndefined (String)
  , "Ec2SubnetId" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestedEc2SubnetIds" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256List)
  , "Ec2AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "RequestedEc2AvailabilityZones" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256List)
  , "IamInstanceProfile" :: NullOrUndefined.NullOrUndefined (String)
  , "EmrManagedMasterSecurityGroup" :: NullOrUndefined.NullOrUndefined (String)
  , "EmrManagedSlaveSecurityGroup" :: NullOrUndefined.NullOrUndefined (String)
  , "ServiceAccessSecurityGroup" :: NullOrUndefined.NullOrUndefined (String)
  , "AdditionalMasterSecurityGroups" :: NullOrUndefined.NullOrUndefined (StringList)
  , "AdditionalSlaveSecurityGroups" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeEc2InstanceAttributes :: Newtype Ec2InstanceAttributes _
derive instance repGenericEc2InstanceAttributes :: Generic Ec2InstanceAttributes _
instance showEc2InstanceAttributes :: Show Ec2InstanceAttributes where
  show = genericShow
instance decodeEc2InstanceAttributes :: Decode Ec2InstanceAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEc2InstanceAttributes :: Encode Ec2InstanceAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _
derive instance repGenericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow
instance decodeErrorCode :: Decode ErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCode :: Encode ErrorCode where
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


-- | <p>The details of the step failure. The service attempts to detect the root cause for many common failures.</p>
newtype FailureDetails = FailureDetails 
  { "Reason" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  , "LogFile" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeFailureDetails :: Newtype FailureDetails _
derive instance repGenericFailureDetails :: Generic FailureDetails _
instance showFailureDetails :: Show FailureDetails where
  show = genericShow
instance decodeFailureDetails :: Decode FailureDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureDetails :: Encode FailureDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A job flow step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.</p>
newtype HadoopJarStepConfig = HadoopJarStepConfig 
  { "Properties" :: NullOrUndefined.NullOrUndefined (KeyValueList)
  , "Jar" :: (XmlString)
  , "MainClass" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "Args" :: NullOrUndefined.NullOrUndefined (XmlStringList)
  }
derive instance newtypeHadoopJarStepConfig :: Newtype HadoopJarStepConfig _
derive instance repGenericHadoopJarStepConfig :: Generic HadoopJarStepConfig _
instance showHadoopJarStepConfig :: Show HadoopJarStepConfig where
  show = genericShow
instance decodeHadoopJarStepConfig :: Decode HadoopJarStepConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHadoopJarStepConfig :: Encode HadoopJarStepConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A cluster step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.</p>
newtype HadoopStepConfig = HadoopStepConfig 
  { "Jar" :: NullOrUndefined.NullOrUndefined (String)
  , "Properties" :: NullOrUndefined.NullOrUndefined (StringMap)
  , "MainClass" :: NullOrUndefined.NullOrUndefined (String)
  , "Args" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeHadoopStepConfig :: Newtype HadoopStepConfig _
derive instance repGenericHadoopStepConfig :: Generic HadoopStepConfig _
instance showHadoopStepConfig :: Show HadoopStepConfig where
  show = genericShow
instance decodeHadoopStepConfig :: Decode HadoopStepConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHadoopStepConfig :: Encode HadoopStepConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an EC2 instance provisioned as part of cluster.</p>
newtype Instance = Instance 
  { "Id" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "Ec2InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "PublicDnsName" :: NullOrUndefined.NullOrUndefined (String)
  , "PublicIpAddress" :: NullOrUndefined.NullOrUndefined (String)
  , "PrivateDnsName" :: NullOrUndefined.NullOrUndefined (String)
  , "PrivateIpAddress" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (InstanceStatus)
  , "InstanceGroupId" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceFleetId" :: NullOrUndefined.NullOrUndefined (InstanceFleetId)
  , "Market" :: NullOrUndefined.NullOrUndefined (MarketType)
  , "InstanceType" :: NullOrUndefined.NullOrUndefined (InstanceType)
  , "EbsVolumes" :: NullOrUndefined.NullOrUndefined (EbsVolumeList)
  }
derive instance newtypeInstance :: Newtype Instance _
derive instance repGenericInstance :: Generic Instance _
instance showInstance :: Show Instance where
  show = genericShow
instance decodeInstance :: Decode Instance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstance :: Encode Instance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceCollectionType = InstanceCollectionType String
derive instance newtypeInstanceCollectionType :: Newtype InstanceCollectionType _
derive instance repGenericInstanceCollectionType :: Generic InstanceCollectionType _
instance showInstanceCollectionType :: Show InstanceCollectionType where
  show = genericShow
instance decodeInstanceCollectionType :: Decode InstanceCollectionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceCollectionType :: Encode InstanceCollectionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an instance fleet, which is a group of EC2 instances that host a particular node type (master, core, or task) in an Amazon EMR cluster. Instance fleets can consist of a mix of instance types and On-Demand and Spot instances, which are provisioned to meet a defined target capacity. </p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleet = InstanceFleet 
  { "Id" :: NullOrUndefined.NullOrUndefined (InstanceFleetId)
  , "Name" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Status" :: NullOrUndefined.NullOrUndefined (InstanceFleetStatus)
  , "InstanceFleetType" :: NullOrUndefined.NullOrUndefined (InstanceFleetType)
  , "TargetOnDemandCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "TargetSpotCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "ProvisionedOnDemandCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "ProvisionedSpotCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "InstanceTypeSpecifications" :: NullOrUndefined.NullOrUndefined (InstanceTypeSpecificationList)
  , "LaunchSpecifications" :: NullOrUndefined.NullOrUndefined (InstanceFleetProvisioningSpecifications)
  }
derive instance newtypeInstanceFleet :: Newtype InstanceFleet _
derive instance repGenericInstanceFleet :: Generic InstanceFleet _
instance showInstanceFleet :: Show InstanceFleet where
  show = genericShow
instance decodeInstanceFleet :: Decode InstanceFleet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleet :: Encode InstanceFleet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configuration that defines an instance fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetConfig = InstanceFleetConfig 
  { "Name" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "InstanceFleetType" :: (InstanceFleetType)
  , "TargetOnDemandCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "TargetSpotCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "InstanceTypeConfigs" :: NullOrUndefined.NullOrUndefined (InstanceTypeConfigList)
  , "LaunchSpecifications" :: NullOrUndefined.NullOrUndefined (InstanceFleetProvisioningSpecifications)
  }
derive instance newtypeInstanceFleetConfig :: Newtype InstanceFleetConfig _
derive instance repGenericInstanceFleetConfig :: Generic InstanceFleetConfig _
instance showInstanceFleetConfig :: Show InstanceFleetConfig where
  show = genericShow
instance decodeInstanceFleetConfig :: Decode InstanceFleetConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetConfig :: Encode InstanceFleetConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceFleetConfigList = InstanceFleetConfigList (Array InstanceFleetConfig)
derive instance newtypeInstanceFleetConfigList :: Newtype InstanceFleetConfigList _
derive instance repGenericInstanceFleetConfigList :: Generic InstanceFleetConfigList _
instance showInstanceFleetConfigList :: Show InstanceFleetConfigList where
  show = genericShow
instance decodeInstanceFleetConfigList :: Decode InstanceFleetConfigList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetConfigList :: Encode InstanceFleetConfigList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceFleetId = InstanceFleetId String
derive instance newtypeInstanceFleetId :: Newtype InstanceFleetId _
derive instance repGenericInstanceFleetId :: Generic InstanceFleetId _
instance showInstanceFleetId :: Show InstanceFleetId where
  show = genericShow
instance decodeInstanceFleetId :: Decode InstanceFleetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetId :: Encode InstanceFleetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceFleetList = InstanceFleetList (Array InstanceFleet)
derive instance newtypeInstanceFleetList :: Newtype InstanceFleetList _
derive instance repGenericInstanceFleetList :: Generic InstanceFleetList _
instance showInstanceFleetList :: Show InstanceFleetList where
  show = genericShow
instance decodeInstanceFleetList :: Decode InstanceFleetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetList :: Encode InstanceFleetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration parameters for an instance fleet modification request.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetModifyConfig = InstanceFleetModifyConfig 
  { "InstanceFleetId" :: (InstanceFleetId)
  , "TargetOnDemandCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "TargetSpotCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  }
derive instance newtypeInstanceFleetModifyConfig :: Newtype InstanceFleetModifyConfig _
derive instance repGenericInstanceFleetModifyConfig :: Generic InstanceFleetModifyConfig _
instance showInstanceFleetModifyConfig :: Show InstanceFleetModifyConfig where
  show = genericShow
instance decodeInstanceFleetModifyConfig :: Decode InstanceFleetModifyConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetModifyConfig :: Encode InstanceFleetModifyConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The launch specification for Spot instances in the fleet, which determines the defined duration and provisioning timeout behavior.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetProvisioningSpecifications = InstanceFleetProvisioningSpecifications 
  { "SpotSpecification" :: (SpotProvisioningSpecification)
  }
derive instance newtypeInstanceFleetProvisioningSpecifications :: Newtype InstanceFleetProvisioningSpecifications _
derive instance repGenericInstanceFleetProvisioningSpecifications :: Generic InstanceFleetProvisioningSpecifications _
instance showInstanceFleetProvisioningSpecifications :: Show InstanceFleetProvisioningSpecifications where
  show = genericShow
instance decodeInstanceFleetProvisioningSpecifications :: Decode InstanceFleetProvisioningSpecifications where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetProvisioningSpecifications :: Encode InstanceFleetProvisioningSpecifications where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceFleetState = InstanceFleetState String
derive instance newtypeInstanceFleetState :: Newtype InstanceFleetState _
derive instance repGenericInstanceFleetState :: Generic InstanceFleetState _
instance showInstanceFleetState :: Show InstanceFleetState where
  show = genericShow
instance decodeInstanceFleetState :: Decode InstanceFleetState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetState :: Encode InstanceFleetState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides status change reason details for the instance fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetStateChangeReason = InstanceFleetStateChangeReason 
  { "Code" :: NullOrUndefined.NullOrUndefined (InstanceFleetStateChangeReasonCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInstanceFleetStateChangeReason :: Newtype InstanceFleetStateChangeReason _
derive instance repGenericInstanceFleetStateChangeReason :: Generic InstanceFleetStateChangeReason _
instance showInstanceFleetStateChangeReason :: Show InstanceFleetStateChangeReason where
  show = genericShow
instance decodeInstanceFleetStateChangeReason :: Decode InstanceFleetStateChangeReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetStateChangeReason :: Encode InstanceFleetStateChangeReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceFleetStateChangeReasonCode = InstanceFleetStateChangeReasonCode String
derive instance newtypeInstanceFleetStateChangeReasonCode :: Newtype InstanceFleetStateChangeReasonCode _
derive instance repGenericInstanceFleetStateChangeReasonCode :: Generic InstanceFleetStateChangeReasonCode _
instance showInstanceFleetStateChangeReasonCode :: Show InstanceFleetStateChangeReasonCode where
  show = genericShow
instance decodeInstanceFleetStateChangeReasonCode :: Decode InstanceFleetStateChangeReasonCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetStateChangeReasonCode :: Encode InstanceFleetStateChangeReasonCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status of the instance fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetStatus = InstanceFleetStatus 
  { "State" :: NullOrUndefined.NullOrUndefined (InstanceFleetState)
  , "StateChangeReason" :: NullOrUndefined.NullOrUndefined (InstanceFleetStateChangeReason)
  , "Timeline" :: NullOrUndefined.NullOrUndefined (InstanceFleetTimeline)
  }
derive instance newtypeInstanceFleetStatus :: Newtype InstanceFleetStatus _
derive instance repGenericInstanceFleetStatus :: Generic InstanceFleetStatus _
instance showInstanceFleetStatus :: Show InstanceFleetStatus where
  show = genericShow
instance decodeInstanceFleetStatus :: Decode InstanceFleetStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetStatus :: Encode InstanceFleetStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetTimeline = InstanceFleetTimeline 
  { "CreationDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeInstanceFleetTimeline :: Newtype InstanceFleetTimeline _
derive instance repGenericInstanceFleetTimeline :: Generic InstanceFleetTimeline _
instance showInstanceFleetTimeline :: Show InstanceFleetTimeline where
  show = genericShow
instance decodeInstanceFleetTimeline :: Decode InstanceFleetTimeline where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetTimeline :: Encode InstanceFleetTimeline where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceFleetType = InstanceFleetType String
derive instance newtypeInstanceFleetType :: Newtype InstanceFleetType _
derive instance repGenericInstanceFleetType :: Generic InstanceFleetType _
instance showInstanceFleetType :: Show InstanceFleetType where
  show = genericShow
instance decodeInstanceFleetType :: Decode InstanceFleetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceFleetType :: Encode InstanceFleetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This entity represents an instance group, which is a group of instances that have common purpose. For example, CORE instance group is used for HDFS.</p>
newtype InstanceGroup = InstanceGroup 
  { "Id" :: NullOrUndefined.NullOrUndefined (InstanceGroupId)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Market" :: NullOrUndefined.NullOrUndefined (MarketType)
  , "InstanceGroupType" :: NullOrUndefined.NullOrUndefined (InstanceGroupType)
  , "BidPrice" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined.NullOrUndefined (InstanceType)
  , "RequestedInstanceCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "RunningInstanceCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "Status" :: NullOrUndefined.NullOrUndefined (InstanceGroupStatus)
  , "Configurations" :: NullOrUndefined.NullOrUndefined (ConfigurationList)
  , "EbsBlockDevices" :: NullOrUndefined.NullOrUndefined (EbsBlockDeviceList)
  , "EbsOptimized" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  , "ShrinkPolicy" :: NullOrUndefined.NullOrUndefined (ShrinkPolicy)
  , "AutoScalingPolicy" :: NullOrUndefined.NullOrUndefined (AutoScalingPolicyDescription)
  }
derive instance newtypeInstanceGroup :: Newtype InstanceGroup _
derive instance repGenericInstanceGroup :: Generic InstanceGroup _
instance showInstanceGroup :: Show InstanceGroup where
  show = genericShow
instance decodeInstanceGroup :: Decode InstanceGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroup :: Encode InstanceGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration defining a new instance group.</p>
newtype InstanceGroupConfig = InstanceGroupConfig 
  { "Name" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Market" :: NullOrUndefined.NullOrUndefined (MarketType)
  , "InstanceRole" :: (InstanceRoleType)
  , "BidPrice" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "InstanceType" :: (InstanceType)
  , "InstanceCount" :: (Int)
  , "Configurations" :: NullOrUndefined.NullOrUndefined (ConfigurationList)
  , "EbsConfiguration" :: NullOrUndefined.NullOrUndefined (EbsConfiguration)
  , "AutoScalingPolicy" :: NullOrUndefined.NullOrUndefined (AutoScalingPolicy)
  }
derive instance newtypeInstanceGroupConfig :: Newtype InstanceGroupConfig _
derive instance repGenericInstanceGroupConfig :: Generic InstanceGroupConfig _
instance showInstanceGroupConfig :: Show InstanceGroupConfig where
  show = genericShow
instance decodeInstanceGroupConfig :: Decode InstanceGroupConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupConfig :: Encode InstanceGroupConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupConfigList = InstanceGroupConfigList (Array InstanceGroupConfig)
derive instance newtypeInstanceGroupConfigList :: Newtype InstanceGroupConfigList _
derive instance repGenericInstanceGroupConfigList :: Generic InstanceGroupConfigList _
instance showInstanceGroupConfigList :: Show InstanceGroupConfigList where
  show = genericShow
instance decodeInstanceGroupConfigList :: Decode InstanceGroupConfigList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupConfigList :: Encode InstanceGroupConfigList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed information about an instance group.</p>
newtype InstanceGroupDetail = InstanceGroupDetail 
  { "InstanceGroupId" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Name" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Market" :: (MarketType)
  , "InstanceRole" :: (InstanceRoleType)
  , "BidPrice" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "InstanceType" :: (InstanceType)
  , "InstanceRequestCount" :: (Int)
  , "InstanceRunningCount" :: (Int)
  , "State" :: (InstanceGroupState)
  , "LastStateChangeReason" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "CreationDateTime" :: (Date)
  , "StartDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeInstanceGroupDetail :: Newtype InstanceGroupDetail _
derive instance repGenericInstanceGroupDetail :: Generic InstanceGroupDetail _
instance showInstanceGroupDetail :: Show InstanceGroupDetail where
  show = genericShow
instance decodeInstanceGroupDetail :: Decode InstanceGroupDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupDetail :: Encode InstanceGroupDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupDetailList = InstanceGroupDetailList (Array InstanceGroupDetail)
derive instance newtypeInstanceGroupDetailList :: Newtype InstanceGroupDetailList _
derive instance repGenericInstanceGroupDetailList :: Generic InstanceGroupDetailList _
instance showInstanceGroupDetailList :: Show InstanceGroupDetailList where
  show = genericShow
instance decodeInstanceGroupDetailList :: Decode InstanceGroupDetailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupDetailList :: Encode InstanceGroupDetailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupId = InstanceGroupId String
derive instance newtypeInstanceGroupId :: Newtype InstanceGroupId _
derive instance repGenericInstanceGroupId :: Generic InstanceGroupId _
instance showInstanceGroupId :: Show InstanceGroupId where
  show = genericShow
instance decodeInstanceGroupId :: Decode InstanceGroupId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupId :: Encode InstanceGroupId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupIdsList = InstanceGroupIdsList (Array XmlStringMaxLen256)
derive instance newtypeInstanceGroupIdsList :: Newtype InstanceGroupIdsList _
derive instance repGenericInstanceGroupIdsList :: Generic InstanceGroupIdsList _
instance showInstanceGroupIdsList :: Show InstanceGroupIdsList where
  show = genericShow
instance decodeInstanceGroupIdsList :: Decode InstanceGroupIdsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupIdsList :: Encode InstanceGroupIdsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupList = InstanceGroupList (Array InstanceGroup)
derive instance newtypeInstanceGroupList :: Newtype InstanceGroupList _
derive instance repGenericInstanceGroupList :: Generic InstanceGroupList _
instance showInstanceGroupList :: Show InstanceGroupList where
  show = genericShow
instance decodeInstanceGroupList :: Decode InstanceGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupList :: Encode InstanceGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Modify an instance group size.</p>
newtype InstanceGroupModifyConfig = InstanceGroupModifyConfig 
  { "InstanceGroupId" :: (XmlStringMaxLen256)
  , "InstanceCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "EC2InstanceIdsToTerminate" :: NullOrUndefined.NullOrUndefined (EC2InstanceIdsToTerminateList)
  , "ShrinkPolicy" :: NullOrUndefined.NullOrUndefined (ShrinkPolicy)
  }
derive instance newtypeInstanceGroupModifyConfig :: Newtype InstanceGroupModifyConfig _
derive instance repGenericInstanceGroupModifyConfig :: Generic InstanceGroupModifyConfig _
instance showInstanceGroupModifyConfig :: Show InstanceGroupModifyConfig where
  show = genericShow
instance decodeInstanceGroupModifyConfig :: Decode InstanceGroupModifyConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupModifyConfig :: Encode InstanceGroupModifyConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupModifyConfigList = InstanceGroupModifyConfigList (Array InstanceGroupModifyConfig)
derive instance newtypeInstanceGroupModifyConfigList :: Newtype InstanceGroupModifyConfigList _
derive instance repGenericInstanceGroupModifyConfigList :: Generic InstanceGroupModifyConfigList _
instance showInstanceGroupModifyConfigList :: Show InstanceGroupModifyConfigList where
  show = genericShow
instance decodeInstanceGroupModifyConfigList :: Decode InstanceGroupModifyConfigList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupModifyConfigList :: Encode InstanceGroupModifyConfigList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupState = InstanceGroupState String
derive instance newtypeInstanceGroupState :: Newtype InstanceGroupState _
derive instance repGenericInstanceGroupState :: Generic InstanceGroupState _
instance showInstanceGroupState :: Show InstanceGroupState where
  show = genericShow
instance decodeInstanceGroupState :: Decode InstanceGroupState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupState :: Encode InstanceGroupState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status change reason details for the instance group.</p>
newtype InstanceGroupStateChangeReason = InstanceGroupStateChangeReason 
  { "Code" :: NullOrUndefined.NullOrUndefined (InstanceGroupStateChangeReasonCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInstanceGroupStateChangeReason :: Newtype InstanceGroupStateChangeReason _
derive instance repGenericInstanceGroupStateChangeReason :: Generic InstanceGroupStateChangeReason _
instance showInstanceGroupStateChangeReason :: Show InstanceGroupStateChangeReason where
  show = genericShow
instance decodeInstanceGroupStateChangeReason :: Decode InstanceGroupStateChangeReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupStateChangeReason :: Encode InstanceGroupStateChangeReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupStateChangeReasonCode = InstanceGroupStateChangeReasonCode String
derive instance newtypeInstanceGroupStateChangeReasonCode :: Newtype InstanceGroupStateChangeReasonCode _
derive instance repGenericInstanceGroupStateChangeReasonCode :: Generic InstanceGroupStateChangeReasonCode _
instance showInstanceGroupStateChangeReasonCode :: Show InstanceGroupStateChangeReasonCode where
  show = genericShow
instance decodeInstanceGroupStateChangeReasonCode :: Decode InstanceGroupStateChangeReasonCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupStateChangeReasonCode :: Encode InstanceGroupStateChangeReasonCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The details of the instance group status.</p>
newtype InstanceGroupStatus = InstanceGroupStatus 
  { "State" :: NullOrUndefined.NullOrUndefined (InstanceGroupState)
  , "StateChangeReason" :: NullOrUndefined.NullOrUndefined (InstanceGroupStateChangeReason)
  , "Timeline" :: NullOrUndefined.NullOrUndefined (InstanceGroupTimeline)
  }
derive instance newtypeInstanceGroupStatus :: Newtype InstanceGroupStatus _
derive instance repGenericInstanceGroupStatus :: Generic InstanceGroupStatus _
instance showInstanceGroupStatus :: Show InstanceGroupStatus where
  show = genericShow
instance decodeInstanceGroupStatus :: Decode InstanceGroupStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupStatus :: Encode InstanceGroupStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The timeline of the instance group lifecycle.</p>
newtype InstanceGroupTimeline = InstanceGroupTimeline 
  { "CreationDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeInstanceGroupTimeline :: Newtype InstanceGroupTimeline _
derive instance repGenericInstanceGroupTimeline :: Generic InstanceGroupTimeline _
instance showInstanceGroupTimeline :: Show InstanceGroupTimeline where
  show = genericShow
instance decodeInstanceGroupTimeline :: Decode InstanceGroupTimeline where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupTimeline :: Encode InstanceGroupTimeline where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupType = InstanceGroupType String
derive instance newtypeInstanceGroupType :: Newtype InstanceGroupType _
derive instance repGenericInstanceGroupType :: Generic InstanceGroupType _
instance showInstanceGroupType :: Show InstanceGroupType where
  show = genericShow
instance decodeInstanceGroupType :: Decode InstanceGroupType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupType :: Encode InstanceGroupType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceGroupTypeList = InstanceGroupTypeList (Array InstanceGroupType)
derive instance newtypeInstanceGroupTypeList :: Newtype InstanceGroupTypeList _
derive instance repGenericInstanceGroupTypeList :: Generic InstanceGroupTypeList _
instance showInstanceGroupTypeList :: Show InstanceGroupTypeList where
  show = genericShow
instance decodeInstanceGroupTypeList :: Decode InstanceGroupTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceGroupTypeList :: Encode InstanceGroupTypeList where
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


newtype InstanceList = InstanceList (Array Instance)
derive instance newtypeInstanceList :: Newtype InstanceList _
derive instance repGenericInstanceList :: Generic InstanceList _
instance showInstanceList :: Show InstanceList where
  show = genericShow
instance decodeInstanceList :: Decode InstanceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceList :: Encode InstanceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.</p>
newtype InstanceResizePolicy = InstanceResizePolicy 
  { "InstancesToTerminate" :: NullOrUndefined.NullOrUndefined (EC2InstanceIdsList)
  , "InstancesToProtect" :: NullOrUndefined.NullOrUndefined (EC2InstanceIdsList)
  , "InstanceTerminationTimeout" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeInstanceResizePolicy :: Newtype InstanceResizePolicy _
derive instance repGenericInstanceResizePolicy :: Generic InstanceResizePolicy _
instance showInstanceResizePolicy :: Show InstanceResizePolicy where
  show = genericShow
instance decodeInstanceResizePolicy :: Decode InstanceResizePolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceResizePolicy :: Encode InstanceResizePolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceRoleType = InstanceRoleType String
derive instance newtypeInstanceRoleType :: Newtype InstanceRoleType _
derive instance repGenericInstanceRoleType :: Generic InstanceRoleType _
instance showInstanceRoleType :: Show InstanceRoleType where
  show = genericShow
instance decodeInstanceRoleType :: Decode InstanceRoleType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceRoleType :: Encode InstanceRoleType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceState = InstanceState String
derive instance newtypeInstanceState :: Newtype InstanceState _
derive instance repGenericInstanceState :: Generic InstanceState _
instance showInstanceState :: Show InstanceState where
  show = genericShow
instance decodeInstanceState :: Decode InstanceState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceState :: Encode InstanceState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The details of the status change reason for the instance.</p>
newtype InstanceStateChangeReason = InstanceStateChangeReason 
  { "Code" :: NullOrUndefined.NullOrUndefined (InstanceStateChangeReasonCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInstanceStateChangeReason :: Newtype InstanceStateChangeReason _
derive instance repGenericInstanceStateChangeReason :: Generic InstanceStateChangeReason _
instance showInstanceStateChangeReason :: Show InstanceStateChangeReason where
  show = genericShow
instance decodeInstanceStateChangeReason :: Decode InstanceStateChangeReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceStateChangeReason :: Encode InstanceStateChangeReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceStateChangeReasonCode = InstanceStateChangeReasonCode String
derive instance newtypeInstanceStateChangeReasonCode :: Newtype InstanceStateChangeReasonCode _
derive instance repGenericInstanceStateChangeReasonCode :: Generic InstanceStateChangeReasonCode _
instance showInstanceStateChangeReasonCode :: Show InstanceStateChangeReasonCode where
  show = genericShow
instance decodeInstanceStateChangeReasonCode :: Decode InstanceStateChangeReasonCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceStateChangeReasonCode :: Encode InstanceStateChangeReasonCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceStateList = InstanceStateList (Array InstanceState)
derive instance newtypeInstanceStateList :: Newtype InstanceStateList _
derive instance repGenericInstanceStateList :: Generic InstanceStateList _
instance showInstanceStateList :: Show InstanceStateList where
  show = genericShow
instance decodeInstanceStateList :: Decode InstanceStateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceStateList :: Encode InstanceStateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The instance status details.</p>
newtype InstanceStatus = InstanceStatus 
  { "State" :: NullOrUndefined.NullOrUndefined (InstanceState)
  , "StateChangeReason" :: NullOrUndefined.NullOrUndefined (InstanceStateChangeReason)
  , "Timeline" :: NullOrUndefined.NullOrUndefined (InstanceTimeline)
  }
derive instance newtypeInstanceStatus :: Newtype InstanceStatus _
derive instance repGenericInstanceStatus :: Generic InstanceStatus _
instance showInstanceStatus :: Show InstanceStatus where
  show = genericShow
instance decodeInstanceStatus :: Decode InstanceStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceStatus :: Encode InstanceStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The timeline of the instance lifecycle.</p>
newtype InstanceTimeline = InstanceTimeline 
  { "CreationDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeInstanceTimeline :: Newtype InstanceTimeline _
derive instance repGenericInstanceTimeline :: Generic InstanceTimeline _
instance showInstanceTimeline :: Show InstanceTimeline where
  show = genericShow
instance decodeInstanceTimeline :: Decode InstanceTimeline where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceTimeline :: Encode InstanceTimeline where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceType = InstanceType String
derive instance newtypeInstanceType :: Newtype InstanceType _
derive instance repGenericInstanceType :: Generic InstanceType _
instance showInstanceType :: Show InstanceType where
  show = genericShow
instance decodeInstanceType :: Decode InstanceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceType :: Encode InstanceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An instance type configuration for each instance type in an instance fleet, which determines the EC2 instances Amazon EMR attempts to provision to fulfill On-Demand and Spot target capacities. There can be a maximum of 5 instance type configurations in a fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceTypeConfig = InstanceTypeConfig 
  { "InstanceType" :: (InstanceType)
  , "WeightedCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "BidPrice" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "BidPriceAsPercentageOfOnDemandPrice" :: NullOrUndefined.NullOrUndefined (NonNegativeDouble)
  , "EbsConfiguration" :: NullOrUndefined.NullOrUndefined (EbsConfiguration)
  , "Configurations" :: NullOrUndefined.NullOrUndefined (ConfigurationList)
  }
derive instance newtypeInstanceTypeConfig :: Newtype InstanceTypeConfig _
derive instance repGenericInstanceTypeConfig :: Generic InstanceTypeConfig _
instance showInstanceTypeConfig :: Show InstanceTypeConfig where
  show = genericShow
instance decodeInstanceTypeConfig :: Decode InstanceTypeConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceTypeConfig :: Encode InstanceTypeConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceTypeConfigList = InstanceTypeConfigList (Array InstanceTypeConfig)
derive instance newtypeInstanceTypeConfigList :: Newtype InstanceTypeConfigList _
derive instance repGenericInstanceTypeConfigList :: Generic InstanceTypeConfigList _
instance showInstanceTypeConfigList :: Show InstanceTypeConfigList where
  show = genericShow
instance decodeInstanceTypeConfigList :: Decode InstanceTypeConfigList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceTypeConfigList :: Encode InstanceTypeConfigList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configuration specification for each instance type in an instance fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceTypeSpecification = InstanceTypeSpecification 
  { "InstanceType" :: NullOrUndefined.NullOrUndefined (InstanceType)
  , "WeightedCapacity" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  , "BidPrice" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "BidPriceAsPercentageOfOnDemandPrice" :: NullOrUndefined.NullOrUndefined (NonNegativeDouble)
  , "Configurations" :: NullOrUndefined.NullOrUndefined (ConfigurationList)
  , "EbsBlockDevices" :: NullOrUndefined.NullOrUndefined (EbsBlockDeviceList)
  , "EbsOptimized" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  }
derive instance newtypeInstanceTypeSpecification :: Newtype InstanceTypeSpecification _
derive instance repGenericInstanceTypeSpecification :: Generic InstanceTypeSpecification _
instance showInstanceTypeSpecification :: Show InstanceTypeSpecification where
  show = genericShow
instance decodeInstanceTypeSpecification :: Decode InstanceTypeSpecification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceTypeSpecification :: Encode InstanceTypeSpecification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceTypeSpecificationList = InstanceTypeSpecificationList (Array InstanceTypeSpecification)
derive instance newtypeInstanceTypeSpecificationList :: Newtype InstanceTypeSpecificationList _
derive instance repGenericInstanceTypeSpecificationList :: Generic InstanceTypeSpecificationList _
instance showInstanceTypeSpecificationList :: Show InstanceTypeSpecificationList where
  show = genericShow
instance decodeInstanceTypeSpecificationList :: Decode InstanceTypeSpecificationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceTypeSpecificationList :: Encode InstanceTypeSpecificationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates that an error occurred while processing the request and that the request was not completed.</p>
newtype InternalServerError = InternalServerError Types.NoArguments
derive instance newtypeInternalServerError :: Newtype InternalServerError _
derive instance repGenericInternalServerError :: Generic InternalServerError _
instance showInternalServerError :: Show InternalServerError where
  show = genericShow
instance decodeInternalServerError :: Decode InternalServerError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerError :: Encode InternalServerError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception occurs when there is an internal failure in the EMR service.</p>
newtype InternalServerException = InternalServerException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerException :: Newtype InternalServerException _
derive instance repGenericInternalServerException :: Generic InternalServerException _
instance showInternalServerException :: Show InternalServerException where
  show = genericShow
instance decodeInternalServerException :: Decode InternalServerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerException :: Encode InternalServerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception occurs when there is something wrong with user input.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "ErrorCode" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _
derive instance repGenericInvalidRequestException :: Generic InvalidRequestException _
instance showInvalidRequestException :: Show InvalidRequestException where
  show = genericShow
instance decodeInvalidRequestException :: Decode InvalidRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequestException :: Encode InvalidRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A description of a cluster (job flow).</p>
newtype JobFlowDetail = JobFlowDetail 
  { "JobFlowId" :: (XmlStringMaxLen256)
  , "Name" :: (XmlStringMaxLen256)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "AmiVersion" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "ExecutionStatusDetail" :: (JobFlowExecutionStatusDetail)
  , "Instances" :: (JobFlowInstancesDetail)
  , "Steps" :: NullOrUndefined.NullOrUndefined (StepDetailList)
  , "BootstrapActions" :: NullOrUndefined.NullOrUndefined (BootstrapActionDetailList)
  , "SupportedProducts" :: NullOrUndefined.NullOrUndefined (SupportedProductsList)
  , "VisibleToAllUsers" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "JobFlowRole" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "ServiceRole" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "AutoScalingRole" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "ScaleDownBehavior" :: NullOrUndefined.NullOrUndefined (ScaleDownBehavior)
  }
derive instance newtypeJobFlowDetail :: Newtype JobFlowDetail _
derive instance repGenericJobFlowDetail :: Generic JobFlowDetail _
instance showJobFlowDetail :: Show JobFlowDetail where
  show = genericShow
instance decodeJobFlowDetail :: Decode JobFlowDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobFlowDetail :: Encode JobFlowDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobFlowDetailList = JobFlowDetailList (Array JobFlowDetail)
derive instance newtypeJobFlowDetailList :: Newtype JobFlowDetailList _
derive instance repGenericJobFlowDetailList :: Generic JobFlowDetailList _
instance showJobFlowDetailList :: Show JobFlowDetailList where
  show = genericShow
instance decodeJobFlowDetailList :: Decode JobFlowDetailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobFlowDetailList :: Encode JobFlowDetailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The type of instance.</p>
newtype JobFlowExecutionState = JobFlowExecutionState String
derive instance newtypeJobFlowExecutionState :: Newtype JobFlowExecutionState _
derive instance repGenericJobFlowExecutionState :: Generic JobFlowExecutionState _
instance showJobFlowExecutionState :: Show JobFlowExecutionState where
  show = genericShow
instance decodeJobFlowExecutionState :: Decode JobFlowExecutionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobFlowExecutionState :: Encode JobFlowExecutionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobFlowExecutionStateList = JobFlowExecutionStateList (Array JobFlowExecutionState)
derive instance newtypeJobFlowExecutionStateList :: Newtype JobFlowExecutionStateList _
derive instance repGenericJobFlowExecutionStateList :: Generic JobFlowExecutionStateList _
instance showJobFlowExecutionStateList :: Show JobFlowExecutionStateList where
  show = genericShow
instance decodeJobFlowExecutionStateList :: Decode JobFlowExecutionStateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobFlowExecutionStateList :: Encode JobFlowExecutionStateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the status of the cluster (job flow).</p>
newtype JobFlowExecutionStatusDetail = JobFlowExecutionStatusDetail 
  { "State" :: (JobFlowExecutionState)
  , "CreationDateTime" :: (Date)
  , "StartDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastStateChangeReason" :: NullOrUndefined.NullOrUndefined (XmlString)
  }
derive instance newtypeJobFlowExecutionStatusDetail :: Newtype JobFlowExecutionStatusDetail _
derive instance repGenericJobFlowExecutionStatusDetail :: Generic JobFlowExecutionStatusDetail _
instance showJobFlowExecutionStatusDetail :: Show JobFlowExecutionStatusDetail where
  show = genericShow
instance decodeJobFlowExecutionStatusDetail :: Decode JobFlowExecutionStatusDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobFlowExecutionStatusDetail :: Encode JobFlowExecutionStatusDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A description of the Amazon EC2 instance on which the cluster (job flow) runs. A valid JobFlowInstancesConfig must contain either InstanceGroups or InstanceFleets, which is the recommended configuration. They cannot be used together. You may also have MasterInstanceType, SlaveInstanceType, and InstanceCount (all three must be present), but we don't recommend this configuration.</p>
newtype JobFlowInstancesConfig = JobFlowInstancesConfig 
  { "MasterInstanceType" :: NullOrUndefined.NullOrUndefined (InstanceType)
  , "SlaveInstanceType" :: NullOrUndefined.NullOrUndefined (InstanceType)
  , "InstanceCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstanceGroups" :: NullOrUndefined.NullOrUndefined (InstanceGroupConfigList)
  , "InstanceFleets" :: NullOrUndefined.NullOrUndefined (InstanceFleetConfigList)
  , "Ec2KeyName" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Placement" :: NullOrUndefined.NullOrUndefined (PlacementType)
  , "KeepJobFlowAliveWhenNoSteps" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "TerminationProtected" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HadoopVersion" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Ec2SubnetId" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Ec2SubnetIds" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256List)
  , "EmrManagedMasterSecurityGroup" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "EmrManagedSlaveSecurityGroup" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "ServiceAccessSecurityGroup" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "AdditionalMasterSecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroupsList)
  , "AdditionalSlaveSecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroupsList)
  }
derive instance newtypeJobFlowInstancesConfig :: Newtype JobFlowInstancesConfig _
derive instance repGenericJobFlowInstancesConfig :: Generic JobFlowInstancesConfig _
instance showJobFlowInstancesConfig :: Show JobFlowInstancesConfig where
  show = genericShow
instance decodeJobFlowInstancesConfig :: Decode JobFlowInstancesConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobFlowInstancesConfig :: Encode JobFlowInstancesConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specify the type of Amazon EC2 instances that the cluster (job flow) runs on.</p>
newtype JobFlowInstancesDetail = JobFlowInstancesDetail 
  { "MasterInstanceType" :: (InstanceType)
  , "MasterPublicDnsName" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "MasterInstanceId" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "SlaveInstanceType" :: (InstanceType)
  , "InstanceCount" :: (Int)
  , "InstanceGroups" :: NullOrUndefined.NullOrUndefined (InstanceGroupDetailList)
  , "NormalizedInstanceHours" :: NullOrUndefined.NullOrUndefined (Int)
  , "Ec2KeyName" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Ec2SubnetId" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Placement" :: NullOrUndefined.NullOrUndefined (PlacementType)
  , "KeepJobFlowAliveWhenNoSteps" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "TerminationProtected" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HadoopVersion" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  }
derive instance newtypeJobFlowInstancesDetail :: Newtype JobFlowInstancesDetail _
derive instance repGenericJobFlowInstancesDetail :: Generic JobFlowInstancesDetail _
instance showJobFlowInstancesDetail :: Show JobFlowInstancesDetail where
  show = genericShow
instance decodeJobFlowInstancesDetail :: Decode JobFlowInstancesDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobFlowInstancesDetail :: Encode JobFlowInstancesDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html">Use Kerberos Authentication</a> in the <i>EMR Management Guide</i>.</p>
newtype KerberosAttributes = KerberosAttributes 
  { "Realm" :: (XmlStringMaxLen256)
  , "KdcAdminPassword" :: (XmlStringMaxLen256)
  , "CrossRealmTrustPrincipalPassword" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "ADDomainJoinUser" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "ADDomainJoinPassword" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  }
derive instance newtypeKerberosAttributes :: Newtype KerberosAttributes _
derive instance repGenericKerberosAttributes :: Generic KerberosAttributes _
instance showKerberosAttributes :: Show KerberosAttributes where
  show = genericShow
instance decodeKerberosAttributes :: Decode KerberosAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKerberosAttributes :: Encode KerberosAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A key value pair.</p>
newtype KeyValue = KeyValue 
  { "Key" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "Value" :: NullOrUndefined.NullOrUndefined (XmlString)
  }
derive instance newtypeKeyValue :: Newtype KeyValue _
derive instance repGenericKeyValue :: Generic KeyValue _
instance showKeyValue :: Show KeyValue where
  show = genericShow
instance decodeKeyValue :: Decode KeyValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyValue :: Encode KeyValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyValueList = KeyValueList (Array KeyValue)
derive instance newtypeKeyValueList :: Newtype KeyValueList _
derive instance repGenericKeyValueList :: Generic KeyValueList _
instance showKeyValueList :: Show KeyValueList where
  show = genericShow
instance decodeKeyValueList :: Decode KeyValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyValueList :: Encode KeyValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input determines which bootstrap actions to retrieve.</p>
newtype ListBootstrapActionsInput = ListBootstrapActionsInput 
  { "ClusterId" :: (ClusterId)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListBootstrapActionsInput :: Newtype ListBootstrapActionsInput _
derive instance repGenericListBootstrapActionsInput :: Generic ListBootstrapActionsInput _
instance showListBootstrapActionsInput :: Show ListBootstrapActionsInput where
  show = genericShow
instance decodeListBootstrapActionsInput :: Decode ListBootstrapActionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBootstrapActionsInput :: Encode ListBootstrapActionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This output contains the bootstrap actions detail.</p>
newtype ListBootstrapActionsOutput = ListBootstrapActionsOutput 
  { "BootstrapActions" :: NullOrUndefined.NullOrUndefined (CommandList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListBootstrapActionsOutput :: Newtype ListBootstrapActionsOutput _
derive instance repGenericListBootstrapActionsOutput :: Generic ListBootstrapActionsOutput _
instance showListBootstrapActionsOutput :: Show ListBootstrapActionsOutput where
  show = genericShow
instance decodeListBootstrapActionsOutput :: Decode ListBootstrapActionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBootstrapActionsOutput :: Encode ListBootstrapActionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input determines how the ListClusters action filters the list of clusters that it returns.</p>
newtype ListClustersInput = ListClustersInput 
  { "CreatedAfter" :: NullOrUndefined.NullOrUndefined (Date)
  , "CreatedBefore" :: NullOrUndefined.NullOrUndefined (Date)
  , "ClusterStates" :: NullOrUndefined.NullOrUndefined (ClusterStateList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListClustersInput :: Newtype ListClustersInput _
derive instance repGenericListClustersInput :: Generic ListClustersInput _
instance showListClustersInput :: Show ListClustersInput where
  show = genericShow
instance decodeListClustersInput :: Decode ListClustersInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListClustersInput :: Encode ListClustersInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This contains a ClusterSummaryList with the cluster details; for example, the cluster IDs, names, and status.</p>
newtype ListClustersOutput = ListClustersOutput 
  { "Clusters" :: NullOrUndefined.NullOrUndefined (ClusterSummaryList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListClustersOutput :: Newtype ListClustersOutput _
derive instance repGenericListClustersOutput :: Generic ListClustersOutput _
instance showListClustersOutput :: Show ListClustersOutput where
  show = genericShow
instance decodeListClustersOutput :: Decode ListClustersOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListClustersOutput :: Encode ListClustersOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListInstanceFleetsInput = ListInstanceFleetsInput 
  { "ClusterId" :: (ClusterId)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListInstanceFleetsInput :: Newtype ListInstanceFleetsInput _
derive instance repGenericListInstanceFleetsInput :: Generic ListInstanceFleetsInput _
instance showListInstanceFleetsInput :: Show ListInstanceFleetsInput where
  show = genericShow
instance decodeListInstanceFleetsInput :: Decode ListInstanceFleetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInstanceFleetsInput :: Encode ListInstanceFleetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListInstanceFleetsOutput = ListInstanceFleetsOutput 
  { "InstanceFleets" :: NullOrUndefined.NullOrUndefined (InstanceFleetList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListInstanceFleetsOutput :: Newtype ListInstanceFleetsOutput _
derive instance repGenericListInstanceFleetsOutput :: Generic ListInstanceFleetsOutput _
instance showListInstanceFleetsOutput :: Show ListInstanceFleetsOutput where
  show = genericShow
instance decodeListInstanceFleetsOutput :: Decode ListInstanceFleetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInstanceFleetsOutput :: Encode ListInstanceFleetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input determines which instance groups to retrieve.</p>
newtype ListInstanceGroupsInput = ListInstanceGroupsInput 
  { "ClusterId" :: (ClusterId)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListInstanceGroupsInput :: Newtype ListInstanceGroupsInput _
derive instance repGenericListInstanceGroupsInput :: Generic ListInstanceGroupsInput _
instance showListInstanceGroupsInput :: Show ListInstanceGroupsInput where
  show = genericShow
instance decodeListInstanceGroupsInput :: Decode ListInstanceGroupsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInstanceGroupsInput :: Encode ListInstanceGroupsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input determines which instance groups to retrieve.</p>
newtype ListInstanceGroupsOutput = ListInstanceGroupsOutput 
  { "InstanceGroups" :: NullOrUndefined.NullOrUndefined (InstanceGroupList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListInstanceGroupsOutput :: Newtype ListInstanceGroupsOutput _
derive instance repGenericListInstanceGroupsOutput :: Generic ListInstanceGroupsOutput _
instance showListInstanceGroupsOutput :: Show ListInstanceGroupsOutput where
  show = genericShow
instance decodeListInstanceGroupsOutput :: Decode ListInstanceGroupsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInstanceGroupsOutput :: Encode ListInstanceGroupsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input determines which instances to list.</p>
newtype ListInstancesInput = ListInstancesInput 
  { "ClusterId" :: (ClusterId)
  , "InstanceGroupId" :: NullOrUndefined.NullOrUndefined (InstanceGroupId)
  , "InstanceGroupTypes" :: NullOrUndefined.NullOrUndefined (InstanceGroupTypeList)
  , "InstanceFleetId" :: NullOrUndefined.NullOrUndefined (InstanceFleetId)
  , "InstanceFleetType" :: NullOrUndefined.NullOrUndefined (InstanceFleetType)
  , "InstanceStates" :: NullOrUndefined.NullOrUndefined (InstanceStateList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListInstancesInput :: Newtype ListInstancesInput _
derive instance repGenericListInstancesInput :: Generic ListInstancesInput _
instance showListInstancesInput :: Show ListInstancesInput where
  show = genericShow
instance decodeListInstancesInput :: Decode ListInstancesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInstancesInput :: Encode ListInstancesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This output contains the list of instances.</p>
newtype ListInstancesOutput = ListInstancesOutput 
  { "Instances" :: NullOrUndefined.NullOrUndefined (InstanceList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListInstancesOutput :: Newtype ListInstancesOutput _
derive instance repGenericListInstancesOutput :: Generic ListInstancesOutput _
instance showListInstancesOutput :: Show ListInstancesOutput where
  show = genericShow
instance decodeListInstancesOutput :: Decode ListInstancesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListInstancesOutput :: Encode ListInstancesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSecurityConfigurationsInput = ListSecurityConfigurationsInput 
  { "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListSecurityConfigurationsInput :: Newtype ListSecurityConfigurationsInput _
derive instance repGenericListSecurityConfigurationsInput :: Generic ListSecurityConfigurationsInput _
instance showListSecurityConfigurationsInput :: Show ListSecurityConfigurationsInput where
  show = genericShow
instance decodeListSecurityConfigurationsInput :: Decode ListSecurityConfigurationsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSecurityConfigurationsInput :: Encode ListSecurityConfigurationsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSecurityConfigurationsOutput = ListSecurityConfigurationsOutput 
  { "SecurityConfigurations" :: NullOrUndefined.NullOrUndefined (SecurityConfigurationList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListSecurityConfigurationsOutput :: Newtype ListSecurityConfigurationsOutput _
derive instance repGenericListSecurityConfigurationsOutput :: Generic ListSecurityConfigurationsOutput _
instance showListSecurityConfigurationsOutput :: Show ListSecurityConfigurationsOutput where
  show = genericShow
instance decodeListSecurityConfigurationsOutput :: Decode ListSecurityConfigurationsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSecurityConfigurationsOutput :: Encode ListSecurityConfigurationsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input determines which steps to list.</p>
newtype ListStepsInput = ListStepsInput 
  { "ClusterId" :: (ClusterId)
  , "StepStates" :: NullOrUndefined.NullOrUndefined (StepStateList)
  , "StepIds" :: NullOrUndefined.NullOrUndefined (XmlStringList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListStepsInput :: Newtype ListStepsInput _
derive instance repGenericListStepsInput :: Generic ListStepsInput _
instance showListStepsInput :: Show ListStepsInput where
  show = genericShow
instance decodeListStepsInput :: Decode ListStepsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStepsInput :: Encode ListStepsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This output contains the list of steps returned in reverse order. This means that the last step is the first element in the list.</p>
newtype ListStepsOutput = ListStepsOutput 
  { "Steps" :: NullOrUndefined.NullOrUndefined (StepSummaryList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeListStepsOutput :: Newtype ListStepsOutput _
derive instance repGenericListStepsOutput :: Generic ListStepsOutput _
instance showListStepsOutput :: Show ListStepsOutput where
  show = genericShow
instance decodeListStepsOutput :: Decode ListStepsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStepsOutput :: Encode ListStepsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _
derive instance repGenericMarker :: Generic Marker _
instance showMarker :: Show Marker where
  show = genericShow
instance decodeMarker :: Decode Marker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarker :: Encode Marker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MarketType = MarketType String
derive instance newtypeMarketType :: Newtype MarketType _
derive instance repGenericMarketType :: Generic MarketType _
instance showMarketType :: Show MarketType where
  show = genericShow
instance decodeMarketType :: Decode MarketType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarketType :: Encode MarketType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A CloudWatch dimension, which is specified using a <code>Key</code> (known as a <code>Name</code> in CloudWatch), <code>Value</code> pair. By default, Amazon EMR uses one dimension whose <code>Key</code> is <code>JobFlowID</code> and <code>Value</code> is a variable representing the cluster ID, which is <code>${emr.clusterId}</code>. This enables the rule to bootstrap when the cluster ID becomes available.</p>
newtype MetricDimension = MetricDimension 
  { "Key" :: NullOrUndefined.NullOrUndefined (String)
  , "Value" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMetricDimension :: Newtype MetricDimension _
derive instance repGenericMetricDimension :: Generic MetricDimension _
instance showMetricDimension :: Show MetricDimension where
  show = genericShow
instance decodeMetricDimension :: Decode MetricDimension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricDimension :: Encode MetricDimension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricDimensionList = MetricDimensionList (Array MetricDimension)
derive instance newtypeMetricDimensionList :: Newtype MetricDimensionList _
derive instance repGenericMetricDimensionList :: Generic MetricDimensionList _
instance showMetricDimensionList :: Show MetricDimensionList where
  show = genericShow
instance decodeMetricDimensionList :: Decode MetricDimensionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricDimensionList :: Encode MetricDimensionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyInstanceFleetInput = ModifyInstanceFleetInput 
  { "ClusterId" :: (ClusterId)
  , "InstanceFleet" :: (InstanceFleetModifyConfig)
  }
derive instance newtypeModifyInstanceFleetInput :: Newtype ModifyInstanceFleetInput _
derive instance repGenericModifyInstanceFleetInput :: Generic ModifyInstanceFleetInput _
instance showModifyInstanceFleetInput :: Show ModifyInstanceFleetInput where
  show = genericShow
instance decodeModifyInstanceFleetInput :: Decode ModifyInstanceFleetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyInstanceFleetInput :: Encode ModifyInstanceFleetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Change the size of some instance groups.</p>
newtype ModifyInstanceGroupsInput = ModifyInstanceGroupsInput 
  { "ClusterId" :: NullOrUndefined.NullOrUndefined (ClusterId)
  , "InstanceGroups" :: NullOrUndefined.NullOrUndefined (InstanceGroupModifyConfigList)
  }
derive instance newtypeModifyInstanceGroupsInput :: Newtype ModifyInstanceGroupsInput _
derive instance repGenericModifyInstanceGroupsInput :: Generic ModifyInstanceGroupsInput _
instance showModifyInstanceGroupsInput :: Show ModifyInstanceGroupsInput where
  show = genericShow
instance decodeModifyInstanceGroupsInput :: Decode ModifyInstanceGroupsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyInstanceGroupsInput :: Encode ModifyInstanceGroupsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NewSupportedProductsList = NewSupportedProductsList (Array SupportedProductConfig)
derive instance newtypeNewSupportedProductsList :: Newtype NewSupportedProductsList _
derive instance repGenericNewSupportedProductsList :: Generic NewSupportedProductsList _
instance showNewSupportedProductsList :: Show NewSupportedProductsList where
  show = genericShow
instance decodeNewSupportedProductsList :: Decode NewSupportedProductsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNewSupportedProductsList :: Encode NewSupportedProductsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonNegativeDouble = NonNegativeDouble Number
derive instance newtypeNonNegativeDouble :: Newtype NonNegativeDouble _
derive instance repGenericNonNegativeDouble :: Generic NonNegativeDouble _
instance showNonNegativeDouble :: Show NonNegativeDouble where
  show = genericShow
instance decodeNonNegativeDouble :: Decode NonNegativeDouble where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonNegativeDouble :: Encode NonNegativeDouble where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon EC2 Availability Zone configuration of the cluster (job flow).</p>
newtype PlacementType = PlacementType 
  { "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256List)
  }
derive instance newtypePlacementType :: Newtype PlacementType _
derive instance repGenericPlacementType :: Generic PlacementType _
instance showPlacementType :: Show PlacementType where
  show = genericShow
instance decodePlacementType :: Decode PlacementType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlacementType :: Encode PlacementType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutAutoScalingPolicyInput = PutAutoScalingPolicyInput 
  { "ClusterId" :: (ClusterId)
  , "InstanceGroupId" :: (InstanceGroupId)
  , "AutoScalingPolicy" :: (AutoScalingPolicy)
  }
derive instance newtypePutAutoScalingPolicyInput :: Newtype PutAutoScalingPolicyInput _
derive instance repGenericPutAutoScalingPolicyInput :: Generic PutAutoScalingPolicyInput _
instance showPutAutoScalingPolicyInput :: Show PutAutoScalingPolicyInput where
  show = genericShow
instance decodePutAutoScalingPolicyInput :: Decode PutAutoScalingPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutAutoScalingPolicyInput :: Encode PutAutoScalingPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutAutoScalingPolicyOutput = PutAutoScalingPolicyOutput 
  { "ClusterId" :: NullOrUndefined.NullOrUndefined (ClusterId)
  , "InstanceGroupId" :: NullOrUndefined.NullOrUndefined (InstanceGroupId)
  , "AutoScalingPolicy" :: NullOrUndefined.NullOrUndefined (AutoScalingPolicyDescription)
  }
derive instance newtypePutAutoScalingPolicyOutput :: Newtype PutAutoScalingPolicyOutput _
derive instance repGenericPutAutoScalingPolicyOutput :: Generic PutAutoScalingPolicyOutput _
instance showPutAutoScalingPolicyOutput :: Show PutAutoScalingPolicyOutput where
  show = genericShow
instance decodePutAutoScalingPolicyOutput :: Decode PutAutoScalingPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutAutoScalingPolicyOutput :: Encode PutAutoScalingPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveAutoScalingPolicyInput = RemoveAutoScalingPolicyInput 
  { "ClusterId" :: (ClusterId)
  , "InstanceGroupId" :: (InstanceGroupId)
  }
derive instance newtypeRemoveAutoScalingPolicyInput :: Newtype RemoveAutoScalingPolicyInput _
derive instance repGenericRemoveAutoScalingPolicyInput :: Generic RemoveAutoScalingPolicyInput _
instance showRemoveAutoScalingPolicyInput :: Show RemoveAutoScalingPolicyInput where
  show = genericShow
instance decodeRemoveAutoScalingPolicyInput :: Decode RemoveAutoScalingPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveAutoScalingPolicyInput :: Encode RemoveAutoScalingPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveAutoScalingPolicyOutput = RemoveAutoScalingPolicyOutput Types.NoArguments
derive instance newtypeRemoveAutoScalingPolicyOutput :: Newtype RemoveAutoScalingPolicyOutput _
derive instance repGenericRemoveAutoScalingPolicyOutput :: Generic RemoveAutoScalingPolicyOutput _
instance showRemoveAutoScalingPolicyOutput :: Show RemoveAutoScalingPolicyOutput where
  show = genericShow
instance decodeRemoveAutoScalingPolicyOutput :: Decode RemoveAutoScalingPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveAutoScalingPolicyOutput :: Encode RemoveAutoScalingPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This input identifies a cluster and a list of tags to remove.</p>
newtype RemoveTagsInput = RemoveTagsInput 
  { "ResourceId" :: (ResourceId)
  , "TagKeys" :: (StringList)
  }
derive instance newtypeRemoveTagsInput :: Newtype RemoveTagsInput _
derive instance repGenericRemoveTagsInput :: Generic RemoveTagsInput _
instance showRemoveTagsInput :: Show RemoveTagsInput where
  show = genericShow
instance decodeRemoveTagsInput :: Decode RemoveTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsInput :: Encode RemoveTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This output indicates the result of removing tags from a resource.</p>
newtype RemoveTagsOutput = RemoveTagsOutput Types.NoArguments
derive instance newtypeRemoveTagsOutput :: Newtype RemoveTagsOutput _
derive instance repGenericRemoveTagsOutput :: Generic RemoveTagsOutput _
instance showRemoveTagsOutput :: Show RemoveTagsOutput where
  show = genericShow
instance decodeRemoveTagsOutput :: Decode RemoveTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsOutput :: Encode RemoveTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RepoUpgradeOnBoot = RepoUpgradeOnBoot String
derive instance newtypeRepoUpgradeOnBoot :: Newtype RepoUpgradeOnBoot _
derive instance repGenericRepoUpgradeOnBoot :: Generic RepoUpgradeOnBoot _
instance showRepoUpgradeOnBoot :: Show RepoUpgradeOnBoot where
  show = genericShow
instance decodeRepoUpgradeOnBoot :: Decode RepoUpgradeOnBoot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRepoUpgradeOnBoot :: Encode RepoUpgradeOnBoot where
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


-- | <p> Input to the <a>RunJobFlow</a> operation. </p>
newtype RunJobFlowInput = RunJobFlowInput 
  { "Name" :: (XmlStringMaxLen256)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "AdditionalInfo" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "AmiVersion" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "ReleaseLabel" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Instances" :: (JobFlowInstancesConfig)
  , "Steps" :: NullOrUndefined.NullOrUndefined (StepConfigList)
  , "BootstrapActions" :: NullOrUndefined.NullOrUndefined (BootstrapActionConfigList)
  , "SupportedProducts" :: NullOrUndefined.NullOrUndefined (SupportedProductsList)
  , "NewSupportedProducts" :: NullOrUndefined.NullOrUndefined (NewSupportedProductsList)
  , "Applications" :: NullOrUndefined.NullOrUndefined (ApplicationList)
  , "Configurations" :: NullOrUndefined.NullOrUndefined (ConfigurationList)
  , "VisibleToAllUsers" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "JobFlowRole" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "ServiceRole" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "SecurityConfiguration" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "AutoScalingRole" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "ScaleDownBehavior" :: NullOrUndefined.NullOrUndefined (ScaleDownBehavior)
  , "CustomAmiId" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "EbsRootVolumeSize" :: NullOrUndefined.NullOrUndefined (Int)
  , "RepoUpgradeOnBoot" :: NullOrUndefined.NullOrUndefined (RepoUpgradeOnBoot)
  , "KerberosAttributes" :: NullOrUndefined.NullOrUndefined (KerberosAttributes)
  }
derive instance newtypeRunJobFlowInput :: Newtype RunJobFlowInput _
derive instance repGenericRunJobFlowInput :: Generic RunJobFlowInput _
instance showRunJobFlowInput :: Show RunJobFlowInput where
  show = genericShow
instance decodeRunJobFlowInput :: Decode RunJobFlowInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunJobFlowInput :: Encode RunJobFlowInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The result of the <a>RunJobFlow</a> operation. </p>
newtype RunJobFlowOutput = RunJobFlowOutput 
  { "JobFlowId" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  }
derive instance newtypeRunJobFlowOutput :: Newtype RunJobFlowOutput _
derive instance repGenericRunJobFlowOutput :: Generic RunJobFlowOutput _
instance showRunJobFlowOutput :: Show RunJobFlowOutput where
  show = genericShow
instance decodeRunJobFlowOutput :: Decode RunJobFlowOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunJobFlowOutput :: Encode RunJobFlowOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScaleDownBehavior = ScaleDownBehavior String
derive instance newtypeScaleDownBehavior :: Newtype ScaleDownBehavior _
derive instance repGenericScaleDownBehavior :: Generic ScaleDownBehavior _
instance showScaleDownBehavior :: Show ScaleDownBehavior where
  show = genericShow
instance decodeScaleDownBehavior :: Decode ScaleDownBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScaleDownBehavior :: Encode ScaleDownBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.</p>
newtype ScalingAction = ScalingAction 
  { "Market" :: NullOrUndefined.NullOrUndefined (MarketType)
  , "SimpleScalingPolicyConfiguration" :: (SimpleScalingPolicyConfiguration)
  }
derive instance newtypeScalingAction :: Newtype ScalingAction _
derive instance repGenericScalingAction :: Generic ScalingAction _
instance showScalingAction :: Show ScalingAction where
  show = genericShow
instance decodeScalingAction :: Decode ScalingAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingAction :: Encode ScalingAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activities triggered by automatic scaling rules will not cause an instance group to grow above or below these limits.</p>
newtype ScalingConstraints = ScalingConstraints 
  { "MinCapacity" :: (Int)
  , "MaxCapacity" :: (Int)
  }
derive instance newtypeScalingConstraints :: Newtype ScalingConstraints _
derive instance repGenericScalingConstraints :: Generic ScalingConstraints _
instance showScalingConstraints :: Show ScalingConstraints where
  show = genericShow
instance decodeScalingConstraints :: Decode ScalingConstraints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingConstraints :: Encode ScalingConstraints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A scale-in or scale-out rule that defines scaling activity, including the CloudWatch metric alarm that triggers activity, how EC2 instances are added or removed, and the periodicity of adjustments. The automatic scaling policy for an instance group can comprise one or more automatic scaling rules.</p>
newtype ScalingRule = ScalingRule 
  { "Name" :: (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (String)
  , "Action" :: (ScalingAction)
  , "Trigger" :: (ScalingTrigger)
  }
derive instance newtypeScalingRule :: Newtype ScalingRule _
derive instance repGenericScalingRule :: Generic ScalingRule _
instance showScalingRule :: Show ScalingRule where
  show = genericShow
instance decodeScalingRule :: Decode ScalingRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingRule :: Encode ScalingRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScalingRuleList = ScalingRuleList (Array ScalingRule)
derive instance newtypeScalingRuleList :: Newtype ScalingRuleList _
derive instance repGenericScalingRuleList :: Generic ScalingRuleList _
instance showScalingRuleList :: Show ScalingRuleList where
  show = genericShow
instance decodeScalingRuleList :: Decode ScalingRuleList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingRuleList :: Encode ScalingRuleList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The conditions that trigger an automatic scaling activity.</p>
newtype ScalingTrigger = ScalingTrigger 
  { "CloudWatchAlarmDefinition" :: (CloudWatchAlarmDefinition)
  }
derive instance newtypeScalingTrigger :: Newtype ScalingTrigger _
derive instance repGenericScalingTrigger :: Generic ScalingTrigger _
instance showScalingTrigger :: Show ScalingTrigger where
  show = genericShow
instance decodeScalingTrigger :: Decode ScalingTrigger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalingTrigger :: Encode ScalingTrigger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configuration of the script to run during a bootstrap action.</p>
newtype ScriptBootstrapActionConfig = ScriptBootstrapActionConfig 
  { "Path" :: (XmlString)
  , "Args" :: NullOrUndefined.NullOrUndefined (XmlStringList)
  }
derive instance newtypeScriptBootstrapActionConfig :: Newtype ScriptBootstrapActionConfig _
derive instance repGenericScriptBootstrapActionConfig :: Generic ScriptBootstrapActionConfig _
instance showScriptBootstrapActionConfig :: Show ScriptBootstrapActionConfig where
  show = genericShow
instance decodeScriptBootstrapActionConfig :: Decode ScriptBootstrapActionConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScriptBootstrapActionConfig :: Encode ScriptBootstrapActionConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityConfigurationList = SecurityConfigurationList (Array SecurityConfigurationSummary)
derive instance newtypeSecurityConfigurationList :: Newtype SecurityConfigurationList _
derive instance repGenericSecurityConfigurationList :: Generic SecurityConfigurationList _
instance showSecurityConfigurationList :: Show SecurityConfigurationList where
  show = genericShow
instance decodeSecurityConfigurationList :: Decode SecurityConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityConfigurationList :: Encode SecurityConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The creation date and time, and name, of a security configuration.</p>
newtype SecurityConfigurationSummary = SecurityConfigurationSummary 
  { "Name" :: NullOrUndefined.NullOrUndefined (XmlString)
  , "CreationDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeSecurityConfigurationSummary :: Newtype SecurityConfigurationSummary _
derive instance repGenericSecurityConfigurationSummary :: Generic SecurityConfigurationSummary _
instance showSecurityConfigurationSummary :: Show SecurityConfigurationSummary where
  show = genericShow
instance decodeSecurityConfigurationSummary :: Decode SecurityConfigurationSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityConfigurationSummary :: Encode SecurityConfigurationSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroupsList = SecurityGroupsList (Array XmlStringMaxLen256)
derive instance newtypeSecurityGroupsList :: Newtype SecurityGroupsList _
derive instance repGenericSecurityGroupsList :: Generic SecurityGroupsList _
instance showSecurityGroupsList :: Show SecurityGroupsList where
  show = genericShow
instance decodeSecurityGroupsList :: Decode SecurityGroupsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupsList :: Encode SecurityGroupsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The input argument to the <a>TerminationProtection</a> operation. </p>
newtype SetTerminationProtectionInput = SetTerminationProtectionInput 
  { "JobFlowIds" :: (XmlStringList)
  , "TerminationProtected" :: (Boolean)
  }
derive instance newtypeSetTerminationProtectionInput :: Newtype SetTerminationProtectionInput _
derive instance repGenericSetTerminationProtectionInput :: Generic SetTerminationProtectionInput _
instance showSetTerminationProtectionInput :: Show SetTerminationProtectionInput where
  show = genericShow
instance decodeSetTerminationProtectionInput :: Decode SetTerminationProtectionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetTerminationProtectionInput :: Encode SetTerminationProtectionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input to the SetVisibleToAllUsers action.</p>
newtype SetVisibleToAllUsersInput = SetVisibleToAllUsersInput 
  { "JobFlowIds" :: (XmlStringList)
  , "VisibleToAllUsers" :: (Boolean)
  }
derive instance newtypeSetVisibleToAllUsersInput :: Newtype SetVisibleToAllUsersInput _
derive instance repGenericSetVisibleToAllUsersInput :: Generic SetVisibleToAllUsersInput _
instance showSetVisibleToAllUsersInput :: Show SetVisibleToAllUsersInput where
  show = genericShow
instance decodeSetVisibleToAllUsersInput :: Decode SetVisibleToAllUsersInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetVisibleToAllUsersInput :: Encode SetVisibleToAllUsersInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Policy for customizing shrink operations. Allows configuration of decommissioning timeout and targeted instance shrinking.</p>
newtype ShrinkPolicy = ShrinkPolicy 
  { "DecommissionTimeout" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstanceResizePolicy" :: NullOrUndefined.NullOrUndefined (InstanceResizePolicy)
  }
derive instance newtypeShrinkPolicy :: Newtype ShrinkPolicy _
derive instance repGenericShrinkPolicy :: Generic ShrinkPolicy _
instance showShrinkPolicy :: Show ShrinkPolicy where
  show = genericShow
instance decodeShrinkPolicy :: Decode ShrinkPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShrinkPolicy :: Encode ShrinkPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An automatic scaling configuration, which describes how the policy adds or removes instances, the cooldown period, and the number of EC2 instances that will be added each time the CloudWatch metric alarm condition is satisfied.</p>
newtype SimpleScalingPolicyConfiguration = SimpleScalingPolicyConfiguration 
  { "AdjustmentType" :: NullOrUndefined.NullOrUndefined (AdjustmentType)
  , "ScalingAdjustment" :: (Int)
  , "CoolDown" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeSimpleScalingPolicyConfiguration :: Newtype SimpleScalingPolicyConfiguration _
derive instance repGenericSimpleScalingPolicyConfiguration :: Generic SimpleScalingPolicyConfiguration _
instance showSimpleScalingPolicyConfiguration :: Show SimpleScalingPolicyConfiguration where
  show = genericShow
instance decodeSimpleScalingPolicyConfiguration :: Decode SimpleScalingPolicyConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSimpleScalingPolicyConfiguration :: Encode SimpleScalingPolicyConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The launch specification for Spot instances in the instance fleet, which determines the defined duration and provisioning timeout behavior.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype SpotProvisioningSpecification = SpotProvisioningSpecification 
  { "TimeoutDurationMinutes" :: (WholeNumber)
  , "TimeoutAction" :: (SpotProvisioningTimeoutAction)
  , "BlockDurationMinutes" :: NullOrUndefined.NullOrUndefined (WholeNumber)
  }
derive instance newtypeSpotProvisioningSpecification :: Newtype SpotProvisioningSpecification _
derive instance repGenericSpotProvisioningSpecification :: Generic SpotProvisioningSpecification _
instance showSpotProvisioningSpecification :: Show SpotProvisioningSpecification where
  show = genericShow
instance decodeSpotProvisioningSpecification :: Decode SpotProvisioningSpecification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSpotProvisioningSpecification :: Encode SpotProvisioningSpecification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SpotProvisioningTimeoutAction = SpotProvisioningTimeoutAction String
derive instance newtypeSpotProvisioningTimeoutAction :: Newtype SpotProvisioningTimeoutAction _
derive instance repGenericSpotProvisioningTimeoutAction :: Generic SpotProvisioningTimeoutAction _
instance showSpotProvisioningTimeoutAction :: Show SpotProvisioningTimeoutAction where
  show = genericShow
instance decodeSpotProvisioningTimeoutAction :: Decode SpotProvisioningTimeoutAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSpotProvisioningTimeoutAction :: Encode SpotProvisioningTimeoutAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Statistic = Statistic String
derive instance newtypeStatistic :: Newtype Statistic _
derive instance repGenericStatistic :: Generic Statistic _
instance showStatistic :: Show Statistic where
  show = genericShow
instance decodeStatistic :: Decode Statistic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatistic :: Encode Statistic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This represents a step in a cluster.</p>
newtype Step = Step 
  { "Id" :: NullOrUndefined.NullOrUndefined (StepId)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Config" :: NullOrUndefined.NullOrUndefined (HadoopStepConfig)
  , "ActionOnFailure" :: NullOrUndefined.NullOrUndefined (ActionOnFailure)
  , "Status" :: NullOrUndefined.NullOrUndefined (StepStatus)
  }
derive instance newtypeStep :: Newtype Step _
derive instance repGenericStep :: Generic Step _
instance showStep :: Show Step where
  show = genericShow
instance decodeStep :: Decode Step where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStep :: Encode Step where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specification of a cluster (job flow) step.</p>
newtype StepConfig = StepConfig 
  { "Name" :: (XmlStringMaxLen256)
  , "ActionOnFailure" :: NullOrUndefined.NullOrUndefined (ActionOnFailure)
  , "HadoopJarStep" :: (HadoopJarStepConfig)
  }
derive instance newtypeStepConfig :: Newtype StepConfig _
derive instance repGenericStepConfig :: Generic StepConfig _
instance showStepConfig :: Show StepConfig where
  show = genericShow
instance decodeStepConfig :: Decode StepConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepConfig :: Encode StepConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepConfigList = StepConfigList (Array StepConfig)
derive instance newtypeStepConfigList :: Newtype StepConfigList _
derive instance repGenericStepConfigList :: Generic StepConfigList _
instance showStepConfigList :: Show StepConfigList where
  show = genericShow
instance decodeStepConfigList :: Decode StepConfigList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepConfigList :: Encode StepConfigList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Combines the execution state and configuration of a step.</p>
newtype StepDetail = StepDetail 
  { "StepConfig" :: (StepConfig)
  , "ExecutionStatusDetail" :: (StepExecutionStatusDetail)
  }
derive instance newtypeStepDetail :: Newtype StepDetail _
derive instance repGenericStepDetail :: Generic StepDetail _
instance showStepDetail :: Show StepDetail where
  show = genericShow
instance decodeStepDetail :: Decode StepDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepDetail :: Encode StepDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepDetailList = StepDetailList (Array StepDetail)
derive instance newtypeStepDetailList :: Newtype StepDetailList _
derive instance repGenericStepDetailList :: Generic StepDetailList _
instance showStepDetailList :: Show StepDetailList where
  show = genericShow
instance decodeStepDetailList :: Decode StepDetailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepDetailList :: Encode StepDetailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepExecutionState = StepExecutionState String
derive instance newtypeStepExecutionState :: Newtype StepExecutionState _
derive instance repGenericStepExecutionState :: Generic StepExecutionState _
instance showStepExecutionState :: Show StepExecutionState where
  show = genericShow
instance decodeStepExecutionState :: Decode StepExecutionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepExecutionState :: Encode StepExecutionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The execution state of a step.</p>
newtype StepExecutionStatusDetail = StepExecutionStatusDetail 
  { "State" :: (StepExecutionState)
  , "CreationDateTime" :: (Date)
  , "StartDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastStateChangeReason" :: NullOrUndefined.NullOrUndefined (XmlString)
  }
derive instance newtypeStepExecutionStatusDetail :: Newtype StepExecutionStatusDetail _
derive instance repGenericStepExecutionStatusDetail :: Generic StepExecutionStatusDetail _
instance showStepExecutionStatusDetail :: Show StepExecutionStatusDetail where
  show = genericShow
instance decodeStepExecutionStatusDetail :: Decode StepExecutionStatusDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepExecutionStatusDetail :: Encode StepExecutionStatusDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepId = StepId String
derive instance newtypeStepId :: Newtype StepId _
derive instance repGenericStepId :: Generic StepId _
instance showStepId :: Show StepId where
  show = genericShow
instance decodeStepId :: Decode StepId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepId :: Encode StepId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepIdsList = StepIdsList (Array XmlStringMaxLen256)
derive instance newtypeStepIdsList :: Newtype StepIdsList _
derive instance repGenericStepIdsList :: Generic StepIdsList _
instance showStepIdsList :: Show StepIdsList where
  show = genericShow
instance decodeStepIdsList :: Decode StepIdsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepIdsList :: Encode StepIdsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepState = StepState String
derive instance newtypeStepState :: Newtype StepState _
derive instance repGenericStepState :: Generic StepState _
instance showStepState :: Show StepState where
  show = genericShow
instance decodeStepState :: Decode StepState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepState :: Encode StepState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The details of the step state change reason.</p>
newtype StepStateChangeReason = StepStateChangeReason 
  { "Code" :: NullOrUndefined.NullOrUndefined (StepStateChangeReasonCode)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeStepStateChangeReason :: Newtype StepStateChangeReason _
derive instance repGenericStepStateChangeReason :: Generic StepStateChangeReason _
instance showStepStateChangeReason :: Show StepStateChangeReason where
  show = genericShow
instance decodeStepStateChangeReason :: Decode StepStateChangeReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepStateChangeReason :: Encode StepStateChangeReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepStateChangeReasonCode = StepStateChangeReasonCode String
derive instance newtypeStepStateChangeReasonCode :: Newtype StepStateChangeReasonCode _
derive instance repGenericStepStateChangeReasonCode :: Generic StepStateChangeReasonCode _
instance showStepStateChangeReasonCode :: Show StepStateChangeReasonCode where
  show = genericShow
instance decodeStepStateChangeReasonCode :: Decode StepStateChangeReasonCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepStateChangeReasonCode :: Encode StepStateChangeReasonCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepStateList = StepStateList (Array StepState)
derive instance newtypeStepStateList :: Newtype StepStateList _
derive instance repGenericStepStateList :: Generic StepStateList _
instance showStepStateList :: Show StepStateList where
  show = genericShow
instance decodeStepStateList :: Decode StepStateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepStateList :: Encode StepStateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The execution status details of the cluster step.</p>
newtype StepStatus = StepStatus 
  { "State" :: NullOrUndefined.NullOrUndefined (StepState)
  , "StateChangeReason" :: NullOrUndefined.NullOrUndefined (StepStateChangeReason)
  , "FailureDetails" :: NullOrUndefined.NullOrUndefined (FailureDetails)
  , "Timeline" :: NullOrUndefined.NullOrUndefined (StepTimeline)
  }
derive instance newtypeStepStatus :: Newtype StepStatus _
derive instance repGenericStepStatus :: Generic StepStatus _
instance showStepStatus :: Show StepStatus where
  show = genericShow
instance decodeStepStatus :: Decode StepStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepStatus :: Encode StepStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The summary of the cluster step.</p>
newtype StepSummary = StepSummary 
  { "Id" :: NullOrUndefined.NullOrUndefined (StepId)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Config" :: NullOrUndefined.NullOrUndefined (HadoopStepConfig)
  , "ActionOnFailure" :: NullOrUndefined.NullOrUndefined (ActionOnFailure)
  , "Status" :: NullOrUndefined.NullOrUndefined (StepStatus)
  }
derive instance newtypeStepSummary :: Newtype StepSummary _
derive instance repGenericStepSummary :: Generic StepSummary _
instance showStepSummary :: Show StepSummary where
  show = genericShow
instance decodeStepSummary :: Decode StepSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepSummary :: Encode StepSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StepSummaryList = StepSummaryList (Array StepSummary)
derive instance newtypeStepSummaryList :: Newtype StepSummaryList _
derive instance repGenericStepSummaryList :: Generic StepSummaryList _
instance showStepSummaryList :: Show StepSummaryList where
  show = genericShow
instance decodeStepSummaryList :: Decode StepSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepSummaryList :: Encode StepSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The timeline of the cluster step lifecycle.</p>
newtype StepTimeline = StepTimeline 
  { "CreationDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "StartDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeStepTimeline :: Newtype StepTimeline _
derive instance repGenericStepTimeline :: Generic StepTimeline _
instance showStepTimeline :: Show StepTimeline where
  show = genericShow
instance decodeStepTimeline :: Decode StepTimeline where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStepTimeline :: Encode StepTimeline where
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


newtype StringMap = StringMap (StrMap.StrMap String)
derive instance newtypeStringMap :: Newtype StringMap _
derive instance repGenericStringMap :: Generic StringMap _
instance showStringMap :: Show StringMap where
  show = genericShow
instance decodeStringMap :: Decode StringMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringMap :: Encode StringMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The list of supported product configurations which allow user-supplied arguments. EMR accepts these arguments and forwards them to the corresponding installation script as bootstrap action arguments.</p>
newtype SupportedProductConfig = SupportedProductConfig 
  { "Name" :: NullOrUndefined.NullOrUndefined (XmlStringMaxLen256)
  , "Args" :: NullOrUndefined.NullOrUndefined (XmlStringList)
  }
derive instance newtypeSupportedProductConfig :: Newtype SupportedProductConfig _
derive instance repGenericSupportedProductConfig :: Generic SupportedProductConfig _
instance showSupportedProductConfig :: Show SupportedProductConfig where
  show = genericShow
instance decodeSupportedProductConfig :: Decode SupportedProductConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupportedProductConfig :: Encode SupportedProductConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupportedProductsList = SupportedProductsList (Array XmlStringMaxLen256)
derive instance newtypeSupportedProductsList :: Newtype SupportedProductsList _
derive instance repGenericSupportedProductsList :: Generic SupportedProductsList _
instance showSupportedProductsList :: Show SupportedProductsList where
  show = genericShow
instance decodeSupportedProductsList :: Decode SupportedProductsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupportedProductsList :: Encode SupportedProductsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A key/value pair containing user-defined metadata that you can associate with an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html">Tag Clusters</a>. </p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined.NullOrUndefined (String)
  , "Value" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
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


-- | <p> Input to the <a>TerminateJobFlows</a> operation. </p>
newtype TerminateJobFlowsInput = TerminateJobFlowsInput 
  { "JobFlowIds" :: (XmlStringList)
  }
derive instance newtypeTerminateJobFlowsInput :: Newtype TerminateJobFlowsInput _
derive instance repGenericTerminateJobFlowsInput :: Generic TerminateJobFlowsInput _
instance showTerminateJobFlowsInput :: Show TerminateJobFlowsInput where
  show = genericShow
instance decodeTerminateJobFlowsInput :: Decode TerminateJobFlowsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateJobFlowsInput :: Encode TerminateJobFlowsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Unit'' = Unit'' String
derive instance newtypeUnit'' :: Newtype Unit'' _
derive instance repGenericUnit'' :: Generic Unit'' _
instance showUnit'' :: Show Unit'' where
  show = genericShow
instance decodeUnit'' :: Decode Unit'' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnit'' :: Encode Unit'' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.</p>
newtype VolumeSpecification = VolumeSpecification 
  { "VolumeType" :: (String)
  , "Iops" :: NullOrUndefined.NullOrUndefined (Int)
  , "SizeInGB" :: (Int)
  }
derive instance newtypeVolumeSpecification :: Newtype VolumeSpecification _
derive instance repGenericVolumeSpecification :: Generic VolumeSpecification _
instance showVolumeSpecification :: Show VolumeSpecification where
  show = genericShow
instance decodeVolumeSpecification :: Decode VolumeSpecification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeSpecification :: Encode VolumeSpecification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WholeNumber = WholeNumber Int
derive instance newtypeWholeNumber :: Newtype WholeNumber _
derive instance repGenericWholeNumber :: Generic WholeNumber _
instance showWholeNumber :: Show WholeNumber where
  show = genericShow
instance decodeWholeNumber :: Decode WholeNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWholeNumber :: Encode WholeNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype XmlString = XmlString String
derive instance newtypeXmlString :: Newtype XmlString _
derive instance repGenericXmlString :: Generic XmlString _
instance showXmlString :: Show XmlString where
  show = genericShow
instance decodeXmlString :: Decode XmlString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXmlString :: Encode XmlString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype XmlStringList = XmlStringList (Array XmlString)
derive instance newtypeXmlStringList :: Newtype XmlStringList _
derive instance repGenericXmlStringList :: Generic XmlStringList _
instance showXmlStringList :: Show XmlStringList where
  show = genericShow
instance decodeXmlStringList :: Decode XmlStringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXmlStringList :: Encode XmlStringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype XmlStringMaxLen256 = XmlStringMaxLen256 String
derive instance newtypeXmlStringMaxLen256 :: Newtype XmlStringMaxLen256 _
derive instance repGenericXmlStringMaxLen256 :: Generic XmlStringMaxLen256 _
instance showXmlStringMaxLen256 :: Show XmlStringMaxLen256 where
  show = genericShow
instance decodeXmlStringMaxLen256 :: Decode XmlStringMaxLen256 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXmlStringMaxLen256 :: Encode XmlStringMaxLen256 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype XmlStringMaxLen256List = XmlStringMaxLen256List (Array XmlStringMaxLen256)
derive instance newtypeXmlStringMaxLen256List :: Newtype XmlStringMaxLen256List _
derive instance repGenericXmlStringMaxLen256List :: Generic XmlStringMaxLen256List _
instance showXmlStringMaxLen256List :: Show XmlStringMaxLen256List where
  show = genericShow
instance decodeXmlStringMaxLen256List :: Decode XmlStringMaxLen256List where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXmlStringMaxLen256List :: Encode XmlStringMaxLen256List where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
