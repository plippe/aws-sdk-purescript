

-- | <p>Amazon EMR is a web service that makes it easy to process large amounts of data efficiently. Amazon EMR uses Hadoop processing combined with several AWS products to do tasks such as web indexing, data mining, log file analysis, machine learning, scientific simulation, and data warehousing.</p>
module AWS.EMR where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "EMR" :: String


-- | <p>Adds an instance fleet to a running cluster.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x.</p> </note>
addInstanceFleet :: forall eff. AddInstanceFleetInput -> Aff (err :: AWS.RequestError | eff) AddInstanceFleetOutput
addInstanceFleet = AWS.request serviceName "AddInstanceFleet" 


-- | <p>Adds one or more instance groups to a running cluster.</p>
addInstanceGroups :: forall eff. AddInstanceGroupsInput -> Aff (err :: AWS.RequestError | eff) AddInstanceGroupsOutput
addInstanceGroups = AWS.request serviceName "AddInstanceGroups" 


-- | <p>AddJobFlowSteps adds new steps to a running cluster. A maximum of 256 steps are allowed in each job flow.</p> <p>If your cluster is long-running (such as a Hive data warehouse) or complex, you may require more than 256 steps to process your data. You can bypass the 256-step limitation in various ways, including using SSH to connect to the master node and submitting queries directly to the software running on the master node, such as Hive and Hadoop. For more information on how to do this, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html">Add More than 256 Steps to a Cluster</a> in the <i>Amazon EMR Management Guide</i>.</p> <p>A step specifies the location of a JAR file stored either on the master node of the cluster or in Amazon S3. Each step is performed by the main function of the main class of the JAR file. The main class can be specified either in the manifest of the JAR or by using the MainFunction parameter of the step.</p> <p>Amazon EMR executes each step in the order listed. For a step to be considered complete, the main function must exit with a zero exit code and all Hadoop jobs started while the step was running must have completed and run successfully.</p> <p>You can only add steps to a cluster that is in one of the following states: STARTING, BOOTSTRAPPING, RUNNING, or WAITING.</p>
addJobFlowSteps :: forall eff. AddJobFlowStepsInput -> Aff (err :: AWS.RequestError | eff) AddJobFlowStepsOutput
addJobFlowSteps = AWS.request serviceName "AddJobFlowSteps" 


-- | <p>Adds tags to an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html">Tag Clusters</a>. </p>
addTags :: forall eff. AddTagsInput -> Aff (err :: AWS.RequestError | eff) AddTagsOutput
addTags = AWS.request serviceName "AddTags" 


-- | <p>Cancels a pending step or steps in a running cluster. Available only in Amazon EMR versions 4.8.0 and later, excluding version 5.0.0. A maximum of 256 steps are allowed in each CancelSteps request. CancelSteps is idempotent but asynchronous; it does not guarantee a step will be canceled, even if the request is successfully submitted. You can only cancel steps that are in a <code>PENDING</code> state.</p>
cancelSteps :: forall eff. CancelStepsInput -> Aff (err :: AWS.RequestError | eff) CancelStepsOutput
cancelSteps = AWS.request serviceName "CancelSteps" 


-- | <p>Creates a security configuration, which is stored in the service and can be specified when a cluster is created.</p>
createSecurityConfiguration :: forall eff. CreateSecurityConfigurationInput -> Aff (err :: AWS.RequestError | eff) CreateSecurityConfigurationOutput
createSecurityConfiguration = AWS.request serviceName "CreateSecurityConfiguration" 


-- | <p>Deletes a security configuration.</p>
deleteSecurityConfiguration :: forall eff. DeleteSecurityConfigurationInput -> Aff (err :: AWS.RequestError | eff) DeleteSecurityConfigurationOutput
deleteSecurityConfiguration = AWS.request serviceName "DeleteSecurityConfiguration" 


-- | <p>Provides cluster-level details including status, hardware and software configuration, VPC settings, and so on. For information about the cluster steps, see <a>ListSteps</a>.</p>
describeCluster :: forall eff. DescribeClusterInput -> Aff (err :: AWS.RequestError | eff) DescribeClusterOutput
describeCluster = AWS.request serviceName "DescribeCluster" 


-- | <p>This API is deprecated and will eventually be removed. We recommend you use <a>ListClusters</a>, <a>DescribeCluster</a>, <a>ListSteps</a>, <a>ListInstanceGroups</a> and <a>ListBootstrapActions</a> instead.</p> <p>DescribeJobFlows returns a list of job flows that match all of the supplied parameters. The parameters can include a list of job flow IDs, job flow states, and restrictions on job flow creation date and time.</p> <p>Regardless of supplied parameters, only job flows created within the last two months are returned.</p> <p>If no parameters are supplied, then job flows matching either of the following criteria are returned:</p> <ul> <li> <p>Job flows created and completed in the last two weeks</p> </li> <li> <p> Job flows created within the last two months that are in one of the following states: <code>RUNNING</code>, <code>WAITING</code>, <code>SHUTTING_DOWN</code>, <code>STARTING</code> </p> </li> </ul> <p>Amazon EMR can return a maximum of 512 job flow descriptions.</p>
describeJobFlows :: forall eff. DescribeJobFlowsInput -> Aff (err :: AWS.RequestError | eff) DescribeJobFlowsOutput
describeJobFlows = AWS.request serviceName "DescribeJobFlows" 


-- | <p>Provides the details of a security configuration by returning the configuration JSON.</p>
describeSecurityConfiguration :: forall eff. DescribeSecurityConfigurationInput -> Aff (err :: AWS.RequestError | eff) DescribeSecurityConfigurationOutput
describeSecurityConfiguration = AWS.request serviceName "DescribeSecurityConfiguration" 


-- | <p>Provides more detail about the cluster step.</p>
describeStep :: forall eff. DescribeStepInput -> Aff (err :: AWS.RequestError | eff) DescribeStepOutput
describeStep = AWS.request serviceName "DescribeStep" 


-- | <p>Provides information about the bootstrap actions associated with a cluster.</p>
listBootstrapActions :: forall eff. ListBootstrapActionsInput -> Aff (err :: AWS.RequestError | eff) ListBootstrapActionsOutput
listBootstrapActions = AWS.request serviceName "ListBootstrapActions" 


-- | <p>Provides the status of all clusters visible to this AWS account. Allows you to filter the list of clusters based on certain criteria; for example, filtering by cluster creation date and time or by status. This call returns a maximum of 50 clusters per call, but returns a marker to track the paging of the cluster list across multiple ListClusters calls.</p>
listClusters :: forall eff. ListClustersInput -> Aff (err :: AWS.RequestError | eff) ListClustersOutput
listClusters = AWS.request serviceName "ListClusters" 


-- | <p>Lists all available details about the instance fleets in a cluster.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
listInstanceFleets :: forall eff. ListInstanceFleetsInput -> Aff (err :: AWS.RequestError | eff) ListInstanceFleetsOutput
listInstanceFleets = AWS.request serviceName "ListInstanceFleets" 


-- | <p>Provides all available details about the instance groups in a cluster.</p>
listInstanceGroups :: forall eff. ListInstanceGroupsInput -> Aff (err :: AWS.RequestError | eff) ListInstanceGroupsOutput
listInstanceGroups = AWS.request serviceName "ListInstanceGroups" 


-- | <p>Provides information for all active EC2 instances and EC2 instances terminated in the last 30 days, up to a maximum of 2,000. EC2 instances in any of the following states are considered active: AWAITING_FULFILLMENT, PROVISIONING, BOOTSTRAPPING, RUNNING.</p>
listInstances :: forall eff. ListInstancesInput -> Aff (err :: AWS.RequestError | eff) ListInstancesOutput
listInstances = AWS.request serviceName "ListInstances" 


-- | <p>Lists all the security configurations visible to this account, providing their creation dates and times, and their names. This call returns a maximum of 50 clusters per call, but returns a marker to track the paging of the cluster list across multiple ListSecurityConfigurations calls.</p>
listSecurityConfigurations :: forall eff. ListSecurityConfigurationsInput -> Aff (err :: AWS.RequestError | eff) ListSecurityConfigurationsOutput
listSecurityConfigurations = AWS.request serviceName "ListSecurityConfigurations" 


-- | <p>Provides a list of steps for the cluster in reverse order unless you specify stepIds with the request.</p>
listSteps :: forall eff. ListStepsInput -> Aff (err :: AWS.RequestError | eff) ListStepsOutput
listSteps = AWS.request serviceName "ListSteps" 


-- | <p>Modifies the target On-Demand and target Spot capacities for the instance fleet with the specified InstanceFleetID within the cluster specified using ClusterID. The call either succeeds or fails atomically.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
modifyInstanceFleet :: forall eff. ModifyInstanceFleetInput -> Aff (err :: AWS.RequestError | eff) Unit
modifyInstanceFleet = AWS.request serviceName "ModifyInstanceFleet" 


-- | <p>ModifyInstanceGroups modifies the number of nodes and configuration settings of an instance group. The input parameters include the new target instance count for the group and the instance group ID. The call will either succeed or fail atomically.</p>
modifyInstanceGroups :: forall eff. ModifyInstanceGroupsInput -> Aff (err :: AWS.RequestError | eff) Unit
modifyInstanceGroups = AWS.request serviceName "ModifyInstanceGroups" 


-- | <p>Creates or updates an automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric.</p>
putAutoScalingPolicy :: forall eff. PutAutoScalingPolicyInput -> Aff (err :: AWS.RequestError | eff) PutAutoScalingPolicyOutput
putAutoScalingPolicy = AWS.request serviceName "PutAutoScalingPolicy" 


-- | <p>Removes an automatic scaling policy from a specified instance group within an EMR cluster.</p>
removeAutoScalingPolicy :: forall eff. RemoveAutoScalingPolicyInput -> Aff (err :: AWS.RequestError | eff) RemoveAutoScalingPolicyOutput
removeAutoScalingPolicy = AWS.request serviceName "RemoveAutoScalingPolicy" 


-- | <p>Removes tags from an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html">Tag Clusters</a>. </p> <p>The following example removes the stack tag with value Prod from a cluster:</p>
removeTags :: forall eff. RemoveTagsInput -> Aff (err :: AWS.RequestError | eff) RemoveTagsOutput
removeTags = AWS.request serviceName "RemoveTags" 


-- | <p>RunJobFlow creates and starts running a new cluster (job flow). The cluster runs the steps specified. After the steps complete, the cluster stops and the HDFS partition is lost. To prevent loss of data, configure the last step of the job flow to store results in Amazon S3. If the <a>JobFlowInstancesConfig</a> <code>KeepJobFlowAliveWhenNoSteps</code> parameter is set to <code>TRUE</code>, the cluster transitions to the WAITING state rather than shutting down after the steps have completed. </p> <p>For additional protection, you can set the <a>JobFlowInstancesConfig</a> <code>TerminationProtected</code> parameter to <code>TRUE</code> to lock the cluster and prevent it from being terminated by API call, user intervention, or in the event of a job flow error.</p> <p>A maximum of 256 steps are allowed in each job flow.</p> <p>If your cluster is long-running (such as a Hive data warehouse) or complex, you may require more than 256 steps to process your data. You can bypass the 256-step limitation in various ways, including using the SSH shell to connect to the master node and submitting queries directly to the software running on the master node, such as Hive and Hadoop. For more information on how to do this, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/AddMoreThan256Steps.html">Add More than 256 Steps to a Cluster</a> in the <i>Amazon EMR Management Guide</i>.</p> <p>For long running clusters, we recommend that you periodically store your results.</p> <note> <p>The instance fleets configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions. The RunJobFlow request can contain InstanceFleets parameters or InstanceGroups parameters, but not both.</p> </note>
runJobFlow :: forall eff. RunJobFlowInput -> Aff (err :: AWS.RequestError | eff) RunJobFlowOutput
runJobFlow = AWS.request serviceName "RunJobFlow" 


-- | <p>SetTerminationProtection locks a cluster (job flow) so the EC2 instances in the cluster cannot be terminated by user intervention, an API call, or in the event of a job-flow error. The cluster still terminates upon successful completion of the job flow. Calling <code>SetTerminationProtection</code> on a cluster is similar to calling the Amazon EC2 <code>DisableAPITermination</code> API on all EC2 instances in a cluster.</p> <p> <code>SetTerminationProtection</code> is used to prevent accidental termination of a cluster and to ensure that in the event of an error, the instances persist so that you can recover any data stored in their ephemeral instance storage.</p> <p> To terminate a cluster that has been locked by setting <code>SetTerminationProtection</code> to <code>true</code>, you must first unlock the job flow by a subsequent call to <code>SetTerminationProtection</code> in which you set the value to <code>false</code>. </p> <p> For more information, see<a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/UsingEMR_TerminationProtection.html">Managing Cluster Termination</a> in the <i>Amazon EMR Management Guide</i>. </p>
setTerminationProtection :: forall eff. SetTerminationProtectionInput -> Aff (err :: AWS.RequestError | eff) Unit
setTerminationProtection = AWS.request serviceName "SetTerminationProtection" 


-- | <p>Sets whether all AWS Identity and Access Management (IAM) users under your account can access the specified clusters (job flows). This action works on running clusters. You can also set the visibility of a cluster when you launch it using the <code>VisibleToAllUsers</code> parameter of <a>RunJobFlow</a>. The SetVisibleToAllUsers action can be called only by an IAM user who created the cluster or the AWS account that owns the cluster.</p>
setVisibleToAllUsers :: forall eff. SetVisibleToAllUsersInput -> Aff (err :: AWS.RequestError | eff) Unit
setVisibleToAllUsers = AWS.request serviceName "SetVisibleToAllUsers" 


-- | <p>TerminateJobFlows shuts a list of clusters (job flows) down. When a job flow is shut down, any step not yet completed is canceled and the EC2 instances on which the cluster is running are stopped. Any log files not already saved are uploaded to Amazon S3 if a LogUri was specified when the cluster was created.</p> <p>The maximum number of clusters allowed is 10. The call to <code>TerminateJobFlows</code> is asynchronous. Depending on the configuration of the cluster, it may take up to 1-5 minutes for the cluster to completely terminate and release allocated resources, such as Amazon EC2 instances.</p>
terminateJobFlows :: forall eff. TerminateJobFlowsInput -> Aff (err :: AWS.RequestError | eff) Unit
terminateJobFlows = AWS.request serviceName "TerminateJobFlows" 


newtype ActionOnFailure = ActionOnFailure String
derive instance newtypeActionOnFailure :: Newtype ActionOnFailure _


newtype AddInstanceFleetInput = AddInstanceFleetInput 
  { "ClusterId" :: (XmlStringMaxLen256)
  , "InstanceFleet" :: (InstanceFleetConfig)
  }
derive instance newtypeAddInstanceFleetInput :: Newtype AddInstanceFleetInput _


newtype AddInstanceFleetOutput = AddInstanceFleetOutput 
  { "ClusterId" :: NullOrUndefined (XmlStringMaxLen256)
  , "InstanceFleetId" :: NullOrUndefined (InstanceFleetId)
  }
derive instance newtypeAddInstanceFleetOutput :: Newtype AddInstanceFleetOutput _


-- | <p>Input to an AddInstanceGroups call.</p>
newtype AddInstanceGroupsInput = AddInstanceGroupsInput 
  { "InstanceGroups" :: (InstanceGroupConfigList)
  , "JobFlowId" :: (XmlStringMaxLen256)
  }
derive instance newtypeAddInstanceGroupsInput :: Newtype AddInstanceGroupsInput _


-- | <p>Output from an AddInstanceGroups call.</p>
newtype AddInstanceGroupsOutput = AddInstanceGroupsOutput 
  { "JobFlowId" :: NullOrUndefined (XmlStringMaxLen256)
  , "InstanceGroupIds" :: NullOrUndefined (InstanceGroupIdsList)
  }
derive instance newtypeAddInstanceGroupsOutput :: Newtype AddInstanceGroupsOutput _


-- | <p> The input argument to the <a>AddJobFlowSteps</a> operation. </p>
newtype AddJobFlowStepsInput = AddJobFlowStepsInput 
  { "JobFlowId" :: (XmlStringMaxLen256)
  , "Steps" :: (StepConfigList)
  }
derive instance newtypeAddJobFlowStepsInput :: Newtype AddJobFlowStepsInput _


-- | <p> The output for the <a>AddJobFlowSteps</a> operation. </p>
newtype AddJobFlowStepsOutput = AddJobFlowStepsOutput 
  { "StepIds" :: NullOrUndefined (StepIdsList)
  }
derive instance newtypeAddJobFlowStepsOutput :: Newtype AddJobFlowStepsOutput _


-- | <p>This input identifies a cluster and a list of tags to attach.</p>
newtype AddTagsInput = AddTagsInput 
  { "ResourceId" :: (ResourceId)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsInput :: Newtype AddTagsInput _


-- | <p>This output indicates the result of adding tags to a resource.</p>
newtype AddTagsOutput = AddTagsOutput 
  { 
  }
derive instance newtypeAddTagsOutput :: Newtype AddTagsOutput _


newtype AdjustmentType = AdjustmentType String
derive instance newtypeAdjustmentType :: Newtype AdjustmentType _


-- | <p>An application is any Amazon or third-party software that you can add to the cluster. This structure contains a list of strings that indicates the software to use with the cluster and accepts a user argument list. Amazon EMR accepts and forwards the argument list to the corresponding installation script as bootstrap action argument. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-mapr.html">Using the MapR Distribution for Hadoop</a>. Currently supported values are:</p> <ul> <li> <p>"mapr-m3" - launch the cluster using MapR M3 Edition.</p> </li> <li> <p>"mapr-m5" - launch the cluster using MapR M5 Edition.</p> </li> <li> <p>"mapr" with the user arguments specifying "--edition,m3" or "--edition,m5" - launch the cluster using MapR M3 or M5 Edition, respectively.</p> </li> </ul> <note> <p>In Amazon EMR releases 4.x and later, the only accepted parameter is the application name. To pass arguments to applications, you supply a configuration for each application.</p> </note>
newtype Application = Application 
  { "Name" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  , "Args" :: NullOrUndefined (StringList)
  , "AdditionalInfo" :: NullOrUndefined (StringMap)
  }
derive instance newtypeApplication :: Newtype Application _


newtype ApplicationList = ApplicationList (Array Application)
derive instance newtypeApplicationList :: Newtype ApplicationList _


-- | <p>An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. An automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See <a>PutAutoScalingPolicy</a>.</p>
newtype AutoScalingPolicy = AutoScalingPolicy 
  { "Constraints" :: (ScalingConstraints)
  , "Rules" :: (ScalingRuleList)
  }
derive instance newtypeAutoScalingPolicy :: Newtype AutoScalingPolicy _


-- | <p>An automatic scaling policy for a core instance group or task instance group in an Amazon EMR cluster. The automatic scaling policy defines how an instance group dynamically adds and terminates EC2 instances in response to the value of a CloudWatch metric. See <a>PutAutoScalingPolicy</a>.</p>
newtype AutoScalingPolicyDescription = AutoScalingPolicyDescription 
  { "Status" :: NullOrUndefined (AutoScalingPolicyStatus)
  , "Constraints" :: NullOrUndefined (ScalingConstraints)
  , "Rules" :: NullOrUndefined (ScalingRuleList)
  }
derive instance newtypeAutoScalingPolicyDescription :: Newtype AutoScalingPolicyDescription _


newtype AutoScalingPolicyState = AutoScalingPolicyState String
derive instance newtypeAutoScalingPolicyState :: Newtype AutoScalingPolicyState _


-- | <p>The reason for an <a>AutoScalingPolicyStatus</a> change.</p>
newtype AutoScalingPolicyStateChangeReason = AutoScalingPolicyStateChangeReason 
  { "Code" :: NullOrUndefined (AutoScalingPolicyStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeAutoScalingPolicyStateChangeReason :: Newtype AutoScalingPolicyStateChangeReason _


newtype AutoScalingPolicyStateChangeReasonCode = AutoScalingPolicyStateChangeReasonCode String
derive instance newtypeAutoScalingPolicyStateChangeReasonCode :: Newtype AutoScalingPolicyStateChangeReasonCode _


-- | <p>The status of an automatic scaling policy. </p>
newtype AutoScalingPolicyStatus = AutoScalingPolicyStatus 
  { "State" :: NullOrUndefined (AutoScalingPolicyState)
  , "StateChangeReason" :: NullOrUndefined (AutoScalingPolicyStateChangeReason)
  }
derive instance newtypeAutoScalingPolicyStatus :: Newtype AutoScalingPolicyStatus _


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _


-- | <p>Configuration of a bootstrap action.</p>
newtype BootstrapActionConfig = BootstrapActionConfig 
  { "Name" :: (XmlStringMaxLen256)
  , "ScriptBootstrapAction" :: (ScriptBootstrapActionConfig)
  }
derive instance newtypeBootstrapActionConfig :: Newtype BootstrapActionConfig _


newtype BootstrapActionConfigList = BootstrapActionConfigList (Array BootstrapActionConfig)
derive instance newtypeBootstrapActionConfigList :: Newtype BootstrapActionConfigList _


-- | <p>Reports the configuration of a bootstrap action in a cluster (job flow).</p>
newtype BootstrapActionDetail = BootstrapActionDetail 
  { "BootstrapActionConfig" :: NullOrUndefined (BootstrapActionConfig)
  }
derive instance newtypeBootstrapActionDetail :: Newtype BootstrapActionDetail _


newtype BootstrapActionDetailList = BootstrapActionDetailList (Array BootstrapActionDetail)
derive instance newtypeBootstrapActionDetailList :: Newtype BootstrapActionDetailList _


-- | <p>Specification of the status of a CancelSteps request. Available only in Amazon EMR version 4.8.0 and later, excluding version 5.0.0.</p>
newtype CancelStepsInfo = CancelStepsInfo 
  { "StepId" :: NullOrUndefined (StepId)
  , "Status" :: NullOrUndefined (CancelStepsRequestStatus)
  , "Reason" :: NullOrUndefined (String)
  }
derive instance newtypeCancelStepsInfo :: Newtype CancelStepsInfo _


newtype CancelStepsInfoList = CancelStepsInfoList (Array CancelStepsInfo)
derive instance newtypeCancelStepsInfoList :: Newtype CancelStepsInfoList _


-- | <p>The input argument to the <a>CancelSteps</a> operation.</p>
newtype CancelStepsInput = CancelStepsInput 
  { "ClusterId" :: NullOrUndefined (XmlStringMaxLen256)
  , "StepIds" :: NullOrUndefined (StepIdsList)
  }
derive instance newtypeCancelStepsInput :: Newtype CancelStepsInput _


-- | <p> The output for the <a>CancelSteps</a> operation. </p>
newtype CancelStepsOutput = CancelStepsOutput 
  { "CancelStepsInfoList" :: NullOrUndefined (CancelStepsInfoList)
  }
derive instance newtypeCancelStepsOutput :: Newtype CancelStepsOutput _


newtype CancelStepsRequestStatus = CancelStepsRequestStatus String
derive instance newtypeCancelStepsRequestStatus :: Newtype CancelStepsRequestStatus _


-- | <p>The definition of a CloudWatch metric alarm, which determines when an automatic scaling activity is triggered. When the defined alarm conditions are satisfied, scaling activity begins.</p>
newtype CloudWatchAlarmDefinition = CloudWatchAlarmDefinition 
  { "ComparisonOperator" :: (ComparisonOperator)
  , "EvaluationPeriods" :: NullOrUndefined (Int)
  , "MetricName" :: (String)
  , "Namespace" :: NullOrUndefined (String)
  , "Period" :: (Int)
  , "Statistic" :: NullOrUndefined (Statistic)
  , "Threshold" :: (NonNegativeDouble)
  , "Unit''" :: NullOrUndefined (Unit'')
  , "Dimensions" :: NullOrUndefined (MetricDimensionList)
  }
derive instance newtypeCloudWatchAlarmDefinition :: Newtype CloudWatchAlarmDefinition _


-- | <p>The detailed description of the cluster.</p>
newtype Cluster = Cluster 
  { "Id" :: NullOrUndefined (ClusterId)
  , "Name" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (ClusterStatus)
  , "Ec2InstanceAttributes" :: NullOrUndefined (Ec2InstanceAttributes)
  , "InstanceCollectionType" :: NullOrUndefined (InstanceCollectionType)
  , "LogUri" :: NullOrUndefined (String)
  , "RequestedAmiVersion" :: NullOrUndefined (String)
  , "RunningAmiVersion" :: NullOrUndefined (String)
  , "ReleaseLabel" :: NullOrUndefined (String)
  , "AutoTerminate" :: NullOrUndefined (Boolean)
  , "TerminationProtected" :: NullOrUndefined (Boolean)
  , "VisibleToAllUsers" :: NullOrUndefined (Boolean)
  , "Applications" :: NullOrUndefined (ApplicationList)
  , "Tags" :: NullOrUndefined (TagList)
  , "ServiceRole" :: NullOrUndefined (String)
  , "NormalizedInstanceHours" :: NullOrUndefined (Int)
  , "MasterPublicDnsName" :: NullOrUndefined (String)
  , "Configurations" :: NullOrUndefined (ConfigurationList)
  , "SecurityConfiguration" :: NullOrUndefined (XmlString)
  , "AutoScalingRole" :: NullOrUndefined (XmlString)
  , "ScaleDownBehavior" :: NullOrUndefined (ScaleDownBehavior)
  , "CustomAmiId" :: NullOrUndefined (XmlStringMaxLen256)
  , "EbsRootVolumeSize" :: NullOrUndefined (Int)
  , "RepoUpgradeOnBoot" :: NullOrUndefined (RepoUpgradeOnBoot)
  , "KerberosAttributes" :: NullOrUndefined (KerberosAttributes)
  }
derive instance newtypeCluster :: Newtype Cluster _


newtype ClusterId = ClusterId String
derive instance newtypeClusterId :: Newtype ClusterId _


newtype ClusterState = ClusterState String
derive instance newtypeClusterState :: Newtype ClusterState _


-- | <p>The reason that the cluster changed to its current state.</p>
newtype ClusterStateChangeReason = ClusterStateChangeReason 
  { "Code" :: NullOrUndefined (ClusterStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeClusterStateChangeReason :: Newtype ClusterStateChangeReason _


newtype ClusterStateChangeReasonCode = ClusterStateChangeReasonCode String
derive instance newtypeClusterStateChangeReasonCode :: Newtype ClusterStateChangeReasonCode _


newtype ClusterStateList = ClusterStateList (Array ClusterState)
derive instance newtypeClusterStateList :: Newtype ClusterStateList _


-- | <p>The detailed status of the cluster.</p>
newtype ClusterStatus = ClusterStatus 
  { "State" :: NullOrUndefined (ClusterState)
  , "StateChangeReason" :: NullOrUndefined (ClusterStateChangeReason)
  , "Timeline" :: NullOrUndefined (ClusterTimeline)
  }
derive instance newtypeClusterStatus :: Newtype ClusterStatus _


-- | <p>The summary description of the cluster.</p>
newtype ClusterSummary = ClusterSummary 
  { "Id" :: NullOrUndefined (ClusterId)
  , "Name" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (ClusterStatus)
  , "NormalizedInstanceHours" :: NullOrUndefined (Int)
  }
derive instance newtypeClusterSummary :: Newtype ClusterSummary _


newtype ClusterSummaryList = ClusterSummaryList (Array ClusterSummary)
derive instance newtypeClusterSummaryList :: Newtype ClusterSummaryList _


-- | <p>Represents the timeline of the cluster's lifecycle.</p>
newtype ClusterTimeline = ClusterTimeline 
  { "CreationDateTime" :: NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined (Date)
  }
derive instance newtypeClusterTimeline :: Newtype ClusterTimeline _


-- | <p>An entity describing an executable that runs on a cluster.</p>
newtype Command = Command 
  { "Name" :: NullOrUndefined (String)
  , "ScriptPath" :: NullOrUndefined (String)
  , "Args" :: NullOrUndefined (StringList)
  }
derive instance newtypeCommand :: Newtype Command _


newtype CommandList = CommandList (Array Command)
derive instance newtypeCommandList :: Newtype CommandList _


newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _


-- | <note> <p>Amazon EMR releases 4.x or later.</p> </note> <p>An optional configuration specification to be used when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR. A configuration consists of a classification, properties, and optional nested configurations. A classification refers to an application-specific configuration file. Properties are the settings you want to change in that file. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html">Configuring Applications</a>.</p>
newtype Configuration = Configuration 
  { "Classification" :: NullOrUndefined (String)
  , "Configurations" :: NullOrUndefined (ConfigurationList)
  , "Properties" :: NullOrUndefined (StringMap)
  }
derive instance newtypeConfiguration :: Newtype Configuration _


newtype ConfigurationList = ConfigurationList (Array Configuration)
derive instance newtypeConfigurationList :: Newtype ConfigurationList _


newtype CreateSecurityConfigurationInput = CreateSecurityConfigurationInput 
  { "Name" :: (XmlString)
  , "SecurityConfiguration" :: (String)
  }
derive instance newtypeCreateSecurityConfigurationInput :: Newtype CreateSecurityConfigurationInput _


newtype CreateSecurityConfigurationOutput = CreateSecurityConfigurationOutput 
  { "Name" :: (XmlString)
  , "CreationDateTime" :: (Date)
  }
derive instance newtypeCreateSecurityConfigurationOutput :: Newtype CreateSecurityConfigurationOutput _


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _


newtype DeleteSecurityConfigurationInput = DeleteSecurityConfigurationInput 
  { "Name" :: (XmlString)
  }
derive instance newtypeDeleteSecurityConfigurationInput :: Newtype DeleteSecurityConfigurationInput _


newtype DeleteSecurityConfigurationOutput = DeleteSecurityConfigurationOutput 
  { 
  }
derive instance newtypeDeleteSecurityConfigurationOutput :: Newtype DeleteSecurityConfigurationOutput _


-- | <p>This input determines which cluster to describe.</p>
newtype DescribeClusterInput = DescribeClusterInput 
  { "ClusterId" :: (ClusterId)
  }
derive instance newtypeDescribeClusterInput :: Newtype DescribeClusterInput _


-- | <p>This output contains the description of the cluster.</p>
newtype DescribeClusterOutput = DescribeClusterOutput 
  { "Cluster" :: NullOrUndefined (Cluster)
  }
derive instance newtypeDescribeClusterOutput :: Newtype DescribeClusterOutput _


-- | <p> The input for the <a>DescribeJobFlows</a> operation. </p>
newtype DescribeJobFlowsInput = DescribeJobFlowsInput 
  { "CreatedAfter" :: NullOrUndefined (Date)
  , "CreatedBefore" :: NullOrUndefined (Date)
  , "JobFlowIds" :: NullOrUndefined (XmlStringList)
  , "JobFlowStates" :: NullOrUndefined (JobFlowExecutionStateList)
  }
derive instance newtypeDescribeJobFlowsInput :: Newtype DescribeJobFlowsInput _


-- | <p> The output for the <a>DescribeJobFlows</a> operation. </p>
newtype DescribeJobFlowsOutput = DescribeJobFlowsOutput 
  { "JobFlows" :: NullOrUndefined (JobFlowDetailList)
  }
derive instance newtypeDescribeJobFlowsOutput :: Newtype DescribeJobFlowsOutput _


newtype DescribeSecurityConfigurationInput = DescribeSecurityConfigurationInput 
  { "Name" :: (XmlString)
  }
derive instance newtypeDescribeSecurityConfigurationInput :: Newtype DescribeSecurityConfigurationInput _


newtype DescribeSecurityConfigurationOutput = DescribeSecurityConfigurationOutput 
  { "Name" :: NullOrUndefined (XmlString)
  , "SecurityConfiguration" :: NullOrUndefined (String)
  , "CreationDateTime" :: NullOrUndefined (Date)
  }
derive instance newtypeDescribeSecurityConfigurationOutput :: Newtype DescribeSecurityConfigurationOutput _


-- | <p>This input determines which step to describe.</p>
newtype DescribeStepInput = DescribeStepInput 
  { "ClusterId" :: (ClusterId)
  , "StepId" :: (StepId)
  }
derive instance newtypeDescribeStepInput :: Newtype DescribeStepInput _


-- | <p>This output contains the description of the cluster step.</p>
newtype DescribeStepOutput = DescribeStepOutput 
  { "Step" :: NullOrUndefined (Step)
  }
derive instance newtypeDescribeStepOutput :: Newtype DescribeStepOutput _


newtype EC2InstanceIdsList = EC2InstanceIdsList (Array InstanceId)
derive instance newtypeEC2InstanceIdsList :: Newtype EC2InstanceIdsList _


newtype EC2InstanceIdsToTerminateList = EC2InstanceIdsToTerminateList (Array InstanceId)
derive instance newtypeEC2InstanceIdsToTerminateList :: Newtype EC2InstanceIdsToTerminateList _


-- | <p>Configuration of requested EBS block device associated with the instance group.</p>
newtype EbsBlockDevice = EbsBlockDevice 
  { "VolumeSpecification" :: NullOrUndefined (VolumeSpecification)
  , "Device" :: NullOrUndefined (String)
  }
derive instance newtypeEbsBlockDevice :: Newtype EbsBlockDevice _


-- | <p>Configuration of requested EBS block device associated with the instance group with count of volumes that will be associated to every instance.</p>
newtype EbsBlockDeviceConfig = EbsBlockDeviceConfig 
  { "VolumeSpecification" :: (VolumeSpecification)
  , "VolumesPerInstance" :: NullOrUndefined (Int)
  }
derive instance newtypeEbsBlockDeviceConfig :: Newtype EbsBlockDeviceConfig _


newtype EbsBlockDeviceConfigList = EbsBlockDeviceConfigList (Array EbsBlockDeviceConfig)
derive instance newtypeEbsBlockDeviceConfigList :: Newtype EbsBlockDeviceConfigList _


newtype EbsBlockDeviceList = EbsBlockDeviceList (Array EbsBlockDevice)
derive instance newtypeEbsBlockDeviceList :: Newtype EbsBlockDeviceList _


-- | <p>The Amazon EBS configuration of a cluster instance.</p>
newtype EbsConfiguration = EbsConfiguration 
  { "EbsBlockDeviceConfigs" :: NullOrUndefined (EbsBlockDeviceConfigList)
  , "EbsOptimized" :: NullOrUndefined (BooleanObject)
  }
derive instance newtypeEbsConfiguration :: Newtype EbsConfiguration _


-- | <p>EBS block device that's attached to an EC2 instance.</p>
newtype EbsVolume = EbsVolume 
  { "Device" :: NullOrUndefined (String)
  , "VolumeId" :: NullOrUndefined (String)
  }
derive instance newtypeEbsVolume :: Newtype EbsVolume _


newtype EbsVolumeList = EbsVolumeList (Array EbsVolume)
derive instance newtypeEbsVolumeList :: Newtype EbsVolumeList _


-- | <p>Provides information about the EC2 instances in a cluster grouped by category. For example, key name, subnet ID, IAM instance profile, and so on.</p>
newtype Ec2InstanceAttributes = Ec2InstanceAttributes 
  { "Ec2KeyName" :: NullOrUndefined (String)
  , "Ec2SubnetId" :: NullOrUndefined (String)
  , "RequestedEc2SubnetIds" :: NullOrUndefined (XmlStringMaxLen256List)
  , "Ec2AvailabilityZone" :: NullOrUndefined (String)
  , "RequestedEc2AvailabilityZones" :: NullOrUndefined (XmlStringMaxLen256List)
  , "IamInstanceProfile" :: NullOrUndefined (String)
  , "EmrManagedMasterSecurityGroup" :: NullOrUndefined (String)
  , "EmrManagedSlaveSecurityGroup" :: NullOrUndefined (String)
  , "ServiceAccessSecurityGroup" :: NullOrUndefined (String)
  , "AdditionalMasterSecurityGroups" :: NullOrUndefined (StringList)
  , "AdditionalSlaveSecurityGroups" :: NullOrUndefined (StringList)
  }
derive instance newtypeEc2InstanceAttributes :: Newtype Ec2InstanceAttributes _


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>The details of the step failure. The service attempts to detect the root cause for many common failures.</p>
newtype FailureDetails = FailureDetails 
  { "Reason" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  , "LogFile" :: NullOrUndefined (String)
  }
derive instance newtypeFailureDetails :: Newtype FailureDetails _


-- | <p>A job flow step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.</p>
newtype HadoopJarStepConfig = HadoopJarStepConfig 
  { "Properties" :: NullOrUndefined (KeyValueList)
  , "Jar" :: (XmlString)
  , "MainClass" :: NullOrUndefined (XmlString)
  , "Args" :: NullOrUndefined (XmlStringList)
  }
derive instance newtypeHadoopJarStepConfig :: Newtype HadoopJarStepConfig _


-- | <p>A cluster step consisting of a JAR file whose main function will be executed. The main function submits a job for Hadoop to execute and waits for the job to finish or fail.</p>
newtype HadoopStepConfig = HadoopStepConfig 
  { "Jar" :: NullOrUndefined (String)
  , "Properties" :: NullOrUndefined (StringMap)
  , "MainClass" :: NullOrUndefined (String)
  , "Args" :: NullOrUndefined (StringList)
  }
derive instance newtypeHadoopStepConfig :: Newtype HadoopStepConfig _


-- | <p>Represents an EC2 instance provisioned as part of cluster.</p>
newtype Instance = Instance 
  { "Id" :: NullOrUndefined (InstanceId)
  , "Ec2InstanceId" :: NullOrUndefined (InstanceId)
  , "PublicDnsName" :: NullOrUndefined (String)
  , "PublicIpAddress" :: NullOrUndefined (String)
  , "PrivateDnsName" :: NullOrUndefined (String)
  , "PrivateIpAddress" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (InstanceStatus)
  , "InstanceGroupId" :: NullOrUndefined (String)
  , "InstanceFleetId" :: NullOrUndefined (InstanceFleetId)
  , "Market" :: NullOrUndefined (MarketType)
  , "InstanceType" :: NullOrUndefined (InstanceType)
  , "EbsVolumes" :: NullOrUndefined (EbsVolumeList)
  }
derive instance newtypeInstance :: Newtype Instance _


newtype InstanceCollectionType = InstanceCollectionType String
derive instance newtypeInstanceCollectionType :: Newtype InstanceCollectionType _


-- | <p>Describes an instance fleet, which is a group of EC2 instances that host a particular node type (master, core, or task) in an Amazon EMR cluster. Instance fleets can consist of a mix of instance types and On-Demand and Spot instances, which are provisioned to meet a defined target capacity. </p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleet = InstanceFleet 
  { "Id" :: NullOrUndefined (InstanceFleetId)
  , "Name" :: NullOrUndefined (XmlStringMaxLen256)
  , "Status" :: NullOrUndefined (InstanceFleetStatus)
  , "InstanceFleetType" :: NullOrUndefined (InstanceFleetType)
  , "TargetOnDemandCapacity" :: NullOrUndefined (WholeNumber)
  , "TargetSpotCapacity" :: NullOrUndefined (WholeNumber)
  , "ProvisionedOnDemandCapacity" :: NullOrUndefined (WholeNumber)
  , "ProvisionedSpotCapacity" :: NullOrUndefined (WholeNumber)
  , "InstanceTypeSpecifications" :: NullOrUndefined (InstanceTypeSpecificationList)
  , "LaunchSpecifications" :: NullOrUndefined (InstanceFleetProvisioningSpecifications)
  }
derive instance newtypeInstanceFleet :: Newtype InstanceFleet _


-- | <p>The configuration that defines an instance fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetConfig = InstanceFleetConfig 
  { "Name" :: NullOrUndefined (XmlStringMaxLen256)
  , "InstanceFleetType" :: (InstanceFleetType)
  , "TargetOnDemandCapacity" :: NullOrUndefined (WholeNumber)
  , "TargetSpotCapacity" :: NullOrUndefined (WholeNumber)
  , "InstanceTypeConfigs" :: NullOrUndefined (InstanceTypeConfigList)
  , "LaunchSpecifications" :: NullOrUndefined (InstanceFleetProvisioningSpecifications)
  }
derive instance newtypeInstanceFleetConfig :: Newtype InstanceFleetConfig _


newtype InstanceFleetConfigList = InstanceFleetConfigList (Array InstanceFleetConfig)
derive instance newtypeInstanceFleetConfigList :: Newtype InstanceFleetConfigList _


newtype InstanceFleetId = InstanceFleetId String
derive instance newtypeInstanceFleetId :: Newtype InstanceFleetId _


newtype InstanceFleetList = InstanceFleetList (Array InstanceFleet)
derive instance newtypeInstanceFleetList :: Newtype InstanceFleetList _


-- | <p>Configuration parameters for an instance fleet modification request.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetModifyConfig = InstanceFleetModifyConfig 
  { "InstanceFleetId" :: (InstanceFleetId)
  , "TargetOnDemandCapacity" :: NullOrUndefined (WholeNumber)
  , "TargetSpotCapacity" :: NullOrUndefined (WholeNumber)
  }
derive instance newtypeInstanceFleetModifyConfig :: Newtype InstanceFleetModifyConfig _


-- | <p>The launch specification for Spot instances in the fleet, which determines the defined duration and provisioning timeout behavior.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetProvisioningSpecifications = InstanceFleetProvisioningSpecifications 
  { "SpotSpecification" :: (SpotProvisioningSpecification)
  }
derive instance newtypeInstanceFleetProvisioningSpecifications :: Newtype InstanceFleetProvisioningSpecifications _


newtype InstanceFleetState = InstanceFleetState String
derive instance newtypeInstanceFleetState :: Newtype InstanceFleetState _


-- | <p>Provides status change reason details for the instance fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetStateChangeReason = InstanceFleetStateChangeReason 
  { "Code" :: NullOrUndefined (InstanceFleetStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInstanceFleetStateChangeReason :: Newtype InstanceFleetStateChangeReason _


newtype InstanceFleetStateChangeReasonCode = InstanceFleetStateChangeReasonCode String
derive instance newtypeInstanceFleetStateChangeReasonCode :: Newtype InstanceFleetStateChangeReasonCode _


-- | <p>The status of the instance fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetStatus = InstanceFleetStatus 
  { "State" :: NullOrUndefined (InstanceFleetState)
  , "StateChangeReason" :: NullOrUndefined (InstanceFleetStateChangeReason)
  , "Timeline" :: NullOrUndefined (InstanceFleetTimeline)
  }
derive instance newtypeInstanceFleetStatus :: Newtype InstanceFleetStatus _


-- | <p>Provides historical timestamps for the instance fleet, including the time of creation, the time it became ready to run jobs, and the time of termination.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceFleetTimeline = InstanceFleetTimeline 
  { "CreationDateTime" :: NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined (Date)
  }
derive instance newtypeInstanceFleetTimeline :: Newtype InstanceFleetTimeline _


newtype InstanceFleetType = InstanceFleetType String
derive instance newtypeInstanceFleetType :: Newtype InstanceFleetType _


-- | <p>This entity represents an instance group, which is a group of instances that have common purpose. For example, CORE instance group is used for HDFS.</p>
newtype InstanceGroup = InstanceGroup 
  { "Id" :: NullOrUndefined (InstanceGroupId)
  , "Name" :: NullOrUndefined (String)
  , "Market" :: NullOrUndefined (MarketType)
  , "InstanceGroupType" :: NullOrUndefined (InstanceGroupType)
  , "BidPrice" :: NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined (InstanceType)
  , "RequestedInstanceCount" :: NullOrUndefined (Int)
  , "RunningInstanceCount" :: NullOrUndefined (Int)
  , "Status" :: NullOrUndefined (InstanceGroupStatus)
  , "Configurations" :: NullOrUndefined (ConfigurationList)
  , "EbsBlockDevices" :: NullOrUndefined (EbsBlockDeviceList)
  , "EbsOptimized" :: NullOrUndefined (BooleanObject)
  , "ShrinkPolicy" :: NullOrUndefined (ShrinkPolicy)
  , "AutoScalingPolicy" :: NullOrUndefined (AutoScalingPolicyDescription)
  }
derive instance newtypeInstanceGroup :: Newtype InstanceGroup _


-- | <p>Configuration defining a new instance group.</p>
newtype InstanceGroupConfig = InstanceGroupConfig 
  { "Name" :: NullOrUndefined (XmlStringMaxLen256)
  , "Market" :: NullOrUndefined (MarketType)
  , "InstanceRole" :: (InstanceRoleType)
  , "BidPrice" :: NullOrUndefined (XmlStringMaxLen256)
  , "InstanceType" :: (InstanceType)
  , "InstanceCount" :: (Int)
  , "Configurations" :: NullOrUndefined (ConfigurationList)
  , "EbsConfiguration" :: NullOrUndefined (EbsConfiguration)
  , "AutoScalingPolicy" :: NullOrUndefined (AutoScalingPolicy)
  }
derive instance newtypeInstanceGroupConfig :: Newtype InstanceGroupConfig _


newtype InstanceGroupConfigList = InstanceGroupConfigList (Array InstanceGroupConfig)
derive instance newtypeInstanceGroupConfigList :: Newtype InstanceGroupConfigList _


-- | <p>Detailed information about an instance group.</p>
newtype InstanceGroupDetail = InstanceGroupDetail 
  { "InstanceGroupId" :: NullOrUndefined (XmlStringMaxLen256)
  , "Name" :: NullOrUndefined (XmlStringMaxLen256)
  , "Market" :: (MarketType)
  , "InstanceRole" :: (InstanceRoleType)
  , "BidPrice" :: NullOrUndefined (XmlStringMaxLen256)
  , "InstanceType" :: (InstanceType)
  , "InstanceRequestCount" :: (Int)
  , "InstanceRunningCount" :: (Int)
  , "State" :: (InstanceGroupState)
  , "LastStateChangeReason" :: NullOrUndefined (XmlString)
  , "CreationDateTime" :: (Date)
  , "StartDateTime" :: NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined (Date)
  }
derive instance newtypeInstanceGroupDetail :: Newtype InstanceGroupDetail _


newtype InstanceGroupDetailList = InstanceGroupDetailList (Array InstanceGroupDetail)
derive instance newtypeInstanceGroupDetailList :: Newtype InstanceGroupDetailList _


newtype InstanceGroupId = InstanceGroupId String
derive instance newtypeInstanceGroupId :: Newtype InstanceGroupId _


newtype InstanceGroupIdsList = InstanceGroupIdsList (Array XmlStringMaxLen256)
derive instance newtypeInstanceGroupIdsList :: Newtype InstanceGroupIdsList _


newtype InstanceGroupList = InstanceGroupList (Array InstanceGroup)
derive instance newtypeInstanceGroupList :: Newtype InstanceGroupList _


-- | <p>Modify an instance group size.</p>
newtype InstanceGroupModifyConfig = InstanceGroupModifyConfig 
  { "InstanceGroupId" :: (XmlStringMaxLen256)
  , "InstanceCount" :: NullOrUndefined (Int)
  , "EC2InstanceIdsToTerminate" :: NullOrUndefined (EC2InstanceIdsToTerminateList)
  , "ShrinkPolicy" :: NullOrUndefined (ShrinkPolicy)
  }
derive instance newtypeInstanceGroupModifyConfig :: Newtype InstanceGroupModifyConfig _


newtype InstanceGroupModifyConfigList = InstanceGroupModifyConfigList (Array InstanceGroupModifyConfig)
derive instance newtypeInstanceGroupModifyConfigList :: Newtype InstanceGroupModifyConfigList _


newtype InstanceGroupState = InstanceGroupState String
derive instance newtypeInstanceGroupState :: Newtype InstanceGroupState _


-- | <p>The status change reason details for the instance group.</p>
newtype InstanceGroupStateChangeReason = InstanceGroupStateChangeReason 
  { "Code" :: NullOrUndefined (InstanceGroupStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInstanceGroupStateChangeReason :: Newtype InstanceGroupStateChangeReason _


newtype InstanceGroupStateChangeReasonCode = InstanceGroupStateChangeReasonCode String
derive instance newtypeInstanceGroupStateChangeReasonCode :: Newtype InstanceGroupStateChangeReasonCode _


-- | <p>The details of the instance group status.</p>
newtype InstanceGroupStatus = InstanceGroupStatus 
  { "State" :: NullOrUndefined (InstanceGroupState)
  , "StateChangeReason" :: NullOrUndefined (InstanceGroupStateChangeReason)
  , "Timeline" :: NullOrUndefined (InstanceGroupTimeline)
  }
derive instance newtypeInstanceGroupStatus :: Newtype InstanceGroupStatus _


-- | <p>The timeline of the instance group lifecycle.</p>
newtype InstanceGroupTimeline = InstanceGroupTimeline 
  { "CreationDateTime" :: NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined (Date)
  }
derive instance newtypeInstanceGroupTimeline :: Newtype InstanceGroupTimeline _


newtype InstanceGroupType = InstanceGroupType String
derive instance newtypeInstanceGroupType :: Newtype InstanceGroupType _


newtype InstanceGroupTypeList = InstanceGroupTypeList (Array InstanceGroupType)
derive instance newtypeInstanceGroupTypeList :: Newtype InstanceGroupTypeList _


newtype InstanceId = InstanceId String
derive instance newtypeInstanceId :: Newtype InstanceId _


newtype InstanceList = InstanceList (Array Instance)
derive instance newtypeInstanceList :: Newtype InstanceList _


-- | <p>Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.</p>
newtype InstanceResizePolicy = InstanceResizePolicy 
  { "InstancesToTerminate" :: NullOrUndefined (EC2InstanceIdsList)
  , "InstancesToProtect" :: NullOrUndefined (EC2InstanceIdsList)
  , "InstanceTerminationTimeout" :: NullOrUndefined (Int)
  }
derive instance newtypeInstanceResizePolicy :: Newtype InstanceResizePolicy _


newtype InstanceRoleType = InstanceRoleType String
derive instance newtypeInstanceRoleType :: Newtype InstanceRoleType _


newtype InstanceState = InstanceState String
derive instance newtypeInstanceState :: Newtype InstanceState _


-- | <p>The details of the status change reason for the instance.</p>
newtype InstanceStateChangeReason = InstanceStateChangeReason 
  { "Code" :: NullOrUndefined (InstanceStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInstanceStateChangeReason :: Newtype InstanceStateChangeReason _


newtype InstanceStateChangeReasonCode = InstanceStateChangeReasonCode String
derive instance newtypeInstanceStateChangeReasonCode :: Newtype InstanceStateChangeReasonCode _


newtype InstanceStateList = InstanceStateList (Array InstanceState)
derive instance newtypeInstanceStateList :: Newtype InstanceStateList _


-- | <p>The instance status details.</p>
newtype InstanceStatus = InstanceStatus 
  { "State" :: NullOrUndefined (InstanceState)
  , "StateChangeReason" :: NullOrUndefined (InstanceStateChangeReason)
  , "Timeline" :: NullOrUndefined (InstanceTimeline)
  }
derive instance newtypeInstanceStatus :: Newtype InstanceStatus _


-- | <p>The timeline of the instance lifecycle.</p>
newtype InstanceTimeline = InstanceTimeline 
  { "CreationDateTime" :: NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined (Date)
  }
derive instance newtypeInstanceTimeline :: Newtype InstanceTimeline _


newtype InstanceType = InstanceType String
derive instance newtypeInstanceType :: Newtype InstanceType _


-- | <p>An instance type configuration for each instance type in an instance fleet, which determines the EC2 instances Amazon EMR attempts to provision to fulfill On-Demand and Spot target capacities. There can be a maximum of 5 instance type configurations in a fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceTypeConfig = InstanceTypeConfig 
  { "InstanceType" :: (InstanceType)
  , "WeightedCapacity" :: NullOrUndefined (WholeNumber)
  , "BidPrice" :: NullOrUndefined (XmlStringMaxLen256)
  , "BidPriceAsPercentageOfOnDemandPrice" :: NullOrUndefined (NonNegativeDouble)
  , "EbsConfiguration" :: NullOrUndefined (EbsConfiguration)
  , "Configurations" :: NullOrUndefined (ConfigurationList)
  }
derive instance newtypeInstanceTypeConfig :: Newtype InstanceTypeConfig _


newtype InstanceTypeConfigList = InstanceTypeConfigList (Array InstanceTypeConfig)
derive instance newtypeInstanceTypeConfigList :: Newtype InstanceTypeConfigList _


-- | <p>The configuration specification for each instance type in an instance fleet.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype InstanceTypeSpecification = InstanceTypeSpecification 
  { "InstanceType" :: NullOrUndefined (InstanceType)
  , "WeightedCapacity" :: NullOrUndefined (WholeNumber)
  , "BidPrice" :: NullOrUndefined (XmlStringMaxLen256)
  , "BidPriceAsPercentageOfOnDemandPrice" :: NullOrUndefined (NonNegativeDouble)
  , "Configurations" :: NullOrUndefined (ConfigurationList)
  , "EbsBlockDevices" :: NullOrUndefined (EbsBlockDeviceList)
  , "EbsOptimized" :: NullOrUndefined (BooleanObject)
  }
derive instance newtypeInstanceTypeSpecification :: Newtype InstanceTypeSpecification _


newtype InstanceTypeSpecificationList = InstanceTypeSpecificationList (Array InstanceTypeSpecification)
derive instance newtypeInstanceTypeSpecificationList :: Newtype InstanceTypeSpecificationList _


-- | <p>Indicates that an error occurred while processing the request and that the request was not completed.</p>
newtype InternalServerError = InternalServerError 
  { 
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _


-- | <p>This exception occurs when there is an internal failure in the EMR service.</p>
newtype InternalServerException = InternalServerException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerException :: Newtype InternalServerException _


-- | <p>This exception occurs when there is something wrong with user input.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _


-- | <p>A description of a cluster (job flow).</p>
newtype JobFlowDetail = JobFlowDetail 
  { "JobFlowId" :: (XmlStringMaxLen256)
  , "Name" :: (XmlStringMaxLen256)
  , "LogUri" :: NullOrUndefined (XmlString)
  , "AmiVersion" :: NullOrUndefined (XmlStringMaxLen256)
  , "ExecutionStatusDetail" :: (JobFlowExecutionStatusDetail)
  , "Instances" :: (JobFlowInstancesDetail)
  , "Steps" :: NullOrUndefined (StepDetailList)
  , "BootstrapActions" :: NullOrUndefined (BootstrapActionDetailList)
  , "SupportedProducts" :: NullOrUndefined (SupportedProductsList)
  , "VisibleToAllUsers" :: NullOrUndefined (Boolean)
  , "JobFlowRole" :: NullOrUndefined (XmlString)
  , "ServiceRole" :: NullOrUndefined (XmlString)
  , "AutoScalingRole" :: NullOrUndefined (XmlString)
  , "ScaleDownBehavior" :: NullOrUndefined (ScaleDownBehavior)
  }
derive instance newtypeJobFlowDetail :: Newtype JobFlowDetail _


newtype JobFlowDetailList = JobFlowDetailList (Array JobFlowDetail)
derive instance newtypeJobFlowDetailList :: Newtype JobFlowDetailList _


-- | <p>The type of instance.</p>
newtype JobFlowExecutionState = JobFlowExecutionState String
derive instance newtypeJobFlowExecutionState :: Newtype JobFlowExecutionState _


newtype JobFlowExecutionStateList = JobFlowExecutionStateList (Array JobFlowExecutionState)
derive instance newtypeJobFlowExecutionStateList :: Newtype JobFlowExecutionStateList _


-- | <p>Describes the status of the cluster (job flow).</p>
newtype JobFlowExecutionStatusDetail = JobFlowExecutionStatusDetail 
  { "State" :: (JobFlowExecutionState)
  , "CreationDateTime" :: (Date)
  , "StartDateTime" :: NullOrUndefined (Date)
  , "ReadyDateTime" :: NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined (Date)
  , "LastStateChangeReason" :: NullOrUndefined (XmlString)
  }
derive instance newtypeJobFlowExecutionStatusDetail :: Newtype JobFlowExecutionStatusDetail _


-- | <p>A description of the Amazon EC2 instance on which the cluster (job flow) runs. A valid JobFlowInstancesConfig must contain either InstanceGroups or InstanceFleets, which is the recommended configuration. They cannot be used together. You may also have MasterInstanceType, SlaveInstanceType, and InstanceCount (all three must be present), but we don't recommend this configuration.</p>
newtype JobFlowInstancesConfig = JobFlowInstancesConfig 
  { "MasterInstanceType" :: NullOrUndefined (InstanceType)
  , "SlaveInstanceType" :: NullOrUndefined (InstanceType)
  , "InstanceCount" :: NullOrUndefined (Int)
  , "InstanceGroups" :: NullOrUndefined (InstanceGroupConfigList)
  , "InstanceFleets" :: NullOrUndefined (InstanceFleetConfigList)
  , "Ec2KeyName" :: NullOrUndefined (XmlStringMaxLen256)
  , "Placement" :: NullOrUndefined (PlacementType)
  , "KeepJobFlowAliveWhenNoSteps" :: NullOrUndefined (Boolean)
  , "TerminationProtected" :: NullOrUndefined (Boolean)
  , "HadoopVersion" :: NullOrUndefined (XmlStringMaxLen256)
  , "Ec2SubnetId" :: NullOrUndefined (XmlStringMaxLen256)
  , "Ec2SubnetIds" :: NullOrUndefined (XmlStringMaxLen256List)
  , "EmrManagedMasterSecurityGroup" :: NullOrUndefined (XmlStringMaxLen256)
  , "EmrManagedSlaveSecurityGroup" :: NullOrUndefined (XmlStringMaxLen256)
  , "ServiceAccessSecurityGroup" :: NullOrUndefined (XmlStringMaxLen256)
  , "AdditionalMasterSecurityGroups" :: NullOrUndefined (SecurityGroupsList)
  , "AdditionalSlaveSecurityGroups" :: NullOrUndefined (SecurityGroupsList)
  }
derive instance newtypeJobFlowInstancesConfig :: Newtype JobFlowInstancesConfig _


-- | <p>Specify the type of Amazon EC2 instances that the cluster (job flow) runs on.</p>
newtype JobFlowInstancesDetail = JobFlowInstancesDetail 
  { "MasterInstanceType" :: (InstanceType)
  , "MasterPublicDnsName" :: NullOrUndefined (XmlString)
  , "MasterInstanceId" :: NullOrUndefined (XmlString)
  , "SlaveInstanceType" :: (InstanceType)
  , "InstanceCount" :: (Int)
  , "InstanceGroups" :: NullOrUndefined (InstanceGroupDetailList)
  , "NormalizedInstanceHours" :: NullOrUndefined (Int)
  , "Ec2KeyName" :: NullOrUndefined (XmlStringMaxLen256)
  , "Ec2SubnetId" :: NullOrUndefined (XmlStringMaxLen256)
  , "Placement" :: NullOrUndefined (PlacementType)
  , "KeepJobFlowAliveWhenNoSteps" :: NullOrUndefined (Boolean)
  , "TerminationProtected" :: NullOrUndefined (Boolean)
  , "HadoopVersion" :: NullOrUndefined (XmlStringMaxLen256)
  }
derive instance newtypeJobFlowInstancesDetail :: Newtype JobFlowInstancesDetail _


-- | <p>Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html">Use Kerberos Authentication</a> in the <i>EMR Management Guide</i>.</p>
newtype KerberosAttributes = KerberosAttributes 
  { "Realm" :: (XmlStringMaxLen256)
  , "KdcAdminPassword" :: (XmlStringMaxLen256)
  , "CrossRealmTrustPrincipalPassword" :: NullOrUndefined (XmlStringMaxLen256)
  , "ADDomainJoinUser" :: NullOrUndefined (XmlStringMaxLen256)
  , "ADDomainJoinPassword" :: NullOrUndefined (XmlStringMaxLen256)
  }
derive instance newtypeKerberosAttributes :: Newtype KerberosAttributes _


-- | <p>A key value pair.</p>
newtype KeyValue = KeyValue 
  { "Key" :: NullOrUndefined (XmlString)
  , "Value" :: NullOrUndefined (XmlString)
  }
derive instance newtypeKeyValue :: Newtype KeyValue _


newtype KeyValueList = KeyValueList (Array KeyValue)
derive instance newtypeKeyValueList :: Newtype KeyValueList _


-- | <p>This input determines which bootstrap actions to retrieve.</p>
newtype ListBootstrapActionsInput = ListBootstrapActionsInput 
  { "ClusterId" :: (ClusterId)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListBootstrapActionsInput :: Newtype ListBootstrapActionsInput _


-- | <p>This output contains the bootstrap actions detail.</p>
newtype ListBootstrapActionsOutput = ListBootstrapActionsOutput 
  { "BootstrapActions" :: NullOrUndefined (CommandList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListBootstrapActionsOutput :: Newtype ListBootstrapActionsOutput _


-- | <p>This input determines how the ListClusters action filters the list of clusters that it returns.</p>
newtype ListClustersInput = ListClustersInput 
  { "CreatedAfter" :: NullOrUndefined (Date)
  , "CreatedBefore" :: NullOrUndefined (Date)
  , "ClusterStates" :: NullOrUndefined (ClusterStateList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListClustersInput :: Newtype ListClustersInput _


-- | <p>This contains a ClusterSummaryList with the cluster details; for example, the cluster IDs, names, and status.</p>
newtype ListClustersOutput = ListClustersOutput 
  { "Clusters" :: NullOrUndefined (ClusterSummaryList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListClustersOutput :: Newtype ListClustersOutput _


newtype ListInstanceFleetsInput = ListInstanceFleetsInput 
  { "ClusterId" :: (ClusterId)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListInstanceFleetsInput :: Newtype ListInstanceFleetsInput _


newtype ListInstanceFleetsOutput = ListInstanceFleetsOutput 
  { "InstanceFleets" :: NullOrUndefined (InstanceFleetList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListInstanceFleetsOutput :: Newtype ListInstanceFleetsOutput _


-- | <p>This input determines which instance groups to retrieve.</p>
newtype ListInstanceGroupsInput = ListInstanceGroupsInput 
  { "ClusterId" :: (ClusterId)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListInstanceGroupsInput :: Newtype ListInstanceGroupsInput _


-- | <p>This input determines which instance groups to retrieve.</p>
newtype ListInstanceGroupsOutput = ListInstanceGroupsOutput 
  { "InstanceGroups" :: NullOrUndefined (InstanceGroupList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListInstanceGroupsOutput :: Newtype ListInstanceGroupsOutput _


-- | <p>This input determines which instances to list.</p>
newtype ListInstancesInput = ListInstancesInput 
  { "ClusterId" :: (ClusterId)
  , "InstanceGroupId" :: NullOrUndefined (InstanceGroupId)
  , "InstanceGroupTypes" :: NullOrUndefined (InstanceGroupTypeList)
  , "InstanceFleetId" :: NullOrUndefined (InstanceFleetId)
  , "InstanceFleetType" :: NullOrUndefined (InstanceFleetType)
  , "InstanceStates" :: NullOrUndefined (InstanceStateList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListInstancesInput :: Newtype ListInstancesInput _


-- | <p>This output contains the list of instances.</p>
newtype ListInstancesOutput = ListInstancesOutput 
  { "Instances" :: NullOrUndefined (InstanceList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListInstancesOutput :: Newtype ListInstancesOutput _


newtype ListSecurityConfigurationsInput = ListSecurityConfigurationsInput 
  { "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListSecurityConfigurationsInput :: Newtype ListSecurityConfigurationsInput _


newtype ListSecurityConfigurationsOutput = ListSecurityConfigurationsOutput 
  { "SecurityConfigurations" :: NullOrUndefined (SecurityConfigurationList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListSecurityConfigurationsOutput :: Newtype ListSecurityConfigurationsOutput _


-- | <p>This input determines which steps to list.</p>
newtype ListStepsInput = ListStepsInput 
  { "ClusterId" :: (ClusterId)
  , "StepStates" :: NullOrUndefined (StepStateList)
  , "StepIds" :: NullOrUndefined (XmlStringList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListStepsInput :: Newtype ListStepsInput _


-- | <p>This output contains the list of steps returned in reverse order. This means that the last step is the first element in the list.</p>
newtype ListStepsOutput = ListStepsOutput 
  { "Steps" :: NullOrUndefined (StepSummaryList)
  , "Marker" :: NullOrUndefined (Marker)
  }
derive instance newtypeListStepsOutput :: Newtype ListStepsOutput _


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _


newtype MarketType = MarketType String
derive instance newtypeMarketType :: Newtype MarketType _


-- | <p>A CloudWatch dimension, which is specified using a <code>Key</code> (known as a <code>Name</code> in CloudWatch), <code>Value</code> pair. By default, Amazon EMR uses one dimension whose <code>Key</code> is <code>JobFlowID</code> and <code>Value</code> is a variable representing the cluster ID, which is <code>${emr.clusterId}</code>. This enables the rule to bootstrap when the cluster ID becomes available.</p>
newtype MetricDimension = MetricDimension 
  { "Key" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }
derive instance newtypeMetricDimension :: Newtype MetricDimension _


newtype MetricDimensionList = MetricDimensionList (Array MetricDimension)
derive instance newtypeMetricDimensionList :: Newtype MetricDimensionList _


newtype ModifyInstanceFleetInput = ModifyInstanceFleetInput 
  { "ClusterId" :: (ClusterId)
  , "InstanceFleet" :: (InstanceFleetModifyConfig)
  }
derive instance newtypeModifyInstanceFleetInput :: Newtype ModifyInstanceFleetInput _


-- | <p>Change the size of some instance groups.</p>
newtype ModifyInstanceGroupsInput = ModifyInstanceGroupsInput 
  { "ClusterId" :: NullOrUndefined (ClusterId)
  , "InstanceGroups" :: NullOrUndefined (InstanceGroupModifyConfigList)
  }
derive instance newtypeModifyInstanceGroupsInput :: Newtype ModifyInstanceGroupsInput _


newtype NewSupportedProductsList = NewSupportedProductsList (Array SupportedProductConfig)
derive instance newtypeNewSupportedProductsList :: Newtype NewSupportedProductsList _


newtype NonNegativeDouble = NonNegativeDouble Number
derive instance newtypeNonNegativeDouble :: Newtype NonNegativeDouble _


-- | <p>The Amazon EC2 Availability Zone configuration of the cluster (job flow).</p>
newtype PlacementType = PlacementType 
  { "AvailabilityZone" :: NullOrUndefined (XmlString)
  , "AvailabilityZones" :: NullOrUndefined (XmlStringMaxLen256List)
  }
derive instance newtypePlacementType :: Newtype PlacementType _


newtype PutAutoScalingPolicyInput = PutAutoScalingPolicyInput 
  { "ClusterId" :: (ClusterId)
  , "InstanceGroupId" :: (InstanceGroupId)
  , "AutoScalingPolicy" :: (AutoScalingPolicy)
  }
derive instance newtypePutAutoScalingPolicyInput :: Newtype PutAutoScalingPolicyInput _


newtype PutAutoScalingPolicyOutput = PutAutoScalingPolicyOutput 
  { "ClusterId" :: NullOrUndefined (ClusterId)
  , "InstanceGroupId" :: NullOrUndefined (InstanceGroupId)
  , "AutoScalingPolicy" :: NullOrUndefined (AutoScalingPolicyDescription)
  }
derive instance newtypePutAutoScalingPolicyOutput :: Newtype PutAutoScalingPolicyOutput _


newtype RemoveAutoScalingPolicyInput = RemoveAutoScalingPolicyInput 
  { "ClusterId" :: (ClusterId)
  , "InstanceGroupId" :: (InstanceGroupId)
  }
derive instance newtypeRemoveAutoScalingPolicyInput :: Newtype RemoveAutoScalingPolicyInput _


newtype RemoveAutoScalingPolicyOutput = RemoveAutoScalingPolicyOutput 
  { 
  }
derive instance newtypeRemoveAutoScalingPolicyOutput :: Newtype RemoveAutoScalingPolicyOutput _


-- | <p>This input identifies a cluster and a list of tags to remove.</p>
newtype RemoveTagsInput = RemoveTagsInput 
  { "ResourceId" :: (ResourceId)
  , "TagKeys" :: (StringList)
  }
derive instance newtypeRemoveTagsInput :: Newtype RemoveTagsInput _


-- | <p>This output indicates the result of removing tags from a resource.</p>
newtype RemoveTagsOutput = RemoveTagsOutput 
  { 
  }
derive instance newtypeRemoveTagsOutput :: Newtype RemoveTagsOutput _


newtype RepoUpgradeOnBoot = RepoUpgradeOnBoot String
derive instance newtypeRepoUpgradeOnBoot :: Newtype RepoUpgradeOnBoot _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


-- | <p> Input to the <a>RunJobFlow</a> operation. </p>
newtype RunJobFlowInput = RunJobFlowInput 
  { "Name" :: (XmlStringMaxLen256)
  , "LogUri" :: NullOrUndefined (XmlString)
  , "AdditionalInfo" :: NullOrUndefined (XmlString)
  , "AmiVersion" :: NullOrUndefined (XmlStringMaxLen256)
  , "ReleaseLabel" :: NullOrUndefined (XmlStringMaxLen256)
  , "Instances" :: (JobFlowInstancesConfig)
  , "Steps" :: NullOrUndefined (StepConfigList)
  , "BootstrapActions" :: NullOrUndefined (BootstrapActionConfigList)
  , "SupportedProducts" :: NullOrUndefined (SupportedProductsList)
  , "NewSupportedProducts" :: NullOrUndefined (NewSupportedProductsList)
  , "Applications" :: NullOrUndefined (ApplicationList)
  , "Configurations" :: NullOrUndefined (ConfigurationList)
  , "VisibleToAllUsers" :: NullOrUndefined (Boolean)
  , "JobFlowRole" :: NullOrUndefined (XmlString)
  , "ServiceRole" :: NullOrUndefined (XmlString)
  , "Tags" :: NullOrUndefined (TagList)
  , "SecurityConfiguration" :: NullOrUndefined (XmlString)
  , "AutoScalingRole" :: NullOrUndefined (XmlString)
  , "ScaleDownBehavior" :: NullOrUndefined (ScaleDownBehavior)
  , "CustomAmiId" :: NullOrUndefined (XmlStringMaxLen256)
  , "EbsRootVolumeSize" :: NullOrUndefined (Int)
  , "RepoUpgradeOnBoot" :: NullOrUndefined (RepoUpgradeOnBoot)
  , "KerberosAttributes" :: NullOrUndefined (KerberosAttributes)
  }
derive instance newtypeRunJobFlowInput :: Newtype RunJobFlowInput _


-- | <p> The result of the <a>RunJobFlow</a> operation. </p>
newtype RunJobFlowOutput = RunJobFlowOutput 
  { "JobFlowId" :: NullOrUndefined (XmlStringMaxLen256)
  }
derive instance newtypeRunJobFlowOutput :: Newtype RunJobFlowOutput _


newtype ScaleDownBehavior = ScaleDownBehavior String
derive instance newtypeScaleDownBehavior :: Newtype ScaleDownBehavior _


-- | <p>The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.</p>
newtype ScalingAction = ScalingAction 
  { "Market" :: NullOrUndefined (MarketType)
  , "SimpleScalingPolicyConfiguration" :: (SimpleScalingPolicyConfiguration)
  }
derive instance newtypeScalingAction :: Newtype ScalingAction _


-- | <p>The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activities triggered by automatic scaling rules will not cause an instance group to grow above or below these limits.</p>
newtype ScalingConstraints = ScalingConstraints 
  { "MinCapacity" :: (Int)
  , "MaxCapacity" :: (Int)
  }
derive instance newtypeScalingConstraints :: Newtype ScalingConstraints _


-- | <p>A scale-in or scale-out rule that defines scaling activity, including the CloudWatch metric alarm that triggers activity, how EC2 instances are added or removed, and the periodicity of adjustments. The automatic scaling policy for an instance group can comprise one or more automatic scaling rules.</p>
newtype ScalingRule = ScalingRule 
  { "Name" :: (String)
  , "Description" :: NullOrUndefined (String)
  , "Action" :: (ScalingAction)
  , "Trigger" :: (ScalingTrigger)
  }
derive instance newtypeScalingRule :: Newtype ScalingRule _


newtype ScalingRuleList = ScalingRuleList (Array ScalingRule)
derive instance newtypeScalingRuleList :: Newtype ScalingRuleList _


-- | <p>The conditions that trigger an automatic scaling activity.</p>
newtype ScalingTrigger = ScalingTrigger 
  { "CloudWatchAlarmDefinition" :: (CloudWatchAlarmDefinition)
  }
derive instance newtypeScalingTrigger :: Newtype ScalingTrigger _


-- | <p>Configuration of the script to run during a bootstrap action.</p>
newtype ScriptBootstrapActionConfig = ScriptBootstrapActionConfig 
  { "Path" :: (XmlString)
  , "Args" :: NullOrUndefined (XmlStringList)
  }
derive instance newtypeScriptBootstrapActionConfig :: Newtype ScriptBootstrapActionConfig _


newtype SecurityConfigurationList = SecurityConfigurationList (Array SecurityConfigurationSummary)
derive instance newtypeSecurityConfigurationList :: Newtype SecurityConfigurationList _


-- | <p>The creation date and time, and name, of a security configuration.</p>
newtype SecurityConfigurationSummary = SecurityConfigurationSummary 
  { "Name" :: NullOrUndefined (XmlString)
  , "CreationDateTime" :: NullOrUndefined (Date)
  }
derive instance newtypeSecurityConfigurationSummary :: Newtype SecurityConfigurationSummary _


newtype SecurityGroupsList = SecurityGroupsList (Array XmlStringMaxLen256)
derive instance newtypeSecurityGroupsList :: Newtype SecurityGroupsList _


-- | <p> The input argument to the <a>TerminationProtection</a> operation. </p>
newtype SetTerminationProtectionInput = SetTerminationProtectionInput 
  { "JobFlowIds" :: (XmlStringList)
  , "TerminationProtected" :: (Boolean)
  }
derive instance newtypeSetTerminationProtectionInput :: Newtype SetTerminationProtectionInput _


-- | <p>The input to the SetVisibleToAllUsers action.</p>
newtype SetVisibleToAllUsersInput = SetVisibleToAllUsersInput 
  { "JobFlowIds" :: (XmlStringList)
  , "VisibleToAllUsers" :: (Boolean)
  }
derive instance newtypeSetVisibleToAllUsersInput :: Newtype SetVisibleToAllUsersInput _


-- | <p>Policy for customizing shrink operations. Allows configuration of decommissioning timeout and targeted instance shrinking.</p>
newtype ShrinkPolicy = ShrinkPolicy 
  { "DecommissionTimeout" :: NullOrUndefined (Int)
  , "InstanceResizePolicy" :: NullOrUndefined (InstanceResizePolicy)
  }
derive instance newtypeShrinkPolicy :: Newtype ShrinkPolicy _


-- | <p>An automatic scaling configuration, which describes how the policy adds or removes instances, the cooldown period, and the number of EC2 instances that will be added each time the CloudWatch metric alarm condition is satisfied.</p>
newtype SimpleScalingPolicyConfiguration = SimpleScalingPolicyConfiguration 
  { "AdjustmentType" :: NullOrUndefined (AdjustmentType)
  , "ScalingAdjustment" :: (Int)
  , "CoolDown" :: NullOrUndefined (Int)
  }
derive instance newtypeSimpleScalingPolicyConfiguration :: Newtype SimpleScalingPolicyConfiguration _


-- | <p>The launch specification for Spot instances in the instance fleet, which determines the defined duration and provisioning timeout behavior.</p> <note> <p>The instance fleet configuration is available only in Amazon EMR versions 4.8.0 and later, excluding 5.0.x versions.</p> </note>
newtype SpotProvisioningSpecification = SpotProvisioningSpecification 
  { "TimeoutDurationMinutes" :: (WholeNumber)
  , "TimeoutAction" :: (SpotProvisioningTimeoutAction)
  , "BlockDurationMinutes" :: NullOrUndefined (WholeNumber)
  }
derive instance newtypeSpotProvisioningSpecification :: Newtype SpotProvisioningSpecification _


newtype SpotProvisioningTimeoutAction = SpotProvisioningTimeoutAction String
derive instance newtypeSpotProvisioningTimeoutAction :: Newtype SpotProvisioningTimeoutAction _


newtype Statistic = Statistic String
derive instance newtypeStatistic :: Newtype Statistic _


-- | <p>This represents a step in a cluster.</p>
newtype Step = Step 
  { "Id" :: NullOrUndefined (StepId)
  , "Name" :: NullOrUndefined (String)
  , "Config" :: NullOrUndefined (HadoopStepConfig)
  , "ActionOnFailure" :: NullOrUndefined (ActionOnFailure)
  , "Status" :: NullOrUndefined (StepStatus)
  }
derive instance newtypeStep :: Newtype Step _


-- | <p>Specification of a cluster (job flow) step.</p>
newtype StepConfig = StepConfig 
  { "Name" :: (XmlStringMaxLen256)
  , "ActionOnFailure" :: NullOrUndefined (ActionOnFailure)
  , "HadoopJarStep" :: (HadoopJarStepConfig)
  }
derive instance newtypeStepConfig :: Newtype StepConfig _


newtype StepConfigList = StepConfigList (Array StepConfig)
derive instance newtypeStepConfigList :: Newtype StepConfigList _


-- | <p>Combines the execution state and configuration of a step.</p>
newtype StepDetail = StepDetail 
  { "StepConfig" :: (StepConfig)
  , "ExecutionStatusDetail" :: (StepExecutionStatusDetail)
  }
derive instance newtypeStepDetail :: Newtype StepDetail _


newtype StepDetailList = StepDetailList (Array StepDetail)
derive instance newtypeStepDetailList :: Newtype StepDetailList _


newtype StepExecutionState = StepExecutionState String
derive instance newtypeStepExecutionState :: Newtype StepExecutionState _


-- | <p>The execution state of a step.</p>
newtype StepExecutionStatusDetail = StepExecutionStatusDetail 
  { "State" :: (StepExecutionState)
  , "CreationDateTime" :: (Date)
  , "StartDateTime" :: NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined (Date)
  , "LastStateChangeReason" :: NullOrUndefined (XmlString)
  }
derive instance newtypeStepExecutionStatusDetail :: Newtype StepExecutionStatusDetail _


newtype StepId = StepId String
derive instance newtypeStepId :: Newtype StepId _


newtype StepIdsList = StepIdsList (Array XmlStringMaxLen256)
derive instance newtypeStepIdsList :: Newtype StepIdsList _


newtype StepState = StepState String
derive instance newtypeStepState :: Newtype StepState _


-- | <p>The details of the step state change reason.</p>
newtype StepStateChangeReason = StepStateChangeReason 
  { "Code" :: NullOrUndefined (StepStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeStepStateChangeReason :: Newtype StepStateChangeReason _


newtype StepStateChangeReasonCode = StepStateChangeReasonCode String
derive instance newtypeStepStateChangeReasonCode :: Newtype StepStateChangeReasonCode _


newtype StepStateList = StepStateList (Array StepState)
derive instance newtypeStepStateList :: Newtype StepStateList _


-- | <p>The execution status details of the cluster step.</p>
newtype StepStatus = StepStatus 
  { "State" :: NullOrUndefined (StepState)
  , "StateChangeReason" :: NullOrUndefined (StepStateChangeReason)
  , "FailureDetails" :: NullOrUndefined (FailureDetails)
  , "Timeline" :: NullOrUndefined (StepTimeline)
  }
derive instance newtypeStepStatus :: Newtype StepStatus _


-- | <p>The summary of the cluster step.</p>
newtype StepSummary = StepSummary 
  { "Id" :: NullOrUndefined (StepId)
  , "Name" :: NullOrUndefined (String)
  , "Config" :: NullOrUndefined (HadoopStepConfig)
  , "ActionOnFailure" :: NullOrUndefined (ActionOnFailure)
  , "Status" :: NullOrUndefined (StepStatus)
  }
derive instance newtypeStepSummary :: Newtype StepSummary _


newtype StepSummaryList = StepSummaryList (Array StepSummary)
derive instance newtypeStepSummaryList :: Newtype StepSummaryList _


-- | <p>The timeline of the cluster step lifecycle.</p>
newtype StepTimeline = StepTimeline 
  { "CreationDateTime" :: NullOrUndefined (Date)
  , "StartDateTime" :: NullOrUndefined (Date)
  , "EndDateTime" :: NullOrUndefined (Date)
  }
derive instance newtypeStepTimeline :: Newtype StepTimeline _


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _


newtype StringMap = StringMap (Map String String)
derive instance newtypeStringMap :: Newtype StringMap _


-- | <p>The list of supported product configurations which allow user-supplied arguments. EMR accepts these arguments and forwards them to the corresponding installation script as bootstrap action arguments.</p>
newtype SupportedProductConfig = SupportedProductConfig 
  { "Name" :: NullOrUndefined (XmlStringMaxLen256)
  , "Args" :: NullOrUndefined (XmlStringList)
  }
derive instance newtypeSupportedProductConfig :: Newtype SupportedProductConfig _


newtype SupportedProductsList = SupportedProductsList (Array XmlStringMaxLen256)
derive instance newtypeSupportedProductsList :: Newtype SupportedProductsList _


-- | <p>A key/value pair containing user-defined metadata that you can associate with an Amazon EMR resource. Tags make it easier to associate clusters in various ways, such as grouping clusters to track your Amazon EMR resource allocation costs. For more information, see <a href="http://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-plan-tags.html">Tag Clusters</a>. </p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (String)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


-- | <p> Input to the <a>TerminateJobFlows</a> operation. </p>
newtype TerminateJobFlowsInput = TerminateJobFlowsInput 
  { "JobFlowIds" :: (XmlStringList)
  }
derive instance newtypeTerminateJobFlowsInput :: Newtype TerminateJobFlowsInput _


newtype Unit'' = Unit'' String
derive instance newtypeUnit'' :: Newtype Unit'' _


-- | <p>EBS volume specifications such as volume type, IOPS, and size (GiB) that will be requested for the EBS volume attached to an EC2 instance in the cluster.</p>
newtype VolumeSpecification = VolumeSpecification 
  { "VolumeType" :: (String)
  , "Iops" :: NullOrUndefined (Int)
  , "SizeInGB" :: (Int)
  }
derive instance newtypeVolumeSpecification :: Newtype VolumeSpecification _


newtype WholeNumber = WholeNumber Int
derive instance newtypeWholeNumber :: Newtype WholeNumber _


newtype XmlString = XmlString String
derive instance newtypeXmlString :: Newtype XmlString _


newtype XmlStringList = XmlStringList (Array XmlString)
derive instance newtypeXmlStringList :: Newtype XmlStringList _


newtype XmlStringMaxLen256 = XmlStringMaxLen256 String
derive instance newtypeXmlStringMaxLen256 :: Newtype XmlStringMaxLen256 _


newtype XmlStringMaxLen256List = XmlStringMaxLen256List (Array XmlStringMaxLen256)
derive instance newtypeXmlStringMaxLen256List :: Newtype XmlStringMaxLen256List _
