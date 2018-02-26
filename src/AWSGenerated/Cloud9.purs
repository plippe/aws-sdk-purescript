

-- | <fullname>AWS Cloud9</fullname> <p>AWS Cloud9 is a collection of tools that you can use to code, build, run, test, debug, and release software in the cloud.</p> <p>For more information about AWS Cloud9, see the <a href="https://docs.aws.amazon.com/cloud9/latest/user-guide">AWS Cloud9 User Guide</a>.</p> <p>AWS Cloud9 supports these operations:</p> <ul> <li> <p> <code>CreateEnvironmentEC2</code>: Creates an AWS Cloud9 development environment, launches an Amazon EC2 instance, and then connects from the instance to the environment.</p> </li> <li> <p> <code>CreateEnvironmentMembership</code>: Adds an environment member to an environment.</p> </li> <li> <p> <code>DeleteEnvironment</code>: Deletes an environment. If an Amazon EC2 instance is connected to the environment, also terminates the instance.</p> </li> <li> <p> <code>DeleteEnvironmentMembership</code>: Deletes an environment member from an environment.</p> </li> <li> <p> <code>DescribeEnvironmentMemberships</code>: Gets information about environment members for an environment.</p> </li> <li> <p> <code>DescribeEnvironments</code>: Gets information about environments.</p> </li> <li> <p> <code>DescribeEnvironmentStatus</code>: Gets status information for an environment.</p> </li> <li> <p> <code>ListEnvironments</code>: Gets a list of environment identifiers.</p> </li> <li> <p> <code>UpdateEnvironment</code>: Changes the settings of an existing environment.</p> </li> <li> <p> <code>UpdateEnvironmentMembership</code>: Changes the settings of an existing environment member for an environment.</p> </li> </ul>
module AWS.Cloud9 where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Cloud9" :: String


-- | <p>Creates an AWS Cloud9 development environment, launches an Amazon Elastic Compute Cloud (Amazon EC2) instance, and then connects from the instance to the environment.</p>
createEnvironmentEC2 :: forall eff. CreateEnvironmentEC2Request -> Aff (err :: AWS.RequestError | eff) CreateEnvironmentEC2Result
createEnvironmentEC2 = AWS.request serviceName "CreateEnvironmentEC2" 


-- | <p>Adds an environment member to an AWS Cloud9 development environment.</p>
createEnvironmentMembership :: forall eff. CreateEnvironmentMembershipRequest -> Aff (err :: AWS.RequestError | eff) CreateEnvironmentMembershipResult
createEnvironmentMembership = AWS.request serviceName "CreateEnvironmentMembership" 


-- | <p>Deletes an AWS Cloud9 development environment. If an Amazon EC2 instance is connected to the environment, also terminates the instance.</p>
deleteEnvironment :: forall eff. DeleteEnvironmentRequest -> Aff (err :: AWS.RequestError | eff) DeleteEnvironmentResult
deleteEnvironment = AWS.request serviceName "DeleteEnvironment" 


-- | <p>Deletes an environment member from an AWS Cloud9 development environment.</p>
deleteEnvironmentMembership :: forall eff. DeleteEnvironmentMembershipRequest -> Aff (err :: AWS.RequestError | eff) DeleteEnvironmentMembershipResult
deleteEnvironmentMembership = AWS.request serviceName "DeleteEnvironmentMembership" 


-- | <p>Gets information about environment members for an AWS Cloud9 development environment.</p>
describeEnvironmentMemberships :: forall eff. DescribeEnvironmentMembershipsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEnvironmentMembershipsResult
describeEnvironmentMemberships = AWS.request serviceName "DescribeEnvironmentMemberships" 


-- | <p>Gets status information for an AWS Cloud9 development environment.</p>
describeEnvironmentStatus :: forall eff. DescribeEnvironmentStatusRequest -> Aff (err :: AWS.RequestError | eff) DescribeEnvironmentStatusResult
describeEnvironmentStatus = AWS.request serviceName "DescribeEnvironmentStatus" 


-- | <p>Gets information about AWS Cloud9 development environments.</p>
describeEnvironments :: forall eff. DescribeEnvironmentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEnvironmentsResult
describeEnvironments = AWS.request serviceName "DescribeEnvironments" 


-- | <p>Gets a list of AWS Cloud9 development environment identifiers.</p>
listEnvironments :: forall eff. ListEnvironmentsRequest -> Aff (err :: AWS.RequestError | eff) ListEnvironmentsResult
listEnvironments = AWS.request serviceName "ListEnvironments" 


-- | <p>Changes the settings of an existing AWS Cloud9 development environment.</p>
updateEnvironment :: forall eff. UpdateEnvironmentRequest -> Aff (err :: AWS.RequestError | eff) UpdateEnvironmentResult
updateEnvironment = AWS.request serviceName "UpdateEnvironment" 


-- | <p>Changes the settings of an existing environment member for an AWS Cloud9 development environment.</p>
updateEnvironmentMembership :: forall eff. UpdateEnvironmentMembershipRequest -> Aff (err :: AWS.RequestError | eff) UpdateEnvironmentMembershipResult
updateEnvironmentMembership = AWS.request serviceName "UpdateEnvironmentMembership" 


newtype AutomaticStopTimeMinutes = AutomaticStopTimeMinutes Int


-- | <p>The target request is invalid.</p>
newtype BadRequestException = BadRequestException 
  { 
  }


newtype BoundedEnvironmentIdList = BoundedEnvironmentIdList (Array EnvironmentId)


newtype ClientRequestToken = ClientRequestToken String


-- | <p>A conflict occurred.</p>
newtype ConflictException = ConflictException 
  { 
  }


newtype CreateEnvironmentEC2Request = CreateEnvironmentEC2Request 
  { "Name'" :: (EnvironmentName)
  , "Description'" :: NullOrUndefined (EnvironmentDescription)
  , "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken)
  , "InstanceType'" :: (InstanceType)
  , "SubnetId'" :: NullOrUndefined (SubnetId)
  , "AutomaticStopTimeMinutes'" :: NullOrUndefined (AutomaticStopTimeMinutes)
  , "OwnerArn'" :: NullOrUndefined (UserArn)
  }


newtype CreateEnvironmentEC2Result = CreateEnvironmentEC2Result 
  { "EnvironmentId'" :: NullOrUndefined (EnvironmentId)
  }


newtype CreateEnvironmentMembershipRequest = CreateEnvironmentMembershipRequest 
  { "EnvironmentId'" :: (EnvironmentId)
  , "UserArn'" :: (UserArn)
  , "Permissions'" :: (MemberPermissions)
  }


newtype CreateEnvironmentMembershipResult = CreateEnvironmentMembershipResult 
  { "Membership'" :: NullOrUndefined (EnvironmentMember)
  }


newtype DeleteEnvironmentMembershipRequest = DeleteEnvironmentMembershipRequest 
  { "EnvironmentId'" :: (EnvironmentId)
  , "UserArn'" :: (UserArn)
  }


newtype DeleteEnvironmentMembershipResult = DeleteEnvironmentMembershipResult 
  { 
  }


newtype DeleteEnvironmentRequest = DeleteEnvironmentRequest 
  { "EnvironmentId'" :: (EnvironmentId)
  }


newtype DeleteEnvironmentResult = DeleteEnvironmentResult 
  { 
  }


newtype DescribeEnvironmentMembershipsRequest = DescribeEnvironmentMembershipsRequest 
  { "UserArn'" :: NullOrUndefined (UserArn)
  , "EnvironmentId'" :: NullOrUndefined (EnvironmentId)
  , "Permissions'" :: NullOrUndefined (PermissionsList)
  , "NextToken'" :: NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype DescribeEnvironmentMembershipsResult = DescribeEnvironmentMembershipsResult 
  { "Memberships'" :: NullOrUndefined (EnvironmentMembersList)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype DescribeEnvironmentStatusRequest = DescribeEnvironmentStatusRequest 
  { "EnvironmentId'" :: (EnvironmentId)
  }


newtype DescribeEnvironmentStatusResult = DescribeEnvironmentStatusResult 
  { "Status'" :: NullOrUndefined (EnvironmentStatus)
  , "Message'" :: NullOrUndefined (String)
  }


newtype DescribeEnvironmentsRequest = DescribeEnvironmentsRequest 
  { "EnvironmentIds'" :: (BoundedEnvironmentIdList)
  }


newtype DescribeEnvironmentsResult = DescribeEnvironmentsResult 
  { "Environments'" :: NullOrUndefined (EnvironmentList)
  }


-- | <p>Information about an AWS Cloud9 development environment.</p>
newtype Environment = Environment 
  { "Id'" :: NullOrUndefined (EnvironmentId)
  , "Name'" :: NullOrUndefined (EnvironmentName)
  , "Description'" :: NullOrUndefined (EnvironmentDescription)
  , "Type'" :: NullOrUndefined (EnvironmentType)
  , "Arn'" :: NullOrUndefined (String)
  , "OwnerArn'" :: NullOrUndefined (String)
  }


newtype EnvironmentDescription = EnvironmentDescription String


newtype EnvironmentId = EnvironmentId String


newtype EnvironmentIdList = EnvironmentIdList (Array EnvironmentId)


newtype EnvironmentList = EnvironmentList (Array Environment)


-- | <p>Information about an environment member for an AWS Cloud9 development environment.</p>
newtype EnvironmentMember = EnvironmentMember 
  { "Permissions'" :: NullOrUndefined (Permissions)
  , "UserId'" :: NullOrUndefined (String)
  , "UserArn'" :: NullOrUndefined (UserArn)
  , "EnvironmentId'" :: NullOrUndefined (EnvironmentId)
  , "LastAccess'" :: NullOrUndefined (Number)
  }


newtype EnvironmentMembersList = EnvironmentMembersList (Array EnvironmentMember)


newtype EnvironmentName = EnvironmentName String


newtype EnvironmentStatus = EnvironmentStatus String


newtype EnvironmentType = EnvironmentType String


-- | <p>An access permissions issue occurred.</p>
newtype ForbiddenException = ForbiddenException 
  { 
  }


newtype InstanceType = InstanceType String


-- | <p>An internal server error occurred.</p>
newtype InternalServerErrorException = InternalServerErrorException 
  { 
  }


-- | <p>A service limit was exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }


newtype ListEnvironmentsRequest = ListEnvironmentsRequest 
  { "NextToken'" :: NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype ListEnvironmentsResult = ListEnvironmentsResult 
  { "NextToken'" :: NullOrUndefined (String)
  , "EnvironmentIds'" :: NullOrUndefined (EnvironmentIdList)
  }


newtype MaxResults = MaxResults Int


newtype MemberPermissions = MemberPermissions String


-- | <p>The target resource cannot be found.</p>
newtype NotFoundException = NotFoundException 
  { 
  }


newtype Permissions = Permissions String


newtype PermissionsList = PermissionsList (Array Permissions)


newtype SubnetId = SubnetId String


-- | <p>Too many service requests were made over the given time period.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { 
  }


newtype UpdateEnvironmentMembershipRequest = UpdateEnvironmentMembershipRequest 
  { "EnvironmentId'" :: (EnvironmentId)
  , "UserArn'" :: (UserArn)
  , "Permissions'" :: (MemberPermissions)
  }


newtype UpdateEnvironmentMembershipResult = UpdateEnvironmentMembershipResult 
  { "Membership'" :: NullOrUndefined (EnvironmentMember)
  }


newtype UpdateEnvironmentRequest = UpdateEnvironmentRequest 
  { "EnvironmentId'" :: (EnvironmentId)
  , "Name'" :: NullOrUndefined (EnvironmentName)
  , "Description'" :: NullOrUndefined (EnvironmentDescription)
  }


newtype UpdateEnvironmentResult = UpdateEnvironmentResult 
  { 
  }


newtype UserArn = UserArn String
