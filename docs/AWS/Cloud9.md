## Module AWS.Cloud9

<fullname>AWS Cloud9</fullname> <p>AWS Cloud9 is a collection of tools that you can use to code, build, run, test, debug, and release software in the cloud.</p> <p>For more information about AWS Cloud9, see the <a href="https://docs.aws.amazon.com/cloud9/latest/user-guide">AWS Cloud9 User Guide</a>.</p> <p>AWS Cloud9 supports these operations:</p> <ul> <li> <p> <code>CreateEnvironmentEC2</code>: Creates an AWS Cloud9 development environment, launches an Amazon EC2 instance, and then connects from the instance to the environment.</p> </li> <li> <p> <code>CreateEnvironmentMembership</code>: Adds an environment member to an environment.</p> </li> <li> <p> <code>DeleteEnvironment</code>: Deletes an environment. If an Amazon EC2 instance is connected to the environment, also terminates the instance.</p> </li> <li> <p> <code>DeleteEnvironmentMembership</code>: Deletes an environment member from an environment.</p> </li> <li> <p> <code>DescribeEnvironmentMemberships</code>: Gets information about environment members for an environment.</p> </li> <li> <p> <code>DescribeEnvironments</code>: Gets information about environments.</p> </li> <li> <p> <code>DescribeEnvironmentStatus</code>: Gets status information for an environment.</p> </li> <li> <p> <code>ListEnvironments</code>: Gets a list of environment identifiers.</p> </li> <li> <p> <code>UpdateEnvironment</code>: Changes the settings of an existing environment.</p> </li> <li> <p> <code>UpdateEnvironmentMembership</code>: Changes the settings of an existing environment member for an environment.</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createEnvironmentEC2`

``` purescript
createEnvironmentEC2 :: forall eff. CreateEnvironmentEC2Request -> Aff (err :: RequestError | eff) CreateEnvironmentEC2Result
```

<p>Creates an AWS Cloud9 development environment, launches an Amazon Elastic Compute Cloud (Amazon EC2) instance, and then connects from the instance to the environment.</p>

#### `createEnvironmentMembership`

``` purescript
createEnvironmentMembership :: forall eff. CreateEnvironmentMembershipRequest -> Aff (err :: RequestError | eff) CreateEnvironmentMembershipResult
```

<p>Adds an environment member to an AWS Cloud9 development environment.</p>

#### `deleteEnvironment`

``` purescript
deleteEnvironment :: forall eff. DeleteEnvironmentRequest -> Aff (err :: RequestError | eff) DeleteEnvironmentResult
```

<p>Deletes an AWS Cloud9 development environment. If an Amazon EC2 instance is connected to the environment, also terminates the instance.</p>

#### `deleteEnvironmentMembership`

``` purescript
deleteEnvironmentMembership :: forall eff. DeleteEnvironmentMembershipRequest -> Aff (err :: RequestError | eff) DeleteEnvironmentMembershipResult
```

<p>Deletes an environment member from an AWS Cloud9 development environment.</p>

#### `describeEnvironmentMemberships`

``` purescript
describeEnvironmentMemberships :: forall eff. DescribeEnvironmentMembershipsRequest -> Aff (err :: RequestError | eff) DescribeEnvironmentMembershipsResult
```

<p>Gets information about environment members for an AWS Cloud9 development environment.</p>

#### `describeEnvironmentStatus`

``` purescript
describeEnvironmentStatus :: forall eff. DescribeEnvironmentStatusRequest -> Aff (err :: RequestError | eff) DescribeEnvironmentStatusResult
```

<p>Gets status information for an AWS Cloud9 development environment.</p>

#### `describeEnvironments`

``` purescript
describeEnvironments :: forall eff. DescribeEnvironmentsRequest -> Aff (err :: RequestError | eff) DescribeEnvironmentsResult
```

<p>Gets information about AWS Cloud9 development environments.</p>

#### `listEnvironments`

``` purescript
listEnvironments :: forall eff. ListEnvironmentsRequest -> Aff (err :: RequestError | eff) ListEnvironmentsResult
```

<p>Gets a list of AWS Cloud9 development environment identifiers.</p>

#### `updateEnvironment`

``` purescript
updateEnvironment :: forall eff. UpdateEnvironmentRequest -> Aff (err :: RequestError | eff) UpdateEnvironmentResult
```

<p>Changes the settings of an existing AWS Cloud9 development environment.</p>

#### `updateEnvironmentMembership`

``` purescript
updateEnvironmentMembership :: forall eff. UpdateEnvironmentMembershipRequest -> Aff (err :: RequestError | eff) UpdateEnvironmentMembershipResult
```

<p>Changes the settings of an existing environment member for an AWS Cloud9 development environment.</p>

#### `AutomaticStopTimeMinutes`

``` purescript
newtype AutomaticStopTimeMinutes
  = AutomaticStopTimeMinutes Int
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException {  }
```

<p>The target request is invalid.</p>

#### `BoundedEnvironmentIdList`

``` purescript
newtype BoundedEnvironmentIdList
  = BoundedEnvironmentIdList (Array EnvironmentId)
```

#### `ClientRequestToken`

``` purescript
newtype ClientRequestToken
  = ClientRequestToken String
```

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException {  }
```

<p>A conflict occurred.</p>

#### `CreateEnvironmentEC2Request`

``` purescript
newtype CreateEnvironmentEC2Request
  = CreateEnvironmentEC2Request { "Name'" :: EnvironmentName, "Description'" :: NullOrUndefined (EnvironmentDescription), "ClientRequestToken'" :: NullOrUndefined (ClientRequestToken), "InstanceType'" :: InstanceType, "SubnetId'" :: NullOrUndefined (SubnetId), "AutomaticStopTimeMinutes'" :: NullOrUndefined (AutomaticStopTimeMinutes), "OwnerArn'" :: NullOrUndefined (UserArn) }
```

#### `CreateEnvironmentEC2Result`

``` purescript
newtype CreateEnvironmentEC2Result
  = CreateEnvironmentEC2Result { "EnvironmentId'" :: NullOrUndefined (EnvironmentId) }
```

#### `CreateEnvironmentMembershipRequest`

``` purescript
newtype CreateEnvironmentMembershipRequest
  = CreateEnvironmentMembershipRequest { "EnvironmentId'" :: EnvironmentId, "UserArn'" :: UserArn, "Permissions'" :: MemberPermissions }
```

#### `CreateEnvironmentMembershipResult`

``` purescript
newtype CreateEnvironmentMembershipResult
  = CreateEnvironmentMembershipResult { "Membership'" :: NullOrUndefined (EnvironmentMember) }
```

#### `DeleteEnvironmentMembershipRequest`

``` purescript
newtype DeleteEnvironmentMembershipRequest
  = DeleteEnvironmentMembershipRequest { "EnvironmentId'" :: EnvironmentId, "UserArn'" :: UserArn }
```

#### `DeleteEnvironmentMembershipResult`

``` purescript
newtype DeleteEnvironmentMembershipResult
  = DeleteEnvironmentMembershipResult {  }
```

#### `DeleteEnvironmentRequest`

``` purescript
newtype DeleteEnvironmentRequest
  = DeleteEnvironmentRequest { "EnvironmentId'" :: EnvironmentId }
```

#### `DeleteEnvironmentResult`

``` purescript
newtype DeleteEnvironmentResult
  = DeleteEnvironmentResult {  }
```

#### `DescribeEnvironmentMembershipsRequest`

``` purescript
newtype DescribeEnvironmentMembershipsRequest
  = DescribeEnvironmentMembershipsRequest { "UserArn'" :: NullOrUndefined (UserArn), "EnvironmentId'" :: NullOrUndefined (EnvironmentId), "Permissions'" :: NullOrUndefined (PermissionsList), "NextToken'" :: NullOrUndefined (String), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `DescribeEnvironmentMembershipsResult`

``` purescript
newtype DescribeEnvironmentMembershipsResult
  = DescribeEnvironmentMembershipsResult { "Memberships'" :: NullOrUndefined (EnvironmentMembersList), "NextToken'" :: NullOrUndefined (String) }
```

#### `DescribeEnvironmentStatusRequest`

``` purescript
newtype DescribeEnvironmentStatusRequest
  = DescribeEnvironmentStatusRequest { "EnvironmentId'" :: EnvironmentId }
```

#### `DescribeEnvironmentStatusResult`

``` purescript
newtype DescribeEnvironmentStatusResult
  = DescribeEnvironmentStatusResult { "Status'" :: NullOrUndefined (EnvironmentStatus), "Message'" :: NullOrUndefined (String) }
```

#### `DescribeEnvironmentsRequest`

``` purescript
newtype DescribeEnvironmentsRequest
  = DescribeEnvironmentsRequest { "EnvironmentIds'" :: BoundedEnvironmentIdList }
```

#### `DescribeEnvironmentsResult`

``` purescript
newtype DescribeEnvironmentsResult
  = DescribeEnvironmentsResult { "Environments'" :: NullOrUndefined (EnvironmentList) }
```

#### `Environment`

``` purescript
newtype Environment
  = Environment { "Id'" :: NullOrUndefined (EnvironmentId), "Name'" :: NullOrUndefined (EnvironmentName), "Description'" :: NullOrUndefined (EnvironmentDescription), "Type'" :: NullOrUndefined (EnvironmentType), "Arn'" :: NullOrUndefined (String), "OwnerArn'" :: NullOrUndefined (String) }
```

<p>Information about an AWS Cloud9 development environment.</p>

#### `EnvironmentDescription`

``` purescript
newtype EnvironmentDescription
  = EnvironmentDescription String
```

#### `EnvironmentId`

``` purescript
newtype EnvironmentId
  = EnvironmentId String
```

#### `EnvironmentIdList`

``` purescript
newtype EnvironmentIdList
  = EnvironmentIdList (Array EnvironmentId)
```

#### `EnvironmentList`

``` purescript
newtype EnvironmentList
  = EnvironmentList (Array Environment)
```

#### `EnvironmentMember`

``` purescript
newtype EnvironmentMember
  = EnvironmentMember { "Permissions'" :: NullOrUndefined (Permissions), "UserId'" :: NullOrUndefined (String), "UserArn'" :: NullOrUndefined (UserArn), "EnvironmentId'" :: NullOrUndefined (EnvironmentId), "LastAccess'" :: NullOrUndefined (Number) }
```

<p>Information about an environment member for an AWS Cloud9 development environment.</p>

#### `EnvironmentMembersList`

``` purescript
newtype EnvironmentMembersList
  = EnvironmentMembersList (Array EnvironmentMember)
```

#### `EnvironmentName`

``` purescript
newtype EnvironmentName
  = EnvironmentName String
```

#### `EnvironmentStatus`

``` purescript
newtype EnvironmentStatus
  = EnvironmentStatus String
```

#### `EnvironmentType`

``` purescript
newtype EnvironmentType
  = EnvironmentType String
```

#### `ForbiddenException`

``` purescript
newtype ForbiddenException
  = ForbiddenException {  }
```

<p>An access permissions issue occurred.</p>

#### `InstanceType`

``` purescript
newtype InstanceType
  = InstanceType String
```

#### `InternalServerErrorException`

``` purescript
newtype InternalServerErrorException
  = InternalServerErrorException {  }
```

<p>An internal server error occurred.</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>A service limit was exceeded.</p>

#### `ListEnvironmentsRequest`

``` purescript
newtype ListEnvironmentsRequest
  = ListEnvironmentsRequest { "NextToken'" :: NullOrUndefined (String), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `ListEnvironmentsResult`

``` purescript
newtype ListEnvironmentsResult
  = ListEnvironmentsResult { "NextToken'" :: NullOrUndefined (String), "EnvironmentIds'" :: NullOrUndefined (EnvironmentIdList) }
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `MemberPermissions`

``` purescript
newtype MemberPermissions
  = MemberPermissions String
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException {  }
```

<p>The target resource cannot be found.</p>

#### `Permissions`

``` purescript
newtype Permissions
  = Permissions String
```

#### `PermissionsList`

``` purescript
newtype PermissionsList
  = PermissionsList (Array Permissions)
```

#### `SubnetId`

``` purescript
newtype SubnetId
  = SubnetId String
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException {  }
```

<p>Too many service requests were made over the given time period.</p>

#### `UpdateEnvironmentMembershipRequest`

``` purescript
newtype UpdateEnvironmentMembershipRequest
  = UpdateEnvironmentMembershipRequest { "EnvironmentId'" :: EnvironmentId, "UserArn'" :: UserArn, "Permissions'" :: MemberPermissions }
```

#### `UpdateEnvironmentMembershipResult`

``` purescript
newtype UpdateEnvironmentMembershipResult
  = UpdateEnvironmentMembershipResult { "Membership'" :: NullOrUndefined (EnvironmentMember) }
```

#### `UpdateEnvironmentRequest`

``` purescript
newtype UpdateEnvironmentRequest
  = UpdateEnvironmentRequest { "EnvironmentId'" :: EnvironmentId, "Name'" :: NullOrUndefined (EnvironmentName), "Description'" :: NullOrUndefined (EnvironmentDescription) }
```

#### `UpdateEnvironmentResult`

``` purescript
newtype UpdateEnvironmentResult
  = UpdateEnvironmentResult {  }
```

#### `UserArn`

``` purescript
newtype UserArn
  = UserArn String
```


