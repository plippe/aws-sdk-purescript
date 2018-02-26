## Module AWS.GameLift

<fullname>Amazon GameLift Service</fullname> <p> Amazon GameLift is a managed service for developers who need a scalable, dedicated server solution for their multiplayer games. Use Amazon GameLift for these tasks: (1) set up computing resources and deploy your game servers, (2) run game sessions and get players into games, (3) automatically scale your resources to meet player demand and manage costs, and (4) track in-depth metrics on game server performance and player usage.</p> <p>The Amazon GameLift service API includes two important function sets:</p> <ul> <li> <p> <b>Manage game sessions and player access</b> -- Retrieve information on available game sessions; create new game sessions; send player requests to join a game session.</p> </li> <li> <p> <b>Configure and manage game server resources</b> -- Manage builds, fleets, queues, and aliases; set autoscaling policies; retrieve logs and metrics.</p> </li> </ul> <p>This reference guide describes the low-level service API for Amazon GameLift. You can use the API functionality with these tools: </p> <ul> <li> <p>The Amazon Web Services software development kit (<a href="http://aws.amazon.com/tools/#sdk">AWS SDK</a>) is available in <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-supported.html#gamelift-supported-clients">multiple languages</a> including C++ and C#. Use the SDK to access the API programmatically from an application, such as a game client.</p> </li> <li> <p>The <a href="http://aws.amazon.com/cli/">AWS command-line interface</a> (CLI) tool is primarily useful for handling administrative actions, such as setting up and managing Amazon GameLift settings and resources. You can use the AWS CLI to manage all of your AWS services.</p> </li> <li> <p>The <a href="https://console.aws.amazon.com/gamelift/home">AWS Management Console</a> for Amazon GameLift provides a web interface to manage your Amazon GameLift settings and resources. The console includes a dashboard for tracking key resources, including builds and fleets, and displays usage and performance metrics for your games as customizable graphs.</p> </li> <li> <p>Amazon GameLift Local is a tool for testing your game's integration with Amazon GameLift before deploying it on the service. This tools supports a subset of key API actions, which can be called from either the AWS CLI or programmatically. See <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/integration-testing-local.html">Testing an Integration</a>.</p> </li> </ul> <p> <b>Learn more</b> </p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/"> Developer Guide</a> -- Read about Amazon GameLift features and how to use them. </p> </li> <li> <p> <a href="https://gamedev.amazon.com/forums/tutorials">Tutorials</a> -- Get started fast with walkthroughs and sample projects.</p> </li> <li> <p> <a href="http://aws.amazon.com/blogs/gamedev/">GameDev Blog</a> -- Stay up to date with new features and techniques.</p> </li> <li> <p> <a href="https://gamedev.amazon.com/forums/spaces/123/gamelift-discussion.html">GameDev Forums</a> -- Connect with the GameDev community.</p> </li> <li> <p> <a href="http://aws.amazon.com/releasenotes/Amazon-GameLift/">Release notes</a> and <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/doc-history.html">document history</a> -- Stay current with updates to the Amazon GameLift service, SDKs, and documentation. </p> </li> </ul> <p> <b>API SUMMARY</b> </p> <p>This list offers a functional overview of the Amazon GameLift service API.</p> <p> <b>Managing Games and Players</b> </p> <p>Use these actions to start new game sessions, find existing game sessions, track game session status and other information, and enable player access to game sessions.</p> <ul> <li> <p> <b>Discover existing game sessions</b> </p> <ul> <li> <p> <a>SearchGameSessions</a> -- Retrieve all available game sessions or search for game sessions that match a set of criteria. </p> </li> </ul> </li> <li> <p> <b>Start new game sessions</b> </p> <ul> <li> <p>Start new games with Queues to find the best available hosting resources across multiple regions, minimize player latency, and balance game session activity for efficiency and cost effectiveness. </p> <ul> <li> <p> <a>StartGameSessionPlacement</a> -- Request a new game session placement and add one or more players to it.</p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> -- Get details on a placement request, including status.</p> </li> <li> <p> <a>StopGameSessionPlacement</a> -- Cancel a placement request. </p> </li> </ul> </li> <li> <p> <a>CreateGameSession</a> -- Start a new game session on a specific fleet. <i>Available in Amazon GameLift Local.</i> </p> </li> </ul> </li> <li> <p> <b>Match players to game sessions with FlexMatch matchmaking</b> </p> <ul> <li> <p> <a>StartMatchmaking</a> -- Request matchmaking for one players or a group who want to play together. </p> </li> <li> <p> <a>StartMatchBackfill</a> - Request additional player matches to fill empty slots in an existing game session. </p> </li> <li> <p> <a>DescribeMatchmaking</a> -- Get details on a matchmaking request, including status.</p> </li> <li> <p> <a>AcceptMatch</a> -- Register that a player accepts a proposed match, for matches that require player acceptance. </p> </li> <li> <p> <a>StopMatchmaking</a> -- Cancel a matchmaking request. </p> </li> </ul> </li> <li> <p> <b>Manage game session data</b> </p> <ul> <li> <p> <a>DescribeGameSessions</a> -- Retrieve metadata for one or more game sessions, including length of time active and current player count. <i>Available in Amazon GameLift Local.</i> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> -- Retrieve metadata and the game session protection setting for one or more game sessions.</p> </li> <li> <p> <a>UpdateGameSession</a> -- Change game session settings, such as maximum player count and join policy.</p> </li> <li> <p> <a>GetGameSessionLogUrl</a> -- Get the location of saved logs for a game session.</p> </li> </ul> </li> <li> <p> <b>Manage player sessions</b> </p> <ul> <li> <p> <a>CreatePlayerSession</a> -- Send a request for a player to join a game session. <i>Available in Amazon GameLift Local.</i> </p> </li> <li> <p> <a>CreatePlayerSessions</a> -- Send a request for multiple players to join a game session. <i>Available in Amazon GameLift Local.</i> </p> </li> <li> <p> <a>DescribePlayerSessions</a> -- Get details on player activity, including status, playing time, and player data. <i>Available in Amazon GameLift Local.</i> </p> </li> </ul> </li> </ul> <p> <b>Setting Up and Managing Game Servers</b> </p> <p>When setting up Amazon GameLift resources for your game, you first <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html">create a game build</a> and upload it to Amazon GameLift. You can then use these actions to configure and manage a fleet of resources to run your game servers, scale capacity to meet player demand, access performance and utilization metrics, and more.</p> <ul> <li> <p> <b>Manage game builds</b> </p> <ul> <li> <p> <a>CreateBuild</a> -- Create a new build using files stored in an Amazon S3 bucket. To create a build and upload files from a local path, use the AWS CLI command <code>upload-build</code>.</p> </li> <li> <p> <a>ListBuilds</a> -- Get a list of all builds uploaded to a Amazon GameLift region.</p> </li> <li> <p> <a>DescribeBuild</a> -- Retrieve information associated with a build.</p> </li> <li> <p> <a>UpdateBuild</a> -- Change build metadata, including build name and version.</p> </li> <li> <p> <a>DeleteBuild</a> -- Remove a build from Amazon GameLift.</p> </li> </ul> </li> <li> <p> <b>Manage fleets</b> </p> <ul> <li> <p> <a>CreateFleet</a> -- Configure and activate a new fleet to run a build's game servers.</p> </li> <li> <p> <a>ListFleets</a> -- Get a list of all fleet IDs in a Amazon GameLift region (all statuses).</p> </li> <li> <p> <a>DeleteFleet</a> -- Terminate a fleet that is no longer running game servers or hosting players.</p> </li> <li> <p>View / update fleet configurations.</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> / <a>UpdateFleetAttributes</a> -- View or change a fleet's metadata and settings for game session protection and resource creation limits.</p> </li> <li> <p> <a>DescribeFleetPortSettings</a> / <a>UpdateFleetPortSettings</a> -- View or change the inbound permissions (IP address and port setting ranges) allowed for a fleet.</p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> / <a>UpdateRuntimeConfiguration</a> -- View or change what server processes (and how many) to run on each instance in a fleet.</p> </li> </ul> </li> </ul> </li> <li> <p> <b>Control fleet capacity</b> </p> <ul> <li> <p> <a>DescribeEC2InstanceLimits</a> -- Retrieve maximum number of instances allowed for the current AWS account and the current usage level.</p> </li> <li> <p> <a>DescribeFleetCapacity</a> / <a>UpdateFleetCapacity</a> -- Retrieve the capacity settings and the current number of instances in a fleet; adjust fleet capacity settings to scale up or down.</p> </li> <li> <p>Autoscale -- Manage autoscaling rules and apply them to a fleet.</p> <ul> <li> <p> <a>PutScalingPolicy</a> -- Create a new autoscaling policy, or update an existing one.</p> </li> <li> <p> <a>DescribeScalingPolicies</a> -- Retrieve an existing autoscaling policy.</p> </li> <li> <p> <a>DeleteScalingPolicy</a> -- Delete an autoscaling policy and stop it from affecting a fleet's capacity.</p> </li> </ul> </li> </ul> </li> <li> <p> <b>Manage VPC peering connections for fleets</b> </p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> -- Authorize a peering connection to one of your VPCs.</p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> -- Retrieve valid peering connection authorizations. </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> -- Delete a peering connection authorization.</p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> -- Establish a peering connection between the VPC for a Amazon GameLift fleet and one of your VPCs.</p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> -- Retrieve information on active or pending VPC peering connections with a Amazon GameLift fleet.</p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> -- Delete a VPC peering connection with a Amazon GameLift fleet.</p> </li> </ul> </li> <li> <p> <b>Access fleet activity statistics</b> </p> <ul> <li> <p> <a>DescribeFleetUtilization</a> -- Get current data on the number of server processes, game sessions, and players currently active on a fleet.</p> </li> <li> <p> <a>DescribeFleetEvents</a> -- Get a fleet's logged events for a specified time span.</p> </li> <li> <p> <a>DescribeGameSessions</a> -- Retrieve metadata associated with one or more game sessions, including length of time active and current player count.</p> </li> </ul> </li> <li> <p> <b>Remotely access an instance</b> </p> <ul> <li> <p> <a>DescribeInstances</a> -- Get information on each instance in a fleet, including instance ID, IP address, and status.</p> </li> <li> <p> <a>GetInstanceAccess</a> -- Request access credentials needed to remotely connect to a specified instance in a fleet.</p> </li> </ul> </li> <li> <p> <b>Manage fleet aliases</b> </p> <ul> <li> <p> <a>CreateAlias</a> -- Define a new alias and optionally assign it to a fleet.</p> </li> <li> <p> <a>ListAliases</a> -- Get all fleet aliases defined in a Amazon GameLift region.</p> </li> <li> <p> <a>DescribeAlias</a> -- Retrieve information on an existing alias.</p> </li> <li> <p> <a>UpdateAlias</a> -- Change settings for a alias, such as redirecting it from one fleet to another.</p> </li> <li> <p> <a>DeleteAlias</a> -- Remove an alias from the region.</p> </li> <li> <p> <a>ResolveAlias</a> -- Get the fleet ID that a specified alias points to.</p> </li> </ul> </li> <li> <p> <b>Manage game session queues</b> </p> <ul> <li> <p> <a>CreateGameSessionQueue</a> -- Create a queue for processing requests for new game sessions. </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> -- Retrieve game session queues defined in a Amazon GameLift region.</p> </li> <li> <p> <a>UpdateGameSessionQueue</a> -- Change the configuration of a game session queue.</p> </li> <li> <p> <a>DeleteGameSessionQueue</a> -- Remove a game session queue from the region.</p> </li> </ul> </li> <li> <p> <b>Manage FlexMatch resources</b> </p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> -- Create a matchmaking configuration with instructions for building a player group and placing in a new game session. </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> -- Retrieve matchmaking configurations defined a Amazon GameLift region.</p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> -- Change settings for matchmaking configuration. queue.</p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> -- Remove a matchmaking configuration from the region.</p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> -- Create a set of rules to use when searching for player matches. </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> -- Retrieve matchmaking rule sets defined in a Amazon GameLift region.</p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> -- Verify syntax for a set of matchmaking rules. </p> </li> </ul> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `acceptMatch`

``` purescript
acceptMatch :: forall eff. AcceptMatchInput -> Aff (err :: RequestError | eff) AcceptMatchOutput
```

<p>Registers a player's acceptance or rejection of a proposed FlexMatch match. A matchmaking configuration may require player acceptance; if so, then matches built with that configuration cannot be completed unless all players accept the proposed match within a specified time limit. </p> <p>When FlexMatch builds a match, all the matchmaking tickets involved in the proposed match are placed into status <code>REQUIRES_ACCEPTANCE</code>. This is a trigger for your game to get acceptance from all players in the ticket. Acceptances are only valid for tickets when they are in this status; all other acceptances result in an error.</p> <p>To register acceptance, specify the ticket ID, a response, and one or more players. Once all players have registered acceptance, the matchmaking tickets advance to status <code>PLACING</code>, where a new game session is created for the match. </p> <p>If any player rejects the match, or if acceptances are not received before a specified timeout, the proposed match is dropped. The matchmaking tickets are then handled in one of two ways: For tickets where all players accepted the match, the ticket status is returned to <code>SEARCHING</code> to find a new match. For tickets where one or more players failed to accept the match, the ticket status is set to <code>FAILED</code>, and processing is terminated. A new matchmaking request for these players can be submitted as needed. </p> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>

#### `createAlias`

``` purescript
createAlias :: forall eff. CreateAliasInput -> Aff (err :: RequestError | eff) CreateAliasOutput
```

<p>Creates an alias for a fleet. In most situations, you can use an alias ID in place of a fleet ID. By using a fleet alias instead of a specific fleet ID, you can switch gameplay and players to a new fleet without changing your game client or other game components. For example, for games in production, using an alias allows you to seamlessly redirect your player base to a new game server update. </p> <p>Amazon GameLift supports two types of routing strategies for aliases: simple and terminal. A simple alias points to an active fleet. A terminal alias is used to display messaging or link to a URL instead of routing players to an active fleet. For example, you might use a terminal alias when a game version is no longer supported and you want to direct players to an upgrade site. </p> <p>To create a fleet alias, specify an alias name, routing strategy, and optional description. Each simple alias can point to only one fleet, but a fleet can have multiple aliases. If successful, a new alias record is returned, including an alias ID, which you can reference when creating a game session. You can reassign an alias to another fleet by calling <code>UpdateAlias</code>.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>

#### `createBuild`

``` purescript
createBuild :: forall eff. CreateBuildInput -> Aff (err :: RequestError | eff) CreateBuildOutput
```

<p>Creates a new Amazon GameLift build record for your game server binary files and points to the location of your game server build files in an Amazon Simple Storage Service (Amazon S3) location. </p> <p>Game server binaries must be combined into a <code>.zip</code> file for use with Amazon GameLift. See <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-intro.html">Uploading Your Game</a> for more information. </p> <important> <p>To create new builds quickly and easily, use the AWS CLI command <b> <a href="http://docs.aws.amazon.com/cli/latest/reference/gamelift/upload-build.html">upload-build</a> </b>. This helper command uploads your build and creates a new build record in one step, and automatically handles the necessary permissions. See <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html"> Upload Build Files to Amazon GameLift</a> for more help.</p> </important> <p>The <code>CreateBuild</code> operation should be used only when you need to manually upload your build files, as in the following scenarios:</p> <ul> <li> <p>Store a build file in an Amazon S3 bucket under your own AWS account. To use this option, you must first give Amazon GameLift access to that Amazon S3 bucket. See <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build"> Create a Build with Files in Amazon S3</a> for detailed help. To create a new build record using files in your Amazon S3 bucket, call <code>CreateBuild</code> and specify a build name, operating system, and the storage location of your game build.</p> </li> <li> <p>Upload a build file directly to Amazon GameLift's Amazon S3 account. To use this option, you first call <code>CreateBuild</code> with a build name and operating system. This action creates a new build record and returns an Amazon S3 storage location (bucket and key only) and temporary access credentials. Use the credentials to manually upload your build file to the storage location (see the Amazon S3 topic <a href="http://docs.aws.amazon.com/AmazonS3/latest/dev/UploadingObjects.html">Uploading Objects</a>). You can upload files to a location only once. </p> </li> </ul> <p>If successful, this operation creates a new build record with a unique build ID and places it in <code>INITIALIZED</code> status. You can use <a>DescribeBuild</a> to check the status of your build. A build must be in <code>READY</code> status before it can be used to create fleets.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>

#### `createFleet`

``` purescript
createFleet :: forall eff. CreateFleetInput -> Aff (err :: RequestError | eff) CreateFleetOutput
```

<p>Creates a new fleet to run your game servers. A fleet is a set of Amazon Elastic Compute Cloud (Amazon EC2) instances, each of which can run multiple server processes to host game sessions. You set up a fleet to use instances with certain hardware specifications (see <a href="http://aws.amazon.com/ec2/instance-types/">Amazon EC2 Instance Types</a> for more information), and deploy your game build to run on each instance. </p> <p>To create a new fleet, you must specify the following: (1) a fleet name, (2) the build ID of a successfully uploaded game build, (3) an EC2 instance type, and (4) a run-time configuration, which describes the server processes to run on each instance in the fleet. If you don't specify a fleet type (on-demand or spot), the new fleet uses on-demand instances by default.</p> <p>You can also configure the new fleet with the following settings:</p> <ul> <li> <p>Fleet description</p> </li> <li> <p>Access permissions for inbound traffic</p> </li> <li> <p>Fleet-wide game session protection</p> </li> <li> <p>Resource usage limits</p> </li> </ul> <ul> <li> <p>VPC peering connection (see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html">VPC Peering with Amazon GameLift Fleets</a>)</p> </li> </ul> <p>If you use Amazon CloudWatch for metrics, you can add the new fleet to a metric group. By adding multiple fleets to a metric group, you can view aggregated metrics for all the fleets in the group. </p> <p>If the <code>CreateFleet</code> call is successful, Amazon GameLift performs the following tasks. You can track the process of a fleet by checking the fleet status or by monitoring fleet creation events:</p> <ul> <li> <p>Creates a fleet record. Status: <code>NEW</code>.</p> </li> <li> <p>Begins writing events to the fleet event log, which can be accessed in the Amazon GameLift console.</p> <p>Sets the fleet's target capacity to 1 (desired instances), which triggers Amazon GameLift to start one new EC2 instance.</p> </li> <li> <p>Downloads the game build to the new instance and installs it. Statuses: <code>DOWNLOADING</code>, <code>VALIDATING</code>, <code>BUILDING</code>. </p> </li> <li> <p>Starts launching server processes on the instance. If the fleet is configured to run multiple server processes per instance, Amazon GameLift staggers each launch by a few seconds. Status: <code>ACTIVATING</code>.</p> </li> <li> <p>Sets the fleet's status to <code>ACTIVE</code> as soon as one server process is ready to host a game session.</p> </li> </ul> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `createGameSession`

``` purescript
createGameSession :: forall eff. CreateGameSessionInput -> Aff (err :: RequestError | eff) CreateGameSessionOutput
```

<p>Creates a multiplayer game session for players. This action creates a game session record and assigns an available server process in the specified fleet to host the game session. A fleet must have an <code>ACTIVE</code> status before a game session can be created in it.</p> <p>To create a game session, specify either fleet ID or alias ID and indicate a maximum number of players to allow in the game session. You can also provide a name and game-specific properties for this game session. If successful, a <a>GameSession</a> object is returned containing the game session properties and other settings you specified.</p> <p> <b>Idempotency tokens.</b> You can add a token that uniquely identifies game session requests. This is useful for ensuring that game session requests are idempotent. Multiple requests with the same idempotency token are processed only once; subsequent requests return the original result. All response values are the same with the exception of game session status, which may change.</p> <p> <b>Resource creation limits.</b> If you are creating a game session on a fleet with a resource creation limit policy in force, then you must specify a creator ID. Without this ID, Amazon GameLift has no way to evaluate the policy for this new game session request.</p> <p> <b>Player acceptance policy.</b> By default, newly created game sessions are open to new players. You can restrict new player access by using <a>UpdateGameSession</a> to change the game session's player session creation policy.</p> <p> <b>Game session logs.</b> Logs are retained for all active game sessions for 14 days. To access the logs, call <a>GetGameSessionLogUrl</a> to download the log files.</p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `createGameSessionQueue`

``` purescript
createGameSessionQueue :: forall eff. CreateGameSessionQueueInput -> Aff (err :: RequestError | eff) CreateGameSessionQueueOutput
```

<p>Establishes a new queue for processing requests to place new game sessions. A queue identifies where new game sessions can be hosted -- by specifying a list of destinations (fleets or aliases) -- and how long requests can wait in the queue before timing out. You can set up a queue to try to place game sessions on fleets in multiple regions. To add placement requests to a queue, call <a>StartGameSessionPlacement</a> and reference the queue name.</p> <p> <b>Destination order.</b> When processing a request for a game session, Amazon GameLift tries each destination in order until it finds one with available resources to host the new game session. A queue's default order is determined by how destinations are listed. The default order is overridden when a game session placement request provides player latency information. Player latency information enables Amazon GameLift to prioritize destinations where players report the lowest average latency, as a result placing the new game session where the majority of players will have the best possible gameplay experience.</p> <p> <b>Player latency policies.</b> For placement requests containing player latency information, use player latency policies to protect individual players from very high latencies. With a latency cap, even when a destination can deliver a low latency for most players, the game is not placed where any individual player is reporting latency higher than a policy's maximum. A queue can have multiple latency policies, which are enforced consecutively starting with the policy with the lowest latency cap. Use multiple policies to gradually relax latency controls; for example, you might set a policy with a low latency cap for the first 60 seconds, a second policy with a higher cap for the next 60 seconds, etc. </p> <p>To create a new queue, provide a name, timeout value, a list of destinations and, if desired, a set of latency policies. If successful, a new queue object is returned.</p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>

#### `createMatchmakingConfiguration`

``` purescript
createMatchmakingConfiguration :: forall eff. CreateMatchmakingConfigurationInput -> Aff (err :: RequestError | eff) CreateMatchmakingConfigurationOutput
```

<p>Defines a new matchmaking configuration for use with FlexMatch. A matchmaking configuration sets out guidelines for matching players and getting the matches into games. You can set up multiple matchmaking configurations to handle the scenarios needed for your game. Each matchmaking ticket (<a>StartMatchmaking</a> or <a>StartMatchBackfill</a>) specifies a configuration for the match and provides player attributes to support the configuration being used. </p> <p>To create a matchmaking configuration, at a minimum you must specify the following: configuration name; a rule set that governs how to evaluate players and find acceptable matches; a game session queue to use when placing a new game session for the match; and the maximum time allowed for a matchmaking attempt.</p> <p> <b>Player acceptance</b> -- In each configuration, you have the option to require that all players accept participation in a proposed match. To enable this feature, set <i>AcceptanceRequired</i> to true and specify a time limit for player acceptance. Players have the option to accept or reject a proposed match, and a match does not move ahead to game session placement unless all matched players accept. </p> <p> <b>Matchmaking status notification</b> -- There are two ways to track the progress of matchmaking tickets: (1) polling ticket status with <a>DescribeMatchmaking</a>; or (2) receiving notifications with Amazon Simple Notification Service (SNS). To use notifications, you first need to set up an SNS topic to receive the notifications, and provide the topic ARN in the matchmaking configuration (see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-notification.html"> Setting up Notifications for Matchmaking</a>). Since notifications promise only "best effort" delivery, we recommend calling <code>DescribeMatchmaking</code> if no notifications are received within 30 seconds.</p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>

#### `createMatchmakingRuleSet`

``` purescript
createMatchmakingRuleSet :: forall eff. CreateMatchmakingRuleSetInput -> Aff (err :: RequestError | eff) CreateMatchmakingRuleSetOutput
```

<p>Creates a new rule set for FlexMatch matchmaking. A rule set describes the type of match to create, such as the number and size of teams, and sets the parameters for acceptable player matches, such as minimum skill level or character type. Rule sets are used in matchmaking configurations, which define how matchmaking requests are handled. Each <a>MatchmakingConfiguration</a> uses one rule set; you can set up multiple rule sets to handle the scenarios that suit your game (such as for different game modes), and create a separate matchmaking configuration for each rule set. See additional information on rule set content in the <a>MatchmakingRuleSet</a> structure. For help creating rule sets, including useful examples, see the topic <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-intro.html"> Adding FlexMatch to Your Game</a>.</p> <p>Once created, matchmaking rule sets cannot be changed or deleted, so we recommend checking the rule set syntax using <a>ValidateMatchmakingRuleSet</a> before creating the rule set.</p> <p>To create a matchmaking rule set, provide the set of rules and a unique name. Rule sets must be defined in the same region as the matchmaking configuration they will be used with. Rule sets cannot be edited or deleted. If you need to change a rule set, create a new one with the necessary edits and then update matchmaking configurations to use the new rule set.</p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>

#### `createPlayerSession`

``` purescript
createPlayerSession :: forall eff. CreatePlayerSessionInput -> Aff (err :: RequestError | eff) CreatePlayerSessionOutput
```

<p>Adds a player to a game session and creates a player session record. Before a player can be added, a game session must have an <code>ACTIVE</code> status, have a creation policy of <code>ALLOW_ALL</code>, and have an open player slot. To add a group of players to a game session, use <a>CreatePlayerSessions</a>.</p> <p>To create a player session, specify a game session ID, player ID, and optionally a string of player data. If successful, the player is added to the game session and a new <a>PlayerSession</a> object is returned. Player sessions cannot be updated. </p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `createPlayerSessions`

``` purescript
createPlayerSessions :: forall eff. CreatePlayerSessionsInput -> Aff (err :: RequestError | eff) CreatePlayerSessionsOutput
```

<p>Adds a group of players to a game session. This action is useful with a team matching feature. Before players can be added, a game session must have an <code>ACTIVE</code> status, have a creation policy of <code>ALLOW_ALL</code>, and have an open player slot. To add a single player to a game session, use <a>CreatePlayerSession</a>.</p> <p>To create player sessions, specify a game session ID, a list of player IDs, and optionally a set of player data strings. If successful, the players are added to the game session and a set of new <a>PlayerSession</a> objects is returned. Player sessions cannot be updated.</p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `createVpcPeeringAuthorization`

``` purescript
createVpcPeeringAuthorization :: forall eff. CreateVpcPeeringAuthorizationInput -> Aff (err :: RequestError | eff) CreateVpcPeeringAuthorizationOutput
```

<p>Requests authorization to create or delete a peer connection between the VPC for your Amazon GameLift fleet and a virtual private cloud (VPC) in your AWS account. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. Once you've received authorization, call <a>CreateVpcPeeringConnection</a> to establish the peering connection. For more information, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html">VPC Peering with Amazon GameLift Fleets</a>.</p> <p>You can peer with VPCs that are owned by any AWS account you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different regions.</p> <p>To request authorization to create a connection, call this operation from the AWS account with the VPC that you want to peer to your Amazon GameLift fleet. For example, to enable your game servers to retrieve data from a DynamoDB table, use the account that manages that DynamoDB resource. Identify the following values: (1) The ID of the VPC that you want to peer with, and (2) the ID of the AWS account that you use to manage Amazon GameLift. If successful, VPC peering is authorized for the specified VPC. </p> <p>To request authorization to delete a connection, call this operation from the AWS account with the VPC that is peered with your Amazon GameLift fleet. Identify the following values: (1) VPC ID that you want to delete the peering connection for, and (2) ID of the AWS account that you use to manage Amazon GameLift. </p> <p>The authorization remains valid for 24 hours unless it is canceled by a call to <a>DeleteVpcPeeringAuthorization</a>. You must create or delete the peering connection while the authorization is valid. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>

#### `createVpcPeeringConnection`

``` purescript
createVpcPeeringConnection :: forall eff. CreateVpcPeeringConnectionInput -> Aff (err :: RequestError | eff) CreateVpcPeeringConnectionOutput
```

<p>Establishes a VPC peering connection between a virtual private cloud (VPC) in an AWS account with the VPC for your Amazon GameLift fleet. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. You can peer with VPCs in any AWS account that you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different regions. For more information, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html">VPC Peering with Amazon GameLift Fleets</a>.</p> <p>Before calling this operation to establish the peering connection, you first need to call <a>CreateVpcPeeringAuthorization</a> and identify the VPC you want to peer with. Once the authorization for the specified VPC is issued, you have 24 hours to establish the connection. These two operations handle all tasks necessary to peer the two VPCs, including acceptance, updating routing tables, etc. </p> <p>To establish the connection, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Identify the following values: (1) The ID of the fleet you want to be enable a VPC peering connection for; (2) The AWS account with the VPC that you want to peer with; and (3) The ID of the VPC you want to peer with. This operation is asynchronous. If successful, a <a>VpcPeeringConnection</a> request is created. You can use continuous polling to track the request's status using <a>DescribeVpcPeeringConnections</a>, or by monitoring fleet events for success or failure using <a>DescribeFleetEvents</a>. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>

#### `deleteAlias`

``` purescript
deleteAlias :: forall eff. DeleteAliasInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an alias. This action removes all record of the alias. Game clients attempting to access a server process using the deleted alias receive an error. To delete an alias, specify the alias ID to be deleted.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>

#### `deleteBuild`

``` purescript
deleteBuild :: forall eff. DeleteBuildInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a build. This action permanently deletes the build record and any uploaded build files.</p> <p>To delete a build, specify its ID. Deleting a build does not affect the status of any active fleets using the build, but you can no longer create new fleets with the deleted build.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>

#### `deleteFleet`

``` purescript
deleteFleet :: forall eff. DeleteFleetInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes everything related to a fleet. Before deleting a fleet, you must set the fleet's desired capacity to zero. See <a>UpdateFleetCapacity</a>.</p> <p>This action removes the fleet's resources and the fleet record. Once a fleet is deleted, you can no longer use that fleet.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `deleteGameSessionQueue`

``` purescript
deleteGameSessionQueue :: forall eff. DeleteGameSessionQueueInput -> Aff (err :: RequestError | eff) DeleteGameSessionQueueOutput
```

<p>Deletes a game session queue. This action means that any <a>StartGameSessionPlacement</a> requests that reference this queue will fail. To delete a queue, specify the queue name.</p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>

#### `deleteMatchmakingConfiguration`

``` purescript
deleteMatchmakingConfiguration :: forall eff. DeleteMatchmakingConfigurationInput -> Aff (err :: RequestError | eff) DeleteMatchmakingConfigurationOutput
```

<p>Permanently removes a FlexMatch matchmaking configuration. To delete, specify the configuration name. A matchmaking configuration cannot be deleted if it is being used in any active matchmaking tickets.</p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>

#### `deleteScalingPolicy`

``` purescript
deleteScalingPolicy :: forall eff. DeleteScalingPolicyInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a fleet scaling policy. This action means that the policy is no longer in force and removes all record of it. To delete a scaling policy, specify both the scaling policy name and the fleet ID it is associated with.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `deleteVpcPeeringAuthorization`

``` purescript
deleteVpcPeeringAuthorization :: forall eff. DeleteVpcPeeringAuthorizationInput -> Aff (err :: RequestError | eff) DeleteVpcPeeringAuthorizationOutput
```

<p>Cancels a pending VPC peering authorization for the specified VPC. If the authorization has already been used to create a peering connection, call <a>DeleteVpcPeeringConnection</a> to remove the connection. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>

#### `deleteVpcPeeringConnection`

``` purescript
deleteVpcPeeringConnection :: forall eff. DeleteVpcPeeringConnectionInput -> Aff (err :: RequestError | eff) DeleteVpcPeeringConnectionOutput
```

<p>Removes a VPC peering connection. To delete the connection, you must have a valid authorization for the VPC peering connection that you want to delete. You can check for an authorization by calling <a>DescribeVpcPeeringAuthorizations</a> or request a new one using <a>CreateVpcPeeringAuthorization</a>. </p> <p>Once a valid authorization exists, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Identify the connection to delete by the connection ID and fleet ID. If successful, the connection is removed. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>

#### `describeAlias`

``` purescript
describeAlias :: forall eff. DescribeAliasInput -> Aff (err :: RequestError | eff) DescribeAliasOutput
```

<p>Retrieves properties for an alias. This operation returns all alias metadata and settings. To get an alias's target fleet ID only, use <code>ResolveAlias</code>. </p> <p>To get alias properties, specify the alias ID. If successful, the requested alias record is returned.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>

#### `describeBuild`

``` purescript
describeBuild :: forall eff. DescribeBuildInput -> Aff (err :: RequestError | eff) DescribeBuildOutput
```

<p>Retrieves properties for a build. To request a build record, specify a build ID. If successful, an object containing the build properties is returned.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>

#### `describeEC2InstanceLimits`

``` purescript
describeEC2InstanceLimits :: forall eff. DescribeEC2InstanceLimitsInput -> Aff (err :: RequestError | eff) DescribeEC2InstanceLimitsOutput
```

<p>Retrieves the following information for the specified EC2 instance type:</p> <ul> <li> <p>maximum number of instances allowed per AWS account (service limit)</p> </li> <li> <p>current usage level for the AWS account</p> </li> </ul> <p>Service limits vary depending on region. Available regions for Amazon GameLift can be found in the AWS Management Console for Amazon GameLift (see the drop-down list in the upper right corner).</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `describeFleetAttributes`

``` purescript
describeFleetAttributes :: forall eff. DescribeFleetAttributesInput -> Aff (err :: RequestError | eff) DescribeFleetAttributesOutput
```

<p>Retrieves fleet properties, including metadata, status, and configuration, for one or more fleets. You can request attributes for all fleets, or specify a list of one or more fleet IDs. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>FleetAttributes</a> object is returned for each requested fleet ID. When specifying a list of fleet IDs, attribute objects are returned only for fleets that currently exist. </p> <note> <p>Some API actions may limit the number of fleet IDs allowed in one request. If a request exceeds this limit, the request fails and the error message includes the maximum allowed.</p> </note> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `describeFleetCapacity`

``` purescript
describeFleetCapacity :: forall eff. DescribeFleetCapacityInput -> Aff (err :: RequestError | eff) DescribeFleetCapacityOutput
```

<p>Retrieves the current status of fleet capacity for one or more fleets. This information includes the number of instances that have been requested for the fleet and the number currently active. You can request capacity for all fleets, or specify a list of one or more fleet IDs. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>FleetCapacity</a> object is returned for each requested fleet ID. When specifying a list of fleet IDs, attribute objects are returned only for fleets that currently exist. </p> <note> <p>Some API actions may limit the number of fleet IDs allowed in one request. If a request exceeds this limit, the request fails and the error message includes the maximum allowed.</p> </note> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `describeFleetEvents`

``` purescript
describeFleetEvents :: forall eff. DescribeFleetEventsInput -> Aff (err :: RequestError | eff) DescribeFleetEventsOutput
```

<p>Retrieves entries from the specified fleet's event log. You can specify a time range to limit the result set. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a collection of event log entries matching the request are returned.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `describeFleetPortSettings`

``` purescript
describeFleetPortSettings :: forall eff. DescribeFleetPortSettingsInput -> Aff (err :: RequestError | eff) DescribeFleetPortSettingsOutput
```

<p>Retrieves the inbound connection permissions for a fleet. Connection permissions include a range of IP addresses and port settings that incoming traffic can use to access server processes in the fleet. To get a fleet's inbound connection permissions, specify a fleet ID. If successful, a collection of <a>IpPermission</a> objects is returned for the requested fleet ID. If the requested fleet has been deleted, the result set is empty.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `describeFleetUtilization`

``` purescript
describeFleetUtilization :: forall eff. DescribeFleetUtilizationInput -> Aff (err :: RequestError | eff) DescribeFleetUtilizationOutput
```

<p>Retrieves utilization statistics for one or more fleets. You can request utilization data for all fleets, or specify a list of one or more fleet IDs. When requesting multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>FleetUtilization</a> object is returned for each requested fleet ID. When specifying a list of fleet IDs, utilization objects are returned only for fleets that currently exist. </p> <note> <p>Some API actions may limit the number of fleet IDs allowed in one request. If a request exceeds this limit, the request fails and the error message includes the maximum allowed.</p> </note> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `describeGameSessionDetails`

``` purescript
describeGameSessionDetails :: forall eff. DescribeGameSessionDetailsInput -> Aff (err :: RequestError | eff) DescribeGameSessionDetailsOutput
```

<p>Retrieves properties, including the protection policy in force, for one or more game sessions. This action can be used in several ways: (1) provide a <code>GameSessionId</code> or <code>GameSessionArn</code> to request details for a specific game session; (2) provide either a <code>FleetId</code> or an <code>AliasId</code> to request properties for all game sessions running on a fleet. </p> <p>To get game session record(s), specify just one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>GameSessionDetail</a> object is returned for each session matching the request.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `describeGameSessionPlacement`

``` purescript
describeGameSessionPlacement :: forall eff. DescribeGameSessionPlacementInput -> Aff (err :: RequestError | eff) DescribeGameSessionPlacementOutput
```

<p>Retrieves properties and current status of a game session placement request. To get game session placement details, specify the placement ID. If successful, a <a>GameSessionPlacement</a> object is returned.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `describeGameSessionQueues`

``` purescript
describeGameSessionQueues :: forall eff. DescribeGameSessionQueuesInput -> Aff (err :: RequestError | eff) DescribeGameSessionQueuesOutput
```

<p>Retrieves the properties for one or more game session queues. When requesting multiple queues, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>GameSessionQueue</a> object is returned for each requested queue. When specifying a list of queues, objects are returned only for queues that currently exist in the region.</p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>

#### `describeGameSessions`

``` purescript
describeGameSessions :: forall eff. DescribeGameSessionsInput -> Aff (err :: RequestError | eff) DescribeGameSessionsOutput
```

<p>Retrieves a set of one or more game sessions. Request a specific game session or request all game sessions on a fleet. Alternatively, use <a>SearchGameSessions</a> to request a set of active game sessions that are filtered by certain criteria. To retrieve protection policy settings for game sessions, use <a>DescribeGameSessionDetails</a>.</p> <p>To get game sessions, specify one of the following: game session ID, fleet ID, or alias ID. You can filter this request by game session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>GameSession</a> object is returned for each game session matching the request.</p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `describeInstances`

``` purescript
describeInstances :: forall eff. DescribeInstancesInput -> Aff (err :: RequestError | eff) DescribeInstancesOutput
```

<p>Retrieves information about a fleet's instances, including instance IDs. Use this action to get details on all instances in the fleet or get details on one specific instance.</p> <p>To get a specific instance, specify fleet ID and instance ID. To get all instances in a fleet, specify a fleet ID only. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, an <a>Instance</a> object is returned for each result.</p>

#### `describeMatchmaking`

``` purescript
describeMatchmaking :: forall eff. DescribeMatchmakingInput -> Aff (err :: RequestError | eff) DescribeMatchmakingOutput
```

<p>Retrieves one or more matchmaking tickets. Use this operation to retrieve ticket information, including status and--once a successful match is made--acquire connection information for the resulting new game session. </p> <p>You can use this operation to track the progress of matchmaking requests (through polling) as an alternative to using event notifications. See more details on tracking matchmaking requests through polling or notifications in <a>StartMatchmaking</a>. </p> <p>To request matchmaking tickets, provide a list of up to 10 ticket IDs. If the request is successful, a ticket object is returned for each requested ID that currently exists.</p> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>

#### `describeMatchmakingConfigurations`

``` purescript
describeMatchmakingConfigurations :: forall eff. DescribeMatchmakingConfigurationsInput -> Aff (err :: RequestError | eff) DescribeMatchmakingConfigurationsOutput
```

<p>Retrieves the details of FlexMatch matchmaking configurations. with this operation, you have the following options: (1) retrieve all existing configurations, (2) provide the names of one or more configurations to retrieve, or (3) retrieve all configurations that use a specified rule set name. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a configuration is returned for each requested name. When specifying a list of names, only configurations that currently exist are returned. </p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>

#### `describeMatchmakingRuleSets`

``` purescript
describeMatchmakingRuleSets :: forall eff. DescribeMatchmakingRuleSetsInput -> Aff (err :: RequestError | eff) DescribeMatchmakingRuleSetsOutput
```

<p>Retrieves the details for FlexMatch matchmaking rule sets. You can request all existing rule sets for the region, or provide a list of one or more rule set names. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a rule set is returned for each requested name. </p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>

#### `describePlayerSessions`

``` purescript
describePlayerSessions :: forall eff. DescribePlayerSessionsInput -> Aff (err :: RequestError | eff) DescribePlayerSessionsOutput
```

<p>Retrieves properties for one or more player sessions. This action can be used in several ways: (1) provide a <code>PlayerSessionId</code> to request properties for a specific player session; (2) provide a <code>GameSessionId</code> to request properties for all player sessions in the specified game session; (3) provide a <code>PlayerId</code> to request properties for all player sessions of a specified player. </p> <p>To get game session record(s), specify only one of the following: a player session ID, a game session ID, or a player ID. You can filter this request by player session status. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, a <a>PlayerSession</a> object is returned for each session matching the request.</p> <p> <i>Available in Amazon GameLift Local.</i> </p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `describeRuntimeConfiguration`

``` purescript
describeRuntimeConfiguration :: forall eff. DescribeRuntimeConfigurationInput -> Aff (err :: RequestError | eff) DescribeRuntimeConfigurationOutput
```

<p>Retrieves the current run-time configuration for the specified fleet. The run-time configuration tells Amazon GameLift how to launch server processes on instances in the fleet.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `describeScalingPolicies`

``` purescript
describeScalingPolicies :: forall eff. DescribeScalingPoliciesInput -> Aff (err :: RequestError | eff) DescribeScalingPoliciesOutput
```

<p>Retrieves all scaling policies applied to a fleet.</p> <p>To get a fleet's scaling policies, specify the fleet ID. You can filter this request by policy status, such as to retrieve only active scaling policies. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, set of <a>ScalingPolicy</a> objects is returned for the fleet.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `describeVpcPeeringAuthorizations`

``` purescript
describeVpcPeeringAuthorizations :: forall eff. DescribeVpcPeeringAuthorizationsInput -> Aff (err :: RequestError | eff) DescribeVpcPeeringAuthorizationsOutput
```

<p>Retrieves valid VPC peering authorizations that are pending for the AWS account. This operation returns all VPC peering authorizations and requests for peering. This includes those initiated and received by this account. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>

#### `describeVpcPeeringConnections`

``` purescript
describeVpcPeeringConnections :: forall eff. DescribeVpcPeeringConnectionsInput -> Aff (err :: RequestError | eff) DescribeVpcPeeringConnectionsOutput
```

<p>Retrieves information on VPC peering connections. Use this operation to get peering information for all fleets or for one specific fleet ID. </p> <p>To retrieve connection information, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Specify a fleet ID or leave the parameter empty to retrieve all connection records. If successful, the retrieved information includes both active and pending connections. Active connections identify the IpV4 CIDR block that the VPC uses to connect. </p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>

#### `getGameSessionLogUrl`

``` purescript
getGameSessionLogUrl :: forall eff. GetGameSessionLogUrlInput -> Aff (err :: RequestError | eff) GetGameSessionLogUrlOutput
```

<p>Retrieves the location of stored game session logs for a specified game session. When a game session is terminated, Amazon GameLift automatically stores the logs in Amazon S3 and retains them for 14 days. Use this URL to download the logs.</p> <note> <p>See the <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_gamelift">AWS Service Limits</a> page for maximum log file sizes. Log files that exceed this limit are not saved.</p> </note> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `getInstanceAccess`

``` purescript
getInstanceAccess :: forall eff. GetInstanceAccessInput -> Aff (err :: RequestError | eff) GetInstanceAccessOutput
```

<p>Requests remote access to a fleet instance. Remote access is useful for debugging, gathering benchmarking data, or watching activity in real time. </p> <p>Access requires credentials that match the operating system of the instance. For a Windows instance, Amazon GameLift returns a user name and password as strings for use with a Windows Remote Desktop client. For a Linux instance, Amazon GameLift returns a user name and RSA private key, also as strings, for use with an SSH client. The private key must be saved in the proper format to a <code>.pem</code> file before using. If you're making this request using the AWS CLI, saving the secret can be handled as part of the GetInstanceAccess request. (See the example later in this topic). For more information on remote access, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html">Remotely Accessing an Instance</a>.</p> <p>To request access to a specific instance, specify the IDs of the instance and the fleet it belongs to. If successful, an <a>InstanceAccess</a> object is returned containing the instance's IP address and a set of credentials.</p>

#### `listAliases`

``` purescript
listAliases :: forall eff. ListAliasesInput -> Aff (err :: RequestError | eff) ListAliasesOutput
```

<p>Retrieves all aliases for this AWS account. You can filter the result set by alias name and/or routing strategy type. Use the pagination parameters to retrieve results in sequential pages.</p> <note> <p>Returned aliases are not listed in any particular order.</p> </note> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>

#### `listBuilds`

``` purescript
listBuilds :: forall eff. ListBuildsInput -> Aff (err :: RequestError | eff) ListBuildsOutput
```

<p>Retrieves build records for all builds associated with the AWS account in use. You can limit results to builds that are in a specific status by using the <code>Status</code> parameter. Use the pagination parameters to retrieve results in a set of sequential pages. </p> <note> <p>Build records are not listed in any particular order.</p> </note> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>

#### `listFleets`

``` purescript
listFleets :: forall eff. ListFleetsInput -> Aff (err :: RequestError | eff) ListFleetsOutput
```

<p>Retrieves a collection of fleet records for this AWS account. You can filter the result set by build ID. Use the pagination parameters to retrieve results in sequential pages.</p> <note> <p>Fleet records are not listed in any particular order.</p> </note> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `putScalingPolicy`

``` purescript
putScalingPolicy :: forall eff. PutScalingPolicyInput -> Aff (err :: RequestError | eff) PutScalingPolicyOutput
```

<p>Creates or updates a scaling policy for a fleet. An active scaling policy prompts Amazon GameLift to track a certain metric for a fleet and automatically change the fleet's capacity in specific circumstances. Each scaling policy contains one rule statement. Fleets can have multiple scaling policies in force simultaneously.</p> <p>A scaling policy rule statement has the following structure:</p> <p>If <code>[MetricName]</code> is <code>[ComparisonOperator]</code> <code>[Threshold]</code> for <code>[EvaluationPeriods]</code> minutes, then <code>[ScalingAdjustmentType]</code> to/by <code>[ScalingAdjustment]</code>.</p> <p>For example, this policy: "If the number of idle instances exceeds 20 for more than 15 minutes, then reduce the fleet capacity by 10 instances" could be implemented as the following rule statement:</p> <p>If [IdleInstances] is [GreaterThanOrEqualToThreshold] [20] for [15] minutes, then [ChangeInCapacity] by [-10].</p> <p>To create or update a scaling policy, specify a unique combination of name and fleet ID, and set the rule values. All parameters for this action are required. If successful, the policy name is returned. Scaling policies cannot be suspended or made inactive. To stop enforcing a scaling policy, call <a>DeleteScalingPolicy</a>.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `requestUploadCredentials`

``` purescript
requestUploadCredentials :: forall eff. RequestUploadCredentialsInput -> Aff (err :: RequestError | eff) RequestUploadCredentialsOutput
```

<p>Retrieves a fresh set of credentials for use when uploading a new set of game build files to Amazon GameLift's Amazon S3. This is done as part of the build creation process; see <a>CreateBuild</a>.</p> <p>To request new credentials, specify the build ID as returned with an initial <code>CreateBuild</code> request. If successful, a new set of credentials are returned, along with the S3 storage location associated with the build ID.</p>

#### `resolveAlias`

``` purescript
resolveAlias :: forall eff. ResolveAliasInput -> Aff (err :: RequestError | eff) ResolveAliasOutput
```

<p>Retrieves the fleet ID that a specified alias is currently pointing to.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>

#### `searchGameSessions`

``` purescript
searchGameSessions :: forall eff. SearchGameSessionsInput -> Aff (err :: RequestError | eff) SearchGameSessionsOutput
```

<p>Retrieves all active game sessions that match a set of search criteria and sorts them in a specified order. You can search or sort by the following game session attributes:</p> <ul> <li> <p> <b>gameSessionId</b> -- Unique identifier for the game session. You can use either a <code>GameSessionId</code> or <code>GameSessionArn</code> value. </p> </li> <li> <p> <b>gameSessionName</b> -- Name assigned to a game session. This value is set when requesting a new game session with <a>CreateGameSession</a> or updating with <a>UpdateGameSession</a>. Game session names do not need to be unique to a game session.</p> </li> <li> <p> <b>gameSessionProperties</b> -- Custom data defined in a game session's <code>GameProperty</code> parameter. <code>GameProperty</code> values are stored as key:value pairs; the filter expression must indicate the key and a string to search the data values for. For example, to search for game sessions with custom data containing the key:value pair "gameMode:brawl", specify the following: gameSessionProperties.gameMode = "brawl". All custom data values are searched as strings.</p> </li> <li> <p> <b>maximumSessions</b> -- Maximum number of player sessions allowed for a game session. This value is set when requesting a new game session with <a>CreateGameSession</a> or updating with <a>UpdateGameSession</a>.</p> </li> <li> <p> <b>creationTimeMillis</b> -- Value indicating when a game session was created. It is expressed in Unix time as milliseconds.</p> </li> <li> <p> <b>playerSessionCount</b> -- Number of players currently connected to a game session. This value changes rapidly as players join the session or drop out.</p> </li> <li> <p> <b>hasAvailablePlayerSessions</b> -- Boolean value indicating whether a game session has reached its maximum number of players. It is highly recommended that all search requests include this filter attribute to optimize search performance and return only sessions that players can join. </p> </li> </ul> <note> <p>Returned values for <code>playerSessionCount</code> and <code>hasAvailablePlayerSessions</code> change quickly as players join sessions and others drop out. Results should be considered a snapshot in time. Be sure to refresh search results often, and handle sessions that fill up before a player can join. </p> </note> <p>To search or sort, specify either a fleet ID or an alias ID, and provide a search filter expression, a sort expression, or both. If successful, a collection of <a>GameSession</a> objects matching the request is returned. Use the pagination parameters to retrieve results as a set of sequential pages. </p> <p>You can search for game sessions one fleet at a time only. To find game sessions across multiple fleets, you must search each fleet separately and combine the results. This search feature finds only game sessions that are in <code>ACTIVE</code> status. To locate games in statuses other than active, use <a>DescribeGameSessionDetails</a>.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `startGameSessionPlacement`

``` purescript
startGameSessionPlacement :: forall eff. StartGameSessionPlacementInput -> Aff (err :: RequestError | eff) StartGameSessionPlacementOutput
```

<p>Places a request for a new game session in a queue (see <a>CreateGameSessionQueue</a>). When processing a placement request, Amazon GameLift searches for available resources on the queue's destinations, scanning each until it finds resources or the placement request times out.</p> <p>A game session placement request can also request player sessions. When a new game session is successfully created, Amazon GameLift creates a player session for each player included in the request.</p> <p>When placing a game session, by default Amazon GameLift tries each fleet in the order they are listed in the queue configuration. Ideally, a queue's destinations are listed in preference order.</p> <p>Alternatively, when requesting a game session with players, you can also provide latency data for each player in relevant regions. Latency data indicates the performance lag a player experiences when connected to a fleet in the region. Amazon GameLift uses latency data to reorder the list of destinations to place the game session in a region with minimal lag. If latency data is provided for multiple players, Amazon GameLift calculates each region's average lag for all players and reorders to get the best game play across all players. </p> <p>To place a new game session request, specify the following:</p> <ul> <li> <p>The queue name and a set of game session properties and settings</p> </li> <li> <p>A unique ID (such as a UUID) for the placement. You use this ID to track the status of the placement request</p> </li> <li> <p>(Optional) A set of IDs and player data for each player you want to join to the new game session</p> </li> <li> <p>Latency data for all players (if you want to optimize game play for the players)</p> </li> </ul> <p>If successful, a new game session placement is created.</p> <p>To track the status of a placement request, call <a>DescribeGameSessionPlacement</a> and check the request's status. If the status is <code>FULFILLED</code>, a new game session has been created and a game session ARN and region are referenced. If the placement request times out, you can resubmit the request or retry it with a different queue. </p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `startMatchBackfill`

``` purescript
startMatchBackfill :: forall eff. StartMatchBackfillInput -> Aff (err :: RequestError | eff) StartMatchBackfillOutput
```

<p>Finds new players to fill open slots in an existing game session. This operation can be used to add players to matched games that start with fewer than the maximum number of players or to replace players when they drop out. By backfilling with the same matchmaker used to create the original match, you ensure that new players meet the match criteria and maintain a consistent experience throughout the game session. You can backfill a match anytime after a game session has been created. </p> <p>To request a match backfill, specify a unique ticket ID, the existing game session's ARN, a matchmaking configuration, and a set of data that describes all current players in the game session. If successful, a match backfill ticket is created and returned with status set to QUEUED. The ticket is placed in the matchmaker's ticket pool and processed. Track the status of the ticket to respond as needed. For more detail how to set up backfilling, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-backfill.html"> Backfill Existing Games with FlexMatch</a>. </p> <p>The process of finding backfill matches is essentially identical to the initial matchmaking process. The matchmaker searches the pool and groups tickets together to form potential matches, allowing only one backfill ticket per potential match. Once the a match is formed, the matchmaker creates player sessions for the new players. All tickets in the match are updated with the game session's connection information, and the <a>GameSession</a> object is updated to include matchmaker data on the new players. For more detail on how match backfill requests are processed, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-intro.html"> How Amazon GameLift FlexMatch Works</a>. </p> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>

#### `startMatchmaking`

``` purescript
startMatchmaking :: forall eff. StartMatchmakingInput -> Aff (err :: RequestError | eff) StartMatchmakingOutput
```

<p>Uses FlexMatch to create a game match for a group of players based on custom matchmaking rules, and starts a new game for the matched players. Each matchmaking request specifies the type of match to build (team configuration, rules for an acceptable match, etc.). The request also specifies the players to find a match for and where to host the new game session for optimal performance. A matchmaking request might start with a single player or a group of players who want to play together. FlexMatch finds additional players as needed to fill the match. Match type, rules, and the queue used to place a new game session are defined in a <code>MatchmakingConfiguration</code>. For complete information on setting up and using FlexMatch, see the topic <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-intro.html"> Adding FlexMatch to Your Game</a>.</p> <p>To start matchmaking, provide a unique ticket ID, specify a matchmaking configuration, and include the players to be matched. You must also include a set of player attributes relevant for the matchmaking configuration. If successful, a matchmaking ticket is returned with status set to <code>QUEUED</code>. Track the status of the ticket to respond as needed and acquire game session connection information for successfully completed matches.</p> <p> <b>Tracking ticket status</b> -- A couple of options are available for tracking the status of matchmaking requests: </p> <ul> <li> <p>Polling -- Call <code>DescribeMatchmaking</code>. This operation returns the full ticket object, including current status and (for completed tickets) game session connection info. We recommend polling no more than once every 10 seconds.</p> </li> <li> <p>Notifications -- Get event notifications for changes in ticket status using Amazon Simple Notification Service (SNS). Notifications are easy to set up (see <a>CreateMatchmakingConfiguration</a>) and typically deliver match status changes faster and more efficiently than polling. We recommend that you use polling to back up to notifications (since delivery is not guaranteed) and call <code>DescribeMatchmaking</code> only when notifications are not received within 30 seconds.</p> </li> </ul> <p> <b>Processing a matchmaking request</b> -- FlexMatch handles a matchmaking request as follows: </p> <ol> <li> <p>Your client code submits a <code>StartMatchmaking</code> request for one or more players and tracks the status of the request ticket. </p> </li> <li> <p>FlexMatch uses this ticket and others in process to build an acceptable match. When a potential match is identified, all tickets in the proposed match are advanced to the next status. </p> </li> <li> <p>If the match requires player acceptance (set in the matchmaking configuration), the tickets move into status <code>REQUIRES_ACCEPTANCE</code>. This status triggers your client code to solicit acceptance from all players in every ticket involved in the match, and then call <a>AcceptMatch</a> for each player. If any player rejects or fails to accept the match before a specified timeout, the proposed match is dropped (see <code>AcceptMatch</code> for more details).</p> </li> <li> <p>Once a match is proposed and accepted, the matchmaking tickets move into status <code>PLACING</code>. FlexMatch locates resources for a new game session using the game session queue (set in the matchmaking configuration) and creates the game session based on the match data. </p> </li> <li> <p>When the match is successfully placed, the matchmaking tickets move into <code>COMPLETED</code> status. Connection information (including game session endpoint and player session) is added to the matchmaking tickets. Matched players can use the connection information to join the game. </p> </li> </ol> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>

#### `stopGameSessionPlacement`

``` purescript
stopGameSessionPlacement :: forall eff. StopGameSessionPlacementInput -> Aff (err :: RequestError | eff) StopGameSessionPlacementOutput
```

<p>Cancels a game session placement that is in <code>PENDING</code> status. To stop a placement, provide the placement ID values. If successful, the placement is moved to <code>CANCELLED</code> status.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `stopMatchmaking`

``` purescript
stopMatchmaking :: forall eff. StopMatchmakingInput -> Aff (err :: RequestError | eff) StopMatchmakingOutput
```

<p>Cancels a matchmaking ticket that is currently being processed. To stop the matchmaking operation, specify the ticket ID. If successful, work on the ticket is stopped, and the ticket status is changed to <code>CANCELLED</code>.</p> <p>Matchmaking-related operations include:</p> <ul> <li> <p> <a>StartMatchmaking</a> </p> </li> <li> <p> <a>DescribeMatchmaking</a> </p> </li> <li> <p> <a>StopMatchmaking</a> </p> </li> <li> <p> <a>AcceptMatch</a> </p> </li> <li> <p> <a>StartMatchBackfill</a> </p> </li> </ul>

#### `updateAlias`

``` purescript
updateAlias :: forall eff. UpdateAliasInput -> Aff (err :: RequestError | eff) UpdateAliasOutput
```

<p>Updates properties for an alias. To update properties, specify the alias ID to be updated and provide the information to be changed. To reassign an alias to another fleet, provide an updated routing strategy. If successful, the updated alias record is returned.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>

#### `updateBuild`

``` purescript
updateBuild :: forall eff. UpdateBuildInput -> Aff (err :: RequestError | eff) UpdateBuildOutput
```

<p>Updates metadata in a build record, including the build name and version. To update the metadata, specify the build ID to update and provide the new values. If successful, a build object containing the updated metadata is returned.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>

#### `updateFleetAttributes`

``` purescript
updateFleetAttributes :: forall eff. UpdateFleetAttributesInput -> Aff (err :: RequestError | eff) UpdateFleetAttributesOutput
```

<p>Updates fleet properties, including name and description, for a fleet. To update metadata, specify the fleet ID and the property values that you want to change. If successful, the fleet ID for the updated fleet is returned.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `updateFleetCapacity`

``` purescript
updateFleetCapacity :: forall eff. UpdateFleetCapacityInput -> Aff (err :: RequestError | eff) UpdateFleetCapacityOutput
```

<p>Updates capacity settings for a fleet. Use this action to specify the number of EC2 instances (hosts) that you want this fleet to contain. Before calling this action, you may want to call <a>DescribeEC2InstanceLimits</a> to get the maximum capacity based on the fleet's EC2 instance type.</p> <p>If you're using autoscaling (see <a>PutScalingPolicy</a>), you may want to specify a minimum and/or maximum capacity. If you don't provide these, autoscaling can set capacity anywhere between zero and the <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_gamelift">service limits</a>.</p> <p>To update fleet capacity, specify the fleet ID and the number of instances you want the fleet to host. If successful, Amazon GameLift starts or terminates instances so that the fleet's active instance count matches the desired instance count. You can view a fleet's current capacity information by calling <a>DescribeFleetCapacity</a>. If the desired instance count is higher than the instance type's limit, the "Limit Exceeded" exception occurs.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `updateFleetPortSettings`

``` purescript
updateFleetPortSettings :: forall eff. UpdateFleetPortSettingsInput -> Aff (err :: RequestError | eff) UpdateFleetPortSettingsOutput
```

<p>Updates port settings for a fleet. To update settings, specify the fleet ID to be updated and list the permissions you want to update. List the permissions you want to add in <code>InboundPermissionAuthorizations</code>, and permissions you want to remove in <code>InboundPermissionRevocations</code>. Permissions to be removed must match existing fleet permissions. If successful, the fleet ID for the updated fleet is returned.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `updateGameSession`

``` purescript
updateGameSession :: forall eff. UpdateGameSessionInput -> Aff (err :: RequestError | eff) UpdateGameSessionOutput
```

<p>Updates game session properties. This includes the session name, maximum player count, protection policy, which controls whether or not an active game session can be terminated during a scale-down event, and the player session creation policy, which controls whether or not new players can join the session. To update a game session, specify the game session ID and the values you want to change. If successful, an updated <a>GameSession</a> object is returned. </p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `updateGameSessionQueue`

``` purescript
updateGameSessionQueue :: forall eff. UpdateGameSessionQueueInput -> Aff (err :: RequestError | eff) UpdateGameSessionQueueOutput
```

<p>Updates settings for a game session queue, which determines how new game session requests in the queue are processed. To update settings, specify the queue name to be updated and provide the new settings. When updating destinations, provide a complete list of destinations. </p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>

#### `updateMatchmakingConfiguration`

``` purescript
updateMatchmakingConfiguration :: forall eff. UpdateMatchmakingConfigurationInput -> Aff (err :: RequestError | eff) UpdateMatchmakingConfigurationOutput
```

<p>Updates settings for a FlexMatch matchmaking configuration. To update settings, specify the configuration name to be updated and provide the new settings. </p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>

#### `updateRuntimeConfiguration`

``` purescript
updateRuntimeConfiguration :: forall eff. UpdateRuntimeConfigurationInput -> Aff (err :: RequestError | eff) UpdateRuntimeConfigurationOutput
```

<p>Updates the current run-time configuration for the specified fleet, which tells Amazon GameLift how to launch server processes on instances in the fleet. You can update a fleet's run-time configuration at any time after the fleet is created; it does not need to be in an <code>ACTIVE</code> status.</p> <p>To update run-time configuration, specify the fleet ID and provide a <code>RuntimeConfiguration</code> object with the updated collection of server process configurations.</p> <p>Each instance in a Amazon GameLift fleet checks regularly for an updated run-time configuration and changes how it launches server processes to comply with the latest version. Existing server processes are not affected by the update; they continue to run until they end, while Amazon GameLift simply adds new server processes to fit the current run-time configuration. As a result, the run-time configuration changes are applied gradually as existing processes shut down and new processes are launched in Amazon GameLift's normal process recycling activity.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `validateMatchmakingRuleSet`

``` purescript
validateMatchmakingRuleSet :: forall eff. ValidateMatchmakingRuleSetInput -> Aff (err :: RequestError | eff) ValidateMatchmakingRuleSetOutput
```

<p>Validates the syntax of a matchmaking rule or rule set. This operation checks that the rule set uses syntactically correct JSON and that it conforms to allowed property expressions. To validate syntax, provide a rule set string.</p> <p>Operations related to match configurations and rule sets include:</p> <ul> <li> <p> <a>CreateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DescribeMatchmakingConfigurations</a> </p> </li> <li> <p> <a>UpdateMatchmakingConfiguration</a> </p> </li> <li> <p> <a>DeleteMatchmakingConfiguration</a> </p> </li> <li> <p> <a>CreateMatchmakingRuleSet</a> </p> </li> <li> <p> <a>DescribeMatchmakingRuleSets</a> </p> </li> <li> <p> <a>ValidateMatchmakingRuleSet</a> </p> </li> </ul>

#### `AcceptMatchInput`

``` purescript
newtype AcceptMatchInput
  = AcceptMatchInput { "TicketId" :: MatchmakingIdStringModel, "PlayerIds" :: StringList, "AcceptanceType" :: AcceptanceType }
```

<p>Represents the input for a request action.</p>

#### `AcceptMatchOutput`

``` purescript
newtype AcceptMatchOutput
  = AcceptMatchOutput {  }
```

#### `AcceptanceType`

``` purescript
newtype AcceptanceType
  = AcceptanceType String
```

#### `Alias`

``` purescript
newtype Alias
  = Alias { "AliasId" :: NullOrUndefined (AliasId), "Name" :: NullOrUndefined (NonBlankAndLengthConstraintString), "AliasArn" :: NullOrUndefined (ArnStringModel), "Description" :: NullOrUndefined (FreeText), "RoutingStrategy" :: NullOrUndefined (RoutingStrategy), "CreationTime" :: NullOrUndefined (Number), "LastUpdatedTime" :: NullOrUndefined (Number) }
```

<p>Properties describing a fleet alias.</p> <p>Alias-related operations include:</p> <ul> <li> <p> <a>CreateAlias</a> </p> </li> <li> <p> <a>ListAliases</a> </p> </li> <li> <p> <a>DescribeAlias</a> </p> </li> <li> <p> <a>UpdateAlias</a> </p> </li> <li> <p> <a>DeleteAlias</a> </p> </li> <li> <p> <a>ResolveAlias</a> </p> </li> </ul>

#### `AliasId`

``` purescript
newtype AliasId
  = AliasId String
```

#### `AliasList`

``` purescript
newtype AliasList
  = AliasList (Array Alias)
```

#### `ArnStringModel`

``` purescript
newtype ArnStringModel
  = ArnStringModel String
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue { "S" :: NullOrUndefined (NonZeroAndMaxString), "N" :: NullOrUndefined (DoubleObject), "SL" :: NullOrUndefined (StringList), "SDM" :: NullOrUndefined (StringDoubleMap) }
```

<p>Values for use in <a>Player</a> attribute key:value pairs. This object lets you specify an attribute value using any of the valid data types: string, number, string array or data map. Each <code>AttributeValue</code> object can use only one of the available properties.</p>

#### `AwsCredentials`

``` purescript
newtype AwsCredentials
  = AwsCredentials { "AccessKeyId" :: NullOrUndefined (NonEmptyString), "SecretAccessKey" :: NullOrUndefined (NonEmptyString), "SessionToken" :: NullOrUndefined (NonEmptyString) }
```

<p>Temporary access credentials used for uploading game build files to Amazon GameLift. They are valid for a limited time. If they expire before you upload your game build, get a new set by calling <a>RequestUploadCredentials</a>.</p>

#### `BooleanModel`

``` purescript
newtype BooleanModel
  = BooleanModel Boolean
```

#### `Build`

``` purescript
newtype Build
  = Build { "BuildId" :: NullOrUndefined (BuildId), "Name" :: NullOrUndefined (FreeText), "Version" :: NullOrUndefined (FreeText), "Status" :: NullOrUndefined (BuildStatus), "SizeOnDisk" :: NullOrUndefined (PositiveLong), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "CreationTime" :: NullOrUndefined (Number) }
```

<p>Properties describing a game build.</p> <p>Build-related operations include:</p> <ul> <li> <p> <a>CreateBuild</a> </p> </li> <li> <p> <a>ListBuilds</a> </p> </li> <li> <p> <a>DescribeBuild</a> </p> </li> <li> <p> <a>UpdateBuild</a> </p> </li> <li> <p> <a>DeleteBuild</a> </p> </li> </ul>

#### `BuildId`

``` purescript
newtype BuildId
  = BuildId String
```

#### `BuildList`

``` purescript
newtype BuildList
  = BuildList (Array Build)
```

#### `BuildStatus`

``` purescript
newtype BuildStatus
  = BuildStatus String
```

#### `ComparisonOperatorType`

``` purescript
newtype ComparisonOperatorType
  = ComparisonOperatorType String
```

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The requested operation would cause a conflict with the current state of a service resource associated with the request. Resolve the conflict before retrying this request.</p>

#### `CreateAliasInput`

``` purescript
newtype CreateAliasInput
  = CreateAliasInput { "Name" :: NonBlankAndLengthConstraintString, "Description" :: NullOrUndefined (NonZeroAndMaxString), "RoutingStrategy" :: RoutingStrategy }
```

<p>Represents the input for a request action.</p>

#### `CreateAliasOutput`

``` purescript
newtype CreateAliasOutput
  = CreateAliasOutput { "Alias" :: NullOrUndefined (Alias) }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreateBuildInput`

``` purescript
newtype CreateBuildInput
  = CreateBuildInput { "Name" :: NullOrUndefined (NonZeroAndMaxString), "Version" :: NullOrUndefined (NonZeroAndMaxString), "StorageLocation" :: NullOrUndefined (S3Location), "OperatingSystem" :: NullOrUndefined (OperatingSystem) }
```

<p>Represents the input for a request action.</p>

#### `CreateBuildOutput`

``` purescript
newtype CreateBuildOutput
  = CreateBuildOutput { "Build" :: NullOrUndefined (Build), "UploadCredentials" :: NullOrUndefined (AwsCredentials), "StorageLocation" :: NullOrUndefined (S3Location) }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreateFleetInput`

``` purescript
newtype CreateFleetInput
  = CreateFleetInput { "Name" :: NonZeroAndMaxString, "Description" :: NullOrUndefined (NonZeroAndMaxString), "BuildId" :: BuildId, "ServerLaunchPath" :: NullOrUndefined (NonZeroAndMaxString), "ServerLaunchParameters" :: NullOrUndefined (NonZeroAndMaxString), "LogPaths" :: NullOrUndefined (StringList), "EC2InstanceType" :: EC2InstanceType, "EC2InboundPermissions" :: NullOrUndefined (IpPermissionsList), "NewGameSessionProtectionPolicy" :: NullOrUndefined (ProtectionPolicy), "RuntimeConfiguration" :: NullOrUndefined (RuntimeConfiguration), "ResourceCreationLimitPolicy" :: NullOrUndefined (ResourceCreationLimitPolicy), "MetricGroups" :: NullOrUndefined (MetricGroupList), "PeerVpcAwsAccountId" :: NullOrUndefined (NonZeroAndMaxString), "PeerVpcId" :: NullOrUndefined (NonZeroAndMaxString), "FleetType" :: NullOrUndefined (FleetType) }
```

<p>Represents the input for a request action.</p>

#### `CreateFleetOutput`

``` purescript
newtype CreateFleetOutput
  = CreateFleetOutput { "FleetAttributes" :: NullOrUndefined (FleetAttributes) }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreateGameSessionInput`

``` purescript
newtype CreateGameSessionInput
  = CreateGameSessionInput { "FleetId" :: NullOrUndefined (FleetId), "AliasId" :: NullOrUndefined (AliasId), "MaximumPlayerSessionCount" :: WholeNumber, "Name" :: NullOrUndefined (NonZeroAndMaxString), "GameProperties" :: NullOrUndefined (GamePropertyList), "CreatorId" :: NullOrUndefined (NonZeroAndMaxString), "GameSessionId" :: NullOrUndefined (IdStringModel), "IdempotencyToken" :: NullOrUndefined (IdStringModel), "GameSessionData" :: NullOrUndefined (GameSessionData) }
```

<p>Represents the input for a request action.</p>

#### `CreateGameSessionOutput`

``` purescript
newtype CreateGameSessionOutput
  = CreateGameSessionOutput { "GameSession" :: NullOrUndefined (GameSession) }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreateGameSessionQueueInput`

``` purescript
newtype CreateGameSessionQueueInput
  = CreateGameSessionQueueInput { "Name" :: GameSessionQueueName, "TimeoutInSeconds" :: NullOrUndefined (WholeNumber), "PlayerLatencyPolicies" :: NullOrUndefined (PlayerLatencyPolicyList), "Destinations" :: NullOrUndefined (GameSessionQueueDestinationList) }
```

<p>Represents the input for a request action.</p>

#### `CreateGameSessionQueueOutput`

``` purescript
newtype CreateGameSessionQueueOutput
  = CreateGameSessionQueueOutput { "GameSessionQueue" :: NullOrUndefined (GameSessionQueue) }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreateMatchmakingConfigurationInput`

``` purescript
newtype CreateMatchmakingConfigurationInput
  = CreateMatchmakingConfigurationInput { "Name" :: MatchmakingIdStringModel, "Description" :: NullOrUndefined (NonZeroAndMaxString), "GameSessionQueueArns" :: QueueArnsList, "RequestTimeoutSeconds" :: MatchmakingRequestTimeoutInteger, "AcceptanceTimeoutSeconds" :: NullOrUndefined (MatchmakingAcceptanceTimeoutInteger), "AcceptanceRequired" :: BooleanModel, "RuleSetName" :: MatchmakingIdStringModel, "NotificationTarget" :: NullOrUndefined (SnsArnStringModel), "AdditionalPlayerCount" :: NullOrUndefined (WholeNumber), "CustomEventData" :: NullOrUndefined (CustomEventData), "GameProperties" :: NullOrUndefined (GamePropertyList), "GameSessionData" :: NullOrUndefined (GameSessionData) }
```

<p>Represents the input for a request action.</p>

#### `CreateMatchmakingConfigurationOutput`

``` purescript
newtype CreateMatchmakingConfigurationOutput
  = CreateMatchmakingConfigurationOutput { "Configuration" :: NullOrUndefined (MatchmakingConfiguration) }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreateMatchmakingRuleSetInput`

``` purescript
newtype CreateMatchmakingRuleSetInput
  = CreateMatchmakingRuleSetInput { "Name" :: MatchmakingIdStringModel, "RuleSetBody" :: RuleSetBody }
```

<p>Represents the input for a request action.</p>

#### `CreateMatchmakingRuleSetOutput`

``` purescript
newtype CreateMatchmakingRuleSetOutput
  = CreateMatchmakingRuleSetOutput { "RuleSet" :: MatchmakingRuleSet }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreatePlayerSessionInput`

``` purescript
newtype CreatePlayerSessionInput
  = CreatePlayerSessionInput { "GameSessionId" :: ArnStringModel, "PlayerId" :: NonZeroAndMaxString, "PlayerData" :: NullOrUndefined (PlayerData) }
```

<p>Represents the input for a request action.</p>

#### `CreatePlayerSessionOutput`

``` purescript
newtype CreatePlayerSessionOutput
  = CreatePlayerSessionOutput { "PlayerSession" :: NullOrUndefined (PlayerSession) }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreatePlayerSessionsInput`

``` purescript
newtype CreatePlayerSessionsInput
  = CreatePlayerSessionsInput { "GameSessionId" :: ArnStringModel, "PlayerIds" :: PlayerIdList, "PlayerDataMap" :: NullOrUndefined (PlayerDataMap) }
```

<p>Represents the input for a request action.</p>

#### `CreatePlayerSessionsOutput`

``` purescript
newtype CreatePlayerSessionsOutput
  = CreatePlayerSessionsOutput { "PlayerSessions" :: NullOrUndefined (PlayerSessionList) }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreateVpcPeeringAuthorizationInput`

``` purescript
newtype CreateVpcPeeringAuthorizationInput
  = CreateVpcPeeringAuthorizationInput { "GameLiftAwsAccountId" :: NonZeroAndMaxString, "PeerVpcId" :: NonZeroAndMaxString }
```

<p>Represents the input for a request action.</p>

#### `CreateVpcPeeringAuthorizationOutput`

``` purescript
newtype CreateVpcPeeringAuthorizationOutput
  = CreateVpcPeeringAuthorizationOutput { "VpcPeeringAuthorization" :: NullOrUndefined (VpcPeeringAuthorization) }
```

<p>Represents the returned data in response to a request action.</p>

#### `CreateVpcPeeringConnectionInput`

``` purescript
newtype CreateVpcPeeringConnectionInput
  = CreateVpcPeeringConnectionInput { "FleetId" :: FleetId, "PeerVpcAwsAccountId" :: NonZeroAndMaxString, "PeerVpcId" :: NonZeroAndMaxString }
```

<p>Represents the input for a request action.</p>

#### `CreateVpcPeeringConnectionOutput`

``` purescript
newtype CreateVpcPeeringConnectionOutput
  = CreateVpcPeeringConnectionOutput {  }
```

#### `CustomEventData`

``` purescript
newtype CustomEventData
  = CustomEventData String
```

#### `DeleteAliasInput`

``` purescript
newtype DeleteAliasInput
  = DeleteAliasInput { "AliasId" :: AliasId }
```

<p>Represents the input for a request action.</p>

#### `DeleteBuildInput`

``` purescript
newtype DeleteBuildInput
  = DeleteBuildInput { "BuildId" :: BuildId }
```

<p>Represents the input for a request action.</p>

#### `DeleteFleetInput`

``` purescript
newtype DeleteFleetInput
  = DeleteFleetInput { "FleetId" :: FleetId }
```

<p>Represents the input for a request action.</p>

#### `DeleteGameSessionQueueInput`

``` purescript
newtype DeleteGameSessionQueueInput
  = DeleteGameSessionQueueInput { "Name" :: GameSessionQueueName }
```

<p>Represents the input for a request action.</p>

#### `DeleteGameSessionQueueOutput`

``` purescript
newtype DeleteGameSessionQueueOutput
  = DeleteGameSessionQueueOutput {  }
```

#### `DeleteMatchmakingConfigurationInput`

``` purescript
newtype DeleteMatchmakingConfigurationInput
  = DeleteMatchmakingConfigurationInput { "Name" :: MatchmakingIdStringModel }
```

<p>Represents the input for a request action.</p>

#### `DeleteMatchmakingConfigurationOutput`

``` purescript
newtype DeleteMatchmakingConfigurationOutput
  = DeleteMatchmakingConfigurationOutput {  }
```

#### `DeleteScalingPolicyInput`

``` purescript
newtype DeleteScalingPolicyInput
  = DeleteScalingPolicyInput { "Name" :: NonZeroAndMaxString, "FleetId" :: FleetId }
```

<p>Represents the input for a request action.</p>

#### `DeleteVpcPeeringAuthorizationInput`

``` purescript
newtype DeleteVpcPeeringAuthorizationInput
  = DeleteVpcPeeringAuthorizationInput { "GameLiftAwsAccountId" :: NonZeroAndMaxString, "PeerVpcId" :: NonZeroAndMaxString }
```

<p>Represents the input for a request action.</p>

#### `DeleteVpcPeeringAuthorizationOutput`

``` purescript
newtype DeleteVpcPeeringAuthorizationOutput
  = DeleteVpcPeeringAuthorizationOutput {  }
```

#### `DeleteVpcPeeringConnectionInput`

``` purescript
newtype DeleteVpcPeeringConnectionInput
  = DeleteVpcPeeringConnectionInput { "FleetId" :: FleetId, "VpcPeeringConnectionId" :: NonZeroAndMaxString }
```

<p>Represents the input for a request action.</p>

#### `DeleteVpcPeeringConnectionOutput`

``` purescript
newtype DeleteVpcPeeringConnectionOutput
  = DeleteVpcPeeringConnectionOutput {  }
```

#### `DescribeAliasInput`

``` purescript
newtype DescribeAliasInput
  = DescribeAliasInput { "AliasId" :: AliasId }
```

<p>Represents the input for a request action.</p>

#### `DescribeAliasOutput`

``` purescript
newtype DescribeAliasOutput
  = DescribeAliasOutput { "Alias" :: NullOrUndefined (Alias) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeBuildInput`

``` purescript
newtype DescribeBuildInput
  = DescribeBuildInput { "BuildId" :: BuildId }
```

<p>Represents the input for a request action.</p>

#### `DescribeBuildOutput`

``` purescript
newtype DescribeBuildOutput
  = DescribeBuildOutput { "Build" :: NullOrUndefined (Build) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeEC2InstanceLimitsInput`

``` purescript
newtype DescribeEC2InstanceLimitsInput
  = DescribeEC2InstanceLimitsInput { "EC2InstanceType" :: NullOrUndefined (EC2InstanceType) }
```

<p>Represents the input for a request action.</p>

#### `DescribeEC2InstanceLimitsOutput`

``` purescript
newtype DescribeEC2InstanceLimitsOutput
  = DescribeEC2InstanceLimitsOutput { "EC2InstanceLimits" :: NullOrUndefined (EC2InstanceLimitList) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeFleetAttributesInput`

``` purescript
newtype DescribeFleetAttributesInput
  = DescribeFleetAttributesInput { "FleetIds" :: NullOrUndefined (FleetIdList), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeFleetAttributesOutput`

``` purescript
newtype DescribeFleetAttributesOutput
  = DescribeFleetAttributesOutput { "FleetAttributes" :: NullOrUndefined (FleetAttributesList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeFleetCapacityInput`

``` purescript
newtype DescribeFleetCapacityInput
  = DescribeFleetCapacityInput { "FleetIds" :: NullOrUndefined (FleetIdList), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeFleetCapacityOutput`

``` purescript
newtype DescribeFleetCapacityOutput
  = DescribeFleetCapacityOutput { "FleetCapacity" :: NullOrUndefined (FleetCapacityList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeFleetEventsInput`

``` purescript
newtype DescribeFleetEventsInput
  = DescribeFleetEventsInput { "FleetId" :: FleetId, "StartTime" :: NullOrUndefined (Number), "EndTime" :: NullOrUndefined (Number), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeFleetEventsOutput`

``` purescript
newtype DescribeFleetEventsOutput
  = DescribeFleetEventsOutput { "Events" :: NullOrUndefined (EventList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeFleetPortSettingsInput`

``` purescript
newtype DescribeFleetPortSettingsInput
  = DescribeFleetPortSettingsInput { "FleetId" :: FleetId }
```

<p>Represents the input for a request action.</p>

#### `DescribeFleetPortSettingsOutput`

``` purescript
newtype DescribeFleetPortSettingsOutput
  = DescribeFleetPortSettingsOutput { "InboundPermissions" :: NullOrUndefined (IpPermissionsList) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeFleetUtilizationInput`

``` purescript
newtype DescribeFleetUtilizationInput
  = DescribeFleetUtilizationInput { "FleetIds" :: NullOrUndefined (FleetIdList), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeFleetUtilizationOutput`

``` purescript
newtype DescribeFleetUtilizationOutput
  = DescribeFleetUtilizationOutput { "FleetUtilization" :: NullOrUndefined (FleetUtilizationList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeGameSessionDetailsInput`

``` purescript
newtype DescribeGameSessionDetailsInput
  = DescribeGameSessionDetailsInput { "FleetId" :: NullOrUndefined (FleetId), "GameSessionId" :: NullOrUndefined (ArnStringModel), "AliasId" :: NullOrUndefined (AliasId), "StatusFilter" :: NullOrUndefined (NonZeroAndMaxString), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeGameSessionDetailsOutput`

``` purescript
newtype DescribeGameSessionDetailsOutput
  = DescribeGameSessionDetailsOutput { "GameSessionDetails" :: NullOrUndefined (GameSessionDetailList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeGameSessionPlacementInput`

``` purescript
newtype DescribeGameSessionPlacementInput
  = DescribeGameSessionPlacementInput { "PlacementId" :: IdStringModel }
```

<p>Represents the input for a request action.</p>

#### `DescribeGameSessionPlacementOutput`

``` purescript
newtype DescribeGameSessionPlacementOutput
  = DescribeGameSessionPlacementOutput { "GameSessionPlacement" :: NullOrUndefined (GameSessionPlacement) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeGameSessionQueuesInput`

``` purescript
newtype DescribeGameSessionQueuesInput
  = DescribeGameSessionQueuesInput { "Names" :: NullOrUndefined (GameSessionQueueNameList), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeGameSessionQueuesOutput`

``` purescript
newtype DescribeGameSessionQueuesOutput
  = DescribeGameSessionQueuesOutput { "GameSessionQueues" :: NullOrUndefined (GameSessionQueueList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeGameSessionsInput`

``` purescript
newtype DescribeGameSessionsInput
  = DescribeGameSessionsInput { "FleetId" :: NullOrUndefined (FleetId), "GameSessionId" :: NullOrUndefined (ArnStringModel), "AliasId" :: NullOrUndefined (AliasId), "StatusFilter" :: NullOrUndefined (NonZeroAndMaxString), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeGameSessionsOutput`

``` purescript
newtype DescribeGameSessionsOutput
  = DescribeGameSessionsOutput { "GameSessions" :: NullOrUndefined (GameSessionList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeInstancesInput`

``` purescript
newtype DescribeInstancesInput
  = DescribeInstancesInput { "FleetId" :: FleetId, "InstanceId" :: NullOrUndefined (InstanceId), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeInstancesOutput`

``` purescript
newtype DescribeInstancesOutput
  = DescribeInstancesOutput { "Instances" :: NullOrUndefined (InstanceList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeMatchmakingConfigurationsInput`

``` purescript
newtype DescribeMatchmakingConfigurationsInput
  = DescribeMatchmakingConfigurationsInput { "Names" :: NullOrUndefined (MatchmakingIdList), "RuleSetName" :: NullOrUndefined (MatchmakingIdStringModel), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeMatchmakingConfigurationsOutput`

``` purescript
newtype DescribeMatchmakingConfigurationsOutput
  = DescribeMatchmakingConfigurationsOutput { "Configurations" :: NullOrUndefined (MatchmakingConfigurationList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeMatchmakingInput`

``` purescript
newtype DescribeMatchmakingInput
  = DescribeMatchmakingInput { "TicketIds" :: MatchmakingIdList }
```

<p>Represents the input for a request action.</p>

#### `DescribeMatchmakingOutput`

``` purescript
newtype DescribeMatchmakingOutput
  = DescribeMatchmakingOutput { "TicketList" :: NullOrUndefined (MatchmakingTicketList) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeMatchmakingRuleSetsInput`

``` purescript
newtype DescribeMatchmakingRuleSetsInput
  = DescribeMatchmakingRuleSetsInput { "Names" :: NullOrUndefined (MatchmakingRuleSetNameList), "Limit" :: NullOrUndefined (RuleSetLimit), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeMatchmakingRuleSetsOutput`

``` purescript
newtype DescribeMatchmakingRuleSetsOutput
  = DescribeMatchmakingRuleSetsOutput { "RuleSets" :: MatchmakingRuleSetList, "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribePlayerSessionsInput`

``` purescript
newtype DescribePlayerSessionsInput
  = DescribePlayerSessionsInput { "GameSessionId" :: NullOrUndefined (ArnStringModel), "PlayerId" :: NullOrUndefined (NonZeroAndMaxString), "PlayerSessionId" :: NullOrUndefined (PlayerSessionId), "PlayerSessionStatusFilter" :: NullOrUndefined (NonZeroAndMaxString), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribePlayerSessionsOutput`

``` purescript
newtype DescribePlayerSessionsOutput
  = DescribePlayerSessionsOutput { "PlayerSessions" :: NullOrUndefined (PlayerSessionList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeRuntimeConfigurationInput`

``` purescript
newtype DescribeRuntimeConfigurationInput
  = DescribeRuntimeConfigurationInput { "FleetId" :: FleetId }
```

<p>Represents the input for a request action.</p>

#### `DescribeRuntimeConfigurationOutput`

``` purescript
newtype DescribeRuntimeConfigurationOutput
  = DescribeRuntimeConfigurationOutput { "RuntimeConfiguration" :: NullOrUndefined (RuntimeConfiguration) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeScalingPoliciesInput`

``` purescript
newtype DescribeScalingPoliciesInput
  = DescribeScalingPoliciesInput { "FleetId" :: FleetId, "StatusFilter" :: NullOrUndefined (ScalingStatusType), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `DescribeScalingPoliciesOutput`

``` purescript
newtype DescribeScalingPoliciesOutput
  = DescribeScalingPoliciesOutput { "ScalingPolicies" :: NullOrUndefined (ScalingPolicyList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DescribeVpcPeeringAuthorizationsInput`

``` purescript
newtype DescribeVpcPeeringAuthorizationsInput
  = DescribeVpcPeeringAuthorizationsInput {  }
```

#### `DescribeVpcPeeringAuthorizationsOutput`

``` purescript
newtype DescribeVpcPeeringAuthorizationsOutput
  = DescribeVpcPeeringAuthorizationsOutput { "VpcPeeringAuthorizations" :: NullOrUndefined (VpcPeeringAuthorizationList) }
```

#### `DescribeVpcPeeringConnectionsInput`

``` purescript
newtype DescribeVpcPeeringConnectionsInput
  = DescribeVpcPeeringConnectionsInput { "FleetId" :: NullOrUndefined (FleetId) }
```

<p>Represents the input for a request action.</p>

#### `DescribeVpcPeeringConnectionsOutput`

``` purescript
newtype DescribeVpcPeeringConnectionsOutput
  = DescribeVpcPeeringConnectionsOutput { "VpcPeeringConnections" :: NullOrUndefined (VpcPeeringConnectionList) }
```

<p>Represents the returned data in response to a request action.</p>

#### `DesiredPlayerSession`

``` purescript
newtype DesiredPlayerSession
  = DesiredPlayerSession { "PlayerId" :: NullOrUndefined (NonZeroAndMaxString), "PlayerData" :: NullOrUndefined (PlayerData) }
```

<p>Player information for use when creating player sessions using a game session placement request with <a>StartGameSessionPlacement</a>.</p>

#### `DesiredPlayerSessionList`

``` purescript
newtype DesiredPlayerSessionList
  = DesiredPlayerSessionList (Array DesiredPlayerSession)
```

#### `DoubleObject`

``` purescript
newtype DoubleObject
  = DoubleObject Number
```

#### `EC2InstanceCounts`

``` purescript
newtype EC2InstanceCounts
  = EC2InstanceCounts { "DESIRED" :: NullOrUndefined (WholeNumber), "MINIMUM" :: NullOrUndefined (WholeNumber), "MAXIMUM" :: NullOrUndefined (WholeNumber), "PENDING" :: NullOrUndefined (WholeNumber), "ACTIVE" :: NullOrUndefined (WholeNumber), "IDLE" :: NullOrUndefined (WholeNumber), "TERMINATING" :: NullOrUndefined (WholeNumber) }
```

<p>Current status of fleet capacity. The number of active instances should match or be in the process of matching the number of desired instances. Pending and terminating counts are non-zero only if fleet capacity is adjusting to an <a>UpdateFleetCapacity</a> request, or if access to resources is temporarily affected.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `EC2InstanceLimit`

``` purescript
newtype EC2InstanceLimit
  = EC2InstanceLimit { "EC2InstanceType" :: NullOrUndefined (EC2InstanceType), "CurrentInstances" :: NullOrUndefined (WholeNumber), "InstanceLimit" :: NullOrUndefined (WholeNumber) }
```

<p>Maximum number of instances allowed based on the Amazon Elastic Compute Cloud (Amazon EC2) instance type. Instance limits can be retrieved by calling <a>DescribeEC2InstanceLimits</a>.</p>

#### `EC2InstanceLimitList`

``` purescript
newtype EC2InstanceLimitList
  = EC2InstanceLimitList (Array EC2InstanceLimit)
```

#### `EC2InstanceType`

``` purescript
newtype EC2InstanceType
  = EC2InstanceType String
```

#### `Event`

``` purescript
newtype Event
  = Event { "EventId" :: NullOrUndefined (NonZeroAndMaxString), "ResourceId" :: NullOrUndefined (NonZeroAndMaxString), "EventCode" :: NullOrUndefined (EventCode), "Message" :: NullOrUndefined (NonEmptyString), "EventTime" :: NullOrUndefined (Number), "PreSignedLogUrl" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Log entry describing an event that involves Amazon GameLift resources (such as a fleet). In addition to tracking activity, event codes and messages can provide additional information for troubleshooting and debugging problems.</p>

#### `EventCode`

``` purescript
newtype EventCode
  = EventCode String
```

#### `EventList`

``` purescript
newtype EventList
  = EventList (Array Event)
```

#### `FleetAttributes`

``` purescript
newtype FleetAttributes
  = FleetAttributes { "FleetId" :: NullOrUndefined (FleetId), "FleetArn" :: NullOrUndefined (ArnStringModel), "FleetType" :: NullOrUndefined (FleetType), "InstanceType" :: NullOrUndefined (EC2InstanceType), "Description" :: NullOrUndefined (NonZeroAndMaxString), "Name" :: NullOrUndefined (NonZeroAndMaxString), "CreationTime" :: NullOrUndefined (Number), "TerminationTime" :: NullOrUndefined (Number), "Status" :: NullOrUndefined (FleetStatus), "BuildId" :: NullOrUndefined (BuildId), "ServerLaunchPath" :: NullOrUndefined (NonZeroAndMaxString), "ServerLaunchParameters" :: NullOrUndefined (NonZeroAndMaxString), "LogPaths" :: NullOrUndefined (StringList), "NewGameSessionProtectionPolicy" :: NullOrUndefined (ProtectionPolicy), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "ResourceCreationLimitPolicy" :: NullOrUndefined (ResourceCreationLimitPolicy), "MetricGroups" :: NullOrUndefined (MetricGroupList) }
```

<p>General properties describing a fleet.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `FleetAttributesList`

``` purescript
newtype FleetAttributesList
  = FleetAttributesList (Array FleetAttributes)
```

#### `FleetCapacity`

``` purescript
newtype FleetCapacity
  = FleetCapacity { "FleetId" :: NullOrUndefined (FleetId), "InstanceType" :: NullOrUndefined (EC2InstanceType), "InstanceCounts" :: NullOrUndefined (EC2InstanceCounts) }
```

<p>Information about the fleet's capacity. Fleet capacity is measured in EC2 instances. By default, new fleets have a capacity of one instance, but can be updated as needed. The maximum number of instances for a fleet is determined by the fleet's instance type.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `FleetCapacityExceededException`

``` purescript
newtype FleetCapacityExceededException
  = FleetCapacityExceededException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The specified fleet has no available instances to fulfill a <code>CreateGameSession</code> request. Clients can retry such requests immediately or after a waiting period.</p>

#### `FleetCapacityList`

``` purescript
newtype FleetCapacityList
  = FleetCapacityList (Array FleetCapacity)
```

#### `FleetId`

``` purescript
newtype FleetId
  = FleetId String
```

#### `FleetIdList`

``` purescript
newtype FleetIdList
  = FleetIdList (Array FleetId)
```

#### `FleetStatus`

``` purescript
newtype FleetStatus
  = FleetStatus String
```

#### `FleetType`

``` purescript
newtype FleetType
  = FleetType String
```

#### `FleetUtilization`

``` purescript
newtype FleetUtilization
  = FleetUtilization { "FleetId" :: NullOrUndefined (FleetId), "ActiveServerProcessCount" :: NullOrUndefined (WholeNumber), "ActiveGameSessionCount" :: NullOrUndefined (WholeNumber), "CurrentPlayerSessionCount" :: NullOrUndefined (WholeNumber), "MaximumPlayerSessionCount" :: NullOrUndefined (WholeNumber) }
```

<p>Current status of fleet utilization, including the number of game and player sessions being hosted.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `FleetUtilizationList`

``` purescript
newtype FleetUtilizationList
  = FleetUtilizationList (Array FleetUtilization)
```

#### `FreeText`

``` purescript
newtype FreeText
  = FreeText String
```

#### `GameProperty`

``` purescript
newtype GameProperty
  = GameProperty { "Key" :: GamePropertyKey, "Value" :: GamePropertyValue }
```

<p>Set of key-value pairs that contain information about a game session. When included in a game session request, these properties communicate details to be used when setting up the new game session, such as to specify a game mode, level, or map. Game properties are passed to the game server process when initiating a new game session; the server process uses the properties as appropriate. For more information, see the <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-client-api.html#gamelift-sdk-client-api-create"> Amazon GameLift Developer Guide</a>.</p>

#### `GamePropertyKey`

``` purescript
newtype GamePropertyKey
  = GamePropertyKey String
```

#### `GamePropertyList`

``` purescript
newtype GamePropertyList
  = GamePropertyList (Array GameProperty)
```

#### `GamePropertyValue`

``` purescript
newtype GamePropertyValue
  = GamePropertyValue String
```

#### `GameSession`

``` purescript
newtype GameSession
  = GameSession { "GameSessionId" :: NullOrUndefined (NonZeroAndMaxString), "Name" :: NullOrUndefined (NonZeroAndMaxString), "FleetId" :: NullOrUndefined (FleetId), "CreationTime" :: NullOrUndefined (Number), "TerminationTime" :: NullOrUndefined (Number), "CurrentPlayerSessionCount" :: NullOrUndefined (WholeNumber), "MaximumPlayerSessionCount" :: NullOrUndefined (WholeNumber), "Status" :: NullOrUndefined (GameSessionStatus), "StatusReason" :: NullOrUndefined (GameSessionStatusReason), "GameProperties" :: NullOrUndefined (GamePropertyList), "IpAddress" :: NullOrUndefined (IpAddress), "Port" :: NullOrUndefined (PortNumber), "PlayerSessionCreationPolicy" :: NullOrUndefined (PlayerSessionCreationPolicy), "CreatorId" :: NullOrUndefined (NonZeroAndMaxString), "GameSessionData" :: NullOrUndefined (GameSessionData), "MatchmakerData" :: NullOrUndefined (MatchmakerData) }
```

<p>Properties describing a game session.</p> <p>A game session in ACTIVE status can host players. When a game session ends, its status is set to <code>TERMINATED</code>. </p> <p>Once the session ends, the game session object is retained for 30 days. This means you can reuse idempotency token values after this time. Game session logs are retained for 14 days.</p> <p>Game-session-related operations include:</p> <ul> <li> <p> <a>CreateGameSession</a> </p> </li> <li> <p> <a>DescribeGameSessions</a> </p> </li> <li> <p> <a>DescribeGameSessionDetails</a> </p> </li> <li> <p> <a>SearchGameSessions</a> </p> </li> <li> <p> <a>UpdateGameSession</a> </p> </li> <li> <p> <a>GetGameSessionLogUrl</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `GameSessionActivationTimeoutSeconds`

``` purescript
newtype GameSessionActivationTimeoutSeconds
  = GameSessionActivationTimeoutSeconds Int
```

#### `GameSessionConnectionInfo`

``` purescript
newtype GameSessionConnectionInfo
  = GameSessionConnectionInfo { "GameSessionArn" :: NullOrUndefined (ArnStringModel), "IpAddress" :: NullOrUndefined (StringModel), "Port" :: NullOrUndefined (PositiveInteger), "MatchedPlayerSessions" :: NullOrUndefined (MatchedPlayerSessionList) }
```

<p>Connection information for the new game session that is created with matchmaking. (with <a>StartMatchmaking</a>). Once a match is set, the FlexMatch engine places the match and creates a new game session for it. This information, including the game session endpoint and player sessions for each player in the original matchmaking request, is added to the <a>MatchmakingTicket</a>, which can be retrieved by calling <a>DescribeMatchmaking</a>.</p>

#### `GameSessionData`

``` purescript
newtype GameSessionData
  = GameSessionData String
```

#### `GameSessionDetail`

``` purescript
newtype GameSessionDetail
  = GameSessionDetail { "GameSession" :: NullOrUndefined (GameSession), "ProtectionPolicy" :: NullOrUndefined (ProtectionPolicy) }
```

<p>A game session's properties plus the protection policy currently in force.</p>

#### `GameSessionDetailList`

``` purescript
newtype GameSessionDetailList
  = GameSessionDetailList (Array GameSessionDetail)
```

#### `GameSessionFullException`

``` purescript
newtype GameSessionFullException
  = GameSessionFullException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The game instance is currently full and cannot allow the requested player(s) to join. Clients can retry such requests immediately or after a waiting period.</p>

#### `GameSessionList`

``` purescript
newtype GameSessionList
  = GameSessionList (Array GameSession)
```

#### `GameSessionPlacement`

``` purescript
newtype GameSessionPlacement
  = GameSessionPlacement { "PlacementId" :: NullOrUndefined (IdStringModel), "GameSessionQueueName" :: NullOrUndefined (GameSessionQueueName), "Status" :: NullOrUndefined (GameSessionPlacementState), "GameProperties" :: NullOrUndefined (GamePropertyList), "MaximumPlayerSessionCount" :: NullOrUndefined (WholeNumber), "GameSessionName" :: NullOrUndefined (NonZeroAndMaxString), "GameSessionId" :: NullOrUndefined (NonZeroAndMaxString), "GameSessionArn" :: NullOrUndefined (NonZeroAndMaxString), "GameSessionRegion" :: NullOrUndefined (NonZeroAndMaxString), "PlayerLatencies" :: NullOrUndefined (PlayerLatencyList), "StartTime" :: NullOrUndefined (Number), "EndTime" :: NullOrUndefined (Number), "IpAddress" :: NullOrUndefined (IpAddress), "Port" :: NullOrUndefined (PortNumber), "PlacedPlayerSessions" :: NullOrUndefined (PlacedPlayerSessionList), "GameSessionData" :: NullOrUndefined (GameSessionData), "MatchmakerData" :: NullOrUndefined (MatchmakerData) }
```

<p>Object that describes a <a>StartGameSessionPlacement</a> request. This object includes the full details of the original request plus the current status and start/end time stamps.</p> <p>Game session placement-related operations include:</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul>

#### `GameSessionPlacementState`

``` purescript
newtype GameSessionPlacementState
  = GameSessionPlacementState String
```

#### `GameSessionQueue`

``` purescript
newtype GameSessionQueue
  = GameSessionQueue { "Name" :: NullOrUndefined (GameSessionQueueName), "GameSessionQueueArn" :: NullOrUndefined (ArnStringModel), "TimeoutInSeconds" :: NullOrUndefined (WholeNumber), "PlayerLatencyPolicies" :: NullOrUndefined (PlayerLatencyPolicyList), "Destinations" :: NullOrUndefined (GameSessionQueueDestinationList) }
```

<p>Configuration of a queue that is used to process game session placement requests. The queue configuration identifies several game features:</p> <ul> <li> <p>The destinations where a new game session can potentially be hosted. Amazon GameLift tries these destinations in an order based on either the queue's default order or player latency information, if provided in a placement request. With latency information, Amazon GameLift can place game sessions where the majority of players are reporting the lowest possible latency. </p> </li> <li> <p>The length of time that placement requests can wait in the queue before timing out. </p> </li> <li> <p>A set of optional latency policies that protect individual players from high latencies, preventing game sessions from being placed where any individual player is reporting latency higher than a policy's maximum.</p> </li> </ul> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>

#### `GameSessionQueueDestination`

``` purescript
newtype GameSessionQueueDestination
  = GameSessionQueueDestination { "DestinationArn" :: NullOrUndefined (ArnStringModel) }
```

<p>Fleet designated in a game session queue. Requests for new game sessions in the queue are fulfilled by starting a new game session on any destination configured for a queue. </p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>

#### `GameSessionQueueDestinationList`

``` purescript
newtype GameSessionQueueDestinationList
  = GameSessionQueueDestinationList (Array GameSessionQueueDestination)
```

#### `GameSessionQueueList`

``` purescript
newtype GameSessionQueueList
  = GameSessionQueueList (Array GameSessionQueue)
```

#### `GameSessionQueueName`

``` purescript
newtype GameSessionQueueName
  = GameSessionQueueName String
```

#### `GameSessionQueueNameList`

``` purescript
newtype GameSessionQueueNameList
  = GameSessionQueueNameList (Array GameSessionQueueName)
```

#### `GameSessionStatus`

``` purescript
newtype GameSessionStatus
  = GameSessionStatus String
```

#### `GameSessionStatusReason`

``` purescript
newtype GameSessionStatusReason
  = GameSessionStatusReason String
```

#### `GetGameSessionLogUrlInput`

``` purescript
newtype GetGameSessionLogUrlInput
  = GetGameSessionLogUrlInput { "GameSessionId" :: ArnStringModel }
```

<p>Represents the input for a request action.</p>

#### `GetGameSessionLogUrlOutput`

``` purescript
newtype GetGameSessionLogUrlOutput
  = GetGameSessionLogUrlOutput { "PreSignedUrl" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `GetInstanceAccessInput`

``` purescript
newtype GetInstanceAccessInput
  = GetInstanceAccessInput { "FleetId" :: FleetId, "InstanceId" :: InstanceId }
```

<p>Represents the input for a request action.</p>

#### `GetInstanceAccessOutput`

``` purescript
newtype GetInstanceAccessOutput
  = GetInstanceAccessOutput { "InstanceAccess" :: NullOrUndefined (InstanceAccess) }
```

<p>Represents the returned data in response to a request action.</p>

#### `IdStringModel`

``` purescript
newtype IdStringModel
  = IdStringModel String
```

#### `IdempotentParameterMismatchException`

``` purescript
newtype IdempotentParameterMismatchException
  = IdempotentParameterMismatchException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>A game session with this custom ID string already exists in this fleet. Resolve this conflict before retrying this request.</p>

#### `Instance`

``` purescript
newtype Instance
  = Instance { "FleetId" :: NullOrUndefined (FleetId), "InstanceId" :: NullOrUndefined (InstanceId), "IpAddress" :: NullOrUndefined (IpAddress), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "Type" :: NullOrUndefined (EC2InstanceType), "Status" :: NullOrUndefined (InstanceStatus), "CreationTime" :: NullOrUndefined (Number) }
```

<p>Properties that describe an instance of a virtual computing resource that hosts one or more game servers. A fleet may contain zero or more instances.</p>

#### `InstanceAccess`

``` purescript
newtype InstanceAccess
  = InstanceAccess { "FleetId" :: NullOrUndefined (FleetId), "InstanceId" :: NullOrUndefined (InstanceId), "IpAddress" :: NullOrUndefined (IpAddress), "OperatingSystem" :: NullOrUndefined (OperatingSystem), "Credentials" :: NullOrUndefined (InstanceCredentials) }
```

<p>Information required to remotely connect to a fleet instance. Access is requested by calling <a>GetInstanceAccess</a>. </p>

#### `InstanceCredentials`

``` purescript
newtype InstanceCredentials
  = InstanceCredentials { "UserName" :: NullOrUndefined (NonEmptyString), "Secret" :: NullOrUndefined (NonEmptyString) }
```

<p>Set of credentials required to remotely access a fleet instance. Access credentials are requested by calling <a>GetInstanceAccess</a> and returned in an <a>InstanceAccess</a> object.</p>

#### `InstanceId`

``` purescript
newtype InstanceId
  = InstanceId String
```

#### `InstanceList`

``` purescript
newtype InstanceList
  = InstanceList (Array Instance)
```

#### `InstanceStatus`

``` purescript
newtype InstanceStatus
  = InstanceStatus String
```

#### `InternalServiceException`

``` purescript
newtype InternalServiceException
  = InternalServiceException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The service encountered an unrecoverable internal failure while processing the request. Clients can retry such requests immediately or after a waiting period.</p>

#### `InvalidFleetStatusException`

``` purescript
newtype InvalidFleetStatusException
  = InvalidFleetStatusException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The requested operation would cause a conflict with the current state of a resource associated with the request and/or the fleet. Resolve the conflict before retrying.</p>

#### `InvalidGameSessionStatusException`

``` purescript
newtype InvalidGameSessionStatusException
  = InvalidGameSessionStatusException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The requested operation would cause a conflict with the current state of a resource associated with the request and/or the game instance. Resolve the conflict before retrying.</p>

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>One or more parameter values in the request are invalid. Correct the invalid parameter values before retrying.</p>

#### `IpAddress`

``` purescript
newtype IpAddress
  = IpAddress String
```

#### `IpPermission`

``` purescript
newtype IpPermission
  = IpPermission { "FromPort" :: PortNumber, "ToPort" :: PortNumber, "IpRange" :: NonBlankString, "Protocol" :: IpProtocol }
```

<p>A range of IP addresses and port settings that allow inbound traffic to connect to server processes on Amazon GameLift. Each game session hosted on a fleet is assigned a unique combination of IP address and port number, which must fall into the fleet's allowed ranges. This combination is included in the <a>GameSession</a> object. </p>

#### `IpPermissionsList`

``` purescript
newtype IpPermissionsList
  = IpPermissionsList (Array IpPermission)
```

#### `IpProtocol`

``` purescript
newtype IpProtocol
  = IpProtocol String
```

#### `LatencyMap`

``` purescript
newtype LatencyMap
  = LatencyMap (Map NonEmptyString PositiveInteger)
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The requested operation would cause the resource to exceed the allowed service limit. Resolve the issue before retrying.</p>

#### `ListAliasesInput`

``` purescript
newtype ListAliasesInput
  = ListAliasesInput { "RoutingStrategyType" :: NullOrUndefined (RoutingStrategyType), "Name" :: NullOrUndefined (NonEmptyString), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonEmptyString) }
```

<p>Represents the input for a request action.</p>

#### `ListAliasesOutput`

``` purescript
newtype ListAliasesOutput
  = ListAliasesOutput { "Aliases" :: NullOrUndefined (AliasList), "NextToken" :: NullOrUndefined (NonEmptyString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `ListBuildsInput`

``` purescript
newtype ListBuildsInput
  = ListBuildsInput { "Status" :: NullOrUndefined (BuildStatus), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonEmptyString) }
```

<p>Represents the input for a request action.</p>

#### `ListBuildsOutput`

``` purescript
newtype ListBuildsOutput
  = ListBuildsOutput { "Builds" :: NullOrUndefined (BuildList), "NextToken" :: NullOrUndefined (NonEmptyString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `ListFleetsInput`

``` purescript
newtype ListFleetsInput
  = ListFleetsInput { "BuildId" :: NullOrUndefined (BuildId), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `ListFleetsOutput`

``` purescript
newtype ListFleetsOutput
  = ListFleetsOutput { "FleetIds" :: NullOrUndefined (FleetIdList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `MatchedPlayerSession`

``` purescript
newtype MatchedPlayerSession
  = MatchedPlayerSession { "PlayerId" :: NullOrUndefined (NonZeroAndMaxString), "PlayerSessionId" :: NullOrUndefined (PlayerSessionId) }
```

<p>Represents a new player session that is created as a result of a successful FlexMatch match. A successful match automatically creates new player sessions for every player ID in the original matchmaking request. </p> <p>When players connect to the match's game session, they must include both player ID and player session ID in order to claim their assigned player slot.</p>

#### `MatchedPlayerSessionList`

``` purescript
newtype MatchedPlayerSessionList
  = MatchedPlayerSessionList (Array MatchedPlayerSession)
```

#### `MatchmakerData`

``` purescript
newtype MatchmakerData
  = MatchmakerData String
```

#### `MatchmakingAcceptanceTimeoutInteger`

``` purescript
newtype MatchmakingAcceptanceTimeoutInteger
  = MatchmakingAcceptanceTimeoutInteger Int
```

#### `MatchmakingConfiguration`

``` purescript
newtype MatchmakingConfiguration
  = MatchmakingConfiguration { "Name" :: NullOrUndefined (MatchmakingIdStringModel), "Description" :: NullOrUndefined (NonZeroAndMaxString), "GameSessionQueueArns" :: NullOrUndefined (QueueArnsList), "RequestTimeoutSeconds" :: NullOrUndefined (MatchmakingRequestTimeoutInteger), "AcceptanceTimeoutSeconds" :: NullOrUndefined (MatchmakingAcceptanceTimeoutInteger), "AcceptanceRequired" :: NullOrUndefined (BooleanModel), "RuleSetName" :: NullOrUndefined (MatchmakingIdStringModel), "NotificationTarget" :: NullOrUndefined (SnsArnStringModel), "AdditionalPlayerCount" :: NullOrUndefined (WholeNumber), "CustomEventData" :: NullOrUndefined (CustomEventData), "CreationTime" :: NullOrUndefined (Number), "GameProperties" :: NullOrUndefined (GamePropertyList), "GameSessionData" :: NullOrUndefined (GameSessionData) }
```

<p>Guidelines for use with FlexMatch to match players into games. All matchmaking requests must specify a matchmaking configuration.</p>

#### `MatchmakingConfigurationList`

``` purescript
newtype MatchmakingConfigurationList
  = MatchmakingConfigurationList (Array MatchmakingConfiguration)
```

#### `MatchmakingConfigurationStatus`

``` purescript
newtype MatchmakingConfigurationStatus
  = MatchmakingConfigurationStatus String
```

#### `MatchmakingIdList`

``` purescript
newtype MatchmakingIdList
  = MatchmakingIdList (Array MatchmakingIdStringModel)
```

#### `MatchmakingIdStringModel`

``` purescript
newtype MatchmakingIdStringModel
  = MatchmakingIdStringModel String
```

#### `MatchmakingRequestTimeoutInteger`

``` purescript
newtype MatchmakingRequestTimeoutInteger
  = MatchmakingRequestTimeoutInteger Int
```

#### `MatchmakingRuleSet`

``` purescript
newtype MatchmakingRuleSet
  = MatchmakingRuleSet { "RuleSetName" :: NullOrUndefined (MatchmakingIdStringModel), "RuleSetBody" :: RuleSetBody, "CreationTime" :: NullOrUndefined (Number) }
```

<p>Set of rule statements, used with FlexMatch, that determine how to build a certain kind of player match. Each rule set describes a type of group to be created and defines the parameters for acceptable player matches. Rule sets are used in <a>MatchmakingConfiguration</a> objects.</p> <p>A rule set may define the following elements for a match. For detailed information and examples showing how to construct a rule set, see <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/match-rulesets.html">Build a FlexMatch Rule Set</a>. </p> <ul> <li> <p>Teams -- Required. A rule set must define one or multiple teams for the match and set minimum and maximum team sizes. For example, a rule set might describe a 4x4 match that requires all eight slots to be filled. </p> </li> <li> <p>Player attributes -- Optional. These attributes specify a set of player characteristics to evaluate when looking for a match. Matchmaking requests that use a rule set with player attributes must provide the corresponding attribute values. For example, an attribute might specify a player's skill or level.</p> </li> <li> <p>Rules -- Optional. Rules define how to evaluate potential players for a match based on player attributes. A rule might specify minimum requirements for individual players, teams, or entire matches. For example, a rule might require each player to meet a certain skill level, each team to have at least one player in a certain role, or the match to have a minimum average skill level. or may describe an entire group--such as all teams must be evenly matched or have at least one player in a certain role. </p> </li> <li> <p>Expansions -- Optional. Expansions allow you to relax the rules after a period of time when no acceptable matches are found. This feature lets you balance getting players into games in a reasonable amount of time instead of making them wait indefinitely for the best possible match. For example, you might use an expansion to increase the maximum skill variance between players after 30 seconds.</p> </li> </ul>

#### `MatchmakingRuleSetList`

``` purescript
newtype MatchmakingRuleSetList
  = MatchmakingRuleSetList (Array MatchmakingRuleSet)
```

#### `MatchmakingRuleSetNameList`

``` purescript
newtype MatchmakingRuleSetNameList
  = MatchmakingRuleSetNameList (Array MatchmakingIdStringModel)
```

#### `MatchmakingTicket`

``` purescript
newtype MatchmakingTicket
  = MatchmakingTicket { "TicketId" :: NullOrUndefined (MatchmakingIdStringModel), "ConfigurationName" :: NullOrUndefined (MatchmakingIdStringModel), "Status" :: NullOrUndefined (MatchmakingConfigurationStatus), "StatusReason" :: NullOrUndefined (StringModel), "StatusMessage" :: NullOrUndefined (StringModel), "StartTime" :: NullOrUndefined (Number), "EndTime" :: NullOrUndefined (Number), "Players" :: NullOrUndefined (PlayerList), "GameSessionConnectionInfo" :: NullOrUndefined (GameSessionConnectionInfo), "EstimatedWaitTime" :: NullOrUndefined (WholeNumber) }
```

<p>Ticket generated to track the progress of a matchmaking request. Each ticket is uniquely identified by a ticket ID, supplied by the requester, when creating a matchmaking request with <a>StartMatchmaking</a>. Tickets can be retrieved by calling <a>DescribeMatchmaking</a> with the ticket ID.</p>

#### `MatchmakingTicketList`

``` purescript
newtype MatchmakingTicketList
  = MatchmakingTicketList (Array MatchmakingTicket)
```

#### `MaxConcurrentGameSessionActivations`

``` purescript
newtype MaxConcurrentGameSessionActivations
  = MaxConcurrentGameSessionActivations Int
```

#### `MetricGroup`

``` purescript
newtype MetricGroup
  = MetricGroup String
```

#### `MetricGroupList`

``` purescript
newtype MetricGroupList
  = MetricGroupList (Array MetricGroup)
```

#### `MetricName`

``` purescript
newtype MetricName
  = MetricName String
```

#### `NonBlankAndLengthConstraintString`

``` purescript
newtype NonBlankAndLengthConstraintString
  = NonBlankAndLengthConstraintString String
```

#### `NonBlankString`

``` purescript
newtype NonBlankString
  = NonBlankString String
```

#### `NonEmptyString`

``` purescript
newtype NonEmptyString
  = NonEmptyString String
```

#### `NonZeroAndMaxString`

``` purescript
newtype NonZeroAndMaxString
  = NonZeroAndMaxString String
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>A service resource associated with the request could not be found. Clients should not retry such requests.</p>

#### `OperatingSystem`

``` purescript
newtype OperatingSystem
  = OperatingSystem String
```

#### `PlacedPlayerSession`

``` purescript
newtype PlacedPlayerSession
  = PlacedPlayerSession { "PlayerId" :: NullOrUndefined (NonZeroAndMaxString), "PlayerSessionId" :: NullOrUndefined (PlayerSessionId) }
```

<p>Information about a player session that was created as part of a <a>StartGameSessionPlacement</a> request. This object contains only the player ID and player session ID. To retrieve full details on a player session, call <a>DescribePlayerSessions</a> with the player session ID.</p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `PlacedPlayerSessionList`

``` purescript
newtype PlacedPlayerSessionList
  = PlacedPlayerSessionList (Array PlacedPlayerSession)
```

#### `Player`

``` purescript
newtype Player
  = Player { "PlayerId" :: NullOrUndefined (NonZeroAndMaxString), "PlayerAttributes" :: NullOrUndefined (PlayerAttributeMap), "Team" :: NullOrUndefined (NonZeroAndMaxString), "LatencyInMs" :: NullOrUndefined (LatencyMap) }
```

<p>Represents a player in matchmaking. When starting a matchmaking request, a player has a player ID, attributes, and may have latency data. Team information is added after a match has been successfully completed.</p>

#### `PlayerAttributeMap`

``` purescript
newtype PlayerAttributeMap
  = PlayerAttributeMap (Map NonZeroAndMaxString AttributeValue)
```

#### `PlayerData`

``` purescript
newtype PlayerData
  = PlayerData String
```

#### `PlayerDataMap`

``` purescript
newtype PlayerDataMap
  = PlayerDataMap (Map NonZeroAndMaxString PlayerData)
```

#### `PlayerIdList`

``` purescript
newtype PlayerIdList
  = PlayerIdList (Array NonZeroAndMaxString)
```

#### `PlayerLatency`

``` purescript
newtype PlayerLatency
  = PlayerLatency { "PlayerId" :: NullOrUndefined (NonZeroAndMaxString), "RegionIdentifier" :: NullOrUndefined (NonZeroAndMaxString), "LatencyInMilliseconds" :: NullOrUndefined (Number) }
```

<p>Regional latency information for a player, used when requesting a new game session with <a>StartGameSessionPlacement</a>. This value indicates the amount of time lag that exists when the player is connected to a fleet in the specified region. The relative difference between a player's latency values for multiple regions are used to determine which fleets are best suited to place a new game session for the player. </p>

#### `PlayerLatencyList`

``` purescript
newtype PlayerLatencyList
  = PlayerLatencyList (Array PlayerLatency)
```

#### `PlayerLatencyPolicy`

``` purescript
newtype PlayerLatencyPolicy
  = PlayerLatencyPolicy { "MaximumIndividualPlayerLatencyMilliseconds" :: NullOrUndefined (WholeNumber), "PolicyDurationSeconds" :: NullOrUndefined (WholeNumber) }
```

<p>Queue setting that determines the highest latency allowed for individual players when placing a game session. When a latency policy is in force, a game session cannot be placed at any destination in a region where a player is reporting latency higher than the cap. Latency policies are only enforced when the placement request contains player latency information.</p> <p>Queue-related operations include:</p> <ul> <li> <p> <a>CreateGameSessionQueue</a> </p> </li> <li> <p> <a>DescribeGameSessionQueues</a> </p> </li> <li> <p> <a>UpdateGameSessionQueue</a> </p> </li> <li> <p> <a>DeleteGameSessionQueue</a> </p> </li> </ul>

#### `PlayerLatencyPolicyList`

``` purescript
newtype PlayerLatencyPolicyList
  = PlayerLatencyPolicyList (Array PlayerLatencyPolicy)
```

#### `PlayerList`

``` purescript
newtype PlayerList
  = PlayerList (Array Player)
```

#### `PlayerSession`

``` purescript
newtype PlayerSession
  = PlayerSession { "PlayerSessionId" :: NullOrUndefined (PlayerSessionId), "PlayerId" :: NullOrUndefined (NonZeroAndMaxString), "GameSessionId" :: NullOrUndefined (NonZeroAndMaxString), "FleetId" :: NullOrUndefined (FleetId), "CreationTime" :: NullOrUndefined (Number), "TerminationTime" :: NullOrUndefined (Number), "Status" :: NullOrUndefined (PlayerSessionStatus), "IpAddress" :: NullOrUndefined (IpAddress), "Port" :: NullOrUndefined (PortNumber), "PlayerData" :: NullOrUndefined (PlayerData) }
```

<p>Properties describing a player session. Player session objects are created either by creating a player session for a specific game session, or as part of a game session placement. A player session represents either a player reservation for a game session (status <code>RESERVED</code>) or actual player activity in a game session (status <code>ACTIVE</code>). A player session object (including player data) is automatically passed to a game session when the player connects to the game session and is validated.</p> <p>When a player disconnects, the player session status changes to <code>COMPLETED</code>. Once the session ends, the player session object is retained for 30 days and then removed.</p> <p>Player-session-related operations include:</p> <ul> <li> <p> <a>CreatePlayerSession</a> </p> </li> <li> <p> <a>CreatePlayerSessions</a> </p> </li> <li> <p> <a>DescribePlayerSessions</a> </p> </li> <li> <p>Game session placements</p> <ul> <li> <p> <a>StartGameSessionPlacement</a> </p> </li> <li> <p> <a>DescribeGameSessionPlacement</a> </p> </li> <li> <p> <a>StopGameSessionPlacement</a> </p> </li> </ul> </li> </ul>

#### `PlayerSessionCreationPolicy`

``` purescript
newtype PlayerSessionCreationPolicy
  = PlayerSessionCreationPolicy String
```

#### `PlayerSessionId`

``` purescript
newtype PlayerSessionId
  = PlayerSessionId String
```

#### `PlayerSessionList`

``` purescript
newtype PlayerSessionList
  = PlayerSessionList (Array PlayerSession)
```

#### `PlayerSessionStatus`

``` purescript
newtype PlayerSessionStatus
  = PlayerSessionStatus String
```

#### `PortNumber`

``` purescript
newtype PortNumber
  = PortNumber Int
```

#### `PositiveInteger`

``` purescript
newtype PositiveInteger
  = PositiveInteger Int
```

#### `PositiveLong`

``` purescript
newtype PositiveLong
  = PositiveLong Number
```

#### `ProtectionPolicy`

``` purescript
newtype ProtectionPolicy
  = ProtectionPolicy String
```

#### `PutScalingPolicyInput`

``` purescript
newtype PutScalingPolicyInput
  = PutScalingPolicyInput { "Name" :: NonZeroAndMaxString, "FleetId" :: FleetId, "ScalingAdjustment" :: Int, "ScalingAdjustmentType" :: ScalingAdjustmentType, "Threshold" :: Number, "ComparisonOperator" :: ComparisonOperatorType, "EvaluationPeriods" :: PositiveInteger, "MetricName" :: MetricName }
```

<p>Represents the input for a request action.</p>

#### `PutScalingPolicyOutput`

``` purescript
newtype PutScalingPolicyOutput
  = PutScalingPolicyOutput { "Name" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `QueueArnsList`

``` purescript
newtype QueueArnsList
  = QueueArnsList (Array ArnStringModel)
```

#### `RequestUploadCredentialsInput`

``` purescript
newtype RequestUploadCredentialsInput
  = RequestUploadCredentialsInput { "BuildId" :: BuildId }
```

<p>Represents the input for a request action.</p>

#### `RequestUploadCredentialsOutput`

``` purescript
newtype RequestUploadCredentialsOutput
  = RequestUploadCredentialsOutput { "UploadCredentials" :: NullOrUndefined (AwsCredentials), "StorageLocation" :: NullOrUndefined (S3Location) }
```

<p>Represents the returned data in response to a request action.</p>

#### `ResolveAliasInput`

``` purescript
newtype ResolveAliasInput
  = ResolveAliasInput { "AliasId" :: AliasId }
```

<p>Represents the input for a request action.</p>

#### `ResolveAliasOutput`

``` purescript
newtype ResolveAliasOutput
  = ResolveAliasOutput { "FleetId" :: NullOrUndefined (FleetId) }
```

<p>Represents the returned data in response to a request action.</p>

#### `ResourceCreationLimitPolicy`

``` purescript
newtype ResourceCreationLimitPolicy
  = ResourceCreationLimitPolicy { "NewGameSessionsPerCreator" :: NullOrUndefined (WholeNumber), "PolicyPeriodInMinutes" :: NullOrUndefined (WholeNumber) }
```

<p>Policy that limits the number of game sessions a player can create on the same fleet. This optional policy gives game owners control over how players can consume available game server resources. A resource creation policy makes the following statement: "An individual player can create a maximum number of new game sessions within a specified time period".</p> <p>The policy is evaluated when a player tries to create a new game session. For example, with a policy of 10 new game sessions and a time period of 60 minutes, on receiving a <code>CreateGameSession</code> request, Amazon GameLift checks that the player (identified by <code>CreatorId</code>) has created fewer than 10 game sessions in the past 60 minutes.</p>

#### `RoutingStrategy`

``` purescript
newtype RoutingStrategy
  = RoutingStrategy { "Type" :: NullOrUndefined (RoutingStrategyType), "FleetId" :: NullOrUndefined (FleetId), "Message" :: NullOrUndefined (FreeText) }
```

<p>Routing configuration for a fleet alias.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `RoutingStrategyType`

``` purescript
newtype RoutingStrategyType
  = RoutingStrategyType String
```

#### `RuleSetBody`

``` purescript
newtype RuleSetBody
  = RuleSetBody String
```

#### `RuleSetLimit`

``` purescript
newtype RuleSetLimit
  = RuleSetLimit Int
```

#### `RuntimeConfiguration`

``` purescript
newtype RuntimeConfiguration
  = RuntimeConfiguration { "ServerProcesses" :: NullOrUndefined (ServerProcessList), "MaxConcurrentGameSessionActivations" :: NullOrUndefined (MaxConcurrentGameSessionActivations), "GameSessionActivationTimeoutSeconds" :: NullOrUndefined (GameSessionActivationTimeoutSeconds) }
```

<p>A collection of server process configurations that describe what processes to run on each instance in a fleet. All fleets must have a run-time configuration. Each instance in the fleet launches the server processes specified in the run-time configuration and launches new ones as existing processes end. Each instance regularly checks for an updated run-time configuration and follows the new instructions. </p> <p>The run-time configuration enables the instances in a fleet to run multiple processes simultaneously. Potential scenarios are as follows: (1) Run multiple processes of a single game server executable to maximize usage of your hosting resources. (2) Run one or more processes of different build executables, such as your game server executable and a related program, or two or more different versions of a game server. (3) Run multiple processes of a single game server but with different launch parameters, for example to run one process on each instance in debug mode.</p> <p>A Amazon GameLift instance is limited to 50 processes running simultaneously. A run-time configuration must specify fewer than this limit. To calculate the total number of processes specified in a run-time configuration, add the values of the <code>ConcurrentExecutions</code> parameter for each <code> <a>ServerProcess</a> </code> object in the run-time configuration.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `S3Location`

``` purescript
newtype S3Location
  = S3Location { "Bucket" :: NullOrUndefined (NonEmptyString), "Key" :: NullOrUndefined (NonEmptyString), "RoleArn" :: NullOrUndefined (NonEmptyString) }
```

<p>Location in Amazon Simple Storage Service (Amazon S3) where build files can be stored for access by Amazon GameLift. This location is specified in a <a>CreateBuild</a> request. For more details, see the <a href="http://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-build-cli-uploading.html#gamelift-build-cli-uploading-create-build">Create a Build with Files in Amazon S3</a>.</p>

#### `ScalingAdjustmentType`

``` purescript
newtype ScalingAdjustmentType
  = ScalingAdjustmentType String
```

#### `ScalingPolicy`

``` purescript
newtype ScalingPolicy
  = ScalingPolicy { "FleetId" :: NullOrUndefined (FleetId), "Name" :: NullOrUndefined (NonZeroAndMaxString), "Status" :: NullOrUndefined (ScalingStatusType), "ScalingAdjustment" :: NullOrUndefined (Int), "ScalingAdjustmentType" :: NullOrUndefined (ScalingAdjustmentType), "ComparisonOperator" :: NullOrUndefined (ComparisonOperatorType), "Threshold" :: NullOrUndefined (Number), "EvaluationPeriods" :: NullOrUndefined (PositiveInteger), "MetricName" :: NullOrUndefined (MetricName) }
```

<p>Rule that controls how a fleet is scaled. Scaling policies are uniquely identified by the combination of name and fleet ID.</p> <p>Fleet-related operations include:</p> <ul> <li> <p> <a>CreateFleet</a> </p> </li> <li> <p> <a>ListFleets</a> </p> </li> <li> <p>Describe fleets:</p> <ul> <li> <p> <a>DescribeFleetAttributes</a> </p> </li> <li> <p> <a>DescribeFleetPortSettings</a> </p> </li> <li> <p> <a>DescribeFleetUtilization</a> </p> </li> <li> <p> <a>DescribeRuntimeConfiguration</a> </p> </li> <li> <p> <a>DescribeFleetEvents</a> </p> </li> </ul> </li> <li> <p>Update fleets:</p> <ul> <li> <p> <a>UpdateFleetAttributes</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetPortSettings</a> </p> </li> <li> <p> <a>UpdateRuntimeConfiguration</a> </p> </li> </ul> </li> <li> <p>Manage fleet capacity:</p> <ul> <li> <p> <a>DescribeFleetCapacity</a> </p> </li> <li> <p> <a>UpdateFleetCapacity</a> </p> </li> <li> <p> <a>PutScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeScalingPolicies</a> (automatic scaling)</p> </li> <li> <p> <a>DeleteScalingPolicy</a> (automatic scaling)</p> </li> <li> <p> <a>DescribeEC2InstanceLimits</a> </p> </li> </ul> </li> <li> <p> <a>DeleteFleet</a> </p> </li> </ul>

#### `ScalingPolicyList`

``` purescript
newtype ScalingPolicyList
  = ScalingPolicyList (Array ScalingPolicy)
```

#### `ScalingStatusType`

``` purescript
newtype ScalingStatusType
  = ScalingStatusType String
```

#### `SearchGameSessionsInput`

``` purescript
newtype SearchGameSessionsInput
  = SearchGameSessionsInput { "FleetId" :: NullOrUndefined (FleetId), "AliasId" :: NullOrUndefined (AliasId), "FilterExpression" :: NullOrUndefined (NonZeroAndMaxString), "SortExpression" :: NullOrUndefined (NonZeroAndMaxString), "Limit" :: NullOrUndefined (PositiveInteger), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `SearchGameSessionsOutput`

``` purescript
newtype SearchGameSessionsOutput
  = SearchGameSessionsOutput { "GameSessions" :: NullOrUndefined (GameSessionList), "NextToken" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the returned data in response to a request action.</p>

#### `ServerProcess`

``` purescript
newtype ServerProcess
  = ServerProcess { "LaunchPath" :: NonZeroAndMaxString, "Parameters" :: NullOrUndefined (NonZeroAndMaxString), "ConcurrentExecutions" :: PositiveInteger }
```

<p>A set of instructions for launching server processes on each instance in a fleet. Each instruction set identifies the location of the server executable, optional launch parameters, and the number of server processes with this configuration to maintain concurrently on the instance. Server process configurations make up a fleet's <code> <a>RuntimeConfiguration</a> </code>.</p>

#### `ServerProcessList`

``` purescript
newtype ServerProcessList
  = ServerProcessList (Array ServerProcess)
```

#### `SnsArnStringModel`

``` purescript
newtype SnsArnStringModel
  = SnsArnStringModel String
```

#### `StartGameSessionPlacementInput`

``` purescript
newtype StartGameSessionPlacementInput
  = StartGameSessionPlacementInput { "PlacementId" :: IdStringModel, "GameSessionQueueName" :: GameSessionQueueName, "GameProperties" :: NullOrUndefined (GamePropertyList), "MaximumPlayerSessionCount" :: WholeNumber, "GameSessionName" :: NullOrUndefined (NonZeroAndMaxString), "PlayerLatencies" :: NullOrUndefined (PlayerLatencyList), "DesiredPlayerSessions" :: NullOrUndefined (DesiredPlayerSessionList), "GameSessionData" :: NullOrUndefined (GameSessionData) }
```

<p>Represents the input for a request action.</p>

#### `StartGameSessionPlacementOutput`

``` purescript
newtype StartGameSessionPlacementOutput
  = StartGameSessionPlacementOutput { "GameSessionPlacement" :: NullOrUndefined (GameSessionPlacement) }
```

<p>Represents the returned data in response to a request action.</p>

#### `StartMatchBackfillInput`

``` purescript
newtype StartMatchBackfillInput
  = StartMatchBackfillInput { "TicketId" :: NullOrUndefined (MatchmakingIdStringModel), "ConfigurationName" :: MatchmakingIdStringModel, "GameSessionArn" :: ArnStringModel, "Players" :: PlayerList }
```

<p>Represents the input for a request action.</p>

#### `StartMatchBackfillOutput`

``` purescript
newtype StartMatchBackfillOutput
  = StartMatchBackfillOutput { "MatchmakingTicket" :: NullOrUndefined (MatchmakingTicket) }
```

<p>Represents the returned data in response to a request action.</p>

#### `StartMatchmakingInput`

``` purescript
newtype StartMatchmakingInput
  = StartMatchmakingInput { "TicketId" :: NullOrUndefined (MatchmakingIdStringModel), "ConfigurationName" :: MatchmakingIdStringModel, "Players" :: PlayerList }
```

<p>Represents the input for a request action.</p>

#### `StartMatchmakingOutput`

``` purescript
newtype StartMatchmakingOutput
  = StartMatchmakingOutput { "MatchmakingTicket" :: NullOrUndefined (MatchmakingTicket) }
```

<p>Represents the returned data in response to a request action.</p>

#### `StopGameSessionPlacementInput`

``` purescript
newtype StopGameSessionPlacementInput
  = StopGameSessionPlacementInput { "PlacementId" :: IdStringModel }
```

<p>Represents the input for a request action.</p>

#### `StopGameSessionPlacementOutput`

``` purescript
newtype StopGameSessionPlacementOutput
  = StopGameSessionPlacementOutput { "GameSessionPlacement" :: NullOrUndefined (GameSessionPlacement) }
```

<p>Represents the returned data in response to a request action.</p>

#### `StopMatchmakingInput`

``` purescript
newtype StopMatchmakingInput
  = StopMatchmakingInput { "TicketId" :: MatchmakingIdStringModel }
```

<p>Represents the input for a request action.</p>

#### `StopMatchmakingOutput`

``` purescript
newtype StopMatchmakingOutput
  = StopMatchmakingOutput {  }
```

#### `StringDoubleMap`

``` purescript
newtype StringDoubleMap
  = StringDoubleMap (Map NonZeroAndMaxString DoubleObject)
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array NonZeroAndMaxString)
```

#### `StringModel`

``` purescript
newtype StringModel
  = StringModel String
```

#### `TerminalRoutingStrategyException`

``` purescript
newtype TerminalRoutingStrategyException
  = TerminalRoutingStrategyException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The service is unable to resolve the routing for a particular alias because it has a terminal <a>RoutingStrategy</a> associated with it. The message returned in this exception is the message defined in the routing strategy itself. Such requests should only be retried if the routing strategy for the specified alias is modified. </p>

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The client failed authentication. Clients should not retry such requests.</p>

#### `UnsupportedRegionException`

``` purescript
newtype UnsupportedRegionException
  = UnsupportedRegionException { "Message" :: NullOrUndefined (NonEmptyString) }
```

<p>The requested operation is not supported in the region specified.</p>

#### `UpdateAliasInput`

``` purescript
newtype UpdateAliasInput
  = UpdateAliasInput { "AliasId" :: AliasId, "Name" :: NullOrUndefined (NonBlankAndLengthConstraintString), "Description" :: NullOrUndefined (NonZeroAndMaxString), "RoutingStrategy" :: NullOrUndefined (RoutingStrategy) }
```

<p>Represents the input for a request action.</p>

#### `UpdateAliasOutput`

``` purescript
newtype UpdateAliasOutput
  = UpdateAliasOutput { "Alias" :: NullOrUndefined (Alias) }
```

<p>Represents the returned data in response to a request action.</p>

#### `UpdateBuildInput`

``` purescript
newtype UpdateBuildInput
  = UpdateBuildInput { "BuildId" :: BuildId, "Name" :: NullOrUndefined (NonZeroAndMaxString), "Version" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents the input for a request action.</p>

#### `UpdateBuildOutput`

``` purescript
newtype UpdateBuildOutput
  = UpdateBuildOutput { "Build" :: NullOrUndefined (Build) }
```

<p>Represents the returned data in response to a request action.</p>

#### `UpdateFleetAttributesInput`

``` purescript
newtype UpdateFleetAttributesInput
  = UpdateFleetAttributesInput { "FleetId" :: FleetId, "Name" :: NullOrUndefined (NonZeroAndMaxString), "Description" :: NullOrUndefined (NonZeroAndMaxString), "NewGameSessionProtectionPolicy" :: NullOrUndefined (ProtectionPolicy), "ResourceCreationLimitPolicy" :: NullOrUndefined (ResourceCreationLimitPolicy), "MetricGroups" :: NullOrUndefined (MetricGroupList) }
```

<p>Represents the input for a request action.</p>

#### `UpdateFleetAttributesOutput`

``` purescript
newtype UpdateFleetAttributesOutput
  = UpdateFleetAttributesOutput { "FleetId" :: NullOrUndefined (FleetId) }
```

<p>Represents the returned data in response to a request action.</p>

#### `UpdateFleetCapacityInput`

``` purescript
newtype UpdateFleetCapacityInput
  = UpdateFleetCapacityInput { "FleetId" :: FleetId, "DesiredInstances" :: NullOrUndefined (WholeNumber), "MinSize" :: NullOrUndefined (WholeNumber), "MaxSize" :: NullOrUndefined (WholeNumber) }
```

<p>Represents the input for a request action.</p>

#### `UpdateFleetCapacityOutput`

``` purescript
newtype UpdateFleetCapacityOutput
  = UpdateFleetCapacityOutput { "FleetId" :: NullOrUndefined (FleetId) }
```

<p>Represents the returned data in response to a request action.</p>

#### `UpdateFleetPortSettingsInput`

``` purescript
newtype UpdateFleetPortSettingsInput
  = UpdateFleetPortSettingsInput { "FleetId" :: FleetId, "InboundPermissionAuthorizations" :: NullOrUndefined (IpPermissionsList), "InboundPermissionRevocations" :: NullOrUndefined (IpPermissionsList) }
```

<p>Represents the input for a request action.</p>

#### `UpdateFleetPortSettingsOutput`

``` purescript
newtype UpdateFleetPortSettingsOutput
  = UpdateFleetPortSettingsOutput { "FleetId" :: NullOrUndefined (FleetId) }
```

<p>Represents the returned data in response to a request action.</p>

#### `UpdateGameSessionInput`

``` purescript
newtype UpdateGameSessionInput
  = UpdateGameSessionInput { "GameSessionId" :: ArnStringModel, "MaximumPlayerSessionCount" :: NullOrUndefined (WholeNumber), "Name" :: NullOrUndefined (NonZeroAndMaxString), "PlayerSessionCreationPolicy" :: NullOrUndefined (PlayerSessionCreationPolicy), "ProtectionPolicy" :: NullOrUndefined (ProtectionPolicy) }
```

<p>Represents the input for a request action.</p>

#### `UpdateGameSessionOutput`

``` purescript
newtype UpdateGameSessionOutput
  = UpdateGameSessionOutput { "GameSession" :: NullOrUndefined (GameSession) }
```

<p>Represents the returned data in response to a request action.</p>

#### `UpdateGameSessionQueueInput`

``` purescript
newtype UpdateGameSessionQueueInput
  = UpdateGameSessionQueueInput { "Name" :: GameSessionQueueName, "TimeoutInSeconds" :: NullOrUndefined (WholeNumber), "PlayerLatencyPolicies" :: NullOrUndefined (PlayerLatencyPolicyList), "Destinations" :: NullOrUndefined (GameSessionQueueDestinationList) }
```

<p>Represents the input for a request action.</p>

#### `UpdateGameSessionQueueOutput`

``` purescript
newtype UpdateGameSessionQueueOutput
  = UpdateGameSessionQueueOutput { "GameSessionQueue" :: NullOrUndefined (GameSessionQueue) }
```

<p>Represents the returned data in response to a request action.</p>

#### `UpdateMatchmakingConfigurationInput`

``` purescript
newtype UpdateMatchmakingConfigurationInput
  = UpdateMatchmakingConfigurationInput { "Name" :: MatchmakingIdStringModel, "Description" :: NullOrUndefined (NonZeroAndMaxString), "GameSessionQueueArns" :: NullOrUndefined (QueueArnsList), "RequestTimeoutSeconds" :: NullOrUndefined (MatchmakingRequestTimeoutInteger), "AcceptanceTimeoutSeconds" :: NullOrUndefined (MatchmakingAcceptanceTimeoutInteger), "AcceptanceRequired" :: NullOrUndefined (BooleanModel), "RuleSetName" :: NullOrUndefined (MatchmakingIdStringModel), "NotificationTarget" :: NullOrUndefined (SnsArnStringModel), "AdditionalPlayerCount" :: NullOrUndefined (WholeNumber), "CustomEventData" :: NullOrUndefined (CustomEventData), "GameProperties" :: NullOrUndefined (GamePropertyList), "GameSessionData" :: NullOrUndefined (GameSessionData) }
```

<p>Represents the input for a request action.</p>

#### `UpdateMatchmakingConfigurationOutput`

``` purescript
newtype UpdateMatchmakingConfigurationOutput
  = UpdateMatchmakingConfigurationOutput { "Configuration" :: NullOrUndefined (MatchmakingConfiguration) }
```

<p>Represents the returned data in response to a request action.</p>

#### `UpdateRuntimeConfigurationInput`

``` purescript
newtype UpdateRuntimeConfigurationInput
  = UpdateRuntimeConfigurationInput { "FleetId" :: FleetId, "RuntimeConfiguration" :: RuntimeConfiguration }
```

<p>Represents the input for a request action.</p>

#### `UpdateRuntimeConfigurationOutput`

``` purescript
newtype UpdateRuntimeConfigurationOutput
  = UpdateRuntimeConfigurationOutput { "RuntimeConfiguration" :: NullOrUndefined (RuntimeConfiguration) }
```

<p>Represents the returned data in response to a request action.</p>

#### `ValidateMatchmakingRuleSetInput`

``` purescript
newtype ValidateMatchmakingRuleSetInput
  = ValidateMatchmakingRuleSetInput { "RuleSetBody" :: RuleSetBody }
```

<p>Represents the input for a request action.</p>

#### `ValidateMatchmakingRuleSetOutput`

``` purescript
newtype ValidateMatchmakingRuleSetOutput
  = ValidateMatchmakingRuleSetOutput { "Valid" :: NullOrUndefined (BooleanModel) }
```

<p>Represents the returned data in response to a request action.</p>

#### `VpcPeeringAuthorization`

``` purescript
newtype VpcPeeringAuthorization
  = VpcPeeringAuthorization { "GameLiftAwsAccountId" :: NullOrUndefined (NonZeroAndMaxString), "PeerVpcAwsAccountId" :: NullOrUndefined (NonZeroAndMaxString), "PeerVpcId" :: NullOrUndefined (NonZeroAndMaxString), "CreationTime" :: NullOrUndefined (Number), "ExpirationTime" :: NullOrUndefined (Number) }
```

<p>Represents an authorization for a VPC peering connection between the VPC for an Amazon GameLift fleet and another VPC on an account you have access to. This authorization must exist and be valid for the peering connection to be established. Authorizations are valid for 24 hours after they are issued.</p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>

#### `VpcPeeringAuthorizationList`

``` purescript
newtype VpcPeeringAuthorizationList
  = VpcPeeringAuthorizationList (Array VpcPeeringAuthorization)
```

#### `VpcPeeringConnection`

``` purescript
newtype VpcPeeringConnection
  = VpcPeeringConnection { "FleetId" :: NullOrUndefined (FleetId), "IpV4CidrBlock" :: NullOrUndefined (NonZeroAndMaxString), "VpcPeeringConnectionId" :: NullOrUndefined (NonZeroAndMaxString), "Status" :: NullOrUndefined (VpcPeeringConnectionStatus), "PeerVpcId" :: NullOrUndefined (NonZeroAndMaxString), "GameLiftVpcId" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents a peering connection between a VPC on one of your AWS accounts and the VPC for your Amazon GameLift fleets. This record may be for an active peering connection or a pending connection that has not yet been established.</p> <p>VPC peering connection operations include:</p> <ul> <li> <p> <a>CreateVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>DescribeVpcPeeringAuthorizations</a> </p> </li> <li> <p> <a>DeleteVpcPeeringAuthorization</a> </p> </li> <li> <p> <a>CreateVpcPeeringConnection</a> </p> </li> <li> <p> <a>DescribeVpcPeeringConnections</a> </p> </li> <li> <p> <a>DeleteVpcPeeringConnection</a> </p> </li> </ul>

#### `VpcPeeringConnectionList`

``` purescript
newtype VpcPeeringConnectionList
  = VpcPeeringConnectionList (Array VpcPeeringConnection)
```

#### `VpcPeeringConnectionStatus`

``` purescript
newtype VpcPeeringConnectionStatus
  = VpcPeeringConnectionStatus { "Code" :: NullOrUndefined (NonZeroAndMaxString), "Message" :: NullOrUndefined (NonZeroAndMaxString) }
```

<p>Represents status information for a VPC peering connection. Status is associated with a <a>VpcPeeringConnection</a> object. Status codes and messages are provided from EC2 (see <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpcPeeringConnectionStateReason.html">VpcPeeringConnectionStateReason</a>). Connection status information is also communicated as a fleet <a>Event</a>.</p>

#### `WholeNumber`

``` purescript
newtype WholeNumber
  = WholeNumber Int
```


